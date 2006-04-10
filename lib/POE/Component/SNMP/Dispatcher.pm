package POE::Component::SNMP::Dispatcher;

$VERSION = '1.13';

use strict;

use POE::Kernel;
use POE::Session;

use Net::SNMP::Dispatcher ();
our @ISA = qw/Net::SNMP::Dispatcher/;

our $INSTANCE;            # Reference to our Singleton object

our $TRACE = 0;
# $TRACE = 1 if $ENV{TRACE_DISPATCHER};

# REPLACE Net::SNMP's singleton helper object with our POE-ized one:
$Net::SNMP::DISPATCHER = POE::Component::SNMP::Dispatcher->instance;

# {{{ SUBCLASSED METHODS

# {{{ instance

sub instance {
    $INSTANCE ||= POE::Component::SNMP::Dispatcher->_new;
}

# }}} instance
# {{{ _new

# I think this says, invoke base constructor, then call the
# _new_session() method on the new object (of our class).
sub _new {
    shift->SUPER::_new(@_)->_new_session();
}

# }}} _new
# {{{ activate

# In Net::SNMP::Dispatcher, this function invokes the event
# dispatch loop.  Here, we let POE handle things for us instead,
# and overload with a no-op.
sub activate { }

# }}} activate

# {{{ Net::SNMP v5.x

# In our SUPER class (Net::SNMP::Dispatcher), the critical methods to
# interecept are calls to register() (listen for data on a socket) and
# schedule() (to schedule a timeout action if no response is
# received), and the corresponding calls to deregister() and cancel().
# Our versions hand the appropriate actions to POE.

# {{{ schedule

# jump into POE space and hand this event off for processing.
sub schedule {
    my ($this, $time, $callback) = @_;

    POE::Kernel->call(dispatcher => __schedule_event => $time, $this->_callback_create($callback));
}

# }}} schedule
# {{{ cancel

# jump into POE space and cancel a scheduled timeout.
sub cancel {
    my ($this, $event) = @_;

    # print "remove delay id $event->[1]\n" if $TRACE;
    POE::Kernel->alarm_remove($event->[1]);
    # $event->[1] is the POE alarm_id, which we stashed in __schedule()
}

# }}} cancel

# {{{ register

## version support
#
# These two variables are kindof a hack around the syntax that I
# *want* to work, but is not valid perl: $self->SUPER::$method()
#
# (caller(0))[3] can't always be trusted to find the value of $method,
# especially when the function being called started its life being
# called &register, but is now being invoked as &_listen.
#
# so here we manually list our SUPER::$method names, and in case it
# turns out we're working with Net::SNMP v4.x, we change the names
# (and symbol table entries) below.

our $SUPER_register = 'SUPER::register';
our $SUPER_deregister = 'SUPER::deregister';

## coding notes
#
# We call(dispatcher => '__listen' ), which does select_read() *within
# a POE::Session* and returns, instead of simply invoking select_read()
# here, so that select_read() is guaranteed to occur from within the
# 'dispatcher' session (instead of possibly the parent 'snmp'
# session).  Otherwise, when we reach _unlisten(), we could get a
# (silent) failure because the "session doesn't own handle".

# <rant> This was a *GIGANTIC* hassle to debug, and I don't care who
# knows about it.  During the course of tracing this down, Rocco even
# added a diagnostic message to indicate this problem (see the Changes
# file for POE 0.29 ), so at least I can have the satisfaction of
# having been responsible for somebody else down the line not having
# to spend the hours debugging this same problem that I did.</rant>

# this function and the one that follows are templates that work for
# both 4.x _listen() & _unlisten() and 5.x register() & deregister().
sub register {
    my ($this, $transport, $callback) = @_;

    # print "register: listening on ", $transport->fileno, "\n" if $TRACE;
    if (ref ($transport = $this->$SUPER_register($transport, $callback))) {
	POE::Kernel->call(dispatcher => __listen => $transport);
    }

    $transport;
}

# }}} register
# {{{ deregister

sub deregister {
    my ($this, $transport) = @_;

    # print "deregister: unlistening on ", $transport->fileno, "\n" if $TRACE;
    if (ref ($transport = $this->$SUPER_deregister($transport))) {
	# Stop listening on this socket
	POE::Kernel->select_read($transport->socket);
    }

    POE::Kernel->call(dispatcher => __pdu_sent => $transport->fileno);

    $transport;
}

# }}} deregister

# }}} Net::SNMP v5.x
# {{{ Net::SNMP v4.x

if (Net::SNMP->VERSION() < 5.0) {

    # In our SUPER class (Net::SNMP::Dispatcher), the critical methods
    # to interecept are calls to _listen (listen for data on a socket)
    # and _schedule (schedule a timeout action if no response is
    # received), and the corresponding calls to _unlisten and _cancel.
    #
    # Here, we play games with the symbol table so that these
    # functions, which were renamed from 4.x to 5.x, are subclassed
    # appropriately.

    *_schedule = \&schedule;
    *_cancel   = \&cancel;

    *_listen   = \&register;
    *_unlisten = \&deregister;

    $SUPER_register = 'SUPER::_listen';
    $SUPER_deregister = 'SUPER::_unlisten';

}

# }}} Net::SNMP v4.x

# {{{ _send_pdu

# This method is our primary hook into invoking POE's event loop when
# Net::SNMP goes to send a message.  When this method is called, it
# jumps into POE space with a call that cascades into the rest of the
# POE + Net::SNMP interaction.
sub _send_pdu {
    my ($this, $pdu, $timeout, $retries) = @_;

    POE::Kernel->call(dispatcher => __send_next_pdu => $pdu, $timeout, $retries);
}

# }}} _send_pdu

# }}} SUBCLASSED METHODS
# {{{ POE EVENTS

# By convention here, all POE states, except _start and _stop, have
# two leading underscores.

# {{{ _new_session

sub _new_session {
    my $self = shift;

    # $self->{_active} = Net::SNMP::Message::TRUE;
    $self->{_active} = 1;

    POE::Session->create(
                         object_states =>
                         [ $self => [
                                     qw/
                                        _start
                                        _stop
                                        __schedule_event
                                        __invoke_callback
                                        __socket_callback
                                        __listen
                                        __send_next_pdu
                                        __pdu_sent
                                        /
                                    ],
                         ],
                        );


    $self;
}

# }}} _new_session
# {{{ _start

sub _start { $_[KERNEL]->alias_set('dispatcher')   }

# }}} _start
# {{{ _stop

sub _stop  { $_[KERNEL]->alias_remove('dispatcher') }

# }}} _stop
# {{{ __schedule_event

sub __schedule_event {
    my ($this, $kernel, $time, $callback) = @_[ OBJECT, KERNEL, ARG0, ARG1 ];
    # print "$_[STATE]: ($time) ", dump_args([ $callback ]) if $TRACE;

    # In the original Net::SNMP::Dispatcher::_schedule(), the return
    # value is an $event, as that module understands $events.  In most
    # cases, the return value of _schedule() is ignored, but when it
    # is NOT ignored, the return value is expected to be of this
    # general form:
    #
    #  [ is_active, event_id (?), callback_reference ]
    #
    # is_active is always true for us.  event_id is internal to
    # Net::SNMP's event loop and is NOT used with POE's, so we reuse
    # that slot for our timeout_id if necessary.
    #
    # This is the only place I had to really EMULATE the superclass's
    # guts, because the event is not an object, and subjectively feels
    # like I'm too close to its internals.  If this ever changes in
    # Net::SNMP::Dispacher, I'll have to update this return value to
    # account for it (In fact, this is probably my only criticism of
    # the otherwise masterfully designed object-orientation of
    # Net::SNMP and its support modules).

    my $timeout_id = undef; # yes, I could eliminate this variable,
                            # but it's so descriptive in the next
                            # line:
    my $event = [ $this->{_active}, $timeout_id, $callback ];

    if ($time) {
	# We can reuse the $event->[1] slot here to preserve the POE
	# alarm_id if set.
	$timeout_id = $kernel->delay_set(__invoke_callback => $time, $callback);
	$event->[1] = $timeout_id;
    } else {
	# we yield here to give the rest of POE a chance to do
	# something for a sec.
	$kernel->yield(__invoke_callback => $callback);
    }

    # give Net::SNMP something it can understand.
    $event;
}

# }}} __schedule_event
# {{{ __invoke_callback

# Invokes a callback immediately.  Called when schedule() is invoked
# without a delay.

sub __invoke_callback {
    my ($this, $callback) = @_[OBJECT, ARG0];

    # print "$_[STATE]: ", dump_args([ $callback ]) if $TRACE;
    $this->_callback_execute($callback);
}

# }}} __invoke_callback
# {{{ __socket_callback

# Invokes a callback when a socket receives data.  Called when data
# comes in on sockets that have been prepped with register().  As with
# all POE handle callbacks, the first parameter is the handle that is
# ready for data.  What's REALLY nice here is, I don't have to process
# or do *anything* to the input, the callback design here (which I'm
# just working with without really following) is so tight that all I
# have to do is map POE's socket to its filehandle, and invoke that
# callback.  Damn.

sub __socket_callback {
    my ($this, $socket) = @_[OBJECT, ARG0];

    my $fileno = $socket->fileno;

    # The original call to Net::SNMP::register() preps the appropriate callback
    # information into $this->{_descriptors}, based on the fileno of
    # $socket.  All we have to do here is invoke it.
    #
    # Note, I could have made this:
    #
    # $kernel->yield(__invoke_callback => @{$this->{_descriptors}->{$fileno}});
    #
    # ... but why add the overhead for the call?
    $this->_callback_execute(@{$this->{_descriptors}->{$fileno}});
}

# }}} __socket_callback
# {{{ __listen

# See comments on register().
sub __listen {
    my ($kernel, $transport) = @_[KERNEL, ARG0];

    $kernel->select_read($transport->socket, '__socket_callback');
}

# }}} __listen

# {{{ __send_next_pdu

# This module is designed to prevent conflicts between listening
# sockets and pending requests, because POE can't listen to two at a
# time on the same handle.  If a request "backs up" on a socket (that
# socket is currently listening for a reply to a different request),
# the request PDU is placed in a queue.
#
# (which again additionally POE-izes Net::SNMP)
#
# This function is invoked by both _send_pdu() and (via __pdu_sent())
# deregister().

sub __send_next_pdu {
    my ($this, $kernel, $heap, $arg, $timeout, $retries) = @_[OBJECT, KERNEL, HEAP, ARG0..$#_];

    # here we've used a little bit of "bad behavior":
    #
    # the $_[ARG0] argument, $arg, passed to this function is EITHER a
    # pdu object reference OR a fileno (integer) corresponding to a
    # socket.

    if (defined $arg and ref $arg) {
        # invoked with a pdu object.
	my $pdu = $arg;

	# these are the three args this function was called with.
	my @pdu_args = ( $pdu, $timeout, $retries );

	# schedule or execute
	if (not exists $heap->{_current_pdu}{$pdu->transport->fileno}) {

	    # print "$_[STATE]: calling SUPER::_send_pdu\n";
	    $heap->{_current_pdu}{$pdu->transport->fileno} = \@pdu_args;
	    $this->SUPER::_send_pdu(@pdu_args);

	} else {

	    # print "$_[STATE]: queueing SUPER::_send_pdu\n" if $TRACE;
	    push @{$heap->{_pending_pdu}{$pdu->transport->fileno}}, \@pdu_args;

	}

    } else { 	# execute next or NOOP
        # Here is where look up the fileno in our pending requests,
        # and invoke the next one.
	my $fileno = $arg;

	if ( exists $heap->{_pending_pdu}{$fileno}
             and  @{$heap->{_pending_pdu}{$fileno}} ) {

	    # print "$_[STATE]: calling (queued) SUPER::_send_pdu\n" if $TRACE;

	    # remove the first item off of the _pending_pdu list
	    $heap->{_current_pdu}{$fileno} = shift @{$heap->{_pending_pdu}{$fileno}};
	    # deref it as a listref and pass it to _send_pdu
	    $this->SUPER::_send_pdu(@{$heap->{_current_pdu}{$fileno}});

	}
    }
}

# }}} __send_next_pdu
# {{{ __pdu_sent

sub __pdu_sent {
    my ($kernel, $heap, $fileno) = @_[KERNEL, HEAP, ARG0];

    delete $heap->{_current_pdu}{$fileno};
    $kernel->yield(__send_next_pdu => $fileno);
}

# }}} __pdu_sent

# }}} POE EVENTS

# {{{ method call tracing

# this code generates overload stubs for EVERY method in class
# SUPER, that warn their name and args before calling SUPER::whatever.
if (0) {
    my $package = __PACKAGE__ . "::";
    my $super = "$ISA[0]::";
    no strict "refs";

    for (grep defined *{"$super$_"}{CODE}, keys %{$super}) {
	next if /_*[A-Z]+$/;
	next if defined *{ "$package$_" }{CODE};
	# print "assigning trace for $_\n";

	*{ "$package::$_" } =
	  eval qq[ sub {
		       my (\$package, \$filename, \$line, \$subroutine, \$sub) = caller (1);
		       print "$super$_ from \$subroutine:\$line ", (dump_args(\\\@_));
		       goto &{"$super$_"};
		   }
		 ];

	warn "$@" if $@;	# in case we screwed something up
    }

}

# {{{ dump_args

sub dump_args {
    my @out;
    my $first = 0;
    for (@{$_[0]}) {
	next if ref eq __PACKAGE__;
	# next if $first++;
	my $out;
	if (ref eq 'ARRAY') {
	    $out .= '[';
	    $out .= join ' ', map {ref $_ ? ref $_ : $_ || 'undef'} @$_;
	    $out .= ']';
	} else {
	    $out .= ref $_ ? ref $_ : $_;
	}
	push @out, $out;
    }

    return '{' . join (" ", @out) . '}' . "\n";
}

# }}} dump_args

# }}} method call tracing

1;

__END__
