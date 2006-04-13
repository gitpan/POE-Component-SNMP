package POE::Component::SNMP::Dispatcher;

$VERSION = '1.14';

use strict;

use POE::Kernel;
use POE::Session;

use Net::SNMP::Dispatcher ();
our @ISA = qw/Net::SNMP::Dispatcher/;

our $INSTANCE;            # Reference to our Singleton object

*DEBUG_INFO = \&Net::SNMP::Dispatcher::DEBUG_INFO;

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

# {{{ send_pdu

# This is a cut-and-pasted copy of Net::SNMP::Dispatcher::send_pdu.
# (except I added the DEBUG_INFO line and changed TRUE at the end to a
# 1 because I don't import that constant)
#
# I overrode this function to mess with something different, just to
# see what would happen.  And after I cut/pasted it, I ran make test
# to ensure things still worked.
#
# However, its presence speeds up PoCo::SNMP by a HUGE amount.
# Before, there were weird delays and pauses in between
# requests... now those are completely gone.
#
# This was a complete accident!  I was just going to do some
# debugging, and it turned out to be exactly what I needed!
#
# It has to do with the reference to \&_send_pdu in the call to
# $this->schedule() referring to _send_pdu in the CURRENT PACKAGE.  So
# inheritance doesn't help me, it's actually a reference to the
# particular function, not a dispatchable method call.  So subclassing
# the method with identical behavior lets the reference be in the
# right scope (calling my POE-ized version of _send_pdu()), without
# changing the behavior.

sub send_pdu
{
   my ($this, $pdu, $delay) = @_;

   DEBUG_INFO('%s', dump_args( [ $pdu, $delay ] ));

   # Clear any previous errors
   $this->_error_clear;

   if ((@_ < 2) || !ref($pdu)) {
      return $this->_error('Required PDU missing');
   }

   # If the Dispatcher is active and the delay value is negative,
   # send the message immediately.

   if ($delay < 0) {
      if ($this->{_active}) {
         return $this->_send_pdu($pdu, $pdu->timeout, $pdu->retries);
      }
      $delay = 0;
   }

   $this->schedule($delay, [\&_send_pdu, $pdu, $pdu->timeout, $pdu->retries]);

   1;
}

# }}} send_pdu
# {{{ _send_pdu

# _send_pdu() tosses requests into POE space at the __send_next_pdu
# state, which invokes SUPER::_send_pdu() appropriately as connections
# become available.

sub _send_pdu {
    my ($this, $pdu, $timeout, $retries) = @_;

    DEBUG_INFO('%s', dump_args( [ $pdu, $timeout, $retries ] ));
    # CCCC should this be call() or post()?
    POE::Kernel->post(dispatcher => __send_next_pdu => $pdu, $timeout, $retries);

    1; # return true, like our SUPER version does
}

# }}} _send_pdu

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

    DEBUG_INFO("%d seconds %s", $time, dump_args([ $callback ]));

    # this *must* be call() because the return value is significant
    POE::Kernel->call(dispatcher => __schedule_event => $time, $this->_callback_create($callback));
}

# }}} schedule
# {{{ cancel

# jump into POE space and cancel a scheduled timeout.
sub cancel {
    my ($this, $event) = @_;

    # $event->[1] is the POE alarm_id, which we stashed in __schedule()
    DEBUG_INFO('remove delay id %d', $event->[1]);
    POE::Kernel->alarm_remove($event->[1]);
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
# Here we say POE::Kernel->call(dispatcher => '__listen' ), which does
# select_read() *within a POE::Session* and returns, instead of simply
# invoking select_read() here, so that select_read() is guaranteed to
# occur from within the 'dispatcher' session (instead of possibly the
# parent 'snmp' session).  Otherwise, when we reach _unlisten(), we
# could get a (silent) failure because the "session doesn't own
# handle".

# <rant> This was a *GIGANTIC* hassle to debug, and I don't care who
# knows about it.  During the course of tracing this down, Rocco even
# added a diagnostic message to indicate this problem (see the Changes
# file for POE 0.29 ), so at least I can have the satisfaction of
# having been responsible for somebody else down the line not having
# to spend the hours debugging this same problem that I did.</rant>

sub register {
    my ($this, $transport, $callback) = @_;

    DEBUG_INFO('listening on %d', $transport->fileno);
    if (ref ($transport = $this->$SUPER_register($transport, $callback))) {
	# CCCC should this be call() or post()?
        POE::Kernel->post(dispatcher => __listen => $transport);
    }

    $transport;
}

# }}} register
# {{{ deregister

# there is an optimization here in not having a __unlisten state
# (avoiding call() overhead), and just telling the kernel directly to
# stop watching the handle.  __listen only needs to exist because of
# context.

sub deregister {
    my ($this, $transport) = @_;

    DEBUG_INFO('unlistening on %d', $transport->fileno);
    if (ref ($transport = $this->$SUPER_deregister($transport))) {
            # stop listening on this socket
            POE::Kernel->select_read($transport->socket);
    }

    # CCCC should this be call() or post()?
    POE::Kernel->post(dispatcher => __pdu_sent => $transport->fileno);

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

# }}} SUBCLASSED METHODS
# {{{ POE EVENTS

# By convention, all POE states, except _start and _stop, have
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

    # Net::SNMP::Dispatcher::schedule() returns an $event, as that
    # module understands $events.  In most cases, the return value of
    # _schedule() is ignored, but when it is NOT ignored, the return
    # value is expected to be of this general form:
    #
    #  [ _ACTIVE, _TIME, _CALLBACK ]
    #
    # _ACTIVE is always true for us.  _TIME is internal to Net::SNMP's
    # event loop and is NOT used with POE's, so we reuse that slot for
    # POE's timeout_id if necessary.  _CALLBACK is an opaque callback
    # reference.

    my $timeout_id = undef;
    # yes, I could eliminate $timeout_id, but it's so descriptive in
    # this next line:
    my $event = [ $this->{_active}, $timeout_id, $callback ];

    if ($time) {
	# We use the $event->[1] slot here to preserve the POE
	# delay_id if set.
	$timeout_id = $kernel->delay_set(__invoke_callback => $time, $callback);
	$event->[1] = $timeout_id;

	DEBUG_INFO('%d seconds (delay id %d) %s', $time, $timeout_id, dump_args([ $callback ]));

    } else {
	DEBUG_INFO('immediate %s', dump_args([ $callback ]));

        if (1) {
            # we yield here to give the rest of POE a chance to do
            # something for a sec.
            $kernel->yield(__invoke_callback => $callback);
        } else {
            # run the callback directly, saving the POE overhead.
            $this->_callback_execute($callback);
        }

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
    DEBUG_INFO('%s', dump_args([ $callback ]));

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
    my $descriptor = $socket->fileno;

    DEBUG_INFO('socket %d got data, calling: %s',
               $descriptor,
               dump_args($this->{_descriptors}->{$descriptor}));

    # The original call to Net::SNMP::register() preps the appropriate callback
    # information into $this->{_descriptors}, based on the fileno of
    # $socket.  All we have to do here is look it up and invoke it.
    #
    # Note, I could have made this:
    #
    # $kernel->yield(__invoke_callback => @{$this->{_descriptors}->{$descriptor}}[0,1]);
    #
    # ... but why add the overhead for the call()?
    $this->_callback_execute(@{$this->{_descriptors}->{$descriptor}}[0,1]);
}

# }}} __socket_callback
# {{{ __listen

# See comments on register().
sub __listen {
    my ($kernel, $heap, $transport) = @_[KERNEL, HEAP, ARG0];
    my $descriptor = $transport->fileno;

    DEBUG_INFO('listening on descriptor %d', $descriptor);
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

    # use Data::Dumper; print Dumper( [ @_[STATE, ARG0..$#_] ] );

    # here we've used a little bit of "bad behavior":
    #
    # the $_[ARG0] argument, $arg, passed to this function is EITHER a
    # pdu object reference OR a fileno (integer) corresponding to a
    # socket.

    if (defined $arg and ref $arg) {
        # invoked with a pdu object.
	my $pdu = $arg;
        my $descriptor = $pdu->transport->fileno;

	# these are the three args this function was called with.
	my @pdu_args = ( $pdu, $timeout, $retries );

        # schedule or execute
        if (not exists $heap->{_current_pdu}{$descriptor}) {

            DEBUG_INFO('calling SUPER::_send_pdu for descriptor %d', $descriptor);
            $heap->{_current_pdu}{$descriptor} = \@pdu_args;
            $this->SUPER::_send_pdu(@pdu_args);

        } else {

            DEBUG_INFO('queueing SUPER::_send_pdu for descriptor %d', $descriptor);
            push @{$heap->{_pending_pdu}{$descriptor}}, \@pdu_args;

        }

    } else {                    # execute next or NOOP
        # Here is where look up the fileno in our pending requests,
        # and invoke the next one.
	my $descriptor = $arg;

	if ( exists $heap->{_pending_pdu}{$descriptor}
             and  @{$heap->{_pending_pdu}{$descriptor}} ) {

            DEBUG_INFO('calling (queued) SUPER::_send_pdu for descriptor %d', $descriptor);

	    # remove the first item off of the _pending_pdu list
	    $heap->{_current_pdu}{$descriptor} = shift @{$heap->{_pending_pdu}{$descriptor}};
	    # deref it as a listref and pass it to _send_pdu
	    $this->SUPER::_send_pdu(@{$heap->{_current_pdu}{$descriptor}});

	} else {
            DEBUG_INFO('no pending PDUs');
        }
    }
}

# }}} __send_next_pdu
# {{{ __pdu_sent

sub __pdu_sent {
    my ($kernel, $heap, $descriptor) = @_[KERNEL, HEAP, ARG0];

    DEBUG_INFO('calling __send_next_pdu on descriptor %d', $descriptor);

    # use Data::Dumper; print STDERR Dumper ($heap);

    delete $heap->{_current_pdu}{$descriptor};
    $kernel->yield(__send_next_pdu => $descriptor);
}

# }}} __pdu_sent

# }}} POE EVENTS

# {{{ method call tracing

# this code generates overload stubs for EVERY method in class
# SUPER, that warn their name and args before calling SUPER::whatever.
if (0) {
# if (1) {
    my $package = __PACKAGE__ . "::";
    my $super = "$ISA[0]::";
    no strict "refs";

    for (grep defined *{"$super$_"}{CODE}, keys %{$super}) {
	next if /_*[A-Z]+$/; # ignore constants
	next if defined *{ "$package$_" }{CODE};
	print "assigning trace for $_\n";

	*{ "$package::$_" } =
	  eval qq[ sub {
		       my (\$package, \$filename, \$line, \$subroutine, \$sub) = caller (1);
		       print "$super$_ from \$subroutine:\$line ", (dump_args(\\\@_)), "\n";
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

    return '{' . join (" ", @out) . '}';
}

# }}} dump_args

# }}} method call tracing

1;

__END__
