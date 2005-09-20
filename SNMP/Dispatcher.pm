package POE::Component::SNMP::Dispatcher;

use strict;

use POE; # for the kernel
use POE::Session;
use Net::SNMP ();
use Net::SNMP::Message qw(TRUE FALSE);

# this already comes in with Net::SNMP I prefer to be explicit in this
# case, since it is the module we're subclassing.
use Net::SNMP::Dispatcher ();
our @ISA = qw/Net::SNMP::Dispatcher/;

our $VERSION = '1.11';

our $INSTANCE;            # Reference to our Singleton object

our $DEBUG = FALSE;       # Debug flag
# our $DEBUG = TRUE;       # Debug flag
our $TRACE = FALSE;

if ($ENV{TRACE_DISPATCHER}) {
    $TRACE = TRUE;
}

# use Data::Dumper ();

sub INTERCEPT_SEND_PDU() { 1 };

# Hook in to Net::SNMP's singleton helper objects:
#
# REPLACE Net::SNMP's version with our POE-ized one
$Net::SNMP::DISPATCHER = POE::Component::SNMP::Dispatcher->instance;
#
# this one might be necessary if we sublass any methods that use it.
# our $MESSAGE_PROCESSING = $Net::SNMP::Dispatcher::MESSAGE_PROCESSING;

# {{{ SUBCLASSED METHODS

# {{{ instance

sub instance {
    &_trace;

    $INSTANCE ||= POE::Component::SNMP::Dispatcher->_new;
}

# }}} instance
# {{{ _new

sub _new {
    &_trace;
    shift->SUPER::_new(@_)->_new_session();
}

# }}} _new
# {{{ activate

sub activate {
    # In Net::SNMP::Dispatcher, this function invokes the event
    # dispatch loop.  Here, we let POE handle things for us instead,
    # and subclass this function with a noop.
}

# }}} activate

# {{{ Net::SNMP v4.x

# In our SUPER class (Net::SNMP::Dispatcher), the critical methods to
# interecept are calls to _listen (listen on a socket) and _schedule
# (to schedule a timeout action if no response is received), and the
# corresponding calls to _unlisten and _cancel.  Our versions hand the
# appropriate actions to POE.

# {{{ _schedule

sub _schedule {
    &_trace;
    my ($this, $time, $callback) = @_;

    $poe_kernel->call(dispatcher => __schedule_event => $time, $this->_callback_create($callback));
}

# }}} _schedule
# {{{ _cancel

sub _cancel {
    &_trace;
    my ($this, $event) = @_;
    print "remove delay id $event->[1]\n" if $TRACE;

    $poe_kernel->alarm_remove( $event->[1] );
}

# }}} _cancel
# {{{ _listen

# We call( dispatcher => '__listen'), which does select_read() within
# a POE::Session end returns, instead of simply invoking select_read()
# here, so that select_read() is guaranteed to occur in the
# 'dispatcher' session (instead of possibly the parent 'snmp'
# session).  Otherwise, when we reach _unlisten(), we could get a
# (silent) failure because the "session doesn't own handle".

# <rant> This was a *GIGANTIC* hassle to debug, and I don't care who
# knows about it.  During the course of tracing this down, Rocco even
# added a diagnostic message to indicate this problem (see the Changes
# file for POE 0.29 ), so at least I can have the satisfaction of
# having been responsible for somebody else down the line not having
# to spend hours debugging this same problem. </rant>

sub _listen {
    &_trace;
    my ($this, $transport, $callback) = @_;

    print "_listen: listening on ", $transport->fileno, "\n" if $TRACE;
    if (ref ($transport = $this->SUPER::_listen($transport, $callback))) {
	$poe_kernel->call( dispatcher => __listen => $transport );
    }

    return $transport;
}

# }}} _listen
# {{{ _unlisten

sub _unlisten {
    &_trace;
    my ($this, $transport) = @_;

    print "_unlisten: unlistening on ", $transport->fileno, "\n" if $TRACE;
    if (ref ($transport = $this->SUPER::_unlisten($transport))) {
	# Stop listening on this socket
	$poe_kernel->select_read($transport->socket);
    }

    $poe_kernel->call(dispatcher => __pdu_sent => $transport->fileno ) if INTERCEPT_SEND_PDU;

    return $transport;
}

# }}} _unlisten

# }}} Net::SNMP v4.x
# {{{ Net::SNMP v5.0.0

# In our SUPER class (Net::SNMP::Dispatcher), the critical methods to
# interecept are calls to register() (listen on a socket) and
# schedule() (to schedule a timeout action if no response is
# received), and the corresponding calls to deregister() and cancel().
# Our versions hand the appropriate actions to POE.

# {{{ schedule

sub schedule {
    &_trace;
    my ($this, $time, $callback) = @_;

    $poe_kernel->call(dispatcher => __schedule_event => $time, $this->_callback_create($callback));
}

# }}} schedule
# {{{ cancel

sub cancel {
    &_trace;
    my ($this, $event) = @_;
    print "remove delay id $event->[1]\n" if $TRACE;

    $poe_kernel->alarm_remove( $event->[1] );
}

# }}} cancel
# {{{ register

# We call( dispatcher => '__listen' ), which does select_read() within
# a POE::Session end returns, instead of simply invoking select_read()
# here, so that select_read() is guaranteed to occur in the
# 'dispatcher' session (instead of possibly the parent 'snmp'
# session).  Otherwise, when we reach _unlisten(), we could get a
# (silent) failure because the "session doesn't own handle".

# <rant> This was a *GIGANTIC* hassle to debug, and I don't care who
# knows about it.  During the course of tracing this down, Rocco even
# added a diagnostic message to indicate this problem (see the Changes
# file for POE 0.29 ), so at least I can have the satisfaction of
# having been responsible for somebody else down the line not having
# to spend hours debugging this same problem. </rant>

sub register {
    &_trace;
    my ($this, $transport, $callback) = @_;

    print "register: listening on ", $transport->fileno, "\n" if $TRACE;
    if (ref ($transport = $this->SUPER::register($transport, $callback))) {
	$poe_kernel->call( dispatcher => __listen => $transport );
    }

    return $transport;
}

# }}} register
# {{{ deregister

sub deregister {
    &_trace;
    my ($this, $transport) = @_;

    print "deregister: unlistening on ", $transport->fileno, "\n" if $TRACE;
    if (ref ($transport = $this->SUPER::deregister($transport))) {
	# Stop listening on this socket
	$poe_kernel->select_read($transport->socket);
    }

    $poe_kernel->call(dispatcher => __pdu_sent => $transport->fileno ) if INTERCEPT_SEND_PDU;

    return $transport;
}

# }}} deregister

# }}} Net::SNMP v5.0.0

# {{{ _send_pdu

# I can't decide if I need this to be here.  It's here because if
# there's anything we want to happen that involves knowing when a
# request is started *or* completed, this method is the hook for
# bookkeeping.

# Currently, we use this slot to ensure that no two listens go out on
# the same host (socket).

sub _send_pdu {
    return shift->SUPER::_send_pdu(@_) unless INTERCEPT_SEND_PDU;

    &_trace;
    my ($this, $pdu, $timeout, $retries) = @_;

    $poe_kernel->call(dispatcher => __send_next_pdu => $pdu, $timeout, $retries);
}

# }}} _send_pdu

# }}} SUBCLASSED METHODS
# {{{ POE EVENTS

# {{{ _new_session

sub _new_session {
    my $self = shift;

    $self->{_active} = TRUE;

    POE::Session->create(
                         object_states =>
                         [ $self => [ qw/_start _stop
                                         __schedule_event
                                         __invoke_callback
                                         __socket_callback __listen
                                         __send_next_pdu __pdu_sent
                                         /
                                    ],
                         ],
                        );


    $self;
}

# }}} _new_session
# {{{ _start

sub _start {
    $_[KERNEL]->alias_set('dispatcher');
}

# }}} _start
# {{{ _stop

sub _stop {
    print __PACKAGE__, "::_stop\n" if $TRACE;
    $_[KERNEL]->alias_remove('dispatcher');
}

# }}} _stop
# {{{ __schedule_event

sub __schedule_event {
    my ($this, $kernel, $time, $callback) = @_[ OBJECT, KERNEL, ARG0, ARG1 ];

    print "$_[STATE]: ($time) ", dump_args([ $callback ]) if $TRACE;

    my $timeout_id = 0;

    # In the original Net::SNMP::Dispatcher::_schedule(), the return
    # value is an $event, as that module understands $events.  In most
    # cases, the return value of _schedule() is ignored, but when it
    # is NOT ignored, the return value is expected to be of this
    # general form.  This is the only place I had to really EMULATE
    # the superclass's guts, and subjectively feels like I'm too close
    # to its internals.  If this ever changes in Net::SNMP::Dispacher,
    # I'll have to update this return value to account for it (In
    # fact, this is probably my only criticism of the otherwise
    # masterfully designed object-orientation of Net::SNMP and its
    # support modules).

    # We reuse the $event->[1] slot here to preserve the POE delay_id
    # if set.
    my $event = [ $this->{_active}, $timeout_id, $callback ];

    if ($time) {
	$timeout_id = $kernel->delay_set(__invoke_callback => $time, $callback, $event);
	$event->[1] = $timeout_id;
    } else {
	$kernel->yield(__invoke_callback => $callback);
    }

    $event;
}

# }}} __schedule_event
# {{{ __invoke_callback

sub __invoke_callback {
    my ($this, $callback, $event) = @_[OBJECT, ARG0, ARG1];
    print "$_[STATE]: ", dump_args([ $callback ]) if $TRACE;

    $this->_callback_execute($callback);
}

# }}} __invoke_callback
# {{{ __socket_callback

sub __socket_callback {
    my ($this, $socket) = @_[OBJECT, ARG0];

    my $fileno = $socket->fileno;
    DEBUG_INFO('descriptor [%d] ready', $fileno);

    # The original call to _listen() prepped the appropriate callback
    # information into $this->{_descriptors} based on the fileno of
    # $socket.  All we have to do here is invoke whatever is queued.
    $this->_callback_execute(@{$this->{_descriptors}->{$fileno}});
}

# }}} __socket_callback
# {{{ __listen

# See comments on _listen().
sub __listen {
    my ($kernel, $transport) = @_[KERNEL, ARG0];

    $kernel->select_read($transport->socket, '__socket_callback');
}

# }}} __listen

# {{{ __send_next_pdu

# This module is designed to prevent conflicts between listening
# sockets and pending requests.  If a request "backs up" on a socket
# (that socket is currently listening for a reply to a different
# request), the request PDU is placed in a queue.
#
# (which again additionally POE-izes Net::SNMP)
#
# this function is invoked by both _send_pdu() and (via __pdu_sent())
# /_unlisten|deregister/()

sub __send_next_pdu {
    # &_trace;
    my ($this, $kernel, $heap, $pdu, $timeout, $retries) = @_[OBJECT, KERNEL, HEAP, ARG0..$#_];

    if (defined $pdu and ref $pdu) {
	my $pdu_call = [ $pdu, $timeout, $retries ];

	# schedule or execute
	if (not exists $heap->{_current_pdu}{$pdu->transport->fileno}) {

	    # print "$_[STATE]: calling SUPER::_send_pdu\n";
	    $heap->{_current_pdu}{$pdu->transport->fileno} = $pdu_call;
	    $this->SUPER::_send_pdu(@$pdu_call);

	} else {

	    # print "$_[STATE]: queueing SUPER::_send_pdu\n";
	    push @{$heap->{_pending_pdu}{$pdu->transport->fileno}}, $pdu_call;

	}

    } else { 	# execute next or noop
	# here we've used a little bit of "bad behavior":
	#
	# the $_[ARG0] argument passed to this function is EITHER a
	# $pdu object reference OR a fileno (integer).
	my $fileno = $pdu;

	if (exists $heap->{_pending_pdu}{$fileno}
	    and  @{$heap->{_pending_pdu}{$fileno}}) {
	    # print "$_[STATE]: calling (queued) SUPER::_send_pdu\n";
	    $heap->{_current_pdu}{$fileno} = shift @{$heap->{_pending_pdu}{$fileno}};
	    $this->SUPER::_send_pdu(@{$heap->{_current_pdu}{$fileno}});
	}
    }
}

# }}} __send_next_pdu
# {{{ __pdu_sent

sub __pdu_sent {
    &_trace;
    my ($kernel, $heap, $fileno) = @_[KERNEL, HEAP, ARG0];

    # XXX Programmer has confused self XXX
    #
    # This delete shouldn't be necessary. Meaning, I can't figure out
    # WHY it's necessary, on this code review. Dang me to heck for not
    # documenting it when I first wrote it.
    #
    # __send_next_pdu simply CALLS any pending $pdu if the argument is
    # not a reference (like, a fileno argument), and doesn't even
    # *check* for _current_pdu.  HOWEVER, if I disable this call to
    # delete here the module ceases to work.
    #
    # XXX XXXXXXXXXXXXXXXXXXXXXXXXXXXX XXX
    delete $heap->{_current_pdu}{$fileno};

    $kernel->yield(__send_next_pdu => $fileno);
}

# }}} __pdu_sent

# }}} POE EVENTS

# {{{ DEBUG_INFO

# will somebody tell me why I need this?  it doesn't seem to inherit
# properly

sub DEBUG_INFO
{
   return unless $DEBUG;

   printf(
      sprintf('debug: [%d] %s(): ', (caller(0))[2], (caller(1))[3]) .
      shift(@_) .
      "\n",
      @_
   );

   $DEBUG;
}

# }}} DEBUG_INFO
# {{{ _trace

sub _trace {
    if ($TRACE) {
	printf "%s from %s:%s %s", (caller(1))[3], (caller (2))[3,2], dump_args(\@_);
    }
}

if ($TRACE) {
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

# }}} _trace

1;

__END__
