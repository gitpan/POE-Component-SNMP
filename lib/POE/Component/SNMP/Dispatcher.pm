package POE::Component::SNMP::Dispatcher;

$VERSION = '1.25';

use strict;

use base qw/Net::SNMP::Dispatcher/;


use POE::Kernel;
use POE::Session;

use Time::HiRes qw/time/;

our $INSTANCE;            # Reference to our Singleton object

our $MESSAGE_PROCESSING;  # reference to singled MP object

*DEBUG_INFO = \&Net::SNMP::Dispatcher::DEBUG_INFO;
# sub DEBUG_INFO() { }

use constant VERBOSE => 1; # debugging, that is

sub _ACTIVE()   { 0 }     # State of the event ( not used )
sub _TIME()     { 1 }     # Execution time
sub _CALLBACK() { 2 }     # Callback reference
sub _DELAY()    { 3 }     # Callback reference

# {{{ SUBCLASSED METHODS

# all subclassed methods return the same values as their base
# versions.

# {{{ instance methods and constructor

sub instance { $INSTANCE ||= POE::Component::SNMP::Dispatcher->_new }

# In Net::SNMP::Dispatcher, this function invokes the event
# dispatch loop.  Here, we let POE handle things for us instead,
# and overload with a no-op.
sub activate { }

sub _new     { shift->SUPER::_new(@_)->_new_session() }

sub _new_session {
    my $this = shift;

    # $this->{_active} = Net::SNMP::Message::TRUE;
    $this->{_active} = 1;

    $MESSAGE_PROCESSING = $Net::SNMP::Dispatcher::MESSAGE_PROCESSING;
    POE::Session->create( object_states =>
                          [ $this => [
                                      qw/
                                         _start
                                         _stop
                                         __schedule_event
                                         __invoke_callback
                                         __socket_callback
                                         __listen
                                         __dispatch_pdu
                                         __dispatch_pending_pdu
                                         __clear_pending
                                         /
                                     ],
                          ]);

    $this;
}

# }}} instance methods and constructor
# {{{ send_pdu and _send_pdu

# Net::SNMP::Dispatcher::send_pdu() takes a reference to &_send_pdu in
# its own package, which bypasses inheritance.  Here we temporarily
# replace that reference to point to our own local copy before
# continuing.
#
# This is the first method in the chain of calls to
# Net::SNMP::Dispatcher that gets the action going.
sub send_pdu {
    my ($this, $pdu, $delay) = @_;

    DEBUG_INFO('%s', dump_args( [ $pdu, $delay ] ));

    # use Data::Dumper; print Dumper($pdu); print dump_args([ [ $pdu->callback ], {} ]), "\n";

    local *Net::SNMP::Dispatcher::_send_pdu = \&_send_pdu;
    VERBOSE and DEBUG_INFO('{--------  SUPER::send_pdu()');
    my $retval = $this->SUPER::send_pdu($pdu, $delay);
    VERBOSE and DEBUG_INFO(' --------} SUPER::send_pdu()');
    return $retval;
}

# _send_pdu() tosses requests into POE space at the __dispatch_pdu
# state, which invokes SUPER::_send_pdu() or queues requests for
# later, as appropriate.
sub _send_pdu {
    my ($this, $pdu, $timeout, $retries) = @_;

    DEBUG_INFO('dispatching request [%d] %s', $pdu->transport->fileno,
               VERBOSE ? dump_args( [ $pdu, $timeout, $retries ] ) : '');

    POE::Kernel->post(_poe_component_snmp_dispatcher => __dispatch_pdu =>
                      $pdu, $timeout, $retries);

    1;
}

# }}} send_pdu and _send_pdu
# {{{ schedule and cancel

# Net::SNMP v5.x

# In Net::SNMP::Dispatcher, the critical methods to intercept are:
# - register()  : listen for data on a socket
# - schedule()  : to schedule a timeout action if no response is received
# - deregister(): stop listening on a socket
# - cancel()    : cancel a pending event
# Our versions hand the appropriate actions to POE.
#

sub schedule {
    my ($this, $when, $callback) = @_;
    my $time = time;

    # cook the args like Net::SNMP::schedule() does for _event_insert()
    my $event  = [ $this->{_active}, $time + $when, $this->_callback_create($callback), $when ];

    my $fileno = $event->[_CALLBACK]->[1]->transport->fileno;
    if ($event->[_TIME] <= $time) {
        # run the callback NOW, instead of invoking __invoke_callback.  saves a POE call().

        DEBUG_INFO('{--------  invoking callback [%d] %s', $fileno,
                   VERBOSE ? dump_args( $event->[_CALLBACK] ) : '');

        $this->_callback_execute($event->[_CALLBACK]); # no cooking needed!

        DEBUG_INFO(' --------} callback complete [%d]', $fileno);
     } else {
         DEBUG_INFO("%0.1f seconds [%d] %s", $event->[_DELAY], $fileno,
                   VERBOSE ? dump_args( $event->[_CALLBACK] ) : '');

        POE::Kernel->post(_poe_component_snmp_dispatcher => __schedule_event => $event);
     }

     $event;
}

sub cancel {
    my ($this, $event) = @_;

    # this catches a stray shutdown case where __schedule has been
    # queued but not yet dispatched.  In this case, $event->[_TIME]
    # will be an epoch time in the future, meaning that we never
    # replaced it with a POE delay id, which means there is no POE
    # event to cancel.
    return if $event->[_TIME] > time;

    # $event->[_TIME] is the POE alarm id, which was stashed in __schedule_event
    DEBUG_INFO('remove alarm id %d', $event->[_TIME]);
    POE::Kernel->alarm_remove($event->[_TIME]);

    return !! $this->_pending_pdu_count; # boolean: are there entries are left
}

# }}} schedule and cancel
# {{{ register and deregister

## version support
# see the notes on Net::SNMP v4.x support

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

    DEBUG_INFO('register on [%d] %s', $transport->fileno, VERBOSE ? dump_args([ $callback ]) : '');

    if (ref ($transport = $this->$SUPER_register($transport, $callback))) {

        POE::Kernel->post(_poe_component_snmp_dispatcher => __listen => $transport);

        # POE::Kernel->post(_poe_component_snmp_dispatcher => __listen => $transport,
        #                 [ $this->_callback_create($callback), $transport ]);
    }

    $transport;
}

# there is an optimization here in not having a __unlisten state
# corresponding to __listen (avoiding call() overhead), and just
# telling the kernel directly to stop watching the handle.  __listen
# only needs to exist because when we watch a socket, we have to be in
# the right session.

sub deregister {
    my ($this, $transport) = @_;
    my $fileno = $transport->fileno;

    DEBUG_INFO('deregister on [%d] %s', $transport->fileno,
               VERBOSE ? dump_args([ $transport ]) : '');

    if (ref ($transport = $this->$SUPER_deregister($transport))) {
        $this->_unwatch_transport($transport);
    }

    # no more current.
    $this->_clear_current_pdu($fileno);

    if ($this->_pending_pdu_count($fileno)) {
        # run next pending
        POE::Kernel->post(_poe_component_snmp_dispatcher => __dispatch_pending_pdu => $fileno);
    }

    $transport;
}

# }}} register and deregister

# {{{ Net::SNMP v4.x

# Net::SNMP 5.x changed some of the method names of methods I was
# overriding.  I decided to support both versions.

# The two variables $SUPER_register and $SUPER_deregister are kindof a
# hack around the syntax that I *want* to work, but is not valid perl:
# $self->SUPER::$method()
#
# (caller(0))[3] can't always be trusted to find the value of $method,
# especially when the function being called started its life being
# called &register, but is now being invoked as &_listen.
#
# so here we manually list our SUPER::$method names, and in case it
# turns out we're working with Net::SNMP v4.x, we change the names
# (and symbol table entries) below.

if (Net::SNMP->VERSION() < 5.0) {

    # In our SUPER class (Net::SNMP::Dispatcher), the critical methods
    # to interecept are:
    #
    # _listen: listen for data on a socket
    # _schedule: schedule a timeout action if no response is received
    # _unlisten: stop listening on a socket
    # _cancel: cancel a timeout if a response is received
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
# {{{ PRIVATE METHODS

### These two methods are the only place in this module where the
### socket refcounting is done, so it's all self-contained.

##### socket methods
# {{{ _watch_transport

# socket listen with refcount.  If socket refcount, increment it. Else
# set refcount and listen on the socket.
#
# accesses global kernel.
sub _watch_transport {
    my ($this, $transport) = @_;
    my $fileno = $transport->fileno;
    my $socket = $transport->socket;

    if (not $this->{_refcount}{$fileno}) {
        # reference counting starts at 1 for the controlling
        # *session*, and 1 for this *request*.
        #
        # refcount will fluctuate between 1 and 2 until the owning
        # snmp session is stopped, then it will drop to 0 and we'll
        # stop watching that handle.
        $this->{_refcount}{$fileno} = 1 + 1;
        DEBUG_INFO('[%d] refcount %d, select', $fileno, $this->{_refcount}{$fileno});

        POE::Kernel->select_read($socket, '__socket_callback');
    } else {
        $this->{_refcount}{$fileno}++;
        DEBUG_INFO('[%d] refcount %d, resume', $fileno, $this->{_refcount}{$fileno});

        POE::Kernel->select_resume_read($socket);
    }
    $this->{_refcount}{$fileno};
}

# }}} _watch_transport
# {{{ _unwatch_transport

# decrement the socket refcount. unlisten if refcount == 0.
# accesses global kernel.
sub _unwatch_transport {
    my ($this, $transport) = @_;
    my $fileno = $transport->fileno;
    my $socket = $transport->socket;

    if (--$this->{_refcount}{$fileno} <= 0) {
        DEBUG_INFO('[%d] refcount %d, unselect', $fileno, $this->{_refcount}{$fileno});

        # stop listening on this socket
        POE::Kernel->select_read($socket, undef);
    } else {
        DEBUG_INFO('[%d] refcount %d, pause %s',
                   $fileno, $this->{_refcount}{$fileno}, ('(deferred)') x defined $this->_current_pdu($fileno) );

        POE::Kernel->select_pause_read($socket) unless $this->_current_pdu($fileno);

    }
    $this->{_refcount}{$fileno}
}

# }}} _unwatch_transport
#####

##### current and pending PDU pethods
# {{{ _current_pdu

# if called with one argument, a fileno, returns the current pdu.
#
# if called with two arguments, a fileno and a pdu, makes that pdu the
# current pdu.
sub _current_pdu {
    my ($this, $fileno, $pdu) = @_;

    if (@_ == 3) {
        $this->{_current_pdu}{$fileno} = $pdu;
    }

    $this->{_current_pdu}{$fileno};
}

# remove the current pdu. return it.
sub _clear_current_pdu {
    my ($this, $fileno) = @_;

    delete $this->{_current_pdu}{$fileno};
}

# }}} _current_pdu
# {{{ (_enqueue_pending|_get_next_pending|_clear_pending)_pdu

# enqueues an array reference
sub _enqueue_pending_pdu {
    my ($this, $fileno, $arg) = @_;

    push @{$this->{_pending_pdu}{$fileno}}, $arg;
}

# dequeues an array reference and dereferences it, returning an array
sub _get_next_pending_pdu {
    my ($this, $fileno) = @_;

    shift @{$this->{_pending_pdu}{$fileno}}
}

# deletes the pending queue
sub _clear_pending_pdu {
    my ($this, $fileno) = @_;

    delete $this->{_pending_pdu}{$fileno};
}

# }}} (_enqueue_pending|_get_next_pending|_clear_pending)_pdu
# {{{ _pending_pdu_count

sub _pending_pdu_count {
    my ($this, $fileno) = @_;

    exists         $this->{_pending_pdu}{$fileno}
      and      ref $this->{_pending_pdu}{$fileno} eq 'ARRAY'
        ? scalar @{$this->{_pending_pdu}{$fileno}}
          : 0
}

# }}} _pending_pdu_count
#####

# {{{ _current_callback

# fetch the "current" callback for the fileno
sub _current_callback {
    my ($this, $fileno) = @_;

    $this->{_descriptors}{$fileno}
}

# }}} _current_callback

# }}} PRIVATE METHODS
# {{{ POE EVENTS

# By convention, all POE states, except _start and _stop, have
# two leading underscores.

# {{{ _start and _stop

sub _start { $_[KERNEL]->alias_set('_poe_component_snmp_dispatcher')    }
sub _stop  { $_[KERNEL]->alias_remove('_poe_component_snmp_dispatcher') }

# }}} _start and _stop
# {{{ __dispatch_pdu

# We want to prevent conflicts between listening sockets and pending
# requests, because POE can't listen to two at a time on the same
# handle.  If that socket is currently listening for a reply to a
# different request, the request is placed in a queue.
#
# (which again additionally POE-izes Net::SNMP)
#
# this event is invoked by _send_pdu()
sub __dispatch_pdu {
    my ($this, $heap, $pdu, $timeout, $retries) = @_[OBJECT, HEAP, ARG0..$#_];
    my @pdu_args = ( $pdu, $timeout, $retries ); # these are the args this state was invoked with.
    my $fileno = $pdu->transport->fileno;

    # schedule or execute
    # if ($this->_current_pdu($fileno)) {
    if ($this->_current_pdu($fileno)) {

        $this->_enqueue_pending_pdu($fileno => \@pdu_args);
        DEBUG_INFO('queued request for [%d] %d requests pending',
                   $fileno, $this->_pending_pdu_count($fileno));

    } else {

        DEBUG_INFO('sending request for [%d]', $fileno);

        # $this->_current_pdu($fileno => 1);
        $this->_current_pdu($fileno => $pdu);

        VERBOSE and DEBUG_INFO('{--------  SUPER::__send_pdu() for [%d]', $fileno);
        $this->SUPER::_send_pdu(@pdu_args);
        VERBOSE and DEBUG_INFO(' --------} SUPER::__send_pdu() for [%d]', $fileno );
    }
}

# }}} __dispatch_pdu
# {{{ __dispatch_pending_pdu

# __dispatch_pending_pdu dispatches the next request pending on the
# (socket) that has just been freed.  we moved the "if pending" check
# to deregister().
#
# this event is invoked by deregister().
sub __dispatch_pending_pdu {
    my ($this, $heap, $fileno) = @_[OBJECT, HEAP, ARG0];

    return if $this->{_abort};

    DEBUG_INFO('sending (queued) request on [%d] %d remaining',
               $fileno, $this->_pending_pdu_count($fileno) - 1);

    # mark this fileno active
    my $next_pdu = $this->_get_next_pending_pdu($fileno);
    return unless ref $next_pdu eq 'ARRAY';
    $this->_current_pdu($fileno => $next_pdu->[0]);

    VERBOSE and DEBUG_INFO('{--------  SUPER::__send_pdu() for [%d]', $fileno);
    $this->SUPER::_send_pdu( @{ $next_pdu } );
    VERBOSE and DEBUG_INFO(' --------} SUPER::__send_pdu() for [%d]', $fileno );
}

# }}} __dispatch_pending_pdu
# {{{ __schedule_event

# this event is invoked by schedule() / _event_insert()
sub __schedule_event {
    my ($this, $kernel, $event) = @_[ OBJECT, KERNEL, ARG0 ];

    return if $this->{_abort};

    # $event->[_ACTIVE] is always true for us, and we ignore it.
    #
    # $event->[_TIME] is the epoch time this event should fire.  We
    # use that value for scheduling the POE event, then replace it
    # with POE's alarm id.
    #
    # $event->[_CALLBACK] is an opaque callback reference.
    #
    # We get this same $event back in cancel(), where we reference
    # $event->[_TIME] as alarm id to deactivate.

    my $timeout_id = undef;

    $timeout_id = $kernel->alarm_set(__invoke_callback => $event->[_TIME], $event->[_CALLBACK]);

    # stash the alarm id.  since $event is a reference, this
    # assignment is "global".
    $event->[_TIME] = $timeout_id;

    # I only use $event->[_DELAY] for debugging.
    DEBUG_INFO("alarm id %d, %0.1f seconds [%d] %s",
               $timeout_id, $event->[_DELAY],
               $event->[_CALLBACK]->[1]->transport->fileno,
               VERBOSE ? dump_args([ $event->[_CALLBACK] ]) : ''
              );
}

# }}} __schedule_event
# {{{ __invoke_callback

# Invokes a callback immediately.
#
# this event is invoked when an delay has fired.
sub __invoke_callback {
    my ($this, $callback) = @_[OBJECT, ARG0];

    my $fileno = $callback->[1]->transport->fileno;
    DEBUG_INFO('{--------  invoking scheduled callback for [%d] %s',
               $fileno, VERBOSE ? dump_args([ $callback ]) : '');

    $this->_callback_execute($callback);

    DEBUG_INFO(' --------} callback complete for [%d]', $fileno );
}

# }}} __invoke_callback
# {{{ __listen

# stash the supplied $callback based on the fileno of the $transport
# object.  tell POE to watch the $transport's socket.
#
# this event is invoked by register()
sub __listen {
    my ($this, $kernel, $heap, $transport, $callback) = @_[OBJECT, KERNEL, HEAP, ARG0, ARG1];
    my $fileno = $transport->fileno;
    # we'll fetch the callback directly from $this in __socket_callback

    return if $this->{_abort};

    DEBUG_INFO('listening on [%d]', $fileno);
    $this->_watch_transport($transport);
}

# }}} __listen
# {{{ __socket_callback

# fetch the stashed callback and execute it.
#
# this event is invoked when a watched socket becomes ready to read
# data.
sub __socket_callback {
    my ($this, $heap, $socket) = @_[OBJECT, HEAP, ARG0];
    my $fileno = $socket->fileno;

    DEBUG_INFO('{--------  invoking callback for [%d] %s',
	       $fileno, dump_args($this->_current_callback($fileno)));

    $this->_callback_execute( @{ $this->_current_callback($fileno) } ); # the extra argument is harmless

    DEBUG_INFO(' --------} callback complete for [%d]', $fileno);
}

# }}} __socket_callback
# {{{ __clear_pending

# account for a 'finish' request to a parent snmp session.  Cancels
# any *pending* requests for the specified session. However, if
# 'finish' is called on a session while the Dispatcher is currently
# listening for a reply to that session, that reply *will* be
# delivered when it arrives.
#
# this event is invoked from P::C::S::close_snmp_session(), to help us
# keep in sync.
sub __clear_pending {
    my ($this, $session) = @_[OBJECT, ARG0];

    DEBUG_INFO('start');

    # print Data::Dumper::Dumper($this);
    # use Data::Dumper; print Data::Dumper::Dumper($session);
    # $session->{_callback}[0]->($session);

    my $IMMEDIATE_SHUTDOWN = 1; # let the last pending request deliver itself or just quit

    my ($fileno) = $session->transport->socket->fileno;

    DEBUG_INFO('clearing %d pending requests', $this->_pending_pdu_count($fileno));
    $this->_clear_pending_pdu($fileno);

    # we purposely do NOT delete $this->_current_pdu($fileno) until
    # *AFTER* the select() stuff, so that it doesn't bother doing
    # socket ops, because next we will stop listening all the way.

    # drop reference count
    $this->_unwatch_transport($session->transport);

    if (defined (my $pdu = $this->_clear_current_pdu($fileno))) {

        DEBUG_INFO('cancelling current request');

        # stop listening
        $this->deregister($pdu->transport);

        # cancel timeout
        #
        # Fetch the last cached reference held to our request (and its
        # postback) held outside our own codespace...
        if (defined (my $request = $MESSAGE_PROCESSING->msg_handle_delete($pdu->request_id))) {
            # ... which returns enough information to cancel anything
            # we had pending:
            $this->cancel($request->timeout_id);
        }

    }

    $this->{_abort}++;

    DEBUG_INFO('done');
}

# }}} __clear_pending

# }}} POE EVENTS

# {{{ method call tracing

# this code generates overload stubs for EVERY method in class
# SUPER, that warn their name and args before calling SUPER:: whatever.
if (0) {
    no strict;
    my $package = __PACKAGE__ . "::";
    my $super = "$ISA[0]::";

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

        warn "$@" if $@;        # in case we screwed something up
    }

}

# {{{ dump_args

# get sub_fullname from Sub::Identify if it's present.  If it's not,
# generate our own, simple version.
eval { require Sub::Identify };
# eval { die };
if ($@) {
    eval { sub sub_fullname { ref shift } }
} else {
    Sub::Identify->import('sub_fullname');
}

sub dump_args {
    my @out;
    my $first = 0;
    for (@{$_[0]}) {
        next if ref eq __PACKAGE__;
        # next if $first++;
        my $out;
        if (ref eq 'ARRAY') {
            $out .= '[';
            $out .= join ' ', map {ref $_ ? (ref $_ eq 'CODE' ? sub_fullname($_) : ref $_ ) : $_ || 'undef'} @$_;
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
