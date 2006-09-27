package POE::Component::SNMP;

use strict;

our $VERSION = '1.07';

package POE::Net::SNMP;

use base q/Net::SNMP/;

our %localport;

# {{{ session

sub session {
    my $class = shift;
    my @arg = @_;
    my ($session, $error);

    # see if there is a localport supplied stash it on our list.
    my ($localport, %arg) = POE::Component::SNMP::_arg_scan(localport => @arg);

    if (defined $localport) {

        if (exists $localport{$localport}) {
            ($session, $error) = (undef, "Address already in use");
        } else {
            ($session, $error) =
              $class->SUPER::session( -nonblocking => 1,
                                      -localport => $localport,
                                      %arg,
                                    );
        }

    } else {

        # each session binds to a different local port/socket.  This
        # do..while loop catches potential port conflicts.
        do {

            # pick a port that's not already in use by *us*
            do {
		$localport = int(rand(65536 - 1025) + 1025)
	    } while (exists $localport{$localport});

            ($session, $error) =
              $class->SUPER::session( -nonblocking => 1,
                                      -localport => $localport,
                                      %arg,
                                    );

        } while ($error =~ /^bind..:/);

    }

    if ($session) {
        # remember it
        $localport{$localport} = 1;
        $session->{_poe_component_snmp_localport} = $localport;
    }

    ($session, $error);
}

# }}} session
# {{{ DESTROY

sub DESTROY {
    my $session = shift;
    if ((my $localport = delete $session->{_poe_component_snmp_localport})) {
        delete $localport{$localport};
    }
}

# }}} DESTROY

package POE::Component::SNMP;

use Carp;
use POE::Session;
use POE::Component::SNMP::Dispatcher; # the real magic starts here

our $DISPATCHER;

# {{{ BEGIN

BEGIN
{
   # Validate the creation of the Dispatcher object.

   if (!defined($DISPATCHER = $Net::SNMP::DISPATCHER = POE::Component::SNMP::Dispatcher->instance)) {
      die('FATAL: Failed to create Dispatcher instance');
   }
}

# }}} BEGIN

# {{{ create

sub create {
    my $class = shift;
    my @arg = @_;
    my %arg; # = @_;

    # my $alias = delete $arg{alias} || delete $arg{Alias} || delete $arg{-alias} || 'snmp';
    my $alias;

    # we don't do alias dupe checks anymore, we leave that to POE
    ($alias, %arg) = _arg_scan(alias => @arg);
    $alias ||= 'snmp';

    # die unless we get a hostname
    unless ( (_arg_scan(hostname => @arg))[0] ) {
        croak "hostname parameter required";
    }

    # make sure we have a dispatcher!
    if (!defined($DISPATCHER = $Net::SNMP::DISPATCHER = POE::Component::SNMP::Dispatcher->instance)) {
        die('FATAL: Failed to create Dispatcher instance');
    }

    my ($session, $error);
    ($session, $error) = POE::Net::SNMP->session( %arg );

    croak $error unless $session;

    POE::Session->create( inline_states => { _start        => \&start_snmp_session,
                                             _stop         => \&end_snmp_session,
                                             finish        => \&close_snmp_session,

                                             get           => \&snmp_get,
                                             getnext       => \&snmp_getnext,
                                             walk          => \&snmp_walk,
                                             getbulk       => \&snmp_getbulk,
                                             trap          => \&snmp_trap,
                                             trap2c        => \&snmp_trap2c,
                                             inform        => \&snmp_inform,
                                             set           => \&snmp_set,

                                             errmsg        => \&snmp_errmsg,
					     callback_args => \&snmp_callback_args,
                                           },
                          args => [
                                   $alias,   # component alias
                                   $session, # Net::SNMP session
                                  ],
			);
}

# }}} create
# {{{ start_snmp_session

sub start_snmp_session {
    my ($kernel, $heap, $alias, $session) = @_[KERNEL, HEAP, ARG0..$#_];

    # make sure we aren't duplicating component aliases!
    # if ($kernel->alias_set($alias)) {
    if ( ! ($POE::VERSION < 0.38 and POE::Kernel::ASSERT_DEFAULT)
         and defined $kernel->alias_resolve($alias)
       ) {
        local $Carp::CarpLevel = 4; # munge up to the right level of code

        croak "A ", __PACKAGE__, " instance called '$alias' already exists!";
    }

    $kernel->alias_set($alias);
    # $heap->{comp_alias}   = $alias;    # component alias
    $heap->{snmp_session} = $session;  # Net::SNMP session
    $heap->{postback_args} = [ $alias, $session->hostname ];
}

# }}} start_snmp_session
# {{{ close_snmp_session

sub close_snmp_session {
    my ($kernel, $session, $heap) = @_[KERNEL, SESSION, HEAP];
    my $snmp_session = $heap->{snmp_session};

    return unless defined $snmp_session;

    if ($snmp_session->debug & 0x08) {
        print "debug: [", __LINE__, "] ", __PACKAGE__, "::close_snmp_session: calling __clear_pending\n";
    }

    # cancel all current and pending requests
    my $rv = $kernel->call(_poe_component_snmp_dispatcher => __clear_pending => $snmp_session);

    # remove our alias... since we have no more pending requests, we
    # will go away now.
    $kernel->alias_remove($_) for $kernel->alias_list( $session );


    # use Data::Dumper; print Dumper $snmp_session;
    # now the only thing keeping this session alive are any postback
    # references that have yet to be delivered.
}

# }}} close_snmp_session
# {{{ end_snmp_session

sub end_snmp_session {
    my ($kernel, $heap) = @_[KERNEL, HEAP];

    $heap->{snmp_session}->close;
}

# }}} end_snmp_session

# {{{ requests

sub snmp_get     { snmp_request( get_request      => @_ ) }
sub snmp_getnext { snmp_request( get_next_request => @_ ) }
sub snmp_walk    { snmp_request( get_table        => @_ ) }
sub snmp_getbulk { snmp_request( get_bulk_request => @_ ) }
sub snmp_inform  { snmp_request( inform_request   => @_ ) }
sub snmp_set     { snmp_request( set_request      => @_ ) }

# }}} requests
# {{{ snmp_request

sub snmp_request {
    # first parameter is the Net::SNMP method to call
    my $method = shift;
    # then standard POE args
    my ($kernel, $heap, $sender, $state_name, @snmp_args) = @_[KERNEL, HEAP, SENDER, ARG0..$#_];

    # extract the PoCo::SNMP request method called, for diagnostics
    # 'POE::Component::SNMP::snmp_get' => 'get'
    my $action = (caller(1))[3]; $action =~ s/POE::Component::SNMP::snmp_//;

    my (@callback_args, $callback_args);
    ($callback_args, @snmp_args) = _arg_scan(callback_args => @snmp_args);

    my $ok = 1;
    # if $callback_args is defined, we got a callback_args in the request.
    if (defined $callback_args) {
        if (ref $callback_args eq 'ARRAY') {
            @callback_args = @$callback_args;
        } else {
            $ok;
            $heap->{snmp_session}->_error("Argument to -callback_args must be an arrayref");
            @callback_args = ($callback_args); # stash the "bad" argument to return with the error
        }
    }

    # do this before the 'set' logic to return an original copy of
    # @snmp_args to the callback.
    my @postback_args = (@{$heap->{postback_args}}, $action, @snmp_args);

    if ($ok) {
        if ($method eq 'set_request') {
            # string => numeric constant processing
            @snmp_args = _dwim_set_request_args(@snmp_args);
        }

	# this $postback is a closure.  it goes away after firing.
        my $postback = $sender->postback($state_name => @postback_args);
        $ok = $heap->{snmp_session}->$method( @snmp_args,
                                              -callback =>
                                              [ sub { $postback->( ( defined ($_[0]->var_bind_list) ?
                                                                     $_[0]->var_bind_list : $_[0]->error
                                                                   ),
                                                                   @callback_args,
                                                                 );
                                                  }
                                              ]
                                            );

    }


    unless ($ok) {
        $kernel->post( $sender => $state_name => \@postback_args,
                       [ $heap->{snmp_session}->error,
                         @callback_args,
                       ]
                     );
    }

}

# }}} snmp_request

# {{{ snmp_trap

# invoke with: $status = $kernel->call( $alias => trap );
sub snmp_trap {
    my ($kernel, $heap, @snmp_args) = @_[KERNEL, HEAP, ARG0..$#_];
    $heap->{snmp_session}->trap( @snmp_args );
}

# }}} snmp_trap
# {{{ snmp_trap2c

# invoke with: $error = $kernel->call( $alias => error );
sub snmp_trap2c {
    my ($kernel, $heap, @snmp_args) = @_[KERNEL, HEAP, ARG0..$#_];
    $heap->{snmp_session}->snmpv2_trap( @snmp_args );
}

# }}} snmp_trap2c

# {{{ snmp_errmsg

# invoke with: $error = $kernel->call( $alias => error );
sub snmp_errmsg { $_[HEAP]{snmp_session}->error }

# }}} snmp_errmsg
# {{{ snmp_callback_args

# invoke with: $kernel->post( $alias => callback_args => @args );
sub snmp_callback_args {
    my ($heap, @args) = @_[HEAP, ARG0..$#_];

    $heap->{callback_args} = \@args;
}

# }}} snmp_callback_args

# internal methods
# {{{ _arg_scan

# scan an array for a key matching qw/ -key key Key KEY / and fetch
# the value. return the value and the remaining arg list minus the
# key/value pair.
sub _arg_scan {
    my ($key, @arg) = @_;

    my $value;
    # scan the @arg for any keys that are callback args.
    for (0..$#arg) {
        if ($arg[$_] =~ /^-?$key$/i) {
            $value = $arg[$_ + 1];

            # splice out the cb args from @arg:
            splice @arg, $_, 2;
        }
    }

    ($value, @arg);
}

# }}} _arg_scan
# {{{ _dwim_set_request_args

# change string constant like 'OCTET_STRING' to a number by calling
# OCTET_STRING()
#
# For a set request, the 2nd item of the varbindlist should be a
# string constant indicating the value type.  This block does a lookup
# of the numeric equivalent and replaces it in the parameter list.
sub _dwim_set_request_args {
    my %snmp_args = @_;

    # extract the varbindlist from args
    my ($vbl) = _arg_scan(varbindlist => @_);

    # make $type refer to the string in $vbl->[1]
    my $type = ref($vbl) eq 'ARRAY' ? \$vbl->[1] : \ 'foo';

    # if Net::SNMP::Message knows about it, use it to replace the
    # string with its numeric equivalent, e.g. 'OCTET_STRING' => 4
    if ( Net::SNMP::Message->can($$type) ) {
        $$type = Net::SNMP::Message->${$type}();
    }

    %snmp_args; # flatten back to a simple list.
}

# }}} _dwim_set_request_args

1;

__END__

=pod

=head1 NAME

POE::Component::SNMP - POE interface to Net::SNMP

=head1 SYNOPSIS

  # this script is included in the distribution as eg/snmp_sample.pl
  use POE qw/Component::SNMP/;

  my %system = ( sysUptime   => '.1.3.6.1.2.1.1.3.0',
                 sysName     => '.1.3.6.1.2.1.1.5.0',
                 sysLocation => '.1.3.6.1.2.1.1.6.0',
               );
  my @oids = values %system;
  my $base_oid = '.1.3.6.1.2.1.1'; # system.*

  POE::Session->create( inline_states =>
                        { _start       => \&_start,
                          snmp_handler => \&snmp_handler,
                        }
                      );

  sub _start {
    my ($kernel, $heap) = @_[KERNEL, HEAP];

    POE::Component::SNMP->create( alias     => 'snmp', # same as default
                                  hostname  => 'localhost',
                                  community => 'public',
                                  version   => 'snmpv2c',
                                  # debug => 0x0A,
                                );

    $kernel->post( snmp => get     => snmp_handler =>
                   -varbindlist    => \@oids );

    # ... or maybe ...

    $kernel->post( snmp => walk    => snmp_handler =>
                   -baseoid        => $base_oid );

    # ... or possibly even ...

    my @callback_args = (1, 2, 3);
    $kernel->post( snmp => getbulk => snmp_handler =>
                   -varbindlist    => [ $base_oid ],
                   -maxrepetitions => 6,
		   -callback_args  => \@callback_args
                 );

    $heap->{pending} = 3;
  }

  sub snmp_handler {
    my ($kernel, $heap, $request, $response) = @_[KERNEL, HEAP, ARG0, ARG1];
    my ($alias, $host, $cmd, @args) = @$request;
    my ($results, @callback_args)   = @$response;

    if (ref $results) {
      print "$host SNMP config ($cmd):\n";
      print "sysName:     $results->{$system{sysName}}\n";
      print "sysUptime:   $results->{$system{sysUptime}}\n";
      print "sysLocation: $results->{$system{sysLocation}}\n";
    } else {
      print "$host SNMP error ($cmd => @args):\n$results\n";
    }

    print "Additional args: @callback_args\n";

    if (--$heap->{pending} == 0) {
      $kernel->post( $alias => 'finish' );
    }
  }

  $poe_kernel->run();

  # see the eg/ folder in the distribution archive for more samples

=head1 DESCRIPTION

POE::Component::SNMP is a L<POE>-ized wrapper around the L<Net::SNMP>
module written by David M. Town.  Most of its arguments aren't even
evaluated by POE, except for C<-alias> and C<-callback_args>, as
described below.

=head1 CREATING SNMP COMPONENTS

=over 4

=item B<create> - create an SNMP session

  POE::Component::SNMP->create(
      hostname  => $hostname,   # required
     [alias     => $alias,    ] # default 'snmp'
     [community => $community,] # default 'public'
     [version   => $version,  ] # default '1', SNMPv1
     [timeout   => $timeout,  ] # default 5.0 (seconds)
     [retries   => $retries,  ] # default 1
     [debug     => $debug,    ] # default 0
     [ ... any other arguments Net::SNMP recognizes ... ]
  );

C<create()> passes all of its arguments to the constructor for a
L<Net::SNMP> object untouched with the exception of C<-alias>.  See
L<Net::SNMP::session()|Net::SNMP/session() - create a new Net::SNMP
object>.  The constructor supports either of the following two
parameter naming styles:

  $object->method(-parameter => $value);
  $object->method( parameter => $value);

C<-hostname> is required.  This differs from the behavior in Net::SNMP
which is to default to C<'localhost'>.

C<-alias> is not required unless you want to query more than one host.
See L</Concurrency>, below.

=back

=head2 Concurrency

In order to access multiple SNMP hosts simultaneously, you must create
a separate instance of the component for each host, by giving each
component a different C<-alias> parameter in the constructor.

The C<-alias> and C<-hostname> parameters, as well as additional
request-specific data, are passed back to callback events, as
described in L</CALLBACKS> below, so the callback can determine what
context the current response (or timeout) is related to.

B<NOTE:> It is an error to attempt to create more than one SNMP
session with the same C<-alias>.  It's not fatal unless you run POE
with ASSERT_USAGE, but it won't work regardless.

=head2 Sockets

By default, L<Net::SNMP> creates a single socket per I<network
interface>.  This is possible because the L<Net::SNMP> event loop
processes all SNMP requests in FIFO order and is thus able to reuse
the same socket for each request; however, it is not asynchronous.
Since we can only watch one connection per socket at a time, this
creates a conflict if you want to contact more than one remote host
simultaneously.  The workaround used by the module is to create each
socket using a different randomly generated value for the
C<-localport> parameter, specifying a unique local UDP port for each
instance of the component.  This could potentially interfere with
remote communications if your local firewall policy requires a
specific source port for outgoing SNMP requests (as noted by David
Town, the author of L<Net::SNMP>).  In this situation, you can supply
an explicit C<-localport> argument to the constructor, but remember
that every active session requires its own I<unique> local port per
session/host, per interface.

=head1 REQUESTS

Most of the events accept a list of arguments which are passed
directly to a L<Net::SNMP> session.  See L<Net::SNMP/METHODS> for more
information on these arguments.

Requests take the form:

  $poe_kernel->post( $session_alias => $request =>
                     $callback_state => @snmp_args );

See the SYNOPSIS for specific examples.

=over 4

=item C<get>

See L<Net::SNMP::get_request()|Net::SNMP/get_request() - send a SNMP get-request to the remote agent>.

=item C<getnext>

See L<Net::SNMP::get_next_request()|Net::SNMP/get_next_request() - send a SNMP get-next-request to the remote agent>.

=item C<getbulk>

See L<Net::SNMP::get_bulk_request()|Net::SNMP/get_bulk_request() - send a SNMP get-bulk-request to the remote agent>.

=item C<walk>

See L<Net::SNMP::get_table()|Net::SNMP/get_table() - retrieve a table from the remote agent>.

=item C<inform>

See L<Net::SNMP::inform_request()|Net::SNMP/inform_request() - send a SNMP inform-request to the remote manager>.

=item C<set>

See L<Net::SNMP::set_request()|Net::SNMP/set_request() - send a SNMP set-request to the remote agent>.

=item C<trap>

  $kernel->post( snmp => trap => @snmp_args );
  # or, even better:
  my $status = $kernel->call( snmp => trap => @snmp_args );

Send a SNMPv1 trap message.  See L<Net::SNMP::trap()|Net::SNMP/trap()
- send a SNMP trap to the remote manager>.  This method differs from
the requests in that it does I<not> take a state name as a callback
parameter.  If the method is invoked with
L<POE::Kernel::call()|POE::Kernel/call SESSION, EVENT_NAME,
PARAMETER_LIST>, the return value is that of
L<Net::SNMP::trap()|Net::SNMP/trap() - send a SNMP trap to the remote
manager>. A false value indicates an error, and the error message can
be retrieved using C<errmsg>, below.

=item C<trap2c>

  $kernel->post( snmp => trap2c => @snmp_args );
  # or, even better:
  my $status = $kernel->call( snmp => trap2c => @snmp_args );

Send a SNMPv2c trap message.  See
L<Net::SNMP::snmpv2_trap()|Net::SNMP/snmpv2_trap() - send a SNMP
snmpV2-trap to the remote manager>.  This method differs from the
others in that it does I<not> take a state name as a callback
parameter.  If the method is invoked with C<POE::Kernel::call()>, the
return value is that of
L<Net::SNMP::snmpv2_trap()|Net::SNMP/snmpv2_trap() - send a SNMP
snmpV2-trap to the remote manager>. A false value indicates an error,
and the error message can be retrieved using calling C<errmsg>, below.

=item C<errmsg>

  my $last_snmp_error_message = $kernel->call( snmp => 'errmsg' );

Retrieves the last error message, if any, from the specified SNMP
session.

=item C<finish>

  $kernel->post( snmp => 'finish' );

Shut down the SNMP component.  Cancels all current and pending
requests immediately and closes the session.  If the component is
currently dispatching a request (waiting for a reply) when this
request is received, the response NOT be delivered.

=back

=head1 CALLBACKS

When a request receives a response (or times out), the supplied
callback event (a POE event name defined in the session that called
the SNMP component) is invoked.  (See
L<POE::Session|POE::Session/PREDEFINED EVENT FIELDS> for more
information about $_[_ARG0] and $_[_ARG1])

The callback's C<$_[ARG0]> parameter is an array reference containing
the request information: the component alias, hostname, the method
called (e.g. 'get'), and parameters supplied to the request.

The callback's C<$_[ARG1]> parameter is an array reference containing
the response information.  The first element (C<$_[ARG1][0]>) is
I<either> a hash reference containing response data I<or> a scalar
error message string.  If any arguments have been passed to the
request via C<-callback_args> (below), they will be returned as
additional elements in C<$_[ARG1]>.

B<NOTE:> This is a change from older versions of the module!
Previously, errors were returned in C<$_[ARG1][1]>.

=over

=item C<-callback_args>

  # $callback_state receives @args in $_[_ARG1]
  $kernel->post( $alias => get => $callback_state =>
                 -callback_args => \@args,
                 -varbindlist   => \@oids );

This optional parameter to all component requests returning a response
sets a list of additional values to be passed to the POE state as
parameters.  The argument must be an array reference, which will be
dereferenced as a list of additional response parameters after the
SNMP response data.

=back

=head1 SEE ALSO

  Net::SNMP
  POE

=head1 AUTHOR

Adopted and maintained by Rob Bloodgood E<lt>rdb@cpan.orgE<gt>

Originally by Todd Caine E<lt>tcaine@eli.netE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2004-2006 by Rob Bloodgood

Copyright 2003 by Todd Caine

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
