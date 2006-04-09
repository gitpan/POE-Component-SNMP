package POE::Component::SNMP;

$VERSION = '0.95';

use strict;

use Net::SNMP ();
use POE::Session;
use POE::Component::SNMP::Dispatcher;

use Carp;

sub create {
    my $class = shift;
    my %arg = @_;

    my $alias = delete $arg{alias} || delete $arg{-alias} || 'snmp';

    # bitch unless we get a community
    unless (exists $arg{community} || exists $arg{-community}) {
        carp "using default value 'public' for missing -community parameter";
        $arg{-community} = 'public';
    }

    unless (exists $arg{hostname} || exists $arg{-hostname}) {
        croak "-hostname parameter required";
    }

    my ($session, $error);

    # each session binds to a different local port/socket.  This
    # do..while loop catches any potential port conflicts.
    do {
	($session, $error) =
          Net::SNMP->session( -nonblocking => 1,
                              -localport => int(rand(65536 - 1025) + 1025 ),
                              %arg,
                            );
    } while ($error =~ /^bind..:/);

    croak $error unless $session;

    POE::Session->create( inline_states => { _start   => \&start_snmp_session,
                                             # _stop    => \&end_snmp_session,
                                             finish   => \&close_snmp_session,

                                             get      => \&snmp_get,
                                             getnext  => \&snmp_getnext,
                                             walk     => \&snmp_walk,
                                             getbulk  => \&snmp_getbulk,
                                             trap     => \&snmp_trap,
                                             inform   => \&snmp_inform,
                                             set      => \&snmp_set,

                                             errmsg   => \&snmp_last_error_message,
                                           },
                          args => [
                                   $alias,   # component alias
                                   $session, # Net::SNMP session
                                   $error,   # Net::SNMP error
                                  ],
			);

}

# invoke with: $error = $kernel->call( $alias => error );
sub snmp_last_error_message { $_[HEAP]{snmp_session}->error }

sub start_snmp_session {
    my ($kernel, $heap, $alias, $session, $error) = @_[KERNEL, HEAP, ARG0..$#_];

    # make sure we aren't duplicating component aliases!
    # if ($kernel->alias_set($alias)) {
    if (eval { $kernel->alias_resolve($alias) }) {
        local $Carp::CarpLevel = 4; # munge up to the right level of code

        croak "A ", __PACKAGE__, " instance called '$alias' already exists!";
    }

    $kernel->alias_set($alias);
    $heap->{comp_alias}   = $alias;    # component alias
    $heap->{snmp_session} = $session;  # Net::SNMP session
    $heap->{postback_args} = [ $alias, $session->hostname ];
}

sub close_snmp_session {
    my ($kernel, $heap) = @_[KERNEL, HEAP];
    my $snmp_session = delete $heap->{snmp_session};
    $snmp_session->close;
    my $alias = delete $heap->{comp_alias};
    $kernel->alias_remove($alias);
}

sub end_snmp_session {
    my ($kernel, $heap) = @_[KERNEL, HEAP];
}

sub snmp_get     { snmp_request( get_request      => @_ ) }
sub snmp_getnext { snmp_request( get_next_request => @_ ) }
sub snmp_walk    { snmp_request( get_table        => @_ ) }
sub snmp_getbulk { snmp_request( get_bulk_request => @_ ) }
sub snmp_inform  { snmp_request( inform_request   => @_ ) }
sub snmp_set     { snmp_request( set_request      => @_ ) }

sub snmp_request {
    my $method = shift;
    my ($kernel, $heap, $sender, $state_name, @snmp_args) = @_[KERNEL, HEAP, SENDER, ARG0..$#_];

    # extract the PoCo::SNMP request method called, for diagnostics
    my $action = (caller(1))[3];  # POE::Component::SNMP::snmp_get
    $action =~ s/.*_//;           # => 'get'

    # do this before the 'set' logic to return an original copy of @snmp_args to the callback
    my $postback = $sender->postback($state_name => @{$heap->{postback_args}}, $action, [ @snmp_args ]);

    if ($action eq 'set') {
        # string => numeric constant processing
        @snmp_args = _dwim_set_request_args(@snmp_args);
    }

    my $ok = $heap->{snmp_session}->$method( @snmp_args,
                                             -callback =>
                                             sub { $postback->( $_[0]->var_bind_list, $_[0]->error ) },
                                    );

    $kernel->post( $sender => $state_name =>
                   [ @{$heap->{postback_args}},
                     $action => [ $_[ARG1..$#_] ] # original, unmodified @snmp_args
                   ],
                   [ $heap->{snmp_session}->var_bind_list,
                     $heap->{snmp_session}->error
                   ]
                 ) unless $ok;
}

sub snmp_trap {
    my ($kernel, $heap, @snmp_args) = @_[KERNEL, HEAP, ARG0..$#_];
    my $ok = $heap->{snmp_session}->trap( @snmp_args );
    # carp $heap->{snmp_session}->error unless $ok;

    $ok; # if we get call()'d, pass on the return value
}

# change string constant like 'OCTET_STRING' to a number

# For a set request, the 2nd item of the varbindlist should be a
# string constant indicating the value type.  This block does a lookup
# of the numeric equivalent and replaces it in the parameter list.

sub _dwim_set_request_args {
    my %snmp_args = @_;

    # extract the varbindlist from args
    my $vbl = exists $snmp_args{varbindlist}
      ? $snmp_args{varbindlist}
        : $snmp_args{-varbindlist};

    # make $type refer to the string constant
    my $type = ref($vbl) eq 'ARRAY' ? \$vbl->[1] : \'foo';

    # if Net::SNMP::Message knows about it, use it to replace the
    # string with its numeric equivalent, e.g. 'OCTET_STRING' => 4
    if ( Net::SNMP::Message->can($$type) ) {
        $$type = Net::SNMP::Message->${$type}();
    }

    %snmp_args; # flatten back to a simple list.
}

1;

__END__


=pod

=head1 NAME

POE::Component::SNMP - POE interface to Net::SNMP

=head1 SYNOPSIS

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
				      timeout   => 3,
                                    );

        $kernel->post( snmp => get  => snmp_handler => -varbindlist => \@oids );
        # ... or maybe even ...
        $kernel->post( snmp => walk => snmp_handler => -baseoid => $base_oid );

        $heap->{pending} = 2;
    }

    sub snmp_handler {
        my ($kernel, $heap, $request, $response) = @_[KERNEL, HEAP, ARG0, ARG1];
        my ($alias,   $host, $cmd, @args) = @$request;
        my ($results, $error)             = @$response;

        if ($error) {
            warn "$host snmp error ($cmd => @args): $error\n";
        } else {
	    print "$host SNMP config ($cmd):\n";
	    if (ref $results) {
		print "sysName:     $results->{$system{sysName}}\n";
		print "sysUptime:   $results->{$system{sysUptime}}\n";
		print "sysLocation: $results->{$system{sysLocation}}\n";
	    } else {
		print "server response: $results\n";
	    }
        }

        if (--$heap->{pending} == 0) {
            $kernel->post( $alias => 'finish' );
        }
    }

    $poe_kernel->run();

=head1 DESCRIPTION

POE::Component::SNMP is a L<POE>-ized wrapper around the L<Net::SNMP>
module written by David M. Town.  Most of its arguments aren't even
evaluated by POE, except for C<alias>, as described below.

=head1 CREATING SNMP COMPONENTS

=over 4

=item B<create> - create an SNMP session

  POE::Component::SNMP->create(
      hostname  => $hostname,   # required
     [alias     => $alias,    ] # default 'snmp'
     [community => $community,] # default 'public', with a warning
     [version   => $version,  ] # default '1', SNMPv1
     [timeout   => $timeout,  ] # default 5.0
     [retries   => $retries,  ] # default 1
     [debug     => $debug,    ] # default 0
     [ ... any other arguments Net::SNMP recognizes ... ]
  );

B<create()> passes all of its arguments to the constructor for a
L<Net::SNMP> object untouched with the exception of C<alias>.  See
L<Net::SNMP::session()> for details.  The constructor supports
either of the following two parameter naming styles:

  $object->method(-parameter => $value);
  $object->method( parameter => $value);

C<hostname> is required.

C<community> defaults to 'public', but the module will emit a runtime
warning if the community is not explicitly specified.

C<alias> is not required unless you want to query more than one host.
See L<Concurrency>, below.

=back

=head2 Concurrency

In order to access multiple SNMP hosts simultaneously, you must create
a separate instance of the component for each host, by giving each
component a different C<alias> parameter in the constructor.

The C<alias> and C<hostname> parameters, as well as other values, are
passed back to callback events, as described in L<CALLBACKS> below, so
the callback can determine which host the current response (or
timeout) is related to.

Note: It is a fatal runtime error to attempt to create more than one
SNMP session with the same C<alias>.

=head2 Sockets

By default, L<Net::SNMP> creates a single socket per I<network
interface>.  Since POE can only watch one connection per socket at a
time, this creates a conflict if you want to contact more than one
remote host simultaneously.  The workaround used by the module is to
create each socket using a different randomly generated value for the
C<localport> parameter, specifying a unique local UDP port for each
host.  This could potentially interfere with remote communications if
your local firewall policy requires a specific source port for
outgoing SNMP requests (as noted by David Town).  In this situation,
you can supply an explicit C<localport> argument to the constructor,
but remember that every active session requires its own I<unique>
local port per session/host, per interface.

=head1 REQUESTS

Most of the events accept a list of arguments which are passed
directly to a L<Net::SNMP> session.  See L<Net::SNMP/METHODS> for more
information on these arguments.

Requests take the form:

  $poe_kernel->post( $session_alias => $request => $callback_state => @snmp_args );

See the SYNOPSIS for specific examples.

=over 4

=item B<get>

See L<Net::SNMP::get_request()>.

=item B<getnext>

See L<Net::SNMP::get_next_request()>.

=item B<getbulk>

See L<Net::SNMP::get_bulk_request()>.

=item B<walk>

See L<Net::SNMP::get_table()>.

=item B<inform>

See L<Net::SNMP::inform_request()>.

=item B<set>

See L<Net::SNMP::set_request()>.

=item B<trap>

  $kernel->post( snmp => trap => @snmp_args );
  # or, even better:
  my $status = $kernel->call( snmp => trap => @snmp_args );

Send a SNMPv1 trap message.  See L<Net::SNMP::trap()>.  This method
differs from the others in that it does *not* take a state name as a
callback parameter.  If the method is invoked with C<$kernel-E<gt>call()>,
the return value is that of L<Net::SNMP::trap()>. A false value
indicates an error, and the error message can be retrieved using
C<errmsg>, below.

=item B<errmsg>

  my $last_snmp_error_message = $kernel->call( snmp => 'errmsg' );

Retrieves the last SNMP error message, if any, from the specified SNMP
session.

=item B<finish>

  $kernel->post( snmp => 'finish' );

Shut down the SNMP component.  Cancels any pending requests and closes
the session.

=back

=head1 CALLBACKS

When a request either receives a response or times out, the supplied
callback event (a POE event name defined in the session that called
the SNMP component) is invoked.

The callback's C<$_[ARG0]> parameter is an array reference containing the
request information: the component alias, hostname, the method called,
and parameters supplied to the request.

The callback's C<$_[ARG1]> parameter is an array reference containing the
response information: a scalar *or* a hash reference containing response
data, and a string error message (which is only defined if the request
failed).

Note: A timeout (no response) or other server-side fault condition is
I<NOT> returned in $error, e.g. C<$_[ARG1][1]>.  It is returned as a
scalar string result, I<NOT> a hash ref, in C<$_[ARG1][0]>.  Meaning,
if you define C<($data, $error) = @$response> as in the SYNOPSIS,
check that C<ref $data eq 'HASH'> first before trying to dereference
it.

=head1 SEE ALSO

  Net::SNMP
  POE

=head1 AUTHOR

Adopted and maintained by Rob Bloodgood
E<lt>rob@exitexchange.comE<gt>

Originally by Todd Caine E<lt>tcaine@eli.netE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2004-2006 by Rob Bloodgood

Copyright 2003 by Todd Caine

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
