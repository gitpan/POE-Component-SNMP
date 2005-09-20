package POE::Component::SNMP;

$VERSION = '0.93';

use strict;

use Net::SNMP;
use POE::Session;
use POE::Component::SNMP::Dispatcher;

sub create {
    my $class = shift;
    my %arg = @_;

    my $alias = delete $arg{alias} || delete $arg{-alias} || 'snmp';
    my ($session, $error);

    # each session binds to a different local port/socket.  This
    # do..while loop catches any potential port conflicts.
    do {
	($session, $error) = Net::SNMP->session( -nonblocking => 1,
						 -localport => int(rand(65536 - 1025) + 1025 ),
						 %arg,
					       );
    } while ($error =~ /^bind..:/);
    warn $error if $error;

    POE::Session->create(
			 inline_states => {
					   _start   => \&start_snmp_session,
					   _stop    => \&end_snmp_session,
					   finish   => \&close_snmp_session,
					   # dispatch => \&dispatch_events,

					   get      => \&snmp_get,
					   getnext  => \&snmp_getnext,
					   walk     => \&snmp_walk,
					   getbulk  => \&snmp_getbulk,
					   trap     => \&snmp_trap,
					   inform   => \&snmp_inform,
					   set      => \&snmp_set,
					  },
			 args => [
				  $alias,               # component alias
				  $session,             # Net::SNMP session
				  $error,               # Net::SNMP error

				  # DEPRECATED - all instances of
				  # "delayed" are assumed to be false.
				  # SNMP queries are dispatched
				  # immediately, via POE's event loop,
				  # as soon as they are enqueued.

 				  # exists($arg{delayed})	# SNMP dispatch option
 				  # ? $arg{delayed}
 				  # : 1,

				 ],
			);

    return;
}

sub start_snmp_session {
    my ($kernel,
        $heap,
        $alias,
        $session,
        $error) = @_[KERNEL, HEAP, ARG0..ARG3];
    $kernel->alias_set($alias);
    $heap->{comp_alias}   = $alias;    # component alias
    $heap->{snmp_session} = $session;  # Net::SNMP session

    $heap->{postback_args} = [ $alias, $session->hostname ];
}

sub close_snmp_session {
    my ($kernel, $heap) = @_[KERNEL, HEAP];
    my $snmp_session = delete $heap->{snmp_session};
    $snmp_session->close;
}

sub end_snmp_session {
    my ($kernel, $heap) = @_[KERNEL, HEAP];
    my $alias = delete $heap->{comp_alias};
    $kernel->alias_remove($alias);
}

sub snmp_get {
    my ($kernel,
        $heap,
        $sender,
        $state_name,
        @snmp_args) = @_[KERNEL, HEAP, SENDER, ARG0..$#_];
    my $postback = $sender->postback($state_name, @{$heap->{postback_args}});
    my $ok = $heap->{snmp_session}->get_request(
        @snmp_args, -callback => sub { $postback->( $_[0]->var_bind_list, $_[0]->error) },
    );
    warn $heap->{snmp_session}->error unless $ok;
}

sub snmp_getnext {
    my ($kernel,
        $heap,
        $sender,
        $state_name,
        @snmp_args) = @_[KERNEL, HEAP, SENDER, ARG0..$#_];
    my $postback = $sender->postback($state_name, @{$heap->{postback_args}});
    my $ok = $heap->{snmp_session}->get_next_request(
        @snmp_args, -callback => sub { $postback->( $_[0]->var_bind_list, $_[0]->error ) },
    );
    warn $heap->{snmp_session}->error unless $ok;
}

sub snmp_walk {
    my ($kernel,
        $heap,
        $sender,
        $state_name,
        @snmp_args) = @_[KERNEL, HEAP, SENDER, ARG0..$#_];
    my $postback = $sender->postback($state_name, @{$heap->{postback_args}});
    my $ok = $heap->{snmp_session}->get_table(
        @snmp_args, -callback => sub { $postback->( $_[0]->var_bind_list, $_[0]->error ) },
    );
    warn $heap->{snmp_session}->error unless $ok;
}

sub snmp_getbulk {
    my ($kernel,
        $heap,
        $sender,
        $state_name,
        @snmp_args) = @_[KERNEL, HEAP, SENDER, ARG0..$#_];
    my $postback = $sender->postback($state_name, @{$heap->{postback_args}});
    my $ok = $heap->{snmp_session}->get_bulk_request(
        @snmp_args, -callback => sub { $postback->( $_[0]->var_bind_list, $_[0]->error ) },
    );
    warn $heap->{snmp_session}->error unless $ok;
}

sub snmp_set {
    my ($kernel,
        $heap,
        $sender,
        $state_name,
        %snmp_args) = @_[KERNEL, HEAP, SENDER, ARG0..$#_];

    my $vbl = exists $snmp_args{varbindlist}
        ? $snmp_args{varbindlist}
        : $snmp_args{-varbindlist};

    my $type = ref($vbl) eq 'ARRAY' ? \$vbl->[1] : \'foo';
    if ( Net::SNMP::Message->can($$type) ) {
        $$type = Net::SNMP::Message->${$type}();
    }

    my $postback = $sender->postback($state_name, @{$heap->{postback_args}});
    my $ok = $heap->{snmp_session}->set_request(
        %snmp_args, -callback => sub { $postback->( $_[0]->var_bind_list, $_[0]->error ) },
    );
    warn $heap->{snmp_session}->error unless $ok;
}

sub snmp_trap {
    my ($kernel,
        $heap,
        @snmp_args) = @_[KERNEL, HEAP, ARG0..$#_];
    my $ok = $heap->{snmp_session}->trap( @snmp_args );
    warn $heap->{snmp_session}->error unless $ok;
}

sub snmp_inform {
    my ($kernel,
        $heap,
        $sender,
        $state_name,
        @snmp_args) = @_[KERNEL, HEAP, SENDER, ARG0..$#_];
    my $postback = $sender->postback($state_name, @{$heap->{postback_args}});
    my $ok = $heap->{snmp_session}->inform_request(
        @snmp_args, -callback => sub { $postback->( $_[0]->var_bind_list, $_[0]->error ) },
    );
    warn $heap->{snmp_session}->error unless $ok;
}

# DEPRECATED -- this happens automatically
# sub dispatch_events {
#     my $heap = $_[HEAP];
#     # $heap->{snmp_session}->snmp_dispatcher if $heap->{delayed};
# }


1;
__END__

=pod

=head1 NAME

POE::Component::SNMP - event-driven SNMP interface

=head1 SYNOPSIS

  use POE::Component::SNMP;

  POE::Component::SNMP->create(
      hostname  => 'my.host.org',
      community => 'public',
  );

  $kernel->post( snmp => 'get',     'snmp_callback', -varbindlist => [$oid] );
  $kernel->post( snmp => 'getnext', 'snmp_callback', -varbindlist => [$oid] );
  $kernel->post( snmp => 'walk',    'snmp_callback', -baseoid => $base_oid );
  $kernel->post( snmp => 'getbulk', 'snmp_callback', %snmp_args );

  sub snmp_callback {
      my ($results, $error) = @{$_[ARG1]};

      if ($error) {
          warn "error: $error\n";
      }
      else {
          print "sysUptime: " . $results->{$system{sysUptime}} . "\n";
          print "sysLocation: " . $results->{$system{sysLocation}} . "\n";
      }
  }

=head1 DESCRIPTION

This module is an event-driven SNMP interface for POE.

POE::Component::SNMP uses the Net::SNMP module written by David M. Town.

=head1 CREATING SNMP COMPONENTS

=over 4

=item B<create> - creates a POE::Component::SNMP session

  POE::Component::SNMP->create(
      hostname  => $hostname,
      [community => $community,]
      [alias    => $alias,]
      [version  => $version,]
      [timeout  => $timeout,]
      [retries  => $retries,]
      [debug    => $debug,]
  );

=back

=head1 SNMP COMPONENT EVENTS

Most of the events accept a list of arguments, I<@snmp_args>, which are passed directly to a Net::SNMP session.  See the Net::SNMP documentation for more information on these arguments.

=over 4

=item get

  $kernel->post( snmp => 'get', 'snmp_callback', @snmp_args );

Send a SNMP get request event.

=item getnext

  $kernel->post( snmp => 'getnext', 'snmp_callback', @snmp_args );

Send a SNMP getnext request event.

=item walk

  $kernel->post( snmp => 'walk', 'snmp_callback', @snmp_args );

Send a SNMP walk request event.

=item getbulk

  $kernel->post( snmp => 'getbulk', 'snmp_callback', @snmp_args );

Send a SNMP getbulk request event.

=item trap

  $kernel->post( snmp => 'trap', @snmp_args );

Send a SNMPv1 trap request event.

=item inform

  $kernel->post( snmp => 'inform', 'snmp_callback', @snmp_args );

Send a SNMP inform request event.

=item set

  $kernel->post( snmp => 'set', @snmp_args );

Send a SNMP set request event.

=item finish

  $kernel->post( snmp => 'finish' );

Shutdown the SNMP component.

=back

=head1 SEE ALSO

  Net::SNMP
  POE
  POE::Session
  POE::Kernel

=head1 AUTHOR

Originally by Todd Caine E<lt>tcaine@eli.netE<gt>.

Adopted and maintained by Rob Bloodgood
E<lt>rob@exitexchange.comE<gt>.

=head1 COPYRIGHT AND LICENSE

Copyright 2003 by Todd Caine, 2004-2005 by Rob Bloodgood.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
