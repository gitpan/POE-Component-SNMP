use Test::More; # qw/no_plan/;
use strict;

# BEGIN { use_ok 'POE::Component::SNMP' };

BEGIN {
    use_ok POE => qw/Component::SNMP/;
    # use_ok 'POE::Component::SNMP';
}

my $CONF = do "config.cache";

if ( $CONF->{skip_all_tests} ) {
    plan skip_all => 'No SNMP data specified.';
} else {
    if (1) {
        plan tests => 11;
    } else {
        $poe_kernel->run(); # quiets the warning about POE::Kernel
                            # never being run.
        plan skip_all => 'not done yet';
    }
}

my %system = ( # sysUptime   => '.1.3.6.1.2.1.1.3.0',
               sysName     => '.1.3.6.1.2.1.1.5.0',
               # sysLocation => '.1.3.6.1.2.1.1.6.0',
             );

my @oids = values %system;
my $base_oid = '.1.3.6.1.2.1.1'; # system.*

my $DEBUG_FLAG = 0x00; # none
# my $DEBUG_FLAG = 0x08; # dispatcher
# my $DEBUG_FLAG = 0x0B; # transport+dispatcher
# my $DEBUG_FLAG = 0x1B; # transport+dispatcher+message processing
# my $DEBUG_FLAG = 0xFF; # everything

my $session2 = 1;

sub snmp_run_tests {
    my ($kernel, $heap) = @_[KERNEL, HEAP];

    # no warnings;
    POE::Component::SNMP->create(
                                 alias     => 'snmp',
                                 hostname  => $CONF->{'hostname'},
                                 community => $CONF->{'community'},
                                 # nolocalport => 1,
                                 debug     => $DEBUG_FLAG, # * $ENV{TRACE_DISPATCHER}, # transport + dispatcher

                                 # timeout   => 5,

                                );
    ok $kernel->alias_resolve( 'snmp' ), "1st session created";
    # use warnings;

  SKIP: {
        skip "only testing with one for now", 1 unless $session2;

        POE::Component::SNMP->create(
                                     alias     => 'snmp_2',
                                     hostname  => # 'count1' || 
                                     $CONF->{'hostname'},
                                     community => $CONF->{'community'},
                                     # nolocalport => 1,
                                     # localport => $localport, # intentionally duplicate port number
                                     debug     => $DEBUG_FLAG, # * $ENV{TRACE_DISPATCHER},

                                     # timeout   => 5,
                                    );


        # ok $@, '-hostname parameter required';
        # this asserts that the alias does *not* exist
        ok $kernel->alias_resolve( 'snmp_2' ), "2st session created";

    }
    ;




    # 'walk' takes longer to return than 'get'. So we do it first to
    # arrange that the response to the second request, 'get', comes
    # BEFORE the first request, 'walk'.
    # $kernel->post( snmp => walk => walk_cb => -baseoid => $base_oid ); $heap->{pending}++;
    if ($session2) {
        $kernel->post( snmp_2 => get   => get_cb2   => -varbindlist => \@oids ); $heap->{pending}{snmp_2}++;
        # $kernel->post( snmp_2 => walk  => walk_cb2  => -baseoid => $base_oid ); $heap->{pending}{snmp_2}++;

        $kernel->post( snmp_2 => get   => get_cb   => -varbindlist => \@oids ); $heap->{pending}{snmp_2}++;
        # $kernel->post( snmp_2 => walk  => walk_cb  => -baseoid => $base_oid ); $heap->{pending}{snmp_2}++;
    }

    $kernel->post( snmp   => get   => get_cb  => -varbindlist => \@oids ); $heap->{pending}{snmp}++;
    # $kernel->post( snmp   => walk  => walk_cb => -baseoid => $base_oid ); $heap->{pending}{snmp}++;

    $kernel->post( snmp   => get   => get_cb2 => -varbindlist => \@oids ); $heap->{pending}{snmp}++;

    # $kernel->post( snmp_2 => get   => get_cb  => -varbindlist => \@oids ); $heap->{pending}{snmp}++;


}

sub get_cb {
    my ($kernel, $heap, $request, $response) = @_[KERNEL, HEAP, ARG0, ARG1];
    my ($alias,   $host, $cmd, @args) = @$request;
    my ($results)                     = @$response;

  SKIP: {
        skip "alias checks", 1;
        ok $alias eq 'snmp', "'snmp' session got alias '$alias'";
    }
    ok $cmd eq 'get', "callback destination is preserved (get)";

    if (1) {
        if (ref $results) {
            if (0) {
                print "$host SNMP config ($cmd):\n";
                print "sysName:     $results->{$system{sysName}}\n";
                print "sysUptime:   $results->{$system{sysUptime}}\n";
                print "sysLocation: $results->{$system{sysLocation}}\n";
            }
        } else {
            print STDERR "$host SNMP error ($cmd => @args):\n$results\n";
        }
    }

    if (--$heap->{pending}{$alias} == 0) {
        $kernel->post( $alias => 'finish' );
    }

}

sub get_cb2 {
    my ($kernel, $heap, $request, $response) = @_[KERNEL, HEAP, ARG0, ARG1];
    my ($alias,   $host, $cmd, @args) = @$request;
    my ($results)                     = @$response;

  SKIP: {
        skip "alias checks", 1;
        ok $alias eq 'snmp_2', "'snmp_2' session got alias '$alias'";
    }
    ok $cmd eq 'get', "callback destination is preserved (get)";

    if (--$heap->{pending}{$alias} == 0) {
        $kernel->post( $alias => 'finish' );
    }

}

sub walk_cb {
    my ($kernel, $heap, $request, $response) = @_[KERNEL, HEAP, ARG0, ARG1];
    my ($alias,   $host, $cmd, @args) = @$request;
    my ($results)                     = @$response;

  SKIP: {
        skip "alias checks", 1;
        ok $alias eq 'snmp', "'snmp' session got alias '$alias'";
    }
    ok $cmd eq 'walk', "callback destination is preserved (walk)";

    if (--$heap->{pending}{$alias} == 0) {
        $kernel->post( $alias => 'finish' );
    }

}

sub walk_cb2 {
    my ($kernel, $heap, $request, $response) = @_[KERNEL, HEAP, ARG0, ARG1];
    my ($alias,   $host, $cmd, @args) = @$request;
    my ($results)                     = @$response;

  SKIP: {
        skip "alias checks", 1;
        ok $alias eq 'snmp_2', "'snmp_2' session got alias '$alias'";
    }
    ok $cmd eq 'walk', "callback destination is preserved (walk)";

    if (--$heap->{pending}{$alias} == 0) {
        $kernel->post( $alias => 'finish' );
    }
}

sub stop_session {
    # print STDERR "$_[STATE]: --MARK--\n";
    ok 1, "clean shutdown";
}

### declarations done. let's run it!

POE::Session->create
( inline_states =>
  { _start   => \&snmp_run_tests,
    _stop    => \&stop_session,
    get_cb   => \&get_cb,
    get_cb2  => \&get_cb2,
    walk_cb  => \&walk_cb,
    walk_cb2 => \&walk_cb2,
  },
);

$poe_kernel->run;

exit 0;

