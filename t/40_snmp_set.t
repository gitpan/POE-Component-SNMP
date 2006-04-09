
use Test::More;

use POE;
use POE::Component::SNMP;

my $sysContact = '.1.3.6.1.2.1.1.4.0';

my $CONF = do "config.cache";

if( $CONF->{skip_all_tests} ) {
    POE::Kernel->run(); # quiets warning: POE::Kernel's run() method was never called.
    plan skip_all => 'No SNMP data specified.';
} elsif ( not length $CONF->{wcommunity} ) {
    POE::Kernel->run(); # quiets warning: POE::Kernel's run() method was never called.
    plan skip_all => 'No write community specified.';
} else {
    plan tests => 1;
}

POE::Session->create
( inline_states =>
  {
    _start      => \&snmp_set_tests,
    snmp_set_cb => \&snmp_set_cb,
  },
);

$poe_kernel->run;

exit 0;


sub snmp_set_tests {
    my ($kernel, $heap) = @_[KERNEL, HEAP];

    POE::Component::SNMP->create(
        alias     => 'snmp',
        hostname  => $CONF->{hostname} || 'localhost',
        community => $CONF->{wcommunity} || 'public',
        version   => 'snmpv2c',
        debug     => 0,
    );

    $kernel->post(
        snmp => 'set',
        'snmp_set_cb',
        -varbindlist => [$sysContact, 'OCTET_STRING', 'support@eli.net'],
    );
}

sub snmp_set_cb {
    my ($kernel, $args) = @_[KERNEL, ARG1];
    my $results = $args->[0];
    is $results->{$sysContact}, 'support@eli.net';
    $_[KERNEL]->post( snmp => 'finish' );
}

