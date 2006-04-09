
use Test::More;

BEGIN { use_ok('POE::Component::SNMP') };

use POE;
use POE::Component::SNMP;

my $CONF = do "config.cache";

if( $CONF->{skip_all_tests} ) {
    plan skip_all => 'No SNMP data specified.';
}
else {
    plan tests => 7;
}


POE::Session->create
( inline_states =>
  {
    _start      => \&snmp_get_tests,
    _stop       => \&stop_session,
    snmp_get_cb => \&snmp_get_cb,
  },
);

$poe_kernel->run;

exit 0;


sub snmp_get_tests {
    my ($kernel, $heap) = @_[KERNEL, HEAP];

    POE::Component::SNMP->create(
        alias     => 'snmp',
        hostname  => $CONF->{'hostname'},
        community => $CONF->{'community'},
        version   => 'snmpv2c',
        debug     => 0,
    );

    $kernel->post(
        snmp => 'getbulk',
        'snmp_get_cb',
        -nonrepeaters => 0,
        -maxrepetitions => 8,
        -varbindlist => ['.1.3.6.1.2.1.1'],
    );
}

# store results for future processing
sub snmp_get_cb {
    my ($kernel, $heap, $aref) = @_[KERNEL, HEAP, ARG1];
    my $href = $aref->[0];
    foreach my $k (keys %$href) {
        $heap->{results}{$k} = $href->{$k};
    }
    $kernel->post( snmp => 'finish' );
}

sub stop_session {
   my $r = $_[HEAP]->{results};
   ok exists($r->{'.1.3.6.1.2.1.1.1.0'});
   ok exists($r->{'.1.3.6.1.2.1.1.2.0'});
   ok exists($r->{'.1.3.6.1.2.1.1.3.0'});
   ok exists($r->{'.1.3.6.1.2.1.1.4.0'});
   ok exists($r->{'.1.3.6.1.2.1.1.5.0'});
   ok exists($r->{'.1.3.6.1.2.1.1.6.0'});
   # not exported by cygwin
   # ok exists($r->{'.1.3.6.1.2.1.1.7.0'});
   ok exists($r->{'.1.3.6.1.2.1.1.8.0'});
}


