
use Test::More;

BEGIN { use_ok('POE::Component::SNMP') };

use POE;
use POE::Component::SNMP;

my $CONF = do "config.cache";

if( $CONF->{skip_all_tests} ) {
    plan skip_all => 'No SNMP data supplied.';
}
else {
    plan tests => 6;
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
        debug     => 0,
    );

    $kernel->post( 
        snmp => 'getnext', 
        'snmp_get_cb', 
        -varbindlist => [
            '.1.3.6.1.2.1.1.1.0', 
            '.1.3.6.1.2.1.1.2.0', 
            '.1.3.6.1.2.1.1.3.0', 
            '.1.3.6.1.2.1.1.4.0', 
            '.1.3.6.1.2.1.1.5.0', 
            '.1.3.6.1.2.1.1.6.0',
        ],
    );

    $kernel->post( snmp => 'dispatch' );
    $kernel->post( snmp => 'finish' );
}

# store results for future processing
sub snmp_get_cb {
    my ($kernel, $heap, $aref) = @_[KERNEL, HEAP, ARG1];
    my $href = $aref->[0];
    foreach my $k (keys %$href) {
        $heap->{results}{$k} = $href->{$k};
    }  
}

sub stop_session {
    my $heap = $_[HEAP];
    ok exists($heap->{results}{'.1.3.6.1.2.1.1.2.0'});
    ok exists($heap->{results}{'.1.3.6.1.2.1.1.3.0'});
    ok exists($heap->{results}{'.1.3.6.1.2.1.1.4.0'});
    ok exists($heap->{results}{'.1.3.6.1.2.1.1.5.0'});
    ok exists($heap->{results}{'.1.3.6.1.2.1.1.6.0'});
    ok exists($heap->{results}{'.1.3.6.1.2.1.1.7.0'});
}


