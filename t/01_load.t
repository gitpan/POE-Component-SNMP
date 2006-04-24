use Test::More;

use POE;
plan tests => 4;


# this quiets the warning "POE::Kernel's run() method was never called."
END{ $poe_kernel->run() }


# BEGIN { $| = 1; print "1..2\n"; }
# END {print "not ok 1\n" unless $loaded;}
eval { require POE::Component::SNMP };
# $loaded = 1;
# print "ok 1\n";
ok $POE::Component::SNMP::Dispatcher::INSTANCE, 'loaded';

# THERE IS A TYPO IN '-hosntame'! this should generate an error!
eval { POE::Component::SNMP->create(
                                    -alias     => 'snmp',
                                    -hosntame  => $CONF->{hostname} || 'localhost',
                                    -community => $CONF->{community}|| 'public',
                                    -debug     => $CONF->{debug},
                                    -timeout   => 5,
                                   )
}
;


# ok $@, $@;

ok $@ =~ /^-?hostname parameter required/, 'catches parameter typo';

# THIS ONE HAS THE TYPO ON 'debug';
eval { POE::Component::SNMP->create(
                                    -alias     => 'snmp',
                                    -hostname  => $CONF->{hostname} || 'localhost',
                                    -community => $CONF->{community}|| 'public',
                                    -debgu     => $CONF->{debug},
                                    -timeout   => 5,
                                   )
}
;

ok $@ =~ /^Invalid argument/, 'catches parameter typo';

eval { POE::Component::SNMP->create() };

ok $@ =~ /hostname parameter required at/; # dies, no params supplied. actually dies on missing hostname parameter.
