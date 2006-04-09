BLOCK:
{
    # paste BELOW this line BETWEEN braces for sample program docs.

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

}
