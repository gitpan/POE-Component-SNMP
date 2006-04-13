use Test::More; # qw/no_plan/;
use strict;

BEGIN { use_ok 'POE::Component::SNMP' };

use POE;
use POE::Component::SNMP;

my $CONF = do "config.cache";

if ( $CONF->{skip_all_tests} ) {
    plan skip_all => 'No SNMP data specified.';
} else {
    plan tests => 19;
}


POE::Session->create
( inline_states =>
  { _start      => \&snmp_run_tests,
    _stop       => \&stop_session,
    snmp_get_cb => \&snmp_get_cb,
  },
);

$poe_kernel->run;

exit 0;

my $sysName = "1.3.6.1.2.1.1.5.0";
my $system_base  = "1.3.6.1.2.1.1";

sub snmp_run_tests {
    my ($kernel, $heap) = @_[KERNEL, HEAP];

    eval {
        # throws an error because no hostname:
        POE::Component::SNMP->create(
                                     alias     => 'snmp_no_hostname',
                                     # hostname  => $CONF->{'hostname'},
                                     community => $CONF->{'community'},
                                     debug     => 0,

                                    );
    };

    ok $@, '-hostname parameter required';
    # this asserts that the alias does *not* exist
    ok ! $kernel->alias_resolve( 'snmp_no_hostname' ), "session not created with missing hostname";


    #### dupe sessions:

    # normal session
    POE::Component::SNMP->create( alias     => 'snmp',
                                  hostname  => $CONF->{'hostname'},
                                  community => $CONF->{'community'},
				  timeout   => 2,
                                );
    ok $kernel->alias_resolve('snmp'), "normal session create";

  SKIP: {
	skip "dupe session check not done", 2;
        #  if exists $ENV{POE_ASSERT_DEFAULT}; # and $POE::VERSION <= 0.34;

        eval {
	    POE::Component::SNMP->create( alias     => 'snmp',
					  hostname  => $CONF->{'hostname'},
					  community => $CONF->{'community'},
					  timeout   => 2,
					);
        };

        # ok $@, $@;
        ok $@ =~ /'snmp' already exists|'snmp' is in use by another session/, "duplicate alias is fatal";
	# test the session does *not* exist
	ok $kernel->alias_resolve('snmp'), "duplicate session not created";

    }

    $heap->{done} = 0;

    ###
    ### Throw some client-side errors:
    ###

  SKIP: {
        0 and
          skip "client stuff", 3;

        # wants baseoid, NOT varbindlist
        # Invalid argument '-varbindlist'
        $heap->{planned}++;
        $kernel->post(
                      snmp => walk =>
                      'snmp_get_cb',
                      -varbindlist => $sysName,
                     );

        # doesn't like empty baseoid parameter
        # Expected base OBJECT IDENTIFIER in dotted notation
        $heap->{planned}++;
        $kernel->post(
                      snmp => walk =>
                      'snmp_get_cb',
                      -baseoid => '',
                     );

        # wants varbindlist, NOT baseoid
        # Invalid argument '-baseoid'
        $heap->{planned}++;
        $kernel->post(
                      snmp => get =>
                      'snmp_get_cb',
                      -baseoid => $system_base,
                     );


    }

  SKIP: {
	###
	### Now throw some server-side errors:
	###

	0 and
	  skip "server stuff", 5;

	###
	### THIS DOES *NOT* throw an error!
	###
        ##
	## This returns an empty, VALID result hash!
	## NOT to be confused with an empty string.
	$heap->{planned}++;
	$kernel->post(
		      snmp => get =>
		      'snmp_get_cb',
		      -varbindlist => undef,
		     );


	# doesn't like empty varbindlist
	# Expected array reference for variable-bindings
	$heap->{planned}++;
	$kernel->post(
		      snmp => get =>
		      'snmp_get_cb',
		      -varbindlist => '',
		     );

	if (0) {
	    # I expected this to complain, like the others, because
	    # sysname isn't an array ref.  It didn't.  I'll figure it
	    # out later.


	    # doesn't like string varbindlist, wants an array ref:
	    # Expected array reference for variable-bindings
	    $heap->{planned}++;
	    $kernel->post(
			  snmp => get =>
			  'snmp_get_cb',
			  -varbindlist => $sysName,
			 );
	}


	# OID value out of range
	# An OBJECT IDENTIFIER must begin with either 0 (ccitt), 1 (iso), or 2 (joint-iso-ccitt)
	$heap->{planned}++;
	$kernel->post(
		      snmp => get =>
		      'snmp_get_cb',
		      -varbindlist => [ '9.9.9.9.9.9.9' ],
		     );



	# no such variable in this MIB
	# Received noSuchName(2) error-status at error-index 1
	$heap->{planned}++;
	$kernel->post(
		      snmp => get =>
		      'snmp_get_cb',
		      -varbindlist => [ '1.9.9.9.9.9.9' ],
		     );

    }

  SKIP: {

	###
	### do some bad 'set' requests
	###
	0 and
	  skip "write tests", 3;

	skip "no writeable SNMP device available", 3 if not length $CONF->{wcommunity};

	# I picked this OID at random because I figured it would be readonly:
	# system.sysORTable.sysOREntry.sysORDescr.1
	my $read_only_string_oid = ".1.3.6.1.2.1.1.9.1.3.1";


	# invalid parms for 'set'
	# Expected [OBJECT IDENTIFIER, ASN.1 type, object value] combination
	$heap->{planned}++;
	$kernel->post(
		      snmp => set =>
		      'snmp_get_cb',
		      -varbindlist => [ '1.9.9.9.9.9.9' ],
		     );

	# invalid parms for 'set'
	# Unknown ASN.1 type [STRING]
	$heap->{planned}++;
	$kernel->post(
		      snmp => set =>
		      'snmp_get_cb',
		      -varbindlist => [ $read_only_string_oid, 'STRING', 'hi mom' ],
		     );

	# write to a readonly value
	# Received noSuchName(2) error-status at error-index 0
	$heap->{planned}++;
	$kernel->post(
		      snmp => set =>
		      'snmp_get_cb',
		      -varbindlist => [ $read_only_string_oid, 'OCTET_STRING', 'hi mom' ],
		     );

    }

}

# store results for future processing
sub snmp_get_cb {
    my ($kernel, $heap, $request, $aref) = @_[KERNEL, HEAP, ARG0, ARG1];
    my $href = $aref->[0];

    if (ref $href) { # got server results

	# catch the results of $kernel->post( snmp => get => -varbindlist => undef )
	# which should be: [ {}, '' ]
	if ($request->[2] eq 'get' and
	    $request->[3] eq '-varbindlist' and
	    not defined $request->[4]) {
	    ok keys %$href == 0, "undef args";
	    # should be '', which is false.
	    ok ! $_[ARG1][1], "undef args";
	    ok ++$heap->{saw_empty}, "empty_request returned for undef -varbindlist";
	}

	foreach my $k (keys %$href) {
	    $heap->{results}{$k} = $href->{$k};
	}

    } elsif (defined $href) {
	my $message = $href;

	ok $message, "error: $message";
    }
    $kernel->post( snmp => 'finish' ) if ++$heap->{done} == $heap->{planned};
}

sub stop_session {
   my $r = $_[HEAP]->{results};

   ok $_[HEAP]->{saw_empty}, "undef -varbindlist result seen";
   ok ! keys %$r, "all expected results received";

#    ok exists($r->{'.1.3.6.1.2.1.1.1.0'});
#    ok exists($r->{'.1.3.6.1.2.1.1.2.0'});
#    ok exists($r->{'.1.3.6.1.2.1.1.3.0'});
#    ok exists($r->{'.1.3.6.1.2.1.1.4.0'});
#    ok exists($r->{'.1.3.6.1.2.1.1.5.0'});
#    ok exists($r->{'.1.3.6.1.2.1.1.6.0'});

#    # not exported by cygwin?
#    # ok exists($r->{'.1.3.6.1.2.1.1.7.0'});
#    ok exists($r->{'.1.3.6.1.2.1.1.8.0'});
}
