
use ExtUtils::MakeMaker;

my $CONFIG_MODE = 0600;
my $CONFIG_FILE = "config.cache";

WriteMakefile(
    'NAME'		=> 'POE::Component::SNMP',
    'VERSION_FROM'	=> 'SNMP.pm',
    'PREREQ_PM'		=> {'POE' => 0, 'Net::SNMP' => 0},
    ($] >= 5.005 ?
      (AUTHOR     => 'Rob Bloodgood <rob@exitexchange.com>') : ()),
);

exit 0 if -e $CONFIG_FILE;

print "The test suite requires the use of an SNMP agent.\n";
my $hostname = prompt("enter a hostname [leave empty to skip all tests]");

if ($hostname) {
    my $rcomm    = prompt("enter read community string", 'public');
    my $wcomm    = prompt("enter write community string [leave empty to skip write tests]");

    print "creating $CONFIG_FILE\n";
    open TESTCONF, "> $CONFIG_FILE" or die "can't create $CONFIG_FILE: $!\n";
    print TESTCONF <<CONFIGURATION;
 \$CONF = {
    hostname   => '$hostname',
    community  => '$rcomm',
    wcommunity => '$wcomm',
 };
CONFIGURATION
    close TESTCONF or die "can't close $CONFIG_FILE: $!\n";
} else {
    print "creating $CONFIG_FILE\n";
    open TESTCONF, "> $CONFIG_FILE" or die "can't create $CONFIG_FILE: $!\n";
    print TESTCONF "\$TEST_CONF = { skip_all_tests => 1 };\n";
    close TESTCONF or die "can't close $CONFIG_FILE: $!\n";
}

chmod $CONFIG_MODE, $CONFIG_FILE;

exit 0;

