# Before 'make install' is performed this script should be runnable with
# 'make test'. After 'make install' it should work as 'perl broken.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use strict;
use warnings;

use Test::More tests => 1;
BEGIN { use_ok('broken') };
mkdir("/tmp/__1234_real");
symlink("/tmp/__1234_real","/tmp/__1234");


my $x = "a" x 2000;
my @a = ();
my $data = {};
for my $n(1..1500_000) {
    push @a,$x;
}
broken::table();
#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

