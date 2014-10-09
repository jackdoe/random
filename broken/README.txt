use strict;
use warnings;

use Test::More tests => 1;
BEGIN { use_ok('broken') };
my $x = "a" x 2000;
my @a = ();
my $data = {};
for my $n(1..1500_000) {
    push @a,$x;
}

broken::table();


$ perl -Ilib -Iblib/arch/ t/broken.t
1..1
ok 1 - use broken;
Segmentation fault
$ gdb perl
GNU gdb (GDB) Red Hat Enterprise Linux (7.2-64.el6_5.2)
Copyright (C) 2010 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.  Type "show copying"
and "show warranty" for details.
This GDB was configured as "x86_64-redhat-linux-gnu".
For bug reporting instructions, please see:
<http://www.gnu.org/software/gdb/bugs/>...
Reading symbols from /usr/local/perl/5.18.2/bin/perl...(no debugging symbols found)...done.
(gdb) set args -Ilib -Iblib/arch/ t/broken.t
(gdb) run
Starting program: /usr/local/perl/5.18.2/bin/perl -Ilib -Iblib/arch/ t/broken.t
[Thread debugging using libthread_db enabled]
1..1
ok 1 - use broken;

Program received signal SIGSEGV, Segmentation fault.
__strlen_sse2 () at ../sysdeps/x86_64/strlen.S:32
32		movdqu	(%rdi), %xmm1
Missing separate debuginfos, use: debuginfo-install perl5182-5.18.2-1.el6.x86_64
(gdb) bt
#0  __strlen_sse2 () at ../sysdeps/x86_64/strlen.S:32
#1  0x00007ffff6a369eb in _IO_puts (str=0xffffffffb8609500 <Address 0xffffffffb8609500 out of bounds>) at ioputs.c:37
#2  0x00007ffff06c4c8e in OS_get_table () at can.c:6
#3  0x00007ffff06c4a6d in XS_broken_table (cv=0x8353d0) at broken.xs:14
#4  0x00007ffff7b11ce5 in Perl_pp_entersub () from /usr/local/perl/5.18.2/lib/CORE/libperl.so
#5  0x00007ffff7b10313 in Perl_runops_standard () from /usr/local/perl/5.18.2/lib/CORE/libperl.so
#6  0x00007ffff7aa9c4c in perl_run () from /usr/local/perl/5.18.2/lib/CORE/libperl.so
#7  0x0000000000400ddc in main ()
