use 5.018002;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'broken',
    VERSION_FROM      => 'lib/broken.pm', # finds $VERSION
    PREREQ_PM         => {}, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/broken.pm', # retrieve abstract from module
       AUTHOR         => 'Borislav Nikolov <bnikolov@lhr4.prod.booking.com>') : ()),
    LIBS              => [''], # e.g., '-lm'
    'OBJECT'    => 'broken.o can.o',
    DEFINE            => '-g3 -O0', # e.g., '-DHAVE_SOMETHING'
    INC               => '-I.', # e.g., '-I. -I/usr/include/other'
	# Un-comment this if you add C files to link with later:
    # OBJECT            => '$(O_FILES)', # link all the C files too
);
