#!/usr/bin/perl
use ExtUtils::MakeMaker;

#------------------------------------------------------------
# Makefile:
#------------------------------------------------------------

# Write the Makefile:
WriteMakefile(
	      NAME         => 'MIME::Lite',
	      VERSION_FROM => "./lib/MIME/Lite.pm",
	      DISTNAME     => "MIME-Lite",
	      EXE_FILES    => [@EXES],
	      'dist'       => {
		  PREOP    => "./MakefileZG.PL ; make -f MakefileZG",
		  COMPRESS => 'gzip',
		  SUFFIX   => 'gz',
	      }
	      );

