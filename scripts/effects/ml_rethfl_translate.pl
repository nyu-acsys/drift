#!/usr/bin/perl
#
# Generate Rethfl HFLz translations of the (ml-tuple-translated)
# MoCHi benchmarks to be used with Rethfl.
#
# input: 
#   1. the path to mochi.exe
#   2. directory where to search for programs and properties
#   3. directory where to save the cps translated programs
#
# example:  ./ml_rethfl_translate.py  \
#               /tools/home/eric/MoCHi/src/mochi.exe \
#               ../../tests/effects/tr_tuple_mochi \
#               ../../tests/effects/tr_tuple_hflz
#
use strict;
use warnings;
use File::Spec;

if (@ARGV != 3) {
    die "Usage: $0 mochi.exe .../tr_tuple_mochi .../tr_tuple_hflz\n";
}
my ($mochi_exe, $indir, $outdir) = @ARGV;

opendir(my $dh, $indir) or die "Cannot open directory '$indir': $!";
while (my $file = readdir($dh)) {
    next if $file eq '.' or $file eq '..';  # Skip special entries

    my $full_path = File::Spec->catfile($indir, $file);

    if (-f $full_path) {
        #print "Translating $full_path ...\n";
        my $cmd = qq{$mochi_exe -trans HFLz $full_path > $outdir/$file\n};
        print "$cmd\n";
        print qx{$cmd};
        # Do something with $full_path...
        #exit;
    }
}
closedir($dh);