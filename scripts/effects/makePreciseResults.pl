#!/usr/bin/perl

use lib '.';
use strict;
use Data::Dumper;
use ExpUtils qw/parse_results_files run2tool run2toolBest cleanRes/;

#
# Generates Latex Results output
#
#.  This assumes you have already run benchexec (according to the
#.    "make precise" target) and table-generator to produce per-tool
#.    table CSVs, whose file names are stored in "csvs.txt"
#

my @resultsfiles;
if (@ARGV != 1) {
    die "usage: makePreciseResults.pl csvs.txt\n";
}
open CSVS, $ARGV[0] or die "unable to open $ARGV[1]\n";
while(<CSVS>){
    chomp;
    die "file: $_ does not exist\n" if (! -f $_);
    push @resultsfiles, $_;
}
print "makePreciseResults.pl: using CSVs: ".join(', ', @resultsfiles)."\n";


my @RUNDEFINITIONS = (
    'default.mochibenchmarks',
    'default.realmochibenchmarks',
    'default.rethflbenchmarks',
    'default.driftwrapperbenchmarks',
    );
my $COARMOCHI_RD = 'default.mochibenchmarks';
my $REALMOCHI_RD = 'default.realmochibenchmarks';
my $RETHFL_RD    = 'default.rethflbenchmarks';

my $d = parse_results_files(\@resultsfiles);
compute_best_drift($d);


#
# Generate paper body table showing only the best
#
open BODY, ">exp-body.tex" or die $!;

my @geos_coarmochi; my @geos_realmochi; my @geos_rethfl; my @geos_evtrans; my @geos_direct;
my $newOverCoarMochi = 0; my $newOverRealMochi = 0; my $newOverRethfl = 0; my $newOverTrans = 0; my $benchCount = 0; my $ct = 1;
my $driftVerified = 0; my $evdriftVerified = 0;
my $realmochiVerified = 0; my $rcamlVerified = 0; my $rethflVerified = 0;
my @bothSolved; my @evAndMochiSolved; my @evAndRcamlSolved; my @evAndRethflSolved; 
foreach my $b (sort keys %$d) {
    # new benchmarks
    #next if $b =~ /concurrent_sum/;
    #next if $b =~ /nondet_max/;
    #next if $b =~ /order-irrel-nondet/;
    next if $b =~ /mochi-sum/;
    next if $b =~ /mochi-test/;
    next if $b =~ /nums_evens/;
    next if $b =~ /num_evens/;
    #next if $b =~ /disj-nondet/;

    # next if $b =~ /auction/;
    # next if $b =~ /binomial_heap/;
    # next if $b =~ /ho-shrink/; # old name;
    my $tt = $b; $tt =~ s/\_/\\_/g;
    $tt =~ s/negated/neg/;
    #warn "b: $b\n".Dumper($d->{$b});
    my $footnote = ($b =~ /^(last-ev-even|order-irrel|temperature|sum-of-ev-even|disj)$/ ? "\$^+\$" : '');
    print BODY "$ct. \\texttt{\\scriptsize $tt$footnote} "; ++$ct;
    #warn "tool rd: ".Dumper($b,$d->{$b},$d->{$b}->{BEST_TRANS},$d->{$b}->{BEST_TRANS}->{rd});
    die "don't have a BEST_TRANS rundef for $b" unless $d->{$b}->{BEST_TRANS}->{rd} =~ /[a-z]/;
    die "don't have a $REALMOCHI_RD rundef for $b" unless $d->{$b}->{$REALMOCHI_RD}->{rd} =~ /[a-z]/;
    die "don't have a $COARMOCHI_RD rundef for $b" unless $d->{$b}->{$COARMOCHI_RD}->{rd} =~ /[a-z]/;
    die "don't have a $RETHFL_RD rundef for $b" unless $d->{$b}->{$RETHFL_RD}->{rd} =~ /[a-z]/;
    die "don't have a BEST_DRIFTEV rundef for $b" unless $d->{$b}->{BEST_DRIFTEV}->{rd} =~ /[a-z]/;

    # Trans-Drift
    print BODY sprintf("& %-5s & %3.1f & %s ", # %3.1f & 
           cleanRes($d->{$b}->{BEST_TRANS}->{res}),
           $d->{$b}->{BEST_TRANS}->{cpu},
           #$d->{$b}->{BEST_TRANS}->{mem},
           run2toolBest($d->{$b}->{BEST_TRANS}->{rd},$b));
    # Trans-COARMochi
    print BODY sprintf("& %-5s & %3.1f ", # %3.1f & 
           cleanRes($d->{$b}->{$COARMOCHI_RD}->{res}),
           $d->{$b}->{$COARMOCHI_RD}->{cpu},
           #$d->{$b}->{$COARMOCHI_RD}->{mem}
           );
    # Trans-Mochi
    print BODY sprintf("& %-5s & %3.1f ", # %3.1f & 
           cleanRes($d->{$b}->{$REALMOCHI_RD}->{res}),
           $d->{$b}->{$REALMOCHI_RD}->{cpu},
           #$d->{$b}->{$REALMOCHI_RD}->{mem}
           );
    # Trans-Rethfl
    print BODY sprintf("& %-5s & %3.1f ", # %3.1f & 
           cleanRes($d->{$b}->{$RETHFL_RD}->{res}),
           $d->{$b}->{$RETHFL_RD}->{cpu},
           #$d->{$b}->{$REALMOCHI_RD}->{mem}
           );
    # DriftEV
    print BODY sprintf("& %-5s & %3.1f & %s \\\\ \n",# %3.1f & 
           cleanRes($d->{$b}->{BEST_DRIFTEV}->{res}),
           $d->{$b}->{BEST_DRIFTEV}->{cpu},
           #$d->{$b}->{BEST_DRIFTEV}->{mem},
           run2toolBest($d->{$b}->{BEST_DRIFTEV}->{rd},$b));
    #printf "best EDrift result for %-40s : %-10s : %s\n", $b, $d->{$b}->{BEST_DRIFTEV}->{res}, $d->{$b}->{BEST_DRIFTEV}->{rd};
    printf "  %-20s | Dr: %-10s | eDr: %-10s | Mo: %-10s | RC: %-10s | Reth: %-10s \n",
                 $b,
                 $d->{$b}->{BEST_TRANS}->{res},
                 $d->{$b}->{BEST_DRIFTEV}->{res},
                 $d->{$b}->{$REALMOCHI_RD}->{res},
                 $d->{$b}->{$COARMOCHI_RD}->{res},
                 $d->{$b}->{$RETHFL_RD}->{res};
#    printf "best Drift   result for %-40s : %-10s : %s\n", $b, $d->{$b}->{BEST_TRANS}->{res}, $d->{$b}->{BEST_TRANS}->{rd};
#    printf "best evDrift result for %-40s : %-10s : %s\n", $b, $d->{$b}->{BEST_DRIFTEV}->{res}, $d->{$b}->{BEST_DRIFTEV}->{rd};
    # save the runtimes for statistics
    push @geos_evtrans, $d->{$b}->{BEST_TRANS}->{cpu}
      if $d->{$b}->{BEST_TRANS}->{res} eq 'true' && $d->{$b}->{BEST_TRANS}->{cpu} < 900 && $d->{$b}->{BEST_TRANS}->{cpu} > 0;
    push @geos_coarmochi, $d->{$b}->{$COARMOCHI_RD}->{cpu}
      if $d->{$b}->{$COARMOCHI_RD}->{res} eq 'true' && $d->{$b}->{$COARMOCHI_RD}->{cpu} < 900 && $d->{$b}->{$COARMOCHI_RD}->{cpu} > 0;
    push @geos_realmochi, $d->{$b}->{$REALMOCHI_RD}->{cpu}
      if $d->{$b}->{$REALMOCHI_RD}->{res} eq 'true' && $d->{$b}->{$REALMOCHI_RD}->{cpu} < 900 && $d->{$b}->{$REALMOCHI_RD}->{cpu} > 0;
    push @geos_rethfl, $d->{$b}->{$RETHFL_RD}->{cpu}
      if $d->{$b}->{$RETHFL_RD}->{res} eq 'true' && $d->{$b}->{$RETHFL_RD}->{cpu} < 900 && $d->{$b}->{$RETHFL_RD}->{cpu} > 0;
    push @geos_direct, $d->{$b}->{BEST_DRIFTEV}->{cpu}
      if $d->{$b}->{BEST_DRIFTEV}->{res} eq 'true' && $d->{$b}->{BEST_DRIFTEV}->{cpu} < 900 && $d->{$b}->{BEST_DRIFTEV}->{cpu} > 0;
    #
    $driftVerified++ if $d->{$b}->{BEST_TRANS}->{res} eq 'true'; 
    $evdriftVerified++ if $d->{$b}->{BEST_DRIFTEV}->{res} eq 'true'; 
    $realmochiVerified++ if $d->{$b}->{$REALMOCHI_RD}->{res} eq 'true'; 
    $rcamlVerified++ if $d->{$b}->{$COARMOCHI_RD}->{res} eq 'true'; 
    $rethflVerified++ if $d->{$b}->{$RETHFL_RD}->{res} eq 'true'; 
    $newOverCoarMochi++ if $d->{$b}->{BEST_DRIFTEV}->{res} eq 'true' && $d->{$b}->{$COARMOCHI_RD}->{res} ne 'true';
    $newOverRealMochi++ if $d->{$b}->{BEST_DRIFTEV}->{res} eq 'true' && $d->{$b}->{$REALMOCHI_RD}->{res} ne 'true';
    $newOverRethfl++ if $d->{$b}->{BEST_DRIFTEV}->{res} eq 'true' && $d->{$b}->{$RETHFL_RD}->{res} ne 'true';
    $newOverTrans++ if $d->{$b}->{BEST_DRIFTEV}->{res} eq 'true' && $d->{$b}->{BEST_TRANS}->{res} ne 'true';
    $benchCount++;
    # remember which ones were solved by Drift for speedup calculation
    push @bothSolved, $b if $d->{$b}->{BEST_TRANS}->{res} eq 'true' && $d->{$b}->{BEST_DRIFTEV}->{res} eq 'true';
    # remember which ones were solved by Drift and by real mochi
    push @evAndMochiSolved, $b if $d->{$b}->{BEST_DRIFTEV}->{res} eq 'true' && $d->{$b}->{$REALMOCHI_RD}->{res} eq 'true';
    # remember which ones were solved by Drift and by Rcaml
    push @evAndRcamlSolved, $b if $d->{$b}->{BEST_DRIFTEV}->{res} eq 'true' && $d->{$b}->{$COARMOCHI_RD}->{res} eq 'true';
    # remember which ones were solved by Drift and by Rethfl
    push @evAndRethflSolved, $b if $d->{$b}->{BEST_DRIFTEV}->{res} eq 'true' && $d->{$b}->{$RETHFL_RD}->{res} eq 'true';
}
close BODY;
print "wrote: exp-body.tex\n";

print "Calculating some statistics ...\n";
# calculate the speedup on the ones that Drift and evDrift BOTH solved
my @driftTimes  = map { $d->{$_}->{BEST_TRANS}->{cpu} } @bothSolved;
my @evDiftTimes = map { $d->{$_}->{BEST_DRIFTEV}->{cpu} } @bothSolved;
# print "GM of evtrans:".Dumper(\@geos_evtrans, 0+@geos_evtrans);
# print "GM of direct:".Dumper(\@geos_direct, 0+@geos_direct);
#print "times:".Dumper(\@driftTimes, \@evDiftTimes);
my $speedupEVoverDrift = geometric_mean(@driftTimes)/geometric_mean(@evDiftTimes);

# calculate speedup of Real Mochi over evDrift
warn "  ev+mochi solved: ".join(',',@evAndMochiSolved)."\n";
my @EVMO_ev_times = map { $d->{$_}->{BEST_DRIFTEV}->{cpu} } @evAndMochiSolved;
my @EVMO_mo_times = map { $d->{$_}->{$REALMOCHI_RD}->{cpu} } @evAndMochiSolved;
my $speedupEVoverRealMochi = geometric_mean(@EVMO_mo_times)/geometric_mean(@EVMO_ev_times);

# calculate speedup of RCaml over evDrift
warn "  ev+rcaml solved: ".join(',',@evAndRcamlSolved)."\n";
my @EVMO_ev_times = map { $d->{$_}->{BEST_DRIFTEV}->{cpu} } @evAndRcamlSolved;
my @EVMO_rc_times = map { $d->{$_}->{$COARMOCHI_RD}->{cpu} } @evAndRcamlSolved;
my $speedupEVoverRcaml = geometric_mean(@EVMO_rc_times)/geometric_mean(@EVMO_ev_times);

# calculate speedup of Rethfl over evDrift
warn "  ev+rethfl solved: ".join(',',@evAndRethflSolved)."\n";
my @EVMO_ev_times = map { $d->{$_}->{BEST_DRIFTEV}->{cpu} } @evAndRethflSolved;
my @EVMO_rc_times = map { $d->{$_}->{$RETHFL_RD}->{cpu} } @evAndRethflSolved;
my $speedupEVoverRethfl = geometric_mean(@EVMO_rc_times)/geometric_mean(@EVMO_ev_times);



use Statistics::Basic qw(:all);
open STATS, ">exp-stats.tex" or die $!;
print STATS join("\n", (
   ('\newcommand\expGMevtrans{'.geometric_mean(@geos_evtrans).'}'),
   ('\newcommand\expGMcoarmochi{'.geometric_mean(@geos_coarmochi).'}'),
   ('\newcommand\expGMrealmochi{'.geometric_mean(@geos_realmochi).'}'),
   ('\newcommand\expGMrethfl{'.geometric_mean(@geos_rethfl).'}'),
   ('\newcommand\expGMdirect{'.geometric_mean(@geos_direct).'}'),
   ('\newcommand\expSpeedupEVoverDrift{'.sprintf("%0.1f", $speedupEVoverDrift).'}'),
   ('\newcommand\expSpeedupEVoverRcaml{'.sprintf("%0.1f", $speedupEVoverRcaml).'}'),
   ('\newcommand\expSpeedupEVoverRethfl{'.sprintf("%0.1f", $speedupEVoverRethfl).'}'),
   ('\newcommand\expSpeedupEVoverRealMochi{'.sprintf("%0.1f", $speedupEVoverRealMochi).'}'),
#   ('\newcommand\expSpeedupEvtrans{'.sprintf("%0.1f", geometric_mean(@geos_evtrans)/geometric_mean(@geos_direct)).'}'),
#   ('\newcommand\expSpeedupMochi{'.sprintf("%0.1f", geometric_mean(@geos_mochi)/geometric_mean(@geos_direct)).'}'),
   ('\newcommand\expNewOverCoarMochi{'.$newOverCoarMochi.'}'),
   ('\newcommand\expNewOverRealMochi{'.$newOverRealMochi.'}'),
   ('\newcommand\expNewOverRethfl{'.$newOverRethfl.'}'),
   ('\newcommand\expNewOverTrans{'.$newOverTrans.'}'),
   ('\newcommand\expBenchCount{'.$benchCount.'}'),
   ('\newcommand\expDriftVerified{'.$driftVerified.'}'),
   ('\newcommand\expEDriftVerified{'.$evdriftVerified.'}'),
   ('\newcommand\expRealMochiVerified{'.$realmochiVerified.'}'),
   ('\newcommand\expRcamlVerified{'.$rcamlVerified.'}'),
   ('\newcommand\expRethflVerified{'.$rethflVerified.'}'),
   "% Overview 1st example:",
   ('\newcommand\expBestOverviewEVDrift{'.sprintf("%0.1f", $d->{overview1}->{BEST_DRIFTEV}->{cpu}).'}'),
   ('\newcommand\expBestOverviewTrans{'.sprintf("%0.1f", $d->{overview1}->{BEST_TRANS}->{cpu}).'}'),
   "% Overview temperature example:",
   ('\newcommand\expBestTempEVDrift{'.sprintf("%d", $d->{temperature}->{BEST_DRIFTEV}->{cpu}).'}'),
   ('\newcommand\expBestTempTrans{'.sprintf("%d", $d->{temperature}->{BEST_TRANS}->{cpu}).'}'),
   ('\newcommand\expBestAuctionEVDrift{'.sprintf("%d", $d->{auction}->{BEST_DRIFTEV}->{cpu}).'}'),
   ('\newcommand\expBestAuctionTrans{'.sprintf("%d", $d->{auction}->{BEST_TRANS}->{cpu}).'}'),
))."\n";
close STATS;
print "wrote: exp-stats.tex\n" or die $!;