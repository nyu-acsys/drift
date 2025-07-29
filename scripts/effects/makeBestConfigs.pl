#!/usr/bin/perl

use lib '.';
use strict;
use Data::Dumper;
use ExpUtils qw/parse_results_files compute_best_drift/;

if (@ARGV == 0) {
    die "usage: makeBestConfigs.pl results/results.2025-03-24_09-39-21.table.csv\n";
}
my $VERBOSE = 0;

#
# Parse the table-generator output, and calculate the best
#
my @resultsfiles = @ARGV;
print "makeBestConfigs.pl: using CSVs: ".join(', ', @resultsfiles)."\n";
my $d = parse_results_files(\@resultsfiles);
compute_best_drift($d);

#
# Generate the full exp-apx.tex list of everything
#
open EXT, ">exp-apx.tex" or die $!;
my $ct = 1;
foreach my $b (sort keys %$d) {
    my $tt = $b; $tt =~ s/\_/\\_/g;
    $tt =~ s/negated/neg/;
    print EXT "$ct. \\texttt{\\scriptsize $tt} \\\\\n"; ++ $ct;
    # sort the run defs by tool:
    my @driftRds = grep {$d->{$b}->{$_}->{rd} =~ /TRtrans/ } keys %{$d->{$b}};
    my @evdriftRds = grep {$d->{$b}->{$_}->{rd} =~ /TRdirect/ } keys %{$d->{$b}};
    foreach my $tool (@driftRds, @evdriftRds) { # @RUNDEFINITIONS) {
        next if $tool =~ /BEST/;
        my $drift = ($tool =~ /TRtrans/ ? '\drift' : '\evdrift');
        my $isBest = ($d->{$b}->{BEST_DRIFTEV}->{rd} eq $tool ? '\hl ' : '    ');
        #warn "run2tool($tool)\n";
        #print Dumper($d->{$b}->{$tool});
        print EXT sprintf("& $drift & $isBest %s & $isBest %-5s & $isBest %3.2f \\\\\n", #  & $isBest %3.2f 
           run2tool($tool),
           cleanRes($d->{$b}->{$tool}->{res}),
           $d->{$b}->{$tool}->{cpu},
#           $d->{$b}->{$tool}->{wall},
           #$d->{$b}->{$tool}->{mem}
           ); 
           # d->{$b}->{$tool}->{rd}));
    }
    print  EXT "\\hline\n";
}
close EXT;
print "wrote: exp-apx.tex\n";

#
# Generate best_configs_drift.csv and best_configs_evdrift.csv
#
open CFG_DRIFT, ">best_configs_drift.csv" or die $!;
open CFG_EVDRIFT, ">best_configs_evdrift.csv" or die $!;
foreach my $b (sort keys %$d) {
    die "don't have a BEST_TRANS rundef for $b" unless $d->{$b}->{BEST_TRANS}->{rd} =~ /[a-z]/;
    die "don't have a BEST_DRIFTEV rundef for $b" unless $d->{$b}->{BEST_DRIFTEV}->{rd} =~ /[a-z]/;
    if ($VERBOSE) {
        printf "best Drift   result for %-40s : %-10s : %s\n", $b, $d->{$b}->{BEST_TRANS}->{res}, $d->{$b}->{BEST_TRANS}->{rd};
        printf "best evDrift result for %-40s : %-10s : %s\n", $b, $d->{$b}->{BEST_DRIFTEV}->{res}, $d->{$b}->{BEST_DRIFTEV}->{rd};
    }
    # save the best configurations for Drift/evDrift:
    my $args = cfg2cmd('',$b,$d->{$b}->{BEST_TRANS}->{rd});
    $args =~ s/^\.\/drift.exe //; $args =~ s/\n//;
    $args =~ s/-ev-trans trans //;
    print CFG_DRIFT "tests/effects/$b.ml|$args|$d->{$b}->{BEST_TRANS}->{rd}\n";
    $args = cfg2cmd('',$b,$d->{$b}->{BEST_DRIFTEV}->{rd});
    $args =~ s/^\.\/drift.exe //; $args =~ s/\n//;
    print CFG_EVDRIFT "tests/effects/$b.ml|$args|$d->{$b}->{BEST_DRIFTEV}->{rd}\n";
}
close CFG_DRIFT;
close CFG_EVDRIFT;
print "wrote: best_configs_drift.csv\n";
print "wrote: best_configs_evdrift.csv\n";
