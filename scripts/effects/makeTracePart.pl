#!/usr/bin/perl

use lib '.';
use strict;
use Data::Dumper;
use ExpUtils qw/parse_results_files compute_best_drift/;

if (@ARGV == 0) {
    die "usage: makeTracePart.pl results/results.2025-03-24_09-39-21.table.csv\n";
}
my $VERBOSE = 0;

#
# Generate trace partition comparison
#
my @resultsfiles = @ARGV;
print "makeTracePart.pl: using CSVs: ".join(', ', @resultsfiles)."\n";
my $d = parse_results_files(\@resultsfiles);


# compute the best run set (only non-ev trans)
foreach my $bench (sort keys %$d) {
    # choose a starting point for both BEST_TRANS and BEST_DRIFTEV
    # my $someRD = (grep($_ !~ /mochi/ && $_ =~ /trans/,keys %{$d->{$bench}}))[0];
    # newBest('BEST_TRANS',$bench,$someRD);
    # my $someRD = (grep($_ !~ /mochi/ && $_ !~ /trans/,keys %{$d->{$bench}}))[0];
    # newBest('BEST_DRIFTEV',$bench,$someRD);
    
    foreach my $rd (keys %{$d->{$bench}}) {
        next if $rd =~ /BEST/;
        next if $rd =~ /mochi/i;
        next if $rd =~ /rethfl/i;
        #next if $rd =~ /TRtrans/;
        next if $rd !~ /TL1/; # CA on June 25th said for TP only do TL=1
        my $BEST = ($rd =~ /TPfalse/i ? 'BEST_TPOFF' : 'BEST_TPON');
        $BEST = $BEST.($rd =~ /TRtrans/ ? '_DRIFT' : '_EVDRIFT');
        # we have nothing yet, so we take it
        if (not defined $d->{$bench}->{$BEST}) {
            newBest($BEST,$bench,$rd);
        # does it improve because previously BEST coudln't prove it?
        } elsif ($d->{$bench}->{$BEST}->{res} ne 'true') {
            newBest($BEST,$bench,$rd);
        # does it improve because it's faster?
        } elsif ($d->{$bench}->{$rd}->{res} eq 'true'
                && $d->{$bench}->{$rd}->{cpu} < $d->{$bench}->{$BEST}->{cpu}) {
            newBest($BEST,$bench,$rd);
        } else {
            # warn "not better\n";
        }
    }
}

open TP, ">exp-tp.tex" or die $!;
open SCRIPT, ">generate_table2" or die $!;
$ct = 1;
my $newTPOverNoTPevDrift = 0;
my @geos_notp_drift;   my @geos_tp_drift; 
my @geos_notp_evdrift; my @geos_tp_evdrift;
foreach my $b (sort keys %$d) {
    #print "b: $b\n";
    next if $b =~ /nums?_even/;
    my $tt = $b; $tt =~ s/\_/\\_/g;
    $tt =~ s/negated/neg/;
    print TP "$ct. \\texttt{\\scriptsize $tt} "; ++$ct;
    #warn "tool rd: ".Dumper($b,$d->{$b},$d->{$b}->{BEST_TRANS},$d->{$b}->{BEST_TRANS}->{rd});
    # Best Drift+TP
    print TP sprintf("& %-5s & %3.1f & %s ", # & %3.1f 
           cleanRes($d->{$b}->{BEST_TPON_DRIFT}->{res}),
           $d->{$b}->{BEST_TPON_DRIFT}->{cpu},
           #$d->{$b}->{BEST_TPON_DRIFT}->{mem},
           run2tool($d->{$b}->{BEST_TPON_DRIFT}->{rd}));
    # Best EVDrift no TP
    print TP sprintf("& %-5s & %3.1f & %s ", # & %3.1f 
           cleanRes($d->{$b}->{BEST_TPOFF_EVDRIFT}->{res}),
           $d->{$b}->{BEST_TPOFF_EVDRIFT}->{cpu},
           #$d->{$b}->{BEST_TPOFF_EVDRIFT}->{mem},
           run2tool($d->{$b}->{BEST_TPOFF_EVDRIFT}->{rd}));
    # Best EVDrift+TP
    print TP sprintf("& %-5s & %3.1f & %s \\\\ \n", # & %3.1f 
           cleanRes($d->{$b}->{BEST_TPON_EVDRIFT}->{res}),
           $d->{$b}->{BEST_TPON_EVDRIFT}->{cpu},
           #$d->{$b}->{BEST_TPON_EVDRIFT}->{mem},
           run2tool($d->{$b}->{BEST_TPON_EVDRIFT}->{rd}));

    # save the runtimes for statistics
    push @geos_notp_drift, $d->{$b}->{BEST_TPOFF_DRIFT}->{cpu}
      if $d->{$b}->{BEST_TPOFF_DRIFT}->{cpu} < 900 && $d->{$b}->{BEST_TPOFF_DRIFT}->{cpu} > 0;
    push @geos_tp_drift, $d->{$b}->{BEST_TPON_DRIFT}->{cpu}
      if $d->{$b}->{BEST_TPON_DRIFT}->{cpu} < 900 && $d->{$b}->{BEST_TPON_DRIFT}->{cpu} > 0;
    push @geos_notp_evdrift, $d->{$b}->{BEST_TPOFF_EVDRIFT}->{cpu}
      if $d->{$b}->{BEST_TPOFF_EVDRIFT}->{cpu} < 900 && $d->{$b}->{BEST_TPOFF_EVDRIFT}->{cpu} > 0;
    push @geos_tp_evdrift, $d->{$b}->{BEST_TPON_EVDRIFT}->{cpu}
      if $d->{$b}->{BEST_TPON_EVDRIFT}->{cpu} < 900 && $d->{$b}->{BEST_TPON_EVDRIFT}->{cpu} > 0;
    #
    $newTPOverNoTPevDrift++ if $d->{$b}->{BEST_TPON_EVDRIFT}->{res} eq 'true' && $d->{$b}->{BEST_TPOFF_EVDRIFT}->{res} ne 'true';
    # script for tp-vs-noTP
    print SCRIPT "# Drift + Trace partitioning $b:\n".cfg2cmd('',$b,$d->{$b}->{BEST_TPON_DRIFT}->{rd});
    print SCRIPT "# evDrift without Trace partitioning $b:\n".cfg2cmd('',$b,$d->{$b}->{BEST_TPOFF_EVDRIFT}->{rd});
    print SCRIPT "# evDrift + Trace partitioning $b:\n".cfg2cmd('',$b,$d->{$b}->{BEST_TPOFF_EVDRIFT}->{rd});
}
close TP;
close SCRIPT;
print "wrote: exp-tp.tex\n";
print "wrote: generate_table2\n";

warn "empty list will cause problems for geomean - geos_tp_drift" unless @geos_tp_drift;
warn "empty list will cause problems for geomean - geos_tp_evdrift" unless @geos_tp_evdrift;

use Statistics::Basic qw(:all);
open STATS, ">exp-tpstats.tex" or die $!;
print STATS join("\n", (
   "% TP Improvements:",
   ('\newcommand\expTPGMoffDrift{'.geometric_mean(@geos_notp_drift).'}'),
   ('\newcommand\expTPGMonDrift{'.geometric_mean(@geos_tp_drift).'}'),
   ('\newcommand\expTPGMoffevDrift{'.geometric_mean(@geos_notp_evdrift).'}'),
   ('\newcommand\expTPGMonevDrift{'.geometric_mean(@geos_tp_evdrift).'}'),
   ('\newcommand\expTPSpeedupDrift{'.sprintf("%0.1f", geometric_mean(@geos_notp_drift)/geometric_mean(@geos_tp_drift)).'}'),
   ('\newcommand\expTPSpeedupevDrift{'.sprintf("%0.1f", geometric_mean(@geos_notp_evdrift)/geometric_mean(@geos_tp_evdrift)).'}'),
   ('\newcommand\expTPNew{'.$newTPOverNoTPevDrift.'}'),
