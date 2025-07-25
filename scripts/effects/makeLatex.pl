#!/usr/bin/perl

use strict;
use Data::Dumper;


######################################################################
# CONFIGURATION
my @resultsfiles;

# try to get it from commandline: makeLatex.pl --csvs csvs.txt
if (@ARGV != 2) {
    die "usage: makeLatex.pl --csvs csvs.txt\n";
}
if ($ARGV[0] ne '--csvs') {
    die "usage: makeLatex.pl --csvs csvs.txt\n";
}
open CSVS, $ARGV[1] or die "unable to open $ARGV[1]\n";
while(<CSVS>){
    chomp;
    if (! -f $_) {
        die "file: $_ does not exist\n"
    }
    push @resultsfiles, $_;
}
print "makeLatex.pl: using CSVs: ".join(', ', @resultsfiles)."\n";
#exit;

    # "results/benchmark-coarmochi.2025-03-20_17-56-40.results.default.mochibenchmarks.csv",
    # "results/benchmark-mochi.2025-03-20_18-58-49.results.default.realmochibenchmarks.csv",
    # "results/benchmark-rethfl.2025-07-15_10-37-45.results.default.realmochibenchmarks.csv",
    # "results/results.2025-03-24_09-39-21.table.csv"

# 2) load the RunDefinitions defined in the autogen XML file
my @RUNDEFINITIONS = (
    'default.mochibenchmarks',
    'default.realmochibenchmarks',
    'default.rethflbenchmarks',
    'default.driftwrapperbenchmarks',
    );

# 3) which one to use for mochi?
my $COARMOCHI_RD = 'default.mochibenchmarks';
my $REALMOCHI_RD = 'default.realmochibenchmarks';
my $RETHFL_RD    = 'default.rethflbenchmarks';

# 4) Give human-readible names for these rundefinitions for column headers:
sub run2tool {
    my ($rdName) = @_;
    return 'Tuple+Rethfl' if $rdName eq 'default.rethflbenchmarks';
    return 'CPS+COaRRCaml' if $rdName eq 'default.mochibenchmarks';
    return 'Tuple+Mochi' if $rdName eq 'default.realmochibenchmarks';
    if($rdName =~ /NOTE(.*)-TL(.*)-TP(.*)-TH(.*)-DM(.*)-IO([^-]+)-TR([^\.]*)(\.effects)?/) {
        my $tp = ($3 eq 'true' ? 'T' : 'F');
        my $th = ($4 eq 'true' ? 'T' : 'F');
        my $io = ($6 eq 'true' ? 'T' : 'F');
        my $isTrans = $7;
        return "\\humanCfg".$isTrans."{$1}{$2}{$tp}{$th}{$5}{$io}";
    } elsif ($rdName =~ /^(e?v?drift)-best/) {
        return "\\bestCfg".$1."{}";
    } else {
        die "don't know how to parse rundef: $rdName\n";
    }
}
my %dm2domain = (
    ls => 'Polka_ls',
    st => 'Polka_st',
    pg => 'PolkaGrid',
    Oct => 'Octagon',
);
sub cfg2cmd {
    my ($subdir,$benchname, $cfg) = @_;
    if ($cfg =~ /TL([^-]+)-TP([^-]+)-TH([^-]+)-DM([^-]+)-IO([^-]+)-TR([^-]+)\.effects/) {
        my ($tl,$tp,$th,$dm,$io,$tr) = ($1,$2,$3,$4,$5,$6);
        return join(' ', (
            './drift.exe',
            '-file', "tests/effects/$subdir$benchname.ml",
            '-prop', "tests/effects/$subdir$benchname.yml.prp",
            '-ev-trans', $tr,
            '-trace-len', $tl,
            '-if-part', $tp,
            '-io-effects', $io,
            '-out', 2,
            '-domain', $dm2domain{$dm},
            '-thold', $th
        ))."\n";
    }
}
# my %run2tool = (
#     'drift-new-len0.effects' => 'EDrift len0',
#     'drift-new-len1.effects' => 'EDrift len1',
#     'drift-trans-len0.effects' => 'Trans+Drift len0',
#     'drift-trans-len1.effects' => 'Trans+Drift len1',
#     'default.mochibenchmarks' => 'CPS+Mochi',
#     'CA-March20.effects' => '3/20/24 TP',
#     'CA-March20-trans.effects' => '3/20/24 TP + trans'
# );
######################################################################
use List::Util qw(product);
use Math::Complex;
sub geometric_mean {
    my @numbers = @_; # Get the list of integers passed to the function
    warn "geo_mean on empty list".Dumper(\@numbers) unless @numbers;
    return 1 unless @numbers; # Return 1 if the list is empty

    my $product = product(@numbers); # Calculate the product of all integers in the list
    my $n = scalar @numbers; # Count the total number of integers
    return sprintf("%0.1f", exp(log($product)/$n));
}
######################################################################

sub cleanRes {
    my ($r) = @_;
    return '\Chk' if $r eq 'true';
    return '\TO' if $r eq 'TIMEOUT';
    return '\TO' if $r > 900;
    return '\MO' if $r eq 'OUT OF MEMORY';
    return '\Unk' if $r eq 'unknown';
    return '\ERR' if $r eq 'ERROR (1)';
    return $r;    
}


my $d;
sub parseResultsFile {
    my ($fn) = @_;
    open F, $fn or die "opening $fn - $!";
    # unfortunately the mochi output is a little different (one fewer column) then the other CSV
    #warn "TODO parseResultsFile decide based on $fn";
    my $isCoarMochi = ($fn =~ /coarmochi/ ? 1 : 0);
    my $isRealMochi = ($fn =~ /realmochi/ ? 1 : 0);
    my $isRethfl    = ($fn =~ /rethfl/ ? 1 : 0);
    my $isDriftWrap = ($fn =~ /driftwrap/ ? 1 : 0);

#====
# tool    mochi   mochi   mochi
# run set default.realmochibenchmarks     default.realmochibenchmarks     default.realmochibenchmarks
# ../../tests/effects/tr_tuple_mochi/     status  cputime (s)     walltime (s)
# all-ev-pos.ml   true    1.377243        1.4184973880037433
# alt-inev.ml     OUT OF MEMORY   295.228083      295.32891325000674
#====
# tool            drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  drift 0.0.todo  d>
# run set         NOTEoopsla25mar18-TL0-TPfalse-THtrue-DMls-IOtrue-TRdirect.effects       NOTEoopsla25mar18-TL0-TPfalse-THtrue-DMls-IOtrue-TRdirect.effects       NOTEoopsla25mar18-TL0-TPfalse-THtrue-DMls-IOtrue-TRdirect.effects       NOTEoopsl>
# ../../tests/effects/            status  cputime (s)     walltime (s)    status  cputime (s)     walltime (s)    status  cputime (s)     walltime (s)    status  cputime (s)     walltime (s)    status  cputime (s)     walltime (s)    status  c>
# all-ev-pos.yml  all-ev-pos.yml  true    0.300607        0.3039371459999529      unknown
#
    my @runSets;
    while(<F>) {
        next if /^tool/; # ignore line 1
        next if /\tstatus\t/; # ignore line 3
        # capture the runDef names
        # these will be repeated 3x starting with col idx 1: _ _ A A A B B B ...
        if (/^run set/) {
            chomp($_);
            @runSets = split /\t/, $_;
            # trim off the first column
            shift @runSets;
            # trim another column, but only when we are exploring all possible drift configs
            shift @runSets unless ($isCoarMochi || $isRealMochi || $isRethfl || $isDriftWrap);
            #die Dumper(\@runSets) if $isDriftWrap;

        } else {
            chomp($_);
            # trim off the first column, saving it as the bench name
            my ($bench,@RCWs) = split /\t/, $_;
            # trim another column for Drift
            shift @RCWs unless ($isCoarMochi || $isRealMochi || $isRethfl || $isDriftWrap);
            # ignore some benchmarks
            next if $bench =~ /higher-order-disj/;
            next if $bench =~ /traffic/;
            next if $bench =~ /kobayashi/;
            $bench =~ s/cps_// if $isCoarMochi;
            $bench =~ s/\.y?ml$//;
            # traverse the columns 
            for(my $i=0; $i <= $#RCWs; $i+=3) {
                $d->{$bench}->{$runSets[$i]}->{res} = $RCWs[$i];
                $d->{$bench}->{$runSets[$i]}->{cpu} = $RCWs[$i+1];
                $d->{$bench}->{$runSets[$i]}->{wall} = $RCWs[$i+2];
                $d->{$bench}->{$runSets[$i]}->{mem} = 'missing-BE'; # $RCWMs[$i+3];
                $d->{$bench}->{$runSets[$i]}->{rd}  = $runSets[$i];
                $d->{$bench}->{$runSets[$i]}->{bench}  = $bench;
                #print Dumper(\@runSets,$d->{$bench}->{$runSets[$i]});
#                push @RUNDEFINITIONS, $runSets[$i];
            }
            #my ($fn,$res,$cpu,$wall,$mem) = split /\t/, $_;
            # $fn =~ s/^cps_//;
            # $fn =~ s/_/\\_/g;
            # printf("%-30s & %-5s & %0.2f & %0.2f & %0.2f \\\\\n",
            #     $fn, cleanRes($res), $cpu, $wall, $mem);
        }
    }
}

foreach my $fn (@resultsfiles) {
    parseResultsFile($fn);
}
#die "here\n".Dumper($d->{'all-ev-pos'});
sub newBest {
    my ($BEST,$bench,$rd) = @_;
    if ($bench eq 'disj-gte' and $BEST eq 'BEST_TRANS') {
        print "Found new best for $BEST on $bench: $rd  - $d->{$bench}->{$rd}->{res}\n";
    }
    # We only TP with Drift when it's for BEST_TPON_DRIFT
    if ($rd =~ /TPtrue-.*TRtrans/) {
        # don't allow adoption of this possible new best
        return unless $BEST =~ /BEST_TP/;
    }

    $d->{$bench}->{$BEST}->{res}  = $d->{$bench}->{$rd}->{res};
    $d->{$bench}->{$BEST}->{cpu}  = $d->{$bench}->{$rd}->{cpu};
    $d->{$bench}->{$BEST}->{wall} = $d->{$bench}->{$rd}->{wall};
    $d->{$bench}->{$BEST}->{mem}  = $d->{$bench}->{$rd}->{mem};
    $d->{$bench}->{$BEST}->{rd}   = $rd;
}
# compute the best (non-mochi) run set (for Table 1)
foreach my $bench (sort keys %$d) {
    my $done = 0;
    foreach my $rd (keys %{$d->{$bench}}) {
        next if $rd =~ /BEST/;
        next if $rd =~ /mochi/i;
        next if $rd =~ /rethfl/i;
        # For Drift+Trans, don't allow Trace Partitioning
        next if $rd =~ /TRtrans/ && $rd =~ /TPtrue/;
#        next unless $d->{$bench}->{$rd}->{res} eq 'true';
        # which are we improving, Drift or evDrift?
        my $BEST;
        if ($rd =~ /best/) {
            # this rundef is from driftwrapped
            $BEST = ($rd =~ /evdrift-best/i ? 'BEST_DRIFTEV' : 'BEST_TRANS');
        } else {
            # this rundef is from the collections of rundefs
            $BEST = ($rd =~ /trans/i ? 'BEST_TRANS' : 'BEST_DRIFTEV');
        }
        # we have nothing yet, so we take it
        if ($d->{$bench}->{$BEST}->{rd} !~ /[a-z]/) {
            if ($BEST eq 'BEST_DRIFTEV') { ++$done; die "bad" if $done++ > 1; }
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
#print Dumper($d);

open EXT, ">exp-apx.tex" or die $!;
# print EXT "     ";
# foreach my $tool (@RUNDEFINITIONS) {
#     print EXT " & \\multicolumn{3}{|c||}{$run2tool{$tool}}";
# }
# print EXT "\\\\ \n";
# print EXT "{\\bf Bench} ";
# foreach my $tool (@RUNDEFINITIONS) {
#     print EXT " & {\\bf Res} & {\\bf CPU} & {\\bf Mem} ";
# }
# print EXT "\\\\ \n";
# print EXT "\\hline\n";
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
        warn "run2tool($tool)\n";
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

### Generate paper body table showing only the best

open BODY, ">exp-body.tex" or die $!;
open SCRIPT, ">generate_table1" or die $!;
open UNSAFE, ">runall_unsafe" or die $!;
open CFG_DRIFT, ">best_configs_drift.csv" or die $!;
open CFG_EVDRIFT, ">best_configs_evdrift.csv" or die $!;
# print BODY "     ";
# foreach my $tool (@RUNDEFINITIONS) {
#     print BODY " & \\multicolumn{3}{|c||}{$run2tool{$tool}}";
# }
# print BODY "\\\\ \n";
# print BODY "{\\bf Bench} ";
# foreach my $tool (@RUNDEFINITIONS) {
#     print BODY " & {\\bf Res} & {\\bf CPU} & {\\bf Mem} ";
# }
# print BODY "\\\\ \n";
# print BODY "\\hline\n";
my @geos_coarmochi; my @geos_realmochi; my @geos_rethfl; my @geos_evtrans; my @geos_direct;
my $newOverCoarMochi = 0; my $newOverRealMochi = 0; my $newOverRethfl = 0; my $newOverTrans = 0; my $benchCount = 0; $ct = 1;
my $driftVerified = 0; my $evdriftVerified = 0;
my $realmochiVerified = 0; my $rcamlVerified = 0; my $rethflVerified = 0;
my @bothSolved; my @evAndMochiSolved; my @evAndRcamlSolved; my @evAndRethflSolved; 
#print Dumper($d->{overview1});
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
    print SCRIPT "\n#\n# Benchmark: $b\n#\n";

    # Trans-Drift
    print BODY sprintf("& %-5s & %3.1f & %s ", # %3.1f & 
           cleanRes($d->{$b}->{BEST_TRANS}->{res}),
           $d->{$b}->{BEST_TRANS}->{cpu},
           #$d->{$b}->{BEST_TRANS}->{mem},
           run2tool($d->{$b}->{BEST_TRANS}->{rd}));
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
           run2tool($d->{$b}->{BEST_DRIFTEV}->{rd}));
    #printf "best EDrift result for %-40s : %-10s : %s\n", $b, $d->{$b}->{BEST_DRIFTEV}->{res}, $d->{$b}->{BEST_DRIFTEV}->{rd};
    printf "best Drift   result for %-40s : %-10s : %s\n", $b, $d->{$b}->{BEST_TRANS}->{res}, $d->{$b}->{BEST_TRANS}->{rd};
    printf "best evDrift result for %-40s : %-10s : %s\n", $b, $d->{$b}->{BEST_DRIFTEV}->{res}, $d->{$b}->{BEST_DRIFTEV}->{rd};
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
    # script for drift and evdrift
    print SCRIPT "# Drift\n".cfg2cmd('',$b,$d->{$b}->{BEST_TRANS}->{rd});
    print SCRIPT "# evDrift\n".cfg2cmd('',$b,$d->{$b}->{BEST_DRIFTEV}->{rd});
    # save the best configurations for Drift/evDrift:
    my $args = cfg2cmd('',$b,$d->{$b}->{BEST_TRANS}->{rd});
    $args =~ s/^\.\/drift.exe //; $args =~ s/\n//;
    print CFG_DRIFT "tests/effects/$b.ml|$args|$d->{$b}->{BEST_TRANS}->{rd}\n";
    $args = cfg2cmd('',$b,$d->{$b}->{BEST_DRIFTEV}->{rd});
    $args =~ s/^\.\/drift.exe //; $args =~ s/\n//;
    print CFG_EVDRIFT "tests/effects/$b.ml|$args|$d->{$b}->{BEST_DRIFTEV}->{rd}\n";
    #print UNSAFE "# Drift on $b:\n".cfg2cmd('unsafe/',$b,$d->{$b}->{BEST_TRANS}->{rd});
    #print UNSAFE "# evDrift on $b:\n".cfg2cmd('unsafe/',$b,$d->{$b}->{BEST_DRIFTEV}->{rd});
    print SCRIPT "# MoCHi\n"
       ."./mochi.exe -use-temp tests/effects/tr_tuple_mochi/$b.ml\n";
    print SCRIPT "# RCaml\n"
       ."./rcaml.exe -c config/solver/rcaml_wopp_spacer.json -p ml "
       ."   tests/effects/tr_tuple_rcaml/$b.ml\n";
    print SCRIPT "# Rethfl\n"
       ."./rethfl --solver=eldarica "
       ."   tests/effects/tr_tuple_hflz/$b.ml\n";
    print UNSAFE cfg2cmd('unsafe/',$b,$d->{$b}->{BEST_DRIFTEV}->{rd});
}
close BODY;
close SCRIPT;
close UNSAFE;
close CFG_DRIFT;
close CFG_EVDRIFT;
print "wrote: exp-body.tex\n";
print "wrote: generate_table1\n";
print "wrote: runall_unsafe\n";
print "wrote: best_configs_drift.csv\n";
print "wrote: best_configs_evdrift.csv\n";

# calculate the speedup on the ones that Drift and evDrift BOTH solved
my @driftTimes  = map { $d->{$_}->{BEST_TRANS}->{cpu} } @bothSolved;
my @evDiftTimes = map { $d->{$_}->{BEST_DRIFTEV}->{cpu} } @bothSolved;
# print "GM of evtrans:".Dumper(\@geos_evtrans, 0+@geos_evtrans);
# print "GM of direct:".Dumper(\@geos_direct, 0+@geos_direct);
#print "times:".Dumper(\@driftTimes, \@evDiftTimes);
my $speedupEVoverDrift = geometric_mean(@driftTimes)/geometric_mean(@evDiftTimes);

# calculate speedup of Real Mochi over evDrift
warn "ev+mochi solved: ".join(',',@evAndMochiSolved)."\n";
my @EVMO_ev_times = map { $d->{$_}->{BEST_DRIFTEV}->{cpu} } @evAndMochiSolved;
my @EVMO_mo_times = map { $d->{$_}->{$REALMOCHI_RD}->{cpu} } @evAndMochiSolved;
my $speedupEVoverRealMochi = geometric_mean(@EVMO_mo_times)/geometric_mean(@EVMO_ev_times);

# calculate speedup of RCaml over evDrift
warn "ev+rcaml solved: ".join(',',@evAndRcamlSolved)."\n";
my @EVMO_ev_times = map { $d->{$_}->{BEST_DRIFTEV}->{cpu} } @evAndRcamlSolved;
my @EVMO_rc_times = map { $d->{$_}->{$COARMOCHI_RD}->{cpu} } @evAndRcamlSolved;
my $speedupEVoverRcaml = geometric_mean(@EVMO_rc_times)/geometric_mean(@EVMO_ev_times);

# calculate speedup of Rethfl over evDrift
warn "ev+rethfl solved: ".join(',',@evAndRethflSolved)."\n";
my @EVMO_ev_times = map { $d->{$_}->{BEST_DRIFTEV}->{cpu} } @evAndRethflSolved;
my @EVMO_rc_times = map { $d->{$_}->{$RETHFL_RD}->{cpu} } @evAndRethflSolved;
my $speedupEVoverRethfl = geometric_mean(@EVMO_rc_times)/geometric_mean(@EVMO_ev_times);


### Generate trace partition comparison

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
   ('\newcommand\expSpeedupEVoverRealMochi{'.$speedupEVoverRealMochi.'}'),
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
   "% TP Improvements:",
   ('\newcommand\expTPGMoffDrift{'.geometric_mean(@geos_notp_drift).'}'),
   ('\newcommand\expTPGMonDrift{'.geometric_mean(@geos_tp_drift).'}'),
   ('\newcommand\expTPGMoffevDrift{'.geometric_mean(@geos_notp_evdrift).'}'),
   ('\newcommand\expTPGMonevDrift{'.geometric_mean(@geos_tp_evdrift).'}'),
   ('\newcommand\expTPSpeedupDrift{'.sprintf("%0.1f", geometric_mean(@geos_notp_drift)/geometric_mean(@geos_tp_drift)).'}'),
   ('\newcommand\expTPSpeedupevDrift{'.sprintf("%0.1f", geometric_mean(@geos_notp_evdrift)/geometric_mean(@geos_tp_evdrift)).'}'),
   ('\newcommand\expTPNew{'.$newTPOverNoTPevDrift.'}'),
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


# while(<DATA>) {
#     next if /^tool/;
#     next if /^run set/;
#     next if /\tstatus\t/;
#     my ($fn,$res,$cpu,$wall,$mem) = split /\t/, $_;
#     $fn =~ s/^cps_//;
#     $fn =~ s/_/\\_/g;
#     printf("%-30s & %-5s & %0.2f & %0.2f & %0.2f \\\\\n",
#         $fn, cleanRes($res), $cpu, $wall, $mem);
# }

__DATA__
tool	coarmochi NO_VERSION_UTIL	coarmochi NO_VERSION_UTIL	coarmochi NO_VERSION_UTIL	coarmochi NO_VERSION_UTIL
run set	default.mochibenchmarks	default.mochibenchmarks	default.mochibenchmarks	default.mochibenchmarks
../../tests/effects/mochi/	status	cputime (s)	walltime (s)	memory (MB)
cps_acc-pos-net0-negated.ml	OUT OF MEMORY	39.561118434	39.52044827118516	999.997440
cps_acc-pos-net0-similar-negated.ml	OUT OF MEMORY	40.443142798	40.41010303609073	999.997440
cps_acc-pos-net0-similar.ml	TIMEOUT	900.251731749	900.2475049868226	454.119424
cps_acc-pos-net0.ml	TIMEOUT	900.254324895	900.2498775236309	448.516096
cps_all-ev-even-sink.ml	unknown	0.031445387	0.0340123288333416	35.680256
cps_assert-true.ml	unknown	4.797628357	4.7890766356140375	92.082176
cps_depend.ml	OUT OF MEMORY	57.696388143	57.63335299119353	999.997440
cps_disj.ml	OUT OF MEMORY	237.340026792	237.0736487712711	999.997440
cps_ho-shrink.ml	TIMEOUT	900.261575017	900.1133371777833	999.997440
cps_last-ev-even.ml	unknown	3.632835373	3.6285442896187305	99.291136
cps_max-min.ml	TIMEOUT	900.272918367	900.2402329705656	999.997440
cps_min-max.ml	TIMEOUT	900.2684614	900.2167596127838	999.997440
cps_order-irrel.ml	unknown	18.950768298	18.91433823108673	143.339520
cps_resource-analysis.ml	TIMEOUT	900.253579096	900.2900423351675	270.442496
cps_sum-appendix.ml	unknown	0.036942231	0.03894921950995922	36.855808
cps_sum-of-ev-even.ml	unknown	3.27330404	3.268652807921171	88.887296
cps_traffic_light_fo_simple.ml	OUT OF MEMORY	254.436113917	254.29541855305433	999.997440
table-generator results/benchmark-drift.2024-02-25_12-16-28.results.drift-new-len0.effects.xml.bz2 results/benchmark-drift.2024-02-25_12-16-28.results.drift-new-len1.effects.xml.bz2 results/benchmark-drift.2024-02-25_12-16-28.results.drift-trans-len0.effects.xml.bz2 results/benchmark-drift.2024-02-25_12-16-28.results.drift-trans-len1.effects.xml.bz2
