#!/usr/bin/perl

use Data::Dumper;
use DateTime;
use Time::ParseDate;

use Exporter qw/import/;
our @EXPORT_OK = qw/compute_best_drift cleanRes geometric_mean run2tool %dm2domain cfg2cmd parse_results_files/;
use strict;


sub newBest {
    my ($d,$BEST,$bench,$rd) = @_;
    # We only TP with Drift when it's for BEST_TPON_DRIFT
    # if ($rd =~ /TPtrue-.*TRtrans/) {
    #     # don't allow adoption of this possible new best
    #     die "hm";
    #     return unless $BEST =~ /BEST_TP/;
    # }
    # print "rd: $rd\n";
    $d->{$bench}->{$BEST}->{res}  = $d->{$bench}->{$rd}->{res};
    $d->{$bench}->{$BEST}->{cpu}  = $d->{$bench}->{$rd}->{cpu};
    $d->{$bench}->{$BEST}->{wall} = $d->{$bench}->{$rd}->{wall};
    $d->{$bench}->{$BEST}->{mem}  = $d->{$bench}->{$rd}->{mem};
    $d->{$bench}->{$BEST}->{rd}   = $rd;
    return $d;
}
sub compute_best_drift {
    my ($d) = @_;

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
                $d = newBest($d,$BEST,$bench,$rd);
            # does it improve because previously BEST coudln't prove it?
            } elsif ($d->{$bench}->{$BEST}->{res} ne 'true') {
                $d = newBest($d,$BEST,$bench,$rd);
            # does it improve because it's faster?
            } elsif ($d->{$bench}->{$rd}->{res} eq 'true'
                    && $d->{$bench}->{$rd}->{cpu} < $d->{$bench}->{$BEST}->{cpu}) {
                $d = newBest($d,$BEST,$bench,$rd);
            } else {
                # warn "not better\n";
            }
        }    
    }
}


# 4) Give human-readible names for these rundefinitions for column headers:
sub run2tool {
    my ($rdName) = @_;
    return 'Tuple+Rethfl' if $rdName eq 'default.rethflbenchmarks';
    return 'CPS+COaRRCaml' if $rdName eq 'default.mochibenchmarks';
    return 'Tuple+Mochi' if $rdName eq 'default.realmochibenchmarks';
    if($rdName =~ /NOTE(.*)-TL(.*)-TP(.*)-TH(.*)-DM(.*)-IO([^-]+)-TR([^\.]*)(\.(ev)?drifttasks)?/) {
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
    if ($cfg =~ /TL([^-]+)-TP([^-]+)-TH([^-]+)-DM([^-]+)-IO([^-]+)-TR([^-]+)\.(ev)?drifttasks/) {
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

sub parseResultsFile {
    my ($d,$fn) = @_;
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
            print;
            chomp($_);
            # trim off the first column, saving it as the bench name
            my ($bench,@RCWs) = split /\t/, $_;
            # trim another column if it is a property
            shift @RCWs if $RCWs[0] =~ /yml/;
            # shift @RCWs unless ($isCoarMochi || $isRealMochi || $isRethfl || $isDriftWrap);
            # ignore some benchmarks
            next if $bench =~ /higher-order-disj/;
            next if $bench =~ /traffic/;
            next if $bench =~ /kobayashi/;
            next if $bench =~ /intro-ord3/;
            next if $bench =~ /num_evens/;
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
                # sanity check
                die "parsing error: runset $runSets[$i] no CPU.".Dumper(\@RCWs)
                   unless $RCWs[$i+1] =~ /\d\.\d/;
                die "parsing error: runset $runSets[$i] no Wall.".Dumper(\@RCWs)
                   unless $RCWs[$i+2] =~ /\d\.\d/;
                die "parsing error: runset $runSets[$i] no result.".Dumper(\@RCWs)
                   unless $RCWs[$i] =~ /unknown|TIMEOUT|OUT OF MEMORY|ERROR|true/;
                   print Dumper($d->{$bench}->{$runSets[$i]});
            }
        }
    }
    return $d;
}

sub parse_results_files {
    my ($resultFileList) = @_;
    my $d;
    foreach my $fn (@$resultFileList) {
        $d = parseResultsFile($d,$fn);
    }
    return $d;
}

1;