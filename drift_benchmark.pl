#!/usr/bin/perl
#
# This script runs a single benchmark, using the (previously discovered)
# best configuration for that benchmark.
#
# Example usage:
#
#  drift_benchmark.pl [Drift|evDrift] ./best_configs.csv tests/effects/all-ev-pos.ml
#
use strict;
use warnings;

# Check for exactly 2 arguments
if (@ARGV != 2) {
    die "Usage: $0 best_configs_evdrift.csv <tests/effects/benchmark.ml>\n";
}

my ($config_file,$benchmark) = @ARGV[0..1];
$benchmark =~ s{^.*tests/effects/}{tests/effects/};

# Open the config file
open(my $fh, '<', $config_file) or die "Could not open '$config_file': $!";

my @args;
while (my $line = <$fh>) {
    chomp $line;
    next if $line =~ /^\s*$/;        # Skip empty lines
    next if $line =~ /^\s*#/;        # Skip comments

    my ($bench, $argstr, $rdName) = split(/\|/, $line, 2);
    if (defined $bench && $bench eq $benchmark) {
        @args = split(/\s+/, $argstr // '');
        last;
    }
}
close($fh);

# If no matching benchmark found
if (!@args) {
    die "./drift_benchmark.pl: Benchmark '$benchmark' not found or has no arguments in config file.\n";
}

print "./drift_benchmark.pl: now Executing:\n  drift.exe ".join(' ',@args)."\n";
exec('drift.exe', @args) or die "Failed to exec Drift: $!\n";
