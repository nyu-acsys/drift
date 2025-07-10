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

# Check for exactly 3 arguments
if (@ARGV != 3) {
    die "Usage: $0 [Drift|evDrift] best_configs.csv <benchmark.ml>\n";
}

my ($DriftOrEv,$config_file,$benchmark) = @ARGV[0..2];

# Open the config file
open(my $fh, '<', $config_file) or die "Could not open '$config_file': $!";

my @args;
while (my $line = <$fh>) {
    chomp $line;
    next if $line =~ /^\s*$/;        # Skip empty lines
    next if $line =~ /^\s*#/;        # Skip comments

    my ($bench, $argstr) = split(/\|/, $line, 2);
    if (defined $bench && $bench eq $benchmark) {
        @args = split(/\s+/, $argstr // '');
        last;
    }
}
close($fh);

# If no matching benchmark found
if (!@args) {
    die "Benchmark '$benchmark' not found or has no arguments in config file.\n";
}

# Replace the current process with the new program Y and its arguments
exec('echo', @args) or die "Failed to exec Drift: $!\n";
