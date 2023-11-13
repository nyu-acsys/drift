#!/usr/bin/perl
use strict;
use warnings;

# Open the input file
my $filename = 'latest-results.csv'; # Replace with your file name
open(my $fh, '<', $filename) or die "Could not open file '$filename' $!";

print "\\begin{tabular}{|l|c|rrr|c|rrr|}\n";
print "\\hline\n";
print "                & \\multicolumn{4}{c|}{\\bf New Drift} & \\multicolumn{4}{c|}{\\bf Drift+Trans} \\\\\n";
print "{\\bf Benchmark} & {\\bf Res.} & {\\bf CPU (s)} & {\\bf Wall (s)} & {\\bf Mem (MB)} & {\\bf Res.} & {\\bf CPU (s)} & {\\bf Wall (s)} & {\\bf Mem (MB)} \\\\\n";
print "\\hline\n\\hline\n";

my $header1 = <$fh>;
my $header2 = <$fh>;
my $header3 = <$fh>;
# Read the file line by line
while (my $row = <$fh>) {
    chomp $row;
    # Skip empty lines
    next if $row =~ /^\s*$/;

    $row =~ s/_/\\_/g;
    # Split the row into columns
    my ($name, $name2, $D2res, $D2cpu, $D2wall, $D2mem, $D1res, $D1cpu, $D1wall, $D1mem) = split /\t/, $row;

    $name =~ s/\.yml/\.ml/;
    
    # Format and print the row for LaTeX
    printf ("%-28s & %-8s & %3.2f & %3.2f & %3.2f & %-8s & %3.2f & %3.2f & %3.2f \\\\\n",
        "\\texttt{".$name."}", $D2res, $D2cpu, $D2wall, $D2mem, $D1res, $D1cpu, $D1wall, $D1mem
    );
    print "\\hline\n";
}

print "\\end{tabular}\n";

close $fh;
