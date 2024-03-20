#!/usr/bin/perl

use strict;
use warnings;

chdir("../../tests/effects") or die $!;

opendir(my $dh, '.') or die "Cannot open directory: $!";
my @files = readdir($dh) or die $!;
closedir($dh);

# Process each .prp file
foreach my $file (@files) {
    if ($file =~ /^(.*)\.yml\.prp$/) {
        print "Generating Yaml for $file... ";
        my $rt = $1;
        if (!-e "$rt.ml") {
            die "can't find an ML file for $file\n";
        }
        my $yaml_file = $rt . ".yml";

        # Open the new file for writing
        open(my $fh, '>', $yaml_file) or die "Cannot create file $yaml_file: $!";
        print $fh <<EOT;
# https://gitlab.com/sosy-lab/benchmarking/task-definition-format

format_version: "2.0"

input_files:
  - '$rt.ml'

properties:
  - property_file: $rt.yml.prp
    expected_verdict: true
EOT
        chmod 0777, $fh;
        close($fh);


        print "Created file: $yaml_file\n";
    } else {
        #print "Ignoring: $file\n";
    }
}

__DATA__
