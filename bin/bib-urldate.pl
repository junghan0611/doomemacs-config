#!/usr/bin/env perl
# bib-urldate.pl — Extract BibTeX entries by urldate range
# Usage: bib-urldate.pl FROM TO FILE...
# Output: KEY\tTITLE per line
use strict;
use warnings;

my $from = shift or die "Usage: $0 FROM TO FILE...\n";
my $to   = shift or die "Usage: $0 FROM TO FILE...\n";

local $/ = "\n\n";  # paragraph mode (entry separator)

while (<>) {
    if (/urldate\s*=\s*\{(\d{4}-\d{2}-\d{2})\}/) {
        my $ud = $1;
        if ($ud ge $from && $ud le $to) {
            /^\@\w+\{([^,]+)/ or next;
            my $key = $1;
            /title\s*=\s*[\{"]([^}"]+)/ or next;
            my $title = $1;
            $title =~ s/[\{\}]//g;
            print "$key\t$title\n";
        }
    }
}
