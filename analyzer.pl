#!/usr/bin/env perl

use warnings;
use strict;
use List::Util qw( sum0 );

my %strategies = (
  "ComponentConnectorStrategy" => 0,
  "DumbObstructorStrategy" => 0,
  "FutureStrategy" => 0,
  "GreedyStrategy" => 0,
  "MineOccupationStrategy" => 0,
  "RandomConnectorStrategy" => 0);

my $score = 0;
my $fullfilledFutures = 0;
my $totalFutures = 0;

my $currentStrategy = "";
my $lastScore = 0;
while (<>) {
  chomp;
  if (/: Selected strategy: org.codingteam.icfpc2017.strategy.([a-zA-Z]+)/) {
    $currentStrategy = $1;
  }
  if (/INFO o.c.i.HandlerLoop\$: Our expected score: (\d+)/) {
    $strategies{$currentStrategy} += $1 - $lastScore;
    $lastScore = $1;
  }
  if (/INFO o.c.i.HandlerLoop\$: Our score: (\d+). Futures fullfilled: (\d+) of (\d+)/) {
    $score = $1;
    $fullfilledFutures = $2;
    $totalFutures = $3;
  }
}

foreach my $strategy (sort keys %strategies) {
  printf "%s: %s\n", $strategy, $strategies{$strategy};
}

print "Our score: $score. Futures fullfilled: $fullfilledFutures of $totalFutures.\n";

my $W = sum0 (values %strategies);
my $cw = $strategies{"ComponentConnectorStrategy"} / $W;
my $dw = $strategies{"DumbObstructorStrategy"} / $W;
my $fw = $strategies{"FutureStrategy"} / $W;
my $gw = $strategies{"GreedyStrategy"} / $W;
my $mw = $strategies{"MineOccupationStrategy"} / $W;
my $rw = $strategies{"RandomConnectorStrategy"} / $W;

printf "Suggested coefficients: %.3f %.3f %.3f %.3f %.3f %.3f\n", $gw, $fw, $mw, $cw, $dw, $rw;
print "                        |     |     |     |     |     ` random connector\n";
print "                        |     |     |     |      ` dumb obstructor\n";
print "                        |     |     |      ` component connector\n";
print "                        |     |      ` mine occupation\n";
print "                        |      ` future\n";
print "                         ` greedy\n";
