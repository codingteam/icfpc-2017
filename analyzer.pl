#!/usr/bin/env perl

%strategies;
$score = 0;
$fullfilledFutures = 0;
$totalFutures = 0;

while (<>) {
  chomp;
  if (/: Selected strategy: org.codingteam.icfpc2017.strategy.([a-zA-Z]+)/) {
    # printf("Strategy: $1\n");
    $strategies{$1}++;
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
