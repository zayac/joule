#!/usr/bin/perl

use strict;
use warnings;
use File::Basename;
use Time::HiRes;

if (scalar(@ARGV) != 3)
{
	die "Usage: test.pl <app> <suffix> <error_suffix>";
}

my $app = $ARGV[0];
my $suffix = $ARGV[1];
my $err_suffix = $ARGV[2];

my @tests = split(/\s+/, `find tests -type f`);

my $start_time = Time::HiRes::gettimeofday();
for my $test (sort @tests)
{
	next if $test !~ m/\.t$/;

	print "Running $test...\n";

	my $test_out = $test;
    my $test_err = $test;
	$test_out =~ s/\.t$/\.$suffix/;
	$test_err =~ s/\.t$/\.$err_suffix/;
    my $script_path = dirname $test;
	my $sys_ret = system("$script_path/run_one_test.sh $app $test $test_out $test_err");
}
my $end_time = Time::HiRes::gettimeofday();
printf("Overall execution time: %.5f\n", $end_time - $start_time);
