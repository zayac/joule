#!/usr/bin/perl

use strict;
use warnings;

if (scalar(@ARGV) != 4)
{
	die "Usage: compare_results.pl <ref_suffix> <my_suffix> <ref_error_suffix> <my_error_suffix>";
}

my $ref_suffix = $ARGV[0];
my $my_suffix = $ARGV[1];
my $ref_err_suffix = $ARGV[2];
my $my_err_suffix = $ARGV[3];

my @tests = split(/\s+/, `find tests -type f`);

my $ntests = 0;
my $npass = 0;

print "\n";

for my $test (sort @tests)
{
	next if $test !~ m/\.t$/;

	print "$test: ";

	$ntests++;

	my $testbase = $test;

	$testbase =~ s/\.t$//;

	my $reftest = "$testbase.$ref_suffix";
	my $mytest = "$testbase.$my_suffix";
	my $referrtest = "$testbase.$ref_err_suffix";
	my $myerrtest = "$testbase.$my_err_suffix";

    my $reftest_data = `cat $reftest`;
	my $mytest_data = `cat $mytest`;
	my $referrtest_data = `cat $referrtest`;
	my $myerrtest_data = `cat $myerrtest`;

	chomp($reftest_data);
	chomp($mytest_data);
	chomp($referrtest_data);
	chomp($myerrtest_data);
	
    if ($reftest_data eq $mytest_data && $referrtest_data eq $myerrtest_data)
	{
		$npass++;
		print "PASS\n";
	}
	else
	{
        sub print_file
        {
            my $file = $_[0];
            open my $data, $file or die $!;
            my @lines = <$data>;
            chomp @lines;
            my $count = 1;
            foreach my $line (@lines) {
                printf("%4d| %s\n", $count++, $line);
            }
            print "\n";
            close $file;
        }
		print "ERROR: Output does not match reference implementation\n\n";
        print "Input file:\n\n";
        print_file($test);
        print "Reference output:\n\n";
        print_file($reftest);
        print "Execution output:\n\n";
        print_file($mytest);
        print "Reference error output:\n\n";
        print_file($referrtest);
        print "Execution error output:\n\n";
        print_file($myerrtest);
		print "TEST FAIL\n";
		exit(1);
	}
}

print "\nALL TESTS PASS\n";
