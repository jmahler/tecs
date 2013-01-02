#!/usr/bin/perl
use strict;

#
# DESCRIPTION
#
# This is a assembler designed for the assembly language described
# in the book "The Elements of Computing Systems" by Noam Nissan
# and Shimon Schocken.
#
# SYNOPSIS
#
# ./Assembler.pl add/Add.asm > out.hack
#

my $in_file = $ARGV[0];
open IN, "< $in_file"
	or die "$!";

my $label_alloc = 16;

my @lines;

# symbol table
my %symtbl;

# {{{ Stage I, construct symbol table
#
# A cursory examination of the source and
# construction of a symbol table.
# Saves a condensed version of the source
# in to @lines2 to processed in the second stage.

my @lines2;

my @symbols;  # found symbols

my $linen = 0;  # line number in file
my $objlinen = 0;  # line numbers in assembled file, memory offset
foreach my $line (<IN>) {
	$linen++;

	# remove white space and comments
	$line =~ s/\r//;
	chomp($line);
	$line =~ s/^\s*//;
	$line =~ s/\/\/.*$//;

	# skip blank lines
	next if $line eq '';

	# named address
	#   e.g. (LOOP)
	if ($line =~ /\(.*\)/) {
		my $x = $line;
		$x =~ s/^\(//;
		$x =~ s/\)$//;
		push @symbols, [$x, $objlinen];
		next;
	} elsif ($line =~ /^\@\d+$/) {
		push @lines2, $line;
	} elsif ($line =~ /^\@SP$/) {
		push @lines2, '@0';
	} elsif ($line =~ /^\@LCL$/) {
		push @lines2, '@1';
	} elsif ($line =~ /^\@ARG$/) {
		push @lines2, '@2';
	} elsif ($line =~ /^\@THIS$/) {
		push @lines2, '@3';
	} elsif ($line =~ /^\@THAT$/) {
		push @lines2, '@4';
	} elsif ($line =~ /^\@R\d\d?/) {
		my $x = $line;
		$x =~ s/^\@R//;
		push @lines2, "\@$x";
	} elsif ($line =~ /^\@SCREEN$/) {
		push @lines2, '@16384';
	} elsif ($line =~ /^\@KBD$/) {
		push @lines2, '@24576';
	} elsif ($line =~ /^\@.*$/) {
		# otherwise the address is a label
		my $x = $line;
		$x =~ s/^\@//;
		push @symbols, [$x, ''];
		push @lines2, $line;
	} else {
		# Some non-address command
		push @lines2, $line;
	}

	# We have a line of code that is not a named address
	$objlinen++;
}

# complete the symbol table

# first fill in the address references
foreach my $sym (@symbols) {
	my ($key, $n) = @$sym;

	if ($n ne '') {
		$symtbl{$key} = $n;
	}
}
# then allocate for the variables
foreach my $sym (@symbols) {
	my ($key, $n) = @$sym;

	if (! exists $symtbl{$key}) {
		$symtbl{$key} = $label_alloc++;
	}
}

close(IN);
# }}}

# {{{ Stage II, assemble

my $linen = 0;
foreach my $line (@lines2) {
	$linen++;

	if ($line =~ /^\@/) {
		# A-command
		# @1234 or @name

		# remove the '@'
		$line =~ s/^\@//;

		my $x = $line;
		if ($x =~ /^\d+/) {
			# if a number, ready to be converted
		} else {
			if ((! exists $symtbl{$x}) or $symtbl{$x} eq '') {
				print STDERR "Unresolved symbol '$x'\n";
				exit 1;
			}

			$x = $symtbl{$x};
		}

		# pack it as an unsigned short (16-bit)
		$x = pack("S", $x);
		# unpack it as a binary 
		($x) = unpack("b[16]", $x);
		# fix little endian on right side (not left)
		$x = reverse "$x";

		print "$x\n";
	} elsif ($line =~ /^\s*([0ADM]);\s*(J\w+)\s*/) {
		# A JUMP command

		my ($dest, $jmp) = ($1, $2);

		my @d = qw(0 0 0); # default
		$d[0] = "1" if ($dest =~ /A/);
		$d[1] = "1" if ($dest =~ /D/);
		$d[2] = "1" if ($dest =~ /M/);
		my $d = join "", @d;

		my @jcfg = (
			["JGT", "001"],
			["JEQ", "010"],
			["JGE", "011"],
			["JLT", "100"],
			["JNE", "101"],
			["JLE", "110"],
			["JMP", "111"],
		);

		# default
		my $j;
		my $c;
		my $a = "0";

		my $i;
		for ($i = 0; $i < @jcfg; $i++) {
			if ($jmp =~ /^$jcfg[$i]->[0]/) {
				$j = $jcfg[$i]->[1];
				if ($d eq "001") {  # M
					$c = "110000";
					$a = "1";
				} elsif ($d eq "100") { # A
					$c = "110000";
				} elsif ($d eq "000") {
					$c = "101010";
				} else {
					$c = "001100";
				}

				last;
			}
		}
		if ($i == @jcfg) {
			print STDERR "jump command '$jmp' not found\n";
			exit 1;
		}

		print "1" . "11" . $a . $c . "000" . $j . "\n";
	} else {
		# C-Command
		# D=A
		# D=D+A
		# D=1
		# D=!D
		# ...

		# i xx a cccccc ddd jjj
		# Refer to Chapter 6 for a table of all the command options.

		my ($dest, $cX) = split /\=/, $line;
		my ($comp) = split /\s+/, $cX;

		my $ccfg = [
			# match   ccccccc   a
			["0",    [qw(1 0 1 0 1 0)], "0"],
			["1",    [qw(1 1 1 1 1 1)], "0"],
			["-1",   [qw(1 1 1 0 1 0)], "0"],
			["D",    [qw(0 0 1 1 0 0)], "0"],
			["A",    [qw(1 1 0 0 0 0)], "0"],
			["M",    [qw(1 1 0 0 0 0)], "1"],
			["!D",   [qw(0 0 1 1 0 1)], "0"],
			["!A",   [qw(1 1 0 0 0 1)], "0"],
			["!M",   [qw(1 1 0 0 0 1)], "1"],
			["-D",   [qw(0 0 1 1 1 1)], "0"],
			["-A",   [qw(1 1 0 0 1 1)], "0"],
			["-M",   [qw(1 1 0 0 1 1)], "1"],
			["D+1",  [qw(0 1 1 1 1 1)], "0"],
			["A+1",  [qw(1 1 0 1 1 1)], "0"],
			["M+1",  [qw(1 1 0 1 1 1)], "1"],
			["D-1",  [qw(0 0 1 1 1 0)], "0"],
			["A-1",  [qw(1 1 0 0 1 0)], "0"],
			["M-1",  [qw(1 1 0 0 1 0)], "1"],
			["D+A",  [qw(0 0 0 0 1 0)], "0"],
			["D+M",  [qw(0 0 0 0 1 0)], "1"],
			["D-A",  [qw(0 1 0 0 1 1)], "0"],
			["D-M",  [qw(0 1 0 0 1 1)], "1"],
			["A-D",  [qw(0 0 0 1 1 1)], "0"],
			["M-D",  [qw(0 0 0 1 1 1)], "1"],
			["D&A",  [qw(0 0 0 0 0 0)], "0"],
			["D&M",  [qw(0 0 0 0 0 0)], "1"],
			["D|A",  [qw(0 1 0 1 0 1)], "0"],
			["D|M",  [qw(0 1 0 1 0 1)], "1"],
		];

		my $a = "0";
		my @c = qw(0 0 0 0 0 0);

		my $i;
		for ($i = 0; $i < @$ccfg; $i++) {
			if ($comp eq $ccfg->[$i][0]) {
				@c = @{$ccfg->[$i][1]};
				$a = $ccfg->[$i][2];
				last;
			}
		}
		if ($i == @$ccfg) {
			print STDERR "command '$line' not found\n";
			exit 1;
		}

		my $c = join "", @c;

		my @d = qw(0 0 0); # default
		$d[0] = "1" if ($dest =~ /A/);
		$d[1] = "1" if ($dest =~ /D/);
		$d[2] = "1" if ($dest =~ /M/);
		my $d = join "", @d;

		print "1" . "11" . $a . $c . $d . "000" . "\n";
	}
}

# }}}

