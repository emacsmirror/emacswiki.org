#!/usr/bin/perl -w
#
# emo2html.pl version 1.0
#
#  Make HTML from an Emacs outline.
#
# AUTHOR: Mike Andrews <rocko@gweep.net>
#         http://www.gweep.net/~rocko/emo2html/
#
# Copyright (c) 2001 Mike Andrews.  All Rights Reserved.
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use, copy,
# modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions: The above
# copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

# @numerals: the input to the "type" attribute of the HTML <ol> tag.
# The default is set to standard outline format.
my @numerals_standard = ( '', 'I', 'A', '1', 'a', 'i');
my @numerals_numeric = ( '', '1', '1', '1', '1', '1');
my @numerals = @numerals_standard;

# @format_open and @format_close: HTML tags inserted before (open) and
# after (close) the current line of text.  The tags do not affect the
# outline numbering.
my @format_open = ( '<br><i>', '<b>', '', '', '', '' );
my @format_close = ( '</i>', '</b>', '', '', '', '' );

#---------------------------------------------------------------------
my $level = 0;
my $tll = 0; # This Line Level
my $line = '';
my $body = 1;

# 
if ($#ARGV + 1 < 1)
{
    print "\nThis is an Emacs outline to HTML converter.\n";
    print "\nUSAGE:  emo2html.pl [file] > [file.html]\n\n";
    print "  HTML is sent to the standard output.  If [file] is '-', the\n";
    print "  outline is taken from the standard input.  See the code for\n";
    print "  customization options.\n\n";
    exit;
}

my $filename = shift;
open(FILE, $filename) or die "\nCan't open file $filename: $!\n";

# HTML Header
print <<EOF;
<html>
<head>
</head>
<body>
EOF

# Parse STDIN, one line at a time
LINE: while (<FILE>)
{
    chomp;
    next LINE unless /\w/; # Don't deal with blank lines
    /^(\*+)(.*)/;
    $tll = length $1;
    if ( $tll )
    {
	$line = $2;
	print '<br>' if $body == 1;
	$body = 0;
    }
    else
    {
	print '<br>' if $body == 0;
	$body = 1;
	$line = $_;
    }

    if ( not $body )
    {
	while ( $tll < $level )
	{
	    print "</ol>\n";
	    $level--;
	}
	while ( $tll > $level )
	{
	    $level++;
	    print '<ol type=', $numerals[$level], ">\n";
	}
	print '<br><li>';
    }

    my $fmt_lvl = 0;
    $fmt_lvl = $level unless $body;
    print $format_open[$fmt_lvl], $line, $format_close[$fmt_lvl], "\n";
}

# HTML Foot
print <<EOF;

<p></p>

</body>
</html>
EOF
