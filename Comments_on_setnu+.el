I'm afraid (and sorry!) there's a terrible bug with this.

I often use a shell script, which is a filter, to filter parts
of my text through.  This filter I called notrail, which
strips off trailing whitspace from lines.

When I have line numbers mode on, and run notrail on
a selected part of a buffer, the line numbers get deleted
also.  Looks very silly and messy.

Even before this bug surfaced, thinking about line numbers
(which at present I badly need for a project I'm working on),
it occurred to me why emacs itself has never implemented
a decent line numbers option to be placed in the location
that is most 'fitting', namely the fringe!

I googled, but to my growing surprise, I didn't find anything
meaningful.

bjd

-- Bauke Jan Douma 2013-10-06 12:28 UTC


----

This is an addition to my previous comment here.
I forgot to give the (admittedly non-impressive) source
code of the notrail script:

{{{
#!/bin/bash
# remove trailing whitespace from lines
sed 's/[ 	]*$//g'
}}}

Also, there was another problem with setnu+ that surfaced
even before the notrail bug;  I often paste code example
snippets into my emacs buffer.  I was doing that with a
code example of two lines (cut from the web) which I pasted
into a buffer with setnu+ line numbers.  In this case I pasted
them at the final line (I have the option to number the last
line, after the last newline, also numbered).  What happened
was that (I have to write this from memory) somehow the two
pasted lines got pasted on one line in emacs, with the line
number that used to be the final line number in between
them.  After a quick wtf I tried to remove the line number there,
but that didn't work.

Again I'm sorry to say setnu/setnu+ doesn't sufficiently 'work'
for me, because I presently badly need the lineno's!

-- Bauke Jan Douma 2013-10-06 12:36 UTC

