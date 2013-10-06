
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

