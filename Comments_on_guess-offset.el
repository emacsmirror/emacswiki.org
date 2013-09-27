I tried to use this but found it did not work well for me.

The issue seems to be that the algorithm gets confused between what is an indent and what is a multiple of an indent.

For example, I have a file with 2-space indent, that has lots of code at 3 level of indentation (6 spaces).  The debug output suggests the algorithm thinks the file may be 2 and it may be 3 with comparable probability, I suppose because 6 is a multiple of both 2 and 3.  So it thinks the file is ambiguous and leaves the offset alone.

The file is not ambiguous, it is clearly a 2 space indent.  There are no 3 space indents in the file at all, so it seems the algorithm for detection is a bit deficient.

-- Anonymous 2013-09-27 18:42 UTC

