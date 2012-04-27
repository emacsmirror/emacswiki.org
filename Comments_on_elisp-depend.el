IMO it's not a good idea to upload the file instead of pasting its text. Why? Because clicking Diff is useless in that case.  I click Diff to see what you changed (even just to see if the change is spam), and all I see is the file name as the diff content.

-- DrewAdams 2012-04-27 06:27 UTC


----

The header says the file is compatible with GnuEmacs versions 20-23, but it is no longer compatible with GnuEmacs 20 or 21 because you use functions `propertize' and `delete-dups'.  You can easily forego using these two functions and make it once again compatible with the older versions.

-- DrewAdams 2012-04-27 06:34 UTC

