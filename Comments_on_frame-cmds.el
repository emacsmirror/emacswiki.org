Why was the requirement reverted? Is it not better for a package to explicitly state their requirements?

-- PuercoPop 2013-07-21 04:31 UTC


----

1. The version numbers of the referenced libraries are not significant in this case.  In some other cases of my libraries the version numbers are significant.

2. `Package-Requires' is not required for/by [[MELPA]], AFAIK.

3. You can install or upgrade any dependencies manually.

4. The file header includes a list of features that might be useful (see ##Features that might be required by this library##).  That list includes the two libraries you wanted to add to `Package-Requires' as well as four others.

5. The libraries you wanted to add to `Package-Requires' are not, in fact, both required.  The only hard `require' is for <tt>[[frame-fns.el]]</tt>.  Library <tt>[[misc-fns.el]]</tt> is not required.

6. The code itself makes clear which library is required (<tt>[[frame-fns.el]]</tt>) and which libraries are soft-required (used if available, but not required): <tt>[[misc-fns.el]]</tt> and <tt>[[strings.el]]</tt>.

7. Please always suggest changes to a library to the author instead of changing the code directly on the wiki.  You can email me with any suggestion or question.

Thx.

-- DrewAdams 2013-07-21 06:27 UTC

