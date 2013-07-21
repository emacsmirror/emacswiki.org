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


----

Hi, I violated the social rules of the wiki and for that I apologise. Even though in retrospect it seems clear to me that it is disrespectful to change the file without contacting the author first (and against what I was told to do), I want to state that it was out an intent of helping the community by fixing menial tasks that I edited the file.
    With that said I want to say the following. Package requires is required by MELPA. You can see for yourself [https://github.com/milkypostman/melpa/issues/898#issuecomment-21196594 here]. I run into the missing frame-fns issue after installing frame-cmds from melpa. It would help more people in the future if the depencies where automatically installed. Would you please add back the frame-fns requirement?
    Once again sorry for my disruptive behaviour, I will be more careful on the future.

-- PuercoPop 2013-07-21 07:59 UTC


----

I added back a ##Package-Requires##, for the time being.

But there should be no need for it, I think.  It is especially not good to have to specify a specific version for the required library, since the `require' is not version-dependent and the version number is insignificant in this case anyway.  MELPA is being overly precise, where that precision is meaningless and misleading.  Precision without accuracy is wrong.  The Lisp `require' specifies the proper dependency: feature `frame-fns' is required; nothing more, no specific version.

It's also not clear to me what you mean by "required by MELPA".  Does it mean you cannot download the file from MELPA without it?  I think not.  It might mean that ##package.el## won't "install" it for you automatically.  If that's the only problem then I don't see it as a hindrance.

Better that users should just
put the libraries in their `load-path' than that the file should have to advertise a phony version dependency.  The library is for people, not just to satisfy some tools that might be overly rigid.  If an Emacs user cannot download a Lisp file and put it in `load-path' then s?he probably should not be using the file.  Seriously.

Don't get me wrong.  I do not want to put extra obstacles in a user's way.  But I'm not crazy about specifying version info that is meaningless just because some tool doesn't know any better.

-- DrewAdams 2013-07-21 09:14 UTC

