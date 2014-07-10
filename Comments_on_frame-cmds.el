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


----

I meant that it won't download frame-fns when I install frame-cmds. You are right it appears you can not put a package as a dependency requirement without an specific version. I don't see it as a mayor hindrance but as small one that could be automated away. Although I agree that being forced to put a version sucks. It could easily default version "0" if there was no version, after all any version is higher than 0.

-- PuercoPop 2013-07-21 10:44 UTC


----

No, it should not default ''anything'' about the version.  If a version is not specified then any version at all should suffice.  If none is specified and there are multiple versions available in the MELPA repository then any of them could be downloaded, i.e., no guarantee of which you get.  And in principle, if no specific version is specified as required then any version should be OK.

But I'm glad you clarified that `Package-Requires' is NOT at all required by MELPA, just as I thought.  ''Just download the separate libraries you need -- end of story.''

The reduction in your convenience by the source-code header not including `Package-Requires' in a case like this is offset by the increase in clarity and accuracy: ##frame-cmds.el## should NOT be specifying that it needs some particular version of ##frame-fns.el##, because it does not.

What happens if I change the version number of ##frame-fns.el## but forget to also update the `Package-Requires' of ##frame-cmds.el##?  Your nifty automatic download is then broken.  Such an oversight is easy to make -- `Package-Requires' is not used by Lisp and is irrelevant to someone who has the necessary libraries in `load-path', as s?he should.  

Using `Package-Requires' to specify dependencies, redundantly and ''inaccurately''  (because overly specific) is just wrong.  The Lisp '''''code''''' deals with library dependencies, using software mechanisms that are rigorous (`require', `load-library', `autoload').  Automating package handling based on header comments is a less robust approach generally, and in the case of `Package-Requires', which mandates a version number, it is just plain ''wrong''.


Users of Lisp libraries need to learn a minimum of information about `load-path', library dependencies (e.g. `require'), etc.  And in the case of my libraries they are additionally helped by the header section '''##Features that might be required by this library##'''.  That should be more than enough.

For complex, multi-file libraries such as '''[[Icicles]]''' and '''[[Bookmark+]]''' I try to ensure that ##package.el## and [[MELPA]] do the right thing, by providing proper `autoload' cookies or sexps.  But for a simple file like <tt>[[frame-cmds.el]]</tt> I think users should be able to take care of downloading and "installing" what's needed.

-- DrewAdams 2013-07-21 17:58 UTC


----

By default I meant the author should be able to not specify a version. The tools should accommodate to the programmer not the other way around. Although it is unfortunate that most of the time is the other way around. If you don't update the version file it doesn't break, because the version is meant as a minimum required version. I agree with the principle and that ##package.el## has a design flaw. Btw, I have been negligent in thanking you for making my Emacs experience a little bit better. Thank you for writing ##frame-cmds.el##.

-- PuercoPop 2013-07-22 19:20 UTC


----

Thanks to you to for bringing this to my attention. I've been in contact with the folks at MELPA about it.

-- DrewAdams 2013-07-23 00:15 UTC


----

Old discussion, I know...

; Fact #1 : You object about MELPA requiring devs to specify dependency versions. TBH, I agree to your abjection.
; Fact #2 : You *are* actually publishing ##frame-cmds## on MELPA.
; Fact #3 : ##frame-cmds##' dependency on ##frame-fns## is not advertised anywhere outside of its source code. Not even in MELPA's long package description!
; Fact #4 : Installation of ##frame-cmds## through MELPA *just plain fails*.

Given above facts I can only see 2 logical options:
; You keep to your principles : Then you remove ##frame-cmds## from MELPA and you happily move on. People'll find ##frame-cmds.el## at EmacsWiki, read source, and install it without any further fuss.
; You resolve to keep using MELPA which, some day, will maybe be perfect : Then you clench your teeth and someway arrange for the intended MELPA's installation workflow to actually work with your package. You could even hack a version 0 requirement as suggested; at least until your pestering the folks at MELPA accomplishes something better ;-)

Anything else (like keeping a package which cannot be installed without code reading and manual dependency installation in the repository) is just messing around and putting a spoke on MELPA's wheel. Anyway, let me ask a few, rhetorical, questions in order to make my point clear

What'd be the point in using any package manager if users have to read sources in order to successfully install a package?
That being the case, wouldn't it be more straightforward to just manually setup the whole thing?
Does the following look like a sane workflow for ##frame-cmds## installation?
* M-x package-list
* Navigate to ##frame-cmds##, then i, x
* Check compilation messages to see why compilation failed
* Back to package list, RET, read description --> nothing mentioned...
* d, x
* Navigate to ##frame-fns##, then i, x
* Navigate to ##frame-cmds##, then i, x
* [shouting] wohooo!! now onto some real work...

Well, that's actually what I went through just an hour ago, thanks very much!

-- DavidRequena 2014-07-10 12:36 UTC

