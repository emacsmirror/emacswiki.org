Hi Adam,
First thanks for your great work on Bookmark+.
Would you mind using let instead of lexical-let ?
It's obsolete since emacs 24.1 and was removed on emacs 29.1.

-- Anonymous 2023-10-03 17:13 UTC


----

@Anonymous:

Thanks for the report. Sorry, but I don't have the time right now to work on that -- it's on my list. 

There are about 50 uses of `lexical-let[*]' in the Bookmark+ code, which is intentionally compatible with Emacs releases prior to the introduction of lexical scoping and variable `lexical-binding' into Emacs (Bookmark+ is compatible back through Emacs 20).  And as the comment in (Emacs 29) `cl.el' says, that's not 100% compatible with `lexical-let' anyway.

It'll require some time to fiddle with the existing code, to get everything right, conditionally.

As a workaround for now, I guess you'll need to load `cl.el' (at least at byte-compile time) before Bookmark+, to pick up the macros `lexical-let' and `lexical-let*'.

Sorry for the trouble. (Let me know if you have trouble with the workaround of loading `cl.el'.)

 -- Drew

-- DrewAdams 2023-10-04 22:22 UTC

