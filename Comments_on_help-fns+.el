Add your comment here.
Hi, it after installing from melpa help-fns+ I got the warning that interactive-p is an obsolete function that one should use called-interactively-p instead. Do you mind if code?

-- PuercoPop 2013-11-17 18:44 UTC


----

You can ignore all such compiler warnings.  The byte-compiler assumes that the code being compiled is written only for the same Emacs version as the byte-compiler. For many third-party libraries the code is designed to work with multiple Emacs versions. And that is the case here.

-- DrewAdams 2013-11-17 19:16 UTC


----

Hi, It seems to break the <code>?</code>/<code>RET</code> key function (<code>package-menu-describe-package</code>) in <code>M-x list-packages</code> .

-- JunkBlocker 2014-05-02 05:23 UTC


----

Should be OK now - please download and try the latest.  (Emacs Dev changed the development code recently to use a defstruct for the package description.)

-- DrewAdams 2014-05-02 15:12 UTC


----

Thank you, DrewAdams, it is working correctly now.

-- Anonymous 2014-05-03 03:42 UTC

