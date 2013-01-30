Hi Rubikitch.  I don't understand --- why bother?  You can just use `C-y' to retrieve the region as the ##FROM## string (or as ##TO## string).  `C-y' is as easy as `M-n', no?

FWIW, see Lisp:replace+.el for more flexible defaulting for query-replace commands --- it might help.  If you really prefer the active region text as the default then just customize `search/replace-default-fn' to a function that gives you that text.

Or better, customize it to function `region-or-non-nil-symbol-name-nearest-point', which gives you the nearest symbol name whenever the region is not active.  That function is in Lisp:thingatpt+.el.

HTH.

-- DrewAdams 2013-01-30 03:48 UTC

