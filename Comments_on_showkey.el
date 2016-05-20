Compile on current stable (24.5) results in message below.

showkey.el:289:19:Warning: `special-display-frame-alist' is an obsolete
    variable (as of 24.3); use `display-buffer-alist' instead.

-- Anonymous 2016-05-20 15:36 UTC


----

Pay no attention to such warnings.

1. It is a warning, not an error.
2. This variable should not, IMHO, be deprecated.
3. Although deprecated, it is still supported. And it works fine.

Even when such a warning is appropriate (unlike this case, IMO), it can typically be ignored for any good library.  Most often such a warning is seen for a library that needs to support multiple versions of Emacs, and the library code correctly distinguishes which code paths to use for which versions.

-- DrewAdams 2016-05-20 20:51 UTC

