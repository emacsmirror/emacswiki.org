Add your comment below, at the end.

[new]
Hi, it after installing from melpa help-fns+ I got the warning that interactive-p is an obsolete function that one should use called-interactively-p instead. Do you mind if code? -- PuercoPop 2013-11-17 18:44 UTC


----

You can ignore all such compiler warnings.  The byte-compiler assumes that the code being compiled is written only for the same Emacs version as the byte-compiler. For many third-party libraries the code is designed to work with multiple Emacs versions. And that is the case here.
-- DrewAdams 2013-11-17 19:16 UTC


----

[new]

Hi, It seems to break the <code>?</code>/<code>RET</code> key function (<code>package-menu-describe-package</code>) in <code>M-x list-packages</code>. -- JunkBlocker 2014-05-02 05:23 UTC


----

Should be OK now - please download and try the latest.  (Emacs Dev changed the development code recently to use a defstruct for the package description.) -- DrewAdams 2014-05-02 15:12 UTC


----

Thank you, DrewAdams, it is working correctly now.
-- Anonymous 2014-05-03 03:42 UTC

----

You're welcome.  Thanks for the bug report. -- DrewAdams


[new]

----

Hi, I installed hep-fns+ with el-get, but I can't find the functions with M-x I have to explicitly add (require 'help-fns+) in my init file. Is this an issue with el-get or help-fns+? 
Thank you!
Here's my setting in the init file for el-get if it helps.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
(with-current-buffer
(url-retrieve-synchronously
"https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
(goto-char (point-max))
(eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

-- Anonymous 2014-10-09 17:42 UTC


----

I replied to your posting of the same question [[http://stackoverflow.com/a/26290889/729907 here]].

I'm no expert on el-get; sorry. Perhaps someone else can help with that. I would say just put <tt>[[help-fns+.el]]</tt> in your `load-path' and `require' it - done. - DrewAdams

-- DrewAdams 2014-10-10 02:57 UTC


----

Hello Drew. Are commands `describe-buffer' and `describe-keymap' missing autoload cookie on purpose?
I'm also starting a new emacs init based on el-get and this seems to be the root of the problem that person above me encountered.

-- myeffort 2014-11-05 04:09 UTC

