I tried emailing the author of findr.el to suggest a feature: "When searching through files, skip binaries." But the email bounced, as cadet@bu.edu is no longer valid. What does EmacsWiki typically do in such cases?

-- [http://www.yellosoft.us/ mcandre] 2014-08-13 19:03 UTC


----

I notice some problems with findr’s default replacement text (“”).  First, if I accept it, I get an error (see below).  Secondly, once you’ve replaced it with something non-empty, you can never specify an empty replacement.  The workaround is to use \9 as the replacement, assuming you don’t have 9 match groups in your regexp.

{{{
Debugger entered--Lisp error: (wrong-type-argument stringp nil)
  replace-match(nil t nil #(".lineLimit(nil)\n" 0 1 (fontified t) 1 10 (fontified t face swift-mode:function-call-face) 10 11 (fontified t) 11 14 (fontified t face swift-mode:constant-keyword-face) 14 15 (fontified t) 15 16 (fontified t)) nil)
  match-substitute-replacement(nil t nil)
  perform-replace("\\(^ *\\)?\\.lineLimit(nil)\n?" nil t t nil nil (keymap (78 . exit-current) (89 . automatic-all) keymap (escape . exit-prefix) (M-prior . scroll-other-window-down) (M-next . scroll-other-window) (prior . scroll-down) (next . scroll-up) (27 keymap (33554454 . scroll-other-window-down) (22 . scroll-other-window) (118 . scroll-down)) (22 . scroll-up) (29 . quit) (7 . quit) (63 . help) (help . help) (f1 . help) (8 . help) (85 . undo-all) (117 . undo) (94 . backup) (33 . automatic) (12 . recenter) (23 . delete-and-edit) (18 . edit) (46 . act-and-exit) (return . exit) (13 . exit) (113 . exit) (44 . act-and-show) (69 . edit-replacement) (101 . edit-replacement) (78 . skip) (89 . act) (110 . skip) (121 . act) (backspace . skip) (delete . skip) (127 . skip) (32 . act)))
  eval((perform-replace (quote "\\(^ *\\)?\\.lineLimit(nil)\n?") (quote nil) t t (quote nil) nil multi-query-replace-map))
  tags-loop-eval((perform-replace (quote "\\(^ *\\)?\\.lineLimit(nil)\n?") (quote nil) t t (quote nil) nil multi-query-replace-map))
  tags-loop-continue((findr name dir))
  tags-query-replace("\\(^ *\\)?\\.lineLimit(nil)\n?" nil nil (findr name dir))
  findr-query-replace("\\(^ *\\)?\\.lineLimit(nil)\n?" nil ".*\\.swift.*" "/Users/dave/src/a/")
  funcall-interactively(findr-query-replace "\\(^ *\\)?\\.lineLimit(nil)\n?" nil ".*\\.swift.*" "/Users/dave/src/a/")
  call-interactively(findr-query-replace record nil)
  command-execute(findr-query-replace record)
  execute-extended-command(nil "findr-query-replace" nil)
  funcall-interactively(execute-extended-command nil "findr-query-replace" nil)
  call-interactively(execute-extended-command nil nil)
  command-execute(execute-extended-command)
}}}

-- DaveAbrahams 2019-07-11 13:53 UTC

