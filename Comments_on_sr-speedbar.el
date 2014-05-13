Emacs 24.4.50.1, will broken with following error:
{{{
Debugger entered--Lisp error: (void-function sr-speedbar-handle-other-window-advice)
  (sr-speedbar-handle-other-window-advice value)
  (progn (sr-speedbar-handle-other-window-advice value))
  (if (get (quote other-window) (quote ad-advice-info)) (progn (sr-speedbar-handle-other-window-advice value)))
  (if (and (>= emacs-major-version 24) (>= emacs-minor-version 4)) (if (get (quote other-window) (quote ad-advice-info)) (progn (sr-speedbar-handle-other-window-advice value))) (if (ad-advised-definition-p (quote other-window)) (progn (sr-speedbar-handle-other-window-advice value))))
  (lambda (symbol value) (set symbol value) (if (and (>= emacs-major-version 24) (>= emacs-minor-version 4)) (if (get (quote other-window) (quote ad-advice-info)) (progn (sr-speedbar-handle-other-window-advice value))) (if (ad-advised-definition-p (quote other-window)) (progn (sr-speedbar-handle-other-window-advice value)))))(sr-speedbar-skip-other-window-p nil)
  custom-initialize-reset(sr-speedbar-skip-other-window-p nil)
  custom-declare-variable(sr-speedbar-skip-other-window-p nil "Whether skip `sr-speedbar' window with `other-window'.\nDefault, can use `other-window' select window in cyclic\nordering of windows.  But sometimes we don't want select\n`sr-speedbar' window use `other-window'.\nJust want make `sr-speedbar' window as a view sidebar.\n\nSo please turn on this option if you want skip\n`sr-speedbar' window with `other-window'.\n\nDefault is nil." :type boolean :set (lambda (symbol value) (set symbol value) (if (and (>= emacs-major-version 24) (>= emacs-minor-version 4)) (if (get (quote other-window) (quote ad-advice-info)) (progn (sr-speedbar-handle-other-window-advice value))) (if (ad-advised-definition-p (quote other-window)) (progn (sr-speedbar-handle-other-window-advice value))))) :group sr-speedbar)
  eval-buffer(#<buffer  *load*> nil "/Users/wzheng/Dropbox/common/.emacs.d/plugins/sr-speedbar.el" nil t)  ; Reading at buffer position 11241
  load-with-code-conversion("/Users/wzheng/Dropbox/common/.emacs.d/plugins/sr-speedbar.el" "/Users/wzheng/Dropbox/common/.emacs.d/plugins/sr-speedbar.el" nil t)
  require(sr-speedbar)
  eval((require (quote sr-speedbar)) nil)
  eval-last-sexp-1(nil)
  eval-last-sexp(nil)
  call-interactively(eval-last-sexp nil nil)
  command-execute(eval-last-sexp)
}}}

-- 匿名者 2014-05-13 13:18 UTC

