;;; bc-mode.el --- BC code editing commands for Emacs
;; Copyright (C) 2005  Kumar Appaiah

;; Author: Kumar Appaiah <akumar_NOSPAM@ee.iitm.ac.in>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Sets up C-mode with support for BC. Any improvements needed?
;;
;; For autoload, ass the following lines to your .emacs:
;; (autoload 'bc-mode "bc-mode.el" "bc-mode" t 'nil)
;; (add-to-list 'auto-mode-alist '(".bc\\'" . bc-mode))
;; (add-to-list 'interpreter-mode-alist '("bc" . bc-mode))
;;
;; Kumar Appaiah akumar_NOSPAM@ee.iitm.ac.in
;; Date: 27th June, 2005

;;; Code:

(require 'cc-mode)

(defcustom bc-mode-hook nil
  "Normal hook run when entering bc-mode."
  :type 'hook
  :group 'data)

(defvar bc-command-line "bc -l"
  "Command line executed for `bc'.")

;; Regexps
(defconst bc-font-lock-keywords
  (eval-when-compile
    (list
     '("\\(#.*\\)$"
       (1 font-lock-comment-face))
     ;;
     ;; Function names.
     '("^[ \t]*\\(define\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     ;;
     ;; Variable names.
     (cons (regexp-opt
	    '("SCALE" "IBASE" "OBASE" "LAST") 'words)
	   'font-lock-variable-name-face)
     ;;
     ;; Keywords.
     (regexp-opt
      '("break" "continue" "delete" "exit" "else" "for" "quit" "auto" "local"
	"if" "print" "return" "while") 'words)
     ;;
     ;; Builtins.
     (list (regexp-opt
	    '("length" "read" "scale" "sqrt") 'words)
	   1 'font-lock-builtin-face)
     ;;
     ;; Operators.  Is this too much?
     (cons (regexp-opt '("&&" "||" "<=" "<" ">=" ">" "==" "!="))
	   'font-lock-constant-face)
     ))
 "Default expressions to highlight in BC mode.")

;;;###autoload
(define-derived-mode bc-mode c-mode "BC"
  "Major mode for editing BC code.
This is much like C mode except for the syntax of comments.  Its keymap
inherits from C mode's and it has the same variables for customizing
indentation.  It uses `c-mode-syntax-table'.

Turning on BC mode runs `bc-mode-hook'."
  (set-syntax-table c-mode-syntax-table)
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) " */")
  (set (make-local-variable 'comment-start-skip) "#+ *|/\\* +")
  (setq font-lock-defaults '(bc-font-lock-keywords nil nil ((?_ . "w"))))
  (run-hooks 'bc-mode-hook))

(provide 'bc-mode)

;;; bc-mode.el ends here
