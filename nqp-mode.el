;;; nqp-mode.el --- NQP code editing commands for GNU Emacs

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: Duzy Chan
;; Email: duzy@duzy.info
;; Keywords: languages, NQP

;; Adapted from C code editing commands 'c-mode.el', Copyright 1987 by the
;; Free Software Foundation, under terms of its General Public License.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

(defvar nqp-mode-hook nil
  "Hook used in NQP mode.")

(defvar nqp-mode-map
  (let ((m (make-keymap))) ; or make-sparse-keymap
    (define-key m "\C-j" 'newline-and-indent)
    ;(define-key m "\M-j" 'newline)
    m)
  "Keymap used in NQP mode.")

(defconst nqp-font-lock-keywords-1
  (list
   ;;
   ;; Fontify keywords, except those fontified otherwise.
   (concat "\\<"
           (regexp-opt '("if" "until" "while" "elsif" "else" "unless" "do"
                         "for" "proto" "try"
                         )
                       t)
           "\\>")

   ;;
   ;; Fontify local and my keywords as types.
   '("\\<\\(local\\|my\\|our\\)\\>" (1 font-lock-builtin-face))

   '("\\<\\(make\\|return\\|self\\)\\>" (1 font-lock-builtin-face))

   ;;
   ;; Fontify function and package names in declarations.
   ;'("\\<\\(package\\|class\\|grammar\\|sub\\|method\\|token\\|rule\\|is\\|has\\)\\>[ \t]*\\(\\sw+\\)?"
   '("\\<\\(package\\|class\\|grammar\\|sub\\|method\\|token\\|rule\\|is\\|has\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))

   ;;
   ;; Fontify function, variable and file name references.
   '("&\\(\\sw+\\(::\\sw+\\)*\\)" 1 font-lock-function-name-face)

   ;;
   ;; Additionally underline non-scalar variables.  Maybe this is a bad idea.
   '("\\([$*@%][!{]?\\)\\(\\sw+\\(::\\sw+\\)*\\)"
     (1 font-lock-type-face)
     (2 font-lock-variable-name-face))
   '("\\([$*@%]\\)\\([/_]\\)"
     (1 font-lock-type-face)
     (2 font-lock-variable-name-face))
   '("\\([$*@%]?\\)\\(<\\)\\([!?-.]?\\)\\([^>=\n]+\\)=?\\([^>\n]*\\)\\(>\\)"
     (1 font-lock-type-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-builtin-face)
     (4 font-lock-constant-face)
     (5 font-lock-preprocessor-face)
     (6 font-lock-variable-name-face))

   '("[^:]\\(:\\)\\(\\sw+\\)"
     (1 font-lock-builtin-face)
     (2 font-lock-builtin-face))

   ;;
   ;; Fontify keywords with/and labels as we do in `c++-font-lock-keywords'.
   ;'("^[ \t]*\\(\\sw+\\)[ \t]*\\(:\\)[^:]"
   '("^[ \t]*\\(\\sw+\\)[ \t]*\\(:\\)[ \t]*\n"
     (1 font-lock-constant-face)
     (2 font-lock-builtin-face))
   '("\\<\\(continue\\|goto\\|last\\|next\\|redo\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))

   '("\\<\\(import\\|no\\|require\\|use\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))

   '("\\<\\(INIT\\)\\>[ \t\n]*{"
     (1 font-lock-builtin-face))
   )
  "NQP mode keyword set 1.")

(defconst nqp-font-lock-keywords nqp-font-lock-keywords-1
  "Default highlight in NQP mode.")

(defvar nqp-font-lock-syntactic-keywords
  `(;;
    ;; Turn POD into b-style comments
    ("^\\(=\\)\\sw" (1 "< b"))
    ("^=cut[ \t]*\\(\n\\)" (1 "> b"))

    ;;
    ;; Catch ${ so that ${var} doesn't screw up indentation.
    ;; This also catches $' to handle 'foo$', although it should really
    ;; check that it occurs inside a '..' string.
    ("\\(\\$\\)[{']" (1 ". p"))

    ;("<[^>]+>" (1 ". p"))

    ;;
    ;; Handle funny names like $DB'stop.
    ("\\$ ?{?^?[_a-zA-Z][_a-zA-Z0-9]*\\('\\)[_a-zA-Z]" (1 "_"))
    ))

(defvar nqp-mode-syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))
    ;; `$' is also a prefix char so I was tempted to say "/ p",
    ;; but the `p' thingy basically overrides the `/' :-(   --stef
    (modify-syntax-entry ?$ "/" st)
    (modify-syntax-entry ?% ". p" st)
    (modify-syntax-entry ?@ ". p" st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?/ "." st)
    ;(modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?< "(" st)
    (modify-syntax-entry ?= "." st)
    ;(modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?> ")" st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?` "\"" st)
    (modify-syntax-entry ?| "." st)
    ;(modify-syntax-entry ?[ "." st)
    ;(modify-syntax-entry ?] "." st)
    (modify-syntax-entry ?[ "(" st)
    (modify-syntax-entry ?] ")" st)
    (modify-syntax-entry ?{ "(" st)
    (modify-syntax-entry ?} ")" st)
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table in use in `nqp-mode' buffers.")



(defun nqp-indent-line (&optional nochange parse-start)
  "Indent current line as NQP mode."
  (interactive)
  (if (bobp) ; beginning-of-buffer ?
      (indent-line-to 0) ; or (beginning-of-line)
    (let ((indent 0)
          (scope-indent 0)
          )
      ;; (setq indent (save-excursion
      ;;                (let ((f (nqp-beginning-of-function)))
      ;;                  (beginning-of-line)
      ;;                  (- f (point)))))
      (setq scope-indent (nqp-calculate-scope-indent))
      (setq indent (+ scope-indent indent))
      (message (format "scope-indent: %d, indent: %d" scope-indent indent))
      (indent-line-to indent) ; this will removes or adds spaces and tabs
      )))

(defun nqp-calculate-scope-indent ()
  "Calculate the level of scope in NQP mode."
  (let ((scope-level 0)
        (stop (point))
        scope)
    (save-excursion
      (while (not (bobp))
        (if (re-search-backward "{" nil t 1)
            (progn
              (setq scope (match-beginning 0))
              (if (not (or (nqp-point-in-comment-p)
                           (save-excursion (re-search-forward "}" stop t))))
                  (progn (setq stop (1- scope))
                         (setq scope-level (1+ scope-level)))))
          (beginning-of-buffer))))
    (* 4 scope-level) ; (* default-tab-width scope-level)
    ))

(defun nqp-point-in-comment-p ()
  "Return t if the point is in a comment block in NQP mode."
  nil)

(defun nqp-beginning-of-function (&optional arg)
  "Move backword to next beginning-of-function, or as far as possible.
With argument, repeat that many times; negative args move forward.
Returns new value of point in all cases."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (forward-char 1))
  (and ;(/= arg 0)
       (re-search-backward
        (concat "^\\s(\\|^\\s-*"
                ;"proto?\\s-*"
                (regexp-opt '("sub" "method" "token") t)
                "\\b[ \t\n]*\\_<[^{]+{\\|^\\s-*format\\b[^=]*=\\|^\\.")
        nil 'move arg)
       (goto-char (1- (match-end 0))))
  (point))


(defun nqp-mode ()
  "NQP mode."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "NQP")
  (setq major-mode 'nqp-mode)
  (use-local-map nqp-mode-map)
  (set-syntax-table nqp-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(nqp-font-lock-keywords ; KEYWORDS
         nil ; KEYWORDS-ONLY
         nil ; CASE-FOLD
         ((?\_ . "w") (?\' . "\"") (?\\ . "\\"))
         nil ; SYNTAX-BEGIN ...
         (font-lock-syntactic-keywords . nqp-font-lock-syntactic-keywords)
         ;(parse-sexp-lookup-properties . t)
         ))
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  ;(set (make-local-variable 'comment-start-skip) "\\(^\\|\\s-\\);?}?#+ *")
  (set (make-local-variable 'comment-start-skip) "\\(^\\|\\s-\\);?}?>?]?#+ *")
  (set (make-local-variable 'indent-line-function) 'nqp-indent-line)
  (run-hooks 'nqp-mode-hook) ; or run-mode-hooks
  )

(setq auto-mode-alist (append (list (cons "\\.\\(nqp\\|pm\\)\\'" 'nqp-mode))
                              auto-mode-alist))

(provide 'nqp-mode)
