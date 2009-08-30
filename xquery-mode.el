;;; xquery-mode.el --- A simple mode for editing xquery programs
;; Time-stamp: <2005-03-26 18:05:39 sacharya>

;;; Copyright (C) 2005 Suraj Acharya

;; Author: Suraj Acharya <sacharya@cs.indiana.edu>

;; This file is not part of GNU Emacs.

;; xquery-mode.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.:

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; 

(require 'nxml-mode)

;;; Code:
(define-generic-mode 'xquery-mode
  '(("(:" . ":)") ("<!--" . "-->"))
  '("xquery" "version" "encoding" "at" "module" "namespace" "child" "descendant" "parent" "attribute" "self" "descendant-or-self" "ancestor" "following-sibling" "preceding-sibling" "following" "preceding" "ancestor-or-self" "declare" "function" "option" "ordering" "ordered" "unordered" "default" "order" "external" "or" "and" "div" "idiv" "mod" "in"  "construction" "satisfies" "return" "then" "else" "boundary-space" "base-uri" "preserve" "strip" "copy-namespaces" "no-preserve" "inherit" "no-inherit" "to" "where" "collation" "intersect" "union" "except" "as" "case" "instance" "of" "castable" "item" "element" "schema-element" "schema-attribute" "processing-instruction" "comment" "text" "empty" "import" "schema" "is" "eq" "ne" "gt" "ge" "lt" "le" "some" "every" "for" "let" "cast" "treat" "validate" "document-node" "document" "node" "if" "typeswitch" "by" "stable" "ascending" "descending" "greatest" "least" "variable") ;keywords
  '(("\\(\\$\\w+\\)" 1 font-lock-variable-name-face) ;; \\(\\s_\\|\\w\\)
    ("\\(\\w*:?\\w+\\)\\s *(" 1 font-lock-function-name-face)
    ("\\(<\\)\\(/?\\)\\(\\w*\\)\\(:?\\)\\(\\w+\\).*?\\(/?\\)\\(>\\)" 
     (1 'nxml-tag-delimiter-face) 
     (2 'nxml-tag-slash-face)
     (3 'nxml-element-prefix-face) 
     (4 'nxml-element-colon-face)
     (5 'nxml-element-local-name-face)
     (6 'nxml-tag-slash-face)
     (7 'nxml-tag-delimiter-face) 
     )
    ("\\(\\w*\\)\\(:?\\)\\(\\w+\\)=\\([\"']\\)\\(.*?\\)\\([\"']\\)" 
     (1 'nxml-attribute-prefix-face) 
     (2 'nxml-attribute-colon-face)
     (3 'nxml-attribute-local-name-face) 
     (4 'nxml-attribute-value-delimiter-face)
     (5 'nxml-attribute-value-face)
     (6 'nxml-attribute-value-delimiter-face))
    ("\\(/\\)\\(\\w*\\)\\(:?\\)\\(\\w+\\)" 
     (1 font-lock-constant-face)
     (2 font-lock-constant-face) 
     (3 font-lock-constant-face)
     (4 font-lock-constant-face)
     )
    ("as\\s +\\(\\w*:?\\w+\\)" 
     (1 font-lock-type-face)
     )
    ) ;font-lock-list
  '(".xq\\'") ;auto-mode-list
  '(xquery-set-indent-function xquery-set-up-syntax-table)         ;function list
  "A Major mode for editing xquery."
  )



(defun xquery-set-indent-function ()
  "Set the indent function for xquery mode."
  (setq nxml-prolog-end (point-min))
  (setq nxml-scan-end (copy-marker (point-min) nil))
  (set (make-local-variable 'indent-line-function) 'xquery-indent-line)
  (make-local-variable 'forward-sexp-function)
  (setq forward-sexp-function 'xquery-forward-sexp)
  (local-set-key "/" 'nxml-electric-slash)
  )

(defun xquery-forward-sexp (&optional arg)
  "Xquery forward s-expresssion.
This function is not very smart, it tries to use
`nxml-forward-balanced-item' if it sees '>' or '<' characters in
the direction you are going, and uses the regular `forward-sexp'
otherwise. "
  (if (> arg 0)
      (progn                                 
        (if (looking-at "[ \t]*<")
            (nxml-forward-balanced-item arg)
          (let ((forward-sexp-function nil)) (forward-sexp arg))))
    (if (looking-back ">[ \t]*")
        (nxml-forward-balanced-item arg)
                (let ((forward-sexp-function nil)) (forward-sexp arg))))
  )


(defun xquery-set-up-syntax-table ()
  "Allow the hypen character to be recognized as part of a xquery symbol."
  (modify-syntax-entry ?- "w" (syntax-table))
  (modify-syntax-entry ?/ "." (syntax-table))
  ;; set-up the syntax table correctly for parentheis type characters
  (modify-syntax-entry ?\{ "(}" (syntax-table))
  (modify-syntax-entry ?\} "){" (syntax-table))
  (modify-syntax-entry ?\[ "(]" (syntax-table))
  (modify-syntax-entry ?\] ")]" (syntax-table))
  (modify-syntax-entry ?\< "(>1" (syntax-table))
  (modify-syntax-entry ?\> ")<4" (syntax-table))
  ;; xquery comments are like (: :)
  (modify-syntax-entry ?\( "()1" (syntax-table)) 
  (modify-syntax-entry ?\) ")(4" (syntax-table))
;;   (modify-syntax-entry ?\: ".23" (syntax-table))
  )



(defun xquery-indent-line ()
  "Indent current line as xquery code."
  (interactive)
   (let ((savep (> (current-column) (current-indentation)))
	 (indent (condition-case err (max (xquery-calculate-indentation) 0)
		   (error (message "%S" err)))))
     (if savep
	 (save-excursion (indent-line-to indent))
       (indent-line-to indent))))

(defvar xquery-start-block-regexp "[ \t]*\\((\|{\\|for\\|let\\|where\\|return\\|if\\|else\\|typeswitch\\|declare[ \t]+function\\|.*[({]$\\)"
  "A regular expression which indicates that a xquery block is starting.")

(defvar xquery-flwr-block-regexp "[ \t]*\\(for\\|let\\|where\\|return\\|order\\|stable\\s *order\\)")

(defvar xquery-indent-size 2
  "The size of each indent level.")

(defvar xquery-indent-debug nil)

(defun xquery-toggle-debug-indent ()
  "Toggle the debug flag used in `xquery-calculate-indentation'. "
  (interactive)
  (setq xquery-indent-debug (not xquery-indent-debug))
  (message (concat "xquery-indent-debug is " (if xquery-indent-debug "en" "dis") "abled"))
  )

(defun xquery-calculate-indentation ()
   "Return the column to which the current line should be indented."
  (beginning-of-line)
  (if (bobp)
      0 ; First line is always non-indented
    (skip-chars-forward " \t")
    (cond
     ;; do nothing if this is a comment
     ((eq (get-text-property (point) 'face) 'font-lock-comment-face) (current-indentation))

     ((looking-at "\\(</?\\w\\|{\\)")  ;; xml constructor or enclosed expressions
      (if xquery-indent-debug
          (message "xquery-indent-debug: xml constructor"))
      (let ((nxml-prolog-end (point-min))
            (nxml-scan-end (copy-marker (point-min) nil)))
        (nxml-compute-indent)
        ))

     ;; for close braces or else statements indent to the same level as the opening { 
     ((looking-at "}")
      (if xquery-indent-debug
          (message "xquery-indent-debug: }"))
      (save-excursion
        (backward-up-list)
        (let ((cc (current-column)))
          (beginning-of-line)
          (if (looking-at xquery-start-block-regexp)
              (current-indentation)
            cc))))

     ((looking-at "else")
      (if xquery-indent-debug
          (message "xquery-indent-debug: else"))
      (save-excursion
        (xquery-previous-non-empty-line)
        (- (current-indentation) xquery-indent-size)
        ))

     ;; for close parens, indent to the start of the func call
     ((looking-at ")")
      (if xquery-indent-debug
          (message "xquery-indent-debug: )"))
      (save-excursion
        (backward-up-list) 
        (if (looking-back "\\w+\\s *")
            (backward-word))
        (current-column)
     ))

     ;; order flwr expressions on the same column
     ((save-excursion
        (when
            (and
             (looking-at xquery-flwr-block-regexp)
             (progn 
               (xquery-previous-non-empty-line)
               (beginning-of-line)
               (looking-at xquery-flwr-block-regexp)))
      (if xquery-indent-debug
          (message "xquery-indent-debug: nested flwr"))
          (current-indentation)
            )
        ))

     ;; if this is the first non-empty line after a block, indent xquery-indent-size chars relative to the block
     ((save-excursion
        (xquery-previous-non-empty-line)
        (beginning-of-line)
        (when (looking-at xquery-start-block-regexp)
          (if xquery-indent-debug
              (message "xquery-indent-debug: first line in block"))
          (+ xquery-indent-size (current-indentation))))
      )

     ;; for everything else indent relative to the outer list
     (t       
      (if xquery-indent-debug
          (message "xquery-indent-debug: everyting else"))
      (save-excursion (xquery-previous-non-empty-line) (current-indentation)))
     )))


(defun xquery-previous-non-empty-line ()
  "Move to the last non-empty line."
  (re-search-backward "\\S " (point-min) t)
  )

(provide 'xquery-mode)

;;; xquery-mode.el ends here
