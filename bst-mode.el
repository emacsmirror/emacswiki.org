;;; bst-mode.el --- major mode for editing BibTeX style files

;; Copyright (C) 2002-3005  Jakub Narebski

;; Authors: Jakub Narebski <jnareb@fuw.edu.pl>
;;          Nelson H. F. Beebe <beebe@math.utah.edu>
;; Maintainer: Jakub Narebski <jnareb@fuw.edu.pl>
;; Version: 0.3
;; Keywords: BibTeX, LaTeX, TeX
;; Created: 29-09-2002

;; $Id: bst-mode.el,v 1.2 2005/03/12 18:24:36 jnareb Exp jnareb $

;; This file is *NOT* part of GNU Emacs.
;; This file is distributed under the same terms as GNU Emacs.

;;{{{ GPL

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; http://www.fsf.org/copyleft/gpl.html

;;}}}


;;; Commentary:

;; Integrated with bst.el by Nelson H. F. Beebe <beebe@math.utah.edu>

;;; Installation:

;; To use this package, put the following line in your .emacs:
;;
;;    (require 'bst-mode)
;;
;; or, if you want to load this package only when necessary
;;
;;    (add-to-list 'auto-mode-alist '("\\.bst$" . bst-mode))
;;    (autoload 'bst-mode "bst-mode" "BibTeX-style major mode." t)

;;; Code:


;; Version
(defconst bst-mode-version "0.3"
  "Version of bst-mode (BibTeX style)")

;; Syntax Table and Mode Keywords
(defconst bst-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?%  "<" st)
    (modify-syntax-entry ?'  "_" st)
    (modify-syntax-entry ?$  "w" st)
    (modify-syntax-entry ?#  "_" st)
    (modify-syntax-entry ?.  "w" st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\f ">" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defconst bst-style-file-commands
  (regexp-opt
   '("ENTRY" "FUNCTION" "INTEGERS" "MACRO" "STRINGS" ; declare/define
     "READ"				             ; read
     "EXECUTE" "ITERATE" "REVERSE" "SORT")           ; output
   'words))

(defconst bst-built-in-operators
  (regexp-opt
   '("<" ">" "=" "+" "-" "*" ":=")
   'words))

(defconst bst-built-in-functions
  (regexp-opt
   '("add.period$" "call.type$" "change.case$" "chr.to.int$" "cite$"
     "duplicate$" "empty$" "format.name$" "if$" "int.to.chr$" "int.to.str$"
     "missing$" "newline$" "num.names$" "pop$" "preamble$" "purify$"
     "quote$" "skip$" "stack$" "substring$" "swap$" 
     "text.length$" "text.prefix$" "top$" "type$" "warning$" "while$"
     "width$" "write$")
   'words))

(defconst bst-number
  "#[-+]?[0-9]+")

(defconst bst-font-lock-keywords
  (list bst-style-file-commands		                       ; REGEXP
	(cons bst-built-in-functions 'font-lock-builtin-face)  ; (MATCHER . FACENAME)
	(cons bst-number             'font-lock-constant-face) ; (MATCHER . FACENAME)
	(list (concat "\\<FUNCTION\\>" " *" "{ *\\([a-zA-Z.$]+\\) *}")
	      1 'font-lock-function-name-face)                 ; (MATCHER. HIGHLIGHTER)
;	(list "\\('\\)[a-zA-Z.$]*"
;	      1 'font-lock-warning-face)                       ; (MATCHER. HIGHLIGHTER)
	))

(defvar bst-comment-prefix "%%: "
  "*Comment prefix string for bst-comment and bst-uncomment.  This is
used in regular-expression matching by bst-uncomment, so it should
NOT contain any regular-expression pattern characters like . or *.")

;;;
(defun bst-mode ()
  "Major mode for editing BibTeX style files."
  (interactive)
  (kill-all-local-variables)

  ;; Define major mode
  (setq major-mode 'bst-mode)
  (setq mode-name "BibTeX-style")

  ;; Tabs-mode and Auto-filling
  (setq indent-tabs-mode nil)
  (auto-fill-mode nil)

  ;; Local commands
  (local-set-key [tab]    'indent-for-tab-command)
  (local-set-key "\C-c :" 'bst-uncomment-region)
  (local-set-key "\C-c ;" 'bst-comment-region)

  ;; Syntax information, Syntax table and Font lock support
  (setq comment-end "")
  (setq comment-start "% ")
  (setq comment-start-skip "%+ *")

  (set-syntax-table bst-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
      `(bst-font-lock-keywords		; KEYWORDS
	nil				; KEYWORDS-ONLY
	t				; CASE-FOLD
	nil				; SYNTAX-ALIST
	nil)) 				; SYNTAX-BEGIN

  ;; Finally, run the user mode hooks
  (run-hooks 'bst-mode-hook 'BibTeX-style-mode-hook 'bibtex-style-mode-hook)
  )

;; Aliases for `bst-mode'
(fset 'BibTeX-style-mode 'bst-mode)
(fset 'bibtex-style-mode 'bst-mode)

;;; Local functions
(defun bst-comment-region ()
  "Insert a distinctive comment prefix at the start of each line in
the current region."
  (interactive)
  (let ((start (region-beginning)) (end (region-end)))
    (goto-char start)
    (if (bolp)
	t
      (forward-line 1)
      (setq start (point)))
    (goto-char end)
    (if (bolp)
	t
      (beginning-of-line)
      (setq end (1+ (point))))
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (and (bolp) (< (point) (point-max)))
	(insert bst-comment-prefix)
	(forward-line 1)))))


(defun bst-uncomment-region ()
  "Remove a distinctive comment prefix at the start of each line in
the current region."
  (interactive)
  (let ((start (region-beginning)) (end (region-end)) (n (length bst-comment-prefix)))
    (goto-char start)
    (if (bolp)
	t
      (forward-line 1)
      (setq start (point)))
    (goto-char end)
    (if (bolp)
	t
      (beginning-of-line)
      (setq end (1+ (point))))
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (and (bolp) (< (point) (point-max)))
	(if (looking-at bst-comment-prefix)
	    (delete-char n))
	(forward-line 1)))))


(defun make-bst-TAGS-for-current-file ()
  "Create a TAGS file in the current directory for the BibTeX style
file in the current buffer.

Because BibTeX style files share most of their functions, it is not
useful to create a TAGS file for all *.bst files in the current
directory.

Attention: It erases TAGS file in current directory.

This is a stopgap function until the standalone etags utility can be
updated to recognize BibTeX style files."
  (interactive)
  (if (not (string-equal mode-name "BibTeX-style")) ; hardcoded
      (error "This buffer is not in BibTeX-style mode"))
  (let ((line-number 1) (start (point)) (start-name) (tags-buffer))
    (save-excursion
      (find-file "./TAGS")		
      (delete-region (point-min) (point-max))
      (setq tags-buffer (current-buffer))) 
    (goto-char (point-min))
    (princ (format "\f\n%s,0\n" buffer-file-name) tags-buffer)
    (while (< (point) (point-max))
      (if (looking-at "^ *FUNCTION *{[^}]*}")
	  (progn
	    (save-excursion
	      (save-match-data
		(goto-char (match-beginning 0))
		(search-forward "{")
		(setq start-name (point))))
	    (princ (format "%s\177%s\001%d,%d\n"
			   (buffer-substring (match-beginning 0) (match-end 0))
			   (buffer-substring start-name (1- (match-end 0)))
			   line-number
			   (1- (match-beginning 0)))
		   tags-buffer)))
      (forward-line 1)
      (setq line-number (1+ line-number)))
    (save-excursion
      (set-buffer tags-buffer)
      (save-buffer 0)
      (visit-tags-table (buffer-file-name)))
    (goto-char start)))


;; Providen features
(provide 'bst-mode)

;;; bst-mode.el ends here
