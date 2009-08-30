;;; py-indent.el --- Python indentation with annotations

;; Copyright (C) 2008,2009 Davis Herring

;; Author: Davis Herring <herring@lanl.gov>
;; Keywords: python indent
;; Updated: January 13 2009
;; Version: 0.3

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides support for annotating Python code with C-like block
;; delimiters (in Python comments) that allow the indentation of the file to
;; be reconstructed after modification or damage.  Annotations may be
;; generated automatically from the current indentation; the user may freely
;; add or remove annotations to change the extent of blocks, and then apply
;; the annotations to update or restore the indentation.  Entry points:

;; `pyi-annotate' - add annotations to the buffer based on indentation
;; `pyi-unannotate' - remove all annotations
;; `pyi-apply' - recalculate all indentation from annotations
;; `pyi-line' - recalculate one line's indentation

;; The automatic annotation and unannotation try to be smart about placement
;; of the annotations and cleaning up blank spots where they are removed.

;; Warning: annotations, if used, must exist for a whole buffer, because a
;; region with none added can't be distinguished from one that should be
;; evenly indented!

;; This code attempts to work with both "python-mode.el" and "python.el".

;;; Bugs:

;; Strings that contain literal newlines and/or hashes can confuse the code,
;; especially with "python-mode.el" and with Emacs 21.  Escaping newlines or
;; hashes with backslashes helps in some cases.

;;; History:

;; Version 0.0, October 27 2008:
;;  - adding annotations only
;; Version 0.1, October 28 2008:
;;  - functional version for Emacs 22
;; Version 0.2, October 28 2008:
;;  - added support for Emacs 21 and python-mode.el
;; Version 0.2.1, October 29 2008:
;;  - fixed recognition of continuation lines in `pyi-line'
;;  - fixed annotating block-open lines that contain # in a string
;; Version 0.2.2, October 31 2008:
;;  - added default indent in case no python mode is loaded/helpful
;; Version 0.2.3, January 13 2009:
;;  - added warning/error for unmatched open/close annotations
;; Version 0.3, January 13 2009:
;;  - fixed `pyi-beginning-of-line' breaking at beginning of buffer
;;  - used 22/python.el features when available to deal with strings better
;;  - supplied `comment-start-skip' when needed (for python.el)
;;  - added errors to `debug-ignored-errors'

;;; Code:

;; Rules: Lines that end within () [] {} or with a \ outside of a comment or
;; string are joined to the next, and lines that contain only [ \t\f] and/or a
;; comment are ignored for indentation.  \ has \ syntax in either mode.

(eval-when-compile (require 'cl))	; incf, decf
(require 'newcomment)			; comment-indent

(defgroup pyi nil
  "Python indentation with annotations."
  :prefix "pyi-")
(defcustom pyi-annotate-start "py{"
  "String to annotate the beginning of a block."
  :type 'string
  :group 'pyi)
(put 'pyi-annotate-start 'safe-local-variable 'stringp)
(defcustom pyi-annotate-end "}py"
  "String to annotate the end of a block."
  :type 'string
  :group 'pyi)
(put 'pyi-annotate-end 'safe-local-variable 'stringp)
(defcustom pyi-backup-indent 4
  "Offset per level of indentation.
Only used if no known Python mode has specified one."
  :type 'integer
  :group 'pyi)
(put 'pyi-backup-indent 'safe-local-variable 'integerp)

(defconst pyi-whitespace " \t\f")
(defconst pyi-whitespace-regexp (concat "[" pyi-whitespace "]*"))
(defconst pyi-comment-regexp (concat pyi-whitespace-regexp "#"))
(defconst pyi-empty-regexp (concat pyi-whitespace-regexp "$"))
(defconst pyi-line-end-regexp "\\(?:#.*\\)?$")
(defconst pyi-blank-regexp (concat pyi-whitespace-regexp pyi-line-end-regexp))

(defun pyi-indent ()
  "Get the offset for each level of indentation.
Checks for `python-indent' and `py-indent-offset'.
Uses `pyi-backup-indent' if neither is available."
  (cond ((boundp 'python-indent) python-indent)
	((boundp 'py-indent-offset) py-indent-offset) (pyi-backup-indent)))

(defun pyi-how-many (re s e)
  "Return number of matches for RE between S and E."
  (if (>= emacs-major-version 22) (how-many re s e)
    (save-excursion
      (let ((n 0))
	(goto-char s) (while (re-search-forward re e t) (incf n)) n))))

(defun pyi-comment-p ()
  "Return non-nil if the current line bears a comment.
Leaves point somewhere on the line."	; really, in comment or at end
  (if (fboundp 'syntax-ppss)
      (eq 'comment (syntax-ppss-context (syntax-ppss (line-end-position))))
    ;; `comment-beginning' fails on # inside strings; even this fails if they
    ;; contain the end of the current line.
    (forward-line 0)
    ;; Emacs 22 python.el doesn't define `comment-start-skip'.
    (let ((comment-start-skip (or comment-start-skip "#+ *")))
      (comment-search-forward (line-end-position) t))))
;; (let ((e (line-end-position)))
;; (while
;;     (progn
;;       (re-search-forward pyi-whitespace-regexp)
;;       (unless (or (eq (char-after) ?#) (eolp))
;; 	(skip-syntax-forward ".w_()") ; don't skip whole paren-sexps here
;; 	(or (zerop (save-excursion (skip-syntax-forward "\"|")))
;; 	    (progn (forward-sexp) (< (point) e))))))
;; (and (< (point) e) (eq (char-after) ?#)))))

(defun pyi-beginning-of-line ()
  "Move to beginning of current logical line.
Absent python.el, point must not be within a string."
  (interactive)
  (if (fboundp 'python-beginning-of-statement) (python-beginning-of-statement)
    (forward-line 0)			;get out of any comment
    (let ((parse-sexp-ignore-comments t))
      (while
	  (progn (ignore-errors (while t (up-list -1)))
		 (forward-line 0)
		 (and (not (bobp))
		      (save-excursion
			(backward-char)
			(and (eq (char-before) ?\\) (not (pyi-comment-p))))))
	(forward-line -1)))))		;advance past strings and parens

(defun pyi-skip-line ()
  "Advance past real content of current logical line.
Absent python.el, point must not be within a string.
Leave point before any trailing whitespace and/or comment."
  (if (fboundp 'python-end-of-statement)
      (progn (python-end-of-statement)
	     (let ((syntax (syntax-ppss)))
	       (if (nth 4 syntax) (goto-char (nth 8 syntax))))
	     (skip-chars-backward pyi-whitespace))
    (while
	(unless (looking-at pyi-blank-regexp)
	  (re-search-forward pyi-whitespace-regexp)
	  (cond ((looking-at "\\\\$") (forward-line 1))
		((> (skip-syntax-forward ".w_)") 0))
		(t (forward-sexp))) t))))

(defun pyi-end-of-line ()
  "Go to end of current logical line, before any trailing whitespace/comment.
Absent python.el, point must not be within a string."
  (interactive)
  ;; Find our way out of enclosing parens before proceeding forward.
  (unless (fboundp 'python-end-of-statement) (pyi-beginning-of-line))
  (pyi-skip-line))

(defun pyi-annotate ()
  "Annotate Python indentation in the current buffer."
  (interactive "*")
  (save-excursion
    (save-restriction
      (widen) (goto-char (point-min))
      (and (or (save-excursion (search-forward pyi-annotate-start nil t))
	       (search-forward pyi-annotate-end nil t))
	   (error "Annotations already present; maybe change annotation style"))
      (re-search-forward pyi-whitespace-regexp)
      (if (> (point) (point-min))
	  (error "Indentation at start of file"))
      (let ((tab-width 8) (is '(0)) last) ;indentation stack, annotation spot
	(while (not (eobp))
	  ;; Each iteration advances through a logical line.
	  (re-search-forward pyi-whitespace-regexp)
	  (let ((id (current-column)) (ci (car is)) up) ;up: update `last'
	    (if (looking-at pyi-line-end-regexp)
		;; Even if it's just a comment, we want to close any enclosing
		;; block after it.
		(and (eq (char-after) ?#) (= id ci) (setq up t))
	      (setq up t)
	      (cond
	       ((> id ci)
		(push id is)
		(save-excursion
		  (goto-char last)
		  (if (pyi-comment-p) (progn (end-of-line) (insert ?\ ))
		    (comment-indent))
		  (insert pyi-annotate-start)))
	       ((< id ci)
		(let ((d 0))
		  (while (progn (pop is) (incf d) (< id (car is))))
		  (if (/= id (car is)) (error "Unrecognized dedent"))
		  (save-excursion
		    ;; It's important that we avoid inserting on our own line
		    ;; where the excursion marker is.
		    (goto-char last) (insert ?\n) (indent-to id) (insert ?#)
		    ;; d is always positive at this point.
		    (while (progn (insert ?\  pyi-annotate-end)
				  (> (decf d) 0)))))))
	      (pyi-skip-line))
	    (forward-line 1)
	    (if up (setq last (1- (point))))))
	(when (setq is (cdr is))	; we ended in a block
	  (let ((fn (bolp)))		; final-newline status
	    (unless fn (insert ?\n)) (insert ?#)
	    (while (progn (insert ?\  pyi-annotate-end) (setq is (cdr is))))
	    (if fn (insert ?\n))))))))

(add-to-list 'debug-ignored-errors "^Annotations already present\>")
(add-to-list 'debug-ignored-errors "^Indentation at start of file$")
(add-to-list 'debug-ignored-errors "^Unrecognized dedent$")

(defun pyi-unannotate ()
  "Remove indentation annotations in the current buffer.
They must lie inside comments, but need not be alone in them."
  (interactive "*")
  (save-excursion
    (save-restriction
      (widen) (goto-char (point-min))
      (let ((re (concat "\\(?:\n?" pyi-whitespace-regexp "#\\)?\\(?:"
			pyi-whitespace-regexp "\\(?:"
			(regexp-quote pyi-annotate-start) "\\|"
			(regexp-quote pyi-annotate-end) "\\)\\)+")))
	(while (re-search-forward re nil t) (replace-match ""))))))

(defun pyi-apply ()
  "Apply indentation annotations in the current buffer.
Destroys all extant indentation except that of continuation lines."
  (interactive "*")
  (save-excursion
    (save-restriction
      (widen) (goto-char (point-min))
      (let ((id 0) (res (regexp-quote pyi-annotate-start))
	    (ree (regexp-quote pyi-annotate-end)))
	(while (not (eobp))
	  ;; Each iteration advances through a logical line.
	  (let ((p (point)) (c (looking-at pyi-comment-regexp))
		(e (looking-at pyi-empty-regexp)) (i id)
		(lep (progn (pyi-skip-line) (line-end-position))))
	    (unless (wholenump (incf id (- (pyi-how-many res p lep)
					   (pyi-how-many ree p lep))))
	      (message "Unexpected end annotation here") (sit-for 2)
	      (error "Unexpected end annotation"))
	    ;; Now go back and indent if the line wasn't empty, to the old
	    ;; value unless the line is only a comment and the indent
	    ;; decreased (a block-close comment).
	    (unless e
	      (save-excursion
		(goto-char p)
		(indent-line-to (* (if c (min i id) i) (pyi-indent))))))
	  (forward-line 1))
	(unless (zerop id) (message "Warning: unmatched start annotation"))))))

(add-to-list 'debug-ignored-errors "^Unexpected end annotation$")

(defun pyi-calculate ()
  "Get indentation for current line based on indentation annotations.
Other non-blank, non-comment lines are assumed to be properly indented.
Continuation lines are simply indented half a level past their starts.
This function does not move point."
  (save-excursion
    (forward-line 0)
    (let ((p (point)) (res (regexp-quote pyi-annotate-start))
	  (ree (regexp-quote pyi-annotate-end)))
      (pyi-beginning-of-line)
      (cond ((bobp) 0)
	    ((/= p (point)) (+ (/ (pyi-indent) 2) (current-indentation)))
	    (t (while (progn (forward-line -1) (pyi-beginning-of-line)
			     (looking-at pyi-comment-regexp)))
	       (+ (* (pyi-indent) (- (pyi-how-many res (point) p)
				    (pyi-how-many ree (point) p)))
		  (current-indentation)))))))

(defun pyi-line ()
  "Indent current line based on indentation annotations.
Other non-blank, non-comment lines are assumed to be properly indented."
  (interactive "*")
  (save-excursion (indent-line-to (pyi-calculate))))

(provide 'py-indent)

;; py-indent.el ends here
