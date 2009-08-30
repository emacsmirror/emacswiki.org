;;; screenplay.el --- support writing of plain text screen plays

;; Copyright (C) 2002  Alex Schroeder <alex@gnu.org>

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.0
;; Keywords: wp
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ScreenPlay

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains code that will help you write screenplays in plain
;; text files.  Note that there is probably a LaTeX style out there that
;; will do the same thing for you, and look nicer.  But writing plain
;; text has its benefits: small, simple, easy to share, easy to mail.

;; Call `screenplay-reformat-region' or `screenplay-reformat-buffer' to
;; reformat your text.

;;; Code:

(defun screenplay-reformat-buffer ()
  "Reformat the buffer as a screenplay."
  (interactive)
  (screenplay-reformat-region (point-min) (point-max)))

(defun screenplay-reformat-region (start end)
   "Reformat the region between START and END as a screenplay."
   (interactive "r")
   (goto-char start)
   (beginning-of-line)
   (let ((end (copy-marker end))
	 (format (screenplay-line-type))
	 (old nil)
	 (mark nil))
     (while (< (point) end)
       ;; handle filling if necessary
       (cond ((eq old 'description)
	      (when (not (eq format 'description))
		(screenplay-wrap mark (point) 0 65)
		(setq mark nil)))
	     ((eq old 'dialog)
	      (when (not (eq format 'dialog))
		(screenplay-wrap mark (point) 10 45)
		(setq mark nil))))
       ;; indent lines, etc.
       (cond ((eq format 'empty)
	      (kill-line 1)
	      (forward-line -1))
	     ((eq format 'instructions)
	      (delete-horizontal-space)
	      (indent-to 16))
	     ((eq format 'heading)
	      (unless (bobp)
		(newline 2)))
	     ((eq format 'name)
	      (newline 1)
	      (delete-horizontal-space)
	      (indent-to 22))
	     ((eq format 'dialog)
	      (unless mark
		(setq mark (point))))
	     ((eq format 'description)
	      (unless mark
		(newline)
		(setq mark (point))))
	     (t (error "Format %S unhandled" format)))
       (forward-line 1)
       (setq old format
	     format (screenplay-line-type)))))

(defun screenplay-wrap (from to left right)
  "Fill region between FROM and TO.
LEFT and RIGHT are the margin columns used."
  (let ((left-margin left)
	(fill-column right)
	(to (copy-marker to)))
    (save-excursion
      (goto-char from)
      (while (< (point) to)
	(delete-horizontal-space)
	(forward-line)))
    (fill-region from to 'left)))

(defun screenplay-line-type ()
  "Return line type of current line."
  (save-excursion
    (beginning-of-line)
    (let ((upcase (screenplay-upcase-p (point)))
	  (column-zero (looking-at "\\S-"))
	  (parens (looking-at "\\s-*(.*)\\s-*$"))
	  (empty (looking-at "\\s-*$")))
      (cond (parens 'instructions)
	    (empty 'empty)
	    ((and upcase column-zero) 'heading)
	    (upcase 'name)
	    (column-zero 'description)
	    (t 'dialog)))))

(defun screenplay-upcase-p (start)
  "Return t if no lower case characters appear between START
and the end of the line."
  (save-excursion
    (goto-char start)
    (let (quit found (c (char-after)))
      (while (not (or quit found))
	(cond ((eobp) (setq quit t))
	      ((eolp) (setq quit t))
	      ;; if the character has two different cases, and it is
	      ;; lowercase...
	      ((and (not (eq (downcase c) (upcase c)))
		    (eq c (downcase c)))
	       (setq found t))
	      (t (forward-char)
		 (setq c (char-after)))))
      (not found))))

;;; screenplay.el ends here
