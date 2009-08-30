;;; simple-call-tree.el -- analyze source code based on font-lock text-properties

;; Copyright (C) 2003  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.0
;; Keywords: programming
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?CallTree

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code analyses a buffer and uses text properties to find
;; function names (these have the face font-lock-function-name-face).
;; Therefore, it should work for any source language, as long as it is
;; fontified correctly.

;;; Code:

(defvar simple-call-tree-alist nil
  "Alist of functions and the functions they call.")

(defun simple-call-tree-analyze (&optional test)
  "Analyze the current buffer.
The result is stored in `simple-call-tree-alist'.
If optional function TEST is given, it must return non-nil when
called with one parameter, the starting position of the function
name."
  (interactive)
  (setq simple-call-tree-alist nil)
  (let ((pos (point-min))
	(count 0))
    (while pos
      (when (and (eq (get-text-property pos 'face)
		     'font-lock-function-name-face)
		 (or (not (functionp test))
		     (funcall test pos)))
	(setq count (1+ count))
	(message "Identifying functions...%d" count)
	(let ((start pos))
	  (setq pos (next-single-property-change pos 'face))
	  (setq simple-call-tree-alist (cons (list (buffer-substring-no-properties
					     start pos))
				      simple-call-tree-alist))))
      (setq pos (next-single-property-change pos 'face)))
    (setq pos (point-min)
	  max count
	  count 0)
    (save-excursion
      (let ((old (point-min))
	    (old-defun '("*Start*"))
	    defun)
	(while pos
	  (when (and (eq (get-text-property pos 'face)
			 'font-lock-function-name-face)
		     (or (not (functionp test))
			 (funcall test pos)))
	    (setq end (next-single-property-change pos 'face)
		  defun (assoc (buffer-substring-no-properties pos end)
			       simple-call-tree-alist))
	    (setq count (1+ count))
	    (message "Identifying functions called...%d/%d" count max)
	    (simple-call-tree-add old pos old-defun)
	    (setq old end
		  pos end
		  old-defun defun))
	  (setq pos (next-single-property-change pos 'face))))))
  (message "simple-call-tree done"))

(defun simple-call-tree-add (start end alist)
  "Add tokes between START and END to ALIST.
ALIST is a list with a string identifying the function in its car,
and the list of functions it calls in the cdr."
  (dolist (entry simple-call-tree-alist)
    (goto-char start)
    (catch 'done
      (while (search-forward (car entry) end t)
	(let ((faces (get-text-property (point) 'face)))
	  (unless (listp faces)
	    (setq faces (list faces)))
	  (unless (or (memq 'font-lock-comment-face faces)
		      (memq 'font-lock-string-face faces))
	    (setcdr alist (cons (car entry)
				(cdr alist)))
	    (throw 'done t)))))))

(defun simple-call-tree-analyze-perl ()
  "Call `simple-call-tree-analyze-perl' for CPerl code."
  (interactive)
  (simple-call-tree-analyze (lambda (pos)
		       (goto-char pos)
		       (beginning-of-line)
		       (looking-at "sub"))))

(defun simple-call-tree-invert (alist)
  "Invert ALIST."
  (let (result)
    (mapc (lambda (entry)
	    (mapc (lambda (func)
		    (let ((elem (assoc func result)))
		      (if elem
			  (setcdr elem (cons (car entry)
					     (cdr elem)))
			(setq result (cons (list func (car entry))
					   result)))))
		  (cdr entry)))
	  simple-call-tree-alist)
    result))

'(eval-when-compile
   (assert (and (simple-call-tree-analyze)
                (equal simple-call-tree-alist
                       '(("simple-call-tree-invert")
                         ("simple-call-tree-analyze-perl" "simple-call-tree-analyze")
                         ("simple-call-tree-add")
                         ("simple-call-tree-analyze" "simple-call-tree-add")))))
   (assert (equal (simple-call-tree-invert simple-call-tree-alist)
                  '(("simple-call-tree-add" "simple-call-tree-analyze")
                    ("simple-call-tree-analyze" "simple-call-tree-analyze-perl")))))

(provide 'simple-call-tree)
;;; simple-call-tree.el ends here
