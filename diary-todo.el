;;; diary-todo.el --- display `to-do' list in diary.

;; Authors:         Martin Schwenke <martin@meltin.net>
;; Maintainer:      martin@meltin.net
;; Created:         1998-11-26
;; Last Modified:   $Id: diary-todo.el,v 1.7 2002/04/23 04:03:09 martins Exp $
;; Keywords: diary todo

;; Copyright (C) 1998, 1999, 2001 Martin Schwenke

;; This file is NOT part of GNU Emacs.  It is, however, distributed
;; under the same conditions as GNU Emacs, which are as follows:

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Takes the file specified by `todo-file-top', removes the
;; todo-prefix stuff from each line and inserts in the diary for the
;; current day.

;; INSTALLATION
;;    To install, simply copy this file into a directory in your
;;    load-path and add the following two commands in your .emacs file:
;;
;;   (autoload 'diary-include-todo-file "diary-todo"
;;     "Include `to do' list in diary.")
;;   (add-hook 'list-diary-entries-hook 'diary-include-todo-file t)

;; src: https://web.archive.org/web/20060721170921/http://meltin.net/hacks/emacs/src/diary-todo.el [2026-03-14]

(require 'todo-mode)

(defvar diary-todo-prefix ""
  "*String prefix inserted in front of each `to do' entry in the diary.")

;; Someone, somewhere has probably done this, but...
(defun diary-todo-get-file-lines (file)
  "Return lines of FILE as a list, in order, including empty lines."

  (let* ((b1 (find-buffer-visiting file))
	 (b2 (or b1 (find-file-noselect file t))))
    (if b2
	(let (lines eol done)
	  (save-excursion
	    (set-buffer b2)
	    (goto-char (point-max))
	    (end-of-line)
	    (while (not done)
	      (setq eol (point))
	      (beginning-of-line)
	      (setq lines (cons (buffer-substring (point) eol) lines))
	      (setq done (= (point) (point-min)))
	      (forward-line -1)
	      (end-of-line))
	    (if (not b1)
		(kill-buffer b2)))
	  lines))))

(defun diary-todo-category-p (entry)
  "Return t if ENTRY is a category heading, nil otherwise."

  (equal (string-match (regexp-quote todo-category-beg) entry) 0))

(defun diary-todo-add-to-diary-list (entry)
  "Call add-to-diary-list to add ENTRY to the diary on original-date.
The variable `diary-todo-prefix' is a string to prefix each diary
entry with."

  ;; original-date comes from diary-lib.  I don't know what the 3rd
  ;; argument to add-to-diary-list does under Emacs 20!
  (let ((fullentry (concat diary-todo-prefix entry)))
    (if (>= emacs-major-version 20)
	(add-to-diary-list original-date fullentry "")
      (add-to-diary-list original-date fullentry))))

(defun diary-include-todo-file ()
  "Include entries from `to do' file in diary.
The variable `diary-todo-prefix' is a string to prefix each diary
entry with.  The `to do' entries appear after any other diary entries
for the current day.  If the `to do' file is empty then nil is
returned."

  (let ((ll (and todo-file-top (diary-todo-get-file-lines todo-file-top)))
	(original-diary-entries diary-entries-list))

    ;; Grab all of the original entries up until the current date.
    (setq diary-entries-list nil)
    (while (and original-diary-entries
		(not (calendar-date-compare (list original-date "")
					    (car original-diary-entries))))
      (setq diary-entries-list (append diary-entries-list
				       (list (car original-diary-entries)))
	    original-diary-entries (cdr original-diary-entries)))

    ;; Now add the `to do' entries, skipping empty categories.
    (let (prev)
      (while ll
	(let ((l (car ll)))
	  (and (not (string= l ""))
	       (string-match (regexp-quote todo-prefix) l)
	       (let ((curr (substring l (match-end 0))))
		 (if (and prev
			  (or (not (diary-todo-category-p prev))
			      (not (diary-todo-category-p curr))))
		     (progn
		       (if (not (diary-todo-category-p prev))
			   (setq prev (concat " " prev)))
		       (diary-todo-add-to-diary-list prev)))
		 (setq prev curr))))
	(setq ll (cdr ll)))
      (if (and prev
	       (not (diary-todo-category-p prev)))
	  (diary-todo-add-to-diary-list prev)))

    ;; And finally tack on the rest of the original list.
    (setq diary-entries-list
	  (append diary-entries-list original-diary-entries))))

(provide 'diary-todo)
