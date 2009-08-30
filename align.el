;;; align.el --- aling text seperated by whitespaces in columns

;; Copyright (C) 1999 by Matthias Helmling

;; Author: Matthias Helmling <matt@acid.rhein-neckar.de>
;; Keywords: extensions
;; Version: 0.2
;; Time-stamp: "99/01/30 15:46:16 matt"

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; tested with XEmacs 20.4 and GNU Emacs 20.2.1

;;; Code:
(defun align-cols (start end max-cols)
"Align text between point and mark as columns.
Columns are separated by whitespace characters.
Prefix arg means align that many columns. (default is all)"
  (interactive "r\nP")
  (save-excursion
    (let ((p start)
	  pos
	  end-of-line
	  word
	  count
	  (max-cols (if (numberp max-cols) (max 0 (1- max-cols)) nil))
	  (pos-list nil)
	  (ref-list nil))
      ;; find the positions
      (goto-char start)
      (while (< p end)
	(beginning-of-line)
	(setq count 0)
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(re-search-forward "^\\s-*" end-of-line t)
	(setq pos (current-column))	;start of first word
	(if (null (car ref-list))
	    (setq pos-list (list pos))
	  (setq pos-list (list (max pos (car ref-list))))
	  (setq ref-list (cdr ref-list)))
	(while (and (if max-cols (< count max-cols) t)
		    (re-search-forward "\\s-+" end-of-line t))
	  (setq count (1+ count))
	  (setq word (- (current-column) pos))
	  ;; length of next word including following whitespaces
	  (setq pos (current-column))
	  (if (null (car ref-list))
	      (setq pos-list (cons word pos-list))
	    (setq pos-list (cons (max word (car ref-list)) pos-list))
	    (setq ref-list (cdr ref-list))))
	(while ref-list
	  (setq pos-list (cons (car ref-list) pos-list))
	  (setq ref-list (cdr ref-list)))
	(setq ref-list (nreverse pos-list))
	(forward-line)
	(setq p (point)))
      ;; aling the cols starting with last row
      (setq pos-list (copy-sequence ref-list))
      (setq start 
	    (save-excursion (goto-char start) (beginning-of-line) (point)))
      (goto-char end)
      (beginning-of-line)
      (while (>= p start)
	(beginning-of-line)
	(setq count 0)
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(re-search-forward "^\\s-*" end-of-line t)
	(goto-char (match-end 0))
	(setq pos (nth count pos-list))
	(while (< (current-column) pos)
	  (insert-char ?\040 1))
	(setq end-of-line (save-excursion (end-of-line) (point)))
	(while (and (if max-cols (< count max-cols) t)
		    (re-search-forward "\\s-+" end-of-line t))
	  (setq count (1+ count))
	  (setq pos   (+  pos (nth count pos-list)))
	  (goto-char (match-end 0))
	  (while (< (current-column) pos)
	    (insert-char ?\040 1))
	  (setq end-of-line (save-excursion (end-of-line) (point))))
	(forward-line -1)
	(if (= p (point-min)) (setq p (1- p))
	  (setq p (point))))
      )))

(provide 'align)

;;; align.el ends here
