;;; rep-words.el --- find repeat words that repeat too much

;; Version: 0.1
;; Copyright (C) 2007 Theron Tlax
;; Time-stamp: <2007-06-01 18:15:20 thorne>
;; Author: thorne <thorne@timbral.net>
;; Created: 2007.5.27
;; Keywords: wp
;; Favorite color: Blue--no, yellow!

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License,
;; version 2, as published by the Free Software Foundation.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

 
;;; Commentary:

;; This quick little hack searches from point forward for words that
;; repeat too many times within a short space of text--a common
;; problem (or phenomenon, at any rate) in fiction or other writing.
;; Once found, said words are highlighted and you have the option to
;; recursively edit them or skip them.  This is part of a larger
;; project i am working on--called `litmus'--for working with
;; literary manuscripts.  I wrote this to use with my own novel
;; manuscript because i couldn't find such a thing ready-made.  I
;; found it useful, so here it is.

;; The entry point is the function `rep-words'.

;; To use, do M-x rep-words RET.  With a prefix argument `rep-words'
;; will prompt for how large an area of text should be searched
;; surrounding the current word.  If you say `100' then you will be
;; shown words that have too many repeats within 50 words one way or
;; the other; then it will prompt for the number of repeats that in
;; your view constitutes `too many'.  So you might do:

;; C-u M-x rep-words RET 200 RET 3 RET -- to search for any word
;; that is repeated more than 3 (4 or more) times in any 200 word
;; stretch.

;; Without a prefix argument `rep-words' will use the values of
;; `rep-max-repeats' (default: 2) and `rep-region-size' (default:
;; 100).

;; Each time a match is found all occurrences of it in the buffer
;; will be highlighted and you will be given the option to skip it,
;; quit entirely (leaving point where it is) or recursively edit.
;; If you choose to edit, the usual C-M-c will take you back to the
;; search, but starting at the CURRENT location of point, not where
;; it was when you entered the edit.

;; Finally, since some words are so common that they will constantly
;; make matches you don't care about, the variable `rep-ignore-list'
;; contains a list of strings--words you want the function to
;; ignore.  I seed it below with a few of the more obvious English
;; words.  Change it to anything you want.

;;; Requirements:

;; Uses hi-lock.el.  I think that's an Emacs 22 thing.  Maybe works
;; with earlier versions.

;;; Bugs:

;; Standard disclaimer: i'm not a real programmer, etc.  The word
;; matching regexp could be improved?--the whole approach could be
;; improved?  Who knows.  Also, no Emacs lisp library should have
;; more comments than code, as this one does.

;;; History:

;; Basic testing on Emacs 22 and 23.

 
;;; Code:

(require 'hi-lock)

(defvar rep-region-size 100
  "*Number of words surrounding the current word to be searched
for repeat words by function `rep-words'.")

(defvar rep-max-repeats 2
  "*Maximum allowed number of repeat words in the current search
region before the word is flagged by function `rep-words'.")

;; Words to ignore:
(defvar rep-ignore-list
  '("a" "and" "i" "in" "it"
    "of" "s" "the" "to" "you")
  "*List of words that `rep-words' should ignore.")

(defun rep-regexp-word (string)
  "Return a regexp matching all occurrences of STRING as a word."
  (concat "\\b" string "\\b"))

(defun rep-highlight-word (word)
  "Highlight WORD."
  (interactive)
  (highlight-phrase (rep-regexp-word (capitalize word)))
  (highlight-phrase (rep-regexp-word (downcase word))))

(defun rep-unhighlight-word (word)
  "Unhighlight WORD."
  (interactive)
  (unhighlight-regexp (rep-regexp-word (capitalize word)))
  (unhighlight-regexp (rep-regexp-word (downcase word))))

(defun rep-check-word (word region-size max-occur)
  "Return `t' if WORD occurs more than MAX-COUNT times in
REGION-SIZE words surrounding point."
  (let* ((position (point)))
    (save-excursion
      (let* ((start (progn (backward-word (/ region-size 2))
			   (point)))
	     (end (progn (forward-word region-size) (point))))
	(goto-char start)
	(if (word-search-forward word end t max-occur)
	    t
	  nil)))))


(defun rep-word-command-loop (word)
  "Interactive loop for `rep-words'."
  (let ((char (char-to-string (read-char "[e]dit or [s]kip; all else quits"))))
    (cond ((equal char "s") t)
	  ((equal char "e") (recursive-edit))
	  (t (progn (rep-unhighlight-word word)
		    (keyboard-quit))))))

 
;;;###autoload
(defun rep-words (&optional prefix)
  "Recursively edit words that repeat in close proximity.
From point, highlight the first word that occurs too many times
in its surrounding area; query for edit \(or continue,
highlighting the next such word\).  Ignore words occurring in
variable `rep-ignore-list'.

With prefix arg, query for values to use for the size of the area
searched \(in number of words\) and for the maximum allowed
number of repetitions in that area.  Without prefix, obtain these
values from variables `rep-max-repeats' and `rep-region-size'"
  (interactive "P")
  (let ((region-size (if prefix
			 (string-to-number
			  (read-string "Size of area to search: "))
		       rep-region-size))
	(max-repeats (if prefix
			 (string-to-number
			  (read-string "Maximum allowed repetitions in that area: "))
		       rep-max-repeats)))
    (while (forward-word 1)
      (let ((word (current-word)))
	(unless (member-ignore-case word rep-ignore-list)
	  (if (rep-check-word word region-size (1+ max-repeats))
	      (progn (rep-highlight-word word)
		     (rep-word-command-loop word)
		     (rep-unhighlight-word word))))))))

(provide 'rep-words)

;;; rep-words.el ends here
