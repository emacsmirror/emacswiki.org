;;; tab-in-tabular.el - Treat ampersands like table field separators

;; Copyright - (CC-BY-SA) Some Rights Reserved Brandon Maister
;; Author: quodlibetor <quodlibetor@gmail.com>
;; Website: http://blog.quodlibetor.com
;; Keywords: LaTeX, keyconfig

;; Created: Sun Sep 14, 2008
;; Version: 0.1
;; URL: http://svn.quodlibetor.com/pubsh/elisp/tab-in-tabular.el
;; This file is not part of GNU Emacs.

;;; What is here:

;; This file contains a couple of functions to make emacs treat `&'s
;; more like cell separators in e.g. excell. This makes sense if you
;; are using the tabular environment in LaTeX all the time. Maybe
;; other times as well.

;; There are two core functions: tab-in-tabular and
;; backtab-in-tabular, the first of which tries first to find a
;; logical next ampersand to jump to, and if there isn't one it checks
;; if there is an amp on the previous line that it can align a new one
;; with. The next paragraph gives the algorithm in excruciating
;; detail, feel free to ignore it.


;; tab-in-tabular checks if there is an ampersand to the right of
;; point on the current line, if there is it moves point to the right
;; of it. If there is not, it checks if there is an ampersand to the
;; right of point on the immediately previous line, if *that* exists
;; an amp is created directly beneath it, and point is moved to the
;; right of it. Third option, if the first two failed, it checks if
;; there is a `\\' to the right of point on the current line, and if
;; there is it checks if there is an amp on the next line, which if
;; there is it moves point a little to the left of it. (That's sort of
;; a bug, see bugs) If there are no appropriate ampersands, it does
;; the normal tab-thing

;; backtab-in-tabular just checks if there is an amp before point on
;; this line or the immediately preceding line and moves point to the
;; left of the word inside the field, sort of.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; What is not here (bugs):

;; re-search-backward, or match-beginning/end doesn't do what i want,
;; so i can't seem to make point go to the left of a word.

;; Basically, the leftmost column doesn't do exactly what i want,
;; because of that bug.

;; It's kind of ugly that i search for a (nearly) impossible string
;; instead of just returning nil at the end of my utility
;; functions. Can't find a function to return nil, though. Help/advice
;; welcome.

;;;;;;;;;;;;;;;;;

;;; Installation:

;; stick this file in your load-path.

;; Then, stick something like the following in your .emacs file:

;; (add-hook 'LaTeX-mode-hook 'latex-redefine-tab)
;; (defun latex-redefine-tab ()
;;   "just calls `define-key' and sets tab up to use my function"
;;   (load "tab-in-tabular")
;;   (define-key LaTeX-mode-map "\t\ " 'tab-in-tabular)
;;   (define-key LaTeX-mode-map [S-iso-lefttab] 'backtab-in-tabular)
;;   (define-key LaTeX-mode-map [C-tab] 'indent-for-tab-command))

;; That [S-iso-lefttab] is maybe/probably wrong for your keyboard, to
;; find out what the correct thing is type `S-<TAB> M-x view-lossage
;; <RET>' and you'll see what your computer does for backtab. Just
;; replace the angle brackets with square brackets and put it in the
;; second define-key line.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2, or, at your
;; option, any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA


(defun amp-on-previous-line ()
  "Find an ampersand in the immediately previous row,
if there is an amp, return the difference in columns between its
position and starting, else return nil"
  (save-excursion
    (setq goal-column (current-column))
    (previous-line)
    (let ((start (point))
	  (end (progn
		 (move-end-of-line nil)
		 (point))))
      (setq goal-column nil)
      (goto-char start)
      (if (search-forward "&" end t)
	  (progn
	    (goto-char (match-beginning 0))
	    (current-column))
	; this next line because i can't figure out how to just return nil
	(search-forward "aintnosunshinewhenshesgone" end t)))))

(defun amp-on-this-line ()
  "return position of ampersand on this line, if there is one"
    (let ((start (point))
	  (end (save-excursion
		 (progn
		   (move-end-of-line nil)
		   (point)))))
      (if (re-search-forward "& ?" end t)
	  (point)
	(goto-char start)
	(search-forward "aintnosunshinewhenshesgone" nil t))))

(defun amp-on-next-line ()
  "return position of first char of first word of the first field
on the nex line, if this line ends in \\ and there is an amp on
the next line"
  (save-excursion
    (let ((line-end (save-excursion
		      (progn
			(move-end-of-line nil)
			(point))))
	  (end (save-excursion
		 (progn
		   (next-line)
		   (move-end-of-line nil)
		   (point))))
	  (new-point nil))
      (if (search-forward "\\\\" line-end t)
	  (progn
	    (setq new-point (progn (search-forward "&" end t) (point)))
	      (if (re-search-backward "\\w+" line-end t)
		  (progn
		    (goto-char (match-beginning 0))
		    (point))
		(goto-char new-point)
		(backward-char 3)
		(point)))
	(search-forward "aintnosunshinewhenshesgone" nil t)))))

(defun tab-in-tabular ()
  "If there is an `&' in the previous line, insert one in this
line, correctly aligned"
  (interactive)
  (let (column
	next-amp)
    (if (amp-on-this-line)
	(message "tab")
      ;; if no amp on this line, but one on previous:
      (if (setq column (amp-on-previous-line))
	  (progn
	    (move-to-column column t)
	    (insert "& ")
	    (message "tabbed editing: ACTIVATED!"))
	;; if no amp on this OR previous, but line ends in \\ and amp
	;; on next line:
	(if (setq next-amp (amp-on-next-line))
	    (goto-char next-amp)
	  (indent-for-tab-command))))))

(defun backtab-in-tabular ()
  "Just go backwards if there is an amp on this or previous line"
  (interactive)
  (let ((begin (save-excursion
		 (progn
		   (move-beginning-of-line nil)
		   (point)))))
    (if (re-search-backward " ?&" begin t)
	(progn
	  (goto-char (match-beginning 0))
	  (if (re-search-backward "& ?" begin t)
	      (goto-char (match-end 0))))
      (setq begin (save-excursion
		    (progn
		      (previous-line)
		      (move-beginning-of-line nil)
		      (point))))
      (re-search-backward "& ?" begin t)
      (goto-char (match-end 0)))))

;;; tab-in-tabular doesn't provide any features, see "installation"
;;; in the header to make use of the functions herein
;;;
;;; tab-in-tabular.el ends here.
