;;; guess-offset.el -- Automatically determine c-basic-offset

;; Copyright (C) 2003 Julian Scheid

;; Version: 0.1.1
;; Author: Julian Scheid <julian@sektor37.de>
;; Keywords: indent c-mode c-basic-offset

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A hook for C, C++ and Java modes that guesses the indentation
;; offset used in an existing C-like source code.

;; To install, add this file to one of your load directories,
;; byte-compile it and put the following at the very end of your
;; .emacs file:

;;    (require 'guess-offset)

;; When installed, guess-offset will briefly analyze every visited C
;; or C-like source code (C++ and Java, at the time) and make a
;; "guess" what indentation offset was used for creating this source.
;; If the guess is considered reliable, c-basic-offset is overridden
;; to reflect the proposed offset.

;; Guess-offset doesn't add anything to your Emacs environment, but it
;; removes something you won't miss - the hassle to manually deal with
;; source codes that are formatted with a different indentation
;; offset.  With guess-offset, Emacs will transparently adapt to
;; foreign indentation offsets.

;; It doesn't work perfectly for every possible source file.  In
;; particular, it won't work with source codes that use a single space
;; per level, and not with those that use varying indentation
;; depending on the outer construct.

;; Still, it will make dealing with a heap of source codes much
;; easier, and leave you no worse off with the remainder than before.
;; Remember, the c-basic-offset setting will only be tinkered with if
;; there is enough evidence that the guessed offset is the right one.

;; GuessOffset relies on heuristics and has a couple of variables you
;; can play with.  Please tune these settings if you are not satisfied
;; with the current behaviour.  If you think your settings are better
;; than the defaults, please send me a copy.

;; Note to users of other languages: the only reason guess-offset is
;; currently limited to C-like languages is that it can only deal with
;; C comments and expressions ("/*...*/" and "(...)", resp.).  It
;; shouldn't be too hard to extend this to work with other languages.
;; Please send a patch to the author if you do.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;-----------------------------------------------------------------
;;; Dependencies

(require 'cc-vars)

;;;-----------------------------------------------------------------
;;; Customization Definitions:

(defgroup guess-offset nil
  "Functions for automatically determining the indentation offset
used in a buffer."
  :version "21.1"
  :group 'tools)

(defcustom guess-offset-minimum-tab-width 2
  "*The minimum offset that can be guessed for a buffer."
  :type 'number
  :group 'guess-offset)

(defcustom guess-offset-maximum-tab-width 8
  "*The maximum offset that can be guessed for a buffer."
  :type 'number
  :group 'guess-offset)

(defcustom guess-offset-minimum-line-count 8
  "*The minimum count of non-empty, indented lines a buffer must
contain for guess-offset to readjust the offset. If there are
less lines, guess-offset will leave the buffer alone."
  :type 'number
  :group 'guess-offset)

(defcustom guess-offset-minimum-probability 0.5
  "*The minimum probability a guess must have for guess-offset to
readjust the offset. If the best guess is less probably than this
number, guess-offset will leave the buffer alone."
  :type 'number
  :group 'guess-offset)

(defcustom guess-offset-minimum-superiority 2.0
  "*The minimum factor the best guess must be better than the second
best guess for guess-offset to readjust the offset. If the best guess
is not significantly better than the second best guess, guess-offset
will leave the buffer alone. Note that guesses with equal (or similar)
probability are treated as one guess in this context. This avoids
problems with offsets that are multiples of other valid offsets (like
2, 4 and 8.)"
  :type 'number
  :group 'guess-offset)

(defcustom guess-offset-probability-gravity 0.01
  "*The difference between two probability values (percentages) below
which both values are considered equal. This tolerance is necessary to
avoid a fractional offset of the real offset being preferred over the
actual offset used in the buffer. For example, if an offset of two has
the probability 64.2% and an offset of four has the probability 64.9%,
then guess-offset will assume an offset of four instead of two when
the gravity is set to 0.007 (0.7 percent) or more."
  :type 'number
  :group 'guess-offset)

(defcustom guess-offset-quiet-p nil
  "*Whether guess-offset should refrain from outputting any messages."
  :type 'boolean
  :group 'guess-offset)

(defcustom guess-offset-debug-p nil
  "*Whether guess-offset should output lots of debugging
information."
  :type 'boolean
  :group 'guess-offset)

;; The function to use for detecting the start of a multi-line
;; comment.
(defvar guess-offset-skip-comment-start
  'guess-offset-skip-c-comment-start)

;; The function to use for detecting the end of a multi-line comment.
(defvar guess-offset-skip-comment-end
  'guess-offset-skip-c-comment-end)

;;;-----------------------------------------------------------------
;;; Function Definitions:

(defun guess-offset-skip-c-comment-start (eol)
  "Skip to the next C/C++/Java comment start in this line."
  (re-search-forward "/[*]" eol t))

(defun guess-offset-skip-c-comment-end (eol)
  "Skip to the next C/C++/Java comment end in this line."
  (re-search-forward "[*]/" eol t))

(defun guess-offset-collect-indent-widths ()
  "Return a indentation histogram for the current buffer."
  (let ((tab-positions nil)
	(line-count 0)
	go-in-comment)
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "[^\t\r\n ]" nil t)
	(let ((column (- (current-column) 1)))
	  (when (and (> column 0)
		     (not go-in-comment))
	    (let ((tabinfo (assoc column tab-positions)))
	      (if (null tabinfo)
		  (setq tab-positions (cons (cons column 1)
					    tab-positions))
		(setcdr tabinfo
			(+ 1 (cdr tabinfo)))))
	    (setq line-count (+ line-count 1))))
	;; check whether a comment begins or ends on this line
	(let ((eol (save-excursion (end-of-line) (point))))
	  (beginning-of-line)
	  (while
	      (or
	       (and (not go-in-comment)
		    (funcall guess-offset-skip-comment-start eol)
		    (setq go-in-comment t))
	       (and (not go-in-comment)
		    (re-search-forward "(" eol t)
		    (let ((bracket-level 1))
		      (while (and (/= bracket-level 0)
				  (re-search-forward "[()]" nil t))
			(if (eq ?\( (char-before))
			    (setq bracket-level (+ bracket-level 1))
			  (setq bracket-level (- bracket-level 1))))))
	       (and go-in-comment
		    (funcall guess-offset-skip-comment-end eol)
		    (not (setq go-in-comment nil))))))
	(beginning-of-line 2)))
    (cons line-count tab-positions)))

(defun guess-offset-get-tab-width-linecount (tab-positions try-offset)
  "Return the number of lines the given offset is used for."
  (let ((result 0))
    (while (not (null tab-positions))
      (let ((tab-size (car (car tab-positions)))
	    (line-count (cdr (car tab-positions))))
	(when (eq 0 (mod tab-size try-offset))
	  (setq result (+ result line-count))))
      (setq tab-positions (cdr tab-positions)))
    result))


(defun guess-offset-purge-suboffsets (offset-probability-list)
  "Return a modified version of the list passed in with all offsets
removed that are dividends of another offset and have a similar
probability."
  ;; sort the list by offsets in descending order, largest offset
  ;; first.
  (setq offset-probability-list
	(sort offset-probability-list
	      '(lambda (list1 list2)
		 (> (car list1) (car list2)))))

  ;; for each offset in this list, starting with the largest...
  (let ((offset-probability-iterator offset-probability-list))
    (while offset-probability-iterator

      ;; ... remove all possible sub-offsets from the remaining part
      ;; of the list:
      (let ((offset (car (car offset-probability-iterator))))

	;; for each `sub-offset' between `offset' - 1 and
	;; `guess-offset-minimum-tab-width', including, check whether
	;; `offset' is a multiple of `sub-offset' (in decending order)
	(let ((sub-offset (- offset 1)))

	  (while (>= sub-offset guess-offset-minimum-tab-width)
	    (let ((sub-offset-probability-pair
		   (assoc sub-offset offset-probability-list)))
	      (and sub-offset-probability-pair
		   (= 0 (mod offset sub-offset))

		   ;; if so, check whether its probability is near the
		   ;; probability of the current outer offset
		   ;; iterator.
		   (< (abs (- (cdr (car offset-probability-iterator))
			      (cdr sub-offset-probability-pair)))
		      guess-offset-probability-gravity)

		   ;; if this is the case, merge both entries. set the
		   ;; probability of the current entry to the mean
		   ;; value of both probabilities ...
		   (setcdr
		    (car offset-probability-iterator)
		    (/ (+ (cdr (car offset-probability-iterator))
			  (cdr sub-offset-probability-pair))
		       2))

		   ;; ... and throw away the other.
		   (setq offset-probability-list
			 (delq sub-offset-probability-pair
			       offset-probability-list)))
	      (setq sub-offset (- sub-offset 1))))))
      (setq offset-probability-iterator
	    (cdr offset-probability-iterator))))

  ;; result is the modified list
  offset-probability-list)

(defun guess-offset-get-guess ()
  "Guess the indentation offset used by the current buffer. Return the
offest or nil if the offset could not be guessed reliably."
  (let ((collect-result (guess-offset-collect-indent-widths))
	offset-probabilities)
    (let ((tab-positions
	   (sort (cdr collect-result)
		 '(lambda (list1 list2)
		    (> (cdr list1) (cdr list2)))))
	  (try-tab-width guess-offset-minimum-tab-width)
	  (line-count (car collect-result)))
      (when guess-offset-debug-p
	(message (concat "Indentation histogram:\n"
			 (mapconcat
			  '(lambda (pair)
			     (concat "  "
				     (number-to-string (car pair))
				     "->"
				     (number-to-string (cdr pair))))
			  tab-positions
			  "\n"))))
      (when (> line-count guess-offset-minimum-line-count)
	(while (<= try-tab-width guess-offset-maximum-tab-width)
	  (let ((probability (/ (float
				 (guess-offset-get-tab-width-linecount
				  tab-positions try-tab-width))
				line-count)))
	    (setq offset-probabilities
		  (cons (cons try-tab-width probability)
			offset-probabilities))
	    (setq try-tab-width (+ 1 try-tab-width))))
	(let ((sorted-probabilities
	       (sort (guess-offset-purge-suboffsets
		      offset-probabilities)
		     '(lambda (list1 list2)
			(let ((prob1 (cdr list1))
			      (offs1 (car list1))
			      (prob2 (cdr list2))
			      (offs2 (car list2)))

			  ;; two probabilities are considered equal
			  ;; when their difference is below
			  ;; guess-offset-probability-gravity
			  ;;
			  ;; if they are equal,
			  (or (> prob1 prob2)
			      (and (= prob1 prob2)
				   (> offs1 offs2))))))))

	  (when guess-offset-debug-p
	    (message
	     (concat "Probable offsets:\n"
		     (mapconcat
		      '(lambda (pair)
			 (concat "  Offset "
				 (number-to-string (car pair))
				 " has probability "
				 (format "%f" (cdr pair))))
		      sorted-probabilities
		      "\n"))))

	  (let ((gold-offset
		 (car (nth 0 sorted-probabilities)))
		(gold-probability
		 (cdr (nth 0 sorted-probabilities)))
		(silver-probability
		 (cdr (nth 1 sorted-probabilities))))

	    (when (and (>= gold-probability
			   guess-offset-minimum-probability)
		       (or (null silver-probability)
			   (= 0 silver-probability)
			   (>= (/ (float gold-probability)
				  (float silver-probability) )
			       guess-offset-minimum-superiority)))
	      gold-offset)))))))

(defun guess-offset-set-c-basic-offset ()
  "Guess the proper c-basic-offset for the current buffer and change
it if a reliable guess could be made."
  (let ((guessed-tab-width (guess-offset-get-guess)))
    (when (not (or (null guessed-tab-width)
		   (eq guessed-tab-width c-basic-offset)))
      (setq c-basic-offset guessed-tab-width)
      (unless guess-offset-quiet-p
	(message (concat "Note: c-basic-offset adjusted to "
			 (number-to-string guessed-tab-width)
			 " for buffer "
			 (buffer-name)
			 "."))))))

;;;-----------------------------------------------------------------
;;; Installation

(defvar guess-offset-hooks
 (list 'c-mode-hook
       'c++-mode-hook
       (if (featurep 'jde)
	      'jde-mode-hook
	    'java-mode-hook)))

(mapcar '(lambda (hook-name)
	   (add-hook hook-name
		     'guess-offset-set-c-basic-offset
		     t))
	guess-offset-hooks)

(provide 'guess-offset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; guess-offset.el ends here
