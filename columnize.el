;;; columnize.el --- Formats a list of items into columns (pillars)

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Rick Bielawski <rbielaws@i1.net>
;; Keywords: tools convenience formatting

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;;; Commentary:

;; `columnize-text' is an interactive autoload function for formatting words
;; numbers etc into an evenly spaced series of columns (pillars).  A trivial
;; wrapper `columnize-strings' similarly formats a list of quoted strings.
;;
;; See C-h f columnize-text <ret> for a complete description.

;;; Installing:

;; 1) Put columnize.el on your load path.
;; 2) Put one of the following two lines in your .emacs
;;    (autoload 'columnize-text "columnize"
;;              "Formats a list of items into columns (pillars)" t)
;;    (load "columnize" nil t)
;;
;; Optionally add a key mapping like this.
;;    (global-set-key [?\C-x ?c] 'columnize-text)

;;; To-do:

;; Create a version that arranges the items by pillar rather than row.
;; That is, like this:  a d g  rather than this:  a b c
;;                      b e h                     d e f
;;                      c f i                     g h i

;;; History:

;; 2005-09-24 RGB Seems usable enough to release.
;; 2005-09-26 RGB Moved regexp used by columnize-strings to
;;                columnize-quoted-regexp
;; 2005-09-29 RGB Make pillar-width consistent no matter if the user 
;;                specifies the number of pillars or the number is
;;                chosen by the columnize-text default algorithm.
;;; Code:

(defvar columnize-regexp "\\([^ \t\n]+\\)"
  "Expression to separate words being columnized from discardable whitespace.
By overriding this value you can cause other entities such as quoted strings
containing embedded spaces to be recognized.  See `columnize-strings'.

NOTE: anything not matching match-string 1 of this regexp will be 
considered whitespace and discarded." )

(defvar columnize-quoted-regexp
  ;; Maybe this should recognize syntax table defined quotes rather than just
  ;; the ASCII quote character.  But since modes can always define a buffer
  ;; local version of this var that recognizes their particular string syntax
  ;; I didn't take the trouble.  It executes faster for more users this way.
  "\\(?:\\=\\|[^\"\\]\\)\\(\"[^\n\"]*[^\"\n\\]?\"\\)"
  ;; this is a wiki bug workaround comment \"
  "Expression used by `columnize-strings' to separate words from whitespace.
The strings are not allowed to span lines.

NOTE: anything not matching match-string 1 of this regexp will be 
considered whitespace and discarded." )

(defvar columnize-spacing 2
  "Minimum number of spaces between pillars when calculating a default.
The default pillar width is the length of the longest word being columnized
plus this value.  It must not be less than 1." )

(defvar columnize-span 1
  "Maximum number of pillars a word can span.
This is applicable when columnize-text is called with a pillar-width that is
smaller than some of the words being columnized.  If any word would span too
many pillars an error occurs and the text is not modified.  A value of 0 means
any number of pillars can be spanned.  1 disables spanning multiple 
pillars.")

;;;###autoload
(defun columnize-strings (begin end &optional pillar-width)
  "Trivial wrapper for `columnize-text' which see.
Uses `columnize-quoted-regexp' to recognize quoted strings as words even if
they contain embedded spaces."
  (interactive "r\np")
  (let ((columnize-regexp columnize-quoted-regexp))
    (columnize-text begin end pillar-width)))

;;;###autoload
(defun columnize-text (begin end &optional pillar-width)
  "Turns a list of words in a region into an evenly spaced list.
For clarity I use pillar to describe a column of words since the word
column already has a meaning in emacs.

The region is defined by BEGIN and END.  If PILLAR-WIDTH is not specified then
the length of the longest word in the region + `columnize-spacing' is used.
The number of pillars is determined by how many will fit between the indent
column and `fill-column'.  The list is indented to BEGIN's `current-column'.
Words are defined as the sequence of characters matching `columnize-regexp'.

Interactively, point and mark determine the region, a prefix argument sets
PILLAR-WIDTH.  A value of 0 or 1 means use the default.  Negative values mean
calculate a width that yields that number of pillars.  For example -4 means
calculate a value for PILLAR-WIDTH that yields 4 pillars.

If PILLAR-WIDTH is specified but is less than or equal to the longest word,
words will overflow into adjacent pillars but cannot span more than 
`columnize-span' pillars which see.

Example using a fill-column of 70 and letting pillar-width default.

     here is a list of various length words that I want columnized

     here        is          a           list        of
     various     length      words       that        I
     want        columnized

See also `columnize-strings'."
  (interactive "r\np")
  (let ((max-len 0)             ; Length of longest word
        num-pillars             ; Number of pillars to be created
        first-column            ; Starting position of first pillar
        line-width              ; Space between first & fill columns
        word-len                ; Length of current word
        word-width              ; 1 unless word-len > pillar-width
        (current-pillar 0)      ; Determines when to start a new line
        pad-count               ; Num spaces needed before next word
        replacement)            ; Accumulation of reformatted text
    (if (< columnize-spacing 1)
        (error "Illegal value for columnize-spacing"))
    (goto-char begin)
    (setq first-column (current-column))
    (setq line-width (- fill-column first-column))
    ;; Find length of longest word
    (while (re-search-forward columnize-regexp end t)
      (setq word-len (- (match-end 1) (match-beginning 1)))
      (if (> word-len max-len) (setq max-len word-len)))
    ;; Determine number and width of each pillar
    (if (= max-len 0) (error "No words detected"))
    (cond
     ((> pillar-width line-width)
      (setq line-width pillar-width))
     ((> pillar-width 1))          ;use as is
     ((> (- pillar-width) (/ line-width 2))
      (error "Pillars must be at least 2 columns wide."))
     ((< pillar-width 0)
      ;; I add columnize-spacing so that you get the same results when
      ;; specifying the number of columns as when the default chooses that
      ;; number of columns.  I think that's a reasonable expectation. 
      (setq pillar-width (/ (+ line-width columnize-spacing)
                            (- pillar-width))))
     (t (setq pillar-width (+ max-len columnize-spacing)))
    )
    (setq num-pillars (/ (+ line-width columnize-spacing) pillar-width))
    (if (= num-pillars 0) (setq num-pillars 1))
    ;; Check the limit of pillars a word can span
    (if (and (> num-pillars 1)
             (> columnize-span 0)
             (>= max-len (* pillar-width columnize-span)))
        (error "Words would span too many pillars."))
    ;; Create the reformatted text.
    (goto-char begin)
    (while (re-search-forward columnize-regexp end t)
      (setq word-len (- (match-end 1) (match-beginning 1))
            word-width (+ 1 (/ word-len pillar-width)))
      ;; Start a new line if needed
      (if (> (+ word-width current-pillar) num-pillars)
          (setq replacement (concat replacement "\n"
                                    (make-string first-column ? ))
                current-pillar 0))
      ;; Add this word to the replacement string being generated.
      (setq current-pillar (+ current-pillar word-width)
            pad-count (- (* pillar-width word-width) word-len)
            replacement (concat replacement (match-string 1)
                                (and (> pad-count 0)
                                     (make-string pad-count ? )))))
    ;; Replace the old region with the newly generated text
    (kill-region begin end)
    (insert replacement)))

(provide 'columnize)

;;; Local Variables: ***
;;; fill-column:78 ***
;;; emacs-lisp-docstring-fill-column:78 ***
;;; End: ***
;;; columnize.el ends here.
