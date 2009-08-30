;;; bidi.el --- bidi support for Emacs

;; Copyright (C) 2001  Alex Schroeder <alex@gnu.org>

;; Version: $Id: bidi.el,v 1.3 2001/11/21 11:59:58 alex Exp $
;; Keywords: wp
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?CategoryBiDi

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This is modelled after characters.el.  At the moment, however, we
;; don't have categories assigned, so we must create them ourselves.
;; The new categories are identified by a character, like all other
;; categories.  We store them in the following variables.

;; The existing categories and syntax tables are not enough to resolve
;; bidi issues: Some of these categories specify that the "real"
;; category must be determined from context.  See the Unicode Standard
;; Annex #9, available from http://www.unicode.org/unicode/reports/tr9/.

(defvar bidi-testing t
  "Non-nil will use bidi-table-test.el to setup up bidi categories.
Furthermore, while loading this file, some tests are run.")

(defvar bidi-category-l nil
  "Strong Left-to-Right: Most alphabetic, syllabic, Han ideographic
characters, digits that are neither European nor Arabic, all unassigned
characters except in the ranges (0590-05FF, FB1D-FB4F) and (0600-07BF,
FB50-FDFF, FE70-FEFF).")
(defvar bidi-category-lre nil
  "Strong Left-to-Right Embedding: LRE.")
(defvar bidi-category-lro nil
  "Strong Left-to-Right Override: LRO.")
(defvar bidi-category-r nil
  "Strong Right-to-Left: RLM, Hebrew alphabet, most punctuation specific
to that script, all unassigned characters in the ranges (0590-05FF,
FB1D-FB4F)")
(defvar bidi-category-al nil
  "Strong Right-to-Left Arabic: Arabic, Thaana, and Syriac alphabets,
most punctuation specific to those scripts, all unassigned characters in
the ranges (0600-07BF, FB50-FDFF, FE70-FEFF).")
(defvar bidi-category-rle nil
  "Strong Right-to-Left Embedding: RLE.")
(defvar bidi-category-rlo nil
  "Strong Right-to-Left Override: RLO.")
(defvar bidi-category-pdf nil
  "Weak Pop Directional Format: PDF.")
(defvar bidi-category-en nil
  "Weak European Number: European digits, Eastern Arabic-Indic digits.")
(defvar bidi-category-es nil
  "Weak European Number Separator: Solidus (Slash).")
(defvar bidi-category-et nil
  "Weak European Number Terminator: Plus Sign, Minus Sign, Degree,
Currency symbols.")
(defvar bidi-category-an nil
  "Weak Arabic Number: Arabic-Indic digits, Arabic decimal & thousands
separators.")
(defvar bidi-category-cs nil
  "Weak Common Number Separator: Colon, Comma, Full Stop (Period),
Non-breaking space.")
(defvar bidi-category-nsm nil
  "Weak Non-Spacing Mark: Characters marked Mn (Non-Spacing Mark) and Me
\(Enclosing Mark) in the Unicode Character Database.")
(defvar bidi-category-bn nil
  "Weak Boundary Neutral: Formatting and control characters, other than
those explicitly given types above. (These are to be ignored in
processing bidirectional text.)")
(defvar bidi-category-b nil
  "Neutral Paragraph Separator: Paragraph Separator, appropriate Newline
Functions, higher-protocol paragraph determination.")
(defvar bidi-category-s nil
  "Neutral Segment Separator: Tab")
(defvar bidi-category-ws nil
  "Neutral Whitespace: Space, Figure Space, Line Separator, Form Feed,
General Punctuation Spaces, ...")
(defvar bidi-category-on nil
  "Other Neutrals: All other characters, including OBJECT REPLACEMENT
CHARACTER.")
(defvar bidi-categories
  '(bidi-category-l
    bidi-category-lre
    bidi-category-lro
    bidi-category-r
    bidi-category-al
    bidi-category-rle
    bidi-category-rlo
    bidi-category-pdf
    bidi-category-en
    bidi-category-es
    bidi-category-et
    bidi-category-an
    bidi-category-cs
    bidi-category-nsm
    bidi-category-bn
    bidi-category-b
    bidi-category-s
    bidi-category-ws
    bidi-category-on)
  "List of categories variables used by bidi algorithms.
Each category variable holds a character.  This character
identifies the category.")

(defun bidi-setup-categories ()
  "Create new categories for bidi according to UAX#9.
This sets all the categories defined in `bidi-categories'."
  (let ((table (standard-category-table)))
    (mapc (lambda (var)
	      (let ((cat (get-unused-category table))
		    (doc (get var 'variable-documentation)))
		(when (symbol-value var)
		  (error "%S is already set" var))
		(unless cat
		  (error "No more unused categories available"))
		(set var cat)
		(define-category cat doc table)))
	    bidi-categories)))

;; Do category setup once.

(bidi-setup-categories)

(when bidi-testing
  ;; Fake category used in the test code.
  ;; UAX#9 also uses this in the examples.
  (defvar bidi-category-n bidi-category-ws
    "Neutral or Separator (B, S, WS, ON), not used as a category."))

;; Assign the categories to the characters.  Choose either the real
;; table or the testing table (for ASCII only and using capital letters
;; for right-to-left).

(if bidi-testing
    (load-file "bidi-table-test.el")
  (load-file "bidi-table.el"))

;; Here's how the translation will work for a string STR: First, produce
;; a string/array of bidi categories TYPES used in STR.  Then apply some
;; rules to TYPES and based on the result, invert selected parts of STR.
;; Note that we don't have explicit levels in visual-to-logical
;; transformation, so levels will be ignored.  Levels are important for
;; logical-to-visual transformation, ie. for display.  This is done
;; elsewhere.

;; Note that eventually we will also want to call this code to process
;; chunks of text such as received by comint.  For this to work we will
;; have to keep a data structure which remembers starting position of
;; the current embedding level.  When the level is zero, text can be
;; inserted and the starting position can be incrememted.  When the
;; level was non-zero and returns to zero, the entire text between the
;; starting position and the current position will have to be reversed
;; recursively.  The issues are not clear, yet.  FIXME

;; Note that some text in visual order may still contain the explicit
;; directional codes.  Wether these ought to be used while reversing is
;; not clear, yet.  FIXME

(defun bidi-get-category (char)
  "Return category for CHAR."
  (let ((categories bidi-categories)
	(set (char-category-set char))
	result)
    (while (and (not result) categories)
      (let ((category (symbol-value (car categories))))
	(if (aref set category)
	    (setq result category)
	  (setq categories (cdr categories)))))
    result))

(when bidi-testing
  (assert (eq bidi-category-l (bidi-get-category ?a)))
  (assert (eq bidi-category-r (bidi-get-category ?A))))

(defun bidi-get-types (str)
  "Return bidi categories for STR in a list."
  (mapcar 'bidi-get-category str))

(when bidi-testing
  (assert (string=
	   (string bidi-category-l
		   bidi-category-ws
		   bidi-category-r)
	   (apply 'string (bidi-get-types "a A")))))

(defun bidi-get-mnemonics (types)
  "Return mnemonics for all bidi categories in TYPES."
  (let ((table (mapcar
		(lambda (sym)
		  (cons (symbol-value sym)
			(intern
			 (upcase
			  (nth 2 (split-string
				  (symbol-name sym) "-"))))))
		       bidi-categories)))
    (mapcar (lambda (type)
	      (cdr (assq type table)))
	    types)))

(when bidi-testing
  (assert (equal
	   (bidi-get-mnemonics
	    (list bidi-category-l
		  bidi-category-ws
		  bidi-category-r))
	   '(L WS R))))

(defun bidi-apply-w1 (sor types)
  "Apply UAX#9 rule W1.
SOR is the start-of-level-run bidi type.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

Examine each non-spacing mark (NSM) in the level run, and change the
type of the NSM to the type of the previous character. If the NSM is at
the start of the level run, it will get the type of sor."
  ;; Skip this if no NSM types are found.
  (if (null (memq bidi-category-nsm types))
      types
    (let ((previous-type sor)
	  (lst types))
      (while lst
	(if (eq (car lst) bidi-category-nsm)
	    (setcar lst previous-type)
	  (setq previous-type (car lst)))
	(setq lst (cdr lst)))
      types)))

(when bidi-testing
  (assert (eq (bidi-apply-w1 nil nil) nil))
  (assert (equal (bidi-apply-w1
		  nil
		  (list bidi-category-al
			bidi-category-nsm
			bidi-category-nsm))
		 (list bidi-category-al
		       bidi-category-al
		       bidi-category-al)))
  (assert (equal (bidi-apply-w1
		  bidi-category-r
		  (list bidi-category-nsm))
		 (list bidi-category-r))))

(defun bidi-apply-w2 (sor types)
  "Apply UAX#9 rule W2.
SOR is the start-of-level-run bidi type.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

Search backwards from each instance of a European number until the first
strong type (R, L, AL, or sor) is found.  If an AL is found, change the type of
the European number to Arabic number."
  (if (null (memq bidi-category-en types))
      types
    (let* ((previous-type sor)
	   (lst types)
	   type)
      (while lst
	(setq type (car lst))
	(if (eq type bidi-category-en)
	    (when (eq previous-type bidi-category-al)
	      (setcar lst bidi-category-al))
	  (when (or (eq type bidi-category-r)
		    (eq type bidi-category-l)
		    (eq type bidi-category-al))
	    (setq previous-type type)))
	(setq lst (cdr lst)))
      types)))

(when bidi-testing
  (assert (equal (bidi-apply-w2
		  nil
		  (list bidi-category-al
			bidi-category-en))
		 (list bidi-category-al
		       bidi-category-al)))
  (assert (equal (bidi-apply-w2
		  nil
		  (list bidi-category-al
			bidi-category-n
			bidi-category-en))
		 (list bidi-category-al
		       bidi-category-n
		       bidi-category-al)))
  (assert (equal (bidi-apply-w2
		  bidi-category-r
		  (list bidi-category-n
			bidi-category-en))
		 (list bidi-category-n
		       bidi-category-en)))
  (assert (equal (bidi-apply-w2
		  nil
		  (list bidi-category-l
			bidi-category-n
			bidi-category-en))
		 (list bidi-category-l
		       bidi-category-n
		       bidi-category-en)))
  (assert (equal (bidi-apply-w2
		  nil
		  (list bidi-category-r
			bidi-category-n
			bidi-category-en))
		 (list bidi-category-r
		       bidi-category-n
		       bidi-category-en))))

(defun bidi-apply-replacement (list old new)
  "Replace all instances of OLD with NEW in LIST.
LIST will be modified in place."
  (let ((lst (memq old list)))
    (while lst
      (setcar lst new)
      (setq lst (memq old (cdr lst)))))
  list)

(when bidi-testing
  (assert (equal (bidi-apply-replacement '(1 2 3) 2 5)
		 '(1 5 3))))

(defun bidi-apply-w3 (types)
  "Apply UAX#9 rule W3.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

Change all ALs to R."
  (bidi-apply-replacement types bidi-category-al bidi-category-r))

(when bidi-testing
  (assert (equal (bidi-apply-w3
		  (list bidi-category-al
			bidi-category-en))
		 (list bidi-category-r
		       bidi-category-en)))
  (assert (equal (bidi-apply-w3
		  (list bidi-category-en
			bidi-category-al))
		 (list bidi-category-en
		       bidi-category-r)))
  (assert (equal (bidi-apply-w3
		  (list bidi-category-en))
		 (list bidi-category-en))))

(defun bidi-apply-w4 (types)
  "Apply UAX#9 rule W4.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

A single European separator between two European numbers changes to a
European number. A single common separator between two numbers of the
same type changes to that type."
  ;; I don't want to use string-match, here, because while the
  ;; categories are characters, they may have special meanings in
  ;; regexps, so I'd have to escape all special characters and then call
  ;; string-match.  That seems like too much overhead.
  (let ((lst (memq bidi-category-en types)))
    (while lst
      (when (and (or (eq (nth 1 lst) bidi-category-cs)
		     (eq (nth 1 lst) bidi-category-es))
		 (eq (nth 2 lst) bidi-category-en))
	(setq lst (cdr lst))
	(setcar lst bidi-category-en)
	(setq lst (cdr lst)))
      (setq lst (memq bidi-category-en (cdr lst)))))
  (let ((lst (memq bidi-category-an types)))
    (while lst
      (when (and (eq (nth 1 lst) bidi-category-cs)
		 (eq (nth 2 lst) bidi-category-an))
	(setq lst (cdr lst))
	(setcar lst bidi-category-an)
	(setq lst (cdr lst)))
      (setq lst (memq bidi-category-en (cdr lst)))))
  types)

(when bidi-testing
  (assert (equal (bidi-apply-w4
		  (list bidi-category-en
			bidi-category-es
			bidi-category-en))
		 (list bidi-category-en
		       bidi-category-en
		       bidi-category-en)))
  (assert (equal (bidi-apply-w4
		  (list bidi-category-en
			bidi-category-cs
			bidi-category-en))
		 (list bidi-category-en
		       bidi-category-en
		       bidi-category-en)))
  (assert (equal (bidi-apply-w4
		  (list bidi-category-an
			bidi-category-cs
			bidi-category-an))
		 (list bidi-category-an
		       bidi-category-an
		       bidi-category-an)))
  (assert (equal (bidi-apply-w4
		  (list bidi-category-en
			bidi-category-cs
			bidi-category-cs
			bidi-category-en))
		 (list bidi-category-en
		       bidi-category-cs
		       bidi-category-cs
		       bidi-category-en))))

(defun bidi-apply-w5-sub (types)
  "Change all ET following EN to EN.
Called by `bidi-apply-w5', once for the real list, once for the reversed list."
  (let ((lst (memq bidi-category-en types)))
    (while lst
      (setq lst (cdr lst))
      (while (and lst
		  (eq (car lst) bidi-category-et))
	(setcar lst bidi-category-en)
	(setq lst (cdr lst)))
      (setq lst (memq bidi-category-en lst))))
  types)

(defun bidi-apply-w5 (types)
  "Apply UAX#9 rule W5.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

A sequence of European terminators adjacent to European numbers changes
to all European numbers."
  ;; We use the following trick, here.  First we search for any ET
  ;; following EN, then we reverse the list, and repeat.
  (if (not (memq bidi-category-et types))
      types
    (setq types (bidi-apply-w5-sub types))
    (if (not (memq bidi-category-et types))
	types
      (nreverse (bidi-apply-w5-sub (nreverse types))))))

(when bidi-testing
  (assert (equal (bidi-apply-w5
		  (list bidi-category-et
			bidi-category-et
			bidi-category-en))
		 (list bidi-category-en
		       bidi-category-en
		       bidi-category-en)))
  (assert (equal (bidi-apply-w5
		  (list bidi-category-en
			bidi-category-et
			bidi-category-et))
		 (list bidi-category-en
		       bidi-category-en
		       bidi-category-en)))
  (assert (equal (bidi-apply-w5
		  (list bidi-category-an
			bidi-category-et
			bidi-category-en))
		 (list bidi-category-an
		       bidi-category-en
		       bidi-category-en))))

(defun bidi-apply-w6 (types)
  "Apply UAX#9 rule W6.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

Separators and terminators change to Other Neutral."
  ;; Difficult decision: Is it faster to search the list three times
  ;; using memq or is it faster to step through the list on our own?
  ;; Better to reuse code.  And it looks good.
  (bidi-apply-replacement
   (bidi-apply-replacement 
    (bidi-apply-replacement
     types
     bidi-category-cs bidi-category-on)
    bidi-category-es bidi-category-on)
   bidi-category-et bidi-category-on))

(when bidi-testing
  (assert (equal (bidi-apply-w6
		  (list bidi-category-an
			bidi-category-et))
		 (list bidi-category-an
		       bidi-category-on)))
  (assert (equal (bidi-apply-w6
		  (list bidi-category-l
			bidi-category-es
			bidi-category-en))
		 (list bidi-category-l
		       bidi-category-on
		       bidi-category-en)))
  (assert (equal (bidi-apply-w6
		  (list bidi-category-en
			bidi-category-cs
			bidi-category-an))
		 (list bidi-category-en
		       bidi-category-on
		       bidi-category-an)))
  (assert (equal (bidi-apply-w6
		  (list bidi-category-et
			bidi-category-an))
		 (list bidi-category-on
		       bidi-category-an))))

(defun bidi-apply-w7 (sor types)
  "Apply UAX#9 rule W7.
SOR is the start-of-level-run bidi type.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

Search backwards from each instance of a European number until the first
strong type (R, L, or sor) is found. If an L is found, then change the
type of the European number to L."
  ;; Looks very similar to `bidi-apply-w7'.  Does it make sense to
  ;; factor this out?  I don't think so.  That would be "method
  ;; stealing".
  (if (null (memq bidi-category-en types))
      types
    (let* ((previous-type sor)
	   (lst types)
	   type)
      (while lst
	(setq type (car lst))
	(if (eq type bidi-category-en)
	    (when (eq previous-type bidi-category-l)
	      (setcar lst bidi-category-l))
	  (when (or (eq type bidi-category-r)
		    (eq type bidi-category-l))
	    (setq previous-type type)))
	(setq lst (cdr lst)))
      types)))

(when bidi-testing
  (assert (equal (bidi-apply-w7
		  nil
		  (list bidi-category-l
			bidi-category-n
			bidi-category-en))
		 (list bidi-category-l
		       bidi-category-n
		       bidi-category-l)))
  (assert (equal (bidi-apply-w7
		  nil
		  (list bidi-category-r
			bidi-category-n
			bidi-category-en))
		 (list bidi-category-r
		       bidi-category-n
		       bidi-category-en))))

(defun bidi-apply-conversion (start-lst end-lst new)
  "Transform elements from START-LST to END-LST to NEW.
START-LST is a list, and END-LST is a sublist, a tail, in START-LST.
Every car in START-LST will then be changed to NEW until END-LST is
reached.  END-LST remains unmodified.  START-LST is modified in place."
  ;; A potential error check has been left out.  start-lst ought never
  ;; to be nil.
  (let ((start start-lst))
    (while (and start-lst
		(not (eq start-lst end-lst)))
      (setcar start-lst new)
      (setq start-lst (cdr start-lst)))
    start))

(when bidi-testing
  (assert (equal
	   (let* ((lst '(a b c d e))
		  (end (nthcdr 3 lst)))
	     (bidi-apply-conversion lst end 'foo))
	   '(foo foo foo d e))))

(defun bidi-apply-n1 (sor eor types)
  "Apply UAX#9 rule N1.
SOR is the start-of-level-run bidi type.
EOR is the end-of-level-run bidi type.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

A sequence of neutrals takes the direction of the surrounding strong
text if the text on both sides has the same direction. European and
Arabic numbers are treated as though they were R. Start-of-level-run
\(sor) and end-of-level-run \(eor) are used at level run boundaries."
  ;; The idea is this: We step through the list.  start-lst points to
  ;; the last strong type we encountered.  This marks the start of a
  ;; "stretch of neutrals".  lst points to the current position in the
  ;; list.  The elements between start-lst and lst may be switched.
  (let ((start-lst types)
	(lst types)
	(start-type sor)
	(type (car types)))
    (while lst
      (cond ((eq type bidi-category-l); potential end or start of L stretch
	     (if (eq start-type bidi-category-l)
		 (bidi-apply-conversion start-lst lst bidi-category-l)
	       (setq start-type bidi-category-l))
	     (setq start-lst (cdr lst)))
	    ((or (eq type bidi-category-r); potential end or start of R stretch
		 (eq type bidi-category-en)
		 (eq type bidi-category-an))
	     (if (eq start-type bidi-category-r)
		 (bidi-apply-conversion start-lst lst bidi-category-r)
	       (setq start-type bidi-category-r))
	     (setq start-lst (cdr lst)))
	    ;; is the following clause ever needed?
	    ((not (or (eq type bidi-category-b); definitely not part of a stretch
		      (eq type bidi-category-s)
		      (eq type bidi-category-ws)
		      (eq type bidi-category-on)))
	     (setq start-type nil
		   start-lst nil)))
      (setq lst (cdr lst)
	    type (car lst)))
    ;; final stretch
    (when (and start-lst
	       (eq start-type eor))
      (bidi-apply-conversion start-lst nil start-type)))
  types)

(when bidi-testing
  (assert (equal (bidi-apply-n1
		  nil nil
		  (list bidi-category-r
			bidi-category-n
			bidi-category-r))
		 (list bidi-category-r
		       bidi-category-r
		       bidi-category-r)))
  (assert (equal (bidi-apply-n1
		  nil nil
		  (list bidi-category-l
			bidi-category-n
			bidi-category-l))
		 (list bidi-category-l
		       bidi-category-l
		       bidi-category-l)))
  (assert (equal (bidi-apply-n1
		  nil nil
		  (list bidi-category-r
			bidi-category-n
			bidi-category-an))
		 (list bidi-category-r
		       bidi-category-r
		       bidi-category-an)))
  (assert (equal (bidi-apply-n1
		  nil nil
		  (list bidi-category-an
			bidi-category-n
			bidi-category-r))
		 (list bidi-category-an
		       bidi-category-r
		       bidi-category-r)))
  (assert (equal (bidi-apply-n1
		  nil nil
		  (list bidi-category-r
			bidi-category-n
			bidi-category-en))
		 (list bidi-category-r
		       bidi-category-r
		       bidi-category-en)))
  (assert (equal (bidi-apply-n1
		  nil nil
		  (list bidi-category-en
			bidi-category-n
			bidi-category-r))
		 (list bidi-category-en
		       bidi-category-r
		       bidi-category-r))))

(defun bidi-apply-n2 (e types)
  "Apply UAX#9 rule N2.
TYPES is a list of bidi types to operate on.
E is the embedding level to use.
TYPES will be modified in place.

Any remaining neutrals take the embedding direction."
  ;; Looks very similar to `bidi-apply-w6'.
  (bidi-apply-replacement
   (bidi-apply-replacement 
    (bidi-apply-replacement
     (bidi-apply-replacement
      types
      bidi-category-b e)
     bidi-category-s e)
    bidi-category-ws e)
   bidi-category-on e))

;; Note that the following tests provided for the N2 rule in UAX#9 seem
;; to work only if both N1 and N2 are applied.
;; 
;; From UAX#9 (skipped in the test code below):
;; "Assume in this example that eor is L, and sor is R:
;;  L N eor => L L eor
;;  sor N R => sor R R"
(when bidi-testing
  (assert (equal (bidi-apply-n2
		  bidi-category-r
		  (list bidi-category-r
			bidi-category-n
			bidi-category-l))
		 (list bidi-category-r
		       bidi-category-r
		       bidi-category-l)))
  (assert (equal (bidi-apply-n2
		  bidi-category-l
		  (list bidi-category-r
			bidi-category-n
			bidi-category-l))
		 (list bidi-category-r
		       bidi-category-l
		       bidi-category-l))))

(defun bidi-resolve-weak-types (types &optional r2l-context)
  "Resolve weak bidi types in TYPES, a list of bidi categories.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

This uses rules W1 to W7 and rules N1 and N2 from UAX#9.

Since this function doesn't take care of explicit codes, all types are
considered to be at the same level.  If you decomposed a string into
stretches of differing embedding levels, you can call this function for
every such stretch.

The resulting list will have the same number of elements and all weak
types will be replaced by strong types.  If the optional argument
R2L-CONTEXT is given, the context will be right-to-left.  If R2L-CONTEXT
is nil, a left-to-right context will be assumed.

In UAX#9 parlance: Each paragraph has a text ordering type E.  If
R2L-CONTEXT is given, E is set to R, otherwise E is set to L.
Furthermore, we set the start-of-level-run type SOR and the
end-of-level-run type EOR to E.  E, SOR, and EOR are used for rules W1,
W2, W7, N1, and N2.

For more information in the various rules, see the functions:
 `bidi-apply-w1'
 `bidi-apply-w2'
 `bidi-apply-w3'
 `bidi-apply-w4'
 `bidi-apply-w5'
 `bidi-apply-w6'
 `bidi-apply-w7'
 `bidi-apply-n1'
 `bidi-apply-n2'"
  (let* ((e (if r2l-context bidi-category-r bidi-category-l))
	 (sor e)
	 (eor e))
    (bidi-apply-n2 e
     (bidi-apply-n1 sor eor
      (bidi-apply-w7 sor
       (bidi-apply-w6
	(bidi-apply-w5
	 (bidi-apply-w4
	  (bidi-apply-w3
	   (bidi-apply-w2 sor
	    (bidi-apply-w1 sor types)))))))))))

(when bidi-testing
  (assert (equal
	   (bidi-resolve-weak-types
	    (bidi-get-types "he said \"THE VALUES ARE 123, 456, 789, OK\"."))
	   (nconc
	    (make-list 9 bidi-category-l)
	    (make-list 15 bidi-category-r)
	    (make-list 3 bidi-category-en)
	    (make-list 2 bidi-category-r)
	    (make-list 3 bidi-category-en)
	    (make-list 2 bidi-category-r)
	    (make-list 3 bidi-category-en)
	    (make-list 4 bidi-category-r)
	    (make-list 2 bidi-category-l)))))

(defun bidi-resolve-implicit-levels (types &optional levels)
  "Resolve the implicit levels in TYPES.
TYPES is a list of bidi types to operate on.
LEVELS is an optional list of explicit levels.  If it is provided,
the list should have the same length as TYPES.
LEVELS will be modified in place, if it is provided.

This uses rules I1 and I2 from UAX#9.

For all characters with an even (left-to-right) embedding direction,
those of type R go up one level and those of type AN or EN go up two
levels.  For all characters with an odd (right-to-left) embedding
direction, those of type L, EN or AN go up one level."
  (setq levels (or levels (make-list (length types) 0)))
  (let ((result levels))
    (while types
      (let* ((type (car types))
	     (level (car levels)))
	(if (evenp level)
	    ;; I1: even (left-to-right) embedding direction
	    (cond ((eq type bidi-category-r)
		   (setcar levels (1+ level)))
		  ((or (eq type bidi-category-an)
		       (eq type bidi-category-en))
		   (setcar levels (+ 2 level))))
	  ;; I2: odd (right-to-left) embedding direction
	  (when (or (eq type bidi-category-l)
		    (eq type bidi-category-en)
		    (eq type bidi-category-an))
	    (setcar levels (1+ level))))
	(setq types (cdr types)
	      levels (cdr levels))))
    result))

(when bidi-testing
  (assert (equal
	   (bidi-resolve-implicit-levels
	    (bidi-resolve-weak-types
	     (bidi-get-types "car means CAR.")))
	   '(0 0 0 0 0 0 0 0 0 0 1 1 1 0)))
  ;; The number embedding works.
  (assert (equal
	   (bidi-resolve-implicit-levels
	    (bidi-resolve-weak-types
	     (bidi-get-types "he said \"THIS IS 123, RIGHT?\".")))
	   '(0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 0 0 0)))
  (assert (equal
	   (bidi-resolve-implicit-levels
	    (bidi-resolve-weak-types
	     (bidi-get-types "car MEANS CAR."))
	    (make-list (length "car MEANS CAR.") 1))
	   ;; FIXME: is the 2 at the end wrong?
	   '(2 2 2 2 1 1 1 1 1 1 1 1 1 2))))

(defun bidi-find-pos (list elem &optional invert)
  "Return position of element ELEM in LIST.
With optional argument INVERT, return the position of the first element
which is not eq to ELEM."
  (let ((pos 0)
	result)
    (while list
      (if (or (and (not invert) (eq (car list) elem))
	      (and invert (not (eq (car list) elem))))
	  (setq list nil
		result pos)
	(setq pos (1+ pos)))
      (setq list (cdr list)))
    result))

(when bidi-testing
  (assert (eq 2 (bidi-find-pos '(0 0 2 3 4) 0 t)))
  (assert (eq 2 (bidi-find-pos '(0 1 2 3 4) 2)))
  (assert (eq 2 (car (nthcdr 2  '(0 1 2 3 4))))))

(defun bidi-apply-l2 (str levels &optional limit)
  "Apply UAX#9 rule L2.
STR is the string to reorder.
LEVELS is a list of resolved levels.
Both STR and LEVELS must have the same length.

The optional argument LIMIT prevents the reversal of characters on level
LIMIT and below.

From the highest level found in the text to the lowest odd level on each
line, reverse any contiguous sequence of characters that are at that
level or higher."
  ;; Here's the idea: We will work with lists instead of strings and use
  ;; nreverse at will.
  (setq limit (or limit 0))
  (let* ((orig-levels levels)
	 (level (apply 'max levels))
	 (char-list (append '(foo) str nil))
	 (head (cdr char-list))
	 stretch tail
	 (pos (bidi-find-pos levels level)))
    (while (> level limit)
      ;; Find beginning of stretch.  The stuff before is the head.  It
      ;; must be separated from the stretch.  Keep the list of levels up
      ;; to date.  Take care when the stretch starts right at the
      ;; beginning.
      (while pos
	(setq levels (nthcdr pos levels)
	      stretch (nthcdr pos head))
	;; Separate head from body.
	(if (> pos 0)
	    (setcdr (nthcdr (1- pos) head) nil)
	  ;; When there is no head, the stretch starts at the first
	  ;; character.  This can only happen at the beginning of str,
	  ;; because later, this would require two adjacent stretches
	  ;; with the same level.  Those would be treated as one
	  ;; contiguous stretch, therefore this can't happen.  We can't
	  ;; set head to nil, because then we would only keep the last
	  ;; character of the stretch (a problem with nconc futher
	  ;; down).  That is why we can point head to the beginning of
	  ;; char-list.
	  (setq head char-list)
	  (setcdr head nil))
	;; Determine where stretch ends.  The stuff after the stretch is
	;; the tail.  The stretch must be separated from the tail.  Keep
	;; the list of levels up to date.  Take care when the stretch
	;; covers the entire rest of the list.
	(setq pos (bidi-find-pos levels level t))
	(if (not pos)
	    (setq levels nil
		  tail nil)
	  (setq levels (nthcdr pos levels)
		tail (nthcdr pos stretch))
	  (setcdr (nthcdr (1- pos) stretch) nil))
	;; Reverse stretch
	(setq stretch (nreverse stretch))
	;; Stitch them together again
	(nconc head stretch tail)
	;; Prepare next round
	(setq head tail
	      pos (bidi-find-pos levels level)))
      ;; Decrease level and repeat
      (let ((new-level (1- level)))
	;; make sure new level actually exists
	(while (and (not (memq new-level orig-levels))
		    (> new-level 0))
	  (setq new-level (1- new-level)))
	;; Reset lists and start again
	(setq levels (bidi-apply-replacement orig-levels level new-level)
	      level new-level
	      head (cdr char-list)
	      pos (bidi-find-pos levels level))))
    ;; Return a string
    (apply 'string (cdr char-list))))

(when bidi-testing
  (assert (string=
	   (bidi-apply-l2
	    "car means CAR."
	    '(0 0 0 0 0 0 0 0 0 0 1 1 1 0))
	   "car means RAC."))
  (assert (string=
	   (bidi-apply-l2
	    "it means THIS and THAT."
	    (bidi-resolve-implicit-levels
	     (bidi-resolve-weak-types
	      (bidi-get-types "it means THIS and THAT."))))
	   "it means SIHT and TAHT."))
  (assert (string=
	   (bidi-apply-l2 
	    "he said \"THIS IS 123, RIGHT?\"."
	    (bidi-resolve-implicit-levels
	     (bidi-resolve-weak-types
	      (bidi-get-types "he said \"THIS IS 123, RIGHT?\"."))))
	   "he said \"THGIR ,123 SI SIHT?\"."))
  (assert (string=
	   (bidi-apply-l2 
	    "FOO is a word for FOOLS."
	    (bidi-resolve-implicit-levels
	     (bidi-resolve-weak-types
	      (bidi-get-types "FOO is a word for FOOLS."))))
	   "OOF is a word for SLOOF.")))

(defun bidi-get-mirror-char (char)
  "Return the mirror character of CHAR or nil if there is none.
See `bidi-mirroring-table'."
  (cdr (assq char bidi-mirroring-table)))

(when bidi-testing
  (assert (eq ?\) (bidi-get-mirror-char ?\()))
  (assert (eq ?\] (bidi-get-mirror-char ?\[))))

(defun bidi-apply-l4 (str levels)
  "Replace inverted glyphs with their mirrors if necessary.
STR is the string to reorder.  STR will be modified in place.
LEVELS is a list of resolved levels.
Both STR and LEVELS must have the same length.

This uses rule L4 from UAX#9.

A character that possesses the mirrored property as specified by Section
4.7, Mirrored, must be depicted by a mirrored glyph if the resolved
directionality of that character is R."
  (let ((i 0)
	(len (length str))
	mirror-char)
    (while (< i len)
      (setq mirror-char (bidi-get-mirror-char (aref str i)))
      (when (and mirror-char (oddp (nth i levels)))
	(aset str i mirror-char))
      (setq i (1+ i)))
    str))

(when bidi-testing
  (assert (string= "(abc)" (bidi-apply-l4 "(abc)" '(0 0 0 0 0))))
  (assert (string= ")abc(" (bidi-apply-l4 "(abc)" '(1 1 1 1 1))))
  (assert (string= "(abc(" (bidi-apply-l4 "(abc)" '(0 0 1 1 3)))))

(defun bidi-reorder-string (str levels)
  "Reorder string STR according to resolved LEVELS.
Both STR and LEVELS must have the same length.

This uses rules L2 and L4 from UAX#9.  Note that we apply rule L4
before we apply L2 because after applying L2, STR and LEVELS will
no longer by in sync.

For more information in the various rules, see the functions
`bidi-apply-l2' and `bidi-apply-l4'."
  (bidi-apply-l2
   (bidi-apply-l4 str levels)
   levels))

;; Some reversibility tests
(when bidi-testing
  (assert (string=
	   (bidi-reorder-string
	    "FOO is a word for FOOLS."
	    (bidi-resolve-implicit-levels
	     (bidi-resolve-weak-types
	      (bidi-get-types "FOO is a word for FOOLS."))))
	   "OOF is a word for SLOOF."))
  (assert (string=
	   (bidi-reorder-string
	    "OOF is a word for SLOOF."
	    (bidi-resolve-implicit-levels
	     (bidi-resolve-weak-types
	      (bidi-get-types "OOF is a word for SLOOF."))))
	   "FOO is a word for FOOLS."))
  (assert (string=
	   (bidi-reorder-string
	    "I SAW A f14 FLYING BY."
	    (bidi-resolve-implicit-levels
	     (bidi-resolve-weak-types
	      (bidi-get-types "I SAW A f14 FLYING BY."))))
	   "A WAS I f14 YB GNIYLF."))
  (assert (string=
	   (bidi-reorder-string
	    "he said \"I AM GOING TO london TODAY\"."
	    (bidi-resolve-implicit-levels
	     (bidi-resolve-weak-types
	      (bidi-get-types "he said \"I AM GOING TO london TODAY\"."))))
	   "he said \"OT GNIOG MA I london YADOT\"."))
  (assert (string=
	   (let ((str "this is a normal paragraph containing some
words in ARAB and HEBREW.  these strings need to be
reverted.  numbers like 123 should work, even when
ENCLOSED IN ARAB TEXT SUCH AS 4567.89 IS ON THIS LINE."))
	     (bidi-reorder-string
	      str
	      (bidi-resolve-implicit-levels
	       (bidi-resolve-weak-types
		(bidi-get-types str)))))
	   "this is a normal paragraph containing some
words in BARA and WERBEH.  these strings need to be
reverted.  numbers like 123 should work, even when
ENIL SIHT NO SI 4567.89 SA HCUS TXET BARA NI DESOLCNE.")))

(defun bidi-apply-p2 (str)
  "Apply UAX#9 rule P2.
Return the type of the first character in STR of bidi type L, AL, or R.

In each paragraph, find the first character of type L, AL, or R."
  ;; Note that this function doesn't take a TYPES argument.  The reason
  ;; is this: Each line is reordered on its own in
  ;; `bidi-reorder-string-1'.  This is where TYPES will be computed.
  ;; The paragraph embedding level is determined by paragraph, not by
  ;; line.  Therefore it cannot be recomputed for every line.  At the
  ;; same time, we cannot just use the first line -- the first call to
  ;; `bidi-reorder-string-1', because the string might contain no strong
  ;; types on the first line (example: "...\ntest\n").  Therefore, we
  ;; could either rewrite `bidi-reorder-string' such that 1. TYPES are
  ;; computed once for rule P2 and once again for every line, or
  ;; 2. TYPES are computed once and then, when the string is split into
  ;; lines, TYPES is split along the same boundaries, or 3. we don't use
  ;; TYPES for rule P2.  That's why we use STR for rule P2.
  (let (type
	result
	(i 0)
	(len (length str)))
    (while (and (not result)
		(< i len))
      (setq type (bidi-get-category (aref str i)))
      (if (or (= type bidi-category-l)
		(= type bidi-category-r)
		(= type bidi-category-al))
	  (setq result type)
	(setq i (1+ i))))
    result))

(when bidi-testing
  (assert (= bidi-category-r (bidi-apply-p2 "-- CAR")))
  (assert (= bidi-category-l (bidi-apply-p2 "this is a CAR")))
  (assert (= bidi-category-r (bidi-apply-p2 "CAR is this"))))

(defun bidi-apply-p3 (type) 
  "Apply UAX#9 rule P3.
Return 1 if TYPE is of bidi type AL or R; otherwise return 0.

If a character is found in P2 and it is of type AL or R, then set the
paragraph embedding level to one\; otherwise, set it to zero."
  ;; (not (= type bidi-category-l))
  (if (or (= type bidi-category-r)
	  (= type bidi-category-al))
      1
    0))

(when bidi-testing
  (assert (= 0 (bidi-apply-p3 bidi-category-l)))
  (assert (= 1 (bidi-apply-p3 bidi-category-al)))
  (assert (= 1 (bidi-apply-p3 bidi-category-r))))

;; FIXME: There is no function `bidi-resolve-explicit-levels'.  Such a
;; thing would be needed for true logical-to-visual transformation.  If
;; such a thing existed, then it could be used to present a better
;; LEVELS list to `bidi-reorder-string'.

;;; Entry points -- important top-level stuff

(defun bidi-reorder-1 (str paragraph-level)
  "Reorder STR, where STR does not contain newlines.
PARAGRAPH-LEVEL indicates the bidi context of STR.  See `bidi-reorder'."
  (bidi-reorder-string
   str
   (bidi-resolve-implicit-levels
    (bidi-resolve-weak-types (bidi-get-types str) (oddp paragraph-level))
    (make-list (length str) paragraph-level))))

;;;###autoload
(defun bidi-reorder (str &optional context)
  "Reorder STR line by line.
What UAX#9 calls a paragraph is actually a line in Emacs because Emacs
doesn't store the text of a paragraph in one long line.  Instead of
applying rule P1, therefore, we reorder STR line by line.

Optional argument CONTEXT indicates wether STR is from a left-to-right
context, from a right-to-left context, or wether STR stands on its own.
This is used to find the paragraph embedding level.  The possible values
of CONTEXT are:

 nil -- embedding is guessed using the first strong character in STR
 L2R -- left-to-right embedding is assumed
 R2L -- right-to-left embedding is assumed"
  (let ((paragraph-level
	 (cond ((not context)
		(bidi-apply-p3
		 (bidi-apply-p2 str)))
	       ((eq context 'L2R)
		0)
	       ((eq context 'R2L)
		1)
	       ((integerp context); undocumented feature
		context)
	       (t (error "Illegal bidi context: %S" context)))))
    (mapconcat (function identity)
	       (mapcar (lambda (s)
			 (bidi-reorder-1 s paragraph-level))
		       (split-string str "\n"))
	       "\n")))

(when bidi-testing
  (assert (string=
	   (bidi-reorder "THIS IS EITHER ARABIC OR HEBREW AND SHOULD ALL BE INVERTED.  CURRENTLY
IT IS NOT REALLY AS JUSTIFIED AS IT COULD BE.  WE'LL LOOK AT THIS
LATER.  cheers, alex." 'L2R)
	   "YLTNERRUC  .DETREVNI EB LLA DLUOHS DNA WERBEH RO CIBARA REHTIE SI SIHT
SIHT TA KOOL LL'EW  .EB DLUOC TI SA DEIFITSUJ SA YLLAER TON SI TI
RETAL.  cheers, alex."))
  (assert (string=
	   (bidi-reorder "this is either arabic or hebrew and should all be inverted.  currently
it is not really as justified as it could be.  we'll look at this
later.  CHEERS, ALEX.")
	   "this is either arabic or hebrew and should all be inverted.  currently
it is not really as justified as it could be.  we'll look at this
later.  XELA ,SREEHC.")))

;;;###autoload
(defun bidi-logical-to-visual (str &optional context)
  "Reorder STR line by line.
See `bidi-reorder' for a description of CONTEXT."
  ;; This is just a wrapper.  It makes sense to guess context based on
  ;; the first character, if STR is at the beginning of a paragraph.
  ;; Otherwise, it makes sense to provide CONTEXT.  So all this wrapper
  ;; does is make direction (logical to visual) explicit.
  (bidi-reorder str context))

(when bidi-testing
  ;; The following are from the PGBA page
  (assert (string= (bidi-logical-to-visual "car is THE CAR in arabic")
		   "car is RAC EHT in arabic"))
  (assert (string= (bidi-logical-to-visual "CAR IS the car IN ENGLISH")
		   "HSILGNE NI the car SI RAC"))
  (assert (string= (bidi-logical-to-visual "he said \"IT IS 123, 456, OK\"")
		   "he said \"KO ,456 ,123 SI TI\""))
  (assert (string= (bidi-logical-to-visual "he said \"IT IS (123, 456), OK\"")
		   "he said \"KO ,(456 ,123) SI TI\""))
  (assert (string= (bidi-logical-to-visual "he said \"IT IS 123,456, OK\"")
		   "he said \"KO ,123,456 SI TI\""))
  (assert (string= (bidi-logical-to-visual "he said \"IT IS (123,456), OK\"")
		   "he said \"KO ,(123,456) SI TI\""))
  (assert (string= (bidi-logical-to-visual "HE SAID \"it is 123, 456, ok\"")
		   "\"it is 123, 456, ok\" DIAS EH"))
  (assert (string= (bidi-logical-to-visual "<H123>shalom</H123>")
		   "<123H/>shalom<123H>"))
  (assert (string= (bidi-logical-to-visual "<h123>SAALAM</h123>")
		   "<h123>MALAAS</h123>"))
  (assert (string= (bidi-logical-to-visual "HE SAID \"it is a car!\" AND RAN")
		   "NAR DNA \"!it is a car\" DIAS EH"))
  (assert (string= (bidi-logical-to-visual "HE SAID \"it is a car!x\" AND RAN")
		   "NAR DNA \"it is a car!x\" DIAS EH"))
  (assert (string= (bidi-logical-to-visual "-2 CELSIUS IS COLD")
		   "DLOC SI SUISLEC -2"))
  (assert (string= (bidi-logical-to-visual "-10% CHANGE")
		   "EGNAHC -10%"))
  (assert (string= (bidi-logical-to-visual "SOLVE 1*5 1-5 1/5 1+5")
		   ;; This is the PGBA result, not the Unicode Reference
		   ;; Implementation
		   "1+5 1/5 1-5 5*1 EVLOS"))
  (assert (string= (bidi-logical-to-visual "THE RANGE IS 2.5..5")
		   "5..2.5 SI EGNAR EHT"))
  (assert (string= (bidi-logical-to-visual "he said \"IT IS A CAR!\"")
		   "he said \"RAC A SI TI!\""))
  (assert (string= (bidi-logical-to-visual "he said \"IT IS A CAR!X\"")
		   "he said \"X!RAC A SI TI\""))
  (assert (string= (bidi-logical-to-visual "(TEST) abc")
		   "abc (TSET)"))
  (assert (string= (bidi-logical-to-visual "abc (TEST)")
		   "abc (TSET)"))
  (assert (string= (bidi-logical-to-visual "#@$ TEST")
		   "TSET $@#"))
  (assert (string= (bidi-logical-to-visual "TEST 23 ONCE abc")
		   "abc ECNO 23 TSET"))
  (assert (string= (bidi-logical-to-visual "TEST ~~~23%%% ONCE abc")
		   "abc ECNO 23%%%~~~ TSET"))
  (assert (string= (bidi-logical-to-visual "TEST abc ~~~23%%% ONCE abc")
		   "abc ECNO abc ~~~23%%% TSET"))
  (assert (string= (bidi-logical-to-visual "TEST abc@23@cde ONCE")
		   "ECNO abc@23@cde TSET"))
  (assert (string= (bidi-logical-to-visual "TEST abc 23 cde ONCE")
		   "ECNO abc 23 cde TSET"))
  (assert (string= (bidi-logical-to-visual "TEST abc 23 ONCE cde")
		   "cde ECNO abc 23 TSET"))
  (assert (string= (bidi-logical-to-visual "Xa 2 Z")
		   "Z a 2X")))

;;;###autoload
(defun bidi-visual-to-logical (str context)
  "Reorder STR line by line.
See `bidi-reorder' for a description of CONTEXT."
  ;; This is just a wrapper.  It doesn't make sense to guess context
  ;; based on the first character, so all this wrapper does is make
  ;; direction (visual to logical) and context (argument required)
  ;; explicit.
  (bidi-reorder str context))

(when bidi-testing
  ;; The following are from the PGBA page
  (assert (string= (bidi-visual-to-logical "car is RAC EHT in arabic" 'L2R)
		   "car is THE CAR in arabic"))
  (assert (string= (bidi-visual-to-logical "HSILGNE NI the car SI RAC" 'R2L)
		   "CAR IS the car IN ENGLISH"))
  (assert (string= (bidi-visual-to-logical "he said \"KO ,456 ,123 SI TI\"" 'L2R)
		   "he said \"IT IS 123, 456, OK\""))
  (assert (string= (bidi-visual-to-logical "he said \"KO ,(456 ,123) SI TI\"" 'L2R)
		   "he said \"IT IS (123, 456), OK\""))
  (assert (string= (bidi-visual-to-logical "he said \"KO ,123,456 SI TI\"" 'L2R)
		   "he said \"IT IS 123,456, OK\""))
  (assert (string= (bidi-visual-to-logical "he said \"KO ,(123,456) SI TI\"" 'L2R)
		   "he said \"IT IS (123,456), OK\""))
  (assert (string= (bidi-visual-to-logical "\"it is 123, 456, ok\" DIAS EH" 'R2L)
		   "HE SAID \"it is 123, 456, ok\""))
  (ignore (string= (bidi-visual-to-logical "<123H/>shalom<123H>" 'R2L);; FIXME
		   "<H123>shalom</H123>"))
  (assert (string= (bidi-visual-to-logical "<h123>MALAAS</h123>" 'L2R)
		   "<h123>SAALAM</h123>"))
  (assert (string= (bidi-visual-to-logical "NAR DNA \"!it is a car\" DIAS EH" 'R2L)
		   "HE SAID \"it is a car!\" AND RAN"))
  (assert (string= (bidi-visual-to-logical "NAR DNA \"it is a car!x\" DIAS EH" 'R2L)
		   "HE SAID \"it is a car!x\" AND RAN"))
  (assert (string= (bidi-visual-to-logical "DLOC SI SUISLEC -2" 'R2L)
		   "-2 CELSIUS IS COLD"))
  (assert (string= (bidi-visual-to-logical "EGNAHC -10%" 'R2L)
		   "-10% CHANGE"))
  (assert (string= (bidi-visual-to-logical "1+5 1/5 1-5 5*1 EVLOS" 'R2L)
		   "SOLVE 1*5 1-5 1/5 1+5"))
  (assert (string= (bidi-visual-to-logical "5..2.5 SI EGNAR EHT" 'R2L)
		   "THE RANGE IS 2.5..5"))
  (assert (string= (bidi-visual-to-logical "he said \"RAC A SI TI!\"" 'L2R)
		   "he said \"IT IS A CAR!\""))
  (assert (string= (bidi-visual-to-logical "he said \"X!RAC A SI TI\"" 'L2R)
		   "he said \"IT IS A CAR!X\""))
  (assert (string= (bidi-visual-to-logical "abc (TSET)" 'R2L)
		   "(TEST) abc"))
  (assert (string= (bidi-visual-to-logical "abc (TSET)" 'L2R)
		   "abc (TEST)"))
  (assert (string= (bidi-visual-to-logical "TSET $@#" 'R2L)
		   "#@$ TEST"))
  (assert (string= (bidi-visual-to-logical "abc ECNO 23 TSET" 'R2L)
		   "TEST 23 ONCE abc"))
  (assert (string= (bidi-visual-to-logical "abc ECNO 23%%%~~~ TSET" 'R2L)
		   "TEST ~~~23%%% ONCE abc"))
  (assert (string= (bidi-visual-to-logical "abc ECNO abc ~~~23%%% TSET" 'R2L)
		   "TEST abc ~~~23%%% ONCE abc"))
  (assert (string= (bidi-visual-to-logical "ECNO abc@23@cde TSET" 'R2L)
		   "TEST abc@23@cde ONCE"))
  (assert (string= (bidi-visual-to-logical "ECNO abc 23 cde TSET" 'R2L)
		   "TEST abc 23 cde ONCE"))
  (assert (string= (bidi-visual-to-logical "cde ECNO abc 23 TSET" 'R2L)
		   "TEST abc 23 ONCE cde"))
  (assert (string= (bidi-visual-to-logical "Z a 2X" 'R2L)
		   "Xa 2 Z")))

;;;###autoload
(defun bidi-reorder-region (start end &optional context)
  "Reorder the region between START and END.
See `bidi-reorder' for a description of CONTEXT."
  (interactive "r")
  (let ((str (buffer-substring start end)))
    (delete-region start end)
    (goto-char start)
    (insert
     (bidi-reorder str context))))

;;;###autoload
(defun bidi-visual-to-logical-paragraph (context)
  "Reorder the current paragraph.
See `bidi-reorder' for a description of CONTEXT."
  ;; This is just a wrapper.  It doesn't make sense to guess context
  ;; based on the first character, so all this wrapper does is make
  ;; direction (visual to logical) and context (argument required)
  ;; explicit.
  (interactive (let ((answer (completing-read
			      "Context: "
			      '(("left-to-right") 
				("right-to-left")))))
		 (if (eq (intern answer) 'left-to-right)
		     '(L2R)
		   '(R2L))))
  (let ((start (save-excursion
		 (backward-paragraph)
		 (forward-char)
		 (point)))
	(end (save-excursion
	       (forward-paragraph)
	       (backward-char)
	       (point))))
    (bidi-reorder-region start end context)))

;;;###autoload
(defun bidi-logical-to-visual-paragraph ()
  "Reorder the current paragraph."
  ;; This is just a wrapper.  Since we start with a new paragraph, the
  ;; context is guessed based on the first strong character.  So all
  ;; this wrapper does is make direction (logical to visual) explicit,
  ;; and it forces a guess.
  (interactive)
  (let ((start (save-excursion
		 (backward-paragraph)
		 (forward-char)
		 (point)))
	(end (save-excursion
	       (forward-paragraph)
	       (backward-char)
	       (point))))
    (bidi-reorder-region start end)))

;; The functions above should work for the following paragraph as well:

;; this is a normal paragraph containing some
;; words in ARAB and HEBREW.  these strings need to be
;; reverted.  numbers like 123 should work, even when
;; ENCLOSED IN ARAB TEXT SUCH AS 4567.89 IS ON THIS LINE
;; AND ON THE NEXT.

;; Debugging aids

(defun bidi-pretty-print (object)
  "Pretty print OBJECT.
OBJECT may be a string, a list of characters, a list of integers,
or a list of symbols."
  (apply 'concat
	 (mapcar (lambda (c)
		   (cond ((symbolp c)
			  (format "%4s" c))
			 ((< c 32)
			  (format "%4d" c))
			 ((= c 34)
			  "  \"")
			 (t
			  (format "%4c" c))))
		 object)))

(when bidi-testing
  (assert (string= (bidi-pretty-print '(1 2 3 4 5))
		   "   1   2   3   4   5"))
  (assert (string= (bidi-pretty-print '(R L NSM WS))
		   "   R   L NSM  WS"))
  (assert (string= (bidi-pretty-print "foo")
		   "   f   o   o"))
  (assert (string= (bidi-pretty-print "\"foo")
		   "  \"   f   o   o")))

;;; bidi.el ends here
