;;; bidi-table-test.el --- test bidi type categories for Emacs -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001  Alex Schroeder <alex@gnu.org>

;; Version: $Id: bidi-table-test.el,v 1.2 2001/11/21 11:49:44 alex Exp $
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

;; This is a temporary table using only the first 128 characters from
;; UCS -- ie. ASCII characters.  The bidi categories are taken from the
;; UnicodeData.txt file available from http://www.unicode.org/.  The
;; test table adds one one particular information: It makes all capital
;; characters right-to-left.  The table can thus be used whenever
;; examples from the standards need to be tested.

;;; Code:


(let ((table (standard-category-table))
      (test-bidi-alist
       '((0 . bidi-category-bn) ;; NULL
	 (1 . bidi-category-bn) ;; START OF HEADING
	 (2 . bidi-category-bn) ;; START OF TEXT
	 (3 . bidi-category-bn) ;; END OF TEXT
	 (4 . bidi-category-bn) ;; END OF TRANSMISSION
	 (5 . bidi-category-bn) ;; ENQUIRY
	 (6 . bidi-category-bn) ;; ACKNOWLEDGE
	 (7 . bidi-category-bn) ;; BELL
	 (8 . bidi-category-bn) ;; BACKSPACE
	 (9 . bidi-category-s) ;; HORIZONTAL TABULATION
	 (10 . bidi-category-b) ;; LINE FEED
	 (11 . bidi-category-s) ;; VERTICAL TABULATION
	 (12 . bidi-category-ws) ;; FORM FEED
	 (13 . bidi-category-b) ;; CARRIAGE RETURN
	 (14 . bidi-category-bn) ;; SHIFT OUT
	 (15 . bidi-category-bn) ;; SHIFT IN
	 (16 . bidi-category-bn) ;; DATA LINK ESCAPE
	 (17 . bidi-category-bn) ;; DEVICE CONTROL ONE
	 (18 . bidi-category-bn) ;; DEVICE CONTROL TWO
	 (19 . bidi-category-bn) ;; DEVICE CONTROL THREE
	 (20 . bidi-category-bn) ;; DEVICE CONTROL FOUR
	 (21 . bidi-category-bn) ;; NEGATIVE ACKNOWLEDGE
	 (22 . bidi-category-bn) ;; SYNCHRONOUS IDLE
	 (23 . bidi-category-bn) ;; END OF TRANSMISSION BLOCK
	 (24 . bidi-category-bn) ;; CANCEL
	 (25 . bidi-category-bn) ;; END OF MEDIUM
	 (26 . bidi-category-bn) ;; SUBSTITUTE
	 (27 . bidi-category-bn) ;; ESCAPE
	 (28 . bidi-category-b) ;; FILE SEPARATOR
	 (29 . bidi-category-b) ;; GROUP SEPARATOR
	 (30 . bidi-category-b) ;; RECORD SEPARATOR
	 (31 . bidi-category-s) ;; UNIT SEPARATOR
	 (32 . bidi-category-ws) ;; 
	 (33 . bidi-category-on) ;; 
	 (34 . bidi-category-on) ;; 
	 (35 . bidi-category-et) ;; 
	 (36 . bidi-category-et) ;; 
	 (37 . bidi-category-et) ;; 
	 (38 . bidi-category-on) ;; 
	 (39 . bidi-category-on) ;; APOSTROPHE-QUOTE
	 (40 . bidi-category-on) ;; OPENING PARENTHESIS
	 (41 . bidi-category-on) ;; CLOSING PARENTHESIS
	 (42 . bidi-category-on) ;; 
	 (43 . bidi-category-et) ;; 
	 (44 . bidi-category-cs) ;; 
	 (45 . bidi-category-et) ;; 
	 (46 . bidi-category-cs) ;; PERIOD
	 (47 . bidi-category-es) ;; SLASH
	 (48 . bidi-category-en) ;; 
	 (49 . bidi-category-en) ;; 
	 (50 . bidi-category-en) ;; 
	 (51 . bidi-category-en) ;; 
	 (52 . bidi-category-en) ;; 
	 (53 . bidi-category-en) ;; 
	 (54 . bidi-category-en) ;; 
	 (55 . bidi-category-en) ;; 
	 (56 . bidi-category-en) ;; 
	 (57 . bidi-category-en) ;; 
	 (58 . bidi-category-cs) ;; 
	 (59 . bidi-category-on) ;; 
	 (60 . bidi-category-on) ;; 
	 (61 . bidi-category-on) ;; 
	 (62 . bidi-category-on) ;; 
	 (63 . bidi-category-on) ;; 
	 (64 . bidi-category-on) ;; 
	 (65 . bidi-category-r) ;; 
	 (66 . bidi-category-r) ;; 
	 (67 . bidi-category-r) ;; 
	 (68 . bidi-category-r) ;; 
	 (69 . bidi-category-r) ;; 
	 (70 . bidi-category-r) ;; 
	 (71 . bidi-category-r) ;; 
	 (72 . bidi-category-r) ;; 
	 (73 . bidi-category-r) ;; 
	 (74 . bidi-category-r) ;; 
	 (75 . bidi-category-r) ;; 
	 (76 . bidi-category-r) ;; 
	 (77 . bidi-category-r) ;; 
	 (78 . bidi-category-r) ;; 
	 (79 . bidi-category-r) ;; 
	 (80 . bidi-category-r) ;; 
	 (81 . bidi-category-r) ;; 
	 (82 . bidi-category-r) ;; 
	 (83 . bidi-category-r) ;; 
	 (84 . bidi-category-r) ;; 
	 (85 . bidi-category-r) ;; 
	 (86 . bidi-category-r) ;; 
	 (87 . bidi-category-r) ;; 
	 (88 . bidi-category-r) ;; 
	 (89 . bidi-category-r) ;; 
	 (90 . bidi-category-r) ;; 
	 (91 . bidi-category-on) ;; OPENING SQUARE BRACKET
	 (92 . bidi-category-on) ;; BACKSLASH
	 (93 . bidi-category-on) ;; CLOSING SQUARE BRACKET
	 (94 . bidi-category-on) ;; SPACING CIRCUMFLEX
	 (95 . bidi-category-on) ;; SPACING UNDERSCORE
	 (96 . bidi-category-on) ;; SPACING GRAVE
	 (97 . bidi-category-l) ;; 
	 (98 . bidi-category-l) ;; 
	 (99 . bidi-category-l) ;; 
	 (100 . bidi-category-l) ;; 
	 (101 . bidi-category-l) ;; 
	 (102 . bidi-category-l) ;; 
	 (103 . bidi-category-l) ;; 
	 (104 . bidi-category-l) ;; 
	 (105 . bidi-category-l) ;; 
	 (106 . bidi-category-l) ;; 
	 (107 . bidi-category-l) ;; 
	 (108 . bidi-category-l) ;; 
	 (109 . bidi-category-l) ;; 
	 (110 . bidi-category-l) ;; 
	 (111 . bidi-category-l) ;; 
	 (112 . bidi-category-l) ;; 
	 (113 . bidi-category-l) ;; 
	 (114 . bidi-category-l) ;; 
	 (115 . bidi-category-l) ;; 
	 (116 . bidi-category-l) ;; 
	 (117 . bidi-category-l) ;; 
	 (118 . bidi-category-l) ;; 
	 (119 . bidi-category-l) ;; 
	 (120 . bidi-category-l) ;; 
	 (121 . bidi-category-l) ;; 
	 (122 . bidi-category-l) ;; 
	 (123 . bidi-category-on) ;; OPENING CURLY BRACKET
	 (124 . bidi-category-on) ;; VERTICAL BAR
	 (125 . bidi-category-on) ;; CLOSING CURLY BRACKET
	 (126 . bidi-category-on) ;; 
	 (127 . bidi-category-bn)))) ;; DELETE
  ;; (apply 'string (mapcar 'car test-bidi-alist))
  (dolist (pair test-bidi-alist)
    (let ((char (car pair))
	  (bidi-type (symbol-value (cdr pair))))
      (modify-category-entry char bidi-type table))))

(assert (aref (char-category-set ?a) bidi-category-l))
(assert (aref (char-category-set ?A) bidi-category-r)); Test!
(assert (not (aref (char-category-set ?a) bidi-category-r)))
(assert (not (aref (char-category-set ?A) bidi-category-l))); Test!
(assert (aref (char-category-set ? ) bidi-category-ws))

;; (dolist (pair bidi-mirroring-table)
;;   (let ((char (car pair))
;; 	(mirror (cdr pair)))
;;     (if (< char 128)
;; 	(insert (format "(%d . %d)\n" char mirror)))))

(setq bidi-mirroring-table '((125 . 123)
			     (123 . 125)
			     (93 . 91)
			     (91 . 93)
			     (62 . 60)
			     (60 . 62)
			     (41 . 40)
			     (40 . 41)))

(assert (not (assq ?a bidi-mirroring-table)))
(assert (eq ?\) (cdr (assq ?\( bidi-mirroring-table))))
(assert (eq ?\[ (cdr (assq ?\] bidi-mirroring-table))))

;;; bidi-table-test.el ends here.
