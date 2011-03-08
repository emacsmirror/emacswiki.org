;;; mon-doc-help-char-encoding-lossage.el --- Show charset encoding lossage
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-doc-help-char-encoding-lossage.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2011-03-06T00:18:18-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-doc-help-char-encoding-lossage provides Show charset encoding lossage
;;
;; FUNCTIONS:►►►
;; `mon-get-encoding-codepoint', `mon-get-encoding-position-lossage',
;; `mon-get-encoding-point-hist', `mon-get-encoding-map-results',
;; `mon-get-encoding-point-hist-map-plists',
;; `mon-make-encoding-position-lossage-table',
;; `mon-help-cp1252-iso-8859-1-lossage', `mon-help-iso-8859-1-8859-15-lossage',
;; `mon-help-cp1252-iso-8859-15-lossage',
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;; `*cp1252-8859-15-lossage*', `*cp1252-8859-15-lossage-rules*',
;; `*cp1252-8859-1-lossage*', `*cp1252-8859-1-lossage-rules*',
;; `*8859-1-8859-15-lossage*', `*8859-1-8859-15-lossage-rules*',
;; `*mon-doc-help-char-encoding-lossage-xrefs*'
;;
;; GROUPS:
;; `mon-doc-help-char-encoding-lossage'
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; TODO:
;;
;; NOTES:
;;
;; Example usage:
;;
;; (mon-get-encoding-point-hist-map-plists *cp1252-8859-15-lossage* *cp1252-8859-15-lossage-rules* t)
;; (mon-get-encoding-point-hist-map-plists *cp1252-8859-1-lossage* *cp1252-8859-1-lossage-rules* t)
;; (mon-get-encoding-point-hist-map-plists *8859-1-8859-15-lossage* *8859-1-8859-15-lossage-rules* t)
;;
;; :SEE (URL `http://www.eki.ee/letter/')
;; :SEE (URL `http://en.wikipedia.org/wiki/Character_encoding')
;; :SEE (URL `http://en.wikipedia.org/wiki/Windows-1252')
;; :SEE (URL `http://en.wikipedia.org/wiki/ISO/IEC_8859-15')
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-doc-help-char-encoding-lossage.el
;; FIRST-PUBLISHED: <Timestamp: #{2011-03-07T16:54:01-05:00Z}#{11101} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-doc-help-char-encoding-lossage. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2011-03-06T00:18:18-05:00Z}#{11097} - by MON KEY>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))


;;; ==============================
;;; :CHANGESET 2421
;;; :CREATED <Timestamp: #{2011-03-07T16:02:21-05:00Z}#{11101} - by MON KEY>
(defgroup mon-doc-help-char-encoding-lossage nil
  "Customization group for variables and functions of :FILE mon-doc-help-char-encoding-lossage.el\n
:SEE-ALSO .\n►►►"
  ;; :prefix "<PREFIX>"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE"
          "http://www.emacswiki.org/emacs/mon-doc-help-char-encoding-lossage.el")
  :link '(emacs-library-link 
          :tag ":FILE mon-doc-help-char-encoding-lossage.el"
          "mon-doc-help-char-encoding-lossage.el")
  :group 'mon-doc-help-utils)

;;; ==============================
;;; :CHANGESET 2421
;;; :CREATED <Timestamp: #{2011-03-07T16:03:32-05:00Z}#{11101} - by MON KEY>
(defcustom *mon-doc-help-char-encoding-lossage-xrefs*
  '(mon-get-encoding-codepoint mon-get-encoding-position-lossage
    mon-get-encoding-point-hist mon-get-encoding-map-results
    mon-get-encoding-point-hist-map-plists
    mon-make-encoding-position-lossage-table
    mon-help-cp1252-iso-8859-1-lossage mon-help-iso-8859-1-8859-15-lossage
    mon-help-cp1252-iso-8859-15-lossage
    ;; :VARIABLES
    *cp1252-8859-15-lossage* *cp1252-8859-15-lossage-rules*
    *cp1252-8859-1-lossage* *cp1252-8859-1-lossage-rules*
    *8859-1-8859-15-lossage* *8859-1-8859-15-lossage-rules*
    *mon-doc-help-char-encoding-lossage-xrefs*)
"Xrefing list of mon type/predicate symbols, functions constants, and variables.\n
The symbols contained of this list are defined in :FILE mon-doc-help-char-encoding-lossage.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-testme-utils-xrefs*',
`*mon-button-utils-xrefs*', `*mon-buffer-utils-xrefs*',
`*mon-line-utils-xrefs*', `*mon-plist-utils-xrefs*'
`*mon-seq-utils-xrefs*', `*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*',
`*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*', `*mon-ulan-utils-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-doc-help-char-encoding-lossage
  :group 'mon-xrefs)

 
;;; ==============================
;; *8859-1-8859-15-lossage* *8859-1-8859-15-lossage-rules*
(defcustom *8859-1-8859-15-lossage*
  (mon-make-encoding-position-lossage-table
   '(:8859-1 :CODE-PSN-ORIG :8859-15)
   '(("\u00A4"  #xA4  "\u20ac") ("\u00a6"  #xa6  "\u0160")
     ("\u00a8"  #xa8  "\u0161") ("\u00b4"  #xb4  "\u017D")
     ("\u00b8"  #xb8  "\u017E") ("\u00bc"  #xbc  "\u0152")
     ("\u00bd"  #xbd  "\u0153") ("\u00be"  #xbe  "\u0178")))
  "Character Encoding losssage table for ISO-8859-1 -> ISO-8859-15 conversions.\n
:EXAMPLE\n\n(plist-get \(car *8859-1-8859-15-lossage*\) :8859-1\)\n
:NOTE Changes from ISO-8859-1 -> ISO-8859-15:\n
           #xA4   #xA6    #xA8    #xB4    #xB8    #xBC    #xBD    #xBE
   8859-1    ¤      ¦       ¨       ´       ¸       ¼       ½       ¾
   8859-15   €      Š       š       Ž       ž       Œ       œ       Ÿ\n
Character glyph \"€\" became necessary when the Euro was introduced.\n
Character gyphss: \"Š\", \"š\", \"Ž\", and \"ž\" are used in some \"loanwords\"
and transliteration of Russian names in Finnish and Estonian typography.\n
Character glyphs \"Œ\" and \"œ\" are French ligatures.\n
Character glyph \"Ÿ\" is needed in French all-caps text, as it appears in some
proper names e.g. the city of l'Haÿ-les-Roses or of poet Pierre Louÿs.\n
:SEE \(URL `http://www.eki.ee/letter/chardata.cgi?cp=8859-1&cp1=8859-15'\)
:SEE \(URL `http://www.eki.ee/letter/'\)
:SEE \(URL `http://en.wikipedia.org/wiki/ISO/IEC_8859-1'\)
:SEE \(URL `http://en.wikipedia.org/wiki/ISO/IEC_8859-15'\)
:SEE \(URL `http://www.cs.tut.fi/~jkorpela/latin9.html'\)
:SEE \(URL `http://en.wikipedia.org/wiki/Character_encoding'\)\n
:SEE-ALSO `*cp1252-8859-15-lossage-rules*', `*cp1252-8859-1-lossage*',
`*cp1252-8859-1-lossage-rules*', `*8859-1-8859-15-lossage*',
`*8859-1-8859-15-lossage-rules*'.\n►►►"
  :type '(repeat plist)
  :group 'mon-doc-help-char-encoding-lossage)
;;
(defcustom *8859-1-8859-15-lossage-rules*
  '((mon-get-encoding-codepoint :8859-1)
    (mon-get-encoding-position-lossage   :CODE-PSN-ORIG)
    (mon-get-encoding-codepoint :8859-15))
  "Formatting rules for character encoding lossage table `*8859-1-8859-15-lossage*'.\n
:EXAMPLE\n
 \(assoc 'mon-get-encoding-position-lossage *8859-1-8859-15-lossage-rules*\)\n
:SEE-ALSO `*cp1252-8859-15-lossage*', `*cp1252-8859-1-lossage*',
`*cp1252-8859-1-lossage-rules*', `*8859-1-8859-15-lossage-rules*'.\n►►►"
  :type '(repeat (list function symbol))
  :group 'mon-doc-help-char-encoding-lossage)

 
;;; ==============================
;; CP-1252 --> 8859-15
(defcustom *cp1252-8859-15-lossage*
 (mon-make-encoding-position-lossage-table
 '(:CP1252 :CODE-PSN-ORIG :8859-15 (:CODE-PSN-MOVE . :CHANGE))
 '(("\u201A" #x82 "unassigned" "lost") ("\u0192" #x83 "unassigned" "lost")
   ("\u201E" #x84 "unassigned" "lost") ("\u2026" #x85 "unassigned" "lost")
   ("\u2020" #x86 "unassigned" "lost") ("\u2021" #x87 "unassigned" "lost")
   ("\u02C6" #x88 "unassigned" "lost") ("\u2030" #x89 "unassigned" "lost")
   ("\u2039" #x8B "unassigned" "lost") ("\u2018" #x91 "unassigned" "lost")
   ("\u2019" #x92 "unassigned" "lost") ("\u201C" #x93 "unassigned" "lost")
   ("\u201D" #x94 "unassigned" "lost") ("\u2022" #x95 "unassigned" "lost")
   ("\u2013" #x96 "unassigned" "lost") ("\u2014" #x97 "unassigned" "lost")
   ("\u02DC" #x98 "unassigned" "lost") ("\u2122" #x99 "unassigned" "lost")
   ("\u203A" #x9B "unassigned" "lost") ("\u0161" #x9A "unassigned" #xA8)
   ("\u00A8" #xA8 "\u0161"     "lost") ("\u017D" #x8E "unassigned" #xB4)
   ("\u00B4" #xB4 "\u017D"     "lost") ("\u0152" #x8C "unassigned" #xBC)
   ("\u00BC" #xBC "\u0152"     "lost") ("\u0153" #x9C "unassigned" #xBD)
   ("\u00BD" #xBD "\u0153"     "lost") ("\u0160" #x8A "unassigned" #xA6)
   ("\u00A6" #xA6 "\u0160"     "lost") ("\u017E" #x9E "unassigned" #xB8)
   ("\u00B8" #xB8 "\u017E"     "lost") ("\u20AC" #x80 "unassigned" #xA4)
   ("\u00A4" #xA4 "\u20AC"     "lost") ("\u0178" #x9F "unassigned" #xBE)
   ("\u00BE" #xBE "\u0178"     "lost")))
"Character Encoding losssage table for WINDOWS-1252 -> ISO-8859-15 conversions.\n
:EXAMPLE\n\n(plist-get \(car *cp1252-8859-15-lossage*\) :CP1252\)
:SEE \(URL `http://www.eki.ee/letter/chardata.cgi?cp=CP1252+%28Western%29&cp1=8859-15'\)
:SEE \(URL `http://www.eki.ee/letter/'\)
:SEE \(URL `http://en.wikipedia.org/wiki/Windows-1252'\)
:SEE \(URL `http://en.wikipedia.org/wiki/ISO/IEC_8859-15'\)
:SEE \(URL `http://en.wikipedia.org/wiki/Character_encoding'\)\n
:SEE-ALSO `*cp1252-8859-15-lossage-rules*', `*cp1252-8859-1-lossage*',
`*cp1252-8859-1-lossage-rules*', `*8859-1-8859-15-lossage*',
`*8859-1-8859-15-lossage-rules*'.\n►►►"
  :type '(repeat plist)
  :group 'mon-doc-help-char-encoding-lossage)
;; RULES
(defcustom *cp1252-8859-15-lossage-rules*
  '((mon-get-encoding-codepoint :CP1252)
    (mon-get-encoding-position-lossage   :CODE-PSN-ORIG)
    (mon-get-encoding-codepoint :8859-15 "unassigned")
    (mon-get-encoding-position-lossage   :CODE-PSN-MOVE)
    (mon-get-encoding-codepoint :CHANGE "lost"))
"Formatting rules for character encoding lossage table `*cp1252-8859-15-lossage*'.\n
:EXAMPLE\n
 \(assoc 'mon-get-encoding-position-lossage *cp1252-8859-15-lossage*\)\n
:SEE-ALSO `*cp1252-8859-1-lossage*', `*cp1252-8859-1-lossage-rules*',
`*8859-1-8859-15-lossage*', `*8859-1-8859-15-lossage-rules*'.\n►►►"
  :type '(repeat sexp)
  :group 'mon-doc-help-char-encoding-lossage)

 
;;; ==============================
;; :CP1252 --> :8859-1
(defcustom *cp1252-8859-1-lossage*
  (mon-make-encoding-position-lossage-table
   '(:CP1252 :CODE-PSN-ORIG :8859-1 :CHANGE)
   '(("\u20AC" #x80 "unassigned" "lost") ("\u201A" #x82 "unassigned" "lost")
     ("\u0192" #x83 "unassigned" "lost") ("\u201E" #x84 "unassigned" "lost")
     ("\u2026" #x85 "unassigned" "lost") ("\u2020" #x86 "unassigned" "lost")
     ("\u2021" #x87 "unassigned" "lost") ("\u02C6" #x88 "unassigned" "lost")
     ("\u2030" #x89 "unassigned" "lost") ("\u0160" #x8A "unassigned" "lost")
     ("\u2039" #x8B "unassigned" "lost") ("\u0152" #x8C "unassigned" "lost")
     ("\u017D" #x8E "unassigned" "lost") ("\u2018" #x91 "unassigned" "lost")
     ("\u2019" #x92 "unassigned" "lost") ("\u201C" #x93 "unassigned" "lost")
     ("\u201D" #x94 "unassigned" "lost") ("\u2022" #x95 "unassigned" "lost")
     ("\u2013" #x96 "unassigned" "lost") ("\u2014" #x97 "unassigned" "lost")
     ("\u02DC" #x98 "unassigned" "lost") ("\u2122" #x99 "unassigned" "lost")
     ("\u0161" #x9A "unassigned" "lost") ("\u203A" #x9B "unassigned" "lost")
     ("\u0153" #x9C "unassigned" "lost") ("\u017E" #x9E "unassigned" "lost")
     ("\u0178" #x9F "unassigned" "lost")))
  "Character Encoding losssage table for WINDOWS-1252 -> ISO-8859-1 conversions.\n
:EXAMPLE\n\n\(plist-get \(car *cp1252-8859-1-lossage*\) :CP1252\)
:SEE \(URL `http://www.eki.ee/letter/chardata.cgi?cp=CP1252+%28Western%29&cp1=8859-1'\)\n
:SEE \(URL `http://www.eki.ee/letter/'\)
:SEE \(URL `http://en.wikipedia.org/wiki/Windows-1252'\)
:SEE \(URL `http://en.wikipedia.org/wiki/ISO/IEC_8859-1'\)
:SEE \(URL `http://en.wikipedia.org/wiki/Character_encoding'\)
:SEE-ALSO `*cp1252-8859-1-lossage-rules*', `*cp1252-8859-15-lossage*',
`*cp1252-8859-15-lossage-rules*', `*8859-1-8859-15-lossage*',
`*8859-1-8859-15-lossage-rules*'.\n►►►"
  :type '(repeat plist)
  :group 'mon-doc-help-char-encoding-lossage)
;; :RULES
(defcustom *cp1252-8859-1-lossage-rules*
  '((mon-get-encoding-codepoint :CP1252)
    (mon-get-encoding-position-lossage   :CODE-PSN-ORIG)
    (mon-get-encoding-codepoint :8859-1 "unassigned")
    (mon-get-encoding-position-lossage   :CODE-PSN-MOVE)
    (mon-get-encoding-codepoint :CHANGE "lost"))
  "Formatting rules for character encoding lossage table `*cp1252-8859-15-lossage*'.\n
:EXAMPLE\n
 \(assoc 'mon-get-encoding-position-lossage *cp1252-8859-1-lossage*\)\n
:SEE-ALSO `*cp1252-8859-15-lossage*', `*cp1252-8859-15-lossage-rules*',
`*8859-1-8859-15-lossage*', `*8859-1-8859-15-lossage-rules*'.\n►►►"
  :type '(repeat sexp)
  :group 'mon-doc-help-char-encoding-lossage)

;;; ==============================
;;; :CHANGESET 2420
;;; :CREATED <Timestamp: #{2011-03-06T00:24:36-05:00Z}#{11097} - by MON KEY>
(defun mon-get-encoding-codepoint (from-plist w-key &optional when-not-char &rest rest)
  "Find W-KEY in plist FROM-PLIST.\n
Arg WHEN-NOT-CHAR when non-nil is a string or the boolean T.\n
When it is a string it is used to match a string-equal value of W-KEY. 
If a match does not exist signal an error.
If WHEN-NOT-CHAR is the boolean T it may match either strings of length 1 or greater.
It is intended that callers use the boolean to indicate that a keys value is a
string occuring as a reference to a character glyph inside a string, where it is
known that a keys value is some other type of string callers should specify
WHEN-NOT-CHAR as the expected string value _not_ T.\n
Arg REST is effectively a \(declare \(ignore rest\)\) were it possible w/ Emacs lisp.
It is required in order to allow callers to pass a value for W-RAW-BYTE args in
lieu of WHEN-NOT-CHAR.\n
:EXAMPLE\n\n
 \(mon-get-encoding-codepoint '\(:8859-1 \"\\u00A4\" :CODE-PSN-ORIG #xA4 :8859-15 \"\\u20ac\"\) :8859-1 t\)
 \(mon-get-encoding-codepoint '\(:8859-1 \"\\u00A4\" :CODE-PSN-ORIG #xA4 :8859-15 \"\\u20ac\"\) :8859-15\)
 \(mon-get-encoding-codepoint '\(:8859-1 \"\\u00A4\" :CODE-PSN-ORIG #xA4 :8859-15 \"\\u20ac\"\) :8859-15 \"unassigned\"\)
 \(mon-get-encoding-codepoint '\(:8859-1 \"\\u00A4\" :CODE-PSN-ORIG #xA4 :8859-15 \"unassigned\"\) :8859-15 \"unassigned\"\)
 \(mon-get-encoding-codepoint '\(:CP1252 \"\\u00BE\" :CODE-PSN-ORIG #xBE :8859-15 \"\\u0178\" :CHANGE \"lost\"\) :CHANGE  \"lost\"\)
 \(mon-get-encoding-codepoint '\(:CP1252 \"\\u00BE\" :CODE-PSN-ORIG #xBE :8859-15 \"\\u0178\" :CHANGE \"lost\"\) :CHANGE t\)
 \(mon-get-encoding-codepoint '\(:CP1252 \"\\u00BE\" :CODE-PSN-ORIG #xBE :8859-15 \"\\u0178\" :CHANGE \"lost\"\) :NOT-key \"bubba\"\)
 \(mon-get-encoding-codepoint '\(:CP1252 \"\\u00BE\" :CODE-PSN-ORIG #xBE :8859-15 \"\\u0178\" :CHANGE \"lost\"\) :CHANGE\)
 \(let \(\(chk '\(mon-get-encoding-codepoint :8859-1\)\)
       \(pl '\(:8859-1 \"\\u00A4\"  :CODE-PSN-ORIG #xA4  :8859-15 \"\\u20ac\"\)\)\)
   \(apply \(car chk\)  pl \(cdr chk\)\)\)
;; Following successfully signals an error:
 \(mon-get-encoding-codepoint '\(:CP1252  \"\\u00BE\" :CODE-PSN-ORIG #xBE :8859-15 \"\\u0178\" :CHANGE \"lost\"\) :CHANGE \"bubba\"\)\n
:SEE-ALSO `mon-get-encoding-codepoint', `mon-get-encoding-position-lossage',
`mon-get-encoding-point-hist', `mon-get-encoding-map-results',
`mon-get-encoding-point-hist-map-plists',
`mon-make-encoding-position-lossage-table', `mon-help-char-coding-functions',
`mon-help-char-functions', `mon-help-char-composition',
`mon-help-char-raw-bytes', `mon-help-char-representation',
`mon-help-char-table-functions', `mon-help-char-unidata-table',
`mon-help-char-ascii', `mon-help-char-iso-8859-1', `mon-help-char-ecma-35',
`mon-help-char-ecma-48', `mon-help-char-logic'.\n►►►"
  (let ((mgfe-if (plist-get from-plist w-key)))
    (and mgfe-if 
         (or (and (= (length mgfe-if) 1)
                  (or (null when-not-char)
                      (stringp when-not-char)
                      (eq when-not-char t))
                  (cons t (format "%s :CODE-CHAR #\\%s :CHAR-CODE #x%04X"
                                  w-key mgfe-if (string-to-char mgfe-if))))
             (and when-not-char 
                  (stringp when-not-char)
                  (or (and (string-equal mgfe-if when-not-char)
                           (cons t (format "%s %S" w-key mgfe-if)))
                      (mon-format :w-fun #'error
                                  :w-spec '("Arg WHEN-NOT-CHAR was: %S\n"
                                            "but no match in arg FROM-PLIST got:\n %S\n")
                                  :w-args `(,when-not-char ,from-plist))))
             (and when-not-char 
                  (eq when-not-char t)
                  (equal (nth (- (length from-plist) 2) from-plist) w-key)
                  (equal (mon-list-last from-plist) mgfe-if)
                  (cons t (format "%s %S" w-key mgfe-if)))
             (cons t "")))))


;;; ==============================
;;; :CHANGESET 2420
;;; :CREATED <Timestamp: #{2011-03-06T00:24:39-05:00Z}#{11097} - by MON KEY>
(defun mon-get-encoding-position-lossage (from-plist key-for-int-val 
                                                     &optional w-raw-byte)
  "Return value of key KEY-FOR-INT-VAL in FROM-PLIST as consed pair.
Return value has the form:
 \( { <BOOLEAN> | <INT> } .  
   { \"<KEY-FOR-INT-VAL> :HEX #x<HEX> :OCTAL #o<OCT> :DECIMAL <DEC>\" |
   { \"<KEY-FOR-INT-VAL> :HEX #x<HEX> :OCTAL #o<OCT> :DECIMAL <DEC> :RAW-BYTE\" } \)\n
Car of return value is either a boolean or a positive integer.\n
If KEY-FOR-INT-VAL was found in FROM-PLIST and W-RAW-BYTE is ommitted car is T.
When W-RAW-BYTE is non-nil and KEY-FOR-INT-VAL was found it is an INT.\n
When W-RAW-BYTE is non-nil cdr of return value will contain \" :RAW-BYTE \".\n
:EXAMPLE\n
\(mon-get-encoding-codepoint '\(:CP1252 \"\\u00BE\" :CODE-PSN-ORIG #xBE :8859-15 \"\\u0178\" :CHANGE \"lost\"\) :CHANGE t\)\n
\(mon-get-encoding-position-lossage '\(:8859-1 \"\\u00A4\" :CODE-PSN-ORIG #xA4 :8859-15 \"\\u20ac\"\) :CODE-PSN-ORIG\)\n
\(mon-get-encoding-position-lossage '\(:8859-1 \"\\u00A4\" :CODE-PSN-ORIG #xA4 :8859-15 \"\\u20ac\"\) :CODE-PSN-ORIG t\)\n
\(mon-get-encoding-position-lossage '\(:CP1252 \"\\u0161\" :CODE-PSN-ORIG #x9A :8859-15 \"unassigned\" :CODE-PSN-MOVE #xA8\) :CODE-PSN-MOVE\)\n
\(mon-get-encoding-position-lossage '\(:CP1252 \"\\u0161\" :CODE-PSN-ORIG #x9A :8859-15 \"unassigned\" :CODE-PSN-MOVE #xA8\) :CODE-PSN-MOVE t\)\n
\(cdr \(mon-get-encoding-position-lossage '\(:8859-1 \"\\u00A4\" :CODE-PSN-ORIG #xA4 :8859-15 \"\\u20ac\"\) :CODE-PSN-ORIG t\)\)\n
\(car \(mon-get-encoding-position-lossage '\(:8859-1 \"\\u00A4\" :CODE-PSN-ORIG #xA4 :8859-15 \"\\u20ac\"\) :NOT-A-PSN-ORIG t\)\)\n
:SEE-ALSO `mon-get-encoding-codepoint', `mon-get-encoding-position-lossage',
`mon-get-encoding-point-hist', `mon-get-encoding-map-results',
`mon-get-encoding-point-hist-map-plists',
`mon-make-encoding-position-lossage-table', `mon-help-char-coding-functions',
`mon-help-char-functions', `mon-help-char-composition',
`mon-help-char-raw-bytes', `mon-help-char-representation',
`mon-help-char-table-functions', `mon-help-char-unidata-table',
`mon-help-char-ascii', `mon-help-char-iso-8859-1', `mon-help-char-ecma-35',
`mon-help-char-ecma-48', `mon-help-char-logic'.\n►►►"
  (let ((mgp-if (plist-get from-plist key-for-int-val)))
    (cons 
     (and mgp-if
          (or (and w-raw-byte mgp-if) t))
     (and mgp-if
          (format "%s :HEX #x%X :OCTAL #o%o :DECIMAL %d%s"
                  key-for-int-val
                  mgp-if  mgp-if  mgp-if 
                  (or (and w-raw-byte " :RAW-BYTE ") ""))))))

;;; ==============================
;;; :CHANGESET 2420
;;; :CREATED <Timestamp: #{2011-03-06T00:24:41-05:00Z}#{11097} - by MON KEY>
(defun mon-get-encoding-point-hist (w-plist rules &optional w-raw-byte); &rest rules)
  "Apply RULES to W-PLIST maybe W-RAW-BYTE.\n
Helper function for `mon-get-encoding-point-hist-map-plists' which calls it
iteratively within a `mon-get-encoding-map-results' form.\n
:EXAMPLE\n
\(let \(\(eg-rules '\(\(mon-get-encoding-codepoint :8859-1\)
                  \(mon-get-encoding-position-lossage   :CODE-PSN-ORIG\)
                  \(mon-get-encoding-codepoint :8859-15 \"unassigned\"\)
                  \(mon-get-encoding-position-lossage   :CODE-PSN-MOVE\)
                  \(mon-get-encoding-codepoint :CHANGE \"lost\"\)\)\)
      \(eg-plists '\(\(:8859-1 \"\\u00A4\" :CODE-PSN-ORIG #x9A 
                            :8859-15 \"unassigned\" :CHANGE \"lost\"\)
                   \(:8859-1 \"\\u00A4\" :CODE-PSN-ORIG #x9A
                            :8859-15 \"unassigned\" :CODE-PSN-MOVE  #xA8\)
                   \(:8859-1 \"\\u00A4\" :CODE-PSN-ORIG #xA4 :8859-15 \"\\u20ac\"\)\)\)\)
  \(mapcar #'\(lambda \(pl\)
              \(mon-get-encoding-point-hist pl eg-rules t\)\)
          eg-plists\)\)\n
:SEE-ALSO `mon-get-encoding-codepoint', `mon-get-encoding-position-lossage',
`mon-get-encoding-point-hist', `mon-get-encoding-map-results',
`mon-get-encoding-point-hist-map-plists',
`mon-make-encoding-position-lossage-table', `mon-help-char-coding-functions',
`mon-help-char-functions', `mon-help-char-composition',
`mon-help-char-raw-bytes', `mon-help-char-representation',
`mon-help-char-table-functions', `mon-help-char-unidata-table',
`mon-help-char-ascii', `mon-help-char-iso-8859-1', `mon-help-char-ecma-35',
`mon-help-char-ecma-48', `mon-help-char-logic'.\n►►►"
  (let ((rstls '()))
    (dolist (r rules (nreverse rstls))
      (let ((rl-if (apply (car r) w-plist `(,@(cdr r) ,w-raw-byte))))
        (when (car rl-if) (push rl-if rstls))))))



;;; ==============================
;;; :CHANGESET 2420
;;; :CREATED <Timestamp: #{2011-03-06T00:24:44-05:00Z}#{11097} - by MON KEY>
(defun mon-get-encoding-map-results (results w-buffer)
  "Helper function for `mon-get-encoding-point-hist-map-plists'.
Used as a wrapper to iterate over RESULTS of `mon-get-encoding-point-hist'.\n
Return value inserted in buffer W-BUFFER.\n
:SEE-ALSO `mon-get-encoding-codepoint', `mon-get-encoding-position-lossage',
`mon-get-encoding-point-hist', `mon-get-encoding-map-results',
`mon-get-encoding-point-hist-map-plists',
`mon-make-encoding-position-lossage-table', `mon-help-char-coding-functions',
`mon-help-char-functions', `mon-help-char-composition',
`mon-help-char-raw-bytes', `mon-help-char-representation',
`mon-help-char-table-functions', `mon-help-char-unidata-table',
`mon-help-char-ascii', `mon-help-char-iso-8859-1', `mon-help-char-ecma-35',
`mon-help-char-ecma-48', `mon-help-char-logic'.\n►►►"
  (with-current-buffer w-buffer
    (insert "\n" (make-string 5 45) "\n")
    (mapc #'(lambda (mgemr-L-0) 
              (let ((mgemr-hd (car mgemr-L-0)))
                (and mgemr-hd
                     (or (and (eq mgemr-hd t)
                              (or (insert (cdr mgemr-L-0) " ") t))
                         (and (integerp mgemr-hd)
                              (or (insert (cdr mgemr-L-0)) t)
                              (or (insert-byte mgemr-hd 1) t)
                              (terpri (current-buffer))))))
              (unless (eql (line-beginning-position) (line-end-position)) 
                (terpri (current-buffer))))
          results)))


;;; ==============================
;;; :CHANGESET 2420
;;; :CREATED <Timestamp: #{2011-03-06T00:24:46-05:00Z}#{11097} - by MON KEY>
(defun* mon-get-encoding-point-hist-map-plists (w-plists w-rules &optional w-raw-byte
                                                         &key w-buffer-named w-buffer-header)
  "Interface function for presenting character encoding conversion lossage.\n
W-PLISTS a list of plists to map according to values of W-RULES.\n
W-RULES a list of rules for formatting values of W-PLISTS.
W-RAW-BYTE when non-nil will insert as if by `insert-byte' the value of character
encoding postitions.\n
Keyword arg W-BUFFER-NAMED is a string naming a buffer. When ommitted return
results to buffer named \"*ENCODING-LOSSAGE*\" contents of buffer are erased
prior to insertion of return value.\n
Keyword arg W-BUFFER-HEADER is a string to insert at beginning of returned buffer.\n
:EXAMPLE\n\n\(mon-get-encoding-point-hist-map-plists
  *cp1252-8859-15-lossage*
  *cp1252-8859-15-lossage-rules*
  t
  :w-buffer-named \"*MON-HELP-CP1252-8859-1-LOSSAGE*\"
  :w-buffer-header \";; Lossage when converting Windows CP-1252 --> ISO-8859-1\"\)\n
\(mon-get-encoding-point-hist-map-plists *cp1252-8859-15-lossage* *cp1252-8859-15-lossage-rules* t\)\n
\(mon-get-encoding-point-hist-map-plists *cp1252-8859-1-lossage* *cp1252-8859-1-lossage-rules* t\)\n
\(mon-get-encoding-point-hist-map-plists *8859-1-8859-15-lossage* *8859-1-8859-15-lossage-rules* t\)\n
:SEE-ALSO `mon-get-encoding-codepoint', `mon-get-encoding-position-lossage',
`mon-get-encoding-point-hist', `mon-get-encoding-map-results',
`mon-get-encoding-point-hist-map-plists',
`mon-make-encoding-position-lossage-table', `mon-help-char-coding-functions',
`mon-help-char-functions', `mon-help-char-composition',
`mon-help-char-raw-bytes', `mon-help-char-representation',
`mon-help-char-table-functions', `mon-help-char-unidata-table',
`mon-help-char-ascii', `mon-help-char-iso-8859-1', `mon-help-char-ecma-35',
`mon-help-char-ecma-48', `mon-help-char-logic'.\n►►►"
  (let ((mgephmp-bfr (get-buffer-create (or w-buffer-named "*ENCODING-LOSSAGE*"))))
    (with-current-buffer mgephmp-bfr
      (erase-buffer)
      (save-excursion 
        (when w-buffer-header (insert w-buffer-header "\n" (make-string 68 59) "\n"))
        (mapc #'(lambda (wplst)
                  (mon-get-encoding-map-results 
                   (mon-get-encoding-point-hist wplst w-rules w-raw-byte)
                   (current-buffer)))
              w-plists))
      (whitespace-cleanup) 
      (display-buffer mgephmp-bfr t))))

;;; ==============================
;;; :CHANGESET 2421
;;; :CREATED <Timestamp: #{2011-03-07T13:43:16-05:00Z}#{11101} - by MON KEY>
(defun mon-make-encoding-position-lossage-table (table-keys table-vals)
  "Map keys in TABLE-KEYS to values in TABLE-VALS.\n
TABLE-KEYS is a list. Each elt is either a keyword or consed pair of keywords.\n
It has the form:\n
 \( { :KEY-N | \\\(:KEY-Na . :KEY-Nb\\\) }* \)\n
TABLE-VALS is a list of value lists. Each elt of the values list corresponding
to a keyword element in TABLE-KEYS. Each elt in TABLE-VALS \(e.g. a <values-list>\)
should satisfy the following constraint:\n
 \(= \(length <values-list>\) \(length TABLE-KEYS\)\)\n
When an elt of TABLE-KEYS is a consed pair, the corresponding elt of the
<values-list> being mapped should be either `integerp' or `stringp', an error is
signalled if not.\n
:EXAMPLE\n\n\(mon-make-encoding-position-lossage-table
  '\(:CP1252 :CODE-PSN-ORIG :8859-15 \(:CODE-PSN-MOVE . :CHANGE\)\)
  '\(\(\"\\u0161\" #x9A \"unassigned\" #xA8\)
    \(\"\\u00A8\" #xA8 \"\\u0161\"    \"lost\"\)\)\)\n
:SEE-ALSO `mon-get-encoding-codepoint', `mon-get-encoding-position-lossage',
`mon-get-encoding-point-hist', `mon-get-encoding-map-results',
`mon-get-encoding-point-hist-map-plists',
`mon-make-encoding-position-lossage-table', `mon-help-char-coding-functions',
`mon-help-char-functions', `mon-help-char-composition',
`mon-help-char-raw-bytes', `mon-help-char-representation',
`mon-help-char-table-functions', `mon-help-char-unidata-table',
`mon-help-char-ascii', `mon-help-char-iso-8859-1', `mon-help-char-ecma-35',
`mon-help-char-ecma-48', `mon-help-char-logic'.\n►►►"
  (let ((mmeplt-err-str (concat "mapping key as consed pair: %S\n "
                                "but value neither `integerp' nor `stringp',\n "
                                "got: %S\n type-of: %s")))
    (loop for vl in table-vals
          collect (loop for k in table-keys
                        for v in vl
                        append  (if (consp k)
                                    (or (and (integerp v) (list (car k) v))
                                        (and (stringp v)  (list (cdr k) v))
                                        (error mmeplt-err-str k v (type-of v)))
                                  (list k v)) into gthr-val
                        finally (return gthr-val)) into gthr-vals
          finally (return gthr-vals))))

;;; ==============================
;;; :CHANGESET 2419
;;; :CREATED <Timestamp: #{2011-03-05T17:28:40-05:00Z}#{11096} - by MON KEY>
;;;###autoload
(defun mon-help-cp1252-iso-8859-1-lossage ()
  "Return CP-1252 -> ISO-8859-1 character encoding conversion lossage.\n
Results returned to buffer with name: \"*MON-HELP-CP1252-8859-1-LOSSAGE*\".\n
:EXAMPLE\n\n\(mon-help-cp1252-iso-8859-1-lossage\)\n
:SEE \(URL `http://en.wikipedia.org/wiki/Windows-1252'\)
:SEE \(URL `http://en.wikipedia.org/wiki/ISO/IEC_8859-1'\)
:SEE \(URL `http://www.eki.ee/letter/chardata.cgi?cp=CP1252+%28Western%29&cp1=8859-1'\)
:SEE \(URL `http://www.eki.ee/letter/'\)
:SEE-ALSO `mon-get-encoding-codepoint', `mon-get-encoding-position-lossage',
`mon-get-encoding-point-hist', `mon-get-encoding-map-results',
`mon-get-encoding-point-hist-map-plists',
`mon-make-encoding-position-lossage-table', `mon-help-char-coding-functions',
`mon-help-char-functions', `mon-help-char-composition',
`mon-help-char-raw-bytes', `mon-help-char-representation',
`mon-help-char-table-functions', `mon-help-char-unidata-table',
`mon-help-char-ascii', `mon-help-char-iso-8859-1', `mon-help-char-ecma-35',
`mon-help-char-ecma-48', `mon-help-char-logic'.\n►►►"
  (interactive)
  (mon-get-encoding-point-hist-map-plists
   *cp1252-8859-1-lossage* *cp1252-8859-1-lossage-rules* t
   :w-buffer-named "*MON-HELP-CP1252-8859-1-LOSSAGE*"
   :w-buffer-header ";; Lossage when converting Windows CP-1252 --> ISO-8859-1"))


;;; ==============================
;;; :CHANGESET 2421
;;; :CREATED <Timestamp: #{2011-03-07T14:26:02-05:00Z}#{11101} - by MON KEY>
;;;###autoload
(defun mon-help-iso-8859-1-8859-15-lossage ()
  "Return ISO-8859-1 --> ISO-8859-15 character encoding conversion lossage.\n
Results returned to buffer with name: \"*MON-HELP-CP1252-8859-1-LOSSAGE*\".\n
:EXAMPLE\n\n\(mon-help-iso-8859-1-8859-15-lossage\)\n
:SEE \(URL `http://en.wikipedia.org/wiki/ISO/IEC_8859-15'\)
:SEE (URL `http://www.cs.tut.fi/~jkorpela/latin9.html')
:SEE \(URL `http://www.eki.ee/letter/'\)
:SEE-ALSO `mon-get-encoding-codepoint', `mon-get-encoding-position-lossage',
`mon-get-encoding-point-hist', `mon-get-encoding-map-results',
`mon-get-encoding-point-hist-map-plists',
`mon-make-encoding-position-lossage-table', `mon-help-char-coding-functions',
`mon-help-char-functions', `mon-help-char-composition',
`mon-help-char-raw-bytes', `mon-help-char-representation',
`mon-help-char-table-functions', `mon-help-char-unidata-table',
`mon-help-char-ascii', `mon-help-char-iso-8859-1', `mon-help-char-ecma-35',
`mon-help-char-ecma-48', `mon-help-char-logic'.\n►►►"
  (interactive)
  (mon-get-encoding-point-hist-map-plists
   *8859-1-8859-15-lossage* *8859-1-8859-15-lossage-rules* t
   :w-buffer-named "*MON-HELP-8859-1-8859-15-LOSSAGE*"
   :w-buffer-header ";; Lossage when converting ISO-8859-1 --> ISO-8859-15"))

;;; ==============================
;;; :CHANGESET 2421
;;; :CREATED <Timestamp: #{2011-03-07T16:30:22-05:00Z}#{11101} - by MON KEY>
;;;###autoload
(defun mon-help-cp1252-iso-8859-15-lossage ()
  "Return CP-1252 -> ISO-8859-15 character encoding conversion lossage.\n
Results returned to buffer with name: \"*MON-HELP-CP1252-8859-15-LOSSAGE*\".\n
:EXAMPLE\n\n\(mon-help-cp1252-iso-8859-1-lossage\)\n
:SEE \(URL `http://en.wikipedia.org/wiki/Windows-1252'\)
:SEE \(URL `http://en.wikipedia.org/wiki/ISO/IEC_8859-15'\)
:SEE \(URL `http://www.cs.tut.fi/~jkorpela/latin9.html'\)
:SEE \(URL `http://www.eki.ee/letter/chardata.cgi?cp=CP1252+%28Western%29&cp1=8859-15'\)
:SEE \(URL `http://www.eki.ee/letter/'\)
:SEE-ALSO ;; `*cp1252-8859-15-lossage*', `*cp1252-8859-15-lossage-rules*',
`mon-get-encoding-codepoint', `mon-get-encoding-position-lossage',
`mon-get-encoding-point-hist', `mon-get-encoding-map-results',
`mon-get-encoding-point-hist-map-plists',
`mon-make-encoding-position-lossage-table', `mon-help-char-coding-functions',
`mon-help-char-functions', `mon-help-char-composition',
`mon-help-char-raw-bytes', `mon-help-char-representation',
`mon-help-char-table-functions', `mon-help-char-unidata-table',
`mon-help-char-ascii', `mon-help-char-iso-8859-1', `mon-help-char-ecma-35',
`mon-help-char-ecma-48', `mon-help-char-logic'.\n►►►"
  (interactive)
  (mon-get-encoding-point-hist-map-plists
   *cp1252-8859-1-lossage* *cp1252-8859-15-lossage-rules* t
   :w-buffer-named "*MON-HELP-CP1252-8859-15-LOSSAGE*"
   :w-buffer-header ";; Lossage when converting Windows CP-1252 --> ISO-8859-15"))




 
;;; ==============================
;; `*8859-1-8859-15-lossage*'
;; => ((:8859-1 "\u00A4"  :CODE-PSN-ORIG #xA4  :8859-15 "\u20ac")
;;     (:8859-1 "\u00a6"  :CODE-PSN-ORIG #xa6  :8859-15 "\u0160")
;;     (:8859-1 "\u00a8"  :CODE-PSN-ORIG #xa8  :8859-15 "\u0161")
;;     (:8859-1 "\u00b4"  :CODE-PSN-ORIG #xb4  :8859-15 "\u017D")
;;     (:8859-1 "\u00b8"  :CODE-PSN-ORIG #xb8  :8859-15 "\u017E")
;;     (:8859-1 "\u00bc"  :CODE-PSN-ORIG #xbc  :8859-15 "\u0152")
;;     (:8859-1 "\u00bd"  :CODE-PSN-ORIG #xbd  :8859-15 "\u0153")
;;     (:8859-1 "\u00be"  :CODE-PSN-ORIG #xbe  :8859-15 "\u0178"))
;;; ==============================

;;; ==============================
;; `*cp1252-8859-15-lossage*'
;; => '((:CP1252 "\u201A"  :CODE-PSN-ORIG #x82  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u0192"  :CODE-PSN-ORIG #x83  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u201E"  :CODE-PSN-ORIG #x84  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2026"  :CODE-PSN-ORIG #x85  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2020"  :CODE-PSN-ORIG #x86  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2021"  :CODE-PSN-ORIG #x87  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u02C6"  :CODE-PSN-ORIG #x88  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2030"  :CODE-PSN-ORIG #x89  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2039"  :CODE-PSN-ORIG #x8B  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2018"  :CODE-PSN-ORIG #x91  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2019"  :CODE-PSN-ORIG #x92  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u201C"  :CODE-PSN-ORIG #x93  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u201D"  :CODE-PSN-ORIG #x94  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2022"  :CODE-PSN-ORIG #x95  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2013"  :CODE-PSN-ORIG #x96  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2014"  :CODE-PSN-ORIG #x97  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u02DC"  :CODE-PSN-ORIG #x98  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2122"  :CODE-PSN-ORIG #x99  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u203A"  :CODE-PSN-ORIG #x9B  :8859-15 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u0161"  :CODE-PSN-ORIG #x9A  :8859-15 "unassigned" :CODE-PSN-MOVE  #xA8)
;;      (:CP1252 "\u00A8"  :CODE-PSN-ORIG #xA8  :8859-15 "\u0161"     :CHANGE   "lost")
;;      (:CP1252 "\u017D"  :CODE-PSN-ORIG #x8E  :8859-15 "unassigned" :CODE-PSN-MOVE  #xB4)
;;      (:CP1252 "\u00B4"  :CODE-PSN-ORIG #xB4  :8859-15 "\u017D"     :CHANGE   "lost")
;;      (:CP1252 "\u0152"  :CODE-PSN-ORIG #x8C  :8859-15 "unassigned" :CODE-PSN-MOVE  #xBC)
;;      (:CP1252 "\u00BC"  :CODE-PSN-ORIG #xBC  :8859-15 "\u0152"     :CHANGE   "lost")
;;      (:CP1252 "\u0153"  :CODE-PSN-ORIG #x9C  :8859-15 "unassigned" :CODE-PSN-MOVE  #xBD)
;;      (:CP1252 "\u00BD"  :CODE-PSN-ORIG #xBD  :8859-15 "\u0153"     :CHANGE   "lost")
;;      (:CP1252 "\u0160"  :CODE-PSN-ORIG #x8A  :8859-15 "unassigned" :CODE-PSN-MOVE  #xA6)
;;      (:CP1252 "\u00A6"  :CODE-PSN-ORIG #xA6  :8859-15 "\u0160"     :CHANGE   "lost")
;;      (:CP1252 "\u017E"  :CODE-PSN-ORIG #x9E  :8859-15 "unassigned" :CODE-PSN-MOVE  #xB8)
;;      (:CP1252 "\u00B8"  :CODE-PSN-ORIG #xB8  :8859-15 "\u017E"     :CHANGE   "lost")
;;      (:CP1252 "\u20AC"  :CODE-PSN-ORIG #x80  :8859-15 "unassigned" :CODE-PSN-MOVE  #xA4)
;;      (:CP1252 "\u00A4"  :CODE-PSN-ORIG #xA4  :8859-15 "\u20AC"     :CHANGE   "lost")
;;      (:CP1252 "\u0178"  :CODE-PSN-ORIG #x9F  :8859-15 "unassigned" :CODE-PSN-MOVE  #xBE)
;;      (:CP1252 "\u00BE"  :CODE-PSN-ORIG #xBE  :8859-15 "\u0178"     :CHANGE   "lost"))
;;
;;; ==============================
;; `*cp1252-8859-1-lossage*'
;; => '((:CP1252 "\u20AC"  :CODE-PSN-ORIG #x80  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u201A"  :CODE-PSN-ORIG #x82  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u0192"  :CODE-PSN-ORIG #x83  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u201E"  :CODE-PSN-ORIG #x84  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2026"  :CODE-PSN-ORIG #x85  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2020"  :CODE-PSN-ORIG #x86  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2021"  :CODE-PSN-ORIG #x87  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u02C6"  :CODE-PSN-ORIG #x88  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2030"  :CODE-PSN-ORIG #x89  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u0160"  :CODE-PSN-ORIG #x8A  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2039"  :CODE-PSN-ORIG #x8B  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u0152"  :CODE-PSN-ORIG #x8C  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u017D"  :CODE-PSN-ORIG #x8E  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2018"  :CODE-PSN-ORIG #x91  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2019"  :CODE-PSN-ORIG #x92  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u201C"  :CODE-PSN-ORIG #x93  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u201D"  :CODE-PSN-ORIG #x94  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2022"  :CODE-PSN-ORIG #x95  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2013"  :CODE-PSN-ORIG #x96  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2014"  :CODE-PSN-ORIG #x97  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u02DC"  :CODE-PSN-ORIG #x98  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u2122"  :CODE-PSN-ORIG #x99  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u0161"  :CODE-PSN-ORIG #x9A  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u203A"  :CODE-PSN-ORIG #x9B  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u0153"  :CODE-PSN-ORIG #x9C  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u017E"  :CODE-PSN-ORIG #x9E  :8859-1 "unassigned" :CHANGE   "lost")
;;      (:CP1252 "\u0178"  :CODE-PSN-ORIG #x9F  :8859-1 "unassigned" :CHANGE   "lost"))
;;; ==============================

;;; ==============================
(provide 'mon-doc-help-char-encoding-lossage)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-doc-help-char-encoding-lossage.el ends here
;;; EOF

