;;; mon-aliases.el --- { A one line description of: mon-aliases. }
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-aliases.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-04T20:03:21-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-aliases provides { some description here. }
;;
;; FUNCTIONS:►►►
;;
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
;;
;; GROUPS:
;;
;; ALIASED/ADVISED/SUBST'D:
;; `char-or-stringp'      -> `char-or-string-p'
;; `string-or-characterp' -> `char-or-string-p'
;; `character-or-strinp'  -> `char-or-string-p'
;; `atomp'                -> `atom'
;; `macrop'               -> `apropos-macrop'
;; `make-array'           -> `make-vector'
;; `line-join-previous'   -> `delete-indentation'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; TODO:
;; - add toplevel defcustom for each mon-*.el file that provides a choice of
;;   which function names to alias.
;;
;; - add function to jump to a relevant location in this file from the file
;;   which defines the symbol being aliased. function should:
;;   -- identify enclosing line markers, where enclosing line markers are
;;      (for each relevant file) comprised of the following pair of text forms:
;;   --- each local bloc relevant to a file should: 
;;       ---- begin with a line marker: ";;; mon-*.el►►►"; 
;;       ---- end with the line marker: ";;; mon-*.el►►►";
;;   -- If no enclosing relevant file line markers are found:
;;      --- offer to create them;
;;   -- If relevant enclosing file line markers are found, narrow-to-region;
;;   --- search over the narrowed-region attempting to identify any pre-existing
;;       occurences aliases for the symbol being aliased;
;;   ---- If one is found point should be at the next beginning-of-line
;;        following the lastmost pre-existing alias;
;;   ---- If no pre-existing aliases are found, point should be at the next
;;        empty-line before the closing line marker for to the file which
;;        defined symbol;
;;   -- widen the narrowed region
;;
;;  - add function which for given a symbol to be aliased: 
;;    -- can grok whether the symbol to be aliased is variable or function;
;;    -- determines if symbol is interned (without interning symbol in so doing);
;;    -- verifies that symbol is `boundp'/`fboundp';
;;    -- verifies that aliasing symbol is not:
;;       --- already interned (without interning symbol in so doing)
;;       --- already `boundp'/`fboundp';
;;    :NOTE It isn't clear how such a function might affect etags which IIUC
;;    idetifies alias by their occurence at BOL. IOW if we programatically
;;    create aliases [ce]tags might not find them... Which FWIW is why the
;;    existing (verbose) `defalias' forms occur without indentation! 
;;    - Verify that this is true/still the case.
;;
;; NOTES:
;;
;; Following is an idiosyncratic discussion of symbol aliasing.
;;
;; Aliased symbols are one of the following:
;;  - A symbol provided by an Emacs distribution without a namespace prefix.
;;    These are denoted as: <CORE-SYMBOL>;
;;
;;  - A symbol which is not a <CORE-SYMBOL> and which does not have a namespace
;;    prefix. These are denoted as: <NON-CORE-SYMBOL>
;;
;;  - A symbol provided by an Emacs distribution with a namespace prefix.
;;    These are denoted as: <PREFIX>-<CORE-SYMBOL>;
;;
;;  - A symbol which is not a <CORE-SYMBOL> and which does have a namespace
;;    prefix. These are denoted as: <PREFIX>-<NON-CORE-SYMBOL>
;;
;; Aliasing symbols are one of the following:
;; - A symbol without a namespace prefix. 
;;   These are denoted as: <UNQUALIFIED-ALIAS>
;; 
;; - A symbol with a namespace prefix.
;;   These are denoted as: <PREFIX>-<QUALIFIED>
;;
;; An aliasing symbol can be of the form:
;;
;;   (defalias '<QUALIFIED-ALIAS>    '<CORE-SYMBOL>)
;;   (defalias '<UNQUALIFIED-ALIAS>  '<CORE-SYMBOL>)
;;   (defalias '<PREFIX>-<QUALIFIED> '<CORE-SYMBOL>)
;;   (defalias '<UNQUALIFIED-ALIAS>  '<PREFIX>-<CORE-SYMBOL>)
;;   (defalias '<PREFIX>-<QUALIFIED> '<PREFIX>-<CORE-SYMBOL>)
;;   (defalias '<UNQUALIFIED-ALIAS>  '<PREFIX>-<NON-CORE-SYMBOL>)
;;   (defalias '<PREFIX>-<QUALIFIED> '<PREFIX>-<NON-CORE-SYMBOL>)
;;   (defalias '<PREFIX>-<NON-CORE-SYMBOL> '<PREFIX>-<NON-CORE-SYMBOL>)
;;
;; An aliasing symbol may only be of type <QUALIFIED-ALIAS> when it is defined
;; within an Emacs core file and aliases a <CORE-SYMBOL>. <QUALIFIED-ALIAS>'s
;; are the only types of aliasing symbols which should shadow an existing
;; <CORE-SYMBOL> or <PREFIX>-<CORE-SYMBOL>.
;;
;; An aliasing symbol defined within a non-core file without a qualifying
;; prefix, is always of type: <UNQUALIFIED-ALIAS> independent of whether the
;; aliased <SYMBOL> is a <CORE-SYMBOL> <PREFIX>-<CORE-SYMBOL>,
;; <NON-CORE-SYMBOL>, or <PREFIX>-<NON-CORE-SYMBOL>
;;
;; An aliasing symbol is of type <PREFIX>-<NON-CORE-SYMBOL> where the defining
;; file does not "own" the "<PREFIX>-" namespace because the other symbols
;; defined within the "<PREFIX>-" namespace occur as <CORE-SYMBOL>'s as provided
;; by the core Emacs featurset. For example, in the following aliasing form:
;;
;;  (defalias 'dired-non-existent-function 'mon-existent-function)
;;
;; the aliasing symbol `dired-non-existent-function' is not defined in the
;; namespace of an Emacs core file, _but_ symbols defined in the "dired-"
;; namespace are normally considered to belong to Emacs core. In this situation
;; the aliasing symbol has encroached what is otherwise an informally priveleged
;; namespace.  Because the symbol `dired-non-existent-function' is not defined
;; in the dired namespace by an Emacs core file it should not be considered as
;; member in the set of <CORE-SYMBOL>'s and is therefor a <NON-CORE-SYMBOL> even
;; though it shares namepsace. Likewise, the symbol should not be considered to
;; be <PREFIX>-<QUALIFIED> because the defining file is not really "qualified"
;; to use the prefix when interning symbols, if the defining file were qualified
;; use of the prefix then the aliasing symbol would be of type <QUALIFIED-ALIAS>
;; and the symbol's defining file is either shadowing an existing symbol or
;; creating a new alias for an existing symbol, as it would othewise have simply
;; (re)defined the symbol instead of aliasing it.
;;
;; Following are examples of aliasing forms and corresponding denotations:
;;
;; (defalias 'string=              'string-equal)
;; (defalias '<QUALIFIED-ALIAS>    '<CORE-SYMBOL>)
;;
;; (defalias 'symbolA               'suprp)
;; (defalias '<UNQUALIFIED-ALIAS>   '<CORE-SYMBOL>)
;;
;; (defalias 'symbolB               'dired-other-frame)
;; (defalias '<UNQUALIFIED-ALIAS>   '<PREFIX>-<CORE-SYMBOL>)
;; 
;; (defalias 'mon-symbolC           'suprp)
;; (defalias '<PREFIX>-<QUALIFIED>  '<CORE-SYMBOL>)
;;
;; (defalias 'mon-symbolD           'dired-other-frame)
;; (defalias '<PREFIX>-<QUALIFIED>  '<PREFIX>-<CORE-SYMBOL>)
;;
;; (defalias 'symbolE               'make-doctor-variables)
;; (defalias '<UNQUALIFIED-ALIAS>   '<NON-CORE-SYMBOL>)
;;
;; (defalias 'mon-symbolF           'make-doctor-variables)
;; (defalias '<PREFIX>-<QUALIFIED>  '<NON-CORE-SYMBOL>)
;;
;; (defalias 'symbolG               'mon-some-symbol)
;; (defalias '<UNQUALIFIED-ALIAS>   '<PREFIX>-<NON-CORE-SYMBOL>)
;;
;; (defalias 'mon-symbolH           'mon-some-symbol)
;; (defalias '<PREFIX>-<QUALIFIED>  '<PREFIX>-<NON-CORE-SYMBOL>)
;;
;; (defalias 'dired-symbolI              'mon-symbolI) 
;; (defalias '<PREFIX>-<NON-CORE-SYMBOL> '<PREFIX>-<NON-CORE-SYMBOL>)
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-aliases.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-aliases. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-04T20:03:21-04:00Z}#{10444} - by MON KEY>
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
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;;; :NOTE Lines below begginig w/ "(defalias" occur without indentation for etags
;;; which likes to find them at BOL.

;;; ==============================
;;; mon-text-property-utils.el►►►
;;; ==============================

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "mon-get-text-properties-region->kill-ring" obarray)
             (fboundp 'mon-get-text-properties-region->kill-ring))
(defalias 'mon-get-text-properties-region->kill-ring 'mon-get-text-properties-region-to-kill-ring))
;;
(unless (and (intern-soft "mon-kill-ring-save-w-props" obarray)
            (fboundp 'mon-kill-ring-save-w-props))
(defalias 'mon-kill-ring-save-w-props 'mon-get-text-properties-region-to-kill-ring))

(unless (and (intern-soft "mon-help-face-next-property-change" obarray) 
             (fboundp 'mon-help-face-next-property-change))
(defalias 'mon-help-face-next-property-change 'mon-get-next-face-property-change))
;;
(unless (and (intern-soft "mon-remove-text-with-property" obarray)
             (fboundp 'mon-remove-text-with-property))
(defalias 'mon-remove-text-with-property 'mon-get-text-property-remove-all))
;;
(unless (and (intern-soft "with-print-gensyms" obarray) 
             (fboundp 'with-print-gensyms))
(defalias 'with-print-gensyms 'mon-with-print-gensyms))

;;; ==============================
;;; mon-text-property-utils.el►►►
;;; ==============================

;;; ==============================
;;; mon-utils.el►►►
;;; ==============================

;;; ==============================
;;; <UNQUALIFIED-ALIAS> <CORE-SYMBOL>
;;; ==============================

;;; ==============================
;;; :NOTE Is it possible to implement a light-weight CL style `make-array' which
;;; specializes on a keyword argument for an `:element-type`, e.g. 'bit, 'sbit,
;;; 'character? Could such a thing maybe leverage `make-char-table' or
;;; `make-keymap' for displaced arrays?. Maybe for signed-byte use a buffer
;;; markder combo as an index into an adjustable array and position-bytes as its
;;; fill-pointer barring unibyte/multibyte/EOL/encoding issues of course :P
;;;
;;; <UNQUALIFIED-ALIAS> <CORE-SYMBOL>
(unless (and (intern-soft "make-array" obarray)
             (fboundp 'make-array))
(defalias 'make-array 'make-vector))
;;
(unless (and (intern-soft "atomp" obarray) 
             (fboundp (intern-soft "atomp" obarray)))
(defalias 'atomp 'atom))
;;
(unless (and (intern-soft "line-join-previous" obarray)
             (fboundp 'line-join-previous))
(defalias 'line-join-previous 'delete-indentation))
;;

(unless (and (intern-soft "stringp-or-null" obarray) 
             (fboundp 'stringp-or-null)) 
(defalias 'stringp-or-null 'string-or-null-p))
;;
(unless (and (intern-soft "char-or-stringp" obarray) 
             (fboundp 'char-or-stringp))
(defalias 'char-or-stringp 'char-or-string-p))
;;
(unless (and (intern-soft "string-or-characterp" obarray) 
             (fboundp 'string-or-characterp))
(defalias 'string-or-characterp 'char-or-string-p))
;;
(unless (and (intern-soft "character-or-strinp" obarray) 
             (fboundp 'character-or-strinp))
(defalias 'character-or-strinp 'char-or-string-p))

;;; ==============================
;;;  <UNQUALIFIED-ALIAS>  <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

;;; ==============================
;;; :NOTE `byte-compile-arglist-warn' has a MACROP parameter.
;;; :CHANGESET 2119
;;; :CREATED <Timestamp: #{2010-09-13T17:30:20-04:00Z}#{10371} - by MON KEY>
;;; <UNQUALIFIED-ALIAS> <PREFIX>-<CORE-SYMBOL>
(unless (and (intern-soft "macrop" obarray) 
             (fboundp (intern-soft "macrop" obarray)))
(defalias 'macrop 'apropos-macrop))
;;
(when (and (intern-soft "IS-MON-SYSTEM-P" obarray)
           (bound-and-true-p IS-MON-SYSTEM-P))
  (unless (and (intern-soft "buffer-narrowed-p" obarray)
               (fboundp 'buffer-narrowed-p))
(defalias 'buffer-narrowed-p 'mon-buffer-narrowed-p)))
;;
(unless (and (intern-soft "nshuffle-vector" obarray)
             (fboundp (intern-soft "nshuffle-vector" obarray)))
(defalias 'nshuffle-vector 'mon-nshuffle-vector))
;;
(when (and (intern-soft "IS-MON-SYSTEM-P" obarray) 
           (not (and (intern-soft "with-gensyms" obarray)
                     (fboundp (intern-soft "with-gensyms" obarray)))))
(defalias 'with-gensyms 'mon-with-gensyms))
;;
(when (and (intern-soft "IS-MON-SYSTEM-P" obarray)
           (bound-and-true-p IS-MON-SYSTEM-P)
           (if (intern-soft "buffer-exists-p" obarray)
               (not (fboundp (intern-soft "buffer-exists-p" obarray)))
             t))
(defalias 'buffer-exists-p 'mon-buffer-exists-p))
;;
(unless (and (intern-soft "string-or-null-and-zerop" obarray) 
             (fboundp 'string-or-null-and-zerop))
(defalias 'string-or-null-and-zerop 'mon-string-or-null-and-zerop))
;;
(unless (and (intern-soft "stringp-and-zerop-or-null" obarray) 
             (fboundp 'stringp-and-zerop-or-null))
(defalias 'stringp-and-zerop-or-null 'mon-string-or-null-and-zerop))
;;
(unless (and (intern-soft "string-not-null-or-zerop" obarray) 
             (fboundp 'string-not-null-or-zerop))
(defalias 'string-not-null-or-zerop 'mon-string-not-null-nor-zerop))
;;
(unless (and (intern-soft "stringp-not-null-nor-zerop" obarray) 
             (fboundp 'stringp-not-null-nor-zerop))
(defalias 'stringp-not-null-nor-zerop 'mon-string-not-null-nor-zerop))
;;
(unless (and (intern-soft "zerop-or-one" obarray)
             (fboundp 'zerop-or-one))
(defalias 'zerop-or-one 'mon-zero-or-onep))
;;
(unless (and (intern-soft "next-almost-prime" obarray)
             (fboundp 'next-almost-prime))
(defalias 'next-almost-prime 'mon-next-almost-prime))
;;
(unless (and (intern-soft "delq-dups" obarray)
             (fboundp 'delq-dups))
(defalias 'delq-dups 'mon-delq-dups))
;;
(unless (and (intern-soft "proper-list-p" obarray)
             (fboundp 'proper-list-p))
(defalias 'proper-list-p 'mon-list-proper-p))

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "mon-line-keep-match" obarray) 
             (fboundp 'mon-line-keep-match))
(defalias 'mon-line-keep-match  'keep-lines))
;;
(unless (and (intern-soft "mon-line-delete-match" obarray) 
             (fboundp 'mon-line-delete-match))
(defalias 'mon-line-delete-match 'flush-lines))
;;
(unless (and (intern-soft "mon-line-count-match" obarray) 
             (fboundp 'mon-line-count-match))
(defalias 'mon-line-count-match  'how-many))
;;
(unless (and (intern-soft "mon-line-join-previous" obarray) 
             (fboundp 'mon-line-join-nextline))
(defalias 'mon-line-join-previous 'delete-indentation))
;;
;;; ==============================
;;; :NOTE Alias these and don't forget to use them!
;;; :CREATED <Timestamp: Wednesday July 01, 2009 @ 06:32.08 PM - by MON KEY>
(unless (and (intern-soft "mon-string-combine-and-quote" obarray)
             (fboundp 'mon-string-combine-and-quote))
(defalias 'mon-string-combine-and-quote 'combine-and-quote-strings))
;;
(unless (and (intern-soft "mon-string-split-and-unquote" obarray)
             (fboundp 'mon-string-split-and-unquote))
(defalias 'mon-string-split-and-unquote 'split-string-and-unquote))
;;
(unless (and (intern-soft "mon-replace-char-in-region" obarray)
             (fboundp 'mon-replace-char-in-region))
(defalias 'mon-replace-char-in-region 'subst-char-in-region))
;;
;;; ==============================
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T17:16:18-04:00Z}#{10381} - by MON KEY>
(unless (and (intern-soft "mon-delq-alist" obarray)
             (fboundp 'mon-delq-alist))
(defalias 'mon-delq-alist 'assq-delete-all))
;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-02T12:47:38-05:00Z}#{10092} - by MON KEY>
;;; `store-substring' <- mule-util.el
(unless (and (intern-soft "mon-string-set-char-at-idx" obarray) 
             (fboundp 'mon-string-set-char-at-idx))
(defalias 'mon-string-set-char-at-idx 'store-substring
 "Set OBJ \(string or character\) at index IDX of STRING.\n
:EXAMPLE\n\n\(length \"bubba\"\)\n
\(mon-string-set-char-at-idx \"bubba\" 4 \"s\"\)\n
\(mon-string-set-char-at-idx \"bubba\" 4 \\=?s\)\n
\(mon-string-set-char-at-idx \"bubba\" 5 \\=?s\) ;out of bounds\n
:NOTE This function appears to retain existing text-properties.\n
:SEE-ALSO `aset', `aref', `vconcat', `string-to-list', `string-to-vector'.\n►►►"))
;;
(unless (and (intern-soft "mon-string-insert-string-at-idx" obarray) 
              (fboundp 'mon-string-insert-string-at-idx))
(defalias 'mon-string-insert-string-at-idx 'store-substring
  "Set OBJ (string or character) at index IDX of STRING.\n
:EXAMPLE\n\n\(length \"bubba\"\)\n
\(store-substring \"bubba\" 0 \"B\"\)
\(store-substring \"bubba\" 0 \" a\"\)
\(store-substring \"bubba\" 0 \"bubba\"\)
\(store-substring \"bubba\" 4 \"ba\"\)      ;out of bounds
\(store-substring \"bubba\" 3 \"bas\"\)     ;out of bounds
:NOTE This function appears to retain existing text-properties.\n
:SEE-ALSO `aset', `aref', `vconcat', `string-to-list', `string-to-vector'.\n►►►"))

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<CORE-SYMBOL>
;;; ==============================

;;; ==============================
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T16:21:29-04:00Z}#{10381} - by MON KEY>
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<CORE-SYMBOL>
(unless (and (intern-soft "mon-skip-whitespace" obarray)
             (fboundp 'mon-skip-whitespace))
(defalias 'mon-skip-whitespace 'edebug-skip-whitespace
    "Leave point before the next token, skipping white space and comments.\n
Skipping is as if by `skip-chars-forward' not `skip-syntax-forward'.\n
Skips over following chars:\n
 SPC \(char 32\), TAB \(char 9\), LF \(char 10\), FF \(char 12\), CR \(char 13\)\n
:EXAMPLE\n\n\(mon-skip-whitespace\) !\n
\(mon-skip-whitespace\)	!\n
\(mon-skip-whitespace\)
	!\n
\(mon-skip-whitespace\)
\xd !\n
\(mon-skip-whitespace\)
\xc !\n
:SEE-ALSO `following-char', `char-after', `mon-cln-BIG-whitespace',
`mon-cln-trail-whitespace', `mon-cln-whitespace', `mon-insert-whitespace',
`mon-kill-whitespace', `*mon-whitespace-chars*', `*regexp-whitespace-chars*'.\n►►►"))
;;
;;; ==============================
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T16:20:53-04:00Z}#{10381} - by MON KEY>
(unless (and (intern-soft "mon-save-restricton" obarray)
             (fboundp (intern-soft "mon-save-restricton" obarray)))
(defalias 'mon-save-restricton 'edebug-save-restriction))
;;
(unless (and (intern-soft "mon-sort-alist" obarray)
             (fboundp 'mon-sort-alist))
(defalias 'mon-sort-alist 'edebug-sort-alist
    "Return the ALIST sorted with comparison function FUNCTION.\n
This uses 'sort so the sorting is destructive.
:EXAMPLE\n\n
:SEE-ALSO `mon-delq-alist'.\n►►►"))
;;
(unless (and (intern-soft "mon-macrop" obarray) 
             (fboundp (intern-soft "mon-macrop" obarray)))
(defalias 'mon-macrop 'apropos-macrop))
;;
(unless (and (intern-soft "mon-string-prefix-p" obarray)
             (fboundp 'mon-string-prefix-p))
(defalias 'mon-string-prefix-p 'vc-string-prefix-p
    "Return non-nil when STRING has PREFIX.\n
:EXAMPLE\n\n\(mon-string-prefix-p \"mon-\" \"mon-string-prefix-p\"\)\n
\(mon-string-prefix-p \"mon-\" \"string-mon-prefix-p\"\)\n
:SEE-ALSO .\n►►►"))

;;
;;; ==============================
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-23T11:30:09-04:00Z}#{10384} - by MON KEY>
(when (and (intern-soft "ido-chop" obarray) (fboundp 'ido-chop))
  (unless (and (intern-soft "mon-list-chop" obarray) 
               (fboundp 'mon-list-chop))
(defalias 'mon-list-chop 'ido-chop
      "Remove all elements before ELEM and put them at the end of ITEMS.\n
:EXAMPLE\n\n\(mon-list-chop '\(a b c d e f\) 'd\)\n
\(mon-list-chop '\(a b \"c\" d e f) \"c\"\)\n
\(mon-list-chop '\(a b (c \"c\" \"d\") d e f) '(c \"c\" \"d\"\)\)\n
\(mon-list-chop '\(a b [c \"c\" \"d\"] d e f\) '[c \"c\" \"d\"]\)
:SEE-ALSO .\n►►►")))

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "mon-buffer-do-with-undo-disabled" obarray)
             (fboundp (intern-soft "mon-buffer-do-with-undo-disabled" obarray)))
(defalias 'mon-buffer-do-with-undo-disabled 'mon-with-buffer-undo-disabled))
;;
(unless (and (intern-soft "mon-window-get-if-buffer" obarray)
             (fboundp (intern-soft "mon-window-get-if-buffer" obarray)))
(defalias 'mon-window-get-if-buffer 'mon-get-buffer-window-if))
;;
(unless (and (intern-soft "get-buffer-window-if" obarray)
             (fboundp (intern-soft "get-buffer-window-if" obarray)))
(defalias 'get-buffer-window-if 'mon-get-buffer-window-if))
;;
(unless (and (intern-soft "mon-buffer-name-print-readably" obarray)
             (fboundp (intern-soft "mon-buffer-name-print-readably" obarray)))
(defalias 'mon-buffer-name-print-readably 'mon-print-buffer-object-readably))
;;
(unless (and (intern-soft "mon-buffer-get-w-mode" obarray)
             (fboundp (intern-soft "mon-buffer-get-w-mode" obarray)))
(defalias 'mon-buffer-get-w-mode 'mon-get-buffer-w-mode))
;;
(unless (and (intern-soft "mon-functionp" obarray)
             (fboundp 'mon-functionp))
(defalias 'mon-functionp 'mon-function-object-p))
;;
(unless (and (intern-soft "mon-one-or-zerop" obarray)
              (fboundp 'mon-one-or-zerop))
(defalias 'mon-one-or-zerop 'mon-zero-or-onep))
;;
(unless (and (intern-soft "mon-1-or-0-p" obarray)
             (fboundp 'mon-1-or-0-p))
(defalias 'mon-1-or-0-p 'mon-zero-or-onep))
;;
(unless (and (intern-soft "mon-1-or-0-p" obarray)
             (fboundp 'mon-1-or-0-p))
(defalias 'mon-1-or-0-p 'mon-zero-or-onep))
;;
(unless (and (intern-soft "mon-zerop-or-one" obarray)
             (fboundp 'mon-zerop-or-one))
(defalias 'mon-zerop-or-one 'mon-zero-or-onep))
;;
(unless (and (intern-soft "mon-boolean-to-binary" obarray)
             (fboundp 'mon-boolean-to-binary))
(defalias 'mon-boolean-to-binary 'mon-booleanp-to-binary))
;;
(unless (and (intern-soft "mon-t-to-1" obarray)
             (fboundp 'mon-t-to-1))
(defalias 'mon-t-to-1 'mon-booleanp-to-binary))
;;
(unless (and (intern-soft "mon-true-to-one" obarray)
             (fboundp 'mon-true-to-one))
(defalias 'mon-true-to-one 'mon-booleanp-to-binary))
;;
(unless (and (intern-soft "mon-nil-to-0" obarray)
             (fboundp 'mon-nil-to-0))
(defalias 'mon-nil-to-0 'mon-booleanp-to-binary))
;;
(unless (and (intern-soft "mon-false-to-zero" obarray)
             (fboundp 'mon-false-to-zero))
(defalias 'mon-false-to-zero 'mon-booleanp-to-binary))
;;
(unless (and (intern-soft "mon-buffer-make-shell" obarray)
             (fboundp 'mon-buffer-make-shell))
(defalias 'mon-buffer-make-shell 'mon-make-shell-buffer))
;;
(unless (and (intern-soft "mon-buffer-get-shell" obarray)
             (fboundp 'mon-buffer-get-shell))
(defalias 'mon-buffer-get-shell 'mon-shell))
;;
(unless (and (intern-soft "mon-buffer-get-scratch" obarray)
             (fboundp 'mon-buffer-get-scratch))
(defalias 'mon-buffer-get-scratch 'mon-scratch))
;;
(unless (and (intern-soft "mon-buffer-get-messages" obarray)
             (fboundp 'mon-buffer-get-messages))
(defalias 'mon-buffer-get-messages 'mon-switch-to-messages))
;;
(unless (and (intern-soft "mon-append-next-kill" obarray)
             (fboundp 'mon-append-next-kill))
(defalias 'mon-append-next-kill 'mon-kill-appending))
;;
(unless (and (intern-soft "mon-buffer-kill-completions" obarray)
             (fboundp 'mon-buffer-kill-completions))
(defalias 'mon-buffer-kill-completions 'mon-kill-completions))
;;
(unless (and (intern-soft "mon-window-flip" obarray)
             (fboundp 'mon-window-flip))
(defalias 'mon-window-flip 'mon-flip-windows))
;;
(unless (and (intern-soft "mon-window-split-horiz" obarray)
             (fboundp 'mon-window-split-horiz))
(defalias 'mon-window-split-horiz 'mon-twin-horizontal))
;;
(unless (and (intern-soft "mon-window-split-vert" obarray)
             (fboundp 'mon-window-split-vert))
(defalias 'mon-window-split-vert 'mon-twin-vertical))
;;
(unless (and (intern-soft "mon-register-append" obarray)
             (fboundp 'mon-register-append))
(defalias 'mon-register-append 'mon-append-to-register))
;;
(unless (and (intern-soft "mon-buffer-append-to" obarray)
             (fboundp 'mon-buffer-append-to))
(defalias 'mon-buffer-append-to 'mon-append-to-buffer))
;;
(unless (and (intern-soft "mon-buffer-end" obarray)
             (fboundp 'mon-buffer-end))
(defalias 'mon-buffer-end 'mon-g2be))
;;
(unless (and (intern-soft "mon-capitalize-region" obarray)
              (fboundp (intern-soft "mon-capitalize-region" obarray)))
(defalias 'mon-capitalize-region 'mon-region-capitalize))
;;
(unless (and (intern-soft "mon-region-reverse-chars" obarray)
              (fboundp (intern-soft "mon-region-reverse-chars" obarray)))
(defalias 'mon-region-reverse-chars 'mon-region-reverse))
;; 
(unless (and (intern-soft "mon-read-keys-last-event" obarray)
             (fboundp 'mon-read-keys-last-event))
(defalias 'mon-read-keys-last-event 'mon-test-keypresses))
;; 
(unless (and (intern-soft "mon-string-from-keyboard-input" obarray)
             (fboundp 'mon-string-from-keyboard-input))
(defalias 'mon-string-from-keyboard-input 'mon-read-keys-as-string))
;;
(unless (and (intern-soft "mon-get-hidden-buffers" obarray)
             (fboundp 'mon-get-hidden-buffers))
(defalias 'mon-get-hidden-buffers 'mon-get-buffer-hidden))
;;
(unless (and (intern-soft "mon-help-hidden-buffers" obarray)
             (fboundp 'mon-help-hidden-buffers))
(defalias 'mon-help-hidden-buffers 'mon-get-buffer-hidden))
;;
(unless (and (intern-soft "mon-buffer-get-hidden" obarray)
             (fboundp 'mon-buffer-get-hidden))
(defalias 'mon-buffer-get-hidden   'mon-get-buffer-hidden))
;;
(unless (and (intern-soft "mon-region-wrap" obarray)
             (fboundp 'mon-region-wrap))
(defalias 'mon-region-wrap 'mon-wrap-selection))
;;
(unless (and (intern-soft "mon-split-string" obarray)
             (fboundp 'mon-split-string))
(defalias 'mon-split-string 'mon-string-split))
;;
(unless (and (intern-soft "mon-string->symbol" obarray) 
             (fboundp 'mon-string->symbol))
(defalias 'mon-string->symbol 'mon-string-to-symbol))
;;
(unless (and (intern-soft "mon-symbol->string" obarray) 
             (fboundp 'mon-symbol->string))
(defalias 'mon-symbol->string    'mon-symbol-to-string))
;;
(unless (and (intern-soft "mon-string-from-symbol" obarray) 
             (fboundp 'mon-string-from-symbol))
(defalias 'mon-string-from-symbol 'mon-symbol-to-string))
;;
(unless (and (intern-soft "mon-string<-symbol" obarray)
             (fboundp 'mon-string<-symbol))
(defalias 'mon-string<-symbol    'mon-symbol-to-string))
;;
(unless (and (intern-soft "mon-sequence-to-string" obarray)
             (fboundp 'mon-sequence-to-string))
(defalias 'mon-sequence-to-string 'mon-string-from-sequence))
;;
(unless (and (intern-soft "mon-seq->string" obarray)
             (fboundp 'mon-seq->string))
(defalias 'mon-seq->string 'mon-string-from-sequence))
;;
(unless (and (intern-soft "mon-string-suffix-p" obarray) 
             (fboundp 'mon-string-suffix-p))
(defalias 'mon-string-suffix-p 'mon-string-has-suffix))
;;
(unless (and (intern-soft "mon-replace-char-in-string" obarray) 
             (fboundp 'mon-replace-char-in-string))
(defalias 'mon-replace-char-in-string 'mon-string-replace-char))
;;
(unless (and (intern-soft "mon-remove-char-in-string" obarray) 
             (fboundp 'mon-remove-char-in-string))
(defalias 'mon-remove-char-in-string 'mon-string-replace-char))
;;
(unless (and (intern-soft "mon-indent-lines-from-to-col" obarray)
             (fboundp 'mon-indent-lines-from-to-col))
(defalias 'mon-indent-lines-from-to-col 'mon-line-indent-from-to-col))
;;
(unless (and (intern-soft "mon-buffer-get-word-list" obarray)
              (fboundp (intern-soft "mon-buffer-get-word-list" obarray)))
(defalias 'mon-buffer-get-word-list 'mon-word-get-list-in-buffer))
;;
(unless (and (intern-soft "mon-region-reverse-words" obarray)
             (fboundp 'mon-region-reverse-words))
(defalias 'mon-region-reverse-words 'mon-word-reverse-region))
;;
(unless (and (intern-soft "mon-reverse-region-words" obarray)
              (fboundp (intern-soft "mon-reverse-region-words" obarray)))
(defalias 'mon-reverse-region-words 'mon-word-reverse-region))
;; 
(unless (and (intern-soft "mon-buffer-get-word-count" obarray) 
             (fboundp 'mon-buffer-get-word-count))
(defalias 'mon-buffer-get-word-count 'mon-word-count-occurrences))
;;
(unless (and (intern-soft "mon-get-next-almost-prime" obarray)
             (fboundp 'mon-get-next-almost-prime))
(defalias 'mon-get-next-almost-prime 'mon-next-almost-prime))
;;  :)
(unless (and (intern-soft "mon-generate-wonky" obarray) 
             (fboundp 'mon-generate-wonky))
(defalias 'mon-generate-wonky 'mon-string-wonkify))
;;
(unless (and (intern-soft "mon-hex-list-as-string" obarray) 
             (fboundp 'mon-hex-list-as-string))
(defalias 'mon-hex-list-as-string 'mon-string-from-hex-list))
;;
(unless (and (intern-soft "mon-list-ify-bool-vector" obarray)
             (fboundp 'mon-list-ify-bool-vector))
(defalias 'mon-list-ify-bool-vector   'mon-bool-vector-pp))
;;
(unless (and (intern-soft "mon-bool-vector-to-list" obarray)
             (fboundp 'mon-bool-vector-to-list))
(defalias 'mon-boolean-vector-to-list 'mon-bool-vector-pp))
;;
(unless (and (intern-soft "mon-bool-vector-to-list" obarray)
             (fboundp 'mon-bool-vector-to-list))
(defalias 'mon-bool-vector-to-list    'mon-bool-vector-pp))
;;
;; :NOTE This is a bad name but its hard to fit it in with my other mnemonics :(
(unless (and (intern-soft "mon-byte-table-bits" obarray)
             (fboundp 'mon-byte-table-bits))
(defalias 'mon-byte-table-bits 'mon-get-bit-table))
;;
(unless (and (intern-soft "mon-bit-table-bits" obarray)
             (fboundp 'mon-bit-table-bits))
(defalias 'mon-bit-table-bits  'mon-get-bit-table))
;;
(unless (and (intern-soft "mon-rectangle-kill-w-longest-line" obarray)
             (fboundp 'mon-rectangle-kill-w-longest-line))
(defalias 'mon-rectangle-kill-w-longest-line 'mon-kill-rectangle-w-beer-belly))
;;
(unless (and (intern-soft "mon-get-window-plist" obarray)
             (fboundp 'mon-get-window-plist))
(defalias 'mon-get-window-plist 'mon-map-windows->plist))
;;
(unless (and (intern-soft "mon-window-map-active-to-plist" obarray)
             (fboundp 'mon-window-map-active-to-plist))
(defalias 'mon-window-map-active-to-plist 'mon-map-windows->plist))
;;
(unless (and (intern-soft "mon-list-all-booleanp" obarray)
             (fboundp 'mon-list-all-booleanp))
(defalias 'mon-list-all-booleanp 'mon-sequence-all-booleanp))
;;
(unless (and (intern-soft "mon-list-delq-dups" obarray)
             (fboundp 'mon-list-delq-dups))
(defalias 'mon-list-delq-dups 'mon-delq-dups))
;; 
(unless (and (intern-soft "mon-delete-dups-eql" obarray) 
             (fboundp 'mon-delete-dups-eql))
(defalias 'mon-delete-dups-eql 'mon-deleql-dups))
;;
(unless (and (intern-soft "mon-list-deleql-dups" obarray) 
             (fboundp 'mon-list-deleql-dups))
(defalias 'mon-list-deleql-dups 'mon-deleql-dups))
;;
(unless (and (intern-soft "mon-list-delete-first" obarray)
             (fboundp    'mon-list-delete-first))
(defalias 'mon-list-delete-first 'mon-delete-first))
;;
(unless (and (intern-soft "mon-list-remove-dups" obarray)
             (fboundp 'mon-list-remove-dups))
(defalias 'mon-list-remove-dups 'mon-remove-dups))
;;
(unless (and (intern-soft "mon-proper-list-p" obarray)
             (fboundp 'mon-proper-list-p))
(defalias 'mon-proper-list-p 'mon-list-proper-p))
;;
(unless (and (intern-soft "mon-list-mappable-p" obarray) 
             (fboundp 'mon-list-mappable-p))
(defalias 'mon-list-mappable-p 'mon-sequence-mappable-p))
;;
(unless (and (intern-soft "mon-mappable-sequence-p" obarray) 
             (fboundp 'mon-mappable-sequence-p))
(defalias 'mon-mappable-sequence-p 'mon-sequence-mappable-p))
;;
(unless (and (intern-soft "mon-reorder-vector" obarray) 
             (fboundp 'mon-reorder-vector)) 
(defalias 'mon-reorder-vector 'mon-list-reorder))
;;
(unless (and (intern-soft "mon-sequence-reorder" obarray) 
             (fboundp 'mon-sequence-reorder))
(defalias 'mon-sequence-reorder 'mon-list-reorder))
;;
(unless (and (intern-soft "mon-list-union" obarray)
             (fboundp 'mon-list-union))
(defalias 'mon-list-union 'mon-union))
;;
(unless (and (intern-soft "mon-list-member-if" obarray) 
             (fboundp 'mon-list-member-if))
(defalias 'mon-list-member-if 'mon-member-if))
;;
(unless (and (intern-soft "mon-list-delete-if" obarray) 
             (fboundp 'mon-list-delete-if)) 
(defalias 'mon-list-delete-if 'mon-delete-if))
;;
(unless (and (intern-soft "mon-list-remove-if-not" obarray)
             (fboundp 'mon-list-remove-if-not))
(defalias 'mon-list-remove-if-not 'mon-remove-if-not))
;;
(unless (and (intern-soft "mon-list-remove-if" obarray)
             (fboundp 'mon-list-remove-if))
(defalias 'mon-list-remove-if 'mon-remove-if))
;;
(unless (and (intern-soft "mon-list-intersect" obarray) 
             (fboundp 'mon-list-intersect))
(defalias 'mon-list-intersect 'mon-intersection))
;;
(unless (and (intern-soft "mon-list-set-diff" obarray) 
             (fboundp 'mon-list-set-diff)) 
(defalias 'mon-list-set-diff 'mon-set-difference))
;;
(unless (and (intern-soft "mon-list-nqueue" obarray) 
             (fboundp 'mon-list-nqueue))
(defalias 'mon-list-nqueue 'mon-moveq))
;;
(unless (and (intern-soft "mon-list-flatten" obarray)
             (fboundp (intern-soft "mon-list-flatten" obarray)))
(defalias 'mon-list-flatten 'mon-flatten))
;;
(unless (and (intern-soft "mon-rotate-flatten-list" obarray)
             (fboundp 'mon-rotate-flatten-list))
(defalias 'mon-rotate-flatten-list 'mon-list-flatten-rotated))
;;
(unless (and (intern-soft "mon-list-mismatch" obarray) 
             (fboundp 'mon-list-mismatch)) 
(defalias 'mon-list-mismatch 'mon-mismatch))
;;
(unless (and (intern-soft "mon-list-combine" obarray)
             (fboundp 'mon-list-combine))
(defalias 'mon-list-combine 'mon-combine))
;;
(unless (and (intern-soft "mon-map-combine" obarray)
             (fboundp 'mon-map-combine))
(defalias 'mon-map-combine 'mon-combine))
;;
(unless (and (intern-soft "mon-list-recurse-apply" obarray) 
             (fboundp 'mon-list-recurse-apply)) 
(defalias 'mon-list-recurse-apply 'mon-recursive-apply))
;;
(unless (and (intern-soft "mon-merge-list" obarray)
             (fboundp 'mon-merge-list))
(defalias 'mon-merge-list 'mon-list-merge))
;;
(unless (and (intern-soft "mon-string-escape-lisp-region" obarray)
             (fboundp 'mon-string-escape-lisp-region))
(defalias 'mon-string-escape-lisp-region 'mon-escape-lisp-string-region))
;;
(unless (and (intern-soft "mon-lisp-escape-region" obarray)
             (fboundp 'mon-lisp-escape-region))
(defalias 'mon-lisp-escape-region 'mon-escape-lisp-string-region))
;;
(unless (and (intern-soft "mon-string-unescape-lisp-region" obarray)
             (fboundp 'mon-string-unescape-lisp-region))
(defalias 'mon-string-unescape-lisp-region 'mon-unescape-lisp-string-region))
;;
(unless (and (intern-soft "mon-lisp-unescape-region" obarray)
             (fboundp 'mon-lisp-unescape-region))
(defalias 'mon-lisp-unescape-region 'mon-unescape-lisp-string-region))

;;; ==============================
;;; mon-utils.el◄◄◄
;;; ==============================

;;; ==============================
;;; mon-dir-utils.el►►►
;;; ==============================


;;; ==============================
;;; <UNQUALIFIED-ALIAS> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "file-truename-p" obarray) 
             (fboundp 'file-truename-p))
(defalias 'file-truename-p 'mon-file-truename-p))
;;
(unless (and (intern-soft "file-attributes-plist" obarray) 
             (fboundp 'file-attributes-plist))
(defalias 'file-attributes-plist 'mon-file-dir-attributes->plist))
;;
(unless (and (intern-soft "directory-attributes-plist" obarray)
             (fboundp 'directory-attributes-plist))
(defalias 'directory-attributes-plist 'mon-file-dir-attributes->plist))
;;
(unless (and (intern-soft "find-buffer-visiting-other-live-frame" obarray) 
             (fboundp 'find-buffer-visiting-other-live-frame)) 
(defalias 'find-buffer-visiting-other-live-frame 'mon-find-buffer-visiting-other-live-frame))
;;
(unless (and (intern-soft "get-buffer-visiting-other-live-frame" obarray) 
             (fboundp 'get-buffer-visiting-other-live-frame))
(defalias 'get-buffer-visiting-other-live-frame 'mon-find-buffer-visiting-other-live-frame))
;;
(unless (and (intern-soft "frame-live-visible-graphic-p" obarray)
             (fboundp 'frame-live-visible-graphic-p))
(defalias 'frame-live-visible-graphic-p 'mon-frame-live-visible-graphic-p))

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <CORE-SYMBOL>
;;; ==============================

;;; ==============================
;;; :NOTE For whatever reason I can't recall that rename-file also moves it...
(unless (and (intern-soft "mon-move-file" obarray) 
             (fboundp 'mon-move-file))
(defalias 'mon-move-file 'rename-file))
;;
(unless (and (intern-soft "mon-mv-file" obarray) 
             (fboundp 'mon-mv-file))
(defalias 'mon-mv-file 'rename-file))
;;
(unless (and (intern-soft "mon-file-move" obarray) 
             (fboundp 'mon-file-move))
(defalias 'mon-file-move 'rename-file))
;;
(unless (and (intern-soft "mon-file-mv" obarray) 
             (fboundp 'mon-file-mv)) 
(defalias 'mon-file-mv 'rename-file))

(unless (and (intern-soft "mon-rename-file" obarray)
             (fboundp 'mon-rename-file))
(defalias 'mon-rename-file 'rename-file))
;;
(unless (and (intern-soft "mon-rnm-file" obarray)
             (fboundp 'mon-rnm-file)) 
(defalias 'mon-rnm-file 'rename-file))
;;
(unless (and (intern-soft "mon-file-rename" obarray) 
             (fboundp 'mon-file-rename))
(defalias 'mon-file-rename 'rename-file))
;;
(unless (and (intern-soft "mon-file-rnm" obarray) 
             (fboundp 'mon-file-rnm))
(defalias 'mon-file-rnm 'rename-file))

;;; ==============================
;;; <PREFIX>-<NON-CORE-SYMBOL> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================
(unless (and (intern-soft "dired-find-file-other-frame" obarray)
             (fboundp 'dired-find-file-other-frame))
(defalias 'dired-find-file-other-frame 'mon-dired-find-file-other-frame))
;;
(unless (and (intern-soft "dired-up-here" obarray)
             (fboundp 'dired-up-here))
(defalias 'dired-up-here 'mon-dired-up-directory-this-buffer))
;;
(unless (and (intern-soft "dired-uninsert-subdir" obarray)
             (fboundp 'dired-uninsert-subdir))
(defalias 'dired-uninsert-subdir 'mon-dired-uninsert-subdir))
;;
(unless (and (intern-soft "dired-subdir-uninsert" obarray)
             (fboundp 'dired-subdir-uninsert))
(defalias 'dired-subdir-uninsert 'mon-dired-uninsert-subdir))
;;
(unless (and (intern-soft "dired-uninsert-subdir-all" obarray)
             (fboundp 'dired-uninsert-subdir-all))
(defalias 'dired-uninsert-subdir-all 'mon-dired-uninsert-subdir-all))
;;
(unless (and (intern-soft "dired-subdir-uninsert-all" obarray)
             (fboundp 'dired-subdir-uninsert-all))
(defalias 'dired-subdir-uninsert-all 'mon-dired-uninsert-subdir-all))

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================
(unless (and (intern-soft "mon-get-buffer-visiting-other-live-frame" obarray) 
             (fboundp 'mon-get-buffer-visiting-other-live-frame))
(defalias 'mon-get-buffer-visiting-other-live-frame 'mon-find-buffer-visiting-other-live-frame))
;;
(unless  (and (intern-soft "mon-file-attributes-plist" obarray)
              (fboundp 'mon-file-attributes-plist))
(defalias 'mon-file-attributes-plist 'mon-file-dir-attributes->plist))
;;
(unless  (and (intern-soft "mon-directory-attributes-plist" obarray) 
              (fboundp 'mon-directory-attributes-plist))
(defalias 'mon-directory-attributes-plist 'mon-file-dir-attributes->plist))
;;
(unless (and (intern-soft "mon-dired-toggle-dwim-target" obarray)
             (fboundp 'mon-dired-toggle-dwim-target))
(defalias 'mon-dired-toggle-dwim-target 'mon-toggle-dired-dwim-target))
;;
(unless (and (intern-soft "mon-dired-kill-files-to-list" obarray) 
             (fboundp 'mon-dired-kill-files-to-list))
(defalias 'mon-dired-kill-files-to-list 'mon-copy-file-dired-as-list))
;;
(unless (and (intern-soft "mon-dired-copy-files-to-list" obarray) 
             (fboundp 'mon-dired-copy-files-to-list))
(defalias 'mon-dired-copy-files-to-list 'mon-copy-file-dired-as-list))
;;
(unless (and (intern-soft "mon-dired-kill-files-to-strings" obarray) 
             (fboundp 'mon-dired-kill-files-to-strings))
(defalias 'mon-dired-kill-files-to-strings 'mon-copy-file-dired-as-string))
;;
(unless (and (intern-soft "mon-dired-copy-files-to-strings" obarray) 
             (fboundp 'mon-dired-copy-files-to-strings))
(defalias 'mon-dired-copy-files-to-strings 'mon-copy-file-dired-as-string))
;;
(unless (and (intern-soft "mon-directory-get-size" obarray) 
             (fboundp 'mon-directory-get-size))
(defalias 'mon-directory-get-size 'mon-get-dir-size))
;;
(unless (and (intern-soft "mon-buffer-get-new-w-stamp" obarray) 
             (fboundp 'mon-buffer-get-new-w-stamp)) 
(defalias 'mon-buffer-get-new-w-stamp 'mon-get-new-buffer-w-stamp))
;;
(unless (and (intern-soft "mon-dir-name-absolute" obarray) 
             (fboundp 'mon-dir-name-absolute))
(defalias 'mon-dir-name-absolute 'mon-get-dir-name-absolute))
;;
(unless (and (intern-soft "mon-dir-name-relative-w-absolute" obarray) 
             (fboundp 'mon-dir-name-relative-w-absolute))
(defalias 'mon-dir-name-relative-w-absolute 'mon-get-relative-w-absolute))
;;
(unless (and (intern-soft "mon-file-copy-in-sub-dirs" obarray) 
             (fboundp 'mon-file-copy-in-sub-dirs))
(defalias 'mon-file-copy-in-sub-dirs 'mon-copy-files-in-sub-dirs))
;;
(unless (and (intern-soft "mon-file-copy-multiple" obarray) 
             (fboundp 'mon-file-copy-multiple))
(defalias 'mon-file-copy-multiple 'mon-copy-file-multiple))
;;
(unless (and (intern-soft "mon-make-path" obarray) 
             (fboundp 'mon-make-path))
(defalias 'mon-make-path 'mon-build-path))
;;
(unless (and (intern-soft "mon-buffer-string-split-name" obarray) 
             (fboundp 'mon-buffer-string-split-name))
(defalias 'mon-buffer-string-split-name 'mon-string-split-buffer-name))
;;
(unless (and (intern-soft "mon-buffer-string-split-parent-dir" obarray) 
              (fboundp 'mon-buffer-string-split-parent-dir))
(defalias 'mon-buffer-string-split-parent-dir 'mon-string-split-buffer-parent-dir))
;;
(unless (and (intern-soft "mon-buffer-string-split-parent-dir" obarray) 
             (fboundp 'mon-buffer-string-split-parent-dir))
(defalias 'mon-buffer-string-split-parent-dir 'mon-string-split-buffer-parent-dir))
;;
(unless (and (intern-soft "mon-buffer-get-parent-dir" obarray) 
             (fboundp 'mon-buffer-get-parent-dir))
(defalias 'mon-buffer-get-parent-dir 'mon-get-buffer-parent-dir))
;;
(unless (and (intern-soft "mon-dir-name-truncate-for-prompt" obarray) 
             (fboundp 'mon-dir-name-truncate-for-prompt))
(defalias 'mon-dir-name-truncate-for-prompt 'mon-truncate-path-for-prompt))
;;
(unless (and (intern-soft "mon-dir-recurse-string-split" obarray) 
             (fboundp 'mon-dir-recurse-string-split))
(defalias 'mon-dir-recurse-string-split 'mon-string-split-dir-recurse))
;;
(unless (and (intern-soft "mon-buffer-string-split-dir-recurse" obarray) 
             (fboundp 'mon-buffer-string-split-dir-recurse))
(defalias 'mon-buffer-string-split-dir-recurse 'mon-string-split-dir-recurse))
;;
(unless (and (intern-soft "mon-get-dir-common-path" obarray) 
             (fboundp 'mon-get-dir-common-path))
(defalias 'mon-get-dir-common-path 'mon-dir-common-paths))
;;
(unless (and (intern-soft "mon-buffer-subdirs-insert" obarray) 
             (fboundp 'mon-buffer-subdirs-insert))
(defalias 'mon-buffer-subdirs-insert 'mon-insert-subdirs-in-buffer))

;;; ==============================
;;; mon-dir-utils.el◄◄◄
;;; ==============================

;;; ==============================
;;; mon-alphabet-list-utils.el►►►
;;; ==============================

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "mon-make-list-alphabet" obarray) 
             (fboundp 'mon-make-list-alphabet))
(defalias 'mon-make-list-alphabet 'mon-alphabet-as-type))

;;; ==============================
;;; mon-alphabet-list-utils.el◄◄◄
;;; ==============================

;;; ==============================
;;; mon-doc-help-utils.el►►►
;;; ==============================

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "mon-line-strings-region-delimited" obarray)
             (fboundp 'mon-line-strings-region-delimited))
(defalias 'mon-line-strings-region-delimited 'mon-help-delimited-region))
;;
(unless (and (intern-soft "mon-line-strings-get-delimited-region" obarray) 
             (fboundp 'mon-line-strings-get-delimited-region)) 
(defalias 'mon-line-strings-get-delimited-region 'mon-help-delimited-region))
;;
(unless (and (intern-soft "mon-function-arity" obarray) 
             (fboundp 'mon-function-arity)) 
(defalias 'mon-function-arity 'mon-help-function-arity))
;;
(unless (and (intern-soft "mon-function-args" obarray) 
             (fboundp 'mon-function-args)) 
(defalias 'mon-function-args 'mon-help-function-args))
;;
(unless (and (intern-soft "mon-insert-documentation" obarray)
             (fboundp 'mon-insert-documentation)) 
(defalias 'mon-insert-documentation 'mon-help-insert-documentation))
;;
(unless (and (intern-soft "mon-help-reference-sheet" obarray)
             (fboundp 'mon-help-reference-sheet)) 
(defalias 'mon-help-reference-sheet 'mon-help-mon-help))
;;
(unless (and (intern-soft "mon-help-finder-keywords" obarray)
             (fboundp 'mon-help-finder-keywords)) 
(defalias 'mon-help-finder-keywords 'mon-help-package-keywords))
;;
(unless (and (intern-soft "mon-help-directory-file-functions-usage" obarray)
             (fboundp 'mon-help-directory-file-functions-usage)) 
(defalias 'mon-help-directory-file-functions-usage 'mon-help-file-dir-functions-usage))
;;
(unless (and (intern-soft "mon-help-network-process" obarray)
             (fboundp 'mon-help-network-process))
(defalias 'mon-help-network-process 'mon-help-make-network-process))
;;
(unless (and (intern-soft "mon-help-types" obarray)
             (fboundp 'mon-help-types))
(defalias 'mon-help-types 'mon-help-type-predicates))
;;
(unless (and (intern-soft "mon-help-face-functions" obarray)
             (fboundp 'mon-help-face-functions)) 
(defalias 'mon-help-face-functions 'mon-help-faces))
;;
(unless (and (intern-soft "mon-help-charset-coding-functions" obarray)
             (fboundp 'mon-help-charset-coding-functions)) 
(defalias 'mon-help-charset-coding-functions 'mon-help-char-coding-functions))
;;
(unless (and (intern-soft "mon-help-ascii-chars" obarray)
             (fboundp 'mon-help-ascii-chars)) 
(defalias 'mon-help-ascii-chars 'mon-help-char-ascii)) 
;;
(unless (and (intern-soft "mon-help-ecma-48-chars-cntl->hex" obarray)
             (fboundp 'mon-help-ecma-48-chars-cntl->hex)) 
(defalias 'mon-help-ecma-48-chars-cntl->hex 'mon-help-char-ecma-48))
;;
(unless (and (intern-soft "mon-help-cntl->hex->ecma-35" obarray)
             (fboundp 'mon-help-cntl->hex->ecma-35)) 
(defalias 'mon-help-cntl->hex->ecma-35 'mon-help-char-ecma-35)) 
;;
(unless (and (intern-soft "mon-help-time-iso-8601" obarray)
             (fboundp 'mon-help-time-iso-8601)) 
(defalias 'mon-help-time-iso-8601 'mon-help-iso-8601))
;;
(unless (and (intern-soft "mon-help-elisp-info" obarray)
             (fboundp 'mon-help-elisp-info)) 
(defalias 'mon-help-elisp-info 'mon-index-elisp-symbol))
;;
(unless (and (intern-soft "*reference-sheet-help-A-HAWLEY*" obarray)
             (bound-and-true-p *reference-sheet-help-A-HAWLEY*))
(defvaralias '*reference-sheet-help-A-HAWLEY* '*mon-help-reference-keys*))


;;; ==============================
;;; mon-doc-help-utils.el◄◄◄
;;; ==============================


;;; ==============================
;;; mon-time-utils.el◄◄◄
;;; ==============================

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "mon-stamp-date-only" obarray)
             (fboundp 'mon-stamp-date-only)) 
(defalias 'mon-stamp-date-only 'mon-date-stamp))
;;
(unless (and (intern-soft "mon-today-stamp" obarray)
             (fboundp 'mon-today-stamp)) 
(defalias 'mon-today-stamp  'mon-date-stamp))

;;; ==============================
;;; mon-time-utils.el◄◄◄
;;; ==============================

;;; ==============================
;;; mon-dir-locals-alist.el►►►
;;; ==============================

(when (and (and (intern-soft "*emacs2html-temp*" obarray) 
                (bound-and-true-p *emacs2html-temp*))
           (and (intern-soft "*mon-bind-dir-locals-alist*" obarray) 
                (bound-and-true-p *mon-bind-dir-locals-alist*)))
  (unless (and (intern-soft "*mon-emacs2html-temp*" obarray)
               (bound-and-true-p *mon-emacs2html-temp*))
(defvaralias '*mon-emacs2html-temp* '*emacs2html-temp*)))

;;; ==============================
;;; mon-dir-locals-alist.el◄◄◄
;;; ==============================

;;; ==============================
;;; mon-boxcutter.el►►►
;;; ==============================

;;; ==============================
;;; <PREFIX>-<NON-CORE-SYMBOL> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "boxcutter-verify-image-type")
             (fboundp 'boxcutter-verify-image-type))
(defalias 'boxcutter-verify-image-type 'mon-image-verify-type))

;;; ==============================
;;; mon-dir-locals-alist.el◄◄◄
;;; ==============================

;;; ==============================
(provide 'mon-aliases)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-aliases.el ends here
;;; EOF
