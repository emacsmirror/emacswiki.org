;;; mon-aliases.el --- consolidated aliases for functions provided by MON
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-aliases.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-04T20:03:21-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: emacs, lisp, extensions, local

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-aliases provides consolidated aliases for functions provided by MON
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
;; `menubar-mode'         -> `menu-bar-mode'
;; `char-or-stringp'      -> `char-or-string-p'
;; `string-or-characterp' -> `char-or-string-p'
;; `character-or-strinp'  -> `char-or-string-p'
;; `atomp'                -> `atom'
;; `macrop'               -> `apropos-macrop'
;; `make-array'           -> `make-vector'
;; `line-join-previous'   -> `delete-indentation'
;; `shell-command-async'        -> `async-shell-command'
;; `asynchronous-shell-command' ->`shell-command-async'
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
;; An aliasing symbol can be of the form**:
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
;;   ** Presumably above enumeration w/ "<PREFIX>-" can be inverted as
;;      "-<SUFFIX>" and the following discussion remains applicable.
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
;; FIRST-PUBLISHED: <Timestamp: #{2010-11-10T22:28:27-05:00Z}#{10453} - by MON>
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
;; Copyright © 2010-2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; :NOTE Lines below begginig w/ "(defalias" occur without indentation for
;;; etags which likes to find them at BOL.

;; :NOTE Following aliases do not have a corresponding mon-*.el file
 
 
;;; ==============================
;;; mon-aliases.el►►►
;;; ==============================

;;; <UNQUALIFIED-ALIAS> <CORE-SYMBOL>
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
;;
(unless (and (intern-soft "shell-command-async" obarray) 
             (fboundp 'shell-command-async))
(defalias 'shell-command-async 'async-shell-command))
;;
(unless (and (intern-soft "asynchronous-shell-command" obarray) 
             (fboundp 'asynchronous-shell-command))
(defalias 'asynchronous-shell-command 'shell-command-async))
;;
(unless (and (intern-soft "menubar-mode" obarray) 
             (fboundp 'menubar-mode))
(defalias 'menubar-mode 'menu-bar-mode))

;;
;;; ==============================
;;; :NOTE Is it possible to implement a light-weight CL style `make-array' which
;;; specializes on a keyword argument for an `:element-type`, e.g. 'bit, 'sbit,
;;; 'character? Could such a thing maybe leverage `make-char-table' or
;;; `make-keymap' for displaced arrays?. Maybe for signed-byte use a buffer
;;; markder combo as an index into an adjustable array and position-bytes as its
;;; fill-pointer barring unibyte/multibyte/EOL/encoding issues of course :P
(unless (and (intern-soft "make-array" obarray)
             (fboundp 'make-array))
(defalias 'make-array 'make-vector))
;;
;;; <UNQUALIFIED-ALIAS> <PREFIX>-<CORE-SYMBOL>
;;
;;; :NOTE `byte-compile-arglist-warn' has a MACROP parameter.
(unless (and (intern-soft "macrop" obarray) 
             (fboundp (intern-soft "macrop" obarray)))
(defalias 'macrop 'apropos-macrop))


;;; ==============================
;;; mon-aliases.el►►►
;;; ==============================

 
;;; ==============================
;;; mon-utils.el►►►
;;; ==============================

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
;;
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
;;
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-buffer-do-with-undo-disabled" obarray)
             (fboundp (intern-soft "mon-buffer-do-with-undo-disabled" obarray)))
(defalias 'mon-buffer-do-with-undo-disabled 'mon-with-buffer-undo-disabled))
;;
(unless (and (intern-soft "mon-window-get-if-buffer" obarray)
             (fboundp (intern-soft "mon-window-get-if-buffer" obarray)))
(defalias 'mon-window-get-if-buffer 'mon-get-buffer-window-if))
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
(unless (and (intern-soft "mon-generate-wonky" obarray) 
             (fboundp 'mon-generate-wonky))
(defalias 'mon-generate-wonky 'mon-string-wonkify))
;;
(unless (and (intern-soft "mon-hex-list-as-string" obarray) 
             (fboundp 'mon-hex-list-as-string))
(defalias 'mon-hex-list-as-string 'mon-string-from-hex-list))
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
;;
(unless (and (intern-soft "mon-string-ify-current-line" obarray) 
             (fboundp 'mon-string-ify-current-line))
(defalias 'mon-string-ify-current-line 'mon-line-string-split))

;;; ==============================
;;; mon-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-macs.el►►►
;;; ==============================
;;
;;;  <UNQUALIFIED-ALIAS>  <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "with-print-gensyms" obarray) 
             (fboundp 'with-print-gensyms))
(defalias 'with-print-gensyms 'mon-with-print-gensyms))
;;
(unless (and (intern-soft "nshuffle-vector" obarray)
             (fboundp (intern-soft "nshuffle-vector" obarray)))
(defalias 'nshuffle-vector 'mon-nshuffle-vector))
;;
(when (and (intern-soft "IS-MON-SYSTEM-P" obarray)   ;; *IS-MON-OBARRAY*
           (not (and (intern-soft "with-gensyms" obarray)
                     (fboundp (intern-soft "with-gensyms" obarray)))))
(defalias 'with-gensyms 'mon-with-gensyms))
;;
(when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
           (bound-and-true-p IS-MON-SYSTEM-P)
           (if (intern-soft "buffer-exists-p" obarray)
               (not (fboundp (intern-soft "buffer-exists-p" obarray)))
             t))
(defalias 'buffer-exists-p 'mon-buffer-exists-p))
;;
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-buffer-print-in-if" obarray)
             (fboundp  'mon-buffer-print-in-if))
(defalias 'mon-buffer-print-in-if  'mon-print-in-buffer-if-p))

;;; ==============================
;;; mon-macs.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-text-property-utils.el►►►
;;; ==============================
;;
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
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
(unless (and (intern-soft "mon-remove-text-properties-region-all" obarray) 
             (fboundp 'mon-remove-text-properties-region-all))
(defalias 'mon-remove-text-properties-region-all 'mon-nuke-text-properties-region))
;;
(unless (and (intern-soft "mon-remove-all-text-properties-region" obarray) 
             (fboundp 'mon-remove-all-text-properties-region))
(defalias 'mon-remove-all-text-properties-region 'mon-nuke-text-properties-region))

;;; ==============================
;;; mon-text-property-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-error-utils.el►►►
;;; ==============================
;;
;;; <UNQUALIFIED-ALIAS>  <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "write-string" obarray)
             (fboundp 'write-string))
(defalias 'write-string 'mon-write-string))
;;
;;; <PREFIX>-<QUALIFIED> <CORE-SYMBOL>
;;
;;; :NOTE To remember function exists. `prin1-char' is defined in lisp-mode.el
(defalias 'mon-prin1-char->?char 'prin1-char
  "Return a string representing char as a character rather than as an integer.\n
If char is not a character, return nil.\n
:EXAMPLE\n\(prin1-char 32\)\n\(prin1-char 63\)\n\(prin1-char 10\)\n
:SEE-ALSO `mon-write-string', `prin1-char', `princ', `prin1',
`with-output-to-string', `mon-help-print-functions'.\n►►►")

;;; ==============================
;;; mon-error-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-buffer-utils.el►►►
;;; ==============================
;;
;;;  <UNQUALIFIED-ALIAS>  <PREFIX>-<NON-CORE-SYMBOL>
;;
(when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
           (bound-and-true-p IS-MON-SYSTEM-P))
  (unless (and (intern-soft "buffer-narrowed-p" obarray)
               (fboundp 'buffer-narrowed-p))
(defalias 'buffer-narrowed-p 'mon-buffer-narrowed-p)))
;;
(unless (and (intern-soft "get-buffer-window-if" obarray)
             (fboundp (intern-soft "get-buffer-window-if" obarray)))
(defalias 'get-buffer-window-if 'mon-get-buffer-window-if))
;;
(unless (and (intern-soft "goto-line-25%" obarray)
             (fboundp (intern-soft "goto-line-25%" obarray)))
(defalias 'goto-line-25% 'mon-goto-line-25%))
;;
(unless (and (intern-soft "goto-line-50%" obarray)
             (fboundp (intern-soft "goto-line-50%" obarray)))
(defalias 'goto-line-50% 'mon-goto-line-50%))
;;
(unless (and (intern-soft "goto-line-75%" obarray)
             (fboundp (intern-soft "goto-line-75%" obarray)))
(defalias 'goto-line-75% 'mon-goto-line-75%))
;;
;;;  <PREFIX>-<QUALIFIED>  <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-buffer-end" obarray)
             (fboundp 'mon-buffer-end))
(defalias 'mon-buffer-end 'mon-g2be))
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
(unless (and (intern-soft "mon-longlines-mode-p" obarray) 
             (fboundp 'mon-longlines-mode-p))
(defalias 'mon-longlines-mode-p 'mon-buffer-longlines-mode-p))
;;
(unless (and (intern-soft "mon-buffer-append-to" obarray)
             (fboundp 'mon-buffer-append-to))
(defalias 'mon-buffer-append-to 'mon-append-to-buffer))
;;
(unless (and (intern-soft "mon-buffer-make-shell" obarray)
             (fboundp 'mon-buffer-make-shell))
(defalias 'mon-buffer-make-shell 'mon-make-shell-buffer))
;;
(unless (and (intern-soft "mon-buffer-get-shell" obarray)
             (fboundp 'mon-buffer-get-shell))
(defalias 'mon-buffer-get-shell 'mon-shell))
;;
(unless (and (intern-soft "mon-kill-hidden-buffer-if" obarray)
             (fboundp 'mon-kill-hidden-buffer-if))
(defalias 'mon-kill-hidden-buffer-if 'mon-buffer-kill-hidden-if))
;;
(unless (and (intern-soft "mon-buffer-get-hidden-if" obarray)
             (fboundp 'mon-buffer-get-hidden-if))
(defalias 'mon-buffer-get-hidden-if 'mon-get-buffer-hidden-if))

;;; ==============================
;;; mon-buffer-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-randomize-utils.el►►►
;;; ==============================
;;
;;;  <UNQUALIFIED-ALIAS>  <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "next-almost-prime" obarray)
             (fboundp 'next-almost-prime))
(defalias 'next-almost-prime 'mon-next-almost-prime))
;;
;;;  <PREFIX>-<QUALIFIED>  <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-get-next-almost-prime" obarray)
             (fboundp 'mon-get-next-almost-prime))
(defalias 'mon-get-next-almost-prime 'mon-next-almost-prime))

;;; ==============================
;;; mon-randomize-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-rectangle-utils.el►►►
;;; ==============================
;;
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-rectangle-kill-w-longest-line" obarray)
             (fboundp 'mon-rectangle-kill-w-longest-line))
(defalias 'mon-rectangle-kill-w-longest-line 'mon-kill-rectangle-w-beer-belly))

;;; ==============================
;;; mon-rectangle-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-line-utils.el►►►
;;; ==============================
;;
;;; <PREFIX>-<QUALIFIED> <CORE-SYMBOL>
;;
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
(unless (and (featurep 'slime)
             (intern-soft "mon-line-same-p" obarray)
             (fboundp 'mon-line-same-p))
(defalias 'mon-line-same-p 'slime-same-line-p))

;;; ==============================
;;; mon-line-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-window-utils.el►►►
;;; ==============================
;;
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-get-window-plist" obarray)
             (fboundp 'mon-get-window-plist))
(defalias 'mon-get-window-plist 'mon-map-windows->plist))
;;
(unless (and (intern-soft "mon-window-map-active-to-plist" obarray)
             (fboundp 'mon-window-map-active-to-plist))
(defalias 'mon-window-map-active-to-plist 'mon-map-windows->plist))
;;
(unless (and (intern-soft "mon-buffer-get-scratch" obarray)
             (fboundp 'mon-buffer-get-scratch))
(defalias 'mon-buffer-get-scratch 'mon-scratch))
;;
(unless (and (intern-soft "mon-buffer-get-messages" obarray)
             (fboundp 'mon-buffer-get-messages))
(defalias 'mon-buffer-get-messages 'mon-switch-to-messages))
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

;;; ==============================
;;; mon-window-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-seq-utils.el►►►
;;; ==============================
;;
;;;  <UNQUALIFIED-ALIAS>  <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "delq-dups" obarray)
             (fboundp 'delq-dups))
(defalias 'delq-dups 'mon-delq-dups))
;;
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-string-longest-in-list" obarray) 
             (fboundp 'mon-string-longest-in-list))
(defalias 'mon-string-longest-in-list 'mon-list-string-longest))
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
(unless (and (intern-soft "mon-list-permute-combine" obarray)
             (fboundp 'mon-list-permute-combine))
(defalias 'mon-list-permute-combine 'mon-permute-combine))
;;
(unless (and (intern-soft "mon-list-permute-combine-1" obarray)
             (fboundp 'mon-list-permute-combine-1))
(defalias 'mon-list-permute-combine-1 'mon-permute-combine-1))
;;
(unless (and (intern-soft "mon-list-recurse-apply" obarray) 
             (fboundp 'mon-list-recurse-apply)) 
(defalias 'mon-list-recurse-apply 'mon-recursive-apply))
;;
(unless (and (intern-soft "mon-merge-list" obarray)
             (fboundp 'mon-merge-list))
(defalias 'mon-merge-list 'mon-list-merge))

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
(unless (and (intern-soft "mon-list-transpose" obarray)
             (fboundp 'mon-list-transpose))
(defalias 'mon-list-transpose 'mon-transpose))
;;
(unless (and (intern-soft "mon-bool-vector-to-list" obarray)
             (fboundp 'mon-bool-vector-to-list))
(defalias 'mon-bool-vector-to-list    'mon-bool-vector-pp))
;;
(unless (and (intern-soft "mon-list-ify-bool-vector" obarray)
             (fboundp 'mon-list-ify-bool-vector))
(defalias 'mon-list-ify-bool-vector   'mon-bool-vector-pp))
;;
(unless (and (intern-soft "mon-bool-vector-to-list" obarray)
             (fboundp 'mon-bool-vector-to-list))
(defalias 'mon-boolean-vector-to-list 'mon-bool-vector-pp))
;;
(unless (and (intern-soft "mon-pop2" obarray) 
             (fboundp 'mon-pop2))
(defalias 'mon-pop2 'cl-pop2
  "Pop car and car and cadr from PLACE return cadr.\n
:EXAMPLE\n\n\(let \(\(bubba '\(a 1 3 e\)\)\)
 \(cl-pop2 bubba\)\)\n
\(let \(\(bubba '\(a 1 3 e\)\)\)
 \(cl-pop2 bubba\)
  bubba\)\n
:SEE-ALSO `pop', `push', `setf'.\n►►►"))
;;
(unless (and (intern-soft "mon-list-pop2" obarray)
             (fboundp 'mon-list-pop2))
(defalias 'mon-list-pop2 'cl-pop2))
;;
(eval-when-compile
  (put 'cl-pop2 'function-documentation (get 'mon-pop2 'function-documentation))
  (put 'mon-list-pop2 'function-documentation (get 'mon-pop2 'function-documentation)))

;;; ==============================
;;; mon-seq-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-event-utils.el►►►
;;; ==============================
;;
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-region-append-to-register" obarray) 
             (fboundp 'mon-region-append-to-register))
(defalias 'mon-region-append-to-register 'mon-append-to-register))
;;
(unless (and (intern-soft "mon-append-next-kill" obarray)
             (fboundp 'mon-append-next-kill))
(defalias 'mon-append-next-kill 'mon-kill-appending))
;;
(unless (and (intern-soft "mon-register-append" obarray)
             (fboundp 'mon-register-append))
(defalias 'mon-register-append 'mon-append-to-register))
;; 
(unless (and (intern-soft "mon-read-keys-last-event" obarray)
             (fboundp 'mon-read-keys-last-event))
(defalias 'mon-read-keys-last-event 'mon-test-keypresses))
;; 
(unless (and (intern-soft "mon-string-from-keyboard-input" obarray)
             (fboundp 'mon-string-from-keyboard-input))
(defalias 'mon-string-from-keyboard-input 'mon-read-keys-as-string))

;;; ==============================
;;; mon-event-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-type-utils.el►►►
;;; ==============================
;;
;;;  <UNQUALIFIED-ALIAS>  <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "alpha-char-p" obarray) 
             (fboundp 'alpha-char-p))
(defalias 'alpha-char-p 'mon-alpha-char-p))
;;
(unless (and (intern-soft "zerop-or-one" obarray)
             (fboundp 'zerop-or-one))
(defalias 'zerop-or-one 'mon-zero-or-onep))
;;
(unless (and (intern-soft "proper-list-p" obarray)
             (fboundp 'proper-list-p))
(defalias 'proper-list-p 'mon-list-proper-p))
;;
(unless (and (intern-soft "string-or-null-and-zerop" obarray) 
             (fboundp 'string-or-null-and-zerop))
;;
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
;;; <PREFIX>-<QUALIFIED>                <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-is-alpha-char" obarray) 
             (fboundp 'mon-is-alpha-char))
(defalias 'mon-is-alpha-char 'mon-alpha-char-p))
;;
(unless (and (intern-soft "mon-proper-list-p" obarray)
             (fboundp 'mon-proper-list-p))
(defalias 'mon-proper-list-p 'mon-list-proper-p))
;;
(unless (and (intern-soft "mon-dotted-list-p" obarray)
             (fboundp 'mon-dotted-list-p))
(defalias 'mon-dotted-list-p 'mon-list-dotted-p))
;;
(unless (and (intern-soft "mon-list-dotted-and-proper-p" obarray)
             (fboundp 'mon-list-dotted-and-proper-p))
(defalias 'mon-list-dotted-and-proper-p 'mon-list-proper-and-dotted-p))
;;
(unless (and (intern-soft "mon-list-mappable-p" obarray) 
             (fboundp 'mon-list-mappable-p))
(defalias 'mon-list-mappable-p 'mon-sequence-mappable-p))
;;
(unless (and (intern-soft "mon-mappable-sequence-p" obarray) 
             (fboundp 'mon-mappable-sequence-p))
(defalias 'mon-mappable-sequence-p 'mon-sequence-mappable-p))
;;
(unless (and (intern-soft "mon-list-all-booleanp" obarray)
             (fboundp 'mon-list-all-booleanp))
(defalias 'mon-list-all-booleanp 'mon-sequence-all-booleanp))
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
(unless (and (intern-soft "mon-char-coerce")
             (fboundp 'mon-char-coerce))
(defalias 'mon-char-coerce 'mon-coerce->char))
;;
(unless (and (intern-soft "mon-symbol-cells-boundp")
             (fboundp 'mon-symbol-cells-boundp))
(defalias 'mon-symbol-cells-boundp 'mon-symbol-cells-bound-p))

;;; ==============================
;;; mon-type-utils.el◄◄◄
;;; ==============================


 
;;; ==============================
;;; mon-region-utils.el►►►
;;; ==============================
;;
;;; <PREFIX>-<QUALIFIED>       <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-capitalize-region" obarray)
              (fboundp (intern-soft "mon-capitalize-region" obarray)))
(defalias 'mon-capitalize-region 'mon-region-capitalize))
;;
(unless (and (intern-soft "mon-region-reverse-chars" obarray)
              (fboundp (intern-soft "mon-region-reverse-chars" obarray)))
(defalias 'mon-region-reverse-chars 'mon-region-reverse))
;;
(unless (and (intern-soft "mon-region-wrap" obarray)
             (fboundp 'mon-region-wrap))
(defalias 'mon-region-wrap 'mon-wrap-selection))
;;
(unless (and (intern-soft "mon-string-split-commas" obarray) 
             (fboundp 'mon-string-split-commas))
(defalias 'mon-string-split-commas 'mon-region-split-commas))
;;
(unless (and (intern-soft "mon-split-region-at-commas" obarray) 
             (fboundp 'mon-split-region-at-commas))
(defalias 'mon-split-region-at-commas 'mon-region-split-commas))
;;
(unless (and (intern-soft "mon-indent-refill-region" obarray) 
             (fboundp 'mon-indent-refill-region))
(defalias 'mon-indent-refill-region 'mon-region-indent-refill))
;;
(unless (and (intern-soft "mon-indent-region-refill" obarray) 
             (fboundp 'mon-indent-region-refill))
(defalias 'mon-indent-region-refill 'mon-region-indent-refill))
;;
(unless (and (intern-soft "mon-region-refill-indent" obarray) 
             (fboundp 'mon-region-refill-indent))
(defalias 'mon-region-refill-indent 'mon-region-indent-refill))
;;
(unless (and (intern-soft "mon-region-count-regexp-matches" obarray)
             (fboundp 'mon-region-count-regexp-matches))
(defalias 'mon-region-count-regexp-matches 'comint-how-many-region))

;;; ==============================
;;; mon-region-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-string-utils.el►►►
;;; ==============================

(unless (and (intern-soft "mon-string-at-point" obarray) 
             (fboundp 'mon-string-at-point))
(defalias 'mon-string-at-point 'comint-extract-string))

;;; ==============================
;;; mon-string-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-env-proc-utils.el►►►
;;; ==============================

;;; ==============================
;;; mon-env-proc-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-name-utils.el►►►
;;; ==============================
;;
(unless (and (intern-soft "mon-permute-string" obarray) 
             (fboundp 'mon-permute-string))
(defalias 'mon-permute-string 'mon-string-permute))

;;; ==============================
;;; mon-name-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-word-syntax-utils.el►►►
;;; ==============================

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

;;; ==============================
;;; mon-word-syntax-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-alphabet-list-utils.el►►►
;;; ==============================

;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-make-list-alphabet" obarray) 
             (fboundp 'mon-make-list-alphabet))
(defalias 'mon-make-list-alphabet 'mon-alphabet-as-type))

;;; ==============================
;;; mon-alphabet-list-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-dir-utils.el►►►
;;; ==============================

;;; <UNQUALIFIED-ALIAS> <PREFIX>-<NON-CORE-SYMBOL>
;;
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
;;
;;; <PREFIX>-<QUALIFIED> <CORE-SYMBOL>
;;
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
;;
;;; <PREFIX>-<NON-CORE-SYMBOL> <PREFIX>-<NON-CORE-SYMBOL>
;;
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
;;
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
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
;;
(unless (and (intern-soft "mon-dir-get-subdir" obarray) 
             (fboundp 'mon-dir-get-subdir))
(defalias 'mon-dir-get-subdir 'mon-get-dir-subdir-default))
;;
(unless (and (intern-soft "mon-file-rename-serial" obarray) 
             (fboundp 'mon-file-rename-serial))
(defalias 'mon-file-rename-serial  'mon-rename-file-serial))
;;
(unless (and (intern-soft "mon-file-copy-path" obarray) 
             (fboundp 'mon-file-copy-path))
(defalias 'mon-file-copy-path        'mon-copy-file-path))
;;
(unless (and (intern-soft "mon-buffer-file-copy-path" obarray) 
             (fboundp 'mon-buffer-file-copy-path))
(defalias 'mon-buffer-file-copy-path 'mon-copy-file-path))
;;
(unless (and (intern-soft "mon-buffer-get-directories" obarray) 
             (fboundp 'mon-buffer-get-directories))
(defalias 'mon-buffer-get-directories 'mon-get-buffers-directories))
;;
(unless (and (intern-soft "mon-dir-get-subdirs-descend" obarray) 
             (fboundp 'mon-dir-get-subdirs-descend))
(defalias 'mon-dir-get-subdirs-descend 'mon-add-subdirs-to-list))
;;
(unless (and (intern-soft "mon-get-directory-and-subdirs-list" obarray) 
             (fboundp 'mon-dir-add-subdirs-to-list))
(defalias 'mon-get-directory-and-subdirs-list 'mon-add-subdirs-to-list))
;;
(unless (and (intern-soft "mon-dir-async-du" obarray) 
             (fboundp 'mon-dir-async-du))
(defalias 'mon-dir-async-du 'mon-async-du-dir))
;;
(unless (and (intern-soft "mon-directory-du-async" obarray) 
             (fboundp 'mon-directory-du-async))
(defalias 'mon-directory-du-async 'mon-async-du-dir))
;;
(unless (and (intern-soft "mon-du-async-dir" obarray) 
             (fboundp 'mon-du-async-dir))
(defalias 'mon-du-async-dir 'mon-async-du-dir))


;;; ==============================
;;; mon-dir-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-url-utils.el►►►
;;; ==============================

;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (fboundp 'w3m-copy-this-url-as-kill)
(defalias 'w3m-copy-this-url-as-kill 'w3m-print-this-url))
;;
(unless (fboundp 'w3m-copy-current-url-as-kill)
(defalias 'w3m-copy-current-url-as-kill 'w3m-print-current-url))
;;
(unless (and (intern-soft "mon-url-escape" obarray)
             (fboundp 'mon-url-escape))
(defalias 'mon-url-escape 'mon-url-encode))
;;
(unless (and (intern-soft "mon-encode-url" obarray)
             (fboundp 'mon-encode-url))
(defalias 'mon-encode-url 'mon-url-encode))
;;
(unless (and (intern-soft "mon-url-unescape")
             (fboundp 'mon-url-unescape))
(defalias 'mon-url-unescape 'mon-url-decode))
;;
(unless (and (intern-soft "mon-decode-url")
             (fboundp 'mon-decode-url))
(defalias 'mon-decode-url 'mon-url-decode))
;;
(unless (and (intern-soft "mon-search-wiki")
             (fboundp 'mon-search-wiki))
(defalias 'mon-search-wiki 'mon-search-wikipedia))
;;
(unless (and (intern-soft "mon-buffer-get-retrieved-url")
             (fboundp 'mon-buffer-get-retrieved-url))
(defalias 'mon-buffer-get-retrieved-url 'mon-url-retrieve-to-new-buffer))
;;
(unless (and (intern-soft "mon-get-w3m-dired-file" obarray)
             (fboundp 'mon-get-w3m-dired-file))
(defalias 'mon-get-w3m-dired-file  'mon-w3m-dired-file))
;;
(unless (and (intern-soft "mon-w3m-get-url-at-point-maybe" obarray)
             (fboundp 'mon-w3m-get-url-at-point-maybe))
(defalias 'mon-w3m-get-url-at-point-maybe 'mon-get-w3m-url-at-point-maybe))
;;
(unless (and (intern-soft "mon-get-w3m-url-at-point" obarray)
             (fboundp 'mon-get-w3m-url-at-point))
(defalias 'mon-w3m-get-url-at-point 'mon-get-w3m-url-at-point))
;;
(unless (and (intern-soft "mon-get-w3m-read-gnu-lists-nxt-prv" obarray)
             (fboundp 'mon-get-w3m-read-gnu-lists-nxt-prv))
(defalias 'mon-get-w3m-read-gnu-lists-nxt-prv 'mon-w3m-read-gnu-lists-nxt-prv))


;;; ==============================
;;; mon-url-utils.el►►►
;;; ==============================

 
;;; ==============================
;;; mon-doc-help-utils.el►►►
;;; ==============================

;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
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
;;; <VARIABLE>
(unless (and (intern-soft "*reference-sheet-help-A-HAWLEY*" obarray)
             (bound-and-true-p *reference-sheet-help-A-HAWLEY*))
(defvaralias '*reference-sheet-help-A-HAWLEY* '*mon-help-reference-keys*))
;;
(unless (and (intern-soft "*doc-cookie*" obarray)
             (bound-and-true-p *doc-cookie*))
(defvaralias '*doc-cookie* '*mon-doc-cookie*))

;;; ==============================
;;; mon-doc-help-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-doc-help-CL.el►►►
;;; ==============================

(unless (and (intern-soft "mon-help-cl-packages")
             (fboundp 'mon-help-cl-packages))
(defalias 'mon-help-cl-packages 'mon-help-CL-pkgs))
;;
(unless (and (intern-soft "mon-help-slime-keys") 
             (fboundp 'mon-help-slime-keys))
(defalias 'mon-help-slime-keys 'mon-help-CL-slime-keys))
;;
(unless (and (intern-soft "mon-help-swank-functions")
             (fboundp 'mon-help-swank-functions))
(defalias 'mon-help-swank-functions 'mon-help-CL-swank-functions))
;;
(unless (and (intern-soft "mon-help-cl-symbols")
             (fboundp 'mon-help-cl-symbols))
(defalias 'mon-help-cl-symbols 'mon-help-CL-symbols))
;;
(unless (and (intern-soft "mon-hyperspec-lookup") 
             (fboundp 'mon-hyperspec-lookup))
(defalias 'mon-hyperspec-lookup 'mon-help-CL-symbols))

;;; ==============================
;;; mon-doc-help-CL.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-doc-help-css.el►►►
;;; ==============================

;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-help-csstidy" obarray)
             (fboundp 'mon-help-csstidy)) 
(defalias 'mon-help-csstidy 'mon-help-css-check))

;;; ==============================
;;; mon-doc-help-css.el
;;; ==============================

 
;;; ==============================
;;; mon-bzr-utils.el►►►
;;; ==============================

;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;; 
(unless (and (intern-soft "mon-help-bzr-commands" obarray)
             (fboundp 'mon-help-bzr-commands)) 
(defalias 'mon-help-bzr-commands 'mon-help-bzr-topics))

;;; ==============================
;;; mon-bzr-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-time-utils.el◄◄◄
;;; ==============================

;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
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

;;; <VARIABLE>

(when (and (and (intern-soft "*emacs2html-temp*" obarray) 
                (bound-and-true-p *emacs2html-temp*))
           (and (intern-soft "*mon-bind-dir-locals-alist*" obarray)  ;; *IS-MON-OBARRAY* ???
                (bound-and-true-p *mon-bind-dir-locals-alist*)))
  (unless (and (intern-soft "*mon-emacs2html-temp*" obarray)
               (bound-and-true-p *mon-emacs2html-temp*))
(defvaralias '*mon-emacs2html-temp* '*emacs2html-temp*)))

;;; ==============================
;;; mon-dir-locals-alist.el◄◄◄
;;; ==============================


;;; ==============================
;;; mon-dir-locals-alist.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-insertion-utils.el►►►
;;; ==============================

;;; <UNQUALIFIED-ALIAS> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "comment-divider" obarray)
             (fboundp 'comment-divider))
(defalias 'comment-divider 'mon-comment-divider))

;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "mon-string-insert-n-fancy-times" obarray)
             (fboundp 'mon-string-n-fancy-times))
(defalias 'mon-string-n-fancy-times 'mon-insert-string-n-fancy-times))
;;
(unless (and (intern-soft "mon-string-insert-n-times" obarray)
             (fboundp 'mon-string-insert-n-times))
(defalias 'mon-string-insert-n-times 'mon-insert-string-n-times))
;;
(unless (and (intern-soft "mon-comment-divider->col" obarray) 
             (fboundp 'mon-comment-divider->col))
(defalias 'mon-comment-divider->col 'mon-comment-divider-to-col))
;;
(unless (and (intern-soft "mon-lisp-comment-to-col" obarray) 
             (fboundp 'mon-lisp-comment-to-col))
(defalias 'mon-lisp-comment-to-col 'mon-comment-lisp-to-col))
;;
(unless (and (intern-soft "mon-insert-doc-xref-eg" obarray)
             (fboundp 'mon-insert-doc-xref-eg))
(defalias 'mon-insert-doc-xref-eg 'mon-insert-lisp-doc-eg-xref))
;;
(unless (and (intern-soft "mon-CL-package-complete" obarray) 
             (fboundp 'mon-CL-package-complete))
(defalias 'mon-CL-package-complete 'mon-lisp-CL-package-complete))
;;
(unless (and (intern-soft "mon-insert-CL-mode-line-template" obarray) 
             (fboundp 'mon-insert-CL-mode-line-template))
(defalias 'mon-insert-CL-mode-line-template 'mon-insert-lisp-CL-mode-line-template))
;;
(unless (and (intern-soft "mon-add-lisp-CL-file-local-prop-template" obarray) 
             (fboundp 'mon-add-lisp-CL-file-local-prop-template))
(defalias 'mon-add-lisp-CL-file-local-prop-template  'mon-insert-lisp-CL-mode-line-template))
;;
(unless (and (intern-soft "mon-insert-CL-file-template" obarray) 
             (fboundp 'mon-insert-CL-file-template))
(defalias 'mon-insert-CL-file-template 'mon-insert-lisp-CL-file-template))
;;
(unless (and (intern-soft "mon-insert-CL-package-template" obarray) 
             (fboundp 'mon-insert-CL-package-template))
(defalias 'mon-insert-CL-package-template 'mon-insert-lisp-CL-package-template))

;;
(unless (and (intern-soft "mon-insert-CL-eval-when" obarray) 
             (fboundp 'mon-insert-CL-eval-when))
(defalias 'mon-insert-CL-eval-when 'mon-insert-lisp-CL-eval-when))
;;
(unless (and (intern-soft "mon-insert-CL-debug" obarray) 
             (fboundp 'mon-insert-CL-debug))
(defalias 'mon-insert-CL-debug 'mon-insert-lisp-CL-debug))
;;
(unless (and (intern-soft "mon-insert-jump-lisp-doc" obarray) 
             (fboundp 'mon-insert-jump-lisp-doc))
(defalias 'mon-insert-jump-lisp-doc 'mon-insert-lisp-CL-jump-doc))

;;
;;; :NOTE It isn't clear if this is prfx-non-core e.g. `bug-reference-*'...
(unless (and (intern-soft "bug-insert-copyright" obarray) 
             (fboundp 'bug-insert-copyright)) 
(defalias 'bug-insert-copyright 'mon-insert-copyright
  "Insert a copyright string with relevant details.\n
Conditional upon `IS-BUG-P' returning non-nil.\n
:ALIAS-OF `mon-insert-copyright'\n
:SEE-ALSO `mon-build-copyright-string'.\nUsed in `naf-mode'.\n►►►"))
;;
(unless (and (intern-soft "mon-insert-naf-mode-file-template" obarray) 
             (fboundp 'mon-insert-naf-mode-file-template))
(defalias 'mon-insert-naf-mode-file-template 'mon-insert-file-template))

;;; ==============================
;;; mon-insertion-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-regexp-symbols.el►►►
;;; ==============================

;;; <VARIABLE>
(unless (and (intern-soft "*regexp-version-alist*" obarray) 
             (bound-and-true-p *regexp-version-alist*))
(defvaralias '*regexp-version-alist* 'version-regexp-alist))
;;
;;; <VARIABLE>
(unless (and (intern-soft "*whitespace-chars*" obarray) 
             (bound-and-true-p *whitespace-chars*))
(defvaralias '*whitespace-chars* '*mon-whitespace-chars*))
;;
(unless (and (intern-soft "*mon-digit-chars*" obarray) 
             (bound-and-true-p *mon-digit-chars*))
(defvaralias '*mon-digit-chars* 'parse-time-digits
  "A 256 elt simple array with indexes for the digit chars 0-9.\n
:EXAMPLE\n\n\(aref *mon-digit-chars* ?0\)\n
\(mapcar #'\(lambda \(dgt-chr\)
             \(aref *mon-digit-chars* dgt-chr\)\)
        \(number-sequence 48 57\)\)\n
:SEE-ALSO `*mon-whitespace-chars*', `*mon-ascii-alpha-chars*', `mon-is-digit'.\n►►►"))

;;; ==============================
;;; mon-regexp-symbols.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-replacement-utils.el►►►
;;; ==============================
;;
;;; ==============================
;;; :NOTE MON always forget to use these functions, lets get reminded!
;;; :CREATED <Timestamp: Wednesday May 13, 2009 @ 01:33.46 PM - by MON KEY>

;;; ==============================
;;; <NON-CORE-SYMBOL>-<SUFFIX> <CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "replace-char-in-string-mon" obarray) 
             (fboundp 'replace-char-in-string-mon))
(defalias 'replace-char-in-string-mon 'subst-char-in-string))
;;
(unless (and (intern-soft "replace-in-string-mon" obarray) 
             (fboundp 'replace-in-string-mon))
(defalias 'replace-in-string-mon      'subst-char-in-string))

;;; ==============================
;;; <PREFIX>-<NON-CORE-SYMBOL> <CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "mon-replace-char-in-string" obarray) 
             (fboundp 'mon-replace-char-in-string))
(defalias 'mon-replace-char-in-string 'subst-char-in-string))

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "mon-canonical-string" obarray)
             (fboundp 'mon-string-canonical))
(defalias 'mon-canonical-string 'mon-string-canonical))
;;
(unless (and (intern-soft "mon-map-regexp-matches" obarray)
             (fboundp 'mon-map-regexp-matches))
(defalias 'mon-map-regexp-matches 'mon-regexp-map-match))
;;
(unless (and (intern-soft "mon-map-regexp-matches-in-region")
             (fboundp 'mon-map-regexp-matches-in-region))
(defalias 'mon-map-regexp-matches-in-region 'mon-regexp-map-match-in-region))
;;
(unless (and (intern-soft "mon-region-increment-line-numbers" obarray) 
             (fboundp 'mon-region-increment-line-numbers))
(defalias 'mon-region-increment-line-numbers 'mon-line-number-region-incr))
;;
(unless (and (intern-soft "mon-region-increment-numbered-lines" obarray) 
             (fboundp 'mon-region-increment-numbered-lines))
(defalias 'mon-region-increment-numbered-lines 'mon-line-number-region-incr))

;;
(unless (and (intern-soft "mon-cln-duplicate-lines" obarray) 
             (fboundp 'mon-cln-duplicate-lines))
(defalias 'mon-cln-duplicate-lines 'mon-line-find-duplicates-cln))
;;
(unless (and (intern-soft "mon-remove-duplicate-lines" obarray) 
             (fboundp 'mon-remove-duplicate-lines))
(defalias 'mon-remove-duplicate-lines 'mon-line-find-duplicates-cln))

;;
(unless (and (intern-soft "naf-delete-back-up-list" obarray) 
             (fboundp 'naf-delete-back-up-list))
(defalias 'naf-delete-back-up-list 'mon-delete-back-up-list))
;;
(unless (and (intern-soft "mon-defranc-benezit" obarray) 
             (fboundp 'mon-defranc-benezit))
(defalias 'mon-defranc-benezit 'mon-cln-benezit))
;;
(unless (and (intern-soft "mon-defranc-benezit-fields" obarray) 
             (fboundp 'mon-defranc-benezit-fields))
(defalias 'mon-defranc-benezit-fields 'mon-cln-benezit-fields))
;;
;;
(unless (and (intern-soft "mon-cln-common-abbrevs" obarray) 
             (fboundp 'mon-cln-common-abbrevs))
(defalias 'mon-cln-common-abbrevs 'mon-replace-common-abbrevs))

;;; ==============================
;;; mon-replacement-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-color-utils.el►►►
;;; ==============================

;;; ==============================
;;; <UNQUALIFIED-ALIAS> <CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "list-colors-defined" obarray)
              (fboundp 'list-colors-defined))
(defalias 'list-colors-defined 'defined-colors))

;;; ==============================
;; <PREFIX>-<NON-CORE-SYMBOL> <CORE-SYMBOL>
;;; ==============================
;;
(unless (and (intern-soft "mon-color-list-defined" obarray)
              (fboundp 'mon-color-list-defined))
(defalias 'mon-color-list-defined 'defined-colors))
;;
;; :NOTE there is also `facemenu-read-color'
(unless (and (intern-soft "mon-color-read" obarray)
              (fboundp 'mon-color-read))
(defalias 'mon-color-read 'read-color))
;;
(unless (and (intern-soft "mon-color-list-duplicates" obarray)
              (fboundp 'mon-color-list-duplicates))
(defalias 'mon-color-list-duplicates 'list-colors-duplicates))
;;
(unless (and (intern-soft "mon-color-list-display" obarray)
              (fboundp 'mon-color-list-display))
(defalias 'mon-color-list-display 'list-colors-display))

;;; ==============================
;;; mon-color-utils.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-rename-image-utils.el►►►
;;; ==============================

;;; ==============================
;;; <UNQUALIFIED-ALIAS> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "get-bmps-in-dir" obarray) 
             (fboundp 'get-bmps-in-dir))
(defalias 'get-bmps-in-dir 'mon-get-ebay-bmps-in-dir))
;;
(unless (and (intern-soft "get-nefs-in-dir" obarray) 
             (fboundp 'get-nefs-in-dir))
(defalias 'get-nefs-in-dir 'mon-get-nefs-in-dir))

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "mon-get-ebay-bmps-list" obarray) 
             (fboundp 'mon-get-ebay-bmps-list))
(defalias 'mon-get-ebay-bmps-list 'mon-get-ebay-bmps-in-dir))

;;; ==============================
;;; mon-rename-image-utils.el◄◄◄
;;; ==============================


;;; ==============================
;;; mon-image-utils.el►►►
;;; ==============================

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "mon-rotate-images" obarray) 
             (fboundp 'mon-rotate-images))
(defalias 'mon-rotate-images 'mon-image-rotate))
;;
(unless (and (intern-soft "mon-identify-image" obarray) 
             (fboundp 'mon-identify-image))
(defalias 'mon-identify-image 'mon-image-identify))

;;; ==============================
;;; mon-image-utils.el◄◄◄
;;; ==============================


 
;;; ==============================
;;; mon-boxcutter.el►►►
;;; ==============================

;;; ==============================
;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;; ==============================

(unless (and (intern-soft "boxcutter-verify-image-type")
             (fboundp 'boxcutter-verify-image-type))
(defalias 'boxcutter-verify-image-type 'mon-image-verify-type))

;;; ==============================
;;; mon-boxcutter.el◄◄◄
;;; ==============================

 
;;; ==============================
;;; mon-dir-utils-locals.el►►►
;;; ==============================
;;
(unless (and (intern-soft "naf-drive-dired-artist-letter" obarray)
             (fboundp 'naf-drive-dired-artist-letter))
(defalias 'naf-drive-dired-artist-letter 'mon-dired-naf-artist-letter))
;;
(unless (and (intern-soft "naf-dired-artist-letter" obarray)
             (fboundp 'naf-dired-artist-letter))
(defalias 'naf-dired-artist-letter 'mon-dired-naf-artist-letter))
;;
(unless (and (intern-soft "naf-drive-dired-brand-letter-letter" obarray)
             (fboundp 'naf-drive-dired-brand-letter-letter))
(defalias 'naf-drive-dired-brand-letter 'mon-dired-naf-brand-letter))
;;
(unless (and (intern-soft "naf-dired-brand-letter" obarray)
             (fboundp 'naf-dired-brand-letter))
(defalias 'naf-dired-brand-letter 'mon-dired-naf-brand-letter))

;;; ==============================
;;; mon-dir-utils-locals.el◄◄◄
;;; ==============================

 
;; :NOTE Can be removed once `slot-makeunbound' is removed/renamed/aliased in
;; :FILE lisp/emacs-lisp/eieio.el
(eval-after-load "eieio"
  '(when (and (intern-soft "slot-makeunbound" obarray)
             (fboundp 'slot-makeunbound))
     (unless (and (intern-soft "slot-makunbound" obarray)
                  (fboundp 'slot-makunbound))
(defalias 'slot-makunbound 'slot-makeunbound))))

 
;;; ==============================
;;; slime-loads-GNU-clbuild.el►►►
;;; ==============================

;;; <PREFIX>-<QUALIFIED> <PREFIX>-<NON-CORE-SYMBOL>
;;
(unless (and (intern-soft "slime-next-xref" obarray)
             (fboundp 'slime-next-xref))
(defalias 'slime-next-xref 'mon-next-xref-slime))
;;
(unless (and (intern-soft "slime-prev-xref" obarray)
             (fboundp 'slime-prev-xref))
(defalias 'slime-prev-xref 'mon-prev-xref-slime))
;;
(unless (and (intern-soft "mon-slime-insert-arglist" obarray)
             (fboundp 'mon-slime-insert-arglist))
(defalias 'mon-slime-insert-arglist 'mon-insert-slime-arglist))
;;
(unless (and (intern-soft "mon-slime-quit-description" obarray)
             (fboundp 'mon-slime-quit-description))
(defalias 'mon-slime-quit-description 'mon-quit-slime-description-window))


;;; ==============================
;;; slime-loads-GNU-clbuild.el◄◄◄
;;; ==============================

;;; ==============================
(provide 'mon-aliases)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:

;;; ====================================================================
;;; mon-aliases.el ends here
;;; EOF
