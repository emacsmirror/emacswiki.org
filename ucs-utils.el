;;; ucs-utils.el --- Utilities for Unicode characters
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker walker@pobox.com
;; URL: https://github.com/rolandwalker/ucs-utils.el
;; Version: 0.6.1
;; Last-Updated: 23 Aug 2012
;; EmacsWiki: UcsUtils
;; Package-Requires: ((persistent-soft "0.8.0"))
;; Keywords: I18n
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This library provides some utilities for manipulating Unicode
;; characters.  There are three interactive commands
;;
;;    `ucs-utils-ucs-insert'       ; `ucs-insert' workalike using ido
;;    `ucs-utils-eval'             ; the inverse of `ucs-insert'
;;    `ucs-utils-install-aliases'  ; install shorter aliases
;;
;; The other functions are only useful from other Lisp code:
;;
;;    `ucs-utils-char'
;;    `ucs-utils-first-existing-char'
;;    `ucs-utils-vector'
;;    `ucs-utils-string'
;;    `ucs-utils-intact-string'
;;    `ucs-utils-pretty-name'
;;    `ucs-utils-read-char-by-name'
;;    `ucs-utils-subst-char-in-region'
;;
;; To use ucs-utils, place the ucs-utils.el library somewhere Emacs
;; can find it, and add the following to your ~/.emacs file:
;;
;;    (require 'ucs-utils)
;;
;; and optionally
;;
;;    (ucs-install-aliases)
;;
;; See Also
;;
;;    M-x customize-group RET ucs-utils RET
;;    http://en.wikipedia.org/wiki/Universal_Character_Set
;;
;; Notes
;;
;; Compatibility
;;
;;    Tested only on GNU Emacs version 24.1
;;
;; Bugs
;;
;; TODO
;;
;;    accept synonyms on inputs? at least Tab would be nice.
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requires

;; for callf, callf2, assert, flet, loop, let*
(eval-when-compile
  (require 'cl))

(require 'memoize nil t)

(autoload 'pp                        "pp"              "Output the pretty-printed representation of OBJECT, any Lisp object.")
(autoload 'pp-display-expression     "pp"              "Prettify and display EXPRESSION in an appropriate way, depending on length.")

(autoload 'persistent-soft-store     "persistent-soft" "Under SYMBOL, store VALUE in the LOCATION persistent data store."   t)
(autoload 'persistent-soft-fetch     "persistent-soft" "Return the value for SYMBOL in the LOCATION persistent data store." t)
(autoload 'persistent-soft-exists-p  "persistent-soft" "Return t if SYMBOL exists in the LOCATION persistent data store."   t)
(autoload 'persistent-soft-flush     "persistent-soft" "Flush data for the LOCATION data store to disk."                    t)

;;; customizable variables

;;;###autoload
(defgroup ucs-utils nil
  "Utilities for Unicode characters."
  :version "0.6.0"
  :link '(emacs-commentary-link "ucs-utils")
  :prefix "ucs-utils-"
  :group 'extensions)

(defcustom ucs-utils-trade-memory-for-speed t
  "At the cost of memory, speed up checks for UCS characters.

The speedup is greater than an order of magnitude.  The memory
cost is roughly 3 Megabytes."
  :type 'boolean
  :group 'ucs-utils)

(defcustom ucs-utils-use-persistent-storage "ucs-utils"
  "Use persistent disk storage when available.

This speeds some operations between sessions.

Internally, this value is a string which is used for the filename
of the persistent data store."
  :type '(choice
          (const :tag "Yes"  "ucs-utils")
          (const :tag "No"   nil))
  :group 'ucs-utils)

;;; variables

(defvar ucs-utils-names-hash nil "A hashed copy of the `ucs-names' alist.")
(defvar ucs-utils-all-prettified-names nil "List of all UCS names, prettified.")
(defvar persistent-soft-storage-expiration (* 60 60 24 30) "Number of seconds to keep on-disk storage.")

(defvar ucs-utils-names-corrections
  '(("LATIN CAPITAL LETTER YOGH"       . #x021C)
    ("LATIN SMALL LETTER YOGH"         . #x021D)
    ("CYRILLIC CAPITAL LETTER E"       . #x042D)
    ("CYRILLIC CAPITAL LETTER I"       . #x0418)
    ("CYRILLIC SMALL LETTER I"         . #x0438)
    ("CYRILLIC SMALL LETTER E"         . #x044D)
    ("TIBETAN LETTER -A"               . #x0F60)
    ("TIBETAN SUBJOINED LETTER -A"     . #x0FB0)
    ("GEORGIAN SMALL LETTER AN"        . #x2D00)
    ("GEORGIAN SMALL LETTER BAN"       . #x2D01)
    ("GEORGIAN SMALL LETTER GAN"       . #x2D02)
    ("GEORGIAN SMALL LETTER DON"       . #x2D03)
    ("GEORGIAN SMALL LETTER EN"        . #x2D04)
    ("GEORGIAN SMALL LETTER VIN"       . #x2D05)
    ("GEORGIAN SMALL LETTER ZEN"       . #x2D06)
    ("GEORGIAN SMALL LETTER TAN"       . #x2D07)
    ("GEORGIAN SMALL LETTER IN"        . #x2D08)
    ("GEORGIAN SMALL LETTER KAN"       . #x2D09)
    ("GEORGIAN SMALL LETTER LAS"       . #x2D0A)
    ("GEORGIAN SMALL LETTER MAN"       . #x2D0B)
    ("GEORGIAN SMALL LETTER NAR"       . #x2D0C)
    ("GEORGIAN SMALL LETTER ON"        . #x2D0D)
    ("GEORGIAN SMALL LETTER PAR"       . #x2D0E)
    ("GEORGIAN SMALL LETTER ZHAR"      . #x2D0F)
    ("GEORGIAN SMALL LETTER RAE"       . #x2D10)
    ("GEORGIAN SMALL LETTER SAN"       . #x2D11)
    ("GEORGIAN SMALL LETTER TAR"       . #x2D12)
    ("GEORGIAN SMALL LETTER UN"        . #x2D13)
    ("GEORGIAN SMALL LETTER PHAR"      . #x2D14)
    ("GEORGIAN SMALL LETTER KHAR"      . #x2D15)
    ("GEORGIAN SMALL LETTER GHAN"      . #x2D16)
    ("GEORGIAN SMALL LETTER QAR"       . #x2D17)
    ("GEORGIAN SMALL LETTER SHIN"      . #x2D18)
    ("GEORGIAN SMALL LETTER CHIN"      . #x2D19)
    ("GEORGIAN SMALL LETTER CAN"       . #x2D1A)
    ("GEORGIAN SMALL LETTER JIL"       . #x2D1B)
    ("GEORGIAN SMALL LETTER CIL"       . #x2D1C)
    ("GEORGIAN SMALL LETTER CHAR"      . #x2D1D)
    ("GEORGIAN SMALL LETTER XAN"       . #x2D1E)
    ("GEORGIAN SMALL LETTER JHAN"      . #x2D1F)
    ("GEORGIAN SMALL LETTER HAE"       . #x2D20)
    ("GEORGIAN SMALL LETTER HE"        . #x2D21)
    ("GEORGIAN SMALL LETTER HIE"       . #x2D22)
    ("GEORGIAN SMALL LETTER WE"        . #x2D23)
    ("GEORGIAN SMALL LETTER HAR"       . #x2D24)
    ("GEORGIAN SMALL LETTER HOE"       . #x2D25)
    ("HANGUL LETTER KIYEOK"            . #x3131)
    ("HANGUL LETTER PIEUP"             . #x3142)
    ("HANGUL LETTER CIEUC"             . #x3148)
    ("PARENTHESIZED HANGUL KIYEOK"     . #x3200)
    ("PARENTHESIZED HANGUL PIEUP"      . #x3205)
    ("PARENTHESIZED HANGUL CIEUC"      . #x3208)
    ("CIRCLED HANGUL KIYEOK"           . #x3260)
    ("CIRCLED HANGUL PIEUP"            . #x3265)
    ("CIRCLED HANGUL CIEUC"            . #x3268)
    ("HALFWIDTH HANGUL LETTER KIYEOK"  . #xFFA1)
    ("HALFWIDTH HANGUL LETTER PIEUP"   . #xFFB2)
    ("HALFWIDTH HANGUL LETTER CIEUC"   . #xFFB8)
    ("SQUARED MV"                      . #x1F14B)
    ("BELL"                            . #x1F514))
  "Corrections for ambiguities in `ucs-names', resolved in favor of Unicode 6.1.")

;; note: outside the ucs-utils- namespace
(defvar character-name-history nil "History of character names entered in the minibuffer.")

;;; compatibility functions

(unless (fboundp 'memoize)
  ;; by Christopher Wellons <mosquitopsu@gmail.com>
  (defun memoize (func)
    "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
    (typecase func
      (symbol (fset func (memoize-wrap (symbol-function func))) func)
      (function (memoize-wrap func))))
  (defun memoize-wrap (func)
    "Return the memoized version of the given function."
    (let ((table-sym (gensym))
          (val-sym (gensym))
          (args-sym (gensym)))
      (set table-sym (make-hash-table :test 'equal))
      `(lambda (&rest ,args-sym)
         ,(concat (documentation func) "\n(memoized function)")
         (let ((,val-sym (gethash ,args-sym ,table-sym)))
           (if ,val-sym
               ,val-sym
             (puthash ,args-sym (apply ,func ,args-sym) ,table-sym)))))))

;;; utility functions

;; Unfortunately we can't be dash-insensitive b/c UCS names are
;; sensitive to dashes eg TIBETAN LETTER -A vs TIBETAN LETTER A
;; or HANGUL JUNGSEONG O-E vs HANGUL JUNGSEONG OE.
;;
;; Counting deprecated forms, ucs-list is even space-sensitive,
;; though that could be resolved by using only the latest version.
;; Example LATIN SMALL LETTER D Z vs LATIN SMALL LETTER DZ
(defun ucs-utils--lookup (name)
  "Look up the character corresponding to NAME, a UCS name.

NAME is not sensitive to case, allows leading and trailing
double-quotes, and does not distinguish between spaces and
underscores in a UCS name.

Deprecated UCS names are permitted, but conflicts are resolved
in favor of later versions of the Unicode spec.  \"BELL\" is
a famous example of a conflict.

Returns nil if NAME does not exist."
  (save-match-data
    (callf upcase name)
    (setq name (replace-regexp-in-string "\\`[ \t\"]+" ""
                  (replace-regexp-in-string "[ \t\"]+\\'" ""
                     (replace-regexp-in-string "[ \t]\\{2,\\}" " "
                        (replace-regexp-in-string "_" " "  name)))))
    (when (and ucs-utils-trade-memory-for-speed
               (not (hash-table-p ucs-utils-names-hash)))
      (unless (hash-table-p (setq ucs-utils-names-hash (persistent-soft-fetch 'ucs-utils-names-hash ucs-utils-use-persistent-storage)))
        (let ((dupes nil)
              (key nil))
          (setq ucs-utils-names-hash (make-hash-table :size (length (ucs-names)) :test 'equal))
          (dolist (cell (ucs-names))
            (setq key (car cell))
            (when (and (gethash key ucs-utils-names-hash)
                       (not (eq (gethash key ucs-utils-names-hash) (cdr cell))))
              (push key dupes))
            (puthash key (cdr cell) ucs-utils-names-hash))
          (delete-dups dupes)
          (dolist (key dupes)
            (remhash key ucs-utils-names-hash))
          (dolist (cell ucs-utils-names-corrections)
            (puthash (car cell) (cdr cell) ucs-utils-names-hash))
        (persistent-soft-store 'ucs-utils-names-hash ucs-utils-names-hash ucs-utils-use-persistent-storage)
        (persistent-soft-flush ucs-utils-use-persistent-storage))))
    (cond
      ((hash-table-p ucs-utils-names-hash)
       (gethash name ucs-utils-names-hash))
      ((assoc-string name ucs-utils-names-corrections t)
       (cdr (assoc-string name ucs-utils-names-corrections t)))
      (t
       (cdr (assoc-string name (ucs-names) t))))))

(defun ucs-utils-vector-flatten (vec)
  "Flatten vector or string VEC which may contain other vectors or strings."
  (cond
    ((stringp vec)
     (string-to-vector vec))
    ((or (null vec)
         (and (sequencep vec)
              (= 0 (length vec))))
     nil)
    ((vectorp vec)
     (let ((elt (aref vec 0)))
       (setf (aref vec 0) nil)
       (vconcat (ucs-utils-vector-flatten elt) (ucs-utils-vector-flatten (setq vec (delete nil vec))))))
    (t
     (vector vec))))

(defun ucs-utils-prettify-ucs-string (name)
  "Prettify NAME, a string holding the UCS name for a character."
  (callf capitalize name)
  (save-match-data
    (when (string-match "([^()]+?)\\'" name)
      (setq name (replace-match (upcase (match-string-no-properties 0 name)) 'fixed-case 'literal name)))
    (let ((case-fold-search nil))
      (while (string-match " \\(?:And\\|Or\\|Of\\|On\\|The\\|For\\|In\\|With\\|Over\\|Under\\) " name)
        (setq name (replace-match (downcase (match-string-no-properties 0 name)) 'fixed-case 'literal name)))
      (while (string-match "\\<Logical and\\>" name)
        (setq name (replace-match "Logical AND" 'fixed-case 'literal name)))
      (while (string-match "\\<Logical or\\>" name)
        (setq name (replace-match "Logical OR" 'fixed-case 'literal name)))
      (while (string-match "\\<N-Ary\\>" name)
        (setq name (replace-match "N-ary" 'fixed-case 'literal name)))
      (while (string-match "\\<Per-Em\\>" name)
        (setq name (replace-match "per-Em" 'fixed-case 'literal name)))
      (while (string-match "\\<Jack-O-Lantern\\>" name)
        (setq name (replace-match "Jack-o-Lantern" 'fixed-case 'literal name)))
      (while (string-match "\\<Fleur-De-Lis\\>" name)
        (setq name (replace-match "Fleur-de-Lis" 'fixed-case 'literal name)))
      (while (string-match "\\<Left-To-Right\\>" name)
        (setq name (replace-match "Left-to-Right" 'fixed-case 'literal name)))
      (while (string-match "\\<Right-To-Left\\>" name)
        (setq name (replace-match "Right-to-Left" 'fixed-case 'literal name)))
      (while (string-match "\\<Minus-Or-Plus\\>" name)
        (setq name (replace-match "Minus-or-Plus" 'fixed-case 'literal name)))
      (while (string-match "\\<Plus-Or-Minus\\>" name)
        (setq name (replace-match "Plus-or-Minus" 'fixed-case 'literal name)))
      (while (string-match "\\<\\(?:Cjk\\|Apl\\|Ocr\\)\\>" name)
        (setq name (replace-match (upcase (match-string-no-properties 0 name)) 'fixed-case 'literal name)))
      (when (string-match "\\`Nko\\>" name)
        (setq name (replace-match "NKo" 'fixed-case 'literal name))))
    name))

(defun ucs-utils-all-prettified-names (&optional progress regenerate)
  "All prettified UCS names, cached in list `ucs-utils-all-prettified-names'.

When optional PROGRESS is given, show progress when generating
cache.

When optional REGENERATE is given, re-generate cache."
  (when regenerate
    (setq ucs-utils-all-prettified-names nil)
    (persistent-soft-store 'ucs-utils-all-prettified-names nil ucs-utils-use-persistent-storage))
  (cond
    (ucs-utils-all-prettified-names
     t)
    ((and (not regenerate)
          (persistent-soft-exists-p 'ucs-utils-all-prettified-names ucs-utils-use-persistent-storage)
          (consp (setq ucs-utils-all-prettified-names (persistent-soft-fetch 'ucs-utils-all-prettified-names ucs-utils-use-persistent-storage))))
     t)
    (t
     (let ((reporter (make-progress-reporter "Caching formatted UCS names... " 0 (length (ucs-names))))
           (counter 0)
           (draft-list nil)
           (prev-name nil))
       (dolist (cell (ucs-names))
         (when progress
           (progress-reporter-update reporter (incf counter)))
         (push (replace-regexp-in-string " " "_" (or (ucs-utils-pretty-name (cdr cell) 'no-hex) "")) draft-list))
       (dolist (name (delete "" (sort draft-list 'string<)))
         (unless (equal name prev-name)
           (push name ucs-utils-all-prettified-names))
         (setq prev-name name))
       (callf nreverse ucs-utils-all-prettified-names)
       (persistent-soft-store 'ucs-utils-all-prettified-names ucs-utils-all-prettified-names ucs-utils-use-persistent-storage)
       (persistent-soft-flush ucs-utils-use-persistent-storage)
       (progress-reporter-done reporter))))
  ucs-utils-all-prettified-names)

(defun ucs-utils--subst-char-in-region-1 (start end from-char to-char)
  "Internal driver for `usr-utils-subst-char-in-region'.

Arguments START, END, FROM-CHAR, and TO-CHAR are as documented at
`ucs-utils-subst-char-in-region'."
  ;; done in a specific fashion to maintain markers
  (loop for i from start to (1- end)
        if (eq (char-after i) from-char)
        do (save-excursion
             (goto-char i)
             (insert to-char)
             (delete-char 1))))

;;; external interface

;;;###autoload
(defun ucs-utils-char (name &optional fallback test)
  "Return the character corresponding to NAME, a UCS name.

NAME is matched leniently by `ucs-utils--lookup'.

Returns FALLBACK if NAME does not exist or is not displayable
according to TEST.  FALLBACK may be either a UCS name or
character, or one of the special symbols described in the next
paragraph.

If FALLBACK is nil or 'drop, returns nil on failure.  If FALLBACK
is 'error, throws an error on failure.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

When NAME is a character, it passes through unchanged, unless
TEST is set, in which case it must pass TEST."
  (let ((char name)
        (orig-fallback fallback))
    (when (and (eq test 'cdp)
               (not (fboundp 'cdp)))
      (setq test 'char-displayable-p))
    (when (stringp char)
      (setq char (ucs-utils--lookup char)))
    (when (stringp fallback)
      (setq fallback (ucs-utils--lookup fallback))
      (assert (integerp fallback) nil "Invalid fallback: %s" orig-fallback))
    (cond
      ((and (integerp char)
            (or (not test) (funcall test char)))
       char)
      ((eq fallback 'error)
       (error "Character invalid or undisplayable: %s" name))
      ((or (eq fallback 'drop)
           (null fallback))
       nil)
      ((vectorp fallback)
       fallback)
      (t
       (assert (integerp fallback) nil "Invalid fallback: %s" orig-fallback)
       fallback))))
(when ucs-utils-trade-memory-for-speed
  (memoize 'ucs-utils-char))

;;;###autoload
(defun ucs-utils-first-existing-char (sequence &optional test)
  "Return the first existing element in SEQUENCE of character names.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol."
 (catch 'name
   (dolist (name sequence)
     (when (ucs-utils-char name nil test)
       (throw 'name name)))))

;;;###autoload
(defun ucs-utils-string (sequence &optional fallback test)
  "Return a string corresponding to SEQUENCE of UCS names or characters.

If SEQUENCE is a single string, it will be coerced to a list of
length 1.  Each name in SEQUENCE is matched leniently by
`ucs-utils--lookup'.

FALLBACK should be a sequence of equal length to SEQUENCE, (or
one of the special symbols described in the next paragraph).  For
any element of SEQUENCE which does not exist or is not
displayable according to TEST, that element degrades to the
corresponding element of FALLBACK.

When FALLBACK is nil or 'drop, characters which do not exist or
are undisplayable will be silently dropped from the return value.
When FALLBACK is 'error, such characters cause an error to be
thrown.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol."
  (callf or fallback 'drop)
  (concat (ucs-utils-vector sequence fallback test)))

;;;###autoload
(defun ucs-utils-intact-string (sequence fallback &optional test)
  "Return a string corresponding to SEQUENCE of UCS names or characters.

This function differs from `ucs-utils-string' in that FALLBACK is
a non-optional single string, to be used unless every member of
SEQUENCE exists and passes TEST.  FALLBACK may not be nil, 'error,
or 'drop as in `ucs-utils-string'.

If SEQUENCE is a single string, it will be coerced to a list of
length 1.  Each name in SEQUENCE is matched leniently by
`ucs-utils--lookup'.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol."
  (assert (stringp fallback) nil "FALLBACK must be a string")
  (condition-case nil
      (concat (ucs-utils-vector sequence 'error test))
    (error nil
           fallback)))

;;;###autoload
(defun ucs-utils-vector (sequence &optional fallback test no-flatten)
  "Return a vector corresponding to SEQUENCE of UCS names or characters.

If SEQUENCE is a single string or character, it will be coerced
to a list of length 1.  Each name in SEQUENCE is matched
leniently by `ucs-utils--lookup'.

FALLBACK should be a sequence of equal length to SEQUENCE, (or
one of the special symbols described in the next paragraph).  For
any element of SEQUENCE which does not exist or is not
displayable according to TEST, that element degrades to the
corresponding element of FALLBACK.

When FALLBACK is nil, characters which do not exist or are
undisplayable will be given as nils in the return value.  When
FALLBACK is 'drop, such characters will be silently dropped from
the return value.  When FALLBACK is 'error, such characters cause
an error to be thrown.

To allow fallbacks of arbitrary length, give FALLBACK as a vector-
of-vectors, with outer length equal to the length of sequence.  The
inner vectors may contain a sequence of characters, a literal
string, or nil.  Eg

   (ucs-utils-vector '(\"Middle Dot\" \"Ampersand\" \"Horizontal Ellipsis\")
                     '[?.           [?a ?n ?d]  [\"...\"]              ])

or

   (ucs-utils-vector \"Horizontal Ellipsis\" '[[\"...\"]])

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

If NO-FLATTEN is non-nil, then a vector-of-vectors may be returned
if multi-character fallbacks were used as in the example above."
  (let ((strip-char :nonexistent)
        (flattener (if no-flatten 'identity 'ucs-utils-vector-flatten)))
    (cond
      ((vectorp sequence)
       (callf append sequence nil))
      ((or (stringp sequence)
           (characterp sequence))
       (callf list sequence)))
    (cond
      ((eq fallback 'drop)
       (setq strip-char nil)
       (setq fallback (make-list (length sequence) nil)))
      ((null fallback)
       (setq fallback (make-list (length sequence) nil)))
      ((eq fallback 'error)
       (setq fallback (make-list (length sequence) 'error)))
      ((vectorp fallback)
       (callf append fallback nil))
      ((or (stringp fallback)
           (characterp fallback))
       (callf list fallback)))
    (assert (and (listp sequence) (listp fallback)) nil "SEQUENCE and FALLBACK should be lists or vectors.")
    (assert (= (length sequence) (length fallback)) nil "SEQUENCE and FALLBACK should be the same length.")
    (funcall flattener (apply 'vector (delq strip-char (loop for elt in sequence
                                                             for back-elt in fallback
                                                             collect (ucs-utils-char elt back-elt test)))))))

;;;###autoload
(defun ucs-utils-pretty-name (char &optional no-hex)
  "Return a prettified UCS name for CHAR.

Based on `get-char-code-property'.  The result has been
title-cased for readability, and will not match into the
`ucs-names' alist until it has been upcased.
`ucs-utils-char' can be used on the title-cased name.

Returns a hexified string if no name is found.  If NO-HEX is
non-nil, then a nil value will be returned when no name is
found."
  (let ((name (get-char-code-property char 'name)))
    (when (equal "<control>" name)
      (setq name (get-char-code-property char 'old-name)))
    (when (eq char ?\s)
      (callf or name "Space"))
    (cond
      ((and no-hex
            (= (length name) 0))
       (setq name nil))
      ((= (length name) 0)
       (setq name (concat "#x" (upcase (format "%02x" char)))))
      (t
       (ucs-utils-prettify-ucs-string name)))))

;;;###autoload
(defun ucs-utils-subst-char-in-region (start end from-char to-char &optional no-undo)
  "From START to END, replace FROM-CHAR with TO-CHAR each time it occurs.

If optional arg NO-UNDO is non-nil, don't record this change for
undo and don't mark the buffer as really changed.

Characters may be of differing byte-lengths.

The character at the position END is not included, matching the
behavior of `subst-char-in-region'.

This function is slower than `subst-char-in-region'."
  (if no-undo
      (let ((buffer-undo-list nil)
            (modified (buffer-modified-p)))
        (ucs-utils--subst-char-in-region-1 start end from-char to-char)
        (set-buffer-modified-p modified))
    ;; else
    (ucs-utils--subst-char-in-region-1 start end from-char to-char)))

(fset 'ucs-utils-orig-read-char-by-name (symbol-function 'read-char-by-name))

;;;###autoload
(defun ucs-utils-read-char-by-name (prompt &optional ido)
  "Read a character by its Unicode name or hex number string.

A wrapper for `read-char-by-name', with the option to use
`ido-completing-read'.

PROMPT is displayed, and a string that represents a character by
its name is read.

When IDO is set, several seconds are required on the first
run as all completion candidates are pre-generated."
  (if (not ido)
      (ucs-utils-orig-read-char-by-name prompt)
    ;; else
    (let ((input (ido-completing-read
                  prompt
                  (ucs-utils-all-prettified-names 'progress) nil nil nil character-name-history)))
      (when (string-match-p "\\`[0-9a-fA-F]+\\'" input)
        (callf2 concat "#x" input))
      (if (string-match-p "\\`#" input)
          (read input)
        ;; else
        (ucs-utils--lookup input)))))

;;; interactive commands

;;;###autoload
(defun ucs-utils-eval (&optional pos arg)
  "Display a string UCS name for the character at POS.

POS defaults to the current point.

If `transient-mark-mode' is enabled and there is an active
region, return a list of strings UCS names, one for each
character in the region.  If called from Lisp with an
explicit POS, ignores the region.

If called with universal prefix ARG, display the result
in a separate buffer.  If called with two universal
prefix ARGs, replace the current character or region with
its UCS name translation."
  (interactive)
  (let ((result nil)
        (print-level nil)
        (print-length nil))
    (callf or arg current-prefix-arg)
    (cond
      ((and (not pos)
            (use-region-p))
       (dolist (char (string-to-list (buffer-substring-no-properties (region-beginning) (region-end))))
         (push (ucs-utils-pretty-name char) result))
       (setq result (nreverse result)))
      (t
       (save-excursion
         (goto-char (or pos (point)))
         (setq result (ucs-utils-pretty-name (char-after))))))
    (assert result nil "Failed to find name for character at: %s" pos)
    (cond
      ((equal arg '(4))
       (flet ((frame-width (&rest args) 0))
         (pp-display-expression result "*Pp Eval Output*")))
      ((consp arg)
       (if (and (not pos)
                (use-region-p))
           (progn
             (let ((begin (region-beginning)))
               (delete-region (region-beginning) (region-end))
               (goto-char begin)
               (pp result (current-buffer))))
         ;; else
         (goto-char (or pos (point)))
         (insert (concat "\"" (prin1 result) "\""))
         (delete-char 1)))
      (t
       (pp-display-expression result "*Pp Eval Output*")))))

;;;###autoload
(defun ucs-utils-ucs-insert (character &optional count inherit)
  "Insert CHARACTER in COUNT copies, where CHARACTER is a Unicode code point.

Works like `ucs-insert', but uses `ido-completing-read'.

In addition, if `transient-mark-mode' is enabled, and the region
contains a valid UCS character name, that value is used as the
character name and the region is replaced.

INHERIT is as documented at `ucs-insert'."
  (interactive
   (list
    (if (and (use-region-p)
             (ucs-utils-char (buffer-substring-no-properties (region-beginning) (region-end)) nil))
        (prog1
          (ucs-utils-char (buffer-substring-no-properties (region-beginning) (region-end)) nil)
          (delete-region (region-beginning) (region-end)))
      ;; else
      (ucs-utils-read-char-by-name "Unicode (name or hex): " 'ido))
    (prefix-numeric-value current-prefix-arg)
    t))
  (ucs-insert character count inherit))

;;;###autoload
(defun ucs-utils-install-aliases ()
  "Install aliases outside the \"ucs-utils-\" namespace.

The following aliases will be installed

   ucs-char                  for   ucs-utils-char
   ucs-first-existing-char   for   ucs-utils-first-existing-char
   ucs-string                for   ucs-utils-string
   ucs-intact-string         for   ucs-utils-intact-string
   ucs-vector                for   ucs-utils-vector
   ucs-pretty-name           for   ucs-utils-pretty-name
   ucs-eval                  for   ucs-utils-eval"
  (interactive)
  (defalias 'ucs-char                 'ucs-utils-char)
  (defalias 'ucs-first-existing-char  'ucs-utils-first-existing-char)
  (defalias 'ucs-string               'ucs-utils-string)
  (defalias 'ucs-intact-string        'ucs-utils-intact-string)
  (defalias 'ucs-vector               'ucs-utils-vector)
  (defalias 'ucs-pretty-name          'ucs-utils-pretty-name)
  (defalias 'ucs-eval                 'ucs-utils-eval))

(provide 'ucs-utils)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; End:
;;
;; LocalWords:  UcsUtils utils eval callf flet YOGH alist ZHAR PHAR
;; LocalWords:  KHAR GHAN JHAN KIYEOK PIEUP CIEUC HALFWIDTH Fleur
;; LocalWords:  JUNGSEONG
;;

;;; ucs-utils.el ends here
