;;; mon-error-utils.el --- extensions for conditions and error handling
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-error-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-09-15T20:42:00-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs, development, 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-error-utils provides extensions for conditions and error handling
;; The functions `mon-error-toplevel' and `mon-error' are intended as entry
;; points for a lightweight MON specific condition system. Time will tell.
;;
;; FUNCTIONS:►►►
;; `mon-error-toplevel', `mon-error', `mon-error-gather',
;; `mon-error-gather-peek', `mon-error-gather-finalize',
;; `mon-error-gather-reset', `mon-display-warning', `mon-message',
;; `mon-error-string-err-format', `mon-write-string',
;; `mon-truncate-path-for-prompt', `mon-format',
;; `mon-string-not-null-nor-zerop-ERROR', `mon-symbol-void-ERROR',
;; `mon-vectorp-ERROR', `mon-file-non-existent-ERROR',
;; `mon-buffer-non-existent-ERROR', `mon-list-proper-p-ERROR'
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
;; `*mon-error-gather*', `*mon-message-function*', `*mon-error-utils-xrefs*',
;;
;; GROUPS:
;; `mon-error-utils', `mon-error-warn'
;;
;; ALIASED/ADVISED/SUBST'D:
;; `write-string'                             -> `mon-write-string'
;; `mon-dir-name-truncate-for-prompt'         -> `mon-truncate-path-for-prompt'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-truncate-path-for-prompt'       <- mon-dir-utils.el
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-error-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-09-17T19:21:40-04:00Z}#{10375} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-error-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-09-15T20:42:00-04:00Z}#{10373} - by MON KEY>
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

(declare-function %mon-format-chk-keys         "mon-macs"      (w-keys-lst))
(declare-function mon-booleanp                 "mon-utils"     (putative-boolean))
(declare-function mon-function-object-p        "mon-utils"     (fncn-sym))
(declare-function mon-equality-or-predicate    "mon-utils"     (predicate arg1 arg2))
(declare-function mon-booleanp-to-binary       "mon-utils"     (maybe-a-boolean &optional return-if-not))
(declare-function mon-string-or-null-and-zerop "mon-utils"     (maybe-str-or-null-obj))
(declare-function mon-bool-vector-pp           "mon-seq-utils" (bool-vec))
(declare-function mon-subseq                   "mon-seq-utils" (in-seq seq-start &optional seq-end))
(declare-function mon-mapcar                   "mon-seq-utils" (mapcar-fun mapcar-lst &rest more-lsts))
(declare-function mon-list-proper-p            "mon-seq-utils" (putatively-proper))


;;; ==============================
;;; :CHANGESET 2389
;;; :CREATED <Timestamp: #{2011-01-12T20:41:13-05:00Z}#{11023} - by MON KEY>
(defgroup mon-error-utils nil
  "Customization group for mon symbols, functions, variables, errors, warnings.\n
:SEE-ALSO .\n►►►"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE (URL `http://www.emacswiki.org/emacs/mon-error-utils.el')"
          "http://www.emacswiki.org/emacs/mon-error-utils.el")
  :link '(emacs-library-link 
          :tag ":FILE mon-error-utils.el" 
          "mon-error-utils.el")
  :group 'mon-base)

;;; ==============================
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-02T12:06:51-04:00Z}#{10396} - by MON KEY>
(defgroup mon-error-warn nil
  "Customization group for mon errors and warnings.\n
:SEE-ALSO `*mon-error-utils-xrefs*'.\n►►►"
  :group 'mon-error-utils)

;;; ==============================
;;; :CHANGESET 2389
;;; :CREATED <Timestamp: #{2011-01-12T19:48:17-05:00Z}#{11023} - by MON KEY>
(defcustom *mon-error-utils-xrefs* 
  '(mon-error-toplevel mon-error mon-error-string-err
    mon-error-string-err-format mon-string-not-null-nor-zerop-ERROR
    mon-symbol-void-ERROR mon-vectorp-ERROR mon-file-non-existent-ERROR
    mon-buffer-non-existent-ERROR mon-list-proper-p-ERROR
    mon-error-gather mon-error-gather-peek mon-error-gather-reset
    mon-error-gather-finalize mon-format mon-message
    mon-truncate-path-for-prompt mon-display-warning %mon-write-string-pusher
    %mon-write-string-writer %mon-write-string-reader %mon-write-string-mapper
    %mon-write-string-reset mon-write-string 
    ;; :VARIABLES
    *mon-error-gather* *mon-message-function* *mon-error-utils-xrefs*)
  "Xrefing list of `mon-error-*' symbols, functions constants, and variables.\n
The symbols contained of this list are defined in :FILE <FILE>\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-keybindings-xrefs*',
`*mon-testme-utils-xrefs*', `*mon-button-utils-xrefs*',
`*mon-buffer-utils-xrefs*', `*mon-line-utils-xrefs*', `*mon-plist-utils-xrefs*',
`*mon-post-load-hooks-xrefs*', `*mon-seq-utils-xrefs*',
`*mon-string-utils-xrefs*', `*mon-type-utils-xrefs*',
`*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*', `*mon-slime-xrefs*',
`*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*', `*mon-ulan-utils-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-error-utils
  :group 'mon-xrefs)

;;; ==============================
;;; :CHANGESET 2141
;;; :CREATED <Timestamp: #{2010-09-17T19:18:44-04:00Z}#{10375} - by MON KEY>
(defvar *mon-error-gather* nil
  "Temporary string container for gathering error messages.\n
:SEE-ALSO `mon-error-toplevel' `mon-error', `mon-error-protect',
`mon-error-gather', `mon-error-gather-peek', `mon-error-gather-finalize',
`mon-error-gather-reset', `*mon-emacs-help-errors*', 
`mon-error-string-err-format', `mon-message', `mon-help-errors',
`mon-help-CL-error-condition-restart', `report-errors'.\n►►►")

;;; ==============================
;;; :COURTESY slime.el :WAS `slime-message-function'
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-02T11:10:48-04:00Z}#{10396} - by MON KEY>
(defcustom *mon-message-function* 'message
  "Defualt message formatting function for `mon-message'.\n
Function is called with a format-string spec and optionally a list of arguments to
format-string's spec e.g. function is `apply'ed as:\n
 \(apply <FUNCTION> <FORMAT-SPEC> <FORMAT-ARGS>\)\n
:EXAMPLE\n\n\(let* \(\(eg-fncn #'\(lambda \(frmt-str &rest frmt-args\)
                  \(apply 'format frmt-str frmt-args\)\)\)
       \(*mon-message-function* eg-fncn\)\)
  \(apply *mon-message-function*
         \(concat 
          \"Example with special variable `*mon-message-function*' dynamically bound:\\n\"
          \"<VALUE-AS-IF-BY-PRINC> %s\\n<VALUE-AS-DECIMAL> %d\\n<VALUE-AS-IF-BY-PRIN1> %S\\n\"\)
          '\(\"Some `princ' like string \"  8 '\(some-sexp-value \"as if by `prin1'\"\)\)\)\)\n
:SEE-ALSO `message', `mon-format' `mon-error-string-err-format',
`report-errors'.\n►►►"
  :type 'function
  :group 'mon-error-utils)

 
;;; ==============================
;;; :PREFIX "mfrmt-"
;;; :CHANGESET 2353
;;; :CREATED <Timestamp: #{2010-12-06T12:56:39-05:00Z}#{10491} - by MON KEY>
(defun mon-format (&rest fmt-keys)
  "Like `format' but supports keywords and multi string/arg sequences.\n
Keywords supported: :W-FUN :W-SPEC :W-ARGS :W-DELIM\n
Keyword :W-FUN is a symbol naming a function to apply to :W-SPEC and :W-ARGS.
When omitted defaults to `format'. When provided it should satisfy
`mon-function-object-p', signal an error if not.\n
Keyword :W-SPEC is a string, or list of strings to pass to :W-FUN it
is coalesced as if by `mapconcat'.\n
Keyword :W-ARGS is an atom or sequence of arguments satisyfying
`mon-sequence-mappable-p' and is applied as arguments to a `format' spec.\n
Keyword :W-DELIM is a string to delimit _prefix_ of each line of return value.\n
:EXAMPLE\n
\(mon-format :w-fun  #'error 
            :w-spec '\(\":FUNCTION `mon-format' \" 
                      \"-- keyword :W-FUN does not satisfy \"
                      \"`mon-function-object-p', got: %S\"\)
            :w-args 'some-fun-that-is-not\)\n
\(mon-format :w-spec \(concat \":FUNCTION `mon-format' \" 
                            \"-- example message with arg `%s' and arg `%d'\"\)
            :w-args '(bubba 666\)\)\n
\(mon-format :w-spec '\(\":FUNCTION `mon-format' \" 
                      \"-- example message with arg `%s' and arg `%d'\"\)
            :w-args '\(bubba 666\)\)\n
\(mon-format :w-fun  '\(lambda \(&rest x\) 
                       \(with-output-to-string \(princ \(apply 'format x\)\)\)\)
            :w-spec '\(\":FUNCTION `mon-format' \" \"-- `%d'\"\) 
            :w-delim \"\\n\"
            :w-args  666\)\n
\(mon-format :w-fun  #'\(lambda \(&rest x\) 
                        \(with-output-to-string \(princ \(apply 'format x\)\)\)\)
            :w-spec \":FUNCTION `mon-format' -- `%d'\" 
            :w-args  666\)\n
\(mon-format :w-fun  #'\(lambda \(&rest x\) 
                        \(with-output-to-string \(princ \(apply 'format x\)\)\)\)
            :w-spec  '\(\":FUNCTION `mon-format' \" \"bubba-%d\" \"bubba-%d\" \"bubba-%d\"\)
            :w-delim \"\\n-- \"
            :w-args  \(number-sequence 8 10\)\)\n
\(mon-format :w-spec  [\":FUNCTION `mon-format' \" \"bubba-%d\" \"bubba-%d\" \"bubba-%d\"]
            :w-delim \"\\n-- \"
            :w-args  \(number-sequence 8 10\)\)\n
\(mon-format :w-spec  [\":FUNCTION `mon-format' \" \"bubba-%d\" \"bubba-%d\" \"bubba-%d\"]
            :w-delim \"\\n-- \"
            :w-args  `[,@\(number-sequence 8 10\)]\)\n
\(mon-format :w-fun  '\(lambda \(&rest x\) 
                        \(with-output-to-string \(princ \(apply 'format x\)\)\)\)
            :w-spec `\(\":FUNCTION `mon-format' \" 
                      ,\(concat \"example message\" \" with arg `%s' \"\)
                      \"and arg `%d'\"\)
            :w-delim \"  \\n-- \"
            :w-args  `[bubba ,\(+ 8 666\)]\)

As a special case, when value of keyword :W-SPEC satisfies `bool-vector-p',
return value has a somewhat different semantics, and its final value depends on
the interaction of arguments to the :W-ARGS and :W-DELIM keys.

When :W-ARGS is non-nil, it should be a sequence of integer indexes into a
bool-vector greater than length 0. Its values are mapped and displayed according to
the boolean value at each integer index such that each indexed value generates a
corresponding formatted line have one of the following forms:\n
 :at-index <INT> :with-value \(t   . 1\)
 :at-index <INT> :with-value \(nil . 0\)

The returned consed pair at the second key `:with-value` corresponds to the
boolean value at a particular bool-vector index. The boolean value at index is
presented in both t/nil and 0/1 representations such that car is a lisp boolean
either { t | nil } and cdr is a binary integer value either { 0 | 1 }.

When keyword :W-ARGS is non-nil, the first two elements of return value will
always contain the following keys:\n
 `:bit-string` and `:bool-vector`

The first, `:bit-string`, will appear at the first line of return value its
value is always suffixed as either:\n
 :bit-string #b{...}
 :bit-string #*{...}

The #b{...} suffix is returned for bool-vectors with length less than 29.
This notation is readable by Emacs and Common Lisp as a binary number.

The #*{...} suffix is returned for bool-vectors with length greater than 29.
This notation is readable by Common Lisp as a bit vector.

The second, `:bool-vector` will appear on the second line of return value and is
always suffixed as:\n
  :bool-vector #&<INT>\"{....}\"

This is a readable bool-vector for Emacs lisp use returned as if by `prin1'.

When :W-ARGS is non-nil and :w-delim is ommited or its value is `t', the default
prefix for each line of return value is a one character string containing 
a single whitespace character \(char 32, #o40, #x20\) e.g. \" \".

When :W-ARGS is non-nil and :W-DELIM is non-nil, it is a string to prepend to
each line of return value.  Its first character should be a `\\n' \(char 10,
#o12, #xa\) followed by any number of additional characters. However, the first
line of return value will not be preceded by a leading newline, IOW if the char
at index 0 is `\\n', it alone is stripped but all subsequent chars remain.

When both keywrods :W-ARGS and :W-DELIM are ommitted, return value is a single
line without trailing whitespace containing the appropriate bit-vector or binary
number form with either the #*{...} or #b{...} notation.

When :W-ARGS is ommited and :W-DELIM is `t', return value is as the default
above but with each line prepended with a single whitespace character \(char 32,
#o40, #x20\), and with the first line returned containing an appropriate
keys/value pair either:\n
  :bit-string #b{...}
  :bit-string #*{...}

and with the second line returned containing the key/value pair for a readable
bool-vector, e.g.:\n
  :bool-vector #&<INT>\"{....}\"

When :W-ARGS is ommited and :W-DELIM is the empty string (e.g. \"\"), return
value is as above but first line returned does not contain the `:bit-string'
prefix nor any leading whitespace and instead contains only a bit-vector or
binary number form with either the #*{...} or #b{...} notation.

When :W-ARGS is ommited and keyword :W-DELIM is non-nil and neither `t' nor the
empty-string, it should be a string value to prepend to both the first and
second line of return value. In which case, :W-DELIM's first character should be
a `\\n' (char 10, #o12, #xa) followed by any number of additional
characters. Again, the first line of return value will not be preceded by a
leading newline, it is stripped but any subesequent chars remain.

:EXAMPLE\n
\(mon-format :w-spec \(make-bool-vector 18 t\)\)\n
\(mon-format :w-spec \(make-bool-vector 38 t\) :w-delim \"\"\)\n
\(mon-format :w-spec \(make-bool-vector 28 t\) :w-delim t\)\n
\(mon-format :w-spec \(make-bool-vector 31 t\) :w-delim \"\\n-- \"\)\n
\(mon-format :w-spec \(make-bool-vector 31 t\) :w-args '\(8 13 18 22 26\)\)\n
\(mon-format :w-spec \(make-bool-vector 18 t\) :w-delim \"\\n ---+ \"\)\n
\(mon-format :w-spec \(make-bool-vector 31 t\) :w-delim \"\\n-- \"\)\n
\(mon-format :w-spec \(let \(\(bv \(make-bool-vector 18 t\)\)\)
                       \(dolist \(i \(number-sequence 1 18 3\) bv\)
                         \(aset bv i nil\)\)\)
            :w-delim \"\\n ---+ \" 
            :w-args \(number-sequence 0 17\)\)\n
\(mon-format :w-spec \(make-bool-vector 31 t\) 
            :w-args [8 13 18 22 26]
            :w-delim \"\\n ^_^ \"\)\n
:SEE-ALSO `mon-message', `mon-display-warning', `mon-sequence-mappable-p',
`mon-bool-vector-pp', `mon-booleanp-to-binary', `mon-string-or-null-and-zerop',
`mon-booleanp', `mon-string-not-null-nor-zerop', `mon-string-or-null-and-zerop',
`mon-list-proper-p', `mon-subseq', `mon-mapcar', `vectorp', `stringp',
`bool-vector-p', `*mon-function-object-types*'.\n►►►
\n\(fn &key W-FUN W-SPEC W-ARGS W-DELIM\)"
  ;; \n\(fn &key W-FUN <SYMBOL> W-SPEC <OBJECT> W-ARGS <OBJECT> W-DELIM <STRING>|<BOOLEAN>\)"
  (destructuring-bind (w-fun w-spec w-args w-delim)
      (%mon-format-chk-keys fmt-keys)
    (apply
     (or (and (null w-fun) #'format)
         ;; :TODO Convert macro args to `funcall'able lambda forms.
         (and (memq (mon-function-object-p w-fun)
                    (remove 'macro *mon-function-object-types*)) 
              w-fun)
         (mon-format :w-fun #'error 
                     :w-spec '(":FUNCTION `mon-format' " 
                               "-- keyword :W-FUN does not satisfy "
                               "`mon-function-object-p', got: %S, type-of: %S")
                     :w-args `(,w-fun ,(type-of w-fun))))
     (or 
      ;; W-SPEC is `stringp' or `bool-vector-p'.
      (and 
       (or 
        ;; W-SPEC is a string.
        (and (stringp w-spec) w-spec)
        ;; W-SPEC is a bool-vector.
        (and (bool-vector-p w-spec)               
             (or (and (not w-args) 
                      (setq w-args (mon-bool-vector-pp w-spec))
                      (setq w-spec `(,(elt w-args 1)
                                     ,(format "%s %S" 
                                              (car (memq :bool-vector w-args))
                                              (cadr (memq :bool-vector w-args)))))
                      (or (setq w-args nil) t))
                 (and w-args
                      ;; W-ARGS is either a proper list or a vector, else signal an error.
                      (or (or (mon-list-proper-p w-args)
                              (and (vectorp w-args)
                                   ;; W-ARGS gets `push'/`pop'd below.
                                   (setq w-args (append w-args nil))))
                          (mon-format :w-fun  #'error 
                                      :w-spec '(":FUNCTION `mon-format' "
                                                "--  with keyword :W-SPEC `bool-vectorp' and "
                                                "keyword :W-ARGS not `mon-list-proper-p' or `vectorp', " 
                                                "got: %S type-of: %S")
                                      :w-args `(,w-args ,(type-of w-args))))
                      (setq w-spec (mon-bool-vector-pp w-spec))
                      (let* ((mfrmt-bv (cadr (memq :bool-vector w-spec)))
                             ;; Don't try to access an elt outside the bv's indexable range.
                             (mfrmt-idx-bnd  (length mfrmt-bv)))
                        (setq mfrmt-idx-bnd 
                              (mapcar #'(lambda (mfrmt-L-0)
                                          ;; `mon-bool-vector-pp' signals when (= (length  bv) 0)
                                          ;; IOW don't worry about 0 length bv bugs, just ensure
                                          ;; we have `wholenump' that doesn't exceed her bounds.
                                          (let (mfrmt-L-0-lcl)
                                            (when (or (and (wholenump mfrmt-L-0) (< mfrmt-L-0 mfrmt-idx-bnd))
                                                      (mon-format :w-fun #'error
                                                                  :w-spec '(":FUNCTION `mon-format' "
                                                                            " -- keyword :W-ARGS has elt not `wholenump' "
                                                                            " when keyword :W-SPEC `bool-vectorp', "
                                                                            "got: %S type-of: %S")
                                                                  :w-args `(,w-args ,(type-of w-args))))
                                              (setq mfrmt-L-0-lcl (aref mfrmt-bv mfrmt-L-0))
                                              (setq mfrmt-L-0-lcl 
                                                    `(,mfrmt-L-0 (,mfrmt-L-0-lcl . ,(mon-booleanp-to-binary mfrmt-L-0-lcl)))))))
                                      w-args))
                        ;; Record how many W-ARGS we got. We end up w/ a list of the form: 
                        ;;  (10 (1 (t . 1)) (4 (nil . 0)) (7 (nil . 0)) { ... } )
                        (setq w-args (push (length mfrmt-idx-bnd) mfrmt-idx-bnd))
                        ;; Now map over the W-DELIM string.
                        (and (or (and w-delim (stringp w-delim)
                                      ;; Hold onto the W-DELIM arg for when everything comes back together.
                                      (setq w-delim (cons ":at-index %2d :with-value %S" w-delim)))
                                 (and (or (mon-string-or-null-and-zerop w-delim)
                                          ;; W-DELIM was `t' - ignore it.
                                          (and (car (mon-booleanp w-delim))
                                               (or (setq w-delim nil) t))
                                          ;; Disregard any non-string.
                                          (not (stringp w-delim)))
                                      ;; Put "w-delim-ommitted\n " to prevent putting "\n " at beginning of return string.
                                      (setq w-delim (cons ":at-index %2d :with-value %S" "w-delim-ommitted\n  "))))
                             ;; Stay inside the `or' branch. Build a list of format strings using W-DELIM and W-ARGS.
                             (setcar w-delim (make-list (pop w-args) (car w-delim)))
                             (prog2
                                 ;; Re-use local var `mfrmt-idx-bnd` to store the mapped value.
                                 (setq mfrmt-idx-bnd 
                                       (mon-mapcar #'(lambda (mfrmt-L-1-a mfrmt-L-1-b)
                                                       (apply #'format `(,mfrmt-L-1-a ,(car mfrmt-L-1-b) ,(cadr mfrmt-L-1-b))))
                                                   (car w-delim) w-args))
                                 ;; :NOTE Following is to allow using a user-supplied
                                 ;; delim on every line including the first first line. 
                                 ;; Otherwise, all lines are prefixed with two spaces.
                                 (setq w-spec                                  
                                       (concat 
                                        (or (and (string-match-p "w-delim-ommitted\n  " (cdr w-delim))
                                                 (setcdr w-delim (replace-regexp-in-string "w-delim-ommitted" "" (cdr w-delim)))
                                                 "  ")
                                            (replace-regexp-in-string "^\n" "" (cdr w-delim)))
                                        ;; Make it it like this:
                                        ;;  :binary|:bit-string "#[b*]101001 {...}
                                        (apply #'format "%s %s%s" `(,@(mon-subseq w-spec 0 2) ,(cdr w-delim) ))
                                        ;; `mon-bool-vector-pp' has variable length return value
                                        ;;  We already have hold of its last elt, so use it.
                                        (format ":bool-vector %S%s" mfrmt-bv (cdr w-delim))
                                        ;; "\n  :at-index %d :with-value %S" {...}
                                        (mapconcat #'identity mfrmt-idx-bnd (cdr w-delim)) ))
                               (setq w-args nil)
                               (setq w-delim 'bool-vector))))))))
       ;; W-DELIM was provided and not a zero length string so map it.
       (or (and (or (and (not (eq w-delim 'bool-vector))
                         ;; If W-SPEC is a list it was originally abool-vector.
                         (mon-list-proper-p w-spec)
                         (or 
                          ;; It was the boolean `t'.
                          (and (car (mon-booleanp w-delim))
                               ;; This for congruence with the return value when W-ARGS was non-nil. 
                               ;; e.g. every line is preceded by with: " :colon-pfxd-key <VALUE>" 
                               ;; Else, we leave first line as:  "#b{...}" or "#*{...}"
                               ;; and callers can still easily parse the bit-rep up to the first EOL.
                               (setcar w-spec (concat "  :bit-string " (car w-spec)))
                               (setq w-delim "\n  "))
                          (and (mon-string-not-null-nor-zerop w-delim)
                               (setcar w-spec (concat (replace-regexp-in-string "^\n" "" w-delim) ":bit-string " (car w-spec))))
                          ;; It was null or zero length string.
                          (and (mon-string-or-null-and-zerop w-delim)
                               ;; If it was null, return only the one line as either: "#b{...}" or "#*{...}"
                               ;; Else, return as two lines with only the second prefixed:
                               ;;  "#b{...}"
                               ;;  :bit-vector #&{...}
                               (or (and (null w-delim)
                                        (setq w-delim "")
                                        (setq w-spec (car w-spec)))
                                   (setq w-delim "\n")))))
                    ;; Don't look further we've already handled the bool-vector.
                    (not (eq w-delim 'bool-vector)))
                (or 
                 ;; W-DELIM was the boolean `t'
                 (and (cadr (mon-booleanp w-delim)) (setq w-delim ""))
                 ;; If we're here its not null and the next test guarantees a string.
                 w-delim)
                (or (mon-string-or-null-and-zerop w-delim)
                    ;; Allow W-DELIM as the empty string "", but delay so we error when not `stringp'.
                    (or (stringp w-delim)
                        (mon-error-string-err-format "mon-error-string-err-format" "w-delim" w-delim t)))
                ;; When W-SPEC is `bool-vector-p' it was converted to a list above.
                ;; This is the only way W-SPEC can possibly satisfy `mon-list-proper-p'
                ;; inside this branch. So, get the value of `:binary` or `:bin-string`
                ;; keyword either:  "#b{...}" or "#*{...}"
                (mapconcat #'identity 
                           (or (and (mon-list-proper-p w-spec)
                                    w-spec)
                               (list (or w-spec "")))
                           w-delim))
           w-spec))
      ;; W-SPEC is a proper list or `vectorp'.
      (and  (or (mon-list-proper-p w-spec)
                (eq (type-of w-spec) 'vector))
            ;; It is, so make sure each elt is `stringp' before mapping.
            (or (= (apply #'+ (mapcar #'(lambda (mfrmt-L-2)
                                          (mon-booleanp-to-binary (not (stringp mfrmt-L-2))))
                                      w-spec)) 
                   0)
                (mon-format :w-fun #'error
                            :w-spec '(":FUNCTION `mon-format' " 
                                      "-- element list supplied for keyword :W-SPEC "
                                      "does not satisfy `stringp', got: %S type-of %S")
                            :w-args `(,w-spec ,(type-of w-spec))))
            ;; If W-DELIM was provided and `stringp' use it.
            ;; If we got `t' and W-ARGS is null use "\n" as delim.
            ;; If not provided, or , or its a zero length string map `""`.
            ;; Else, its some other thing - signal an error.
            (mapconcat #'identity w-spec 
                       (or (and (stringp w-delim) w-delim)
                           (or (and (mon-string-or-null-and-zerop w-delim) "")
                               (and (car (mon-booleanp w-delim))
                                    (or (and (not w-args) "\n")
                                        "")))
                           (and w-delim 
                                (mon-error-string-err-format "mon-error-string-err-format" "w-delim" w-delim t))))))
     ;; Finish up for the outer apply form.
     (or (and (mon-list-proper-p w-args) w-args)
         (and (vectorp w-args) (append w-args nil))
         (and (atom w-args) (list w-args))))))

 
;;; ==============================
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-02T11:10:54-04:00Z}#{10396} - by MON KEY>
(defun* mon-message (&key msg-spec msg-args msg-delim (msg-fun *mon-message-function*))
  "Like `message' but supports keyword args and multi-line delimited messages.\n
Keywords supported: :MSG-FUN :MSG-SPEC :MSG-ARGS :MSG-DELIM\n
Keyword :MSG-FUN is a symbol naming a function to apply to :MSG-SPEC and :MSG-ARGS.
When omitted defaults to value of variable `*mon-message-function*'.
Siganl an error if it does not satisfy `mon-function-object-p'.\n
Keyword :MSG-SPEC is a string, or list of strings to pass to message MSG-MSG-FUN it
is coalesced as if by `mapconcat'.\n
Keyword :MSG-ARGS is an atom or sequence of arguments satisyfying
`mon-sequence-mappable-p' and applied as arguments to a `format' spec.\n
Keyword :MSG-DELIM is a string to delimit _prefix_ of each line of return value.\n
:EXAMPLE\n
\(mon-message :msg-spec '\(\":FUNCTION `mon-message' \" \"-- `%d'\"\) 
             :msg-delim \"\\n\"
             :msg-args 666\)\n
\(mon-message :msg-fun #'minibuffer-message
             :msg-spec '\(\":FUNCTION `mon-message' \" 
                         \"-- example message with arg `%s' and arg `%d'\"\)
             :msg-args '\(bubba 666\)\)\n
:NOTE Keywords are as those of `mon-format' and have an equivalent semantics as
indicated with the following table:\n
 `mon-format'   `mon-message'
   :w-fun     <-  :msg-fun 
   :w-spec    <-  :mst-spec
   :w-args    <-  :msg-args
   :w-delim   <-  :msg-delim\n
:SEE-ALSO `mon-display-warning', `minibuffer-message',
`mon-error-string-err-format', `mon-symbol-void-ERROR', `mon-vectorp-ERROR',
`mon-file-non-existent-ERROR', `mon-buffer-non-existent-ERROR', `assert'.\n►►►"
  ;; (fn [&keys msg-fun <VALUE> msg-spec <VALUE> msg-args <VALUE> msg-delim <VALUE>])
  (apply 'mon-format `(:w-fun   ,msg-fun 
                       :w-spec  ,msg-spec 
                       :w-args  ,msg-args 
                       :w-delim ,msg-delim)))

;;; ==============================
;;; :CHANGESET 2358
;;; :CREATED <Timestamp: #{2010-12-08T21:35:58-05:00Z}#{10493} - by MON KEY>
(defun* mon-string-not-null-nor-zerop-ERROR (&key fun-name locus got-val w-error)
  "Format and maybe signal error for `mon-string-not-null-nor-zerop'.\n
Keyword :FUN-NAME is the function orginating the error.\n
Keyword :LOCUS is a symbol naming the paramter which can not be satisfied.\n
Keyword :GOT-VAL is the local symbol or object naming the offending value.\n
Keyword :W-ERROR when non-nil will pass generated format string to error.\n
:EXAMPLE\n\n\(mon-string-not-null-nor-zerop-ERROR\)\n
\(mon-string-or-null-and-zerop-ERROR :fun-name \"some-function-name\"\)\n
\(mon-string-or-null-and-zerop-ERROR :fun-name \"some-function-name\" 
                                    :locus \"some-param-name\"\)\n
\(mon-string-or-null-and-zerop-ERROR :fun-name \"some-function-name\" 
                                    :locus \"some-param-name\" 
                                    :got-val 8.8\)\n
\(mon-string-or-null-and-zerop-ERROR :w-error t
                                    :fun-name \"some-function-name\" 
                                    :locus \"some-param-name\" 
                                    :got-val 8.8\)\n
:SEE-ALSO `mon-error-string-err-format', `mon-symbol-void-ERROR',
`mon-vectorp-ERROR', `mon-file-non-existent-ERROR',
`mon-buffer-non-existent-ERROR', `mon-format', `mon-error', `mon-message',
`mon-error-toplevel',`mon-error-gather', `mon-error-gather-peek',
`*mon-emacs-help-errors*', `mon-help-errors', `assert'.\n►►►"
  ;; (fn &key FUN-NAME <STRING> LOCUS <STRING> GOT-VAL <OBJECT> W-ERROR <BOOLEAN>)
  (mon-format :w-fun (and w-error #'error)
              :w-spec `(,(or (and fun-name ":FUNCTION `%s' -- ") "%s") 
                        ,(or (and locus "arg %s does not satisfy ")
                             "%sCan not satisfy ")
                        "`mon-string-not-null-nor-zerop'"
                        ,(or (and got-val ", got: %S type-of: %s") "%s"))
              :w-args `(,(or fun-name "")
                        ,(or (and locus (upcase locus)) "")
                        ,@(or (and got-val `(,got-val ,(type-of got-val)))
                              `("")))))
;;
;; (put 'mon-string-not-null-nor-zerop-ERROR  'common-lisp-indent-function
;;  
;; (mon-string-not-null-nor-zerop-ERROR)
;; (mon-string-not-null-nor-zerop-ERROR :fun-name "some-function-name")
;; (mon-string-not-null-nor-zerop-ERROR :fun-name "some-function-name" :locus "some-param-name")
;; (mon-string-not-null-nor-zerop-ERROR :fun-name "some-function-name" :locus "some-param-name" :got-val 8.8)
;; (mon-string-not-null-nor-zerop-ERROR :w-error t :fun-name "some-function-name" :locus "some-param-name" :got-val 8.8)

;;; ==============================
;;; :CHANGESET 2370
;;; :CREATED <Timestamp: #{2010-12-31T13:24:02-05:00Z}#{10525} - by MON KEY>
(defun* mon-symbol-void-ERROR (&key fun-name locus got-val w-error)
  "Format and maybe signal error for void or uninterned symbols not in obarray.\n
Keyword :FUN-NAME is the function orginating the error.\n
Keyword :LOCUS is a symbol naming the paramter which can not be satisfied.\n
Keyword :GOT-VAL is a string naming the local void/uninterned symbol.
Its value must satisfy `mon-string-not-null-nor-zerop', an error is signaled if not.\n
Keyword :W-ERROR when non-nil will pass generated format string to error.\n
:EXAMPLE\n\n\(mon-symbol-void-ERROR :fun-name \"tt--fun\" 
                       :locus \":tt-locus\" 
                       :got-val \(symbol-name 
                                 \(make-symbol 
                                  \(format \"%d--%s\" \(incf *gensym-counter*\) \"nope\"\)\)\)\)\n
\(mon-symbol-void-ERROR :w-error t
                       :fun-name \"tt--fun\" 
                       :locus \":tt-locus\" 
                       :got-val \(symbol-name
                                 \(make-symbol 
                                  \(format \"%d--%s\" \(incf *gensym-counter*\) \"nope\"\)\)\)\)\n
;; Following will fail successfully b/c keyword :GOT-VAL is not `stringp'\n
\(mon-symbol-void-ERROR :fun-name \"tt--fun\" 
                       :locus \":tt-locus\" 
                       :got-val \(make-symbol 
                                 \(format \"%d--%s\" \(incf *gensym-counter*\) \"nope\"\)\)\)\n
:SEE-ALSO `mon-string-not-null-nor-zerop-ERROR', `mon-vectorp-ERROR',
`mon-file-non-existent-ERROR', `mon-buffer-non-existent-ERROR', `assert',
`mon-format', `mon-message', `mon-error',
`mon-error-toplevel',`mon-error-gather', `mon-error-gather-peek',
`mon-error-string-err-format', `*mon-emacs-help-errors*',
`mon-help-errors'.\n►►►"
  ;; (fn [&keys fun-name <STRING> locus <STRING> got-val <SYMBOL-NAME> w-error <BOOLEAN>])
  (and (or (mon-string-not-null-nor-zerop got-val)
           (mon-format :w-fun #'error
                       :w-spec '(":FUNCTION `mon-symbol-void-ERROR' "
                                 "-- value of keyword `:locus` does not satisfy `mon-string-not-null-nor-zerop'")))
       (mon-format :w-fun (and w-error #'error)
                   :w-spec `(,(or (and fun-name ":FUNCTION `%s' -- ") "%s") 
                             ,(or (and locus "value of arg %s void or not interned in obarray, ")
                                  "%svalue of arg %s void or not interned in obarray, ")
                             "got: %S")
                   :w-args `(,(or fun-name "")
                             ,(or (and locus (or (and (stringp locus) (upcase locus)) locus)) "")
                             ,got-val))))

;;; ==============================
;;; :CHANGESET 2370
;;; :CREATED <Timestamp: #{2010-12-31T15:38:45-05:00Z}#{10525} - by MON KEY>
(defun* mon-vectorp-ERROR (&key fun-name locus got-val w-error)
  "Format and maybe signal error for failed `vectorp' constraint.\n
Keyword :FUN-NAME is the function orginating the error.\n
Keyword :LOCUS is a symbol naming the paramter which can not be satisfied.\n
Keyword :GOT-VAL is a local symbol or value naming an object which isn't a vector.\n
Keyword :W-ERROR when non-nil will pass generated format string to error.\n
:EXAMPLE\n\n\(let \(\(empty-obarray \"\"\)\)
  \(or \(vectorp empty-obarray\)
      \(mon-vectorp-ERROR :fun-name \"ttesting\" 
                         :locus    \"ttesting-arg\"  
                         :got-val  empty-obarray 
                         :w-error  t\)\)\)\n
\(let \(\(empty-obarray \(make-bool-vector 3 t\)\)\)
  \(or \(vectorp empty-obarray\)
      \(mon-vectorp-ERROR :fun-name \"ttesting\" 
                         :locus    \"ttesting-arg\"  
                         :got-val  empty-obarray\)\)\)\n
:SEE-ALSO `mon-string-not-null-nor-zerop-ERROR', `mon-vectorp-ERROR',
`mon-file-non-existent-ERROR', `mon-buffer-non-existent-ERROR', `assert',
`mon-format', `mon-message', `mon-error',
`mon-error-toplevel',`mon-error-gather', `mon-error-gather-peek',
`mon-error-string-err-format', `*mon-emacs-help-errors*',
`mon-help-errors'.\n►►►"
  ;; (fn [&keys fun-name <STRING> locus <STRING> got-val <OBJECT> w-error <BOOLEAN>])
  (mon-format :w-fun (and w-error #'error)
              :w-spec `(,(or (and fun-name ":FUNCTION `%s' -- ") "%s") 
                        ,(or (and locus "arg %s does not satisfy ")
                             "%sCan not satisfy ")
                        "`vectorp'"
                        ,(or (and got-val ", got: %S type-of: %s") "%s"))
              :w-args `(,(or fun-name "")
                        ,(or (and locus (upcase locus)) "")
                        ,@(or (and got-val `(,got-val ,(type-of got-val)))
                              `("")))))

;;; ==============================
;;; :CHANGESET 2389
;;; :CREATED <Timestamp: #{2011-01-12T19:56:11-05:00Z}#{11023} - by MON KEY>
(defun* mon-file-non-existent-ERROR (&key fun-name locus got-val w-error)
  "Format and maybe signal error for failed `file-exists-p' constraint.\n
Keyword :FUN-NAME is the function orginating the error.\n
Keyword :LOCUS is a symbol naming the paramter which can not be satisfied.\n
Keyword :GOT-VAL is a local symbol or object which isn't the file expected.\n
Keyword :W-ERROR when non-nil will pass generated format string to error.\n
:EXAMPLE\n\n\(let \(\(eg-fl \(concat default-directory \"NO-WAY-I-EXIST\"\)\)\)
  \(or \(file-exists-p eg-fl\)
      \(mon-file-non-existent-ERROR 
       :fun-name \"mon-file-older-than-file-p\"
       :locus \"eg-fl\" 
       :got-val eg-fl
       :w-error t\)\)\)\n
:SEE-ALSO `mon-file-truename-p', `file-exists-p',
`mon-buffer-name-is-file-name-p', `mon-string-not-null-nor-zerop-ERROR',
`mon-vectorp-ERROR', `assert', `mon-buffer-non-existent-ERROR', `mon-format',
`mon-message', `mon-error', `mon-error-toplevel',`mon-error-gather',
`mon-error-gather-peek', `mon-error-string-err-format',
`*mon-emacs-help-errors*', `mon-help-errors', `*mon-error-utils-xrefs*'.\n►►►"
  ;; (fn [&KEYS FUN-NAME <STRING> LOCUS <STRING> GOT-VAL <OBJECT> W-ERROR <BOOLEAN>])
  (mon-format :w-fun (and w-error #'error)
              :w-spec `(,(or (and fun-name ":FUNCTION `%s' -- ") "%s") 
                        ,(or (and locus "arg %s does not satisfy ")
                             "%sCan not satisfy ")
                        "`file-exists-p'"
                        ,(or (and got-val ",\n got: %S \n type-of: %s") "%s"))
              :w-args `(,(or fun-name "")
                        ,(or (and locus (upcase locus)) "")
                        ,@(or (and got-val `(,got-val ,(type-of got-val)))
                              `("")))))

;;; ==============================
;;; :CHANGESET 2389
;;; :CREATED <Timestamp: #{2011-01-14T12:42:04-05:00Z}#{11025} - by MON KEY>
(defun* mon-buffer-non-existent-ERROR (&key fun-name locus got-val w-error)
  "Format and maybe signal error for failed `mon-buffer-exists-p' constraint.\n
Keyword :FUN-NAME is the function orginating the error.\n
Keyword :LOCUS is a symbol naming the paramter which can not be satisfied.\n
Keyword :GOT-VAL is a local symbol or object which isn't the buffer expected.\n
Keyword :W-ERROR when non-nil will pass generated format string to error.\n
:EXAMPLE\n
\(let \(\(will-kill \"*BAD-IF-NOT-KILLED*\"\)
      kill-bfr\)
  \(prog2 \(get-buffer-create will-kill\)
      \(setq kill-bfr \(mon-buffer-exists-p \"*BAD-IF-NOT-KILLED*\"\)\)
    \(kill-buffer \(mon-buffer-exists-p will-kill\)\)\)
   \(unless \(buffer-live-p kill-bfr\)
     \(mon-buffer-non-existent-ERROR :w-error t
                                    :fun-name \"a-buffer-frobbing-fun\"
                                    :locus \"will-kill\"
                                    :got-val kill-bfr\)\)\)\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-exists-so-kill', `buffer-live-p',
`mon-buffer-name-is-file-name-p', `mon-buffer-name-print-readably',
`mon-string-not-null-nor-zerop-ERROR', `mon-vectorp-ERROR', `assert',
`mon-file-non-existent-ERROR', `mon-format', `mon-message', `mon-error',
`mon-error-toplevel',`mon-error-gather', `mon-error-gather-peek',
`mon-error-string-err-format', `*mon-emacs-help-errors*', `mon-help-errors',
`*mon-error-utils-xrefs*'.\n►►►"
  ;; (fn [&keys fun-name <STRING> locus <STRING> got-val <OBJECT> w-error <BOOLEAN>])
  (mon-format :w-fun (and w-error #'error)
              :w-spec `(,(or (and fun-name ":FUNCTION `%s' -- ") "%s") 
                        ,(or (and locus "arg %s does not satisfy ")
                             "%sCan not satisfy ")
                        "`mon-buffer-exists-p'"
                        ,(or (and got-val ",\n got: %S \n type-of: %s") "%s"))
              :w-args `(,(or fun-name "")
                        ,(or (and locus (upcase locus)) "")
                        ,@(or (and got-val `(,got-val ,(type-of got-val)))
                              `("")))))

;;; ==============================
;;; :CHANGESET 2405
;;; :CREATED <Timestamp: #{2011-01-20T17:04:16-05:00Z}#{11034} - by MON KEY>
(defun* mon-list-proper-p-ERROR (&key fun-name locus got-val w-error)
  "Format and maybe signal error for failed `mon-list-proper-p' constraint.\n
Keyword :FUN-NAME is the function orginating the error.\n
Keyword :LOCUS is a symbol naming the paramter which can not be satisfied.\n
Keyword :GOT-VAL is a local symbol or object which isn't the buffer expected.\n
Keyword :W-ERROR when non-nil will pass generated format string to error.\n
:EXAMPLE\n\n\(let \(\(funny-vals '\(\"a\" \"b\" . \"c\"\)\)\)
  \(mon-list-proper-p-ERROR :w-error t 
                           :fun-name \"funny\" 
                           :locus \"funny-vals\" 
                           :got-val funny-vals \)\)\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-exists-so-kill', `buffer-live-p',
`mon-buffer-name-is-file-name-p', `mon-buffer-name-print-readably',
`mon-string-not-null-nor-zerop-ERROR', `mon-vectorp-ERROR', `assert',
`mon-file-non-existent-ERROR', `mon-format', `mon-message', `mon-error',
`mon-error-toplevel',`mon-error-gather', `mon-error-gather-peek',
`mon-error-string-err-format', `*mon-emacs-help-errors*', `mon-help-errors',
`*mon-error-utils-xrefs*'.\n►►►"
  (mon-format :w-fun (and w-error #'error)
              :w-spec `(,(or (and fun-name ":FUNCTION `%s' -- ") "%s") 
                        ,(or (and locus "arg %s does not satisfy ")
                             "%sCan not satisfy ")
                        "`mon-list-proper-p'"
                        ,(or (and got-val ",\n got: %S \n type-of: %s") "%s"))
              :w-args `(,(or fun-name "")
                        ,(or (and locus (upcase locus)) "")
                        ,@(or (and got-val `(,got-val ,(type-of got-val)))
                              `("")))))
  

 
;;; ==============================
;;; :TODO We really need a local version of `coerce' like that in `mon-map' but
;;; which also checks `mon-sequence-mappable-p'.
;;
;; (defun mon-error-seq-length-err-format (fun-name locus-arg wanted-arg-val got-arg-val)
;;   (format (concat ":FUNCTION `%s' "
;;                   "-- arg %s is not a sequence with length: %d, got: %d")          
;;           ))
;;
;;; (defun mon-error-list-proper-p-err-format (fun-name locus-arg got-arg-val)
;;     (error (concat ":FUNCTION 'mon-elt->elt " 
;;                    "-- arg W-OLD-MAKE-NEW-LST does not satisfy 'mon-list-proper-p', got: %S")
;;            w-old-make-new-lst))
;;
;;; ==============================
;; num-predicate is one of:
;; (defun* mon-error-not-a-number-err-format (&key fun-name locus num-predicate got-val w-error) 
;; (case num-predicate
;; (wholenump  <arg>)
;; (natnump   <arg>)
;; (numberp    <arg>)
;; (integerp   <arg>)
;; (integer-or-marker-p <arg>)
;; (number-or-marker-p <arg>)
;; (floatp     <arg>)
;; (zerop      <arg>)
;; (evenp      <arg>)
;; (oddp       <arg>)
;; (minusp     <arg>)
;; (plusp      <arg>)
;; (characterp
;;;;;
;; :NOTE from `cl-make-type-test' in :FILE lisp/emacs-lisp/cl-macs.el
;; (case number-predicate 
;;  (realp    `(numberp ,val))
;;  (fixnump  `(integerp ,val))
;;
;; cl-float-limits
;; max-char
;; most-positive-fixnum
;; most-negative-fixnum
;; most-positive-float
;; most-negative-float
;; least-negative-float
;; most-positive-float
;; least-positive-normalized-float
;; least-negative-normalized-float
;;
;; (concat ":FUNCTION `%s' "-- arg END-W not a number") msi-fncn)
;;; ==============================

 
;;; ==============================
;;; :PREFIX "mtpfp-"
;;; :CREATED <Timestamp: Friday May 29, 2009 @ 07:26.02 PM - by MON>
(defun mon-truncate-path-for-prompt (&optional intrp)
  "Return a truncated path string of current buffers path.\n
Useful for passing around to helper functions that prompt.\n
:EXAMPLE\n\n(mon-truncate-path-for-prompt)\n
:ALIASED-BY `mon-dir-name-truncate-for-prompt'\n
:SEE-ALSO `mon-file-reduce-name'.\n►►►"
  (interactive "p")
  (let* ((mtpfp-pth (directory-file-name (expand-file-name "./")))
	 (mtpfp-splt (save-match-data (split-string mtpfp-pth "/")))
	 (mtpfp-len (length mtpfp-splt))
	 mtpfp-gthr)
    (setq mtpfp-gthr (cond ((>= mtpfp-len 3)(last mtpfp-splt 3))
                        ((>= mtpfp-len 2)(last mtpfp-splt 2))
                        ((>= mtpfp-len 1)(last mtpfp-splt))))
    (setq mtpfp-gthr (mapconcat #'identity mtpfp-gthr "/"))
    (if intrp (message (concat ":FUNCTION `mon-truncate-path-for-prompt' "
                               "-- truncated path: %s")
                       mtpfp-gthr) mtpfp-gthr)))
;;
;;; :TEST-ME (mon-truncate-path-for-prompt)

;;; ==============================
;;; :COURTESY slime.el :WAS `slime-display-warning'
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-02T12:04:54-04:00Z}#{10396} - by MON KEY>
(defun mon-display-warning (message &rest args)
  "Invoke `display-warning' with arg type as `mon` and message as `warning`.\n
:EXAMPLE\n\n\(mon-display-warning \(concat \":FUNCTION `mon-display-warning' \" 
                             \"-- example warning with arg `%s' and arg `%d'\"\)
                     'bubba 666\)\n
:SEE-ALSO `mon-message', `warning-series', `warning-prefix-function',
`warning-fill-prefix'.\n►►►"
  (display-warning '(mon-error-warn warning) (apply #'format message args)))

;;; ==============================
(defun mon-error-toplevel (&rest args)
  "Top level error MON error form.\n
When evaluated `signal's itself by passing a list containg ARGS and the error
string stored on the `*mon-error-gather*'.\n
The string on stack is retrieved via `mon-error-gather-finalize' which also
resets the stack with `mon-error-gather-reset'.\n
This function is called from `mon-error'.\n
:EXAMPLE\n\n
:SEE-ALSO `handler-case', `condition-case', `mon-error', `mon-error-gather',
`mon-error-gather-peek', `mon-error-string-err-format', `mon-message',
`stack-trace-on-error', `backtrace', 
`redirect-debugging-output', `external-debugging-output', `debug-on-signal',
`debug-on-error', `debug-ignored-errors', `signal-hook-function',
`*mon-emacs-help-errors*', `mon-help-errors',
`mon-help-CL-error-condition-restart', `report-errors'.\n►►►"
  (let ((emsg (mon-error-gather-finalize)))
    (signal 'mon-error-toplevel `(,emsg ,@args))))
;;
;;; :NOTE We add `t' as :FILE src/eval.c has this comment in find_handler_clause:
;;;  /* t is used by handlers for all conditions, set up by C code.  */
;;;   if (EQ (handlers, Qt))
;;
(put 'mon-error-toplevel 'error-conditions '(error mon-error-toplevel mon-error t))
(put 'mon-error-toplevel 'error-message ":MON-ERROR-TOPLEVEL")

;;; ==============================
(defun* mon-error (&key signaler function format-control format-arguments handler)
  "
Keyword :FUNCTION a symbol identifying the function originating the error.\n
Keyword :SIGNALER a symbol designating the error type specific to the problem.
Default is `mon-error'.\n
Keyword :HANDLER a symbol, identifying the a preferred function to invoke when
attempting recovery from the error.\n
Keyword :FORMAT-CONTROL a string suitable for use as a format spec to construct
an error message. It is applied with FORMAT-ARGUMENTS as its arguments.\n
Keyword :FORMAT-ARGUMENTS a list it is applied as both the:\n
 - arguments applicable to the format spec specified by keyword FORMAT-ARGUMENTS;\n
 - local symbol applicable as the VAR in a `condition-case' condition form;\n
:EXAMPLE\n\n\(let \(\(not-a-nmbr \"| local string val not 88 |\"\)\)
  \(condition-case err
      \(unless \(numberp not-a-nmbr\)
        \(mon-error  :signaler 'mon-error
                    :function 'test-fun 
                    :format-control \"arg not numberp, got: %S -- wanted: `%s'\"
                    :format-arguments `\(,not-a-nmbr 88 i-am-an-arg-for-handler\)
                    :handler 'imaginary-hndlr\)\)
    \(mon-error-toplevel err\)\)\)\n
:SEE-ALSO `handler-case', `condition-case', `mon-error-toplevel' `mon-error',
`mon-error-gather', `mon-error-gather-peek', `mon-error-gather-finalize',
`mon-error-gather-reset', `*mon-error-gather*', `mon-error-string-err-format',
`mon-message', `mon-help-errors', `*mon-emacs-help-errors*',
`mon-help-CL-error-condition-restart', `report-errors'.\n►►►"
  (let* ( ;;(jstfy (propertize "\t" 'display '(space :align-to 32)))
         (jstfy (make-string  21 32)) ;; <- (eq 21 (length "(mon-error-toplevel \""))
         (emsg (concat (format ":SIGNALER `%s'" signaler)
                       "\n" jstfy (format ":FUNCTION `%s' -- " function)
                       (cond ((and format-control format-arguments)
                              (apply 'format format-control format-arguments))
                             ((and format-control (not format-arguments))
                              format-control)
                             ((and (not format-control) format-arguments)
                              (format "applicaple args: %S" format-arguments)))
                       (or (and handler (format (concat "\n" jstfy ":HANDLER  `%s'") handler))
                           (format (concat "\n" jstfy ":HANDLER  `no-applicable-handler'"))))))
    (princ emsg 'mon-error-gather)
    (mon-error-toplevel `(:FUNCTION ,function
                          :SIGNALER ,signaler 
                          :HANDLER (,(or handler 'no-applicable-handler) ,@format-arguments))))) 
                                    
;;
(put 'mon-error 'error-conditions '(error mon-error-toplevel mon-error))
(put 'mon-error 'error-message ":MON-ERROR-MESSAGE")
;; (put 'no-applicable-handler 'error-conditions 
;;       '(error mon-error-toplevel mon-error no-applicable-handler))
;; (put 'no-applicable-handler 'error-message ":MON-ERROR handler unspecifed or not applicaple")
;;
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (let ((not-a-nmbr "| local string val not 88 |"))
;; |   (condition-case err
;; |       (unless (numberp not-a-nmbr)
;; |         (mon-error  :signaler 'mon-error
;; |                     :function 'test-fun 
;; |                     :format-control "arg not numberp, got: %S -- wanted: `%s'"
;; |                     :format-arguments `(,not-a-nmbr 88 i-am-an-arg-for-handler)
;; |                     :handler 'imaginary-hndlr))
;; |     (mon-error-toplevel err)))
;; `----

 
;;; ==============================
(defun mon-error-string-err (function error-locus handler)
  "
:EXAMPLE\n\n
:SEE-ALSO `mon-error-string-err-format', `mon-message'.\n►►►"
  (mon-error :signaler 'mon-error-string 
             :function function
             :format-control "arg %s does not satisfy `stringp'"
             :format-arguments `(,error-locus)
             :handler handler))
;;
(put 'mon-error-string 'error-conditions '(mon-error-toplevel))

;;; ==============================
;; (defun mon-error-string-fun-check-and-format (fun-name)
;; (let 
;; (format (concat ":FUNCTION `%s' "
;;         (or 
;;          ;; :NOTE This should intern-soft the string symbol, then check the value with `mon-function-object-p'
;;          ;; (and (stringp fun-name) (mon-function-object-p (intern-soft fun-name obarray))
;;          (and (stringp fun-name) fun-name)
;;          (and (symbolp fun-name) (fboundp fun-name) (symbol-name fun-name))
;;          (let ((mesef-LT-1 `(mon-error-string-err-format fun-name ,fun-name)))
;;            (error (apply #'mon-error-string-err-format mesef-LT-1)))))

 
;;; ==============================
;;; :PREFIX "mesef-" 
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-07T12:35:40-04:00Z}#{10404} - by MON KEY>
(defun mon-error-string-err-format (fun-name locus-arg got-arg-val
                                        &optional w-error-signaled)
  "Return formatted `mon-error-*' string for an arg not satisfying `stringp'.\n
FUN-NAME is the function name signaling.`n
LOCUS-ARG is the args local symbol name quoting rules apply.\n
It is converted to a string and `upcase'd for format.\n
GOT-ARG-VAL is the argument value. It is passed to evaluate to its value.\n
When optional arg W-ERROR-SIGNALED is non-nil signal an error with generated
error-string.\n
:EXAMPLE\n\n\(mon-error-string-err-format 'subrp 'bubba-arg 'bubba-val\)\n
\(mon-error-string-err-format \"subrp\" 'bubba-arg 'bubba-val\)\n
\(mon-error-string-err-format 'subrp 'some-arg 88\)\n
\(mon-error-string-err-format 'subrp \"some-arg\" 88\)\n
\(mon-error-string-err-format \"subrp\" 'some-arg 88\)\n
\(mon-error-string-err-format \"subrp\" \"some-arg\" 88 t\)\n
;; :NOTE Following should all fail successfully\n
\(mon-error-string-err-format 'non-existent-function 'bubba-arg 'bubba-val\)\n
;; These fail with 88, 8.8 and not-a-fnnc. Never see 2nd error around 'bubba
\(mon-error-string-err-format 'subrp 88 'bubba\)\n
\(mon-error-string-err-format 'subrp 8.8 'bubba\)\n
\(mon-error-string-err-format 'not-a-fncn 88 'bubba\)\n
:SEE-ALSO `mon-error', `mon-error-toplevel',`mon-error-gather',
`mon-error-gather-peek', `mon-error-string-err-format', `mon-message',
`assert', `with-output-to-string', `report-errors',
`*mon-emacs-help-errors*', `mon-help-errors', 
`mon-help-CL-error-condition-restart'.\n►►►"
  ;; :NOTE As this is an error signalling routine we should assume that the
  ;; args are not always what we expect.
  (mon-format 
   :w-fun  (and w-error-signaled #'error) ;; Else defaults to #'format
   :w-spec '(":FUNCTION `%s' " "-- arg %s does not satisfy `stringp', got: %S")
   :w-args `(,(or 
               ;; :NOTE This should intern-soft the string symbol and then
               ;; check the value with `mon-function-object-p'
               (and (stringp fun-name) fun-name) 
               (and (symbolp fun-name) (fboundp fun-name) (symbol-name fun-name))
               (let ((mesef-LT-1 `(mon-error-string-err-format fun-name ,fun-name)))
                 (error (apply #'mon-error-string-err-format mesef-LT-1))))
             ,(or (and (stringp locus-arg) (upcase locus-arg))
                  (and (atom locus-arg)
                       (or (and (not (null locus-arg))
                                (not (cadr (mon-booleanp locus-arg)))
                                (not (number-or-marker-p locus-arg)) ;; (type-of (current-buffer))
                                (and (symbolp locus-arg) (upcase (symbol-name locus-arg))))
                           (let ((mesef-LT-2 `(,fun-name ,(format "%S" locus-arg) ,locus-arg)))
                             (error (apply #'mon-error-string-err-format mesef-LT-2))))))
             ,got-arg-val)))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (stringp (mon-error-string-err-format 'subrp 'bubba-arg 'bubba-val))
;; | (stringp (mon-error-string-err-format "subrp" 'bubba-arg 'bubba-val))
;; | (stringp (mon-error-string-err-format 'subrp 'some-arg 88))
;; | (stringp (mon-error-string-err-format 'subrp "some-arg" 88))
;; | (stringp (mon-error-string-err-format "subrp" 'some-arg 88))
;; | (not (ignore-errors (mon-error-string-err-format 'subrp 'bubba-arg 'bubba-val t)))
;; | (not (ignore-errors (mon-error-string-err-format "subrp" 'bubba-arg 'bubba-val t)))
;; | (not (ignore-errors (mon-error-string-err-format 'subrp 'some-arg 88 t)))
;; | (not (ignore-errors (mon-error-string-err-format 'subrp "some-arg" 88 t)))
;; | (not (ignore-errors (mon-error-string-err-format "subrp" "some-arg" 88 t)))
;; | ;; Following successfully fail:
;; | (not (ignore-errors (mon-error-string-err-format 'subrp 88 'bubba-val)))
;; | (not (ignore-errors (mon-error-string-err-format 'subrp 8.8 'bubba-val)))
;; | (not (ignore-errors (mon-error-string-err-format 'non-existent-function 'bubba-arg 'bubba-val)))
;; | ;; :NOTE This one never sees the second error.
;; | (not (ignore-errors (mon-error-string-err-format 'non-existent-function 88 'bubba-val)))
;; `----

 
;;; ==============================
;;; :CHANGESET 2141
;;; :CREATED <Timestamp: #{2010-09-17T19:18:29-04:00Z}#{10375} - by MON KEY>
(defun mon-error-gather (arg)
  "Push ARG \(a char\) onto current `*mon-error-gather*'s stack.\n
Called as function arg to `princ's PRINTCHARFUN to accumulate multiple error
messages.\n
:EXAMPLE\n\n\(progn
  \(mon-error-gather-reset\)
  \(princ \"I'm on the `*mon-error-gather*' stack\" 'mon-error-gather\)
 \(mon-error-gather-finalize\)\)\n
:SEE-ALSO `with-output-to-string', `mon-error-toplevel' `mon-error',
`mon-error-gather-peek', `mon-error-gather-finalize', `mon-error-gather-reset',
`*mon-error-gather*', `*mon-help-emacs-errors*', `mon-help-errors',
`mon-help-CL-error-condition-restart', `report-errors'.\n►►►"
    (push arg *mon-error-gather*))

;;; ==============================
;;; :CHANGESET 2141
;;; :CREATED <Timestamp: #{2010-09-17T19:18:26-04:00Z}#{10375} - by MON KEY>
(defun mon-error-gather-peek ()
  "Peek at the current errors on the `*mon-error-gather*'s stack.\n
When stack is empty return the empty string.\n
:EXAMPLE\n\n\(unwind-protect
    \(progn ;; :NOTE `and' guards echoing via `princ'.
      \(and \(princ \"bubba\" 'mon-error-gather\) 
           \(princ \"more bubba\" 'mon-error-gather\) t\)
      ;; Peak at current stack
      \(momentary-string-display \(mon-error-gather-peek\) \(point\)\)\)
  ;; Reset the stack to nil
  \(mon-error-gather-reset\)\)\n
\(mon-error-gather-peek\)
:SEE-ALSO `mon-error-toplevel' `mon-error', `mon-error-gather',
`mon-error-gather-finalize', `mon-error-gather-reset', `mon-help-errors',
`*mon-help-emacs-errors*', `mon-help-CL-error-condition-restart',
`report-errors'.\n►►►"
  (concat (reverse *mon-error-gather*) nil))

;;; ==============================
;;; :CHANGESET 2141
;;; :CREATED <Timestamp: #{2010-09-17T19:18:33-04:00Z}#{10375} - by MON KEY>
(defun mon-error-gather-reset ()
  "Reset the `*mon-error-gather*' errors stack.\n
:EXAMPLE\n\n\(progn
  \(princ \"bubba on stack\" 'mon-error-gather\)
  \(mon-error-gather-reset\)
  \(mon-error-gather-peek\)\)\n
:SEE-ALSO `mon-error-toplevel' `mon-error', `mon-error-gather',
`mon-error-gather-peek', `mon-error-gather-finalize', `*mon-help-emacs-errors*',
`mon-help-errors', `mon-help-CL-error-condition-restart', `report-errors'.\n►►►"
  (setq *mon-error-gather* nil))

;;; ==============================
;;; :CHANGESET 2141
;;; :CREATED <Timestamp: #{2010-09-17T19:18:37-04:00Z}#{10375} - by MON KEY>
(defun mon-error-gather-finalize ()
  "Return finalized string on the `*mon-error-gather*'s stack.\n
Reset it with `mon-error-gather-reset'.\n
:EXAMPLE\n\n\(progn 
  \(princ \"bubba\" 'mon-error-gather\)
  \(momentary-string-display \(mon-error-gather-peek\) \(point\)\)
  \(mon-error-gather-reset\)
  \(princ \" more bubbaness\" 'mon-error-gather\)
  \(momentary-string-display \(mon-error-gather-peek\) \(point\)\)
  \(princ \" done now\" 'mon-error-gather\)
  \(momentary-string-display \(mon-error-gather-finalize\) \(point\)\)
  \(null *mon-error-gather*\)\)\n
:SEE-ALSO `mon-error-toplevel' `mon-error', `mon-error-gather',
`mon-error-gather-peek', `*mon-help-emacs-errors*', `mon-help-errors',
`mon-help-CL-error-condition-restart', `report-errors'.\n►►►"
  (prog1 (concat (nreverse *mon-error-gather*) nil)
    (mon-error-gather-reset)))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (progn 
;; |   (princ "bubba" 'mon-error-gather)
;; |   (momentary-string-display (mon-error-gather-peek) (point))
;; |   (mon-error-gather-reset)
;; |   (princ " more bubbaness" 'mon-error-gather)
;; |   (momentary-string-display (mon-error-gather-peek) (point))
;; |   (princ " done now" 'mon-error-gather)
;; |   (momentary-string-display (mon-error-gather-finalize) (point))
;; |   (null *mon-error-gather*))
;; |
;; | (dolist (pc '("aisis " "sisis " "sisoso " "isoso ") 
;; |             (mon-error-gather-finalize))
;; |   (princ pc 'mon-error-gather))
;; | 
;; `----

 
;;; ==============================
;;; :NOTE Following establishes a lexical environment for `mon-write-string'
;;; helper functions.
;;; :CHANGESET 2195
;;; :CREATED <Timestamp: #{2010-10-18T15:02:34-04:00Z}#{10421} - by MON KEY>
(eval-when (compile load eval)
  (lexical-let (mws-gthr)
    ;;
    (defun %mon-write-string-pusher (%mwsp-chr) ;; %mwsp-chr
      "Helper function for `mon-write-string'.\n
Arg %MWSP-CHR evaluated in a lexical environment which captures/accesses a
string-stack state as passed by `%mon-write-string-writer' %MWSP-CHR arg.
`nconc's %MWSP-CHR onto the stack.\n
:EXAMPLE\n\n\(indirect-function '%mon-write-string-pusher\)\n
:SEE-ALSO `%mon-write-string-mapper', `%mon-write-string-reader',
`%mon-write-string-writer', `%mon-write-string-reset',
`%mon-write-string-pusher', `mon-write-string-reset-bind-TEST'.\n►►►"
      (funcall #'(lambda (%mwsp-L-1)
                   ;; :NOTE Not sure which is better/faster: 
                   ;;   concat/char-to-string or nconc/list
                   ;; (setq mws-gthr (concat mws-gthr (char-to-string psh-chr))))
                   (setq mws-gthr (nconc mws-gthr (list %mwsp-L-1))))
               %mwsp-chr))
    ;;
    (defun %mon-write-string-writer (%mwsw-chr)
      "Helper function for `mon-write-string'.\n
Evaluated in a lexical environment which captures/accesses a string-stack state
arg %MWSW-CHR a character is passed to `write-char' with
`%mon-write-string-pusher' as its PRINTCHARFUN arg.\n
:EXAMPLE\n\n\(indirect-function '%mon-write-string-writer\)\n
:SEE-ALSO `%mon-write-string-mapper', `%mon-write-string-reader',
`%mon-write-string-writer', `%mon-write-string-reset',
`%mon-write-string-pusher', `mon-write-string-reset-bind-TEST'.\n►►►"
      (funcall #'(lambda (%mwsw-L-1)
                   (write-char %mwsw-L-1 #'%mon-write-string-pusher))
               %mwsw-chr))
    ;;
    (defun %mon-write-string-reader ()
      "Helper function for `mon-write-string'.\n
Evaluated in a lexical environment which captures/accesses a string-stack state
:EXAMPLE\n\n(indirect-function '%mon-write-string-reader)
:SEE-ALSO `%mon-write-string-mapper', `%mon-write-string-reader',
`%mon-write-string-writer', `%mon-write-string-reset',
`%mon-write-string-pusher', `mon-write-string-reset-bind-TEST'.\n►►►"
      (funcall  #'(lambda () 
                    (concat mws-gthr))))
    ;;
    (defun %mon-write-string-mapper (%mwsm-bag)  ;; 
      "Helper function for `mon-write-string' key :W-STRING.\n
Arg %MWSM-BAG is a string the characters to map with `%mon-write-string-writer'.\n
Evaluated in a lexical environment which captures/accesses a string-stack state.\n
:EXAMPLE\n\n(indirect-function '%mon-write-string-mapper)\n
:SEE-ALSO `%mon-write-string-mapper', `%mon-write-string-reader',
`%mon-write-string-writer', `%mon-write-string-reset',
`%mon-write-string-pusher', `mon-write-string-reset-bind-TEST'.\n►►►"
      (funcall #'(lambda (%mwsm-L-1)
                   (mapc #'%mon-write-string-writer (vconcat %mwsm-L-1))
                   %mwsm-bag)
               %mwsm-bag))
    ;; CL `clear-output'-ish
    (defun %mon-write-string-reset ()
      "Helper function for `mon-write-string'.\n
Arg A-NOOP is ignored but must be passed.\n
Evaluated in a lexical environment which captures/accesses a string-stack
state resets the value of the currrently captured string-stack state.\n
:EXAMPLE\n\n\(indirect-function '%mon-write-string-mapper\)\n
:SEE-ALSO `%mon-write-string-mapper', `%mon-write-string-reader',
`%mon-write-string-writer', `%mon-write-string-reset',
`%mon-write-string-pusher', `mon-write-string-reset-bind-TEST'.\n►►►"
      (funcall #'(lambda ()
                   (prog1 (concat mws-gthr)
                     (setq mws-gthr nil)))))
    t))
;;
;; (dolist (un '("%mon-write-string-pusher" 
;;               "%mon-write-string-reset" 
;;               "%mon-write-string-writer" 
;;               "%mon-write-string-reader" 
;;               "%mon-write-string-mapper"))
;;   (unintern un obarray))

 
;;; ==============================
;;; :TODO Add support for :start :end indexes into string as per CL's `write-string'
;;; :CHANGESET 2195
;;; :CREATED <Timestamp: #{2010-10-18T21:01:35-04:00Z}#{10421} - by MON KEY>
(defun* mon-write-string (&key w-string read-current reset-null reset-bind)
  "Capture/access lexical environment value of current string-stack state.\n
When keyword W-STRING is non-nil it is a string to push onto the current string
stack by passing W-STRING to `%mon-write-string-mapper'.\n
When keyword READ-CURRENT is non-nil return value of the current string stack as
per `%mon-write-string-reader'.\n
When keyword RESET-NULL is non-nil reset string stack to the empty string and
return value of the current string stack as if by `%mon-write-string-reset'.\n
When keyword RESET-BIND is non-nil reset the current string stack to the empty
string, but only after dumping it somewhere.\n
RESET-BIND is a symbol, buffer, marker, or function with output is as per the
specification for `standard-output' \(which see\).\n
When it is a symbol, set symbol to the value of current string stack and return
a cons with the format:\n\n ( <STRING> . <SYMBOL> )\n
When it is a buffer output the contents of string stack to that buffer by
mapping the string string elts as if by `write-char' with output inserted in
buffer before point and return a cons with the format:\n
 \(buffer . #<BUFFER-OBJECT> \)\n
When it is a marker, output to marker loacation \(advances marker\) and return a
cons with the format:\n\n \(marker . #<MARKER-OBJECT> \)\n
When it is a function of one argument, call it for each char and return a cons
with the format:\n\n \( <FUNCTION-TYPE> . <FUNCTION-NAME> \)\n\n
When it is any other type, return value of current string stack inside a cons
with the format:\n\n \( <STRING> . other\)\n
:EXAMPLE\n\n\(progn 
  \(mon-write-string :reset-null t\)
  \(dotimes \(i 12 \(mon-write-string :read-current t\)\)
    \(mon-write-string :w-string 
                      \(format \(concat \"%\" \(number-to-string i\) \"d\"\) i\)\)\)\)\n
\(let \(\(bots  '\(\"bubba \" \"on \" \"the \" \"stack \"\)\)\)
  \(mon-write-string :reset-null t\)
  \(dotimes \(i \(length bots\) 
              \(prog1
                  \(mon-write-string :read-current t\)
                \(mon-write-string :reset-null t\)\)\)
    \(mon-write-string :w-string \(pop bots\)\)\)\)\n
\(progn 
  \(mon-write-string :reset-null t\)
  \(mon-write-string :w-string \"bubba got nullified\"\)
  `\(:reset-returned ,\(mon-write-string :reset-null t\)
    :stack-length-zerop ,\(eq \(length \(mon-write-string :read-current t\)\) 0\)\)\)\n
\(with-temp-buffer
  \(mon-write-string :reset-null t\)
  \(mon-write-string :w-string \"bubba with a marker\"\)
  `\(,\(mon-write-string :reset-bind
                       \(set-marker \(make-marker\) \(point\) \(current-buffer\)\)\)
    ,\(buffer-string\)\)\)\n
\(mon-write-string-reset-bind-TEST\)\n
:ALIASED-BY `write-string'\n
:SEE-ALSO `with-output-to-string'.\n►►►"
  (cond (w-string      
         (unless (stringp w-string)
           (mon-error-string-err-format "mon-write-string" ":w-string" w-string t))
         (%mon-write-string-mapper w-string))
        (read-current  (%mon-write-string-reader))
        (reset-bind    
         ;; Check for type:
         ;; (mon-write-string :reset-bind (current-buffer))  ;=> buffer
         ;; (mon-write-string :reset-bind (make-marker))     ;=> marker
         ;; (mon-write-string :reset-bind 'mon-write-string) ;=> function
         ;; (mon-write-string :reset-bind 'quoted-symbol)    ;=> symbol
         ;; (mon-write-string :reset-bind 8)                 ;=> other
         ;; (mon-write-string :reset-bind "string")          ;=> other
         ;; (mon-write-string :reset-bind '(co . ns))        ;=> other
         ;; :NOTE See the todo above about the need for a better `coerce'...
         (let (mws-chk-typ)
           (setq mws-chk-typ 
                 (or (car (memq (type-of reset-bind) '(buffer marker)))
                     (mon-equality-or-predicate #'(lambda (mws-L-2 mws-L-3) ;ignore second arg
                                               (and (not (or (characterp mws-L-2)
                                                             (consp mws-L-2)))
                                                    (car (memq (mon-function-object-p mws-L-2)
                                                               '(function subr macro autoload 
                                                                          compiled-function)))))
                                           reset-bind t)
                     (and (symbolp reset-bind) (not (string-or-null-p reset-bind)) 'symbol)
                     'other))
           (cond ((memq mws-chk-typ '(buffer marker function subr macro
                                      autoload compiled-function))
                  (mapc #'(lambda (mws-L-1)
                            (write-char mws-L-1 reset-bind))
                        (%mon-write-string-reader))
                  (%mon-write-string-reset)
                  `(,mws-chk-typ . ,reset-bind))
                 ((and (symbolp reset-bind) (not (string-or-null-p reset-bind)))
                  (progn
                    (set reset-bind (%mon-write-string-reader))
                    (%mon-write-string-reset)
                    `(,(symbol-value reset-bind) . ,reset-bind)))
                 (t `(,(%mon-write-string-reset) . ,mws-chk-typ)))))
        (reset-null    (%mon-write-string-reset))))

;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |
;; | (mon-write-string-reset-bind-TEST)
;; |
;; | (progn 
;; |   (mon-write-string :reset-null t)
;; |   (dotimes (i 4)
;; |     (mon-write-string :w-string "bubba"))
;; |   (equal (mon-write-string :read-current t)
;; |               "buabbbuabbubbbaubbba"))
;; | (equal
;; |  (progn 
;; |    (mon-write-string :reset-null t)
;; |    (dotimes (i 12 (mon-write-string :read-current t))
;; |      (mon-write-string :w-string 
;; |                        (format (concat "%" (number-to-string i) "d") i))))
;; |  "01 2  3   4    5     6      7       8        9        10         11")
;; | 
;; | (mon-write-string :read-current t)
;; | (mon-write-string :reset-bind 'some-var)
;; | (mon-write-string :reset-bind 8)
;; | 
;; | (progn (mon-write-string :reset-null t)
;; |        (mon-write-string :w-string "bubba")
;; |        (mon-write-string :reset-bind 
;; |                          (get-buffer-create "*MON-WRITE-STRING-TEST*")))
;; | (let ((some-var (make-marker)))
;; |   (mon-write-string :reset-null t)
;; |   (mon-write-string :w-string "bubba")
;; |   (mon-write-string :reset-bind
;; |                     (set-marker some-var (point) 
;; |                                 (get-buffer-create "*MON-WRITE-STRING-TEST*"))))
;; | (equal 
;; |  (progn 
;; |    (mon-write-string :reset-null t)
;; |    (mon-write-string :w-string "bubba ")
;; |    (mon-write-string :w-string "on ")
;; |    (mon-write-string :w-string "the ")
;; |    (mon-write-string :w-string "stack")
;; |    (prog1
;; |        (mon-write-string :read-current t)
;; |      (mon-write-string :reset-null t)))
;; |  "bubba on the stack")
;; | 
;; | (equal
;; |  (format "%S"
;; |          (with-temp-buffer
;; |   (mon-write-string :reset-null t)
;; |   (mon-write-string :w-string "bubba")
;; |   `(,(mon-write-string :reset-bind
;; |                        (set-marker (make-marker) (point) (current-buffer)))
;; |     ,(buffer-string))))
;; |  "((marker . #<marker in no buffer>) \"bubba\")")
;; |
;; | (equal
;; |  (progn 
;; |    (mon-write-string :reset-null t)
;; |    (mon-write-string :w-string "bubba")
;; |    `(:reset-returned ,(mon-write-string :reset-null t)  
;; |                      :stack-length-zerop ,(eq (length (mon-write-string :read-current t)) 0)))
;; |  '(:reset-returned "bubba" :stack-length-zerop t))
;; |
;; `----

 
;;; ==============================
;;; :CHANGESET 1980
;;; :CREATED <Timestamp: #{2010-07-16T12:57:27-04:00Z}#{10285} - by MON KEY>
;; (defun mon-error-format (symbol-type symbol-name descr-spec &rest fmt-args)
;;   "Return a formatted error string.\n
;; SYMBOL-TYPE is the type of symbol to report about it is one of:
;;  function macro variable constant\n
;; SYMBOL-NAME is the name of the symbol for report.\n
;; DESCR-SPEC format spec string as per format.\n
;; FMT-ARGS any number of individual arguments to format DESCR-SPEC string.\n
;; Return value is a string with the form:
;;  \":<SYMBOL-TYPE> <SYMBOL-NAME> \\n
;;   -- <DESCR-SPEC>\"
;; "
;;   (let* ((styp (case symbol-type
;;                  (function ":FUNCTION")
;;                  (macro ":MACRO")
;;                  (variable ":VARIABLE")
;;                  (constant ":CONSTANT")
;;                  ;;(face ":FACE "))
;;                  ;;(theme ":THEME "))
;;                  ))
;;          (synm (format "`%s'" symbol-name))
;;          (rtn-fmt
;;           (apply 'format (concat styp " " synm " \n-- " descr-spec) fmt-args)))
;;     (signal 'error (list rtn-fmt))))
;; 
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (mon-error-format 'function 'mon-error-format 
;; |                   "has signalled an error with FMT-ARGS %d and %d" 5 5)
;; | 
;; | (mon-error-format 'function 'mon-error-format 
;; |                   "has signalled an error with FMT-ARGS")
;; | 
;; | (mon-error-format 'variable 'mon-error-format 
;; |                   " signalled an error with FMT-ARGS %d and %d" 5 5)
;; | 
;; | (mon-error-format 'constant 'mon-error-format 
;; |                   " signalled an error with FMT-ARGS %d and %d" 5 5)
;; `----

;;; ==============================
(provide 'mon-error-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-error-utils.el ends here
;;; EOF
