;;; mon-type-utils-vars.el --- variables useful for interogating lisp objects
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-type-utils-vars.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-23T17:12:54-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-type-utils-vars provides variables useful for interogating lisp objects
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
;; `*mon-special-forms-types*', `*mon-non-mappable-object-types*',
;; `*mon-equality-or-predicate-function-types*', `*mon-function-object-types*',
;; `*mon-whitespace-chars*', `*regexp-whitespace-chars*',
;; `*mon-ascii-alpha-chars*',
;;
;; GROUPS:
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `*mon-special-forms-types*'                   <- mon-utils.el
;; `*mon-non-mappable-object-types*'             <- mon-utils.el
;; `*mon-equality-or-predicate-function-types*'  <- mon-utils.el
;; `*mon-function-object-types*'                 <- mon-utils.el
;;
;; `*mon-whitespace-chars*'                      <- mon-regexp-symbols.el
;; `*mon-whitespace-chars*'                      <- mon-regexp-symbols.el
;; `*mon-ascii-alpha-chars*'                     <- mon-regexp-symbols.el
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
;; URL: http://www.emacswiki.org/emacs/mon-type-utils-vars.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-11-25T02:27:00-05:00Z}#{10476} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-type-utils-vars. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-23T17:12:54-05:00Z}#{10472} - by MON KEY>
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

;;; ==============================
;;; :TODO add time related symbols  
;;; `timer-duration-words'

 
;;; ==============================
;;; :CHANGESET 2299
;;; :CREATED <Timestamp: #{2010-11-11T17:01:49-05:00Z}#{10454} - by MON KEY>
(defvar *mon-special-forms-types*
  '(setq quote let let* 
    and or if cond while
    progn prog1 prog2 unwind-protect catch condition-case 
    defun defmacro function defconst defvar
    setq-default
    interactive 
    save-excursion  save-restriction save-current-buffer save-window-excursion
    with-output-to-temp-buffer
    track-mouse
    ;; :NOTE The manual says that following are special-forms:
    ;;  `eval-and-compile' `eval-when-compile' `with-no-warnings'
    ;; but `describe-function' says they are defined in:
    ;; :FILE lisp/emacs-lisp/byte-run.el
    ;; eval-and-compile eval-when-compile with-no-warnings
    ;;
    ;; If elisp had them: 
    ;; `block' `return' `return-from'
    )
  "List of Emacs lisp special forms.\n
:NOTE List does not include following symbols:\n
 `eval-and-compile' `eval-when-compile' `with-no-warnings'\n
These are defined in :FILE lisp/emacs-lisp/byte-run.el\n
:SEE info node `(elisp)Special Forms'\n
:SEE-ALSO `*mon-function-object-types*',
`*mon-equality-or-predicate-function-types*', `*mon-non-mappable-object-types*',
`*mon-help-subrs*', `*mon-help-side-effect-free*',
`*mon-help-side-effect-and-error-free*', `*mon-help-pure-functions*',
`*mon-help-permanent-locals*', `*mon-function-object-types*',
`*mon-equality-or-predicate-function-types*', `*mon-non-mappable-object-types*',
`*mon-help-risky-local-variables*', `mon-help-symbol-functions'.\n►►►")

 
;;; ==============================
;;; :CHANGESET 2211
;;; :CREATED <Timestamp: #{2010-10-27T15:06:17-04:00Z}#{10433} - by MON KEY>
(defvar *mon-non-mappable-object-types* 
  '(compiled-function subr
    hash-table char-table 
    integer marker float
    buffer overlay 
    frame window window-configuration ;; :NTOE A frame-configuration is mappable
    process
    font-entity font-object font-spec
    ;; :NOTE  Don't be tempted to add `symbol` to this list! 
    ;; `type-of' returns 'symbol for the empty list: (type-of '()) 
    ;; Its `mon-sequence-mappable-p's job to check for this, so let her do it.
    ;;
    ;; :NOTE Leave `t' as last elt. It isn't mappable, and its nice to have it
    ;; as the _only_ elt when return value from `memq' e.g.:
    ;;  (memq t *mon-non-mappable-object-types*)
    ;;  (car (memq t *mon-non-mappable-object-types*))
    t)
  "List of Emacs object types which are not mappable.\n
A \"mappable\" object is one which by can occur as a SEQUENCE arge to:\n
 `mapc' `mapcar' `mapconcat'\n
:EXAMPLE\n\n\(memq 'process *mon-non-mappable-object-types*\)\n
\(not \(memq 'cons *mon-non-mappable-object-types*\)\)\n
:NOTE Objects of type `hash-table' are mappable with `maphash'.\n
:NOTE For `nil' and empty list `type-of' returns `symbol' which is why it isn't
provided by the current list.\n
:SEE info node `(elisp)Type Predicates'
:SEE info node `(elisp) Lisp Data Types'\n
:SEE-ALSO `mon-sequence-mappable-p', `mon-hash-or-mappable-p',
`mon-list-proper-p', `mon-booleanp',
`*mon-equality-or-predicate-function-types*', `*mon-function-object-types*',
`*mon-special-forms-types*', `*mon-help-emacs-errors*',
`*mon-help-side-effect-free*', `*mon-help-side-effect-and-error-free*',
`*mon-help-pure-functions*', `*mon-help-permanent-locals*',
`*mon-help-byte-optimizer-vals*', `*mon-help-risky-local-variables*',
`byte-boolean-vars', `mon-map-obarray-symbol-plist-props',
`mon-help-byte-optimizer-find'.\n►►►")

 
;;; ==============================
;;; :CHANGESET 2178
;;; :CREATED <Timestamp: #{2010-10-04T22:30:10-04:00Z}#{10401} - by MON KEY>
(defcustom *mon-equality-or-predicate-function-types* 
  '(eq eql equal 
    memq memql member
    member-ignore-case
    > < <= >= =
    assq assoc rassq rassoc
    assoc-default assoc-ignore-representation
    ;; would be nice if `compare-strings' could be here as well.
    string-equal string-lessp 
    string-match-p string-prefix-p 
    equal-including-properties
    car-less-than-car            ;; IMO this is a horrible name for a predicate.
    char-equal
    subregexp-context-p
    file-attributes-lessp
    file-newer-than-file-p
    bw-eqdir
    ;; cl
    equalp subsetp tailp typep
    ;;
    time-less-p
    timer--time-less-p
    tramp-equal-remote tramp-time-less-p    
    customize-version-lessp
    version-list-< version-list-=
    version-list-<= version< version<= version=
    face-equal internal-lisp-face-equal-p
    facemenu-color-equal    
    ;; 
    erc-port-equal
    ;;
    ediff-mark-if-equal
    ;;
    mon-file-older-than-file-p
    )
  "List of predicates or two argument predicate-like functions.\n
For use with `mon-equality-or-predicate'.\n
:EXAMPLE\n\n\(memq 'equalp *mon-equality-or-predicate-function-types*\)\n
\(not \(memq 'subrp *mon-equality-or-predicate-function-types*\)\)\n
:SEE info node `(elisp)Type Predicates'\n
:SEE-ALSO `mon-equality-for-type',`*mon-function-object-types*',
`*mon-special-forms-types*', `*mon-non-mappable-object-types*',
`*mon-help-side-effect-free*' `*mon-help-side-effect-and-error-free*',
`*mon-help-pure-functions*', `*mon-help-permanent-locals*',
`*mon-help-byte-optimizer-vals*', `*mon-help-permanent-locals*',
`*mon-help-risky-local-variables*', `*mon-help-emacs-errors*', `mon-booleanp',
`byte-boolean-vars', `mon-map-obarray-symbol-plist-props',
`mon-help-byte-optimizer-find'.\n►►►"
  :type  '(repeat symbol)
  :group 'mon-type-utils
  :group 'mon-base)


 
;;; ==============================
;;; :NOTE The list of possible return values a moving target b/c:
;;; - Improving/adjusting it
;;; - Still working out what to do w/ compiled vs. interpretted 
;;; - Pending lexbind integration might change things...
;;; :CHANGESET 2211
;;; :CREATED <Timestamp: #{2010-10-26T16:50:41-04:00Z}#{10432} - by MON KEY>
(defvar *mon-function-object-types* '(function compiled-function 
                                      subr macro lambda autoload)
  "List of return values for `mon-function-object-p' which name function objects.\n
:EXAMPLE\n\n\(memq 'function *mon-function-object-types*\)\n
\(car \(memq \(mon-function-object-p 'mon-function-object-p\) 
           *mon-function-object-types*\)\)\n
:SEE-ALSO `functionp', `indirect-function', `symbol-function', `apropos-macrop',
`edebug-macrop', `commandp', `*mon-equality-or-predicate-function-types*',
`*mon-non-mappable-object-types*', `*mon-help-emacs-errors*'.\n►►►")

 
;;; ==============================
;;; :CHANGESET 2325
;;; :CREATED <Timestamp: #{2010-11-22T14:51:38-05:00Z}#{10471} - by MON KEY>
(defvar *mon-ascii-alpha-chars* (make-vector 256 nil)
   "A 256 elt simple array with indexes for the ASCII alpha chars 65-90 and 92-122.\n
:EXAMPLE\n\n\(aref *mon-ascii-alpha-chars* ?A\)\n
\(eq \(aref *mon-ascii-alpha-chars* ?A\) ?A\)\n
\(mapcar #'\(lambda \(alpha-char\)
             \(char-to-string \(aref *mon-ascii-alpha-chars* alpha-char\)\)\)
         \(append \(number-sequence 65 90\) \(number-sequence 97 122\)\)\)\n
\(aref *mon-digit-chars* \(car *mon-digit-registers*\)\)\n
:SEE-ALSO `*mon-digit-chars*', `*mon-whitespace-chars*', `*mon-digit-registers*',
`*mon-cntl-char-registers*', `mon-help-ascii-chars'.\n►►►")
;;
(unless (eq (aref *mon-ascii-alpha-chars* ?A) ?A)
  (loop for U from ?A to ?Z ;; A-Z 65-90 
        for D from ?a to ?z ;; a-z 97-122
        do (aset *mon-ascii-alpha-chars* U U)
        (aset *mon-ascii-alpha-chars* D D)))

;; :ASCII CONTROL-CHARS
;; `(,@(number-sequence 1 8) ,@(number-sequence 11 26)) 

;;; ==============================
;;; :CHANGESET 2256
;;; :CREATED <Timestamp: #{2010-11-01T13:04:03-04:00Z}#{10441} - by MON KEY>
(defvar *mon-whitespace-chars* '(12 11 13 10 9 32) 
  "List of ASCII whitespace chars.\n
List includes:\n
 SPACE                \\x20    (32, #o40, #x20)
 CHARACTER TABULATION \\x9     ( 9, #o11, #x9)
 LINE FEED (LF)       \\xa C-j (10, #o12, #xa)
 LINE TABULATION      \xb  C-k (11, #o13, #xb)
 FORM FEED (FF)       \xc  C-l (12, #o14, #xc)
 CARRIAGE RETURN (CR) \xd      (13, #o15, #xd)\n
:EXAMPLE\n\n\(mapcar #'char-to-string *mon-whitespace-chars*\)\n
\(memq (string-to-char \"\\xb\") *mon-whitespace-chars*\)\n
:NOTE Order of list elements is specified least to most important. This provides
a handle for reductive queries which further filter their return value, e.g.:\n
 \(let \(\(some-char-val 10\) wspc-myb\)
   \(prog2 
       \(setq wspc-myb \(memq some-char-val *mon-whitespace-chars*\)\)
       \(and wspc-myb \(or \(and \(= \(car wspc-myb\) 32\) 32\)
                         \(and \(= \(car wspc-myb\) 9\) 9\)
                         \(car wspc-myb\)\)\)\)\)\n
:ALIASED-BY `*whitespace-chars*'\n
:SEE-ALSO `*mon-digit-chars*', `*mon-ascii-alpha-chars*',
`*regexp-whitespace-chars*', `mon-help-ascii-chars', `mon-spacep',
`mon-spacep-is-bol', `mon-spacep-not-bol', `mon-spacep', `mon-line-bol-is-eol',
`mon-line-next-bol-is-eol', `mon-line-previous-bol-is-eol',
`mon-spacep-is-after-eol', `mon-cln-spc-tab-eol'`mon-skip-whitespace',
`mon-cln-BIG-whitespace' `mon-cln-trail-whitespace', `mon-cln-whitespace',
`mon-insert-whitespace', `mon-kill-whitespace'.\n►►►")



;;; ==============================
(provide 'mon-type-utils-vars)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-type-utils-vars.el ends here
;;; EOF
