;;; mon-type-utils.el --- procedures for interogating lisp objects
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-type-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-22T17:09:20-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-type-utils provides procedures for interogating lisp objects
;;
;; FUNCTIONS:►►►
;; `mon-function-object-p', `mon-equality-or-predicate', `mon-booleanp',
;; `mon-booleanp-to-binary', `mon-zero-or-onep', `mon-string-or-null-and-zerop',
;; `mon-string-not-null-nor-zerop', `mon-sequence-all-booleanp',
;; `mon-list-proper-p', `mon-list-dotted-p', `mon-list-proper-and-dotted-p',
;; `mon-sequence-mappable-p', `mon-get-bit-table', `mon-coerce->char',
;; `mon-char-code', `mon-is-digit', `mon-is-letter', `mon-is-alphanum',
;; `mon-is-digit-simp', `mon-is-letter-simp', `mon-is-alphanum-simp',
;; `mon-alpha-char-p', `mon-symbol-to-string', `mon-string-to-symbol',
;; `mon-string-to-sequence', `mon-string-from-sequence',
;; `mon-hash-or-mappable-p', `mon-fractionp', `mon-xor', `mon-bitset-ternary',
;; `mon-symbol-cells-bound-p',
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
;; `*mon-bit-table*'
;;
;; GROUPS:
;;
;; ALIASED/ADVISED/SUBST'D:
;; :NOTE Aliases defined in :FILE mon-aliases.el
;;
;; <UNQUALIFIED-ALIAS>                  <PREFIX>-<NON-CORE-SYMBOL>
;; `zerop-or-one'                    -> `mon-zero-or-onep'
;; `proper-list-p'                   -> `mon-list-proper-p'
;; `string-or-null-and-zerop'        -> `mon-string-or-null-and-zerop'
;; `stringp-and-zerop-or-null'       -> `mon-string-or-null-and-zerop'
;; `string-not-null-nor-zerop'       -> `mon-string-not-null-nor-zerop'
;; `alpha-char-p'                    -> `mon-alpha-char-p'
;;
;;  <PREFIX>-<QUALIFIED>                <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-one-or-zerop'                -> `mon-zero-or-onep'
;; `mon-0-or-1-p'                    -> `mon-zero-or-onep'
;; `mon-1-or-0-p'                    -> `mon-zero-or-onep'
;; `mon-zerop-or-one'                -> `mon-zero-or-onep'
;; `mon-bit-table-bits'              -> `mon-get-bit-table'
;; `mon-byte-table-bits'             -> `mon-get-bit-table'
;; `mon-t-to-1'                      -> `mon-booleanp-to-binary'
;; `mon-true-to-one'                 -> `mon-booleanp-to-binary'
;; `mon-nil-to-0'                    -> `mon-booleanp-to-binary'
;; `mon-false-to-zero'               -> `mon-booleanp-to-binary'
;; `mon-boolean-to-binary'           -> `mon-booleanp-to-binary'
;; `mon-list-all-booleanp'           -> `mon-sequence-all-booleanp'
;; `mon-list-mappable-p'             -> `mon-seqeunce-mappable-p'
;; `mon-proper-list-p'               -> `mon-list-proper-p'
;; `mon-dotted-list-p'               -> `mon-list-dotted-p'
;; `mon-mappable-sequence-p'         -> `mon-seqeunce-mappable-p'
;; `mon-char-coerce'                 -> `mon-coerce->char'
;; `mon-is-alpha-char'               -> `mon-alpha-char-p'
;; `mon-string<-symbol'              -> `mon-symbol-to-string'
;; `mon-symbol->string'              -> `mon-symbol-to-string'
;; `mon-string-from-symbol'          -> `mon-symbol-to-string'
;; `mon-string->symbol'              -> `mon-string-to-symbol'
;; `mon-seq->string'                 -> `mon-string-from-sequence'
;; `mon-sequence-to-string'          -> `mon-string-from-sequence'
;; `mon-list-dotted-and-proper-p'    -> `mon-list-proper-and-dotted-p'
;; `mon-symbol-cells-boundp'         -> `mon-symbol-cells-bound-p'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-get-bit-table'                            <- mon-utils.el
;; `mon-function-object-p'                        <- mon-utils.el
;; `mon-equality-or-predicate'                    <- mon-utils.el
;; `mon-booleanp'                                 <- mon-utils.el
;; `mon-booleanp-to-binary'                       <- mon-utils.el
;; `mon-zero-or-onep'                             <- mon-utils.el
;; `mon-string-or-null-and-zerop'                 <- mon-utils.el
;; `mon-string-not-null-nor-zerop'                <- mon-utils.el
;; `mon-sequence-all-booleanp'                    <- mon-utils.el
;; `mon-list-proper-p'                            <- mon-utils.el
;; `mon-sequence-mappable-p'                      <- mon-utils.el
;; `mon-get-bit-table'                            <- mon-utils.el
;; `mon-char-code'        			  <- mon-utils.el
;; `mon-alpha-char-p'     			  <- mon-utils.el
;; `mon-is-digit'         			  <- mon-utils.el
;; `mon-is-letter'        			  <- mon-utils.el
;; `mon-is-alphanum'      			  <- mon-utils.el
;; `mon-is-digit-simp'    			  <- mon-utils.el
;; `mon-is-letter-simp'   			  <- mon-utils.el
;; `mon-is-alphanum-simp' 			  <- mon-utils.el
;; `mon-symbol-to-string' 			  <- mon-utils.el
;; `mon-string-to-symbol' 			  <- mon-utils.el
;; `mon-string-to-sequence'                       <- mon-utils.el
;; `mon-string-from-sequence'                     <- mon-utils.el
;; `*mon-bit-table*'                              <- mon-utils.el
;;
;; TODO:
;;
;; NOTES:
;; This is cool: ((lambda (x) (list x x)) (lambda (x) (list x x)))
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-type-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-11-25T02:27:00-05:00Z}#{10476} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-type-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-22T17:09:20-05:00Z}#{10471} - by MON KEY>
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

(require 'mon-type-utils-vars)


;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T19:08:54-05:00Z}#{11022} - by MON KEY>
(defgroup mon-type-utils nil
  "Customization group for variables and functions of :FILE mon-type-utils.el\n
:SEE-ALSO .\n►►►"
  ;; :prefix "<PREFIX>"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE" "http://www.emacswiki.org/emacs/mon-type-utils.el")
  :link '(emacs-library-link "mon-type-utils.el")
  :group 'mon-base)


;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T19:08:57-05:00Z}#{11022} - by MON KEY>
(defcustom *mon-type-utils-xrefs*
  '(mon-function-object-p mon-symbol-cells-bound-p mon-equality-or-predicate
    mon-booleanp mon-xor mon-string-or-null-and-zerop
    mon-string-not-null-nor-zerop mon-zero-or-onep mon-booleanp-to-binary
    mon-sequence-all-booleanp mon-bitset-ternary mon-list-proper-p mon-list-dotted-p
    mon-list-proper-and-dotted-p mon-sequence-mappable-p mon-hash-or-mappable-p
    mon-get-bit-table mon-fractionp mon-char-code mon-alpha-char-p mon-is-digit
    mon-is-letter mon-is-alphanum mon-is-digit-simp mon-is-letter-simp
    mon-is-alphanum-simp mon-coerce->char mon-string-to-symbol
    mon-symbol-to-string mon-string-to-sequence mon-string-from-sequence
    ;; :VARIABLES
    *mon-bit-table* *mon-type-utils-xrefs*)
  "Xrefing list of mon type/predicate symbols, functions constants, and variables.\n
The symbols contained of this list are defined in :FILE mon-type-utils.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-testme-utils-xrefs*',
`*mon-button-utils-xrefs*', `*mon-buffer-utils-xrefs*',
`*mon-line-utils-xrefs*', `*mon-plist-utils-xrefs*'
`*mon-seq-utils-xrefs*', `*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*',
`*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*', `*mon-ulan-utils-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-type-utils
  :group 'mon-xrefs)

 
;;; ==============================
;;; :NOTE :FILE lisp/help-funs.el has this:
;;; ,---- 
;;; | ;; cl-hack-byte-compiler runs, after bytecomp is loaded.
;;; | (when (and (symbolp function)
;;; |            (eq (get function 'byte-compile) 'cl-byte-compile-compiler-macro)
;;; |            (princ "This function has a compiler macro")
;;; |            (let ((lib (get function 'compiler-macro-file)))
;;; `----
;;;
;;; (let (gthr-cmplr-macs)
;;;      (setq gthr-cmplr-macs (mon-map-obarray-symbol-plist-props 'byte-compile))
;;;      (setq gthr-cmplr-macs
;;;           (mapcar #'(lambda (bcd)
;;;      	   (when (eq (get bcd 'byte-compile) 'cl-byte-compile-compiler-macro)
;;;             bcd)) 
;;; 	    gthr-cmplr-macs)))
;;;
;;; :TODO This check: `ad-advice-p' is missing.
;;; :TODO Extend w/ `deftype'
;;; :TODO Build a test function once all issues finalized.
;;; :TODO This really isn't a predicate and should be renamed
;;; `mon-function-object-maybe' and add a new function that checks if the
;;; return-value is one of: 
;;; { function subr macro autoload compiled-function }
;;; But, first must decide how to handle return value for interpreted functions
;;; and macros. See comments below.
;;; :PREFIX "mfop-"
;;; :CHANGESET 2119 <Timestamp: #{2010-09-17T21:11:32-04:00Z}#{10375} - by MON KEY>
;;; <Timestamp: #{2010-09-16T17:43:19-04:00Z}#{10374} - by MON>
(defun mon-function-object-p (fncn-sym)
  "Test if FNCN-SYM is a function object.\n
Return non-nil if FNCN-SYM object is \"function-like\" which is any of the
following object types named in the variable `*mon-function-object-types*'.\n
When FNCN-SYM is a boolean return 'boolean.\n
When FNCN-SYM is not null and none of the above return value is per `type-of'
for the `indirect-function' value of FNCN-SYM.\n
:EXAMPLE\n\n\(mon-function-object-p 'mon-function-object-p\)\n
\(mon-function-object-p 'subrp\)\n
\(mon-function-object-p 'visible-mode\)\n
\(mon-function-object-p 'define-minor-mode\)\n
\(mon-function-object-p 'goto-next-locus\)\n
\(mon-function-object-p 'deactivate-mark\)\n
\(mon-function-object-p 'handwrite\)\n
\(mon-function-object-p '\(lambda \(x\) x\)\)\n
\(mon-function-object-p #'\(lambda \(x\) x\)\)\n
\(mon-function-object-p 't\)\n
\(mon-function-object-p nil\)\n
\(mon-function-object-p 'handwrite-13pt-numlines\)\n
:NOTE Following enumartes structures of the object types which
`indirect-function' may return.\n
A compiled function:\n
 #[ \( <ARG-LIST> \) 
      <BYTE-STR> 
    [ <CONSTANTS>* ]  
      <CONST-CNT-INT>    ;; :NOTE 0 indexed
      \( <PATHNAME-FILE> . <OFFSET>\) ]\n
A compiled macro:\n
 \(macro . #[ \( <ARG-LIST> \) 
               <BYTE-STR> 
              [ <CONSTANTS>* ]  
                <CONST-CNT-INT>  ;; :NOTE 0 indexed
              \( <PATHNAME-FILE> . <OFFSET>\) ] \)\n
A subr, primitive, or special operator:\n
 #<subr `SYMBOL-NAME`>\n
An interpreted function or lambda form:\n
 \(lambda \( <ARG-LIST> \) <DOCSTR> \( <MACRO-FORM> \)\)\n
An interpreted macro:\n
 \(macro lambda \( <ARG-LIST> \) <DOCSTR> \( <MACRO-FORM> \)\)\n
An autoload symbol, note <TYPE> may be a quoted symbol either macro or keymap:\n
 \(autoload  <FILENAME> <DOCSTRING-OFFSET> <INTERACTIVE> <TYPE> \)\n 
:ALIASED-BY `mon-functionp'\n
:SEE-ALSO `mon-symbol-cells-bound-p', `apropos-macrop', `edebug-macrop',
`byte-code-function-p', `find-lisp-object-file-name', `describe-function-1',
`functionp', `commandp', `subr-name', `subr-arity', `interactive-form',
`indirect-variable', `user-variable-p', `custom-variable-p',
`edebug-lookup-function', `edebug-lambda-list-keywordp', `lambda-list-keywords',
`help-function-arglist', `ad-has-proper-definition', `ad-definition-type',
`ad-lambda-p', `ad-macro-p', `ad-compiled-p', `ad-subr-p', `ad-special-form-p',
`mon-help-function-args', `subr-arity', `debugger-special-form-p',
`debug-convert-byte-code', `backtrace', `mon-help-function-arity',
`mon-help-symbol-functions', `mon-help-byte-compile-functions'.\n►►►"
  (or ;; Its a lambda form, e.g.: 
   ;; (indirect-function (lambda (x) x)) (indirect-function #'(lambda (x) x))
   (and (consp fncn-sym) 
        (eq (car-safe (indirect-function fncn-sym t)) 'lambda)
        'lambda)
   ;; Some other thing we can't stop now want and/or if we find what we want now.
   (car (memq (type-of fncn-sym) '(;; We special case `compiled-function` here b/c
                                   ;; its print syntax is #[{...}] which signals an
                                   ;; error in the the next branch b/c there we depend
                                   ;; on FNCN-SYM being a symbol-like but not
                                   ;; necessarily an `indirect-*' symbol.
                                   compiled-function
                                   subr
                                   ;; :NOTE Could branch on this and interrogate the
                                   ;; return value of an `intern-soft' on `stringp'
                                   ;; For example, could be a string naming a
                                   ;; function object or might be `facep':
                                   string 
                                   ;; Short circuit on stuff we know we don't want:
                                   integer float 
                                   marker buffer window frame
                                   ;; That is, hash-tables don't currently hold
                                   ;; function objects. Maybe they could?
                                   hash-table 
                                   char-table bool-vector
                                   overlay process
                                   ;; :NOTE Keep an eye on these they might
                                   ;; change in future GNU Emacsen:
                                   window-configuration  frame-configuration
                                   ;; `internal-lisp-face-p', `check-face', e.g.
                                   ;; (and (null (ignore-errors (check-face <thing>))) 'face)
                                   )))
   ;; Its `nil' or `t'
   (and (cadr (mon-booleanp fncn-sym)) 'boolean)
   ;; :NOTE It is very-important when interrogating byte-code-function objects
   ;; that we don't inadverdently attempt a direct access of  #[ ... ] objects. 
   ;; Doing so may (in some circumstances) _immediately_ segfault Emacs -- 
   ;; e.g. each of following will lose _badly_:
   ;;   (type-of #[abc])  (cdr (thing .  #[abc]))
   ;; At least when:  (<= 23.2.1 (emacs-version))
   ;; :SEE bug#6835: 23.2; eval'ing `type-of' with #[abc] as arg gets a Fatal error
   ;; (URL `http://lists.gnu.org/archive/html/bug-gnu-emacs/2010-08/msg00289.html')
   (let ((mfop-lkng (and fncn-sym ;; its a something
                         (and  
                          ;; Not a list but could evaluate to nil
                          (nlistp fncn-sym)
                          ;; Not a cons but could be a symbol evaluates evaluate to nil
                          (not (consp fncn-sym)) 
                          ;; Def. not a cons but maybe could evaluate to nil                              
                          (atom fncn-sym)        
                          ;; Def. not a nil
                          (not (null fncn-sym))
                          ;; Can't do fboundp/boundp on a #[....] but need to
                          ;; know if its function like and can't make the full
                          ;; determination without also taking the
                          ;; indirect-function of an indirect-function
                          ;; :NOTE Does this finally rule out a `cyclic-function-indirection`?
                          (or (and (or (and (symbolp fncn-sym)
                                            (fboundp fncn-sym))
                                       (and (symbolp fncn-sym) 
                                            (boundp fncn-sym)))
                                   (indirect-function fncn-sym t))
                              (indirect-function fncn-sym t)))))
         mfop-cot)
     ;; :NOTE What about `advice'?
     (unless (null mfop-lkng) ;; Prob can't happen
       (when (subrp mfop-lkng) (setq mfop-cot 'subr))
       (unless mfop-cot
         (when (or (byte-code-function-p mfop-lkng)
                   ;; There is a problem here. Interpreted functions have the format: 
                   ;; (lambda {...}
                   ;; Interpreted macros have the format: 
                   ;; (macro lambda {...}
                   ;; How do/should we distinguish between what are essentially
                   ;; both lambda forms? e.g. which set is best?
                   ;; 
                   ;; { 'lambda-function/'lambda-macro 'compiled-function/'compiled-macro }
                   ;; { 'lambda-function/'lambda-macro 'compiled-lambda-function/'compiled-lambda-macro }
                   ;; { 'function-interpreted/'macro-interpreted  'compiled-function/'compiled-macro }
                   ;; { 'interpreted-function/'interpreted-macro  'compiled-function/'compiled-macro }
                   ;;
                   (eq (car-safe mfop-lkng) 'lambda))
           (setq mfop-cot 'function)))
       (unless mfop-cot 
         ;; :TODO If we interrogate the list with `mon-proper-list-p'
         ;; we should be able to tell if the object is a compiled macro or
         ;; interpreted b/c a compile macro is a cons whereas an interpreted
         ;; macro is a proper list. But, right now we return 'macro. If we check
         ;; for a difference what should we return instead 'compiled-macro and
         ;; 'interpreted-macro 
         ;; likewise could do: 
         ;; (listp (cdr-safe (indirect-function 'some-compiled-macro)))
         ;; Also, what to do about autoload macros? e.g. `apropos-macrop'
         ;; 
         (when (eq (car-safe (indirect-function mfop-lkng t)) 'macro)
           ;; (if (mon-list-proper-p (indirect-function mfop-lkng))
           (setq mfop-cot 'macro)))
       (unless mfop-cot 
         (when (eq (car-safe (indirect-function mfop-lkng t)) 'autoload)
           (setq mfop-cot 
                 (or (and (eq (nth 4 mfop-lkng) 'keymap) 'keymap)
                     (and (eq (nth 4 mfop-lkng) 'macro)  'macro)
                     'autoload)))))
     (or mfop-cot (type-of mfop-lkng)))))

 
;;; ==============================
;;; :CHANGESET 2372
;;; :CREATED <Timestamp: #{2010-12-31T16:21:21-05:00Z}#{10525} - by MON KEY>
(defun mon-symbol-cells-bound-p (sym-string &optional w-obarray)
  "Check for values of symbol-cells for symbol named by SYM-STRING.\n
SYM-STRING is a string naming a symbol. Signal an error if SYM-STRING does not satisfy
`mon-string-not-null-nor-zerop'.\n
When optional arg W-OBARRAY is non-nil check for values of symbol-cells for
symbol named by SYM-STRING in some non-standard obarray. Default is `obarray'.\n
When W-OBARRAY is non-nil it is an obarray or quoted symbol which evaluates to
one, its value should satisfy `vectorp'.  Signal an error if not.\n
When SYM-STRING is not interned, return value is a property list comprised of two
key/value pairs with the format:\n
 \(:symbol-name       <SYM-STRING>
  :symbol-obarray    { obarray | <W-OBARRAY> | <VECTOR> }\)\n
When SYM-STRING is interned return value is property list of comprised of six
key/value pairs with the format:\n
 \(:symbol-name       <SYM-STRING>
  :symbol-p          <SYMBOL>      ;; non-nil when symbol is interned
  :symbol-value-p    <BOOLEAN>     ;; t when `boundp'
  :symbol-function-p <BOOLEAN>     ;; t when `indirect-function' is non-nil
  :symbol-plist-p t  <BOOLEAN>     ;; t when `symbol-plist' is non-nil
  :symbol-obarray    { obarray | <W-OBARRAY> | <VECTOR> }\)\n
:EXAMPLE\n\n\(mon-symbol-cells-bound-p \"mon-symbol-cells-bound-p\"\)\n
\(progn
  \(intern \"tt--pl\" obarray\)
  \(setplist 'tt--pl \(mon-alphabet-as-type 'plistD->num\)\)
  \(mon-symbol-cells-bound-p \"tt--pl\"\)\)\n
\(let \(\(newob \(make-vector 17 0\)\)\)
  \(unintern \"tt--pl\" obarray\)
  \(intern \"tt--pl\" newob\)
  \(mon-symbol-cells-bound-p \"tt--pl\" 'newob\)\)\n
\(let \(\(newob \(make-vector 17 0\)\)\)
  \(unintern \"tt--pl\" obarray\)
  \(intern \"tt--pl\" newob\)
  \(mon-symbol-cells-bound-p \"tt--pl\" newob\)\)\n
\(prog1
    \(mon-symbol-cells-bound-p \"tt--pl\"\)
  \(unintern \"tt--pl\" obarray\)\)\n
\(null \(plist-get \(mon-symbol-cells-bound-p \"tt--pl\"\) :symbol-p\)\)\n
\(equal \(plist-get \(mon-symbol-cells-bound-p \"tt--pl\"\) :symbol-name\) \"tt--pl\"\)\n
;; :Following fails successfully:\n
\(mon-symbol-cells-bound-p \"mon-symbol-cells-bound-p\" 'some-non-existent-obarray\)\n
:ALIASED-BY `mon-symbol-cells-boundp'\n
:SEE-ALSO `mon-function-object-p', `symbolp', `boundp', `bound-and-true-p',
`symbol-value', `indirect-variable', `symbol-function', `indirect-function',
`symbol-plist', `obarray', `intern', `intern-soft' `make-symbol',
`mon-help-symbol-functions'.\n►►►"
  (or (mon-string-not-null-nor-zerop sym-string)
      (mon-string-not-null-nor-zerop-ERROR :w-error   t
                                           :fun-name "mon-symbol-cells-bound-p"
                                           :locus    "sym-string"
                                           :got-val  sym-string))
  (let (mscb-is-bnd 
        mscb-gthr
        (mscb-ob (or (and w-obarray 
                          (typecase w-obarray 
                            (vector (cons nil w-obarray))
                            (symbol (or 
                                     (or (and (null (boundp (bound-and-true-p w-obarray)))
                                              (mon-symbol-void-ERROR :w-error t
                                                                     :fun-name "mon-symbol-cells-bound-p"
                                                                     :locus    "w-obarray"
                                                                     :got-val  (format "%s" w-obarray))))
                                     (and (vectorp (symbol-value w-obarray))
                                          (cons (identity w-obarray) (symbol-value w-obarray)))
                                     (mon-vectorp-ERROR :w-error  t
                                                        :fun-name "mon-symbol-cells-bound-p"
                                                        :locus    "w-obarray"
                                                        :got-val  w-obarray)))
                            (t (mon-vectorp-ERROR :w-error t
                                                  :fun-name "mon-symbol-cells-bound-p"
                                                  :locus    "w-obarray"
                                                  :got-val  w-obarray))))
                     (list 'obarray))))
    (setq mscb-is-bnd (intern-soft sym-string (or (cdr mscb-ob) obarray)))
    (setq mscb-gthr 
          (nconc (list :symbol-name sym-string)
                 (and mscb-is-bnd 
                      (list :symbol-p mscb-is-bnd
                            :symbol-value-p (boundp mscb-is-bnd)
                            :symbol-function-p (and (indirect-function mscb-is-bnd t) t)
                            :symbol-plist-p 
                            (and (symbol-plist (intern-soft sym-string (or (cdr mscb-ob) obarray))) t)))
                 (list :symbol-obarray (or (and (symbolp (car mscb-ob))
                                                (not (null (car mscb-ob)))
                                                (car mscb-ob))
                                           ;; Its a raw vector object
                                           (cdr mscb-ob)))
                 mscb-gthr))))

 
;;; ==============================
;;; :PREFIX "meop-"
;;; :CHANGESET 2178
;;; :CREATED <Timestamp: #{2010-10-04T22:30:05-04:00Z}#{10401} - by MON KEY>
(defun mon-equality-or-predicate (predicate arg1 arg2)
  "Evaluate PREDICATE with ARG1 ARG2.\n
PREDICATE is function accepting two args and is either a member of
`*mon-equality-or-predicate-function-types*' or a symbol satisfying the predicate
`mon-function-object-p' including macros which are wrapped in a lambda form.\n
:EXAMPLE\n\n\(mon-equality-or-predicate 
 'memq 'eq '\(eq eql equal memq memql member\)\)\n
\(mon-equality-or-predicate 
 'member \"string\" '\(\"a\" \"b\" \"c\" \"string\"\)\)\n
\(mon-equality-or-predicate #'\(lambda \(q z\) 
                          \(and \(stringp q\) \(stringp z\)
                               \(compare-strings q 0 1 z 0 1\)\)\)
                       \"bubba\" \"babel\"\)\n
:SEE-ALSO `mon-equality-for-type', `mon-booleanp',
`mon-bitset-ternary', `mon-help-byte-optimizer-find', `byte-boolean-vars',
`mon-get-text-properties-parse-prop-val-type-ckh',
`mon-symbol-cells-bound-p', `mon-help-symbol-functions'.\n►►►"
  (let ((meop-chk-fun 
         (or (and (consp predicate)
                  (or (eq (car-safe predicate) 'lambda)
                      (error (concat ":FUNCTION `mon-equality-or-predicate' "
                                     "-- arg PREDICATE satisfies `consp' car not 'lambda "
                                     " - got: %S") predicate))
                  predicate)
             (and (symbolp predicate)
                  (memq predicate *mon-equality-or-predicate-function-types*)
                  predicate)
             (let ((meop-rly-chk (cons (car (memq (mon-function-object-p predicate)
                                             *mon-function-object-types*))
                                  predicate)))
               (and (car meop-rly-chk)
                    (or ;; if its a macro make it `funcall'/`apply'able
                     (and (eq (car meop-rly-chk) 'macro)
                          `(lambda (meop-L-1-arg1 meop-L-1-arg2)
                             (,(cdr meop-rly-chk) meop-L-1-arg1 meop-L-1-arg2)))
                     predicate))))))
    (if meop-chk-fun 
        (funcall meop-chk-fun arg1 arg2)
      (error (concat ":FUNCTION `mon-equality-or-predicate' "
                     "-- arg PREDICATE not applicable, got: %S") predicate))))

 
;;; ==============================
;;; :TODO Define a boolean type w/ `deftype'
;;; :SEE (URL `http://lists.gnu.org/archive/html/bug-gnu-emacs/2010-09/msg00488.html')
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-24T12:14:50-04:00Z}#{10385} - by MON KEY>
(defun mon-booleanp (putative-boolean)
  "Like `booleanp' but return two element list when PUTATIVE-BOOLEAN is either `t' or `nil'.\n
When PUTATIVE-BOOLEAN is any other value return nil.\n
:EXAMPLE\n\n
\(mon-booleanp \"bubba\"\)\n;=> (nil nil)\n
\(mon-booleanp t\)\n;=> \(t t\)\n
\(mon-booleanp nil\)\n;=> \(nil t\)\n
\(mon-booleanp \(\)\)\n;=> \(nil t\)\n
\(mon-booleanp '\(\)\)\n;=> \(nil t\)\n
\(mon-booleanp 't\)\n;=> ;=> \(t t\)   ; :NOTE Understands the quoted t and nil\n 
\(mon-booleanp 'nil\)\n;=> \(nil t\)\n
\(not \(eq :not-a-boolean \(unless \(cadr \(mon-booleanp nil\)\) :not-a-boolean\)\)\)\n
\(eq :not-a-boolean \(unless \(cadr \(mon-booleanp \"bubba\"\)\) :not-a-boolean\)\)\n
:NOTE Returning a two element list on success shares some similarity with
Common Lisp's multiple values which seem pertinent for special cases
like this one in that there is provision for reflection that is hard
to obtain with a uni-valued return, e.g. With `booleanp' when you ask:\n
\(let \(\(query-truth \(eq 8 3\)\)\)
     \(car \(booleanp query-truth\)\)\)\n;=> nil\n
It is hard to know if the thing being queried of was indeed a boolean.
Whereas with a two element proper list:\n
\(let \(\(query-truth \(eq 8 3\)\)\)
     \(and \(cadr \(booleanp query-truth\)\)
          \(not \(car \(booleanp query-truth\)\)\)\)\)\n
:SEE-ALSO `mon-zero-or-onep', `mon-string-or-null-and-zerop',
`mon-sequence-all-booleanp' `mon-bitset-ternary', `mon-equality-or-predicate',
`mon-symbol-cells-bound-p', `byte-boolean-vars'.\n►►►"
  ;; :WAS (or (and (eq putative-boolean t)   '(t t))
  ;;          (and (eq putative-boolean nil) '(nil t))
  ;;          '(nil nil)))
  (or (and (atom putative-boolean)
           (symbolp putative-boolean)
           (boundp putative-boolean)
           (or (and (nlistp putative-boolean)
                    (and putative-boolean)
                    (or (and (eq putative-boolean 't) '(t t))
                        (and (eq putative-boolean t)  '(t t))))
               (or (and (listp putative-boolean)
                        (not (consp putative-boolean))
                        (null putative-boolean)
                        (and (eq putative-boolean nil) '(nil t)))
                   (and (eq putative-boolean 'nil) '(nil t)))))
      '(nil nil)))


;;; ==============================
;;; :COURTESY org
;;; :CHANGESET 2369
;;; :CREATED <Timestamp: #{2010-12-20T21:32:24-05:00Z}#{10511} - by MON KEY>
(defun mon-xor (a b)
  "Exclusive or.\n
:SEE-ALSO `mon-booleanp', `mon-bitset-ternary', `logior', `logand',
`mon-help-CL-bit-byte-bool-logic'.\n►►►"
  (if a (not b) b))

;;; ==============================
;;; TODO Abstract the functionality of `completion--do-completion' and
;;; `minibuffer--bitset' to less dedicated feature.
;;; :COURTESY minibuffer.el `minibuffer--bitset' 
(defun mon-bitset-ternary (a4/0 b2/0 c1/0)
  "Return bitwise-or of A4/0 B2/0 C1/0.\n
        t   nil\n
 A4/0   4   0
 B2/0   2   0
 C1/0   1   0\n
  A4/0 B2/0 C1/0\n
 \(t    t    t\)   ;=> 7
 \(t    t    nil\) ;=> 6
 \(t    nil  t\)   ;=> 5
 \(t    nil  nil\) ;=> 4
 \(nil  t    t\)   ;=> 3
 \(nil  t    nil\) ;=> 2
 \(nil  nil  t\)   ;=> 1
 \(nil  nil  nil\) ;=> 0\n
:EXAMPLE\n\n\(mapcar #'\(lambda \(mbs\) 
              `\(#::table ,mbs 
               #::bitset ,\(apply #'mon-bitset-ternary mbs\)\)\)
           \(mon-combine '\(t nil\) '\(t nil\) '\(t nil\)\)\)\n
:NOTE See the interaction of `completion--do-completion' with
`minibuffer--bitset' for additional implementation examples.
:SEE-ALSO `mon-booleanp', `mon-xor', `logior', `logand', `mon-bit-table-bits',
`mon-bool-vector-pp', `mon-string-not-null-nor-zerop', `string-or-null-p',
`zerop', `mon-zero-or-onep', `mon-booleanp', `mon-booleanp-to-binary',
`mon-sequence-all-booleanp', `dbus-byte-array-to-string',
`dbus-string-to-byte-array', `mon-help-binary-representation',
`mon-help-CL-bit-byte-bool-logic' .\n►►►"
  (logior
   (or (and a4/0 4) 0)
   (or (and b2/0 2) 0)
   (or (and c1/0 1) 0)))

;;; ==============================
;;; :CHANGESET 2211
;;; :CREATED <Timestamp: #{2010-10-26T11:58:20-04:00Z}#{10432} - by MON KEY>
(defun mon-string-or-null-and-zerop (maybe-str-or-null-obj)
  "Return non-nil when both `string-or-null-p' and of `length' `zerop'.\n
Arg MAYBE-STR-OR-NULL-OBJ is an object to interrogate.\n
:EXAMPLE\n\n\(mon-string-or-null-and-zerop \"\"\)\n
\(mon-string-or-null-and-zerop \"longer than 0\"\)\n
\(mon-string-or-null-and-zerop nil\)\n
\(mon-string-or-null-and-zerop \(\)\)\n
\(mon-string-or-null-and-zerop 0\)\n
\(mon-string-or-null-and-zerop-TEST\)\n
:NOTE The need for this procedure is because of the following:\n
 \(and \(eq \(length nil\) \(length \"\"\)\) 
     \(apply 'string '\( 98 111 116 104  32 110 117 108 108 32
                      118  97 108 117 101  32  97 110 100 32
                       34  34  32  97 114 101  32  48  33\)\)\)\n
:ALIASED-BY `string-or-null-and-zerop'
:ALIASED-BY `stringp-and-zerop-or-null'\n
:SEE-ALSO `mon-string-not-null-nor-zerop', `string-or-null-p', `zerop',
`mon-zero-or-onep', `mon-booleanp', `mon-booleanp-to-binary',
`mon-sequence-all-booleanp', `mon-bitset-ternary'.\n►►►"
  (and (string-or-null-p maybe-str-or-null-obj)
       (zerop (length maybe-str-or-null-obj))))
;;
;;; :TEST-ME (mon-string-or-null-and-zerop-TEST)

;;; ==============================
;;; :CHANGESET 2233
;;; :CREATED <Timestamp: #{2010-11-03T11:48:37-04:00Z}#{10443} - by MON KEY>
(defun mon-string-not-null-nor-zerop (w-putative-string)
  "Return W-PUTATIVE-STRING if it is `stringp' and greater than `length' 0.\n
:EXAMPLE\n\n\(mon-string-not-null-nor-zerop \"I am a good string\"\)\n
\(equal \(mon-string-not-null-nor-zerop \"I am a good string\"\) \"I am a good string\"\)\n
\(mon-string-not-null-nor-zerop nil\)\n
\(mon-string-not-null-nor-zerop \"\"\)\n
\(mon-string-not-null-nor-zerop 'I-am-not-a-good-string\)\n
\(and \(not \(mon-string-not-null-nor-zerop 8\)\) \"got 8\"\)\n
:ALIASED-BY `string-not-null-or-zerop'\n
:SEE-ALSO `mon-string-or-null-and-zerop', `string-or-null-p', `zerop',
`mon-zero-or-onep', `mon-booleanp', `mon-booleanp-to-binary',
`mon-sequence-all-booleanp', `mon-bitset-ternary'.\n►►►"
  (and (not (mon-string-or-null-and-zerop w-putative-string))
       (stringp w-putative-string)
       w-putative-string))
;;
;;; :TEST-ME (equal (mon-string-not-null-nor-zerop "I am a good string") "I am a good string")
;;; :TEST-ME (and (not (mon-string-not-null-nor-zerop nil)) "value null")
;;; :TEST-ME (and (not (mon-string-not-null-nor-zerop "")) "empty string")
;;; :TEST-ME (and (not (mon-string-not-null-nor-zerop 'I-am-not-a-good-string)) "got symbol")

;;; ==============================
;;; :CHANGESET 2206
;;; :CREATED <Timestamp: #{2010-10-23T13:54:40-04:00Z}#{10426} - by MON KEY>
(defun mon-zero-or-onep (maybe-one-or-zero)
  "Return non-nil when arg MAYBE-ONE-OR-ZERO is and integer 0 or 1.\n
:EXAMPLE\n\n\(mon-zero-or-onep 0\)\n\n\(mon-zero-or-onep 1\)\n
\(mon-zero-or-onep #x1\)\n\n\(mon-zero-or-onep #o1\)\n
\(mon-zero-or-onep #x0\)\n\n\(mon-zero-or-onep #o0\)\n
\(mapcar  #'mon-zerop-or-one
         \(cadr \(memq :bin-table \(mon-bool-vector-pp \(make-bool-vector 8 t\)\)\)\)\)\n
\(mon-zero-or-onep \(length \(not \(not nil\)\)\)\)\n
\(mon-zero-or-onep \(with-temp-buffer \(point-min\)\)\)\n
\(not \(mon-zero-or-onep 
      \(with-temp-buffer \(insert \(make-string 8 32\)\)\(point-max\)\)\)\)\n
\(not \(mon-zero-or-onep 8\)\)\n\n\(not \(mon-zero-or-onep \"bubba\"\)\)\n
\(not \(mon-zero-or-onep []\)\)\n\n\(not \(mon-zero-or-onep '\(\)\)\)\n
\(not \(mon-zero-or-onep nil\)\)\n\n\(not \(mon-zero-or-onep \(not nil\)\)\)\n
:ALIASED-BY `zerop-or-one'
:ALIASED-BY `mon-zerop-or-one'
:ALIASED-BY `mon-one-or-zerop'
:ALIASED-BY `mon-1-or-0-p'
:ALIASED-BY `mon-0-or-1-p'\n
:SEE-ALSO `mon-string-or-null-and-zerop', `mon-booleanp-to-binary',
`mon-booleanp', `mon-sequence-all-booleanp', `mon-equality-or-predicate',
`mon-bitset-ternary'.\n►►►"
  (and (wholenump maybe-one-or-zero)
       (or (zerop maybe-one-or-zero)
           (zerop (1- maybe-one-or-zero)))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (and (mon-zero-or-onep 0) (mon-zero-or-onep 1)
;; |      (mon-zero-or-onep #x1)
;; |      (mon-zero-or-onep #o1)
;; |      (mon-zero-or-onep #x0)
;; |      (mon-zero-or-onep #o0)
;; |      (mon-zero-or-onep (length (not (not nil))))
;; |      (mon-zero-or-onep 
;; |       (with-temp-buffer (point-min)))
;; |      (not (mon-zero-or-onep 
;; |            (with-temp-buffer (insert (make-string 8 32))(point-max))))
;; |      (not (mon-zero-or-onep 8))
;; |      (not (mon-zero-or-onep "bubba"))
;; |      (not (mon-zero-or-onep [])) 
;; |      (not (mon-zero-or-onep '())) 
;; |      (not (mon-zero-or-onep nil)) 
;; |      (not (mon-zero-or-onep (not nil))))
;; `----
;;; ==============================
;;; :CHANGESET 2206
;;; :CREATED <Timestamp: #{2010-10-23T15:08:40-04:00Z}#{10426} - by MON KEY> 
(defun mon-booleanp-to-binary (maybe-a-boolean &optional return-if-not)
  "Convert a boolean value to 1 or 0.\n
When MAYBE-A-BOOLEAN satisfies `mon-booleanp' return 0 when `nil' 1 when `t'.\n
When optional arg RETURN-IF-NOT is non-nil and MAYBE-A-BOOLEAN is not a boolean
return MAYBE-A-BOOLEAN.\n
:EXAMPLE\n\n\(mon-booleanp-to-binary nil\)\n\n\(mon-booleanp-to-binary t\)\n
\(mon-booleanp-to-binary 0\)\n\n\(mon-booleanp-to-binary 1\)\n
\(mon-booleanp-to-binary #o1 t\) ;; :NOTE Handy if `wholenump' is already known\n
\(mon-zero-or-onep \(mon-booleanp-to-binary #x1 t\)\)\n
\(equal \(mon-booleanp-to-binary \"not-a-boolean\" t) \"not-a-boolean\"\)\n
\(not \(mon-booleanp-to-binary \"not-a-boolean\"\)\)\n
\(let \(\(mk-bvec \(make-bool-vector 64 t\)\)\)
  \(loop for z from 0 upto \(1- \(length mk-bvec\)\)
        do \(when \(evenp z\)
             \(aset mk-bvec z nil\)\)
        finally return 
        \(loop for bv across \(vconcat mk-bvec\)
              for i from 0
              collect \(if bv \(mon-booleanp-to-binary bv\) i\) into rtn
              finally return rtn\)\)\)\n
\(let \(\(mk-bvec  \(make-bool-vector 29 t\)\)
      \(not-true \(number-sequence 1 28 3\)\)
      rslts\)
  \(dolist \(tgl not-true mk-bvec\)
    \(unless \(eq tgl 28\)
      \(aset mk-bvec tgl nil\)\)\)
  \(setq rslts 
        \(mapcar #'mon-booleanp-to-binary
                \(cadr \(memq :true-table \(mon-bool-vector-pp mk-bvec\)\)\)\)\)
  \(nconc \(list :true-items \(apply '+ rslts\)\)
         \(list :not-true-at \(number-sequence 1 28 3\)\)
         \(list :true-table rslts\)\)\)\n
\(mon-booleanp-to-binary-TEST\)\n
:ALIASED-BY `mon-boolean-to-binary'
:ALIASED-BY `mon-t-to-1'
:ALIASED-BY `mon-nil-to-0'
:ALIASED-BY `mon-true-to-one'
:ALIASED-BY `mon-false-to-zero'\n
:SEE-ALSO `mon-bitset-ternary', `mon-one-or-zerop',
`mon-string-or-null-and-zerop', `mon-sequence-all-booleanp'
`mon-bool-vector-pp', `byte-boolean-vars', `fillarray', `zerop'.\n►►►"
  (let ((myb-bool (mon-booleanp maybe-a-boolean)))
    (or (and (cadr myb-bool)
             (or (and (car myb-bool) 1)
                 (and (not (car myb-bool)) 0)))
        (when return-if-not maybe-a-boolean))))
;;
;;; :TEST-ME (mon-booleanp-to-binary-TEST)


;;; ==============================
;;; :CHANGESET 2291
;;; :CREATED <Timestamp: #{2010-11-10T20:05:57-05:00Z}#{10453} - by MON KEY>
(defun mon-sequence-all-booleanp (check-t-or-nil w-map-fun w-map-seq 
                                                 &optional w-type-on-fail)
  "Whether each elt in mapped sequence is `t' or `nil'.\n
According to value passed for CHECK-T-OR-NIL, return value has one of formats:\n
 t              => \(t  t\)    ; Arg was `t' and all mapped elts were eq `t'
 t              => \(t nil\)   ; Arg was `t' one a mapped elt is not eq `t'
 nil            => \(nil t\)   ; Arg was `nil' and all mapped elts were `null'
 nil            => \(nil nil\) ; Arg was `nil' and a mapped elt is `null'
 w-type-on-fail => \(\(<TYPE> . <POSN>\) <SEQ>\) ; See below\n
CHECK-T-OR-NIL is a boolean, either `t' or `nil' and should satisfy `mon-booleanp'.
An error is signaled if not.\n
W-MAP-FUN is a function object accepting one argument.
It is passed to `mapcar' and should and return some non-boolean value in W-MAP-SEQ.\n
W-MAP-SEQ is a mappaple sequence satisfying `mon-sequence-mappable-p'.
An error is signaled if not.\n
When W-TYPE-ON-FAIL is non-nil and a non-boolean is encountered inside the
mapped value of W-MAP-SEQ return value has the form:\n
 \(\( <TYPE> . <POSN> \) <SEQ> \)\n
The first elt of list is a consed pair.\n
The cdr, <POSN> is index into the mapped value of W-MAP-SEQ.\n
The car, <TYPE> is the non-booleans type.
It is indicated as if by `type-of', except when <TYPE> is `t' or `nil' in which
case <TYPE> is indicated as `boolean` and not `symobl` this is a deviation from
the return value for `type-of'.\n
The intent is that when a \"non-boolean\" is encountered which is in fact a
_boolean_ but not one `eq' CHECK-T-OR-NIL, calling functions might benefit by
not having to re-interrogate quite so vigorously the thing just looked at.\n
The cadr of return value, <SEQ> is the mapped value of W-MAP-SEQ.\n
It is therefor possible to invoke `elt' on `cadr' with the `cdar' of return
value to obtain the value of the offending non-boolean and to do so
according to some heuristic per the type indicated at `caar'.\n
:EXAMPLE\n
\(mon-sequence-all-booleanp t 'cadr '\(\(a t\) \(b t\) \(c t\)\)\)\n
\(mon-sequence-all-booleanp t 'cadr '\(\(a t\) \(b t\) \(c nil\)\)\)\n
\(mon-sequence-all-booleanp t 'cadr '\(\(a t\) \(b t\) \(c nil\)\) t\)\n 
\(mon-sequence-all-booleanp t 'car `\(\(t a\) \(t b\) \(\"I'm a string\" c\)\) t\)\n
\(mon-sequence-all-booleanp t 'cadr '[\(a t\) \(b t\) \(c t\)]\)\n
\(mon-sequence-all-booleanp t 'car '[\(t a\) \(nil b\) \(c nil\)]\)\n
\(mon-sequence-all-booleanp t 'caddr '[\(a 1 t\) \(b 2 t\) \(c 3 nil\)] t\)\n
\(mon-sequence-all-booleanp t 'caddr `[\(a 1 t\) \(b 2 t\) \(c 3 ,\(make-vector 3 '6\)\)] t\)\n
\(mon-sequence-all-booleanp nil 'cadr '\(\(a nil\) \(b nil\) \(c nil\)\)\)\n
\(mon-sequence-all-booleanp nil 'cadr '\(\(a nil\) \(b nil\) \(c t\)\)\)\n
\(mon-sequence-all-booleanp nil 'cadr '\(\(a nil\) \(b nil\) \(c t\)\) t\)\n
\(mon-sequence-all-booleanp nil 'car `\(\(nil a\) \(nil b\) \(,\(make-vector 3 '6\) c\)\) t\)\n
\(mon-sequence-all-booleanp nil 'cadr '[\(a nil\) \(b nil\) \(c nil\)]\)\n
\(mon-sequence-all-booleanp nil 'car '[\(nil a\) \(t b\) \(c t\)]\)\n
\(mon-sequence-all-booleanp nil 'caddr '[\(a 1 nil\) \(b 2 nil\) \(c 3 t\)] t\)\n
\(mon-sequence-all-booleanp nil 'car `\(\(nil a\) \(nil b\) \(\"I'm a string\" c\)\) t\)\n
;; Following succesfully fail:\n
\(mon-sequence-all-booleanp 'a #'identity '\(a b\)\)\n
\(mon-sequence-all-booleanp nil #'identity '\(a . b\)\)\n
\(mon-sequence-all-booleanp t  #'identity \(current-buffer\)\)\n
\(mon-sequence-all-booleanp-TEST t\)\n
:ALIASED-BY `mon-list-all-booleanp'\n
:SEE-ALSO `mon-booleanp', `mon-booleanp-to-binary', `mon-zero-or-onep',
`mon-bitset-ternary', `facemenu-iterate'.\n►►►"
  (let ((msab-args (mon-booleanp check-t-or-nil)))
    (and (or (and (cadr msab-args) (progn (setq check-t-or-nil (car msab-args)) t))
             (error (concat ":FUNCTION `mon-sequence-all-booleanp' "
                            "-- arg CHECK-T-OR-NIL not `mon-booleanp', got: %S type-of: %s")
                    check-t-or-nil (type-of check-t-or-nil)))
         ;; :NOTE Reusing MSAB-ARGS. Now holding `mon-sequence-mappable-p'
         (setq msab-args (mon-sequence-mappable-p w-map-seq nil t))
         (or (car msab-args)
             (error (concat ":FUNCTION `mon-sequence-all-booleanp' "
                            "-- arg W-MAP-SEQ not `mon-sequence-mappable-p', got: %S type-of: %s")
                    w-map-seq (cdr msab-args)))
         ;;
         ;; :NOTE Check if `w-map-fun' is applicable/funcallable and signal/coerce if not? e.g.:
         ;; (or (memq (mon-function-object-p w-map-fun) (remq 'macro *mon-function-object-types*)) 
         ;;
         ;; Reusing MSAB-ARGS. Reset to nil for use below, but don't kick out of the conditional.
         (progn (setq msab-args) t))
    (catch 'failed-at
      (and w-type-on-fail (setq w-type-on-fail 0))
      (setq msab-args 
            (mapcar #'(lambda (msab-L-1)
                        ;; Convert elt to 1 or 0 if it is `t' or `nil'
                        (setq msab-L-1 (mon-booleanp-to-binary msab-L-1 t) 
                              ;; Establish an idx if W-TYPE-ON-FAIL
                              w-type-on-fail (and w-type-on-fail (1+ w-type-on-fail)))
                        (or  (and 
                              w-type-on-fail 
                              (mon-zero-or-onep msab-L-1) 
                              (or (and (not check-t-or-nil) 
                                       (or (= msab-L-1 0) 
                                           ;; Invert it back to `t' b/c we're gonna throw
                                           (progn (setq msab-L-1 t) nil)))
                                  (and check-t-or-nil 
                                       (or (= msab-L-1 1)
                                           ;; Invert it back to `nil' b/c we're gonna throw
                                           (setq msab-L-1 nil))))
                              msab-L-1)
                             (and (not w-type-on-fail) (mon-zero-or-onep msab-L-1) msab-L-1)
                             (throw 'failed-at ;; ((<TYPE> . <POSN>) <SEQ>) 
                                    (or (and w-type-on-fail 
                                             (prog1
                                                 (setq w-type-on-fail
                                                       ;; Report `t' and `nil' as 'boolean not 'symbol!
                                                       `((,(or (and (cadr (mon-booleanp msab-L-1)) 'boolean)
                                                               (type-of msab-L-1)) .  ,w-type-on-fail) ,msab-args))
                                               (setq msab-args nil)))
                                        (progn
                                          (setq w-type-on-fail (list check-t-or-nil nil)
                                                msab-args nil)
                                          w-type-on-fail)))))
                    (setq msab-args (mapcar w-map-fun w-map-seq))))
      ;; If CHECK-T-OR-NIL is `t' and sum is = length MSAB-ARGS all elts were `t'.
      ;; If it is `nil' we want 0 e.g.: (= (reduce '+ '(0 0 0)) 0)
      (or (and msab-args 
               (setq msab-args `(,check-t-or-nil 
                                 ,(= (apply '+ msab-args) 
                                     (or (and check-t-or-nil (length msab-args)) 0)))))
          w-type-on-fail))))
;;
;;; :TEST-ME (mon-sequence-all-booleanp)

;;; ==============================
;;; :COURTESY ert/lisp/emacs-lisp/ert.el :WAS `ert--proper-list-p'
;; (defun mon-list-proper-p (x)
;;   "Return non-nil if X is a proper list, nil otherwise."
;;   (loop
;;    for firstp = t then nil
;;    for fast = x then (cddr fast)
;;    for slow = x then (cdr slow) do
;;    (when (null fast) (return t))
;;    (when (not (consp fast)) (return nil))
;;    (when (null (cdr fast)) (return t))
;;    (when (not (consp (cdr fast))) (return nil))
;;    (when (and (not firstp) (eq fast slow)) (return nil))))
;;; ==============================

;;; ==============================
;;; :COURTESY :FILE lisp/format.el :WAS `format-proper-list-p'
;;; :CHANGESET 2001
;;; :CREATED <Timestamp: #{2010-07-27T16:48:36-04:00Z}#{10302} - by MON KEY>
(defun mon-list-proper-p (putatively-proper)
  "Return t if list PUTATIVELY-PROPER is a proper list.\n
A proper list is a list ending with a nil or cdr, not an atom.\n
:EXAMPLE\n\n\(mon-list-proper-p '\(a . b\)\)\n
\(mon-list-proper-p '\(a  b\)\)\n
\(mon-list-proper-p nil\)\n
\(mon-list-proper-p '\(\)\)\n
\(mon-list-proper-p '\(nil\)\)\n
\(mon-list-proper-p '\(\(\) nil\)\)\n
\(mon-list-proper-p '\(\(\) nil . a\)\)\n
\(mon-list-proper-p '\(\(\) nil . nil\)\)\n
\(mon-list-proper-p '\(\(\) \(\) . \(\)\)\)\n
\(mon-list-proper-p '\(\(\) . \(\(\) . \(\)\)\)\)\n
:ALIASED-BY `proper-list-p'
:ALIASED-BY `mon-proper-list-p'\n
:SEE-ALSO `mon-list-dotted-p', `mon-list-proper-and-dotted-p',
`mon-sequence-mappable-p', `mon-maybe-cons', `mon-list-match-tails',
`mon-list-make-unique', `mon-delq-cons', `mon-delq-cons', `mon-remove-dups',
`mon-remove-if', `mon-list-reorder', `mon-assoc-replace', `mon-intersection',
`mon-combine', `mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose',
`mon-flatten', `mon-recursive-apply', `mon-sublist', `mon-sublist-gutted',
`mon-nshuffle-vector', `mon-list-nshuffle', `mon-list-shuffle-safe',
`mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt'.\n►►►"
  (when (listp putatively-proper)
    (while (consp putatively-proper)
      (setq putatively-proper (cdr putatively-proper)))
    (null putatively-proper)))
;;
;;; :TEST-ME (mon-list-proper-p nil)
;;; :TEST-ME (mon-list-proper-p '(nil . nil))
;;; :TEST-ME (mon-list-proper-p '(a  b))

;;; ==============================
;;; :CHANGESET 2337
;;; :CREATED <Timestamp: #{2010-12-02T17:10:56-05:00Z}#{10484} - by MON KEY>
(defun mon-list-dotted-p (putatively-dotted)
  "Return non-nil when PUTATIVELY-DOTTED is a consed pair with non-null cdr.\n
PUTUTATIVELY-DOTTED is considered a \"dotted list\" if it is an improper list with a
terminating atom that is not the empty list.  A non-nil atom by itself is not
considered to be a list of any kind--not even a dotted list.\n
:EXAMPLE\n\n\(mon-list-dotted-p '\(t . t\)\)\n
\(mon-list-dotted-p '\(nil . t\)\)\n
\(mon-list-dotted-p '\(t . nil\)\)\n
\(mon-list-dotted-p  nil\)\n
\(mon-list-dotted-p '\(nil . nil\)\)\n
\(mon-list-dotted-p '\(a  b\)\)\n
:NOTE Does not test for cicularity.\n
:ALIASED-BY `mon-dotted-list-p'\n
:SEE-ALSO `mon-list-proper-p', `mon-list-proper-and-dotted-p',
`mon-sequence-mappable-p', `mon-delq-cons', `mon-maybe-cons',
`mon-pairlis'.\n►►►"
  (and (not (null putatively-dotted))
       (listp putatively-dotted)
       (not (mon-list-proper-p putatively-dotted))
       (not (zerop (safe-length putatively-dotted)))
       (eq (last putatively-dotted) putatively-dotted)))

;;; ==============================
;;; :CHANGESET 2331
;;; :CREATED <Timestamp: #{2010-12-02T20:24:14-05:00Z}#{10484} - by MON KEY>
(defun mon-list-proper-and-dotted-p (proper-and-dotted)
  "Return non-nil when list is proper and each elt is dotted.\n
Return PROPER-AND-DOTTED when list satisfies `mon-list-proper-p' and each of its
sublists is `mon-list-dotted-p'. Signal an error if not.\n
:EXAMPLE\n\n\(mon-list-proper-and-dotted-p '\(\(a . b\) \(c . b\) \(e . f\)\)\)\n
;; Following fail successfully:\n
\(mon-list-proper-and-dotted-p '\(a b\)\)\n
\(mon-list-proper-and-dotted-p '\(a . b\)\)\n
\(mon-list-proper-and-dotted-p '\(\(a . b\) \(c . d\) . e\)\)\n
\(mon-list-proper-and-dotted-p '\(\(a . b\) \(c . d\) \(e\)\)\)\n
\(mon-list-proper-and-dotted-p '\(\(a . b\) \(c . d\) \(\)\)\)\n
:ALIASED-BY `mon-list-dotted-and-proper-p'
:SEE-ALSO `mon-sequence-mappable-p', `mon-hash-or-mappable-p'.\n►►►"
  (mapc #'(lambda (mlpadp-L-1)
            (or (mon-list-dotted-p mlpadp-L-1)
                (error (concat ":FUNCTION `mon-list-proper-and-dotted-p' "
                               "-- elt of arg PROPER-AND-DOTTED not "
                               " `mon-list-dotted-p', got %S")
                       mlpadp-L-1)))
        (or (and (mon-list-proper-p proper-and-dotted) proper-and-dotted)
            (error (concat ":FUNCTION `mon-list-proper-and-dotted-p' "
                           "-- arg PROPER-AND-DOTTED not `mon-list-proper-p', got %S")
                   proper-and-dotted))))

;;; ==============================
;;; :CHANGESET 2211
;;; :CREATED <Timestamp: #{2010-10-27T15:43:58-04:00Z}#{10433} - by MON KEY>
(defun mon-sequence-mappable-p (seq-putatively-mappable &optional no-map-null
                                                        w-return-as-list)
  "Is SEQ-PUTATIVELY-MAPPABLE with: `mapc', `mapcar', or `mapconcat'.\n
Like `sequencep' but returns non-nil when an object satisfies one of the
following predicates:\n
  `stringp' `mon-list-proper-p' `bool-vector-p' `vectorp'\n
When optional arg NO-MAP-NULL is non-nil do not consider `nil' mappable.\n
When optional arg W-RETURN-AS-LIST is non-nil return a list or consed pair.
The car is non-nil if the object is mappable.
The cdr contains a type designator describing object.
Whether cdr is a cons or an atom depends on the value of the NO-MAP-NULL arg.
When NO-MAP-NULL is non-nil and object is either `t' `nil' or the empty-list
cdr of return value is a list. In such cases, its cadr will be either the symbol
`cons` or `boolean` and caddr is a truth-table as per `mon-booleanp'.
Return value when W-RETURN-AS-LIST is non-nil will have one of these forms:\n
  \(t   . cons\)                      ;; <PROPER> nil t
  \(t   . string\)                    ;; <STRING> nil t
  \(t   . vector\)                    ;; <VECTOR> nil t
  \(t   . boole-vector\)              ;; <BVECTR> nil t
  \(nil . <TYPE>\)                    ;; <TYPE>   nil t
  \(t     cons    \(nil t\)\)           ;; nil      nil t
  \(nil   boolean \(nil t\)\)           ;; nil        t t
  \(nil   boolean \(t   t\)\)           ;; t        nil t\n 
:EXAMPLE\n
\(mon-sequence-mappable-p t\)
\(mon-sequence-mappable-p t nil t\)\n
\(mon-sequence-mappable-p \(\)\)
\(mon-sequence-mappable-p \(\) nil t\)\n
\(mon-sequence-mappable-p \(\) 'no-map-null\)
\(mon-sequence-mappable-p \(\) 'no-map-null t\)\n
\(mon-sequence-mappable-p \(cons nil t\)\)
\(mon-sequence-mappable-p \(cons nil t\) nil t\)\n
\(mon-sequence-mappable-p \(make-list   8 1\) nil t\)
\(mon-sequence-mappable-p \(make-list   8 1\) nil t\)\n
\(mon-sequence-mappable-p \(make-string 8 32\)\)
\(mon-sequence-mappable-p \(make-string 8 32\) nil t\)\n
\(mon-sequence-mappable-p \(make-vector 8 0\) nil t\)
\(mon-sequence-mappable-p \(make-vector 8 0\) nil t\)\n
\(mon-sequence-mappable-p \(make-bool-vector 8 t\)\)
\(mon-sequence-mappable-p \(make-bool-vector 8 t\) nil t\)\n
\(mon-sequence-mappable-p \(current-buffer\)\)
\(mon-sequence-mappable-p \(current-buffer\) nil t\)\n
:NOTE char-tables and hashtables aren't mappable sequences use:
 `map-char-table', `maphash'\n
:NOTE This function is `print-gensym' agnostic and following returns
correctly reporting null when the cons is not mappable e.g.:\n
 \(let \(\(empty-cons  '\(#::not-really-here . #::neither-am-i\)\)\)
  `\(#::was-mappable   ,\(mon-sequence-mappable-p empty-cons\)
    #::was-consp      ,\(consp empty-cons\)
    #::w-empty-cons   ,empty-cons    
    #::car-empty-p ,\(not \(intern-soft \(car empty-cons\)\)\)
    #::cdr-empty-p ,\(not \(intern-soft \(cdr empty-cons\)\)\)\)\)\n
However, its list conterpart returns non-nil \(maybe no what you are expecting\):\n
 \(mon-with-print-gensyms
   \(let \(\(not-a-sym \(list '#::not-really-here '#::neither-am-i\)\)\)
     \(prin1 \(list '#::was-mappable
                  \(mon-sequence-mappable-p not-a-sym\)
                  not-a-sym\)\)\)\)\n
:NOTE This function differs with `sequencep' in some subtle but useful ways:\n
 - It can preempt attempts to map consed lists:\n
    \(sequencep '\(nil . t\)\)
    \(mon-sequence-mappable-p '\(nil . t\)\)\n
 - It can short-circuit a potential mapping over the empty list:\n
   Now, with NO-MAP-NULL we can say, \"Don't bother mapping if thing is null\":\n
   \(sequencep \(\)\)           
   \(mon-sequence-mappable-p \(\) t\)\n
- Likewise, if needed, we can now find out when thing was a boolean and whether
  that boolean was `t' or `nil':\n
    \(mon-sequence-mappable-p \(\) 'no-map-null t\)\n
- Additionally, when we want to map nil or the empty list but would still like
  to have access to the type of things given/gotten we can do this too,
  e.g. when passing strings, vectors, lists, (and possibly sordid other junk):\n
   \(let \(gthr \(minibuffer-message-timeout  3\)\)
     \(dolist \(mspp  `\(t nil \(a . b\) \(l i s t\) 
                      \"string\" [v e c t o r]
                      ,\(make-bool-vector 8 t\)
                      ;; Whoops, how'd these get in there?
                      ,\(make-marker\) ,\(current-buffer\)\)
                    \(setq gthr \(nreverse gthr\)\)\)
       \(let \(\(can-map \(mon-sequence-mappable-p mspp nil t\)\)\)
         \(when \(car can-map\)
           \(ignore 'do-something-here 'with 'car 'of 'can-map\)\)
         \(push can-map gthr\)\)\)
     \(prog1 gthr
       \(mapc #'\(lambda \(x\) 
                 \(when \(and \(consp \(cdr x\)\)
                            \(eq  \(cadr x\) 'cons\)
                            \(minibuffer-message \"Got us a sneaky cons.\"\)
                            \(minibuffer-message \"Dutifully mapped it anyways: %s\"
                                                \(when \(car x\) \"yep\"\)\)
                            \(minibuffer-message  
                             \"But, was it really just a `nil' in cons clothing: %s\"
                             \(when \(car \(cdaddr x\)\) \"the evidence suggests it\"\)\)\)\)\)
             gthr\)\)\)\n
:ALIASED-BY `mon-list-mappable-p'
:ALIASED-BY `mon-mappable-sequence-p'\n
:SEE-ALSO `mon-hash-or-mappable-p', `mon-list-dotted-p',
`mon-list-proper-and-dotted-p', `mon-list-string-longest', `mon-map-append',
`mon-map-combine', `mon-map1', `mon-mapcan', `mon-mapcar', `mon-mapcar-mac',
`mon-mapcon', `mon-mapl', `mon-maplist', `mon-maptree', `mon-bool-vector-pp',
`type-of', `mon-get-text-properties-parse-prop-val-type-chk',
`mon-help-sequence-functions', `*mon-non-mappable-object-types*'.\n►►►"
  (let ((msmp-typ  (type-of seq-putatively-mappable))
        (msmp-bool (mon-booleanp seq-putatively-mappable))
        msmp-mybe-rtn)
    (setq msmp-typ
          (not (or (and 
                    ;; We got a boolean `t', `nil', or an empty-list.
                    ;; We make a record of this for later.
                    ;; When its a null-like we first consider both
                    ;; `nil'/empty-list to be 'boolean not a 'cons and not a
                    ;; 'symbol.  If needed we loosen accordingly in the final
                    ;; branch according to args W-RETURN-AS-LIST and NO-MAP-NULL
                    (and (cadr msmp-bool) (setq msmp-mybe-rtn 'boolean))
                    ;; If its `t' we're done.
                    (or (and (car msmp-bool) (cadr msmp-bool))
                        ;; If not its `nil' or an empty list are we gonna map it?
                        (and no-map-null (and (not (car msmp-bool)) (cadr msmp-bool)))))
                   (and (memq msmp-typ *mon-non-mappable-object-types*)
                        (setq msmp-mybe-rtn msmp-typ))
                   (and (eq msmp-typ 'cons)
                        (not (mon-list-proper-p seq-putatively-mappable))
                        (setq msmp-mybe-rtn msmp-typ))
                   ;; It is a mappable store the type into msmp-mybe-rtn but don't
                   ;; trigger the `or' conditional in so doing, and don't step
                   ;; on 'boolean if its already there.
                   (and (not msmp-mybe-rtn)
                        (setq msmp-mybe-rtn msmp-typ)
                        nil))))
    (if w-return-as-list 
        (cond ((eq msmp-mybe-rtn 'boolean)
               (if no-map-null 
                   ;; Call it a boolean, e.g: -> (t boolean ( <t|nil> t))) 
                   `(,msmp-typ ,msmp-mybe-rtn ,msmp-bool)
                 ;; If its `nil' or empty-list call it a cons but record that it
                 ;; wasn't really, e.g:
                 ;;  (t cons (nil t))
                 ;; Else, its `t' do as above and calling it a boolean.
                 (if (listp seq-putatively-mappable)
                     `(,msmp-typ cons ,msmp-bool)
                   `(,msmp-typ ,msmp-mybe-rtn ,msmp-bool))))
              ;; Return anything else as a consed pair 
              (t `(,msmp-typ . ,msmp-mybe-rtn)))
      msmp-typ)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-11-30T20:52:35-05:00Z}#{10482} - by MON>
(defun mon-hash-or-mappable-p (string-symbol-hash)
  "Convenience function for `mon-hash-get-symbol-name-if'.\n
:SEE-ALSO `mon-sequence-mappable-p', `mon-list-dotted-p', `mon-list-proper-p',
`mon-list-proper-and-dotted-p', `mon-string-not-null-nor-zerop',
`mon-sequence-mappable-p', `mon-booleanp'.\n►►►"
  (mon-hash-get-symbol-name-if string-symbol-hash))

 
;;; ==============================
;;; :CHANGESET 2064
;;; :CREATED <Timestamp: #{2010-08-16T20:10:59-04:00Z}#{10331} - by MON KEY>
(defvar *mon-bit-table* nil
  "Variable caching the results of `mon-get-bit-table'.\n
:EXAMPLE\n\n\(mapcar #'\(lambda \(urng\) 
            \(memq :max-unsigned urng\)\) *mon-bit-table*\)\n
:SEE-ALSO `mon-bool-vector-pp', `mon-bitset-ternary',
`dbus-byte-array-to-string', `mon-help-binary-representation',
`mon-help-char-raw-bytes'.\n►►►")

;;; ==============================
;;; :PREFIX "mgbt-"
;;; :CHANGESET 2064
;;; :CREATED <Timestamp: #{2010-08-16T20:11:02-04:00Z}#{10331} - by MON KEY>
(defun mon-get-bit-table (&optional dsplyp intrp)
  "Return a list of bit values for bits 1-29.\n
When optional arg DSPLYP is non nil or called-interactively display results in
buffer named \"*MON-BIT-TABLE*\".\n
Return a list with the format:\n
 \(:bit-<N> :bit-weight N :2^ N :max-signed N :max-unsigned \(N . N\)\)\n
The values of these keys map as follows:\n
:bit-<N>       The bit identity \(0 indexed\)
:byte          The byte of the bit.
:2^            The bits value as a power of 2.
:bit-dec       The decimal value of bit at index.
:bit-oct       The octal value of bit at index.
:bit-hex       The hex value of bit at index.
:max-int       The maximimun signed number representable by bit.
:max-uint      A cons bounding the maximum unsigned range representable by bit.\n
:EXAMPLE\n\n\(assq :bit-28 \(mon-get-bit-table\)\)\n
\(memq :max-uint \(assq :bit-29 \(mon-get-bit-table\)\)\)\n
\(mon-get-bit-table t\)\n
:NOTE The first time this function is called it caches the results of its
evaluation to the variable `*mon-bit-table*'.\n
:ALIASED-BY `mon-byte-table-bits'
:ALIASED-BY `mon-bit-table-bits'\n
:SEE-ALSO `mon-bool-vector-pp', `mon-bitset-ternary',
`mon-help-binary-representation', `mon-help-char-raw-bytes', `fillarray'.\n►►►"
  (interactive "i\np")
  (let ((mgbt-gthr (when (bound-and-true-p *mon-bit-table*)
                *mon-bit-table*)))
    (when (null mgbt-gthr)
      ;; :PREFIX mgbt-D-1-
      (dotimes (mgbt-D-1 29 (progn (setq mgbt-gthr (nreverse mgbt-gthr))
                            (setq *mon-bit-table* mgbt-gthr)))
        (let* ((mgbt-D-1-nxt-i (1+ mgbt-D-1))
               (mgbt-D-1-bky       (car (read-from-string (format ":bit-%d" mgbt-D-1-nxt-i))))
               (mgbt-D-1-bit-wgt   (expt 2 mgbt-D-1))
               (mgbt-D-1-byt-wgt   (if (eq (% mgbt-D-1-nxt-i 8) 0)
                              (/ mgbt-D-1-nxt-i 8)
                            (1+ (/ (- mgbt-D-1-nxt-i (% mgbt-D-1-nxt-i 8)) 8))))
               (mgbt-D-1-oct-wgt (make-symbol (format "#o%o" mgbt-D-1-bit-wgt)))
               (mgbt-D-1-hex-wgt (make-symbol (format "#x%X" mgbt-D-1-bit-wgt)))
               ;; What no CL `reduce' at compile time w/out a
               ;; byte-compile-warning?  Thanks Emacs for protecting my
               ;; namespace... After all its not like `reduce' isn't stupid
               ;; fukcking usefull!!! Goddamn how I loathe thee CL runtime ban.
               ;; :WAS (mgbt-D-1-mx-sgn (+ mgbt-D-1-bit-wgt (reduce '+ mgbt-gthr :key 'caddr)))
               ;; 
               (mgbt-D-1-mx-sgn  (apply #'+ mgbt-D-1-bit-wgt 
                                        (mapcar #'(lambda (mgbt-L-1-fk-rdc) 
                                                    (nth 6 mgbt-L-1-fk-rdc)) mgbt-gthr)))
               (mgbt-D-1-unsgn-bot (/ (lognot mgbt-D-1-mx-sgn) 2))
               (mgbt-D-1-unsgn-top (lognot mgbt-D-1-unsgn-bot)))
          (push `(,mgbt-D-1-bky 
                  :byte ,mgbt-D-1-byt-wgt 
                  :2^ ,mgbt-D-1
                  :bit-weight ,mgbt-D-1-bit-wgt 
                  :bit-oct ,mgbt-D-1-oct-wgt 
                  :bit-hex ,mgbt-D-1-hex-wgt 
                  :max-int ,mgbt-D-1-mx-sgn 
                  :max-uint (,mgbt-D-1-unsgn-bot . ,mgbt-D-1-unsgn-top)) mgbt-gthr))))
    (when (or intrp dsplyp)
      (with-current-buffer 
          (get-buffer-create (upcase (symbol-name '*mon-bit-table*)))
        (erase-buffer)
        (save-excursion 
          (princ mgbt-gthr (current-buffer))
          (newline))
        (down-list)
        (ignore-errors (while (forward-list) (newline)))
        (save-excursion
          (mon-g2be -1)
          (forward-list)
          (backward-char 2)
          (delete-char 1))
        (emacs-lisp-mode)
        (display-buffer (current-buffer) t)))
    mgbt-gthr))
;;
;;; :TEST-ME (progn (makunbound '*mon-bit-table*) (mon-get-bit-table t))
;;; :TEST-ME (assq :bit-29 (mon-get-bit-table))
;;; :TEST-ME (memq :max-uint (assq :bit-29 (mon-get-bit-table)))


;;; ==============================
;;; :COURTESY ido.el :WAS `ido-fractionp'
;;; :CHANGESET 2360
;;; :CREATED <Timestamp: #{2010-12-13T14:23:28-05:00Z}#{10501} - by MON KEY>
(defun mon-fractionp (putative-fraction)
  "Return non-nil when PUTATIVE-FRACTION is in the range -1.0 to 1.0.\n
PUTATIVE-FRACTION should be a number satisfying `numberp' and not `zerop'.\n
:EXAMPLE\n\n\(mon-fractionp 0.1\)\n
\(mon-fractionp 0.5\)\n
\(mon-fractionp 1.0\)\n
\(mon-fractionp 1.001\)\n
\(mon-fractionp -0.1\)\n
\(mon-fractionp -0.5\)\n
\(mon-fractionp -1.0\)\n
\(mon-fractionp -1.001\)\n
:SEE-ALSO `mon-is-digit-simp', `mon-is-digit', `mon-equality-or-predicate',
`mon-booleanp', `mon-booleanp-to-binary', `mon-zero-or-onep',
`mon-string-or-null-and-zerop', `mon-string-not-null-nor-zerop',
`mon-sequence-all-booleanp', `mon-help-number-functions'.\n►►►"
  (and (numberp putative-fraction)
       (not (zerop putative-fraction))
       (or (and (> putative-fraction 0.0) 
                (<= putative-fraction 1.0))
           (and (< putative-fraction 0.0) 
                (>= putative-fraction -1.0)))))


;;; ==============================
;;; :CHANGESET 2389
;;; :CREATED <Timestamp: #{2011-01-13T14:22:38-05:00Z}#{11024} - by MON KEY>
(defun mon-integer-and-chacterp (int-or-char)
  "Whether INT-OR-CHAR is both an integer and a `characterp'
Return INT-OR-CHAR if it is `wholenump' and of the range 0 - `max-char'  inclusive."
  (and (wholenump int-or-char)
       (<= int-or-char (max-char))
       int-or-char))

;;; ==============================
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T16:06:56-04:00Z}#{10381} - by MON KEY>
(defun mon-char-code (w-char)
  "Return the integer code of W-CHAR.\n
W-CHAR is a char-literal, string, or symbol.\n
:EXAMPLE\n\n\(mon-char-code ?a\)\n
\(mon-char-code 97\)\n
\(mon-char-code \"a\"\)\n
\(mon-char-code 'a\)\n
:SEE-ALSO `prin1-char', `mon-coerce->char', `mon-mapl', `mon-maplist',
`mon-mapcar', `mon-mapcan', `mon-mapcon', `mon-remove-if-not', `mon-delete-if',
`mon-member-if', `mon-intersection', `mon-merge-list', `mon-alpha-char-p',
`digit-char-p', `mon-alpha-char-p', `*mon-whitespace-chars*',
`*mon-ascii-alpha-chars*', `*mon-digit-chars*'.\n►►►"
  (let ((rtn-char
         (case (type-of w-char)
           (integer (abs w-char))
           (string (string-to-char w-char))
           (symbol (aref (symbol-name w-char) 0))
           (t (1+ (max-char))))))
    (if (<=  rtn-char (max-char)) 
        rtn-char
      (error (concat 
              ":FUNCTION `mon-char-code' "
              "arg W-CHAR greater-than `max-char' or not coercable to char")))))

;;; ==============================
;;; :CHANGESET 2320
;;; :CREATED <Timestamp: #{2010-11-22T15:21:09-05:00Z}#{10471} - by MON KEY>
(defun mon-alpha-char-p (maybe-alpha-char)
  "Like Common Lisps `alpha-char-p'.\n
Return non-nil when MAYBE-ALPHA-CHAR is an alpha char in the ASCII ranges 
ASCII alpha chars in the ranges 65-90 and 92-122.\n
MAYBE-ALPHA-CHAR is a string or character.
:EXAMPLE\n\n\(mon-alpha-char-p ?A\)\n
\(mon-alpha-char-p ?Á\)\n
:ALIASED-BY `alpha-char-p'\n
:SEE-ALSO `mon-is-letter', `*mon-ascii-alpha-chars*', `mon-is-letter-simp',
`mon-is-alphanum', `mon-is-alphanum-simp', `mon-help-char-functions'.\n►►►"
  (mon-is-letter maybe-alpha-char t))

;;; ==============================
;;; :NOTE Erik Naggum already defined `digit-char-p' as a defsubst which arefs a
;;; 256 elt vector in :FILE parse-time.el We piggy-back that.
;;;
;;; (defvar parse-time-digits (make-vector 256 nil))
;;;  (unless (aref parse-time-digits ?0)
;;;         (loop for i from ?0 to ?9
;;;         do (aset parse-time-digits i (- i ?0))))
;;; (defsubst digit-char-p (char)
;;;    (aref parse-time-digits char))
;;;
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `is-digit'
(defun mon-is-digit (maybe-digit-char)
  "Reutrn non-nil when MAYBE-DIGIT-CHAR is a digit character.\n
MAYBE-DIGIT-CHAR is a character or string when `stringp' it is coerced to a char
as if by `string-to-char'.\n
:SEE-ALSO `mon-is-digit-simp', `mon-is-letter', `mon-is-alphanum',
`mon-string-index', `mon-string-position', `mon-char-code',
`mon-coerce->char', `mon-alpha-char-p', `digit-char-p',
`mon-help-number-functions'.\n►►►"
  (eval-when-compile (require 'parse-time))
  (cond ((stringp maybe-digit-char) 
         (mon-is-digit (string-to-char maybe-digit-char)))
        ((natnump maybe-digit-char)
         ;; (and (>= maybe-digit-char ?0)
         ;;      (<= maybe-digit-char ?9))
         (digit-char-p maybe-digit-char))
        (t nil)))
;;
;;; :TEST-ME (mon-is-digit (char-after (point)))8
;;; :TEST-ME (mon-is-digit (char-after (point)))x

;;; `alpha-char-p', `graphic-char-p', `digit-char-p'
;;; `alpha-char-p' constrained to ASCII set whereas `alphanumericp' isn't.
;;; `graphic-char-p'  printing-char (space through ~ in ASCII)

;; CL-USER> (alphanumericp (code-char 9658)) '=> nil
;; CL-USER> (code-char 5090) '=> #\CHEROKEE_LETTER_TLV
;; CL-USER> (alphanumericp (code-char 5090))   ;=> T
;; CL-USER> (princ-to-string (code-char 5090)) ;=> "Ꮲ"


;;; (defvar parse-time-digits (make-vector 256 nil))
;;;  (unless (aref parse-time-digits ?0)
;;;         (loop for i from ?0 to ?9
;;;         do (aset parse-time-digits i (- i ?0))))
;;; (defsubst digit-char-p (char)
;;;    (aref parse-time-digits char))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `is-letter'
(defun mon-is-letter (maybe-alpha-char &optional ascii-only)
  "Return non-nil when MAYBE-ALPHA-CHAR is an alpha character.\n
When optional arg ASCII-ONLY is non-nil only consider ASCII alpha chars in the
ranges 65-90 and 92-122.\n
:EXAMPLE\n\(mon-is-alphanum \(char-after \(point\)\)\)\x56
\(mon-is-alphanum \(char-after \(point\)\)\)8\n
\(mon-is-letter ?á\)
\(mon-is-letter ?á t\)
\(not \(mon-is-letter ?á t\)\)\n
:NOTE Similiar to Common Lisp's `alpha-char-p' but handles non-ASCII
characters.\n
:SEE-ALSO `mon-is-digit', `mon-is-alphanum', `mon-alpha-char-p',
`mon-string-index', `mon-string-position', `mon-alphabet-as-type',
`mon-char-code', `mon-coerce->char', `mon-help-char-functions'.\n►►►"
  (cond ((stringp maybe-alpha-char) 
         (mon-is-letter (string-to-char maybe-alpha-char) ascii-only))
        ((natnump maybe-alpha-char)
         (if ascii-only 
             (eq (aref *mon-ascii-alpha-chars* maybe-alpha-char) maybe-alpha-char)
           (not (equal (downcase maybe-alpha-char) 
                       (upcase   maybe-alpha-char)))))
        (t nil)))
;;
;;; (mon-is-letter 9658) (upcase "►") ) (upcase 9658) (upcase ?á)
;;; :TEST-ME (mon-is-letter (char-after (point)))x
;;; :TEST-ME (mon-is-letter (char-after (point)))8
;;; :TEST-ME (mon-is-letter ?á)
;;; :TEST-ME (not (mon-is-letter ?á t))

;;; ==============================
(defun mon-is-alphanum (maybe-alphanum)
  "Return t when MAYBE-ALPHANUM is either an alpha character or an integer.\n
:EXAMPLE\n\(mon-is-alphanum \(char-after \(point\)\)\)\C-h 
\(mon-is-alphanum \(char-after \(point\)\)\)8\n
:SEE-ALSO `mon-is-digit', `mon-alpha-char-p', `mon-is-digit-simp', `mon-string-index', 
`mon-string-position', `mon-alphabet-as-type', `mon-char-code',
`mon-coerce->char', `mon-help-char-functions'.\n►►►"
  (or (mon-is-letter maybe-alphanum)
      (mon-is-digit  maybe-alphanum)))
;;
;;; :TEST-ME (mon-is-alphanum "8")
;;; :TEST-ME (mon-is-alphanum "A")
;;; :TEST-ME (mon-is-alphanum "a")
;;; :TEST-ME (mon-is-alphanum "?")
;;; :TEST-ME (mon-is-alphanum (char-to-string 88)) ;X
;;; :TEST-ME (mon-is-alphanum (char-to-string 10)) ;C-j LF newline
;;; :TEST-ME (mon-is-alphanum (char-to-string 32)) ;SPC
;;; :TEST-ME (mon-is-alphanum ?\C-m)
;;; :TEST-ME (mon-is-alphanum ?\13)
;;; :TEST-ME (mon-is-alphanum 13)
;;; :TEST-ME (mon-is-alphanum "13") ;; (char-to-string (string-to-char "13"))
;;; :TEST-ME (mon-is-alphanum (let ((what ?\b)) (format "%s" what)))
;;; :TEST-ME (progn (insert ?\8) (mon-is-alphanum (char-before (point))))
;;; :TEST-ME (progn (insert 8) (mon-is-alphanum (char-before (point))))

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `bibtex-isdigit'
;;; :CREATED <Timestamp: 2009-08-03-W32-1T10:27:00-0400Z - by MON KEY>
(defun mon-is-digit-simp (maybe-digit-char)
 "Return non-nil if MAYBE-DIGIT-CHAR is a digit character.\n
Unlike `mon-is-digit' fails when other than \\? prefixed digit.
Wants char literals.\n:EXAMPLE\n\(mon-is-digit-simp ?0\)
\(mon-is-digit-simp \"0\"\)\n\(mon-is-digit \"0\"\)\n
:SEE-ALSO `mon-is-letter-simp', `mon-is-alphanum-simp', `mon-alpha-char-p',
`mon-is-digit', `mon-is-letter', `mon-is-alphanum', `mon-string-index',
`mon-string-position', `mon-alphabet-as-type', `mon-char-code',
`mon-coerce->char', `*mon-whitespace-chars*', `*mon-ascii-alpha-chars*',
`*mon-digit-chars*', `mon-help-number-functions'.\n►►►"
 (and (>= maybe-digit-char ?0) (<= maybe-digit-char ?9)))
;;
;;; :TEST-ME (mon-is-digit-simp ?0)

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `bibtex-isalpha'
;;; :MODIFICATIONS <Timestamp: #{2010-03-30T16:35:04-04:00Z}#{10132} - by MON KEY>
;;; :RENAMED Arg `C' -> SIMP-LTR
;;; :CREATED <Timestamp: 2009-08-03-W32-1T10:26:57-0400Z - by MON KEY>
(defun mon-is-letter-simp (simp-ltr) 
 "Return non-nil when SIMP-LTR is an alphabetic character, and otherwise, nil.
Unlike `mon-is-letter' fails when other than \\? prefixed chars.
Wants char literals.\n
:EXAMPLE\n\(mon-is-letter-simp ?x\)\n
\(mon-is-letter-simp \"x\"\)
\(mon-is-letter \"x\"\)\n
:SEE-ALSO `mon-is-digit-simp',`mon-is-alphanum-simp'.  `mon-is-letter',
`mon-is-digit', `mon-alpha-char-p', `mon-string-index', `mon-string-position',
`mon-alphabet-as-type', `mon-char-code', `mon-coerce->char',
`*mon-whitespace-chars*', `*mon-ascii-alpha-chars*', `*mon-digit-chars*',
`mon-help-char-functions'.\n►►►"
 (or  (and (>= simp-ltr ?a) (<= simp-ltr ?z))
      (and (>= simp-ltr ?A) (<= simp-ltr ?Z))))
;;
;;; :TEST-ME (mon-is-letter-simp ?x)

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-03-W32-1T15:18:01-0400Z - by MON KEY>
(defun mon-is-alphanum-simp (simp-alph)
  "Return t when SIMP-ALPH is either an alpha character or integer.\n
Unlike `mon-is-alphanum' fails when other than \\? prefixed chars or digits.
Wants char literals.\n
\(mon-is-alphanum-simp ?8\)             
\(mon-is-alphanum-simp ?A\)             
\(mon-is-alphanum-simp \"8\"\)            
\(mon-is-alphanum-simp \"A\"\)            
\(mon-is-alphanum-simp \(prin1-char 88\)\)
\(mon-is-alphanum \(char-to-string 88\)\)\n
:SEE-ALSO `mon-is-digit-simp' `mon-is-letter-simp', `mon-string-index',
`mon-is-digit', `mon-is-letter', `mon-alpha-char-p', `mon-string-position',
`mon-alphabet-as-type', `mon-char-code', `mon-coerce->char',
`*mon-whitespace-chars*', `*mon-ascii-alpha-chars*', `*mon-digit-chars*',
`mon-help-char-functions'.\n►►►"
  (or (mon-is-letter-simp simp-alph)
      (mon-is-digit-simp simp-alph)))
;;
;;; :TEST-ME (mon-is-alphanum-simp ?8)
;;; :TEST-ME (mon-is-alphanum-simp ?A)
;;; :TEST-ME (mon-is-alphanum-simp "8") ;should fail
;;; :TEST-ME (mon-is-alphanum-simp "A");should fail
;;; :TEST-ME (mon-is-alphanum-simp (prin1-char 88)) ;should fail


;;; ==============================
;;; :CHANGESET 1985 <Timestamp: #{2010-07-16T18:36:28-04:00Z}#{10285} - by MON KEY>
;;; :CREATED <Timestamp: 2009-08-03-W32-1T18:47:33-0400Z - by MON KEY>
(defun mon-coerce->char (thing->char &optional no-abs-no-flt)
  "Convert THING->CHAR with length of 1 to a char as per `string-to-char'.\n
THING->CHAR can be a number, symbol, string, float.\n
IF THING->CHAR is characterp returns thing->char.\n
If coercion of thing->char fails signal an error.\n
When optional NO-ABS-NO-FLT is non-nil do not invoke `round' on when THING->CHAR
satisfies the predicate `floatp', do not invoke `abs' on when THING->CHAR is an
integer that does not satisfy the predicate `wholenump'.\n
:ALIASED-BY `mon-char-coerce'\n
:SEE-ALSO `string-to-char',`prin1-char', `get-char-code-property',
`mon-string-to-hex-list', `mon-string-from-hex-list',
`mon-string-to-hex-list-cln-chars', `mon-string-to-hex-string',
`mon-string-to-regexp', `mon-string-to-sequence', `mon-string-to-symbol',
`mon-string-upto-index', `mon-symbol-to-string', `mon-help-char-functions'
`mon-help-char-raw-bytes', `mon-help-char-representation',
`mon-help-char-table-functions', `mon-help-char-unidata-table',
`mon-help-charset-coding-functions', `mon-help-char-coding-functions',
`mon-help-char-composition'.\n►►►"
  ;; :NOTE (get-char-code-property thing->char 'numeric-value)
  (let* ((t->c thing->char) 
         (typ-tc (type-of t->c)))
    (cond   ((mon-is-digit t->c)
             (cond ((stringp t->c)               
                    (cond ((= (length t->c) 1) (string-to-char t->c))
                          ((> (length t->c) 1) (mon-coerce->char (string-to-number t->c)))))
                   ((not (stringp t->c)) t->c))) 
            ((mon-is-letter t->c)
             (if (stringp t->c)
                 (if (= (length t->c) 1)
                     (string-to-char t->c)
                   (error (concat ":FUNCTION `mon-coerce->char' "
                                  "-- arg THING->CHAR has length %d "
                                  "can only coerce strings of length 1, got: %S")
                          (length t->c) t->c ))
               t->c))
            ((and (numberp t->c) 
                  (not (mon-is-digit t->c))
                  (not (mon-is-letter t->c))
                  (cond ((and (wholenump t->c) (not (floatp t->c)))
                         (if (> 10 t->c) 
                             (string-to-char (number-to-string t->c))
                           t->c))
                        ((floatp t->c)
                         (if (and (wholenump (abs (round t->c)))
                                  (not no-abs-no-flt))
                             (mon-coerce->char (abs (round t->c)))
                           ;; Whats up with the `format' calls? Surely these aren't needed?
                           ;; :WAS (error (format "Can't coerce %S '%S' to char" typ-tc t->c)))) 
                           (error (concat ":FUNCTION `mon-coerce->char' "
                                          "-- arg THING->CHAR can not coerce to char, "
                                          "got: %S type-of: %S")
                                  t->c typ-tc )))
                        
                        ((not (wholenump t->c))
                         (if (and (wholenump (abs t->c))
                                  (not no-abs-no-flt))
                             (mon-coerce->char (abs t->c))
                           ;; :WAS (error (format "Can't coerce %S '%S' to char" typ-tc t->c))))))) 
                           (error (concat ":FUNCTION `mon-coerce->char' "
                                          "-- can not coerce %S '%S' to char")
                                  typ-tc t->c))))))

            ((stringp t->c)
             (cond ((/= (string-to-number  t->c) 0)
                    (mon-coerce->char (string-to-number t->c)))
                   ;; :MODIFICATIONS <Timestamp: #{2009-08-26T15:31:55-04:00Z}#{09353} - by MON KEY>
                   ;; The logic on this was busted. we're throwing an error uncondtionally the truth.
                   ;; Most likely was transcribing an if and didn't close it
                   ;; ((= (length t->c) 1)
                   ;;  (string-to-char t->c)
                   ;;  (error (format "%s has a length of %s, can only coerce strings of length 1" t->c (length t->c))))))
                   ((= (length t->c) 1) (string-to-char t->c))
                   ((or (= (length t->c) 0) (> (length t->c) 1))
                    (error (format "%s has a length of %s, can only coerce strings of length 1" t->c (length t->c))))))
            ((eq typ-tc 'symbol)
             (let ((thing-string (format "%s" (identity t->c))))
               (if (= (length thing-string) 1)
                   (mon-coerce->char thing-string)
                 (error (format "Can't coerce %S '%s' with > length 1" (type-of t->c) thing-string)))))
            (t (error   (format "Can't coerce %S '%S' to char" (type-of t->c) t->c))))))
;;
;; 
;;; :TEST-ME (mon-coerce->char 'b)
;;; :TEST-ME (mon-coerce->char "b")
;;; :TEST-ME (mon-coerce->char ?b)
;;; :TEST-ME (mon-coerce->char 8)
;;; :TEST-ME (mon-coerce->char '8)
;;; :TEST-ME (mon-coerce->char "8")
;;; :TEST-ME (mon-coerce->char 44)
;;; :TEST-ME (mon-coerce->char "44")
;;; :TEST-ME (mon-coerce->char '44)
;;; :TEST-ME (mon-coerce->char ?8)
;;; :TEST-ME (mon-coerce->char -8)
;;; :NOTE Following should fail:
;;; :TEST-ME (mon-coerce->char 8.8)
;;; :TEST-ME (mon-coerce->char "8.8")
;;; :TEST-ME (mon-coerce->char (get-buffer (buffer-name)))

;;; ==============================
;;; :PREFIX "msts-" 
;;; :CHANGESET 2119 <Timestamp: #{2010-09-14T17:02:55-04:00Z}#{10372} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-14T11:06:04-04:00Z}#{09423} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-26T17:08:02-04:00Z}#{09353} - by MON KEY>
(defun mon-string-to-symbol (str-to-sym &optional start end)
  "Return string STR-TO-SYM as a symbol.\n
When optional args START and END are non-nil delimit the `substring' of STR-TO-SYM.\n
START and END default to 0 and \(length string\) respectively.\n
If STR-TO-SYM (or a substring therin if START is non-ni) satisfies `intern-soft' for
default `obarray' return value is as if by `read', else symbol is allocated as
if by `make-symbol' with its value and function cells void, and its property
list `nil'.\n
:EXAMPLE\n\(and \(intern-soft \(format \"%S\"
                          \(mon-string-to-symbol \"mon-string-to-symbol\"\)\)
                  obarray\) t\)\n
\(mon-string-to-symbol \(mon-symbol->string 'bubba\)\)\n
\(progn 
  \(unintern \"bubba\" obarray\) 
  \(not \(intern-soft \(format \"%S\" \(mon-string-to-symbol \"bubba\"\)\) obarray\)\)\)\n
\(progn 
  \(unintern \"bubba\" obarray\) 
  \(and \(intern-soft 
        \(format \"%S\" \(intern 
                      \(symbol-name \(mon-string-to-symbol \"bubba\"\)\)
                      obarray\)
                obarray\)\) t\)\)\n
\(progn 
  \(unintern \"bubb\" obarray\)
  \(not \(intern-soft 
        \(symbol-name \(mon-string-to-symbol \"bubba\" 0 4\)\)
        obarray\)\)\)\n
\(prog2 
    \(unintern \"bubb\" obarray\)
    \(and \(intern-soft 
          \(format \"%S\" \(intern 
                        \(symbol-name \(mon-string-to-symbol \"bubba\" 0 4\)\)
                        obarray\)\)
          obarray\) t\)
  \(unintern \"bubb\" obarray\)\)\n
:SEE-ALSO `mon-symbol-cells-bound-p', `mon-symbol-to-string',
`mon-string-to-sequence', `mon-string-from-sequence', `mon-string-alpha-list',
`mon-string-index', `mon-string-has-suffix', `mon-alphabet-as-type',
`mon-string-replace-char', `mon-help-symbol-functions'.\n►►►"
  ;; :WAS (car (read-from-string str-to-sym start end)))
  (when (or ;;(null str-to-sym) 
         ;; Don't handle the empty string. YUK!
         (mon-string-or-null-and-zerop str-to-sym)
         (not (stringp str-to-sym)))
    (mon-error-string-err-format "mon-string-to-symbol" "str-to-sym" str-to-sym t))
  ;; (error (concat ":FUNCTION `mon-string-to-symbol'"
  ;;                "-- arg STR-TO-SYM does not satisfy `stringp', got %S")
  ;;        str-to-sym))
  ;; str-to-sym
  (let ((msts-sbstr 
         ;; Bad idea generating arbitrary symbols with negative integer indexes
         ;; into STR-TO-SYM. If either START or END aren't `wholenump' signal.
         (or (and start 
                  (or (wholenump start)
                      (error (concat 
                              ":FUNCTION `mon-string-to-symbol'"
                              "-- optional arg START not `wholenump', got %S")
                             start))
                  (substring str-to-sym start 
                             (and end 
                                  (or (and (wholenump end) end)
                                      (error (concat 
                                              ":FUNCTION `mon-string-to-symbol'"
                                              "-- optional arg END not `wholenump', got %S")
                                             end))
                                  )))
             str-to-sym)))
    (if (intern-soft msts-sbstr obarray)
        (read msts-sbstr) 
      (make-symbol msts-sbstr))))
;;
;;; :TEST-ME (intern-soft (mon-string-to-symbol "mon-string-to-symbol") obarray)
;;; :TEST-ME (progn (unintern "bubba" obarray) (intern-soft (mon-string-to-symbol "bubba")))
;;; :TEST-ME (intern-soft (intern (symbol-name (mon-string-to-symbol "bubba"))))
;;; :TEST-ME (progn (unintern "bubb" obarray) (intern-soft (mon-string-to-symbol "bubba" 0 4) obarray))
;;; :TEST-ME (intern-soft (intern (symbol-name (mon-string-to-symbol "bubba" 0 4))) obarray)
;;; :TEST-ME (mon-string-to-symbol "bubba")
;;; :TEST-ME (mon-string-to-symbol "mon-string-to-symbol" 4 10)
;;; :TEST-ME (mon-string-to-symbol "mon-string-to-symbol" 4)
;;; :TEST-ME (mon-string-to-symbol (mon-symbol->string 'bubba))
;;; :TEST-ME (mon-string-to-symbol "mon-string-to-symbol" -1) ;; should fail successfully
;;; :TEST-ME (mon-string-to-symbol "mon-string-to-symbol" 0 -9) ;; should fail successfully
;;; :TEST-ME (mon-string-to-symbol (current-buffer)) ;; should fail successfully

;;; ==============================
;;; :NOTE Periodically MON is completely at a loss for how to accomplish this.
;;;       Lets make _damn_ sure it never happens again!!
;;; :CHANGESET 1911 <Timestamp: #{2010-06-22T15:13:06-04:00Z}#{10252} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-09-29T21:00:43-04:00Z}#{09403} - by MON KEY>
(defun mon-symbol-to-string (symbol-to-frob) 
  "Return SYMBOL as a string.\n
:EXAMPLE\n(mon-symbol-to-string 'bubba)\n
\(mon-symbol-to-string \(mon-string-to-symbol \"bubba\"\)\)\n
\(progn
  \(unintern \(intern-soft \"some-uninterned-symbol\" obarray\)\)
  \(mon-symbol-to-string
   \(make-symbol \"some-uninterned-symbol\"\)\)\)\n
:SEE-ALSO `mon-symbol-cells-bound-p', `mon-string-to-symbol',
`mon-string-to-sequence', `mon-string-from-sequence', `mon-alphabet-as-type',
`mon-string-split', `mon-string-replace-char', `symbol-name'
`mon-help-symbol-functions'.\n►►►"
  ;; Which is more correct? 
  ;; (format "%s" symbol) 
  ;; (format "%S" symbol-to-frob)
  ;; <Timestamp: #{2010-10-29T18:58:45-04:00Z}#{10435} - by MON KEY>
  ;; Well, if we happen to get a string then "%s" is better...
  ;; So, added the `stringp' check below. NTIM, the `format' thing was silly :P
  ;;
  (or (and (stringp symbol-to-frob) symbol-to-frob)
      ;; :WAS (format "%S" symbol-to-frob))) ; <-Silly 
      ;; :WAS (symbol-name (intern (format "%S" symbol-to-frob) 
      ;;                      (make-vector 2 nil))) ; <- Also Silly
      (symbol-name symbol-to-frob)))
;;
;;; :TEST-ME (mon-symbol->string 'bubba)
;;; :TEST-ME (mon-symbol->string (mon-string-to-symbol "bubba"))
;;; :TEST-ME (progn  (and (intern-soft "some-uninterned-symbol" obarray) 
;;                         (unintern "some-uninterned-symbol" obarray))
;;;            (mon-symbol-to-string (make-symbol "some-uninterned-symbol")))

;;; ==============================
;;; :PREFIX "msts-"
;;; :CHANGESET 1899 <Timestamp: #{2010-06-22T14:58:14-04:00Z}#{10252} - by MON KEY>
;;; :CREATED <Timestamp: Wednesday June 24, 2009 @ 11:50.11 AM - by MON KEY>
(defun mon-string-to-sequence (string-to-frob &rest more-strings)
  "Return string STRING-TO-FROB as a list of chars.\n
When rest arg MORE-STRINGS is non-nil each additional string is converted chars
and added to the list of returned chars.\n
Signal an error if MORE-STRINGS does not satisfy predicate `string-or-null-p'.\n
:EXAMPLE\n\n\(mon-string-to-sequence \"?\\C-lstring\"\)\n
\(apply 'string \(mon-string-to-sequence \"?\\C-lstring\"\)\)\n
\(mon-string-to-sequence \"str1\" \"str2\"\)\n
\(mon-string-to-sequence \"str1\" \"str2\" \"str3\"\)\n
\(mon-string-to-sequence \"str1\" \"str2\" nil \"str3\"\)\n
\(apply 'mon-string-to-sequence \"str1\" \"str2\" nil \"str3\" nil '\(\"more string\"\)\)\n
:SEE-ALSO `mon-string-from-sequence', `mon-string-index', `mon-string-position',
`mon-string-split', `mon-string-alpha-list', `mon-is-alphanum', `mon-is-digit',
`mon-is-letter', `mon-alphabet-as-type', `mon-string-replace-char',
`string-to-list', `string-to-vector'.\n►►►"
  ;; :NOTE `string-to-list' does this: (append string nil)
  (if more-strings
      (let ((msts-w/more 
             (progn
               (mapc #'(lambda (msts-L-1) 
                         (unless (string-or-null-p msts-L-1)
                           (error (concat 
                                   ":FUNCTION `mon-string-to-sequence' "
                                   "-- arg MORE-STRINGS did not satisfy `string-or-null-p'"))))
                     more-strings)
               `(,string-to-frob ,@more-strings nil))))
        (apply 'append (car msts-w/more) (cdr msts-w/more)))
    (let (msts-to-seq)
      (mapc #'(lambda (msts-L-2) 
                (push msts-L-2 msts-to-seq)) string-to-frob)
      (nreverse msts-to-seq))))
;;
;;; :TEST-ME (mon-string-to-sequence "?\C-lstring")
;;; :TEST-ME (apply 'string (mon-string-to-sequence "?\C-lstring"))
;;; :TEST-ME (mon-string-to-sequence "str1" "str2")
;;; :TEST-ME (mon-string-to-sequence "str1" "str2" "str3")
;;; :TEST-ME (mon-string-to-sequence "str1" "str2" nil "str3")
;;; :TEST-ME (apply 'mon-string-to-sequence "str1" "str2" nil "str3" nil '("more string"))
;;; :TEST-ME (mon-string-to-sequence "str1" "str2" "str3" '("str4")) ;; <- error

;;; ==============================
;;; :PREFIX "msfs-"
;;; :MODIFICATIONS <Timestamp: #{2009-10-09T16:07:57-04:00Z}#{09415} - by MON>
;;; :CREATED <Timestamp: #{2009-09-30T13:31:42-04:00Z}#{09403} - by MON KEY>
(defun mon-string-from-sequence (stringify-seq &rest other-seqs)
  "Return STRINGIFY-SEQ - a sequence of character integers - as a string.\n
When OTHER-SEQS is non-nil these can be lists (quoted), vectors, or strings in
any combination these will be concatenated to return value also.\n
:EXAMPLE\n\n\(mon-string-from-sequence '(115 116 114 105 110 103))\n
\(mon-string-from-sequence\n '(115 116 114 105 110 103 48)
 '\(115 116 114 105 110 103 115 49\)\n '\(115 116 114 50\)\)\n
\(mon-string-from-sequence \(number-sequence 0 127\)\)\n
\(mon-string-from-sequence\n '\(98 117 98 98 97 115\)
 \"string0\"\n [32 98 117 98 98 97 115 32]
 '\(115 116 114 105 110 103 49 32\)\n [98 117 98 98 97 32]
 '\(103 111 116 32 110 105 108\)\)\n
:ALIASED-BY `mon-sequence-to-string', `mon-seq->string'\n
:SEE-ALSO `mon-string-index',`mon-string-position', `mon-string-alpha-list',
`mon-is-alphanum',`mon-is-digit',`mon-is-letter', `mon-alphabet-as-type',
`mon-string-replace-char', `mon-string-split'.\n►►►"
  (let ((msfs-L-str (lambda (msfs-L-1) (apply #'string msfs-L-1)))
        (msfs-chk-seqs (when (and other-seqs (sequencep other-seqs))
                    (mapcar #'(lambda (msfs-L-2) 
                                (cond ((vectorp msfs-L-2) (append msfs-L-2 nil))
                                      ((stringp msfs-L-2) (mon-string-to-sequence msfs-L-2))
                                      ((listp  msfs-L-2) msfs-L-2)))
                            other-seqs)))
        msfs-seq-seqs)
    (while msfs-chk-seqs 
      (push (funcall msfs-L-str (pop msfs-chk-seqs)) msfs-seq-seqs))
    (setq msfs-seq-seqs (nreverse msfs-seq-seqs))
    (push (funcall msfs-L-str 
                   (if (nlistp stringify-seq)
                       (cond ((vectorp stringify-seq) 
                              (append stringify-seq nil))
                             ((stringp stringify-seq) 
                              (mon-string-to-sequence stringify-seq)))
                     stringify-seq)) msfs-seq-seqs)
    (apply #'concat (car msfs-seq-seqs) (cdr msfs-seq-seqs))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (string-equal
;; |  (mon-string-from-sequence '(98 117 98 98 97)) 
;; |  "bubba")
;; | 
;; | (string-equal 
;; |  (mon-string-from-sequence (string-to-list "bubba")) 
;; |  "bubba")
;; | 
;; | (string-equal 
;; |  (mon-string-from-sequence '(98 117 98 98 97 115 97) (string-to-list "bubba")) 
;; |  "bubbasabubba")
;; | 
;; | (string-equal 
;; |  (mon-string-from-sequence '(98 117 98 98 97) [98 117 98 98 97 115 97]) 
;; |  "bubbabubbasa")
;; | 
;; | (string-equal (mon-string-from-sequence '(98 117 98 98 97) 
;; |                                         "string0" 
;; |                                         [98 117 98 98 97 115 97] 
;; |                                         "string" [98 117 98 98 97 115 97])
;; |               "bubbastring0bubbasastringbubbasa")
;; `----

;;; ==============================
(provide 'mon-type-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-type-utils.el ends here
;;; EOF
