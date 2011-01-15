;;; mon-macs.el --- macros for use with mon-*utils features
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-macs.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-20T20:21:14-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-macs provides macros for use with mon-*utils features
;;
;; FUNCTIONS:►►►
;;
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;; `mon-error-protect', `handler-case', `%mon-format-chk-keys',
;;
;; `mon-copy-list-mac', `mon-mapcar-mac', `mon-foreach', `mon-for', `mon-loop',
;; `mon-list-sift',
;;
;; `mon-equality-for-type', `mon-nshuffle-vector',
;;
;; `mon-with-print-gensyms', `mon-with-gensyms', `mon-gensym',
;; `defconstant', `defparameter',
;;
;; `mon-set-text-properies-region', `mon-get-face-at-posn',
;;
;; `mon-with-file-buffer', `mon-buffer-exists-p', `mon-with-buffer-undo-disabled',
;; `mon-with-inhibit-buffer-read-only', `mon-print-in-buffer-if-p',
;;
;; `mon-line-dolines', `mon-cat',
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
;; `*mon-macs-xrefs*',
;;
;; GROUPS:
;; `mon-macs'
;;
;; ALIASED/ADVISED/SUBST'D:
;; :NOTE Aliases defined in :FILE mon-aliases.el
;; 
;; <UNQUALIFIED-ALIAS>                  <PREFIX>-<NON-CORE-SYMBOL>
;; `nshuffle-vector'                 -> `mon-nshuffle-vector'
;; `with-gensyms'                    -> `mon-with-gensyms'
;; `with-print-gensyms'              -> `mon-with-print-gensyms'
;; `buffer-exists-p'                 -> `mon-buffer-exists-p'
;;
;;  <PREFIX>-<QUALIFIED>                <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-buffer-do-with-undo-disabled'  -> `mon-with-buffer-undo-disabled'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-print-in-buffer-if-p'            <- mon-insertion-utils.el
;; `mon-toggle-restore-llm'              <- mon-replacement-utils.el
;; `mon-naf-mode-toggle-restore-llm'     <- mon-replacement-utils.el
;; `mon-cat'                             <- mon-dir-utils.el
;; `mon-set-text-properies-region'       <- mon-text-property-utils.el
;; `mon-get-face-at-posn'                <- mon-text-property-utils.el
;; `mon-intern-artist'
;; TODO:
;;
;; NOTES:
;; Don't forget when (and (featurep 'slime) "you've got slime"), the macros
;; `with-struct', `when-let', `destructure-case', `slime-with-rigid-indentation'
;; have also been defined for all of your elisp hacking needs, as are the
;; functions `slime-curry', `slime-rcurry', `slime-indent-rigidly',
;; `slime-insert-indented', `slime-prin1-to-string',
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-macs.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-macs. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-20T20:21:14-05:00Z}#{10466} - by MON KEY>
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

(require 'edebug) 

(declare-function mon-g2be                      "mon-buffer-utils" (&optional min/max-go no-go))
(declare-function mon-list-proper-p             "mon-seq-utils" (putatively-proper))
(declare-function mon-mapcar                    "mon-seq-utils" (mapcar-fun mapcar-lst &rest more-lsts))
(declare-function mon-gensym-counter-randomizer "mon-randomize-utils" (randomize-sym/str))

;;; ==============================
;;; :CHANGESET 2392
;;; :CREATED <Timestamp: #{2011-01-14T16:56:07-05:00Z}#{11025} - by MON KEY>
(defgroup mon-macs nil
  "Customization group for variables and functions of :FILE mon-macs.el\n
:SEE-ALSO .\n►►►"
  ;; :prefix "<PREFIX>"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE (URL `http://www.emacswiki.org/emacs/mon-macs.el')")
  :link '(emacs-library-link 
          :tag  ":FILE mon-macs.el" 
          "mon-macs.el")
  :group 'mon-base)


;;; ==============================
;;; :CHANGESET 2392
;;; :CREATED <Timestamp: #{2011-01-14T16:56:04-05:00Z}#{11025} - by MON KEY>
(defcustom *mon-macs-xrefs* 
  '(mon-error-protect %mon-format-chk-keys handler-case mon-copy-list-mac
  mon-mapcar-mac mon-nshuffle-vector mon-list-sift mon-foreach mon-for mon-loop
  mon-equality-for-type mon-gensym mon-with-gensyms mon-with-print-gensyms
  defconstant defparameter mon-set-text-properies-region mon-get-face-at-posn
  mon-with-file-buffer mon-buffer-exists-p mon-with-buffer-undo-disabled
  mon-print-in-buffer-if-p mon-with-inhibit-buffer-read-only
  mon-toggle-restore-llm mon-naf-mode-toggle-restore-llm mon-line-dolines
  mon-cat *mon-macs-xrefs*)
  "Xrefing list of `mon-*' macros.\n
The symbols contained of this list are defined in :FILE mon-macs.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-keybindings-xrefs*',
`*mon-testme-utils-xrefs*', `*mon-button-utils-xrefs*',
`*mon-buffer-utils-xrefs*', `*mon-error-utils-xrefs*', `*mon-line-utils-xrefs*',
`*mon-macs-xrefs*', `*mon-plist-utils-xrefs*', `*mon-post-load-hooks-xrefs*', 
`*mon-seq-utils-xrefs*', `*mon-string-utils-xrefs*', `*mon-type-utils-xrefs*',
`*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*', `*mon-slime-xrefs*',
`*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*', `*mon-ulan-utils-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-macs
  :group 'mon-xrefs)

;;; ==============================
;;; :NOTE Appears deceptively simple :O
;;; :CHANGESET 2141
;;; :CREATED <Timestamp: #{2010-09-17T19:17:27-04:00Z}#{10375} - by MON KEY>
(defmacro mon-error-protect (err-arg &rest body)
  "Signal an error inside a `condition-case' form but ignore it momentarily.\n
The \"protected\" condidtion-case BODYFORM is evaluated with `ignore-errors'
non-nil for the duration of BODY.\n
When body returns normally (i.e. no errors were signaled during body) protect when
`error` is the default handler.\n
ERR-ARG is the form returned from the error condition handler.\n
:EXAMPLE\n\n\(let \(pre-error\) ;; Value we want to ensure executes inside BODY
  ;; If an error is signaled in body let us know, but delay
  \(prog1 \(when \(eq \(mon-error-protect 
                    'error-but-body-executed
                    \(progn 
                      \(setq pre-error \"pre-error got set\"\)
                      \(error \"\"\)\)\) 'error-but-body-executed\)
           'error-but-body-executed\)
    ;; First show what happened inside body:
    \(minibuffer-message pre-error\)\)\)\n
\(mon-error-protect-PP-EXPAND-TEST 
 '\(mon-error-protect 'never-see-me\)\)\n
\(mon-error-protect-PP-EXPAND-TEST 
 '\(mon-error-protect 'will-see-me '\(/ 0 0\)\)\)\n
\(mon-error-protect-PP-EXPAND-TEST
 '\(let \(pre-error\) ;; Value we want to ensure executes inside body
  ;; If an error is signaled in body let us know, but delay
  \(prog1 \(when \(eq \(mon-error-protect 
                    'error-but-body-executed
                    '\(progn 
                      \(setq pre-error \"pre-error got set\"\)
                      \(error \"\"\)\)\) 'error-but-body-executed\)
           'error-but-body-executed\)
    ;; First show what happened inside body:
    \(minibuffer-message pre-error\)\)\)\)\n
:SEE-ALSO `stack-trace-on-error', `report-errors', `mon-error',
`mon-error-toplevel',`mon-error-gather', `mon-error-gather-peek',
`mon-error-string-err-format', `mon-message', `redirect-debugging-output',
`external-debugging-output', `debug-on-signal', `debug-on-error',
`debug-ignored-errors', `signal-hook-function', `*mon-emacs-help-errors*',
`mon-help-errors', `Info-no-error', `mon-help-CL-error-condition-restart'.\n►►►"
  (let ((bdy-wrap (make-symbol "bdy-wrap")))
    `(let ((,bdy-wrap (or ,@body t)))
       (condition-case nil
        (unless (ignore-errors (or (null (eval ,bdy-wrap)) t))
          (error ""))
      (error ,err-arg)))))
;;
;; (put 'mon-error-protect 'lisp-indent-function <INT>) 

;;; ==============================
;;; :TODO Add additinal keyword spec :w-stream
;;; :CHANGESET 2353
;;; :CREATED <Timestamp: #{2010-12-04T19:58:17-05:00Z}#{10486} - by MON KEY>
(defmacro %mon-format-chk-keys (w-keys-lst)
  "Helper macro `mon-format' map and verify keyword arguments.\n
Return a 4 elt list of values extracted from W-KEYS.\n
Arg W-KEYS is a list of are keywords/value pair arguments to `mon-format'.
Valid keywords are:\n
 :w-fun :w-spec :w-args :w-delim\n
:EXAMPLE\n
\(pp-macroexpand-expression
  '\(%mon-format-chk-keys '\(:w-fun 
                         :w-spec \":bubba bobby %d\\n\"
                         :w-args 8 
                         :w-delim t\)\)\)\n
\(pp-macroexpand-expression
 '\(%mon-format-chk-keys '\(:w-spec \":bubba bobby %d\\n\"
                         :w-args 8 
                         :w-delim t 
                         :w-fun 'format\)\)\n
\(pp-macroexpand-expression
 '\(%mon-format-chk-keys '\(:w-spec :w-args :w-delim :w-fun #'format\)\)\)\n
;; Following is with :w-delim without an value it is correctly ignored.
\(pp-macroexpand-expression
 '\(%mon-format-chk-keys
   '\(:w-delim 
     :w-spec '\(\":FUNCTION `mon-format' \" 
               \"-- example message with arg `%S' and arg `%d'\"\)
     :w-fun #'\(lambda \(&rest x\)
                \(setq x \(concat \(apply #'format x\) \"\\n\" \)\)
                \(princ x \(get-buffer-create \"*MON-FORMAT*\"\)\)
                \(display-buffer \"*MON-FORMAT*\"\)\)
     :w-args '\(bubba 666\)\)\)\)\n
;; Following is with missing :w-fun key/value pair:
\(pp-macroexpand-expression 
 '\(%mon-format-chk-keys '\(:w-spec '\(\":bubba\" \"bobby\"\) 
                          :w-args 8 
                          :w-delim t\)\)\)\n
;; Following is without key/value pairs:
\(pp-macroexpand-expression '\(%mon-format-chk-keys nil\)\)\n
:SEE-ALSO `destructuring-bind', `edebug-match-&key', `lambda-list-keywords'.\n►►►"
  (declare (indent 0) (debug t))
  (let ((mf-keys (make-symbol "--mf-keys--"))
        (mf-chk-keys '(:w-fun :w-spec :w-args :w-delim))) ;; :w-stream
    `(let ((,mf-keys 
            (mapcar #'(lambda (mf-L-0a)
                        (and (setq mf-L-0a (memq mf-L-0a ,w-keys-lst)) ;;,@w-keys-lst))
                             (not (memq (cadr mf-L-0a) (quote ,mf-chk-keys)))
                             (cadr mf-L-0a)))
                    (quote ,mf-chk-keys))))
       ,mf-keys)))
;;
;; (put '%mon-format-chk-keys 'lisp-indent-function <INT>) 

;;; ==============================
;;; :COURTESY PJB "Pascal J. Bourguignon" 
;;;  comp.lang.lisp 2010-11-29 Subject: Re: condition-case
;;; :SOURCE (URL `http://groups.google.com/group/comp.lang.lisp/msg/88ab3733d8836271?dmode=source')
(defmacro handler-case (expression &rest clauses)
  "Attempt to be like Common Lisp's `handler-case'.\n
The clause variable symbols are substituted by one single `condition-case'
variable symbol.
:NOTE This may cause problems if the same symbol is used as data or if it's a dynamic
variable.
EXPRESSION a form.
CLAUSES ::= <ERROR-CLAUSE> | <NO-ERROR-CLAUSE>
ERROR-CLAUSE ::= (typespec ([var]) {declaration}* {form}*)
NO-ERROR-CLAUSE ::= (:no-error LAMBDA-LIST {declaration}* {form}*)
:EXAMPLE\n\n
:SEE-ALSO `mon-error-protect', `stack-trace-on-error'.\n►►►"
  ;;  (declare (indent 2) (debug t))
  (let* ((var (edebug-gensym "--mon-")) ;; :WAS `gensym'
         ;; :WAS (neclause (assoc :NO-ERROR clauses))         
         ;; Use `assoc-string' b/c we can catch either :no-error or :NO-ERROR         
         (neclause (assoc-string :no-error clauses t))         
         (nell     (cadr neclause))
         (nebody   (cddr neclause))
         (handlers (mapcar #'(lambda (clause)
                               (let ((typespec (car clause))
                                     (clausvar (cadr clause))
                                     (body     (cddr clause))
                                     ;; )
                                     ;; This is `cl-do-subst' which `subst' calls unless arg CL-OLD is
                                     ;; `floatp'.. and its not here.
                                     (cds-L-1 #'(lambda (cds-L-1-new cds-L-1-old cds-L-1-tree)
                                               (cond ((eq cds-L-1-tree cds-L-1-old) cds-L-1-new)
                                                     ((consp cds-L-1-tree)
                                                      (with-no-warnings
                                                        (let ((cds-L-1-a 
                                                             (apply cds-L-1 (list cds-L-1-new cds-L-1-old (car cds-L-1-tree))))
                                                            (cds-L-1-d 
                                                             (apply cds-L-1 (list cds-L-1-new cds-L-1-old (cdr cds-L-1-tree)))))
                                                        (if (and (eq cds-L-1-a (car cds-L-1-tree)) (eq cds-L-1-d (cdr cds-L-1-tree)))
                                                            cds-L-1-tree (cons cds-L-1-a cds-L-1-d)))))
                                                     (t cds-L-1-tree))))
                                     )
                                 (cons (if (and (consp typespec)
                                                (eq 'or (car typespec)))
                                           (cdr typespec)
                                         typespec)
                                       (if (null clausvar)
                                           body
                                         ;; 
                                         ;; :WAS  (subst  var (car clausvar) body)))))
                                         ;;
                                         (apply cds-L-1 (list var (car clausvar) body)) ))))
                           (remove neclause clauses))))
    (if neclause
        `(condition-case ,var
             (multiple-value-bind ,nell ,expression ,@nebody)
           ,@handlers)
      `(condition-case ,var
           ,expression
         ,@handlers))))
;;
;; (put 'handler-case 'lisp-indent-function <INT>) 

 
;;; ==============================
;;; :PREFIX "mclm-"
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-21T16:48:46-04:00Z}#{10382} - by MON KEY>
(defmacro mon-copy-list-mac (cpy-lst)
  "Macrofied version of `copy-list'/`copy-tree'.\n
Return a copy of list, which may be a dotted list.\n
Elements of list are not copied, just the list structure itself.\n
:EXAMPLE\n\n\(mon-copy-list-mac '\(a nil . \(c . d\)\)\)\n
\(mon-copy-list-mac '\(a b \(nil . \(c . d\)\)\)\)\n
\(mon-copy-list-mac '\(a b \(c . d\)\)\)\n
\(mon-copy-list-mac [nil 3]\)\n
\(mon-copy-list-mac '\(a b . \(c . d\)\)\)\n
\(pp-macroexpand-expression '\(mon-copy-list-mac '\(a b \(c . d\)\)\)\)\n
\(pp-macroexpand-expression '\(mon-copy-list-mac '\(a b . \(c d\)\)\)\)\n
\(pp-macroexpand-expression '\(mon-copy-list-mac '\(a b . \(c . d\)\)\)\)\n
:NOTE `copy-sequence' signals an error when we try to copy the dotted list.\n
CL `copy-list' returns a copy of CPY-LST even when a dotted list as does
`copy-tree', but the latter copies recursively along cdrs with additional checks
for vectors which we don't need/want.\n So, to avoid byte compiler warnings for
the `mon-map1' fncns we will use this litle fella instead.\n
Following checks help verify that list copies returned from `mon-copy-list-mac' and
`copy-tree' are functionaly equivalent:\n
\(let \(\(cp '\(a b . \(nil . \(c . d\)\)\)\) ;≣ \(a b nil c . d\)
      cp-mac cp-tree cp-lst-CL\)
  \(setq cp-mac    \(mon-copy-list-mac cp\)\)
  \(setq cp-tree   \(copy-tree cp\)\)
  \(setq cp-lst-CL \(copy-list cp\)\)
  `\(:macro-vrsn ,\(progn \(setcdr \(cdddr cp-mac\) \"bubba mac\"\) cp-mac\)
    :tree-vrsn  ,\(progn \(setcdr \(cdddr cp-tree\) \"bubba tree\"\) cp-tree\)
    :CL-vrsn    ,\(progn \(setcdr \(cdddr cp-lst-CL\) \"bubba CL\"\) cp-lst-CL\)
    :orig-dttd-l ,cp \)\)\n
\(let \(chk-equal\)
  \(setq chk-equal
        `\(,\(let \(\(mac-cmplx-strct '\([a nil] . \(nil . [c \(d . [q z]\)]\)\)\)
                 mac-aset-cmplx\)  
             \(setq mac-aset-cmplx \(mon-copy-list-mac mac-cmplx-strct\)\)
             \(aset \(cdr \(aref \(cddr mac-aset-cmplx\) 1\)\) 1 'bubba-aset\)
             `\(:asetd-mac ,mac-aset-cmplx :orig ,mac-cmplx-strct\)\)
          ,\(let \(\(tree-cmplx-strct '\([a nil] . \(nil . [c \(d . [q z]\)]\)\)\)
                 tree-aset-cmplx\)
             \(setq tree-aset-cmplx \(mon-copy-list-mac tree-cmplx-strct\)\)
             \(aset \(cdr \(aref \(cddr tree-aset-cmplx\) 1\)\) 1 'bubba-aset\)
             `\(:asetd-tre ,tree-aset-cmplx :orig ,tree-cmplx-strct\)\)
          ,\(let \(\(mac-setf-cmplx '\([a nil] . \(nil . [c \(d . [q z]\)]\)\)\)
                 mac-setfd\)
             \(setq mac-setfd \(mon-copy-list-mac mac-setf-cmplx\)\)
             \(setf  \(aref \(cdr \(aref \(cddr mac-setfd\) 1\)\) 1\) 'bubba-setfd\)
             `\(:setfd-mac ,mac-setfd :orig ,mac-setf-cmplx\)\)
          ,\(let \(\(tre-setf-cmplx '\([a nil] . \(nil . [c \(d . [q z]\)]\)\)\)
                 tree-setfd\)
             \(setq tree-setfd \(mon-copy-list-mac tre-setf-cmplx\)\)
             \(setf \(aref \(cdr \(aref \(cddr tree-setfd\) 1\)\) 1\) 'bubba-setfd\)
             `\(:setfd-tre ,tree-setfd :orig ,tre-setf-cmplx\)\)\)\)
  \(setq chk-equal  `\(:equal-tre-aset-orig 
                     ,\(equal \(cadr  \(assq :asetd-tre chk-equal\)\)
                             \(cadddr  \(assq :asetd-tre chk-equal\)\)\)
                     :equal-mac-aset-orig
                     ,\(equal \(cadr    \(assq :asetd-mac chk-equal\)\)
                             \(cadddr  \(assq :asetd-mac chk-equal\)\)\)
                     :equal-asetd-mac-orig-tre-orig
                     ,\(equal \(cadddr  \(assq :asetd-tre chk-equal\)\)
                             \(cadddr  \(assq :asetd-mac chk-equal\)\)\)
                     :equal-tre-setfd-orig
                     ,\(equal \(cadr  \(assq :setfd-tre chk-equal\)\)
                             \(cadddr  \(assq :setfd-tre chk-equal\)\)\)
                     :equal-mac-setfd-orig
                     ,\(equal \(cadr  \(assq :setfd-mac chk-equal\)\)
                             \(cadddr  \(assq :setfd-mac chk-equal\)\)\)
                     :equal-mac-setfd-orig-tre-setfd-orig
                     ,\(equal \(cadddr  \(assq :setfd-tre chk-equal\)\)
                             \(cadddr  \(assq :setfd-mac chk-equal\)\)\)
                     ,@chk-equal\)\)\)\n
:SEE-ALSO `mon-mapl', `mon-maplist',  `mon-mapcar', `mon-mapcan', `mon-mapcon',
`mon-map'.\n►►►"
  ;; (declare (indent 1) (debug t))
  (let ((mclm-res (make-symbol "mclm-res"))
        (mclm-cpy (make-symbol "mclm-cpy")))
    `(let ((,mclm-cpy ,cpy-lst)
           (,mclm-res nil))
       (if (consp ,mclm-cpy)
           (if (mon-list-proper-p ,mclm-cpy)
               ;; Could also check `vectorp' here and coerce with:
               ;; (append <VECTOR> nil) which might allow extending elisp
               ;; handling of vectors to the cl mapping fncns but would need to
               ;; record that it occured so we could coerce it back to a vector
               ;; once finished...
               ;; :NOTE Or, use `mon-sequence-mappable-p'
               (copy-sequence ,mclm-cpy)
             (progn
               (while (consp ,mclm-cpy) (push (pop ,mclm-cpy) ,mclm-res))
               (prog1 (nreverse ,mclm-res) (setcdr ,mclm-res ,mclm-cpy))))
         (car ,mclm-cpy)))))
;;
;; (put 'mon-copy-list-mac 'lisp-indent-function <INT>) 

;;; ==============================
;;; :PREFIX "mmc-"
;;; :COURTESY :FILE gnus/gnus-util.el :WAS `gnus-mapcar'
;;; :CHANGESET 2112
;;; :CREATED <Timestamp: #{2010-09-06T16:36:17-04:00Z}#{10361} - by MON KEY>
(defmacro mon-mapcar-mac (mapmac-fun seq1 &rest seqs2_n)
  "Apply MAPMAC-FUN to each element of SEQ1 and make a list of the results.\n
If there are several &rest sequences, MAPMAC-FUN is called with that many arguments.
Mapping terminates with the shortest sequence.\n
With just one sequence, this is like `mapcar'.\n
With several, it is like the Common Lisp `mapcar' function extended to arbitrary
sequence types.\n
:EXAMPLE\n\n\(mon-mapcar-mac 'list '\(a b c\) '\(1 2 3\) '\(d e f\) '\(4 5 6\)\)\n
\(mon-mapcar-mac 'cons '\(a b c\) '\(1 2 3\)\)\n
\(pp-macroexpand-expression '\(mon-mapcar-mac 'cons '\(a b c\) '\(1 2 3\)\)\)\n
\(let* \(\(tmp-alst '\(\(KEY-D D0 D1 D2\)\)\)
       \(tmp-keys '\(KEY-A KEY-B KEY-C\)\)
       \(tmp-vals '\(\(A0 A1 A2\) \(B0 B1 B2\) \(C0 C1 C2\)\)\)
       \(rtn-mon-mapcar tmp-alst\)
       rtn-pairlis\)
  \(setq rtn-mon-mapcar \(nconc \(mon-mapcar #'cons tmp-keys tmp-vals\) tmp-alst\)\)
  \(setq rtn-pairlis \(pairlis tmp-keys tmp-vals tmp-alst\)\)
  `\(:TREE-EQUAL-MMC-PAIRL ,\(tree-equal rtn-mon-mapcar rtn-pairlis\)
    :MON-MAPCAR/PAIRLIS ,rtn-mon-mapcar :CL-PKG/PAIRLIS ,rtn-pairlis\)\)\n
:NOTE Last example is basically Emacs lisp's version of Common Lisp's `parilis'.\n
:SEE-ALSO `mon-mapcar', `mon-map1', `mon-mapcan', `mon-mapcon', `mon-mapl',
`mon-maplist', `mon-maptree', `mon-map-combine', `mon-map-append', `mon-map'.\n►►►"
  ;; (declare (indent 1) (debug t))
  (if (not seqs2_n)
      `(mapcar ,mapmac-fun ,seq1)
    (let* ((mmc-seqs      (cons seq1 seqs2_n))
           (mmc-cnt       0)
           (mmc-heads     (mapcar 
                           ;; :NOTE arg MMC-L-1 prevents backquote expansion to: "(lambda nil"
                           #'(lambda (mmc-L-1) 
                               (make-symbol (concat "--mmc-L-1_" 
                                                    (number-to-string (setq mmc-cnt (1+ mmc-cnt))))))
                           mmc-seqs))
           (mmc-rslt      (make-symbol "mmc-rslt"))
           (mmc-rslt-tl   (make-symbol "mmc-rslt-tl")))
      `(let* ,(let* ((mmc-bind  (cons nil nil))
                     (mmc-heads  mmc-heads))
                (nconc mmc-bind (list (list mmc-rslt '(cons nil nil))))
                (nconc mmc-bind (list (list mmc-rslt-tl mmc-rslt)))
                (while mmc-heads
                  (nconc mmc-bind (list (list (pop mmc-heads) (pop mmc-seqs)))))
                (cdr mmc-bind))
         (while (and ,@mmc-heads)
           (setcdr ,mmc-rslt-tl 
                   (cons (funcall ,mapmac-fun ,@(mapcar #'(lambda (mmc-L-2)
                                                     (list 'car mmc-L-2))
                                                      mmc-heads)) 
                    nil))
           (setq ,mmc-rslt-tl (cdr ,mmc-rslt-tl)
                 ,@(apply #'nconc 
                          (mapcar #'(lambda (mmc-L-3) 
                                      (list mmc-L-3 (list 'cdr mmc-L-3))) 
                                  mmc-heads))))
         (cdr ,mmc-rslt)))))
;;
;;; (put 'mon-mapcar-mac 'lisp-indent-function <INT>) 

;;; ==============================
;;; :PREFIX "mnsv-"
;;; :NOTE A macro'd version of `shuffle-vector' :FILE lisp/play/cookie1.el
;;; :CREATED <Timestamp: #{2010-07-31T11:34:53-04:00Z}#{10306} - by MON>
(defmacro mon-nshuffle-vector (mixup-vector)
  "Destructive random permutation of MIXUP-VECTOR elts, return MIXUP-VECTOR.\n
All permutations are equally likely.\n
:EXAMPLE\n\n\\(pp-macroexpand-expression 
 '\(mon-nshuffle-vector [37 41 43 47 53 59]\)\)\n
:ALIASED-BY `nshuffle-vector'\n
:SEE-ALSO `mon-list-nshuffle', `mon-list-shuffle-safe',`shuffle-vector',
`slime-shuffle-list'.\n►►►"
  ;; :NOTE This is called repeatedly by `mon-*-gensym' procedures so we
  ;; need to gensym the local vars by hand.
  (declare (indent 0) (debug t))
  (let ((mnsv-vec    (make-symbol "mnsv-vec"))
        (mnsv-incr   (make-symbol "mnsv-incr"))
        (mnsv-rndmz  (make-symbol "mnsv-rndr"))
        (mnsv-temp   (make-symbol "mnsv-temp"))
        (mnsv-len    (make-symbol "mnsv-len")))
    `(let ((,mnsv-vec ,mixup-vector)
           (,mnsv-incr 0)
           (,mnsv-len (length ,mixup-vector))
           ,mnsv-rndmz
           ,mnsv-temp)
       (while (< ,mnsv-incr ,mnsv-len)
         (setq ,mnsv-rndmz (+ ,mnsv-incr (random (- ,mnsv-len ,mnsv-incr))))
         (setq ,mnsv-temp (aref ,mnsv-vec ,mnsv-incr))
         (aset ,mnsv-vec ,mnsv-incr (aref ,mnsv-vec ,mnsv-rndmz))
         (aset ,mnsv-vec ,mnsv-rndmz ,mnsv-temp)
         (setq ,mnsv-incr (1+ ,mnsv-incr)))
       ,mnsv-vec)))
;;
;; (put 'mon-nshuffle-vector 'lisp-indent-function <INT>) 
;;
;;; (pp-macroexpand-expression '(mon-nshuffle-vector [37 41 43 47 53 59]))

;;; ==============================
;;; :PREFIX "mls-"
;;; :COURTESY Geoff Summerhayes comp.lang.lisp :WAS `sift-list'
;;; :DATE Tue, 21 May 2002 18:41:19 GMT
;;; :SUBJECT Re: Stumped (basic LISP question)
(defmacro mon-list-sift (sift-list &rest sift-tests)
  "SIFT-LIST with SIFT-TESTS.\n
On a Common Lisp return is as if by values.\n
:EXAMPLE\n
\(mon-list-sift '\( 1 2 3 4 5 6 7 8 9 10\) #'\(lambda \(x\) \(> x 4\)\)\)
;=> \(10 9 8 7 6 5\) \(4 3 2 1\)\n
\(mon-list-sift '\(1 2 3 -1 -2 -3\) #'oddp #'plusp\)
;=> \(-3 -1 3 1\) \(2\) \(-2\)\n
\(mon-list-sift '\(1 2 3 -1 -2 -3\) #'plusp #'oddp\)
;=> \(3 2 1\) \(-3 -1\) \(-2\)\n
:SEE-ALSO `mon-list-filter', `mon-list-last', `mon-delete-first',
`mon-equality-for-type', `mon-equality-or-predicate', `car-less-than-car'.\n►►►"
  ;; Common Lisp version, note the functional `values' in the tail:
  ;; (defmacro list-sift (sift-list &rest sift-tests)
  ;; (let ((mls-gthr-sftd (mapcar #'(lambda (x) (declare (ignore x))
  ;;       				(gensym)) sift-tests))
  ;;       (sft-last (gensym)))
  ;;   `(let (,@mls-gthr-sftd ,sft-last)
  ;;      (dolist (sft-itm ,sift-list)
  ;;        (cond ,@(mapcar #'(lambda (sft-x sft-y)
  ;;                            `((funcall ,sft-x sft-itm) (push sft-itm ,sft-y)))
  ;;                        sift-tests mls-gthr-sftd)
  ;;              (t (push sft-itm ,sft-last))))
  ;;      (values ,@mls-gthr-sftd ,sft-last))))
  ;; (declare (indent 1) (debug t))
  (let ((mls-gthr-sftd (mon-mapcar #'(lambda (mls-L-1) 
                                       (edebug-gensym "--mon-sift-cllct--")) 
                                   sift-tests))
        (mls-sft-last (edebug-gensym "--mon-sift-last--")))
    `(let (,@mls-gthr-sftd ,mls-sft-last)
       (dolist (mls-sft-itm ,sift-list)
         (cond ,@(mon-mapcar #'(lambda (mls-L-2 mls-l-3)
                                 `((funcall ,mls-L-2 mls-sft-itm) 
                                   (push mls-sft-itm ,mls-l-3)))
                             sift-tests mls-gthr-sftd)
               (t (push mls-sft-itm ,mls-sft-last))))
       (list ,@mls-gthr-sftd ,mls-sft-last))))
;;
;; (put 'mon-list-sift 'lisp-indent-function <INT>) 

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-utilities.el :WAS foreach
(defmacro mon-foreach (w-var on-list &rest body)
  "A foreach style macro idiom for looping W-VARS ON-LIST with BODY.\n
:EXAMPLE\n\n\(mon-foreach for-var                 ; <- w-var
             '\(1 2 3 4)              ; <- on-list
             \(+ for-var for-var\)\)    ; <- body\n
\(pp-macroexpand-expression 
 '\(mon-foreach for-var '\(1 2 3 4\) \(+ for-var for-var\)\)\)\n
:SEE-ALSO `mon-for', `mon-loop', `mon-mapcar'.\n►►►"
  ;; (declare (indent 1) (debug t))
  `(mapcar #'(lambda (,w-var) ,@body) ,on-list))
;;
;; (put 'mon-foreach 'lisp-indent-function <INT>) 

;;; ==============================
;;; :PREFIX "mf-"
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-utilities.el :WAS `for'
(defmacro mon-for (var init final &rest body)
  "Execute a simple for loop .\n
:EXAMPLE\n\n\(mon-for i  1  10  \(print i\)\)\n
\(pp-macroexpand-expression '\(mon-for i 1 10  \(print i\)\)\)\n
:SEE-ALSO `mon-foreach', `mon-loop', `mon-mapcar'.\n►►►"
  ;; (declare (indent 1) (debug t))
  (let ((mf-tempvar (make-symbol "mf-tempvar")))
    `(let ((,var ,init)
           (,mf-tempvar ,final))
       (if (< ,var ,mf-tempvar)
           (while (<= ,var ,mf-tempvar)
             ,@body
             (setq ,var (+ ,var 1)))
         (while (>= ,var ,mf-tempvar)
           ,@body
           (setq ,var (- ,var 1)))))))
;;
;; (put 'mon-for 'lisp-indent-function <INT>) 

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `rloop'
;;; :MODIFICATIONS <Timestamp: #{2009-09-29T13:35:36-04:00Z}#{09402} - by MON KEY>
(defmacro mon-loop (clauses &rest body)
  "Macro to execute a loop over clauses.\n
:SEE-ALSO `mon-foreach', `mon-for', `mon-mapcar'.\n►►►"
  ;; (declare (indent 2) (debug t))
  (if (null clauses)
      `(progn ,@body)
    `(loop ,@(car clauses) do (mon-loop ,(cdr clauses) ,@body))))
;;
;; (put 'mon-loop 'lisp-indent-function <INT>) 

 
;;; ==============================
;;; PREFIX "meft-"
;;; :CHANGESET 2233
;;; :CREATED <Timestamp: #{2010-11-05T22:11:35-04:00Z}#{10445} - by MON KEY>
(defmacro mon-equality-for-type (thing)
  "Return preferred equality predicate for thing.\n
:EXAMPLE\n\n\(mon-equality-for-type 3.3\)
\(pp-macroexpand-expression '\(mon-equality-for-type \"string\"\)\)
:SEE-ALSO `mon-equality-or-predicate',
`*mon-equality-or-predicate-function-types*'
`mon-get-text-properties-parse-prop-val-type-chk', `deftype', `type-of',
`typep', `coerce'.\n►►►"
  (declare (indent 0) (debug t))
  (let ((meft-thing (make-symbol "meft-thing")))
    `(let ((,meft-thing (type-of ,thing)))
       (or 
        (and (memq ,meft-thing '(integer symbol buffer
                                 frame window marker char-table
                                 hash-table subr ))
             'eq)
        (and (memq ,meft-thing '(cons string vector bool-vector char-table)) 'equal))
       ;; (get 'eql 'byte-compile) (get 'eql 'cl-compiler-macro)
       (and (eq ,meft-thing 'float) 'eql))
    ;; window-configuration
    ;; frame-configuration
    ;; overlay
    ;; compiled-function
    ;; `cl-make-type-test' has:
    ;; ((eq type 'real) `(numberp ,val))
    ;; ((eq type 'fixnum) `(integerp ,val))
    ;; (face `face-equal', `internal-lisp-face-equal-p'
    ))
;;
;; (put 'mon-equality-for-type 'lisp-indent-function <INT>) 


;;; ==============================
;;; :PREFIX "mgs-"
;;; :CREATED <Timestamp: #{2010-07-29T15:07:15-04:00Z}#{10304} - by MON>
(defmacro mon-gensym (&optional prefix counter)
  "Generate a new uninterned symbol.\n
When optional arg PREFIX (a string) return a symbol-name by appending the value
of `*gensym-counter*' to PREFIX. The default prefix is \"M\".\n
When optional arg COUNTER satisfies the predicate `integerp' and PREFIX
satisfies the predicate `stringp' it is appended to PREFIX instead of
`*gensym-counter*'s value.\n
Like the `gensym' function in CL package but defined as a macro instead.\n
:EXAMPLE\n\n\(pp-macroexpand-expression '\(mon-gensym\)\)\n
\(pp-macroexpand-expression '\(mon-gensym \"EG\" 666\)\)\n
:NOTE `edebug-gensym' is identical to the `gensym' but doesn't sigal a
byte-compiler warning.\n
:SEE-ALSO `mon-gensym-counter-randomizer', `mon-with-gensyms',
`mon-gensym-counter-randomizer-TEST', `edebug-gensym'
`easy-menu-make-symbol', `easy-menu-item-count'.\n►►►"
  (declare (indent 0) (debug t))
  (let ((mgs-pfix (make-symbol "mgs-pfix"))
        (mgs-num  (make-symbol "mgs-num"))) ;; (print-gensym t))
    `(let ((,mgs-pfix (cond ((and ,prefix (stringp ,prefix)) ,prefix)
                           ((and ,prefix (not (stringp ,prefix)))
                            (symbol-name ,prefix))
                           (t "M")))
           (,mgs-num  (if (and ,prefix ,counter (integerp ,counter))
                         ,counter
                       (prog1 
                           edebug-gensym-index ;; :WAS *gensym-counter* 
                         (incf edebug-gensym-index))))) ;; :WAS *gensym-counter*)))))
       (make-symbol (format "%s%d" ,mgs-pfix ,mgs-num)))))
;;
;;; (put 'mon-gensym 'lisp-indent-function <INT>) 

;;; ==============================
;;; :NOTE This appears to work similiarly to Lars Brinkhoff's version but
;;;  without the CL requirements for `assert' `every' and `mapcar*'.
;;; :SEE (URL `http://www.emacswiki.org/emacs/macro-utils.el')
;;;   (assert (every #'symbolp symbols))
;;;    `(let ,(mapcar* #'list symbols '#1=((mon-gensym) . #1#))
;;;      ,@body))
;;; :CREATED <Timestamp: #{2010-07-29T20:09:35-04:00Z}#{10304} - by MON>
(defmacro mon-with-gensyms (w-syms &rest body)
  "Execute BODY in a context where the variables in W-SYMS are bound to
freshly allocated uninterned symbol as returned by `mon-gensym'.\n
:EXAMPLE\n\n\(unwind-protect
    \(progn
      \(defmacro tt--mgs \(arg1 arg2 arg3\)
        \(mon-with-gensyms 
          \(somea someb somec get-some\)
          `\(let \(\(,somea ,arg1\)
                 \(,someb ,arg2\)
                 \(,somec ,arg3\)
                 \(,get-some \(\)\)\)
             \(dolist \(q \(list ,somea ,someb ,somec\)
                        \(setq ,get-some 
                              \(mapconcat 'identity \(nreverse ,get-some\) \"\\n\"\)\)\)
               \(push \(concat \"a name: \" q \) ,get-some\)\)
             \(and \(null \(fboundp 'tt--mgs\)\)
                  \(equal ,get-some \"a name: bubba\\na name: sally\\na name: suzy\"\)\)\)\)\)
       \(pp-macroexpand-expression '\(tt--mgs \"bubba\" \"sally\" \"suzy\"\)\)\)
  \(progn \(fmakunbound 'tt--mgs\) \(unintern 'tt--mgs\)\)\)\n
:NOTE `edebug-gensym' is identical to the `gensym' but doesn't sigal a CL
byte-compiler warning.\n
:ALIASED-BY `with-gensyms'\n
:SEE-ALSO `mon-gensym-counter-randomizer', `mon-gensym', `mon-with-print-gensyms'.\n"
  (declare (indent 0) (debug t))
  `(let ,(mapcar #'(lambda (mwg-mks) 
                     `(,mwg-mks (mon-gensym 
                                  ,(symbol-name mwg-mks) ;;,(format "%s" mks)
                                  ,(mon-gensym-counter-randomizer mwg-mks)))) w-syms)
     ,@body))
;;
;;; (put 'mon-with-gensym 'lisp-indent-function <INT>)
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;|
;;| (unwind-protect
;;|     (progn
;;|       (defmacro tt--mgs (arg1 arg2 arg3)
;;|         (mon-with-gensyms 
;;|           (somea someb somec get-some)
;;|           `(let ((,somea ,arg1)
;;|                  (,someb ,arg2)
;;|                  (,somec ,arg3)
;;|                  (,get-some ()))
;;|              (dolist (q (list ,somea ,someb ,somec)
;;|                         (setq ,get-some 
;;|                               (mapconcat 'identity (nreverse ,get-some) "\n")))
;;|                (push (concat "a name: " q ) ,get-some))
;;|              (and (null (fboundp 'tt--mgs))
;;|                   (equal ,get-some "a name: bubba\na name: sally\na name: suzy")))))
;;|        (pp-macroexpand-expression '(tt--mgs "bubba" "sally" "suzy")))
;;|   (progn (fmakunbound 'tt--mgs) (unintern 'tt--mgs)))
;;|
;;`----

;;; ==============================
;;; :CHANGESET 2233
;;; :CREATED <Timestamp: #{2010-11-05T21:46:49-04:00Z}#{10445} - by MON KEY>
(defmacro mon-with-print-gensyms (&rest body)
  "Evaluate body with `print-gensym' bound `non-nil'.\n
:EXAMPLE\n
\(save-excursion
  \(mon-with-inhibit-buffer-read-only
      \(newline\) \(newline\)
      \(mon-with-print-gensyms
        \(princ
         \(pp-to-string
          `\(let \(was-sym\)
             \(prog2 
                 \(setq was-sym
                       \(symbol-name 
                        \(car '\(,\(make-symbol 
                                 \(format \"not-a-sym-%d\" \(random\)\)\)\)\)\)\)
                 \(and \(not \(intern-soft was-sym obarray\)\)
                      \(not \(unintern was-sym obarray\)\)\)\)\)\)
         \(current-buffer\)\)\)\)\)\n
\(pp-macroexpand-expression 
 '\(mon-with-print-gensyms
   \(let \(hld-buba\)
     \(setq hld-buba `\(,\(make-symbol \(format \"not-a-sym-%d\" \(random\)\)\)\)\)
     \(prin1 hld-buba \(current-buffer\)\)\)\)\)\n
:ALIASED-BY `with-print-gensyms'\n
:SEE-ALSO `mon-gensym', `print-circle', `read-circle', `mon-help-print-functions'.\n►►►"
  (declare (indent 0) (debug t))
  `(let ((print-gensym t))
     ,@body))
;;
;; (put 'mon-with-print-gensyms 'lisp-indent-function <INT>) 

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-cl.el :LICENSE LGPL
;;; :NOTE Could prop just defalias this but...
;;; :DOCSTRING is same as `defconstant'
;;; :CREATED <Timestamp: #{2010-01-15T15:46:09-05:00Z}#{10025} - by MON KEY>
(defmacro defconstant (symbol initvalue &optional docstring)
  ;; (declare (indent 1) (debug t))
  `(defconst ,symbol ,initvalue ,docstring))
;;
;; Now, tack on some docs.
(eval-when (compile) ;; (compile load)
  (let ((defcon-d (replace-regexp-in-string "^(fn.*)$" "" (documentation 'defconst))))
    (setq defcon-d
          (concat defcon-d
                  (mapconcat 
                   #'identity
                   '(":NOTE This is a CL compatibility feature, it macro-expands to elisp's `defconst'.\n"
                     ":EXAMPLE\n\n(pp-macroexpand-expression '(defconstant bubba-consistently \"doin' the bubba\" \"undocd\"\)\)\n"
                     ":SEE info node `(CL)Porting Common Lisp'.\n"
                     ":SEE-ALSO `defparameter', `defvar', `defcustom', `set-variable',"
                     "`make-local-variable', `make-variable-buffer-local', `make-symbol', `intern',"
                     "`intern-soft', `obarray', `boundp', `bound-and-true-p', `makunbound', `unintern'."
                     "►►►") "\n")))
    (put 'defconstant 'function-documentation defcon-d)))
;;
;; (put 'defconstant 'lisp-indent-function <INT>) 

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-cl.el :LICENSE LGPL
;;; :DOCSTRING paraphrased from dpansr3.
;;; :NOTE Could prop just defalias this but...
;;; :CREATED <Timestamp: #{2010-01-15T15:46:09-05:00Z}#{10025} - by MON KEY>
(defmacro defparameter (param-name &optional initial-value docstring)
  "Unconditionally assign the INITIAL-VALUE to the dynamic variable named NAME.\n
In contrast to `defvar' the defparameter macro assigns INITIAL-VALUE (if
supplied) to the dynamic variable named NAME only if NAME is not already bound.\n
If no INITIAL-VALUE is supplied, `defvar' leaves the value cell of the dynamic
variable named NAME undisturbed; if NAME was previously bound, its old value
persists, and if it was previously unbound, it remains unbound.\n
If DOCSTRING is supplied, it is attached to NAME as a documentation
string of kind variable.\n
:EXAMPLE\n\n\(pp-macroexpand-expression '\(defparameter \"tt-\"\)\)
:NOTE This is a CL compatibility feature and expands to elisp's defvar.\n
:SEE info node `(elisp)Defining Variables'\n
:SEE-ALSO `defconstant', `defconst', `defcustom', `set-variable', 
`make-local-variable', `make-variable-buffer-local', `user-variable-p',
`make-symbol', `intern', `intern-soft', `obarray', `boundp', `bound-and-true-p',
`makunbound', `unintern'.\n►►►"
  ;; (let ((dp-name "dp-name"))
  ;;   `(let ((,dp-name ,param-name))
  ;; (declare (indent 1) (debug t))
  `(progn
     (defvar ,param-name nil ,docstring)
     (setq   ,param-name ,initial-value)))
;;
;; (put 'defparameter 'lisp-indent-function <INT>) 
;;
;; ,---- :UNCOMMENT-TO-TEST
;; | (progn
;; |   (defparameter *bubba* "bubba")
;; |   (let* ((bub-s '*bubba*)
;; |          (bub-n (symbol-name bub-s))
;; |          (bub-v (symbol-value bub-s))
;; |          (msg (format (concat 
;; |                        "We just made %S the value of parameter %s.\n"
;; |                        "Now, say goodbye to %s and his %S.")
;; |                       bub-v bub-n bub-n bub-v)))
;; |     (makunbound '*bubba*) (unintern '*bubba*)
;; |     (message msg)))
;; `----

 
;;; ==============================
;;; :COURTESY slime.el :WAS `slime-propertize-region'
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-29T13:51:01-04:00Z}#{10393} - by MON KEY>
(defmacro mon-set-text-properies-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.\n
More precisely, PROPS are added to the region between the point's positions
before and after executing BODY.\n
:SEE-ALSO `mon-insert-w-text-properties'.\n►►►"
  ;; (declare (indent 1) (debug t))
  (let ((start (edebug-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))
;;
(put 'mon-set-text-properies-region 'lisp-indent-function 1)

;;; ==============================
;;; :COURTESY :FILE gnus-util.el :WAS `gnus-faces-at'
;;; Removed the Xemacs conditional added a `gensym'.
;;; :CREATED <Timestamp: #{2010-02-04T14:03:31-05:00Z}#{10054} - by MON KEY>
(defmacro mon-get-face-at-posn (position)
  "Return a list of faces at POSITION.\n
:SEE-ALSO `mon-get-face-at-point', `mon-help-faces',
`mon-help-faces-basic', `mon-help-faces-themes'.\n►►►"
  ;; (declare (indent 1) (debug t))
  (let ((mgfap-pos (make-symbol "mgfap-pos")))
    `(let ((,mgfap-pos ,position))
       (delq nil (cons (get-text-property ,mgfap-pos 'face)
		       (mapcar #'(lambda (overlay)
                                   (overlay-get overlay 'face))
                               (overlays-at ,mgfap-pos)))))))
;;
;; (put 'mon-get-face-at-posn 'lisp-indent-function <INT>) 
;;
;; :ALIASED-BY `mon-get-text-properties-face-at-posn'\n
;;
;; (unless (and (intern-soft "mon-get-text-property-face-at-posn")
;;              (fboundp 'mon-get-text-property-face-at-posn))
;;   (defalias 'mon-get-text-properties-face-at-posn 'mon-get-face-at-posn))


 
;;; ==============================
;;; :PREFIX "mwfb-"
;;; :COURTESY Raphael Van Dyck :HIS km-frames.el :WAS `with-file-buffer'
;;; :SEE (URL `http://www.algo.be/cl/KMgen/Free-KMgen.zip')
;;; :SEE (URL `http://www.algo.be/dev-logiciels.htm')
;;; :CREATED <Timestamp: #{2009-10-23T15:17:35-04:00Z}#{09435} - by MON KEY>
(defmacro mon-with-file-buffer (w-buffer-var w-file &rest body)
  "Evaluate BODY with W-BUFFER-VAR bound to buffer visiting W-FILE.\n
:EXAMPLE\n
\(let \(read-some\)
   \(with-file-buffer some-buffer some-file
    \(save-excursion
      \(set-buffer some-buffer\)
      \(mon-g2be -1\)
      \(setq read-some \(read some-buffer\)\)\)\)\)\n
:SEE-ALSO `mon-with-buffer-undo-disabled', `mon-buffer-exists-p',
`mon-buffer-written-p', `mon-buffer-exists-so-kill', `mon-print-in-buffer-if-p',
`mon-get-buffer-w-mode', `mon-get-buffer-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-string-split-buffer-name', `mon-string-split-buffer-parent-dir',
`with-current-buffer', `with-temp-file', `with-temp-buffer',
`get-file-buffer', `find-buffer-visiting', `buffer-file-truename',
`mon-help-buffer-functions'.\n►►►"
  ;; (declare (indent 1) (debug t))
  ;; :WAS (let ((file-var (make-symbol "file"))
  ;;        (buffer-already-there-p-var (make-symbol "buffer-already-there-p")))
  (let ((mwfb-fl-var (make-symbol "mwfb-fl-var")) 
        (mwfb-bfr-p  (make-symbol  "mwfb-bfr-p")))
    `(let* ((,mwfb-fl-var ,w-file)
            (,w-buffer-var (get-file-buffer ,mwfb-fl-var))
            (,mwfb-bfr-p ,w-buffer-var))
       (unless ,mwfb-bfr-p
         (setq ,w-buffer-var (find-file-noselect ,mwfb-fl-var)))
       (unwind-protect
            (progn ,@body)
         ;; If buffer was already visiting W-FILE don't kill.
         (unless ,mwfb-bfr-p
           (kill-buffer ,w-buffer-var))))))
;;
;;; (put 'mon-with-file-buffer 'lisp-indent-function <INT>) 


;;; ==============================
;;; :PREFIX "mbep-"
;;; :COURTESY :FILE gnus-util.el :WAS `gnus-buffer-exists-p'
;;; :ADDED let wrapper gensym for local var BUFF-P
;;; :CREATED <Timestamp: #{2010-02-04T14:17:59-05:00Z}#{10054} - by MON KEY>
(defmacro mon-buffer-exists-p (buffer-to-check &optional no-invert)
  "Return buffer-name of BUFFER-TO-CHECK if it exists.\n
When BUFFER-TO-CHECK is a buffer object return a string.\n
When BUFFER-TO-CHECK is a string return a buffer object.\n
When optional arg NO-INVERT is non-nil the `type-of' BUFFER-TO-CHECK is returned
without inversion.\n
:EXAMPLE\n\n\(mon-buffer-exists-p \(current-buffer\)\)\n
\(mon-buffer-exists-p \(buffer-name \(current-buffer\)\)\)\n
\(mon-buffer-exists-p \(current-buffer\) 'no-invert\)\n
\(mon-buffer-exists-p \(buffer-name \(current-buffer\)\) 'no-invert\)\n
\(with-temp-buffer \(mon-buffer-exists-p \(current-buffer\)\)\)\n
\(with-temp-buffer \(mon-buffer-exists-p \(current-buffer\) 'no-invert\)\)\n
\(pp-macroexpand-expression '\(mon-buffer-exists-p  \(current-buffer\)\)\)\n
\(prog2 \(get-buffer-create \"*BAD-IF-NOT-KILLED*\"\)
    \(mon-buffer-exists-p \"*BAD-IF-NOT-KILLED*\"\)
  \(kill-buffer \(mon-buffer-exists-p \"*BAD-IF-NOT-KILLED*\"\)\)\)\n
\(buffer-live-p
 \(prog2 \(get-buffer-create \"*BAD-IF-NOT-KILLED*\"\)
     \(mon-buffer-exists-p \"*BAD-IF-NOT-KILLED*\"\)
   \(kill-buffer \(mon-buffer-exists-p \"*BAD-IF-NOT-KILLED*\"\)\)\)\)\n
\(pp-macroexpand-expression '\(mon-buffer-exists-p \(buffer-name \(current-buffer\)\)\)\)\n
:ALIASED-BY `buffer-exists-p'\n
:SEE-ALSO `mon-buffer-exists-so-kill', `mon-with-file-buffer',
`mon-buffer-narrowed-p', `mon-buffer-sub-no-prop',
`mon-buffer-sub-no-prop-check', `mon-with-buffer-undo-disabled',
`mon-buffer-written-p', `mon-print-in-buffer-if-p',
`mon-buffer-name->kill-ring', `mon-get-buffer-w-mode',
`mon-get-buffer-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-name',
`mon-string-split-buffer-parent-dir', `with-current-buffer', `with-temp-file',
`with-temp-buffer'.\n►►►"
  (declare (indent 2) (debug t))
  (let ((mbep-bffr-p (make-symbol "mbep-bffr-p")))
    `(let ((,mbep-bffr-p ,buffer-to-check))
       (when (and ,mbep-bffr-p (buffer-live-p (get-buffer ,mbep-bffr-p)))
         (if ,no-invert ,mbep-bffr-p
           (case (type-of ,mbep-bffr-p)
             (string (get-buffer  ,mbep-bffr-p))
             (buffer (buffer-name ,mbep-bffr-p))))))))
;;
;;; (put 'mon-buffer-exists-p 'lisp-indent-function <INT>) 
;;
;;; :TEST-ME (mon-buffer-exists-p (current-buffer))
;;; :TEST-ME (mon-buffer-exists-p (buffer-name (current-buffer)))
;;; :TEST-ME (mon-buffer-exists-p (current-buffer) 'no-invert)
;;; :TEST-ME (mon-buffer-exists-p (buffer-name (current-buffer)) 'no-invert)
;;; :TEST-ME (prog2 (get-buffer-create "*BAD-IF-NOT-KILLED*") 
;;;                 (mon-buffer-exists-p "*BAD-IF-NOT-KILLED*")
;;;                 (kill-buffer (mon-buffer-exists-p "*BAD-IF-NOT-KILLED*")))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-13T11:50:55-04:00Z}#{10237} - by MON>
(defmacro mon-with-buffer-undo-disabled (&rest body)
  "Evaluate BODY in an`buffer-disable-undo' set non-nil.\n
Arg BODY occurs inside an `unwind-protect' finishing with `buffer-enable-undo'.\n
:EXAMPLE\n
\(pp-macroexpand-expression
 '\(progn
    \(mon-with-buffer-undo-disabled
     \(save-excursion
       \(insert \"bubba\"\)\)
     \(kill-line\)
     \(if \(and buffer-undo-list \(atom buffer-undo-list\)\)
         \(message \"Toggled buffer-undo-list %S\" buffer-undo-list\)
       \(message \":BUFFER-UNDO-LIST -- fail!\" buffer-undo-list\)\)
     \(sit-for 2\)\)
    \(save-excursion \(insert \"I'm the bubba that got saved\"\)\)
    \(kill-line\)
    buffer-undo-list\)\)\n
\(mon-with-buffer-undo-disabled-TEST\)\n
\(mon-with-buffer-undo-disabled-TEST 'force-fail\)\n\n
:NOTE Useful for forms which programatically `erase-buffer' contents and the undo list
is not needed.\n
:ALIASED-BY `mon-buffer-do-with-undo-disabled'\n
:SEE-ALSO `mon-with-buffer-undo-disabled-TEST', `buffer-undo-list', 
`mon-buffer-exists-p', `mon-buffer-written-p', `mon-buffer-exists-so-kill',
`mon-buffer-narrowed-p', `mon-buffer-sub-no-prop', `mon-buffer-sub-no-prop-check',
`mon-print-in-buffer-if-p', `mon-get-buffer-w-mode',
`mon-get-buffer-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-name',
`mon-string-split-buffer-parent-dir', `with-current-buffer', `with-temp-file',
`with-temp-buffer', `mon-help-buffer-functions'.\n►►►"
  (declare (indent 2) (debug t))
  `(unwind-protect
       (progn
         (buffer-disable-undo)
         ,@body)
     (buffer-enable-undo)))
;;
;;; (put 'mon-with-buffer-undo-disabled 'lisp-indent-function <INT>) 

;;; ==============================
;;; :CHANGESET 1843 <Timestamp: #{2010-06-10T16:05:22-04:00Z}#{10234} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-11-20T19:38:47-05:00Z}#{09476} - by MON KEY>
(defmacro mon-print-in-buffer-if-p (print-type form)
  "Evalutate PRINT-TYPE with FORM in current buffer.\n
Only evaluated when buffer is visiting a file that exists, is writable, and
buffer is not read-only.\n
Otherwise, just return the printed value of form.\n
:EXAMPLE\n\n\(pp-macroexpand-expression 
 '\(mon-print-in-buffer-if-p 'princ \(buffer-name\)\)\)\n
:ALIASED-BY `mon-buffer-print-in-if'\n
:SEE-ALSO `mon-buffer-exists-so-kill', `mon-buffer-written-p',
`mon-buffer-exists-p', `mon-buffer-name->kill-ring', `mon-with-file-buffer',
`mon-get-buffer-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-name',
`mon-string-split-buffer-parent-dir'.\n►►►"
  ;; (declare (indent 1) (debug t))
  (let ((mpibip-cb (make-symbol "mpibip-cb")))
    `(let ((,mpibip-cb (get-buffer (current-buffer))))
       (if (and (not (buffer-local-value 'buffer-read-only ,mpibip-cb))
                (buffer-file-name ,mpibip-cb)
                (file-writable-p (buffer-file-name ,mpibip-cb))
                (file-exists-p (buffer-file-name ,mpibip-cb)))
           (funcall ,print-type ,form ,mpibip-cb)
         (funcall ,print-type ,form)))))
;;
;;; (put 'mon-print-in-buffer-if-p 'lisp-indent-function <INT>)
;;
;;; :TEST-ME (mon-print-in-buffer-if-p 'princ (buffer-name))

;;; ==============================
;;; :PREFIX "mwibro-"
;;; :CREATED <Timestamp: #{2010-03-26T14:56:18-04:00Z}#{10125} - by MON KEY>
(defmacro mon-with-inhibit-buffer-read-only (&rest uninhibited-body)
  "Temporarily set value of `buffer-read-only' nil in `current-buffer'.\n
Execute the UNINHIBITED-BODY inside an `unwind-protect' form which restores value
of `buffer-read-only'.\n
:EXAMPLE\n\n\(with-current-buffer \(progn 
                       \(describe-function 'mon-with-inhibit-buffer-read-only\) 
                       \(get-buffer \"*Help*\"\)\)
  \(mon-with-inhibit-buffer-read-only 
      \(sit-for 1\) \(forward-line 3\)
      \(dotimes \(i 3\) 
        \(progn \(kill-line\) \(insert \"A LINE JUST DIED HERE\\n\"\) \(sit-for 1\)\)\)\)
  \(message \"buffer-read-only value in *Help* buffer rebound -> %s\"
           \(buffer-local-value 'buffer-read-only \(get-buffer \"*Help*\"\)\)\)\)\n
\(mon-with-inhibit-buffer-read-only-PP-TEST\)\n
\(mon-with-inhibit-buffer-read-only-TEST\)\n
\(mon-with-inhibit-buffer-read-only-TEST t\)\n
:SEE-ALSO `mon-inhibit-read-only', `mon-with-inhibit-buffer-read-only-TEST',
`mon-with-inhibit-buffer-read-only-PP-TEST', `mon-inhibit-modification-hooks',
`mon-inhibit-point-motion-hooks', `mon-toggle-read-only-point-motion'.\n►►►"
  (declare (indent 3) (debug t))
  (let ((mwibro-re-inhib (make-symbol "mwibro-re-inhib")))
    `(let ((,mwibro-re-inhib (buffer-local-value buffer-read-only (current-buffer))))
       (with-current-buffer (current-buffer)
         (unwind-protect        
             (progn
               (set (make-local-variable 'buffer-read-only) nil)
               ,@uninhibited-body)
           (when ,mwibro-re-inhib (set (make-local-variable 'buffer-read-only) t)))))))
;;
;;; (put 'mon-with-inhibit-buffer-read-only 'lisp-indent-function <INT>) 

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-12T13:23:06-05:00Z}#{10105} - by MON KEY>
;;; :WAS
;;; (defmacro mon-toggle-restore-llm (&optional toggle-in-buffer &rest body)
;;;   "Wrapper macro to temporarily toggle `longlines-mode' in current-buffer.\n
;;; Like `mon-naf-mode-toggle-restore-llm' but doesn't perform `naf-mode' check.
;;; :EXAMPLE\n\n\(pp-macroexpand-expression '\(mon-naf-mode-toggle-restore-llm \"bubba\"\)\)\n
;;; :SEE-ALSO `mon-is-naf-mode-and-llm-p', `mon-is-naf-mode-p'.\n►►►"
;;;   (declare (indent 1) (debug t))
;;;   (let ((llm-toggled (make-symbol "llm-toggled")))
;;;     `(let ((,llm-toggled 
;;;             (if (mon-is-naf-mode-and-llm-p) t nil)))
;;;        (when ,llm-toggled (longlines-mode 0))
;;;        (unwind-protect
;;;             ,@body
;;; 	 (when ,llm-toggled (longlines-mode 1))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-17T16:36:06-04:00Z}#{10113} - by MON KEY>
(defmacro mon-toggle-restore-llm (&optional toggle-in-buffer &rest body)
  "Wrapper macro to temporarily toggle `longlines-mode' in current-buffer.\n
When optional arg TOGGLE-IN-BUFFER is non-nil check value in that buffer and
exectute BODY there. Default is `current-buffer'.
:EXAMPLE\n\n\(pp-macroexpand-expression '\(mon-naf-mode-toggle-restore-llm \"bubba\"\)\)\n
\(progn \(get-buffer-create \"*MON-TOGGLE-RESTORE-LLM-TEST*\"\)
       \(pp-macroexpand-expression 
        '\(mon-toggle-restore-llm \"*MON-TOGGLE-RESTORE-LLM-TEST*\" 
          \(with-current-buffer \"*MON-TOGGLE-RESTORE-LLM-TEST*\" 
            \(longlines-mode\) 
           \(prog1 \(buffer-name\)\(kill-buffer \"*MON-TOGGLE-RESTORE-LLM-TEST*\"\)\)\)\)\)
       \(display-buffer \"*Pp Macroexpand Output*\"\)\)\n
:SEE-ALSO `mon-is-naf-mode-and-llm-p', `mon-is-naf-mode-p'.\n►►►"
  (declare (indent 1) (debug t))
  (let ((llm-toggled (make-symbol "llm-toggled")))
    `(let ((,llm-toggled 
            (cond (,toggle-in-buffer
                   (cond ((mon-is-naf-mode-p ,toggle-in-buffer)
                          (mon-is-naf-mode-and-llm-p ,toggle-in-buffer))
                         ((mon-buffer-longlines-mode-p ,toggle-in-buffer))))
                  ((not ,toggle-in-buffer)
                   (if (mon-is-naf-mode-p (get-buffer (current-buffer)))
                       (mon-is-naf-mode-and-llm-p (get-buffer (current-buffer)))
                       (mon-buffer-longlines-mode-p (get-buffer (current-buffer))))))))
       (when ,llm-toggled 
         (with-current-buffer 
             (or ,toggle-in-buffer (get-buffer (current-buffer)))
           (longlines-mode 0)))
       (unwind-protect
            ,@body
         (when ,llm-toggled 
           (with-current-buffer 
               (or ,toggle-in-buffer (get-buffer (current-buffer)))
             (longlines-mode 1)))))))
;;
;; (put 'mon-toggle-restore-llm 'lisp-indent-function <INT>) 
;;
;;; :TEST-ME \(progn \(get-buffer-create \"mmm\"\)
;;;                    \(pp-macroexpand-expression 
;;;                     '\(mon-toggle-restore-llm \"mmm\" 
;;;                       \(with-current-buffer \"mmm\" \(buffer-name\)\)\)\)
;;;                    \(display-buffer \"*Pp Macroexpand Output*\"\)\)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-03-17T17:48:29-04:00Z}#{10113} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-09-08T15:52:50-04:00Z}#{09372} - by MON KEY>
;; :WAS
;;; (defmacro mon-naf-mode-toggle-restore-llm (&rest body)
;;;   "Wrapper macro to temporarily toggle `longlines-mode' in `naf-mode' buffers.\n
;;; :EXAMPLE\n\n\(pp-macroexpand-expression '\(mon-naf-mode-toggle-restore-llm \"bubba\"\)\)\n
;;; :SEE-ALSO `mon-toggle-restore-llm', `mon-naf-mode-toggle-restore-llm',
;;; `mon-is-naf-mode-and-llm-p', `mon-is-naf-mode-p'.\n►►►"
;;;   (declare (indent 1) (debug t))
;;;   (let ((llm-toggled (make-symbol "llm-toggled")))
;;;     `(let ((,llm-toggled (if (mon-is-naf-mode-and-llm-p) t nil)))
;;;        (when ,llm-toggled (longlines-mode 0))
;;;        (unwind-protect
;;;             ,@body
;;; 	 (when ,llm-toggled (longlines-mode 1))))))
;;
(defmacro mon-naf-mode-toggle-restore-llm (&optional toggle-buffer &rest naf-body)
  "Wrapper macro to temporarily toggle `longlines-mode' in `naf-mode' buffers.\n
When optional arg TOGGLE-BUFFER is non-nil check value in that buffer and
exectute BODY there. Default is `current-buffer'.
:EXAMPLE\n\n\(pp-macroexpand-expression '\(mon-naf-mode-toggle-restore-llm nil \"bubba\"\)\)\n
:SEE-ALSO `mon-toggle-restore-llm', `mon-naf-mode-toggle-restore-llm',
`mon-is-naf-mode-and-llm-p', `mon-is-naf-mode-p'.\n►►►"
  (declare (indent 1) (debug t))
  `(mon-toggle-restore-llm ,toggle-buffer ,@naf-body))
;;
;; (put 'mon-naf-mode-toggle-restore-llm 'lisp-indent-function <INT>) 
;;
;;; :TEST-ME (pp-macroexpand-expression '(mon-naf-mode-toggle-restore-llm "bubba"))

 
;;; ==============================
;;; :PREFIX "mld-"
;;; :COURTESY Pascal .J Bourguignon :WAS `dolines'
;;; :CHANGESET 1773 <Timestamp: #{2010-05-26T16:14:14-04:00Z}#{10213} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-12-28T15:57:08-05:00Z}#{09531} - by MON KEY>
(defmacro* mon-line-dolines (start-end &body body)
  "Executes the body with START-END `destructoring-bind'ed to the start and
end of each line of the current-buffer in turn.\n
:EXAMPLE\n\(mon-line-dolines-TEST\)\n
:SEE (URL `http://lists.gnu.org/archive/html/help-gnu-emacs/2009-12/msg00614.html')\n
:SEE-ALSO `mon-line-dolines-TEST', `mon-line-dolines-setup-TEST'.\n►►►"
  (declare (indent 2) (debug t))
  (let ((mld-vline  (make-symbol "mld-vline"))
        (mld-sm     (make-symbol "mld-sm"))
        (mld-em     (make-symbol "mld-em")))
    (destructuring-bind (start-var end-var) start-end
      `(let ((,mld-sm (make-marker))
             (,mld-em (make-marker)))
         (unwind-protect
             ;; :WAS (progn
             (save-restriction
               (when (use-region-p) ;; (region-active-p) 
                 (narrow-to-region (region-beginning) (region-end)))
               (mon-g2be -1)
               (while (< (point) (mon-g2be 1 t))
                 (let ((,mld-vline (point)))
                   (set-marker ,mld-sm (point)) ;; (current-buffer)
                   (set-marker ,mld-em (goto-char (line-end-position))) ;; (current-buffer)
                   (let ((,start-var  (marker-position ,mld-sm))
                         (,end-var    (marker-position ,mld-em)))
                     ,@body)
                   (goto-char ,mld-vline)
                   (forward-line 1))))
           (set-marker ,mld-sm nil)
           (set-marker ,mld-em nil))
         nil))))
;;
;; (put 'mon-line-dolines 'lisp-indent-function <INT>) 

;;; ==============================
;;; :PREFIX "mc-"
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `cat'
;;; :MODIFICATIONS <Timestamp: #{2009-10-26T11:19:18-04:00Z}#{09441} - by MON>
;;; :ADDED `make-symbol', `buffer-substring-no-properties', and docstring.
(defmacro mon-cat (file &optional from-psn to-psn)
  "Return FILE contents as string - like `cat'.\n
When optional args FROM-PSN and TO-PSN are non-nil these specify what portion
of the file to return. Signal an error if this range is not accessible.\n
:EXAMPLE\n\n\(mon-cat \"~/.emacs\"\)\n
\(mon-cat \"~/.emacs\" 10 253\)\n
:SEE \(man \"cat\"\)
:SEE info node `(coreutils)cat invocation'\n
:SEE-ALSO `insert-file-contents'.\n►►►"
  ;; (declare (indent 1) (debug t))
  (let ((mc-fc (make-symbol "mc-fc")))
    `(let ((mc-fc (with-temp-buffer
                    (insert-file-contents ,file nil ,from-psn ,to-psn)
                    ;; :WAS (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)) )))
                    (mon-buffer-sub-no-prop) )))
       ;; ??? ,mc-fc))) 
       mc-fc)))
;;
;; (put 'mon-cat 'lisp-indent-function <INT>) 

;;; ==============================
(provide 'mon-macs)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-macs.el ends here
;;; EOF
