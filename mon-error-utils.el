;;; mon-error-utils.el --- extensions for conditions and error handling
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
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
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;; `mon-error-protect',
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;; `*mon-error-gather*',
;;
;; GROUPS:
;; `mon-error-warn'
;;
;; ALIASED/ADVISED/SUBST'D:
;; `write-string' -> `mon-write-string'
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
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;; *mon-function-object-types*

(declare-function mon-booleanp              "mon-utils" '(putative-boolean))
(declare-function mon-function-object-p     "mon-utils" '(fncn-sym))
(declare-function mon-mapcar                "mon-utils" '(mapcar-fun mapcar-lst &rest more-lsts))
(declare-function mon-subseq                "mon-utils" '(mon-subseq))
(declare-function mon-equality-or-predicate "mon-utils" '(predicate arg1 arg2))

;;; ==============================
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-02T12:06:51-04:00Z}#{10396} - by MON KEY>
(defgroup mon-error-warn nil
  "Customization group for mon errors and warnings.\n
:SEE-ALSO .\n►►►"
  :link '(url-link :tag ":EMACSWIKI-FILE" 
                   "http://www.emacswiki.org/emacs/mon-error-utils.el")
  :link '(emacs-library-link "mon-error-utils.el")
  :group 'mon-base)

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
:SEE-ALSO `message', `mon-error-string-err-format', `report-errors'.\n►►►"
  :type 'function
  :group 'mon-error-warn)
;;
;; (unless (and (intern-soft "*mon-message-function*")
;;              (bound-and-true-p *mon-message-function*))
;;   (setq *mon-message-function* 'message))

(declare-function mon-error-protect-PP-EXPAND-TEST "ext:mon-testme-utils.el" '(expand-form))
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
:SEE-ALSO `report-errors', `mon-error', `mon-error-toplevel',`mon-error-gather',
`mon-error-gather-peek', `mon-error-string-err-format', `mon-message',
`redirect-debugging-output', `external-debugging-output', `debug-on-signal',
`debug-on-error', `debug-ignored-errors', `signal-hook-function',
`*mon-emacs-help-errors*', `mon-help-errors', `Info-no-error',
`mon-help-CL-error-condition-restart'.\n►►►"
  (let ((bdy-wrap (make-symbol "bdy-wrap")))
    `(let ((,bdy-wrap (or ,@body t)))
       (condition-case nil
        (unless (ignore-errors (or (null (eval ,bdy-wrap)) t))
          (error ""))
      (error ,err-arg)))))

;;; ==============================
(defun mon-error-toplevel (&rest args)
  "Top lovel error MON error form.\n
When evaluated `signal's itself by passing a list containg ARGS and the error
string stored on the `*mon-error-gather*'.\n
The string on stack is retrieved via `mon-error-gather-finalize' which also
resets the stack with `mon-error-gather-reset'.\n
This function is called from `mon-error'.\n
:EXAMPLE\n\n
:SEE-ALSO `condition-case', `mon-error', `mon-error-gather',
`mon-error-gather-peek', `mon-error-string-err-format', `mon-message',
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
(put 'mon-error-toplevel 'error-conditions '(error mon-error-toplevel mon-error t))
(put 'mon-error-toplevel 'error-message ":MON-ERROR-TOPLEVEL")

;; (mon-error-toplevel (caddr err))))
;;; ==============================
;; sig-str sig-arg 
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
:SEE-ALSO `condition-case', `mon-error-toplevel' `mon-error',
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
\(mon-error-string-err-format 'subrp 88 'bubba-val\)\n
\(mon-error-string-err-format 'subrp 8.8 'bubba-val\)\n
\(mon-error-string-err-format 'not-a-fncn 88 'bubba\) ;Never see second error.\n
:SEE-ALSO `mon-error', `mon-error-toplevel',`mon-error-gather',
`mon-error-gather-peek', `mon-error-string-err-format', `mon-message',
`*mon-emacs-help-errors*', `mon-help-errors',
`mon-help-CL-error-condition-restart', `with-output-to-string', `report-errors'.\n►►►"
  ;; :NOTE As this is an error signalling routine we should assume that the
  ;; args are not always what we expect.
  ;; w-error-signaled
  (let (mesef-err-str)
    (setq mesef-err-str
          (format (concat ":FUNCTION `%s' "
                          "-- arg %s does not satisfy `stringp', got: %S")
                  (or 
                   ;; :NOTE This should intern-soft the string symbol and then
                   ;; check the value with `mon-function-object-p'
                   (and (stringp fun-name) fun-name) 
                   (and (symbolp fun-name) (fboundp fun-name) (symbol-name fun-name))
                   (let ((mesef-LT-1 `(mon-error-string-err-format fun-name ,fun-name)))
                     (error (apply #'mon-error-string-err-format mesef-LT-1))))
                  (or (and (stringp locus-arg) (upcase locus-arg))
                      (and (atom locus-arg)
                           (or (and (not (null locus-arg))
                                    (not (cadr (mon-booleanp locus-arg)))
                                    (not (number-or-marker-p locus-arg)) ;; (type-of (current-buffer))
                                    (and (symbolp locus-arg) (upcase (symbol-name locus-arg))))
                               (let ((mesef-LT-2 `(,fun-name ,(format "%S" locus-arg) ,locus-arg)))
                                 (error (apply #'mon-error-string-err-format mesef-LT-2))))))
                  got-arg-val))
    (if w-error-signaled
        (error mesef-err-str)
      mesef-err-str)))
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
;; (defun mon-error-seq-length-err-format (fun-name locus-arg wanted-arg-val got-arg-val)
;;   (format (concat ":FUNCTION `%s' "
;;                   "-- arg %s is not a sequence with length: %d, got: %d")          
;;           ))

;;; (defun mon-error-list-proper-p-err-format (fun-name locus-arg got-arg-val)
;;     (error (concat ":FUNCTION 'mon-elt->elt " 
;;                    "-- arg W-OLD-MAKE-NEW-LST does not satisfy 'mon-list-proper-p', got: %S")
;;            w-old-make-new-lst))
;;; ==============================
;; (defun mon-error-not-a-number-err-format
;; (numberp <arg>)
;; (integerp <arg>)
;; (floatp  <arg>)
;; (concat ":FUNCTION `%s' "-- arg END-W not a number") msi-fncn)


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
  (setq *mon-error-gather*))


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

(declare-function mon-booleanp-to-binary       "mon-utils")
(declare-function mon-string-or-null-and-zerop "mon-utils")
(declare-function mon-bool-vector-to-list      "mon-utils")
(declare-function mon-list-proper-p            "mon-utils")


;; (concat ":FUNCTION `%s' "
;;         "-- arg %s does not satisfy `stringp', got: %S")

;;; ==============================
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-02T11:10:54-04:00Z}#{10396} - by MON KEY>
(defun* mon-message (&key w-spec w-args w-delim (w-msg-fun *mon-message-function*))
  "Like `message' but with special support for multi-line messages.\n
Single-line messages use the echo area.\n
W-SPEC is a string of list of strings to pass to message function.
When W-SPEC is a list of strings it is coalesced as if by `mapconcat'.
and FORMAT-ARGUMENTS are as per `format's STRING and
OBJECTS arguments.\n
When FORMAT-CONTROL-STRING is a 
Optional arg W-MESSAGE-FUNCTION is a symbol naming a function to apply to
FORMAT-CONTOL-STRING's and FORMAT-ARGUMENTS when omitted defaults to value of
variable `*mon-message-function*'.\n
:EXAMPLE\n
\(mon-message :w-spec \(concat \":FUNCTION `mon-message' \" 
                             \"-- example message with arg `%s' and arg `%d'\"\)
             :w-args '(bubba 666\)\)\n
\(mon-message :w-spec '\(\":FUNCTION `mon-message' \" 
                         \"-- example message with arg `%s' and arg `%d'\"\)
             :w-args '\(bubba 666\)\)\n
\(mon-message  :w-msg-fun '\(lambda \(&rest x\) 
                              \(with-output-to-string \(princ \(apply 'format x\)\)\)\)
              :w-spec '\(\":FUNCTION `mon-message' \" \"-- `%d'\"\) 
              :w-delim \"\\n\"
              :w-args 666\)\n
\(mon-message :w-msg-fun #'\(lambda \(&rest x\) 
                              \(with-output-to-string \(princ \(apply 'format x\)\)\)\)
             :w-spec \":FUNCTION `mon-message' -- `%d'\" 
             :w-args 666\)\n
\(mon-message  :w-msg-fun #'\(lambda \(&rest x\) 
                             \(with-output-to-string \(princ \(apply 'format x\)\)\)\)
              :w-spec '\(\":FUNCTION `mon-message' \" \"bubba-%d\" \"bubba-%d\" \"bubba-%d\"\)
              :w-delim \"\\n-- \"
              :w-args \(number-sequence 8 10\) \)\n
\(mon-message :w-spec [\":FUNCTION `mon-message' \" \"bubba-%d\" \"bubba-%d\" \"bubba-%d\"]
             :w-delim \"\\n-- \"
             :w-args  \(number-sequence 8 10\)\)\n
\(mon-message :w-spec [\":FUNCTION `mon-message' \" \"bubba-%d\" \"bubba-%d\" \"bubba-%d\"]
             :w-delim \"\\n-- \"
             :w-args  `[,@\(number-sequence 8 10\)]\)\n
\(mon-message :w-msg-fun '\(lambda \(&rest x\) 
                              \(with-output-to-string \(princ \(apply 'format x\)\)\)\)
            :w-spec `\(\":FUNCTION `mon-message' \" 
                          ,\(concat \"example message\" \" with arg `%s' \"\)
                           \"and arg `%d'\"\)
            :w-delim \"  \\n-- \"
            :w-args `[bubba ,\(+ 8 666\)]\)\n
\(mon-message :w-spec \(make-bool-vector 18 t\)\)\)\n
\(mon-message :w-spec \(make-bool-vector 31 t\) :w-args '\(8 13 18 22 26\)\)\n
\(mon-message :w-spec \(make-bool-vector 18 t\) :w-delim \"\\n ---+ \"\)\n
\(mon-message :w-spec \(make-bool-vector 31 t\) :w-delim \"\\n-- \"\)\n
\(mon-message :w-spec \(make-bool-vector 31 t\) 
             :w-args [8 13 18 22 26]
             :w-delim \"\\n ^_^ \"\)\n
:SEE-ALSO `mon-display-warning'.\n►►►"
  (apply 
   (or (and (eq w-msg-fun *mon-message-function*) w-msg-fun)
       (and (memq (mon-function-object-p w-msg-fun)
                  ;; Don't bother with `apply' on macros
                  (remove 'macro *mon-function-object-types*)) 
            w-msg-fun)
       (error (concat ":FUNCTION `mon-message' " 
                      "-- keyword :W-MSG-FUN does not satisfy `mon-function-object-p', got: %S")
              w-msg-fun))
   (or 
    ;; :w-spec is `stringp' or `bool-vector-p'
    (and (or 
          ;; w-spec is a string
          (and (stringp w-spec) w-spec)
          ;; w-spec is a bool-vector
          ;; Why not? Could be interesting using boole-vector to
          ;; indicate where in an EIEIO class vector something went awry.
          ;; The also, have nice compact read/print format.
          (and (bool-vector-p w-spec)               
               (or (and (not w-args) (setq w-spec (mon-bool-vector-to-list w-spec)))
                   (and w-args
                        ;; w-args is either a proper list or a vector else signal
                        (or (or (mon-list-proper-p w-args)
                                (and (vectorp w-args)
                                     ;; w-args gets `push'/`pop'd below                                        
                                     (setq  w-args (append w-args nil))))
                            (error (concat 
                                    ":FUNCTION `mon-message' "
                                    "--  with keyword :W-SPEC `bool-vectorp' and "
                                    "keyword :W-ARGS not `mon-list-proper-p' or `vectorp'" 
                                    ",got: %S")
                                   w-args))
                        (setq w-spec (mon-bool-vector-to-list w-spec))
                        (let* ((mmsg-bv (cadr (memq :bool-vector w-spec)))
                               ;; Don't try to access an elt outside the bv's indexable range
                               (mmsg-idx-bnd  (length mmsg-bv)))
                          (setq mmsg-idx-bnd 
                                (mapcar #'(lambda (mmsg-L-0) 
                                            ;; `mon-bool-vector-to-list' signals when (= (length  bv) 0)
                                            ;; IOW don't worry about the 0 length bv bugs, just make
                                            ;; sure we have `wholenump' that doesn't exceed her bounds.
                                            (let (mmsg-L-lcl1)
                                              (when (or (and (wholenump mmsg-L-0) (< mmsg-L-0 mmsg-idx-bnd))
                                                        (error 
                                                         (concat 
                                                          ":FUNCTION `mon-message' "
                                                          " -- keyword :W-ARGS has elt not `wholenump' "
                                                          " when keyword :W-SPEC `bool-vectorp', got: %S")
                                                         w-args))
                                                (setq mmsg-L-lcl1 (aref mmsg-bv mmsg-L-0))
                                                (setq mmsg-L-lcl1 
                                                      `(,mmsg-L-0 
                                                        (,mmsg-L-lcl1 . ,(mon-booleanp-to-binary mmsg-L-lcl1)))))))
                                        w-args))
                          ;; Record how many "w-args" we got. We end up w/ a list with this format: 
                          ;; (10 (1 (t . 1)) (4 (nil . 0)) (7 (nil . 0)) { ... } )
                          (setq w-args (push (length mmsg-idx-bnd) mmsg-idx-bnd))
                          ;; Now map over the W-DELIM string
                          (and
                           (or (and w-delim (stringp w-delim)
                                    ;; Hold onto the W-DELIM arg for when everything comes back together
                                    (setq w-delim (cons ":at-index %2d :with-value %S" w-delim)))
                               (and (or (mon-string-or-null-and-zerop w-delim)
                                        (cadr (mon-booleanp w-delim)) ; we got `t'
                                        ;; disregard any non-string
                                        (not (stringp w-delim)))
                                    (setq w-delim (cons ":at-index %2d :with-value %S" "\n  "))))
                           ;; Stay inside the `or' branch build up a list
                           ;; of format strings using W-DELIM and W-ARGS
                           (setcar w-delim (make-list (pop w-args) (car w-delim)))
                           (prog2
                               ;; re-use mmsg-idx-bnd to store mapped value
                               (setq mmsg-idx-bnd
                                     (mon-mapcar #'(lambda (frmt-delims frmt-w-args)
                                                     (apply #'format 
                                                            `(,frmt-delims 
                                                              ,(car frmt-w-args) 
                                                              ,(cadr frmt-w-args))))
                                                 (car w-delim) w-args))
                               (setq w-spec
                                     (concat 
                                      (cdr w-delim)
                                      ;; :binary|:bit-string "#[b*]101001 {...} 
                                      (apply #'format "%s %s " (mon-subseq w-spec 0 2)) 
                                      (cdr w-delim)
                                      ;; `mon-bool-vector-pp' has variable
                                      ;; length return value and we already
                                      ;; have hold of its last elt so use it.
                                      (format ":bool-vector %S" mmsg-bv)
                                      (cdr w-delim)
                                      ;; "\n  :at-index %d :with-value %S" {...}
                                      (mapconcat #'identity mmsg-idx-bnd (cdr w-delim)) ))
                             (setq w-args nil)
                             (setq w-delim 'bool-vector))))))))
         ;; W-DELIM was provided and not a zero length string so map it.
         (or (and (or (and (not (eq w-delim 'bool-vector))
                           (mon-list-proper-p w-spec)
                           (mon-string-or-null-and-zerop w-delim)
                           (setq w-delim "\n  "))
                      ;; don't look further we've already handled the boole-vector
                      (not (eq w-delim 'bool-vector)))
                  w-delim ;; if we're here its not null so the next test guarantee a string
                  (or (mon-string-or-null-and-zerop w-delim)
                      ;; allow "" but delay so we error when not `stringp'
                      (or (stringp w-delim)
                          ;; :w-delim is not something other than `stringp' so signal.
                          (mon-error-string-err-format 
                           "mon-error-string-err-format" "w-delim" w-delim t)))
                  ;; When :w-spec is `bool-vector-p' it was converted to a list above.
                  ;; This is the only way :w-spec can possibly satisfy `mon-list-proper-p'
                  ;; inside this branch. So, get the value of `:binary` or `:bin-string`
                  ;; keyword either:  "#b..." or "#*..."
                  (mapconcat #'identity 
                             (list (or (and (mon-list-proper-p w-spec) 
                                            (elt w-spec 1))
                                       w-spec)
                                   "")
                             w-delim))
             w-spec))
    ;; :w-spec is a proper list or `vectorp'
    (and  (or (mon-list-proper-p w-spec)
              (eq (type-of w-spec) 'vector))
          ;; It is so make sure each elt is `stringp' before mapping
          (or (= (apply #'+ (mapcar #'(lambda (mmsg-L-1)
                                        (mon-booleanp-to-binary (not (stringp mmsg-L-1))))
                                    w-spec)) 
                 0)
              ;; :TODO Convert to `mon-error-string-err-format' once finalized.
              ;; Currently can't specify the second concated protion of generated message
              ;; (mon-error-string-err-format "mon-error-string-err-format" "w-spec" w-spec t)
              (error (concat ":FUNCTION `mon-message' " 
                             "-- element list supplied for keyword :W-SPEC "
                             "does not satisfy `stringp', got: %S") w-spec))
          ;; If W-DELIM was provided and `stringp' use it.
          ;; If not provided, or its a zero length string map `""`.
          ;; Else, its some other thing - signal error.
          (mapconcat #'identity w-spec 
                     (or (and (stringp w-delim) w-delim)
                         (and (mon-string-or-null-and-zerop w-delim) "")
                         (and w-delim (mon-error-string-err-format 
                                       "mon-error-string-err-format" "w-delim" w-delim t))))))
   ;; Finish up for the outer apply form
   (or (and (mon-list-proper-p w-args) w-args)
       (and (vectorp w-args) (append w-args nil))
       (and (atom w-args) (list w-args)))))


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
string-stack state as passed by `%mon-write-string-writer' %MWSP-CHR
arg. `nconc's %MWSP-CHR onto the stack.\n
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
           (error (mon-error-string-err-format
                   "mon-write-string" ":w-string" w-string)))
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
(unless (and (intern-soft "write-string" obarray)
             (fboundp 'write-string))
(defalias 'write-string 'mon-write-string))
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
(provide 'mon-error-utils)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-error-utils.el ends here
;;; EOF
