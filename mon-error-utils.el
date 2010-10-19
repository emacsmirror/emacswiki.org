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
;; `mon-error-toplevel', `mon-error', `mon-error-gather', `mon-error-gather-peek',
;; `mon-error-gather-finalize', `mon-error-gather-reset',
;; `mon-error-protect-PP-EXPAND-TEST', `mon-display-warning', `mon-message',
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
`mon-help-CL-error-condition-restart'.\n►►►")

;;; ==============================
;;; :COURTESY slime.el :WAS `slime-message-function'
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-02T11:10:48-04:00Z}#{10396} - by MON KEY>
(defcustom *mon-message-function* 'message
  "Defualt message formatting function for `mon-message'.\n
:EXAMPLE\n\n
:SEE-ALSO `message', `mon-error-string-err-format'.\n►►►"
  :type 'function
  :group 'mon-error-warn)
;;
;; (unless (and (intern-soft "*mon-message-function*")
;;              (bound-and-true-p *mon-message-function*))
;;   (setq *mon-message-function* 'message))


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
:SEE-ALSO `mon-error', `mon-error-toplevel',`mon-error-gather',
`mon-error-gather-peek', `mon-error-string-err-format', `mon-message',
`redirect-debugging-output', `external-debugging-output', `debug-on-signal',
`debug-on-error', `debug-ignored-errors', `signal-hook-function',
`*mon-emacs-help-errors*', `mon-help-errors',
`mon-help-CL-error-condition-restart'.\n►►►"
  (let ((bdy-wrap (make-symbol "bdy-wrap")))
    `(let ((,bdy-wrap (or ,@body t)))
       (condition-case nil
        (unless (ignore-errors (or (null (eval ,bdy-wrap)) t))
          (error ""))
      (error ,err-arg)))))

;;; ==============================
;;; :CHANGESET 2141
;;; :CREATED <Timestamp: #{2010-09-16T21:12:01-04:00Z}#{10374} - by MON KEY>
(defun mon-error-protect-PP-EXPAND-TEST (expand-form)
  "Test function for macro `mon-error-protect'.\n
Return and display results to buffer named \"*PP-EXPAND-ALL*\"
:SEE-ALSO .\n►►►"
  (with-current-buffer (get-buffer-create "*PP-EXPAND-ALL*")
    (erase-buffer)
    (save-excursion
      (prin1 (macroexpand-all expand-form) (current-buffer))
      (emacs-lisp-mode)
      (pp-buffer)
      (display-buffer (current-buffer) t))))

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
`mon-help-CL-error-condition-restart'.\n►►►"
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
(defun* mon-error (&key signaler function sig-str sig-arg handler)
  "FUNCTION is the symbol originating the error.\n
SINGALER is the type of error specific to the problem. Default is `mon-error'.\n
HANDLER is a preferred function to invoke to attempt recovering from the error.\n
SIG-STR string arg is a format spec it takes SIG-ARG as its arguments to
construct an error message.\n
SIG-ARG is a list of args, this is the VAR which becomes local in a
`condition-case' condition form.\n
:EXAMPLE\n\n\(let \(\(not-a-nmbr \"| local string val not 88 |\"\)\)
  \(condition-case err
      \(unless \(numberp not-a-nmbr\)
        \(mon-error  :signaler 'mon-error
                    :function 'test-fun 
                    :sig-str \"GOT %S -- wanted `%s'\" 
                    :sig-arg `\(,not-a-nmbr 88 i-am-an-arg-for-handler\)
                    :handler 'imaginary-hndlr\)\)
    \(mon-error-toplevel err\)\)\)\n
:SEE-ALSO `condition-case', `mon-error-toplevel' `mon-error',
`mon-error-gather', `mon-error-gather-peek', `mon-error-gather-finalize',
`mon-error-gather-reset', `*mon-error-gather*', `mon-error-string-err-format',
`mon-message', `mon-help-errors', `*mon-emacs-help-errors*',
`mon-help-CL-error-condition-restart'.\n►►►"
  (let* ( ;;(jstfy (propertize "\t" 'display '(space :align-to 32)))
         (jstfy (make-string  21 32)) ;; <- (eq 21 (length "(mon-error-toplevel \""))
         (emsg (concat (format ":SIGNALER `%s'" signaler)
                       "\n" jstfy (format ":FUNCTION `%s' -- " function)
                       (cond ((and sig-str sig-arg)
                              (apply 'format sig-str sig-arg))
                             ((and sig-str (not sig-arg))
                              sig-str)
                             ((and (not sig-str) sig-arg)
                              (format "applicaple args: %S" sig-arg)))
                       (or (and handler (format (concat "\n" jstfy ":HANDLER  `%s'") handler))
                           (format (concat "\n" jstfy ":HANDLER  `no-applicable-handler'"))))))
    (princ emsg 'mon-error-gather)
    (mon-error-toplevel `(:FUNCTION ,function 
                          :SIGNALER ,signaler 
                          :HANDLER (,(or handler 'no-applicable-handler) ,@sig-arg))))) 
                                    
;;
(put 'mon-error 'error-conditions '(error mon-error-toplevel mon-error))
(put 'mon-error 'error-message ":MON-ERROR-MESSAGE")
;;                     
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (let ((not-a-nmbr "| local string val not 88 |"))
;; |   (condition-case err
;; |       (unless (numberp not-a-nmbr)
;; |         (mon-error  :signaler 'mon-error
;; |                     :function 'test-fun 
;; |                     :sig-str "GOT %S -- wanted `%s'" 
;; |                     :sig-arg `(,not-a-nmbr 88 i-am-an-arg-for-handler)
;; |                     :handler 'imaginary-hndlr))
;; |     ;; (mon-error-toplevel (caddr err))))
;; |     (mon-error-toplevel err))) 
;; `----

(defun mon-error-string-err (function error-locus handler)
  "
:EXAMPLE\n\n
:SEE-ALSO `mon-error-string-err-format', `mon-message'.\n►►►"
  (mon-error :signaler 'mon-error-string 
             :function function
             :sig-str "arg %s does not satisfy `stringp'"
             :sig-arg `(,error-locus)
             :handler handler))
;;
(put 'mon-error-string 'error-conditions '(mon-error-toplevel))


;;; ==============================
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-07T12:35:40-04:00Z}#{10404} - by MON KEY>
(defun mon-error-string-err-format (fun-name locus-arg got-arg-val)
  "Return a formatted mon-error string for an arg not satisfying `stringp'.\n
FUN-NAME is the function name signaling.
LOCUS-ARG is the args local symbol name quoting rules apply. 
It is converted to a string and `upcase'd for format.
GOT-ARG-VAL is the argument value. It is passed to evaluate to its value.\n
:EXAMPLE\n\n\(mon-error-string-err-format 'subrp 'bubba-arg 'bubba-val\)\n
\(mon-error-string-err-format \"subrp\" 'bubba-arg 'bubba-val\)\n
\(mon-error-string-err-format 'subrp 'some-arg 88\)\n
\(mon-error-string-err-format 'subrp \"some-arg\" 88\)\n
\(mon-error-string-err-format \"subrp\" \"some-arg\" 88\)\n
;; :NOTE Following should all fail successfully\n
\(mon-error-string-err-format 'non-existent-function 'bubba-arg 'bubba-val\)\n
\(mon-error-string-err-format 'subrp 88 'bubba-val\)\n
\(mon-error-string-err-format 'subrp 8.8 'bubba-val\)\n
\(mon-error-string-err-format 'not-a-fncn 88 'bubba\) ;Never see second error.\n
:SEE-ALSO `mon-error', `mon-error-toplevel',`mon-error-gather',
`mon-error-gather-peek', `mon-error-string-err-format', `mon-message',
`*mon-emacs-help-errors*', `mon-help-errors',
`mon-help-CL-error-condition-restart'.\n►►►"
  ;; :NOTE As this is an error signalling routine we should assume that the
  ;; args are not always what we expect.
  (format (concat ":FUNCTION `%s' "
                  "-- arg %s does not satisfy `stringp', got: %S")
          (or (and (stringp fun-name) fun-name)
              (and (symbolp fun-name) (fboundp fun-name) (symbol-name fun-name))
              (let ((lcl-eargs `(mon-error-string-err-format fun-name ,fun-name)))
                (error (apply #'mon-error-string-err-format lcl-eargs))))
          (or (and (stringp locus-arg) (upcase locus-arg))
              (and (atom locus-arg)
                   (or (and (not (null locus-arg))
                            (not (eq locus-arg 'nil)) 
                            (not (eq locus-arg 't)) 
                            (not (numberp locus-arg))
                            (and (symbolp locus-arg) (upcase (symbol-name locus-arg))))
                       (let ((lcl-eargs `(,fun-name ,(format "%S" locus-arg) ,locus-arg)))
                         (error (apply #'mon-error-string-err-format lcl-eargs))))))
          got-arg-val))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (stringp (mon-error-string-err-format 'subrp 'bubba-arg 'bubba-val))
;; | (stringp (mon-error-string-err-format "subrp" 'bubba-arg 'bubba-val))
;; | (stringp (mon-error-string-err-format 'subrp 'some-arg 88))
;; | (stringp (mon-error-string-err-format 'subrp "some-arg" 88))
;; | (stringp (mon-error-string-err-format "subrp" "some-arg" 88))
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
:SEE-ALSO `mon-error-toplevel' `mon-error', `mon-error-gather-peek',
`mon-error-gather-finalize', `mon-error-gather-reset', `*mon-error-gather*',
`*mon-help-emacs-errors*', `mon-help-errors',
`mon-help-CL-error-condition-restart'.\n►►►"
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
`mon-error-gather-finalize', `mon-error-gather-reset',
`*mon-help-emacs-errors*', `mon-help-errors',
`mon-help-CL-error-condition-restart'.\n►►►"
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
`mon-error-gather-peek', `mon-error-gather-finalize', 
`*mon-help-emacs-errors*', `mon-help-errors', `mon-help-CL-error-condition-restart'.\n►►►"
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
`mon-help-CL-error-condition-restart'.\n►►►"
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
;;; :COURTESY slime.el :WAS `slime-message'
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-02T11:10:54-04:00Z}#{10396} - by MON KEY>
(defun mon-message (format &rest args)
  "Like `message' but with special support for multi-line messages.\n
Single-line messages use the echo area.\n
Message is formatted as if by application of `*mon-message-function*'.\n
:EXAMPLE\n\n\(mon-message \(concat \":FUNCTION `mon-message' \" 
                             \"-- example message with arg `%s' and arg `%d'\"\)
                     'bubba 666\)\n
:SEE-ALSO `mon-display-warning'.\n►►►"
  (apply *mon-message-function* format args))

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
;;; :CHANGESET 2195
;;; :CREATED <Timestamp: #{2010-10-18T15:02:34-04:00Z}#{10421} - by MON KEY>
(eval-when (compile load eval)
  (lexical-let (mws-gthr)
    (defun %mon-write-string-pusher (psh-chr)
      (funcall #'(lambda (psh-chr)
                   (setq mws-gthr (nreverse (nconc (list psh-chr) mws-gthr))))
               psh-chr))
    (defun %mon-write-string-writer (chr)
      (funcall #'(lambda (chr)
                   (write-char chr #'%mon-write-string-pusher)) chr))
    (defun %mon-write-string-reader () 
      (funcall  #'(lambda () 
                    (concat mws-gthr))))
    (defun %mon-write-string-mapper (char-bag)
      (funcall #'(lambda (char-bag)
                   (mapc #'%mon-write-string-writer (vconcat char-bag))
                   char-bag) char-bag))
    (defun %mon-write-string-reset ()
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
;;; :CHANGESET 2195
;;; :CREATED <Timestamp: #{2010-10-18T21:01:35-04:00Z}#{10421} - by MON KEY>
(defun* mon-write-string (&key w-string read-current reset-bind reset-null)
  "
When keyword W-STRING is non-nil it is a string to push onto the current string
stack.\n
When keyword READ-CURRENT is non-nil return value of the current string stack.\n
When keyword RESET-BIND is non-nil reset the current string stack to the empty
string, but only after dumping it somewhere.\n
RESET-BIND is a symbol, buffer, marker, or function. \n
When it is a symbol set, symbol to the value of current string stack and return
a cons with the format:\n(<STRING> . <SYMBOL>)\n
When RESET-BIND is a buffer, marker or function output is as per `standard-output'.
When it is a buffer output the contents of stringstack to that buffer by mapping
the string string elts as if by `write-char' with output inserted in buffer
before point.\n
When it is a marker output is inserted and marker is advanced.\n
:ALIASED-BY `write-string'\n
:SEE-ALSO .\n►►►"
  ;; (mon-write-string :reset-bind (make-marker))
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
                     (mon-equality-or-pred #'(lambda (mws-L-2 mws-L-3) ;ignore second arg
                                               (and (not (or (characterp mws-L-2)
                                                             (consp mws-L-2)))
                                                    (car (memq (mon-function-object-p mws-L-2)
                                                               '(function subr macro autoload)))))
                                           reset-bind t)
                     (and (symbolp reset-bind) (not (string-or-null-p reset-bind)) 'symbol)
                     'other))
           (cond ((memq mws-chk-typ '(buffer marker function subr macro autoload))
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

;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (progn 
;; |   (mon-write-string :reset-null t)
;; |   (dotimes (i 4)
;; |     (mon-write-string :w-string "bubba"))
;; |   (equal (mon-write-string :read-current t)
;; |               "buabbbuabbubbbaubbba"))
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
