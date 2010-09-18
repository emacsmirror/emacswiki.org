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
;; `mon-error-protect-PP-EXPAND-TEST',
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
;;; :CHANGESET 2141
;;; :CREATED <Timestamp: #{2010-09-17T19:18:44-04:00Z}#{10375} - by MON KEY>
(defvar *mon-error-gather* nil
  "Temporary string container for gathering error messages.\n
:SEE-ALSO `mon-error-toplevel' `mon-error', `mon-error-protect',
`mon-error-gather', `mon-error-gather-peek', `mon-error-gather-finalize',
`mon-error-gather-reset'.\n►►►")

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
:SEE-ALSO .\n►►►"
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
:SEE-ALSO `mon-error', `mon-error-gather', `mon-error-gather-peek'.\n►►►"
  (let ((emsg (mon-error-gather-finalize)))
    (signal 'mon-error-toplevel `(,emsg ,@args))))
;;
(put 'mon-error-toplevel 'error-conditions '(mon-error-toplevel error))
(put 'mon-error-toplevel 'error-message ":MON-ERROR-TOPLEVEL")

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
    ;; \(mon-error-toplevel \(caddr err\)\)\)\)
    \(mon-error-toplevel err\)\)\)\n
:SEE-ALSO `mon-error-toplevel' `mon-error', `mon-error-gather',
`mon-error-gather-peek', `mon-error-gather-finalize', `mon-error-gather-reset' ,
`*mon-error-gather*'.\n►►►"
  (let* (;; (length "(mon-error-toplevel ")
         (jstfy (propertize "\t" 'display '(space :align-to 32)))
         (emsg (concat jstfy (format ":SIGNALER `%s'" signaler)
                       "\n" jstfy (format ":FUNCTION `%s' -- " function)
                       (apply 'format sig-str sig-arg)
                       (or (and handler (format (concat "\n" jstfy ":HANDLER `%s'\n") handler))
                           "\n"))))
    (princ emsg 'mon-error-gather)
    (mon-error-toplevel `(:FUNCTION ,function 
                          :SIGNALER ,signaler 
                          :HANDLER (,handler ,@sig-arg)))))
;;
(put 'mon-error 'error-conditions '(mon-error mon-error-toplevel error))
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
`mon-error-gather-finalize', `mon-error-gather-reset',
`*mon-error-gather*'.\n►►►"
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
`mon-error-gather-finalize', `mon-error-gather-reset'.\n►►►"
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
`mon-error-gather-peek', `mon-error-gather-finalize'.\n►►►"
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
`mon-error-gather-peek'.\n►►►"
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
(provide 'mon-error-utils)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-error-utils.el ends here
;;; EOF
