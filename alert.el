;;; alert.el --- Alternatives to `message'
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/alert
;; URL: http://raw.github.com/rolandwalker/alert/master/alert.el
;; Version: 0.5.10
;; Last-Updated: 22 Oct 2012
;; EmacsWiki: Alert
;; Keywords: extensions, message, interface
;; Package-Requires: ((string-utils "0.0.2"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'alert)
;;     (alert-install-aliases)
;;
;;     (alert "important message")
;;
;;     (with-message-logonly
;;       (do-something-noisy))
;;
;; Explanation
;;
;; Alert.el provides alternatives to Emacs' built-in `message'
;; function.
;;
;; This library is generally only useful when programming in Emacs
;; Lisp.  However, some end-users may find it useful to control
;; messaging, especially for the case of quietening chatty libraries
;; in their ~/.emacs files (see below).
;;
;; The principal `alert' function by default works differently from
;; `message' in almost every respect, displaying with sound and visual
;; highlight, and not writing to the log.  See the `alert' docstring
;; for details.  The variant function `alert*' has identical
;; functionality, but accepts CL-style arguments.
;;
;; The following functions provided by this library are drop-in
;; alternatives to `message' which may be useful in an `flet'
;; construct:
;;
;;     `alert-message-nolog'
;;     `alert-message-logonly'
;;     `alert-message-highlight'
;;     `alert-message-insert'
;;     `alert-message-notify'
;;     `alert-message-popup'
;;     `alert-message-temp'
;;     `alert-message-string'
;;
;; The following macros modify the behavior of `message' within
;; the enclosing expression:
;;
;;     `alert-with-message-nolog'
;;     `alert-with-message-logonly'
;;     `alert-with-message-highlight'
;;     `alert-with-message-insert'
;;     `alert-with-message-notify'
;;     `alert-with-message-popup'
;;     `alert-with-message-temp'
;;     `alert-with-message-string'
;;
;; For example, the following code would redirect messages from a very
;; chatty library to the log:
;;
;;     (alert-with-message-nolog
;;       (require 'very-chatty-library))
;;
;; The same method may also be handy with `defadvice':
;;
;;     (defadvice very-chatty-function (around very-chatty-redirect activate)
;;       (alert-with-message-nolog
;;         ad-do-it))
;;
;; Similarly, important messages may be redirected to a more visible
;; form:
;;
;;     (defadvice an-important-function (around an-important-function activate)
;;       (alert-with-message-notify
;;         ad-do-it))
;;
;; To use `alert', place the alert.el library somewhere Emacs can find
;; it, and add the following to your ~/.emacs file:
;;
;;     (require 'alert)
;;     (alert-install-aliases)     ; optionally, can also be set in customize
;;
;; Running `alert-install-aliases' or setting the corresponding
;; variable in customize will install convenience aliases outside
;; the "alert-" namespace.  This is disabled by default.
;;
;; See Also
;;
;;     M-x customize-group RET alert RET
;;     M-x customize-group RET notify RET
;;     M-x customize-group RET popup RET
;;
;; Notes
;;
;;     The function `alert-message-noformat' is also available, but it
;;     is not quite a drop-in replacement for `message'.
;;
;;     Some of the functions require the availability of notify.el,
;;     todochiku.el or popup.el.  In all cases, the function will
;;     degrade to an ordinary message if the external library is not
;;     present.
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.3-devel     : yes, at the time of writing
;;     GNU Emacs version 24.1 & 24.2    : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     Uses if present: string-utils.el, notify.el, todochiku.el,
;;                      popup.el
;;
;; Bugs
;;
;;     Soft dependency on unpublished popup-volatile.
;;
;;     `message' is a subr.  Macros such as `alert-with-message-logonly'
;;     will only affect calls to `message' from Lisp.
;;
;; TODO
;;
;;     Default icons and timeouts for notifications.
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

;;; requirements

(eval-and-compile
  ;; for callf, callf2, assert, flet/cl-flet
  (require 'cl)
  (unless (fboundp 'cl-flet)
    (defalias 'cl-flet 'flet)
    (put 'cl-flet 'lisp-indent-function 1)
    (put 'cl-flet 'edebug-form-spec '((&rest (defun*)) cl-declarations body))))

(autoload 'notify            "notify"         "Notify TITLE, BODY via `notify-method'.")
(autoload 'todochiku-message "todochiku"      "Send a message via growl, snarl, etc.")
(autoload 'popup-tip         "popup"          "Show a tooltip of STRING at POINT.")
(autoload 'popup-volatile    "popup-volatile" "Create a volatile tooltip using `popup-tip'.")

(require 'string-utils nil t)

;;; declarations

(declare-function string-utils-propertize-fillin "string-utils.el")
(declare-function alert-message                  "alert.el")

;;; customizable variables

;;;###autoload
(defgroup alert nil
  "Alternatives to `message'."
  :version "0.5.10"
  :link '(emacs-commentary-link "alert")
  :prefix "alert-"
  :group 'extensions)

(defcustom alert-message-seconds 2
  "Default period to display alert messages."
  :type 'integer
  :group 'alert)

(defcustom alert-message-notify-title "Emacs"
  "Default title for messages presented by `alert-message-notify'."
  :type 'integer
  :group 'alert)

(defcustom alert-face 'highlight
  "Face to use for highlighting alert messages."
  :group 'alert)

;;;###autoload
(defcustom alert-install-short-aliases nil
  "Install short aliases such as `message-nolog' for `alert-message-nolog'."
  :type 'boolean
  :group 'alert)

;;; aliases and fsets

(fset 'alert-message (symbol-function 'message))

;;;###autoload
(progn
  (defun alert-install-aliases (&optional arg)
    "Install aliases outside the \"alert-\" namespace.

With optional negative ARG, uninstall aliases.

The following aliases will be installed:

   message-nolog      for   alert-message-nolog
   message-logonly    for   alert-message-logonly
   message-noformat   for   alert-message-noformat
   message-highlight  for   alert-message-highlight
   message-insert     for   alert-message-insert
   message-notify     for   alert-message-notify
   message-popup      for   alert-message-popup
   message-temp       for   alert-message-temp"
    (let ((syms '(
                  nolog
                  logonly
                  highlight
                  insert
                  noformat
                  notify
                  popup
                  temp
                  string
                  )))
      (cond
        ((and (numberp arg)
              (< arg 0))
         (dolist (sym syms)
           (when (ignore-errors
                   (eq (symbol-function (intern-soft (format "message-%s" sym)))
                                        (intern-soft (format "alert-message-%s" sym))))
             (fmakunbound (intern (format "message-%s" sym))))
           (when (ignore-errors
                   (eq (symbol-function (intern-soft (format "with-message-%s" sym)))
                                        (intern-soft (format "alert-with-message-%s" sym))))
             (fmakunbound (intern (format "with-message-%s" sym))))))
        (t
         (dolist (sym syms)
           (defalias (intern (format "message-%s" sym)) (intern (format "alert-message-%s" sym)))
           (defalias (intern (format "with-message-%s" sym)) (intern (format "alert-with-message-%s" sym)))))))))

;;;###autoload
(when alert-install-short-aliases
  (alert-install-aliases))

;;; compatibility functions

(unless (fboundp 'string-utils-propertize-fillin)
  (defun string-utils-propertize-fillin (str-val &rest properties)
    "Return a copy of STRING with text properties added, without overriding.

Works exactly like `propertize', except that (character-by-character)
already existing properties are respected."
    (unless (= 0 (% (length properties) 2))
      (error "Wrong number of arguments"))
    (while properties
      (let ((prop (pop properties))
            (val  (pop properties)))
        (font-lock-fillin-text-property 0 (length str-val) prop val str-val)))
    str-val))

;;; utility functions

;;;###autoload
(defun alert-message-noformat (content &rest _ignored)
  "An alternative for `message' which assumes a pre-formatted CONTENT string.

Any arguments after CONTENT are ignored, meaning this is not
functionally equivalent to `message'.  However, flet'ing
`message' to this function is safe in the sense that it does not
call `message' directly."
  (if (null content)
      (alert-message content)
    ;; else
    (assert (stringp content) nil "CONTENT must be a string")
    (alert-message (replace-regexp-in-string "%" "%%" content))))

(defun alert-message-maybe-formatted (&rest args)
  "Dispatch `message' according to the variable `alert-message-preformatted'.

Formatting is not performed if `alert-message-preformatted' is
bound and non-nil.

When formatting is performed, ARGS are treated as for `message', including
a format-string.  When formatting is not performed, only the first element
of ARGS is respected.  It should be a pre-formatted string."
    (if (and (boundp 'alert-message-preformatted)
             alert-message-preformatted)
        (apply 'alert-message-noformat args)
      ;; else
      (apply 'alert-message args)))

(defun alert--message-insert-1 (msg)
  "Internal driver for `alert-message-insert'.

Inserts pre-formatted MSG at the current position with line feeds as needed."
  (unless (eq (line-beginning-position) (point))
    (insert "\n"))
  (insert msg)
  (unless (eq (line-beginning-position) (point))
    (insert "\n")))

;;;###autoload
(defun alert-message-logonly (&rest args)
  "An flet'able replacement for `message' which logs but does not echo.

ARGS are as for `message', including a format-string."
  (when message-log-max
    (if (get-buffer "*Messages*")
        ;; don't worry about truncating the message log, some standard
        ;; call to `message' will catch up with it later.
        (with-current-buffer "*Messages*"
          (save-excursion
            (goto-char (point-max))
            (let ((msg (if (and (boundp 'alert-message-preformatted)
                                alert-message-preformatted)
                           (car args)
                         (apply 'format args))))
              (alert--message-insert-1 msg)
              msg)))
      ;; else
      (let ((current-msg (current-message)))
        (apply 'alert-message-maybe-formatted args)
        (alert-message current-msg)))))

;;;###autoload
(defun alert-message-insert (&rest args)
  "An flet'able replacement for `message' which inserts text instead of echoing.

ARGS are as for `message', including a format-string."
  (let ((msg (if (and (boundp 'alert-message-preformatted) alert-message-preformatted) (car args) (apply 'format args)))
        (alert-message-preformatted t))
    (alert-message-logonly msg)
    (alert--message-insert-1 msg)
    msg))

;;;###autoload
(defun alert-message-string (&rest args)
  "An flet'able replacement for `message' which returns a string instead of echoing.

Newline is appended to the return value as with `message'.

ARGS are as for `message', including a format-string."
  (let ((msg (if (and (boundp 'alert-message-preformatted) alert-message-preformatted) (car args) (apply 'format args))))
    (concat msg "\n")))

;;;###autoload
(defun alert-message-nolog (&rest args)
  "An flet'able replacement for `message' which echos but does not log.

ARGS are as for `message', including a format-string."
  (let ((message-log-max nil))
    (apply 'alert-message-maybe-formatted args)))

;;;###autoload
(defun alert-message-temp (&rest args)
  "An flet'able replacement for `message' which displays temporarily.

The display time is governed by `alert-message-seconds'.

ARGS are as for `message', including a format-string."
  (when (car args)
    (let ((current-msg (current-message))
          (retval (apply 'alert-message-maybe-formatted args)))
      (when (numberp alert-message-seconds)
        (sit-for alert-message-seconds))
      (alert-message (or current-msg ""))
      retval)))

;;;###autoload
(defun alert-message-popup (&rest args)
  "An flet'able replacement for `message' which uses popups instead of echoing.

The functions `popup-volatile' and `popup' are attempted in
order to create a popup.  If both functions fail, the message
content will appear in the echo area as usual.

ARGS are as for `message', including a format-string."
  (let ((msg (if (and (boundp 'alert-message-preformatted) alert-message-preformatted) (car args) (apply 'format args)))
        (alert-message-preformatted t))
    (alert-message-logonly msg)
    (condition-case nil
        (progn
          (popup-volatile msg)
          msg)
      (error nil
         (condition-case nil
             (progn
               (popup-tip msg)
               msg)
           (error nil
                (alert-message-nolog msg)))))))

;;;###autoload
(defun alert-message-notify (&rest args)
  "An flet'able replacement for `message' which uses notifications instead echo.

The following functions are attempted in order call system
notifications: `notify' and `todochiku-message'.  If both
functions fail, the message content will appear in the echo
area as usual.

ARGS are as for `message', including a format-string."
  (let ((msg (if (and (boundp 'alert-message-preformatted) alert-message-preformatted) (car args) (apply 'format args)))
        (alert-message-preformatted t))
    (alert-message-logonly msg)
    (condition-case nil
        (progn
          (notify alert-message-notify-title msg)
          msg)
      (error nil
         (condition-case nil
             (progn
               (todochiku-message alert-message-notify-title msg "")
               msg)
           (error nil
                (alert-message-nolog msg)))))))

;;;###autoload
(defun alert-message-highlight (&rest args)
  "An flet'able replacement for `message' which echos highlighted text.

Text without added properties is logged to the messages buffer as
usual.

ARGS are as for `message', including a format-string."
  (let ((retval (apply 'alert-message-logonly args)))
    (when (stringp (car args))
      (callf concat (car args) (propertize " " 'display '(space :align-to right-margin)))
      (callf string-utils-propertize-fillin (car args) 'face (append (face-attr-construct 'default)
                                                                     (face-attr-construct alert-face))))
    (let ((message-log-max nil))
      (apply 'alert-message-maybe-formatted args))
    retval))

;;;###autoload
(defun alert (content &optional quiet seconds nocolor log notify popup)
  "Transiently and noticeably display CONTENT in the echo area.

CONTENT should be a pre-`format'ted if it is a string.

CONTENT will be coerced to a string if it is not a string.

Optional QUIET suppresses the bell, which is on by default.

Optional SECONDS determines the number of seconds CONTENT will be
displayed before reverting to the previous content of the echo
area.  Default is `alert-message-seconds'.  If SECONDS is 0, or
non-numeric, the message is not timed out, and remains visible
until the next write to the echo area.

Optional NOCOLOR suppresses coloring the message with face held
in the variable `alert-face'.

Optional LOG enables logging of CONTENT for any non-nil value.
If LOG is 'log-only, then CONTENT goes only to the *Messages*
buffer and all other options are ignored.

Optional NOTIFY enables sending the message via the notifications
system of the underlying OS.  The default is nil.  If NOTIFY is
'replace-echo, then the notification will be used instead of the
echo area.  For any other non-nil value, the notification will be
used in addition to the echo area.

Optional POPUP enables sending the message via `popup-tip' from
popup.el.  The default is nil.  If POPUP is 'replace-echo, then
the popup will be used instead of the echo area.  For any other
non-nil value, the popup will be used in addition to the echo
area.

The behavior of `alert' is very different from `message':

  - CONTENT must already be formatted.

  - Non-strings are accepted for CONTENT.

  - The content is displayed with added color.

  - The bell is rung.

  - CONTENT is not written to the messages buffer (log).

  - After display, the previous contents of the echo area are
    restored.

The following forms using `message` and `alert` are equivalent:

   (message \"hello, %s\" name)
   (alert (format \"hello, %s\" name) 'quiet 0 'nocolor 'log)"
  (unless (stringp content)
    (if (fboundp 'string-utils-stringify-anything)
        (callf string-utils-stringify-anything content)
      ;; else
      (callf2 format "%s" content)))
  (let ((alert-message-seconds alert-message-seconds)
        (message-log-max message-log-max)
        (colored-content content)
        (alert-message-preformatted t))
    (unless (or quiet
                (eq log 'log-only)
                (eq notify 'replace-echo)
                (eq popup 'replace-echo))
      (ding t))
    (when log
      (alert-message-logonly content))
    (when notify
      (alert-message-notify content))
    (when popup
      (alert-message-popup content))
    (setq message-log-max nil)
    (cond
      ((numberp seconds)
       (setq alert-message-seconds seconds))
      ((not (null seconds))
       (setq alert-message-seconds 0)))
    (unless (or (eq log 'log-only)
                (eq notify 'replace-echo)
                (eq popup 'replace-echo))
      (unless (stringp content)
        (callf2 format "%s" content))
      (unless nocolor
        (callf concat colored-content (propertize " " 'display '(space :align-to right-margin)))
        (callf string-utils-propertize-fillin colored-content 'face (append (face-attr-construct 'default)
                                                                            (face-attr-construct alert-face))))
      (if (and (numberp alert-message-seconds)
               (> alert-message-seconds 0))
          (alert-message-temp colored-content)
        (alert-message-noformat colored-content))))
  content)

;;;###autoload
(defun* alert* (content &key quiet seconds nocolor log notify popup)
  "An alternate version of `alert' which uses Common Lisp semantics.

CONTENT, QUIET, SECONDS, NOCOLOR, LOG, NOTIFY, and POPUP are as
documented for `alert'."
  (alert content quiet seconds nocolor log notify popup))

;;;###autoload
(defmacro alert-with-message-logonly (&rest body)
  "Execute BODY, redirecting the output of `message' to the log only.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(cl-flet ((message (&rest args)
                      (apply 'alert-message-logonly args)))
     ,@body))

;;;###autoload
(defmacro alert-with-message-nolog (&rest body)
  "Execute BODY, keeping the output of `message' from being added to the log.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(cl-flet ((message (&rest args)
                      (apply 'alert-message-nolog args)))
     ,@body))

;;;###autoload
(defmacro alert-with-message-highlight (&rest body)
  "Execute BODY, highlighting the output of `message'.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(cl-flet ((message (&rest args)
                      (apply 'alert-message-highlight args)))
     ,@body))

;;;###autoload
(defmacro alert-with-message-notify (&rest body)
  "Execute BODY, redirecting the output of `message' to system notifications.

notify.el or todochiku.el may be used to provide the interface to
system notifications.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(cl-flet ((message (&rest args)
                      (apply 'alert-message-notify args)))
     ,@body))

;;;###autoload
(defmacro alert-with-message-popup (&rest body)
  "Execute BODY, redirecting the output of `message' to popups.

popup.el is required.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(cl-flet ((message (&rest args)
                      (apply 'alert-message-popup args)))
     ,@body))

;;;###autoload
(defmacro alert-with-message-insert (&rest body)
  "Execute BODY, redirecting the output of `message' to `insert'.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(cl-flet ((message (&rest args)
                      (apply 'alert-message-insert args)))
     ,@body))

;;;###autoload
(defmacro alert-with-message-string (&rest body)
  "Execute BODY, capturing the output of `message' to a string.

Accumulated message output is returned.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  (let ((output (gensym "--with-message-string--")))
    `(let ((,output ""))
       (cl-flet ((message (&rest args)
                          (callf concat ,output (apply 'alert-message-string args))))
         ,@body)
       ,output)))

;;;###autoload
(defmacro alert-with-message-temp (&rest body)
  "Execute BODY, making all `message' output temporary.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(cl-flet ((message (&rest args)
                      (apply 'alert-message-temp args)))
     ,@body))

;;;###autoload
(defmacro alert-with-message-noformat (&rest body)
  "Execute BODY, keeping `message' from formatting its arguments.

All arguments to `message' after the first one will be dropped.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(cl-flet ((message (&rest args)
                      (apply 'alert-message-noformat args)))
     ,@body))

(provide 'alert)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: noformat logonly nolog flet'able NOCOLOR nocolor fsets
;; LocalWords: flet todochiku ARGS args callf
;;

;;; alert.el ends here
