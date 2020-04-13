;;; express.el --- Alternatives to `message'
;;
;; Copyright (c) 2012-13 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/express
;; URL: http://raw.githubusercontent.com/rolandwalker/express/master/express.el
;; Version: 0.6.0
;; Last-Updated: 23 Oct 2013
;; EmacsWiki: Express
;; Keywords: extensions, message, interface
;; Package-Requires: ((string-utils "0.3.2"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'express)
;;     (express-install-aliases)
;;
;;     (express "important message")
;;
;;     (with-message-logonly
;;       (do-something-noisy))
;;
;; Explanation
;;
;; Express.el provides alternatives to Emacs' built-in `message'
;; function.
;;
;; This library is generally only useful when programming in Emacs
;; Lisp.  However, some end-users may find it useful to control
;; messaging, especially for the case of quietening chatty libraries
;; in their ~/.emacs files (see below).
;;
;; The principal `express' function by default works differently from
;; `message' in almost every respect, displaying with sound and visual
;; highlight, and not writing to the log.  See the `express' docstring
;; for details.  The variant function `express*' has identical
;; functionality, but accepts CL-style arguments.
;;
;; The following functions provided by this library are drop-in
;; alternatives to `message' which may be useful in an `flet'
;; construct:
;;
;;     `express-message-nolog'
;;     `express-message-logonly'
;;     `express-message-highlight'
;;     `express-message-insert'
;;     `express-message-notify'
;;     `express-message-popup'
;;     `express-message-temp'
;;     `express-message-string'
;;
;; The following macros modify the behavior of `message' within
;; the enclosing expression:
;;
;;     `express-with-message-nolog'
;;     `express-with-message-logonly'
;;     `express-with-message-highlight'
;;     `express-with-message-insert'
;;     `express-with-message-notify'
;;     `express-with-message-popup'
;;     `express-with-message-temp'
;;     `express-with-message-string'
;;
;; For example, the following code would redirect messages from a very
;; chatty library to the log:
;;
;;     (express-with-message-nolog
;;       (require 'very-chatty-library))
;;
;; The same method may also be handy with `defadvice':
;;
;;     (defadvice very-chatty-function (around very-chatty-redirect activate)
;;       (express-with-message-nolog
;;         ad-do-it))
;;
;; Similarly, important messages may be redirected to a more visible
;; form:
;;
;;     (defadvice an-important-function (around an-important-function activate)
;;       (express-with-message-notify
;;         ad-do-it))
;;
;; To use `express', place the express.el library somewhere Emacs can find
;; it, and add the following to your ~/.emacs file:
;;
;;     (require 'express)
;;     (express-install-aliases)     ; optionally, can also be set in customize
;;
;; Running `express-install-aliases' or setting the corresponding
;; variable in customize will install convenience aliases outside
;; the "express-" namespace.  This is disabled by default.
;;
;; See Also
;;
;;     M-x customize-group RET express RET
;;     M-x customize-group RET notify RET
;;     M-x customize-group RET popup RET
;;
;; Notes
;;
;;     The function `express-message-noformat' is also available, but it
;;     is not quite a drop-in replacement for `message'.
;;
;;     Some of the functions require the availability of notify.el,
;;     todochiku.el or popup.el.  In all cases, the function will
;;     degrade to an ordinary message if the external library is not
;;     present.
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.4-devel     : yes, at the time of writing
;;     GNU Emacs version 24.3           : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.2           : yes, with some limitations
;;     GNU Emacs version 21.x and lower : unknown
;;
;;     Uses if present: string-utils.el, notify.el, todochiku.el,
;;                      popup.el
;;
;; Bugs
;;
;;     Soft dependency on unpublished popup-volatile.
;;
;;     `message' is a subr.  Macros such as `express-with-message-logonly'
;;     will only affect calls to `message' from Lisp.
;;
;; TODO
;;
;;     Aliases are not turning on from customize setting alone.  The
;;     variable express-install-short-aliases does not seem to be
;;     set after loading `custom-file'.
;;
;;     Truncation options based on string-utils.el
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
  ;; for callf, callf2, assert, gensym
  (require 'cl))

(autoload 'notify            "notify"         "Notify TITLE, BODY via `notify-method'.")
(autoload 'todochiku-message "todochiku"      "Send a message via growl, snarl, etc.")
(autoload 'popup-tip         "popup"          "Show a tooltip of STRING at POINT.")
(autoload 'popup-volatile    "popup-volatile" "Create a volatile tooltip using `popup-tip'.")

(require 'string-utils nil t)

;;; declarations

(declare-function string-utils-propertize-fillin "string-utils.el")
(declare-function express-message                "express.el")

;;; customizable variables

;;;###autoload
(defgroup express nil
  "Alternatives to `message'."
  :version "0.6.0"
  :link '(emacs-commentary-link :tag "Commentary" "express")
  :link '(url-link :tag "GitHub" "http://github.com/rolandwalker/express")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/Express")
  :prefix "express-"
  :group 'extensions)

(defcustom express-message-seconds 2
  "Default period to display express messages."
  :type 'integer
  :group 'express)

(defcustom express-message-notify-title "Emacs"
  "Default title for messages presented by `express-message-notify'."
  :type 'integer
  :group 'express)

(defcustom express-face 'highlight
  "Face to use for highlighting express messages."
  :group 'express)

;;;###autoload
(defcustom express-install-short-aliases nil
  "Install short aliases such as `message-nolog' for `express-message-nolog'."
  :type 'boolean
  :group 'express)

;;; aliases and fsets

(fset 'express-message (symbol-function 'message))

;;;###autoload
(progn
  (defun express-install-aliases (&optional arg)
    "Install aliases outside the \"express-\" namespace.

With optional negative ARG, uninstall aliases.

The following aliases will be installed:

   message-nolog      for   express-message-nolog
   message-logonly    for   express-message-logonly
   message-noformat   for   express-message-noformat
   message-highlight  for   express-message-highlight
   message-insert     for   express-message-insert
   message-notify     for   express-message-notify
   message-popup      for   express-message-popup
   message-temp       for   express-message-temp"
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
                                        (intern-soft (format "express-message-%s" sym))))
             (fmakunbound (intern (format "message-%s" sym))))
           (when (ignore-errors
                   (eq (symbol-function (intern-soft (format "with-message-%s" sym)))
                                        (intern-soft (format "express-with-message-%s" sym))))
             (fmakunbound (intern (format "with-message-%s" sym))))))
        (t
         (dolist (sym syms)
           (defalias (intern (format "message-%s" sym)) (intern (format "express-message-%s" sym)))
           (defalias (intern (format "with-message-%s" sym)) (intern (format "express-with-message-%s" sym)))))))))

;;;###autoload
(when express-install-short-aliases
  (express-install-aliases))

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

;;; utility macros

(defmacro express--with-fset (func1 func2 &rest body)
  "Execute BODY, within which FUNC1 (a symbol) is `fset' to FUNC2.

This has dynamic (not lexical) effect.  FUNC2 may be a lambda.

This is portable to versions of Emacs without dynamic `flet`."
  (declare (debug t) (indent 2))
  (let ((o (gensym "--function--")))
    `(let ((,o (ignore-errors (symbol-function ,func1))))
       (fset ,func1 ,func2)
       (unwind-protect
           (progn ,@body)
         (when ,o
           (fset ,func1 ,o))))))

;;; utility functions

;;;###autoload
(defun express-message-noformat (content &rest _ignored)
  "An alternative for `message' which assumes a pre-formatted CONTENT string.

Any arguments after CONTENT are ignored, meaning this is not
functionally equivalent to `message'.  However, flet'ing
`message' to this function is safe in the sense that it does not
call `message' directly."
  (if (null content)
      (express-message content)
    ;; else
    (assert (stringp content) nil "CONTENT must be a string")
    (express-message (replace-regexp-in-string "\\(%\\)" "%\\1" content))))

(defun express-message-maybe-formatted (&rest args)
  "Dispatch `message' according to the variable `express-message-preformatted'.

Formatting is not performed if `express-message-preformatted' is
bound and non-nil.

When formatting is performed, ARGS are treated as for `message', including
a format-string.  When formatting is not performed, only the first element
of ARGS is respected.  It should be a pre-formatted string."
    (if (and (boundp 'express-message-preformatted)
             express-message-preformatted)
        (apply 'express-message-noformat args)
      ;; else
      (apply 'express-message args)))

(defun express--message-insert-1 (msg)
  "Internal driver for `express-message-insert'.

Inserts pre-formatted MSG at the current position with line feeds as needed."
  (unless (eq (line-beginning-position) (point))
    (insert "\n"))
  (insert msg)
  (unless (eq (line-beginning-position) (point))
    (insert "\n")))

;;;###autoload
(defun express-message-logonly (&rest args)
  "An flet'able replacement for `message' which logs but does not echo.

ARGS are as for `message', including a format-string."
  (when message-log-max
    (if (get-buffer "*Messages*")
        ;; don't worry about truncating the message log, some standard
        ;; call to `message' will catch up with it later.
        (with-current-buffer "*Messages*"
          (save-excursion
            (goto-char (point-max))
            (let ((msg (if (and (boundp 'express-message-preformatted)
                                express-message-preformatted)
                           (car args)
                         (apply 'format args)))
                  (inhibit-read-only t))
              (express--message-insert-1 msg)
              msg)))
      ;; else
      (let ((current-msg (current-message)))
        (apply 'express-message-maybe-formatted args)
        (express-message current-msg)))))

;;;###autoload
(defun express-message-insert (&rest args)
  "An flet'able replacement for `message' which inserts text instead of echoing.

ARGS are as for `message', including a format-string."
  (let ((msg (if (and (boundp 'express-message-preformatted) express-message-preformatted) (car args) (apply 'format args)))
        (express-message-preformatted t))
    (express-message-logonly msg)
    (express--message-insert-1 msg)
    msg))

;;;###autoload
(defun express-message-string (&rest args)
  "An flet'able replacement for `message' which returns a string instead of echoing.

Newline is appended to the return value as with `message'.

ARGS are as for `message', including a format-string."
  (let ((msg (if (and (boundp 'express-message-preformatted) express-message-preformatted) (car args) (apply 'format args))))
    (concat msg "\n")))

;;;###autoload
(defun express-message-nolog (&rest args)
  "An flet'able replacement for `message' which echos but does not log.

ARGS are as for `message', including a format-string."
  (let ((message-log-max nil))
    (apply 'express-message-maybe-formatted args)))

;;;###autoload
(defun express-message-temp (&rest args)
  "An flet'able replacement for `message' which displays temporarily.

The display time is governed by `express-message-seconds'.

ARGS are as for `message', including a format-string."
  (when (car args)
    (let ((current-msg (current-message))
          (retval (apply 'express-message-maybe-formatted args)))
      (when (numberp express-message-seconds)
        (sit-for express-message-seconds))
      (express-message (or current-msg ""))
      retval)))

;;;###autoload
(defun express-message-popup (&rest args)
  "An flet'able replacement for `message' which uses popups instead of echoing.

The functions `popup-volatile' and `popup' are attempted in
order to create a popup.  If both functions fail, the message
content will appear in the echo area as usual.

ARGS are as for `message', including a format-string."
  (let ((msg (if (and (boundp 'express-message-preformatted) express-message-preformatted) (car args) (apply 'format args)))
        (express-message-preformatted t))
    (express-message-logonly msg)
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
                (express-message-nolog msg)))))))

;;;###autoload
(defun express-message-notify (&rest args)
  "An flet'able replacement for `message' which uses notifications instead echo.

The following functions are attempted in order call system
notifications: `notify' and `todochiku-message'.  If both
functions fail, the message content will appear in the echo
area as usual.

ARGS are as for `message', including a format-string."
  (let ((msg (if (and (boundp 'express-message-preformatted) express-message-preformatted) (car args) (apply 'format args)))
        (express-message-preformatted t))
    (express-message-logonly msg)
    (condition-case nil
        (progn
          (notify express-message-notify-title msg)
          msg)
      (error nil
         (condition-case nil
             (progn
               (todochiku-message express-message-notify-title msg "")
               msg)
           (error nil
                (express-message-nolog msg)))))))

;;;###autoload
(defun express-message-highlight (&rest args)
  "An flet'able replacement for `message' which echos highlighted text.

Text without added properties is logged to the messages buffer as
usual.

ARGS are as for `message', including a format-string."
  (let ((retval (apply 'express-message-logonly args)))
    (when (stringp (car args))
      (callf concat (car args) (propertize " " 'display '(space :align-to right-margin)))
      (callf string-utils-propertize-fillin (car args) 'face (append (face-attr-construct 'default)
                                                                     (face-attr-construct express-face))))
    (let ((message-log-max nil))
      (apply 'express-message-maybe-formatted args))
    retval))

;;;###autoload
(defun express (content &optional quiet seconds nocolor log notify popup)
  "Transiently and noticeably display CONTENT in the echo area.

CONTENT should be a pre-`format'ted if it is a string.

CONTENT will be coerced to a string if it is not a string.

Optional QUIET suppresses the bell, which is on by default.

Optional SECONDS determines the number of seconds CONTENT will be
displayed before reverting to the previous content of the echo
area.  Default is `express-message-seconds'.  If SECONDS is 0, or
non-numeric, the message is not timed out, and remains visible
until the next write to the echo area.

Optional NOCOLOR suppresses coloring the message with face held
in the variable `express-face'.

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

The behavior of `express' is very different from `message':

  - CONTENT must already be formatted.

  - Non-strings are accepted for CONTENT.

  - The content is displayed with added color.

  - The bell is rung.

  - CONTENT is not written to the messages buffer (log).

  - After display, the previous contents of the echo area are
    restored.

The following forms using `message` and `express` are equivalent:

   (message \"hello, %s\" name)
   (express (format \"hello, %s\" name) 'quiet 0 'nocolor 'log)"
  (unless (stringp content)
    (if (fboundp 'string-utils-stringify-anything)
        (callf string-utils-stringify-anything content)
      ;; else
      (callf2 format "%s" content)))
  (let ((express-message-seconds express-message-seconds)
        (message-log-max message-log-max)
        (colored-content content)
        (express-message-preformatted t))
    (unless (or quiet
                (eq log 'log-only)
                (eq notify 'replace-echo)
                (eq popup 'replace-echo))
      (ding t))
    (when log
      (express-message-logonly content))
    (when notify
      (express-message-notify content))
    (when popup
      (express-message-popup content))
    (setq message-log-max nil)
    (cond
      ((numberp seconds)
       (setq express-message-seconds seconds))
      ((not (null seconds))
       (setq express-message-seconds 0)))
    (unless (or (eq log 'log-only)
                (eq notify 'replace-echo)
                (eq popup 'replace-echo))
      (unless (stringp content)
        (callf2 format "%s" content))
      (unless nocolor
        (callf concat colored-content (propertize " " 'display '(space :align-to right-margin)))
        (callf string-utils-propertize-fillin colored-content 'face (append (face-attr-construct 'default)
                                                                            (face-attr-construct express-face))))
      (if (and (numberp express-message-seconds)
               (> express-message-seconds 0))
          (express-message-temp colored-content)
        (express-message-noformat colored-content))))
  content)

;;;###autoload
(defun* express* (content &key quiet seconds nocolor log notify popup)
  "An alternate version of `express' which uses Common Lisp semantics.

CONTENT, QUIET, SECONDS, NOCOLOR, LOG, NOTIFY, and POPUP are as
documented for `express'."
  (express content quiet seconds nocolor log notify popup))

;;;###autoload
(defmacro express-with-message-logonly (&rest body)
  "Execute BODY, redirecting the output of `message' to the log only.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(express--with-fset 'message 'express-message-logonly
     ,@body))

;;;###autoload
(defmacro express-with-message-nolog (&rest body)
  "Execute BODY, keeping the output of `message' from being added to the log.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(express--with-fset 'message 'express-message-nolog
     ,@body))

;;;###autoload
(defmacro express-with-message-highlight (&rest body)
  "Execute BODY, highlighting the output of `message'.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(express--with-fset 'message 'express-message-highlight
     ,@body))

;;;###autoload
(defmacro express-with-message-notify (&rest body)
  "Execute BODY, redirecting the output of `message' to system notifications.

notify.el or todochiku.el may be used to provide the interface to
system notifications.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(express--with-fset 'message 'express-message-notify
     ,@body))

;;;###autoload
(defmacro express-with-message-popup (&rest body)
  "Execute BODY, redirecting the output of `message' to popups.

popup.el is required.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(express--with-fset 'message 'express-message-popup
     ,@body))

;;;###autoload
(defmacro express-with-message-insert (&rest body)
  "Execute BODY, redirecting the output of `message' to `insert'.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(express--with-fset 'message 'express-message-insert
     ,@body))

;;;###autoload
(defmacro express-with-message-string (&rest body)
  "Execute BODY, capturing the output of `message' to a string.

Accumulated message output is returned.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  (let ((output (gensym "--with-message-string--"))
        (capfun (gensym "--with-message-func--")))
    `(let ((,output ""))
       (defun ,capfun (&rest args)
         (callf concat ,output (apply 'express-message-string args)))
       (express--with-fset 'message (function ,capfun)
         ,@body)
       ,output)))

;;;###autoload
(defmacro express-with-message-temp (&rest body)
  "Execute BODY, making all `message' output temporary.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(express--with-fset 'message 'express-message-temp
     ,@body))

;;;###autoload
(defmacro express-with-message-noformat (&rest body)
  "Execute BODY, keeping `message' from formatting its arguments.

All arguments to `message' after the first one will be dropped.

Note that since `message' is a subr, only calls to `message' from
Lisp will be affected."
  (declare (indent 0) (debug t))
  `(express--with-fset 'message 'express-message-noformat
     ,@body))

(provide 'express)

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

;;; express.el ends here
