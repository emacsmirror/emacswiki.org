;;; alert.el --- Alternatives to `message'
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/alert
;; URL: http://raw.github.com/rolandwalker/alert/master/alert.el
;; Version: 0.5.4
;; Last-Updated: 5 Sep 2012
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
;;    (require 'alert)
;;    (alert "important message")
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
;; `message' in almost every respect, displaying with sound and
;; visual highlight, and not writing to the log.  See the `alert'
;; docstring for details.
;;
;; The following functions provided by this library are drop-in
;; alternatives to `message':
;;
;;    `alert-message-nolog'
;;    `alert-message-logonly'
;;    `alert-message-highlight'
;;    `alert-message-insert'
;;    `alert-message-notify'
;;    `alert-message-popup'
;;    `alert-message-temp'
;;
;; which may be useful in an `flet' construct to control messaging.
;; For example, the following code would redirect messages from a very
;; chatty library to the log:
;;
;;    (flet ((message (&rest args)
;;                    (apply 'alert-message-logonly args)))
;;      (require 'very-chatty-library))
;;
;; The same method may also be handy with `defadvice':
;;
;;    (defadvice very-chatty-function (around very-chatty-redirect activate)
;;       (flet ((message (&rest args)
;;                       (apply 'alert-message-logonly args)))
;;         ad-do-it))
;;
;; Similarly, important messages may be redirected to a more visible
;; form:
;;
;;    (defadvice an-important-function (around an-important-function activate)
;;       (flet ((message (&rest args)
;;                       (apply 'alert-message-notify args)))
;;         ad-do-it))
;;
;; To use `alert', place the alert.el library somewhere Emacs can find
;; it, and add the following to your ~/.emacs file:
;;
;;    (require 'alert)
;;    (alert-install-aliases)     ; optionally
;;
;; See Also
;;
;;    M-x customize-group RET alert RET
;;    M-x customize-group RET notify RET
;;    M-x customize-group RET popup RET
;;
;; Notes
;;
;;    The function `message-noformat' is also available, but it is
;;    not quite a drop-in replacement for `message'.
;;
;;    Some of the functions require the availability of notify.el,
;;    todochiku.el or popup.el.  In all cases, the function will
;;    degrade to an ordinary message if the external library is not
;;    present.
;;
;; Compatibility and Requirements
;;
;;    Tested on GNU Emacs versions 23.3 and 24.1
;;
;;    Uses if present: string-utils.el, notify.el, todochiku.el,
;;                     popup.el
;;
;; Bugs
;;
;;    Soft dependency on unpublished popup-volatile.
;;
;; TODO
;;
;;    Default icons and timeouts for notifications.
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

;; for callf
(eval-when-compile
  (require 'cl))

(autoload 'notify            "notify"         "Notify TITLE, BODY via `notify-method'.")
(autoload 'todochiku-message "todochiku"      "Send a message via growl, snarl, etc.")
(autoload 'popup-tip         "popup"          "Show a tooltip of STRING at POINT.")
(autoload 'popup-volatile    "popup-volatile" "Create a volatile tooltip using `popup-tip'.")

(require 'string-utils nil t)

(declare-function string-utils-propertize-fillin "string-utils.el")
(declare-function alert-message                  "alert.el")

;;; customizable variables

;;;###autoload
(defgroup alert nil
  "Alternatives to `message'."
  :version "0.5.4"
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

;;; aliases and fsets

(fset 'alert-message (symbol-function 'message))

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
(defun alert (content &optional quiet seconds nocolor log notify popup)
  "Transiently and noticeably display CONTENT in the echo area.

CONTENT should be a pre-formatted string.

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

  - The content is displayed with added color.

  - The bell is rung.

  - CONTENT is not written to the messages buffer (log).

  - After display, the previous contents of the echo area are
    restored.

The following forms using `message` and `alert` are equivalent:

   (message \"hello, %s\" name)
   (alert (format \"hello, %s\" name) 'quiet 0 'nocolor 'log)"
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

;;;###autoload
(defun alert-message-noformat (content &rest args)
  "An alternative for `message' which assumes a pre-formatted CONTENT string.

ARGS are ignored, meaning this is not functionally equivalent to `message'.
However, flet'ing `message' to this function is safe in the sense that
it does not call `message' directly."
  (if (null content)
      (alert-message content)
    ;; else
    (assert (stringp content) nil "CONTENT must be a string")
    (alert-message (replace-regexp-in-string "%" "%%" content))))

;;;###autoload
(defun alert-message-nolog (&rest args)
  "An flet'able replacement for `message' which echos but does not log.

ARGS are as for `message', including a format-string."
  (let ((message-log-max nil))
    (apply 'alert-message-maybe-formatted args)))

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
(defun alert-message-insert (&rest args)
  "An flet'able replacement for `message' which inserts text instead of echoing.

ARGS are as for `message', including a format-string."
  (let ((msg (if (and (boundp 'alert-message-preformatted) alert-message-preformatted) (car args) (apply 'format args)))
        (alert-message-preformatted t))
    (alert-message-logonly msg)
    (alert--message-insert-1 msg)
    msg))

(defun alert--message-insert-1 (msg)
  "Internal driver for `alert-message-insert'.

Inserts pre-formatted MSG at the current position with line feeds as needed."
  (unless (eq (line-beginning-position) (point))
    (insert "\n"))
  (insert msg)
  (unless (eq (line-beginning-position) (point))
    (insert "\n")))

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

;;; interactive commands

;;;###autoload
(defun alert-install-aliases ()
  "Install aliases outside the \"alert-\" namespace.

The following aliases will be installed:

   message-nolog      for   alert-message-nolog
   message-logonly    for   alert-message-logonly
   message-noformat   for   alert-message-noformat
   message-highlight  for   alert-message-highlight
   message-insert     for   alert-message-insert
   message-notify     for   alert-message-notify
   message-popup      for   alert-message-popup
   message-temp       for   alert-message-temp"
  (interactive)
  (defalias 'message-nolog      'alert-message-nolog)
  (defalias 'message-logonly    'alert-message-logonly)
  (defalias 'message-highlight  'alert-message-highlight)
  (defalias 'message-insert     'alert-message-insert)
  (defalias 'message-noformat   'alert-message-noformat)
  (defalias 'message-notify     'alert-message-notify)
  (defalias 'message-popup      'alert-message-popup)
  (defalias 'message-temp       'alert-message-temp))

(provide 'alert)

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
;; LocalWords: noformat logonly nolog flet'able NOCOLOR nocolor fsets
;; LocalWords: flet todochiku ARGS args callf
;;

;;; alert.el ends here
