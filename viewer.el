;;; viewer.el --- View-mode extension
;; $Id: viewer.el,v 1.7 2012/01/16 14:46:49 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: view, extensions
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/viewer.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Viewer is view-mode extension. View-mode is finger-friendly than
;; normal editing because of many alphabetical commands. Let's use
;; view-mode more to protect our pinky.

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `view-mode-by-default-regexp'
;;    *Regexp of file name to open by `view-mode'.
;;    default = nil
;;  `viewer-aggressive-minimum-size'
;;    *Minimum file size to enable aggressive view-mode.
;;    default = 100
;;  `viewer-aggressive-writable'
;;    *When non-nil, aggressive view-mode buffer is writable.
;;    default = t
;;  `viewer-modeline-color-unwritable'
;;    *Modeline color when file is not writable.
;;    default = "tomato"
;;  `viewer-modeline-color-view'
;;    *Modeline color for `view-mode'.
;;    default = "orange"

;; This package provides:
;; * specify files to open by `view-mode' by default.
;; * stay in `view-mode' in unwritable files.
;; * indicate `view-mode' by modeline color.
;; * override default `view-mode-map' for major modes.
;; * open ANY files by `view-mode' for enthusiasts.

;;; Installation:

;; Put viewer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'viewer)
;; (viewer-stay-in-setup)
;; (setq viewer-modeline-color-unwritable "tomato"
;;       viewer-modeline-color-view "orange")
;; (viewer-change-modeline-color-setup)
;;
;; If you want to open any file by `view-mode', add the following:
;; (viewer-aggressive-setup 'force)
;; 
;; Note that the command `view-mode' should be bound in easy-to-type
;; key.
;;
;; If you want to open any file matching a regexp, add the following:
;; (setq view-mode-by-default-regexp "/regexp-to-path")
;;
;; No need more.

;;; Customize:

;;
;; All of the above can customize by:
;;      M-x customize-group RET viewer RET
;;

;;; Tips:

;; Defining major-mode specific `view-mode' is useful.
;; See `define-overriding-view-mode-map'.
;;
;; [EVAL IT] (describe-function 'define-overriding-view-mode-map)


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x viewer-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of viewer.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "viewer.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x viewer-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; $Log: viewer.el,v $
;; Revision 1.7  2012/01/16 14:46:49  rubikitch
;; viewer-change-modeline-color-setup: Use window-configuration-change-hook
;;
;; Revision 1.6  2010/10/30 12:13:10  rubikitch
;; viewer-install-extension: call only if view-mode is enabled because (view-mode -1) on emacs24 calls it. mystery.
;;
;; Revision 1.5  2010/05/04 08:56:49  rubikitch
;; Added bug report command
;;
;; Revision 1.4  2009/03/11 08:52:12  rubikitch
;; fix bug in `aggressive-view-mode'
;;
;; Revision 1.3  2009/02/19 02:50:02  rubikitch
;; * view-mode by default
;; * update doc
;;
;; Revision 1.2  2009/02/18 23:57:11  rubikitch
;; * Aggressive view-mode
;; * Stay in view-mode
;; * Change mode-line color
;;
;; Revision 1.1  2009/02/18 15:04:14  rubikitch
;; Initial revision
;;

;;; Code:

(defvar viewer-version "$Id: viewer.el,v 1.7 2012/01/16 14:46:49 rubikitch Exp $")
(eval-when-compile (require 'cl))

;;;; (@* "Overriding view-mode keymap")
(defun define-overriding-view-mode-map-internal (mode-name key-bindings)
  (let ((mapsym (intern (concat (symbol-name mode-name) "-view-map")))
        (view-mode-sym (intern (concat (symbol-name mode-name) "-view-mode"))))
    (eval `(defvar ,mapsym nil))
    (let ((map (make-sparse-keymap)))
      (loop for (key . command) in key-bindings
            do (define-key map (read-kbd-macro key) command))
      (set mapsym map)
      (setq minor-mode-map-alist
          (cons (cons view-mode-sym map)
                (delete (assq view-mode-sym minor-mode-map-alist) minor-mode-map-alist)))
      nil)))

(defmacro define-overriding-view-mode-map (mode-name &rest key-bindings)
  "Define major-mode specific view-mode keymap.
Note that the car of an element of KEY-BINDINGS is `kbd' notation.

For example, to define `view-mode' keys for `emacs-lisp-mode':
 (define-overriding-view-mode-map emacs-lisp-mode
   (\"C-m\" . find-function)
   (\"C-j\" . find-variable))
"
  `(define-overriding-view-mode-map-internal ',mode-name ',key-bindings))

(defun viewer-install-extension ()
  (when view-mode
    (set (make-local-variable (intern (concat (symbol-name major-mode) "-view-mode")))
         t)))
(add-hook 'view-mode-hook 'viewer-install-extension)
(defun viewer-uninstall-extension ()
  (kill-local-variable (intern (concat (symbol-name major-mode) "-view-mode"))))
(defadvice view-mode-disable (before viewer activate)
  (viewer-uninstall-extension))

;;;; (@* "View-mode by default")
(defcustom view-mode-by-default-regexp nil
  "*Regexp of file name to open by `view-mode'."
  :type 'string  
  :group 'viewer)

(defun view-mode-by-default-setup ()
  (when (and buffer-file-name view-mode-by-default-regexp
             (string-match view-mode-by-default-regexp buffer-file-name))
    (view-mode 1)
    (message "view-mode by view-mode-by-default-regexp.")))
(add-hook 'find-file-hook 'view-mode-by-default-setup)

;;;; (@* "Aggressive view-mode")
(defgroup viewer nil
  "View-mode extensions"
  :group 'view)

(defcustom viewer-aggressive-minimum-size 100
  "*Minimum file size to enable aggressive view-mode."
  :type 'integer
  :group 'viewer)

(defcustom viewer-aggressive-writable t
  "*When non-nil, aggressive view-mode buffer is writable."
  :type 'boolean  
  :group 'viewer)
(defadvice find-file-noselect (after switch-to-view-file)
  (when (bufferp ad-return-value)
    (with-current-buffer ad-return-value
      (aggressive-view-mode))))

(defun aggressive-view-mode ()
  (when (and buffer-file-name
             (> (buffer-size) viewer-aggressive-minimum-size)
             (file-regular-p buffer-file-name))
        (view-mode 1)
        (setq buffer-read-only (not (file-writable-p buffer-file-name)))))

;; (describe-function 'find-file-noselect)
;; (memq 'aggressive-view-mode find-file-hook)

;; (viewer-aggressive-setup nil)
;; (viewer-aggressive-setup t)
;; (viewer-aggressive-setup 'force)
(defun viewer-aggressive-setup (arg)
  "Setup aggressive `view-mode'.

When ARG is t, all new files are opened by `view-mode'.
When ARG is 'force, enable `view-mode' even if file buffer is selected.
When ARG is nil, uninstall it."
  (case arg
    ('force
     (remove-hook 'find-file-hook 'aggressive-view-mode)
     (ad-enable-advice 'find-file-noselect 'after 'switch-to-view-file)
     (ad-update 'find-file-noselect))
    (nil
     (remove-hook 'find-file-hook 'aggressive-view-mode)
     (ad-disable-advice 'find-file-noselect 'after 'switch-to-view-file)
     (ad-update 'find-file-noselect))
    (t
     (add-hook 'find-file-hook 'aggressive-view-mode)
     (ad-disable-advice 'find-file-noselect 'after 'switch-to-view-file)
     (ad-update 'find-file-noselect))))

;;;; (@* "Stay in view-mode")
(defvar view-mode-force-exit nil)
(defmacro viewer-stay-in-unless-writable-advice (f)
  `(defadvice ,f (around viewer-stay-in-unless-writable activate)
     (if (and (buffer-file-name)
              (not view-mode-force-exit)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))


(defun view-mode-force-exit ()
  (interactive)
  (let ((view-mode-force-exit t)) (view-mode-exit)))
(add-hook 'edebug-setup-hook 'view-mode-force-exit)

(defun viewer-stay-in-setup ()
  "Setup stay-in view-mode.
Stay in `view-mode' when the file is unwritable."
  (viewer-stay-in-unless-writable-advice view-mode-exit)
  (viewer-stay-in-unless-writable-advice view-mode-disable))

;;;; (@* "Change mode-line color")
(defvar viewer-modeline-color-default (face-background 'modeline))
(defcustom viewer-modeline-color-unwritable "tomato"
  "*Modeline color when file is not writable."
  :type 'string
  :group 'viewer)
(defcustom viewer-modeline-color-view "orange"
  "*Modeline color for `view-mode'."
  :type 'string
  :group 'viewer)

(defun viewer-change-modeline-color ()
  (interactive)
  (when (eq (selected-window)
            (get-buffer-window (current-buffer)))
    (set-face-background
     'modeline
     (cond ((and buffer-file-name view-mode
                 (not (file-writable-p buffer-file-name)))
            viewer-modeline-color-unwritable)
           (view-mode
            viewer-modeline-color-view)
           (t
            viewer-modeline-color-default)))
    (force-mode-line-update)))

(defmacro viewer-change-modeline-color-advice (f)
  `(defadvice ,f (after change-mode-line-color activate)
     (viewer-change-modeline-color)))

(defun viewer-change-modeline-color-setup ()
  "Setup coloring modeline.
See also `viewer-modeline-color-unwritable' and `viewer-modeline-color-view'."
  (add-hook 'window-configuration-change-hook 'viewer-change-modeline-color)
  (viewer-change-modeline-color-advice toggle-read-only)
  (viewer-change-modeline-color-advice view-mode-enable)
  (viewer-change-modeline-color-advice view-mode-disable)
  (viewer-change-modeline-color-advice other-window)
  (defadvice select-window (around change-modeline-color activate)
    (let ((curwin (selected-window))
          (destwin (ad-get-arg 0)))
      ad-do-it
      (unless (or (called-interactively-p) (eq curwin destwin))
        (viewer-change-modeline-color))))
  (viewer-change-modeline-color-advice select-frame)
  nil)

;;;; Bug report
(defvar viewer-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar viewer-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of viewer.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"viewer.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun viewer-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   viewer-maintainer-mail-address
   "viewer.el"
   (apropos-internal "^view" 'boundp)
   nil nil
   viewer-bug-report-salutation))

(provide 'viewer)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "viewer.el")
;;; viewer.el ends here
