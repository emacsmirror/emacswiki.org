;;; multi-shell.el --- Multi-Shell Manager

;; Filename: multi-shell.el
;; Description: Multi-Shell Manager
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-10 12:03:20
;; Version: 0.2.2
;; Last-Updated: 2009-06-29 18:04:34
;;           By: Andy Stewart
;; URL:
;; Keywords: multi-shell, shell
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `shell' `ansi-color' `cl'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Multi-Shell Manager
;;
;; Here, have below command you can use:
;;
;;      `multi-shell-new'                create a new shell buffer.
;;      `multi-shell-next'               switch to next shell buffer.
;;      `multi-shell-prev'               switch to previous shell buffer.
;;      `multi-shell-current-directory'  create a new shell with current-directory.
;;
;;      choose your like key bind above command. ;)
;;
;; This package extension `shell-mode' with below features:
;;
;; 1 ->
;;      Can create or navigation shell buffers quickly.
;;
;; 2 ->
;;      Close buffer when type command `exit' in shell buffer.
;;
;; 3 ->
;;      Interrupt sub-process before kill shell buffer forcible.
;;
;; 4 ->
;;      Revert window configuration before completion window popup.
;;

;;; Installation:
;;
;; Put multi-shell.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'multi-shell)
;;
;; And setup program that `multi-shell-command' will need:
;;
;; (setq multi-shell-command "/bin/bash")
;;
;;      or setup like me "/bin/zsh" ;)
;;

;;; Customize:
;;
;; `multi-shell-command' default is nil, so when create new shell buffer,
;; send environment variable of `SHELL' (`ESHELL', `/bin/sh').
;;
;; And you can set it to your like, like me: ;-)
;;
;; (setq multi-shell-command "/bin/zsh")
;;
;; `multi-shell-use-ansi-color' whether translate ANSI escape sequences into faces.
;;
;; `multi-shell-try-create' will create a new shell buffer when
;; haven't any shell buffers exist.
;;
;; `multi-shell-default-dir' default is `~/', only use when current buffer
;; is in non-exist directory.
;;
;; `multi-shell-bottom-window-height' is window height when use function
;; `multi-shell-current-directory'.
;;
;; `multi-shell-buffer-name' is a name of shell buffer.
;;
;; `multi-shell-revert-window-after-complete' is revert window layout after
;; `comint-dynamic-complete'.
;;
;; `multi-shell-revert-window-keystroke' is the keystroke to revert window layout.
;; bind it with your like keystroke, default is `SPACE'.
;;
;; All above setup can customize by:
;;      M-x customize-group RET multi-shell RET
;;


;;; Change log:
;;
;; 2009/06/29
;;      Fix regexp bug.
;;
;; 2008/12/17
;;      Fix the device bug of `multi-shell-revert-window-after-complete'.
;;
;; 2008/12/10
;;      Advice `comint-dynamic-complete' to revers window configuration.
;;
;;      First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'shell)
(require 'ansi-color)
(require 'cl)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar multi-shell-window-configuration-before-complete nil
  "The window configuration will record before complete window popup first time,
and window configuration will revert when you type `multi-shell-revert-window-keystroke' next time.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice comint-dynamic-complete (around record-window-configuration-before-popup-window)
  "This advice record window configuration before popup complete window.
And just record when variable `multi-shell-window-configuration-before-complete' is nil."
  (if (and (boundp 'multi-shell-window-configuration-before-complete)
           multi-shell-window-configuration-before-complete)
      ad-do-it
    (let (window-configuration-before-do
          window-configuration-after-do)
      (setq window-configuration-before-do (current-window-configuration))
      ad-do-it
      (setq window-configuration-after-do (current-window-configuration))
      (unless (equal window-configuration-before-do window-configuration-after-do)
        (setq multi-shell-window-configuration-before-complete window-configuration-before-do)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup multi-shell nil
  "Multi-Shell Manager"
  :group 'shell)

(defcustom multi-shell-command nil
  "The command that multi-shell will use.
If nil, will try to get value of
environment variable `SHELL' or `ESHELL'."
  :type 'string
  :group 'multi-shell)

(defcustom multi-shell-use-ansi-color t
  "Whether translate ANSI escape sequences into faces.
And default is t."
  :type 'boolean
  :group 'multi-shell)

(defcustom multi-shell-try-create t
  "Try to create a new shell buffer when switch.

When use `multi-shell-next' or `multi-shell-prev' to switch shell buffer,
try to create a new shell buffer if haven't any shell buffers exist."
  :type 'boolean
  :group 'multi-shell)

(defcustom multi-shell-default-dir "~/"
  "The default directory for create shell buffer,
when current local directory is no exist."
  :type 'string
  :group 'multi-shell)

(defcustom multi-shell-buffer-name "multi-shell"
  "The name of shell buffer."
  :type 'string
  :group 'multi-shell)

(defcustom multi-shell-bottom-window-height -13
  "The height of bottom window create."
  :type 'integer
  :group 'multi-shell)

(defcustom multi-shell-revert-window-keystroke "SPC"
  "The keystroke that revert window after `comint-dynamic-complete'.
Default is `SPACE."
  :type 'string
  :group 'multi-shell)

(defcustom multi-shell-revert-window-after-complete t
  "Revert window Lat's after `comint-dynamic-complete'.

Default, type `TAB' (comint-dynamic-complete) in `shell-mode',
will popup a window complete, but when you type TAB many times,
`shell-mode' can't revert window layout before you type TAB.

So it's too bad use `comint-dynamic-complete' in complex window layout,
it can break your window layout and can't revert.

If you type `TAB' in `multi-shell' buffer, and window layout
will revert when you type keystroke `multi-shell-revert-window-keystroke' next time."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (if value
             (progn
               (define-key shell-mode-map
                 (read-kbd-macro multi-shell-revert-window-keystroke)
                 'multi-shell-insert-or-revert-window-layout)
               (ad-enable-advice 'comint-dynamic-complete 'around 'record-window-configuration-before-popup-window))
           (define-key shell-mode-map
             (read-kbd-macro multi-shell-revert-window-keystroke)
             'self-insert-command)
           (ad-disable-advice 'comint-dynamic-complete 'around 'record-window-configuration-before-popup-window))
         (ad-activate 'comint-dynamic-complete))
  :group 'multi-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multi-shell-new ()
  "Create a new multi-shell buffer."
  (interactive)
  (let* ((shell-buffer (multi-shell-get-buffer)))
    (set-buffer shell-buffer)
    ;; Add `ansi-color' to shell-mode
    (if multi-shell-use-ansi-color
        (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
      (remove-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))
    ;; Load mode
    (save-window-excursion
      (shell (buffer-name shell-buffer)))
    ;; Handle shell buffer close
    (multi-shell-handle-close)
    ;; Handle revert window layout after complete
    (multi-shell-handle-revert-window-after-complete)
    ;; Switch shell buffer
    (switch-to-buffer shell-buffer)
    ;; Add hook to be sure `shell' interrupt subjob before buffer killed
    (add-hook 'kill-buffer-hook 'multi-shell-handle-kill-buffer)
    ))

(defun multi-shell-get-buffer ()
  "Get shell buffer."
  (let* ((shell-list-length (length (multi-shell-list)))          ;get length of shell list
         (index (if shell-list-length (1+ shell-list-length) 1))) ;setup new shell index
    (with-temp-buffer
      ;; switch to current local directory,
      ;; if in-existence, switch to `multi-shell-default-dir'.
      (cd (or default-directory (expand-file-name multi-shell-default-dir)))
      ;; adjust value N when max index of shell buffer is less than length of shell list
      (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-shell-buffer-name index)))
        (incf index))
      ;; Set explicit shell program
      (setq explicit-shell-file-name (or multi-shell-command
                                         (getenv "SHELL")
                                         (getenv "ESHELL")
                                         "/bin/sh"))
      ;; Return buffer
      (get-buffer-create (format "*%s<%s>*" multi-shell-buffer-name index))
      )))

(defun multi-shell-handle-close ()
  "This function for close current shell buffer.
When `exit' from shell buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun multi-shell-handle-kill-buffer ()
  "Function that hook `kill-buffer-hook'."
  ;; Interrupt the current subjob
  ;; when have alive process with current shell buffer
  (when (and (eq major-mode 'shell-mode)
             (comint-check-proc (current-buffer)))
    (comint-interrupt-subjob)))

(defun multi-shell-handle-revert-window-after-complete ()
  "Handle window configuration after complete."
  (if multi-shell-revert-window-after-complete
      (progn
        (define-key shell-mode-map
          (read-kbd-macro multi-shell-revert-window-keystroke)
          'multi-shell-insert-or-revert-window-layout)
        (ad-enable-advice 'comint-dynamic-complete 'around 'record-window-configuration-before-popup-window))
    (define-key shell-mode-map
      (read-kbd-macro multi-shell-revert-window-keystroke)
      'self-insert-command)
    (ad-disable-advice 'comint-dynamic-complete 'around 'record-window-configuration-before-popup-window))
  (ad-activate 'comint-dynamic-complete))

(defun multi-shell-insert-or-revert-window-layout (&optional arg)
  "Handle insert self or window configuration revert."
  (interactive "P")
  (if multi-shell-window-configuration-before-complete
      (progn
        (set-window-configuration multi-shell-window-configuration-before-complete)
        (setq multi-shell-window-configuration-before-complete nil)
        (message "Have revert window layout."))
    (self-insert-command (or arg 1))))

(defun multi-shell-list ()
  "The shell buffers presently active."
  ;; Autload command `remove-if-not'.
  (autoload 'remove-if-not "cl-seq")
  (sort
   (remove-if-not (lambda (b)
                    (setq case-fold-search t)
                    (string-match
                     (format "^\\\*%s<[0-9]+>\\\*$" multi-shell-buffer-name)
                     (buffer-name b)))
                  (buffer-list))
   (lambda (a b)
     (< (string-to-number
         (cadr (split-string (buffer-name a) "[<>]")))
        (string-to-number
         (cadr (split-string (buffer-name b)  "[<>]")))))))

(defun multi-shell-switch (direction offset)
  "Switch to shell buffer.

If DIRECTION is `NEXT', switch to next shell buffer.
if DIRECTION is `PREVIOUS', switch to previous shell buffer.

Default OFFSET is 1.

If option `multi-shell-try-create' is non-nil, will create a new shell buffer
if have any shell buffer exist."
  (let (shells this-buffer)
    (setq shells (multi-shell-list))
    (if (consp shells)
        (progn
          (setf (cdr (last shells)) shells)
          (setq this-buffer (position (current-buffer) (multi-shell-list)))
          (if this-buffer
              (if (eql direction 'NEXT)
                  (switch-to-buffer (nth (+ this-buffer offset) shells))
                (switch-to-buffer (nth (+ (- (length (multi-shell-list)) offset)
                                          this-buffer) shells)))
            (switch-to-buffer (car shells))))
      (if multi-shell-try-create
          (progn
            (multi-shell-new)
            (message "Create a new `multi-shell' buffer."))
        (message "Haven't any `multi-shell' buffer exist.")))))

(defun multi-shell-next (&optional offset)
  "Switch to next shell buffer."
  (interactive "P")
  (multi-shell-switch 'NEXT (or offset 1)))

(defun multi-shell-prev (&optional offset)
  "Switch to previous shell buffer."
  (interactive "P")
  (multi-shell-switch 'PREVIOUS (or offset 1)))

(defun multi-shell-current-directory ()
  "Open shell that start at current directory."
  (interactive)
  (split-window-vertically multi-shell-bottom-window-height)
  (other-window 1)
  (multi-shell-new))

(provide 'multi-shell)

;;; multi-shell.el ends here

;;; LocalWords:  multi SPC
