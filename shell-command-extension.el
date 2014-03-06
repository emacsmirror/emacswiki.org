;;; shell-command-extension.el --- Some extension for shell-command

;; Filename: shell-command-extension.el
;; Description: Some extension for shell-command
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-07 18:11:01
;; Version: 0.1
;; Last-Updated: 2009-02-07 18:11:01
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/shell-command-extension.el
;; Keywords: shell-command
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
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
;; Some extension for shell-command
;;

;;; Installation:
;;
;; Put shell-command-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'shell-command-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2009/02/07
;;      * First released.
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


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defcustom my-root-passwd nil
;;   "The passwd for super user to execute command."
;;   :type 'string
;;   :group 'shell-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Advices ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice shell-command (around surpress-popup-window)
  "This advice is surpress popup window for show information."
  (let ((old-window-configuration (current-window-configuration)))
    ad-do-it
    (set-window-configuration old-window-configuration)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interacitve Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shell-command-unique (cmd &rest cmd-args)
  "Execute shell command asynchronously with unique buffer.
Argument CMD the main command."
  (interactive)
  (let* ((time-now (current-time))
         (command-buffer (format "*Asynchronously Command <%s-%s-%s>"
                                 (nth 0 time-now)
                                 (nth 1 time-now)
                                 (nth 2 time-now)))
         command-args)
    ;; Concat command arguments.
    (dolist (args cmd-args)
      (setq command-args (concat command-args " " args)))
    (setq cmd (concat cmd command-args))
    ;; Execute command.
    (shell-command cmd command-buffer)
    ;; Try to input password.
    ;; (sit-for 1)
    ;; (with-current-buffer command-buffer
    ;;   (goto-char (point-min))
    ;;   (if (search-forward-regexp (format "\\[sudo\\] password for %s: $" (user-login-name)) nil t)
    ;;       (progn
    ;;         (insert my-root-passwd)
    ;;         (comint-send-input))
    ;;     (call-interactively 'move-end-of-line)))
    ))

(defun shell-aliase (alias)
  "Execute shell aliase in term.
Argument ALIAS the alias name that define in bash."
  (interactive)
  (shell-command-unique (format "bash -c -i %s &" alias)))

(defun shell-command-surpress-popup-window (command &optional output-buffer error-buffer)
  "This function is like `shell-command', but surpress popup-window."
  (interactive
   (list
    (read-shell-command "Shell command: " nil nil
                        (and buffer-file-name
                             (file-relative-name buffer-file-name)))
    current-prefix-arg
    shell-command-default-error-buffer))
  (ad-enable-advice 'shell-command 'around 'surpress-popup-window)
  (ad-activate 'shell-command)
  (shell-command command output-buffer error-buffer)
  (ad-disable-advice 'shell-command 'around 'surpress-popup-window)
  (ad-activate 'shell-command))

(provide 'shell-command-extension)

;;; shell-command-extension.el ends here
