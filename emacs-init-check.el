;;;; emacs-init-check.el --- Automatic init file checker
;; Time-stamp: <2012-07-26 23:22:50 rubikitch>

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: lisp, vc
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/emacs-init-check.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;;
;; Do error check your ~/.emacs.el automatically by:
;;   emacs -batch --eval '(setq debug-on-error t)' -l ~/.emacs
;; It is desirable after VC check-in.
;;
;; If your init file contains error, pop up backtrace.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `emacs-init-check'
;;    Do init file check.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `auto-emacs-init-check-file-regexp'
;;    *Do `emacs-init-check' after files matching this regexp are checked in to VC.
;;    default = "/\\.emacs\\.d/"

;;; Installation:
;;
;; Put emacs-init-check.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'emacs-init-check)
;; (add-hook 'vc-checkin-hook 'auto-emacs-init-check)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET emacs-init-check RET
;;


;;; History:

;; See http://www.rubyist.net/~rubikitch/gitlog/emacs-init-check.txt

;;; Code:

(defvar emacs-init-check-version "1.0")
(eval-when-compile (require 'cl))
(defgroup emacs-init-check nil
  "emacs-init-check"
  :group 'convenience)

(defvar auto-emacs-init-check t
  "If non-nil, enable emacs-init-check.")
(defcustom auto-emacs-init-check-file-regexp  "/\\.emacs\\.d/"
  "*Do `emacs-init-check' after files matching this regexp are checked in to VC."
  :type 'string  
  :group 'emacs-init-check)
(defvar auto-emacs-init-check-program-args
  (list (car command-line-args) "-batch" "--eval" "(setq debug-on-error t)" "-l" user-init-file)
  "Command line of `emacs-init-check'.")
(defun emacs-init-check ()
  "Do init file check."
  (interactive)
  (with-current-buffer (get-buffer-create "*emacs init check*")
    (erase-buffer)
    (buffer-disable-undo)
    (set-process-sentinel
     (apply 'start-process "emacs init check" (current-buffer)
            auto-emacs-init-check-program-args)
     (lambda (proc stat)
       (if (zerop (process-exit-status proc))
           (message "emacs-init-check exited normally.")
         (display-buffer (process-buffer proc)))))))
(defun auto-emacs-init-check ()
  "Do `emacs-init-check' automatically. It is intended for hook."
  (when (and auto-emacs-init-check buffer-file-name
             (string-match auto-emacs-init-check-file-regexp buffer-file-name))
    (emacs-init-check)))

(provide 'emacs-init-check)

;; How to save (DO NOT REMOVE!!)
;; (progn (git-log-upload) (emacswiki-post "emacs-init-check.el"))
;;; emacs-init-check.el ends here
