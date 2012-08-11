;;; auto-async-byte-compile.el --- Automatically byte-compile when saved
;; Time-stamp: <2010-12-05 11:36:45 rubikitch>

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: lisp, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/auto-async-byte-compile.el

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
;; Automatically byte-comple elisp files ASYNCHRONOUSLY when saved.
;; It invokes "emacs -Q --batch --eval '(setq load-path ...)'
;; -l ~/.emacs.d/initfuncs.el -f batch-byte-compile this-file.el"
;;
;; If you define your own macros, put them into ~/.emacs.d/initfuncs.el first.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `auto-async-byte-compile-mode'
;;    With no argument, toggles the auto-async-byte-compile-mode.
;;  `auto-async-byte-compile'
;;    Byte-compile this file asynchronously.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `auto-async-byte-compile-init-file'
;;    *Load this file when batch-byte-compile is running.
;;    default = "~/.emacs.d/initfuncs.el"
;;  `auto-async-byte-compile-display-function'
;;    *Display function of auto byte-compile result.
;;    default = (quote display-buffer)
;;  `auto-async-byte-compile-hook'
;;    *Hook after completing auto byte-compile.
;;    default = nil
;;  `auto-async-byte-compile-exclude-files-regexp'
;;    *Regexp of files to exclude auto byte-compile.
;;    default = nil

;;; Installation:
;;
;; Put auto-async-byte-compile.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET auto-async-byte-compile RET
;;


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x aabc/-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of auto-async-byte-compile.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "auto-async-byte-compile.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x aabc/-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; See http://www.rubyist.net/~rubikitch/gitlog/auto-async-byte-compile.txt

;;; Code:

(eval-when-compile (require 'cl))
(defgroup auto-async-byte-compile nil
  "auto-async-byte-compile"
  :group 'emacs)

(defcustom auto-async-byte-compile-init-file "~/.emacs.d/initfuncs.el"
  "*Load this file when batch-byte-compile is running."
  :type 'string  
  :group 'auto-async-byte-compile)

(defcustom auto-async-byte-compile-display-function 'display-buffer
  "*Display function of auto byte-compile result."
  :type 'symbol  
  :group 'auto-async-byte-compile)

(defcustom auto-async-byte-compile-hook nil
  "*Hook after completing auto byte-compile.
The variable `exitstatus' is exit status of byte-compile process."
  :type 'hook  
  :group 'auto-async-byte-compile)

(defcustom auto-async-byte-compile-exclude-files-regexp nil
  "*Regexp of files to exclude auto byte-compile."
  :type 'string  
  :group 'auto-async-byte-compile)

(defvar aabc/result-buffer " *auto-async-byte-compile*")

(define-minor-mode auto-async-byte-compile-mode
"With no argument, toggles the auto-async-byte-compile-mode.
With a numeric argument, turn mode on iff ARG is positive.

This minor-mode performs `batch-byte-compile' automatically after saving elisp files."
  nil "" nil
  (if auto-async-byte-compile-mode
      (add-hook 'after-save-hook 'auto-async-byte-compile nil 'local)
    (remove-hook 'after-save-hook 'auto-async-byte-compile 'local)))

(defun enable-auto-async-byte-compile-mode ()
  (auto-async-byte-compile-mode 1))

(defun auto-async-byte-compile ()
  "Byte-compile this file asynchronously."
  (interactive)
  (and buffer-file-name
       (string-match "\\.el$" buffer-file-name)
       (not (and auto-async-byte-compile-exclude-files-regexp
                 (string-match auto-async-byte-compile-exclude-files-regexp buffer-file-name)))
       (aabc/doit)))

(defun aabc/doit ()
  (with-current-buffer (get-buffer-create aabc/result-buffer)
    (erase-buffer))
  (set-process-sentinel
        (apply 'start-process
               (format "auto-async-byte-compile %s"
                       (file-name-nondirectory buffer-file-name))
               aabc/result-buffer
               (aabc/byte-compile-start-process-args buffer-file-name))
        'aabc/process-sentinel))

(defun aabc/process-sentinel (proc state)
  (let ((status (aabc/status (process-exit-status proc) aabc/result-buffer)))
    (aabc/display-function (process-name proc) (process-buffer proc) status)
    (run-hooks 'auto-async-byte-compile-hook)))

(defun aabc/display-function (process-name result-buffer status)
  (if (eq status 'normal)
      (message "%s completed" process-name)
    (funcall auto-async-byte-compile-display-function result-buffer)))

(defun aabc/status (exitstatus buffer)
  (cond ((eq exitstatus 1)
         'error)
        ((with-current-buffer buffer
           (goto-char 1)
           (search-forward ":Warning:" nil t))
         'warning)
        (t
         'normal)))

(defun aabc/emacs-command ()
  (car command-line-args))

(defun aabc/byte-compile-start-process-args (file)
  (append (list
           (aabc/emacs-command)    ;emacs
           "-Q" "-batch" "--eval"
           (format "(setq load-path (cons \".\" '%S))" load-path))
          (if (file-exists-p auto-async-byte-compile-init-file)
              (list "-l" auto-async-byte-compile-init-file))
          (list "-f" "batch-byte-compile" file)))

;;;; Bug report
(defvar aabc/maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar aabc/bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of auto-async-byte-compile.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"auto-async-byte-compile.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun aabc/-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   aabc/maintainer-mail-address
   "auto-async-byte-compile.el"
   (apropos-internal "^\\(aabc\\|auto-async-byte-compile\\)" 'boundp)
   nil nil
   aabc/bug-report-salutation))

(provide 'auto-async-byte-compile)

;; How to save (DO NOT REMOVE!!)
;; (progn (git-log-upload) (emacswiki-post "auto-async-byte-compile.el"))
;;; auto-async-byte-compile.el ends here
