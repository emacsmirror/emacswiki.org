;;;; sudo-ext.el --- sudo support
;; Time-stamp: <2011-01-17 15:52:34 rubikitch>

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: unix
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/sudo-ext.el

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
;; `sudo' support in Emacs.
;; Currently it has two features.
;;
;; * `sudoedit' command opens files as root using sudoedit program.
;;   This command needs emacsserver or gnuserv.
;;   Try M-x server-start or M-x gnuserv-start first.
;;   Be sure to you can run `sudoedit FILE' in shell.
;;   
;; * `sudo' support in shell execution in Emacs.
;;   In executing sudo shell command, password prompt is appeared if needed.
;;   * M-x compile
;;   * M-x grep
;;   * M-!
;;   * M-|
;;   * M-&
;;   * M-x executable-interpret

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `sudo-K'
;;    Run `sudo -K'.
;;  `sudoedit'
;;    Run `sudoedit FILE' to edit FILE as root.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; Put sudo-ext.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (server-start)
;; (require 'sudo-ext)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET sudo-ext RET
;;


;;; History:

;; See http://www.rubyist.net/~rubikitch/gitlog/sudo-ext.txt

;;; Code:

(defvar sudo-ext-version "0.1")
(eval-when-compile (require 'cl))
(defgroup sudo-ext nil
  "sudo-ext"
  :group 'emacs)

(defun sudo-internal (continuation)
  (with-current-buffer (get-buffer-create " *sudo-process*")
    (erase-buffer)
    
    (let ((proc (start-process "sudo" (current-buffer) "sudo" "-v")))
      (lexical-let ((continuation continuation)
                    (return-value 'sudo--undefined))

        (set-process-filter proc 'sudo-v-process-filter)
        (set-process-sentinel
         proc
         (lambda (&rest args) (setq return-value (funcall continuation))))
        (while (eq return-value 'sudo--undefined)
          (sit-for 0.01))
        return-value))))
(defun sudo-v ()
  "Run `sudo -v'. Maybe requires password."
  (sudo-internal 'ignore))

(defun sudo-v-process-filter (proc string)
  (when (string-match "password" string)
    (process-send-string proc (concat (read-passwd "Sudo Password: ") "\n"))))

(defmacro sudo-wrapper (args &rest body)
  "Run `sudo -v' then execute BODY. ARGS are variables to pass to body.
Because BODY is executed as asynchronous function, ARGS should be lexically bound."
  `(lexical-let ,(mapcar (lambda (arg) (list arg arg)) args)
     (sudo-internal
      (lambda () ,@body))))
(put 'sudo-wrapper 'lisp-indent-function 1)

(defun sudo-K ()
  "Run `sudo -K'."
  (interactive)
  (call-process "sudo" nil nil nil "-K"))

(defun sudoedit-editor ()
  (cond ((and (fboundp 'server-running-p)
              (server-running-p))
         "emacsclient")
        ((and (fboundp 'gnuserv-running-p)
              (gnuserv-running-p))
         "gnuclient")
        (t
         (error (substitute-command-keys "Not running server. Start server by \\[server-start] or \\[gnuserv-start]")))))

(defun sudoedit (file)
  "Run `sudoedit FILE' to edit FILE as root.
Be sure to you can run `sudoedit FILE' in shell."
  (interactive "FSudoedit: ")
  (sudo-wrapper (file)
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "EDITOR" (sudoedit-editor))
      (setenv "VISUAL" (sudoedit-editor))
      (start-process "sudoedit" (get-buffer-create " *sudoedit*")
                    "sudoedit" file))))
;; (sudoedit "/etc/fstab")
;; (sudo-K)

(defmacro sudo-advice (func argpos)
  "Activate advice to make FUNC sudo-awared. ARGPOS is command position."
  `(defadvice ,func (before sudo-advice activate)
     (when (string-match "\\bsudo\\b" (ad-get-arg ,argpos))
       (sudo-v))))
;; (sudo-K)
;; (shell-command "sudo sh -c 'echo $USER'")
;; (async-shell-command "sudo sh -c 'echo $USER'")
(sudo-advice shell-command 0)
(sudo-advice shell-command-on-region 2)
(sudo-advice compilation-start 0)
;;; Disable it because `shell-command-to-string' is too low-level function.
;;; If internally used shell command contains a string `sudo',
;;; password prompt may be appeared. It disturbs commands like `anything'.
;; (sudo-advice shell-command-to-string 0)

(provide 'sudo-ext)

;; How to save (DO NOT REMOVE!!)
;; (progn (git-log-upload) (emacswiki-post "sudo-ext.el"))
;;; sudo-ext.el ends here
