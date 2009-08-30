;;; bitlbee.el --- Help get Bitlbee (http://www.bitlbee.org) up and running
;;
;; Copyright (C) 2008 pmade inc. (Peter Jones pjones@pmade.com)
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;; Commentary:
;;
;; Start and stop bitlbee from within emacs.
;;
;; Assumes you have a ~/.bitlbee directory where the bitlbee.conf file
;; lives, along with the account information XML files.  The directory
;; can be set using the `bitlbee-user-directory' variable, and is
;; created automatically if necessary.
;;
;; You might also need to set the `bitlbee-executable' variable.
;;
;; Usage:
;;
;; (require 'bitlbee)
;; M-x bitlbee-start
;;
;; Latest version:
;;
;; git clone git://pmade.com/elisp

(defvar bitlbee-user-directory "~/.bitlbee"
  "The directory where user configuration goes")

(defvar bitlbee-options "-n -D -v "
  "The options passed to Bitlbee on the command line.")

(defvar bitlbee-executable "bitlbee"
  "The full path to the Bitlbee executable")

(defvar bitlbee-buffer-name "*bitlbee*"
  "The name of the bitlbee process buffer")

(defun bitlbee-running-p ()
  "Returns non-nil if bitlbee is running"
  (if (get-buffer-process bitlbee-buffer-name) t nil))

(defun bitlbee-start ()
  "Start the bitlbee server"
  (interactive)
  (if (bitlbee-running-p) (message "bitlbee is already running")
    (make-directory (expand-file-name bitlbee-user-directory) t)
    (let ((proc (start-process-shell-command "bitlbee" bitlbee-buffer-name bitlbee-executable (bitlbee-command-line))))
      (set-process-sentinel proc 'bitlbee-sentinel-proc))
      (message "started bitlbee")))

(defun bitlbee-stop ()
  "Stop the bitlbee server"
  (interactive)
  (let ((proc (get-buffer-process bitlbee-buffer-name)))
    (when proc (kill-process proc t))))

(defun bitlbee-sentinel-proc (proc msg)
  (when (memq (process-status proc) '(exit signal))
    (setq msg (replace-regexp-in-string "\n" "" (format "stopped bitlbee (%s)" msg)))
  (message msg)))

(defun bitlbee-command-line ()
  "Create the full command line necessary to run bitlbee"
  (concat bitlbee-options " -d " bitlbee-user-directory " -c " bitlbee-user-directory "/bitlbee.conf"))

(provide 'bitlbee)

