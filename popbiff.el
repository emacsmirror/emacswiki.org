;;; popbiff.el --- Checks for mail using POP3
;;; $Id: popbiff.el,v 1.9 1996/10/22 11:08:52 ddw Exp $
;; Copyright (C) 1996 by Free Software Foundation, Inc.
 
;; Author: Dominique de Waleffe <ddw@acm.org>
;; Keywords: mail, tools
 
;; This file is part of GNU Emacs.
 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;;; Commentary:
 
;; This file checks for mail using the POP3 protocol
;;
;; Usage:
;;  In your .emacs put
;;  (require 'popbiff)
;;  (popbiff 5) ;; check every 5 minutes
;;
;; Note: may need to change \r\n in some strings to run on Unix...
;;
 
;;; configuration variables
;;;
(defvar popbiff-user (user-real-login-name) "POP user name")
(defvar popbiff-pwd 'ask
  "Either the POP password or the symbol 'ask. In this case, Emacs
will query the user for the password")
 
(defvar popbiff-host "localhost" "Your POP server")
 
(defvar popbiff-port 110 "TCP port on which POP server listens"  )
 
 
;;; Code:
(require 'cl)
(require 'ange-ftp)
 
(defvar popbiff-mode-line-info "")
 
(eval-when (load)
  (ange-ftp-set-passwd popbiff-host popbiff-user nil)
  (if (not (memq 'popbiff-mode-line-info global-mode-string))
      (setq global-mode-string (cons "" (cons 'popbiff-mode-line-info
     global-mode-string)))))
(defvar popbiff-timer nil)
 
(defun popbiff (minutes)
  "Sets up a connection every MINUTES from now that reports the
  number of mails waiting on the POP3 account"
  (interactive "N")
  (if popbiff-timer (cancel-timer popbiff-timer))
  (if (not (or (null minutes)(zerop minutes)))
      (progn (setq popbiff-timer
     (run-at-time nil (* 60 minutes) 'popbiff-docheck
    popbiff-user
    popbiff-pwd
    popbiff-host popbiff-port))
      t)))
 
(defun popbiff-filter (proc string)
  (save-match-data
    (cond ((string-match "+OK 0 .*" string);; no mail
    (setq popbiff-mode-line-info ""))
   ((string-match "^+OK \\([0-9]+\\) [0-9]+" string)
    ;;    (message "%s Mail waiting" (substring string
    ;;  (match-beginning 1)
    ;; (match-end 1)))
    (setq popbiff-mode-line-info
   (concat "[Mail:" (substring string
          (match-beginning 1)
          (match-end 1))
    "] ")))
    
   ((or
     (string-match
      "^-ERR No password specified"
      string)
     ;; Zmail pop server message
     (string-match
      "^-ERR Password supplied for \"\\(.*\\)\" is incorrect."
      string)
     ;; Post Office 1.93
     (string-match
      "^-ERR Password \\(failed\\|required\\) for \\([^\r\n]*\\)"
      string))
    (ange-ftp-set-passwd popbiff-host popbiff-user nil)
    (message "Wrong POP3 password for %s, reset " popbiff-user) )
   ((string-match "^-ERR.*" string)
    (message "POP3 error: %s" string))
   (t nil))))
     
(defvar in-popbiff-docheck nil)
(defun popbiff-docheck(user pwd host port)
  (if (not in-popbiff-docheck)
      (unwind-protect
   (progn
     (setq in-popbiff-docheck t)
     (with-timeout (20 (setq popbiff-mode-line-info "[Mail:??]"))
       (let ((pwd (popbiff-get-pwd host user pwd))
      (process (open-network-stream "POPCHECK" "*POP3*" host
        port)))
  (if  (not process)
      (error "Could not connect to POP3 server on %s" host)
    ;;      (erase-buffer "*POP3*")
    (set-process-filter process 'popbiff-filter)
    (process-send-string process (format "user %s" user))
    (process-send-string process "\r\n")
    (process-send-string process (format "pass %s" pwd))
    (process-send-string process "\r\n")
    (process-send-string process "stat")
    (process-send-string process "\r\n")
    (process-send-string process "quit")
    (process-send-string process "\r\n") ))))
 (setq in-popbiff-docheck nil))))
 
(defun popbiff-get-pwd(host user pwd)
  (cond ((eq pwd 'ask) (ange-ftp-get-passwd host user))
 ((stringp pwd) pwd)
 (t "")))
 
(provide 'popbiff)
 
;;; popbiff.el ends here
