;;; gnuslog.el --- Gnus Splitting Log Facility

;; Copyright (C) 2003  

;; Author: Johan Bockgård
;;  <http://www.dd.chalmers.se/~bojohan/emacs/lisp/>
;; Maintainer: Adrian Aichner <adrian@xemacs.org>

;; Version: 1.5

;; Keywords: gnus log splitting

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; Hooks into gnus splitting functions and writes a log to file. The
;; log file can be viewed in Gnus as a Document Group.

;;; Disclaimer: I only use the nnfolder back end.

;;; Usage:
;; Load the code by putting (require 'gnuslog) in your ~/.gnus file.
;; Create a document group for the log file using G f ~/.gnuslog RET
;; in the summary buffer.

;;; Code:

;;; Nndoc Type Definition

;; Avoid miscompiling macro `gnus-gethash' and other macros in absence
;; of loaded definition from 'gnus.
;; See 2004-03-15_23-01_macro_err_1.txt in
;; http://labb.contactor.se/~matsl/smoketest/logs/
;; or newer results for miscompiled macros.
(eval-when-compile (require 'gnus))

(defun nndoc-gnuslog-type-p ()
  ;; This is slightly ugly
  (equal (expand-file-name gnuslog-file) nndoc-address))

(nndoc-add-type '(gnuslog (article-begin . "^From")) 'last)

;;; Logging

(defvar gnuslog-file "~/.gnuslog")

(defvar gnuslog-file-coding-system
  (if (featurep 'xemacs)
      'binary
    'emacs-mule))

(defun gnuslog-setup-hook ()
  (unless
      (gnus-gethash
       (format "nndoc+%s:%s"
	       (expand-file-name gnuslog-file)
	       (file-name-nondirectory gnuslog-file))
       gnus-newsrc-hashtb)
    (cond
     ((gnus-group-make-doc-group gnuslog-file 'gnuslog)
      (message "gnuslog-file %S has now been subscribed to."
	       gnuslog-file))
     (t
      (error "gnuslog-file %S has not been subscribed to."))))
  (defadvice nnmail-article-group (after nnmail-log activate)
    ;; Catch the return value (group-art-list)
    (gnuslog-log ad-return-value)))

; In the current implementation it's critical to use the appropriate
; hook:
; gnus-newsrc-hashtb needs to be set up already and no mail must have
; been processed yet!
(add-hook 'gnus-read-newsrc-el-hook
	  'gnuslog-setup-hook
	  'append)

;; If we don't want to catch duplicates we could just use
;; nnmail-spool-hook, called as
;; (run-hook-with-args 'nnmail-spool-hook id grp subject)
(defun gnuslog-log (group-art-list)
  ;; Is this call inside `nnmail-check-duplication'? that means(?)
  ;; this is incoming mail.
  (when (boundp 'duplication)
    ;; Use (boundp 'gnus-command-method),
    ;; bound during `B r', not by `B q' or `B t'?.
    (let ((msg (save-excursion
		 (save-restriction
		   (article-narrow-to-head)
		   (concat
		    (mapconcat (lambda (field)
				 (concat field ": " (message-fetch-field field)))
			       '("From" "To" "Subject" "Date")
			       "\n")
		    (format "\nReferences: %s"
			    (message-fetch-field  "Message-ID"))
		    "\nX-Gnus-Groups: "
		    (when (and
			   (boundp 'gnus-command-method)
			   gnus-command-method)
		      (mapconcat
		       (lambda (group-art)
			 (let ((server (second gnus-command-method))
			       (backend
				(symbol-name (first gnus-command-method)))
			       ;; (article-number (cdr group-art))
			       (group-name (car group-art)))
			   (format "%s%s:%s"
				   backend
				   (if (string-equal server "")
				       server
				     (format "+%s" server))
				   group-name)))
		       group-art-list ", ")))))))
      (with-temp-buffer
	;; Or coding-system-for-write
	(let ((buffer-file-coding-system
	       gnuslog-file-coding-system))
	  (insert msg "\n\n")
	  (append-to-file (point-min) (point-max) gnuslog-file))))))

;;; Summary Formatting

;; This is what *I* use. You may want to adapt it.
(defvar gnuslog-summary-line-format
  "%U%R%z%I%(%[%4L: %-23,23f%]%) %-12u&gnus-groups; %s\n"
  ;; "%ua%U%R%4N %6&user-date; %1{%[%} %(%-20,20f%)%* %1{%]%} %0{%S%} => %u&gnus-groups;\n"
  )

(defun gnus-user-format-function-gnus-groups (header)
  ;; Fetch the X-Gnus-Groups header
  (let ((folder (cdr (assq 'X-Gnus-Groups
			   (mail-header-extra header)))))
    (if (or (not folder)
	    (string= "" folder))
	(propertize "(none)" 'face gnus-face-1)
      folder)))

(add-hook 'gnus-started-hook
          (lambda ()
            (add-to-list 'gnus-extra-headers 'X-Gnus-Groups)))

(add-to-list 'gnus-parameters
	     '("^nndoc.*\\.gnuslog$"
	       (gnus-summary-line-format gnuslog-summary-line-format)
	       (gnus-summary-highlight nil)))

(defun gnuslog-goto-destination-article ()
  "Fetch destination article from gnuslog article and display it.

Headers X-Gnus-Groups and References are examined to find destination
article.
"
  (interactive)
  (let (x-gnus-groups groups message-id)
    (save-excursion
      (gnus-summary-select-article t 'force)
      (set-buffer gnus-original-article-buffer)
      (save-restriction
	(message-narrow-to-head)
	(setq x-gnus-groups (message-fetch-field "X-Gnus-Groups"))
	(setq message-id  (message-fetch-field "References"))))
    (if (null x-gnus-groups)
	(message
	 "gnuslog-goto-destination-article cannot find X-Gnus-Groups header")
      (setq groups (split-string x-gnus-groups "\\s-*,\\s-*" t))
      (gnus-group-read-group 50 t (first groups))
      (if (null message-id)
	  (message
	   "gnuslog-goto-destination-article cannot find References header")
	(flet
	    (
	     ;; Disable searching outside current group.
	     ;; Variable gnus-refer-article-method gets ignored, when
	     ;; set to nil!
	     (gnus-refer-article-methods ()))
	  (unless
	      (gnus-summary-goto-article message-id nil t)
	    (gnus-summary-rescan-group)
	    (gnus-summary-goto-article message-id nil t)))))))

(provide 'gnuslog)

;;; gnuslog.el ends here
