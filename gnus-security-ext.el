;;; gnus-security-ext.el --- Extra gnus functions for virus scanning, etc.

;; Filename: gnus-security-ext.el
;; Description: Extra gnus functions for virus scanning, etc.
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2016, Joe Bloggs, all rites reversed.
;; Created: 2016-05-24 14:19:05
;; Version: 0.1
;; Last-Updated: Thu May 26 03:07:54 2016
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/gnus-security-ext
;; Keywords: comm
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires:  
;;
;; Features that might be required by this library:
;;
;; gnus-art gnus-sum gnus-summary-ext mm-util mml-sec
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;;
;; Extra gnus functions for virus scanning, etc.
;;
;; NOTE: this has only been tested on gnu/linux systems. I have a feeling it won't work on windows,
;;       but you can try.
;; To use the virus checker, first make sure you have clamscan installed: http://www.clamav.net/
;; Then customize `gnus-mime-action-alist' and add `gnus-mime-clamscan' to the list.
;; To improve handling of signed messages add `message-set-signer' to `message-send-hook':
;;             (add-hook 'message-send-hook 'message-set-signer)
;; 
;; 
;;;;;;;;

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `gnus-mime-clamscan'
;;    Scan a mime attachment for viruses with clamscan.
;;    Keybinding: M-x gnus-mime-clamscan
;;  `gnus-clamscan-marked'
;;    Scan all marked articles for viruses.
;;    Keybinding: M-x gnus-clamscan-marked
;;  `gnus-report-419-spam'
;;    Automatically report 419 spam email to fraudaid.com or 419scam.org.
;;    Keybinding: M-x gnus-report-419-spam
;;  `gnus-article-whois-sender'
;;    Perform whois lookup on sender of email if possible.
;;    Keybinding: M-x gnus-article-whois-sender
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `gnus-clamscan-flags'
;;    Extra flags for clamscan/clamdscan.
;;    default = nil
;;  `gnus-419-report-settings'
;;    Email address and settings for sending 419 scam reports.
;;    default = (quote ("419@419scam.org" "[419]" "" t nil))

;;
;; All of the above can be customized by:
;;      M-x customize-group RET gnus-security-ext RET
;;

;;; Installation:
;;
;; Put gnus-security-ext.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'gnus-security-ext)

;;; History:

;;; Require
(require 'gnus-art)
(require 'gnus-sum)
(require 'gnus-summary-ext)
(require 'mm-util)
(require 'mml-sec)

;;; Code:

(defcustom gnus-clamscan-flags nil
  "Extra flags for clamscan/clamdscan.
These flags will be used by `gnus-mime-clamscan' and `gnus-clamscan-marked'
when clamscan is called."
  :group 'gnus-security-ext
  :type 'string)

(defcustom gnus-419-report-settings '("419@419scam.org" "[419]" "" t nil)
  "Email address and settings for sending 419 scam reports.
First element should be an email address.
Second element is a subject line for the email, or nil to keep existing subject.
Third element is a \"From\" address, or nil to use address the email was sent to.
Fourth element says whether to forward as an attachment or not.
Fifth element says whether attachments should be forwarded as MML instead of
MIME (if unsure leave as nil)."
  :group 'gnus-security-ext
  :type '(list (string :tag "Email address")
	       (string :tag "Subject header")
	       (string :tag "From")
	       (boolean :tag "Forward as attachment")
	       (boolean :tag "Send attachments as MML")))

;;;###autoload
(defun system-process-running-p (name)
  "Return non-nil if system process NAME is running."
  (let* ((pids (list-system-processes))
	 (len (length pids))
	 (i 0)
	 pid found)
    (cl-loop while (and (< i len) (not found))
	     do (progn
		  (setq pid (nth i pids))
		  (if (equal name
			     (cdr (assoc
				   'comm
				   (process-attributes (nth i pids)))))
		      (setq found t))
		  (setq i (1+ i))))
    found))

;;;###autoload
(defun gnus-mime-clamscan nil
  "Scan a mime attachment for viruses with clamscan.
This function should be added to `gnus-mime-action-alist'."
  (interactive)
  (gnus-article-check-buffer)
  (unless (executable-find "clamscan")
    (error "Can't find clamscan"))
  (let* ((data (get-text-property (point) 'gnus-data))
	 (cmd (concat (if (system-process-running-p "clamd")
			  "clamdscan"
			"clamscan")
		      " " gnus-clamscan-flags " -"))
	 (buf (get-buffer-create "*clamscan*")))
    (when data
      (with-current-buffer buf
	(goto-char (point-max))
	(let ((rslts (mm-with-unibyte-buffer
		       (mm-insert-part data)
		       (mm-add-meta-html-tag data)
		       (let ((coding-system-for-write 'binary))
			 (shell-command-on-region (point-min) (point-max) cmd nil t))
		       (buffer-substring-no-properties (point-min) (point-max)))))
	  (insert "\nScanning " (mm-handle-filename data) ":\n\n" rslts)
	  (message rslts))))))

;;;###autoload
(defun gnus-clamscan-marked (arg)
  "Scan all marked articles for viruses.
If ARG is non-nil or a prefix arg is supplied it indicates how many articles forward (if positive) or 
backward (if negative) from the current article to include. Otherwise if region is active, process
the articles within the region, otherwise process the process marked articles."
  (interactive "P")
  (gnus-summary-ext-act-on-parts-in-marked
   arg 'gnus-mime-clamscan nil t t t))

;;;###autoload
(defun gnus-report-419-spam nil
  "Automatically report 419 spam email to fraudaid.com or 419scam.org.
This should be called in message buffer of 419 spam email."
  (interactive)
  (let ((to (first gnus-419-report-settings))
	(subj (second gnus-419-report-settings))
	(from (third gnus-419-report-settings))
	(message-forward-as-mime
	 (fourth gnus-419-report-settings))
	(message-forward-show-mml
	 (fifth gnus-419-report-settings)))
    (gnus-summary-toggle-header 1)
    (message-forward)
    (message-replace-header "To" to)
    (if (and from (not (equal from "")))
	(message-replace-header "From" from))
    (if (and subj (not (equal subj "")))
	(message-change-subject subj))
    (message-send)))

;;;###autoload
(defun grab-ip-at-point nil
  "Return as string ip address at point."
  (let (from to str)
    (save-excursion (re-search-backward "[^.0-9]")
		    (setq from (1+ (point))))
    (save-excursion (re-search-forward "[^.0-9]")
		    (setq to (1- (point))))
    (setq str (buffer-substring-no-properties from to))
    (if (string-match "\\(25[0-5]\\|2[0-4][0-9]\\|1?[0-9]\\{1,2\\}\\)\\(\\.\\(25[0-5]\\|2[0-4][0-9]\\|1?[0-9]\\{1,2\\}\\)\\)\\{3\\}"
		      str)
	str)))

;; FIXME!
;;;###autoload
(defun gnus-article-get-sender-ip nil
  "Return IP address of sender of current article, or nil if none found."
  (save-excursion
    (gnus-summary-toggle-header 1)
    (end-of-buffer)
    (prog1
	(if (and (re-search-backward "ClientAddr\\|Originating-IP" nil t)
		 (re-search-forward "[0-9]+\\." nil t))
	    (grab-ip-at-point)
	  (end-of-buffer)
	  (if (and (re-search-backward "client-ip" nil t)
		   (re-search-forward "[0-9]+\\." nil t))
	      (grab-ip-at-point)))
      (gnus-summary-toggle-header -1))))

;;;###autoload
(defun gnus-article-whois-sender nil
  "Perform whois lookup on sender of email if possible.
Works by checking the headers for X-ClientAddr or X-Originating-IP."
  (interactive)
  (let ((ip (gnus-article-get-sender-ip)))
    (if ip
	(let ((whois-server-name whois-reverse-lookup-server))
	  (whois nil ip))
      (message "Can't find IP address!"))))

;;;###autoload
(defun message-set-signer nil
  "If current message is signed, then use private key corresponding to the sender.
If that can't be found, prompt the user for a key.
Add this to `message-send-hook'."
  (let ((from (message-fetch-field "From"))
	email key)
    (string-match "[^<>
        ]+@[^<>
        ]+" from)
    (setq email (match-string 0 from)
	  mml1991-verbose mml-secure-verbose
	  mml1991-signers nil)
    (if email
	(setq key (car (epg-list-keys (epg-make-context epa-protocol) email t))))
    (if key
	(setq mml1991-signers (list (epg-sub-key-id (car (epg-key-sub-key-list key)))))
      (setq mml1991-verbose t))))

(provide 'gnus-security-ext)

;;; gnus-security-ext.el ends here
