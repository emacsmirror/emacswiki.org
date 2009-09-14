;;; bbdb-mew.el --- BBDB interface to Mew

;; Copyright (C) 1991, 1992 Jamie Zawinski
;; Copyright (C) 1996 Shuhei KOBAYASHI
;; Copyright (C) 1996 Daisuke Kanda
;; Copyright (C) 1999 Mitsuo Nishizawa

;; Author: Jamie Zawinski <jwz@netscape.com>
;;         Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;;         Daisuke Kanda <small@first.tsukuba.ac.jp>
;;         Mitsuo Nishizawa <mitsuo@phys2.med.osaka-u.ac.jp>
;; Maintenance: Chris Beggy
;; Created: 1996/11/04
;; Version: $Id: bbdb-mew.el,v 1.5 2001/12/29 16:12:20 chrisb Exp $

;; Keywords: mail, BBDB

;; This file is not part of BBDB (Insidious Big Brother Database).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  2002-12-28 checked and edited for bbdb CVS version + mew 3.0.51 + emacs 21.1

;; Installation:
;;
;; Put bbdb-mew.el in your load path, so that emacs can find it.
;;
;; Run a patched bbdb-com.el to allow mew to be the mailer bbdb 
;; uses if bbdb-send-mail-style is set to 'mew
;;
;; Insert the following lines in your ~/.emacs:
;;      (other BBDB stuff comes here)
;;              :
;; (autoload 'bbdb-insinuate-mew      "bbdb-mew"   "Hook BBDB into Mew")
;; (add-hook 'mew-init-hook 'bbdb-insinuate-mew)
;; (setq bbdb-send-mail-style 'mew)
;;
;; To use BBDB name at From: field of header in citation, please set
;; (setq mew-cite-bbdb-header t)
;;
;; Chris Beggy started doing some maintenance.

;;; Codes:

(require 'bbdb)
(require 'mew)

(defvar mew-cite-bbdb-header nil)
(defvar mew-cite-bbdb-enable nil)

(or (fboundp 'mew-header-get-value)
    (fset 'mew-header-get-value (symbol-function 'mew-field-get-value))
    )

(defun bbdb/mew-update-record (&optional offer-to-create)
  "Returns the record corresponding to the current mew message,
creating or modifying it as necessary.  A record will be created if 
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation."
  (save-excursion
    (set-buffer (mew-buffer-message))
    (if bbdb-use-pop-up
        (bbdb/mew-pop-up-bbdb-buffer offer-to-create)
      (let* ((from (mew-header-get-value mew-from:))
             (addr (and from
                        (car (cdr (mail-extract-address-components from))))))
        (if (or (null from)
                (null addr)
                (string-match (bbdb-user-mail-names) addr))
            (setq from (or (mew-header-get-value mew-to:) from)))
        (if from
            (bbdb-annotate-message-sender
             from t
             (or (bbdb-invoke-hook-for-value
                  bbdb/mail-auto-create-p)
                 offer-to-create)
             offer-to-create))))))

(defun bbdb/mew-annotate-sender (string)
  "Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message."
  (interactive
   (list (if bbdb-readonly-p
             (error "The Insidious Big Brother Database is read-only.")
           (read-string "Comments: "))))
  (bbdb-annotate-notes (bbdb/mew-update-record t) string))

(defun bbdb/mew-edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (let ((record (or (bbdb/mew-update-record t) (error ""))))
    (bbdb-display-records (list record))
    (if arg
	(bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))

(defun bbdb/mew-show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings."
  (interactive)
  (let ((record (bbdb/mew-update-record t)))
    (if record
	(bbdb-display-records (list record))
      (error "unperson"))))

(defun bbdb/mew-pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the mew windows,
displaying the record corresponding to the sender of the current message."
  (let ((framepop (eq temp-buffer-show-function 'framepop-display-buffer)))
    (or framepop
        (bbdb-pop-up-bbdb-buffer
         (function
          (lambda (w)
            (let ((b (current-buffer)))
              (set-buffer (window-buffer w))
              (prog1 (eq major-mode 'mew-message-mode)
                (set-buffer b)))))))
    (let ((bbdb-gag-messages t)
          (bbdb-use-pop-up nil)
          (bbdb-electric-p nil))
      (let ((record (bbdb/mew-update-record offer-to-create))
            (bbdb-display-layout bbdb-pop-up-display-layout)
            (b (current-buffer)))
        (if framepop
            (if record
                (bbdb-display-records (list record))
              (framepop-banish))
          (bbdb-display-records (if record (list record) nil)))
        (set-buffer b)
        record))))

;;; Utilities
;;;

(defun bbdb-record-field (record field)
  (cond
   ((eq field 'firstname) (bbdb-record-firstname record))
   ((eq field 'lastname)  (bbdb-record-lastname record))
   ((eq field 'aka)       (bbdb-record-aka record))
   ((eq field 'company)   (bbdb-record-company record))
   ((eq field 'phones)    (bbdb-record-phones record))
   ((eq field 'addresses) (bbdb-record-addresses record))
   ((eq field 'net)       (bbdb-record-net record))
   ((eq field 'raw-notes) (bbdb-record-raw-notes record))
   ((eq field 'cache)     (bbdb-record-cache record))
   (t
    (and (consp (bbdb-record-raw-notes record))
         (cdr (assq field (bbdb-record-raw-notes record)))
         ))))

(defun bbdb-record-fields (record fields)
  (let (value)
    (while (and fields (null value))
      (setq value (bbdb-record-field record (car fields)))
      (setq fields (cdr fields)))
    value))

;;; Register citation attribution in BBDB
;;;

(defvar mew-cite-bbdb-fields '(attribution lastname firstname))

(defun mew-cite-prefix-bbdb ()
 (if mew-cite-bbdb-enable
  (let (from petname prefix)
    (setq from (mew-header-get-bbdb-name))
    (if (and mew-use-petname mew-petname-alist
	     (setq petname 
		   (cdr (mew-assoc-case-equal from mew-petname-alist 0))))
	(setq prefix petname)
      (setq prefix (mew-addrstr-extract-user from)))
    (if mew-ask-cite-prefix
	(setq prefix (read-string "Citation prefix: " prefix)))
    (format "%s> " prefix)
    )))

(defun mew-header-get-bbdb-name ()
 (if mew-cite-bbdb-enable
  (let* ((from (mew-header-parse-address mew-from:))
         (addr from)
	 (name nil)
	 (net addr)
	 (record (and addr 
		      (bbdb-search-simple name 
		       (if (and net bbdb-canonicalize-net-hook)
			   (bbdb-canonicalize-address net)
			 net)))))
    (or (and record
             (bbdb-record-fields record mew-cite-bbdb-fields))
        net))))

(or (fboundp 'bbdb:mew-cite-strings)
    (fset 'bbdb:mew-cite-strings (symbol-function 'mew-cite-strings)))

(defun mew-cite-strings-bbdb ()
 (if mew-cite-bbdb-enable
  (let (fields)
    (if mew-cite-bbdb-header
        (setq fields
               (mapcar
                (function
                 (lambda (x)
                   (or (if (string= x mew-from:)
                           (mew-header-get-bbdb-name)
                            (mew-header-get-value x))
                       "")))
                mew-cite-fields))
        (setq fields 
               (mapcar (function mew-header-get-value) mew-cite-fields)))
    (setq fields (mapcar (lambda (x) (or x "")) fields))
    (if mew-use-petname
	(setq fields (mew-cite-strings-with-petname fields mew-cite-fields))
      )
    (if mew-use-bbdb
        (apply (function format) mew-cite-format fields)
        (bbdb:mew-cite-strings)))))

;;; Installation
;;;

(defun bbdb-insinuate-mew ()
  "Call this function to hook BBDB into Mew."
  (if (string-match "2.3" (bbdb-version))
    (add-hook 'mew-message-hook 'bbdb/mew-update-record)
    (bbdb-add-hook 'mew-message-hook 'bbdb/mew-update-record))
  (define-key mew-summary-mode-map ":" 'bbdb/mew-show-sender)
  (define-key mew-summary-mode-map ";" 'bbdb/mew-edit-notes)
  )

(provide 'bbdb-mew)

;;; bbdb-mew.el ends here.
