;;; bbdb-vcard-export.el -- export BBDB as vCard files
;; 
;; Copyright (c) 2002 Jim Hourihan
;; Copyright (c) 2005 Alex Schroeder
;;
;; bbdb-vcard-export.el is free software you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Author: Jim Hourihan <jimh@panix.com>
;; Created: 2002-08-08
;; Version: $Id: bbdb-vcard-export.el,v 1.3 2006/03/14 00:00:00 malcolmp Exp $
;; Keywords: vcard ipod

;;; Commentary

;; I use this code to sync my ipod with bbdb under OS X. To do so:
;;
;;	M-x bbdb-vcard-export-update-all
;;
;; and enter `/Volumes/IPOD_NAME/Contacts/' at the prompt
;;
;; vCard documentated in RFC 2426 <http://www.faqs.org/rfcs/rfc2426.html>
;; Value types documented in RFC 2425 <http://www.faqs.org/rfcs/rfc2425.html>

;; The coding system used for writing the files is UTF-16 by default.
;; To use anything else, use a prefix argument: C-u M-x
;; bbdb-vcard-export-update-all.  You will be prompted for another
;; coding system to use.  Latin-1 is probably a good choice.
;; bbdb-file-coding-system's default value is iso-2022-7bit, which is
;; probably useless for vCard exports.

;;; Code:

(require 'bbdb)

; XEmacs prior to 21.5 is not dumped with replace-regexp-in-string.  In those
; cases it can be found in the xemacs-base package.
(eval-and-compile
  (if (and (not (fboundp 'replace-regexp-in-string)) (featurep 'xemacs))
      (require 'easy-mmode)))

(defvar bbdb-translation-table
  '(("Mobile" . "Cell"))
  "Translations of text items, typically for labels.")

(defun bbdb-translate (str)
  "Translate STR into some other string based on `bbdb-translation-table'."
  (let ((translation (assoc str bbdb-translation-table)))
    (if translation
	(cdr translation)
      str)))

;; 2.3 Predefined VALUE Type Usage

;;    The predefined data type values specified in [MIME-DIR] MUST NOT be
;;    repeated in COMMA separated value lists except within the N,
;;    NICKNAME, ADR and CATEGORIES value types.

;;    The text value type defined in [MIME-DIR] is further restricted such
;;    that any SEMI-COLON character (ASCII decimal 59) in the value MUST be
;;    escaped with the BACKSLASH character (ASCII decimal 92).

(defun bbdb-vcard-export-escape (str)
  "Return a copy of STR with ; , and newlines escaped."
  (setq str (bbdb-translate str)
	str (or str ""); get rid of nil values
	str (replace-regexp-in-string "\\(;\\|,\\|\\\\\\)" "\\\\\\1" str)
	str (replace-regexp-in-string "\n" "\\\\n" str)))

;; (insert (bbdb-vcard-export-escape "this is, not \\ or \n true"))

(defun bbdb-vcard-export-several (list)
  "Return a comma-separated list of escaped unique elements in LIST."
  (let ((hash (make-hash-table :test 'equal))
	result)
    (dolist (item list)
      (puthash (bbdb-vcard-export-escape item) t hash))
    (maphash (lambda (key val)
	       (setq result (cons key result)))
	     hash)
    (bbdb-join result ",")))

;; The component values MUST be specified in
;; their corresponding position. The structured type value corresponds,
;; in sequence, to the post office box; the extended address; the street
;; address; the locality (e.g., city); the region (e.g., state or
;; province); the postal code; the country name. When a component value
;; is missing, the associated component separator MUST still be
;; specified.

;; The text components are separated by the SEMI-COLON character (ASCII
;; decimal 59). Where it makes semantic sense, individual text
;; components can include multiple text values (e.g., a "street"
;; component with multiple lines) separated by the COMMA character
;; (ASCII decimal 44).
(defun bbdb-vcard-export-address-string (address)
  "Return the address string"
  (let ((streets (bbdb-address-streets address))
	(city (bbdb-address-city address))
	(state (bbdb-address-state address))
	(country (bbdb-address-country address))
	(zip (bbdb-address-zip address)))
    (concat 
     "adr;type=" (bbdb-vcard-export-escape (bbdb-address-location address)) ":"
     ";;" ;; no post office box, no extended address
     (bbdb-vcard-export-several streets) ";"
     (bbdb-vcard-export-escape city) ";"
     (bbdb-vcard-export-escape state) ";"
     (bbdb-vcard-export-escape zip) ";"
     (bbdb-vcard-export-escape country))))

(defun bbdb-vcard-export-record-insert-vcard (record)
  "Insert a vcard formatted version of RECORD into the current buffer"
  (let ((name (bbdb-record-name record))
	(first-name (bbdb-record-firstname record))
	(last-name (bbdb-record-lastname record))
	(aka (bbdb-record-aka record))
	(company (bbdb-record-company record))
	(notes (bbdb-record-notes record))
	(phones (bbdb-record-phones record))
	(addresses (bbdb-record-addresses record))
	(net (bbdb-record-net record))
	(categories (bbdb-record-getprop
		     record
		     bbdb-define-all-aliases-field)))
    (insert "begin:vcard\n"
	    "version:3.0\n")
    ;; Specify the formatted text corresponding to the name of the
    ;; object the vCard represents.  The property MUST be present in
    ;; the vCard object.
    (insert "fn:" (bbdb-vcard-export-escape name) "\n")
    ;; Family Name, Given Name, Additional Names, Honorific
    ;; Prefixes, and Honorific Suffixes
    (when (or last-name first-name)
      (insert "n:"
	      (bbdb-vcard-export-escape last-name) ";"  
	      (bbdb-vcard-export-escape first-name) ";;;\n"))
    ;; Nickname of the object the vCard represents.  One or more text
    ;; values separated by a COMMA character (ASCII decimal 44).
    (when aka
      (insert "nickname:" (bbdb-vcard-export-several aka) "\n"))
    ;; FIXME: use face attribute for this one.
    ;; PHOTO;ENCODING=b;TYPE=JPEG:MIICajCCAdOgAwIBAgICBEUwDQYJKoZIhvcN
    ;; AQEEBQAwdzELMAkGA1UEBhMCVVMxLDAqBgNVBAoTI05ldHNjYXBlIENvbW11bm
    ;; ljYXRpb25zIENvcnBvcmF0aW9uMRwwGgYDVQQLExNJbmZvcm1hdGlvbiBTeXN0

    ;; FIXME: use birthday attribute if there is one.
    ;; BDAY:1996-04-15
    ;; BDAY:1953-10-15T23:10:00Z
    ;; BDAY:1987-09-27T08:30:00-06:00

    ;; A single structured text value consisting of components
    ;; separated the SEMI-COLON character (ASCII decimal 59).  But
    ;; BBDB doesn't use this.  So there's just one level:
    (when company
      (insert "org:" (bbdb-vcard-export-escape company) "\n"))
    (when notes
      (insert "note:" (bbdb-vcard-export-escape notes) "\n"))
    (dolist (phone phones)
      (insert "tel;type=" (bbdb-vcard-export-escape (bbdb-phone-location phone)) ":"
	      (bbdb-vcard-export-escape (bbdb-phone-string phone)) "\n"))
    (dolist (address addresses)
      (insert (bbdb-vcard-export-address-string address) "\n"))
    (dolist (mail net)
      (insert "email;type=internet:" (bbdb-vcard-export-escape mail) "\n"))
    ;; Use CATEGORIES based on mail-alias.  One or more text values
    ;; separated by a COMMA character (ASCII decimal 44).
    (when categories
      (insert "categories:" 
	      (bbdb-join (mapcar 'bbdb-vcard-export-escape
				 (bbdb-split categories ",")) ",") "\n"))
    (insert "end:vcard\n")))
		    
(defun bbdb-vcard-export-vcard-name-from-record (record)
  "Come up with a vcard name given a record"
  (let ((name (bbdb-record-name record))
	(first-name (elt record 0))
	(last-name (elt record 1)))
    (concat first-name "_" last-name ".vcf")))

(defun bbdb-vcard-export-make-vcard (record vcard-name)
  "Make a record buffer and write it"
  (let ((buffer (get-buffer-create "*bbdb-vcard-export*")))
    (save-excursion
      (set-buffer buffer)
      (kill-region (point-min) (point-max))
      (bbdb-vcard-export-record-insert-vcard record)
      (write-region (point-min) (point-max) vcard-name))
    (kill-buffer buffer)))

(defun bbdb-vcard-do-record (record output-dir coding-system)
  "Update the vcard of one bbdb record"
  (setq coding-system (or coding-system 'utf-16))
  (let ((coding-system-for-write coding-system))
    (message "Updating %s" (bbdb-record-name record))
    (bbdb-vcard-export-make-vcard 
     record
     (concat output-dir
	     (bbdb-vcard-export-vcard-name-from-record record)))))

(defun bbdb-vcard-export-update-all (output-dir coding-system)
  "Update the vcard Contacts directory from the bbdb database"
  (interactive "DDirectory to update: \nZCoding system: ")
  (bbdb ".*" nil)
  (dolist (record (bbdb-records))
    (bbdb-vcard-do-record record output-dir coding-system)))

(defun bbdb-vcard-export (regexp output-dir coding-system)
  "Update the vcard Contacts directory from records matching REGEXP"
  (interactive "sExport records matching: \nDDirectory to update: \nZCoding system: ")
  (bbdb regexp nil)
  (let ((notes (cons '* regexp)))
    (dolist (record (bbdb-search (bbdb-records) regexp regexp regexp notes nil))
      (message "Updating %s" (bbdb-record-name record))
      (bbdb-vcard-do-record record output-dir coding-system))))

(defun bbdb-vcard-export-current (output-dir coding-system)
  "Update the vcard of the current record"
  (interactive "DDirectory to update: \nZCoding system: ")
  (let ((record (bbdb-current-record nil)))
    (bbdb-vcard-do-record record output-dir coding-system)))

(define-key bbdb-mode-map [(v)] 'bbdb-vcard-export-current)


(provide 'bbdb-vcard-export)

;;; bbdb-vcard-export.el ends here
