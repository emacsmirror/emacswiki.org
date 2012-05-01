;;; jl-sime.el --- Improved support for S/MIME with MML
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2011, 2012 Jens Lechtenbörger

;; Version: $Id: jl-smime.el,v 1.2 2012/05/01 07:50:43 lechten Exp lechten $
;; Compatibility: Should work with GNU Emacs 23.1 and later

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.
;; If not, see http://www.gnu.org/licenses/ or write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Keywords: mail, encryption, S/MIME, LDAP

;; URL: http://www.emacswiki.org/emacs/jl-smime.el
;; EmacsWiki: ExtendSMIME

;; A signed version of this file is available over there:
;; http://www.informationelle-selbstbestimmung-im-internet.de/emacs/

;; Requires: jl-encrypt.el   http://www.emacswiki.org/emacs/jl-encrypt.el

;;; Commentary:
;; In general, I recommend GnuPG over S/MIME (which is the default for Gnus).
;; Unless you are in a restricted setting, say in a university or corporate
;; setting, the trust model regarding certificate authorities is broken.
;; You may want to recall the recent certificate authority failures concerning
;; DigiNotar and Trustwave:
;; https://freedom-to-tinker.com/blog/sjs/diginotar-hack-highlights-critical-failures-our-ssl-web-security-model
;; http://www.h-online.com/news/item/Trustwave-issued-a-man-in-the-middle-certificate-1429982.html
;; Although the impact of those failures appears to be restricted to SSL
;; certificates, I don't see any reason why S/MIME certificates should be more
;; trustworthy.

;; Having said that, in my university, I trust the CA to sign keys for the
;; correct owners, and more colleagues seem to own S/MIME certificates than
;; GnuPG keys.

;; S/MIME support in Gnus is somewhat limited.  This file redefines some Gnus
;; functions and adds new ones to include proper support for
;; * multiple recipients (smime-encrypt-region enhanced),
;; * retrieval  and caching of certificates from LDAP servers
;;   (smime-cert-by-ldap-1 redefined, including a bug fix),
;; * extraction and caching of certificates from signed messages
;;   (smime-verify-region enhanced),
;; * and (via jl-encrypt.el) automatic insertion of MML tags into messages if
;;   certificates for all recipients are available.

;; The general idea of ExtendSMIME is to store available certificates in
;; files under smime-certificate-directory.  The file names under which a
;; certificate is stored are its e-mail addresses.
;; A certificate file is written in two cases:  First, if a certificate is
;; received on demand via LDAP.  Second, if a signed message containing a
;; certificate is verified successfully then the contained certificate is
;; stored for each of its e-mail addresses.

;; For basic setup information concerning S/MIME in GNU Emacs (Gnus, in
;; fact), I recommend that you read Info node `(message) Security'
;; https://www.gnu.org/software/emacs/manual/html_node/message/Security.html
;; and the Emacs Wiki entry: http://www.emacswiki.org/emacs/GnusSMIME

;; Install:
;; Place this file as well as jl-encrypt.el into your load-path,
;; and add the following to ~/.emacs:

;;     (load "jl-smime")

;; You may want to check the subsequent comments to understand the rationale
;; of my modifications and of my additions to standard message behavior.

;; This file is *NOT* part of GNU Emacs.

;;; Code:
(require 'smime)
(require 'mml-smime)
(require 'jl-encrypt)

;; S/MIME does not have an encrypt-to option like GnuPG in gpg.conf.
;; To compensate for this lack, add From header to recipients.
(add-to-list 'jl-recipient-headers "from")

;; I'm searching for S/MIME certificates via LDAP at DFN-Verein.
(require 'ldap)
(setq ldap-default-base "O=DFN-Verein,C=DE"
      ; -x: no SASL authentication, -tt: store result in file
      ldap-ldapsearch-args '("-x" "-tt")
      )

;; Cache S/MIME passphrase for 600 seconds.  (Default is 16.)
(require 'password-cache)
(setq password-cache-expiry 600)

;;
;; No configuration options beyond this point.  Just code.
;;

;;
;; First, some redefinitions.
;;

;; Redefine smime-cert-by-ldap-1 to work around a bug and to store
;; retrieved certificates in files.
(defun smime-cert-by-ldap-1 (mail host)
  "Get certificate for MAIL from the ldap server at HOST."
  (let ((ldapresult
	 ;; JL: ldapresult contains lots of nil elements.
	 ;; These seem to be unexpected in the let body below.
	 ;; Hence, the following line is necessary...
	 (remove nil (funcall
	  (if (or (featurep 'xemacs)
		  ;; For Emacs >= 22 we don't need smime-ldap.el
		  (< emacs-major-version 22))
	      (progn
		(require 'smime-ldap)
		'smime-ldap-search)
	    'ldap-search)
	  (concat "mail=" mail)
	  host '("userCertificate") nil)))
	(retbuf (generate-new-buffer (format "*certificate for %s*" mail)))
	cert)
    (if (and (>= (length ldapresult) 1)
             (> (length (cadaar ldapresult)) 0))
	(with-current-buffer retbuf
	  ;; Certificates on LDAP servers _should_ be in DER format,
	  ;; but there are some servers out there that distributes the
	  ;; certificates in PEM format (with or without
	  ;; header/footer) so we try to handle them anyway.
	  (if (or (string= (substring (cadaar ldapresult) 0 27)
			   "-----BEGIN CERTIFICATE-----")
		  (string= (substring (cadaar ldapresult) 0 3)
			   "MII"))
	      (setq cert
		    (smime-replace-in-string
		     (cadaar ldapresult)
		     (concat "\\(\n\\|\r\\|-----BEGIN CERTIFICATE-----\\|"
			     "-----END CERTIFICATE-----\\)")
		     "" t))
	    (setq cert (base64-encode-string (cadaar ldapresult) t)))
	  (insert "-----BEGIN CERTIFICATE-----\n")
	  (let ((i 0) (len (length cert)))
	    (while (> (- len 64) i)
	      (insert (substring cert i (+ i 64)) "\n")
	      (setq i (+ i 64)))
	    (insert (substring cert i len) "\n"))
	  (insert "-----END CERTIFICATE-----\n")
	  ;; JL: Store retrieved cert...
	  (write-file (concat smime-certificate-directory mail) t)
	  )
      (kill-buffer retbuf)
      (setq retbuf nil))
    retbuf))

;; Redefine smime-encrypt-region to handle multiple recipients/certfiles.
(defun smime-encrypt-region (b e certfiles)
  "Encrypt region between B and E for recipients specified in CERTFILES.
If encryption fails, the buffer is not modified.  Region is
assumed to have proper MIME tags.  CERTFILES musts be a list of
filenames.  If that list contains exactly one string, then this
string may either be a single filename or a list of filenames
separated by `;'.  Each file is expected to contain a PEM encoded
certificate."
  (smime-new-details-buffer)
  (let ((buffer (generate-new-buffer " *smime*"))
	(tmpfile (smime-make-temp-file "smime"))
	;; JL: Handle multiple certfiles encoded in one string
	(jl-certfiles (if (and (= (length certfiles) 1)
			      (not (file-readable-p (car certfiles))))
			 (split-string (car certfiles) ";")
		       certfiles)))
    (prog1
	(when (prog1
		  (apply 'smime-call-openssl-region b e (list buffer tmpfile)
			 "smime" "-encrypt" smime-encrypt-cipher
			 (mapcar 'expand-file-name jl-certfiles))
		(with-current-buffer smime-details-buffer
		  (insert-file-contents tmpfile)
		  (delete-file tmpfile)))
	  (delete-region b e)
	  (insert-buffer-substring buffer)
	  (goto-char b)
	  (when (looking-at "^MIME-Version: 1.0$")
	    (delete-region (point) (progn (forward-line 1) (point))))
	  t)
      (with-current-buffer smime-details-buffer
	(goto-char (point-max))
	(insert-buffer-substring buffer))
      (kill-buffer buffer))))

;; Usually, signed messages contain the signer's certificate.
;; Redefine smime-verify-region to write that certificate to a file.
(defun smime-verify-region (b e)
  "Verify S/MIME message in region between B and E.
Returns non-nil on success.
Any details (stdout and stderr) are left in the buffer specified by
`smime-details-buffer'.
If signature verification is successful, the signing certificate is
written into a file under smime-certificate-directory."
  (smime-new-details-buffer)
  (let ((CAs (append (if smime-CA-file
			 (list "-CAfile"
			       (expand-file-name smime-CA-file)))
		     (if smime-CA-directory
			 (list "-CApath"
			       (expand-file-name smime-CA-directory))))))
    (unless CAs
      (error "No CA configured"))
    (if smime-crl-check
	(add-to-list 'CAs smime-crl-check))
    ;; JL: Added
    ;; - let statement to create tmpfile,
    ;; - options '"-signer" tmpfile' in call to openssl to write certificate,
    ;; - and call jl-rename-certfile.
    (let ((tmpfile (smime-make-temp-file "smime")))
      (if (apply 'smime-call-openssl-region b e (list smime-details-buffer t)
	       "smime" "-verify" "-signer" tmpfile "-out" "/dev/null" CAs)
	(jl-rename-certfile tmpfile)
      (insert-buffer-substring smime-details-buffer)
      nil))))

;; Redefine mml-smime-openssl-encrypt-query to deal with all addresses of
;; a message.
(defun mml-smime-openssl-encrypt-query ()
  "Return certificate file names to which to encrypt, separated by `;'."
  (list 'certfile
	(mapconcat 'identity
		   (jl-fetch-certs-possibly-from-ldap
		    (jl-message-fetch-recipients))
		   ";")))

;;
;; New functions to make the above work.
;;
(defun jl-fetch-certs-possibly-from-ldap (addresses)
  "Return list of certificate file names for given ADDRESSES.
First, check for existing file; then retrieve file via LDAP."
    (if (= (length addresses) 0)
      nil
    (let* ((email (cadar addresses))
	  (certfile (concat smime-certificate-directory email)))
      (unless (file-readable-p certfile)
	(message "Trying to get certificate via LDAP: %s" email)
	(smime-cert-by-ldap email))
      (unless (file-readable-p certfile)
	(error "Certfile not available for recipient: %s" email))
      (cons certfile
	    (jl-fetch-certs-possibly-from-ldap (cdr addresses))))))

(defun jl-rename-certfile (tmpfile)
  "Extract e-mail addresses from TMPFILE and copy TMPFILE once per address.
TMPFILE must contain a certificate.  For each e-mail address, TMPFILE is
copied to smime-certificate-directory with the e-mail address as filename.
Afterwards, TMPFILE is deleted."
  (unless (file-readable-p tmpfile)
    (error "Certfile not available: %s" tmpfile))
  (smime-new-details-buffer)
  (with-current-buffer smime-details-buffer
    (when (call-process smime-openssl-program nil smime-details-buffer nil
			"x509" "-in" tmpfile "-email" "-noout")
      (let ((addresses (mapcar 'downcase (smime-buffer-as-string-region
					  (point-min) (point-max)))))
	(dolist (address addresses t)
	  (copy-file tmpfile (concat smime-certificate-directory address) t))
	(delete-file tmpfile)
	t))))
;;; jl-smime.el ends here
