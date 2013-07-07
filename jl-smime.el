;;; jl-sime.el --- Improved support for S/MIME with MML
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2011, 2012, 2013 Jens Lechtenbörger

;; Version: $Id: jl-smime.el,v 1.6 2013/06/29 10:00:10 lechten Exp $
;; Changelog:
;; 2012/03/09, Version 1.1, initial release
;; 2012/05/01, Version 1.2, extract certificate from signed message to file
;; 2013/06/29, Version 2.1:
;;    - compatibility with jl-encrypt.el 3.1
;;    - customizable LDAP search for missing certificates upon send
;;    - moved customization for other packages from code to comments

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
;; You may want to recall some certificate authority failures such as
;; Comodo (2011), DigiNotar (2011), and Trustwave (2012):
;; http://www.h-online.com/security/news/item/SSL-meltdown-a-cyber-war-attack-1214104.html
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
;;   (smime-cert-by-ldap-1 redefined, including a bug fix and negative
;;   caching),
;; * extraction and caching of certificates from signed messages
;;   (smime-verify-region enhanced),
;; * and (via jl-encrypt.el) automatic insertion of MML tags into messages if
;;   certificates for all recipients are available.

;; The general idea of ExtendSMIME is to store available certificates in
;; files under smime-certificate-directory.  The file names under which a
;; certificate is stored are its e-mail addresses.
;; A certificate file is written in two cases: First, if a certificate is
;; received on demand via LDAP (note that LDAP searches can be customized via
;; jl-smime-permit-ldap).  Second, if a signed message containing a
;; certificate is verified successfully then the contained certificate is
;; stored for each of its e-mail addresses.

;; For basic setup information concerning S/MIME in GNU Emacs (Gnus, in
;; fact), I recommend that you read Info node `(message) Security'
;; https://www.gnu.org/software/emacs/manual/html_node/message/Security.html
;; and the Emacs Wiki entry: http://www.emacswiki.org/emacs/GnusSMIME

;; Install:
;; Place this file as well as jl-encrypt.el into your load-path,
;; and add the following to ~/.emacs:
;;
;;     (load "jl-smime")
;;
;; Customizable variables are jl-smime-permit-ldap and
;; jl-smime-negative-cache-dir.  You may want to read their documentation.
;;
;; I'm using the following code after (load "jl-smime") in my setup:
;;
;; ;; Allow automatic LDAP queries for certificates within my domain.
;; (setq jl-smime-permit-ldap "@\\(.+\\.\\)?uni-muenster\\.de$")
;;
;; ;; I'm searching for S/MIME certificates via LDAPS at DFN-Verein.
;; ;; Note that ldap.el in Emacs requires a minor workaround to perform
;; ;; encrypted connections via LDAPS.  In fact, ldapsearch is being invoked
;; ;; to use unencrypted plaintext LDAP communication with the parameter "-h".
;; ;; Maybe I'm doing something wrong but I only got LDAPS to work with the
;; ;; parameter "-H ldaps://ldap.pca.dfn.de".  To get rid of the default
;; ;; parameter -h, I'm passing the empty string as hostname, setting
;; ;; smime-ldap-host-list to '("").  Finally, ldapsearch aborts the
;; ;; connection if it is not told where to find the CA certificate for the
;; ;; LDAPS server (which is a Good Thing).
;; ;; I created ~/.ldaprc with a single line pointing to that CA certificate:
;; ;; TLS_CACERT /path/to/server/cert
;; (require 'ldap)
;; (setq smime-ldap-host-list '(""))
;; (setq ldap-default-base "O=DFN-Verein,C=DE"
;;       ; -x: no SASL authentication, -tt: store result in file
;;       ; -H: connect to specified URI.
;;       ldap-ldapsearch-args '("-x" "-tt" "-H ldaps://ldap.pca.dfn.de")
;;       )
;;
;; ;; Cache S/MIME passphrase for 600 seconds.  (Default is 16.)
;; (require 'password-cache)
;; (setq password-cache-expiry 600)
;;
;; For the record: Previously, I used plaintext LDAP with the following
;; differences to the above setup:
;; (setq ldap-ldapsearch-args '("-x" "-tt"))
;; (setq smime-ldap-host-list '("ldap.pca.dfn.de"))

;; This file is *NOT* part of GNU Emacs.

;;; Code:
(require 'smime)
(require 'mml-smime)
(require 'jl-encrypt)

(defgroup jl-smime nil
  "Customization options for jl-smime.el which extend jl-encrypt.el"
  :group 'jl-encrypt)

(defcustom jl-smime-permit-ldap nil
  "Control when LDAP queries should look for missing certificates.
If this variable is nil (the default), LDAP queries are only
performed by standard Gnus message behavior (e.g., if you insert
an S/MIME encryption tag via `C-c RET c s').
Otherwise, this variable must be a regular expression matching
e-mail addresses.  Whenever an S/MIME certificate is missing for
an e-mail address that (a) matches this regular expression
and (b) is not recorded under `jl-smime-negative-cache-dir', an
LDAP query for a certificate is performed.
Note that such an LDAP query tells the LDAP server and—if you do
not use encrypted LDAPS communication—*every* party controlling
any node or link between you and that server to whom you are
about to send an e-mail.  Such information leakage may or may not
be tolerable in your situation.
At work I'm using \"@\\(.+\\.\\)?uni-muenster\\.de$\" to check for
certificates for all addresses under \"uni-muenster.de\"."
  :group 'jl-smime
  :type '(choice (const nil) (regexp)))

(defun jl-smime-mkcachedir ()
  "Internal function to compute default name of negative cache directory."
  (let ((result (file-name-as-directory
		 (concat
		  (file-name-as-directory smime-certificate-directory)
		  "negative-cache"))))
    (if (not (file-directory-p result))
	(make-directory result t))
    result))

(defcustom jl-smime-negative-cache-dir (jl-smime-mkcachedir)
  "Directory to record e-mail addresses for which LDAP failed.
LDAP queries are used to search for missing S/MIME certificates.
If a query fails, its e-mail address is recorded under this
directory.  As long as an e-mail address is recorded there,
jl-smime will not initiate further LDAP queries for that e-mail
address.  An e-mail address will be removed from the negative
cache if jl-smime observes its certificate (e.g., if you receive
a signed e-mail containing the certificate or if
`smime-cert-by-ldap-1' retrieves the certificate, possibly when
you perform `C-c RET c s').
Essentially, this negative caching is meant to limit the amount
of information leakage explained under `jl-smime-permit-ldap'."
  :group 'jl-smime
  :type '(string))

;;
;; No configuration options beyond this point.  Just code.
;;

;; S/MIME does not have an encrypt-to option like GnuPG in gpg.conf.
;; Compensate for this lack:
(setq jl-encrypt-ignore-from nil)

(add-to-list 'jl-method-table
	     '("smime" (("test" jl-certfile-available-p)
			("doit" jl-secure-message-smime)
			("ask" "S/MIME certificates available \
for all recipients.  Really proceed *without* encryption? "))))

;;
;; Some redefinitions.
;;

;; Redefine smime-cert-by-ldap-1 to work around a bug and to either store
;; retrieved certificate in file or create negative cache file.
(require 'cl)
(defun smime-cert-by-ldap-1 (mail host)
  "Get certificate for MAIL from the ldap server at HOST."
  (let ((certdir (file-name-as-directory smime-certificate-directory))
	(ldapresult
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
	  (write-file (concat certdir mail) t)
	  (jl-smime-del-negcache mail)
	  )
      (kill-buffer retbuf)
      (setq retbuf nil)
      ;; JL: Create negative cache file
      (jl-smime-add-negcache mail))
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
written into a file under `smime-certificate-directory'."
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

(defun jl-certfile-available-p (recipient)
  "Check whether certificate file is available for RECIPIENT.
This tests whether `smime-certificate-directory' contains a
certificate file whose name equals the e-mail address of
RECIPIENT (which is in the format of
`mail-extract-address-components') or its lower-case variant."
  (let ((certdir (file-name-as-directory smime-certificate-directory))
	(email (downcase recipient)))
    (if (jl-smime-certfile-exists-p recipient)
	t
      (if (jl-smime-ldap-permitted-p email)
	  (smime-cert-by-ldap email))
      (file-exists-p (concat certdir email)))))

(defun jl-smime-certfile-exists-p (recipient)
  "Check whether certificate file for RECIPIENT exists."
  (let ((email (downcase recipient)))
    (or
     (file-exists-p (concat certdir recipient))
     (file-exists-p (concat certdir email)))))

(defun jl-smime-isnegcached-p (email)
  "Check whether EMAIL is present in negative cache."
  (file-exists-p (concat jl-smime-negative-cache-dir email)))

(defun jl-smime-ldap-permitted-p (email)
  "Check whether LDAP query for EMAIL is permitted.
An LDAP query is permitted if (a) no negative cache file for
email exists, (b) `jl-smime-permit-ldap' is not nil but a regular
expression, and (c) email matches `jl-smime-permit-ldap'."
  (and (not (jl-smime-isnegcached-p email))
       (stringp jl-smime-permit-ldap)
       (string-match-p jl-smime-permit-ldap email)))

(defun jl-smime-add-negcache (email)
  "Create negative cache file for EMAIL."
  (let ((cachename (concat jl-smime-negative-cache-dir email)))
    (write-region "" nil cachename)
    (message "Created negative cache file for %s" email)))

(defun jl-smime-del-negcache (email)
  "Remove negative cache file for EMAIL if it exists."
  (let ((cachename (concat jl-smime-negative-cache-dir email)))
    (when (file-exists-p cachename)
      (delete-file cachename)
      (message "Deleted negative cache file for %s" email))))

(defun jl-secure-message-smime ()
  "Invoke MML function to add appropriate secure tag for S/MIME.
Creation of signatures is controlled by `jl-do-not-sign-p'."
  (mml-secure-message-encrypt-smime (jl-do-not-sign-p)))

(defun jl-fetch-certs-possibly-from-ldap (addresses)
  "Return list of certificate file names for given ADDRESSES.
First, check for existing file; then retrieve file via LDAP."
    (if (= (length addresses) 0)
      nil
    (let* ((email (car addresses))
	   (certdir (file-name-as-directory smime-certificate-directory))
	   (certfile (concat certdir email))
	   )
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
copied to `smime-certificate-directory' with the e-mail address as filename.
Afterwards, TMPFILE is deleted."
  (unless (file-readable-p tmpfile)
    (error "Certfile not available: %s" tmpfile))
  (smime-new-details-buffer)
  (with-current-buffer smime-details-buffer
    (when (call-process smime-openssl-program nil smime-details-buffer nil
			"x509" "-in" tmpfile "-email" "-noout")
      (let ((certdir (file-name-as-directory smime-certificate-directory))
	    (addresses (mapcar 'downcase (smime-buffer-as-string-region
					  (point-min) (point-max)))))
	(dolist (address addresses t)
	  (copy-file tmpfile (concat certdir address) t)
	  (message "Wrote certificate for %s" address)
	  (jl-smime-del-negcache address))
	(delete-file tmpfile)
	t))))
;;; jl-smime.el ends here
