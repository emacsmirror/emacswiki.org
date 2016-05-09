;;; jl-sime.el --- Improved support for S/MIME with MML and LDAP
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2011, 2012, 2013, 2016 Jens Lechtenbörger

;; Changelog:
;; 2012/03/09, Version 1.1, initial release
;; 2012/05/01, Version 1.2, extract certificate from signed message to file
;; 2013/06/29, Version 2.1:
;;    - compatibility with jl-encrypt.el 3.1
;;    - customizable LDAP search for missing certificates upon send
;;    - moved customization for other packages from code to comments
;; 2013/12/20, Version 3-beta:
;;    - switch from openssl to gpgsm via EasyPG (certificates are no longer
;;      stored in files but imported into gpgsm)
;;    - new customizable variable jl-smime-negcache-maxage
;;    - new command jl-smime-key-available-p
;; 2013/12/31, Version 3.1:
;;    - updated documentation
;; 2016/03/28, Version 3.2:
;;    - version for Emacs 25.1, no change in functionality
;; 2016/05/09, Version 3.3:
;;    - bug fix, replace mml-epg-find-usable-keys with
;;      mml-secure-find-usable-keys

;; Compatibility: GNU Emacs 25.1

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

;; Keywords: mail, encryption, S/MIME, CMS, LDAP

;; URL: http://www.emacswiki.org/emacs/jl-smime.el
;; EmacsWiki: ExtendSMIME

;; A signed version of this file is available over there:
;; http://www.informationelle-selbstbestimmung-im-internet.de/emacs/

;; Requires: jl-encrypt.el   http://www.emacswiki.org/emacs/jl-encrypt.el

;;; Commentary:
;; ExtendSMIME enhances DefaultEncrypt to support S/MIME.  Moreover, it allows
;; to retrieve certificates via LDAP and to import them into gpgsm.
;;
;; Preliminaries:
;; I use, but do not recommend, S/MIME.  I recommend GnuPG.
;; Here is what I think about the “trust” model of S/MIME:
;; https://blogs.fsfe.org/jens.lechtenboerger/2013/12/23/openpgp-and-smime/
;;
;; Functionality:
;; The general idea of ExtendSMIME is to enhance S/MIME support in Gnus for
;; * retrieval of certificates from LDAP servers with import into gpgsm
;;   (smime-cert-by-ldap-1 redefined, including a bug fix and negative
;;   caching) and
;; * (via jl-encrypt.el) automatic insertion of MML tags into messages if
;;   certificates for all recipients are available.
;;
;; Note that version 3 of ExtendSMIME introduces a major change: Previous
;; versions used openssl for cryptographic operations while version 3 uses
;; gpgsm (part of gnupg-2).  In particular, certificates are no longer
;; maintained by ExtendSMIME but are imported into gpgsm.  In fact, gpgsm
;; manages certificates on its own and automatically extracts certificates
;; from signed messages, which simplifies the code of ExtendSMIME a lot.
;;
;; The general idea of ExtendSMIME is to search for missing certificates via
;; LDAP before an e-mail is sent.  Resulting certificates are imported
;; into gpgsm.  LDAP search is customizable (and documented) via
;; `jl-smime-permit-ldap'.
;;
;; In addition to this automatic LDAP search, the command
;; `M-x jl-smime-key-available-p' allows manual searches.  It asks for an
;; e-mail address (an address at point is used as default value).  If no
;; certificate is available, an LDAP search is started (ignoring negative
;; caching and `jl-smime-permit-ldap').  If a certificate is found, it is
;; imported into gpgsm; otherwise, a negative cache entry is created.
;;
;; Documentation concerning S/MIME with GNU Emacs is mostly targeting openssl
;; but not gpgsm.  See installation instructions below.

;; Install:
;; Message security (as described in Info node `(message) Security')
;; https://www.gnu.org/software/emacs/manual/html_node/message/Security.html
;; automatically prefers EasyPG with gpgsm over openssl if EasyPG is loaded
;; first.  (See `mml-smime-use'.)
;; Thus, place jl-smime.el as well as jl-encrypt.el into your load-path,
;; and add the following to ~/.emacs:
;;
;;     (require 'epa-file)
;;     (load "jl-smime")
;;
;; Optionally, but unrelated to e-mail, you may also want:
;;     (setq epa-file-encrypt-to "<your-key-id>")
;;     (setq epg-debug t)
;;
;; Customizable variables of ExtendSMIME are jl-smime-permit-ldap,
;; jl-smime-negative-cache-dir, and jl-smime-negcache-maxage.
;; You may want to read their documentation.
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
;; If you used openssl previously, you need to import your keys and
;; certificates into gpgsm.  Here are three-and-a-half steps, which worked for
;; me.
;;
;; First, import the private key.
;; $ gpgsm --import my-private-key.p12
;;
;; Second, the CA signing my certificate is unknown to gpgsm (gpg-agent, in
;; fact).  Thus, gpgsm refuses to use that key pair.  I added the option
;; `allow-mark-trusted' to ~/.gnupg/gpg-agent.conf.  Then I tried again to
;; sign a file.  This time, gpg-agent displayed the root CA's fingerprint and
;; asked whether I trust it.  Saying "yes" added that fingerprint to
;; ~/.gnupg/trustlist.txt, and the key became usable.  (Note that you must
;; “trust” the root CA; adding an intermediate CA's fingerprint does not work.
;; You read my article on “trust”, didn’t you?)
;;
;; Third, import (a subset of) your old certificates.  With ExtendSMIME before
;; version 3, those were stored in files under
;; `smime-certificate-directory', ~/Mail/certs, by default.  In that
;; directory, execute: $ gpgsm --import <certfiles>
;;
;; Finally, decide how you ensure that you can decrypt your own e-mails.
;; See comments for DefaultEncrypt in jl-encrypt.el and note that, similarly
;; to gpg, gpgsm understands the option "--encrypt-to" (which is not
;; mentioned in the man page).  I have the following line in gpgsm.conf:
;; encrypt-to <colon-separated-fingerprint-of-own-key>

;; This file is *NOT* part of GNU Emacs.

;;; Code:
(require 'smime)
(require 'mml-smime)
(require 'jl-encrypt)

(defgroup jl-smime nil
  "Customization options for jl-smime.el which extend jl-encrypt.el"
  :group 'jl-encrypt)

(defcustom jl-smime-permit-ldap nil
  "Control whether LDAP queries should look for missing certificates.
If this variable is nil (the default), LDAP queries are only
performed by standard Gnus message behavior (with EasyPG probably
never).
Otherwise, this variable must be a regular expression matching
e-mail addresses.  Whenever an S/MIME certificate is missing for
an e-mail address that (a) matches this regular expression
and (b) is not recorded under `jl-smime-negative-cache-dir', an
LDAP query for a certificate is performed.
Note that such an LDAP query tells the LDAP server and (if you do
not use encrypted LDAPS communication) *every* party controlling
any node or link between you and that server to whom you are
about to send an e-mail.  Such information leakage may or may not
be tolerable in your situation.

At work I'm using \"@\\(.+\\.\\)?uni-muenster\\.de$\" to check for
certificates for all addresses under \"uni-muenster.de\".
Thus, if I send an e-mail to an address within my domain, and gpgsm
does not have a certificate for that address, an LDAP search is
started.  If that search returns a certificate, it is imported
into gpgsm.  If no certificate is found, a negative cache entry
for the e-mail address is created.  As long as that entry exists
\(see `jl-smime-negcache-maxage'), no further LDAP query will be
started for that address."
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

(defcustom jl-smime-negcache-maxage 0
  "Number of days after which negative cache entries expire.
If 0, keep entries forever."
  :group 'jl-smime
  :type '(integer))

;;
;; No configuration options beyond this point.  Just code.
;;

(add-to-list 'mml-secure-method-table
	     '(CMS (("test" jl-smime-key-available-p)
		    ("doit" jl-secure-message-smime)
		    ("ask" "S/MIME certificates available \
for all recipients.  Really proceed *without* encryption? "))))

;; cadaar is defined in cl.el
(require 'cl)
(defun jl-smime-cert-by-ldap-1 (mail host)
  "Get certificate for MAIL from LDAP server HOST and import into gpgsm."
  ;; Following funcall and subsequent test on ldapresult are copied from
  ;; smime-cert-by-ldap-1.  However, here the ldapresult is imported into
  ;; gpgsm or a negative cache entry is created, and no certificate buffer is
  ;; created.
  (let ((ldapresult
	 ;; JL: In some versions of emacs, ldapresult contains lots of
	 ;; nil elements.
	 ;; These seem to be unexpected in the let body below.
	 ;; Hence, the following remove operation is necessary...
	 (remove nil (funcall
	  (if (or (featurep 'xemacs)
		  ;; For Emacs >= 22 we don't need smime-ldap.el
		  (< emacs-major-version 22))
	      (progn
		(require 'smime-ldap)
		'smime-ldap-search)
	    'ldap-search)
	  (concat "mail=" mail)
	  host '("userCertificate") nil))))
    (if (and (>= (length ldapresult) 1)
             (> (length (cadaar ldapresult)) 0))
	(jl-smime-import-ldap-certs ldapresult mail)
      ;; No certificate found.  Record in negative cache.
      (jl-smime-add-negcache mail)
      nil)))

(defun jl-smime-cert-by-ldap (mail)
  "Find certificate via LDAP for address MAIL."
  ;; Iteration over smime-ldap-host-list taken from smime-cert-by-ldap
  ;; in smime.el.
  (if smime-ldap-host-list
      (catch 'imported
	(dolist (host smime-ldap-host-list)
	  (let ((result (jl-smime-cert-by-ldap-1 mail host)))
	    (when result
	      (let ((considered (car result))
		    (imported (cadr result)))
		(if (< 0 considered)
		    (if (= 0 imported)
			(message "No new keys (out of %s) for %s at host \"%s\""
				 considered mail host)
		      (message "Imported %s key(s) for %s at host \"%s\""
			       imported mail host)
		      (throw 'imported imported))
		  (message "No keys for %s at host \"%s\"" mail host)))))))))

(defun jl-smime-import-ldap-certs (ldapresult mail)
  "Import certificates from LDAPRESULT for MAIL into EasyPG (gpgsm)."
  (let ((context (epg-make-context 'CMS))
	(imported 0)
	(considered 0))
    (dolist (entry ldapresult (list considered imported))
      (when (> (length (cadar entry)) 0)
	(epg-import-keys-from-string context (cadar entry))
	(let ((result (epg-context-result-for context 'import)))
	  (setq imported (+ imported (epg-import-result-imported result))
		considered (+ considered (epg-import-result-considered result)))
	)))))

(defun jl-smime-key-available-p (recipient &optional forced)
  "Check whether EasyPG knows an S/MIME certificate for RECIPIENT.
The check may initiate an LDAP search to retrieve such a certificate.
If optional FORCED is true, do not check whether an LDAP query is permitted
when a certificate is missing.  If called interactively, an e-mail address at
point is used as default value for RECIPIENT and FORCED is set to t."
  (interactive (list
                (read-string (format "E-Mail address for LDAP search (%s): "
				     (thing-at-point 'email))
                             nil nil (thing-at-point 'email))
		t))
  (let* ((context (epg-make-context 'CMS))
	 (found (or (mml-secure-find-usable-keys context recipient 'encrypt)
		    (when (or forced (jl-smime-ldap-permitted-p recipient))
		      (jl-smime-cert-by-ldap recipient)
		      (mml-secure-find-usable-keys
		       context recipient 'encrypt))))
	 (no (length found)))
    (if forced
	;; Display of message in interactive call does not return nil in case
	;; of missing certificate, which I find OK.
	(if (= 0 no)
	    (message "No certificate found")
	  (jl-smime-del-negcache recipient)
	  (message "Found %s certificate(s)" no))
      found)))

(defun jl-smime-cachename (email)
  "Internal function to compute filename for certificate of EMAIL."
  (concat jl-smime-negative-cache-dir (downcase email)))

(defun jl-smime-add-negcache (email)
  "Create negative cache entry for EMAIL."
  (let ((cachename (jl-smime-cachename email)))
    (write-region "" nil cachename)
    (message "Created negative cache entry %s" cachename)))

(defun jl-smime-del-negcache (email)
  "Remove negative cache file for EMAIL if it exists.
Always return nil."
  (let ((cachename (jl-smime-cachename email)))
    (when (file-exists-p cachename)
      (delete-file cachename)
      (message "Deleted negative cache entry %s" cachename))
    nil))

(defun jl-smime-negcache-expire (email)
  "Delete cache entry for EMAIL if it has expired.
An entry expires once it is older than `jl-smime-negcache-maxage'."
  (let ((cachename (jl-smime-cachename email)))
    (if (and (> jl-smime-negcache-maxage 0)
	     (file-exists-p cachename))
	(unless (> jl-smime-negcache-maxage
		   (- (time-to-days (current-time))
		      (time-to-days (nth 6 (file-attributes cachename)))))
	  (delete-file cachename)
	  (message "Expired negative cache entry %s" cachename)))))

(defun jl-smime-isnegcached-p (email)
  "Return non-nil if EMAIL is negatively cached."
  (let ((cachename (jl-smime-cachename email)))
    (jl-smime-negcache-expire email)
    (file-exists-p cachename)))

(defun jl-smime-ldap-permitted-p (email)
  "Return non-nil if LDAP query for EMAIL is permitted.
An LDAP query is permitted if (a) no negative cache file for
email exists, (b) `jl-smime-permit-ldap' is not nil but a regular
expression, and (c) email matches `jl-smime-permit-ldap'."
  (and (not (jl-smime-isnegcached-p email))
       (stringp jl-smime-permit-ldap)
       (string-match jl-smime-permit-ldap email)))

(defun jl-secure-message-smime ()
  "Invoke MML function to add appropriate secure tag for S/MIME.
Creation of signatures is controlled by `mml-secure-do-not-sign-p'."
  (mml-secure-message-encrypt-smime (mml-secure-do-not-sign-p)))

;;; jl-smime.el ends here
