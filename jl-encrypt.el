;;; jl-encrypt.el --- Insert MML encryption tags if public keys are available
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2011, 2012, 2013 Jens Lechtenbörger
;; Copyright (C) 2013 konubinix

;; Version: $Id: jl-encrypt.el,v 1.3 2013/03/13 06:58:20 lechten Exp $
;; Changelog:
;; 2012/03/09, Version 1.1, initial release
;; 2013/03/12, Version 1.2, integrate patch by konubinix to also encrypt
;;   e-mail that already contains a secure sign tag:
;;   - Add functions jl-is-signed-p and jl-do-not-sign-p.
;;   - Change jl-secure-message-gpg and jl-secure-message-smime to use
;;     jl-do-not-sign-p (and not only jl-encrypt-without-signature).
;;   - Change jl-message-send-maybe-exit to look for secure encrypt tag
;;     (instead of any secure tag).
;; 2013/03/13, Version 1.3, add Changelog, give credit to konubinix

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

;; Keywords: mail, encryption, GnuPG, gpg, S/MIME, OpenPGP

;; URL: http://www.emacswiki.org/emacs/jl-encrypt.el
;; EmacsWiki: DefaultEncrypt

;; A signed version of this file is available over there:
;; http://www.informationelle-selbstbestimmung-im-internet.de/emacs/

;;; Commentary:
;; If you sometimes send plaintext e-mails that really should have been
;; encrypted ones, then this file may be for you.
;; I assume that you are familiar with GnuPG support for Gnus messages as
;; described in Info node `(message) Security':
;; https://www.gnu.org/software/emacs/manual/html_node/message/Security.html
;;
;; The code aims for automatic insertion of an MML secure encryption tags into
;; messages if public keys (either GnuPG public keys or S/MIME certificates)
;; for all recipients are available.  In addition, before a message is sent,
;; the user is asked if plaintext should really be sent unencryptedly when
;; public keys for all recipients are available.
;; This works by rebinding `C-c C-c' and `C-c C-s' as well as by adding
;; `jl-encrypt-if-possible' to `gnus-message-setup-hook'.
;; If you are really interested in S/MIME then I suggest that you take a
;; look at jl-smime.el in addition to this file.

;; Install:
;; Place this file into your load-path and add the following to ~/.emacs.
;;     (require 'jl-encrypt)
;; If you share my preferences, no further configuration should be necessary.
;; Configurable variables are `jl-gpg-without-mime',
;; `jl-encrypt-without-signature', and `jl-recipient-headers'.

;; Sanity check:
;; Without any further configuration, send a GnuPG encrypted e-mail to
;; yourself as follows.  Enter your own e-mail address after To, choose some
;; Subject, and enter `M-x spook' in the body, which will insert suitable
;; text.  Then press `C-c C-c' to send the e-mail as usual (forgetting to
;; encrypt).  If you own a GnuPG public for the To e-mail address then you
;; will be asked whether you really want to send that e-mail as plaintext.
;; Answering `no' will insert an MML secure tag for you.  Press `C-c C-c'
;; again, and an encrypted e-mail will be sent.  If you receive that e-mail
;; with garbled attachments read the comment for `jl-gpg-without-mime'.

;; You may want to check the subsequent comments to understand the rationale
;; of my changes to standard message behavior.

;; This file is *NOT* part of GNU Emacs.

;;; Code:
(require 'gnus)
(require 'message)

;; For pgp(mime) the following variable ensures that replies to encrypted
;; e-mails are encrypted (i.e., an MML tag is inserted into the message
;; buffer).
;; However, this does not work for S/MIME encrypted e-mails.  Once decrypted,
;; they loose their encrypted property in gnus-article-wash-types.  If you
;; are interested in S/MIME, you may want to consider jl-smime.el.
(setq gnus-message-replyencrypt t)

;; Rebind keys for sending messages to check whether encryption is possible
;; (all necessary public keys are available).
;; In the past, this could have prevented me from sending plaintext e-mails
;; that should have been sent encrypted.
(define-key message-mode-map (kbd "C-c C-c") 'jl-message-send-and-exit)
(define-key message-mode-map (kbd "C-c C-s") 'jl-message-send)

;; Make gnus insert MML encryption tags if keys for all recipients are
;; available.  Thus, if you reply (or wide reply) to a message or edit
;; a saved draft, then MML encryption tags will be inserted right away.
(add-hook 'gnus-message-setup-hook 'jl-encrypt-if-possible)

(defvar jl-gpg-without-mime nil
  "Control whether MML encryption should use MIME Security with OpenPGP.
RFC 3156 specifies how OpenPGP (and, thus, GnuPG) and MIME work together.
In Gnus, `mml-secure-message-encrypt-pgpmime' follows that standard.
An alternative is `mml-secure-message-encrypt-pgp', which represents a
less powerful approach.  If you (like me in the past) happen to send
e-mails in an environment using broken M$ SMTP servers, then your
beautiful e-mails produced by `mml-secure-message-encrypt-pgpmime',
following RFC 3156, will be corrupted along the way.  E.g., the SMTP
server at my department throws away the e-mail's Content-Type
`multipart/encrypted' and its `protocol=\"application/pgp-encrypted\"'
and inserts a meaningless `multipart/mixed' one.  Thus, the recipient
will have a hard time figuring out what the e-mail's strange attachments
are good for.  FUBAR.
If this variable is set to nil (the default) then your e-mails are built
according to RFC 3156.  I suggest that you send an encrypted e-mail to
yourself.  Complain to your IT department if you receive garbled
attachments.  Then set this variable to non-nil, while they are setting
up a reasonable SMTP server.")

(defvar jl-encrypt-without-signature t
  "Control whether MML encryption tags should also produce signatures.
Set to nil to produce an MML tag that signs in addition to encryption.
In general, I'm not interested in signing my e-mails.  In contrast, I
believe that off-the-record communication aims for the correct set of
security goals: Confidentiality with perfect forward secrecy, integrity,
and repudiability, see: http://www.cypherpunks.ca/otr/
Unfortunately, that's not an option for e-mail.")

(defvar jl-recipient-headers '("to" "cc")
  "List of headers that determine whose keys must be available.
An MML secure tag will only be inserted based on `jl-encrypt' if public keys
or certificates are available for all recipients mentioned in these headers.
Note that this list does not include From; in case of GnuPG, I recommend the
encrypt-to option in gpg.conf instead (which makes sure that you can decrypt
e-mails sent by yourself).
Also, be careful with Bcc headers; it *might* be possible to make this work
without giving away the Bcc'ed identities, but I did not test this and
recommend against such a thing: Only add Bcc if you are absolutely sure that
you know what you are doing.  And let me know how to do that properly ;)")

;; For the reason stated in the docstring of jl-encrypt-without-signature,
;; I'm not using:
;; (setq gnus-message-replysign t)

;; Moreover, I don't think that setting mml-default-encrypt-method makes much
;; sense.  The code in this file determines what method to use based on the
;; availability of public keys or certificates.  And on jl-gpg-without-mime.
;; Be sure to read its comment.

;;
;; No configuration options beyond this point.  Just code.
;;

(defun jl-message-send-and-exit (&optional arg)
  "Delegate work to `jl-message-send-maybe-exit', passing ARG."
  (interactive "p")
  (jl-message-send-maybe-exit t arg))

(defun jl-message-send (&optional arg)
  "Delegate work to `jl-message-send-maybe-exit', passing ARG."
  (interactive "p")
  (jl-message-send-maybe-exit nil arg))

(defun jl-message-send-maybe-exit (exit arg)
  "Send message if MML secure encrypt tag is present or not appropriate.
If MML secure encrypt tag is not present, check via
`jl-proceed-without-encryption-p' whether public keys for all
recipients are available and an MML secure tag should be added, or
whether the message should be sent without encryption.  In the latter
case EXIT controls whether `message-send-and-exit' or `message-send'
is called, and ARG is passed as argument."
  (save-excursion
    (goto-char (point-min))
    (if (or (re-search-forward "<#secure.+encrypt" nil t)
	    (jl-proceed-without-encryption-p))
	(if exit
	    (message-send-and-exit arg)
	  (message-send arg)))))

(defun jl-proceed-without-encryption-p ()
  "Return t if no (additional) encryption is necessary.
This happens if (a) the message cannot be encrypted because a key
for some recipient is missing, or (b) all keys are available but
the user explicitly answered `yes' to proceed without encryption.
Otherwise, i.e., all keys are available and the user answered
`no', an appropriate MML tag is inserted, and nil is returned."
  (interactive)
  (let ((recipients (jl-message-fetch-recipients)))
    (if (jl-test-list recipients 'jl-gpgkey-available-p)
	(if (yes-or-no-p "GnuPG public keys available for all recipients.  Really proceed *without* encryption? ")
	    t
	  (jl-secure-message-gpg)
	  nil)
      (if (jl-test-list recipients 'jl-certfile-available-p)
	  (if (yes-or-no-p "S/MIME certificates available for all recipients.  Reylly proceed *without* encryption? ")
	      t
	    (jl-secure-message-smime)
	    nil)
	t))))

(defun jl-message-fetch-recipients ()
  "Return list of current message's recipients.
Each list element represents one recipient (among those listed in the
headers `jl-recipient-headers') using the format of
`mail-extract-address-components'."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (let ((recipients
	     (mapconcat 'identity
			(remove nil (delete-dups
				     (mapcar 'message-fetch-field
					     jl-recipient-headers)))
			",")))
	  (mail-extract-address-components recipients t)))))

(defun jl-certfile-available-p (recipient)
  "Check whether certificate file is available for RECIPIENT.
This tests whether `smime-certificate-directory' contains a
certificate file whose name equals the e-mail address of
RECIPIENT (which is in the format of
`mail-extract-address-components').
Probably this test is only useful in combination with `jl-smime'."
  (file-exists-p (concat smime-certificate-directory (cadr recipient))))

(defun jl-gpgkey-available-p (recipient)
  "Check whether GnuPG knows a public key for the given RECIPIENT.
RECIPIENT must be in the format of `mail-extract-address-components'."
  (= 0 (call-process "gpg" nil nil nil "--list-key" (cadr recipient))))

(defun jl-test-list (list predicate)
  "To each element of LIST apply PREDICATE.
Return nil if some test returns nil; otherwise, return t."
  (let ((result (mapcar predicate list)))
    (if (memq nil result)
	nil
      t)))

(defun jl-secure-message-gpg ()
  "Invoke MML function to add appropriate secure tag for GnuPG.
The choice between pgp or pgpmime is based on `jl-gpg-without-mime'.
Creation of signatures is controlled by `jl-do-not-sign-p'."
  (if jl-gpg-without-mime
      (mml-secure-message-encrypt-pgp (jl-do-not-sign-p))
    (mml-secure-message-encrypt-pgpmime (jl-do-not-sign-p))))

(defun jl-secure-message-smime ()
  "Invoke MML function to add appropriate secure tag for S/MIME.
Creation of signatures is controlled by `jl-do-not-sign-p'."
  (mml-secure-message-encrypt-smime (jl-do-not-sign-p)))

(defun jl-is-signed-p ()
  "Check whether secure sign tag is present."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "<#secure.+sign>" nil t)))

(defun jl-do-not-sign-p ()
  "Check whether the message should not be signed.
This is the case if the `jl-encrypt-without-signature' is True and no secure
sign tag is present."
  (and jl-encrypt-without-signature
       (not (jl-is-signed-p))))

(defun jl-encrypt-if-possible ()
  "Insert MML encryption tag if appropriate.
If there is at least one recipient (not counting the From header)
and GnuPG public keys or S/MIME certificates are available for all
recipients in `jl-recipient-headers' then insert MML encryption tag."
  (let ((recipients (jl-message-fetch-recipients)))
    (if (> (length recipients)
	   (if (member-ignore-case "from" jl-recipient-headers)
	       1
	     0))
	(if (jl-test-list recipients 'jl-gpgkey-available-p)
	    (jl-secure-message-gpg)
	  (if (jl-test-list recipients 'jl-certfile-available-p)
	      (jl-secure-message-smime))))))

(provide 'jl-encrypt)
;;; jl-encrypt.el ends here
