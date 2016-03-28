;;; jl-encrypt.el --- Insert MML encryption tags if public keys are available
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2011, 2012, 2013, 2014, 2016 Jens Lechtenbörger
;; Copyright (C) 2013 konubinix

;; Changelog:
;; 2012/03/09, Version 1.1, initial release
;; 2013/03/18, Version 2.1, integrate patch by konubinix to also encrypt
;;   e-mail that already contains a secure sign tag:
;;   - Add functions jl-is-signed-p and jl-do-not-sign-p.
;;   - Change jl-secure-message-gpg and jl-secure-message-smime to use
;;     jl-do-not-sign-p (and not only jl-encrypt-without-signature).
;;   - Change jl-message-send-maybe-exit to look for secure encrypt tag
;;     (instead of any secure tag).
;;   - Add Changelog, give credit to konubinix
;; 2013/07/01, Version 3.1, support XEmacs, don't break news, don't replace
;;   S/MIME tag with GnuPG tag if possible.  New version started based on
;;   feedback and testing by Michael Strauss.
;;   - Replace delete-dups with mm-delete-duplicates as XEmacs does not have
;;     delete-dups.
;;   - Bug fixed when rebinding keys: Use (interactive "P") instead of
;;     (interactive "p").
;;   - Do not attempt to encrypt news postings, only go for e-mails with
;;     non-empty recipient lists.
;;   - If the user inserted an S/MIME signature tag, and S/MIME as well as
;;     GnuPG keys are available, don't prefer GnuPG any more but upgrade
;;     S/MIME signature to S/MIME sign+encrypt.
;;   - Refactoring.
;;     - Added jl-method-table, jl-access-method-table, jl-mail-recipients,
;;       jl-maybe-add-tag, jl-maybe-add-tag-for-args, jl-is-smime-p.
;;     - Based on above, simplified jl-proceed-without-encryption-p and
;;       jl-encrypt-if-possible.
;;     - Moved jl-certfile-available-p and jl-secure-message-smime to
;;       jl-smime.
;;   - Explain potential unsafety of Bcc headers.  Safety check added.
;;     Added jl-encrypt-safe-bcc-list.
;;   - Changed jl-gpgkey-available-p to produce less false positives.
;;   - Replaced jl-encrypt-without-signature with jl-encrypt-insert-signature.
;;     Have jl-encrypt-if-possible use it.  Bug fix in jl-is-signed-p.
;;   - Different treatment of From header.  See comment for new variable
;;     jl-encrypt-ignore-from.
;;   - Simplified internal recipient lists now contain just lower-case e-mail
;;     addresses (no names any longer).
;;   - Use defcustom instead of defvar for user variables, which are also
;;     renamed.
;; 2013/12/20, Version 4-beta, unified use of EasyPG for gpg and gpgsm.
;;   - Removed variables jl-encrypt-ignore-from and
;;     jl-encrypt-recipient-headers.
;;     Treatment of encryption targets now controlled entirely by EasyPG
;;     (and GnuPG configuration files).  New functions jl-epg-find-usable-keys
;;     and jl-epg-check-unique-keys towards that end.
;;   - New variable jl-encrypt-manual-choice.
;;     If several public keys are available for a recipient, ask user which
;;     one to use.  (Requires `mm-encrypt-option' introduced in Emacs 23.2.)
;; 2013/12/31, Version 4.1.
;;   - Take care of interface change for mml2015-epg-find-usable-key
;;     starting with Ma Gnus v0.7.
;;   - Updated documentation.
;; 2014/09/16, Version 4.2.
;;   - No change in functionality.
;;   - Use message-send-hook instead of key rebinding.
;;   - Replaced jl-encrypt-manual-choice with
;;     jl-encrypt-i-am-aware-of-mm-encrypt-option.
;;   - Renamed jl-mail-recipients to jl-message-recipients and
;;     jl-encrypt-address-list to jl-message-address-list.
;; 2016/03/28, Version 4.3.
;;   - No change in functionality.
;;   - Adapted for Emacs 25.1.

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

;; Keywords: mail, encryption, GnuPG, gpg, OpenPGP, gpgsm, S/MIME, CMS

;; URL: http://www.emacswiki.org/emacs/jl-encrypt.el
;; EmacsWiki: DefaultEncrypt

;; A signed version of this file is available over there:
;; http://www.informationelle-selbstbestimmung-im-internet.de/emacs/

;;; Commentary:
;; Gnus supports GnuPG via the insertion of so-called MML secure tags, which
;; contain encryption instructions to be performed before a message is sent.
;; However, by default those tags need to be added manually, which can easily
;; be forgotten.  In contrast, DefaultEncrypt aims to insert those tags
;; automatically if public keys for all recipients are available.
;;
;; I assume that you are familiar with GnuPG support for Gnus messages as
;; described in Info node `(message) Security':
;; https://www.gnu.org/software/emacs/manual/html_node/message/Security.html
;; Moreover, DefaultEncrypt is based on EasyPG, the default interface of
;; Emacs for OpenPGP and S/MIME, see Info node `(epa)':
;; https://www.gnu.org/software/emacs/manual/html_node/epa/index.html
;;
;; DefaultEncrypt aims for automatic insertion of an MML secure encryption
;; tags into messages if public keys (either GnuPG public keys or S/MIME
;; certificates) for all recipients are available.  In addition, before a
;; message is sent, the user is asked whether plaintext should really be sent
;; unencryptedly when public keys for all recipients are available.
;;
;; The above works by adding
;; `mml-secure-encrypt-if-possible' to `gnus-message-setup-hook' and
;; `mml-secure-check-encryption-p' to `message-send-hook'.
;;
;; Note that with EasyPG, e-mail is *not* encrypted to (a key for) the
;; From address (see Info node `(epa) Mail-mode integration').
;; Instead, to make sure that you can decrypt your own e-mails, typical
;; options are
;; * to customize mml-secure-openpgp-encrypt-to-self or
;; * to use a Bcc header to encrypt the e-mail to one of your own
;;   GnuPG identities (see `mml-secure-safe-bcc-list' then) or
;; * to use the encrypt-to or hidden-encrypt-to option in gpg.conf or
;; * to use the encrypt-to option in gpgsm.conf.
;; Note that encrypt-to gives away your identity for *every*
;; encryption without warning, which is not what you want if you are
;; using, e.g., remailers.
;; Also, be careful with Bcc headers; it *might* be possible to make
;; this work without giving away the Bcc'ed identities, but it did
;; not work by default in my case, and I recommend against such a
;; thing: Only add Bcc if you are absolutely sure that you know what
;; you are doing.  See also `mml-secure-bcc-is-safe'.
;;
;; If you are interested in S/MIME then I suggest that you read this:
;; https://blogs.fsfe.org/jens.lechtenboerger/2013/12/23/openpgp-and-smime/
;; If you are still interested in S/MIME afterwards, take a look
;; at ExtendSMIME (jl-smime.el) in addition to this file.

;; Install:
;; Place this file into your load-path and add the following to ~/.emacs.
;;     (require 'jl-encrypt)
;; If you share my preferences, no further configuration should be necessary.
;;
;; Customizable variables defined in this file are
;; `mml-secure-insert-signature', `mml-secure-pgp-without-mime'.
;;
;; In addition, as explained in the following `mml-default-encrypt-method' and
;; `mml-default-sign-method' influence whether to prefer S/MIME or GnuPG in
;; certain situations, and the value of `gnus-message-replysignencrypted' may
;; be changed by this code.
;; If `mml-secure-insert-signature' is nil (the default),
;; `gnus-message-replysignencrypted' will be set to nil to avoid signatures
;; for reply messages.  You may manually add MML sign tags any time, of
;; course, but the whole point of this file is to create MML tags automatically
;; (e.g., by customizing `mml-secure-insert-signature').
;; If GnuPG and S/MIME keys are both available for the given recipients and no
;; MML tag is present, `mml-default-encrypt-method' determines what method to
;; prefer.  If `mml-secure-insert-signature' is set to `always',
;; `mml-default-sign-method' determines what method to prefer upon message
;; creation.

;; Sanity check:
;; Without any further configuration, send a GnuPG encrypted e-mail to
;; yourself as follows.  Enter your own e-mail address after To, choose some
;; Subject, and enter `M-x spook' in the body, which will insert suitable
;; text.  Then press `C-c C-c' to send the e-mail as usual (forgetting to
;; encrypt).  If you own a GnuPG public for the To e-mail address then you
;; will be asked whether you really want to send that e-mail as plaintext.
;; Answering `no' will insert an MML secure tag for you.  Press `C-c C-c'
;; again, and an encrypted e-mail will be sent.  If you receive that e-mail
;; with garbled attachments read the comment for
;; `mml-secure-pgp-without-mime'.

;; You may want to check the subsequent comments to understand the rationale
;; of my changes to standard message behavior.

;; This file is *NOT* part of GNU Emacs.

;;; Code:
(require 'gnus)
(require 'message)
(require 'mml-sec)
(require 'epg)

;; Make gnus insert MML encryption tags if keys for all recipients are
;; available.  Thus, if you reply (or wide reply) to a message or edit
;; a saved draft, then MML encryption tags will be inserted right away.
;; Moreover, if mml-secure-insert-signature is always, an MML signature
;; tag will be added immediately.
(add-hook 'gnus-message-setup-hook 'mml-secure-encrypt-if-possible)
(add-hook 'message-send-hook 'mml-secure-check-encryption-p)

(defgroup jl-encrypt nil
  "Customization options for jl-encrypt.el.")

(defcustom mml-secure-insert-signature nil
  "Control whether MML signature tags should be produced.
If it is nil, no signatures are created.  If it is `always', signature
tags are inserted for new messages (with their type determined by
`mml-default-sign-method').  If is is `encrypted', messages that are
encrypted are also signed."
  :group 'jl-encrypt
  :type '(choice (const nil) (const always) (const encrypted)))

;; Note that mml-default-encrypt-method combines two separate concepts: First,
;; whether to use OpenPGP or S/MIME.  Second, if OpenPGP is chosen whether to
;; use MIME Security or not.  For me, this is not good enough.  Suppose I
;; preferred S/MIME in general (which I do not).  Then, I would need to
;; customize mml-default-encrypt-method for S/MIME.  If I then send an e-mail
;; to someone who does not use S/MIME but OpenPGP, the variable
;; mml-default-encrypt-method does not help at all, and there is no way to
;; know whether MIME Security should be used or not.  Thus, I introduce an
;; additional variable that focuses on MIME Security for OpenPGP:
(defcustom mml-secure-pgp-without-mime nil
  "Non-nil to use MML encryption without MIME Security for OpenPGP.
RFC 3156 specifies how OpenPGP (and, thus, GnuPG) and MIME work
together.  In Gnus, `mml-secure-message-encrypt-pgpmime' follows
that standard.  An alternative is
`mml-secure-message-encrypt-pgp', which represents a less
powerful approach.  If you (like me in the past) happen to send
e-mails in an environment using broken M$ SMTP servers, then your
beautiful e-mails produced by
`mml-secure-message-encrypt-pgpmime', following RFC 3156, will be
corrupted along the way.  E.g., the SMTP server at my department
throws away the e-mail's Content-Type \"multipart/encrypted\" and
its \"protocol=application/pgp-encrypted\" and inserts a
meaningless \"multipart/mixed\" one.  Thus, the recipient will
have a hard time figuring out what the e-mail's strange
attachments are good for.  FUBAR.
If this variable is set to nil (the default) then your e-mails
are built according to RFC 3156.  I suggest that you send an
encrypted e-mail to yourself.  Complain to your IT department if
you receive garbled attachments.  Then set this variable to
non-nil, while they are setting up a reasonable SMTP server.
If you are signing all your e-mails with GnuPG you probably also
want to set `mml-default-sign-method' to \"pgp\" (instead of
\"pgpmime\")."
  :group 'jl-encrypt
  :type 'boolean)

;;
;; No configuration options beyond this point.  Just code.
;;

;; Message header functions.
;; Could go into message.el

(defun jl-message-address-list (header)
  "Return e-mail addresses of HEADER as list."
  (let ((hdr (mail-strip-quoted-names (message-fetch-field header))))
    (when hdr
      ;; Split recipients at "," boundary, omit empty strings (t),
      ;; and strip whitespace.
      (split-string hdr "," t "\\s-+"))))

(defun jl-message-recipients ()
  "Return recipients of current message or nil.
Recipients are only returned if the message is an e-mail with at
least one recipient."
  (if (message-mail-p)
      (let ((recipients
	     (delete-dups
	      ;; Split recipients at "," boundary, omit empty strings (t),
	      ;; and strip whitespace.
	      (split-string (message-options-set-recipient)
			    "," t "\\s-+"))))
	(if (>= (length recipients) 1)
	    recipients
	  nil))
    nil))


;; epa functions.
;; Could go into mml-sec.el or a new file mml-epg.el.

(defun mml-epg-gpgkey-available-p (recipient)
  "Check whether EasyPG knows a public OpenPGP key for RECIPIENT."
  (mml-secure-find-usable-keys
   (epg-make-context 'OpenPGP) recipient 'encrypt t))


;; MML functions.
;; Could go into mml-sec.el

(defun mml-secure-message-gpg ()
  "Invoke MML function to add appropriate secure tag for GnuPG.
The choice between pgp or pgpmime is based on `mml-secure-pgp-without-mime'.
Creation of signatures is controlled by `mml-secure-do-not-sign-p'."
  (if mml-secure-pgp-without-mime
      (mml-secure-message-encrypt-pgp (mml-secure-do-not-sign-p))
    (mml-secure-message-encrypt-pgpmime (mml-secure-do-not-sign-p))))

(defun mml-secure-is-secure-p (string)
  "Check whether secure tag containing STRING is present."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"
	     "<#secure[^>]+" string)
     nil t)))

(defun mml-secure-is-encrypted-p ()
  "Check whether secure encrypt tag is present."
  (mml-secure-is-secure-p "encrypt"))

(defun mml-secure-is-smime-p ()
  "Check whether secure tag for S/MIME is present."
  (mml-secure-is-secure-p "method=smime"))

(defun mml-secure-is-signed-p ()
  "Check whether secure sign tag is present."
  (mml-secure-is-secure-p "sign"))

(defun mml-secure-do-not-sign-p ()
  "Check whether the message should not be signed.
This is the case if `mml-secure-insert-signature' is nil and
no secure sign tag is present."
  (and (not mml-secure-insert-signature)
       (not (mml-secure-is-signed-p))))

(defun mml-secure-encrypt-if-possible ()
  "Insert MML security tag if appropriate.
This function may insert MML tags for signing and/or encryption.
Creation of sign tags is controlled by `mml-secure-insert-signature'.  If
that variable is `always', a sign tag for method `mml-default-sign-method'
is created.
Moreover, the creation of an encrypt tag is determined based on the existence
of public keys for all recipients of the current message, see
`mml-secure-maybe-add-tag'.  If an encrypt tag is added, the message will
additionally be signed if `mml-secure-insert-signature' is `encrypted'."
  (if (not mml-secure-insert-signature)
      (setq gnus-message-replysignencrypted nil)
    (if (eq 'always mml-secure-insert-signature)
	(mml-secure-message mml-default-sign-method 'sign)))
  (mml-secure-maybe-add-tag t))

(defun mml-secure-maybe-add-tag-for-args (recipients method &optional dontask)
  "Maybe add MML secure tag for RECIPIENTS and METHOD.
If keys are available for all RECIPIENTS and METHOD and DONTASK is
nil, ask whether no encryption should be performed.  If the user
answers \"yes\",don't add an MML tag and return `yes'; if the user
answers \"no\", insert tag and return `no'.
Otherwise, if DONTASK is t, insert tag and return 'inserted.
Otherwise, return `failed'."
  (if (gnus-test-list recipients
		      (mml-secure-access-method-table method "test"))
      (if (not dontask)
	  (if (yes-or-no-p (mml-secure-access-method-table method "ask"))
	      'yes
	    (funcall (mml-secure-access-method-table method "doit"))
	    'no)
	(funcall (mml-secure-access-method-table method "doit"))
	(message "Inserted %s tag" method)
	'inserted)
    (message "Failed to insert %s tag" method)
    'failed))

(defun mml-secure-maybe-add-tag (&optional dontask)
  "Maybe add MML secure encryption tag to current message.
If no recipients are returned by `jl-message-recipients',
immediately return `failed'.  Otherwise, try to add a tag for
those recipients.  If jl-smime.el has been loaded, S/MIME tags
may be inserted; otherwise only the default of GnuPG tags.
Which method is tried first if both are available depends on the
current message and the value of `mml-default-encrypt-method': If
`mml-default-encrypt-method' indicates S/MIME or if the message
carries an S/MIME signature tag, go for S/MIME first; otherwise
for GnuPG.  Call `mml-secure-maybe-add-tag-for-args' with recipients,
chosen method, and DONTASK.
If that call does not return `failed', return this result.
Otherwise, re-try the call with the second method and return its
result."
  (let ((recipients (jl-message-recipients)))
    (if recipients
	(let* ((smime-supported (assq 'CMS mml-secure-method-table))
	       (is-smime (and smime-supported
			      (or (string= "smime" mml-default-encrypt-method)
				  (mml-secure-is-smime-p))))
	       (first-method (if is-smime 'CMS 'OpenPGP))
	       (second-method (if is-smime 'OpenPGP
				(if smime-supported 'CMS)))
	       (first-result (mml-secure-maybe-add-tag-for-args
			      recipients first-method dontask)))
	  (if (and (eq 'failed first-result) second-method)
	      (mml-secure-maybe-add-tag-for-args
	       recipients second-method dontask)
	    first-result))
      'failed)))

(defun mml-secure-proceed-without-encryption-p (&optional dontask)
  "Return t if no (additional) encryption is necessary.
This happens if `mml-secure-maybe-add-tag' called with DONTASK does
not return 'no.  Otherwise, raise an error."
  (if (not (eq 'no (mml-secure-maybe-add-tag dontask)))
      t
    (error "Encryption is a Good Thing.")))

(defun mml-secure-check-encryption-p ()
  "Check whether MML secure encrypt tag is present or not appropriate.
If MML secure encrypt tag is not present, check via
`mml-secure-proceed-without-encryption-p' whether public keys for all
recipients are available and an MML secure tag should be added, or
whether the message should be sent without encryption.  In the latter
case return t.  Otherwise, raise an error."
  (save-excursion
    (or (mml-secure-is-encrypted-p)
	(mml-secure-proceed-without-encryption-p))))

(defvar mml-secure-method-table '((OpenPGP
				   (("test" mml-epg-gpgkey-available-p)
				    ("doit" mml-secure-message-gpg)
				    ("ask" "GnuPG public keys available \
for all recipients.  Really proceed *without* encryption? "))))
  "Internal functionality for supported security methods.")

(defun mml-secure-access-method-table (method what)
  "Return object for METHOD representing WHAT."
  (let ((method-spec (assq method mml-secure-method-table)))
    (if method-spec
	(cadr (assoc what (cadr method-spec)))
      (if (eq method 'CMS)
	  (error "You must load jl-smime for S/MIME support")
	(error
	 "Method `%s' not supported by `mml-secure-method-table'" method)))))

(provide 'jl-encrypt)
;;; jl-encrypt.el ends here
