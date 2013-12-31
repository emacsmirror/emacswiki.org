;;; jl-encrypt.el --- Insert MML encryption tags if public keys are available
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: utf-8 -*-

;; Copyright (C) 2011, 2012, 2013 Jens Lechtenbörger
;; Copyright (C) 2013 konubinix

;; Version: $Id: jl-encrypt.el,v 1.23 2013/12/31 12:02:22 lechten Exp $
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
;; 2013/12/31, Version 4.1,
;;   - Take care of interface change for mml2015-epg-find-usable-key
;;     starting with Ma Gnus v0.7.
;;   - Updated documentation.

;; Compatibility: Developed on GNU Emacs 24.3; may work with GNU Emacs 23.2
;; and later

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
;; Moreover, DefaultEncrypt is based on EasyPG, the default interface of Emacs
;; for OpenPGP and S/MIME, see Info node `(epa)':
;; https://www.gnu.org/software/emacs/manual/html_node/epa/index.html
;;
;; DefaultEncrypt aims for automatic insertion of an MML secure encryption
;; tags into messages if public keys (either GnuPG public keys or S/MIME
;; certificates) for all recipients are available.  In addition, before a
;; message is sent, the user is asked whether plaintext should really be sent
;; unencryptedly when public keys for all recipients are available.
;;
;; Moreover, a consistency check aims to avoid unsafe usage of Bcc headers.
;; (Some details are given below.)
;;
;; Finally, another consistency check tests whether multiple public keys are
;; available for some recipient.  If so, EasyPG would select one of those
;; keys, which may or may not be a desirable one.  In this case,
;; DefaultEncrypt sets `mm-encrypt-option' to 'multiple (available since Emacs
;; 23.2), which causes EasyPG to display (mostly) relevant keys in a separate
;; buffer.  In that buffer, you need to select keys for *all* recipients (not
;; just those with multiple public keys).  Relevant key bindings in that
;; buffer are: m (select a key), <mouse-2> (display key info), C-c C-c
;; (OK, after you selected keys for all recipients)
;; See also the documentation for `jl-encrypt-manual-choice'.
;;
;; The above works by rebinding the standard keys for message sending
;; (`C-c C-c' and `C-c C-s') as well as by adding the check
;; `jl-encrypt-if-possible' to `gnus-message-setup-hook'.
;;
;; Note that with EasyPG, e-mail is *not* encrypted to (a key for) the
;; From address (see Info node `(epa) Mail-mode integration').
;; Instead, to make sure that you can decrypt your own e-mails, typical
;; options are
;; * to use a Bcc header to encrypt the e-mail to one of your own
;;   GnuPG identities (see `jl-encrypt-safe-bcc-list' then) or
;; * to use the encrypt-to or hidden-encrypt-to option in gpg.conf or
;; * to use the encrypt-to option in gpgsm.conf.
;; Note that encrypt-to gives away your identity for *every*
;; encryption without warning, which is not what you want if you are
;; using, e.g., remailers.
;; Also, be careful with Bcc headers; it *might* be possible to make
;; this work without giving away the Bcc'ed identities, but it did
;; not work by default in my case, and I recommend against such a
;; thing: Only add Bcc if you are absolutely sure that you know what
;; you are doing.  And let me know how to do that properly ;)
;; See also `jl-encrypt-bcc-is-safe'.
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
;; `jl-encrypt-insert-signature', `jl-encrypt-gpg-without-mime',
;; `jl-encrypt-safe-bcc-list', and `jl-encrypt-manual-choice'.
;;
;; In addition, as explained in the following `mml-default-encrypt-method' and
;; `mml-default-sign-method' influence whether to prefer S/MIME or GnuPG in
;; certain situations, and the value of `gnus-message-replysignencrypted' may
;; be changed by this code.
;; If `jl-encrypt-insert-signature' is nil (the default),
;; `gnus-message-replysignencrypted' will be set to nil to avoid signatures
;; for reply messages.  You may manually add MML sign tags any time, of
;; course, but the whole point of this file is to create MML tags automatically
;; (e.g., by customizing `jl-encrypt-insert-signature').
;; If GnuPG and S/MIME keys are both available for the given recipients and no
;; MML tag is present, `mml-default-encrypt-method' determines what method to
;; prefer.  If `jl-encrypt-insert-signature' is set to `always',
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
;; `jl-encrypt-gpg-without-mime'.

;; You may want to check the subsequent comments to understand the rationale
;; of my changes to standard message behavior.

;; This file is *NOT* part of GNU Emacs.

;;; Code:
(require 'gnus)
(require 'message)
(require 'mml2015)
(require 'epg)
(require 'mm-encode) ; mm-encrypt-option

;; XEmacs does not have delete-dups.  mm-util provides mm-delete-duplicates.
(require 'mm-util)

;; Rebind keys for sending messages to check whether encryption is possible
;; (all necessary public keys are available).
;; In the past, this could have prevented me from sending plaintext e-mails
;; that should have been sent encrypted.
(define-key message-mode-map (kbd "C-c C-c") 'jl-message-send-and-exit)
(define-key message-mode-map (kbd "C-c C-s") 'jl-message-send)

;; Make gnus insert MML encryption tags if keys for all recipients are
;; available.  Thus, if you reply (or wide reply) to a message or edit
;; a saved draft, then MML encryption tags will be inserted right away.
;; Moreover, if jl-encrypt-insert-signature is always, an MML signature
;; tag will be added immediately.
(add-hook 'gnus-message-setup-hook 'jl-encrypt-if-possible)

(defgroup jl-encrypt nil
  "Customization options for jl-encrypt.el.")

(defcustom jl-encrypt-manual-choice t
  "Non-nil if user should choose in case of multiple public keys.
In general, multiple public keys may exist for a single e-mail address (e.g.,
for different security levels or before one key expires).  In such a case,
EasyPG and gpg automatically select a key, while gpgsm reports an ambiguity.
By default, DefaultEncrypt switches to manual key selection in such cases.
Set to nil for automatic selection by EasyPG."
  :group 'jl-encrypt
  :type 'boolean)

(defcustom jl-encrypt-insert-signature nil
  "Control whether MML signature tags should be produced.
If it is nil, no signatures are created.  If it is `always', signature
tags are inserted for new messages (with their type determined by
`mml-default-sign-method').  If is is `encrypted', messages that are
encrypted are also signed."
  :group 'jl-encrypt
  :type '(choice (const nil) (const always) (const encrypted)))

(defcustom jl-encrypt-gpg-without-mime nil
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

(defcustom jl-encrypt-safe-bcc-list nil
  "List of e-mail addresses that are safe to use in Bcc headers.
EasyPG encrypts e-mails to Bcc addresses, and the encrypted e-mail
by default identifies the used encryption keys, giving away the
Bcc'ed identities.  Clearly, this contradicts the original goal of
*blind* copies.
For an academic paper explaining the problem, see URL
`http://crypto.stanford.edu/portia/papers/bb-bcc.pdf'.
Use this variable to specify e-mail addresses whose owners do not
mind if they are identifiable as recipients.  This may be useful if
you use Bcc headers to encrypt e-mails to yourself."
  :group 'jl-encrypt
  :type '(repeat string))

;;
;; No configuration options beyond this point.  Just code.
;;

(defvar jl-method-table '((OpenPGP (("test" jl-gpgkey-available-p)
				    ("doit" jl-secure-message-gpg)
				    ("ask" "GnuPG public keys available \
for all recipients.  Really proceed *without* encryption? "))))
  "Internal functionality for supported security methods.")

(defun jl-access-method-table (method what)
  "Return object for METHOD representing WHAT."
  (let ((method-spec (assq method jl-method-table)))
    (if method-spec
	(cadr (assoc what (cadr method-spec)))
      (if (eq method 'CMS)
	  (error "You must load jl-smime for S/MIME support")
	(error "Method `%s' not supported by `jl-method-table'" method)))))

(defun jl-message-send-and-exit (&optional arg)
  "Delegate work to `jl-message-send-maybe-exit', passing ARG."
  (interactive "P")
  (jl-message-send-maybe-exit t arg))

(defun jl-message-send (&optional arg)
  "Delegate work to `jl-message-send-maybe-exit', passing ARG."
  (interactive "P")
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
    (when (or (re-search-forward "<#secure[^>]+encrypt" nil t)
	      (jl-proceed-without-encryption-p))
      ;; jl-proceed-without-encryption-p may insert an encrypt tag.
      ;; So re-check for encryption.
      (goto-char (point-min))
      (let ((encrypted (re-search-forward "<#secure[^>]+encrypt" nil t)))
	(jl-encrypt-bcc-is-safe encrypted)
	(let ((mm-encrypt-option
	       (if (and jl-encrypt-manual-choice
			(eq 'multiple
			    (jl-epg-check-unique-keys (jl-mail-recipients))))
		   'guided
		 ;; Otherwise, try to preserve mm-encrypt-option (without
		 ;; error for versions where that variable does not exist).
		 (if (boundp 'mm-encrypt-option)
		     mm-encrypt-option
		   nil))))
	  (if exit
	      (message-send-and-exit arg)
	    (message-send arg)))))))

(defun jl-encrypt-address-list (header)
  "Return e-mail addresses of HEADER as list."
  (let ((hdr (mail-strip-quoted-names (message-fetch-field header))))
    (when hdr
      (split-string hdr ", "))))

(defun jl-encrypt-bcc-is-safe (encrypted)
  "Check whether usage of Bcc is safe (or absent).
Bcc usage is safe in two cases: first, if ENCRYPTED is nil;
second, if the Bcc addresses are a subset of `jl-encrypt-safe-bcc-list'.
In all other cases, ask the user whether Bcc usage is safe.
Raise error if user answers no.
Note that this function does not produce a meaningful return value:
either an error is raised or not."
  (if encrypted
      (let ((bcc-list (jl-encrypt-address-list "bcc")))
	(unless (jl-subsetp bcc-list jl-encrypt-safe-bcc-list)
	  (unless (yes-or-no-p "Message for encryption contains Bcc header.\
  This may give away all Bcc'ed identities to all recipients.\
  Are you sure that this is safe?\
  (Customize `jl-encrypt-safe-bcc-list' to avoid this warning.) ")
	    (error "Aborted"))))))

(defun jl-mail-recipients ()
  "Return recipients of current message or nil.
Recipients are only returned if the message is an e-mail with at
least one recipient."
  (if (message-mail-p)
      (let ((recipients
	     (mm-delete-duplicates
	      (split-string (message-options-set-recipient) ", "))))
	(if (>= (length recipients) 1)
	    recipients
	  nil))
    nil))

(defun jl-maybe-add-tag-for-args (recipients method &optional dontask)
  "Maybe add MML secure tag for RECIPIENTS and METHOD.
If keys are available for all RECIPIENTS and METHOD and DONTASK is
nil, ask whether no encryption should be performed.  If the user
answers \"yes\",don't add an MML tag and return `yes'; if the user
answers \"no\", insert tag and return `no'.
Otherwise, if DONTASK is t, insert tag and return 'inserted.
Otherwise, return `failed'."
  (if (jl-test-list recipients (jl-access-method-table method "test"))
      (if (not dontask)
	  (if (yes-or-no-p (jl-access-method-table method "ask"))
	      'yes
	    (funcall (jl-access-method-table method "doit"))
	    'no)
	(funcall (jl-access-method-table method "doit"))
	(message "Inserted %s tag" method)
	'inserted)
    (message "Failed to insert %s tag" method)
    'failed))

(defun jl-maybe-add-tag (&optional dontask)
  "Maybe add MML secure encryption tag to current message.
If no recipients are returned by `jl-mail-recipients',
immediately return `failed'.  Otherwise, try to add a tag for
those recipients.  If jl-smime.el has been loaded, S/MIME tags
may be inserted in addition to the default of GnuPG tags.  Which
method is tried first if both are available, depends on the
current message and the value of `mml-default-encrypt-method': If
`mml-default-encrypt-method' indicates smime or if the message
carries an S/MIME signature tag, go for S/MIME first; otherwise
for GnuPG.  Call `jl-maybe-add-tag-for-args' with recipients,
chosen method, and DONTASK.
If that call does not return `failed', return this result.
Otherwise, re-try the call with the second method and return its
result."
  (let ((recipients (jl-mail-recipients)))
    (if recipients
	(let* ((smime-supported (assq 'CMS jl-method-table))
	       (is-smime (and smime-supported
			      (or (string= "smime" mml-default-encrypt-method)
				  (jl-is-smime-p))))
	       (first-method (if is-smime 'CMS 'OpenPGP))
	       (second-method (if is-smime 'OpenPGP
				(if smime-supported 'CMS)))
	       (first-result (jl-maybe-add-tag-for-args
			      recipients first-method dontask)))
	  (if (and (eq 'failed first-result) second-method)
	      (jl-maybe-add-tag-for-args recipients second-method dontask)
	    first-result))
      'failed)))

(defun jl-proceed-without-encryption-p (&optional dontask)
  "Return t if no (additional) encryption is necessary.
This happens if `jl-maybe-add-tag' called with DONTASK does not
return 'no."
  (let ((tag-added (jl-maybe-add-tag dontask)))
    (not (eq 'no tag-added))))

(defun jl-epg-find-usable-keys (recipient)
  "Find usable encryption keys for e-mail address RECIPIENT.
Variable `epa-protocol' determines the type of keys."
  (let ((candidates (epg-list-keys (epg-make-context epa-protocol) recipient))
	keys)
    (dolist (candidate candidates keys)
      ;; Beware: There are lots of similar functions to check candidate, e.g.,
      ;; mml2015-epg-find-usable-key, mml1991-epg-find-usable-key,
      ;; mml-smime-epg-find-usable-key.
      ;; With Gnus v5.13, the first two are exact copies; they exclude
      ;; disabled, revoked, and expired keys. The third one does not test
      ;; for disabled keys; maybe S/MIME keys cannot be disabled.
      ;; Hence, it probably does not hurt to apply mml2015-epg-find-usable-key
      ;; in all cases.
      ;; With Ma Gnus v0.7, mml2015-epg-find-usable-key is rewritten with an
      ;; incompatible list of arguments (but mml1991-epg-find-usable-key
      ;; remains unchanged) to use the new function mml2015-epg-check-sub-key.
      (if (or (and (fboundp 'mml2015-epg-check-sub-key)
		   (mml2015-epg-check-sub-key candidate 'encrypt))
	      (and (not (fboundp 'mml2015-epg-check-sub-key))
		   (mml2015-epg-find-usable-key (list candidate) 'encrypt)))
	  (push candidate keys)))))

(defun jl-epg-check-unique-keys (recipients)
  "Check if all RECIPIENTS have unique encryption keys.
Return 'ok if a unique key is found for every recpient.  Return nil, if no key
is found for some recipient; otherwise, return 'multiple."
  (let ((epa-protocol (if (jl-is-smime-p)
			  'CMS
			'OpenPGP)))
    (catch 'break
      (dolist (recipient recipients 'ok)
	(let* ((candidates (jl-epg-find-usable-keys recipient))
	       (candno (length candidates)))
	  (unless (= 1 candno)
	    (if (= 0 candno)
		(progn (message "No key for %s" recipient)
		       (throw 'break nil))
	      (message "Multiple keys for %s" recipient)
	      (sit-for 2)
	      (throw 'break 'multiple))))))))

(defun jl-gpgkey-available-p (recipient)
  "Check whether EasyPG knows a public OpenPGP key for RECIPIENT."
  (let ((epa-protocol 'OpenPGP))
    (jl-epg-find-usable-keys recipient)))

(defun jl-secure-message-gpg ()
  "Invoke MML function to add appropriate secure tag for GnuPG.
The choice between pgp or pgpmime is based on `jl-encrypt-gpg-without-mime'.
Creation of signatures is controlled by `jl-do-not-sign-p'."
  (if jl-encrypt-gpg-without-mime
      (mml-secure-message-encrypt-pgp (jl-do-not-sign-p))
    (mml-secure-message-encrypt-pgpmime (jl-do-not-sign-p))))

(defun jl-is-smime-p ()
  "Check whether secure tag for S/MIME is present."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "<#secure[^>]+method=smime" nil t)))

(defun jl-is-signed-p ()
  "Check whether secure sign tag is present."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "<#secure[^>]+sign" nil t)))

(defun jl-do-not-sign-p ()
  "Check whether the message should not be signed.
This is the case if `jl-encrypt-insert-signature' is nil and
no secure sign tag is present."
  (and (not jl-encrypt-insert-signature)
       (not (jl-is-signed-p))))

(defun jl-encrypt-if-possible ()
  "Insert MML encryption tag if appropriate.
Function used in `gnus-message-setup-hook'.  Set
`gnus-message-replysignencrypted' to nil if `jl-encrypt-insert-signature'
is nil.  Insert MML signature tag if `jl-encrypt-insert-signature' is
`always'.  Call `jl-maybe-add-tag' with t to avoid being asked a question."
  (if (not jl-encrypt-insert-signature)
      (setq gnus-message-replysignencrypted nil)
    (if (eq 'always jl-encrypt-insert-signature)
	(mml-secure-message mml-default-sign-method 'sign)))
  (jl-maybe-add-tag t))

(defun jl-test-list (list predicate)
  "To each element of LIST apply PREDICATE.
Return nil if list is empty or some test returns nil;
otherwise, return t."
  (if list
      (let ((result (mapcar predicate list)))
	(if (memq nil result)
	    nil
	  t))))

(defun jl-subsetp (list1 list2)
  "Return t if LIST1 is a subset of LIST2.
Similar to `subsetp' but use member for element test so that this works for
lists of strings."
  (if list1
      (and (member (car list1) list2)
	   (jl-subsetp (cdr list1) list2))
    t))

(provide 'jl-encrypt)
;;; jl-encrypt.el ends here
