;;; mon-doc-help-mail.el --- extends mon-doc-help-utils package with mail xrefs
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-doc-help-mail.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-08-10T13:14:49-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: docs, help, mail, installation, environment, external, 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-doc-help-mail provides extensions to mon-doc-help-utils package with help
;; for Emacs mail related features, functions, variables, documentation xrefs,
;; file pathnames, info-nodes, etc. 
;;
;; FUNCTIONS:►►►
;; `mon-help-mail-functons'
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-doc-help-mail.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-doc-help-mail. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-08-10T13:14:49-04:00Z}#{10322} - by MON KEY>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-08-10T14:22:56-04:00Z}#{10322} - by MON KEY>
(defun mon-help-mail-functons (&optional insertp intrp)
  "Email related functions, variables, pathnames, info-nodes, groups, etc.\n

;; :MAIL-VARIABLES-ENVIRONMENT
EMAIL
GPG_AGENT_INFO
MAILCAPS
SMTPSERVER

;; :MAIL-VARIABLES-IDENTIFICATION       ; :NOTE Each of these is `defcustom'
`user-mail-address'
`mail-host-address'
`user-full-name'

;; :MAIL-FUNCTIONS-SIMPLE                  :SEE :FILE lisp/simple.el
`compose-mail'
`compose-mail-other-window'
`compose-mail-other-frame'
`define-mail-user-agent'                   :SEE :FILE lisp/subr.el

;; :MAIL-VARIABLES-AGENTS-SIMPLE           :SEE :FILE lisp/simple.el
`mail-user-agent' .                       
                  | `message-user-agent'
		  | `sendmail-user-agent'
		  | `mh-e-user-agent'
		  | `gnus-user-agent'

`read-mail-command' .
		    | `rmail'
		    | `gnus'
		    | `mh-rmail'

`message-user-agent'
`sendmail-user-agent'
`sendmail-user-agent-compose'
`message-send-mail-function'
`compose-mail-check-user-agent'

;; :MAIL-FUNCTIONS-SENDMAIL                 :SEE lisp/mail/sendmail.el
`mail'
`mail-mode'
`mail-send'
`mail-send-and-exit'
`mail-recover'

;; :MAIL-FUNCTIONS-SENDMAIL-CONTENT-FIELDS
`mail-attach-file'
`mail-bcc'
`mail-cc'
`mail-fcc'
`mail-mail-reply-to'
`mail-mail-followup-to'
`mail-recover'
`mail-reply-to'
`mail-signature'
`mail-to'
`mail-subject'

;; :MAIL-FUNCTIONS-SENDMAIL-DISPLAY
`mail-other-frame'
`mail-other-window'

;; :MAIL-FUNCTIONS-SENDMAIL-CONTENT-BODY
`mail-abbrevs-mode'
`mail-split-line'
`mail-text'
`mail-fill-yanked-message'
`mail-decode-encoded-word-region'
`mail-encode-encoded-word-buffer'
`mail-unquote-printable-region'
`mail-yank-original'
`mail-yank-region'
`mail-quote-printable-region'

;; :MAIL-VARIABLES-SENDMAIL
`mail-default-reply-to' 
`mail-mailing-lists'
`mail-mode-hook'
`mail-send-hook' 
`mail-setup-hook'
`mail-yank-hooks'
`mail-archive-file-name'
`mail-default-reply-to'
`mail-default-directory'
`mail-mailing-lists'
`mail-self-blind'
`message-auto-save-directory'
`mail-signature-file'
`mail-signature'
`mail-send-hook'               
`mail-citation-hook'

;; :MAIL-FUNCTIONS-MAIL-UTILS              :SEE :FILE lisp/mail/mail-utils.el
`mail-string-delete'

;; :MAIL-FUNCTIONS-SMTPMAIL                :SEE :FILE lisp/mail/smtpmail.el
`smtpmail-send-it'
`smtpmail-send-queued-mail'

`smtpmail-fqdn'
`smtpmail-maybe-append-domain'

:MAIL-FUNCTIONS-SMTPMAIL-AUTHENTICATION
`smtpmail-find-credentials'
`smtpmail-cred-server'
`smtpmail-cred-port'
`smtpmail-cred-key'
`smtpmail-cred-user'
`smtpmail-cred-cert'
`smtpmail-cred-passwd'


;; :MAIL-VARIABLES-SMTPMAIL
`smtpmail-default-smtp-server'
`smtpmail-smtp-server'
`smtpmail-smtp-service'
`smtpmail-debug-info'
`smtpmail-debug-verb'
`smtpmail-local-domain'
`smtpmail-sendto-domain'
`smtpmail-mail-address'
`smtpmail-code-conv-from'
`smtpmail-queue-mail'
`smtpmail-queue-dir'
`smtpmail-queue-index-file'

;; :MAIL-VARIABLES-GNUS                    :SEE :FILE lisp/gnus/*
`gnus-secondary-select-methods'
`gnus-verbose'

;; :MAIL-FUNCTIONS-EPA
`epa-global-mail-mode'
`epa-global-mail-mode'
`epa-mail-decrypt'
`epa-mail-encrypt'
`epa-mail-import-keys'
`epa-mail-mode'
`epa-mail-sign'
`epa-mail-verify'

;; :MAIL-FUNCTIONS-FEEDMAIL                :SEE :FILE lisp/mail/feedmail.el
`feedmail-queue-reminder'
`feedmail-run-the-queue'
`feedmail-run-the-queue-global-prompt'
`feedmail-run-the-queue-no-prompts'

;; :MAIL-FUNCTIONS-PASSWORD-CACHE          :SEE :FILE lisp/password-cache.el
`password-read'

;; :MAIL-VIEW
`browse-url-mail'
`mailcap-print-command'

;; :MAIL-VARIABLES-AUTHENTICATION-AUTH-SOURCE
`auth-sources'
`auth-source-user-or-password-smtp'
`auth-source-user-or-password'
`auth-source-forget-all-cached'

;; :MAIL-VARIABLES-AUTHENTICATION-EPA
`epa-file-cache-passphrase-for-symmetric-encryption' 
`epa-file-passphrase-alist'

;; :MAIL-VARIABLES-AUTHENTICATION-EPG        :SEE :FILE lisp/epg-config.el
`epg-debug'
`epg-gpg-home-directory', 
`epg-gpg-minimum-version'
`epg-gpg-program'
`epg-gpgsm-program'
`epg-passphrase-coding-system'

;; :MAIL-AUTHENTICATION-VARIABLES-MAILCAP     :SEE :FILE lisp/gnus/mailcap.el
`ssl-view-user-cert'
`ssl-view-site-cert'

;; :MAIL-VARIABLES-AUTHENTICATION-PASSWORD    :SEE :FILE lisp/password-cache.el
password-read

;; :MAIL-VARIABLES-AUTHENTICATION-SMTPMAIL    :SEE :FILE lisp/mail/smtpmail.el
`smtpmail-auth-supported'
`smtpmail-auth-credentials'
`smtpmail-starttls-credentials'
`smtpmail-starttls-credentials'

;; :MAIL-VARIABLES-AUTHENTICATION-STARTTLS
`starttls-gnutls-program'
`starttls-use-gnutls'

;; :MAIL-FILES-EMACS
:FILE lisp/password-cache.el
:FILE lisp/subr.el
:FILE lisp/simple.el
:FILE lisp/epa*.el
:FILE lisp/epg*.el
:FILE lisp/gnus/*
:FILE lisp/gnus/starttls.el
:FILE lisp/gnus/auth-source.el
:FILE lisp/gnus/mailcap.el
:FILE lisp/mail/*
:FILE lisp/mail/feedmail.el
:FILE lisp/mail/smtpmail.el
:FILE lisp/mail/sendmail.el
:FILE lisp/mail/mailclient.el
:FILE lisp/mail/mail-utils.el
:FILE lisp/net/eudc.el

;; :MAIL-FILES-SYSTEM-ETC
:FILE /etc/ca-certificates.conf
:FILE /etc/mailcap 
:FILE /etc/mail.rc 
:FILE /etc/mime.types
:FILE /etc/services
:FILE /etc/ssh
:FILE /etc/ssl /etc/ssl/certs/ca-certificates.crt

;; :MAIL-FILES-SYSTEM-HOME
:FILE ~/.mailcap  ~/mail.cap  ~/etc/mail.cap
:FILE ~/Mail ~/Mail/queued-mail
:FILE ~/.mime.types
:FILE ~/.authinfo.gpg 
:FILE ~/.gnupg gpg-agent.conf gpg.conf
:FILE ~/.ssh

;; :MAIL-FILES-SYSTEM-USR
:FILE /usr/share/ca-certificates

;; :MAIL-INFO-NODES
:SEE info node `(emacs)Sending Mail'
:SEE info node `(message)'

;; :MAIL-MAN-PAGES
:SEE \(man \"update-ca-certificates\"\)

:; :MAIL-RFC
:SEE RFC-1524
:SEE RFC-2104

:SEE-ALSO .\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-mail-functons :insertp t)
    (message (concat ":FUNCTION `mon-help-mail-functons' " 
                     "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-mail-functons )
;;; :TEST-ME (mon-help-mail-functons )
;;; :TEST-ME (mon-help-mail-functons )


;;; ==============================
(provide 'mon-doc-help-mail)
;;; ==============================

;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-doc-help-mail.el ends here
;;; EOF
