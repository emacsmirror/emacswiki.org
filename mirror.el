;;; mirror.el --- Maintain a (remote) copy of a (local) file

;; Copyright (C) 2002 Free Software Foundation, Inc.
;; Author: Kahlil (Kal) HODGSON <dorge@tpg.com.au>
;; X-URL: http://www.emacswiki.org/elisp/mirror.el
;; Keywords: convenience, files
;; Time-stamp: <2002-09-30 13:22:07 kahlil>
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides a facility to update a (possibly remote) copy of the
;; current buffer either automatically (by setting
;; `mirror-update-on-save') or on demand (via the function
;; `mirror-update-this-file').  Just how this happens is determined by
;; a series of buffer local variables.  Since it is very lightweight
;; and makes few assumptions about the remote mirror files and hosts
;; it can handle certain situations that `ange-ftp' or `tramp' cannot
;; (yet).

;;; Installation:

;; Place the following somewhere in your .emacs file

;;  (require 'mirror)

;; and set one or more of the following in the local variables section
;; of a file you wish to mirror (see the bottom of this file for
;; example).

;; `mirror-file-path': set this to the full path (including filename)
;; to the mirror file.  Note that the mirror file need not have the
;; same name as the local file.  This can be useful for getting around
;; suffix conventions on different machines (eg .html versus .HTM).

;; `mirror-file-host': set this to the name of the remote host on
;; which the mirror file resides.  If this is nil (the default) then
;; the file is assumed to reside on the same file system and hence the
;; emacs lisp function `copy-file' is used to perform the update.  If
;; this variable is non-nil then a remote connection to the host is
;; established using `mirror-sftp-command'.

;; `mirror-sftp-command': set this to the shell command used to
;; establish a remote connection to `mirror-file-host'. The default is
;; "sftp".  This remote session will persist throughout your emacs
;; session and the same session will be used to update any mirror
;; files on that host.  You may also have to set
;; `mirror-password-prompt-regexp' `mirror-login-prompt-regexp' and
;; `mirror-prompt-regexp' to get this to work.

;; `mirror-time-to-connection': set this to the amount of time you are
;; willing to wait for a connection to the remote host to be
;; established.  The default is 10 seconds which is usually enough,
;; but you can set it higher for connections you expect to be
;; especially slow.

;; `mirror-update-command': set this to the command that should be
;; passed to `mirror-sftp-command' whenever a the mirror file is to be
;; updated. The default is "put". This command must take two
;; arguments: the full path to the current file and the full path to
;; the remote file.

;; `mirror-update-on-save': if this is non-nil then the mirror file
;; will automatically be updated whenever the buffer is saved. The
;; default is nil. This facility is most useful when you wish to edit
;; files on a remote system where using `tramp' or `ange-ftp' are too
;; slow or just don't work, for example, editing files on a Windows
;; 2000 server (not running CygWin) from a UNIX machine (this was the
;; original motivation for the package).

;;; Code:

(require 'comint)

(defvar mirror-sftp-command "sftp"
  "*The shell command used to start a remote access session e.g. sftp on
`mirror-file-host'.")
(make-variable-buffer-local 'mirror-sftp-command)

(defvar mirror-password-prompt-regexp  comint-password-prompt-regexp
  "Regular expression to match password prompt if any.")
;;(set-default 'mirror-paswword-prompt-regexp comint-password-prompt-regexp)
(make-variable-buffer-local 'mirror-password-prompt-regexp)

(defvar mirror-login-prompt-regexp
  "Login:\\|Name.*:"
  "Regular expression matching login name prompt.")
(make-variable-buffer-local 'mirror-login-prompt-regexp)

(defvar mirror-prompt-regexp  "^s*ftp>"
  "Regular expression matching the sftp name prompt.")
;;(set-default 'mirror-prompt-regexp "^sftp>")
(make-variable-buffer-local 'mirror-prompt-regexp)

(defvar mirror-time-to-connection 10
  "*Time we are willing to wait for a connection to the remote host to
be established.")
(make-variable-buffer-local 'mirror-time-to-connection)

(defvar mirror-update-command "put"
  "*Command passed to `mirror-sftp-command' in order to update the
mirror file.  Must take two arguments: the full path to the current
file and the full path to the remote file.")
(make-variable-buffer-local 'mirror-update-command)

(defvar mirror-update-on-save nil
  "*Automatically update the remote mirror whenever this file is saved.")
(make-variable-buffer-local 'mirror-update-on-save)

(defvar mirror-file-host nil
  "*Remote host on which the mirror file resides.  If this is nil the
local host is assumed and `copy-file' is used to update the mirror.")
(make-variable-buffer-local 'mirror-file-host)

(defvar mirror-file-path nil
  "*Full pathname of the (remote) mirror file")
(make-variable-buffer-local 'mirror-file-path)

(defun mirror-update-this-file-maybe ()
  "Update the remote mirror iff local variable `mirror-this-file' is t."
  (condition-case error
      ;; double check in case we have turned this off temporarily with
      ;; M-x set-variable
      (when mirror-update-on-save
	(mirror-update-this-file))
    (error t)))

(defun check-for-mirror-this-file ()
  "Add `mirror-update-this-file-maybe' to local `after-save-hook'
iff local variable `mirror-update-on-save' is t."
  (condition-case error
      (when mirror-update-on-save ;; local-variable
	(add-hook 'after-save-hook 'mirror-update-this-file-maybe t t))
    (error t)))

(add-hook 'find-file-hooks 'check-for-mirror-this-file t nil)

(defun mirror-update-this-file ()
  "*Update the mirror of this file using either copy-file or sftp."
  (interactive)
  (if mirror-file-host
      (mirror-update-remote-file)
    (copy-file (buffer-file-name) mirror-file-path t t)
    (message "Mirror file updated!")))

;; Set up an sftp connection if required
;; based on telnet mode

(defun mirror-initial-filter (proc string)
  "Handle any password or user-name prompts that may pop up."
  ;;For reading up to and including password;
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((case-fold-search t))
      (cond ((string-match "No such host" string)
	     (kill-buffer (process-buffer proc))
	     (error "No such host"))

	    ((string-match mirror-password-prompt-regexp string)
	     (comint-output-filter proc string)
	     (send-string proc (concat (comint-read-noecho "Password: " t)
				       "\n"))
	     (clear-this-command-keys))

	    ((string-match mirror-login-prompt-regexp string)
	     ;; need a login name
	     (comint-output-filter proc string)
	     (send-string proc (concat (read-string "User Name: ")
				       "\n"))
	     (clear-this-command-keys))

	    ((string-match mirror-prompt-regexp string)
	     ;; successful login
	     (comint-output-filter proc string)
	     (set-process-filter proc 'comint-output-filter))

	    (t (comint-output-filter proc string))))))

(defun mirror-update-remote-file ()
  "Update the mirror of this file using sftp."
  (let* ((local-file-path (buffer-file-name))
	 (remote-file-path mirror-file-path)
	 (mirror-buffer-identifier (concat "mirror-sftp-" mirror-file-host))
	 (mirror-buffer-name (concat " *" mirror-buffer-identifier "*"))
	 (mirror-buffer (get-buffer mirror-buffer-name))
	 (host mirror-file-host)
	 (process nil)
	 (found nil)
	 ;; these variables are buffer local and need to be exported
	 ;; to the comint buffer
	 (login-prompt-regexp mirror-login-prompt-regexp)
	 (password-prompt-regexp mirror-password-prompt-regexp)
	 (prompt-regexp mirror-prompt-regexp))

    ;; only make one connection per host
    (unless (and mirror-buffer (get-buffer-process mirror-buffer))

      ;; does it need to be a commit buffer?
      (make-comint mirror-buffer-identifier mirror-sftp-command nil host)
      (setq mirror-buffer (get-buffer (concat "*" mirror-buffer-identifier "*")))

      (save-excursion
	(set-buffer mirror-buffer)
	(setq process (get-buffer-process mirror-buffer))

	;; export the local variables
	(setq mirror-login-prompt-regexp login-prompt-regexp
	      mirror-password-prompt-regexp password-prompt-regexp)

	(set-process-filter process 'mirror-initial-filter)

	;; make the buffer invisible
	(rename-buffer mirror-buffer-name mirror-buffer)

	;; wait for connection
	(message "Waiting %d seconds for connection to %s..."
		 mirror-time-to-connection host)
	(if (with-timeout (10 (not found))
	      (while (not found)
		(accept-process-output process 1)
		(goto-char (point-max))
		(beginning-of-line)
		(setq found (looking-at prompt-regexp))))
	    (message "Waiting %d seconds for connection  %s...failed"
		     mirror-time-to-connection host)
	  (message "Waiting %d seconds for connection %s...done"
		   mirror-time-to-connection host)
	  )))

    (setq process (get-buffer-process mirror-buffer))

    ;; use full paths so we don't have to worry about directories
    (process-send-string
     process (concat mirror-update-command " "
		     local-file-path " "
		     remote-file-path "\n"))

    ;; wait for response
    (save-excursion
      (set-buffer mirror-buffer)
      (setq found nil)
      (if (with-timeout (10 (not found))
	      (while (not found)
		(accept-process-output process 1)
		(goto-char (point-max))
		(beginning-of-line)
		(setq found (looking-at prompt-regexp))))
	  (progn
	    (message "Connection to %s broken!" host)
	    (kill-buffer mirror-buffer)))
	  (message "Mirror file updated!"))
    ))


(provide 'mirror)

;;; mirror.el ends here
