;;; keychain-environment.el --- Loads keychain environment variables into emacs

;; Copyright (C) 2008-2011 Tabitha Tipper

;; Original Author:  Tabitha Tipper <bluefoo at googlemail dot com>
;; Heavy Patches from: Michael "cofi" Markert <markert dot michael at googlemail dot com>
;;                     Jonas Bernoulli <jonas at emacsmirror dot org>
;;                     Philip Hudson <phil dot hudson at iname dot com>
;; Keywords: keychain, ssh, gpg, agent
;; Created: 18 Dec 2008

;; Version: 1.1.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; Designed for use with Keychain (see:
;; http://docs.funtoo.org/wiki/Keychain) a tool for loading the SSH
;; Agent and keeping it running and accessible on a machine for longer
;; than a single login seession.
;; 
;; This library loads the file "$HOME/.keychain/$HOSTNAME-sh" and parses it for
;; the SSH_AUTH_SOCK and SSH_AUTH_PID variables.

;; It also checks "$HOME/.keychain/$HOSTNAME-sh-gpg" for the
;; GPG_AGENT_INFO variable, placing these into the environment of
;; Emacs.  If these files are empty, or the variables can't be parsed
;; out of them it doesn't replace existing environment variables.
;;
;; This is useful for situations where you are running Emacs under X, not
;; directly from a terminal, and its inheriting its environment from the
;; window manager, which doesn't have these variables as you started keychain
;; after you logged in (say as part of your .bashrc)
;;
;; The function (keychain-refresh-environment) can also be run at any time
;; these variables change.

;;; Installation:
;; Put the file in your load-path then use:
;; 
;;   (require 'keychain-environment)
;;   (keychain-refresh-environment)
;;
;; If you want to customise the location of the keychain file then use:
;;
;;   M-x customize-group
;;   keychain

;;; History:
;; 2008-12-18 Initial development.
;; 2009-02-25 Fixed bug with system-name being evaluated to the full hostname
;; 2010-07-27 Added GPG_AGENT support
;;            (by Michael Markert: markert dot michael at googlemail dot com)
;; 2011-08-10 Rolled several patches together

;;; Code: 

(defvar keychain-ssh-suffix "-sh" "Suffix of keychain SSH file")
(defvar keychain-gpg-suffix "-sh-gpg" "Suffix of keychain GPG file")

(defgroup keychain nil
  "Keychain integration."
  :group 'comm
  :group 'external
  :tag "Keychain"
  :link '(url-link "Homepage" "http://www.emacswiki.org/emacs/download/keychain-environment.el")
  :prefix "keychain-")

(defcustom keychain-dir (concat (getenv "HOME") "/.keychain/" )
  "Location of keychain directory. Normally `$HOME/.keychain'"
  :type 'string
  :group 'keychain)

;; This should increase portability as this is how keychain does it.
(defcustom keychain-host
  (if (executable-find "uname")
      (replace-regexp-in-string "\n$" "" (shell-command-to-string "uname -n"))
    (car (split-string system-name "\\." t)))
  "The hostname used to compose keychain filenames, goes between `keychain-dir' and `keychain-ssh-suffix' or `keychain-gpg-suffix'"
  :type 'string
  :group 'keychain)

(defcustom keychain-ssh-file
  (concat keychain-dir
          keychain-host
          keychain-ssh-suffix)
  "Stores the location of the keychain SSH file to load, normally found in `keychain-dir' and called '$HOSTNAME-sh'."
  :type 'string
  :group 'keychain)

(defcustom keychain-gpg-file
  (concat keychain-dir
          keychain-host
          keychain-gpg-suffix)
  "Stores the location of the keychain GPG file to load, normally found in `keychain-dir' and called '$HOSTNAME-gpg-sh'."
  :type 'string
  :group 'keychain)

;; Thanks to cofi for this method
(defun keychain-read-file (filename)
  "Takes a filename, reads the data from it and returns it as a string"
  (let ((real-filename (expand-file-name filename)))
    (if (file-exists-p real-filename)
        (with-temp-buffer
          (insert-file-contents real-filename)
          (buffer-string))
      "")))

;; Thanks to Philip Hudson, only parse variables if theres data and
;; Jonas Bernoulli only set variables if their valid.
(defun keychain-refresh-environment ()
  "Reads the keychain file for /bin/sh and sets the SSH_AUTH_SOCK, SSH_AGENT_PID
and GPG_AGENT variables into the environment and returns them as a list."
  (interactive)
  (let* ((ssh-data (keychain-read-file keychain-ssh-file))
         (gpg-data (keychain-read-file keychain-gpg-file))

         ssh-auth-sock
         ssh-auth-pid
         gpg-agent-info)
    
    (unless (string= "" ssh-data)
      (setq ssh-auth-sock
            (progn
              (string-match "SSH_AUTH_SOCK=\\(.*?\\);" ssh-data)
              (match-string 1 ssh-data)))
      (setq ssh-auth-pid
            (progn
              (string-match "SSH_AGENT_PID=\\([0-9]*\\)?;" ssh-data)
              (match-string 1 ssh-data))))
    
    (unless (string= "" gpg-data)
      (setq gpg-agent-info
            (progn
              (string-match "GPG_AGENT_INFO=\\(.*?\\);" gpg-data)
              (match-string 1 gpg-data))))
    
    (unless (string= "" ssh-auth-sock)
      (setenv "SSH_AUTH_SOCK" ssh-auth-sock))
    (unless (string= "" ssh-auth-pid)
      (setenv "SSH_AUTH_PID" ssh-auth-pid))
    (unless (string= "" gpg-agent-info)
      (setenv "GPG_AGENT_INFO" gpg-agent-info))
    
    (list ssh-auth-sock ssh-auth-pid gpg-agent-info)))

;; tipper: Only because I named it very badly the first time we alias
;; it now, this should be withdrawn in the future.
(defalias 'refresh-keychain-environment 'keychain-refresh-environment)

(provide 'keychain-environment)
