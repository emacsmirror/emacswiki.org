;;; authinfo.el -- hiding unnecessary passwords
;; 
;; Copyright (C) 2017  Alex Schroeder <alex@gnu.org>
;; 
;; Latest version:
;; https://github.com/kensanata/emacs-setup/blob/master/config/authinfo-conf.el
;; 
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;; 
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; I keep some passwords in my ~/.authinfo.gpg file. When I looked for
;; a password, I used to open the file and search for the correct host
;; name (i.e. the machine name). Sometimes there were people near my
;; screen and I did not feel comfortable looking for passwords in such
;; an environment, specially if there is an entire page full of them.
;; So now I want to use C-c p to copy a password and have it removed
;; from the clipboard after a few seconds.
;;
;; Things I might want to add in the future:
;; - completion of hosts and usernames

;;; Code

(require 'auth-source)
(require 'cl)

(global-set-key (kbd "C-c p") 'authinfo-copy-password)

(defvar authinfo-copied-passwords nil)

(defun authinfo-copy-password (host user)
  "Copy the password entry for a particular entry.

Remove the password from the kill ring after 10s.
The passwords to be removed are temporarily stored in
`authinfo-copied-passwords'."
  (interactive (authinfo-prompt))
  (let ((found (nth 0 (auth-source-search :host host :user user))))
    (when found
      (let ((secret (plist-get found :secret)))
	(when (functionp secret)
	  (setq secret (funcall secret)))
	(push secret authinfo-copied-passwords)
	(kill-new secret)
	(run-with-idle-timer 10 nil 'authinfo-clean-kill-ring)
	(message "Copied password")))))

(defun authinfo-clean-kill-ring ()
  "Remove all passwords from the kill-ring.

The passwords to be removed are temporarily stored in
`authinfo-copied-passwords'."
  ;; some experiments on W32 seem to indicate that this is the one
  (when (member (gui-get-selection 'CLIPBOARD) authinfo-copied-passwords)
    (gui-set-selection 'CLIPBOARD "SECRET"))
  ;; the kill-ring gets saved, too
  (setq kill-ring (delete-if (lambda (s)
			       (member s authinfo-copied-passwords))
			     kill-ring)
	authinfo-copied-passwords nil)
  (message "Cleared passwords from the kill ring"))

(defvar authinfo-host-history nil)

(defun authinfo-prompt ()
  "Prompt for host and username."
  (let* ((hosts (delete-duplicates
		 (loop for spec in
		       (auth-source-search :max most-positive-fixnum)
		       collect (plist-get spec :host))
		 :test 'string=))
	 (host (completing-read "Host: " hosts nil t nil
				'authinfo-host-history))
	 (users (delete-duplicates
		 (loop for spec in
		       (auth-source-search :host host
					   :max most-positive-fixnum)
		       collect (plist-get spec :user))
		 :test 'string=))
	 (user (if (> (length users) 1)
		   (completing-read (format "User for %s: " host)
				    users nil t nil
				    'authinfo-host-history)
		 (car users))))
    (list host user)))

(provide 'authinfo)
