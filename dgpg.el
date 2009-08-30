;;; dgpg.el - David's GPG Extensions for Emacs
;;;

;; --------------------------------------------------
;; Copyright (c) 2006 David Elentok
;;
;; Author: David Elentok <3david@gmail.com>
;;
;; Contributors: this was partly inspired by mc-gpg-file-mode.el as
;;               found on http://www.emacswiki.org
;;               minor improvements by R. Gloeckner: 
;;                  autocomplete key-inputs
;;                  enable kill-ring for password input 
;;                    "..." will be replaced by (current-kill 0)
;;
;; Version: 0.1 of 2006-02-03
;; --------------------------------------------------


;; --------------------------------------------------
;; This file is not part of GNU Emacs.

;; dgpg.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; dgpg.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;; --------------------------------------------------

;; --------------------------------------------------
;; Abstract
;;
;; This mode is independent from mailcrypt, the only requirement is a 
;; working GnuPG setup.
;;
;; This file includes the dgpg mode and a set of function that can be used
;; outside this mode:
;;
;; * dgpg-mode:
;;   
;;   every file that ends with ".asc" or ".gpg" will be automatically decrypted
;;   (after asking the user for the password)
;;
;;   when saving the file to the disk it will be automatically encrypted.
;;
;; * dgpg-encrypt-buffer:
;;  
;;   This function encrypts the current buffer after asking the user for 
;;   the recipient.
;;
;; * dgpg-decrypt-buffer:
;;   
;;   This function decrypts the current buffer after asking the user for
;;   the password.
;;
;; * dgpg-sign-buffer:
;;
;;   This function creates a signature of the current buffer and adds it at
;;   the end of the buffer.
;;
;; * dgpg-verify-buffer:
;;
;;   This function looks for a GPG/PGP message block in the buffer and 
;;   checks who signed it.
;;
;; --------------------------------------------------

;; --------------------------------------------------
;; How to use this mode:
;;
;; * Just put this in your .emacs file:
;;
;;   (require 'dgpg)
;;   (setq dgpg-program "path-to-gpg-program")
;;
;;   ;; the primary recipient (used for the --recipient gpg argument)
;;   (setq dgpg-user-id "user-id-for-primary-recipient")
;;
;;   ;; this list is for the autocompletion when asked which key to use
;;   (setq dgpg-recipients
;;         (list dgpg-user-id
;;               "email@somewhere.com"
;;               "another-email@somewhere-else.com"))
;; --------------------------------------------------

;; --------------------------------------------------
;; Dependencies and Compatability:
;;
;; This version of dgpg was tested with (GNU) emacs 21.3.1, gpg 1.4.2 on
;; Windows XP Pro. (sorry, can't switch yet)
;; --------------------------------------------------

;; --------------------------------------------------
;; Beginning of the code
;; --------------------------------------------------

(require 'comint)

;; Default Variables
(setq-default dgpg-program "path-to-gpg")
(setq-default dgpg-user-id "standard@email.adr")
(setq-default dgpg-comment "Encrypted using David's Emacs GPG Extension (small improvements by R. Gloeckner)")
(setq-default dgpg-recipients
	      (list dgpg-user-id
		    "noone@nowhere.som"
		    "anotherone@nowhere.som"))

;; --------------------------------------------------
;; Get the key to use from the user (with autocomplete)
;; --------------------------------------------------
(defun dgpg-get-key ()
  "reads a key (email-adress) from minibuffer using autocomplete list"
  (interactive)
  (completing-read 
   (concat "Key to use [" dgpg-user-id "]: ")
   dgpg-recipients))

;; --------------------------------------------------
;; Get the passphrase from the user
;; --------------------------------------------------
(defun dgpg-get-passwd ()
  "reads a passwd from minibuffer if ... last kill will be inserted"
  (interactive)
  (let ((pwd (comint-read-noecho "Password: ")))
    (cond 
     ((string= pwd "...") (current-kill 0))
     (t pwd))))

;; --------------------------------------------------
;; Encrypt a region: dgpg-encrypt-region
;; --------------------------------------------------
(defun dgpg-encrypt-region ()
  "encrypts a buffer-region via gpg"
  (interactive)
  (shell-command-on-region 
   (point) (mark) 
   (concat dgpg-program 
	   " --armor --comment \""
	   dgpg-comment
	   "\" --encrypt --recipient "
	   (dgpg-get-key))
   nil t))

;; --------------------------------------------------
;; Encrypt the entire buffer: dgpg-encrypt-buffer
;; --------------------------------------------------
(defun dgpg-encrypt-buffer ()
  "encrypts a whole buffer via gpg"
  (interactive)
  (shell-command-on-region 
   (point-min) (point-max) 
   (concat dgpg-program
	   " -a --comment \""
	   dgpg-comment
	   "\" -e -r "
	   (dgpg-get-key))
   nil t))


;; --------------------------------------------------
;;    a helper function to shorten code
;; --------------------------------------------------
(defun dgpg-after-proc (proc) 
  "do the after-process-work"
  (process-send-eof proc)
  ;; wait for it to finish
  (while (eq 'run (process-status proc))
    (accept-process-output proc 5))
  
  ;; remember result codes
  (setq status (process-status proc))
  (setq rc (process-exit-status proc))
  (message (if (= rc 0)
 	       (format "Signed Successfully")
	     (format "Error: gpg return code = %s" rc)))
  
  (delete-process proc)
  
  ;; remove the "Process *GPG* killed" message
  (goto-char (point-max))
  (if (re-search-backward "\nProcess \\*GPG.*\n\\'" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
  (goto-char (point-min))
  
  ;; ponder process death: signal, not just rc!=0
  (if (or (eq 'stop status) (eq 'signal status))
      ;; process died
      (error "%s exited abnormally: '%s'" program rc) ;;is rc a string?
    )
  
  (if (= 127 rc)
      (error "%s could not be found" program) ;; at least on my system
    ))




;; --------------------------------------------------
;; Decrypt Buffer: dgpg-decrypt-buffer
;; --------------------------------------------------
(defun dgpg-decrypt-buffer()
  "decryption via gpg"
  (interactive)
  (setq prev-point-max (point-max))
  ;; create the process
  (setq proc
	(apply 'start-process-shell-command 
	       "*GPG*" (buffer-name) dgpg-program 
	       (list "--passphrase-fd"
		     "0"
		     "-d")))
  ;; send the password
  (process-send-string proc (concat (dgpg-get-passwd) "\n"))
  ;; send the region
  (process-send-region proc (point-min) (point-max))
  ;; finish
  (dgpg-after-proc proc))


;; --------------------------------------------------
;; Sign Buffer: d-pgp-sign-buffer
;; --------------------------------------------------
(defun dgpg-clearsign-buffer()
  "clearsigns a buffer (kills content before)"
  (interactive)
  (setq prev-point-max (point-max))
  (setq key (dgpg-get-key))
  ;; create the process
  (setq proc
	(apply 'start-process-shell-command 
	       "*GPG*" (buffer-name) dgpg-program 
	       (list "--passphrase-fd"
		     "0"
		     "--comment"
		     (concat "\"" dgpg-comment "\"")
		     "--armor"
		     "--clearsign"
		     "--local-user"
		     (concat "\"" key "\""))))
  ;; send the password
  (process-send-string proc (concat (dgpg-get-passwd) "\n"))
  ;; send the region
  (kill-region (point-min) (point-max))
  (process-send-string proc (current-kill 0))
  ;; finish
  (dgpg-after-proc proc))




(defun dgpg-sign-buffer()
  "sign (not clearsign) a buffer via gpg"
  (interactive)
  (setq prev-point-max (point-max))
  (setq key (dgpg-get-key))
  ;; create the process
  (setq proc
	(apply 'start-process-shell-command 
	       "*GPG*" (buffer-name) dgpg-program 
	       (list "--passphrase-fd"
		     "0"
		     "--comment"
		     (concat "\"" dgpg-comment "\"")
		     "--armor"
		     "--sign"
		     "--local-user"
		     (concat "\"" key "\""))))
  ;; send the password
  (process-send-string proc (concat (dgpg-get-passwd) "\n"))
  ;; send the region
  (process-send-region proc (point-min) (point-max))
  ;; finish
  (dgpg-after-work proc))


;; --------------------------------------------------
;; Verify Signature: d-pgp-verify-signature
;; --------------------------------------------------
(defun dgpg-verify-signature()
  "verify gpg signature"
  (interactive)
  
  ;; find the beginning of the pgp message
  (goto-char (point-min))
  (re-search-forward "-*BEGIN PGP MESSAGE-*" nil t)
  (setq point-start (match-beginning 0))
  (re-search-forward "-*END PGP MESSAGE-*" nil t)
  (setq point-end (match-end 0))
  ;;(message (format "found: %d = %d" point-start point-end)))

  (setq prev-point-max (point-max))
  ;; create the process
  (setq proc
	(apply 'start-process-shell-command 
	       "*GPG*" (buffer-name) dgpg-program 
	       (list "--verify")))
  ;; send the region
  (process-send-region proc point-start point-end)
  ;; finish
  (dgpg-after-proc proc))


;; --------------------------------------------------
;; dgpg mode
;; --------------------------------------------------
(define-generic-mode 'dgpg-mode
  (list ?#) 
  nil nil
  '(".gpg\\'" ".asc\\'" ".gpg-encrypted\\'")
  (list (lambda () 
	  (add-hook 'local-write-file-hooks
		    (lambda () 
		      (dgpg-encrypt-buffer)
		    nil nil) nil t)
	  (add-hook 'after-save-hook 
		    (lambda () 
		      (undo)
		      ;;(dgpg-decrypt-buffer)
		      (set-buffer-modified-p nil)
		      (message "File Saved (encrypted)"))
		    nil t)
	  (dgpg-decrypt-buffer)
	  (auto-save-mode nil)
	  (set-buffer-modified-p nil)))
  "Mode for gpg encrypted files")

(provide 'dgpg)
