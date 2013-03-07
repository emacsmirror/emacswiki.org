;;; jabber-encrypt.el --- 
;; 
;; Filename: jabber-encrypt.el
;; Description: Send and recieve message with encription transparently.
;; Author: Christian Giménez
;; Maintainer: 
;; Created: mié mar  6 19:52:00 2013 (-0300)
;; Version: 
;; Last-Updated: mié mar  6 21:55:15 2013 (-0300)
;;           By: Christian
;;     Update #: 41
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 6-Mar-2013    Christian  
;;    Last-Updated: mié mar  6 21:54:47 2013 (-0300) #39 (Christian)
;;    Added some "requires".
;; 6-Mar-2013    Christian Giménez
;;    Last-Updated: mié mar  6 21:53:04 2013 (-0300) #37 (Christian)
;;    First release! :)
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'jabber)
(require 'jabber-core)

(defvar jabber-enc-user-list nil
  "This store a list of usernames where to get the public keys.
Use M-x `jabber-enc-add-user' to add a user to this list. 

Ensure that `epg-list-keys' return the apropiate keys to use.")

(defvar jabber-enc-on nil
  "Buffer local variable. This is t when this jabber-chat buffer must encrypt the output and decrypt the input.")

(make-variable-buffer-local 'jabber-enc-user-list)
(make-variable-buffer-local 'jabber-enc-on)

(defun jabber-enc-add-user (username)
  "Add a USERNAME to the list of recipients to this jabber-chat buffer.

Try to be very explicit about the username. 

See `jabber-enc-user-list'."
  (interactive "MUsername?")
  (add-to-list 'jabber-enc-user-list username)
  )

(defun jabber-enc-list-users ()
  "Return the usernames in the list of recipients of this jabber-chat-buffer.

See `jabber-enc-user-list'."
  (interactive)  
  (if jabber-enc-user-list
      (let ((str-list ""))
	(dolist (e jabber-enc-user-list)
	  (setq str-list (concat str-list "*" e "*"))
	  )
	(message str-list)
	)
    (message "No recipients is in the list.")
    )
  )

(defun jabber-enc-list-clear ()
  "Clear the list of recipients for this jabber-chat-buffer."
  (setq jabber-enc-user-list nil)
  )

(defun jabber-enc-start ()
  "Start with a chat encripting the output and decrypting the input."
  (interactive)
  (setq jabber-enc-on t)
  )

(defun jabber-enc-stop ()
  "Start with a chat encripting the output and decrypting the input."
  (interactive)
  (setq jabber-enc-on nil)
  )

(defun jabber-enc-msg (body)
  "Encript message. Use USERSLIST as a list of nicknames or user names.
BODY must be the message to encrypt."
  (let ((context (epg-make-context 'OpenPGP t)))
    (epg-encrypt-string
     context
     (encode-coding-string body 'utf-8)
     (epg-list-keys context jabber-enc-user-list)
     )
    )
  )

(defun jabber-dec-msg (msg)
  "Decrypt message MSG."
  (epg-decrypt-string (epg-make-context 'OpenPGP t) msg)
  )


(defadvice jabber-chat-send (before jabber-enc-send-adv (jc body))
  "Check if we have to encrypt the output according to `jabber-enc-on'. If it is t, encrypt the BODY."  
  (when jabber-enc-on	   
    ;;(setq body (jabber-enc-msg body))
    (setq body (concat "*ENC*" body))
    )
  )

(defadvice jabber-send-sexp (before jabber-send-sexp-adv (jc stanza))
  "Encrypt before sending."
  (when jabber-enc-on
    (let ((text (assoc 'body stanza))
	  )
      (when text
	(setq text (list 
		    (car text)
		    (cadr text)
		    (jabber-enc-msg (caddr text)))) 
	(assq-delete-all 'body stanza)
	(add-to-list 'stanza text t)
	)
      )
    )
  )

(defun jabber-chat-decrypt-incomming (from buffer body title)
  "Find BODY in the current buffer and change it for the decrypted message of BODY itself."
  (when jabber-enc-on
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
	    )
	(goto-char (point-max))
	(when (search-backward body nil t)
	  (replace-match (jabber-dec-msg body))
	  )
	)
      )
    )
  )

(add-hook 'jabber-alert-message-hooks 'jabber-chat-decrypt-incomming)

(ad-activate 'jabber-chat-send)
(ad-activate 'jabber-send-sexp)

(provide 'jabber-encrypt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jabber-encrypt.el ends here
