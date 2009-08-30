;;; facebook.el --- Access the Facebook API from emacs
     
;;; Copyright (C) 2007 Paul Huff
     
;;; Author: Paul Huff <paul.huff@gmail.com>
;;; Maintainer: Paul Huff <paul.huff@gmail.com>
;;; Created: 5 Oct 2007
;;; Version: 0.0.1
;;; Package-Requires: ((json "0"))
;;; Keywords: facebook, frivolity

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA     

;;; Commentary:

;;;   First pass at interfacing with facebook.  You must add the "developers 
;;;   app" and get your own application api-key and application secret key to 
;;;   use this.  Also, facebook's authentication scheme is, well, ridiculous, 
;;;   so you've got to have gnutls or openssl installed, as well as a working
;;;   browser that handles https to log in and give your app permissions.
;;;   Also, it requires hober's excellent json.el which can be obtained here:
;;;   http://edward.oconnor.cx/elisp/json.el

;;;   To log into facebook, call (facebook-login)
;;;   To check credentials/optionally login call (facebook-check-creds)
;;;   To make api calls call (facebook-call-function "<funcname>" (list (cons "of" "args") (cons "more" "args")))

;;;   Only two actual api functions are present right now, (though you can 
;;;   use facebook-call-function to call whatever you'd like...):
;;;   facebook-users-has-app-permission = users.hasAppPermission
;;;   facebook-users-set-status = users.setStatus
;;;   facbeook-users-set-status checks for the applicable app permission 
;;;   redirects the user to the permission page if needed and then goes and 
;;;   updates the status.

;;;   The sf.net project homepage for this project can be found here:
;;;   http://sf.net/projects/facebook-el

;;; Code:
(require 'json)
(defcustom facebook-api-key "" "Your facebook api-key")
(defcustom facebook-api-secret "" "Your facebook api secret")
(defcustom facebook-session-info 'nil "Your facebook session info.  This will be set by facebook.el")

(defun facebook-call-function-post (method-name args)
  "Send ARGS to URL as a POST request."
  (let* ((url "http://api.facebook.com/restserver.php")
	 (url-request-method "POST")
	 (url-request-extra-headers
	  '(("Content-Type" . "application/x-www-form-urlencoded")))
	 (full-args (append (list (cons "method" method-name) (cons "v" "1.0")
				  (cons "format" "JSON") 
				  (cons "api_key" facebook-api-key)) 
			    args))
	 (full-args-call-id (cons (cons "call_id" 
					(mapconcat 'number-to-string 
						   (current-time) "")) 
				  full-args))
	 (sorted-full-args (sort full-args-call-id 'car-string<))
	 (sig-full-args (cons (cons "sig" 
				    (facebook-sig-creator sorted-full-args))
			      sorted-full-args))
	 (url-request-data
	  (mapconcat (lambda (arg)
		       (concat (url-hexify-string (car arg))
			       "="
			       (url-hexify-string (cdr arg))))
		     sig-full-args
		     "&"))
	 (result 
	  (save-excursion
	    (set-buffer (url-retrieve-synchronously url))
	    (progn (goto-char (point-min))
		   (delete-region (point-min) (search-forward "\n\n"))
		   (buffer-substring (point-min) (point-max))))))
    
    (setq facebook-last-raw-json-result result)
    (json-read-from-string result)))

(defun car-string< (a b)
  (string< (car a) (car b)))

(defun facebook-sig-creator (args-array &optional secret)
  (let* ((request-str (mapconcat (lambda (arg)
				   (concat (car arg)
					   "="
					   (cdr arg)))
				 args-array
				 ""))
	 (request-str-secret (if secret
				 (concat request-str secret)
			       (if (and facebook-session-info
					(or (equal (assoc-default 'expires facebook-session-info) 0)
					    (<= (map-concat 'number-to-string (current-time) "")
						(assoc-default 'expires facebook-session-info))))
				   (concat request-str (assoc-default 'secret facebook-session-info))
				 (concat request-str facebook-api-secret)))))
    (md5 request-str-secret)))

;;;###autoload
(defun facebook-login ()
  (interactive)
  (let* ((auth-token (facebook-call-function-post "auth.createToken" '()))
	 (login-url (concat 
		     "https://login.facebook.com/login.php?v=1.0&api_key=" 
		     facebook-api-key "&auth_token=" auth-token)))
    (browse-url login-url)
    (read-string "Hit enter here after you've logged into facebook (this is their lame scheme, not facebook.el's")
    (customize-save-variable 'facebook-session-info 
			     (facebook-call-function-post "auth.getSession"  
							  (list (cons 
								 "auth_token" 
								 auth-token)))
			     )))


(defun facebook-check-creds ()
  (interactive)
  (let* ((session-expires 
	  (assoc-default 'expires facebook-session-info 'nil -1))
	 (current-time (mapconcat 'number-to-string (current-time) "")))
    (if (not (equal session-expires 0))
	(if (or (eq session-expires 'nil) 
		(<= session-expires current-time))
	    (facebook-login)
	  't)
      't)))

(defun facebook-users-has-app-permission (permission)
  (interactive)
  (let ((result 
	 (facebook-call-function-post "users.hasAppPermission" 
				      (list (cons "session_key" 
						  (assoc-default 
						   'session_key 
						   facebook-session-info)) 
					    (cons "ext_perm" permission)))))
    (setq facebook-last-app-permission result)))


;;;###autoload
(defun facebook-users-set-status (status_message)
  (interactive)
  (progn
    (facebook-check-creds)
    (if (equal (facebook-users-has-app-permission "status_update") 0)
	(let* ((auth-url (concat 
			  "http://www.facebook.com/authorize.php?api_key="
			  facebook-api-key "&v=1.0&ext_perm=status_update"))) 
	  (progn
	    (browse-url auth-url)
	    (read-string "Hit enter here after you've granted your app_key permission to update your status (should only have to do this once... this is their lame scheme, not facebook.el's)")
	    )))
    (message "Setting status to: %s" status_message)
    (let ((result 
	   (facebook-call-function-post "users.setStatus" 
					(list 
					 (cons "session_key" 
					       (assoc-default 
						'session_key 
						facebook-session-info)) 
					 (cons "status" status_message)))))
      (setq facebook-set-status-result result))))

;;;###autoload
(defun facebook-status (status_message)
  (interactive  "sStatus message: ")
  (facebook-users-set-status status_message))


(provide 'facebook)
;;; facebook.el ends here
