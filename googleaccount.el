;;; googleaccount.el --- Google Accounts login from Emacs

;;; Author: Riccardo Murri <riccardo.murri@gmail.com>
;;; Version: 1.1
;;; X-URL: http://www.emacswiki.org/cgi-bin/emacs/googleaccount.el


;;; Copyright (c) 2007-2009 Riccardo Murri <riccardo.murri@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA


;;; Commentary:

;; This file should be useful only to authors of packages that
;; interface Emacs to Google services; it implements functions to
;; request an authorization token using the Google ClientLogin web
;; service (see
;; http://code.google.com/apis/accounts/AuthForInstalledApps.html )
;; and handle the error cases.

;; Sample usage::
;;
;;   (require 'googleaccount)
;;   ...
;;   (setq auth-header
;;      (googleaccount-login service email passwd))
;;   ...
;;   (if auth-header
;;     (let ((url-request-extra-headers (list auth-headers)))
;;        (url-retrieve ...some Google service URL...)

;; Function `googleaccount-login' tries to do the right thing: 
;; - returns the authorization header to be added to
;;   `url-request-extra-headers' when authorization is successful;
;; - returns nil when Google requires a CAPTCHA challenge to unlock
;;   the account, so that the calling program may retry login at a
;;   later time; 
;; - signals an error on any other case.

;; If you want full control, use `googleaccount-login-response', that
;; simply returns an alist with all data sent and received, and never
;; interacts with the user.

;; Only tried in Emacs 22.1 and 23.0.91.1; all the code relating to
;; CAPTCHA and errors other than "BadAuthentication" is *totally*
;; untested.


;;; History:

;; 2007-10-23: Initial release.
;; 2009-04-19: Fix for Emacs 23 by Taiki SUGAWARA <buzz.taiki@gmail.com>


;;; Code:

(require 'url)


(defconst googleaccount-login-url "https://www.google.com/accounts/ClientLogin"
  "URL to submit GoogleAccount login requests to.")

(defconst googleaccount-captcha-unlock-url 
  "https://www.google.com/accounts/DisplayUnlockCaptcha"
  "URL for Google page to unlock CAPTCHAs.")

(defconst googleaccount-source-name "emacs-googleaccount-1.0"
  "Client application name and version string.
Required by Google for logging purposes.")


(eval-when-compile
  (defmacro googleaccount-define-error (error-symbol &optional error-message)
  `(put ,error-symbol
        'error-conditions
        `(error googleaccount-error ,error-symbol))
  (if error-message
    `(put ,error-symbol 'error-message ,error-message))))

(googleaccount-define-error 
 'googleaccount-error "GoogleAccount login error")
;; see http://code.google.com/apis/accounts/AuthForInstalledApps.html#Errors
(googleaccount-define-error 'googleaccount-bad-authentication)
(googleaccount-define-error 'googleaccount-not-verified)
(googleaccount-define-error 'googleaccount-terms-not-agreed)
(googleaccount-define-error 'googleaccount-captcha-required)
(googleaccount-define-error 'googleaccount-account-deleted)
(googleaccount-define-error 'googleaccount-account-disabled)
(googleaccount-define-error 'googleaccount-service-disabled)
(googleaccount-define-error 'googleaccount-service-unavailable)
(googleaccount-define-error 'googleaccount-unknown)
(googleaccount-define-error 'googleaccount-unexpected)


(defun googleaccount-parse-response (&optional buffer alist)
  "Add (string) key=value pairs found in BUFFER to ALIST, and return it."
  (declare (special url-http-end-of-headers))
  (save-excursion
    (if buffer (set-buffer buffer))
    (goto-char (1+ url-http-end-of-headers))
    (while (re-search-forward "^\\([A-Za-z]+\\)=\\(.+\\)$" nil t)
      (setq alist (list* (cons (match-string 1) (match-string 2)) alist))))
  alist)


(defun googleaccount-uncamelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
Default for SEP is a hyphen \"-\".

If third argument START is non-nil, start converting at that
index in STRING.

Return modified string."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-") 
                                             (downcase (match-string 0 s))) 
                                     t nil s)))
    (downcase s)))


(defun googleaccount-login (service email passwd)
  "Login into Google Account identified by EMAIL and PASSWD.
Upon successful login, returns the pair of strings 
(AUTH-HEADER . AUTH-KEY), suitable for adding 
to `url-request-extra-headers'.
If a CAPTCHA challenge was received, notify user and return nil.
Otherwise, signal error.
"
  (lexical-let* ((response (googleaccount-login-response service email passwd))
                 (status (cdr (assq 'status response))))
  (cond
    ((eq status 'ok)
      (googleaccount-auth-http-header response))
    ((eq status 'captcha)
     (googleaccount-handle-captcha response))
    ((eq status 'error)
     (googleaccount-handle-error response)))))


(defun googleaccount-login-response (service email passwd)
  "Login into Google Account identified by EMAIL and PASSWD, and return response.

Returns an alist, associating:
- to the key `status', one of the symbols:
    - `ok': user was successfully authenticated;
    - `captcha': a CAPTCHA challenge was issued;
    - `error': some other error occurred, in which case the key
      `google-error' contains the appropriate Emacs error symbol;
- to the string keys \"Email\", \"Passwd\", \"service\" and
  \"source\", the like-named arguments of this function;
- any other key/value pair corresponds to the key/value pairs
  gotten from the Google ClientLogin HTTP response.

See also `googleaccount-login'.
"
  (let* ((rr (list
              (cons "accountType" "HOSTED_OR_GOOGLE")
              (cons "Email" email)
              (cons "Passwd" passwd)
              (cons "service" service)
              (cons "source" googleaccount-source-name)))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          (mapconcat (lambda (arg)
                       (format (concat (car arg) "=%s") 
                               (url-hexify-string (cdr arg))))
                     rr "&"))
         (response-buf (url-retrieve-synchronously googleaccount-login-url)))
    (if (not response-buf)
        (signal 'googleaccount-error "Error in HTTP POST to Google Account URL"))
    (nconc rr (googleaccount-parse-response response-buf))
    (kill-buffer response-buf)
    (cond
     ((assoc-string "Auth" rr t)
      ;; set 'status to 'ok and return response
      (list* '(status . ok) rr))
           
     ((assoc-string "CaptchaUrl" rr t)
      ;; set 'status to 'captcha and return response
      (list* '(status . captcha) rr))

     ((assoc-string "Error" rr t)
      (list* '(status . error)
             (cons 'google-error 
                   (intern (concat "googleaccount-"
                                   (googleaccount-uncamelcase-string 
                                    (cdr (assoc-string "Error" rr t))))))
             rr)))))


(defun googleaccount-handle-captcha (login-response)
  "Handle a CAPTCHA response from Google ClientLogin.
Offers user to display the CAPTCHA unlock page: if user refuses,
signal an error, otherwise try to show it with `browse-url'.

Argument LOGIN-RESPONSE should be the association list returned
by `googleaccount-login-response'.
"
  ;; FIXME: how to handle this in Emacs?
  (if (yes-or-no-p 
       "A CAPTCHA is required to unlock the account; do you want to visit the CAPTCHA URL now?")
      (progn
        (require 'browse-url)
        (browse-url googleaccount-captcha-unlock-url)
        (message "Login again when the CAPTCHA is unlocked."))
    (signal 'googleaccount-captcha-required
            (concat
             "A CAPTCHA is required to unlock the Google Account; visit "
             googleaccount-captcha-unlock-url
             " to unlock"))))


(defun googleaccount-authorized-p (login-response)
  "Return t if LOGIN-RESPONSE indicates successful authentication."
  (eq 'ok (assq 'status login-response)))


(defun googleaccount-captcha-p (login-response)
  "Return t if LOGIN-RESPONSE indicates that a CAPTCHA is required."
  (eq 'captcha (assq 'status login-response)))


(defun googleaccount-error-p (login-response)
  "Return t if LOGIN-RESPONSE indicates that Google reported an error."
  (eq 'error (assq 'status login-response)))


(defun googleaccount-auth-http-header (login-response)
  "Returns HTTP headers needed for authenticated Google service sessions.
Argument LOGIN-RESPONSE should be the association list returned
by `googleaccount-login-response'.

Return the pair of strings (AUTH-HEADER . AUTH-KEY), 
suitable for adding to `url-request-extra-headers'.
If LOGIN-RESPONSE contains no authorization tokens,
retuns nil."
  (lexical-let ((token (assoc "Auth" login-response)))
    (if token
        (cons "Authorization" 
              (format "GoogleLogin auth=%s" (cdr token))))))


(defun googleaccount-handle-error (login-response)
  "Signal the error condition appropriate to LOGIN-RESPONSE.
Argument LOGIN-RESPONSE should be an alist returned by
`googleaccount-login-response'."  
  (lexical-let ((err (cdr (assq 'google-error login-response)))
                (msg (googleaccount-error-message login-response)))
    (signal err msg)))


(defun googleaccount-error-message (login-response)
  "Return error message pertaining to LOGIN-RESPONSE."
  (lexical-let ((err (cdr (assq 'google-error login-response)))
                (errname (cdr (assoc-string "Error" login-response t)))
                (email (cdr (assoc-string "Email" login-response t)))
                (service (cdr (assoc-string "service" login-response t))))
    (format-spec
     (cdr (assq err
               ;; see http://code.google.com/apis/accounts/AuthForInstalledApps.html#Errors
               ;;
               ;; format specs:
               ;;   %u for email (user id)
               ;;   %s for service
               ;;
               '((googleaccount-bad-authentication .
                  "Authentication failed, either Email or password is incorrect")
                 (googleaccount-not-verified .
                  "The account email address has not been verified.  Access the Google account directly to resolve the issue before trying to log in again")
                 (googleaccount-terms-not-agreed .
                  "User of account %u has not agreed to terms. Access the Google account directly to resolve the issue before trying to log in again")
                 (googleaccount-captcha-required .
                  "A CAPTCHA is required")
                 (googleaccount-account-deleted .
                  "Google account `%u' has been deleted")
                 (googleaccount-account-disabled .
                  "Google account `%u' has been disabled")
                 (googleaccount-service-disabled .
                  "Account `%u' access to Google service has been disabled")
                 (googleaccount-service-unavailable .
                  "Google service `%s' is not available; try again later")
                 (googleaccount-unknown .
                  "Unspecified error accessing Google account `%u'")
                 (googleaccount-unexpected .
                   "Unexpected error accessing Google account `%u': `%n'"))))
     (list
      (cons ?n errname)
      (cons ?u email)
      (cons ?s service)))))

  
;; that's all folks!
(provide 'googleaccount)

;;; googleaccount.el ends here
