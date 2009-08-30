;;; amigo.el --- interface with friendfeed.com

;; Copyright (c) 2008 Andrew Gwozdziewycz
;; Author: Andrew Gwozdziewycz <web@apgwoz.com>
;; Created: 2008.7.9
;; Keywords: comm, friendfeed
;; Version: %Id: 1%

;; code originally based on twit.el by Theron Tlax and Jonathan Arkell
;; http://www.emacswiki.org/cgi-bin/emacs/twit.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, 
;; see http://www.gnu.org/licenses/gpl.txt, or write to the 
;; Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;; This mode aims to make it easy to interact with FriendFeed.com through
;; GNU Emacs. It will probably work at some point, but if it breaks feel
;; free to keep both pieces.

;; Currently, amigo will allow you to post a message or a link to FriendFeed
;; but does not read or display your friend feed.

;; This mode _will_ require json.el which is obtainable in the Emacs CVS, or
;; http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs
;; I believe json.el is also part of more recent Emacs (since it's in CVS)
;; but Carbon Emacs seems to not have it at this time.

;;(require 'json)
(require 'url)
(require 'url-http)

(defvar amigo-version-number "0.0.1")

(defvar amigo-status-mode-map (make-sparse-keymap))
(defvar amigo-followers-mode-map (make-sparse-keymap))
 
; 'r' key for reloading/refreshing the buffer
;(define-key amigo-feed-mode-map "r" 'amigo-update-feed)
;(dolist (info '(("s" . amigo-update-feed)
;                ("p" . amigo-share)
;  (define-key amigo-status-mode-map (car info) (cdr info))
;  (define-key amigo-followers-mode-map (car info) (cdr info)))


;; Everything uses this as a base
(defvar amigo-base-url "http://friendfeed.com/api")

(defconst amigo-home-feed-url
  (concat amigo-base-url "/feed/home?format=json"))
(defconst amigo-share-url
  (concat amigo-base-url "/share"))

(defconst amigo-share-success-msg 
  "Ok, I think it's shared.")
(defconst amigo-share-fail-msg
  "Failed to share that item.")
(defconst amigo-message-too-long-count 300)
(defconst amigo-message-too-long-msg
  (format "Message must be less than %d characters" amigo-message-too-long-count))
(defconst amigo-link-too-long-count 512)
(defconst amigo-link-too-long-msg
  (format "Link must be less than ~d characters" amigo-link-too-long-count))

(defgroup amigo nil
  "amigo.el customizations"
  :version "0.1"
  :group 'amigo)

(defcustom amigo-user
  ""
  "Your friendfeed username."
  :group 'amigo
  :type 'string)

(defcustom amigo-pass
  ""
  "Your friendfeed API auth token."
  :group 'amigo
  :type 'string)

;;; Authentication
(if (and (not (assoc "friendfeed.com:80" (symbol-value url-basic-auth-storage)))
         (not (string= amigo-pass ""))
         (not (string= amigo-user "")))
	(progn
         (set url-basic-auth-storage
              (cons (list "friendfeed.com:80"
                          (cons "FriendFeed API"
                                (base64-encode-string (format "%s:%s" amigo-user amigo-pass))))
                    (symbol-value url-basic-auth-storage)))))


(defun amigo-share-function (url post)
  "Only allows sharing messages and links"
  (let ((url-request-method "POST")
        (url-request-data post)
        (url-request-extra-headers `(("Connection" . "close")
                                     ("Content-Type" . "application/x-www-form-urlencoded")
                                     ("User-Agent" . "amigo.el"))))
    (message "%s" url-request-data)
    (url-retrieve url (lambda (arg) (message "%s" arg) (kill-buffer (current-buffer))))))

(defun amigo-query-for-input (prompt)
  "Query for post text in the minibuffer."
  (read-string prompt))

(defun amigo-make-share-payload (message link)
  "Sets up the post data by urlencoding it first and concatenating that
which is needed to form the payload"
  (let ((hex-message (url-hexify-string message))
        (hex-link (url-hexify-string link)))
    (concat "title=" hex-message)
            (if (> (length hex-link) 0)
                (concat "&link=" hex-link)
              "")))

;;; Interactives
(defun amigo-share ()
  "Shares something with friendfeed.
Prompt the first time for auth-key and username \(unless
`amigo-user' and/or `amigo-pass' is set\) and for message and a link
Messages have to be <= 300 characters. A link has to be <= 512
characters"
  (interactive)
  (let* ((message (amigo-query-for-input "Message: "))
         (link (amigo-query-for-input "Link (blank for none): ")))
    (if (> (length message) amigo-message-too-long-count)
        (error amigo-message-too-long-msg)
      (if (> (length message) 0)
          (if (> (length link) amigo-link-too-long-count)
              (error amigo-link-too-long-msg)
            (if (amigo-share-function amigo-share-url 
                                      (amigo-make-share-payload message link))
                (message amigo-share-success-msg)))))))

(defun amigo-share-region (start end)
  " Sends the region as a message to friendfeed.
Uses `amigo-share-function'"
  (interactive "r")
  (let ((message (buffer-substring start end)))
    (if (> (length post) amigo-message-too-long-count)
        (error amigo-message-too-long-msg)
      (if (amigo-share-function amigo-share-url 
                                (amigo-make-share-payload message link))
          (message amigo-share-success-msg)))))

(defun amigo-share-buffer ()
  "Sends the buffer contents as a message to 
friendfeed via `amigo-share-function'"
  (interactive "r")
  (let ((message (buffer-substring (point-min) (point-max))))
    (if (> (length post) amigo-message-too-long-count)
        (error amigo-message-too-long-msg)
      (if (amigo-share-function amigo-share-url 
                                (amigo-make-share-payload message link))
          (message amigo-share-success-msg)))))

(provide 'amigo)

;;; amigo.el ends here
