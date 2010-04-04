;; Copyright 2008, 2009  Neil Roberts, 2010 Chris Done
;;
;; PURPOSE: Load Haskell files using normal
;; inferior-haskell-load-file, but detect when there is an error, grab
;; it, format it, and tweet it.
;; 
;; Stole some code from here:
;;  http://git.busydoingnothing.co.uk/cgit.cgi/twitter.git/plain/twitter.el?id=HEAD
;; So I have to include the GPL license. Grumble.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;

;; USAGE:
;; 
;; Like so:
;;   (inferior-haskell-load-file-tweet-errors "<email/username>" "<password>")
;; 
;; I tend to bind inferior-haskell-load-file to F5 (rather than C-c C-l), so:
;;
;; (add-hook 'haskell-mode-hook
;;           '(lambda ()
;;              (load "haskell-tweet-errors.el")
;;              (define-key haskell-mode-map [f5]
;;                (lambda ()
;;                  (interactive)
;;                  (inferior-haskell-load-file-tweet-errors
;;                   "<username>" "<password>")))))

(require 'url)
(require 'url-http)

(defun inferior-haskell-load-file-tweet-errors (username password)
  (interactive)
  (save-excursion
    (inferior-haskell-load-file)
    (inferior-haskell-wait-for-prompt (inferior-haskell-process))
    (set-buffer (process-buffer (inferior-haskell-process)))
    (let ((msg (inferior-haskell-ghc-type-error)))
      (if msg
          (let ((type-error (twitter-limit-string
                             (inferior-haskell-clean-error-msg msg))))
            (twitter-status-post username password type-error))))))

(defun twitter-limit-string (str)
  (if (> (length str) 140)
      (substring str 0 140)
    str))

(defun inferior-haskell-ghc-type-error ()
  "Grab the type error from GHC's error messages."
  ;; Example below. Annotated with the places Emacs jumps to in order to
  ;; find the last error. Code is annotated with numbers to match up
  ;; this order of navigation.

  ;; Haskell>(1)(3) :load \"Foo.hs\"
  ;; [1 of 1] Compiling Main             ( /home/chris/Foo.hs, interpreted )

  ;; /home/chris/Foo.hs:74:15:
  ;; (4)    Kind mis-match
  ;;     Expected kind `* -> *', but `Char' has kind `*'
  ;;     In the instance declaration for `Monad Char'
  ;; Failed(2)(5), modules loaded: none.
  ;; (0.01 secs, 4787872 bytes)
  (when (and (search-backward-regexp "^[a-zA-Z0-9][^>]+>" nil t)     ;; (1)
             (search-forward-regexp "Failed" nil t)                  ;; (2)
             (search-backward-regexp "^[a-zA-Z0-9][^>]+>" nil t)     ;; (3)
             (search-forward-regexp "[^:]+:[0-9]+:[0-9]+:\n" nil t)) ;; (4)
    (let ((start-point (point)))
      (when (search-forward-regexp "^Failed" nil t)                  ;; (5)
        (backward-word)
        (buffer-substring-no-properties start-point (point))))))

(defun inferior-haskell-clean-error-msg (msg)
  (replace-regexp-in-string
   "[ ]+" " " 
   (replace-regexp-in-string
    "^[ \r\n]*\\(.*\\)[ \r\n]*" "\\1"
    (replace-regexp-in-string
     "[\r\n]+\\(.+\\)" "; \\1"
     msg))))

;;; Code below stolen and mangled from twitter.el.

(defun twitter-retrieve-url (twitter-username twitter-password url cb)
  (when (and twitter-username twitter-password)
    (let ((server-cons
           (or (assoc "twitter.com:80" url-http-real-basic-auth-storage)
               (car (push (cons "twitter.com:80" nil)
                          url-http-real-basic-auth-storage)))))
      (unless (assoc "Twitter API" server-cons)
        (setcdr server-cons
                (cons (cons "Twitter API"
                            (base64-encode-string
                             (concat twitter-username
                                     ":" twitter-password)))
                      (cdr server-cons))))))
  (url-retrieve url cb nil))

(defun twitter-status-post (username password status)
  (interactive)
  (let ((url-request-method "POST")
        (url-request-data (concat "status="
                                  (url-hexify-string status))))
    (twitter-retrieve-url
     username
     password
     "http://twitter.com/statuses/update.xml"
     'twitter-status-callback)))

(defun twitter-status-callback (status)
  (let ((errmsg (plist-get status :error)))
    (when errmsg
      (signal (car errmsg) (cdr errmsg)))
    (message "Tweeted. Now the world knows about your type error!")))
