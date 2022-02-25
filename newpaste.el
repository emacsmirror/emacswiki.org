;;; newpaste.el --- Paste directly from Emacs to http://paste.lisp.org using Emacs `url' library

;; Copyright (C) 2012  Max Mikhanosha

;; Author: Max Mikhanosha <max@momoland.openchat.com>
;; Keywords: 

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

;; Usage:
;;
;; 1) Select a region
;; 2) M-x newpaste
;; 

;;; Code:

(require 'cl)
(require 'url)

(defvar newpaste-channel nil
  "Default channel")
(defvar newpaste-username nil
  "Default username. Still will be asked if `newpaste-confirm-username' is set")
(defvar newpaste-title nil)
(defvar newpaste-expiration nil)

(defvar newpaste-ask-username t
  "When non-NIL paste will ask for username, even if its already
  known because of previous paste")

(defvar newpaste-ask-expiration t
  "When non-NIL ask for expiration")

(defvar newpaste-ask-title t
  "When non-NIL ask for title")

(defvar newpaste-ask-channel nil
  "When non-NIL ask for channel")

(defvar newpaste-ask-colorization t
  "When non-NIL will ask for colorization")

(defvar newpaste-debug nil
  "When non-nil will leave buffers *newpaste-get* and
*newpaste-post* behind")


(defvar newpaste-colorizations-list
  '("None"
    "Basic Lisp"
    "Scheme"
    "Emacs Lisp"
    "Common Lisp"
    "C"
    "C++"
    "Java"
    "Objective C"
    "Erlang"
    "Python"
    "Haskell"
    "Unified Context Diff"
    "WebKit (text or diff)"))

(defvar newpaste-expiration-list
  '("Never expires (recommended)"
    "One hour"
    "One day"
    "One week"
    "Four weeks"))

(defvar newpaste-mode-colorization-alist
  '((emacs-lisp-mode . "Emacs Lisp")
    (lisp-mode . "Common Lisp")
    (slime-repl-mode . "Common Lisp")
    (c-mode . "C")
    (c++-mode . "C++")
    (java-mode . "Java")))

(defun newpaste-ask (prompt default choices)
  "Ask user for a string value, offering DEFAULT as choice. If
CHOICES is set then completing read is used, and input must match
one of them"
  (if choices
      (completing-read prompt choices nil t nil nil default)
    (read-string prompt default)))

(defun newpaste-maybe-ask-for (prompt ask-p defaults &optional choices)
  "When ASK-P is non-NIL ask user for the value. Use first of the
DEFAULTS that is non-nil as default.

When ASK-P is NIL return the first non-NIL value in DEFAULTs or
show error

When CHOICES is non-NIL then `completing-read' is used, and input must match
one of the choices"
  (let ((default (some #'identity defaults)))
    (if ask-p (newpaste-ask prompt default choices)
      (or default (newpaste-ask prompt default choices)))))

(defun newpaste-guess-colorization (text text-major-mode)
  (or (cdr (assoc text-major-mode newpaste-mode-colorization-alist))
      (and (string-match "defun" text) "Basic Lisp")
      (and (string-match "void" text) "C++")))

(defun newpaste-paste-internal (text &optional text-major-mode)
  (let* ((url-cookie-trusted-urls '("http://paste.lisp.org/.*"))
         (buffer (url-retrieve-synchronously "http://paste.lisp.org/new")))
    (with-current-buffer buffer
      (when newpaste-debug
        (when (get-buffer "*newpaste-get*")
          (kill-buffer "*newpaste-get*"))
        (rename-buffer "*newpaste-get*"))
      (let ((inputs '())
            (captcha-answer)
            (outputs))
        (goto-char (point-min))
        (while (re-search-forward "<input " nil t)
          (let ((attrs '()))
            (while (looking-at "[ \t\n]*\\([a-z]+\\)=\"\\([^\"]+\\)\"")
              (let ((attr-name (match-string 1))
                    (attr-value (match-string 2)))
                (push attr-value attrs)
                (push (intern (format ":%s" attr-name)) attrs))
              (goto-char (1+ (match-end 0))))
            (push attrs inputs)))
        (goto-char (point-min))
        (when (re-search-forward "What do you get when you multiply \\([0-9]+\\) by \\([0-9]+\\)\\?"
                                 nil t)
          (setq captcha-answer (* (string-to-number (match-string 1))
                                  (string-to-number (match-string 2)))))
        (dolist (input inputs)
          (cond ((equal "reset" (getf input :type)))
                ((equal "submit" (getf input :type))
                 (push (cons "submit" (getf input :value)) outputs))
                ((equal "username" (getf input :name))
                 (push (cons "username"
                             (newpaste-maybe-ask-for
                              "Username: " newpaste-ask-username
                              (list
                               (getf input :value)
                               newpaste-username)))
                       outputs))
                ((equal "channel" (getf input :name))
                 (push (cons "channel"
                             (newpaste-maybe-ask-for
                              "Channel: " newpaste-ask-channel
                              (list
                               newpaste-channel
                               (getf input :value))))
                       outputs))
                ((equal "captchaid" (getf input :name))
                 (push (cons "captchaid" (getf input :value))
                       outputs))
                ((equal "title" (getf input :name))
                 (push (cons "title" (newpaste-maybe-ask-for
                                      "Title: " newpaste-ask-title
                                      (list
                                       newpaste-title
                                       (getf input :value))))
                       outputs))
                ((equal "captcha" (getf input :name))
                 (or captcha-answer (error "Unable to find captcha"))
                 (push (cons "captcha" (format "%d" captcha-answer)) outputs))))
        (push (cons "text" text) outputs)
        (push (cons "colorize"
                    (newpaste-maybe-ask-for
                     "Colorize as: " newpaste-ask-colorization
                     (list
                      (newpaste-guess-colorization text text-major-mode))
                     newpaste-colorizations-list))
              outputs)
        (push (cons "expiration"
                    (newpaste-maybe-ask-for
                     "Expiration: " newpaste-ask-expiration
                     newpaste-expiration-list
                     newpaste-expiration-list)) outputs)
        (unless newpaste-debug
          (kill-buffer))
        (let ((url-request-method "POST")
              (url-request-extra-headers
               '(("Content-Type" . "application/x-www-form-urlencoded")))
              (url-request-data
               (mapconcat (lambda (arg)
                            (concat (url-hexify-string (car arg))
                                    "="
                                    (url-hexify-string (cdr arg))))
                          outputs
                          "&")))
          (with-current-buffer (url-retrieve-synchronously "http://paste.lisp.org/submit")
            (when newpaste-debug
              (when (get-buffer "*newpaste-post*")
                (kill-buffer "*newpaste-post*"))
              (rename-buffer "*newpaste-post*"))
            (goto-char (point-min))
            (let ((paste-num
                   (when (re-search-forward "Paste number \\([0-9]+\\) pasted" nil t)
                     (match-string 1))))
              (if paste-num
                  (let ((url (format "http://paste.lisp.org/display/%s" paste-num)))
                    (kill-new url)
                    (unless newpaste-debug
                      (kill-buffer))
                    (message url))
                (message "Unable to paste, check *paste-post* buffer for errors")
                (unless newpaste-debug
                  (when (get-buffer "*paste-post*")
                    (kill-buffer "*paste-post*")
                    (rename-buffer "*paste-post*")))))))))))

(defun newpaste (beg end)
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (when (string-match "\\`[ \t\n]*\\'" text)
      (error "Can't paste empty string"))
    (newpaste-paste-internal text major-mode)))

(provide 'newpaste)
;;; newpaste.el ends here

