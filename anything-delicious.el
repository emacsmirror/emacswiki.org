;;; anything-delicious.el --- 

;; Copyright (C) 2008, 2009 Thierry Volpiatto, all rights reserved

;; Filename: anything-delicious.el
;; Description: 
;; Author: thierry
;; Maintainer: 
;; URL: 
;; Keywords: 
;; Compatibility: 
 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;  ==========
;;
;; Anything interface for Delicious bookmarks.
;; This code use curl and wget.
;; You need to install `anything' also.
;; Install:
;; =======
;;
;; Add to .emacs:
;; (require 'anything-delicious)
;;
;; after subscribing to http://delicious.com/
;; Setup your login and delicious password:
;;
;; You can set it up in your init file with
;;
;; `anything-delicious-user' and `anything-delicious-password'
;; (use setq)
;;
;; or better:
;;
;; Add a line like this in your .authinfo file:
;;
;; machine api.del.icio.us:443 port https login xxxxx password xxxxx
;;
;; and add to you init file (.emacs):
;; (require 'auth-source)
;;
;; (if (file-exists-p "~/.authinfo.gpg")
;;     (setq auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t)))
;;     (setq auth-sources '((:source "~/.authinfo" :host t :protocol t))))
;;
;; Warning:
;;
;; DON'T CALL `anything-delicious-authentify', this will set your login and password
;; globally.
;;
;; Use:
;; ===
;;
;; M-x anything-delicious
;; That should create a "~/.delicious-cache" file.
;; (you can set that to another value with `anything-c-delicious-cache-file')
;; You can also add `anything-c-source-delicious-tv' to the `anything-sources'.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; Code:

(require 'xml)

;; User variables
(defvar anything-c-delicious-api-url
  "https://api.del.icio.us/posts/all"
  "Url used to retrieve all bookmarks")
(defvar anything-c-delicious-api-url-delete
  "https://api.del.icio.us/v1/posts/delete?&url=%s"
  "Url used to delete bookmarks from delicious")
(defvar anything-c-delicious-api-url-add
  "https://api.del.icio.us/v1/posts/add?&url=%s&description=%s&tags=%s"
  "Url used to add bookmarks to delicious")
(defvar anything-c-delicious-cache-file "~/.delicious.cache")
(defvar anything-delicious-user nil
  "Your Delicious login")
(defvar anything-delicious-password nil
  "Your Delicious password")

;; Faces
(defface anything-delicious-tag-face '((t (:foreground "VioletRed4" :weight bold)))
  "Face for w3m bookmarks" :group 'anything)

(defface anything-w3m-bookmarks-face '((t (:foreground "cyan1" :underline t)))
  "Face for w3m bookmarks" :group 'anything)

;; Internal variables (don't modify)
(defvar anything-c-delicious-cache nil)
(defvar anything-delicious-last-candidate-to-deletion nil)
(defvar anything-delicious-last-pattern nil)
                                     
(defvar anything-c-source-delicious-tv
  '((name . "Del.icio.us")
    (init . (lambda ()
              (unless anything-c-delicious-cache
                (setq anything-c-delicious-cache
                      (anything-set-up-delicious-bookmarks-alist)))))
    (candidates . (lambda ()
                    (mapcar #'car
                            anything-c-delicious-cache)))
    (candidate-transformer anything-c-highlight-delicious-bookmarks)
    (action . (("Browse Url" . (lambda (elm)
                                 (anything-c-delicious-browse-bookmark elm)
                                 (setq anything-delicious-last-pattern anything-pattern)))
               ("Copy Url" . (lambda (elm)
                               (kill-new (anything-c-delicious-bookmarks-get-value elm))))
               ("Delete bookmark" . (lambda (elm)
                                      (anything-c-delicious-delete-bookmark elm)))
               ("Update" . (lambda (elm)
                             (message "Wait Loading bookmarks from Delicious...")
                             (anything-wget-retrieve-delicious)))
               ("Browse Url Firefox" . (lambda (candidate)
                                         (anything-c-delicious-browse-bookmark candidate t)))))
    (delayed)))

;; (anything 'anything-c-source-delicious-tv)

(defvar anything-source-is-delicious nil)
(defadvice anything-select-action (before remember-anything-pattern () activate)
  "Remember anything-pattern when opening anything-action-buffer"
  (when anything-source-is-delicious
    (setq anything-delicious-last-pattern anything-pattern)))

(defun anything-delicious-remove-flag ()
  (setq anything-source-is-delicious nil))

(add-hook 'anything-cleanup-hook 'anything-delicious-remove-flag)

(defun anything-delicious-authentify ()
  "Authentify user from .authinfo file.
You have to setup correctly `auth-sources' to make this function
finding the path of your .authinfo file that is normally ~/.authinfo."
  (let ((anything-delicious-auth
         (auth-source-user-or-password  '("login" "password")
                                        "api.del.icio.us:443"
                                        "https")))
    (when anything-delicious-auth
      (setq anything-delicious-user (car anything-delicious-auth)
            anything-delicious-password (cadr anything-delicious-auth))
      nil)))

(defun anything-wget-retrieve-delicious ()
  "Get the delicious bookmarks asynchronously
with external program wget"
  (interactive)
  (let (anything-delicious-user anything-delicious-password)
    (unless (and anything-delicious-user anything-delicious-password)
      (anything-delicious-authentify))
    (start-process-shell-command "wget-retrieve-delicious" nil "wget"
                                 (format "-q -O %s --user %s --password %s %s"
                                         anything-c-delicious-cache-file
                                         anything-delicious-user
                                         anything-delicious-password
                                         anything-c-delicious-api-url))
    (set-process-sentinel (get-process "wget-retrieve-delicious")
                          #'(lambda (process event)
                              (message
                               "%s is %s Delicious bookmarks should be up to date!"
                               process
                               event)
                              (setq anything-c-delicious-cache nil)))))


(defun anything-c-delicious-delete-bookmark (candidate)
  "Delete delicious bookmark on the delicious side"
  (let* ((url     (anything-c-delicious-bookmarks-get-value candidate))
         (url-api (format anything-c-delicious-api-url-delete
                          url))
         anything-delicious-user
         anything-delicious-password
         auth)
    (unless (and anything-delicious-user anything-delicious-password)
      (anything-delicious-authentify))
    (setq auth (concat anything-delicious-user ":" anything-delicious-password))
    (message "Wait sending request to delicious...")
    (setq anything-delicious-last-candidate-to-deletion candidate)
    (apply #'start-process "curl-delicious-delete" "*delicious-delete*" "curl"
                   (list "-u"
                         auth
                         url-api))
    (set-process-sentinel (get-process "curl-delicious-delete")
                          'anything-delicious-delete-sentinel)))


(defun anything-delicious-delete-sentinel (process event)
  "Sentinel func for `anything-c-delicious-delete-bookmark'"
  (message "%s process is %s" process event)
  (sit-for 1)
  (with-current-buffer "*delicious-delete*"
    (goto-char (point-min))
    (if (re-search-forward "<result code=\"done\" />" nil t)
        (progn
          (anything-c-delicious-delete-bookmark-local
           anything-delicious-last-candidate-to-deletion)
          (setq anything-c-delicious-cache nil)
          (message "Ok %s have been deleted with success"
                   (substring-no-properties
                    anything-delicious-last-candidate-to-deletion)))
        (message "Fail to delete %s"
                 (substring-no-properties
                  anything-delicious-last-candidate-to-deletion)))
    (setq anything-delicious-last-candidate-to-deletion nil)))


(defun anything-c-delicious-delete-bookmark-local (candidate)
  "Delete delicious bookmark on the local side"
  (let ((cand (when (string-match "\\[.*\\]" candidate)
                (substring candidate (1+ (match-end 0))))))
    (save-excursion
      (find-file anything-c-delicious-cache-file)
      (goto-char (point-min))
      (when (re-search-forward cand)
        (beginning-of-line)
        (delete-region (point) (point-at-eol))
        (delete-blank-lines))
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun anything-set-up-delicious-bookmarks-alist ()
  "Setup an alist of all delicious bookmarks from xml file"
  (let ((gen-alist   nil)
        (final-alist nil))
    (unless (file-exists-p anything-c-delicious-cache-file)
      (message "Wait Loading bookmarks from Delicious...")
      (anything-wget-retrieve-delicious))
    (with-temp-buffer
      (insert-file-contents anything-c-delicious-cache-file)
      (setq gen-alist (xml-get-children
                       (car (xml-parse-region (point-min)
                                              (point-max)))
                       'post)))
    (setq final-alist
          (loop for i in gen-alist
             collect (cons (concat "["
                                   (xml-get-attribute i 'tag)
                                   "] "
                                   (xml-get-attribute i 'description))
                           (xml-get-attribute i 'href))))))


(defun w3m-add-delicious-bookmark (description tag)
  "Add a bookmark to delicious from w3m"
  (interactive (list (read-from-minibuffer "Description: "
                                           nil nil nil nil
                                           w3m-current-title)
                     (completing-read "Tag: "
                                      (anything-delicious-get-all-tags-from-cache))))
  (setq description
        (replace-regexp-in-string " " "+" description))
  (let* ((url     w3m-current-url)
         (url-api (format anything-c-delicious-api-url-add
                          url
                          description
                          tag))
         anything-delicious-user
         anything-delicious-password
         auth)
    (unless (and anything-delicious-user anything-delicious-password)
      (anything-delicious-authentify))
    (setq auth (concat anything-delicious-user ":" anything-delicious-password))
    (with-temp-buffer
      (apply #'call-process "curl" nil t nil
             `("-u"
               ,auth
               ,url-api))
      (buffer-string)
      (goto-char (point-min))
      (if (re-search-forward "<result code=\"done\" />" nil t)
          (unwind-protect
               (progn
                 (message "%s added to delicious" description)
                 (when current-prefix-arg
                   (w3m-bookmark-write-file url
                                            (replace-regexp-in-string "\+"
                                                                      " "
                                                                      description)
                                            tag)
                   (message "%s added to delicious and to w3m-bookmarks" description)))
            (anything-wget-retrieve-delicious))
          (message "Fail to add bookmark to delicious")
          (when current-prefix-arg
            (if (y-or-n-p "Add anyway to w3m-bookmarks?")
                (progn
                  (w3m-bookmark-write-file url
                                           (replace-regexp-in-string "\+" " "
                                                                     description)
                                           tag)
                  (message "%s added to w3m-bookmarks" description))))))))

(defun anything-delicious-get-all-tags-from-cache ()
  "Get the list of all your tags from Delicious
That is used for completion on tags when adding bookmarks
to Delicious"
  (save-excursion
    (goto-char (point-min))
    (find-file anything-c-delicious-cache-file)
    (let ((temp-tag-list)
          (tag-list)
          (tag (xml-get-children (car (xml-parse-region (point-min)
                                                        (point-max)))
                                 'post)))
      (dolist (i tag)
        (push (xml-get-attribute i 'tag) temp-tag-list))
      (save-buffer)
      (kill-buffer (current-buffer))
      (dolist (n temp-tag-list)
        (when (not (member n tag-list))
          (push n tag-list)))
      tag-list)))

(defun anything-c-delicious-bookmarks-get-value (elm)
  "Get the value of key elm from alist"
  (replace-regexp-in-string "\"" ""
                            (cdr (assoc elm
                                        anything-c-delicious-cache))))

(defun anything-c-delicious-browse-bookmark (elm &optional use-firefox new-tab)
  "Action function for anything-delicious"
  (let* ((fn  (if use-firefox
                'browse-url-firefox
                'w3m-browse-url))
         (arg (if (and (eq fn 'w3m-browse-url)
                       new-tab)
                  t
                  nil)))
    (funcall fn (anything-c-delicious-bookmarks-get-value elm) arg)))

(defun anything-c-highlight-delicious-bookmarks (books)
  "Highlight all Delicious bookmarks"
  (let* (tag
         rest-text
         (cand-mod (loop for i in books
                        do
                        (setq tag (progn
                                    (string-match "\\[.*\\]" i)
                                    (match-string 0 i))
                              rest-text (substring i (match-end 0)))
                      collect (concat (propertize tag
                                          'face 'anything-delicious-tag-face)
                                      (propertize rest-text
                                                  'face 'anything-w3m-bookmarks-face
                                                  'help-echo (anything-c-delicious-bookmarks-get-value i))))))
    cand-mod))

(defun anything-delicious ()
  "Start anything-delicious outside of main anything"
  (interactive)
  (setq anything-source-is-delicious t)
  (let ((rem-pattern (if anything-delicious-last-pattern
                         anything-delicious-last-pattern)))
    (anything 'anything-c-source-delicious-tv rem-pattern)))

(provide 'anything-delicious)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (emacswiki-post "anything-delicious.el")
;;; anything-delicious.el ends here
