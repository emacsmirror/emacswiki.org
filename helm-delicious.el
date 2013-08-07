;;; helm-delicious.el --- helm extensions for delicious bookmarks

;; Filename: helm-delicious.el
;; Description: 
;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyright (C) 2008, 2009 Thierry Volpiatto, all rights reserved
;; Version: 1.1
;; Last-Updated: 07/08/2013 01:28:00
;; URL: https://github.com/vapniks/helm-delicious
;; Keywords: 
;; Compatibility: Gnus Emacs 24.3
;;
;; Features that might be required by this library:
;;
;; `helm' `xml'
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;  ==========
;;
;; Helm interface for Delicious bookmarks.
;; This code use curl and wget.
;; You need to install `helm' also.
;; Install:
;; =======
;;
;; Add to .emacs:
;; (require 'helm-delicious)
;;
;; after subscribing to http://delicious.com/
;; Setup your login and delicious password:
;;
;; You can set it up in your init file with
;;
;; `helm-delicious-user' and `helm-delicious-password'
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
;; DON'T CALL `helm-delicious-authentify', this will set your login and password
;; globally.
;;
;; Use:
;; ===
;;
;; M-x helm-delicious
;; That should create a "~/.delicious-cache" file.
;; (you can set that to another value with `helm-c-delicious-cache-file')
;; You can also add `helm-c-source-delicious-tv' to the `helm-sources'.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; Code:

(require 'xml)

;; User variables
(defvar helm-c-delicious-api-url
  "https://api.del.icio.us/v1/posts/all?"
  "Url used to retrieve all bookmarks")
(defvar helm-c-delicious-api-url-delete
  "https://api.del.icio.us/v1/posts/delete?&url=%s"
  "Url used to delete bookmarks from delicious")
(defvar helm-c-delicious-api-url-add
  "https://api.del.icio.us/v1/posts/add?&url=%s&description=%s&tags=%s"
  "Url used to add bookmarks to delicious")
(defvar helm-c-delicious-cache-file "~/.delicious.cache")
(defvar helm-delicious-user nil
  "Your Delicious login")
(defvar helm-delicious-password nil
  "Your Delicious password")

;; Faces
(defface helm-delicious-tag-face '((t (:foreground "VioletRed4" :weight bold)))
  "Face for w3m bookmarks" :group 'helm)

(defface helm-w3m-bookmarks-face '((t (:foreground "cyan1" :underline t)))
  "Face for w3m bookmarks" :group 'helm)

;; Internal variables (don't modify)
(defvar helm-c-delicious-cache nil)
(defvar helm-delicious-last-candidate-to-deletion nil)
(defvar helm-delicious-last-pattern nil)
                                     
(defvar helm-c-source-delicious-tv
  '((name . "Del.icio.us")
    (init . (lambda ()
              (unless helm-c-delicious-cache
                (setq helm-c-delicious-cache
                      (helm-set-up-delicious-bookmarks-alist)))))
    (candidates . (lambda () (mapcar #'car helm-c-delicious-cache)))
    (candidate-transformer helm-c-highlight-delicious-bookmarks)
    (action . (("Browse Url default" . (lambda (elm)
                                 (helm-c-delicious-browse-bookmark elm)
                                 (setq helm-delicious-last-pattern helm-pattern)))
               ("Browse Url Firefox" . (lambda (candidate)
                                         (helm-c-delicious-browse-bookmark candidate 'firefox)))
               ("Browse Url Chromium" . (lambda (candidate)
                                         (helm-c-delicious-browse-bookmark candidate 'chromium)))
               ("Browse Url w3m" . (lambda (candidate)
                                         (helm-c-delicious-browse-bookmark candidate 'w3m)
                                         (setq helm-delicious-last-pattern helm-pattern)))
               ("Delete bookmark" . (lambda (elm)
                                      (helm-c-delicious-delete-bookmark elm)))
               ("Copy Url" . (lambda (elm)
                               (kill-new (helm-c-delicious-bookmarks-get-value elm))))
               ("Update" . (lambda (elm)
                             (message "Wait Loading bookmarks from Delicious...")
                             (helm-wget-retrieve-delicious)))))))


;; (helm 'helm-c-source-delicious-tv)

(defvar helm-source-is-delicious nil)
(defadvice helm-select-action (before remember-helm-pattern () activate)
  "Remember helm-pattern when opening helm-action-buffer"
  (when helm-source-is-delicious
    (setq helm-delicious-last-pattern helm-pattern)))

(defun helm-delicious-remove-flag ()
  (setq helm-source-is-delicious nil))

(add-hook 'helm-cleanup-hook 'helm-delicious-remove-flag)

(defun helm-delicious-authentify ()
  "Authentify user from .authinfo file.
You have to setup correctly `auth-sources' to make this function
finding the path of your .authinfo file that is normally ~/.authinfo."
  (let ((helm-delicious-auth
         (auth-source-user-or-password  '("login" "password")
                                        "api.del.icio.us:443"
                                        "https")))
    (when helm-delicious-auth
      (setq helm-delicious-user (car helm-delicious-auth)
            helm-delicious-password (cadr helm-delicious-auth))
      nil)))

;;;###autoload
(defun helm-wget-retrieve-delicious (&optional sentinel)
  "Get the delicious bookmarks asynchronously with external program wget."
  (interactive)
  (let ((fmd-command "wget -q --no-check-certificate -O %s --user %s --password %s %s"))
    (unless (and helm-delicious-user helm-delicious-password)
      (helm-delicious-authentify))
    (message "Syncing with Delicious in Progress...")
    (start-process-shell-command
     "wget-retrieve-delicious" nil
     (format fmd-command
             helm-c-delicious-cache-file
             helm-delicious-user
             helm-delicious-password
             helm-c-delicious-api-url))
    (set-process-sentinel
     (get-process "wget-retrieve-delicious")
     (if sentinel
         sentinel
         #'(lambda (process event)
             (if (string= event "finished\n")
                 (message "Syncing with Delicious...Done.")
                 (message "Failed to synchronize with Delicious."))
             (setq helm-c-delicious-cache nil))))))


(defun helm-c-delicious-delete-bookmark (candidate &optional url-value-fn sentinel)
  "Delete delicious bookmark on the delicious side"
  (let* ((url     (if url-value-fn
                      (funcall url-value-fn candidate)
                      (helm-c-delicious-bookmarks-get-value candidate)))
         (url-api (format helm-c-delicious-api-url-delete
                          url))
         helm-delicious-user
         helm-delicious-password
         auth)
    (unless (and helm-delicious-user helm-delicious-password)
      (helm-delicious-authentify))
    (setq auth (concat helm-delicious-user ":" helm-delicious-password))
    (message "Wait sending request to delicious...")
    (setq helm-delicious-last-candidate-to-deletion candidate)
    (apply #'start-process "curl-delicious-delete" "*delicious-delete*" "curl"
                   (list "-u"
                         auth
                         url-api))
    (set-process-sentinel (get-process "curl-delicious-delete")
                          (or sentinel 'helm-delicious-delete-sentinel))))


(defun helm-delicious-delete-sentinel (process event)
  "Sentinel func for `helm-c-delicious-delete-bookmark'"
  (message "%s process is %s" process event)
  (sit-for 1)
  (with-current-buffer "*delicious-delete*"
    (goto-char (point-min))
    (if (re-search-forward "<result code=\"done\" />" nil t)
        (progn
          (helm-c-delicious-delete-bookmark-local
           helm-delicious-last-candidate-to-deletion)
          (setq helm-c-delicious-cache nil)
          (message "Ok %s have been deleted with success"
                   (substring-no-properties
                    helm-delicious-last-candidate-to-deletion)))
        (message "Fail to delete %s"
                 (substring-no-properties
                  helm-delicious-last-candidate-to-deletion)))
    (setq helm-delicious-last-candidate-to-deletion nil)))


(defun helm-c-delicious-delete-bookmark-local (candidate)
  "Delete delicious bookmark on the local side"
  (let ((cand (when (string-match "\\[.*\\]" candidate)
                (substring candidate (1+ (match-end 0))))))
    (with-current-buffer (find-file-noselect helm-c-delicious-cache-file)
      (goto-char (point-min))
      (when (re-search-forward cand (point-max) t)
        (beginning-of-line)
        (delete-region (point) (point-at-eol))
        (delete-blank-lines))
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun helm-set-up-delicious-bookmarks-alist ()
  "Setup an alist of all delicious bookmarks from xml file"
  (let ((gen-alist ())
        (tag-list ())
        (tag-len 0))
    (unless (file-exists-p helm-c-delicious-cache-file)
      (message "Wait Loading bookmarks from Delicious...")
      (helm-wget-retrieve-delicious))
    (setq tag-list (helm-delicious-get-all-tags-from-cache))
    (loop for i in tag-list
       for len = (length i) 
       when (> len tag-len) do (setq tag-len len))
    (with-temp-buffer
      (insert-file-contents helm-c-delicious-cache-file)
      (setq gen-alist (xml-get-children
                       (car (xml-parse-region (point-min)
                                              (point-max)))
                       'post)))
    (loop for i in gen-alist
       for tag = (xml-get-attribute i 'tag)
       for desc = (xml-get-attribute i 'description)
       for url = (xml-get-attribute i 'href)
       for interval = (- tag-len (length tag))
       collect (cons (concat "[" tag "]"
                             (make-string (+ 2 interval) ? ) desc)
                     url))))

;;;###autoload
(defun w3m-add-delicious-bookmark (description tag)
  "Add a bookmark to delicious from w3m"
  (interactive (list (read-from-minibuffer "Description: "
                                           nil nil nil nil
                                           w3m-current-title)
                     (completing-read "Tag: "
                                      (helm-delicious-get-all-tags-from-cache))))
  (setq description
        (replace-regexp-in-string " " "+" description))
  (let* ((url     w3m-current-url)
         (url-api (format helm-c-delicious-api-url-add url description tag))
         helm-delicious-user
         helm-delicious-password
         auth)
    (unless (and helm-delicious-user helm-delicious-password)
      (helm-delicious-authentify))
    (setq auth (concat helm-delicious-user ":" helm-delicious-password))
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
            (helm-wget-retrieve-delicious))
          (message "Fail to add bookmark to delicious")
          (when current-prefix-arg
            (if (y-or-n-p "Add anyway to w3m-bookmarks?")
                (progn
                  (w3m-bookmark-write-file url
                                           (replace-regexp-in-string "\+" " "
                                                                     description)
                                           tag)
                  (message "%s added to w3m-bookmarks" description))))))))

(defun helm-delicious-get-all-tags-from-cache ()
  "Get the list of all your tags from Delicious
That is used for completion on tags when adding bookmarks
to Delicious"
  (with-current-buffer (find-file-noselect helm-c-delicious-cache-file)
    (goto-char (point-min))
    (let* ((all (car (xml-parse-region (point-min) (point-max))))
           (tag (xml-get-children all 'post))
           tag-list)
      (dolist (i tag)
        (let ((tg (xml-get-attribute i 'tag)))
          (unless (member tg tag-list) (push tg tag-list))))
      (kill-buffer)
      tag-list)))

(defun helm-c-delicious-bookmarks-get-value (elm)
  "Get the value of key elm from alist"
  (replace-regexp-in-string
   "\"" "" (cdr (assoc elm helm-c-delicious-cache))))

(defun helm-c-delicious-browse-bookmark (elm &optional browser new-tab)
  "Action function for helm-delicious"
  (let* ((fn (case browser
               (firefox 'browse-url-firefox)
               (chromium 'browse-url-chromium)
               (w3m 'w3m-browse-url)
               (t 'browse-url)))
         (arg (and (eq fn 'w3m-browse-url) new-tab)))
    (funcall fn (helm-c-delicious-bookmarks-get-value elm) arg)))

(defun helm-c-highlight-delicious-bookmarks (books)
  "Highlight all Delicious bookmarks"
  (let (tag rest-text)
    (loop for i in books
       when (string-match "\\[.*\\] *" i)
       collect (concat (propertize (match-string 0 i)
                                   'face 'helm-delicious-tag-face)
                       (propertize (substring i (match-end 0))
                                   'face 'helm-w3m-bookmarks-face
                                   'help-echo (helm-c-delicious-bookmarks-get-value i))))))

;;;###autoload
(defun helm-delicious ()
  "Start helm-delicious outside of main helm"
  (interactive)
  (setq helm-source-is-delicious t)
  (let ((rem-pattern (if helm-delicious-last-pattern
                         helm-delicious-last-pattern)))
    (helm 'helm-c-source-delicious-tv
              rem-pattern nil nil nil "*Helm Delicious*")))

(provide 'helm-delicious)


;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "helm-delicious.el" (buffer-name) (buffer-string) "update")

;;; helm-delicious.el ends here
