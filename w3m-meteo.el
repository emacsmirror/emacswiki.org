;;; w3m-meteo.el --- store weather-url in emacs


;; Author: Thierry Volpiatto

;; Copyright (C) 2008 Thierry Volpiatto
;;
;; this file is NOT part of GNU Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA


;; Created: Mon Mar 10 21:04:49 2008 +0100
;; Last modified: Mon Apr 21 20:50:48 2008 +0200

;; INSTALL:
;; =======

;; Of course you need w3m.
;; Put this file in your load path
;; Add to your .emacs.el:
;; (require 'w3m-meteo)
;; You can set a global key for w3m-frweather:
;; (global-set-key (kbd "<f7> w") 'w3m-frweather)
;; and a local key to store bookmarks:
;; (define-key w3m-mode-map (kbd "ma") 'w3m-meteo-bookmark-add)
;; (define-key w3m-mode-map (kbd "mk") 'w3m-meteo-bookmark-delete)

;; USAGE:
;; =====

;; Start with M-x `w3m-frweather' RET
;; On first start you have no bookmarks and you will be linked
;; to `w3m-meteo-initial-site', from there navigate to your favorite
;; meteo site and bookmark them with `w3m-meteo-bookmark'
;; you have now completion on your bookmarks with TAB

;; NOTE:
;; A file (see `w3m-cache-file') will be created when this file will be loaded
;; Normally you don't have to edit it manually.


;; Commentary:
;; A good alternative to w3m-weather for non Japonese users

;;; Code:

(require 'w3m)

(defgroup w3m-meteo nil
  "Provide a way to bookmark meteo site in emacs"
  :prefix "w3m-meteo"
  :group 'w3m)

(defcustom w3m-cache-file
  "~/.emacs.d/w3m-meteo-cache"
  "Default file to store bookmarks"
  :type 'string
  :group 'w3m-meteo)

(defcustom w3m-meteo-default-city
  "Toulon"
  "Your favorite city"
  :type 'string
  :group 'w3m-meteo)

(defcustom w3m-meteo-initial-site
  "http://fr.weather.yahoo.com/"
  "The site where you can start seaching;
however you can bookmark from any other web site
even if they are different from this one, thats just
to find an url on startup"
  :type 'string
  :group 'w3m-meteo)

(defvar w3m-meteo-cache (make-hash-table)
  "Init w3m-meteo hashtable")

(defun set-w3m-meteo ()
  "Load cache file or create cache file if don't exist"
  (save-excursion
    (if (file-exists-p w3m-cache-file)
        (load w3m-cache-file)
      (find-file w3m-cache-file)
      (goto-char (point-min))
      (erase-buffer)
      (insert ";;; w3m-meteo-cache -*- mode: emacs-lisp; coding: utf-8; -*-")
      (save-buffer)
      (quit-window))))

(set-w3m-meteo)

(defvar meteo-completing-read (if (and (boundp 'ido-mode)
                                      ido-mode)
                                 'ido-completing-read
                               'completing-read)
  "Idea and code stolen in `dvc'.See the doc of `completing-read'
for arguments of function `meteo-completing-read'")

(defun meteo-completing-read (&rest args)
  "Read a string in the minibuffer, with completion.
Set `meteo-completing-read' to determine which function to use.

See `completing-read' for a description of ARGS."
  (apply meteo-completing-read args))

;;;###autoload
(defmacro hash-has-key (key hash-table)
  "check if hash-table have key key
key here must be a symbol and not a string"
  `(let ((keys-list (hash-get-symbol-keys ,hash-table)))
     (if (memq ,key keys-list)
         t
       nil)))

;;;###autoload
(defmacro hash-get-symbol-keys (hash-table)
  "Get the list of all the keys in hash-table
keys are given under string form"
  `(let ((li-keys nil)
         (li-all (hash-get-items ,hash-table)))
     (setq li-keys (mapcar #'car li-all))
     li-keys))

;;;###autoload
(defmacro hash-get-items (hash-table)
  "Get the list of all keys/values of hash-table
values are given under string form"
  `(let ((li-items nil)) 
    (maphash #'(lambda (x y) (push (list x y) li-items))
             ,hash-table)
    li-items))

(defun get-meteo-keys ()
  "Get the list of all the keys in hash-table"
  (let ((li-keys nil)
        (str-li-keys nil)) 
    (maphash #'(lambda (x y) (push x li-keys))
             w3m-meteo-cache)
    (dolist (x li-keys)
      (push (symbol-name x) str-li-keys))
    str-li-keys))

;;;###autoload
(defun w3m-frweather (town)
  "Go to bookmarked url"
  (interactive (list (meteo-completing-read (format "Town (Default %s): " w3m-meteo-default-city)
                                      (get-meteo-keys)
                                      nil t nil 'minibuffer-history w3m-meteo-default-city)))
  (if (not (gethash (intern town) w3m-meteo-cache))
      (catch 'goto-url
        (w3m-browse-url (gethash (intern w3m-meteo-default-city) w3m-meteo-cache))
        (throw 'goto-url
               (progn
                 (w3m-browse-url w3m-meteo-initial-site)
                 (message "Find here a town and bookmark it with M-x W3m-meteo-bookmark"))))
    (w3m-browse-url (gethash (intern town) w3m-meteo-cache))))

;;;###autoload
(defun w3m-meteo-bookmark-add (town-name)
  "Bookmark current url as town-name"
  (interactive "sAddTownName: ")
  (if (not (hash-has-key (intern town-name) w3m-meteo-cache))
      (progn
        (if (equal (current-buffer)
                   (get-buffer "*w3m*"))
            (progn
              (save-excursion
                (let ((url-meteo w3m-current-url))
                  (unless (equal town-name "")
                    (find-file w3m-cache-file)
                    (goto-char (point-max))
                    (when (looking-back "\)")
                      (newline))
                    (insert (concat "(puthash " "\'" town-name " \"" url-meteo "\"" " w3m-meteo-cache)"))
                    (save-buffer)))
                (quit-window)
                (load w3m-cache-file))
              (display-buffer "*w3m*"))
          (error "We are not in w3m!")))
    (error "Bookmark already exist!")))

;;;###autoload
(defun w3m-meteo-bookmark-delete (town-name)
  "Delete meteo bookmark" 
  (interactive (list (meteo-completing-read "DeleteTownName: "
                                      (get-meteo-keys)
                                      nil t nil t w3m-meteo-default-city)))
  (if (equal (current-buffer)
             (get-buffer "*w3m*"))
      (progn
        (save-excursion
          (let (beg
                end)
            (unless (equal town-name "")
              (find-file w3m-cache-file)
              (goto-char (point-min))
              (when (re-search-forward town-name)
                (end-of-line)
                (setq end (point))
                (beginning-of-line)
                (setq beg (point)))
              (delete-region beg end)
              (delete-blank-lines)
              (goto-char (point-max))
              (save-buffer)
              (kill-buffer (current-buffer))
              (remhash (intern town-name) w3m-meteo-cache)))
          (display-buffer "*w3m*")))
    (error "We are not in w3m!")))
            

(provide 'w3m-meteo)

;;; w3m-meteo ends here

