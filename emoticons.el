;;; emoticons.el --- Replace text with emoticons

;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 11 Apr 2008
;; Version: 0.01
;; Keywords: games, wp

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
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Icons is downloaded from http://www.adiumxtras.com/index.php?a=xtras&xtra_id=980
;; Extract the zip file and rename the directory `iChat-Complete.AdiumEmoticonSet'
;; to your favorite place, the default is $HOME/.emacs.d/icons/adium.
;; You can download the emoticons from
;; http://www.emacswiki.org/cgi-bin/wiki/Emoticons
;; Be sure rename the file to .tar.gz and extract it with
;;  $ tar zvxf Emoticons.tar.gz
;;
;; To turn on display image, just M-x emotions-mode and invoke the command again
;; to turn off.
;;
;;; Known BUGS:
;; 1. The same neighboring image will merge with previous. I can't
;;    remember which property can fix it?

;; Put this file into your load-path and the following into your ~/.emacs:
;; 
;;   (setq emotions-icon-dir "parent-directory-of-icons"
;;         emotions-theme "directory-of-icons")
;;   (require 'emotions)

;; To use it with ERC:
;; (add-to-list 'erc-replace-alist
;;       '(emoticons-regexp . emoticons-replace))
;;   
;; But there is something wrong I can fix, you have to redefine the
;; function `erc-replace-insert', so here is my configuration:
;; (eval-after-load "erc"
;;     '(progn
;;        (require 'emoticons)
;;        (erc-replace-mode t)
;;        (add-to-list 'erc-replace-alist
;;                     '(emoticons-regexp . emoticons-replace))
;;        (defun erc-replace-insert ()
;;          "Function to run from `erc-insert-modify-hook'.
;; It replaces text according to `erc-replace-alist'."
;;          (mapcar (lambda (elt)
;;                    (goto-char (point-min))
;;                    (let ((from (car elt))
;;                          (to (cdr elt)))
;;                      (unless (stringp from)
;;                        (setq from (eval from)))
;;                      (while (re-search-forward from nil t)
;;                        (cond ((stringp to)
;;                               (replace-match to nil t))
;;                              ((and (symbolp to) (fboundp to))
;;                               (replace-match (funcall to (match-string 0))
;;                                              nil t))
;;                              (t (eval to))))))
;;                  erc-replace-alist))))
;; Tips:
;; If you think the text is replace incorrectly, you can use
;; M-x emoticons-erase-region to remove the icons.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'tree-widget)
(require 'format-spec)
(require 'xml)

(defgroup emoticons nil
  "Display ASCII emoticons with icons"
  :group 'multimedia)

(defcustom emoticons-theme "adium"
  "Theme name for emoticons."
  :type 'string
  :group 'emoticons)

(defcustom emoticons-icon-dir "~/.emacs.d/icons"
  "Directory of icons."
  :type 'directory
  :group 'emoticons)

(defcustom emoticons-image-properties
  '(:ascent center)
  "Extra properties for the image to display"
  :type 'sexp
  :group 'emoticons)

(defcustom emoticons-help-echo-format "%e %n"
  "Format string for display help-echo"
  :type 'string
  :group 'emoticons)

(defvar emoticons-image-suffix-regexp
  (regexp-opt (apply 'append (mapcar 'cdr (tree-widget-image-formats))))
  "Regexp to match suffex of images")

;; ( ("theme-name" . (("name" . image))) )
(defvar emoticons--theme nil
  "Cache images that used.")

(defvar emoticons-mode nil)

(defun emoticons-parse-plist (file)
  "Read ASCII emoticons and icon files pair from xml file."
  (let* ((xml (xml-parse-file file))
         alist file)
    (dolist (node (xml-node-children (car (xml-get-children (car (xml-get-children (car xml) 'dict)) 'dict))))
      (when (listp node)
        (cond ((eq (xml-node-name node) 'key)
               (setq file (car (xml-node-children node))))
              ((eq (xml-node-name node) 'dict)
               (dolist (str (xml-get-children (car (xml-get-children node 'array)) 'string))
                 (push (cons (car (xml-node-children str)) file)
                       alist))))))
    alist))

(defvar emoticons-alist
  (let ((file (expand-file-name
               "Emoticons.plist"
               (concat (file-name-as-directory emoticons-icon-dir)
                       emoticons-theme))))
    (when (file-exists-p file)
      (emoticons-parse-plist file)))
  "A list to transform ASCII emoticons to icon file.")

(defvar emoticons-regexp
  (concat "\\(?:\\<\\|\\s-\\)" (regexp-opt (mapcar 'car emoticons-alist) t))
  "Regexp to match ASCII emoticons.")

(defvar emoticons-keywords 
  (list
   (list emoticons-regexp
         '(1 (add-text-properties
              (match-beginning 1) (match-end 1)
              (emoticons-text-properties (match-string 1))))))
  "Font-lock keywords to display emoticons.")

(defun emoticons-find-image (name)
  "Find the image with NAME in current theme"
  (let ((theme (assoc emoticons-theme emoticons--theme))
        image)
    (if (setq image (assoc name (cdr theme)))
        (cdr image)
      (let ((dir (concat (file-name-as-directory emoticons-icon-dir)
                         emoticons-theme))
            formats file)
        ;; if the name is a file name, find the format in
        ;; (tree-widget-image-formats)
        (if (string-match emoticons-image-suffix-regexp name)
            (catch 'found
              (mapc (lambda (fmt)
                      (dolist (f (cdr fmt))
                        (when (string-match (regexp-quote f) name)
                          (setq formats `((,(car fmt) "")))
                          (throw 'found t))))
                    (tree-widget-image-formats)))
          (setq formats (tree-widget-image-formats)))
        (setq image
              (catch 'found
                (dolist (fmt formats)
                  (dolist (ext (cdr fmt))
                    (setq file (expand-file-name (concat name ext) dir))
                    (and (file-readable-p file)
                         (file-regular-p file)
                         (throw 'found
                                (apply 'create-image file (car fmt) nil
                                       emoticons-image-properties)))))))
        (setq emoticons--theme
              (cons (cons emoticons-theme
                          (cons (cons name image) (cdr theme)))
                    (delq theme emoticons--theme)))
        image))))

(defun emoticons-text-properties (str)
  (let ((name (assoc-default str emoticons-alist)))
    (when name
      (list
       'display (emoticons-find-image name)
       'emoticons t
       'help-echo (format-spec emoticons-help-echo-format
                               (format-spec-make
                                ?e str
                                ?n name))))))

(defun emoticons-mode (&optional arg)
  "Toggle display emoticons."
  (interactive "P")
  (setq emoticons-mode
        (if (null arg) (not emoticons-mode)
          (> (prefix-numeric-value arg) 0)))
  (if emoticons-mode
      (progn
        (font-lock-add-keywords nil emoticons-keywords)
        (font-lock-fontify-buffer))
    (font-lock-remove-keywords nil emoticons-keywords)
    ;; remove images
    (save-restriction
      (widen)
      (emoticons-erase-region (point-min) (point-max)))))

(defun emoticons-erase-region (beg end)
  "Remove all icons in region."
  (interactive "r")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (save-restriction
      (narrow-to-region beg end)
      (let ((s (point-min))
            (end (point-max)))
        (if (get-text-property s 'display)
            (remove-text-properties s (setq s (or (next-single-property-change s 'emoticons)
                                                  (point-max)))
                                    (list 'display nil
                                          'emoticons nil
                                          'help-echo nil)))
        (while (and s (< s end))
          (setq s (next-single-property-change s 'emoticons))
          (when s
            (remove-text-properties
             s (setq s (or (next-single-property-change s 'emoticons)
                           (point-max)))
             (list 'display nil 'emoticons t 'help-echo nil))))))
    (set-buffer-modified-p modified)))

(defun emoticons-erase-buffer ()
  "Remove all icons in current buffer."
  (interactive)
  (emoticons-erase-region (point-min) (point-max)))

(defun emoticons-fill-region (beg end)
  "Substitue all ASCII emoticons in the region with icons."
  (interactive "r")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (re-search-forward emoticons-regexp nil t)
          (add-text-properties
           (match-beginning 1) (match-end 1)
           (emoticons-text-properties (match-string 1)))))
      (set-buffer-modified-p modified))))

(defun emoticons-fill-buffer ()
  "Substitue all ASCII emoticons in current buffer with icons."
  (interactive)
  (emoticons-fill-region (point-min) (point-max)))

(defun emoticons-replace (str)
  (save-match-data
    (string-match "^\\(\\s-*\\)" str)
    (concat (match-string 0 str)
            (let ((rest (substring str (match-end 0))))
              (apply 'propertize rest
                     (emoticons-text-properties rest))))))

(defun emoticons-help-echo ()
  (let ((tip (get-text-property (point) 'help-echo)))
    (and tip (message tip))))

(provide 'emoticons)
;;; emoticons.el ends here
