;;; insert-translated-name.el --- Insert translated string as variable or function name  -*- lexical-binding: t; -*-

;; Filename: insert-translated-name.el
;; Description: Insert translated string as variable or function name
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-22 10:54:16
;; Version: 0.2
;; Last-Updated: 2018-09-22 15:35:00
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/insert-translated-name.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Insert translated string as variable or function name
;;

;;; Installation:
;;
;; Put insert-translated-name.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'insert-translated-name)
;;
;; No need more.

;;; Customize:
;;
;; `insert-translated-name-line-style-mode-list'
;; `insert-translated-name-underline-style-mode-list'
;; `insert-translated-name-camel-style-mode-list'
;;

;;; Change log:
;;
;; 2018/09/22
;;      * First released.
;;      * Change query translation asynchronous, don't insert buffer if query duration more than 2 seconds.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defconst insert-translated-name-api-url
  "http://fanyi.youdao.com/openapi.do?keyfrom=YouDaoCV&key=659600698&type=data&doctype=json&version=1.1&q=%s"
  "Youdao dictionary API template, URL `http://dict.youdao.com/'.")

(defvar insert-translated-name-line-style-mode-list
  '(web-mode emacs-lisp-mode))

(defvar insert-translated-name-camel-style-mode-list
  '(js-mode))

(defvar insert-translated-name-underline-style-mode-list
  '(ruby-mode))

(defun insert-translated-name (word)
  (interactive "sTranslate with current mode style: ")
  (cond ((insert-translated-name-match-modes insert-translated-name-line-style-mode-list)
         (insert-translated-name-with-line word))
        ((insert-translated-name-match-modes insert-translated-name-camel-style-mode-list)
         (insert-translated-name-with-camel word))
        ((insert-translated-name-match-modes insert-translated-underline-camel-style-mode-list)
         (insert-translated-name-with-underline word))
        (t
         (insert-translated-name-with-underline word))))

(defun insert-translated-name-match-modes (mode-list)
  (cl-remove-if 'null (mapcar '(lambda (mode) (derived-mode-p mode)) mode-list)))

(defun insert-translated-name-with-line (word)
  (interactive "sTranslate with line style: ")
  (insert-translated-name-get-translation word "line"))

(defun insert-translated-name-with-underline (word)
  (interactive "sTranslate with underline style: ")
  (insert-translated-name-get-translation word "underline"))

(defun insert-translated-name-with-camel (word)
  (interactive "sTranslate with camel style: ")
  (insert-translated-name-get-translation word "camel"))

(defun insert-translated-name-get-translation (word style)
  "Request WORD, return JSON as an alist if successes."
  (let ((placeholder (insert-translated-name--generate-uuid)))
    (insert placeholder)
    (url-retrieve
     (format insert-translated-name-api-url (url-hexify-string word))
     'insert-translated-name-retrieve-callback
     (list style (current-buffer) placeholder))))

(defun insert-translated-name-convert-translation (translation style)
  (let ((words (split-string translation " ")))
    (cond ((string-equal style "line")
           (string-join (mapcar 'downcase words) "-"))
          ((string-equal style "underline")
           (string-join (mapcar 'downcase words) "_"))
          ((string-equal style "camel")
           (concat (downcase (car words)) (string-join (mapcar 'capitalize (cdr words))))))))

(defun insert-translated-name-retrieve-callback (&optional redirect style insert-buffer placeholder)
  (let (json word translation result)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (when (not (string-match "200 OK" (buffer-string)))
      (error "Problem connecting to the server"))
    (re-search-forward "^$" nil 'move)
    (setq json (json-read-from-string
                (buffer-substring-no-properties (point) (point-max))))
    (kill-buffer (current-buffer))
    (setq word (assoc-default 'query json))
    (setq translation (elt (assoc-default 'translation json) 0))
    (setq result (insert-translated-name-convert-translation translation style))
    (with-current-buffer insert-buffer
      (save-excursion
        (goto-char (point-min))
        (search-forward placeholder)
        (replace-match result)))))

(defun insert-translated-name--generate-uuid ()
  "Generate a 32 character UUID."
  (md5 (number-to-string (float-time))))

(provide 'insert-translated-name)

;;; insert-translated-name.el ends here
