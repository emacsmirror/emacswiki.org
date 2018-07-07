;;; moccur-extension.el --- Search something at pointer with moccur-grep-find-pwd

;; Filename: moccur-extension.el
;; Description: Search something at pointer with moccur-grep-find-pwd
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2017, Andy Stewart, all rights reserved.
;; Created: 2017-06-14 17:38:23
;; Version: 0.4
;; Last-Updated: 2018-07-07 09:32:42
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/moccur-extension.el
;; Keywords:
;; Compatibility: GNU Emacs 25.0.50.1
;;
;; Features that might be required by this library:
;;
;; `color-mocurr' `thing-edit'
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
;; Search something at pointer with moccur-grep-find-pwd
;;

;;; Installation:
;;
;; Put moccur-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'moccur-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET moccur-extension RET
;;

;;; Change log:
;;
;; 2018/07/07
;;      * Refacotry code make interactive functions eaiser to use.
;;      * Add new function `moccur-grep-in-rails-app-directory', `moccur-read-input', `moccur-pointer-string'.
;;      * Fix prompt string.
;;
;; 2018/07/02
;;      * Add `file-binary-p' from `dired-extension.el'
;;
;; 2018/06/29
;;      * Create `moccur-grep-pointer' and `moccur-grep-input'
;;      * Rename to `moccur-extension.el'
;;
;; 2017/06/14
;;      * First released.
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
(require 'color-moccur)
(require 'subr-x)

;;; Code
(defun moccur-grep-pointer ()
  (interactive)
  (moccur-grep-find-without-binary-files
   default-directory
   (moccur-regexp-string (moccur-pointer-string))))

(defun moccur-grep-input ()
  (interactive)
  (moccur-grep-find-without-binary-files
   default-directory
   (moccur-read-input "Moccur grep input (%s): ")))

(defun moccur-grep-in-rails-app-directory ()
  (interactive)
  (require 'projectile-rails)
  (moccur-grep-find-without-binary-files
   (concat (projectile-project-root) "app")
   (moccur-read-input "Moccur grep rails app directory (%s): ")))

(defun moccur-read-input (prompt-string)
  (let* ((current-symbol (moccur-pointer-string))
         (input-string (string-trim (read-string (format prompt-string current-symbol)))))
    (when (string-blank-p input-string)
      (setq input-string current-symbol))
    (moccur-regexp-string input-string)))

(defun moccur-pointer-string ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (string-remove-prefix "." (thing-at-point 'symbol))))

(defun moccur-regexp-string (input)
  (if (use-region-p)
      (moccur-split-string (concat (replace-regexp-in-string  "\\s-+" "\\\\s-" input)))
    (moccur-split-string (concat "\\b" (replace-regexp-in-string  "\\s-+" "\\\\s-" input) "\\b"))))

(defun moccur-grep-find-without-binary-files (dir inputs)
  "This function is copy `moccur-grep-find' from `color-moccur.el' and with little improve.
Default `moccur-grep-find' will search keyword from all files under directory, include binary files.
This function will fitler binary files before search continue, avoid view binary when we cross search results."
  (interactive
   (list (moccur-grep-read-directory)
         (moccur-grep-read-regexp moccur-grep-default-mask)))
  (moccur-setup)
  (setq moccur-last-command 'moccur-grep-find)

  (let (regexps
        mask (files nil)
        ;;(default-directory dir)
        )
    (setq regexps
          (mapconcat 'concat
                     (if (= 1 (length inputs))
                         inputs
                       (reverse (cdr (reverse inputs))))
                     " "))
    (setq mask
          (if (= 1 (length inputs))
              "."
            (car (reverse inputs))))
    (message "Listing files...")
    (cond
     ((listp dir)
      (while dir
        (cond
         ((file-directory-p (car dir))
          (setq files (append
                       (reverse (moccur-grep-find-subdir (car dir) mask))
                       files)))
         (t
          (setq files (cons
                       (car dir)
                       files))))
        (setq dir (cdr dir))))
     (t
      (setq files (reverse (moccur-grep-find-subdir dir mask)))))
    (message "Listing files done!")
    (moccur-search-files regexps (seq-remove 'file-binary-p files))
    ))

(defun file-binary-p (file &optional full)
  "Return t if FILE contains binary data.  If optional FULL is non-nil,
check for the whole contents of FILE, otherwise check for the first
  1000-byte."
  (let ((coding-system-for-read 'binary)
        default-enable-multibyte-characters)
    (with-temp-buffer
      (insert-file-contents file nil 0 (if full nil 1000))
      (goto-char (point-min))
      (and (re-search-forward
            "[\000-\010\016-\032\034-\037]"
            nil t)
           t))))

(provide 'moccur-extension)

;;; moccur-extension.el ends here
