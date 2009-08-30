;;; etags-extension.el --- A collect extensions for etags.

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-07-05 17:48:30
;; Version: 1.0
;; Last-Updated: 2008-07-05 17:48:35
;; URL: not distributed yet
;; Keywords: etags
;; Compatibility: GNU Emacs 23.0.60.1

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

;; Features that might be required by this library:
;;
;;  `etags'
;;

;;; Installation:
;;
;; Copy etags-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'etags-extension)
;;
;; No need more

;;; Commentary:
;;
;; A collect functions for etags generate, find, etc.
;;

;;; Change log:
;;
;; 2008/07/05
;;         First release.
;;

;;; Acknowledgments:
;;
;;              All emacsers.
;;

;;; TODO
;;
;; None
;;

;;; Code:

(defvar tags-default-target-directory "~/MyEmacs/")  ;default index directory.
(defvar tags-default-storage-directory "~/MyEmacs/") ;default storage directory.
(defvar tags-default-suffix "*.el")                  ;default suffix

(defun generate-tag-table-of-emacs ()
  "Generate tag tables of emacs"
  (interactive)
  (generate-tag-table tags-default-target-directory
                      tags-default-storage-directory
                      tags-default-suffix))

(defun generate-tag-table (&optional tags-target-directory
                                     tags-storage-directory
                                     tags-suffix)
  "Generate tag tables with special directory."
  (interactive)
  (or tags-target-directory
      (setq tags-target-directory (read-directory-name "Target directory: ")))
  (or tags-storage-directory
      (setq tags-storage-directory (read-directory-name "Storage directory: ")))
  (or tags-suffix
      (setq tags-suffix (read-string "Suffix: ")))
  (with-temp-buffer
    (cd tags-storage-directory)
    (shell-command
     (format "find %s -name \"%s\" | xargs etags" tags-target-directory tags-suffix))
    (message "Tags index...")))

(defun find-tag-window ()
  "View tag in little window."
  (interactive)
  (find-tag+ t))

(defun find-tag+ (&optional show-only)
  "Show tag in other window with no prompt in minibuffer."
  (interactive)
  (let ((default (funcall (or find-tag-default-function
                              (get major-mode 'find-tag-default-function)
                              'find-tag-default))))
    (if show-only
        (progn
          (find-tag-other-window default)
          (shrink-window (- (window-height) 12))
          (recenter 1)
          (other-window 1))
      (find-tag default))))

(defun release-small-tag-window ()
  "Kill other window also pop tag mark."
  (interactive)
  (delete-other-windows)
  (ignore-errors
    (pop-tag-mark)))

(provide 'etags-extension)

;;; etags-extension.el ends here

;;; LocalWords:  etags MyEmacs xargs
