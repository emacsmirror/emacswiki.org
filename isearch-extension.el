;;; isearch-extension.el --- Extension for isearch.

;; Filename: isearch-extension.el
;; Description: Extension for isearch.
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-22 16:45:21
;; Version: 0.1
;; Last-Updated: 2008-12-22 16:45:21
;;           By: Andy Stewart
;; URL:
;; Keywords: isearch
;; Compatibility: GNU Emacs 23.0.60.1
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
;; Extension for isearch.
;;

;;; Installation:
;;
;; Put isearch-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'isearch-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/22
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

;;; Code:

(defun isearch-find-regexp (regexp-string)
  "Find duplicate regexp in current buffer."
  (interactive)
  (isearch-yank-string regexp-string)
  (setq isearch-regexp t)
  (isearch-repeat-forward))

(defun isearch-find-duplicate-word ()
  "Find duplicate word in current buffer."
  (interactive)
  (isearch-find-regexp "\\<\\(\\w+\\)\\([ \n\t]\\)+\\1\\>"))

(defun isearch-find-duplicate-line ()
  "Find duplicate line in current buffer."
  (interactive)
  (isearch-find-regexp "\\<\\(^.*$\\)\n+\\1\\>"))

(defun isearch-beginning-of-buffer ()
  "Move isearch point to the beginning of the buffer."
  (interactive)
  (goto-char (point-min))
  (isearch-repeat-forward))

(defun isearch-end-of-buffer ()
  "Move isearch point to the end of the buffer."
  (interactive)
  (goto-char (point-max))
  (isearch-repeat-backward))

(defun isearch-delete-ring-element ()
  "Delete the current minibuffer history element from the history.
After deleting the element the history position is changed either
to the the previous history element, or to the next history element
if the deleted element was the last in the history list."
  (interactive)
  (if isearch-regexp
      (setq regexp-search-ring (delete isearch-string regexp-search-ring))
    (setq search-ring (delete isearch-string search-ring)))
  (isearch-ring-advance))

(provide 'isearch-extension)

;;; isearch-extension.el ends here
