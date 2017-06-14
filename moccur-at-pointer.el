;;; moccur-at-pointer.el --- Search something at pointer with moccur-grep-find-pwd

;; Filename: moccur-at-pointer.el
;; Description: Search something at pointer with moccur-grep-find-pwd
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2017, Andy Stewart, all rights reserved.
;; Created: 2017-06-14 17:38:23
;; Version: 0.1
;; Last-Updated: 2017-06-14 17:38:23
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/moccur-at-pointer.el
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
;; Put moccur-at-pointer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'moccur-at-pointer)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET moccur-at-pointer RET
;;

;;; Change log:
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

(defun moccur-at-pointer (beg end)
  ;; Get symbol at pointer or selected text.
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (beginning-of-thing 'symbol) (end-of-thing 'symbol))))
  (let ((content (buffer-substring-no-properties beg end)))
    ;; Convert content whitespace with \s- and around \b, then pass regex to moccur-grep-find to search.
    (moccur-grep-find
     default-directory
     (moccur-split-string (concat "\\b" (replace-regexp-in-string  "\\s-+" "\\\\s-" content) "\\b")))))


(provide 'moccur-at-pointer)

;;; moccur-at-pointer.el ends here
