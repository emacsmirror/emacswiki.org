;;; doi-extension.el --- Extension for doi.el

;; Filename: doi-extension.el
;; Description: Extension for doi.el
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-13 20:06:39
;; Version: 0.1
;; Last-Updated: 2008-12-13 20:06:39
;;           By: Andy Stewart
;; URL:
;; Keywords: doi
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `doi'
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
;; Extension for doi.el
;;

;;; Installation:
;;
;; Put doi-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'doi-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All above can customize easy through:
;;      M-x customize-group RET doi-extension RET
;;

;;; Change log:
;;
;; 2008/12/13
;;      First released.
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
(require 'doi)

;;; Code:

(defun doi-sdcv-search-pointer+ (&optional arg)
  "Do `doi' with `sdcv-search-pointer+'."
  (interactive "p")
  (doi 'sdcv-search-pointer+ arg))

(defun doi-erc-command (&optional arg)
  "Do `doi' with `/'."
  (interactive "p")
  (doi '(lambda ()
          (end-of-buffer)
          (insert "/"))))

(defun doi-tabbar-backward-tab (&optional arg)
  "Do `doi' with `tabbar-backward-tab'."
  (interactive "p")
  (doi 'tabbar-backward-tab arg))

(defun doi-tabbar-forward-tab (&optional arg)
  "Do `doi' with `tabbar-forward-tab'."
  (interactive "p")
  (doi 'tabbar-forward-tab arg))

(provide 'doi-extension)

;;; doi-extension.el ends here

;;; LocalWords:  doi sdcv
