;;; lazy-search-extension.el --- Lazy Search Extension

;; Filename: lazy-search-extension.el
;; Description: Lazy Search Extension
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-23 23:13:39
;; Version: 0.1
;; Last-Updated: 2008-12-23 23:13:39
;;           By: Andy Stewart
;; URL:
;; Keywords: lazy-search
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `lazy-search' `paredit' `color-moccur'
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
;; Lazy Search Extension
;;

;;; Installation:
;;
;; Put lazy-search-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lazy-search-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/24
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;

;;; Require
(require 'lazy-search)
(require 'paredit)
(require 'color-moccur)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mark Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-mark-parentheses ()
  "Mark parentheses."
  (interactive)
  (save-excursion
    (if (paredit-in-string-p)
        (lazy-search-mark
         (point)
         (1+ (car (paredit-string-start+end-points)))
         (cdr (paredit-string-start+end-points)))
      (lazy-search-mark
       (point)
       (progn
         (backward-up-list)
         (forward-char +1)
         (point))
       (progn
         (up-list)
         (forward-char -1)
         (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Copy Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-copy-parentheses ()
  "Copy parentheses at point."
  (interactive)
  (save-excursion
    (if (paredit-in-string-p)
        (lazy-search-mark
         (point)
         (1+ (car (paredit-string-start+end-points)))
         (cdr (paredit-string-start+end-points)))
      (lazy-search-copy
       (progn
         (backward-up-list)
         (forward-char +1)
         (point))
       (progn
         (up-list)
         (forward-char -1)
         (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Others ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lazy-search-moccur ()
  "Use moccur for search object."
  (interactive)
  (setq isearch-string lazy-search-object) ;set isearch string with object
  (isearch-moccur)
  (lazy-search-quit))

(defun lazy-search-moccur-all ()
  "Use moccur all for search object."
  (interactive)
  (setq isearch-string lazy-search-object) ;set isearch string with object
  (isearch-moccur-all)
  (lazy-search-quit))

(dolist (elt-cons '(
                    ;; Mark
                    (("'" . "Mark Parentheses") . lazy-search-mark-parentheses)
                    ;; Copy
                    (("\"" . "Copy Parentheses") . lazy-search-copy-parentheses)
                    ;; Other
                    (("v" . "Moccur") . lazy-search-moccur)
                    (("V" . "Moccur All") . lazy-search-moccur-all)
                    ))
  (add-to-alist 'lazy-search-menu-alist elt-cons))

(provide 'lazy-search-extension)

;;; lazy-search-extension.el ends here

;;; LocalWords:  elt
