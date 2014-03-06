;;; init-multiple-cursors.el --- Init for awesome multiple-cursors

;; Filename: init-multiple-cursors.el
;; Description: Init for awesome multiple-cursors
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-31 22:43:22
;; Version: 0.1
;; Last-Updated: 2013-12-31 22:43:22
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-multiple-cursors.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
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
;; Init for awesome multiple-cursors
;;

;;; Installation:
;;
;; Put init-multiple-cursors.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-multiple-cursors)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-multiple-cursors RET
;;

;;; Change log:
;;
;; 2013/12/31
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

(require 'rectangular-region-mode)
(require 'mc-cycle-cursors)
(require 'mc-edit-lines)
(require 'mc-mark-more)
(require 'mc-mark-pop)
(require 'mc-separate-operations)
(require 'multiple-cursors-core)
(require 'multiple-cursors)

;;; Code:

(defvar one-key-menu-multiple-cursors-alist nil
  "The `one-key' menu alist for multiple cursors.")

(setq one-key-menu-multiple-cursors-alist
      '(
        (("i" . "Insert numbers") . mc/insert-numbers)
        (("s" . "Sort regions") . mc/sort-regions)
        (("r" . "Reverse regions") . mc/reverse-regions)
        ))

(defun one-key-menu-multiple-cursors ()
  "The `one-key' menu for multiple cursors."
  (interactive)
  (one-key-menu "multiple cursors" one-key-menu-multiple-cursors-alist t nil nil
                '(lambda ()
                   (interactive)
                   (unless (eq major-mode 'w3m-mode)
                     (w3m)))))

(provide 'init-multiple-cursors)

;;; init-multiple-cursors.el ends here
