;;; init-auto-save.el --- Init for auto-save.el

;; Filename: init-auto-save.el
;; Description: Init for auto-save.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-04 14:18:20
;; Version: 0.3
;; Last-Updated: 2018-07-06 20:43:59
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-auto-save.el
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
;; Init for auto-save.el
;;

;;; Installation:
;;
;; Put init-auto-save.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-auto-save)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-auto-save RET
;;

;;; Change log:
;;
;; 2018/07/08
;;      * Move `auto-save-delete-trailing-whitespace-except-current-line' to `auto-save.el'.
;;
;; 2018/06/25
;;      * Delete whitespace before auto-save file.
;;
;; 2014/01/04
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

(require 'auto-save)

;;; Code:

(auto-save-enable)
(setq auto-save-slient t)
(setq auto-save-delete-trailing-whitespace t)

(provide 'init-auto-save)

;;; init-auto-save.el ends here
