;;; init-coffee-mode.el --- Simple configure for coffee mode

;; Filename: init-coffee-mode.el
;; Description: Simple configure for coffee mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2012, Andy Stewart, all rights reserved.
;; Created: 2012-12-26 11:43:14
;; Version: 0.1
;; Last-Updated: 2012-12-26 11:43:14
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-coffee-mode.el
;; Keywords:
;; Compatibility: GNU Emacs 24.0.94.2
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
;; Simple configure for coffee mode
;;

;;; Installation:
;;
;; Put init-coffee-mode.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-coffee-mode)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-coffee-mode RET
;;

;;; Change log:
;;
;; 2012/12/26
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

(setq coffee-tab-width 4)

(add-hook 'coffee-mode-hook
          (lambda () (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))))


(provide 'init-coffee-mode)

;;; init-coffee-mode.el ends here
