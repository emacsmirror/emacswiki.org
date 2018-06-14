;;; init-minibuffer-tray.el --- Init for minibuffer-tray.el

;; Filename: init-minibuffer-tray.el
;; Description: Init for minibuffer-tray.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-10-07 23:26:04
;; Version: 0.2
;; Last-Updated: 2018-06-14 12:57:42
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-minibuffer-tray.el
;; Keywords:
;; Compatibility: GNU Emacs 24.4.50.1
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
;; Init for minibuffer-tray.el
;;

;;; Installation:
;;
;; Put init-minibuffer-tray.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-minibuffer-tray)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-minibuffer-tray RET
;;

;;; Change log:
;;
;; 2018/06/14
;;      * Call `minibuffer-tray-stop-process' when kill emacs.
;;
;; 2014/10/07
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

(require 'minibuffer-tray)

;;; Code:

(minibuffer-tray-mode 1)
(add-hook 'kill-emacs-hook 'minibuffer-tray-stop-process)

(provide 'init-minibuffer-tray)

;;; init-minibuffer-tray.el ends here
