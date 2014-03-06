;;; init-window.el --- Init window

;; Filename: init-window.el
;; Description: Init window
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 14:54:31
;; Version: 0.1
;; Last-Updated: 2013-12-30 14:54:31
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-window.el
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
;; Init window
;; 

;;; Installation:
;;
;; Put init-window.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-window)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-window RET
;;

;;; Change log:
;;	
;; 2013/12/30
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

(defvar one-key-menu-window-navigation-alist nil
  "The `one-key' menu alist for WINDOW-NAVIGATION.")

(setq one-key-menu-window-navigation-alist
      '(
        (("j" . "Downward") . windmove-down)
        (("k" . "Upward") . windmove-up)
        (("h" . "Leftward") . windmove-left)
        (("l" . "Rightward") . windmove-right)
        (("s" . "Move Down") . buf-move-down)
        (("d" . "Move Up") . buf-move-up)
        (("a" . "Move Left") . buf-move-left)
        (("f" . "Move Right") . buf-move-right)
        (("u" . "Enlarge Down") . (lambda () (interactive) (windresize-up-inwards '-1)))
        (("i" . "Enlarge Up") . (lambda () (interactive) (windresize-down-inwards '-1)))
        (("y" . "Enlarge Left") . (lambda () (interactive) (windresize-right-inwards '-1)))
        (("o" . "Enlarge Right") . (lambda () (interactive) (windresize-left-inwards '-1)))
        (("m" . "Shrink Down") . (lambda () (interactive) (windresize-up-inwards '1)))
        (("," . "Shrink Up") . (lambda () (interactive) (windresize-down-inwards '1)))
        (("n" . "Shrink Left") . (lambda () (interactive) (windresize-right-inwards '1)))
        (("." . "Shrink Right") . (lambda () (interactive) (windresize-left-inwards '1)))
        (("x" . "Outward Window") . outward-window)
        (("c" . "Inward Window") . inward-window)
        (("7" . "Tabbar Left") . tabbar-backward-tab)
        (("8" . "Tabbar Right") . tabbar-forward-tab)
        (("9" . "Tabbar Next") . tabbar-backward-group)
        (("0" . "Tabbar Previous") . tabbar-forward-group)
        ((";" . "Kill Buffer") . kill-this-buffer)
        ((":" . "Kill Other Windows") . delete-other-windows)
        (("'" . "Kill Buffer And Window") . delete-current-buffer-and-window)
        (("b" . "Anything Mode") . anything)
        (("e" . "List Registers") . list-registers)
        (("r" . "Remember Register") . frame-configuration-to-register)
        (("t" . "Jump Register") . jump-to-register)
        (("g" . "Split Horizontally") . split-window-horizontally)
        (("v" . "Split Vertically") . split-window-vertically)
        ))

(defun one-key-menu-window-navigation ()
  "The `one-key' menu for WINDOW-NAVIGATION."
  (interactive)
  (one-key-menu "WINDOW-NAVIGATION" one-key-menu-window-navigation-alist t t))

(provide 'init-window)

;;; init-window.el ends here
