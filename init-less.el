;;; init-less.el --- Init less

;; Filename: init-less.el
;; Description: Init less
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 15:38:12
;; Version: 0.1
;; Last-Updated: 2013-12-30 15:38:12
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-less.el
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
;; Init less
;;

;;; Installation:
;;
;; Put init-less.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-less)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-less RET
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

(require 'less)

;;; Code:

(lazy-set-key
 '(
   ("J" . less-scroll-up-one-line)      ;向下浏览
   ("K" . less-scroll-down-one-line)    ;向上浏览
   (">" . beginning-of-buffer)          ;BUFFER结尾
   ("<" . end-of-buffer)                ;BUFFER开始
   ("q" . less-quit)                    ;退出less模式
   ("b" . one-key-menu-hideshow)        ;hideshow 菜单
   ("t" . one-key-menu-etags)           ;Etags 菜单
   ("dd" . auto-scroll-mode)            ;开始滚屏
   ("df" . auto-scroll-faster)          ;滚动的快一点
   ("ds" . auto-scroll-slower)          ;滚动的慢一点
   )
 less-minor-mode-map
 )
(lazy-set-mode-autoload-key sdcv-key-alist less-minor-mode-map nil "init-sdcv") ;sdcv的局部按键绑定
(lazy-set-mode-autoload-key vi-move-key-alist less-minor-mode-map nil "less")

(provide 'init-less)

;;; init-less.el ends here
