;;; init-woman.el --- Init woman module

;; Filename: init-woman.el
;; Description: Init woman module
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-01 20:12:44
;; Version: 0.1
;; Last-Updated: 2014-01-01 20:12:44
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-woman.el
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
;; Init woman module
;;

;;; Installation:
;;
;; Put init-woman.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-woman)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-woman RET
;;

;;; Change log:
;;
;; 2014/01/01
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

(require 'woman)
(require 'lazy-set-key)

;;; Code:

(lazy-set-key
 '(
   ("J" . scroll-up-one-line)           ;向上滚动一行
   ("K" . scroll-down-one-line)         ;向下滚动一行
   ("," . end-of-buffer)                ;buffer末尾
   ("." . beginning-of-buffer)          ;buffer开始
   ("M-n" . Man-next-section)           ;下一节
   ("M-p" . Man-previous-section)       ;上一节
   ("g" . Man-goto-section)             ;跳转到某一节
   ("G" . Man-goto-see-also-section)    ;跳转到 see-also
   ("f" . Man-follow-manual-reference)  ;当前处的man手册引用
   ("F" . man-follow)                   ;某man手册引用
   ("N" . Man-next-manpage)             ;下一个页面
   ("P" . Man-previous-manpage)         ;上一个页面
   ("q" . Man-quit)                     ;隐藏
   ("Q" . Man-kill)                     ;退出
   )
 Man-mode-map
 )
(lazy-set-mode-autoload-key sdcv-key-alist Man-mode-map nil "woman")    ;sdcv的局部按键绑定
(lazy-set-mode-autoload-key vi-move-key-alist Man-mode-map nil "woman") ;vi-move 的局部按键

(setq woman-default-indent 7            ;缩进格式
      woman-fill-frame t                ;填充满屏幕
      woman-use-own-frame nil           ;同一个frame
      woman-cache-level 3)              ;缓存级别, 最快

(provide 'init-woman)

;;; init-woman.el ends here
