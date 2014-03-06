;;; init-doc-view.el --- Init doc view

;; Filename: init-doc-view.el
;; Description: Init doc view
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 16:03:55
;; Version: 0.1
;; Last-Updated: 2013-12-30 16:03:55
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-doc-view.el
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
;; Init doc view
;;

;;; Installation:
;;
;; Put init-doc-view.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-doc-view)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-doc-view RET
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

(require 'doc-view)
(require 'doc-view-extension)

;;; Code:

(setq doc-view-cache-directory "~/.emacs.d/deepin-emacs/book-cache")
(setq doc-view-image-width (- (display-pixel-width) 16))
(setq doc-view-resolution 200)

(lazy-unset-key
 '(".")
 doc-view-mode-map)                     ;卸载按键
(lazy-unset-key
 '("x" "M-<" "M->")
 doc-view-mode-map)                     ;卸载一些按键
(lazy-set-key
 '(
   ([remap scroll-up] . doc-view-next-line-or-next-page) ;重新定向按键, 支持 auto-scroll
   )
 doc-view-mode-map
 )
(lazy-set-key
 '(
   ("N" . doc-view-next-page)                      ;下一页
   ("P" . doc-view-previous-page)                  ;上一页
   ("." . doc-view-first-page)                     ;第一页
   ("," . doc-view-last-page)                      ;最后一页
   ("g" . doc-view-goto-page)                      ;跳到第几页
   ("e" . doc-view-scroll-down-or-previous-page)   ;向上滚动一屏
   ("SPC" . doc-view-scroll-up-or-next-page)       ;向下滚动一屏
   ("j" . doc-view-next-line-or-next-page)         ;下一行或下一屏
   ("k" . doc-view-previous-line-or-previous-page) ;上一行或上一屏
   ("t" . doc-view-show-tooltip)                   ;当前页提示
   ("q" . bury-buffer)                             ;隐藏buffer
   ("Q" . doc-view-kill-proc-and-buffer)           ;退出并结束进程
   ("C-s" . doc-view-search)                       ;搜索
   ("C-S-n" . doc-view-search-next-match)          ;下一个匹配
   ("C-S-p" . doc-view-search-previous-match)      ;上一个匹配
   ("+" . doc-view-enlarge)                        ;放大页面
   ("-" . doc-view-shrink)                         ;缩小页面
   ("C-c C-c" . doc-view-toggle-display)           ;在文本和图像间切换
   ("C-c C-t" . doc-view-open-text)                ;打开文本
   ("r" . revert-buffer)                           ;刷新
   ("s" . auto-scroll-mode)                        ;自动滚屏
   ("<" . auto-scroll-faster)                      ;加快滚屏速度
   (">" . auto-scroll-slower)                      ;减慢滚屏速度
   )
 doc-view-mode-map
 )
(lazy-set-key sdcv-key-alist doc-view-mode-map) ;sdcv的局部按键绑定

(provide 'init-doc-view)

;;; init-doc-view.el ends here
