;;; init-info.el --- Init for Info-mode

;; Filename: init-info.el
;; Description: Init for Info-mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-04 13:30:42
;; Version: 0.1
;; Last-Updated: 2014-01-04 13:30:42
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-info.el
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
;; Init for Info-mode
;;

;;; Installation:
;;
;; Put init-info.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-info)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-info RET
;;

;;; Change log:
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

(require 'info)

;;; Code:

;;; ### Info ###
;;; --- Info 模式
(lazy-set-key
 '(
   ("f" . Info-follow-nearest-node)     ;进入当前节点
   ("<tab>" . Info-next-reference)      ;下一个引用
   ("<backtab>" . Info-prev-reference)  ;上一个引用
   ("E" . Info-edit)                    ;编辑
   ("?" . Info-summary)                 ;帮助
   ("N" . Info-next)                    ;下一个同级节点
   ("P" . Info-prev)                    ;上一个同级节点
   ("J" . scroll-up-one-line)           ;向下滚动一行
   ("K" . scroll-down-one-line)         ;向上滚动一行
   ("." . go-to-char-forward)           ;向后查找某一个字符
   ("," . go-to-char-backward)          ;向前查找某一个字符
   ("<" . Info-forward-node)            ;下一个节点
   (">" . Info-backward-node)           ;上一个节点
   ("C-<" . Info-final-node)            ;最后一个节点
   ("C->" . Info-top-node)              ;最前一个节点
   ("s" . Info-search)                  ;搜索
   ("S" . Info-search-case-sensitively) ;区分大小写搜索
   ("g" . Info-goto-node)               ;跳到指定的节点
   ("q" . Info-exit)                    ;退出
   ("m" . Info-menu)                    ;菜单补全
   ("d" . Info-directory)               ;总目录
   ("I" . Info-index)                   ;索引
   ("o" . Info-follow-reference)        ;随后的引用补全
   ("H" . Info-history)                 ;历史
   ("F" . Info-history-forward)         ;历史向前
   ("B" . Info-history-back)            ;历史向后
   ("M-s" . Info-search)                ;节点搜索
   ("C" . clone-buffer)                 ;克隆当前buffer
   ("c" . Info-copy-current-node-name)  ;拷贝当前节点名字
   ("u" . Info-up)                      ;跳到上一级
   ("T" . Info-toc)                     ;内容索引
   ("e" . Info-scroll-down)             ;向上滚动, vi-move 的后面重新加载
   (" " . Info-fscroll-up)              ;向下滚动
   )
 Info-mode-map
 )
(lazy-set-key sdcv-key-alist Info-mode-map)    ;sdcv的局部按键绑定
(lazy-set-key vi-move-key-alist Info-mode-map) ;vi-move 的局部按键

(provide 'init-info)

;;; init-info.el ends here
