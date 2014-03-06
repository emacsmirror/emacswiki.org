;;; init-ide.el --- IDE configuration

;; Filename: init-ide.el
;; Description: IDE configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:22:09
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:22:12
;;           By: Andy Stewart
;; URL:
;; Keywords: ide
;; Compatibility: GNU Emacs 23.0.60.1
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
;; IDE configuration
;;

;;; Installation:
;;
;; Put init-ide.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-ide)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/20
;;      First released.
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

;;; ### EDE ###
;;; --- Emacs 开发环境
;; (global-ede-mode 1)

;;; ### Semantic ###
;;; --- 代码分析， 智能补全
;; (add-hook 'semantic-init-hooks 'semantic-idle-completions-mode) ;空闲时进行补全分析
;; (setq semanticdb-project-roots (list (expand-file-name "/")))   ;配置Semantic的检索范围
;; (autoload 'senator-try-expand-semantic "senator")               ;优先调用了senator的分析结果

;;; ### Hippie-exapnd ###
;;; --- 符号补全
;; hippie-expand 自动补全策略
(setq hippie-expand-try-functions-list
      '(
        ;; senator-try-expand-semantic        ;优先调用senator的分析结果
        try-expand-dabbrev-visible         ;dabbrev策略, 可见窗口优先
        try-expand-dabbrev                 ;dabbrev策略
        try-expand-dabbrev-all-buffers     ;dabbrev策略, 包括所有窗口(除了当前窗口)
        try-expand-dabbrev-from-kill       ;dabbrev策略, 从所有删除记录里搜索
        try-complete-file-name             ;补全文件名
        try-complete-file-name-partially   ;补全文件名, 匹配优先
        try-expand-list                    ;补全list
        try-expand-list-all-buffers        ;补全list, 包括所有窗口(除了当前窗口)
        try-expand-line                    ;整行补全
        try-expand-line-all-buffers        ;整行补全, 包括所有窗口(除了当前窗口)
        try-complete-lisp-symbol           ;补全符号, 符号太多了, 设置低优先级利于高效补全
        try-complete-lisp-symbol-partially ;补全符号, 包括所有窗口(除了当前窗口)
        try-expand-whole-kill              ;kill-ring里面补全
        ))

;;; ### Xrefactory ###
;;; --- Java & C 重构环境
;; (defvar xref-current-project nil)       ;设定当前的工程
;; (defvar xref-key-binding 'none)         ;设定当前的按键邦定
;; (setq exec-path (cons (expand-file-name "/usr/share/deepin-emacs/Site-Lisp/Packages/xref") exec-path))
;; (setq load-path (cons (expand-file-name "/usr/share/deepin-emacs/Site-Lisp/Packages/xref/emacs") load-path))
;; (load "xrefactory")
;; (setq xref-auto-update-tags-before-push t)                  ;自动刷新Tags
;; (setq xref-completion-inserts-parenthesis t)                ;自动插入圆括号
;; (setq xref-save-files-and-update-tags-after-refactoring t)  ;重构后自动刷新Tags
;; (setq xref-save-files-and-update-tags-before-refactoring t) ;重构前自动刷新Tags
;; (setq xref-files-encoding 'euc-cn)                          ;设置文件编码, 支持中文

(provide 'init-ide)

;;; init-ide.el ends here
