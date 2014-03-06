;;; init-icicles.el --- Init for iciciles

;; Filename: init-icicles.el
;; Description: Init for iciciles
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-03 19:37:23
;; Version: 0.1
;; Last-Updated: 2014-01-03 19:37:23
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-icicles.el
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
;; Init for iciciles
;;

;;; Installation:
;;
;; Put init-icicles.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-icicles)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-icicles RET
;;

;;; Change log:
;;
;; 2014/01/03
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

(require 'icicles)

;;; Code:

(icy-mode 1)

;;; ### Icicles ###
;;; --- Minibuffer的智能补全
(setq icicle-top-level-key-bindings nil) ;禁止icicles的按键生效
(setq icicle-key-complete-keys nil)      ;禁止icicles的补全按键加载
(setq icicle-download-dir "/usr/share/deepin-emacs/site-lisp/extensions/icicles/") ;设置icicles的下载目录, 运行 'icicle-download-wizard' 即可更新
(setq icicle-highlight-input-completion-failure-delay 0.0) ;输入补全失败延迟高亮
(setq icicle-incremental-completion-delay 0.0)             ;增量补全延迟
(setq icicle-default-value nil)                            ;不显示默认的值
(setq icicle-highlight-lighter-flag nil)                   ;不显示 Icicles 标志
(setq icicle-unpropertize-completion-result-flag t)        ;解决Gnus附件产生文本属性的bug
(setq icicle-redefine-standard-commands-flag nil)          ;不要重新定义标准按键

;; ### Icicles ###
;; --- Minibuffer 输入补全和切换
(add-hook 'icicle-mode-hook 'bind-icicles-minibuffer-keys)
(defun bind-icicles-minibuffer-keys ()
  "Replace some default Icicles minibuffer bindings with others."
  (dolist
      (map (list
            minibuffer-local-isearch-map             ;isearch
            minibuffer-local-ns-map                  ;当空格不允许时
            minibuffer-local-shell-command-map       ;补全shell命令时
            minibuffer-local-map                     ;从minibuffer读取
            minibuffer-local-completion-map          ;输入补全
            minibuffer-local-must-match-map          ;输入补全精确匹配
            minibuffer-local-filename-completion-map ;文件名补全
            ))
    (when icicle-mode
      (lazy-set-key
       '(
         ("s-o" . icicle-insert-history-element) ;插入历史元素
         )
       map
       )
      (ido-my-keys map)))
  (when icicle-mode
    (lazy-set-key
     '(
       ("TAB" . isearch-complete-edit)
       ("M-k" . isearch-delete-ring-element))
     minibuffer-local-isearch-map
     )))

(provide 'init-icicles)

;;; init-icicles.el ends here
