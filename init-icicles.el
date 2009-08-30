;;; init-icicles.el --- Config file for icicles.el

;; Filename: init-icicles.el
;; Description: Config file for icicles.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-03-13 01:18:07
;; Version: 0.1
;; Last-Updated: 2009-03-13 01:18:07
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-icicles.el
;; Keywords: icicels
;; Compatibility: GNU Emacs 23.0.91.1
;;
;; Features that might be required by this library:
;;
;; `icicles'
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
;; Config file for icicles.el
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
;; 2009/03/13
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


;;; ### Icicles ###
;;; --- Minibuffer的智能补全
(setq icicle-top-level-key-bindings nil) ;禁止icicles的按键生效
(setq icicle-key-complete-keys nil)      ;禁止icicles的补全按键加载
(setq icicle-download-dir "~/MyEmacs/Site-Lisp/Packages/icicles/") ;设置icicles的下载目录, 运行 'icicle-download-wizard' 即可更新
(setq icicle-highlight-input-completion-failure-delay 0.0)         ;输入补全失败延迟高亮
(setq icicle-incremental-completion-delay 0.0)                     ;增量补全延迟
(setq icicle-default-value nil)                                    ;不显示默认的值
(setq icicle-highlight-lighter-flag nil)                           ;不显示 Icicles 标志
(setq icicle-unpropertize-completion-result-flag t)                ;解决Gnus附件产生文本属性的bug
(setq icicle-redefine-standard-commands-flag nil)                  ;不要重新定义标准按键
(icicle-mode 1)                                                    ;打开icicles模式

(provide 'init-icicles)

;;; init-icicles.el ends here

