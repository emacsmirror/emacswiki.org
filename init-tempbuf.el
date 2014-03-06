;;; init-tempbuf.el --- Init tempbuf

;; Filename: init-tempbuf.el
;; Description: Init tempbuf
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 16:06:54
;; Version: 0.1
;; Last-Updated: 2013-12-30 16:06:54
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-tempbuf.el
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
;; Init tempbuf
;;

;;; Installation:
;;
;; Put init-tempbuf.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-tempbuf)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-tempbuf RET
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

(require 'tempbuf)

;;; Code:

(setq tempbuf-kill-message nil)         ;不在Mode-line显示删除临时buffer提示消息
(setq tempbuf-minimum-timeout 30)       ;删除 buffer 的最低期限
(dolist (hook (list
               'compilation-mode-hook     ;编译模式
               'comint-mode-hook          ;comint 模式
               'completion-list-mode-hook ;补全列表模式
               'help-mode-hook            ;帮助模式
               'Info-mode-hook            ;Info 模式
               'calc-mode-hook            ;计算器模式
               'gnus-article-mode-hook    ;Gnus 文章模式
               'gnus-kill-file-mode       ;Gnus 删除文件模糊
               ))
  (add-hook hook 'turn-on-tempbuf-mode)) ;加载自动清理临时buffer

(provide 'init-tempbuf)

;;; init-tempbuf.el ends here
