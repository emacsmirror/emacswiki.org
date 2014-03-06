;;; init-multiterm.el --- Init for multi-term.el

;; Filename: init-multiterm.el
;; Description: Init for multi-term.el
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-27 23:36:59
;; Version: 0.1
;; Last-Updated: 2013-12-27 23:36:59
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-multiterm.el
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
;; Init for multi-term.el
;; 

;;; Installation:
;;
;; Put init-multiterm.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-multiterm)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-multiterm RET
;;

;;; Change log:
;;	
;; 2013/12/27
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

(require 'multi-term)                   ;多标签SHELL

;;; Code:

;;; ### Multi-term ###
;;; --- 多标签 term
(setq term-eol-on-send t)                         ;输入前跳转到最后一行
(setq multi-term-dedicated-skip-other-window-p t) ;`other-window' 不在专注窗口中经过
(setq multi-term-scroll-show-maximum-output t)    ;最大输出时滚动
(setq multi-term-scroll-to-bottom-on-output nil)  ;到达底部时不滚动
(setq multi-term-dedicated-select-after-open-p t) ;打开专注终端窗口时聚焦

(provide 'init-multiterm)

;;; init-multiterm.el ends here
