;;; init-rebuilder.el --- Init for re-builder

;; Filename: init-rebuilder.el
;; Description: Init for re-builder
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-04 13:16:57
;; Version: 0.1
;; Last-Updated: 2014-01-04 13:16:57
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-rebuilder.el
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
;; Init for re-builder
;; 

;;; Installation:
;;
;; Put init-rebuilder.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-rebuilder)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-rebuilder RET
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

(require 're-builder)
(require 're-builder+)

;;; Code:

(lazy-set-key
 '(
   ("C-c b" . reb-change-target-buffer) ;改变目标buffer
   ("C-c c" . reb-toggle-case)          ;切换大小写
   ("C-c e" . reb-enter-subexp-mode)    ;进入表达式模式
   ("C-c r" . reb-prev-match)           ;前一个匹配
   ("C-c s" . reb-next-match)           ;后一个匹配
   ("C-c u" . reb-force-update)         ;更新
   ("C-c w" . reb-copy)                 ;拷贝
   ("C-c q" . reb-quit)                 ;退出
   ("C-c TAB" . reb-change-syntax)      ;改变语法
   )
 reb-mode-map
 )

(provide 'init-rebuilder)

;;; init-rebuilder.el ends here
