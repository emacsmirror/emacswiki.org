;;; init-irfc.el --- Init irfc

;; Filename: init-irfc.el
;; Description: Init irfc
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 16:01:24
;; Version: 0.1
;; Last-Updated: 2013-12-30 16:01:24
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-irfc.el
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
;; Init irfc
;;

;;; Installation:
;;
;; Put init-irfc.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-irfc)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-irfc RET
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

(require 'irfc)

;;; Code:

(setq irfc-directory "/space/data/Book/Network_Programming/RFC-all") ;设置存储目录
(custom-set-variables                                                ;自动关联 `irfc-mode'
 '(irfc-assoc-mode t))
(lazy-set-key
 '(
   ("c" . kill-this-buffer)                         ;关闭当前buffer
   ("C" . kill-current-mode-buffers-except-current) ;关闭所有后台标签
   ("m" . tabbar-forward-tab)                       ;向右一个标签
   ("n" . tabbar-backward-tab)                      ;向左一个标签
   ("<" . end-of-buffer)                            ;最下面
   (">" . beginning-of-buffer)                      ;最上面
   )
 irfc-mode-map
 )
(lazy-set-key sdcv-key-alist irfc-mode-map)

(provide 'init-irfc)

;;; init-irfc.el ends here
