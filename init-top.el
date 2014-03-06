;;; init-top.el --- Init for top

;; Filename: init-top.el
;; Description: Init for top
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-04 13:22:14
;; Version: 0.1
;; Last-Updated: 2014-01-04 13:22:14
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-top.el
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
;; Init for top
;;

;;; Installation:
;;
;; Put init-top.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-top)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-top RET
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

(require 'top-mode)

;;; Code:

(lazy-set-key
 '(
   ("s" . isearch-forward)				;搜索
   ("g" . top)							;刷新
   ("q" . quit-window)					;退出
   ("d" . top-mode-kill)				;删除
   ("D" . top-mode-kill-noconfirm)		;不需要确认删除
   ("t" . top-mode-strace)
   ("T" . top-mode-strace-noconfirm)
   ("r" . top-mode-renice)
   ("R" . top-mode-renice-noconfirm)
   ("m" . top-mode-mark)				;标记
   ("u" . top-mode-unmark)				;删除标记
   ("U" . top-mode-show-specific-user))
 top-mode-map
 )
(lazy-set-key sdcv-key-alist top-mode-map)    ;sdcv 的局部按键
(lazy-set-key vi-move-key-alist top-mode-map) ;vi-mode的局部按键

(provide 'init-top)

;;; init-top.el ends here
