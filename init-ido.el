;;; init-ido.el --- Ido configuration

;; Filename: init-ido.el
;; Description: Ido configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:19:33
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:19:36
;;           By: Andy Stewart
;; URL:
;; Keywords: ido
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
;; Ido configuration
;;

;;; Installation:
;;
;; Put init-ido.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-ido)
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

(ido-mode t)                                        ;开启ido模式
(setq ido-enable-flex-matching t)                   ;模糊匹配
(setq ido-everywhere nil)                           ;禁用ido everyting, 拷贝操作不方便
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime) ;文件的排序方法
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)  ;目录的排序方法

(provide 'init-ido)

;;; init-ido.el ends here
