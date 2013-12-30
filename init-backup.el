;;; init-backup.el --- 备份设置

;; Filename: init-backup.el
;; Description: 备份设置设置
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:13:17
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:13:20
;;           By: Andy Stewart
;; URL:
;; Keywords: time
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
;; 备份设置设置
;;

;;; Installation:
;;
;; Put init-backup.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-backup)
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

(setq backup-directory-alist `((".*" . ,temporary-file-directory))) ;让所有 ~ 备份文件全部放在 /tmp 目录
(setq make-backup-files t)                                          ;开启备份
(setq kept-old-versions 10)                                         ;备份老版本的数量
(setq kept-new-versions 20)                                         ;备份新版本的数量
(setq delete-old-versions t)                                        ;自动删除老版本
(setq backup-by-copying t)                                          ;拷贝时自动备份
(setq version-control t)                                            ;多版本备份

(provide 'init-backup)

;;; init-backup.el ends here
