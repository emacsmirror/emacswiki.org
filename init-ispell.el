;;; init-ispell.el --- Ispell configuration

;; Filename: init-ispell.el
;; Description: Ispell configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:27:08
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:27:11
;;           By: Andy Stewart
;; URL:
;; Keywords: ispell
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be requried by this library:
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
;; Ispell configuration
;;

;;; Installation:
;;
;; Put init-ispell.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-ispell)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/20
;;      First realead.
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

(require 'ispell)

;;; Code:

(setq-default ispell-program-name "aspell")     ;用aspell替换ispell, 更加智能
(setq-default ispell-extra-args '("--reverse")) ;修复aspell与ispell冲突的bug
(setq ispell-personal-dictionary "~/.emacs.d/deepin-emacs/Configure-File/Ispell/personal-dictionary") ;设置个人词典
(setq ispell-silently-savep t)          ;保存自己的个人词典不需要询问
(setq ispell-dictionary "english")      ;设置英文词典

(provide 'init-ispell)

;;; init-ispell.el ends here
