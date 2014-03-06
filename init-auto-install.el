;;; init-auto-install.el --- Init for auto-install

;; Filename: init-auto-install.el
;; Description: Init for auto-install
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 16:00:06
;; Version: 0.1
;; Last-Updated: 2013-12-30 16:00:06
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-auto-install.el
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
;; Init for auto-install
;;

;;; Installation:
;;
;; Put init-auto-install.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-auto-install)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-auto-install RET
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

(require 'auto-install)

;;; Code:

(setq auto-install-directory "/usr/share/deepin-emacs/site-lisp/extensions/lazycat/") ;设置默认的安装目录
(setq auto-install-from-w3m-confirm nil) ;从w3m安装不提醒
(setq auto-install-save-confirm nil)     ;不需要确认保存
(setq auto-install-install-compile nil)  ;默认不编译文件

(provide 'init-auto-install)

;;; init-auto-install.el ends here
