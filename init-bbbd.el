;;; init-bbbd.el --- Bbbd configuration

;; Filename: init-bbbd.el
;; Description: Bbbd configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:53:31
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:53:31
;;           By: Andy Stewart
;; URL:
;; Keywords: Bbbd
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
;; Bbbd configuration
;;

;;; Installation:
;;
;; Put init-bbbd.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-bbbd)
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

(bbdb-initialize 'gnus 'message)                              ;初始化 BBDB
(bbdb-define-all-aliases)                                     ;定义别名
(setq bbdb-file "~/.emacs.d/deepin-emacs/Configure-File/Bbdb/database") ;数据库文件
(setq bbdb-north-american-phone-numbers-p nil)                ;不使用北美标准的电话号码
(setq bbdb-user-mail-names my-mail)                           ;把你的 email 地址告诉bbdb
(setq bbdb-complete-name-allow-cycling t)                     ;补全 email 地址的时候循环往复
(setq bbdb-use-pop-up nil)                                    ;不用弹出
(setq bbdb-default-country "China")                           ;设置默认的国家
(setq bbdb-quiet-about-name-mismatches t)                     ;当名字改变时不提醒
(setq bbdb-use-alternate-names nil)                           ;不使用别用名字
(setq bbdb-send-mail-style 'gnus)                             ;设置bbdb发送邮件使用的风格

(provide 'init-bbbd)

;;; init-bbbd.el ends here
