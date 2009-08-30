;;; init-time.el --- 时间设置

;; Filename: init-time.el
;; Description: 时间设置设置
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:11:57
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:12:00
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
;; 时间设置设置
;;

;;; Installation:
;;
;; Put init-time.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-time)
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

(setq display-time-day-and-date t)                             ;打开日期显示
(display-time-mode 1)                                          ;打开时间显示
(display-time)                                                 ;显示时间
(setq display-time-format " %Y/%m/%d %H:%M %A")                ;设定时间显示格式
(setq display-time-24hr-format t)                              ;打开24小时显示模式
(setq time-stamp-format "%3a %3b %2d %02H:%02M:%02S %:y (%z)") ;设置时间戳的显示格式

(provide 'init-time)

;;; init-time.el ends here
