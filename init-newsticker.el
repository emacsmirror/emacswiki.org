;;; init-newsticker.el --- Newsticker configuration

;; Filename: init-newsticker.el
;; Description: Newsticker configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:47:08
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:47:08
;;           By: Andy Stewart
;; URL:
;; Keywords: Newsticker
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
;; Newsticker configuration
;;

;;; Installation:
;;
;; Put init-newsticker.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-newsticker)
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

(setq newsticker-dir "~/.emacs.d/deepin-emacs/Configure-File/Newsticker")  ;种子目录
(setq newsticker-url-list-defaults nil)                      ;设置默认的列表为空
(setq newsticker-automatically-mark-items-as-old t)          ;自动标记项目为已经检索的项目
(setq newsticker-automatically-mark-visited-items-as-old t)  ;自动标记已经访问过的项目
(setq newsticker-retrieval-interval 600)                     ;newsticker更新的时间周期(second)
(setq newsticker-html-renderer 'w3m-region)                  ;用w3m处理HTML格式的信息
(setq newsticker-retrieval-method 'extern)                   ;用wget抓取
(setq newsticker-treeview-treewindow-width 40)               ;树列表宽度
(setq newsticker-treeview-listwindow-height 30)              ;消息窗口高度
(setq newsticker-wget-arguments '("-q" "-O" "-"              ;wget抓取参数
                                  "--user-agent" "testing")) ;--user-agent参数是为了从google上抓取
(setq newsticker-url-list
      '(("EmacsWiki Recently Change"    ;包括小的改进
         "http://www.emacswiki.org/cgi-bin/emacs?action=rss;showedit=1"
         nil nil nil)
        ("Planet Emacsen"
         "http://planet.emacsen.org/atom.xml"
         nil 86400 nil)
        ("Haskell Planet"
         "http://planet.haskell.org/rss20.xml"
         nil 86400 nil)
        ("Bo Wen Wang"
         "http://feeds.bowenwang.com.cn"
         nil 86400 nil)
        ("Google Mi"
         "http://www.chromi.org/feed"
         nil 1800 nil)))

(provide 'init-newsticker)

;;; init-newsticker.el ends here
