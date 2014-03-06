;;; init-apt-utils.el --- Init for apt utils

;; Filename: init-apt-utils.el
;; Description: Init for apt utils
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-29 22:43:13
;; Version: 0.1
;; Last-Updated: 2013-12-29 22:43:13
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-apt-utils.el
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
;; Init for apt utils
;;

;;; Installation:
;;
;; Put init-apt-utils.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-apt-utils)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-apt-utils RET
;;

;;; Change log:
;;
;; 2013/12/29
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

(require 'apt-utils)                    ;APT搜索管理工具

;;; Code:

(setq apt-utils-automatic-update t)     ;总是自动重建包列表， 不用询问
(lazy-unset-key
 '("s")
 apt-utils-mode-map)                    ;卸载按键
(lazy-set-key
 '(
   ("#" . apt-utils-rebuild-package-lists)    ;重建包列表
   ("*" . apt-utils-list-package-files)       ;列出包文件
   ("F" . apt-utils-choose-package-link)      ;选择包连接
   ("f" . apt-utils-follow-link)              ;进入连接
   ("<backtab>" . apt-utils-previous-package) ;上一个连接
   ("TAB" . apt-utils-next-package)           ;下一个连接
   ("q" . apt-utils-quit)                     ;退出
   ("d" . apt-utils-describe-package)         ;解释
   ("B" . apt-utils-view-previous-package)    ;上一个视图
   ("J" . scroll-up-one-line)                 ;向上滚动一行
   ("K" . scroll-down-one-line)               ;向下滚动一行
   ("t" . apt-utils-toggle-package-info)      ;切换info
   ("S" . apt-utils-show-package)             ;显示包
   ("v" . one-key-menu-apt-utils-view)        ;查看菜单
   ("s" . one-key-menu-apt-utils-search)      ;搜索菜单
   ("b" . one-key-menu-apt-utils-browse)      ;浏览菜单
   )
 apt-utils-mode-map
 )
(lazy-set-key vi-move-key-alist apt-utils-mode-map) ;vi-move 的局部按键

(defvar one-key-menu-apt-utils-view-alist nil
  "The `one-key' menu alist for APT-UTILS-VIEW.")

(setq one-key-menu-apt-utils-view-alist
      '(
        (("c" . "Changelog") . apt-utils-view-changelog)
        (("C" . "Debian Changelog") . apt-utils-view-debian-changelog)
        (("r" . "Readme") . apt-utils-view-readme)
        (("R" . "Debian Readme") . apt-utils-view-debian-readme)
        (("n" . "News ") . apt-utils-view-news)
        (("N" . "Debian News") . apt-utils-view-debian-news)
        (("e" . "Emacs Startup File") . apt-utils-view-emacs-startup-file)
        (("f" . "Package Files") . apt-utils-view-package-files)
        (("p" . "Copyright") . apt-utils-view-copyright)
        (("m" . "Man Page") . apt-utils-view-man-page)
        (("v" . "Version") . apt-utils-view-version)
        ))

(defun one-key-menu-apt-utils-view ()
  "The `one-key' menu for APT-UTILS-VIEW."
  (interactive)
  (one-key-menu "APT-UTILS-VIEW" one-key-menu-apt-utils-view-alist t t))

(defvar one-key-menu-apt-utils-search-alist nil
  "The `one-key' menu alist for APT-UTILS-SEARCH.")

(setq one-key-menu-apt-utils-search-alist
      '(
        (("s" . "Search") . apt-utils-search)
        (("f" . "Search Filename") . apt-utils-search-file-names)
        (("g" . "Search Grep") . apt-utils-search-grep-dctrl)
        (("n" . "Search Name Only") . apt-utils-search-names-only)
        ))

(defun one-key-menu-apt-utils-search ()
  "The `one-key' menu for APT-UTILS-SEARCH."
  (interactive)
  (one-key-menu "APT-UTILS-SEARCH" one-key-menu-apt-utils-search-alist t t))

(defvar one-key-menu-apt-utils-browse-alist nil
  "The `one-key' menu alist for APT-UTILS-BROWSE.")

(setq one-key-menu-apt-utils-browse-alist
      '(
        (("c" . "Browse Changelog") . apt-utils-web-browse-debian-changelog)
        ((("b" . "Browse Bur Reports") . apt-utils-web-browse-bug-reports))
        (("l" . "Browse Copyright") . apt-utils-web-browse-copyright)
        (("v" . "Browse Versions") . apt-utils-web-browse-versions)
        ))

(defun one-key-menu-apt-utils-browse ()
  "The `one-key' menu for APT-UTILS-BROWSE."
  (interactive)
  (one-key-menu "APT-UTILS-BROWSE" one-key-menu-apt-utils-browse-alist t t))

(provide 'init-apt-utils)

;;; init-apt-utils.el ends here
