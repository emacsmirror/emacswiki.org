;;; init-sdcv.el --- Configuration file for sdcv.el

;; Filename: init-sdcv.el
;; Description: Configuration file for sdcv.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-03-26 11:44:00
;; Version: 0.1
;; Last-Updated: 2009-03-26 11:44:00
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-sdcv.el
;; Keywords: sdcv
;; Compatibility: GNU Emacs 23.0.91.1
;;
;; Features that might be required by this library:
;;
;; `sdcv'
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
;; Configuration file for sdcv.el
;;

;;; Installation:
;;
;; Put init-sdcv.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-sdcv)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-sdcv RET
;;

;;; Change log:
;;
;; 2009/03/26
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
(require 'sdcv)

;;; Code:

(setq sdcv-dictionary-simple-list       ;星际译王屏幕取词词典, 简单, 快速
      '("懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "KDic11万英汉词典"))
(setq sdcv-dictionary-complete-list     ;星际译王的词典, 完全, 详细
      '("KDic11万英汉词典"
        "懒虫简明英汉词典"
        "朗道英汉字典5.0"
        "XDICT英汉辞典"
        "朗道汉英字典5.0"
        "XDICT汉英辞典"
        "懒虫简明汉英词典"
        "牛津英汉双解美化版"
        "stardict1.3英汉辞典"
        "英汉汉英专业词典"
        "CDICT5英汉辞典"
        "Jargon"
        "FOLDOC"
        "WordNet"))

(provide 'init-sdcv)

;;; init-sdcv.el ends here

