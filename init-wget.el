;;; init-wget.el --- Wget configuration

;; Filename: init-wget.el
;; Description: Wget configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:40:31
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:40:31
;;           By: Andy Stewart
;; URL:
;; Keywords: wget
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
;; Wget configuration
;;

;;; Installation:
;;
;; Put init-wget.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-wget)
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

(setq wget-basic-options '("-v"))                                 ;wget1.7版本以下版本设置
(setq wget-basic-options (cons "-equiet=off" wget-basic-options)) ;防止wget下载失败
(setq wget-basic-options (cons "-P." wget-basic-options))         ;设置目录设置
(setq wget-download-directory my-default-download-directory)      ;wget的下载目录
(setq wget-default-options '("-c"                                 ;断点续传
                             "-nv"                                ;不显示详细信息
                             "--passive-ftp"                      ;被动ftp传输
                             ))

(provide 'init-wget)

;;; init-wget.el ends here
