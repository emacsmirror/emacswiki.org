;;; init-shell.el --- Some configuration for shell-mode

;; Filename: init-shell.el
;; Description: Some configuration for shell-mode
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-10 11:35:12
;; Version: 0.1
;; Last-Updated: 2008-12-10 11:35:14
;;           By: Andy Stewart
;; URL:
;; Keywords: shell,
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
;; Some configuration for shell-mode
;;

;;; Installation:
;;
;; Put init-shell.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-shell)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/10
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

;;; ### Cominit ###
;;; --- Cominit 模式
(setq comint-prompt-read-only t)        ;提示符只读

;;; ### Shell ###
;;; --- Shell 模式
(setq shell-command-completion-mode t)     ;开启命令补全模式
(setq shell-file-name multi-shell-command) ;设置shell的文件名字

(provide 'init-shell)

;;; init-shell.el ends here
