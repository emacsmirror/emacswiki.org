;;; init-moccur.el --- Init moccur

;; Filename: init-moccur.el
;; Description: Init moccur
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-27 23:33:54
;; Version: 0.1
;; Last-Updated: 2013-12-27 23:33:54
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-moccur.el
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
;; Init moccur
;; 

;;; Installation:
;;
;; Put init-moccur.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-moccur)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-moccur RET
;;

;;; Change log:
;;	
;; 2013/12/27
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

(require 'color-moccur)                 ;人性化的搜索功能
(require 'moccur-edit)                  ;搜索编辑

;;; Code:

;;; ### Color moccur ###
;;; --- 增强的Buffer搜索
(setq moccur-kill-moccur-buffer t)         ;退出buffer时自动关闭其buffer
(setq moccur-edit-highlight-edited-text t) ;高亮编辑过的内容
(defadvice moccur-edit-change-file         ;编辑过后自动保存buffer
  (after save-after-moccur-edit-buffer activate)
  "Automatically save buffer when edit in moccur."
  (save-buffer))

(provide 'init-moccur)

;;; init-moccur.el ends here
