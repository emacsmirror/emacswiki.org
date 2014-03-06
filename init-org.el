;;; init-org.el --- Org configuration

;; Filename: init-org.el
;; Description: Org configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:39:30
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:39:30
;;           By: Andy Stewart
;; URL:
;; Keywords: org-mode
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
;; Org configuration
;;

;;; Installation:
;;
;; Put init-org.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-org)
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

(setq org-hide-leading-stars t)         ;使星号不可见
(setq org-enable-table-editor 1)        ;启用内建的电子表格
(setq org-log-done t)                   ;日志记录
(setq org-log-done '(done))             ;日志记录类型
(setq org-agenda-include-diary t)       ;集成日历
(add-hook 'org-mode-hook
          '(lambda ()
             (smiley-buffer (current-buffer)) ;自动转换笑脸
             ))
(org-remember-insinuate)                ;Org-remeber 初始化
(setq org-directory "/usr/share/deepin-emacs/Org/")   ;设置默认的目录
(setq org-default-notes-file            ;设置默认的笔记文件
      (concat org-directory "Dream.org"))
(setq org-remember-templates            ;设置 Remeber 模板信息
      '(
        ("Todo" ?o "* TODO %?\n  %i\n  %a" "/usr/share/deepin-emacs/Org/Dream.org" "Other")
        ))

(provide 'init-org)

;;; init-org.el ends here
