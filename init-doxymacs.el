;;; init-doxymacs.el --- Init for doxymacs

;; Filename: init-doxymacs.el
;; Description: Init for doxymacs
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-04 11:03:11
;; Version: 0.1
;; Last-Updated: 2014-01-04 11:03:11
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-doxymacs.el
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
;; Init for doxymacs
;;

;;; Installation:
;;
;; Put init-doxymacs.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-doxymacs)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-doxymacs RET
;;

;;; Change log:
;;
;; 2014/01/04
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


;;; Code:

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               ))
  (require 'doxymacs)
  (require 'lazycat-toolkit)
  (doxymacs-font-lock)                                                    ;注释高亮模式
  (add-hook hook 'doxymacs-mode)                                          ;加载文档模式
  (add-hook hook (lambda () (local-set-key (kbd "C-m") 'my-doxymacs-return))) ;注释智能换行
  )

(provide 'init-doxymacs)

;;; init-doxymacs.el ends here
