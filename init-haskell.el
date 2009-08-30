;;; init-haskell.el --- Haskell configuration

;; Filename: init-haskell.el
;; Description: Haskell configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:50:26
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:50:26
;;           By: Andy Stewart
;; URL:
;; Keywords: haskell
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
;; Haskell configuration
;;

;;; Installation:
;;
;; Put init-haskell.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-haskell)
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

(load "~/MyEmacs/Site-Lisp/Packages/haskell-mode/haskell-site-file")
(autoload 'haskell-refac-mode "haskell-refac"
  "Minor mode for refactoring Haskell programs" t)
(add-hook 'haskell-mode-hook 'turn-on-font-lock)        ;高亮模式
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)   ;智能缩进模式
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)     ;GHCi 交互模式
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode) ;文档模式
(add-hook 'haskell-mode-hook 'haskell-refac-mode)       ;重构
(add-hook 'haskell-mode-hook 'hs-lint-mode-hook)        ;代码建议

(provide 'init-haskell)

;;; init-haskell.el ends here
