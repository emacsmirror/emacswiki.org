;;; init-indent.el --- Init indent config

;; Filename: init-indent.el
;; Description: Init indent config
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-28 01:18:27
;; Version: 0.1
;; Last-Updated: 2013-12-28 01:18:27
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-indent.el
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
;; Init indent config
;;

;;; Installation:
;;
;; Put init-indent.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-indent)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-indent RET
;;

;;; Change log:
;;
;; 2013/12/28
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

(defun adjust-languages-indent (n)
  ;; Java/C/C++
  (setq-local c-basic-offset n)

  ;; Web development
  (setq-local coffee-tab-width n)
  (setq-local javascript-indent-level n)
  (setq-local js-indent-level n)
  (setq-local js2-basic-offset n)

  (setq-local web-mode-attr-indent-offset n)
  (setq-local web-mode-attr-value-indent-offset n)
  (setq-local web-mode-code-indent-offset n)
  (setq-local web-mode-css-indent-offset n)
  (setq-local web-mode-markup-indent-offset n)
  (setq-local web-mode-sql-indent-offset n)

  (setq-local css-indent-offset n))

(defun use-two-spaces-instead-tab ()
  (setq indent-tabs-mode nil)
  (adjust-languages-indent 2))

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'asm-mode-hook
               'emms-tag-editor-mode-hook
               'sh-mode-hook
               'haskell-cabal-mode-hook
               'ruby-mode-hook
               'qml-mode-hook
               'web-mode-hook
               'scss-mode-hook
               'coffee-mode-hook
               ))
  (add-hook hook '(lambda () (use-two-spaces-instead-tab))))

(provide 'init-indent)

;;; init-indent.el ends here
