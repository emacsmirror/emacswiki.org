;;; init-flycheck.el --- Configuration for flycheck

;; Filename: init-flycheck.el
;; Description: Configuration for flycheck
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-07-04 21:35:23
;; Version: 0.3
;; Last-Updated: 2018-07-10 07:58:50
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-flycheck.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `flycheck'
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
;; Configuration for flycheck
;;

;;; Installation:
;;
;; Put init-flycheck.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-flycheck)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-flycheck RET
;;

;;; Change log:
;;
;; 2018/07/10
;;      * Turn off js2 mode warnings
;;
;; 2018/07/05
;;      * Use `posframe' for MacOS, bug has fixed at: https://www.emacswiki.org/emacs/init-startup.el
;;
;; 2018/07/04
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
(require 'flycheck)

;;; Code:

;; I don't like `global-flycheck-mode', some mode, such as elisp mode don't need.
(dolist (hook (list
               'ruby-mode-hook
               'python-mode-hook
               'swift-mode-hook
               'go-mode-hook
               ))
  (add-hook hook '(lambda () (flycheck-mode 1))))

;; Use posframe as flycheck UI.
(with-eval-after-load 'flycheck
  (require 'flycheck-posframe)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; Add flycheck for swift.
(add-hook 'swift-mode-hook
          (lambda ()
            (require 'flycheck-swift)
            (flycheck-swift-setup)
            ))

;; Don't show missing semi warning in js2-mode.
(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-mode-show-strict-warnings nil)))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
