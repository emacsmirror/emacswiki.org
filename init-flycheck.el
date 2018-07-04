;;; init-flycheck.el --- Configuration for flycheck

;; Filename: init-flycheck.el
;; Description: Configuration for flycheck
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-07-04 21:35:23
;; Version: 0.1
;; Last-Updated: 2018-07-04 21:35:23
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
               ))
  (add-hook hook '(lambda () (flycheck-mode 1))))

;; Use tooltip show errors.
;; Don't use tooltip in Mac, it's font too smalll!!!
(unless (string-equal system-type "darwin")
  (require 'flycheck-pos-tip)
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

(add-hook 'swift-mode-hook
          (lambda ()
            (require 'flycheck-swift)
            (flycheck-swift-setup)
            ))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
