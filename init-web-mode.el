 ;;; init-web-mode.el --- Init web mode

;; Filename: init-web-mode.el
;; Description: Init web mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014 ~ 2018 Andy Stewart, all rights reserved.
;; Created: 2014-03-06 15:50:39
;; Version: 0.2
;; Last-Updated: 2018-06-11 14:12:44
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-web-mode.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; `web-mode' `emmet-mode' `tagedit'
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
;; Init web mode
;;

;;; Installation:
;;
;; Put init-web-mode.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-web-mode)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-web-mode RET
;;

;;; Change log:
;;
;; 2018/06/11
;;      * Set `web-mode-tag-auto-close-style' with 2, make auto close tag more smart.
;;      * Binding Ctrl + k to `tagedit-kill', it's much better than paredit-kill for web-mode.
;;      * Add some frequent commands in web-mode-map.
;;
;; 2014/03/06
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

(require 'web-mode)
(require 'emmet-mode)
(require 'tagedit)

;;; Code:

(dolist (hook (list
               'sgml-mode-hook
               'css-mode-hook
               'web-mode-hook
               ))
  (add-hook hook (lambda ()
                   (setq emmet-preview-default nil) ;don't show preview when expand code
                   (emmet-mode)
                   )))

(lazy-set-key paredit-key-alist web-mode-map)
(lazy-set-mode-autoload-key
 '(
   ("M-(" . web-mode-element-wrap+)
   ("M-)" . web-mode-element-unwrap)
   ("M-R" . web-mode-element-rename)
   ("M-s-SPC" . web-mode-element-content-select)
   ("C-s-l" . web-mode-element-clone)
   ("C-M-SPC" . web-mode-mark-and-expand)
   ("%" . web-mode-match-paren)
   ("C-:" . web-mode-comment-or-uncomment)
   ("C-k" . tagedit-kill)
   )
 web-mode-map nil "web-mode-extension")
(lazy-set-mode-autoload-key
 '(
   ("C-c C-r" . mc/mark-sgml-tag-pair))
 web-mode-map nil "multiple-cursors")
(setq web-mode-tag-auto-close-style 2) ;2 mean auto-close with > and </.

(provide 'init-web-mode)

;;; init-web-mode.el ends here
