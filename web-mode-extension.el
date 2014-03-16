;;; web-mode-extension.el --- Extension for web-mode

;; Filename: web-mode-extension.el
;; Description: Extension for web-mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-03-14 21:45:07
;; Version: 0.1
;; Last-Updated: 2014-03-16 17:10:13
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/web-mode-extension.el
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
;; Extension for web-mode
;;

;;; Installation:
;;
;; Put web-mode-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'web-mode-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET web-mode-extension RET
;;

;;; Change log:
;;
;; 2014/03/16
;;      * Add `web-mode-element-unwrap'.
;;
;; 2014/03/14
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
(require 'sgml-mode)

;;; Code:

(defun web-mode-match-paren (arg)
  "Go to the matching tag if on tag, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "<")
         (sgml-skip-tag-forward 1))
        ((looking-back ">")
         (sgml-skip-tag-backward 1))
        (t (self-insert-command (or arg 1)))))

(defun web-mode-element-unwrap ()
  "Like `web-mode-element-vanish', but you don't need jump parent tag to unwrap.
Just like `paredit-splice-sexp+' style."
  (interactive)
  (save-excursion
    (progn
      (web-mode-element-parent)
      (web-mode-element-vanish)
      (back-to-indentation)
      )
    ))

(provide 'web-mode-extension)

;;; web-mode-extension.el ends here
