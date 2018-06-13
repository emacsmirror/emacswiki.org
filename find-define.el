;;; find-define.el --- Find function or variable definition.

;; Filename: find-define.el
;; Description: Find function or variable definition.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-13 17:19:58
;; Version: 0.2
;; Last-Updated: 2018-06-13 17:47:21
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/find-define.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `find-func-extension' `dumb-jump'
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
;; Find function or variable definition.
;;
;; `dumb-jump' is awesome extension, but it's not smart enough for emacs-lisp mode.
;; So i use my `find-function-or-variable-at-point' instead `dumb-jump-go' if current mode is emacs-lisp mode.
;;

;;; Installation:
;;
;; Put find-define.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'find-define)
;;
;; And binding any key you like with below commands:
;; 
;; `find-define-back'           (I like use Ctrl + 7)
;; `find-define-go'             (I like use Ctrl + 8)
;; `find-define-prompt'         (I like use Ctrl + 9)

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET find-define RET
;;

;;; Change log:
;;
;; 2018/06/13
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
(require 'find-func-extension)
(require 'dumb-jump)

;;; Code:

(defun find-define-go (&optional prefix)
  (interactive "P")
  (setq find-define-symbol-cache (thing-at-point 'symbol))
  (find-define-remember-position)
  (if (equal 'emacs-lisp-mode major-mode)
      (find-function-or-variable-at-point prefix)
    (if (null prefix)
        (dumb-jump-go)
      (dumb-jump-go-other-window))))

(defun find-define-back ()
  (interactive)
  (find-define-restore-position))

(defun find-define-prompt (&optional prefix)
  (interactive "P")
  (if (equal 'emacs-lisp-mode major-mode)
      (find-define-go prefix)
    (dumb-jump-go-prompt)))

(defun find-define-remember-position ()
  (interactive)
  (point-to-register 8))

(defun find-define-restore-position ()
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

(provide 'find-define)

;;; find-define.el ends here
