;;; find-define.el --- Find function or variable definition.

;; Filename: find-define.el
;; Description: Find function or variable definition.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-13 17:19:58
;; Version: 0.5
;; Last-Updated: 2018-09-01 20:54:49
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/find-define.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `dumb-jump' `elisp-def'
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
;; So i use `elisp-defs' instead `dumb-jump-go' if current mode is emacs-lisp mode.
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
;; 2018/09/01
;;      * Use `elisp-def.el' instead my `find-func-extension.el', `elisp-def.el' is more smarter.
;;
;; 2018/06/15
;;      * Python mode use `jedi:goto-definition'
;;
;; 2018/06/13
;;      * Set `dumb-jump-prefer-searcher' with rg.
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
(require 'elisp-def)
(require 'dumb-jump)

;;; Code:

;; Add `elisp-def-mode' in elisp mode.
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook #'elisp-def-mode))

;; Prefer use rg, because rg is much faster than ag, grep, ack.
;; If rg is not installed, use other grep tools.
(setq dumb-jump-prefer-searcher 'rg)

(defun find-define-go (&optional prefix)
  (interactive "P")
  (setq find-define-symbol-cache (thing-at-point 'symbol))
  (find-define-remember-position)
  (cond ((equal 'emacs-lisp-mode major-mode)
         (find-elisp-define prefix))
        ((equal 'python-mode major-mode)
         (find-python-define prefix))
        (t
         (if (null prefix)
             (dumb-jump-go)
           (dumb-jump-go-other-window))
         )))

(defun find-elisp-define (&optional prefix)
  (interactive "P")
  (if (null prefix)
      (elisp-def)
    (progn
      (switch-to-buffer-other-window (buffer-name))
      (elisp-def))))

(defun find-python-define (&optional prefix)
  (interactive "P")
  (require 'jedi-core)
  (if (null prefix)
      (jedi:goto-definition)
    (progn
      (switch-to-buffer-other-window (buffer-name))
      (jedi:goto-definition))))

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
