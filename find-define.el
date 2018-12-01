;;; find-define.el --- Jump to the definition of a function or variable

;; Filename: find-define.el
;; Description: Find function or variable definition.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-13 17:19:58
;; Version: 0.6
;; Last-Updated: 2018-12-01 12:17:01
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/find-define.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
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
;; Jump to the definition of a function or variable, and support multiple programming languages
;;
;; `find-define' supports the following programming languages:
;;
;; | Language   | Backend   |
;; | Elisp      | elisp-def |
;; | Python     | jedi-core |
;; | Golang     | go-mode   |
;; | JavaScript | tide      |
;; | Other      | dumb-jump |
;;

;;; Installation:
;;
;; Put find-define.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;;
;;     (add-to-list 'load-path (expand-file-name "~/elisp"))
;;     (require 'find-define)
;;
;; And binding any key you like with below commands:
;;
;; `find-define'                (I like use Ctrl + 8)
;; `find-define-back'           (I like use Ctrl + 7)
;;

;;; Customize:
;;
;;

;;; Change log:
;;
;; 2018/12/01
;;      * Refactory code.
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

;;; Code:

;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;
(defun find-define-elisp (&optional prefix)
  (require 'elisp-def)
  (if (null prefix)
      (elisp-def)
    (progn
      (switch-to-buffer-other-window (buffer-name))
      (elisp-def))))

(defun find-define-python (&optional prefix)
  (require 'jedi-core)
  (if (null prefix)
      (jedi:goto-definition)
    (progn
      (switch-to-buffer-other-window (buffer-name))
      (jedi:goto-definition))))

(defun find-define-go (&optional prefix)
  (require 'go-mode)
  (if (null prefix)
      (godef-jump (point))
    (godef-jump-other-window (point))))

(defun find-define-js (&optional prefix)
  (require 'tide)
  (if (null prefix)
      (xref--find-definitions (symbol-name (symbol-at-point)) nil)
    (xref--find-definitions (symbol-name (symbol-at-point)) 'window)))

(defun find-define-common (&optional prefix)
  (require 'dumb-jump)
  (setq dumb-jump-prefer-searcher 'rg)
  (if (null prefix)
      (dumb-jump-go)
    (dumb-jump-go-other-window)))

(defun find-define-remember-position ()
  (point-to-register 8))

;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;
(defun find-define (&optional prefix)
  (interactive "P")
  (find-define-remember-position)
  (cond ((or
          (derived-mode-p 'emacs-lisp-mode)
          (derived-mode-p 'ielm-mode))
         (find-define-elisp prefix))
        ((derived-mode-p 'python-mode)
         (find-define-python prefix))
        ((derived-mode-p 'go-mode)
         (find-define-go prefix))
        ((derived-mode-p 'js-mode)
         (find-define-js prefix))
        (t
         (find-define-common prefix)
         )))

(defun find-define-back ()
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

(provide 'find-define)

;;; find-define.el ends here
