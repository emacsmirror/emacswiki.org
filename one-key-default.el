;;; one-key-default.el --- one-key-ize default key bindings
;; $Id: one-key-default.el,v 1.4 2009/02/17 00:01:19 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: one-key, convenience, help
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/one-key-default.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; == Quote From Commentary of one-key.el ==
;; One Key provide a handle with TOP keystroke, and then when you
;; type TOP keystroke, you will get a keystroke menu with pop-up
;; window, and will show a group keystroke in pop-up window.
;;
;; Then you just type keystroke in window show, you can execute
;; command corresponding.
;;
;; So you need just remember the TOP keystroke with group command.
;; Others keystroke notify will display in pop-up window.
;; == End of Quote ==
;;
;; This file provides One Key configuration to learn Emacs' default
;; key bindings.

;;; Installation:

;; Add below code AT THE BOTTOM OF your ~/.emacs
;;
;; (require 'one-key-default)
;; (one-key-default-setup-keys)

;;; History:

;; $Log: one-key-default.el,v $
;; Revision 1.4  2009/02/17 00:01:19  rubikitch
;; Replace global prefix maps with one-key-menu-* only when the key is prefix-map.
;;
;; Revision 1.3  2009/02/09 22:43:43  rubikitch
;; added (require 'one-key)
;;
;; Revision 1.2  2009/02/09 22:42:53  rubikitch
;; refactoring. installation doc.
;;
;; Revision 1.1  2009/02/09 18:00:07  rubikitch
;; Initial revision
;;

;;; Code:

(defvar one-key-default-version "$Id: one-key-default.el,v 1.4 2009/02/17 00:01:19 rubikitch Exp $")
(require 'one-key)

(defun one-key-default-create-menu (key &rest depends)
  (ignore-errors
    (dolist (key depends)
      (let ((sym (intern (format "one-key-menu-%s"
                                 (replace-regexp-in-string " " "-" key)))))
        (one-key-default-set-key key sym)))
    (with-temp-buffer
      (one-key-insert-template key key)
      (eval-buffer))))

(defun one-key-default-set-key (keystroke command)
  (let ((kb (read-kbd-macro keystroke)))
    (when (keymapp (key-binding kb))
      (global-set-key kb command))))

(defun one-key-default-setup-keys ()
  (one-key-default-create-menu "ESC ESC")
  (one-key-default-create-menu "ESC" "ESC ESC")
  (one-key-default-create-menu "C-x RET")
  (one-key-default-create-menu "C-x ESC")
  (one-key-default-create-menu "C-x 4")
  (one-key-default-create-menu "C-x 5")
  (one-key-default-create-menu "C-x n")
  (one-key-default-create-menu "C-x r")
  (one-key-default-create-menu "C-x v")
  (one-key-default-create-menu "M-g ESC")
  (one-key-default-create-menu "M-g" "M-g ESC")
  (one-key-default-create-menu "M-o ESC")
  (one-key-default-create-menu "M-o" "M-o ESC")
  (one-key-default-create-menu "<f1> 4")
  (one-key-default-create-menu "<f1>" "<f1> 4")
  (one-key-default-create-menu "C-x a i")
  (one-key-default-create-menu "C-x a" "C-x a i")
  ;;(one-key-default-create-menu "C-x @")
  (one-key-default-create-menu "C-x"
                               "C-x RET" "C-x ESC" "C-x 4" "C-x 5" "C-x a"
                               "C-x n" "C-x r" "C-x v")

  (one-key-default-set-key "<f1>" 'one-key-menu-<f1>)
  (one-key-default-set-key "M-g" 'one-key-menu-M-g)
  (global-set-key (kbd "C-x ?") 'one-key-menu-C-x)
  (setq one-key-help-window-max-height nil))
;; (one-key-default-setup-keys)
;; (one-key-show-template "ESC ESC" "esc2")
;; (one-key-show-template "ESC" "esc2")
(provide 'one-key-default)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "one-key-default.el")
;;; one-key-default.el ends here
