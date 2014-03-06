;;; init-shell-command.el --- Init shell command

;; Filename: init-shell-command.el
;; Description: Init shell command
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 14:56:08
;; Version: 0.1
;; Last-Updated: 2013-12-30 14:56:08
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-shell-command.el
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
;; Init shell command
;; 

;;; Installation:
;;
;; Put init-shell-command.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-shell-command)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-shell-command RET
;;

;;; Change log:
;;	
;; 2013/12/30
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

(require 'shell-command-extension)

;;; Code:

(defvar one-key-menu-backup-file-alist nil
  "The `one-key' menu alist for BACKUP-FILE.")

(setq one-key-menu-backup-file-alist
      '(
        (("e" . "Emacs") . (lambda () (interactive) (shell-aliase "bake")))
        (("x" . "XMonad") . (lambda () (interactive) (shell-aliase "bakx")))
        (("q" . "Qemu") . (lambda () (interactive) (shell-aliase "bakq")))
        (("v" . "VirtualBox") . (lambda () (interactive) (shell-aliase "bakv")))
        (("s" . "Stardict") . (lambda () (interactive) (shell-aliase "baks")))
        (("c" . "Configure File") . (lambda () (interactive) (shell-aliase "bakc")))
        (("p" . "Projects") . (lambda () (interactive) (shell-aliase "bakp")))
        (("l" . "Package List") . (lambda () (interactive) (shell-aliase "bakl")))
        (("h" . "Hanatee") . (lambda () (interactive) (shell-aliase "bakh")))
        (("d" . "Deepin Emacs") . (lambda () (interactive) (shell-aliase "bakd")))
        (("a" . "All") . (lambda () (interactive) (shell-aliase "bak")))
        ))

(defun one-key-menu-backup-file ()
  "The `one-key' menu for BACKUP-FILE."
  (interactive)
  (one-key-menu "BACKUP-FILE" one-key-menu-backup-file-alist t))

(provide 'init-shell-command)

;;; init-shell-command.el ends here
