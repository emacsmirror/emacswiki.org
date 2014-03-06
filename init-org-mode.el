;;; init-org-mode.el --- Init org mode

;; Filename: init-org-mode.el
;; Description: Init org mode
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 09:56:41
;; Version: 0.1
;; Last-Updated: 2013-12-30 09:56:41
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-org-mode.el
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
;; Init org mode
;; 

;;; Installation:
;;
;; Put init-org-mode.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-org-mode)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-org-mode RET
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

(require 'org)                          ;个人信息管理
(require 'org-extension)                ;Org增强
(require 'org-w3m)                      ;Org w3m 交互转换
(require 'org-oddmuse)                  ;转换 Org-mode 到 Oddmuse 模式

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-org-mode-alist nil
  "The `one-key' menu alist for ORG-MODE.")

(setq one-key-menu-org-mode-alist
      '(
        ;; Move
        (("h" . "Backward Char") . backward-char)
        (("l" . "Forward Char") . forward-char)
        (("j" . "Next Line") . next-line)
        (("k" . "Previous Line") . previous-line)
        (("e" . "Scroll Down") . scroll-down)
        (("SPC" . "Scroll Up") . scroll-up)
        (("J" . "Scroll Up One Line") . scroll-up-one-line)
        (("K" . "Scroll Down One Line") . scroll-down-one-line)
        (("H" . "Next Title") . outline-next-visible-heading)
        (("L" . "Prev Title") . outline-previous-visible-heading)
        (("N" . "Next Same Level") . outline-forward-same-level)
        (("P" . "Prev Same Level") . outline-backward-same-level)
        (("O" . "Up Level") . outline-up-heading)
        ;; TODO item.
        (("y" . "Insert sup TODO heading") . org-insert-sup-todo-heading)
        (("u" . "Insert cur TODO heading") . org-insert-cur-todo-heading)
        (("i" . "Insert sub TODO heading") . org-insert-sub-todo-heading)
        ;; Item.
        (("Y" . "Insert sup item") . org-insert-sup-item)
        (("U" . "Insert cur item") . org-insert-cur-item)
        (("I" . "Insert sub item") . org-insert-sub-item)
        ;; Move current item.
        (("a" . "Move item to left") . org-metaleft)
        (("f" . "Move item to right") . org-metaright)
        (("s" . "Move item to down") . org-metadown)
        (("d" . "Move item to up") . org-metaup)
        ;; Move sub-tree.
        (("A" . "Move tree to left") . org-shiftmetaleft)
        (("F" . "Move tree to right") . org-shiftmetaright)
        (("S" . "Move tree to down") . org-shiftmetadown)
        (("D" . "Move tree to up") . org-shiftmetaup)
        ;; Switch current item mission status.
        (("," . "Switch item status left") . org-shiftleft)
        (("." . "Swtich item status right") . org-shiftright)
        ;; Switch sub-tree misssion status.
        (("<" . "Swtich tree status left") . org-subtree-shiftleft)
        ((">" . "Swtich tree status right") . org-subtree-shiftright)
        ;; Switch priority.
        (("p" . "Up priority") . org-priority-up)
        (("n" . "Down priority") . org-priority-down)
        ;; Copy or Paste.
        (("w" . "Copy Speical") . org-copy-special)
        (("W" . "Paste Speical") . org-cut-special)
        ;; Misc.
        (("TAB" . "Org Cycle") . org-cycle)
        (("c" . "Switch to DONE") . org-switch-item-to-done)
        (("x" . "Archive all done item.") . org-archive-all-done-item)
        (("z" . "Switch and Archive DONE") . org-switch-done-and-archive)
        (("Z" . "Switch and Archive DONE") . org-switch-done-and-archive_)
        (("o" . "Show all TODO item.") . org-display-all-todo-item)
        (("g" . "Org Goto") . org-goto)
        (("/" . "Org Spare Tree") . org-sparse-tree)
        (("^" . "Org Sort") . org-sort)
        ))

(defun one-key-menu-org-mode ()
  "The `one-key' menu for ORG-MODE."
  (interactive)
  (one-key-menu "ORG-MODE" one-key-menu-org-mode-alist t nil nil nil t))

(defun one-key-menu-org-mode-recursive ()
  "This function like `one-key-menu-org-mode', but recursive."
  (interactive)
  (one-key-menu "ORG-MODE RECURSIVE" one-key-menu-org-mode-alist t t nil nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org File ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-org-file-alist nil
  "The `one-key' menu alist for ORG-FILE.")

(setq one-key-menu-org-file-alist
      '(
        (("s" . "Dream") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/Dream.org")))
        (("p" . "Projects") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/LazyCat.org")))
        (("d" . "Debian") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/DebianInstall.org")))
        (("l" . "Living") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/Living.org")))
        (("r" . "ReadingNotes") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/ReadingNotes.org")))
        (("o" . "Others") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/Other.org")))
        (("u" . "Ubuntu") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/UbuntuInstall.org")))
        (("a" . "Haskell Article") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/Haskell_Article.org")))
        (("g" . "Gtk2hs API") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/UpdateGtk2hsApi.org")))
        (("h" . "Hs4Gi") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/Hs4Gi.org")))
        (("i" . "Android") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/AndroidInstall.org")))
        (("n" . "AndroidTodo") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/Android.org")))
        (("m" . "Linux Deepin") . (lambda () (interactive) (find-file "~/.emacs.d/deepin-emacs/Org/LinuxDeepin.org")))
        ))

(defun one-key-menu-org-file ()
  "The `one-key' menu for ORG-FILE."
  (interactive)
  (one-key-menu "ORG-FILE" one-key-menu-org-file-alist t))

(lazy-set-key
 '(
   ("s-u" . one-key-menu-org-mode)           ;Org-mode 菜单
   ("s-U" . one-key-menu-org-mode-recursive) ;Org-mode 菜单, 但是递归的
   ("M-O" . org-display-all-todo-item)       ;显示所有TODO列表
   )
 org-mode-map
 )

(provide 'init-org-mode)

;;; init-org-mode.el ends here
