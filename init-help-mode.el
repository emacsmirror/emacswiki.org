;;; init-help-mode.el --- Init help mode

;; Filename: init-help-mode.el
;; Description: Init help mode
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 14:47:42
;; Version: 0.1
;; Last-Updated: 2013-12-30 14:47:42
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-help-mode.el
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
;; Init help mode
;;

;;; Installation:
;;
;; Put init-help-mode.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-help-mode)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-help-mode RET
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

(require 'one-key)
(require 'apropos)

;;; Code:

(defvar one-key-menu-help-alist nil
  "The `one-key' menu alist for help.")

(setq one-key-menu-help-alist
      '(
        ;; Apropos.
        (("z" . "Apropos") . apropos)
        (("{" . "Apropos Library") . apropos-library)
        (("\"" . "Apropos Value") . apropos-value)
        (("C" . "Apropos Command") . apropos-command)
        (("F" . "Apropos Function") . apropos-function)
        (("V" . "Apropos Variable") . apropos-variable)
        (("O" . "Apropos Option") . apropos-option)
        (("a" . "Apropos Command") . apropos-command)
        (("d" . "Apropos Documentation") . apropos-documentation)
        ;; Describe.
        (("/" . "Describe Input Method") . describe-input-method)
        (("f" . "Describe Function") . describe-function)
        (("g" . "Describe Gnu Project") . describe-gnu-project)
        (("h" . "Describe Hash") . describe-hash)
        (("b" . "Describe Bindings") . describe-bindings)
        (("c" . "Describe Command") . describe-command)
        (("m" . "Describe Mode") . describe-mode)
        (("k" . "Describe Key") . describe-key)
        (("K" . "Describe Key Briefly") . describe-key-briefly)
        (("o" . "Describe Option") . describe-option)
        (("p" . "Describe Char") . describe-char)
        (("s" . "Describe Syntax") . describe-syntax)
        (("u" . "Describe Unbound Keys") . describe-unbound-keys)
        (("v" . "Describe Variable") . describe-variable)
        (("L" . "Describe Language Environment") . describe-language-environment)
        (("w" . "Describe No Warranty") . describe-no-warranty)
        (("M-f" . "Describe Face") . describe-face)
        (("M-c" . "Describe Copying") . describe-copying)
        (("M-f" . "Describe File") . describe-file)
        (("M-k" . "Describe Keymap") . describe-keymap)
        (("M-t" . "Describe Option Of Type") . describe-option-of-type)
        ;; Info.
        (("M-i" . "Info") . info)
        (("M-o" . "Info Other Window") . info-other-window)
        (("M-s" . "Info Lookup Symbol") . info-lookup-symbol)
        (("M-k" . "Info Goto Emacs Key Command Node") . Info-goto-emacs-key-command-node)
        (("M-m" . "Info Emacs Manual") . info-emacs-manual)
        ;; Xray.
        (("M-B" . "Xray Help Buffer") . xray-help-buffer)
        (("M-S" . "Xray Help Symbol") . xray-help-symbol)
        (("M-W" . "Xray Help Window") . xray-help-window)
        (("M-H" . "Xray Help Hooks") . xray-help-hooks)
        (("M-M" . "Xray Help Marker") . xray-help-marker)
        (("M-O" . "Xray Help Overlay") . xray-help-overlay)
        (("M-P" . "Xray Help Position") . xray-help-position)
        (("M-E" . "Xray Help Screen") . xray-help-screen)
        (("M-Z" . "Xray Help Frame") . xray-help-frame)
        (("M-X" . "Xray Help Features") . xray-help-features)
        (("M-C" . "Xray Help Faces") . xray-help-faces)
        ;; View.
        (("C-d" . "View Emacs Debugging") . view-emacs-debugging)
        (("C-e" . "View External Packages") . view-external-packages)
        (("C-f" . "View Emacs FAQ") . view-emacs-FAQ)
        (("C-n" . "View Emacs News") . view-emacs-news)
        (("C-p" . "View Emacs Problems") . view-emacs-problems)
        (("C-t" . "View Emacs Todo") . view-emacs-todo)
        (("C-r" . "View Order Manuals") . view-order-manuals)
        (("C-E" . "View Echo Area Messages") . view-echo-area-messages)
        (("C-l" . "View Lossage") . view-lossage)
        (("C-n" . "View Emacs News") . view-emacs-news)
        ;; Misc.
        (("C-F" . "Eyedropper Background") . eyedropper-background)
        (("C-B" . "Eyedropper Foreground") . eyedropper-foreground)
        (("C-P" . "Finder By Keyword") . finder-by-keyword)
        (("C-u" . "Display Local Help") . display-local-help)
        (("C-a" . "About Emacs") . about-emacs)
        (("C-h" . "Help For Help") . help-for-help)
        (("C-H" . "Help With Tutorial") . help-with-tutorial)
        (("C-s" . "Wtf Is") . wtf-is)
        (("C-z" . "Sys Apropos") . sys-apropos)
        (("C-w" . "Where Is") . where-is)
        (("x" . "Find Function On Key") . find-function-on-key)
        ))

(defun one-key-menu-help ()
  "The `one-key' menu for help."
  (interactive)
  (one-key-menu "help" one-key-menu-help-alist t nil nil nil t))

;;; ### Apropos ###
;;; --- 程序员命令查询
(lazy-set-key
 '(
   ("C-m" . apropos-follow)                ;进入
   ("N" . forward-button-with-line-begin)  ;下一个条目
   ("P" . backward-button-with-line-begin) ;上一个条目
   ("J" . scroll-up-one-line)              ;向上滚动一行
   ("K" . scroll-down-one-line)            ;向下滚动一行
   ("q" . quit-window)                     ;退出
   ("f" . push-button)                     ;确定
   )
 apropos-mode-map
 )
(lazy-set-key sdcv-key-alist apropos-mode-map)    ;sdcv的局部按键绑定
(lazy-set-key vi-move-key-alist apropos-mode-map) ;vi-move 的局部按键

(provide 'init-help-mode)

;;; init-help-mode.el ends here
