;;; init-startup.el --- Config load when startup

;; Filename: init-startup.el
;; Description: Config load when startup
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-20 23:58:38
;; Versio: 0.5
;; Last-Updated: 2018-07-06 10:02:57
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-startup.el
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
;; Config load when startup
;;

;;; Installation:
;;
;; Put init-startup.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-startup)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-startup RET
;;

;;; Change log:
;;
;; 2018/07/05
;;      * Make emacs fullscreen mode works with MacOS.
;;      * Make emacs fullscreen works perfect even MacOS fullscreen emacs first.
;;      * Don't need `sleep-for' now.
;;
;; 2014/01/20
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

(if (featurep 'cocoa)
    (progn
      ;; 在Mac平台, Emacs不能进入Mac原生的全屏模式,否则会导致 `make-frame' 创建时也集成原生全屏属性后造成白屏和左右滑动现象.
      ;; 所以先设置 `ns-use-native-fullscreen' 和 `ns-use-fullscreen-animation' 禁止Emacs使用Mac原生的全屏模式.
      ;; 而是采用传统的全屏模式, 传统的全屏模式, 只会在当前工作区全屏,而不是切换到Mac那种单独的全屏工作区,
      ;; 这样执行 `make-frame' 先关代码或插件时,就不会因为Mac单独工作区左右滑动产生的bug.
      ;;
      ;; Mac平台下,不能直接使用 `set-frame-parameter' 和 `fullboth' 来设置全屏,
      ;; 那样也会导致Mac窗口管理器直接把Emacs窗口扔到单独的工作区, 从而对 `make-frame' 产生同样的Bug.
      ;; 所以, 启动的时候通过 `set-frame-parameter' 和 `maximized' 先设置Emacs为最大化窗口状态, 启动5秒以后再设置成全屏状态,
      ;; Mac就不会移动Emacs窗口到单独的工作区, 最终解决Mac平台下原生全屏窗口导致 `make-frame' 左右滑动闪烁的问题.
      (setq ns-use-native-fullscreen nil)
      (setq ns-use-fullscreen-animation nil)
      (run-at-time "5sec" nil
                   (lambda ()
                     (let ((fullscreen (frame-parameter (selected-frame) 'fullscreen)))
                       ;; If emacs has in fullscreen status, maximized window first, drag emacs window from Mac's single space.
                       (when (memq fullscreen '(fullscreen fullboth))
                         (set-frame-parameter (selected-frame) 'fullscreen 'maximized))
                       ;; Call `toggle-frame-fullscreen' to fullscreen emacs.
                       (toggle-frame-fullscreen)))))

  ;; 非Mac平台直接全屏
  (require 'fullscreen)
  (fullscreen))

(setq ad-redefinition-action 'accept)   ;不要烦人的 redefine warning
(setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
(tool-bar-mode -1)              ;禁用工具栏
(menu-bar-mode -1)              ;禁用菜单栏
(scroll-bar-mode -1)            ;禁用滚动条
(server-start)                  ;为emacsclient准备使用场景，比如git

(provide 'init-startup)

;;; init-startup.el ends here
