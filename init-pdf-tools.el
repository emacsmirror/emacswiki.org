;;; init-pdf-tools.el --- Configure file that can load when emacs pdf-tools.

;; Filename: init-pdf-tools.el
;; Description: Configure file that can load when emacs pdf-tools.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-01-12 16:34:11
;; Version: 0.2
;; Last-Updated: 2018-06-12 14:16:37
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-pdf-tools.el
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
;; Configure file that can load when emacs pdf-tools.
;;

;;; Installation:
;;
;; Put init-pdf-tools.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-pdf-tools)
;;
;; Install epdfinfo:
;; Some linux system (such as ArchLinux) not build binary package epdfinfo,
;; make pdf-tools report error cause by `pdf-info-epdfinfo-program' is nil.
;;
;; You can use below commands to install epdfinfo:
;; 1. Install cask:
;;    curl -fsSkL https://raw.github.com/cask/cask/master/go | python
;;    
;; 2. Export cash bin path make pdf-tools know which find cash:
;;    export PATH="/home/yourusername/.cask/bin:$PATH"
;;
;; 3. Download pdf-tools server from git:
;;    git clone https://github.com/politza/pdf-tools
;;
;; 4. Compile epdfinfo:
;;    make
;;
;; 5. Copy epdfinfo to same directory that put `pdf-tools.el'
;;    cp server/epdfinfo ~/pdf-tools-elisp-path/

;;; Change log:
;;
;; 2018/06/12
;;      * Add commands of install epdfinfo, make me can use this awesome extension in ArchLinux. 
;;
;; 2014/03/17
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
(require 'pdf-tools)
(require 'pdf-occur)
(require 'pdf-history)
(require 'pdf-links)
(require 'pdf-outline)
(require 'pdf-annot)
(require 'pdf-sync)
(require 'pdf-tools-extension)

;;; Code:
(pdf-tools-install)

;; midnite mode hook
(add-hook 'pdf-view-mode-hook (lambda ()
                                (pdf-view-midnight-minor-mode))) ; automatically turns on midnight-mode for pdfs

(setq pdf-view-midnight-colors '("#00B800" . "#000000" )) ; set the green profile as default (see below)

(defun pdf-no-filter ()
  "View pdf without colour filter."
  (interactive)
  (pdf-view-midnight-minor-mode -1)
  )

;; change midnite mode colours functions
(defun pdf-midnite-original ()
  "Set pdf-view-midnight-colors to original colours."
  (interactive)
  (setq pdf-view-midnight-colors '("#839496" . "#002b36" )) ; original values
  (pdf-view-midnight-minor-mode)
  )

(defun pdf-midnite-amber ()
  "Set pdf-view-midnight-colors to amber on dark slate blue."
  (interactive)
  (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; amber
  (pdf-view-midnight-minor-mode)
  )

(defun pdf-midnite-green ()
  "Set pdf-view-midnight-colors to green on black."
  (interactive)
  (setq pdf-view-midnight-colors '("#00B800" . "#000000" )) ; green
  (pdf-view-midnight-minor-mode)
  )

(defun pdf-midnite-colour-schemes ()
  "Midnight mode colour schemes bound to keys"
  (local-set-key (kbd "!") (quote pdf-no-filter))
  (local-set-key (kbd "@") (quote pdf-midnite-amber))
  (local-set-key (kbd "#") (quote pdf-midnite-green))
  (local-set-key (kbd "$") (quote pdf-midnite-original))
  )

(add-hook 'pdf-view-mode-hook 'pdf-midnite-colour-schemes)

(lazy-unset-key
 '(".")
 pdf-view-mode-map)                     ;卸载按键
(lazy-unset-key
 '("x" "M-<" "M->")
 pdf-view-mode-map)                     ;卸载一些按键
(lazy-set-key
 '(
   ([remap scroll-up] . pdf-view-next-line-or-next-page) ;重新定向按键, 支持 auto-scroll
   )
 pdf-view-mode-map
 )
(lazy-set-key
 '(
   ("N" . pdf-view-next-page)                      ;下一页
   ("P" . pdf-view-previous-page)                  ;上一页
   ("," . pdf-view-first-page)                     ;第一页
   ("." . pdf-view-last-page)                      ;最后一页
   ("g" . pdf-view-goto-page)                      ;跳到第几页
   ("e" . pdf-view-scroll-down-or-previous-page)   ;向上滚动一屏
   ("SPC" . pdf-view-scroll-up-or-next-page)       ;向下滚动一屏
   ("j" . pdf-view-next-line-or-next-page)         ;下一行或下一屏
   ("k" . pdf-view-previous-line-or-previous-page) ;上一行或上一屏
   ("O" . pdf-occur)                               ;全局搜索
   ("q" . bury-buffer)                             ;隐藏buffer
   ("Q" . kill-this-buffer)                        ;退出
   ("s" . auto-scroll-mode)                        ;自动滚屏
   ("<" . auto-scroll-faster)                      ;加快滚屏速度
   (">" . auto-scroll-slower)                      ;减慢滚屏速度
   )
 pdf-view-mode-map
 )
(lazy-set-key sdcv-key-alist pdf-view-mode-map) ;sdcv的局部按键绑定

(provide 'init-pdf-tools)

;;; init-pdf-tools.el ends here
