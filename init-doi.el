;;; init-doi.el --- Init for doi

;; Filename: init-doi.el
;; Description: Init for doi
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-03 23:02:16
;; Version: 0.1
;; Last-Updated: 2014-01-03 23:02:16
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-doi.el
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
;; Init for doi
;;

;;; Installation:
;;
;; Put init-doi.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-doi)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-doi RET
;;

;;; Change log:
;;
;; 2014/01/03
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

(require 'cus-edit)

;;; Code:

;;; ### Doi ###
;;; --- Do Or Insert
(defvar doi-key-alist nil
  "The key alist that like doi.")
(setq doi-key-alist
      '(("SPC" . doi-scroll-up)            ;向上滚动一屏
        ("e" . doi-scroll-down)            ;向下滚动一屏
        ("k" . doi-previous-line)          ;上一行
        ("j" . doi-next-line)              ;下一行
        ("h" . doi-backward-char)          ;后一个字符
        ("l" . doi-forward-char)           ;前一个字符
        ("J" . doi-scroll-up-one-line)     ;向上滚动一行
        ("K" . doi-scroll-down-one-line)   ;向下滚动一行
        ("H" . doi-backward-word)          ;后一个词
        ("L" . doi-forward-word)           ;前一个词
        ("y" . doi-sdcv-search-pointer+)   ;翻译
        ("s" . doi-isearch-forward)        ;向前搜索
        ("r" . doi-isearch-backward)       ;向后搜索
        ("A" . doi-move-beginning-of-line) ;移动到行首
        ("E" . doi-move-end-of-line)       ;移动到行末
        ("," . doi-end-of-buffer)          ;移动到buffer末尾
        ("." . doi-beginning-of-buffer)    ;移动到buffer开头
        ))
(lazy-set-mode-autoload-key doi-key-alist custom-mode-map nil "doi")

(provide 'init-doi)

;;; init-doi.el ends here
