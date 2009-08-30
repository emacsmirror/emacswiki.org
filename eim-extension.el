;;; eim-extension.el --- Some extension for eim (Emacs Input Method)

;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-06 11:26:51
;; Version: 0.1
;; Last-Updated: 2008-10-06 11:26:54
;; URL:
;; Keywords: eim, input method
;; Compatibility: GNU Emacs 23.0.60.1

;; This file is not part of GNU Emacs

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

;;; Features that might be required by this library:
;;
;; `eim-extra'
;;

;;; Overview:
;;
;; This package is some configuration with `eim-extra'.
;;

;;; Commentary:
;;
;; `eim-extra' is a Emacs Chinese Input Method, this package is some configuration
;; and key binding for `eim-extra'.
;;

;;; Installation:
;;
;; Put eim-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eim-extension)
;;
;; No need more

;;; Configuration:
;;
;;
;;

;;; Change log:
;;
;; 2008/10/06
;;      First released.
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
(require 'eim-extra)

;;; Code:
(autoload 'eim-use-package "eim" "Another emacs input method") ;load
(setq eim-punc-translate-p nil)                                ;user English punctuation
(setq eim-use-tooltip nil)                                     ;don't use tooltip
(register-input-method                                         ;register input method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt"
 'my-eim-py-activate-function)
(set-input-method "eim-py")             ;use Pinyin input method
(setq activate-input-method t)          ;active input method
(toggle-input-method nil)               ;default is turn off
;; key binding for input `eim-extra'.
;; (global-set-key (kbd "M-j") 'toggle-input-method) ;toggle input method
(defun my-eim-py-activate-function ()
  (add-hook 'eim-active-hook
            (lambda ()
              (let ((map (eim-mode-map)))
                (basic-set-key-alist
                 map
                 '(
                   ("M-o" . eim-delete-last-char) ;delete last char
                   ("M-c" . eim-quit-clear)       ;quit and clear
                   ("M-m" . eim-quit-no-clear)    ;quit and no clear
                   ("M-n" . eim-next-page)        ;page down
                   ("M-p" . eim-previous-page)    ;page up
                   ))))))

(provide 'eim-extension)

;;; eim-extension.el ends here

;;; LocalWords:  eim punc py euc cn txt
