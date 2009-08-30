;;; xgtags-extension.el --- Some extension for xgtags.el

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-09-23 13:01:19
;; Version: 0.1
;; Last-Updated: 2008-09-23 13:01:23
;; URL: not distributed yet
;; Keywords: gtags, xtags
;; Compatibility: GNU Emacs 23.0.60.1

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

;; Features that might be required by this library:
;;
;;  `gtags' `xgtags'
;;

;;; Installation:
;;
;; Copy xgtags-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'xgtags-extension)
;;
;; No need more

;;; Commentary:
;;
;; This package is some extension for xgtags.
;;

;;; Change log:
;;
;; 2008/09/23
;;         First released.
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
(require 'gtags)
(require 'xgtags)

;;; Code:

(defvar xgtags-select-window-configuration nil
  "Window configuration that xgtags select.")

(defun generate-gtags-files ()
  "Generate gtags reference file for global."
  (interactive)
  (cd
   (read-from-minibuffer
    "directory: "
    default-directory))
  (shell-command "gtags --gtagslabel gtags")
  (xgtags-make-complete-list))

(defun xgtags-select-tag-return-window ()
  "Return window configuration before run `xgtags-select-tag-other-window'."
  (interactive)
  (when xgtags-select-window-configuration
    (set-window-configuration xgtags-select-window-configuration)
    (setq xgtags-select-window-configuration nil)))

(defun xgtags-select-tag-other-window ()
  "Select gtag tag other window."
  (interactive)
  (if (not xgtags-select-window-configuration)
      (setq xgtags-select-window-configuration (current-window-configuration)))
  (xgtags-select-tag-near-point)
  (delete-other-windows)
  (split-window-vertically 16)
  (switch-to-buffer "*xgtags*"))

(defun xgtags-find-rtag-no-prompt ()
  "Input tag name and move to the referenced point."
  (interactive)
  (xgtags--goto-tag (funcall 'xgtags--token-at-point) 'reference))

(defun xgtags-select-next-tag-line ()
  "Select next tag line"
  (interactive)
  (let (remember-point)
    (setq remember-point (point))
    (if  (search-forward-regexp "^.*\\[[0-9]+\\].*\n" nil t)
        (progn
          (forward-line -1)
          (forward-char 1))
      (goto-char remember-point))))

(defun xgtags-select-prev-tag-line ()
  "Select previous tag line"
  (interactive)
  (let (remember-point)
    (setq remember-point (point))
    (if  (search-backward-regexp "^.*\\[[0-9]+\\].*\n" nil t)
        (forward-char 1)
      (goto-char remember-point))))

(defun xgtags-select-next-tag-line-show ()
  "Move next tag line and show at other window."
  (interactive)
  (xgtags-select-next-tag-line)
  (xgtags-select-tag-other-window)
  (forward-char 1)
  )

(defun xgtags-select-prev-tag-line-show ()
  "Move previous tag line and show at other window."
  (interactive)
  (xgtags-select-prev-tag-line)
  (xgtags-select-tag-other-window)
  (forward-char 1)
  )

(defun xgtags-select-next-file ()
  "Select next file"
  (interactive)
  (let (remember-point)
    (setq remember-point (point))
    (if  (search-forward-regexp "^/.*\n" nil t)
        (progn
          (forward-line -1)
          (forward-char 1))
      (goto-char remember-point))))

(defun xgtags-select-prev-file ()
  "Select previous file"
  (interactive)
  (let (remember-point)
    (setq remember-point (point))
    (if  (search-backward-regexp "^/.*\n" nil t)
        (forward-char 1)
      (goto-char remember-point))))

(provide 'xgtags-extension)

;;; xgtags-extension.el ends here

;;; LocalWords:  xgtags gtags xtags gtagslabel gtag rtag
