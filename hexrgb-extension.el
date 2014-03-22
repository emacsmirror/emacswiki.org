;;; hexrgb-extension.el --- Extension for hexrgb

;; Filename: hexrgb-extension.el
;; Description: Extension for hexrgb
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-03-22 14:38:31
;; Version: 0.1
;; Last-Updated: 2014-03-22 14:38:31
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/hexrgb-extension.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; `hexrgb' `paredit-extension'
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
;; In GUI programming, we always want convert color from hex string to rgb list, or reverse.
;; hexrgb-extension.el is provide two functions make life better:
;;      `rgb2hex' convert rgb list around point to hex string
;;      `hex2rgb' convert hex string around point to rgb list
;;

;;; Installation:
;;
;; Put hexrgb-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'hexrgb-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET hexrgb-extension RET
;;

;;; Change log:
;;
;; 2014/03/22
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

(require 'hexrgb)
(require 'paredit-extension)

;;; Code:

(defun rgb2hex ()
  "Convert rgb color around point to hex format."
  (interactive)
  (save-excursion
    (let (color-start
          color-end
          color-string)
      (if mark-active
          (progn
            (setq color-start (region-beginning))
            (setq color-end (region-end)))
        (if (paredit-in-string-p)
            (progn
              (setq color-start (1+ (car (paredit-string-start+end-points))))
              (setq color-end (cdr (paredit-string-start+end-points))))
          (setq color-start (progn
                              (backward-up-list)
                              (forward-char +1)
                              (point)))
          (setq color-end (progn
                            (up-list)
                            (forward-char -1)
                            (point)))))
      (setq color-string (buffer-substring color-start color-end))
      (kill-region color-start color-end)
      (paredit-splice-sexp+)
      (insert "\"" (hexrgb-color-values-to-hex (mapcar (lambda (x) (string-to-number x)) (split-string color-string)) 2) "\""))))

(defun hex2rgb ()
  "Convert hex color around point to rgb format."
  (interactive)
  (save-excursion
    (let (color-start
          color-end
          color-string
          in-string-p)
      (if mark-active
          (progn
            (setq color-start (region-beginning))
            (setq color-end (region-end)))
        (if (paredit-in-string-p)
            (progn
              (setq color-start (1+ (car (paredit-string-start+end-points))))
              (setq color-end (cdr (paredit-string-start+end-points)))
              (setq in-string-p t)
              )
          (setq color-start (beginning-of-thing 'sexp))
          (setq color-end (end-of-thing 'sexp))))
      (setq color-string (buffer-substring color-start color-end))
      (kill-region color-start color-end)
      (if in-string-p
          (paredit-splice-sexp+))
      (insert (format "%s" (hexrgb-hex-to-color-values color-string)))
      )))

(provide 'hexrgb-extension)

;;; hexrgb-extension.el ends here
