;;; show-help.el --- Show help as showtip

;; Filename: show-help.el
;; Description: Show help as showtip
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-16 08:33:03
;; Version: 0.1
;; Last-Updated: 2008-10-16 08:33:08
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/show-help.el
;; Keywords: help, tooltip, showtip, eldoc
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `showtip'
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
;; This code was inspired by tooltip-help.el by Tamas Patrovics.
;; Use `showtip' to replace the toolitp display code of
;; `tooltip-help.el'.
;;
;; Show help information as showtip.
;; When your text cursor in at a valid symbol, use function
;; `sh-show-help' can get help information.
;;
;; `sh-show-help' will get different help information along with
;; the type of current symbol.
;; Now can support function, variable, face for emacs-lisp-mode.
;;

;;; Installation:
;;
;; Put show-help.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'show-help)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/16
;;      First released.
;;

;;; Acknowledgements:
;;
;;      Tamas Patrovics for created tooltip-help.el
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'showtip)

;;; Code:

(defun sh-show-help ()
  "Show help with current mode as showtip."
  (interactive)
  (let ((help-handler (intern (concat "sh-" (symbol-name major-mode) "-handler")))
        help-message)
    (if (functionp help-handler)
        (progn
          (setq help-message (funcall help-handler))
          (if (equal help-message "")
              (message "No help available.")
            (showtip help-message)
            ))
      (message "The current major mode is not supported"))))

(defun sh-emacs-lisp-mode-handler ()
  "The help handler that for `emacs-lisp-mode'."
  (let (help)
    (setq help "")
    (if (equal 0 (variable-at-point))
        (if (function-called-at-point)
            (setq help (sh-elisp-get-help-text 'describe-function (function-called-at-point)))
          (if (face-at-point)
              (setq help (sh-elisp-get-help-text 'describe-variable (variable-at-point t)))))
      (setq help (sh-elisp-get-help-text 'describe-variable (variable-at-point))))))

(defun sh-elisp-get-help-text (func symbol)
  "Return help as text with special function and SYMBOL.
Argument FUNC get help information.
Argument SYMBOL special symbol at current point."
  (flet ((message (&rest args)))        ;for filter output message in echo area
    (let ((pop-up-frames nil)
          (wincfg (current-window-configuration)))
      (if (get-buffer "*Help*")
          (kill-buffer "*Help*"))

      (funcall func symbol)
      (if (get-buffer "*Help*")
          (progn
            (set-window-configuration wincfg)
            (with-current-buffer "*Help*" (buffer-string)))
        ""))))

(defun sh-lisp-interaction-mode-handler ()
  (sh-emacs-lisp-mode-handler))

(provide 'show-help)

;;; show-help.el ends here

;;; LocalWords:  showtip eldoc Patrovics toolitp func args wincfg
