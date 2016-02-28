;;; delight-powerline.el --- Integrate powerline.el with delight.el
;;
;; Author: Phil S.
;; URL: http://www.emacswiki.org/emacs/DelightedPowerLine
;; Keywords: convenience
;; Created: 28 Feb 2016
;; Package-Requires: ((delight "1.04") (powerline "1.0"))
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version. See <http://www.gnu.org/licenses/>.

;;; Example usage:
;;
;; ;; Configure delighted modes
;; (require 'delight)
;; (delight 'emacs-lisp-mode "eLisp" :major)
;;
;; ;; Enable powerline
;; (require 'powerline)
;; (require 'delight-powerline)
;; (powerline-default-theme)

;;; Commentary:
;;
;; With the exception of Emacs’ default mode line rendering, anything
;; rendering a mode line construct will call `format-mode-line'. By
;; default, delight.el wants to display delighted major mode names
;; ONLY in the mode line and not elsewhere, and so delighted major
;; mode names are inhibited during calls to `format-mode-line'.
;;
;; Powerline, however, calls `format-mode-line' for the purpose of
;; replacing the standard mode line rendering, in which case we DO
;; want to see the delighted major mode names.
;;
;; As of version 1.04, delight.el respects the value of
;; `inhibit-mode-name-delight' if it has been bound by something else,
;; and so it is possible for other libraries/advice to let-bind this
;; variable (to `nil') around calls to `format-mode-line' in order
;; that the delighted value will be used for major modes.
;;
;; This library implements this behaviour for powerline specifically.

;;; Code:

(defadvice powerline-major-mode (around delight-powerline-major-mode activate)
  "Ensure that powerline's major mode names are delighted.

See `delight-major-mode'."
  (let ((inhibit-mode-name-delight nil))
    ad-do-it))

(provide 'delight-powerline)

;;; delight-powerline.el ends here
