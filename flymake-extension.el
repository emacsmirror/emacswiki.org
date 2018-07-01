;;; flymake-extension.el --- Some extension for flymake

;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-11 23:05:38
;; Version: 0.5
;; Last-Updated: 2018-07-01 21:15:44
;; URL:
;; Keywords: flymake
;; Compatibility: GNU Emacs 27.0.50

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
;; `flymake' `fringe-helper'
;;

;;; Commentary:
;;
;; Some extensions for `flymake'
;;

;;; Installation:
;;
;; Put flymake-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'flymake-extension)
;;
;; No need more

;;; Customize:
;;
;; You can setup option `flymake-extension-use-tooltip' with `t',
;; if you want to show error or warning with tooltip.
;;
;; You can setup option `flymake-extension-auto-show' with `t',
;; if you want to show error or waring automatically.
;;
;; And all above options can customize by:
;;      M-x customize-group RET flymake-extension RET
;;

;;; Change log:
;;
;; 2018/07/01
;;      * Add support for MacOS, use `popup-tip'.
;;
;; 2018/06/20
;;      * Switch from `showtip' to `posframe' to show tooltip.
;;      * Rename `flymake-extension-use-showtip' to `flymake-extension-use-tooltip'.
;;
;; 2009/02/04
;;      * Add new option `flymake-extension-use-showtip'.
;;      * Add new option `flymake-extension-auto-show'.
;;
;; 2008/10/11
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
(require 'flymake)
(require 'fringe-helper)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup flymake-extension nil
  "Some extension functions for flymake."
  :group 'flymake)

(defcustom flymake-extension-use-tooltip t
  "Display error or warning in tooltip.
If nil flymake display error or warning in minibuffer.
Otherwise use `tooltip' display.
If you use X window, you can try to enable this option.
Default is nil."
  :type 'boolean
  :group 'flymake-extension)

(defcustom flymake-extension-auto-show nil
  "Whether show error or warning automatically.
Default is nil."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (if value
             (add-hook 'post-command-hook 'flymake-extension-show+)
           (remove-hook 'post-command-hook 'flymake-extension-show+)))
  :group 'flymake-extension)

(defcustom flymake-extension-tooltip-name "*flymake-extension*"
  "The name of flymake tooltip name."
  :type 'string
  :group 'flymake-extension)

(defcustom flymake-extension-tooltip-timeout 10
  "The timeout of flymake tooltip show time, in seconds."
  :type 'integer
  :group 'flymake-extension)

(defface flymake-extension-tooltip-face
  '((t (:foreground "orange" :background "gray12")))
  "Face for flymake tooltip"
  :group 'flymake-extension)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar multiline-flymake-mode nil)

(defvar flymake-split-output-multiline nil)

(defvar flymake-fringe-overlays nil)
(make-variable-buffer-local 'flymake-fringe-overlays)

(defvar flymake-extension-tooltip-last-point 0
  "Hold last point when show tooltip, use for hide tooltip after move point.")

(defvar flymake-extension-tooltip-last-scroll-offset 0
  "Hold last scroll offset when show tooltip, use for hide tooltip after window scroll.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Show error ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flymake-show-next-error (&optional reversed)
  "Show next error of flymake."
  (interactive)
  (if reversed
      (flymake-goto-prev-error)
    (flymake-goto-next-error))
  (flymake-extension-show))

(defun flymake-show-prev-error()
  "Show previous error of flymake."
  (interactive)
  (flymake-show-next-error t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flymake-report-fatal-status (status warning)
  "Display a warning and switch flymake mode off."
  (flymake-extension-show (format "Flymake: %s. Flymake will be switched OFF" warning))
  (flymake-mode 0))

(defun flymake-extension-show (&optional msg no-echo)
  "Show error or warning.
If option `flymake-extension-use-tooltip' is t, use `tooltip' display.
If MSG nil, try to get current error or warning.
If NO-ECHO t, don't display message when no error or waring."
  ;; Just check when `flymake-mode' is enable.
  (when flymake-mode
    (or msg (setq msg (get-char-property (point) 'help-echo)))
    (if msg
        (progn
          ;; Remove blank lines form information.
          (setq msg (replace-regexp-in-string "^[ \t]*\n" "" msg))
          ;; Show information.
          (if flymake-extension-use-tooltip
              (if (string-equal system-type "darwin")
                  (progn
                    (require 'popup)
                    (popup-tip msg))
                (require 'posframe)
                (posframe-show
                 flymake-extension-tooltip-name
                 :string (concat "\n" msg)
                 :position (point)
                 :timeout flymake-extension-tooltip-timeout
                 :background-color (face-attribute 'flymake-extension-tooltip-face :background)
                 :foreground-color (face-attribute 'flymake-extension-tooltip-face :foreground)
                 :min-height 3)
                (add-hook 'post-command-hook 'flymake-extension-hide-tooltip-after-move)
                (setq flymake-extension-tooltip-last-point (point))
                (setq flymake-extension-tooltip-last-scroll-offset (window-start))
                )
            (message msg)))
      (unless no-echo
        (message "No error or waring.")))))

(defun flymake-extension-hide-tooltip-after-move ()
  (ignore-errors
    (when (get-buffer flymake-extension-tooltip-name)
      (unless (and
               (equal (point) flymake-extension-tooltip-last-point)
               (equal (window-start) flymake-extension-tooltip-last-scroll-offset))
        (posframe-delete flymake-extension-tooltip-name)
        (kill-buffer flymake-extension-tooltip-name)))))

(defun flymake-extension-show+ ()
  "This function is similar with `flymake-extension-show'.
Always try to get error or waring around point.
Except don't display message when no error or waring."
  (flymake-extension-show nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rules for `flymake' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice flymake-split-output
    ;; this needs to be advised as flymake-split-string is used in other places
    ;; and I don't know of a better way to get at the caller's details
    (around flymake-split-output-multiline activate protect)
  (if multiline-flymake-mode
      (let ((flymake-split-output-multiline t))
        ad-do-it)
    ad-do-it))

(defadvice flymake-split-string
    (before flymake-split-string-multiline activate)
  (when flymake-split-output-multiline
    (ad-set-arg 1 "^\\s *$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fringe for `flymake' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice flymake-make-overlay (after add-to-fringe first
                                       (beg end tooltip-text face mouse-face)
                                       activate compile)
  (push (fringe-helper-insert-region
         beg end
         (fringe-lib-load fringe-lib-exclamation-mark)
         'left-fringe
         (if (eq face 'flymake-errline)
             'font-lock-warning-face
           'font-lock-function-name-face))
        flymake-fringe-overlays))

(defadvice flymake-delete-own-overlays (after remove-from-fringe activate compile)
  (mapc 'fringe-helper-remove flymake-fringe-overlays)
  (setq flymake-fringe-overlays nil))

(provide 'flymake-extension)

;;; flymake-extension.el ends here

;;; LocalWords:  flymake haskell impl inplace cmdline perl hg errline
;;; LocalWords:  multiline ghc fbyte hs msg
