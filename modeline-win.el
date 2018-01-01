;;; modeline-win.el --- Highlight buffer name in mode line for selected window.
;;
;; Filename: modeline-win.el
;; Description:  Highlight buffer name in mode line for selected window.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2015-2018, Drew Adams, all rights reserved.
;; Created: Fri Jul 10 09:37:03 2015 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon Jan  1 15:04:08 2018 (-0800)
;;           By: dradams
;;     Update #: 39
;; URL: https://www.emacswiki.org/emacs/download/modeline-win.el
;; Doc URL: https://www.emacswiki.org/emacs/ModeLineSelectedWindow
;; Keywords: mode-line, buffer, window
;; Compatibility: GNU Emacs: 24.4+, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Highlight buffer name in mode-line for selected window.
;;
;;  Minor mode `mlw-mode-line-buf-id-sel-win-mode' uses face
;;  `mlw-mode-line-buffer-id-selected-window' to highlight the buffer
;;  name of the selected window.
;;
;;  This library requires Emacs 24.4 or later.  It has no effect for
;;  earlier Emacs versions.
;;
;;
;;  Commands defined here:
;;
;;    `mlw-mode-line-buf-id-sel-win-mode'.
;;
;;  Faces defined here:
;;
;;    `mlw-mode-line-buffer-id-selected-window'.
;;
;;  Non-interactive functions defined here:
;;
;;    `mlw-pre-redisplay-selected-window'.
;;
;;  Internal variables defined here:
;;
;;    `mlw-orig-mode-line-buf-id',
;;    `mlw-pre-redisplay-selected-window'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015/07/10 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(when (boundp 'pre-redisplay-function)  ; Emacs 24.4+

  (defface mlw-mode-line-buffer-id-selected-window
    '((t (:box (:line-width 1 :color "green") :background "#EF47FFFFC847")))
    "Face for `mode-line-buffer-identification' of selected window."
    :group 'mode-line-faces :group 'basic-faces)

  (defvar mlw-pre-redisplay-selected-window nil
    "Window selected before redisplay.")

  (defvar mlw-orig-mode-line-buf-id (default-value 'mode-line-buffer-identification)
    "Original default value of `mode-line-buffer-identification'.")

  (defun mlw-pre-redisplay-selected-window (_windows)
    "Set `mlw-pre-redisplay-selected-window' to window selected before redisplay."
    (with-demoted-errors "`mlw-pre-redisplay-selected-window': %S"
      (unless (eq (selected-window) (active-minibuffer-window))
        (setq mlw-pre-redisplay-selected-window  (selected-window)))))

  (define-minor-mode mlw-mode-line-buf-id-sel-win-mode
    "Highlight `mode-line-buffer-identification' for selected window."
    nil nil nil :global t
    (cond (mlw-mode-line-buf-id-sel-win-mode
           (add-function :before pre-redisplay-function #'mlw-pre-redisplay-selected-window)
           (setq-default mode-line-buffer-identification
                         (let ((mlbi  (default-value 'mode-line-buffer-identification)))
                           `((:eval (list (propertize
                                           (format-mode-line ',mlbi)
                                           'face (if (eq mlw-pre-redisplay-selected-window
                                                         (get-buffer-window))
                                                     'mlw-mode-line-buffer-id-selected-window
                                                   'mode-line-buffer-id))))))))
          (t
           (remove-function pre-redisplay-function #'mlw-pre-redisplay-selected-window)
           (setq mlw-pre-redisplay-selected-window  nil)
           (setq-default mode-line-buffer-identification mlw-orig-mode-line-buf-id))))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'modeline-win)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modeline-win.el ends here
