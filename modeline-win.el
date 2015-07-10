;;; modeline-win.el --- Highlight buffer name in mode line for selected window.
;;
;; Filename: modeline-win.el
;; Description:  Highlight buffer name in mode line for selected window.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2015, Drew Adams, all rights reserved.
;; Created: Fri Jul 10 09:37:03 2015 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Fri Jul 10 09:44:56 2015 (-0700)
;;           By: dradams
;;     Update #: 7
;; URL: http://www.emacswiki.org/modeline-char.el
;; Doc URL: http://www.emacswiki.org/emacs/ModeLineSelectedWindow
;; Keywords: mode-line, buffer, window
;; Compatibility: GNU Emacs 24.4 and later
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Highlight buffer name in mode line for selected window.
;;
;;  Highlights using face `mode-line-buffer-id-selected-window'.
;;
;;  This library requires Emacs 24.4 or later.  It has no effect for
;;  earlier Emacs versions.
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(when (boundp 'pre-redisplay-function)  ; Emacs 24.4+

  (defface mode-line-buffer-id-selected-window
      '((t (:box (:line-width 1 :color "green") :background "#EF47FFFFC847")))
    "Face for `mode-line-buffer-identification' of selected window."
    :group 'mode-line-faces :group 'basic-faces)

  (defvar pre-redisplay-selected-window nil)

  (defun pre-redisplay-selected-window (_windows)
    "Set `pre-redisplay-selected-window' to window selected before redisplay."
    (with-demoted-errors "`pre-redisplay-selected-window': %S"
      (unless (eq (selected-window) (active-minibuffer-window))
        (setq pre-redisplay-selected-window  (selected-window)))))

  (add-function :before pre-redisplay-function #'pre-redisplay-selected-window)

  (setq-default mode-line-buffer-identification
                (let ((mlbi  (default-value 'mode-line-buffer-identification)))
                  `((:eval (list (propertize (format-mode-line ',mlbi)
                                             'face (if (eq pre-redisplay-selected-window
                                                           (get-buffer-window))
                                                       'mode-line-buffer-id-selected-window
                                                     'mode-line-buffer-id)))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'modeline-win)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modeline-win.el ends here
