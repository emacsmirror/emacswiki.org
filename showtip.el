;;; showtip.el --- Show tip at cursor

;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 29 Mar 2008
;; Version: 0.01
;; Keywords: help

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This library export one function for elisp programer to show
;; tooltip near the cursor not the mouse.

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup showtip nil
  "Customization group for showtip"
  :group 'help)

(defcustom showtip-timeout 10
  "Seconds to wait before displaying a tooltip the first time."
  :type 'number
  :group 'showtip)

(defcustom showtip-top-adjust 40
  "Basic adjust."
  :type 'number
  :group 'showtip)

(defface showtip-face '((((class color)) :inherit tooltip))
  "face to display items"
  :group 'showtip)

(defun showtip-frame-posn-at-point (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window."
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (let ((x-y (posn-x-y (posn-at-point position window)))
        (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car  edges))
          (+ (cdr x-y) (cadr edges)))))

(defun showtip (text)
  "Show tooltip text near cursor."
  (let ((pos (showtip-frame-posn-at-point))
        (fg (face-attribute 'showtip-face :foreground nil 'tooltip))
        (bg (face-attribute 'showtip-face :background nil 'tooltip))
        (params tooltip-frame-parameters)
        (top-adjust (- (+ (if menu-bar-mode 25 0)
                          (if tool-bar-mode 35 0)
                          showtip-top-adjust)
                       (if header-line-format
                           (frame-char-height) 0)))
        (max-width 84)
        (max-height 30)
        (frame-height (frame-pixel-height))
        (frame-width (frame-pixel-width))
        (lines (split-string text "\n"))
        width height left top)
    (setq height (* (frame-char-height) (min max-height (length lines))))
    (setq lines (nbutlast lines (- (length lines) (min max-height (length lines)))))
    (setq width (* (frame-char-width)
                   (min max-width (apply 'max (mapcar 'string-width lines)))))
    (setq left (+ (frame-parameter nil 'left) (frame-char-width))
          top (frame-parameter nil 'top))
    ;; if the cursor is at near the right frame fringe or at bottom
    ;; of the bottom fringe, move the frame to
    ;; -frame-width or -frame-height from right or bottom
    (if (< (- frame-width (car pos)) width)
        (setq left (+ left (max 0 (- frame-width width))))
      (setq left (+ left (car pos))))
    (if (< (- frame-height (+ (cdr pos) top-adjust)) height)
        (setq top (+ top frame-height (- height)))
      (setq top (+ top (cdr pos))))
    (setq top (+ top top-adjust))
    (when (stringp fg)
      (setq params (append params `((foreground-color . ,fg)
                                    (border-color . ,fg)))))
    (when (stringp bg)
      (setq params (append params `((background-color . ,bg)))))
    (setq params (append params `((left . ,left)
                                  (top . ,top))))
    (x-show-tip (propertize text 'face 'showtip-face)
                (selected-frame) params showtip-timeout)))

(provide 'showtip)
;;; showtip.el ends here
