;;; eyedropper.el --- Pick foreground and background colors at cursor or pointer.
;; 
;; Filename: eyedropper.el
;; Description: Pick foreground and background colors at cursor or pointer.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006-2009, Drew Adams, all rights reserved.
;; Created: Fri Jun 23 08:07:15 2006
;; Version: 20
;; Last-Updated: Sat Aug  1 15:23:32 2009 (-0700)
;;           By: dradams
;;     Update #: 157
;; URL: http://www.emacswiki.org/cgi-bin/wiki/eyedropper.el
;; Keywords: color, rgb, hsv, hexadecimal, face, frame
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;; 
;; Features that might be required by this library:
;;
;;   `hexrgb'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;;  Use the commands defined here to examine or save the background or
;;  foreground color at the text cursor or the mouse pointer.
;;
;;  After using commands `eyedrop-pick-background-*' or
;;  `eyedrop-pick-foreground-*', the picked color is saved in variable
;;  `eyedrop-picked-background' or `eyedrop-picked-foreground',
;;  respectively.
;;
;;  If you have Emacs 22 or later, all of the functionality provided
;;  here, and much more, is provided in library `palette.el'.  Use
;;  library `eyedropper' instead of `palette.el' if you do not want to
;;  use the color palette or if your Emacs version is older than Emacs
;;  22.  If you load `palette.el', there is no reason to also load
;;  `eyedropper.el'.  However, if for some reason you do load both
;;  `palette.el' and `eyedropper.el', load `palette.el' second, so
;;  that its definitions will override those provided here, providing
;;  additional functionality for the color palette.
;;
;;  To use this library:
;;
;;    Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;      (require 'eyedropper) ; Load this library.
;; 
;;    You will also need my library `hexrgb.el'; it is loaded
;;    automatically by `eyedropper.el'.  Get it here:
;;    http://www.emacswiki.org/cgi-bin/wiki/hexrgb.el.
;; 
;;  Commands defined here:
;;
;;    `background-color', `eyedrop-background-at-mouse',
;;    `eyedrop-background-at-point', `eyedrop-foreground-at-mouse',
;;    `eyedrop-foreground-at-point', `eyedropper-background',
;;    `eyedropper-foreground', `eyedrop-pick-background-at-mouse',
;;    `eyedrop-pick-background-at-point',
;;    `eyedrop-pick-foreground-at-mouse',
;;    `eyedrop-pick-foreground-at-point', `foreground-color',
;;    `pick-background-color', `pick-foreground-color'.
;;
;;  Non-interactive functions defined here:
;;
;;    `eyedrop-color-message', `eyedrop-face-at-point', `keywordp'.
;;
;;  Internal variables defined here:
;;
;;    `eyedrop-last-picked-color', `eyedrop-picked-background',
;;    `eyedrop-picked-foreground'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2007/10/11 dadams
;;     eyedrop-(back|fore)ground-at-(mouse|point),
;;     eyedrop-pick-(back|fore)ground-at-(mouse|point), pick-(back|fore)ground-color:
;;       Added optional MSG-P arg (instead of interactive-p).
;; 2006/07/28 dadams
;;     eyedrop-face-at-point: Use car, not caar, for (*-color . "...") test.
;; 2006/06/25 dadams
;;     Added: eyedrop-last-picked-color.  Set it whenever set picked fg or bg.
;; 2006/06/24 dadams
;;     Added: keywordp (for Emacs 20), eyedrop-face-at-point.
;;     eyedrop-(back|fore)ground-at-point: Use eyedrop-face-at-point also.
;; 2006/06/23 dadams
;;     Created.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'hexrgb) ;; hexrgb-hex-to-rgb, hexrgb-rgb-to-hsv

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eyedrop-picked-background nil
  "Color last picked from a face or frame background.
You can use `eyedrop-pick-background-at-point' or
`eyedrop-pick-background-at-mouse' to pick the color.")

(defvar eyedrop-picked-foreground nil
  "Color last picked from a face or frame foreground.
You can use `eyedrop-pick-foreground-at-point' or
`eyedrop-pick-foreground-at-mouse' to pick the color.")

(defvar eyedrop-last-picked-color nil
  "Color last picked from a face or frame foreground or background.")

;; This is built-in in Emacs 21; not defined before Emacs 21.
(unless (fboundp 'keywordp)
  (defun keywordp (object)
    "Return t if OBJECT is a keyword.
This means that it is a symbol with a print name beginning with `:'
interned in the initial obarray."
    (and (symbolp object) (string-match "^:" (symbol-name object)) t)))

(defun eyedrop-color-message (color)
  "Display information about COLOR as a message."
  (let* ((rgb (hexrgb-hex-to-rgb color))
         (hsv (apply #'hexrgb-rgb-to-hsv rgb)))
    (message (format "Color: %s, RGB: %s, HSV: %s" color rgb hsv)))
  color)                                ; Return it.

(defun eyedrop-background-at-mouse (event &optional msg-p)
  "Return the background color under the mouse pointer.
Non-nil optional arg MSG-P means display an informative message."
  (interactive "e\np")
  ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
  (while (input-pending-p) (discard-input))
  (set-buffer (window-buffer (posn-window (event-end event))))
  (mouse-set-point event)
  (let ((bg (eyedrop-background-at-point)))
    (when msg-p (if bg (eyedrop-color-message bg) (message "No background color here")))
    bg))

(defun eyedrop-foreground-at-mouse (event &optional msg-p)
  "Return the foreground color under the mouse pointer.
Non-nil optional arg MSG-P means display an informative message."
  (interactive "e\np")
  ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
  (while (input-pending-p) (discard-input))
  (set-buffer (window-buffer (posn-window (event-end event))))
  (mouse-set-point event)
  (let ((fg (eyedrop-foreground-at-point)))
    (when msg-p (if fg (eyedrop-color-message fg) (message "No foreground color here")))
    fg))

;; RMS added this function to Emacs (23) as `face-at-point'.
(defun eyedrop-face-at-point ()
  "Return the face under the text cursor.
If there is more than one face, return the first one.
Return nil if there is no face at point."
  (let* ((faceprop (or (get-char-property (point) 'read-face-name)
                       (get-char-property (point) 'face)
                       'default))
         (face (cond ((symbolp faceprop) faceprop)
                     ;; List of faces (don't treat an attribute spec).
                     ;; Just use the first face.
                     ((and (consp faceprop) (not (keywordp (car faceprop)))
                           (not (memq (car faceprop) '(foreground-color background-color))))
                      (car faceprop))
                     (t nil))))         ; Invalid face value.
    (if (facep face) face nil)))

;; RMS added this function to Emacs (23) as `background-color-at-point'.
(defalias 'background-color 'eyedrop-background-at-point)
(defun eyedrop-background-at-point (&optional msg-p)
  "Return the background color under the text cursor.
Non-nil optional arg MSG-P means display an informative message."
  (interactive "p")
  ;; `eyedrop-face-at-point' alone is not sufficient.  It only gets named faces.
  ;; Need also pick up any face properties that are not associated with named faces.
  (let* ((face (or (eyedrop-face-at-point)
                   (get-char-property (point) 'read-face-name)
                   (get-char-property (point) 'face)))
         (bg (cond ((and face (symbolp face))
                    (condition-case nil
                        (face-background face nil 'default) ; Emacs 22.
                      (error (or (face-background face) ; Emacs 20
                                 (cdr (assq 'background-color (frame-parameters)))))))
                   ((consp face)
                    (cond ((memq 'background-color face) (cdr (memq 'background-color face)))
                          ((memq ':background face) (cadr (memq ':background face)))))
                   (t nil))))           ; Invalid face value.
    (when msg-p
      (if bg (eyedrop-color-message bg) (message "No background color here")))
    bg))

;; RMS added this function to Emacs (23) as `foreground-color-at-point'.
(defalias 'foreground-color 'eyedrop-foreground-at-point)
(defun eyedrop-foreground-at-point (&optional msg-p)
  "Return the foreground color under the text cursor.
Non-nil optional arg MSG-P means display an informative message."
  (interactive "p")
  ;; `eyedrop-face-at-point' alone is not sufficient.  It only gets named faces.
  ;; Need also pick up any face properties that are not associated with named faces.
  (let* ((face (or (eyedrop-face-at-point)
                   (get-char-property (point) 'read-face-name)
                   (get-char-property (point) 'face)))
         (fg (cond ((and face (symbolp face))
                    (condition-case nil
                        (face-foreground face nil 'default) ; Emacs 22.
                      (error (or (face-foreground face) ; Emacs 20
                                 (cdr (assq 'foreground-color (frame-parameters)))))))
                   ((consp face)
                    (cond ((memq 'foreground-color face) (cdr (memq 'foreground-color face)))
                          ((memq ':foreground face) (cadr (memq ':foreground face)))))
                   (t nil))))           ; Invalid face value.
    (when msg-p
      (if fg (eyedrop-color-message fg) (message "No foreground color here")))
    fg))

(defun eyedrop-pick-background-at-mouse (event &optional msg-p)
  "Pick background of face or frame at character under the mouse pointer.
Save the background color in `eyedrop-picked-background' and
`eyedrop-last-picked-color'.  Return the picked color.
Non-nil optional arg MSG-P means display an informative message."
  (interactive "e\np")
  (setq eyedrop-picked-background (eyedrop-background-at-mouse event)
        eyedrop-last-picked-color eyedrop-picked-background)
  (unless (stringp eyedrop-picked-background) (error "No background color here to pick"))
  (when msg-p (eyedrop-color-message eyedrop-picked-background))
  eyedrop-picked-background)

(defun eyedrop-pick-foreground-at-mouse (event &optional msg-p)
  "Pick foreground of face or frame at character under the mouse pointer.
Save the foreground color in `eyedrop-picked-foreground' and
`eyedrop-last-picked-color'.  Return the picked color.
Non-nil optional arg MSG-P means display an informative message."
  (interactive "e\np")
  (setq eyedrop-picked-foreground (eyedrop-foreground-at-mouse event)
        eyedrop-last-picked-color eyedrop-picked-foreground)
  (unless (stringp eyedrop-picked-foreground) (error "No foreground color here to pick"))
  (when msg-p (eyedrop-color-message eyedrop-picked-foreground))
  eyedrop-picked-foreground)

(defalias 'eyedropper-background 'eyedrop-pick-background-at-point)
(defalias 'pick-background-color 'eyedrop-pick-background-at-point)
(defun eyedrop-pick-background-at-point (&optional msg-p)
  "Pick background of face or frame at character at text cursor (point).
Save the background color in `eyedrop-picked-background' and
`eyedrop-last-picked-color'.  Return the picked color.
Non-nil optional arg MSG-P means display an informative message."
  (interactive "p")
  (setq eyedrop-picked-background (eyedrop-background-at-point)
        eyedrop-last-picked-color eyedrop-picked-background)
  (unless (stringp eyedrop-picked-background) (error "No background color here to pick"))
  (when msg-p (eyedrop-color-message eyedrop-picked-background))
  eyedrop-picked-background)

(defalias 'eyedropper-foreground 'eyedrop-pick-foreground-at-point)
(defalias 'pick-foreground-color 'eyedrop-pick-foreground-at-point)
(defun eyedrop-pick-foreground-at-point (&optional msg-p)
  "Pick foreground of face or frame at character at text cursor (point).
Save the foreground color in `eyedrop-picked-foreground' and
`eyedrop-last-picked-color'.  Return the picked color.
Non-nil optional arg MSG-P means display an informative message."
  (interactive "p")
  (setq eyedrop-picked-foreground (eyedrop-foreground-at-point)
        eyedrop-last-picked-color eyedrop-picked-foreground)
  (unless (stringp eyedrop-picked-foreground) (error "No foreground color here to pick"))
  (when msg-p (eyedrop-color-message eyedrop-picked-foreground))
  eyedrop-picked-foreground)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'eyedropper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eyedropper.el ends here
