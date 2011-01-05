;;; face-remap+.el --- Extensions to standard library `face-remap.el'.
;;
;; Filename: face-remap+.el
;; Description: Extensions to standard library `face-remap.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2009-2011, Drew Adams, all rights reserved.
;; Created: Wed Jun 17 14:26:21 2009 (-0700)
;; Version: 23.1
;; Last-Updated: Tue Jan  4 09:06:21 2011 (-0800)
;;           By: dradams
;;     Update #: 156
;; URL: http://www.emacswiki.org/cgi-bin/wiki/face-remap+.el
;; Keywords: window frame face font
;; Compatibility: GNU Emacs 23.x
;;
;; Features that might be required by this library:
;;
;;   `face-remap'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;  Commands `text-scale-decrease', `text-scale-increase', and
;;  `text-scale-adjust' (bound to `C-x C--', `C-x C-+', `C-x C-=', and
;;  `C-x C-0') let you resize the text in the current buffer by
;;  changing its scale factor.  When you shrink or enlarge the
;;  apparent text size this way, however, the window takes no notice
;;  of it.  In particular, although shrinking text can result in extra
;;  horizontal space at the right, window commands do not see this
;;  space as extra.
;;
;;  With this library, user option `text-scale-resize-window' lets you
;;  automatically resize the selected window (horizontally,
;;  vertically, or both) when text is resized, so that the way the
;;  window fits the buffer text remains relatively constant.
;;  Shrinking the text in one window shrinks that window, giving more
;;  space to adjacent windows.
;;
;;  If you also use library `fit-frame.el', then one-window frames
;;  also respond to text resizing by scaling.  If not, then the
;;  text-scale commands have no effect on frame size for one-window
;;  frames.
;;
;;  See also:
;;
;;  * Library `zoom-frm.el', which provides commands `zoom-in' and
;;    `zoom-out', which let you zoom the text in a buffer (as in text
;;    scaling) or the text in an frame.  In the latter case, the
;;    default font of the frame is enlarged or shrunk dynamically.
;;
;;  * Library `doremi-frm.el', which provides commands
;;    `doremi-buffer-font-size+' and `doremi-frame-font-size+', which
;;    provide another way to zoom incrementally.
;;
;;  To use library `face-remap+.el', put it in your `load-path' and
;;  put this sexp in your init file (~/.emacs):
;;
;;   (require 'face-remap+)
;;
;;
;;  Options (user variables) defined here:
;;
;;    `text-scale-resize-window'.
;;
;;
;;  ***** NOTE: The following standard functions defined in `face-remap.el'
;;              have been REDEFINED HERE:
;;
;;    `text-scale-increase' -- Possibly resize the window or frame.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2009/06/22 dadams
;;     Removed vestigial defvar (unused variable).
;; 2009/06/17 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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

(require 'face-remap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defcustom text-scale-resize-window t
  "*Non-nil means text scaling resizes the window or frame accordingly.
For example, if you use `C-x C--' (`text-scale-decrease')' to make the
text smaller, then the window or frame is made smaller by a similar
factor.

If the window is not alone in its frame, then the window is resized.
Otherwise, the frame is resized (provided you also use library
`fit-frame.el').  The frame is always resized both horizontally and
vertically."
  :type '(choice
          (const :tag "Do not resize window when scale text"  nil)
          (const :tag "Resize window when scale text"         t)
          (const :tag "Resize only horizontally"              horizontally)
          (const :tag "Resize only vertically"                vertically))
  :group 'display)


;; REPLACES ORIGINAL `text-scale-increase' defined in `face-remap.el',
;;
;; Resize window or frame if `text-scale-resize-window' is non-nil.
;;
;;;###autoload
(defun text-scale-increase (inc)
  "Increase the height of the default face in the current buffer by INC steps.
If the new height is other than the default, `text-scale-mode' is enabled.

Each step scales the height of the default face by the variable
`text-scale-mode-step' (a negative number of steps decreases the
height by the same amount).  As a special case, an argument of 0
removes any scaling currently active.

If option `text-scale-resize-window' is non-nil, then the selected
window or frame is resized accordingly, so as to keep roughly the same
text visible in the window.  Normally, it is the window that is
resized, but if the window is alone in its frame (and if you use
library `fit-frame.el'), then the frame is resized instead.

See option `text-scale-resize-window' for the possible behaviors."
  (interactive "p")
  (let* ((oamount       (if text-scale-mode text-scale-mode-amount 0))
         (scale-factor  (expt text-scale-mode-step (if (= inc 0) (- oamount) inc)))
         (use-frame-p   (and (fboundp 'fit-frame) (one-window-p 'nomini)))
         (edges         (if use-frame-p (window-inside-edges) (window-edges)))
         (owidth        (- (nth 2 edges) (nth 0 edges)))
         ;; If resizing frame, don't count header line offset (Top) - just use Bottom.
         (oheight       (- (nth 3 edges) (if use-frame-p 0 (nth 1 edges)))))
    (setq text-scale-mode-amount
          (if (= inc 0) 0 (+ (if text-scale-mode text-scale-mode-amount 0) inc)))
    (text-scale-mode (if (zerop text-scale-mode-amount) -1 1))
    (when text-scale-resize-window
      (if use-frame-p
          (let* ((width           (round (* owidth  scale-factor)))
                 (height          (round (* oheight scale-factor)))
                 (fparams         (frame-parameters))
                 (tool-bar-lines  (or (cdr (assq 'tool-bar-lines fparams)) 0))
                 (menu-bar-lines  (or (cdr (assq 'menu-bar-lines fparams)) 0))
                 ;; `window-line-height' doesn't seem to work - I filed Emacs bug #3602.
                 (header-line     (window-line-height 'header-line)))
            ;; `set-frame-size' includes frame's menu-bar, tool-bar, and minibuffer.
            (when (cdr (assq 'modeline  fparams)) (setq height  (1+ height)))
            (when (cdr (assq 'minibuffer fparams)) (setq height  (1+ height)))
            (when header-line (setq height (+ height 1)))
            (setq height  (+ height tool-bar-lines menu-bar-lines))
            (fit-frame nil width height))
        (unless (eq text-scale-resize-window 'vertically)
          (condition-case nil
              (enlarge-window-horizontally (round (- (* owidth scale-factor) owidth)))
            (error nil)))
        (unless (eq text-scale-resize-window 'horizontally)
          (condition-case nil
              (enlarge-window (round (- (* oheight scale-factor) oheight)))
            (error nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'face-remap+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; face-remap+.el ends here
