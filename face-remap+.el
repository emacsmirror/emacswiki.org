;;; face-remap+.el --- Extensions to standard library `face-remap.el'.
;;
;; Filename: face-remap+.el
;; Description: Extensions to standard library `face-remap.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2009-2023, Drew Adams, all rights reserved.
;; Created: Wed Jun 17 14:26:21 2009 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Mar 19 14:44:31 2023 (-0700)
;;           By: dradams
;;     Update #: 209
;; URL: https://www.emacswiki.org/emacs/download/face-remap%2b.el
;; Doc URL: https://emacswiki.org/emacs/SetFonts
;; Keywords: window frame face font
;; Compatibility: GNU Emacs: 23.x, 24.x, 25.x, 26.x
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
;;  changing its scale factor.
;;
;;  In vanilla Emacs:
;;
;;  * When you shrink or enlarge the apparent text size this way,
;;    however, the window takes no notice of it.  In particular,
;;    although shrinking text can result in extra horizontal space at
;;    the right, window commands do not see this space as extra.
;;
;;  * If you change the major mode in a buffer that you've
;;    text-scaled, that mode change resets the text size, so you lose
;;    any scaling you've done in it.
;;
;;  This library provides two enhancements to standard library
;;  `face-remap.el':
;;
;;  1. You can optionally have text-scaling also automatically resize
;;     the selected window (horizontally, vertically, or both) when
;;     text is resized, so that the way the window fits the buffer
;;     text remains relatively constant.  Shrinking the text in one
;;     window shrinks that window, giving more space to adjacent
;;     windows.  This is governed by user option
;;     `text-scale-resize-window'.
;;
;;     If you also use library `fit-frame.el', then one-window frames
;;     also respond to text resizing by scaling.  If not, then the
;;     text-scale commands have no effect on frame size for one-window
;;     frames.
;;
;;  2. You can optionally make text-scaling be permanently
;;     buffer-local, for all buffers.  This has the effect that if you
;;     change the major mode in a buffer that's been text-scaled, that
;;     mode change has no effect on the text size: whatever size you
;;     scaled it to remains in effect.  This is governed by global
;;     minor mode `text-scale-keep-mode'.
;;
;;  For Emacs versions 23-28, this library also fixes a regression
;;  (bugs #46973 and #54114) introduced in 23 - it provides the Emacs
;;  29 version of function `face-remap-set-base'.
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
;;  Commands defined here:
;;
;;    `text-scale-keep-mode'.
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
;;; Change Log:
;;
;; 2023/03/19 dadams
;;     Added: text-scale-keep-mode.
;; 2022/02/23 dadams
;;     Added vanilla Emacs 29 version of face-remap-set-base (fixes regression).
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
  "Non-nil means text scaling resizes the window or frame accordingly.
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

;;;###autoload
(define-minor-mode text-scale-keep-mode
  "Keep the same text-scaling when a buffer changes major mode.
Disabling the mode restores the vanilla Emacs behavior, where changing
the major mode loses a buffer's current text-scaling.  This minor mode
is global: it affects all buffers."
  :init-value nil :global t
  (put 'face-remapping-alist 'permanent-local text-scale-keep-mode))


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

;; This is the vanilla Emacs 29 version, fixing bugs #46973 and #54114 - a regression.
;;
(when (and (> emacs-major-version 22)  (< emacs-major-version 29))

  (defun face-remap-set-base (face &rest specs)
    "Set the base remapping of FACE in the current buffer to SPECS.
This causes the remappings specified by `face-remap-add-relative'
to apply on top of the face specification given by SPECS.

The remaining arguments, SPECS, specify the base of the remapping.
Each one of SPECS should be either a face name or a property list
of face attribute/value pairs, like in a `face' text property.

If SPECS is empty or a single face `eq' to FACE, call `face-remap-reset-base'
to use the normal definition of FACE as the base remapping; note that
this is different from SPECS containing a single value nil, which means
not to inherit from the global definition of FACE at all."
    ;; Simplify the specs in the case where it's just a single face (and
    ;; it's not a list with just a nil).
    (while (and (consp specs) (not (null (car specs))) (null (cdr specs)))
      (setq specs (car specs)))
    (if (or (null specs)
	    (eq specs face))            ; default
        ;; Set entry back to default
        (face-remap-reset-base face)
      ;; Set the base remapping
      (make-local-variable 'face-remapping-alist)
      (let ((entry (assq face face-remapping-alist)))
        (if entry
	    (setcar (last entry) specs)	; overwrite existing base entry
	  (push (list face specs) face-remapping-alist)))
      ;; Force redisplay of this buffer.
      (force-mode-line-update)))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'face-remap+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; face-remap+.el ends here
