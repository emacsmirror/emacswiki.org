;;; autofit-frame.el --- Automatically resize one-window frames to fit
;;
;; Filename: autofit-frame.el
;; Description: Automatically resize one-window frames to fit.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2000-2011, Drew Adams, all rights reserved.
;; Created: Thu Dec  7 10:06:18 2000
;; Version: 21.0
;; Last-Updated: Fri Feb 25 17:31:26 2011 (-0800)
;;           By: dradams
;;     Update #: 601
;; URL: http://www.emacswiki.org/cgi-bin/wiki/autofit-frame.el
;; Keywords: internal, extensions, convenience, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `fit-frame', `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Automatically resize one-window frames to fit.
;;
;;  Functions are provided here to automatically resize each frame to
;;  fit its selected window, when there is no other window in the
;;  frame.  Standard Emacs primitive functions are redefined to do
;;  this: `display-buffer', `switch-to-buffer', and `pop-to-buffer'.
;;
;;  Automatic frame resizing is also provided here implicitly for
;;  functions `switch-to-buffer-other-window' and
;;  `switch-to-buffer-other-frame', since they ultimately use
;;  `display-buffer'. (Command `switch-to-buffer' does not use
;;  `display-buffer', so it is redefined separately here.)
;;
;;  Put the following in your Emacs initialization file (`~/.emacs'),
;;  in order to provide for automatic frame resizing:
;;
;;    (require 'autofit-frame)
;;    (add-hook 'after-make-frame-functions 'fit-frame)
;;
;;  The second line here causes newly created frames to be fitted to
;;  their buffer (window).  Even if you load `auto-fit-frames.el', you
;;  will still need to do this, because `display-buffer' and so on are
;;  not called when a new frame is created.
;;
;;  To automatically fit frames that show a temporary buffer in their
;;  sole window, add this to your initialization file also:
;;
;;    (add-hook 'temp-buffer-show-hook
;;              'fit-frame-if-one-window 'append)
;;
;;  User option (variable) `autofit-frames-flag' turns on and off the
;;  automatic resizing defined here.  Setting it to nil turns it off:
;;  (setq autofit-frames-flag nil).  You can also bind it to nil to
;;  temporarily inhibit frame resizing in Lisp code:
;;
;;         (let ((autofit-frames-flag nil))...)
;;
;;
;;  New user option (variable) defined here: `autofit-frames-flag'.
;;
;;  New function defined here (useful as a `temp-buffer-show-hook'):
;;
;;    `fit-frame-if-one-window'.
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have been
;;              REDEFINED HERE:
;;
;;  `window--display-buffer-1' -
;;     Resize frame to fit sole window if `autofit-frames-flag'.
;;
;;
;;  ***** NOTE: The following EMACS PRIMITIVES are REDEFINED HERE:
;;
;;  `display-buffer' (prior to Emacs 23 only) -
;;     1) Use `read-buffer' in interactive spec.
;;     2) Resize frame to fit sole window if `autofit-frames-flag'
;;        (and provided buffer was not yet displayed).
;;     3) Raise the frame.
;;     4) Restore point in buffer - fixes unknown Emacs 22 bug.
;;
;;  `pop-to-buffer' (prior to Emacs 23 only) -
;;     Use the `display-buffer' defined here.
;;
;;  `switch-to-buffer' -
;;     1) Use `read-buffer' in interactive spec.
;;     2) If current window is dedicated, then use another window.
;;     3) Resize frame to fit sole window if `autofit-frames-flag'
;;        (unless BUFFER is already the `current-buffer').
;;
;;
;;  This file loads file `fit-frame.el', which provides the main
;;  functionality behind the automatic frame resizing.  See it for
;;  user options to do such things as customize default frame sizes.
;;
;;  The reason for separating the code here from that in
;;  `fit-frame.el' is to let you load that code but not load the code
;;  here, if you do not want to redefine Emacs primitives.
;;
;;  This file was formerly called `shrink-fit-all.el', then
;;  `auto-resize-frames.el'.
;;
;;
;;  See also these files for other frame commands:
;;
;;     `doremi-frm.el'    - Incrementally adjust frame properties
;;                          using arrow keys and/or mouse wheel.
;;
;;     `frame-cmds.el'    - Miscellaneous frame and window commands.
;;
;;     `thumb-frm.el'     - Shrink frames to a thumbnail size and
;;                          restore them again.
;;
;;     `zoom-frm.el'      - Zoom a frame, so that its font becomes
;;                          larger or smaller.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2011/02/25 dadams
;;     switch-to-buffer:
;;       If BUFFER is nil, use other-buffer.  Thx to Martial Boniou.
;; 2011/01/03 dadams
;;     Removed autoload cookies: non def* sexps, non-interactive functions.
;; 2009/05/03 dadams
;;     Use (fboundp 'window--display-buffer-1), not Emacs version test.
;; 2009/03/08 dadams
;;     Do not redefine for Emacs 23+: pop-to-buffer, display-buffer.
;;     Added (Emacs 23+): window--display-buffer-1.
;;     display-buffer: Added save-excursion around save-selected-window.
;; 2007/09/03 dadams
;;     Added: redefinition of pop-to-buffer.
;;     display-buffer: Raise frame.  Needed for buffer in thumbnail frame.
;;                     Don't fit frame if buffer was already displayed.
;; 2007/02/14 dadams
;;     display-buffer: Hack: restore point, to work around some Emacs 22 bug.
;; 2006/03/07 dadams
;;     switch-to-buffer: Bug fix: return destination buffer.  Thx to AndreyZ.
;; 2006/01/07 dadams
;;     Added :link for sending bug report.
;; 2006/01/06 dadams
;;     Removed defgroup - we require fit-frame.el, which does it.
;; 2005/09/02 dadams
;;     switch-to-buffer: Don't require existing buffer.
;; 2005/05/28 dadams
;;     autofit-frames-flag: defvar -> defcustom.  Added defgroup fit-frame.
;;     switch-to-buffer: Use explicit default in read-buffer.
;;       Require misc-fns.el.
;; 2004/10/07 dadams
;;     Renamed resize-* to fit-*, per RMS.
;;     Renamed automatically-resize-frames-p to autofit-frames-flag.
;; 2004/09/21 dadams
;;     Commented-out redefinition of pop-to-buffer.  Removed it from intro.
;; 2004/06/01 dadams
;;     1. Replaced fit-frame-when-display-p, fit-frame-when-pop-to-p,
;;        and fit-frame-when-switch-to-p by
;;        automatically-resize-frames-p.
;;     2. Removed pop-to-buffer and
;;        resize-frame-if-one-window-and-cond.
;;     3. Renamed fit-frame-* to resize-frame-*.  Renamed file.
;;     4. switch-to-buffer: Don't resize if buffer is already current.
;; 2004/05/07 dadams
;;     Updated to work with Emacs 21:
;;       pop-to-buffer has additional arg and doc-string changes
;;       display-buffer has doc-string changes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when

(require 'fit-frame) ;; fit-frame
(require 'strings nil t) ;; (no error if not found) read-buffer
(require 'misc-fns nil t) ;; (no error if not found) another-buffer

(unless (> emacs-major-version 22)      ; Quiet the byte-compiler.
  (defvar display-buffer-reuse-frames))

;;;;;;;;;;;;;;;;;;;;;;;
 
;;; User options ---------------------------------------------------

;;;###autoload
(defcustom autofit-frames-flag t
  "*Non-nil means automatically resize one-window frames to fit buffer."
  :type 'boolean :group 'Fit-Frame      ; Group is defined in `fit-frame.el'.
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
autofit-frame.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions.")))




;;; Non-interactive functions ---------------------------------

;; This is not used here.  It is useful as a `temp-buffer-show-hook':
;;   (add-hook 'temp-buffer-show-hook 'fit-frame-if-one-window 'append)
(defun fit-frame-if-one-window ()
  "Resize frame to fit selected window if it is alone in the frame.
Usable in `temp-buffer-show-hook'.
This does nothing if `autofit-frames-flag' is nil."
  (and (one-window-p t) autofit-frames-flag (fit-frame)))


(unless (> emacs-major-version 22)

  (or (fboundp 'old-pop-to-buffer)
      (fset 'old-pop-to-buffer (symbol-function 'pop-to-buffer)))

  ;; REPLACES ORIGINAL (built-in):
  ;; Calls my version of `display-buffer'.  Needed because built-in
  ;; `pop-to-buffer' calls C version, Fdisplay_buffer, not `display-buffer'.
  ;;
  (defun pop-to-buffer (buffer &optional other-window norecord)
    "Select buffer BUFFER in some window, preferably a different one.
BUFFER may be a buffer, a string (a buffer name), or nil.
 If BUFFER is a string which is not the name of an existing buffer,
    then create a buffer with that name.
 If BUFFER is nil, then choose some other buffer.
 If `pop-up-windows' is non-nil, windows can be split.

If optional second arg OTHER-WINDOW is non-nil, then insist on finding
another window, even if BUFFER is already visible in the selected
window, and ignore `same-window-regexps' and
`same-window-buffer-names'.

Optional third arg NORECORD non-nil means do not put this buffer at
the front of the list of recently selected ones.

Return the buffer switched to.
This uses function `display-buffer' as a subroutine; see the documentation
of `display-buffer' for additional customization information."
    (display-buffer (get-buffer-create buffer))
    (old-pop-to-buffer buffer other-window norecord)))
 
;;; Commands ---------------------------------------------------

(unless (> emacs-major-version 22)

  (or (fboundp 'old-display-buffer)
      (fset 'old-display-buffer (symbol-function 'display-buffer)))

  ;; REPLACES ORIGINAL (built-in):
  ;; 1) Uses `read-buffer' in interactive spec.
  ;; 2) Resizes frame to fit sole window if `autofit-frames-flag'.
  ;; 3) Raise the frame.  In particular, this ensures that a thumbified frame
  ;;    is raised (see `thumb-frm.el').
  ;; 4) Hack to restore point in buffer - fixes unknown Emacs 22 bug.
  ;;
  ;; NOTE: It would be better to rewrite the C code, so that the frame
  ;;       is not resized if the frame is simply _raised_.
  (defun display-buffer (buffer &optional not-this-window frame)
    "Make BUFFER appear in some window but don't select it.
BUFFER can be a buffer or the name of an existing buffer.
If BUFFER is shown already in some window, just use that one,
unless the window is the selected window and the optional second
argument NOT-THIS-WINDOW is non-nil (interactively, with prefix arg).
If `pop-up-frames' is non-nil, make a new frame if no window shows BUFFER.
Return the window displaying BUFFER.

Emacs 21 or later only:
  If `display-buffer-reuse-frames' is non-nil, and another frame is
  currently displaying BUFFER, then simply raise that frame.

The variables `special-display-buffer-names', `special-display-regexps',
`same-window-buffer-names', and `same-window-regexps' customize how certain
buffer names are handled.

If optional argument FRAME is `visible', search all visible frames.
If FRAME is 0, search all visible and iconified frames.
If FRAME is t, search all frames.
If FRAME is a frame, search only that frame.
If FRAME is nil, search only the selected frame
 (actually the last nonminibuffer frame),
 unless `pop-up-frames' or `display-buffer-reuse-frames' is non-nil,
 which means search visible and iconified frames.

Emacs 21 or later only:
  If `even-window-heights' is non-nil, window heights will be evened
  out if displaying the buffer causes two vertically adjacent windows
  to be displayed.

Resizes frame to fit sole window if `autofit-frames-flag', but only if
BUFFER was not yet displayed.  If BUFFER was already displayed, its
frame is assumed to already be the size you want (perhaps you resized
it manually)."
    (interactive
     (list (read-buffer "Display buffer: " (current-buffer) 'existing)
           current-prefix-arg))
    (let* ((wins ())
           (old-wins (progn (walk-windows (lambda (w) (push w wins)) t 0)
                            wins))
           (win (old-display-buffer buffer not-this-window frame))
           (pt (save-excursion (set-buffer buffer) (point))))
      (when (window-live-p win)
        (save-excursion
          (save-selected-window
            (select-window win)
            (raise-frame)
            (goto-char pt)              ; Hack to fix unknown Emacs 22 bug.
            (and (one-window-p t)
                 autofit-frames-flag
                 (not (member win old-wins)) ; Don't fit if already displayed.
                 (fit-frame)))))
      win)))                            ; Return the window



;; REPLACES ORIGINAL defined in `window.el' (Emacs 23+):
;; Resizes frame to fit sole window if `autofit-frames-flag'.
;;
(when (fboundp 'window--display-buffer-1) ; Emacs 23+

  ;; We save the original anyway, but we don't use it.
  (or (fboundp 'old-window--display-buffer-1)
      (fset 'old-window--display-buffer-1
            (symbol-function 'window--display-buffer-1)))

  (defun window--display-buffer-1 (window)
    "Raise the frame containing WINDOW.
Do not raise the selected frame.
Resize frame to fit sole window if `autofit-frames-flag'.
Return WINDOW."
    (let* ((frame (window-frame window))
           (visible (frame-visible-p frame)))
      (unless (or (not visible)
                  ;; Assume the selected frame is already visible enough.
                  (eq frame (selected-frame))
                  ;; Assume frame from which we invoked minibuffer is visible.
                  (and (minibuffer-window-active-p (selected-window))
                       (eq frame (window-frame
                                  (minibuffer-selected-window)))))
        (raise-frame frame)
        (save-excursion
          (save-selected-window
            (select-window window)
            (and (one-window-p t) autofit-frames-flag (fit-frame)))))
      window)))



(or (fboundp 'old-switch-to-buffer)
    (fset 'old-switch-to-buffer (symbol-function 'switch-to-buffer)))

;; REPLACES ORIGINAL (built-in):
;; 1) Uses `read-buffer' in interactive spec.
;; 2) If current window is dedicated, then use another window.
;;    NOTE: Emacs versions >= 19.34 signal error if dedicated window, instead
;;          of using another one.  Don't know what the 19.28 version did.
;;          Don't know if this is needed any more.
;; 3) Resizes frame to fit sole window if `autofit-frames-flag'
;;    (unless BUFFER is already the `current-buffer').
;;;###autoload
(defun switch-to-buffer (buffer &optional norecord)
  "Select buffer BUFFER in current window, unless the window is dedicated.
If current window is dedicated (`window-dedicated-p'), then another window
is used.

BUFFER may be a buffer, a string (a buffer name), or nil.  If BUFFER
is a string that does not identify an existing buffer, then a new
buffer with that name is created.  If BUFFER is nil, then function
`other-buffer' is used to choose a buffer.

Optional second arg NORECORD non-nil means do not put BUFFER at the
front of the list of recently selected buffers.

The buffer switched to is returned.

*WARNING*: This is NOT the way to work on another buffer temporarily
within a Lisp program!  Use `set-buffer' instead, to avoid messing
with correspondences between windows and buffers.

Resizes frame to fit sole window if `autofit-frames-flag'
\(unless BUFFER is already the `current-buffer')."
  (interactive
   (list (read-buffer "Switch to buffer: "
                      (if (fboundp 'another-buffer) ; In `misc-fns.el'.
                          (another-buffer nil t)
                        (other-buffer (current-buffer))))))
  ;; If string arg, convert to a buffer.  If nil, use `other-buffer'.
  (setq buffer  (if buffer (get-buffer-create buffer) (other-buffer)))
  (let ((orig-buf (current-buffer)))
    (prog1 (if (window-dedicated-p (selected-window))
               (switch-to-buffer-other-window buffer)
             (old-switch-to-buffer buffer norecord))
      (and (one-window-p t)
           (not (eq buffer orig-buf))   ; Don't resize if same buffer.
           autofit-frames-flag
           (fit-frame)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'autofit-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autofit-frame.el ends here
