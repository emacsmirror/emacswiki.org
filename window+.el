;;; window+.el --- Extensions to `window.el'.
;;
;; Filename: window+.el
;; Description: Extensions to `window.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2017, Drew Adams, all rights reserved.
;; Created: Thu Jan 25 14:22:13 1996
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 12:00:53 2017 (-0800)
;;           By: dradams
;;     Update #: 215
;; URL: http://www.emacswiki.org/window%2b.el
;; Doc URL: http://emacswiki.org/Delete_Frames_Easily_-_But_Not_Too_Easily
;; Doc URL: http://www.emacswiki.org/OneOnOneEmacs
;; Keywords: internal, window
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `window.el'.
;;
;;
;;  ***** NOTE: The following functions defined in `window.el' have
;;              been REDEFINED HERE:
;;
;;  `count-windows' -
;;     Only use arg MINIBUF if current frame has a minibuffer.
;;
;;  `special-display-popup-frame' (Emacs 24+) - Fit the frame.
;;
;;  `quit-window' - Call `delete-window' inside `condition-case'.
;;
;;  This file should be loaded after loading the standard GNU file
;;  `window.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "window" '(require 'window+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/10/25 dadams
;;     special-display-popup-frame: Protect set-window-prev-buffers with fboundp.
;; 2014/02/16 dadams
;;     special-display-popup-frame:
;;       Use vanilla Emacs 24 code for display-buffer-record-window,
;;       set-window-buffer, etc.  Fixes Emacs bug #16768.
;; 2012/10/15 dadams
;;     Do not redefine quit-window for Emacs 24+, so do not delete frame for NEWS.
;;       Thx to Martin Rudalics.
;; 2012/09/10 dadams
;;     special-display-popup-frame: Protect fit-frame call with fboundp.
;; 2012/08/31 dadams
;;     special-display-popup-frame:
;;       save-selected-window -> save-window-excursion -
;;         fixes bug that moved point to bob when use pp-eval-last-sexp.
;; 2012/08/25 dadams
;;     special-display-popup-frame:
;;       Put back missing (set-window-buffer window buffer) - removed accidentally.
;;       Call fit-frame with the buffer's WINDOW selected.  Inhibit before then.
;; 2012/08/11 dadams
;;     special-display-popup-frame:
;;       Adapt redefinition for all Emacs versions.
;;       Do not raise or fit frame if (car ARGS) is a FUNCTION.  Make it do the work.
;; 2012/08/10 dadams
;;     special-display-popup-frame: Updated for latest Emacs 24.
;; 2011/06/29 dadams
;;     Added: special-display-popup-frame (for Emacs 24+).
;;     Require fit-frame.el (for Emacs 24+).
;; 2011/01/04 dadams
;;     Added autoload cookie for command quit-window.
;; 2008/02/27 dadams
;;     Added redefinition of quit-window.
;; 1996/01/25 dadams
;;     count-windows: Returned to original meaning of arg,
;;                    but only use arg ifframe has a minibuffer.
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

 ;; Cannot do (require 'window), because `window.el' does no `provide'.
 ;; Don't want to do a (load-library "window") either, because it wouldn't
 ;; allow doing (eval-after-load "window" '(progn (require 'window+)))

(when (> emacs-major-version 23)
  (require 'fit-frame nil t)) ;; (no error if not found): fit-frame

;;;;;;;;;;;;;;;;;;;;;;;;;;


;; REPLACE ORIGINAL in `window.el':
;;
;; 1. Only use arg MINIBUF if current frame has a minibuffer.
;;
;; 2. Corrected doc string:
;;    Original doc string indicated opposite behavior for arg MINIBUF.
;;
(defun count-windows (&optional minibuf)
  "Return the number of visible windows in selected frame.
Optional arg MINIBUF is used only if selected frame has a minibuffer.

MINIBUF = t means count the minibuffer window even if *not* active.
MINIBUF = nil or omitted means count the minibuffer iff it is active.
If MINIBUF is neither t nor nil it means not to count the minibuffer
even if it is active.  (See function `walk-windows'.)"
  (let ((count 0))
    (walk-windows (function (lambda (w) (setq count (+ count 1))))
                  (and (memq (cdr (assoc 'minibuffer (frame-parameters)))
                             '(only t)) ; If this frame has a minibuffer,
                       minibuf))        ; pass the arg.  (Else pass nil.)
    count))


;; REPLACE ORIGINAL in `window.el' (in `frame.el' prior to Emacs 24).
;;
;; 1. (Emacs 20 only) Call `make-frame' while BUFFER is current, so that
;;    any frame hooks (e.g. `after-make-frame-functions') will use BUFFER,
;;    not the previously current buffer.
;;
;; 2. Call `fit-frame', with BUFFER's window selected.  Inhibit fitting before then.
;;
;; NOTE: The same definition is in `frame+.el'.  If this is changed, update that too.
;;
(defun special-display-popup-frame (buffer &optional args)
  "Pop up a frame displaying BUFFER.  Return its window.
If BUFFER is already displayed in a visible or iconified frame then
raise that frame.  Otherwise, display BUFFER in a new frame.

Optional argument ARGS is a list specifying additional information.

If ARGS is an alist, use it as a list of frame parameters.  If these
parameters contain (same-window . t) then display BUFFER in the
selected window.  If they contain (same-frame . t) then display BUFFER
in a window of the selected frame.

If ARGS is a list whose car is a symbol then use (car ARGS) as a
function to do the work: display the buffer and raise its frame.  Pass
it BUFFER as first argument, and (cdr ARGS) as the rest of the
arguments."
  (if (and args (symbolp (car args)))
;;;   Should we let/make the FUNCTION that is (car ARGS) do everything, or should we
;;;   ensure that the frame is fit and raised?  For now, make FUNCTION do everything.
;;;   (let* ((window  (apply (car args) buffer (cdr args)))
;;;          (frame   (window-frame window)))
;;;     (when (fboundp 'fit-frame) (fit-frame (window-frame window)))
;;;     (raise-frame frame)
;;;     window)                         ; Return the window.
      (apply (car args) buffer (cdr args))
    (let ((window  (get-buffer-window buffer 0)))
      (or
       ;; If we have a window already, make it visible.
       (and window
            (let ((frame  (window-frame window)))
              (make-frame-visible frame)
              (raise-frame frame)
              (when (fboundp 'display-buffer-record-window) ; Emacs 24+
                (display-buffer-record-window 'reuse window buffer))
              (when (fboundp 'fit-frame) (fit-frame frame))
              window))                  ; Return the window.
       ;; Reuse the selected window if the caller requested it.
       (and (cdr (assq 'same-window args))
            (condition-case nil         ; Try Emacs 24 `switch-to-buffer' first.
                (progn (switch-to-buffer buffer nil t) (selected-window))
              (error                    ; Try again, with old `switch-to-buffer'.
               (condition-case nil
                   (progn (switch-to-buffer buffer) (selected-window))
                 (error nil)))))
       ;; Stay on the same frame if requested.
       (and (or (cdr (assq 'same-frame args))  (cdr (assq 'same-window args)))
            (let ((pop-up-windows                t)
                  (pop-up-frames                 nil)
                  (special-display-buffer-names  ())
                  (special-display-regexps       ()))
              (display-buffer buffer)))
       ;; If no window yet, make one in a new frame.
       ;; `make-frame' creates the frame before the buffer is shown in it, so do not
       ;; call `fit-frame' until we can select the buffer's window.
       (let* ((make-frame-functions  (delq 'fit-frame after-make-frame-functions))
              (frame                 (with-current-buffer buffer
                                       (make-frame
                                        (append args special-display-frame-alist))))
              (window                (frame-selected-window frame)))
         (when (fboundp 'display-buffer-record-window) ; Emacs 24+
           (display-buffer-record-window 'frame window buffer))
	 (unless (eq buffer (window-buffer window))
	   (set-window-buffer window buffer)
           (when (fboundp 'set-window-prev-buffers) ; Emacs 24+
             (set-window-prev-buffers window ())))
	 (set-window-dedicated-p window t)
         ;; Now call `fit-frame', with WINDOW selected.
         ;; Needs to be `save-window-excursion', not just `save-selected-window'.
         (when (fboundp 'fit-frame)
           (save-window-excursion (select-window window) (fit-frame)))
         window)))))                    ; Return the window.


;; REPLACE ORIGINAL in `window.el':
;;
;; Do not avoid calling `delete-window', even if `one-window-p'.
;; Instead, wrap the call in `condition-case'.
;; This way, if you use my version of `delete-window' (defined in
;; `frame-cmds.el'), then the frame is also deleted if `one-window-p'.
;; Not needed (and not appropriate) for Emacs 24.
;;
(when (< emacs-major-version 24)
  (defun quit-window (&optional kill window)
    "Quit the current buffer.  Bury it, and maybe delete the selected frame.
\(The frame is deleted if it contains a dedicated window for the buffer.)
With a prefix argument, kill the buffer instead.

Noninteractively, if KILL is non-nil, then kill the current buffer,
otherwise bury it.

If WINDOW is non-nil, it specifies a window; we delete that window,
and the buffer that is killed or buried is the one in that window."
    (interactive "P")
    (let ((buffer (window-buffer window))
          (frame (window-frame (or window (selected-window))))
          (window-solitary
           (save-selected-window
             (if window
                 (select-window window))
             (one-window-p t)))
          window-handled)

      (save-selected-window
        (if window
            (select-window window))
        (or (window-minibuffer-p)
            (window-dedicated-p (selected-window))
            (switch-to-buffer (other-buffer))))

      ;; Get rid of the frame, if it has just one dedicated window
      ;; and other visible frames exist.
      (and (or (window-minibuffer-p) (window-dedicated-p window))
           (delq frame (visible-frame-list))
           window-solitary
           (if (and (eq default-minibuffer-frame frame)
                    (= 1 (length (minibuffer-frame-list))))
               (setq window nil)
             (delete-frame frame)
             (setq window-handled t)))

      ;; Deal with the buffer.
      (if kill
          (kill-buffer buffer)
        (bury-buffer buffer))

      ;; Maybe get rid of the window.
      (unless window-handled
        (condition-case nil
            (delete-window window)
          (error nil))))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'window+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; window+.el ends here
