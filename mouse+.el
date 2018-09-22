;;; mouse+.el --- Extensions to `mouse.el'.
;;
;; Filename: mouse+.el
;; Description: Extensions to `mouse.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2018, Drew Adams, all rights reserved.
;; Created: Fri Jun 28 14:47:12 1996
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sat Sep 22 16:42:29 2018 (-0700)
;;           By: dradams
;;     Update #: 628
;; URL: https://www.emacswiki.org/emacs/download/mouse%2b.el
;; Doc URL: https://emacswiki.org/emacs/MousePlus
;; Keywords: mouse
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `mouse', `second-sel'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `mouse.el'.
;;
;;  Command `mouse-flash-position' highlights the character after the
;;  mouse pointer position, even as you drag it.  This can help make
;;  it clearer exactly where a `yank' will occur when you use
;;  `mouse-2'.  When you press `mouse-2', if the highlighted position
;;  is not exactly what you want, just keep `mouse-2' held while you
;;  move to the right location.  To enable this behavior, bind
;;  `mouse-flash-position' to `down-mouse-2'.
;;
;;  Command `mouse-flash-position-or-M-x' is the same thing as
;;  `mouse-flash-position', except that it has a special behavior in
;;  the echo area (that is, the minibuffer space when the minibuffer
;;  is inactive).  In the echo area, it calls `M-x'.  To enable this
;;  behavior, bind `mouse-flash-position-or-M-x' to `down-mouse-2'.
;;
;;  Command `mouse-scan-lines' tracks the mouse position, highlighting
;;  the line at that position.  It is handy in buffers like Dired that
;;  are essentially tables with columns - it helps you to align
;;  entries that are in the same row.
;;
;;  Command `mouse-scan-lines-or-M-:' is the same thing as
;;  `mouse-scan-lines', except that it has a special behavior in the
;;  echo area.  In the echo area, it calls `M-:'.  To enable this
;;  behavior, bind `mouse-scan-lines-or-M-:' to `S-down-mouse-2'.
;;
;;  See also library `second-sel.el' for enhancements to
;;  `mouse-drag-secondary' and `mouse-secondary-save-then-kill' that
;;  use a separate ring, `secondary-selection-ring', instead of the
;;  `kill-ring'.
;;
;;
;;  Faces defined here:
;;
;;    `mouse-flash-position', `mouse-scan-lines'.
;;
;;  Commands defined here:
;;
;;    `mouse-flash-position', `mouse-flash-position-or-M-x',
;;    `mouse-M-:', `mouse-scan-lines', `mouse-scan-lines-or-M-:',
;;    `tear-off-window-if-not-alone'.
;;
;;  Non-interactive functions defined here:
;;
;;    `mouse-flash-posn-track', `mouse-move-flash-posn-overlay'.
;;
;;  Constants defined here:
;;
;;    `mouse-flash-posn-overlay', `mouse-scan-lines-overlay'.
;;
;;
;;  ***** NOTE: The following functions defined in `mouse.el' have
;;              been REDEFINED HERE:
;;
;;  `mouse-drag-region'     - If click echo area and `*Messages*' is
;;                            displayed, do `M-x', delete `*Messages*'
;;  `(mouse-)tear-off-window' - Don't delete window if it is alone in
;;                              frame.  Instead, clone frame and window.
;;  `mouse-yank-secondary' - Error if (x-get-selection 'SECONDARY)=nil
;;
;;
;;  Do this in your init file (~/.emacs or ~/_emacs):
;;
;;   (require 'mouse+)
;;
;;
;;  Suggested bindings:
;;
;;   The first sexp is NECESSARY for Emacs 24 or later, if you want to
;;   take advantage of the `mouse-drag-region' behavior defined here
;;   wrt buffer `*Messages*' and `M-x'.
;;
;;   ;; Do not use `view-echo-area-messages' for `mouse-1'.   Use
;;   ;; version of `mouse-drag-region' defined here, which does more.
;;   (when (> emacs-major-version 23)
;;     (define-key minibuffer-inactive-mode-map [down-mouse-1] nil)
;;     (define-key minibuffer-inactive-mode-map [mouse-1]      nil))
;;
;;   (global-set-key [down-mouse-2]        'mouse-flash-position-or-M-x)
;;   (global-set-key [S-down-mouse-2]      'mouse-scan-lines-or-M-:)
;;   (global-set-key [mode-line C-mouse-1] 'tear-off-window)
;;   (define-key ctl-x-5-map "1"           'tear-off-window)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2018/09/22 dadams
;;     Moved here from frame-cmds.el: tear-off-window(-if-not-alone).
;;     (mouse-)tear-off-window(-if-not-alone): Updated per Emacs bug #32799.
;;                                             Raise error for Emacs 20.
;; 2016/12/09 dadams
;;     mouse-yank-secondary: x-get-selection -> gui-get-selection for Emacs 25+.
;; 2015/11/22 dadams
;;     mouse-drag-region: Corrected test for mouse-drag-track call to Emacs 25+.
;;     Removed lexical-binding declaration to file.
;; 2015/11/21 dadams
;;     Added lexical-binding declaration to file.  Otherwise, mouse-drag-region no longer
;;       set the region (when mouse-drag-copy-region = t).
;; 2014/07/13 dadams
;;     mouse-drag-region:
;;      Use save-window-excursion instead of trying to restore buffer.
;;      If *Messages* is selected then bury-buffer instead of delete-window.
;;      Mention in Commentary: must remove Emacs 24 binding of view-echo-area-messages.
;; 2014/05/19 dadams
;;     mouse-drag-region: Handle incompatible change for Emacs 24.4+.
;; 2011/12/19 dadams
;;     mouse-scan-lines: Use line-(beginning|end)-position, not (beginning|end)-of-line.
;; 2011/01/04 dadams
;;     Removed autoload cookie from non def* sexp.  Added for defface.
;; 2010/10/12 dadams
;;     mouse-flash-position: Updated doc string for Emacs 24.
;; 2009/04/26 dadams
;;     mouse-scan-lines: Bind inhibit-field-text-motion to t, for end-of-line.
;; 2008/09/29 dadams
;;     Added: redefinition of mouse-drag-region.
;; 2008/05/23 dadams
;;     Soft-require second-sel.el.
;;     mouse-yank-secondary: Added prefix arg treatment.
;; 2008/05/02 dadams
;;     (put 'mouse-yank-secondary 'delete-selection 'yank)
;; 2007/01/27 dadams
;;     Added: mouse-scan-lines(-overlay), mouse-scan-lines-or-M-:,
;; 2006/11/04 dadams
;;     Added: mouse-M-:.
;;     mouse-flash-position-or-M-x: Use switch-to-buffer.  Skip minibuffers.
;; 2006/11/03 dadams
;;     Added: mouse-flash-position-or-M-x.
;; 2006/08/12 dadams
;;     mouse-flash-posn-overlay: Added mouse-face to overlay.
;;     mouse-flash-posn-track:
;;       Replaced push with setq...cons, to avoid runtime require of cl.el for Emacs 20.
;; 2006/08/11 dadams
;;     Added: mouse-flash-position (face and command), mouse-flash-posn-overlay,
;;            mouse-flash-posn-track, mouse-move-flash-posn-overlay.
;; 2004/09/28 dadams
;;     Added: mouse-yank-secondary.
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


(require 'mouse)

(require 'second-sel nil t) ;; yank-secondary

;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defface mouse-scan-lines '((t (:background "Yellow")))
  "*Face used to temporarily highlight line at mouse position."
  :group 'mouse)

;;;###autoload
(defface mouse-flash-position '((t (:background "Yellow")))
  "*Face used to highlight mouse position temporarily."
  :group 'mouse)

(defconst mouse-scan-lines-overlay
    ;; Create and immediately delete, to get "overlay in no buffer".
    (let ((ol  (make-overlay (point-min) (point-min))))
      (delete-overlay ol)
      (overlay-put ol 'face       'mouse-scan-lines)
      (overlay-put ol 'mouse-face 'mouse-scan-lines)
      (overlay-put ol 'priority   1000000)
      ol)
  "Overlay to highlight line at mouse position.")

(defconst mouse-flash-posn-overlay
    ;; Create and immediately delete, to get "overlay in no buffer".
  (let ((ol  (make-overlay (point-min) (point-min))))
    (delete-overlay ol)
    (overlay-put ol 'face 'mouse-flash-position)
    (overlay-put ol 'mouse-face 'mouse-flash-position)
    (overlay-put ol 'priority 1000000)
    ol)
  "Overlay to highlight current mouse position.")



;;  Candidate for binding to `S-down-mouse-2'.
;;;###autoload
(defun mouse-scan-lines-or-M-: (start-event)
  "In echo area, `M-:'.  Else, highlight current line, tracking pointer."
  (interactive "e")
  (let ((win  (posn-window (event-start start-event)))
        (bufs (buffer-list))
        (M-:-cmd (key-binding "\M-:" t)))
    (cond ((and (window-minibuffer-p win) (not (minibuffer-window-active-p win)) M-:-cmd)
           (read-event)                 ; Ignore mouse up event.
           (while (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name (car bufs)))
             (pop bufs))
           (when bufs (set-buffer (car bufs)))
           (switch-to-buffer-other-window (current-buffer))
           (call-interactively M-:-cmd nil [(meta ?:)]))
          (t
           (mouse-scan-lines start-event)))))

;;;###autoload
(defun mouse-M-: (start-event)
  "In the echo area, do `M-:'.  Otherwise, do nothing."
  (interactive "e")
  (let ((win      (posn-window (event-start start-event)))
        (bufs     (buffer-list))
        (M-:-cmd  (key-binding "\M-:" t)))
    (cond ((and (window-minibuffer-p win) (not (minibuffer-window-active-p win)) M-:-cmd)
           (read-event)                 ; Ignore mouse up event.
           (while (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name (car bufs)))
             (pop bufs))
           (when bufs (set-buffer (car bufs)))
           (switch-to-buffer-other-window (current-buffer))
           (call-interactively M-:-cmd nil [(meta ?:)]))
          (t
           (run-hooks 'mouse-leave-buffer-hook))))) ; Let temp modes like isearch turn off.

;;;###autoload
(defun mouse-scan-lines (start-event)
  "Track mouse drags, highlighting the line under the pointer."
  (interactive "e")
  (save-excursion
    (run-hooks 'mouse-leave-buffer-hook) ; Let temporary modes like isearch turn off.
    (let* ((original-window            (selected-window))
           (echo-keystrokes            0)
           (start-posn                 (event-start start-event))
           (start-point                (posn-point start-posn))
           (start-window               (posn-window start-posn))
           (inhibit-field-text-motion  t)) ; Just to be sure, for `end-of-line'.
      (move-overlay mouse-scan-lines-overlay
                    (save-excursion (goto-char start-point) (line-beginning-position))
                    (save-excursion (goto-char start-point) (line-end-position)))
      (let (event end  end-point)
        (track-mouse
          (while (progn (setq event  (read-event))
                        (or (mouse-movement-p event)
                            (memq (car-safe event) '(switch-frame select-window))))
            (unless (memq (car-safe event) '(switch-frame select-window))
              (setq end        (event-end event)
                    end-point  (posn-point end))
              (when (and (eq (posn-window end) start-window) (integer-or-marker-p end-point))
                (move-overlay
                 mouse-scan-lines-overlay
                 (save-excursion (goto-char end-point) (line-beginning-position))
                 (save-excursion (goto-char end-point) (line-end-position)))))))
        (delete-overlay mouse-scan-lines-overlay)))))

(defun mouse-move-flash-posn-overlay (ol start end)
  "Move `mouse-flash-posn-overlay' to position END.
START is the position of the start of the current drag operation."
  (unless (= start end)
    ;; Go to START first, so that when we move to END, if it's in the middle
    ;; of intangible text, point jumps in the direction away from START.
    ;; Don't do it if START=END, otherwise a single click risks selecting
    ;; a region if it's on intangible text.  This exception was originally
    ;; only applied on entry to mouse-drag-region, which had the problem
    ;; that a tiny move during a single-click would cause the intangible
    ;; text to be selected.
    (goto-char start)
    (goto-char end)
    (setq end  (point)))
  (move-overlay ol end (min (point-max) (1+ end))))

;; Inspired from `mouse-drag-region'.  Candidate for binding to `down-mouse-2'.
;;;###autoload
(defun mouse-flash-position-or-M-x (start-event)
  "In the echo area, do `M-x'.  Otherwise, do `mouse-flash-position'."
  (interactive "e")
  (let ((win      (posn-window (event-start start-event)))
        (bufs     (buffer-list))
        (M-x-cmd  (key-binding "\M-x" t)))
    (cond ((and (window-minibuffer-p win) (not (minibuffer-window-active-p win)) M-x-cmd)
           (read-event)                 ; Ignore mouse up event.
           (while (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name (car bufs)))
             (pop bufs))
           (when bufs (set-buffer (car bufs)))
           (switch-to-buffer-other-window (current-buffer))
           (call-interactively M-x-cmd nil [(meta ?x)]))
          (t
           (run-hooks 'mouse-leave-buffer-hook) ; Let temporary modes like isearch turn off.
           (mouse-flash-posn-track start-event)))))

;; Inspired from `mouse-drag-region'.  Candidate for binding to `down-mouse-2'.
;;;###autoload
(defun mouse-flash-position (start-event)
  "Highlight the mouse position as you drag the mouse.
This must be bound to a button-down mouse event.

If you bind this to `down-mouse-2', and `mouse-2' is bound to
`mouse-yank-primary' or `mouse-yank-at-click' (defaults for Emacs 24
and earlier, respectively), then the yank occurs just before the
highlighted character.

If you want to prevent the `mouse-2' up-button yank from taking place,
perhaps because you changed your mind, you can press and hold `C-g'
while releasing the mouse button (press `mouse-2'; drag; press `C-g';
release `mouse-2'; release `C-g')."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Let temporary modes such as isearch turn off.
  (mouse-flash-posn-track start-event))

(defun mouse-flash-posn-track (start-event)
  "Track mouse drags by highlighting the mouse position"
  (mouse-minibuffer-check start-event)
  (let* ((original-window                 (selected-window))
         (echo-keystrokes                 0)
         (start-posn                      (event-start start-event))
         (start-point                     (posn-point start-posn))
         (start-window                    (posn-window start-posn))
         (start-window-start              (window-start start-window))
         (start-hscroll                   (window-hscroll start-window))
         (bounds                          (window-edges start-window))
         (make-cursor-line-fully-visible  nil)
         (top                             (nth 1 bounds))
         (bottom                          (if (window-minibuffer-p start-window)
                                              (nth 3 bounds)
                                            (1- (nth 3 bounds))))) ; Don't count mode line.
    (mouse-move-flash-posn-overlay mouse-flash-posn-overlay start-point start-point)
    (overlay-put mouse-flash-posn-overlay 'window start-window)
    (deactivate-mark)
    (unwind-protect
         (let (event end end-point last-end-point)
           (track-mouse
             (while (progn (setq event  (read-event))
                           (or (mouse-movement-p event)
                               (memq (car-safe event) '(switch-frame select-window))))
               (unless (memq (car-safe event) '(switch-frame select-window))
                 (setq end        (event-end event)
                       end-point  (posn-point end))
                 (when (numberp end-point) (setq last-end-point  end-point))
                 (cond
                   ((and (eq (posn-window end) start-window) ; Moving within original window.
                         (integer-or-marker-p end-point))
                    (mouse-move-flash-posn-overlay mouse-flash-posn-overlay
                                                   start-point end-point))
                   (t
                    (let ((mouse-row  (cddr (mouse-position))))
                      (cond
                        ((null mouse-row))
                        ((< mouse-row top)
                         (mouse-scroll-subr start-window (- mouse-row top)
                                            mouse-flash-posn-overlay start-point))
                        ((>= mouse-row bottom)
                         (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                            mouse-flash-posn-overlay start-point)))))))))
           ;; In case we did not get a mouse-motion event for the final move of
           ;; the mouse before a drag event, pretend that we did get one.
           (when (and (memq 'drag (event-modifiers (car-safe event)))
                      (setq end        (event-end event)
                            end-point  (posn-point end))
                      (eq (posn-window end) start-window)
                      (integer-or-marker-p end-point))
             (mouse-move-flash-posn-overlay mouse-flash-posn-overlay start-point end-point))
           (when (consp event)          ; Handle the terminating event.
             (let ((fun  (key-binding (vector (car event)))))
               ;; Run the binding of the terminating up-event, if possible.
               (let* ((stop-point  (if (numberp (posn-point (event-end event)))
                                       (posn-point (event-end event))
                                     last-end-point))
                      (drag-end    (if (and stop-point (< stop-point start-point))
                                       (overlay-start mouse-flash-posn-overlay)
                                     (overlay-end mouse-flash-posn-overlay)))
                      (drag-start  (- (+ (overlay-end mouse-flash-posn-overlay)
                                         (overlay-start mouse-flash-posn-overlay))
                                      drag-end))
                      last-command this-command)
                 (delete-overlay mouse-flash-posn-overlay)
                 (when (and (= start-hscroll (window-hscroll start-window))
                            (or end-point
                                (= (window-start start-window) start-window-start)))
                   (setq unread-command-events  (cons event unread-command-events)))))))
      (delete-overlay mouse-flash-posn-overlay))))



;; REPLACES ORIGINAL in `mouse.el':
;;
;; If window is alone in its frame then just clone frame.
;; Suppress error "Attempt to delete minibuffer or sole ordinary window" in that context.
;; Use `pop-to-buffer-same-window', not `switch-to-buffer'.
;;
;;;###autoload
(defun tear-off-window (click)
  "Create a new frame displaying buffer of window clicked on.
If window is not the only one in frame, then delete it.
Otherwise, this command effectively clones the frame and window."
  (interactive
   (progn (unless (> emacs-major-version 21)
            (error "You need Emacs 22 or later for this command"))
          (list last-nonmenu-event)))   ; See bug #32799
  (mouse-minibuffer-check click)
  (let* ((window  (posn-window (event-start click)))
         (buf     (window-buffer window))
         (frame   (make-frame)))
    (select-frame frame)
    (if (fboundp 'pop-to-buffer-same-window)
        (pop-to-buffer-same-window buf)
      (switch-to-buffer buf))
    (save-window-excursion (select-window window)
                           (unless (one-window-p) (delete-window window)))))

;; This is redundant for Emacs 24.4+, but it is needed for older releases.
;;
;;;###autoload
(defalias 'mouse-tear-off-window 'tear-off-window)

;;;###autoload
(defun tear-off-window-if-not-alone (click)
  "Move selected window to a new frame, unless it is alone in its frame.
If it is alone, do nothing.  Otherwise, delete it and create a new
frame showing the same buffer."
  (interactive
   (progn (unless (> emacs-major-version 21)
            (error "You need Emacs 22 or later for this command"))
          (list last-nonmenu-event)))   ; See bug #32799
  (mouse-minibuffer-check click)
  (if (one-window-p 'NOMINI)
      (message "Sole window in frame")
    (tear-off-window click)))



;; REPLACES ORIGINAL in `mouse.el':
;; If click in echo area and `*Messages*' is already displayed,
;; then do `M-x' and delete `*Messages*' window.
;;
(when (> emacs-major-version 21)
  (defun mouse-drag-region (start-event)
    "Set the region to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event.
In Transient Mark mode, the highlighting remains as long as the mark
remains active.  Otherwise, it remains until the next input event.

If the click is in the echo area, then:
  If buffer `*Messages' is not displayed, display it.
  Else run the command bound to `M-x'."
    (interactive "e")
    (let ((clickwin  (posn-window (event-start start-event))))
      (if (and (window-minibuffer-p clickwin)
               (not (minibuffer-window-active-p clickwin)))
          (let* ((Messages-buf  (get-buffer-create "*Messages*"))
                 (Messages-win  (get-buffer-window Messages-buf 'visible)))
            (if Messages-win
                (let ((M-x-cmd  (or (key-binding "\M-x" t) 'execute-extended-command)))
                  (read-event)          ; Swallow the up-event.
                  (if (eq Messages-win (selected-window))
                      (bury-buffer)
                    (delete-window Messages-win))
                  (save-window-excursion (call-interactively M-x-cmd nil [(meta ?x)])))
              (save-excursion
                (read-event)            ; Swallow the up-event.
                (set-buffer Messages-buf)
                (goto-char (point-max))
                (display-buffer (current-buffer)))))

        ;; Give temporary modes such as isearch a chance to turn off.
        (run-hooks 'mouse-leave-buffer-hook)
        (if (> emacs-major-version 24)
            (mouse-drag-track start-event)
          (mouse-drag-track start-event t))))))



;; REPLACES ORIGINAL in `mouse.el':
;;
;; 1. Use `yank-secondary' if defined.
;; 2. If `mouse-yank-at-point', insert at point regardless of click position.
;; 3. Fixes bug when (x-get-selection 'SECONDARY) returns nil.
;;
;;;###autoload
(defun mouse-yank-secondary (click arg)
  "Insert the secondary selection at the position clicked on.
Move point to the end of the inserted text.
If `mouse-yank-at-point' is non-nil, insert at point regardless of
 where you click.
If command `yank-secondary' is defined (see library `second-sel.el'),
 then a prefix arg N means insert the Nth most recent secondary
 selection."
  (interactive "e\nP")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (or mouse-yank-at-point (mouse-set-point click))
  (setq mouse-selection-click-count  0)
  (if (not (fboundp 'yank-secondary))
      (let ((secondary  (if (fboundp 'gui-get-selection) ; Emacs 25.1+.
                            (gui-get-selection 'SECONDARY)
                          (x-get-selection 'SECONDARY))))
        (unless secondary (error "No secondary selection"))
        (funcall (if (fboundp 'insert-for-yank) 'insert-for-yank 'insert) secondary))
    (setq this-command  'yank-secondary)
    (yank-secondary arg)))

;; Tell `delete-selection-mode' to replace active region by yanked secondary selection.
(put 'mouse-yank-secondary 'delete-selection 'yank)



;; REPLACES ORIGINAL in `mouse.el':
;; Iconify *Completions* frame after choosing completion.
;; Free variable COMPLETION-REFERENCE-BUFFER is defined in `simple.el'.
;  ;;;###autoload
;(defun mouse-choose-completion (event)
;  "Click on an alternative in the `*Completions*' buffer to choose it."
;  (interactive "e")
;  ;; Give temporary modes such as isearch a chance to turn off.
;  (run-hooks 'mouse-leave-buffer-hook)
;  (let ((buffer  (window-buffer))
;        choice
;       base-size)
;    (save-excursion
;      (set-buffer (window-buffer (posn-window (event-start event))))
;      (when completion-reference-buffer   ; Defined in `simple.el'.
;        (setq buffer  completion-reference-buffer))
;      (setq base-size  completion-base-size)
;      (save-excursion
;       (goto-char (posn-point (event-start event)))
;       (let (beg end)
;         (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
;            (setq end  (point))
;            (setq beg  (1+ (point))))
;         (unless beg (error "No completion here"))
;         (setq beg  (previous-single-property-change beg 'mouse-face))
;         (setq end  (or (next-single-property-change end 'mouse-face)
;                        (point-max)))
;         (setq choice  (buffer-substring beg end)))))
;    (save-window-excursion
;      (select-window (posn-window (event-start event)))
;      (when (one-window-p t 'selected-frame) (iconify-frame (selected-frame))))
;    (choose-completion-string choice buffer base-size))) ; In `simple+.el'.

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mouse+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mouse+.el ends here
