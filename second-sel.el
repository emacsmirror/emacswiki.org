;;; second-sel.el --- Secondary selection commands
;;
;; Filename: second-sel.el
;; Description: Secondary selection commands
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2008-2012, Drew Adams, all rights reserved.
;; Created: Fri May 23 09:58:41 2008 ()
;; Version: 22.0
;; Last-Updated: Thu Aug 23 16:44:03 2012 (-0700)
;;           By: dradams
;;     Update #: 285
;; URL: http://www.emacswiki.org/emacs-en/second-sel.el
;; Doc URL: http://emacswiki.org/emacs/SecondarySelection
;; Keywords: region, selection, yank, paste, edit
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Commands that use the secondary selection.
;;
;;  You can enhance what `second-sel.el' offers in these ways:
;;
;;  1. Use library `browse-kill-ring+.el'.
;;
;;     This lets you use `M-y' at the top level to browse either the
;;     `kill-ring' or the `secondary-selection-ring', or both, to
;;     choose a selection to yank.
;;
;;     And `M-y' following a yank from either of these rings replaces
;;     that yank with the next (or prefix-argth) ring entry.  IOW, it
;;     does a `yank-pop' or a `yank-pop-secondary', as appropriate.
;;
;;     (If you use `browse-kill-ring+.el', load `second-sel.el'
;;     first.)
;;
;;  2. Use Icicles (library `icicles.el').
;;
;;     If you do that then the behavior is similar to that provided by
;;     `browse-kill-ring+.el', except that `M-y' at the top level lets
;;     you choose a selection from either ring using completion
;;     against the ring entries.  And during completion you can use
;;     `S-delete' to delete entries from the ring.
;;
;;     (This is the case by default, but you can customize Icicles to
;;     not do this by removing the `M-y' binding, if, for example, you
;;     prefer the `browse-kill-ring+.el' behavior to completing.
;;
;;
;;  Commands defined here:
;;
;;    `isearch-yank-secondary', `primary-to-secondary',
;;    `rotate-secondary-selection-yank-pointer', `secondary-dwim',
;;    `secondary-swap-region', `secondary-to-primary',
;;    `yank-pop-commands', `yank-pop-secondary', `yank-secondary'.
;;
;;  User options defined here:
;;
;;    `secondary-selection-ring-max',
;;    `secondary-selection-yank-commands',
;;    `secondary-selection-yank-secondary-commands',
;;
;;  Non-interactive functions defined here:
;;
;;    `add-secondary-to-ring', `current-secondary-selection'.
;;
;;  Internal variables defined here:
;;
;;    `secondary-selection-ring',
;;    `secondary-selection-ring-yank-pointer', `yank-undo-function'.
;;
;;
;;  ***** NOTE: The following functions defined in `mouse.el'
;;              have been REDEFINED or ADVISED HERE:
;;
;;    `mouse-drag-secondary',  `mouse-secondary-save-then-kill'.
;;
;;
;;  Suggested key bindings:
;;
;;   (global-set-key [(control meta ?y)]     'secondary-dwim)
;;   (define-key esc-map "y"                 'yank-pop-commands)
;;   (define-key isearch-mode-map "\C-\M-y"  'isearch-yank-secondary)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/01/07 dadams
;;     yank-pop-commands: If in a browse-kill-ring sel-ring buffer, browse other ring.
;; 2011/05/03 dadams
;;     mouse-secondary-save-then-kill: Better error message.
;; 2011/01/04 dadams
;;     Added autoload cookies (for defcustom and commands).
;; 2010/04/22 dadams
;;     Added: isearch-yank-secondary.
;; 2009/06/25 dadams
;;     Renamed: yank-secondary-or-swap-w-region to secondary-dwim.
;;     Added: secondary-swap-region.
;;     secondary-dwim: Negative prefix arg is secondary-swap-region.
;;     primary-to-secondary: exchange-point-and-mark if needed. Deactivate the mark.
;;     secondary-to-primary: Call select-frame-set-input-focus.
;; 2009/06/24 dadams
;;     Added: secondary-selection-yank(-secondary)-commands.
;;     yank-pop-commands:
;;       Disable yank-pop advice here, not at top-level of file.
;;       Test membership in each list of yank commands, instead of testing eq 'yank.
;;       Pass prefix arg to browse-kill-ring, if browse-kill-ring+.el is loaded.
;;     yank-pop-secondary: Use secondary-selection-yank-secondary-commands.
;; 2008/05/23 dadams
;;     Created file.  Moved here from misc-cmds.el:
;;       primary-to-secondary, rotate-secondary-selection-yank-pointer,
;;       secondary-to-primary, yank-pop(-commands|secondary), yank-undo-function,
;;       current-secondary-selection,  yank-secondary(-or-swap-w-region),
;;       secondary-selection(-ring(-max|-yank-pointer)), add-secondary-to-ring,
;;       mouse-drag-secondary, mouse-secondary-save-then-kill.
;;     Added (removed defadvice): mouse-secondary-save-then-kill.
;; 2008/05/22 dadams
;;     Added: secondary-selection(-ring(-max|-yank-pointer)), yank-undo-function,
;;            rotate-secondary-selection-yank-pointer, current-secondary-selection,
;;            add-secondary-to-ring, yank-pop-(commands|secondary).
;;     yank-secondary: Added optional arg - use current-secondary-selection.
;;                     Use insert-for-yank.
;;     primary-to-secondary: Use add-secondary-to-ring, filter-buffer-substring.
;;                           Delay setting this-command.  Exchange point and mark.
;;                           Return nil.
;;     Disable browse-kill-ring's advice.
;; 2008/05/06 dadams
;;     Renamed: yank-secondary-or-convert-primary to yank-secondary-or-swap-w-region.
;;     yank-secondary-or-swap-w-region: Prefix arg < 0 means secondary-to-primary.
;;     primary-to-secondary: Overlay uses current buffer.
;; 2008/05/03 dadams
;;     (put 'yank-secondary 'delete-selection 'yank).
;;     Added: primary-to-secondary, secondary-to-primary,
;;            yank-secondary-or-convert-primary.
;; 2006/10/21 dadams
;;     yank-secondary: Error message if there is no secondary selection.
;; 1996/02/15 dadams
;;     Added yank-secondary.
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

;; Quiet the byte-compiler.

(defvar yank-window-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defcustom secondary-selection-ring-max 60
  "*Maximum length of `secondary-selection-ring'.
After the ring is maximally filled, adding a new element replaces the
oldest element."
  :type 'integer :group 'killing)

;;;###autoload
(defcustom secondary-selection-yank-commands (if (boundp 'browse-kill-ring-yank-commands)
                                                 browse-kill-ring-yank-commands
                                               '(yank icicle-yank-maybe-completing))
  "*Commands that `yank-pop-commands' recognizes as yanking text."
  :type '(repeat (restricted-sexp :match-alternatives (symbolp commandp) :value ignore))
  :group 'killing)

;;;###autoload
(defcustom secondary-selection-yank-secondary-commands '(mouse-yank-secondary
                                                         secondary-dwim
                                                         yank-secondary)
  "*Commands that yank the secondary selection."
  :type '(repeat (restricted-sexp :match-alternatives (symbolp commandp) :value ignore))
  :group 'killing)

(defvar secondary-selection-ring nil
  "History of secondary selections, as a ring.")

(defvar secondary-selection-ring-yank-pointer nil
  "A tail of the secondary selection ring.  It indicates the current entry.
This is the tail whose car is the last secondary selection yanked.")

(unless (boundp 'yank-undo-function)
  (defvar yank-undo-function nil
    "If non-nil, function used by `yank-pop' to delete last stretch of yanked text.
Function is called with two parameters, START and END corresponding to
the value of the mark and point; it is guaranteed that START <= END.
Normally set from the UNDO element of a yank-handler; see `insert-for-yank'."))

;;;###autoload
(defun secondary-dwim (arg)             ; Suggested binding: `C-M-y'
  "Do-What-I-Mean with the secondary selection.
Prefix arg:

 None: Yank secondary.
 Zero: Select secondary as region.
 > 0:  Move secondary to region.
 < 0:  Swap region and secondary.

Details:

No prefix arg: Yank the secondary selection at point.  Move point to
the end of the inserted text.  Leave mark where it was.

Zero arg: Select the secondary selection and pop to its buffer.

Non-zero arg: Move the secondary selection to this buffer's region.

Negative arg: Also go to where the secondary selection was and select
it as the region.  That is, swap the region and the secondary
selection."
  (interactive "P")
  (cond (arg
         (setq arg  (prefix-numeric-value arg))
         (cond ((> arg 0) (call-interactively #'primary-to-secondary))
               ((< arg 0) (call-interactively #'secondary-swap-region))
               ((= arg 0) (call-interactively #'secondary-to-primary))))
        (t
         (setq this-command  'yank-secondary)
         (when delete-selection-mode (delete-selection-pre-hook)) ; Hack!
         (call-interactively #'yank-secondary))))

;;;###autoload
(defun yank-secondary (&optional arg)
  "Insert the secondary selection at point.
Moves point to the end of the inserted text.  Does not change mark.

Numeric prefix arg N means insert the Nth most recently yanked
secondary selection.  Plain `C-u' is the same as N=1.

You can also use `M-y' after this command to yank previous secondary
selections.  With no prefix arg, this always yanks the active
secondary selection (the one that is highlighted), not the last
selection yanked."
  (interactive "*P")
  (setq yank-window-start  (window-start)
        this-command       t)           ; Don't set to `yank-secondary' until the end.
  (push-mark (point))
  (let ((sel  (if arg
                  (current-secondary-selection (cond ((consp arg) 0)
                                                     ((eq arg '-) -2)
                                                     (t (1- arg))))
                (x-get-selection 'SECONDARY))))
    (unless sel (error "No secondary selection"))
    (funcall (if (fboundp 'insert-for-yank) 'insert-for-yank 'insert) sel))
  (when (consp arg)
    ;; This is like exchange-point-and-mark, but it doesn't activate the mark.
    ;; It is cleaner to avoid activation, even though the command
    ;; loop would deactivate the mark because we inserted text.
    (goto-char (prog1 (mark t)
                 (set-marker (mark-marker) (point) (current-buffer)))))
  (when (eq this-command t) (setq this-command  'yank-secondary)) ; Do this last.
  nil)

;;;###autoload
(defun isearch-yank-secondary ()        ; Suggested Isearch binding: `C-M-y'
  "Yank string from secondary-selection ring into search string."
  (interactive)
  (isearch-yank-string (current-secondary-selection 0)))

;; Tell `delete-selection-mode' to replace active region by yanked secondary selection.
(put 'yank-secondary 'delete-selection 'yank)

;;;###autoload
(defun primary-to-secondary (beg end)
  "Make the region in the current buffer into the secondary selection.
Deactivate the region.  Do not move the cursor."
  (interactive "r")
  (setq mouse-secondary-start  (make-marker))
  (set-marker mouse-secondary-start beg)
  (if mouse-secondary-overlay
      (move-overlay mouse-secondary-overlay beg end (current-buffer))
    (setq mouse-secondary-overlay  (make-overlay beg end (current-buffer)))
    (overlay-put mouse-secondary-overlay 'face 'secondary-selection))
  (let ((sel  (if (fboundp 'filter-buffer-substring)
                  (filter-buffer-substring beg end)
                (buffer-substring beg end))))
    (add-secondary-to-ring sel)
    (x-set-selection 'SECONDARY sel))
  (when (> beg end) (exchange-point-and-mark))
  (setq mark-active  nil))

;;;###autoload
(defun secondary-swap-region (beg end)
  "Make the region into the secondary selection, and vice versa.
Pop to the buffer that has the secondary selection, and change it to
the region.  Leave behind the secondary selection in place of the
original buffer's region."
  (interactive "r")
  (let ((osecondary  (x-get-selection 'SECONDARY))
        osec-buf osec-start osec-end)
    (unless (and osecondary (overlayp mouse-secondary-overlay))
      (error "No secondary selection"))
    (setq osec-buf    (overlay-buffer mouse-secondary-overlay)
          osec-start  (overlay-start mouse-secondary-overlay)
          osec-end    (overlay-end mouse-secondary-overlay))
    ;; Make this buffer's region into secondary selection.
    (setq mouse-secondary-start  (make-marker))
    (set-marker mouse-secondary-start beg)
    (if mouse-secondary-overlay
        (move-overlay mouse-secondary-overlay beg end (current-buffer))
      (setq mouse-secondary-overlay  (make-overlay beg end (current-buffer)))
      (overlay-put mouse-secondary-overlay 'face 'secondary-selection))
    (let ((sel  (if (fboundp 'filter-buffer-substring)
                    (filter-buffer-substring beg end)
                  (buffer-substring beg end))))
      (add-secondary-to-ring sel)
      (x-set-selection 'SECONDARY sel))
    (setq mark-active  nil)
    ;; Make original secondary selection into region and pop to it.
    (x-set-selection 'PRIMARY osecondary)
    (pop-to-buffer osec-buf)
    ;;  ; Shouldn't need to set frame focus, but we apparently must, in Windows at least.
    (select-window (get-buffer-window (current-buffer)))
    (select-frame-set-input-focus (window-frame (selected-window)))
    (push-mark osec-start 'nomsg 'activate)
    (goto-char osec-end)
    (setq deactivate-mark  nil)))

;;;###autoload
(defun secondary-to-primary ()
  "Convert the secondary selection into the active region.
Select the secondary selection and pop to its buffer."
  (interactive)
  (let ((secondary  (x-get-selection 'SECONDARY)))
    (unless (and secondary (overlayp mouse-secondary-overlay))
      (error "No secondary selection"))
    (x-set-selection 'PRIMARY secondary))
  (pop-to-buffer (overlay-buffer mouse-secondary-overlay))
  ;;  ; Shouldn't need to set frame focus, but we apparently must, in Windows at least.
  (select-window (get-buffer-window (current-buffer)))
  (select-frame-set-input-focus (window-frame (selected-window)))
  (push-mark (overlay-start mouse-secondary-overlay) 'nomsg 'activate)
  (goto-char (overlay-end mouse-secondary-overlay))
  (setq deactivate-mark  nil))

;; Like `kill-new'.
(defun add-secondary-to-ring (string &optional replace yank-handler)
  "Make STRING the latest entry in the secondary selection ring.
Set `secondary-selection-ring-yank-pointer' to point to it.

Optional second argument REPLACE non-nil means that STRING will
replace the front of the secondary selection ring, rather than being
added separately to the ring.

Optional third argument YANK-HANDLER is used only for Emacs version 22
or later.  It controls how the STRING is later inserted into a buffer;
see `insert-for-yank' for details.  When a yank handler is specified,
STRING must be non-empty (the yank handler, if non-nil, is stored as a
`yank-handler' text property on STRING).

When the yank handler has a non-nil PARAM element, the original STRING
argument is not used by `insert-for-yank'.  However, since Lisp code
may access and use elements from the kill ring directly, the STRING
argument should still be a useful string for such uses."
  (if (> (length string) 0)
      (when yank-handler (put-text-property 0 (length string) 'yank-handler
                                            yank-handler string))
    (when yank-handler
      (signal 'args-out-of-range
              (list string "yank-handler specified for empty string"))))
  ;; (when (fboundp 'menu-bar-update-yank-menu)
  ;;   (menu-bar-update-yank-menu string (and replace (car secondary-selection-ring))))
  ;; (when (and (boundp 'browse-kill-ring-no-duplicates) browse-kill-ring-no-duplicates)
  ;;  (setq secondary-selection-ring  (delete (ad-get-arg 0) secondary-selection-ring)))
  (if (and replace secondary-selection-ring)
      (setcar secondary-selection-ring string)
    (push string secondary-selection-ring)
    (when (> (length secondary-selection-ring) secondary-selection-ring-max)
      (setcdr (nthcdr (1- secondary-selection-ring-max) secondary-selection-ring) nil)))
  (setq secondary-selection-ring-yank-pointer  secondary-selection-ring))

;;;###autoload
(defun yank-pop-commands (&optional arg) ; Suggested binding: `M-y'
  "`yank-pop' or `yank-pop-secondary', depending on previous command.
If previous command was a yank-secondary command, then
   `yank-pop-secondary'.
Else if previous command was a yank command, then `yank-pop'.
Else if `browse-kill-ring' is defined, then `browse-kill-ring'.
 If in a `browse-kill-ring' selection-ring buffer, then browse the
  other selection ring.
Suggestion: Bind this command to `M-y'."
  (interactive "p")
  ;; Disable `browse-kill-ring's advice, since we handle such things here instead.
  (when (fboundp 'browse-kill-ring)
    (condition-case nil
        (ad-disable-advice 'yank-pop 'around 'kill-ring-browse-maybe)
      (error nil)))
  (cond ((memq last-command secondary-selection-yank-secondary-commands)
         (when buffer-read-only (error "Buffer is read-only: %S" (current-buffer)))
         (yank-pop-secondary arg))
        ((memq last-command secondary-selection-yank-commands)
         (when buffer-read-only (error "Buffer is read-only: %S" (current-buffer)))
         (yank-pop arg))
        ((boundp 'browse-kill-ring-alternative-ring) ; `browse-kill-ring+.el'.
         (let ((use-alt-ring-p  (or current-prefix-arg
                                    (eq major-mode 'browse-kill-ring-mode))))
           (browse-kill-ring use-alt-ring-p)))
        ((fboundp 'browse-kill-ring) (browse-kill-ring)))) ; `browse-kill-ring.el'.

;;;###autoload
(defun yank-pop-secondary (&optional arg)
  "Replace just-yanked secondary selection with a different one.
You can use this only immediately after a `yank-secondary' or a
`yank-pop-secondary'.

At such a time, the region contains a stretch of reinserted
previously-killed text.  `yank-pop-secondary' deletes that text and
inserts in its place a different stretch of killed text.

With no prefix argument, inserts the previous secondary selection.
With argument N, inserts the Nth previous (or Nth next, if negative).
The ring of secondary selections wraps around.

This command honors `yank-excluded-properties' and `yank-handler'."
  (interactive "*p")
  (unless (memq last-command secondary-selection-yank-secondary-commands)
    (error "Previous command did not yank secondary selection"))
  (setq this-command  'yank-secondary)
  (unless arg (setq arg  1))
  (let ((inhibit-read-only  t)
        (before             (< (point) (mark t))))
    (if before
        (funcall (or yank-undo-function 'delete-region) (point) (mark t))
      (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
    (setq yank-undo-function  nil)
    (set-marker (mark-marker) (point) (current-buffer))
    (funcall (if (fboundp 'insert-for-yank) 'insert-for-yank 'insert)
             (current-secondary-selection arg))
    ;; Set the window start back where it was in the yank command, if possible.
    (set-window-start (selected-window) yank-window-start t)
    (when before
      ;; This is like `exchange-point-and-mark', but this doesn't activate the mark.
      ;; It is cleaner to avoid activation, even though the command
      ;; loop would deactivate the mark because we inserted text.
      (goto-char (prog1 (mark t)
                   (set-marker (mark-marker) (point) (current-buffer))))))
  nil)

;; Like `current-kill'.
(defun current-secondary-selection (n &optional do-not-move)
  "Rotate yanking point by N places, then return that secondary selection.
If optional arg DO-NOT-MOVE is non-nil, then don't actually
move the yanking point; just return the Nth kill forward."
  (or secondary-selection-ring (error "No secondary selection"))
  (let ((secondary-elt  (nthcdr (mod (- n (length secondary-selection-ring-yank-pointer))
                                     (length secondary-selection-ring))
                                secondary-selection-ring)))
    (unless do-not-move (setq secondary-selection-ring-yank-pointer  secondary-elt))
    (car secondary-elt)))

;; Not used.
;;;###autoload
(defun rotate-secondary-selection-yank-pointer (arg)
  "Rotate the yanking point in the secondary selection ring.
With prefix arg, rotate that many kills forward or backward."
  (interactive "p")
  (current-secondary-selection arg))


;; ADVISES ORIGINAL in `mouse.el'.
;;
(defadvice mouse-drag-secondary (after populate-secondary-ring activate)
  "Add secondary selection to `secondary-selection-ring'."
  (and (overlayp mouse-secondary-overlay)
       (overlay-buffer mouse-secondary-overlay)
       (add-secondary-to-ring
        (x-set-selection 'SECONDARY
                         (buffer-substring (overlay-start mouse-secondary-overlay)
                                           (overlay-end   mouse-secondary-overlay))))))


;;; REPLACES ORIGINAL in `mouse.el'.
;;;
;;; Use `add-secondary-to-ring' instead of `kill-new'.
;;;
;;;###autoload
(defun mouse-secondary-save-then-kill (click)
  "Extend or delete secondary selection and save in ring.
Adds the extended secondary selection to `secondary-selection-ring'.
Use this in a buffer where you have recently done `\\[mouse-start-secondary]'.
If you have already made a secondary selection in that buffer, this
command extends or retracts the selection to where you click.  If you
do this again in a different position, it extends or retracts again.
If you do this twice in the same position, it kills the selection."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let ((posn        (event-start click))
        (click-posn  (posn-point (event-start click)))
        ;; Don't let a subsequent kill command append to this one:
        ;; prevent setting this-command to kill-region.
        (this-command this-command))
    (or (eq (window-buffer (posn-window posn))
            (or (and (overlayp mouse-secondary-overlay)
                     (overlay-buffer mouse-secondary-overlay))
                (if mouse-secondary-start
                    (marker-buffer mouse-secondary-start))))
        (error "`mouse-secondary-save-then-kill': wrong buffer - no secondary sel here"))
    (with-current-buffer (window-buffer (posn-window posn))
      (if (> (mod mouse-secondary-click-count 3) 0)
          (if (not (and (eq last-command 'mouse-secondary-save-then-kill)
                        (equal click-posn
                               (car (cdr-safe (cdr-safe mouse-save-then-kill-posn))))))
              ;; Find both ends of the object selected by this click.
              (let ((range  (mouse-start-end click-posn click-posn
                                             mouse-secondary-click-count)))
                ;; Move whichever end is closer to the click.
                ;; That's what xterm does, and it seems reasonable.
                (if (< (abs (- click-posn (overlay-start mouse-secondary-overlay)))
                       (abs (- click-posn (overlay-end mouse-secondary-overlay))))
                    (move-overlay mouse-secondary-overlay (car range)
                                  (overlay-end mouse-secondary-overlay))
                  (move-overlay mouse-secondary-overlay
                                (overlay-start mouse-secondary-overlay)
                                (nth 1 range)))
                (add-secondary-to-ring (x-set-selection
                                        'SECONDARY
                                        (buffer-substring
                                         (overlay-start mouse-secondary-overlay)
                                         (overlay-end mouse-secondary-overlay)))
                                       t) ; Replace old with extended.
                ;; Arrange for a repeated mouse-3 to kill this region.
                (setq mouse-save-then-kill-posn
                      (list (car kill-ring) (point) click-posn)))
            ;; If we click this button again without moving it,
            ;; that time kill.
            (progn
              (mouse-save-then-kill-delete-region
               (overlay-start mouse-secondary-overlay)
               (overlay-end mouse-secondary-overlay))
              (setq mouse-save-then-kill-posn    nil
                    mouse-secondary-click-count  0)
              (delete-overlay mouse-secondary-overlay)))
        (if (and (eq last-command 'mouse-secondary-save-then-kill)
                 mouse-save-then-kill-posn
                 (eq (car mouse-save-then-kill-posn) (car kill-ring))
                 (equal (cdr mouse-save-then-kill-posn) (list (point) click-posn)))
            ;; If this is the second time we've called
            ;; mouse-secondary-save-then-kill, delete the text from the buffer.
            (progn
              (mouse-save-then-kill-delete-region
               (overlay-start mouse-secondary-overlay)
               (overlay-end mouse-secondary-overlay))
              (setq mouse-save-then-kill-posn  nil)
              (delete-overlay mouse-secondary-overlay))
          (if (overlay-start mouse-secondary-overlay)
              ;; We have a selection, so adjust it.
              (progn
                (if (numberp click-posn)
                    (progn
                      ;; Move whichever end of the region is closer to the click.
                      ;; That is what xterm does, and it seems reasonable.
                      (if (< (abs (- click-posn (overlay-start mouse-secondary-overlay)))
                             (abs (- click-posn (overlay-end mouse-secondary-overlay))))
                          (move-overlay mouse-secondary-overlay click-posn
                                        (overlay-end mouse-secondary-overlay))
                        (move-overlay mouse-secondary-overlay
                                      (overlay-start mouse-secondary-overlay)
                                      click-posn))
                      (setq deactivate-mark  nil)))
                (if (eq last-command 'mouse-secondary-save-then-kill)
                    (add-secondary-to-ring (x-set-selection
                                            'SECONDARY
                                            (buffer-substring
                                             (overlay-start mouse-secondary-overlay)
                                             (overlay-end mouse-secondary-overlay)))
                                           t) ; Replace old with extended.
                  (let ((deactivate-mark  nil))
                    (copy-region-as-kill (overlay-start mouse-secondary-overlay)
                                         (overlay-end mouse-secondary-overlay)))))
            (if mouse-secondary-start
                ;; All we have is one end of a selection,
                ;; so put the other end here.
                (let ((start  (+ 0 mouse-secondary-start)))
                  (kill-ring-save start click-posn)
                  (if mouse-secondary-overlay
                      (move-overlay mouse-secondary-overlay start click-posn)
                    (setq mouse-secondary-overlay  (make-overlay start click-posn)))
                  (overlay-put mouse-secondary-overlay 'face 'secondary-selection))))
          (setq mouse-save-then-kill-posn  (list (car kill-ring) (point) click-posn))))
      (when (overlay-buffer mouse-secondary-overlay)
        (x-set-selection 'SECONDARY (buffer-substring
                                     (overlay-start mouse-secondary-overlay)
                                     (overlay-end mouse-secondary-overlay)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'second-sel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; second-sel.el ends here
