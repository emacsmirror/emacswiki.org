;;; misc-cmds.el --- Miscellaneous commands (interactive functions).
;;
;; Filename: misc-cmds.el
;; Description: Miscellaneous commands (interactive functions).
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2013, Drew Adams, all rights reserved.
;; Created: Wed Aug  2 11:20:41 1995
;; Version: 21.1
;; Last-Updated: Fri Dec 28 10:13:14 2012 (-0800)
;;           By: dradams
;;     Update #: 3076
;; URL: http://www.emacswiki.org/misc-cmds.el
;; Keywords: internal, unix, extensions, maint, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `frame-fns', `misc-cmds', `misc-fns', `strings',
;;   `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Miscellaneous commands (interactive functions).
;;
;;  Commands defined here:
;;
;;    `beginning-of-line+', `beginning-of-visual-line+',
;;    `beginning-or-indentation', `chgrp', `chmod', `chown',
;;    `clear-regexp-search-history', `clear-regexp-search-ring'
;;    `clear-search-history', `clear-search-ring',
;;    `clear-search-histories', `count-chars-in-region',
;;    `delete-lines', `end-of-line+', `end-of-visual-line+.',
;;    `forward-char-same-line', `forward-overlay',
;;    `goto-previous-mark', `indirect-buffer',
;;    `kill-buffer-and-its-windows', `list-colors-nearest',
;;    `list-colors-nearest-color-at', `mark-buffer-after-point',
;;    `mark-buffer-before-point', `old-rename-buffer',
;;    `recenter-top-bottom', `recenter-top-bottom-1',
;;    `recenter-top-bottom-2', `region-length', `region-to-buffer',
;;    `region-to-file', `resolve-file-name',
;;    `revert-buffer-no-confirm', `selection-length',
;;    `view-X11-colors'.
;;
;;  Non-interactive functions defined here:
;;
;;    `line-number-at-pos', `read-shell-file-command'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;    `rename-buffer' - Uses (lax) completion.
;;
;;  Suggested key bindings:
;;
;;   (define-key ctl-x-map [home]     'mark-buffer-before-point)
;;   (define-key ctl-x-map [end]      'mark-buffer-after-point)
;;   (define-key ctl-x-map "\M-f"     'region-to-file)
;;   (global-set-key [C-S-f1]         'region-to-buffer)
;;   (global-set-key [C-S-backspace]  'region-to-file)
;;   (global-set-key [home]           'backward-line-text)
;;   (global-set-key [f5]             'revert-buffer-no-confirm) ; A la MS Windows
;;   (substitute-key-definition       'kill-buffer
;;                                    'kill-buffer-and-its-windows global-map)
;;   (substitute-key-definition       'recenter 'recenter-top-bottom global-map)
;;   (substitute-key-definition       'beginning-of-line 'beginning-of-line+ global-map)
;;   (substitute-key-definition       'end-of-line 'end-of-line+ global-map)
;;
;;   The first two of these are needed to remove the default remappings.
;;   (define-key visual-line-mode-map [remap move-beginning-of-line] nil)
;;   (define-key visual-line-mode-map [remap move-end-of-line] nil)
;;   (define-key visual-line-mode-map [home] 'beginning-of-line+)
;;   (define-key visual-line-mode-map [end]  'end-of-line+)
;;   (define-key visual-line-mode-map "\C-a" 'beginning-of-visual-line+)
;;   (define-key visual-line-mode-map "\C-e" 'end-of-visual-line+)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/12/24 dadams
;;     Added: beginning-of-visual-line+, end-of-visual-line+.
;; 2012/11/10 dadams
;;     Added: list-colors-nearest, list-colors-nearest-color-at.
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     resolve-file-name: Invoke tap-define-aliases-wo-prefix if thingatpt+.el loaded.
;;                        Use tap-bounds-of-thing-at-point if defined.
;; 2011/12/21 dadams
;;     Replaced redefinition of rename-buffer with defadvice.
;; 2011/12/19 dadams
;;     goto-long-line: Use line-end-position, not end-of-line + point.
;; 2011/12/12 dadams
;;     Added (redefinition of) rename-buffer.
;;     Soft require strings.el.
;; 2011/09/06 dadams
;;     Added: resolve-file-name.
;; 2011/05/10 dadams
;;     Removed hide/show-comments - moved it to thing-cmds.el.
;; 2011/05/06 dadams
;;     Added: hide/show-comments.
;; 2011/01/04 dadams
;;     Added autoload cookies for commands.  Removed from non-interactive function.
;; 2010/01/12 dadams
;;     region-to-buffer: save-excursion + set-buffer -> with-current-buffer.
;; 2009/09/24 dadams
;;     Removed no-op - use predefined function ignore instead.
;; 2009/06/02 dadams
;;     revert-buffer-no-confirm: Redefined using existing args (duh).
;; 2009/04/26 dadams
;;     forward-char-same-line, end-of-line+, goto-long(est)-line, delete-lines:
;;       Bind inhibit-field-text-motion to t, for end-of-line.
;; 2009/04/08 dadams
;;     Added: revert-buffer-no-confirm.
;;     eval-when-compile cl.el, regardless of Emacs version.
;; 2008/05/23 dadams
;;     Moved to new library second-sel.el:
;;       primary-to-secondary, rotate-secondary-selection-yank-pointer,
;;       secondary-to-primary, yank-pop(-commands|secondary), yank-undo-function,
;;       current-secondary-selection,  yank-secondary(-or-swap-w-region),
;;       secondary-selection(-ring(-max|-yank-pointer)), add-secondary-to-ring,
;;       mouse-drag-secondary, mouse-secondary-save-then-kill.
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
;; 2008/03/02 dadams
;;     describe-file: Use default dir if arg is nil.
;;                    Removed "icicle" typos. Removed save-excursion.
;;     Moved describe-file to help+20.el and help-fns+.el.
;; 2008/01/23 dadams
;;     goto-longest-line: Fix to work with narrowed buffer.
;;       Allow empty region, so can just search forward directly.
;;       If point at end of region, exchange with mark.
;;       If end is before beg, swap them.
;;       If only 1 line in region, give "Only..." msg, flash, and deactivate mark.
;;       Don't stop while loop just because mark is inactive.
;;       Go to target relative to point-min, not to absolute line nb, in case narrowed.
;;       Highlight current line (Emacs 22).
;;     Removed: region-or-buffer-limits.
;; 2007-11-14
;;     Rewrote recenter-top-bottom to combine the best of -1 and -2.
;; 2007/11/11 dadams
;;     recenter-top-bottom: Rewrote to base destination on current window position.
;;     Renamed original recenter-top-bottom to recenter-top-bottom-bis, and added
;;       treatment of scroll-conservatively.
;; 2007/11/06 dadams
;;     Added: recenter-top-bottom.
;; 2007/09/24 dadams
;;     Added: mark-buffer-(before|after)-point.
;; 2007/09/19 dadams
;;     Define goto-previous-mark only if pop-to-mark-command is not defined (Emacs <22).
;;     Removed goto-previous-global-mark.
;; 2007/04/28 dadams
;;     goto-longest-line: Fixed mapconcat arg for end message.
;; 2007/04/02 dadams
;;     Added: region-or-buffer-limits
;;     goto-longest-line: Redefined using region-or-buffer-limits.
;; 2007/03/10 dadams
;;     goto-longest-line: Raise error if region is empty.
;; 2007/01/13 dadams
;;     Added: describe-file.
;; 2006/10/21 dadams
;;     yank-secondary: Error message if there is no secondary selection.
;; 2006/08/19 dadams
;;     Added: goto-long(est)-line, line-number-at-pos.
;; 2006/02/11 dadams
;;     Added: region-length (selection-length, count-chars-in-region).
;; 2006/01/28 dadams
;;     Added: clear(-regexp)-search-history, clear-search-histories.
;; 2006/01/01 dadams
;;     defsubst -> defun.
;; 2005/07/15 dadams
;;     Moved delete-lines back here.
;; 2005/07/14 dadams
;;     forward-overlay: ensure arg is a number.
;; 2005/07/12 dadams
;;     forward-char-same-line: Convert raw prefix arg to numeric before arithmetic.
;; 2005/07/10 dadams
;;     Removed delete-lines (moved to icicles.el and renamed icicles-delete-lines).
;; 2005/05/28 dadams
;;     region-to-buffer: Use another-buffer, if available.
;; 2005/05/09 dadams
;;     Renamed: flash-ding-minibuffer-frame to 1on1-flash-ding-minibuffer-frame.
;; 2005/01/20 dadams
;;     Removed exit-with-confirmation (use kill-emacs-query-functions in setup.el).
;; 2004/11/16 dadams
;;     Replaced beginning-of-line*, end-of-line* with + versions.
;; 2004/11/14 dadams
;;     Added beginning-or-indentation, beginning-of-line*, end-of-line*.
;; 2000/11/28 dadams
;;     Optional require's via 3rd arg=t now.
;; 1999/04/13  dadams
;;     Added: delete-lines.
;; 1999/03/17 dadams
;;     1. Protect with fboundp.
;;     2. kill-buffer-and-its-windows: use get-buffer-window-list.
;;     3. Commented out: xwud, display-xwd-image-file, xwd,
;;        capture-image-as-xwd-file, display-buffer.
;; 1996/06/03 dadams
;;     display-xwd-image-file: Do via background processes:
;;                             shell-command -> start-process-shell-command.
;; 1996/06/03 dadams
;;     display-xwd-image-file:
;;       1. Allow XWD-FILE arg as list.  Added DIR arg.
;;       2. No longer provide -noclick option by default.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/04/24 dadams
;;     Added: read-shell-file-command, chmod, chgrp, chown.
;; 1996/04/23 dadams
;;     Added display-xwd-image-file (xwud) and capture-image-as-xwd-file (xwd).
;; 1996/04/23 dadams
;;     Added: goto-previous-mark, goto-previous-global-mark.
;; 1996/04/16  dadams
;;     Added declp-buffer-w-switches and declp-region-w-switches.
;; 1996/03/20 dadams
;;     no-op, exit-with-confirmation, view-X11-colors, forward-overlay,
;;     declp-buffer, declp-region, yank-secondary: defun -> defsubst
;; 1996/02/28 dadams
;;     Added forward-overlay, forward-char-same-line.
;; 1996/02/15 dadams
;;     Added yank-secondary.
;; 1996/02/06 dadams
;;     Put variable-interactive property on appropriate user option vars.
;; 1996/02/05 dadams
;;     1. Added: default-pr-switches, declp-switches, declp-sheet-options.
;;     2. declp-buffer,declp-region,pr-declp-buffer,pr-declp-region: Optional args.
;;     3. pr-declp-buffer, pr-declp-region, declp-region-1:
;;        Proper treatment of pr switches; pr error treatment; No BSD lpr shortcut.
;; 1996/01/25 dadams
;;     kill-buffer-and-its-windows: Added args to call to windows-on.
;; 1996/01/16 dadams
;;     Added: read-number-up, declp-buffer, declp-region, pr-declp-buffer,
;;            pr-declp-region.
;; 1996/01/12 dadams
;;     Added region-to-buffer, region-to-file.
;; 1996/01/08  dadams
;;     Added redefinition of display-buffer that raises frame.
;; 1995/08/24 dadams
;;     1) Added view-X11-colors.  2) flash-ding -> flash-ding-minibuffer-frame.
;; 1995/08/18 dadams
;;     1) Added no-op and local version of print-region-1.
;; 1995/08/08 dadams
;;     Added: exit-with-confirmation, lpr stuff.
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

(eval-when-compile (require 'cl)) ;; case, plus for Emacs < 21: dolist, pop

(require 'frame-fns nil t) ;; (no error if not found): flash-ding
(require 'misc-fns nil t) ;; (no error if not found): another-buffer, color-named-at
(require 'strings nil t) ;; (no error if not found): read-buffer

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-cmds)
(require 'misc-cmds)                 ; Ensure loaded before compile this.

;;;;;;;;;;;;;;;;;;;;;;;


;; ADVISE ORIGINAL `rename-buffer' (built-in).
;;
;; 1. Provide (lax) completion for new buffer name.
;; 2. Use name of current buffer as default (< Emacs 23).
;;
(defadvice rename-buffer (before read-buffer-completing activate)
  "Interactively, (lax) completion is available for the buffer name."
  (interactive (list (read-buffer "Rename buffer (to new name): " (buffer-name))
                     current-prefix-arg)))

;;;###autoload
(defun forward-overlay (&optional arg)
  "Move forward ARG overlays.
Move cursor to next position where an overlay starts or ends.
If there are no more overlay boundaries, move to (point-max)."
  (interactive "p")
  (setq arg (or arg 1))
  (setq arg (1- arg))
  (while (natnump arg) (goto-char (next-overlay-change (point))) (decf arg)))

;;;###autoload
(defun forward-char-same-line (&optional arg)
  "Move forward a max of ARG chars on the same line, or backward if ARG < 0.
Returns the signed number of chars moved if /= ARG, else returns nil."
  (interactive "p")
  (let* ((start                      (point))
         (fwd-p                      (natnump arg))
         (inhibit-field-text-motion  t) ; Just to be sure, for `end-of-line'.
         (max (save-excursion (if fwd-p (end-of-line) (beginning-of-line))
                              (- (point) start))))
    (setq arg  (prefix-numeric-value arg))
    (forward-char (if fwd-p (min max arg) (max max arg)))
    (and (< (abs max) (abs arg)) max)))

;;;###autoload
(defun end-of-line+ (&optional n)
  "Move cursor to end of current line or end of next line if repeated.
This is similar to `end-of-line', but:
  If called interactively with no prefix arg:
     If the previous command was also `end-of-line+', then move to the
     end of the next line.  Else, move to the end of the current line.
  Otherwise, move to the end of the Nth next line (Nth previous line
     if N<0).  Command `end-of-line', by contrast, moves to the end of
     the (N-1)th next line."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 0)))
  (unless n (setq n 0))                 ; non-interactive with no arg
  (if (and (eq this-command last-command) (not current-prefix-arg))
      (forward-line 1)
    (forward-line n))
  (let ((inhibit-field-text-motion  t))  (end-of-line)))

;;;###autoload
(defun beginning-of-line+ (&optional n)
  "Move cursor to beginning of current line or next line if repeated.
This is the similar to `beginning-of-line', but:
1. With arg N, the direction is the opposite: this command moves
   backward, not forward, N lines.
2. If called interactively with no prefix arg:
      If the previous command was also `beginning-of-line+', then move
      to the beginning of the previous line.  Else, move to the
      beginning of the current line.
   Otherwise, move to the beginning of the Nth previous line (Nth next
      line if N<0).  Command `beginning-of-line', by contrast, moves to
      the beginning of the (N-1)th next line."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 0)))
  (unless n (setq n 0))                 ; non-interactive with no arg
  (if (and (eq this-command last-command) (not current-prefix-arg))
      (forward-line -1)
    (forward-line (- n))))

;;;###autoload (autoload 'end-of-visual-line+ "misc-cmds")
;;;###autoload (autoload 'beginning-of-visual-line+ "misc-cmds")
(when (fboundp 'visual-line-mode)       ; Emacs 23+
  (defun end-of-visual-line+ (&optional n)
    "Move cursor to end of current visual line, or end of next if repeated.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t.

This is similar to `end-of-visual-line', but:
  If called interactively with no prefix arg:
     If the previous command was also `end-of-visual-line+', then move
     to the end of the next visual line.  Else, end of current one.
  Otherwise, move to the end of the Nth next visual line (Nth previous
     one if N<0).  Command `end-of-visual-line', by contrast, moves to
     the end of the (N-1)th next line."
    (interactive
     (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 0)))
    (unless n (setq n 0))               ; non-interactive with no arg 
    (let ((line-move-visual  t))
      (if (and (eq this-command last-command)  (not current-prefix-arg))
          (line-move 1 t)
        (line-move n t)))
    ;; Unlike `move-beginning-of-line', `move-end-of-line' doesn't
    ;; constrain to field boundaries, so we don't either.
    (vertical-motion (cons (window-width) 0)))

  (defun beginning-of-visual-line+ (&optional n)
    "Move cursor to beginning of current visual line or next if repeated.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t.

This is the similar to `beginning-of-visual-line', but:
1. With arg N, the direction is the opposite: this command moves
   backward, not forward, N visual lines.
2. If called interactively with no prefix arg:
      If the previous command was also `beginning-of-visual-line+',
      then move to the beginning of the previous visual line.  Else,
      move to the beginning of the current visual line.
   Otherwise, move to the beginning of the Nth previous visual line
      (Nth next one if N<0).  Command `beginning-of-visual-line', by
      contrast, moves to the beginning of the (N-1)th next visual
      line."
    (interactive
     (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 0)))
    (unless n (setq n 0))               ; non-interactive with no arg
    (let ((opoint  (point)))
      (let ((line-move-visual  t))
        (if (and (eq this-command last-command)  (not current-prefix-arg))
            (line-move -1 t)
          (line-move n t)))
      (vertical-motion 0)
      ;; Constrain to field boundaries, like `move-beginning-of-line'.
      (goto-char (constrain-to-field (point) opoint (/= n 1))))))

;;;###autoload
(defun beginning-or-indentation (&optional n)
  "Move cursor to beginning of this line or to its indentation.
If at indentation position of this line, move to beginning of line.
If at beginning of line, move to beginning of previous line.
Else, move to indentation position of this line.

With arg N, move backward to the beginning of the Nth previous line.
Interactively, N is the prefix arg."
  (interactive "P")
  (cond ((or (bolp) n)
         (forward-line (- (prefix-numeric-value n))))
        ((save-excursion (skip-chars-backward " \t") (bolp)) ; At indentation.
         (forward-line 0))
        (t (back-to-indentation))))

;;;###autoload
(defun recenter-top-bottom (&optional arg)
  "Move current line to window center, top, and bottom, successively.
With a prefix argument, this is the same as `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain `C-u', move current line to window center.

Otherwise move current line to window center on first call, and to
top, middle, or bottom on successive calls.

The starting position of the window determines the cycling order:
 If initially in the top or middle third: top -> middle -> bottom.
 If initially in the bottom third: bottom -> middle -> top.

Top and bottom destinations are actually `scroll-conservatively' lines
from true window top and bottom."
  (interactive "P")
  (if arg                               ; Always respect ARG.
      (recenter arg)
    (case last-command
      (recenter-tb-top                  ; Top -> middle -> bottom
       (setq this-command  'recenter-tb-middle)
       (recenter))
      (recenter-tb-middle
       (setq this-command  'recenter-tb-bottom)
       (recenter (1- (- scroll-conservatively))))
      (recenter-tb-bottom
       (setq this-command  'recenter-tb-top)
       (recenter scroll-conservatively))
      (recenter-tb-bottom-1             ; Bottom -> middle -> top
       (setq this-command  'recenter-tb-middle-1)
       (recenter))
      (recenter-tb-middle-1
       (setq this-command  'recenter-tb-top-1)
       (recenter scroll-conservatively))
      (recenter-tb-top-1
       (setq this-command  'recenter-tb-bottom-1)
       (recenter (1- (- scroll-conservatively))))
      (otherwise                        ; First time - save mode and recenter.
       (let ((top      (1+ (count-lines 1 (window-start))))
             (current  (1+ (count-lines 1 (point))))
             (total    (window-height)))
         (setq this-command  (if (< (- current top) (/ total 3))
                                 'recenter-tb-middle
                               'recenter-tb-middle-1)))
       (recenter)))))

;; An alternative.
;;;###autoload
(defun recenter-top-bottom-1 (&optional arg)
  "Move current line to window center, top, and bottom, successively.
With prefix ARG, move current line to window-line ARG.
Top and bottom destinations are actually `scroll-conservatively' lines
from true top and bottom."
  (interactive "P")
  (cond ((and (eq this-command last-command) (not arg))
         (setq this-command 'recenter-top-bottom-top)
         (recenter scroll-conservatively))
        ((and (eq 'recenter-top-bottom-top last-command) (not arg))
         (setq this-command 'recenter-top-bottom-bottom)
         (recenter (1- (- scroll-conservatively))))
        (t (recenter arg))))

;; Another alternative.
;;;###autoload
(defun recenter-top-bottom-2 (&optional arg)
  "Move current line to line ARG, window center, top, or bottom.
With a prefix argument, this is the same as `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain `C-u', move current line to window center.

Otherwise, the window starting position determines the next position:
 If in the top third, move to bottom.
 If in middle third,  move to top.
 If in bottom third,  move tocenter.

Top and bottom destinations are actually `scroll-conservatively' lines
from true top and bottom."
  (interactive "P")
  (cond (arg  (recenter arg))
        (t
         (let* ((top      (1+ (count-lines 1 (window-start))))
                (bottom   (1+ (count-lines 1 (window-end))))
                (current  (1+ (count-lines 1 (point))))
                (total    (window-height)))
           (cond ((< (- current top) (/ total 3))
                  (recenter (1- (- scroll-conservatively))))
                 ((< (- bottom current) (/ total 3)) (recenter '(4)))
                 (t (recenter scroll-conservatively)))))))

;;;###autoload
(defun mark-buffer-after-point (reversep)
  "Select the part of the buffer after point.
With a prefix argument, select the part before point."
  (interactive "P")
  (push-mark (if reversep (point-min) (point-max)) nil t)
  (setq deactivate-mark  nil))

;;;###autoload
(defun mark-buffer-before-point (reversep)
  "Select the part of the buffer before point.
With a prefix argument, select the part after point."
  (interactive "P")
  (mark-buffer-after-point t))

;;;###autoload
(defalias 'selection-length 'region-length)
;;;###autoload
(defalias 'count-chars-in-region 'region-length)
;;;###autoload
(defun region-length ()
  "Display the number of characters in the region in a message."
  (interactive)
  (let ((len  (abs (- (mark) (point)))))
    (message "Region contains %s characters" len)
    len))

(unless (fboundp 'line-number-at-pos)   ; Exists in Emacs 22.
  (defun line-number-at-pos (&optional pos)
    "Buffer line number at position POS. Current line number if POS is nil.
Counting starts at (point-min), so any narrowing restriction applies."
    (1+ (count-lines (point-min) (save-excursion (when pos (goto-char pos))
                                                 (forward-line 0) (point))))))

;;;###autoload
(defun goto-longest-line (beg end)
  "Go to the first of the longest lines in the region or buffer.
If the region is active, it is checked.
If not, the buffer (or its restriction) is checked.

Returns a list of three elements:

 (LINE LINE-LENGTH OTHER-LINES LINES-CHECKED)

LINE is the first of the longest lines measured.
LINE-LENGTH is the length of LINE.
OTHER-LINES is a list of other lines checked that are as long as LINE.
LINES-CHECKED is the number of lines measured.

Interactively, a message displays this information.

If there is only one line in the active region, then the region is
deactivated after this command, and the message mentions only LINE and
LINE-LENGTH.

If this command is repeated, it checks for the longest line after the
cursor.  That is *not* necessarily the longest line other than the
current line.  That longest line could be before or after the current
line.

To search only from the current line forward, not throughout the
buffer, you can use `C-SPC' to set the mark, then use this
\(repeatedly)."
  (interactive
   (if (or (not mark-active) (null (mark)))
       (list (point-min) (point-max))
     (if (< (point) (mark))
         (list (point) (mark))
       (list (mark) (point)))))
  (when (and (not mark-active) (= beg end))
    (error "The buffer is empty"))
  (when (and mark-active (> (point) (mark))) (exchange-point-and-mark))
  (when (< end beg) (setq end (prog1 beg (setq beg end))))
  (when (eq this-command last-command)
    (forward-line 1) (setq beg (point)))
  (goto-char beg)
  (when (eobp) (error "End of buffer"))
  (cond ((<= end (save-excursion (goto-char beg) (forward-line 1) (point)))
         (let ((inhibit-field-text-motion  t))  (beginning-of-line))
         (when (and (> emacs-major-version 21) (require 'hl-line nil t))
           (let ((hl-line-mode  t))  (hl-line-highlight))
           (add-hook 'pre-command-hook #'hl-line-unhighlight nil t))
         (let ((lineno  (line-number-at-pos))
               (chars   (let ((inhibit-field-text-motion t))
                          (save-excursion (end-of-line) (current-column)))))
           (message "Only line %d: %d chars" lineno chars)
           (let ((visible-bell  t))  (ding))
           (setq mark-active  nil)
           (list lineno chars nil 1)))
        (t
         (let* ((start-line                 (line-number-at-pos))
                (max-width                  0)
                (line                       start-line)
                (inhibit-field-text-motion  t)
                long-lines col)
           (when (eobp) (error "End of buffer"))
           (while (and (not (eobp)) (< (point) end))
             (end-of-line)
             (setq col  (current-column))
             (when (>= col max-width)
               (setq long-lines  (if (= col max-width)
                                     (cons line long-lines)
                                   (list line))
                     max-width   col))
             (forward-line 1)
             (setq line  (1+ line)))
           (setq long-lines  (nreverse long-lines))
           (let ((lines  long-lines))
             (while (and lines (> start-line (car lines))) (pop lines))
             (goto-char (point-min))
             (when (car lines) (forward-line (1- (car lines)))))
           (when (and (> emacs-major-version 21) (require 'hl-line nil t))
             (let ((hl-line-mode  t))  (hl-line-highlight))
             (add-hook 'pre-command-hook #'hl-line-unhighlight nil t))
           (when (interactive-p)
             (let ((others  (cdr long-lines)))
               (message "Line %d: %d chars%s (%d lines measured)"
                (car long-lines) max-width
                (concat
                 (and others
                      (format ", Others: {%s}" (mapconcat
                                                (lambda (line) (format "%d" line))
                                                (cdr long-lines) ", "))))
                (- line start-line))))
           (list (car long-lines) max-width (cdr long-lines) (- line start-line))))))

;;;###autoload
(defun goto-long-line (len)
  "Go to the first line that is at least LEN characters long.
Use a prefix arg to provide LEN.
Plain `C-u' (no number) uses `fill-column' as LEN."
  (interactive "P")
  (setq len  (if (consp len) fill-column (prefix-numeric-value len)))
  (let ((start-line                 (line-number-at-pos))
        (len-found                  0)
        (found                      nil)
        (inhibit-field-text-motion  t))
    (while (and (not found) (not (eobp)))
      (forward-line 1)
      (setq found  (< len (setq len-found  (- (line-end-position) (point))))))
    (if found
        (when (interactive-p)
          (message "Line %d: %d chars" (line-number-at-pos) len-found))
      (goto-line start-line)
      (message "Not found"))))

;;;###autoload
(defun delete-lines (num-lines)
  "Delete NUM-LINES lines, starting at point.
Lines are deleted, not killed.
With positive prefix arg, deletion is forward.
With negative prefix arg, deletion is backward."
  (interactive "p")
  (when (not (zerop num-lines))
    (let ((column                     (current-column))
          (forward-p                  (natnump num-lines))
          (inhibit-field-text-motion  t))
      (if forward-p (beginning-of-line) (end-of-line))
      (let ((beg  (point)))
        (forward-line (if forward-p (1- num-lines) (1+ num-lines)))
        (if forward-p (end-of-line) (beginning-of-line))
        (delete-region beg (point)))
      (when (eq (following-char) ?\n) (delete-char 1))
      (move-to-column column))))

;;;(defvar default-pr-switches "-fl68"
;;;  "*String of default switches to pass to `pr'.
;;;These may be overridden in `pr-declp-buffer' and `pr-declp-region'.")
;;;(put 'default-pr-switches 'variable-interactive
;;;     "sDefault switches to pass to `pr' (e.g. \"-fl68\"): ")

;;;(defvar declp-switches nil
;;;  "*List of strings to pass as extra switch args to `declp-command'.")

;;;(defvar declp-command "declp" "*Shell command for printing a file.
;;;Should usually be either \"declp\" or \"declpt\".")
;;;(put 'declp-command 'variable-interactive
;;;     "sShell command for printing a file. (\"declp\" or \"declpt\"): ")

;;;;;;###autoload
;;;(defmacro declp-sheet-options (number-up)
;;;  (` (if (and (integerp (, number-up)) (not (zerop (, number-up))))
;;;         (if (natnump (, number-up))
;;;             (format " -K 2 -N %d " (, number-up))
;;;           (format " -N %d " (, number-up)))
;;;       "")))

;;;;;;###autoload
;;;(defun declp-buffer-w-switches ()
;;;  "Print buffer using `declp-command' and switches that you specify.
;;;Variable `declp-switches' is a list of proposed default switches."
;;;  (interactive)
;;;  (let ((cmd (read-from-minibuffer
;;;              (concat "Print buffer `" (buffer-name) "' with command:   ")
;;;              (apply 'concat declp-command " " declp-switches) nil nil
;;;              'minibuffer-history)))
;;;    (save-restriction (widen) (message "Spooling...")
;;;                      (shell-command-on-region (point-min) (point-max) cmd)))
;;;  (message "Spooling... done"))

;;;(defun declp-buffer (&optional number-up)
;;;  "Print buffer contents using `declp-command'.
;;;NUM-UP pages are printed on a side of paper, bordered by a rectangle
;;;if NUM-UP is a non-zero integer.  NUM-UP is the prefix arg, if any.
;;;Otherwise you are prompted for NUM-UP.
;;;   NUM-UP > 0 => Print on both sides of paper.
;;;   NUM-UP < 0 => Only print on one side of paper.
;;;   Otherwise  => Print 1 page per sheet, on one side of paper, and
;;;                 do not print a rectangular border around each page.
;;;Global variable `declp-switches' is a list of switches (strings)
;;;for `declp-command'."
;;;  (interactive (list (if current-prefix-arg
;;;                         (prefix-numeric-value current-prefix-arg)
;;;                       (read-number-up 'declp-buffer))))
;;;  (declp-region-1 (point-min) (point-max)
;;;                  (cons (declp-sheet-options number-up) declp-switches)))

;;;;;;###autoload
;;;(defun declp-region-w-switches (start end)
;;;  "Print region using `declp-command' and switches that you specify.
;;;Variable `declp-switches' is a list of proposed default switches."
;;;  (interactive "r")
;;;  (let ((cmd (concat (read-from-minibuffer
;;;                      (concat "Print region with command:   ")
;;;                      (apply 'concat declp-command " " declp-switches) nil nil
;;;                      'minibuffer-history))))
;;;    (message "Spooling...")
;;;    (shell-command-on-region start end cmd))
;;;  (message "Spooling... done"))

;;;(defun declp-region (start end &optional number-up)
;;;  "Print region contents using `declp-command'.
;;;NUM-UP pages are printed on a side of paper, bordered by a rectangle
;;;if NUM-UP is a non-zero integer.  NUM-UP is the prefix arg, if any.
;;;Otherwise you are prompted for NUM-UP.
;;;   NUM-UP > 0 => Print on both sides of paper.
;;;   NUM-UP < 0 => Only print on one side of paper.
;;;   Otherwise  => Print 1 page per sheet, on one side of paper, and
;;;                 do not print a rectangular border around each page.
;;;Global variable `declp-switches' is a list of switches (strings)
;;;for `declp-command'."
;;;  (interactive (list (region-beginning) (region-end)
;;;                     (if current-prefix-arg
;;;                         (prefix-numeric-value current-prefix-arg)
;;;                       (read-number-up 'declp-region))))
;;;  (declp-region-1 start end
;;;                  (cons (declp-sheet-options number-up) declp-switches)))

;;;;;;###autoload
;;;(defun pr-declp-buffer (&optional number-up pr-switches)
;;;  "Print buffer with page headings using `declp-command'.
;;;The Unix `pr' command is used to provide the page headings.
;;;You are prompted for PR-SWITCHES, which is a string of switches
;;;to the `pr' command.  For information on `pr', type `\\[manual-entry] pr'.
;;;\(Note: The `-m' option to `pr' makes no sense in this context.)

;;;NUM-UP pages are printed on a side of paper, bordered by a rectangle
;;;if NUM-UP is a non-zero integer.  NUM-UP is the prefix arg, if any.
;;;Otherwise you are prompted for NUM-UP.
;;;   NUM-UP > 0 => Print on both sides of paper.
;;;   NUM-UP < 0 => Only print on one side of paper.
;;;   Otherwise  => Print 1 page per sheet, on one side of paper, and
;;;                 do not print a rectangular border around each page.

;;;Global variables:
;;;`declp-switches' is a list of switches (strings) for `declp-command'.
;;;`default-pr-switches' is a string of default switches for `pr'.
;;;Switches in PR-SWITCHES override those in `default-pr-switches'."
;;;  (interactive
;;;   (let (pr-opt
;;;         (pr-opts ()))
;;;     (list (if current-prefix-arg
;;;               (prefix-numeric-value current-prefix-arg)
;;;             (read-number-up 'pr-declp-region))
;;;           (progn
;;;             (setq pr-opts (list (read-from-minibuffer "Page title: "
;;;                                                       (cons (buffer-name) 1))
;;;                                 "-h")) ; Order reversed below to '-h title'.
;;;             (while (not (string= "" pr-opt))
;;;               (push (setq pr-opt (read-from-minibuffer
;;;                                   "Switches for `pr' (RET to end): "))
;;;                     pr-opts))
;;;             (pop pr-opts)              ; ""
;;;             (nreverse pr-opts)))))
;;;  (declp-region-1 (point-min) (point-max)
;;;                  (cons (declp-sheet-options number-up) declp-switches)
;;;                  (or pr-switches ""))) ; Non-nil for pr.

;;;;;;###autoload
;;;(defun pr-declp-region (start end &optional &optional number-up pr-switches)
;;;  "Print region with page headings using `declp-command'.
;;;The Unix `pr' command is used to provide the page headings.
;;;You are prompted for PR-SWITCHES, which is a string of switches
;;;to the `pr' command.  For information on `pr', type `\\[manual-entry] pr'.
;;;\(Note: The `-m' option to `pr' makes no sense in this context.)

;;;NUM-UP pages are printed on a side of paper, bordered by a rectangle
;;;if NUM-UP is a non-zero integer.  NUM-UP is the prefix arg, if any.
;;;Otherwise you are prompted for NUM-UP.
;;;   NUM-UP > 0 => Print on both sides of paper.
;;;   NUM-UP < 0 => Only print on one side of paper.
;;;   Otherwise  => Print 1 page per sheet, on one side of paper, and
;;;                 do not print a rectangular border around each page.

;;;Global variables:
;;;`declp-switches' is a list of switches (strings) for `declp-command'.
;;;`default-pr-switches' is a string of default switches for `pr'.
;;;Switches in PR-SWITCHES override those in `default-pr-switches'."
;;;  (interactive
;;;   (let (pr-opt
;;;         (pr-opts ()))
;;;     (list (region-beginning) (region-end)
;;;           (if current-prefix-arg
;;;               (prefix-numeric-value current-prefix-arg)
;;;             (read-number-up 'pr-declp-region))
;;;           (progn
;;;             (setq pr-opts (list (read-from-minibuffer "Page title: ") "-h"))
;;;             (while (not (string= "" pr-opt))
;;;               (push (setq pr-opt (read-from-minibuffer
;;;                                   "Switches for `pr' (RET to end): "))
;;;                     pr-opts))
;;;             (pop pr-opts)              ; ""
;;;             (nreverse pr-opts)))))
;;;  (declp-region-1 start end
;;;                  (cons (declp-sheet-options number-up) declp-switches)
;;;                  (or pr-switches ""))) ; Non-nil for pr.

;;;;; Adapted from `print-region-1' in `lpr.el'.
;;;(defun declp-region-1 (start end switches &optional page-headers)
;;;  ;; On some MIPS system, having a space in the job name
;;;  ;; crashes the printer demon.  But using dashes looks ugly
;;;  ;; and it seems too annoying to do for those MIPS systems.
;;;  (let ((name (concat (buffer-name) " Emacs buffer"))
;;;     (title (concat (buffer-name) " Emacs buffer"))
;;;     (width tab-width))
;;;    (save-excursion
;;;      (when (/= tab-width 8)
;;;        (print-region-new-buffer start end)
;;;        (setq tab-width width)
;;;        (save-excursion (goto-char end) (setq end (point-marker)))
;;;        (untabify (point-min) (point-max)))
;;;      ;; Filter region through `pr'.
;;;      (message "Filtering with `pr'...")
;;;      (when page-headers
;;;        (print-region-new-buffer start end)
;;;        (when (not (zerop (apply 'call-process-region start end "pr" t t nil
;;;                                 default-pr-switches page-headers)))
;;;          (display-buffer " *spool temp*")
;;;          (error "Error in switches to `pr'"))
;;;        (setq start (point-min))
;;;        (setq end (point-max)))
;;;      (message "Spooling...")
;;;      (apply 'shell-command-on-region
;;;             (list start end (apply 'concat declp-command " " switches)))
;;;      (when (markerp end) (set-marker end nil))
;;;      (message "Spooling... done"))))

;;;(defun read-number-up (fn)
;;;  "Read NUMBER-UP argument for a declp print function,
;;;`declp-buffer', `declp-region', `pr-declp-buffer', or `pr-declp-region'."
;;;  (let ((prompt "Number of pages per sheet of paper (`?' for help): ")
;;;        input)
;;;    (while (not (and (condition-case nil (setq input (read-minibuffer prompt))
;;;                       (error nil))     ; Read a non-Lisp expression.
;;;                     (numberp input)))  ; Read a Lisp sexp, but not a number.
;;;      (save-window-excursion (describe-function fn))) ; Defined in `help.el'.
;;;    (round input)))                     ; Convert floating point to integer.


(unless (fboundp 'pop-to-mark-command)
  (defun goto-previous-mark ()
    "Jump to previous mark, rotating the (local) `mark-ring'.
Does not affect the `global-mark-ring'.
This is equivalent to `set-mark-command' with a non-nil argument."
    (interactive) (set-mark-command t)))

;;;###autoload
(defun region-to-buffer (start end buffer arg)
  "Copy region to BUFFER: At beginning (prefix >= 0), end (< 0), or replace.
START and END are the region boundaries.
BUFFER is a buffer or its name (a string).
With prefix ARG >= 0: `append-to-buffer':
  Append contents of region to end of BUFFER.
  (Point is moved to end of BUFFER first.)
With prefix ARG < 0:  `prepend-to-buffer':
  Prepend contents of region to beginning of BUFFER.
  (Point is moved to beginning of BUFFER first.)
With no prefix ARG (nil): `copy-to-buffer'.
  Write region to BUFFER, replacing any previous contents."
  (interactive
   (let ((arg  (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
     (list (region-beginning)
           (region-end)
           (read-buffer (concat (if arg
                                    (if (natnump arg) "Append" "Prepend")
                                  "Write")
                                " region to buffer: ")
                        (if (fboundp 'another-buffer) ; Defined in `misc-fns.el'.
                            (another-buffer nil t)
                          (other-buffer (current-buffer))))
           arg)))
  (setq buffer  (get-buffer-create buffer)) ; Convert to buffer.
  (when (eq buffer (current-buffer)) (error "Cannot copy region to its own buffer"))
  (cond ((natnump arg)
         (with-current-buffer buffer (goto-char (point-max)))
         (append-to-buffer buffer start end))
        (arg
         (with-current-buffer buffer (goto-char (point-min)))
         (prepend-to-buffer buffer start end))
        (t (copy-to-buffer buffer start end))))

;;;###autoload
(defun region-to-file (start end filename arg)
  "With prefix arg, this is `append-to-file'.  Without, it is `write-region'.
START and END are the region boundaries.
Prefix ARG non-nil means append region to end of file FILENAME.
Prefix ARG nil means write region to FILENAME, replacing contents."
  (interactive
   (list (region-beginning)
         (region-end)
         (read-file-name (concat (if current-prefix-arg "Append" "Write")
                                 " region to file: "))
         current-prefix-arg))
  (let* ((curr-file    (buffer-file-name))
         (same-file-p  (and curr-file  (string= curr-file filename))))
    (cond ((or (not same-file-p)
               (progn (when (fboundp 'flash-ding) (flash-ding))
                      (yes-or-no-p
                       (format
                        "Do you really want to REPLACE the contents of `%s' by \
just the REGION? "
                        (file-name-nondirectory curr-file)))))
           (write-region start end filename arg)
           (when same-file-p (revert-buffer t t)))
          (t (message "OK.  Not written.")))))

;(defalias 'xwud 'display-xwd-image-file)
;;;;###autoload
;(defun display-xwd-image-file (xwd-file &optional options dir)
;  "Display an xwd image file XWD-FILE using the Unix `xwud' command.
;Arg XWD-FILE is a string naming the file, or else a list of such
;strings (non-interactively).

;If XWD-FILE is a list, then each of the files named in it is displayed
;in turn, a mouse click on an image causing it to be replaced by the
;next one.  In this case, relative file names are taken as relative to
;the directory DIR (the optional third arg), which defaults to the
;current `default-directory'.

;A non-nil prefix arg => You are prompted for `xwud' options.
;For a list of possible options, type \"-help\" as an option.
;For more information, type `\\[manual-entry] xwud'.

;Output from the `xwud' processes is put into buffer \"*XWD Display*\",
;but that buffer is not displayed."
;  (interactive "F*.xwd file to display: \nP")
;  (when (and options (not (stringp options)))
;    (setq options (read-from-minibuffer "`xwud' options: " nil nil nil
;                                        'minibuffer-history)))
;  (setq dir (or dir default-directory))
;  (if (listp xwd-file)
;      (dolist (file xwd-file)
;        (funcall 'display-xwd-image-file (expand-file-name file dir) options))
;    (let ((buf (get-buffer-create "*XWD Display*")))
;      (save-excursion (set-buffer buf) (erase-buffer))
;      (start-process-shell-command "xwud" buf "xwud"
;                                   (concat options " -in " xwd-file)))))

;;;; TO TEST:
;;;;(display-xwd-image-file
;;;;   (directory-files "~/ICONS" nil "drew-poster.+\.xwd$" t) nil "~/ICONS")

;(defalias 'xwd 'capture-image-as-xwd-file)
;;;;###autoload
;(defun capture-image-as-xwd-file (xwd-file &optional options)
;  "Capture an X window image as an *.xwd file via Unix `xwd' command.
;The \"-nobdrs\" `xwd' option is provided by default.
;A non-nil prefix arg => You are prompted for `xwd' options.
;For a list of options, type \"-help\" as an option.
;For more information, type `\\[manual-entry] xwud'."
;  (interactive "F*.xwd image file to create: \nP")
;  (if options
;      (unless (stringp options)
;        (setq options (read-from-minibuffer "`xwd' options: " " -nobdrs "
;                                            nil nil 'minibuffer-history)))
;    (setq options " -nobdrs "))
;  (message
;   "Click in X window you want to capture as image file `%s'." xwd-file)
;  (shell-command (concat "xwd " options " -out " xwd-file)))

(defun resolve-file-name (bounds &optional killp)
  "Replace the file name at/near point by its absolute, true file name.
If the region is active, replace its content instead, treating it as a
file name.

If library `thingatpt+.el' is available then use the file name
*nearest* point.  Otherwise, use the file name *at* point.

With a prefix arg, add both the original file name and the true name
to the kill ring.  Otherwise, add neither to the kill ring.  (If the
region was active then its content was already added to the ring.)"
  (interactive
   (let* ((regionp   (and transient-mark-mode mark-active))
          (thg+bnds  (and (not regionp)
                          (require 'thingatpt+ nil t)
                          (tap-define-aliases-wo-prefix) ; Defined in `thingatpt+.el'.
                          (tap-put-thing-at-point-props)
                          (thing-nearest-point-with-bounds 'filename)))
          (bnds      (if regionp
                         (cons (region-beginning) (region-end))
                       (if thg+bnds
                           (cdr thg+bnds)
                         (if (fboundp 'tap-bounds-of-thing-at-point)
                             (tap-bounds-of-thing-at-point 'filename)
                           (bounds-of-thing-at-point 'filename)))))
          (fname     (if bnds
                         (buffer-substring (car bnds) (cdr bnds))
                       (message "No file name at point"))))
     (list bnds current-prefix-arg)))
  (when bounds
    (let* ((file       (buffer-substring (car bounds) (cdr bounds)))
           (absfile    (expand-file-name (buffer-substring (car bounds) (cdr bounds))))
           (dir        (or (file-name-directory absfile) default-directory))
           (true-dir   (file-truename dir))
           (relfile    (file-name-nondirectory absfile))
           (true-file  (concat true-dir relfile)))
      (unless (equal file true-file)
        (cond (killp
               (if (and transient-mark-mode mark-active)
                   (delete-region (car bounds) (cdr bounds)) ; Don't add it twice.
                 (kill-region (car bounds) (cdr bounds)))
               (insert (kill-new true-file)))
              (t
               (delete-region (car bounds) (cdr bounds))
               (insert true-file)))))))

(defun read-shell-file-command (command)
  "Prompt for shell COMMAND, using current buffer's file as default arg.
If buffer is not associated with a file, you are prompted for a file.
COMMAND is a symbol."
  (let ((file (or (buffer-file-name) (read-file-name "File: "))))
    (setq file     (and file (file-name-nondirectory file))
          command  (format "%s  " command)) ; Convert to string.
    (read-from-minibuffer
     "" (cons (concat command (and file (concat " " file)))  (length command)))))

;;;###autoload
(defun chmod (cmd)
  "Execute Unix command `chmod'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chmod')."
  (interactive (list (read-shell-file-command 'chmod)))
  (shell-command cmd))

;;;###autoload
(defun chgrp (cmd)
  "Execute Unix command `chgrp'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chgrp')."
  (interactive (list (read-shell-file-command 'chgrp)))
  (shell-command cmd))

;;;###autoload
(defun chown (cmd)
  "Execute Unix command `chown'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chown')."
  (interactive (list (read-shell-file-command 'chown)))
  (shell-command cmd))


;;  ***** NOTE: The following EMACS PRIMITIVE has been REDEFINED HERE:
;;
;;  `display-buffer' - Raises frame too.

;(or (fboundp 'old-display-buffer)
;(fset 'old-display-buffer (symbol-function 'display-buffer)))

;;; REPLACES ORIGINAL (C source code?): Raises frame too.
;;;;###autoload
;(defun display-buffer (buffer &optional not-this-window)
;  "Make BUFFER appear in some window but don't select it.
;BUFFER can be a buffer or a buffer name.  Returns the window.

;If BUFFER is shown already in some window, just use that one,
;unless it is the selected window and the optional second arg
;NOT-THIS-WINDOW is non-nil (interactively, with prefix arg).
;Raises the frame in which buffer is already shown.

;If `pop-up-frames' is non-nil, make a new frame if no window
;shows BUFFER."
;  (interactive (list (read-buffer "Display buffer: " (other-buffer) 'existing)
;                     current-prefix-arg))
;  (let ((win (get-buffer-window buffer t)))
;    (if (or not-this-window (not win))
;        (old-display-buffer buffer not-this-window)
;      (raise-frame (window-frame win))
;      win)))                            ; Return the window.


;; Candidate as replacement for `kill-buffer', at least when used interactively.
;; Should not just redefine `kill-buffer', because some programs count on a
;; specific other buffer taking the place of the killed buffer (in the window).
;;;###autoload
(defun kill-buffer-and-its-windows (buffer)
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing)))
  (setq buffer  (get-buffer buffer))
  (if (buffer-live-p buffer)            ; Kill live buffer only.
      (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (and (buffer-modified-p buffer)
                   (fboundp '1on1-flash-ding-minibuffer-frame))
          (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
        (when (kill-buffer buffer)      ; Only delete windows if buffer killed.
          (dolist (win  wins)           ; (User might keep buffer if modified.)
            (when (window-live-p win) (delete-window win)))))
    (when (interactive-p)
      (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

;;; Like `clone-indirect-buffer' of Emacs 21.
;;;###autoload
(defun indirect-buffer ()
  "Edit stuff in this buffer in an indirect-buffer window.
The indirect buffer can have a different major mode from current."
  (interactive)
  (let ((buffer-name  (generate-new-buffer-name "*indirect*")))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))))

;;;###autoload
(defalias 'clear-search-ring 'clear-search-history)
;;;###autoload
(defun clear-search-history (&optional regexp-too-p)
  "Clear the search history (empty it).
With prefix arg, clear also the regular-expression search history."
  (interactive "P")
  (setq search-ring  ())
  (when regexp-too-p (setq regexp-search-ring nil)))

;;;###autoload
(defalias 'clear-regexp-search-ring 'clear-regexp-search-history)
;;;###autoload
(defun clear-regexp-search-history (&optional simple-too-p)
  "Clear the regular-expression search history (empty it).
With prefix arg, clear also the simple search history."
  (interactive "P")
  (setq regexp-search-ring ())
  (when simple-too-p (setq search-ring nil)))

;;;###autoload
(defun clear-search-histories ()
  "Clear both search histories: simple search and regexp search."
  (interactive)
  (setq regexp-search-ring ())
  (setq search-ring nil))

;;;###autoload
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

;;;###autoload
(defun view-X11-colors ()
  "View file `/usr/lib/X11/rgb.txt', which lists available X11 colors."
  (interactive) (view-file-other-window "/usr/lib/X11/rgb.txt")) ; In `view.el'.

;; Inspired from code at http://www.masteringemacs.org/.
(when (> emacs-major-version 23)        ; Needs variable `list-colors-sort'.
  (defun list-colors-nearest (color &optional use-hsv-p)
    "List colors, in order of distance from COLOR.
Use RGB distance by default.  Non-nil optional arg USE-HSV-P
\(interactively, the prefix arg) means use HSV distance instead of RGB
distance."
    (interactive (list (if (fboundp 'icicle-read-color-wysiwyg)
                           (icicle-read-color-wysiwyg 9999)
                         (read-color))
                       current-prefix-arg))
    (let ((list-colors-sort  (if (or use-hsv-p  current-prefix-arg)
                                 (cons 'hsv-dist color)
                               (cons 'rgb-dist color))))
      (if (color-defined-p color)
          (list-colors-display)
        (error "No such color: `%s'" color))))

  (defun list-colors-nearest-color-at (&optional position use-hsv-p)
    "List colors, in order of distance from color named at POSITION.
POSITION defaults to point.
Use RGB distance by default.  Non-nil optional arg USE-HSV-P
\(interactively, the prefix arg) means use HSV distance instead of RGB
distance."
    (interactive "d")
    (list-colors-nearest (color-named-at position) use-hsv-p))) ; In `misc-fns.el'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc-cmds.el ends here
