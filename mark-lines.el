;;; mark-lines.el --- triple-click like line marking functions

;; Copyright (C) 2003 Le Wang

;; Author: Le wang (remove numbers) <lewang097 AT yahoo DOT com>
;; Version: 0.5
;; Keywords: convenience whole-line region editing
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?MarkLines

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author or from the Free Software Foundation, Inc., 675 Mass Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:

;; `mark-lines' provides whole line selection functionality.  The behaviour of
;; triple-clicking mouse-1 is used as a model.

;; In (GNU |X)Emacs, when you triple click on a line, that line is
;; automatically selected, and you go into a mode of region selection where
;; lines are added to the region when you move the point up or down the buffer
;; by dragging the mouse.  The original line always remains selected, and new
;; lines are added to the region based on the vertical position of point
;; relative to the original line.

;; This package seeks to duplicate that behaviour with the keyboard, with some
;; improvements.  See usage for details.

;;; Installation:

;; 1. Add this file to a directory in your `load-path'
;;
;; 2. Add (require 'mark-lines) to your ~/.emacs
;;
;; 3. Add your desired key-bindings to your ~/.emacs, e.g.:
;;
;;     (global-set-key [(control x) (control p)] 'mark-lines-previous-line)
;;     (global-set-key [(control x) (control n)] 'mark-lines-next-line)

;;; Usage:

;; *** terminology ***

;;            "entry command" => `mark-lines-next-line'     or
;;                               `mark-lines-previous-line'

;;    "line movement command" => `mark-lines-next-line'     or
;;                               `mark-lines-previous-line' or
;;                               `next-line'                or
;;                               `previous-line'

;; *** activation ***

;; Line marking is NOT implemented as a minor mode.  You have to activate it
;; by invoking one of the "entry commands".

;; Once active, the current region is expanded to consist of only whole lines
;; by the invoking any of the "line movement commands".

;; `mark-lines-next-line' and `mark-lines-previous-line' expand the region by
;; moving the mark; the line at point will always stay in the region.
;; `next-line' and `previous-line' expand the region by moving the point;
;; the line at mark will always stay in the region.  THIS IS THE TRIPLE-CLICK
;; BEHAVIOUR.

;; *** deactivation ***

;; `mark-lines' can be deactivated without aborting the current region by
;; prefixing any "entry command" with the `universal-argument' (bound to <C-u>
;; by default).

;; If `mark-lines-electric' is non-nil (this is the default) the point will be
;; moved to a more interesting position on the line once the region
;; deactivates, since the beginning is usually not very interesting.

;; `mark-lines' is automatically deactivated when the region deactivates.


;;; Compatibility:

;; Tested with XEmacs 21.4.12, and GNU Emacs 21.2.1

;;; Caveat:

;;
;; * incompatible with `cua-mode'
;;

;;; Known bugs:

;;; To do:

;;; ChangeLog

;; 0.5 Mon 17 Feb 2003
;;      Fixed horrible border-line case bug. (when the point is at eob, on a
;;      non-empty line).

;; 0.4 Sat 15 Feb 2003
;;      Changed copyright to me.  ( I had originally pasted in the license
;;      w/o checking.)
;;      More documentation fixes.

;; 0.3 Sat 15 Feb 2003
;;      Documentation updates.

;; 0.2 Sat Feb 15, 2003
;;      Added changelog.
;;      Fixed column tracking bug with goal-column.
;;      Small documentation fixes.

;; 0.1 Sat Feb 15, 2003
;;      Initial release.

;;; Code:

(provide 'mark-lines)

;; customization

(defgroup mark-lines nil
  "Whole line marking."
  :prefix "mark-lines"
  :group 'editing
  :group 'convenience)

(defcustom mark-lines-verbose nil
  "*Output status messages to minibuffer if t."
  :type 'boolean
  :group 'mark-lines)

(defcustom mark-lines-electric t
  "*Non-nill means when the region deactivates, try to point to a place of
  interest."
  :type 'boolean
  :group 'mark-lines)

(defvar mark-lines nil
  "is t when a mark-line is active")

;; compatibility

(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    "Xemacs compatibility."
    mark-active))

(defmacro zmacs-region-stay-please ()
  "make region stay in XEmacs."
  (when (boundp 'zmacs-region-stays)
    '(setq zmacs-region-stays t)))

;; setup

(add-hook (eval-and-compile
            (if (boundp 'zmacs-deactivate-region-hook)
                'zmacs-deactivate-region-hook
              'deactivate-mark-hook))
          'mark-lines-deactivate-hook)

(defadvice next-line (around mark-lines activate)
  "support whole line marking."
  (zmacs-region-stay-please)
  (if mark-lines
      (let ((goal-column 0)
             old-point
             mark-is-beginning)
        (unless (mark-lines-setup-region)
          (setq old-point (point)
                mark-is-beginning (eq (region-beginning) (mark)))
          ad-do-it
              (if mark-is-beginning
                  (when (> (mark) (point))
                    (set-mark (save-excursion
                                (goto-char (mark))
                                (point-at-bol 2))))
                (when (<= (mark) (point))
                  (when (= (mark) (point))
                    (forward-line 1))
                  (set-mark (save-excursion
                              (goto-char (mark))
                              (point-at-bol 0)))))))
    ad-do-it))

(defadvice previous-line (around mark-lines activate)
  "support whole line marking."
  (zmacs-region-stay-please)
  (if mark-lines
      (let ((goal-column 0)
             old-point
             old-point-is-beginning)
        (unless (mark-lines-setup-region)
          (setq old-point (point)
                old-point-is-beginning (eq (region-beginning) old-point))
          ad-do-it
              (if old-point-is-beginning
                  (when (> (point) (mark))
                    (set-mark (save-excursion
                                (goto-char (mark))
                                (point-at-bol 0))))
                (when (<= (point) (mark))
                  (when (= (point) (mark))
                    (forward-line -1))
                  (set-mark (save-excursion
                              (goto-char (mark))
                              (point-at-bol 2)))))))
    ad-do-it))

;; functions

(defun mark-lines-deactivate ()
  "deactivate mark-lines"
  (setq mark-lines nil)
  (when mark-lines-verbose
    (message "mark-lines stopped.")))

(defun mark-lines-deactivate-hook ()
  "function run from deactivate region hook."
  (when (and mark-lines-electric
             mark-lines
             (bolp))
    (skip-chars-forward " \t")
    (when (or (memq 'font-lock-comment-face
                    (text-properties-at (point)))
              (and comment-start
                   (looking-at (regexp-quote comment-start))))
      (when (looking-at comment-start-skip)
        (goto-char (match-end 0)))))
  (mark-lines-deactivate))

(defun mark-lines-activate ()
  "deactivate `mark-lines'"
  (setq mark-lines t)
  (when mark-lines-verbose
    (message "mark-lines started.")))


(defun mark-lines-setup-region ()
  "set up the region to ge whole lines only.

The mark and point are modified.  The region is expanded in such a way that
the displacements of mark and point are minimal.

Return t if point or mark had to be moved.

BUG: goto the end of a buffer that's not an empty line, try <C-x> <C-p> then,
<C-p>, it crashes."
  (when (region-active-p)
    (let ((p (point))
          result)
      (if (< p (mark))
          (progn
            (unless (or (bolp) (eobp))
              (forward-line 0)
              ;; we don't need this, since previous/next line funciton
              ;; obliterates point any how
;;              (setq p (point))
              (setq result t))
            (goto-char (mark))
            (unless (or (bolp) (eobp))
              (set-mark (point-at-bol))
              (setq result t))
            (goto-char p)) ; but we do need to go back to the old line
        (goto-char (mark))
        (unless (or (bolp) (eobp))
          (set-mark (point-at-bol))
          (setq result t))
        (goto-char p)
        (unless (or (bolp) (eobp))
          (forward-line 1)
          (setq result t)))
      result)))

(defun mark-lines--move (movement-command arg)
  "internal function.

see \\[mark-lines-previous-line] and \\[mark-lines-next-line]."
  (let ((was-marking-lines mark-lines))
    (cond ((and arg
                (listp arg))
           (when mark-lines
             (mark-lines-deactivate)))
          ((not arg)
           (setq arg 1)
           (when (not mark-lines)
             (mark-lines-activate)))
          ((when (not mark-lines)
             (mark-lines-activate))))
    (when mark-lines
      (if (region-active-p)
          (if (not was-marking-lines)
              (funcall movement-command arg)
            (exchange-point-and-mark)
            (funcall movement-command arg)
            (exchange-point-and-mark))
        (if (and (eq movement-command 'previous-line)
                 (= 1 (forward-line 1)))
            (push-mark (point-at-eol))
          (push-mark (point-at-bol) t t))
        (funcall movement-command arg)))))


;;;###autoload
(defun mark-lines-previous-line (arg)
  "mark the current line and move up ARG line.

If the region is already active, and it's a whole-lines region, move mark up ARG
lines.

If the region is active, but not a whole-lines region, expand it so that it
becomes a whole region.

If the universal argument is received, turn mark-lines off for the currently
active region.

Otherwose arg is treated exactly as `previous-line'."
  (interactive "P")
  (mark-lines--move 'previous-line arg))


;;;###autoload
(defun mark-lines-next-line (arg)
  "same as `mark-lines-previous-line', going in the other direction."
  (interactive "P")
  (mark-lines--move 'next-line arg))


;;; mark-lines.el ends here
