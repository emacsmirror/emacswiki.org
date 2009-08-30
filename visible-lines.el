;;; visible-lines.el --- move across visible lines instead of logic lines

;; Authors:    an00na@gmail.com
;; Maintainer: an00na@gmail.com
;; Keywords: convenience, continuation
;; Last updated: Jan 27, 2008

;; Copyright (C) Ling Wang

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:

;; Long lines can be wrapped for nice visual effect in Emacs. However
;; the line-oriented movements are still done on logic lines, which is
;; not so convenient or even anti-intuitive sometimes. For example,
;; imagine you were on the first visible line of a wrapped very long
;; line(say, wrapped to totally 10 visible lines) and wanted to move to
;; the 5th visible line. How would you do it? It turns out that no
;; matter you traveled from the very beginning or the very end(you could
;; move-end-of-line first), it was a suffering. Long lines mode which is
;; included as a standard library solve most of the long lines problem;
;; but it only provides movements across those space-delimited
;; continuation lines, which is unreasonable to people whose language
;; does not use space characters to delimit words or even sentences, for
;; example, we Chinese. Visible line mode provides means to do more
;; general visible-line-oriented movements, irrespective of where the
;; lines are wrapped.

;; To Chinese users:
;; visible lines mode 支持由字符宽度不等的多种字符集组成的混合文本，可以正确处
;; 理中英文混合编辑。

;;; Change Log:

;; 2007-06-13 add visible-lines-kill-line.
;; 2008-01-27 add more optional key bindings suggested by Greg Reagle <greagle@citidc.com>

;;; Key bindings:

;; C-n  visible-lines-next-line
;; C-p  visible-lines-previous-line
;; C-a  visible-lines-move-beginning-of-line
;; C-e  visible-lines-move-end-of-line
;; C-k  visible-lines-kill-line       
;; Refer to the code for more

;;; Install:

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;       (require 'visible-lines)

;;; Code:
(defvar visible-lines-goal-column nil
  "Semipermanent goal column for vertical motion.")
(make-variable-buffer-local 'visible-lines-goal-column)

(defvar visible-lines-mode-map nil
  "Local keymap for visible line mode.")

;; set up the keymap
(when (null visible-lines-mode-map)
  (setq visible-lines-mode-map (make-sparse-keymap))
  (define-key visible-lines-mode-map "\C-n" 'visible-lines-next-line)
  (define-key visible-lines-mode-map "\C-p" 'visible-lines-previous-line)
  (define-key visible-lines-mode-map "\C-a" 'visible-lines-move-beginning-of-line)
  (define-key visible-lines-mode-map "\C-e" 'visible-lines-move-end-of-line)
  (define-key visible-lines-mode-map "\C-k" 'visible-lines-kill-line)
  ;; uncomment the code below to rebind all the line-oriented movement key
  ;; chords to visible-lines-* functions
;;;   (define-key visible-lines-mode-map [remap next-line]
;;;     'visible-lines-next-line)
;;;   (define-key visible-lines-mode-map [remap previous-line]
;;;     'visible-lines-previous-line)
;;;   (define-key visible-lines-mode-map [remap move-beginning-of-line]
;;;     'visible-lines-move-beginning-of-line)
;;;   (define-key visible-lines-mode-map [remap move-end-of-line]
;;;     'visible-lines-move-end-of-line)
;;;   (define-key visible-lines-mode-map [remap kill-line]
;;;     'visible-lines-kill-line)
  )

(defun visible-lines-beginning-column (&optional n)
  "Return the column of the first character on the current visible line.
With argument n not nil or 1, move forward n - 1 visible lines
first. If scan reaches end of buffer, return that column.

This function does not move point."
  (if (not n)
      (setq n 1))
  (save-excursion
    (visible-lines-beginning-of-line n)
    (current-column)))

(defun visible-lines-end-column (&optional n)
  "Return the column of the last character on the current visible line.
With argument n not nil or 1, move forward n - 1 lines visible
first. If scan reaches end of buffer, return that column.

This function does not move point."
  (if (not n)
      (setq n 1))
  (save-excursion
    (visible-lines-end-of-line n)
    (current-column)))

(defun visible-lines-next-line (&optional line)
  "next-line over visible lines"
  (interactive "p")
  (visible-lines-forward-line line)
  (move-to-column
   (+ (current-column)
      (min (- (visible-lines-end-column) (current-column)) visible-lines-goal-column))))


(defun visible-lines-previous-line (&optional line)
  "previous-line over visible lines"
  (interactive "p")
  (visible-lines-next-line (- line)))

(defun visible-lines-move-beginning-of-line (&optional line)
  "Move point to beginning of current visible line.
With argument line not nil or 1, move forward arg - 1 lines
first. If point reaches the beginning or end of buffer, it stops
there."
  (interactive "p")
  (visible-lines-beginning-of-line line))

(defun visible-lines-beginning-of-line (&optional line)
  "Move point to beginning of current visible line.
With argument line not nil or 1, move forward arg - 1 lines
first. If point reaches the beginning or end of buffer, it stops
there."
  (if (not line)
      (setq line 1))
  (vertical-motion (1- line)))

(defun visible-lines-move-end-of-line (&optional line)
  "Move point to end of current visible line.
 With argument line not nil or 1, move forward arg - 1 lines
first. If point reaches the beginning or end of buffer, it stops
there."
  (interactive "p")
  (visible-lines-end-of-line line))

(defun visible-lines-end-of-line (&optional line)
  "Move point to end of current visible line.
 With argument line not nil or 1, move forward arg - 1 lines
first. If point reaches the beginning or end of buffer, it stops
there."
  (if (not line)
      (setq line 1))
  (if (= (vertical-motion line) line)
      (forward-char -1)))

(defun visible-lines-forward-line (arg)
  "Move forward by ARG visible lines.
If ARG is negative, move backward -ARG visible lines. If ARG is
zero, move to the beginning of the current visible line."
  (condition-case nil
      (progn   
        (if (not (member last-command '(visible-lines-next-line visible-lines-previous-line)))
            (setq visible-lines-goal-column
                  (- (current-column)
                     (visible-lines-beginning-column))))
        (vertical-motion arg))      
    ((beginning-of-buffer end-of-buffer)
     nil)))

(defun bovlp ()
  "Tell whether the point is at visible-lines-beginning-of-line."
  (let ((current-point (point)))
    (save-excursion
      (visible-lines-beginning-of-line)
      (= current-point (point)))))

(defun eovlp ()
  "Tell whether the point is at visible-lines-end-of-line."
  (let ((current-point (point)))
    (save-excursion
      (visible-lines-end-of-line)
      (= current-point (point)))))

(defun visible-lines-kill-line (&optional arg)
  "Kill the rest of the current visible line.
With prefix argument, kill that many visible lines from point.
Negative arguments kill visible lines backward. With zero
argument, kills the text before point on the current visible
line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

To kill a whole visible line, when point is not at the beginning,
type \ \\[visible-lines-move-beginning-of-line] \\[visible-lines-kill-line].

If you want to append the killed visible line to the last killed
text, use \\[append-next-kill] before \\[visible-lines-kill-line].

If the buffer is read-only, Emacs will beep and refrain from
deleting the visible line, but put the visible line in the kill
ring anyway. This means that you can use this command to copy
text from a read-only buffer. \(If the variable
`kill-read-only-ok' is non-nil, then this won't even beep.)"
  (interactive "P")
  (kill-region (point)
               ;; It is better to move point to the other end of the kill
               ;; before killing.  That way, in a read-only buffer, point
               ;; moves across the text that is copied to the kill ring.
               ;; The choice has no effect on undo now that undo records
               ;; the value of point from before the command was run.
               (progn
                 (if arg
                     (visible-lines-forward-line (prefix-numeric-value arg))
                   (if (eobp)
                       (signal 'end-of-buffer nil))
                   (let ((end
                          (save-excursion
                            (visible-lines-end-of-line) (point))))
                     (goto-char end)
                     (or (eobp)
                         (forward-char 1))))
                 (point))))

(define-minor-mode visible-lines-mode
  "Toggle Visible Line mode.
In Visible Lines mode, continuation lines are treated as separate lines in 
process of line-oriented movements and editing:
        `visible-lines-next-line'
        `visible-lines-previous-line'
        `visible-lines-beginning-of-line'
        `visible-lines-end-of-line'
        `visible-lines-kill-line'"
  :lighter " Visible-Lines" :keymap visible-lines-mode-map)

(provide 'visible-lines)
;;; visible-lines.el ends here

