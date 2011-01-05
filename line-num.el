;;; line-num.el --- Display line numbers in left-margin of buffer.
;;
;; Filename: line-num.el
;; Description: Display line numbers in left-margin of buffer.
;; Author: (Darryl Okahata) darrylo@hpsrdmo, Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2011, Drew Adams, all rights reserved.
;; Copyright (C) 1989, Hewlett-Packard, all rights reserved.
;; Created: Wed Mar 31 16:18:24 1999
;; Version: 21.0
;; Last-Updated: Tue Jan  4 11:03:41 2011 (-0800)
;;           By: dradams
;;     Update #: 209
;; URL: http://www.emacswiki.org/cgi-bin/wiki/line-num.el
;; Keywords: local
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
;;    Display line numbers in left-margin of buffer.
;;
;; New functions defined here:
;;
;;    `display-line-numbers', `toggle-line-numbers-display',
;;    `turn-on-line-numbers-display', `turn-off-line-numbers-display'.
;;
;; NOTE: `setnu.el' now provides similar, but generally better,
;; functionality.
;;
;; Original author was Darryl Okahata darrylo@hpsrdmo: The copy on
;; which the current (Adams) modifications were made was obtained from
;; Rick Kunin (rickk@sperdk).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2004/11/26 dadams
;;     Replaced decf with setq...1+.
;;     turn-off-line-numbers-display: Error if not displaying line #s.
;;     Removed calls to `fit-frame' (and require of fit-frame.el).
;; 2000/11/01 dadams
;;     1. Added: toggle-line-numbers-display, turn-on-line-numbers-display,
;;               turn-off-line-numbers-display.
;;     2. Added global vars: displaying-line-numbers-p,
;;        display-line-numbers-format-string, display-line-numbers-first-line,
;;        display-line-numbers-count, display-line-numbers-buffer-name,
;;        display-line-numbers-modified-p.
;; 1999/04/14 dadams
;;     Commented out assignment to unused free var: insert-end.
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

;;>> Problem:  Tabs at beginning of lines


(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless

;;;;;;;;;;;;;


;;;###autoload
(defun display-line-numbers ()
  "Temporarily display line numbers in left margin of current buffer."
  (interactive)
  (or (eq (current-buffer) (window-buffer (selected-window)))
      (error "Current buffer, %s, is not the selected window's buffer"
             (buffer-name)))
  (let ((buffer-read-only nil)
        (modified (buffer-modified-p))
        (name buffer-file-name)
        (point (point-marker))
        format-string
        line-number
        (count 0)
        nlines
        first-line)
    (save-restriction
      (widen)
      (save-excursion
        (setq first-line (window-start (selected-window)))
        (goto-char first-line)
        (setq line-number (1+ (count-lines (point-min) (point))))
        (move-to-window-line -1)
        (beginning-of-line)
        (setq nlines (count-lines first-line (point)))
        (let ((max (+ line-number nlines)))
          (setq format-string (cond ((< max 100) "%2d ")
                                    ((< max 1000) "%3d ")
                                    ((< max 10000) "%4d ")
                                    (t "%7d "))))))
    (save-excursion
      (unwind-protect
          (progn
            (goto-char first-line)
            ;; defeat file locking... don't try this at home, kids!
            (setq buffer-file-name nil)
            (while (<= count nlines)
              (insert-before-markers (format format-string line-number))
              ;;;(setq insert-end (point)) THIS VAR IS FREE - AND UNUSED!
              (setq line-number (1+ line-number))
              (setq count (1+ count))
              (forward-line 1))
            (set-window-start (selected-window) first-line)
            (goto-char point)
            (set-buffer-modified-p modified)
            (message "<<< Press SPACE to continue >>>")
            (let ((char (read-char)))
              (or (eql char ?\  )
                  (setq unread-command-events (list char))))
            )
        (goto-char first-line)
        (let ((n (1+ (- (aref format-string 1) ?0))))
          (while (> count 0)
            (setq count (1- count))
            (delete-char n)
            (forward-line 1)))
        (setq buffer-file-name name)
        (set-buffer-modified-p modified)))))

;;;-----------------------------------------------------------------

(defvar displaying-line-numbers-p nil)
(make-variable-buffer-local 'displaying-line-numbers-p)
(defvar display-line-numbers-format-string nil)
(make-variable-buffer-local 'display-line-numbers-format-string)
(defvar display-line-numbers-first-line nil)
(make-variable-buffer-local 'display-line-numbers-first-line)
(defvar display-line-numbers-count 0)
(make-variable-buffer-local 'display-line-numbers-count)
(defvar display-line-numbers-buffer-name nil)
(make-variable-buffer-local 'display-line-numbers-buffer-name)
(defvar display-line-numbers-modified-p nil)
(make-variable-buffer-local 'display-line-numbers-modified-p)

;;;###autoload
(defun toggle-line-numbers-display (arg)
  "Display/clear line numbers in left margin of current buffer.
With prefix ARG, just number lines in current window, not all lines in
buffer."
  (interactive "P")
  (if displaying-line-numbers-p
      (turn-off-line-numbers-display)
    (turn-on-line-numbers-display arg)))

;;;###autoload
(defun turn-on-line-numbers-display (arg)
  "Display line numbers in left margin of current buffer.
With prefix ARG, just number lines in current window, not all lines in
buffer."
  (interactive "P")
  (or (eq (current-buffer) (window-buffer (selected-window)))
      (error "Current buffer, %s, is not the selected window's buffer"
             (buffer-name)))
  (let ((buffer-read-only nil)
        (point (point-marker))
        line-number
        nlines)
    (setq display-line-numbers-buffer-name buffer-file-name)
    (setq display-line-numbers-modified-p (buffer-modified-p))
    (save-restriction
      (widen)
      (save-excursion
        (setq display-line-numbers-first-line
              (if arg
                  (window-start (selected-window))
                (point-min)))
        (goto-char display-line-numbers-first-line)
        (setq line-number (1+ (count-lines (point-min) (point))))
        (if arg
            (move-to-window-line -1)
          (goto-char (point-max)))
        (beginning-of-line)
        (setq nlines (count-lines display-line-numbers-first-line (point)))
        (let ((max (+ line-number nlines)))
          (setq display-line-numbers-format-string (cond ((< max 100) "%2d ")
                                    ((< max 1000) "%3d ")
                                    ((< max 10000) "%4d ")
                                    (t "%7d "))))))
    (save-excursion
      (condition-case nil
          (progn
            (goto-char display-line-numbers-first-line)
            (setq buffer-file-name nil) ; To prevent saving with line numbers etc.
            (setq displaying-line-numbers-p t)
            (while (<= display-line-numbers-count nlines)
              (insert-before-markers
               (format display-line-numbers-format-string line-number))
              (setq line-number (1+ line-number))
              (setq display-line-numbers-count (1+ display-line-numbers-count))
              (forward-line 1))
            (when arg
              (set-window-start (selected-window) display-line-numbers-first-line))
            (goto-char point)
            (set-buffer-modified-p display-line-numbers-modified-p))
    (error
     (progn
           (goto-char display-line-numbers-first-line)
           (let ((n (1+ (- (aref display-line-numbers-format-string 1) ?0))))
             (while (> display-line-numbers-count 0)
               (setq display-line-numbers-count (1- display-line-numbers-count))
               (delete-char n)
               (forward-line 1)))
           (setq buffer-file-name display-line-numbers-buffer-name)
           (set-buffer-modified-p display-line-numbers-modified-p)
           (setq displaying-line-numbers-p nil))))))
  (let ((curr-line (count-lines (window-start) (point))))
    (when (> curr-line 0) (setq curr-line (1+ curr-line)))
    (recenter curr-line)))

;;;###autoload
(defun turn-off-line-numbers-display ()
  "Clear displayed line numbers from left margin of current buffer."
  (interactive)
  (unless (eq (current-buffer) (window-buffer (selected-window)))
    (error "Current buffer, `%s', is not the selected window's buffer"
             (buffer-name)))
  (unless displaying-line-numbers-p
    (error "Not displaying line numbers in buffer `%s'" (buffer-name)))
  (let ((buffer-read-only nil))
    (save-excursion
      (when (boundp 'display-line-numbers-buffer-name)
        (setq buffer-file-name display-line-numbers-buffer-name))
      (goto-char display-line-numbers-first-line)
      (let ((n (1+ (- (aref display-line-numbers-format-string 1) ?0))))
        (while (> display-line-numbers-count 0)
          (setq display-line-numbers-count (1- display-line-numbers-count))
          (delete-char n)
          (forward-line 1)))
      (when (boundp 'display-line-numbers-modified-p)
        (set-buffer-modified-p display-line-numbers-modified-p))
      (setq displaying-line-numbers-p nil))))


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'line-num)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; line-num.el ends here
