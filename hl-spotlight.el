;;; hl-spotlight.el --- Extension of hl-line.el to spotlight current few lines.
;; 
;; Filename: hl-spotlight.el
;; Description: Extension of hl-line.el to spotlight current few lines.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2009-2012, Drew Adams, all rights reserved.
;; Created: Sat Aug 26 18:17:18 2006
;; Version: 22.0
;; Last-Updated: Sun Jan  1 14:37:16 2012 (-0800)
;;           By: dradams
;;     Update #: 458
;; URL: http://www.emacswiki.org/cgi-bin/wiki/hl-spotlight.el
;; Keywords: highlight, cursor, accessibility
;; Compatibility: GNU Emacs: 22.x, 23.x
;; 
;; Features that might be required by this library:
;;
;;   `hl-line'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;;  This library extends standard library `hl-line.el'.  It provides
;;  local and global modes to highlight several lines surrounding
;;  point using a different face, `hl-spotlight'.  You can enlarge or
;;  shrink this spotlight highlighting, using command
;;  `hl-spotlight-enlarge'.  You can repeat, to enlarge or shrink the
;;  spotlight incrementally, by using `C-x z z z z...'.
;;
;;  Spotlight highlighting can be used together with library
;;  `centered-cursor-mode.el', which keeps point (hence also the
;;  spotlight) centered in the window.  This can be helpful when
;;  reading text (as opposed to code).  This is controlled by user
;;  option `hl-spotlight-keep-centered-flag'. You can obtain library
;;  `centered-cursor-mode.el' here:
;;  http://www.emacswiki.org/emacs/centered-cursor-mode.el.
;;
;;  If you want the spotlight to automatically move down the page
;;  progressively, use command `hl-spotlight-scan'.  You might find
;;  this useful for some kinds of reading.  Or not.  ;-)
;;
;;  To use this library, put this in your Emacs init file (~/.emacs):
;;
;;    (require 'hl-spotlight) ; Load this file (it will load `hl-line.el')
;;
;;
;;  See also library `hl-line+.el', which extends `hl-line.el' in
;;  other ways.
;;
;;
;;  Faces defined here:
;;
;;    `hl-spotlight'.
;;
;;  User options defined here:
;;
;;    `hl-spotlight-height', `hl-spotlight-keep-centered-flag',
;;    `hl-spotlight-scan-period'.
;;
;;  Commands defined here:
;;
;;    `global-hl-spotlight-mode', `hl-spotlight-enlarge',
;;    `hl-spotlight-mode', `hl-spotlight-scan'.
;;
;;  Non-interactive functions defined here:
;;
;;    `hl-spotlight-down', `hl-spotlight-limits'.
;;
;;  Internal variables defined here:
;;
;;    `hl-spotlight-old-state', `hl-spotlight-scan-timer'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2011/01/04 dadams
;;     Added autoload cookies for defcustom, defface, and commands.
;; 2009/02/16 dadams
;;     Added: hl-spotlight-scan*, hl-spotlight-down.
;;     Created: Moved code from hl-line+.el.
;;
;; Log from hl-line+.el:
;;
;; 2009/02/15 dadams
;;     Added: hl-spotlight(-height|-old-state|widen|limits|mode),
;;            global-hl-spotlight-mode.
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

(require 'hl-line) ;; global-hl-line-highlight, global-hl-line-mode,
                   ;; global-hl-line-overlay, global-hl-line-unhighlight,
                   ;; hl-line-face, hl-line-highlight, hl-line-mode,
                   ;; hl-line-overlay, hl-line-range-function, hl-line-sticky-flag,
                   ;; hl-line-unhighlight.

(eval-when-compile
  (require 'centered-cursor-mode nil t)) ;; (no error if not found):
                                         ;; centered-cursor-mode,
                                         ;; global-centered-cursor-mode.

(defvar hl-line-face)                   ; Quiet the byte-compiler.
(defvar global-hl-line-mode)            ; Quiet the byte-compiler.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defface hl-spotlight
  '((t :inherit highlight))
  "*Face for the spotlight in Hl-Line-Window mode."
  :group 'hl-line)

;;;###autoload
(defcustom hl-spotlight-height 2
  "*Number of lines to highlight, before and after the current line."
  :type 'integer :group 'hl-line)

;;;###autoload
(defcustom hl-spotlight-keep-centered-flag t
  "*Non-nil means keep the cursor and spotlight centered in the window.
This has no effect unless library `centered-cursor-mode' is available."
  :type 'boolean :group 'hl-line)

;;;###autoload
(defcustom hl-spotlight-scan-period 1.5
  "*Number of seconds to wait before moving cursor to next line.
Set this to nil if you do not want the cursor to automatically scan."
  :type '(choice
          (const :tag "No cursor scanning" nil)
          (number :tag "Seconds before cursor moves to next line"))
  :group 'hl-line)

(defvar hl-spotlight-old-state nil
  "Saved Hl-Line mode values, before `hl-spotlight-mode'.")

(defvar hl-spotlight-scan-timer (timer-create)
  "Timer used to move point downward.")

;;;###autoload
(defun hl-spotlight-enlarge (n)
  "Enlarge the hl-line spotlight by N lines.
N is the numeric prefix arg (one, by default).
A negative prefix arg shrinks the spotlight.
The spotlight is used by `hl-spotlight-mode' and
`global-hl-spotlight-mode'."
  (interactive "p")
  (set-variable 'hl-spotlight-height (+ n hl-spotlight-height))
  (when global-hl-spotlight-mode (global-hl-line-highlight))
  (when hl-spotlight-mode (hl-line-highlight)))

(defun hl-spotlight-limits ()
  "Return a cons of the limits to use for `hl-line-range-function'."
  (let ((start  (save-excursion (forward-line (- hl-spotlight-height)) (point)))
        (end    (save-excursion (forward-line (1+ hl-spotlight-height)) (point))))
    (cons start end)))

;;;###autoload
(define-minor-mode hl-spotlight-mode
    "Buffer-local minor mode to highlight lines surrounding point.
With ARG, turn Hl-Spotlight mode on if ARG is positive, off otherwise.

Hl-Spotlight mode uses Hl-Line mode.  Whenever Hl-Spotlight mode is on
in the current buffer, its overlay is used by Hl-Line mode, which
means that face `hl-spotlight' and option `hl-spotlight-height' are
used; face `hl-line' is not used.

Turn the spotlight on and off by using toggle command
`hl-spotlight-mode'.  After turning Hl-Spotlight mode on, command
`hl-line-mode' also toggles the spotlight on and off, but without
turning off Hl-Spotlight mode.  To return to the normal behavior of
`hl-line-mode', you must turn off Hl-Spotlight mode.  Turning off
Hl-Spotlight mode also turns off Hl-Line mode."
  :group 'hl-line
  (cond (hl-spotlight-mode
         (unless hl-spotlight-old-state
           (setq hl-spotlight-old-state  (list hl-line-face
                                               hl-line-overlay
                                               global-hl-line-overlay
                                               hl-line-range-function
                                               hl-line-sticky-flag)))
         (hl-line-unhighlight)
         (setq hl-line-overlay         nil
               hl-line-face            'hl-spotlight
               hl-line-range-function  'hl-spotlight-limits
               hl-line-sticky-flag     nil)
         (when (and (require 'centered-cursor-mode nil t)
                    hl-spotlight-keep-centered-flag)
           (centered-cursor-mode 1))
         ;; In case `kill-all-local-variables' is called.
         (add-hook 'change-major-mode-hook #'hl-line-unhighlight nil t)
         (if hl-line-sticky-flag
             (remove-hook 'pre-command-hook #'hl-line-unhighlight t)
           (add-hook 'pre-command-hook #'hl-line-unhighlight nil t))
         (add-hook 'post-command-hook #'hl-line-highlight nil t))
        (t
         (cancel-timer hl-spotlight-scan-timer)
         (setq hl-line-face             (nth 0 hl-spotlight-old-state)
               hl-line-overlay          (nth 1 hl-spotlight-old-state)
               global-hl-line-overlay   (nth 2 hl-spotlight-old-state)
               hl-line-range-function   (nth 3 hl-spotlight-old-state)
               hl-line-sticky-flag      (nth 4 hl-spotlight-old-state))
         (when hl-spotlight-old-state (setq hl-spotlight-old-state  nil))
         (when (require 'centered-cursor-mode nil t) (centered-cursor-mode -1))
         (remove-hook 'post-command-hook #'hl-line-highlight t)
         (hl-line-unhighlight)
         (remove-hook 'change-major-mode-hook #'hl-line-unhighlight t)
         (remove-hook 'pre-command-hook #'hl-line-unhighlight t)))
  (hl-line-mode (if hl-spotlight-mode 1 -1)))

;;;###autoload
(define-minor-mode global-hl-spotlight-mode
    "Global minor mode to highlight lines around point in current window.
With ARG, turn Global-Hl-Spotlight mode on if ARG is positive, off
otherwise.

See `hl-spotlight-mode'.  The interaction between
`global-hl-spotlight-mode' and `global-hl-line-mode' is similar to
that between `hl-spotlight-mode' and `hl-line-mode'."
  :global t :group 'hl-line
  (cond (global-hl-spotlight-mode
         (unless hl-spotlight-old-state
           (setq hl-spotlight-old-state  (list hl-line-face
                                               hl-line-overlay
                                               global-hl-line-overlay
                                               hl-line-range-function
                                               hl-line-sticky-flag)))
         (global-hl-line-unhighlight)
         (setq global-hl-line-overlay  nil
               hl-line-face            'hl-spotlight
               hl-line-range-function  'hl-spotlight-limits
               hl-line-sticky-flag     nil)
         (when (and (require 'centered-cursor-mode nil t)
                    hl-spotlight-keep-centered-flag)
           (global-centered-cursor-mode 1))
         (add-hook 'pre-command-hook #'global-hl-line-unhighlight)
         (add-hook 'post-command-hook #'global-hl-line-highlight))
        (t
         (setq hl-line-face            (nth 0 hl-spotlight-old-state)
               hl-line-overlay         (nth 1 hl-spotlight-old-state)
               global-hl-line-overlay  (nth 2 hl-spotlight-old-state)
               hl-line-range-function  (nth 3 hl-spotlight-old-state)
               hl-line-sticky-flag     (nth 4 hl-spotlight-old-state))
         (when hl-spotlight-old-state (setq hl-spotlight-old-state  nil))
         (when (require 'centered-cursor-mode nil t)
           (global-centered-cursor-mode -1))
         (global-hl-line-unhighlight)
         (remove-hook 'pre-command-hook #'global-hl-line-unhighlight)
         (remove-hook 'post-command-hook #'global-hl-line-highlight)))
  (global-hl-line-mode (if global-hl-spotlight-mode 1 -1)))

;;;###autoload
(defun hl-spotlight-scan (arg)
  "Scan the buffer, moving the cursor down automatically.
Every `hl-spotlight-scan-period' seconds, move the cursor down one
line or the number of lines specified by a prefix arg.  Scanning
starts at point.

With a plain prefix arg (`C-u'), stop a scan already in progess.

With a numeric prefix arg, scan down that many lines.
A negative prefix arg means scan up, not down.
With `C-u C-u', scan down the height of a full spotlight."
  (interactive "P")
  (if (and (consp arg) (= (car arg) 4))
      (cancel-timer hl-spotlight-scan-timer)
    (when (consp arg) (setq arg  (1+ (* 2 hl-spotlight-height))))
    (setq arg  (prefix-numeric-value arg))
    (setq hl-spotlight-scan-timer (run-at-time 0 hl-spotlight-scan-period
                                          #'hl-spotlight-down arg))))

(defun hl-spotlight-down (&optional n)
  "Move the spotlight down N lines (default 1)."
  (unless n (setq n  1))
  (if (eobp)
      (cancel-timer hl-spotlight-scan-timer)
    (forward-line n)
    (ccm-position-cursor)
    (hl-line-highlight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hl-spotlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hl-spotlight.el ends here
