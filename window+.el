;;; window+.el --- Extensions to `window.el'.
;;
;; Filename: window+.el
;; Description: Extensions to `window.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2011, Drew Adams, all rights reserved.
;; Created: Thu Jan 25 14:22:13 1996
;; Version: 21.0
;; Last-Updated: Tue Jan  4 15:50:22 2011 (-0800)
;;           By: dradams
;;     Update #: 92
;; URL: http://www.emacswiki.org/cgi-bin/wiki/window+.el
;; Keywords: internal, window
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
;;    Extensions to `window.el'.
;;
;;
;;  ***** NOTE: The following function defined in `window.el' has been
;;              REDEFINED HERE:
;;
;;  `count-windows' -
;;     Only use arg MINIBUF if current frame has a minibuffer.
;;
;;  `quit-window' - Call `delete-window' inside `condition-case'.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `window.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "window" '(require 'window+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL in `window.el':
;; 1. Only use arg MINIBUF if current frame has a minibuffer.
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


;; REPLACES ORIGINAL in `window.el':
;; Don't avoid calling `delete-window', even if `one-window-p'.
;; Instead, wrap the call in `condition-case'.
;; This way, if you use my version of `delete-window' (defined in
;; `frame-cmds.el'), then the frame is also deleted if `one-window-p'.
;;
;;;###autoload
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
        (error nil)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'window+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; window+.el ends here
