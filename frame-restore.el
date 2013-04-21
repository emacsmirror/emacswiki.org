;;; frame-restore.el --- Save/restore frame size & position with desktop-save

;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author: Patrick Anderson 
;; Version: 1.4

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; ChangeLog
;; 1.4: overhauled code; removed font save/restore, and made support for maximizing/deiconifying generic
;; 1.3: simplified install (now just copy one line to .emacs, and eval the next)
;; 1.3: added checks for running in terminal
;; 1.3: added checks for running on non-w32
;; 1.2: added font save/restore
;; 1.1: added more descriptive, correct installation docs

;;; Installation:
;; 1. Put this file in your load path and add the next line to your init file (without the semicolons):
;;(require 'frame-restore)
;; 2. Evaluate the next line (put the cursor at the end and type C-x C-e)
;;(progn (require 'desktop) (customize-set-variable 'desktop-enable t) (require 'frame-restore))
;; 3. Restart Emacs to test.

;;; Code:
(require 'cl)

;; This must be global, so it can be saved by desktop-globals-to-save
(defvar frame-restore-params '(50 50 150 50 nil)) ; left, top, width, height, maximized

(defun frame-restore ()
  "Restore frame from `frame-restore-params'."
  (when desktop-save-mode
    ;; now size and position frame according to the  values read from disk
    (set-frame-size (selected-frame) (third frame-restore-params) (fourth frame-restore-params))
    (set-frame-position (selected-frame) (max (eval (first frame-restore-params)) 0)
                        (max (eval (second frame-restore-params)) 0))
    (when (fifth frame-restore-params)
      (set-frame-parameter (selected-frame) 'fullscreen 'maximized))))

(add-hook 'desktop-after-read-hook 'frame-restore)

;; Add our vars to the save list so `desktop.el' will save them out to disk
(defun frame-restore-save ()
  "Save the frame parameters in `frame-restore-params'."
  (make-frame-visible)
  (add-to-list 'desktop-globals-to-save 'frame-restore-params)
  (setq frame-restore-params 
        (list
         (frame-parameter (selected-frame) 'left)
         (frame-parameter (selected-frame) 'top)
         (frame-width)
         (frame-height)
         (listp (frame-parameter (selected-frame) 'left))))) ; if this is a list, we're probably maximized

(add-hook 'desktop-save-hook 'frame-restore-save)

(provide 'frame-restore)
;;; frame-restore.el ends here
