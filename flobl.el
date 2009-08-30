;;; flobl.el -- frame-local buffer list

;; Copyright (C) 1997, 2000 Kai Grossjohann
;;
;; Author: Kai Grossjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>
;; Maintainer: Kai Grossjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>
;; Created: 19 April 1997
;; Version: 1.1
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;; USA.

;; LCD Archive Entry: No archive entry.

;;; Installation:
;;
;; Put the file flobl.el in a directory in your load-path.  Byte
;; compile it.  Put the following in your .emacs (or site-start.el or
;; default.el):
;;
;;              (require 'flobl)
;;
;; It does not make sense to autoload this file.

;;; Commentary:
;;
;; When working with several frames, I was always annoyed that
;; switch-to-buffer commands from frame A interfered with the ordering
;; of the buffers when doing C-x C-b in frame B.  To see this, start
;; Emacs, then create a buffer (C-x b aaa RET).  Then, create a new
;; frame and create two new buffers from the second frame (C-x b bbb
;; RET, then C-x b ccc RET).  Now, go back to the first frame and do
;; C-x C-b.  I wanted Emacs to show the *scratch* buffer and aaa as
;; the top two buffers, but the buffers bbb and ccc get in between.
;;
;; This little package solves this problem.
;;
;; This package doesn't change the behavior of the other-buffer
;; function.  This means that the default buffer offered when you do
;; C-x b (switch-to-buffer) doesn't change.  This behavior was `wrong'
;; with Emacs 19 and has been `corrected' for Emacs 20.  It is not
;; possible to fix this for Emacs 19 without hacking the C code.
;;
;; TODO:
;;
;; - Deal with bury-buffer correctly.  (This means to put the buffer
;;   at the bottom of the list, right?)

(defvar flobl-time-tick 0
  "Incremented by 1 for each switch buffer operation.")

(defvar flobl-buffer-order nil
  "Contains information about recently visited buffers for each frame.")

(defvar flobl-last-frame nil
  "Info about last frame the user was in.")

(defvar flobl-last-buffer nil
  "Info about last buffer the user was in.")

(defun flobl-buffer-order-get-frame (fram)
  "Returns the prio information of buffers for FRAM."
  (plist-get flobl-buffer-order fram))

(defun flobl-buffer-order-get-buffer (fram buf)
  "Returns a number indicating how recently BUF has been visited in FRAM."
  (if (not (framep fram)) (error "%s must be a frame"))
  (if (not (bufferp buf)) (error "%s must be a buffer"))
  (or (plist-get (flobl-buffer-order-get-frame fram) buf) 0))

(defun flobl-buffer-order-update (fram buf &optional prio)
  "Sets the prio information of BUF in FRAM to PRIO.
Uses `flobl-time-tick' if PRIO is not given.  `flobl-time-tick' is also
incremented automatically."
  (if (not (framep fram)) (error "%s must be a frame"))
  (if (not (bufferp buf)) (error "%s must be a buffer"))
  (let ((forder (flobl-buffer-order-get-frame fram)))
    (setq forder (plist-put forder buf (or prio flobl-time-tick)))
    (setq flobl-time-tick (1+ flobl-time-tick))
    (setq flobl-buffer-order
          (plist-put flobl-buffer-order fram forder))))

(defun flobl-cmp-buffers (buf1 buf2)
  "Compares two buffers according to `flobl-buffer-order'."
  (let ((p1 (flobl-buffer-order-get-buffer (selected-frame) buf1))
        (p2 (flobl-buffer-order-get-buffer (selected-frame) buf2)))
    (< p2 p1)))

(unless (fboundp 'flobl-orig-buffer-list)
  (fset 'flobl-orig-buffer-list (symbol-function 'buffer-list)))

(defadvice buffer-list (after frame-local-buffer-lists first activate)
  "switch-to-buffer commands in other frames don't affect order of buffers here."
  (setq ad-return-value
        (sort (copy-sequence ad-return-value) 'flobl-cmp-buffers)))

(defun flobl-post-command-hook ()
  "Runs after every command to update buffer priority information."
  (let ((new-buf (current-buffer))
        (new-fram (selected-frame)))
    (when (and (eq flobl-last-frame new-fram)
               (not (eq flobl-last-buffer new-buf)))
      (flobl-buffer-order-update new-fram new-buf))
    (when (eq this-command 'bury-buffer)
      (flobl-buffer-order-update (selected-frame)
                                 (car (last (flobl-orig-buffer-list)))
                                 0))
    (setq flobl-last-frame new-fram)
    (setq flobl-last-buffer new-buf)))

(add-hook 'post-command-hook 'flobl-post-command-hook)

(provide 'flobl)

;;; flobl.el ends here
