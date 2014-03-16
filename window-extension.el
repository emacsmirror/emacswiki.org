;;; window-extension.el --- Some extension functions manipulate window.

;; Filename: window-extension.el
;; Description: Some extension functions manipulate window.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-07 18:29:22
;; Version: 0.1
;; Last-Updated: 2014-03-16 14:47:34
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/window-extension.el
;; Keywords: window
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some extension functions manipulate window.
;;

;;; Installation:
;;
;; Put window-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'window-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2014/03/16
;;      * Add `toggle-window-split'.
;; 
;; 2009/02/07
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defvar toggle-one-window-window-configuration nil
  "The window configuration use for `toggle-one-window'.")

(defun delete-buffer-and-window (buffer-name)
  "Delete buffer and window that special.
Argument BUFFER-NAME the buffer name that will delete."
  (interactive)
  (if (bufferp (get-buffer buffer-name))
      (progn
        (delete-buffer-window buffer-name)
        (kill-buffer (get-buffer buffer-name)))
    (message "Buffer %s is not exist." buffer-name)))

(defun delete-current-buffer-and-window ()
  "Delete current buffer and window."
  (interactive)
  (delete-buffer-and-window (buffer-name)))

(defun delete-buffer-window (buffer-name)
  "Delete the window of special buffer.
Argument BUFFER-NAME the buffer name that will delete."
  (interactive)
  (if (bufferp (get-buffer buffer-name))
      (delete-window (get-buffer-window (get-buffer buffer-name)))
    (message "Buffer %s is not exist." buffer-name)))

(defun delete-current-buffer-window ()
  "Delete the window of current buffer."
  (interactive)
  (delete-buffer-window (current-buffer)))

(defun select-next-window ()
  "Select next window."
  (interactive)
  (other-window +1))

(defun select-prev-window ()
  "Select next window."
  (interactive)
  (other-window -1))

(defun toggle-one-window ()
  "Toggle between window layout and one window."
  (interactive)
  (if (one-window-p t)
      (if toggle-one-window-window-configuration
          (progn
            (set-window-configuration toggle-one-window-window-configuration)
            (setq toggle-one-window-window-configuration nil))
        (message "No other windows exist."))
    (setq toggle-one-window-window-configuration (current-window-configuration))
    (delete-other-windows)))

(defun sticky-window-keep-window-visible ()
  "Insure the buffer associated with the current window stays visible.
This is handy for ERC buffers where you would like to see the
conversation while you work in other windows within the frame.
This is intended to be used with `sticky-window-delete-window'."
  (interactive)
  (set-window-dedicated-p (selected-window) t))

(defun sticky-window-delete-window ()
  "This is intended to be a replacement for `delete-window', but
that avoids deleting windows that have been marked as dedicated
with `sticky-window-keep-window-visible'."
  (interactive)
  (let ((window (selected-window)))
    (if (and (not current-prefix-arg) (window-dedicated-p window))
        (error "This is a dedicated window. Use C-u prefix on this keybinding to really delete it.")
      (set-window-dedicated-p (selected-window) nil)
      (delete-window window))))

(defun sticky-window-delete-other-windows ()
  "Delete all other windows that are not marked to be visible with `sticky-window-keep-window-visible'."
  (interactive)
  (mapcar (lambda (window)
            (if (not (window-dedicated-p window))
                (delete-window window)))
          (cdr (window-list))))

(defun delete-other-windows-vertically+ ()
  "Delete all windows above or below the current window."
  (interactive)
  (let ((win (selected-window)))
    (save-excursion
      (while (condition-case nil (windmove-up) (error nil))
        (delete-window)
        (select-window win))
      (while (condition-case nil (windmove-down) (error nil))
        (delete-window)
        (select-window win)))))

(defun delete-other-windows-horizontally+ ()
  "Delete all windows left or right of the current window."
  (interactive)
  (let ((win (selected-window)))
    (save-excursion
      (while (condition-case nil (windmove-left) (error nil))
        (delete-window)
        (select-window win))
      (while (condition-case nil (windmove-right) (error nil))
        (delete-window)
        (select-window win)))))

(defun another-window ()
  "Select the next window or split the current one."
  (interactive)
  (cond ((one-window-p)
         (split-window-vertically)
         (other-window 1)
         (switch-to-buffer nil))
        (t (other-window 1))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(provide 'window-extension)

;;; window-extension.el ends here
