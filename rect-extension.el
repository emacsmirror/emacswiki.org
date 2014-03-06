;;; rect-extension.el --- Some extensions for rect.el

;; Filename: rect-extension.el
;; Description: Some extensions for rect.el
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-18 11:36:38
;; Version: 0.1
;; Last-Updated: 2008-10-18 11:36:42
;;           By: Andy Stewart
;; URL:
;; Keywords: rect, region
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `rect' `rect-mark'
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
;; Some extensions for rect.el
;;
;; Have some nice functions:
;;      `execute-command-with-region' can execute command with region
;;      or rectangle, and then return result.
;;
;;      `execute-command-with-region-replace' can execute command with
;;      region or rectangle and replace original one.
;;
;;      `execute-command-with-region-kill' like `execute-command-with-region-replace',
;;      different is use kill action instead replace action.
;;
;;      `mark-rectangle-to-end' will mark rectangle to the longest column
;;      in current rectangle.
;;

;;; Installation:
;;
;; Put rect-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'rect-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/18
;;      First released.
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
(require 'rect)
(require 'rect-mark)

;;; Code:

(defun execute-command-with-region (command)
  "Execute command with current region area.
If current mark is not active, use entire buffer.
Region area can support rectangle area."
  (interactive)
  (let (beg end str rect-p)
    (if mark-active                     ;if mark region is rectangle
        (progn
          (setq rect-p rm-mark-active)
          (setq beg (region-beginning))
          (setq end (region-end))
          (deactivate-mark))
      (setq beg (point-min))
      (setq end (point-max)))
    (if rect-p                          ;if mark region is rectangle
        (let ((string-list (delete-extract-rectangle beg end))
              (rectangle '()))
          (goto-char beg)
          (dolist (string-element string-list) ;generate a new rectangle list
            (add-to-list 'rectangle (funcall command string-element) t))
          rectangle)
      (funcall command (delete-and-extract-region beg end)))))

(defun execute-command-with-region-replace (command)
  "Execute command with current region area and replace it."
  (interactive "*aCommand: ")
  (let (result)
    (setq result (execute-command-with-region command))
    (if (listp result)                  ;if result is string list
        (insert-rectangle result)       ;use `insert-rectangle' to replace rectangle
      (insert result))))                ;otherwise use `insert' to replace region

(defun execute-command-with-region-kill (command)
  "Execute command with current region area and kill it in ring."
  (interactive "*aCommand: ")
  ;; (setq killed-rectangle (execute-command-with-region command))
  (let (result)
    (setq result (execute-command-with-region command))
    (if (listp result)                   ;if result is string list
        (setq killed-rectangle result)   ;set `killed-rectangle'
      (kill-new (format "%s" result))))) ;kill result to kill ring

(defun mark-rectangle-to-end ()
  "Mark current rectangle area to the end.
The end of point is equal end of longest line in rectangle.
And will fill blank at the end of others line."
  (interactive)
  (let ((deactivate-mark)           ;for save mark with `save-excursion' even run `deactivate-mark'.
        beg
        end
        column-end)
    (if rm-mark-active
        (progn
          (setq beg (region-beginning))
          (setq end (region-end))
          ;; fill with blank at end of last line
          ;; if the length of last line is less than
          ;; longest line in rectangle.
          (save-excursion
            (goto-longest-line beg end)
            (end-of-line)
            (setq column-end (current-column))
            (goto-char end)
            (goto-column column-end)    ;fill or longest column
            (setq end (point)))
          (goto-char end))
      (message "Please set rectangle mark first."))))

(defun rte-format-filename (str)
  "RecT Extension format filename."
  (with-temp-buffer
    (insert str)
    ;; capitalize word of filename.
    (goto-char (point-min))
    (while (not (eobp))
      (call-interactively 'capitalize-word))
    ;; downcase word of file extension name.
    (goto-char (point-min))
    (while (not (eobp))
      (if (search-forward-regexp "\\.[^\\.]*$" (line-end-position) t)
          (progn
            (goto-char (match-beginning 0))
            (call-interactively 'downcase-word)))
      (forward-line +1))
    ;; downcase character after '
    (goto-char (point-min))
    (while (not (eobp))
      (while (search-forward-regexp "'" (line-end-position) t)
        (goto-char (match-beginning 0))
        (call-interactively 'downcase-word))
      (forward-line +1))
    ;; replace _ with blank.
    (goto-char (point-min))
    (insert (replace-regexp-in-string "_" " " (delete-and-extract-region (point-min) (point-max))))
    ;; replace multi-blank with one blank
    (goto-char (point-min))
    (insert (replace-regexp-in-string "[ \t]+" " " (delete-and-extract-region (point-min) (point-max))))
    ;; clean up blank between file part and extension part.
    (goto-char (point-min))
    (insert (replace-regexp-in-string "[ ]+\\." "." (delete-and-extract-region (point-min) (point-max))))
    ;; clean up blank that end of line.
    (goto-char (point-min))
    (insert (replace-regexp-in-string "[ \t]+$" "" (delete-and-extract-region (point-min) (point-max))))
    ;; return string
    (buffer-string)))

(provide 'rect-extension)

;;; rect-extension.el ends here

;;; LocalWords:  rect str aCommand RecT multi
