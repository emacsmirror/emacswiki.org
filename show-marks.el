;;; show-marks.el --- Navigate and visualize the mark-ring

;; Filename: show-marks.el
;; Description: Navigate and visualize the mark-ring
;; Author: Greg Rowe <emacs@therowes.net>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyright (c) 2003 Greg Rowe
;; Created: 2003
;; Version: 0.4
;; Last-Updated: 2013-08-05 15:48:00
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/mark
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((fm "1.0"))
;;
;; Features that might be required by this library:
;;
;; fm
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
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; * Commentary
;; This library provides the commands to navigate and display the mark ring.
;; The `show-marks' command displays a buffer listing the marks in the buffer from which it was called.
;; You can then press enter on one of the listed marks to jump to it, or press d to delete it from the
;; mark ring. You can also use the `forward-mark' and `backward-mark' commands to navigate the marks in
;; the mark ring. 

;;; Installation:
;;
;; Put show-marks.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'show-marks)
;;
;;  I recommend also binding the commands to global keys, e.g:
;; 
;;    (global-set-key (kbd "<C-s-right>") 'forward-mark)
;;    (global-set-key (kbd "<C-s-left>") 'backward-mark)
;;    (global-set-key (kbd "<C-s-down>") 'show-marks)
;; 
;;;;



;;; Customize:
;;
;;

;;
;; All of the above can customized by:
;;      M-x customize-group RET mark RET
;;

;;; Change log:
;;	
;; 2013/05/10 - Joe Bloggs
;;      * Added to github
;; 2010/02/17 - Joe Bloggs
;;      * Use fm with mark-mode
;;      * New commands: mark-mode-delete mark-mode-prev-mark mark-mode-next-mark
;;      * New keybindings for mark-mode
;;

;;; Acknowledgements:
;;
;; Original author: Greg Rowe
;;

;;; TODO
;;
;; 
;;

;;; Require
(require 'fm)

;;; Code:

(defvar mark-ring-pos -1
  "This tracks the current position in the mark ring for movement.")

(make-variable-buffer-local 'mark-ring-pos)

;; Advise `set-mark-command' 
(defadvice set-mark-command (after mark-reset-pos (arg))
  "After set-mark-command is called the mark-ring position will be
reset."
  (setq mark-ring-pos -1))

(ad-activate 'set-mark-command)

;; use addition to go backwards, and subtraction to go forward
;;;###autoload
(defun backward-mark (arg)
  "Moves the point arg points backward in the mark ring."
  (interactive "P")
  (if (null arg) (setq arg 1))
  (if (or mark-ring (mark t))
      (progn
        (let ((mark-ring-length (length mark-ring)))
          (setq mark-ring-pos (+ mark-ring-pos (abs arg)))
          (if (> mark-ring-pos mark-ring-length)
              (setq mark-ring-pos 
                    (- (- mark-ring-pos mark-ring-length ) 1)))
          (goto-nth-mark mark-ring-pos)))))

;;;###autoload  
(defun forward-mark (arg)
  "Moves the point arg points forward in the mark ring."
  (interactive "P")
  (if (= -1 mark-ring-pos) (setq mark-ring-pos 0))
  (if (null arg) (setq arg 1))
  (if (or mark-ring (mark t))
      (progn
        (let ((mark-ring-length (length mark-ring)))
          (setq mark-ring-pos (- mark-ring-pos (abs arg)))
          (if (<  mark-ring-pos 0)
                (setq mark-ring-pos 
                    (+ (+ mark-ring-length mark-ring-pos) 1)))
          (goto-nth-mark mark-ring-pos)))))

(defun goto-nth-mark (arg)
  ;; Moves the point to the nth position in the mark ring (starts at 1).
  (let ((the-place (cond
                    ((= arg 0) (mark t))
                    ((= arg 1) (car mark-ring))
                    (t (car (nthcdr (- arg 1) mark-ring))))))
    (if the-place
        (goto-char the-place))))

(defvar mark-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'mark-mode-goto)
    (define-key map (kbd "<delete>") 'mark-mode-delete)
    (define-key map "d" 'mark-mode-delete)
    (define-key map "q" 'delete-window)
    (define-key map (kbd "<up>") 'mark-mode-prev-mark)
    (define-key map (kbd "<down>") 'mark-mode-next-mark)
    (define-key map "x" (lambda nil (interactive) (kill-buffer (current-buffer))))    
    map)
  "Keymap for `mark-mode'.")


(defvar mark-buffer nil
  "Name of buffer for last show-marks.")

(defcustom show-marks-move-point t
  "If non-nil then the cursor will be moved to the *marks* buffer when `show-marks' is called."
  :type 'boolean)

(put 'mark-mode 'mode-class 'special)

(define-derived-mode mark-mode nil "mark"
  "Major mode for output from \\[show-marks].
\\<mark-mode-map>Move point to one of the items in this buffer, then use
\\[mark-mode-goto] to go to the mark that the item refers to.
\\{mark-mode-map}"
  (make-local-variable 'mark-buffer)
  (add-to-list 'fm-modes '(mark-mode mark-mode-goto))
  (toggle-read-only 1)
  (fm-start))

;;;###autoload
(defun mark-mode-goto ()
  "Go to the occurrence the current line describes."
  (interactive)
  (let ((pos (get-text-property (point) 'marker)))
    (pop-to-buffer mark-buffer)
    (goto-char pos)))

;;;###autoload
(defun mark-mode-delete nil
  "Delete mark at current line from mark-ring."
  (interactive)
  (let ((mark (get-text-property (point) 'marker)))
    (with-current-buffer mark-buffer
      (setq mark-ring (delete mark mark-ring)))
    (toggle-read-only -1)
    (kill-line 1)
    (toggle-read-only 1)))

;;;###autoload
(defun mark-mode-prev-mark ()
  "Move to previous mark in *mark* buffer, wrapping if necessary."
  (interactive)
  (if (> (line-number-at-pos) 1)
      (previous-line)
    (goto-char (point-max))
    (previous-line)
    (move-beginning-of-line nil)))

;;;###autoload
(defun mark-mode-next-mark ()
  "Move to next mark in *mark* buffer, wrapping if necessary."
  (interactive)
  (if (< (line-number-at-pos) (count-lines (point-min) (point-max)))
      (next-line)
    (goto-char (point-min))
    (move-beginning-of-line nil)))

(defun show-mark (mark-list)
  (if mark-list
      (let ((mymarker (car mark-list))
            (linenum 0)
            (mybol 0)
            (myeol 0)
            (prop-start 0))
        (save-current-buffer
          (set-buffer mark-buffer)
          (setq linenum (+ (count-lines 1 mymarker) 1))
          (save-excursion
            (goto-char  mymarker)
            (setq mybol (point-at-bol))
            (setq myeol (point-at-eol))))
        (setq prop-start (point))
        (insert (format "%6d: " linenum))
        (insert-buffer-substring mark-buffer mybol myeol)
        (insert "\n")
        (put-text-property prop-start (point) 'marker mymarker)
        (show-mark (cdr mark-list)))))

;;;###autoload
(defun show-marks ()
  "Displays all the lines for each point in the mark ring.  Pressing
RET in the result buffer will send you to corresponding mark point
with out affecting the mark-ring."
  (interactive)
  (with-output-to-temp-buffer "*marks*"
    (let ((old-buffer-mark-ring mark-ring))
      ;; prepend the current mark
      (setq old-buffer-mark-ring (cons (copy-marker (mark-marker)) mark-ring))
      (setq mark-buffer (current-buffer))
      (set-buffer standard-output)
      (show-mark old-buffer-mark-ring)
      (mark-mode)))
  (if show-marks-move-point
      (select-window (get-buffer-window "*marks*"))))

(provide 'show-marks)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "show-marks.el" (buffer-name) (buffer-string) "update")

;;; show-marks.el ends here
