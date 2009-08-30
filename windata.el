;;; windata.el --- convert window configuration to list

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: $Id: windata.el,v 0.0 2007/12/13 00:32:15 ywb Exp $
;; Keywords: convenience, frames
;; 
;; This file is part of PDE (Perl Development Environment).
;; But it is useful for generic programming.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; This extension is useful when you want save window configuration
;; between emacs sessions or for emacs lisp programer who want handle
;; window layout.

;;; Dependencies:
;;  no extra libraries is required

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'windata)
;; You can use desktop to save window configuration between different
;; session:
;;   (require 'desktop)
;;   (add-to-list 'desktop-globals-to-save 'windata-named-winconf)

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar windata-named-winconf nil
  "Name of all window configuration")
(defvar windata-data-function 'windata-data-default
  "Function to save window data.
The data should be persistent permanent.")
(defvar windata-data-restore-function 'windata-data-restore-default
  "Function to restore window data.")

(defvar windata-total-width nil
  "Internal variable.")
(defvar windata-total-height nil
  "Internal variable.")

(defun windata-fix (arg fix)
  "Round the ARG with FIX decimal.

  (windata-fix 0.123456 4) => 0.1234"
  (let ((n (expt 10 fix)))
    (/ (float (floor (* arg n))) n)))

(defun windata-window-width (win)
  (let ((width (if (windowp win)
                   (window-width win)
                 (let ((edge (cadr win)))
                   (- (nth 2 edge) (car edge))))))
    (windata-fix (/ width windata-total-width) 4)))

(defun windata-window-height (win)
  (let ((height (if (windowp win)
                    (window-height win)
                  (let ((edge (cadr win)))
                    (- (nth 3 edge) (cadr edge))))))
    (windata-fix (/ height windata-total-height) 4)))

(defun windata-data-default (win)
  (buffer-name (window-buffer win)))

(defun windata-data-restore-default (win name)
  (and (buffer-live-p (get-buffer name))
       (set-window-buffer win (get-buffer name))))

(defun windata-window-tree->list (tree)
  (if (windowp tree)
      (funcall windata-data-function tree)
    (let ((dir (car tree))
          (children (cddr tree)))
      (list (if dir 'vertical 'horizontal)
            (if dir
                (windata-window-height (car children))
              (windata-window-width (car children)))
            (windata-window-tree->list (car children))
            (if (> (length children) 2)
                (windata-window-tree->list (cons dir (cons nil (cdr children))))
              (windata-window-tree->list (cadr children)))))))

(defun windata-list->window-tree (conf)
  (if (listp conf)
      (let ((newwin
             (if (eq (car conf) 'horizontal)
                 (split-window nil (floor (* (cadr conf) windata-total-width)) t)
               (split-window nil (floor (* (cadr conf) windata-total-height))))))
        (windata-list->window-tree (nth 2 conf))
        (select-window newwin)
        (windata-list->window-tree (nth 3 conf)))
    (funcall windata-data-restore-function (selected-window) conf)))

(defun windata-window-path (tree win &optional path)
  (if (windowp tree)
      (if (eq win tree)
          path)
    (let ((children (cddr tree))
          (i 0)
          conf)
      (while children
        (setq conf (windata-window-path (car children) win (append path (list i))))
        (if conf
            (setq children nil)
          (setq i (1+ i)
                children (cdr children))))
      conf)))

(defun windata-current-winconf ()
  (let ((tree (car (window-tree)))
        (windata-total-width (float (frame-width)))
        (windata-total-height (float (frame-height))))
    (cons (windata-window-tree->list tree)
          (windata-window-path tree (selected-window)))))

(defun windata-restore-winconf (winconf &optional inside-p)
  "Restore window configuration from `windata-current-winconf'.
When INSIDE-P is non-nil, that mean the window configuration
is restore in current window, that is to say not delete other
windows."
  (let ((path (cdr winconf))
        windata-total-width
        windata-total-height
        tree)
    (if inside-p
        (setq windata-total-width (float (window-width))
              windata-total-height (float (window-height)))
      (setq windata-total-width (float (frame-width))
            windata-total-height (float (frame-height)))
      (delete-other-windows))
    (windata-list->window-tree (car winconf))
    ;; I don't know how to select the window when inside
    (unless inside-p
      (setq tree (car (window-tree)))
      (while path
        (setq tree (nth (car path) (cddr tree))
              path (cdr path)))
      (select-window tree))))

 
;; An example
;;;###autoload 
(defun windata-name-winconf (name)
  "Save window configuration with NAME.
After save the window configuration you can restore it by NAME using
`windata-restore-named-winconf'."
  (interactive "sName of window configuration: ")
  (setq windata-named-winconf
        (cons
         (cons name (windata-current-winconf))
         (delq (assoc name windata-named-winconf) windata-named-winconf))))

;;;###autoload 
(defun windata-restore-named-winconf (name)
  "Restore saved window configuration."
  (interactive (list (completing-read "Save named window configuration: "
                                      windata-named-winconf nil t)))
  (windata-restore-winconf (assoc-default name windata-named-winconf)))

;; To learn what the function behaves, you can eval these forms when two
;; or more window present in current frame.
;; (windata-display-buffer (get-buffer "*Messages*") nil 'top 0.3 nil)
;; (windata-display-buffer (get-buffer "*Messages*") nil 'bottom 0.3 nil)
;; (windata-display-buffer (get-buffer "*Messages*") nil 'left 0.3 nil)
;; (windata-display-buffer (get-buffer "*Messages*") nil 'right 0.3 nil)

;; (windata-display-buffer (get-buffer "*Messages*") t 'top 0.3 nil)
;; (windata-display-buffer (get-buffer "*Messages*") t 'bottom 0.3 nil)
;; (windata-display-buffer (get-buffer "*Messages*") t 'left 0.3 nil)
;; (windata-display-buffer (get-buffer "*Messages*") t 'right 0.3 nil)

;; ;; when delete-p is presented, the second parameter make no sense.
;; (windata-display-buffer (get-buffer "*Messages*") t 'top 0.3 t)
;; (windata-display-buffer (get-buffer "*Messages*") t 'bottom 0.3 t)
;; (windata-display-buffer (get-buffer "*Messages*") t 'left 0.3 t)
;; (windata-display-buffer (get-buffer "*Messages*") t 'right 0.3 t)
;; (windata-display-buffer (get-buffer "*Messages*") nil 'top 0.3 t)
;; (windata-display-buffer (get-buffer "*Messages*") nil 'bottom 0.3 t)
;; (windata-display-buffer (get-buffer "*Messages*") nil 'left 0.3 t)
;; (windata-display-buffer (get-buffer "*Messages*") nil 'right 0.3 t)
;;;###autoload
(defun windata-display-buffer (buf frame-p dir size &optional delete-p)
  "Display buffer more precisely.
FRAME-P is non-nil and not window, the displayed buffer affect
the whole frame, that is to say, if DIR is right or left, the
displayed buffer will show on the right or left in the frame. If
it is nil, the buf will share space with current window.

DIR can be one of member of (right left top bottom).

SIZE is the displayed windowed size in width(if DIR is left or
right) or height(DIR is top or bottom). It can be a decimal which
will stand for percentage of window(frame) width(heigth)

DELETE-P is non-nil, the other window will be deleted before
display the BUF."
  (or (get-buffer-window buf)
      (let ((win (selected-window))
            newwin horflag total)
        (if (eq frame-p 'window)
            (setq frame-p nil))
        (if (eq delete-p 'no-delete)
            (setq delete-p nil))
        (if (memq dir '(left right))
            ;; '(top bottom))
            (setq horflag t))
        (setq total (if (or delete-p frame-p)
                        (if horflag
                            (frame-width)
                          (frame-height))
                      (if horflag
                          (window-width)
                        (window-height))))
        (if (< size 1)
            (setq size (floor (* total size))))
        (if (memq dir '(right bottom))
            (setq size (- total size)))
        (if (or delete-p (not frame-p))
            (progn
              (and delete-p (delete-other-windows))
              (setq newwin (split-window nil size horflag))
              (when (memq dir '(left top))
                (setq win newwin
                      newwin (selected-window)))
              (set-window-buffer newwin buf)
              (select-window win))
          (let ((conf (windata-current-winconf))
                (curbuf (current-buffer)))
            (delete-other-windows)
            (setq newwin (split-window nil size horflag))
            (when (memq dir '(left top))
              (setq win newwin
                    newwin (selected-window))
              (select-window win))
            (windata-restore-winconf conf t)
            (set-window-buffer newwin buf)
            (select-window (get-buffer-window curbuf))))
        newwin)))

(provide 'windata)
;;; windata.el ends here
