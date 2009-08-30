;;; list-processes+.el --- 

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2006-10-15 18:35:43>
;; Version: $Id: list-process-mode.el,v 0.0 <2006-10-15 17:13:28> ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

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

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (autoload "list-processes+" 'list-processes+
;;          "A enhance list processes command" t)

;;; Code:

(provide 'list-processes+)
(eval-when-compile
  (require 'cl))

(defvar list-processes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-k" 'list-processes-kill-process)
    (define-key map "\C-m" 'list-processes-goto-buffer)
    (define-key map "G" 'list-processes+)
    (define-key map "S" 'list-processes-sort)
    map)
  "")

(defun list-processes+ (&optional query-only)
  ""
  (interactive "P")
  (list-processes query-only)
  (let ((procs (process-list))
        (inhibit-read-only t))
    (if query-only
        (setq procs (remove-if-not 'process-query-on-exit-flag procs)))
    (save-excursion
      (set-buffer (get-buffer "*Process List*"))
      (goto-char (point-min))
      (forward-line 2)
      (while (not (eobp))
        (put-text-property (point)
                           (progn
                             (forward-line 1)
                             (point)) 'process (car procs))
        (setq procs (cdr procs)))
      (list-processes-mode))))

(define-minor-mode list-processes-mode
  "Add process management to `list-processes'"
  :lighter " LP+"
  :keymap list-processes-mode-map)

(defun list-processes-sort (&optional reverse)
  ""
  (interactive "P")
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (sort-lines reverse (point) (point-max)))))

(defun list-processes-kill-process ()
  ""
  (interactive)
  (let ((proc (get-text-property (point) 'process)))
    (when (and proc
               (y-or-n-p (format "Kill process %s? " (process-name proc))))
      (delete-process proc)
      (list-processes+))))

(defun list-processes-goto-buffer ()
  ""
  (interactive)
  (let ((proc (get-text-property (point) 'process)))
    (when proc
      (if (and (process-buffer proc)
               (buffer-live-p (process-buffer proc)))
          (switch-to-buffer (process-buffer proc))
        (message "No associate buffer!")))))

;;; list-process-mode.el ends here
