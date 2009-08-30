;;; keep-end.el --- 

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2006-10-15 17:35:45>
;; Version: $Id: keep-tail.el,v 0.0 <2006-10-05 23:15:08> ywb Exp $
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
;;   (require 'keep-end)

;;;; use it for some interactive program, such as *SQL*, *R*:
;; (add-hook 'sql-interactive-mode-hook
;;           (lambda ()
;;             (keep-end-watch-this (current-buffer))))
;; (add-hook 'inferior-ess-mode-hook
;;           (lambda ()
;;             (keep-end-watch-this (current-buffer))))

;;; Code:

(provide 'keep-end)
(eval-when-compile
  (require 'cl))

(defvar keep-end-timer nil)
(defvar keep-end-buffer-list nil)

(defun keep-end-handler ()
  (when (not (minibuffer-window-active-p (selected-window)))
    (let ((selwin (selected-window)))
      (dolist (win (remove-if (lambda (w) (eq w selwin))
                              (window-list)))
        (if (member (window-buffer win) keep-end-buffer-list)
            (with-selected-window win
              (when (not (eobp))
                (with-no-warnings
                  (end-of-buffer))
                (recenter '(t)))))))))

(define-minor-mode keep-end-mode
  "If on, keep some buffer always at end."
  :global t
  (if keep-end-mode
      (or (and keep-end-timer
               (timerp keep-end-timer))
          (setq keep-end-timer
                (run-with-idle-timer 0.5 t 'keep-end-handler)))
    (and keep-end-timer (cancel-timer keep-end-timer))))

(defun keep-end-watch-this (buf)
  "Keep this buffer always at end of buffer if not activate."
  (interactive "bWatch this buffer: ")
  (unless keep-end-mode
    (keep-end-mode 1))
  (setq keep-end-buffer-list (remove-if-not 'buffer-live-p keep-end-buffer-list))
  (add-to-list 'keep-end-buffer-list (get-buffer buf)))

(defun keep-end-remove-this (buf)
  (interactive (list (completing-read "Remove this buffer: "
                                      (mapcar 'buffer-name keep-end-buffer-list)
                                      nil t
                                      (buffer-name
                                       (unless (member (buffer-name) keep-end-buffer-list)                                          
                                         (car keep-end-buffer-list))))))
  (setq keep-end-buffer-list (delete (get-buffer buf) keep-end-buffer-list)))

;; keep-end.el ends here
