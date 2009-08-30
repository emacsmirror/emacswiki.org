;;; orgindent.el --- 

;; Copyright (C) 2009  

;; Author:  <me@HOME-E3EEE1B651>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; proof of concept org tree indentation

;;; Code:


(defconst orgindent-indent-width 2)


(defvar orgindent-overlays nil)
(make-variable-buffer-local 'orgindent-overlays)


(defun orgindent-indent-window ()
  (save-excursion
    (let* ((window (selected-window))
           (limit (window-end window t))
           overlays
           level)
      (goto-char (window-start))

      (setq level (save-excursion
                    (beginning-of-line)
                    (while (and (not (bobp))
                                (not (looking-at org-outline-regexp)))
                      (previous-line))
                    (if (bobp)
                        1
                      (looking-at "^\\(\\*+\\)")
                      (length (match-string 0)))))
          
      (while (and (not (eobp)) (<= (point) limit))
        (if (looking-at "^\\(\\*+\\)")
            (setq level (length (match-string 0))))
        (let ((overlay (if orgindent-overlays
                           (progn
                             (move-overlay (car orgindent-overlays)
                                           (point) (point))                             
                             (pop orgindent-overlays))

                         (make-overlay (point) (point)))))
          (overlay-put overlay 'before-string 
                       (propertize (make-string (* orgindent-indent-width
                                                   (1- level))
                                                ? )
                                   ;; debug
                                   ;'face  'lazy-highlight))
                                   ))
          (push overlay overlays))
        
        (next-line))

      (mapc (lambda (overlay)
              (delete-overlay overlay))
            orgindent-overlays)
      (setq orgindent-overlays overlays))))



(defun orgindent-window-scroll (buffer start)
  (orgindent-indent-window))


(defvar orgindent-change-timer nil)

(defun orgindent-after-change (start end length)
  (if orgindent-change-timer
      (cancel-timer orgindent-change-timer))
  (setq orgindent-change-timer
        (run-with-idle-timer 0 nil  'orgindent-indent-window)))


(defun orgindent-activate ()
  (interactive)
  (add-hook 'window-scroll-functions 'orgindent-window-scroll nil t)
  (add-hook 'after-change-functions 'orgindent-after-change nil t)
  (add-hook  'org-cycle-hook (lambda (arg) (orgindent-indent-window)) nil t)
  (orgindent-indent-window))
  


(provide 'orgindent)
;;; orgindent.el ends here
