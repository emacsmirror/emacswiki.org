;;; point-ring.el --- Cycle between point positions
;; This file is free software.
;; Copyright FengNingNing (afengtf@188.com) 2009-4-10
;; Released under the GPL, available at http://www.gnu.org
;;
;; You'll probably want the following in your .emacs file.  
;;
;; (require 'point-ring) 
;; (global-set-key (kbd "C-c i") 'point-ring-insert)
;; (global-set-key (kbd "C-c d") 'point-ring-remove)
;; (global-set-key (kbd "C-.") 'point-ring-previous)
;; (global-set-key (kbd "C-,") 'point-ring-next)

(defvar point-ring (make-ring 23))

(defun point-ring-insert ()
  (interactive)
  (message "Location inserted.")
  (ring-insert point-ring (list (current-buffer) (point))))

(defun point-ring-remove ()
  (interactive)
  (if (ring-empty-p point-ring)
      (message "Point ring empty.")
    (progn
      (ring-remove point-ring 0)
      (message "Location removed."))))

(defun point-ring-rotate-left ()
  (interactive)
  (if (ring-empty-p point-ring)
      (message "Point ring empty.")
    (progn
      (let ((head (ring-ref point-ring 0)))
        (ring-remove point-ring 0)
        (ring-insert-at-beginning point-ring head)))))

(defun point-ring-rotate-right ()
  (interactive)
    (if (ring-empty-p point-ring)
      (message "Point ring empty.")
    (progn
      (let ((tail (ring-ref point-ring  -1)))
        (ring-remove point-ring)
        (ring-insert point-ring tail)))))


(defun point-ring-next ()
  (interactive)
  (if (ring-empty-p point-ring)
      (message "Point ring empty.")
    (progn
      (let ((next (ring-ref point-ring 1)))
        (switch-to-buffer (car next))
        (goto-char (car (cdr next)))
        (point-ring-rotate-left))
      (message "Point ring next entry."))))

(defun point-ring-previous ()
  (interactive)
    (if (ring-empty-p point-ring)
      (message "Point ring empty.")
    (progn
      (let ((previous (ring-ref point-ring -1)))
        (switch-to-buffer (car previous))
        (goto-char (car (cdr previous)))
        (point-ring-rotate-right))
      (message "Point ring previous entry."))))


(provide 'point-ring)
