;;; annotate.el --- simple file annotation system
;;;
;;; Copyright (C) 2005  Tamas Patrovics
;;;
;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;
;;;
;;; M-x annotate-file pops up a window where the annotation for the
;;; current file can be edited. If the annotation window is left open
;;; it always shows the annotation belonging to the current buffer.
;;; 
;;; 

;;; Code:

(defvar annotate-storage-file "~/.annotate"
  "File containing the stored annotations.")


(defvar annotate-current-buffer nil
  "The last buffer known to be the current one.")

(defvar annotate-window-configuration nil
  "The window configuration before the annoation buffer was displayed.")


(defun annotate-file ()
  (interactive)
  (annotate-narrow-to-annotation)

  (setq annotate-window-configuration (current-window-configuration))
  (pop-to-buffer (get-file-buffer annotate-storage-file))

  (local-set-key (kbd "C-c C-c") 'annotate-finish)
  (message "Type C-c C-c to hide the annotation window.")

  (setq annotate-current-buffer 'nosuchbuffer)
  (ad-activate 'switch-to-buffer))


(defun annotate-narrow-to-annotation ()
  "Narrow the annotation buffer to the portion belonging to the file associated
wit the current buffer.
If no annotation exists for the file a new section is created.

If the current buffer has no file associated with it then show a warning message."
  (let* ((buffer (find-file-noselect annotate-storage-file))
         (file (buffer-file-name))
         (filename (if file
                        (expand-file-name file)
                      "nofile")))

    (with-current-buffer buffer
      (widen)
      (unless (equal filename (expand-file-name annotate-storage-file))
        (goto-char (point-min))
        (if (re-search-forward (concat "^\014 " filename) nil t)
            (forward-line)
          (goto-char (point-min))
          (insert "\014 " filename "\n\n")
          (forward-line -1))

        (let ((begin (point)))
          (if (re-search-forward "^\014 " nil t)
              (progn (forward-line -1)
                     (end-of-line))
            (goto-char (point-max)))
          (narrow-to-region begin (point))
          (goto-char (point-min)))

        ;; prepare warning message if it does not exist yet
        (if (and (not file)
                 (= (point-min) (point-max)))
            (insert "The current buffer has no file associated with it, "
                    "so it cannot have an annotation."))))))


(defadvice switch-to-buffer (after annotate-handle-buffer-change)
  (annotate-update-annotation-display))


(defun annotate-update-annotation-display ()
  "Synchronize the displayed annotation to the current buffer if the annotation
window is visible. Otherwise cancel current buffer monitoring."
  (if (get-buffer-window  (let ((buffer (get-file-buffer annotate-storage-file)))
                            (or buffer
                                (find-file-noselect annotate-storage-file))))
      (unless (equal (current-buffer) annotate-current-buffer)
        (setq annotate-current-buffer (current-buffer))
        (annotate-narrow-to-annotation))
      
    (ad-deactivate 'switch-to-buffer)))


(defun annotate-finish ()
  "Hide the annotation buffer and restore previous window configuration."
  (interactive)
  (set-window-configuration annotate-window-configuration)
  (with-current-buffer (get-file-buffer annotate-storage-file)
    (widen)
    (save-buffer)))


(provide 'annotate)
;;; annotate.el ends here
