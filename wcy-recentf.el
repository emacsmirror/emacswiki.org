;;; wcy-recentf.el --- open recent saved file and recent opened file

;; Copyright (C) 2006  Free Software Foundation, Inc.

;; Author: ChunYe Wang <chunye.wang@nsn.com>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:
(defvar wcy-recentf-opened-files-name 
  (format "~/.recent-opened-files.%s@%s" (user-login-name) (system-name)))
(defvar wcy-recentf-saved-files-name 
  (format "~/.recent-saved-files.%s@%s" (user-login-name) (system-name)))
;;; --------internal --------

;;;###autoload
(defun wcy-recentf-put-a-filename-into-a-list(file date old-list)
  (cons (cons file date)
        (remove-if 
         (lambda (elt)
           (string= (car elt) file))
         old-list)))
;;;###autoload
(defun wcy-recentf-read-filename-list-from-file (filename)
 (let ((file-list nil))
  (with-temp-buffer 
    (if (file-readable-p filename) (insert-file-contents filename))
    (goto-char (point-max))
    (while (re-search-backward (concat 
                                "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\) "
                                "\\(.*\\)")
                               nil t nil
                               )
        (let ((file (match-string 2))
              (date (match-string 1)))
          (setq file-list (wcy-recentf-put-a-filename-into-a-list
                           file
                           date
                           file-list))))
    file-list)))
(defvar wcy-recentf-opened-files-list
  (wcy-recentf-read-filename-list-from-file wcy-recentf-opened-files-name))
(defvar wcy-recentf-saved-files-list
  (wcy-recentf-read-filename-list-from-file wcy-recentf-saved-files-name))
(run-with-idle-timer 2 nil 'wcy-recentf-kill-emacs-hook)
(add-hook 'kill-emacs-hook 'wcy-recentf-kill-emacs-hook)
(add-hook 'write-file-hooks 'wcy-recentf-save-file-hook) 
(add-hook 'find-file-hook 'wcy-recentf-open-file-hook)
;;;###autoload
(defun wcy-recentf-save-filename-list-to-file (filename-list file)
  (with-temp-file file
    (mapc 
     (lambda (filename)
       (insert
        (format 
         "%s %s\n"
         (cdr filename) (car filename) )))
     filename-list)))
;; ;;;###autoload
;; (defun wcy-recentf-save-buffer-file-name(file to-file)
;;   (let* ((find-file-hooks nil)
;;          (write-file-hooks nil)
;;          (filename-list (wcy-recentf-read-filename-list-from-file to-file))
;;          (new-filename-list (wcy-recentf-put-a-filename-into-a-list 
;;                              (expand-file-name file)
;;                              (format-time-string "%Y-%m-%d %H:%M")
;;                              filename-list)))
;;     (wcy-recentf-save-filename-list-to-file new-filename-list to-file)))
;;;###autoload
(defun wcy-recentf-open-file-hook()
  (let ((file (buffer-file-name)))
    (when file
      (setq wcy-recentf-opened-files-list 
            (wcy-recentf-put-a-filename-into-a-list 
             file 
             (format-time-string "%Y-%m-%d %H:%M")
             wcy-recentf-opened-files-list
             )))
    nil))
;;;###autoload
(defun wcy-recentf-save-file-hook()
  (let ((file (buffer-file-name)))
    (when file
      (setq wcy-recentf-saved-files-list  
            (wcy-recentf-put-a-filename-into-a-list 
             file 
             (format-time-string "%Y-%m-%d %H:%M")
             wcy-recentf-saved-files-list
             )))
    nil))

;;;###autoload
(defun wcy-recentf-kill-emacs-hook()
  (wcy-recentf-save-filename-list-to-file 
   wcy-recentf-saved-files-list
   wcy-recentf-saved-files-name)
  (wcy-recentf-save-filename-list-to-file 
   wcy-recentf-opened-files-list
   wcy-recentf-opened-files-name))

;; interface to user.
;;;###autoload
(defun wcy-recentf-open-file-internal ( buffer filename)
  (let ((keym (make-sparse-keymap))
        ov)
    (define-key keym (kbd "<RET>") 'find-file-at-point)
    (if (not (file-readable-p filename))
        (call-interactively 'find-file-at-point)
      (with-current-buffer buffer
        (erase-buffer)
        (insert-file-contents filename)
        (goto-char (point-min))
        (while (re-search-forward (concat 
                                   "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\} "
                                   "\\(.*\\)")
                                  nil t nil
                                  )
          (setq ov (make-overlay (match-beginning 1)
                                 (match-end 1)))
          (overlay-put ov 'keymap keym )
          (overlay-put ov 'face 'highlight))
        (goto-char (point-min))
        (forward-word 5)
        (forward-char 1)
        )
      (pop-to-buffer buffer))))
;;;###autoload
(defun wcy-recentf-open-recent-opened-file()
  (interactive)
  (wcy-recentf-save-filename-list-to-file 
   wcy-recentf-opened-files-list
   wcy-recentf-opened-files-name)
  (wcy-recentf-open-file-internal (get-buffer-create " *open recent opened file*")
                                  wcy-recentf-opened-files-name))
;;;###autoload
(defun wcy-recentf-open-recent-saved-file()
  (interactive)
  (wcy-recentf-save-filename-list-to-file 
   wcy-recentf-saved-files-list
   wcy-recentf-saved-files-name)
  (wcy-recentf-open-file-internal (get-buffer-create " *open recent saved file*")
                                  wcy-recentf-saved-files-name))
(provide 'wcy-recentf)
;;; wcy-recentf.el ends here
