;;; multiverse.el --- manage multiple versions of buffers in an Emacs session

;; Copyright (C) 2006 Tamas Patrovics

;; $Date: 2008/12/17 18:07:00 $

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

;;; Sometimes during programming I want to make experimental
;;; modifications on a file and also want to be able to return to the
;;; previous version if the modifications prove to be a dead end.
;;; 
;;; Version control systems make this easier, but often I don't yet
;;; want to check in the current version, because it's not complete,
;;; but want to experiment nevertheless.
;;; 
;;; In such cases I usually make a backup copy of the file, but it's
;;; tedious, so I made this package which keeps multiple versions of
;;; the current buffer in temporary files, so that the user can return
;;; quickly to a previously saved version.
;;; 

;;; Code:

(eval-when-compile (require 'cl))


(defvar multiverse-stored-versions nil
  "The different stored versions of buffers are kept here.")


(defun multiverse-store ()
  "Store current buffer contents."
  (interactive)
  (let* ((buffer-info (assoc (current-buffer) multiverse-stored-versions))
         (versions (cdr buffer-info))
         (name (completing-read "Store with this name: " versions)))
    (when (not (equal name ""))
      (unless buffer-info
        (setq buffer-info (cons (current-buffer) nil))
        (push buffer-info multiverse-stored-versions))

      (let ((filename (multiverse-save-buffer))
            (version (assoc name versions)))
        (if version
            (progn (delete-file (cdr version))
                   (setcdr version filename))
          (push (cons name filename) (cdr buffer-info)))))))


(defun multiverse-restore ()
  "Restore current buffer contents from a previous version."
  (interactive)
  (let ((file (multiverse-get-version)))
    (if (not file)
        (message "There are no previous stored versions for this buffer.")
      
      (erase-buffer)
      (insert-file-contents file))))


(defun multiverse-forget ()
  "Delete previous stored versions of the current buffer."
  (interactive)
  (let ((buffer-info (assoc (current-buffer) multiverse-stored-versions)))
    (if (not buffer-info)        
        (message "There are no previous stored versions for this buffer.")
      
      (dolist (version (cdr buffer-info))
        (delete-file (cdr version)))
      
      (setq multiverse-stored-versions
            (assq-delete-all (current-buffer) multiverse-stored-versions)))))


(defun multiverse-diff-current ()
  "Compare the current buffer contents to a previous version."
  (interactive)
  (let ((filename (multiverse-save-buffer)))
    (unwind-protect
        (multiverse-make-diff (multiverse-get-version) filename)

      (delete-file filename))))

  
(defun multiverse-diff-other ()
  "Compare two previous versions of the current buffer."
  (interactive)
  (multiverse-make-diff (multiverse-get-version)
                        (multiverse-get-version)))

  
(defun multiverse-save-buffer ()
  "Save current buffer to a temporary file and return the name of the
file."
  (let ((filename (make-temp-file "multiverse"))
        (buffer (current-buffer)))
    (with-temp-file filename
      (insert-buffer-substring buffer))
    filename))
  

(defun multiverse-get-version ()
  "Read a name and return the file name of the stored version of
buffer associated with that name."
  (let* ((buffer-info (assoc (current-buffer) multiverse-stored-versions))
         (versions (cdr buffer-info)))
    (if versions
        (let ((name (completing-read "Select version: " versions nil t)))        
          (if (not (equal name ""))
              (cdr (assoc name versions)))))))
  

(defun multiverse-make-diff (oldfile newfile)
  "Compare the given two buffer versions."
  (if (and oldfile newfile)      
      (diff oldfile newfile nil t)

    (message "There are no previous stored versions for this buffer.")))
  
(provide 'multiverse)
;;; multiverse.el ends here

