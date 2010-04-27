;;; wcy-desktop.el --- faster than destop.el and less features.

;; Copyright (C) 2009  

;; Author:  <chunye.wang@nsn.com>
;; Keywords: convenience

;; This file is not part of GNU Emacs.

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; save the desktop, i.e. save the opened file. no other features mentioned in
;; desktop.el. in order to make it fater, the file is not really loaded, it is
;; only loaded after you press any key.
;; installation: 
;; (require 'wcy-desktop) 
;; (wcy-destop-init)
;;; Code:

;;;###autoload 
(defvar wcy-desktop-file-name "~/.wcy_desktop_save")
(defvar wcy-desktop-key-map nil)
(when (null wcy-desktop-key-map)
  (setq wcy-desktop-key-map (make-keymap))
  (define-key wcy-desktop-key-map (kbd "C-x") ctl-x-map)
  (fillarray (cadr wcy-desktop-key-map) 'wcy-desktop-load-file))
(defun  wcy-desktop-on-kill-emacs ()
  "save the buffer list, this should be part of kill-emacs-hook"
  (with-temp-file wcy-desktop-file-name
    (print
     (mapcar #'(lambda(b) (with-current-buffer b
			    (cons default-directory buffer-file-name)))
	     (remove-if-not 'buffer-file-name (buffer-list)))
     (current-buffer))))
(defun  wcy-desktop-init ()
  "this function install the wcy-desktop. put
it (wcy-desktop-init) in your ~/.emacs "
  (add-hook 'kill-emacs-hook 'wcy-desktop-on-kill-emacs)
  (wcy-desktop-open-last-opened-files))
(defun  wcy-desktop-open-last-opened-files ()
  "open files which are still open in last session."
  (when (file-readable-p wcy-desktop-file-name)
    (with-temp-buffer
      (insert-file-contents wcy-desktop-file-name)
      (goto-char (point-min))
      (dolist (x (read (current-buffer)))
        (let* ((my-default-directory (car x))
               (my-buffer-file-name (cdr x)))
          (when (file-readable-p my-buffer-file-name)
            (let ((buffer (or (get-file-buffer my-buffer-file-name)
                              (create-file-buffer my-buffer-file-name))))
              (with-current-buffer buffer
                (insert "THE BUFFER IS NOT LOADED YET. PRESS ANY KEY TO LOAD IT.")
                (goto-char 1)
                (set (make-local-variable 'wcy-desktop-is-buffer-loaded) nil)
                (use-local-map wcy-desktop-key-map)
                (setq default-directory  my-default-directory
                      buffer-file-name my-buffer-file-name
                      major-mode 'not-loaded-yet
                      buffer-read-only t
                      mode-name  "not loaded yet")
                (set-buffer-modified-p nil)))))))))
(defun  wcy-desktop-load-file (&optional buffer)
  "load file by reverting buffer"
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (when (local-variable-p 'wcy-desktop-is-buffer-loaded)
      (message "wcy desktop: %s is loaded" buffer-file-name)
      (revert-buffer nil t nil)
      (when (eq major-mode 'not-loaded-yet)
        (fundamental-mode)))))
(provide 'wcy-desktop)
;;; wcy-desktop.el ends here
