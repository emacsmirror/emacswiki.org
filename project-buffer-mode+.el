;;; project-buffer-mode+.el --- Extension for project-buffer-mode
;;
;; Author:      Cedric Lallain <kandjar76@hotmail.com>
;; Version:     1.10
;; Keywords:    project mode buffer viewer extension
;; Description: Extension for project-buffer-mode.
;; Tested with: GNU Emacs 22.x and GNU Emacs 23.x
;;
;; This file is *NOT* part of GNU Emacs.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Summary:
;;

;; This is an extension for project-buffer-mode.

;;; Commentary:
;; 

;; It provides commands to run the project-buffer actions: such as
;; build, clean, run, debug and update while browsing the files
;; belonging to this project.
;;
;; It also provides a command to directly go to the project-buffer
;; attached to the current file.
;;
;; How does it work?
;;
;; When you open a file from the project-buffer; it will attach the
;; project-buffer to the file's buffer.
;; The project-buffer is stored inside a local variable called:
;;  `project-buffer-mode-p-attached-project-buffer'.
;;
;; To install it:
;;
;; Put the following lines in your .emacs:
;;
;; (eval-after-load "project-buffer-mode"
;;  '(progn
;;     (require 'project-buffer-mode+)
;;     (project-buffer-mode-p-setup)))
;;
;;
;; By default this bind the following keys:
;;  C-x p s    Go to the project-buffer attached to the current file
;;  C-x p B    Kick the 'build action from the attached project-buffer.
;;  C-x p C    Kick the 'clean action from the attached project-buffer.
;;  C-x p R    Kick the 'run action from the attached project-buffer.
;;  C-x p D    Kick the 'debug action from the attached project-buffer.
;;  C-x p U    Kick the 'update action from the attached project-buffer.
;;


;;; History:
;;
;; v1.0: First official release.
;; v1.1: Added a function to the project-buffer's refresh-hook which
;;       check if visited file buffers belongs to the current project.
;;


(require 'project-buffer-mode)

;;; Code:


;;
;;  Helper functions:
;;

(defun project-buffer-mode-p-attach-project-buffer(project-buffer file-buffer)
  "Attach PROJECT-BUFFER buffer to the buffer FILE-BUFFER."
  (with-current-buffer file-buffer
    (unless (local-variable-p 'project-buffer-mode-p-attached-project-buffer)
      (make-local-variable 'project-buffer-mode-p-attached-project-buffer))
    (setq project-buffer-mode-p-attached-project-buffer project-buffer)))


(defun project-buffer-mode-p-get-attached-project-buffer()
  "Get the attached project-buffer."
  (when (and (local-variable-p 'project-buffer-mode-p-attached-project-buffer)
	     (bufferp project-buffer-mode-p-attached-project-buffer))
    project-buffer-mode-p-attached-project-buffer))


;;
;; Interactive commands:
;;

(defun project-buffer-mode-p-run-project-buffer-build-action()
  "Kick the 'build action from the attached project-buffer."
  (interactive)
  (let ((buffer (project-buffer-mode-p-get-attached-project-buffer)))    
    (unless buffer (error "No project-buffer attached to this file"))
    (with-current-buffer buffer
      (project-buffer-perform-build-action))))


(defun project-buffer-mode-p-run-project-buffer-clean-action()
  "Kick the 'clean action from the attached project-buffer."
  (interactive)
  (let ((buffer (project-buffer-mode-p-get-attached-project-buffer)))
    (unless buffer (error "No project-buffer attached to this file"))
    (with-current-buffer buffer
      (project-buffer-perform-clean-action))))


(defun project-buffer-mode-p-run-project-buffer-run-action()
  "Kick the 'run action from the attached project-buffer."
  (interactive)
  (let ((buffer (project-buffer-mode-p-get-attached-project-buffer)))
    (unless buffer (error "No project-buffer attached to this file"))
    (with-current-buffer buffer
      (project-buffer-perform-run-action))))


(defun project-buffer-mode-p-run-project-buffer-debug-action()
  "Kick the 'debug action from the attached project-buffer."
  (interactive)
  (let ((buffer (project-buffer-mode-p-get-attached-project-buffer)))
    (unless buffer (error "No project-buffer attached to this file"))
    (with-current-buffer buffer
      (project-buffer-perform-debug-action))))


(defun project-buffer-mode-p-run-project-buffer-update-action()
  "Kick the 'update action from the attached project-buffer."
  (interactive)
  (let ((buffer (project-buffer-mode-p-get-attached-project-buffer)))
    (unless buffer (error "No project-buffer attached to this file"))
    (with-current-buffer buffer
      (project-buffer-perform-update-action))))


(defun project-buffer-mode-p-go-to-attached-project-buffer()
  "Go to the project-buffer attached to the current file."
  (interactive)
  (let ((buffer (project-buffer-mode-p-get-attached-project-buffer)))
    (unless buffer (error "No project-buffer attached to this file"))
    (switch-to-buffer buffer)))


;;
;;  Hook function:
;;

(defun project-buffer-mode-p-register-project-to-file(project-buffer file-buffer)
  "Register the PROJECT-BUFFER to the FILE-BUFFER.
This will allow to retrieve the buffer."
  (project-buffer-mode-p-attach-project-buffer project-buffer file-buffer))


(defun project-buffer-mode-p-link-buffers-to-current-project(project-list content)
  "Check the different opened file buffer to see if they belong to the project.

Note: technically it's possible to also limit the research to the
current project or to the projects in the list. I don't see why
someone would wanna do that!?"
  (let ((project-buffer (current-buffer))
	(buffers-assoc (remq nil (mapcar (lambda (cur-buf) 
				     (let ((file (buffer-file-name cur-buf))) 
				       (and file (cons (expand-file-name file) cur-buf))))
				   (buffer-list))))
	(count 0))
    (project-buffer-apply-to-each-file (lambda (project-file-name file-path project-name buffers)
					 (let ((assoc-data (assoc (expand-file-name file-path) buffers)))
					   (when assoc-data
					     (project-buffer-mode-p-attach-project-buffer project-buffer (cdr assoc-data))
					     (setq count (1+ count)))))
				       buffers-assoc)
    (message "%i buffer%s attached to this project" count (if (> count 1) "s" ""))
    ))


;;
;;  Setup function:
;;

(defun project-buffer-mode-p-setup(&optional no-key-bindings)
  "Setup the hook and the global keuys if KEY-BINDINGS is set to t."
  (add-hook 'project-buffer-post-find-file-hook 'project-buffer-mode-p-register-project-to-file)
  (add-hook 'project-buffer-refresh-hook 'project-buffer-mode-p-link-buffers-to-current-project)
  (unless no-key-bindings
    (define-key global-map [(control x) (?p) (?s)] 'project-buffer-mode-p-go-to-attached-project-buffer)
    (define-key global-map [(control x) (?p) (?B)] 'project-buffer-mode-p-run-project-buffer-build-action)
    (define-key global-map [(control x) (?p) (?C)] 'project-buffer-mode-p-run-project-buffer-clean-action)
    (define-key global-map [(control x) (?p) (?R)] 'project-buffer-mode-p-run-project-buffer-run-action)
    (define-key global-map [(control x) (?p) (?D)] 'project-buffer-mode-p-run-project-buffer-debug-action)
    (define-key global-map [(control x) (?p) (?U)] 'project-buffer-mode-p-run-project-buffer-update-action)
    ))


;;

(provide 'project-buffer-mode+)

;;; project-buffer-mode+.el ends here
