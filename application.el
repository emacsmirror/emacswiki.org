;;; application.el --- start application for files matching entries from application-caller-list

;; Copyright (C) 2010  TN

;; Author: TN
;; Keywords: dired, application

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Files with binary format are better opened in appropriate
;; applications. This regards for an instance Openoffice-files or
;; Starcalc-files. If this package is loaded the find-file command of
;; emacs starts appropriate application for files with names matching
;; entries in application-caller-list (we refer to such a file as an
;; application document). The association of the applications with the
;; file names is implemented rather low-level.  Because of that the
;; application ist started whenever one opens an application document
;; via find-file. That works with C-x C-f as well as with dired-find.
;; 
;; The file buffer corresponding to the application document is
;; associated with the application process.  If the application ends
;; the buffer is killed.

;;; Code:


(add-to-list 'inhibit-file-name-handlers 'application-handler)

(defgroup application nil
  "Start applications for binary files.")

(defcustom application-caller-list
  '(
    (".*\\.pdf$" . "xpdf")
    (".*\\.xls$" . "openoffice.org")
    (".*\\.doc\\(x\\)?$" . "openoffice.org")
    (".*\\.ppt\\(x\\)?$" . "openoffice.org")
    (".*\\.is[xm]$" ("emacs") ("simx3" . "simx3") ("simx3.4officialRelease" . "simx3.4officialRelease") ("simx3.4stable" . "simx3.4stable"))
    )
  "List assigning applications to file names. Each element of the list is a cons with two strings: the regular expression matching the file names as car and the application to be started as cdr."
  :group 'application)

(defun application-caller-regexp ()
  "Constructs a regexp matching all file names for application-hanlder from `application-caller-list'."
  (let ((a application-caller-list)
	(s "\\("))
    (while a
      (setq s (concat s (caar a) "\\)"))
      (setq a (cdr a))
      (if a (setq s (concat s "\\|\\("))))
    s))

(defun application-handler-set ()
  "Set `application-handler' as handler for file-name-operations."
  (let ((handler-entry (rassoc 'application-handler file-name-handler-alist)))
    (if handler-entry
	(setcar (car (memq handler-entry file-name-handler-alist)) (application-caller-regexp))
      (setq file-name-handler-alist (cons (cons (application-caller-regexp) 'application-handler) file-name-handler-alist)))))

(application-handler-set)

(defvar application-bash-program "bash"
  "The applications in `application-caller-list' are started through the bash shell. This is the command string to call bash.")

(defun application-popup-menu (file-name app-list)
  (x-popup-menu t (list (concat "Applications for " file-name)
				(append '("") app-list))))

(defun new-buffer-name (name)
  "Create new unique buffer name basing on NAME."
  (while (get-buffer name)
    (if (string-match "\\(.*\\)<\\([0-9]+\\)>$" name)
	(setq name (concat (match-string 1 name) "<"
			   (number-to-string
			    (1+
			     (string-to-number (match-string 2 name))))
			   ">"))
      (setq name (concat name "<1>"))))
  name)

(defun application-handler (fct &rest args)
  "For files whos names match one of the regexps in `application-caller-list' insert-file-contents does not really insert the file contents but starts the corresponding application. The file buffer becomes the application process buffer."
  (or
   (and (equal fct 'insert-file-contents)
	(let* ((filename (car args))
	       (framed-filename (concat "\"" filename "\""))
	       (handler-entry (assoc-if '(lambda (filename-regexp) (string-match filename-regexp filename)) application-caller-list))
	       (application-filename-regexp (car handler-entry))
	       (application-caller (cdr handler-entry))
	       application-process)
	  (if (listp application-caller)
	      (setq application-caller (application-popup-menu framed-filename application-caller)))
	  (if application-caller
	      (progn
		(message "Starting \"%S\" on file \"%s\"" application-caller framed-filename)
		(if (nth 1 args) (setq buffer-file-name filename))
		(let ((application-process (start-process (concat "*proc:" buffer-file-name "*") nil application-bash-program "-c" (concat application-caller " " framed-filename))))
		  (insert (format "Application \n%s\nwith process\n%S\nfor file\n%s" application-caller application-process framed-filename))
		  (set-buffer-modified-p nil)
		  (set-process-buffer application-process (current-buffer))
		  (set-process-sentinel application-process 'application-sentinel))
		(setq buffer-file-name (new-buffer-name (concat "*app:" filename "*")))
		(rename-buffer buffer-file-name)
		(list buffer-file-name 0)))))
   (let ((inhibit-file-name-operation fct))
     (apply fct args))))

(defun application-sentinel (proc eventType)
  "This sentinel is registered by `application-handler' for
application processes. It kills the associated file buffer when the application ends."
  (if (string= eventType "finished\n")
      (with-current-buffer (process-buffer proc)
	(set-buffer-modified-p nil)
	(kill-buffer))))

(provide 'application)
;; End of package application.
