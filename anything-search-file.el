;;; anything-search-file.el --- Search file by locate or find.

;; Author: Takayuki YAMAGUCHI <d@ytak.info>
;; Keywords: anything locate find
;; Version: 0.1.0
;; Created: Wed Aug 19 22:24:02 2009

;; Copyright 2009 Takayuki YAMAGUCHI
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later 
;; version. 
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT 
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
;; 
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

(require 'anything)

(defvar anything-search-file-locate-home-database (concat (getenv "HOME") "/.emacs.d/home_locate_db"))

;; function of updatedb for home directory
;; for ubuntu 8.04
(defvar anything-search-file-updatedb-command (concat "updatedb " "-l 0 -U " (getenv "HOME") " -o "
						      anything-search-file-locate-home-database))
;; for debian 4.0
;; (defvar anything-search-file-updatedb-command (concat "updatedb --output=" anything-search-file-locate-home-database
;; 							" --localpaths=" (getenv "HOME")))

(defun anything-search-file-updatedb-home-directory ()
  "Update locate database for home directory."
  (interactive)
  (start-process-shell-command "updatedb-process" nil "nice" anything-search-file-updatedb-command))

(defface anything-dir-name-face
  '((t (:foreground "DodgerBlue1")))
  "*Face used for directory privilege indicator (d) in dired buffers."
  :group 'anything)

(defface anything-file-name-face
  '((t (:foreground "aquamarine3")))
  "*Face used for file names (without suffixes) in dired buffers."
  :group 'anything)

(defvar anything-c-file-names-face1 'anything-dir-name-face)
(defvar anything-c-file-names-face2 'anything-file-name-face)

(defun anything-c-highlight-file-name (files)
  (loop for i in files
        if (file-directory-p i)
        collect (propertize i
                            'face anything-c-file-names-face1
                            'help-echo (expand-file-name i))
        else
        collect (propertize i
                            'face anything-c-file-names-face2
                            'help-echo (expand-file-name i))))



(defvar anything-search-file-pattern nil)

(defvar anything-search-file-locate-default-options nil)

(defvar anything-search-file-locate-minimum-string-number 2)

(defvar anything-search-file-locate-process-name "locate-process")

(defun anything-search-file-locate-kill-process ()
  (if (get-process anything-search-file-locate-process-name) (delete-process anything-search-file-locate-process-name)))

(defun anything-search-file-locate-init ()
  (if anything-search-file-locate-database
      (apply 'start-process anything-search-file-locate-process-name (anything-candidate-buffer 'global)
	     (append `("locate" "-d" ,anything-search-file-locate-database "-i" "-r" ,anything-search-file-pattern)
		     anything-search-file-locate-default-options))
    (apply 'start-process anything-search-file-locate-process-name (anything-candidate-buffer 'global)
	   (append `("locate" "-i" "-r" ,anything-search-file-pattern)
		   anything-search-file-locate-default-options)))
  
  (set-process-sentinel (get-process anything-search-file-locate-process-name) (lambda (process event)))
  (set-process-filter (get-process anything-search-file-locate-process-name)
		      (lambda (process output)
			(with-current-buffer (process-buffer process)
    			  (save-excursion
			    (goto-char (process-mark process))
			    (insert (replace-regexp-in-string (concat "^" (getenv "HOME")) "~" output))
			    (set-marker (process-mark process) (point)))))))

(defvar anything-c-source-locate-file
  '((name . "Locate")
    (init . anything-search-file-locate-init)
    (cleanup . anything-search-file-locate-kill-process)
    (candidates-in-buffer)
    (type . file)
    (delayed)
    (candidate-transformer anything-c-highlight-file-name)))

(defun anything-search-file-locate (locate-arg &optional all-dirs pattern)
  "Find file by locate."
  (interactive "slocate argument: \np")
  (let ((anything-search-file-pattern) (anything-search-file-locate-database))
    (if (and (stringp locate-arg) (> (length locate-arg) anything-search-file-locate-minimum-string-number))
	(progn
	  (if (or (not all-dirs) (and (integerp all-dirs) (= all-dirs 1)))
	      (progn
		(setq anything-search-file-locate-database anything-search-file-locate-home-database)
		(setcdr (assoc 'name anything-c-source-locate-file) (concat "Locate \"" locate-arg "\" in home directory")))
	    (setcdr (assoc 'name anything-c-source-locate-file) (concat "Locate \"" locate-arg "\" in all directories")))
	  (setq anything-search-file-pattern locate-arg)
	  (anything '(anything-c-source-locate-file anything-c-source-search-file) pattern "pattern: " nil nil "*anything search file*"))
      (message "Please set string of which length is larger than %d."
	       anything-search-file-locate-minimum-string-number)
      (anything-search-file-locate (read-from-minibuffer "locate argument: ") all-dirs pattern))))


(defvar anything-search-file-find-directory nil)

(defvar anything-search-file-find-process-name "find-process")

(defun anything-search-file-find-kill-process ()
  (if (get-process anything-search-file-find-process-name) (delete-process anything-search-file-find-process-name)))

(defun anything-search-file-find-init ()
  (let ((dir (or anything-search-file-find-directory default-directory)))
    (apply 'start-process anything-search-file-find-process-name (anything-candidate-buffer 'global)
	   `("find" ,(expand-file-name dir))))
  (set-process-sentinel (get-process anything-search-file-find-process-name) (lambda (process event)))
  (set-process-filter (get-process anything-search-file-find-process-name)
		      (lambda (process output)
			(with-current-buffer (process-buffer process)
    			  (save-excursion
			    (goto-char (process-mark process))
			    (insert (replace-regexp-in-string (concat "^" (getenv "HOME")) "~" output))
			    (set-marker (process-mark process) (point)))))))

(defvar anything-c-source-find-file
  '((name . "Find")
    (init . anything-search-file-find-init)
    (cleanup . anything-search-file-find-kill-process)
    (candidates-in-buffer)
    (type . file)
    (delayed)
    (candidate-transformer anything-c-highlight-file-name)))

(defun anything-search-file-find (&optional dir pattern)
  (interactive "Dfind: ")
  (let ((anything-search-file-find-directory dir))
    (progn
      (setcdr (assoc 'name anything-c-source-find-file) (concat "Find in \"" anything-search-file-find-directory "\""))
      (anything '(anything-c-source-find-file anything-c-source-search-file) pattern "pattern: " nil nil "*anything search file*"))))

(defvar anything-c-source-search-file
  '((name . "Search File")
    (candidates . ("Locate in Home Directory: " "Locate other Name in Home Directory" 
		   "Locate in All Directories: " "Locate other Name in All Directories" "Find in Particular Directory"))
    (filtered-candidate-transformer . (lambda (candidates source)
					(mapcar (lambda (a)
						  (if (string-match ": $" a) (concat a "\"" (car (split-string anything-pattern " ")) "\"")
						    a)) candidates)))
    (action . (("Search File" .
		(lambda (arg)
		  (cond
		   ((string-match "^Locate in" arg)
		    (let* ((loc-arg (car (split-string anything-pattern " ")))
			   (pattern (replace-regexp-in-string (concat "^" loc-arg) "" anything-pattern)))
		      (setq pattern (replace-regexp-in-string "\\(^ *\\)\\|\\( *$\\)" "" pattern))
		      (cond
		       ((string-match "^Locate in Home Directory" arg)
			(anything-search-file-locate loc-arg nil pattern))
		       ((string-match "^Locate in All Directories" arg)
			(anything-search-file-locate loc-arg t pattern)))))
		   ((string-match "^Locate other" arg)
		    (let ((loc-arg (read-from-minibuffer "locate argument: ")))
		      (cond
		       ((string-match "^Locate other Name in Home Directory" arg)
			(anything-search-file-locate loc-arg nil))
		       ((string-match "^Locate other Name in All Directories" arg)
			(anything-search-file-locate loc-arg t)))))
		   ((string-match "^Find in Particular Directory" arg)
		    (let ((dir (read-directory-name "find: ")))
		      (anything-search-file-find dir anything-pattern)))
		   )))))
    (match identity)
    (delayed)))

(provide 'anything-search-file)
