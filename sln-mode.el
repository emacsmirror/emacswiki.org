;;; sln-mode.el --- Create a project-buffer using sln file
;;
;; Author:      Cedric Lallain <kandjar76@hotmail.com>
;; Version:     1.02
;; Keywords:    project buffer msvc sln vcproj viewer
;; Description: SLN File Project Viewer
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


;;; Commentary:
;; 

;; This is an add-on library for project-buffer-mode
;; 
;; This library provides a function to create a project-buffer
;; extracting the project information from a SLN file.
;;
;; To install it: just add the following lines to your init file:
;;   (autoload 'find-sln "sln-mode")
;;
;; find-sln is the command to execute to open a 'sln project'.
;;
;; Then check the project-buffer-mode for more documentation about
;; this mode.
;;

;; Note about the build/clean/run/debug actions:
;; 
;; The command line are different between a 2005 and a 2008 project;
;; by default it will use the 2005 configuration mode; use the prefix
;; argument to switch to the 2008 mode.
;;
;; -------

;; Extra note: 
;;
;; It doesn't currently support modifying the SLN file. It's currently
;; just a 'viewer'.  Note that it doens't have to stay that way if
;; people really need this feature. ;-)
;;
;; However it is possible to save the project as project-buffer
;; project files and reload it from there.
;;
;; -------


;;; History:
;; 
;; v1.00: First official release.
;; v1.01: Register the project local variable in `project-buffer-locals-to-save'
;; v1.02: Ask confirmation before cleaning the project.  
;;        Added refresh handler which reload the sln-file or the
;;        vcproj of the current project.


(require 'cl)
(require 'project-buffer-mode)


;;; Code:


;;
;; Customize data:
;;

(defgroup sln-mode nil
  "Customize the sln-mode library.")


(defcustom sln-mode-devenv-2005 "Devenv"
  "Path to Devenv 2005."
  :type 'string
  :group 'sln-mode
  )

(defcustom sln-mode-devenv-2008 "Devenv"
  "Path to Devenv 2008."
  :type 'string
  :group 'sln-mode
  )

;;
;; Helper function:
;;

(defvar sln-mode-solution-name nil
  "Local variable to store the solution name.")


(defun vcproj-extract-platforms (current-block)
  "Extract a list of platform from CURRENT-BLOCK."
  (unless (eq (car current-block) 'Platforms) (error "Expected a list like '(Platforms ...)"))
  (let ((data (cdddr current-block))
	cur ret)
    (while data
      (setq cur (pop data))
      (when (listp cur)
	(unless (eq (car cur) 'Platform) (error "Unknown id: '%S' expected 'Platform" (car cur)))
	(unless (eq (caaadr cur) 'Name)   (error "Unknown id: '%S' expected 'Name" (car cur)))
	(setq ret (cons (cdaadr cur) ret))))
    (reverse ret)))


(defun vcproj-extract-configurations (current-block)
  "Extract a list of configuration from CURRENT-BLOCK."
  (unless (eq (car current-block) 'Configurations) (error "Expected a list like '(Configurations ...)"))
  (let ((data (cdddr current-block))
	cur ret)
    (while data
      (setq cur (pop data))
      (when (listp cur)
	(unless (eq (car cur) 'Configuration) (error "Unknown id: '%S' expected 'Configuration" (car cur)))
	(let ((search-list (cadr cur))
	      name)
	  (while (and search-list (not name))
	    (let ((item (pop search-list)))
	      (setq name (and (eq (car item) 'Name) (cdr item)))))
	  (unless name (error "Unknown configuration name!"))
	  (setq ret (cons (car (split-string name "|")) ret)))))
    (reverse ret)))


(defun vcproj-extract-file(current-item)
  "Extract the relative path of the current file contain in CURRENT-ITEM"
  (unless (eq (car current-item) 'File) (error "Expected a list like '(File ...)"))
  (let ((data (cadr current-item))
	file)
    (while (and data (not file))
      (let ((cur (pop data)))
	(setq file (and (eq (car cur) 'RelativePath) (cdr cur)))))
    file))

	      
(defun vcproj-extract-filter-name(current-item)
  "Extract the filter name of the CURRENT-ITEM"
  (unless (eq (car current-item) 'Filter) (error "Expected a list like '(Filter ...)"))
  (let ((data (cadr current-item))
	filter)
    (while (and data (not filter))
      (let ((cur (pop data)))
	(setq filter (and (eq (car cur) 'Name) (cdr cur)))))
    filter))


(defun vcproj-extract-filter-list(current-item)
  "Extract the files/filter list attach to the current filter in CURRENT-ITEM"
    (unless (eq (car current-item) 'Filter) (error "Expected a list like '(Filter ...)"))
  (cddr current-item))


(defun vcproj-convert-file-list(file-list)
  "Convert FILE-LIST from a list '((\"virt-subfolder\" \"virt-subfolder\"...) \"full-path\") to a list '(\"virtual-folder\" \"full-path\")"
  (let (ret)
    (while file-list
      (let* ((node (pop file-list))
	     (vnode (car node))
	     (fullpath (replace-regexp-in-string "\\\\" "/" (cdr node)))
	     (file (file-name-nondirectory fullpath))
	     (virt-folder (if vnode "/" "")))
	(while vnode
	  (let ((item (pop vnode)))
	    (setq virt-folder (concat item virt-folder))))
	(push (cons (concat virt-folder file) fullpath) ret)))
    ret))


(defun vcproj-extract-files(current-block)
  "Extract a list of files from CURRENT-BLOCK"
  (unless (eq (car current-block) 'Files) (error "Expected a list like '(Files ...)"))
  (let ((data (cdddr current-block))
	cur ret stack folder)
    (push data stack)
    (while stack
      (let ((node (pop stack)))
        (pop folder)
	(while node
	  (let ((item (pop node)))
	    (when (listp item)
	      (cond ((eq (car item) 'Filter)
		     (push node stack)
		     (push (vcproj-extract-filter-name item) folder)
		     (setq node (vcproj-extract-filter-list item)))
		    ((eq (car item) 'File)
		     (push (cons folder (vcproj-extract-file item)) ret))
		    (t (error "Unknown data - id: %S" (car item)))))))))
    (vcproj-convert-file-list ret)))
 
  

(defun vcproj-extract-data(vcproj-file)
  "Extract files and directory from VCPROJ-FILE"
  (save-excursion
    (let* ((xml-tags (with-temp-buffer
		       (insert-file vcproj-file)
		       (xml-parse-region (point-min) (point-max))))
	   (vs-data (car xml-tags))
	   (vs-tags  (and (eq (car vs-data) 'VisualStudioProject)
			  (cdddr vs-data)))
	   ;;
	   vc-platforms
	   vc-configurations
	   vc-files
	   )
      ;; 
      (while vs-tags
	(let ((cur-block (pop vs-tags)))
	  (when (listp cur-block)
	    (let ((block-tag (car cur-block)))
	      (cond ((eq block-tag 'Platforms)
		     (setq vc-platforms (append (vcproj-extract-platforms cur-block) vc-platforms)))
		    ((eq block-tag 'ToolFiles))     ; Currently ignored
		    ((eq block-tag 'Configurations)
		     (setq vc-configurations (append (vcproj-extract-configurations cur-block) vc-configurations)))
		    ((eq block-tag 'References))    ; Currently ignored
		    ((eq block-tag 'Files)
		     (setq vc-files (append (vcproj-extract-files cur-block) vc-files)))
		    ((eq block-tag 'Globals))       ; Currently ignored
		    (t (error (format "Unknown block tag: %S" block-tag))))
	    ))))
      (list vc-platforms vc-configurations vc-files))))


(defun vcproj-update-file-folders(vc-files folder)
  "Update the folder of each files in VC-FILES adding FOLDER in front of them"
  (mapcar '(lambda (item)
	     (cons (car item)
		   (if (file-name-absolute-p (cdr item))
		       (cdr item)
		       (let ((rela-path (file-relative-name (expand-file-name (concat folder (cdr item)))))
			     (full-path (abbreviate-file-name (expand-file-name (concat folder (cdr item))))))
			 (if (> (length rela-path) (length full-path))
			     full-path
			     rela-path)))))
	  vc-files))


(defun sln-extract-projects(sln-file)
  "Extract projects from the SLN file"
  (save-excursion
    (with-temp-buffer
      (insert-file sln-file)
      (goto-char (point-min))
      (let ((result nil))
	(while (re-search-forward "Project(\"{[-A-Z0-9]+}\")[ 	]+=[ 	]+\"\\([^\"]+\\)\"[ 	]*,[ 	]+\"\\([^\"]+\\)\""
				  (point-max)  t)
	  (add-to-list 'result (cons (match-string-no-properties 1) (replace-regexp-in-string "\\\\" "/" (match-string-no-properties 2))) t))
	result))))

(defun sln-file-p (filename)
  "Check if FILENAME is a sln file."
  (or
   (null (file-name-extension filename))
   (string= (file-name-extension filename) "sln")))


(defun sln-action-handler-2005(action project-name project-path platform configuration)
  "Project-Buffer action handler."
  (let ((sln-cmd (cond ((eq action 'build) "Build")
		       ((eq action 'clean) "Clean")
		       ((eq action 'run)   "RunExit")
		       ((eq action 'debug) "DebugExe"))))
    (when (or (not (eq action 'clean))
	      (funcall project-buffer-confirm-function (format "Clean the project %s " project-name)))
      (compile
       (concat sln-mode-devenv-2005 " \"" sln-mode-solution-name "\" /" sln-cmd " \""  (concat configuration "|" platform) "\" /project \"" project-path "\"")))))

(defun sln-action-handler-2008(action project-name project-path platform configuration)
  "Project-Buffer action handler."
  (let* ((prj-str (concat "/Project \"" project-path "\" "))
	 (cfg-str (concat "\"" configuration "|" platform "\" "))
	 (sln-cmd (cond ((eq action 'build) (concat "/Build " cfg-str))
			((eq action 'clean) (concat "/Clean " cfg-str))
			((eq action 'run)   (concat "/ProjectConfig " cfg-str ))
			((eq action 'debug) (concat "/ProjectConfig " cfg-str )))))
    (when (or (not (eq action 'clean))
	      (funcall project-buffer-confirm-function (format "Clean the project %s " project-name)))
      (compile
       (concat sln-mode-devenv-2008 " \"" sln-mode-solution-name "\" "
	       prj-str sln-cmd)))))


(defun sln-add-vcproj-project(project-name vcproj-file)
  "Add a new project named PROJECT-NAME to the project bufffer,
and use the content of VCPROJ-FILE to populate it."
  (let* ((project-dir (file-name-directory vcproj-file))
	 (project-data (and (file-exists-p vcproj-file)
			    (vcproj-extract-data vcproj-file)))
	 (platforms (car project-data))
	 (configurations (cadr project-data)))
    ;; Create a project node / update its platform and build configuration...
    (project-buffer-insert project-name 'project vcproj-file project-name)
    (project-buffer-set-project-platforms project-name platforms)
    (project-buffer-set-project-build-configurations project-name configurations)
    (when project-data
      (let ((files (vcproj-update-file-folders (caddr project-data) project-dir)))
	(while files
	  (let ((file (pop files)))
	    ;; then insert each project file into the buffer
	    (project-buffer-insert (car file) 'file (cdr file) project-name)))))))


(defun sln-refresh-handler(project-list content)
  "Refresh handler.
Base on CONTENT, it will either reload the sln file and recreate
the projects; or just refresh the selected projects."
  (when (and project-list
	     (funcall project-buffer-confirm-function 
		      (if (eq content 'all)
			  (format "Reload %s " sln-mode-solution-name)
			  (format "Reload %s " (project-buffer-get-project-path (car project-list))))))
    (if (and (eq content 'all)
	     (file-exists-p sln-mode-solution-name))
	;; Clear the whole buffer and recreate each projects:
	(let ((sln-projects (sln-extract-projects sln-mode-solution-name)))
	  (project-buffer-erase-all project-buffer-status)
	  (while sln-projects
	    (let ((current (pop sln-projects)))
	      (sln-add-vcproj-project (car current) (cdr current)))))
	;; Delete the specified projects and recreate them:
	(while project-list
	  (let* ((project-name (pop project-list))
		 (vcproj-file  (project-buffer-get-project-path project-name)))
	    (when (file-exists-p vcproj-file)
	      (project-buffer-delete-project project-name)
	      (sln-add-vcproj-project project-name vcproj-file)))))))


(defun make-sln-project-buffer(sln-file &optional using2008)
  "Create a project buffer interpreting SLN-FILE to populate it."
  (let ((buffer (generate-new-buffer (concat "ms:" (file-name-nondirectory sln-file))))
	(sln-projects (sln-extract-projects sln-file)) ; list of proj-nane / project file
	)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      ;; Make sure the buffer path match the project's path
      (cd (file-name-directory sln-file))
      ;; Turn on the project-buffer-mode
      (project-buffer-mode)
      (make-local-variable 'sln-mode-solution-name)
      (add-to-list 'project-buffer-locals-to-save 'sln-mode-solution-name)
      (setq sln-mode-solution-name (file-name-nondirectory sln-file))
      (if using2008
	  (add-hook 'project-buffer-action-hook 'sln-action-handler-2008 nil t)
	  (add-hook 'project-buffer-action-hook 'sln-action-handler-2005 nil t))
      (add-hook 'project-buffer-refresh-hook 'sln-refresh-handler)
      ;;
      (while sln-projects
	;; For every project reference in the SLN file,
	(let ((current (pop sln-projects)))
	  (sln-add-vcproj-project (car current) (cdr current)))
	))))


;;
;; Interactive command:
;;

;;;###autoload
(defun find-sln(solution-name &optional using2008)
  "Open an sln file and create a project buffer using the data in it."
  (interactive
   (list (read-file-name "SLN file: " nil nil t nil 'sln-file-p)
	 current-prefix-arg))
  (when (and solution-name
	     (> (length solution-name) 0))
    (make-sln-project-buffer solution-name using2008)))


;;

(provide 'sln-mode)

;;; sln-mode.el ends here
