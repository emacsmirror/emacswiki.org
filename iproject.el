;;; iproject.el --- Interactive Project Mode
;;
;; Author:      Cedric Lallain <kandjar76@hotmail.com>
;; Version:     1.0
;; Keywords:    interactive project buffer makefile filesystem management
;; Description: Interactive Project Extension For Project-Buffer-Mode
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

;; This is an add-on library for project-buffer-mode.
;;
;; iproject stands for Interactive Project; based on the
;; project-buffer-mode; it provides an interactive way to create
;; projects.
;;
;; Simply creates a new project using `iproject-new' (C-x p n); then
;; add new projects using `iproject-add-project' (C-c n).
;;
;; Once the project is created it is possible to add extra files to
;; the current project using `iproject-add-files-to-current-project'
;; (C-c +).
;;
;; iproject will ask the user for command lines (with default values
;; based on the selected project type) to run each user action which
;; are: build/clean/run/debug and update.  These command lines will be
;; stored in the project but also saved with the project.
;;
;; HOW TO INSTALL IT:
;; 
;; Just add to your .emacs:
;;   (require 'iproject)
;;   (iproject-key-binding)
;;
;; KEY BINDINGS IN THE IPROJECT BUFFER:
;;
;; C-c n   to add new project
;; C-c +   to add file to an existing project
;; C-c C-r to revert the project
;; C-x C-w to write the project
;; C-x C-s to save the project
;;
;; as well as all standard project-buffer-mode key-bindings.
;;
;; GLOBAL KEY BINDINGS:
;;
;; C-x p n to create a new iproject
;; C-x p f to load a project file
;;


;;; History:
;;
;; v1.0: First official release.
;;


(require 'project-buffer-mode)


;;; Code:


;;
;;  Global configuration variable:
;;

(defvar iproject-filters
  '((c++       ("\\.[cChH][pPxX+][pPxX+]$" "\\.[cChH]$" "\\.[iI][nN][lL]$" "\\.[cC][cC]$"))
    (c         ("\\.[cChH]$" "\\.[iI][nN][lL]$" "\\.[cC][cC]$"))
    (elisp     ("\\.el$"))
    (perl      ("\\.pl$"))
    (ruby      ("\\.rb$"))
    (sharp     ("\\.[cjf]s$"))
    (python    ("\\.py$"))
    (smalltalk ("\\.st$"))
    (haskell   ("\\.hs$"))
    (ocaml     ("\\.ml$"))
    (lisp      ("\\.cl$"))
    (awk       ("\\.awk$"))
    (java      ("\\.java$" "\\.js$"))
    (cg        ("\\.cg\\(?:fx\\)?$"))
    (web       ("\\.htm\\(?:l\\)?$" "\\.xml" "\\.php$" "\\.js$" "\\.css$"))
    (custom    (nil)))
  "List of the different file filters."
)

(defvar iproject-project-type
  '((makefile ("\\.mak$" "Makefile$")
	      ((build . "make -C {root} CONFIG={build}")
	       (clean . "make -C {root} clean CONFIG={build}")))
    (cmake    ("CMakeLists.txt")
	      ((build . "make -C {root} CONFIG={build}")
	       (clean . "make -C {root} clean CONFIG={build}")))
    (jam      ("Jamfile\\(?:s\\)?$" "Jamrules$" "Jambase$" "Jamroot$")
	      ((build . "jam -a {project}")
	       (clean . "jam clean -a {project}")))
    (scons    ("SConstruct$" "Sconscript$")
	      ((build . "scons")
	       (clean . "scons --clean")))
    (dmconfig ("build.dmc$")
	      ((build . "make {platform}.{project}-{build}.build")
	       (clean . "make {platform}.{project}-{build}.clean")
	       (run . "make {platform}.{project}-{build}.run")
	       (debug . "make {platform}.{project}-{build}.debug")))
    (cabal    ("\\.cabal$")
	      ((build . "cabal build")
	       (clean . "cabal clean")))
    (any      (".*$")
	      ((build . "")))
    (blank    nil
	      ((build . ""))))
  "List of the different project type.

Each project type is a list of the following format:
  (symbol matching-regexp (action-string-list)) where
  action-string-list is a set of 4 strings representing the default
  command to 'build' 'clean' 'run' and 'debug'.
  the following wild cards can be use in each action string:
   {build}    the current selected build version
   {platform} the current selected platform
   {project}  name of the project
   {projfile} path of the project's main file
   {root}     root folder of the project"
)

(defvar iproject-ignore-folder
  '(".git" ".svn" "bzr" ".hg" "CVS" ".CVS" "build" "lib" "Debug" "Release")
  "List of folder to ignore during the recursive search.")


;;
;; History:
;;


(defvar iproject-project-type-history nil)
(defvar iproject-file-filter-history nil)
(defvar iproject-file-filter-query-history nil)
(defvar iproject-file-filter-regexp-history nil)
(defvar iproject-file-filter-extension-list-history nil)
(defvar iproject-project-name-history nil)
(defvar iproject-platforms-history nil)
(defvar iproject-build-configurations-history nil)
(defvar iproject-action-commands-history nil)
(defvar iproject-last-base-directory-history nil)


;;
;;  Local variables:
;;

(defvar iproject-last-project-type-choosen "makefile")
(defvar iproject-last-filter-type-choosen "c++")
(defvar iproject-last-file-filter-query-mode-choosen "regexp")
(defvar iproject-last-file-filter-regexp-choosen nil)
(defvar iproject-last-file-extension-list-choosen nil)
(defvar iproject-platform-list nil)
(defvar iproject-build-configuration-list nil)
(defvar iproject-last-base-directory-choosen nil)


;;
;;  Functions:
;;

(defun iproject-choose-project-type()
  "Request and return the selected project type"
  (let* ((project-type-string (completing-read (format "Project Type [default %s]: " iproject-last-project-type-choosen)
					       iproject-project-type nil t nil 'iproject-project-type-history iproject-last-project-type-choosen))
	 (project-type (intern project-type-string)))
    (setq iproject-last-project-type-choosen project-type-string)
    (assoc project-type iproject-project-type)))

(defun iproject-shorten-string(str max-lgt)
  "If the length of STR is greater than MAX-LGT; shorten the string adding '...' at the end."
  (if (> (length str) max-lgt)
      (concat (substring str 0 (- max-lgt 3)) "...")
      str))

(defun iproject-choose-file-filter()
  "Read the file filter."
  (let* ((filter-type-string (completing-read (format "Filter Type [default %s]: " iproject-last-filter-type-choosen)
					       iproject-filters nil t nil 'iproject-file-filter-history iproject-last-filter-type-choosen))
	 (filter-type (intern filter-type-string)))
    (setq iproject-last-filter-type-choosen filter-type-string)
    (if (not (eq filter-type 'custom))
	;; If not custom: return the selected file-filter:
	(assoc filter-type iproject-filters)
	;; In case of custom file filter:
	;; Let's first ask how to specify the filter:
	(let* ((query-mode-string (completing-read (format "Enter the file system query mode (regexp, file-extension) [default %s]: " iproject-last-file-filter-query-mode-choosen)
						   '("regexp" "file-extension") nil t nil 'iproject-file-filter-query-history iproject-last-file-filter-query-mode-choosen))
	       (query-mode (intern query-mode-string)))
	  (setq iproject-last-file-filter-query-mode-choosen query-mode-string)
	  (cond ((eq query-mode 'regexp)
		 ;; A regexp:
		 (let* ((def-string (if iproject-last-file-filter-regexp-choosen
					(concat " [default " (iproject-shorten-string iproject-last-file-filter-regexp-choosen 9) "]")
					""))
			(file-filter-regexp (read-from-minibuffer (format "Enter the file filter regexp%s: " def-string)
								  nil nil nil 'iproject-file-filter-regexp-history)))
		   (if (= (length file-filter-regexp) 0)
		       (setq file-filter-regexp iproject-last-file-filter-regexp-choosen)
		       (setq iproject-last-file-filter-regexp-choosen file-filter-regexp))
		   (list 'custom (list file-filter-regexp))))
		((eq query-mode 'file-extension)
		 ;; A list of file extension:
		 (let* ((def-string (if iproject-last-file-extension-list-choosen
					(concat " [default " (iproject-shorten-string iproject-last-file-extension-list-choosen 9) "]")
					""))
			(file-extension-list (read-from-minibuffer (format "Enter the list of extension separated by spaces%s: " def-string)
								   nil nil nil 'iproject-file-filter-extension-list-history)))
		   (if (= (length file-extension-list) 0)
		       (setq file-extension-list iproject-last-file-extension-list-choosen)
		       (setq iproject-last-file-extension-list-choosen file-extension-list))
		   (list 'custom (list (concat "\\." (regexp-opt (split-string file-extension-list)) "$")))))
		(t (error "Unknown Query Mode")))))))


(defun iproject-collect-files(root-folder file-filter-list &optional ignore-folders)
  "Parse ROOT-FOLDER and its sub-folder and create a list of full path filename matching one of the regexp of FILE-FILTER-LIST.
The folder defined inside in IGNORE-FOLDERS will be skipped."
  (let ((dir-list (directory-files-and-attributes root-folder t))
	(ign-reg  (concat (regexp-opt ignore-folders) "$"))
	file-list)
    (while dir-list
      (let* ((cur-node (pop dir-list))
	     (fullpath (car cur-node))
	     (is-dir   (eq (car (cdr cur-node)) t))
	     (is-file  (not (car (cdr cur-node))))
	     (basename (file-name-nondirectory fullpath)))
	(cond
	 ;; if the current node is a directory different from "." or "..", all it's file gets added to the list
	 ((and is-dir
	       (not (string-equal basename "."))
	       (not (string-equal basename ".."))
	       (or (not ignore-folders)
		   (not (string-match ign-reg basename))))
	       (setq dir-list (append dir-list (directory-files-and-attributes fullpath t))))
	 ;; if the current node is a file
	 (is-file
	  ;; check against the file filter, if it succeed: add the file to the file-list
	  (when (some '(lambda (item) (string-match item basename)) file-filter-list)
	    (setq file-list (cons fullpath file-list)))
	  ))))
    file-list))


(defun iproject-generate-user-data(action-string-list
				     project-name
				     project-main-file
				     project-root-folder)
  "Generate the project's user data based from ACTION-STRING-LIST.
ACTION-STRING-LIST is a list of string; each of them corresponding to the project actions.
This function returns a assoc-list of assoc-list such as:
  (cdr (assoc buildconfig (cdr (assoc platform data)))) should returns a list of user actions.

In each action string list may contain the following wildcard
which will be replaced by their respective value:
   {build}    the current selected build version
   {platform} the current selected platform
   {project}  name of the project
   {projfile} path of the project's main file
   {root}     root folder of the project"

  (let ((platform-list iproject-platform-list)
	user-data)
    (while platform-list
      (let ((current-platform (pop platform-list))
	    (build-config-list iproject-build-configuration-list)
	    bc-list)
	(setq user-data (cons (cons current-platform
				    (progn (while build-config-list
					     (let ((current-build-config (pop build-config-list)))
					       (setq bc-list (cons (cons current-build-config
									 (mapcar (lambda (action-node)
										   (let* ((action-string (cdr action-node))
											  (repl1 (replace-regexp-in-string "{build}"    current-build-config  action-string))
											  (repl2 (replace-regexp-in-string "{platform}" current-platform      repl1))
											  (repl3 (replace-regexp-in-string "{project}"  project-name          repl2))
											  (repl4 (replace-regexp-in-string "{projfile}" project-main-file     repl3))
											  (repl5 (replace-regexp-in-string "{root}"     project-root-folder   repl4)))
										     (cons (car action-node) repl5)))
										 action-string-list))
								   bc-list))
					       ))
					   bc-list)
				      )
			      user-data))))
    user-data))


(defun iproject-action-handler(action project-name project-path platform configuration)
  (let* ((user-data (project-buffer-get-project-user-data project-name))
	 (query-string (concat (upcase-initials (format "%s" action)) " command: "))
	 user-command)
    ;; user data's format is: '((platform1 (config1 . ((action1 . "cmd") (action2 . "cmd"))) (config2 ...)) (platform2...))
    ;; platform-data:         '(curplat (config1 . ((act...))) (config2 ...))
    ;; config-data:           '(config1 (act1 ...) (act2...))
    ;; action-data:           '(action . "cmd")
    (if user-data
      (let ((platform-data (assoc platform user-data)))
	(if platform-data
	    (let ((config-data (assoc configuration (cdr platform-data))))
	      (if config-data
		  (let ((action-data (assoc action (cdr config-data))))
		    (if action-data
			(progn (setq user-command (read-from-minibuffer query-string (cdr action-data) nil nil 'iproject-action-commands-history))
			       (setcdr action-data user-command))
			(progn (setq user-command (read-from-minibuffer query-string nil nil nil 'iproject-action-commands-history))
			       (setcdr config-data (acons action user-command (cdr config-data))))))
		  (progn (setq user-command (read-from-minibuffer query-string nil nil nil 'iproject-action-commands-history))
			 (setcdr platform-data (acons configuration (acons action user-command nil) (cdr platform-data))))))
	    (progn (setq user-command (read-from-minibuffer query-string nil nil nil 'iproject-action-commands-history))
		   (setcdr user-data (copy-alist user-data))
		   (setcar user-data (cons platform (acons configuration (acons action user-command nil) nil))))))
      (progn (setq user-command (read-from-minibuffer query-string nil nil nil 'iproject-action-commands-history))
	     (project-buffer-set-project-user-data project-name (acons platform (acons configuration (acons action user-command nil) nil) nil))))
    (compile user-command)))


;;
;;  User command:
;;


(defun iproject-add-project(&optional project-type project-main-file project-root-folder project-name file-filter)
  "Select a FOLDER, a MAIN-FILE and a FILE-FILTER, then add all
files under the current folder and sub-folder matching the
FILE-FILTER will be added to the project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (when (interactive-p)
    ;; Read the project-type
    (unless project-type
      (setq project-type (iproject-choose-project-type)))
    ;; Read the project-main-file (if the project's type is 'blank' there is no root filename)
    (unless project-main-file
      (when (nth 1 project-type)
	(let* ((project-filter (nth 1 project-type))
	       (project-predicate (lambda (filename)
				    (and (not (string-equal filename "./"))
					 (not (string-equal filename "../"))
					 (or (file-directory-p filename)
					     (some '(lambda (item) (string-match item filename)) project-filter))))))
	  (while (or (not project-main-file)
		     (file-directory-p project-main-file)
		     (not (funcall project-predicate project-main-file)))
	    (let ((def-dir (and project-main-file (file-directory-p project-main-file) project-main-file)))
	      (setq project-main-file (read-file-name "Project Main File: " def-dir nil t nil project-predicate))
	      )))))
    ;; Read the project-root-folder:
    (unless project-root-folder
      (let ((def-dir (if project-main-file
			 (file-name-directory project-main-file)
			 default-directory)))
	(while (or (not project-root-folder)
		   (= (length project-root-folder) 0))
	  (setq project-root-folder (read-directory-name "File Search - Root Folder: " def-dir def-dir t)))
	(unless (string-equal (substring project-root-folder -1) "/")
	  (setq project-root-folder (concat project-root-folder "/")))
	))
    ;; Read the project name:
    (unless project-name
      (while (not project-name)
	(setq project-name (read-from-minibuffer "Project Name: "
						 (file-name-nondirectory (substring project-root-folder 0 -1))
						 nil nil 'iproject-project-name-history))
	(when (project-buffer-project-exists-p project-name)
	  (message "Project %s already exists!" project-name)
	  (sit-for 2)
	  (setq project-name nil))
	))
    ;; Read the file-filter:
    (unless file-filter
      (setq file-filter (iproject-choose-file-filter)))
    )

  (let (file-list user-data project-settings)
    ;;
    ;; Collect the project's file
    ;;
    (setq file-list (iproject-collect-files project-root-folder (nth 1 file-filter) iproject-ignore-folder))

    ;;
    ;; Populate the project-buffer-mode:
    ;;

    ;; Generate the project node's user-data:
    (setq user-data (iproject-generate-user-data (nth 2 project-type)
						   project-name
						   project-main-file
						   project-root-folder))

    ;; Create the initial project settings:
    ;; project settings is a list of lists, each sub list should follow the following format:
    ;;   (root-project-folder root-file-folder file-filter-list ignore-folder-list)
    (setq project-settings (list (list "" project-root-folder (nth 1 file-filter) iproject-ignore-folder)))

    ;; Add the project node
    (project-buffer-insert project-name 'project project-main-file project-name)
    (project-buffer-set-project-build-configurations project-name iproject-build-configuration-list)
    (project-buffer-set-project-platforms            project-name iproject-platform-list)
    (project-buffer-set-project-user-data            project-name user-data)
    (project-buffer-set-project-settings-data        project-name  project-settings)

    ;; Add each individual files to the project:
    (mapcar (lambda (name)
	      (let* ((relative-path (file-relative-name name))
		     (full-path     (abbreviate-file-name name))
		     (file-name     (if (> (length relative-path) (length full-path)) full-path relative-path))
		     (proj-name     (substring name (length (expand-file-name project-root-folder)) (length name))))
		(project-buffer-insert proj-name 'file  file-name project-name)))
	    file-list)
    ;; Add the project's main file to the project:
    (when project-main-file
      (project-buffer-insert (file-name-nondirectory project-main-file) 'file  project-main-file project-name))
  ))


(defun iproject-uniquify-name(file-name file-path project)
  "Returns a uniq name based on FILE-NAME to be inserted inside PROJECT.
Returns nil if the FILE-NAME is already in PROJECT."
  (let (cur-name)
    ;; Check: if file-name ends with " (N)" but not file-path; we'll remove it.
    (let ((ndx (string-match " ([0-9]+)$" file-name)))
      (if (and ndx (not (string-match " ([0-9]+)$" file-path))) ;; I'm ignoring the fact the number may be different :)
	  (setq cur-name (substring file-name 0 ndx))
	  (setq cur-name file-name)))
    ;; Check name conflict:
    (let ((exists        (project-buffer-exists-p cur-name project))
	  (existing-path (project-buffer-get-file-path cur-name project))
	  (count 2))
      (when exists
	(if (and existing-path (string-equal file-path existing-path))
	    (setq cur-name nil) ; if the file is already present, skip it (note: the search is very basic; it is possible to trick the system and add a file twice...)
	    (setq cur-name (concat cur-name " (1)"))))
      (while (and exists cur-name)
	(setq exists        (project-buffer-exists-p cur-name project))
	(setq existing-path (project-buffer-get-file-path cur-name project))
	(when exists
	  (if (and existing-path (string-equal file-path existing-path))
	      (setq cur-name nil) ; if the file is already present, skip it
	      (setq cur-name (concat (substring proj-name 0 -2) (format "%i)" count))
		    count (1+ count)))))
      cur-name)))


(defun iproject-add-file-list-to-current-project(current-project base-virtual-folder root-folder file-list)
  "Add files contained in FILE-LIST in the CURRENT-PROJECT
prefixing the project's filename with BASE-VIRTUAL-FOLDER."
  (mapcar (lambda (name)
	    (let* ((relative-path (file-relative-name name))
		   (full-path     (abbreviate-file-name name))
		   (file-name     (if (> (length relative-path) (length full-path)) full-path relative-path))
		   (proj-name     (iproject-uniquify-name (concat base-virtual-folder (substring name (length (expand-file-name root-folder)) (length name)))
							  file-name current-project)))
		  (when proj-name
		    (project-buffer-insert proj-name 'file  file-name current-project))))
	      file-list))


(defun iproject-add-files-to-current-project(&optional root-folder file-filter base-virtual-folder)
  "Add extra files to the current project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((current-project (project-buffer-get-current-project-name)))
    (unless current-project (error "No current project found"))
    (when (interactive-p)
      ;; Read the root-folder:
      (unless root-folder
	(while (or (not root-folder)
		   (= (length root-folder) 0))
	    (setq root-folder (read-directory-name "File Search - Root Folder: " nil nil t)))
	(unless (string-equal (substring root-folder -1) "/")
	  (setq root-folder (concat root-folder "/"))))
      ;; Read the file-filter:
      (unless file-filter
	(setq file-filter (iproject-choose-file-filter)))
      ;; Read the base-virtual-path:
      (unless base-virtual-folder
	(let* ((def-string (if iproject-last-base-directory-choosen
			       (concat " [default " (iproject-shorten-string iproject-last-base-directory-choosen 9) "]")
			       "")))	       
	  (setq base-virtual-folder (read-from-minibuffer (format "Enter the base directory in the project%s: " def-string)
							  nil nil nil 'iproject-last-base-directory-history))))
      )

    (let (file-list user-data project-settings)
      ;; Collect the project's file
      (setq file-list (iproject-collect-files root-folder (nth 1 file-filter) iproject-ignore-folder))

      ;; Make sure the base-virtual-folder doesn't start with a '/' and end with one:
      (when (and (> (length base-virtual-folder) 0)
		 (string-equal (substring base-virtual-folder 0 1) "/"))
	(setq base-virtual-folder (substring base-virtual-folder 1)))
      (unless (or (= (length base-virtual-folder) 0)
		  (string-equal (substring base-virtual-folder -1) "/"))
	(setq base-virtual-folder (concat base-virtual-folder "/")))
      
      ;; Update the project settings:
      (setq project-settings (cons (list base-virtual-folder root-folder (nth 1 file-filter) iproject-ignore-folder)
				   (project-buffer-get-project-settings-data current-project)))
      (project-buffer-set-project-settings-data current-project project-settings)
     
      ;; Add each individual files to the project:
      (iproject-add-file-list-to-current-project current-project base-virtual-folder root-folder file-list)
      )))


(defun iproject-move-files-within-project(file-list folder-name)
  "Move the file present in FILE-LIST into the folder FOLDER-NAME.
FILE-LIST should be a list of list '(file-name file-path project)."
  (let ((virtual-folder folder-name))
    ;; Make sure the folder name doesn't start with a '/' but ends with one.
    (when (and (> (length virtual-folder) 0)
	       (string-equal (substring virtual-folder 0 1) "/"))
      (setq virtual-folder (substring virtual-folder 1)))
    (unless (or (= (length virtual-folder) 0)
		(string-equal (substring virtual-folder -1) "/"))
      (setq virtual-folder (concat virtual-folder "/")))
    ;; Let's delete all files from the project:
    (mapcar (lambda (file-node)
		(project-buffer-delete-file (car file-node) (nth 2 file-node) t))
	    file-list)
    ;; Re-add each node making sure they are uniq:
    (mapcar (lambda (file-node)
	      (let ((file-name (nth 0 file-node))
		    (file-path (nth 1 file-node))
		    (project   (nth 2 file-node)))
		(setq file-name (iproject-uniquify-name (concat virtual-folder (file-name-nondirectory file-name))
							file-path project))
		(when file-name
		  (project-buffer-insert file-name 'file file-path project))))
	    file-list)))
  
  
(defun iproject-move-marked-files-or-current-file-within-project(&optional folder-name)
  "Move the marked files into an specified project's folder."
  (interactive)
  (let* ((node-list    (project-buffer-get-marked-node-list))
	 (current-node (unless node-list (project-buffer-get-current-file-data))))
    (unless (or node-list current-node) (error "No marked files / No current file found"))
    (unless folder-name
      (let ((def-string (if iproject-last-base-directory-choosen
			    (concat " [default " (iproject-shorten-string iproject-last-base-directory-choosen 9) "]")
			    ""))
	    (file-str (if node-list (if (> (length node-list) 1) "marked files" "marked file") "current file")))
	(setq folder-name (read-from-minibuffer (format "Enter the base directory to move the %s into%s: " file-str def-string)
						nil nil nil 'iproject-last-base-directory-history))))
    (unless node-list
      (setq node-list (list current-node)))
    (iproject-move-files-within-project node-list folder-name)))


(defun iproject-refresh-project(project)
  "Reparse the directories associated to PROJECT, add the new files to it."
  (let ((settings (project-buffer-get-project-settings-data project)))
    (while settings
      (let ((current (pop settings))
	    file-list)
	(when (file-directory-p (nth 1 current))
	  ;; currest is a list (root-project-folder root-file-folder file-filter-list ignore-folder-list)
	  (setq file-list (iproject-collect-files (nth 1 current) (nth 2 current) (nth 3 current)))
	  ;; Add each individual files to the project:
	  (iproject-add-file-list-to-current-project project (nth 0 current) (nth 1 current) file-list)
	  )))))


(defun iproject-refresh-handler(project-list content)
  "Refresh all projects, check if there are new files to be added.
PROJECT-LIST is a list containing the project's names"
  (when (and project-list
	     (funcall project-buffer-confirm-function 
		      (if (cdr project-list) "Reparse the projects' directoriess "
			  (format "Reparse %s's directories " (car project-list)))))
    (while project-list
      (iproject-refresh-project (pop project-list)))))


(defun iproject-setup-local-key()
  "Define a local key-bindings."
  (local-set-key [(control ?c) ?n] 'iproject-add-project)
  (local-set-key [(control ?c) ?+] 'iproject-add-files-to-current-project)
  (local-set-key [(control ?c) ?m] 'iproject-move-marked-files-or-current-file-within-project)

  (local-set-key [(control ?c) (control ?r)] 'project-buffer-revert)
  (local-set-key [(control ?x) (control ?s)] 'project-buffer-save-file)
  (local-set-key [(control ?x) (control ?w)] 'project-buffer-write-file))


;;
;;  User commands:
;;


;;;###autoload
(defun iproject-new (name root-folder)
  "Create a iproject buffer named NAME with a `default-directory' set to ROOT-FOLDER."
  (interactive "sProject Buffer Name: \nDRoot Folder: ")
  (let ((buffer (generate-new-buffer (concat "ipb:" name))))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (cd root-folder)
      (project-buffer-mode)
      ;; local variables:
      (make-local-variable 'iproject-last-project-type-choosen)
      (make-local-variable 'iproject-last-filter-type-choosen)
      (make-local-variable 'iproject-last-file-filter-query-mode-choosen)
      (make-local-variable 'iproject-last-file-filter-regexp-choosen)
      (make-local-variable 'iproject-last-file-extension-list-choosen)
      (make-local-variable 'iproject-platform-list)
      (make-local-variable 'iproject-build-configuration-list)
      ;; register the local variable to be saved:
      (add-to-list 'project-buffer-locals-to-save 'iproject-last-project-type-choosen)
      (add-to-list 'project-buffer-locals-to-save 'iproject-last-filter-type-choosen)
      (add-to-list 'project-buffer-locals-to-save 'iproject-last-file-filter-query-mode-choosen)
      (add-to-list 'project-buffer-locals-to-save 'iproject-last-file-filter-regexp-choosen)
      (add-to-list 'project-buffer-locals-to-save 'iproject-last-file-extension-list-choosen)
      (add-to-list 'project-buffer-locals-to-save 'iproject-platform-list)
      (add-to-list 'project-buffer-locals-to-save 'iproject-build-configuration-list)
      ;; ask for the platform list:
      (setq iproject-platform-list            (split-string (read-from-minibuffer "Enter the list of platforms separated by spaces: "
										  (if iproject-platforms-history (car iproject-platforms-history) (format "%s" system-type))
										  nil nil 'iproject-platforms-history)))
      (setq iproject-build-configuration-list (split-string (read-from-minibuffer "Enter the list of build configurations separated by spaces: "
										  (if iproject-build-configurations-history (car iproject-build-configurations-history) "release debug")
										  nil nil 'iproject-build-configurations-history)))
      ;;
      (iproject-setup-local-key)
      (add-hook 'project-buffer-post-load-hook 'iproject-setup-local-key nil t)
      (add-hook 'project-buffer-action-hook    'iproject-action-handler  nil t)
      (add-hook 'project-buffer-refresh-hook   'iproject-refresh-handler nil t)
      )))


;;;###autoload
(defun iproject-key-binding ()
  "Setup some global key-bindings."
  (define-key global-map [(control x) (?p) (?n)] 'iproject-new)
  (define-key global-map [(control x) (?p) (?f)] 'project-buffer-find-file))


;;

(provide 'iproject)

;;; iproject.el ends here
