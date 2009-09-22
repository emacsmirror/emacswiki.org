;;; project-buffer-mode.el --- Generic mode to browse project file
;;
;; Author:      Cedric Lallain <kandjar76@hotmail.com>
;; Version:     1.22
;; Keywords:    project mode buffer viewer generic
;; Description: Generic mode to handle projects.
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

;; project-buffer-mode is a generic mode to handle projects.  It
;; provides an easy and intuitive way to interact with the multiple
;; projects with in one buffer.
;;
;;
;; Three project implementation uses this mode:
;; - fsproject  - which creates a project based on the file system
;; - sln-mode   - which parses a sln files and create a project representing it
;; - iproject   - which stands for Interactive Project allowing the user to easily add/remove projects.
;;
;; Two generic extensions are also available:
;; - project-buffer-mode+ - which allow to run user actions such as build/clean.. commands from the project files
;; - project-buffer-occur - which provides a function to search in project files and list all occurrences
;;
;; Key features:
;; - find files based on regular expression
;; - four different view modes
;; - advanced 'search in files' system
;; - notion of master project to launch build/clean/run/debug and update.
;; - intuitive key bindings (at least I hope)
;; - full save/load of a project including hooks and local configuration.
;; - mouse support to expand/collapse a folder or open a file


;;; Commentary:
;;

;; project-buffer-mode provides a generic way to handle multiple
;; projects in a buffer.
;;
;; A Project is defined by:
;;  - its name
;;  - its main file (Makefile, Jam, Scons...)
;;  - a build configuration list (Debug, Release, ...)
;;  - a platform list (Win32, PocketPC, Linux...)
;;  - and obviously a list of files.
;;
;;
;; QUICK FIND FILE USING REGEXP:
;;
;; Through a hierarchical view, the project-buffer mode provides an
;; very easy and intuitive way to search for a particular files (key:
;; '/', then 'n' or 'p' to go to the next or previous matching
;; result).  Note: press 'q' to cancel the research.
;;
;; Opening the current is a simple as pressing <enter> or
;; 'o' to open it in another window.
;; Press 'f' if you want to open all marked files.
;;
;;
;; FOUR DIFFERENT VIEW-MODE:
;;
;; Four different view-modes are currently supported:
;; - folder-view (<default>)
;; - flat-view
;; - folder-hidden-view
;; - marked-view
;;
;; It's possible to switch between them using 'c v'.
;;
;; The first three modes show the project with their associated files:
;; - folder-view shows a tree-view of files.
;; - flat-view shows the list of the files prefixed by their folder
;; - folder-hidden-view shows the list of just the file names, next
;; to it, it displays the real path for each of them.
;;
;; The final view mode named marked-view shows only the list of marked
;; files, prefixed by their project and folders.
;;
;;
;; MARKING FILE MATCHING A REGEXP:
;;
;; Files can be marked/unmarked individually, but you can also easily
;; mark all files whose names are matching a regular expression ('/'
;; then 'm').
;; Note: using the mark/unmark command in front of a folder of a
;; project results in marking every files which belong to this folder
;; or this project.
;;
;;
;; ADVANCED SEARCH IN FILES SYSTEM:
;;
;; The search in files functionality comes with three different behaviors:
;; - Narrow the marked files (<default>)
;; - All files
;; - Current project
;;
;; Before talking about the "Narrow the marked files" behavior which
;; is the default one; let's quickly go throught the others two:
;;
;; - If the search behavior is set to "All files", the search-in-files
;; command ('s') will do a search-regexp in files for each unmarked
;; files (all projects) and mark the ones which contain the regexp.
;;
;; - If the search behavior is set to "Current Project" the
;; search-in-files will do search-regexp in files for each unmarked
;; file contained in the current project.  The current project being
;; defined by the position of the cursor.  Again, each matching files
;; will be marked.
;;
;; Note: it is possible to have the search-regexp in file unmarking
;; the files instead by using the prefix argument (C-u).
;;
;; Finally in case the search behavior is set to "Narrow the marked
;; files": if no files are actually marked, it will behave the same
;; way as the "All files" behavior.  In case some files are marked, it
;; will only perform the "search-regexp in files" in the marked files,
;; unmarking the ones which don't contain the regular expression.
;;
;; This provide an easy way to narrow/refine some research.
;;
;; The search behavior can be either customized or locally change
;; (pressing 'c s')
;;
;; Note: in case a search-in-files mark or unmark some files; the view
;; mode will automatically be switched to marked-view.  This behavior
;; can be disabled.
;;
;;
;; MASTER PROJECT / BUILD CONFIGURATION / PLATFORM:
;;
;; The master project, build configuration and platform can be easily
;; changed using respectively: 'c t' 'c b' 'c p' Using the capital
;; letter ('c T' 'c B' and 'c P') will prompt the user for the new
;; value.
;;
;; This value allows to take quick actions for the master project:
;; build/clean/run/debug/update (keys: 'B' 'C' 'R' 'D' 'G')
;;
;;
;; KEY BINDINGS:
;;
;; Shortkey in the project-buffer-mode:
;;    +    -> collapse/expand folder/project (cursor has to be on a folder/project)
;;    m    -> mark the 'matching regexp' filename or the current file
;;    u    -> unmark file
;;    t    -> toggle marked files
;;    M    -> mark all
;;    U    -> unmark all
;;    f    -> open marked files
;;    q    -> cancel file search or bury project-buffer
;;    g    -> refresh the display / the projects (C-u g: refresh the current project only)
;;    ?    -> show brief help!!
;;    /    -> search file name matching regexp
;;    n    -> next file matching regexp
;;    p    -> prev file matching regexp
;;    v    -> view current file in view-mode
;;    o    -> find file at current pos in other window
;;    s    -> (un)mark files containing regexp...
;;   <TAB> -> collapse/expand folder/project (work if the cursor is on a file)
;;   <RET> -> open file at cursor pos
;;   <DEL> -> Delete the current node or the marked files
;;   <BCK> -> go to parent
;;   <SPC> -> next line
;; S-<SPC> -> prev line
;; C-<DWN> -> move to the next folder/project
;; C-<UP>  -> move to the previous folder/project
;; C-<LFT> -> expand if collapsed move to the first folder; move inside if expanded
;; C-<RGT> -> move up if folded collapsed; collapse if in front of folder ; move to the folded if in front of a file
;;    c s  -> Toggle search mode
;;    c v  -> Toggle view mode (flat / flat with the folder hidden / folder / marked files view)
;;    c b  -> switch to the next build configuration
;;    c m  -> switch the master project to be the current project
;;    c p  -> switch to the next platform
;;    c B  -> prompt to change build configuration
;;    c M  -> prompt for the master project (project to build)
;;    c P  -> prompt to change platform
;;    B    -> launch build
;;    C    -> launch clean
;;    D    -> launch run/with debugger
;;    R    -> launch run/without debugger
;;    G    -> launch the update command (useful to regenerate some makefile/vcproj... from cmake for example); can also be consider a user command.
;;    1    -> Switch to folder-view mode
;;    2    -> Switch to flat-view mode
;;    3    -> Switch to folder-hidden-view mode
;;    4    -> Switch to marked-view mode
;;
;;
;; Future improvement:
;;    T    -> touch marked files (need a variable to make sure touch is always available)
;;    h    -> find corresponding header/source (need regexps to match one and the other such as: source/header = ( "\.c\(pp\)?" . "\.h\(pp\)?" ) )
;;    d    -> show/hide project dependencies
;;    b    -> compile/buils marked files
;;
;;



;;; Raw mode:
;;

;; As it was mentioned earlier, project-buffer-mode is just an abstract
;; project manager.  Even if some extensions already exist, you may
;; want to be able to handle you own project system.
;;
;; Here is a sample code which shows how to create a new project:
;;
;; (defun test-projbuff()
;;   (interactive)
;;   (let ((buffer (generate-new-buffer "test-project-buffer")))                       ; Creation of a buffer for the project
;;     (display-buffer buffer)                                                         ; We want to switch to this buffer right away.
;;     (with-current-buffer buffer
;;       (cd "~/temp")                                                                 ; It's always better to set the root directory if it's known.
;;       (project-buffer-mode)                                                         ; Initialize the project buffer mode
;;
;;       (project-buffer-insert "test1" 'project "test1/Makefile" "test1")             ; Create an insert a project node called 'test1' (note: it's recommended to have project and node being the same)
;;       (project-buffer-insert "src/gfr.cpp" 'file  "~/temp/test1/gfr.cpp" "test1")   ; Add "~/tenp/gfr.cpp" to the project 'test1' it's project path will be: "src/gfr.cpp"
;;       (project-buffer-insert "src/abc.cpp" 'file  "~/temp/test1/abc.cpp" "test1")   ; Add "~/tenp/abc.cpp" to the project 'test1' it's project path will be: "src/abc.cpp"
;;
;;       (project-buffer-insert "test2" 'project "test2/Makefile" "test2")             ; Creation of a second project namded "test2"
;;       (project-buffer-insert "header/yyy.h" 'file  "~/temp/test2/zzz.h" "test2")    ; Add some file to this project; note that the project path and the physical file name can be completely different
;;       (project-buffer-insert "src/roo.c" 'file  "~/temp/test2/roo.c" "test2")       ;  the file name research will be based on the project-path and not on the physical file name
;;       (project-buffer-insert "script.awk" 'file "~/temp/test2/script.awk" "test2")  ;
;; )))
;;
;;
;; List of user functions available to handle your own project:
;; - `project-buffer-mode'                      which initialize the project-buffer mode
;; - `project-buffer-insert'                    to insert a file or project to the view
;; - `project-buffer-delete-file'               to remove a file
;; - `project-buffer-delete-folder'             to remove a folder and all its files
;; - `project-buffer-delete-project'            to remove a project and all its files
;; - `project-buffer-set-project-platforms'     to set the platform configuration for a particular project
;; - `project-buffer-set-build-configurations'  to set the build configurations for a particular project
;; - `project-buffer-raw-save'                  to save a project into a file
;; - `project-buffer-raw-load'                  to load a project from a file
;; - `project-buffer-set-project-user-data'     to set user data to a project node
;; - `project-buffer-get-project-user-data'     to get user data from a project node
;; - `project-buffer-set-file-user-data'        to set user data to a file node
;; - `project-buffer-get-file-user-data'        to get user data from a file node
;; - `project-buffer-get-current-project-name'  to get the nane of the current project the cursor is on
;; - `project-buffer-get-current-file-data'     to get data about the current file the cursor is on; nil if it's on a folder or a project
;; - `project-buffer-exists-p'                  to check if a node exists (file or folder) inside a project
;; - `project-buffer-project-exists-p'          to check if a project exists
;; - `project-buffer-get-project-path'          to get a project's path
;; - `project-buffer-get-file-path'             to get the path of a file of the project
;; - `project-buffer-get-current-node-type'     to get the type of the current node (including folder)
;; - `project-buffer-get-current-node-name'     to get the name  of the current node (including folder)
;; - `project-buffer-get-marked-node-list'      to get the list of marked files
;; - `project-buffer-set-project-settings-data' to set user project settings data
;; - `project-buffer-get-project-settings-data' to retrive the user project settings data
;; - `project-buffer-apply-to-each-file'        to perform a function call on every file node
;; - `project-buffer-apply-to-marked-files'     to perform a function call on eveyr marked files; the function returns nil if no marked files were found
;; - `project-buffer-apply-to-project-files'    to perform a function call on every files belonging to a specified project
;;
;; If you need to have some local variables to be saved; register them in `project-buffer-locals-to-save'.
;; The same way, if there is need to save extra hooks: register them in `project-buffer-hooks-to-save'.


;;; Todo:
;;

;;  - show project dependencies
;;     e.g: [+] ProjName1           <deps: ProjName3, ProjName2>
;;  - add collapsed all / expand all commands
;;  - provide a touch marked files command
;;  - provide a compile/build marked files command
;;  - add a command to easily find the corresponding header/source for the current file (or specified file)
;;  - disable project which doesn't have the current selected platform/build-configuration in their list ???




;;; History:
;;

;; v1.00: First public release.
;; v1.10: Added mouse support and save/load.
;;        - Enable click on folder/project to expand/collapse them.
;;        - Enable click on filename to open them.
;;        - Added global command to load/save/write/revert a project buffer.
;;        - Added new hook: `project-buffer-post-find-file-hook'.
;;        - Added possibilty to attach user data to each nodes.
;; v1.11: Bugs fixed
;;        - project-buffer-find-node-up was return nil in view-mode other than folder-view
;;        - file-exist-p has been renamed to file-exists-p
;;        - minor visibility bug when a files get added to the project if the view-mode is different from folder-view
;; v1.12: New action added
;;        - 'update' to allow some project generation from cmake or other build system
;; v1.20: Add new commands:
;;        - Delete current node
;;        - Delete marked files
;;        - Delete current node or marked file if in front of one (bound to <DEL>
;;        Add the following user functions:
;;        - `project-buffer-get-current-project-name' to get the project name the cursor is on
;;        - `project-buffer-get-current-file-data' to get data about the file the cursor is on
;;        - `project-buffer-get-file-path' to get the file path
;;        - `project-buffer-get-current-node-type' to get the type of the current node (including folder)
;;        - `project-buffer-get-current-node-name' to get the name  of the current node (including folder)
;;        - `project-buffer-delete-folder' to remove a folder and all its files
;;        - `project-buffer-exists-p' to check if a node exists (file or folder) inside a project
;;        - `project-buffer-project-exists-p' to check if a project exists
;; v1.21: Remap the update action to G; to remove the key conflict with the 'unmark all' command.
;;        Added the following user function:
;;        - `project-buffer-get-marked-node-list' to get the list of marked files
;;        Fix bug when deleting the cached folder.
;;        Added the refresh command bound to 'g'.
;;        The non-existing files are now 'grayed' out (can be disabled
;;          setting `project-buffer-check-file-existence' to nil)
;; v1.22: Added the following user functions:
;;        - `project-buffer-set-project-settings-data' to set user project settings data
;;        - `project-buffer-get-project-settings-data' to retrieve the user project settings data
;;        - `project-buffer-apply-to-each-file'        to perform a function call on every file node
;;        - `project-buffer-apply-to-marked-files'     to perform a function call on eveyr marked files; the function returns nil if no marked files were found
;;        - `project-buffer-apply-to-project-files'    to perform a function call on every files belonging to a specified project
;;        - `project-buffer-get-project-path'          to get a project's path
;;        Refresh hooks now receive the current project or the project list as argument.
;;        It is now possible to refresh the current project only using the prefix argument

(require 'cl)
(require 'ewoc)



;;; Code:



;;
;; Group definition:
;;


(defgroup project-buffer nil
  "A special mode to manage projects."
)


;;
;; Constants:
;;

(defconst project-buffer-mode-version "1.22"
  "Version numbers of this version of `project-buffer-mode'.")


;;
;; Customizable variables:
;;


(defcustom project-buffer-new-project-collapsed t
  "Newly added project will be collapsed by default."
  :type 'boolean
  :group 'project-buffer)


(defcustom project-buffer-search-in-files-mode 'narrow-marked-files
  "Variable defining the current search-in-files mode.
The different search mode set to 'narrow-marked-files it will
search in the selected marked files, removing the one failing the
research, set to 'all-files it will launch the search on all
files in the projects, 'current-project will only search with the
current project Note: if no files are marked while using
narrow-marked-files, the search will occur in all files in the
project."
  :type '(choice (const :tag "Narrow the marked files" narrow-marked-files)
		 (const :tag "All files" all-files)
		 (const :tag "Current project" current-project))
  :group 'project-buffer)


(defcustom project-buffer-autoswitch-marked-view-mode t
  "If set to t, the view-mode will automatically be switched to
the marked-view mode after performing a search-in-files (unless
no files got marked/unmarked)."
  :type 'boolean
  :group 'project-buffer)


(defcustom project-buffer-confirm-function 'yes-or-no-p
  "Confirmation function called before clean and node deletion."
  :type '(radio function
                (function-item yes-or-no-p)
                (function-item y-or-n-p))
  :group 'project-buffer)

(defcustom project-buffer-cleanup-empty-projects nil
  "When set, deleting the last file of a project will result in
deleting the project itself."
  :type 'boolean
  :group 'project-buffer)


(defcustom project-buffer-check-file-existence t
  "When set, the displayed files will be displayed with
'project-buffer-file-doesnt-exist' font if the file doesn't
exists."
  :type 'boolean
  :group 'project-buffer)



;;
;;  Font
;;


(defface project-buffer-project-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "salmon")))
  "Project buffer mode face used to highlight project nodes."
  :group 'project-buffer)

(defface project-buffer-master-project-face
  '((default (:inherit project-buffer-project-face :bold t)))
  "Master project buffer mode face used to highlight project nodes."
  :group 'project-buffer)

(defface project-buffer-folder-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) (:foreground "cyan")))
  "Project buffer mode face used to highlight folder nodes."
  :group 'project-buffer)

(defface project-buffer-file-face
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark)) (:foreground "white")))
  "Project buffer mode face used to highlight file nodes."
  :group 'project-buffer)

(defface project-buffer-project-button-face
  '((((class color) (background light)) (:foreground "gray50"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Project buffer mode face used highligh [ and ] in front of the project name."
  :group 'project-buffer)

(defface project-buffer-indent-face
  '((((class color) (background light)) (:foreground "gray50"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Project buffer mode face used to highlight indent characters."
  :group 'project-buffer)

(defface project-buffer-mark-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "tomato")))
  "Project buffer mode face used highligh marks."
  :group 'project-buffer)

(defface project-buffer-filename-face
  '((((class color) (background light)) (:foreground "gray50"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Project buffer mode face used highligh file names."
  :group 'project-buffer)

(defface project-buffer-matching-file-face
  '((default (:inherit project-buffer-file-face :bold t)))
  "Project buffer mode face used matching file."
  :group 'project-buffer)


(defface project-buffer-file-doesnt-exist
  '((((class color) (background light)) (:foreground "dark gray"))
    (((class color) (background dark)) (:foreground "dim gray")))
  "Project buffer mode face used to highlight non-existing file nodes."
  :group 'project-buffer)



;;
;;  User hook:
;;


(defcustom project-buffer-mode-hook nil
  "Post `project-buffer-mode' initialization hook."
  :type 'hook
  :group 'project-buffer)


(defcustom project-buffer-action-hook nil
  "Hook to perform the actions (build, clean, run...)

The function should follow the prototype:
  (lambda (action project-name project-path platform configuration)
 Where ACTION represents the action to apply to the project,
 it may be: 'build 'clean 'run 'debug 'update,
 PROJECT-NAME is the name of the master project,
 PROJECT-PATH is the file path of the project
 PLATFORM is the name of the selected platform,
 and CONFIGURATION correspond to the selected build configuration."
  :type 'hook
  :group 'project-buffer)


(defcustom project-buffer-post-load-hook nil
  "Hook to run after performing `project-buffer-raw-load'.

Register functions here to keep the customization after reloading the project.")


(defcustom project-buffer-post-find-file-hook nil
  "Hook to run after performing `project-buffer-find-file' or
`project-buffer-find-file-other-window'.

The function should follow the prototype:
  (lambda (project-buffer file-buffer))
Where PROJECT-BUFFER is the buffer of the project, and
FILE-BUFFER is the buffer of the file.")


(defcustom project-buffer-refresh-hook nil
  "Hook to run before refreshing every node..

The function should follow the prototype:
  (lambda (project-list content))
Where PROJECT-LIST is a list of project names (can be nil), 
and CONTENT can either be 'current or 'all.

This is the place to add functions which reload the project file,
check if any files should be added or remove from the proejct.")


;;
;;  Buffer local variables:
;;


(defvar project-buffer-status nil)
(defvar project-buffer-view-mode nil)
(defvar project-buffer-cache-project nil)
(defvar project-buffer-cache-subdirectory nil)
(defvar project-buffer-projects-list nil)
(defvar project-buffer-master-project nil)
(defvar project-buffer-platforms-list nil)
(defvar project-buffer-current-platform nil)
(defvar project-buffer-build-configurations-list nil)
(defvar project-buffer-current-build-configuration nil)
(defvar project-buffer-file-name nil)
(defvar project-buffer-locals-to-save nil)
(defvar project-buffer-hooks-to-save nil)



;;
;; History:
;;


(defvar project-buffer-regexp-history nil
  "History list of regular expressions used in project-buffer commands.")



;;
;;  Data type:
;;


;; Structure to store data attached to each ewoc-node.
;; Each node represents either a file or a project or a folder indide the project"
(defstruct (project-buffer-node
	    (:copier nil)
	    (:constructor project-buffer-create-node (name type filename project &optional hidden))
	    (:conc-name project-buffer-node->))
  name				;; string displayed to represent the file (usually the file.ext)
  type				;; project? file? folder?

  marked			;; is the file marked?
  hidden			;; hidden files (currently: = project/folder close)
  collapsed			;; is the folder/project collapsed or not?
  project-collapsed		;; t if the project the file belong to is collapsed

  matched			;; the file matches the regexp search

  filename			;; path to the filename
  project			;; name of the project the file belongs to
  parent			;; parent node (parent folder or project or nil)

  platform-list			;; list of the platform available for the project (valid in project node only)
  build-configurations-list	;; list of build configuration avalailable for the project (valid in project node only)

  user-data                     ;; user data could be set (mainly useful to store something per project)
  project-settings              ;; user data field used to store the project settings
)



;;
;;  Key Bindings:
;;


;; Define the key mapping for the spu mode:
(defvar project-buffer-mode-map
  (let ((project-buffer-mode-map (make-keymap)))
    (define-key project-buffer-mode-map [?+] 'project-buffer-toggle-expand-collapse)
    (define-key project-buffer-mode-map [?\t] 'project-buffer-toggle-expand-collapse-even-on-file)
    (define-key project-buffer-mode-map [?m] 'project-buffer-mark-matched-files-or-current-file)
    (define-key project-buffer-mode-map [?u] 'project-buffer-unmark-matched-files-or-current-file)
    (define-key project-buffer-mode-map [?M] 'project-buffer-mark-all)

    (define-key project-buffer-mode-map [?U] 'project-buffer-unmark-all)
    (define-key project-buffer-mode-map [?t] 'project-buffer-toggle-all-marks)
    (define-key project-buffer-mode-map [?f] 'project-buffer-find-marked-files)
    (define-key project-buffer-mode-map [?/] 'project-buffer-search-forward-regexp)
    (define-key project-buffer-mode-map [?n] 'project-buffer-goto-next-match)

    (define-key project-buffer-mode-map [?p] 'project-buffer-goto-prev-match)
    (define-key project-buffer-mode-map [?v] 'project-buffer-view-file)
    (define-key project-buffer-mode-map [?c ?s] 'project-buffer-toggle-search-mode)
    (define-key project-buffer-mode-map [?c ?v] 'project-buffer-toggle-view-mode)
    (define-key project-buffer-mode-map [?c ?b] 'project-buffer-next-build-configuration)
    (define-key project-buffer-mode-map [?c ?p] 'project-buffer-next-platform)
    (define-key project-buffer-mode-map [?c ?m] 'project-buffer-select-current-as-master-project)
    (define-key project-buffer-mode-map [?c ?B] 'project-buffer-choose-build-configuration)
    (define-key project-buffer-mode-map [?c ?P] 'project-buffer-choose-platform)
    (define-key project-buffer-mode-map [?c ?M] 'project-buffer-choose-master-project)
    (define-key project-buffer-mode-map [backspace] 'project-buffer-goto-dir-up)

    (define-key project-buffer-mode-map [?\ ] 'project-buffer-next-file)
    (define-key project-buffer-mode-map [(shift ?\ )] 'project-buffer-prev-file)
    (define-key project-buffer-mode-map [return] 'project-buffer-node-find-file)
    (define-key project-buffer-mode-map [mouse-1] 'project-buffer-mouse-find-file)
    (define-key project-buffer-mode-map [?o] 'project-buffer-node-find-file-other-window)
    (define-key project-buffer-mode-map [(control left)] 'project-buffer-goto-dir-up-or-collapsed)

    (define-key project-buffer-mode-map [(control right)] 'project-buffer-next-file-or-expand)
    (define-key project-buffer-mode-map [(control up)] 'project-buffer-go-to-previous-folder-or-project)
    (define-key project-buffer-mode-map [(control down)] 'project-buffer-go-to-next-folder-or-project)
    (define-key project-buffer-mode-map [??] 'project-buffer-help)
    (define-key project-buffer-mode-map [?q] 'project-buffer-quit)
    (define-key project-buffer-mode-map [?g] 'project-buffer-refresh)

    (define-key project-buffer-mode-map [?B] 'project-buffer-perform-build-action)
    (define-key project-buffer-mode-map [?C] 'project-buffer-perform-clean-action)
    (define-key project-buffer-mode-map [?R] 'project-buffer-perform-run-action)
    (define-key project-buffer-mode-map [?D] 'project-buffer-perform-debug-action)
    (define-key project-buffer-mode-map [?G] 'project-buffer-perform-update-action)
    (define-key project-buffer-mode-map [?s] 'project-buffer-mark-files-containing-regexp)

    (define-key project-buffer-mode-map [?1] 'project-buffer-set-folder-view-mode)
    (define-key project-buffer-mode-map [?2] 'project-buffer-set-flat-view-mode)
    (define-key project-buffer-mode-map [?3] 'project-buffer-set-folder-hidden-view-mode)
    (define-key project-buffer-mode-map [?4] 'project-buffer-set-marked-view-mode)

    (define-key project-buffer-mode-map [delete] 'project-buffer-delete-current-node-or-marked-files)

    project-buffer-mode-map))


;;
;;  Internal Utility Functions:
;;


(defun project-buffer-erase-all(status)
  "Erase all nodes from the buffer."
  (let ((node (ewoc-nth status 0)))
    (while node
      (project-buffer-delete-project-node status (project-buffer-node->name (ewoc-data node)) node)
      (setq node (ewoc-nth status 0)))
    (setq project-buffer-cache-project nil)
    (setq project-buffer-cache-subdirectory nil)
    (setq project-buffer-projects-list nil)
    (setq project-buffer-master-project nil)
    (setq project-buffer-platforms-list nil)
    (setq project-buffer-current-platform nil)
    (setq project-buffer-build-configurations-list nil)
    (setq project-buffer-current-build-configuration nil)
    (project-buffer-refresh-ewoc-hf status)
    ))


(defun project-buffer-mark-matching-file(status regexp)
  "Check each file name and mark the files matching the regular expression REGEXP"
  (let ((node (ewoc-nth status 0)))
    (while node
      (let* ((node-data (ewoc-data node))
	     (node-type (project-buffer-node->type node-data))
	     (node-name (project-buffer-node->name node-data))
	     (file      (file-name-nondirectory node-name)))
	(when (string-match regexp file)
	  (let ((parent (project-buffer-find-node-up status node)))
	    (while (and parent
			(not (eq (project-buffer-node->type (ewoc-data parent)) 'project))
			(not (project-buffer-node->matched (ewoc-data parent))))
	      (setf (project-buffer-node->matched (ewoc-data parent)) t)
	      (ewoc-invalidate status parent)
	      (setq parent (project-buffer-find-node-up status parent))
	      ))
	  (setf (project-buffer-node->matched node-data) t)
	  (ewoc-invalidate status node)
	  ))
      (setq node (ewoc-next status node)))))


(defun project-buffer-read-regexp(prompt)
  "Read a regular expression from the minibuffer."
  (read-from-minibuffer prompt nil nil nil 'project-buffer-regexp-history))


(defun project-buffer-clear-matched-mark(status)
  "Clear 'matched' flag"
  (let (result)
    (ewoc-map (lambda (node)
		(when (project-buffer-node->matched node)
		  (setf (project-buffer-node->matched node) nil)
		  (setq result t)))
	      status)
    result))


(defun project-buffer-get-marked-nodes(status)
  "Return the list of marked node or the current node if none are marked"
  (or (ewoc-collect status (lambda (node) (project-buffer-node->marked node)))
      (list (ewoc-data (ewoc-locate status)))))


(defun project-buffer-convert-name-for-display(node-data)
  "Convert the node name into the displayed string depending on the project-buffer-view-mode."
  (let* ((node-name   (project-buffer-node->name node-data))
	 (file-color  (if (project-buffer-node->matched node-data) 
			  'project-buffer-matching-file-face 
			  (if (and project-buffer-check-file-existence
				   (eq (project-buffer-node->type node-data) 'file)
				   (not (file-exists-p (project-buffer-node->filename node-data))))
			      'project-buffer-file-doesnt-exist
			      'project-buffer-file-face)))
	 (node-color  (if (eq (project-buffer-node->type node-data) 'file) file-color 'project-buffer-folder-face))
	 (file-help   (concat "mouse-1: find file other window: " (project-buffer-node->filename node-data)))
	 (folder-help (concat "mouse-1: "
			      (if (project-buffer-node->collapsed node-data) "expand" "collapse")
			      " folder " node-name ".")))
    (cond ((eq project-buffer-view-mode 'flat-view)
	   (concat (propertize " `- " 'face 'project-buffer-indent-face)
		   (and (file-name-directory node-name)
			(propertize (file-name-directory node-name) 'face 'project-buffer-folder-face))
		   (propertize (file-name-nondirectory node-name)
			       'face file-color
			       'mouse-face 'highlight
			       'help-echo file-help)))
	  ((eq project-buffer-view-mode 'folder-hidden-view)
	   (concat (propertize " `- " 'face 'project-buffer-indent-face)
		   (propertize (file-name-nondirectory node-name)
			       'face file-color
			       'mouse-face 'highlight
			       'help-echo file-help)))
	  ((eq project-buffer-view-mode 'folder-view)
	   (let ((dir-list (split-string node-name "/"))
		 (str (if (eq (project-buffer-node->type node-data) 'file)
			  " `- "
			  (concat " `"
				  (propertize (if (project-buffer-node->collapsed node-data) "+" "-")
					      'mouse-face 'highlight
					      'help-echo folder-help)
				  " ")))
		 (cur 1))
	     (while (< cur (length dir-list))
	       (setq str (concat " |  " str)
		     cur (1+ cur)))
	     (concat (propertize str 'face 'project-buffer-indent-face)
		     (if (eq (project-buffer-node->type node-data) 'file)
			 (propertize (file-name-nondirectory node-name)
				     'face node-color
				     'mouse-face 'highlight
				     'help-echo file-help)
			 (propertize (file-name-nondirectory node-name)
				     'face node-color
				     'mouse-face 'highlight
				     'help-echo folder-help))
			 )))
	  ((eq project-buffer-view-mode 'marked-view)
	   (concat (propertize " - " 'face 'project-buffer-indent-face)
		   (and (file-name-directory node-name)
			(propertize (file-name-directory node-name) 'face 'project-buffer-folder-face))
		   (propertize (file-name-nondirectory node-name)
			       'face file-color
			       'mouse-face 'highlight
			       'help-echo file-help
			       )))
	  (t (format "Unknown view mode: %S" project-buffer-view-mode) ))))


(defun project-buffer-prettyprint(node)
  "Pretty-printer function"
  (let ((node-collapsed (project-buffer-node->collapsed node))
	(node-name     (project-buffer-node->name  node))
	(node-marked   (project-buffer-node->marked node))
	(node-type     (project-buffer-node->type node))
	(node-hidden   (project-buffer-node->hidden node))
	(node-matching (project-buffer-node->matched node))
	(node-prjcol   (project-buffer-node->project-collapsed node))
	(node-project  (project-buffer-node->project node))
	(project-help  (concat "mouse-1: "
			       (if (project-buffer-node->collapsed node) "expand" "collapse")
			       " project "
			       (project-buffer-node->name node))))
    (if (eq project-buffer-view-mode 'marked-view)
	(when (and (eq node-type 'file)
		   (or node-marked node-matching))
	  (insert (concat " "
			  (if node-marked (propertize "*" 'face 'project-buffer-mark-face) " ")
			  " "
			  (propertize (if (> (length node-project) 16)
					  (substring node-project 0 16)
					  node-project)
				      'face 'project-buffer-project-face)))
   	  (indent-to-column 19)
	  (insert (concat (project-buffer-convert-name-for-display node)
			  "\n")))
	(when (or (and (eq project-buffer-view-mode 'folder-view)
		       (or (not node-hidden)
			   node-matching))
		  (and (not (eq project-buffer-view-mode 'folder-view))
		       (not (eq node-type 'folder))
		       (or (not node-prjcol)
			   node-matching))
		  (eq node-type 'project))
	  (insert (concat " "
			  (if node-marked (propertize "*" 'face 'project-buffer-mark-face)" ")
			  " "
			  (cond ((not (eq node-type 'project)) "   ")
				(node-collapsed                (propertize "[+]"
									   'face 'project-buffer-project-button-face
									   'mouse-face 'highlight
									   'help-echo project-help))
				(t                             (propertize "[-]"
									   'face 'project-buffer-project-button-face
									   'mouse-face 'highlight
									   'help-echo project-help)))
			  " "
			  (or (and (eq node-type 'project)
				   (propertize node-name
					       'face (or (and project-buffer-master-project
							      (string= node-name (car project-buffer-master-project))
							      'project-buffer-master-project-face)
							 'project-buffer-project-face)
					       'mouse-face 'highlight
					       'help-echo project-help))
			      (project-buffer-convert-name-for-display node))))
	  (when (and (eq project-buffer-view-mode 'folder-hidden-view)
		     (project-buffer-node->filename node)
		     (eq (project-buffer-node->type node) 'file))
	    (indent-to-column 40)
	    (insert (concat " " (propertize (project-buffer-node->filename node)
					    'face 'project-buffer-filename-face))))
	  (insert "\n"))
	)))


(defun project-buffer-refresh-ewoc-hf(status)
  "Refresh ewoc header/footer"
  (ewoc-set-hf status
	       (concat (format "Project view mode:   %s\n" project-buffer-view-mode)
		       (format "Platform:            %s\n" (or project-buffer-current-platform "N/A"))
		       (format "Build configuration: %s\n" (or project-buffer-current-build-configuration "N/A"))
		       (format "Search mode:         %s\n" project-buffer-search-in-files-mode)
		       "\n\n") ""))


(defun project-buffer-extract-folder(name type)
  "Return the folder associated to the node's NAME of the type TYPE.
Return nil if TYPE is project."
  (cond ((eq type 'folder) name)
	((eq type 'project) nil)
	(t (let ((dirname (file-name-directory name)))
	     (and dirname (substring dirname 0 -1))))))


(defun project-buffer-directory-lessp(dir1 dir2 type2)
  "Return t if DIR1 is less than (DIR2,TYPE2)."
  (let* ((list1  (and dir1 (split-string dir1 "/")))
	 (list2  (and dir2 (split-string dir2 "/")))
	 (cnt 0))
    (if (and list1 list2)
	(progn (while (and (< cnt (length list1))
			   (< cnt (length list2))
			   (string= (nth cnt list1) (nth cnt list2)))
		 (setq cnt (1+ cnt)))
	       (if (and (< cnt (length list1))
			(< cnt (length list2)))
		   (string-lessp (nth cnt list1) (nth cnt list2))
		   (and (eq type2 'file)
			(< cnt (length list1)))
		   ))
	(null list2))))


(defun project-buffer-parent-of-p(child parent)
  "Check if CHILD is a child of the directory PARENT."
  (let* ((clist (and child  (split-string child "/")))
	 (plist (and parent (split-string parent "/")))
	 (cont t)
	 res)
    (while (and clist plist cont)
      (let ((cname (pop clist))
	    (pname (pop plist)))
	(setq cont (string-equal cname pname))))
    (and cont (null plist))))


(defun project-buffer-find-node-up(status node)
  "Return the directory or project in which the node belong.
This may change depending on the view mode."
  (if (eq project-buffer-view-mode 'folder-view)
      (project-buffer-node->parent (ewoc-data node))
      (let ((parent (project-buffer-node->parent (ewoc-data node))))
	(when parent
	  (while (not (eq (project-buffer-node->type (ewoc-data parent)) 'project))
	    (setq parent (project-buffer-node->parent (ewoc-data parent))))
	  parent))))


(defun project-buffer-search-project-node(status project-name)
  "Return the node of the project node named PROJECT-NAME or nil if absent"
  (if (string-equal (car project-buffer-cache-project) project-name)
      (cdr project-buffer-cache-project)
      (let ((node (ewoc-nth status 0)))
	(while (and node
		    (or (not (eq (project-buffer-node->type (ewoc-data node)) 'project))
			(not (string-equal (project-buffer-node->name (ewoc-data node)) project-name))))
	  (setq node (ewoc-next status node)))
	node)))


(defun project-buffer-set-project-platforms-data(status project platform-list)
  "Attached the list of platform contained in PLATFORM-LIST to the project named PROJECT."
  (let ((node (project-buffer-search-project-node status project)))
    ;; Now, if the project has been found:
    (when node
      (setf (project-buffer-node->platform-list (ewoc-data node)) platform-list)
      ;; also:
      (while platform-list
	(add-to-list 'project-buffer-platforms-list (pop platform-list) t))
      (unless project-buffer-current-platform
	(setq project-buffer-current-platform (car project-buffer-platforms-list)))))
  (project-buffer-refresh-ewoc-hf status))


(defun project-buffer-set-project-build-configurations-data(status project build-configuration-list)
  "Attached the list build configurations in BUILD-CONFIGURATION-LIST to the project named PROJECT."
  (let ((node (project-buffer-search-project-node status project)))
    ;; Now, if the project has been found:
    (when node
      (setf (project-buffer-node->build-configurations-list (ewoc-data node)) build-configuration-list)
      ;; also:
      (while build-configuration-list
	(add-to-list 'project-buffer-build-configurations-list (pop build-configuration-list) t))
      (unless project-buffer-current-build-configuration
	(setq project-buffer-current-build-configuration (car project-buffer-build-configurations-list)))))
  (project-buffer-refresh-ewoc-hf status))


(defun project-buffer-insert-node(status data)
  "Insert a file in alphabetic order in it's project/directory."
  (let ((node           (ewoc-nth status 0))
	(folder-data    (project-buffer-extract-folder (project-buffer-node->name data)      (project-buffer-node->type data)))
	(name-data      (file-name-nondirectory (project-buffer-node->name data)))
	(type-data      (project-buffer-node->type data))
	(proj-data      (project-buffer-node->project data))
	(node-data      nil)
	(here           nil)
	(proj-found     nil)
	(folder         nil)
	(hidden-flag    nil)
	(skip           nil)
	(proj-root-node nil)
	(folder-node    nil)
	(parent-node    nil)
	)
    (when (eq type-data 'folder)
      (error "Not supported -- in particular project-buffer-directory-lessp may returns a incorrect value"))


    ;; Cache check:
    (when project-buffer-cache-project
      (cond
       ;; cache-project < current-project -> we can start the search from here (at least).
       ((string-lessp (car project-buffer-cache-project) proj-data)
	(setq node (cdr project-buffer-cache-project)
	      project-buffer-cache-subdirectory nil))

       ;; cache-project == current-project -> check the folders...
       ((string-equal (car project-buffer-cache-project) proj-data)
	;; cache-subdir < current-subdir -> we can start from here.
	;; cache-subdir = current-subdir -> good starting point
	(if (and project-buffer-cache-subdirectory
		 folder-data
		 (or (string-equal (car project-buffer-cache-subdirectory) folder-data)
		     (project-buffer-directory-lessp (car project-buffer-cache-subdirectory) folder-data 'folder)))
	    (setq node (cdr project-buffer-cache-subdirectory)
		  proj-root-node (cdr project-buffer-cache-project)
		  proj-found t)
	    (setq node (cdr project-buffer-cache-project)
		  project-buffer-cache-subdirectory nil)))
       ;; other wise: cache miss...
       (t
	(setq project-buffer-cache-project nil
	      project-buffer-cache-subdirectory nil))))

    ;; Search where to insert the node:
    (while (and node (not here) (not skip))
      (setq node-data (ewoc-data node))

      (cond
       ;; data.project < node.project -> insert here...
       ((string-lessp proj-data (project-buffer-node->project node-data))
	(if (eq (project-buffer-node->type data) 'project)
	    (setq here node)
	    (setq here (and proj-found node)
		  skip (not proj-found))))

       ;; node.project == data.project -> check folder/file name
       ((string-equal proj-data (project-buffer-node->project node-data))
	(if (eq (project-buffer-node->type data) 'project)
	    ;; If we're trying to add the project when the project already exist... we'll skip it.
	    (setq skip t)
	    ;; Otherwise:
	    (let* ((folder-db   (project-buffer-extract-folder (project-buffer-node->name node-data) (project-buffer-node->type node-data)))
		   (name-db     (file-name-nondirectory (project-buffer-node->name node-data)))
		   (type-db     (project-buffer-node->type node-data)))
	      ;; Are we're on the project line???
	      (if (eq type-db 'project)
		  (setq proj-root-node node)
		  (if (and folder-db folder-data)
		      ;; Both the current node and the new one have a directory
		      (progn (when (and (eq type-db 'folder)
					(project-buffer-parent-of-p (project-buffer-node->name data) folder-db))
			       (setq folder-node node))
			     (cond ((project-buffer-directory-lessp folder-data folder-db type-db)
				    (setq here node))

				   ((string-equal folder-data folder-db)
				    (when (eq type-db 'folder)
				      (setq folder-node node))
				    (setq folder folder-data)
				    (if (eq type-data 'folder)
					(setq skip t)
					(unless (eq type-db 'folder)
					  (when (string-lessp name-data name-db)
					    (setq here node)))))

				   (t (setq folder folder-db))))
		      ;; Either:
		      ;; - the current node has no folder, meaning:
		      ;;   -> either the new node has a directory in which case we'll add it here.
		      ;;   -> or we'll search for the right place to add it.
		      ;; - the current node has a folder, meaning:
		      ;;   -> the new one has no folder, therefore, we need to carry on until we reach the no-folder area.
		      (unless folder-db
			(if folder-data
			    (setq here node)
			    (when (string-lessp name-data name-db)
			      (setq here node)))))))
	      (setq proj-found t))
	))

       ;; Carry on...
       (setq node (ewoc-next status node)))

    ;; Insert before here...
    (when (not skip)

      ;; Here we can set the parent folder:
      (if folder-node
	(setf (project-buffer-node->parent data) folder-node)
	(setf (project-buffer-node->parent data) proj-root-node))

      ;; Once the node added we will need to check if it should be hidden or not.
      ;; At first, if it's a file, it will be hidden to not have any glitch in the displayed buffer
      (if (eq type-data 'project)
	  (progn (setf (project-buffer-node->project-collapsed data) project-buffer-new-project-collapsed)
		 (setf (project-buffer-node->collapsed data) project-buffer-new-project-collapsed)
		 (add-to-list 'project-buffer-projects-list name-data)
		 (unless project-buffer-master-project
		   (setq project-buffer-master-project (cons name-data nil)))) ; to prevent blinking
	  (progn (setf (project-buffer-node->hidden data) t)
		 (setf (project-buffer-node->project-collapsed data) (project-buffer-node->project-collapsed (ewoc-data (project-buffer-node->parent data))))
		 (unless proj-root-node
		   (error "Project '%s' not found" proj-data))))

      (if here
	  (setq node (ewoc-enter-before status here data))
	  (setq node (ewoc-enter-last status data)))

      (when (eq type-data 'project)
	(unless (cdr project-buffer-master-project)
	  (setq project-buffer-master-project (cons name-data node)))
	(setq proj-root-node node))

      ;;

      ;; If it's not a project type, search up in all possible parent to see if the node is supposed to be visible or not
      (unless (eq type-data 'project)
	(let* ((shown t)
	       (parent (project-buffer-find-node-up status node)))
	  (setf (project-buffer-node->project-collapsed data) (project-buffer-node->project-collapsed (ewoc-data parent)))
	  (setq shown (not (and parent (project-buffer-node->collapsed (ewoc-data parent)))))
	  (while (and parent
		      shown
		      (not (eq (project-buffer-node->type (ewoc-data parent)) 'project)))
	    (setq parent (project-buffer-find-node-up status parent))
	    (setq shown  (not (and parent (project-buffer-node->collapsed (ewoc-data parent)))))
	    )
	  (setq hidden-flag (not shown)))
	(unless hidden-flag
	  (setf (project-buffer-node->hidden data) nil)
	  (ewoc-invalidate status node)))

      ;; In case some folder needed to be created:
      (when folder-data
	(let* ((db-list     (and folder (split-string folder "/")))
	       (curr-list   (split-string folder-data "/"))
	       (cnt 0))
	  (while (and (< cnt (length curr-list))
		      (< cnt (length db-list))
		      (string= (nth cnt db-list) (nth cnt curr-list)))
	    (setq cnt (1+ cnt)))
	  ;; Add the extra folder:
	  (if (< cnt (length curr-list))
	      (let ((ndx 0)
		    (str nil))
		(while (< ndx cnt)
		  (setq str (or (and str (concat str "/" (nth ndx curr-list)))
				(nth ndx curr-list)))
		  (setq ndx (1+ ndx)))
		(while (< ndx (length curr-list))
		  (setq str (or (and str (concat str "/" (nth ndx curr-list)))
				(nth ndx curr-list)))

		  (setq parent-node (or folder-node proj-root-node))
		  (let ((new-data (project-buffer-create-node str 'folder folder proj-data hidden-flag)))
		    (setf (project-buffer-node->project-collapsed new-data) (project-buffer-node->project-collapsed data))
		    (setq folder-node (ewoc-enter-before status node new-data)))
		  (setf (project-buffer-node->parent (ewoc-data folder-node)) parent-node)

		  (setf (project-buffer-node->parent data) folder-node)
		  (setq ndx (1+ ndx)))))
	  ))
      )

    ;; Save the project root node:
    ;; - to speed up the next insert (we stop looking for the project if it's the same one)
    (setq project-buffer-cache-project (cons proj-data proj-root-node))
    (setq project-buffer-cache-subdirectory (and folder-node
						 (cons folder-data folder-node)))
))


(defun project-buffer-delete-node(status node &optional dont-delete-project)
  "Delete a specific node.
Also cleanup with empty folder/project resulting of the deletion."
  (let ((parent-node       (project-buffer-node->parent (ewoc-data node)))
	(project           (project-buffer-node->project (ewoc-data node)))
	(inhibit-read-only t))
    ;; Delete the found node:
    (when (and project-buffer-cache-subdirectory
	       (eq node (cdr project-buffer-cache-subdirectory)))
      (setq project-buffer-cache-subdirectory nil))
    (ewoc-delete status node)

    ;; Now it's time to check the parent node the file belong to:
    (while parent-node
      (let ((next-node   (ewoc-next status parent-node))
	    (parent-data (ewoc-data parent-node)))
	(if (and next-node
		 (eq (project-buffer-node->parent (ewoc-data next-node)) parent-node))
	    (setq parent-node nil)
	    (let ((new-parent-node (and (not (eq (project-buffer-node->type parent-data) 'project))
					(project-buffer-node->parent parent-data))))
	      (if (not new-parent-node)
		  (unless dont-delete-project
		    (project-buffer-delete-project-node status project parent-node))
		  (progn (when (and project-buffer-cache-subdirectory
				    (eq parent-node (cdr project-buffer-cache-subdirectory)))
			   (setq project-buffer-cache-subdirectory nil))
			 (ewoc-delete status parent-node)))
	      (setq parent-node new-parent-node))
	    )))
    ))


(defun project-buffer-delete-file-node(status name project &optional dont-delete-project)
  "Delete the node named NAME which belongs to PROJECT.
Empty folder node will also be cleared up."
  (let* ((node (project-buffer-search-node status name project)))
    (when node
      (project-buffer-delete-node status node dont-delete-project))
    ))


(defun project-buffer-delete-folder-node(status folder-node &optional dont-delete-project)
  "Delete the folder FOLDER-NODE and all it's files.
Empty parent folder node will also be cleared up."
  (let* ((folder (and folder-node (project-buffer-node->name (ewoc-data folder-node)))))
    (when folder
      ;; First, let delete the content of the folder:
      (let ((inhibit-read-only t))
	(save-excursion
	  (let* ((node      (ewoc-next status folder-node))
		 (node-data (and node (ewoc-data node)))
		 next-node)
	    (while (and node
			(not (eq (project-buffer-node->type node-data) 'project))
			(project-buffer-parent-of-p (project-buffer-node->name node-data) folder))
	      (setq next-node (ewoc-next status node))
	      (when (and project-buffer-cache-subdirectory (eq node (cdr project-buffer-cache-subdirectory)))
		(setq project-buffer-cache-subdirectory nil))
	      (ewoc-delete status node)
	      (setq node next-node
		    node-data (and node (ewoc-data node)))))))
      ;; Now let's delete the node:
      (project-buffer-delete-node status folder-node dont-delete-project)
      )))


(defun project-buffer-delete-project-node(status proj-name proj-node)
  "Delete the project node PROJ-NODE.
Each files/folder under the project will also be deleted."
  (when proj-node
    (let ((proj-data (ewoc-data proj-node))
	  (prev-node (ewoc-prev status proj-node))
	  (curr-node proj-node))
      ;; Let's start by removing the project from the project list:
      (setq project-buffer-projects-list (remove proj-name project-buffer-projects-list))

      ;; Check the cache:
      (when (string-equal (car project-buffer-cache-project) proj-name)
	(setq project-buffer-cache-project nil)
	(setq project-buffer-cache-subdirectory nil))

      ;; Delete the nodes:
      (let ((inhibit-read-only t))
	(while (and curr-node
		    (string-equal (project-buffer-node->project (ewoc-data curr-node)) proj-name))
	  (let ((next-node (ewoc-next status curr-node)))
	    (ewoc-delete status curr-node)
	    (setq curr-node next-node)
	    )))

      ;; Now: the master project may need to be readjusted
      (when (string-equal proj-name (car project-buffer-master-project))
	(if curr-node
	    ;; By default the next project become the new master one:
	    (progn  (setq project-buffer-master-project (cons (project-buffer-node->project (ewoc-data curr-node)) curr-node))
		    (ewoc-invalidate status curr-node))
	    ;; Otherwise: if the previous node is invalid, it's project will become the new master one:
	    (if prev-node
		(let ((prev-parent (project-buffer-node->parent (ewoc-data prev-node))))
		  (while (not (eq (project-buffer-node->type (ewoc-data prev-parent)) 'project))
		    (setq prev-parent (project-buffer-node->parent (ewoc-data prev-parent))))
		  (setq project-buffer-master-project (cons (project-buffer-node->project (ewoc-data prev-parent)) prev-parent))
		  (ewoc-invalidate status prev-parent))
		(setq project-buffer-master-project nil))))
      )))


(defun project-buffer-refresh-all-items(status)
  "Refresh all ewoc item from the buffer."
  (ewoc-map (lambda (info)  t) status)) ; (ewoc-refresh status) doesn't work properly.


(defun project-buffer-perform-action-hook(action)
  "Call the user hook to perform ACTION."
  (run-hook-with-args 'project-buffer-action-hook
		      action
		      (car project-buffer-master-project)
		      (project-buffer-node->filename (ewoc-data (cdr project-buffer-master-project)))
		      project-buffer-current-platform
		      project-buffer-current-build-configuration))


(defun project-buffer-search-and-mark-files(status regexp project marked-flag)
  "Search REGEXP in with all files if PROJECT is nil or in each file of the specified PROJECT.
If REGEXP is found, the marked-flag field associated to the file get set to MARKED-FLAG
The function returns the number of files whose marked-flag field changed"
  (let ((count 0))
    (ewoc-map (lambda (node)
		(when (and (eq (project-buffer-node->type node) 'file)				; check only files
			   (or (not project)							; ( if a project is specified,
			       (string-equal (project-buffer-node->project node) project))	;   make sure it matches the node's project )
			   (not (eq (project-buffer-node->marked node) marked-flag)))		; which aren't already (un)marked (based on request)
		  ;; Check if the file contain the regexp:
		  (let ((filename (project-buffer-node->filename node)))
		    (when (and filename
			       (file-readable-p filename)
			       (let ((fbuf (get-file-buffer filename)))
				 (message "Project '%s' -- Searching in '%s'" (project-buffer-node->project node) (project-buffer-node->name node))
				 (if fbuf
				     (with-current-buffer fbuf
				       (save-excursion
					 (goto-char (point-min))
					 (re-search-forward regexp nil t)))
				     (with-temp-buffer
				       (insert-file-contents filename)
				       (goto-char (point-min))
				       (re-search-forward regexp nil t)))))
		      (setf (project-buffer-node->marked node) marked-flag)
		      (setq count (1+ count))
		      t  )))) ; to force the update of the display.
	      status)
    count))


(defun project-buffer-refine-mark-files(status regexp marked-flag)
  "Search REGEXP in with all marked files.
If REGEXP is found, the marked-flag field associated to the file get set to MARKED-FLAG
The function returns the number of files whose marked-flag field changed
Note: if no files are marked, the search will occur in all existing files of the project"
  (let ((count 0)
	marked-file-found)
    (ewoc-map (lambda (node)
		(when (and (eq (project-buffer-node->type node) 'file)	; check only files
			   (project-buffer-node->marked node))		; which are already marked
		  (setq marked-file-found t)
		  ;; Check if the file contain the regexp:
		  (let ((filename (project-buffer-node->filename node)))
		    (when (and filename
			       (file-readable-p filename)
			       (let ((found (let ((fbuf (get-file-buffer filename)))
					      (message "Project '%s' -- Searching in '%s'" (project-buffer-node->project node) (project-buffer-node->name node))
					      (if fbuf
						  (with-current-buffer fbuf
						    (save-excursion
						      (goto-char (point-min))
						      (re-search-forward regexp nil t)))
						  (with-temp-buffer
						    (insert-file-contents filename)
						    (goto-char (point-min))
						    (re-search-forward regexp nil t))))))
				 (or (and found (not marked-flag))
				     (and (not found) marked-flag))))
		      (setf (project-buffer-node->marked node) nil)
		      (setq count (1+ count))
		      t  )))) ; to force the update of the display.
	      status)
    (if marked-file-found
	count
	( - 0 (project-buffer-search-and-mark-files status regexp nil marked-flag)))))


(defun project-buffer-set-master-project(status project-name)
  "Set PROJECT-NAME to be the new master project."
  (let ((old-node (cdr project-buffer-master-project))
	(cur-node (project-buffer-search-project-node status project-name)))
    (when cur-node
      ;; Let's replace the old node by the new one
      (setq project-buffer-master-project (cons (project-buffer-node->name (ewoc-data cur-node)) cur-node))
      ;; Force the refresh:
      (ewoc-invalidate status old-node)
      (ewoc-invalidate status cur-node)
      (ewoc-goto-node status cur-node))))



(defun project-buffer-raw-print-hooks(hook-symbol hook-list)
  "Print a hooks block in the current buffer."
  (print (list 'begin 'hook hook-symbol) (current-buffer))
  (while hook-list
    (let ((hook-item (pop hook-list)))
      (print (cond ((booleanp hook-item)
		    (list 'value hook-item))
		   ((symbolp hook-item)
		    (list 'symbol hook-item  (abbreviate-file-name (symbol-file hook-item))))
		   ((functionp hook-item)
		    (list 'value hook-item))
		   (t (error "Unknown type found in the hook list")))
	     (current-buffer))))
  (print (list 'end 'hook hook-symbol) (current-buffer)))


(defun project-buffer-raw-print-locals(local-list)
  "Print a local block in the current-buffer."
  (print (list 'begin 'locals) (current-buffer))
  (while local-list
    (print (pop local-list) (current-buffer)))
  (print (list 'end 'locals) (current-buffer)))


(defun project-buffer-read-header(status data-buffer &optional set-buffer-name set-current-directory)
  "Read the header of the saved file from the DATA-BUFFER."
  (let ((header-data (read data-buffer)))
    (unless (and header-data
		 (listp header-data)
		 (eq (car header-data) 'project-buffer-mode))
      (error "Not in project-buffer save file"))
    ;; The header list is: '(project-buffer-mode version buffer-name directory)
    (when set-buffer-name
      (rename-buffer (nth 2 header-data) t))
    (when set-current-directory
      (cd (nth 3 header-data)))
    ;; Finally, let's return the version:
    (nth 1 header-data)))


(defun project-buffer-read-block-hook(status data-buffer block-header run-mode-hooks)
  "Read a project-buffer-hook block; set the local hook and
attempt to load the definition file if a hook function isnt't bound."
  ;; block-header should be: '(begin hook hook-symbol)
  (unless (and (listp block-header)
	       (eq (car block-header) 'begin)
	       (eq (nth 1 block-header) 'hook)
	       (= (length block-header) 3))
    (error "Invalid block-header"))
  (let ((hook-symbol (nth 2 block-header)))
    (unless (symbolp hook-symbol)
      (error "Invalid block-header"))
    (if (and (boundp hook-symbol)
	     (listp (eval hook-symbol)))
	;; If the hook variable exists:
	(let ((block-line (read data-buffer)))
	  (add-to-list 'project-buffer-hooks-to-save hook-symbol)
	  (while (and block-line
		      (not (and  (listp block-line)
				 (eq (car block-line) 'end)
				 (eq (nth 1 block-line) 'hook)
				 (eq (nth 2 block-line) hook-symbol))))
	    (if (listp block-line)
		(cond ((eq (car block-line) 'symbol)
		       (let ((func (nth 1 block-line))
			     (file (nth 2 block-line)))
			 (add-hook hook-symbol func nil t)
			 (when (and (not (fboundp func))
				    (file-exists-p file)
				    (file-readable-p file))
			   (load-file file))))
		      ((eq (car block-line) 'value)
		       (add-hook hook-symbol (nth 1 block-line) nil t))
		      (t (error "Unknown hook type: %s!" (car block-line))))
		(error "Unknown hook line: %s" block-line))
	    (setq block-line (read data-buffer)))
	  ;; Check if it's the mode-hook:
	  (when (and run-mode-hooks
		     (eq hook-symbol 'project-buffer-mode-hook))
	    (run-hooks 'project-buffer-mode-hook))
	  )
	;; If the hook variable doesn't exist, we just skip the block:
	(project-buffer-skip-block status data-buffer block-header))))


(defun project-buffer-read-block-node-list(status data-buffer block-header)
  "Read a project-buffer-node-list block; add each node to the
project-buffer context."
  ;; block-header should be: '(begin node-list)
  (unless (and (listp block-header)
	       (eq (car block-header) 'begin)
	       (eq (nth 1 block-header) 'node-list))
    (error "Invalid block-header"))
  (let ((block-line (read data-buffer)))
    (while (and block-line
		(not (and  (listp block-line)
			   (eq (car block-line) 'end)
			   (eq (nth 1 block-line) 'node-list))))
      (if (and (listp block-line)
	       (> (length block-line) 5))
	  (let ((name                      (nth 0 block-line))
		(type                      (nth 1 block-line))
		(filename                  (nth 2 block-line))
		(project                   (nth 3 block-line))
		(platform-list             (nth 4 block-line))
		(build-configurations-list (nth 5 block-line))
		(user-data                 (and (> (length block-line) 6) (nth 6 block-line)))
		(project-settings          (and (> (length block-line) 7) (nth 7 block-line))))
	    (let ((data (project-buffer-create-node name type filename project)))
	      (project-buffer-insert-node status data)
	      (when platform-list
		(project-buffer-set-project-platforms-data status project platform-list))
	      (when build-configurations-list
		(project-buffer-set-project-build-configurations-data status project build-configurations-list))
	      (when user-data
		(setf (project-buffer-node->user-data data) user-data))
	      (when project-settings
		(setf (project-buffer-node->project-settings data) project-settings))
	      ))
	  (error "Unknown node-list line: %s" block-line))
      (setq block-line (read data-buffer)))))


(defun project-buffer-read-block-locals(status data-buffer block-header)
  "Read a project-buffer-locals block; set the local variable of
the buffer with their specified values.  Skip non local
variable."
  ;; block-header should be: '(begin locals)
  (unless (and (listp block-header)
	       (eq (car block-header) 'begin)
	       (eq (nth 1 block-header) 'locals))
    (error "Invalid block-header"))
  (let ((block-line (read data-buffer)))
    (while (and block-line
		(not (and  (listp block-line)
			   (eq (car block-line) 'end)
			   (eq (nth 1 block-line) 'locals))))
      (if (and (listp block-line)
	       (symbolp (car block-line)))
	  (progn (unless (local-variable-p (car block-line))
		   (make-local-variable (car block-line)))
		 (set (car block-line) (cdr block-line))
		 (add-to-list 'project-buffer-locals-to-save (car block-line)))
	  (error "Unknown local line: %s" block-line))
      (setq block-line (read data-buffer)))))



(defun project-buffer-skip-block(status data-buffer block-header)
  "Skip project-buffer block."
  (unless (and (listp block-header)
	       (eq (car block-header) 'begin))
    (error "Invalid block-header"))
  (let ((block-line (read data-buffer))
	(block-type (nth 1 block-header)))
    (while (and block-line
		(not (and  (listp block-line)
			   (eq (car block-line) 'end)
			   (eq (nth 1 block-line) block-type))))
      (setq block-line (read data-buffer)))))


(defun project-buffer-read-line-master-project(status block-header)
  "Read the project-buffer-master-project line."
  (unless (and (listp block-header)
	       (eq (car block-header) 'one-line)
	       (eq (nth 1 block-header) 'master-project))
    (error "Invalid block-header"))
  (project-buffer-set-master-project status (nth 2 block-header)))


(defun project-buffer-read-block(status data-buffer run-mode-hooks)
  "Read and parse the next block from the DATA-BUFFER."
  (let ((block-header (read data-buffer))
	(goon t))
    (if (listp block-header)
	(cond ((eq (car block-header) 'begin)
	       (cond ((eq (nth 1 block-header) 'hook)
		      (project-buffer-read-block-hook status data-buffer block-header run-mode-hooks))
		     ((eq (nth 1 block-header) 'node-list)
		      (project-buffer-read-block-node-list status data-buffer block-header))
		     ((eq (nth 1 block-header) 'locals)
		      (project-buffer-read-block-locals status data-buffer block-header))
		     (t
		      (project-buffer-skip-block status data-buffer block-header))))
	      ((eq (car block-header) 'one-line)
	       (cond ((eq (nth 1 block-header) 'master-project)
		      (project-buffer-read-line-master-project status block-header)))))
	(setq goon (not (and (symbolp block-header) (eq block-header 'eof))))
      )
    goon) ;; carry on
  )


(defun project-buffer-set-view-mode(status view-mode)
  "Set the view mode to VIEW-MODE."
  (unless (eq project-buffer-view-mode view-mode)
    (let ((node (ewoc-locate project-buffer-status)))
      (setq project-buffer-view-mode view-mode)
      (message "View mode set to: %s" project-buffer-view-mode)
      (project-buffer-refresh-all-items status)
      (project-buffer-refresh-ewoc-hf status)
      (ewoc-goto-node status node))))


(defun project-buffer-search-node(status name project)
  "Search a node named NAME which belongs to PROJECT."
  (let ((node           (ewoc-nth status 0))
	(folder-data    (project-buffer-extract-folder name 'file))
	(proj-data      project)
	(found          nil)
	(folder-found   nil)
	(node-data      nil))

    ;; Before checking the cache; let's check the current node:
    (let* ((cur-node (ewoc-locate status))
	   (cur-data (and cur-node (ewoc-data cur-node))))
      (setq found (and cur-node
		       (string-equal (project-buffer-node->project cur-data) project)
		       (string-equal (project-buffer-node->name cur-data) name)
		       cur-node)))

    ;; Cache check: <no cache update>
    (when (and (not found) project-buffer-cache-project)
      (cond
       ;; cache-project < current-project -> we can start the search from here (at least).
       ((string-lessp (car project-buffer-cache-project) proj-data)
	(setq node (cdr project-buffer-cache-project)))

       ;; cache-project == current-project -> check the folders...
       ((string-equal (car project-buffer-cache-project) proj-data)
	;; cache-subdir < current-subdir -> we can start from here.
	;; cache-subdir = current-subdir -> good starting point
	(if (and project-buffer-cache-subdirectory
		 folder-data
		 (or (string-equal (car project-buffer-cache-subdirectory) folder-data)
		     (project-buffer-directory-lessp (car project-buffer-cache-subdirectory) folder-data 'folder)))
	    (setq node (cdr project-buffer-cache-subdirectory))
	    (setq node (cdr project-buffer-cache-project))))
       ;; other wise: cache miss...
       ))

    ;; Search the node:
    (while (and node (not found))
      (setq node-data (ewoc-data node))

      (cond
       ;; data.project < node.project -> not found...
       ((string-lessp proj-data (project-buffer-node->project node-data))
	(setq node nil))

       ;; node.project == data.project -> check folder/file name
       ((string-equal proj-data (project-buffer-node->project node-data))
	(let* ((folder-db (project-buffer-extract-folder (project-buffer-node->name node-data) (project-buffer-node->type node-data)))
	       (type-db   (project-buffer-node->type node-data)))
	  ;; Make sure it's not the project line:
	  (unless (eq type-db 'project)
	    (setq found (and (string-equal (project-buffer-node->name node-data) name) node))))))

      ;; next node:
      (setq node (and node (ewoc-next status node))))

    ;; Final result:
    found
    ))


;;
;;  External functions:
;;


(defun project-buffer-mode (&optional skip-mode-hooks)
  "Major mode to view project.

Commands:
\\{project-buffer-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq mode-name "project-buffer"
	major-mode 'project-buffer-mode
	buffer-read-only t)
  (use-local-map project-buffer-mode-map)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (let ((status (ewoc-create 'project-buffer-prettyprint "" "" t)))
      (make-local-variable 'project-buffer-status)
      (make-local-variable 'project-buffer-view-mode)
      (make-local-variable 'project-buffer-cache-project)
      (make-local-variable 'project-buffer-cache-subdirectory)
      (make-local-variable 'project-buffer-platforms-list)
      (make-local-variable 'project-buffer-current-platform)
      (make-local-variable 'project-buffer-build-configurations-list)
      (make-local-variable 'project-buffer-current-build-configuration)
      (make-local-variable 'project-buffer-master-project)
      (make-local-variable 'project-buffer-projects-list)
      (make-local-variable 'project-buffer-file-name)
      (make-local-variable 'project-buffer-locals-to-save)
      (make-local-variable 'project-buffer-hooks-to-save)

      (setq project-buffer-status status)
      (setq project-buffer-view-mode 'folder-view)
      (setq project-buffer-cache-project nil)
      (setq project-buffer-cache-subdirectory nil)
      (setq project-buffer-platforms-list nil)
      (setq project-buffer-current-platform nil)
      (setq project-buffer-build-configurations-list nil)
      (setq project-buffer-current-build-configuration nil)
      (setq project-buffer-master-project nil)
      (setq project-buffer-projects-list nil)
      (setq project-buffer-file-name nil)
      (setq project-buffer-locals-to-save '(project-buffer-view-mode project-buffer-current-platform project-buffer-current-build-configuration))
      (setq project-buffer-hooks-to-save '(project-buffer-mode-hook project-buffer-action-hook project-buffer-post-load-hook project-buffer-post-find-file-hook project-buffer-refresh-hook))

      (project-buffer-refresh-ewoc-hf status)

      (unless skip-mode-hooks
	(run-hooks 'project-buffer-mode-hook))
      )))


(defun project-buffer-insert (name type filename project)
  "Insert a file in alphabetic order in it's project/directory.

NAME is the name of the file in the project with it's virtual project directory,
both name and directory may be virtual
TYPE type of the node in the project: should be either 'project or 'file
FILENAME should be either a full path to the project's file or a relative path based
on the current directory of the buffer
PROJECT is the name of the project in which to insert the node
note: regarding the project node, it's recommended to have NAME = PROJECT"
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-insert-node project-buffer-status
			      (project-buffer-create-node name type filename project)))

(defun project-buffer-delete-file (name project &optional dont-delete-project)
  "Delete the node named NAME which belongs to PROJECT.
Empty folder node will also be cleared up.  If no more file
remain in the project; the project will also be deleted unless
DONT-DELETE-PROJECT is set."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-delete-file-node project-buffer-status name project dont-delete-project))


(defun project-buffer-delete-folder (name project &optional dont-delete-project)
  "Delete the node named NAME which belongs to PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-delete-folder-node project-buffer-status
				     (project-buffer-search-node project-buffer-status name project)
				     dont-delete-project))


(defun project-buffer-delete-project (project)
  "Delete the project PROJECT.
Each files/folder under the project will also be deleted."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-delete-project-node project-buffer-status
				      project
				      (project-buffer-search-project-node project-buffer-status project)))


(defun project-buffer-set-project-platforms (project platform-list)
  "Attached the list of platform contained in PLATFORM-LIST to the project named PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-set-project-platforms-data project-buffer-status
					     project
					     platform-list))

(defun project-buffer-set-project-build-configurations (project build-configuration-list)
  "Attached the list build configurations in BUILD-CONFIGURATION-LIST to the project named PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-set-project-build-configurations-data project-buffer-status
							project
							build-configuration-list))


(defun project-buffer-raw-save (filename)
  "Save the project data in FILENAME; the project can later be
reloaded through `project-buffer-raw-load' function."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status                  project-buffer-status)
	 (node                    (ewoc-nth status 0))
	 (buf-name                (buffer-name))
	 (buf-dir                 default-directory)
	 (project-buffer          (current-buffer))
	 (hooks-list              (mapcar (lambda (item) (cons item (and (local-variable-p item) (eval item))))
					  project-buffer-hooks-to-save))
	 (locals-list             (remove nil
				   (mapcar (lambda (item) (and (local-variable-p item) (cons item (eval item))))
					   project-buffer-locals-to-save))))
    (with-temp-buffer
      ;; First, let's write a quick header:
      (print (list 'project-buffer-mode
		   project-buffer-mode-version
		   buf-name
		   buf-dir) (current-buffer))
      ;; Save the hooks:
      (mapcar (lambda (item) (when (cdr item) (project-buffer-raw-print-hooks (car item) (cdr item))))
	      hooks-list)
      ;; Save the locals:
      (project-buffer-raw-print-locals locals-list)
      ;; Save each nodes:
      (print (list 'begin 'node-list) (current-buffer))
      (while node
	(let ((data (ewoc-data node)))
	  (unless (eq (project-buffer-node->type data) 'folder)
	    (print (list (project-buffer-node->name data)
			 (project-buffer-node->type data)
			 (project-buffer-node->filename data)
			 (project-buffer-node->project data)
			 (project-buffer-node->platform-list data)
			 (project-buffer-node->build-configurations-list data)
			 (project-buffer-node->user-data data)
			 (project-buffer-node->project-settings data))
		   (current-buffer))))
	(setq node (ewoc-next status node)))
      (print (list 'end 'node-list) (current-buffer))
      ;; Save the master project:
      (print (list 'one-line 'master-project (car (buffer-local-value 'project-buffer-master-project project-buffer)))
	     (current-buffer))
      ;; End of file:
      (print 'eof (current-buffer))
      ;; Finally: write the file.
      (write-file filename))))


(defun project-buffer-raw-load (filename &optional set-buffer-name run-mode-hooks)
  "Load a project saved by `project-buffer-raw-data'.
This function does not restore the mode and assume the
project-buffer-mode to be set.  It doesn't clear the existing
nodes either."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((project-buffer (current-buffer))
	(status project-buffer-status))
    (with-temp-buffer
      (insert-file filename)
      (goto-char (point-min))
      (let ((data-buffer (current-buffer))
	    data-version
	    block-header)
	(with-current-buffer project-buffer
	  (setq data-version (project-buffer-read-header status data-buffer set-buffer-name t))
	  ;; The rest of the file is defined by blocks:
	  (while (project-buffer-read-block status data-buffer run-mode-hooks))
	  )))
    (run-hooks 'project-buffer-post-load-hook)
    ))


(defun project-buffer-set-file-user-data (name project user-data)
  "Attach user data to a node named NAME in the project PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (project-buffer-search-node project-buffer-status name project)))
    (when node
      (setf (project-buffer-node->user-data (ewoc-data node)) user-data))))


(defun project-buffer-set-project-user-data (project user-data)
  "Attach user data to the project node named PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (project-buffer-search-project-node project-buffer-status project)))
    (when node
      (setf (project-buffer-node->user-data (ewoc-data node)) user-data))))


(defun project-buffer-get-file-user-data (name project)
  "Retrieve user data to a node named NAME in the project PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (project-buffer-search-node project-buffer-status name project)))
    (when node
      (project-buffer-node->user-data (ewoc-data node)))))


(defun project-buffer-get-project-user-data (project)
  "Retrieve user data to the project node named PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (project-buffer-search-project-node project-buffer-status project)))
    (when node
      (project-buffer-node->user-data (ewoc-data node)))))


(defun project-buffer-get-current-project-name ()
  "Retrieve the name of the project the cursor is on."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (ewoc-locate project-buffer-status)))
    (when node
      (project-buffer-node->project (ewoc-data node)))))


(defun project-buffer-get-current-file-data ()
  "Retrieve data about the current file the cursor is on.
Return nil if the cursor is not on a file.
If non-nil the return value is a list containing:
  '(project-file-name file-path project-name)"
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (data (and node (ewoc-data node))))
    (when (and data (eq (project-buffer-node->type data) 'file))
      (list (project-buffer-node->name data)
	    (project-buffer-node->filename data)
	    (project-buffer-node->project data)))))


(defun project-buffer-set-project-settings-data (project settings-data)
  "Attach SETTINGS-DATA to the project node named PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (project-buffer-search-project-node project-buffer-status project)))
    (when node
      (setf (project-buffer-node->project-settings (ewoc-data node)) settings-data))))


(defun project-buffer-get-project-settings-data (project)
  "Retrieve the project settings from PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (project-buffer-search-project-node project-buffer-status project)))
    (when node
      (project-buffer-node->project-settings (ewoc-data node)))))


(defun project-buffer-exists-p (name project)
  "Return true if a node NAME exists in PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (project-buffer-search-node project-buffer-status name project)))
    (and node t)))


(defun project-buffer-project-exists-p (project)
  "Return true if the project PROJECT exists."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (project-buffer-search-project-node project-buffer-status project)))
    (and node t)))


(defun project-buffer-get-project-path (project)
  "Return the path/file attached to the project PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (project-buffer-search-project-node project-buffer-status project)))
    (and node (project-buffer-node->filename (ewoc-data node)))))


(defun project-buffer-get-file-path (name project)
  "Retrieve the path of the file NAME in PROJECT."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (project-buffer-search-node project-buffer-status name project)))
    (when node
      (project-buffer-node->filename (ewoc-data node)))))


(defun project-buffer-get-current-node-type ()
  "Retrieve the type of the current node."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (ewoc-locate project-buffer-status)))
    (when node
      (project-buffer-node->type (ewoc-data node)))))


(defun project-buffer-get-current-node-name ()
  "Retrieve the type of the current node."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (ewoc-locate project-buffer-status)))
    (when node
      (project-buffer-node->name (ewoc-data node)))))


(defun project-buffer-get-marked-node-list ()
  "Retrieve the list of marked files.
Each node of the returned list are also list as:
  '(project-file-name file-path project-name)"
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-nth status 0))
	 marked-node-list)
    (while node
      (let ((node-data (ewoc-data node)))
	(when (and (eq (project-buffer-node->type node-data) 'file)
		   (project-buffer-node->marked node-data))
	  (setq marked-node-list (cons (list (project-buffer-node->name node-data)
					     (project-buffer-node->filename node-data)
					     (project-buffer-node->project node-data))
				       marked-node-list))))
      (setq node (ewoc-next status node)))
    (reverse marked-node-list)
))


(defun project-buffer-apply-to-each-file(func &rest args)
  "Call FUNC for each existing file nodes.
FUNC's prototype must be: 
  (lambda (project-file-name file-path project-name &rest ARGS) ...)"
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-nth status 0)))
    (while node
      (let ((node-data (ewoc-data node)))
	(when (eq (project-buffer-node->type node-data) 'file)
	  (apply func
		 (project-buffer-node->name node-data)
		 (project-buffer-node->filename node-data)
		 (project-buffer-node->project node-data)
		 args))
	(setq node (ewoc-next status node))))))


(defun project-buffer-apply-to-marked-files(func &rest args)
  "Call FUNC for each marked file nodes.
FUNC's prototype must be:
  (lambda (project-file-name file-path project-name &rest ARGS) ...)"
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-nth status 0))
	 found-marked-files)
    (while node
      (let ((node-data (ewoc-data node)))
	(when (and (eq (project-buffer-node->type node-data) 'file)
		   (project-buffer-node->marked node-data))
	  (setq found-marked-files t)
	  (apply func
		 (project-buffer-node->name node-data)
		 (project-buffer-node->filename node-data)
		 (project-buffer-node->project node-data)
		 args))
	(setq node (ewoc-next status node))))
    found-marked-files))


(defun project-buffer-apply-to-project-files(project func &rest args)
  "Call FUNC for each file nodes in PROJECT.
FUNC's prototype must be:
  (lambda (project-file-name file-path project-name &rest ARGS) ...)"
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (project-buffer-search-project-node status project)))
    (while (and node
		(string= (project-buffer-node->project (ewoc-data node)) project))
      (let ((node-data (ewoc-data node)))
	(when (eq (project-buffer-node->type node-data) 'file)
	  (apply func
		 (project-buffer-node->name node-data)
		 (project-buffer-node->filename node-data)
		 (project-buffer-node->project node-data)
		 args))
	(setq node (ewoc-next status node))))))



;;
;;  Interactive commands:
;;


(defun project-buffer-goto-dir-up ()
  "Go to the project/folder containing the current file/folder."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate status)))
    (setq node (and node (project-buffer-find-node-up status node)))
    (when node
      (ewoc-goto-node status node))))


(defun project-buffer-goto-dir-up-or-collapsed ()
  "Go to the project/folder containing the current file/folder unless the cursor is on a expanded folder/project in which case, it will collapse it."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate status))
	 (node-data (and node (ewoc-data node))))
    (when node
      (if (or (eq (project-buffer-node->type node-data) 'file)
	      (project-buffer-node->collapsed node-data))
	  (progn (setq node (and node (project-buffer-find-node-up status node)))
		 (when node (ewoc-goto-node status node)))
	  (project-buffer-toggle-expand-collapse)
	  ))))


(defun project-buffer-search-forward-regexp (regexp)
  "Search file matching REGEXP."
  (interactive "sSearch forward (regexp): ")
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-clear-matched-mark project-buffer-status)
  (when (and regexp
	     (> (length regexp) 0))
    (let* ((status project-buffer-status)
	   (node (ewoc-locate status)))
      (project-buffer-mark-matching-file project-buffer-status regexp)
      ;; goto first match
      (while (and node
		  (or (not (eq (project-buffer-node->type (ewoc-data node)) 'file))
		      (not (project-buffer-node->matched (ewoc-data node)))))
	(setq node (ewoc-next status node)))
      ;; if failed: go to the last search instead
      (unless node
	(setq node (ewoc-locate status))
	(while (and node
		    (or (not (eq (project-buffer-node->type (ewoc-data node)) 'file))
			(not (project-buffer-node->matched (ewoc-data node)))))
	  (setq node (ewoc-prev status node))))
      (if node
	(ewoc-goto-node status node)
	(message "Search failed: %s." regexp)))))


(defun project-buffer-goto-next-match ()
  "Go to the next matching."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate status)))
    (if node (setq node (ewoc-next status node)))
    ;; goto first match
    (while (and node
		(or (not (eq (project-buffer-node->type (ewoc-data node)) 'file))
		    (not (project-buffer-node->matched (ewoc-data node)))))
      (setq node (ewoc-next status node)))
    (if node
	(ewoc-goto-node status node)
	(message "Failing forward search."))))


(defun project-buffer-goto-prev-match ()
  "Go to the previous matching."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate status)))
    (if node (setq node (ewoc-prev status node)))
    ;; goto first match
    (while (and node
		(or (not (eq (project-buffer-node->type (ewoc-data node)) 'file))
		    (not (project-buffer-node->matched (ewoc-data node)))))
      (setq node (ewoc-prev status node)))
    (if node
	(ewoc-goto-node status node)
	(message "Failing backward search."))))


(defun project-buffer-quit ()
  "Burry project-buffer mode or cancel the research."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless (project-buffer-clear-matched-mark project-buffer-status)
    (bury-buffer)))


(defun project-buffer-help ()
  "Display help for project-buffer mode."
  (interactive)
  (describe-function 'project-buffer-mode))


(defun project-buffer-next-file (&optional n)
  "Move the cursor down N files."
  (interactive "p")
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (ewoc-goto-next project-buffer-status n))


(defun project-buffer-next-file-or-expand ()
  "Go to the project/folder containing the current file/folder unless the cursor is on a expanded folder/project in which case, it will collapse it."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate status))
	 (node-data (and node (ewoc-data node))))
    (when node
      (if (or (eq (project-buffer-node->type node-data) 'file)
	      (not (project-buffer-node->collapsed node-data)))
	  (ewoc-goto-next status 1)
	  (project-buffer-toggle-expand-collapse)
	  ))))


(defun project-buffer-prev-file (&optional n)
  "Move the cursor up N files."
  (interactive "p")
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (ewoc-goto-prev project-buffer-status n))


(defun project-buffer-find-marked-files ()
  "Run find-files on the marked files."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((file-list (project-buffer-get-marked-nodes project-buffer-status))
	 (cnt 0)
	 buffer)
    (project-buffer-clear-matched-mark project-buffer-status)
    (while file-list
      (let ((node (pop file-list)))
	(when (eq (project-buffer-node->type node) 'file)
	  (setq buffer (find-file-noselect (project-buffer-node->filename node))
		cnt (1+ cnt)))))
    (cond ((> cnt 1) (message "Find %i files." cnt))
	  ((= cnt 1) (display-buffer buffer))
	  (t (message "No files selected")))))


(defun project-buffer-go-to-previous-project ()
  "Go to previous project line."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate project-buffer-status))
	 (search (ewoc-prev status node)))
    (while (and search
		(not (eq (project-buffer-node->type (ewoc-data search)) 'project)))
      (setq search (ewoc-prev status search)))
    (when search
      (ewoc-goto-node status search))))


(defun project-buffer-go-to-previous-folder-or-project ()
  "If the cursor is on a file, go up to the previous project/folder.
If the cursor is on a folder, search up for the previous project/folder.
If the cursor is on a project, go to previous project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate project-buffer-status))
	 (node-data (and node (ewoc-data node))))
    (cond ((eq (project-buffer-node->type node-data) 'file)
	   (project-buffer-goto-dir-up))
	  ((eq (project-buffer-node->type node-data) 'folder)
	   (let ((search (ewoc-prev status node)))
	     (while (and search
			 (eq (project-buffer-node->type (ewoc-data search)) 'file))
	       (setq search (ewoc-prev status search)))
	     (when search
	       (ewoc-goto-node status search))))
	  ((eq (project-buffer-node->type node-data) 'project)
	   (let ((search (ewoc-prev status node)))
	     (while (and search
			 (not (eq (project-buffer-node->type (ewoc-data search)) 'project)))
	       (setq search (ewoc-prev status search)))
	     (when search
	       (ewoc-goto-node status search))))
	  (t (error "Unknown node type! (%S)" (project-buffer-node->type node-data))))))


(defun project-buffer-go-to-next-project ()
  "Go to next project line."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status project-buffer-status)
	 (node (ewoc-locate project-buffer-status))
	 (search (ewoc-next status node)))
    (while (and search
		(not (eq (project-buffer-node->type (ewoc-data search)) 'project)))
      (setq search (ewoc-next status search)))
    (when search
      (ewoc-goto-node status search))))


(defun project-buffer-go-to-next-folder-or-project ()
  "If the cursor is on a file, go down to the next project/folder.
If the cursor is on a folder, search down for the next project/folder.
If the cursor is on a project, go to next project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status    project-buffer-status)
	 (node      (ewoc-locate project-buffer-status))
	 (node-data (and node (ewoc-data node)))
	 (fold-ok   (and node
			 (not (eq (project-buffer-node->type node-data) 'project))
			 (eq project-buffer-view-mode 'folder-view)))
	 (search    (and node (ewoc-next status node))))

    (while (and search
		(not (eq (project-buffer-node->type (ewoc-data search)) 'project))
		(not (and fold-ok
			  (eq (project-buffer-node->type (ewoc-data search)) 'folder))))
	    (setq search (ewoc-next status search)))
    (when search
      (ewoc-goto-node status search))))


(defun project-buffer-node-find-file ()
  "Find the file the cursor is on."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (project-buffer (current-buffer)))
    (project-buffer-clear-matched-mark project-buffer-status)
    (if (eq (project-buffer-node->type node-data) 'file)
	(let ((file-buffer (find-file (project-buffer-node->filename node-data))))
	  (run-hook-with-args 'project-buffer-post-find-file-hook project-buffer file-buffer))
	(project-buffer-toggle-expand-collapse))))


(defun project-buffer-mouse-find-file(event)
  "Find the file you click on."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-end event))))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      (if (get-text-property (point) 'mouse-face)
	  (project-buffer-node-find-file-other-window)))))


(defun project-buffer-node-find-file-other-window ()
  "Find the file the cursor is on in another window."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (project-buffer (current-buffer)))
    (project-buffer-clear-matched-mark project-buffer-status)
    (if (eq (project-buffer-node->type node-data) 'file)
	(let ((file-buffer (find-file-other-window (project-buffer-node->filename node-data))))
	  (run-hook-with-args 'project-buffer-post-find-file-hook project-buffer file-buffer))
	(project-buffer-toggle-expand-collapse))))


(defun project-buffer-mark-file ()
  "Mark the file that the cursor is on and move to the next one."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (status project-buffer-status))
    (cond
     ;; Mark the current file:
     ((eq (project-buffer-node->type node-data) 'file)
      (setf (project-buffer-node->marked node-data) t)
      (ewoc-invalidate status node)
      (ewoc-goto-next status 1))
     ;; Or all files which belong to the project:
     ((eq (project-buffer-node->type node-data) 'project)
      (let ((prj-name (project-buffer-node->name node-data)))
	(save-excursion
	  (setq node      (ewoc-next status node)
		node-data (and node (ewoc-data node)))
	  (while (and node (string-equal (project-buffer-node->project node-data) prj-name))
	    (when (eq (project-buffer-node->type node-data) 'file)
	      (setf (project-buffer-node->marked node-data) t)
	      (ewoc-invalidate status node))
	    (setq node      (ewoc-next status node)
		  node-data (and node (ewoc-data node)))))))
     ;; Or finally, all files which are under the current folder:
     ((eq (project-buffer-node->type node-data) 'folder)
      (let ((folder (project-buffer-node->name node-data)))
	(save-excursion
	  (setq node      (ewoc-next status node)
		node-data (and node (ewoc-data node)))
	  (while (and node
		      (not (eq (project-buffer-node->type node-data) 'project))
		      (project-buffer-parent-of-p (project-buffer-node->name node-data) folder))
	    (when (eq (project-buffer-node->type node-data) 'file)
	      (setf (project-buffer-node->marked node-data) t)
	      (ewoc-invalidate status node))
	    (setq node      (ewoc-next status node)
		  node-data (and node (ewoc-data node)))))))
     )))


(defun project-buffer-unmark-file ()
  "Unmark the file that the cursor is on and move to the next one."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (status project-buffer-status))
    (cond
     ;; Mark the current file:
     ((eq (project-buffer-node->type node-data) 'file)
      (setf (project-buffer-node->marked node-data) nil)
      (ewoc-invalidate project-buffer-status node)
      (when (eq node (ewoc-locate project-buffer-status))
	(ewoc-goto-next project-buffer-status 1)))
     ;; Or all files which belong to the project:
     ((eq (project-buffer-node->type node-data) 'project)
      (let ((prj-name (project-buffer-node->name node-data)))
	(save-excursion
	  (setq node      (ewoc-next status node)
		node-data (and node (ewoc-data node)))
	  (while (and node (string-equal (project-buffer-node->project node-data) prj-name))
	    (when (eq (project-buffer-node->type node-data) 'file)
	      (setf (project-buffer-node->marked node-data) nil)
	      (ewoc-invalidate status node))
	    (setq node      (ewoc-next status node)
		  node-data (and node (ewoc-data node)))))))
     ;; Or finally, all files which are under the current folder:
     ((eq (project-buffer-node->type node-data) 'folder)
      (let ((folder (project-buffer-node->name node-data)))
	(save-excursion
	  (setq node      (ewoc-next status node)
		node-data (and node (ewoc-data node)))
	  (while (and node
		      (not (eq (project-buffer-node->type node-data) 'project))
		      (project-buffer-parent-of-p (project-buffer-node->name node-data) folder))
	    (when (eq (project-buffer-node->type node-data) 'file)
	      (setf (project-buffer-node->marked node-data) nil)
	      (ewoc-invalidate status node))
	    (setq node      (ewoc-next status node)
		  node-data (and node (ewoc-data node)))))))
     )))


(defun project-buffer-mark-all ()
  "Mark all files."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (ewoc-map (lambda (node) (when (and (eq (project-buffer-node->type node) 'file)
				      (not (project-buffer-node->marked node)))
                             (setf (project-buffer-node->marked node) t)))
	    project-buffer-status))


(defun project-buffer-unmark-all ()
  "Unmark all files."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (ewoc-map (lambda (node) (when (and (eq (project-buffer-node->type node) 'file)
				      (project-buffer-node->marked node))
                             (setf (project-buffer-node->marked node) nil) t))
	    project-buffer-status))


(defun project-buffer-toggle-all-marks ()
  "Toggle all file mark."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (ewoc-map (lambda (node) (when (eq (project-buffer-node->type node) 'file)
			     (setf (project-buffer-node->marked node) (not (project-buffer-node->marked node))) t))
	    project-buffer-status))


(defun project-buffer-toggle-expand-collapse-even-on-file ()
  "Expand / Collapse project and folder that the cursor is on.
If the cursor is on a file - search up for the nearest folder and collapse it."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node      (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (status    project-buffer-status))
    (project-buffer-clear-matched-mark status)
    (when (eq (project-buffer-node->type node-data) 'file)
      (setq node (and node (project-buffer-find-node-up status node)))
      (when node (ewoc-goto-node status node)))
    (when node
      (project-buffer-toggle-expand-collapse))))


(defun project-buffer-toggle-expand-collapse ()
  "Expand / Collapse project and folder that the cursor is on.
If the cursor is on a file - nothing will be done."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node      (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (status    project-buffer-status)
	 prj-sel
	 hidden-flag
	 project
	 skip-under
	 folder)
    (project-buffer-clear-matched-mark status)
    (unless (eq (project-buffer-node->type node-data) 'file)
      (when (eq (project-buffer-node->type node-data) 'folder)
	(setq folder (project-buffer-node->name node-data)))
      (setf (project-buffer-node->collapsed node-data) (not (project-buffer-node->collapsed node-data)))
      (setq hidden-flag (project-buffer-node->collapsed node-data))
      (setq prj-sel (eq (project-buffer-node->type node-data) 'project))
      (when prj-sel
	(setf (project-buffer-node->project-collapsed node-data) hidden-flag))
      (ewoc-invalidate status node)
      (setq project (project-buffer-node->project node-data)
	    node (ewoc-next status node))
      (while node
	(setq node-data (ewoc-data node))
	(when skip-under
	  (unless (project-buffer-parent-of-p (project-buffer-node->name  node-data) skip-under)
	    (setq skip-under nil)))
	(if (and (string-equal (project-buffer-node->project node-data) project)
		 (or (not folder)
		     (project-buffer-parent-of-p (project-buffer-node->name  node-data) folder)))
	    (progn
	      (when prj-sel
		(setf (project-buffer-node->project-collapsed node-data) hidden-flag)
		(ewoc-invalidate status node))
	      (unless skip-under
		(setf (project-buffer-node->hidden node-data) hidden-flag)
		(ewoc-invalidate status node)
		(if (and (eq (project-buffer-node->type node-data) 'folder)
			 (project-buffer-node->collapsed node-data)
			 (not hidden-flag))
		    (setq skip-under (project-buffer-node->name node-data))))
	      (setq node (ewoc-next status node)))
	    (setq node nil))))))


(defun project-buffer-set-folder-view-mode()
  "Set the view mode to folder-view."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-set-view-mode project-buffer-status 'folder-view))

(defun project-buffer-set-flat-view-mode()
  "Set the view mode to flat-view."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-set-view-mode project-buffer-status 'flat-view))

(defun project-buffer-set-folder-hidden-view-mode()
  "Set the view mode to folder-hidden-view."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-set-view-mode project-buffer-status 'folder-hidden-view))


(defun project-buffer-set-marked-view-mode()
  "Set the view mode to marked-view."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-set-view-mode project-buffer-status 'marked-view))


(defun project-buffer-toggle-view-mode ()
  "Toggle between the different view mode (folder-view / flat-view / folder-hidden-view)."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (ewoc-locate project-buffer-status)))
    (setq project-buffer-view-mode
	  (cond ((eq project-buffer-view-mode 'folder-view)        'flat-view)
		((eq project-buffer-view-mode 'flat-view)          'folder-hidden-view)
		((eq project-buffer-view-mode 'folder-hidden-view) 'marked-view)
		((eq project-buffer-view-mode 'marked-view)        'folder-view)
		))
    (let ((status project-buffer-status))
      (message "View mode set to: %s" project-buffer-view-mode)
      (project-buffer-refresh-all-items status)
      (project-buffer-refresh-ewoc-hf status)
      (ewoc-goto-node status node)
      )))


(defun project-buffer-toggle-search-mode()
  "Toggle between the different search-in-files mode (narrow-marked-files / all-files / current-project)."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (ewoc-locate project-buffer-status)))
    (setq project-buffer-search-in-files-mode
	  (cond ((eq project-buffer-search-in-files-mode 'narrow-marked-files) 'all-files)
		((eq project-buffer-search-in-files-mode 'all-files)         'current-project)
		((eq project-buffer-search-in-files-mode 'current-project)   'narrow-marked-files)))
    (let ((status project-buffer-status))
      (message "Search mode set to: %s" project-buffer-search-in-files-mode)
      (project-buffer-refresh-ewoc-hf status)
      (ewoc-goto-node status node)
      )))


(defun project-buffer-choose-build-configuration()
  "Ask the user for the build configuration using a completion list"
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless project-buffer-build-configurations-list (error "No build configuration available"))
  (if (cdr project-buffer-build-configurations-list)
      (let ((new-build-configuration (completing-read "Build-Configuration: " project-buffer-build-configurations-list nil t)))
	(when (and new-build-configuration (> (length new-build-configuration) 0))
	  (setq project-buffer-current-build-configuration new-build-configuration)))
      (message "This is the only one build configuration available."))
  (project-buffer-refresh-ewoc-hf project-buffer-status))


(defun project-buffer-next-build-configuration ()
  "Select next build configuration (rotate through them)."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless project-buffer-build-configurations-list (error "No build configuration available"))
  (if (cdr project-buffer-build-configurations-list)
      (let ((current (member project-buffer-current-build-configuration project-buffer-build-configurations-list)))
	(unless current (error "The current build configuration is invalid"))
	(setq project-buffer-current-build-configuration (or (and (cdr current) (cadr current))
							 (car project-buffer-build-configurations-list)))
	(message "Build configuration set to: %s" project-buffer-current-build-configuration))
      (message "This is the only one build configuration available."))
  (project-buffer-refresh-ewoc-hf project-buffer-status))


(defun project-buffer-choose-platform ()
  "Ask the user for the platform using a completion list."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless project-buffer-platforms-list (error "No build configuration available"))
  (if (cdr project-buffer-platforms-list)
      (let ((new-platform (completing-read "Platform: " project-buffer-platforms-list nil t)))
	(when (and new-platform (> (length new-platform) 0))
	  (setq project-buffer-current-platform new-platform)))
      (message "This is the only one platform available."))
  (project-buffer-refresh-ewoc-hf project-buffer-status))


(defun project-buffer-next-platform ()
  "Select next platform (rotate through them)."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless project-buffer-platforms-list (error "No build configuration available"))
  (if (cdr project-buffer-platforms-list)
      (let ((current (member project-buffer-current-platform project-buffer-platforms-list)))
	(unless current (error "The current build configuration is invalid"))
	(setq project-buffer-current-platform (or (and (cdr current) (cadr current))
						    (car project-buffer-platforms-list)))
	(message "Platform set to: %s" project-buffer-current-platform))
      (message "This is the only one platform available."))
  (project-buffer-refresh-ewoc-hf project-buffer-status))


(defun project-buffer-choose-master-project ()
  "Prompt the user for the master project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((status    project-buffer-status)
	(proj-name (completing-read "Enter the master project: " project-buffer-projects-list nil t)))
    (when (and proj-name
	      (> (length proj-name) 0))
      (project-buffer-set-master-project status proj-name))))


(defun project-buffer-select-current-as-master-project ()
  "Make the current project the new master project."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((status project-buffer-status)
	(old-node (cdr project-buffer-master-project))
	(cur-node (ewoc-locate project-buffer-status)))
    ;; Search for the project node:
    (while (and cur-node
		(not (eq (project-buffer-node->type (ewoc-data cur-node)) 'project)))
      (setq cur-node (project-buffer-find-node-up status cur-node)))
    ;; Let's replace the old node by the new one
    (setq project-buffer-master-project (cons (project-buffer-node->name (ewoc-data cur-node))
					      cur-node))
    ;; Force the refresh:
    (ewoc-invalidate status old-node)
    (ewoc-invalidate status cur-node)
    (ewoc-goto-node status cur-node)))


(defun project-buffer-perform-build-action ()
  "Run the user hook to perform the build action."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-perform-action-hook 'build))


(defun project-buffer-perform-clean-action ()
  "Run the user hook to perform the build action."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-perform-action-hook 'clean))


(defun project-buffer-perform-run-action ()
  "Run the user hook to perform the build action."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-perform-action-hook 'run))


(defun project-buffer-perform-debug-action ()
  "Run the user hook to perform the build action."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-perform-action-hook 'debug))

(defun project-buffer-perform-update-action ()
  "Run the user hook to perform the build action."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-perform-action-hook 'update))


(defun project-buffer-mark-files-containing-regexp (regexp &optional unmark)
  "Mark all files containing REGEXP -- A prefix argument means to UNMARK the files containing the REGEXP instead."
  (interactive
   (list (project-buffer-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
					     " files containing (regexp): "))
	 current-prefix-arg))
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node))
	 (current-project (project-buffer-node->project node-data))
	 (count (cond ((eq project-buffer-search-in-files-mode 'narrow-marked-files)
		      (project-buffer-refine-mark-files project-buffer-status regexp (not unmark)))
		     ((eq project-buffer-search-in-files-mode 'all-files)
		      (project-buffer-search-and-mark-files project-buffer-status regexp nil (not unmark)))
		     ((eq project-buffer-search-in-files-mode 'current-project)
		      (project-buffer-search-and-mark-files project-buffer-status regexp current-project (not unmark))))))
    (if (< count 0)
	(message "%i files marked." (- 0 count))
	(message "%i files %s."
		 count
		 (if (or unmark
			 (eq project-buffer-search-in-files-mode 'narrow-marked-files))
		     "unmarked" "marked")))
    (when project-buffer-autoswitch-marked-view-mode
      (unless (= count 0)
	(setq project-buffer-view-mode 'marked-view)
	(let ((status project-buffer-status))
	  (project-buffer-refresh-all-items status)
	  (project-buffer-refresh-ewoc-hf status)
	  (ewoc-goto-node status node))))
    ))


(defun project-buffer-mark-matched-files-or-current-file(force-marked-current)
  "Mark the matched files or the current file if no filename research are in progress or if FORCE-MARKED-CURRENT is set."
  (interactive "P")
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let (result)
    (unless (or force-marked-current
		(not (project-buffer-node->matched (ewoc-data (ewoc-locate project-buffer-status)))))
      (ewoc-map (lambda (node-data)
		  (when (and (eq (project-buffer-node->type node-data) 'file)
			     (project-buffer-node->matched node-data))
		    (setf (project-buffer-node->marked node-data) t)
		    (setq result t)))
		project-buffer-status))
    (unless result
      (project-buffer-mark-file))))


(defun project-buffer-unmark-matched-files-or-current-file(force-unmarked-current)
  "Unmark the matched files or the current file if no filename research are in progress or if FORCE-UNMARKED-CURRENT is set."
  (interactive "P")
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let (result)
    (unless (or force-unmarked-current
		(not (project-buffer-node->matched (ewoc-data (ewoc-locate project-buffer-status)))))
      (ewoc-map (lambda (node-data)
		  (when (and (eq (project-buffer-node->type node-data) 'file)
			     (project-buffer-node->matched node-data))
		    (setf (project-buffer-node->marked node-data) nil)
		    (setq result t)))
		project-buffer-status))
    (unless result
      (project-buffer-unmark-file))))


(defun project-buffer-view-file ()
  "Examine the current file using the view mode."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((node (ewoc-locate project-buffer-status))
	 (node-data (ewoc-data node)))
    (when (eq (project-buffer-node->type node-data) 'file)
      (view-file (project-buffer-node->filename node-data)))))


(defun project-buffer-delete-current-node()
  "Delete the current node."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((status project-buffer-status)
	(node (ewoc-locate project-buffer-status)))
    (when node
      (let* ((node-data (ewoc-data node))
	     (type      (project-buffer-node->type node-data))
	     (name      (project-buffer-node->name node-data)))
	(when (funcall project-buffer-confirm-function
		       (concat (format "Delete %s%s " name (if (eq type 'file) "" " and its content"))))
	  (message "Deleting %s..." name)
	  (cond ((eq type 'file)
		 (project-buffer-delete-node status node (not project-buffer-cleanup-empty-projects)))
		((eq type 'folder)
		 (project-buffer-delete-folder-node status node (not project-buffer-cleanup-empty-projects)))
		((eq type 'project)
		 (project-buffer-delete-project-node project-buffer-status name node))
		(t (error "Unknown data type"))))))))


(defun project-buffer-delete-marked-files()
  "Delete the marked files from the buffer."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((status project-buffer-status)
	node-list)
    (let ((node (ewoc-nth status 0))
	  node-data)
      (while node
	(setq node-data (ewoc-data node))
	(when (and (eq (project-buffer-node->type node-data) 'file)
		   (project-buffer-node->marked node-data))
	  (setq node-list (cons node node-list)))
	(setq node (ewoc-next status node))))

    (when node-list
      (let* ((lgt (length node-list))
	     (confirm-str (if (> lgt 1)
			      (format "Delete marked files [%i files] " lgt)
			      (format "Delete %s " (project-buffer-node->name (ewoc-data (car node-list))))))
	     (result-str  (format "%i deletion%s done" lgt (if (> lgt 1) "s" ""))))
	(when (funcall project-buffer-confirm-function confirm-str)
	  (while node-list
	    (project-buffer-delete-node status (pop node-list) (not project-buffer-cleanup-empty-projects)))
	  (message result-str))))))


(defun project-buffer-delete-current-node-or-marked-files()
  "Delete either the current node or the marked files.
The decision is based on the current node: if the current node is
marked, the deletion will attempt to delete all marked files;
otherwise only the current node (and potentially it's content)
will get deleted."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((node (ewoc-locate project-buffer-status)))
    (when node
      (if (and (eq (project-buffer-node->type (ewoc-data node)) 'file)
	       (project-buffer-node->marked (ewoc-data node)))
	  (project-buffer-delete-marked-files)
	  (project-buffer-delete-current-node)))))


(defun project-buffer-refresh(current-project-only)
  "Call the `project-buffer-refresh-hook' then redisplay every nodes."
  (interactive "P")
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (save-excursion
    (if current-project-only
	(run-hook-with-args 'project-buffer-refresh-hook (list (project-buffer-get-current-project-name)) 'current)
	(run-hook-with-args 'project-buffer-refresh-hook project-buffer-projects-list 'all))
    (project-buffer-refresh-all-items project-buffer-status)
    (project-buffer-refresh-ewoc-hf project-buffer-status)))


;;
;;  Global command:
;;


(defun project-buffer-write-file (filename)
  "Save the content of `project-buffer-mode' buffer to FILENAME."
  (interactive "FSave project to file: ")
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (project-buffer-raw-save filename)
  (setq project-buffer-file-name filename))


;;;###autoload
(defun project-buffer-find-file (filename)
  "Create a `project-buffer-mode' buffer based on the content of FILENAME."
  (interactive "fFind project: ")
  (let ((new-buffer (generate-new-buffer "*project-temp*")))
    (with-current-buffer new-buffer
      (project-buffer-mode t)
      (project-buffer-raw-load filename t t)
      (setq project-buffer-file-name filename))
    (switch-to-buffer new-buffer)))


(defun project-buffer-save-file ()
  "Save the content of the project-buffer-mode buffer into
`project-buffer-file-name'.  If `project-buffer-file-name' is
nil; the command will request a file name."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (if project-buffer-file-name
      (project-buffer-raw-save project-buffer-file-name)
      (call-interactively 'project-buffer-write-file)))


(defun project-buffer-revert ()
  "Revert the prvoject-buffer-mode buffer to match the content
from `project-buffer-file-name'."
  (interactive)
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (unless project-buffer-file-name (error "No file-name attached to this project-buffer"))
  (project-buffer-erase-all project-buffer-status)
  (project-buffer-raw-load project-buffer-file-name))



;;

(provide 'project-buffer-mode)

;;; project-buffer-mode.el ends here
