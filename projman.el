;;; projman.el --- Simple source file project management

;; Copyright (C) 2007,2008,2009  David Shilvock

;; Author: David Shilvock <davels@telus.net>
;; Version: 0.7
;; Keywords: tools

;; $Id: projman.el 176 2009-02-19 01:28:55Z dave $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; A simple global minor mode for working with a collection of source code files
;; in a directory tree, a 'project'.
;;
;; projman makes it easy to switch between multiple projects and automatically
;; setup your emacs environment correctly for each.  A project is identified by
;; a root directory and a series of project keywords, variables and hook
;; functions that are applied when the project is opened.  Each project can also
;; maintain a list of open buffers, closing them when quiting a project and
;; automatically restoring them when re-opening it (similar to
;; desktop-mode).  There are also a series of commands that can be applied to
;; the files of a project.
;;
;; Use `projman-projects' to define your projects and options (see docstring for
;; details).
;; 
;; eg.
;; (setq projman-projects
;;       '(("myproject"
;;          :root "c:/_proj/myproject"
;;          :ignore-dirs ("tests")
;;          :type c
;;          :open-hook (lambda () (global-auto-revert-mode 1))
;;          :compile-command "devenv.com c:/_proj/myproject/myproject.sln /build Debug"
;;           )))
;;
;; See `projman-mode' docstring for a complete list of commands
;; 
;; projman-switch-project - used to switch between projects
;; projman-create-project - create a new project definition on the fly
;;                          (only allows a limited set of options to be defined)
;; projman-switch-buffer  - switch to another project file using `iswtichb'
;; projman-dired-root     - open a dired buffer on the project root
;; projman-grep           - run rgrep in the project root 
;; projman-occur          - `multi-occur' against all open project buffers
;; projman-svn-status     - psvn status in the project root
;; projman-create-tags    - create a tags file for project
;;
;; If `projman-use-active-file' is non-nil (the default) projman will record the
;; open buffers when a project is closed.  These will then be automatically
;; visited when the project is later opened.  If `projman-lazy-load-buffers' is
;; non-nil this will be done is a lazy fashion.  These project state files will
;; be placed in `projman-active-directory' which should already exist.  See also
;; `projman-close-buffers-when-close-project' to automatically close the buffers
;; when existing a project.
;;
;; "C-cp" prefix is use for all keyboard commands.  If you want somthing else
;; set `projman-mode-prefix' before loading.
;;
;; TIPS
;; 
;; You can show the current project in the frame title.  Something similar could
;; be used to show it in the mode line as well.
;; (setq frame-title-format
;;       '("Emacs: %b %+%+ %f"
;;         (projman-project-name ("  [" projman-project-name "]"))))
;; 
;; NOTE
;; 
;; Because projman makes use of the find command it will only work on a unix
;; type environment, but windows+cygwin works fine provided that cygwin's find
;; command is used instead of the windows find command. See
;; `projman-find-command' for a way to specify  the command path if needed.
;;
;; TODO
;;
;; - run tags command in background
;; - currently all project options are saved to the active file along with the
;; open buffers.  Options specified in elisp always take precedence over any in
;; the active file but I'm not sure this is the best thing.
;; - allow more project options to be set interactively
;; - sub-projects: allows switching project options while sharing saved state
;; - more extensive use of customize

;;; Code:

(require 'iswitchb)
(eval-when-compile (require 'grep))

;; ==== USER VARIABLES

(defgroup projman nil
  "Customizing projman-mode"
  :group 'tools)

(defvar projman-projects nil
  "List of defined projects.
Each project is a list containing the project name followed by a series of
keywords and values.  Valid keywords are:
  * :root            - root directory for project.  Should always specify this.
  * :type            - type symbol see `projman-types'.
  * :globs           - list of glob patterns to match project files, overrides
                       `projman-types'
  * :tag-files       - list of 'other' tag files to include in tag searches.
  * :gtag-libs       - used to setup GTAGSLIBPATH env variable for global
  * :ignore-dirs     - list of directories to ignore in project.
                       see `grep-find-ignored-directories' for details.
  * :compile-command - set `compile-command' to this
  * :open-hook       - hook run when project is opened
  * :close-hook      - hook run when project is closed
  * :fileload-hook   - hook run when a project file is loaded

  Eg. '(\"Name\" :root \"/start/here/\" :type c)
")

(defvar projman-types
  '((c "C/C++" ("*.[CcHh]" "*.[CcHh][PpXx+][PpXx+]" "*.[Cc][Cc]" "*.[Hh][Hh]"))
    (elisp "elisp" ("*.el"))
    (python "Python" ("*.py"))
    (tg "TurboGears" ("*.py" "*.html" "*.kid" "*.js" "*.css"))
    (aspx "ASP.NET" ("*.as[pca]x" "*.asa" "*.cs" "*.js" "*.css"))
    )
  "List of project types.
Each type is a list of symbol, string name, and a list of glob patterns
  matching files to include in project searches.")

(defvar projman-project-switch-hook nil
  "Hook run after switching to a new project.")

(defvar projman-find-command "find"
  "Shell command to run the 'find' command.")

(defvar projman-tags-command "ctags"
  "Shell command to run the 'tags' command.")

(defvar projman-tags-args "-e -R"
  "Command args for `projman-tag-command'.
Args should include -e (Emacs format) and -R (recursive).")

(defvar projman-gtags-command "gtags"
  "Shell command to run the global 'gtags' command.")

(defvar projman-gtags-args nil
  "Command args for `projman-gtags-command'.")

(defvar projman-active-directory "~/projman/"
  "*Name of the directory that stores project active files.
This value must be an absolute directory name; thus on a GNU or Unix system, it
must end in a slash.")

(defvar projman-version-control nil
  "*Controls whether to make numbered backups of the projman active files.
It can have four values: t, nil, `never', and `nospecial'.  The first three have
the same meaning that they do for the variable `version-control', and the final
value `nospecial' means just use the value of `version-control'.")

(defvar projman-use-active-file t
  "If non-nil save/restore project options and buffers to/from active file.
The active file will be placed in `projman-active-directory' and named after the
  project name.")

(defvar projman-close-buffers-when-close-project nil
  "If non-nil all project buffers will be killed when closing a project.")

(defvar projman-lazy-load-buffers t
  "Non-nil to open active project buffers in a lazy fashion using idle time.")

(defvar projman-cache-project-file-list t
  "Non-nil to cache file list between calls of `projman-switch-buffer'.")

;; ==== INTERNAL

(defsubst projman-w32p ()
  (eq system-type 'windows-nt))

(defun projman-assoc-delete-all (key alist)
  "Delete all entries for key from alist, returning new list"
  (let (new)
    (while alist
	  (if (not (equal (caar alist) key))
	      (setq new (cons (car alist) new)))
	  (setq alist (cdr alist)))
	new))

(defun projman-merge-options (opts newopts)
  "Return a new option list consisting of all options from OPTS plus those in
NEWOPTS not already in OPTS.
An option list is a list of alternating :name and value items."
  (while newopts
    (let ((key (car newopts))
          (value (cadr newopts)))      
      (setq newopts (cddr newopts))
      (unless (memq key opts)
        (setq opts (append (list key value) opts)))))
  opts)

(defvar projman-temp-projects nil)

(defvar projman-current-project nil)
(defvar projman-project-name nil)

(defsubst projman-type-name (type)
  (nth 1 (assoc type projman-types)))

(defsubst projman-type-patterns (type)
  (nth 2 (assoc type projman-types)))

(defsubst projman-project-name ()
  (car projman-current-project))

(defsubst projman-project-root (&optional proj)
  (let ((dir (cadr (memq :root (or proj projman-current-project)))))
    (and dir (file-name-as-directory dir))))

(defsubst projman-project-type ()
  (cadr (memq :type projman-current-project)))

(defsubst projman-project-tag-files ()
  (cadr (memq :tag-files projman-current-project)))

(defsubst projman-project-gtag-libs ()
  (cadr (memq :gtag-libs projman-current-project)))

(defsubst projman-project-ignored-directories ()
  (cadr (memq :ignore-dirs projman-current-project)))

(defsubst projman-project-compile-command ()
  (cadr (memq :compile-command projman-current-project)))

(defsubst projman-project-open-hook ()
  (cadr (memq :open-hook projman-current-project)))

(defsubst projman-project-close-hook ()
  (cadr (memq :close-hook projman-current-project)))

(defsubst projman-project-file-load-hook ()
  (cadr (memq :fileload-hook projman-current-project)))

(defsubst projman-project-file-patterns ()
  (or (cadr (memq :globs projman-current-project))
      (projman-type-patterns (projman-project-type))))

(defun projman-iswtichb-read (prompt choices)
  (let ((iswitchb-use-virtual-buffers nil)
        (iswitchb-buffer-ignore nil)
        (iswitchb-make-buflist-hook
         (lambda () (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt)))

(defun projman-get-var (var)
  (let ((v (memq var projman-current-project)))
    (if v
        (cdr v)
      (symbol-value var))))

(defun projman-project-root-safe ()
  "Return the root of the current project.  Signal an error if no current root."
  (or projman-current-project
      (error "No current project"))
  (or (projman-project-root)
      (error "No project root defined")))

(defmacro projman-in-root (&rest body)
  `(let ((default-directory
                  (file-name-as-directory
                   (expand-file-name (projman-project-root-safe)))))
            ,@body))

(defun projman-project-root-for-dir (proj dir)
  "If DIR is part of the project PROJ, return the root directory."
  (let ((root (projman-project-root proj))
        (case-fold-search (projman-w32p)))
    (and root
         (setq root (expand-file-name root))
         (string-match (concat "^" (regexp-quote root)) dir)
         root)))

(defun projman-find-project-root (dir)
  "Find the project that contains DIR and return it's root directory."
  ;;(message "PROJMAN FPR: %S" dir)
  (let (root projects)
    (setq root (projman-project-root-for-dir projman-current-project dir))
    (setq projects projman-projects)
    (while (and (null root) projects)
      (setq root (projman-project-root-for-dir (car projects) dir)
            projects (cdr projects)))
    (setq projects projman-temp-projects)
    (while (and (null root) projects)
      (setq root (projman-project-root-for-dir (car projects) dir)
            projects (cdr projects)))
    root))

(defun projman-project-files ()
  "Return a list of all files belonging to the current project."
  (require 'grep)
  (projman-in-root
   (message "Building project file list...")
   (let ((patterns (projman-project-file-patterns))
         (cmd (concat projman-find-command " . "
                      (shell-quote-argument "(")
                      ;; we should use shell-quote-argument here
                      " -path "
                      (mapconcat #'(lambda (dir)
                                     (shell-quote-argument
                                      (concat "*/" dir)))
                                 (append grep-find-ignored-directories
                                         (projman-project-ignored-directories))
                                 " -o -path ")
                      " "
                      (shell-quote-argument ")")
                      " -prune -o -type f"))
         files file)
     (or patterns (error "No file patterns defined for project."))
     (setq cmd (concat cmd
                       " " (shell-quote-argument "(")
                       " -name "
                       (mapconcat #'(lambda (f) (shell-quote-argument f))
                                  patterns
                                  " -o -name ")
                       " " (shell-quote-argument ")")))
     (setq cmd (concat cmd " -print"))
     (setq files (split-string (shell-command-to-string cmd) "[\r\n]+" t))
     (message "Building project file list... done")
     (mapcar 'expand-file-name files))))

(defvar projman-file-cache nil)

(defun projman-project-files-from-cache (&optional regen)
  (if (or regen
          (null (projman-get-var 'projman-cache-project-file-list))
          (null projman-file-cache))
      (setq projman-file-cache (projman-project-files))
    projman-file-cache))

(defun projman-is-project-file (filepath)
  "Is FILEPATH part of the current project."
  (let ((proj-file-re (concat "^" (regexp-quote (projman-project-root-safe))))
        (case-fold-search (if (projman-w32p) t case-fold-search)))
    (and filepath
         (string-match proj-file-re filepath))))

(defvar projman-buffers-not-to-save "^\\(TAGS\\)$")
(defvar projman-files-not-to-save "")

(defun projman-active-buffers ()
  (let ((proj-file-re (concat "^" (regexp-quote (projman-project-root-safe))))
        (case-fold-search (if (projman-w32p) t case-fold-search))
        bufs)
    (dolist (b (buffer-list) bufs)      
      (let ((bufname (buffer-name b))
            (filename (buffer-file-name b)))
        (if (or (and filename
                     (string-match proj-file-re filename)
                     (not (string-match projman-buffers-not-to-save bufname)))
                (with-current-buffer b
                  (and (eq major-mode 'dired-mode)
                       (string-match proj-file-re default-directory))))
            (setq bufs (cons b bufs)))))
    (nreverse bufs)))

(defun projman-close-active-buffers ()
  ;; borrowed from `save-some-buffers'
  (save-window-excursion
    (let* ((bufferlist (projman-active-buffers))
           queried some-automatic)
      ;; First save any buffers that we're supposed to save unconditionally.
      ;; That way the following code won't ask about them.
      (dolist (buffer bufferlist)
        (with-current-buffer buffer
          (when (and buffer-save-without-query (buffer-modified-p))
            (setq some-automatic t)
            (save-buffer))))
      ;; Ask about those buffers that merit it,
      (map-y-or-n-p
       #'(lambda (buffer)
           (and (buffer-modified-p buffer)
                (not (buffer-base-buffer buffer))
                (buffer-file-name buffer)
                (setq queried t)
                (if (buffer-file-name buffer)
                    (format "Save file %s? "
                            (buffer-file-name buffer))
                  (format "Save buffer %s? "
                          (buffer-name buffer)))))
       #'(lambda (buffer)
           (set-buffer buffer)
           (save-buffer))
       bufferlist
       '("buffer" "buffers" "save")
       save-some-buffers-action-alist)
      ;; Now kill all the project buffers
      (dolist (buffer bufferlist)
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (kill-buffer buffer))))))

;; Save/Restore project state

(defun projman-load-active-state (projname)
  (let ((file (concat (expand-file-name projman-active-directory)
                      projname))
        state)
    (if (file-readable-p file)
        (save-excursion
          (message "Loading active project state from %s..." file)
          ;; don't want to use find-file because we have been
          ;; adding hooks to it.
          (set-buffer (get-buffer-create " *Saved State*"))
          (delete-region (point-min) (point-max))
          (insert-file-contents file)
          (goto-char (point-min))
          (setq state
                (car (read-from-string
                      (buffer-substring (point-min) (point-max)))))
          (kill-buffer (current-buffer))
          (message "Loading active project state from %s... done" file)
          (sit-for 3)
          (message "")))
    state))

(defun projman-restore-options (options)
  (setcdr projman-current-project
          (projman-merge-options (cdr projman-current-project)
                                 options)))

;; code for lazy loading buffers courtesy of lazy-desktop.el
;; <http://homepages.cs.ncl.ac.uk/phillip.lord/download/emacs/lazy-desktop.el>

(defvar projman-lazy-buffer-create-stack nil)
(defvar projman-lazy-buffer-create-timer nil)

(defun projman-restore-buffers (buffers)
  (if (not (projman-get-var 'projman-lazy-load-buffers))
      ;; load all buffers at once
      (dolist (finfo buffers)
        (projman-restore-one-buffer finfo))
    ;; lazy load buffers
    ;; move any buffer marked current to the front of the list
    (let ((bufs buffers))
      (while bufs
        (let ((curr (car bufs)))
          (setq bufs
                (if (nth 2 curr) ; buffer marked current, move to front and exit
                    (and (setq buffers (cons curr (delq curr buffers)))
                         nil)
                  (cdr bufs))))))
    ;; load first buffer immediately then load rest in idle timer
    (setq projman-lazy-buffer-create-stack buffers)
    (projman-lazy-restore-next-buffer)
    (setq projman-lazy-buffer-create-timer
          (run-with-idle-timer 5 t 'projman-lazy-buffer-create-idle-func))))

(defun projman-lazy-buffer-create-idle-func ()
  (let ((repeat 1))
    (while (and repeat projman-lazy-buffer-create-stack)
      (projman-lazy-restore-next-buffer)
      (setq repeat (sit-for 0.2)))
    (unless projman-lazy-buffer-create-stack
      (cancel-timer projman-lazy-buffer-create-timer)
      (message "Projman lazy buffer load complete")
      (sit-for 3)
      (message ""))))

(defun projman-lazy-restore-next-buffer ()
  (when projman-lazy-buffer-create-stack
    (projman-restore-one-buffer (pop projman-lazy-buffer-create-stack))))
    
(defun projman-restore-one-buffer (binfo)
  ;; each active buffer info is a list of: (filename readonly current_buffer)
  (let ((view-inhibit-help-message t)
        (fname (car binfo))
        (readonly (nth 1 binfo))
        (current (nth 2 binfo)))
    (if (file-exists-p fname)
        (let ((buf (find-file-noselect fname)))
          (when buf
            (cond
             ((eq readonly 'view)
              (with-current-buffer buf
                (view-mode-enter nil 'kill-buffer)))
             (readonly
              (with-current-buffer buf
                (setq buffer-read-only t))))
            (if current
              (switch-to-buffer buf))))
      (message "Projman: File \"%s\" no longer exists." fname))))
  
(defun projman-setup-project-options ()
  (let ((compile (projman-project-compile-command)))
    (if compile
        (setq compile-command compile)))
  (let ((tagfiles (projman-project-tag-files)))
    (if (null tagfiles)
        (setq tags-file-name (expand-file-name "TAGS" (projman-project-root))
              tags-table-list nil)
      (setq tags-file-name nil
            tags-table-list (cons
                             (projman-project-root)
                             (projman-project-tag-files)))))
  (let ((gtaglibs (projman-project-gtag-libs)))
    (setenv "GTAGSLIBPATH" (mapconcat 'identity gtaglibs ";"))))

(defun projman-capture-active-state ()
  (save-current-buffer
    (let ((currbuf (current-buffer)))
      (cons
       ;; project options
       (cdr projman-current-project)
       ;; state for each project buffer
       (append
        (mapcar
         #'(lambda (b)
             (with-current-buffer b
               (list
                ;; 1. filepath
                (if (eq major-mode 'dired-mode)
                    default-directory
                  (buffer-file-name))
                ;; 2. read only?
                (cond ((and (boundp 'view-mode)
                            view-mode
                            (eq view-exit-action 'kill-buffer))
                       'view)
                      (buffer-read-only
                       t))
                ;; 3. current buffer?
                (eq b currbuf))))
         (projman-active-buffers))
        ;; be sure to include any buffers still waiting to be loaded
        projman-lazy-buffer-create-stack)))))

(defun projman-save-active-state (projname state)
  (let* ((file (concat (expand-file-name projman-active-directory)
                       projname)))
    (save-excursion
      (message "Saving active project state to %s..." file)
      (set-buffer (get-buffer-create " *Saved State*"))
      (delete-region (point-min) (point-max))
      (let ((print-length nil)
            (print-level nil))
        (insert ";; Created by projman on " (current-time-string) ".")
        (print state (current-buffer)))
      (let ((version-control
             (cond ((null projman-version-control) nil)
                   ((eq 'never projman-version-control) 'never)
                   ((eq 'nospecial projman-version-control) version-control)
                   (t t))))
        (condition-case nil
            ;; Don't use write-file; we don't want this buffer to visit it.
            (write-region (point-min) (point-max) file)
          (file-error (message "Can't write %s" file)))
        (kill-buffer (current-buffer))
        (message "Saving active project state to %s... done" file)
        (sit-for 3)
        (message "")))))

(defun projman-capture-and-save-active-state ()
  (projman-save-active-state (projman-project-name)
                             (projman-capture-active-state)))
    
;; COMMANDS

(defun projman-current-project-name ()
  (interactive)
  (let ((projname (projman-project-name)))
    (if projname
        (message "Current project is %s" projname)
      (message "No current project"))))

(defun projman--active-files ()
  "List of project files."
  (directory-files
   (expand-file-name projman-active-directory)
   nil "^[-A-Za-z0-9_]+$"))

(defun projman--read-projname ()
  (let* ((choices (delete-dups
                   (append (mapcar 'car projman-projects)
                           (mapcar 'car projman-temp-projects)
                           (and projman-use-active-file
                                (projman--active-files)))))
         (iswitchb-use-virtual-buffers nil)
         (iswitchb-make-buflist-hook
          (lambda () (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer "Choose Project: ")))

(defun projman-switch-project (&optional projname)
  "Switch to a different project.  Saves any current project first."
  (interactive (list (projman--read-projname)))
  (projman-close-project)
  ;; load project
  (let ((proj (or (assoc projname projman-projects)
                  (assoc projname projman-temp-projects)
                  (and projman-use-active-file
                       (member projname (projman--active-files))
                       (car (setq projman-temp-projects
                                  (cons (list projname) projman-temp-projects))))
                  (or (and (y-or-n-p
                            (format "Project %s does not exist.  Create it? "
                                    projname))
                           (projman-create-project projname t))
                      (error "Canceled")))))
    (setq projman-current-project proj)
    (setq projman-project-name (projman-project-name)))
  (message "Switched to project %s" (projman-project-name))
  (let (options buffers)
    (if projman-use-active-file
        ;; state is cons: (options . (active buffers info))
        (let ((state (projman-load-active-state (projman-project-name))))
          (setq options (car state))
          (setq buffers (cdr state))))
    (projman-restore-options options)
    ;; sanity checks
    (unless (projman-project-root)
      (setq projman-current-project nil
            projman-project-name nil)
      (error "ERROR: project has not root defined"))
    ;; make sure all options are setup before loading any buffers
    (projman-setup-project-options)
    (projman-restore-buffers buffers))
  (run-hooks 'projman-project-switch-hook)
  (let ((hook (projman-project-open-hook)))
    (if hook (funcall hook))))
  
(defun projman-create-project (&optional projname auto-switch)
  "Create a new project on the fly."
  (interactive)
  (let (name root type again proj)
    (setq again t)
    (while again
      (setq name (or projname (completing-read "Project name: "
                                               (mapcar 'car projman-temp-projects)))
            again (or (and (assoc name projman-projects)
                           (message "%s already defines a permanent project" name)
                           t)
                      ;; XXX also test active files for a match to existing project
                      (and (assoc name projman-temp-projects)
                           (not (y-or-n-p
                                 (format "%s already defines a project.  Replace? "
                                         name)))))))
    (setq root (expand-file-name (read-directory-name "Root directory: "
                                                      nil nil t)))
    (setq type (completing-read "Type: " (cons "UNDEFINED"
                                          (mapcar 'cadr projman-types)) nil t))
    (setq proj (list name :root root))
    (dolist (type-spec projman-types)
      (if (string= type (cadr type-spec))
          (setq proj (append proj (list :type (car type-spec))))))
    ;; delete any existing temp project with same name
    (setq projman-temp-projects
          (projman-assoc-delete-all name projman-temp-projects))
    ;; add temp project
    (setq projman-temp-projects
          (cons proj projman-temp-projects))
    (if (or auto-switch
            (y-or-n-p (format "Switch to project %s? " name)))
        (projman-switch-project name))
    proj))

(defun projman-dired-root ()
  "Run `dired' in the root of the current project."
  (interactive)
  (dired (projman-project-root-safe)))

(defun projman-switch-buffer (&optional regen)
  "Switch to another project buffer using iswitchb.
Prefix arg non-nil to force a refresh of the cached file list."
  (interactive "P")
  (let ((filelist (projman-project-files-from-cache regen))
        files choice)
    (setq files (mapcar #'(lambda (f) (list (file-name-nondirectory f) f)) filelist))
    (let ((iswitchb-use-virtual-buffers nil)
          (iswitchb-make-buflist-hook
           (lambda () (setq iswitchb-temp-buflist (mapcar 'car files)))))
      (setq choice (iswitchb-read-buffer "Project File: ")))
    (find-file (cadr (assoc choice files)))))

(defun projman--read-grep-regexp ()
  "Read regexp arg for grep command."
  (require 'grep)
  (let ((default (grep-tag-default)))
    (read-string (concat "Grep project for"
                         (if (and default (> (length default) 0))
                             (format " (default \"%s\"): " default) ": "))
                 nil 'grep-regexp-history default)))

(defun projman-grep (regexp)
  "Run `rgrep' for REGEXP in the root of the current project."
  (interactive (list (projman--read-grep-regexp)))
  (require 'grep)
  (grep-compute-defaults)
  (let ((grep-find-ignored-directories (append grep-find-ignored-directories
                                               (projman-project-ignored-directories))))
    (rgrep regexp
           (mapconcat 'identity (projman-project-file-patterns) " ")
           (projman-project-root-safe))))

(defun projman--read-occur-regexp ()
  "Read regexp arg for occur command."
  (require 'grep)
  (let ((default (grep-tag-default)))
    (read-string (concat "List lines in open project files that match"
                         (if (and default (> (length default) 0))
                             (format " (default \"%s\"): " default) ": "))
                 nil 'regexp-history default)))

(defun projman-occur (regexp)
  "Run `multi-occur' against all open project files."
  (interactive (list (projman--read-occur-regexp)))
  (multi-occur-in-matching-buffers
   (concat "^" (regexp-quote (projman-project-root-safe))) regexp))

(defun projman-svn-status ()
  "Run psvn status in the project root"
  (interactive)
  (if (fboundp 'svn-status-this-directory)
      (projman-in-root
       (svn-status-this-directory nil))
    (error "psvn not available")))

(defun projman-create-tags ()
  "Create tags file for project."
  (interactive)
  (projman-in-root
   (message "Creating TAGS, please wait...")
   (shell-command (concat (projman-get-var 'projman-tags-command)
                          " "
                          (projman-get-var 'projman-tags-args))))
  (message "Creating TAGS, please wait... done")
  (sit-for 3)
  (message "")
  ;; if default TAGS file is setup for use, then switch to new TAGS file
  (if (null (projman-project-tag-files))
      (setq tags-file-name (expand-file-name "TAGS" (projman-project-root))
            tags-table-list nil)))

(defun projman-create-cscope-index ()
  "Create cscope index."
  (interactive)
  (require 'xcscope)
  (cscope-index-files (projman-project-root-safe)))

(defun projman-create-global-index ()
  "Create global index."
  (interactive)
  (projman-in-root
   (message "Creating GLOBAL index, please wait...")
   (let ((args (projman-get-var 'projman-gtags-args)))
     (shell-command (concat (projman-get-var 'projman-gtags-command)
                            (if args " ") args)))
   (message "Creating GLOBAL index, please wait... done")
   (sit-for 3)   
   (message "")))

(defun projman-close-project (&optional nosave)
  (interactive "P")
  (if projman-lazy-buffer-create-timer
      (cancel-timer projman-lazy-buffer-create-timer))
  (when projman-current-project
    (if (and projman-use-active-file (not nosave))
        (projman-capture-and-save-active-state))
    (let ((hook (projman-project-close-hook)))
      (if hook (run-hooks hook)))
    (if (projman-get-var 'projman-close-buffers-when-close-project)
        (projman-close-active-buffers)))
  ;; clear state vars
  (setq projman-current-project nil
        projman-project-name nil
        projman-file-cache nil
        projman-lazy-buffer-create-stack nil))
    
  
(defun projman-quit ()
  "Quit projman mode, closing any open project."
  (interactive)
  (projman-close-project)
  (setq tags-file-name nil
        tags-table-list nil)
  (setenv "GTAGSLIBPATH" nil)
  (projman-mode -1))
  
;; MINOR MODE

(defun projman-run-file-hook ()
  (let ((hook (projman-project-file-load-hook)))
    (if (and hook
             (projman-is-project-file (buffer-file-name)))
        (funcall hook))))

(defvar projman-mode-prefix "\C-cp"
  "Prefix key for projman mode key bindings.")

(defvar projman-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'projman-switch-buffer)
    (define-key map "c" 'projman-create-project)
    (define-key map "g" 'projman-grep)
    (define-key map "i" 'projman-current-project-name)
    (define-key map "o" 'projman-occur)
    (define-key map "p" 'projman-switch-project)
    (define-key map "q" 'projman-close-project)
    (define-key map "r" 'projman-dired-root)
    (define-key map "v" 'projman-svn-status)
    (define-key map "\C-t" 'projman-create-tags)
    (define-key map "\C-s" 'projman-create-cscope-index)
    (define-key map "\C-w" 'projman-create-global-index)
    (define-key map "\C-q" 'projman-quit)
    map))

(defvar projman-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))
    map))

(defvar projman-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map projman-mode-prefix projman-mode-prefix-map)
    (define-key map [menu-bar] projman-mode-menu-bar-map)
    map))

(define-minor-mode projman-mode
  "Projman minor mode.
Basic tools for working with a collection of source code files, a \"project\".

Key Bindings:
\\{projman-mode-map}
"
  nil
  " PM"
  projman-mode-map
  :group 'projman
  :global t
  (if projman-mode
      (add-hook 'find-file-hook 'projman-run-file-hook)
    (remove-hook 'find-file-hook 'projman-run-file-hook)))
  
(add-hook 'kill-emacs-hook 'projman-close-project)


;; semantic hooks

(eval-after-load "semanticdb"
  '(add-to-list 'semanticdb-project-root-functions 'projman-find-project-root))

(defun projman-semanticdb-analyze ()
  "Scan all project files for semantic tags.
`global-semanticdb-minor-mode' should already be on."
  (interactive)
  (let ((recentf-exclude (list ".*")) ; don't want scanned files in recentf list
        (files (projman-project-files))
        (file))
    (message "Visiting files...")
    (while files
      (setq file (car files)
            files (cdr files))
      (unless (find-buffer-visiting file)
        (message "Visiting %s" file)
        (let ((buf (find-file-noselect file))
              (tags))
          (setq tags (semantic-fetch-tags))
          (kill-buffer buf)))))
  (semanticdb-save-all-db)
  (message "Finished analyzing files."))

(defun projman-ede-setup-c++-project (rootfile &rest args)
  "Call from a projman open-hook to add ede support to c++ projects."
  (global-ede-mode 1)
  (apply 'ede-cpp-root-project
         (projman-project-name)
         :name (projman-project-name)
         :file (expand-file-name rootfile (projman-project-root))
         args))

;; ecb hooks

(defun projman-ecb-source-path-function ()
  "Define project root directory for ECB."
  (and projman-current-project
       (list (projman-project-root)
        ;;(cons (projman-project-root) (projman-project-name))
        )))

(eval-after-load "ecb"
  '(progn
     (add-to-list 'ecb-source-path-functions 'projman-ecb-source-path-function)
     ))

;; bs hooks

(eval-when-compile (require 'bs))

(defvar projman-bs-show-buffers
  '("*Help*")
  "Buffers to always show in project bs configuration.")

(defvar projman-bs-dir-modes
  '(dired-mode shell-mode svn-status-mode ssdir-mode)
  "Major modes that aren't visiting a file but have a `default-directory'.
Projman bs config uses this to determine whether to match `default-directory'
  instead of `buffer-file-name' when checking if a buffer belongs to a
  project.")

(defun projman-bs-dont-show-func (buffer)
  "Don't show buffers that aren't in the project directory tree.
This can include files that are in the tree but don't match the project glob
pattern."
  (let ((root (projman-project-root))
        (file (or (buffer-file-name buffer)
                  (save-excursion
                    (set-buffer buffer)
                    (and (memq major-mode projman-bs-dir-modes)
                         default-directory)))))
    (not (and root
              file
              (string-match (concat "^" (regexp-quote (expand-file-name root)))
                            file)))))

(defun projman-install-bs-config ()
  "Install the 'project' config for bs."
  ;; NOTE: we remove then add our config so it picks up any changes made to
  ;;       `projman-bs-show-buffers'.
  ;; delete the "project" config if it already exists
  (let ((configs bs-configurations)
        c new)
    (while configs
      (setq c (car configs)
            configs (cdr configs))
      (or (string= (car c) "project")
          (setq new (cons c new))))
    (setq bs-configurations (nreverse new)))
  ;; now add it back in
  (let ((conf
         `("project"
           ,(concat "^" (regexp-opt projman-bs-show-buffers) "$")
           nil
           nil
           projman-bs-dont-show-func
           bs-sort-buffer-interns-are-last)))
    (add-to-list 'bs-configurations conf)))

(eval-after-load 'bs
  '(projman-install-bs-config))


(provide 'projman)
;;; projman.el ends here
