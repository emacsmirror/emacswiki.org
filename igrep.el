;;; igrep.el --- An improved interface to `grep` and `find`
;;; -*-coding: iso-latin-1;-*-

;; Copyright © 1993-1998,2000-2005 Kevin Rodgers

;; Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; Created:  22 Jun 1993
;; Version: 2.113
;; Keywords: tools, processes, search
;; SCCS: @(#)igrep.el	2.113

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; The `igrep' command is like `grep' except that it takes three
;; required arguments (PROGRAM, REGEX, and FILES) and an optional
;; argument (OPTIONS) instead of just one argument (COMMAND).  The
;; analogous `egrep' and `fgrep' commands are also defined for
;; convenience.
;; 
;; The `igrep-find' command is like `igrep' except that it uses `find`
;; to recursively `grep` a directory.  The analogous `egrep-find' and
;; `fgrep-find' commands are also defined for convenience.
;; 
;; When called interactively, `igrep' and `igrep-find' (and their
;; analogues) provide defaults for the REGEX and FILES arguments based
;; on the current word and the visited file name (if the `igrep-regex-
;; default' and `igrep-files-default' options are set, respectively).
;; The `igrep-insert-default-key' option allows the default value to be
;; inserted into the minibuffer for editing; since Emacs 20 provides
;; that via the minibuffer history, it's only enabled for older
;; versions by default. Other options that control the user interface
;; are `igrep-insert-default-directory', `igrep-read-options', `igrep-
;; read-multiple-files', `igrep-verbose-prompts', `igrep-save-buffers',
;; and `igrep-menu-bar'.
;; 
;; Besides the basic `igrep-program' and `igrep-find-program' global
;; variables, other variables control the syntax of the `grep` and
;; `find` shell commands that are executed: `igrep-options', `igrep-
;; regex-option', `igrep-case-fold-search', `igrep-find-prune-clause',
;; `igrep-find-file-clause', and `igrep-find-use-xargs'.
;; 
;; The `igrep-use-zgrep' user option controls whether the corresponding
;; GNU (gzip) "zPROGRAM" script is used, to `grep` compressed files.
;; Special minibuffer history lists are maintained for the REGEX and
;; FILES arguments.
;; 
;; The `agrep' and `agrep-find' commands are interfaces to the
;; approximate `grep` utility, which is distributed with the `glimpse'
;; indexing and query tool (available from http://www.tgries.de/agrep/).
;; 
;; `grep' itself can be advised to provide the `igrep' interface when
;; it is invoked interactively (but still use the original argument
;; list when it is called from Emacs Lisp), via the `igrep-insinuate'
;; command.  `igrep-insinuate' also defines `grep-find' as an alias for
;; `igrep-find', `dired-do-grep' and `dired-do-grep-find' as aliases
;; for `dired-do-igrep' and `dired-do-igrep-find', and `Buffer-menu-
;; grep' as an alias for `Buffer-menu-igrep'.
;; 
;; When run interactively from Dired mode, the various `igrep' commands
;; provide defaults for the REGEX and FILES arguments that are based on
;; the visited directory (including any inserted subdirectories) and
;; the current file.  The alternative `dired-do-igrep' and `dired-do-
;; igrep-find' commands respect the `dired-do-*' command conventions: a
;; prefix argument is interpreted as the number of succeeding files to
;; `grep`, otherwise all the marked files are `grep`ed.
;; 
;; The `igrep-visited-files' command provides a simple way to `grep`
;; just those files that are being visited in buffers.  The `Buffer-
;; menu-igrep' command does the same thing, for buffers marked for
;; selection in Buffer Menu mode.

;; Installation:
;; 
;; 1. Put this file in a directory that is a member of load-path, and
;;    byte-compile it (e.g. with `M-x byte-compile-file') for better
;;    performance.  You can ignore any warnings about references to free
;;    variables and "not known to be defined" functions.
;; 2. Put these forms in default.el or ~/.emacs:
;;    (autoload 'igrep "igrep"
;;       "*Run `grep` PROGRAM to match REGEX in FILES..." t)
;;    (autoload 'igrep-find "igrep"
;;       "*Run `grep` via `find`..." t)
;;    (autoload 'igrep-visited-files "igrep"
;;       "*Run `grep` ... on all visited files." t)
;;    (autoload 'dired-do-igrep "igrep"
;;       "*Run `grep` on the marked (or next prefix ARG) files." t)
;;    (autoload 'dired-do-igrep-find "igrep"
;;       "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
;;    (autoload 'Buffer-menu-igrep "igrep"
;;      "*Run `grep` on the files visited in buffers marked with '>'." t)
;;    (autoload 'igrep-insinuate "igrep"
;;      "Define `grep' aliases for the corresponding `igrep' commands." t)
;; 2. a. For completeness, you can add these forms as well:
;;    (autoload 'grep "igrep"
;;       "*Run `grep` PROGRAM to match REGEX in FILES..." t)
;;    (autoload 'egrep "igrep"
;;       "*Run `egrep`..." t)
;;    (autoload 'fgrep "igrep"
;;       "*Run `fgrep`..." t)
;;    (autoload 'agrep "igrep"
;;       "*Run `agrep`..." t)
;;    (autoload 'grep-find "igrep"
;;       "*Run `grep` via `find`..." t)
;;    (autoload 'egrep-find "igrep"
;;       "*Run `egrep` via `find`..." t)
;;    (autoload 'fgrep-find "igrep"
;;       "*Run `fgrep` via `find`..." t)
;;    (autoload 'agrep-find "igrep"
;;       "*Run `agrep` via `find`..." t)
;; 3. If you are running Windows 95/NT, you should install findutils
;;    and grep from release 17.1 (or higher) of the Cygnus GNU-Win32
;;    distribution (http://www.cygnus.com/misc/gnu-win32/).

;; Usage:
;; 
;; These igrep commands accept 1, 2, or 3 `C-u' prefix arguments:
;; 	M-x igrep		M-x igrep-find
;; 	M-x  grep		M-x  grep-find	[after `M-x igrep-insinuate']
;; 	M-x egrep		M-x egrep-find
;; 	M-x fgrep		M-x fgrep-find
;; 	M-x agrep		M-x agrep-find
;; 
;; These igrep commands accept a single `C-u' prefix argument:
;; 	M-x igrep-visited-files
;; 	M-x Buffer-menu-igrep	[in the *Buffer List* buffer]
;; 
;; These igrep commands interpret a prefix argument like the Emacs
;; `dired-do-*' commands:
;; 	M-x dired-do-igrep	M-x dired-do-igrep-find
;; 	M-x  dired-do-grep	M-x  dired-do-grep-find	[after `M-x
;; 							 igrep-insinuate']
;; 
;; These Emacs commands can be used after any igrep command:
;; 	C-x ` (M-x next-error)
;; 	C-c C-c (M-x compile-goto-error)	[in the *igrep* buffer]

;; Customization examples:
;; 
;; To ignore case by default:
;; 	(setq igrep-options "-i")
;; or:
;; 	(setq igrep-case-fold-search t)
;; To search subdirectories by default:
;; 	(setq igrep-find t)
;; To search files with the GNU (gzip) zgrep script:
;; 	(setq igrep-use-zgrep t)
;; or define new igrep commands (this works for zegrep and zfgrep as well):
;; 	(igrep-define zgrep)		; M-x zgrep
;; 	(igrep-find-define zgrep)	; M-x zgrep-find
;; To search "*.[ch]" files by default in C mode:
;; 	(put 'igrep-files-default 'c-mode
;; 	     (lambda () "*.[ch]"))
;; To disable the default search regex and/or files pattern, except for
;; specific modes:
;; 	(setq igrep-regex-default 'ignore)
;; 	(setq igrep-files-default 'ignore)
;; To avoid exceeding some shells' limit on command argument length
;; (this only searches files in the current directory):
;; 	(setq igrep-find t
;; 	      igrep-find-prune-clause "-type d \\! -name .")

;; To do:
;; 1. Replace igrep-options with a table that maps igrep-program
;;    to the appropriate options, and/or support POSIX (egrep -> `grep -E`).
;; 2. Generalize support for the -prune find clause (e.g. -fstype nfs).
;; 3. Provide support for `glimpse`.

;;; Code:

;; Package interface:

(require 'custom)			; defgroup, defcustom

(require 'easymenu)			; easy-menu-define, easy-menu-add-item

(or (condition-case nil
	(require 'grep)			; Emacs 22
      (error nil))
    (require 'compile))			; compile-internal, grep-regexp-alist,
					; grep-null-device

(eval-when-compile
  (require 'dired)			; dired-directory,
					; dired-get-filename,
					; dired-current-directory,
					; dired-get-marked-files,
					; dired-mark-get-files
  (or (featurep 'ange-ftp)
      (featurep 'efs)
      (condition-case nil
	  (load-library "ange-ftp")	; ange-ftp-ftp-name
	(error nil))
      (condition-case nil
	  (load-library "efs")		; efs-ftp-path
	(error nil)))
  )

(defconst igrep-version "2.113"
  "This version of igrep.el.")

(defgroup igrep nil
  "An improved interface to `grep` and `find`."
  :group 'compilation)

;; User options:

(defcustom igrep-options nil
  "*The options passed by `\\[igrep]' to `igrep-program', or nil.

\"-n\" will automatically be passed to `igrep-program', to generate the
output expected by `\\[next-error]' and `\\[compile-goto-error]'.
\"-e\" will automatically be passed to `igrep-program', if it supports
that option."
  :group 'igrep
  :type '(choice (const nil) (string)))
(put 'igrep-options 'variable-interactive
     "xOptions (\"-xyz\" or nil): ")

(defcustom igrep-case-fold-search nil
  "*If non-nil, `\\[igrep]' ignores case unless REGEX has uppercase letters."
  :group 'igrep
  :type '(boolean))
(put 'igrep-case-fold-search 'variable-interactive
     "XIgnore case? (t or nil): ")

(defcustom igrep-read-options nil
  "*If non-nil, `\\[igrep]' always prompts for options;
otherwise, it only prompts when 1 or 3 `C-u's are given as a prefix arg."
  :group 'igrep
  :type '(boolean))
(put 'igrep-read-options 'variable-interactive
     "XAlways prompt for options? (t or nil): ")

(defcustom igrep-read-multiple-files nil
  "*If non-nil, `\\[igrep]' always prompts for multiple-files;
otherwise, it only prompts when 2 or 3 `C-u's are given as a prefix arg."
  :group 'igrep
  :type '(boolean))
(put 'igrep-read-multiple-files 'variable-interactive
     "XAlways prompt for multiple files? (t or nil): ")

(defcustom igrep-regex-default 'current-word
  "*If non-nil, a function that returns a default REGEX for `\\[igrep]'.
The function is called with no arguments and should return a string (or nil).

A different function can be specified for any particular mode by specifying
a value for that `major-mode' property; for example:
	(put 'igrep-regex-default 'dired-mode
	     'igrep-dired-file-current-word)"
  :group 'igrep
  :type '(choice (const nil) (function)))
(put 'igrep-regex-default 'variable-interactive
     "SProvide a default regex? (function or nil): ")
(put 'igrep-regex-default 'dired-mode
     'igrep-dired-file-current-word)

(defcustom igrep-files-default 'igrep-buffer-file-name-pattern
  "*If non-nil, a function that returns the default FILES for `\\[igrep]'.
The function is called with no arguments and should return a string,
or a list of strings (or nil).

A different function can be specified for any particular mode by specifying
a value for that `major-mode' property; for example:
	(put 'igrep-files-default 'dired-mode
	     'igrep-dired-directory-file-pattern)"
  :group 'igrep
  :type '(choice (const nil) (function)))
(put 'igrep-files-default 'variable-interactive
     "SProvide a default file name pattern? (function or nil): ")
(put 'igrep-files-default 'dired-mode
     'igrep-dired-directory-file-pattern)

(defcustom igrep-verbose-prompts t
  "*If t, `\\[igrep]' prompts for arguments verbosely;
if not t but non-nil, `\\[igrep]' prompts for arguments semi-verbosely;
if nil, `\\[igrep]' prompts for arguments tersely."
  :group 'igrep
  :type '(choice (const :tag "Verbose" t)
		 (other :tag "Semi-verbose" semi)
		 (const :tag "Terse" nil)))
(put 'igrep-verbose-prompts 'variable-interactive
     "XPrompt verbosely? (t, 'semi, or nil): ")

(defcustom igrep-insert-default-directory nil
  "*The value of `insert-default-directory' for `\\[igrep]'."
  :group 'igrep
  :type '(boolean))
(put 'igrep-insert-default-directory 'variable-interactive
     "XPrompt with directory in the minibuffer? (t or nil): ")

(defcustom igrep-insert-default-key
  (if (< emacs-major-version 20) "\C-c\C-e")
  "*The key used to insert the default argument in the minibuffer.
In Emacs 20, the default is available via the minibuffer history \
\(\\<minibuffer-local-map>\\[next-history-element])."
  :group 'igrep
  :type '(choice (const nil) (string) (vector))) ; key-binding
(put 'igrep-insert-default-key 'variable-interactive
     "kSet key to insert the default `\\[igrep]' argument in the minibuffer: ")

(defcustom igrep-save-buffers 'query
  "*If t, `\\[igrep]' first saves each modified file buffer;
if not t but non-nil, `\\[igrep]' offers to save each modified file buffer."
  :group 'igrep
  :type '(choice (const :tag "Save" t)
		 (other :tag "Query" query)
		 (const :tag "Don't Save" nil)))
(put 'igrep-save-buffers 'variable-interactive
     "XSave modified buffers? (t, 'query, or nil): ")

(defcustom igrep-menu-bar t
  "*If non-nil, enable the `igrep-menu' submenu on the \"Tools\" menu bar."
  :group 'igrep
  :type '(boolean))
(put 'igrep-menu-bar 'variable-interactive
     "XEnable menu bar? (t or nil): ")

;; User variables:

(defsubst igrep-easy-menu-item (name callback help-keyword help-text)
  "Return a [NAME CALLBACK HELP-KEYWORD HELP-TEXT] menu item.
See `easy-menu-define'."
  (if (featurep 'xemacs)                ; no :help keyword
      (vector name callback)
    (vector name callback help-keyword help-text)))

(eval-when-compile
  (unless (and (fboundp 'keywordp) (keywordp :help))
    (defvar :help ':help)))             ; Emacs 19

(defvar igrep-easy-menu
  `("Search Files and Directories (igrep)"
    ,@(cond ((featurep 'xemacs) '(:included igrep-menu-bar))
	    ((>= emacs-major-version 20) '(:active igrep-menu-bar))
	    (t ()))
    ("Search files"
     ,(igrep-easy-menu-item "`grep` files..." 'igrep
                            :help "Search files for basic regex(5)s")
     ,(igrep-easy-menu-item "`egrep` files..." 'egrep
                            :help "Search files for extended regex(5)s")
     ,(igrep-easy-menu-item "`fgrep` files..." 'fgrep
                            :help "Search files for strings"))
    ("Search directories"
     ,(igrep-easy-menu-item "`find | grep` directories..." 'igrep-find
                            :help "Search directories for basic regex(5)s")
     ,(igrep-easy-menu-item "`find | egrep` directories..." 'egrep-find
                            :help "Search directories for extended regex(5)s")
     ,(igrep-easy-menu-item "`find | fgrep` directories..." 'fgrep-find
                            :help "Search directories for strings"))
    "--"
    ,(igrep-easy-menu-item "Search visited files..." 'igrep-visited-files
                           :help "Search visited files for basic regex(5)s"))
  "If non-nil, the menu bar submenu of `igrep' commands.
See `easy-menu-define'.")

(defvar igrep-null-device
  (cond ((boundp 'null-device) null-device) ; Emacs 20
	((boundp 'grep-null-device) grep-null-device)) ; Emacs 19
  "The system null device.")

(defvar igrep-program "grep"
  "The default program run by `\\[igrep]' and `\\[igrep-find]'.
It must accept a `grep` regex argument and one or more file names, plus
the \"-n\" option.  If nil, `\\[igrep]' prompts for the program to run.")

(defvar igrep-regex-option
  (if (equal (call-process igrep-program nil nil nil
			   "-e" "foo" igrep-null-device)
	     1)
      "-e")
  "If non-nil, the option used to specify the REGEX argument to `\\[igrep]'.
This protects an initial \"-\" from option processing.")

(defvar igrep-program-table		; referenced by igrep-use-zgrep
  (let ((exec-directories exec-path)
	(program-obarray (make-vector 11 0)))
    (while exec-directories
      (if (and (car exec-directories)
	       (file-directory-p (car exec-directories))
	       (file-readable-p (car exec-directories)))
	  (let ((grep-programs
		 (directory-files (car exec-directories)
				  nil "grep\\(\\.exe\\)?\\'")))
	    (while grep-programs
	      ;; Check `(file-executable-p (car grep-programs))'?
	      (if (save-match-data
		    (string-match "\\.exe\\'" (car grep-programs)))
		  (intern (substring (car grep-programs) 0 -4) program-obarray)
		(intern (car grep-programs) program-obarray))
	      (setq grep-programs (cdr grep-programs)))))
      (setq exec-directories (cdr exec-directories)))
    program-obarray)
  "An obarray of available `grep` programs.
This is passed by `igrep-read-program' to `completing-read' when
`igrep-program' is nil.")

(defvar igrep-use-zgrep
  (if (intern-soft "zgrep" igrep-program-table)
      'files)
  "If t, `\\[igrep]' searches files using the GNU (gzip) `zPROGRAM` script;
If not t but non-nil, `\\[igrep]' searches compressed FILES using `zPROGRAM`;
if nil, `\\[igrep]' searches files with `PROGRAM`.")

(defvar igrep-find nil
  "If non-nil, `\\[igrep]' searches directories using `find`.
See `igrep-find'.")

(defvar igrep-find-program "find"
  "The program run by `\\[igrep-find]'.")

(defvar igrep-find-prune-clause
  (if (equal (call-process igrep-find-program nil nil nil
			   igrep-null-device "-prune")
	     0)
      (format "-type d %s -name RCS -o -name CVS -o -name SCCS %s"
	      (shell-quote-argument "(")
	      (shell-quote-argument ")")))
  "The `find` clause used to prune directories, or nil;
see `igrep-find'.")

(defvar igrep-find-file-clause
  (format "-type f %s -name %s %s -name %s %s -name %s %s -name %s" ; -type l
	  (shell-quote-argument "!")
	  (shell-quote-argument "*~")	; Emacs backup
	  (shell-quote-argument "!")
	  (shell-quote-argument "*,v")	; RCS file
	  (shell-quote-argument "!")
	  (shell-quote-argument "s.*")	; SCCS file
	  (shell-quote-argument "!")
	  (shell-quote-argument ".#*"))	; CVS file
  "The `find` clause used to filter files passed to `grep`, or nil;
see `igrep-find'.")

(defvar igrep-find-use-xargs
  (cond ((equal (call-process igrep-find-program nil nil nil
                              igrep-null-device "-print0")
                0)
	 'gnu)
        ((equal (call-process "xargs" nil nil nil "-e") 0)))
  "Whether `\\[igrep-find]' uses the `xargs` program or not.
If `gnu', it executes
	`find ... -print0 | xargs -0 -e grep ...`;
if not `gnu' but non-nil, it executes
	`find ... -print | xargs -e grep ...`;
if nil, it executes
	`find ... -exec grep ...`.")

(defvar igrep-program-default "grep"
  "The default `grep` program.
This is passed by `igrep-read-program' to `completing-read' when
`igrep-program' is nil.")

;; Internal variables:

(defvar igrep-regex-history '()
  "The minibuffer history list for `\\[igrep]'s REGEX argument.")

(defvar igrep-files-history '()
  "The minibuffer history list for `\\[igrep]'s FILES argument.")

;; Commands:

;;;###autoload
(defun igrep-insinuate (&optional override)
  "Define `grep' aliases for the corresponding `igrep' commands.
With a prefix arg, OVERRIDE the current `grep' command definitions."
  (interactive "P")
  (if override
      (defalias 'grep 'igrep)
    (defadvice grep (around igrep-interactive first (&rest command-args)
			    activate)
      "If called interactively, use the `\\[igrep]' interface instead,
where COMMAND-ARGS is (PROGRAM REGEX FILES [OPTIONS]); if called
programmatically, COMMAND-ARGS is still (COMMAND)."
      (interactive (igrep-read-args))
      (if (interactive-p)
	  (apply 'igrep command-args)
	ad-do-it)))
  (if (or (not (fboundp 'grep-find))
	  override)
      (defalias 'grep-find 'igrep-find))
  (if (or (not (fboundp 'dired-do-grep))
	  override)
      (defalias 'dired-do-grep 'dired-do-igrep))
  (if (or (not (fboundp 'dired-do-grep-find))
	  override)
      (defalias 'dired-do-grep-find 'dired-do-igrep-find))
  (if (or (not (fboundp 'Buffer-menu-grep))
	  override)
      (defalias 'Buffer-menu-grep 'Buffer-menu-igrep)))

(defsubst igrep-quote-file-name (file)
  "Quote FILE name pattern for `shell-file-name'."
  (if (fboundp 'shell-quote-wildcard-pattern) ; Emacs 21
      (shell-quote-wildcard-pattern file)
    (shell-quote-argument file)))

;;;###autoload
(defun igrep (program regex files &optional options)
  "*Run `grep` PROGRAM to match REGEX in FILES.
The output is displayed in the *igrep* buffer, which `\\[next-error]' and
`\\[compile-goto-error]' parse to find each line of matched text.

PROGRAM may be nil, in which case it defaults to `igrep-program'.

REGEX is automatically quoted by `shell-quote-argument'.

FILES is either a file name pattern (automatically quoted by
`shell-quote-wildcard-pattern', then expanded by the `shell-file-name' shell),
or a list of file name patterns.

Optional OPTIONS is also passed to PROGRAM; it defaults to `igrep-options'.

If a prefix argument \
\(`\\[universal-argument]') \
is given when called interactively,
or if `igrep-read-options' is set, OPTIONS is read from the minibuffer.

If two prefix arguments \
\(`\\[universal-argument] \\[universal-argument]') \
are given when called interactively,
or if `igrep-read-multiple-files' is set, FILES is read from the minibuffer
multiple times.

If three prefix arguments \
\(`\\[universal-argument] \\[universal-argument] \\[universal-argument]') \
are given when called interactively,
or if `igrep-read-options' and `igrep-read-multiple-files' are set,
OPTIONS is read and FILES is read multiple times.

If `igrep-find' is non-nil, the directory or directories
containing FILES is recursively searched for files whose name matches
the file name component of FILES (and whose contents match REGEX)."
  (interactive
   (igrep-read-args))
  (if (null program)
      (setq program (or igrep-program "grep")))
  (if (null options)
      (setq options igrep-options))
  (if (not (listp files))		; (stringp files)
      (setq files (list files)))
  (if (and (member ?~ (mapcar 'string-to-char files))
	   (save-match-data
	     (string-match "\\`[rj]?sh\\(\\.exe\\)?\\'"
			   (file-name-nondirectory shell-file-name))))
      ;; (restricted, job-control, or standard) Bourne shell doesn't expand ~:
      (setq files
	    (mapcar 'expand-file-name files)))
  (let* ((use-zgrep (cond ((eq igrep-use-zgrep t))
			  (igrep-use-zgrep
			   (let ((files files)
				 (compressed-p nil))
			     (while (and files (not compressed-p))
			       (if (save-match-data
				     (string-match "\\.g?[zZ]\\'" (car files)))
				   (setq compressed-p t))
			       (setq files (cdr files)))
			     compressed-p))
			  (t nil)))
	 (command (format "%s -n %s %s %s %s %s"
			  (if (and use-zgrep
				   (save-match-data
				     (not (string-match "\\`z" program))))
			      (setq program (concat "z" program))
			    program)
			  (or options
			      (and igrep-case-fold-search
				   (equal regex (downcase regex))
				   "-i")
			      "")
			  (or igrep-regex-option
			      (progn
				(if (save-match-data
				      (string-match "\\`-" regex))
				    (setq regex (concat "\\" regex)))
				""))
			  (shell-quote-argument regex)
			  (if igrep-find
			      (if igrep-find-use-xargs
				  ""
				(shell-quote-argument "{}"))
			    (mapconcat (lambda (file)
					 (let ((dir (file-name-directory file)))
					   (if dir
					       (expand-file-name
						(file-name-nondirectory file)
						(igrep-quote-file-name dir))
					     file)))
				       files " "))
			  igrep-null-device)))
    (if igrep-find
	(setq command
	      (igrep-format-find-command command files)))
    (cond ((eq igrep-save-buffers t) (save-some-buffers t))
	  (igrep-save-buffers (save-some-buffers)))
    (if (fboundp 'compilation-start)    ; Emacs 22
        (let ((compilation-process-setup-function 'grep-process-setup))
          (or (fboundp 'igrep-mode)
              (define-derived-mode igrep-mode grep-mode "Igrep"))
          (compilation-start command
                             'igrep-mode
                             nil
                             (cond ((eq compilation-highlight-regexp t))
                                   (compilation-highlight-regexp
                                    (if (eq program "fgrep")
                                        (regexp-quote regex)
                                      regex)))))
      (compile-internal command (format "No more %s matches" program)
                        "Igrep" nil grep-regexp-alist))))

;; Analogue commands:

(defmacro igrep-define (analogue-command &rest igrep-bindings)
  "Define ANALOGUE-COMMAND as an `igrep' analogue command.
Optional (VARIABLE VALUE) arguments specify the temporary IGREP-BINDINGS
for the command."
  ;; (interactive "SCommand: ") ; C-u => read bindings?
  (let ((analogue-program (symbol-name analogue-command)))
    `(defun ,analogue-command (&rest igrep-args)
       ,(format "*Run `%s` via `\\[igrep]'.
All arguments (including prefix arguments, when called interactively)
are handled by `igrep'."
		analogue-program)
       (interactive
	(let ((igrep-program (if igrep-program ,analogue-program))
	      (igrep-program-default ,analogue-program))
	  (igrep-read-args)))
       (let (,@ igrep-bindings)
	 (apply 'igrep
		(cond ((interactive-p) (car igrep-args))
		      ((car igrep-args))
		      (t ,analogue-program))
		(cdr igrep-args))))))

(igrep-define egrep)
(igrep-define fgrep)
(igrep-define agrep
  (igrep-use-zgrep nil)
  (igrep-regex-option "-e"))

;; Recursive (`find`) commands:

;;;###autoload
(defun igrep-find (&rest igrep-args)
  "*Run `grep` via `find`; see `igrep' and `igrep-find'.
All IGREP-ARGS (including prefix arguments, when called interactively)
are handled by `igrep'."
  (interactive
   (let ((igrep-find t))
     (igrep-read-args)))
  (let ((igrep-find t))
    (apply 'igrep igrep-args)))

;; Analogue recursive (`find`) commands:

(defmacro igrep-find-define (analogue-command &rest igrep-bindings)
  "Define ANALOGUE-COMMAND-find as an `igrep' analogue `find` command.
Optional (VARIABLE VALUE) arguments specify the temporary IGREP-BINDINGS
for the command."
  ;; (interactive "SCommand: ") ; C-u => read bindings?
  (let ((analogue-program (symbol-name analogue-command)))
    (setq analogue-command
	  (intern (format "%s-find" analogue-command)))
    `(defun ,analogue-command (&rest igrep-args)
       ,(format "*Run `%s` via `\\[igrep-find]'.
All arguments (including prefix arguments, when called interactively)
are handled by `igrep'."
		analogue-program)
       (interactive
	(let ((igrep-program (if igrep-program ,analogue-program))
	      (igrep-program-default ,analogue-program)
	      (igrep-find t))
	  (igrep-read-args)))
       (let (,@ igrep-bindings)
	 (apply 'igrep-find
		(cond ((interactive-p) (car igrep-args))
		      ((car igrep-args))
		      (t ,analogue-program))
		(cdr igrep-args))))))

(igrep-find-define egrep)
(igrep-find-define fgrep)
(igrep-find-define agrep
  (igrep-use-zgrep nil)
  (igrep-regex-option "-e"))

;;;###autoload
(defun igrep-visited-files (program regex &optional options)
  "*Run `grep` PROGRAM to match REGEX (with optional OPTIONS) \
on all visited files.
See `\\[igrep]'."
  (interactive
   (let ((igrep-args (igrep-read-args 'no-files)))
     ;; Delete FILES:
     (setcdr (nthcdr 1 igrep-args) (nthcdr 3 igrep-args))
     igrep-args))
  (igrep program regex
	 (let ((directory-abbrev-alist
		(cons (cons (regexp-quote (expand-file-name default-directory))
			    "./")	; or even ""
		      directory-abbrev-alist)))
	   (mapcar 'abbreviate-file-name
		   (apply 'nconc
			  (mapcar (lambda (buffer)
				    (let ((file (buffer-file-name buffer)))
				      (if (and file
					       (cond ((featurep 'ange-ftp)
						      (not (ange-ftp-ftp-name file)))
						     ((featurep 'efs)
						      (not (efs-ftp-path file)))
						     (t t))
					       ;; (file-exists-p file)
					       )
					  (list file))))
				  (buffer-list)))))
	 options))

;; Dired commands:

;;;###autoload
(defun dired-do-igrep (program regex &optional options arg)
  "*Search the marked (or next prefix ARG) files.
See `\\[igrep]' for a description of PROGRAM, REGEX, and OPTIONS."
  (interactive
   (let ((igrep-args
	  (let ((current-prefix-arg nil))
	    (igrep-read-args 'no-files))))
     ;; Delete FILES:
     (setcdr (nthcdr 1 igrep-args) (nthcdr 3 igrep-args))
     ;; Append ARG:
     (nconc igrep-args (list current-prefix-arg))))
  (igrep program regex
	 (funcall (cond ((fboundp 'dired-get-marked-files) ; GNU Emacs
			 'dired-get-marked-files)
			((fboundp 'dired-mark-get-files) ; XEmacs
			 'dired-mark-get-files))
		  t arg)
	 options))

;; Dired recursive (`find`) commands:

;;;###autoload
(defun dired-do-igrep-find (program regex &optional options arg)
  "*Run `grep` on the marked (or next prefix ARG) directories.
See `\\[igrep]' for a description of PROGRAM, REGEX, and OPTIONS."
  (interactive
   (let ((igrep-args
	  (let ((current-prefix-arg nil)
		(igrep-find t))
	    (igrep-read-args 'no-files))))
     ;; Delete FILES:
     (setcdr (nthcdr 1 igrep-args) (nthcdr 3 igrep-args))
     ;; Append ARG:
     (nconc igrep-args (list current-prefix-arg))))
  (let ((igrep-find t))
    (dired-do-igrep program regex options arg)))

;; Buffer menu commands:

;;;###autoload
(defun Buffer-menu-igrep (program regex &optional options)
  "*Run `grep` on the files visited in buffers marked with '>'.
See `\\[igrep]' for a description of PROGRAM, REGEX, and OPTIONS."
  (interactive
   (let ((igrep-args (igrep-read-args 'no-files)))
     ;; Delete FILES:
     (setcdr (nthcdr 1 igrep-args) (nthcdr 3 igrep-args))
     igrep-args))
  ;; See Buffer-menu-select:
  (let ((marked-files '())
	marked-buffer
	file)
    (goto-char (point-min))
    (while (search-forward "\n>" nil t)
      (setq marked-buffer (Buffer-menu-buffer t)
	    file (buffer-file-name marked-buffer))
      (if (and file
	       ;; local:
	       (cond ((featurep 'ange-ftp)
		      (not (ange-ftp-ftp-name file)))
		     ((featurep 'efs)
		      (not (efs-ftp-path file)))
		     (t t)))
	  (setq marked-files (cons file marked-files)))
;;;    (let ((buffer-read-only nil))
;;;      (delete-char -1)
;;;      (insert ?\ ))
      )
    (setq marked-files (nreverse marked-files))
    (igrep program regex
	   (let ((directory-abbrev-alist
		  (cons (cons (regexp-quote (expand-file-name default-directory))
			      "./")	; or even ""
			directory-abbrev-alist)))
	     (mapcar 'abbreviate-file-name marked-files))
	   options)))

;; User functions:

(defun igrep-dired-file-current-word ()
  "Return the current word in the file on this line, if it is visible;
else, return the file name on this line, if there is one;
otherwise, return the current word."
  (let* ((dired-file
	  (dired-get-filename t t))
	 (dired-file-buffer
	  (if dired-file
	      (get-file-buffer (expand-file-name dired-file))))
	 (dired-file-buffer-window
	  (if dired-file-buffer
	      (get-buffer-window dired-file-buffer))))
    (cond (dired-file-buffer-window (save-excursion
				      (set-buffer dired-file-buffer)
				      (current-word)))
	  (dired-file)
	  (t (current-word)))))

(defun igrep-buffer-file-name-pattern ()
  "Return a shell file name pattern based on the visited file name.
If the `buffer-file-name' variable is nil, return \"*\"."
  ;; (Based on other-possibly-interesting-files in ~/as-is/unix.el, by
  ;; Wolfgang Rupprecht <wolfgang@mgm.mit.edu>.)
  (if buffer-file-name
      (let ((file-name (file-name-nondirectory buffer-file-name)))
	(concat "*"
		(save-match-data
		  (if (string-match "\\.[^.]+\\(\\.g?[zZ]\\)?\\'"
				    file-name)
		      (substring file-name (match-beginning 0)
				 (match-end 0))))))
    "*"))

(defun igrep-dired-directory-file-pattern ()
  "Return a shell file name pattern based on `dired-directory', or \"*\"."
  (cond ((stringp dired-directory)
	 (if (file-directory-p dired-directory)
	     "*"
	   (file-name-nondirectory dired-directory))) ; wildcard
	((consp dired-directory)	; (DIR FILE ...)
	 (mapconcat 'identity (cdr dired-directory) " "))))

;; Utilities:

(defsubst igrep-file-directory (name)
  "Return the directory component of NAME, or \".\" if it has none."
  (directory-file-name (or (file-name-directory name)
			   (file-name-as-directory "."))))

(defsubst igrep-file-pattern (name)
  "Return the file component of NAME, or \"*\" if it has none."
  (let ((pattern (file-name-nondirectory name)))
       (if (string= pattern "")
	   "*"
	 pattern)))

(defun igrep-format-find-command (command files)
  "Format `grep` COMMAND to be invoked via `find` on FILES."
  (let ((directories '())
	(patterns '()))
    (while files
      (let ((dir (igrep-file-directory (car files)))
	    (pat (igrep-file-pattern (car files))))
	(if (and (not (string= dir "."))
		 (file-symlink-p dir))
	    (setq dir (concat dir "/.")))
	(if (not (member dir directories))
	    (setq directories (cons dir directories)))
	(cond ((equal pat "*")
	       (setq patterns t))
	      ((and (listp patterns)
		    (not (member pat patterns)))
	       (setq patterns (cons pat patterns)))))
      (setq files (cdr files)))
    (format (cond ((eq igrep-find-use-xargs 'gnu)
		   ;; | \\\n
		   "%s %s %s %s %s -print0 | xargs -0 -e %s")
		  (igrep-find-use-xargs
		   ;; | \\\n
		   "%s %s %s %s %s -print | xargs -e %s")
		  (t
		   "%s %s %s %s %s -exec %s %s"))
	    igrep-find-program
	    (mapconcat 'igrep-quote-file-name (nreverse directories)
		       " ")
	    (if igrep-find-prune-clause
		(format "%s -prune -o" igrep-find-prune-clause)
	      "")
	    (or igrep-find-file-clause "")
	    (if (listp patterns)
		(if (cdr patterns)	; (> (length patterns) 1)
		    (format "%s %s %s"
			    (shell-quote-argument "(")
			    (mapconcat (lambda (pat)
					 (format "-name %s"
						 (shell-quote-argument pat)))
				       (nreverse patterns)
				       " -o ")
			    (shell-quote-argument ")"))
		  (format "-name %s" (shell-quote-argument (car patterns))))
	      "")
	    command
	    (shell-quote-argument ";")
	    )))

(defmacro igrep-default-arg (variable)
  "Return the default arg based on VARIABLE."
  `(if ,variable
       (cond ((get (quote ,variable) major-mode)
	      (funcall (get (quote ,variable) major-mode)))
	     (t (funcall ,variable)))))

(defun igrep-default-regex ()
  "Return the default REGEX for `\\[igrep]'."
  (let ((default-regex (igrep-default-arg igrep-regex-default)))
    (if (not (equal default-regex ""))
	default-regex)))

(defun igrep-default-files ()
  "Return the default FILES for `\\[igrep]'."
  (let* ((dired-subdirectory (if (cond ((fboundp 'derived-mode-p) ; Emacs 21
					(derived-mode-p 'dired-mode))
				       (t (eq major-mode 'dired-mode)))
				 (dired-current-directory t)))
	 (default-files (igrep-default-arg igrep-files-default)))
    (if (not (listp default-files))	; stringp
	(setq default-files (list default-files)))
    (if dired-subdirectory
	(mapcar (lambda (file)
		  (concat dired-subdirectory file))
		default-files)
      default-files)))

(defsubst igrep-prefix (prefix string &rest strings)
  "Concatenate PREFIX (if non-nil), STRING, and any other STRINGS."
  (if (or prefix strings)
      (apply 'concat prefix string strings)
    string))

(defun igrep-read-args (&optional no-files)
  "Read and return a list: (PROGRAM REGEX FILES OPTIONS).
If NO-FILES is non-nil, then FILES is not read and nil is returned
in its place."
  (let* ((pre-prefix (if (and igrep-find (eq igrep-verbose-prompts t))
			 "[find] "))
	 (program
	  (igrep-read-program pre-prefix))
	 (prefix (if (and program (eq igrep-verbose-prompts t))
		     (igrep-prefix pre-prefix program " ")
		   pre-prefix))
	 (options
	  (igrep-read-options prefix))
	 (post-prefix (if (and options (eq igrep-verbose-prompts t))
			    (igrep-prefix prefix options " ")
			  prefix)))
    (list program
	  (igrep-read-regex post-prefix)
	  (if (not no-files)
	      (igrep-read-files post-prefix))
	  options)))

(defun igrep-read-program (&optional prompt-prefix)
  "Read and return a `grep` program name from the minibuffer.
If `igrep-program' is non-nil, it.

Optional PROMPT-PREFIX is prepended to the \"Program: \" prompt."
  (or igrep-program
      (let ((prompt "Program: "))
	(completing-read (igrep-prefix prompt-prefix prompt) igrep-program-table
			 nil t igrep-program-default))))

(defun igrep-read-options (&optional prompt-prefix)
  "Read and return an options string from the minibuffer.
If `current-prefix-arg' is '(4) or '(64), return `igrep-options'.

Optional PROMPT-PREFIX is prepended to the \"Options: \" prompt."
  (if (or igrep-read-options
	  (and (consp current-prefix-arg)
	       (memq (prefix-numeric-value current-prefix-arg)
		     '(4 64))))
      (let ((prompt "Options: "))
	(read-string (igrep-prefix prompt-prefix prompt)
		     (or igrep-options "-")))
    igrep-options))

(defun igrep-read-regex (&optional prompt-prefix)
  "Read and return a `grep` regex(5) string from the minibuffer.
Optional PROMPT-PREFIX is prepended to the \"Regex: \" prompt."
  (if igrep-insert-default-key
      (define-key minibuffer-local-map igrep-insert-default-key
	'igrep-insert-default-regex))
  (let* ((default-regex (igrep-default-regex))
	 (prompt (igrep-prefix prompt-prefix
			       (if default-regex
				   (format "Regex [default: %s]: "
					   default-regex)
				 "Regex: ")))
	 (regex (cond ((featurep 'xemacs) ; incompatible
                       ;; DEFAULT-VALUE is the 7th arg in 21.4 (but 21.1
                       ;; only accepts 6 args):
		       (read-from-minibuffer prompt
					     nil nil nil
					     'igrep-regex-history
					     nil)) ; ABBREV-TABLE
		      ((>= emacs-major-version 20)
		       (read-from-minibuffer prompt
					     nil nil nil
					     'igrep-regex-history
					     default-regex))
		      (t
		       (read-from-minibuffer prompt
					     nil nil nil
					     'igrep-regex-history)))))
    (if (equal regex "")
	(progn
	  (or (equal default-regex (car igrep-regex-history))
	      (setq igrep-regex-history
		    (cons default-regex igrep-regex-history)))
	  default-regex)
      regex)))

(defun igrep-insert-default-regex (&optional clear-minibuffer)
  "*Insert the default regex in the minibuffer.
If a prefix argument is specified, CLEAR-MINIBUFFER contents first."
  (interactive "P")
  (if clear-minibuffer
      (delete-region (if (fboundp 'minibuffer-prompt-end) ; Emacs 21
			 (minibuffer-prompt-end)
		       (point-min))
		     (point-max)))
  (insert (or (save-excursion
		(set-buffer (window-buffer minibuffer-scroll-window))
		(igrep-default-regex))
	      "")))

(defun igrep-insert-default-files (&optional clear-minibuffer)
  "*Insert the default files in the minibuffer.
If a prefix argument is specified, CLEAR-MINIBUFFER contents first."
  (interactive "P")
  (if clear-minibuffer
      (delete-region (if (fboundp 'minibuffer-prompt-end) ; Emacs 21
			 (minibuffer-prompt-end)
		       (point-min))
		     (point-max)))
  (insert (mapconcat 'identity
		     (save-excursion
		       (set-buffer (window-buffer minibuffer-scroll-window))
		       (igrep-default-files))
		     " ")))

(defsubst igrep-default-key (command &optional keymap key)
  "Return the key bound to COMMAND in KEYMAP, preferably KEY."
  (if (null keymap)
      (setq keymap (current-global-map)))
  (if (and key
	   (eq (lookup-key keymap key) command))
      key
    (where-is-internal command keymap t)))

(defun igrep-read-files (&optional prompt-prefix)
  "Read and return a file name pattern from the minibuffer.
If `current-prefix-arg' is '(16) or '(64), read multiple file name
patterns and return them in a list.  Optional PROMPT-PREFIX is
prepended to the \"File(s): \" prompt."
  (let* ((default-files (igrep-default-files))
	 (default-files-string (mapconcat 'identity default-files " "))
	 (insert-default-directory igrep-insert-default-directory)
	 (file (igrep-read-file-name
		(igrep-prefix prompt-prefix
			      (if default-files
				  (format "File(s) [default: %s]: "
					  default-files-string)
				"File(s): "))
		nil (if default-files default-files-string "") nil nil
		'igrep-files-history))
	 (files (list file)))
    (if (or igrep-read-multiple-files
	    (and (consp current-prefix-arg)
		 (memq (prefix-numeric-value current-prefix-arg)
		       '(16 64))))
	(let* ((key (igrep-default-key 'exit-minibuffer
				       minibuffer-local-completion-map
				       "\r"))
	       (prompt
		(igrep-prefix prompt-prefix
			      (if igrep-verbose-prompts
				  (format "File(s): [Type `%s' when done] "
					  (key-description key))
				"File(s): "))))
	  (while (and (setq file
			    (igrep-read-file-name prompt
						  nil "" nil nil
						  'igrep-files-history))
		      (not (equal file "")))
	    (setq files (cons file files)))))
    (mapcar (lambda (file)
	      (if (file-directory-p file)
		  ;; really should map expand-file-name over default-files:
		  (expand-file-name (if default-files default-files-string "*")
				    file)
		file))
	    (nreverse files))))

(defun igrep-read-file-name (prompt
  &optional directory default existing initial history)
  "Just like `read-file-name,' but with optional HISTORY."
  (if igrep-insert-default-key
      (define-key minibuffer-local-completion-map igrep-insert-default-key
	'igrep-insert-default-files))
  (if history
      (let ((file-name-history (symbol-value history)))
	(prog1 (read-file-name prompt directory default existing initial)
	  (set history file-name-history)))
    (read-file-name prompt directory default existing initial)))

;; Menu bar:

(if igrep-easy-menu
    (progn
      (easy-menu-define igrep-menu nil
	"Menu keymap for igrep."
	igrep-easy-menu)
      (cond ((fboundp 'add-submenu)	; XEmacs
	     (add-submenu '("Tools") igrep-menu "Grep..."))
	    ((fboundp 'easy-menu-add-item) ; Emacs 20
	     (easy-menu-add-item menu-bar-tools-menu nil igrep-menu
				 'grep))
	    (t				; Emacs 19
	     (define-key-after menu-bar-tools-menu [igrep]
	       (cons (car igrep-easy-menu) igrep-menu)
	       (and (lookup-key menu-bar-tools-menu [grep]) 'grep))))))

;;; Local Variables:
;;; eval: (put 'igrep-define 'lisp-indent-function 1)
;;; eval: (put 'igrep-find-define 'lisp-indent-function 1)
;;; eval: (put 'easy-menu-define 'lisp-indent-function 'defun)
;;; End:

(provide 'igrep)

;;; igrep.el ends here
