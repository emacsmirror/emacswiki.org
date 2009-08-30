;;; bk.el --- Emacs support for BitKeeper configuration management

;; Copyright (C) 2003 Bryan O'Sullivan

;; Author: Bryan O'Sullivan <bos@serpentine.com>

;; $Id: s.bk.el 1.57 03/04/09 14:38:16-07:00 ;;bos@serpentine.internal.keyresearch.com $

;; bk.el ("this file") is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file, GNU Emacs, or XEmacs; see the file COPYING
;; (`C-h C-l').  If not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mode builds upon Emacs's VC mode to provide flexible
;; integration with the BitKeeper software configuration management
;; product.

;; To get going as quickly as possible, load this file into Emacs and
;; type `C-c b h', which runs bk-help-overview, which prints a helpful
;; usage overview.

;; Much of the inspiration for bk.el comes from Rajesh
;; Vaidheeswarran's excellent p4.el, which does an admirably thorough
;; job for the Perforce SCM product.  In fact, substantial chunks of
;; code are adapted from p4.el.

;; This code has been developed under XEmacs 21.4, and probably will
;; not work as well under GNU Emacs (albeit tested under 21.2).
;; Patches to enhance the portability of this code, fix bugs, and add
;; features are most welcome.  You can obtain a BitKeeper repository
;; for this package by cloning bk://bk-emacs.bkbits.net/emacs

;; Please send problem reports and suggestions to bos@serpentine.com.

;;; Code:

(require 'advice)
(require 'cl)
(require 'diff-mode)
(require 'easymenu)
(require 'man)
(require 'timer)
(require 'vc)
;; It's fine to turn this off if you don't actually need it.
(require 'x-migrant)

;; XEmacs has view-less, while GNU Emacs has view.  Joy.

(condition-case nil
    (require 'view-less)
  (error nil))
(condition-case nil
    (require 'view)
  (error nil))

(defconst bk-version "$Revision: 1.57 $"
  "The current version of the Emacs/BitKeeper integration package.")

(defgroup bk nil
  "BitKeeper SCM System."
  :group 'tools)

(defcustom bk-binary
  (dolist (path '("/net/bk/bin/bk"
		  "/usr/bin/bk"
		  "/usr/local/bin/bk"
		  "/usr/local/bitkeeper/bk"
		  "/usr/libexec/bitkeeper/bk"))
    (when (file-executable-p path)
      (return path)))
  "The path to BitKeeper's bk executable."
  :type 'string
  :group 'bk)
      
(defcustom bk-sfiles-args "-ig"
  "Arguments to pass to 'bk sfiles' by default."
  :type 'string
  :group 'bk)
      
(defcustom bk-diffs-args "-u"
  "Arguments to pass to 'bk diffs' by default."
  :type 'string
  :group 'bk)

(defcustom bk-user-name (or (getenv "BK_USER") (user-login-name) (getenv "USER"))
  "The name of the user under which to perform bk operations."
  :type 'string
  :group 'bk)

(defconst bk-running-xemacs (string-match "XEmacs" emacs-version)
  "Is bk.el running under XEmacs?")

(defun bk-replace-in-string (str regexp newtext &optional literal)
  "Replace all matches in STR for REGEXP with NEWTEXT string.
Return the new string.  Optional LITERAL non-nil means do a literal
replacement.

This function bridges yet another pointless impedance gap between
XEmacs and GNU Emacs."
  (if (fboundp 'replace-in-string)
      (replace-in-string str regexp newtext literal)
    (replace-regexp-in-string regexp newtext str nil literal)))

(defun bk-chomp (str)
  "Strip trailing newlines from a string."
  (bk-replace-in-string str "[\r\n]+$" ""))

(defun bk-run-command (command &rest args)
  "Run the shell command COMMAND, returning (EXIT-CODE . COMMAND-OUTPUT).
The list ARGS contains a list of arguments to pass to the command."
  (let* (exit-code
	 (output
	  (with-output-to-string
	    (with-current-buffer
		standard-output
	      (setq exit-code
		    (apply 'call-process command nil t nil args))))))
    (cons exit-code output)))
    
(defun bk-run (&rest args)
  "Run the BitKeeper command COMMAND, returning (EXIT-CODE . COMMAND-OUTPUT)."
  (apply 'bk-run-command bk-binary args))

(defun bk-run0 (&rest args)
  "Run the BitKeeper command COMMAND, returning its output.
If the command does not exit with a zero status code, error out."
  (let ((res (apply 'bk-run-command bk-binary args)))
    (if (not (eq (car res) 0))
	(error "BitKeeper command failed %s - exit code %s" args (car res))
      (cdr res))))
  
(defcustom bk-bin-directory
  (and bk-binary (bk-chomp (bk-run0 "bin")))
  "The BitKeeper binary directory."
  :type 'string
  :group 'bk)

(defcustom bk-sccs-is-bk
  (dolist (path (split-string (getenv "PATH") ":"))
    (when (string-match "/bitkeeper/delta$"
			(file-truename (concat path "/delta")))
      (return bk-bin-directory)))
  "Non-nil if SCCS commands in the shell path are implemented by BitKeeper."
  :type 'string
  :group 'bk)

(defcustom bk-register-switches nil
  "A list of strings; extra switches for registering a file under BitKeeper.
These are passed to the checkin program by \\[vc-register]."
  :type '(choice (const :tag "None" nil)
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'bk)

(defcustom bk-mode-hook nil
  "Hook run when a buffer enters bk-mode."
  :type 'sexp
  :group 'bk)

(defcustom bk-global-prefix "\C-cb"
  "The global prefix for bk keymap bindings."
  :type 'sexp
  :group 'bk)

(defvar bk-mode nil
  "Is this file managed by BitKeeper?")
(make-variable-buffer-local 'bk-mode)
(put 'bk-mode 'permanent-local t)

(defvar bk-license nil
  "If this file is managed by BitKeeper, indicate the repository's license.
The value of this variable is the same as returned by \\[bk-license].")
(make-variable-buffer-local 'bk-license)

(defvar bk-output-buffer-name "*bk*"
  "The name to use for BK output buffers.")

(defvar bk-file-name-history nil)

(unless (fboundp 'set-keymap-name)
  (defun set-keymap-name (map name)))

(defvar bk-prefix-map
  (let ((map (copy-keymap vc-prefix-map)))
    (set-keymap-name map 'bk-prefix-map)
    map)
  "This keymap overrides some default vc-mode bindings.")
(fset 'bk-prefix-map bk-prefix-map)
(define-key bk-prefix-map "=" 'bk-diffs)
(define-key bk-prefix-map "g" 'bk-annotate)
(define-key bk-prefix-map "l" 'bk-print-revision-summary)
(define-key bk-prefix-map "~" 'bk-print-rev)

(global-set-key "\C-xvi" 'bk-register)

(defvar bk-mode-map (make-sparse-keymap))
(define-key bk-mode-map "\C-xv" 'bk-prefix-map)
  
(defvar bk-global-map (make-sparse-keymap))
(fset 'bk-global-map bk-global-map)
(global-set-key bk-global-prefix 'bk-global-map)
(define-key bk-global-map "," 'bk-pull)
(define-key bk-global-map "." 'bk-push)
(define-key bk-global-map "=" 'bk-recursive-diffs)
(define-key bk-global-map "i" 'bk-interesting-diffs)
(define-key bk-global-map "?" 'bk-help-overview)
(define-key bk-global-map "c" 'bk-citool)
(define-key bk-global-map "h" 'bk-help-overview)
(define-key bk-global-map "k" 'bk-apropos) ; by analogy with "man -k"
(define-key bk-global-map "m" 'bk-help) ; by analogy with "man"
(define-key bk-global-map "r" 'bk-revtool)
(define-key bk-global-map "s" 'bk-sfiles)

(add-minor-mode 'bk-mode 'bk-mode bk-mode-map)

(easy-menu-define bk-mode-menu bk-mode-map
  "Menu for `bk-mode'."
  '("BitKeeper"
    ["Compare With Last Revision"	bk-diffs			t]
    ["Revision Summary"			bk-print-revision-summary	t]
    ["Annotated Listing"		bk-annotate			t]
    ["Visit Other Revision"		bk-print-rev			t]
    "-----"
    ["Check File In/Out"		vc-next-action			t]
    ["Revert to Last Revision"		vc-revert-buffer		t]
    "-----"
    ["Push ChangeSets to Parent"	bk-push				t]
    ["Pull ChangeSets from Parent"	bk-pull				t]
    "-----"
    ("GUI Tools"
     ["Browse Revisions"		bk-revtool			t]
     ["Browse ChangeSets"		bk-csettool			t]
     ["Browse Differences"		bk-difftool			t]
     "-----"
     ["Check In / Commit Changes"	bk-citool			t]
     "-----"
     ["Browse Online Help"		bk-helptool			t]
     )
    ["Help Topics"			bk-help				t]
    ["Search Help"			bk-apropos			t]
    ))

(defvar bk-view-mode-map
  (let ((map (copy-keymap (if (boundp 'view-minor-mode-map)
			      view-minor-mode-map
			    view-mode-map))))
    (set-keymap-name map 'bk-view-mode-map)
    map))
(fset 'bk-view-mode-map bk-view-mode-map)
(define-key bk-view-mode-map
  (if bk-running-xemacs [button2] [mouse-2])
  'bk-buffer-mouse-clicked)

(defun bk-split-lines (str)
  (split-string (bk-chomp str) "[\n]"))

(defun bk-checkin-or-revert ()
  "Revert the current buffer if it is the same as the sfile, else checkin."
  (let ((diff (bk-run0 "diffs" buffer-file-name)))
    (if (string= diff "")
	(progn
	  (vc-backend-revert buffer-file-name)
	  (vc-resynch-window buffer-file-name t t))
      (vc-checkin buffer-file-name))))
      
;; GNU Emacs does not have vc-locking-user.

(defsubst bk-locking-user (file)
  (if (fboundp 'vc-state)
      (progn
	(vc-file-clearprops file)
	(if (eq (vc-state file) 'up-to-date)
	    nil
	  (if (memq (vc-state file) '(edited needs-merge))
	      (vc-user-login-name)
	    (vc-state file))))
    (vc-locking-user file)))

;; Lifted from XEmacs' vc.
(defun bk-checkout-writable-buffer (&optional file rev)
  "Retrieve a writable copy of the latest version of the current buffer's file."
  (vc-checkout (or file (buffer-file-name)) t rev))

(defadvice vc-toggle-read-only (around bk-toggle-read-only activate)
  "Augmented to deal better with BitKeeper.
Specifically, if the file is not under BitKeeper management, handle it
with VC.  Otherwise, check the file in or out using BitKeeper."
  (vc-buffer-sync)
  (let* ((file buffer-file-name)
	 (vc-type (vc-backend file))
	 (vc-file (vc-name file))
	 (vc-owner (bk-locking-user file)))
    (cond
     ((not vc-file)
      (if (and bk-sccs-is-bk (eq vc-type 'SCCS))
	  (bk-register)
	ad-do-it))
     ((not vc-owner)
      (bk-checkout-writable-buffer))
     ((string= vc-owner (vc-user-login-name))
      (bk-checkin-or-revert))
     (error "This file is locked by %s" vc-owner))))

(defun bk-root (&optional path)
  "Return the root of the BK repository that houses PATH, or nil if none."
  (interactive)
  (unless path
    (setq path ""))
  (let ((root (bk-run "root" (expand-file-name path))))
    (if (interactive-p)
	(if (eq (car root) 0)
	    (message "The root of this repository is \"%s\"."
		     (bk-chomp (cdr root)))
	  (message "This path is not under BitKeeper control!"))
      (if (eq (car root) 0) (bk-chomp (cdr root)) nil))))
      
(defun bk-parent ()
  "Return the parent of the current BK repository."
  (interactive)
  (let* ((result (bk-run "parent" "-p"))
	 (parent (and (eq (car result) 0) (bk-chomp (cdr result)))))
    (if (interactive-p)
	(if parent
	    (message "The parent of this repository is `%s'" parent)
	  (message "I see no parent here!"))
      parent)))

(defun bk-file-is-bk (file)
  "If FILE is under BK control, return its SCCS version, else nil."
  (let ((res (bk-run "prs" "-h" "-r+" "-d:REV:" (expand-file-name file))))
    (and (eq (car res) 0) (cdr res))))

(defun bk-license (&optional path)
  "Indicate the license of a BitKeeper repository.

BitKeeper manages licensing on a per-repository basis.  This function
lets you determine which license applies to a particular repository.

Called interactively, it prompts for a path name within a BK
repository, and indicates what the license is.

Otherwise, this function returns a pair, of which the car is one of
the following symbols:

single-user   license is single-user BKL, cdr is the licensed user
logging       license is multi-user BKL, cdr is the logging email address
commercial    license is commercial BitKeeper license, cdr is nil"
  (interactive "FEnter a path in a BK repository: ")
  (if path
      (setq path (expand-file-name path))
    (setq path buffer-file-name))
  (let* ((root (bk-root path))
	 license)
    (when root
      (let ((config (bk-run0 "get" "-q" "-p"
			     (concat root "/BitKeeper/etc/config"))))
	(cond
	 ((string-match "^single_user:\\s-*\\([^\r\n$]*\\)" config)
	  (setq license (cons 'single-user (substring config
						      (match-beginning 1)
						      (match-end 1)))))
	 ((string-match "^licsign[0-9]:" config)
	  (setq license '(commercial)))
	 ((string-match "^logging:\\s-*\\([^\r\n$]*\\)" config)
	  (setq license (cons 'logging (substring config
						  (match-beginning 1)
						  (match-end 1))))))))
    (if (not (interactive-p))
	license
      (case (car license)
	(single-user
	 (message "Licensed under the single-user BKL, to user %s"
		  (cdr license)))
	(logging
	 (message "Licensed under the open-logging BKL, logging to %s"
		  (cdr license)))
	(commercial
	 (message "Commercially licensed under the BKCL"))
	(t
	 (if root
	     (message "Unknown license type in %s" root)
	   (message "There is no BitKeeper repository at %s" path)))))))

(defun bk-modify-menu-item (menu item-name state)
  (let ((item (find-if (function 
			(lambda (x)
			  (and (vectorp x) (equal (aref x 0) item-name))))
		       menu)))
    (when item
      (aset item 2 state))))

(defun bk-find-file-hook ()
  (when (bk-file-is-bk buffer-file-name)
    (let ((license (bk-license buffer-file-name)))
      (if (memq (car license) '(single-user logging))
          (setq bk-mode " BKL")
        (setq bk-mode " BKCL"))
      (setq bk-license license))
    (easy-menu-add bk-mode-menu)
    (run-hooks 'bk-mode-hook)))

(defalias 'bk-checkin-hook 'bk-find-file-hook)

(add-hook 'find-file-hooks 'bk-find-file-hook)
(add-hook 'vc-checkin-hook 'bk-checkin-hook)

(defun bk-vc-do-command (buffer okstatus command file last &rest flags)
  (if bk-running-xemacs
      (apply 'vc-do-command buffer okstatus command file last flags)
    (apply 'vc-do-command buffer okstatus command file flags)))

(defun bk-register (&optional rev comment)
  "Register a file under BitKeeper.
The file may have an edit or get applied after registration, depending
on your repository's policy."
  (interactive "P")
  (unless buffer-file-name
    (error "No visited file"))
  (when (vc-backend buffer-file-name)
    (when (vc-registered buffer-file-name)
      (error "This file is already registered"))
    (unless (y-or-n-p "Previous master file has vanished.  Make a new one? ")
	(error "Aborted")))
  (when (and (not (buffer-modified-p))
	     (zerop (buffer-size))
	     (not (file-exists-p buffer-file-name)))
    (set-buffer-modified-p t))
  (vc-buffer-sync)
  (if (or (bk-root buffer-file-name) bk-sccs-is-bk)
      (progn
	(when (and (bk-root buffer-file-name) rev)
	  (error "You cannot specify an initial rev for BitKeeper files!"))
	(bk-register-1 buffer-file-name comment))
    (vc-register rev comment)))

(defun bk-register-1 (file &optional comment)
  (message "Registering %s with BitKeeper..." file)
  (let ((switches (append bk-register-switches
			  (if (stringp vc-register-switches)
			      (list vc-register-switches)
			    vc-register-switches)
			  (if (boundp 'vc-sccs-register-switches)
			      (if (stringp vc-sccs-register-switches)
				  (list vc-sccs-register-switches)
				vc-sccs-register-switches)))))
    (apply 'bk-vc-do-command nil 0 "delta" file 'MASTER
	   "-i"
	   (and comment (concat "-y" comment))
	   (format
	    (car (rassq 'SCCS vc-master-templates))
	    (or (file-name-directory file) "")
	    (file-name-nondirectory file))
	   switches))
  (message "Registering %s with BitKeeper...done" file)
  (when vc-keep-workfiles
    (when (not (file-exists-p file))
      (bk-vc-do-command nil 0 "get" file 'MASTER))
    (find-alternate-file file)))

(when (fboundp 'vc-backend-admin)
  (defadvice vc-backend-admin (around bk-vc-admin-workaround activate)
    "Replace the default VC handler for registering SCCS files.
This handler deals more correctly with BitKeeper's SCCS command line."
    (if (and bk-sccs-is-bk (eq (vc-backend file) 'SCCS))
	(progn
	  (when (and (bk-root file) rev)
	    (error "You cannot specify an initial rev for BitKeeper files!"))
	  (bk-register-1 file comment))
      ad-do-it)))

(defun bk-check-supported (command called-interactively)
  (when (and bk-mode (bk-root))
    (error "The '%s' %s is not supported inside a BitKeeper repository."
	   command
	   (if called-interactively "command" "function"))))

(defadvice vc-create-snapshot (around bk-maybe-unsupported activate)
  "This command is not supported inside a BitKeeper repository."
  (interactive (progn (bk-check-supported this-command t)
		      (list (read-string "New snapshot name: "))))
  (bk-check-supported 'vc-create-snapshot (interactive-p))
  ad-do-it)

(defadvice vc-retrieve-snapshot (around bk-maybe-unsupported activate)
  "This command is not supported inside a BitKeeper repository."
  (interactive (progn (bk-check-supported this-command t)
		      (list (read-string "New snapshot name: "))))
  (bk-check-supported 'vc-retrieve-snapshot (interactive-p))
  ad-do-it)

(defun bk-list-revs (file)
  "Return a list of all revisions of FILE.
The list is ordered from newest to eldest."
  (split-string (bk-chomp (bk-run0 "prs" "-h" "-d:REV:\\n"
				   (expand-file-name file)))
		"\n"))

(defun bk-read-rev (file &optional prompt latest)
  "Read FILE's revision number from the minibuffer.
Offer completion on existing revision numbers for FILE."
  (let* ((revs (bk-list-revs file))
	 (dups (mapcar (function (lambda (x) (list x x))) revs))
	 (def (car revs))
	 (rev (completing-read (format "%s %s%s: "
				      (or prompt "Revision of")
				      (file-name-nondirectory file)
				      (if latest (concat " (" def ")") ""))
			       dups nil t nil nil (and latest def))))
      (when (> (length rev) 0)
	rev)))

(defun bk-list-csets ()
  "Return a list of all ChangeSets in the current repository.
The list is ordered from newest to eldest."
  (split-string (bk-chomp (bk-run0 "changes" "-d:REV:\\n")) "\n"))

(defun bk-read-cset (&optional prompt latest)
  "Read a ChangeSet revision number from the minibuffer.
Offer completion on existing ChangeSet numbers."
  (let* ((csets (bk-list-csets))
	 (dups (mapcar (function (lambda (x) (list x x))) csets))
	 (def (car dups))
	 (cset (completing-read (format "%s %s: "
					(or prompt "ChangeSet")
					(if latest (concat "(" def ")") ""))
				dups nil t nil nil (and latest def))))
    (when (> (length cset) 0)
      cset)))

(defmacro bk-view-output (args &rest body)
  "Execute BODY in a clean buffer, then switch that buffer to view-mode.
ARGS is of the form (BUFFER-NAME &optional FILE), where BUFFER-NAME is
the name of the buffer to create, and FILE is the name of the file
being viewed."
  (let ((name (gensym "buf-name-"))
	(prev-buf (gensym "prev-buf-"))
	(v-b-name (car args))
	(v-m-rest (cdr args)))
    `(let ((view-buf-name ,v-b-name)
	   (,prev-buf (current-buffer)))
       (get-buffer-create view-buf-name)
       (kill-buffer view-buf-name)
       (pop-to-buffer view-buf-name)
       (save-excursion
	 ,@body)
       (bk-view-mode ,prev-buf ,@v-m-rest))))

(put 'bk-view-output 'lisp-indent-function 1)

(defun bk-print-rev (file rev &optional prev-buf)
  "Display revision REV of FILE in a `view-mode' buffer."
  (interactive
   (let ((file
	  (if (and bk-mode (not current-prefix-arg))
	      buffer-file-name
	    (read-file-name
	     "File to display: " nil nil t nil 'bk-file-name-history))))
     (list file (bk-read-rev file nil t))))
  (bk-view-output ((format "BK: Rev %s of %s" rev file))
    (call-process bk-binary nil t nil "get" "-p" "-q" (concat "-r" rev)
		  (expand-file-name file))
    (let ((buffer-file-name file)
	  (buffer-file-truename file))
      (set-auto-mode)
      (run-hooks 'find-file-hooks))))

(defun bk-diffs (file rev1 &optional rev2 prev-buf)
  "Display diffs between revisions REV1 and REV2 of FILE in a buffer.
If REV2 is nil, the diffs will run from REV1 to whatever is currently
checked out.

If called with a prefix argument, this command prompts for the file
name and revisions to use."
  (interactive
   (let ((file
	  (if (and bk-mode (not current-prefix-arg))
	      buffer-file-name
	    (read-file-name "File to diff: " nil nil t nil
			    'bk-file-name-history))))
     (if current-prefix-arg
	 (list file
	       (bk-read-rev file "First revision of" t)
	       (bk-read-rev file "Second revision of"))
       (let ((newest (car (bk-list-revs file))))
	 (list file newest nil)))))
  (let ((afile (bk-abbrev-file-name file)))
    (bk-view-output ((cond
		     ((not rev2)
		      (format "BK: Rev %s to Current of %s"
			      rev1 afile))
		     ((string= rev1 rev2)
		      (format "BK: Rev %s of %s" rev1 afile))
		     ((format "BK: Revs %s to %s of %s"
			      rev1 rev2 afile))))
      (call-process bk-binary nil t nil "diffs" bk-diffs-args
		    (if rev2
			(concat "-r" rev1 ".." rev2)
		      (concat "-r" rev1))
		    (expand-file-name file))
      (diff-mode)
      (font-lock-fontify-buffer))))

(unless (fboundp 'read-directory-name)
  (defalias 'read-directory-name 'read-file-name))

(defvar bk-sfiles-history nil)

(defun bk-recursive-diffs (&optional sfiles-args dir)
  "Run 'bk diffs' recursively under DIR, on files identified by SFILES-ARGS.
If DIR is nil, use the root of the current BK repository.  Called
interactively, prompt for DIR."
  (interactive)
  (unless sfiles-args
    (setq sfiles-args (or (and (interactive-p) current-prefix-arg
			       (read-string "Arguments to bk sfiles: "
					    "-gc"
					    bk-sfiles-history))
			  "-gc")))
  (unless dir
    (setq dir (or (and (interactive-p) current-prefix-arg
		       (read-directory-name "Directory to diff: "))
		  (bk-root)
		  ".")))
  (bk-view-output ((format "BK: Diffs From 'sfiles %s' Under %s"
			   sfiles-args
			   (bk-abbrev-file-name dir)))
    (shell-command (format "cd %s && %s sfiles %s | %s diffs %s -"
			   (expand-file-name dir)
			   bk-binary
			   sfiles-args
			   bk-binary
			   bk-diffs-args)
		   t)
    (diff-mode)
    (font-lock-fontify-buffer)))

(defun bk-interesting-diffs ()
  "Run 'bk diffs' recursively under the repo root, on interesting files.
An 'interesting' file is one that is locked and modified, or has been
checked in but not yet committed to a ChangeSet.

If DIR is nil, use the root of the current BK repository.  Called
interactively, prompt for DIR."
  (interactive)
  (bk-view-output ((concat "BK: Interesting Diffs Under "
			   (bk-abbrev-file-name (bk-root))))
    (call-process bk-binary nil t nil "diffs" "-C-1" bk-diffs-args)
    (diff-mode)
    (font-lock-fontify-buffer)))

(defun bk-buffer-commands (pnt)
  "Use the properties of a character to do something sensible."
  (interactive "d")
  (let ((rev (get-char-property pnt 'rev))
	(file (get-char-property pnt 'file))
	(date (get-char-property pnt 'date))
	(user (get-char-property pnt 'user))
	(host (get-char-property pnt 'host))
	(help (get-char-property pnt 'help))
	(prev-buf (current-buffer)))
    (cond
     (file
      (find-file-other-window file))
     (rev
      (bk-diffs bk-view-file-name rev rev prev-buf))
     (help
      (bk-help help))
     ((message "I don't know how to do that yet")))))

(defun bk-buffer-mouse-clicked (event)
  "Translate the mouse clicks in a BK log buffer to character events.
These are then handed off to `bk-buffer-commands'.

Handle frickin' frackin' gratuitous event-related incompatibilities."
  (interactive "e")
  (if bk-running-xemacs
      (progn
	(select-window (event-window event))
	(bk-buffer-commands (event-point event)))
    (select-window (posn-window (event-end event)))
    (bk-buffer-commands (posn-point (event-start event)))))

(defun bk-set-extent-property (start end property value)
  "Set a property on an extent or overlay, depending on your religion."
  (if bk-running-xemacs
      (set-extent-property (make-extent start end)
			   property value)
    (overlay-put (make-overlay start end)
		 property value)))

(unless (fboundp 'view-minor-mode)
  (defun view-minor-mode (prev-buffer exit-func)
    (view-mode)))

(defun bk-abbrev-file-name (file)
  (if bk-running-xemacs
      (abbreviate-file-name file t)
    (abbreviate-file-name file)))

(defun bk-exit-view-mode (buf)
  "Exit from bk-view-mode.
We delete the current window if entering bk-view-mode split the
current frame."
  (when (and (eq buf (current-buffer))
	     (> (length (window-list)) 1))
    (delete-window))
  (kill-buffer buf))

(defun bk-view-mode (prev-buffer &optional file-name)
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (toggle-read-only t)
  (view-minor-mode prev-buffer 'bk-exit-view-mode)
  (use-local-map bk-view-mode-map)
  (setq truncate-lines t)
  (when file-name
    (set (make-local-variable 'bk-view-file-name)
	 (bk-abbrev-file-name file-name))))
  
(defun bk-create-magic-link (start end prop-list)
  (bk-set-extent-property start end 'face 'bold)
  (bk-set-extent-property start end 'mouse-face 'highlight)
  (while prop-list
    (bk-set-extent-property start end (caar prop-list) (cdar prop-list))
    (setq prop-list (cdr prop-list))))

(defun bk-print-revision-summary (&optional file prs-options)
  "Print the revision summary of FILE in a `view-mode' buffer.
The displayed summary will have certain headers highlighted.  If
you're using a windowing environment, you can middle-click on a header
to see what happens."
  (interactive)
  (unless file
    (setq file (if current-prefix-arg
		   (read-file-name "Print revision summary: " nil nil t
				   bk-file-name-history)
		 buffer-file-name)))
  (bk-view-output ((concat "BK Revisions: " (bk-abbrev-file-name file))
		   file)
    (apply 'call-process bk-binary nil t nil "prs" "-h" (expand-file-name file)
	   prs-options)
    (goto-char (point-min))
    (while (< (point) (1- (point-max)))
      (let ((prefix (intern (buffer-substring (point) (1+ (point))))))
	(case prefix
	  ('D
	   (delete-char 2)
	   (when (looking-at (concat "^\\([0-9.]+\\) "
				     "\\([0-9/]+\\) "
				     "\\([0-9:-]+\\) "
				     "\\([^ @]+\\)@\\([^ ]+\\) "
				     "\\([0-9]+ [0-9]+\\) "
				     "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)"))
	     (let ((rev-match 1)
		   (date-match 2)
		   (time-match 3)
		   (user-match 4)
		   (host-match 5)
		   (incr-match 6)
		   (ins-match 7)
		   (del-match 8)
		   (unch-match 9))
	       (bk-create-magic-link (match-beginning rev-match)
				     (match-end rev-match)
				     `((rev . ,(match-string rev-match))))
	       (bk-create-magic-link (match-beginning date-match)
				     (match-end date-match)
				     `((date . ,(match-string date-match))))
	       (bk-create-magic-link (match-beginning user-match)
				     (match-end user-match)
				     `((user . ,(match-string user-match))))
	       (bk-create-magic-link (match-beginning host-match)
				     (match-end host-match)
				     `((host . ,(match-string host-match))))
	       (goto-char (1- (match-beginning unch-match)))
	       (kill-line)
	       (goto-char (match-end del-match))
	       (insert "-")
	       (goto-char (match-end ins-match))
	       (insert "+")
	       (goto-char (match-beginning incr-match))
	       (delete-char (- (1+ (match-end incr-match))
			       (match-beginning incr-match))))))
	  ('P
	   (kill-line))
	  ('C
	   (delete-char 2)
	   (insert "    "))
	  ('-
	   (kill-line))
	  (t
	   (message "?: %s" prefix)
	   (sit-for 1))))
      (forward-line))))

(defvar bk-help-history nil)

(defvar bk-help-completions nil
  "Help topic completions for feeding to completing-read.
Constructed lazily.")

(defun bk-help-completions ()
  "Return help topic completions for feeding to completing-read."
  (unless bk-help-completions
    (setq bk-help-completions
	  (mapcar
	   (function (lambda (x) (list x x)))
	   (cons "topics" (split-string (bk-chomp
					 (shell-command-to-string
					  (concat bk-binary " "
						  "help topics | "
						  "awk '/^  bk/{print $2}'")))
					"\n")))))
  bk-help-completions)
  
(unless (fboundp 'Manual-mode)
  (defalias 'Manual-mode 'Man-mode))

(defun bk-help (topic)
  "Display BitKeeper's online help documentation for TOPIC.
If you are calling this command interactively, note that tab
completion is available for topic names.

Cross-references to other BitKeeper help entries are highlighted\; you
can middle-click on them for more information."
  (interactive
   (list (completing-read "BK help topic (hit `enter' for more help): "
			  (bk-help-completions)
			  nil
			  t
			  nil
			  'bk-help-history
			  "All")))
  (bk-view-output ((concat "BK Help: " topic))
    (call-process bk-binary nil t nil "help" topic)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (re-search-forward "^\\s-*\\([A-Z][A-Z0-9 ]+\\)$" nil t)
	(bk-set-extent-property (match-beginning 1) (match-end 1)
				'face 'man-heading))
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\s-*\\(bk help\\s-+\\)\\([A-Za-z0-9_-]+\\)" nil t)
	(bk-create-magic-link (match-beginning 1)
			      (match-end 2)
			      `((help . ,(match-string 2)))))
      (when (member topic '("All" "topics"))
	(goto-char (point-min))
	(while (re-search-forward
		"^\\s-*\\(bk \\)\\([A-Za-z0-9_-]+\\) -" nil t)
	  (bk-create-magic-link (match-beginning 1)
				(match-end 2)
				`((help . ,(match-string 2)))))))))

(defun bk-apropos (term)
  "Display fragments of BitKeeper help files that contain TERM.
Middle-click on the first column of output to view a help entry."
  (interactive "sBK apropos: ")
  (bk-view-output ((concat "BK Apropos: " term))
    (call-process bk-binary nil t nil "help" "-kp" term)
    (goto-char (point-min))
    (while (re-search-forward "^\\([A-Za-z0-9_-]+\\)" nil t)
      (bk-create-magic-link (match-beginning 1)
			    (match-end 1)
			    `((help . ,(match-string 1)))))))
  
(defun bk-annotate (&optional file)
  "Display an annotated version of FILE."
  (interactive)
  (unless file
    (setq file
	  (if current-prefix-arg
	      (read-file-name "Annotate file: " nil nil t nil 
			      'bk-file-name-history)
	    buffer-file-name)))
  (bk-view-output ((concat "BK Annotated: " file))
    (call-process bk-binary nil t nil "get" "-p" "-dum"
		  (expand-file-name file))
    (goto-char (point-min))
    (while (re-search-forward (concat "^\\([0-9/]+\\)\\s-+"
				      "\\([a-zA-Z0-9_-]+\\)\\s-+"
				      "\\([0-9.]+\\)")
			      nil t)
      (bk-create-magic-link (match-beginning 1)
			    (match-end 1)
			    `((date . ,(match-string 1))))
      (bk-create-magic-link (match-beginning 2)
			    (match-end 2)
			    `((user . ,(match-string 2))))
      (bk-create-magic-link (match-beginning 3)
			    (match-end 3)
			    `((rev . ,(match-string 3)))))))

(defun bk-help-overview ()
  "This is an overview of the BitKeeper integration mode for Emacs.
You are using the following version: $Revision: 1.57 $

You can find the source code, license (GPL v2), and credits for this
code by typing `M-x find-library bk RET'.

The BK integration mode is based on VC, so if you're already familiar
with VC, the same keybindings and functions will generally work.

When you are editing a file managed by BK, the modeline will show
`BKL' if the file is in a repository licensed under the BKL, or `BKCL'
for the BKCL.  Type `M-x bk-help licensing RET' for information on why
this is important.

There are two key prefixes that let you access bk commands more
efficiently.  `C-x v' is the same as the VC key prefix, and typically
just overrides VC's per-file commands.  `C-c b' is the global key
prefix, and lets you run commands that have wider applicability than
to just a single file.

Task				Key Binding	Command Name
----				-----------	------------
Help overview (i.e. this)	C-c b h		bk-help-overview (*)
View a BitKeeper manual entry	C-c b m		bk-help (*)
Search BitKeeper docs		C-c b a		bk-apropos (*)

Register file in a BK repo	C-x v i		bk-register (*)
Check file in / out		C-x C-q		vc-toggle-read-only
Revert file to last checkin	C-x v u		vc-revert-buffer

Check files in / create cset	C-c b c		bk-citool (*)
Push outgoing csets		C-c b .		bk-push (*)
Pull incoming csets		C-c b ,		bk-pull (*)

View diffs vs last checkin	C-x v =		bk-diffs
View current diffs in repo	C-c b =		bk-recursive-diffs
View interesting diffs in repo	C-c b i		bk-interesting-diffs
View revision summary		C-x v l		bk-print-revision-summary
Revision history browser	C-c b r		bk-revtool (*)
View annotated file		C-x v g		bk-annotate
View specific revision		C-x v ~		bk-print-rev
List interesting files		C-c b s		bk-sfiles (*)

In the task list above, entries suffixed with an asterisk (`*') are
accessible globally via key bindings.  Others are only accessible via
key bindings when editing files managed by BitKeeper."
  (interactive)
  (bk-view-output ("BK Help Overview")
    (insert (documentation 'bk-help-overview))))

(defun bk-resync-buffers ()
  "Check bk-mode buffers for files that may have changed under them.
For those buffers that have had files changed, revisit the files."
  (save-excursion
    (message "Resyncing bk-mode buffers...")
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
		 (buffer-file-name buf))
	(set-buffer buf)
	(when bk-mode
	  (when (or (eq (file-writable-p buffer-file-name)
			buffer-read-only)
		    (and (file-readable-p buffer-file-name)
			 (not (buffer-modified-p buf))
			 (not (verify-visited-file-modtime buf))))
	    (set-buffer buf)
	    (find-alternate-file buffer-file-name))))))
  (message "Resyncing bk-mode buffers...done"))
    
(defun bk-process-sentinel (process event)
  (save-excursion
    (let ((buf (process-buffer process)))
      (set-buffer buf)
      (message "BitKeeper: `bk %s' %s"
	       (process-name process)
	       (substring event 0 (1- (length event))))
      (goto-char (point-min))
      (while (re-search-forward "\r\n?" nil t)
	(replace-match "\n" nil t))
      (goto-char (point-min))
      (while (search-forward "\b" nil t)
	(delete-backward-char 2))
      (let ((prev-buf (current-buffer))
	    (resync (boundp 'bk-resync)))
	(if (buffer-modified-p buf)
	    (progn
	      (pop-to-buffer buf)
	      (bk-view-mode prev-buf))
	  (kill-buffer buf))
	(when resync
	  (run-with-idle-timer 5 nil 'bk-resync-buffers))))))

(defun bk-run-async (cmdline process-name &optional resync)
  "Run CMDLINE asynchronously, with the async process named PROCESS-NAME.
Optional RESYNC indicates that bk-mode buffers should be checked after
the process exits, in case their files may have changed under them."
  (let ((buf-name (concat "BK Output: bk " process-name)))
    (message "Running `bk %s' in the background" process-name)
    (get-buffer-create buf-name)
    (kill-buffer buf-name)
    (let ((proc (start-process-shell-command process-name buf-name cmdline)))
      (set-process-sentinel proc 'bk-process-sentinel)
      (when resync
	(save-excursion
	  (set-buffer (process-buffer proc))
	  (set (make-local-variable 'bk-resync) nil))))))

(defun bk-citool ()
  "Run BitKeeper's graphical file checkin and ChangeSet commit tool."
  (interactive)
  (save-some-buffers)
  (bk-run-async (concat bk-binary " citool")
		"citool"
		t))
  
(defun bk-helptool ()
  "Run BitKeeper's graphical help browser."
  (interactive)
  (bk-run-async (concat bk-binary " helptool") "helptool"))

(defun bk-run-async-with-options (cmd &optional options history resync)
  "Run a BitKeeper program asynchronously.
If OPTIONS is a string, pass it to CMD.  If OPTIONS is not a string
and is not nil, prompt for an option string.  HISTORY is the variable
to use for option history."
  (when (and options (not (stringp options)))
    (setq options (read-string (format "Options for %s: " cmd) nil history)))
  (let ((opts (if options (concat " " options) "")))
    (bk-run-async (concat bk-binary " " cmd opts)
		  (concat cmd opts)
		  resync)))
  
(defvar bk-revtool-history nil)

(defun bk-revtool (&optional options)
  "Run BitKeeper's graphical revision history browser."
  (interactive)
  (bk-run-async-with-options "revtool"
			     (or options
				 (and (interactive-p) current-prefix-arg))
			     'bk-revtool-history))

(defvar bk-csettool-history nil)

(defun bk-csettool (&optional options)
  "Run BitKeeper's graphical ChangeSet history browser."
  (interactive)
  (bk-run-async-with-options "csettool"
			     (or options
				 (and (interactive-p) current-prefix-arg))
			     'bk-csettool-history))

(defvar bk-difftool-history nil)

(defun bk-difftool (&optional options)
  "Run BitKeeper's graphical file change browser."
  (interactive)
  (bk-run-async-with-options "difftool"
			     (or options
				 (and (interactive-p) current-prefix-arg))
			     'bk-difftool-history))

;;; XXX The definitions of push and pull are complete crap, and need
;;; to be able to handle resolves and so on.  Still useful as
;;; placeholders.

(defvar bk-push-history nil)

(defun bk-push (&optional options)
  "Push pending ChangeSets to the parent of this repository."
  (interactive)
  (unless (bk-parent)
    (error "No parent repository to push to!"))
  (bk-run-async-with-options "push"
			     (or options
				 (and (interactive-p) current-prefix-arg))
			     'bk-push-history
			     t))

(defvar bk-pull-history nil)

(defun bk-pull (&optional options)
  "Pull pending ChangeSets from the parent of this repository."
  (interactive)
  (unless (bk-parent)
    (error "No parent repository to pull from!"))
  (bk-run-async-with-options "pull"
			     (or options
				 (and (interactive-p) current-prefix-arg))
			     'bk-pull-history
			     t))


(defun bk-sfiles (&optional dir args)
  "Print information about the sfiles in DIR, passing ARGS to 'bk sfiles'.
By default, DIR is the root of the current repository, and ARGS is
whatever the value of 'bk-sfiles-args' is.

Called interactively with a prefix, this function prompts for the
arguments to pass to 'bk sfiles'."
  (interactive)
  (unless dir
    (setq dir (bk-root)))
  (unless args
    (setq args (if (and current-prefix-arg (interactive-p))
		   (read-string "Arguments to sfiles: "
				bk-sfiles-args bk-sfiles-history)
		 bk-sfiles-args)))
  (bk-view-output ((format "BK: 'sfiles %s' in %s"
			   args
			   (bk-abbrev-file-name dir)))
    (shell-command (format "cd %s && %s sfiles %s" dir bk-binary args) t)
    (goto-char (point-min))
    (while (re-search-forward (concat "^\\([lujx ][jxc ][jxp ][jx ] \\)?"
				      "\\([^\n]+\\)$") nil t)
      (bk-create-magic-link (match-beginning 2)
			    (match-end 2)
			    `((file . ,(concat dir "/" (match-string 2))))))))
  
(provide 'bk)


;;; Local Variables:
;;; mode: emacs-lisp
;;; prompt-to-byte-compile: nil
;;; end:
;;; bk.el is free software
;;; bk.el ends here
