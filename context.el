;;; context.el
;;; context.el is free software
;; LCD Archive Entry:
;; context|David Neves|davidneves@hotmail.com|
;; Save some context from previous editing sessions.|
;; 92-11-16||~/misc/context.el.Z|
;;
;;; File: context.el
;;; Date: Mar '05
;;; First version: 1987
;;;
;;; Author: David Neves, davidneves@hotmail.com
;;;
;;; Documentation:
;;; Restore some context from previous editing sessions.
;;; The location of the cursor point is saved (in the file ~/.emacs_context).
;;; Thereafter, whenever a file is read into a buffer you will find 
;;; yourself back at the point where you left off.
;;;
;;; In addition to restoring context of files manually read in there are
;;; three optional user commands:
;;;
;;; 1.  "Meta-x cdired" or "Meta-x context-dired"  It runs DIRED on your
;;; list of context files so that you can easily jump to a file that you
;;; have edited recently.  By default, context for the last 50 files visited is saved.
;;; You can set the default to something else -- see below for context-max-size.
;;; I use this ALL the time.
;;;
;;; 2.  "Meta-x context-restore".  
;;; This restores the context of all files edited (within a 24 hour window).
;;; For example, if during the day you started Emacs 5 times and read in 5 different
;;; files, when you next execute Emacs and do a context-restore all 5 files would be read into Emacs.
;;; If you place "(context-restore)" in your .emacs file then these files will automatically
;;; be read in when emacs starts up.  See below for installation instructions for this.
;;; I almost never use this except when I'm in the middle of editing a lot of files and have to 
;;; reboot or close down my system.  I use cdired above instead.
;;;
;;; 3.  "Meta-x context-filter"
;;; This restores the context of all files filtered by a string you type in.
;;; If you type in a directory (or part of a directory) then all your context files contained
;;; within that directory (or a subdirectory of it) will be read in.
;;; e.g. typing "foodir" as an argument will read in all context files contained within
;;;     ~/foodir.  typing "/" is a way of reading in all your context files.
;;; I've never used this functionality.
;;;
;;; The context is automatically saved when the user exits Emacs.  
;;; The user can also save the context while in Emacs by typing
;;; Meta-x save-context
;;;
;;; see saveplace.el in the emacs distribution for a similar program.
;;;
;;; Easy install:
;;;  1.  Put this file in a directory in load-path
;;;      (or use an absolute pathname for the load in step 2 below,
;;;       e.g., 
;;;      (load "~/context.el") ;or (load "c:/context.el") for Windows
;;;  2.  Put the following 2 lisp expressions in your .emacs file
;;;      (load "context.el")
;;;      (read-context)    ;reads the file context from the context file.
;;;      (context-restore) ;this is optional. It will read in files from the last
;;;                        ;emacs session(s) saved within 24 hours.
;;; Optional installation steps with compilation:
;;;  1.  Byte compile this file (context.el) with
;;;      Meta-x byte-compile-file
;;;  2.  Put the compiled file (context.elc) in a directory in load-path.
;;;      (or use an absolute pathname for the load in step 3 below,
;;;       e.g. (load "~/context.elc")    )
;;;  3.  Put the following 2 lisp expressions in your .emacs file
;;;      (load "context.elc") 
;;;      (read-context)    ;reads the context from the context file.
;;;      (context-restore) ;this is optional. It will read in files from the last
;;;                        ;emacs session(s) saved within 24 hours.
;;;
;;; Changes:
;;; Mar '05 -  Take out expand-file-name in context-filter
;;; Oct '04 -  Change cdired so that files that don't exist or are not currently reachable (i.e.,
;;;            in a shared drive) no longer beep when read into dired.
;;; Oct '03 -  Add def of float-time for earlier version Emacs
;;; Sep '03 -  Gosh, 10 years without changes!
;;;            Rename user commands.
;;;            Add "context-restore" which loads all the files within 24 hours
;;;            Change xxx and rename to "context-filter" to take as an option some
;;;            part of a file or directory name to use as a filter
;;;            to read in.
;;;            FINALLY handle gracefully the cases where one has multiple emacs
;;;            running.  It used to be the case that only context for the last
;;;            emacs exited was kept.  Now all contexts are appended and kept.
;;;            Remove code for version 18 of Emacs and other misc changes.
;;; Oct '93 -  Get CDired to work in version 19.  Take out call to
;;;            reverse in version 19 of cdired.  Take out repeated code
;;;            that somehow snuck in.  Document and fix context-restore.
;;; Oct '93 -  Adapted to Emacs 19 by Johan Vromans - jv@mh.nl (Thanks Johan)
;;;            This file should work in Emacs versions 18 & 19.
;;; Oct '92    Cdired Flags to tcsh are now -fc rather than -f-c
;;; ===
;;; 1988
;;; Mon May  8 handle the tcsh shell. don't read in its .cshrc file.
;;; Fri Sep 23 quote file names for ls in cdired in case they have 
;;;            shell sensitive characters in them.
;;; Thu Sep 15 move RMAIL file check to a check for rmail-mode.
;;;            Context is saved even for files whose point is at
;;;            the beginning so they can be viewed by cdired.
;;; Wed Sep 7  Sys V shells don't like ; to start lines.  Fixed.
;;;            Other misc changes made.
;;; Sun Sep 4  Installed cdired command to run DIRED on context files.
;;; 1987
;;; Wed Nov 25 Don't save context of files/directories in context-ignore-files
;;; Tue Nov 24 reverting a buffer now turns off context
;;;
 
;;; USER settable globals
(defconst context-file "~/.emacs_context" "*File for Emacs context")

(defvar context-max-size 50 ;why 50?  why not?
  "*Maximum number of files that context is saved for.
If not a number (e.g. nil) then the number of files is allowed to
grow arbitrarily large.  This will result in slower performance because
the context-alist is searched linearly.")

;;; If you set this to nil why are you using this software???
(defvar context-flag t
  "*If non-nil the `save-context' command will always be run before Emacs is
exited and context will be applied to files that are read in.  In other words,
you can turn off all context processing by setting this flag to nil.")

(defvar context-ignore-files
  (list "/tmp")  ;use "list" so one can evaluate expressions
  "*List of files and/or directories to ignore for context processing")

;;; currently set to 24 hours.  Set it to something small if you only want the last session restored.
;;; Window is in seconds, hence the computation of hours * minutes * seconds
(defvar context-restore-window (* 24 60 60) "context restore will restore files saved within this range")

;;; Non user settable globals
(defvar context-alist nil "Association list holding some file context.
  The structure is ( (file-name1 point timesaved) (file-name2 point timesaved) ...)")

(defvar context-file-date nil) ;time when context file was last written

(defmacro context-second (l)  `(car (cdr ,l)))
(defmacro context-get-filename (l) `(car ,l))
(defmacro context-get-point (l)  `(car (cdr ,l)))
(defmacro context-get-time (l) `(car (cddr ,l)))
(defmacro context-make-tuple (file point time) `(list ,file ,point ,time))

(defun context-emacs-major-version ()
  "Return major version of Emacs, e.g. 18 or 19"
  (car (read-from-string (substring emacs-version 0 2))))

(or (functionp #'dolist)
    (require 'cl))

;;; Call get-context when a file is loaded into a buffer.
;;; Should only add get-context to file-file-hooks if it isn't there.
;;;  (just in case this file is loaded more than once.)
(if (not (memq 'get-context find-file-hooks))
    (setq find-file-hooks (cons 'get-context find-file-hooks)))

;;; Turn off context processing when reverting a buffer so you stay
;;; at the current point rather than being sent to the context point.
(if (null revert-buffer-function)
    (setq revert-buffer-function 
	  (function (lambda (&optional arg noconfirm) 
		      (let ((context-flag nil)
			    (revert-buffer-function nil))
			(revert-buffer arg noconfirm))))))

(defun read-context ()
   "Read in an Emacs context.  Usually done when Emacs is initially called.
    This function should be called in .emacs ."
   (interactive)
      (if (not (file-exists-p context-file)) (progn (setq context-alist nil) (save-context))
	(load context-file t t t))
      (setq context-file-date (context-file-write-time)))

(defun context-file-write-time ()
  "Write time of the .emacs-context file or nil if no such file"
  (and (file-exists-p context-file) (nth 5 (file-attributes context-file))))

;;; Apply the context that is saved for the current file.
;;; Called in find-file-hooks (i.e. when a file is loaded).
;;; Doesn't apply context if context-flag is nil.
(defun get-context nil
  (if context-flag
      (let* ((buf (current-buffer))
	     (file-name (buffer-file-name buf))
	     file-data)
	(if (null file-name) nil
	  (setq file-data (assoc file-name context-alist))
	  (if (null file-data) nil
	    (goto-char (context-get-point file-data)))))))

(defun save-context-maybe ()
  "Save context if context-flag is not nil."
   (if context-flag
       (save-context)))

(defun save-context ()
  "Save context (currently, the point) of all Emacs buffers.
The context information goes into a file whose name is stored 
in the variable 'context-file')."
  (interactive)
  (when (and context-file-date (not (equal context-file-date (context-file-write-time))))
    (let ((old-alist context-alist))
      (read-context)
      (setq context-alist (reverse context-alist))
      (dolist (item context-alist)
	(if (not (assoc (context-get-filename item) old-alist))
	    (push item old-alist)))
      (setq context-alist old-alist)))
  (save-excursion
    (let ((time (float-time)))
      (dolist (buf (reverse (buffer-list))) (read-buffer-context buf time)))
    (let ((buf (get-buffer-create "*context*"))
	  nth-part)
      (cond ((numberp context-max-size)
	     (setq nth-part (nthcdr (1- context-max-size) context-alist))
	     (if nth-part (rplacd nth-part nil))));reduce size of context-alist
      (set-buffer buf)
      (erase-buffer)
      (insert "(setq context-alist '(")
      (mapcar (function (lambda (l) 
			  ;; print function in 18.4x outputs 2 newlines
			  ;; so use terpri and prin1 instead
			  (terpri buf)
			  (prin1 l buf))) context-alist)
      (insert "))")
      (if (file-exists-p context-file) (delete-file context-file))
      (write-region 1 (point-max) context-file nil 'nomessage)
      (kill-buffer buf))))

;;; place buffer context in the list "context-alist".
;;; If it already exists in that list then also move that
;;; information to the front of the alist.
(defun read-buffer-context (buf time)
  (let ((file-name (buffer-file-name buf))
	buffer-data
	before
	pointloc)
    (set-buffer buf)
    (setq pointloc (point))
    (when (not (or (null file-name) 
		   ;; rmail assumes point is at position 1 when RMAIL
		   ;; file is read in.
		   (eq major-mode 'rmail-mode)  ;thanks Graham
		   (context-ignore-file file-name)))
      (setq before (context-assoc file-name context-alist))
      (if (null before) 
	  (if (equal file-name (context-get-filename (car context-alist)))
	      (setq context-alist (cdr context-alist)))
	(rplacd before (cdr (cdr before)))) ; splice it out
      (setq buffer-data (context-make-tuple file-name pointloc time)) 
      (push buffer-data context-alist))))

(if (>= (context-emacs-major-version) 19)
    (add-hook 'kill-emacs-hook 'save-context-maybe)
  ;; change kill-emacs so that context will be saved out when you leave emacs.
  (if (not (fboundp 'original-kill-emacs))
      (fset 'original-kill-emacs (symbol-function 'kill-emacs)))

  (defun kill-emacs (&optional query)
    "End this Emacs session.
Prefix ARG or optional first ARG non-nil means exit with no questions asked,
even if there are unsaved buffers.  If Emacs is running non-interactively
and ARG is an integer, then Emacs exits with ARG as its exit code.

If the variable `context-flag' is non-nil,
the function save-context will be called first."
  (interactive "P")
  (save-context-maybe)
  (original-kill-emacs query)))

;;; returns true if no context should be saved out for filename
(defun context-ignore-file (filename)
  (let ((ignore-list context-ignore-files)
        (answer nil))
   (while (and ignore-list (null answer))
     (if (context-match (car ignore-list) filename) (setq answer t)
       (setq ignore-list (cdr ignore-list))))
   answer))

;;; version of assoc that part of list before item found so it can be spliced out.
;;; e.g. (context-assoc 'foo '((a b) (c d) (foo bar) (e f)))
;;;       --> ((c d) (foo bar) (e f))
(defun context-assoc (key alist)
  (let ((before nil) (current alist))
    (while (and current (not (equal key (car (car current)))))
      (setq before current)
      (setq current (cdr current)))
    before))

;;; is str1 at the front of str2?
(defun context-match (str1 str2)
  (let ((result (string-match str1 str2)))
    (and (numberp result) (zerop result))))

;;; return the first n elements in l
(defun context-first-n (l n)
  (let (nl)
    (while (not (zerop n))
      (setq nl (cons (car l) nl))
      (setq l (cdr l))
      (setq n (1- n)))
    nl))

(defun context-restore ()
  "Restore files saved within a 24 hour window."
  (interactive)
  (let (time timewindow gettime filename)
    (setq time (context-get-time (car context-alist)))  ; get latest time save
    (when time
      (setq timewindow (- time context-restore-window))
      (dolist (item context-alist)
	(setq gettime (context-get-time item)) 
	(setq filename (context-get-filename item))
	;;first part of AND is probably not needed except for backward compatibility
	(when (and gettime (> gettime timewindow) (file-exists-p filename))
	    (find-file filename)
	    (message filename)
	    )))))

(defun context-filter (arg)
  "Restore files filtered by a directory name or file name, or part of name"
  (interactive "s Type directory or file name to use as a filter: ")
  (let ((calist context-alist)
	filename)
;    (setq arg (expand-file-name arg))
    (setq calist (reverse calist))
    (message arg)
    (while calist
      (setq filename (car (car calist)))
      (and (string-match arg filename)
	   (file-exists-p filename)
	   (message filename) (find-file filename))
      (setq calist (cdr calist)))))
    
;; for Emacs version 19 and afterwards.
(defun cdired nil 
  "Apply DIRED to files for which some state was saved."
  (interactive)
  (let* ( (homedirectory (expand-file-name "~/"))
	  (homelength (length homedirectory))
	  filelist newcontextalist)
    ;; only look at currently reachable files
    (dolist (filepair context-alist)
      (when (file-exists-p (context-get-filename filepair))
	(push filepair newcontextalist)))
    (setq newcontextalist (reverse newcontextalist))
    (setq filelist
	  (mapcar (function (lambda (filepair) 
			      (context-strip-homedir (context-get-filename filepair) homedirectory homelength)))
		  newcontextalist))
    (dired (cons homedirectory filelist))))

(defun context-dired nil
  "Apply DIRED to files for which some state was saved."
  (interactive)
  (cdired))

;;; strip off home directory from start of filenames.
(defun context-strip-homedir (filename homedirectory homelength)
  (if (context-match homedirectory filename) (substring filename homelength)
    filename))

;;; float-time from 1998 Sam Steingold posting
(or (functionp #'float-time)
    (defun float-time (&optional tm)
      "Convert `current-time` to a float number of seconds."
      (setq tm (or tm (current-time)))
      (+ (float (cadr tm)) (* (float (ash 1 16)) (car tm))))
)

(provide 'context)
;;; context.el ends here
