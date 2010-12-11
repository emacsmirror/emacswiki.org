;;; trashcan.el --- A recoverable file deletion system

;; Copyright (C) 2006-2007 Davin Pearson

;; Author/Maintainer: Davin Pearson http://www.davinpearson.com
;; Keywords: Amiga MacOS Trash Can Windows Recycle Bin
;; Version: 1.0

;; NOTE: this file contains a slight alteration to the `trashcan--is-a-windows-system'
;; function from the original. I couldn't find the contact details for the original author
;; (the weblink is dead), but I'm posting to emacswiki anyway for the benefit of others.
;; Joe Bloggs (<vapniks@yahoo.com>).

;;; Limitation of Warranty

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; The file trashcan.el contains a recoverable file deletion system
;; that behaves like the trash can or recycle bin that many operating
;; systems, present and past, show on their screens. This system
;; creates special directories known as trashcan directories to hold
;; files that can be deleted (permanently) or undeleted (restored /
;; recovered). On Unix systems there is one trashcan directory for
;; each user and the default values are $HOME/.TRASHCAN for each value
;; of $HOME. On Windows systems there are trashcan directories at the
;; following default locations: a:\TRASHCAN, b:\TRASHCAN, c:\TRASHCAN
;; etc.

;; This system changes the behaviour of the "x" key in dired mode from
;; permanently deleting files to a two stage system. If you are not in
;; a trashcan directory, then the selected files are moved into a
;; trashcan directory. If you are already in a trashcan directory, the
;; the selected files are permanently deleted. Files in a trashcan
;; directory can be restored by viewing that directory in dired mode,
;; selecting some files and executing the command M-x
;; trashcan-restore. All of the files in a trashcan directory can also
;; be permanently deleted in one hit by issuing the command M-x
;; trashcan-empty. The name "trashcan" comes from my old Amiga
;; Computer which I still have fond memories of!

;;; Install Instructions:

;; Drag the mouse over the text of this document, taking care not to
;; select the navigation menus at the top and bottom of this page.
;; Load an empty file trashcan.el into Emacs and paste the contents
;; into that buffer, then save it.  To use, put this file somewhere
;; in load-path and load it by putting the following command
;; in your .emacs file:

;; (require 'trashcan)

;;; More Info:

;; See the following URL for the latest info:

;; http: //www.geocities.com/davinpearson/research/2006/mopa2e.html#trashcan

;;; Known Bugs:

;;  (1) Doesn't respect make-auto-save-file-name

;;  (2) Doesn't preserve the marked files (*) in dired buffers when files as
;;      moved in or out of a trashcan directory

;;  (3) Windows detection function trashcan--is-a-windows-system could be improved
;;  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;  NOTE: I (Joe Bloggs - vapniks@yahoo.com) have now changed this function to use
;;        the `system-type' variable since I was having problems with the original function.
;;        I could not find the contact details of the original author (the weblink is dead)
;;        but I am posting this on emacswiki anyway for the benefit of others.

;;  (4) The name of this file trashcan.el might conflict with other Lisp files

;;  (5) Richard Stallman told me that code that is to be distributed with Emacs
;;      should not use the defadvice feature.   I leave the task of removing
;;      all calls to defadvice to someone else.

;;; Code:

(defun trashcan--is-a-windows-system ()
  (memq system-type '(windows-nt ms-dos)))

;; If above function doesn't work try using the one below instead.
;; (defun trashcan--is-a-windows-system ()
;;   (file-exists-p "c:/"))

(defun trashcan--is-a-unix-system ()
  (not (trashcan--is-a-windows-system)))

(defvar trashcan-dirname (if (trashcan--is-a-windows-system) "TRASHCAN" ".Trash")

  "This variable specifies what directory to move files into with the
\"x\" key in dired mode.  Do not add any prefix to the directory such
as \"~/\" or \"/\".

If this is a Windows system, the trashcan directories are located at
the following regexp:

       (concat \"^[a-zA-Z]:/\" (regexp-quote trashcan-dirname))

If this is a Unix system, there is one trashcan directory for
each user and ares located at the following places:

		  (concat \"~/\" trashcan-dirname)

In Windows, DO NOT give this the same name as the windows RECYCLER
directory as this will confuse the hell out of Windows.

")

(defvar trashcan-patch-delete-stuff-p t
  "This variable if set causes the functions delete-file and
delete-directory to be patched to use the trashcan directories
instead of deleting files permenantely."
)

;;; (trashcan--split (setq file "d:/home/mylisp/trashcan.el"))
;;; (trashcan--split (setq file "/home/mylisp/trashcan.el"))

(defun trashcan--split (file)
  ;;
  ;; NOTE: this function gives meaningful results for both WINDOWS and UNIX
  ;;
  (setq file (expand-file-name file))
  (if (string-match "^[a-zA-Z]:/" file)
      (cons (substring file 0 3) (substring file 3))
    (cons (expand-file-name "~/") (substring file 1))
    ))

;;; (trashcan--encode (setq file "/home/foomatic.txt"))
;;; (trashcan--encode (setq file "d:/home/foomatic.txt"))
;;; (trashcan--encode (setq file "d:/home/mylisp"))
;;; (trashcan--encode (setq file "/home/mylisp/trashcan.el"))     "d:/home/TRASHCAN/home!mylisp!trashcan.el"
;;; (trashcan--encode (setq file "d:/home/mylisp/trashcan.el"))   "d:/TRASHCAN/home!mylisp!trashcan.el"
(defun trashcan--encode (file)
  ;;(debug)

  (let* ((s (trashcan--split file))
	 (d (car s))
	 (f (cdr s)))

    ;;(debug)
    (let ((i 0))
      (while (< i (length f))
	(if (eq ?/ (aref f i))
	    (aset f i ?!))
	(incf i)))

    (let ((new (concat d trashcan-dirname "/" f)))
      (if (file-exists-p new)
	  (let ((count  1)
		(result nil))
	    (while (file-exists-p (setq result (concat new "." (format "%d" count))))
	      (incf count))
	    result)
	new))))

;;; (trashcan--split "/home/TRASHCAN/home!mylisp!trashcan.el")
;;; (trashcan--split "d:/TRASHCAN/home!mylisp!trashcan.el")
;;; (trashcan--decode (setq file "/home/TRASHCAN/home!mylisp!trashcan.el"))
;;; (trashcan--decode (setq file "d:/TRASHCAN/home!mylisp!trashcan.el"))

(defun trashcan--decode (file)

  (if (string-match (concat "^[a-zA-Z]:/" (regexp-quote trashcan-dirname)) file)
      ;;
      ;; NOTE: we are in DOS mode in this branch
      ;;
      (let ((d (substring file 0 3))
	    (f (substring file (+ 4 (length trashcan-dirname))))
	    (i 0))
	(while (< i (length f))
	  (if (eq ?! (aref f i))
	      (aset f i ?/))
	  (incf i))
	(concat d f))

    (progn
      ;;
      ;; NOTE: we are in UNIX mode in this branch
      ;;
      (assert (string-match (concat (expand-file-name "~/") (regexp-quote trashcan-dirname) "/\\(.*\\)$") file))
      (let ((x (substring file (match-beginning 1) (match-end 1)))
	    (i 0))
	(while (< i (length x))
	  (if (eq ?! (aref x i))
	      (aset x i ?/))
	  (incf i))
	(concat "/" x)))))

(defun trashcan--walk-buffers (sexp)
  ;;
  ;; NOTE: a long name is used here to guard against accidental aliasing
  ;;
  (save-window-excursion
    (let ((trashcan--walk-buffers--ptr (buffer-list)))
      (while trashcan--walk-buffers--ptr
	(set-buffer (car trashcan--walk-buffers--ptr))
	(eval sexp)
	(setq trashcan--walk-buffers--ptr (cdr trashcan--walk-buffers--ptr))))))

;;; (trashcan--delete-dangerous (setq file-or-directory "d:/TRASHCAN/workspace/"))
;;; (trashcan--delete-dangerous (setq file-or-directory "c:/TRASHCAN"))
(defun trashcan--delete-dangerous (file-or-directory)
  "Is better than the built-in function delete-file in that it also deletes directories,
therefore is more dangerous than delete-file"
  ;;
  ;; NOTE: cannot use delete-file here because that command calls this one (i.e. an infinite loop)
  ;;
  (if (file-exists-p file-or-directory)
      (shell-command (concat "rm -rvf \"" file-or-directory "\""))))

;;; (trashcan--in-windows-trashcan filename)
(defun trashcan--in-windows-trashcan (filename &optional OR-SUBDIR)
  "Returns the relevant windows trashcan directory or nil if there isn't one"
  (setq filename (expand-file-name filename))
  (let ((dirname (file-name-directory filename)))
    (if OR-SUBDIR
	(if (string-match (concat "^\\([a-zA-Z]:/" (regexp-quote trashcan-dirname) "\\)") dirname)
	    (substring dirname (match-beginning 1) (match-end 1)))
      (if (string-match (concat "^\\([a-zA-Z]:/" (regexp-quote trashcan-dirname) "\\)/?$") dirname)
	  (substring dirname (match-beginning 1) (match-end 1))))))

(defun trashcan--in-unix-trashcan (filename &optional OR-SUBDIR)
  "Returns the relevant unix trashcan directory or nil if there isn't one"
  (setq filename (expand-file-name filename))
  (let ((dirname (file-name-directory filename)))
    (if OR-SUBDIR
	(if (string-match (concat "^" (expand-file-name "~/") (regexp-quote trashcan-dirname)) dirname)
	    (concat (expand-file-name "~/") trashcan-dirname))
      (if (string-match (concat "^" (expand-file-name "~/") (regexp-quote trashcan-dirname) "/?$") dirname)
	  (concat (expand-file-name "~/") trashcan-dirname)))))

(defun trashcan--in-trashcan (filename &optional OR-SUBDIR)
  (or (trashcan--in-windows-trashcan filename OR-SUBDIR)
      (trashcan--in-unix-trashcan filename OR-SUBDIR)))

(defun trashcan--after-permanent-deletion ()
  ;;
  ;; NOTE: conditionally kills file buffers that have been deleted
  ;;
  ;; NOTE: unconditionally kills dired buffers that have been deleted
  ;;
  (let (dirname)
    (cond
     ((setq dirname (trashcan--in-windows-trashcan default-directory 'OR-SUBDIR)))
     ((setq dirname (trashcan--in-unix-trashcan    default-directory 'OR-SUBDIR)))
     (t
      (error "Should never happen")))

    (trashcan--walk-buffers
     '(if (or (and (buffer-file-name)
		   (string-match (concat "^" dirname) default-directory)
		   (y-or-n-p (concat "Kill buffer " (buffer-file-name) " too? ")))
	      (and (eq major-mode 'dired-mode) (not (file-exists-p default-directory))))
	  (kill-buffer nil)))))

(setq trashcan--global-refresh-count 1)

;;;
;;; FIXME: too slow for large amounts of files
;;;
(defun trashcan--rename-to-trash (file-list)

  (let ((dir nil))
    (let ((ptr file-list))
      (while ptr
	;;
	;; NOTE: Creates a trash directory if none exists, then renames the file to trash directory.
	;;
	(let* ((new-name (trashcan--encode (car ptr)))
	       (fnd      (file-name-directory new-name)))

	  ;;(debug)

	  (make-directory fnd 'PARENTS)
	  (setq dir fnd)
	  (rename-file (car ptr) new-name))
	(setq ptr (cdr ptr)))

      ;;(debug)

      (setq ptr file-list)

      (incf trashcan--global-refresh-count)

      (if (not (boundp 'trashcan--refresh-count))
	  (setq-default trashcan--refresh-count nil))

      (while ptr

	(trashcan--walk-buffers
	 '(progn
	    (make-local-variable 'trashcan--refresh-count)
	    (if (and (buffer-file-name)
		     (string-match (concat "^" (regexp-quote (car ptr))) (buffer-file-name))
		     (not (eq trashcan--global-refresh-count trashcan--refresh-count)))
		(set-visited-file-name (trashcan--encode (car ptr)) 'NO-QUERY))
	    (setq trashcan--refresh-count trashcan--global-refresh-count)))

	;;
	;; NOTE: reverts all direds of the original file
	;;
	(let ((dirname (file-name-directory (car ptr))))
	  (trashcan--walk-buffers
	   '(progn
	      (make-local-variable 'trashcan--refresh-count)
	      (if (and (eq major-mode 'dired-mode)
		       (string-match (concat "^" (regexp-quote dirname) "/?$") default-directory)
		       (not (eq trashcan--global-refresh-count trashcan--refresh-count)))
		  (revert-buffer))
	      (set (make-local-variable 'trashcan--refresh-count) trashcan--global-refresh-count))))

	(setq ptr (cdr ptr))))

    (if (trashcan--is-a-windows-system)
	(setq dir (downcase dir)))

    ;;
    ;; NOTE: deletes all dired buffers that have had their dirs deleted
    ;;
    (trashcan--walk-buffers
     '(if (and (eq major-mode 'dired-mode) (not (file-exists-p (expand-file-name default-directory))))
	  (kill-buffer nil)))

    ;;
    ;; NOTE: reverts trashcan buffers that have been changed
    ;;
    (trashcan--walk-buffers
     '(if (and (eq major-mode 'dired-mode) (string=
					    (if (trashcan--is-a-windows-system)
						(downcase default-directory)
					      default-directory) dir))
	  (revert-buffer)))))

;;(require 'dired)

;;;
;;; NOTE: This function advised takes two args (l arg)
;;;
(defadvice dired-internal-do-deletions (around trashcan-stub activate)

  ;;(beeps "Calling dired-internal-do-deletions")
;;(defun dired-internal-do-deletions (l arg)

  "This function replaces the function of the same name in the standard Emacs file dired.el"
  ;;(my-foo)

  (if (not (eq major-mode 'dired-mode))
      (error "You must be in dired mode to execute dired-internal-do-deletions"))

  ;;(debug)

  (let ((ptr l))
    (while ptr
      ;;(debug)
      (if (or (string-match "/\\./?$" (caar ptr)) (string-match "/\\.\\./?$" (caar ptr)))
	  (error "You cannot delete the directories . or .."))
      (setq ptr (cdr ptr))))

  (let ((ptr l))
     (while ptr
       (if (or (string-match (concat "^[a-zA-Z]:/" (regexp-quote trashcan-dirname) "/?$") (caar ptr))
	       (string-match (concat "^" (expand-file-name "~/") (regexp-quote trashcan-dirname) "/?$") (caar ptr)))

	   (progn
	     ;;(debug)
	     (error (concat "You cannot move a trashcan directory (%s) into a trashcan directory "
			    "(Try \"rm -r\" instead)")
		    trashcan-dirname)))
       (setq ptr (cdr ptr))))

  ;;(debug)

  (let ((in-trash (trashcan--in-trashcan default-directory 'OR-SUBDIR))
	(files (mapcar (function car) l)))

    ;; NOTE: these two have the same result...
    (setq files (nreverse (mapcar (function dired-make-relative) files)))
    ;;(setq files (nreverse (mapcar 'dired-make-relative files)))

    ;;(debug)

    (if in-trash
	(if (dired-mark-pop-up " *Deletions*"
			       'delete
			       files
			       dired-deletion-confirmer
			       (format "Permanently Delete %s " (dired-mark-prompt arg files)))
	    (let ((ptr l))

	      ;;(debug)
	      (while ptr
		(trashcan--delete-dangerous (caar ptr))
		(message "Deleted file %s " (caar ptr))
		(setq ptr (cdr ptr)))
	      ;;(debug)
	      (revert-buffer)
	      (trashcan--after-permanent-deletion)))

      (if (dired-mark-pop-up " *Deletions*"
			       'delete
			       files
			       dired-deletion-confirmer
			       (format "Move to trashcan %s " (dired-mark-prompt arg files)))
	  (let ((ptr l)
		(list nil))
	    (while ptr
	      (setq list (cons (caar ptr) list))
	      (setq ptr (cdr ptr)))

	    ;;(debug)
	    (trashcan--rename-to-trash list)
	    (revert-buffer))))))

(defun trashcan--make-absolute (filename)
  (setq filename (expand-file-name filename))

  (if (string-match "/$" filename)
      (setq filename (substring filename 0 (1- (length filename)))))

  ;;(debug)

  (if (not (or (string-match "^[a-zA-Z]:/" filename)
	       (string-match "^/" filename)))
      (concat (expand-file-name default-directory) filename)
    filename))

;;
;; NOTE: This function advised takes one arg (filename)
;;
(defadvice delete-file (around trashcan-stub activate)
  "Adds trashcan functionality to delete-file.  If given an autosave
file, it behaves like the default setting of delete-file.  See the
variable trashcan-patch-delete-stuff-p"
  ;;(beeps "Calling delete-file")
  ;;(debug)
  ;;(debug)
  ;;(if (string-match

  (if (or (not trashcan-patch-delete-stuff-p)
	  (string-match "^#.*#$" (file-name-nondirectory filename)))
      (progn
	;;(beeps "file=%s" (file-name-nondirectory filename))
	ad-do-it)
    (setq filename (trashcan--make-absolute filename))
    ;;(debug)
    (if (trashcan--in-trashcan filename)
	(progn
	  ;; ad-do-it
	  (trashcan--delete-dangerous filename)
	  (trashcan--after-permanent-deletion))
      (trashcan--rename-to-trash (list filename)))))

;;
;; NOTE: This function advised takes one arg (directory)
;;
(defadvice delete-directory (around trashcan-stub activate)
  "Adds trashcan functionality to delete-directory.  If given an
autosave file, it behaves like the default setting of delete-file.
See the variable trashcan-patch-delete-stuff-p"
  ;;(beeps "Calling delete-directory")
  ;;(beeps "directory=%s" directory)
  (if (or (not trashcan-patch-delete-stuff-p)
	  (string-match "^#.*#$" (file-name-nondirectory directory)))
      ad-do-it
    (setq directory (trashcan--make-absolute directory))
    ;;(debug)
    (if (trashcan--in-trashcan directory)
	(progn
	  ;;ad-do-it
	  (trashcan--delete-dangerous directory)
	  (trashcan--after-permanent-deletion))
      (trashcan--rename-to-trash (list directory)))))

(defun trashcan-restore ()
  (interactive)

  (if (not (trashcan--in-trashcan default-directory))
      (error "You must be in the trashcan directory (%s) to execute this command" trashcan-dirname))

  (let* ((list (dired-get-marked-files))
	 (ptr  list))

    (while ptr
      (let* ((source (car ptr))
	     (target (trashcan--decode source))
	     (fnd    (file-name-directory target)))

	;;(debug)

	(if (file-exists-p target)
	    (error "File %s already exists" target))

	(make-directory fnd 'PARENTS)
	(rename-file source target)

	;;
	;; NOTE: are we editing one of the files that we want to restore?
	;;
	;;(trashcan--walk-buffers
	;; '(if (string= (buffer-file-name) source)
	;;      (set-visited-file-name target 'NO-QUERY)))

	;;
	;; NOTE: are we editing a files of a subdirectory that we want to restore
	;;
	(trashcan--walk-buffers
	 '(if (and (buffer-file-name) (string-match (concat "^" (regexp-quote source)) (buffer-file-name)))
	      (let ((n (substring (buffer-file-name) (length source))))
		;;(debug)
		(set-visited-file-name (concat target n) 'NO-QUERY))))

	(trashcan--walk-buffers
	 '(if (and (eq major-mode 'dired-mode) (string= fnd (expand-file-name default-directory)))
	      (revert-buffer)))

	(trashcan--walk-buffers
	 '(if (and (eq major-mode 'dired-mode) (not (file-exists-p (expand-file-name default-directory))))
	      (kill-buffer nil)))

	)
      (setq ptr (cdr ptr))))

  (trashcan--walk-buffers
   '(if (and (eq major-mode 'dired-mode) (trashcan--in-trashcan default-directory 'OR-SUBDIR))
	(revert-buffer))))

(defun trashcan-empty ()
  "Careful when using this command as it cannot be undone"
  (interactive)
  (cond
   ((not (trashcan--in-trashcan default-directory))
    (error "You must be in the trashcan to execute this command"))

   ((not (eq major-mode 'dired-mode))
    (error "You must be in dired mode to execute this command"))

   (t
    (if (yes-or-no-p "Really empty trashcan? ")
	(let (dirname)

	  (cond
	   ((setq dirname (trashcan--in-windows-trashcan default-directory)))
	   ((setq dirname (trashcan--in-unix-trashcan    default-directory)))
	   (t
	    (error "Should never happen")))

	  ;;(debug)

	  (save-window-excursion
	    (trashcan--delete-dangerous dirname))

	  ;;(beeps "Deleting file %s" dirname)

	  (make-directory dirname 'PARENTS)
	  (revert-buffer)
	  (trashcan--after-permanent-deletion))))))

(provide 'trashcan)
;;; trashcan.el ends here
