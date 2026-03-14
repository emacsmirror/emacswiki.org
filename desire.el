;;; desire.el --- versatile configuration for emacs lisp packages

;; Authors:         Martin Schwenke <martin@meltin.net>
;;                  Graham Williams <Graham.Williams@cmis.csiro.au>
;; Maintainer:      martin@meltin.net
;; Created:         20-Jun-1995
;; $Id: desire.el,v 1.13 2001/02/12 00:41:42 martin Exp $	

;; Keywords: setup configuration

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001 Martin Schwenke and Graham Williams

;; This file is NOT part of GNU Emacs.  It is, however, distributed
;; under the same conditions as GNU Emacs, which are as follows:

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; sourced from: https://web.archive.org/web/20060720155936/http://meltin.net/hacks/emacs/src/desire.el [2026-03-14]

(defvar desire-load-path nil
  "*List of directories to be searched by `desire' for configuration
data.")

(defvar desire-extension ".ecf"
  "*The extension given to configuration files used by `desire'.")

(defvar desire-loaddefs "loaddefs"
  "*A string which is the name of a configuration file that is loaded
before a package itself is loaded.  This is useful for autoloading
functions.")

(defvar desire-desire "desire"
  "*A string which is the name of a configuration file that is loaded
after a package itself is loaded, but before the configuration files
for any other desirable package.  This is useful for performing some
initial setup.")

(defvar desire-execute "execute"
  "*A string which is the name of a configuration file that is loaded
after a package itself is loaded, and also after the configuration
files for any other desirable package.  This is useful for performing
some final setup.")

(defvar desirable nil
  "A list of strings for packages which have been successfully desired.
The function `desired' will add an item to this list.")

(defun desired (package)
  "Add PACKAGE (a symbol) as something which is `desirable'."
  (or (symbolp package)
      (error "Wrong type argument to `desired': symbolp, %s"
	     (prin1-to-string package)))
  (add-to-list 'desirable (symbol-name package)))

(defun desiredp (package)
  "Return t if PACKAGE is `desirable'.  PACKAGE can be a symbol or a string."
  (let ((p (or
	    (and (symbolp package) (symbol-name package))
	    (and (stringp package) package)
	    (error
	     "Wrong type argument to `desiredp': symbolp or stringp, %s"
	     (prin1-to-string package)))))
    (and (member p desirable) t)))

(defun desire (package &optional fname)
  "Arrange loading and configuration of a desired emacs PACKAGE.
PACKAGE is a symbol representing the name of a package.  The aim is to
set up some autoloads and other initial configuration, and possibly
organise for more configuration files to be dynamically loaded when
the package itself is finally loaded.  The optional argument FNAME is
a string containing the name of the file that, when loaded, will
trigger dynamic loading of extra configuration files.  If FNAME is
omitted then the string corresponding to PACKAGE is used instead.

Each directory in `desire-load-path' is searched in order to see if
configuration data for PACKAGE exists.  The configuration data takes
one of 2 forms:

1. A file named PACKAGE followed by the value of the variable
   `desire-extension'.  If such a file exists, then that file is taken
   to be the sole configuration file for the package.  This file is
   loaded immediately.  The file might contain autoloads or might load
   the package itself.

2. A directory named PACKAGE.  If the directory contains a file
   corresponding to `desire-loaddefs' then that file is loaded
   immediately.  Other files in the directory are processed using the
   function `desire-process-directory' after the package is actually
   loaded. 

Note that a single file, as described in (1), takes precedence over a
directory, as described in (2).  For a directory, if the package has
already been loaded then the configuration data *is* loaded but the
package is *not*.  This depends on the `eval-after-load' function
behaving similarly.

If a package is successfully configured then it is `desired' and,
therefore, marked as `desirable'.  Desirability can be checked using
the function `desiredp'.  If PACKAGE has been previously `desired'
then nothing happens and nil is returned."

  (or (symbolp package)
      (error "Wrong type argument to `desire': symbolp, %s"
	     (prin1-to-string package)))

  (if (not (desiredp package))
      (let* ((dirs desire-load-path)
	     (pname (symbol-name package))
	     (lname (if fname fname pname)))
	
	(while dirs
	  
	  (let ((prefix (expand-file-name pname (car dirs))))
	    
	    (cond
	     ;; Check for configuration file.
	     ((desire-readable-regular-file-p
	       (concat prefix desire-extension))
	      
	      ;; Load configuration data.
	      (desire-load-file (car dirs) pname)
	      
	      ;; Finished!
	      (desired package)
	      (setq dirs nil))
	     
	     ;; Check for configuration directory.
	     ((and (file-directory-p prefix)
		   (file-readable-p prefix))
	      
	      ;; If file specified by desire-loaddefs exists then load
	      ;; it.
	      (if (and desire-loaddefs
		       (desire-readable-regular-file-p
			(expand-file-name
			 (concat desire-loaddefs desire-extension)
			 prefix)))
		  
		  (desire-load-file prefix desire-loaddefs))
	      
	      ;; Setup processing of directory.
	      (eval-after-load
	       lname
	       (` (desire-process-directory (, prefix))))
	      
	      ;; Finished!
	      (desired package)
	      (setq dirs nil))
	     
	     ;; Otherwise 
	     (t
	      
	      (setq dirs (cdr dirs)))))))))

(defun desire-load-file (dir file)
  "Load FILE from DIR after appending `desire-extension'."

  (load-file (expand-file-name (concat file desire-extension) dir)))

(defun desire-process-directory (dir)

  "Load files in DIR if they correspond to desirable packages.

If a configuration file for a previously desired package is present in
DIR, then that file is loaded.  For example, if a package called
\"goodthing\" has previously been desired, then if a file exists in
DIR called \"goodthing\" (plus `desire-extension') it will be loaded.
If \"goodthing\" has not previously been desired, then this file will
be ignored.

A file for the pseudo-package specified by `desire-desire' is always
loaded first.  A file for the pseudo-package specified by
`desire-execute' is always loaded last."

  (let ((fs (desire-directory-file-prefixes dir))
	exec)

    ;; Check for the desire-desire file.
    (if (member desire-desire fs)

	(progn
	  (desire-load-file dir desire-desire)
	  (setq fs (delete desire-desire fs))))

    ;; Check for the desire-execute file.
    (if (member desire-execute fs)
	(progn
	  (setq exec t)
	  (setq fs (delete desire-execute fs))))

    ;; Load desirable files!
    (while fs
      (let ((hd (car fs)))

	;; Check to see if the prefix represents a feature.
	(if (desiredp hd)
	    (desire-load-file dir hd)))

      (setq fs (cdr fs)))

    ;; Load desire-execute if required.
    (if exec
	(desire-load-file dir desire-execute))))
    
(defun desire-directory-file-prefixes (dir)

  "Return the prefixes of configuration files in DIR.
Configuration files are those that readable, regular files which have
the extension specified by variable `desire-extension'.  The prefix
is the file name with the directory and extension removed."

  (let ((ext-regexp (concat (regexp-quote desire-extension) "$"))
	(out))

    (apply 'append
	   (mapcar
	    (function
	     (lambda (f)
	       (let ((full (expand-file-name f dir)))
		 (if (and
		      (desire-readable-regular-file-p full)
		      (string-match ext-regexp f))
		     (list (substring f 0 (string-match ext-regexp f)))))))
	    (directory-files dir)))))

(defun desire-readable-regular-file-p (f)

  "Determine if F is a readable, regular file."

  (and (if (fboundp 'file-regular-p)
	   (file-regular-p f)
	 t)
       (file-readable-p  f)))

(provide 'desire)
