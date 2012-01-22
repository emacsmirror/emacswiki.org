;;; symlink-fix --- Remove symbolic links from file pathnames.
;; Copyright (C) 1989, 1990, 1991, 1993 Free Software Foundation, Inc.

;; This file is not officially part of GNU Emacs.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
;; to anyone for the consequences of using it or for whether it serves any
;; particular purpose or works at all, unless he says so in writing.
;; Refer to the GNU Emacs General Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute this
;; file, but only under the conditions described in the GNU Emacs General
;; Public License.  A copy of this license is supposed to have been given
;; to you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  Among other
;; things, the copyright notice and this notice must be preserved on all
;; copies.

;; Created by: Joe Wells, joew@uswest.com
;; Created on: summer 1988
;; Last modified by: Joe Wells, jbw@csd
;; Last modified on: Tue Feb 23 23:05:51 1993
;; Filename: symlink-fix.el
;; Purpose: remove symbolic links from pathnames
;; Change log: 
;; 
;; Tue Feb 23 21:31:57 1993  Joe Wells  (jbw at csd)
;; 
;; 	* Made overloading more sophisticated.  Avoid possible recursion
;; 	resulting from Emacs Lisp primitives being redefined to call
;; 	expand-file-name.  (Ange-FTP changes file-symlink-p to call
;; 	expand-file-name.)  Allow changing the order of overloading to put
;; 	this package's function on top of the overloading stack.  Allow
;; 	prevention of symlink-expansion (even calling file-symlink-p) on
;; 	certain pathnames.  (It can be very slow to call file-symlink-p on
;; 	a remote file accessed through Ange-FTP.)  Allow dynamically
;; 	toggling symlink resolution by expand-file-name.  Save and restore
;;      match data.
;; 
;; Fri Aug 23 12:54:22 1991  Joe Wells  (jbw at teton)
;; 
;; 	* Fixed some bugs with the symlink-mapping-alist.
;; 
;; Thu Aug 22 14:58:33 1991  Joe Wells  (jbw at teton)
;; 
;; 	* Added documentation.
;; 

;; LCD Archive Entry:
;; symlink-fix|Joe Wells|jbw@cs.bu.edu|
;; Remove symbolic links from file pathnames|
;; 1993-02-23||~/packages/symlink-fix.el.Z|

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Documentation (what little there is).
;;;

;; Features:

;; 1. Makes Emacs use the real path, which prevents accidentally visiting
;; the same file twice by different paths.  This problem could happen when
;; Emacs used the output of a program that had called getwd, and also in
;; other similar circumstances.

;; Known problems:

;; 1. expand-file-name (if overloaded and symlink resolution is turned on)
;; saves and restores the match data.  In old versions of Emacs, the
;; routines for saving and restoring the match data were buggy.  The
;; solution is to upgrade to a modern version of Emacs.

;; 2. symlink-expand-file-name does too much string creation.  This should
;; be fixed by having it use a scratch buffer instead.

;; 3. expand-file-name (if overloaded and symlink resolution is turned on)
;; doesn't work really well with the automounter.  In essence, you have to
;; selectively disable part of its functionality to prevent the
;; automounter from unmounting file systems from under you.  What would be
;; really great is if some enterprising individual would write a routine
;; that restores automounter symbolic links back into pathnames.

;; 4. expand-file-name (if overloaded and symlink resolution is turned on)
;; doesn't resolve symlinks if the overloading occurred before ange-ftp
;; was loaded unless you modify ange-ftp-expand-file-name to not be clever
;; and always call ange-ftp-real-expand-file-name.  Overloading after
;; ange-ftp is loaded works correctly.  You can either load this file
;; after Ange-FTP or set ange-ftp-load-hook (Nonexistent yet!  Arggh!) to
;; call the function symlink-overload-expand-file-name.

;; 5. You may not like having symlinks removed from remote file names
;; because it can be slow, so in that case you should set the variable
;; symlink-dont-resolve-symlinks-regexp to match Ange-FTP remote filenames
;; like this:
;;
;;   (setq symlink-no-resolve-symlinks-regexp (car ange-ftp-path-format))
;;

;; Sample configuration:

;; This sample configuration uses all of the configuration variables.  You
;; probably don't want all of these configurations, and some of it would
;; have to be adjusted for your site anyway.

;; (setq symlink-overload-expand-file-name-p t)
;; (require 'symlink-fix)
;; (setq expand-file-name-resolve-symlinks-p t)
;; (setq symlink-mapping-alist '(("\\`/nfs/" . nil)))
;; *** This won't work yet because there is no ange-ftp-load-hook!  Arrgh! ***
;; (defun symlink-ange-ftp-hook-function ()
;;   (symlink-overload-expand-file-name)
;;   (setq symlink-no-resolve-symlinks-regexp
;; 	(car ange-ftp-path-format)))
;; (if (featurep 'ange-ftp)
;;     (symlink-ange-ftp-hook-function)
;;   (setq ange-ftp-load-hook
;; 	(cons symlink-ange-ftp-hook-function ange-ftp-load-hook)))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Configuration variables.
;;;

(defvar symlink-overload-expand-file-name-p nil

  "*Whether expand-file-name is overloaded at load time of symlink-fix.el.

This determines whether the function symlink-overload-expand-file-name is
called at load time.  (This is actually a simplification, as the actual
behavior is more sophisticated.)  This does not necessarily change the
behavior of expand-file-name, since symlinks will not be resolved by the
overloading function unless the variable
expand-file-name-resolve-symlinks-p is non-nil.

The function symlink-expand-file-name always calls the function value that
expand-file-name had at the last time the function
symlink-overload-expand-file-name was called.  (This is another
simplification, as the actual overloading behavior is more sophisticated.)
This should work with other packages that overload expand-file-name,
unless they do not always call the function they overload, in which case
you can overload expand-file-name after the other packages do by calling
symlink-overload-expand-file-name again.

Setting this variable after symlink-fix.el is loaded has no effect.")

(defvar expand-file-name-resolve-symlinks-p t
  "*Whether expand-file-name will resolve symbolic links after it has been
overloaded by symlink-overload-expand-file-name.  Set to nil to get the
default behavior of expand-file-name.")

(defvar symlink-mapping-alist nil
  "*Used to hide certain directories during symlink elimination.

Should be an alist where each element is of the form \(REGEXP .
REPLACEMENT\).  When an absolute symbolic link points to a path matched by
REGEXP, the portion of the path that matches either the entire REGEXP is
replaced by REPLACEMENT \(as described for replace-match\).  If REPLACEMENT
is nil, then the original symbolic link is used rather than the path it
points to.  This is only applied when the symbolic link points to an
absolute path.

As an example, if /tmp_mnt is an automounter directory, you might want to
make symlink-mapping-alist have this value:

  ((\"\\\\`/tmp_mnt/\" . nil))

Another example is if /u is a directory containing symbolic links to each
person's real home directory, which are located in /home/machine/username,
and it is desired that this be hidden.  Then symlink-mapping-alist can be
given this value:

  ((\"\\\\`/home/[-a-z0-9]+/[a-z]+\\\\'\" . nil))

Note that doing this defeats one of the main points of using
symlink-expand-file-name, which is to have Emacs use the same pathname
that is reported by getwd \(which is used by many other programs whose
output Emacs uses to find files\).

Also note that any transformation specified should yield the same file
pointed to by the original symbolic link or an equivalent one.  Otherwise
all bets are off.")

(defvar symlink-no-resolve-symlinks-regexp nil
  "*A regular expression for symbolic links that will not be resolved.

If the absolute pathname of a symbolic link \(possibly including some .
and .. elements if it is pointed to by another symbolic link\) matches
this regular expression, then the symbolic link's value will not be
substituted into the pathname by the function symlink-expand-file-name
\(and expand-file-name if overloaded and symlink resolution is turned
on\).")

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The symbolic link resolution routines.
;;;

(defun symlink-expand-file-name (file-name &optional directory)
  "Convert FILENAME to absolute, and canonicalize it.
Second arg DIRECTORY is directory to start with if FILENAME is relative
\(does not start with slash); if DIRECTORY is nil or missing,
the current buffer's value of default-directory is used.
Filenames containing . or .. as components are simplified;
initial ~ is expanded.  See also the function  substitute-in-file-name.

All symbolic links are resolved from FILENAME, with exceptions specified
by symlink-mapping-alist and symlink-no-resolve-symlinks-regexp."

  (let ((match-data (match-data))
	left right split link new)
    
    ;; Call the original expand-file-name.  This is an essential part of
    ;; any overloading protocol, that you avoid short-circuiting things in
    ;; case some other package is overloading the same function.
    (setq right (symlink-original-expand-file-name file-name directory))
    (setq left "")
    
    ;; Resolve symbolic links to return a canonical path.
    (while (not (equal right ""))
      (setq split (symlink-split-file-name right))
      (setq left (symlink-join-file-name left (car split)))
      (setq right (cdr split))
      
      ;; Allow users to specify that they don't want to resolve symbolic
      ;; links in filenames matching a particular pattern.  The usual
      ;; reason for this will be that the operation is too expensive, for
      ;; example with remote FTP filenames.
      ;; Otherwise find a symbolic link if any.
      (setq link
	    (if (and symlink-no-resolve-symlinks-regexp
		     (string-match symlink-no-resolve-symlinks-regexp left))
		nil
	      (file-symlink-p left)))

      (if (null link)
	  nil

	;; We found a symbolic link, handle it.
	(if (eq 0 (length link)) (setq link "."))
	(if (not (eq (aref link 0) ?/))

	    ;; Handle a relative symbolic link.
	    (setq split (symlink-split-file-name link)
		  left (symlink-join-file-name (file-name-directory left)
					       (car split))
		  right (symlink-join-file-name (cdr split) right))
	  
	  ;; Handle an absolute symbolic link.
	  ;; Check for symlink hiding.
	  (setq new (symlink-assoc-string-match link symlink-mapping-alist))
	  (if new

	      ;; The symbolic link is hidden from resolution or has an
	      ;; alternate resolution specified.
	      (if (cdr new)

		  ;; An alternate resolution for the symbolic link is
		  ;; specified.
		  ;; *** TODO: Check that this alternate resolution still
		  ;; *** points to the same file!
		  (setq left (symlink-replace-regexp-string
			      link (car new) (cdr new))))
	    
	    ;; The symbolic link is not hidden from resolution.
	    ;; We have an absolute path now so we're starting over.
	    (setq right (symlink-join-file-name link right))
	    (setq left "")))))

    ;; *** I'm not quite sure why I'm doing this.  I think this is to
    ;; *** handle relative symbolic links with ".." or "." in them.
    (setq left (symlink-original-expand-file-name left))
    
    ;; Restore the prior regexp match data since we trashed it.
    ;; expand-file-name doesn't trash the match data, so we can't either.
    (store-match-data match-data)
    
    ;; Return something that is EQ to the input argument if the result is
    ;; the same as the input.  expand-file-name behaves this way, so we
    ;; have to do this too.
    (if (string-equal left file-name)
	file-name
      left)))

(defun symlink-join-file-name (left right)
  "Concatenates LEFT and RIGHT, preserving at most one slash between them.
This horrible hack is necessary to work around the fact that
expand-file-name treats // specially."
  (let* ((llen (length left))
	 (rlen (length right))
	 (lend llen)
	 (rstart 0)
	 slash-found)
    (while (and (> lend 0)
		(eq ?/ (aref left (1- lend))))
      (setq slash-found t
	    lend (1- lend)))
    (while (and (< rstart rlen)
		(eq ?/ (aref right rstart)))
      (setq slash-found t
	    rstart (1+ rstart)))
    (concat (if (eq lend llen) left (substring left 0 lend))
	    (if slash-found "/" "")
	    (if (eq rstart 0) right (substring right rstart rlen)))))

(defun symlink-split-file-name (file-name)
  "Splits FILENAME into two strings, and returns a list of the two
strings.  The first string will be the first filename component in
FILENAME, plus any leading slashes, and the second string will be the
rest of FILENAME, possibly a string of length 0."
  (if (string-match "\\`\\(/*[^/]+\\)\\(/.*\\)\\'" file-name)
      (cons (substring file-name (match-beginning 1) (match-end 1))
	    (substring file-name (match-beginning 2) (match-end 2)))
    (cons file-name "")))

(defun symlink-assoc-string-match (string alist)
  "Like CL (assoc STRING ALIST :test #'(lambda (x y) (string-match y x))).
If a match is made, then the match data is from the successful match,
otherwise it is clobbered."
  ;; TODO: use a real save-match-data
  (let (item)
    (catch 'found
      (while (consp alist)
	(setq item (car alist))
	(and (consp item)
	     (string-match (car item) string)
	     (throw 'found item))
	(setq alist (cdr alist))))))

;; A function which is not in standard GNU Emacs Lisp but should be.
;; TODO: give it arguments like replace-regexp, not replace-match
(defun symlink-replace-regexp-string (string regexp to-string
					     &optional fixedcase literal)
  "Replace matches in STRING for REGEXP by TO-STRING.
If fourth arg FIXEDCASE is non-nil, do not alter case of replacement text.
Otherwise convert to all caps or cap initials, like replaced text.
If fifth arg LITERAL is non-nil, use TO-STRING literally.
Otherwise treat \ as special:
  \& in TO-STRING means substitute original matched text,
  \N means substitute match for \(...\) number N,
  \\ means insert one \."
  (save-excursion
    (set-buffer (get-buffer-create " *replace-regexp-string*"))
    (erase-buffer)
    (if (string-match "^19" emacs-version)
	(buffer-disable-undo (current-buffer))
      (buffer-flush-undo (current-buffer)))
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match to-string fixedcase literal))
    (buffer-string)))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Overloading routines and bookkeeping variables.
;;;

;;
;; Arrange for overloading of expand-file-name if necessary.  Arrange for
;; things to work if expand-file-name is not overloaded.
;; 

(defvar symlink-intermediary-overloading-symbol nil
  "Don't touch this!  The value of this variable is an uninterned symbol
whose purpose for existence is to allow the overloading of
expand-file-name by this package to be safely removed from the overloading
stack even though other packages might have overloaded expand-file-name
after this.  The function value of the uninterned symbol is the
overloading function of this package.")

(defvar symlink-intermediary-overloading-symbol-counter 0
  "Don't touch this!  This variable is used to form unique names for the
uninterned symbols that are the values of the variable
symlink-intermediary-overloading-symbol.  Since all uninterned symbols are
different even if they are given the same name, the only reason this
variable exists is to make debugging easier.")

(defvar symlink-expand-file-name-overloaded-yet-p nil
  "Whether expand-file-name has been overloaded at least once by the
symlink-fix package.")

(defun symlink-overload-expand-file-name ()
  "Overload expand-file-name with the function symlink-new-expand-file-name.
If this overloading has been done before, it is removed and redone.  This
allows changing the order of overloading if more than one package is
overloading expand-file-name."

  (if symlink-expand-file-name-overloaded-yet-p
      ;; If we've already overloaded it, then if someone else has overloaded
      ;; it after us, remove ourself from the overloading chain before
      ;; overloading it again, otherwise pop ourself from the top of the
      ;; overloading chain.
      (if (eq (symbol-function 'expand-file-name)
	      symlink-intermediary-overloading-symbol)
	  (fset 'expand-file-name 
		(symbol-function 'symlink-original-expand-file-name))
	(fset symlink-intermediary-overloading-symbol
	      (symbol-function 'symlink-original-expand-file-name))))
  
  (setq symlink-intermediary-overloading-symbol
	(make-symbol
	 (format "symlink-intermediary-overloading-symbol-uninterned-%d"
		 (setq symlink-intermediary-overloading-symbol-counter
		       (1+ symlink-intermediary-overloading-symbol-counter)))))
  (fset 'symlink-original-expand-file-name
	(symbol-function 'expand-file-name))
  (fset 'expand-file-name symlink-intermediary-overloading-symbol)
  (fset symlink-intermediary-overloading-symbol
	'symlink-new-expand-file-name)
  (setq symlink-expand-file-name-overloaded-yet-p t))

(defun symlink-new-expand-file-name (filename &optional directory)
  "Convert FILENAME to absolute, and canonicalize it.
Second arg DIRECTORY is directory to start with if FILENAME is relative
\(does not start with slash); if DIRECTORY is nil or missing,
the current buffer's value of default-directory is used.
Filenames containing . or .. as components are simplified;
initial ~ is expanded.  See also the function  substitute-in-file-name.

All symbolic links are resolved from FILENAME, with exceptions specified
by symlink-mapping-alist and symlink-no-resolve-symlinks-regexp.

NOTE:  This is not the standard expand-file-name that comes with Emacs!
This is the symlink-new-expand-file-name function that is overloading
expand-file-name."

  (if expand-file-name-resolve-symlinks-p
      (let ((expand-file-name-resolve-symlinks-p nil))
	(symlink-expand-file-name filename directory))
    (symlink-original-expand-file-name filename directory)))

;;
;; Give a default initial behavior that will make symlink-expand-file-name
;; work even if expand-file-name is not overloaded.
;;

(or (fboundp 'symlink-original-expand-file-name)
    (fset 'symlink-original-expand-file-name 'expand-file-name))

;;
;; Perform the overloading at load time if requested.
;;

(if (and symlink-overload-expand-file-name-p
	 (not symlink-expand-file-name-overloaded-yet-p))
    (symlink-overload-expand-file-name))

(provide 'symlink-fix)
