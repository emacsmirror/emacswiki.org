;;; ls-lisp.el --- emulate insert-directory completely in Emacs Lisp

;; Copyright (C) 1992, 1994, 2000, 2004 Free Software Foundation, Inc.
;;               2005 Lars Hansen

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;; Modified by: Francis J. Wright <F.J.Wright at qmul.ac.uk>
;; Last-Updated: 22-11-2005 18:00 UTC
;; By: Lars Hansen <larsh at soem dot dk>
;; URL: http://www.emacswiki.org/emacs/ls-lisp.el
;; Keywords: unix, dired

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; OVERVIEW ==========================================================

;; This file redefines the function `insert-directory' to implement it
;; directly from Emacs lisp, without running ls in a subprocess.  It
;; is useful if you cannot afford to fork Emacs on a real memory UNIX,
;; if you don't have the ls program on a non-UNIX platform, or if you
;; want a different format from what ls offers.

;; By default, ls-lisp supports shell wildcards and also expands {}
;; like bash, but it can also use Emacs regexps; see the user option
;; `ls-lisp-support-shell-wildcards'.

;; To use a non-standard ls-lisp in versions of Emacs that have it
;; built in, such as NT Emacs, it must be explicitly loaded; require
;; will not work.  For example, put ls-lisp.el in a directory in your
;; load-path, byte-compile it, and include the following in your
;; .emacs file:
;;
;; (add-hook 'dired-load-hook
;;           (lambda () (load "ls-lisp")))

;; RESTRICTIONS ======================================================

;; * A few obscure ls switches are still ignored: see the docstring of
;; `insert-directory'.

;; * Generally only numeric uid/gid.

;; TO DO =============================================================

;; Complete handling of F switch (if/when possible).

;; FJW: May be able to sort much faster by consing the sort key onto
;; the front of each list element, sorting and then stripping the key
;; off again!

;;; History:

;; Written originally by Sebastian Kremer <sk@thp.uni-koeln.de>
;; Revised by Andrew Innes and Geoff Voelker (and maybe others).

;; Modified by Francis J. Wright <F.J.Wright at qmul.ac.uk>, mainly to
;; support many more ls options, "platform emulation", a hook for
;; external symbolic link support, more robust sorting and {}
;; expansion.

;; Changes by Lars Hansen <larsh at soem dot dk> on 2005-11-22 to
;; file marked "v 1.50 2004/02/15 18:06:30 fjw" found on
;; http://centaur.maths.qmw.ac.uk/Emacs/:
;; Definition of and calls to stub `ls-lisp-parse-symlink' removed.

;;; Code:

(eval-when-compile
  (require 'cus-edit))

;;;###autoload
(defgroup ls-lisp nil
  "Emulation of the ls program completely in Emacs Lisp.
See `insert-directory' for details.  The customization options in this
group are additional to those in the dired group."
  :version "21.1"
  :group 'dired)

(defcustom ls-lisp-use-insert-directory-program nil
  "*Non-nil causes ls-lisp to revert to using `insert-directory-program'.
This is useful on platforms where ls-lisp is dumped into Emacs, such
as Microsoft Windows, but you would still like to use an external ls
program to list the contents of a directory."
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-support-shell-wildcards '{}
  "*Non-nil causes ls-lisp to treat file patterns as shell wildcards.
Otherwise they are treated as Emacs regexps (for backward compatibility).
The atom {} (the default) causes ls-lisp also to expand {a,b,...} like bash.
See `ls-lisp-wildcard-to-regexp' for details."
  :type '(choice (const :tag "Yes, expanding {}" {})
		 (const :tag "Yes, ignoring {}" t)
		 (const :tag "No" nil))
  :group 'ls-lisp
  :version "21.4")

(defun ls-lisp-emulation-set (variable value)
  "Set option VARIABLE to VALUE and then update its dependent options."
  (custom-set-default variable value)
  (let ((dep-vars '(ls-lisp-ignore-case ls-lisp-dirs-first ls-lisp-verbosity)))
    (mapc
     (lambda (v)
       (or (get v 'saved-value)
	   ;; Variable has no custom value, so reset it to its
	   ;; RE-EVALUATED default:
	   (funcall (or (get v 'custom-set) 'set-default)
		    v
		    (eval (car (get v 'standard-value))))))
     dep-vars)
    (if (string= (buffer-name) "*Customize Group: Ls Lisp*")
	(mapc (lambda (widget)
		(if (memq (widget-value widget) dep-vars)
		    (custom-redraw widget)))
	      (widget-get (car custom-options) :children)))))

(defcustom ls-lisp-emulation
  (cond ((eq system-type 'macos) 'MacOS)
	;; ((eq system-type 'windows-nt) 'MS-Windows)
	((memq system-type
	       '(hpux dgux usg-unix-v unisoft-unix rtu irix berkeley-unix))
	 'UNIX))			; very similar to GNU
  ;; Anything else defaults to nil, meaning GNU.
  "*Platform to emulate: GNU (default), MacOS, MS-Windows, UNIX.
Corresponding value is one of the atoms: nil, MacOS, MS-Windows, UNIX.
Emulation need not match the actual platform.
Sets default (but not customized) values for: `ls-lisp-ignore-case',
`ls-lisp-dirs-first', `ls-lisp-verbosity'."
  :type '(choice (const :tag "GNU" nil)
		 (const MacOS)
		 (const MS-Windows)
		 (const UNIX))
  :set 'ls-lisp-emulation-set
  :group 'ls-lisp)

(defcustom ls-lisp-ignore-case
  (memq ls-lisp-emulation '(MS-Windows MacOS))
  "*Non-nil causes ls-lisp primary alphabetic sorting to ignore case.
Default is t if `ls-lisp-emulation' is MS-Windows or MacOS, nil otherwise."
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-dirs-first (eq ls-lisp-emulation 'MS-Windows)
  "*Non-nil causes ls-lisp to sort directories first in any ordering.
\(Or last if it is reversed.)  Follows Microsoft Windows Explorer.
Default is t if `ls-lisp-emulation' is MS-Windows, nil otherwise."
  ;; Functionality suggested by Chris McMahan <cmcmahan at one.net>
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-verbosity
  (cond ((eq ls-lisp-emulation 'MacOS) nil)
	((eq ls-lisp-emulation 'MS-Windows)
	 (if (and (fboundp 'w32-using-nt) (w32-using-nt))
	     '(links)))			; distinguish NT from 9x
	(t '(links uid gid-maybe)))
  "*A list of optional file attributes that ls-lisp should display.
It should contain none or more of the symbols:
   links, uid, gid or gid-maybe.
Nil (or an empty list) means display none of them.

Concepts come from UNIX: links means count of names associated with
the file\; uid means user (owner) identifier\; gid means group
identifier\; gid-maybe means respect ls switches G/g (see
`insert-directory' for details).

If `ls-lisp-emulation' is MacOS then default is nil\;
if `ls-lisp-emulation' is MS-Windows then default is (links) if
platform is Windows NT, nil otherwise\;
otherwise default is (links uid gid-maybe)."
  ;; Functionality suggested by Howard Melman <howard at silverstream.com>
  :type '(set (const :tag "Show Link Count" links)
	      (const :tag "Show User" uid)
	      (choice :tag "Show Group" :value gid ; default
	       (const :tag "ignoring ls switches G/g" gid)
	       (const :tag "honouring ls switches G/g" gid-maybe)))
  :group 'ls-lisp)

(defcustom ls-lisp-hide-.lnk (eq system-type 'windows-nt)
  "*Non-nil causes ls-lisp to hide the .lnk extension on Windows shortcuts.
Default is t on Windows, nil otherwise.
Works only if shortcuts are recognised as symlinks\; ignored otherwise.
Applies to both Microsoft and Cygwin shortcut formats.
Suitable symlink support is provided by w32-symlinks.el from
http://centaur.maths.qmul.ac.uk/Emacs/."
  :type 'boolean
  :group 'ls-lisp
  :link '(url-link "http://centaur.maths.qmul.ac.uk/Emacs/")
  :version "21.4")

(defcustom ls-lisp-past-cutoff (- (* 6 30 24 60 60))
  "*Past file time display cutoff in \(negative\) seconds.
Times more in the past that this negative number of seconds are
displayed using year rather than time-of-day.
Default (following ls) is 6 30-day months.
0 and -1e+INF are (usually) both valid, meaning now or never."
  ;; Functionality suggested by Geert Ribbers <geert.ribbers at realworld.nl>
  :type '(restricted-sexp :match-alternatives
			  ((lambda (x)
			     (and (numberp x) (<= x 0)))))
  :group 'ls-lisp
  :version "21.4")

(defcustom ls-lisp-future-cutoff (* 60 60)
  "*Future file time display cutoff in seconds.
Times more in the future that this number of seconds are displayed
using year rather than time-of-day.  Default (following ls) is 1 hour.
0 and 1e+INF are (usually) both valid, meaning now or never."
  ;; Functionality suggested by Geert Ribbers <geert.ribbers at realworld.nl>
  :type '(restricted-sexp :match-alternatives
			  ((lambda (x)
			     (and (numberp x) (>= x 0)))))
  :group 'ls-lisp
  :version "21.4")

(defcustom ls-lisp-skip-inaccessible-files nil
  "*Non-nil causes ls-lisp to skip inaccessible files.
This means it ignores files that cannot be opened to access their
attributes, which may upset ls-lisp, e.g. by causing a sorting error.
Enable this option only if necessary because it will slow down ls-lisp
slightly."
  ;; Such files can appear in C:\WINNT on Windows NT 4.0!
  :type 'boolean
  :group 'ls-lisp
  :version "21.4")

;; Remember the original insert-directory function
(unless (featurep 'ls-lisp)  ; this file is being reloaded!
  (fset 'original-insert-directory (symbol-function 'insert-directory)))

(defvar ls-lisp-group-display nil	; internal global variable
  "Non-nil causes ls-lisp to display file group.
If the value is a string then it is displayed, e.g. \" root\",
otherwise the real group of the file is displayed.
This variable is set in `insert-directory'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Main functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This version of the function comes from `ls-lisp.el'.  If the value of
`ls-lisp-use-insert-directory-program' is non-nil then it works
exactly like the version from `files.el' and runs a directory listing
program whose name is in the variable `insert-directory-program'; if
also WILDCARD is non-nil then it runs the shell specified by
`shell-file-name'.  If `ls-lisp-use-insert-directory-program' is nil
then it runs a Lisp emulation, which does not run any external
programs or shells.  It supports shell wildcards (and optionally brace
expansion) if `ls-lisp-support-shell-wildcards' is non-nil (or {})\;
otherwise, it interprets wildcards as Emacs regular expressions to
match file names.  It supports the following GNU `ls' switches:

  -a  do not hide entries starting with .
  -A  do not list implied . and ..
  -B  do not list implied entries ending with ~
  -c  with -lt: sort by, and show, ctime (time of last
         modification of file status information)
      with -l: show ctime and sort by name
      otherwise: sort by ctime
  -C  list entries by columns
  -F  append indicator (one of */@) to entries
  -g  display group information (UNIX emulation)
  -G  inhibit display of group information (other emulations)
         [For g/G see also `ls-lisp-emulation' and `ls-lisp-verbosity'.]
  -h  print sizes in human readable format (e.g. 1k, 234M, 2G)
  -i  print index number of each file
  -l  use long listing format (default)
  -L  list entries pointed to by symbolic links
  -r  reverse order while sorting
  -R  list subdirectories recursively
  -s  print size of each file, in blocks
  -S  sort by file size
  -t  sort by modification time
  -u  with -lt: sort by, and show, access time
      with -l: show access time and sort by name
      otherwise: sort by access time
  -U  do not sort; list entries in directory order
  -X  sort alphabetically by entry extension

By default, long listing format is used and entries are sorted
alphanumerically by name.  Invalid switches are silently ignored.

The F and L switches require separate symlink support on non-GNU/UNIX
platforms. Suitable symlink support for MS Windows is provided by
w32-symlinks.el from http://www.emacswiki.org/emacs/w32-symlinks.el."
  (if ls-lisp-use-insert-directory-program
      (original-insert-directory file switches wildcard full-directory-p)
    ;; We need the directory in order to find the right handler.
    (let ((handler (find-file-name-handler (expand-file-name file)
					   'insert-directory)))
      (if handler
	  (funcall handler 'insert-directory file switches
		   wildcard full-directory-p)
	;; Convert SWITCHES to a list of characters.
	(setq switches (delete ?- (append switches nil)))
	;; Handle wildcards, and switches a, A and B.
	(if wildcard
	    (progn
	      ;; Sometimes we get ".../foo*/" as FILE.  While the shell and
	      ;; `ls' don't mind, we certainly do, because it makes us think
	      ;; there is no wildcard, only a directory name.
	      (if (eq (aref file (1- (length file))) ?/)
		  (setq file (substring file 0 -1)))
	      (setq wildcard
		    (if ls-lisp-support-shell-wildcards
			(ls-lisp-wildcard-to-regexp
			 (file-name-nondirectory file))
		      (file-name-nondirectory file))
		    file (file-name-directory file)))
	  (cond ((memq ?A switches)
		 ;; show all files except . and ..
		 (setq wildcard "\\`\\(\\.[^.]\\|[^.]\\)"))
		((not (memq ?a switches))
		 ;; if neither -A  nor -a, flush . files
		 (setq wildcard "\\`[^.]")))
	  (if (memq ?B switches)
	      (setq wildcard (concat wildcard ".*[^~]\\'"))))
	;; Determine display of group.
	(setq ls-lisp-group-display
	      (if (or (memq 'gid ls-lisp-verbosity)
		      (and (memq 'gid-maybe ls-lisp-verbosity)
			   (if (eq ls-lisp-emulation 'UNIX)
			       (memq ?g switches) ; default off
			     (not (memq ?G switches))))) ; default on
		  ;; Useful concept of group?
		  (cond ((eq system-type 'windows-nt) " None") ; Cygwin ls
			((memq system-type '(macos ms-dos)) " root")
			(t t))))	; use real file group
	(ls-lisp-insert-directory
	 file switches (ls-lisp-time-index switches)
	 wildcard full-directory-p)))))

(defun ls-lisp-insert-directory
  (file switches time-index wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.  This is an internal function
optionally called by the `ls-lisp.el' version of `insert-directory'.
It is called recursively if the -R switch is used.
SWITCHES is a *list* of characters.  TIME-INDEX is the time index into
file-attributes according to SWITCHES.  WILDCARD is nil or an *Emacs
regexp*.  FULL-DIRECTORY-P means a full directory listing is expected."
  (if (or wildcard full-directory-p)
      (let* ((dir (file-name-as-directory file))
	     ;; FJW thinks the following line is now redundant:
	     ;; (default-directory dir)	; so that file-attributes works
	     (file-alist
	      ;; Sort directory (unless U switch given) so that
	      ;; secondary sort follows default order:
	      (directory-files-and-attributes
	       dir nil wildcard (memq ?U switches)))
	     (now (current-time))
	     (sum 0.0)
	     ;; do all bindings here for speed
	     total-line files elt short file-size fil attr)
	(if ls-lisp-skip-inaccessible-files
	    ;; Delete files with no attributes from file-alist.
	    (setq file-alist
		  (delq nil
			(mapcar
			 (lambda (filedata) (if (cdr filedata) filedata))
			 file-alist))))
	(if (memq ?L switches)		; dereference switch
	    (setq file-alist (mapcar 'ls-lisp-dereference file-alist)))
	(setq file-alist
	      (ls-lisp-sort file-alist switches time-index))
	(if (memq ?C switches)		; column (-C) format
	    (ls-lisp-column-format file-alist switches)
	  (setq total-line (cons (point) (car-safe file-alist)))
	  (setq files file-alist)
	  (while files			; long (-l) format
	    (setq elt (car files)
		  files (cdr files)
		  short (car elt)
		  attr (cdr elt)
		  file-size (nth 7 attr))
	    (and attr
		 ;; `sum' coerced to float by initializing it to 0.0.
		 (setq sum (+ sum file-size))
		 (insert (ls-lisp-format short attr file-size
					 switches time-index now))))
	  ;; Insert total size of all files:
	  (save-excursion
	    (goto-char (car total-line))
	    (or (cdr total-line)
		;; Shell says ``No match'' if no files match
		;; the wildcard; let's say something similar.
		(insert "(No match)\n"))
	    (insert
	     (if (memq ?h switches)	; human-readable
		 (ls-lisp-human-total-size sum dir)
	       (format "total %.0f\n" (fceiling (/ sum 1024.0)))))))
	(if (memq ?R switches)
	    ;; List the contents of all directories recursively.
	    ;; cadr of each element of `file-alist' is t for
	    ;; directory, string (name linked to) for symbolic
	    ;; link, or nil.
	    (while file-alist
	      (setq elt (car file-alist)
		    file-alist (cdr file-alist))
	      (when (and (eq (cadr elt) t) ; directory
			 (not (string-match "\\`\\.\\.?\\'" (car elt))))
		(setq elt (expand-file-name (car elt) dir))
		(insert "\n" elt ":\n")
		(ls-lisp-insert-directory
		 elt switches time-index wildcard full-directory-p)))))
    ;; If not full-directory-p, FILE *must not* end in /, as
    ;; file-attributes will not recognize a symlink to a directory,
    ;; so must make it a relative filename as ls does:
    (if (eq (aref file (1- (length file))) ?/)
	(setq file (substring file 0 -1)))
    (let ((fattr (file-attributes file)))
      (if fattr
	  (insert (ls-lisp-format file fattr (nth 7 fattr)
				  switches time-index (current-time)))
	(message "%s: doesn't exist or is inaccessible" file)
	(ding) (sit-for 2)))))		; to show user the message!

(defun ls-lisp-human-total-size (size dir)
  "Return human-readable total size SIZE as a string.
Include free size of directory DIR if possible."
  ;; Based on code from `dired-insert-directory'.  The standard code
  ;; there does not work with human-readable format: the unit
  ;; specifier prevents the total size in the buffer from being
  ;; recognised, but the free size would not be in the right format
  ;; anyway, so this function constructs the whole string.
  (if (and (fboundp 'file-system-info)
	   (setq dir (file-system-info dir)))
      (format "total %s  free %s\n"
	      (ls-lisp-human-size size)
	      (ls-lisp-human-size (nth 2 dir)))
    (format "total %s\n" (ls-lisp-human-size size))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sorting functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst ls-lisp-string-lessp (s1 s2)
  "Return t if string S1 is less than string S2 in lexicographic order.
Case is significant if `ls-lisp-ignore-case' is nil.
Unibyte strings are converted to multibyte for comparison."
  (let ((u (compare-strings s1 0 nil s2 0 nil ls-lisp-ignore-case)))
    (and (numberp u) (< u 0))))

(defun ls-lisp-sort (file-alist switches time-index)
  "Return new FILE-ALIST sorted according to SWITCHES.
SWITCHES is a list of characters.  TIME-INDEX is the time index into
file-attributes according to SWITCHES."
  ;; FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
  (let (pred)				; sort predicate
    (unless (memq ?U switches)		; unsorted
      (setq pred
	    (cond ((memq ?S switches)
		   ;; Sort by size:
		   (lambda (x y)
		     ;; 7th file attribute is file size
		     ;; Make largest file come first
		     (< (nth 7 (cdr y)) (nth 7 (cdr x)))))
		  ((if (memq ?l switches)
		       (memq ?t switches)
		     (or (memq ?c switches) (memq ?u switches)))
		   ;; Sort by time as specified by TIME-INDEX:
		   (lambda (x y)
		     (ls-lisp-time-lessp
		      (nth time-index (cdr y))
		      (nth time-index (cdr x)))))
		  ((memq ?X switches)
		   ;; Sort by extension:
		   (lambda (x y)
		     (ls-lisp-string-lessp
		      (ls-lisp-extension (car x))
		      (ls-lisp-extension (car y)))))
		  ;; Default -- sort by name:
		  ((memq ?L switches)	; must ignore directory --
		   (lambda (x y)	; see `ls-lisp-dereference'
		     (ls-lisp-string-lessp
		      (file-name-nondirectory (car x))
		      (file-name-nondirectory (car y)))))
		  (ls-lisp-ignore-case ; otherwise already sorted by name
		   (lambda (x y)
		     (ls-lisp-string-lessp (car x) (car y)))))))
    ;; Sort if necessary, catching and ignoring unexpected errors:
    (if pred
	(condition-case err
	    (setq file-alist
		  ;; Sort modifies its argument, so
		  ;; copy file-alist in case of error.
		  (sort (copy-sequence file-alist) pred))
	  (error (message "Unsorted (ls-lisp sorting error - %s)"
			  (error-message-string err))
		 (ding) (sit-for 2)))))	; to show user the message!

  (if ls-lisp-dirs-first
  ;; Re-sort directories first, without otherwise changing the
  ;; ordering, and reverse whole list.  cadr of each element of
  ;; `file-alist' is t for directory, string (name linked to) for
  ;; symbolic link, or nil.
      (let (el dirs files)
	(while file-alist
	  (if (eq (cadr (setq el (car file-alist))) t) ; directory
	      (setq dirs (cons el dirs))
	    (setq files (cons el files)))
	  (setq file-alist (cdr file-alist)))
	(setq file-alist
	      (if (memq ?U switches)	; unsorted order is reversed
		  (nconc dirs files)
		(nconc files dirs)))))
  ;; Finally reverse file alist if necessary.
  ;; (eq below MUST compare `(null (memq ...))' to force comparison of
  ;; `t' or `nil', rather than list tails!)
  (if (eq (eq (null (memq ?U switches))	; unsorted order is reversed
	      (null (memq ?r switches))) ; reversed sort order requested
	  ls-lisp-dirs-first)		; already reversed
      (nreverse file-alist)
    file-alist))

(defun ls-lisp-extension (filename)
  "Return extension of FILENAME (ignoring any version extension)
FOLLOWED by null and full filename, SOLELY for full alpha sort."
  ;; Force extension sort order: `no ext' then `null ext' then `ext'
  ;; to agree with GNU ls.
  (concat
   (let* ((i (length filename)) end)
     (if (= (aref filename (1- i)) ?.) ; null extension
	 "\0"
       (while (and (>= (setq i (1- i)) 0)
		   (/= (aref filename i) ?.)))
       (if (< i 0) "\0\0"		; no extension
	 (if (/= (aref filename (1+ i)) ?~)
	     (substring filename (1+ i))
	   ;; version extension found -- ignore it
	   (setq end i)
	   (while (and (>= (setq i (1- i)) 0)
		       (/= (aref filename i) ?.)))
	   (if (< i 0) "\0\0"	; no extension
	     (substring filename (1+ i) end))))
       )) "\0" filename))

;; From Roland McGrath.  Can use this to sort on time.
(defun ls-lisp-time-lessp (time0 time1)
  "Return t if time TIME0 is earlier than time TIME1."
  (let ((hi0 (car time0)) (hi1 (car time1)))
    (or (< hi0 hi1)
	(and (= hi0 hi1)
	     (< (cadr time0) (cadr time1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Formatting functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ls-lisp-dereference (filedata)
  "Dereference file if it is a symlink (ls L switch).
FILEDATA has the form (filename . `file-attributes').  Its `cadr' is t
for directory, string (name linked to) for symbolic link, or nil."
  (let ((file-name (cadr filedata)))	; file-type
    (if (stringp file-name)		; symlink?
	(progn
	  ;; Hide directory part, so that display matches ls but
	  ;; dired can still find the file:
	  (put-text-property 0 (length (file-name-directory file-name))
			     'invisible t file-name)
	  ;; Return new file name and attributes:
	  (cons file-name (file-attributes file-name)))
      ;; Return old file name and attributes:
      filedata)))

(defun ls-lisp-classify (file-name file-attributes)
  "Return FILE-NAME with a character appended indicating file type.
Also, for regular files that are executable, append `*'.
Type is determined from FILE-ATTRIBUTES list (see `file-attributes').
The file type indicators are `/' for directories, `@' for symbolic
links, `|' for FIFOs, `=' for sockets, and nothing for regular files.
\[But FIFOs and sockets are not recognised.]"
  (let ((dir (car file-attributes)))
    ;; dir is t for directory, string (name linked to) for symbolic
    ;; link, or nil.
    (cond (dir
	   (concat file-name (if (eq dir t) "/" "@")))
	  ((string-match "x" (nth 8 file-attributes))
	   (concat file-name "*"))
	  (t file-name))))

(defun ls-lisp-column-format (file-alist switches)
  "Insert the file names (only) in FILE-ALIST into the current buffer.
Format in columns, sorted vertically, following GNU ls -C.
Responds to the window width as ls should but may not!
SWITCHES gives the full switch list."
  (let (files fmt ncols collen (nfiles 0) (colwid 0))
    ;; Count number of files as `nfiles', build list of filenames as
    ;; `files', and find maximum filename length as `colwid':
    (let (file-name end len)
      (while file-alist
	(setq nfiles (1+ nfiles)
	      file-name (caar file-alist)
	      end nil)
	;; Optionally hide final .lnk extension:
	(when (and ls-lisp-hide-.lnk
		   (string-match "\\.lnk\\'" file-name))
	  (setq end (length file-name))
	  (put-text-property (- end 4) end 'invisible t file-name))
	(if (memq ?F switches)		; classify switch
	    (setq file-name (ls-lisp-classify file-name (cdar file-alist))))
	(setq files (cons file-name files)
	      file-alist (cdr file-alist)
	      len (length file-name))
	(if end (setq len (- len 4)))	; .lnk hidden
	(if (> len colwid) (setq colwid len))))
    (setq files (nreverse files)
	  colwid (+ 2 colwid)		; 2 character column gap
	  fmt (format "%%-%ds" colwid)	; print format
	  ncols (/ (window-width) colwid) ; no of columns
	  collen (/ nfiles ncols))	; floor of column length
    (if (> nfiles (* collen ncols)) (setq collen (1+ collen)))
    ;; Output the file names in columns, sorted vertically:
    (let ((i 0) j)
      (while (< i collen)
	(setq j i)
	(while (< j nfiles)
	  (insert (format fmt (nth j files)))
	  (setq j (+ j collen)))
	;; FJW: This is completely unnecessary, but I don't like
	;; trailing white space...
	(delete-region (point) (progn (skip-chars-backward " \t") (point)))
	(insert ?\n)
	(setq i (1+ i))))))

(defun ls-lisp-format (file-name file-attr file-size switches time-index now)
  "Format one line of long ls output for file FILE-NAME.
FILE-ATTR and FILE-SIZE give the file's attributes and size.
SWITCHES, TIME-INDEX and NOW give the full switch list and time data."
  (let ((file-type (nth 0 file-attr))
	;; t for directory, string (name linked to)
	;; for symbolic link, or nil.
	(drwxrwxrwx (nth 8 file-attr)))	; attribute string ("drwxrwxrwx")
    (concat (if (memq ?i switches)	; inode number
		(format " %6d" (nth 10 file-attr)))
	    ;; nil is treated like "" in concat
	    (if (memq ?s switches)	; size in K
		(format " %4.0f" (fceiling (/ file-size 1024.0))))
	    drwxrwxrwx			; attribute string
	    (if (memq 'links ls-lisp-verbosity)
		(format " %3d" (nth 1 file-attr))) ; link count
	    ;; Numeric uid/gid are more confusing than helpful;
	    ;; Emacs should be able to make strings of them.
	    ;; They tend to be bogus on non-UNIX platforms anyway so
	    ;; optionally hide them.
	    (if (memq 'uid ls-lisp-verbosity)
		;; (user-login-name uid) works on Windows NT but not
		;; on 9x and maybe not on some other platforms, so...
		(let ((uid (nth 2 file-attr)))
		  (if (= uid (user-uid))
		      (format " %-8s" (user-login-name))
		    (format " %-8d" uid))))
	    (if ls-lisp-group-display
		;; Useful concept of group?
		(if (stringp ls-lisp-group-display) ; e.g. " root"
		    ls-lisp-group-display
		  (let* ((gid (nth 3 file-attr))
			 (group (user-login-name gid)))
		    (if group
			(format " %-8s" group)
		      (format " %-8d" gid)))))
	    (if (memq ?h switches)	; human-readable
		(format " %8s" (ls-lisp-human-size file-size))
	      (format (if (floatp file-size) " %8.0f" " %8d") file-size))
	    " "
	    (ls-lisp-format-time (nth time-index file-attr) now)
	    " "
	    (if (stringp file-type)	; symbolic link
		;; Optionally hide final .lnk extension:
		(let (end)
		  (when (and ls-lisp-hide-.lnk
			     (string-match "\\.lnk\\'" file-name))
		    (setq end (length file-name))
		    (put-text-property (- end 4) end 'invisible t file-name))
		  (concat file-name " -> "
			  (if (memq ?F switches) ; classify switch
			      (ls-lisp-classify file-type
						(file-attributes file-type))
			    file-type)))
	      (if (memq ?F switches) ; classify switch
		  (ls-lisp-classify file-name file-attr)
		file-name))
	    "\n")))

(defun ls-lisp-human-size (file-size)
  "Return FILE-SIZE as a string in human-readable format using k, M, G, T."
  ;; Functionality suggested by Sergej Malinovski <sergej at nospam.dk>
  ;; Format must match regexp used in dired: ".*[0-9][kMGTPEZY]?"
  (format (cond ((< file-size 1024) (if (floatp file-size) "%.0f" "%d"))
		((< (setq file-size (/ file-size 1024.0)) 10.0) "%.1fk")
		((< file-size 1024.0) "%.0fk")
		((< (setq file-size (/ file-size 1024.0)) 10.0) "%.1fM")
		((< file-size 1024.0) "%.0fM")
		((< (setq file-size (/ file-size 1024.0)) 10.0) "%.1fG")
		((< file-size 1024.0) "%.0fG")
		((< (setq file-size (/ file-size 1024.0)) 10.0) "%.1fT")
		(t "%.0fT"))		; enough for now!
	  file-size))

(defun ls-lisp-time-index (switches)
  "Return time index into file-attributes according to ls SWITCHES list.
If u then last access else if c then last mode change else last modtime."
  (cond ((memq ?u switches) 4)		; last access
	((memq ?c switches) 6)		; last mode change
	(t 5)))				; default: last modtime

(defun ls-lisp-format-time (time now)
  "Format time TIME for file relative to current time NOW.
A times is represented as a list of two 16-bit integers: (high low).
Use the same method as ls to decide whether to show time-of-day or year,
depending on distance between file date and NOW, except that cutoffs are
customizable\; see `ls-lisp-past-cutoff' and `ls-lisp-future-cutoff'.
All ls time options, namely c, t and u, are handled."
  ;; File times are represented as lists of two integers.
  ;; First integer has high-order 16 bits of time, second has low 16 bits.
  ;; Emacs integers are (usually) 28 bits long, so a 32-bit time may
  ;; overflow.  Convert to float to avoid this; IEEE float precision
  ;; of about 15 significant figures is better than a 32-bit integer!
  (let ((diff (+ (* (float (- (car time) (car now))) 65536.0)
		 (float (- (cadr time) (cadr now))))))
    ;; Cutoff user options could be invalid, so...
    (condition-case nil
	(format-time-string
	 (if (and (<= ls-lisp-past-cutoff diff)
		  (<= diff ls-lisp-future-cutoff))
	     "%b %e %H:%M"
	   "%b %e  %Y")
	 time)
      (error "Unk  0  0000"))))

(defun ls-lisp-wildcard-to-regexp (wildcard)
  "Given a shell file name pattern WILDCARD, return an equivalent regexp.
The generated regexp will match a filename iff the filename
matches that wildcard according to shell rules.
If `ls-lisp-support-shell-wildcards' is '{} then also expand `{a,b,...}'
like bash, allowing arbitrary nesting.  To use `{', `,' and `}' for
any other purpose they must be escaped by a preceding `\\'."
  ;; Shell wildcards should match the entire filename,
  ;; not its part.  Make the regexp say so.
  (concat "\\`" (ls-lisp-wildcard-to-regexp-1 wildcard) "\\'"))

(defvar i)		       ; bound in ls-lisp-wildcard-to-regexp-1

(defun ls-lisp-wildcard-to-regexp-1 (wildcard)
  "As `ls-lisp-wildcard-to-regexp' (WILDCARD) but without the \\`...\\'.
Called recursively by `ls-lisp-wildcard-to-regexp-{}'."
  (let* ((i (string-match "[[.*+\\^$?{]" wildcard))
	 ;; Copy the initial run of non-special characters.
	 (result (substring wildcard 0 i))
	 (len (length wildcard)))
    ;; If no special characters, we're almost done.
    (if i
	(while (< i len)
	  (let ((ch (aref wildcard i))
		j)
	    (setq
	     result
	     (concat result
		     (cond
		      ((and (eq ch ?\[)
			    (< (1+ i) len)
			    (eq (aref wildcard (1+ i)) ?\]))
		       "\\[")
		      ((eq ch ?\[)   ; [...] maps to regexp char class
		       (progn
			 (setq i (1+ i))
			 (concat
			  (cond
			   ((eq (aref wildcard i) ?!) ; [!...] -> [^...]
			    (progn
			      (setq i (1+ i))
			      (if (eq (aref wildcard i) ?\])
				  (progn
				    (setq i (1+ i))
				    "[^]")
				"[^")))
			   ((eq (aref wildcard i) ?^)
			    ;; Found "[^".  Insert a `\0' character
			    ;; (which cannot happen in a filename)
			    ;; into the character class, so that `^'
			    ;; is not the first character after `[',
			    ;; and thus non-special in a regexp.
			    (progn
			      (setq i (1+ i))
			      "[\000^"))
			   ((eq (aref wildcard i) ?\])
			    ;; I don't think `]' can appear in a
			    ;; character class in a wildcard, but
			    ;; let's be general here.
			    (progn
			      (setq i (1+ i))
			      "[]"))
			   (t "["))
			  (prog1      ; copy everything upto next `]'.
			      (substring wildcard i
					 (setq j (string-match
						  "]" wildcard i)))
			    (setq i (if j (1- j) (1- len)))))))
		      ((eq ch ?.)  "\\.")
		      ((eq ch ?*)  "[^\000]*")
		      ((eq ch ?+)  "\\+")
		      ((eq ch ?^)  "\\^")
		      ((eq ch ?$)  "\\$")
		      ((eq ch ?\\)
		       (setq i (1+ i))
		       (if (< i len)
			   (concat "\\" (char-to-string (aref wildcard i)))
			 "\\\\"))
		      ((eq ch ??)  "[^\000]")
		      ((and (eq ch ?{)	; {a,b,...} -> \(a\|b\|...\)
			    (eq ls-lisp-support-shell-wildcards '{}))
		       (ls-lisp-wildcard-to-regexp-{} wildcard))
		      (t (char-to-string ch)))))
	    (setq i (1+ i)))))
    result))

(defun ls-lisp-wildcard-to-regexp-{} (wildcard)
  "Given a bash `{a,b,...}'-pattern, return an equivalent regexp.
To be called by `ls-lisp-wildcard-to-regexp-1' only!  The pattern begins at
index i in string WILDCARD.  The variable i is fluid-bound."
  ;; [Note that ls-lisp-wildcard-to-regexp-find-,} start index must allow
  ;; for a preceding character [^\], and so is i rather than (1+ i), etc.]
  ;; Find first comma:
  (let (s j ii)
    (if (not (and (setq j (ls-lisp-wildcard-to-regexp-find-,} wildcard i))
		  (eq (aref wildcard j) ?,)))
	"{"				; does not match {a,...}
      (setq s (concat "\\("	   ; Emacs 21: use shy group "\\(?:" ?
		      (ls-lisp-wildcard-to-regexp-1
		       (substring wildcard (1+ i) j)))
	    ii j)
      ;; Find subsequent commas or closing brace:
      (while (and (setq j (ls-lisp-wildcard-to-regexp-find-,} wildcard ii))
		  (eq (aref wildcard j) ?,))
	(setq s (concat s "\\|"
			(ls-lisp-wildcard-to-regexp-1
			 (substring wildcard (1+ ii) j)))
	      ii j))
      ;; Found closing brace or failed:
      (cond
       (j (setq s (concat s "\\|"
			  (ls-lisp-wildcard-to-regexp-1
			   (substring wildcard (1+ ii) j)))
		i j)			; update i
	  (concat s "\\)"))		; return regexp
       (t "{"))				; does not match {a,...}
      )))

(defun ls-lisp-wildcard-to-regexp-find-,} (s i)
  "Return index of first top-level `,' or `}' after `{' in string S at index I.
Allow nested `{...}' and ignore characters escaped by a preceding `\\'."
  (setq i (string-match "[^\\][{,}]" s i))
  (while (and i (eq (aref s (1+ i)) ?{))
    (setq i (ls-lisp-wildcard-to-regexp-skip-{} s (1+ i)))
    (if i (setq i (string-match "[^\\][{,}]" s i))))
  (and i (1+ i)))

(defun ls-lisp-wildcard-to-regexp-skip-{} (s i)
  "Return index of `}' matching `{' in string S at index I.
Allow nested `{...}' and ignore characters escaped by a preceding `\\'."
  (setq i (string-match "[^\\][{}]" s i))
  (while (and i (eq (aref s (1+ i)) ?{))
    (setq i (ls-lisp-wildcard-to-regexp-skip-{} s (1+ i)))
    (if i (setq i (string-match "[^\\][{}]" s i))))
  (and i (1+ i)))

(provide 'ls-lisp)

;;; ls-lisp.el ends here
