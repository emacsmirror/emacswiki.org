;;; w32-symlinks.el --- MS Windows symbolic link (shortcut) support

;; Copyright (C) 2002, 2003 Francis J. Wright, 2005 Lars Hansen

;; Author: Francis J. Wright <F.J.Wright at qmul.ac.uk>
;; Last-Updated: 22-11-2005 18:00 UTC
;; By: Lars Hansen <larsh at soem dot dk>
;; URL: http://www.emacswiki.org/emacs/w32-symlinks.el
;; Keywords: convenience, files, unix

;; This file is not part of GNU Emacs.

;; w32-symlinks is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; w32-symlinks is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file is intended to be used with NTEmacs 21, i.e. GNU Emacs 21
;; compiled as a native Microsoft Windows application and running on
;; Windows.  It should cause no harm on other platforms and might be
;; useful when accessing a Windows file system from another OS, but I
;; have not tested such use.  It provides support for symbolic links
;; on Microsoft Windows platforms by allowing Emacs to handle Windows
;; shortcut files transparently as symbolic links in the same way that
;; Windows itself does, by making .lnk files names "magic".

;; It contains functions to parse Windows .lnk "shortcut" (and also
;; obsolete Cygwin-style "symlink") files, entirely in Lisp.  It allows
;; `dired' to follow symbolic links when using either ls-lisp (the
;; default) or an external Cygwin ls program.  When run on Windows, it
;; also implements the missing `dired-do-symlink' command to make
;; symbolic links.

;; INSTALLATION ======================================================

;; Put this file (w32-symlinks.el) somewhere in your load-path and
;; byte-compile it.  Then choose one of the following options to load
;; w32-symlinks.  Note that, by default, w32-symlinks supports dired
;; only; see option 3 below.

;; 1. To provide symlink support for dired only, using the STANDARD
;;    preloaded version of the NTEmacs 21 or later ls-lisp library,
;;    put this in your .emacs:

;;    (add-hook 'dired-load-hook
;; 	        (lambda () (require 'w32-symlinks)))

;; 2. To provide symlink support for dired only, using a version of
;;    GNU Emacs other than NTEmacs, or using the latest version of the
;;    ls-lisp library from my web site (which must first be installed
;;    as per its instructions), put this in your .emacs:

;;    (add-hook 'dired-load-hook
;;   	        (lambda ()
;; 	         (load "ls-lisp")
;; 	         (require 'w32-symlinks)))

;; 3. To provide symlink support for GNU Emacs 21 in general
;;    (including dired), put this in your .emacs:

;;    (require 'w32-symlinks)

;;    Also execute both the above sexp (by putting point at the end of
;;    the sexp and pressing C-x C-e, which runs the command
;;    eval-last-sexp) and the following sexp

;;    (customize-option 'w32-symlinks-handle-shortcuts)

;;    Turn the option on and save the setting for future sessions.

;;; History:

;; Originally written in April 2000 as part of an enhanced version of
;; ls-lisp for Emacs 21, but separated and not distributed with Emacs.
;; The "magic" file name handler code was added in September 2002.

;; Changes by Lars Hansen <larsh at soem dot dk> on 2005-11-22 to
;; file marked "Time-stamp: <04 May 2003>" found on
;; http://centaur.maths.qmw.ac.uk/Emacs/:

;; 1. Use `w32-symlinks-operate-on-target' as handler for `file-regular-p'.
;; 2. Add handler for `file-symlink-p'.
;; 3. Add handler for `file-attributes' that updates file-modes.
;; 4. Add handler for `directory-files-and-attributes'.
;; 5. Rename `w32-symlinks-parse-symlink' from `ls-lisp-parse-symlink'.
;; 6. Return value from `file-symlink-p' in advice `file-symlink-p-advice'.

;;; Code:

(defgroup w32-symlinks nil
  "Handling of Windows symbolic links (both Microsoft and Cygwin)."
  :group 'dired
  :group 'ls-lisp)

(defcustom w32-symlinks-dired-support '(parse-shortcuts)
  "*A list of Windows symbolic link types that `dired' should support.
It should contain none or more of the following symbols:
   parse-shortcuts, parse-old-symlinks, make-old-symlinks.

They indicate respectively standard Microsoft Windows shortcut (.lnk)
and obsolete Cygwin-style symlink files.  Current versions of Cygwin
use standard .lnk files by default, so the default is to include only
the option parse-shortcuts.

Parsing obsolete symlink files is slow because NTEmacs cannot access
the system attribute, so all files must be checked.  Include the
option parse-old-symlinks only if you use either obsolete symlinks
with Cygwin or the `dired-do-symlink' command without WSH or Cygwin.

The option make-old-symlinks affects only the `dired-do-symlink'
command\; include it only if either your ln command makes obsolete
Cygwin-style symlinks or you do not have an ln command.  It is used
only to ensure that `dired-do-symlink' updates the buffer correctly.

NB: Support for Windows shortcuts outside `dired' is controlled by
`w32-symlinks-handle-shortcuts'.

----------------------------------------------------------------------

The following is copied from \"The Cygwin FAQ\", available in a Cygwin
installation in the directory /usr/doc/cygwin-doc-1.1 or on the web at
http://cygwin.com/faq/ under the following section heading:

How do symbolic links work?

Cygwin knows of two ways to create symlinks.

The old method is the only valid one up to but not including version
1.3.0.  If it's enabled (from 1.3.0 on by setting `nowinsymlinks' in
the environment variable CYGWIN) Cygwin generates link files with a
magic header.  When you open a file or directory that is a link to
somewhere else, it opens the file or directory listed in the magic
header.  Because we don't want to have to open every referenced file
to check symlink status, Cygwin marks symlinks with the system
attribute.  Files without the system attribute are not checked.
Because remote samba filesystems do not enable the system attribute by
default, symlinks do not work on network drives unless you explicitly
enable this attribute.

The new method which is introduced with Cygwin version 1.3.0 is
enabled by default or if `winsymlinks' is set in the environment
variable CYGWIN.  Using this method, Cygwin generates symlinks by
creating Windows shortcuts.  Cygwin created shortcuts have a special
header (which is in that way never created by Explorer) and the R/O
attribute set.  A DOS path is stored in the shortcut as usual and the
description entry is used to store the POSIX path.  While the POSIX
path is stored as is, the DOS path has perhaps to be rearranged to
result in a valid path.  This may result in a divergence between the
DOS and the POSIX path when symlinks are moved crossing mount points.
When a user changes the shortcut, this will be detected by Cygwin and
it will only use the DOS path then.  While Cygwin shortcuts are shown
without the \".lnk\" suffix in `ls' output, non-Cygwin shortcuts are
shown with the suffix.  However, both are treated as symlinks.

Both, the old and the new symlinks can live peacefully together since
Cygwin treats both as symlinks regardless of the setting of
`(no)winsymlinks' in the environment variable CYGWIN."
  :type '(set (const :tag "Parse shortcuts (.lnk files)" parse-shortcuts)
	      (const :tag "Parse obsolete Cygwin symlinks" parse-old-symlinks)
	      (const :tag "Make obsolete Cygwin symlinks" make-old-symlinks))
  :group 'w32-symlinks)

(defcustom w32-symlinks-shortcut-target 'expand
  "*Determine how to return Windows shortcut target filenames.
This applies only to ls-lisp and hence `dired', not more generally.
Value must be a symbol.  The options are:
   'expand -- expand to an absolute canonical filename
   non-nil -- just convert \\ to /
   nil     -- do not change"
  :type '(choice (const :tag "Expand to absolute" expand)
		 (const :tag "Convert \\ to /" t)
		 (const :tag "Leave unchanged" nil))
  :group 'w32-symlinks)

(defconst w32-symlinks-to-follow
  '((					; Simple commands:
     dired-advertised-find-file
     dired-backup-diff
     dired-diff
     dired-display-file
     dired-do-byte-compile
     dired-do-chgrp
     dired-do-chmod
     dired-do-chown
     dired-do-compress
     dired-do-copy
     dired-do-copy-regexp
     dired-do-hardlink
     dired-do-hardlink-regexp
     dired-do-load
     dired-do-print
     dired-do-shell-command
     dired-do-symlink
     dired-do-symlink-regexp
     dired-find-file
     dired-find-file-other-window
     dired-maybe-insert-subdir
     dired-mouse-find-file
     dired-mouse-find-file-other-window
     dired-view-file
     woman-dired-find-file
     )
    .
    (					; Complex commands:
     dired-do-query-replace
     dired-do-search
     ))
  "Cons of lists of `dired-mode' commands that need target of a symlink.
The `car' consists of simple commands and the `cdr' of complex commands.
Complex commands are those that go into the variable `command-history'.
All other `dired-mode' commands receive the symlink itself, as per default.
Does not include w32-shellex commands, which are handled specially.")

(defun w32-symlinks-parse-symlink (file-name)
  "Optionally parse FILE-NAME as a MS Windows symlink file, if possible."
  ;; This function redefines a stub in ls-lisp.
  (and
   w32-symlinks-dired-support
   (condition-case nil
       (or (and (memq 'parse-shortcuts w32-symlinks-dired-support)
		(string-match "\\.lnk\\'" file-name)
		(w32-symlinks-parse-shortcut file-name))
	   (and (memq 'parse-old-symlinks w32-symlinks-dired-support)
		(w32-symlinks-parse-old-Cygwin file-name)))
     (error nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parse Microsoft Windows shortcut (.lnk) and obsolete Cygwin-style
;; symbolic link files, and make dired follow symlinks when
;; appropriate.

(defun w32-symlinks-buffer-substring-as-int (start length)
  "Return contents of part of the current buffer as an unsigned integer.
START is a character position\; LENGTH specifies the length of the
integer in bytes and should be 1, 2 or 4.
Assumes byte order is low to high (little-endian)."
  (let ((idx (+ start length))
	(int 0))
    ;; Base (radix) using unsigned char digits is 2^8 = 256.
    (while (>= (setq idx (1- idx)) start)
      (setq int (+ (* 256 int) (char-after idx))))
    int))

(defun w32-symlinks-parse-shortcut (file)
  "Return file or directory referenced by MS Windows shortcut (.lnk) FILE.
Return nil if the file cannot be parsed."
  ;; Based on "The Windows Shortcut File Format" as
  ;; reverse-engineered by Jesse Hager <jessehager at iname.com>
  ;; available from http://www.wotsit.org/download.asp?f=shortcut.
  (with-temp-buffer
    (let ((inhibit-file-name-handlers
	   (cons 'w32-symlinks-file-name-handler
		 (and (eq inhibit-file-name-operation 'insert-file-contents)
		      inhibit-file-name-handlers))))
      (insert-file-contents-literally file)) ; Eli Zaretskii
    (and
     ;; Parse the File Header Table.
     ;; Check for Shell Link identifier (4 bytes)
     ;; followed by Shell Link GUID (16 bytes):
     (string= (buffer-substring 1 21)  ; otherwise not a shortcut file
	      "L\0\0\0\ \x01\x14\x02\0\0\0\0\0\xC0\0\0\0\0\0\0\x46")
     ;; Get the main flags dword at offset 14h.
     (let ((flags (w32-symlinks-buffer-substring-as-int (+ (point) ?\x14) 4))
	   target)
       ;; Skip to end of Header:
       (forward-char ?\x4C)
       (if (= (logand flags 1) 1)
	   ;; Flag 0 (2^0=1) set means Shell Item Id List present, so
	   ;; skip it.  The list length is the first word, which must
	   ;; also be skipped:
	   (forward-char
	    (+ 2 (w32-symlinks-buffer-substring-as-int (point) 2))))
       (if (= (logand flags 2) 2)
	   ;; Flag 1 (2^1=2) set means File Location Info Table
	   ;; present, so parse it.
	   (progn
	     ;; The full file pathname is (generally) stored in two
	     ;; pieces: a head depending on whether the file is on a
	     ;; local or network volume and a remaining pathname tail.
	     ;; Get and check the volume flags dword at offset 8h:
	     (setq flags (w32-symlinks-buffer-substring-as-int
			  (+ (point) ?\x8) 4))
	     (if (/= (logand flags 3) 0) ; Must have bit 0 or 1 set.
		 (let ((head		; Get local or network
			(save-excursion ; pathname head.
			  ;; If bit 0 then local else network:
			  (if (setq flags (= (logand flags 1) 1))
			      ;; Go to the base pathname on the local
			      ;; system at the offset specified as a
			      ;; dword at offset 10h:
			      (forward-char
			       (w32-symlinks-buffer-substring-as-int
				(+ (point) ?\x10) 4))
			    ;; Go to the network volume table at the
			    ;; offset specified as a dword at offset 14h:
			    (forward-char
			     (w32-symlinks-buffer-substring-as-int
			      (+ (point) ?\x14) 4))
			    ;; Go to the network share name at offset 14h:
			    (forward-char ?\x14))
			  (buffer-substring (point)
					    (1- (search-forward "\0")))))
		       (tail	     ; Get the remaining pathname tail
			(progn		; specified as a dword at
			  (forward-char	; offset 18h.
			   (w32-symlinks-buffer-substring-as-int
			    (+ (point) ?\x18) 4))
			  (buffer-substring (point)
					    (1- (search-forward "\0"))))))
		   (setq target
			 ;; Network share name needs trailing \ added:
			 (concat head
				 (unless (or flags (string= tail "")) "\\")
				 tail)))))
	 ;; Otherwise, continue parsing...
	 ;; NB: Shortcuts generated using WSH seem to use Unicode.
	 ;; May be flag bit 7 indicates use of Unicode (other than in
	 ;; the Shell Item Id List), but I have no confirmation of
	 ;; that, so for now I use the hack below to detect Unicode.
	 (if (= (logand flags 4) 4)
	     ;; Flag 2 (2^2=4) set means Description String present,
	     ;; so skip it.  The string length is the first word,
	     ;; which must also be skipped.
	     (let ((len (w32-symlinks-buffer-substring-as-int (point) 2)))
	       (forward-char 2)		; skip length word
	       (forward-char
		(if (eq (char-after (1+ (point))) 0) ; assume unicode
		    (* len 2)
		  len))))
	 (if (= (logand flags 8) 8)
	     ;; Flag 3 (2^3=8) set means Relative Path String present,
	     ;; so parse it.  The string length is the first word.
	     (let ((len (w32-symlinks-buffer-substring-as-int (point) 2)))
	       (forward-char 2)		; skip length word
	       (setq target
		     (if (eq (char-after (1+ (point))) 0) ; assume unicode
			 (w32-symlinks-unicode-to-ascii
			  (buffer-substring (point) (+ (point) (* len 2))))
		       (buffer-substring (point) (+ (point) len)))))))
       (when target
	 (setq target (decode-coding-string
		       target file-name-coding-system))	; Eli Zaretskii
	 (cond ((eq w32-symlinks-shortcut-target 'expand) ; Canonicalize
		;; Full expansion RELATIVE TO THE SHORTCUT DIRECTORY
		;; is NECESSARY in magic filename handlers!
		(let ((inhibit-file-name-handlers
		       (cons 'w32-symlinks-file-name-handler
			     (and (eq inhibit-file-name-operation
				      'expand-file-name)
				  inhibit-file-name-handlers)))
		      (inhibit-file-name-operation 'expand-file-name))
		  (expand-file-name target (file-name-directory file))))
	       (w32-symlinks-shortcut-target ; Just convert \ to /
		(let ((i (length target)))
		  (while (>= (setq i (1- i)) 0)
		    (if (eq (aref target i) ?\\) (aset target i ?/))))
		target)
	       (t target)))
       ))))

(defsubst w32-symlinks-unicode-to-ascii-error (cond)
  "If COND then report input string format error."
  (if cond
      (error "Input string to w32-symlinks-unicode-to-ascii not UTF16 ASCII")))

(defun w32-symlinks-unicode-to-ascii (in)
  "Convert Windows Unicode 8-bit ASCII unibyte string IN to 8-bit ASCII.
\"Windows Unicode\" means UTF-16LE, since x86 processors are Little Endian.
Hence the bytes of IN are char, NULL, char, NULL, ... and this
function removes all the NULL bytes.
Temporary hack (for Emacs 21.2) until a better method is available."
  (let ((len (length in)) (i 0) out)
    (w32-symlinks-unicode-to-ascii-error (= (% len 2) 1))
    (setq out (make-string (/ len 2) ?\0))
    (while (< i len)
      (aset out (/ i 2) (aref in i))
      (w32-symlinks-unicode-to-ascii-error (not (eq (aref in (1+ i)) ?\0)))
      (setq i (+ i 2)))
    out))

(defun w32-symlinks-parse-old-Cygwin (file)
  "Return file or directory referenced by obsolete Cygwin symbolic link FILE.
Return nil if the file cannot be parsed."
  (with-temp-buffer
    ;; Read at most the first 512 bytes for efficiency:
    (insert-file-contents-literally file nil 0 511) ; Eli Zaretskii
    (when (looking-at "!<symlink>\\(.+\\)\0")
      (setq file (match-string-no-properties 1))
      (decode-coding-string file file-name-coding-system) ; Eli Zaretskii
      )))

(defun w32-symlinks-Cyg-to-Win (file)
  "Convert an absolute filename FILE from Cygwin to Windows form."
  (if (eq (aref file 0) ?/)
      ;; Try to use Cygwin mount table via `cygpath.exe'.
      (condition-case nil
	  (with-temp-buffer
	    ;; cygpath -w file
	    (call-process "cygpath" nil t nil "-w" file)
	    (buffer-substring 1 (buffer-size)))
	(error
	 ;; Assume no `cygpath' program available.
	 ;; Hack /cygdrive/x/ or /x/ or (obsolete) //x/ to x:/
	 (when (string-match "\\`\\(/cygdrive\\|/\\)?/./" file)
	   (if (match-string 1)		; /cygdrive/x/ or //x/ -> /x/
	       (setq file (substring file (match-end 1))))
	   (aset file 0 (aref file 1))	; /x/ -> xx/
	   (aset file 1 ?:))		; xx/ -> x:/
	 file))
    file))

(defadvice dired-get-filename
  (around dired-get-filename-advice activate compile)
  "Return source or target of symlink as appropriate.
Always return source if calling command had prefix argument.
Return target for dired commands in `w32-symlinks-to-follow' unless
called by shellex\; otherwise return source."
  ;; `dired-get-filename' always returns the symlink itself
  ;; but most Windows commands cannot follow symlinks!
  ;; Easy to find target, but not source so let original function do it.
  (if (and (not current-prefix-arg)
	   (save-excursion		; symbolic link?
	     (beginning-of-line)
	     (looking-at ".+\\(\\.lnk\\)? -> \\(.+\\)"))
	   ;; Symbolic link -- return target?
	   (or
	    ;; Always apply these simple commands to the target file:
	    (memq this-command (car w32-symlinks-to-follow))
	    ;; Always apply these complex commands to the target file:
	    (memq (caar command-history) (cdr w32-symlinks-to-follow))
	    ;; But shellex handles MS Windows shortcuts directly:
	    (and (eq this-command 'w32-shellex-dired-on-objects)
		 (not (or (match-beginning 1) ; .lnk
			  ;; if using Cygwin ls then `->' => .lnk
			  ls-lisp-use-insert-directory-program)))
	    ))
      ;; Return target:
      (setq ad-return-value
	    (w32-symlinks-Cyg-to-Win (match-string-no-properties 2)))
    ;; Not symbolic link or source required:
    ad-do-it))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make symlinks on NTEmacs (Microsoft Windows only).

;; The function `make-symbolic-link' does not exist in NTEmacs so
;; emulate it in ELisp; it is implemented in the kernel on other
;; platforms that provide suitable system calls.  Instead, use either
;; Windows Script Host, the Cygwin ln program or simply generate
;; obsolete Cygwin-style symlink files.

;; This code must not be run on any other platform!
(eval-and-compile
  (when (eq system-type 'windows-nt)

    (defcustom w32-symlinks-make-using nil
      "*Determine how `make-symbolic-link' should make symbolic links.
Value must be a symbol.  The options are:
   'wsh -- use Windows Script Host (which was not included in
versions of Windows before 98, but is available free from
msdn.microsoft.com/scripting.)  See also `w32-symlinks-ln-script'.
   'ln  -- use the Cygwin (cygwin.com) port of the GNU ln program.
See also `w32-symlinks-ln-program'.
   'old -- generate obsolete Cygwin-style symlink files.
    nil -- the default -- try in succession each of wsh, ln and old."
      :link '(url-link :tag "Microsoft Scripting Website"
		       "http://msdn.microsoft.com/scripting/")
      :link '(url-link :tag "Cygwin Website" "http://cygwin.com/")
      :type '(choice (const :tag "Windows Script Host" wsh)
		     (const :tag "Cygwin ln" ln)
		     (const :tag "Obsolete Cygwin Symlinks" old)
		     (const :tag "Automatic" nil))
      :group 'w32-symlinks)

    (defcustom w32-symlinks-ln-script
      (substitute-in-file-name "$EMACSPATH/w32-symlinks-ln-s.js")
      "*Absolute filename for the ln script used by `make-symbolic-link'.
There are no constraints on the filename.  The script is automatically
created if necessary and then executed by Windows Script Host.
You can do completion with \\<widget-field-keymap>\\[widget-complete]."
      :type 'file
      :group 'w32-symlinks)

    (defcustom w32-symlinks-ln-program "ln"
      "*Absolute or relative name of the ln program used by `make-symbolic-link'.
Absolute filename is necessary if the program directory is not in `exec-path'.
You can do completion with \\<widget-field-keymap>\\[widget-complete]."
      :type 'file
      :group 'w32-symlinks)

    (defvar w32-symlinks-check-ln-script nil
      "True if `w32-symlinks-ln-script' written this Emacs session.")

    (defun w32-symlinks-check-ln-script ()
      "Write a new copy of `w32-symlinks-ln-script' if necessary.
Normally this happens at most once per Emacs session, when it is first
used, to ensure that it is current."
      (or
       (and
	(file-exists-p w32-symlinks-ln-script)
	w32-symlinks-check-ln-script)
       (with-temp-file w32-symlinks-ln-script
	 (insert "\
// \"ln -s\" implemented in JScript for Microsoft Windows.
// Usage: ln-s oldname newname
//   oldname *MUST* be an ABSOLUTE pathname
//   newname must explicitly end with .lnk
//   (pathnames can be in UNIX format)
// Constructs newname.lnk as a shortcut to oldname.

// Author: Francis J. Wright <F.J.Wright at qmul.ac.uk>
// URL: http://centaur.maths.qmul.ac.uk/Emacs/

// This file was written and is primarily intended to be called
// automatically by the NTEmacs package w32-symlinks.el.
// It can be run explicitly from a command prompt, but it is
// INFLEXIBLE and has NO ERROR CHECKING!

// Requires Windows Script Host, which was not included in versions
// of Windows before 98, but is available free from
// http://msdn.microsoft.com/scripting/.

args = WScript.Arguments;
shell = WScript.CreateObject(\"WScript.Shell\");
link = shell.CreateShortcut(args(1));  // newname
link.Description = \"Generated by NTEmacs w32-symlinks\";
link.TargetPath = args(0);  // oldname
link.Save();")
	 (setq w32-symlinks-check-ln-script t))))

    (eval-when-compile
      (require 'dired-aux))

    (defun make-symbolic-link (file newname &optional ok-if-already-exists)
      "Give FILE symbolic link NEWNAME.  Both args strings.
Signals a `file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with \\[execute-extended-command].
Depending on the value of `w32-symlinks-make-using', use WSH, an
external ln program, or generate an obsolete Cygwin-style symlink.
The latter will not have the system attribute set and so will only be
parsed by w32-symlinks.el."
      ;; Modelled on `add-name-to-file'
      (interactive "fMake symbolic link to file: \nFName for link to %s: \np")
      (if (or (not (file-exists-p newname))
	      (if (numberp ok-if-already-exists)
		  (yes-or-no-p
		   (format
		    "File %s already exists; make it a symlink anyway? "
		    newname))
		ok-if-already-exists)
	      (signal 'file-already-exists
		      (list "File already exists" newname)))
	  (cond ((eq w32-symlinks-make-using 'wsh)
		 (w32-symlinks-make-using-wsh file newname))
		((eq w32-symlinks-make-using 'ln)
		 (w32-symlinks-make-using-wsh file newname))
		((eq w32-symlinks-make-using 'old)
		 (w32-symlinks-make-using-old file newname))
		(t			; default: try each in turn...
		 (condition-case nil
		     (w32-symlinks-make-using-wsh file newname)
		   (error
		    (condition-case nil
			(w32-symlinks-make-using-ln file newname)
		      (error
		       (w32-symlinks-make-using-old file newname)))))))))

    (defun w32-symlinks-make-using-wsh (file newname)
      "Use Windows Script Host to construct a shortcut to FILE called NEWNAME."
      ;; ln -s file newname; newname *MUST* end with .lnk!
      (w32-symlinks-check-ln-script)
      (dired-check-process
       "Making symlink" "CScript" "//E:JScript"
       w32-symlinks-ln-script file newname))

    (defun w32-symlinks-make-using-ln (file newname)
      "Use Cygwin ln to construct a shortcut to FILE called NEWNAME."
      ;; ln -s file newname
      ;; (call-process "ln" nil nil nil "-s" file newname)
      (dired-check-process
       "Making symlink" w32-symlinks-ln-program "-s" "-f" file
       ;; Strip newname of trailing .lnk appended by `dired-create-files'
       ;; advice, since Cygwin ln appends the .lnk automatically:
       (if (and (not (memq 'make-old-symlinks w32-symlinks-dired-support))
		(string= (substring newname -4) ".lnk"))
	   (substring newname 0 -4)
	 newname)))

    (defun w32-symlinks-make-using-old (file newname)
      "Generate obsolete Cygwin-style symlink to FILE called NEWNAME."
      ;; This works, but cannot set system attribute.
      (with-temp-file newname (insert "!<symlink>" file "\0")))

    (defadvice dired-create-files
      (before dired-create-files-advice activate compile)
      "Ensure that actual Windows shortcut symlink target ends in .lnk.
But not if it is an obsolete Cygwin-style symlink file."
      ;; 4th arg, name-constructor, is a function that creates the target
      ;; from the source filename.
      (if (eq file-creator 'make-symbolic-link)
	  (setq name-constructor
		`(lambda (targ)
		   (setq targ (funcall ,name-constructor targ))
		   ;; Add trailing .lnk if absent:
		   (if (or (memq 'make-old-symlinks w32-symlinks-dired-support)
			   ;; (Might be better to deactivate the advice.)
			   (string= (substring targ -4) ".lnk"))
		       targ
		     (concat targ ".lnk"))))))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; File name handler for Windows shortcuts on NTEmacs.
;; See (elisp) Magic File Names.

(defcustom w32-symlinks-handle-shortcuts nil
  "If non-nil then Emacs handles Windows shortcut files as symlinks.
\(This includes Cygwin-generated shortcut files.)
Setting this variable directly does not take effect\; it must be customized.

NB: Symlink support in `dired' is controlled by `w32-symlinks-dired-support'."
  :type 'boolean
  :set (lambda (variable value)
	 (let ((elt '("\\.lnk\\'" . w32-symlinks-file-name-handler)))
	   (if value
	       (add-to-list 'file-name-handler-alist elt)
	     (setq file-name-handler-alist
		   (delete elt file-name-handler-alist))))
	 (custom-set-default variable value))
  :initialize 'custom-initialize-reset ; always call the :set function
  :group 'w32-symlinks)

;; Table of handler function names or constant return values
;; =========================================================

;; The correct behaviour in all cases in not immediately obvious to
;; me, and so may well be wrong!

(mapc
 (lambda (x) (put (car x) 'w32-symlinks (cadr x)))
 '(
   ;; Apply operation directly to the shortcut file:
   ;; (These could be omitted since this is the default action!)
   (add-name-to-file			w32-symlinks-operate-on-source)
   (copy-file				w32-symlinks-operate-on-source)
   (delete-directory			w32-symlinks-operate-on-source)
   (delete-file				w32-symlinks-operate-on-source)
   (directory-file-name			w32-symlinks-operate-on-source)
   (expand-file-name			w32-symlinks-operate-on-source)
   (file-exists-p			w32-symlinks-operate-on-source)
   (file-name-directory			w32-symlinks-operate-on-source)
   (file-name-nondirectory		w32-symlinks-operate-on-source)
   (file-name-sans-versions		w32-symlinks-operate-on-source)
   (file-name-all-completions		w32-symlinks-operate-on-source)
   (file-name-as-directory		w32-symlinks-operate-on-source)
   (file-name-completion		w32-symlinks-operate-on-source)
   (insert-directory			w32-symlinks-operate-on-source)
   (rename-file				w32-symlinks-operate-on-source)

   ;; Apply operation to symlink target:
   (diff-latest-backup-file		w32-symlinks-operate-on-target)
   (directory-files			w32-symlinks-operate-on-target)
   (file-accessible-directory-p		w32-symlinks-operate-on-target)
   (file-directory-p			w32-symlinks-operate-on-target)
   (file-executable-p			w32-symlinks-operate-on-target)
   (file-local-copy			w32-symlinks-operate-on-target)
   (file-modes				w32-symlinks-operate-on-target)
   (file-newer-than-file-p		w32-symlinks-operate-on-target)
   (file-ownership-preserved-p		w32-symlinks-operate-on-target)
   (file-readable-p			w32-symlinks-operate-on-target)
   (file-regular-p			w32-symlinks-operate-on-target)
   (file-truename			w32-symlinks-operate-on-target)
   (file-writable-p			w32-symlinks-operate-on-target)
   (find-backup-file-name		w32-symlinks-operate-on-target)
   (get-file-buffer			w32-symlinks-operate-on-target)
   (load				w32-symlinks-operate-on-target)
   (make-directory			w32-symlinks-operate-on-target)
   (make-symbolic-link			w32-symlinks-operate-on-target)
   (set-file-modes			w32-symlinks-operate-on-target)
   (set-visited-file-modtime		w32-symlinks-operate-on-target)
   (shell-command			w32-symlinks-operate-on-target)
   (unhandled-file-name-directory	w32-symlinks-operate-on-target)
   (vc-registered			w32-symlinks-operate-on-target)
   (verify-visited-file-modtime		w32-symlinks-operate-on-target)
   (write-region			w32-symlinks-operate-on-target)

   ;; Special cases:
   (directory-files-and-attributes	w32-symlinks-directory-files-and-attributes)
   (file-attributes			w32-symlinks-file-attributes)
   (file-symlink-p			w32-symlinks-file-symlink-p)
   (insert-file-contents		w32-symlinks-insert-file-contents)))

;; Currently unhandled cases:
;;   dired-call-process, dired-compress-file, dired-uncache
;;   (Dired operations on symlinks are currently handled elsewhere.)

;; Handler functions
;; =================

(defun w32-symlinks-file-name-handler (operation &rest args)
  "Apply OPERATION to list ARGS, handling Windows shortcuts \(.lnk files)."
  (let ((handler (get operation 'w32-symlinks)))
    (if handler
	(if (symbolp handler)
	    (funcall handler operation args)
	  (car handler))		; constant value
      ;; Default for cases that are currently unhandled:
      (w32-symlinks-operate-on-source operation args))))

(defun w32-symlinks-operate-on-source (operation args)
  "Apply OPERATION to list ARGS, without handling Windows shortcuts."
  (let ((inhibit-file-name-handlers
	 (cons 'w32-symlinks-file-name-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(defun w32-symlinks-operate-on-target
  (operation args &optional insert-file-contents)
  "Apply OPERATION to target of symlink given by first element of list ARGS.
Provides special handling when INSERT-FILE-CONTENTS is non-nil.
Called indirectly by `w32-symlinks-file-name-handler'."
  (let* ((w32-symlinks-shortcut-target 'expand)
	 ;; w32-symlinks-parse-shortcut is internally protected.
	 (filename (w32-symlinks-parse-shortcut (car args))))
    (if filename
	(progn
	  (if insert-file-contents
	      (rename-buffer (file-name-nondirectory filename)))
	  (apply operation (cons filename (cdr args))))
      (w32-symlinks-operate-on-source operation args))))

(defun w32-symlinks-insert-file-contents (operation args)
  "Apply OPERATION to target of symlink given by first element of list ARGS.
Provides special handling for `insert-file-contents'.
Called indirectly by `w32-symlinks-file-name-handler'."
  (w32-symlinks-operate-on-target operation args t))

(defun w32-symlinks-file-symlink-p (operation args)
  (w32-symlinks-parse-symlink (car args)))

(defun set-attr-symlink (file-and-attr function)
  (when (and (cdr file-and-attr)
             (not (cadr file-and-attr))
             (setcar (cdr file-and-attr) (w32-symlinks-parse-symlink (car file-and-attr))))
    (aset (nth 9 file-and-attr) 0 ?l)))

(defun w32-symlinks-file-attributes (operation args)
  (let* ((inhibit-file-name-handlers (cons 'w32-symlinks-file-name-handler
                                           (and (eq inhibit-file-name-operation operation)
                                                inhibit-file-name-handlers)))
         (inhibit-file-name-operation operation)
         (attr (apply operation args)))
    (set-attr-symlink (cons (car args) attr) 'w32-symlinks-parse-symlink)
    attr))

(defun w32-symlinks-directory-files-and-attributes (operation args)
  (let* ((inhibit-file-name-handlers (cons 'w32-symlinks-file-name-handler
                                           (and (eq inhibit-file-name-operation operation)
                                                inhibit-file-name-handlers)))
         (inhibit-file-name-operation operation)
         (attr-alist (apply operation args)))
    (mapc (lambda (file-and-attr)
            (set-attr-symlink file-and-attr 'w32-symlinks-parse-symlink))
          attr-alist)
    attr-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Provide a more convenient way to access shortcut (and other) files
;; literally, by giving find-file, or any function that calls it, a
;; prefix arg.

(defun find-file (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists.
Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  Wildcard expansion
can be suppressed by setting `find-file-wildcards'.
With prefix arg tries to finds files literally, with no conversions."
  (interactive "FFind file: \np")
  (let* ((inhibit-file-name-handlers
	  (cons 'w32-symlinks-file-name-handler
		inhibit-file-name-handlers))
	 (value
	  (find-file-noselect filename nil current-prefix-arg wildcards)))
    (if (listp value)
	(mapcar 'switch-to-buffer (nreverse value))
      (switch-to-buffer value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BUG FIXES included here temporarily; they need to be merged into
;; the standard code base once I am convinced they work.

;; Revised `insert-file-contents-literally' to allow it to be called
;; within a magic file name handler.

(defun insert-file-contents-literally
  (filename &optional visit beg end replace)
  "Like `insert-file-contents', but only reads in the file literally.
A buffer may be modified in several ways after reading into the buffer,
to Emacs features such as format decoding, character code
conversion, `find-file-hooks', automatic uncompression, etc.

This function ensures that none of these modifications will take place."
  (let ((format-alist nil)
	(after-insert-file-functions nil)
	(coding-system-for-read 'no-conversion)
	(coding-system-for-write 'no-conversion)
	(find-buffer-file-type-function
	 (if (fboundp 'find-buffer-file-type)
	     (symbol-function 'find-buffer-file-type)
	   nil))
	(inhibit-file-name-handlers	; FJW
	 (append '(jka-compr-handler image-file-handler)
		 inhibit-file-name-handlers))
	(inhibit-file-name-operation 'insert-file-contents))
    (unwind-protect
	(progn
	  (fset 'find-buffer-file-type (lambda (filename) t))
	  (insert-file-contents filename visit beg end replace))
      (if find-buffer-file-type-function
	  (fset 'find-buffer-file-type find-buffer-file-type-function)
	(fmakunbound 'find-buffer-file-type)))))

(eval-and-compile
  (when (and (< emacs-major-version 22) ; Bug fixed in Emacs 22.
             (eq system-type 'windows-nt))
    (defadvice file-symlink-p (around file-symlink-p-advice activate compile)
      ;; The original version is a built-in function.
      ;; According to (elisp) Magic File Names, it should support magic
      ;; file name handlers, but it does not.
      ;; (I suspect that it always returns nil in NTEmacs.)
      (let ((handler (find-file-name-handler filename 'file-symlink-p)))
	(if handler
	    (setq ad-return-value (funcall handler 'file-symlink-p filename))
	  ad-do-it)))
    ))

;; Elisp Manual Error (fixed in Emacs 22):

;; According to (elisp) Magic File Names, `substitute-in-file-name'
;; does not support magic file name handlers, but it appears that it does.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'w32-symlinks)

;;; w32-symlinks.el ends here
