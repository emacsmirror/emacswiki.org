;;; nc.el --- emulate famos ms-dog file browser
;;; Copyright (C) 1996 Stefan Hegny, Ilya Zakharevich

;; Author: Stefan Hegny (hegny@fzi.de) with improvements by 
;;	Ilya Zakharevich (ilya@math.ohio-state.edu)
;; Keywords: file browser, directory, shell
;; Available: ftp://ftp.math.ohio-state.edu/pub/users/ilya/emacs

;; This file is not part of GNU Emacs.
;; This file is distributed under the same terms as GNU Emacs.

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

;;;	$Id: nc.el 1.7 1998/05/05 02:59:15 ilya Exp ilya $	
;;; Commentary:

;;; Installation:
;;; put nc.el/nc.elc in your load-path
;;; add (autoload 'nc "nc" "Emulate MS-DOG file shell" t) to _emacs/.emacs
;;; type `M-x nc'

;;; The descriptions moved to the docstring for the mode.


;;;Things that could be done but havent been:
;;; - directory tree
;;; - update view of backup-file after saving a buffer from nc
;;;   (see nc-update-current-file and nc-local-set-keys)
;;; (- undo function)

;;; Change Log:
;;;
;;; 1.7: (IZ)
;;;
;;;		`nc-byte-compile' was keeping an index after the listing changed.
;;; 1.6: (IZ)
;;;
;;;		`save-file-hook' was misbehaving if a buffer for a deleted
;;;		file was still present.
;;;		`B' (byte-compile) is made to work on selected files.
;;; 1.5: (IZ)
;;;
;;;		Operations on files (move, copy, delete) treat
;;;		symlinks as ordinary files. Currently they are copied
;;;		"as is", without translation to a different location
;;;		if relative.
;;;
;;;		`nc-select-by-regexp' implemented. Selection uses history.
;;;
;;;		`nc-toggle-selected' implemented. Put on `*'.
;;;
;;;		Keypad + - * made synonymous to the keyboard one.
;;;
;;;		Mode help provided. Selection by regexps works. Mode
;;;		menu added.
;;;
;;;		Mouse-1 file switching works under 19.31 too. Shell
;;;		tracks TABs under 19.31 too.
;;;
;;;		`nc-sort-case-insensitive' added.
;;;
;;;		`nc-write-header', `nc-write-totals',
;;;		`nc-toggle-write-title', `nc-toggle-write-totals'
;;;		added.
;;;
;;;		`nc-show-list' used instead of
;;;		`nc-show-size'. `nc-toggle-show-date',
;;;		`nc-toggle-show-time', `nc-toggle-show-attr' added.
;;;
;;;		Standard faces are used. You can specify a package to
;;;		load to get the faces.
;;;
;;; 1.0beta3: (IZ)
;;;
;;;		Panes track directory of the shell window.
;;;
;;;		The top and botton of panes are not redrawn if not needed.
;;;
;;;		Copy and move work with directories, even on different
;;;		drives (Emacs bug).
;;;
;;;		Number of files in the root directory is correctly calculated.
;;;
;;;		Empty root directory may be handled now (with some settings).
;;;
;;; 1.0beta2: (IZ)
;;;		Uses floats for size.
;;;
;;;		Switching buffers with mouse deselects.
;;;
;;;		Directories are highlighted, same with name-too-long
;;;		and headers.
;;;
;;;		[return] views by default.
;;;
;;;		Double-mouse-1 acts as [return].
;;;
;;;		Total files/size is shown in the summary line.
;;;
;;;		Shell window tracks directories in the panes, but not
;;;		visa versa.
;;;
;;; 1.0beta1: (IZ) `nc-sort-by' instead of `nc-sort-by-time', added
;;; 		bindings for "R" - reverse, "S" - size, variable
;;; 		`nc-sort-dir-first' controls whether the directories
;;; 		should be put first, `nc-sort-reversed' the
;;; 		order. Sort uses name as the secondary criterion.
;;;
;;;		Bug with saving files in different frame corrected.
;;;
;;;		Supports several frames with NC panes, variable width
;;;		panes, variable number of columns per pane (use
;;;		"1"..."9" to switch, or `nc-columns').
;;;
;;;		Bug with going to the end when moving right
;;;		corrected. Left and right take repeat count.
;;;
;;;		Frame name contains directory.
;;;
;;;		Screen estate gains: some commas deleted from
;;;		modeline, dir indicator is <DIR> now, only user
;;;		permissions are shown on DOSish systems. modeline is
;;;		moved to mode-line (yes!), selection info is shown on
;;;		the boundary line (different formats chosen by the
;;;		pane width).
;;;
;;;		EMX is recognized as Dosish system, bug with
;;;		f:/.. corrected.
;;;
;;;		^L redraws screen. S-f? is used instead of
;;;		C-f?. C-f[3568] used to control sorting
;;;		order. Pg-up/down move by panes.
;;;
;;;		Handling of `nc-cl' and friends streamlined.
;;;
;;;		Scrollbars, fill-mode and undo handled.
;;;
;;;		.. could be operated upon.
;;;
;;;		`nc-rescan' added on C-r.
;;;
;;;		Can delete directories now.
;;;
;;;		Can calculate size of directory/selected directories on C-q.
;;;
;;;		May select/deselect directories. Governed by 3
;;;		variables `nc-select-dir', `nc-select-dir-wild',
;;;		`nc-deselect-dir-wild'.
;;;
;;;		`nc-toggle-show-size' implemented, put on #.
;;;
;;; 1.0beta:   renamed assuming it is usable by now
;;;
;;; 1.0alpha7: corrected error with negative values for format-time-string
;;;            (error occured on NT)
;;;
;;; 1.0alpha6: added nc-version (Command and Variable)
;;;            added goto-dir-with-mouse
;;;            added doc for mouse-functions
;;;            replaced down-mouse-n with mouse-n
;;;            made some variables user-settable
;;;
;;; 1.0alpha5: corrected switching between the nc-buffers.
;;;            the side-by-side-view will now  be restored
;;;            every time. 
;;;            Text-cursor will now always be on the file
;;;            where the file cursor would be.
;;;
;;; 1.0alpha4: corrected malfunction for ESC-ESC
;;;            added comments for variables and removed unused ones
;;;
;;; 1.0alpha3: Byte compiling on linked files (I needed this)
;;;
;;; 1.0alpha2: removed bug from nc-update-all-modifications
;;;            now also renaming in the same directory is displayed corr.

;;; Code:

;(require 'rect)
 
(defconst nc-version "1.5"
  "Version of File Browser, 
dont change it unless you changed something else")

;;; Customazible variables:

(defvar nc-load-fonts-from 'font-lock
  "*Not-nil means require this package to get faces.
Should be a symbol.")
(defvar nc-highlight-face 'secondary-selection
  "*Face for Cursor and Title.")
(defvar nc-select-face 'region
  "*Face for selection mark.")
(defvar nc-bold-face 'font-lock-keyword-face
  "*Face for headers/footers.")
(defvar nc-dir-face 'font-lock-string-face
  "*Face for directories.")

(if (and window-system nc-load-fonts-from)
    (require nc-load-fonts-from))	; To get faces.

(defvar nc-highlight-background-color "DarkSlateGray"
  "*Background of Cursor and Title")
(defvar nc-highlight-foreground-color "Yellow"
  "*Foreground of Cursor and Title")
(defvar nc-select-foreground-color "DarkSlateGray"
  "*Background of selection mark")
(defvar nc-select-background-color "Green"
  "*foreground of selection mark")

(defvar nc-time-format "%H:%M"
  "*Format String to display time.")

(defvar nc-date-format "%d-%m-%y"
  "*Format String to display date.")

(defvar nc-date-time-format (concat nc-date-format " " nc-time-format)
  "*Format String to display date and time.")

(defvar nc-sort-by 'name
  "*Sort files by this value.")

(defvar nc-sort-case-insensitive (eq system-type 'emx)
  "*Not-nil means case-fold when sorting by name")

(defvar nc-columns 3
  "*How many columns to use.")

(defvar nc-sort-reversed nil
  "*Not-nil means the sorting order is reversed.")

(defvar nc-sort-dir-first t
  "*Not-nil means directories are at the top.")

(defvar nc-dosish (memq system-type '(ms-dos emx))
  "*If not-nil, nc behaves dosishly.")

(defvar nc-horiz-line-char (if nc-dosish
			       205
			     ?=)
  "Character number for horizontal lines")

(defvar nc-verti-line-char (if nc-dosish
			       179
			     ?|)
  "Character number for vertical lines")

(defvar nc-secondary-verti-line-char (if nc-dosish
					 ?|
				       ?\ )
  "Character number for secondary vertical lines")

(defvar nc-name-too-long-char
  (if nc-dosish
      175
    ?>)
  "Character to designate longer filenames")

(defvar nc-name-too-long-string (char-to-string nc-name-too-long-char)
  "String of Character to designate longer filenames")

(if window-system 
    (put-text-property 0 1 
		       'face nc-bold-face nc-name-too-long-string))

(defvar nc-suffix-length
  (cond ((eql system-type 'ms-dos)
	 3)
	(t 0))
  "Length to be displayed behind dot in filename.")

(defvar nc-show-list nil
  "*List of fields to show in the pane.")

(defvar nc-select-dir t
  "*Not-nil means directories may be selected one-by-one.")

(defvar nc-select-by-regexp nil
  "*Not-nil means (de)selection is done by regexp, nil - by wildcard.")

(defvar nc-select-dir-wild nil
  "*Not-nil means directories may be selected by wildcard.")

(defvar nc-deselect-dir-wild t
  "*Not-nil means directories may be deselected by wildcard.")

(defvar nc-default-action 'view
  "What to do on [return] if nothing else matches.")

(defvar nc-show-shell t			; Unfinished
  "Non-nill means show nc-shell-window.")

(defvar nc-total-size-selected)

(defvar nc-total-size-files)

(defvar nc-write-totals t
  "*Not-nil means write a bottom line with info on total and selected size.")

(defvar nc-write-header t
  "*Not-nil means write a header line with info on fields.")

(defvar nc-num-files)

(defvar nc-files nil
  "Files in file browsers directory.
CAR is a list of the directory, CDR is a list of files.
Known fields of a file: 0 name 1 dir?, 2 size, 3 date, 4 attributes,
5 realname, 6 selection?, 7 symlink, 8 long extension, 9 short extension.")

(defsubst nc-buffers ()
  (cdr (assoc 'nc-buffers (frame-parameters))))

(defsubst set-nc-buffers (val)
  (modify-frame-parameters (selected-frame) (list (cons 'nc-buffers val))))

(defsubst nc-parent-dir ()
  (let ((leng (- (length (car (car nc-files))) 2))
	(stri (car (car nc-files))))
    (while (and (> leng 0)
		(not (= (aref stri leng) 92))
		(not (= (aref stri leng) 47)))
      (setq leng (- leng 1)))
    (cond ((>= leng 0)
	   (substring stri 0 (+ 1 leng))))))

(defvar nc-tmp-buf " *NC tmp*")
(defvar nc-shell-buf "*NC shell*")

(defvar nc-cursor-overlay (make-overlay 0 0))
(defvar nc-dir-overlay (make-overlay 0 0))
(defvar nc-select-overlay (make-overlay 0 0))
(defvar nc-modeline)
(defvar nc-dotdot)
(defvar nc-associated-nc-buffer nil
  "Buffer associated in file browser")
(defvar nc-dir-types
  '(directory ((copy . nc-copy-file)
	       (move . nc-move-file)
	       (delete . nc-delete-file-or-directory))))

;;; Callable Functions:

;;;###autoload
(defun nc-mode (&optional no-reset keep-size)
  "Major mode for File Browser in GNU emacs.\\<nc-mode-map>

Advantages and disadvantages:
a) portable;
b) most functions work over ange-FTP (except those which need running 
commands, like dirsize and shell window);
c) More customizable, works with arbitrary sizes of buffers;
d) Cannot read archives so far;
e) Cannot compare directories, change attributes and times, 
filter, show a tree or quickview, has no action by filemask.
f) Current file in the pane is not preserved after some operations 
\(like changing the sorting order).
g) Will not prompt for `try-anyway' if file permissions are wrong.

Normally displays two adjacent buffers with directory contents. (This
state can always be restored typing `M-x nc').  Buffers can be
resized/rearanged. To inform NC about the change, hit \\[nc-redraw] or
choose Redraw from menu

This package emulates the file browser of a famous ms-dog application.
Look-and-feel is supposed to be close to the original, and there exist the
following functions on the following keys:
\(marked [*] means: in NC-directory buffers and in buffers opened from there
 marked [+] means only in buffers opened from there;
 marked [=] means in the command-line buffer as well 
		if the selected pane is visible
 unmarked: only in NC-directory buffers)

          [*] M-x nc:   Invoke file browser
\[=]up,down,left,right:   Move around 
\[=]         home,end:   Goto first/last item
\[=]	 pgup,pgdown:   Pane left/right
              return:   If dir, change into dir
                        If executable, execute,, otherwise view
\[=]           insert:   Select/deselect file under cursor
                   +:   Select files of pattern
                   -:   Deselect files of pattern
                   *:   Toggle selected state of files.
\[=]           Gray-+:   Select files of pattern
\[=]           Gray--:   Deselect files of pattern
\[=]           Gray-*:   Toggle selected state of files.
\[=]              tab:   Goto other of the two nc-buffers
                  f1:   Short help
\[=]               f2:   Change to a new directory or update current
                        (re-read from disk)
\[=]             S-f2:   Update view of current dirs (dont re-read)
\[=]               f3:   View file under cursor
\[=]               f4:   Edit file under cursor
\[=]               f5:   Copy selected files, 
                        if none selected, use file the cursor is on
\[=]             S-f5:   like f5, default target is dir in same buffer
\[=]               f6:   Move/Rename selected files, 
                        if none selected, use file the cursor is on
\[=]             S-f6:   like f6, default target is dir in same buffer
\[=]               f7:   Make Directory, prompt for name
\[=]               f8:   Delete selected files, 
                        if none selected, use file or dir the cursor is on
             [*] f10:   Quit file browser, killing all its buffers
             ESC ESC:   Get back to two column nc-windows
         [+] ESC ESC:   Get back to two column nc-windows,
                        killing current buffer
  [msdog only]  a..s:   Change disk drive
  [caps] N,T,S,R,X,E:   Sort by [N]ame, [S]ize or [T]ime, [R]eversing
			   short [E]xtension, Long e[X]tension
\[=] C-f3, C-f4, C-f5, C-f6, C-f8: 
			   Same: Name, Extension, Size or Time, Reversing.
	     1 ... 9:   Change the number of columns
	           #:   Toggle display of size.
\[=]	         C-l:   Redraw the screen.
\[=]	         C-r:   Rescan the directory.
	         C-q:   Calculate size of directory/directories.
                   B:   Byte compile file under cursor
                   Q:   Quit file browser, killing all its buffers
             mouse-1:   Set File cursor with mouse
      double-mouse-1:   Goto Dir/Exec File with mouse, or view.
             mouse-2:   Goto Dir/Exec File with mouse, or view.

 Additional keys working in the command-line buffer:
 C-end, C-home:	   	Move to end-, beginning-of-line
 A-insert, M-insert	Toggle overwrite mode.
 A-left, M-left	   	Left char.
 A-right, M-right	Right char.
 Esc Esc		Toggle full-screen display of command-line buffer

NC works under other os' like UN*X.  If you dont like the defaults,
change them (see below)!

Names of Directorys are displayed as CAPS on ms-dog, by appending
\"/\" \(and color) on other os. The variable `nc-sort-dir-first'
controls whether one should put the directories before files in
listing.

The displayed length of names and the number of lines in a column are
well estimated out of (frame-width) and (frame-height).  To adjust
display to a resized frame or after changing the sorting criteria by
hand, type `C-f2' (In this case, the files in the directory are NOT
reloaded), same with `\\[nc-redraw]'. To reload directory, type `f2 RET'
or `\\[nc-rescan]'.

Things that can be changed reasonably include the following variables
\(the list below is obsolete!):

*COLORS*
`nc-highlight-background-color'
`nc-highlight-foreground-color'
`nc-select-background-color'
`nc-select-foreground-color'

*CHARS*
`nc-horiz-line-char'
`nc-verti-line-char'
`nc-name-too-long-char'

*SPECIALS*
`nc-suffix-length'    if not 0, display that much positions of the
                      filename to the right of the reserved space for
                      the first dot in the filename (except dot at pos. 0)
                      if =0, display all filenames flushed left without
                      separating name and suffix
                      Longer filenames or parts thereof are indicated
                      with nc-name-too-long-char
`nc-time-format'      string used to format file time
`nc-sort-by'   	      sort criterium for displayed list of files
`nc-sort-reversed'    Whether to reverse sort
`nc-columns'	      number of columns in the pane

"
  (interactive)
  (let ((assbuff nc-associated-nc-buffer) (currbuf (current-buffer)) ob)
    (nc-set-keys)
    (nc-switch-to-buffers keep-size)
    (cond ((not nc-files)
	   (let ((defdir (expand-file-name (getenv "HOME"))))
	     (if (not (nc-valid-dirname-p defdir))
		 (setq defdir (format "%s/" defdir)))
	     (setq nc-files (list (list defdir)))
	     (setq default-directory defdir))
	   (nc-display-buffer 0)
	   (nc-setup-cursor)))
    (nc-deselect-buffer)
    (setq ob (current-buffer))
    (other-window 1)
    (cond ((not nc-files)
	   (if (not (nc-valid-dirname-p default-directory))
	       (setq default-directory (format "%s/" default-directory)))
	   (setq nc-files (list (list (expand-file-name default-directory))))
	   (nc-display-buffer 0)
	   (nc-setup-cursor)))
    (nc-deselect-buffer)
    (cond ((eq currbuf (current-buffer))
	   (nc-select-buffer))
	  ((eq currbuf ob)
	   (other-window -1)
	   (nc-select-buffer))
	  ((and assbuff
		(not (eql assbuff (current-buffer))))
	   (other-window -1)
	   (nc-select-buffer))
	  (assbuff
	   (other-window -1)
	   (nc-deselect-buffer)
	   (other-window 1)
	   (nc-select-buffer))
	  (t (nc-select-buffer)))
    (nc-redraw no-reset)
    (if (and nc-show-shell (eq currbuf (get-buffer nc-shell-buf)))
	(select-window (get-buffer-window nc-shell-buf)))))

;;;###autoload
(defun nc-version ()
  "Show Version of File Browser"
  (interactive)
  (message "File Browser Version %s" nc-version)
  nc-version)

;;;###autoload
(defalias 'nc 'nc-mode)


;;; Internal Stuff:

(defvar nc-panes nil
  "List of names of used panes.")

(defvar nc-other nil
  "Buffer of the other pane.")

;;;(make-variable-frame-local 'nc-buffers)

(cond ((not (boundp 'frame-title-format))) ; Old emacs
      ((stringp frame-title-format)
       (setq frame-title-format 
	     (list frame-title-format
		   '(nc-files (":   " default-directory)))))
      ((not (listp frame-title-format))) ;nil
      ((member '(nc-files (":   " default-directory)) frame-title-format)) ;nil
      (t (setq frame-title-format 
	     (append frame-title-format
		   '((nc-files (":   " default-directory)))))))

(defvar nc-mode-map (make-sparse-keymap)
  "Keymap for File Browser")

(defvar nc-local-esc-map (copy-keymap esc-map)
  "Keymap for Files openend from File Browser")

(defvar nc-local-map nil
  "local keymap for file browser")

(defvar nc-wd
    (if (eql system-type 'ms-dos)
	12
      (- (/ (- (frame-width) 1) 2) 1))
  "Width of one panel plus 1.")

(defvar nc-cl
    (if (eql system-type 'ms-dos)
	12
      (- (/ nc-wd nc-columns) 1))
  "Width of one column")

(defvar nc-sb nil 
  "Whether frame had scrollbars, 1 or 0.")

(defvar nc-pane-end 0
  "Position in the pane after displayed files")

(defvar nc-cursor-col nil
  "Cursor column in file browser")
(defvar nc-cursor-lin nil
  "Cursor line in file browser")
(defvar nc-first-column nil
  "index of leftmost visible column")
(defvar nc-format nil
  "formatted file lists for file browser columns")
(defvar nc-format-align-right 0
  "offsett of displayed files in file browser")
(defvar nc-title-start nil
  "start column of title in file browser")
(defvar nc-title-end nil
  "end column of title in file browser")
(defvar nc-active-nc-buffer nil
  "NC pane which was active last.")
(defvar nc-selected-files nil
  "List of selected files in file browsers directory")
(defvar nc-file-mod nil
  "List of modified files during file browser operation")

(defvar nc-nl
    (if (eql system-type 'ms-dos)
	18
      (- (frame-height) 7))
  "Number of lines in browser")

(defsubst nc-number-to-string (num)
  (let ((s (number-to-string num)))
    (cond ((string-match "\\." s) (substring s 0 (match-beginning 0)))
	  (t s))))

(defsubst nc-total-size (list)
  (let ((sz 0))
    (mapcar (function (lambda (elt)
			(setq sz (+ sz (or (elt elt 2) 0))))) 
	    list)
    sz))

(define-key nc-local-esc-map [27] 'nc-bufferkill-or-nc)

(defun nc-absolute-from-relative-pos (col lin)
  "Compute the position of leftmost character
of filename in line LIN and column COL as point"
  (+ (* (+ (if nc-write-header 2 1) lin) nc-wd) 1
     (* (+ nc-cl 1) (- col nc-first-column))))

(defun nc-relative-from-absolute-pos (pos)
  "Compute the list (column line) from POS in buffer"
  (setq pos (- pos (* (if nc-write-header 2 1) nc-wd)))
  (if (> pos 0)
      (let ((line (/ pos nc-wd)))
	(if (< line nc-nl)
	    (list (+ (/ (- pos  (* line nc-wd))
			(+ nc-cl 1))
		     nc-first-column)
		  line)
	  '(-1 -1)))
    '(-1 -1)))

(defun nc-toggle-show-size ()
  "Add/delete `size' to/from the value of `nc-show-list'."
  (interactive)
  (cond ((memq 'size nc-show-list)
	 (setq nc-show-list (delq 'size nc-show-list)))
	(t (setq nc-show-list (cons 'size nc-show-list))))
  (nc-redraw))

(defun nc-toggle-show-date ()
  "Add/delete `date' to/from the value of `nc-show-list'."
  (interactive)
  (cond ((memq 'date nc-show-list)
	 (setq nc-show-list (delq 'date nc-show-list)))
	(t (setq nc-show-list (cons 'date nc-show-list))))
  (nc-redraw))

(defun nc-toggle-show-time ()
  "Add/delete `time' to/from the value of `nc-show-list'."
  (interactive)
  (cond ((memq 'time nc-show-list)
	 (setq nc-show-list (delq 'time nc-show-list)))
	(t (setq nc-show-list (cons 'time nc-show-list))))
  (nc-redraw))

(defun nc-toggle-show-attr ()
  "Add/delete `attr' to/from the value of `nc-show-list'."
  (interactive)
  (cond ((memq 'attr nc-show-list)
	 (setq nc-show-list (delq 'attr nc-show-list)))
	(t (setq nc-show-list (cons 'attr nc-show-list))))
  (nc-redraw))

(defun nc-filename-norm (list)
  "Format the NAME of the file for display in File Browser buffer.
Iff (elt LIST 1) is t, letters will be capitals in ms-dog,
name will be suffixed `/' otherwise."
  (let* ((name (file-name-nondirectory (elt list 5)))
	 (name-length (length name))
	 (dotpos  0)
	 (prelength 0)
	 (prestring nil)
	 (pos 0)
	 (postlength 0)
	 delta)
    (cond ((memq 'size nc-show-list) nil)
	  ((and (elt list 1)
		(eql system-type 'ms-dos))
	   (setq name (upcase name)))
	  ((elt list 1)
	   (setq name (format "%s/" name)
		 name-length (+ 1 name-length))))
    (cond ((elt list 1)
	   (put-text-property 0 name-length 'face nc-dir-face name)))
    (while (and (< pos name-length)
		(= 0 dotpos))
      (cond ((and (= 0 dotpos)
		  (= 46 (aref name pos))
		  (>  pos 0))
	     (setq dotpos prelength))
	    ((> dotpos 0)
	     (setq postlength (+ 1 postlength)))
	    (t
	     (setq prelength (+ 1 prelength))))
      (setq pos (+ 1 pos)))
    (if (or (string= name "..")
	    (string= name "../")
	    (= 0 nc-suffix-length))
	(setq prelength (length name) dotpos 0))
    (setq postlength (- name-length prelength 1))
    (cond ((memq 'attr nc-show-list)
	   (setq name (concat
		       (if nc-dosish  
			   (substring (or ; Empty root!
				       (elt list 4) "    ")
				      0 4)
			 (elt list 4))
		       (char-to-string nc-secondary-verti-line-char)
		       name)
		 delta (if nc-dosish 5 11)
		 name-length (+ delta name-length)
		 prelength (+ delta prelength))))
    (cond ((memq 'time nc-show-list)
	   (setq name (concat
		       (format-time-string nc-time-format (elt list 3))
		       (char-to-string nc-secondary-verti-line-char)
		       name)
		 name-length (+ 6 name-length)
		 prelength (+ 6 prelength))))
    (cond ((memq 'date nc-show-list)
	   (setq name (concat
		       (format-time-string nc-date-format (elt list 3))
		       (char-to-string nc-secondary-verti-line-char)
		       name)
		 name-length (+ 8 name-length)
		 prelength (+ 8 prelength))))
    (cond ((memq 'size nc-show-list)
	   (setq name (concat 
		       (format "%9s%c"
			       (if (elt list 2) 
				   (nc-number-to-string (elt list 2)) "<DIR>")
			       nc-secondary-verti-line-char)
		       name)
		 name-length (+ 10 name-length)
		 prelength (+ 10 prelength))))
    name
    ;;;; I did not insert code to format it for DOS, sorry... XXXXX
;;;    (cond ((and (= 0 dotpos)
;;;		(> name-length nc-cl))
;;;	   (concat (substring name 0 (- nc-cl 1)) nc-name-too-long-string))
;;;	  ((= 0 dotpos)
;;;	   (concat name (make-string (- nc-cl name-length) 32)))
;;;	  (t (cond ((> prelength (- nc-cl (+ 1 nc-suffix-length)))
;;;		    (setq prestring
;;;			  (concat (substring name 0
;;;					     (- nc-cl
;;;						(+ 2 nc-suffix-length)))
;;;				  nc-name-too-long-string)))
;;;		   (t (setq prestring
;;;			    (concat (substring name 0 prelength)
;;;				    (make-string
;;;				     (- nc-cl (+ 1 nc-suffix-length) prelength)
;;;				     32)))))
;;;	     (cond ((> postlength nc-suffix-length)
;;;		    (setq postlength
;;;			  (concat
;;;				  (substring name (+ 1 prelength)
;;;					     (+ nc-suffix-length
;;;						prelength))
;;;				  nc-name-too-long-string)))
;;;		   (t (setq postlength
;;;			    (concat
;;;			     (substring name (+ 1 prelength))
;;;			     (make-string
;;;			      (- nc-suffix-length postlength) 32)))))
;;;	     (concat prestring " " postlength)))
    ))

(defun nc-sort-reversed-xor (x y val)
  (cond ((equal (elt x 5) "..") t)
	((equal (elt y 5) "..") nil)
	(t
	 (if (and nc-sort-dir-first (not (eq (elt x 1) (elt y 1)))) ; Dir, notdir
	     (elt x 1)
	   (cond (nc-sort-reversed (not val))
		 (t val))))))

(defun nc-sort-dired-files (files)
  ;; Sort by name if the field is equal
  (cond ((and nc-sort-case-insensitive (eq nc-sort-by 'name))
	 (sort files '(lambda (x y)
			(nc-sort-reversed-xor x y 
					      (string< 
					       (downcase (elt x 5))
					       (downcase (elt y 5)))))))
	((eq nc-sort-by 'name)
	 (sort files '(lambda (x y)
			(nc-sort-reversed-xor x y 
					      (string< (elt x 5) (elt y 5))))))
	((eq nc-sort-by 'size)
	 (sort files '(lambda (x y)
			(nc-sort-reversed-xor 
			 x y 
			 (or (> (or (elt x 2) 0)
				(or (elt y 2) 0)) ; Reverse order
			     (and (= (or (elt x 2) 0)
				     (or (elt y 2) 0))
				  (string< (elt x 5) (elt y 5)))))))) ;
	((eq nc-sort-by 'ext)
	 (sort files '(lambda (x y)
			(nc-sort-reversed-xor 
			 x y 
			 (or (string< (elt x 9)
				      (elt y 9))
			     (and (equal (elt x 9)
					 (elt y 9))
				  (string< (elt x 5) (elt y 5)))))))) ;
	((eq nc-sort-by 'long-ext)
	 (sort files '(lambda (x y)
			(nc-sort-reversed-xor 
			 x y 
			 (or (string< (elt x 8)
				      (elt y 8))
			     (and (equal (elt x 8)
					 (elt y 8))
				  (string< (elt x 5) (elt y 5)))))))) ;
	(t (sort files '(lambda (x y)
			  (nc-sort-reversed-xor 
			   x y
			   (let ((ga (car (elt x 3)))
				 (ka (elt (elt x 3) 1))
				 (gb (car (elt y 3)))
				 (kb (elt (elt y 3) 1)))
			     (or (< ga gb)
				 (and (= ga gb)
				      (< ka kb))
				 (and (= ga gb)
				      (= ka kb)
				      (string< (elt x 5) (elt y 5)))))))))))

(defun nc-file-attributes-list (name)
  (let* ((attr (file-attributes name)) 
	 (isdir (file-directory-p name))
	 (match (if (string-match "[^/]\\(\\.[^/.]*\\)*\\(\\.[^./]*\\)$" name)
		    (cons
		     (substring name (1+ (match-beginning 0)))
		     (substring name (match-beginning 2)))
		  '("" . "")))
	 (list (and attr		; Like f:/..
		    (list
		     nil
		     isdir
		     ;;(and (car attr)	       ;;; 1:directory-p
		     ;;(= ?d (elt (elt attr 8) 0)))
		     (if isdir nil (float (elt attr 7)))  ;;; 2:size or nil
		     (list (max (car (elt attr 5)) 0)
			   (max (car (cdr (elt attr 5))) 0)
			   )		       ;;; 3:date
		     (elt attr 8)		       ;; 4:attributes
		     (file-name-nondirectory name)     ;; 5:realname
		     nil			       ;; 6: place for selected
		     (file-symlink-p name)	       ;; 7: is-symlink
		     (car match)	;; 8: long extension
		     (cdr match)	;; 9: short extension
		     ))))
    (and attr				; Like f:/..
	 (progn
	   (if (string-match "/\\.\\.$" name) (setq nc-dotdot 1))
	   (cons (nc-filename-norm list)	       ;;; 0:the name 
		 (cdr list))))))

(defun nc-dired-to-list (name)
  "Generate a list of files and attributes for
the given directory"
  (let ((files
	 (delete
	  nil
	  (mapcar '(lambda (x)
		     (if (string= (file-name-nondirectory x) ".")
			 nil
		       (nc-file-attributes-list x)))
		  (directory-files name t)))))
    (nc-sort-dired-files files)))

(defun nc-adjust-and-show (&optional soft screen no-right-align no-frame totals no-cursor)
  (cond
   ((nc-adjust-screen soft no-right-align) nil)
   (screen (nc-display-buffer nil no-frame)))
  (nc-write-modeline)
  (if totals (nc-write-totals-line))
  (or no-cursor (nc-show-cursor nc-cursor-col nc-cursor-lin)))

(defun nc-adjust-screen (&optional soft no-right-align)
  "Puts the position of the selected file into reasonable ranges.
If SOFT is t, will change the first-column-to-show as little as possible.
If NO_RIGHT_ALIGN is nil, and the last file is shown, will put it in the
last line of the last column, if possible.
Will redisplay buffer if needed, returns t in this case."
  (if (>= nc-cursor-col (length nc-format))
      (setq nc-cursor-col (- (length nc-format) 1)
	    nc-cursor-lin nc-nl))
  (if (>= nc-cursor-lin
  	  (length (elt nc-format nc-cursor-col)))
      (setq nc-cursor-lin (- (length (elt nc-format nc-cursor-col)) 1)))
  (cond ((> nc-cursor-col (+ -1 nc-columns nc-first-column))
	 (cond
	  (soft (setq nc-first-column (- nc-cursor-col nc-columns -1)))
	  (t (setq nc-first-column
		   (max 0 (- (length nc-format) nc-columns)))))	;????
	 (if (and (null no-right-align) (= (+ nc-columns nc-first-column)
					   (length nc-format)))
	     (setq nc-format-align-right
		   (- (- nc-nl 1) (% (- (length (cdr nc-files)) 1)
			    nc-nl))
		   nc-cursor-lin (- nc-nl 1))
	   (setq nc-format-align-right 0))
	 (nc-display-buffer nil t)
	 t)
	((< nc-cursor-col nc-first-column)
	 (cond ((not soft)
		(setq nc-first-column 0
		      nc-cursor-col 0
		      nc-cursor-lin 0))
	       (t (setq nc-first-column nc-cursor-col)))
	 (if (= 0 nc-first-column)
	     (setq nc-format-align-right 0))
	 (nc-display-buffer)
	 t)))

(defun nc-cursor-left (n)
  "Move file cursor left N columns."
  (interactive "p")
  (nc-cursor-right (- n)))
    
(defun nc-cursor-pane-left (n)
  "Move file cursor left N panes."
  (interactive "p")
  (nc-cursor-right (- (* n nc-columns))))
    
(defun nc-cursor-pane-right (n)
  "Move file cursor right N panes."
  (interactive "p")
  (nc-cursor-right (* n nc-columns)))
    
(defun nc-cursor-up ()
  "Move file cursor up one line"
  (interactive)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
  (cond ((>  nc-cursor-lin
	     0)
	 (setq nc-cursor-lin (- nc-cursor-lin 1)))
	((>  nc-cursor-col
	     0)
	 (setq nc-cursor-col (- nc-cursor-col 1)
	       nc-cursor-lin (- nc-nl 1))))
  ;;(nc-adjust-and-show t)
  (nc-adjust-and-show t nil nil t)
  (force-mode-line-update))

(defun nc-cursor-right (n)
  "Move file cursor right N columns."
  (interactive "p")
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
  (let ((old-lin nc-cursor-lin))
    (setq nc-cursor-col (+ nc-cursor-col n))
    (cond ((< nc-cursor-col 0)
	   (setq nc-cursor-col 0
		 nc-cursor-lin 0))
	  ((< nc-cursor-col (length nc-format))
	   (if (< nc-cursor-lin
		  (length
		   (elt nc-format
			nc-cursor-col)))
	       nil
	     (setq nc-cursor-lin
		   (1- (length (elt nc-format
				    nc-cursor-col))))))
	  (t
	   (setq nc-cursor-col (1- (length nc-format)))
	   (setq nc-cursor-lin
		   (1- (length (elt nc-format
				    nc-cursor-col))))))
    ;; (nc-adjust-and-show t nil t)
    (nc-adjust-and-show t nil t t)
    (force-mode-line-update)))

(defun nc-cursor-home ()
  "Move file cursor to first position"
  (interactive)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
  (setq nc-cursor-col 0
	nc-cursor-lin 0)
  ;;(nc-adjust-and-show)
  (nc-adjust-and-show nil nil nil t)
  (force-mode-line-update))

(defun nc-cursor-end ()
  "Move file cursor to last position"
  (interactive)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
  (setq nc-cursor-col (- (length nc-format) 1)
	nc-cursor-lin (if (> (length nc-format) nc-columns)
			  (- nc-nl 1)
			(- (length
			    (elt nc-format (- (length nc-format) 1)))
			   1)))
  ;;(nc-adjust-and-show)
  (nc-adjust-and-show nil nil nil t)
  (force-mode-line-update))

(defun nc-cursor-down ()
  "Move file cursor down one line"
  (interactive)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
    (cond ((< (+ 1 nc-cursor-lin)
	      (length (elt nc-format
			   nc-cursor-col)))
	   (setq nc-cursor-lin (+ 1 nc-cursor-lin)) )
	  ((< (+ 1 nc-cursor-col)
	      (length nc-format))
	   (setq nc-cursor-col (+ 1 nc-cursor-col)
		 nc-cursor-lin 0)))
    (nc-adjust-and-show t nil nil t)
    (force-mode-line-update))

(defun nc-set-cursor (&optional click)	; 19.29 provides argument
  "Set cursor position by mouse in File Browser"
  (interactive)
  (if nc-other
      (save-excursion
	(set-buffer nc-other)
	(nc-delete-cursor nc-cursor-col nc-cursor-lin)
	(nc-deselect-buffer)))
  (nc-deselect-buffer)
  (mouse-set-point last-nonmenu-event)
  (nc-select-buffer)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
  (let ((newpos (nc-relative-from-absolute-pos
		 (elt (elt last-nonmenu-event 1) 1))))
    (cond ((>= (car newpos) 0)
	   (cond ((or (< (car newpos) (- (length nc-format) 1))
		      (and (= (car newpos) (- (length nc-format) 1))
			   (< (car (cdr newpos))
			      (length (elt nc-format
					   (- (length nc-format) 1))))))
		  (setq nc-cursor-col (car newpos)
			nc-cursor-lin (car (cdr newpos))))
		 (t (setq nc-cursor-col (- (length nc-format) 1)
			  nc-cursor-lin (- (length
					    (elt nc-format
						 (- (length nc-format) 1)))
					   1))))))
    (nc-show-cursor nc-cursor-col nc-cursor-lin)
    (nc-write-modeline)
    (nc-sh-process-cd default-directory)))

(defun nc-other-buffer ()
  "Select other Browser buffer"
  (interactive)
  (nc-mode t)
  (nc-deselect-buffer)
  (set-buffer nc-other)
  (select-window (get-buffer-window (current-buffer)))
  (nc-select-buffer)
  (nc-sh-process-cd default-directory))

(defun nc-valid-dirname-p (name)
  (memq (aref name
	      (- (length name) 1))
	'(92 47)))  ;; should the backslash be in here?

(defun nc-rescan ()
  "Rescan current directory"
  (interactive)
  (setq nc-files (list (list default-directory)))
  ;;(setq nc-files (nc-find-files-for-buffer 1))
  ;;(or (nc-adjust-screen)
  (nc-display-buffer 1)
  ;;  )
  (nc-setup-cursor)
  (nc-sh-process-cd default-directory))

(defun nc-display-new-dir (&optional dir do-not-update-shell)
  "Display directory dir in buffer"
  (interactive "DChange to directory: ")
  ;;(or dir (setq dir default-directory))
  (cond (dir
	 (or (nc-valid-dirname-p dir)
	     (setq dir (format "%s/" dir)))
	 (setq dir (expand-file-name dir))
	 (setq nc-files (list (list dir)))
	 ;;(setq nc-files (nc-find-files-for-buffer 1))
	 (setq default-directory dir)
	 (setq nc-cursor-col 0
	       nc-cursor-lin 0
	       nc-selected-files nil)
	 ;;(or (nc-adjust-screen)
	 (nc-display-buffer 1)
	 ;;    )
	 (nc-setup-cursor)
	 (or do-not-update-shell (nc-sh-process-cd dir)))
	(t (nc-rescan))))

(defun nc-act-on-line-mouse ()
  "Goto the directory in line the mouse clicks on"
  (interactive)
  (nc-deselect-buffer)
  (mouse-set-point last-nonmenu-event)
  (nc-select-buffer)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin)
  (let ((newpos (nc-relative-from-absolute-pos
		 (elt (elt last-nonmenu-event 1) 1))))
    (cond ((>= (car newpos) 0)
	   (cond ((or (< (car newpos) (- (length nc-format) 1))
		      (and (= (car newpos) (- (length nc-format) 1))
			   (< (car (cdr newpos))
			      (length (elt nc-format
					   (- (length nc-format) 1))))))
		  (setq nc-cursor-col (car newpos)
			nc-cursor-lin (car (cdr newpos))))))))
  (nc-act-on-line)
  (nc-show-cursor nc-cursor-col nc-cursor-lin)
  (nc-write-totals-line)
  (nc-write-modeline))

  
(defun nc-act-on-line ()
  "Goto the directory in line"
  (interactive)
  (setq buffer-read-only nil)
  (let* ((abspos (nc-num-from-col-lin nc-cursor-col nc-cursor-lin))
	 (file (elt (cdr nc-files)
		    abspos))
	 (filename (elt file 5)))
    (cond ((string= filename "..")
	   (let ((par (nc-parent-dir)))
	     (if par
		 (nc-display-new-dir par))))
	  ((elt file 1)
	   (let ((newname (if (nc-valid-dirname-p (car (car nc-files)))
			      (format "%s%s/" (car (car nc-files)) filename)
			    (format "%s/%s/"  (car (car nc-files)) filename))))
	     (nc-display-new-dir newname)))
	  ((or (and (eql system-type 'ms-dos)
		    (or (string= (substring
				  filename
				  (- (length filename) 4))
				 ".exe")
			(string= (substring filename
					    (- (length filename) 4))
				 ".bat")))
	       (and (not (eql system-type 'ms-dos))
		    (= ?x (elt (elt file 4) 3))))
	   (shell-command filename))
	  ((eq nc-default-action 'view)
	   (nc-view))
	  ((eq nc-default-action 'edit)
	   (nc-edit)))))

(defun nc-bufferkill-or-nc ()
  "If current buffer invoked from nc, kill and go back.
If current buffer part of nc, restore nc view."
  (interactive)
  (if (or (eql (current-buffer) (car (nc-buffers)))
	  (eql (current-buffer) (car (cdr (nc-buffers)))))
      (nc t t)
    (nc-bufferkill)))

(defun nc-bufferkill ()
  "Kill current-buffer going back to nc"
  (interactive)
  (if (not (memq (current-buffer) (nc-buffers))) nil
    (if (and (buffer-modified-p)
	     (y-or-n-p
	      (format "Save modified buffer %s before killing? "
		      (buffer-name (current-buffer)))))
	(save-buffer))
    (kill-buffer (current-buffer))
    (nc t t)))

(defun nc-bufferswitch ()
  "Switch to other buffer out of nc"
  (interactive)
  (call-interactively 'switch-to-buffer)
  (cond ((not (memq (current-buffer)
		    (nc-buffers)))
	 (delete-other-windows))
	((and (not (eql (current-buffer)
			(car (nc-buffers))))
	      (not (eql (current-buffer)
			(car (cdr (nc-buffers))))))
	 (delete-other-windows))
	(t (nc))))

(defun nc-set-local-keys ()
  (make-local-variable 'nc-local-map)
  (if (current-local-map)
      (setq nc-local-map (copy-keymap (current-local-map)))
    (setq nc-local-map (make-sparse-keymap)))
  (if (fboundp 'make-local-hook)
      (make-local-hook 'after-save-hook)
    (make-local-variable 'after-save-hook))
  (add-hook 'after-save-hook
	    (` (lambda () 
		 (nc-update-current-file (, (current-buffer))))))
  (define-key nc-local-map [27] nc-local-esc-map)
  (use-local-map nc-local-map)
  ;(local-set-key [27 27] 'nc-bufferkill)
  (local-set-key [\f10] 'nc-quit)
  (local-set-key "\C-xb" 'nc-bufferswitch)
  (local-set-key "\C-xk" 'nc-bufferkill)
  (message
   "Type `ESC ESC' in this buffer or `M-x nc RET' to get back"))

(defun nc-view-or-edit (read-only)
  (let* ((abspos (nc-num-from-col-lin nc-cursor-col nc-cursor-lin))
	 (file (elt (cdr nc-files)
		    abspos))
	 (ncbuf (current-buffer))
	 (filename (elt file 5)))
    (cond ((null (elt file 1))
	   (if read-only
	       (find-file-read-only filename)
	     (find-file filename))
	   (delete-other-windows)
	   (make-local-variable 'nc-associated-nc-buffer)
	   (setq nc-associated-nc-buffer ncbuf)
	   (if (not (memq (current-buffer)
			  (nc-buffers)))
	       (set-nc-buffers (append (nc-buffers)
					(list (current-buffer)))))
	   (nc-set-local-keys)))))

(defun nc-edit ()
  "Edit the current file in the brouwer."
  (interactive)
  (nc-view-or-edit nil))

(defun nc-view ()
  "View the current file in the brouwer."
  (interactive)
  (nc-view-or-edit t))

(defun nc-num-from-col-lin (col lin)
  (+ (* nc-nl col)  lin
     (- nc-format-align-right)))

(defun nc-col-from-num (num)
  (/ (+ num nc-format-align-right) nc-nl))

(defun nc-lin-from-num (num &optional col)
  (- num (- nc-format-align-right)
     (* nc-nl (if col
	       col
	     (/ (+ num nc-format-align-right) nc-nl)))))
     
(defun nc-delete-selected-files ()
  (let ((oldsel nc-selected-files))
    (while oldsel
      (nc-delete-select
       (nc-col-from-num (car oldsel))
       (nc-lin-from-num (car oldsel)))
      (setq oldsel (cdr oldsel)))))
      
(defun nc-convert-to-regexp (pattern)
  "Convert a file-name pattern to an emacs-regexp"
  (let ((len (- (length pattern) 1))
	(new ""))
    (while (>= len 0)
      (setq new
	    (cond ((and (= (elt pattern len) ?*);; an asterisk at beginn
			(= 0 len))
		   (format "[^.].*%s" new))
		  ((= (elt pattern  len) ?*);; an asterisk
		   (format ".*%s" new))
		  ((and (= (elt pattern len) ??);; a question-mark at beginn
			(= 0 len))
		   (format "[^.]%s" new))
		  ((= (elt pattern len) ??);; a question-mark
		   (format ".%s" new))
		  ((memq (elt pattern len) '(?. ?+));; dot or plus
		   (format "[%c]%s" (elt pattern len) new))
		  (t
		   (format "%c%s" (elt pattern len) new))))
      (setq len (- len 1)))
    new))

(defvar nc-select-hist '("*"))

(defun nc-toggle-selected ()
  "Toggle the list of selected files.
Directories will be unconditionally untoggled."
  (interactive)
  (setq buffer-read-only nil)
  (let ((files (1- (length nc-files)))
	(oldsel nc-selected-files))
    (nc-delete-selected-files)
    (setq nc-selected-files nil)
    (while (> files 0)
      (or (memq (1- files) oldsel)
	  (elt (elt nc-files files) 1)	; Is dir?
	  (eq (elt (elt nc-files files) 5) "..")
	  (setq nc-selected-files (cons (1- files) nc-selected-files)))
      (setq files (1- files))))
  (setq nc-total-size-selected nil)
  (nc-show-selected-files)
  (nc-show-cursor nc-cursor-col nc-cursor-lin)
  (nc-write-totals-line)
  (nc-write-modeline)
  (setq buffer-read-only t))

(defun nc-select-deselect-pattern (pat deselect regexp)
  (or pat (setq pat (read-from-minibuffer (concat (if deselect "Des" "S")
						  "elect Files "
						  (if regexp
						      "Regexp"
						    "Pattern")
						  ": ")
					  (car nc-select-hist) nil nil 
					  '(nc-select-hist . 0))))
  (setq buffer-read-only nil)
  (let ((files (1- (length nc-files)))
	(newpat (if regexp pat 
		  (nc-convert-to-regexp pat)))
	(select (not deselect))
	member maketrue)
    (if deselect (nc-delete-selected-files))
    (while (> files 0)
      (let* ((beg (string-match newpat (elt (elt nc-files files) 5)))
	     (end (match-end 0)))
	(cond ((and beg
		    (= 0 beg)
		    (= (length (elt (elt nc-files files) 5))
		       end)
		    (or (if select nc-select-dir-wild  nc-deselect-dir-wild)
			(not (elt (elt nc-files files) 1)))
		    (setq member (memq (1- files) nc-selected-files)
			  maketrue t)
		    (if select (not member) member))
	       (setq nc-selected-files
		     (if select
			 (cons (1- files) nc-selected-files)
		       (delete (1- files) nc-selected-files))))))
      (setq files (1- files))))
  (setq nc-total-size-selected nil)
  (nc-show-selected-files)
  (nc-show-cursor nc-cursor-col nc-cursor-lin)
  (nc-write-totals-line)
  (nc-write-modeline)
  (setq buffer-read-only t))

(defun nc-deselect-pattern ()
  "deselect files according to filename pattern."
  (interactive)
  (nc-select-deselect-pattern nil t nc-select-by-regexp))

(defun nc-select-pattern ()
  "Select files according to filename pattern."
  (interactive)
  (nc-select-deselect-pattern nil nil nc-select-by-regexp))
    
(defun nc-deselect-regexp ()
  "deselect files according to regexp."
  (interactive)
  (nc-select-deselect-pattern nil t t))

(defun nc-select-regexp ()
  "Select files according to regexp."
  (interactive)
  (nc-select-deselect-pattern nil nil t))
    
(defun nc-select ()
  "Toggle select/deselect of current file"
  (interactive)
  (setq buffer-read-only nil)
  (let ((file (nc-num-from-col-lin nc-cursor-col
				      nc-cursor-lin)))
    (cond ((equal ".."
		  (elt (elt nc-files (+ file 1)) 5)) nil)
	  ((memq file nc-selected-files);; it is drin
	   (nc-delete-select nc-cursor-col nc-cursor-lin)
	   (setq nc-selected-files
		 (delete file nc-selected-files))
	   (nc-show-cursor nc-cursor-col nc-cursor-lin))
	  (t
	   ;;(nc-delete-selected-files)
	   (cond ((or nc-select-dir 
		      (null (elt (elt nc-files;; it mustn't be a DIR
				      (+ file 1)) 1)))
		  (setq nc-selected-files
			(cons (nc-num-from-col-lin nc-cursor-col
						   nc-cursor-lin)
			      nc-selected-files))
		  (nc-show-select nc-cursor-col nc-cursor-lin))))))
  (setq nc-total-size-selected nil)
  (nc-cursor-down)
  (nc-write-totals-line)
  (nc-write-modeline)
  (setq buffer-read-only t))

(defun nc-change-drive ()
  "Select a new drive in File Browser"
  (interactive)
  (let* ((newdrive last-nonmenu-event)
	 (newdir (format "%c:/" newdrive)))
    (cond ((directory-files newdir)
	   (setq buffer-read-only nil)
	   (setq nc-files (list (list newdir)))
	   (setq default-directory (expand-file-name newdir))
	   (setq nc-cursor-col 0
		 nc-cursor-lin 0)
	   (setq nc-selected-files nil)
	   (or (nc-adjust-screen)
	       (nc-display-buffer 0))
	   (nc-setup-cursor)
	   (setq buffer-read-only t)
	   (nc-sh-process-cd newdir)))))
	   
(defun nc-mkdir (name)
  "Create a directory from File Browser"
  (interactive "FDirectory Name: ")
  (if (nc-valid-dirname-p name)
      (setq name (substring name 0 (- (length name) 1))))
  (setq name (expand-file-name name))
  (cond ((file-directory-p name)
	 (message "The directory %s already exists!" name))
	((string= (file-name-directory  name)
		  default-directory)	; XXXX: check other windows?
	 (make-directory name)
	 (nc-update-all-modifications
	  (list (cons nil
		      name))
	  nil t)
;	 (setq nc-files
;	       (cons (car nc-files)
;		     (nc-sort-dired-files
;		      (cons
;		       (nc-file-attributes-list name)
;		       (cdr nc-files)))))
;	 (nc-display-buffer)
	 (nc-setup-cursor))
	(t (make-directory name))))

(defun nc-save-selections ()
  (let ((le (- (length nc-selected-files) 1)))
    (while (>= le 0)
      (rplaca (nthcdr 6 (elt (cdr nc-files)
			     (elt nc-selected-files le)))
	      t)
      (setq le (- le 1))))
  (setq nc-selected-files nil))

(defun nc-restore-selections ()
  (setq nc-selected-files nil)
  (let ((le (- (length nc-files) 1)))
    (while (> le 0)
      (if (elt (elt nc-files le) 6)
	  (setq nc-selected-files
		(cons (- le 1) nc-selected-files)))
      (rplaca (nthcdr 6 (elt nc-files le))
	      nil)
      (setq le (- le 1)))))
      
(defun nc-update-current-file (buff)
  "Update the display of the file in the buffer BUFF.
Called in the local after-save-hook to show new version"
  (interactive)
  (let ((buf (buffer-file-name buff)))
    (save-excursion
      (if nc-associated-nc-buffer (nc t t))
      (if buf
	  (nc-update-all-modifications
	   (list
	    (cons nil
		  buf))
	   nil t)))))

(defun nc-update-all-modifications (modi source-p dest-p)
  "update modifications for both buffers"
  ;; Problems: both buffers might be associated with
  ;; dirs that have diffenent names (via links etc)
  (let ((b (current-buffer)))
    (if (nc-buffers)
	(save-excursion
	  (set-buffer (car (nc-buffers)))
	  (nc-update-modifications modi source-p dest-p 
				   (eq b (current-buffer)))
	  (set-buffer (car (cdr (nc-buffers))))
	  (nc-update-modifications modi source-p dest-p 
				   (eq b (current-buffer)))))))

(defun nc-update-modifications (modi source-p dest-p active)
  (nc-save-selections)
  (if (and source-p;; remove source files from dir (moved or deleted)
	   (or (null (file-name-directory (car (car modi))))
	       (string= (file-truename
			 (expand-file-name
			  (file-name-directory (car (car modi)))))
			(file-truename (car (car nc-files))))))
      (let ((newfiles nil)
	    (oldfiles (cdr nc-files)))
	(while oldfiles
	  (let ((modis modi))
	    (while (and modis
			(not (string=
			      (file-name-nondirectory (car (car modis)))
			      (elt (car oldfiles) 5))))
	      (setq modis (cdr modis)))
	    (if (and modis (string= (file-name-nondirectory
				     (car (car modis)))
				    (elt (car oldfiles) 5)))
		nil
	      (setq newfiles (cons (car oldfiles) newfiles))))
	  (setq oldfiles (cdr oldfiles)))
	(setq nc-files (cons (car nc-files)
			     (reverse newfiles))
	      nc-num-files (- (length newfiles) nc-dotdot)
	      nc-total-size-files (nc-total-size newfiles)
	      nc-total-size-selected nil)))
  (if (and dest-p;; add dest files if nonexistent
	   (or (null (file-name-directory (cdr (car modi))))
	       (string= (file-truename
			 (expand-file-name
			  (file-name-directory (cdr (car modi)))))
			(file-truename (car (car nc-files))))))
      (let ((newfiles nil)
	    (oldfiles (cdr nc-files)))
	(while oldfiles;; 1 check for existing and replace
	  (let ((modis modi))
	    (while (and modis
			(not (string=
			      (file-name-nondirectory (cdr (car modis)))
			      (elt (car oldfiles) 5))))
	      (setq modis (cdr modis)))
	    (if (and modis
		     (string= (file-name-nondirectory (cdr (car modis)))
			      (elt (car oldfiles) 5)))
		(setq newfiles (cons (nc-file-attributes-list
				      (cdr (car modis)))
				     newfiles))
	      (setq newfiles (cons (car oldfiles) newfiles))))
	  (setq oldfiles (cdr oldfiles)))
	(let ((modis modi))
	  (while modis
	    (let ((oldfiles newfiles))
	      (while (and oldfiles;; 2 add nonexisting
			  (not (string= (file-name-nondirectory
					 (cdr (car modis)))
					(elt (car oldfiles) 5))))
		(setq oldfiles (cdr oldfiles)))
	      (if (or (and oldfiles
		       (string= (file-name-nondirectory
				 (cdr (car modis)))
				(elt (car oldfiles) 5)))
		      (not (file-exists-p (cdr (car  modis)))))
		  nil
		(setq newfiles
		      (cons  (nc-file-attributes-list
			      (cdr (car  modis)))
			     newfiles))))
	    (setq modis (cdr modis))))
	(setq nc-files
	      (cons (car nc-files)
		    (nc-sort-dired-files newfiles))
	      nc-num-files (- (length newfiles) nc-dotdot)
	      nc-total-size-files (nc-total-size newfiles)
	      nc-total-size-selected nil)))
  (setq nc-format (nc-format-files-for-buffer (cdr nc-files)))
  (nc-restore-selections)
  (if (get-buffer-window (current-buffer)) (nc-adjust-and-show 
					    nil t nil nil t (not active))))

(defun nc-get-other-default-dir ()
  "Get the default-directory of the other nc-buffer.
Used as target for copy, move operations"
  ;;(nc-mode)
  (save-excursion
    (set-buffer (cond ((eq (current-buffer) (car (nc-buffers)))
		       (car (cdr (nc-buffers))))
		      (t (car (nc-buffers)))))
    default-directory))

(defun nc-get-target-file (command &optional default-dir default-name)
  "Prompt for filename to operate on one file"
  (if (null default-dir)
      (setq default-dir (expand-file-name (nc-get-other-default-dir)))
    (setq default-dir (expand-file-name default-dir)))
  (or default-name 
      (setq default-name ""))
  (expand-file-name (read-file-name
		     command (concat default-dir default-name) nil nil)))

(defun nc-get-target-dir (command &optional default-dir)
  "Prompt for target of copy or move for multiple files."
  (if (null default-dir)
      (setq default-dir (nc-get-other-default-dir)))
  (let ((erg
	 (expand-file-name
	  (read-file-name command default-dir nil t))))
    (while (not (file-directory-p erg))
      (setq erg (expand-file-name
		 (read-file-name
		  (format "Please select a DIRECTORY for %s"
			  command default-dir)))))
    erg))

(defun nc-get-files-to-operate ()
  ;; Returns list: elts are (NAME FILESPEC ABSPOS)
  (cond ((null nc-selected-files)
	 (let* ((abspos (nc-num-from-col-lin nc-cursor-col
					     nc-cursor-lin))
		(file (elt (cdr nc-files) abspos))
		(filename (elt file 5)))
	   (list (list filename file abspos))))
	((= 1 (length nc-selected-files))
	 (let* ((col (nc-col-from-num (car nc-selected-files)))
		(lin (nc-lin-from-num (car nc-selected-files) col))
		(abspos (nc-num-from-col-lin col lin))
		(file (elt (cdr nc-files);;abspos
			   (car nc-selected-files)
			   ))
		(filename (elt file 5)))
	   (list (list filename file abspos))))
	(t (mapcar '(lambda (x)
		      (let* ((col (nc-col-from-num x))
			     (lin (nc-lin-from-num x col))
			     (abspos (+ (* nc-nl col) lin))
			     (file (elt (cdr nc-files);;abspos
					x
					))
			     (filename (elt file 5)))
			(list filename file abspos)))
		   nc-selected-files))))

(defun nc-get-target-dir-or-file (command &optional nodest target-dir)
  "Returns the target dir-or file and the source file(s)
for copy and move commands.
Return is a list (TARGET REST), rest (will) consists of conses (ORDINAL . NAME)."
  (let ((source (nc-get-files-to-operate)))
    (cons
     (if nodest
	 (car (car source))
       (cond ((= 1 (length source))
	      (nc-get-target-file
	       (format "%s %s %s to: " 
		       command
		       (if (nth 1 (nth 1 (car source))) "directory" "file")
		       (car (car source)))
	       target-dir
	       (car (car source))))
	     (t   (nc-get-target-dir
		   (format "%s %s files to: " command
			   (length source))
		   target-dir))))
     source)))

(defun nc-check-file-op (destname operation ovwrt)
  "If necessary, interact with user to find out
if file shall be operated on"
  (cond ((equal destname "..") 
	 (message "Cannot operate on `..'!")
	 nil)
	((or (and (file-exists-p destname)
		  (not (eql ovwrt 'none))
		  (not (eql ovwrt 'all)))
	     (eql ovwrt 'always))
	 (message "%s %s %s? [y]es [n]o [a]ll [N]one) "
		  operation
		  (cond ((file-symlink-p destname) "symlink")
			((file-directory-p destname) "directory")
			(t "file"))
		  destname)
	 (let ((answ (read-char)))
	   (while (not (memq answ '(?y ?Y ?n ?a ?A ?N)))
	     (message
	      "y/Y[es this file], n[ot this file], a/A[ll files], N[o files]")
	     (setq answ (read-char)))
	   (list (if (memq answ '(?y ?Y ?a ?A))
		     t
		   nil)
		 (cond ((memq answ '(?a ?A))
			'all)
		       ((eql answ ?N)
			'none)
		       (t ovwrt)))))
	((and (file-exists-p destname)
	      (eql ovwrt 'none))
	 (list nil 'none))
	((and (file-exists-p destname)
	      (eql ovwrt 'all))
	 (list t 'all))
	(t (list t ovwrt))))
	
(defun nc-move-or-copy-file (sourcename destname ovwrt op self operation)
  "Copy or move sourcename to destname.
Handle overwrite mode. Directories welcome.
Return new status of ovwrt-flag"
  (cond
   ((string-match "/\\.\\.?$" sourcename) ovwrt)
   ((and (file-directory-p sourcename) (not (file-symlink-p sourcename)))
    (cond ((file-exists-p destname)
	   (message "Cannot %s directory with overwrite, press a key" op)
	   (read-event)
	   (message ""))
	  ;; Bug: move to a different hard disk does not work
	  (t 
	   (make-directory destname)
	   (let* ((s-files (directory-files sourcename t nil t))
		  (d-files (mapcar '(lambda (x) 
				      (format "%s/%s" destname 
					      (file-name-nondirectory x)))
				   s-files)))
	     (while s-files
	       (setq ovwrt (funcall self (car s-files) (car d-files) ovwrt))
	       (setq s-files (cdr s-files)
		     d-files (cdr d-files))))
	   (setq nc-file-mod (cons 
			      (cons (expand-file-name sourcename) destname)
			      nc-file-mod))))
    ovwrt)
   (t   
    (let ((test (nc-check-file-op destname "Overwrite existing" ovwrt)))
      (cond ((car test)
	     (funcall operation sourcename destname t)
	     (setq nc-file-mod
		   (cons (cons (expand-file-name sourcename) destname)
			 nc-file-mod))))
      (car (cdr test))))))

(defun nc-copy-file (sourcename destname ovwrt)
  "Copy sourcename to destname.
Handle overwrite mode. Directories welcome.
Return new status of ovwrt-flag"
  (nc-move-or-copy-file sourcename destname ovwrt 
			"copy" 'nc-copy-file 'copy-file))

(defun nc-move-file (sourcename destname ovwrt)
  "Move/Rename sourcename to destname. Directories welcome."
  (nc-move-or-copy-file sourcename destname ovwrt 
			"move" 'nc-move-file 'rename-file))

(defun nc-delete-file (sourcename destname ovwrt)
  "delete sourcefile"
  (let ((test (nc-check-file-op sourcename "Delete" ovwrt)))
    (cond ((car test)
	   (nc-delete-file-or-directory sourcename t)
	   (setq nc-file-mod (cons (cons (expand-file-name sourcename) nil)
				   nc-file-mod))))
    (car (cdr test))))

(defun nc-delete-file-or-directory (name prompt)
  (if (and (file-directory-p name) (not (file-symlink-p name)))
      (let ((files (directory-files name t nil t)))
	;; We care about empty directories only at this moment:
	(while (and files (string-match "/\\.\\.?$" (car files)))
	  (setq files (cdr files)))
	(cond ((not files) (delete-directory name))
	      ((and prompt 
		    (not (yes-or-no-p 
			  (format
			   "Directory `%s' not empty. Really delete? " name))))
	       (message "Skipping `%s'..." name))
	      (t (while files
		   (if (string-match "/\\.\\.?$" (car files)) nil
		     (nc-delete-file-or-directory (car files) nil))
		   (setq files (cdr files)))
		 (delete-directory name))))
    (delete-file name)))

(defun nc-operate-on-files (sourcdest operation &optional ask)
  (setq nc-file-mod nil)
  (let ((source (cdr sourcdest))
	(target (car sourcdest))
	(dest nil)
	(ovwrt-flag nil))
    (if (and (file-directory-p target)
	     (not (nc-valid-dirname-p target)))
	(setq target (format "%s/" target)))
    (cond ((file-directory-p target)
	   (setq dest
		 (mapcar '(lambda (x)
			    (format "%s%s"
				    target (car x)))
			 source)))
	  (t (setq dest (list target))))
    (let ((ovwrt ask))
      (while source
	(setq ovwrt
	      (funcall operation
		       (car (car source)) (car dest) ovwrt)
	      source (cdr source)
	      dest (cdr dest))))))

(defvar nc-here nil)

(defun nc-copy-here ()
  "Copy one or more files (target defaults to current directory)."
  (interactive)
  (let ((nc-here t))
    (call-interactively 'nc-copy)))

(defun nc-move-here ()
  "Move or rename one or more files (target defaults to current directory)."
  (interactive)
  (let ((nc-here t))
    (call-interactively 'nc-move)))

(defun nc-copy (target)
  "Copy one or more files."
  (interactive
   (list (nc-get-target-dir-or-file "Copy"
				    nil
				    (if nc-here default-directory))))
  (nc-operate-on-files target 'nc-copy-file)
  (if nc-file-mod (nc-update-all-modifications nc-file-mod nil t))
  (nc-show-cursor nc-cursor-col nc-cursor-lin))	; protected by save-excursion otherwise, will put point in wrong position

(defun nc-move (target &optional here)
  "Move or rename one or more files."
  (interactive
   (list (nc-get-target-dir-or-file "Move/Rename"
				    nil
				    (if nc-here default-directory))))
  (nc-operate-on-files target 'nc-move-file)
  (if nc-file-mod (nc-update-all-modifications nc-file-mod t t))
  (nc-show-cursor nc-cursor-col nc-cursor-lin))	; protected by save-excursion otherwise, will put point in wrong position

(defun nc-delete (target)
  "Delete one or more files"
  (interactive
   (list (nc-get-target-dir-or-file "Delete" t)))
  (nc-operate-on-files target 'nc-delete-file 'always)
  (if nc-file-mod (nc-update-all-modifications nc-file-mod t nil))
  (nc-show-cursor nc-cursor-col nc-cursor-lin))	; protected by save-excursion otherwise, will put point in wrong position

(defun nc-dirsize (target)
  "Update sizes of one or more directories"
  (interactive
   (list (nc-get-target-dir-or-file "Delete" t)))
  (let ((files (cdr target)) filegroup file)
    (while files
      (setq filegroup (car files))
      (setq file (car (cdr filegroup))
	    filename (elt file 5))
      (rplaca (nthcdr 2 file) (nc-dir-size filename))
      (setq files (cdr files))))
  (setq nc-total-size-files (nc-total-size (cdr nc-files)))
  (if (memq 'size nc-show-list)
	(nc-redraw)
    ;; Need to update the display of selected files XXXXXXXXX
    (setq nc-total-size-selected nil
	  nc-total-size-files (nc-total-size (cdr nc-files)))
    (nc-write-totals-line)
    (nc-write-modeline)))

(defun nc-sort-one-buffers-files ()
  (setq nc-files
	(cons (car nc-files)
	      (nc-sort-dired-files
	       (mapcar '(lambda (x)
			  (cons
			   (nc-filename-norm x)
			   (cdr x)))
		       (cdr nc-files))))))

(defun nc-set-size ()
  (let ((ow nc-wd) (oc nc-cl) (oe nc-pane-end) (oh nc-nl) (w (get-buffer-window (current-buffer))))
    ;; It may be called with the current buffer switched away from current window.
    (cond (w 
	   (setq nc-wd (window-width w))
	   (if (>= nc-wd 8) nil
	     (message "Panes cannot be more narrow than 8 chars.")
	     (enlarge-window-horizontally (- 8 (window-width)))
	     (setq nc-wd (window-width)))
	   (setq nc-cl
		 (if (eql system-type 'ms-dos)
		     12
		   (1- (/ nc-wd nc-columns))))
	   (if (>= nc-cl 3) nil
	     (setq nc-columns (/ nc-wd 4) nc-cl (1- (/ nc-wd nc-columns)))
	     (message "Column too narrow. Reducing number of columns."))
	   (setq nc-wd (* nc-columns (1+ nc-cl)))
	   (setq nc-suffix-length
		 (cond ((eql system-type 'ms-dos)
			3)
		       (t nc-suffix-length))
		 ;;  ((< nc-cl 13)
		 ;;   3)
		 ;;  ((< nc-cl 17)
		 ;;   4)
		 ;;  (t 5))
		 )
	   (if (< (window-height w) 4) (error "Window too short"))
	   (setq nc-nl (- (window-height w) 
			  2		; Dirinfo line + modeline
			  (if nc-write-header 1 0)
			  (if nc-write-totals 1 0)))
	   (setq nc-pane-end (1+ (* (+ (if nc-write-header 2 1)
				       nc-nl) nc-wd))) ; Why 1+??? But works...
	   (and (eq ow nc-wd) (eq oc nc-cl) (eq oe nc-pane-end) (eq oh nc-nl))))))

(defun nc-redraw (&optional no-reset)
  "Redraw both panes, updating the size of panes."
  (interactive)
  ;;(nc-mode)
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (if (and (nc-set-size) no-reset) nil
    (nc-sort-one-buffers-files)
    (setq nc-first-column 0
	  nc-cursor-col 0
	  nc-cursor-lin 0
	  nc-format-align-right 0
	  nc-format (nc-format-files-for-buffer (cdr nc-files))))
  (or (nc-adjust-screen)
      (nc-display-buffer))
  (nc-setup-cursor)
  (setq buffer-read-only t)
  ;;(other-window 1)
  (save-excursion
    (set-buffer nc-other)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (if (and (nc-set-size) no-reset) nil
      (nc-sort-one-buffers-files)
      (setq nc-first-column 0
	    nc-cursor-col 0
	    nc-cursor-lin 0
	    nc-format-align-right 0
	    nc-format (nc-format-files-for-buffer (cdr nc-files))))
    (or (nc-adjust-screen)
	(nc-display-buffer))
    (nc-setup-cursor)
    (nc-deselect-buffer)
    (setq buffer-read-only t))
  ;;(other-window -1)
  (nc-select-buffer))
  
(defun nc-set-columns ()
  "Set the number of columns to the character typed."
  (interactive)
  (setq nc-columns (- last-command-char ?0))
  (nc-redraw))

(defun nc-columns-set (num)
  "Set the number of columns to the character typed."
  (setq nc-columns num)
  (nc-redraw))

(defun nc-sort-by-name ()
  (interactive)
  (cond ((not (eq nc-sort-by 'name))
	 (setq nc-sort-by 'name)
	 (nc-redraw))))

(defun nc-sort-by-time ()
  (interactive)
  (cond ((not (eq nc-sort-by 'time))
	 (setq nc-sort-by 'time)
	 (nc-redraw))))  

(defun nc-sort-by-size ()
  (interactive)
  (cond ((not (eq nc-sort-by 'size))
	 (setq nc-sort-by 'size)
	 (nc-redraw))))  

(defun nc-sort-by-ext ()
  (interactive)
  (cond ((not (eq nc-sort-by 'ext))
	 (setq nc-sort-by 'ext)
	 (nc-redraw))))  

(defun nc-sort-by-long-ext ()
  (interactive)
  (cond ((not (eq nc-sort-by 'long-ext))
	 (setq nc-sort-by 'long-ext)
	 (nc-redraw))))  

(defun nc-sort-reverse ()
  (interactive)
  (setq nc-sort-reversed (not nc-sort-reversed))
  (nc-redraw))



(defun nc-byte-compile ()
  "Byte compile one or more files in File Browser."
  (interactive)
  (let ((source (nc-get-files-to-operate)) filename list)
    (setq list source)
    (while source
      (nc-byte-compile-one-file (caar source))
      (setq source (cdr source)))
    (nc-update-all-modifications
     (mapcar (function (lambda (elt)
			 (cons nil (format "%sc"
			     ;;(car nc-files)
			     (car elt)))))
      list) nil t)))

;;;(defun nc-byte-compile-old ()		;; Obsolete
;;;  "Byte compile the file in File Browser"
;;;  (interactive)
;;;  (let* ((abspos (nc-num-from-col-lin nc-cursor-col nc-cursor-lin))
;;;	 (file (elt (cdr nc-files)
;;;		    abspos))
;;;	 (ncbuf (current-buffer))
;;;	 (filename (file-truename (elt file 5))))
;;;    (cond ((string= (substring filename
;;;			       (- (length filename)
;;;				  3))
;;;		    ".el")
;;;	   (byte-compile-file filename)
;;;	   (nc-update-all-modifications
;;;	    (list (cons nil (format "%sc"
;;;				    ;(car nc-files)
;;;				    filename)))
;;;	    nil t))
;;;	  (t (message "I will only byte-compile files ending .el !")))))

(defun nc-byte-compile-one-file (name)
  (let* ((filename (file-truename name)))
    (cond ((string= (substring filename
			       (- (length filename)
				  3))
		    ".el")
	   (byte-compile-file filename))
	  (t (message "I will only byte-compile files ending .el ! (%s)" 
		      filename)))))

(defun nc-quit (flag)
  "leave File Browser"
  (interactive
   (list (y-or-n-p "Sure you want to quit File Browsing? ")))
  (if flag
      (let ((bufs (nc-buffers))
	    (buffers (buffer-list)))
	(while bufs
	  (cond ((memq (car bufs) buffers)
		 (switch-to-buffer (car bufs))
		 (setq nc-panes (delete (current-buffer) nc-panes))
		 (setq nc-associated-nc-buffer nil)
		 (kill-buffer (car bufs))))
	  (setq bufs (cdr bufs)))
	(toggle-scroll-bar nc-sb)
	;;(setq after-save-hook nil)
	(set-nc-buffers nil)
	(delete-other-windows)
	(message "Bye... Type `M-x nc' to invoke me again"))))

(defun nc-define-drive-keys ()
  (define-key nc-mode-map "a" 'nc-change-drive)
  (define-key nc-mode-map "b" 'nc-change-drive)
  (define-key nc-mode-map "c" 'nc-change-drive)
  (define-key nc-mode-map "d" 'nc-change-drive)
  (define-key nc-mode-map "e" 'nc-change-drive)
  (define-key nc-mode-map "f" 'nc-change-drive)
  (define-key nc-mode-map "g" 'nc-change-drive)
  (define-key nc-mode-map "h" 'nc-change-drive)
  (define-key nc-mode-map "i" 'nc-change-drive)
  (define-key nc-mode-map "j" 'nc-change-drive)
  (define-key nc-mode-map "k" 'nc-change-drive)
  (define-key nc-mode-map "l" 'nc-change-drive)
  (define-key nc-mode-map "m" 'nc-change-drive)
  (define-key nc-mode-map "n" 'nc-change-drive)
  (define-key nc-mode-map "o" 'nc-change-drive)
  (define-key nc-mode-map "p" 'nc-change-drive)
  (define-key nc-mode-map "q" 'nc-change-drive)
  (define-key nc-mode-map "r" 'nc-change-drive)
  (define-key nc-mode-map "s" 'nc-change-drive))

(defun nc-set-keys ()
  (define-key nc-mode-map [27] nc-local-esc-map)
  ;(local-set-key [27 27] 'nc)
  (define-key nc-mode-map  [14] 'nc-cursor-down)
  (define-key nc-mode-map  [16] 'nc-cursor-up)
  (define-key nc-mode-map  [2] 'nc-cursor-left)
  (define-key nc-mode-map  [6] 'nc-cursor-right)
  (define-key nc-mode-map  [\down] 'nc-cursor-down)
  (define-key nc-mode-map  [\up] 'nc-cursor-up)
  (define-key nc-mode-map  [\left] 'nc-cursor-left)
  (define-key nc-mode-map  [\right] 'nc-cursor-right)
  ;;(define-key nc-mode-map [\mouse-1] 'nc-set-cursor)
  (define-key nc-mode-map [down-mouse-1] 'nc-set-cursor)
  (define-key nc-mode-map [double-down-mouse-1] 'nc-act-on-line-mouse)
  (define-key nc-mode-map [\mouse-2] 'nc-act-on-line-mouse)
  (define-key nc-mode-map [?\r] 'nc-act-on-line)
  (define-key nc-mode-map [?\t] 'nc-other-buffer)
  (define-key nc-mode-map [\home] 'nc-cursor-home)
  (define-key nc-mode-map "\C-a" 'nc-cursor-home)
  (define-key nc-mode-map [\prior] 'nc-cursor-pane-left)
  (define-key nc-mode-map "\ev" 'nc-cursor-pane-left)
  (define-key nc-mode-map [pageup] 'nc-cursor-pane-left)
  (define-key nc-mode-map [\end] 'nc-cursor-end)
  (define-key nc-mode-map "\C-e" 'nc-cursor-end)
  (define-key nc-mode-map [\next] 'nc-cursor-pane-right)
  (define-key nc-mode-map [pagedown] 'nc-cursor-pane-right)
  (define-key nc-mode-map "\C-v" 'nc-cursor-pane-right)
  (define-key nc-mode-map [\insert] 'nc-select)
  (define-key nc-mode-map "+" 'nc-select-pattern)
  (define-key nc-mode-map [kp-add] 'nc-select-pattern)
  (define-key nc-mode-map [kp-separator] 'nc-select-pattern)
  (define-key nc-mode-map [S-kp-add] 'nc-select-regexp)
  (define-key nc-mode-map [S-kp-separator] 'nc-select-regexp)
  (define-key nc-mode-map "-" 'nc-deselect-pattern)
  (define-key nc-mode-map [kp-subtract] 'nc-deselect-pattern)
  (define-key nc-mode-map [S-kp-subtract] 'nc-deselect-regexp)
  (define-key nc-mode-map "*" 'nc-toggle-selected)
  (define-key nc-mode-map [kp-multiply] 'nc-toggle-selected)
  (define-key nc-mode-map [\f1] 'nc-display-help-short)
  (define-key nc-mode-map [\f2] 'nc-display-new-dir)
  (define-key nc-mode-map [S-f2] 'nc-redraw)
  (define-key nc-mode-map "\C-l" 'nc-redraw)
  (define-key nc-mode-map "\C-r" 'nc-rescan)
  (define-key nc-mode-map [\f3] 'nc-view)
  (define-key nc-mode-map [\f4] 'nc-edit)
  (define-key nc-mode-map [\f5] 'nc-copy)
  (define-key nc-mode-map [S-f5] 'nc-copy-here)
  (define-key nc-mode-map [\f6] 'nc-move)
  (define-key nc-mode-map [S-f6] 'nc-move-here)
  (define-key nc-mode-map [\f7] 'nc-mkdir)
  (define-key nc-mode-map [\f8] 'nc-delete)
  (define-key nc-mode-map [\f10] 'nc-quit)
  (define-key nc-mode-map "\C-xo" 'nc-other-buffer)
  (define-key nc-mode-map "1" 'nc-set-columns)
  (define-key nc-mode-map "2" 'nc-set-columns)
  (define-key nc-mode-map "3" 'nc-set-columns)
  (define-key nc-mode-map "4" 'nc-set-columns)
  (define-key nc-mode-map "5" 'nc-set-columns)
  (define-key nc-mode-map "6" 'nc-set-columns)
  (define-key nc-mode-map "7" 'nc-set-columns)
  (define-key nc-mode-map "8" 'nc-set-columns)
  (define-key nc-mode-map "9" 'nc-set-columns)
  (define-key nc-mode-map "T" 'nc-sort-by-time)
  (define-key nc-mode-map "S" 'nc-sort-by-size)
  (define-key nc-mode-map "E" 'nc-sort-by-ext)
  (define-key nc-mode-map "X" 'nc-sort-by-long-ext)
  (define-key nc-mode-map "R" 'nc-sort-reverse)
  (define-key nc-mode-map "N" 'nc-sort-by-name)
  (define-key nc-mode-map "#" 'nc-toggle-show-size)
  (define-key nc-mode-map "_" 'nc-toggle-show-date)
  (define-key nc-mode-map "@" 'nc-toggle-show-time)
  (define-key nc-mode-map "!" 'nc-toggle-show-attr)
  (define-key nc-mode-map [C-f3] 'nc-sort-by-name)
  (define-key nc-mode-map [C-f4] 'nc-sort-by-ext)
  (define-key nc-mode-map [S-C-f4] 'nc-sort-by-long-ext)
  (define-key nc-mode-map [C-f5] 'nc-sort-by-time)
  (define-key nc-mode-map [C-f6] 'nc-sort-by-size)
  (define-key nc-mode-map [C-f8] 'nc-sort-reverse)
  (define-key nc-mode-map "B" 'nc-byte-compile)
  (define-key nc-mode-map "\C-xb" 'nc-bufferswitch)
  (define-key nc-mode-map "\C-Q" 'nc-dirsize)
  (define-key nc-mode-map "Q" 'nc-quit)
  (if nc-dosish
      (nc-define-drive-keys))
  (condition-case nil
      (progn
	(require 'easymenu)
	(easy-menu-define
	 nc-main-menu
	 nc-mode-map
	 "NC mode main menu"
	 '("NC"
	   ["View" nc-view t]
	   ["Edit" nc-edit t]
	   "---"
	   ["Copy" nc-copy t]
	   ["Copy here" nc-copy-here t]
	   ["Move" nc-move t]
	   ["Move here" nc-move-here t]
	   ["Make directory" nc-mkdir t]
	   ["Delete" nc-delete t]
	   "---"
	   ("Selection"
	    ["Select by pattern" nc-select-pattern t]
	    ["Deselect by pattern" nc-deselect-pattern nc-selected-files]
	    ["Select by regexp" nc-select-regexp t]
	    ["Deselect by regexp" nc-deselect-regexp nc-selected-files]
	    ["Toggle" nc-toggle-selected t]
	    ["Select all" (nc-select-deselect-pattern ".*" nil t) t]
	    ["Deselect all" 
	     (nc-select-deselect-pattern ".*" t t) 
	     nc-selected-files])
	   "---"
	   ("Sort by"
	    ["Size" nc-sort-by-size t]
	    ["Extension" nc-sort-by-ext t]
	    ["Long extension" nc-sort-by-long-ext t]
	    ["Time" nc-sort-by-time t]
	    ["Name" nc-sort-by-name t]
	    ["Reverse" nc-sort-reverse t])
	   ("Toggle"
	    ["Show size" nc-toggle-show-size t]
	    ["Show date" nc-toggle-show-date t]
	    ["Show time" nc-toggle-show-time t]
	    ["Show attributes" nc-toggle-show-attr t]
	    "---"
	    ["Show headers" nc-toggle-write-title t]
	    ["Show totals" nc-toggle-write-totals t])
	   ("Columns"
	    ["1    1" (nc-columns-set 1) t]
	    ["2    2" (nc-columns-set 2) t]
	    ["3    3" (nc-columns-set 3) t]
	    ["4    4" (nc-columns-set 4) t]
	    ["5    5" (nc-columns-set 5) t]
	    ["6    6" (nc-columns-set 6) t]
	    ["7    7" (nc-columns-set 7) t]
	    ["8    8" (nc-columns-set 8) t]
	    ["9    9" (nc-columns-set 9) t]
	    ["10" (nc-columns-set 10) t])
	   "-----"
	   ["DirSize" nc-dirsize t]
	   "-----"
	   ["Open directory" nc-display-new-dir t]
	   ["Rescan" nc-rescan t]
	   ["Redraw" nc-redraw t]
	   ["Quit" nc-quit t]
	   )))
    (error (message "Cannot create NC menu"))))

(defun nc-setup-current-buffer (other)
  ;;(goto-char 0)
  (setq major-mode 'nc-mode)
  (use-local-map nc-mode-map)
  (setq mode-name "NC")
  (auto-fill-mode 0)
  (buffer-disable-undo)
  (make-local-variable 'nc-sb)
  (make-local-variable 'nc-modeline)
  (make-local-variable 'nc-write-header)
  (make-local-variable 'nc-write-totals)
  (make-local-variable 'nc-other)
  (setq nc-other other)
  (make-local-variable 'nc-show-list)
  (setq mode-line-format (list "" 'nc-modeline " " global-mode-string))
  (or nc-sb
      (setq nc-sb 
	    (if (cdr (assoc 'vertical-scroll-bars (frame-parameters))) 1 0)))
  (toggle-scroll-bar 0)
;  (make-local-variable 'stack-trace-on-error)
;  (setq stack-trace-on-error t)
  (make-local-variable 'nc-columns)
  (make-local-variable 'nc-sort-by)
  (make-local-variable 'nc-sort-reversed)
  (make-local-variable 'nc-sort-dired-files)
  (make-local-variable 'nc-cursor-col)
  (make-local-variable 'nc-cursor-lin)
  (make-local-variable 'nc-files)
  (make-local-variable 'nc-first-column)
  (make-local-variable 'nc-format)
  (make-local-variable 'nc-format-align-right)
  (make-local-variable 'nc-title-start)
  (make-local-variable 'nc-title-end)
  (make-local-variable 'nc-selected-files)
  (make-local-variable 'nc-associated-nc-buffer)
  (make-local-variable 'nc-file-mod)
  (make-local-variable 'nc-cl)
  (make-local-variable 'nc-wd)
  (make-local-variable 'nc-nl)
  (make-local-variable 'nc-dotdot)
  (make-local-variable 'nc-num-files)
  (make-local-variable 'nc-total-size-files)
  (make-local-variable 'nc-num-selected)
  (make-local-variable 'nc-total-size-selected)
  (make-local-variable 'nc-pane-end)
  (make-local-variable 'nc-is-shell)
  (setq nc-associated-nc-buffer (current-buffer))
  (setq nc-first-column 0
	nc-cursor-col 0
	nc-cursor-lin 0)
  (nc-set-size))

(defun nc-deselect-buffer ()
  ;;(setq buffer-read-only nil)
  (if window-system (delete-overlay nc-dir-overlay))
  ;;(put-text-property nc-title-start nc-title-end 'face 'default)
  ;;(setq buffer-read-only t)
  (nc-delete-cursor nc-cursor-col nc-cursor-lin))

(defun nc-select-buffer ()
  ;;(setq buffer-read-only nil)
  (if window-system
      (progn
	(move-overlay nc-dir-overlay 
		      nc-title-start nc-title-end (current-buffer))
	(overlay-put nc-dir-overlay 
		     'face nc-highlight-face)))
  ;;(put-text-property nc-title-start nc-title-end 'face 'nc-highlight-face)
  ;;(setq buffer-read-only t)
  (nc-show-cursor nc-cursor-col nc-cursor-lin)
  (setq nc-active-nc-buffer (current-buffer)))

(defvar nc-shell-buf-keymap nil)

(defun nc-setup-shell-buf (b)
  (save-excursion
    (set-buffer b)
    (require 'shell)
    (let* ((prog (or explicit-shell-file-name
		       (getenv "ESHELL")
		       (getenv "SHELL")
		       "/bin/sh"))		     
	     (name (file-name-nondirectory prog))
	     (startfile (concat "~/.emacs_" name))
	     (xargs-name (intern-soft (concat "explicit-" name "-args")))
	     shell-buffer)
	(save-excursion
	  (set-buffer (apply 'make-comint "NC shell" prog
			     (if (file-exists-p startfile) startfile)
			     (if (and xargs-name (boundp xargs-name))
				 (symbol-value xargs-name)
			       '("-i"))))
	  (setq shell-buffer (current-buffer))
	  (shell-mode)
	  (setq comint-scroll-to-bottom-on-output t))
	(pop-to-buffer shell-buffer))
    (make-local-variable 'window-min-height)
    (setq window-min-height 1)
    (setq nc-shell-buf-keymap (copy-keymap shell-mode-map))
    (use-local-map nc-shell-buf-keymap)
    (define-key nc-shell-buf-keymap "\e\e" 'nc-sh-escesc)
    (define-key nc-shell-buf-keymap "\t" 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap "\C-v" 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap "\M-v" 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [up] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [down] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [left] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [right] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [M-left] 'backward-char)
    (define-key nc-shell-buf-keymap [M-right] 'forward-char)
    (define-key nc-shell-buf-keymap [pageup] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [pagedown] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [home] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [end] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [f2] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [S-f2] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [f3] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [f4] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [f5] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [f6] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [f7] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [f8] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [S-f5] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [S-f6] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [C-f3] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [C-f4] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [S-C-f4] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [C-f5] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [C-f6] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [?\C-l] 'nc-sh-delegate)
;;;    (define-key nc-shell-buf-keymap [?\C-q] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [?\C-r] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [C-home] 'nc-sh-begin)
    (define-key nc-shell-buf-keymap [C-end] 'nc-sh-end)
    (define-key nc-shell-buf-keymap [?\C-l] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [insert] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [kp-multiply] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [kp-subtract] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [kp-add] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [kp-separator] 'nc-sh-delegate)
    (define-key nc-shell-buf-keymap [M-insert] 'overwrite-mode)
    (define-key nc-shell-buf-keymap [A-insert] 'overwrite-mode)
    (define-key nc-shell-buf-keymap [M-right] 'forward-char)
    (define-key nc-shell-buf-keymap [A-right] 'forward-char)
    (define-key nc-shell-buf-keymap [M-left] 'backward-char)
    (define-key nc-shell-buf-keymap [A-left] 'backward-char)
    (if window-system (font-lock-mode))))

(defun nc-sh-escesc ()
  "Alternately: make one-window or nc-panes."
  (interactive)
  (cond 
   ((one-window-p) (nc))
   (t (delete-other-windows))))

(defun nc-sh-end ()
  (interactive)
  (cond ((and nc-active-nc-buffer
	      (get-buffer-window nc-active-nc-buffer))
	   (end-of-line))
	(t (end-of-buffer))))

(defun nc-sh-begin ()
  (interactive)
  (cond ((and nc-active-nc-buffer
	      (get-buffer-window nc-active-nc-buffer))
	   (comint-bol nil))
	(t (beginning-of-buffer))))

(defadvice shell-cd (after nc-sh-catch-cd (dir) activate)
  "Switch directory of the active pane when cd is performed in the shell window."
  (cond (nc-active-nc-buffer
	 (save-excursion
	   (set-buffer nc-active-nc-buffer)
	   (setq dir (file-name-as-directory (expand-file-name dir)))
	   (or (equal default-directory dir)
	       (nc-display-new-dir dir t))))))

(defun nc-sh-delegate ()
  (interactive)
  (let ((keys (this-command-keys)) com)
    (cond ((and nc-active-nc-buffer
		(get-buffer-window nc-active-nc-buffer)
		(setq com (lookup-key nc-mode-map keys)))
	   (save-window-excursion
	     (set-buffer nc-active-nc-buffer)
	     (call-interactively com)))
	  ((setq com (or (lookup-key shell-mode-map keys)
			 (lookup-key global-map keys)))
	   (call-interactively com)))))

(defun nc-sh-process-cd (dir)
  (if (get-buffer nc-shell-buf)
      (let (b after)
	(save-excursion
	  (set-buffer (get-buffer nc-shell-buf))
	  (goto-char (point-max))
	  (if (bolp) (setq after t))
	  (comint-previous-prompt 1)	; 1 before 19.31?
	  (or after (comint-next-prompt 1))
	  (setq b (point))
	  (insert "cd " dir "\n")
	  (goto-char b)
	  (beginning-of-line)
	  (setq b (point-marker))
	  (end-of-line)
	  (comint-send-input)
	  (goto-char b)
	  (delete-region b (progn (end-of-line) (1+ (point))))
	  (end-of-line)
	  (delete-char 1)))))

(defun nc-switch-to-buffers (&optional keep-size)
  "Select buffers for File Browser"
  (let ((buffers (buffer-list)) w (b 1) b1)
    (cond ((and (nc-buffers)
		(memq (car (nc-buffers)) buffers)
		(memq (car (cdr (nc-buffers))) buffers))
	   (cond 
	    (nc-show-shell
	     (setq b1 (get-buffer nc-shell-buf))
	     (if b1 nil
	       (setq b1 (get-buffer-create nc-shell-buf))
	       (nc-setup-shell-buf b1))
	     (setq w (get-buffer-window b1))
	     (if (and w (not (one-window-p t 'only-this-frame)))
		 (select-window w)
	       (switch-to-buffer b1)
	       (delete-other-windows)
	       (split-window)
	       (other-window 1)
	       (enlarge-window (- 2 (window-height)))
	       (other-window 1))))
	   (setq w (get-buffer-window (car (nc-buffers))))
	   (if w (select-window w)
	     (switch-to-buffer (car (nc-buffers))))
	   ;;(goto-char 0)
	   (setq buffer-read-only t)
	   ;;(delete-other-windows)
	   ;;(split-window-horizontally)
	   (if (or (one-window-p t 'only-this-frame)
		   (and nc-show-shell
			(eq (next-window nil 'no-minibuf 'only-this-frame)
			    (previous-window nil 'no-minibuf 'only-this-frame))))
	     (progn (split-window-horizontally)
		    (if (and keep-size (not (eq nc-wd (window-width))))
			(enlarge-window-horizontally 
			 (- nc-wd (window-width))))))
	   (nc-set-size)
	   (other-window 1)
	   (switch-to-buffer (car (cdr (nc-buffers))))
	   (nc-set-size)
	   ;;(goto-char 0)
	   (setq buffer-read-only t)
	   (other-window -1)
	   (switch-to-buffer (car (nc-buffers))))
	  (t
	   (cond 
	    (nc-show-shell
	     (setq b1 (get-buffer nc-shell-buf))
	     (if b1 nil
	       (setq b1 (get-buffer-create nc-shell-buf))
	       (nc-setup-shell-buf b1))
	     (setq w (get-buffer-window b1))
	     (if w (select-window w)
	       (switch-to-buffer b1))
	     (delete-other-windows)
	     (split-window)
	     (other-window 1)
	     (enlarge-window (- 2 (window-height)))
	     (other-window 1)))
	   (while (member (format "*NC %s*" b) nc-panes) (setq b (1+ b)))
	   (switch-to-buffer (format "*NC %s*" b))
	   (setq nc-panes (cons (current-buffer) nc-panes))
	   (set-nc-buffers (list (current-buffer)))
	   (setq b1 (current-buffer)
		 b (1+ b))
	   (while (member (format "*NC %s*" b) nc-panes) (setq b (1+ b)))
	   (nc-setup-current-buffer (get-buffer-create (format "*NC %s*" b)))
	   (setq buffer-read-only t)
	   (or nc-show-shell (delete-other-windows))
	   (split-window-horizontally)
	   (other-window 1)
	   (switch-to-buffer (format "*NC %s*" b))
	   (setq nc-panes (cons (current-buffer) nc-panes))
	   (nc-setup-current-buffer b1)
	   (set-nc-buffers (list (car (nc-buffers)) (current-buffer)))
	   (other-window -1)))))

(defun nc-setup-cursor ()
  (if (and (boundp 'nc-cursor-col)
	   (boundp 'nc-cursor-lin)
	   (boundp 'nc-first-column))
      nil
    (setq nc-cursor-col 0
	  nc-cursor-lin 0
	  nc-first-column 0))
  (nc-show-cursor nc-cursor-col nc-cursor-lin)
  (nc-write-modeline))

(defun nc-delete-cursor (col lin)
  ;;(let ((pos (nc-absolute-from-relative-pos col lin)))
    ;;(setq buffer-read-only nil)
    (if window-system (delete-overlay nc-cursor-overlay))
    ;;(put-text-property pos (+ pos nc-cl) 'face 'default)
    ;;;;(if (memq (nc-num-from-col-lin col lin) nc-selected-files)
    ;;;;	(nc-show-select col lin))
    ;;(setq buffer-read-only t)
    ;;)
    )

(defun nc-show-cursor (col lin)
  (let ((pos (nc-absolute-from-relative-pos col lin)))
    ;;(setq buffer-read-only nil)
    (goto-char pos)
    (if window-system
	(progn
	  (move-overlay nc-cursor-overlay 
			(+ pos 1) (+ pos nc-cl) (current-buffer))
	  (overlay-put nc-cursor-overlay 
		       'face nc-highlight-face)))
    ;;(put-text-property (+ pos 1) (+ pos nc-cl) 'face 'nc-highlight-face)
    ;;;;(if (memq (nc-num-from-col-lin col lin) nc-selected-files)
    ;;;;	(nc-show-select col lin))
    ;;(setq buffer-read-only t)
    ))

(defun nc-show-select (col lin)
  (let ((pos (nc-absolute-from-relative-pos col lin)))
    (setq buffer-read-only nil)
    (put-text-property (+ pos (if (= 0 nc-suffix-length)
				  0
				(- nc-cl nc-suffix-length 1)))
		       (+ pos (if (= 0 nc-suffix-length)
				  1
				(- nc-cl nc-suffix-length)))
		       'face nc-select-face)
    (setq buffer-read-only t)))

(defun nc-delete-select (col lin)
  (cond ((< col nc-first-column) nil)
	((> col (+ nc-first-column nc-columns -1)) nil)
	(t
	 (let ((pos (nc-absolute-from-relative-pos col lin)))
	   (setq buffer-read-only nil)
	   (put-text-property (+ pos (if (= 0 nc-suffix-length)
					 0
				       (- nc-cl nc-suffix-length 1)))
			      (+ pos (if (= 0 nc-suffix-length)
					 1
				       (- nc-cl nc-suffix-length)))
			      'face 'default)
	   (setq buffer-read-only t)))))

(defun nc-toggle-write-title ()
  (interactive)
  (setq nc-write-header (not nc-write-header))
  (nc-redraw))

(defun nc-toggle-write-totals ()
  (interactive)
  (setq nc-write-totals (not nc-write-totals))
  (nc-redraw))

(defun nc-write-title (title &optional toponly)
  "write TITLE line in File Browser."
  (let* ((title-length (length title))
	 (half-rand (/ (- nc-wd title-length 4) 2))
	 half-title-length
	 (rand (if (> half-rand 0)
		   (make-string half-rand nc-horiz-line-char)
		 (setq half-title-length (/ (- nc-wd 6) 2))
		 nil))
	 (count nc-nl)
	 (top-string-n "Name:")
	 (top-string-a (concat (if (memq 'attr nc-show-list) 
				   (if nc-dosish "Att: " "Attribute: ")
				 "")
			     top-string-n))
	 (top-string-t (concat (if (memq 'time nc-show-list) "Time: " "")
			     top-string-a))
	 (top-string-d (concat (if (memq 'date nc-show-list) "Date:   " "")
			     top-string-t))
	 (top-string (concat (if (memq 'size nc-show-list) "Size:     " "")
			     top-string-d))
	 (title-line (if rand
			 (format "%s %s %s" rand title
				 (make-string (- nc-wd 
						 3 title-length half-rand)
					      nc-horiz-line-char))
		       (format "%c%s...%s%c" nc-horiz-line-char
			       (substring title 0
					  half-title-length)
			       (substring title
					  (- title-length
					     (- nc-wd half-title-length 6)))
			       nc-horiz-line-char)
;;;		       (format " %s... "
;;;			       (substring title 0
;;;					  (- (* 3 (+ 1 nc-cl)) 7)))
		       ))
	 name
	 (cnt nc-columns))
    (insert title-line)
    (cond ((null toponly)
	   (newline)
	   (cond (nc-write-header
		  (put-text-property 0 (length top-string)
				     'face nc-bold-face top-string)
		  (while (> cnt 0)
		    (setq name (if (> nc-cl (length top-string)) 
				   (concat " " top-string 
					   (make-string
					    (- nc-cl (length top-string) 1) 32))
				 (substring (concat top-string " ") 0 nc-cl)))
		    (insert name)
		    (insert (if (eq cnt 1) "" nc-verti-line-char))
		    (setq cnt (1- cnt)))
		  (newline)))
	   (nc-intern-frame nil)
;;;	   (while (> count 0)
;;;	     (setq cnt nc-columns)
;;;	     (while (> cnt 0)
;;;	       (insert (make-string nc-cl 32))
;;;	       (insert (if (eq cnt 1) "" nc-verti-line-char))
;;;	       (setq cnt (1- cnt)))
;;;	     (newline)
;;;	     (setq count (- count 1)))
	   (cond (nc-write-totals
		  (insert (make-string (- nc-wd 2)
				       nc-horiz-line-char))
		  (newline)))))
    (cond ((> half-rand 0)
	   (setq nc-title-start (+ half-rand 2)
		 nc-title-end (+ half-rand title-length 2)))
	  (t
	   (setq nc-title-start 1
		 nc-title-end (- nc-wd 2))))
    ;;(put-text-property nc-title-start nc-title-end 'face 'nc-highlight-face)
    ))
				     
(defun nc-intern-frame (erase)
  "Fill the frame.
If ERASE, remove what was there before."
  (let ((count nc-nl) cnt)
    (goto-char (1+ (* (if nc-write-header 2 1) nc-wd)))
    (if erase (delete-char (* count nc-wd)))
    (while (> count 0)
	     (setq cnt nc-columns)
	     (while (> cnt 0)
	       (insert (make-string nc-cl 32))
	       (insert (if (eq cnt 1) "" nc-verti-line-char))
	       (setq cnt (1- cnt)))
	     (newline)
	     (setq count (- count 1)))))
				     
(defun nc-find-files-for-buffer (&optional re-read)
  "get the list of files to display in BUFFER.
If RE-READ or no files in cache, read directory"
  (let ((dirname (car (car nc-files)))
	(liste nc-files))
    (if (and (cdr liste)
	     (null re-read))
	(cdr liste)
      (if (file-directory-p dirname) 
	  (progn
	    (setq nc-dotdot 0)
	    (rplacd liste
		    (nc-dired-to-list dirname))
	    (setq nc-num-files (- (length (cdr liste)) nc-dotdot)
		  nc-total-size-files (nc-total-size (cdr liste))
		  nc-total-size-selected nil))
	(error 
	 "Not a directory: `%s'. Use F2 to change directory." 
	 dirname)))
    liste))

(defun nc-format-files-for-buffer (file-list)
  "Format files in FILE-LIST in appropriate
columns (lists) and lines (the entrys in list)"
  (if (not (listp (car file-list)))
      (setq file-list (cdr file-list)))
  (let ((erg nil)
	(ergaux nil)
	(count 0)
	(mylist file-list))
    (while mylist
      (setq ergaux nil
	    count 0)
      (while (and mylist
		  (< count nc-nl)
		  (or (> (length erg) 0)
		      (< count (- nc-nl nc-format-align-right))))
	(setq ergaux (cons (car (car mylist))
			   ergaux))
	(setq mylist (cdr mylist)
	      count (+ count 1)))
      (if ergaux
	  (setq erg (cons (reverse ergaux)
			  erg))))
    (reverse erg)))

;;(defun nc-display-column (cols)
;;  "display the contents of columns n"
;;  (let* ((column 0)
;;	 zeile spalte)
;;    (setq buffer-read-only nil)
;;    (goto-char 0)
;;    (forward-line)
;;    (delete-region 0 (point))
;;    (nc-write-title (car (car nc-files)) t)
;;    (while (< column nc-columns)
;;      (cond ((memq (+ column nc-first-column)
;;		   cols)
;;	     (setq spalte (elt nc-format
;;			       (+ column nc-first-column)))
;;	     (if (< (length spalte) nc-nl)
;;		 (setq spalte (append (copy-sequence spalte)
;;				      (make-list
;;				       (- nc-nl (length spalte))
;;				       (make-string nc-cl 32)))))
;;	     (goto-line 3)
;;	     (setq zeile 0)
;;	     (while spalte
;;	       (beginning-of-line)
;;	       (forward-char (+ (* column
;;				   (+ nc-cl 1))))
;;	       (insert (car spalte))
;;	       (delete-char  nc-cl)
;;	       (next-line 1)
;;	       (setq zeile (+ 1 zeile)
;;		     spalte (cdr spalte))
;;	       )))
;;      (setq column (1+ column)))
;;    (goto-char 0)
;;    (setq buffer-read-only t)
;;    ))

(defun nc-show-selected-files ()
  (let ((start (- (* nc-nl nc-first-column) nc-format-align-right))
	(end (- (* nc-nl (+ nc-first-column nc-columns)) nc-format-align-right))
	(liste nc-selected-files))
    (setq buffer-read-only nil)
    (while liste
      (cond ((and (memq (car liste)
			nc-selected-files)
		  (>= (car liste) start)
		  (< (car liste) end))
	     (nc-show-select
	      (nc-col-from-num (car liste))
	      (nc-lin-from-num (car liste)))))
      (setq liste (cdr liste)))
    ;;(goto-char 0)
    (setq buffer-read-only t)))

(defun nc-display-buffer (&optional re-read no-frame)
  "Display the contents of directory.
If RE-READ, will recalculate the file list.
If NO-FRAME, will not redisplay the frame."
  (let* ((files (nc-find-files-for-buffer  re-read))
	 (format (nc-format-files-for-buffer (cdr files)))
	 (column 0)
	 spalte str l)
    (setq nc-format format) 
    (setq buffer-read-only nil)
    (if no-frame 
	(nc-intern-frame t)
      (delete-region (point-min) (point-max))
      (nc-write-title (car (car files))))
    (while (< column nc-columns)
      (setq spalte (elt format
			(+ column nc-first-column)))
      (goto-line (if nc-write-header 3 2))
      (while spalte
	(beginning-of-line)
	(forward-char (+ (* column
			    (+ nc-cl 1))))
	(setq str (car spalte)
	      l (length str))
	(if (<= l nc-cl)
	    (progn
	      (insert str)
	      (delete-char l))
	  (insert (substring str 0 (1- nc-cl)))
	  (insert nc-name-too-long-string)
	  (delete-char  nc-cl))
	;;(insert (car spalte))
	;;(delete-char  nc-cl) 
	(next-line 1)
	(setq spalte (cdr spalte)))
      (setq column (1+ column)))
    (nc-show-selected-files)
    (nc-write-totals-line)
    (setq buffer-read-only t)))

(defun nc-display-help-short ()
  (interactive)
  (message "f2: dir f3: view f4: edit f5: copy f6: move f7: mkdir f8: del f10: quit"))

;;; Against old Emaxen

(or (fboundp 'format-time-string)
    (defun format-time-string (&rest) "Fake plug." "Time: ???"))

(defun nc-write-modeline ()
  (let* ((abspos (nc-num-from-col-lin nc-cursor-col nc-cursor-lin))
	 (file (elt (cdr nc-files)
		    abspos))
	 sz mid)
    (cond (t;;(or (null nc-selected-files)
	   ;; (not (eql system-type 'ms-dos)))
	   (setq sz (format "%s"
			    (cond  ((elt file 7)
				    (format "-> %s" (elt file 7)))
				   ((and (elt file 1) (not (elt file 2)))
				    "<DIR>")
				   (t (nc-number-to-string (or ; Empty root!
							    (elt file 2) 0)))))
		 mid (max 1 (- 17 (length (elt file 5)) (length sz))))
	   (setq nc-modeline
		 (concat (elt file 5) (make-string mid ?\ ) sz " "
			 (format-time-string
			  nc-date-time-format
			  (elt file 3)
			  )
			 (format " %s"	; Skip 10 chars
				 (if nc-dosish  
				     (substring (or ; Empty root!
						 (elt file 4) "    ")
						0 4)
				   (elt file 4)))))))))

(defun nc-write-totals-line ()
  (cond 
   (nc-write-totals
    (setq buffer-read-only nil)
    (save-excursion
      (let* ((abspos (nc-num-from-col-lin nc-cursor-col nc-cursor-lin))
	     (file (elt (cdr nc-files)
			abspos))
	     (message (make-string (1- nc-wd) nc-horiz-line-char))
	     mid sz mes mes2 lm lm2)
	(goto-char nc-pane-end)
	(delete-region (point)
		       (point-max))
	(if nc-selected-files
	    (progn
	      (if nc-total-size-selected nil
		(setq nc-total-size-selected 0)
		(mapcar '(lambda (x)
			   (setq nc-total-size-selected
				 (+ nc-total-size-selected
				    (or (elt (elt  nc-files (+ 1 x)) 2) 0))))
			nc-selected-files))
	      (setq mes (format "Selected: %s bytes in %s file%s"
				(nc-number-to-string nc-total-size-selected)
				(length nc-selected-files)
				(if (eq (length nc-selected-files) 1) "" "s"))
		    lm 8)
	      (if (< (length mes) nc-wd) nil
		(setq mes (format "S: %s/%s"
				  (nc-number-to-string nc-total-size-selected)
				  (length nc-selected-files)))
		(if (< (length mes) nc-wd) nil
		  (setq mes (format "S:%s" (length nc-selected-files))))
		(setq lm 1))
	      (setq message 
		    (concat (make-string 
			     (- nc-wd 1 (length mes)) nc-horiz-line-char)
			    mes)))
	  (setq mes ""))
	(if (and lm window-system) 
	    (put-text-property 0 lm 'face nc-bold-face mes))
	(setq mes2 (concat "Total: " (nc-number-to-string nc-total-size-files) 
			   " bytes in " 
			   (number-to-string nc-num-files) " file"
			   (if (eq nc-num-files 1) "" "s"))
	      lm2 5)
	(cond ((< (length mes2) (- nc-wd (length mes) 1)) nil)
	      ((< (length 
		   (setq mes2 
			 (concat "Total: " 
				 (nc-number-to-string nc-total-size-files)
				 "/" (number-to-string nc-num-files))))
		  (- nc-wd (length mes) 1)) nil)
	      ((< (length 
		   (setq lm2 1
			 mes2 
			 (concat "T: " (nc-number-to-string nc-total-size-files)
				 "/" (number-to-string nc-num-files))))
		  (- nc-wd (length mes) 1)) nil)
	      ((< (length 
		   (setq lm2 1
			 mes2 
			 (concat "T: " (number-to-string nc-num-files))))
		  (- nc-wd (length mes) 1)) nil)
	      (t (setq mes2 "" lm2 nil)))
	(if (and lm2 window-system) 
	    (put-text-property 0 lm2 'face nc-bold-face mes2))
	(setq message 
	      (concat 
	       mes2
	       (make-string 
		(- nc-wd 1 (length mes) (length mes2)) nc-horiz-line-char)
	       mes))
	(insert message)))
    (setq buffer-read-only t)
					;(goto-char 0)
    (nc-display-help-short))))

(defun nc-dir-size (dir)
  (save-excursion
    (setq dir (expand-file-name dir))
    (set-buffer (get-buffer-create nc-tmp-buf))
    (buffer-disable-undo)
    (auto-fill-mode 0)
    (erase-buffer)
    (let ((rc (call-process "du" nil t nil "-sb" dir)))
      (if (not (equal 0 rc))
	  (error "Could not run `du -sb %s', status %s" dir rc)
	(goto-char 1)
	(re-search-forward "\\`[0-9]+")
	(if (not (match-beginning 0))
	    (error "Cannot parse output from `du'")
	  (if (<= (- (match-beginning 0) (match-end 0)) -7)
	      (+ (* 100000.0 
		    (string-to-number
		     (buffer-substring (match-beginning 0) (- (match-end 0) 5))) )
		 (string-to-number
		  (buffer-substring (- (match-end 0) 5) (match-end 0))))
	    (string-to-number
	     (buffer-substring (match-beginning 0) (match-end 0)))))))))

(provide 'nc)
;;; nc.el ends here
