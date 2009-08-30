;;; dired-column-widths.el --- Corrects column widths after entry modification.
;;
;; Author:        Craig Lawson <craig.lawson@alum.mit.edu>
;; Maintainer:    Craig Lawson <craig.lawson@alum.mit.edu>
;; Compatibility: GNU Emacs 21.4.x + GNU ls 6.4.
;;                GNU Emacs 22.1.50.1 (Aquamacs) + BSD ls (Mac OS 10.5.1)
;;                 Don't know about the others.
;; URL:           http://www.emacswiki.org/emacs/dired-column-widths.el
;; Keywords:      dired
;; Version:       1.0.1
;;
;; This file is not part of GNU Emacs, though does extend it.


;;; Commentary:
;;
;; Emacs' dired mode displays directory contents in columns, and column
;; widths are determined by "ls -l". When a subset of the entries are
;; modified (e.g. name or permissions changed), only those entries are
;; redisplayed, and "ls -l" uses different column widths than for the entire
;; directory. The modified content doesn't line up and looks bad. This
;; module fixes the problem with dired hooks.
;;
;; This module works by capturing the column widths when the directory
;; listing is first produced, and applying those column widths whenever
;; content is modified.
;;
;; ls's "-R" option presents a special problem: each subdirectory is listed
;; separately with its own column widths. When "-R" is used, this module
;; reformats the listing so all sections have the same column width. That
;; takes longer, and if you're using "-R" it already takes longer. Sorry. I
;; don't use "-R", so it's unlikely that I'll improve on this.
;;
;; Limitations:
;;
;;   * Sometimes a field's width grows beyond the column's width. For
;;     example, an entry's size or inode may grow one digit larger than all
;;     the others, and this is not discovered until that one entry is
;;     redrawn. Result: while not illegible, it doesn't quite fit correctly.
;;     Work-around: revert dired buffer.
;;
;;   * Unanticipated ls options may insert unanticipated columns before the
;;     date column, and confuse this module. If that happens, modify the
;;     "just" list in dired-capture-column-widths-subdir and add another
;;     option test.


;;; Installation:
;;
;; Add to .emacs:
;;
;;   (add-hook 'dired-load-hook
;;             (lambda ()
;;               (load "dired-column-widths.el") ))
;;
;; You may already load other dired extensions. Brief and incomplete testing
;; indicates that this module does not interact with the others, and has no
;; load order requirements.
;;
;; Also suggested is to add this to the dired-load-hook lambda:
;;
;;                ;; Replace leading inode and size detection pattern to
;;                ;; match metric modifiers output by modern "ls -s".
;;               (setq dired-re-inode-size "\\(?:[0-9. \t]+[kKMGTPEZY]? \\)?")


;;; Configuration:
;;
;;  None. You either love it or hate it.


;;; Change log:
;;
;;  2007/12/21: craig.lawson
;;      v1.0.1  Accommodate extended attribute flag in permissions on Mac OS 10.5.
;;  2007/07/20: craig.lawson
;;      v1.0    Created.


;;; Debugging
;;
;;  Start with this:
;;    1. Load a dired buffer.
;;    2. Evaulate: (with-current-buffer DIRED-BUFFER-NAME dired-column-widths)
;;       Result: how the current dired buffer was parsed.
;;       See dired-capture-column-widths-subdir below for more information.


;;; GNU GPL 2+
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;; Code:

(require 'dired)
(require 'cl)


(defun dired-capture-column-widths ()
  ;; Determine field widths for each directory column.
  ;; Buffer may contain multiple directories ("-R" option).
  (save-excursion
    ;; Collect field widths for each directory listed in buffer.
    (goto-char (point-min))
    (let ((widths (dired-capture-column-widths-subdir)))
      (while (not (eobp))
	(let ((more-widths (dired-capture-column-widths-subdir)))
	  (if more-widths
	      ;; Merge widths from two sections into one using maximum of
	      ;; each field width.
	      (setq widths (map 'list
				(lambda (a b) (if (> (cdr a) (cdr b)) a b))
				widths more-widths)) )))
      (setq dired-column-widths widths) )))

(defun dired-capture-column-widths-subdir ()
  ;; Determine field widths for each directory entry column for one
  ;; directory.
  ;;
  ;; Some columns are right justified, and some are left justified. For each
  ;; field, store pairs ( JUSTIFICATION . WIDTH ), where JUSTIFICATION is
  ;; either 'left, 'right, 'skip, or 'space. 'skip means skip over characters.
  ;; 'space indicates some number of required spaces.
  ;;
  ;; Returns a list of such pairs for [inode], [disk size], perms, links,
  ;; owner, group, and file size (inode and disk size are optional). Ignore
  ;; date and file name: date is already fixed width, and it aligns file
  ;; name.
  ;;
  ;; Returns nil if directory contains no entries.
  ;;
  ;; Because "ls" uses both left and right justified columns, it's often not
  ;; possible to determine the column by looking at a single line. Example:
  ;;   -rwxr-xr-x   1  craig  craig     0 2007-05-16 17:43 file2
  ;;   -rwxr-xr-x  14  root   root   4.1k 2006-02-26 00:00 file1
  ;; (How wide are the group and file size columns?)
  ;; To figure it out, mash all entries together into one string so that
  ;; whitespace is always superseded. Example from above lines:
  ;;   -rwxr-xr-x  14  rootg  rootg  4.1k 2006-02-26 00:00 file1
  ;; Now the column separators used by "ls" are apparent.
  (let ((re-entry (dired-col-widths-re-entry-pattern)))

    ;; Search forward to next directory entry.
    (while (and (not (looking-at re-entry))
		(not (eobp)))
      (forward-line))

    ;; Initialize the mash-up. Skip over leading 2 characters (dired mark and space).
    (if (not (eobp))
	(let ((mash (buffer-substring-no-properties
		     (+ (line-beginning-position) 2)
		     (line-end-position))))
	  (forward-line)

	  ;; Merge each following entry into the mash-up.
	  ;; Stop at the end of this subdirectory's entries.
	  (while (and (looking-at re-entry)
		      (not (eobp)))
	    (let ((line (buffer-substring-no-properties
			 (+ (line-beginning-position) 2)
			 (line-end-position))))
	      ;; Merge line into mash, overwriting only when mash[i] is a
	      ;; space. Ignore differences in line length because we don't
	      ;; care about file names.
	      (dotimes (i (min (length mash) (length line)))
		(let ((c (elt mash i)))
		  (if (char-equal c ? )
		      (store-substring mash i (elt line i)) ))))

	    (forward-line))

	  ;; Compute field widths and assign right/left justification to each.
	  (let ((widths (mapcar 'length (dired-col-widths-split-string mash)))
		(has-inode (string-match "i" dired-actual-switches))
		(has-size  (string-match "s" dired-actual-switches))
		;; What's right/left justified. Don't care about date because
		;; it's always fixed width. Don't care about filename.
		;;           inode         size          perms        links         owner        group        size
		(just (list 'right 'space 'right 'space 'left 'space 'right 'space 'left 'space 'left 'space 'right)))
	    ;; Eliminate optional fields from list.
	    (if (not has-inode)
		(setq just (cddr just)))
	    (if (not has-size)
		(setq just (cddr just)))

	    ;; Merge widths and justification list.
	    ;; Prepend '(skip . 2) for standard 2 character indentation.
	    (append (list '(skip . 2)) (map 'list 'cons just widths)) )))))

(defun dired-col-widths-split-string (s)
  ;; Splits string into whitespace and non-whitespace components.
  ;; Returns list of components. If joined together, they would form the original string.
  (let ((components '())
	(current)
	(current-type nil))

    ;; Accumulate lists of characters in components list.
    ;; Each component list is either all spaces or all non-spaces.
    (dotimes (i (length s))
      (let ((c (elt s i)))
	(let ((c-type (if (char-equal c ? )
			  'space
			'word)))
	(if (eq c-type current-type)
	    ;; Character matches current type: append to current list.
	    (setq current (append current (list c)))
	  ;; Else: Append current list to components and start a new current list.
	  (progn
	    (if current-type
		(setq components (append components (list current))))
	    (setq current (list c))
	    (setq current-type c-type))) )))

    ;; Convert each component list into a string and return the list of strings.
    (map 'list 'concat components)))

(defun dired-apply-column-widths ()
  ;; Apply column widths in dired-column-widths to current buffer.
  ;; For each directory entry in buffer:
  ;;   Insure at least one space between fields.
  ;;   Adjust space between fields so field widths are satisfied.
  ;; Spaces are added but never removed from "ls" output.
  ;; Why: Individual fields in "ls" output for a subset of the entries can
  ;; never be wider than the width for all entries.
  ;; But: it's possible that an entry has changed enough that a field is
  ;; wider. In that case, too bad, and we'll just try to not make the entry
  ;; illegible.
  (beginning-of-buffer)
  (let ((re-entry (dired-col-widths-re-entry-pattern))
	buffer-read-only) ;; Override buffer local.
    (while (not (eobp))
      (if (looking-at re-entry)

	  ;; Process column widths on this line
	  (dolist (w dired-column-widths)
	    (let ((just  (car w))
		  (width (cdr w)))

	      ;; First process this column's width...
	      (cond ((eq 'skip just)
		     (forward-char width))

		    ((eq 'space just)
		     (dired-col-widths-insure-spaces width))

		    ((eq 'left just)
		     ;; Skip over non-spaces, then insure remainders are spaces
		     (dired-col-widths-insure-spaces
		      (- width
			 (dired-col-widths-skip-chars
			  (lambda (c) (not (char-equal c ? )))
			  width))))

		    ((eq 'right just)
		     (let ((begin (point)))
		       ;; Skip over spaces
		       (dired-col-widths-skip-chars
			(lambda (c) (char-equal c ? )))
		       ;; Skip over non-spaces
		       (dired-col-widths-skip-chars
			(lambda (c) (not (char-equal c ? ))))
		       ;; Point is end of field -- is the field
		       ;; the correct width? Ignore if wider than expected.
		       (let ((need (- width (- (point) begin))))
			 (if (> need 0)
			     ;; Too narrow. Insert spaces at front of field.
			     (progn (goto-char begin)
				    (insert-char ?  need)
				    (goto-char (+ begin width)) )))))

		    (t (error "Unexpected column justification %S" w))) )))
	(forward-line) )))

(defun dired-col-widths-re-entry-pattern ()
  ;; Constructs regex pattern which matches beginning of directory entry line.
  ;; Construct on demand because we don't need yet one more config variable.
  ;; When this pattern is used, lines have already been indented 2 spaces,
  ;; and character in column 1 may be a dired mark.
  (concat ". " dired-re-inode-size dired-re-perms))

(defun dired-col-widths-insure-spaces (count)
  ;; Insure at least count spaces follow point. Inserts as needed.
  ;; Moves point count characters forward.
  (dotimes (i count)
    (if (not (char-equal (following-char) ? ))
	(insert ? ) ; moves point forward
      (forward-char))))

(defun dired-col-widths-skip-chars (char-testp &optional count)
  ;; Move point forward one character at a time until test fails.
  ;; char-testp takes a single character parameter.
  ;; If count is provided, move forward at most that many times.
  ;; Returns number of characters moved forward.
  (if (not (integerp count))
      (setq count most-positive-fixnum))
  (let ((original-count count))
    (while (and (> count 0)
		(funcall char-testp (following-char)))
      (forward-char)
      (setq count (1- count)))
    (- original-count count)))





;; Before hook: resets saved column widths to nil.
(add-hook 'dired-before-readin-hook
	  (lambda ()
	    (if (not (local-variable-p 'dired-column-widths))
		(make-local-variable 'dired-column-widths))
	    (setq dired-column-widths nil) ))

;; After hook: if no column widths, capture column widths; otherwise
;; apply them to new content.
(add-hook 'dired-after-readin-hook
	  (lambda ()
	    (let ((first-time (not dired-column-widths)))
	      (if first-time
		  (dired-capture-column-widths))
	      (if (or (not first-time)
		      (string-match "R" dired-actual-switches))
		  ;; If using "-R", apply to entire buffer to make all
		  ;; columns in all listed directories conform to same
		  ;; column widths.
		  (dired-apply-column-widths)) )))

(provide 'dired-column-widths)
;;; dired-column-widths.el ends here
