;;; fix-buffers-list.el --- New M status and colors in buffers list
;;--------------------------------------------------------------------
;;
;; Copyright (C) 2004, David Andersson <l.david.andersson(at)sverige.nu>
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------
;;
;; Author: David Andersson <l.david.andersson(at)sverige.nu>
;; Created: 26 Nov 2004
;; Version: 1.5
;;
;;; Commentary:
;;
;; This module changes the look of Buffers list and Buffer menu.
;;
;; *** Modification status ***
;;
;; Column M is extended to distinguish a buffer that has been modified and
;; saved from a buffer that has never been modified.
;; The new meaning of column M is:
;;   ` '  untouched:  buffer has never been modified
;;   `.'  saved:      buffer has been modified and saved (previously ` ')
;;   `*'  modified:   buffer has modifications that are not saved
;;
;; This is somewhat useful in deciding which buffers to drop or keep.
;; It uses `buffer-undo-list' to detected if there has been modifications.
;;
;; *** Compacting ***
;;
;; If flag `list-buffers-compact' is non-nil the text in the Size and Mode
;; columns are truncated. The Size indicates kilo-chars (k) rather than chars
;; (but only if the combined length of buffer name and size would be to big)
;; and the Mode string is truncated to 12 chars.
;;
;; This somewhat reduces the risk that the File columns is pushed to the right
;; by a long buffer name. (But it can still happen if the buffer name and the
;; mode name is quite long.)
;;
;; *** Colors ***
;;
;; The M, R, Size and Mode columns are shown in `list-buffers-status-face'.
;; This makes it somewhat easier to read the columns, even if they are
;; skewed by very long buffer names.
;;
;; In the M column, `*' is shown in `list-buffers-modified-face' if the
;; buffer is visiting a file.
;; This makes it somewhat easier to find buffers that needs to be saved,
;; as opposed to eg *scratch* and *Messages* that need not.
;;
;; (Font-lock-mode is not used. If font-lock-mode is enabled in "*Buffer List*"
;; it will override this colorization.)
;;
;; *** Character syntax ***
;;
;; The syntax table is changed so that `.' (and other chars that usually
;; occurs in file names) has symbol syntax. Thus it is somewhat easier to
;; copy and paste filenames (and buffer names) by double-clicking on them.
;;
;; *** Buffer menu ***
;;
;; In the Buffers menu the modification column is changed to distinguish a
;; buffer that has been modified and saved from a buffer that has never been
;; modified. It is the same change as for column M in Buffers list:
;;   ` '  untouched:  buffer has never been modified
;;   `.'  saved:      buffer has been modified and saved (previously ` ')
;;   `*'  modified:   buffer has modifications that has not been saved
;;
;; *** Usage examples ***
;;
;;   (require 'fix-buffers-list)
;;   (setq list-buffers-compact t)
;;   (setq list-buffers-modified-face 'bold)
;;
;;   ; or
;;
;;   (require 'fix-buffers-list)
;;   (require 'font-lock)  ; define more faces (colours)
;;   (setq list-buffers-compact t)
;;   (setq list-buffers-status-face   'font-lock-string-face)
;;   (setq list-buffers-modified-face 'font-lock-warning-face)
;;
;;   ; or
;;
;;   (require 'fix-buffers-list)
;;   (require 'font-lock)  ; define more faces (colours)
;;   (setq list-buffers-compact t)
;;   (setq list-buffers-status-face   'font-lock-builtin-face)
;;   (setq list-buffers-modified-face  nil)
;;
;;   ; etc
;;
;; *** Compatibility ***
;;
;; Works with Emacs-19, Emacs-20 and Emacs-21.2.
;; Does not work with Emacs-22 and XEmacs.
;; 
;; This module redefines many existing standard functions that builds the
;; buffer list and the buffer menu. It does not affect `mouse-select-buffer'
;; (msb.el).
;;
;; History:
;;  1.5 david 2010-04-27 Update email
;;  1.4 david 2010-04-18 Incompatibility comment about Emacs-22
;;  1.3 david 2006-05-16 Colour special buffer names
;;  1.2 david 2006-02-21 Compact mode name with list-buffers-mode-alist
;;  1.1 david 2006-02-02 Rename list-buffers-status-face. Compact home dir to ~
;;  1.0 david 2004-11-26 First version
;;
;;; Code:

(defvar list-buffers-compact nil
  "If non-nil: use compact notation for columns Size in *Buffer List*.
If the buffer name and the size together would be longer that 22 chars and
the buffer size is at least 5 digits, then compact the buffer size by dividing
it by 1000 and adding ISO suffix `k', thereby saving 2 chars.
\nIn file names, the users home directory is shortened to `~'.")

(defvar list-buffers-status-face nil
  "Face to highlight M, R, Size and Mode columns in *Buffer List*, or nil.
If nil, the default face is used (no highlighting).
Also see `list-buffers-modified-face'.")

(defvar list-buffers-modified-face nil
  "Face to highlight M column for modified buffers in *Buffer List*, or nil.
If nil, the default face is used (no highlighting).
The face is used only for modified buffers which are visiting a file,
and thus need to be saved.
Also see `list-buffers-separator-face'.")

(defvar list-buffers-separator-face nil
  "Face to highlight directory separators in *Buffer List*, or nil.
Also see `list-buffers-status-face'.")

(defvar list-buffers-special-face nil
  "Face to highlight buffers not attached to a file in *Buffer List*, or nil.
Also see `list-buffers-status-face'.")

(defvar list-buffers-mode-alist nil
  "Assoc list for mode names to be displayed in *Buffers list*")

(add-to-list 'list-buffers-mode-alist '("Dired by name" . "Dired"))
(add-to-list 'list-buffers-mode-alist '("Dired by date" . "Dired"))

;; There was no Buffer-menu-mode-syntax-table. Default syntax was used.
;; Make one so `.' can be changed.
(defvar buffer-menu-mode-syntax-table nil
  "Syntax table for Buffer-menu-mode (*Buffer List*).")
(if (not buffer-menu-mode-syntax-table)
    (progn
      (setq buffer-menu-mode-syntax-table (make-syntax-table))
      ;; Set symbol syntax for chars that usually occur in file names.
      (modify-syntax-entry ?.   "_" buffer-menu-mode-syntax-table)
      (modify-syntax-entry ?/   "_" buffer-menu-mode-syntax-table)
      (modify-syntax-entry ?-   "_" buffer-menu-mode-syntax-table)
      (modify-syntax-entry ?_   "_" buffer-menu-mode-syntax-table)
      (modify-syntax-entry ?\#  "_" buffer-menu-mode-syntax-table)
      (modify-syntax-entry ?~   "_" buffer-menu-mode-syntax-table)
      ;; Set non-symbol syntax for chars that occurs as buffer name suffixes
      ;; (eg, <2>), so I can search for a similar buffer names using C-s C-w.
      (modify-syntax-entry ?<   "." buffer-menu-mode-syntax-table)
      (modify-syntax-entry ?>   "." buffer-menu-mode-syntax-table)
      ))
(add-hook 'buffer-menu-mode-hook (lambda () (set-syntax-table buffer-menu-mode-syntax-table)))

;; This functions is copied from menu-bar.el in emacs-20.7 and modified.
;; (This works in Emacs-19, Emacs-20 and Emacs-21.2, but has no effect in XEmacs-20.4.)
;; Comment "David's fix (.)" for edit-indication.
(defun menu-bar-update-buffers-1 (elt)
  (cons (format
	 (format "%%%ds  %%s%%s  %%s" menu-bar-update-buffers-maxbuf)
	 (cdr elt)
	 (if (buffer-modified-p (car elt))
	     "*"
	   (if (save-excursion (set-buffer (car elt))	; David's fix (.)
			       (consp buffer-undo-list)); David's fix (.)
	       "." 					; David's fix (.)
	     " "))					; David's fix (.)
	 (save-excursion
	   (set-buffer (car elt))
	   (if buffer-read-only "%" " "))
	 (let ((file
		(or (buffer-file-name (car elt))
		    (save-excursion
		      (set-buffer (car elt))
		      list-buffers-directory)
		    "")))
	   (setq file (or (file-name-directory file)
			  ""))
	   (if (> (length file) 20)
	       (setq file (concat "..." (substring file -17))))
	   file))
	(car elt)))

(defun list-buffers-get-status-string ()
  "Return a 2 chars for current buffers modification and read-only status.
First char is `*' for modified, `.' for modified and saved, ` ' for unmodified.
Second char is `%' for read-only, ` ' otherwise.
The string has text-properties set: `list-buffers-status-face' for the visible
chars, except that the `*' has `list-buffers-modified-face' if the buffer is
visiting a file."
  ;; If you try to omptimize this function, make sure that it does not
  ;; modifie chars or text-properties of previously returned strings.
  (concat
   (if (buffer-modified-p)
       (let ((m "*"))
	 (put-text-property 0 1 'face (if (buffer-file-name) list-buffers-modified-face list-buffers-status-face) m)
	 m)
     ;; else
     (if (consp buffer-undo-list)
	 (let ((m "."))
	   (put-text-property 0 1 'face list-buffers-status-face m)
	   m)
       ;; else
       " "))
   (if (or buffer-read-only
	   (eq standard-output (current-buffer)))
       (let ((r "%"))
	 (put-text-property 0 1 'face list-buffers-status-face r)
	 r)
     ;; else
     " ")))

;; This is mangle (smooth, press), not mangle (injure, crush)
(defun list-buffers-mangle-mode (mode)
  "Make a shorter mode string, if possible.
Uses `list-buffers-mode-alist' and truncate string to 12 chars."
  (setq mode (or (cdr (assoc mode list-buffers-mode-alist)) mode))
  (if (> (length mode) 12) (setq mode (substring mode 0 12)))
  mode)

;; I dare not use `abbreviate-file-name' because it seems to be intended
;; for other use and may change the name more than reasonable here.
(defun list-buffers-abbrev-file (filename)
  "Return FILENAME where the users home dir name has been replaced with `~'."
  (let ((abbrev-home-dir (concat "^" (expand-file-name "~") "\\>")))
    (if (and (string-match abbrev-home-dir filename)
	     ;; If the home dir is just /, don't change it.
	     (> (match-end 0) 1))
	(setq filename (concat "~" (substring filename (match-end 0)))))
    (if (and (fboundp 'split-string) list-buffers-separator-face)
	(let ((sep "/")
	      (newsep "/"))
	  (put-text-property 0 1 'face list-buffers-separator-face newsep)
	  (setq filename (mapconcat 'identity (split-string filename sep) newsep))))
    filename))

;; This function is copied from buff-menu.el in emacs-20.7 and modified
;; (This works in Emacs-19, Emacs-20 and Emacs-21.2, but has no effect in XEmacs-20.)
;; (Well, the colours won't appear in Emacs-19.)
;; Comment "David's fix (.)" for edit-indication.
;; Comment "David's fix (h)" for added colorization.
;; Comment "David's fix (c)" for compaction.
(defun list-buffers-noselect (&optional files-only)
  "Create and return a buffer with a list of names of existing buffers.
The buffer is named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

The M column contains a * for buffers that are modified.
The M column contains a . for buffers that has undo information.
The R column contains a % for buffers that are read-only." ; David's fixes (.)
  (let ((old-buffer (current-buffer))
	(standard-output standard-output)
	desired-point)
    (save-excursion
      (set-buffer (get-buffer-create "*Buffer List*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq standard-output (current-buffer))
      (princ "\
 MR Buffer           Size  Mode         File
 -- ------           ----  ----         ----
")
      ;; Record the column where buffer names start.
      (setq Buffer-menu-buffer-column 4)
      (let ((bl (buffer-list)))
	(while bl
	  (let* ((buffer (car bl))
		 (name (buffer-name buffer))
		 (file (buffer-file-name buffer))
		 this-buffer-line-start
		 this-buffer-read-only
		 this-buffer-size
		 this-buffer-mode-name
		 this-buffer-directory
		 this-buffer-status-string)		; David's fix (.)
	    (save-excursion
	      (set-buffer buffer)
	      (setq this-buffer-read-only buffer-read-only)
	      (setq this-buffer-size (buffer-size))
	      (setq this-buffer-mode-name
		    (if (eq buffer standard-output)
			"Buffer Menu" mode-name))
	      (or file
		  ;; No visited file.  Check local value of
		  ;; list-buffers-directory.
		  (if (and (boundp 'list-buffers-directory)
			   list-buffers-directory)
		      (setq this-buffer-directory list-buffers-directory)))
	      (setq this-buffer-status-string		; David's fix (.)
		    (list-buffers-get-status-string)))	; David's fix (.)
	    (cond
	     ;; Don't mention internal buffers.
	     ((string= (substring name 0 1) " "))
	     ;; Maybe don't mention buffers without files.
	     ((and files-only (not file)))
	     ;; Otherwise output info.
	     (t
	      (setq this-buffer-line-start (point))
	      ;; Identify current buffer.
	      (if (eq buffer old-buffer)
		  (progn
		    (setq desired-point (point))
		    (princ "."))
		(princ " "))
	      ;; Identify modified buffers.
	      (insert this-buffer-status-string)	; David's fix (.)
	      (princ " ")				; David's fix (.)
	      ;; Handle readonly status.  The output buffer is special
	      ;; cased to appear readonly; it is actually made so at a later
	      ;; date.
	      ;;(princ (if (or (eq buffer standard-output) ; David's fix (h)
	      ;;               this-buffer-read-only)	; David's fix (h)
	      ;;           "% "				; David's fix (h)
	      ;;         "  "))				; David's fix (h)
	      (let ((name-pos (point)))			; David's fix (h) 
		(princ name)
		(if (or (eq major-mode 'dired-mode)
			(null (buffer-file-name buffer)))
		    (put-text-property name-pos (point)	; David's fix (h)
			 'face list-buffers-special-face))) ; David's fix (h)
	      ;; Put the buffer name into a text property
	      ;; so we don't have to extract it from the text.
	      ;; This way we avoid problems with unusual buffer names.
	      (setq this-buffer-line-start
		    (+ this-buffer-line-start Buffer-menu-buffer-column))
	      (let ((name-end (point)))
		(indent-to 17 1)			; David's fix (h)
		(put-text-property this-buffer-line-start name-end
				   'buffer-name name)
		(put-text-property this-buffer-line-start name-end
				   'mouse-face 'highlight))
	      (let (size
		    mode
		    (size-start (point))		; David's fix (h)
		    (excess (- (current-column) 17)))
		(setq size (if (and list-buffers-compact (> excess 3) (> this-buffer-size 9999)) ; David's fix (c)
			       (format "%7dk" (/ this-buffer-size 1000)) ; David's fix (c)
			     (format "%8d" this-buffer-size))) ; David's fix (c)
		;; Ack -- if looking at the *Buffer List* buffer,
		;; always use "Buffer Menu" mode.  Otherwise the
		;; first time the buffer is created, the mode will be wrong.
		(setq mode this-buffer-mode-name)
		(while (and (> excess 0) (= (aref size 0) ?\ ))
		  (setq size (substring size 1))
		  (setq excess (1- excess)))
		(princ size)
		(indent-to 27 1)
		(setq mode (list-buffers-mangle-mode mode)) ; David's fix (c)
		(princ mode)
		(put-text-property size-start (point)	; David's fix (h)
				   'face list-buffers-status-face)) ; David's fix (h)
	      (indent-to 40 1)
	      (or file (setq file this-buffer-directory))
	      (if file
		  (insert (if list-buffers-compact 		    ; David's fix (h)
			     (list-buffers-abbrev-file file) file))); David's fix (h)
	      (princ "\n"))))
	  (setq bl (cdr bl))))
      (Buffer-menu-mode)
      ;; DESIRED-POINT doesn't have to be set; it is not when the
      ;; current buffer is not displayed for some reason.
      (and desired-point
	   (goto-char desired-point))
      (current-buffer))))

;; Copied from /opt/sfw/share/emacs/20.7/lisp/buff-menu.el
;; Comment "David's fix (.)" for David's fixes.
(defun Buffer-menu-unmark (&optional backup)
  "Cancel all requested operations on buffer on this line and move down.
Optional ARG means move up."
  (interactive "P")
  (beginning-of-line)
  (if (looking-at " [-M]")
      (ding)
    (let* ((buf (Buffer-menu-buffer t))
	   (flags (save-excursion (set-buffer buf) (list-buffers-get-status-string))) ; David's fix (.)
	   (buffer-read-only nil))
      (delete-char 3)
      (insert " " flags)))				; David's fix (.)
  (forward-line (if backup -1 1)))

;; This function is copied from buff-menu.el in emacs-20.7 and modified.
;; It didn't know what to do with a `.' status in the M column.
;; Comment "David's fix (.)" for David's fixes.
;; Comment "David's fix (d)" for doc string that was missing before.
(defun Buffer-menu-not-modified (&optional arg)
  "Mark buffer on this line as unmodified (no changes to save).
With ARG, mark as modified instead."			; David's fix (d)
  (interactive "P")
  (let (flags)						; David's fix (.)
    (save-excursion
      (set-buffer (Buffer-menu-buffer t))
      (set-buffer-modified-p arg)
      (setq flags (list-buffers-get-status-string)))	; David's fix (.)
    (save-excursion
      (beginning-of-line)
      (forward-char 1)
      (if (/= (char-after (point)) (aref flags 0))	; David's fix (.)
	  (let ((buffer-read-only nil))			; David's fix (.)
	    (delete-char 2)				; David's fix (.)
	    (insert flags))))))				; David's fix (.)

;;To do: `mouse-buffer-menu' also list buffers with `%' and `*', fix that too?
;;     (Then re-implement function `mouse-buffer-menu-alist')
;;To do: Update doc string for `list-buffers'.

(provide 'fix-buffers-list)

;;; fix-buffers-list.el ends here
