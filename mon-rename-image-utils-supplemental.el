;;; this is mon-rename-images-supplemental.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-rename-images-supplemental provides functions and vars needed
;;; when using mon-rename-image-utils - mon-rename-image-utils.el
;;; (URL `http://www.emacswiki.org/emacs/mon-rename-image-utils.el')
;;; They are included here rather than requiring external packages.
;;; Functions herein have been pulled from: 
;;; mon-dir-utils.el - (URL `http://www.emacswiki.org/emacs/mon-dir-utils.el')
;;; mon-utils.el - (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;;; mon-dir-locals-alist.el - currently _unavailable_ on emacs-wiki
;;; naf-mode-replacements.el - currently _unavailable_ on emacs-wiki
;;;
;;; Where possible it is recommended to use those packages first. 
;;; Most likely they are more current.
;;;
;;; USE:
;;; Basically, you set up your vars for an image directory tree, then assuming you
;;; are in a file within that tree you can call:
;;;
;;; (mon-build-rename-buffer ".bmp") 
;;; (mon-build-rename-buffer ".jpg")
;;; (mon-build-rename-buffer ".nef")
;;;
;;; And it will either prompt for a better directory in the tree, or snarf the image
;;; file names from the current directory and return them in a pretty buffer full of
;;; text properties for futher processing.
;;;
;;; Lets say you are in the file:
;;; "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY/BMP-Scans/e1143/e1143.dbc"
;;; and you want to rename all of the ".jpg" files associated with the '.bmps" in
;;; the current directory e.g.  
;;;
;;; .bmp's are in => "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY/BMP-Scans/e1143/ 
;;; .jpg's are in => "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY/BIG-cropped-jpg/e1143"
;;;
;;; If you call: 
;;; (mon-build-rename-buffer ".jpg") 
;;; It will return a 'rename-buffer' of all the .jpgs in the 'matching' directory.
;;;
;;; If there aren't any .jpgs in that file it prompts for a new directory within that tree.
;;;
;;; If you call:
;;; (mon-build-rename-buffer ".bmp") 
;;; when there are .bmps in the current dir it will return a '*rename-images*' buffer 
;;; with all the .bmp's in the 'current' directory ready for marking.
;;;
;;; Currently `mon-build-rename-buffer' is only taking an IMAGE-TYPE arg.  The
;;; helper function `mon-rename-imgs-in-dir' takes an alternate path arg ALT-PATH
;;; that will soon allow you to do:
;;;
;;; (mon-build-rename-buffer ".bmp" (expand-file-name "../e1214/")
;;;
;;; i.e. build a *rename-images* buffer from files in some other dir.
;;;
;;; Assuming your var paths are set right the functions have fairly intelligent
;;; heuristics for how they navigate the paths and include completion facilities and
;;; nice prompts which attemtp to DWIM.
;;;
;;; I am particularly proud of the *rename-images* buffer generation code which is
;;; smart about presentation padding according to the filename length of
;;; images. Emacs lisp format is not nearly as extensive a format spec as CL's...
;;;
;;; Starting with `*nef-scan-drive*' the vars below will need to be adjusted
;;; according to your local path and directory tree ideally it mirrors this one:
;;; On MON local system these map out as follows:
;;; `*nef-scan-drive*'        ;=> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS"
;;; `*nef-scan-path*'         ;=> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS"
;;; `*nef-scan-nefs-path*'    ;=> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/NEFS"
;;; `*nef-scan-nef2-path*'    ;=> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2"
;;; `*ebay-images-path*'      ;=> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY"
;;; `*ebay-images-bmp-path*'  ;=> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY/BMP-Scans"
;;; `*ebay-images-jpg-path*'  ;=> "<DRIVE-OR-ROOT:>/NEFS_PHOTOS/Nef_Drive2/EBAY/BIG-cropped-jpg"
;;;
;;; ==============================
;;; EXTERNAL-FUNCTIONS: 
;;; Needed with mon-rename-image-utils.el
;;; `mon-get-buffers-parent-dir' ;mon-dir-utils.el
;;; `mon-split-string-buffer-name' ;mon-dir-utils.el
;;; `mon-truncate-path-for-prompt' ;mon-dir-utils.el
;;; `mon-buffer-written-p' ;mon-dir-utils.el
;;; `mon-toggle-read-only-point-motion' ;mon-utils.el
;;; `mon-build-dir-list' ;mon-dir-utils.el
;;; `mon-line-bol-is-eol' ;mon-utils.el
;;; `mon-delete-back-up-list' ;naf-mode-replacements.el
;;; `mon-cln-trail-whitespace' ;naf-mode-replacements.el
;;;
;;; CONSTANTS or VARIABLES:
;;;
;;; EXTERNAL-VARS: needed with mon-rename-image-utils.el
;;; `*ebay-images-lookup-path*' ;mon-dir-locals-alist.el
;;; `*nef-scan-path*' ;;mon-dir-locals-alist.el
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: 
;;; (URL `http://www.emacswiki.org/emacs/RenameImageUtils')
;;; FILE-PUBLISHED: <Timestamp: #{2009-09-28} - by MON KEY>
;;; (URL `http://www.emacswiki.org/emacs-en/mon-rename-image-utils-supplemental.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-09-20}#{} - by MON KEY>
;;;
;;; FILE-CREATED:
;;; <Timestamp: Tuesday August 11, 2009 @ 02:29.14 PM - by MON KEY>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Copyright (C) 2009 MON KEY
;;; ==========================
;;; CODE:

;;; ==============================
;;; CREATED: <Timestamp: Saturday May 23, 2009 @ 11:28.41 AM - by MON KEY>
(defun mon-get-buffers-parent-dir (&optional full)
  "Returns buffers parent directory as a string default is parent directory _only_.
When FULL is non-nil return full path of buffers parent directory as string.
If we are in a buffer which has been written to a file or _can be_ return files
parent, else return parent of buffers `default-directory'.\n
Could also be accomplished with:
\n\(car \(last\n      \(split-string \n
       \(directory-file-name (expand-file-name \"./\"\)\)\n       \"/\" t \)\)\)\n
See also: `mon-buffer-written-p', `mon-split-string-buffer-name'."
  (interactive)
  (let* ((is-written (mon-buffer-written-p))
	 (ret-buf-dir (if is-written
			  (if full
			      (directory-file-name (file-name-directory (buffer-file-name)))
			    (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
			(if full
			    (directory-file-name default-directory)
			  (file-name-nondirectory (directory-file-name default-directory))))))
    (if is-written
	(progn 
	  (message "buffer: `%s' parent dir is `%s'."  (buffer-name) ret-buf-dir)
	  ret-buf-dir)
      (progn
	(message "buffer: `%s' not written to file yet, parent of buffer's default-directory is `%s'." 
		 (buffer-name) ret-buf-dir)
	ret-buf-dir))))

;;;test-me;(mon-get-buffers-parent-dir t)
;;;test-me;(mon-get-buffers-parent-dir t)

;;; ==============================
;;; FROM: mon-dir-utils.el
;;; ================================================================
;;; CREATED: <Timestamp: Saturday May 23, 2009 @ 11:50.56 AM - by MON KEY>
;;; ================================================================
(defun mon-split-string-buffer-name ()
  "Return current `buffer-name' as a list with split-string."
  (interactive)
(let ((buf-split
      (if (mon-buffer-written-p)
	  (split-string (buffer-file-name) "/" t)
	(split-string default-directory "/" t))))
  (message "%S" buf-split)
  buf-split))

;;;test-me;(mon-split-string-buffer-name)

;;; ==============================
;;; FROM: mon-dir-utils.el
;;; ================================================================
;;; CREATED: <Timestamp: Friday May 29, 2009 @ 07:26.02 PM - by MON KEY>
;;; ================================================================
(defun mon-truncate-path-for-prompt (&optional intrp)
  "Returns a truncated path strog of current buffers path.
Useful for passing around to helper functions that prompt."
(interactive "p")
  (let* ((trunc-pth (directory-file-name (expand-file-name "./")))
	 (trunc-s (split-string trunc-pth "/"))
	 (trunc-l (length trunc-s))
	 (bld-lst))
    (setq bld-lst 
	  (cond ((>= trunc-l 3)(last trunc-s 3))
		((>= trunc-l 2)(last trunc-s 2))
		((>= trunc-l 1)(last trunc-s))))
    (setq bld-lst
	  (mapconcat 'identity bld-lst "/"))
    (if intrp
	(message "truncated path: %s" bld-lst)
      bld-lst)))

;;;test-me;(mon-truncate-path-for-prompt)
;;; ==============================

;;; ==============================
;;; FROM: mon-dir-utils.el
;;; ==============================
;;; CREATED: <Timestamp: Saturday May 23, 2009 @ 11:38.18 AM - by MON KEY>
;;; ================================================================
(defun mon-buffer-written-p ()
  "True if current buffer has been written to a file or created with `find-file'
and _can_ be written in current directory (whether it has been or not)."
  (interactive)
  (let* ((written-p (and (buffer-file-name) t))
	 (has-or-not (if written-p
			 "has or can be"
		       "_hasn't or can't_ be")))
    ;; (message "buffer `%s' %s written to file." (buffer-name) has-or-not )
    written-p))

;;;test-me;(mon-buffer-written-p)

;;; ==============================
;;; FROM: mon-utils.el
;;; ================================================================
;;; CREATED: <Timestamp: Monday June 15, 2009 @ 05:36.12 PM - by MON KEY>
;;; ================================================================
(defun mon-toggle-read-only-point-motion ()
  (interactive)
"Toggle `inhibit-read-only' and `inhibit-point-motion-hooks'."
  (if (or
       (bound-and-true-p inhibit-read-only)
       (bound-and-true-p inhibit-read-only))
      (progn
	(setq inhibit-read-only nil)
	(setq inhibit-point-motion-hooks nil))
    (progn
      (setq inhibit-read-only t)
      (setq inhibit-point-motion-hooks t))))

;;; ==============================
;;; FROM: mon-dir-utils.el
;;; ================================================================
;;; CREATED: <Timestamp: Thursday May 21, 2009 @ 08:06.42 PM - by MON KEY>
;;; ================================================================
(defun mon-build-dir-list (dir &optional not-concat-path)
  "Return a _list_ of directories in DIR.
When non-nil NOT-CONCAT-PATH returns a list _without_ the leading path."
(save-excursion
    (save-window-excursion
      (let ((temp-string)
	    (curr-buff (get-buffer (current-buffer)))
	    (in-dir dir)
	    (rtn-dir))
	(setq temp-string    
	      (with-temp-buffer
		(let ((this-buff)
		      (that-buff)
		      (ss))
		  (setq this-buff (get-buffer (current-buffer)))
		  (list-directory dir t)
		  (setq that-buff (get-buffer "*Directory*"))
		  (set-buffer that-buff)
		  (setq ss (buffer-substring-no-properties (point-min) (point-max)))
		  (set-buffer this-buff)
		  (kill-buffer that-buff)
		  (insert ss)
		  (goto-char (point-min))
		  (keep-lines "^d.*[0-9][0-9]:[0-9][0-9] .*$")
		  (goto-char (point-min))
		  (while
		      (search-forward-regexp "\\(\\(^d.*[0-9][0-9]:[0-9][0-9][[:space:]]\\)\\(.*$\\)\\)" nil t)
		    (replace-match "\\3" ))
		  (mon-cln-trail-whitespace)
		  (goto-char (point-min))
		  (while (search-forward-regexp "^\\(.*\\)$" nil t)
		    (if (and (mon-line-bol-is-eol) (not (eobp)))
			(delete-char 1)
		      (replace-match "\\1|")))
		  (while (search-backward-regexp "^\|$" nil t)
		    (if (= (char-after) 124)
		      (delete-char 1)))
		  (goto-char (point-min))
		  (mon-delete-back-up-list (point-min) (point-max))
		  (buffer-substring-no-properties (point-min) (point-max)))))
	(set-buffer curr-buff)
	(setq rtn-dir
	      (split-string temp-string "| "))
	(setq rtn-dir (delete "" rtn-dir))
	(if (not not-concat-path)
	    (setq rtn-dir
		  (let ((map-dir rtn-dir)
			(conc-dir (concat in-dir "/")))
		    (mapcar '(lambda (x) (concat conc-dir x)) map-dir)))
	  rtn-dir)
	;;(prin1 rtn-dir (current-buffer))
	rtn-dir))))

;;;test-me;(mon-build-dir-list default-directory)
;;;test-me;(mon-build-dir-list default-directory t)

;;; ==============================
;;; FROM: mon-utils.el
;;; ================================================================
;;; CREATED: <Timestamp: Thursday May 07, 2009 @ 03:17.51 PM - by MON KEY>
;;; ================================================================
(defun mon-line-bol-is-eol (&optional intrp)
  "t if postion at beginning of line is eq end of line."
(interactive "p")
  (let ((bol-eol(= (line-end-position) (line-beginning-position))))
     (cond (intrp
	 (if bol-eol
	     (message "Beginning of Line _IS_  End of Line.")
	   (message "Beginning of Line _NOT_ End of Line."))))
    bol-eol))

;;;test-me;(save-excursion (previous-line) (beginning-of-line) (mon-line-bol-is-eol))

;;; ==============================
;;; FROM: naf-mode-replacements.el
;;; ==============================
(defun mon-cln-trail-whitespace ()
    "Indiscriminately clean trailing whitespace in _ENTIRE_ buffer.
Deletes any trailing whitespace, converting tabs to spaces. Operates on 
everything in buffer _not_ region."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (if (search-forward "\t" nil t)
          (untabify (1- (point)) (point-max))))
    nil)

;;; ==============================
;;; FROM: naf-mode-replacements.el
;;; ================================================================
;;; CREATED: <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
;;; ================================================================
(defun mon-delete-back-up-list (start end &optional delim)
  "Given an text item-per-line list with no trailing whitespace, function
moves backwards from point to BOL and deletes 1 char This effecively puts
point on the next line up. With each successive previous line deleting until
point is no longer greater than point-min. Be careful this function can wreck
your data - best to evaluate alongside `with-temp-buffer'."
  (interactive "r\np")
  (let* ((test-llm (and (buffer-local-value longlines-mode (current-buffer))))
	 (is-on (and test-llm))
	 (the-delim 
	  (cond ((eq delim 1) " ")
		((not delim) " ")
		((or delim) delim)))
	 (llm-off))
    (progn
      (when is-on (longlines-mode 0) (setq llm-off 't))
      (let ((bak-start start)
	    (bak-end end)
	    (bak-pipe))
	(setq bak-pipe (buffer-substring-no-properties bak-start bak-end))
	(save-excursion
	  (setq bak-pipe
		(with-temp-buffer
		  (insert bak-pipe)
		  (progn	    
		    (mon-cln-trail-whitespace)
		    (goto-char (point-max))
		    (while
			(> (point)(point-min))
		      (beginning-of-line)
		      (insert the-delim)
		      (beginning-of-line)
		      (delete-backward-char 1)
		      (if (bolp)
			  () (beginning-of-line) ))
		    (goto-char (point-max))
		    (while (re-search-forward " " nil t)
		      (replace-match " " nil nil)))
		  (buffer-substring-no-properties (point-min) (point-max))))
	  (delete-region bak-start bak-end)
	  (insert bak-pipe)))
      (when llm-off (longlines-mode 1) (setq llm-off 'nil)))))

;;; ==============================
;;; Call lonlines-mode at least once before calling `mon-delete-back-up-list'
(save-excursion
  (let (test)
    (setq test
	  (with-temp-buffer
	    (when (not (bound-and-true-p lonlines-mode))
	      (longlines-mode))))
    (when test (message "longlines-mode initialized at least once."))))

;;; ================================================================
;;; Following FROM: mon-dir-locals-alist.el
;;; ================================================================

;;; ==============================
;;; `*nef-scan-path*' 
;;; ==============================

(defvar *nef-scan-drive* nil
  "User conditional path to ebay nef photo drive.
Called by: `mon-get-buffers-directories'.\n
See also; `*nef-scan-nefs-path*', `*nef-scan-nef2-path*', `*ebay-images-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*',`*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *nef-scan-drive*))
  (setq *nef-scan-drive* "<DRIVE-OR-ROOT:>/"))

(defvar *nef-scan-path* nil
  "User conditional path to ebay nef photo drive.
Called by: `mon-get-buffers-directories'.\n
See also; `*nef-scan-nefs-path*', `*nef-scan-nef2-path*', `*ebay-images-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*',`*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *nef-scan-path*))
  (setq *nef-scan-path* (concat *nef-scan-drive* "NEFS_PHOTOS")))

;;; ==============================
;;; `*nef-scan-nefs-path*' 
;;; ==============================
(defvar *nef-scan-nefs-path* nil
  "User conditional path to ebay NEFS drive.
See also; `*nef-scan-path*', `*nef-scan-nef2-path*', `*ebay-images-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*', `*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *nef-scan-nefs-path*))
  (setq *nef-scan-nefs-path* 
	(concat *nef-scan-path* "/NEFS")))


;;; ==============================
;;; `*nef-scan-nef2-path*'
;;; ==============================
(defvar *nef-scan-nef2-path* nil
  "User conditional path to ebay nef photo drive.
See also; `*nef-scan-path*', `*nef-scan-nefs-path*', `*ebay-images-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*', `',
`*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *nef-scan-nef2-path*))
  (setq *nef-scan-nef2-path* 
	(concat *nef-scan-path* "/Nef_Drive2")))

;;; ==============================
;;; `*ebay-images-path*' 
;;; ==============================
(defvar *ebay-images-path* nil
  "User conditional path to ebay image scans.
See also; `*nef-scan-path*', `*nef-scan-nefs-path*', `*nef-scan-nef2-path*',
`*ebay-images-bmp-path*', `*ebay-images-jpg-path*', `'
`*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *ebay-images-path*))
  (setq *ebay-images-path*
	(concat *nef-scan-nef2-path* "/EBAY")))

;;; ==============================
;;; `*ebay-images-bmp-path*' 
;;; ==============================
(defvar *ebay-images-bmp-path* nil
  "User conditional path to ebay .bmp scans.
Called By: `mon-try-comp-dir', `naf-dired-image-dir'.
See also; `*nef-scan-path*', `*nef-scan-nefs-path*', `*nef-scan-nef2-path*',
`*ebay-images-path*', `*ebay-images-jpg-path*', `*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *ebay-images-bmp-path*))
  (setq *ebay-images-bmp-path*
	(concat *ebay-images-path* "/BMP-Scans")))

;;; ==============================
;;; `*ebay-images-jpg-path*'
;;; ==============================
(defvar *ebay-images-jpg-path* nil
  "User conditional path to ebay scans converted to .jpg.
See also; `*nef-scan-path*', `*nef-scan-nefs-path*', `*nef-scan-nef2-path*',
`*ebay-images-path*', `*ebay-images-bmp-path*', `*ebay-images-lookup-path*'.")
;;
(when (not (bound-and-true-p *ebay-images-jpg-path*)) 
  (setq *ebay-images-jpg-path*
 	(concat *ebay-images-path* "/BIG-cropped-jpg")))

;;; ==============================
(defvar *ebay-images-lookup-path* nil
  "alist of paths to examine when functions need to look for images.
alist keys are of the image-type as a string: \".nef\", \".jpg\", or \".bmp\".
For these purposes we don't want to be in the NEFS folder and assume a .nef source image
is in the eBay-bmp path.
See also; `*nef-scan-path*', `*nef-scan-nefs-path*', `*nef-scan-nef2-path*',
`*ebay-images-path*', `*ebay-images-bmp-path*', `*ebay-images-jpg-path*'.")
;;
(when (not (bound-and-true-p *ebay-images-lookup-path*)) 
  (setq *ebay-images-lookup-path*
        '((".nef" *ebay-images-bmp-path* "BMP-Scans")         ; *nef-img-hash*)
          (".jpg" *ebay-images-jpg-path* "BIG-cropped-jpg")   ; *jpg-img-hash*)
          (".bmp" *ebay-images-bmp-path* "BMP-Scans"))) ; *bmp-img-hash*)))
  ;; if these get hashed:
  ;; *nef-img-hash* *bmp-img-hash* *jpg-img-hash*
  )

;;;(dired *nef-scan-nefs-path*)
;;;(boundp '*ebay-images-lookup-path*)
;;;(makunbound '*ebay-images-lookup-path*)
;;;(unintern 'ebay-images-lookup-path*)


;;; ==============================
;;;test-me;(dired *nef-scan-nefs-path*)
;;;test-me;(dired *nef-scan-path*)
;;;test-me;(dired *nef-scan-nefs-path*)
;;;test-me;(dired *nef-scan-nef2-path*)
;;;test-me;(dired *ebay-images-path*)
;;;test-me;(dired *ebay-images-bmp-path*)
;;;test-me;(dired *ebay-images-jpg-path*)
;;; ==============================

;;; ==============================
;; CLEANUP:
;; (mapc '(
;;  (unintern '*nef-scan-path*)
;;  (unintern '*nef-scan-nefs-path*)
;;  (unintern '*nef-scan-nef2-path*)
;;  (unintern '*ebay-images-path*)
;;  (unintern '*ebay-images-bmp-path*)
;;  (unintern '*ebay-images-jpg-path*)))
;; ==============================

;;; ==============================
(provide 'mon-rename-images-supplemental)
;;; ==============================

;;; ================================================================
;;; mon-rename-images-supplemental.el ends here
;;; EOF
