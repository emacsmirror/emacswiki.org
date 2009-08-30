;;; setup-helper.el --- Functions for installation and setup of Emacs

;; Copyright (C) 2004 by Lennart Borgman

;; Author:     Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Maintainer: Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2004-12-04
;; Version: 1.81
;; Keywords: installation setup

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a collection of functions maybe useful when installing or
;; setting up emacs.

;; BETA VERSION!!!!! NOT READY!!!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old modules

;;(defconst mytimes 0)
(defvar setup-helper-overlay nil)

(defvar setup-helper-recursive-quit nil)

(defun setup-helper-mk-overlay(start end)
  (when setup-helper-overlay
    (delete-overlay setup-helper-overlay))
  (setq setup-helper-overlay (make-overlay 0 0))
  (overlay-put setup-helper-overlay 'before-string "<<<")
  (overlay-put setup-helper-overlay 'after-string  ">>>")
  (overlay-put setup-helper-overlay 'face (cons 'background-color "yellow"))
  (move-overlay setup-helper-overlay start end)
  )

(defun setup-helper-find-replace-lisp(old-lisp
				      &optional
				      old-is-regexp
				      replacement
				      query
				      dont-recurse
				      max-point dd)
  "Find and optionally replace a string representing a lisp object.

Searches for OLD-LISP.  Only searches the next lisp object in the
current buffer.  To search the whole of the buffer use
`setup-helper-find-replace-lisp-whole'.

OLD-LISP is considered to be a lisp object and matched with `equal'
unless OLD-IS-REGEXP is non-nil in which case OLD-LISP must be a
string which is used as a regular expression to match the print
representation of whole lisp objects.

REPLAMENT must be nil, a string or 'comment-out. When REPLACEMENT is a
string OLD-LISP are replaced with REPLACEMENT.  If REPLACEMENT is
'comment-out OLD-LISP is commented out.  In this case DONT-RECURSE
must be non-nil. (This restriction is there to avoid problems with
syntax or semantics of the changed lisp code.)

Unless DONT-RECURSE is non-nil the search recurses into inner lisp
objects.  

When QUERY is non-nil it must be a string and the user is asked to
accept or deny each change. In this case the prompt is constructed
from the string QUERY where ${new} in this string is replaced with
with REPLACEMENT.

MAX-POINT is used internally to bound the search.  DD is used for
debugging.

If REPLACEMENT is non-nil return t if OLD-LISP is found.  If
REPLACEMENT is nil return t if there are more lisp objects to read on
current level of recursion.  Otherwise return nil.

Set position after read lisp object.
"
  (interactive)
;;   (message "RL old=%s regexp=%s r=%s q=%s dr=%s %s %s"
;; 	   old-lisp old-is-regexp replacement query dont-recurse max-point dd)
  ;;(setq mytimes (+ mytimes 1))

  ;;;; Check params
  (when old-is-regexp
    (unless (stringp old-lisp)
      (error "OLD-LISP must be a string if OLD-IS-REGEXP is non-nil."))
    (unless (equal (substring old-lisp 0 2) "\\`")
      (error "When OLD-LISP is a regexp it must start with \\\\'"))
    (unless (equal (substring old-lisp -2) "\\'")
      (error "When OLD-LISP is a regexp it must end with \\\\'")) )
  (when (and (eq replacement 'comment-out) (not dont-recurse))
    (error "DONT-RECURSE must be t if REPLACEMENT is 'comment-out"))
  (unless dd (setq dd 0))
  
  (unless setup-helper-recursive-quit
    (while (forward-comment 1))
    (let* ((dbg nil)
	   (start (point-marker))
	   (eo-level)
	   (read-sexp) ;; read lisp object
	   (read-sexp-str) ;; read lisp object as string
	   (buffer-sexp) ;; the actually read text
	   (new-max)
	   (hit)
	   (replaced)
	   (comment-out (eq replacement 'comment-out)))
      (when dbg (message "old-lisp=%s" (prin1-to-string old-lisp)))
      (if (and max-point (> (point) max-point))
	  (progn (setq read-sexp t) (setq eo-level t))
	(condition-case nil
	    (progn (setq read-sexp (read (current-buffer))) 
		   (setq buffer-sexp (buffer-substring start (point)))
		   (setq new-max (point)))
	  (error (setq read-sexp t) (setq eo-level t))))
      (unless (or eo-level (not read-sexp))
	(setq read-sexp-str (prin1-to-string read-sexp))
	(setq hit (if old-is-regexp
		      (progn
;; 			(message "matching old-lisp=%s buffer-sexp=%s\n=== dont-recurse=%s"
;; 				 old-lisp buffer-sexp dont-recurse)
			(string-match old-lisp buffer-sexp)
			)
		    (equal read-sexp old-lisp)))
	;;(when hit (message "hit read-sexp=%s" read-sexp))
	(save-excursion
	  (if hit
	      (when replacement
		(if comment-out
		    (let ((end (point-marker))
			  (do-it t))
		      ;;(message "replace-lisp.comment-out.query=%s" query)
		      (if query (let ((prompt query))
				  (setup-helper-mk-overlay start end)
				  (condition-case nil
				      (setq do-it (y-or-n-p prompt))
				    (quit
				     (setq do-it nil)
				     (setq setup-helper-recursive-quit t)
				     (delete-overlay setup-helper-overlay)))
				  (delete-overlay setup-helper-overlay)))
		      (if do-it
			  (save-excursion
			    ;;(goto-char (+ end 2))
			    (insert "\n")
			    (goto-char start)
			    (insert
			     (concat "\n**** Commented out by setup-helper-find-replace-lisp ("
				     (current-time-string) ")\n"))
			    (comment-region start end)
			    (message "Commented out.")
			    )
			(message "Skipped."))
		      )
		  (let* ((end (point-marker))
			 (repl-in-repl
			  (save-match-data
			    (if old-is-regexp nil
			      (let ((pos 1)
				    (lst nil))
				(while (setq pos (string-match "\\\\&" replacement pos))
				  (push pos lst)
				  (setq pos (+ pos 1)))
				lst))))
			 (old-text)
			 (new-text
			  (if old-is-regexp
			      (progn (replace-match replacement t nil buffer-sexp))
			    (if repl-in-repl
				(mapc (lambda (pos)
					(setq new-text
					      (concat (substring replacement 0 (- pos 1))
						      old-text
						      (substring replacement (+ pos +2)))))
				      repl-in-repl)
			      replacement)))
			 (do-it t))
		    ;;(message "replace-lisp.not comment-out.query=%s" query)
		    (if query
			(save-match-data
			  (let ((prompt (replace-regexp-in-string 
					 "${new}" new-text query)))
			    (setup-helper-mk-overlay start end)
			    ;;(message "replacement=%s" replacement)
			    ;;(message "buffer-sexp=%s" buffer-sexp)
			    ;;(message "read-sexp-str=%s" read-sexp-str)
			    ;;(message "old-is-regexp=%s" old-is-regexp)
			    ;;(message "repl-in-repl=%s" repl-in-repl)
			    ;;(message "query=%s" query)
			    ;;(message "new-text=%s" new-text)
			    ;;(message "prompt=%s" prompt)
			    (condition-case nil
				(setq do-it (y-or-n-p prompt))
			      (quit
			       (setq do-it nil)
			       (setq setup-helper-recursive-quit t)
			       (delete-overlay setup-helper-overlay)))
			    (delete-overlay setup-helper-overlay)))
		      )
		    (if (not do-it)
			(message "Skipped.")
		      (when dbg (message "YES %s %s %s=%s" dd repl-in-repl start read-sexp-str))
		      (save-excursion
			;;(when nil
			(let ((cmnt-text (buffer-substring 
					  (save-excursion 
					    (goto-char start)
					    (beginning-of-line)
					    (point))
					  (save-excursion
					    (goto-char end)
					    (end-of-line)
					    (point)))))
			  (setq old-text (delete-and-extract-region start end))
			  (unless (equal old-text buffer-sexp) (error "old-text <> buffer-sexp"))
			  (save-excursion
			    (beginning-of-line)
			    (let ((cstart (point)))
			      (insert
			       (concat "\n**** Commented out by setup-helper-find-replace-lisp ("
				       (current-time-string) ")\n"
				       cmnt-text
				       "\n**** and replaced with:\n"))
			      (comment-region cstart (point))))
			  (goto-char start)
			  (insert new-text)))
		      (message "Replaced.")
		      (setq replaced t)
		      )))
		(sleep-for 2)
		)
	    (when (not dont-recurse)
	      ;;(message "NO %s %s %s" dd (listp read-sexp) read-sexp)
	      (when (and read-sexp
			 (listp read-sexp))
		;; goto after opening parenthesis
		(goto-char start) (forward-char)
		(while (< (point) new-max)
		  (when
		      (setup-helper-find-replace-lisp old-lisp old-is-regexp replacement
						      query dont-recurse new-max (+ dd 1))
		    (setq replaced t)))
		;;(setq eo-level t)
		)))))
      (if replacement (not eo-level) hit)
      hit
      (if replacement replaced hit)
      )))


(defun setup-helper-find-replace-lisp-whole(old-lisp
					    &optional
					    old-is-regexp replacement
					    query
					    dont-recurse
					    max-point dd)
  "See `setup-helper-find-replace-lisp'."
  (interactive)
  (setq setup-helper-recursive-quit nil)
  (save-excursion
    (save-window-excursion
      (save-match-data
	(goto-char (point-min))
	(let ((hit))
	  (while (not (eobp))
	    ;;(message "whole while")
	    (when (setup-helper-find-replace-lisp
		   old-lisp old-is-regexp replacement query dont-recurse max-point)
	      (setq hit t)))
	  hit)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add sexp
 
(defun setup-helper-add-sexp-if-not-found(file sexpstr &optional comment recurse append)
  "Add sexp to a file if it is not found in the file.
SEXPSTR should be given as a string.

If COMMENT is non-nil it is inserted before SEXPSTR and commented out.

If RECURSE is non-nil look inside the lisp object if it is a list and
does not match.

When APPEND is non-nil do the insertion at the end, otherwise at the
beginning."
  ;;(message "add-sexp-inf %s %s %s %s %s" file sexpstr comment recurse append)
  (let ((sexp (read sexpstr)))
    (unless (listp sexp) 
      (error "Only a full sexp can be added"))
    (save-excursion
      (find-file file)
      (if (setup-helper-find-replace-lisp-whole sexp nil nil nil (not recurse))
	  (message "Already in file.")
	(message "Adding")
	(if append
	    (goto-char (point-max))
	  (goto-char (point-min)))
	(when append (unless (bobp) (insert "\n\n\n")))
	(when comment
	  (let ((start (point-marker)))
	    (insert comment "\n")
	    (comment-region start (point))))
	(insert sexpstr)
	(unless append (unless (eobp) (insert "\n\n\n")))
	(save-buffer)
	))))

;; (defun comment-out-cua()
;;   (interactive)
;;   (let* ((sexplist (list
;; 		    '(require (quote cua))
;; 		    '(require (quote cua) nil t)
;; 		    '(setq CUA-mode t)
;; 		    '(setq CUA-mode nil)
;; 		    '(set (quote CUA-mode) t)
;; 		    '(set (quote CUA-mode) nil)
;; 		    )))
;;     (setup-helper-comment-all-matching-sexp sexplist)
;;     ))



(defun setup-helper-fix-libdbl(library)
  (interactive "sLibrary: ")
  (let* ((file (locate-library library)) (dir) (ver) (verstr)
	 (files (list))
	 (path (mapcar (lambda (dir) (file-name-as-directory (expand-file-name dir)))
		       load-path))
	 )
    (while file
      (setq dir (file-name-directory file))
      (delete dir path)
      (setq ver 0)
      (with-temp-buffer
	(insert-file file)
	(goto-char (point-min))
	(re-search-forward ";;\\s +Version:\\s +\\(\\S-+\\)")
	(when (match-beginning 1)
	  (message "%s (%s)" (match-string 0) (match-string 1))
	  (setq verstr (match-string 1))
	  (setq ver (string-to-number verstr))
	  )
	)
      (push (vector file ver verstr) files)
      ;;(setq files (append files (list (cons file (vector ver verstr)))))
      (message "files=%s dir=%s" files dir)
      (setq file (locate-library library nil path))
      )
    (when (> (length files) 1) 
      (let* ((ol (reverse files))
	     (first (car ol))
	     (next)
	     )
	(message "ol=%s" ol)
	(message "first=%s" first)
	(setq ol (cdr ol))
	(while ol
	  (setq next (car ol))
	  (when (< (elt first 1) (elt next 1))
	    (message "next=%s" next)
	    (if (y-or-n-p 
		 (concat "Version " (elt next 2) " of module " library 
			 " is available on your computer, but it is currently hidden by " 
			 " the file " (elt first 0) " which contains version " (elt first 2)
			 ". Do you want to rename this file so that"
			 " the newer version can be used?"))
		(message "rename")
	      (message "Ok, keeping old version"))
	    )
	  (setq ol (cdr ol))
	  )
	))
    ))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dirs and files

;; I do not know if this is the case for all Emacs installations, but
;; at least in my 21.3 MS Windows installation there are two site-lisp
;; dirs in load-path.

(defun setup-helper-site-lisp1()
  "Path to site-lisp outer dir"
  (expand-file-name (concat exec-directory "../../site-lisp/")))
(defun setup-helper-site-lisp2()
  "Path to site-lisp inner dir"
  (expand-file-name (concat exec-directory "../site-lisp/")))

(defun setup-helper-dot-emacs()
  "Get the full path name to .emacs"
  (expand-file-name "~/.emacs"))

(defun setup-helper-default-el-file-name1()
  "Get the full path name to use for outer default.el"
  (expand-file-name (concat exec-directory "../../site-lisp/default.el")))
(defun setup-helper-default-el-file-name2()
  "Get the full path name to use for inner default.el"
  (expand-file-name (concat exec-directory "../site-lisp/default.el")))

(defun setup-helper-site-start-el-file-name1()
  "Get the full path name to use for outer site-start.el"
  (expand-file-name (concat exec-directory "../../site-lisp/site-start.el")))
(defun setup-helper-site-start-el-file-name2()
  "Get the full path name to use for inner site-start.el"
  (expand-file-name (concat exec-directory "../site-lisp/site-start.el")))
 

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize
 
(defun setup-helper-add-custom(symbol value &optional requests )
  "Adds to custom-set-variables. Call custom-save-all to save!"
  (let ((comment "Added by setup-helper"))
    (put symbol 'saved-value (list value))
    (if requests (put symbol 'custom-requests (list requests)))
    (put symbol 'saved-variable-comment comment)) )


(defun setup-helper-customize-list(defgroup-var defcustom-list)
  (mapcar (lambda (var)
	    (custom-add-to-group defgroup-var var 'custom-variable))
	  defcustom-list)
  (customize-group defgroup-var))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add lines (comments)

;; I do not like it - to unsafe, just saves the code here! 
;; (defun setup-helper-add-lines-if-not-found(file lines append)
;;   "Adds lines to a file if it is not found in the file."
;;   (let ((appendre   (concat "^"  (replace-regexp-in-string "\(" "\(" lines) "$"))
;; 	(file-buffer (find-file-noselect file)))
;;     (save-excursion
;;       (set-buffer file-buffer)
;;       (let ((nonempty-match
;; 	     (if (string-match appendre (buffer-string)) t nil)))
;; 	(if nonempty-match
;; 	    (message "Lines were already in file...")
;; 	  (message "Inserting lines...")
;; 	  (if append
;; 	      (goto-char (point-max))
;; 	    (goto-char (point-min)))
;; 	  (let ((appendline
;; 		 (concat (when append "\n\n\n")
;; 			 ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
;; 			 ";;;; Following line(s) inserted by setup-helper.el:\n"
;; 			 lines
;; 			 (unless append "\n\n\n"))))
;; 	    (insert appendline)
;; 	    (save-buffer) ) )))))


(defun setup-helper-repl-all-regexp (srchexp replstr) 
  "Reg exp replace in current buffer."
  (save-excursion
    (goto-char (point-min))
    (replace-regexp srchexp replstr)))

(defun setup-helper-q (str)
  "Quote string - for better syntax hilighting..."
  (let ((q "\""))
    (concat q str q)))

(provide 'setup-helper)

;;; setup-helper.el ends here
