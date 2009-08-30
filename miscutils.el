;;; miscutils.el --- Miscallaneous utility functions for Editing,
;;  browsing etc.

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Anand B Pillai <abpillai@lycos.com>
;; Keywords: lisp, tools

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;  
;; The latest version of this code should be available at
;;
;;  <URL: http://members.fortunecity.com/anandpillai/emacs/miscutils.el.html>
;; 

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add a path to emacs' load path;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-path-to-loadpath( arg )
(interactive "DAdd directory to load path: ")
(setq load-path (cons arg load-path))
(let* ((files (directory-files arg t)) (len (length files)))
(while files
  (setq file (car files))
  (if (equal (file-name-extension file) "el")
	  (progn
 		(message "Loading file %s..." file)
		(load-file file)))
  (setq files (cdr files)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill the first word in all lines a buffer;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-first-word()
  (interactive)
  (save-excursion
  	(beginning-of-buffer)
  	(while (not (eobp))
  	  (kill-word 1)
  	  (forward-line 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill the last word in all lines in buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-last-word()
  (interactive)
  (save-excursion
	(beginning-of-buffer)
	(while (not (eobp))
	  (end-of-line)
	  (backward-kill-word 1)
	  (forward-line 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert word at end of all lines in buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-word-line-end(arg)
(interactive "sWord: ")
(beginning-of-buffer)
(while (not (eobp))
  (end-of-line)
  (insert-string arg)
  (forward-line 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert word at begin of all lines in buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-word-buffer-start(arg)
(interactive "sWord: ")
(beginning-of-buffer)
(while (not (eobp))
  (beginning-of-line)
  (insert-string arg)
  (forward-line 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill last character of all lines in buffer;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-last-char()
(interactive)
(beginning-of-buffer)
(while (not (eobp))
  (end-of-line)
  (backward-kill-char 1)
  (forward-line 1)))

;;;;;;;;;;;;;;;;;;;;;;
;; Removes newlines ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun remove-newlines()
(interactive)
(while (not (eobp))
  (replace-string "\n" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove duplicate lines in buffer;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-duplicate-lines()
  (interactive)
  (save-excursion
    (let
        ((lines_hash (make-hash-table :test #'equal))
         (numlines (count-lines 1 (progn (end-of-buffer)(point)))))

      ;; Make a hash table with key=line 
      ;;     and value=the smallest line number that contains a line.
      (loop for i from numlines downto 1 do
           (let ((line nil))
             (goto-line i)
             (setf line (buffer-substring 
                         (progn (beginning-of-line) (point))
                         (progn (end-of-line) (point))))
             ;; Want to store the smallest line number for 
             ;;     a particular line.
             (setf (gethash line lines_hash) i)))
      ;; If a line has a line number not equal to the smallest line, kill it.
      (loop for i from numlines downto 1 do
           (let ((line nil))
             (goto-line i)
             (setf line (buffer-substring 
                         (progn (beginning-of-line) (point))
                         (progn (end-of-line) (point))))
             (beginning-of-line)
             (if (not (equal line ""))
                 (if (not (= 
                           (let ((min-line (gethash line lines_hash)))
                             (if (null min-line)
                                 -1
                               min-line))
                           i))
                     (kill-line 1))))))))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment out a line and duplicate it ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun line-comment-and-duplicate()
"Duplicates a line after commenting original one."

(interactive)
(let (
	  (beg (line-beginning-position))
	  (end (+ 1 (line-end-position))))
  (copy-region-as-kill beg end)
  (comment-region beg end)
  (beginning-of-line)
  (forward-line 1)
  (yank)
  (forward-line -1)))


(defun line-comment()
"Comments out current line."
(interactive)
(comment-region (line-beginning-position) (+ 1 (line-end-position)))
(setq line-comment-call nil))

(defun line-uncomment()
"Uncomments current line."
(interactive)
(uncomment-region (line-beginning-position) (+ 1 (line-end-position)))
(setq line-comment-call t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Uncomment matching lines in buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uncomment-matching( arg )
"Uncomment lines matching regular expressions in a buffer."
(interactive "sRegexp: ")
(save-excursion
  (goto-char (point-min))
  (while (re-search-forward arg (point-max) t)
		  (line-uncomment))))

(global-set-key "\C-c\C-u" 'uncomment-matching)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment out matching lines in buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun comment-matching (arg)
"Comment lines matching regular expressions in a buffer."
(interactive "sRegexp: ")
(save-excursion
  (goto-char (point-min))
  (while (re-search-forward arg (point-max) t)
		  (line-comment))))

(global-set-key "\C-c\C-m" 'uncomment-matching)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forward comment a paragraph ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun para-comment-fwd()
(interactive)
(let (posn (para-ending-position))
(while (not (eq (point) posn))
  (line-comment)
  (forward-line 1))))

(defun para-comment-and-duplicate()
"Duplicate a paragraph after commenting out original one."

(interactive)
(let ((beg (line-beginning-position))
  (end (save-excursion (forward-paragraph) (point))))
  (copy-region-as-kill beg end)
  (para-comment-fwd)
  (yank)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yank alternate newlines ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yank-alt-newline()
(interactive)
(while (not (looking-at "\ \n"))
  (end-of-line)
  (kill-line 1)
  (forward-line 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Use wget to download a URL and view it in browser ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wget-fetch-url-and-show( url )
  (interactive "sEnter URL: ")
  (setq opfile (concat (getenv "TEMP") "/" (car (reverse (split-string url "/")))))
  (setq browse-url-new-window-flag nil)
  (setq wget-process (concat "wget --output-document=" opfile " " url ))
  (call-process "sh" nil "*Messages*" " " "-c" wget-process " " "&")
  (if (file-exists-p opfile)
	  (progn
		(require 'browse-url)
  ;; open the page in mozilla
		(browse-url-netscape (concat "file://" opfile)))))


(provide 'miscutils)

;;; miscutils.el ends here
