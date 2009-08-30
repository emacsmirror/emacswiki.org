;;; extraedit.el --- Extra useful edit functions and macros

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Anand B Pillai <abpillai@_remove_me_gmail.com>
;; Keywords: tools, convenience, matching, lisp

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

;;  Mon Mar 03 15:00:43 2003 - First version.
;;  Jan 17 2005  - Fixed bugs.
;;  July 2005    - Added 2 functions, re-ordered.

;;; Code:

;; Editing commands

(setq  indent-tabs-mode nil)

(defmacro get-current-line()
  "Current line string"
  (buffer-substring (save-excursion (beginning-of-line) (point))
            (save-excursion (end-of-line) (point))))

(defmacro line-length()
  "Length of a line in number of characters"
  (length (buffer-substring (save-excursion (beginning-of-line) (point))
                (save-excursion (end-of-line) (point)))))

;; kill the first word in all lines a buffer
(defun kill-first-word()
  "Kill first word in all lines in a buffer"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (not (eobp))
      (kill-word 1)
      (forward-line 1))))

(defun kill-first-char()
  "Kill first character in all lines in a buffer"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (not (eobp))
      (delete-char 1)
      (forward-line 1))))

;; insert a word at end of all lines in a buffer
(defun insert-word-line-end(arg)
  "Insert a word at end of all lines in a buffer"
  (interactive "sWord: ")
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "$")
      (insert-string arg)
      (forward-char))))

;; insert a word at beginning of all lines in a buffer
(defun insert-word-line-start(arg)
  "Insert a word at beginning of all lines in a buffer"
  (interactive "sWord: ")
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "^")
      (insert-string arg)
      (forward-char))))

;; remove all newlines in a buffer
(defun remove-newlines()
  "Remove newlines from a buffer"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "\n")
      (replace-string "\n" "")
      (forward-char))))

;; remove duplicate lines in a buffer
(defun remove-duplicate-lines()
  "Remove duplicate lines in a buffer"
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
             (setf line (get-current-line))
             ;; Want to store the smallest line number for 
             ;;     a particular line.
             (setf (gethash line lines_hash) i)))
      ;; If a line has a line number not equal to the smallest line, kill it.
      (loop for i from numlines downto 1 do
           (let ((line nil))
             (goto-line i)
             (setf line (get-current-line))
             (beginning-of-line)
             (if (not (equal line ""))
                 (if (not (= 
                           (let ((min-line (gethash line lines_hash)))
                             (if (null min-line)
                                 -1
                               min-line))
                           i))
                     (kill-line 1))))))))
     
;; Comment a line and duplicate it
(defun line-comment-and-duplicate()
  "Comment a line and duplicate it."
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

;; comment out current line
(defun line-comment()
  "Comments out current line."
  (interactive)
  (comment-region (+ (current-indentation) (line-beginning-position)) (+ 1 (line-end-position))))

;; Uncomment current line
(defun line-uncomment()
  "Uncomments current line."
  (interactive)
  (uncomment-region (+ (current-indentation) (line-beginning-position)) (+ 1 (line-end-position))))

;; Uncomment matching lines in a buffer, matching the regular expression
;; regexp
(defun uncomment-matching( regexp )
  "Uncomment lines matching regular expressions in a buffer."
  (interactive "sRegexp: ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp (point-max) t)
      (line-uncomment))))

(global-set-key "\C-c\C-u" 'uncomment-matching)

;; Comment matching lines in a buffer, matching the regular expression
;; regexp.
(defun comment-matching (regexp)
  "Comment lines matching regular expressions in a buffer."
  (interactive "sRegexp: ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp (point-max) t)
      (line-comment))))

(global-set-key "\C-c\C-m" 'uncomment-matching)

;; Comment a pargraph forward
(defun para-comment-forward()
  "Comment a paragraph forward from the current line."
  (interactive)
  (while (not (eq (line-length) 0))
      (line-comment)
      (forward-line 1)))

;; Comment a paragraph backward
(defun para-comment-backward()
  "Comment a paragraph backward from the current line."
  (interactive)
    (while (not (eq (line-length) 0))
      (line-comment)
      (forward-line -1)))

;; Comment out a paragraph and duplicate it
(defun para-comment-and-duplicate()
  "Comment out a paragraph and duplicate it"
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion (forward-paragraph) (point))))
    (copy-region-as-kill beg end)
    (para-comment-forward)
    (yank)))

(defun duplicate-paragraph()
  "Duplicate a paragraph"
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion (forward-paragraph) (point))))
    (copy-region-as-kill beg end)
    (yank)))

;; Delete alternate newlines in a buffer
(defun alt-newline-yank()
  "Kill alternate newlines in buffer"
  (interactive)
  (while (not (looking-at "\ \n"))
    (end-of-line)
    (kill-line 1)
    (forward-line 1)))

(defun copy-region-between-lines(arg1 arg2)
  """Copy region between two lines in a buffer. 
     The copied region can be pasted using <yank>"""
  (interactive "nFrom line number: \nnTo line number: ")
  (if (< arg2 arg1)
      (error "Invalid arguments"))
  (save-excursion 
    (goto-line arg1) 
    (setq beg (line-beginning-position))
    (goto-line arg2)
    (setq end (line-end-position))
    (copy-region-as-kill beg end))
  (message "Copied region to clipboard"))

;; Other stuff which actually don't belong here!

;; Add a directory path to the emacs load path
(defun add-path-to-loadpath( arg )
  "Adds a directory to the load-path variable of emacs"
  (interactive "DAdd directory to load path: ")
  (setq load-path (cons arg load-path))
  (let* ((files (directory-files arg t)) (len (length files)) f)
    (while files
      (setq f (car files))
      (if (equal (file-name-extension f) "el")
          (progn
            (message "Loading file %s..." f)
             (load-file f)
             ))
      (setq files (cdr files)))))

;; kill the first word in all lines a buffer
;; Set default font to current frame font
;; and save the setting in user's .emacs file.
(defun set-current-font()
"This function sets the current frame font as the default font 
and saves the option in users emacs init file"
(interactive)
(let ((prevset) (curr-font (face-font 'default)))
  (set-default-font curr-font)
  ;;set it in .emacs file
  (setq font-comment-string ";; default font setting\n")
  (setq font-string (concat font-comment-string "(set-default-font \"" curr-font  "\")\n"))
  (save-excursion
    ;; emacs custom file is saved in the variable "user-init-file"
    (set-buffer (find-file user-init-file))
    (goto-char (point-min))
    ;;find all occurences of previous font-setting 
    (while (re-search-forward "set-default-font" (point-max) t)
      (progn
        (setq prevset t)
        (beginning-of-line)
        ;;delete the previous font-setting command strings
        (kill-line)(kill-line)))
    ;; no previous setting, insert setting at the end
    ;; of the buffer.
    (if (eq prevset nil)
        (goto-char (point-max)))
    (insert-string font-string)
    (save-buffer) (kill-buffer (current-buffer)))))

(provide 'extraedit)
;;; extraedit.el ends here
