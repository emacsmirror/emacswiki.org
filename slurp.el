;;; slurp.el --- Search a file hierarchy for files with matching names

;; Author: Jason Meade (jemeade@gmail.com)
;; Last Update: Aug 5, 2011
;; Version: 1.4

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; slurp.el is a handly tool used to search a file hierarchy for files with
;; names that match a regular expression. Slurp will load those files into
;; emacs buffers for you.

;; Keep setf properly resolved as a macro instead of accidententally compiled
;; as a function call.  (Thanks to Phillip Hudson for pointing this out)

;;; Code:

(eval-when-compile (require 'cl))

;; Create slurp mode definitions
(defvar slurp-mode-map nil "Keymap used while in slurp mode")
(defconst +slurp-preface+ "Slurped: ")

(if slurp-mode-map
    ()
  (setf slurp-mode-map (make-keymap))
  (define-key slurp-mode-map [mouse-1] 'slurp-mode-find-buffer)
  (define-key slurp-mode-map [return] 'slurp-mode-find-buffer-no-event)
  (define-key slurp-mode-map (kbd "RET") 'slurp-mode-find-buffer-no-event))

(defun slurp-mode ()
  "Major mode for buffers used by slurp"
  (interactive)
  (kill-all-local-variables)
  (use-local-map slurp-mode-map)
  (setf mode-name "Slurp")
  (setf major-mode 'slurp-mode)
  (setf buffer-read-only t))

(defun slurp-mode-find-buffer (event)
  (interactive "e")
  (let ((buffer (thing-at-point 'line)))
    (if (is-slurp-buffer buffer)
	(progn
	  (setf buffer (slurp-cleanup-buffer-name buffer))
	  (switch-to-buffer-other-window buffer)))))

(defun slurp-mode-find-buffer-no-event ()
  (interactive)
  (let ((buffer (thing-at-point 'line)))
    (if (is-slurp-buffer buffer)
	(progn
	  (setf buffer (slurp-cleanup-buffer-name buffer))
	  (switch-to-buffer-other-window buffer)))))

(defun is-slurp-buffer (buffer)
  (eq (compare-strings buffer 0 (length +slurp-preface+) +slurp-preface+ 0 (length +slurp-preface+)) t))

(defun slurp-cleanup-buffer-name (buffer)
  (let ((start (length +slurp-preface+))
	(end (- (length buffer) 1)))
    (substring buffer start end)))

;; You may need to adjust this on your system
(setq max-lisp-eval-depth 1000)

(defvar slurp-file-count 0)
(defvar *slurp-buffer* "*slurped-files*")

;; A list of regular expressions to compare files against. Items
;; in this list won't be slurped.
(defvar slurp-ignore-filelist
  (list "^\.#"	   ; session recovery files
	"~$"	   ; backup files
	"\.tar$"   ; common binary files
	"\.gz$"
	"\.tgz$"
	"\.bz2$"
	"\.zip$"
	"\.doc$"
	"\.docx$"
	"\.ppt$"
	"\.xls$"
	"\.xlsx$"
	"\.class$"
	"\.o$"
	"\.obj$"
	))

;;
(defun slurp ()
  "Search for and load files from a directory hierarchy matching a regexp"
  (interactive
   (let ((filter (read-from-minibuffer "Slurp files by regexp: "))
	 (slurp-dir default-directory))
     (if (get-buffer *slurp-buffer*)
	 (kill-buffer *slurp-buffer*))
     (get-buffer-create *slurp-buffer*)
     (switch-to-buffer-other-window *slurp-buffer*)
     (erase-buffer)
     (cd slurp-dir)
     (setq slurp-file-count 0)
     (insert (concat "Slurping contents of " slurp-dir " using regexp filter: " filter))
     (insert "\n\n")
     (slurp-files filter)
     (insert (concat "\nDone. " (int-to-string slurp-file-count) " files slurped\n"))
     (slurp-mode))))

;;
(defun slurp-files (filter)
  "Slurp files in the current directory matching a regexp"
  (dolist (file (directory-files "."))
    (if (slurp-good-directory file)
	(progn
	  (cd file)
	  (slurp-files filter)
	  (cd "..")))
    (if (slurp-do-slurp file filter)
	(slurp-file file))))

;;
(defun slurp-good-directory (file)
  "Check to see if this is a good directory"
  (and (file-directory-p file) (file-executable-p file)
       (not (equal file "."))(not (equal file ".."))
       (not (equal file "CVS"))))

;;
(defun slurp-file (file)
  "Load an individual file"
  (setq slurp-file-count (+ slurp-file-count 1))
  (let ((buffer-name (buffer-name (find-file-noselect file))))
    (insert (concat +slurp-preface+ (propertize buffer-name 'mouse-face 'highlight) "\n"))))

;;
(defun slurp-do-slurp (file filter)
  "Fetch the files you want"
  (let ((do-slurp nil))
    (if (string-match filter file)
	(setq do-slurp t))
    (dolist (expr slurp-ignore-filelist)
      (if (string-match expr file)
	  (setq do-slurp nil)))
    do-slurp))

(provide 'slurp)

;;; End Slurp
