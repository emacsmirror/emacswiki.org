;;; file-journal.el --- revisit files by date

;; Copyright (C) 2008  Tamas Patrovics
;;                     Jonathan Arkell (current mainteiner)

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
(defgroup file-journal '()
  "Filejournal keeps a list of all the files you have visited, like
a persistent most-recently viewed list.  You can control the number
of days to keep the journal for.  Additionally you can set some
files to not be on the list.

Work with files as usual and use M-x fj-show to revisit them later
by date.

")

;;; Changelog:
;; v 0.1 - First release by Tamas
;; v 0.2 - Changes by Jonathan Arkell
;;       - Minor fixes to allow for vars to be customizeable
;;       - Added an exclusion list
;; v 0.3 - Added a timer that saves the journal once an hour.
;;       - Added a code to refresh the *file-journal* buffer
;;         when a new file is visited.
;; v 0.4 - Small optimization to make the exclusion not suck
;;       - Integration with anything

;; TODO:
;; - Hook into, or replace ECBs previous files list.
;; - Open region
;; - Keep a list of the times that the file has been accessed, so
;;   a list of favorite files can be built.


;; Tested on Emacs 22.

;;; Code:

(eval-when-compile
 (require 'cl))

(defcustom fj-journal-size 5
  "Number of past days to keep in the journal."
  :type 'integer
  :group 'file-journal)

(defcustom fj-journal-file "~/.file-journal"
  "File where journal info is stored."
  :type 'file
  :group 'file-journal)

(defcustom fj-exclude-files '()
  "List of files to exclude from journal (each item a regex).

I use the regex .*\.muse$ to not store any muse files, otherwise planner
mode tends to pollute the list."
  :type '(repeat regexp)
  :group 'file-journal)

(defcustom fj-save-timer-interval 3600
  "Autosave the journal after this many seconds."
  :type 'integer
  :group 'file-journal)

(defface fj-header-face
  '((t (:inherit highlight)))
  "Face for date headers."
  :group 'file-journal)

(defvar fj-journal nil
  "List of (DATE . FILES) pairs describing which files were visited when.")


(define-derived-mode fj-mode fundamental-mode "File Journal")

(suppress-keymap fj-mode-map)
(define-key fj-mode-map (kbd "<return>") 'fj-visit-file)


(defun fj-show ()
  "Show the journal and allow the user to select a file."
  (interactive)
  (switch-to-buffer "*file-journal*")
  (fj-mode)
  (fj-update-fj-buffer)
  (message (substitute-command-keys "Visit a file with \\[fj-visit-file]")))


(defun fj-update-fj-buffer ()
  "Update the contents of the journal buffer"
  (erase-buffer)
  (dolist (entry fj-journal)
		  (unless (bobp)
				  (insert "\n"))
		  (let ((start (point)))
			(insert (car entry) "\n")
			(put-text-property start (point) 'face 'fj-header-face))
		  (dolist (file (cdr entry))
				  (insert " " file "\n")))
  (goto-char (point-min)))

(defun fj-visit-file ()
  "Visit file under the cursor."
  (interactive)  
    (if (save-excursion
          (beginning-of-line)
          (looking-at " "))
        (find-file (buffer-substring (1+ (line-beginning-position))
                                     (line-end-position)))
      (error "No file on this line.")))
  
(defun fj-visit-files ()
  "Visit all the files in the region."
  (interactive)
  ;region-beginning
  ;region-end
  
  )

(defun fj-file-in-excluded (file)
  "Test to see if FILE matches the exclusion regex."
  (catch 'excluded
	(dolist (exclusion fj-exclude-files)
			(when (string-match exclusion file)
				  (throw 'excluded 't)))))
(when nil
	  (fj-file-in-excluded "somefoo")
	  (fj-file-in-excluded "somefoo.muse"))



(defun fj-record-file ()
  "Record the file in the journal."
  (when (and buffer-file-name (not (fj-file-in-excluded buffer-file-name)))
    (let* ((date (format-time-string "%Y-%m-%d"))
           (entry (assoc-default date fj-journal)))
      (if entry
          (setq entry (remove (buffer-file-name) entry))

        (push (cons date nil) fj-journal)
        (if (> (length fj-journal) fj-journal-size)
            (setq fj-journal (nbutlast fj-journal (- (length fj-journal)
                                                     fj-journal-size)))))

      (push (buffer-file-name) entry)
      (setcdr (assoc date fj-journal) entry)
	  (when (get-buffer "*file-journal*")
			(save-excursion
			 (set-buffer (get-buffer "*file-journal*"))
			 (fj-update-fj-buffer))))))


(defadvice switch-to-buffer (after fj-switch-to-buffer activate)
  (fj-record-file))


(defun fj-save-journal ()
  "Save journal to file."
  (interactive)
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; Journal entries for visited files\n")
    (prin1 `(setq fj-journal ',fj-journal) (current-buffer))
    (insert ?\n)
    (write-region (point-min) (point-max) fj-journal-file nil
                  (unless (interactive-p) 'quiet))))

(defvar fj-save-journal-timer (run-with-timer fj-save-timer-interval fj-save-timer-interval 'fj-save-journal))

(add-hook 'kill-emacs-hook 'fj-save-journal)

(if (file-readable-p fj-journal-file)
    (load-file fj-journal-file))

;;* anything
(defvar fj--anything-candidates 'fj--anything-candidates)
(defun  fj--anything-candidates ()
  "returns a list of candidates for the anything package."
  (reduce 'append (mapcar 'cdr fj-journal)))

(defvar fj--anything-source
  '(((name . "File Journal")
	 (candidates . fj--anything-candidates)
	 (volatile)
	 (type . file))))

(defun fj--attach-with-anything ()
  (setq anything-sources (append anything-sources fj--anything-source)))

(add-hook 'emacs-startup-hook
		  (lambda ()
			(when (featurep 'anything) 
				  (fj--attach-with-anything))))

(provide 'file-journal)
;;; file-journal.el ends here
