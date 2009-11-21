;;; tag.el -- tagging files

;; Copyright 2004  Alex Schroeder

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Presently you can use this to tag MP3 files using mp3info.
;; Use M-x tag-directory to get started.
;; Hit C-c C-c to save an entry.

;;; Bugs

;; Currently the old tag data is stored as alist, and the new tag data
;; is stored as an alist.  Should be refactored.
;;
;; Add defcustoms.
;;
;; Read and write tag info ourselves?

(defvar tag-info 'tag-run-mp3info-info)
(defvar tag-update 'tag-run-mp3info-update)

(define-derived-mode tag-mode fundamental-mode "Tag"
  "Tag files in directories.")

(define-key tag-mode-map (kbd "C-c C-c") 'tag-update-entry)

(defun tag-directory (dir)
  "Create a new tag edit buffer for DIR."
  (interactive "DDirectory: ")
  (set-buffer (get-buffer-create (format "Tag %s" (directory-file-name dir))))
  (switch-to-buffer (current-buffer))
  (message "Reading info...")
  (when (eq major-mode 'tag-mode)
    (error "Already tagging %s" dir))
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; From wdired -- I temp disable undo for performance: since I'm
    ;; going to clear the undo list, it can save more than a 9% of time
    ;; with big directories because setting properties modify the
    ;; undo-list.
    (buffer-disable-undo)
    (dolist (file (mapcar (lambda (file) 
			    (expand-file-name file dir))
			  (directory-files dir)))
      (if (not (file-readable-p file))
	  (error " is not readable.\n\n")
	(when (file-regular-p file)
	  (tag-info-insert (funcall tag-info file))
	  (newline))))
    (goto-char (point-min))
    (message "Reading info...done")
    (buffer-enable-undo); Performance hack. See above.
    (tag-mode)))

(defun tag-info-insert (entry)
  "Insert the ENTRY as returned by `tag-info'."
  (let ((start (point)))
    (tag-readonly "Artist:   ") (insert (tag-get-artist entry)) (tag-line 'artist)
    (tag-readonly "Track:    ") (insert (tag-get-track entry))  (tag-line 'track)
    (tag-readonly "Album:    ") (insert (tag-get-album entry))  (tag-line 'album)
    (tag-readonly "Track#:   ") (insert (tag-get-number entry)) (tag-line 'number)
    (tag-readonly "Year:     ") (insert (tag-get-year entry))   (tag-line 'year)
    (tag-readonly "Genre:    ") (insert (tag-get-genre entry))  (tag-line 'genre)
    (tag-readonly "Filename: ")
    (insert (file-name-nondirectory (tag-get-file entry)))      (tag-line 'file)
    (add-text-properties start (point) (list 'tag entry))))

(defun tag-line (prop)
  "Tag the current line with PROP and insert a newline."
  (put-text-property (line-beginning-position) (point) 'tag-property prop)
  (newline))

(defun tag-readonly (str)
  "Insert STR as read-only."
  (let ((start (point)))
    (insert str)
    (add-text-properties start (point) '(read-only t front-sticky t rear-nonsticky t))))

;; (tag-run-mp3info (expand-file-name "~/mutella/BachataHits_2003/01 - Monchy & Alexandra - BachataHits 2003 - Dos Locos.mp3"))

(defun tag-run-mp3info-info (file)
  "Return an alist with the music attributes for FILE by running mp3info.
Use this defun as a value for `tag-info'."
  (cons file
	(split-string
	 (with-output-to-string
	   (with-current-buffer
	       standard-output
	     (call-process "mp3info" nil t nil "-p" "%a\\n%t\\n%l\\n%n\\n%y\\n%g"
			   file)))
	 "\n")))

(defun tag-get-file   (item) (tag-get-string (nth 0 item)))
(defun tag-get-artist (item) (tag-get-string (nth 1 item)))
(defun tag-get-track  (item) (tag-get-string (nth 2 item)))
(defun tag-get-album  (item) (tag-get-string (nth 3 item)))
(defun tag-get-number (item) (tag-get-string (nth 4 item)))
(defun tag-get-year   (item) (tag-get-string (nth 5 item)))
(defun tag-get-genre  (item) (tag-get-string (nth 6 item)))

(defun tag-get-string (val)
  "Return VAL, or the empty string if VAL is nil."
  (or val ""))

(defun tag-update-entry ()
  "Update the entry with the current data."
  (interactive)
  (message "Updating...")
  (funcall tag-update (tag-data-at-point))
  (message "Updating...done"))

(defun tag-update-all ()
  "Update all entries in the buffer."
  (interactive) 
  (when (not (buffer-modified-p))
    (error "No files need updating"))
  (message "Updating...")
  (goto-char (point-min))
  (while (= 0 (forward-paragraph 1))
    (let ((data (tag-data-at-point)))
      (message "Updating %s" (cdr (assq 'file data)))
      (funcall tag-update data)))
  (set-buffer-modified-p nil)
  (message "Updating...done"))

(defun tag-data-at-point ()
  "Determine the existing data at point."
  (save-excursion
    (let ((lines (split-string (buffer-substring
				(progn (forward-paragraph -1)
				       (point))
				(progn (forward-paragraph 1)
				       (point)))
			       "\n"))
	  result oldfile)
      (dolist (line lines)
	(when (and (not oldfile)
		   (get-text-property 0 'tag line))
	  (setq oldfile (tag-get-file (get-text-property 0 'tag line))
		result (cons (cons 'oldfile oldfile) result)))
	(let ((key (get-text-property 0 'tag-property line))
	      (pos (next-single-property-change 0 'read-only line)))
	  (when pos
	    (setq result (cons (cons key (substring line pos)) result)))))
      result)))

(defun tag-run-mp3info-update (entry)
  "Update ENTRY by running mp3info.
Use this defun as a value for `tag-update'."
  (with-current-buffer (get-buffer-create "*mp3info")
    (goto-char (point-max))
    (let (args)
      (dolist (item '((artist . "-a")
		      (track . "-t")
		      (album . "-l")
		      (number . "-n")
		      (year . "-y")
		      (genre . "-g")))
	(let ((sym (car item))
	      (option (cdr item)))
	  (when (assq sym entry)
	    (setq args (nconc (list option (cdr (assq sym entry))) args)))))
      (setq args (nconc args (list (cdr (assq 'oldfile entry)))))
      (insert "mp3info " (mapconcat 'identity args " ")) (newline)
      (apply 'call-process "mp3info" nil '(nil t) nil args))))

(provide 'tag)
;;; tag.el ends here
