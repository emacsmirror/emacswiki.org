;;; wikiarea-fixed.el --- Interface to the Emacs Wiki's Elisp area

;; Copyright (C) 2002  Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: convenience

;; Modified by Sergio Mujica <sergio.mujica@terra.cl> Jan 09, 2004
;; Fixed call to http-get to explicitly request that the the
;; be closed, otherwise the busy-wait loops hang.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; see the file COPYING.  If not,
;; write to the Free Software Foundation, Inc., 59 Temple Place -
;; Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; Hey there.

;; The Emacs Wiki (http://www.emacswiki.org/) is just about the
;; coolest thing ever.

;; Here's some code to help you keep up with all of the Elisp in
;; the Emacs Wiki's Elisp area, which lives here:
;;		 http://www.emacswiki.org/elisp/

;; Basically, the idea is that you make a directory in which you
;; keep elisp files from the emacs wiki. This is
;; "~/elisp/from-the-emacs-wiki/" by default. You add this to your
;; `load-path'.

;; Assuming this file is also in your `load-path' somewhere, you
;; want to (require 'wikiarea) in your ~/.emacs or some such. Now,
;; M-x wikiarea RET should pull up a *WikiArea* buffer in which a
;; list of files should appear. Hitting RET on a new or updated
;; file will download it to your `wikiarea-managed-directory',
;; while hitting RET on an up-to-date file will visit it (if it
;; exists). C-u RET will always download a new copy of the file.

;; You might want to seed your `wikiarea-managed-directory' by
;; grabbing the Elisp area tarball from:
;;	      http://www.emacswiki.org/elisp.tar.gz

;; The code uses a cache of the last file info from the website
;; instead of looking at your files' modification times. It should
;; probably do the latter. It also relies on Alex's current HTML
;; formatting, as well as his most excellent `http-get.el', also
;; available in the wiki's lisp area. Thanks to Tom Pierce
;; <tom@pierceport.com> for the patch adding `http-get.el' support
;; & other enhancements.

;;; Code:

(require 'overlay)
(require 'pp)
(require 'http-get)

(defgroup wikiarea nil
  ;; Yay bad docstrings.
  "Group for customizing WikiArea."
  :tag "WikiArea"
  :link '(url-link "http://www.emacswiki.org/cgi-bin/wiki.pl?ElispArea")
  :prefix "wikiarea-"
  :group 'applications)

(defcustom wikiarea-managed-directory
  "~/elisp/from-the-emacs-wiki/"
  "*Directory in which wikiarea should keep elisp files."
  :type 'directory
  :group 'wikiarea)

(defcustom wikiarea-cached-file-list-file
  "~/.wikiarea.eld"
  "*File in which wikiarea keeps some cached data.")

;; These are the lists in which we store the data from Alex's HTML.
(defvar wikiarea--new-file-list '())
(defvar wikiarea--cached-file-list '())

;; A sample entry:

;; ("zenirc-chanbuf.el" 14
;;  (1 27 2002)
;;  (20 48))

;; The format is (FILENAME SIZE-IN-K DATE-LIST TIME-LIST)
;; DATE-LIST is (MONTH DAY YEAR), as in Calendar code.
;; TIME-LIST is (HOUR MINUTE).

(defun wikiarea--process-sentinel (process message)
  "Null -- don't do anything with killed process.  This is probably bad form."
  nil)

(defun wikiarea--get-new-file-list ()
  "Fetch a new file list from the Emacs Wiki."
  (let* ((proc (http-get "http://www.emacswiki.org/elisp/"
                         (list (list "Connection" "close")) 
			 'ignore 
			 1.1))
         (buf (process-buffer proc)))

    ;; Watch us spin and stop Emacs from doing anything else!
    (while (not (equal (process-status proc) 'closed))
      (when (not (accept-process-output proc 30))
        (delete-process proc)
        (error "Network timeout!")))

    (with-current-buffer buf
      ;; Get rid of the stuff before and after the list we're
      ;; interested in.
      (goto-char (point-min))
      (delete-region (point-min) (search-forward "<pre>"))
      (goto-char (point-max))
      (delete-region (point-max) (search-backward "</pre>"))

      ;; Strip the diff links.
      (goto-char (point-min))
      (while (re-search-forward "\\s-+(<a href=\".*\">diff</a>)" nil t)
        (replace-match ""))

      ;; Strip out extra whitespace.
      (goto-char (point-min))
      (while (re-search-forward "\\(\\s-\\)\\(\\s-+\\)" nil t)
        (replace-match "\\1"))

      ;; Strip out the remaining links, stringify the filenames,
      ;; and start each line with an open braket.
      (goto-char (point-min))
      (while (re-search-forward "<a href=\".*\">\\(.*\\)</a>" nil t)
        (replace-match "(\"\\1\""))

      ;; Toss a close paren onto the end of each line.
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]+\\)$" nil t)
        (replace-match "\\1)"))

      ;; Reformat the date into a Calendar-friendly list (MONTH DAY YEAR).
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]\\{4\\}\\)[-]\\([0-9]\\{2\\}\\)[-]\\([0-9]\\{2\\}\\)" nil t)
        (replace-match "(\\2 \\3 \\1)"))

      ;; Reformat the time into a list (HOUR MINUTE).
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]\\{2\\}\\)[:]\\([0-9]\\{2\\}\\)" nil t)
        (replace-match "(\\1 \\2)"))

      ;; Strip the "k" from the size. Here's hoping these files
      ;; stay small.
      (goto-char (point-min))
      (while (re-search-forward "\\(\\s-\\)\\([0-9]+\\)[k]\\(\\s-\\)" nil t)
        (replace-match "\\1 \\2 \\3"))

      ;; Toss parens around the whole thing. Now we have something
      ;; we can just (read). Yay.
      (goto-char (point-min))
      (insert "(")
      (goto-char (point-max))
      (insert ")")

      ;; Return this new value.
      (goto-char (point-min))
      (setq wikiarea--new-file-list (read buf)))
    (kill-buffer buf)
    wikiarea--new-file-list))

(defun wikiarea--get-cached-file-list ()
  "Load the cached file list from `wikiarea-cached-file-list-file'."
  (load wikiarea-cached-file-list-file t t))

(defun wikiarea--save-file-list ()
  "Save the file list into `wikiarea-cached-file-list-file'."
  (save-excursion
    (find-file wikiarea-cached-file-list-file)
    (delete-region (point-min) (point-max))
    (setq wikiarea--cached-file-list
          wikiarea--new-file-list)
    (pp (list 'setq 'wikiarea--cached-file-list
              (list 'quote wikiarea--cached-file-list))
        (current-buffer))
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun wikiarea-entry-newer-p (entry-1 entry-2)
  "Return T if ENTRY-1 is newer than ENTRY-2."
  (let* ((date-1 (nth 2 entry-1))
         (time-1 (nth 3 entry-1))

         (date-2 (nth 2 entry-2))
         (time-2 (nth 3 entry-2))

         (month-1 (nth 0 date-1))
         (day-1 (nth 1 date-1))
         (year-1 (nth 2 date-1))

         (month-2 (nth 0 date-2))
         (day-2 (nth 1 date-2))
         (year-2 (nth 2 date-2))

         (hour-1 (nth 0 time-1))
         (minute-1 (nth 1 time-1))

         (hour-2 (nth 0 time-2))
         (minute-2 (nth 1 time-2)))

    (cond ((> year-1 year-2) t)
          ((< year-1 year-2) nil)
          ((> month-1 month-2) t)
          ((< month-1 month-2) nil)
          ((> day-1 day-2) t)
          ((< day-1 day-2) nil)
          ((> hour-1 hour-2) t)
          ((< hour-1 hour-2) nil)
          ((> minute-1 minute-2) t)
          ((< minute-1 minute-2) nil)
          (t nil))))

(defun wikiarea-download-file (filename)
  "Fetch FILENAME from the Emacs Wiki's elisp area."
  (let* ((proc (http-get (concat "http://www.emacswiki.org/elisp/" filename)
                         (list (list "Connection" "close"))
			 'ignore
			 1.1))
         (buf (process-buffer proc))
         (retval nil))

    ;; Watch us spin and stop Emacs from doing anything else!
    (while (not (equal (process-status proc) 'closed))
      (when (not (accept-process-output proc 180))
        (delete-process proc)
        (error "Network timeout!")))

    (with-current-buffer buf
      (goto-char (point-min))
      (if (not (re-search-forward "404 Not Found" nil t))
          (progn
            (perform-replace "^\\(.*?\\)[
].*" "\\1" nil t nil)
            (goto-char (point-min))
            (forward-line 1)
            (while (looking-at "^.+[:].+$")
              (forward-line 1))
            (forward-line 1)
            (delete-region (point-min) (point))
            (set-visited-file-name (concat wikiarea-managed-directory filename))
            (save-buffer)
            (kill-buffer buf)
            (setq retval t))
        (error "Unable to fetch %s from the Emacs Wiki." filename)))
    retval))

(defun wikiarea-dwim (prefix)
  "Either download or visit the file at point.
With PREFIX, always download the file."
  (interactive "P")
  (let ((overlays (overlays-at (point))))
    (when (and overlays (listp overlays) (overlayp (car overlays)))
      (let* ((overlay (car overlays))
             (type (overlay-get overlay 'wikiarea-type))
             (filename (buffer-substring-no-properties
                        (overlay-start overlay)
                        (overlay-end overlay))))
        (cond ((or prefix (memq type '(updated new)))
               (when (wikiarea-download-file filename)
                 (overlay-put overlay 'face 'default)
                 (overlay-put overlay 'after-string nil)
                 (overlay-put overlay 'wikiarea-type 'up-to-date)))
              ((eq type 'up-to-date)
               (let ((file (concat wikiarea-managed-directory filename)))
                 (if (file-exists-p file)
                     (find-file file)
                   (wikiarea-download-file filename)
                   (find-file file))))
              (t nil))))))

(defvar wikiarea-mode-map (make-sparse-keymap)
  "Keymap for your *WikiArea* buffer.")

(define-key wikiarea-mode-map (kbd "RET") 'wikiarea-dwim)
(define-key wikiarea-mode-map (kbd "C-c C-l") 'wikiarea)

(defun wikiarea-mode ()
  "Major mode for downloading updated elisp files from the Emacs Wiki."
  (kill-all-local-variables)
  (setq major-mode 'wikiarea-mode)
  (setq mode-name "Wiki Area")
  (use-local-map wikiarea-mode-map)
  (cd wikiarea-managed-directory))

(defface wikiarea-new-file-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "red"))
    (t (:underline t)))
  "*Face used to fontify new elisp files."
  :group 'wikiarea)

(defface wikiarea-updated-file-face
  '((((class color) (background light)) (:foreground "green"))
    (((class color) (background dark)) (:foreground "green"))
    (t (:underline t)))
  "*Face used to fontify updated elisp files."
  :group 'wikiarea)

(defun wikiarea--make-overlay (beg end type entry)
  ;; Duh! Say something that isn't so obvious!
  "Make an overlay from BEG to END of TYPE."
  (let ((face (cond ((eq type 'new) 'wikiarea-new-file-face)
		    ((eq type 'updated) 'wikiarea-updated-file-face)
		    (t nil)))
	(after (cond ((eq type 'new) " (New file!)")
		     ((eq type 'updated)
		      (let* ((date (nth 2 entry))
			     (time (nth 3 entry))
			     (month (nth 0 date))
			     (day (nth 1 date))
			     (year (nth 2 date))
			     (hour (nth 0 time))
			     (minute (nth 1 time)))
		      (format " (Updated on %04d-%02d-%02d at %02d:%02d)"
			      year month day hour minute)))
		     (t nil)))
	(overlay (make-overlay beg end (current-buffer) nil nil)))

    (overlay-put overlay 'wikiarea-type type)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'before-string (format "%3dKb " (nth 1 entry)))

    (when face
      (overlay-put overlay 'face face))

    (when after
      (overlay-put overlay 'after-string after))

    overlay))

(defun wikiarea ()
  "Pop up or refresh an interface to the Emacs Wiki's Elisp area."
  (interactive)
  (wikiarea--get-new-file-list)
  (or wikiarea--cached-file-list (wikiarea--get-cached-file-list))

  (switch-to-buffer (get-buffer-create "*WikiArea*"))
  (delete-region (point-min) (point-max))
  (wikiarea-mode)

  (insert "Hello there!\n\n"
	  "Files that are listed "
	  (propertize "like so" 'face 'wikiarea-new-file-face)
	  " have been newly added to the Elisp area\n"
	  "on the wiki, whereas files that are listed "
	  (propertize "like so" 'face 'wikiarea-updated-file-face)
	  " have been updated\n"
	  "since you last downloaded them.\n\n"
	  "Hitting RET on a file should Do The Right Thing.\n\n")

  (let ((updated-entries '())
	(new-entries '())
	(up-to-date-entries '())
	(start (point)))

    (mapcar (lambda (new-entry)
	      (unless (string-match ".gz\\|~$" (car new-entry))
		(let ((start (point))
		      (cached-entry (assoc (car new-entry)
					   wikiarea--cached-file-list)))
		  (insert (car new-entry))
		  (if cached-entry
		      (cond
		       ((wikiarea-entry-newer-p new-entry cached-entry)
			(wikiarea--make-overlay start (point)
						'updated new-entry))
		       (t (wikiarea--make-overlay start (point)
						  'up-to-date new-entry)))
		    (wikiarea--make-overlay start (point) 'new new-entry))
		  (insert "\n"))))

	    wikiarea--new-file-list)
    (goto-char start)
    (wikiarea--save-file-list)))

(provide 'wikiarea)
;;; wikiarea-fixed.el ends here


