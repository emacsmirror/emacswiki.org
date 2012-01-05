;;; find-file-in-tags.el --- directly open files referenced in TAGS files
;;;
;;; Copyright (C) 2010 Trey Jackson
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to bigfaceworm at gmail dot com
;;
;; Installation:
;; Add a binding to the main function, then use it:
;;   (global-set-key (read-kbd-macro "C-,") 'find-file-in-tags)
;;
;; When you call 'find-file-in-tags, you'll be prompted for a file name.
;; If more than one file matches what you've typed, you're offered to
;; refine your choice - this time using the full paths to the files.
;;
;; This routine also takes into account multiple work areas for the same project.
;; Let's say you have a project which has the files
;;      myproject/file.c
;;      myproject/file.h
;;
;; And you've checked out two work areas
;; ~/work/monday_version/TAGS
;; ~/work/monday_version/file.c
;; ~/work/monday_version/file.h
;; ~/work/monday_version/subdir/helper.h
;;
;; ~/work/thursday_version/TAGS
;; ~/work/thursday_version/file.c
;; ~/work/thursday_version/file.h
;; ~/work/thursday_version/subdir/helper.h
;;
;; When you invoke 'find-file-in-tags in the buffer file.c (in monday_version)
;; the file you open up will be found in ~/work/monday_version, e.g.
;;     M-x find-file-in-tags helper.h RET
;; will open up ~/work/monday_version/subdir/helper.h
;; 
;; If you you were visiting file.c in thursday_version, the helper.h opened
;; would be the one in thursday_version
;;
;; And this just uses a single TAGS file.
;;
;; Limitations: the code checks to ensure that TAGS files exist in both locations.
;; AND
;; It currently assumes that the work areas are all rooted in the same directory.
;; i.e. work areas in the following directories would function properly 
;;    ~/work1 ~/work2 ~/some_other_work_directory_name
;; these would fail
;;    ~/work1 ~/subdir/work2 /tmp/work3
;;
;;
;; TODO: make 'find-tag adjust for work areas similarly

(require 'etags)

(defvar ffit-version "1.0")

;; XEmacs doesn't have this function, provide it
(if (not (fboundp 'tags-table-files))
    (defun tags-table-files ()
      (tag-table-files tags-file-name)))

(defun ffit-get-possible-view-extension ()
  (let* ((b-name (buffer-file-name))
         (matched (and b-name (string-match "/view/\\([^/]*\\)" b-name))))
    (if matched (concat "/view/" (match-string 1 b-name)) nil)))

;;;###autoload
(defun ffit-determine-dir-for-current-file ()
  "Return a directory to use as the base directory for a TAGS file, or nil if it couldn't be determined.
Basically, look at the path to the tags file (one level above TAGS, see if it matches that of the current file,
and if so, then use that directory.  Added the additional constraint that there must be a TAGS file in the
directory returned, i.e. if the directory calculated does not have a TAGS file, do not return it as a possibility.

This is to help minimize the number of TAGS files loaded by Emacs, b/c in general you're working on one software project
and the TAGS files for each of the sandboxes are about the same.  So just use one TAGS file, but find the files in
the appropriate sandbox."
  (when default-directory
    (let ((default-directory (file-truename default-directory))
          (two-up (file-name-directory (directory-file-name (file-name-directory tags-file-name))))
          (tags-name (file-name-nondirectory tags-file-name))
          path-to-other-tags)
      (when (and (string-match (concat "^\\(" two-up "[^/]+/\\)") default-directory)
                 (file-exists-p (concat (setq path-to-other-tags (match-string 1 default-directory)) tags-name)))
        path-to-other-tags))))

;;;###autoload
(defun find-file-in-tags (file &optional pre)
  "find file, but completion just works on files found in TAGS
unless a prefix argument is given, only allows one file to be specified
with prefix argument, all files matching what was typed will be loaded."
  (interactive (list (completing-read "find file (from TAGS): "
                                      'ffit-complete-tags-table-file
                                      nil t nil)
                     current-prefix-arg))
  (save-excursion
    (let ((origin-buffer-view (ffit-get-possible-view-extension))
          (possible-tags-file-prefix (ffit-determine-dir-for-current-file)))
      (let ((enable-recursive-minibuffers t))
        (visit-tags-table-buffer))
      (let ((files (tags-table-files))
            (file-re (concat "/" file "[^/]*$"))
            matches)
        (setq matches
              (mapcar (lambda (f)
                        (let ((truename (expand-file-name f
                                                          (or possible-tags-file-prefix
                                                              (file-truename default-directory)))))
                          (when origin-buffer-view
                            (if (string-match "^/view/[^/]*" truename)
                                (setq truename (replace-match origin-buffer-view nil nil truename)))
                            (if (string-match "^/vobs" truename)
                                (setq truename (concat origin-buffer-view truename))))
                            truename))
                      (remove-if-not (lambda (f) (string-match file-re f)) files)))
        (if (or pre (= (length matches) 1))
            (mapcar 'ffit-find-file matches)
          (let ((visible-bell t)
                (new-list (mapcar (lambda (x)
                                    (let ((f (file-name-nondirectory x)))
                                      (cons f x)))
                                  matches))
                match)
            (ding)
            (let ((new-matches (mapcar (lambda (x) (cons x x)) matches)))
              (setq match (completing-read "Restrict more--> find file (from TAGS): "
                                           new-matches
                                           nil 'non-t (try-completion "" new-matches)))
              (ffit-find-file (cdr (assoc match new-matches))))))))))

(defun ffit-find-file (file)
  "Find call find-file, but prompting for check out first if file doesn't exist"
  (if (and (not (file-exists-p file))
           (y-or-n-p (format "File %s doesn't exist, check it out? " (file-name-nondirectory file))))
      (let ((default-directory (file-name-directory file)))
        (shell-command (format "cvs update %s" (file-name-nondirectory file)))))
  (find-file file))

(defun ffit-complete-tags-table-file (string predicate what)
  (save-excursion
    ;; If we need to ask for the tag table, allow that.
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (if (eq what t)
        (all-completions string (mapcar 'list (mapcar 'file-name-nondirectory (tags-table-files)))
                         predicate)
      (try-completion string (mapcar 'list (mapcar 'file-name-nondirectory (tags-table-files)))
                      predicate))))

(defun find-function-prototype (arg)
  "An attempt to provide the function prototype for the function under the point."
  (interactive "P")

  (let ((str (save-window-excursion
    (find-tag (funcall (or find-tag-default-function
                           (get major-mode 'find-tag-default-function)
                           'find-tag-default)))
    (search-forward "(")
    (forward-char -1)
    (let ((b (point))
          e)
      (forward-sexp)
      (setq e (point))
      (message (buffer-substring b e))
      (buffer-substring b e)))))
    (if arg
        (insert str))))

(provide 'find-file-in-tags)
