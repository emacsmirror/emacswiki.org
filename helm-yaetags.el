;;; helm-yaetags.el --- Yet another etags interface with helm.

;; Copyright (C) 2009-2013  Taiki SUGAWARA

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: anything, helm, etags
;; Version: 1.2.0
;; Time-stamp: <27-Jul-2017 09:58:03>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/helm-yaetags.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Requires:
;; - helm.el
;; - GNU Emacs 24 or higher (maybe).

;;; Commentary:
;; Yet another etags interface with helm, like helm-etags.
;;
;; Try this package if you want to find only tag names instead of
;; tag declaration lines with helm and etags.

;;; Installation:
;; Load this package and add `helm-source-yaetags-select' to your favourite helm command.
;;
;; If you want to use this package as a replacement of `find-tag', put
;; the following:
;;
;;   (global-set-key (kbd "M-.") 'helm-yaetags-find-tag)

;;; Todo:
;; - merge to anything-etags?
;; - multi tag definition merging.

;;; History:
;; 2009-04-02  taiki
;;   * initial release.
;; 2013-11-29  Jeremy Moore
;;   * direct port from anything to helm
;; 2017-07-27  Jeremy Moore
;;   * replace obsolete candidates-in-buffer call
       
;;; Code:

(eval-when-compile
  (require 'cl))
(require 'etags)
(require 'helm)

(defvar helm-yaetags-candidates-buffer-name-prefix
  " *Helm-YaETags-Candidates*"
  "Name of `helm-yaetags-candidates-buffer' prefix")
(defvar helm-yaetags-candidates-buffer nil
  "Candidates buffer of tags.
This variable is made to local in TAGS buffer.
See. `helm-yaetags-visit-tags-table'")
(defvar helm-yaetags-tags-file-name "TAGS"
  "TAGS file name.")

;; tag object
(defstruct (helm-yaetags-tagobj
	    (:constructor helm-yaetags-tagobj-create)
	    (:conc-name helm-yaetags-tagobj->))
  tag tag-info file-label file-path goto-func)

(defvar helm-source-yaetags-select
  (helm-build-in-buffer-source "YaETags"
    :init 'helm-yaetags-init
    :action (helm-make-actions "Select Tag" 'helm-yaetags-select)))

(defun helm-yaetags-tags-file-valid-p (tags-file)
  "Return non-nil if TAGS-FILE is valid."
  (and tags-file
       (file-exists-p tags-file)
       (file-regular-p tags-file)))
  
(defun helm-yaetags-visit-tags-table-buffer (tags-file)
  "Visit tags buffer, but disable user prompting."
  (let ((tags-add-tables t)
	(tags-revert-without-query t)
	(large-file-warning-threshold nil))
    (visit-tags-table-buffer tags-file)))

(defun helm-yaetags-find-tags-file (&optional dir)
  "Find TAGS file from DIR upward to upper directories.
Return file path, when TAGS file is found."
  (setq dir (file-name-as-directory (or dir default-directory)))
  (let ((name helm-yaetags-tags-file-name))
    (cond
     ((string= dir (directory-file-name dir))
      nil)
     ((file-exists-p (expand-file-name name dir))
      (file-truename (expand-file-name name dir)))
     (t
      (helm-yaetags-find-tags-file (expand-file-name ".." dir))))))

;;; unified tags selection
(defun helm-yaetags-init ()
  "Initialize tag candidates buffer for `helm'."
  (let ((tags-file (helm-yaetags-find-tags-file)))
    (when (helm-yaetags-tags-file-valid-p tags-file)
      (let ((candidates-buffer (helm-yaetags-visit-tags-table tags-file)))
	(with-current-buffer (helm-candidate-buffer 'global)
	  (erase-buffer)
	  (insert-buffer-substring candidates-buffer))))))

(defun helm-yaetags-select (tag)
  "Select candidate TAG.
If TAG has multiple entries, ask tag of tags to user with `helm'.
Otherwise goto TAG's declaration."
  (let ((tags-file (helm-yaetags-find-tags-file))
	tagobj-list)
    (when (helm-yaetags-tags-file-valid-p tags-file)
      (setq tagobj-list (helm-yaetags-find-same-tags tags-file tag))
      (if (= (length tagobj-list) 1)
	  (helm-yaetags-goto-tag (car tagobj-list))
	(helm-yaetags-ask-tag-of-tags tagobj-list)))))

;;; tag of tags selection
(defun helm-yaetags-ask-tag-of-tags (tagobj-list)
  "Ask tag of tags with `helm'."
  (helm :sources (list (helm-yaetags-ask-tag-of-tags-source tagobj-list))))

(defun helm-yaetags-ask-tag-of-tags-source (tagobj-list)
  "Create asking tag of tags source."
  `((name . "Select Tag")
    (candidates . ,tagobj-list)
    (candidate-transformer
     (lambda (candidates)
       (mapcar (lambda (tagobj)
		 (cons (format "%s:\n  %s"
			       (helm-yaetags-tagobj->file-label tagobj)
			       (helm-yaetags-tagobj->tag tagobj))
		       tagobj))
	       candidates)))
    (multiline . t)
    (action
     ("Goto Tag" . helm-yaetags-goto-tag))))

;;; tag jump
(defun helm-yaetags-goto-tag (tagobj)
  "Goto TAGOBJ's declaration."
  (tag-find-file-of-tag (helm-yaetags-tagobj->file-path tagobj))
  (widen)
  (funcall
   (helm-yaetags-tagobj->goto-func tagobj)
   (helm-yaetags-tagobj->tag-info tagobj)))
  
;;; tags buffer manipulation
(defun helm-yaetags-visit-tags-table (tags-file &optional rebuild-p)
  "Open TAGS-FILE and prepare candidates like a `visit-tags-table'.
Return candidates buffer, if TAGS-FILE is valid."
  (interactive
   (let ((tags-file (helm-yaetags-find-tags-file)))
     (list (read-file-name "Find TAGS file: "
			   (file-name-directory tags-file) nil t
			   (file-name-nondirectory tags-file))
	   current-prefix-arg)))
  (when (helm-yaetags-tags-file-valid-p tags-file)
    (when rebuild-p
      (save-excursion
	(helm-yaetags-visit-tags-table-buffer tags-file)
	(kill-buffer (current-buffer))))
    (save-excursion
      (helm-yaetags-visit-tags-table-buffer tags-file)
      (unless (and (local-variable-p 'helm-yaetags-candidates-buffer)
		   (buffer-live-p helm-yaetags-candidates-buffer))
	(let ((buf (get-buffer-create
		    (concat helm-yaetags-candidates-buffer-name-prefix
			    tags-file)))
	      (candidates (helm-yaetags-make-candidates)))
	  (set (make-local-variable 'helm-yaetags-candidates-buffer) buf)
	  (with-current-buffer buf
	    (buffer-disable-undo)
	    (erase-buffer)
	    (dolist (x candidates)
	      (insert x "\n")))))
      helm-yaetags-candidates-buffer)))

(defun helm-yaetags-make-candidates ()
  "Make tag candidates from current TAGS buffer.
We don't use `etags-tags-completion-table', because this function is faster than `etags-tags-completion-table'."
  (save-excursion
    (let ((tab (make-hash-table :test 'equal :size 511)))
      (let ((reporter
	     (make-progress-reporter
	      (format "Making candidates for %s..." buffer-file-name)
	      (point-min) (point-max))))
	(goto-char (point-min))
	(while (re-search-forward "\^?\\(.+\\)\^a" nil t)
	  (puthash (match-string-no-properties 1) t tab)
	  (progress-reporter-update reporter (point)))
      (let ((msg (format "Sorting candidates for %s..." buffer-file-name))
	    list)
	(message "%s" msg)
	(maphash (lambda (key value) (push key list))
		 tab)
	(prog1
	    (sort list
		  (lambda (a b)
		    (let ((cmp (compare-strings a 0 nil b 0 nil t)))
		      (if (eq cmp t)
			  (string< a b)
			(< cmp 0)))))
	  (message "%sdone" msg)))))))

(defun helm-yaetags-find-same-tags (tags-file tag)
  "Find same TAG entries from TAGS-FILE."
  ;; some copy of `etags-tags-apropos'
  (save-excursion
    (helm-yaetags-visit-tags-table-buffer tags-file)
    (goto-char (point-min))
    (let ((case-fold-search nil)
	  tagobj-list)
      (while (search-forward (concat "\^?" tag "\^a") nil t)
	(beginning-of-line)
	(let* ((goto-func goto-tag-location-function)
	       (tag-info (save-excursion (funcall snarf-tag-function)))
	       (tag (if (eq t (car tag-info)) nil (car tag-info)))
	       (file-path (and tag (file-of-tag)))
	       (file-label (and tag (file-of-tag t))))
	  (when tag
	    (push
	     (helm-yaetags-tagobj-create
	      :tag tag :tag-info tag-info
	      :file-label file-label :file-path file-path
	      :goto-func goto-func)
	     tagobj-list)))
	(forward-line 1))
      (nreverse tagobj-list))))
      
;;; find-tag emulation
(defun helm-yaetags-find-tag (tag)
  "Find TAG's declaration with `helm'."
  (interactive
   (list (funcall (or find-tag-default-function
		      (get major-mode 'find-tag-default-function)
		      'find-tag-default))))
  (helm :sources '(helm-source-yaetags-select) :input tag :prompt nil :resume nil :preselect tag))
	    

(provide 'helm-yaetags)
;;; helm-yaetags.el ends here
