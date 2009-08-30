;;; anything-yaetags.el --- Yet another etags interface with anything.

;; Copyright (C) 2009  Taiki SUGAWARA

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: anything, etags
;; Version: 1.0.1
;; Time-stamp: <2009-04-02 19:22:24 UTC taiki>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/anything-yaetags.el

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
;; - anything.el
;; - GNU Emacs 22 or higher (meybe).

;;; Commentary:
;; Yet another etags interface with anything like a anything-gtags.
;;
;; Try use this package, if you want to find only tag names instead of
;; tag declaration lines with anything and etags.

;;; Installation:
;; Load this package and add `anything-c-source-yaetags-select' to
;; `anything-sources' like followings:
;;
;;   (require 'anything-yaetags)
;;   (add-to-list 'anything-sources 'anything-c-source-yaetags-select)
;;
;; If you want to use this package for replacement of `find-tag', put
;; followings:
;;
;;   (global-set-key (kbd "M-.") 'anything-yaetags-find-tag)

;;; Todo:
;; - merge to anything-etags?
;; - multi tag definition merging.

;;; History:
;; 2009-04-02  taiki
;;   * initial release.
       
;;; Code:

(eval-when-compile
  (require 'cl))
(require 'etags)
(require 'anything)

(defvar anything-yaetags-candidates-buffer-name-prefix
  " *Anything-YaETags-Candidates*"
  "Name of `anything-yaetags-candidates-buffer' prefix")
(defvar anything-yaetags-candidates-buffer nil
  "Candidates buffer of tags.
This variable is made to local in TAGS buffer.
See. `anything-yaetags-visit-tags-table'")
(defvar anything-yaetags-tags-file-name "TAGS"
  "TAGS file name.")

;; tag object
(defstruct (anything-yaetags-tagobj
	    (:constructor anything-yaetags-tagobj-create)
	    (:conc-name anything-yaetags-tagobj->))
  tag tag-info file-label file-path goto-func)

(defvar anything-c-source-yaetags-select
  '((name . "YaETags")
    (init . (anything-yaetags-init))
    (candidates-in-buffer)
    (action
     ("Select Tag" . anything-yaetags-select))))

(defun anything-yaetags-tags-file-valid-p (tags-file)
  "Return non-nil if TAGS-FILE is valid."
  (and tags-file
       (file-exists-p tags-file)
       (file-regular-p tags-file)))
  
(defun anything-yaetags-visit-tags-table-buffer (tags-file)
  "Visit tags buffer, but disable user prompting."
  (let ((tags-add-tables t)
	(tags-revert-without-query t)
	(large-file-warning-threshold nil))
    (visit-tags-table-buffer tags-file)))

(defun anything-yaetags-find-tags-file (&optional dir)
  "Find TAGS file from DIR upward to upper directories.
Return file path, when TAGS file is found."
  (setq dir (file-name-as-directory (or dir default-directory)))
  (let ((name anything-yaetags-tags-file-name))
    (cond
     ((string= dir (directory-file-name dir))
      nil)
     ((file-exists-p (expand-file-name name dir))
      (file-truename (expand-file-name name dir)))
     (t
      (anything-yaetags-find-tags-file (expand-file-name ".." dir))))))

;;; unified tags selection
(defun anything-yaetags-init ()
  "Initialize tag candidates buffer for `anything'."
  (let ((tags-file (anything-yaetags-find-tags-file)))
    (when (anything-yaetags-tags-file-valid-p tags-file)
      (let ((candidates-buffer (anything-yaetags-visit-tags-table tags-file)))
	(with-current-buffer (anything-candidate-buffer 'global)
	  (erase-buffer)
	  (insert-buffer-substring candidates-buffer))))))

(defun anything-yaetags-select (tag)
  "Select candidate TAG.
If TAG has multiple entries, ask tag of tags to user with `anything'.
Otherwise goto TAG's declaration."
  (let ((tags-file (anything-yaetags-find-tags-file))
	tagobj-list)
    (when (anything-yaetags-tags-file-valid-p tags-file)
      (setq tagobj-list (anything-yaetags-find-same-tags tags-file tag))
      (if (= (length tagobj-list) 1)
	  (anything-yaetags-goto-tag (car tagobj-list))
	(anything-yaetags-ask-tag-of-tags tagobj-list)))))

;;; tag of tags selection
(defun anything-yaetags-ask-tag-of-tags (tagobj-list)
  "Ask tag of tags with `anything'."
  (anything (list (anything-yaetags-ask-tag-of-tags-source tagobj-list))))

(defun anything-yaetags-ask-tag-of-tags-source (tagobj-list)
  "Create asking tag of tags source."
  `((name . "Select Tag")
    (candidates . ,tagobj-list)
    (candidate-transformer
     (lambda (candidates)
       (mapcar (lambda (tagobj)
		 (cons (format "%s:\n  %s"
			       (anything-yaetags-tagobj->file-label tagobj)
			       (anything-yaetags-tagobj->tag tagobj))
		       tagobj))
	       candidates)))
    (multiline . t)
    (action
     ("Goto Tag" . anything-yaetags-goto-tag))))

;;; tag jump
(defun anything-yaetags-goto-tag (tagobj)
  "Goto TAGOBJ's declaration."
  (tag-find-file-of-tag (anything-yaetags-tagobj->file-path tagobj))
  (widen)
  (funcall
   (anything-yaetags-tagobj->goto-func tagobj)
   (anything-yaetags-tagobj->tag-info tagobj)))
  
;;; tags buffer manipulation
(defun anything-yaetags-visit-tags-table (tags-file &optional rebuild-p)
  "Open TAGS-FILE and prepare candidates like a `visit-tags-table'.
Return candidates buffer, if TAGS-FILE is valid."
  (interactive
   (let ((tags-file (anything-yaetags-find-tags-file)))
     (list (read-file-name "Find TAGS file: "
			   (file-name-directory tags-file) nil t
			   (file-name-nondirectory tags-file))
	   current-prefix-arg)))
  (when (anything-yaetags-tags-file-valid-p tags-file)
    (when rebuild-p
      (save-excursion
	(anything-yaetags-visit-tags-table-buffer tags-file)
	(kill-buffer (current-buffer))))
    (save-excursion
      (anything-yaetags-visit-tags-table-buffer tags-file)
      (unless (and (local-variable-p 'anything-yaetags-candidates-buffer)
		   (buffer-live-p anything-yaetags-candidates-buffer))
	(let ((buf (get-buffer-create
		    (concat anything-yaetags-candidates-buffer-name-prefix
			    tags-file)))
	      (candidates (anything-yaetags-make-candidates)))
	  (set (make-local-variable 'anything-yaetags-candidates-buffer) buf)
	  (with-current-buffer buf
	    (buffer-disable-undo)
	    (erase-buffer)
	    (dolist (x candidates)
	      (insert x "\n")))))
      anything-yaetags-candidates-buffer)))

(defun anything-yaetags-make-candidates ()
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

(defun anything-yaetags-find-same-tags (tags-file tag)
  "Find same TAG entries from TAGS-FILE."
  ;; some copy of `etags-tags-apropos'
  (save-excursion
    (anything-yaetags-visit-tags-table-buffer tags-file)
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
	     (anything-yaetags-tagobj-create
	      :tag tag :tag-info tag-info
	      :file-label file-label :file-path file-path
	      :goto-func goto-func)
	     tagobj-list)))
	(forward-line 1))
      (nreverse tagobj-list))))
      
;;; find-tag emulation
(defun anything-yaetags-find-tag (tag)
  "Find TAG's declaration with `anything'."
  (interactive
   (list (funcall (or find-tag-default-function
		      (get major-mode 'find-tag-default-function)
		      'find-tag-default))))
  (anything '(anything-c-source-yaetags-select) tag nil nil tag))
	    

(provide 'anything-yaetags)
;;; anything-yaetags.el ends here
