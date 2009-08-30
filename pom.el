;;; pom --- Utility functions for dealing with maven project.xml files

;; Copyright (C) 2005 Ole Arndt

;; Author: Ole Arndt <ole at sugarshark dot com>
;; Maintainer: Ole Arndt <ole at sugarshark dot com>
;; Keywords: development, java, jdee, maven
;; Time-stamp: Thu Nov 17 14:36:08 2005
;; Version: 0.5
;; X-URL: http://sugarshark.com/elisp/mylisp/pom.el

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package holds some functions to deal with maven POMs
;;
;; It lets you set up you JDE variables from a maven project.xml file.
;; It supports project file inheritance and property substitution from the
;; `project.properties' and `build.properties' files.
;;
;; My typical prj.el for the JDEE looks like this:
;;
;;   (require 'pom)
;;   (let ((pom (pom-read-pom)))
;;     (jde-project-file-version "1.0")
;;     (jde-set-variables
;;      '(jde-javadoc-gen-destination-directory "./target/docs/apidocs")
;;      '(jde-project-name (pom-get-project-id pom))
;;      '(jde-global-classpath (pom-get-classpath pom))))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup pom nil
  "Options controlling the maven POM parser
See `pom-read-pom' for more information."
  :group 'tools)

(defcustom pom-maven-local-repository "~/.maven/repository"
  "*Default maven repository directory."
  :type 'directory
  :group 'pom)

(defcustom pom-project-file-name "project.xml"
  "*Default name of a project file."
  :type 'string
  :group 'pom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; POM handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (fboundp 'string-split))
    (defun string-split (separators string)
      (split-string string separators)))

(defun pom-get-dependency-list (pom)
  "Get dependencies from a POM in the form of a list
of (groupId artifactId version jar type) quintuples."
  (let ((deps (pom-get-tag pom "dependencies")) (result ()))
    (dolist (a (and (listp deps) deps))
      (when a
	(let* ((id (pom-substitute-properties pom (cadr (assoc "id" a))))
	       (groupId (or (pom-substitute-properties pom (cadr (assoc "groupId" a)))
			    (and id (car (string-split ":" id)))))
	       (artifactId (or (pom-substitute-properties pom (cadr (assoc "artifactId" a)))
			       (and id (or (cadr (string-split ":" id)) id))))
	       (version (pom-substitute-properties pom (cadr (assoc "version" a))))
	       (type (or (pom-substitute-properties pom (cadr (assoc "type" a))) "jar"))
	       (jar (or (pom-substitute-properties pom (cadr (assoc "jar" a)))
			(concat artifactId "-" version "."
				(if (string= type "plugin") "jar" type)))))
	  (push (list groupId artifactId version jar type) result))))
    (nreverse result)))

(defun pom-get-tag (pom tag-name)
  "Get the contents of a tag. Returns either a string or a list of children."
  (let ((pp pom)
	(name (if (symbolp tag-name)
		  (symbol-name tag-name)
		tag-name))
	(tag nil)
	(res nil))
    (while (car pp)
      (set 'tag (assoc name (car pp)))
      (if (and tag (nth 1 tag))
	  (if (or (not res) (stringp (car res)))
	      (setq res (cdr tag))
	    (if (not (stringp (nth 1 tag)))
		(setq res (append res (cdr tag))))))
      (set 'pp (cdr pp)))
    (if (stringp (car res))
	(pom-substitute-properties pom (car res)) res)))

(defun pom-substitute-properties (pom string)
  "Substitute all occurances of \\${pom.\\(name\\)} with the result of
`(pom-get-tag pom (quote name))'"
  (when string
    (replace-regexp-in-string "\\${\\(pom\\.\\)*\\([^}]+\\)}"
			      (lambda (str)
				(let ((prop (match-string 2 str)))
				  (or (pom-get-tag pom prop) prop))) string)))

(defun pom-find-pom (&optional pom-file)
  "Find the next project.xml file. Search up the directory tree."
  (interactive)
  (let ((pom (expand-file-name (or pom-file pom-project-file-name))))
    (while (not (file-exists-p pom))
      (if (string= (file-name-directory pom) "/")
	  (error "%s does not exist" (file-name-nondirectory pom))
	(setq pom (expand-file-name (concat "../" (file-name-nondirectory pom))
				    (file-name-directory pom)))))
    pom))
	  
(defun pom-read-pom (&optional file)
  "Read a project.xml file  and its parents into a list of lisp structures.
The properties defined in the project.properties and build.properties
files will be parsed as well.
Properities referenced with a ${property} syntax will be substituted in
both the POM and the propertiy files. This also works for POM values.
Example:
     With ${pom.version} you get the value of the version tag.
     With ${pom.dependencies} you get a sublist of the POM representation.
"
  (let* ((pom-file (expand-file-name (or file (pom-find-pom))))
	 (pom-dir (expand-file-name (file-name-directory pom-file)))
	 (builtin-props `(("properties" ("basedir" ,pom-dir))))
	 (build-props (pom-read-properties
		      (expand-file-name "build.properties" pom-dir)))
	 (project-props (pom-read-properties
			(expand-file-name "project.properties" pom-dir)))
	 (pom (append builtin-props project-props build-props (pom-parse-pom pom-file)))
	 (extend (pom-get-tag pom "extend")))
    (append (and (stringp extend) (pom-read-pom extend)) pom)))


(defun pom-get-project-id (&optional pom)
  "Get the project id. If the optional `pom' parameter is nil,
the project.xml file will be read and parsed."
  (pom-get-tag (or pom (pom-read-pom)) "id"))

(defun pom-get-dependencies (&optional pom repository)
  "Get a list of artifacts this project depends on."
  (interactive)
  (let ((pp (or pom (pom-read-pom)))
	(repo (expand-file-name (or repository pom-maven-local-repository))))
    (mapcar (lambda (a) (concat repo "/" (nth 0 a) "/" (nth 4 a) "s/" (nth 3 a)))
	    (pom-get-dependency-list pp))))

(defun pom-get-dependent-jars (&optional pom repository)
  "Get a list of jars this project depends on."
  (interactive)
  (let ((pp (or pom (pom-read-pom)))
	(repo (expand-file-name (or repository pom-maven-local-repository)))
	(result ()))
    (dolist (a (pom-get-dependency-list pp))
      (if (string= (nth 4 a) "jar")
	  (push (concat repo "/" (nth 0 a) "/" (nth 4 a) "s/" (nth 3 a)) result)))
    (nreverse result)))

(defun pom-get-classpath (&optional pom repository)
  "Get the classpath for the project."
  (let* ((pp (or pom (pom-read-pom)))
	 (basedir (pom-get-tag pp "basedir")))
    (append (list (expand-file-name "target/classes" basedir)
		  (expand-file-name "target/test-classes" basedir))
	    (pom-get-dependent-jars pp repository))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading and parsing property files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pom-read-properties (file)
  "Read java property files."
  (let ((keep))
    (if (get-file-buffer file)
	(progn
	  (set-buffer (get-file-buffer file))
	  (setq keep (point)))
      (let (auto-mode-alist magic-mode-alist find-file-hook)
	(set-buffer (find-file-noselect file))))
    (let ((props (pom-parse-properties (point-min) (point-max))))
      (if keep
	  (goto-char keep)
	(kill-buffer (current-buffer)))
      props)))

(defun pom-parse-properties (beg end)
  "Parse java properties in region"
  (let ((props nil))
    (save-excursion
      (goto-char beg)
      (while (search-forward-regexp
	     "^\\s *\\([0-9A-Za-z_\\-\\.]+\\)\\s *=\\s *\\(.*\\)\\s *$" end t)
	(push (list (match-string 1) (match-string 2)) props)))
    (and props (list (append (list "properties") (nreverse props))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple xml parser for the POM.
;;  Taken and simplified from an older verison of xml.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pom-parse-pom (file)
  "Parse the well-formed POM FILE.
If FILE is already edited, this will keep the buffer alive.
Returns the top node with all its children."
  (let ((keep))
    (if (get-file-buffer file)
	(progn
	  (set-buffer (get-file-buffer file))
	  (setq keep (point)))
      (let (auto-mode-alist magic-mode-alist find-file-hook)
	(set-buffer (find-file-noselect file))))
    (let ((pom (pom-parse-region (point-min) (point-max))))
      (if keep
	  (goto-char keep)
	(kill-buffer (current-buffer)))
      pom)))

(defun pom-parse-region (beg end)
  "Parse the region from BEG to END in BUFFER."
  (let (pom result)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
	(if (search-forward "<" end t)
	    (progn
	      (forward-char -1)
	      (if (null pom)
		  (progn
		    (set 'result (pom-parse-tag end))
		    (cond
		     ((listp (car result))
		      (add-to-list 'pom (cdr result)))
		     (t
		      (add-to-list 'pom result))))
		(error "POM files can have only one toplevel tag.")))
	  (goto-char end)))
      (reverse pom))))


(defun pom-parse-tag (end)
  "Parse the tag that is just in front of point.
The end tag must be found before the position END in the current buffer.
Returns one of:
   - a list : the matching node
   - nil    : the point is not looking at a tag."
  (cond
   ;; Processing instructions (like the <?xml version="1.0"?> tag at the
   ;; beginning of a document)
   ((looking-at "<\\?")
    (search-forward "?>" end)
    (skip-chars-forward " \t\n")
    (pom-parse-tag end))
   ;;  Character data (CDATA) sections, in which no tag should be interpreted
   ((looking-at "<!\\[CDATA\\[")
    (let ((pos (match-end 0)))
      (unless (search-forward "]]>" end t)
	(error "CDATA section does not end anywhere in the document"))
      (buffer-substring-no-properties pos (match-beginning 0))))
   ;;  skip DTD for the document
   ((looking-at "<!DOCTYPE")
    (search-forward ">" end)
    (skip-chars-forward " \t\n")
    (pom-parse-tag end))
   ;;  skip comments
   ((looking-at "<!--")
    (search-forward "-->" end)
    (skip-chars-forward " \t\n")
    (pom-parse-tag end))
   ;;  end tag
   ((looking-at "</")
    '())
   ;;  opening tag
   ((looking-at "<\\([^/> \t\n]+\\)")
    (let* ((node-name (match-string 1))
	   (children (list node-name))
	   (case-fold-search nil) ;; XML is case-sensitive
	   pos)
      (goto-char (match-end 1))

      ;; ignore the attribute list; not needed for POMs

      ;; is this an empty element ?
      (if (looking-at "[ \t\n]*/>")
	  (progn
	    (skip-chars-forward " \t\n")
	    (forward-char 2)
	    (skip-chars-forward " \t\n")
	    (append children '("")))

	;; is this a valid start tag ?
	(if (= (char-after) ?>)
	    (progn
	      (forward-char 1)
	      (skip-chars-forward " \t\n")
	      ;;  Now check that we have the right end-tag. Note that this one
	      ;; might contain spaces after the tag name
	      (while (not (looking-at (concat "</" node-name "[ \t\n]*>")))
		(cond
		 ((looking-at "</")
		  (error (concat
			  "POM: invalid syntax -- invalid end tag (expecting "
			  node-name
			  ") at pos " (number-to-string (point)))))
		 ((= (char-after) ?<)
		  (set 'children (append children (list (pom-parse-tag end)))))
		 (t
		  (set 'pos (point))
		  (search-forward "<" end)
		  (forward-char -1)
		  (let ((string (buffer-substring-no-properties pos (point)))
			(pos 0))
		    (set 'children (append children
					   (list string)))))))
	      (goto-char (match-end 0))
	      (skip-chars-forward " \t\n")
	      (if (> (point) end)
		  (error "POM: End tag for %s not found before end of region."
			 node-name))
	      children
	      )

	  ;;  This was an invalid start tag
	  (error "POM: Invalid attribute list")))))))

(provide 'pom)

;;; EOF
