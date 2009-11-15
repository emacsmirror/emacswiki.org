;;; md-readme.el --- Markdown-formatted READMEs for your ELisp

;; Author: Thomas Kappler <tkappler@gmail.com>
;; Created: 2009 November 07
;; Keywords: readme, markdown, header, documentation, github
;; URL: <http://github.com/thomas11/md-readme/tree/master>

;; Copyright (C) 2009 Thomas Kappler

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The git-based source code hosting site <http://github.com> has
;; lately become popular for Emacs Lisp projects. Github has a feature
;; that displays files named "README[.suffix]" automatically on a
;; project's main page. If these files are formatted in Markdown, the
;; formatting is interpreted. See
;; <http://github.com/guides/readme-formatting> for more information.

;; Emacs Lisp files customarily have a header in a fairly standardized
;; format. md-readme extracts this header, re-formats it to Markdown,
;; and writes it to the file "README.md" in the same directory. If you
;; put your code on github, you could have this run automatically, for
;; instance upon saving the file or from a git pre-commit hook, so you
;; always have an up-to-date README on github.

;; It recognizes headings, the GPL license disclaimer which is
;; replaced by a shorter notice linking to the GNU project's license
;; website, lists, and normal paragraphs. Lists are somewhat tricky to
;; recognize automatically, and the program employs a very simple
;; heuristic currently.

;;; Dependencies:
;; None.

;;; Installation:
;; (require 'md-readme), then you can call mdr-generate manually. I
;; have not found a way to call it automatically that I really like,
;; but here is one that works for me:

;;     (require 'md-readme)
;;     (dir-locals-set-class-variables
;;      'generate-README-with-md-readme
;;      '((emacs-lisp-mode . ((mdr-generate-readme . t)))))
;;     (dolist (dir '("~/Projects/wpmail/" "~/Projects/md-readme/"))
;;       (dir-locals-set-directory-class
;;        dir 'generate-README-with-md-readme))
;;     (add-hook 'after-save-hook 
;;               '(lambda () (if (boundp 'mdr-generate-readme) (mdr-generate))))

;;; History:
;; 2009-11:    First release.

;;; Code:
(defun mdr-generate ()
  "Generate README.md from the header of the current file."
  (interactive)
  (let ((header (mdr-extract-header)))
    (with-temp-file "README.md"
      (insert header)
      (mdr-convert-header))))

(defun mdr-convert-header ()
  "Convert the header to Markdown.
This function transforms the header in-place, so be sure to
extract the header first with mdr-extract-header and call it on
the copy."
  (goto-char (point-min))
  (mdr-find-and-replace-disclaimer)
  (while (< (line-number-at-pos) (line-number-at-pos (point-max)))
    (when (looking-at-p ";;")
      (delete-char 2)
      (cond ((looking-at-p ";")  ; heading
	     (delete-char 1)
	     (if (looking-at-p " Code:?")
		 (delete-region (point) (line-end-position))
	       (insert "#")
	       (progn
		 (end-of-line)
		 (backward-char)
		 (when (looking-at-p ":")
		   (delete-char 1)))))
	    ((mdr-looking-at-list-p) (insert "*"))
	    (t (delete-char 1)))) ; whitespace
    (forward-line 1)))

(defun mdr-extract-header ()
  "Extract the standard ELisp file header into a string."
  (buffer-substring (point-min) (mdr-end-of-header)))

(defun mdr-end-of-header ()
  "Find the end of the header and return its position."
  (save-excursion
    (goto-char (point-min))
    (while (or (looking-at-p "\n") (looking-at-p ";;"))
      (forward-line 1))
    (point)))

(defun mdr-looking-at-list-p ()
  "Determine if the line we're looking should become a list item.
Requires point to be at the beginning of the line."
  (looking-at-p " ?[-a-zA-Z0-9]+:"))  ; why does [:alnum:] not work?

(defun mdr-find-and-replace-disclaimer ()
  "Find the GPL license disclaimer, and replace it with a
one-line note linked to the GPL website."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "This program is free software" nil t)
      (let ((start-line (progn (beginning-of-line) (point)))
      	    (end-line (search-forward
      		       "If not, see <http://www.gnu.org/licenses/>."
      		       nil t)))
      	(delete-region start-line end-line)
      	(insert "Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.")))))

(provide 'md-readme)
