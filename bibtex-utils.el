A modified version of this file is now available at

https://bitbucket.org/tws/bibtex-utils

And it should be on MELPA soon.

;;; bibtex-utils.el --- utilities for BibTeX

;; Copyright (C) 2007 Bastien Guerry
;; Copyright (C) 2010-11 Matt Lundin
;;
;; Author: bzg AT altern DOT org
;; Version: 0.1a
;; Keywords: bibtex
;; URL: http://www.cognition.ens.fr/~guerry/u/bibtex-utils.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;; 
;; This file is not part of GNU Emacs.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'bibtex-utils)

;;; Code:

;; FIXME Coud we compute this from `bibtex-entry-field-alist'?
(defvar bibtex-default-keys
       '("abstract" "address" "annote"
	 "author" "booktitle" "editor"
	 "journal" "key" "keywords"
	 "month" "number" "pages"
	 "publisher" "series" "title"
	 "type" "url" "volume" "year"))

(defun bibtex-collect-keywords-values (&optional regexp)
  "Collect values in keywords fields of all BibTeX entries.
Maybe restrict the values to those matching REGEXP."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (keywords kstring)
      (while (re-search-forward "^\\s-*keywords.*{\\([^}]+\\)}" nil t)
	(setq kstring (match-string 1))
	(mapc
	 (lambda (v) 
	   (if regexp (if (string-match regexp v) 
			  (add-to-list 'keywords v t))
	     (add-to-list 'keywords v t)))
	 (split-string kstring ",[ \n]*\\|{\\|}" t)))
      keywords)))

(defun bibtex-select-entries (key regexp)
  "Select bibtex entries that have their KEY matching REGEXP."
  (interactive
   (list (completing-read "Key: " bibtex-default-keys)
	 (read-string "Regexp: ")))
  (save-excursion
    (goto-char (point-min))
    (let (output item)
      (while (re-search-forward "^@[a-zA-Z0-9]+{" nil t)
	(goto-char (match-beginning 0))
	(let* ((entry (bibtex-parse-entry))
	       (key-field (cdr (assoc key entry))))
	  (when (and key-field (string-match regexp key-field))
	    (add-to-list 'output (buffer-substring
				  (bibtex-beginning-of-entry)
				  (bibtex-end-of-entry)) t)))
        (beginning-of-line 2))
      (switch-to-buffer-other-window
       (get-buffer-create "*BibTeX selected entries*"))
      (if (null output)
	  (error "No BibTeX entry which \"%s\" key matches \"%s\""
		 key regexp)
	(prog1 (message "Returned %d entries" (length output))
	  (bibtex-mode)
	  (while (setq item (pop output))
	    (insert item "\n\n"))
	  (goto-char (point-min)))))))

(defun bibtex-make-field-keywords (&optional arg)
  "Make a keywords field.
If ARG is nil, ask for each keyword and offer completion over
keywords that are already available in the buffer.  Inserting 
the empty string will quit the prompt."
  (interactive "P")
  (let ((elist (save-excursion (bibtex-beginning-of-entry)
			       (bibtex-parse-entry)))
	append)
    (if (assoc "keywords" elist)
	(progn (setq append t)
	       (bibtex-beginning-of-entry)
	       (goto-char 
		(car (last (bibtex-search-forward-field "keywords" t)))))
      (bibtex-make-field "keywords" t nil))
    (skip-chars-backward "}")
    (unless arg
      (let ((cnt 0)
	    (keywords (bibtex-collect-keywords-values))
            k)
	(while (and (setq k (completing-read 
			     "Keyword (RET to quit): " keywords nil))
		    (not (equal k "")))
	  (when append (insert ", "))
	  (setq cnt (1+ cnt))
	  (insert (format "%s%s" (if (> cnt 1) ", " "") k)))))))

(global-set-key (kbd "C-c k") 'bibtex-make-field-keywords)

(provide 'bibtex-utils)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

