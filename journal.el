;;; journal.el --- The Emacs Journal

;; Copyright (C) 2005 Hoan Ton-That <hoan@ton-that.org>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;;_* Commentary

;;;_* Prerequisites
(require 'cl)
(require 'emacs-wiki)
(defvar journal-loaded nil)

;;;_* Options
(defun journal-option-customized (sym val)
  (set sym val)
  (when journal-loaded
    (journal-update-wiki-project)))

;;;_ + Journal Options
(defgroup journal nil
  "Options controlling the behavior of journal."
  :group 'applications)

(defvar journal-project-default-name "JournalWiki"
  "*Default name of Journal project.

This is used by `journal-update-wiki-project' to make
sure that any old entries are removed correctly.")

(defcustom journal-project journal-project-default-name
  "The name of this project, used when referencing it from other
emacs-wiki projects."
  :type 'string
  :group 'journal)

(defcustom journal-directory "~/Blog"
  "The directory that contains your journal entries."
  :type 'directory
  :set 'journal-option-customized
  :group 'journal)

(defcustom journal-publishing-directory "~/WebWiki/Blog"
  "The directory where all journal entries are published to."
  :type 'directory
  :set 'journal-option-customized
  :group 'journal)

(defcustom journal-home-page "WelcomePage"
  "Title of the Journal Home page."
  :type 'string
  :set 'journal-option-customized
  :group 'journal)

(defcustom journal-title (concat user-full-name "'s Journal")
  "Title of the journal."
  :type 'string
  :group 'journal)

(defcustom journal-server-prefix "../Blog/"
  "The location of the publishing directory with respect to the
locations of the publishing directories of other emacs-wiki
projects."
  :type 'directory
  :set 'journal-option-customized
  :group 'journal)

(defcustom journal-use-other-window t
  "If non-nil, journal will open in another window."
  :type 'boolean
  :group 'journal)

(defcustom journal-use-day-pages t
  "If non-nil, allow the use of day pages."
  :type 'boolean
  :group 'journal)

(defcustom journal-use-category-pages t
  "If non-nil, allow the use of category pages."
  :type 'boolean
  :group 'journal)

(defcustom journal-default-category "CategoryMisc"
  "Title of the default category page."
  :type 'string
  :group 'journal)

(defcustom journal-reverse-chronological-order t
  "If non-nil, entries are added to the beginning of a page."
  :type 'boolean
  :group 'journal)

(defcustom journal-time-format "%a, %_d %b %Y %H:%M:%S %Z"
  "Format for the date string of journal entries.
See `format-time-string' for more information."
  :type 'string
  :group 'journal)

(defcustom journal-save-after-adding t
  "If non-nil, save buffers after adding an entry."
  :type 'boolean
  :group 'journal)

(defcustom journal-publishing-hooks
  (append (list 'journal-entry-update-maybe 'journal-home-update-maybe)
	  (when journal-use-day-pages
	    (list 'journal-day-update-maybe))
	  (when journal-use-category-pages
	    (list 'journal-category-update-maybe)))
  "A list of functions to be called when publishing entries.
Each function must take an entry as its argument."
  :type '(repeat function)
  :group 'journal)

(defcustom journal-custom-variables nil
  "A list of journal-specific Emacs-Wiki variable settings.
You can customize any emacs-wiki variable to be used specially within
journal mode buffers, except for the following, whose values are
derived from the other journal mode customized variables:

  `emacs-wiki-directories'
  `emacs-wiki-major-mode'
  `emacs-wiki-markup-tags'
  `emacs-wiki-publishing-markup'
  `emacs-wiki-url-regexp'
  `emacs-wiki-name-regexp'
  `emacs-wiki-url-or-name-regexp'
  `emacs-wiki-highlight-regexp'

If you want to customize the derived variables, you can set them from
`journal-mode-hook'."
  :type `(repeat
	  (choice
	   (cons :tag "emacs-wiki-predicate"
		 (const emacs-wiki-predicate) function)
	   (cons :tag "emacs-wiki-project-server-prefix"
		 (const emacs-wiki-project-server-prefix) string)
	   ,@(mapcar
	      (function
	       (lambda (sym)
		 (list 'cons :tag (symbol-name sym)
		       (list 'const sym)
		       (get sym 'custom-type))))
	      (apropos-internal "\\`emacs-wiki-"
				(function
				 (lambda (sym)
				   (get sym 'custom-type)))))))
  :set 'journal-option-customized
  :group 'journal)

;;;_* Variables
(defvar journal-index-title-threshold t
  "*If nil, filenames are always used in the index.
This is faster, but less informative. If a positive integer,
only that many bytes will be scanned for a #title directive.
Else, the entire wiki file is scanned for a #title.")

(defvar journal-category-prefix "Category"
  "The prefix attached to all category pages.")

(defvar journal-category-regexp
  (concat "^" (regexp-quote journal-category-prefix) "\\([A-Z][a-z]+\\)+$")
  "The regular expression which matches category pages.")

(defvar journal-day-regexp
  (concat "[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]#[A-Za-z0-9_%]+"
	  "\\|[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]")
  "The regular expression which matches day pages.")

(defvar journal-entry-regexp "^[0-9]+$"
  "The regular expression which matches entry pages.")

;;;_* Keybindings
(defvar journal-mode-map
  (let ((map (copy-keymap emacs-wiki-mode-map)))
    ;; Creation.
    (define-key map [?\C-c ?\C-c] 'journal-create-entry)
    ;; Movement.
    (define-key map [?\C-c ?\C-j ?\C-a] 'journal-first-entry)
    (define-key map [?\C-c ?\C-j ?\C-b] 'journal-backward-entry)
    (define-key map [?\C-c ?\C-j ?\C-e] 'journal-last-entry)
    (define-key map [?\C-c ?\C-j ?\C-f] 'journal-forward-entry)
    ;; Finding files.
    (define-key map [?\C-c ?\C-o ?\C-c] 'journal-find-category-file)
    (define-key map [?\C-c ?\C-o ?\C-d] 'journal-find-day-file)
    (define-key map [?\C-c ?\C-o ?\C-e] 'journal-find-entry-file)
    ;; Return map.
    map)
  "Keymap used by Journal mode.")

;;;_* Mode
(define-derived-mode journal-mode emacs-wiki-mode "Journal"
  "An extension to Emacs Wiki to maintain a journal.
\\{journal-mode-map}"
  (condition-case err
      (hack-local-variables)
    (error (message "File local-variables error: %s"
		    (prin1-to-string err))))
  ;; Check to see if the mode changed.
  (when (eq major-mode 'journal-mode)
    (let ((hook (if (boundp 'write-file-functions)
		    'write-file-functions
		  'local-write-file-hooks)))
      (add-hook hook 'journal-run-publishing-hooks t t)
    (journal-prepare-file))))

(defun journal-prepare-file ()
  "Prepare the journal file."
  ;; The only file that needs to be prepared is the home page.
  (when (journal-home-page-p)
    (let* ((title "#title ")
	   (new-title (concat title journal-title))
	   (title-regexp (regexp-quote title))
	   (correct-title-regexp (regexp-quote new-title)))
      ;; Goto the beginning of the buffer.
      (goto-char (point-min))
      ;; Check if looking at a title.
      (if (looking-at title-regexp)
	  ;; Check if the title is correct.
	  (if (looking-at correct-title-regexp)
	      ;; Nothing to be done.
	      (ignore)
	    ;; Delete the current title.
	    (delete-region (point-min) (line-end-position))
	    ;; Insert the title.
	    (insert new-title "\n\n"))
	;; Insert the title.
	(insert new-title "\n\n"))
      ;; Make the buffer unmodified.
      (set-buffer-modified-p nil))))

;;;_* Internal Functions

;;;_ + Emacs-Wiki interface
(defun journal-find-file (page &optional command)
  "Open the emacs-wiki PAGE by name.
If COMMAND is non-nil, it is the function used to visit the file."
  (make-directory journal-directory t)
  (funcall (or command 'find-file)
	   (expand-file-name page journal-directory)))

(defun journal-update-wiki-project ()
  "Update the \"journal\" project in `emacs-wiki-projects'."
  ;; Remove the entry associated with Journal
  (setq emacs-wiki-projects
	(delq (assoc journal-project emacs-wiki-projects)
	      emacs-wiki-projects))
  ;; Remove an entry that uses the default Journal project name
  (setq emacs-wiki-projects
	(delq (assoc journal-project-default-name
		     emacs-wiki-projects)
	      emacs-wiki-projects))
  ;; Assign new contents to Journal entry
  (add-to-list 'emacs-wiki-projects
	       `(,journal-project
		 . ((emacs-wiki-directories
		     . (,journal-directory))
		    (emacs-wiki-home-page
		     . ,journal-home-page)
		    (emacs-wiki-major-mode
		     . journal-mode)
		    (emacs-wiki-url-or-name-regexp . nil)
		    (emacs-wiki-publishing-directory
		     . ,journal-publishing-directory)
		    (emacs-wiki-project-server-prefix
		     . ,journal-server-prefix)
		    (emacs-wiki-index-title-threshold
		     . ,journal-index-title-threshold))))
  (emacs-wiki-update-project-interwikis))

(defalias 'journal-make-link 'emacs-wiki-make-link)

(defun journal-get-link-url (link)
  "Return the url part of LINK."
  ;; This should be defined in emacs-wiki.el.
  (let ((s (car (split-string link (regexp-quote "][")))))
    ;; Get rid of the brackets.
    (substring s 2)))

(defun journal-get-link-title (link)
  "Return the title part of LINK."
  ;; This should be defined in emacs-wiki.el.
  (let ((s (cadr (split-string link (regexp-quote "][")))))
    ;; Get rid of the brackets.
    (substring s 0 (- (length s) 2))))

(defun journal-make-category-link (link)
  "Like `journal-make-link' except the title is the stripped category."
  (journal-make-link link (journal-category-strip link)))

;;;_ + Data Representation
(defun journal-make-entry (title text time id category day)
  "Construct a journal entry."
  (unless title
    (setq title "Untitled"))
  (unless text
    (setq text ""))
  (unless time
    (setq time (journal-format-time)))
  (unless id
    (setq id (journal-entry-next)))
  (unless category
    (setq category (list journal-default-category)))
  (unless day
    (setq day (journal-format-time-day)))
  (list title text time id category day))

(defun journal-title (entry)
  "Select the title of ENTRY."
  (elt entry 0))

(defun journal-text (entry)
  "Select the text of ENTRY."
  (elt entry 1))

(defun journal-time (entry)
  "Select the time of ENTRY."
  (elt entry 2))

(defun journal-id (entry)
  "Select the id of ENTRY."
  (elt entry 3))

(defun journal-categories (entry)
  "Select the categories of ENTRY."
  (when journal-use-category-pages
    (elt entry 4)))

(defun journal-day (entry)
  "Select the day of ENTRY."
  (when journal-use-day-pages
    (elt entry 5)))

(defun journal-set-title (entry title)
  "Set the title of ENTRY to TITLE."
  (setcar entry title))

(defun journal-set-text (entry text)
  "Set the text of ENTRY to TEXT."
  (setcar (cdr entry) text))

(defun journal-set-time (entry time)
  "Set the time of ENTRY to TIME."
  (setcar (cddr entry) time))

(defun journal-set-id (entry id)
  "Set the id of ENTRY to ID."
  (setcar (cdddr entry) id))

(defun journal-set-categories (entry categories)
  "Set the categories of ENTRY to CATEGORIES."
  (setcar (cddddr entry) categories))

(defun journal-set-day (entry day)
  "Set the day of ENTRY to DAY."
  (setcar (cdr (cddddr entry)) day))

;;;_ + Insertion
(defun journal-category-insert (entry)
  "Insert ENTRY into each of the category pages of the journal."
  (when journal-use-category-pages
    ;; For every category an entry belongs to...
    (mapc (lambda (category)
	    (save-window-excursion
	      (save-excursion
		(save-restriction
		  ;; The point of the previous entry.
		  (let ((inserted-before nil))
		    ;; Open the category page.
		    (journal-find-file category)
		    ;; Widen.
		    (widen)
		    ;; Goto the first entry.
		    (journal-first-entry)
		    ;; Check if entry is already inserted.
		    (while (not (eobp))
		      ;; Compare by id.
		      (when (equal (journal-id (journal-unformat))
				   (journal-id entry))
			;; If it exists, delete it.
			(journal-erase-current-entry)
			;; Save the point
			(setq inserted-before (point)))
		      ;; Move to the next entry.
		      (journal-forward-entry 1))
		    ;; If the entry has been inserted before, insert
		    ;; in its place.
		    (if inserted-before
			(goto-char inserted-before)
		      ;; Otherwise, if entries are in reverse
		      ;; chronological order then move the point to
		      ;; the first entry, otherwise move it to the end
		      ;; of the buffer.
		      (if journal-reverse-chronological-order
			  (journal-first-entry)
			(goto-char (point-max))))
		    ;; Insert the formatted entry.
		    (insert (journal-category-format entry))
		    ;; Save the buffer.
		    (when journal-save-after-adding
		      ;; Prevent `journal-publishing-hooks' being called
		      ;; again.
		      (let ((journal-publishing-hooks nil))
			(save-buffer))))))))
	  (journal-categories entry))))

(defun journal-day-insert (entry)
  "Insert ENTRY into the day page of the journal."
  (when journal-use-day-pages
    (save-window-excursion
      (save-excursion
	(save-restriction
	  ;; The point of the previous entry.
	  (let ((inserted-before nil))
	    ;; Open the day page.
	    (journal-find-file (journal-day entry))
	    ;; Widen.
	    (widen)
	    ;; Goto the first entry.
	    (journal-first-entry)
	    ;; Check if entry is already inserted.
	    (while (not (eobp))
	      ;; Compare by id.
	      (when (equal (journal-id (journal-unformat))
			   (journal-id entry))
		;; If it exists, delete it.
		(journal-erase-current-entry)
		;; Save the point
		(setq inserted-before (point)))
	      ;; Move to the next entry.
	      (journal-forward-entry 1))
	    ;; If the entry has been inserted before, insert in its
	    ;; place.
	    (if inserted-before
		(goto-char inserted-before)
	      ;; Otherwise, if entries are in reverse chronological
	      ;; order then move the point to the first entry,
	      ;; otherwise move it to the end of the buffer.
	      (if journal-reverse-chronological-order
		  (journal-first-entry)
		(goto-char (point-max))))
	    ;; Insert the formatted entry.
	    (insert (journal-category-format entry))
	    ;; Save the buffer.
	    (when journal-save-after-adding
	      ;; Prevent `journal-publishing-hooks' being called
	      ;; again.
	      (let ((journal-publishing-hooks nil))
		(save-buffer)))))))))

(defun journal-entry-insert (entry)
  "Insert ENTRY into the entry page of the journal."
  (save-window-excursion
    (save-excursion
      (save-restriction
	;; Open the entry.
	(journal-find-file (journal-id entry))
	;; Erase the buffer.
	(erase-buffer)
	;; Insert the entry.
	(insert (journal-entry-format entry))
	;; Save the buffer.
	(when journal-save-after-adding
	  ;; Prevent `journal-publishing-hooks' being called again.
	  (let ((journal-publishing-hooks nil))
	    (save-buffer)))))))

(defun journal-home-insert (entry)
  "Insert ENTRY into the home page of the journal."
  (save-window-excursion
    (save-excursion
      (save-restriction
	;; The point of the previous entry.
	(let ((inserted-before nil))
	  ;; Open the home page.
	  (journal-find-file journal-home-page)
	  ;; Widen.
	  (widen)
	  ;; Goto the first entry.
	  (journal-first-entry)
	  ;; Check if entry is already inserted.
	  (while (not (eobp))
	    ;; Compare by id.
	    (when (equal (journal-id (journal-unformat))
			 (journal-id entry))
	      ;; If it exists, delete it.
	      (journal-erase-current-entry)
	      ;; Save the point
	      (setq inserted-before (point)))
	    ;; Move to the next entry.
	    (journal-forward-entry 1))
	  ;; If the entry has been inserted before, insert in its
	  ;; place.
	  (if inserted-before
	      (goto-char inserted-before)
	    ;; Otherwise, if entries are in reverse chronological
	    ;; order then move the point to the first entry, otherwise
	    ;; move it to the end of the buffer.
	    (if journal-reverse-chronological-order
		(journal-first-entry)
	      (goto-char (point-max))))
	  ;; Insert the formatted entry.
	  (insert (journal-category-format entry))
	  ;; Save the buffer.
	  (when journal-save-after-adding
	    ;; Prevent `journal-publishing-hooks' being called again.
	    (let ((journal-publishing-hooks nil))
	      (save-buffer))))))))

;;;_ + Operations
(defvar journal-next-entry-regexp "^* "
  "The regular expression which matches the next entry.")

(defun journal-first-entry ()
  "Move to the first entry."
  (interactive)
  ;; Goto the beginning of the buffer.
  (goto-char (point-min))
  ;; If not already at an entry.
  (unless (looking-at journal-next-entry-regexp)
    ;; Move forward an entry.
    (journal-forward-entry 1)))

(defun journal-last-entry ()
  "Move to the last entry."
  (interactive)
  ;; Goto the end of the buffer.
  (goto-char (point-max))
  ;; Move backward an entry.
  (journal-backward-entry 1))

(defun journal-forward-entry (&optional arg)
  "Move forward until encountering an entry.
With argument, do this that many times.
If no entry is found go to the end of the buffer."
  (interactive "p")
  ;; If no argument is passed, let it be 1.
  (unless arg
    (setq arg 1))
  ;; If the argument is negative, call journal-backward-entry.
  (when (< arg 0)
    (journal-backward-entry (- arg)))
  ;; If the point is already at an entry, make sure we skip it.
  (when (looking-at journal-next-entry-regexp)
    (setq arg (1+ arg)))
  ;; If an entry is found, go to the beginning of it, otherwise goto
  ;; the end of the buffer.
  (if (re-search-forward journal-next-entry-regexp nil t arg)
      (goto-char (match-beginning 0))
    (goto-char (point-max))))

(defun journal-backward-entry (&optional arg)
  "Move backward until encountering an entry.
With argument, do this that many times.
If no entry is found go to the beginning of the buffer."
  (interactive "p")
  ;; If no argument is passed, let it be 1.
  (unless arg
    (setq arg 1))
  ;; If the argument is negative, call journal-forward-entry.
  (when (< arg 0)
    (journal-forward-entry (- arg)))
  ;; If an entry is found, go to the beginning of it, otherwise goto
  ;; the beginning of the buffer.
  (if (re-search-backward journal-next-entry-regexp nil t arg)
      (goto-char (match-beginning 0))
    (goto-char (point-min))))

(defun journal-narrow-to-current-entry ()
  "Narrow to the current entry."
  (narrow-to-region
   (if (looking-at journal-next-entry-regexp)
       (point)
     (save-excursion (journal-backward-entry 1)
		     (point)))
   (save-excursion (journal-forward-entry 1)
		   (point))))

(defun journal-erase-current-entry ()
  "Erase the current entry."
  (save-window-excursion
    (save-excursion
      (save-restriction
	;; Narrow the current entry.
	(journal-narrow-to-current-entry)
	;; Delete from beginning to end of region.
	(delete-region (point-min) (point-max))))))

;;;_ + Predicates
(defun journal-category-page-p (&optional page)
  "Return t if PAGE is a category page.
If page is not given, the default is the buffer name."
  (unless page
    (setq page (file-name-nondirectory (buffer-file-name))))
  (if (string-match journal-category-regexp page) t nil))

(defun journal-day-page-p (&optional page)
  "Return t if PAGE is a day page.
If page is not given, the default is the buffer name."
  (unless page
    (setq page (file-name-nondirectory (buffer-file-name))))
  (if (string-match journal-day-regexp page) t nil))

(defun journal-entry-page-p (&optional page)
  "Return t if PAGE is an entry page.
If page is not given, the default is the buffer name."
  (unless page
    (setq page (file-name-nondirectory (buffer-file-name))))
  (if (string-match journal-entry-regexp page) t nil))

(defun journal-home-page-p (&optional page)
  "Return t if PAGE is the home page.
If page is not given, the default is the buffer name."
  (unless page
    (setq page (file-name-nondirectory (buffer-file-name))))
  (equal page journal-home-page))

;;;_ + Pages
(defun journal-pages (regexp)
  "Return possible files in `journal-directory' that match REGEXP."
  ;; Remove all files that do not match REGEXP.
  (remove-if-not (lambda (file)
		   (string-match regexp (car file)))
   (cadr (assoc journal-project emacs-wiki-file-alist))))

(defun journal-category-alist ()
  "Return possible categories in `journal-directory'."
  (journal-pages journal-category-regexp))

(defun journal-days-alist ()
  "Return possible day pages in `journal-directory'."
  (journal-pages journal-day-regexp))

(defun journal-entries-alist ()
  "Return possible entries in `journal-directory'."
  (journal-pages journal-entry-regexp))

;;;_ + Prompting
(defalias 'journal-read-name 'emacs-wiki-read-name)
(defvaralias 'journal-default-page 'emacs-wiki-default-page)

(defun journal-prompt-for-categories (&optional default)
  "Prompt for a list of categories.
DEFAULT is the value used when the user types in nothing."
  ;; Initially the list of categories is empty.  The first prompt has
  ;; the default category, and following ones have nil.
  (let* ((categories nil)
	 (next (journal-prompt-for-category (or default
						journal-default-category))))
    ;; Until the user types in nil.
    (while next
      ;; Add the current category to the list.
      (setq categories (cons next categories))
      ;; Get another category.
      (setq next (journal-prompt-for-category)))
    ;; Reverse the category list.
    (nreverse categories)))

(defun journal-prompt-for-category (&optional default)
  "Prompt for a category page.
DEFAULT is the value used when the user types in nothing."
  ;; Set the default value.
  (let ((journal-default-page default))
    ;; Read the page name.
    (let ((x (journal-read-name (journal-category-alist) "Category: ")))
      ;; If the page name is a category.
      (if (or (null x) (journal-category-page-p x))
	  ;; Return it.
	  x
	;; Otherwise make it a category page.
	(concat journal-category-prefix x)))))

(defun journal-prompt-for-day (&optional default)
  "Prompt for a day page.
DEFAULT is the value used when the user types in nothing."
  ;; Set the default value.
  (let ((journal-default-page (or default (journal-format-time-day))))
    ;; Read the page name.
    (let ((x (journal-read-name (journal-days-alist) "Day: ")))
      ;; If the page is a day page.
      (if (journal-day-page-p x)
	  ;; Return it.
	  x
	;; Otherwise prompt again.
	(journal-prompt-for-day default)))))

(defun journal-prompt-for-entry (&optional default)
  "Prompt for a entry page.
DEFAULT is the value used when the user types in nothing."
  ;; Set the default value.
  (let ((journal-default-page (or default (journal-entry-next))))
    ;; Read the page name.
    (let ((x (journal-read-name (journal-entries-alist) "Entry: ")))
      ;; If the page is an entry page.
      (if (journal-entry-page-p x)
	  ;; Return it.
	  x
	;; Otherwise prompt again.
	(journal-prompt-for-entry default)))))

;;;_ + Formatting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WARNING: This is stupid	  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun journal-category-format (entry)
  "Format ENTRY for a day page."
  (let ((title (journal-title entry))
	(text (journal-text entry))
	(time (journal-time entry))
	(id (journal-id entry))
	(categories (journal-categories entry))
	(day (journal-day entry)))
    (format "* %s\n** %s\n\n%s\n\n"
	    ;; The title is linked to the entry page.
	    (journal-make-link id title)
	    ;; The time is linked to the day page.
	    (concat (if journal-use-day-pages
			(journal-make-link day time)
		      time)
		    ;; The stripped categories are linked to full
		    ;; names.
		    (when journal-use-category-pages
		      (concat " --- ("
			      (mapconcat #'journal-make-category-link
					 categories
					 ", ")
			      ")")))
	    text)))

(defun journal-day-format (entry)
  "Format ENTRY for a day page."
  (let ((title (journal-title entry))
	(text (journal-text entry))
	(time (journal-time entry))
	(id (journal-id entry))
	(categories (journal-categories entry))
	(day (journal-day entry)))
    (format "* %s\n** %s\n\n%s\n\n"
	    ;; The title is linked to the entry page.
	    (journal-make-link id title)
	    (concat time
		    ;; The stripped categories are linked to full
		    ;; names.
		    (when journal-use-category-pages
		      (concat " --- ("
			      (mapconcat #'journal-make-category-link
					 categories
					 ", ")
			      ")")))
	    text)))

(defun journal-entry-format (entry)
  "Format ENTRY for an entry page."
  (let ((title (journal-title entry))
	(text (journal-text entry))
	(time (journal-time entry))
	(id (journal-id entry))
	(categories (journal-categories entry))
	(day (journal-day entry)))
    (format "#title %s\n\n* %s\n\n%s\n\n"
	    title
	    (concat (if journal-use-day-pages
			;; The time is linked to the day page.
			(journal-make-link day time)
		      time)
		    ;; The stripped categories are linked to full
		    ;; names.
		    (when journal-use-category-pages
		      (concat " --- ("
			      (mapconcat #'journal-make-category-link
					 categories
					 ", ")
			      ")")))
	    text)))

(defun journal-home-format (entry)
  "Format ENTRY for a day page."
  (let ((title (journal-title entry))
	(text (journal-text entry))
	(time (journal-time entry))
	(id (journal-id entry))
	(categories (journal-categories entry))
	(day (journal-day entry)))
    (format "* %s\n** %s\n\n%s\n\n"
	    ;; The title is linked to the entry page.
	    (journal-make-link id title)
	    (concat (if journal-use-day-pages
			;; The time is linked to the day page.
			(journal-make-link day time)
		      time)
		    ;; The stripped categories are linked to full
		    ;; names.
		    (when journal-use-category-pages
		      (concat " --- ("
			      (mapconcat #'journal-make-category-link
					 categories
					 ", ")
			      ")")))
	    text)))

;;;_ + Unformatting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WARNING: This is really stupid	 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun journal-unformat ()
  "Unformat the current entry."
  ;; Dispatch on the page type.
  (cond ((journal-category-page-p) (journal-category-unformat))
	((journal-day-page-p) (journal-day-unformat))
	((journal-entry-page-p) (journal-entry-unformat))
	((journal-home-page-p) (journal-home-unformat))
	(t (error "Do not know how to unformat current entry."))))

(defun journal-category-unformat ()
  "Unformat the current entry on a category page into list structure."
  (when (and journal-use-category-pages (journal-category-page-p))
    (save-window-excursion
      (save-excursion
	(save-restriction
	  ;; Narrow to the current entry.
	  (journal-narrow-to-current-entry)
	  ;; Goto the beginning of the entry.
	  (goto-char (point-min))
	  ;; Extract the contents.
	  (let ((title
		 (journal-get-link-title
		  (buffer-substring-no-properties
		   (+ (point-min) 2)
		   (line-end-position))))
		(text
		 (buffer-substring-no-properties
		  (save-excursion
		    (forward-line 3)
		    (line-beginning-position))
		  (- (point-max) 2)))
		(time
		 ((lambda (x)
		    (if journal-use-day-pages
			(journal-get-link-title x)
		      x))
		  (buffer-substring-no-properties
		   (save-excursion
		     (forward-line 1)
		     (forward-char 3)
		     (point))
		   (save-excursion
		     (forward-line 1)
		     (search-forward " --- ")
		     (match-beginning 0)))))
		(id
		 (journal-get-link-url
		  (buffer-substring-no-properties
		   (+ (point-min) 2)
		   (line-end-position))))
		(categories
		 (mapcar (lambda (category)
			   (concat journal-category-prefix
				   (journal-get-link-title category)))
			 (split-string (buffer-substring-no-properties
					(save-excursion
					  (forward-line 1)
					  (search-forward "(")
					  (match-end 0))
					(save-excursion
					  (forward-line 1)
					  (search-forward ")")
					  (match-beginning 0)))
				       ", ")))
		(day
		 (when journal-use-day-pages
		   (journal-get-link-url
		    (buffer-substring-no-properties
		     (save-excursion
		       (forward-line 1)
		       (forward-char 3)
		       (point))
		     (save-excursion
		       (forward-line 1)
		       (search-forward " --- ")
		       (match-beginning 0)))))))
	    ;; Put it into list structure.
	    (journal-make-entry title text time id categories day)))))))

(defun journal-day-unformat ()
  "Unformat the current entry on a day page into list structure."
  (when (and journal-use-day-pages (journal-day-page-p))
    (save-window-excursion
      (save-excursion
	(save-restriction
	  ;; Narrow to the current entry.
	  (journal-narrow-to-current-entry)
	  ;; Goto the beginning of the entry.
	  (goto-char (point-min))
	  ;; Extract the contents.
	  (let ((title
		 (journal-get-link-title
		  (buffer-substring-no-properties
		   (+ (point-min) 2)
		   (line-end-position))))
		(text
		 (buffer-substring-no-properties
		  (save-excursion
		    (forward-line 3)
		    (line-beginning-position))
		  (- (point-max) 2)))
		(time
		 (buffer-substring-no-properties
		  (save-excursion
		    (forward-line 1)
		    (forward-char 3)
		    (point))
		  (save-excursion
		    (forward-line 1)
		    (if journal-use-category-pages
			(progn (search-forward " --- ")
			       (match-beginning 0))
		      (line-end-position)))))
		(id
		 (journal-get-link-url (buffer-substring-no-properties
					(+ (point-min) 2)
					(line-end-position))))
		(categories
		 (when journal-use-category-pages
		   (mapcar (lambda (x)
			     (concat journal-category-prefix
				     (journal-get-link-title x)))
			   (split-string (buffer-substring-no-properties
					  (save-excursion
					    (forward-line 1)
					    (search-forward "(")
					  (match-end 0))
					  (save-excursion
					    (forward-line 1)
					    (search-forward ")")
					    (match-beginning 0)))
					 ", "))))
		(day (file-name-nondirectory (buffer-file-name))))
	    ;; Put it into list structure.
	    (journal-make-entry title text time id categories day)))))))

(defun journal-entry-unformat ()
  "Unformat current entry on an entry page into list structure."
  (when (journal-entry-page-p)
    (save-window-excursion
      (save-excursion
	(save-restriction
	  ;; Goto the beginning of the buffer.
	  (goto-char (point-min))
	  ;; Extract the contents.
	  (let ((title
		 (buffer-substring-no-properties
		  (+ (point-min) 7)
		  (line-end-position)))
		(text
		 (buffer-substring-no-properties
		  (save-excursion
		    (forward-line 4)
		    (line-beginning-position))
		  (- (point-max) 2)))
		(time
		 (when journal-use-day-pages
		   (journal-get-link-title
		    (buffer-substring-no-properties
		     (save-excursion
		       (forward-line 2)
		       (forward-char 2)
		       (point))
		     (save-excursion
		       (forward-line 2)
		       (if (search-forward " --- ")
			   (match-beginning 0)
			 (line-end-position)))))))
		(id (file-name-nondirectory (buffer-file-name)))
		(categories
		 (when journal-use-category-pages
		   (mapcar (lambda (category)
			     (concat journal-category-prefix
				     (journal-get-link-title category)))
			   (split-string (buffer-substring-no-properties
					  (save-excursion
					    (forward-line 2)
					    (search-forward "(")
					    (match-end 0))
					  (save-excursion
					    (forward-line 2)
					    (search-forward ")")
					    (match-beginning 0)))
					 ", "))))
		(day
		 (journal-get-link-url
		  (buffer-substring-no-properties
		   (save-excursion
		     (forward-line 2)
		     (forward-char 2)
		     (point))
		   (save-excursion
		     (forward-line 2)
		     (if (search-forward " --- ")
			 (match-beginning 0)
		       (line-end-position)))))))
	    ;; Put it into list structure.
	    (journal-make-entry title text time id categories day)))))))

(defun journal-home-unformat ()
  "Unformat current entry on an home page into list structure."
  (when (journal-home-page-p)
    (save-window-excursion
      (save-excursion
	(save-restriction
	  ;; Narrow to the current entry.
	  (journal-narrow-to-current-entry)
	  ;; Goto the beginning of the entry.
	  (goto-char (point-min))
	  ;; Extract the contents.
	  (let ((title
		 (journal-get-link-title
		  (buffer-substring-no-properties
		   (+ (point-min) 2)
		   (line-end-position))))
		(text
		 (buffer-substring-no-properties
		  (save-excursion
		    (forward-line 3)
		    (line-beginning-position))
		  (- (point-max) 2)))
		(time
		 ((lambda (x)
		    (if journal-use-day-pages
			(journal-get-link-title x)
		      x))
		  (buffer-substring-no-properties
		   (save-excursion
		     (forward-line 1)
		     (forward-char 3)
		     (point))
		   (save-excursion
		     (forward-line 1)
		     (search-forward " --- ")
		     (match-beginning 0)))))
		(id
		 (journal-get-link-url
		  (buffer-substring-no-properties
		   (+ (point-min) 2)
		   (line-end-position))))
		(categories
		 (when journal-use-category-pages
		   (mapcar (lambda (category)
			     (concat journal-category-prefix
				     (journal-get-link-title category)))
			   (split-string (buffer-substring-no-properties
					  (save-excursion
					    (forward-line 1)
					    (search-forward "(")
					    (match-end 0))
					  (save-excursion
					    (forward-line 1)
					    (search-forward ")")
					    (match-beginning 0)))
					 ", "))))
		(day
		 (journal-get-link-url
		  (buffer-substring-no-properties
		   (save-excursion
		     (forward-line 1)
		     (forward-char 3)
		     (point))
		   (save-excursion
		     (forward-line 1)
		     (if (search-forward " --- ")
			 (match-beginning 0)
		       (line-end-position)))))))
	    ;; Put it into list structure.
	    (journal-make-entry title text time id categories day)))))))

;;;_ + Updating
(defun journal-run-publishing-hooks ()
  "Run the hooks in `journal-publishing-hooks'."
  (run-hooks 'journal-publishing-hooks))

(defun journal-day-update-maybe ()
  "Update the current entry to its day page."
  ;; Only when using day pages, and current page is not a day page.
  (when (and journal-use-day-pages (not (journal-day-page-p)))
    (journal-day-insert (journal-unformat))))

(defun journal-category-update-maybe ()
  "Update the current entry to its category page."
  ;; Only when using category pages, and current page is not a
  ;; category page.
  (when (and journal-use-category-pages (not (journal-category-page-p)))
    (journal-category-insert (journal-unformat))))

(defun journal-entry-update-maybe ()
  "Update the current entry to its entry page."
  ;; Only when the current page is not an entry page.
  (when (not (journal-entry-page-p))
    (journal-entry-insert (journal-unformat))))

(defun journal-home-update-maybe ()
  "Update the current entry to its home page."
  ;; Only when the current page is not an home page.
  (when (not (journal-home-page-p))
    (journal-home-insert (journal-unformat))))

;;;_ + Misc
(defun journal-format-time (&optional time)
  "Format TIME with `journal-time-format'."
  (format-time-string journal-time-format time))

(defun journal-format-time-day (&optional time)
  "Format TIME for use as the name of the day page."
  (let ((d (decode-time time)))
    (format "%04d.%02d.%02d"
	    (elt d 5)
	    (elt d 4)
	    (elt d 3))))

(defun journal-category-strip (category)
  "Remove the category prefix from CATEGORY."
  (substring category (length journal-category-prefix)))

(defun journal-entry-operation (op &rest entries)
  "Apply OP to ENTRIES."
  (let ((result (apply op (mapcar #'string-to-number entries))))
    (if (numberp result)
	(number-to-string result)
      result)))

(defun journal-entry-increment (entry)
  "Return the increment of ENTRY."
  (journal-entry-operation #'1+ entry))

(defun journal-entry-< (entry1 entry2)
  "Return t if first entry is less than second entry."
  (journal-entry-operation #'< entry1 entry2))

(defun journal-entry-> (entry1 entry2)
  "Return t if first entry is greater than second entry."
  (journal-entry-operation #'> entry1 entry2))

(defun journal-entry-largest ()
  "Return the largest entry."
  (car (sort (mapcar (lambda (x) (car x)) (journal-entries-alist))
	     #'journal-entry->)))

(defun journal-entry-next ()
  "Return the unique name of the next journal entry."
  (let ((entries (journal-entries-alist)))
    ;; If there are no entries then create 1, otherwise increment the
    ;; largest one.
    (if (not entries)
	"1"
      (journal-entry-increment (journal-entry-largest)))))

;;;_* User Functions

;;;_ + Adding
(defun journal-create-entry (title categories)
  "Create a new entry named TITLE in CATEGORIES."
  (interactive (list (read-string "Title: ")
		     (when journal-use-category-pages
		       (journal-prompt-for-categories))))
  (let ((entry (journal-make-entry title nil nil nil categories nil))
	;; Do not save immediately.
	(journal-save-after-adding nil))
    ;; Insert the newly created entry.  All the nil slots will be
    ;; automatically generated.
    (journal-entry-insert entry)
    ;; Open the entry.
    (journal-find-file (journal-id entry))
    ;; Goto the insertion point.
    (goto-char (- (point-max) 2))))

;;;_ + Visiting
(defun journal-find-category-file (category)
  "Visit a journal CATEGORY file."
  (interactive (list (list (journal-prompt-for-category
			    journal-default-category))))
  (journal-find-file category
		     (when journal-use-other-window
		       'find-file-other-window)))

(defun journal-find-day-file (day)
  "Visit a journal DAY file."
  (interactive (list (journal-prompt-for-day)))
  (journal-find-file day
		     (when journal-use-other-window
		       'find-file-other-window)))

(defun journal-find-entry-file (entry)
  "Visit a entry DAY file."
  (interactive (list (journal-prompt-for-entry)))
  (journal-find-file entry
		     (when journal-use-other-window
		       'find-file-other-window)))

;;;_ + Starting
(defun journal ()
  "Start the journal for the day."
  (interactive)
  (journal-find-file journal-home-page
		     (when journal-use-other-window
		       'find-file-other-window)))

(defun journal-other-window ()
  "Start the journal for the day in another window."
  (interactive)
  ;; Use the other window.
  (let ((journal-use-other-window t))
    ;; Run journal.
    (journal)))

(defun journal-same-window ()
  "Start the journal for the day in this window."
  (interactive)
  ;; Do not use the other window.
  (let ((journal-use-other-window nil))
    ;; Run journal.
    (journal)))

;;;_* Initialization
(setq journal-loaded t)
(journal-update-wiki-project)

(provide 'journal)

;;;_* Local emacs variables

;; Local variables:
;; allout-layout: (* :)
;; End:

;;; journal.el ends here
