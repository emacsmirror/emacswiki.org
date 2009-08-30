;;; embeddedchangelog.el --- edit a change log embedded in a file

;; Copyright (c) 2002 Michele Bini

;; Author: Michele Bini <mibin@libero.it>
;; Created: 12 Jan 2002
;; Version: 0.2
;; Keywords: tools

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; * Description

;; This package allows you to add entries to an embedded change log
;; like the one in this file.

;; * Installation

;; Add the following to your .emacs:
;; (autoload 'embedded-changelog-add-entry
;; "/path/to/embeddedchangelog.el" nil t)

;;; History:
;; 2002-03-07  Michele Bini  <mibin@libero.it>
;;
;;   * embeddedchangelog.el: File name renamed.
;;   (narrow-to-embedded-changelog): Documentation fixes.
;;   (embedded-changelog-buffer): New variable.
;;   (embedded-changelog-add-entry): Use an already existing change
;;   log buffer, if possible.
;;
;; 2002-02-27  Michele Bini  <mibin@libero.it>
;;
;;   * embedded-changelog.el (embedded-changelog-add-entry): Left
;;   margin set to 2.
;;   (embedded-changelog-write-file): Return true.  Use
;;   set-buffer-modified-p.  Ask about adding a Code section before
;;   adding an History one.
;;   (embedded-changelog-left-margin): New variable.

;;; Code:

(require 'add-log)

(defgroup embedded-changelog nil
  "Edit change logs embedded in lisp files."
  :group 'change-log :prefix 'embedded-changelog-)

(defcustom embedded-changelog-left-margin 2
  "Left margin to use with embedded change logs.

If nil, use the default left margin for change logs (typically 8).
See also `left-margin'."
  :group 'embedded-changelog
  :type '(choice
	  integer
	  (const :tag "Use default for change logs" nil)))

(defcustom embedded-changelog-add-code-section 'ask
  "Control separation of the code section from the history one.
If nil, never add a code section automatically.
If t, add it to separate source code from the history section.
If `ask', ask the user interactively about this."
  :group 'embedded-changelog
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Add silently" t)
		 (const :tag "Ask the user" ask)))

(defun narrow-to-embedded-changelog (&optional add-section)
  "Narrow to the change log embedded in the current emacs-lisp file.

With optional argument ADD-SECTION force the adding of a new
'History:' section in the comments if it does not already exists.
If an \"History\" section in the current Emacs Lisp file does not
exist already, a new one is created just before the \"Code\" section
or before the first non top level comment line."
  (interactive)
  (let ((section-found nil))
    (save-excursion
      (goto-char (point-min))
      (goto-char
       (or (save-excursion
	     (and (search-forward-regexp "^;;; History:\n" nil t)
		  (progn (setq section-found t) (point))))
	   (save-excursion
	     ;; put this before the code section, if it exists
	     (and (search-forward-regexp "^;;; Code:\n" nil t)
		  (match-beginning 0)))
	   (save-excursion
	     ;; put this on the first non-comment, non-empty line
	     (while (or (looking-at "^[ \t\n]*$")
			(looking-at "^;"))
	       (forward-line))
	     (and
	      add-section
	      (case embedded-changelog-add-code-section
		((ask)
		 (y-or-n-p
		  "Add a Code section after the History one? "))
		((t) t))
	      (save-excursion
		(insert ";;; Code:\n\n")))
	     (point))
	   (point-max)))
      (cond
       (section-found
	(narrow-to-region
	 (point)
	 (min
	  (save-excursion
	    (or (and (search-forward-regexp "^;;; " nil t)
		     (match-beginning 0))
		(point-max)))
	  (save-excursion
	    (or (and (search-forward-regexp "^[^;]" nil t)
		     (match-beginning 0))
		(point-max))))))
       (add-section
	(progn
	  (insert ";;; History:\n")
	  (save-excursion (insert "\n"))
	  (narrow-to-region (point) (point))))
       (t (narrow-to-region (point) (point)))))))

(defun goto-embedded-changelog ()
  "Move to the beginning of the embedded change log.

See also `narrow-to-embedded-changelog'."
  (interactive)
  (save-restriction
    (narrow-to-embedded-changelog t)))

(defvar embedded-changelog-buffer nil)
(make-variable-buffer-local 'embedded-changelog-buffer)
(defvar embedded-changelog-elisp-buffer nil)
(make-variable-buffer-local 'embedded-changelog-elisp-buffer)

;;;###autoload
(defun embedded-changelog-add-entry
  (&optional whoami changelog-buffer other-window new-entry)
  "Edit the embedded change log in a separate buffer.

The optional argument CHANGELOG-BUFFER specifies an alternate buffer
for editing the change log.
The optional arguments WHOAMI OTHER-WINDOW and NEW-ENTRY behave like
in `add-change-log-entry'.
See also `narrow-to-embedded-changelog'."
  (interactive)
  (let ((elisp-buffer (current-buffer))
	(new-fill-column (- fill-column 3))
	(new-buffer-file-name
	 (concat buffer-file-name "!Changelog")))
    (when (and embedded-changelog-buffer
	       (buffer-live-p embedded-changelog-buffer))
      (setq changelog-buffer embedded-changelog-buffer))
    (unless changelog-buffer
      (setq changelog-buffer (get-file-buffer new-buffer-file-name))
      (when changelog-buffer
	(with-current-buffer changelog-buffer
	  (unless (eq embedded-changelog-elisp-buffer
		    elisp-buffer)
	    (setq changelog-buffer nil)))))
    (unless changelog-buffer
      (setq changelog-buffer
	    (generate-new-buffer
	     (concat "Changelog (" (buffer-name elisp-buffer)
		     ")")))
      (save-excursion
	(save-restriction
	  (narrow-to-embedded-changelog)
	  (with-current-buffer changelog-buffer
	    (insert-buffer-substring elisp-buffer))))
      (set-buffer changelog-buffer)
      (goto-char (point-min))
      (while (search-forward-regexp "^;+ ?" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (change-log-mode)
      (setq
       embedded-changelog-elisp-buffer elisp-buffer
       buffer-file-name new-buffer-file-name
       buffer-file-truename (abbreviate-file-name buffer-file-name)
       fill-column new-fill-column)
      (when embedded-changelog-left-margin
	(setq left-margin embedded-changelog-left-margin))
      (add-hook 'write-contents-hooks 'embedded-changelog-write-file)
      (set-buffer elisp-buffer)))
  (setq embedded-changelog-buffer changelog-buffer)
  (add-change-log-entry
   whoami
   (with-current-buffer changelog-buffer
     buffer-file-name)
   other-window new-entry))

(defun embedded-changelog-write-file ()
  (let ((changelog-buffer (current-buffer)))
    (with-current-buffer embedded-changelog-elisp-buffer
      (save-excursion
	(save-restriction
	  (narrow-to-embedded-changelog t)
	  (delete-region (point-min) (point-max))
	  (insert-buffer-substring changelog-buffer)
	  (goto-char (point-min))
	  (while (and (search-forward-regexp "^")
		      (not (eq (point) (point-max))))
	    (replace-match ";; "))))))
  (set-buffer-modified-p nil)
  t)

(provide 'embedded-changelog)
(provide 'embeddedchangelog)
;;; embeddedchangelog.el ends here
