;;; planner-notes-index-page.el -- create index of notes in a particular page
;;
;;
;; Copyright 2008 Thierry Volpiatto

;; This file is not part of planner.

;; planner is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; planner is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with planner; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

;; Title:planner-notes-index-page.el
;; Author:ThierryVolpiatto
;; Contact: thierry dot volpiatto from gmail dot com


;; INSTALL
;; =======
;; put this file some where in your load-path
;; (require 'planner-notes-index-page)

;; USAGE
;; =====

;; To index a page in a separate buffer:

;; just call M-x planner-notes-index-page RET
;; You have completion with TAB
;; Set limit when prompted to the number of entries you want or RET with nothing if
;; you want all your index

;; To index a page in the page:

;; set your planner-plan-page-template some thing like this:
;; (setq planner-plan-page-template "* Tasks\n\n\n* Notes-index\n\n\n* Notes\n\n\n")
;; if your project page already exist add * Notes-index in your file (just before * Notes
;; + is a good place)
;; now each time you call M-x planner-notes-index-page-in-page
;; it will erase all index entries or create all index entries (it toggle)
;; like that index are not invasive in your plan page

;; Set keys:

;; (global-set-key (kbd "<f9> i p") 'planner-notes-index-page)
;; (define-key planner-mode-map (kbd "<f9> p i") 'planner-notes-index-page-in-page)
;; NOTE: it's safer to set local key like here for planner-notes-index-page-in-page

;;; Code:

(require 'planner)

;;; Customize

(defgroup planner-notes-index-page nil
  "Provide a way to create notes-index in plan pages"
  :prefix "planner-notes-index-page"
  :group 'planner)

(defcustom planner-notes-index-regexp
  "^\* Notes-index *$"
  "regex to match in plan page; 
the notes-index will be inserted just after that"
  :type 'string
  :group 'planner)

(defvar index-page-statut t
  "used to switch index/noindex in a plan page")

;;;###autoload
;; create index with completion in a separate buffer
(defun planner-notes-index-page (page &optional limit)
  "Create an index of page in a separate buffer, hitting RET
on link will call muse-follow-name-at-point, hitting S-RET
will call muse-follow-name-at-point-other-window"
  (interactive (list (completing-read "IndexPage: "
				      (mapcar (lambda (x) (nth 0 x))
					      (nth 3 (planner-tasks-overview-get-summary))))
		     (read-from-minibuffer "Limit: ")))
  (cd (planner-directory))
  (let ((limit (if (equal limit "")
		   (setq limit nil)))
	(liste-notes (planner-notes-index-headlines-on-page page limit))
	(buffer (concat "*Notes-Index-" page "*")))
    (set-buffer (get-buffer-create buffer))
    (text-mode) (erase-buffer) (insert (concat "* Notes-Index: " page "\n\n"))
    (dolist (x liste-notes)
      (insert (concat "[[" page (car x) "]] " (cdr x) "\n")))
    (goto-char (point-min))
    (planner-mode)
    (display-buffer buffer)))

;;;###autoload
;; basename
(defmacro tv-basename (file-name)
  "mimic basename and return the relative name of file"
  `(file-relative-name ,file-name
		       (file-name-directory ,file-name)))

;;;###autoload
;; create notes-index in the same buffer if regex is matched
(defun planner-notes-index-page-in-page ()
  "Create an index of current page in this page if regex
notes-index is found in this buffer, the index is included just
after notes-index; planner-page-template have to be customized
to use this"
  (interactive)
  (let* ((current-page (planner-current-file))
	 (page (replace-regexp-in-string "\.muse" ""
					 (tv-basename current-page)))
	 (liste-notes (planner-notes-index-headlines-on-page page)))
    (goto-char (point-min))
    (when (re-search-forward planner-notes-index-regexp nil t)
	(forward-line)
	(delete-region (point)
		       (if (re-search-forward "^\\*" nil t)
			   (line-beginning-position)
			 (point-max))))
    (goto-char (point-min))
    (if index-page-statut
	(when (re-search-forward planner-notes-index-regexp nil t)
	  (newline 2)
	  (dolist (x liste-notes)
	    (insert (concat "[[" page (car x) "]] " (cdr x) "\n")))
	  (setq index-page-statut nil))
      (setq index-page-statut t)))
  (goto-char (point-min))
  (when (re-search-forward planner-notes-index-regexp nil t)
    (forward-line)))

(provide 'planner-notes-index-page)

;;; planner-notes-index-page.el ends here
