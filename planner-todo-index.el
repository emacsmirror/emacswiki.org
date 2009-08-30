;;; planner-todo-index.el -- find todos in files of project and display to planner page


;; Author: Thierry Volpiatto

;; Copyright (C) 2008 Thierry Volpiatto
;;
;; this file is NOT part of GNU Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

;; You need `TraverseDirectory' for muse installed
;; you can get it here:
;; http://www.emacswiki.org/cgi-bin/wiki/TraverseDirectory

;; USAGE
;; =====

;; 1) Be sure `Traversedirectory' is installed and
;; the option "frontend" is set to muse in .traverse.cfg
;; 2) Put this file in elisp load path and load it
;; 3) Add in your planner-project page "* Todos" BEFORE "* Notes"
;; WARNING: be aware that if you put "* Todos" After "* Notes"
;; your notes will be erased when you call this code.
;; 4) Add a lisp line on the line just after * Todos
;; It should look like this:
;; * Todos
;; [[lisp:/(planner-todo-index-in-page "directory_project" ".el")][My Todos]]
;; Where .el is the type of files used by the project (optional)
;; Now when you press RET on this line you will get all your todos
;; When you press again RET on the line they will disapear.

;;; Code:

(require 'traverse-dir)

(defgroup planner-todo-index nil
  "Provide a way to have link to all lines in files
of project containing the word TODO"
  :prefix "planner-todo-index"
  :group 'planner)

(defcustom planner-todo-index-regexp
  "^\* Todos *$"
    "regex to match in plan page; 
the todos-index will be inserted just after that"
    :type 'string
    :group 'planner-todo-index)

(defcustom planner-todo-regex
  "TODO"
  "The regex to matche in files"
  :type 'string
  :group 'planner-todo-index)

(defvar todo-status t
  "used to switch todo-index/yes-no in a plan page")

;; I reuse the code of planner-index-page-in-page
(defun planner-todo-index-in-page (directory-project ext-file-project)
  "get a todo list of project in planner"
  (interactive)
  (let* ((current-page (planner-current-file))
	 (page (replace-regexp-in-string "\.muse" ""
					 (tv-basename current-page)))
	 (liste-todos (pygrep-call-traverse directory-project
					    planner-todo-regex
					    ext-file-project)))
    (save-restriction
      (goto-char (point-min))
      (when (re-search-forward planner-todo-index-regexp nil t)
	(forward-line 1)
	(end-of-line)
	(newline)
	(delete-region (point)
		       (if (re-search-forward "^\\*" nil t)
			   (line-beginning-position)
			 (point-max))))
      (goto-char (point-min))
      (if todo-status
	  (when (re-search-forward planner-todo-index-regexp nil t)
	    (forward-line 1)
	    (end-of-line)
	    (newline 3)
	    (pygrep-print-all liste-todos)
	    (setq todo-status nil))
	(setq todo-status t)))
    (goto-char (point-min))
    (when (re-search-forward planner-todo-index-regexp nil t)
      (forward-line))))

(provide 'planner-todo-index)

;;; planner-todo-index.el ends here
