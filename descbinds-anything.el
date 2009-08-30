;;; descbinds-anything.el --- Yet Another `describe-bindings' with `anything'.

;; Copyright (C) 2008,2009  Taiki SUGAWARA <buzz.taiki@gmail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: anything, help
;; Version: 1.03
;; Time-stamp: <2009-03-29 17:57:12 UTC taiki>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/descbinds-anything.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package is a replacement of `describe-bindings'.

;;; Usage:
;; Add followings on your .emacs.
;;
;;   (require 'descbinds-anything)
;;   (descbinds-anything-install)
;;
;; Now, `describe-bindings' is replaced to `descbinds-anything'. Type `C-h b',
;; `C-x C-h' these run `descbinds-anything'.
;;
;; In the Anything buffer, you can select key-binds with anything interface.
;; When type RET, selected candidate command is executed.
;; When type ESC, You can "Execute", "Describe Function" or "Find Function" by
;; the menu.

;;; History:
;; 2009-03-29 UTC  Taiki SUGAWARA  <buzz.taiki@gmail.com>
;;
;;   * descbinds-anything.el: Version 1.03
;;   fix typo.
;;
;; 2008-11-16 UTC  Taiki SUGAWARA  <buzz.taiki@gmail.com>
;;
;;   * descbinds-anything.el: Version 1.02
;;   bound `indent-tabs-mode` to t for nil environment.
;;
;; 2008-11-16 UTC  Taiki SUGAWARA  <buzz.taiki@gmail.com>
;;
;;   * descbinds-anything.el: fix infinitive-loop when binding-line
;;   has not tab.

;;; Code:

(require 'anything)

(defgroup descbinds-anything nil
  "Yet Another `describe-bindings' with `anything'."
  :group 'anything)

(defcustom descbinds-anything-actions
  '(("Execute" . descbinds-anything-action:execute)
    ("Describe Function" . descbinds-anything-action:describe)
    ("Find Function" . descbinds-anything-action:find-func))
  "Actions of selected candidate."
  :type '(repeat
	  :tag "Actions"
	  (cons
	   :tag "Action"
	   (string :tag "Name")
	   (function :tag "Function"))))

(defcustom descbinds-anything-candidate-formatter
  'descbinds-anything-default-candidate-formatter
  "Candidate formatter function.
This function called two argument KEY and BINDING."
  :type '(function :tag "Formatter"))

(defcustom descbinds-anything-window-style 'one-window
  "Window splitting style."
  :type '(choice
	  :tag "Styles"
	  (const :tag "One Window" one-window)
	  (const :tag "Same Window" same-window)
	  (const :tag "Split Window" split-window)))

(defun descbinds-anything-all-sections (buffer &optional prefix menus)
  (with-temp-buffer
    (let ((indent-tabs-mode t))
      (describe-buffer-bindings buffer prefix menus))
    (goto-char (point-min))
    (let ((header-p (not (= (char-after) ?\f)))
	  sections header section)
      (while (not (eobp))
	(cond
	 (header-p
	  (setq header (buffer-substring-no-properties (point) (line-end-position)))
	  (setq header-p nil)
	  (forward-line 3))
	 ((= (char-after) ?\f)
	  (push (cons header (nreverse section)) sections)
	  (setq section nil)
	  (setq header-p t))
	 ((looking-at "^[ \t]*$")
	  ;; ignore
	  )
	 (t
	  (let ((binding-start (save-excursion
				 (and (re-search-forward "\t+" nil t)
				      (match-end 0))))
		key binding)
	    (when binding-start
	      (setq key (buffer-substring-no-properties (point) binding-start)
		    key (replace-regexp-in-string"^[ \t\n]+" "" key)
		    key (replace-regexp-in-string"[ \t\n]+$" "" key))
	      (goto-char binding-start)
	      (setq binding (buffer-substring-no-properties binding-start (line-end-position)))
	      (unless (member binding '("self-insert-command"))
		(push (cons key binding) section))))))
	(forward-line))
      (push (cons header (nreverse section)) sections)
      (nreverse sections))))

(defun descbinds-anything-action:execute (candidate)
  "An action that execute selected CANDIDATE command."
  (call-interactively (cdr candidate)))

(defun descbinds-anything-action:describe (candidate)
  "An action that describe selected CANDIDATE function."
  (describe-function (cdr candidate)))

(defun descbinds-anything-action:find-func (candidate)
  "An action that find selected CANDIDATE function."
  (find-function (cdr candidate)))

(defun descbinds-anything-default-candidate-formatter (key binding)
  "Default candidate formatter."
  (format "%-10s\t%s" key binding))

(defun descbinds-anything-sources (buffer &optional prefix menus)
  (mapcar
   (lambda (section)
     (list
      (cons 'name (car section))
      (cons 'candidates (cdr section))
      (cons 'candidate-transformer
	    (lambda (candidates)
	      (mapcar
	       (lambda (pair)
		 (cons (funcall descbinds-anything-candidate-formatter
				(car pair) (cdr pair))
		       (cons (car pair)
			     (intern-soft (cdr pair)))))
	       candidates)))
      (cons 'action descbinds-anything-actions)
      (cons 'action-transformer
	    (lambda (actions candidate)
	      (and (commandp (cdr candidate))
		   actions)))))
   (descbinds-anything-all-sections buffer prefix menus)))

(defun descbinds-anything (&optional prefix buffer)
  "Yet Another `describe-bindings' with `anything'."
  (interactive)
  (let ((anything-sources (descbinds-anything-sources
			   (or buffer (current-buffer))
			   prefix nil))
	(anything-samewindow (and (not (minibufferp))
				  (memq descbinds-anything-window-style
					'(same-window one-window))))
	(anything-before-initialize-hook
	 (if (and (not (minibufferp))
		  (eq descbinds-anything-window-style 'one-window))
	     (cons 'delete-other-windows anything-before-initialize-hook)
	   anything-before-initialize-hook)))
    (anything)))

(defvar descbinds-anything-Orig-describe-bindings
  (symbol-function 'describe-bindings))

(defun descbinds-anything-install ()
  "Use `descbinds-anything' as a replacement of `describe-bindings'."
  (interactive)
  (fset 'describe-bindings 'descbinds-anything))

(defun descbinds-anything-uninstall ()
  "Restore original `describe-bindings'."
  (interactive)
  (fset 'describe-bindings descbinds-anything-Orig-describe-bindings))

(provide 'descbinds-anything)
;;; descbinds-anything.el ends here
