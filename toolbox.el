;;; toolbox.el --- create simple menus in buffers

;; Copyright (C) 2003  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.0
;; Keywords: convenience
;; URL: http://www.emacswiki.org/cgi-bin/emacs?ToolBox

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Run M-x toolbox to create a toolbox buffer.  A toolbox buffer has
;; an ordinary buffer associated with it.  The name of the toolbox
;; buffer will tell you which one it is.  You can save a toolbox as an
;; ordinary file.

;;; Code:

(defvar toolbox-target nil
  "Target of the current toolbox buffer.")

(defvar toolbox-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map emacs-lisp-mode-map)
    (define-key map (kbd "RET") 'toolbox-run)
    (define-key map (kbd "<mouse-2>") 'toolbox-click)
    (define-key map (kbd "C-c C-c") 'toolbox-define)
    map)
  "Keymap for Emacs Lisp mode.")

(defface toolbox-button
  '((t (:weight bold)))
  "Face for toolbox buttons.")

(defun toolbox ()
  "Create a toolbox."
  (interactive)
  (when (eq major-mode 'toolbox-mode)
    (error "This is already a toolbox buffer"))
  (let ((target (current-buffer)))
    (switch-to-buffer-other-window (toolbox-name target))
    (toolbox-mode)
    (toolbox-target target)))

(defun toolbox-name (target)
  "Return the name for the toolbox for TARGET."
  (concat "*Toolbox for " (buffer-name target) "*"))
  
(define-derived-mode toolbox-mode fundamental-mode "Toolbox"
  "Major mode for toolbox buffers.  Use \[toolbox] to create one.
A toolbox buffer allows you to write very simple menus for
a target buffer."
  (eldoc-mode 1))

(defun toolbox-target (target)
  "Switch the toolbox's target to the new buffer TARGET.
This will redefine the toolbox entries."
  (interactive "b")
  (when (stringp target)
    (setq target (get-buffer target)))
  (rename-buffer (toolbox-name target))
  (set (make-local-variable 'toolbox-target) target)
  (toolbox-define))

(defun toolbox-define ()
  "Make every elisp defun clickable.
The elisp defun will be called in the target buffer, if it exists."
  (interactive)
  (message "Toolbox setup...")
  (toolbox-clear-buttons)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\w" nil t)
      (let ((start (match-beginning 0))
	    end command)
	(forward-sexp 1)
	(setq end (point)
	      command (intern (buffer-substring-no-properties start end)))
	(when (commandp command)
	(toolbox-button start end command)))))
  (message "Toolbox setup...Done"))

(defun toolbox-clear-buttons ()
  "Remove all buttons."
  (remove-text-properties (point-min) (point-max)
			  '(mouse-face nil keymap nil face nil)))

(defun toolbox-button (start end sym)
  "Create a button from START to END that will call SYM."
  (put-text-property start end 'face 'toolbox-button)
  (put-text-property start end 'mouse-face 'highlight)
  (put-text-property start end 'keymap toolbox-mode-map)
  (put-text-property start end 'toolbox sym))

(defun toolbox-click (event)
  "Run the command at clicked upon using the mouse.
First argument EVENT is used to set point.  Then
`toolbox-run' is called."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (toolbox-run)))

(defun toolbox-run ()
  "Run the command at point.
This calls the value of the text-property `toolbox' at point.
It should be an interactive elisp function."
  (interactive)
  (let ((func (get-text-property (point) 'toolbox)))
    (cond ((and func toolbox-target)
	   (with-current-buffer toolbox-target
	     (funcall func)))
	  (func
	   (funcall func))
	  (t
	   (error "Nothing to call here")))))

(provide 'toolbox)

;;; toolbox.el ends here.
