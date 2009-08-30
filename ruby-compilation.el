;;; ruby-compilation.el --- run a ruby process in a compilation buffer

;; Copyright (C) 2008 Eric Schulte

;;; License:

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Allow for execution of ruby processes dumping the results into a
;; compilation buffer.  Useful for executing tests, or rake tasks
;; where the ability to jump to errors in source code is desirable.
;;
;; The functions you will probably want to use are
;; 
;; ruby-run-w/compilation
;; ruby-rake-w/compilation
;;

;;; Code:
(require 'ansi-color)
(require 'pcmpl-rake)

(defvar ruby-compilation-error-regexp
  "^\\([[:space:]]*\\|.*\\[\\|.*at \\)\\[?\\([^[:space:]]*\\):\\([[:digit:]]+\\)[]:)]"
  "regular expression to match errors in ruby process output")

(defvar ruby-compilation-error-regexp-alist
  `((,ruby-compilation-error-regexp 2 3))
  "a version of `compilation-error-regexp-alist' to be used in
  rails logs (should be used with `make-local-variable')")

(defun ruby-run-w/compilation (cmd)
  "Run a ruby process dumping output to a ruby compilation buffer."
  (interactive "FRuby Comand: ")
  (let ((name (file-name-nondirectory (car (split-string cmd))))
	(cmdlist (cons "ruby" (ruby-args-to-list (expand-file-name cmd)))))
    (pop-to-buffer (ruby-do-run-w/compilation name cmdlist))))

(defun ruby-rake-w/compilation (&optional edit task)
  "Run a rake process dumping output to a ruby compilation buffer."
  (interactive "P")
  (let* ((task (or task (if (stringp edit) edit)
		   (completing-read "Rake: " (pcmpl-rake-tasks))))
	 (rake-args (if (and edit (not (stringp edit)))
			(read-from-minibuffer "Edit Rake Command: " (concat task " "))
		      task)))
    (pop-to-buffer (ruby-do-run-w/compilation
		    "rake" (cons "rake"
				 (ruby-args-to-list rake-args))))))

(defun ruby-do-run-w/compilation (name cmdlist)
  (let ((comp-buffer-name (format "*%s*" name)))
    (unless (comint-check-proc comp-buffer-name)
      (if (get-buffer comp-buffer-name) (kill-buffer comp-buffer-name))
      (let* ((buffer (apply 'make-comint name (car cmdlist) nil (cdr cmdlist)))
	     (proc (get-buffer-process buffer)))
	(save-excursion
	  (set-buffer buffer) ;; set buffer local variables and process ornaments
	  (set-process-sentinel proc 'ruby-compilation-sentinel)
	  (set-process-filter proc 'ruby-compilation-insertion-filter)
	  (set (make-local-variable 'compilation-error-regexp-alist)
	       ruby-compilation-error-regexp-alist)
	  (set (make-local-variable 'kill-buffer-hook)
	       (lambda ()
		 (let ((orphan-proc (get-buffer-process (buffer-name))))
		   (if orphan-proc
		       (kill-process orphan-proc)))))
	  (compilation-minor-mode)
	  (ruby-compilation-minor-mode))))
    comp-buffer-name))

(defun ruby-compilation-insertion-filter (proc string)
  "Insert text to buffer stripping ansi color codes"
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
	(insert (ansi-color-filter-apply string))
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun ruby-compilation-sentinel (proc msg)
  "Notify to changes in process state"
  (message "%s - %s" proc (replace-regexp-in-string "\n" "" msg)))

(defun ruby-compilation-previous-error-group ()
  "Jump to the start of the previous error group in the current
compilation buffer."
  (interactive)
  (compilation-previous-error 1)
  (while (string-match ruby-compilation-error-regexp (thing-at-point 'line))
    (forward-line -1))
  (forward-line 1) (recenter))

(defun ruby-compilation-next-error-group ()
  "Jump to the start of the previous error group in the current
compilation buffer."
  (interactive)
  (while (string-match ruby-compilation-error-regexp (thing-at-point 'line))
    (forward-line 1))
  (compilation-next-error 1) (recenter))

;; minor mode
(defvar ruby-compilation-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Key map for Ruby Compilation minor mode.")

(define-key ruby-compilation-minor-mode-map "p"    'previous-error-no-select)
(define-key ruby-compilation-minor-mode-map "n"    'next-error-no-select)
(define-key ruby-compilation-minor-mode-map "\M-p" 'ruby-compilation-previous-error-group)
(define-key ruby-compilation-minor-mode-map "\M-n" 'ruby-compilation-next-error-group)
(define-key ruby-compilation-minor-mode-map (kbd "C-c C-c") 'comint-interrupt-subjob)

(define-minor-mode ruby-compilation-minor-mode
  "Enable Ruby Compilation minor mode providing some key-bindings
  for navigating ruby compilation buffers."
  nil
  " Ruby:Comp"
  ruby-compilation-minor-mode-map)

(provide 'ruby-compilation)
;;; ruby-compilation.el ends here
