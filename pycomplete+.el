;;; pycomplete+.el --- complete symbols at point using Pymacs

;; Hacked by: an00na@gmail.com
;; Keywords: python, complete, completion
;; Last updated: Feb 29, 2008

;; Copyright (C) Ling Wang

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:

;; Replace pycomplete.el included in python-mode with this file, then you'll
;; have a more intelligent python code completion.

(require 'pymacs)
(require 'python-mode)

(pymacs-load "pycomplete")

(defun py-complete ()
  (interactive)
  (let ((window (get-buffer-window "*Completions*" 0)))
    (if (and (eq last-command this-command)
	     window (window-live-p window) (window-buffer window)
	     (buffer-name (window-buffer window)))
	;; If this command was repeated, and
	;; there's a fresh completion window with a live buffer,
	;; and this command is repeated, scroll that window.
	(with-current-buffer (window-buffer window)
	  (if (pos-visible-in-window-p (point-max) window)
	      (set-window-start window (point-min))
	    (save-selected-window
	      (select-window window)
	      (scroll-up))))
      (let* ((pymacs-forget-mutability t)
             (pattern (py-symbol-near-point))
             (imports (py-find-global-imports))
             (completion (pycomplete-pycomplete pattern imports)))
                  
        (cond ((not (string= "" completion))
               (insert completion)
               ;; Don't leave around a completions buffer that's out of date.
               (let ((win (get-buffer-window "*Completions*" 0)))
                 (if win (with-selected-window win (bury-buffer)))))
              (t (let ((completion-list (pycomplete-get-all-completions pattern
                                                                        imports))
                       (minibuf-is-in-use
                        (eq (minibuffer-window) (selected-window))))
                   (unless minibuf-is-in-use
                     (message "Making completion list..."))
                   (with-output-to-temp-buffer "*Completions*"
                     (display-completion-list completion-list pattern))
                   (unless minibuf-is-in-use
                     (message "Making completion list...%s" "done")))))))))

(defun py-find-global-imports ()
  (save-excursion
    (let (first-class-or-def imports)
      (goto-char (point-min))
      (setq first-class-or-def
	    (re-search-forward "^ *\\(def\\|class\\) " nil t))
      (goto-char (point-min))
      (setq imports nil)
      (while (re-search-forward
	      "^\\(import \\|from \\([A-Za-z_][A-Za-z_0-9]*\\) import \\).*"
	      nil t)
	(setq imports (append imports
			      (list (buffer-substring
				     (match-beginning 0)
				     (match-end 0))))))
      imports)))

(define-key py-mode-map [C-tab]  'py-complete)

(provide 'pycomplete+)
