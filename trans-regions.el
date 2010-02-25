;;; trans-regions.el --- Transpose regions command.

;; Copyright (C) 2010  Taiki SUGAWARA

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: convenience
;; Version: 0.02
;; Time-stamp: <2010-02-24 19:48:52 UTC taiki>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/trans-regions.el
;; URL: http://bitbucket.org/buzztaiki/elisp/src/tip/trans-regions.el
;; Compatibility: GNU Emacs 22, 23


;; This program is free software; you can redistribute it and/or modify
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

;; This package provides transpose regoins command.
;; 
;; Usage is very simple, type M-x `trans-regions' and follow a
;; mode-line information.

;;; Todo;

;; - what doing when invoke trans-regoins twice?
;; - transposing is incorrect when first regions is bobp.

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar trans-regions-recursive-editing-p nil)
(make-local-variable 'trans-regions-recursive-editing-p)

(defun trans-regions ()
  "Transpose regions command."
  (interactive)
  (if (and (> (recursion-depth) 0)
	   trans-regions-recursive-editing-p)
      (progn
	;; TODO: decide? restart?
	(error "trans regions is in progress."))
    (apply 'transpose-regions (trans-regions-read)))
  (when (interactive-p)
    (trans-regions-unhighlight)))

(defun trans-regions-select (msg)
  (let ((orig-fmt mode-line-format))
    (unwind-protect
	(progn
	  (setq mode-line-format
		(apply
		 'format "Select %s region. decide: \"%s\", abort: \"%s\""
		 msg
		 (loop for x in '(exit-recursive-edit abort-recursive-edit)
		       collect (substitute-command-keys
				(format "\\[%s]" (symbol-name x))))))
	  (deactivate-mark)
	  (trans-regions-recursive-edit)
	  (list (region-beginning) (region-end)))
      (setq mode-line-format orig-fmt))))

(defun trans-regions-read ()
  (condition-case err
      (append
       (destructuring-bind (beg end)
	   (if (trans-regions-use-region-p)
	       (list (region-beginning) (region-end))
	     (trans-regions-select "first"))
	 (trans-regions-highlight beg end 'highlight))
       (trans-regions-select "second"))
    (quit (trans-regions-unhighlight)
	  (signal (car err) (cdr err)))))

(defun trans-regions-highlight (beg end face)
  (let ((ovr (make-overlay beg end)))
    (dolist (x `((trans-regions . t)
		 (face . ,face)
		 (evaporate . t)))
      (overlay-put ovr (car x) (cdr x))))
  (list beg end))

(defun trans-regions-unhighlight ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'trans-regions t))

(defun trans-regions-recursive-edit ()
  (setq trans-regions-recursive-editing-p t)
  (unwind-protect
      (recursive-edit)
    (setq trans-regions-recursive-editing-p nil)))
  
;; borrowed from emacs23's simple.el
(defun trans-regions-use-region-p ()
  (and (and transient-mark-mode mark-active)
       (> (region-end) (region-beginning))))

(provide 'trans-regions)
;;; trans-regions.el ends here
