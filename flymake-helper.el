;;; flymake-helper.el -- flymake helper methods

;; Copyright (C) 2004 Free Software Foundation

;; Author: Nascif A. Abousalh Neto <nascif at acm dot org>
;; Maintainer: Nascif A. Abousalh Neto <nascif at acm.org>
;; Keywords: java, syntax checker, tools
;; Time-stamp: <2005-01-27 10:21:18 naabou>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111, USA.

;;; Commentary:
;;
;; This module provides extensions to Flymake, created by Pavel Kobiakov
;;
;;; Usage:
;;
;; 1) Download and install flymake from 
;;    http://flymake.sourceforge.net/
;; 
;; 2) Add flymake-helper.el to a directory in your load-path
;;
;; 3) Add somewhere in your .emacs
;;    (require 'flymake-helper)
;;
;;; Acknowledgements:
;;
;;; ChangeLog:
;;
;;  0.2 - Adds flymakeh-open-compilation (thanks to Ole Arndt)
;;      - Changes prefix to "flymakeh" to prevent name collisions with the official flymake defuns.
;;
;;  0.1 - Initial version: defines flymake-save-as-kill-err-messages-for-current-line

(require 'flymake)

; flymake helpers
(defun flymakeh-save-as-kill-err-messages-for-current-line()
  "Add to kill ring errors/warnings for current line. Also display error/warnings as Emacs messages (on the minibuffer)"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info (flymake-get-buffer-err-info (current-buffer)) line-no))))
    (flymakeh-save-as-kill-err-data line-err-info-list)
  ))

(defun flymakeh-save-as-kill-err-data(line-err-info-list)
  "Save a list with errors/warnings from line-err-info in the kill ring.
   Also displays them as messages"
  (when line-err-info-list
    (mapcar 'flymakeh-kill-data-from-err-entry line-err-info-list)
    )
  )

(defun flymakeh-kill-data-from-err-entry (line-err-info-entry)
  (let ((err-entry-text (flymake-ler-get-text      line-err-info-entry)))
    (kill-new err-entry-text)
    (message err-entry-text)))


(defun flymakeh-open-compilation (&optional other-files)
  "Open Flymake errors in a compilation buffer.
Shows only errors for the current buffer.
With the optional prefix argument it also shows messages for other files."
  (interactive "P")
  (let ((errs (flymake-get-buffer-err-info (current-buffer)))
	(buf-name (buffer-name))
	(comp-buffer (get-buffer-create "*compilation*")))
    (switch-to-buffer-other-window comp-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (if (null errs)
	(insert "\nCompilation finished at " (current-time-string) "\n")
      (dolist (fileline errs)
	(dolist (errmsg (cadr fileline))
	  (let ((file (nth 0 errmsg))
		(line (nth 1 errmsg))
		(severity (nth 2 errmsg))
		(msg (nth 3 errmsg))
		(path (nth 4 errmsg)))
	    (if (and (or other-files (null file)) (> line 0))
		(insert path ":" (number-to-string line) ":"
			(replace-regexp-in-string "\\`[:0-9]+: \\(Semantic \\|Syntax \\)*" "" msg)
			"\n")))))
      (insert "\nCompilation exited abnormally with code 1 at "
	      (current-time-string) "\n"))
    (compilation-mode)
    (next-error 0 t)))


(provide 'flymake-helper)

; flymake-helper ends here

