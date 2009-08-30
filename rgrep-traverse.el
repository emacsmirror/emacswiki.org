;;; rgrep-traverse.el -- utilise l'interface rgrep pour traverse


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

;; INSTALL:
;; =======

;; Install first the python backend TraverseDirectory
;; Put this file in your load path
;; and just (require 'rgrep-traverse)

;; NOTE: This program use TraverseDirectory as python backend.
;; Be sure to have it working correctly.

;; Code:

(defvar rgrep-py-size-max "1000000")

(defun rg-get-options-line (tree regex only)
  "return a list of all options"
  (let ((options ""))
    (if (equal only "")
	(setq options (split-string (concat
				     " -t "
				     (expand-file-name tree)
				     " -r "
				     regex
				     " -s "
				     rgrep-py-size-max)))
      (setq options (split-string (concat
				   " -t "
				   (expand-file-name tree)
				   " -r "
				   regex
				   " -s "
				   rgrep-py-size-max
				   " -o "
				   only)))
    options)))
		       
;; Main function that call traverse-directory.py
(defun tv-rgrep-py-find (tree regex &optional only)
  "Run traverse-directory.py and write output to a dedicated buffer
in grep-mode to highlight and link every thing"
  (interactive "fTree: \nsRegexp: \nsMatchOnly: ")
  (let* ((options (rg-get-options-line tree regex only))
	(list-result (with-temp-buffer
		       (apply #'call-process "traverse-bin.py" nil t nil
			      options)
		       (buffer-string)))
	(list-line nil))
    (setq list-result (split-string list-result "\n"))
    (if (bufferp (get-buffer "*Traverse-directory*"))
	(progn
	  (kill-buffer "*Traverse-directory*")
	  (set-buffer (get-buffer-create "*Traverse-directory*")))
      (set-buffer (get-buffer-create "*Traverse-directory*")))
    (text-mode)
    (erase-buffer)
    (insert "* Traverse-directory\n\n\n")
    (insert (concat "traverse-bin.py " (mapconcat (lambda (x) x) options " ") "\n\n"))
    (dolist (x list-result)
      (insert (concat x "\n")))
  (switch-to-buffer-other-window "*Traverse-directory*")
  (goto-line (point-min))
  (grep-mode)))

(provide 'rgrep-traverse)

;;; traverse-dir.el ends here
