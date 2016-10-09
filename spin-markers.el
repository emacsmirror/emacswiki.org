;;; spin-markers.el --- spin around a small set of markers

;; Copyright (C) 2016  Tomas Nordin

;; Author: Tomas Nordin <tomasn@posteo.net>
;; Keywords: convenience, matching, markers, bookmarks

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

;; Functions to define a set of markers (bookmarks in some editors)
;; and spin (cycle) around them. The user-level functions are
;; `spin-marker-add' and `spin-marker-jump-next'.

;; Here is a suggestion for the .emacs setting:

;; (require 'spin-markers)
;; (define-key global-map (kbd "C-.") 'spin-marker-add)
;; (define-key global-map (kbd "C-,") 'spin-marker-jump-next)

;; Assuming the bindings above, add a marker at some place with `C-.'.
;; Add some more. Spin around them with `C-,'. Landing on a marker
;; that is no longer wanted is removed with `C-u C-.'.

;; The idea of this is to implement something quick and easy for
;; defining a set of markers for the work at hand. There is no
;; persistance between emacs sessions. For example, writing code in
;; one or two files often require focus on two or three places
;; alternately. This is the use-case for spin-markers.

;;; Code:

;;; List of markers to spin at each jump.
(defvar spin-marker-list nil
  "List of markers to spin at each jump.

There should be no reason to manipulate this list manually, use
the command `spin-marker-add' for that.")

;;; A portion of spin-marker-list. The car of this list is the marker
;;; to jump to next.
(setq spin-marker-next-marker-list nil)

(defun spin-marker-add (arg)
  "Push a marker based on current point to the `spin-marker-list'.

With non-nil prefix argument ARG, remove the mark based on point
instead."

  (interactive "P")
  (if arg
      (spin-marker-remove)
    (let ((cur-mark (point-marker)))
      (if (not (member cur-mark spin-marker-list))
	  (progn
	    (push (point-marker) spin-marker-list)
	    (message "pushed %S" (car spin-marker-list)))
	(message "%S already a member" cur-mark)))
    )
  )

(defun spin-marker-remove ()
  "Remove a marker equal to current point."
  ;; (interactive)
  (let ((cur-mark (point-marker)) (mess-fmt "%S not found in list"))
    (if (member cur-mark spin-marker-list)
	(setq mess-fmt "%S removed from list"))
    (setq spin-marker-list (delete cur-mark spin-marker-list))
    (setq spin-marker-next-marker-list (delete cur-mark spin-marker-next-marker-list))
    (message mess-fmt cur-mark)
    )
  )

(defun spin-marker-jump-next ()
  "Jump to the next marker in the list.

Use `spin-marker-add' to add or remove markers from the
`spin-marker-list'"
  (interactive)

  (if (not (car spin-marker-next-marker-list))
      (setq spin-marker-next-marker-list spin-marker-list))
  
  (let ((next-marker (car spin-marker-next-marker-list)))
    (cond ((not next-marker)
	   (message "spin-marker-list empty"))

	  ((not (marker-buffer next-marker))
	   (setq spin-marker-list (delete next-marker spin-marker-list))
	   (setq spin-marker-next-marker-list
		 (delete next-marker spin-marker-next-marker-list))	   
	   (message "%S lost -- removed -- spin more" next-marker))

	  (t
	   (switch-to-buffer (marker-buffer next-marker))
	   (goto-char next-marker)
	   (setq spin-marker-next-marker-list (cdr spin-marker-next-marker-list))))
    )
  )

(provide 'spin-markers)
;;; spin-markers.el ends here
