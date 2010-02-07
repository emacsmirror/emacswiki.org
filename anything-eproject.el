;;; anything-eproject.el --- Anything integration for eproject.

;; Copyright (C) 2009 Daniel Hackney

;; Author: Daniel Hackney <dan@haxney.org>
;; Keywords: convenience project

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Allows opening and closing eproject projects through anything, as well as
;; selection of files within a project.
;;
;; To activate, add anything-eproject.el to your load path and
;;
;;   (require 'anything-eproject)
;;
;; to your .emacs. You can then use the following sources
;;
;;  `anything-c-source-eproject-files'
;;    Search for files in the current eProject.
;; `anything-c-source-eproject-projects'
;;    Open or close eProject projects.
;;
;;
;; Eproject: http://www.emacswiki.org/emacs/eproject
;; Anything: http://www.emacswiki.org/emacs/Anything

;;; Code:

(defvar anything-c-source-eproject-files
  '((name . "Files in eProject")
    (init . anything-c-source-eproject-files-init)
    (candidates-in-buffer)
    (type . file)
    (real-to-display . (lambda (real)
                         (if real
                             (cadr (split-string real
                                                 (concat
                                                  (expand-file-name (cadr prj-current)) "/")))))))
  "Search for files in the current eProject.")

(defun anything-c-source-eproject-files-init ()
  "Build `anything-candidate-buffer' of eproject files."
  (with-current-buffer (anything-candidate-buffer 'local)
    (mapcar
     (lambda (item)
       (insert (format "%s/%s\n" (cadr prj-current) (car item))))
     prj-files)))

(defvar anything-c-source-eproject-projects
  '((name . "Projects")
    (candidates . (lambda ()
                    (mapcar (lambda (item)
                              (car item))
                            prj-list)))
    (action ("Open Project" . (lambda (cand)
                                (eproject-open cand)))
            ("Close projcet" . (lambda (cand)
                                 (eproject-close)))))
  "Open or close eProject projects.")

(provide 'anything-eproject)

;;; anything-eproject.el ends here
