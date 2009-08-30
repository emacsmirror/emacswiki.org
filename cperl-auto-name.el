;;; cperl-auto-name.el --- auto name pm buffers

;; Copyright (C) 2009  Pat Regan

;; Author: Pat Regan <thehead@patshead.com>
;; Keywords: files

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

;; Renames buffers based on package name.
;; Not very robust, but it does the job for me.

;;; Code:

(add-hook 'find-file-hook 'cperl-auto-name)

(defun cperl-auto-name ()
  "Set name of buffer to match package name."
  (goto-char (point-min))
  (when (re-search-forward "^package \\([A-Za-z0-9\\:]+\\);" nil t)
    (rename-buffer (match-string 1))
   )
  (goto-char (point-min))
)


(provide 'cperl-auto-name)
;;; cperl-auto-name.el ends here
