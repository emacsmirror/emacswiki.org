;;; w32-shell-verb.el --- ShellExecute verbs on a file

;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author: Patrick Anderson 
;; Version: 1a

;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Press the 'apps' key (the 'menu' key between the right 'windows' and
;; 'ctrl' keys on a windows keyboard) to ShellExecute the default 'verb'
;; on that file (typically 'open').

;; install:
;; this file in your load path

;; add
;; (require 'w32-shell-verb)
;; to your .emacs file

;; todo:
;; show available verbs
;; show 'properties' page
;; show real 'context' menu

;;; Code:

;;; w32-shell-verb.el 
(defun w32-shell-execute-verb (&optional verb)
  "Call w32-shell-execute on 'current' file (mode dependent)."
  (interactive)
  (if (eq major-mode 'dired-mode)
	  (dired-map-over-marks (w32-shell-execute verb (dired-get-filename)) ())
	(w32-shell-execute verb (buffer-file-name))))

(defun w32-shell-execute-buffer-choose (verb)
  "Choose verb to apply to shell-execute.  Valid verbs may be \"open\",
\"print\", \"explore\" etc.  Right click on a file (in Explorer.exe that is) to get hints, but the
verbs themselves are stored in the registry - mostly under HKEY_CLASSES_ROOT."
  (interactive "sVerb: ")
  (w32-shell-execute-verb verb))


(define-key global-map [(apps)] 'w32-shell-execute-verb)
(define-key global-map [(shift apps)] 'w32-shell-execute-buffer-choose)

(provide 'w32-shell-verb)
;;; w32-shell-verb.el ends here
