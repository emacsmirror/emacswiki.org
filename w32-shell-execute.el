;;; w32-shell-execute.el --- w32 specific.  execute any of the 'Explorer verbs' available for a file or directory

;; Copyright (C) 2002  Patware Unc.

;; Author: Patrick Anderson <patware@freeshell.org>

;; Keywords: convenience, processes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; ChangeLog:
;; Version 1.4: removed dangerous, non-prompting func, removed `w32-shell-execute-app'
;; Version 1.3: fixed UNC bug
;; Version 1.2: added UNC support
;; Version 1.1: refactored, simplified
;; Version 1.0: initial release

;; keys
(define-key global-map [(apps)] 'w32-shell-execute-verb) ;the `apps' key is the 'menu' key between the right-side 'windows' and 'ctrl' keys on a Windows keyboard
(define-key global-map [(control !)] 'w32-shell-execute-verb)

;; code
(defun w32-shell-execute-verb (verb)
  "Call w32-shell-execute on 'current' file (mode dependent).
Choose verb to apply to shell-execute.  Valid verbs may be \"open\",
\"print\", \"explore\" etc.  Right click on a file (in Explorer.exe that is)
to get hints, but the verbs themselves are stored in the registry -
mostly under HKEY_CLASSES_ROOT."
  (interactive "sVerb (press RET for default): ")
  (if (eq major-mode 'dired-mode)
	  (dired-map-over-marks
	   (w32-shell-execute
		verb
		(replace-regexp-in-string ;for UNC paths
		 "/" "\\" (dired-get-filename)nil t))nil)
	(w32-shell-execute
	 verb
	 (replace-regexp-in-string ;for UNC paths
	  "/" "\\" (buffer-file-name) nil t))))

(provide 'w32-shell-execute)
;;; w32-shell-execute.el ends here
