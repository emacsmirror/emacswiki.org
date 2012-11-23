;;; execute.el --- 

;; Copyright 2012 Christian
;;
;; Author: cnngimenez@gmail.com
;; Version: $Id: execute.el,v 0.0 2012/11/22 16:39:12 cng Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'execute)

;;; Code:

(require 'simple)

(defvar execute-command-alist '()
  "An association list of (command-call . process-object) for all commands executed.")

(defconst execute-list-buffer "*Execute Cmd List*"
  "Name of the execute listing buffer.")

(defun execute-program (command)
  "Execute the COMMAND asynchronously and track it so later can be listed using `execute-list'."
  (interactive (list 
		(read-shell-command "Command?")))
  (let* ((proc-obj 
	  (start-process-shell-command "execute-process" "execute-process-buffer" command))
	 )
    
    (when proc-obj
      ; Add to the alist
      (setq execute-command-alist
	    (cons (cons command proc-obj) execute-command-alist))
      (display-buffer (process-buffer proc-obj))
      )
    )
  )

(defun execute-list ()
  "List all the commands executed."
  (interactive)
  (with-current-buffer (get-buffer-create execute-list-buffer)
    (delete-region (point-min) (point-max))
    (switch-to-buffer (current-buffer))
    (dolist (e execute-command-alist)
      (when (process-status (cdr e))
	(insert (format "%d: '%s' - %s - %s\n" 
			(process-id (cdr e))
			(car e) 
			(process-status (cdr e))
			(process-buffer (cdr e))
			)
		)
	)
      )
    )
  )

(substitute-key-definition 'shell-command 'execute-program (current-global-map))

 
(provide 'execute)

;;; execute.el ends here
