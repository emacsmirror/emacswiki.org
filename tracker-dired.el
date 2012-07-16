;;; tracker-dired.el --- run a tracker-search query and dired the output

;; Copyright (C) 1992, 1994, 1995 Free Software Foundation, Inc.

;; Author: Roland McGrath <roland (at) gnu. ai. mit. edu>,
;;	   Sebastian Kremer <sk (at) thp. uni-koeln. de>
;;         Maarten Grachten <maarten. grachten (at) gmail. com>
;; Maintainer: FSF
;; Keywords: unix

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'dired)
(require 'dbus)
(require 'gnus-util)

(defun djcb-call-tracker (method &rest args)
  "call the tracker method METHOD with ARGS over dbus"
  (apply 'dbus-call-method 
    :session                            ; use the session (not system) bus
    "org.freedesktop.Tracker1"                  ; service name
    "/org/freedesktop/Tracker1/Resources"   ; path name
    "org.freedesktop.Tracker1.Resources"    ; interface name
    method args))

(defun search (query)
  (let ((results (djcb-call-tracker 
		  "SparqlQuery" 
		  (concat "SELECT nie:url(?f) WHERE { ?f fts:match '" 
			  query 
			  "' . ?f nie:mimeType ?m . FILTER ( fn:contains(?m,'text/') )}"))))
	(mapcar '(lambda (x) 
		   (concat " '" 
			   (substring (gnus-url-unhex-string (car x) nil) 7) 
			   "'"))
		results)))

(defgroup tracker-dired nil
  "Run a tracker-search query and dired the output."
  :group 'dired
  :prefix "tracker-")

;; find's -ls corresponds to these switches.
;; Note -b, at least GNU find quotes spaces etc. in filenames
;;;###autoload
(defcustom tracker-ls-option
  (if (eq system-type 'berkeley-unix) '("-ls" . "-gilsb")
    '("-exec ls -ld {} \\;" . "-ld"))
  "*Description of the option to `find' to produce an `ls -l'-type listing.
This is a cons of two strings (TRACKER-OPTION . LS-SWITCHES).  TRACKER-OPTION
gives the option (or options) to `find' that produce the desired output.
LS-SWITCHES is a list of `ls' switches to tell dired how to parse the output."
  :type '(cons (string :tag "Find Option")
	       (string :tag "Ls Switches"))
  :group 'tracker-dired)

;;;###autoload

(defvar tracker-queries nil
  "Last queries given to Tracker by \\[tracker-dired].")

;; History of tracker-queries values entered in the minibuffer.
(defvar tracker-queries-history nil)

;;;###autoload
(defun tracker-dired (search-term)
  "Run `find' and go into Dired mode on a buffer of the output.
The command run (after changing into DIR) is

    find . \\( ARGS \\) -ls

except that the variable `tracker-ls-option' specifies what to use
as the final argument."
  (interactive (list (read-string "Search terms: " tracker-queries
				  '(tracker-queries-history . 1))))
  (let ((results (search search-term)))
	
    (switch-to-buffer (get-buffer-create "*Tracker-Results*"))
    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq tracker-queries search-term)

    (dired-mode)

    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "g" 'undefined)
    ;; Set subdir-alist so that Tree Dired will work:
    
    (if (fboundp 'dired-simple-subdir-alist)
	;; will work even with nested dired format (dired-nstd.el,v 1.15
	;; and later)
	(dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm) 
      (set (make-local-variable 'dired-subdir-alist)
	   (list (cons default-directory (point-min-marker)))))

    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    ;; ``wildcard'' line. 
    (insert "Tracker search results for \"" search-term 
	    "\" (" (format "%d" (length results)) " matches)\n\n")
    ;; Start the process that pipes the results of the tracker search through ls -ld.
    (let ((proc (start-process-shell-command "Tracker Search" (current-buffer) 
					     (concat "ls -ld" (apply 'concat results)))))
      (set-process-filter proc (function tracker-dired-filter))
      (set-process-sentinel proc (function tracker-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) 1 (current-buffer)))
    (setq mode-line-process '(":%s"))))

(defun tracker-dired-filter (proc string)
  ;; Filter for \\[tracker-dired] processes.
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)		; not killed?
	(save-excursion
	  (set-buffer buf)
	  (save-restriction
	    (widen)
	    (save-excursion
	      (let ((buffer-read-only nil)
		    (end (point-max)))
		(goto-char end)
		(insert string)
		(goto-char end)
		(or (looking-at "^")
		    (forward-line 1))
		(while (looking-at "^")
		  (insert "  ")
		  (forward-line 1))
		;; Convert ` ./FILE' to ` FILE'
		;; This would lose if the current chunk of output
		;; starts or ends within the ` ./', so back up a bit:
		(goto-char (- end 3))	; no error if < 0
		(while (search-forward " ./" nil t)
		  (delete-region (point) (- (point) 2)))
		;; Find all the complete lines in the unprocessed
		;; output and process it to add text properties.
		(goto-char end)
		(if (search-backward "\n" (process-mark proc) t)
		    (progn
		      (dired-insert-set-properties (process-mark proc)
						   (1+ (point)))
		      (move-marker (process-mark proc) (1+ (point)))))
		))))
      ;; The buffer has been killed.
      (delete-process proc))))

(defun tracker-dired-sentinel (proc state)
  ;; Sentinel for \\[tracker-dired] processes.
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)
	(save-excursion
	  (set-buffer buf)
	  (let ((buffer-read-only nil))
	    (save-excursion
	      (goto-char (point-max))
	      (insert "\nTracker search " state)
	      (forward-char -1)		;Back up before \n at end of STATE.
	      (insert " at " (substring (current-time-string) 0 19))
	      (forward-char 1)
	      (setq mode-line-process
		    (concat ":"
			    (symbol-name (process-status proc))))
	      ;; Since the buffer and mode line will show that the
	      ;; process is dead, we can delete it now.  Otherwise it
	      ;; will stay around until M-x list-processes.
	      (delete-process proc)
	      (force-mode-line-update)))
	  (message "tracker-dired %s finished." (current-buffer))))))

(provide 'tracker-dired)

;;; tracker-dired.el ends here
