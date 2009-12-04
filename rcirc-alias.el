;;; rcirc-alias.el -- provide a local alias for nicks
;; Copyright 2009  Alex Schroeder

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This is specially useful if using Bitlbee to add multiprotocol
;; Instant Messaging to IRC.

;; Use /ALIAS to list all aliases.
;; Use /ALIAS ALIAS to list all nicks for an alias.
;; Use /ALIAS ALIAS NICK1 NICK2 to define an alias for some nicks.

;; When using /NAMES all nicks will be replaced by their alias, and
;; duplicates will be eliminated.

;;; Todo:

;; Make sure nicks are only used in a single alias when defining them.
;; Use a defcustom for rcirc-alias. Allow TAB completion for
;; aliases. Transparently translate any aliases used to the
;; appropriate nick based on current server. Provide a command to
;; remove nicks (remove a nick when it is prefixed by a minus).

;;; Code:

(defvar rcirc-alias nil
  "An alist mapping nicks to aliases.
These aliases cross server boundaries.")

(defun rcirc-alias-reverse ()
  "Return an inverse map of `rcirc-alias'."
  ;; We're assuming no duplicate definitions.
  (let (result)
    (mapc (lambda (element)
	    (let ((alias (car element))
		  (nicks (cdr element)))
	      (dolist (nick nicks)
		(setq result (cons (cons nick alias) result)))))
	  rcirc-alias)
    result))

(defun-rcirc-command alias (args)
  "Use ALIAS for any NICKS listed.
If no nicks are given, print the nicks associated with the alias.
If no alias is given, print the list of aliases defined."
  (interactive)
  (setq args (split-string args))
  (rcirc-do-alias (car args) (cdr args) process target))

(defun rcirc-alias-report (text)
  "Send TEXT as a notice for the current user, process, and target.
`process' and `target' are inherited via dynamic binding."
  (rcirc-print process (rcirc-nick process) "NOTICE" target text))

(defun rcirc-do-alias (alias nicks process target)
  "Implement /ALIAS."
  ;; FIXME: Make sure each nick only appears in a single alias.
  (cond ((not alias)
	 (if rcirc-alias
	     (dolist (alias rcirc-alias)
	       (rcirc-alias-report (mapconcat 'identity alias " ")))
	   (rcirc-alias-report "No aliases defined")))
	((null nicks)
	 (let ((nicks (cdr (assoc alias rcirc-alias))))
	   (if nicks
	       (rcirc-alias-report (mapconcat 'identity nicks " "))
	     (rcirc-alias-report (concat "Unknown alias " alias)))))
	(t
	 (let* ((list (cdr (assoc alias rcirc-alias)))
		(original list)
		(num 0))
	   (dolist (nick nicks)
	     (when (not (member nick list))
	       (setq list (cons nick list)
		     num (1+ num))))
	   (if original
	       (setcdr (assoc alias rcirc-alias) list)
	     (setq rcirc-alias (cons (cons alias list) rcirc-alias)))
	   (if original
	       (if (= num 0)
		   (rcirc-alias-report
		    (concat "Nothing new to add to " alias))
		 (rcirc-alias-report (concat "Added to " alias)))
	     (rcirc-alias-report (concat "Defined " alias)))))))

;; fix /NAMES

(defadvice rcirc-handler-353 (after rcirc-handler-aliases activate)
  "Replace all nicks known under an alias as the alias only."
  (let ((channel (caddr args))
	(reverse (rcirc-alias-reverse))
	found)
    (with-current-buffer (rcirc-get-temp-buffer-create process channel)
      (with-syntax-table rcirc-nick-syntax-table
	(goto-char (point-min))
	(while (re-search-forward "\\w+" nil t)
	  (let* ((nick (match-string 0))
		 (alias (cdr (assoc nick reverse)))
		 (seen (member alias found)))
	    (when alias
	      (if seen
		  (replace-match "")
		(replace-match alias)
		(setq found (cons alias found)))
	      (when (string-match (char-to-string (char-before))
				  rcirc-nick-prefix-chars)
		(delete-char -1)
		(just-one-space)))))))))

;; (ad-deactivate 'rcirc-handler-353)

(provide 'rcirc-alias)

;;; rcirc-alias.el ends here
