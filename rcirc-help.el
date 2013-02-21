;;; rcirc-help.el -- print doc strings of rcirc commands
;;
;; Copyright 2008-20013  Alex Schroeder <alex@gnu.org>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Use /HELP to list all commands
;; Use /HELP COMMAND show the doc string for rcirc-cmd-COMMAND

;;; Code:

(require 'rcirc)

(defun rcirc-commands ()
  "Return a list of defined IRC commands.
If a command called rcirc-cmd-foo exists, the IRC command /FOO
will be part of the list returned."
  (let ((commands))
    (mapatoms (lambda (sym)
		(let ((name (symbol-name sym)))
		  (when (and (commandp sym)
			     (string= (substring name 0 (min (length name) 10))
				      "rcirc-cmd-"))
		    (setq commands (cons (concat"/" (upcase (substring name 10)))
					 commands))))))
    (sort commands 'string<)))

(defun-rcirc-command help (arg)
  "List rcirc commands or print their doc-string."
  (let* ((sym (intern-soft (concat "rcirc-cmd-" arg)))
	 (msg (and sym (documentation sym)))
	 (blurb (concat
		 "\n\nNote: If PROCESS or TARGET are nil, the values given"
		 "\nby `rcirc-buffer-process' and `rcirc-target' will be used."))
	 (doc (and msg (replace-regexp-in-string blurb "" msg))))
    (cond ((string= arg "")
	   (rcirc-print process (rcirc-nick process) "NOTICE" target
			(mapconcat 'identity (rcirc-commands) " ")))
	  (doc
	   (rcirc-print process (rcirc-nick process) "NOTICE" target doc))
	  (t
	   (rcirc-print process (rcirc-nick process) "NOTICE" target
			(concat "/" (upcase arg) " is undefined"))))))

(provide 'rcirc-help)

;;; rcirc-help.el ends here
