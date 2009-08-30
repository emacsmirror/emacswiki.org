;;; ifind.el --- An interface to `find` based on igrep.el
;;; -*-unibyte: t;-*-

;; Copyright © 2003, 2004 Kevin Rodgers

;; Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; Created: 13 Nov 2003
;; Version: $Revision: 1.1 $
;; Keywords: tools, processes, search
;; RCS: $Id: ifind.el,v 1.1 2004/05/12 19:54:37 kevinr Exp kevinr $

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

;; The `ifind' command is like `igrep-find' (which in turn is like
;; `grep-find'), except that it uses the awk program to show just one
;; line of all files whose names match the user-specified regex.

;; A default file name pattern is provided (based on the current
;; buffer), and the user can customize the `ifind-awk-regex' variable to
;; determine which line of each matching file is shown (to provide a
;; peek at its content).

;;; Code:

(require 'custom)			; defcustom
(require 'igrep)			; igrep-find-use-xargs,
					; igrep-read-multiple-files,
					; igrep-read-files,
					; igrep-format-find-command

(defcustom ifind-awk-regex "."		; non-empty line
  "The default (extended) regex matched by `\\[ifind]'."
  :group 'igrep
  :type '(choice (const :tag "Any text" ".") ; non-empty line
		 (const :tag "Non-whitespace" "[^ \t]")
                 (const :tag "RCS/CVS keyword" "\\$[A-Z]+[a-z]+\(: [^$]+\)?\\$")
                 ;; (const :tag "SCCS keyword" "%[A-Z]%")
		 (string :tag "User-defined")) ; other
  :set (lambda (symbol value)
         (set-default symbol value)     ; default
         (set-default 'ifind-awk-command nil))) ; recompute

(defvar ifind-awk-program "awk"
  "The `awk` program used by `\\[ifind]'.")

(defvar ifind-use-xargs igrep-find-use-xargs
  "The value used by `\\[ifind]' for `igrep-find-use-xargs'.")

(defvar ifind-read-multiple-files igrep-read-multiple-files
  "The value used by `\\[ifind]' for `igrep-read-multiple-files'.")

(defvar ifind-awk-command nil
  "The `awk` command invoked by `\\[ifind]' that emulates `grep -n REGEX`.
See `ifind-awk-regex'.")

(defun ifind-awk-command ()
  "Return the `awk` command that emulates `grep -n REGEX`.
See `ifind-awk-program' and `ifind-awk-regex'."
  (format "%s -v OFS=: \
'FNR == 1 {matched = 0}; \
 matched == 0 && /%s/ {print FILENAME, FNR, $0; matched = 1; %s}'"
	  ifind-awk-program
	  ifind-awk-regex
	  (cond ((equal (call-process-region (point-min)
					     (point-min)
					     ifind-awk-program
					     nil nil nil
					     "{nextfile}")
			0)
		 "nextfile")             ; POSIX/GNU awk
		(ifind-use-xargs "next") ; slow, but works
		("exit"))))              ; fast, but wrong when ifind-use-xargs

(defun ifind (files)
  "*Run the `find` program to find FILES.
The output is displayed in the *ifind* buffer, which `\\[next-error]' and
`\\[compile-goto-error]' parse to find the first line of each file.

FILES is either a file name pattern (automatically quoted by
`shell-quote-wildcard-pattern', then expanded by `shell-file-name'),
or a list of file name patterns.  The first line of each file is matched
and printed by `ifind-awk-command'."
  (interactive (let ((igrep-read-multiple-files ifind-read-multiple-files))
		 (list (igrep-read-files "[find] "))))
  (if (not (listp files))		; (stringp files)
      (setq files (list files)))
  (if (null ifind-awk-command)
      (setq ifind-awk-command (ifind-awk-command)))
  (let* ((igrep-find-use-xargs ifind-use-xargs)
	 (command (if ifind-use-xargs
		      ifind-awk-command
		    (concat ifind-awk-command
			    " "
			    (shell-quote-argument "{}")))))
    (compile-internal (igrep-format-find-command command files) "No more files"
		      "ifind" nil grep-regexp-alist)))

(provide 'ifind)
;;; ifind.el ends here
