;;; bug.el -- A simple, alternative interface to the ant command line

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;
;; Author: Jason Meade (jemeade@gmail.com)
;; Last Update: July 26, 2006
;; Version: 1.1
;;

;;
;; Module: bug.el
;;
;; bug.el is a very simple wrapper around the more robust compile.el.
;; The purpose behind this module is to allow programmers working with 
;; Java to execute ant scripts against the project build.xml file from
;; anywhere within the project subtree. 
;;
;; bug.el will scan the current buffer's folders while looking for any
;; file named "build.xml". This search is not case sensitive. If the 
;; build.xml file is found, then the command line args are passed to 
;; the compile commands. However, if the build.xml file is not found, 
;; then bug will traverse upwards either until it finds a build.xml file, 
;; or until there are no more upwards directories.
;;
;; Instructions:
;; i.  Copy this file to your site-lisp folder.
;; ii. Add (require 'bug) to your .emacs file.

(require 'compile) ; pass all the real work to this library :)

;;
(defvar bug-dir "")
(defvar bug-command "ant -emacs ")

;;
(defun bug-sees-xml ()
  "Returns true if build.xml is in the current folder"
  (directory-files "." nil "[Bb][Uu][Ii][Ll][Dd]\.[Xx][Mm][Ll]"))

;; TODO:
;; Verify on Windows, especially with UNC pathways on network shares. 
(defun bug-is-root ()
  "Returns true if the current directory is the root directory"
  (let* ((dir default-directory))
    (if (not (null (string-match "^[A-Z]:[\\/]$" dir)))
	t
      (if (not (null (string-match "^/$" dir)))
	  t
	))))
;;
(defun bug-go-up ()
  "Moves up one directory in the hierarchy, unless we are already at root"
  (if (bug-is-root)
      nil
    (progn
      (cd "..")
      (pwd))))

;;
(defun bug-did-find ()
  "Searches the current directory upwards until it finds a build.xml file"
  (if (bug-sees-xml)
      t
    (if (bug-go-up)
	(bug-did-find)
      nil)))

;;
(defun bug ()
  "A simple, alternative interface to the ant command line"
  (interactive
   (let ((command (read-from-minibuffer "Bug command: " bug-command)))
     (setq bug-dir default-directory) ; save our loc so we can return 
     (if (bug-did-find)
	 (progn
	   (compile-internal command "No more errors")
	   (cd bug-dir)
	   nil)
       (progn
	 (cd bug-dir)
	 (message "Cannot find build.xml")
	 nil)))))

;;
(provide 'bug)

;;; bug.el ends here

