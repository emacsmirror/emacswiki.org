;;; jswat.el --- JDEE interface to JSwat

;; Copyright (C) 2002 Nascif A. Abousalh Neto

;; Author: Nascif A. Abousalh Neto <nascif at acm dot org>
;; Maintainer: Nascif A. Abousalh Neto
;; Keywords: java, debugging, tools
;; Time-stamp: <2004-04-28 14:17:25 naabou>
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from the Free
;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:
;; This library implements an Emacs interface to JSwat
;; (http://www.bluemarsh.com/java/jswat/), based heavily on the JDEE
;; (http://jdee.sunsite.dk/rootpage.html) configuration. The idea is to save time by
;; re-using the configuration of one tool (JDEE debugger) to use another similar one
;; (JSwat). The target audience is designers that have JDEE
;; installed and configured, but want to play with a different debugger.
;;
;; Usage: (assuming JSwat and JDEE are already installed and configured):
;; 1) Add this library to a directory visible from your load-path
;; 2) Add to your .emacs (after the lines that enable JDEE) the line
;;       (require 'jswat)
;; 3) Customize the variable "jswat-path" to point to the path of your JSwat installation
;; 4) Your probably don't have to do that, but you may need (in future versions of
;;    JSwat) to customize "jswat-main-class" to reflect changes in the JSwat main class
;;    name
;; 5) After loading a Java file (with an associated JDEE project file),
;;    just call it interactively:
;;       M-x jswat
;;
;; Problems:
;; - Probably won't work with paths that contain a space.
;;
;; Log:
;; Version 0.2
;; - 04/28/2004 11:48 Added quotes to classpath command argument to support
;;                    execution on Windows (where path separator is the same as 
;;                    JSwat command separator)
;; Version 0.1
;; - 11/11/2002 11:27 Changes to support JSwat 2.10 and JDEE 2.2.9beta10,
;;                    testing on Solaris,
;;                    removed support for older JDKs (JPDA assumed on tools.jar)
;;
;; - 24/11/2001 19:29 Added support for Jde 2.2.9X vm variables
;;

(require 'jde)

(defgroup jswat nil
  "JSwat Options"
  :group 'tools
  :prefix "jswat-")

(defcustom jswat-path nil
  "Path to JSwat installation directory"
  :group 'jswat
  :type 'file)

(defcustom jswat-main-class "com.bluemarsh.jswat.Main"
  "Main class used to launch JSwat"
  :group 'jswat
  :type 'string)

(defun jswat ()
  "Invokes JSwat using the setup derived from the current JDE session.
   Assumes that Emacs is configured to use a Unix-like (e.g. bash) shell"
  (interactive)
  ;; Java interpreter
  (progn
    (jswat-check-dependencies)
    (jswat-launch (jswat-make-command))))

(defun jswat-check-dependencies ()
  "Verifies that required variables are defined"
  (if (null jswat-path) 
      (error "jswat-path undefined, use customize-variable to set it"))
  (if (or (null jde-run-application-class) (string= jde-run-application-class ""))
      (error "jde-run-application-class undefined, use customize-variable to set it"))
  (if (null jde-sourcepath)
      (error "jde-sourcepath undefined, use customize-variable to set it"))
  (if (and (null jde-global-classpath) (null jde-db-option-classpath))
      (error "Application classpath undefined, use customize-variable to set either jde-global-classpath or jde-db-option-classpath"))
)

(defun jswat-launch (command)
  (let* ((buffer (get-buffer-create "*JSwat*")))
    (set-buffer buffer)
    (erase-buffer)
    ;; insert complete command in JSwat buffer (for debugging of jswat.el only)
    (insert command "\n")
    ;; change to the application runtime directory
    (cd (jde-normalize-path jde-run-working-directory))
    ;; build assynch command and launch JSwat
    (setq form (nconc (list 'start-process "jswat-process" buffer)
                      (split-string command " ")))
    (eval form)))

(defun jswat-make-command ()
  (let* ((java-vm (if (string< jde-version "2.2.9")
                      (if (eq system-type 'windows-nt)
                          jde-run-java-vm-w
                        jde-run-java-vm)
                    (oref (jde-run-get-vm) :path)))
         (jpda-jar (jde-get-tools-jar))
         (jswat-classpath (append (list jpda-jar) 
                                  (jswat-jars)))
         (proj-sourcepath (mapcar 'jswat-remove-last-slash jde-sourcepath)))
    ;; Insert vm call
    (setq command java-vm)
    ;;Insert jswat classpath
    (setq command (concat command " "
                          (jde-build-classpath-arg 'jswat-classpath)))
    ;; Insert sourcepath
    (if proj-sourcepath
        (setq command (concat command " -Djava.source.path="
                              (jswat-replace-path-separator
                               (jde-build-classpath proj-sourcepath)))))
    ;; Insert JSwat main class
    (setq command (concat command " " jswat-main-class))

    ;;;; Build JSwat commands

    (setq command (concat command (jswat-build-app-classpath)))

    ;; JSwat load main class command
    (setq command (concat command " load"))
    ;;Insert classic VM toggle for the debugee VM
    (if jde-db-option-vm-args
        (setq command (concat command " "
                              (mapconcat (lambda (vm-arg) vm-arg)
                                         jde-db-option-vm-args
                                         " "))))
    ;; Insert debugee vm  arguments
    (if (not (null jde-db-option-properties))
        (setq command (concat command " "
                              (mapconcat (lambda (prop-name-value)
                                           (concat "-D" (car prop-name-value) "=" (cdr prop-name-value)))
                                         jde-db-option-properties
                                         " "))))
    ;; Insert application main class and application arguments
    (setq command (concat command " " jde-run-application-class " "
                          (mapconcat (lambda (path) path)
                                     jde-db-option-application-args
                                     " ")))))

(defun jswat-build-app-classpath ()
  "JSwat application classpath setting"
  (interactive)
  (let ((proj-classpath (mapcar 'jswat-remove-last-slash (if jde-db-option-classpath
                                                            jde-db-option-classpath
                                                          jde-global-classpath))))
    (concat " classpath \""
            (jde-build-classpath proj-classpath)
                                        ; the semicolon is JSwat command separator
            "\";")))

(defun jswat-jars ()
  "returns the list of .jar files in the jswat install dir, read from the custom variable jswat-path"
  (mapcar
   (lambda (path)
     (jde-normalize-path path))
   (directory-files jswat-path t "\\.jar$")))


(defun jswat-replace-path-separator (string)
  "replace path separators to match the OS style
   TODO: make more generic, current does only Unix->Windows"
  (if (eq system-type 'windows-nt)
      (jswat-replace-chars-in-string string ?/ ?\\)
    string))

;;; From Steve Kemp's small functions
(defun jswat-replace-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0))
    ;; Replace all occurrences of FROM with TO.
    (while (< idx len)
      (when (= (aref string idx) from)
	(aset string idx to))
      (setq idx (1+ idx)))
    string))

(defun jswat-remove-last-slash (path)
  "Remove last slash from path if present"
  (let ((len (length path)))
    (if (> len 0)
        (let ((last (1- len)))
          (if (or (= (aref path last) ?\\)
                  (= (aref path last) ?/))
              (substring path 0 last)
            path))
      path)))

(provide 'jswat)

;; jswat.el ends here

