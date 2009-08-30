;;; jde-lint.el --- lint4j interface for JDE

;; Copyright (C) 2005 Nascif A. Abousalh Neto

;; Author: Nascif A. Abousalh Neto <nascif at acm dot org>
;; Keywords: java, code checker, tools
;; Time-stamp: <2005-01-26 11:23:14 naabou>
;; 
;; Version: 0.2
;;
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

;;; Commentary:
;; This package provides an interface from JDE (see http://jde.sunsite.dk/) to
;; Lint4j (see http://www.jutils.com/). Lint4j ("Lint for Java") is a static
;; Java source code analyzer that detects locking and threading issues,
;; performance and scalability problems, and checks complex contracts such as
;; Java serialization by performing type, data flow, and lock graph analysis.

;; Installation:
;;
;;  1) Download and install the Lint4J tool from http://www.jutils.com/download.html
;;
;;  2) Put this file on your Emacs-Lisp load path and add the following into
;;  your .emacs startup file
;;
;;      (require 'jde-lint)
;;
;;  3) Customize the variable `jde-lint-option-path' to point to the Lint4j
;;  installation directory.
;;
;;  4) Make sure JDE is properly configured. In particular set the variables
;;  jde-jdk-registry and jde-jdk so that the JVM launcher can be found.

;;; Usage:
;;
;;  M-x `jde-lint' to analyse the current project
;;

;;; Customization:
;;
;;  To customize the jde-lint interface options, use the command:
;;  M-x `jde-lint-customize' 
;;

;;; Acknowledgements:
;;

;;; ChangeLog:
;;  0.2 - adding missing require statement
;;  0.1 - first version, supporting Lint4j 0.7.1

;;; Code:

(require 'jde-compile)
(require 'jde-package)

(if (fboundp 'jde-build-classpath)
    nil
  (require 'jde-run)
  (defalias 'jde-build-classpath 'jde-run-build-classpath-arg)
  )

(defconst jde-lint-version "0.7.1")

(defgroup jde-lint nil
  "JDE Lint4j Options"
  :group 'jde
  :prefix "jde-lint-option-")

(defcustom jde-lint-option-home "c:/home/bin/lint4j-0.7.1"
  "Directory where lint4j is installed."
  :group 'jde-lint
  :type 'string)

(defcustom jde-lint-option-appjar "lint4j.jar"
  "*Lint4j jar file. There is typically no need to change this variable."
  :group 'jde-lint
  :type 'string)

(defcustom jde-lint-option-jvmargs ""
  "*Additional options to the Java VM, for example \"-Xms100M -Xmx200M\" for larger heap size."
  :group 'jde-lint
  :type 'string)

(defcustom jde-lint-option-level "3"
  "*Specify the verbosity of the emitted warnings. Range is from severe (1) to suggestion (5)."
  :group 'jde-lint
  :type 'string)

;; -exact    Display only warnings with the same severity as specified with -v. Default is false. Optional.
;; -exclude  Exclude the given packagename in the analysis

(defcustom jde-lint-option-target ""
  "*Package or packages (space separated) to be analysed. Wildcards '*' can be used as the last token, for example com.*"
  :group 'jde-lint
  :type 'string)

(defun jde-lint-get-java ()
  "Returns the name of the JVM launcher application."
  (if (string< jde-version "2.2.9")
      (if (eq system-type 'windows-nt)
          jde-run-java-vm-w
        jde-run-java-vm)
    (oref (jde-run-get-vm) :path)))

(defun jde-lint-jar ()
  (concat "-jar " jde-lint-option-home "/jars/" jde-lint-option-appjar))

(defun jde-lint-package-name ()
  (let* (
         ;; The JDE always uses ?/ as directory separator so ensure
         ;; [X]Emacs uses the same one when running on Windows!
         (directory-sep-char ?/)
         (package-name (jde-package-get-package-directory)))
    (cond
     ((string= package-name jde-package-unknown-package-name)
      (error
       "The current directory is not accessible from classpath."))
     ((string= package-name "")
      (error
       "Default package now supported."))
     (t
      (jde-package-convert-directory-to-package package-name)))))

(defun jde-lint-vmoptions ()
  (if (not (string=  jde-lint-option-jvmargs ""))
      (concat "-J \"" jde-lint-option-jvmargs "\"")
    ""))

(defun jde-lint-level ()
   (if (not (string=  jde-lint-option-level ""))
       (concat "-v " jde-lint-option-level)
     ""))

(defun jde-lint-classpath ()
  "Returns a string with the classpath used to run the lint4j tool.
"
  (concat "-classpath \'" (jde-build-classpath jde-global-classpath) "\'"))

(defun jde-lint-sourcepath ()
  (concat "-sourcepath \'" (mapconcat (lambda (x) (jde-normalize-path x)) jde-sourcepath ";") "\'"))

(defun jde-lint-target ()
  (if (string=  jde-lint-option-target "")
    (jde-lint-package-name)
    jde-lint-option-target))

(defun jde-lint-customize ()
  "Customization of the group jde-lint."
  (interactive)
  (customize-group "jde-lint"))

(defun jde-lint ()
  "Run lint4j on the customized target.
"
  (interactive)
  (or (eq major-mode 'jde-mode)
      (error "Invalid major mode found.  Must be `jde-mode'"))

  (let ((jde-lint-command
         (concat (jde-lint-get-java) " " 
                 (jde-lint-jar) " "
                 (jde-lint-vmoptions) " "
                 (jde-lint-level) " "
                 (jde-lint-sourcepath) " "
                 (jde-lint-classpath) " "
                 (jde-lint-target))
         ))
    
    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    (if 
        (and (eq system-type 'windows-nt) 
             (not jde-xemacsp))
        (let ((temp last-nonmenu-event))
          ;; The next line makes emacs think that jde-lint
          ;; was invoked from the minibuffer, even when it
          ;; is actually invoked from the menu-bar.
          (setq last-nonmenu-event t)
          (save-some-buffers (not compilation-ask-about-save) nil)
          (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))
    (compile-internal jde-lint-command "No more errors" "jde-lint")))

(provide 'jde-lint)

;;; jde-lint.el ends here
