;;; jde-flymake.el -- an extension to flymake that uses JDEE and Jikes

;; Copyright (C) 2004 Free Software Foundation

;; Author: Nascif A. Abousalh Neto <nascif at acm dot org>
;; Maintainer: Nascif A. Abousalh Neto <nascif at acm.org>
;; Keywords: java, syntax checker, tools
;; Time-stamp: <2004-06-17 14:43:27 naabou>
;; Version: 0.3

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111, USA.

;;; Commentary:
;;
;; This module provides extensions to Flymake, created by Pavel Kobiakov, to
;; allow the JDEE configuration and the jikes compiler to be used for the
;; on-the-fly syntax checking of Java files. 
;;
;; Flymake comes with built-in support for Java but relies on Makefiles for
;; calling Jikes; this module relies instead on JDEE project variables, removing
;; the need to create Makefiles just for the purpose of using Flymake.
;;
;; The advantages of re-using the JDEE configuration are obvious, as it
;; eliminates the redundancy of keeping the extra configuration required for the
;; Makefile, specially since Ant is becoming the preferred tool for building
;; Java projects. 
;;
;; Jikes is quite fast, can be configured to not generate class
;; files (which is nice since the purpose of flymake is to do just syntax
;; checking), and the latest version includes adittional semantic tests derived
;; from the "Effective Java" book which are very useful as well. This feature
;; has drawbacks as well as some valid constructors (like empty catch blocks)
;; are flagged as errors.
;;
;;; Usage:
;;
;; 1) Download and install Jikes from
;;    http://www-124.ibm.com/developerworks/opensource/jikes/
;;   1.1) Make sure it is in your path, OR 
;;   1.2) Customize jde-flymake-jikes-app-name
;;
;; 2) Download and install flymake from 
;;    http://flymake.sourceforge.net/
;;   2.1) Make sure you get version 0.3 or later. Version 0.3 is available from
;;        CVS at http://cvs.sourceforge.net/viewcvs.py/flymake
;; 
;; 3) Customize the variable flymake-allowed-file-name-masks so that the mapping
;;    for java files looks like this:
;;    (".+\\.java$" flymake-jde-jikes-java-init flymake-simple-java-cleanup flymake-get-real-file-name)
;;
;; 4) Optionally customize flymake-errline-face and flymake-warnline-face. I
;;    recommend using underline instead of background colors to reduce the
;;    visual noise.
;; 
;; 5) Make sure you have a JDEE project file (usually prj.el) at the root of
;;    your source tree, and that you have defined jde-sourcepath and
;;    jde-global-classpath
;;
;; 6) Enable flymake-mode in the java files for which you want automatic syntax
;;    checking (you can use a load hook or manually activate it using M-x
;;    flymake-mode)
;;
;; 7) (OPTIONAL) Install flymake-helper.el for the
;;    flymake-save-as-kill-err-messages-for-current-line command
;;
;; 8) (OPTIONAL) Install keybindings for the most used flymake commands. For
;;    example:
;;       (define-key jde-mode-map [M-f7]         'flymake-goto-prev-error)
;;       (define-key jde-mode-map [M-f8]         'flymake-goto-next-error)
;;       (define-key jde-mode-map [M-f12]        'flymake-save-as-kill-err-messages-for-current-line)
;;
;;; ChangeLog:
;;
;;  0.3 - Fixed problem with jde-find-project-file when running in Linux/
;;        Thanks to Milan Zimmermann for the bug report
;;  0.2 - Jikes configuration options, thanks to Christopher <plalleme at free.fr>
;;      - Tested with new flymake.el 0.3 (added patches required by jde-flymake.el)
;;
;;  0.1 - Initial version

(require 'jde)
(require 'flymake)

(defgroup jde-flymake nil
  "JDE Jalopy Options"
  :group 'jde
  :group 'flymake
  :prefix "jde-"
  )

;; (makunbound 'jde-sourcepath)
(defcustom jde-flymake-sourcepath nil
  "*List of source directory paths.  it should be a subset of jde-sourcepath.
  if nul try to get `jde-sourcepath'."
  :group 'jde-flymake
  :type '(repeat (file :tag "Path")))

(defcustom jde-flymake-option-jikes-source "1.4"
  "*Specify which Java SDK release the source syntax obeys. For example,
  to compile code with the assert keyword, you would specify -source 1.4. 
  Recognized releases are 1.1 through 1.4. If unspecified, this defaults to 1.3."
  :group 'jde-flymake
  :type 'string)

(defcustom jde-flymake-jikes-app-name "jikes"
  "*Allows an absolute path to be set for the jikes compiler.
  The default is to attempt to load it from the path."
  :group 'jde-flymake
  :type 'string)

;; since this
(defun flymake-jde-jikes-java-init(buffer)
  "Returns the jikes command line for a directly checked source file, use create-temp-f for creating temp copy"
  "Currently uses the directory of the prj.el file as the execution directory for"
  "jikes (root of the source tree). Another option would be to use the package as determined by"
  "jde-package and go up the directory tree the appropriate number of levels (segments in the package name"
  (let* ((jikes-args          nil)
   (source-file-name   (buffer-file-name buffer))
   (prjfile-name        (jde-find-project-file (file-name-directory source-file-name))))
    (if prjfile-name
  (let* ((temp-source-file-name  
                (flymake-init-create-temp-buffer-copy 
                 buffer 
                 'flymake-create-temp-with-folder-structure)))
          (setq jikes-args (flymake-get-jikes-args temp-source-file-name (file-name-directory prjfile-name))))
      (error "JDE project file not found")
      )

    (if jikes-args
  ;; assumes jikes is on the path
  (list jde-flymake-jikes-app-name jikes-args)
                                        ;else
      nil)))

(defun jde-flymake-customize ()
  "Show the jde-javadoc options panel."
  (interactive)
  (customize-group "jde-flymake"))

(defun flymake-get-jikes-args(source-file-name base-dir)
  "create a command line for the jikes syntax check command"
  (progn
    ;; check dependencies
    (if (not jde-sourcepath)
        (error "The variable jde-sourcepath is not configured"))
    (if (not jde-global-classpath)
        (error "The variable jde-global-classpath is not configured"))
    (let* (
     (rt-jar (expand-file-name "jre/lib/rt.jar" (jde-normalize-path (jde-get-jdk-dir))))
     (proj-classpath (jde-build-classpath jde-global-classpath))
     (proj-sourcepath (jde-build-classpath 
           (if (not jde-flymake-sourcepath) 
               jde-sourcepath
             jde-flymake-sourcepath))))
  
      ;;TODO: make jikes flags configurable
      ;;  (list jde-flymake-option-jikes
      (list "-nowrite" "+E" "+D" "+P" "+Pno-naming-convention" "-deprecation" 
      "-source" jde-flymake-option-jikes-source 
      "-bootclasspath" rt-jar 
      "-classpath" proj-classpath
      "-sourcepath" proj-sourcepath
      source-file-name))))

(provide 'jde-flymake)

; jde-flymake ends here

