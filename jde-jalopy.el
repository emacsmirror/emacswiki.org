;;; jde-jalopy.el --- JALOPY interface for JDE

;; Copyright (C) 2006 Nascif A. Abousalh Neto

;; Author: Nascif A. Abousalh Neto <nascif at acm dot org>
;; Maintainer: Nascif A. Abousalh Neto <nascif@acm.org>
;; Keywords: java, beautifier, pretty printer, tools
;; Time-stamp: <2006-08-14 12:13:39 naabou>
;; 
;; Version: 1.5
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
;; Jalopy (see http://jalopy.sourceforge.net/).  Jalopy is a `a source code
;; formatter for the Sun Java programming language. It layouts any valid Java
;; source code according to some widely configurable rules; to meet a certain
;; coding style without putting a formatting burden on individual developers.'

;; Installation:
;;
;;  1) Download and install the Jalopy console plug-in package from:
;;  http://jalopy.sourceforge.net/download.html
;;
;;  2) Put this file on your Emacs-Lisp load path and add the following into
;;  your .emacs startup file
;;
;;      (require 'jde-jalopy)
;;
;;  3) Customize the variable `jde-jalopy-option-path' to point to the Jalopy
;;  installation directory.
;;
;;  4) Make sure JDE is properly configured. In particular set the variables
;;  jde-jdk-registry and jde-jdk so that the JVM launcher can be found.

;;; Usage:
;;
;;  M-x `jde-jalopy-file' to reformat the Java source file associated to the
;;  current buffer.
;;
;;  M-x `jde-jalopy-buffer' to reformat the contents of the current buffer.
;;

;;; Customization:
;;
;;  To customize the Jalopy-Emacs interface options, use the command:
;;  M-x `jde-jalopy-customize' 
;;
;;  The default behavior for Jalopy is to format the code according to the Sun
;;  Code conventions. If you want to customize the formatting behavior (and
;;  Jalopy supports a large number of customization options), use the command:
;;  M-x `jde-jalopy-preferences'
;;
;;  This command will launch the Jalopy Preferences editor GUI-based tool. You
;;  can use it to create a file with your customized preferences. Point the
;;  variable `jde-jalopy-option-preferences-file' to this file. I suggest you
;;  save the file in the XML format, so that you can edit the preferences file
;;  directly in an Emacs buffer later.

;;; Acknowledgements:
;;
;; This code is heavily based on jde-check.el, by Markus Mohnen.

;;; ChangeLog:
;;  1.5 - Fixed incompatibility with Emacs CVS read-only compilation mode problem;
;;        Removed [INFO] tags introduced by Jalopy;
;;        Cleaned up code a little bit.
;;
;;  1.4 - added changes and new features by Paul Landes
;;
;;  1.3 - jde-jalopy-buffer added, thanks to Bob Schellink
;;
;;  1.2 - jde-jalopy-buffer renamed to jde-jalopy-file to clarify its behavior.
;;
;;  1.1 - updates to support Jalopy 1.0b10 (changes in location of jar files)
;;
;;  1.0 - first version, supporting Jalopy 1.0b8

;;; Code:

(require 'jde-compile)

(if (fboundp 'jde-build-classpath)
    nil
  (require 'jde-run)
  (defalias 'jde-build-classpath 'jde-run-build-classpath-arg)
  )

(defgroup jde-jalopy nil
  "JDE Jalopy Options"
  :group 'jde
  :prefix "jde-jalopy-option-")

(defcustom jde-jalopy-option-class "de.hunsicker.jalopy.plugin.console.ConsolePlugin"
  "*Jalopy console plug-in class.  Specifies the Jalopy console plug-in
class. There is typically no need to change this variable."
  :group 'jde-jalopy
  :type 'string)

(defcustom jde-jalopy-option-preferences-class 
  "de.hunsicker.jalopy.swing.SettingsDialog"
  "*Jalopy console plug-in class.  Specifies the Jalopy console plug-in
class. There is typically no need to change this variable."
  :group 'jde-jalopy
  :type 'string)

(defcustom jde-jalopy-option-path ""
  "*Specify installation path of the Jalopy Console plug-in.  This path will be
used to find the Jalopy .jar files in order to construct a -classpath argument
to pass to the Java interpreter. This option overrides the
`jde-global-classpath' option."
  :group 'jde-jalopy
  :type 'string)

(defcustom jde-jalopy-option-read-args nil
  "*Specify whether to prompt for additional jalopy arguments.
If this variable is non-nil, the jde-jalopy command prompts
you to enter additional jalopy arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The JDE maintains a history list of arguments 
entered in the minibuffer."
  :group 'jde-jalopy
  :type 'boolean)

(defvar jde-interactive-jalopy-args ""
  "String of jalopy arguments entered in the minibuffer.")

(defcustom jde-jalopy-option-command-line-args ""
  "*Specify options as a string of command-line arguments.  The value of this
variable should be a string of switches understood by Jalopy. This variable is
intended to be used to set options not otherwise defined by this version
of jde-jalopy."
  :group 'jde-jalopy
  :type 'string)

(defcustom jde-jalopy-option-encoding "Cp1252"
  "*Encoding of input files. Must be one of the JDK supported encodings."
  :group 'jde-jalopy
  :type 'string)

(defcustom jde-jalopy-option-format "AUTO"
  "*Output file format - one of UNIX, DOS, MAC, AUTO (the default) or DEFAULT
(all case-insensitive)."
  :group 'jde-jalopy
  :type 'string)

(defcustom jde-jalopy-option-force nil
  "*Force format even if file is up-to-date." 
  :group 'jde-jalopy
  :type 'boolean)  

(defcustom jde-jalopy-option-nobackup nil
  "*If true disable the creation of keep backup files."
  :group 'jde-jalopy
  :type 'boolean)

(defcustom jde-jalopy-option-preferences-file ""
  "*Preferences file. Used to configure the many Jalopy options on how to format
the code."
  :group 'jde-jalopy
  :type 'file)

(defcustom jde-jalopy-option-thread 1
  "*Specifies the number of processing threads to be used. This settingg should be
equal to the number of processors your system has."
  :group 'jde-jalopy
  :type 'integer)

(defun jde-jalopy-get-java ()
  "Returns the name of the JVM launcher application."
  (if (string< jde-version "2.2.9")
      (if (eq system-type 'windows-nt)
          jde-run-java-vm-w
        jde-run-java-vm)
    (oref (jde-run-get-vm) :path)))

(defun jde-jalopy-get-options ()
  "Constructs a command-line argument string for jalopy.
The string consists of the contents of the jde-jalopy-options
variable concatenated with the various jde-jalopy-option
settings.
"
  (let (options)
    (if (not (string= jde-jalopy-option-encoding ""))
        (setq options (concat options " --encoding=" jde-jalopy-option-encoding)))
    (if (not (string= jde-jalopy-option-format ""))
        (setq options (concat options " --format=" jde-jalopy-option-format)))
    (if jde-jalopy-option-force
        (setq options (concat options " --force")))
    (if jde-jalopy-option-nobackup
        (setq options (concat options " --nobackup")))
    (if (not (string= jde-jalopy-option-preferences-file ""))
        (setq options (concat options " --style=" 
                              (jde-normalize-path jde-jalopy-option-preferences-file))))
    (if (not (= jde-jalopy-option-thread 1))
        (setq options (concat options " --thread=" 
                              (int-to-string jde-jalopy-option-thread))))
    
    (if (not (string= jde-jalopy-option-command-line-args ""))
        (setq options (concat options " " jde-jalopy-option-command-line-args)))
    options))

(defun jde-jalopy-get-classpath-value ()
  "Returns a string with the classpath used to call the Jalopy console plug-in.
   This is just the value, without quotes or the '-classpath' option tag
"
  (let ((classpath))
    (if jde-jalopy-option-path
        (let
            ((save-jde-expand-classpath-p jde-expand-classpath-p)
             (save-jde-lib-directory-names jde-lib-directory-names))
          (setq jde-expand-classpath-p t)
          (setq jde-lib-directory-names (list "^lib"))
          (setq classpath
                (jde-build-classpath (list (jde-jalopy-get-install-path))))
          (setq jde-expand-classpath-p save-jde-expand-classpath-p)
          (setq jde-lib-directory-names save-jde-lib-directory-names))
      (if (and (boundp 'jde-global-classpath)
               jde-global-classpath)
          (setq classpath
                (jde-build-classpath jde-global-classpath))))
    classpath))

(defun jde-jalopy-get-classpath ()
  "Returns a string with the classpath used to call the Jalopy console plug-in.
   It contains the '-classpath' option tag and the classpath value is quoted.
   Use when passing the command string to a shell for evaluation.
"
  (concat " -classpath \'"
          (jde-jalopy-get-classpath-value)
          "\'"))



(defun jde-jalopy-get-install-path ()
  (concat jde-jalopy-option-path
          (if (string-match ".*[/\\]$" jde-jalopy-option-path)
              "" 
            (if (eq system-type 'windows-nt) "\\" "/"))
          "lib"))

;;;###autoload
(defun jde-jalopy-customize ()
  "Customization of the group jde-jalopy."
  (interactive)
  (customize-group "jde-jalopy"))

(defun jde-jalopy-make-command (more-args)
  "Constructs the java jalopy command as: jde-jalopy + options + buffer file name."
  (concat 
   (jde-jalopy-get-java)
   (jde-jalopy-get-classpath)
   (if (not (string= more-args "")) (concat " " more-args))
   " "
   jde-jalopy-option-class 
   " "
   (jde-jalopy-get-options) 
   " "   
   (file-name-nondirectory buffer-file-name)))

(defun jde-jalopy-make-preferences-command ()
  "Constructs the java jalopy Preferences command."
  (concat 
   (jde-jalopy-get-java)
   (jde-jalopy-get-classpath)
   " "
   jde-jalopy-option-preferences-class))

;;;###autoload
(defun jde-jalopy-file ()
  "Formats the Java source code in the current buffer.
This command invokes the code formatter specified by `jde-jalopy-class'
with the options specified by the JDE customization variables
that begin with `jde-jalopy'. If the variable
`jde-option-read-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled."
  (interactive)

  (if jde-jalopy-option-read-args
      (setq jde-interactive-jalopy-args
            (read-from-minibuffer 
             "Jalopy args: "
             jde-interactive-jalopy-args
             nil nil
             '(jde-interactive-jalopy-arg-history . 1))))

  (let ((jalopy-command
         (jde-jalopy-make-command jde-interactive-jalopy-args)))
    
    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-jalopy from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
             (not jde-xemacsp)) 
        (let ((temp last-nonmenu-event))
          ;; The next line makes emacs think that jde-jalopy
          ;; was invoked from the minibuffer, even when it
          ;; is actually invoked from the menu-bar.
          (setq last-nonmenu-event t)
          (save-some-buffers (not compilation-ask-about-save) nil)
          (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))
    (compile-internal jalopy-command "No more errors")))

(defun jde-jalopy-preferences ()
  "Launches the Jalopy Preferences editor."
  (interactive)

  (let ((jalopy-command
         (jde-jalopy-make-preferences-command)))
    
    (compile-internal jalopy-command "No more errors")))

;; following methods by  Bob Schellink 
;; http://www.mail-archive.com/jde@sunsite.dk/msg07153.html

(defun jde-jalopy-buffer ()
  "Sends the current buffer to jalopy for beautifying.
  This is a interactive function that sends the contents 
  of the current buffer to jalopy for compilation."
  (interactive)

  (save-some-buffers nil nil)

  (if jde-jalopy-option-read-args
      (setq jde-interactive-jalopy-args
            (read-from-minibuffer
             "Jalopy args: "
             jde-interactive-jalopy-args
             nil nil
             '(jde-interactive-jalopy-arg-history . 1))))
  (let ((path-sep (if (eq system-type 'windows-nt)
                      "\\" ;;windows file seperator
                    "/")) ;;unix file seperator
        (jalopy-compilation-buffer "*jalopy-compilation*"))
    (let ((source-file-name (buffer-name))
          (jalopy-script-home (concat jde-jalopy-option-path path-sep "bin"))
          (start (point-min))
          (end (point-max))
          (source-file-point (point))
          (legal-buffer (if (buffer-file-name)
                            "true"
                          nil))
          (jalopy-command
           (jde-jalopy-make-arg (jde-jalopy-trim
                                 jde-interactive-jalopy-args)))
          )

      (if (get-buffer jalopy-compilation-buffer)
          (kill-buffer jalopybuf))
      (setq jalopybuf  (get-buffer-create jalopy-compilation-buffer))
      (copy-to-buffer jalopybuf (point-min) (point-max))
      (save-excursion
        (set-buffer jalopybuf)
        (if legal-buffer
            (compilation-mode jalopy-compilation-buffer))
        (setq buffer-read-only nil)
        (apply 'call-process-region
               start
               end
               (jde-jalopy-get-java)
               t
               t
               nil
               jalopy-command
               )
        (jde-jalopy-setup-buffer)
        (if (not legal-buffer)
            (progn
              (message (concat "The compiled window might not work correctly, because "
                               "the buffer you are trying to beautify is not associated with any "
                               "file."))))
        )
      (goto-char source-file-point)
      )
    )
  )

(defun jde-jalopy-setup-buffer()
  "Function that sets up the jalopy buffer.
   If any errors occurred, a seperate compilation window 
   is displayed. Else the current buffers content are replaced 
   by the beautified content."
  (let ((jalopy-compilation-ind "Listing on stdin\.\.\.\n\[ERROR\]")
        (jalopy-ok-ind "Listing on stdin\.\.\.")
        (jalopy-info-re "^\\[INFO\\]"))

    (goto-char (point-min))
    ;;if string was found, this is a jalopy file with errors
    (if (search-forward jalopy-compilation-ind nil t)
        (progn
          (forward-line -1)
          (beginning-of-line)
                                        ; (kill-line) ;;kill the containing the jalopy string
          (kill-line) ;;kill the containing the jalopy string
          (setq jde-display-command)
          (setq jalopy-command (reverse jalopy-command))
          (while jalopy-command
            (setq jde-display-command (concat (car jalopy-command) " "
                                              jde-display-command))
            (setq jalopy-command (cdr jalopy-command))
            )
          (insert-string (concat (jde-jalopy-get-java) " " jde-display-command
                                 "\n\n"))
          (replace-string "System.in" source-file-name)
          (goto-char (point-min))
          (setq buffer-read-only t)
          (switch-to-buffer-other-window jalopy-compilation-buffer)
          )
      (if (search-forward jalopy-ok-ind nil t) 
          (progn
            ;;if string was found, this is a jalopy file with NO errors"
            (beginning-of-line)
            (kill-line) ;;kill the line containing the jalopy string
            (let ((tmp-point (point)))
              (goto-char (point-min))
              (beginning-of-line)
              (if (looking-at jalopy-info-re)
                  (progn ;;; found first info
                         (kill-line))) ;;kill the line containing the jalopy string
              (goto-char (point-max))
              (if (search-backward-regexp jalopy-info-re nil t) 
                  (progn ;;; found second info
                         (kill-line))) ;;kill the line containing the jalopy string
              (goto-char tmp-point))
            (copy-to-buffer source-file-name (point-min) (point-max))
            (set-buffer source-file-name)
            (kill-buffer jalopy-compilation-buffer)
            )
        (message "Error occurred") ;;else this is a jalopy file with errors
        (switch-to-buffer-other-window jalopy-compilation-buffer)
        ))))

(defun jde-jalopy-make-arg (more-args)
  "Constructs the java jalopy command as: jde-jalopy + options + buffer file name."
  (let* ((jde-tmp-jalopy-options (split-string (jde-jalopy-get-options)))
         (tmp-list (append
                    (list
                     "-classpath"
                     (jde-jalopy-get-classpath-value)                     
                     (if (not (string= more-args "")) (concat more-args))                     
                     jde-jalopy-option-class
                     )
                    jde-tmp-jalopy-options
                    )))                                 
         (delete "" tmp-list))
  )

(defun jde-jalopy-trim(value)
  "Trim leading and trailing whitespace."
  (if (string-match "\\`[ \t\n]+" value)
      (setq value (substring value (match-end 0))))
  (if (string-match "[ \t\n]+\\'" value)
      (setq value (substring value 0 (match-beginning 0))))
  )

(provide 'jde-jalopy)

;;; jde-JALOPY.EL ends here
