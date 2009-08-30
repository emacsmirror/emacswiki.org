;;; jde-eclipse-compiler-server.el --- Eclipse compiler as a compile server for JDEE.


;; Copyright (C) 2006-2007 by Suraj Acharya

;; Author: Suraj Acharya <sacharya@cs.indiana.edu>
;; Maintainer: Suraj Acharya <sacharya@cs.indiana.edu>
;; Created: 22 Feb 2006
;; Keywords: flymake jde-mode java eclipse ecj
;; Version 0.2

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:
;; 
;; * Eclipse java compiler
;;
;; This library adds the option of using the eclipse java compiler as
;; a compile server to `jde-compiler'.  See the section for "Using the
;; batch compiler" at
;; http://help.eclipse.org/help32/index.jsp?topic=/org.eclipse.jdt.doc.isv/guide/jdt_api_compile.htm
;; for a description of the eclipse batch compiler and a list of all
;; the "warn" options that it can take.
;;
;; Usage:
;;
;; To use this library, ensure that this file in your load path and
;; add the following code to your .emacs:
;; (require 'jde-eclipse-compiler-server)

;; Customizing jde-compiler after this should give you a buffer that looks like this:

;; Operate on everything in this buffer:
;;  Set for Current Session Save for Future Sessions
;;  Reset Reset to Saved Erase Customization   Finish

;; Jde Compiler: Hide Value
;; Compiler type
;; ( ) javac
;; ( ) javac server
;; ( ) eclipse java compiler server
;;     Path to ecj.jar (or jdt core jar):
;; (*) jikes
;;     Path to jikes: /opt/jikes-1.22/jikes
;;    State: this option has been changed outside the customize buffer.

;; Note the new option for "eclipse java compiler server".  After
;; selecting this option you will also need to specify the location of
;; the eclipse java compiler classes.

;; If you've installed eclipse locally then this is the jdtcore.jar
;; under <eclipse dir>/plugins/org.eclipse.jdt.core_x.x.x/, where
;; x.x.x depends on the version of eclipse you have.

;; If you don't have eclipse you can download just the JDT
;; compiler.  Go to http://download.eclipse.org/eclipse/downloads/ and
;; pick the release you want, the latest release is usually stable
;; enough to use.  Once you get to the downloads page for the release,
;; scroll down to find the link to download the "JDT Core Batch
;; Compiler".  The 1 MB ecj.jar file is all you need to download.

;; Check that you have the correct jar by trying to run the compiler
;; from a command line like so:
;; java -cp <path to jar> org.eclipse.jdt.internal.compiler.batch.Main
;; This should print out a usage message for the "Eclipse Java Compiler".

;; This library changes the format of the jde-compiler variable so you
;; might encounter problems the first time you switch to using this
;; library or the first time you switch back you have a value for
;; jde-compiler customized using the old customizer.

;; If you see a customization buffer like this :

;;  Set for Current Session Save for Future Sessions
;;  Reset Reset to Saved Erase Customization   Finish
;; 
;; jde-compiler: Hide Value
;; '(("eclipse java compiler server" "/org.eclipse.jdt.core_3.0.0/org.eclipse.jdt.core_3.1.1.jar"))
;;    State: this option has been set and saved.  (mismatch)

;; That is, there are no radio buttons to select, set the value of
;; jde-compiler manually using the minibuffer to its default.  If you
;; are not using jde-eclipse-compiler-server the default is '("javac
;; server" ""), and if you are it is '("javac server")
;;
;; * Flymake
;;
;; Flymake is a minor Emacs mode performing on-the-fly syntax
;; checks. It is included in Emacs 23 onwards. For earlier versions
;; you can get the latest version from
;; http://flymake.sourceforge.net/. It is setup to use the new
;; compile.el from Emacs 23 by default, so you might have to comment
;; out a line which references `compilation-error-regexp-alist-alist'
;; and uncomment the previous line which uses
;; `compilation-error-regexp-alist' instead.
;;
;; You have two options to hook the eclipse java compiler into flymake
;; to get automatic error/warning information in your source buffers:
;; (To use flymake with ecj you first have to select "eclipse java
;; compiler server" as your jdee compiler.)
;;
;; 1) jde-ecj-flymake-init : This is the simple, robust and painfully slow
;;    method, in which flymake forks a new jvm process each time it
;;    decides to error check the buffer.
;; 
;; To use this funtion set the java line in
;; `flymake-allowed-file-name-masks' to 
;; (\"\\.java\\'\" jde-ecj-flymake-init jde-ecj-flymake-cleanup)"

;; 2) jde-ecj-server-flymake-init: This method involves flymake
;;    sending a compile command to the eclipse java compiler server in
;;    the JDEE bsh process, detecting when the compiler is done
;;    printing errors and warnings and then handing control of the bsh
;;    process back to JDEE. This option is much faster than 1) and
;;    compares in speed the jikes flymake integration
;;    (http://www.emacswiki.org/cgi-bin/wiki/jde-flymake.el) but with
;;    java 1.5 syntax support and a larger set of warn
;;    options. However, it requires some significant changes to
;;    `flymake-process-filter' and
;;    `flymake-start-syntax-check-process' and so is more likely to be
;;    flaky, and might cause problems if you use flymake with non-java
;;    buffers.
;;
;; To use this funtion set the java line in
;; `flymake-allowed-file-name-masks' to
;; (\"\\.java\\'\" jde-ecj-server-flymake-init jde-ecj-flymake-cleanup)"
;;
;; The default flymake behaviour is to change the background of error
;; lines to light pink, and for warning lines to light blue. Customize
;; the faces `flymake-errline' and `flymake-warnline' to change this
;; behavior. Red and yellow underlines for errors and warnings work
;; well:
;; 
;; (custom-set-faces
;; ...
;;  '(flymake-errline ((((class color)) (:underline "OrangeRed"))))
;;  '(flymake-warnline ((((class color)) (:underline "yellow"))))
;; ...

;;
;; ChangeLog
;; 0.3 - bug fixes from James Ahlborn <jahlborn@healthmarketscience.com>
;;       deleted unnecessary \n at the end of a bsh-eval string that was causing bsh-buffer-eval to fail occasionally. 
;; 0.2 - Eclipse 3.2 and later support the -Xemacs option which makes
;;       it possible to the hook eclipse compiler into flymake.
;; 0.1 - Initial version

(require 'jde)
(require 'jde-compile)

;;; Code:
(defclass jde-compile-ejc-server (jde-compile-compiler)
  ()
  "Class for using the Eclipse java compiler as a JDEE compile server."
)

(defcustom jde-compiler-new-compile-el
  (boundp 'compilation-error-regexp-alist-alist)
  "Check if we have the new (21.3+) compile.el.
Set this to t if you are running an Emacs with the new compile.el
and want to get slightly better font-locking in the compile
buffer.  A value of nil will force the use of older style
compilation-error-regexp.  This variable tries to auto-detect the
compile.el version by checking if
`compilation-error-regexp-alist-alist' is defined."
  :group 'jde-compile-options
  :type 'boolean)


(if jde-compiler-new-compile-el
    (progn
      (setq compilation-error-regexp-alist
            (cons '("----------\n\\([0-9]+. ERROR in \\(.*\\)\n (at line \\([0-9]+\\))\n\\(\\(.*\n\\)+?\\).*^+\n\\(.*\n\\)\\)"
                    2 3 nil 2 1 (6 compilation-error-face)
                    )
                  compilation-error-regexp-alist))
      
      (setq compilation-error-regexp-alist
            (cons '("----------\n\\([0-9]+. WARNING in \\(.*\\)\n (at line \\([0-9]+\\))\n\\(\\(.*\n\\)+?\\).*^+\n\\(.*\n\\)\\)"
                    2 3 nil 1 1 (6 compilation-warning-face)
                    )
                  compilation-error-regexp-alist)))
  ;; else
  (setq compilation-error-regexp-alist
        (cons '("----------\n[0-9]+. \\(ERROR\\|WARNING\\) in \\(.*\\)\n (at line \\([0-9]+\\))\n\\(\\(.*\n\\)+?\\).*^+\n\\(.*\n\\)"
                2 3)
              compilation-error-regexp-alist)))


(defmethod jde-compile-run-server ((this jde-compile-ejc-server))
    (let* ((directory-sep-char ?/)
	   (args
	    (append
             (list
              "-Xemacs"
              "-noExit"
;;               "-sourcepath"
;;               (mapconcat 'identity (jde-expand-wildcards-and-normalize jde-sourcepath) ":")
              )
	    (jde-compile-get-args this)))
	   (source-path
	    (jde-normalize-path buffer-file-name))
	   (arg-array (concat "new String[] {\"" source-path "\"")))
    
      (if args
	  (setq arg-array
		(concat
		 arg-array
		 ","
		 (mapconcat
		  (lambda (arg)
		    (concat "\"" arg "\""))
		  args
		  ","))))

      (setq arg-array (concat arg-array "}"))
     
	
      (save-excursion
	(set-buffer (oref (oref this buffer) buffer))

	(insert "CompileServer output:\n")
	(insert "\n")

	(let (flag temp)
	  (setq temp
	    (mapconcat
	     (lambda (x)
	       (if (and flag
			jde-compile-option-hide-classpath)
		   (progn
		     (setq flag nil)
		     "...")
		 (if (not (string= x "-classpath"))
		     x
		   (progn
		     (setq flag t)
		     x)))) args " "))

	  (insert temp " "))
	(insert source-path "\n"))

      (if (not (jde-bsh-running-p))
	  (progn
	    (bsh-launch (oref 'jde-bsh the-bsh))
	    (bsh-eval (oref 'jde-bsh the-bsh) (jde-create-prj-values-str))))
      (bsh-eval (oref 'jde-bsh the-bsh)
                   (format "addClassPath(\"%s\");" (oref this :path)))
      (bsh-buffer-eval
       (oref 'jde-bsh the-bsh)
       (concat
	(format
         "if ((new org.eclipse.jdt.internal.compiler.batch.Main(new java.io.PrintWriter(System.out), new java.io.PrintWriter(System.out), true)).compile(%s)) { print (\"0\");} else {print (\"1\");};"
         arg-array)
	"\n")
       (oref this buffer))))



;; convert jde-compiler values from the format defined in
;; jde-compile.el to the one used in this file
(let ((compiler-name (car jde-compiler)))
(when (not (listp compiler-name))
  (setq jde-compiler
        (cond
         ((equal compiler-name "javac") '("javac"))
         ((equal compiler-name "javac server") '("javac server"))
         ((equal compiler-name "jikes") (list (cons "jikes" (cdr jde-compiler))))
         (t "javac server")))))


(defcustom jde-compiler '("javac server")
  "Specify the type, and if necessary, the location of the compiler to
be used to compile source files for the current project. The JDE
supports three compilers: javac server, javac executable, and
jikes. The javac server runs the com.sun.tools.javac package included
with the JDK in the Beanshell. The javac executable shipped with the
JDK also uses this package. The advantage of the javac server is that
it avoids the vm startup time that accounts for most of the
compilation time consumed by the javac executable. The javac server
uses the version of com.sun.tools.javac included in the JDK for the
current project. See `jde-jdk' for more information. If you want to
use the javac executable to compile your project's source files,
select \"javac\" as the compiler type and, optionally, specify
the path to the executable in the \"Path\" field. If you do
not specify a path, the JDE uses the javac executable included in the
JDK for the current project. Similarly, to use jikes, select \"jikes\"
and, if jikes is not on the command path of the Emacs
environment, specify the path of the jikes executable."
  :group 'jde-project
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Compiler type"
           (item "javac")
	   (item "javac server")
	   (list :format "%v"
                 (const "eclipse java compiler server")
                 (file :tag "Path to  ecj.jar (or jdt core jar)"))
           (list :format "%v"
                 (const "jikes")
                 (file :tag "Path to jikes"))
           ))
  )

(defun jde-compile-get-javac ()
  (let* ((jdk-version (jde-java-version))
	 (jdk-split-version (split-string jdk-version "[.]"))
	 (jdk-major-version (nth 0 jdk-split-version))
	 (jdk-minor-version (nth 1 jdk-split-version))
	 (compiler
	  (find-if
	   (lambda (compiler-x)
	     (let* ((compiler-split-version (split-string (oref compiler-x :version) "[.]"))
		    (compiler-major-version (nth 0 compiler-split-version))
		    (compiler-minor-version (nth 1 compiler-split-version)))
	       (and
		(string= jdk-major-version compiler-major-version)
		(string= jdk-minor-version compiler-minor-version))))
	   jde-compile-javac-compilers)))
    (unless compiler
      (let ((latest-javac (car (last jde-compile-javac-compilers))))
	(if
	    (yes-or-no-p
	     (format "The JDE does not recognize JDK %s javac. Assume JDK %s javac?"
		     jdk-version (oref latest-javac :version)))
	    (setq compiler latest-javac))))
    (if compiler
	(if (string= (car jde-compiler) "javac server")
	    (oset compiler :use-server-p t)
	  (progn
	    (oset compiler :use-server-p nil)
	    (oset compiler
		  :path
		  (let ((compiler-path
                         (if (listp (car jde-compiler))
                             (substitute-in-file-name (nth 1 (car jde-compiler)))
                           "")))
		    (if (string= compiler-path "")
			(setq compiler-path (jde-get-jdk-prog 'javac))
		      (if (file-exists-p compiler-path)
			  compiler-path
			(error (format "Invalid compiler path %s"
				       compiler-path)))))))))
    compiler))
	   
	     
(defun jde-compile-get-jikes ()
  (let ((compiler-path
         (substitute-in-file-name (nth 1 (car jde-compiler)))))

    (if (string= compiler-path "")
	(if (executable-find "jikes")
	    (setq compiler-path "jikes")
	  (error "Cannot find jikes"))
      )

  (jde-compile-jikes
     "Jikes"
     :use-server-p nil
     :path compiler-path)))

(defun jde-compile-get-the-compiler ()
  "Get a compiler object that represents the compiler specified by `jde-compiler'."
  (let* ((car-jde-compiler (car jde-compiler))
         (compiler-name (if (listp car-jde-compiler) (car car-jde-compiler) car-jde-compiler)))
    (cond
     ((string-match "javac" compiler-name)
       (jde-compile-get-javac))
     ((string-match "jikes" compiler-name)
      (jde-compile-get-jikes))
     ((string-match "eclipse java compiler server" compiler-name)
      (jde-compile-get-ejc))
     (t
      (error "The JDEE does not support a compiler named %s" compiler-name)))))

(defun jde-compile-get-ejc ()
  "Get the ejc compiler object."
  (let ((compiler-path
         (substitute-in-file-name (nth 1 (car jde-compiler)))))

    (if (string= compiler-path "")
        (error "Cannot find jdt core jar"))
    (jde-compile-ejc-server
     "Eclipse java compiler server"
     :use-server-p t
     :path compiler-path)))


;; Flymake related code
(require 'flymake)

(defcustom jde-ecj-command-line-args '("-d" "none" "-1.5")
  "*Specify Eclipse java complier options as a string of command-line arguments.
The value of this variable should be a list of switches
understood by the compiler, for example, -depend -g. This
variable is used by both `jde-ecj-flymake-init' and
`jde-ecj-server-flymake-init'. It defaults to use the java 1.5
syntax, and not generate class files during compilation."
  :group 'jde-compile-options
  :type '(repeat (string :tag "Argument:")))

;;
;; flymake invoking new ecj processes
;;

(defun jde-ecj-flymake-init ()
  "Run the Eclipse Java compiler to collect flymake errors.
To use this funtion set the java line in `flymake-allowed-file-name-masks' to
  (\"\\.java\\'\" jde-ecj-flymake-init jde-ecj-flymake-cleanup)"
  (if (not (object-of-class-p (jde-compile-get-the-compiler) 'jde-compile-ejc-server))
      (error "The ecj option for flymake can only be set when the jde-compiler is also set to ecj")
      (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                           'jde-ecj-create-temp-file))
             (local-file  (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
        (list "java" (append  (jde-compile-classpath-arg (jde-compile-get-the-compiler))
                              (list "-jar" (oref  (jde-compile-get-ejc) path) "-Xemacs")
                              jde-ecj-command-line-args
                              (list local-file))))))

(defun jde-ecj-create-temp-file (file-name prefix)
  "Create the file FILE-NAME in a unique directory in the temp directory.
This function uses `random' to generate a \"unique\" directory
name. It doesn't just create the file in the temp directory to
prevent another emacs process on this same machine from trying to
use the same file.  PREFIX is ignored in this function as java
compilers want the temporary file to have the same name as the
orginal file."
  (file-truename (expand-file-name (file-name-nondirectory file-name)
                                   (expand-file-name  (int-to-string (random)) (flymake-get-temp-dir)))))

(defun jde-ecj-flymake-cleanup ()
  "Cleanup after `flymake-ecj-init' -- delete temp file and dirs."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (when flymake-temp-source-file-name
    (jde-ecj-flymake-delete-temp-directory
     (file-name-directory flymake-temp-source-file-name))))

(defun jde-ecj-flymake-delete-temp-directory (dir-name)
  "Attempt to delete temp dir DIR-NAME created by `flymake-create-temp-with-folder-structure', do not fail on error."
  (let* ((true-dir-name (file-truename dir-name))
         (true-tmp-dir (file-truename (flymake-get-temp-dir))))
    (when (equal true-tmp-dir (substring true-dir-name 0 (length true-tmp-dir)))
      (while (not (equal true-tmp-dir true-dir-name)) 
        (mapcar 'jde-ecj-safe-delete-file (directory-files true-dir-name t))
        (flymake-safe-delete-directory true-dir-name)
        (setq true-dir-name (file-name-directory (directory-file-name true-dir-name)))))))

(defun jde-ecj-safe-delete-file (file-name)
  (when (and file-name (file-exists-p file-name) (file-regular-p file-name))
    (delete-file file-name)
    (flymake-log 1 "deleted file %s" file-name)))

;;
;; flymake talking to the running ecj server process
;;

(defvar jde-ecj-server-setup-called nil
  "A value of nil indicates that `jde-ecj-server-setup' has not
yet been called for the current emacs session.")

(defun jde-ecj-server-setup ()
  (defalias 'flymake-start-syntax-check-process 'jde-ecj-flymake-start-syntax-check-process)
  (defalias 'flymake-process-filter 'jde-ecj-flymake-process-filter)
  (setq jde-ecj-server-setup-called t))


(defun jde-ecj-server-flymake-init ()
  "Run the Eclipse Java compiler to collect flymake errors.
To use this funtion set the java line in `flymake-allowed-file-name-masks' to
  (\"\\.java\\'\" jde-ecj-server-flymake-init jde-ecj-flymake-cleanup)"

  (unless jde-ecj-server-setup-called
    (jde-ecj-server-setup))

  (if (not (object-of-class-p (jde-compile-get-the-compiler) 'jde-compile-ejc-server))
      (error "The ecj option for flymake can only be set when the jde-compiler is also set to ecj")
    ;; else
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 'jde-ecj-create-temp-file))
           (directory-sep-char ?/)
           (args (append (jde-compile-classpath-arg (jde-compile-get-the-compiler))
                         (list "-Xemacs" "-noExit")
                         jde-ecj-command-line-args))
           (arg-array (concat "new String[] {")))

      (flymake-log 3 "jde-ecj-server-flymake-init temp-file=%s" temp-file)
      (flymake-log 3 "jde-ecj-server-flymake-init flymake-temp-source-file-name=%s" flymake-temp-source-file-name)


      (unless (jde-bsh-running-p)
        (bsh-launch (oref 'jde-bsh the-bsh))
        (bsh-eval (oref 'jde-bsh the-bsh) (jde-create-prj-values-str)))

       (bsh-eval (oref 'jde-bsh the-bsh)
                 (format "addClassPath(\"%s\");" (oref (jde-compile-get-the-compiler) :path)))

      (if args (setq arg-array (concat arg-array
                                       (mapconcat
                                        (lambda (arg)
                                          (concat "\"" arg "\"")) args ",")
                                       ",")))

      (setq arg-array (concat arg-array "\"" temp-file "\"}"))
        

      (list (cons (oref (oref  (oref 'jde-bsh the-bsh) buffer) process) ;; server process
                  "jde-eclipse-compiler-server-done")  ;; output end marker
            ;; compile command
            (concat (format
                     "(new org.eclipse.jdt.internal.compiler.batch.Main(new java.io.PrintWriter(System.out),new java.io.PrintWriter(System.err), true)).compile(%s);print (\"jde-eclipse-compiler-server-done\");"
                     arg-array))))))


(defvar flymake-server-process-saved-buffer nil
"Original process buffer of the flymake server process. This is restored in `jde-ecj-flymake-server-process-end'")

(defvar flymake-server-process-saved-filter nil
"Original process filter of the flymake server process. This is restored in `jde-ecj-flymake-server-process-end'")

(defvar flymake-process-server-end-marker nil
  "When using a process server, this string in the process output
marks the end of the current set of compilations/warnings.")


(defun jde-ecj-flymake-server-process-end (process output)
  "The equivalent of `flymake-process-sentinel' for flymake server processes.
This function is called by `flymake-process-filter' when it sees
the end of output marker `flymake-process-server-end-marker' in
the output stream."
  (let* ((source-buffer     (process-buffer process))
         (cleanup-f         (flymake-get-cleanup-function (buffer-file-name source-buffer))))
    
    (flymake-log 2 "server process %d \"exited\" with output %s" (process-id process) output)
    (condition-case err
        (progn
          (flymake-log 3 "cleaning up using %s" cleanup-f)
          (when (buffer-live-p source-buffer)
            (with-current-buffer source-buffer
              (funcall cleanup-f)))

          (setq flymake-processes (delq process flymake-processes))
          (set-process-buffer process flymake-server-process-saved-buffer)
          (set-process-filter process flymake-server-process-saved-filter)
            
          (when (buffer-live-p source-buffer)
            (with-current-buffer source-buffer

              (flymake-parse-residual)
              (flymake-post-syntax-check 0 buffer-file-name)
              (setq flymake-is-running nil))))
      (error
       (let ((err-str (format "Error in process sentinel for buffer %s: %s"
                              source-buffer (error-message-string err))))
         (flymake-log 0 err-str)
         (with-current-buffer source-buffer
           (setq flymake-is-running nil)))))))
;;
;; flymake modifications to allow it to converse with a running
;; process instead of always starting a new "make" process
;; 
(defun jde-ecj-flymake-start-syntax-check-process (cmd args dir)
  "Start syntax check process."
  (let* ((process nil))
    (condition-case err
	(progn
	  (when dir
	    (let ((default-directory dir))
	      (flymake-log 3 "starting process on dir %s" default-directory)))

          (cond
           ((and (listp cmd) (processp (car cmd)))
            (setq process (car cmd))
            (setq flymake-server-process-saved-filter (process-filter process))
            (setq flymake-server-process-saved-buffer (process-buffer process))
            (set-process-buffer process (current-buffer))
            (process-send-string process args)
            (setq flymake-process-server-end-marker (cdr cmd))
            (flymake-log 2 "sent command=%s, to process=%S"
                         args process))
           (t
            (setq process (apply 'start-process "flymake-proc" (current-buffer) cmd args))
            (set-process-sentinel process 'flymake-process-sentinel)
            (flymake-log 2 "started process %d, command=%s, dir=%s"
                         (process-id process) (process-command process)
                         default-directory)))

	  (set-process-filter process 'flymake-process-filter)
          (push process flymake-processes)

          (setq flymake-is-running t)
          (setq flymake-last-change-time nil)
          (setq flymake-check-start-time (flymake-float-time))

	  (flymake-report-status nil "*")

	  process)
      (error
       (let* ((err-str (format "Failed to launch syntax check process  with args : %s"
			       (error-message-string err)))
	      (source-file-name buffer-file-name)
	      (cleanup-f        (flymake-get-cleanup-function source-file-name)))
	 (flymake-log 0 err-str)
	 (funcall cleanup-f)
	 (flymake-report-fatal-status "PROCERR" err-str))))))


(defun jde-ecj-flymake-process-filter (process output)
  "Parse OUTPUT and highlight error lines.
It is the flymake process filter. It is also responsible for
calling `jde-ecj-flymake-server-process-end' if the process is a server
process and the output contains the end of output marker `flymake-process-server-end-marker'."
  (let ((source-buffer (process-buffer process)))
    (flymake-log 3 "received %d byte(s) of output from process %d"
                 (length output) (process-id process))
    (flymake-log 3 "output : %s" output)
    (setq output (replace-regexp-in-string "bsh % " "" output))
    (when source-buffer
      (with-current-buffer source-buffer
        (flymake-parse-output-and-residual output))))
  (if (and flymake-process-server-end-marker (string-match flymake-process-server-end-marker output))
      (jde-ecj-flymake-server-process-end process output)))

;; Use this for some extra debugging in flymake
;; (defun flymake-get-full-patched-file-name (file-name-from-err-msg
;; base-dirs files)
;;  (let* ((base-dirs-count  (length base-dirs))
;;         (file-count       (length files))
;;         (real-name        nil))

;;    (while (and (not real-name) (> base-dirs-count 0))
;;      (setq file-count (length files))
;;      (while (and (not real-name) (> file-count 0))
;;        (let* ((this-dir        (nth (1- base-dirs-count) base-dirs))
;;               (this-file       (nth 0 (nth (1- file-count) files)))
;;               (this-real-name  (nth 1 (nth (1- file-count) files))))
;;          (flymake-log 0 "this-dir=%s this-file=%s this-real=%s msg-file=%s"
;; this-dir this-file this-real-name file-name-from-err-msg)
;;          (when (and this-dir this-file (flymake-same-files
;;                                         (expand-file-name file-name-from-err-msg this-dir)
;;                                         this-file))
;;            (setq real-name this-real-name)))
;;        (setq file-count (1- file-count)))
;;      (setq base-dirs-count (1- base-dirs-count)))
;;    real-name))


(provide 'jde-eclipse-compiler-server)

;;; jde-eclipse-compiler-server.el ends here
