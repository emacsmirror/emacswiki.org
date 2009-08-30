;;; jde-findbugs.el --- Findbugs interface for JDE
;; $Revision: 1.4 $ $Date: 2006/08/03 23:52:59 $

;; Derived from jde-checkstyle.el

;; Copyright (C) 2001, 2002, 2003, 2004 Markus Mohnen and Paul Kinnucan

;; Authors: Markus Mohnen and Paul Kinnucan
;; Maintainers: Markus Mohnen and Paul Kinnucan
;; Created: 07 Feb 2005
;;
;; Keywords: Java bug finder Emacs
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; ) or from the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; This package provides an interface from JDE (see
;;; http://jde.sunsite.dk/) to Findbugs (see
;;; http://findbugs.sourceforge.net/), a development tool that looks
;;; for bugs in Java code.


;; Currently it has the following limitations: 
;;
;;  You cannot yet run the check on a single class, only either the
;;  current package, or a directory defined in a variable.  Just have
;;  to identify the name(s) of the classes to pass to findbugs.
;;  Should be easy.
;;
;;  You cannot yet specify memory parameters to the JVM.  Should be
;;  easy.
;;
;;  findbugs sometimes outputs extra information to stderr that can
;;  trick the compile window into thinking there is an error to jump
;;  to.  I'm not sure what to do about this.
;;
;;  Not all of the error messages output by findbugs in it's "emacs"
;;  format are actually in the right format (e.g.: UrF). I have
;;  submitted a patch to findbugs that helps somewhat at
;;  http://sourceforge.net/tracker/index.php?func=detail&aid=1117624&group_id=96405&atid=614695


;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;      (require 'jde-findbugs)

;;; Usage:
;;
;;  M-x `jde-findbugs' to check the java file in the current buffer.
;;

;;; Customization:
;;
;;  M-x `jde-findbugs-customize' to customize all the jde-findbugs options.

;;; Code:

(require 'jde-compile)
(require 'jde-package)

(defconst jde-findbugs-version "0.1")

(defgroup jde-findbugs nil
  "This group specifies options for the JDEE's interface to the Findbugs 
package (http://findbugs.sourceforge.net). The Findbugs package
checks Java class files for bug patterns."
  :group 'jde)

(defcustom jde-findbugs-class "edu.umd.cs.findbugs.FindBugs"
  "*Java bug finder class.
Specifies the class of the the program to be used to check the source
in the current buffer. The default is the findbugs program."
  :group 'jde-findbugs
  :type 'string)

(defcustom jde-findbugs-directory ""
  "*Specify path to the findbugs installation directory."
  ;; This is used to define the findbugs jarfile location and to set
  ;; the findbugs.home command line parameter that findbugs requires.
  :group 'jde-findbugs
  :type 'file)

;; (makunbound 'jde-findbugs-exclude)
(defcustom jde-findbugs-exclude nil
  "*Exclude filter file used when checking this project's Java
code. Enter the path of a Findbugs filter file that defines the custom
excludes in this field (see the Findbugs documentation for information
on filter files). This filter file specifies which bug hits should be
excluded -- for example, you could use this to tell findbugs to not
report any PZLA messages."
   :group 'jde-findbugs
   :type 'file)

;; (makunbound 'jde-findbugs-include)
(defcustom jde-findbugs-include nil
  "*Include filter file used when checking this project's Java
code. Enter the path of a Findbugs filter file that defines the custom
includes in this field (see the Findbugs documentation for information
on filter files). This filter file specifies which bug hits should be
excluded -- for example, you could use this to tell findbugs to only
check classes in a certain set of packages."
   :group 'jde-findbugs
   :type 'file)

;; (makunbound 'jde-findbugs-expanded-properties)
(defcustom jde-findbugs-expanded-properties nil
  "*Specify the values of the expanded properties for the java virtual
machine.  To enter a property, select the INS button. Emacs displays a
Property Name field and a Property Value field for the property. Enter
the name of the property, for example, findbugs.workHard, in the
Property Name field; enter its value, for example, true,
in the Property Value field.  Repeat this process to display
additional properties. You can specify as many properties as you like
in this way. To delete a property, select the DEL button next to the
property."
  :group 'jde-findbugs
  :type '(repeat (cons 
		  (string :tag "Property Name") 
		  (string :tag "Property Value"))))

;; (makunbound 'jde-findbugs-class-dir)
(defcustom jde-findbugs-class-dir nil
  "*Path of a directory to check. If you specify a path, Findbugs
checks all the class files in the specified directory. Otherwise, it
checks the class for the file in the current buffer."
   :group 'jde-findbugs
   :type '(choice (const :tag "None" :value nil)
		  (file :menu-tag "Class Directory" :tag "Path")))

;; (makunbound 'jde-findbugs-auxclasspath)
(defcustom jde-findbugs-auxclasspath nil
  "*JARs and directories containing classes that are part of the
program being analyzed but which you don't want analyzed for
bugs"
  :group 'jde-findbugs
  :type '(repeat (file :tag "Path")))

;; (makunbound 'jde-findbugs-finish-hook)
(defcustom jde-findbugs-finish-hook 
  '(jde-compile-finish-kill-buffer)
  "List of functions to be invoked when Findbugs terminates.  Each
function should accept two arguments: the compilation buffer and a
string describing how the compilation finished."
  :group 'jde-findbugs
  :type 'hook)


(defmethod jde-findbugs-get-property-args ((this jde-run-vm))
    "Get property arguments."
    (mapcar
     (lambda (prop)
       (format "-D%s=%s" (car prop) (cdr prop)))
     jde-run-option-properties))
  

;;;###autoload
(defun jde-findbugs-customize ()
  "Set Java bug finding options."
  (interactive)
  (customize-group "jde-findbugs"))


(defclass jde-findbugs-checker ()
  ((buffer           :initarg :buffer
	             :type buffer
	             :documentation
	             "Compilation buffer")
   (window           :initarg :window
		     :type window
		     :documentation
		     "Window that displays the compilation buffer.")
   (source-root      :initarg :source-root
		     :type string
		     :documentation
		     "The entry from classpath/sourcepath that contains the source.")
   (source-subdir    :initarg :source-subdir
		     :type string
		     :documentation
		     "The subdirectory w.r.t source-root that contains the source.")
   (check-package-p  :initarg :check-package-p
		     :type boolean
	             :documentation
	             "Check entire package (and subpackages), rather than the current class."))
  "Class of Java bug finders.")

(defmethod jde-findbugs-create-checker-buffer ((this jde-findbugs-checker))
  (save-excursion
    (let ((buf (get-buffer-create "*find bugs*"))
	  (error-regexp-alist compilation-error-regexp-alist)
	  (enter-regexp-alist (if (boundp 'compilation-enter-directory-regexp-alist)
                                  compilation-enter-directory-regexp-alist))
	  (leave-regexp-alist (if (boundp 'compilation-leave-directory-regexp-alist)
                                  compilation-leave-directory-regexp-alist))
	  (file-regexp-alist (if (boundp 'compilation-file-regexp-alist)
                                 compilation-file-regexp-alist))
	  (nomessage-regexp-alist (if (boundp 'compilation-nomessage-regexp-alist)
                                      compilation-nomessage-regexp-alist))
	  (parser compilation-parse-errors-function)
	  (error-message "No further errors")
          ;; Run from root of source directory, for error messages to resolve to the right file?
	  (classpath-entry (car (jde-findbugs-search-classpath-directories)))) 
          ;;(thisdir default-directory))

      (oset this :buffer buf)
      (oset this :source-root (car classpath-entry))
      (oset this :source-subdir (cadr classpath-entry))

      (set-buffer buf)

      ;; Make sure a findbugs checker process is not
      ;; already running.
      (let ((check-proc (get-buffer-process (current-buffer))))
	(if check-proc
	    (if (or (not (eq (process-status check-proc) 'run))
		    (yes-or-no-p
			 "A findbugs process is running; kill it?"))
		(condition-case ()
		    (progn
		      (interrupt-process check-proc)
		      (sit-for 1)
		      (delete-process check-proc))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once"
			 (buffer-name)))))

      ;; In case the checker buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables)

      ;; Clear out the compilation buffer and make it writable.
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))

      (compilation-mode)
      (setq buffer-read-only nil)
      
      (set (make-local-variable 'compilation-finish-function)
	   (lambda (buf msg) 
	     (run-hook-with-args 'jde-findbugs-finish-hook buf msg)
	     (setq compilation-finish-function nil)))
      (set (make-local-variable 'compilation-parse-errors-function) parser)
      (set (make-local-variable 'compilation-error-message) error-message)
      (set (make-local-variable 'compilation-error-regexp-alist)
	     error-regexp-alist)
      (if (not jde-xemacsp)
	  (progn
	    (set (make-local-variable 'compilation-enter-directory-regexp-alist)
		 enter-regexp-alist)
	    (set (make-local-variable 'compilation-leave-directory-regexp-alist)
		 leave-regexp-alist)
	    (set (make-local-variable 'compilation-file-regexp-alist)
		 file-regexp-alist)
	    (set (make-local-variable 'compilation-nomessage-regexp-alist)
	      nomessage-regexp-alist)))
      (setq default-directory (oref this :source-root)
	    compilation-directory-stack (list (oref this :source-root))))))

(defmethod jde-findbugs-get-property-args ((this jde-findbugs-checker))
    "Get property arguments."
    (mapcar
     (lambda (prop)
       (format "-D%s=%s" (car prop) (cdr prop)))
     jde-findbugs-expanded-properties))
  
(defmethod jde-findbugs-exec ((this jde-findbugs-checker))

  (jde-findbugs-create-checker-buffer this)

  ;; Pop to checker buffer.
  (let ((outwin (display-buffer (oref this :buffer))))
    (compilation-set-window-height outwin)
    (oset this :window outwin))

  (if (not jde-xemacsp)
      (if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))     

  (let* ((outbuf (oref this :buffer))
	 (vm-path (oref (jde-run-get-vm) :path))
         ;; class-dir should be the dir containing the class files we are going to check.
	 (class-dir 
          (expand-file-name (oref this :source-subdir)
                            (if (string= jde-compile-option-directory "")
                                (oref this :source-root)
                              (jde-normalize-path jde-compile-option-directory))))
	 (jde-java-directory
	  (concat
	   (jde-find-jde-data-directory)
	   "java/"))
	 (args (append
                ;; findbugs *needs* this, so we do it here rather than via the expanded-properties mechanism
                (list (format "-Dfindbugs.home=%s" (jde-normalize-path jde-findbugs-directory)))
                (jde-findbugs-get-property-args this)
		(list "-classpath" 
                      (jde-normalize-path
                       (expand-file-name "lib/findbugs.jar" jde-findbugs-directory)))
		(list jde-findbugs-class)		
                (if jde-findbugs-exclude
                    (list "-exclude" 
			  (jde-normalize-path jde-findbugs-exclude)))
                (if jde-findbugs-include
                    (list "-include" 
			  (jde-normalize-path jde-findbugs-include)))
                (list "-emacs")
                (list "-low")

                ; G. Cordrey: add auxiliary classpath
                (when jde-findbugs-auxclasspath
                  (list
                   "-auxclasspath"
                   (jde-build-classpath
                    jde-findbugs-auxclasspath 
                    'jde-findbugs-auxclasspath)
                   ))

		(if jde-findbugs-class-dir
		    (list (jde-normalize-path jde-findbugs-class-dir))
		  (if (oref this :check-package-p)
                      (list class-dir)    ;; check the package directory
                    (list class-dir)))))) ;; check just the class for this source file. TODO give file name of the class for this buffer.

    (save-excursion
      (set-buffer outbuf)
      (setq buffer-read-only nil)

      (insert (format "cd %s\n" default-directory))
      (insert (concat
	       vm-path
	       " "
               (mapconcat 'identity args " ")
	       "\n\n"))

      (let* ((process-environment (cons "EMACS=t" process-environment))
	     (w32-quote-process-args ?\")
	     (win32-quote-process-args ?\") ;; XEmacs
	     (proc (apply 'start-process 
			  (downcase mode-name)
			  outbuf
			  vm-path
			  args)))
	(set-process-sentinel proc 'compilation-sentinel)
	(set-process-filter proc 'compilation-filter)
	(set-marker (process-mark proc) (point) outbuf)
	(setq compilation-in-progress
	      (cons proc compilation-in-progress)))

      (set-buffer-modified-p nil)
      (setq compilation-last-buffer (oref this :buffer)))))


(defun jde-findbugs-search-classpath-directories ()
  "Return a list of sourcepath or classpath matches from which
default-directory is visible or nil if not found. Each match is a list
containing the classpath entry and the remainder of the path to the
default directory."
  (let ((dir (jde-normalize-path default-directory))
        ;; case-insensitive for Windows
        (case-fold-search (eq system-type 'windows-nt)))
    (mapcan
     (lambda (root)
         (let ((root (regexp-quote root)))
           ;;(message "Searching in %S for %S..." root dir)
           (and (string-match root dir)
                (list (list (substring dir 0 (match-end 0)) (substring dir (match-end 0)))))))
     (append (jde-package-get-directories-in-classpath)
	     (mapcar
	      (lambda (p)
		(file-name-as-directory 
		 (jde-normalize-path p 'jde-sourcepath)))
	      jde-sourcepath)))))


;;;###autoload
(defun jde-findbugs ()
  "Finds bugs in the Java program in the current buffer.
This command invokes the findbugs checker specified by `jde-findbugs-class'
with the options specified by the JDEE customization variables
that begin with `jde-findbugs'."
  (interactive)

  (unless jde-findbugs-directory
    (error "jde-findbugs-directory must be defined"))

  (let ((checker (jde-findbugs-checker 
		  "checker" 
		  :check-package-p t))) ;; TODO support check-package-p nil
            
    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-findbugs from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
             (not jde-xemacsp)) 
        (let ((temp last-nonmenu-event))
          ;; The next line makes emacs think that jde-findbugs
          ;; was invoked from the minibuffer, even when it
          ;; is actually invoked from the menu-bar.
          (setq last-nonmenu-event t)
          (save-some-buffers (not compilation-ask-about-save) nil)
          (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))

    (jde-findbugs-exec checker)))

;; Register and initialize the customization variables defined
;; by this package.
(jde-update-autoloaded-symbols)

(provide 'jde-findbugs)

;; $Log: jde-findbugs.el,v $
;; Revision 1.4  2006/08/03 23:52:59  len
;; Only use auxclasspath if defined
;;
;; Revision 1.3  2005/07/12 04:13:55  len
;; Tidying
;;
;; Revision 1.2  2005/07/12 04:07:55  len
;; Added -auxclasspath option patch from Glen Cordrey.
;; Make happy with emacs read-only compilation buffers.
;;
;; Revision 1.1  2005/02/09 03:02:02  len
;; First version
;;
;;; jde-findbugs.el ends here
