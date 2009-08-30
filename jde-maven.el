;;; jde-maven.el --- Use Apache Maven to build your JDE projects

;; $Id: jde-maven.el,v 1.1 2005/11/11 20:29:18 cplate Exp cplate $

;;
;; Author: Raffael Herzog <herzog@raffael.ch>
;; Created: 30 Apr 2004
;;
;; Updated by Christian Plate <cplate@web.de> to work with
;; CVS Emacs 22.0.50
;; Version 0.1.2

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

;; Commentary:

;; This file provides functions to control the Maven console from
;; within Emacs and to use Maven for building Java either by calling
;; it for each build separately or using the Maven console.
;;
;; It's designed to be used in conjunction with JDEE.
;;
;; To install put this file somewhere in your load-path, byte-compile
;; it and add a (require 'jde-maven) to your .emacs *after* JDEE has
;; been loaded.
;;
;; See the customization group jde-maven for customization options.
;; See the description of the command jde-maven-console-build for
;; information on what it does.
;;
;; This provides the following interactive commands:
;;
;; -- jde-maven-build: Build without using the console.
;; -- jde-maven-console-build: Build using the console.
;; -- jde-maven-console-start: Start the console.
;; -- jde-maven-console-stop: Stop the console.
;; -- jde-maven-console-force-stop: Kill the console.
;; -- jde-maven-console-restart: Restart the console.
;;
;; Thanks to Jason Stell and Kevin A. Burton for jde-ant.el.
;; jde-maven-console-build-internal is heavily based on their code,
;; although not much of the original function is left ... :)
;;
;;
;; TODO:
;;
;; -- Support more than one console running. Basic idea: Use one
;;    console for each project file. Allow a maximum number of
;;    consoles to be running concurrently. If this number is exceeded,
;;    stop the least recently used one.
;;
;; -- Automatic detection when the console needs to be restarted.
;;    Check the timestamps of project.xml, maven.xml,
;;    project.properties and build.properties. Question: What about
;;    reactor?
;;
;; -- Is there a way to determine whether the build failed or
;;    succeeded?
;;
;; -- Completion in the goal prompt.
;;
;;
;;; History:
;;
;; -- Version 0.1 (9 May 2004)
;;    Initial Version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup jde-maven nil
  "Build using maven."
  :group 'jde
  :prefix 'jde-maven)
(defcustom jde-maven-command "maven"
  "The command to run maven"
  :group 'jde-maven
  :type 'string)
(defcustom jde-maven-project-file-name "project.xml"
  "The name of the maven project file."
  :group 'jde-maven
  :type '(choice (const :tag "Default" "project.xml")
                 (string :tag "String")))
(defcustom jde-maven-prompt-project-file nil
  "Prompt for the project file? If this is nil, jde-maven will look
  for the next project file the directory tree up the current file.
  The name of the project file can be customized in
  `jde-maven-project-file-name'."
  :group 'jde-maven
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)))
(defcustom jde-maven-goal nil
  "The name of the goal to attain."
  :group 'jde-maven
  :type '(choice (const :tag "Default" nil)
                 (string :tag "String")))
(defcustom jde-maven-prompt-goal t
  "Prompt for the goal to attain?"
  :group 'jde-maven
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)))
(defcustom jde-maven-arguments ""
  "The arguments to be passed to maven."
  :group 'jde-maven
  :type 'string)
(defcustom jde-maven-prompt-arguments nil
  "Prompt for further maven arguments?"
  :group 'jde-maven
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)))
(defcustom jde-maven-console-goal "console"
  "The goal used to start the Maven console."
  :group 'jde-maven
  :type 'string)
(defcustom jde-maven-console-prompt-regex "^[-a-zA-Z0-9.:_/ \\t]+ >\\( \\[[-a-zA-Z0-9.:_/]*\\]\\)?$"
  "The regex for the Maven console prompt."
  :group 'jde-maven
  :type 'string)
(defcustom jde-maven-console-timeout 10
  "The timeout when waiting for output of the maven console."
  :group 'jde-maven
  :type 'integer)


;;
;; define some variables
;;

(defvar jde-maven-project-file-history nil
  "The project file history.")
(defvar jde-maven-current-project-file nil
  "The project file last use for normal compilation or the currently running Maven process.")
(defvar jde-maven-goal-history nil
  "History of goals.")
(defvar jde-maven-arguments-history nil
  "History of Maven arguments.")

(defvar jde-maven-console-process nil
  "The currently running Maven console process")

(defvar jde-maven-console-status 'not-running
  "The current status of the console. Possible values are:
`starting': The console is just starting
`ready': The console is ready to accept commands
`working': The console is currently building one or more goals
`exiting': The console is exiting
`not-running': The console is not running")

(defvar jde-maven-console-current-line ""
  "The process filter is line-oriented, i.e. it writes what it
receives into this variable until a line is complete. Complete lines
are then logged, if the status is `working' inserted into the
compilation buffer.")



;;
;; some constants
;;

(defconst jde-maven-console-process-name "Maven"
  "The name of the Maven process.")
(defconst jde-maven-console-log-buffer-name "*Maven*"
  "The name of the buffer containing the Maven log.")
(defconst jde-maven-console-compilation-buffer-name "*compilation*"
  "The name of the compilation buffer to use.")



;;
;; generic functions for jde-maven
;;

(defun jde-maven-get-project-file()
  "Determine the path to the project file according to the user's
preferences. It either prompts for a project file if
`jde-maven-prompt-project-file' is t, or it searches for a file
called `jde-maven-projecct-file-name' the directory tree upwards
from the current file."
  (let ((project-file nil))
    (setq project-file
          (if jde-maven-prompt-project-file
              (read-file-name "Project File: " jde-maven-current-project-file jde-maven-current-project-file t)
            (catch 'found
              (let ((last-directory nil))
                (setq project-file (file-name-directory (buffer-file-name)))
                (while (not (string= last-directory project-file))
                  (message (concat project-file jde-maven-project-file-name))
                  (when (file-regular-p (concat project-file jde-maven-project-file-name))
                    (throw 'found (concat project-file jde-maven-project-file-name)))
                  (setq last-directory project-file)
                  (setq project-file (file-name-directory (directory-file-name project-file))))
                (error (concat "No " jde-maven-project-file-name " found."))))))
    (setq project-file (expand-file-name project-file))
    ;;(setq jde-maven-current-project-file project-file)
    (setq project-file (if (file-directory-p project-file)
                           (concat (file-name-as-directory project-file) jde-maven-project-file-name)
                         project-file))
    (if (not (file-regular-p project-file))
        (error (concat project-file " does not exist or is not a regular file")))
    project-file))

(defun jde-maven-get-goal()
  "Get the goal to build. Prompt for a goal if `jde-maven-prompt-goal'
is t or use the value of `jde-maven-goal'"
  (if jde-maven-prompt-goal
      (read-string "Goal: " (if jde-maven-goal-history
                                (car jde-maven-goal-history)
                              jde-maven-goal)
                   '(jde-maven-goal-history . 1))
    jde-maven-goal))

(defun jde-maven-get-arguments()
  "Get the arguments to run Maven with. Prompts for arguemtns if
`jde-maven-prompt-arguments' is t or uses `jde-maven-arguments'."
  (if jde-maven-prompt-arguments
      (read-string "Arguments: " (if jde-maven-arguments-history
                                     (car jde-maven-arguments-history)
                                   jde-maven-arguments)
                   '(jde-maven-arguments-history . 1))
    jde-maven-arguments))

(defun jde-maven-build-command-line(project-file goal arguments)
  "Build a Maven command line."
  (concat "cd \"" (file-name-directory project-file)
          "\" && " jde-maven-command " -f " (file-name-nondirectory project-file)
          " " arguments " " goal))



;;
;; do a Maven build without using the console
;;

(defun jde-maven-build (&optional project-file goal args)
  "Do a standard maven build. Consider using the console ... :)"
  (interactive)
  (compile (jde-maven-build-command-line (if project-file
                                             project-file
                                           (jde-maven-get-project-file))
                                         (if goal
                                             goal
                                           (jde-maven-get-goal))
                                         (if args
                                             args
                                           (jde-maven-get-arguments)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Console
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Utilities
;;

(defun jde-maven-console-running-p ()
  "Return t if the console is currently running, nil otherwise."
  (let ((status (process-status jde-maven-console-process-name)))
    (if status
        (if (string= (symbol-name status) "exit")
            (progn
              (delete-process jde-maven-console-process-name)
              nil)
          t)
      nil)))

(defun jde-maven-console-get-log-buffer ()
  "Get the current maven log buffer or create a new one, if it doesn't
exist."
  (let ((buf (get-buffer jde-maven-console-log-buffer-name)))
    (if buf
        buf
      (setq buf (generate-new-buffer jde-maven-console-log-buffer-name))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (buffer-disable-undo))
      buf)))

(defun jde-maven-console-wait-for-status (status)
  "Wait for the console to enter the specified status. See the
description of the variable `jde-maven-console-status' for possible
states."
  (if (not (jde-maven-console-running-p))
      (error "Maven console not running"))
  (let ((old-status jde-maven-console-status))
    (if (not (string= (symbol-name old-status)
                      (symbol-name status)))
        (while (string= (symbol-name jde-maven-console-status)
                        (symbol-name old-status))
          (if (not (jde-maven-console-running-p))
              ;; If not running, set old-status to whatever and exit
              (setq old-status 'dummy-status-not-possible)
            (accept-process-output (get-process jde-maven-console-process-name) jde-maven-console-timeout)))))
  (string= (symbol-name jde-maven-console-status)
           (symbol-name status)))

(defun jde-maven-console-send-command (command &optional new-status)
  "Send a command to the console optionally setting the status to
new-status afterwards. This functions waits for the console to be in
the state `ready' before sending the command."
  (if (not (jde-maven-console-running-p))
      (error "Maven console not running"))
  (message "Waiting for console to be ready to accept commands...")
  (jde-maven-console-wait-for-status 'ready)
  (if new-status (jde-maven-console-set-status new-status))
  (jde-maven-console-log (concat command "\n"))
  (message (concat "Sending command: " command))
  (process-send-string jde-maven-console-process-name (concat command "\n")))

(defun jde-maven-console-log (message)
  "Log message to the console."
  (with-current-buffer (jde-maven-console-get-log-buffer)
    (goto-char (point-max))
    (insert message)
    (set-buffer-modified-p nil)))

(defun jde-maven-console-log-to-compilation-buffer (message)
  "Log message to the compilation buffer."
  (with-current-buffer (get-buffer jde-maven-console-compilation-buffer-name)
    (goto-char (point-max))
    (if message
        (insert message)
      (newline))
    (if (not jde-xemacsp)
        (if compilation-scroll-output
            (let ((win (get-buffer-window "*compilation*")))
              (save-selected-window
                (if win
                    (progn
                      (select-window win)
                      (goto-char (point-max))))))))))

(defun jde-maven-console-set-status (status)
  "Set the current status of the console. For internal use only."
  (when (not (string= (symbol-name jde-maven-console-status)
                      (symbol-name status)))
    (setq jde-maven-console-status status)))


;;
;; process communication stuff
;;

(defun jde-maven-console-filter (process string)
  "The process filter for the console. This basically forwards
everything to the maven log buffer. While in status `working', it
additionally appends things to the compilation buffer. Detects the
prompt and sets the status to `ready'."
  (mapc (lambda (x)
          (if (= x ?\n)
              (progn
                (jde-maven-console-log (concat jde-maven-console-current-line "\n"))
                (if (string-match jde-maven-console-prompt-regex jde-maven-console-current-line)
                    (progn
                      (if (string= (symbol-name jde-maven-console-status) "working")
                          (jde-maven-console-log-to-compilation-buffer
                           (concat "\n\nCompilation finished at " (current-time-string) "\n")))
                      (jde-maven-console-set-status 'ready))
                  (when (and (not (string= (symbol-name jde-maven-console-status) "starting"))
                             (not (string= (symbol-name jde-maven-console-status) "exiting")))
                    (jde-maven-console-set-status 'working)
                    (jde-maven-console-log-to-compilation-buffer
                     (concat jde-maven-console-current-line "\n"))))
                (sit-for 0)
                (setq jde-maven-console-current-line ""))
            (setq jde-maven-console-current-line (concat jde-maven-console-current-line (char-to-string x)))))
  string))

(defun jde-maven-console-sentinel (process string)
  "The process sentinel for the console. This one simply sets the
status to `not-running'"
  (jde-maven-console-log (concat "Maven Sentinel: " string "\n"))
  ;; I think, we can safely assume that whenever this function is called,
  ;; Maven exited for whatever reason
  (jde-maven-console-set-status 'not-running))



;;
;; starting/stopping the console and building
;;

(defun jde-maven-console-start (&optional project-file args)
  "Start the maven console with the given project file and arguments."
  (interactive)
  (if (jde-maven-console-running-p)
      (error "Maven console already running"))
  (message "Starting Maven console...")
  (let ((process-connection-type nil)
        (console nil))
    (jde-maven-console-set-status 'starting)
    (with-current-buffer (jde-maven-console-get-log-buffer)
      (erase-buffer)
      (set-buffer-modified-p nil))

    (insert (jde-maven-build-command-line (if project-file
                                                                         project-file
                                                                       (jde-maven-get-project-file))
                                                                     jde-maven-console-goal
                                                                     (if args
                                                                         args
                                                                       (jde-maven-get-arguments))))
    (setq console
          (start-process-shell-command jde-maven-console-process-name
                                       jde-maven-console-log-buffer-name
                                       (jde-maven-build-command-line (if project-file
                                                                         project-file
                                                                       (jde-maven-get-project-file))
                                                                     jde-maven-console-goal
                                                                     (if args
                                                                         args
                                                                       (jde-maven-get-arguments)))))
    (set-process-filter console 'jde-maven-console-filter)
    (set-process-sentinel console 'jde-maven-console-sentinel)
    (if (jde-maven-console-wait-for-status 'ready)
        (message "Maven console started")
      (error "Starting Maven failed"))))

(defun jde-maven-console-stop ()
  "Stop an already running maven console."
  (interactive)
  (if (jde-maven-console-running-p)
      (progn
        (message "Stopping maven console...")
        (jde-maven-console-send-command "quit" 'exiting)
        (jde-maven-console-wait-for-status 'not-running)
        (if (jde-maven-console-running-p)
            (error "Error stopping Maven console")
          (message "Maven console stopped")))
    (error "Console not running")))

(defun jde-maven-console-force-stop ()
  "Kill a running maven console."
  (interactive)
  (if (jde-maven-console-running-p)
      (kill-process (get-process jde-maven-console-process-name))))

(defun jde-maven-console-restart ()
  "Resteart a running maven console."
  (interactive)
  (if (jde-maven-console-running-p)
      (jde-maven-console-stop))
  (jde-maven-console-start))

;; Thanks to Jack Donohue <donohuej@synovation.com>.
(defun jde-maven-finish-kill-buffer (buf msg)
  "Removes the jde-compile window after a few seconds if no errors."
  (save-excursion
    (set-buffer buf)
    (if (null (or (string-match ".*exited abnormally.*" msg)
                  (string-match ".*BUILD FAILED.*" (buffer-string))))
        ;;no errors, make the compilation window go away in a few seconds
        (lexical-let ((compile-buffer buf))
          (run-at-time
           "2 sec" nil 'jde-compile-kill-buffer
           compile-buffer)
          (message "No compilation errors"))
      ;;there were errors, so jump to the first error
      ;;(if jde-compile-jump-to-first-error (next-error 1)))))
      )))

(defun jde-maven-console-build (&optional restart)
  "Do a build using the console. With prefix argument, stop the
current console before building even if the project file is still the
same.

This command will restart the console as needed. It keeps track of
which project file the current console is running. If the project file
to be used didn't change, it reuses the current console, else it stops
it (if running) and starts a new one for the new project file."
  (interactive "P")
  (if (and restart (jde-maven-console-running-p))
      (jde-maven-console-stop))
  (let (project-file goal args)
    (setq project-file (jde-maven-get-project-file))
    (setq goal (jde-maven-get-goal))
    (if (and (not (string= jde-maven-current-project-file project-file))
             (jde-maven-console-running-p))
        (jde-maven-console-stop))
    (setq jde-maven-current-project-file project-file)
    (when (not (jde-maven-console-running-p))
      (setq args (jde-maven-get-arguments))
      (jde-maven-console-start project-file args))
    (jde-maven-console-build-internal project-file goal)))

(defun jde-maven-console-build-internal (project-file goal)
  "This method displays Maven output in a compilation buffer."
  (let* (error-regexp-alist
         enter-regexp-alist
         leave-regexp-alist
         file-regexp-alist
         nomessage-regexp-alist
         (parser compilation-parse-errors-function)
         outbuf)

    (save-excursion
      (setq outbuf (get-buffer-create jde-maven-console-compilation-buffer-name))
      (set-buffer outbuf)

      ;; In case the compilation buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables))
    (setq error-regexp-alist compilation-error-regexp-alist)
    (setq enter-regexp-alist
          (if (not jde-xemacsp) compilation-enter-directory-regexp-alist))
    (setq leave-regexp-alist
          (if (not jde-xemacsp) compilation-leave-directory-regexp-alist))
    (setq file-regexp-alist
          (if (not jde-xemacsp) compilation-file-regexp-alist))
    (setq nomessage-regexp-alist
          (if (not jde-xemacsp) compilation-nomessage-regexp-alist))

    (save-excursion
      ;; Clear out the compilation buffer and make it writable.
      (set-buffer outbuf)
      (compilation-mode)
      (setq buffer-read-only nil)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))
      (display-buffer outbuf)
      (insert "Maven build\n")
      (insert "~~~~~~~~~~~\n")
      (insert (concat "Project file : " project-file "\n"))
      (insert (concat "Building goal: " goal "\n\n"))
      (set-buffer-modified-p nil))
    (setq outwin (display-buffer outbuf))
    (save-excursion
      (set-buffer outbuf)
      ;; (setq buffer-read-only t)  ;;; Non-ergonomic.
      (set (make-local-variable 'compilation-parse-errors-function)
           parser)
      ;; What's that?
      ;;(set (make-local-variable 'compilation-error-message)
      ;;     error-message)
      (set (make-local-variable 'compilation-error-regexp-alist)
           error-regexp-alist)
      (if (not jde-xemacsp)
          (progn
            (set (make-local-variable
                  'compilation-enter-directory-regexp-alist)
                 enter-regexp-alist)
            (set (make-local-variable
                  'compilation-leave-directory-regexp-alist)
                 leave-regexp-alist)
            (set (make-local-variable 'compilation-file-regexp-alist)
                 file-regexp-alist)
            (set (make-local-variable
                  'compilation-nomessage-regexp-alist)
                 nomessage-regexp-alist)))
      (setq default-directory (file-name-directory project-file)
            compilation-directory-stack (list default-directory))
      (compilation-set-window-height outwin)

      ;;(jde-maven-console-set-status 'building)
      (jde-maven-console-send-command goal 'working)

      (if (not jde-xemacsp)
          (if compilation-process-setup-function
              (funcall compilation-process-setup-function))))
    ;; Make it so the next C-x ` will use this buffer.
    (setq compilation-last-buffer outbuf)

    (if (jde-maven-console-wait-for-status 'ready)
	;; If no error kill the buffer
	(jde-maven-finish-kill-buffer outbuf "Compilation finished sucessfully.")
      )))

(provide 'jde-maven)
