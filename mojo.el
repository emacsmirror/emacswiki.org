;;; mojo.el --- Interactive functions to aid the development of webOS apps
(defconst mojo-version "0.9.2")

(require 'json)

;; Copyright (c)2008 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
;;              2009 Sami Samhuri
;;
;; Authors: Jonathan Arkell <jonnay@jonnay.net>
;;          Sami Samhuri <sami.samhuri@gmail.com>
;;
;; Latest version is available on github:
;;     http://github.com/samsonjs/config/blob/master/emacs.d/mojo.el
;;
;; With sufficient interest mojo.el will get its own repo.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
(defgroup mojo '()
  "Interactive functions to aid the development of webOS apps.

This package is in early beta.  I am open to any contributions or
ideas.  Send me a pull request on github if you hack on mojo.el.")
  
;;; Installation:
;; Put json.el and mojo.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your Emacs init file.
;(require 'mojo)
;;
;; Make sure you customize the variables:
;; `mojo-project-directory', `mojo-sdk-directory' and `mojo-build-directory'
;;
;; I recommend that you define a few keyboard shortcuts in your .emacs file.
;; Maybe something like this:
;;
;;     (global-set-key [f2] 'mojo-generate-scene)
;;     (global-set-key [f3] 'mojo-emulate)
;;     (global-set-key [f4] 'mojo-package)
;;     (global-set-key [f5] 'mojo-package-install-and-inspect)
;;   
;; 

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `mojo-generate'
;;    Generate a new Mojo application in the `mojo-project-directory'.
;;  `mojo-generate-scene'
;;    Generate a new Mojo scene for the application in `mojo-root'.
;;  `mojo-emulate'
;;    Launch the palm emulator.
;;  `mojo-package'
;;    Package the current application.
;;  `mojo-install'
;;    Install the specified package for the current application.
;;    The emulator needs to be running.
;;  `mojo-list'
;;    List all installed packages.
;;  `mojo-delete'
;;    Remove application named APP-NAME.
;;  `mojo-launch'
;;    Launch the current application in an emulator.
;;  `mojo-close'
;;    Close launched application.
;;  `mojo-inspect'
;;    Run the dom inspector on the current application.
;;  `mojo-hard-reset'
;;    Perform a hard reset, clearing all data.
;;  `mojo-package-install-and-launch'
;;    Package, install, and launch the current app.
;;  `mojo-package-install-and-inspect'
;;    Package, install, and launch the current app for inspection.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `mojo-sdk-directory'
;;    Path to where the mojo SDK is.
;;    default = (case system-type ((windows-nt) "c:/progra~1/palm/sdk") (t ""))
;;  `mojo-project-directory'
;;    Directory where all your Mojo projects are located.
;;    default = ""
;;  `mojo-build-directory'
;;    Directory to build Mojo applications in.
;;  `mojo-debug'
;;    Run Mojo in debug mode.  Assumed true while in such an early version.
;;    default = t

;;; TODO:
;; 

;;; CHANGELOG:
;;
;; v 0.9.2 (bug fixes)
;;
;;       - reading json files no longer messes up your buffer history.
;;
;;       - app list completion works now (caching bug)
;;
;; v 0.9.1
;;
;;       - Added mojo-package-install-and-launch.
;;
;;       - New variable for specifying whether commands target the
;;         device or emulator, *mojo-target*.  Set it to 'usb' for a
;;         real device and 'tcp' for the emulator.  Defaults to 'tcp'.
;;         To set the default target you can use the convenience
;;         functions mojo-target-device and mojo-target-emulator.

;; v 0.9
;;
;;       - Automatically find Mojo project root by searching upwards
;;         for appinfo.json.
;; 
;;       - Added command for generating new scenes,
;;         mojo-generate-scene.
;;
;;       - mojo-package now operates only on the current project.
;;
;;       - Parse appinfo.json to get version, used for installing &
;;         launching with less interaction.
;;
;;       - mojo-install, mojo-launch, mojo-inspect, and mojo-delete
;;         still read in arguments but have the current project/app as
;;         the default values.
;; 
;;       - New convenience method: mojo-package-install-and-inspect
;;         This function only operates on the active app and does not
;;         read in any input.
;;
;;       - Remembered filenames and app ids are cleared when the Mojo
;;         project root changes. (DWIM)
;;
;;       - Parse output of `palm-install --list` for app id
;;         completion.  App id completion was ported from cheat.el.

;; v 0.2 - Fixed some minor bugs
;; v 0.1 - Initial release

;;; Code:

(defcustom mojo-sdk-directory
  (case system-type
	((windows-nt) "c:/progra~1/palm/sdk")
	((darwin) "/opt/PalmSDK/Current")
	(t ""))
  "Path to where the mojo SDK is.

Note, using the old-school dos name of progra~1 was the only way i could make
this work."
  :type 'directory
  :group 'mojo)

(defcustom mojo-project-directory ""
  "Directory where all your Mojo projects are located."
  :type 'directory
  :group 'mojo)

(defcustom mojo-build-directory ""
  "Directory where built projects are saved."
  :type 'directory
  :group 'mojo)

;;* debug
(defcustom mojo-debug t
  "Run Mojo in debug mode.  Assumed true while in such an early version."
  :type 'boolean
  :group 'mojo)
 

;;* interactive generate
(defun mojo-generate (title directory)
  "Generate a new Mojo application in the `mojo-project-directory'.

TITLE is the name of the application.
DIRECTORY is the directory where the files are stored."
  ;;TODO handle existing directories (use --overwrite)
  (interactive "sTitle: \nsDirectory Name (inside of mojo-project-directory): \n")
  (let ((mojo-dir (expand-file-name (concat mojo-project-directory "/" directory))))
	(when (file-exists-p mojo-dir)
		  (error "Cannot mojo-generate onto an existing directory! (%s)" mojo-dir))
	(make-directory mojo-dir)
	(mojo-cmd "palm-generate" (list "-p" (format "\"{'title':'%s'}\"" title)
					mojo-dir))
	(find-file (concat mojo-dir "/appinfo.json"))))

;;* interactive
(defun mojo-generate-scene (name)
  "Generate a new Mojo scene for the current application.

NAME is the name of the scene."
  (interactive "sScene Name: \n")
  (let ((mojo-dir (mojo-root)))
    (mojo-cmd "palm-generate" (list "-t" "new_scene"
				    "-p" (format "name=%s" name) mojo-dir))
    (find-file (format "%s/app/assistants/%s-assistant.js" mojo-dir name))
    (find-file (format "%s/app/views/%s/%s-scene.html" mojo-dir name name))))

;;* interactive 
(defun mojo-emulate ()
  "Launch the palm emulator."
  (interactive)
  (mojo-cmd "palm-emulator" nil))

;;* interactive 
(defun mojo-package ()
  "Package the current application into `MOJO-BUILD-DIRECTORY'."
  (interactive)
  (mojo-cmd "palm-package" (list "-o" (expand-file-name mojo-build-directory)
				 (mojo-root))))

;;* interactive 
(defun mojo-install ()
  "Install the package named by `MOJO-PACKAGE-FILENAME'. The emulator needs to be running."
  (interactive)
  (mojo-cmd "palm-install" (list (expand-file-name (mojo-read-package-filename))))
  (mojo-invalidate-app-cache))

;;* interactive 
(defun mojo-list ()
  "List all installed packages."
  (interactive)
  (mojo-cmd "palm-install" (list "--list")))

;;* interactive 
(defun mojo-delete ()
  "Remove the current application using `MOJO-APP-ID'."
  (interactive)
  (mojo-cmd "palm-install" (list "-r" (mojo-read-app-id)))
  (mojo-invalidate-app-cache))

;;* interactive 
(defun mojo-launch ()
  "Launch the current application in an emulator."
  (interactive)
  (mojo-cmd "palm-launch" (list (mojo-read-app-id))))

;;* interactive 
(defun mojo-close ()
  "Close launched application."
  (interactive)
  (mojo-cmd "palm-launch" (list "-c" (mojo-read-app-id))))

;;* launch interactive
(defun mojo-inspect ()
  "Run the DOM inspector on the current application."
  (interactive)
  (mojo-cmd "palm-launch" (list "-i" (mojo-read-app-id))))

;;* emulator interactive
(defun mojo-hard-reset ()
  "Perform a hard reset, clearing all data."
  (interactive)
  (mojo-cmd "palm-emulator" (list "--reset")))

(defun mojo-browse ()
  "Use `browse-url' to visit your application with Palm Host."
  (browse-url "http://localhost:8888"))


;;* interactive 
(defun mojo-package-install-and-inspect ()
  "Package, install, and launch the current application for inspection."
  (interactive)
  (mojo-package)
  (mojo-cmd "palm-install" (list (expand-file-name (mojo-package-filename))))
  (mojo-cmd "palm-launch" (list "-i" (mojo-app-id))))

;;* interactive 
(defun mojo-package-install-and-launch ()
  "Package, install, and launch the current application."
  (interactive)
  (mojo-package)
  (mojo-cmd "palm-install" (list (expand-file-name (mojo-package-filename))))
  (mojo-cmd "palm-launch" (list (mojo-app-id))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some support functions that grok the basics of a Mojo project. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun drop-last-path-component (path)
  "Get the head of a path by dropping the last component."
  (if (< (length path) 2)
      path
    (substring path 0 (- (length path)
			 (length (last-path-component path))
			 1)))) ;; subtract one more for the trailing slash

(defun last-path-component (path)
  "Get the tail of a path, i.e. the last component."
  (if (< (length path) 2)
      path
    (let ((start -2))
      (while (not (string= "/" (substring path start (+ start 1))))
	(setq start (- start 1)))
      (substring path (+ start 1)))))

(defvar *mojo-last-root* ""
  "Last Mojo root found by `MOJO-ROOT'.")

(defun mojo-root ()
  "Find a Mojo project's root directory starting with `DEFAULT-DIRECTORY'."
  (let ((last-component (last-path-component default-directory))
	(dir-prefix default-directory))
    ;; remove last path element until we find appinfo.json
    (while (and (not (file-exists-p (concat dir-prefix "/appinfo.json")))
		(not (< (length dir-prefix) 2)))
      (setq last-component (last-path-component dir-prefix))
      (setq dir-prefix (drop-last-path-component dir-prefix)))

    ;; If no Mojo root found, ask for a directory.
    (if (< (length dir-prefix) 2)
	(setq dir-prefix (mojo-read-root)))

    ;; Invalidate cached values when changing projects.
    (if (or (blank *mojo-last-root*)
	    (not (string= dir-prefix *mojo-last-root*)))
	(progn
	  (setq *mojo-last-root* dir-prefix)
	  (setq *mojo-package-filename* nil)
	  (setq *mojo-app-id* nil)))

    dir-prefix))

(defun read-json-file (filename)
  "Parse the JSON in FILENAME and return the result."
  (save-excursion
    (let ((origbuffer (current-buffer))
	  (filebuffer (find-file-noselect filename)))
      (set-buffer filebuffer)
      (let ((text (buffer-string)))
	(switch-to-buffer origbuffer)
        (json-read-from-string text)))))

(defun mojo-app-version ()
  "Parse the project version from the appinfo.json file in `MOJO-ROOT'."
  (let ((appinfo (read-json-file (concat (mojo-root) "/appinfo.json"))))
	(cdr (assoc 'version appinfo))))

(defun mojo-app-id ()
  "Parse the project id from the appinfo.json file in `MOJO-ROOT'."
  (let ((appinfo (read-json-file (concat (mojo-root) "/appinfo.json"))))
    (cdr (assoc 'id appinfo))))

(defun mojo-package-filename ()
  "Get the package filename for the specified application."
  (format "%s/%s_%s_all.ipk" (expand-file-name mojo-build-directory)
	  (mojo-app-id) (mojo-app-version)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; app listing and completion ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mojo-app-cache-filename* nil)
(defun mojo-app-cache-file (&optional force-reload)
  "Cache the list of applications in a temporary file.  Return the filename."
  (when (or force-reload
	  (not *mojo-app-cache-filename*))
    (setq *mojo-app-cache-filename* (make-temp-file "mojo-app-list-cache"))
    (save-excursion
      (let ((buffer (find-file-noselect *mojo-app-cache-filename*))
	    (apps (mojo-fetch-app-list)))
	(set-buffer buffer)
	(insert (string-join "\n" apps))
	(basic-save-buffer)
	(kill-buffer buffer))))
  *mojo-app-cache-filename*)

(defvar *mojo-app-id* nil
  "Most recently used application id.")

(defvar *mojo-package-filename* nil
  "Most recently used package file.")

(defvar *mojo-app-history* nil
  "List of the most recently used application ids.")

;; cache expires hourly by default
(defvar *mojo-app-cache-ttl* 3600
  "The minimum age of a stale cache file, in seconds.")

(defvar *mojo-package-history* nil
  "List of the most recently used package filenames.")

;; this is from rails-lib.el in the emacs-rails package
(defun string-join (separator strings)
  "Join all STRINGS using SEPARATOR."
  (mapconcat 'identity strings separator))

(defun blank (thing)
  "Return T if THING is nil or an empty string, otherwise nil."
  (or (null thing)
      (and (stringp thing)
           (= 0 (length thing)))))

(defun mojo-read-root ()
  "Get the path to a Mojo application, prompting with completion and
  history."
  (read-file-name "Mojo project: " (expand-file-name (concat mojo-project-directory "/"))))

(defun mojo-read-package-filename ()
  "Get the filename of a packaged application, prompting with completion and
  history.

The app id is stored in *mojo-package-filename* unless it was blank."
  (let* ((default (or *mojo-package-filename*
		      (mojo-package-filename)))
         (package (read-file-name (format "Package file (default: %s): " default)
				  (concat mojo-build-directory "/") default t)))
    (setq *mojo-package-filename* (last-path-component package))
    (expand-file-name package)))

(defun mojo-read-app-id (&optional prompt)
  "Get the id of an existing application, prompting with completion and
  history.

The app id is stored in *mojo-app-id* unless it was blank."
  (let* ((default (or *mojo-app-id* (mojo-app-id)))
         (prompt (or prompt
		     (format "App id (default: %s): " default)))
         (app-id (completing-read prompt
				  (mojo-app-list t)
				  nil
				  t
				  nil
				  '*mojo-app-history*
				  default)))
    (when (blank app-id)
      (setq app-id (mojo-app-id)))
    (setq *mojo-app-id* app-id)
    app-id))

(defun mojo-app-list (&optional fetch-if-missing-or-stale)
  "Get a list of installed Mojo applications."
  (cond ((and (file-readable-p (mojo-app-cache-file))
              (not (mojo-app-cache-stale-p)))
         (save-excursion
           (let* ((buffer (find-file (mojo-app-cache-file)))
                  (apps (split-string (buffer-string))))
             (kill-buffer buffer)
             apps)))
        (fetch-if-missing-or-stale
         (mojo-app-cache-file t) ;; force reload
         (mojo-app-list)) ;; guaranteed cache hit this time
        (t nil)))

(defun mojo-fetch-app-list ()
  "Fetch a fresh list of all applications."
  (let* ((raw-list (nthcdr 7 (split-string (mojo-cmd-to-string "palm-install" (list "--list")))))
	 (apps (list))
	 (appname-regex "^[^0-9][^.]+\\(\\.[^.]+\\)+$")
	 (item (pop raw-list)))
    (while item
	(if (string-match appname-regex item) ;; liberal regex for simplicity
	  (push item apps)
	(print (concat "discarding " item)))
	(setq item (pop raw-list)))
    (nreverse apps)))

(defun mojo-app-cache-stale-p ()
  "Non-nil if the cache in `MOJO-APP-CACHE-FILE' is more than
  *mojo-app-cache-ttl* seconds old.

If the cache file does not exist then it is considered stale."
  (or (null (file-exists-p (mojo-app-cache-file)))
      (let* ((now (float-time (current-time)))
             (last-mod (float-time (sixth (file-attributes
             (mojo-app-cache-file)))))
             (age (- now last-mod)))
        (> age *mojo-app-cache-ttl*))))

(defun mojo-invalidate-app-cache ()
  "Delete the app list cache."
  (delete-file (mojo-app-cache-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;* lowlevel luna
(defun mojo-luna-send (url data)
  "Send something through luna.

Luna-send is a program to send things like incoming calls, GPS status, SMS,
etc.  to your emulator.

This is a low level Emacs interface to luna-send.
URL is the luna url, and DATA is the data."
  (mojo-cmd "luna-send" (list "-n" "1" url data)))

(defvar *mojo-target* "tcp"
  "Used to specify the target platform, \"usb\" for the device
  and \"tcp\" for the emulator.  Deaults to \"tcp\".")

(defun mojo-target-device ()
  "Specify that Mojo commands should target a real device.

Sets `*mojo-target*' to \"usb\"."
  (setq *mojo-target* "usb"))

(defun mojo-target-emulator ()
  "Specify that Mojo commands should target a real device.

Sets `*mojo-target*' to \"tcp\"."
  (setq *mojo-target* "tcp"))

(defun mojo-path-to-cmd (cmd)
  "Return the absolute path to a Mojo SDK command line program."
  (case system-type
    ((windows-nt) (concat mojo-sdk-directory "/bin/" cmd ".bat"))
    (t (concat mojo-sdk-directory "/bin/" cmd))))

;;* lowlevel cmd
(defun mojo-cmd (cmd args &optional target)
  "General interface for running mojo-sdk commands.

CMD is the name of the command (without path or extension) to execute.
 Automagically shell quoted.
ARGS is a list of all arguments to the command.
 These arguments are NOT shell quoted."
  (let ((cmd (mojo-path-to-cmd cmd))
	(args (concat "-d " (or target *mojo-target*) " "
		       (string-join " " args))))
    (if mojo-debug (message "running %s with args %s " cmd args))
    (shell-command (concat cmd " " args))))

;;* lowlevel cmd
(defun mojo-cmd-to-string (cmd args &optional target)
  "General interface for running mojo-sdk commands and capturing the output
   to a string.

CMD is the name of the command (without path or extension) to execute.
 Automatically shell quoted.
ARGS is a list of all arguments to the command.
 These arguments are NOT shell quoted."
  (let ((cmd (mojo-path-to-cmd cmd))
	(args (concat "-d " (or target *mojo-target*) " "
		       (string-join " " args))))
    (if mojo-debug (message "running %s with args %s " cmd args))
    (shell-command-to-string (concat cmd " " args))))

(provide 'mojo)

;;; mojo ends here
