;;; mojo.el --- Interactive functions to aid the development of Palm Pre apps
(defconst mojo-version "0.2")
;; Copyright (c)2008 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
;; Author: Jonathan Arkell <jonnay@jonnay.net>

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
  "Interactive functions to aid the development of Palm Pre apps.

This package is in Early Beta, (they did just release the SDK).
I am open to any contributions or ideas.  For now just post on
the Emacs Wiki, but soon there will be a spot on github for it.")
  
;;; Installation:
;; Put mojo.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your Emacs init file.
;(require 'mojo)
;; Make sure you customize the variables:
;; `mojo-project-directory' and `mojo-sdk-directory'

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `mojo-generate'
;;    Generate a new Mojo application in the `mojo-project-directory'.
;;  `mojo-emulate'
;;    Launch the palm emulator.
;;  `mojo-package'
;;    Package up an application inside of DIR.
;;  `mojo-install'
;;    Install PACKAGE.  The emulator needs to be running.
;;  `mojo-list'
;;    List all installed packages.
;;  `mojo-delete'
;;    Remove application named APP-NAME.
;;  `mojo-launch'
;;    Launch application APP-NAME in an emulator.
;;  `mojo-close'
;;    Close launched application APP-NAME.
;;  `mojo-inspect'
;;    Run the dom inspector on APP-NAME.
;;  `mojo-hard-reset'
;;    Perform a hard reset, clearing all data.
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
;;  `mojo-debug'
;;    Run Mojo in debug mode.  Assumed true while in such an early version.
;;    default = t

;;; TODO:
;; 

;;; CHANGELOG:
;; v 0.2 - Fixed some minor bugs
;; v 0.1 - Initial release

;;; Code:

(defcustom mojo-sdk-directory
  (case system-type
	((windows-nt) "c:/progra~1/palm/sdk")
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

;;* buffer const
(defconst mojo-buffer-name "*mojo*")

;;* buffer var
(defvar mojo-buffer nil
  "Buffer that spits out any mojo commandline messages.")

;;* debug
(defcustom mojo-debug t
  "Run Mojo in debug mode.  Assumed true while in such an early version."
  :type 'boolean
  :group 'mojo)
  
;;* interactive generate
(defun mojo-generate (title directory)
  "Generate a new Mojo application in the `mojo-project-directory'.

TITLE is the name of the application.
ID is the id of the application.
DIRECTORY is the directory where the files are stored."
  ;;TODO handle existing directories (use --overwrite)
  (interactive "sTitle: \nsDirectory Name (inside of mojo-project-directory): \n")
  (let ((mojo-dir (concat mojo-project-directory "/" directory)))
	(when (file-exists-p mojo-dir)
		  (error "Cannot mojo-generate onto an existing directory! (%s)" mojo-dir))
	(make-directory mojo-dir)
	(mojo-cmd "palm-generate" (list "-p" (format "\"{'title':'%s'}\"" title) mojo-dir))
	(find-file (concat mojo-dir "/appinfo.json"))))

;;* interactive 
(defun mojo-emulate ()
  "Launch the palm emulator."
  (interactive)
  (mojo-cmd "palm-emulator" nil))

;;* interactive 
(defun mojo-package (dir)
  "Package up an application inside of DIR."
  (interactive "DPackage up Application Dir: ")
  (let ((default-directory dir))
	(mojo-cmd "palm-package" (list dir))))

;;* interactive 
(defun mojo-install (package)
  "Install PACKAGE.  The emulator needs to be running."
  (interactive "fInstall Package File: ")
  (mojo-cmd "palm-install" (list package)))

;;* interactive 
(defun mojo-list ()
  "List all installed packages."
  (interactive)
  (mojo-cmd "palm-install" (list "--list")))

;;* interactive 
(defun mojo-delete (app-name)
  "Remove application named APP-NAME."
  (interactive "sDelete App: ")
  (mojo-cmd "palm-install" (list "-r" app-name)))

;;* interactive 
(defun mojo-launch (app-name)
  "Launch application APP-NAME in an emulator."
  (interactive "sApp Name to Launch: ")
  (mojo-cmd "palm-launch" (list app-name)))

;;* interactive 
(defun mojo-close (app-name)
  "Close launched application APP-NAME."
  (interactive "sPackage Name:")
  (mojo-cmd "palm-launch" (list "-c" app-name)))

;;* launch interactive
(defun mojo-inspect (app-name)
  "Run the dom inspector on APP-NAME."
  (interactive "sPackage Name:")
  (mojo-cmd "palm-launch" (list "-i" app-name)))

;;* emulator interactive
(defun mojo-hard-reset ()
  "Perform a hard reset, clearing all data."
  (interactive)
  (mojo-cmd "palm-emulator" (list "--reset")))

(defun mojo-browse ()
  "Use `browse-url' to visit your application with Palm Host."
  (browse-url "http://localhost:8888"))

;;* lowlevel luna
(defun mojo-luna-send (url data)
  "Send something through luna.

Luna-send is a program to send things like incoming calls, GPS status, SMS,
etc.  to your emulator.

This is a low level Emacs interface to luna-send.
URL is the luna url, and DATA is the data."
  (mojo-cmd "luna-send" (list "-n" "1" url data)))

(when nil
	  (mojo-get-app-list))

;;* lowlevel app list
(defun mojo-get-app-list ()
  "Retrieve list of all installed applications.

List is in the format of:
  (id version name)
and stored inside of `mojo--app-list'"
  (save-excursion
   (set-buffer mojo-buffer)
   (setq mojo--app-list (point))
   (set-process-sentinel (mojo-list) 'mojo--comint-list-sentinal)))

;;* var list
(defvar mojo--app-list nil
  "Variable for storing the current app list.")

;;* hook list
(defun mojo--comint-process-filter-applist (output)
  "Bunk function. Kept for reference.  To Be Removed."
  (if (string-match "\\([A-Za-z.]+\\) \\([0-9]+\\.[0-9.]+\\) \"\\(.+\\)\"")
	  (aput mojo--app-list (match-string 1) (list (match-string 2) (match-string 3)))))

;;* hook list
(defun mojo--list-sentinal (proc state)
  "Still in progress."
  (if mojo-debug (message "Process got state %s" state))
  (if (integerp mojo--app-list)
	  (save-excursion
	   (set-buffer mojo-buffer)
	   (goto-char mojo-app-list)
	   (line-down 2))))

;;* lowlevel cmd
(defun mojo-cmd (cmd args)
  "General interface for running mojo-skd commands.

CMD is the name of the command (without path or extension) to execute.
 Automagically shell quoted.
ARGS is a list of all arguments to the command.
 These arguments are NOT shell quoted."
  (when (or (null mojo-buffer)
			(not (buffer-live-p mojo-buffer)))
		(setq mojo-buffer (get-buffer-create mojo-buffer-name)))
  (let ((cmd (case system-type
			   ((windows-nt) (concat mojo-sdk-directory "/bin/" cmd ".bat"))
			   (t (concat mojo-sdk-directory "/bin/" cmd)))))
	(if mojo-debug (message "running %s with args %s " cmd args))
	(apply 'start-process "mojo" mojo-buffer cmd args)))

(provide 'mojo)

;;; mojo ends here
