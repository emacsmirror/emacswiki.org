;;; emdroid.el --- Android Wrappers for Emacs

;; Copyright - (cc) Some Rights Reserved 2007 Jonathan Arkell
;; Author: jonnay <jonnay@jonnay.net>
;; Keywords: java

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
;; Why should eclipse guys get all the fun?  Here are some commands for
;; working with android

;;;  Install:
;; 1)  add to your .emacs somehow.
;; 2)  customize-group emdroid, and set the Emdroid Tools Dir to
;;     the location of the Adroid SDK Tools Directory.
;; 2a) optionally set the name of the emdroid activity creator
;;     executeable

;;; Use:
;; emdroid-create-activity
;;   Create a new activity.  Assumes the current directory.
;; emdroid-run
;;   Execute the emulator.
;; emdroid-install
;;   Asks for a file to install, and installs it on the emulator
;; emdroid-dmesg
;;   Prints out the kernel dmesg
;; emdrpoid-info
;;   Displays info of the device

;;; Bugs:
;; - I think my defcustoms are bunk, and the defaults have embedded quotes.

;;; Plan:
;; - Better usage and install docco
;; - Minor mode with key bindings
;; - more (useful) wrappers around adb commands
;; - wrappers around the ohter commands
;; - integration with jde, especially the debugger
;; - maybe some kinda sql-sqlite integration as well?
;; - dired hooks to copy files to and from the device easily?

;;; History:
;; * 0.1.1 - Added a real emacs prelude
;;         - Added command to start emulator
;;         - Added adb wrapper, it kinda sucks but is at least functional.
;;         - Wrote some wrapper commands
;; * 0.1.0 - First Ever version on EmacsWiki.  


(defgroup emdroid nil
  "Customizations for the Emdroid Package."
  :version "0.1"
  :group 'emdroid)

(defcustom emdroid-tools-dir
  nil
  "Directory where the emdroid tools are.  i.e  /android_sdk_windows_m3-rc22a/tools/"
  :group 'emdroid
  :type 'directory)

(defcustom emdroid-activity-creator
  "activityCreator.bat"
  "If you are on windows it is likely ActivityCreator.bat.  If you are on Linux or OS X it is likely ActivityCreator.py."
  :group 'emdroid
  :type 'string)

(defvar emdroid-emulator-process nil
  "Variable storing the emulator process state.")

(defvar emdroid-buffer nil
  "The output buffer for emdroids ADB.")

(defun emdroid-create-activity (class-name)
  "Prompts for a class name, and runs the create activity utility in the current directory."
  (interactive "sFully Qualified Class Name:")
  (shell-command (concat emdroid-tools-dir "/" emdroid-activity-creator " " class-name) "*Create-Activity*"))

(defun emdroid-emulator-live-p ()
  "Returns whether or not the emulator is live"
  (and emdroid-emulator-process
	   (eq (process-status emdroid-emulator-process)
		   'run)))

(defun emdroid-run ()
  "Executes the emulator"
  (interactive)
  (if (emdroid-emulator-live-p)
	  (message "Emulator already running.")
	  (progn
	   (setq emdroid-emulator-process (start-process "em-droid-emulator" nil (concat emdroid-tools-dir "/emulator")))
	   (message "Launching emulator."))))

(defun emdroid-adb (command args)
  (if (not (buffer-live-p emdroid-buffer))
	  (setq emdroid-buffer (get-buffer-create "*ADB*")))  
  (pop-to-buffer emdroid-buffer nil)
  (end-of-buffer)
  (shell-command (concat emdroid-tools-dir "/adb " command " " args) 'true))

(defun emdroid-install (file)
  "Installs an android APK"
  (interactive "fPackage To Install: ")
  (emdroid-adb "install" file))

(defun emdroid-dmesg ()
  "Show the linux dmesg"
  (interactive)
  (emdroid-adb "shell" "dmesg"))

(defun emdroid-info ()
  "Show info about the device.  For now just the serial and device #"
  (interactive)
  (emdroid-adb "get-product" "")
  (emdroid-adb "get-serialno" ""))

(provide 'emdroid)
;;; emdroid.el ends here
