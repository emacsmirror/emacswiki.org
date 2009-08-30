;;; devkitemacs.el --- Helper functions for working with the devkitpro package (NDS and PSP development)

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

;; Some emacsy goodness for devkit pro

;; This file is still REALLLY raw.  It will get better and better.

;; customize the group.  In particular, customize the devkitemacs-emulator, devkitemacs-card-path and devkitemacs-projects vars.
;; Once you have set up your projects, you can change the current project with devkitemacs-set-current-project.
;; Use the following fuctions to build and work with your project:
;;  - devkitemacs-build
;;  - devkitemacs-clean
;;  - devkitemacs-emulate
;;  - devkitemacs-install
;;  - devkitemacs-shell

;;; TODO:
; - Tighter integration with nds gambit would be a boon as well
; - Better documentation

;;; CHANGELOG:
;; 0.0.3
; - More documentation.
; - Fixed keymmap issue (thanks to help on #emacs)
; - Enabled Default project
;; 0.0.2
; - Way better documentation.  Still sucks
; - Changed vars to customs
; - Fixed some general problems

;;; BUGS:
; - The keymap is not working.  I am probably doing something dumb.
; - The current directory keeps getting wacked with the build command.

(defvar devkitemacs-mode-version "0.0.3")

(defgroup devkitemacs nil
  "Customizations for the devkitemacs environment.")

(defcustom devkitemacs-devkitpro-dir
  (getenv "DEVKITPRO")
  "Location of the devkitpro main directory."
  :type 'directory)

(defcustom devkitemacs-devkitarm-dir
  (getenv "DEVKITARM")
  "Location of the Arm Directory inside of devkitpro."
  :type 'directory)

(defcustom devkitemacs-add-path-evar
  t
  "Add devkitpro to the PATH environment var from within emacs.
This might be needed if your emacs shell is cygwin.  If the devkitpro
make files cannot find the DEVKITPRO or DEVKITARM environment
variables, then set this to true."
  :type 'boolean)

(defcustom devkitemacs-set-evars
  t
  "Set the applicable devkitpro environment vars (DEVKITPRO and DEVKITARM) from within emacs.
This might be needed if your emacs shell is cygwin.  If gcc-arm-eabi
or related commands, then set this to true. "
  :type 'boolean)

(defcustom
  devkitemacs-emulator
  "C:/devkitpro/DeSmuME/DeSmuME.exe"
  "Location of your NDS Emnulator"
  :type 'file)

(defcustom
  devkitemacs-projects
  '()
  "A list of projects.   This is an alist in the format of ('project-id dir)."
  :type '(alist :key-type symbol
			    :value-type directory))

(defcustom
  devkitemacs-card-path
  "e:"
  "Location of your USB Drive for installing files.  No trailing slash.
If it is the root of a windows drive, it should not have the trailing slash. For example: 'E:'."
  :type 'directory)

(defcustom
  devkitemacs-default-project
  nil
  "Default (startup) value of the current project."
  :type 'symbol)

(defvar devkitemacs-mode-map
  (let ((m (make-sparse-keymap)))
	(define-key m (kbd "C-c C-d b") 'devkitemacs-build)
	(define-key m (kbd "C-c C-d c") 'devkitemacs-clean)
	(define-key m (kbd "C-c C-d e") 'devkitemacs-emulate)
	(define-key m (kbd "C-c C-d i") 'devkitemacs-install)
	(define-key m (kbd "C-c C-d s") 'devkitemacs-set-current-project)
	(define-key m (kbd "C-c C-d |") 'devkitemacs-shell)
	m)
  "Keymap for devkitemacs.")

(defvar devkitemacs-current-project "" "The project currently being worked on.")

(setq devkitemacs-current-project devkitemacs-default-project)

(when devkitemacs-add-path-evar
	  (progn
	   (setenv "DEVKITPRO" devkitemacs-devkitpro-dir)
	   (setenv "DEVKITARM" devkitemacs-devkitarm-dir)))

(when devkitemacs-add-path-evar
	  (setenv "PATH" (concat (getenv "PATH") path-separator (getenv "DEVKITARM") "/bin")))

(defun devkitemacs-set-current-project (project)
  "Set the current project to the symbol"
  (interactive "SSet devkitemacs current project: ")
  ;; (let ((project (ido-completing-read "Set project to: "
;; 									  (mapcar (lambda (p) (symbol-name (car p))) devkitemacs-projects)
;; 									  nil
;; 									  t
;; 									  devkitemacs-current-project
;; 									  nil
;; 									  nil)))
  (setq devkitemacs-current-project project))

(defun devkitemacs-this-project-name ()
  devkitemacs-current-project)

(defun devkitemacs-this-project-dir ()
  (cdr (assoc devkitemacs-current-project devkitemacs-projects)))

(defun devkitemacs-install-file (file)
  "Copy a file to an SD card."
  (interactive "f Filename to copy to DS Card:")
  (copy-file file (concat devkitemacs-card-path "/" ) t)
  (message (concat "Copied " file " to " devkitemacs-card-path)))

(defun devkitemacs-make (name dir target)
  "Change directory to DIR, and runs make with TARGET in the window NAME."
  (let ((old-dir (file-name-as-directory default-directory))
		(this-buf (current-buffer)))
	(unwind-protect
	 (progn
	  (cd-absolute (file-name-as-directory dir))
	  (compile (concat "make -k " target))
;;  	  (set-buffer "*compilation*")
;;  	  (rename-buffer (concat "*" name "*"))
	  ))
	(set-buffer this-buf)
	(cd-absolute old-dir)))

; this function is likely kinda dumb
(defun devkitemacs-get-project-nds-file ()
  "Returns the current projects NDS file.
Which is to say, it will the the first .nds file found in the project directory."
  (let ((nds-file (directory-files (devkitemacs-this-project-dir) t ".*\.nds")))
	(if nds-file
		(car nds-file)
		(error "No .nds file found.  Please make sure your project is built with devkitemacs-build"))))

(defun devkitemacs-launch-emulator (file)
  "Launch the emulator for the file"
  (message devkitemacs-emulator)
  (start-process "devkitemacs-emu" nil devkitemacs-emulator file))

(defun devkitemacs-build ()
  "Run the make target against the current project"
  (interactive)
  (devkitemacs-make devkitemacs-current-project (devkitemacs-this-project-dir) ""))

(defun devkitemacs-clean ()
  "Run the clean target against the current project."
  (interactive)
  (devkitemacs-make devkitemacs-current-project (devkitemacs-this-project-dir) "clean"))

(defun devkitemacs-emulate ()
  "Launch the Emulator against the first .nds file in the project directory."
  (interactive)
  (devkitemacs-launch-emulator (devkitemacs-get-project-nds-file)))

(defun devkitemacs-install ()
  "Install the projects .nds file onto the card."
  (interactive)
  (devkitemacs-install-file (devkitemacs-get-project-nds-file)))

(defun devkitemacs-shell ()
  "Launch the preferred shell in your project directory.  This also reneames the buffer so that your main shell buffer is un-touched."
  (interactive)
  (let ((old-dir default-directory))
	(save-excursion
	 (cd (devkitemacs-this-project-dir))
	 (let ((n (concat "*" (symbol-name (devkitemacs-this-project-name)) "-shell*")))
	   (shell n)
	   (let ((p (get-buffer-process (get-buffer n))))
		 (comint-send-string p "echo \"Devkitemacs Shell\n\"\n"))))
	(cd old-dir)))

;; Building the Gambit NDS library specific functions here.
(defun devkitgambit-build
  "Build the gambit library, and then build the current project with it.  Try to install it as well."
  (interactive)
  (let ((cur-project devkitemacs-current-project))
	(set devkitemacs-current-project 'gambit-nds)
	(devkitemacs-build)
	(set devkitemacs-current-project cur-project) 
	(devkitemacs-build) ; set this as a post compile hook ... compilation-mode-finish-functions
	(devkitemacs-install)))


(define-minor-mode devkitemacs-mode
  "Toggle devkitemacs-mode
Globally bind some keys to the interactive compile functions.

\\{devkitemacs-mode-map}"
  nil
  " dkP"
  :keymap devkitemacs-mode-map
  :global t
  :group 'devkitemacs 
  :version devkitemacs-mode-version)

(provide 'devkitemacs)
;;; devkitemacs.el ends here
