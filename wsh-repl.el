;;; wsh-repl.el --- Windows Scripting Host REPL (in javascript)
(defconst wsh-repl-version "0.2")
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

;;;; Commentary:
(defgroup wsh-repl '()
  "A windows Scritping Host com-int port and REPL.")


;;; Installation:
;; Put wsh-repl.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;;
;; Add this to your Emacs init file.
;(require 'wsh-repl)
;;
;; You will also need to add the wsh-repl.js file
;;  - Download it from here: http://bunny.jonnay.net/sites/bunny.jonnay.net/files/wsh-repl.js.txt
;;  - I don't (currently) have a good place for this javascript file.  Sorry.  Pick a place you like.
;;  - customize the variable wsh-repl-script with
;;    M-x customize-variable wsh-repl-script
;; and point it to your file.

;;; interactive commands:
;; inferior-wsh-start-process

;;; TODO:
;; - Provide some useful examples of interacting with the Windows Scripting Host
;; - Look at the possibility of a deeper elisp-wsh bridge
;; - do better \n handling, look at moz.el
;; - run through checkdoc

;;; CHANGELOG:
;; v 0.2 - Fix bug where the path to the javascript wasn't being expanded.
;;       - Added multiline support
;;       - added command wsh-save-and-send-file
;;       - added command wsh-send-file
;;       - added wsh-send-region
;;       - added some better \n handling, not 100% on it.
;;       - better elisp error handling on start-process.
;;       - fixed checkdoc errors.
;; v 0.1.1 - Commentary update
;; v 0.1 - Initial release

;;; Code:

(require 'comint)

(defconst wsh-start-multiline-mode "REPL.useMultiLineMode();")
(defconst wsh-end-multiline-mode "!GO")

(defvar wsh-repl-name "wsh")
(defvar inferior-wsh-buffer nil "The buffer that has the inferior wsh process.")

;;*fixme TODO
(defcustom wsh-repl-script ""
   "Path to the wsh REPL script."
   :type 'file
   :group 'wsh)

;;*mode
(define-derived-mode inferior-wsh-mode comint-mode "Inf-wsh"
  "Major mode for interacting with the windows scripting host."
  :syntax-table (if (boundp 'js2-mode) js2-mode-syntax-table js-mode-syntax-table)
  (setq comint-input-sender 'inferior-wsh-input-sender)
    (define-key inferior-moz-mode-map "\C-cc" (lambda () (interactive) (insert "REPL.")))
	(define-key inferior-moz-mode-map "\C-cm" (lambda () (interactive) (insert "REPL.useMultiLineMode();"))))

;;* file send interactive
(defun wsh-save-and-send-file ()
  "Save the current file, and sends it to wsh."
  (interactive)
  (save-buffer)
  (wsh-send-file buffer-file-name))

;;* file send interactive
(defun wsh-send-file (filename)
  "Sends the file FILENAME to the wsh repl using the inlude method."
  (interactive "fFile To Load: ")
  (comint-send-string (concat "REPL.include("
							  (replace-regexp-in-string "/" "\\\\" (convert-standard-filename filename))
							  ");")))

;;* regsion send interactive
(defun wsh-send-region (start end)
  "Sends the current region to the WSH buffer.

START and END is the region to send."
  (interactive "r")
  (comint-send-string (inferior-wsh-process) wsh-start-multiline-mode)
  (sleep-for 0 1)
  (comint-send-region (inferior-wsh-process) start end)
  (comint-sent-string (inferior-wsh-process) wsh-end-multiline-mode))

;;* process send
(defun wsh-send-string (string)
   "Send STRING to the windows script host REPL."
   (comint-send-string (inferior-wsh-process) string))

;;* process send filter
(defun inferior-wsh-input-sender (proc string)
  "Custom function to send input with `comint-send-input'.
Instead of sending input and newline separately like in
`comint-simple-send', here we *first* concatenate input and
newline, then send it all together.  This prevents newline to be
interpreted on its own.

Taken from moz.el

PROC is the process to send to, and STRING is the string to send."
  (comint-send-string proc (concat string "\n")))

;;* process
(defun inferior-wsh-process ()
  "Return an inferior wsh process, starting if needed."
  (or (if (buffer-live-p inferior-wsh-buffer)
		  (get-buffer-process inferior-wsh-buffer))
	  (progn
	   (inferior-wsh-start-process)
	   (inferior-wsh-process))))

;;* process
(defun inferior-wsh-start-process ()
   "Start an inferior wsh process.
This will run the hook `inferior-wsh-hook' after startingthe process
and setting up the buffer."
   (interactive)
   (condition-case err
	 (progn
	  (setq inferior-wsh-buffer
			(make-comint "wsh-repl" "/windows/system32/cscript.exe" nil (expand-file-name wsh-repl-script)))
	  (sleep-for 0 100)
	  (with-current-buffer inferior-wsh-buffer
						   (inferior-wsh-mode)
						   (run-hooks 'inferior-wsh-hook)))
	 (error (message "Could not make process for wsh-repl!  Error: %s" err))))

(provide 'wsh-repl)

(provide 'wsh-repl)

;;; wsh-repl.el ends here
