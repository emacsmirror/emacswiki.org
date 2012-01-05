;;; save-and-dwim.el --- save, then do something after saving.
(defconst sdwim-version "0.1a")
;; Copyright (c) 2008 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
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



;; The canonical version of this file lives on emacswiki.


;;; Commentary:
;; This file is currently in a beta state.  It needs some testing.

;; Frequently after saving a file, I will want to do something with it,
;; like compile, install and test on an emulator; send to a moz-repl;
;; load the elisp file and execute an interactive command.
;;
;; This elisp library will help automate these kinds of tasks.
;;
;; the theory is, that you have a discreet set of tasks when you save a
;; file based on mode (i.e. for a .el: save and load the file, for a .c
;; file: save then compile the file).  And then a set of tasks depending
;; on the file itself (execute the emacs command foo-bar-baz, load the
;; emulator and test, whatever.)
;; 
;; Per-mode sdwim tasks are set up on the mode setup hook.  Sometimes
;; you will have more tasks to do after a save (such as install
;; a compiled and linked file to a location, and run an emulator).
;; You can either write an elisp function to do that, or you can
;; use the kbdmacro facility provided.
;;
;; To use the kbdmacro facility, all you do is save your file, turn
;; on macro recording, and do your post-save-task once.  Finish
;; recording, and call `sdwim-set-last-kbdmacro-as-sdwim-action'.
;; It will save the current kmacro as a sdwim action.
;;
;; For instance, you could go to the top of the file, search for
;; the string ";;--run-tests--", go down a line, forward a sexp
;; and then eval the expression.  Assuming you had your unit
;; testing code all in one place, it would launch it all right
;; after save.
;;
;; If you need to, you can always record new macros, and use
;; `sdwim-set-last-kbdmacro-as-sdwim-action' again.  Each
;; macro is named before it is applied to the save hook for
;; sdwim.
;;
;;; Commands:
;;
;; Below are complete command list:
;;
;;  `sdwim-save'
;;    This saves the file and then runs the registered SDWIM hooks.
;;  `sdwim-set-last-kbdmacro-as-sdwim-action'
;;    Gets the current keyboard macro, and turns it into a sdwim action that gets executed.
;;  `sdwim-set-kbdmacro-as-sdwim-action'
;;    Sets the named keyboard macro as a sdwim action to be executed.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `sdwim-install-defaults'
;;    Install the default SDWIM hooks.
;;    default = nil
;;  `sdwim-override-save-keycommand'
;;    Override the C-x C-s command to run save-dwim instead.
;;    default = nil


;;; Installation:
;; put this file on your load-path somewhere, and load it in your .emacs
;; i.e. (require 'sdwim)


;;; TODO:
;; Look at the save hook to hook in sdwim

;; build in some really basic intelligence.  Start by looking at what
;; the user does after hitting save, if they execute the same task more
;; then x times in a row, offer that as a potential buffer-local-post-save
;; option.
;;
;; Allow some kind of project support so that tasks can be set up on a
;; per project basis, as well as a per file basis.
;;

;;; CHANGELOG:
;;; v 0.1 - First release.

;;; BUGS:

(defgroup sdwim nil
  "Group for Save and Do What I mean.") 

;;* custom 
(defcustom sdwim-install-defaults nil
   "Install the default SDWIM hooks.

This will install some typical good hooks for a few programming modes on startup.
See the function `sdwim-install-default-hooks' for an example of how to set things up.

This only gets executed once SDWIM is loaded."
   :type 'boolean
   :group 'sdwim)

;;* custom
(defcustom sdwim-override-save-keycommand nil
   "Override the C-x C-s command to run save-dwim instead.

This only gets executed once SDWIM is loaded."   
   :type 'boolean
   :group 'sdwim)

;;* var hooks
(defvar sdwim-hooks '()
  "List of functions to run after a save.")

(make-variable-buffer-local 'sdwim-hooks)

;; var save flag
(defvar sdwim-saving nil
  "Flag to protect ourselves from running into an infinite loop.

For instance, with moz mode, you can only `moz-save-buffer-and-send',
you can't just send the file, so we need to be sure that we don't
run into an infinite loop, because sdwim calls a funciton that
saves the file.

This is mostly a protection for if save has been advised.  Not that
it has been really heavily tested yet.")

;;* save interactive
(defun sdwim-save ()
  "This saves the file and then runs the registered SDWIM hooks."
  (interactive)
  (when (not sdwim-saving)
    (setq sdwim-saving 't)
	(unwind-protect
	 (progn
	  (save-buffer)
	  (run-hooks 'sdwim-hooks))
	 (setq sdwim-saving nil))))

;;* macro interactive
(defun sdwim-set-last-kbdmacro-as-sdwim-action ()
  "Gets the current keyboard macro, and turns it into a sdwim action that gets executed.

Warning, this will not persist beyond the current editing session."
  (interactive)
  (let ((kmacro-name (intern (concat "sdwim-*" buffer-file-name "*"))))
	(name-last-kbd-macro kmacro-name)
	(add-hook 'sdwim-hooks `(lambda () (execute-kbd-macro (quote ,kmacro-name))) t)))

;;* macro interactive
(defun sdwim-set-kbdmacro-as-sdwim-action (kmacro-name)
  "Sets the named keyboard macro as a sdwim action to be executed.

Warning, this will not persist beyond the current editing session."
  (interactive "SKeyboard Macro: ")
  (add-hook 'sdwim-hooks `(lambda () (execute-kbd-macro (quote ,kmacro-name))) t))

;;* init helper example
(defun sdwim-install-default-hooks ()
   "Install the default hooks for sdwim.

This also serves as an example as to how to add your own default SDWIM hooks"
   (add-hook 'emacs-lisp-mode-hook '(lambda () (add-hook 'sdwim-hooks 'eval-buffer)))
   (add-hook 'c-mode-hook '(lambda () (add-hook 'sdwim-hooks 'compile)))
   (add-hook 'c++-mode-hook '(lambda () (add-hook 'sdwim-hooks 'compile)))
   (add-hook 'php-mode-hook '(lambda () (add-hook 'sdwim-hooks 'php-check-syntax)))
   (when (featurep 'moz) (add-hook 'emacs-lisp-mode-hook '(lambda () (add-hook 'sdwim-hooks 'moz-save-buffer-and-send)))))

;;* init 
(when sdwim-install-defaults
	  (sdwim-install-default-hooks))

;;* init
(when sdwim-override-save-keycommand
	  (global-set-key "C-x C-s" 'save-dwim))

(provide 'sdwim)

;;* test
(defun sdwim-test ()
   "A simple test function to see if SDWIM works."
   (todochiku-message "Works!" "SDWIM works!" (todochiku-icon 'star)))

(when nil
  (add-hook 'sdwim-hooks 'sdwim-test)
  (todochiku-message "SDWIM-macro" "SDWIM macro test works!" (todochiku-icon 'star))
  )
