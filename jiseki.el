;;; jiseki.el --- Desktop for emacs.
(defconst jiseki-version "0.3")
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
(defgroup jiseki '()
  "A desktop for emacs.")
  
;;; Installation:
;; Put jiseki.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your emacs init file.
;(require 'jiseki)
;;
;; You can bind jiseki-dwim to a shortcut key.  I use f3:
; (global-set-key [f3] 'jiseki-dwim)
;;
;; you will want to add some desktop items in the configuration.  The default
;; configuration will get you started.
;;
;;; jiseki configuration
;; jiseki is configured by setting the variable jiseki-sources.
;; for more information see the documentation of that variable.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `jiseki-desktop'
;;    Clear all the windows and show the jiseki desktop.
;;  `jiseki-dwim'
;;    If Jiseki is not visible, push it onto the stack.  Otherwise pop.
;;  `jiseki-push'
;;    Push the jiseki desktop onto the top of the stack, and get ready to hide it.
;;  `jiseki-pop'
;;    Pop the other window configuration before jiseki-push was invoked.
;;  `jiseki-cancel-timer'
;;    Cancel the volatile timer
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `jiseki-post-display-hook'
;;    Hooks to run once jiseki is displayed.
;;    default = (quote nil)

;;; TODO:
;; - Add images
;; - Fix the volitile sources (better optimization)
;; - random thing source
;; - erc vm and gnus sources.

;;; CHANGELOG:
;; v 0.3 - Determined that the point-jumping bug only happens with
;;         ecb active, and a speedbar ecb window
;;       - Added more panels.
;;       - added jiseki-dwim to push or pop the jiseki window.
;;       - Made the pop button work better.
;; v 0.2 - Fixed bad code for close window-config
;;       - fixed timers a little better
;;       - tagged the code.
;;       - added some more interesting sources.
;;       - first attempt at fixing the point jump bug.
;; v 0.1 - Initial release

;;* custom hook
(defcustom jiseki-post-display-hook '()
   "Hooks to run once jiseki is displayed."
   :type 'hook
   :group 'jiseki)

;;* custom face
(defface jiseki-item-face
  '((default
	  (:family "helv"
	   :box (:line-width 2 :color "grey85" :style nil)
	   :foreground "grey50"
	   :background "grey80")))
  "Face for each jiseki item."
  :group 'jiseki) 

;;* custom face
(defface jiseki-button-face
  '((default
	  (:foreground "grey50"
	   :background "grey70"
	   :inherit (jiseki-item-face)
	   :box (:line-width 2 :color "grey80" :style released-button)) ))
  "Face for jiseki items that have an action behind them."
  :group 'jiseki)

;;* const
(defconst jiseki-buffer-name "*Jiseki*")

;;* const timer
(defconst jiseki-timer-time 2)

;;* source 
(defvar jiseki-jiseki-source '())
(setq jiseki-jiseki-source
  '((name . "Jiseki source")
	(display . (lambda () (concat " Jiseki 自席 - " jiseki-version " ")))))

;;* source time
(defvar time-jiseki-source '())
(setq time-jiseki-source
  '((name . "Time")
	(volatile)
	(display . (lambda () (format-time-string  " %Y/%m/%d %H:%M:%S ")))))

;;* source time
(defvar unix-time-jiseki-source '())
(setq unix-time-jiseki-source
	  '((name . "Unix Time")
		(volatile)
		(display . (lambda () (format-time-string " Unix Time:%s ")))))

;;* source time
(defvar emacs-time-jiseki-source '())
(setq emacs-time-jiseki-source
	  '((name . "Emacs run time")
		(volatile)
		(display . (lambda () (let ((et (get-internal-run-time)))
								(format " EmacsTime: %d%d.%d " (car et) (cadr et) (nth 2 et)))))))

;;* source time
(defvar sunset-sunrise-jiseki-source '())
(setq sunset-sunrise-jiseki-source
	  '((name . "Sunrise and Sunset times.")
		(volatile)
		(display . (lambda () (let ((s (solar-sunrise-sunset (calendar-current-date))))
								(format " 日出 %s | 入相 %s | 昼光 %s "
										(apply 'solar-time-string (car s))
										(apply 'solar-time-string (cadr s))
										(caddr s)))))))

;;* source memory
(defvar consed-jiseki-source '())
(setq consed-jiseki-source
	  '((name . "Information on what has been consed.")
		(volatile)
		(display . (lambda () (format " Consed:  ():%10d  0.1:%10d  []:%12d  ':%10d  \":%10d  i:%10d"
									  cons-cells-consed
									  floats-consed
									  vector-cells-consed
									  symbols-consed
									  strings-consed
									  intervals-consed)))))

;;* source memory
(defvar garbage-collection-jiseki-source '())
(setq garbage-collection-jiseki-source
	  '((name . "Display GC stats")
		(volatile)
		(display . (lambda () (format " Garbage #%d %0.1fs " gcs-done gc-elapsed)))))

;;* source process
(defvar processes-source '())
(setq processes-source
	  '((name . "Show All Open Processes")
		(volatile)
		(display . (lambda () (apply 'concat " Proc: "
									 (mapcar (lambda (p) (concat " " (process-name p) " "))
 											 (process-list)))))))

;;* source external
(defvar twit-jiseki-source '())
(when (featurep 'twit)
	  (setq twit-jiseki-source
			'((name . "Shows the most recent tweet.")
			  (volatile)
			  (action . twit-show-recent-tweets)
			  (display . (lambda () (if (not (null (cadr twit-last-tweet)))
										(format "%s: %s" (cadr twit-last-tweet) (caddr twit-last-tweet))
										" No Last Tweet. "))))))

;;* source net
(defvar net-jiseki-source '())
(setq net-jiseki-source
	  '((name . "Network info")
		(volatile)
		(display . (lambda () (apply 'concat (map '(lambda (x) (format "[%s:%s] " (car x) (cdr x))) (network-interface-list)))))))

;;* source stack
(defvar jiseki-pop-source '())
(setq jiseki-pop-source
	  '((name . "Puts a pop button down when jiseki-deskop is pushed.")
		(volatile)
		(action . jiseki-pop)
		(display .  (lambda () (if (window-configuration-p jiseki-window-config)
								   (progn
									(add-hook 'jiseki-post-display-hook 'jiseki-move-to-pop-button)
									" back ")
								   (progn
									(remove-hook 'jiseki-post-display-hook 'jiseki-move-to-pop-button)
									""))))))

;;* source work-timer
(defvar work-timer-source '())
(setq work-timer-source
	  '((name . "Display the state of the work timer.")
		(volatile)
		(display . (lambda () (case work-timer-state
								((not-running) "Not Running")
								((work-time) "Work")
								((chill-time) "Chill")
								(t "Unknown")))))) 

(defun jiseki-move-to-pop-button ()
  "move back to the Pop button after display."
  (backward-char 6))

;;* vars
(defvar jiseki-buffer nil "Buffer that the desktop is shown on.")
;;* vars timer
(defvar jiseki-timer nil "Timer to handle the display of volatile items")
;;* vars stack
(defvar jiseki-window-config nil "Storage for window configuration when pushing/popping")
;;* vars volatile
(defvar jiseki-volatile-hooks nil)

;;* interactive show timer hook
(defun jiseki-desktop ()
  "Clear all the windows and show the jiseki desktop."
  (interactive)
  (when (not (timerp jiseki-timer))
		(setq jiseki-timer (run-with-timer jiseki-timer-time jiseki-timer-time 'jiseki-run-volatile-timer)))
  (when (or (not (bufferp jiseki-buffer))
		  (not (buffer-live-p jiseki-buffer)))
	    (setq jiseki-buffer (get-buffer-create jiseki-buffer-name)))
  (switch-to-buffer jiseki-buffer)
  (when (not (member 'jiseki-cancel-timer kill-buffer-hook))
		(add-hook 'kill-buffer-hook 'jiseki-cancel-timer 't 't))
  (delete-other-windows)
  (jiseki-write-display)
  (end-of-buffer)
  (run-hooks 'jiseki-post-display-hook))

;;* interactive show hide
(defun jiseki-dwim ()
  "If Jiseki is not visible, push it onto the stack.  Otherwise pop.
This is analogous to the Mac dashboard key, which shows it or hides
it, depending on the state."
  (interactive)
  (if (window-configuration-p jiseki-window-config)
	  (jiseki-pop)
	  (jiseki-push)))

;;* interactive show stack
(defun jiseki-push ()
   "Push the jiseki desktop onto the top of the stack, and get ready to hide it."
   (interactive)
   (when (window-configuration-p jiseki-window-config)
		 (error "Jiseki is currently in a pushed state!  Try jiseki-pop."))
   (setq jiseki-window-config (current-window-configuration))
   (jiseki-desktop))

;;* interactive show stack timer
(defun jiseki-pop ()
   "Pop the other window configuration before jiseki-push was invoked."
   (interactive)
   (when (not (window-configuration-p jiseki-window-config))
		 (error "Jiseki is not currently in a pushed state!"))
   (set-window-configuration jiseki-window-config)
   (setq jiseki-window-config '())
   (jiseki-cancel-timer))

;;* display
(defun jiseki-write-display ()
   "Creates the jiseki-buffer if needed, and displays it."
   (set-buffer jiseki-buffer)
   (toggle-read-only -1)
   (erase-buffer)
   (mapc 'jiseki-write-item jiseki-sources)
   (mapc 'jiseki-setup-volatile-source jiseki-sources)
   (insert "\n")
   (toggle-read-only 1))

;;* display
(defun jiseki-write-item (source)
   "Used by jiseki show to write out an individual item."
   (let ((func (assoc 'display source))
		 (action (assoc 'action source))
		 (action-map (make-sparse-keymap)))
	 (when (not (null action))
		   (define-key action-map [(control mouse-1)] (cdr action))
		   (define-key action-map [mouse-1] (cdr action))
		   (define-key action-map [mouse-2] (cdr action))
		   (define-key action-map [mouse-3] (cdr action))
		   (define-key action-map [space] (cdr action))
		   (define-key action-map [enter] (cdr action)))
	 (if (null func)
		 (message "Jiseki source error, could not work with source %s" source)
		 (let ((insertion-text (apply (cdr func) '())))
		   (if (not (null action))
			   (insert (propertize insertion-text 'face 'jiseki-item-face 'pointer 'hand 'keymap action-map))
			   (insert (propertize insertion-text 'face 'jiseki-item-face)))
		   (when (= (car (posn-x-y (posn-at-point (point) (get-buffer-window jiseki-buffer))))
					0)
				 (backward-char (length insertion-text))
				 (insert "\n")
				 (forward-char (length insertion-text)))
		   (insert "  ")))))

;;* display volatile
(defun jiseki-setup-volatile-source (source)
   "Adds any volatile sources to the proper hook."
   (when (and (member '(volatile) source)
			  (not (member source jiseki-volatile-hooks)))
		 (setq jiseki-volatile-hooks (cons source jiseki-volatile-hooks))))

;;* todo:fixme display volatile timer
(defun jiseki-run-volatile-timer ()
   "Function that is run to update all volatile items."
   (save-window-excursion (save-excursion (jiseki-write-display)))) 

;;* volatile timer interactive
(defun jiseki-cancel-timer ()
  "Cancel the volatile timer"
  (interactive)
  (if (timerp jiseki-timer)
	  (progn (cancel-timer jiseki-timer)
			 (setq jiseki-timer nil))
	  (message "Timer wasn't running.  Nothing done.")))

;;* source
(defvar jiseki-sources '() "Variable listing all the items that should show on the desktop.
`jiseki-sources' is configured a lot like `anything-sources' or
`auto-complete-sources'.

It should be a list of sources.  A source is defined as a list
in the following format:
 ((name . \"Human readable name of the source\")
  (volatile) ; optional, in future it will force updating of the item.
  (display . display-fuction) ; the display function that is called.  Should return a string.
  (action . command)) ; a command that is executed when the item is clicked. ")

(setq jiseki-sources
	  (list jiseki-jiseki-source
			time-jiseki-source
			unix-time-jiseki-source
			emacs-time-jiseki-source
			processes-source
			twit-jiseki-source
			consed-jiseki-source
			garbage-collection-jiseki-source
			sunset-sunrise-jiseki-source
			jiseki-pop-source))

;;* work-timer source
;; Apparently the form needs to be quoted here?!?
(eval-after-load 'work-timer
   '(setq jiseki-sources
		 (append jiseki-sources (list work-timer-source))))

(provide 'jiseki)

