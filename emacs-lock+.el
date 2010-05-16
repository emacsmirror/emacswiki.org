;;; emacs-lock+.el --- extensions to standard library `emacs-lock.el'


;; Author: Michael Heerdegen
;; Maintainer: Michael Heerdegen (concat "michael_heerdegen" "@" "web" ".de")
;; Copyright (C) 2010, Michael Heerdegen, all rights reserved.
;; Created: <2010-05-14 Fr>
;; Last-Updated: -
;; URL: http://www.emacswiki.org/cgi-bin/wiki/emacs-lock+
;; Keywords: lock, extensions
;; Compatibility: Gnu Emacs 23.x (others maybe, but not tested)


;;; Commentary:
;;
;;  emacs-lock+ - extensions to standard library `emacs-lock.el'
;;
;;  Adds the following features:
;;
;;  - When locking any buffer, you are prompted for a little note that
;;    helps you to remember why you locked the buffer.
;;  
;;  - If any buffer prevents Emacs from exiting, pops up this buffer
;;    and displays your note, if there is one. Also displays the note
;;    if you try to kill any locked buffer.
;;  
;;  - Adds a mode-line indicator which displays a capital "L" for
;;    locked buffers (and a lowercase "l" for non-locked buffers, if
;;    this is enabled. You can deactivate this indicator if you don't
;;    want it.)
;;    If you move the mouse over the indicator, the locking note
;;    is displayed.
;;
;;  Installation: Put this file into a directory in your load-path and
;;  byte compile it.  Add
;;
;;     (require 'emacs-lock+)
;;
;;  to your Emacs init file (usually ~/.emacs).
;;
;;  See (customize-group "emacs-lock") for options.
;;

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;; Change log:
;;
;; created, 2010_05_14_00-44-51

;;; Code:


(require 'emacs-lock)

(defgroup emacs-lock nil "Prevents you from exiting if a buffer is locked."
  :tag "Emacs Lock"
  :group 'editing)

(defcustom emacs-lock-indicator-in-modeline 'locked-only
  "*Determines the visibility of the buffer lock mode-line indicator.
nil means don't show the mode-line indicator.
locked-only means show indicator only if the current buffer is locked.
t means always show the indicator.

If the indicator is visible, it shows a capital L for locked buffers
and a lowercase l else. If you click on it with the mouse, you can
toggle the lock status of any buffer."
  :type '(choice
          (const :tag "Never show mode-line indicator" nil)
          (const :tag "Show for locked buffers only"   locked-only)
          (const :tag "Always show indicator"          t))
  :group 'emacs-lock
  :group 'mode-line)

(defcustom emacs-lock-always-use-notes-flag t
  "*Non-nil means always prompt for a note when locking any buffer."
  :type 'boolean :group 'emacs-lock)

(defun check-emacs-lock ()
  "Check if variable `emacs-lock-from-exiting' is non-nil for any buffer.
If any locked buffer is found, signal error and display the buffer's name.
If existent, also display locking note for this buffer."
  (interactive)
  (save-excursion
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when emacs-lock-from-exiting
	(pop-to-buffer buffer)
	(if (stringp emacs-lock-from-exiting)
	    (error "Buffer %s locked: %s" (buffer-name) emacs-lock-from-exiting)
	  (error "Emacs is locked from exit due to buffer: %s"
		 (buffer-name)))))))

(defun emacs-lock-check-buffer-lock ()
  "Check if variable `emacs-lock-from-exiting' is non-nil for a buffer.
If the buffer is locked, signal error and display its name.
If existent, also display locking note for this buffer."
  (cond
   ((stringp emacs-lock-from-exiting)
    (error "Can't delete %s, buffer locked: %s"
	   (buffer-name) emacs-lock-from-exiting))
   (emacs-lock-from-exiting
    (error "Buffer `%s' is locked, can't delete it" (buffer-name)))))

(defun toggle-emacs-lock (&optional arg)
  "Toggle `emacs-lock-from-exiting' for the current buffer.
If `emacs-lock-always-use-notes-flag' is non-nil, prompt for a
note when locking.  With non-nil ARG, always lock the current
buffer and prompt for a locking note."
  (interactive "P")
  (if (or arg
	  (and
	   (not emacs-lock-from-exiting)
	   emacs-lock-always-use-notes-flag))
      (setq emacs-lock-from-exiting
		(read-string "Locking note: " emacs-lock-from-exiting))
    (setq emacs-lock-from-exiting (not emacs-lock-from-exiting)))
  (when (equal emacs-lock-from-exiting "")
    (setq emacs-lock-from-exiting t))	;for help echo
  (force-mode-line-update)
  (if emacs-lock-from-exiting
      (message "Buffer is now locked")
    (message "Buffer is now unlocked")))

(defun mode-line-toggle-emacs-lock (event)
  "Toggle `emacs-lock-from-exiting' from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (toggle-emacs-lock)))

(defun mode-line-set-emacs-lock-note (event)
  "Lock buffer and prompt for a locking note from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (toggle-emacs-lock 'note)))


(let ((indicator
       `((:eval
	  (when
	    (if (eq emacs-lock-indicator-in-modeline 'locked-only)
		emacs-lock-from-exiting
	      emacs-lock-indicator-in-modeline)
	    (propertize
	     (if emacs-lock-from-exiting "L" "l")
	     'mouse-face 'mode-line-highlight
	     'help-echo
	     (lambda (window _ _)
	       (save-selected-window
		 (select-window window)
		 (if (stringp emacs-lock-from-exiting)
		     (format "Buffer locked: %s (mouse-1 unlocks)"
			     emacs-lock-from-exiting)
		   (format "\
Buffer is %s locked\nmouse-1 toggles\nmouse-2 sets locking note"
			   (if emacs-lock-from-exiting "" "not")))))
	     'local-map
	     ',(let ((map (make-sparse-keymap)))
		 (define-key
		   map
		   [mode-line mouse-1]
		   #'mode-line-toggle-emacs-lock)
		 (define-key
		   map
		   [mode-line mouse-2]
		   #'mode-line-set-emacs-lock-note)
		 map)))))))
  (unless
      (featurep 'emacs-lock+)		;don't add indicator twice
    (setq-default mode-line-modified
		  (append mode-line-modified indicator))))


(provide 'emacs-lock+)

;;; emacs-lock+.el ends here
