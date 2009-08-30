;;; swbuff-x.el --- Modifications to David Ponce's swbuff

;; Copyright (C) 2001, 2002, 2003  Free Software Foundation, Inc.

;; Author: Kahlil (Kal) HODGSON <dorge@tpg.com.au>
;; Keywords: files, convenience
;; X-URL: http://www.emacswiki.org/elisp/swbuff-x.el
;; Time-stamp: <2003-11-16 17:23:10 kahlil>

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A few modifications to David Ponce's most excellent swbuff package
;;
;; (1) Fix the display timer so that it doesn't interfere with other
;; packages e.g speedbar and ispell.
;;
;; (2) Maintain buffer list ordering so that only the first and last
;; buffer in a sequence are effected.
;;
;; (3) Exclude buffers whose major mode matches
;; `swbuff-exclude-mode-regexp' from the buffer list but include any
;; buffers that match `swbuff-include-buffer-regexps' (a couterpoint
;; to `swbuff-exclude-buffer-regexps'). Also If
;; `swbuff-this-frame-only' is non-nil exclude buffers displayed in
;; other visible frames.
;;
;; (4) New hook `swbuff-pre-switch-hook' for things you may want to do
;; before switching buffers.
;;
;; (5) New function `swbuff-kill-this-buffer' which useful for
;; selectively cleaning out your buffer list.
;;
;; (6) If `swbuff-start-with-current-centered' is non-nil buffer list
;; display starts with the current buffer roughly in the middle of the
;; display ordering.  This encourages the use of
;; `swbuff-previous-buffer' to get to buffers which would otherwise
;; appear at the end of the list.
;;
;; (7) New variables `swbuff-left' and `swbuff-right' as an
;; alternative to `swbuff-header', `swbuff-trailer' and
;; `swbuff-separator'.  This allows you to place brackets around the
;; buffer name.

;; (8) Display buffer name matching `swbuff-special-buffers-re' using
;; `swbuff-special-buffers-face'.

;; (9) Added variable `swbuff-modeline-format' to make the modeline of the
;; status window configurable (Thanks to Matthias Wedel).
;;

;;; Thanks:

;; Matthias Wedel for spotting some bugs and with XEmacs
;; compatibility.

;;; Code:

(require 'swbuff)

;;; User Variables

(defvar swbuff-this-frame-only t
"*If non-nil, buffers displayed in other visble or iconified frames are
skipped.  This is a convient way of temprorily excluding a particluar
buffer from your cycle list.")

(defvar swbuff-exclude-mode-regexp ""
  "Regular expression matching major modes to skip when cycling.")

(defvar swbuff-include-buffer-regexps '("")
  "List of regular expressions matching buffer names to include in the
`swbuf-buffer-list'.")

(defvar swbuff-pre-switch-hook nil
"Standard hook containing functions to be called before a switch.
You may make this buffer local. This may be useful for handling modes that
use more than one window for display. For example, VM use one (small)
window for it Summary buffer and the remaining frame for the Presentation
buffer.  Switching buffers and retaining the window configuration doesn't
make sense (at least to me) in this context, so I set the following hooks
to delete these extra windows before switching:

\(defun my-vm-mode-hook () \"Delete other windows before a switch.\"
  (make-local-hook 'swbuff-pre-switch-hook)
  (add-hook 'swbuff-pre-switch-hook #'delete-other-windows t t))

\(add-hook 'vm-mode-hook              #'my-vm-mode-hook)
\(add-hook 'vm-summary-mode-hook      #'my-vm-mode-hook)
\(add-hook 'vm-presentation-mode-hook #'my-vm-mode-hook)"
)

(defvar swbuff-start-with-current-centered nil
  "If t then when buffer list is presented, the current buffer will be
the middle one.")

;;(setq swbuff-delay-switch nil)
(defvar swbuff-delay-switch nil
  "If t the first call to `swbuff-next-buffer' or `swbuff-previous-buffer'
simply displays the buffer list rather than switching.")

;;(setq swbuff-display-intermediate-buffers nil)
(defvar swbuff-display-intermediate-buffers nil
  "If t each call to `swbuff-next-buffer' or `swbuff-previous-buffer'
in a sequence causes a new buffer to be displayed. If nil only the last buffer in the sequence is actually displayed.")

;; alternative to head tail and sep
(defvar swbuff-left ""
  "*String placed immediately before a buffer name in the status line.
For example, try \"(\".")
(defvar swbuff-right ""
  "*String placed immediately after a buffer name in the status line.
For example, try \")\".")

(defvar swbuff-special-buffers-re "^\\*"
  "Regular expression matching buffers that should receive special
highlighting in the buffer display.")

(defface swbuff-special-buffers-face
  '((t (:foreground "red" :bold nil :underline nil)))
  "Face for highlighting special buffers in swbuff display." )

;; Respect different Emacsen naming conventions, otherwise interactuve
;; help will not work as expected.

(if xemacsp
  (defvar swbuff-modeline-format ""
    "Adjust modeline of the status window.
See `mode-line-format' for a detailed format description.")
  (defvar swbuff-mode-line-format ""
    "Adjust modeline of the status window.
See `mode-line-format' for a detailed format description."))

;; quiet compilers for both Emacsen
(defvar modeline-format)
(defvar mode-line-format)

;;; Local Variables

;; Store the initial buffer-list, buffer, window, and frame at the
;; time the switch sequence was called.
(defvar swbuff-initial-buffer-list nil "")
(defvar swbuff-initial-buffer nil "")
(defvar swbuff-initial-window nil "")
(defvar swbuff-initial-frame nil "")

;; Current buffer being displayed by swbuff sequence.
(defvar swbuff-current-buffer nil "")

;; Save the status buffer window, in case any external code that runs on a
;; timer changes the current window.
(defvar swbuff-status-window nil "")

(defun swbuff-initialize ()
  "Initialize swbuff variables prior to a switch sequence."
  (setq swbuff-buffer-list    (swbuff-buffer-list)
	swbuff-initial-buffer-list swbuff-buffer-list
	swbuff-initial-buffer (car swbuff-initial-buffer-list)
	swbuff-initial-window (selected-window)
	swbuff-initial-frame  (selected-frame)))

(defun swbuff-kill-this-buffer ()
  "Kill the current buffer but retain the status window.

I bind

  (control right)  to `swbuff-switch-to-next-buffer',
  (control left)   to `swbuff-switch-to-previous-buffer' and
  (delete)         to `swbuff-kill-this-buffer'

which I find useful for cycling through and culling superfluous buffers."
  (interactive)

  (let ((dead-buffer (current-buffer)))
    (if (condition-case nil (kill-buffer dead-buffer))
	(progn
	  (if swbuff-initial-buffer
	      (setq swbuff-buffer-list
		    (delq dead-buffer swbuff-buffer-list)
		    swbuff-initial-buffer-list
		    (delq dead-buffer swbuff-initial-buffer-list))
	    (swbuff-initialize))
	  (if (car swbuff-buffer-list)
	      (progn (switch-to-buffer (car swbuff-buffer-list))
		     (swbuff-show-status-window))
	    (swbuff-discard-status-window)))
      (swbuff-discard-status-window))))

;; rename
(defvar swbuff-display-timer nil)

(defun swbuff-pre-command-hook ()
  "`pre-command-hook' used to track successive calls to switch commands."
  (when (eq (selected-frame) swbuff-initial-frame)
    (remove-hook 'pre-command-hook 'swbuff-pre-command-hook)
    (if (timerp swbuff-display-timer)
	(cancel-timer swbuff-display-timer))
    (setq swbuff-display-timer nil)
    (unless (or (eq this-command 'swbuff-kill-this-buffer)
		(eq this-command 'swbuff-switch-to-previous-buffer)
		(eq this-command 'swbuff-switch-to-next-buffer)
		(eq this-command 'swbuff-ignore))
      (swbuff-discard-status-window)
      ;; discard the command that ends the sequence
      ;; (setq this-command 'ignore) ;; err no
      )))

(defun swbuff-discard-status-window ()
  "Discard the status window.  Called by both `sit-for' in
`swbuff-show-status-window' and `swbuff-post-command-hook'"

  (let ((buffer (get-buffer swbuff-status-buffer-name))
	(buffer-list (nreverse swbuff-initial-buffer-list)))

    (if (window-live-p swbuff-status-window)
	(delete-window swbuff-status-window))

    (if buffer (kill-buffer buffer))

    (unwind-protect
	(when (and swbuff-initial-buffer swbuff-current-buffer)
	  (save-window-excursion

	    ;; Because this may be called from a timer we have to be real
	    ;; careful that we are in the right frame, window and buffer
	    ;; at that time --- other timers (eg those called by
	    ;; speedbar) may put us elsewhere:-)

	    (select-frame swbuff-initial-frame)
	    (select-window swbuff-initial-window)

	    ;; reset visit order to what it was before the sequence began
	    (while (setq buffer (car buffer-list))
	      (switch-to-buffer buffer)
	      (setq buffer-list (cdr buffer-list)))
	    )
	  ;; then switch between the first and last buffers in the sequence
	  (and swbuff-initial-buffer
	       (switch-to-buffer swbuff-initial-buffer))
	  (and swbuff-current-buffer
	       (switch-to-buffer swbuff-current-buffer))
	  )
      ;; protect forms
      (setq swbuff-initial-buffer	 nil
	    swbuff-initial-buffer-list   nil
	    swbuff-current-buffer	 nil
	    swbuff-initial-frame	 nil
	    swbuff-initial-window	 nil
	    swbuff-status-window	 nil))
    ))


(defun swbuff-layout-status-line (window bcurr)
  "Layout a status line in WINDOW current buffer.
BCURR is the buffer name to highlight."
  (let* ((blist swbuff-initial-buffer-list)
	 (head  (or swbuff-header    "" ))
	 (separ (or swbuff-separator " "))
	 (trail (or swbuff-trailer   "" ))
	 (left  (or swbuff-left     "" ))
	 (right (or swbuff-right     "" ))
	 (width (window-width window))
	 (lines 0)
	 (adjust (or (eq swbuff-status-window-layout 'adjust)
		     (swbuff-one-window-p window)))
	 ;; okay, its crazy logic but it works:-)
	 (half-way (1- (/
			(if (= (% (length blist) 2) 0) ;; if even ...
			    (length blist)
			  (1+ (length blist))) ;; make it even
			2)))

	 start end buffer bname fillr)

    (when swbuff-start-with-current-centered
      ;; rearrange blist so that the first elt is in the middle
      (setq blist (append (last blist half-way)      ;; last half
			  (butlast blist half-way)))) ;; first half

    (save-selected-window
      (select-window window)

      (setq header-line-format nil) ;; Hide Emacs 21 header line.
      (if xemacsp
	  (setq modeline-format swbuff-modeline-format)
	(setq mode-line-format swbuff-mode-line-format))

      (erase-buffer)
      (setq start (point))

      (insert head)
      (if (> (point) start)
	  (set-text-properties
	   start (point) '(face swbuff-separator-face)))

      (while blist
	(setq buffer (car blist)
	      blist  (cdr blist))
	(when (buffer-live-p buffer)
	  (setq bname (buffer-name buffer)
		start (point)
		fillr (if blist separ trail))

	  ;; add a newline if we will run out of space
	  (when (and adjust
		     (> (- (+ start (length bname)
			      (length (concat left fillr right)))
			   (* lines width))
			width))
	    (newline)
	    (setq start (point)
		  lines (1+ lines)))

	  (insert left)
	  (if (> (point) start)
	      (set-text-properties
	       start (point) '(face swbuff-separator-face)))

	  (setq start (point))
	  (insert bname)

	  ;; highlight it if it is the current one
	  (cond
	   ((string-equal bname bcurr)
	    (setq end (point))
	    (set-text-properties
	     start end '(face swbuff-current-buffer-face)))
	   ((string-match  swbuff-special-buffers-re bname)
	    (set-text-properties
	     start (point) '(face swbuff-special-buffers-face)))
	   (t
	    (set-text-properties
	     start (point) '(face swbuff-default-face))))

	  (setq start (point))
	  (insert right)
	  (if (> (point) start)
	      (set-text-properties
	       start (point) '(face swbuff-separator-face)))

	  (setq start (point))
	  (insert fillr)
	  (if (> (point) start)
	      (set-text-properties
	       start (point) '(face swbuff-separator-face)))))
      (if adjust
	  (swbuff-adjust-window)
	(swbuff-adjust-window 1)
	(swbuff-scroll-window end)))))


(defun swbuff-show-status-window ()
  "Pop-up a status window at the bottom of the selected window. The
status window shows the list of switchable buffers where the switched
one is hilighted using `swbuff-current-buffer-face'. It is
automatically discarded after any command is executed or after the
delay specified by `swbuff-clear-delay'."

  (if swbuff-initial-buffer-list
      (let ((buffer-name (buffer-name swbuff-current-buffer))
	    (window-min-height 1)
	    (cursor-in-non-selected-windows nil))
	(with-current-buffer (get-buffer-create swbuff-status-buffer-name)
	  (let ((window (or (get-buffer-window swbuff-status-buffer-name)
			    (split-window-vertically -2))))

	    ;; if we forget this we may end up with multiple status
	    ;; windows (kal)
	    (setq swbuff-status-window window)

	    (set-window-buffer window (current-buffer))
	    (swbuff-layout-status-line window buffer-name)
	    (add-hook 'pre-command-hook 'swbuff-pre-command-hook)

	    ;; use a timer that we can cancel rather than sit-for
	    (if (timerp swbuff-display-timer)
		(cancel-timer swbuff-display-timer))
	    (setq swbuff-display-timer
		  (run-with-timer swbuff-clear-delay nil
				  'swbuff-discard-status-window)))))
    (swbuff-discard-status-window)
    (message "No buffers eligible for switching.")
    ))


(defun swbuff-in-other-frame-p (buffer)
  "Return non-nil if BUFFER is being displayed in another visible
frame.  Useful if we want to skip buffers displayed in other frames
\(see `swbuff-buffer-list')."
  (let ((found-in-other-frame nil)
	(window nil)
	(window-list (get-buffer-window-list buffer nil 0)))
    (while (and (setq window (car window-list))
		(not found-in-other-frame))
      (unless (eq (window-frame window) swbuff-initial-frame)
	(setq found-in-other-frame t))
      (pop window-list))
    found-in-other-frame
    ))

(defun swbuff-exclude-mode-p (buffer)
  "Return non-nil iff the major mode of BUFFER matches
`swbuff-exclude-mode-regexps'."
  (unless (string-equal "" swbuff-exclude-mode-regexp)
    (save-excursion
      (set-buffer buffer)
      (string-match swbuff-exclude-mode-regexp
		    (symbol-name major-mode)))))

(defun swbuff-exclude-p (name)
  "Return non-nil if buffer NAME matches one of the
`swbuff-exclude-buffer-regexps'."
  (let ((rl (cons (regexp-quote swbuff-status-buffer-name)
		  (delete "" swbuff-exclude-buffer-regexps))))
    (while (and rl (car rl) (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (not (null rl))))

(defun swbuff-include-p (name)
  "Return non-nil if buffer NAME matches one of the
`swbuff-include-buffer-regexps'."
  (let ((rl (delete "" swbuff-include-buffer-regexps)))
    (while (and rl (car rl) (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (not (null rl))))

(defvar swbuff-buffer-list nil
  "Stores the current list of switchable buffers.
This way we only have to call `swbuff-buffer-list' once.")

(defun swbuff-buffer-list ()
  "Return the list of switchable buffers.  That is without the ones whose
name matches `swbuff-exclude-buffer-regexps'.  If `swbuff-this-frame-only'
is non-nil, buffer that are currently displayed in other visble or
iconified frames are also excluded."
  (let ((blist
	 (delq nil (mapcar
		    (lambda (buf)
		      (and (or (swbuff-include-p (buffer-name buf))
			       (not
				(or
				 (swbuff-exclude-mode-p buf)
				 (swbuff-exclude-p (buffer-name buf)))))
			   (if swbuff-this-frame-only
			       (not (swbuff-in-other-frame-p buf))
			     t)
			   buf))
		    (buffer-list)))))
    (when blist
      ;; add the current buffer if it would normally be skipped
      (unless (memq (current-buffer) blist)
	(setq blist (cons (current-buffer) blist))))
    blist))

(defun swbuff-next-buffer ()
  "Display and activate the next buffer in the buffer list."
  (let ((buf (car swbuff-buffer-list)))
    (when buf
      (setq swbuff-buffer-list (cdr swbuff-buffer-list))
      (setq swbuff-buffer-list (append swbuff-buffer-list (list buf)))
      (setq swbuff-current-buffer (car swbuff-buffer-list))
      (when swbuff-display-intermediate-buffers
	(switch-to-buffer (car swbuff-buffer-list) t)) ;; no record
      )))

(defun swbuff-previous-buffer ()
  "Display and activate the buffer at the end of the buffer list."
  (let ((buf (car (last swbuff-buffer-list))))
    (when buf
      (when swbuff-display-intermediate-buffers
	(switch-to-buffer buf t))
      (setq swbuff-current-buffer buf)
      (setq swbuff-buffer-list (butlast swbuff-buffer-list))
      (setq swbuff-buffer-list (cons buf swbuff-buffer-list)))))

(defun swbuff-switch-to-previous-buffer ()
  "\\[swbuff-switch-to-previous-buffer] switch to the previous buffer
in the buffer list."
  (interactive)
  (run-hooks 'swbuff-pre-switch-hook)
  (if swbuff-initial-buffer
      (and swbuff-delay-switch (swbuff-previous-buffer))
    (swbuff-initialize))
  (or swbuff-delay-switch (swbuff-previous-buffer))
  (swbuff-show-status-window))

(defun swbuff-switch-to-next-buffer ()
  "\\[swbuff-switch-to-next-buffer] switch to the next buffer in the
buffer list."
  (interactive)
  (run-hooks 'swbuff-pre-switch-hook)

  (if swbuff-initial-buffer
      (swbuff-next-buffer)
    ;; first call in the sequence
    (swbuff-initialize)
    (unless swbuff-delay-switch
      (swbuff-next-buffer)))

  (swbuff-show-status-window))

(provide 'swbuff-x)
;;; swbuff-x.el ends here

