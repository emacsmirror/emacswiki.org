;;; minibuffer-complete-cycle.el --- Cycle through the *Completions* buffer
;;; -*-unibyte: t; coding: iso-8859-1;-*-

;; Copyright � 1997,1998,2000,2003,2006 Kevin Rodgers

;; Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; Created: 15 Oct 1997
;; Version: $Revision: 1.24 $
;; Keywords: completion
;; RCS: $Id: minibuffer-complete-cycle.el,v 1.24 2006/07/25 16:49:03 onc04664 Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; The `minibuffer-complete' command, bound by default to TAB in the
;; minibuffer completion keymaps, displays the list of possible
;; completions when no additional characters can be completed.
;; Subsequent invocations of this command cause the window displaying
;; the *Completions* buffer to scroll, if necessary.
;; 
;; This library advises the `minibuffer-complete' command so that
;; subsequent invocations instead select each of the possible
;; completions in turn, inserting it into the minibuffer and
;; highlighting it in the *Completions* buffer.  As before, the window
;; displaying the possible completions is scrolled if necessary.  This
;; feature is enabled by loading this file and setting the
;; `minibuffer-complete-cycle' option to t with `M-x customize-variable'
;; or `M-x set-variable'; it is disabled by unsetting the option (to
;; nil).  Besides t, the special value `auto' enables the feature and
;; also causes the first completion to be selected immediately.
;; 
;; You can also customize the `minibuffer-complete-cycle' face, which is
;; used to highlight the selected completion, with `M-x customize-face'
;; or any of the `M-x set-face-' commands.

;; The technique of deleting the minibuffer contents, then (for file
;; name completion) inserting the directory component of the initial
;; input, and then inserting the completion string itself is based on
;; cycle-mini.el (1.03) by Joe Reiss <jreiss@vt.edu>.


;;; Code:

;; Package interface:
(provide 'minibuffer-complete-cycle)

(require 'custom)			; defgroup, defcustom, defface

;; User options:

(defgroup minibuffer-complete-cycle nil
  "Cycle through the *Completions* buffer."
  :group 'completion)

(defcustom minibuffer-complete-cycle nil
  "*If non-nil, `minibuffer-complete' cycles through the possible completions.
If `auto', `minibuffer-complete' selects the first completion immediately."
  :type '(choice (const t) (const auto) (const nil))
  :group 'minibuffer-complete-cycle
  :require 'minibuffer-complete-cycle)

(defface minibuffer-complete-cycle
  '((t (:inherit secondary-selection)))
  "Face for highlighting the selected completion in the *Completions* buffer."
  :group 'minibuffer-complete-cycle)

;; Internal variables:
(defvar mcc-completion-begin nil	; point in the *Completions* buffer
  "If non-nil, the beginning of the selected completion.")
(defvar mcc-completion-end nil		; point in the *Completions* buffer
  "If non-nil, the end of the selected completion.")

(defvar mcc-completion-property
  (cond ((string-match "XEmacs" emacs-version) 'list-mode-item)
	(t 'mouse-face))
  "The text property used to identify completions.")

(defvar mcc-overlay
  (progn
    (or (face-differs-from-default-p 'minibuffer-complete-cycle)
	(copy-face 'secondary-selection 'minibuffer-complete-cycle)) ; Emacs 19
    (cond ((and (fboundp 'make-extent) (fboundp 'set-extent-property)) ; XEmacs
	   (let ((extent (make-extent 1 1)))
	     (set-extent-property extent 'face 'minibuffer-complete-cycle)
	     extent))
	  ((and (fboundp 'make-overlay) (fboundp 'overlay-put))
	   (let ((overlay (make-overlay 1 1)))
	     (overlay-put overlay 'face 'minibuffer-complete-cycle)
	     overlay))))
  "If non-nil, the overlay used to highlight the *Completions* buffer.")


;; Commands:
(defadvice minibuffer-complete (around cycle (&optional count) activate compile)
  "If the `minibuffer-complete-cycle' option is set, then instead of
just scrolling the window of possible completions, insert each one in
turn in the minibuffer and highlight it in the *Completions* buffer with
the `minibuffer-complete-cycle' face.

Prefix arg means select the COUNT'th next completion.
To cycle to previous completions, type `M-TAB'."
;; `\\<minibuffer-local-completion-map>\\[minibuffer-complete-backward]'
  (interactive "p")
  (if (and minibuffer-complete-cycle
	   ;; See Fminibuffer_complete:
	   (or (eq last-command this-command)
	       (and (eq minibuffer-complete-cycle 'auto)
		    (progn
		      (setq mcc-completion-begin nil
			    mcc-completion-end nil)
		      ad-do-it)))
	   minibuffer-scroll-window
	   (window-live-p minibuffer-scroll-window))
      ;; Delete the current completion, then insert and display the
      ;; next completion:
      (let ((incomplete-path
	     (if (cond ((boundp 'minibuffer-completing-file-name) ; Emacs 20
			(symbol-value 'minibuffer-completing-file-name))
		       ((eq minibuffer-completion-table
			    'read-file-name-internal)))
		 (buffer-substring (if (fboundp 'minibuffer-prompt-end)	; Emacs 21
				       (minibuffer-prompt-end)
				     (point-min))
				   (point-max)))))
	(delete-region (if (fboundp 'minibuffer-prompt-end) ; Emacs 21
			   (minibuffer-prompt-end)
			 (point-min))
		       (point-max))
	(if incomplete-path
	    (progn
	      ;; Truncate to directory:
	      (setq incomplete-path
		    (or (file-name-directory
			 (if (and mcc-completion-begin mcc-completion-end
				  (file-directory-p incomplete-path))
			     (directory-file-name incomplete-path)
			   incomplete-path))
			""))
	      (insert incomplete-path)))
	(insert (mcc-completion-string count))
	(mcc-display-completion (< count 0)))
    ;; Reset the mcc variables and proceed normally:
    (progn
      (setq mcc-completion-begin nil
	    mcc-completion-end nil)
      ad-do-it)))

(defun minibuffer-complete-backward (&optional count)
  "Just like `minibuffer-complete', but cycle to the previous completion.
Prefix arg means select the COUNT'th previous completion."
  (interactive "p")
  (setq this-command 'minibuffer-complete)
  (minibuffer-complete (- count)))


;; Functions:
(defun mcc-define-backward-key ()	; mcc-minor-mode & -keymap
  "Bind `M-TAB' to `minibuffer-complete-backward' in the local keymap.
This has no effect unless the `minibuffer-complete-cycle' option is set and
`M-TAB' is not already bound in the keymap."
  (if (and minibuffer-complete-cycle
	   (null (local-key-binding "\M-\t")))
      (local-set-key "\M-\t" 'minibuffer-complete-backward)))

(add-hook 'minibuffer-setup-hook 'mcc-define-backward-key)

(defun mcc-completion-string (n)
  "Return the Nth next completion.
If N is negative, return the Nth previous completion."
  (let ((completion-buffer (window-buffer minibuffer-scroll-window)))
    ;; Verify the buffer and window configuration:
    (or (eq completion-buffer (get-buffer "*Completions*"))
	(error "minibuffer-scroll-window isn't displaying \
the *Completions* buffer"))
    (save-excursion
      (set-buffer completion-buffer)
      ;; Find the beginning and end of the completion:
      (if (< n 0)
	  (while (< n 0)
	    (setq mcc-completion-end
		  (or (and mcc-completion-begin
			   (previous-single-property-change mcc-completion-begin
							    mcc-completion-property))
		      (point-max)))
	    (setq mcc-completion-begin
		  (previous-single-property-change mcc-completion-end
						   mcc-completion-property
						   nil (point-min)))
	    (setq n (1+ n)))
	(while (> n 0)
	  (setq mcc-completion-begin
		(next-single-property-change (if (and mcc-completion-end
						      (< mcc-completion-end
							 (point-max)))
						 mcc-completion-end
					       (point-min))
					     mcc-completion-property))
	  (setq mcc-completion-end
		(next-single-property-change mcc-completion-begin
					     mcc-completion-property
					     nil (point-max)))
	  (setq n (1- n))))
      ;; Return the next completion (buffer-substring-no-properties?):
      (buffer-substring mcc-completion-begin mcc-completion-end))))

(defun mcc-display-completion (&optional backward)
  "Highlight the current completion and scroll the *Completions* buffer \
if necessary.
Scroll up by default, but scroll down if BACKWARD is non-nil."
  (let ((completion-buffer (window-buffer minibuffer-scroll-window))
	(minibuffer-window (selected-window)))
    (if mcc-overlay
	(cond ((fboundp 'set-extent-endpoints) ; XEmacs
	       (set-extent-endpoints mcc-overlay mcc-completion-begin mcc-completion-end
				     completion-buffer))
	      ((fboundp 'move-overlay)
	       (move-overlay mcc-overlay mcc-completion-begin mcc-completion-end
			     completion-buffer))))
    (unwind-protect
	(progn
	  (select-window minibuffer-scroll-window) ; completion-buffer
	  (or (pos-visible-in-window-p mcc-completion-begin)
	      (if backward
		  (if (= (window-start) (point-min))
		      (set-window-point (selected-window) (point-max))
		    (scroll-down))
		(if (= (window-end) (point-max))
		    (set-window-point (selected-window) (point-min))
		  (scroll-up)))))
      (select-window minibuffer-window))))

;;; minibuffer-complete-cycle.el ends here
