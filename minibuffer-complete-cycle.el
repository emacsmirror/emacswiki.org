;;; minibuffer-complete-cycle.el --- Cycle through the *Completions* buffer

;; Copyright © 1997,1998,2000,2003,2006 Kevin Rodgers
;; Copyright © 2013 Akinori MUSHA

;; Author: Akinori MUSHA <knu@iDaemons.org>
;;         Kevin Rodgers <ihs_4664@yahoo.com>
;; Maintainer: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/minibuffer-complete-cycle
;; Created: 15 Oct 1997
;; Version: 1.25.20130814
;; Keywords: completion
;; X-Original-Version: $Revision: 1.24 $
;; X-Original-RCS: $Id: minibuffer-complete-cycle.el,v 1.24 2006/07/25 16:49:03 onc04664 Exp $

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
;;
;; Emacs 24 introduced `completion-cycle-threshold' which achieves a
;; similar goal.  This extension allows you to see the completion
;; window while cycling, and to cycle backward with <backtab>.

;;; Change log:
;;
;; Version 1.25.20130814  2013-08-14  Akinori MUSHA
;;   Support for Emacs 24.
;;   Fix a bug with partial completion.
;;   Fix a bug when the minibuffer is like "~/dir1/~/".
;;   Bind <backtab> to minibuffer-complete-backward rather than M-TAB.
;;   Make the slash key settle the curent path component if appropriate.

;;; Code:

;; Package interface:
(provide 'minibuffer-complete-cycle)

(require 'custom)			; defgroup, defcustom, defface

(eval-when-compile (require 'cl))

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
  'mouse-face
  "The text property used to identify completions.")

(defvar mcc-overlay
  (let ((overlay (make-overlay 1 1)))
    (overlay-put overlay 'face 'minibuffer-complete-cycle)
    overlay)
  "If non-nil, the overlay used to highlight the *Completions* buffer.")


;; Commands:

(defadvice minibuffer-complete (around cycle (&optional count) activate compile)
  "If the `minibuffer-complete-cycle' option is set, then instead of
just scrolling the window of possible completions, insert each one in
turn in the minibuffer and highlight it in the *Completions* buffer with
the `minibuffer-complete-cycle' face.

Prefix arg means select the COUNT'th next completion.
To cycle to previous completions, type <backtab>."
;; `\\<minibuffer-local-completion-map>\\[minibuffer-complete-backward]'
  (interactive "p")
  (if (and minibuffer-complete-cycle
           (not (eq this-command 'completion-at-point)) ; Emacs 24
           ;; See Fminibuffer_complete:
           (or (eq last-command this-command)
               (eq last-command 'completion-at-point)   ; Emacs 24
               (and (eq minibuffer-complete-cycle 'auto)
                    (progn
                      (setq mcc-completion-begin nil
                            mcc-completion-end nil)
                      ad-do-it)))
           minibuffer-scroll-window
           (window-live-p minibuffer-scroll-window))
      (let ((lastlen (and mcc-completion-begin mcc-completion-end
                      (- mcc-completion-end mcc-completion-begin)))
            (completion (mcc-completion-string count)))
        (cond (lastlen
               ;; Delete the part last completed
               (delete-region (- (point-max) lastlen)
                              (point-max)))
              (minibuffer-completing-file-name
               ;; Skip the last component
               (or (re-search-backward "/" (minibuffer-prompt-end) t)
                   (goto-char (minibuffer-prompt-end)))
               ;; Skip components to be completed
               (let ((str (directory-file-name completion))
                     (start 0))
                 (while (string-match "/" str start)
                   (setq start (match-end 0))
                   (or (re-search-backward "/" (minibuffer-prompt-end) t)
                       (goto-char (minibuffer-prompt-end))))
                 (if (looking-at "/") (forward-char 1))
                 (delete-region (point) (point-max))))
              (t (delete-region (minibuffer-prompt-end)
                                (point-max))))
        (insert completion)
        (mcc-display-completion (< count 0)))
    ;; Reset the mcc variables and proceed normally:
    (setq mcc-completion-begin nil
          mcc-completion-end nil)
    ad-do-it))

(defun minibuffer-complete-backward (&optional count)
  "Just like `minibuffer-complete', but cycle to the previous completion.
Prefix arg means select the COUNT'th previous completion."
  (interactive "p")
  (setq this-command 'minibuffer-complete)
  (minibuffer-complete (- count)))

(defun minibuffer-complete-slash (&optional arg)
  "Insert a slash ARG times, or settle the current path component if complete-cycling is at a directory name."
  (interactive "p")
  (or
   (and (= arg 1)
        (eq last-command 'minibuffer-complete)
        minibuffer-completing-file-name
        (eolp)
        (char-equal (preceding-char) ?/))
   (self-insert-command arg)))

;; Functions:

;;;###autoload
(defun mcc-define-keys ()	; mcc-minor-mode & -keymap
  "Define extra key bindings in the local keymap.
This has no effect unless the `minibuffer-complete-cycle' option is set."
  (when minibuffer-complete-cycle
    (dolist (binding
             '(("<backtab>" . minibuffer-complete-backward)
               ("/"         . minibuffer-complete-slash)
               ))
      (let ((key (kbd (car binding)))
            (func (cdr binding)))
        (and (null (local-key-binding key))
             (local-set-key key func))))))

;;;###autoload
(add-hook 'minibuffer-setup-hook 'mcc-define-keys)

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
      (buffer-substring-no-properties mcc-completion-begin mcc-completion-end))))

(defun mcc-display-completion (&optional backward)
  "Highlight the current completion and scroll the *Completions* buffer \
if necessary.
Scroll up by default, but scroll down if BACKWARD is non-nil."
  (let ((completion-buffer (window-buffer minibuffer-scroll-window))
	(minibuffer-window (selected-window)))
    (if mcc-overlay
        (move-overlay mcc-overlay mcc-completion-begin mcc-completion-end
                      completion-buffer))
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
