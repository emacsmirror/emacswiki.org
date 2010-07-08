;;; longlines-jp.el --- automatically wrap long lines in Japanese text  -*- coding:utf-8 -*-

;; Copyright (C) 2000, 2001, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.

;; Authors:    Happy Kohpy <bonze.head@gmail.com>
;; Keywords: convenience, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; longlines-jp-mode provide function like longlines-mode for Japanese text.

;; Usage
;; Load it with autoload or require. The enter
;; M-x longlines-jpor load it permanently with
;; (longlines-jp t)in your emacs configuration file.

;;; Code:

(defgroup longlines-jp nil
  "Automatic wrapping of long lines when loading files."
  :group 'fill)

(defcustom longlines-jp-auto-wrap t
  "Non-nil means long lines are automatically wrapped after each command.
Otherwise, you can perform filling using `fill-paragraph' or
`auto-fill-mode'.  In any case, the soft newlines will be removed
when the file is saved to disk."
  :group 'longlines-jp
  :type 'boolean)

(defcustom longlines-jp-wrap-follows-window-size nil
  "Non-nil means wrapping and filling happen at the edge of the window.
Otherwise, `fill-column' is used, regardless of the window size.  This
does not work well when the buffer is displayed in multiple windows
with differing widths.

If the value is an integer, that specifies the distance from the
right edge of the window at which wrapping occurs.  For any other
non-nil value, wrapping occurs 2 characters from the right edge."
  :group 'longlines-jp
  :type 'boolean)

(defcustom longlines-jp-show-hard-newlines nil
  "Non-nil means each hard newline is marked on the screen.
\(The variable `longlines-jp-show-effect' controls what they look like.)
You can also enable the display temporarily, using the command
`longlines-jp-show-hard-newlines'."
  :group 'longlines-jp
  :type 'boolean)

(defcustom longlines-jp-show-effect (propertize "«\n" 'face 'jaspace-highlight-eol-face)
  "A string to display when showing hard newlines.
This is used when `longlines-jp-show-hard-newlines' is on."
  :group 'longlines-jp
  :type 'string)

;; Internal variables

(defvar longlines-jp-wrap-beg nil)
(defvar longlines-jp-wrap-end nil)
(defvar longlines-jp-wrap-point nil)
(defvar longlines-jp-showing nil)
(defvar longlines-jp-decoded nil)

(make-variable-buffer-local 'longlines-jp-wrap-beg)
(make-variable-buffer-local 'longlines-jp-wrap-end)
(make-variable-buffer-local 'longlines-jp-wrap-point)
(make-variable-buffer-local 'longlines-jp-showing)
(make-variable-buffer-local 'longlines-jp-decoded)

;; Mode

(defvar message-indent-citation-function)

;;;###autoload
(define-minor-mode longlines-jp-mode
  "Toggle Long Lines JP mode.
In Long Lines JP mode, long lines are wrapped if they extend beyond
`fill-column'.  The soft newlines used for line wrapping will not
show up when the text is yanked or saved to disk.

If the variable `longlines-jp-auto-wrap' is non-nil, lines are automatically
wrapped whenever the buffer is changed.  You can always call
`fill-paragraph' to fill individual paragraphs.

If the variable `longlines-jp-show-hard-newlines' is non-nil, hard newlines
are indicated with a symbol."
  :group 'longlines-jp :lighter " ll-jp"
  (if longlines-jp-mode
      ;; Turn on longlines-jp mode
      (progn
        (use-hard-newlines 1 'never)
        (set (make-local-variable 'require-final-newline) nil)
        (add-to-list 'buffer-file-format 'longlines-jp)
        (add-hook 'change-major-mode-hook 'longlines-jp-mode-off nil t)
	(add-hook 'before-revert-hook 'longlines-jp-before-revert-hook nil t)
        (make-local-variable 'buffer-substring-filters)
        (make-local-variable 'longlines-jp-auto-wrap)
        (add-to-list 'buffer-substring-filters 'longlines-jp-encode-string)
        (when longlines-jp-wrap-follows-window-size
	  (let ((dw (if (and (integerp longlines-jp-wrap-follows-window-size)
			     (>= longlines-jp-wrap-follows-window-size 0)
			     (< longlines-jp-wrap-follows-window-size
				(window-width)))
			longlines-jp-wrap-follows-window-size
		      2)))
	    (set (make-local-variable 'fill-column)
		 (- (window-width) dw)))
          (add-hook 'window-configuration-change-hook
                    'longlines-jp-window-change-function nil t))
        (let ((buffer-undo-list t)
              (inhibit-read-only t)
	      (after-change-functions nil)
              (mod (buffer-modified-p))
	      buffer-file-name buffer-file-truename)
          ;; Turning off undo is OK since (spaces + newlines) is
          ;; conserved, except for a corner case in
          ;; longlines-jp-wrap-lines that we'll never encounter from here
	  (save-restriction
	    (widen)
	    (unless longlines-jp-decoded
	      (longlines-jp-decode-buffer)
	      (setq longlines-jp-decoded t))
	    (longlines-jp-wrap-region (point-min) (point-max)))
          (set-buffer-modified-p mod))
        (when (and longlines-jp-show-hard-newlines
                   (not longlines-jp-showing))
          (longlines-jp-show-hard-newlines))

	;; Hacks to make longlines-jp play nice with various modes.
	(cond ((eq major-mode 'mail-mode)
	       (add-hook 'mail-setup-hook 'longlines-jp-decode-buffer nil t)
	       (or mail-citation-hook
		   (add-hook 'mail-citation-hook 'mail-indent-citation nil t))
	       (add-hook 'mail-citation-hook 'longlines-jp-decode-region nil t))
	      ((eq major-mode 'message-mode)
	       (add-hook 'message-setup-hook 'longlines-jp-decode-buffer nil t)
	       (make-local-variable 'message-indent-citation-function)
	       (if (not (listp message-indent-citation-function))
		   (setq message-indent-citation-function
			 (list message-indent-citation-function)))
	       (add-to-list 'message-indent-citation-function
			    'longlines-jp-decode-region t)))

	(add-hook 'after-change-functions 'longlines-jp-after-change-function nil t)
	(add-hook 'post-command-hook 'longlines-jp-post-command-function nil t)
        (when longlines-jp-auto-wrap
          (auto-fill-mode 0)))
    ;; Turn off longlines-jp mode
    (setq buffer-file-format (delete 'longlines-jp buffer-file-format))
    (if longlines-jp-showing
        (longlines-jp-unshow-hard-newlines))
    (let ((buffer-undo-list t)
	  (after-change-functions nil)
          (inhibit-read-only t)
	  buffer-file-name buffer-file-truename)
      (if longlines-jp-decoded
	  (save-restriction
	    (widen)
	    (longlines-jp-encode-region (point-min) (point-max))
	    (setq longlines-jp-decoded nil))))
    (remove-hook 'change-major-mode-hook 'longlines-jp-mode-off t)
    (remove-hook 'after-change-functions 'longlines-jp-after-change-function t)
    (remove-hook 'post-command-hook 'longlines-jp-post-command-function t)
    (remove-hook 'before-revert-hook 'longlines-jp-before-revert-hook t)
    (remove-hook 'window-configuration-change-hook
                 'longlines-jp-window-change-function t)
    (when longlines-jp-wrap-follows-window-size
      (kill-local-variable 'fill-column))
    (kill-local-variable 'require-final-newline)
    (kill-local-variable 'buffer-substring-filters)
    (kill-local-variable 'use-hard-newlines)))

(defun longlines-jp-mode-off ()
  "Turn off longlines-jp mode.
This function exists to be called by `change-major-mode-hook' when the
major mode changes."
  (longlines-jp-mode 0))

;; Showing the effect of hard newlines in the buffer

(defun longlines-jp-show-hard-newlines (&optional arg)
  "Make hard newlines visible by adding a face.
With optional argument ARG, make the hard newlines invisible again."
  (interactive "P")
    (if arg
        (longlines-jp-unshow-hard-newlines)
      (setq longlines-jp-showing t)
      (longlines-jp-show-region (point-min) (point-max))))

(defun longlines-jp-show-region (beg end)
  "Make hard newlines between BEG and END visible."
  (let* ((pmin (min beg end))
         (pmax (max beg end))
         (pos (text-property-not-all pmin pmax 'hard nil))
	 (mod (buffer-modified-p))
	 (buffer-undo-list t)
	 (inhibit-read-only t)
	 (inhibit-modification-hooks t)
	 buffer-file-name buffer-file-truename)
    (while pos
      (put-text-property pos (1+ pos) 'display
			 (copy-sequence longlines-jp-show-effect))
      (setq pos (text-property-not-all (1+ pos) pmax 'hard nil)))
    (restore-buffer-modified-p mod)))

(defun longlines-jp-unshow-hard-newlines ()
  "Make hard newlines invisible again."
  (interactive)
  (setq longlines-jp-showing nil)
  (let ((pos (text-property-not-all (point-min) (point-max) 'hard nil))
	(mod (buffer-modified-p))
	(buffer-undo-list t)
	(inhibit-read-only t)
	(inhibit-modification-hooks t)
	buffer-file-name buffer-file-truename)
    (while pos
      (remove-text-properties pos (1+ pos) '(display))
      (setq pos (text-property-not-all (1+ pos) (point-max) 'hard nil)))
    (restore-buffer-modified-p mod)))

;; Wrapping the paragraphs.

(defun longlines-jp-wrap-region (beg end)
  "Wrap each successive line, starting with the line before BEG.
Stop when we reach lines after END that don't need wrapping, or the
end of the buffer."
  (let ((mod (buffer-modified-p)))
    (setq longlines-jp-wrap-point (point))
    (goto-char beg)
    (forward-line -1)
    ;; Two successful longlines-jp-wrap-line's in a row mean successive
    ;; lines don't need wrapping.
    (while (null (and (longlines-jp-wrap-line)
		      (or (eobp)
			  (and (>= (point) end)
			       (longlines-jp-wrap-line))))))
    (goto-char longlines-jp-wrap-point)
    (set-buffer-modified-p mod)))

(defun longlines-jp-wrap-line ()
  "If the current line needs to be wrapped, wrap it and return nil.
If wrapping is performed, point remains on the line.  If the line does
not need to be wrapped, move point to the next line and return t."
  (if (longlines-jp-set-breakpoint)
      (progn (insert-before-markers ?\n) nil)
    (if (longlines-jp-merge-lines-p)
        (progn (end-of-line)
     ;; After certain commands (e.g. kill-line), there may be two
     ;; successive soft newlines in the buffer.  In this case, we
     ;; replace these two newlines by a single space.  Unfortunately,
     ;; this breaks the conservation of (spaces + newlines), so we
     ;; have to fiddle with longlines-jp-wrap-point.
	       (if (or (prog1 (bolp) (forward-char 1)) (eolp))
		   (progn
		     (delete-char -1)
		     (if (> longlines-jp-wrap-point (point))
			 (setq longlines-jp-wrap-point
			       (1- longlines-jp-wrap-point))))
		 (delete-char -1))
               nil)
      (forward-line 1)
      t)))

(defun longlines-jp-set-breakpoint ()
  "Place point where we should break the current line, and return t.
If the line should not be broken, return nil; point remains on the
line."
  (move-to-column fill-column)
  (if (and (re-search-forward "." (line-end-position) 1)
           (> (current-column) fill-column))
      ;; This line is too long.  Can we break it?
      (or (longlines-jp-find-break-backward)
          (progn (move-to-column fill-column)
                 (longlines-jp-find-break-forward)))))

(defun longlines-jp-find-break-backward ()
  "Move point backward to the first available breakpoint and return t.
If no breakpoint is found, return nil."
  (and (null (bolp))
       (progn
	 (if (and fill-nobreak-predicate
		  (run-hook-with-args-until-success
		   'fill-nobreak-predicate))
	     (progn (backward-char 1)
		    (longlines-jp-find-break-backward))
	   t))))

(defun longlines-jp-find-break-forward ()
  "Move point forward to the first available breakpoint and return t.
If no break point is found, return nil."
  (and (null (eolp))
       (if (and fill-nobreak-predicate
                (run-hook-with-args-until-success
                 'fill-nobreak-predicate))
           (longlines-jp-find-break-forward)
         t)))

(defun longlines-jp-merge-lines-p ()
  "Return t if part of the next line can fit onto the current line.
Otherwise, return nil.  Text cannot be moved across hard newlines."
 (save-excursion
   (end-of-line)
   (and (null (eobp))
	(null (get-text-property (point) 'hard)))))

(defun longlines-jp-decode-region (&optional beg end)
  "Turn all newlines between BEG and END into hard newlines.
If BEG and END are nil, the point and mark are used."
  (if (null beg) (setq beg (point)))
  (if (null end) (setq end (mark t)))
  (save-excursion
    (let ((reg-max (max beg end)))
      (goto-char (min beg end))
      (while (search-forward "\n" reg-max t)
	(set-hard-newline-properties
	 (match-beginning 0) (match-end 0))))))

(defun longlines-jp-decode-buffer ()
  "Turn all newlines in the buffer into hard newlines."
  (longlines-jp-decode-region (point-min) (point-max)))

(defun longlines-jp-encode-region (beg end &optional buffer)
  "Replace each soft newline between BEG and END with exactly one space.
Hard newlines are left intact.  The optional argument BUFFER exists for
compatibility with `format-alist', and is ignored."
  (save-excursion
    (let ((reg-max (max beg end))
	  (mod (buffer-modified-p)))
      (goto-char (min beg end))
      (while (search-forward "\n" reg-max t)
        (unless (get-text-property (match-beginning 0) 'hard)
          (replace-match "")))
      (set-buffer-modified-p mod)
      end)))

(defun longlines-jp-encode-string (string)
  "Return a copy of STRING with each soft newline replaced by a space.
Hard newlines are left intact."
  (let* ((str (copy-sequence string))
         (pos (string-match "\n" str)))
    (while pos
      (if (null (get-text-property pos 'hard str))
	  (setq str (concat (substring str 0 pos) (substring str (+ pos 1)))))
      (setq pos (string-match "\n" str (1+ pos))))
    str))

;; Auto wrap

(defun longlines-jp-auto-wrap (&optional arg)
  "Toggle automatic line wrapping.
With optional argument ARG, turn on line wrapping if and only if ARG is positive.
If automatic line wrapping is turned on, wrap the entire buffer."
  (interactive "P")
  (setq arg (if arg
		(> (prefix-numeric-value arg) 0)
	      (not longlines-jp-auto-wrap)))
  (if arg
      (progn
	(setq longlines-jp-auto-wrap t)
	(longlines-jp-wrap-region (point-min) (point-max))
	(message "Auto wrap enabled."))
    (setq longlines-jp-auto-wrap nil)
    (message "Auto wrap disabled.")))

(defun longlines-jp-after-change-function (beg end len)
  "Update `longlines-jp-wrap-beg' and `longlines-jp-wrap-end'.
This is called by `after-change-functions' to keep track of the region
that has changed."
  (when (and longlines-jp-auto-wrap (not undo-in-progress))
    (setq longlines-jp-wrap-beg
          (if longlines-jp-wrap-beg (min longlines-jp-wrap-beg beg) beg))
    (setq longlines-jp-wrap-end
          (if longlines-jp-wrap-end (max longlines-jp-wrap-end end) end))))

(defun longlines-jp-post-command-function ()
  "Perform line wrapping on the parts of the buffer that have changed.
This is called by `post-command-hook' after each command."
  (unless (or skk-henkan-mode
	      skk-current-rule-tree)
    (save-excursion
      (when (and longlines-jp-auto-wrap longlines-jp-wrap-beg)
	(if (or (eq this-command 'yank)
		(eq this-command 'yank-pop))
	    (longlines-jp-decode-region (point) (mark t)))
	(if longlines-jp-showing
	    (longlines-jp-show-region longlines-jp-wrap-beg longlines-jp-wrap-end))
	(unless (or (eq this-command 'fill-paragraph)
		    (eq this-command 'fill-region))
	  (longlines-jp-wrap-region longlines-jp-wrap-beg longlines-jp-wrap-end))
	(setq longlines-jp-wrap-beg nil)
	(setq longlines-jp-wrap-end nil)))
    (if (and (bolp) (eq this-command 'backward-delete-char)) (backward-char 1))))

(defun longlines-jp-window-change-function ()
  "Re-wrap the buffer if the window width has changed.
This is called by `window-configuration-change-hook'."
  (let ((dw (if (and (integerp longlines-jp-wrap-follows-window-size)
		     (>= longlines-jp-wrap-follows-window-size 0)
		     (< longlines-jp-wrap-follows-window-size (window-width)))
		longlines-jp-wrap-follows-window-size
	      2)))
    (when (/= fill-column (- (window-width) dw))
      (setq fill-column (- (window-width) dw))
      (longlines-jp-wrap-region (point-min) (point-max)))))

;; Loading and saving

(defun longlines-jp-before-revert-hook ()
  (add-hook 'after-revert-hook 'longlines-jp-after-revert-hook nil t)
  (longlines-jp-mode 0))

(defun longlines-jp-after-revert-hook ()
  (remove-hook 'after-revert-hook 'longlines-jp-after-revert-hook t)
  (longlines-jp-mode 1))

(add-to-list
 'format-alist
 (list 'longlines-jp "Automatically wrap long lines." nil nil
       'longlines-jp-encode-region t nil))

;; Unloading

(defun longlines-jp-unload-function ()
  "Unload the longlines-jp library."
  (save-current-buffer
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (longlines-jp-mode-off)))
  ;; continue standard unloading
  nil)

(provide 'longlines-jp)

;;; longlines-jp.el ends here
