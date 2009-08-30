;;; poor-mans-bidi.el --- BiDirectional editing mode  -*-coding: utf-8;-*-

;; Copyright (C) 2008 Niels Giesen
;; <nielsDODOgiesen@gmailDINOSAURcom, with the extinct animals
;; replaced by dots.> 

;; Author: Niels Giesen nielsDODOgiesen@gmailDINOSAURcom, with
;; the extinct animals replaced by dots. 
;; Keywords: languages, wp
;; Version: 0.2

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This library provides a 'BiDified' mirror of the current
;; buffer, using an external program such as 'fribidi' (although
;; for paragraph mode, you might want to switch that to
;; bidiv). Use 
;; (autoload 'poor-mans-bidi-mode "poor-mans-bidi" "" t)
;; to have this code autoloaded.
;; See the documentation for `poor-mans-bidi-mode' for more
;; information.  

;;; Code:
(defgroup poor-mans-bidi nil
  "Customization group for `poor-mans-bidi-mode'."
  :group 'wp)

(defcustom poor-mans-bidi-filter-command
  (lambda () (format "fribidi --nobreak -w %s" fill-column))
  "Function to return the command invoked by `poor-mans-bidi-decode-buffer'.

It should handle the conversion from logical->visual ordering,
  while preserving lines.  For this reason, `auto-fill-mode' is
  turned  on as well.

Pros fribidi:
- handles characters better than bidiv;
- --nobreak option is nice to keep line numbers the same across
buffers.

Pros bidiv:
- can interpret on paragraph-level

The cons are that either doesn't do/have what the other does/has."
  :group 'poor-mans-bidi
  :type '(function))

(defcustom poor-mans-bidi-font-lock-keywords
  '(("\\([ְֱֲֳִֵֶַָֹֻּֽֿﬞ׀ׁׂ][םמןנבץצחזלךכהגדסףפאיײױװותטרעשק]\\)"
     (0 (progn (compose-region (match-beginning 1) (match-end 1))
	       nil))))
  "Font lock keyword rules for BiDi mirror buffers.

The rules are meant for composing complex characters from base (Unicode) chars
and punctuation marks (that are used in e.g. Yiddish texts) in the mirror
buffer showing bidirectional output.  If you find other rules useful, please
send a report, so that they can be included in this file."
  :group 'poor-mans-bidi
  :type '(alist))

(defcustom poor-mans-bidi-minimal-context-length 3
  "Minimal length of search context (or rather: pretext).

This variable is used by `poor-mans-bidi-decode-buffer' when locating point in
 the mirror buffer. Setting this too short may find point at another, like,
 point, esp. with short palindromes such as the word \"a\" in English or (its
 translation!) \"א\" in Yiddish in BiDi texts (not in one-way texts) . Set it
 to 4 if this really bugs you.

You might also want to change it to something larger to start looking in the
opposite direction sooner, e.g. when you edit texts that are mainly RTL (or
LTR), and not so much intertwined. The RTL-direction is searched first:
assuming that if variable `poor-mans-bidi-mode' is non-nil, you are probably
editing RTL text."
  :group 'poor-mans-bidi
  :type '(integer))

(defcustom poor-mans-bidi-timer-interval .1
  "Interval used by `poor-mans-bidi-timer'.

Set this to a small value (such as 0.01), to have quick response in the mirror
buffer.  Smaller than that is hardly noticeable.  Larger values than the default
might be wise for slower computers.

When changed, make sure to run \\[poor-mans-bidi-disable-altogether] and
\\[poor-mans-bidi-mode] again, in order to reset the timer so it will see its
new interval."
  :group 'poor-mans-bidi
  :type '(float))

(defcustom poor-mans-bidi-inhibited-major-modes
  '((w3m-mode))
  "Rules for alternative modes for bidirectional mirror buffers.

This variable overrides the default behaviour that mirror buffers made with
the command `poor-mans-bidi-mode' have the same major mode as the major mode
for the source buffer.

This variable is an alist of the form ((MAJOR-MODE . ALTERNATIVE-MODE)), where
ALTERNATIVE-MODE will be the major mode for buffers mirroring buffers where
MAJOR-MODE is the major mode.

As a shortcut, if MAJOR MODE is nil, `fundamental-mode' will become the
major mode for the mirror buffer."
  :group 'poor-mans-bidi
  :type '(alist))

(define-minor-mode poor-mans-bidi-mode 
  "Minor mode for writing bidirectional text. 

Show a 'BiDified' mirror of the current buffer, using external
  programs such  as 'fribidi' (although for paragraph mode, you
  might want to switch that to bidiv). Adjust the variable
  `poor-mans-bidi-filter-command' to use another filter.

In the mirroring buffer, a new point tries to be located merely
  on context in source buffer, NOT on any knowledge of the
  algorithm described at url 
`http://www.unicode.org/unicode/reports/tr9/'.  

Toggle the mirroring with the command \\[poor-mans-bidi-mode].
  This sets an idle timer doing its work every tenth of a
  second, so if you are not working on a BiDi text, you'd better
  issue the  command \\[poor-mans-bidi-disable-altogether] 

Could be very well adapted for lisp-only handling however. See
  comments in the definition for the function
  `poor-mans-bidi-decode-buffer' if you feel so inclined." 
  nil
  " BiDi"
  '(("\C-cb" . poor-mans-bidi-mode))
  (if poor-mans-bidi-mode
      (poor-mans-bidi-enable)
    (poor-mans-bidi-disable)))

(defvar poor-mans-bidi-timer nil "Timer object.")
(defvar poor-mans-bidi-tick nil "To compare with output from `buffer-modified-tick'.")
(defvar poor-mans-bidi-point nil "To compare old point with current (point).")
(defvar poor-mans-bidi-mirror-buffer nil "Buffer for BiDi display.")
(defconst poor-mans-bidi-mirror-buffer-postfix "-BiDi")

(defun poor-mans-bidi-enable ()
  "Enable BiDi display in mirror buffer."
  (set (make-local-variable 'poor-mans-bidi-tick)
       (buffer-modified-tick))
  (set (make-local-variable 'poor-mans-bidi-point)
       (point))
  (set (make-local-variable 'poor-mans-bidi-mirror-buffer)
       (concat (buffer-name (current-buffer))
	       poor-mans-bidi-mirror-buffer-postfix))
  (if (timerp poor-mans-bidi-timer)
      (timer-activate poor-mans-bidi-timer)
    (setq poor-mans-bidi-timer
	  (run-with-idle-timer poor-mans-bidi-timer-interval poor-mans-bidi-timer-interval
			  (lambda ()
			    (when
				(and poor-mans-bidi-mode
				(or (and (boundp 'poor-mans-bidi-tick)
 				     (< poor-mans-bidi-tick
 					(buffer-modified-tick)))
				    (not (= poor-mans-bidi-point (point)))))
			      (setq poor-mans-bidi-point (point))
			      (setq poor-mans-bidi-tick (buffer-modified-tick))
			      (poor-mans-bidi-decode-buffer))))))
  (auto-fill-mode 1)
  ;; Copy major mode over and add font-locking
  (let ((mode major-mode))
    (if (assoc mode poor-mans-bidi-inhibited-major-modes)
        (setq mode
              (or (cdr (assoc mode poor-mans-bidi-inhibited-major-modes))
                  'fundamental-mode)))
    (save-window-excursion
      (switch-to-buffer-other-window poor-mans-bidi-mirror-buffer)
      (funcall mode)
      (font-lock-mode -1)
      (poor-mans-bidi-add-font-locking))))

(defun poor-mans-bidi-disable ()
  "Disable BiDi display in mirror buffer."
  (and poor-mans-bidi-mirror-buffer
       (bufferp (get-buffer poor-mans-bidi-mirror-buffer))
   (kill-buffer poor-mans-bidi-mirror-buffer)))

(defun poor-mans-bidi-disable-altogether ()
  "Disable BiDi altogether.

This command disables BiDi altogether, until function `poor-mans-bidi-mode' is
  called again with a positive argument.  Its foremost use is to kill the
  timer.  Kills all associated BiDi-mirror buffers too."
  (interactive)
  (when  (timerp poor-mans-bidi-timer)
    (cancel-timer poor-mans-bidi-timer))
  (setq poor-mans-bidi-timer nil)
  (mapcar (lambda (b)
	    (when (and
                   (buffer-live-p b)
                   (string-match
                    (format "%s$"
                            poor-mans-bidi-mirror-buffer-postfix)
                    (buffer-name b)))
                   (kill-buffer b))
            (when poor-mans-bidi-mode (poor-mans-bidi-mode -1))) (buffer-list)))

(defun poor-mans-bidi-decode-buffer ()
  "Send buffer to filter defined by  `poor-mans-bidi-filter-command'."
  (let ((line-number (line-number-at-pos))
        (column (current-column))
        (context (buffer-substring-no-properties
                       (point-at-bol)
                       (point)))
        (buffer (current-buffer))
        (mode major-mode))
    ;; change following sexp to adopt to Lisp-only logical->visual
    ;; conversion of our buffer.  Just make sure to  put the output in
    ;; `poor-mans-bidi-mirror-buffer', then it will work.
    (shell-command-on-region (point-min)
                             (point-max)
                             (funcall poor-mans-bidi-filter-command)
                             poor-mans-bidi-mirror-buffer)
    ;; Here's  a stab  at what  I  just said.  It works  roughly;
    ;; however `bidi-logical-to-visual'  is not perfect  (yet, as
    ;; this mode isn't either), so I'll rely on fribidi for now.
    ;;
    ;; (save-window-excursion
    ;;   (let ((str (bidi-logical-to-visual (buffer-string))))
    ;;     (switch-to-buffer-other-window poor-mans-bidi-mirror-buffer)
    ;;     (erase-buffer)
    ;;     (insert str)))
    (switch-to-buffer-other-window poor-mans-bidi-mirror-buffer)
    (goto-line line-number)
    ;; Reductively try and find position by context as defined by beginning of
    ;; source line up until point -- and smaller each run. Stop search when
    ;; length of the search string equals the value of
    ;; `poor-mans-bidi-minimal-context-length'. Begin search RTL.
    (end-of-line)
    (unless (< poor-mans-bidi-minimal-context-length
               (length (do ((str context (substring str 1)))
                           ((or (search-backward (poor-mans-bidi-string-reverse
                                                  str)
                                                 (point-at-bol) t)
                         (< (length str) poor-mans-bidi-minimal-context-length))
                            str))))
      (progn (beginning-of-line)
             (do ((str context (substring str 1)))
                 ((or (search-forward str (point-at-eol) t)
                      (< (length str) poor-mans-bidi-minimal-context-length))
                  str))))
    (switch-to-buffer-other-window buffer)))

(defun poor-mans-bidi-add-font-locking ()
  "Add font locking to BiDi mirror buffer.

See also variable `poor-mans-bidi-font-lock-keywords'."
  (unless (and (boundp 'auto-composition-mode)
               auto-composition-mode)
    (font-lock-add-keywords
     nil poor-mans-bidi-font-lock-keywords)
    (font-lock-mode 1)))
  
(defun poor-mans-bidi-remove-font-locking ()
  "Remove font locking to BiDi mirror buffer.

See also variable `poor-mans-bidi-font-lock-keywords'."
  (font-lock-remove-keywords
   nil poor-mans-bidi-font-lock-keywords)
  (font-lock-mode 1))

(defun poor-mans-bidi-string-reverse (str)
  (mapconcat 'identity (reverse (split-string str "")) ""))

(provide 'poor-mans-bidi)
;;; poor-mans-bidi.el ends here
