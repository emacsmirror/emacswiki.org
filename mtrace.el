;;; mtrace.el --- keep track of hidden buffer changes

;; Copyright (c) 2000, 2001, 2002 Michele Bini

;; Author: Michele Bini <mibin@libero.it>
;; Created: 25 Dec 2000
;; Version: 0.9
;; Keywords: convenience

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This packages implements mtrace-mode, a minor mode that allows you
;; to get notified of changes occurring to hidden buffers.

;; It may for example be used to get notified when there is some
;; activity in an IRC channel buffer that is not currently displayed
;; in any window.

;; Another very useful feature is provided by the command
;; mtrace-pop-to-buffer, as it lets you switch directly to
;; those buffers where hidden activity occurred, even in your
;; absence.

;; mtrace-switch-to-buffer-other-window is analogous, but
;; selects the buffer in a different window.

;; New versions will be released in the following locations:
;; - http://www.emacswiki.org/cgi-bin/wiki.pl?MtraceMode
;; - the gnu.emacs.sources newsgroup

;;; History:
;; 2002-02-15  Michele Bini  <mibin@libero.it>
;;
;; 	* mtrace.el: Major rewrite, much code simplification.
;; 	Renamed functions: mtrace-pop-to-buffer and
;; 	mtrace-switch-to-buffer-other-window.
;; 	Added mtrace-display-buffer-hook.
;;
;; 2002-02-06  Michele Bini  <mibin@libero.it>
;;
;; 	* mtrace.el: Added defgroup and defcustom definitions.
;;
;; 2002-01-24  Michele Bini  <mibin@libero.it>
;;
;; 	* mtrace.el: Added mtrace-changes-limit.
;;
;; 2001-12-04  First release on the Emacs Wiki.

;;; Code:


;;;###autoload
(defvar mtrace-mode nil)
(make-variable-buffer-local `mtrace-mode)
(or (assq 'mtrace-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(mtrace-mode " mTrace") minor-mode-alist)))

(defgroup mtrace nil
  "Keep track of hidden buffer changes."
  :group 'convenience :prefix 'mtrace-)

(defcustom mtrace-mode-hook nil
  "Hook run when mtrace mode is enabled in a buffer."
  :group 'mtrace :type 'hook)
(defcustom mtrace-regexp "\n"
  "Regexp defining relevant hidden changes.

Please note that using a regexp matching more than one character can
be problematic under some circumstances."
  :group 'mtrace :type 'regexp)
(defcustom mtrace-notify-changes-limit 7
  "Maximum number of changes to notify, per buffer."
  :group 'mtrace :type 'number)
(defcustom mtrace-display-buffer-hook nil
  "Hook run when a traced buffer with pending changes is displayed.
The buffer must have been displayed using either
`mtrace-pop-to-buffer' or `mtrace-switch-to-buffer-other-window'.
The buffer is run narrowed to the region affected by the collected
changes.  Hooking to this function may be useful to perform actions on
this region, like highlighting strings."
  :group 'mtrace :type 'hook)
(defvar mtrace-pending nil)
(make-variable-buffer-local `mtrace-pending)

(defun mtrace-message (&rest args)
  (let ((message-log-max nil))
    (apply 'message args)))

;;;; delayed notifications
(defgroup mtrace-delay nil
  "Delay mtrace notifications."
  :group 'mtrace-notify :prefix 'mtrace-delay)
(defcustom mtrace-delay-notifications t
  "Whether to delay the notification of changed buffers.
If nil, the notification message will appear immediately in the mode
line whenever a relevant change happens.

See also `mtrace-delay-seconds'."
  :group 'mtrace-delay :type 'boolean)
(defcustom mtrace-delay-seconds 0.2
  "Seconds to wait before displaying a notification.
If another notification is triggered while waiting, it will be
displayed in place of this.  This helps in ensuring that notifications
will not happen too frequently."
  :group 'mtrace-delay :type 'number)
(defvar mtrace-delay-timer nil)
(make-variable-buffer-local 'mtrace-delay-timer)
(defun mtrace-delay-notification ()
  (unless mtrace-delay-timer
    (setq mtrace-delay-timer
	  (run-at-time mtrace-delay-seconds nil 'mtrace-delayed-notify
		       (current-buffer)))))
(defun mtrace-delayed-notify (buffer)
  (with-current-buffer buffer
    (setq mtrace-delay-timer nil)
    (mtrace-notify (buffer-name (current-buffer)))))

(defvar mtrace-marker-min nil)
(defvar mtrace-marker-max nil)
(make-variable-buffer-local 'mtrace-marker-min)
(make-variable-buffer-local 'mtrace-marker-max)

(defvar mtrace-count 0)
(make-variable-buffer-local 'mtrace-count)

(defun mtrace-add (p1 p2)
  (if mtrace-pending
      (progn
	(set-marker mtrace-marker-min
		    (min p1 (marker-position mtrace-marker-min)))
	(set-marker mtrace-marker-max
		    (max p2 (marker-position mtrace-marker-max))))
    (setq mtrace-marker-min
	  (set-marker (or mtrace-marker-min (make-marker)) p1))
    (setq mtrace-marker-max
	  (set-marker (or mtrace-marker-max (make-marker)) p2)))
  (setq mtrace-count (+ mtrace-count 1))
  (when (> mtrace-count mtrace-notify-changes-limit)
    ;;(mtrace-remove-hook) ; this won't work as after-change-functions
					; is overwritten
    (run-with-idle-timer 0 nil 'mtrace-remove-hook (current-buffer)))
  (setq mtrace-pending t))
(defun mtrace-clear ()
  (setq mtrace-count 0)
  (when mtrace-mode (mtrace-install-hook)))
(defun mtrace-notify (&optional info)
  (unless info (setq info ""))
  (if mtrace-pending
      (mtrace-message
       "mTrace[%s]:   %s"
       info
       (apply
	'concat
	(save-excursion
	  (let ((pmin (point-min))
		(beg (marker-position mtrace-marker-min))
		(end (marker-position mtrace-marker-max))
		(w (frame-width))
		(s nil))
	    (setq beg (max pmin (- beg 1) (- end w)))
	    (goto-char end)
	    (let ((p end))
	      (while (> p beg)
		(beginning-of-line)
		(let ((n (point)))
		  (if (< n p)
		      (setq s (cons " ... " (cons
					     (buffer-substring n p)
					     s))))
		  (when (> n pmin)
		    (forward-char -1)))
		(setq p (point))))
	    (and s (reverse (cdr s)))))))
    (mtrace-message "mTrace[%s]: no changes" info)))

(defun mtrace-after-change (&optional arg1 arg2 arg3)
  (when (and (not (get-buffer-window (current-buffer) t))
	     (< arg1 arg2))
    (save-excursion
      (save-match-data
	(goto-char arg1)
	(when (re-search-forward mtrace-regexp arg2 t)
	  (mtrace-add (match-beginning 0) (match-end 0))
	  (if mtrace-delay-notifications
	      (mtrace-delay-notification)
	    (mtrace-notify (buffer-name (current-buffer)))))))))

(defun mtrace-pop-to-buffer (&optional other-window)
  "Switch to a buffer with pending changes.
If the optional argument OTHER-WINDOW is non-nil, select that buffer
in another window.

See also `mtrace-switch-to-buffer-other-window'."
  (interactive)
  (let ((l (buffer-list)) (b nil))
    (while (and (not b) l)
      (with-current-buffer (car l)
	(when mtrace-pending
	  (setq b (car l))))
      (setq l (cdr l)))
    (if b (with-current-buffer b
	    (when mtrace-mode
	      ;;(mtrace-notify)
	      (save-restriction
		(narrow-to-region
		 mtrace-marker-min mtrace-marker-max)
		(run-hooks
		 'mtrace-display-buffer-hook))
	      (mtrace-clear))
	    (setq mtrace-pending nil)
	    (if other-window
		(switch-to-buffer-other-window b)
	      (pop-to-buffer b))
	    (recenter))
      (mtrace-message "mTrace: No changed buffers found."))))
(defun mtrace-switch-to-buffer-other-window ()
  "Display a buffer with pending changes, in a different window.

This is equivalent to calling `mtrace-pop-to-buffer' with the
other-window option set."
  (interactive)
  (save-selected-window
    (mtrace-pop-to-buffer t)))

(defun mtrace-install-hook ()
  (add-hook 'after-change-functions 'mtrace-after-change nil t))
(defun mtrace-remove-hook (&optional buffer)
  (if buffer
      (with-current-buffer buffer
	(remove-hook 'after-change-functions 'mtrace-after-change t))
    (remove-hook 'after-change-functions 'mtrace-after-change t)))

(defun mtrace-mode-enable (&optional arg)
 (mtrace-install-hook)
  (setq mtrace-mode t)
  (run-hooks 'mtrace-mode-hook))
(defun mtrace-mode-disable (&optional arg)
  (mtrace-remove-hook)
  (setq mtrace-mode nil))

(defun mtrace-trigger-on (regexp)
  "Specify which regexp to consider a relevant buffer change.

Takes just one argument, REGEXP."
  (interactive
   (list
    (read-string
     "Regexp to trigger mtrace-on (return to reset):" ""
     'mtrace-trigger-on-history)))
  (if (string-equal "" regexp)
      (kill-local-variable 'mtrace-regexp)
    (setq mtrace-regexp regexp)
    (unless mtrace-mode (mtrace-mode-enable))))

(defun mtrace-mode (&optional arg)
  "Toggle mtrace mode.
This minor mode allows you to keep track of hidden buffer changes.
When an hidden change to a buffer occurs, you are notified of it in
the mode-line with a message (possibly reporting the content of that
change, also).

With prefix ARG, turn mtrace mode on iff ARG is positive.
See also `mtrace-pop-to-buffer'."
  (interactive)
  (cond
   ((null arg)
    (if mtrace-mode (mtrace-mode-disable) (mtrace-mode-enable)))
   ((> (prefix-numeric-value arg) 0)
    (mtrace-mode-enable))
   (t (mtrace-mode-disable))))

(provide 'mtrace)
;;; mtrace.el ends here
