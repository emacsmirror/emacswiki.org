;;; tempbuf.el --- kill unused buffers in the background
;; Copyright (c) 2001, 2002 Michele Bini

;; Author: Michele Bini <mibin@libero.it>
;; Created: 11 Sep 2001
;; Version: 1.0
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

;; This package implements tempbuf-mode, a minor mode that enables
;; buffers to get automatically deleted in the background when it can
;; be deduced that they are no longer of any use.

;; It could be common for example to apply this mode to dired-mode
;; buffers or read-only buffers visiting files, relieving you from
;; having to delete each of them manually when the buffer list grows
;; too large.

;; The algorithm employed increases the life expectancy of the most
;; used buffers, even if their usage is intermittent.  A buffer is
;; considered to be in use if interactive commands are being executed
;; on it or if it is being displayed in some window.
;; Buffers visiting files with unsaved content or with active
;; processes will not get automatically deleted, but you can override
;; this behavior by customizing tempbuf-expire-hook.

;; To turn on this mode on dired buffers, put this line in your .emacs:

;;     (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)

;; I find it also very convenient to turn on this mode in emacs
;; customization buffers, W3 (Emacs' Web Browser) buffers, UNIX 'man'
;; documentation buffers, and any buffer with view-mode activated.

;;     (add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
;;     (add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
;;     (add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
;;     (add-hook 'view-mode-hook 'turn-on-tempbuf-mode)

;; It may also be reasonable to activate it by default on any visited
;; file buffer (buffers with unsaved content will not get automatically
;; deleted, anyway):

;;     (add-hook 'find-file-hooks 'turn-on-tempbuf-mode)

;; You can set up things to make tempbuf-mode terminate idle
;; terminals:

;; (defun my-term-on-tempbuf-expire ()
;;   (when (get-buffer-process (current-buffer))
;;     (term-send-eof)))
;; (defun my-term-mode-patch ()
;;   (turn-on-tempbuf-mode)
;;   (add-hook 'tempbuf-expire-hook 'my-term-on-tempbuf-kill nil t))
;; (add-hook 'term-mode-hook 'my-term-mode-patch)

;; This way, a terminal emulator buffer will start receiving
;; periodic end-of-file signals if it does not seem to get any more
;; attention from the user, causing it to terminate if there is no
;; pending command (assuming that a well behaving shell is running on
;; that terminal).

;; I plan to release new versions in the following locations:
;; - http://www.emacswiki.org/cgi-bin/wiki.pl?TempbufMode
;; - the gnu.emacs.sources newsgroup

;;; History:
;; 2002-02-20  Michele Bini  <mibin@libero.it>
;;
;; 	* tempbuf.el (tempbuf-mode): Added "P" argument to
;; 	interactive, as it should be for a minor mode.
;; 	(turn-on-tempbuf-mode): removed call to make-local-hook; do
;; 	not alter tempbuf-minimum-timeout. Renamed from
;; 	tempbuf-mode-enable.
;; 	(turn-off-tempbuf-mode): Renamed from
;; 	tempbuf-mode-disable.
;;
;; 2002-02-17  Michele Bini  <mibin@libero.it>
;;
;; 	* tempbuf.el: Added customize definitions.
;;
;; 2001-12-21  Michele Bini  <mibin@libero.it>
;;
;; 	Release thru gnu.emacs.sources.
;;
;; 2001-12-16  Michele Bini  <mibin@libero.it>
;;
;; 	(tempbuf-buffers): Removed.
;; 	(tempbuf-expire-hook, tempbuf-expire): Added.
;; 	Buffers with active processes will no more be
;; 	automatically deleted.
;;
;; 2001-12-04  Michele Bini  <mibin@libero.it>
;;
;; 	First public release on the Emacs Wiki

;;; Code:

(defgroup tempbuf nil
  "Kill unused buffers in the background."
  :group 'convenience :prefix 'tempbuf-)

(defcustom tempbuf-life-extension-ratio 2
  "Ratio at which to extend the life expectancy of a used buffer.
This value should be greater than 1."
  :group 'tempbuf :type 'number)

(defcustom tempbuf-kill-message "Killed inactive buffer: %s."
  "Message used to signal the killing of a buffer.
If nil, do not show any message.  If a %s appears in the message, it
will get replaced with the name of the buffer being killed."
  :group 'tempbuf :type '(choice (const :tag "No message." nil)
				 string))

(defcustom tempbuf-mode-hook nil
  "Hook run after tempbuf mode is activated in a buffer."
  :group 'tempbuf :type 'hook)

(defcustom tempbuf-expire-hook nil
  "Hook run when a buffer expires to to inactivity.

The difference between this and `tempbuf-kill-hook' is that this hook
will be called even on buffers visiting files with unsaved content or
with active processes.  This hook can thus be used to kill, or perform
any other reasonable action on such buffers when they become
inactive."
  :group 'tempbuf :type 'hook)

(defcustom tempbuf-kill-hook nil
  "Hook run before a buffer gets killed due to inactivity.

It is possible for any function called by this hook to throw the tag
tempbuf-skip-kill.  When this happens the buffer killing will be
postponed."
  :group 'tempbuf :type 'hook)

(defcustom tempbuf-minimum-timeout 18
  "Wait at least this many seconds before killing a buffer.
The actual timeout for killing a buffer increases with user activity
on it.  This value prevents a completely unused buffer from being
killed too early."
  :group 'tempbuf :type 'number)

(defcustom tempbuf-mode-line-string " TmpB"
  "String displayed on the mode line when tempbuf is active."
  :group 'tempbuf :type '(choice (const :tag "No indicator." nil)
				 string))

(or (assq 'tempbuf-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(tempbuf-mode tempbuf-mode-line-string) minor-mode-alist)))

(defvar tempbuf-mode nil)
(make-variable-buffer-local 'tempbuf-mode)

;;;###autoload
(defun tempbuf-mode (&optional arg)
  "Toggle tempbuf mode.

With prefix ARG, turn the mode on if ARG is positive.
After mode activation, `tempbuf-mode-hook' is run."
  (interactive "P")
  (cond
   ((null arg)
    (if tempbuf-mode (turn-off-tempbuf-mode) (turn-on-tempbuf-mode)))
   ((> (prefix-numeric-value arg) 0) (turn-on-tempbuf-mode))
   (t (turn-off-tempbuf-mode))))

(defvar tempbuf-timer nil
  "Timer used internally by tempbuf mode.")

(defvar tempbuf-last-time nil
  "Holds the last time a command was executed on the buffer.")
(make-variable-buffer-local 'tempbuf-last-time)

(defvar tempbuf-timeout tempbuf-minimum-timeout
  "Current timeout for buffer expiring.")
(make-variable-buffer-local 'tempbuf-timeout)

(defvar tempbuf-activation-time nil
  "Time at which tempbuf mode was activated in the current buffer.")
(make-variable-buffer-local 'tempbuf-activation-time)

(defun tempbuf-time-diff (a b)
  "Return the number of seconds between two timestamps A and B."
  (let ((diff 0.0))
    (setq diff (+ diff (- (car a) (car b))))
    (setq diff (* diff (* 256 256)))
    (setq diff (+ diff (- (cadr a) (cadr b))))
    (when (and (< diff 4900.0) (cdr (cdr a)) (cdr (cdr b)))
      (setq diff
	    (+ diff (/ (float (- (cadr (cdr a)) (cadr (cdr b)))) 1000000))))
    diff))

(defun tempbuf-grace (&optional ct)
  "Extend the life expectancy of the current buffer.

The optional argument CT specifies a pre-calculated \"(current-time)\"
value."
  (setq tempbuf-timeout
	(+ tempbuf-minimum-timeout
	   (* tempbuf-life-extension-ratio
	      (tempbuf-time-diff
	       (or ct (current-time))
	       tempbuf-activation-time)))))

(defun tempbuf-check-buffers ()
  "Check all the buffers with tempbuf mode turned on.

Inactive buffers will expire and eventually get killed, active ones
will get additional life expectancy."
  (let ((ct (current-time)))
    (mapcar
     (lambda (buffer)
       (with-current-buffer buffer
	 (when tempbuf-mode
	   (if (get-buffer-window buffer t)
	       (progn
		 (tempbuf-post-command)
		 (tempbuf-grace ct))
	     (when (and (> (tempbuf-time-diff ct tempbuf-last-time)
			   tempbuf-timeout))
	       (tempbuf-expire ct))))))
     (buffer-list))))

(defun tempbuf-expire (&optional ct)
  "Expire the current buffer.

This function gets called after a certain period of inactivity in the
current buffer.
The hook `tempbuf-expire-hook' is run at first.
If the functions in that hook did not already take care about it,
the current buffer will be killed if it has no unsaved content and no
processes running.
The optional argument CT specifies a pre-calculated \"(current-time)\"
value."
  (let ((buffer (current-buffer)))
    (run-hooks 'tempbuf-expire-hook)
    (when (buffer-live-p buffer)
      (if (or buffer-offer-save
	      (and buffer-file-name (buffer-modified-p))
	      (get-buffer-process buffer))
	  (progn
	    (tempbuf-post-command)
	    (tempbuf-grace ct))
	(let ((name (buffer-name buffer)))
	  (catch 'tempbuf-skip-kill
	    (run-hooks 'tempbuf-kill-hook)
	    (kill-buffer buffer))
	  (when tempbuf-kill-message
	    (unless (buffer-live-p buffer)
	      (message tempbuf-kill-message
		       name))))))))

(defun tempbuf-post-command ()
  "Update `tempbuf-last-time'."
  (setq tempbuf-last-time (current-time)))

;;;###autoload
(defun turn-on-tempbuf-mode ()
  "Turn on tempbuf mode.

See also function `tempbuf-mode'."
  (when (not tempbuf-timer)
    (setq tempbuf-timer (run-at-time 15 15 'tempbuf-check-buffers)))
  (setq tempbuf-activation-time (current-time))
  (setq tempbuf-mode t)
  (tempbuf-grace)
  (add-hook 'post-command-hook 'tempbuf-post-command nil t)
  (tempbuf-post-command)
  (run-hooks 'tempbuf-mode-hook))

(defun turn-off-tempbuf-mode ()
  "Turn off tempbuf mode.

See also function `tempbuf-mode'."
  (setq tempbuf-mode nil))

(provide 'tempbuf)
;;; tempbuf.el ends here
