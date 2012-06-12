;;; dying.el --- Minor mode kills own buffer after lifetime

;; Copyright (C) 2012  Tobias Naehring

;; Author: Tobias Naehring <i@tn-home.de>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; dying-mode is a minor mode. A buffer in dying mode is killed after
;; its lifetime.  The default lifetime is
;; `dying-default-lifetime'. You can change the lifetime via
;; dying-mode's minor-mode menu or with the command
;; dying-set-lifetime.  The command `start-dying-mode' starts
;; `dying-mode' with the specified lifetime.  If there are only
;; `dying-countdown-duration' seconds left before kill a countdown
;; starts.
;;; Changes:
;; 2012-05-17: More general time formats in `start-dying-mode'
;; 2012-06-11: Comfortable input for `dying-default-lifetime' (widget for `customize-variable' and property variable-interactive for `set-variable')
;;; Code:

(require 'timer)
(require 'diary-lib)
(require 'widget)

(define-widget 'dying-lifetime-widget 'editable-field
  "Lifetime in the format as for `run-at-time'"
  :format "Lifetime: %v "
  :prompt-history 'widget-field-history
  :validate (lambda (widget)
	      (unless (timer-translate-time (widget-value widget))
		(widget-put widget :error (format "Invalid time-format: %s"
                                                  (widget-value widget)))
		widget))
  :match (lambda (widget value)
	   (if (dying-check-lifetime value) t)))

(defun dying-check-lifetime (val)
  (if (or (and (stringp val) (string= val ""))
	  (timer-translate-time val))
      val))

(put 'dying-default-lifetime 'variable-interactive
     '(let (lifetime)
	(while (null  (setq lifetime (dying-check-lifetime (read-string "Default Lifetime:" nil dying-default-lifetime-history)))))
	(list lifetime)))

(defvar dying-default-lifetime-history ())

(defcustom dying-default-lifetime "10 minutes" "Default lifetime of buffer in seconds." :group 'dying :type 'dying-lifetime-widget)

(defvar dying-timer nil
  "Timer for dying mode. (nil when unset)")
(make-variable-buffer-local 'dying-timer)

(defcustom dying-countdown-duration 99 "Time before kill when countdown starts." :group 'dying :type 'integer)


;; Stolen from run-at-time:
(defun timer-translate-time (time)
  "Transform time input into the internal format as needed for `current-time'."
  ;; Handle numbers as relative times in seconds.
  (if (numberp time)
      (setq time (timer-relative-time (current-time) time)))

  ;; Handle relative times like "2 hours 35 minutes"
  (if (stringp time)
      (let ((secs (timer-duration time)))
	(if secs
	    (setq time (timer-relative-time (current-time) secs))
	  (setq time nil)
	  )))

  ;; Handle "11:23pm" and the like.  Interpret it as meaning today
  ;; which admittedly is rather stupid if we have passed that time
  ;; already.  (Though only Emacs hackers hack Emacs at that time.)
  (if (stringp time)
      (progn
	(let ((hhmm (diary-entry-time time))
	      (now (decode-time)))
	  (if (>= hhmm 0)
	      (setq time
		    (encode-time 0 (% hhmm 100) (/ hhmm 100) (nth 3 now)
				 (nth 4 now) (nth 5 now) (nth 8 now)))))))
  time)

(defvar dying-lifetime 'dying-default-lifetime
  "Lifetime of the current buffer. Defaults to `dying-default-lifetime'")
(make-variable-buffer-local 'dying-lifetime)

(defun dying-lifetime ()
  "Return the remaining lifetime of the current buffer."
  (float-time (time-subtract dying-kill-time (current-time))))

(defun dying-handler (buf)
  "This internal function is called from the dying-timer.
It kills the buffer when dying-kill-time is reached.
Else it updates the countdown in the mode-line."
  (when (buffer-live-p buf)
    (with-current-buffer buf 
      (if (time-less-p (current-time) dying-kill-time)
	  (let ((next (timer-relative-time (timer--time dying-timer) 1))) ;;< countdown
	    (setq dying-lighter (concat "|" (number-to-string (round (dying-lifetime)))  "|"))
	    (force-mode-line-update)
	    (if (time-less-p next dying-kill-time)
		(timer-set-time dying-timer next)
	      (timer-set-time dying-timer dying-kill-time))
	    (timer-activate dying-timer))
	;; kill:
	(cancel-timer dying-timer)
	(kill-buffer)))))

(defun dying-set-lifetime (&optional time)
  "Set the lifetime of the current buffer.
Don't limit lifetime of current buffer if TIME is the empty string
\(or nil in non-interactive usage)."
  (interactive "sNew Lifetime:")
  (if (and time (null (equal time "")))
      (progn
	(unless (setq time (timer-translate-time time))
	  (error "Wrong lifetime format."))
	(setq dying-kill-time time)
	(setq time (timer-relative-time dying-kill-time (- dying-countdown-duration)))
	(if (time-less-p (current-time) time)
	    (setq dying-lighter (concat ">" (number-to-string dying-countdown-duration) "|"))
	  (let ((lifetime (dying-lifetime)))
	    (setq time (timer-relative-time dying-kill-time (- (floor lifetime))))
	    (setq dying-lighter (concat "|" (number-to-string (round lifetime)) "|"))))
	(force-mode-line-update)
	(if (timerp dying-timer)
	    (progn
	      (timer-set-time dying-timer time)
	      (timer-activate dying-timer))
	  (setq dying-timer (run-with-timer time nil 'dying-handler (current-buffer)))))
    (if (timerp dying-timer)
	(progn
	  (setq dying-lighter "||")
	  (force-mode-line-update)
	  (message "Lifetime not limited.")
	  (cancel-timer dying-timer)))))

(defun dying-cancel ()
  "Don't limit the lifetime of this buffer."
  (interactive)
  (dying-set-lifetime))

(defvar dying-lighter "||")
(make-variable-buffer-local 'dying-lighter)

(defvar dying-kill-time nil)
(make-variable-buffer-local 'dying-kill-time)

(defun start-dying-mode (lifetime)
  "Start `dying-mode' with lifetime LIFETIME (format as for `run-at-time').
If you want to start `dying-mode' with an unlimited lifetime just respond with enter to the lifetime-query.
If you use this function non-interactively, give the lifetime as a the number of seconds (with type `numberp')
or as `nil' when you want to start with an unlimited lifetime."
  (interactive (let (str lifetime) (while (progn
					    (setq str (read-string "Lifetime (number of seconds, or empty string for unlimited lifetime):" nil nil ""))
					    (and
					     (if (string= str "") (setq lifetime nil) t)
					     (null (setq lifetime (timer-translate-time str))))))
		    (list lifetime)))
  (setq dying-lifetime lifetime)
  (dying-mode))

(define-minor-mode dying-mode "Dying minor mode. Buffer is killed after `dying-default-lifetime' seconds.
If you want to set the lifetime directly use the command `start-dying-mode'."
  :lighter dying-lighter
  :init-value 'nil
  :keymap '(([menu-bar Dying] . (menu-item "Dying" (keymap "Dying"
							   (dying-mode-off "Switch Off Dying Mode" . dying-mode)
							   (dying-mode-lifetime "Set Lifetime" . dying-set-lifetime)
							   (dying-mode-cancel "Don't Limit Life Time" . dying-cancel)
							   ))))
  (if dying-mode
      (dying-set-lifetime (if dying-lifetime
			      (if (symbolp dying-lifetime)
				(eval dying-lifetime)
				dying-lifetime)
			    dying-default-lifetime))
    (dying-cancel)))

(provide 'dying)
;;; dying.el ends here
