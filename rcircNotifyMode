;;; rcirc-notify-mode.el -- an emacs buffer in rcirc-notify-mode major mode
;;
;; Copyright (c) 2009 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://pgsql.tapoueh.org/elisp/rcirc
;; Version: 0.2
;; Created: 2009-06-27
;; Keywords: IRC rcirc notify
;;
;; This file is NOT part of GNU Emacs.
;;
;; Emacs Integration:
;; (require 'rcirc-notify-mode)

(require 'rcirc)

(defgroup rcirc-notify-mode nil "rcirc-notify-mode customization group"
  :group 'convenience)

(defcustom rcirc-notify-mode:buffer-name "*rcirc-notify*"
  "buffer name where to collect the notifications"
  :group 'rcirc-notify-mode)

(defcustom rcirc-notify-mode:message "%s is calling your name in %s."
  "Format if message to display in libnotify popup.
'%s' will expand to the nick that notified you."
  :group 'rcirc-notify-mode)

(defcustom rcirc-notify-mode:message-private "%s sent a private message."
  "Format if message to display in libnotify popup.
'%s' will expand to the nick that sent the message."
    :group 'rcirc-notify-mode)

(defcustom rcirc-notify-mode:nick-alist nil
  "An alist of nicks and the last time they tried to trigger a notification."
  :group 'rcirc-notify-mode)

(defcustom rcirc-notify-mode:timeout 1
  "Number of seconds that will elapse between notifications from the same person."
  :group 'rcirc-notify-mode)

(defcustom rcirc-notify-mode:time-format "%H:%M "
  "Time format to use in the *rcirc-notify* buffer as line-prefix"
  :group 'rcirc-notify-mode)

(defvar rcirc-notify-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      'rcirc-notify-mode:follow-message)
    (define-key map (kbd "r")        'rcirc-notify-mode:mark-as-read)
    (define-key map (kbd "p")        'rcirc-notify-mode:mark-all-previous-as-read)
    (define-key map (kbd "a")        'rcirc-notify-mode:mark-all-as-read)
    
    map)
  "Keymap for `rcirc-notify-mode'.")

;;;###autoload
(define-derived-mode rcirc-notify-mode fundamental-mode "rcirc-notify-mode"
  "A major mode for handling rcirc notifications"
  :group 'rcirc-notify-mode)

(defun rcirc-notify-mode:follow-message ()
  (interactive)
  (rcirc-notify-mode:mark-as-read)
  (set-window-buffer (selected-window) (get-text-property (point) 'rcirc-buffer)))

(defun rcirc-notify-mode:mark-as-read ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (remove-text-properties (line-beginning-position) (line-end-position) '(face default)))))

(defun rcirc-notify-mode:mark-all-previous-as-read ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (rcirc-notify-mode:mark-as-read)
      (while (not (eq -1 (forward-line -1)))
	(remove-text-properties 
	 (line-beginning-position) (line-end-position) '(face default))))))

(defun rcirc-notify-mode:mark-all-as-read ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (rcirc-notify-mode:mark-all-previous-as-read))))

(defun rcirc-notify-mode:create-notify-buffer ()
  "Create the rcirc-notify-mode:buffer-name buffer in read-only"
  (let ((notify-buffer (get-buffer-create rcirc-notify-mode:buffer-name)))
    (with-current-buffer notify-buffer
      (setq buffer-read-only t)
      ;; ensure we're in the rcirc-notify-mode major mode -- we could just
      ;; be creating the buffer.
      (unless (eq major-mode 'rcirc-notify-mode)
	(rcirc-notify-mode)))
    notify-buffer))

(defun rcirc-notify-mode:append (message face target-buffer-name)
  "append a message in the buffer rcirc-notify-mode:buffer-name"
  (let ((notify-buffer (rcirc-notify-mode:create-notify-buffer)))
    (with-current-buffer notify-buffer
      (save-excursion
	(goto-char (point-max))
	(insert (propertize message
			    'line-prefix (format-time-string
					  rcirc-notify-mode:time-format (current-time))
			    'read-only t
			    'face face
			    'rcirc-buffer target-buffer-name))
	(insert "\n")))))

(defun rcirc-notify-mode:notify (sender target target-buffer-name)
  "fill in the *rcirc-motify* buffer"
   (rcirc-notify-mode:append (format rcirc-notify-mode:message sender target)
			     'rcirc-nick-in-message 
			     target-buffer-name))

(defun rcirc-notify-mode:notify-private (sender target target-buffer-name)
  "fill in the *rcirc-motify* buffer"
  (rcirc-notify-mode:append (format rcirc-notify-mode:message-private sender target) 
			    'rcirc-nick-in-message-full-line
			    target-buffer-name))

(defun rcirc-notify-mode:nick-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`my-rcirc-notify-timeout'."
  (unless delay (setq delay rcirc-notify-mode:timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick rcirc-notify-mode:nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) rcirc-notify-mode:nick-alist)
      t)))

(defun rcirc-notify-mode:notify-me (proc sender response target text)
  "Notify the current user when someone sends a message that
matches a regexp in `rcirc-keywords'."
  (interactive)

  (when (and (string-match (concat (rcirc-nick proc) "[:, $]") text)
	     ;(not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender))
             (rcirc-notify-mode:nick-allowed sender))
    (rcirc-notify-mode:notify sender target (current-buffer))))

(defun rcirc-notify-mode:privmsg (proc sender response target text)
  "Notify the current user when someone sends a private message to them."
  (interactive)
  (when (and (string= response "PRIVMSG")
             ;(not (string= sender (rcirc-nick proc)))
             (not (rcirc-channel-p target))
             (rcirc-notify-mode:nick-allowed sender))

    (rcirc-notify-mode:notify-private sender target (current-buffer))))

(defun rcirc-notify-mode:switch-to-notify-buffer ()
  "A way to quickly switch to notifications buffer"
  (interactive)
  (let ((notify-buffer (get-buffer-create rcirc-notify-mode:buffer-name)))
    (with-current-buffer notify-buffer
      ;; move point to first unread line, from current position
      ;; that will be the first line (maybe current one) where 'face isn't nil
      ;; loop forward to find unread notification (non nil 'face property)
      (while (and (< (point) (point-max))
		  (not (get-text-property (point) 'face)))
	(forward-line 1)))
	  
    (set-window-buffer (selected-window) notify-buffer)))

(add-hook 'rcirc-print-hooks 'rcirc-notify-mode:privmsg)
(add-hook 'rcirc-print-hooks 'rcirc-notify-mode:notify-me)
(add-hook 'rcirc-mode-hook   (lambda ()
			       (local-set-key (kbd "C-c n") 
					      'rcirc-notify-mode:switch-to-notify-buffer)))

(provide 'rcirc-notify-mode)
