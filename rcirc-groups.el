;;; rcirc-groups.el -- an emacs buffer in rcirc-groups major mode
;;
;; Copyright (c) 2009 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://pgsql.tapoueh.org/elisp/rcirc
;; Version: 0.5
;; Created: 2009-06-27
;; Keywords: IRC rcirc notify
;;
;; This file is NOT part of GNU Emacs.
;;
;; Emacs Integration:
;; (require 'rcirc-groups)
;;
;; TODO: draw a bar showing where you stoped reading (hook to when the
;; buffer becomes non visible?)

(require 'cl)    ; push seems to be there
(require 'rcirc)

(defgroup rcirc-groups nil "rcirc-groups customization group"
  :group 'convenience)

(defcustom rcirc-groups:buffer-name "*rcirc-groups*"
  "buffer name where to collect the notifications"
  :group 'rcirc-groups)

(defcustom rcirc-groups:time-format "%Y-%m-%d %H:%M"
  "format string to use when displaying the time of later notification in *rcirc-groups*"
  :group 'rcirc-groups)

(defvar rcirc-groups:conversation-alist nil
  "An alist of conversation buffers and the number of times they mentionned your nick.")

(defun rcirc-groups:conversation-has-been-killed (conversation-entry)
  "returns t only when conversation's buffer has been killed"
  (endp (buffer-name (car conversation-entry))))

(defun rcirc-groups:format-conversation (conversation-entry)
  "pretty print a conversation in a propertized string, return the string"
  (propertize (buffer-name (car conversation-entry))
	      
	      'line-prefix 
	      (format 
	       "%s %s "
	       (format-time-string rcirc-groups:time-format 
				   (seconds-to-time (cddr elt)))
	       (cadr elt))

	      'face (if (> (cadr elt) 0) 'rcirc-nick-in-message 'default)))

(defun rcirc-groups:update-conversation-alist (buffer-or-name &optional reset)
  "Replace current values for given conversation buffer"
  (let* ((conversation-entry 
	  (assoc (get-buffer buffer-or-name) rcirc-groups:conversation-alist))
	 (notifs (cadr conversation-entry))
	 (buffer (get-buffer buffer-or-name))
	 (buffer-visible (memq buffer (rcirc-visible-buffers)))
	 (buffer-display-time (buffer-local-value 'buffer-display-time buffer))
	 (notification-time (time-to-seconds (current-time)))
	 (new-notif))

    ;; add a new entry in our alist when necessary
    (if conversation-entry
	(progn
	  ;; work out if we want to increment how many notifs we're lagging behind
	  (setq new-notif
		(if reset 0
		  (if (and (not buffer-visible) 
			   (or (null buffer-display-time)
			       (< (time-to-seconds buffer-display-time) 
				  notification-time)))
		      (+ 1 notifs)
		    notifs)))

	  ;; list maintenance: delete current entry, to prepare for pushing
	  ;; new one
	  (setq rcirc-groups:conversation-alist
		(assq-delete-all
		 (car conversation-entry) rcirc-groups:conversation-alist))

	  (push (cons (car conversation-entry)
		      (cons new-notif notification-time)) 
		rcirc-groups:conversation-alist))

      ;; new buffer we didn't track yet
      (when buffer
	(setq conversation-entry (cons buffer
				       (cons 1 notification-time)))
	(push conversation-entry rcirc-groups:conversation-alist)))))

(defvar rcirc-groups-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      'rcirc-groups:switch-to-conversation-buffer)
    (define-key map (kbd "g")        'rcirc-groups:refresh-conversation-alist)
    (define-key map (kbd "r")        'rcirc-groups:refresh-conversation-alist)
    (define-key map (kbd "c")        'rcirc-groups:catchup-conversation)
    (define-key map (kbd "C")        'rcirc-groups:catchup-all-conversations)
    (define-key map (kbd "l")        'rcirc-groups:list-mentionned-conversations)
    (define-key map (kbd "L")        'rcirc-groups:list-all-conversations)
    (define-key map (kbd "n")        'next-line)
    (define-key map (kbd "p")        'previous-line)
    (define-key map (kbd "<")        'beginning-of-buffer)
    (define-key map (kbd ">")        'end-of-buffer)
    (define-key map (kbd "q")        'rcirc-groups:quit-window)
    
    map)
  "Keymap for `rcirc-groups-mode'.")

(defvar rcirc-groups:display-all nil
  "Do we display all conversation regardless of notifications in *rcirc-groups*")

;;;###autoload
(define-derived-mode rcirc-groups-mode fundamental-mode "rcirc-groups-mode"
  "A major mode for handling rcirc notifications"
  :group 'rcirc-groups

  (setq mode-name "rcirc-groups")

  ;; remember whether we display all the conversations or only mentionned ones
  (make-local-variable 'rcirc-groups:display-all)

  ;; prepare a new conversion listing, and display it
  (rcirc-groups:refresh-conversation-alist)
  (rcirc-groups:list-mentionned-conversations))

(defun rcirc-groups:switch-to-conversation-buffer ()
  "switch from *rcirc-groups* buffer to referenced one"
  (interactive)
  (let ((conversation-buffer
	 (buffer-substring (line-beginning-position) (line-end-position))))
    (rcirc-groups:catchup-conversation)
    (set-window-buffer (selected-window) conversation-buffer)))

(defun rcirc-groups:refresh-conversation-alist ()
  "fill in rcirc-groups:conversation-alist all active conversations"
  (interactive)
  (dolist (elt (buffer-list))
    (with-current-buffer elt
      (when (and (eq major-mode 'rcirc-mode))
	(let ((entry (assoc elt rcirc-groups:conversation-alist)))
	  (when (not entry)
	    (push (cons elt (cons 0 (time-to-seconds (current-time))))
		  rcirc-groups:conversation-alist))))))

  ;; refresh also the *rcirc-groups* buffer display
  (rcirc-groups:list-conversations)
  rcirc-groups:conversation-alist)

(defun rcirc-groups:catchup-conversation ()
  "catchup conversation reinits the conversation-alist entry for current buffer"
  (interactive)
  (let* ((conversation-buffer (buffer-substring
				(line-beginning-position) (line-end-position))))    
    (rcirc-groups:update-conversation-alist conversation-buffer t)
    (rcirc-groups:refresh-conversation-alist)))

(defun rcirc-groups:catchup-all-conversations ()
  "catchup all conversation reinits all conversation-alist entries"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (rcirc-groups:catchup-conversation)
      (forward-line 1))))

(defun rcirc-groups:list-conversations ()
  "list all conversations where some notification has not yet been acknowledged"
  (let ((groups-buffer (get-buffer rcirc-groups:buffer-name)))
    (unless groups-buffer (rcirc-groups:create-notify-buffer))

    (with-current-buffer groups-buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(setq header-line-format 
	      (format "                     Last refresh was at %s" 
		      (format-time-string "%T")))

	(dolist (elt rcirc-groups:conversation-alist)
	  (when (and (not (rcirc-groups:conversation-has-been-killed elt))
		     (or rcirc-groups:display-all (> (cadr elt) 0)))
	    (insert (concat (rcirc-groups:format-conversation elt) "\n"))))))))

(defun rcirc-groups:quit-window ()
  "clean the header then quit the window"
  (interactive)
  (setq header-line-format nil)
  (quit-window))

(defun rcirc-groups:list-mentionned-conversations ()
  "list all conversations where some notification has not yet been acknowledged"
  (interactive)
  (setq rcirc-groups:display-all nil)
  (rcirc-groups:list-conversations))

(defun rcirc-groups:list-all-conversations ()
  "list all conversations where some notification has not yet been acknowledged"
  (interactive)
  (setq rcirc-groups:display-all t)
  (rcirc-groups:list-conversations))

(defun rcirc-groups:privmsg (proc sender response target text)
  "update the rcirc-groups:conversation-alist counters"
  (interactive)
  (when (and (string= response "PRIVMSG")
             (string= sender target)
             (not (rcirc-channel-p target)))

    (rcirc-groups:update-conversation-alist (current-buffer))
    (rcirc-groups:refresh-conversation-alist)))

(defun rcirc-groups:notify-me (proc sender response target text)
  "update the rcirc-groups:conversation-alist counters"
  (interactive)
  (when (and (string-match (concat (rcirc-nick proc) "[:, $]") text)
	     (not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender)))

    (rcirc-groups:update-conversation-alist (current-buffer))
    (rcirc-groups:refresh-conversation-alist)))

(defun rcirc-groups:create-notify-buffer ()
  "Create the rcirc-groups:buffer-name buffer in read-only"
  (let ((groups-buffer (get-buffer-create rcirc-groups:buffer-name)))
    (with-current-buffer groups-buffer
      (setq buffer-read-only t)
      ;; ensure we're in the rcirc-groups major mode -- we could just
      ;; be creating the buffer.
      (unless (eq major-mode 'rcirc-groups-mode)
	(rcirc-groups-mode)))
    groups-buffer))

(defun rcirc-groups:switch-to-groups-buffer ()
  "switch to the groups buffer"
  (interactive)
  (let ((groups-buffer (rcirc-groups:create-notify-buffer)))
    (rcirc-groups:refresh-conversation-alist)
    (set-window-buffer (selected-window) groups-buffer)))

(add-hook 'rcirc-print-hooks 'rcirc-groups:privmsg)
(add-hook 'rcirc-print-hooks 'rcirc-groups:notify-me)
(define-key rcirc-mode-map (kbd "C-c g") 'rcirc-groups:switch-to-groups-buffer)

(provide 'rcirc-groups)
