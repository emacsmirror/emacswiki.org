;;; enotify.el --- a TCP based notification system for the emacs modeline

;; Copyright (C) 2012  Alessandro Piras

;; Author: Alessandro Piras <laynor@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/laynor/enotify
;; Version: 0.1.2
;; Package-Requires: ()

;; This file is not part of GNU Emacs.

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

;; This package provides a notification area on the emacs modeline.
;; External programs can send notifications via TCP, using a simple
;; sexp based protocol. See the README.md file for details.
;; To use it, just add the enotify directory to your load-path, or
;; enotify.el if you are using the single file release, and then
;;
;; (require 'enotify)
;; (enotify-minor-mode 1)
;;
;; If you plan to run more emacs sessions, you will probably get
;; in trouble as the port used by enotify is already in use by
;; another emacs sessions.
;; You can wrap (enotify-minor-mode 1) in a condition-case form like
;; this one
;;
;; (condition-case err
;;     (enotify-minor-mode 1)
;;   (error (display-warning 'enotify
;; 			  (format "Cannot start Enotify: %s" err)
;; 			  :error)))
;;
;;; Code:

(eval-when-compile (require 'cl))
(defgroup enotify nil
  "Display notifications on emacs' mode line."
  :group 'modeline)
(eval-and-compile (provide 'enotify-group))

(defvar enotify-minor-mode) ;; Silencing compiler

(defcustom enotify-debug nil
  "Enable debug messages."
  :group 'enotify)
;;;;
;;;; Enotify Message passing
;;;;

;;; Message Buffer Table
;;; Each opened connection has its own message buffer.
;;; Associations between client connections and buffers
;;; are stored in a hash

;; enotify connection --> message buffer table
(defvar enotify-mp-cmbt (make-hash-table :test 'equal))

;; allocate a buffer for a connection
(defun enotify-mp-allocate-buffer (connection)
  (puthash connection (get-buffer-create (format " Enotify-msg-buffer:%S" connection))
	   enotify-mp-cmbt))

(defvar enotify-mp-idle-time 60
  "Idle time before calling enotify-mp-clean-garbage (internal
message buffer cleaning for dead connections")

(defun enotify-mp-reinit ()
  (clrhash enotify-mp-cmbt))

;;; get the buffer associated to a client connection
(defun enotify-mp-buffer (connection)
  (or (gethash connection enotify-mp-cmbt)
      (enotify-mp-allocate-buffer connection)))

;;; Store DATA in the buffer associated to CONNECTION
(defun enotify-mp-store-data (connection data)
  (let ((buffer (enotify-mp-buffer connection)))
    (save-current-buffer 
      (set-buffer buffer)
      (goto-char (point-max))
      (insert data))))

;;; Get a message from the connection (if present)
;; regex matching message size
(defvar enotify-mp-size-regex "|\\([[:digit:]]+\\)|")
(defun enotify-mp-get-message (connection)
  (let ((buf (if (bufferp connection) connection (enotify-mp-buffer connection))))
    (save-current-buffer
      (set-buffer buf)
      (goto-char (point-min))
      (let ((msg-start (re-search-forward enotify-mp-size-regex nil t)))
	(when msg-start
	  (let ((header-len (length (match-string 0)))
		(len (string-to-number (match-string 1))))
	    (delete-region 1 (- msg-start header-len))
	    (when (>= (- (point-max) header-len) len)
	      (let ((msg (buffer-substring (1+ header-len) (+ header-len 1 len))))
		(delete-region 1 (min (point-max) (+ header-len 1 len)))
		msg))))))))

;;; Utility function to print the internal buffers used for message
;;; passing
(defun enotify-mp-lscb ()
  (maphash (lambda (k v)
	     (print (buffer-name v)))
	   enotify-mp-cmbt)
  nil)
(defun enotify-mp-lsib ()
  (delq nil
   (mapcar (lambda (b)
	     (let ((bname (buffer-name b))
		   (pattern " Enotify-msg-buffer:"))
	       (when (and (>= (length bname) (length pattern))
			  (string= (substring bname 0 (length pattern))
				   pattern))
		 bname)))
	   (buffer-list))))
		
(defvar enotify-mp-cgrbg-count 0)
;;; Dead connection cleaning
(defun enotify-mp-clean-garbage ()
  "Calls delete-process for all the dead connections to the
Enotify server and kills all the related buffers."
  (when enotify-debug
    (message "Calling enotify-mp-clean-garbage %S" enotify-mp-cgrbg-count)
    (setq enotify-mp-cgrbg-count (1+ enotify-mp-cgrbg-count)))
  (let (dead-connections)
    (maphash (lambda (conn buff)
	       (when (memq (process-status conn) '(closed failed))
		 (kill-buffer buff)
		 (delete-process conn)
		 (push conn dead-connections)))
	     enotify-mp-cmbt)
    (dolist (dc dead-connections)
      (remhash dc enotify-mp-cmbt))))

(eval-and-compile (provide 'enotify-messages))
;;;; Enotify mode

(defvar enotify-mode-line-string nil
  "String to display in the mode line.")

(put 'enotify-mode-line-string 'risky-local-variable t)

(defcustom enotify-mode-line-prefix "[ "
  "Text to display before the notification text in the mode-line."
  :type 'string
  :group 'enotify)

(defcustom enotify-mode-line-suffix " ]"
  "Text to display after the notification text in the mode-line."
  :type 'string
  :group 'enotify)

(defconst enotify-success-face 'enotify-success-face
  "face to fontify Enotify Success messages")

(defface enotify-success-face
  '((((class color))
     (:foreground "#228833" :weight bold))
    (t (:weight bold)))
  "face to fontify Enotify Success messages"
  :group 'enotify)

(defconst enotify-failure-face 'enotify-failure-face
  "face to fontify Enotify Failure messages")

(defface enotify-failure-face
  '((((class color))
     (:foreground "red" :weight bold))
    (t (:weight bold)))
  "face to fontify Enotify Failure messages"
  :group 'enotify)

(defconst enotify-warning-face 'enotify-failure-face
  "face to fontify Enotify Failure messages")

(defface enotify-warning-face
  '((((class color))
     (:foreground "goldenrod4" :weight bold))
    (t (:weight bold)))
  "face to fontify Enotify Warning messages"
  :group 'enotify)

(defconst enotify-normal-face nil
  "face to notify Enotify Standard messages")

(defvar enotify-faces-alist (copy-sequence `((:standard . nil)
					     (:success . ,enotify-success-face)
					     (:warning . ,enotify-warning-face)
					     (:failure . ,enotify-failure-face))))

(defun enotify-face (face)
  (or (cdr (assoc face enotify-faces-alist))
      face
      enotify-normal-face))
;;; Notification format:
;;; (:text <message>
;;;  :face :warning|:normal|:failure|:success|face
;;;  :mouse-1 <click-handler>
;;;  :help <tooltip text>)
(defvar enotify-mode-line-notifications-table (make-hash-table :test 'equal)
  "Contains the associations between notification \"icons\" and slot
ids.")

(defun enotify-mode-line-notification (slot-id)
  (gethash slot-id enotify-mode-line-notifications-table))

(defvar enotify-mode-line-notifications-separator
  (propertize " | " 'face enotify-normal-face))

(defun enotify-event->text (event)
  "Returns the text object associated to the mouse click event EVENT."
  (car (nth 4 (nth 1 event))))

(defun enotify-icon->slot-id (icon-text)
  "Returns the slot id for ICON-TEXT."
  (get-text-property 0 'slot-id icon-text))

(defun enotify-event->slot-id (mouse-click-event)
  "Returns the slot id of the icon clicked."
  (enotify-icon->slot-id (enotify-event->text mouse-click-event)))

(defun enotify-list-inject (list separator)
  "Returns a new list whose elements are the same of LIST but
interlaced with SEPARATOR."
  (let ((res))
    (mapc (lambda (el)
	    (push separator res)
	    (push el res))
	  list)
    (cdr (reverse res))))


(defun enotify-delete-slot-handler (event)
  "Enotify Mouse event handler that removes a notification icon."
  (interactive "e")
  (enotify-mode-line-remove-notification (enotify-event->slot-id event)))

(defun enotify-delete-slot (slot-id)
  (enotify-mode-line-remove-notification slot-id))

(defvar enotify-popup-menu
  (easy-menu-create-menu "Enotify" '(["Delete Slot" (lambda (event) (interactive "e")
						      (enotify-delete-slot (enotify-event->slot-id event)))]))
  "Menu keymap for the predefined enotify popup menu.")

;; TODO: aggiungere handler per il bottone destro del mouse
(defun enotify-propertize-notification (slot-id notification)
  "Returns a properly propertized text object given SLOT-ID and
  NOTIFICATION.
NOTIFICATION has to be specified in this format:
  (:text <message>
   :face :warning|:normal|:failure|:success|face
   :mouse-1 <click-handler>
   :help <tooltip text>)
the tooltip text should also contain the help text for mouse-1.
The mouse-1 handler should be an (interactive \"e\") command. The
slot-id of the icon clicked can be retrieved using
`enotify-event->slot-id'."
  (destructuring-bind (&key text face mouse-1 help)
      notification
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line mouse-1] mouse-1)
      (define-key map [mode-line mouse-2] 'enotify-delete-slot-handler)
      (define-key map [mode-line C-mouse-1] (lambda (event)
					      (interactive "e")
					      (popup-menu enotify-popup-menu event event))) 
      (propertize text
		  'face (enotify-face face)
		  'help-echo (concat (format "[ %s ]\n" slot-id) help
				     "\nmouse-2: remove notification icon.\nC-mouse-1: Enotify popup menu")
		  'mouse-face (enotify-face face)
		  'slot-id slot-id
		  'local-map map))))
  
(defun enotify-mode-line-notifications-list ()
  "Returns a list with the notifications properly sorted and `propertize'd."
  (let (res)
    (maphash (lambda (slot-id notification)
	       (push (enotify-propertize-notification slot-id notification)
		     res))
	     enotify-mode-line-notifications-table)
    (reverse res)))

(defun enotify-mode-line-update ()
  "Updates the Enotify notification area."
  (interactive)
  (let ((res nil))
    (setq enotify-mode-line-string
	  (append 
	   (list enotify-mode-line-prefix)
	   (enotify-list-inject (enotify-mode-line-notifications-list)
				enotify-mode-line-notifications-separator)
	   (list enotify-mode-line-suffix)))))

(defun enotify-mode-line-update-notification (slot-id notification &optional pos)
  "Updates the notification \"icon\" associated with SLOT-ID to
  NOTIFICATION."
  (puthash slot-id notification enotify-mode-line-notifications-table)
  (enotify-mode-line-update))

(defun enotify-mode-line-remove-notification (slot-id)
  "Removes the notification \"icon\" associated with SLOT-ID from the notification area."
  (remhash slot-id enotify-mode-line-notifications-table)
  (enotify-mode-line-update)
  (force-mode-line-update))


(eval-and-compile (provide 'enotify-mode-line))

;;; enotify-mode-line.el ends here
(require 'enotify-mode-line)
(require 'enotify-messages)

(defcustom enotify-port 5000
  "TCP port used for client notifications"
  :group 'enotify)

(defconst enotify-process-name "Enotify")

(defvar enotify-connection nil
  "Network connection/process of the enotify server")

(defun enotify-start-server ()
  "Starts the Enotify notification service"
  (setq enotify-connection (make-network-process :name enotify-process-name
						 :server t
						 :family 'ipv4
						 :service enotify-port
						 :filter 'enotify-message-filter)))

;;; Notification slot registration
;;; Slot identification:
;;; - named slot
;;; - client ip+port
;;; Filter function alist:
;;;  Each cons cell is composed by:
;;;  (identification . filter-function)
;;;  identification can be:
;;;  slot-name
;;;  ip-address:port


(defvar enotify-message-handlers-table (make-hash-table :test 'equal))

(defun enotify-register-network-slot (id message-handler)
  "Registers a slot identified by ID, handling the messages with MESSAGE-HANDLER"
    (if (functionp message-handler)
	(puthash id message-handler enotify-message-handlers-table)
      (error "Enotify: invalid slot message handler for slot-id %S" id)))


(defun enotify-hash-has-key? (key table)
  (or (gethash key table)
      (not (gethash key table t))))

(defun enotify-slot-registered? (slot)
  (enotify-hash-has-key? slot enotify-message-handlers-table))

(defun enotify-connection-id (network-connection)
  "Returns the slot id for NETWORK-CONNECTION.
This id can be used if the message has :id :connection"
  (let ((string (format "%S" network-connection)))
    (string-match "^#<.*<\\(.*\\)>>$"
		  string)
    (match-string 1 string)))

(defun enotify-message-handler (slot-id)
  "Returns the message handler associated with SLOT-ID."
  (gethash slot-id enotify-message-handlers-table))

(defun enotify-slot-id (network-connection message-id)
  "Returns the SLOT-ID that matches CONNECTION and MESSAGE-ID"
  (if (eql message-id :connection)
      (enotify-connection-id network-connection)
    message-id))


(defun enotify-message-filter (network-connection msg)
  "Dispatches the incoming message MSG from NETWORK-CONNECTION to the
right message handler."
  (enotify-mp-store-data network-connection msg)
  (let ((msg-data (enotify-mp-get-message network-connection)))
    (when msg-data
      (let ((message (car (read-from-string msg-data))))
	(condition-case err 
	    (destructuring-bind (&key id notification data register handler-fn)
		message
	      (cond ((not (null register)) ; Registration message
		     (enotify-register-network-slot
		      register
		      (or handler-fn
			  (lambda (id data)
			    (message "Ignored data: %S" data)))))

		    ((and id notification data) ; Notification message
		     (let* ((message-handler (enotify-message-handler
					      (enotify-slot-id network-connection id))))
		       (cond (message-handler  ; Slot or connection registered
			      (enotify-mode-line-update-notification id notification)
			      (funcall message-handler id data))
			     ;; Unregistered clients
			     (t (message "Enotify: Unregistered client %S: %S"
					 (enotify-connection-id network-connection)
					 message)))))
		    ;; Invalid message format
		    (t (error "some arguments were not specified correctly"))))
	  ;; Error in message format
	  (error (error "Enotify: Bad message from <%s>:: %S -> %S"
			(enotify-connection-id network-connection)
			message err)))))))
	    
(defun enotify-unregister-network-slot (slot-id)
  "Unregisters the slot identified by SLOT-ID."
  (remhash slot-id enotify-message-handlers-table))

;;; FIXME: close connections?
(defun enotify-init-network ()
  (enotify-start-server)
  (clrhash enotify-message-handlers-table))

(eval-and-compile (provide 'enotify-network))
(require 'enotify-group)
(require 'enotify-mode-line)
(require 'enotify-network)

(defvar enotify-idle-timer nil)
;;;###autoload
(define-minor-mode enotify-minor-mode
  "Toggle display of notifications in the mode line."
  :global t :group 'enotify
  (setq enotify-mode-line-string nil)
  (or global-mode-string (setq global-mode-string (list "")))
  (cond ((not enotify-minor-mode)
	 (setq global-mode-string
	       (delq 'enotify-mode-line-string global-mode-string))
	 (when (timerp enotify-idle-timer)
	   (cancel-timer enotify-idle-timer)
	   (setq enotify-idle-timer nil))
	 (delete-process enotify-connection))
	(t (add-to-list 'global-mode-string 'enotify-mode-line-string t)
	   (enotify-init-network)
	   (setq enotify-idle-timer
		 (run-with-timer enotify-mp-idle-time enotify-mp-idle-time
				 'enotify-mp-clean-garbage-timer))
	   (enotify-mode-line-update))))

(defun enotify-version ()
  (interactive)
  (message "0.1.2"))

(provide 'enotify)
;;; enotify.el ends here
