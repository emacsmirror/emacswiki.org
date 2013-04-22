;;; tcp-server.el --- an emacs tcp server

;; Author: frédéric frances. http://www.emacswiki.org/emacs/FredericFrances
;; Keywords: tcp-server

;;; Commentary:
;; This was first inspired by Emacs EchoServer on emacs wiki (I don't
;; know who wrote this page, but many thanks to illustrate simply how
;; to use make-network-process).
;;
;; This is intended to be used for something more complex than echo
;; protocol.  I've used it to handle some text protocol and used emacs
;; as a server simulator to test some clients connection either in
;; text or binary mode.
;;

;; Customize tcp-server-list to create server and associate it to a
;; filter, this filter is used to handle message from incoming client
;; and send back answer to them using tcp-server-send.
 
;; toggle-tcp-server-trace allow activate trace event in *tcp-server* buffer.
;; toggle-tcp-server-list-processes list process when a client connect or disconect. 
;;

;;; History:
;; 

;;; Code:
(defgroup tcp-server nil
  "tcp server facilities"
  :group 'emacs
  :prefix "tcp-server")

(defconst tcp-server-name-index   0)
(defconst tcp-server-port-index   1)
(defconst tcp-server-filter-index 2)

(defvar tcp-server-list-processes-enabled  nil "Enable/disable call to list processes a some point.")
(defvar tcp-server-trace-enabled           nil "Enable/disable trace.")
(defvar tcp-server-trace-popup-enabled     nil "When t, automatically pop *tcp-server* buffer on event.")
(defvar tcp-server-sleep-after-send          5 "Sleep n seconds after each send.")
(defvar tcp-server-clients '()                 "Alist where KEY is a client processes and VALUE is the string.")
(defvar tcp-server-stop-on-disconnect nil)

(defun toggle-tcp-server-trace ()
  "Activate or desactivate trace in *tcp-server* buffer."
  (interactive)
  (setq tcp-server-trace-enabled (not tcp-server-trace-enabled))
  (if (eq t tcp-server-trace-enabled)
      (message "tcp-server traces enabled")
    (message "tcp-server traces disabled")))

(defun toggle-tcp-server-list-processes ()
  "Set `tcp-server-list-processes-enabled'.
When t popup list of process when a client connect.

This is annoying when using same Emacs instance for editing
code."
  (interactive)
  (setq tcp-server-list-processes-enabled (not tcp-server-list-processes-enabled))
  (if (eq t tcp-server-list-processes-enabled)
      (message "tcp-server list-processes enabled")
    (message "tcp-server list-processes disabled")))

(defun toggle-tcp-server-trace-popup ()
 "Set `tcp-server-trace-popup-enabled'.
When `tcp-server-trace-popup-enabled' is t,
switch to *tcp-server* buffer when a tcp event is received.
 
This is annoying when using same Emacs instance for editing code."
  (interactive)
  (setq tcp-server-trace-popup-enabled (not tcp-server-trace-popup-enabled))
  (if (eq t tcp-server-trace-popup-enabled)
      (message "tcp-server traces popup enabled")
    (message "tcp-server traces popup disabled")))

(defun tcp-server-trace (proc string)
  "Trace if a *tcp-server* buffer exists.
Argument PROC process.
Argument STRING message to trace."
  (cond ((eq t tcp-server-trace-enabled)
	 (get-buffer-create "*tcp-server*")
	 (if (eq t tcp-server-trace-popup-enabled)
	     (switch-to-buffer  "*tcp-server*"))
	 (if (get-buffer "*tcp-server*")
	     (with-current-buffer "*tcp-server*"
	       (goto-char (point-max))
	       (insert (current-time-string)
		       (format ":%s:" proc)
		       string)
	       (or (bolp) (newline)))))
	(t nil)))

(defcustom tcp-server-list
  nil
  "List of managed tcp servers.
Theses parameters are used to call `make-network-process' function,
sentinel and filter each represent callback function used by this function.

This call back should receive proc and message as argument.
An answer can be sent using `tcp-server-send' (simply use `tcp-server-send' to make an echo server)"
  :type '(repeat (list (string   :tag "Name  ")
		       (integer  :tag "Port  ")
		       (function :tag "Filter")))
  :group 'tcp-server)

(defun tcp-server-sentinel (proc msg)
  ".
Argument PROC process.
Argument MSG message."
  (let ((pname (process-name proc)))
    (cond ((get-process pname)
	   (tcp-server-trace "admin" (concat pname " is up:" msg))
	   (setq tcp-server-clients (cons proc tcp-server-clients))
	   (list-processes)
	   )
	  (t
	   (tcp-server-trace "admin" (concat pname " is down:" msg))
	   (setq tcp-server-clients (delq proc tcp-server-clients))
	   (list-processes))
	  );cond
    ))

(defun tcp-server-start-all-servers ()
  "Try to start all server described in `tcp-server-parameters'."
  (interactive)
  (mapc (lambda (server)
	  (let ((server-name   (nth tcp-server-name-index server))
		(server-port   (nth tcp-server-port-index server))
		(server-filter (nth tcp-server-filter-index server)))
	    (cond ((eq nil (get-process server-name))
		   (tcp-server-trace "admin" (format "starting server %s:%d using %s as callback" server-name server-port server-filter))
		   (make-network-process :name     server-name
					 :buffer   "*tcp-server*"
					 :family   'ipv4
					 :service  server-port
					 :sentinel 'tcp-server-sentinel
					 :filter   server-filter
					 :server   't))
		  (t (tcp-server-trace "admin" (format "A process having name %s already exist" server-name)))
		  ) ;; cond
	    ) ;; let
	  ) tcp-server-list) ;; mapc
  (list-processes))

(defun tcp-server-delete-client-process (proc)
  "Delete a tcp process.
Argument PROC process."
  (interactive "sprocess:")
  (tcp-server-trace "admin" (format "Stopping client %s" proc))
  (cond ((get-process proc)
	 (delete-process proc)
	 (setq tcp-server-clients (delq proc tcp-server-clients))
	 (tcp-server-trace "admin" (format "%s Stopped" proc)))
	(t
	 (setq tcp-server-clients (delq proc tcp-server-clients))
	 (tcp-server-trace "admin" (format "%s already stopped" proc))))
  )

(defun tcp-server-stop-all-servers nil
  "Disconnect all connected client, and stop all listening server."
  (interactive)
  (while tcp-server-clients
    (tcp-server-delete-client-process (car tcp-server-clients)))
  (mapc (lambda (server)
	  (let ((server-name (nth tcp-server-name-index server)))
	    (tcp-server-trace "admin" (format "Stopping %s" server-name))
	    (cond ((get-process server-name)
		   (delete-process server-name)
		   (tcp-server-trace "admin" (format "Stopped %s" server-name)))
		  (t (tcp-server-trace "admin" (format "%s already stopped" server-name)))
		  )
	    ) ;; let
	  ) ;; lambda
	tcp-server-list) ;; mapc
  (list-processes))

(defun tcp-server-send (proc message)
    "Send to connected PROC a MESSAGE."
  (tcp-server-trace proc (concat "S:" message))
  (process-send-string proc message)
  (when tcp-server-sleep-after-send
    (sleep-for tcp-server-sleep-after-send))
  )

(defun tcp-server-broadcast-now()
  "Send a message to all connected clients.
This function use `thing-at-point' on current line."
  (interactive)
  (mapc (lambda (proc)
          (message "Sending something to %s"  proc)
          (tcp-server-send proc (thing-at-point 'line)))
        tcp-server-clients))

(provide 'tcp-server)
;;; tcp-server.el ends here
