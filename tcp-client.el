;;; tcp-client.el -- Providing a network interface
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: tcp network connection client
;; Description: Provide a tcp client interface 
;; Tested with: GNU Emacs 21.x and GNU Emacs 22.x
;;
;; This file is *NOT* part of GNU Emacs.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The purpose of this library is to provide a network interface to ease the
;; creation of network related script.
;; 
;; This library is dependending on record-type.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Here is an example of how to use it: 
;;
;;  (defun sample-error-report(buffer server port error)
;;    (save-excursion
;;      (set-buffer buffer)
;;      (insert (format "[error] %s:%i -- %s\n" server port error))))
;;  
;;  (defun sample-connection-report(buffer server port)
;;    (save-excursion
;;      (set-buffer buffer)
;;      (insert (format "[connect] %s:%i -- Connection established\n" server port))))
;;  
;;  (defun sample-abort-report(buffer server port)
;;    (save-excursion
;;      (set-buffer buffer)
;;      (insert (format "[error] %s:%i -- Abort connection\n" server port))))
;;  
;;  (defun sample-sentinel-report(process event)
;;    (save-excursion
;;      (set-buffer (process-buffer process))
;;      (insert (format "[event] Process: %s had the event -- %s" process event))))
;;  
;;  (defun sample-filter-report(process message)
;;    (save-excursion
;;      (set-buffer (process-buffer process))
;;      (insert (format "[got] %s" message))))
;;  
;;  
;;  (tcp-connect "*ok*" 
;;  	     (make-new-tcp-connection :server "127.0.0.1" :port 1000)
;;  	     (make-new-tcp-hooks 
;;  			      :connection-failed-handler 'sample-error-report
;;  			      :connection-established-handler 'sample-connection-report
;;  			      :connection-abort-handler 'sample-abort-report))
;;  
;;  (tcp-connect "*test*" 
;;  	     (make-new-tcp-connection :server "127.0.0.1" :port 2000)
;;  	     (make-new-tcp-hooks 
;;  			      :connection-failed-handler 'sample-error-report
;;  			      :connection-established-handler 'sample-connection-report
;;  			      :connection-abort-handler 'sample-abort-report
;;  			      :sentinel-handler 'sample-sentinel-report
;;  			      :filter-handler 'sample-filter-report))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'record-type)


(defrecord tcp-hooks
  "TCP-Client Handlers"
  :connection-established-handler 'functionp ;; (<call> buffer server port)
  :connection-failed-handler      'functionp ;; (<call> buffer server port message)
  :connection-abort-handler       'functionp ;; (<call> buffer server port)
  :sentinel-handler               'functionp ;; (<call> process event) 
  :filter-handler                 'functionp ;; (<call> process received-data)
)

(defrecord tcp-connection
  "Handle a tcp connection"
  :server 'stringp
  :port 'integerp)

(defun tcp-connect (buffer-name connection hooks)
  "Try to established a connection on a specific server

BUFFER-NAME refers to the name of the current buffer to handle the connection.
If the buffer exist, it's going to be displayed, if not, it's first going to be
created and then displayed.

CONNECTION is a record of the type tcp-connection which should contain 
the connection information.

HOOKS is also a record whose type is: tcp-hooks. It contains all different handlers 
you may want to set to intercept the connection data."
  (cond ((not (stringp          buffer-name))          (error "tcp-connect error: Invalid type -- BUFFER-NAME must be a string."))
	((not (tcp-connection-p connection))           (error "tcp-connect error: Invalid type -- CONNECTION's type must be: tcp-connection"))
	((not (tcp-hooks-p      hooks))                (error "tcp-connect error: Invalid type -- HOOKS's type must be: tcp-hooks"))
	((null (get-tcp-connection-server connection)) (error "tcp-connect error: Invalid server name (nil)"))
	((null (get-tcp-connection-port   connection)) (error "tcp-connect error: Invalid server port (nil)"))
	(t (let* ((buffer             (get-buffer-create buffer-name))
		  (process            (get-buffer-process buffer))
		  (server             (get-tcp-connection-server connection))
		  (port               (get-tcp-connection-port   connection))
		  (proc-name          (format "tcp-connection:%s:%i" server port))
		  (abort-handler      (get-tcp-hooks-connection-abort-handler hooks))
		  (error-handler      (get-tcp-hooks-connection-failed-handler hooks))
		  (connection-handler (get-tcp-hooks-connection-established-handler hooks))
		  (sentinel-handler   (get-tcp-hooks-sentinel-handler hooks))
		  (filter-handler     (get-tcp-hooks-filter-handler hooks)))
	     (progn (display-buffer buffer)
		    (sit-for 0) ;; force redisplay
		    (if process
			(progn (delete-process process)
			       (setq process 0)))
		    (condition-case data
			(setq process (open-network-stream proc-name buffer server port))
			(quit (and abort-handler
				   (funcall abort-handler buffer server port)))
			(file-error (cond ((string= (cadr data) "connection failed")
					(and error-handler
						(funcall error-handler buffer server port (caddr data))))
					  ((string= (cadr data) "make client process failed")
					   (and error-handler
						(funcall error-handler buffer server port (caddr data))))
					  (t (signal (car data) (cdr data)))))
			(error (if (and (stringp (cadr data))
					(string-match "^Unknown host" (cadr data)))
					(and error-handler
					(funcall error-handler buffer server port (cadr data)))
				 (apply 'error data))))
		    (if process 
			(progn (if sentinel-handler
				   (set-process-sentinel process sentinel-handler))
			       (if filter-handler
				   (set-process-filter process filter-handler))
			       (if (get-tcp-connection-keep-alive connection)
				   (set-network-process-option process :keepalive t))
			       (set-process-buffer process buffer)
		    (and connection-handler
			 (funcall connection-handler buffer server port))
			       )))))))


(defun tcp-send(process data)
  "Send DATA to the connection using the process PROCESS"
  (process-send-string process data))

(defun tcp-kill(process)
  "Kill the network connection, killing the process PROCESS"
  (delete-process process))

;(defun tcp-default-keep-alive()
;  (featurep 'make-network-process '(:keepalive t)))


(provide 'tcp-client)


