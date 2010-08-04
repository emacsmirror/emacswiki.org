;;; cruisecontrol.el --- Talking to a cruisecontrol server with emacs
(defconst cruisecontrol-version "0.2")
;; Copyright (c)2008 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
;; Author: Jonathan Arkell <jonnay@jonnay.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;;; Commentary:
;;; Installation:
;; Load this file in your .emacs with  (load-file "cruiecontrol.el")
;; and then customize it with
;; M-x customize-group <RET> cruise-control <RET>
;; In order not to give up on the current JMX control, which provides a compmlete list, a switch, '-xmlport' can be added to the command line interface.
;;; interactive commands: 
;; cruisecontrol-build - Launch a cruisecontrol build.  Promps for project name

;;; TODO:
;; - Get a list of projects ->  http://server.tld:port/getattribute?objectname=CruiseControl%20Manager:id=unique&attribute=Projects&format=collection&template=viewcollection
;; - Poll status and todochiku (or message) when a build is finished
;; 

;;; CHANGELOG:
;; v 0.2   - Added provide form.
;;         - Split the webservice component into getting and invoking
;; v 0.1   - Initial version 
;;; BUGS:


(defgroup cruisecontrol
  '()
  "Cruisecontrol commands for emacs.")

(defcustom cruisecontrol-webserver
  ""
  "Webserver (host and port) which accepts commands."
  :type 'string
  :group 'cruisecontrol)

(defcustom cruisecontrol-post-build-hooks
  '()
  "Functions to execute when a cruisectrol build has been launched."
  :type 'hook
  :group 'cruisecontrol)

(defcustom cruisecontrol-projects
  '()
  "List of Cruise Control Projects.  Not currently in use."
  :type 'list
  :group 'cruisecontrol)

(defcustom cruisecontrol-debug
  't
  "Debug the cruisecontrol interface?"
  :type 'boolean
  :group 'cruisecontrol)

(defun cruisecontrol-get-attribute-url (object attribute format template)
  "Retrieve tha attributes on the cruisecontrol server."
  (let* ((url
		  (concat "http://"
				  cruisecontrol-webserver
				  "/getattribute?"
				  "objectname=" (url-hexify-string object)
				  "&"
				  "attribute=" (url-hexify-string attribute)
				  "&"
				  "format=" (url-hexify-string format)
				  "&"
				  "template=" (url-hexify-string template))))
	(when cruisecontrol-debug
		  (message "CruiseControl Attribute URL is: %s" url))
	url))

(defun cruisecontrol-get-invoke-url (operation object)
  "Return tthe url to execute the operation with object on the cruisecontrol server."
  (let ((url
		 (concat "http://"
				 cruisecontrol-webserver
				 "/invoke?"
				 "operation=" (url-hexify-string operation)
				 "&"
				 "objectname=" (url-hexify-string object))))
	(when cruisecontrol-debug
		  (message "cruisecontrol operation url: %s" url))
	url))

(defun cruisecontrol-get-build-url (project)
  (cruisecontrol-get-invoke-url "build"
						 (concat "CruiseControl Project:name=" project)))

(defvar cruisecontrol-url-buffer 'nil)

(defun cruisecontrol-get-projects ()
  (interactive)
  (let ((result nil))
	(save-window-excursion
	  (set-buffer (url-retrieve-synchronously (cruisecontrol-get-attribute-url "CruiseControl Manager:id=unique" "Projects" "collection" "viewcollection")))
	  (goto-char (point-min))
	  (setq result (buffer-string))
	  (kill-buffer (current-buffer))
	  (message "Result is: %S" result))))

(defun cruisecontrol-build (project)
  "Launches a cruisecontrol build."
  (interactive "sCruise Control Project To Launch: ")
  (setq cruisecontrol-url-buffer (url-retrieve (cruisecontrol-get-build-url project) 'cruisecontrol-build-executed)))

(defun cruisecontrol-build-executed (status)
  (run-hooks 'cruisecontrol-post-build-hooks))

(if (featurep 'todochiku)
	(add-hook 'cruisecontrol-post-build-hooks
			  (lambda ()
				(todochiku-message "Cruise Control"
								   "build Started."
								   (todochiku-icon 'compile)))))

(provide 'cruisecontrol)
