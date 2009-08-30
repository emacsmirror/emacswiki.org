;;; imdb.el --- 
;; This is free software.

;; Copyright (C) 2002 Girish Bharadwaj
;;
;; Author: girishb@mvps.org
;; Version: $Id: imdb.el,v 0.0 2002/03/26 21:56:02 gbharadwaj Exp $
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'imdb)

;;; Code:

(provide 'imdb)
(defvar imdb-mode-map nil
  "The keymap we use in imdb mode.")

(defvar imdb-temp-buffer "*imdb-temp*"
  "The name of the temporary buffer we use.")

(defvar imdb-buffer "*imdb*"
  "The name of the buffer the found headlines should be placed into.")

(defvar imdb-file "/Find"
  "The path for the post request")

(defvar imdb-host "us.imdb.com"
  "The hostname to use when connecting to imdb")

(defvar imdb-proxy nil 
  "The name of the firewall proxy, or nil if no proxy is required.")

(defvar imdb-proxyport 80
  "Port number for firewall proxy.")

(defvar imdb-request ""
  "Request formatted.")

(defvar input-data "")
(defvar input-count 0)
  
(if imdb-mode-map
    ()
  (if (fboundp 'widget-minor-mode-map)
      (setq imdb-mode-map (copy-keymap widget-minor-mode-map))
    (setq imdb-mode-map (copy-keymap widget-global-map))))

(define-key imdb-mode-map "q" 'bury-buffer)
(define-key imdb-mode-map " " 'scroll-up)

(defun imdb-encode(strcode)
  (replace-regexp-in-string " " "%%20" strcode)
)

(defun imdb(search-for)
  (interactive(list (read-string "Search For [title]: " )))
  (kill-buffer (get-buffer-create imdb-buffer))
  (setq search-for (imdb-encode search-for))
  (setq input-data (concat "select=title&for=" search-for))
  (message input-data)
  (imdb-get-results (get-buffer-create imdb-temp-buffer) imdb-host 80 imdb-file input-data)
)

(defun imdb-get-results(buf host port file input-data)
       (interactive)
         (let ((tcp-connection))
    (set-buffer buf)
    (if (null imdb-proxy)
	(setq file (concat "" file))
      (progn
	(setq file (concat "http://" imdb-host "/" file))
	(setq host imdb-proxy)
	(setq port imdb-proxyport)
      ))
    (message "asking for %s from %s at port:%s" file host port)
    (or
     (setq tcp-connection
	   (open-network-stream
	    "POST process-name"
	    buf
	    host
	    port
	    ))
     (error "Could not open connection to %s:%d" host port))
    (message "tcp-connection init done")
    (set-marker (process-mark tcp-connection) (point-min))
    (message "marker set")
    (set-process-sentinel tcp-connection 'imdb-sentinel)
    (message "Process-sentinel set")
    (setq imdb-request (concat "POST " file " HTTP/1.0\nUser-Agent:Emacs-W3/2.1.105 URL/1.267 ((win32?) ;GUI; windowsXP)\nContent-Type:application/x-www-form-urlencoded\n"))
    (message imdb-request)
    (setq input-count (length input-data))
    (setq imdb-request (concat imdb-request "Content-length:" (number-to-string input-count) "\n\n" input-data "\n"))
    (message imdb-request)
    (process-send-string tcp-connection imdb-request)))


(defun imdb-sentinel (process string)
  "Process the results from the imdb network connection.
process - The process object that is being notified.
string - The string that describes the notification."
  (let ((buffer (get-buffer-create imdb-buffer)))
    (set-buffer buffer)
    (erase-buffer)
    (goto-char 0)
    (insert "\n\tIMDB Results")
    (insert "\t")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (bury-buffer))
     		 "Bury")
    (insert "\n\n")
    (imdb-parse buffer)
    (pop-to-buffer buffer)
    (kill-all-local-variables)
    (widget-minor-mode 1)
    (setq major-mode 'imdb-mode
	  mode-name "IMDB")
    (use-local-map imdb-mode-map)
    (widget-setup)
    (goto-char 0)
;    (setq buffer-read-only t)
    (kill-buffer (get-buffer-create imdb-temp-buffer))))

(defun imdb-parse (buffer)
  "Parse the result of the IMDB temporary buffer.
BUFFER is the buffer where the beautified headlines should appear."
  (interactive)

  (let ((buf (get-buffer-create imdb-temp-buffer)))
    (set-buffer buf)
    (goto-char 0)
    (imdb-grab-link buffer)
    (replace-string "&lt;" "<")
    (goto-char 0)
    (replace-string "&gt;" ">")
    (goto-char 0)
    (replace-string "&amp;" "&")
    (goto-char 0)
    (while (imdb-parse-article buffer))))

(defun imdb-grab-headline-item (item)
  "Parse headline ITEM out of the current line of the current buffer."
  (re-search-forward (format "<%s>\\(.*\\)</%s>" item item))
  (match-string 1))

(defun imdb-grab-link (data)
  (search-forward "<A HREF")
  (match-beginning 0)
  (replace-string "<A HREF=\"" "<link>http://us.imdb.com")
  (goto-char 0)
 (replace-string "\">" "</link><title>")
 (goto-char 0)
  (replace-string "</A>" "</title>")
  (goto-char 0)
)


(defun imdb-parse-article (buffer)
  "Parse a single article in the imdb buffer.
BUFFER is the buffer where the beautified headlines should appear."
   (if (re-search-forward "<LI>" nil t)
       (progn
;	 (imdb-grab-link (buffer))
	 (let* ((link     (imdb-grab-headline-item "link"));(match-string 1))
		(title        (imdb-grab-headline-item "title")))
	   (message "Found %s" title)
	   (imdb-insert buffer title link)
	   t))
       nil)
     )


(defun imdb-insert (buffer title link)
  "Insert and article, and links into the buffer `buffer'.
BUFFER is the buffer where the beautified headlines should appear.
TITLE is the title of the article.
URL is the URL that the story lives at.
DESCRIPTION is a brief description of the story."
  (let ((current (current-buffer))
	(inhibit-read-only t))
    (set-buffer buffer)
    (insert "\n\t")
    (widget-create 'push-button
		   :url link
		   :notify (lambda (widget &rest ignore)
			     (let ((link (widget-get widget :url)))
			       (message "Viewing %s" link)
			       (browse-url link)))
		   title)
    (insert (concat "\n"))
    (set-buffer current)))


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################





;;; imdb.el ends here
