;;; identica-mode.el --- Major mode for Identica


;; Copyright (C) 2008 Gabriel Saldana

;; Author: Gabriel Saldana <gsaldana@gmail.com>
;; Last update: 2009-02-21
;; Version: 0.5
;; Keywords: identica web
;; URL: http://blog.nethazard.net/identica-mode-for-emacs/
;; Contributors:
;;     Jason McBrayer <jmcbray@carcosa.net> (minor updates for working under Emacs 23)
;;     Alex Schröder <kensanata@gmail.com> (mode map patches)
;;     Christian Cheng (fixed long standing xml parsing bug)
;;     Carlos A. Perilla from denting-mode

;; Identica Mode is a major mode to check friends timeline, and update your
;; status on Emacs.

;; identica-mode.el is a major mode for Identica.  Based on the twittering mode
;; version 0.6 by Y. Hayamizu and Tsuyoshi CHO found at
;; <http://hayamin.com/wiliki.cgi?twittering-mode-en&l=en>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth floor,
;; Boston, MA 02110-1301, USA.

;; Installation

;; Add the following to your .emacs or your prefered customizations file

;; (require 'identica-mode)
;; (setq identica-username "yourusername")
;; (setq identica-password "yourpassword")

;; If you want to post from the minibufer without having identica buffer active, add the following global keybinding.
;; (global-set-key "\C-cip" 'identica-update-status-interactive)

;; If you want to connect to a custom laconica server add this and change
;; identi.ca with your server's doman name.

;; (setq laconica-server "identi.ca")

;; Start using with M-x identica-mode

(require 'cl)
(require 'xml)
(require 'parse-time)

(defconst identica-mode-version "0.5")

(defun identica-mode-version ()
  "Display a message for identica-mode version."
  (interactive)
  (let ((version-string
	 (format "identica-mode-v%s" identica-mode-version)))
    (if (interactive-p)
	(message "%s" version-string)
      version-string)))

(defvar identica-mode-map (make-sparse-keymap))

(defvar identica-timer nil "Timer object for timeline refreshing will be stored here. DO NOT SET VALUE MANUALLY.")

(defvar identica-idle-time 20)

(defvar identica-timer-interval 90)

(defvar identica-username nil)

(defvar identica-password nil)

(defvar laconica-server "identi.ca")

(defvar identica-scroll-mode nil)
(make-variable-buffer-local 'identica-scroll-mode)

(defvar identica-jojo-mode nil)
(make-variable-buffer-local 'identica-jojo-mode)

(defvar identica-status-format nil)
(setq identica-status-format "%i %s,  %@:\n  %t // from %f%L")
;; %s - screen_name
;; %S - name
;; %i - profile_image
;; %d - description
;; %l - location
;; %L - " [location]"
;; %u - url
;; %j - user.id
;; %p - protected?
;; %c - created_at (raw UTC string)
;; %C{time-format-str} - created_at (formatted with time-format-str)
;; %@ - X seconds ago
;; %t - text
;; %' - truncated
;; %f - source
;; %# - id

(defvar identica-buffer "*identica*")
(defun identica-buffer ()
  (identica-get-or-generate-buffer identica-buffer))

(defvar identica-http-buffer "*identica-http-buffer*")
(defun identica-http-buffer ()
  (identica-get-or-generate-buffer identica-http-buffer))

(defvar identica-friends-timeline-data nil)
(defvar identica-friends-timeline-last-update nil)

(defvar identica-username-face 'identica-username-face)
(defvar identica-uri-face 'identica-uri-face)
(defvar identica-reply-face 'identica-reply-face)

(defun identica-get-or-generate-buffer (buffer)
  (if (bufferp buffer)
      (if (buffer-live-p buffer)
	  buffer
	(generate-new-buffer (buffer-name buffer)))
    (if (stringp buffer)
	(or (get-buffer buffer)
	    (generate-new-buffer buffer)))))

(defun assocref (item alist)
  (cdr (assoc item alist)))
(defmacro list-push (value listvar)
  `(setq ,listvar (cons ,value ,listvar)))

;;; Proxy
(defvar identica-proxy-use nil)
(defvar identica-proxy-server nil)
(defvar identica-proxy-port 8080)
(defvar identica-proxy-user nil)
(defvar identica-proxy-password nil)

(defun identica-toggle-proxy () ""
  (interactive)
  (setq identica-proxy-use
	(not identica-proxy-use))
  (message "%s %s"
	   "Use Proxy:"
	   (if identica-proxy-use
	       "on" "off")))

(defun identica-user-agent-default-function ()
  "Identica mode default User-Agent function."
  (concat "Emacs/"
	  (int-to-string emacs-major-version) "." (int-to-string
						   emacs-minor-version)
	  " "
	  "Identica-mode/"
	  identica-mode-version))

(defvar identica-user-agent-function 'identica-user-agent-default-function)

(defun identica-user-agent ()
  "Return User-Agent header string."
  (funcall identica-user-agent-function))

;;; to show image files

(defvar identica-wget-buffer "*identica-wget-buffer*")
(defun identica-wget-buffer ()
  (identica-get-or-generate-buffer identica-wget-buffer))

(defvar identica-tmp-dir
  (expand-file-name (concat "identicamode-images-" (user-login-name))
		    temporary-file-directory))

(defvar identica-icon-mode nil "You MUST NOT CHANGE this variable directory. You should change through function'identica-icon-mode'")
(make-variable-buffer-local 'identica-icon-mode)
(defun identica-icon-mode (&optional arg)
  (interactive)
  (setq identica-icon-mode
	(if identica-icon-mode
	    (if (null arg)
		nil
	      (> (prefix-numeric-value arg) 0))
	  (when (or (null arg)
		    (and arg (> (prefix-numeric-value arg) 0)))
	    (when (file-writable-p identica-tmp-dir)
	      (progn
		(if (not (file-directory-p identica-tmp-dir))
		    (make-directory identica-tmp-dir))
		t)))))
  (identica-render-friends-timeline))

(defun identica-scroll-mode (&optional arg)
  (interactive)
  (setq identica-scroll-mode
	(if (null arg)
	    (not identica-scroll-mode)
	  (> (prefix-numeric-value arg) 0))))

(defun identica-jojo-mode (&optional arg)
  (interactive)
  (setq identica-jojo-mode
	(if (null arg)
	    (not identica-jojo-mode)
	  (> (prefix-numeric-value arg) 0))))

(defvar identica-image-stack nil)

(defun identica-image-type (file-name)
  (cond
   ((string-match "\\.jpe?g" file-name) 'jpeg)
   ((string-match "\\.png" file-name) 'png)
   ((string-match "\\.gif" file-name) 'gif)
   (t nil)))

(defun identica-setftime (fmt string uni)
  (format-time-string fmt ; like "%Y-%m-%d %H:%M:%S"
		      (apply 'encode-time (parse-time-string string))
		      uni))
(defun identica-local-strftime (fmt string)
  (identica-setftime fmt string nil))
(defun identica-global-strftime (fmt string)
  (identica-setftime fmt string t))


(defvar identica-debug-mode nil)
(defvar identica-debug-buffer "*debug*")
(defun identica-debug-buffer ()
  (identica-get-or-generate-buffer identica-debug-buffer))
(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if identica-debug-mode
	   (with-current-buffer (identica-debug-buffer)
	     (insert (prin1-to-string ,obsym))
	     (newline)
	     ,obsym)
	 ,obsym))))

(defun identica-debug-mode ()
  (interactive)
  (setq identica-debug-mode
	(not identica-debug-mode))
  (message (if identica-debug-mode "debug mode:on" "debug mode:off")))

(if identica-mode-map
    (let ((km identica-mode-map))
      (define-key km "\C-c\C-f" 'identica-friends-timeline)
      (define-key km "\C-c\C-s" 'identica-update-status-interactive)
      (define-key km "\C-c\C-e" 'identica-erase-old-statuses)
      (define-key km "\C-m" 'identica-enter)
      (define-key km "\C-c\C-l" 'identica-update-lambda)
      (define-key km [mouse-1] 'identica-click)
      (define-key km "\C-c\C-v" 'identica-view-user-page)
      (define-key km "q" 'bury-buffer)
      (define-key km "j" 'identica-goto-next-status)
      (define-key km "k" 'identica-goto-previous-status)
      (define-key km "l" 'forward-char)
      (define-key km "h" 'backward-char)
      (define-key km "0" 'beginning-of-line)
      (define-key km "^" 'beginning-of-line-text)
      (define-key km "$" 'end-of-line)
      (define-key km "n" 'identica-goto-next-status-of-user)
      (define-key km "p" 'identica-goto-previous-status-of-user)
      (define-key km [backspace] 'backward-char)
      (define-key km "G" 'end-of-buffer)
      (define-key km "H" 'beginning-of-buffer)
      (define-key km "i" 'identica-icon-mode)
      (define-key km "s" 'identica-scroll-mode)
      (define-key km "t" 'identica-toggle-proxy)
      (define-key km "\C-c\C-p" 'identica-toggle-proxy)
      nil))

(defvar identica-mode-syntax-table nil "")

(if identica-mode-syntax-table
    ()
  (setq identica-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" identica-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  identica-mode-syntax-table)
  )

(defun identica-mode-init-variables ()
  ;; (make-variable-buffer-local 'variable)
  ;; (setq variable nil)
  (font-lock-mode -1)
  (defface identica-username-face
    `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-string-face 'identica-username-face)
  (set-face-attribute 'identica-username-face nil :underline t)
  (defface identica-reply-face
    `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-string-face 'identica-reply-face)
  (set-face-attribute 'identica-reply-face nil :foreground "white")
  (set-face-attribute 'identica-reply-face nil :background "DarkSlateGray")
  (defface identica-uri-face
    `((t nil)) "" :group 'faces)

  (set-face-attribute 'identica-uri-face nil :underline t)
  (add-to-list 'minor-mode-alist '(identica-icon-mode " id-icon"))
  (add-to-list 'minor-mode-alist '(identica-scroll-mode " id-scroll"))
  (add-to-list 'minor-mode-alist '(identica-jojo-mode " id-jojo"))
  )

(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key)) keylist))
		't)
	     ,@body)))
       clauses)))

;; If you use Emacs21, decode-char 'ucs will fail unless Mule-UCS is loaded.
;; TODO: Show error messages if Emacs 21 without Mule-UCS
(defmacro identica-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defvar identica-mode-string "Identica mode")

(defvar identica-mode-hook nil
  "Identica-mode hook.")

(defun identica-mode ()
  "Major mode for Identica
\\{identica-mode-map}"
  (interactive)
  (switch-to-buffer (identica-buffer))
  (kill-all-local-variables)
  (identica-mode-init-variables)
  (use-local-map identica-mode-map)
  (setq major-mode 'identica-mode)
  (setq mode-name identica-mode-string)
  (set-syntax-table identica-mode-syntax-table)
  (run-hooks 'identica-mode-hook)
  (font-lock-mode -1)
  (identica-start)
  )

;;;
;;; Basic HTTP functions
;;;

(defun identica-http-get (method-class method &optional parameters sentinel)
  (if (null sentinel) (setq sentinel 'identica-http-get-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (identica-http-buffer))
    (erase-buffer))

  (let (proc server port
	     (proxy-user identica-proxy-user)
	     (proxy-password identica-proxy-password))
    (condition-case nil
	(progn
	  (if (and identica-proxy-use identica-proxy-server)
	      (setq server identica-proxy-server
		    port (if (integerp identica-proxy-port)
			     (int-to-string identica-proxy-port)
			   identica-proxy-port))
	    (setq server laconica-server
		  port "80"))
	  (setq proc
		(open-network-stream
		 "network-connection-process" (identica-http-buffer)
		 server (string-to-number port)))
	  (set-process-sentinel proc sentinel)
	  (process-send-string
	   proc
	   (let ((nl "\r\n")
		 request)
	     (setq request
		   (concat "GET http://" laconica-server "/api/" method-class "/" method
			   ".xml?"
			   (when parameters
			     (concat "?"
				     (mapconcat
				      (lambda (param-pair)
					(format "%s=%s"
						(identica-percent-encode (car param-pair))
						(identica-percent-encode (cdr param-pair))))
				      parameters
				      "&")))
			   " HTTP/1.1" nl
			   (concat "Host: " laconica-server) nl
			   "User-Agent: " (identica-user-agent) nl
			   "Authorization: Basic "
			   (base64-encode-string
			    (concat identica-username ":" (identica-get-password)))
			   nl
			   "Accept: text/xml"
			   ",application/xml"
			   ",application/xhtml+xml"
			   ",application/html;q=0.9"
			   ",text/plain;q=0.8"
			   ",image/png,*/*;q=0.5" nl
			   "Accept-Charset: utf-8;q=0.7,*;q=0.7" nl
			   (when identica-proxy-use
			     "Proxy-Connection: Keep-Alive" nl
			     (when (and proxy-user proxy-password)
			       (concat
				"Proxy-Authorization: Basic "
				(base64-encode-string
				 (concat proxy-user ":"
					 proxy-password))
				nl)))
			   nl))
	     (debug-print (concat "GET Request\n" request))
	     request)))
      (error
       (message "Failure: HTTP GET") nil))))

(defun identica-http-get-default-sentinel (proc stat &optional suc-msg)
  (let ((header (identica-get-response-header))
	(body (identica-get-response-body))
	(status nil)
	)
    (if (string-match "HTTP/1\.[01] \\([A-Za-z0-9 ]+\\)\r?\n" header)
	(progn
	  (setq status (match-string-no-properties 1 header))
	  (case-string
	   status
	   (("200 OK")
	    (mapcar
	     #'identica-cache-status-datum
	     (reverse (identica-xmltree-to-status
		       body)))
	    (identica-render-friends-timeline)
	    (message (if suc-msg suc-msg "Success: Get.")))
	   (t (message status))))
      (message "Failure: Bad http response.")))
  )

(defun identica-render-friends-timeline ()
  (with-current-buffer (identica-buffer)
    (let ((point (point))
	  (end (point-max)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (status)
	      (insert (identica-format-status
		       status identica-status-format))
	      (fill-region-as-paragraph
	       (save-excursion (beginning-of-line) (point)) (point))
	      (insert "\n"))
	    identica-friends-timeline-data)
      (if identica-image-stack
	  (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if identica-scroll-mode (- (point-max) end) 0))))
    ))

(defun identica-format-status (status format-str)
  (flet ((attr (key)
	       (assocref key status))
	 (profile-image
	  ()
	  (let ((profile-image-url (attr 'user-profile-image-url))
		(icon-string "\n  "))
	    (if (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" profile-image-url)
		(let ((filename (match-string-no-properties 1 profile-image-url)))
		  ;; download icons if does not exist
		  (if (file-exists-p (concat identica-tmp-dir
					     "/" filename))
		      t
		    (add-to-list 'identica-image-stack profile-image-url))

		  (when (and icon-string identica-icon-mode)
		    (set-text-properties
		     1 2 `(display ,(create-image (concat identica-tmp-dir "/" filename)))
		     icon-string)
		    icon-string)
		  )))))
    (let ((cursor 0)
	  (result ())
	  c
	  found-at)
      (setq cursor 0)
      (setq result '())
      (while (setq found-at (string-match "%\\(C{\\([^}]+\\)}\\|[A-Za-z#@']\\)" format-str cursor))
	(setq c (string-to-char (match-string-no-properties 1 format-str)))
	(if (> found-at cursor)
	    (list-push (substring format-str cursor found-at) result)
	  "|")
	(setq cursor (match-end 1))

	(case c
	  ((?s)                         ; %s - screen_name
	   (list-push (attr 'user-screen-name) result))
	  ((?S)                         ; %S - name
	   (list-push (attr 'user-name) result))
	  ((?i)                         ; %i - profile_image
	   (list-push (profile-image) result))
	  ((?d)                         ; %d - description
	   (list-push (attr 'user-description) result))
	  ((?l)                         ; %l - location
	   (list-push (attr 'user-location) result))
	  ((?L)                         ; %L - " [location]"
	   (let ((location (attr 'user-location)))
	     (unless (or (null location) (string= "" location))
	       (list-push (concat " [" location "]") result)) ))
	  ((?u)                         ; %u - url
	   (list-push (attr 'user-url) result))
	  ((?j)                         ; %j - user.id
	   (list-push (format "%d" (attr 'user-id)) result))
	  ((?p)                         ; %p - protected?
	   (let ((protected (attr 'user-protected)))
	     (when (string= "true" protected)
	       (list-push "[x]" result))))
	  ((?c)                     ; %c - created_at (raw UTC string)
	   (list-push (attr 'created-at) result))
	  ((?C) ; %C{time-format-str} - created_at (formatted with time-format-str)
	   (list-push (identica-local-strftime
		       (or (match-string-no-properties 2 format-str) "%H:%M:%S")
		       (attr 'created-at))
		      result))
	  ((?@)                         ; %@ - X seconds ago
	   (let ((created-at
		  (apply
		   'encode-time
		   (parse-time-string (attr 'created-at))))
		 (now (current-time)))
	     (let ((secs (+ (* (- (car now) (car created-at)) 65536)
			    (- (cadr now) (cadr created-at))))
		   time-string url)
	       (setq time-string
		     (cond ((< secs 5) "less than 5 seconds ago")
			   ((< secs 10) "less than 10 seconds ago")
			   ((< secs 20) "less than 20 seconds ago")
			   ((< secs 30) "half a minute ago")
			   ((< secs 60) "less than a minute ago")
			   ((< secs 150) "1 minute ago")
			   ((< secs 2400) (format "%d minutes ago"
						  (/ (+ secs 30) 60)))
			   ((< secs 5400) "about 1 hour ago")
			   ((< secs 84600) (format "about %d hours ago"
						   (/ (+ secs 1800) 3600)))
			   (t (format-time-string "%I:%M %p %B %d, %Y" created-at))))
	       (setq url (identica-get-status-url (attr 'id)))
	       ;; make status url clickable
	       (add-text-properties
		0 (length time-string)
		`(mouse-face highlight
			     face identica-uri-face
			     uri ,url)
		time-string)
	       (list-push time-string result))))
	  ((?t)                         ; %t - text
	   (list-push                   ;(clickable-text)
	    (attr 'text)
	    result))
	  ((?')                         ; %' - truncated
	   (let ((truncated (attr 'truncated)))
	     (when (string= "true" truncated)
	       (list-push "..." result))))
	  ((?f)                         ; %f - source
	   (list-push (attr 'source) result))
	  ((?#)                         ; %# - id
	   (list-push (format "%d" (attr 'id)) result))
	  (t
	   (list-push (char-to-string c) result)))
	)
      (list-push (substring format-str cursor) result)
      (let ((formatted-status (apply 'concat (nreverse result))))
	(add-text-properties 0 (length formatted-status)
			     `(username ,(attr 'user-screen-name))
			     formatted-status)
	formatted-status)
      )))

(defun identica-http-post
  (method-class method &optional parameters contents sentinel)
  "Send HTTP POST request to laconica server

METHOD-CLASS must be one of Identica API method classes(statuses, users or direct_messages).
METHOD must be one of Identica API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters. ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (if (null sentinel) (setq sentinel 'identica-http-post-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (identica-http-buffer))
    (erase-buffer))

  (let (proc server port
	     (proxy-user identica-proxy-user)
	     (proxy-password identica-proxy-password))
    (progn
      (if (and identica-proxy-use identica-proxy-server)
	  (setq server identica-proxy-server
		port (if (integerp identica-proxy-port)
			 (int-to-string identica-proxy-port)
		       identica-proxy-port))
	(setq server laconica-server
	      port "80"))
      (setq proc
	    (open-network-stream
	     "network-connection-process" (identica-http-buffer)
	     server (string-to-number port)))
      (set-process-sentinel proc sentinel)
      (process-send-string
       proc
       (let ((nl "\r\n")
	     request)
	 (setq  request
		(concat "POST http://" laconica-server "/api/" method-class "/" method ".xml"
			(when parameters
			  (concat "?"
				  (mapconcat
				   (lambda (param-pair)
				     (format "%s=%s"
					     (identica-percent-encode (car param-pair))
					     (identica-percent-encode (cdr param-pair))))
				   parameters
				   "&")))
			" HTTP/1.1" nl
			(concat "Host: " laconica-server) nl
			"User-Agent: " (identica-user-agent) nl
			"Authorization: Basic "
			(base64-encode-string
			 (concat identica-username ":" (identica-get-password)))
			nl
			"Content-Type: text/plain" nl
			"Content-Length: 0" nl
			(when identica-proxy-use
			  "Proxy-Connection: Keep-Alive" nl
			  (when (and proxy-user proxy-password)
			    (concat
			     "Proxy-Authorization: Basic "
			     (base64-encode-string
			      (concat proxy-user ":"
				      proxy-password))
			     nl)))
			nl))
	 (debug-print (concat "POST Request\n" request))
	 request)))))

(defun identica-http-post-default-sentinel (proc stat &optional suc-msg)

  (condition-case err-signal
      (let ((header (identica-get-response-header))
	    ;; (body (identica-get-response-body)) not used now.
	    (status nil))
	(string-match "HTTP/1\.1 \\([a-z0-9 ]+\\)\r?\n" header)
	(setq status (match-string-no-properties 1 header))
	(case-string status
		     (("200 OK")
		      (message (if suc-msg suc-msg "Success: Post")))
		     (t (message status)))
	)
    (error (message (prin1-to-string err-signal))))
  )

(defun identica-get-response-header (&optional buffer)
  "Exract HTTP response header from HTTP response.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, the value of `identica-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (identica-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (substring  content 0 (string-match "\r?\n\r?\n" content)))))

(defun identica-get-response-body (&optional buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a XML tree as list.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, the value of `identica-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (identica-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (identica-clean-response-body)
    (let ((content (buffer-string)))
      (xml-parse-region (+ (string-match "<\\?xml" content)
			     (length (match-string 0 content)))
                        (point-max))
      )))

(defun identica-clean-weird-chars (&optional buffer)
;;(if (null buffer) (setq buffer (identica-http-buffer)))
(with-current-buffer (identica-http-buffer)
  (goto-char (point-min))
  (while (re-search-forward "\

?
[0-9a-z]*\

?
?" nil t)
(replace-match ""))
(buffer-string))
)

(defun identica-clean-response-body ()
  "Removes weird strings (e.g., 1afc, a or 0) from within the
response body.  Known Laconica issue.  Mostly harmless except if
in tags."
  (goto-char (point-min))
  (while (re-search-forward "\r?\n[0-9a-z]+\r?\n" nil t)
    (replace-match "")))

(defun identica-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default identica-friends-timeline-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'identica-friends-timeline-data))
  (let ((id (cdr (assq 'id status-datum))))
    (if (or (null (symbol-value data-var))
	    (not (find-if
		  (lambda (item)
		    (eql id (cdr (assq 'id item))))
		  (symbol-value data-var))))
	(progn
	  (if identica-jojo-mode
	      (identica-update-jojo (cdr (assq 'user-screen-name status-datum))
				      (cdr (assq 'text status-datum))))
	  (set data-var (cons status-datum (symbol-value data-var)))
	  t)
      nil)))

(defun identica-status-to-status-datum (status)
  (flet ((assq-get (item seq)
		   (car (cddr (assq item seq)))))
    (let* ((status-data (cddr status))
	   id text source created-at truncated
	   (user-data (cddr (assq 'user status-data)))
	   user-id user-name
	   user-screen-name
	   user-location
	   user-description
	   user-profile-image-url
	   user-url
	   user-protected
	   regex-index)

      (setq id (string-to-number (assq-get 'id status-data)))
      (setq text (identica-decode-html-entities
		  (assq-get 'text status-data)))
      (setq source (identica-decode-html-entities
		    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq user-id (string-to-number (assq-get 'id user-data)))
      (setq user-name (identica-decode-html-entities
		       (assq-get 'name user-data)))
      (setq user-screen-name (identica-decode-html-entities
			      (assq-get 'screen_name user-data)))
      (setq user-location (identica-decode-html-entities
			   (assq-get 'location user-data)))
      (setq user-description (identica-decode-html-entities
			      (assq-get 'description user-data)))
      (setq user-profile-image-url (assq-get 'profile_image_url user-data))
      (setq user-url (assq-get 'url user-data))
      (setq user-protected (assq-get 'protected user-data))

      ;; make username clickable
      (add-text-properties
       0 (length user-name)
       `(mouse-face highlight
		    uri ,(concat "http://" laconica-server "/" user-screen-name)
		    face identica-username-face)
       user-name)

      ;; make screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
		    face identica-username-face
		    uri ,(concat "http://" laconica-server "/" user-screen-name)
		    face identica-username-face)
       user-screen-name)

      ;; make URI clickable
      (setq regex-index 0)
      (while regex-index
	(setq regex-index
	      (string-match "@\\([_a-zA-Z0-9]+\\)\\|!\\([_a-zA-Z0-9\-]+\\)\\|#\\([_a-zA-Z0-9\-]+\\)\\|\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)"
;;	      (string-match "@\\([_a-zA-Z0-9]+\\)\\|\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)"
			    text
			    regex-index))
	(when regex-index
	  (let* ((matched-string (match-string-no-properties 0 text))
		 (screen-name (match-string-no-properties 1 text))
		 (group-name (match-string-no-properties 2 text))
		 (tag-name (match-string-no-properties 3 text))
		 (uri (match-string-no-properties 4 text)))
	    (add-text-properties
	     (if (or screen-name group-name tag-name)
		 (+ 1 (match-beginning 0))
	       (match-beginning 0))
	     (match-end 0)
	     (if (or screen-name group-name tag-name)
		 `(mouse-face
		   highlight
		   face identica-uri-face
		   uri ,(if screen-name
			    (concat "http://" laconica-server "/" screen-name)
			  (if group-name
			      (concat "http://" laconica-server "/group/" group-name)
			    (concat "http://" laconica-server "/tag/" tag-name)
			    )))
	       `(mouse-face highlight
			    face identica-uri-face
			    uri ,uri))
	     text))
	  (setq regex-index (match-end 0)) ))


      ;; make source pretty and clickable
      (if (string-match "<a href=\"\\(.*\\)\">\\(.*\\)</a>" source)
	  (let ((uri (match-string-no-properties 1 source))
		(caption (match-string-no-properties 2 source)))
	    (setq source caption)
	    (add-text-properties
	     0 (length source)
	     `(mouse-face highlight
			  face identica-uri-face
			  source ,source)
	     source)
	    ))

;; Last update Thu Oct  2 19:03:12 2008 Gabriel Saldana
      (setq identica-friends-timeline-last-update created-at)

      ;; highlight replies
      (if (string-match (concat "@" identica-username) text)
	  (add-text-properties 0 (length text)
			       `(face identica-reply-face) text))
      (mapcar
       (lambda (sym)
	 `(,sym . ,(symbol-value sym)))
       '(id text source created-at truncated
	    user-id user-name user-screen-name user-location
	    user-description
	    user-profile-image-url
	    user-url
	    user-protected)))))

(defun identica-xmltree-to-status (xmltree)
  (mapcar #'identica-status-to-status-datum
	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
	  ;; On Emacs22, there may be blank strings
	  (let ((ret nil) (statuses (reverse (cddr (car xmltree)))))
	    (while statuses
	      (if (consp (car statuses))
		  (setq ret (cons (car statuses) ret)))
	      (setq statuses (cdr statuses)))
	    ret)))

(defun identica-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
	  (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
      ((identica-url-reserved-p c)
       (char-to-string c))
      ((eq c ? ) "+")
      (t (format "%%%x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun identica-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun identica-decode-html-entities (encoded-str)
  (if encoded-str
      (let ((cursor 0)
	    (found-at nil)
	    (result '()))
	(while (setq found-at
		     (string-match "&\\(#\\([0-9]+\\)\\|\\([A-Za-z]+\\)\\);"
				   encoded-str cursor))
	  (when (> found-at cursor)
	    (list-push (substring encoded-str cursor found-at) result))
	  (let ((number-entity (match-string-no-properties 2 encoded-str))
		(letter-entity (match-string-no-properties 3 encoded-str)))
	    (cond (number-entity
		   (list-push
		    (char-to-string
		     (identica-ucs-to-char
		      (string-to-number number-entity))) result))
		  (letter-entity
		   (cond ((string= "gt" letter-entity) (list-push ">" result))
			 ((string= "lt" letter-entity) (list-push "<" result))
			 (t (list-push "?" result))))
		  (t (list-push "?" result)))
	    (setq cursor (match-end 0))))
	(list-push (substring encoded-str cursor) result)
	(apply 'concat (nreverse result)))
    ""))

(defun identica-timer-action (func)
  (let ((buf (get-buffer identica-buffer)))
    (if (null buf)
	(identica-stop)
      (funcall func)
      )))

(defun identica-update-status-if-not-blank (status)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (identica-http-post "statuses" "update"
			  `(("status" . ,status)
			    ("source" . "emacs-identicamode")))
    t))

(defun identica-update-status-from-minibuffer (&optional init-str)
  (if (null init-str) (setq init-str ""))
  (let ((status init-str) (not-posted-p t))
    (while not-posted-p
      (setq status (read-from-minibuffer "Status: " status nil nil nil nil t))
      (while (<= 140 (length status))
        (setq status (read-from-minibuffer (format "Status (%d): "
                                                   (- 140 (length status)))
                                           status nil nil nil nil t)))
      (setq not-posted-p
	    (not (identica-update-status-if-not-blank status))))))

(defun identica-update-status-from-region (beg end)
  (interactive "r")
  (if (> (- end beg) 140) (setq end (+ beg 140)))
  (if (< (- end beg) -140) (setq beg (+ end 140)))
  (identica-update-status-if-not-blank (buffer-substring beg end)))

(defun identica-update-lambda ()
  (interactive)
  (identica-http-post
   "statuses" "update"
   `(("status" . "\xd34b\xd22b\xd26f\xd224\xd224\xd268\xd34b")
     ("source" . "identicamode"))))

(defun identica-update-jojo (usr msg)
  (if (string-match "\xde21\xd24b\\(\xd22a\xe0b0\\|\xdaae\xe6cd\\)\xd24f\xd0d6\\([^\xd0d7]+\\)\xd0d7\xd248\xdc40\xd226"
		    msg)
      (identica-http-post
       "statuses" "update"
       `(("status" . ,(concat
		       "@" usr " "
		       (match-string-no-properties 2 msg)
		       "\xd0a1\xd24f\xd243!?"))
	 ("source" . "identicamode")))))

;;;
;;; Commands
;;;

(defun identica-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'identica-friends-timeline))
  (if identica-timer
      nil
    (setq identica-timer
	  (run-at-time "0 sec"
		       identica-timer-interval
		       #'identica-timer-action action))))

(defun identica-stop ()
  (interactive)
  (cancel-timer identica-timer)
  (setq identica-timer nil))

(defun identica-friends-timeline ()
  (interactive)
  (let ((buf (get-buffer identica-buffer)))
    (if (not buf)
	(identica-stop)
       (if (not identica-friends-timeline-last-update)
	   (identica-http-get "statuses" "friends_timeline")
	 (let* ((system-time-locale "C")
		(since
		  (identica-global-strftime
		   "%a, %d %b %Y %H:%M:%S GMT"
		   identica-friends-timeline-last-update)))
	   (identica-http-get "statuses" "friends_timeline"
			       `(("since" . ,since))
				)))))

  (if identica-icon-mode
      (if identica-image-stack
	  (let ((proc
		 (apply
		  #'start-process
		  "wget-images"
		  (identica-wget-buffer)
		  "wget"
		  (format "--directory-prefix=%s" identica-tmp-dir)
		  "--no-clobber"
		  "--quiet"
		  identica-image-stack)))
	    (set-process-sentinel
	     proc
	     (lambda (proc stat)
	       (clear-image-cache)
	       (save-excursion
		 (set-buffer (identica-wget-buffer))
		 )))))))

(defun identica-update-status-interactive ()
  (interactive)
  (identica-update-status-from-minibuffer))

(defun identica-erase-old-statuses ()
  (interactive)
  (setq identica-friends-timeline-data nil)
  (if (not identica-friends-timeline-last-update)
      (identica-http-get "statuses" "friends_timeline")
    (let* ((system-time-locale "C")
	   (since
	     (identica-global-strftime
	      "%a, %d %b %Y %H:%M:%S GMT"
	      identica-friends-timeline-last-update)))
      (identica-http-get "statuses" "friends_timeline"
			    `(("since" . ,since))
			   ))))

(defun identica-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun identica-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(uri (get-text-property (point) 'uri)))
    (if username
	(identica-update-status-from-minibuffer (concat "@" username " "))
      (if uri
	  (browse-url uri)))))

(defun identica-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun identica-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(identica-update-status-from-minibuffer (concat "@" username " ")))))

(defun identica-get-password ()
  (or identica-password
      (setq identica-password (read-passwd "identica-mode: "))))

(defun identica-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos))
    (setq pos (identica-get-next-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (message "End of status."))))

(defun identica-get-next-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop identica-username-face)))
	(setq pos (next-single-property-change pos 'face))
	(when (eq pos nil) (throw 'not-found nil))
	(setq prop (get-text-property pos 'face)))
      pos)))

(defun identica-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((pos))
    (setq pos (identica-get-previous-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (message "Start of status."))))

(defun identica-get-previous-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop identica-username-face)))
	(setq pos (previous-single-property-change pos 'face))
	(when (eq pos nil) (throw 'not-found nil))
	(setq prop (get-text-property pos 'face)))
      pos)))

(defun identica-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (identica-get-username-at-pos (point)))
	(pos (identica-get-next-username-face-pos (point))))
    (while (and (not (eq pos nil))
		(not (equal (identica-get-username-at-pos pos) user-name)))
      (setq pos (identica-get-next-username-face-pos pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "End of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun identica-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (identica-get-username-at-pos (point)))
	(pos (identica-get-previous-username-face-pos (point))))
    (while (and (not (eq pos nil))
		(not (equal (identica-get-username-at-pos pos) user-name)))
      (setq pos (identica-get-previous-username-face-pos pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "Start of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun identica-get-username-at-pos (pos)
  (let ((start-pos pos)
	(end-pos))
    (catch 'not-found
      (while (eq (get-text-property start-pos 'face) identica-username-face)
	(setq start-pos (1- start-pos))
	(when (or (eq start-pos nil) (eq start-pos 0)) (throw 'not-found nil)))
      (setq start-pos (1+ start-pos))
      (setq end-pos (next-single-property-change pos 'face))
      (buffer-substring start-pos end-pos))))

(defun identica-get-status-url (id)
  "Generate status URL."
  (format "http://%s/notice/%d" laconica-server id))

;;;###autoload
(defun identica ()
  "Start identica-mode."
  (interactive)
  (identica-mode))

(provide 'identica-mode)
;;; identica.el ends here
