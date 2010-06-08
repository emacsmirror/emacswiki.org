;;; identica-mode.el --- Major mode for Identica

;; Copyright (C) 2008, 2009, 2010 Gabriel Saldana
;; Copyright (C) 2009 Bradley M. Kuhn

;; Author: Gabriel Saldana <gsaldana@gmail.com>
;; Last update: 2009-02-21
;; Version: 1.0
;; Keywords: identica web
;; URL: http://blog.nethazard.net/identica-mode-for-emacs/
;; Contributors:
;;     Jason McBrayer <jmcbray@carcosa.net> (minor updates for working under Emacs 23)
;;     Alex Schröder <kensanata@gmail.com> (mode map patches)
;;     Christian Cheng (fixed long standing xml parsing bug)
;;     Carlos A. Perilla from denting-mode
;;     Alberto Garcia <agarcia@igalia.com> (integrated patch from twittering-mode for retrieving multiplemethods)
;;     Bradley M. Kuhn <bkuhn@ebb.org> (editing status from edit-buffer rather than minibuffer)
;;     Jason McBrayer <jmcbray@carcosa.net> (replace group tags with hashtags on redents, longlines use)
;;     Sean Neakums (patches of bugs flagged by byte-compiler)
;;     Shyam Karanatt <shyam@swathanthran.in> (several patches and code cleanup, new http backend based on url.el)
;;     Tezcatl Franco <tzk@riseup.net> (ur1.ca support)

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

;; You can use M-x customize-group identica-mode to setup all settings or simply
;; add the following to your .emacs or your prefered customizations file

;; (require 'identica-mode)
;; (setq identica-username "yourusername")
;; (setq identica-password "yourpassword")

;; If you want to post from the minibufer without having identica buffer active, add the following global keybinding.
;; Add this to send status updates
;; (global-set-key "\C-cip" 'identica-update-status-interactive)
;; Add this to send direct messages
;; (global-set-key "\C-cid" 'identica-direct-message-interactive)

;; If you want to connect to a custom statusnet server add this and change
;; identi.ca with your server's doman name.

;; (setq statusnet-server "identi.ca")

;; Start using with M-x identica

;; Follow me on identica: http://identi.ca/gabrielsaldana

(eval-when-compile (require 'cl))
(require 'xml)
(require 'parse-time)
(require 'longlines)
(require 'url)
(require 'url-http)
(require 'json)

(defconst identica-mode-version "1.0")

;;url-basepath fix for emacs22
(unless (fboundp 'url-basepath)
  (defalias 'url-basepath 'url-file-directory))

(defgroup identica-mode nil
  "Identica Mode for microblogging"
  :tag "Microblogging"
  :link '(url-link http://blog.nethazard.net/identica-mode-for-emacs/)
  :group 'applications )

(defun identica-mode-version ()
  "Display a message for identica-mode version."
  (interactive)
  (let ((version-string
	 (format "identica-mode-v%s" identica-mode-version)))
    (if (interactive-p)
	(message "%s" version-string)
      version-string)))

(defvar identica-mode-map (make-sparse-keymap "Identi.ca"))
(defvar menu-bar-identica-mode-menu nil)
(defvar identica-timer nil "Timer object for timeline refreshing will be stored here. DO NOT SET VALUE MANUALLY.")
(defvar identica-last-timeline-retrieved nil)

(defvar identica-urlshortening-services-map
  '((tinyurl . "http://tinyurl.com/api-create.php?url=")
    (toly    . "http://to.ly/api.php?longurl=")
    (google . "http://ggl-shortener.appspot.com/?url=")
    (ur1ca . "http://ur1.ca")
    (tighturl    . "http://2tu.us"))
  "Alist of tinyfy services")

(defvar identica-new-dents-count 0
  "Number of new tweets when `identica-new-dents-hook' is run")

(defvar identica-new-dents-hook nil
  "Hook run when new twits are received.

You can read `identica-new-dents-count' to get the number of new
tweets received when this hook is run.")

;; Menu
(unless menu-bar-identica-mode-menu
  (easy-menu-define
    menu-bar-identica-mode-menu identica-mode-map ""
    '("Identi.ca"
      ["Send an update" identica-update-status-interactive t]
      ["Send a direct message" identica-direct-message-interactive t]
      ["Re-dent someone's update" identica-redent t]
      ["Add as favorite" identica-favorite t]
      ["Follow user" identica-follow]
      ["Unfollow user" identica-unfollow]
      ["--" nil nil]
      ["Friends timeline" identica-friends-timeline t]
      ["Public timeline" identica-public-timeline t]
      ["Replies timeline" identica-replies-timeline t]
      ["User timeline" identica-user-timeline t]
      ["Group timeline" identica-group-timeline t]
      ["Tag timeline" identica-tag-timeline t]
)))

(defcustom identica-idle-time 20
  "Idle time"
  :type 'integer
  :group 'identica-mode)

(defcustom identica-timer-interval 90
  "Timer interval to refresh the timeline"
  :type 'integer
  :group 'identica-mode)

(defcustom identica-username nil
  "Your identi.ca username. If nil, you will be prompted"
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'identica-mode)

(defcustom identica-password nil
  "Your identi.ca password. If nil, you will be prompted"
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'identica-mode)

(defcustom statusnet-server "identi.ca"
  "Statusnet instance url"
  :type 'string
  :group 'identica-mode)

(defcustom statusnet-port 80
  "Port on which StatusNet instance listens"
  :type 'integer
  :group 'identica-mode)

(defcustom identica-default-timeline "friends_timeline"
  "Default timeline to retrieve"
  :type 'string
  :options '("friends_timeline" "public_timeline" "replies")
  :group 'identica-mode)

(defcustom identica-display-success-messages nil
  "Display messages when the timeline is successfully retrieved"
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-update-status-edit-confirm-cancellation nil
  "If t, ask user if they are sure when aborting editing of an
  identica status update when using an edit-buffer"
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-soft-wrap-status nil
  "If non-nil, don't fill status messages in the timeline as
  paragraphs. Instead, use visual-line-mode or longlines-mode if
  available to wrap messages.  This may work better for narrow
  timeline windows."
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-update-status-method 'minibuffer
  "Method for performaing status updates.

The available choices are:

  'minibuffer  - edit the status update in the minibuffer.
  'edit-buffer - edit the status update in an independent buffer."
  :type '(choice (const :tag "Edit status in minibuffer" minibuffer)
		 (const :tag "Edit status in independent buffer" edit-buffer))
  :group 'identica-mode)

(defcustom identica-http-get-timeout 10
  "Controls how long to wait for a response from the server."
  :type 'integer
  :group 'identica-mode)

;; Initialize with default timeline
(defvar identica-method identica-default-timeline)
(defvar identica-method-class "statuses")

(defvar identica-scroll-mode nil)
(make-variable-buffer-local 'identica-scroll-mode)

(defvar identica-source "identica-mode")

(defcustom identica-redent-format "♻"
  "The format/symbol to represent redents"
  :type 'string
  :group 'identica-mode)

(defcustom identica-status-format "%i %s,  %@:\n  %t // from %f%L%r"
  "The format used to display the status updates"
  :type 'string
  :group 'identica-mode)
;; %s - screen_name
;; %S - name
;; %i - profile_image
;; %d - description
;; %l - location
;; %L - " [location]"
;; %r - in reply to status
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

(defcustom identica-urlshortening-service 'ur1ca
  "The service to use for URL shortening. Values understood are
ur1ca, tighturl, tinyurl, toly, and google"
  :type 'symbol
  :group 'identica-mode)

(defvar identica-buffer "*identica*")
(defun identica-buffer (&optional method)
  (unless method
    (setq method "friends_timeline"))
  (identica-get-or-generate-buffer identica-buffer))

(defvar identica-http-buffer nil
  "Pointer to the current http response buffer.")

(defvar identica-timeline-data nil)
(defvar identica-timeline-last-update nil)

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
  (identica-render-timeline))

(defun identica-scroll-mode (&optional arg)
  (interactive)
  (setq identica-scroll-mode
	(if (null arg)
	    (not identica-scroll-mode)
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
(defvar identica-debug-buffer "*identica-debug*")
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
      (define-key km "\C-c\C-r" 'identica-replies-timeline)
      (define-key km "\C-c\C-a" 'identica-public-timeline)
      (define-key km "\C-c\C-g" 'identica-group-timeline)
      (define-key km "\C-c\C-t" 'identica-tag-timeline)
      (define-key km "\C-c\C-k" 'identica-stop)
      (define-key km "\C-c\C-u" 'identica-user-timeline)
      (define-key km "\C-c\C-s" 'identica-update-status-interactive)
      (define-key km "\C-c\C-d" 'identica-direct-message-interactive)
      (define-key km "\C-c\C-m" 'identica-redent)
      (define-key km "F" 'identica-favorite)
      (define-key km "\C-c\C-e" 'identica-erase-old-statuses)
      (define-key km "\C-m" 'identica-enter)
      (define-key km "R" 'identica-reply-to-user)
      (define-key km "\t" 'identica-next-link)
      (define-key km [backtab] 'identica-prev-link)
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
      (define-key km [backspace] 'scroll-down)
      (define-key km " " 'scroll-up)
      (define-key km "G" 'end-of-buffer)
      (define-key km "g" 'identica-current-timeline)
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
  (modify-syntax-entry ?\" "w"  identica-mode-syntax-table))

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
  (add-to-list 'minor-mode-alist '(identica-scroll-mode " id-scroll")))

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

(defvar identica-mode-string identica-method)

(defun identica-set-mode-string (loading)
  (setq mode-name
	(if loading (concat "loading " identica-method "...") identica-method)))

(defvar identica-mode-hook nil
  "Identica-mode hook.")

(defun identica-mode ()
  "Major mode for Identica
\\{identica-mode-map}"
  (interactive)
  (switch-to-buffer (identica-buffer))
  (buffer-disable-undo (identica-buffer))
  (kill-all-local-variables)
  (identica-mode-init-variables)
  (use-local-map identica-mode-map)
  (setq major-mode 'identica-mode)
  (setq mode-name identica-mode-string)
  (set-syntax-table identica-mode-syntax-table)
  (run-mode-hooks 'identica-mode-hook)
  (font-lock-mode -1)
  (if identica-soft-wrap-status
      (if (fboundp 'visual-line-mode)
          (visual-line-mode t)
	(if (fboundp 'longlines-mode)
	    (longlines-mode t))))
  (identica-start))

;;;
;;; Basic HTTP functions
;;;

(defun identica-set-proxy (&optional url username passwd server port)
  "Sets the proxy authentication variables as required by url
library.When called with no arguments, it reads `identica-mode' proxy
variables to get the authentication parameters.URL is either a string
or parsed URL. If URL is non-nil and valid, proxy authentication
values are read from it.the rest of the arguments can be used to
directly set proxy authentication.This function essentially adds
authentication parameters from one of the above methods to the double
alist `url-http-proxy-basic-auth-storage' and sets `url-using-proxy'."
  (let* ((href (if (stringp url)
		   (url-generic-parse-url url)
		 url))
	 (port (or (and href (url-port href))
		   port identica-proxy-port))
	 (port (if (integerp port) (int-to-string port) port))
	 (server (or (and href (url-host href))
		     server identica-proxy-server))
	 (server (and server
		      (concat server (when port (concat ":" port)))))
	 (file (if href (let ((file-url (url-filename href)))
			  (cond
			   ((string= "" file-url) "/")
			   ((string-match "/$" file-url) file-url)
			   (t (url-basepath file-url))))
		 "Proxy"))
	 (password (or (and href (url-password href))
		       passwd identica-proxy-password))
	 (auth (concat (or (and href (url-user href))
			   username identica-proxy-user)
		       (and password (concat ":" password)))))
    (when (and identica-proxy-use
	       (not (string= "" server))
	       (not (string= "" auth)))
      (setq url-using-proxy server)
      (let* ((proxy-double-alist
	      (or (assoc server
			 url-http-proxy-basic-auth-storage)
		  (car (push (cons server nil)
			     url-http-proxy-basic-auth-storage))))
	     (proxy-auth-alist (assoc file proxy-double-alist)))
	(if proxy-auth-alist
	    (setcdr proxy-auth-alist (base64-encode-string auth))
	  (setcdr proxy-double-alist
		  (cons (cons file
			      (base64-encode-string auth))
			(cdr-safe proxy-double-alist))))))))

(defun identica-change-user ()
  (interactive)
  "Interactive function to instantly change user authentication by
directly reading parameters from user. This function only sets the
identica-mode variables `identica-username' and
`identica-password'.
It is the `identica-set-auth' function that eventually sets the
url library variables according to the above variables which does the
authentication. This will be done automatically in normal use cases
enabling dynamic change of user authentication."
  (setq identica-username
	(read-string (concat "Username [for " statusnet-server
			     ":" (int-to-string statusnet-port) "]: ")
		     nil nil identica-username)
	identica-password
	(read-passwd "Password: " nil identica-password))
  (identica-get-timeline))

(defun identica-set-auth (&optional url username passwd server port)
  "Sets the authentication parameters as required by url library.
If URL is non-nil and valid, it reads user authentication
parameters from url.
If URL is nil, Rest of the arguments can be used to directly set user
authentication.
When called with no arguments, user authentication parameters are
read from identica-mode variables `identica-username'
`identica-password' `statusnet-server' `statusnet-port'."
  (let* ((href (if (stringp url)
		   (url-generic-parse-url url)
		 url))
	 (port (or (and href (url-port href))
		   port statusnet-port))
	 (port (if (integerp port) (int-to-string port) port))
	 (server (or (and href (url-host href))
		     server statusnet-server))
	 (server (and server
		      (concat server (when port (concat ":" port)))))
	 (file (if href (let ((file-url (url-filename href)))
			  (cond
			   ((string= "" file-url) "/")
			   ((string-match "/$" file-url) file-url)
			   (t (url-basepath file-url))))
		 "Identi.ca API"))
	 (password (or (and href (url-password href))
		       passwd identica-password))
	 (auth (concat (or (and href (url-user href))
			   username identica-username)
		       (and password (concat ":" password)))))
    (when (and (not (string= "" server))
	       (not (string= "" auth)))
      (let* ((server-double-alist
	      (or (assoc server
			 url-http-real-basic-auth-storage)
		  (car (push (cons server nil)
			     url-http-real-basic-auth-storage))))
	     (api-auth-alist (assoc file server-double-alist)))
	(if api-auth-alist
	    (setcdr api-auth-alist (base64-encode-string auth))
	  (setcdr server-double-alist
		  (cons (cons file
			      (base64-encode-string auth))
			(cdr-safe server-double-alist))))))))

(defun identica-http-get (method-class method &optional parameters
				       sentinel sentinel-arguments)
  "Basic function which communicates with server.
METHOD-CLASS and METHOD are parameters for getting dents messages and
other information from server as specified in api documentation.
Third optional arguments specify the additional parameters required by
the above METHOD. It is specified as an alist with parameter name and
its corresponding value
SENTINEL represents the callback function to be called after the http
response is completely retrieved. SENTINEL-ARGUMENTS is the list of
arguments (if any) of the SENTINEL procedure."
  (or sentinel (setq sentinel 'identica-http-get-default-sentinel))
  (let ((url (concat "http://" statusnet-server "/api/" method-class
		     "/" method ".xml"
		     (when parameters
		       (concat "?"
			       (mapconcat
				(lambda (param-pair)
				  (format "%s=%s"
					  (identica-percent-encode (car param-pair))
					  (identica-percent-encode (cdr param-pair))))
				parameters
				"&")))))
	(url-package-name "emacs-identica-mode")
	(url-package-version identica-mode-version)
	(url-show-status nil))
    (identica-set-proxy)
    (identica-set-auth url)
    (when (get-buffer-process identica-http-buffer)
      (delete-process identica-http-buffer)
      (kill-buffer identica-http-buffer))
    (setq identica-http-buffer
	  (url-retrieve url sentinel
			(append (list method-class method parameters)
				sentinel-arguments)))
    (save-excursion
      (set-buffer identica-buffer)
      (identica-set-mode-string t))))

(defun identica-http-get-default-sentinel
  (&optional status method-class method parameters success-message)
  (cond  ((setq error-object
		(or (assoc :error status)
		    (and (equal :error (car status))
			 (cadr status))))
	  (let ((error-data (format "%s" (caddr error-object))))
	    (when (cond
		   ((string= error-data "deleted\n") t)
		   ((and (string= error-data "404") method
			 (= 13 (string-match "/" method)))
		    (message "No Such User: %s" (substring method 14))
		    t)
		   ((y-or-n-p
		     (format "Identica-Mode: Network error:%s Retry? "
			     status))
		    (identica-http-get method-class method parameters)
		    nil))
	      ;; when the network process is deleted by another query
	      ;; or the user queried is not found , query is _finished_
	      ;; unsuccessful and we want to restore identica-method
	      ;; to loose track of this unsuccessful attempt
	      (setq identica-method identica-last-timeline-retrieved))))
	 ((< (- (point-max) (or (re-search-forward ">\r?\n\r*$" nil t) 0)) 2)
		;;Checking the whether the message is complete by
		;;searching for > that closes the last tag, followed by
		;;CRLF at (point-max)
	  (when (setq body (identica-get-response-body))
	    (setq identica-new-dents-count
		  (count t (mapcar
			    #'identica-cache-status-datum
			    (reverse (identica-xmltree-to-status
				      body)))))
	    (identica-render-timeline)
	    (if (> identica-new-dents-count 0)
		(run-hooks 'identica-new-dents-hook))
	    (when identica-display-success-messages
	      (message (or success-message "Success: Get"))))))
  (unless (get-buffer-process (current-buffer))
    (kill-buffer (current-buffer))))

(defun identica-render-timeline ()
  (with-current-buffer (identica-buffer)
    (let ((point (point))
	  (end (point-max)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (status)
	      (insert (identica-format-status
		       status identica-status-format))
	      (if (not identica-soft-wrap-status)
		  (progn
		    (fill-region-as-paragraph
		     (save-excursion (beginning-of-line) (point)) (point))))
	      (insert "\n"))
	    identica-timeline-data)
      (if (and identica-image-stack window-system)
	  (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if identica-scroll-mode (- (point-max) end) 0)))
      (identica-set-mode-string nil)
      (setq identica-last-timeline-retrieved identica-method))))

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
		    icon-string))))))
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
	  ((?r)                         ; %r - in_reply_to_status_id
	   (let ((reply-id (attr 'in-reply-to-status-id))
		 (reply-name (attr 'in-reply-to-screen-name)))
	     (unless (or (null reply-id) (string= "" reply-id)
			 (null reply-name) (string= "" reply-name))
	       (let ((in-reply-to-string (format "in reply to %s" reply-name))
		     (url (identica-get-status-url reply-id)))
		 (add-text-properties
		  0 (length in-reply-to-string)
		  `(mouse-face highlight
			       face identica-uri-face
			       uri ,url)
		  in-reply-to-string)
		 (list-push (concat " " in-reply-to-string) result)))))
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
	   (list-push (char-to-string c) result))))
      (list-push (substring format-str cursor) result)
      (let ((formatted-status (apply 'concat (nreverse result))))
	(add-text-properties 0 (length formatted-status)
			     `(username ,(attr 'user-screen-name)
					id, (attr 'id)
					text ,(attr 'text))
			     formatted-status)
	formatted-status))))

(defun identica-http-post
  (method-class method &optional parameters sentinel sentinel-arguments)
  "Send HTTP POST request to statusnet server
METHOD-CLASS must be one of Identica API method classes(statuses, users or direct_messages).
METHOD must be one of Identica API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters. ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (or sentinel (setq sentinel 'identica-http-post-default-sentinel))
  (let ((url-request-method "POST")
	 (url (concat "http://"statusnet-server "/api/" method-class "/" method ".xml"
		      (when parameters
			(concat "?"
				(mapconcat
				 (lambda (param-pair)
				  (format "%s=%s"
					  (identica-percent-encode (car param-pair))
					  (identica-percent-encode (cdr param-pair))))
				 parameters
				 "&")))))
	 (url-package-name "emacs-identicamode")
	 (url-package-version identica-mode-version)
	 (url-show-status nil))
    (identica-set-proxy)
    (identica-set-auth url)
    (when (get-buffer-process identica-http-buffer)
      (delete-process identica-http-buffer)
      (kill-buffer identica-http-buffer))
    (url-retrieve url sentinel
		  (append (list method-class method parameters)
			  sentinel-arguments))))

(defun identica-http-post-default-sentinel
  (&optional status method-class method parameters success-message)
  (cond  ((and
	   (setq error-object (or (assoc :error status)
				 (and (equal :error (car status))
				      (cadr status))))
	   (y-or-n-p (format "Network error:%s %s Retry?"
			     (cadr error-object)
			     (caddr error-object))))
	  (identica-http-post method-class method parameters nil success-message))
	 (identica-display-success-messages
	  (message (or success-message "Success: Post"))))
  (unless (get-buffer-process (current-buffer))
    (kill-buffer (current-buffer))))

(defun identica-get-response-header (&optional buffer)
  "Exract HTTP response header from HTTP response.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, current-buffer is parsed."
  (or buffer
      (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((end (or (and (search-forward-regexp "\r?\n\r?\n" (point-max) t)
			(match-beginning 0))
		   0)))
      (and (> end 1)
	   (buffer-substring (point-min) end)))))

(defun identica-get-response-body (&optional buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a XML tree as list.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, current-buffer is parsed."
  (or buffer
      (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (set-buffer-multibyte t)
    (let ((start (save-excursion
		  (goto-char (point-min))
		  (and (re-search-forward "<\?xml" (point-max) t)
		       (match-beginning 0)))))
      (identica-clean-response-body)
      (and start
	   (xml-parse-region start (point-max))))))

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
(buffer-string)))

(defun identica-clean-response-body ()
  "Removes weird strings (e.g., 1afc, a or 0) from within the
response body.  Known Statusnet issue.  Mostly harmless except if
in tags."
  (goto-char (point-min))
  (while (re-search-forward "\r?\n[0-9a-z]+\r?\n" nil t)
    (replace-match "")))

(defun identica-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default identica-timeline-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'identica-timeline-data))
  (let ((id (cdr (assq 'id status-datum))))
    (if (or (null (symbol-value data-var))
	    (not (find-if
		  (lambda (item)
		    (eql id (cdr (assq 'id item))))
		  (symbol-value data-var))))
	(progn
	  (set data-var (cons status-datum (symbol-value data-var)))
	  t)
      nil)))

(defun identica-status-to-status-datum (status)
  (flet ((assq-get (item seq)
		   (car (cddr (assq item seq)))))
    (let* ((status-data (cddr status))
	   id text source created-at truncated
	   in-reply-to-status-id
	   in-reply-to-screen-name
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
      (setq in-reply-to-status-id
	    (identica-decode-html-entities
	     (assq-get 'in_reply_to_status_id status-data)))
      (setq in-reply-to-screen-name
	    (identica-decode-html-entities
	     (assq-get 'in_reply_to_screen_name status-data)))
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
		    uri ,(concat "https://" statusnet-server "/" user-screen-name)
		    face identica-username-face)
       user-name)

      ;; make screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
		    face identica-username-face
		    uri ,(concat "https://" statusnet-server "/" user-screen-name)
		    face identica-username-face)
       user-screen-name)

      ;; make URI clickable
      (setq regex-index 0)
      (while regex-index
	(setq regex-index
	      (string-match "@\\([_[:word:]0-9]+\\)\\|!\\([_[:word:]0-9\-]+\\)\\|#\\([_[:word:]0-9\-]+\\)\\|\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)"
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
			    (concat "https://" statusnet-server "/" screen-name)
			  (if group-name
			      (concat "https://" statusnet-server "/group/" group-name)
			    (concat "https://" statusnet-server "/tag/" tag-name)))
		   uri-in-text ,(if screen-name
			    (concat "https://" statusnet-server "/" screen-name)
			  (if group-name
			      (concat "https://" statusnet-server "/group/" group-name)
			    (concat "https://" statusnet-server "/tag/" tag-name)))
                   tag ,tag-name
                   group ,group-name)
	       `(mouse-face highlight
			    face identica-uri-face
			    uri ,uri
			    uri-in-text ,uri))
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
	     source)))

      ;; save last update time
      (setq identica-timeline-last-update created-at)

      ;; highlight replies
      (if (string-match (concat "@" identica-username) text)
	  (add-text-properties 0 (length text)
			       `(face identica-reply-face) text))
      (mapcar
       (lambda (sym)
	 `(,sym . ,(symbol-value sym)))
       '(id text source created-at truncated
	    in-reply-to-status-id
	    in-reply-to-screen-name
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
      (funcall func))))

(defun identica-update-status-if-not-blank (method-class method status &optional parameters reply-to-id)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (if (equal method-class "statuses")
	(identica-http-post method-class method
			    `(("status" . ,status)
			      ("source" . ,identica-source)
			      ,@(if reply-to-id
				    `(("in_reply_to_status_id"
				       . ,(number-to-string reply-to-id))))))
      (identica-http-post method-class method
			  `(("text" . ,status)
			    ("user" . ,parameters) ;must change this to parse parameters as list
			    ("source" . ,identica-source))))

    t))

(defvar identica-update-status-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'identica-update-status-from-edit-buffer-send)
    (define-key map (kbd "C-c C-k") 'identica-update-status-from-edit-buffer-cancel)
    map))

(define-derived-mode identica-update-status-edit-mode text-mode "Identica Status Edit"
  (use-local-map identica-update-status-edit-map))

(defvar identica-update-status-edit-method-class)
(defvar identica-update-status-edit-method)
(defvar identica-update-status-edit-parameters)
(defvar identica-update-status-edit-reply-to-id)

(defun identica-update-status-edit-in-edit-buffer (init-str msgtype method-class method parameters &optional reply-to-id)
  (let ((buf (get-buffer-create "*identica-status-update-edit*")))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (identica-update-status-edit-mode)
      (longlines-mode)
      (make-local-variable 'identica-update-status-edit-method-class)
      (make-local-variable 'identica-update-status-edit-method)
      (make-local-variable 'identica-update-status-edit-parameters)
      (make-local-variable 'identica-update-status-edit-reply-to-id)
      (setq identica-update-status-edit-method-class method-class)
      (setq identica-update-status-edit-method method)
      (setq identica-update-status-edit-parameters parameters)
      (setq identica-update-status-edit-reply-to-id reply-to-id)
      (message identica-update-status-edit-method-class)
      (insert init-str)
      (if (> (length parameters) 0)
          (setq mode-line-format (cons (format "%s(%s) (%%i/140) " msgtype parameters) mode-line-format))
        t (setq mode-line-format (cons (format "%s (%%i/140) " msgtype) mode-line-format)))
      (message "Type C-c C-c to post status update (C-c C-k to cancel)."))))

(defun identica-show-minibuffer-length (&optional beg end len)
  "Show the number of characters in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (let* ((status-len (- (buffer-size) (minibuffer-prompt-width)))
	   (mes (format "%d" status-len)))
      (if (<= 23 emacs-major-version)
	  (minibuffer-message mes) ; Emacs23 or later
	(minibuffer-message (concat " (" mes ")")))
      )))

(defun identica-setup-minibuffer ()
  (identica-show-minibuffer-length)
  (add-hook 'post-command-hook 'identica-show-minibuffer-length t t))

(defun identica-finish-minibuffer ()
  (remove-hook 'post-command-hook 'identica-show-minibuffer-length t))

(defun identica-update-status (update-input-method &optional init-str reply-to-id method-class method parameters)
  (if (null init-str) (setq init-str ""))
  (let ((msgtype "")
	(status init-str)
	(not-posted-p t)
	(user nil)
	(map minibuffer-local-map)
	(minibuffer-message-timeout nil))
    (define-key map (kbd "<f4>") 'identica-shortenurl-replace-at-point)
    (if (null method-class)
        (progn (setq msgtype "Status")
               (setq method-class "statuses")
               (setq method "update"))
      (progn (setq msgtype "Direct message")
             (setq method-class "direct_messages")
             (setq parameters (read-from-minibuffer "To user: " user nil nil nil nil t))
             (setq method "new")))
    (cond ((eq update-input-method 'minibuffer)
	   (add-hook 'minibuffer-setup-hook 'identica-setup-minibuffer t)
	   (add-hook 'minibuffer-exit-hook 'identica-finish-minibuffer t)
	   (unwind-protect
           (while not-posted-p
             (setq status (read-from-minibuffer (concat msgtype ": ") status nil nil nil nil t))
             (while (< 141 (length status))
               (setq status (read-from-minibuffer (format (concat msgtype "(%d): ")
                                                          (- 140 (length status)))
                                                  status nil nil nil nil t)))
             (setq not-posted-p
                   (not (identica-update-status-if-not-blank method-class method status parameters reply-to-id))))
	   (remove-hook 'minibuffer-setup-hook 'identica-setup-minibuffer)
	   (remove-hook 'minibuffer-exit-hook 'identica-finish-minibuffer)))
          ((eq update-input-method 'edit-buffer)
           (identica-update-status-edit-in-edit-buffer init-str msgtype method-class method parameters reply-to-id))
          (t (error "Unknown update-input-method in identica-update-status: %S" update-input-method)))))

(defun identica-update-status-from-edit-buffer-send ()
  (interactive)
  (with-current-buffer "*identica-status-update-edit*"
    (longlines-encode-region (point-min) (point-max))
    (let* ((status (buffer-substring-no-properties (point-min) (point-max)))
           (status-len (length status)))
      (if (< 140 status-len)
          (message (format "Beyond 140 chars.  Remove %d chars." (- status-len 140)))
        (if (identica-update-status-if-not-blank identica-update-status-edit-method-class
              identica-update-status-edit-method status identica-update-status-edit-parameters identica-update-status-edit-reply-to-id)
            (progn
              (erase-buffer)
              (bury-buffer))
          (message "Update failed!"))))))

(defun identica-update-status-from-minibuffer (&optional init-str method-class method parameters reply-to-id)
  (interactive)
  (identica-update-status 'minibuffer init-str method-class method parameters reply-to-id))

(defun identica-update-status-from-edit-buffer (&optional init-str method-class method parameters)
  (interactive)
  (identica-update-status 'edit-buffer init-str method-class method parameters))

(defun identica-update-status-from-edit-buffer-cancel ()
  (interactive)
  (when (or (not identica-update-status-edit-confirm-cancellation)
	    (yes-or-no-p
	     "Really cancel editing this status message (any changes will be lost)?"))
    (erase-buffer)
    (bury-buffer)))

(defun identica-update-status-from-region (beg end)
  (interactive "r")
  (if (> (- end beg) 140) (setq end (+ beg 140)))
  (if (< (- end beg) -140) (setq beg (+ end 140)))
  (identica-update-status-if-not-blank "statuses" "update" (buffer-substring beg end)))

(defun identica-tinyurl-unjson-google (result)
  "Gets only the URL from JSON URL tinyfying service results.

Google's shortening service, goo.gl, returns shortened URLs as a
JSON dictionary. This function retrieves only the URL value from
this dictionary, only if identica-urlshortening-service is 'google.
"
  (if (eq identica-urlshortening-service 'google)
      (cdr (assoc 'short_url (json-read-from-string result)))
    result))

(defun identica-ur1ca-get (api longurl)
  "Shortens url through ur1.ca free service 'as in freedom'"
  (with-temp-buffer
    (call-process "curl" nil (current-buffer) nil "-s" (concat "-dlongurl=" longurl) api)
    (goto-char (point-min))
    (setq ur1short
	  (if (search-forward-regexp "Your .* is: .*>\\(http://ur1.ca/[0-9A-Za-z].*\\)</a>" nil t)
	      (match-string-no-properties 1)))))

(defun identica-shortenurl-get (longurl)
  "Shortens url through a url shortening service"
  (let ((api (cdr (assoc identica-urlshortening-service
			 identica-urlshortening-services-map))))
    (unless api
      (error "`identica-urlshortening-service' was invalid. try one of %s"
	      (mapconcat (lambda (x)
			   (symbol-name (car x)))
			 identica-urlshortening-services-map ", ")
	      "."))
    (if longurl
	(if (or (eq identica-urlshortening-service 'ur1ca) (eq identica-urlshortening-service 'tighturl))
	    (identica-ur1ca-get api longurl)
	  (let ((buffer (url-retrieve-synchronously (concat api longurl))))
	    (with-current-buffer buffer
	    (goto-char (point-min))
	    (prog1
                (identica-tinyurl-unjson-google
                 (if (search-forward-regexp "\n\r?\n\\([^\n\r]*\\)" nil t)
                     (match-string-no-properties 1)
                   (error "URL shortening service failed: %s" longurl)))
	      (kill-buffer buffer))))
      nil))))

(defun identica-shortenurl-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (identica-shortenurl-get (thing-at-point 'url))))
	(when url
	  (save-restriction
	    (narrow-to-region (car url-bounds) (cdr url-bounds))
	    (delete-region (point-min) (point-max))
	    (insert url)))))))
;;;
;;; Commands
;;;

(defun identica-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'identica-current-timeline))
  (if identica-timer
      nil
    (setq identica-timer
	  (run-at-time "0 sec"
		       identica-timer-interval
		       #'identica-timer-action action))))

(defun identica-stop ()
"Stop Current network activitiy (if any) and the reload-timer."
  (interactive)
  (when (get-buffer-process identica-http-buffer)
    (delete-process identica-http-buffer)
    (kill-buffer identica-http-buffer))
  (setq identica-method identica-last-timeline-retrieved)
  (identica-set-mode-string nil)
  (and identica-timer
       (cancel-timer identica-timer))
  (setq identica-timer nil))

(defun identica-get-timeline ()
  (if (not (eq identica-last-timeline-retrieved identica-method))
      (setq identica-timeline-last-update nil
	    identica-timeline-data nil))
  (let ((buf (get-buffer identica-buffer)))
    (if (not buf)
	(identica-stop)
      (identica-http-get identica-method-class identica-method)))
  (if identica-icon-mode
      (if (and identica-image-stack window-system)
	  (let ((proc
		 (apply
		  #'start-process
		  "wget-images"
		  nil
		  "wget"
		  (format "--directory-prefix=%s" identica-tmp-dir)
		  "--no-clobber"
		  "--quiet"
		  identica-image-stack)))
	    (set-process-sentinel
	     proc
	     (lambda (proc stat)
	       (clear-image-cache)
	      ))))))

(defun identica-friends-timeline ()
  (interactive)
  (setq identica-method "friends_timeline")
  (setq identica-method-class "statuses")
  (identica-get-timeline))

(defun identica-replies-timeline ()
  (interactive)
  (setq identica-method "replies")
  (setq identica-method-class "statuses")
  (identica-get-timeline))

(defun identica-public-timeline ()
  (interactive)
  (setq identica-method "public_timeline")
  (setq identica-method-class "statuses")
  (identica-get-timeline))

(defun identica-group-timeline (&optional group)
  (interactive)
  (unless group
    (setq group (read-from-minibuffer "Group: " nil nil nil nil nil t)))
  (setq identica-method-class "statusnet/groups")
  (if (string-equal group "")
      (setq identica-method "timeline")
    (setq identica-method (concat "timeline/" group)))
  (identica-get-timeline))

(defun identica-tag-timeline (&optional tag)
  (interactive)
  (unless tag
    (setq tag (read-from-minibuffer "Tag: " nil nil nil nil nil t)))
  (setq identica-method-class "statusnet/tags")
  (if (string-equal tag "")
      (setq identica-method "timeline")
    (setq identica-method (concat "timeline/" tag)))
  (identica-get-timeline))

(defun identica-user-timeline ()
  (interactive)
  (let ((from-user (read-from-minibuffer "User [Empty for mine]: "
                                         nil nil nil nil nil t)))
    (if (string-equal from-user "")
        (setq identica-method "user_timeline")
      (setq identica-method (concat "user_timeline/" from-user))))
  (identica-get-timeline))

(defun identica-current-timeline ()
  (interactive)
  (identica-get-timeline))

(defun identica-update-status-interactive ()
  (interactive)
  (identica-update-status identica-update-status-method))

(defun identica-direct-message-interactive ()
  (interactive)
  (identica-update-status identica-update-status-method nil nil "direct_messages" "new"))

(defun identica-erase-old-statuses ()
  (interactive)
  (setq identica-timeline-data nil)
  (if (not identica-last-timeline-retrieved)
      (setq identica-last-timeline-retrieved identica-method))
      (identica-http-get "statuses" identica-last-timeline-retrieved)
      )

(defun identica-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun identica-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(id (get-text-property (point) 'id))
	(uri (get-text-property (point) 'uri))
        (group (get-text-property (point) 'group))
        (tag (get-text-property (point) 'tag)))
    (if group (identica-group-timeline group)
      (if tag (identica-tag-timeline tag)
        (if uri (browse-url uri)
          (if username
              (identica-update-status identica-update-status-method
                                      (concat "@" username " ") id)))))))

(defun identica-next-link nil
  (interactive)
  (goto-char (next-single-property-change (point) 'uri))
  (if (not (get-text-property (point) 'uri))
      (goto-char (next-single-property-change (point) 'uri))))

(defun identica-prev-link nil
  (interactive)
  (goto-char (previous-single-property-change (point) 'uri))
  (if (not (get-text-property (point) 'uri))
      (goto-char (previous-single-property-change (point) 'uri))))

(defun identica-follow (&optional remove)
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(method (if remove "destroy" "create"))
	(message (if remove "unfollowing" "following")))
    (unless username
      (setq username (read-from-minibuffer "user: ")))
    (if (> (length username) 0)
	(when (y-or-n-p (format "%s %s? " message username))
	  (identica-http-post (format "friendships/%s" method) username)
	  (message (format "Now %s %s" message username)))
      (message "No user selected"))))

(defun identica-unfollow ()
  (interactive)
  (identica-follow t))

(defun identica-favorite ()
  (interactive)
    (if (y-or-n-p "Do you want to favor this notice? ")
	(let ((id (get-text-property (point) 'id)))
	  (identica-http-post "favorites/create" (number-to-string id))
	  (message "Notice saved as favorite"))))

(defun identica-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun identica-redent ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(id (get-text-property (point) 'id))
	(text (replace-regexp-in-string "!\\(.\\)" "#\\1" (get-text-property (point) 'text))))
    (when username
       (identica-update-status identica-update-status-method
        (concat identica-redent-format " @" username ": " text) id))))

(defun identica-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(id (get-text-property (point) 'id)))
    (if username
	(identica-update-status identica-update-status-method (concat "@" username " ") id))))

(defun identica-get-password ()
  (or identica-password
      (setq identica-password (read-passwd "password: "))))

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
  (format "https://%s/notice/%s" statusnet-server id))

(defun identica-get-context-url (id)
  "Generate status URL."
  (format "https://%s/conversation/%s" statusnet-server id))

;;;###autoload
(defun identica ()
  "Start identica-mode."
  (interactive)
  (identica-mode))

(provide 'identica-mode)
;;; identica.el ends here
