;;; blip.el --- interface with blip.pl
(defvar blip-version-number "0.0.1")
;; Copyright (c) 2009 Adam Wołk
;; Time-stamp: <2009-06-11 00:52:28 mulander>
;; Author: mulander <netprobe@gmail.com>
;; Created: 2009.06.11
;; Version: %Id: 1%
;; Keywords: comm
;;

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

;;; Commentary:
;;
;; This is the beginnings of a library for interfacing with 
;; blip.pl from Emacs. It is heavily based on twit.el (version 0.3.5)
;; which can be found on http://www.emacswiki.org/emacs/twit.el
;; Additionaly, this looks like a nice way to learn some elisp.

;;; Notes:
;; There are some differences between twitter and blip.
;; First of all blip allows the user to associate extra content with a blip: 
;; pictures, videos and recordings.
;; Twitter uses @, .@ to note replies and users. Blip uses ^username to mark users,
;; >username to direct msgs a user and >>username to private msg.
;; Blip API uses JSON for data transmission.
;;
;;; Hacking:
;; Feel free to hack on this if you like, and post it back to
;; the emacswiki.  Just be sure to increment the version number
;; and write a change to the change log.
;;
;; From versions 0.0.1 onwards, versions are incremented like so:
;; <major>.<minor>.<bugfix/feature>
;;
;; Major versions are only incremented when a release is considered
;; truely stable (i.e. no memory leaks) and doesn't have any bugs.
;;
;; Minor version increments happen when there are significant changes
;; to the file (like changing twit-post-function to twit-post-status)
;;
;; bugfix/feature releases are incremented when new features are added
;; or bugs are fixed, that have little impact.
;;
;;; Change Log:.
;; Originally by Adam Wołk <netprobe@gmail.com> 2009-06-11
;; * 0.0.1 -- Initial release. (AW)
;;; TODO:
;; - [X] Key bindings							:0.0.1:
;; - [X] Post blips							:0.0.1:
;; - [X] Display recent blips						:0.0.1:
;; - [X] Inline user avatars						:0.0.1:
;; - [X] Display avatars of both users if the msg is direct or private	:0.0.1:
;; - [ ] Inline images from blips
;; - [ ] Attach images to Your blips 
;;; Bugs;
;; - [ ] First call to blip-show-recent-blips has auth error. Works on the second time.
;;       This issue is reflected in twit.el at least for my setup.
;; - [ ] Regexp failure instead of an error description in case of HTTP 204 no content.
;; Please report bugs to the blip emacs wiki page at:
;;   http://www.emacswiki.org/cgi-bin/wiki/Blip
;;; Code:
(require 'json)
(require 'url)
(require 'url-http)

(eval-when-compile
  (require 'cl))

;;* custom helper auth
(defun blip-set-auth (user pass)
   "Set the http url authentication string from USER and PASS."
   (let ((old-http-storage
          (assoc "api.blip.pl:80" (symbol-value url-basic-auth-storage)))
         (auth-pair
          (cons "Blip API"
                (base64-encode-string (format "%s:%s" user pass)))))
     (when old-http-storage
       (set url-basic-auth-storage
            (delete old-http-storage (symbol-value url-basic-auth-storage))))
     (set url-basic-auth-storage
	  (cons (list "api.blip.pl:80" auth-pair)
		(symbol-value url-basic-auth-storage)))))

;; Set up for easy space for your C-x C-e execution pleasure.
(when nil
	  (set url-basic-auth-storage nil)
	  )

(defun blip-get-auth ()
  (when (or (equal blip-user nil)
	    (equal blip-user ""))
    (setq blip-user (read-string "Username: ")))
  (when (or (equal blip-pass nil)
	    (equal blip-pass ""))
    (setq blip-pass (read-passwd "Password: ")))
  (when (equal (symbol-value url-basic-auth-storage) nil)
    (blip-set-auth blip-user blip-pass))
  (cdadar (symbol-value url-basic-auth-storage)))

;;* custom helper auth
(defun blip-set-user-pass (sym val)
  "Set the username/password pair after a customization.

Called with SYM and VAL by customize.  SYM is generally not used.

Note that this function uses a really cheap hack.
Basically the problem is that we need to run this whenever the `blip-user'
and `blip-pass' variables are customized and loaded.  The problem is, this
funciton is executed by cutomzie on Emacs initialization, during the
setting of `blip-user', but before the binding to `blip-pass', throwing an
error.

We get around this by using `condition-case' and handling the void-varible
error."
  (set-default sym val)
  (condition-case nil
      (let ((blip-pass-var 'blip-pass))
        (when (and (not (string= (symbol-value blip-pass-var) ""))
                   (not (string= blip-user "")))
          (blip-set-auth blip-user (symbol-value blip-pass-var))))
    (void-variable nil)))


;;* custom
(defgroup blip nil
  "blip.el is an emacs package for interfacing with Blip (http://blip.pl),
a Polish microblogging service. The blip.el package provides you with the ability to post
status updates and receive them."
  :version "0.1"
  :group 'blip)

(defcustom blip-user
  ""
  "Your blip username."
  :group 'blip
  :type  'string)

(defcustom blip-pass
  ""
  "Your blip password."
  :group 'blip)

(defcustom blip-show-user-images nil
  "Show user images beside each users blip."
  :type  'boolean
  :group 'blip)

(defcustom blip-user-image-dir
  (concat (car image-load-path) "blip")
  "Directory where blip user avatars are to be stored.

This directory need not be created."
  :type 'string
  :group 'blip)

;;* face
(defface blip-message-face
  '((default
      :family "helv"
      :height 1.1))
  "The font face to use for a blip message."
  :group 'blip)

(defface blip-author-face
  '((t
     (:height 0.8
      :weight bold
      :family "mono")))
  "The font face to use for the authors name"
  :group 'blip)

(defface blip-info-face
  '((t (:height 0.8 :slant italic)))
  "Face for displaying where, how and when someone blipped."
  :group 'blip)

(defface blip-title-face
  '((((background light))
     (:background "PowderBlue"
      :underline  "DeepSkyBlue"
      :box (:line-width 2 :color "PowderBlue" :style 0)))
    (((background dark))
     (:background "PowderBlue"
      :underline  "DeepSkyBlue"
      :box (:line-width 2 :color "PowderBlue" :style 0)))
    (t (:underline "white")))
  "Title Area of the recent blips buffer."
  :group 'blip)

(defface blip-logo-face
  '((((class color))
     (:family "mono"
      :weight bold
      :height 1.5
      :box (:line-width 2 :color "PowderBlue" :style 0)
      :background "Yellow3"
      :foreground "Yellow1"
      :underline  "DeepSkyBlue"))
    (t (:inverse)))
  "(b)"
  :group  'blip)

(defface blip-hash-at-face
    '((((class color) (background light))
       (:foreground "GoldenRod3"))
      (((class color) (background dark))
       (:foreground "GoldenRod"))
      (t (:underline "white")))
  "Face to show @msgs in"
  :group 'blip)

(defface blip-zebra-1-face
    '((((class color) (background light))
       (:foreground "black" :background "gray89"
        :box (:line-width 2 :color "gray89" :style 0)))
      (((class color) (background dark))
       (:foreground "white" :background "black"
        :box (:line-width 2 :color "black" :style 0)))
      (t (:inverse)))
  "Color one of zebra-striping of recent blips and followers list."
  :group 'blip)

(defface blip-zebra-2-face
    '((((class color) (background light))
       (:foreground "black" :background "AliceBlue"
        :box (:line-width 2 :color "AliceBlue" :style 0)))
      (((class color) (background dark))
       (:foreground "white" :background "grey4"
        :box (:line-width 2 :color "grey4" :style 0))))
  "Color two of zebra-striping of recent blips and followers list."
  :group 'blip)

(defface blip-error-face
  '((((class color))
     (:family "mono"
      :background "FireBrick" :foreground "Black"))
    (t (:inverse)))
  "Color of blip.el errors."
  :group 'blip)

(defface blip-too-long-face
  '((((supports :strike-through t)) :strike-through t )
    (t :inherit 'font-lock-warning-face))
  "Face for highlighting a blip that's too long to post"
  :group 'blip)

(defface blip-url-face
  '((default
     :weight bold))
  "Face for showing hyperlinks"
  :group 'blip)

(defface blip-fail-whale-face
  '((((class color))
       (:family "mono"
        :weight bold
        :height 4.0
        :box (:line-width 10 :color "SteelBlue3" :style 0)
        :background "SteelBlue3" :foreground "SteelBlue4"))
      (t (:inverse)))
  "(_)"
  :group 'blip)

;;* var blip http posting process
(defvar blip--process nil)

;;* var keymap
(defvar blip-status-mode-map (make-sparse-keymap)
  "Keymap for status messages and direct messages.")

;;* var keymap
(defvar blip-key-list
  '(("s" . blip-show-recent-blips)
    ("w" . blip-post)
    ("n" . blip-next-blip)
    ("p" . blip-previous-blip)    
    ("q" . bury-buffer)
    ))

(define-key blip-status-mode-map "g" 'blip-show-recent-blips)

;;* var keymap
(dolist (info blip-key-list)
  (define-key blip-status-mode-map (car info) (cdr info))
  )


;;* const url
(defconst blip-base-url "http://api.blip.pl")
(defconst blip-update-url "http://api.blip.pl/updates")
(defconst blip-dashboard-url "http://api.blip.pl/dashboard")

;;* const
(defconst blip-request-headers
  `(("X-Blip-API" . "0.02")
    ))

(defconst blip-time-string "%a %b %e %T %Y"
  "The format of blip time.")

;;* const
(defconst blip-at-regex "@\\([a-zA-Z0-9_]+\\)"
  "Regular expression to parse @messages.")

(defconst blip-hash-at-regex "\\([#@^>][>a-zA-Z0-9_.]+\\)"
  "Regular expression form for matching hashtags (#) and directions (^,>).")

(defconst blip-url-regex "\\(http://[a-zA-Z0-9.]+\.[a-zA-Z0-9%#;~/.=+&$,?@-]+\\)"
   "Regular expression for urls.")

(defconst blip-emacs-lisp-regex "\\([a-zA-Z0-9-.]+\\)\\.el"
  "Regex for Emacs Lisp files.")

;;* const
(defconst blip-max-blip 140 "Maximum length of a blip")

;;* const msg language
(defconst blip-too-long-msg
  (format "Post not sent because length exceeds %d characters"
          blip-max-blip))

;;* macro
(defmacro with-blip-buffer (buffer-name &rest forms)
  "Create a blip buffer with name BUFFER-NAME, and execute FORMS.

The value returned is the current buffer."
  `(with-current-buffer (get-buffer-create ,buffer-name)
     (buffer-disable-undo)
     (toggle-read-only 0)
     (delete-region (point-min) (point-max))
     ,@forms
     (set-buffer-modified-p nil)
     (toggle-read-only 1)
     (use-local-map blip-status-mode-map)
     (goto-char (point-min))
     (current-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General purpose library to wrap blip.pl's api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;* jsonparse
(defun blip-parse-json (url method)
  (let ((result nil)
	(url-request-method method)
	(url-request-extra-headers blip-request-headers)
	(url-show-status nil))
    (setq url-mime-accept-string "application/json")
    (save-window-excursion
      (set-buffer (url-retrieve-synchronously url))
      (let ((first-header-line (buffer-substring (goto-char (point-min))
						 (search-forward "\n"))))
	    (when (blip-header-error-p (blip-parse-header first-header-line))
	      (blip-display-error (list first-header-line))
	      (error "HTTP error on blip-parse-json: %s" (blip-get-header-error (blip-parse-header first-header-line)))))
      ; skip the rest of headers
      (search-forward-regexp "\\\[\\|{")
      (backward-char 1)
      (setq result (json-read))
      (kill-buffer (current-buffer)))
    result))

;;* jsonparse header
(defun blip-parse-header (header-frag)
  "Parse the HEADER-FRAG, and come back with some status codes.

This returns the HTTP status (for now) as a list of three elements.
 (HTTP/Version code Description)

The header fragment should be the first text node from the parsed
json.

The header fragment is actually quite important, it will tell us
if we have run into some kind of condition where we cannot
display blips or other information.  This will ease the fresh-install
pain where uesrs can't see why they have blank timelines."
  "Header format:  (or part we care about)" "HTTP/1.0 <status> <status text>\n"
  (string-match "HTTP/\\([0-9]+\\.[0-9]+\\) \\([1-5][0-9][0-9]\\) \\(.*\\)$"
                header-frag)
  (if (match-string 3 header-frag)
      (list (match-string 1 header-frag)
            (match-string 2 header-frag)
            (match-string 3 header-frag))
      (error "Malformed Header sent to blip-parse-header.   Header: %s"
             header-frag)))

;;* header helper
(defun blip-header-error-p (header)
   "Let us know if the HEADER is an error or not.  Null headers are errors."
   (and (not (null header))
		(<= 400 (string-to-number (cadr header)))))

;;* header display
(defun blip-get-header-error (header)
  "Given a parsed HEADER from `blip-parse-header', return human readable error."
  (if (null header)
      "Null header, probably an error with blip.el"
    (case (string-to-number (cadr header))
         ((200) "Everything is A OK!")
	 ((201) "Everything is A OK! Content was created.")
	 ((204) "Request was OK but there was no content.")
         ((400) "Bad Request.")
         ((401) "Not Authorized.")
         ((404) "Not Found.  404'ed!")
	 ((422) "Unprocessable Entity")
         ((503) "Server overloaded or too many requests. Try again later."))))

;;* header display
(defun blip-display-error (json)
  "Given an json fragment that contain an error, display it to the user."
  (let ((header (blip-parse-header (car json))))
	(when (blip-header-error-p header)
	   (blip-insert-with-overlay-attributes
		 "(_x___}<"
		 '((face "blip-fail-whale-face")))
	   (blip-insert-with-overlay-attributes
		  (concat "         HTTP ERROR!  "
				  "(" (cadr header) ") "
				  (caddr header) "\n\n"
				  "  "(blip-get-header-error header) "\n\n"
				  "  The response from blip was: "
				  "<PLACEHOLDER>"
;				  (format "%s" (xml-first-childs-value (cadr xml) 'error))
				  "\n\n")
		  '((face "blip-error-face"))))))

;;* write helper
(defun blip-write-title (title &rest args)
  "Helper function to write out a title bar for a blip buffer.

TITLE is the title to display, and it is formatted with ARGS."
  (blip-insert-with-overlay-attributes
   (concat (propertize "(b)" 'face 'blip-logo-face)
           " "
           (apply 'format title args)
	   "\n")
   '((face . "blip-title-face"))))

;;* helper write
(defun blip-insert-with-overlay-attributes (text attributes &optional prefix justify)
"Helper function to insert text into a buffer with an overlay.

Inserts TEXT into buffer, add an overlay and apply ATTRIBUTES to the overlay."
  (let ((start (point))
        )
    (insert text)
    (let ((overlay (make-overlay start (point))))
      (dolist (spec attributes)
        (overlay-put overlay (car spec) (cdr spec))))
    ))

;;* image var
(defvar blip-user-image-list 'nil
  "List containing all user images.")
;(setq blip-user-image-list 'nil)

;;* image
(defun blip-get-user-image (url user-id)
  "Retrieve the user image from the list, or from the URL.
USER-ID must be provided."
  (let ((img (assoc url blip-user-image-list)))
    (if (and img (not (bufferp (cdr img))))
        (cdr (assoc url blip-user-image-list))
        (if (file-exists-p (concat blip-user-image-dir
                                   "/" user-id "-"
                                   (file-name-nondirectory url)))
            (let ((img (create-image
                        (concat blip-user-image-dir ;; What's an ana for? lol
                                "/" user-id "-"
                                (file-name-nondirectory url)))))
              (add-to-list 'blip-user-image-list (cons url img))
              img)
            (let* ((url-request-method "GET")
                   (url-show-status nil)
                   (url-buffer (url-retrieve url 'blip-write-user-image
                                             (list url user-id))))
              (if url-buffer
		  (add-to-list 'blip-user-image-list (cons url url-buffer))
		(message "Warning, couldn't load %s " url))
              nil)))))

;;* image
(defun blip-write-user-image (status url user-id)
  "Called by `blip-get-user-image', to write the image to disk.

STATUS, URL and USER-ID are all set by `url-retrieve'."
  (let ((image-file-name
         (concat blip-user-image-dir
                 "/" user-id "-"
                 (file-name-nondirectory url))))
    (when (not (file-directory-p blip-user-image-dir))
      (make-directory blip-user-image-dir))
    (setq buffer-file-coding-system 'no-conversion)
    (setq buffer-file-name image-file-name)
    (goto-char (point-min))
    (delete-region (point-min) (search-forward "\C-j\C-j"))
    (save-buffer)
    (delete (cons url (current-buffer)) blip-user-image-list)
    (kill-buffer (current-buffer))
    (add-to-list 'blip-user-image-list (cons url (create-image image-file-name)))))


;;* jsonparse var
(defvar blip-async-buffer 'nil
  "Buffer that stores the temporary JSON results for blip.el.")

(defvar blip-times-through 0
  "The number of inserted blips used for zebra formatting")

(defun blip-show-recent-blips ()
  "Display blips visible from the dashboard. This includes Your status
updates, the people You follow, direct messages and update tips."
  (interactive)
  (pop-to-buffer
   (with-blip-buffer "*Blip-recent*"
		     (blip-write-title "Recent Blips")
		     (blip-write-recent-blips
		      (blip-parse-json blip-dashboard-url "GET")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for the interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* read post helper
(defun blip--query-for-post-update (&optional beg end length invert)
  (let* ((field-begin (field-beginning (point-max)))
         (field-end (field-end (point-max)))
         (field-length (- field-end field-begin))
         overlay)
    
    ;; remove old overlays
    (mapcar #'(lambda (overlay)
                (when (overlay-get overlay 'blip--query-for-post)
                  (delete-overlay overlay)))
            (overlays-in (point-min) (point-max)))
    
    ;; if necessary, add a new one
    (when (> field-length blip-max-blip)
      (setq overlay (make-overlay (+ field-begin blip-max-blip) field-end))
      (overlay-put overlay 'face 'blip-too-long-face)
      (overlay-put overlay 'blip--query-for-post t))
    
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "(\\(  0\\)/[0-9]+ characters)" nil t)
        (setq overlay (make-overlay (match-beginning 1)
                                    (match-end 1)))
        
        (overlay-put overlay 'blip--query-for-post t)
        (overlay-put overlay 'display (format "%3d" field-length))
        
        (let ((face
               `(:foreground
                 ,(if (<= field-length blip-max-blip) "green" "red"))))
          
          (when invert
            (setq face (append '(:inverse-video t) face)))
          
          (overlay-put overlay 'face face))))))

;;* read post helper
(defun blip--query-for-post-exit-minibuffer ()
  (interactive)
  (let* ((field-begin (field-beginning (point-max)))
         (field-end (field-end (point-max)))
         (field-length (- field-end field-begin)))
    
    (if (<= field-length blip-max-blip)
        (exit-minibuffer)
        (beep)
        (blip--query-for-post-update nil nil nil t)
        (sit-for 1)
        (blip--query-for-post-update nil nil nil))))

;;* read post helper
(defun blip--query-for-post-minibuffer-setup ()
  "Prepare the minibuffer for a blip entry.
Limit main field length to `blip-max-blip' characters" 
  (blip--query-for-post-update)
  (local-set-key [remap exit-minibuffer]
		 #'blip--query-for-post-exit-minibuffer)
  (add-hook 'after-change-functions
	    #'blip--query-for-post-update t t))

;;* read post
(defun blip-query-for-post (prompt-heading initial-input)
  "Query for a blip.pl post text in the minibuffer.

PROMPT-HEADING is the prompt, and has \" (140 char max): \" appended to it.
INITIAL-INPUT is what it is."
  (let ((minibuffer-setup-hook
         (cons #'blip--query-for-post-minibuffer-setup minibuffer-setup-hook)))
    (read-string (concat prompt-heading
                         (format " (  0/%3d characters): "
                                 blip-max-blip))
                 initial-input)))

;;* post query-buffer mode
(define-derived-mode blip-post-mode text-mode
  "Major mode for posting blips."
  (define-key 'blip-post-mode-map [(Control c) (Control c)] 'blip-post-))

;;* post query-buffer
(defun blip-buffer-up-for-post (prompt-heading initial-input)
  "Query for a post inside of a regular buffer instead of the minibuffer."
  (pop-to-buffer (get-buffer-create "*blip-post*"))
  (blip-post-mode)
  (insert blip-popup-buffer-message))

;;* post status
(defun blip-post-status (url post)
  "Post some kind of blip status message at URL with the content POST."
  (blip--post (url-hexify-string post)))

;;* helper internal blip posting
;; Based on http-twiddle-mode-send
(defun blip--post (content)
  "Post a status update to blip.
We can't use `url-retrieve' because 1xx response codes are not currently hadnled by it,
and blip won't accept headers without Expect: 100-continue and Content-Type multipart/form-data;.

CONTENT - text to send"
  ;; close any old connection
  (when blip--process
    (kill-buffer (process-buffer blip--process)))
  (setq blip--process
	(open-network-stream "blip-process" "*HTTP Blip*" "api.blip.pl" 80))
  (process-send-string blip--process
		       (format "POST /updates HTTP/1.1
Host: api.blip.pl
Accept: application/json
User-Agent: blip.el
Authorization: Basic %s
X-Blip-API: 0.02
Content-length: %s
Expect: 100-continue
Content-Type: multipart/form-data;

update[body]=%s"               (blip-get-auth)
			       (+ (length "update[body]=")
				  (length content))
			       content)))

;;* post interactive blip
;;;###autoload
(defun blip-post ()
  "Send a post to blip.pl.
Prompt the first time for password and username \(unless
`blip-user' and/or `blip-pass' is set\) and for the text of the
post; thereafter just for post text.  Posts must be <= `blip-max-blip' chars
long.
"
  (interactive)
  (let* ((post (blip-query-for-post "Post" "")))
    (if (> (length post) blip-max-blip)
        (error blip-too-long-msg)
        (blip-post-status blip-update-url post))))

;;* interactive nav
(defun blip-next-blip (&optional arg)
  "Move forward to the next blip.
 
With argument ARG, move to the ARGth next blip."
  (interactive "p")
  (mapc (lambda (n)
          (goto-char (next-single-char-property-change (point) 'blip-id nil
                                                       (point-max))))
        (number-sequence 1 (or arg 1))))

;;* interactive nav
(defun blip-previous-blip (&optional arg)
  "Move backward to the previous blip.
 
With argument ARG, move to the ARGth previous blip."
  (interactive "p")
  (mapc (lambda (n)
          (goto-char (previous-single-char-property-change (point) 'blip-id nil
                                                           (point-min))))
        (number-sequence 1 (or arg 1))))

(defun blip-get-blip-user-info (user-path)
  (blip-parse-json (concat blip-base-url user-path) "GET"))

(defun blip-get-user-avatar-url (user avatar-size)
  (let* ((avatars (blip-parse-json (concat blip-base-url
					   (cdr (assoc 'avatar_path user)))
				   "GET")))
    (cdr (assoc avatar-size avatars))))

(defun blip-write-blip (blip)
 "Insert a blip into the current buffer.
BLIP should be an json parsed node of a blip message."
 (let* ((blip-id   (cdr (assoc 'id blip)))
	(blip-type (cdr (assoc 'type blip)))
	(blip-user (blip-get-blip-user-info (cdr (assoc 'user_path blip))))
	(recipient (if (or (equal blip-type "DirectedMessage")
			   (equal blip-type "PrivateMessage"))
		       (blip-get-blip-user-info (cdr (assoc 'recipient_path blip)))
		     nil))
	(timestamp nil)
	(user-id   (cdr (assoc 'login    blip-user)))
;	(user-name (cdr (assoc 'login blip-user)))
	(location  (cdr (assoc 'location blip-user)))
	(user-img  (if blip-show-user-images
		       (if (equal blip-type "UpdateTip")
			   (blip-get-user-image "http://static3.blip.pl/images/notice.png" "UpdateTip")
			 (blip-get-user-image (concat blip-base-url
						      (blip-get-user-avatar-url blip-user 'url_50))
					      user-id))
		     nil))
	(recipient-img (if (and blip-show-user-images 
				(or (equal blip-type "DirectedMessage")
				    (equal blip-type "PrivateMessage")))
			   (blip-get-user-image (concat blip-base-url
							(blip-get-user-avatar-url recipient 'url_50))
						(cdr (assoc 'login recipient)))
			 nil))
	(message   (cdr (assoc 'body blip)))
	(src-info  (cdr (assoc 'name (cdr (assoc 'transport blip)))))
	(overlay-start 0)
	(overlay-end   0))
   (setq overlay-start (point))

   (when (and blip-show-user-images user-img)
     (insert " ")
     (insert-image user-img)
     (insert " "))

   (when (and blip-show-user-images recipient-img)
     (insert-image recipient-img))

   (insert " ")

   (when (cdr (assoc 'created_at blip)) ;; blips migh not have a timestamp
     (setq timestamp (format-time-string blip-time-string (date-to-time (cdr (assoc 'created_at blip))))))
   (if (not (equal blip-type "UpdateTip"))
       (progn
	 (blip-insert-with-overlay-attributes (concat user-id
						      (when recipient
							(concat 
							 (if (equal blip-type "PrivateMessage")
							     ">>"
							   ">")
							 (cdr (assoc 'login recipient)))))
					      '((face . "blip-message-face")) " ")
	 (insert ": ")))

   (blip-insert-with-overlay-attributes (blip-keymap-and-fontify-message message) '((face . "blip-message-face")) " ")
   (insert "\n")
   (when (or timestamp location src-info)
     (blip-insert-with-overlay-attributes
      (concat "                          "
	      (when timestamp timestamp)
	      (when location  (concat " @ " location))
	      (when src-info  (concat " ("  src-info ")"))
	      "\n")
      '((face . "blip-info-face")) nil 'right))
   (setq overlay-end (point))
   (let ((o (make-overlay overlay-start overlay-end)))
     (overlay-put o
		  'face (if (= 0 (% blip-times-through 2))
			    "blip-zebra-1-face"
			  "blip-zebra-2-face"))
     (overlay-put o 'blip-id blip-id)
     (overlay-put o 'blip-user user-id)))
 (incf blip-times-through))

;;* blips write last-blip
(defun blip-write-recent-blips (json-data)
  "Function that writes the recent blips to the buffer."
  (mapc 'blip-write-blip json-data)
  
  ;; go back to top so we see the latest messages
  (goto-address)
  (goto-char (point-min)))

;;* write helper
(defun blip-keymap-and-fontify-message (message)
  "Scan through MESSAGE, and fontify and keymap all #foo and @foo."
  (let ((original-txt (substring message 0))) ;; Just to be sure we're using a copy
    (when (string-match blip-hash-at-regex message) ;; usernames
      (setq message (replace-regexp-in-string
                     blip-hash-at-regex
                     (lambda (str)
                       (let ((type (substring str 0 1))
                             (thing (substring str 1)))
                         (setq str (propertize str
                                               'face 'blip-hash-at-face
                                               'pointer 'hand))
                         (when (string-equal "^" type)
                           (setq str (propertize str 'blip-user thing)))
                         (propertize str 'blip-search (concat type thing))
			 ))
                     message)))
    
    (when (string-match blip-url-regex message) ;; URLs
      (setq message (replace-regexp-in-string
                     blip-url-regex
                     (lambda (str)
                       (let ((map (make-sparse-keymap)))
                         (define-key map [enter] 'blip-visit-link)
                         (define-key map [(control) (enter)] 'blip-visit-link)
                         (define-key map [mouse-1] 'blip-visit-link)
                         (define-key map [mouse 2] 'blip-visit-link)
                         (define-key map [mouse-3] 'blip-visit-link)
                         (propertize str
                                     'face 'blip-url-face
                                     'pointer 'hand
                                     'blip-url str
                                     'keymap map)))
                     message)))
    (when (string-match blip-emacs-lisp-regex message) ;; .el's
      (setq message (replace-regexp-in-string
                     blip-emacs-lisp-regex
                     (lambda (str)
                       (propertize str
                                   'face 'blip-url-face
                                   'elisp str))
                     message)))

    ;; message content (plaintext)
    (propertize message 'blip-message original-txt)))

;;* property helper
(defun blip-get-text-property (propname)
  "Return a property named PROPNAME or nil if not available.

This is the reverse of `get-char-property', it checks text properties first."
  (or (get-text-property (point) propname)
      (get-char-property (point) propname)))

;;* mode
;;;###autoload
(define-minor-mode blip-minor-mode
    "Toggle blip-minor-mode, a minor mode that binds some keys for posting.
Globally binds some keys to Blip's interactive functions.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

\\{blip-minor-mode-map}" nil
" Blip"
'(("\C-c\C-ts" . blip-show-recent-blips))
  :global t
  :group 'blip
  :version blip-version-number)
(provide 'blip)
;;; blip.el ends here
