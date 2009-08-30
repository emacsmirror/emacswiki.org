;;;  tumbl.el --- Post to tumblr.com from emacs
;; Copyright (C) 2007 Carl Groner

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; Author: Carl Groner <c.groner at gmail dot com>
;; Created: 29 Oct 2007
;; Version: %Id%

;;; Commentary: 
;; tumbl.el allows posting to tumblr.com through the api the
;; tumblr.com service provides. See http://www.tumblr.com/api for more 
;; info about the tumblr.com api service. 

;; Installation:
;; You will need to register with tumblr.com before use.
;; tumbl.el require http-post-simple which can be found at
;; http://www.emacswiki.org/cgi-bin/wiki/http-post-simple.el 

;; Description:
;; Interactive functions:
;; tumbl-post-buffer-text 
;;     Post complete buffer text to your tumblr log as 
;;     'regular' post type. Prompts for buffer name and title
;; tumbl-post-region-text
;;     Post region as 'regular' post. Prompts for title
;; tumbl-post-region-as-quote
;;     Post region as tumblr quote. Prompts for quote source.
;; tumbl-post-region-as-conversation
;;     Post region as tumblr conversation. Prompts for title.
;; tumbl-post-link-url
;;     Post new link. The url is captured from the current 
;;     region. Prompts for link name and description
;; tumbl-post-link-text
;;     Post new link. The link name is captured from the current 
;;     region. Prompts for link URL and description

;;; Code:

; from http://www.emacswiki.org/cgi-bin/wiki/http-post-simple.el 
(require 'http-post-simple)

(defvar tumbl-write-url "http://www.tumblr.com/api/write"
  "The Tumblr Write API URL")
(defvar tumbl-generator-id "tumbl.el"
  "The generator name reported to the Tumblr API servie")

(defgroup tumbl nil
  "Access the tumblr.com api from emacs"
  :group 'emacs)

(defcustom tumbl-username nil
  "tumble.com username, leave at nil to be prompted"
  :group 'tumbl :type 'string)

(defcustom tumbl-password nil
  "tumblr.com password, leave at nil to be prompted"
  :group 'tumbl :type 'string)

; 'Regular' tumblr posts
(defun tumbl-post-buffer-text (buf post-title)
  "post entire buffer text to tumblr as a 'regular' post."
  (interactive "bBuffer: \nsTitle: ")
  (set-buffer buf)
  (tumbl-make-reg-post post-title (buffer-string)))

(defun tumbl-post-region-text(point mark post-title)
  "post region as 'regular' post."
  (interactive "r \nsTitle: ")
  (let ((text (buffer-substring-no-properties point mark)))
    (tumbl-make-reg-post post-title text)))

(defun tumbl-make-reg-post(post-title post-body)
  "Creates a 'regular' tumblr article using the title and body provided"
  (if (tumbl-confirm-post post-body)
      (tumbl-do-post-helper 
       tumbl-write-url
       (append (tumbl-get-standard-parms)
	       (list (cons 'type "regular")
		     (cons 'title post-title) 
		     (cons 'body post-body))))))

; Post quote
(defun tumbl-post-region-as-quote(point mark source)
  "post region as quote"
  (interactive "r \nsQuote Source (optional): ")
  (let ((quote (buffer-substring-no-properties point mark)))
    (tumbl-make-quote-post quote source)))

(defun tumbl-make-quote-post(quote source)
  "Creates a 'quote' tumblr article"
  (if (tumbl-confirm-post quote)
      (tumbl-do-post-helper
       tumbl-write-url
       (append (tumbl-get-standard-parms)
	       (list (cons 'type "quote")
		     (cons 'quote quote) 
		     (cons 'source source))))))
		
; Post conversation
(defun tumbl-post-region-as-conversation(point mark title)
  "post region as conversation"
  (interactive "r \nsTitle (optional): ")
  (let ((conv (buffer-substring-no-properties point mark)))
    (tumbl-make-conversation-post conv title)))

(defun tumbl-make-conversation-post(conv title)
  "Creates a 'conversation' tumblr article"
  (if (tumbl-confirm-post conv)
      (tumbl-do-post-helper
       tumbl-write-url
       (append (tumbl-get-standard-parms)
	       (list (cons 'type "conversation")
		     (cons 'conversation conv) 
		     (cons 'title title))))))

; Post links
(defun tumbl-post-link-url(point mark name desc)
  "Post link from url in region"
  (interactive "r \nsName: \nsDescription: ")
  (let ((lurl (buffer-substring-no-properties point mark)))
    (tumbl-make-link-post name lurl desc)))

(defun tumbl-post-link-text(point mark url desc)
  "Post link from text in region, prompted for url"
  (interactive "r \nsUrl: \nsDescription: ")
  (let ((ltext (buffer-substring-no-properties point mark)))
    (tumbl-make-link-post ltext url desc)))

(defun tumbl-make-link-post (name url desc)
  "Create link post"
  (if (tumbl-confirm-post url)
      (tumbl-do-post-helper
       tumbl-write-url
       (append (tumbl-get-standard-parms)
	       (list (cons 'type "link")
		     (cons 'name name)
		     (cons 'url url)
		     (cons 'description desc))))))

; Helper functions
(defun tumbl-do-post-helper (url options)
  "Sends http post to url"
  (setq response (http-post-simple url options))
  (if (or (not (listp response)) (< (list-length response) 3))
      (message "Unknown response, post failed!")
    (tumbl-show-post-status (third response))))

(defun tumbl-show-post-status(rsp) 
  "Display message indicating post results to user"
  (message 
   (cond ((eq rsp 200) "%s OK, No post created!")
	 ((eq rsp 201) "%s Success, post created")
	 ((eq rsp 400) "%s Bad request, post failed!")
	 ((eq rsp 403) "%s Forbidden, Tumblr Authentication failed!")
	 (t "%s: Unknown return code! Posting probably failed"))
   rsp))

(defun tumbl-confirm-post(contents)
  "promt user to confirm before posting"
  (y-or-n-p (format "Post : \" %.20s...\"" contents)))

(defun tumbl-get-standard-parms ()
  "Generate list of parms needed for all post requests.
   Includes generator-id, username and password"
  (list (cons 'generator tumbl-generator-id)
	(cons 'email (tumbl-auth-username))
	(cons 'password (tumbl-auth-password))))

(defun tumbl-auth-username()
  "Return username, prompt user if nil"
  (if (not tumbl-username)
      (read-from-minibuffer "Tumblr username: ")
    tumbl-username))

(defun tumbl-auth-password()
  "Return password, prompt user if nil"
  (if (not tumbl-password)
      (read-passwd "Tumblr Password: ")
    tumbl-password))

(provide 'tumbl)
;;; tumbl.el ends here.
