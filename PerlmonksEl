;;; perlmonks.el --- A simple interface to www.perlmonks.org

;;; Copyright (C) (range 2011 'forever) by Terrence Brannon <metaperl@gmail.com>
;;; Acknowledgements: 
;;; - In #emacs: jlf, ashawley, cgroza, bpalmer, ivan-kanis, legumbre
;;; - For emacs: rms

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc.,   51 Franklin Street, Fifth Floor,
;;; Boston, MA  02110-1301  USA

;;; Commentary:

;;; Usage

;;; * Installation
;;; Download perlmonks.el from https://github.com/metaperl/emacs-perlmonks
;;; Put this file somewhere on your load-path and in your .emacs put:
;;; (require 'perlmonks)

;;; * Authentication
;;; The first time you use this mode, you need to call perlmonks-login
;;; which will set a cookie which will last for 10 years

;;; * Editing
;;; You initiate your use of this mode by opening up a file with a .monks 
;;; extension. This will open the file with major mode nxml and minor mode 
;;; perlmonks. From that point, you simply write a post using
;;; Perl Monks Approved HTML tags - http://perlmonks.org/index.pl?node_id=29281
;;; There are some convenience functions within perlmonks.el for editing:
;;; ** Editing Support
;;; perlmonks-paragraph
;;; perlmonks-code
;;; perlmonks-blockquote

;;; * Posting
;;; Once your post is complete, perlmonks.el has support for posting to the
;;; following areas:
;;;  M-x perlmonks-seekers-of-perl-wisdom
;;;  M-x perlmonks-meditation
;;;  M-x perlmonks-reply
;;; Just call one of those commands while in your .monks buffer


(require 'menu-bar)

(add-to-list 'auto-mode-alist '("\\.monks\\'" . nxml-mode))

(setq debug-on-error t)

;;; Code:

(require 'url)

; http://opensource.hld.ca/trac.cgi/browser/trunk/config/emacs/.elisp/w3/url-cookie.el?rev=80
(defun url-cookie-p (obj)
  (and (vectorp obj) (= (length obj) 7) (eq (aref obj 0) 'cookie)))
; another fix has to do with fixing the defstruct url-cookie in url-cookie.el
; similar to this patch http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-05/msg00624.html

;(add-to-list 'url-cookie-trusted-urls ".*perlmonks.org.*")
(setq url-cookie-trusted-urls '(".*perlmonks.*"))

(require 'perlmonks-auth)




;;;###autoload
(defgroup perlmonks nil
  "Perlmonks -- perlmonks.org client"
  :tag "Perlmonks"
  :group 'tools)

(defun metaperl/clipboard-as-string ()
  (with-temp-buffer
    (clipboard-yank)
    (buffer-string)))
    

(defun epm-http-post (url args)
  (interactive)
  "Send ARGS to URL as a POST request."
      (let ((url-request-method "POST")
            (url-request-extra-headers
             '(("Content-Type" . "application/x-www-form-urlencoded")))
            (url-request-data
             (mapconcat (lambda (arg)
                          (concat (url-hexify-string (car arg))
                                  "="
                                  (url-hexify-string (cdr arg))))
                        args
                        "&")))
        ;; if you want, replace `my-switch-to-url-buffer' with `my-kill-url-buffer'
        (url-retrieve url 'my-switch-to-url-buffer)))

    (defun my-kill-url-buffer (status)
      "Kill the buffer returned by `url-retrieve'."
      (kill-buffer (current-buffer)))

    (defun my-switch-to-url-buffer (status)
      "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
      (switch-to-buffer (current-buffer)))

; irc.freenode.net, #emacs
; [14:08] <jlf> er,  (interactive "sString1:\nsString2:") or somesuch


(defun perlmonks-login (username password)
  "Login to perlmonks.org with USERNAME and PASSWORD and set a cookie which will
expire in 10 years."
  (interactive "sUsername? \nsPassword? ")
  (interactive)
  (epm-http-post "http://www.perlmonks.org"
		 `(
		   ("node_id"	. "109")
		   ("op"	. "login")
		   ("user" . 	,username)
		   ("passwd" .	,password)
		   ("expires"	. "+10y")
		   ("sexisgood"	. "submit")
		   (".cgifields" .	"expires"))
		 )
  (url-cookie-write-file)
  )

(defun perlmonks-paragraph ()
  "Insert <p> tags"
  (interactive)
  (insert "
<P>



</P>

")
  (previous-line 4)
  (insert "   "))

(defun perlmonks-code ()
  "Insert <p> tags"
  (interactive)
  (insert "

<CODE>
</CODE>

")
  (previous-line 2)
  (open-line 1)
)


(defun perlmonks-reply (node-title reply-url)
  "Post current buffer as a reply to a node on perlmonks.org. NODE-TITLE will be the title. The REPLY-URL is the url resulting from clicking on 'Reply' or 'Comment'. E.g.

If you visited this node:
http://perlmonks.org/index.pl?node_id=357506

and clicked on 'Comment', you would be at the following REPLY-URL:
http://perlmonks.org/index.pl?parent=357506;node_id=3333

whereas if you had clicked on the 'Reply' below the first comment, you would have this
REPLY-URL:
http://perlmonks.org/index.pl?parent=357638;node_id=3333
"
  (interactive "sNode title? \nsReply url? ")
  (save-some-buffers nil)
  (let* ((msg-text (buffer-substring (point-min) (point-max)))
;	 (reply-url (current-kill 0))
	 (parent-node (progn
			(string-match "parent=\\([0-9]+\\)" reply-url)
			(match-string 1 reply-url))))
    (epm-http-post "http://www.perlmonks.org"
		   `(
		     ("node_id"	. "3333")
		     ("note_parent_node" . ,parent-node)
		     ("type"	. "note")
		     ("node" . 	,node-title)
		     ("note_doctext" .	,msg-text)
		     ("op" .	"create"))
		 )))


(defun perlmonks-seekers-of-perl-wisdom (node-title)
  "Post current buffer to Seekers of Perl Wisdom on perlmonks.org with NODE-TITLE"
  (interactive "sNode title? ")
  (save-some-buffers nil)
  (let ((msg-text (buffer-substring (point-min) (point-max))))
    (epm-http-post "http://www.perlmonks.org"
		 `(
		   ("node_id"	. "479")
		   ("type"	. "perlquestion")
		   ("node" . 	,node-title)
		   ("perlquestion_doctext" .	,msg-text)
		   ("op" .	"create"))
		 )))


(defun perlmonks-meditation (node-title)
  "Post current buffer to Meditations on perlmonks.org with NODE-TITLE"
  (interactive "sNode title? ")
  (save-some-buffers nil)
  (let ((msg-text (buffer-substring (point-min) (point-max))))
    (epm-http-post "http://www.perlmonks.org"
		 `(
		   ("node_id"	. "480")
		   ("type"	. "perlmeditation")
		   ("node" . 	,node-title)
		   ("perlmeditation_doctext" .	,msg-text)
		   ("op" .	"create"))
		 )))

(defun perlmonks-blockquote (text)
   (interactive "sText to quote")
   (insert "
<blockquote><i>


</i></blockquote>

")
 )


(defun perlmonks-blockquote ()
   (interactive)
   (kill-region (point) (mark))
   (insert "\n<blockquote><i>\n    ")
   (yank)
   (insert "\n</i></blockquote>\n\n")
 )

(defun check-cookies ()
  (interactive)
  (dired url-cookie-file)
)


(provide 'perlmonks)

