;;; wiki-remote.el --- edit pages on a remote wiki

;; Copyright (C) 2001, 2002, 2003  Alex Schroeder <alex@gnu.org>

;; Version: 1.0.0
;; Keywords: hypermedia
;; Author: Alex Schroeder <alex AT gnu DOT org>
;; Maintainer: Kahlil (Kal) Hodgosn < kahlil AT tpg DOT com DOT au >
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?WikiRemote

;; This file is not part of GNU Emacs.
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;; Commentary:

;; This package allows you to load and save pages from a remote wiki
;; using either w3 or emacs-w3m functionality.  See `wiki-types' for a
;; list of supported wiki engines.  Just call `wiki-remote-get' and
;; enter page name and wiki name.  In order to save, hit C-c C-c.

;; At some point I may remove W3 functionality completely (since it
;; does not seem to be being maintained anymore) and much of the code
;; rewritten to support emacs-w3m more directly. Please contact me if
;; migrating to emacs-w3m would be a major inconvience -- Kahlil
;; Hodgson.

;;; Thanks:

;; Simon Michael for his zwiki-mode.el which inspired this package.

;; Vebjorn Ljosa <vebjorn@ljosa.com> for CLiki support.

;;; Changes:

;; * Kahlil Hodgson takes over as maintainer ....

;; * Added w3m compatibility (since w3 has gone the way of the dodo)

;; * Make buffer local variables permanent-local to get around the let
;; binding problem.

;; * Validating a save does not seem to work with emacs-w3m
;; anymore. Rearrange the semantics so that saving simply redisplays
;; the edited web-page so we can do a visual check.

;;; Code:

;; w3/w3m compatibility code (Kahlil Hodgson)
(eval-and-compile
  (unless (condition-case nil
	      (require 'w3)
	    (error nil))
    (require 'w3m)

    (defvar url-request-method nil)
    (defvar url-request-data nil)
    (defvar url-inhibit-mime-parsing nil)
    (defalias 'url-hexify-string 'w3m-url-encode-string)

    (defun url-retrieve (url)
      "Return a buffer containing the HTML contents of URL using w3m."
      (let ((w3m-async-exec nil)  ;; wait for output
	    (name "WikiEdit"))
	(save-excursion
	  (set-buffer (get-buffer-create name))
	  (erase-buffer) ;; flush previous edit
	  (w3m-retrieve url
			url-inhibit-mime-parsing
			'no-cache
			url-request-data)
	;; want the cdr of the return value to a buffer object
	  `(t . ,(current-buffer)))))))

(require 'easy-mmode); for easy-mmode-define-minor-mode
(require 'thingatpt); for word-at-point

;; Customization

(defgroup wiki-remote nil
  "Options concerning the editing of pages on remote wikis.
These pages are loaded and saved via the Internet; possibly via
the http or via ftp -- that depens upon the type of wiki engine
running the remote site."
  :group 'wiki)

(defcustom wiki-remote-setup-function 'ignore
  "Function to setup the wiki buffer when editing a remote wiki page.
Note that these functions should not be major modes.  A major mode
will kill all local variables, something that might prevent some
wiki types from working."
  :type '(choice (const ignore)
		 (const wiki-mode)
		 (const emacs-wiki-mode)
		 (function))
  :group 'wiki-remote)

(defcustom wiki-remote-types
  '((usemod usemod-get-page usemod-put-page)
    (zwiki zwiki-get-page zwiki-put-page)
    (cliki cliki-get-page cliki-put-page))
  "A list of wiki types supported and the functions used.
Each element has the form (TYPE GET PUT).

TYPE is a symbol used in `wiki-remote-alist' to denote the wiki engine.

GET is a function must accept the parameters PAGENAME and URL, and it
should return a buffer with the requested plain text in it.

PUT is a function that must accept paramaters PAGENAME, URL, CONTENT,
and save the content as the new pagename.

Usually both GET and PUT will use `url-retrieve' to do their job."
  :type '(repeat (list :tag "Type"
		       (symbol :tag "Name")
		       (function :tag " Get")
		       (function :tag " Put")))
  :group 'wiki-remote)

(defcustom wiki-remote-alist
  '(("Local" "http://localhost/cgi-bin/wiki.pl" usemod)
    ("Emacs" "http://www.emacswiki.org/cgi-bin/wiki.pl" usemod)
    ("UseMod" "http://www.usemod.com/cgi-bin/wiki.pl" usemod)
    ("Meatball" "http://www.usemod.com/cgi-bin/mb.pl" usemod)
    ("Zwiki" "http://joyful.com/zwiki/" zwiki)
    ("Cliki" "http://ww.telent.net/cliki/" cliki))
  "An alist where each element has the form (NAME URL TYPE).

NAME is the name of the wiki.

The URL is the base URL for a wiki.  If the URL of a page is
http://www.usemod.com/cgi-bin/wiki.pl?RecentChanges then the
URL you want is http://www.usemod.com/cgi-bin/wiki.pl.

TYPE is a symbol.  It must be a symbol from `wiki-remote-types'."
  :type `(repeat
	  (list :tag "Wiki"
	   (string :tag "Name")
	   (string :tag " URL")
	   (choice :tag "Type"
		   ,@(mapcar
		     (lambda (i)
		       (list 'const (car i)))
		     wiki-remote-types))))
  :group 'wiki-remote)

(defvar wiki-remote-debug nil "*Debug wiki-remote operations.")
;;(setq wiki-remote-debug nil)

;; Accessor functions

(defun wiki-remote-name ()
  "Query the user for a remote wiki name.
See also the variable `wiki-remote-name'."
  (completing-read "Wiki: " wiki-remote-alist))

(defun wiki-remote-name-by-url (url)
  "Determine the wiki name given the URL
See also the variable `wiki-remote-alist'."
  (let ((alist (mapcar (lambda (wiki)
			 (cons (nth 1 wiki) (nth 0 wiki)))
		       wiki-remote-alist)))
    (cdr (assoc url alist))))

(defun wiki-remote-url (name)
  "Return URL for NAME, based on `wiki-remote-alist'."
  (nth 1 (assoc name wiki-remote-alist)))
; (wiki-remote-url (wiki-remote-name))

(defun wiki-remote-type (name)
  "Return wiki type for NAME, based on `wiki-remote-alist'."
  (nth 2 (assoc name wiki-remote-alist)))
; (wiki-remote-type (wiki-remote-name))

(defun wiki-remote-get-function (type)
  "Return get function for wiki TYPE, based on `wiki-remote-types'."
  (nth 1 (assq type wiki-remote-types)))
; (wiki-remote-get-function 'usemod)

(defun wiki-remote-put-function (type)
  "Return put function for wiki TYPE, based on `wiki-remote-types'."
  (nth 2 (assq type wiki-remote-types)))
; (wiki-remote-put-function 'usemod)

;; Code

(defvar wiki-remote-name nil
  "Name of the wiki currently visited.
This should be a name from `wiki-remote-alist'.")
(make-variable-buffer-local 'wiki-remote-name)
(put 'wiki-remote-name 'permanent-local t)

;; Minor mode

(defvar wiki-remote-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'wiki-remote-put)
    (define-key map (kbd "C-c RET") 'wiki-remote-get)
    map)
  "Keymap used by `wiki-remote-mode'.")

(easy-mmode-define-minor-mode
 wiki-remote-mode
 "In `wiki-remote-mode' you can put the page visited back to wiki server
using \\[wiki-remote-put].  You can get new pages from the remote server
using \\[wiki-remote-get].  Use `wiki-remote-mode-on-hook' to add
customizations."
 nil
 " Remote"
 wiki-remote-mode-map)

;; Interfacing with wiki-mode

(add-hook 'wiki-mode-on-hook 'wiki-remote-maybe)

(defun wiki-remote-maybe ()
  "If the page is a remote page, set `wiki-follow-name-action'."
  (when wiki-remote-mode
    (set (make-local-variable 'wiki-follow-name-action)
	 'wiki-remote-get-nondirectory)))

(defun wiki-remote-get-nondirectory (page)
  "Strip current directory from PAGE and call `wiki-remote-get'."
  (wiki-remote-get (file-name-nondirectory page)))

;; Main function to call

(defun wiki-remote-get (page &optional name)
  "Get wiki PAGE from wiki NAME.

If optional argument URL is not given, it defaults to `wiki-url'.  This
variable is buffer local and holds the URL of the current buffer.  The
default will therefore get pages from the same buffer as the current
page.

If `wiki-url' is also nil, the user is queried for the wiki to use."
  (interactive
   (let* ((str (word-at-point))
	  (val (read-from-minibuffer
		(if str
		    (format "Get page (default %s): " str)
		  "Get page: ")
		str)))
     (list val)))
  (let* ((name (or name wiki-remote-name (wiki-remote-name)))
	 (url (wiki-remote-url name))
	 (type (wiki-remote-type name))
	 (get-func (wiki-remote-get-function type))
	 (buf (funcall get-func page url)))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n"))
    (rename-buffer page)
    (funcall wiki-remote-setup-function)
    (setq wiki-remote-name name)
    (wiki-remote-mode)
    (set-buffer-modified-p nil)))

(defun wiki-remote-put (&optional name)
  "Save current buffer back to the remote wiki.
The remote wiki can be specified via the optional argument NAME.
If NAME is not given, `wiki-remote-name' is used instead."
  (interactive)
  (let* ((name (or name wiki-remote-name (wiki-remote-name)))
	 (url (wiki-remote-url name))
	 (type (wiki-remote-type name))
	 (page (buffer-name))
	 (put-func (wiki-remote-put-function type))
	 (content (buffer-substring-no-properties
		   (point-min) (point-max))))
    (funcall put-func page url content)))

(defun wiki-remote-simple-verify-page (buf page)
  "Verify that buffer BUF actually contains PAGE.
This should catch all errors caused by the wiki engine
that do not result in a HTTP error."
  (save-excursion
    (set-buffer buf)
    (goto-char (point-min))
    (search-forward page nil t)))

(defun wiki-remote-massage (old new from to)
  "Massage buffer replacing the regexp OLD by string NEW.
Do this only between FROM and TO.  To may be a number
or a marker."
  (let ((start from)
	(end (copy-marker to)))
    (goto-char start)
    (while (re-search-forward old end t)
      (replace-match new))))

;; w3m support

(defun wiki-remote-get-w3m ()
  "Edit the current URL displayed in w3m."
  (interactive)
  (if (not w3m-current-url)
      (error "Not in a w3m buffer")
    (let ((params (split-string w3m-current-url "?"))
	  page url)
      (if (not (= 2 (length params)))
	  (error "Not a wiki URL of the form http://some.host/script?page")
	(setq page (nth 1 params)
	      url (nth 0 params))
	(wiki-remote-get page (wiki-remote-name-by-url url))))))

;; UseMod support

(defvar usemod-data nil
  "Data to store between reading and writing the page.")
(make-variable-buffer-local 'usemod-data)
;; else the let binding will prevent setting this ...
(put 'usemod-data 'permanent-local t)

(defun usemod-get-page (page url)
  "Return a buffer with the plain text contents of PAGE at URL."
    (let* ((data (usemod-get-data page url))
	   (text (cdr (assq 'text data)))
	   (buf (get-buffer-create page)))
      (set-buffer buf)
      (erase-buffer)
      (insert text)
      (setq usemod-data data)
      buf))

(defun usemod-refresh (page &optional name)
  "Refresh the current page."
  (interactive
   (let* ((str (or (file-name-nondirectory (buffer-file-name))
		   (buffer-name)))
	  (val (read-from-minibuffer
		(format "Get page (default %s): " str)
		str)))
     (list val)))
  (let* ((name (or name wiki-remote-name (wiki-remote-name)))
	 (url (wiki-remote-url name)))
    (setq wiki-remote-name name)
    (setq usemod-data (usemod-get-data page url)))
  (wiki-remote-mode 1))

(defun usemod-diff-current ()
  "Diff current buffer with meta data."
  (interactive)
  (when (null usemod-data)
    (error "Meta data missing: Must get this page before diffing"))
  (when (not (buffer-file-name))
    (error "Save this buffer to a file, first"))
  (save-buffer)
  (let ((text (cdr (assq 'text usemod-data)))
	(file (make-temp-name "/tmp/usemod")))
    (with-temp-buffer
      (insert text)
      (write-file file))
    (diff file (buffer-file-name))))

(defun usemod-get-data (page url)
  "Do the ugly work."
  (message "Reading %s at %s" page url)
  (save-excursion
    (let ((buf (cdr (url-retrieve (concat url "?action=edit&id="
					  (url-hexify-string page))))))
      (when (null buf)
	(error "Could not retrieve %s" url))
      (when wiki-remote-debug
	(switch-to-buffer-other-window buf))
      (set-buffer buf)
      (let* ((oldtime
	      (progn
		(goto-char (point-min))
		(search-forward-regexp
		 "NAME=\"oldtime\" VALUE=\"\\([0-9]+\\)\"")
		(match-string 1)))
	     (oldconflict
	      (progn
		(goto-char (point-min))
		(search-forward-regexp
		 "NAME=\"oldconflict\" VALUE=\"\\([0-9]+\\)\"")
		(match-string 1)))
	     (text
	      (progn
		(goto-char (point-min))
		(let ((start (search-forward-regexp "NAME=\"text\".*>"))
		      end)
		  (search-forward "</TEXTAREA>")
		  (setq end (copy-marker (match-beginning 0)))
		  (mapcar (lambda (pair)
			    (wiki-remote-massage (car pair) (cdr pair) start end))
			  '(("&gt;" . ">")
			    ("&lt;" . "<")
			    ("&quot;" . "\"")
;; If I have this then all I get is on big line!!
;; what's the rationale behind this?
;;;			    ("
;;;" . "")
			    ))
		  (buffer-substring start end)))))
	`((text . ,text)
	  (title . ,page)
	  (oldtime . ,oldtime)
	  (oldconflict . ,oldconflict))))))

(defun usemod-put-page (page url content)
  "Save buffer to wiki at URL with new CONTENT.

The PAGE name is ignored because the title is already stored in
`usemod-data'.  If that variable is nil, saving is not possible.

Queries for summary interactively and uses that as a summary line when
saving."
  (when (null usemod-data)
    (error "Meta data missing: Must get this page before editing"))
  (let ((url-request-method "POST")
	(url-request-data
	 (concat
	  "title=" (url-hexify-string page)
	  "&text=" (url-hexify-string content)
	  "&summary=" (url-hexify-string (read-from-minibuffer "Summary: " "*"))
	  (if (y-or-n-p "Is this a small edit? ") "&recent_edit=on" "")
	  "&oldtime=" (cdr (assq 'oldtime usemod-data))
	  "&oldconflict=" (cdr (assq 'oldconflict usemod-data))))
	;; Actually we don't want to see the result of the redirection!
	;; Therefore, just ignore it.
	(url-inhibit-mime-parsing t)
	(request url))
    (message "Saving...")
    (if (featurep 'w3m)
	(progn
	  ;; with w3m this does not always return the page
	  (url-retrieve request)
	  (message "Saving...done"))

      (let ((buf (cdr (url-retrieve request))))
	(message "Verifying...")
	(when wiki-remote-debug
	  (switch-to-buffer-other-window buf))
	(when (not (wiki-remote-simple-verify-page buf page))
	  (error "Save could not be verified"))))
    )

  (if (featurep 'w3m)
      (w3m-goto-url (concat url "?" page) 'reload)
    (message "Getting new meta data...")
    (setq usemod-data (usemod-get-data page url))
    (message "Getting new meta data...done")
    (when wiki-remote-debug
      (switch-to-buffer-other-window (cdr (assq 'buf usemod-data))))))

;; Zwiki support

(defun zwiki-get-page (page url)
  "Return a buffer with the plain text contents of PAGE at URL."
  (cdr (url-retrieve (concat url page "/text"))))

(defun zwiki-put-page (page url content)
  "Save buffer to wiki at URL with new CONTENT."
  (interactive)
  (unless (buffer-modified-p)
    (error "No changes need to be saved"))
  (let ((url-request-method "POST")
	(url-request-data
	 (concat
	  "---separator\nContent-Disposition: form-data; name=\"text\"\n\n"
	  content
	  "\n---separator--\n"))
	(url-request-extra-headers
	 '(("Content-type" . "multipart/form-data; boundary=-separator")))
	(request (concat url page "/edit")))
    (let ((buf (cdr (url-retrieve request))))
      (when wiki-remote-debug
	(switch-to-buffer-other-window buf))
      (when (not (wiki-remote-simple-verify-page buf page))
	(error "Save could not be verified")))))

;; Cliki support

(defvar cliki-author-name nil
  "Name of author; for the Recent Changes page.")

(defvar cliki-data nil
  "Data to store between reading and writing the page.")
(make-variable-buffer-local 'cliki-data)
(put 'cliki-data 'permanent-local t)

(defun cliki-get-page (page url)
  "Return a buffer with the plain text contents of PAGE at URL."
  (let* ((data (cliki-get-data page url))
	 (text (cdr (assq 'text data)))
	 (buf (get-buffer-create page)))
    (set-buffer buf)
    (erase-buffer)
    (insert text)
    (setq cliki-data data)
    buf))

(defun cliki-get-data (page url)
  "Do the ugly work."
  (message "Reading %s at %s" page url)
  (save-excursion
    (let ((buf (cdr (url-retrieve (concat url (url-hexify-string page)
					  "?edit")))))
      (when (null buf)
	(error "Could not retrieve %s" url))
      (when wiki-remote-debug
	(switch-to-buffer-other-window buf))
      (set-buffer buf)
      (let* ((page
	      (progn
		(goto-char (point-min))
		(let ((start (search-forward "<TITLE>CLiki : Edit ``"))
		      end)
		  (search-forward "''</TITLE>")
		  (setq end (copy-marker (match-beginning 0)))
		  (buffer-substring start end))))
	     (text
	      (progn
		(goto-char (point-min))
		(let ((start (search-forward "<textarea wrap=virtual name=text rows=20 cols=80>"))
		      end)
		  (search-forward "</textarea>")
		  (setq end (copy-marker (match-beginning 0)))
		  (mapcar (lambda (pair)
			    (wiki-remote-massage (car pair) (cdr pair) start end))
			  '(("&gt;" . ">")
			    ("&lt;" . "<")
			    ("&quot;" . "\"")
			    ("
" . "")))
		  (buffer-substring start end)))))
	`((text . ,text)
	  (title . ,page))))))

(defun cliki-put-page (page url content)
  "Save buffer to wiki at URL with new CONTENT.

The PAGE name is ignored because the title is already stored in
`cliki-data'.  If that variable is nil, saving is not possible.

Queries for summary interactively and uses that as a summary line when
saving."
  (when (null cliki-data)
    (error "Meta data missing: Must get this page before editing"))
  (let ((url-request-method "POST")
	(url-request-data
	 (concat
	  "name=" (or cliki-author-name
		      (setf cliki-author-name
			    (read-from-minibuffer "Your name: " "")))
	  "&text=" (url-hexify-string content)
	  "&summary=" (url-hexify-string (read-from-minibuffer "Summary: " "*"))))
	;; Actually we don't want to see the result of the redirection!
	;; Therefore, just ignore it.
	(url-inhibit-mime-parsing t)
	(request (concat url (url-hexify-string page))))
    (message "Saving...")
    (let ((buf (cdr (url-retrieve request))))
      (message "Verifying...")
      (when wiki-remote-debug
	(switch-to-buffer-other-window buf))
      (when (not (wiki-remote-simple-verify-page buf page))
	(error "Save could not be verified"))))
  (message "Getting new meta data...")
  (setq cliki-data (cliki-get-data page url))
  (when wiki-remote-debug
    (switch-to-buffer-other-window (cdr (assq 'buf cliki-data)))))

(provide 'wiki-remote)

;;; wiki-remote.el ends here
