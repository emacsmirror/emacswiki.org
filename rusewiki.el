;;; rusewiki.el -- edit pages on an RuseWiki wiki using curl
;; Copyright 2007  rubikitch <rubikitch@ruby-lang.org>
;;
;; Based on oddmuse.el by Alex Schroeder
;;
;; $Id: rusewiki.el,v 1.3 2007/01/09 17:16:35 rubikitch Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; A simple mode to edit pages on RuseWiki wikis using Emacs and a
;; command-line HTTP client such as curl.

;; Since text formatting rules depend on the wiki you're writing for,
;; the font-locking can only be an approximation.

;; Put this file in a directory on your `load-path' and 
;; add this to your init file:
;; (autoload 'rusewiki-edit "rusewiki" "Edit a page on an RuseWiki wiki." t)
;; And then use M-x rusewiki-edit to start editing.

;;; History:

;; $Log: rusewiki.el,v $
;; Revision 1.3  2007/01/09 17:16:35  rubikitch
;; fixed a typo.
;;
;; Revision 1.2  2007/01/09 16:59:35  rubikitch
;; added How to Save
;;
;; Revision 1.1  2007/01/09 16:51:25  rubikitch
;; Initial revision
;;

;;; Code:

(defcustom rusewiki-wikis
  '(("Ruby" "http://wiki.rubygarden.org/Ruby/" iso-8859-1)
    ("Ruse" "http://wikis.onestepback.org/Ruse/" utf-8))
  "Alist mapping wiki names to URLs."
  :type '(repeat (list (string :tag "Wiki")
		       (string :tag "URL")
                       (string :tag "coding-system")))
  :group 'rusewiki)

(defcustom rusewiki-username user-full-name
  "Username to use when posting.
Setting a username is the polite thing to do."
  :type '(string)
  :group 'rusewiki)

(defcustom rusewiki-password ""
  "Password to use when posting.
You only need this if you want to edit locked pages and you
know an administrator password."
  :type '(string)
  :group 'rusewiki)

(defvar rusewiki-get-command
  "curl --silent %wpage/edit/%t | ruby -rcgi -e 'CGI.unescapeHTML(STDIN.read[ %r!<textarea.+?>(.+)</textarea>!m, 1]).display'"
  "Command to use for publishing pages.
It must print the page to stdout.

%w  URL of the wiki as provided by `rusewiki-wikis'
%t  URL encoded pagename, eg. HowTo, How_To, or How%20To")

(defvar rusewiki-post-command
  (concat "curl -c %T --silent"
          " --form 'author[name]=%u'"
          " --form 'author[password]=%p'"
          " http://wiki.rubygarden.org/login;"
          "curl -b %T --silent"
          " --form 'posting[summary]=%s'"
          " --form 'commit=Save'"
          " --form 'posting[content]=<-'"
          " '%wpage/update/%t'")

  "Command to use for publishing pages.
It must accept the page on stdin.

%w  URL of the wiki as provided by `rusewiki-wikis'
%t  pagename
%s  summary
%u  username
%p  password")

(defvar rusewiki-link-pattern
  "\\<[A-Z\xc0-\xde]+[a-z\xdf-\xff]+\\([A-Z\xc0-\xde]+[a-z\xdf-\xff]*\\)+\\>"
  "The pattern used for finding WikiName.")

(defvar rusewiki-wiki nil
  "The current wiki.
Must match a key from `rusewiki-wikis'.")

(defvar rusewiki-page-name nil
  "Pagename of the current buffer.")

(define-derived-mode rusewiki-mode text-mode "Ruse"
  "Simple mode to edit wiki pages.

Use \\[rusewiki-follow] to follow links. With prefix, allows you
to specify the target page yourself.

Use \\[rusewiki-post] to post changes. With prefix, allows you to
post the page to a different wiki.

Use \\[rusewiki-edit] to edit a different page. With prefix,
forces a reload of the page instead of just popping to the buffer
if you are already editing the page.

Customize `rusewiki-wikis' to add more wikis to the list.

\\{rusewiki-mode-map}"
  (font-lock-add-keywords
   nil
   '(("^ .+?$" . font-lock-comment-face)
     ("<\\(/?[a-z]+\\)" 1 font-lock-function-name-face)
     ("^[*#]\\([*#]+\\)" . font-lock-constant-face)
     ("^\\([*#]\\)[^*#]" 1 font-lock-builtin-face)))
  (font-lock-add-keywords
   nil
   (list (cons (symbol-value 'rusewiki-link-pattern)
	       'font-lock-keyword-face)))
  (font-lock-mode 1)
  (goto-address)
  (set (make-local-variable 'sgml-tag-alist)
       `(("b") ("code") ("em") ("i") ("strong") ("nowiki")
	 ("pre" \n) ("tt") ("u")))
  (set (make-local-variable 'skeleton-transformation) 'identity)
  (setq indent-tabs-mode nil))

(autoload 'sgml-tag "sgml-mode" t)
(define-key rusewiki-mode-map (kbd "C-c C-t") 'sgml-tag)
(define-key rusewiki-mode-map (kbd "C-c C-o") 'rusewiki-follow)
(define-key rusewiki-mode-map (kbd "C-c C-c") 'rusewiki-post)

(defun rusewiki-edit (wiki pagename)
  "Edit a page on a wiki.
WIKI is the name of the wiki as defined in `rusewiki-wikis',
PAGENAME is the pagename of the page you want to edit.
Use a prefix argument to force a reload of the page."
  (interactive
   (list (completing-read "Wiki: " rusewiki-wikis
			  nil t rusewiki-wiki)
	 (read-from-minibuffer "Pagename: ")))
  (let ((name (concat wiki ":" pagename)))
    (if (and (get-buffer name)
	     (not current-prefix-arg))
	(pop-to-buffer (get-buffer name))
      (let* ((command rusewiki-get-command)
             (triplet (assoc wiki rusewiki-wikis))
             (url (cadr triplet))
             (coding (caddr triplet))
             (buf (get-buffer-create name))
             (coding-system-for-read coding)
             (coding-system-for-write coding))
	(setq command (replace-regexp-in-string "%w" url command t t)
	      command (replace-regexp-in-string "%t" pagename command t t))
	(shell-command command buf)
	(pop-to-buffer buf)
	(rusewiki-mode)
	(set (make-local-variable 'rusewiki-wiki) wiki)
	(set (make-local-variable 'rusewiki-page-name) pagename)))))

(autoload 'word-at-point "thingatpt")

(defun rusewiki-follow (arg)
  "Figure out what page we need to visit
and call `rusewiki-edit' on it."
  (interactive "P")
  (let (pagename)
    (if arg
	(setq pagename (read-from-minibuffer "Pagename: "))
      (setq pagename (word-at-point))
      (unless (and pagename
		   (string-match (concat "^" rusewiki-link-pattern "$")
				 pagename))
	(save-excursion
	  (let* ((pos (point))
		 (start (search-backward "[[" nil t))
		 (end (search-forward "]]" nil t)))
	    (when (or (not start)
		      (not end)
		      (< end pos))
	      (error "No link found at point"))
	    (setq pagename (buffer-substring (+ start 2) (- end 2)))))))
    (rusewiki-edit (or rusewiki-wiki
		      (read-from-minibuffer "URL: "))
		  pagename)))

(defun rusewiki-post (summary)
  "Post the current buffer to the current wiki.
The current wiki is taken from `rusewiki-wiki'."
  (interactive "sSummary: ")
  ;; when using prefix or on a buffer that is not in rusewiki-mode
  (when (or (not rusewiki-wiki) current-prefix-arg)
    (set (make-local-variable 'rusewiki-wiki)
	 (completing-read "Wiki: " rusewiki-wikis nil t)))
  (when (not rusewiki-page-name)
    (set (make-local-variable 'rusewiki-page-name)
	 (read-from-minibuffer "Pagename: " (buffer-name))))
  (let* ((command rusewiki-post-command)
         (triplet (assoc rusewiki-wiki rusewiki-wikis))
         (url (cadr triplet))
         (coding (caddr triplet))
         (coding-system-for-read coding)
         (coding-system-for-write coding)
         (tempfile (make-temp-file "rusewiki")))
    (setq command (replace-regexp-in-string "%w" url command t t)
	  command (replace-regexp-in-string "%t" rusewiki-page-name command t t)
	  command (replace-regexp-in-string "%s" summary command t t)
	  command (replace-regexp-in-string "%u" rusewiki-username command t t)
	  command (replace-regexp-in-string "%p" rusewiki-password command t t)
          command (replace-regexp-in-string "%T" tempfile command t t))
    (shell-command-on-region (point-min) (point-max) command)
    (delete-file tempfile)))

(provide 'rusewiki)

;; How to save (DO NOT REMOVE!!)
;; (let ((oddmuse-wiki "EmacsWiki")(oddmuse-page-name "rusewiki.el")) (call-interactively 'oddmuse-post))
;;; rusewiki.el ends here
