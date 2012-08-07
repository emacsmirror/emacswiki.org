;;; wiki.el --- hypertext authoring the WikiWay

;; Copyright (C) 2001, 2002, 2012  Alex Schroeder <alex@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: wiki.el
;; Version: 2.1.10
;; Keywords: hypermedia
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Description: Hypertext authoring the WikiWay
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?WikiMode

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Wiki is a hypertext and a content management system: Normal users are
;; encouraged to enhance the hypertext by editing and refactoring
;; existing pages and by adding more pages.  This is made easy by
;; requiring a certain way of writing pages.  It is not as complicated
;; as a markup language such as HTML.  The general idea is to write
;; plain ASCII.  Word with mixed case such as ThisOne are WikiNames --
;; they may be a Link or they may not.  If they are, clicking them will
;; take you to the page with that WikiName; if they are not, clicking
;; them will create an empty page for you to fill out.

;; This mode does all of this for you without using a web browser, cgi
;; scripts, databases, etc.  All you need is Emacs!  In order to
;; install, put wiki.el on you load-path, and add the following to your
;; .emacs file:

;; (require 'wiki)

;; This will activate WikiMode for all files in `wiki-directories' as soon
;; as they are opened.  This works by adding `wiki-maybe' to
;; `find-file-hooks'.

;; Emacs provides the functionality usually found on Wiki web sites
;; automatically: To find out how many pages have links to your page,
;; use `grep' or `dired-do-search'.  To get an index of all wikis, use
;; `dired'.  To keep old versions around, use `version-control' or use
;; `vc-next-action'.  To edit wikis, use Emacs!

;; You can publish a wiki using `wiki-publish', or you can use
;; `dired-do-wiki-publish' to publish marked wikis from dired, or you
;; can use `wiki-publish-all' to publish all wikis and write an index
;; file.  This will translate your plain text wikis into HTML according
;; to the rules defined in `wiki-pub-rules'.

;; Find out more: Take a look at http://c2.com/cgi/wiki?StartingPoints

;;; XEmacs

;; XEmacs users will have to get easy-mmode.el, I'm afraid.  You should
;; be able to get easy-mmode.el from a web site carrying the Emacs
;; sources.  It loads fine, so that shouldn't be a problem.  Put
;; `easy-mmode' somewhere in your load-path and install as follows:

;; (require 'easy-mmode)
;; (require 'wiki)

;;; What about a Major Mode?

;; By default, wiki files will be in `fundamental-mode'.  I prefer to be
;; in `text-mode', instead.  You can do this either for all files that
;; have WikiNames by changing `auto-mode-alist', or you can make
;; text-mode the default mode instead of fundamental mode.  Example:

;; (setq default-major-mode 'text-mode)

;; This puts wiki files in `text-mode'.  One problem remains, however.
;; Text mode usually means that the apostrophe is considered to be part
;; of words, and some WikiNames will not be highlighted correctly, such
;; as "WikiName''''s".  In that case, change the syntax table, if you
;; don't mind the side effects.  Example:

;; (modify-syntax-entry ?' "." text-mode-syntax-table)

;;; Thanks

;; Frank Gerhardt <Frank.Gerhardt@web.de>, author of the original wiki-mode.
;;   His latest version is here: http://www.s.netic.de/fg/wiki-mode/wiki.el
;; Thomas Link <t.link@gmx.at>
;; John Wiegley <johnw@gnu.org>, author of emacs-wiki.el.
;;   His latest version is here: http://www.emacswiki.org/emacs/EmacsWikiMode
;; and evolved into Emacs Muse: http://www.emacswiki.org/emacs/EmacsMuse

 

;;; Code:

(require 'easy-mmode); for easy-mmode-define-minor-mode
(require 'info); for info-xref face
(require 'thingatpt); for thing-at-point-looking-at and other things
(require 'compile); for grep-command
(load "goto-addr" t t); optional, for goto-address-mail-regexp

;; Options

(defgroup wiki nil
  "Options controlling the behaviour of Wiki Mode.
See `wiki-mode' for more information.")

(defcustom wiki-directories (list (expand-file-name "~/Wiki/"))
  "List of directories where all wiki files are stored.
The directories should contain fully expanded directory names and they
should end with a slash on most systems, because each element in the
list is compared to the return value of `file-name-directory'.  And that
function returns trailing slashes.  Use `expand-file-name' to expand
directory names if necessary."
  :group 'wiki
  :type '(repeat directory))

(defcustom wiki-extension nil
  "Extension to use for saved files.
A typical extension to use would be \"txt\"."
  :group 'wiki
  :type '(choice (const :tag "none" nil)
		 (const :tag "Text" "txt")))

(defcustom wiki-pub-directory "~/WebWiki"
  "Directory where all wikis are published to.
If set to nil, publishing is disabled."
  :group 'wiki
  :type '(choice directory
                 (const :tag "Disable publishing" nil)))

(defcustom wiki-pub-rules
  (list
   ;; inhibit HTML
   '("<" . "&lt;")
   '(">" . "&gt;")
   ;; strong, WardWiki uses italics instead of strong
   '("''\\(\\(\\|.\\)*\\)''" . "<strong>\\1</strong>")
   ;; empty lines at the beginning of the buffer
   '("\\`\n*" . "<p>\n")
   ;; plain paragraphs
   '("\n\n+" . "\n\n<p>\n")
   ;; list items
   '("^\\*[ \t]*" . "<li>")
   ;; list paragraphs
   '("<p>\n<li>\\(\\([^\n]\n?\\)+\\)" . "<p>\n<ul>\n<li>\\1</ul>\n")
   ;; preformatted paragraphs
   '("<p>\n\\([ \t]+\\([^\n]\n?\\)+\\)" . "<p>\n<pre>\n\\1</pre>\n")
   ;; quoted/indented paragraphs
   '("<p>\n:\\(\\([^\n]\n?\\)+\\)" . "<blockquote>\n<p>\n\\1</blockquote>\n")
   ;; URL
   (cons thing-at-point-url-regexp
	 "<a href=\"\\&\">\\&</a>")
   ;; email addresses without mailto
   (cons (if (boundp 'goto-address-mail-regexp)
	     goto-address-mail-regexp
	   nil)
	 "<a href=\"mailto:\\&\">\\&</a>")
   ;; wiki names
   'wiki-replace-links
   ;; header
   '(beginning-of-buffer . "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\">
<html>
<head>
<title><?name></title>
</head>
<body>
<h1><?name></h1>
")
   '("<\\?name>" . wiki-page-name)
   ;; footer
   '(end-of-buffer . "<hr>
<p>
Last change: <?date>
</body>
</html>")
   '("<\\?date>" . wiki-current-date))
  "List of rules to apply to a wiki page when publishing.
A RULE has one of the following forms:

FUNC

FUNC is a function that will be called once, without any parameters.
The function can do any search and replace it wants.

\(ACTION . REPLACEMENT)

ACTION is either a regular expression to search for using
`re-search-forward', or a function that will be called once, without any
parameters.

REPLACEMENT is either a string or a function returning a string.  This
string will be used as a replacement using `replace-match' if ACTION is
a regular expression, or it will be inserted if ACTION is a function.

Some examples:

  'wiki-replace-links
  '(\"<\" . \"&lt;\")
  '(end-of-buffer . \"</body></html>\")
  '(\"#NAME#\" . wiki-page-name)

All this is done in `wiki-pub-apply-rules'.  The rules are
applied in order, one rule at a time.  Note that case is never ignored.
`case-fold-search' will allways be bound to nil."
  :group 'wiki
  :type '(repeat (choice :value ("regexp" . "newtext")
			 (cons :tag "Rule"
			       (choice
				(regexp :tag "Search a regexp")
				(function :tag "Call a function to place point"
					  :value end-of-buffer))
			       (choice
				(string :tag "Insert or replace a string"
					:value "newtext")
				(function :tag "Insert or replace a function"
					  :value current-time-string)))
			 (function :tag "Function"
				   :value current-time-string))))

(defcustom wiki-maintainer "mailto:Unknown Maintainer"
  "URL where the maintainer can be reached."
  :group 'wiki
  :type 'string)

(defcustom wiki-date-format "%Y-%m-%d"
  "Format of current date for `wiki-current-date'.
This string must be a valid argument to `format-time-string'."
  :group 'wiki
  :type 'string)

(defcustom wiki-pub-file-name-suffix ".html"
  "This suffix will be appended to all wiki names when publishing."
  :group 'wiki
  :type 'string)

(defcustom wiki-index-file-name "index"
  "Filename of the Wiki Index page.
`wiki-pub-file-name-suffix' will be appended."
  :group 'wiki
  :type 'string)

(defcustom wiki-highlight-buffer-hook '(wiki-highlight-wiki-names)
  "Hook with functions to call when a buffer is highlighted."
  :group 'wiki
  :type 'hook)

(defgroup wiki-link nil
  "Options controlling links in Wiki Mode."
  :group 'wiki)

(defcustom wiki-name-regexp "\\<[A-Z][a-z]+\\([A-Z][a-z]+\\)+\\>"
  "Regexp matching WikiNames.
Whenever the regexp is searched for, case is never ignored:
`case-fold-search' will allways be bound to nil.

See `wiki-no-name-p' if you want to exclude certain matches.
See `wiki-name-no-more' if highlighting is not removed correctly."
  :group 'wiki-link
  :type 'regexp)
  
(defcustom wiki-name-no-more "[A-Za-z]+"
  "Regexp matching things that might once have been WikiNames.
Usually that amounts to all legal characters in `wiki-name-regexp'.
This is used to remove highlighting from former WikiNames."
  :group 'wiki-link
  :type 'regexp)

(defcustom wiki-highlight-name-exists 'wiki-name-exists-p
  "Function to call in order to determine wether a WikiName exists already.
This is used when highlighting words using `wiki-highlight-match': If
the word is a non-existing wiki-name, a question mark is appended.

See `wiki-name-regexp' for possible names considered a WikiName."
  :group 'wiki-link
  :type 'function)

(defcustom wiki-follow-name-action 'find-file
  "Function to use when following references.
The function should accept a string parameter, the WikiName.
If the WikiName exists as a file in `wiki-directories', the
fully qualified filename will be passed to the function."
  :group 'wiki-link
  :type 'function)

(defgroup wiki-parse nil
  "Options controlling parsing of the wiki files.
These function only come in handy if you want to do complex things such
as find clusters in the graph or generate a structured table of contents."
  :group 'wiki)

(defcustom wiki-include-function t
  "Function to decide wether to include a file in the `wiki-filter', or t.
If t, then all pages will be included.
The function should accept a filename and a wiki structure as returned
by `wiki-parse-files' as arguments and return non-nil if the file is to
be part of the graph."
  :group 'wiki-parse
  :type '(choice (const :tag "All pages" t)
		 (const :tag "Significant fan out" wiki-significant-fan-out)
		 function))

(defcustom wiki-significant-fan-out 3
  "Pages with a fan out higher than this are significant.
This is used by `wiki-significant-fan-out' which is a
possible value for `wiki-include-function'."
  :group 'wiki-parse
  :type 'integer)

;; Starting up

(defsubst wiki-page-name ()
  "Return page name."
  (file-name-nondirectory (file-name-sans-extension buffer-file-name)))

(defun wiki-no-name-p ()
  "Return non-nil if point is within a URL.
This function is faster than checking using `thing-at-point-looking-at'
and `thing-at-point-url-regexp'.  Override this function if you do not
like it."
  (let ((pos (point)))
    (and (re-search-backward "[]\t\n \"'()<>[^`{}]" nil t)
	 (goto-char (match-end 0))
	 (looking-at thing-at-point-url-regexp)
	 (<= pos (match-end 0)))))

(defun wiki-name-p (&optional shortcut)
  "Return non-nil when `point' is at a true wiki name.
A true wiki name matches `wiki-name-regexp' and doesn't trigger
`wiki-no-name-p'.  In addition to that, it may not be equal to the
current filename.  This modifies the data returned by `match-data'.

If optional argument SHORTCUT is non-nil, we assume that
`wiki-name-regexp' has just been searched for.  Note that the potential
wiki name must be available via `match-string'."
  (let ((case-fold-search nil))
    (and (or shortcut (thing-at-point-looking-at wiki-name-regexp))
	 (or (not buffer-file-name)
	     (not (string-equal (wiki-page-name) (match-string 0))))
	 (not (save-match-data
		(save-excursion
		  (wiki-no-name-p)))))))

(defun wiki-maybe ()
  "Maybe turn `wiki-mode' on for this file.
This happens when the file's directory is a member of
`wiki-directories'."
  (if (member (file-name-directory buffer-file-name)
              wiki-directories)
      (wiki-mode 1)
    (wiki-mode 0)))

(add-hook 'find-file-hooks 'wiki-maybe)

(defun wiki-install ()
  "Install `wiki-highlight-word-wrapper'."
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 'wiki-highlight-word-wrapper))

(defun wiki-deinstall ()
  "Deinstall `wiki-highlight-word-wrapper'."
  (setq after-change-functions (delq 'wiki-highlight-word-wrapper
				     after-change-functions)))

;; The minor mode (this is what you get)

(defvar wiki-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'wiki-follow-name-at-point)
    (if (featurep 'xemacs)
	(define-key map (kbd "<button2>") 'wiki-follow-name-at-mouse)
      (define-key map (kbd "<mouse-2>") 'wiki-follow-name-at-mouse))
    map)
  "Local keymap used by wiki minor mode while on a WikiName.")

(defvar wiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'wiki-highlight-buffer)
    (define-key map (kbd "C-c C-p") 'wiki-publish)
    (define-key map (kbd "C-c C-v") 'wiki-view-published)
    (define-key map (kbd "C-c C-b") 'wiki-backlink)
    (define-key map (kbd "C-c =") 'wiki-backup)
    (define-key map (kbd "<tab>") 'wiki-next-reference)
    (define-key map (kbd "M-n") 'wiki-next-reference)
    (define-key map (kbd "M-p") 'wiki-previous-reference)
    map)
  "Keymap used by wiki minor mode.")

(easy-mmode-define-minor-mode
 wiki-mode
 "Wiki mode transform all WikiNames into links.

Wiki is a hypertext and a content management system: Normal users are
encouraged to enhance the hypertext by editing and refactoring existing
wikis and by adding more.  This is made easy by requiring a certain way
of writing the wikis.  It is not as complicated as a markup language
such as HTML.  The general idea is to write plain ASCII.

Words with mixed case such as ThisOne are WikiNames.  WikiNames are
links you can follow.  If a wiki with that name exists, you will be
taken there.  If such a does not exist, following the link will create a
new wiki for you to fill.  WikiNames for non-existing wikis have a `?'
appended so that you can see wether following the link will give you any
informatin or not.

In order to follow a link, hit RET when point is on the link, or use
mouse-2.

All wikis reside in `wiki-directories'.

\\{wiki-mode-map}"
 nil
 " Wiki"
 wiki-mode-map)

(add-hook 'wiki-mode-on-hook 'wiki-install)
(add-hook 'wiki-mode-on-hook 'wiki-highlight-buffer)
(add-hook 'wiki-mode-on-hook (lambda () (setq indent-tabs-mode nil)))

(add-hook 'wiki-mode-off-hook 'wiki-deinstall)
(add-hook 'wiki-mode-off-hook 'wiki-delete-extents)

(when (fboundp 'goto-address)
  (add-hook 'wiki-highlight-buffer-hook 'goto-address))

;; List of known wiki files

(defvar wiki-last-update nil
  "Time when the `wiki-file-alist' was last updated.")

(defvar wiki-file-alist nil
  "List of existing WikiNames.
This is used by `wiki-existing-names' as a cache.")

(defsubst wiki-existing-page-names ()
  "Return all page names from `wiki-existing-names'."
  (mapcar (lambda (f) (car f)) (wiki-existing-names)))

(defsubst wiki-existing-file-names ()
  "Return all file names from `wiki-existing-names'."
  (mapcar (lambda (f) (cdr f)) (wiki-existing-names)))

(defun wiki-existing-names ()
  "Return wiki filenames in `wiki-directories' as an alist.  
Wiki filenames match `wiki-name-regexp'.  The result is cached and
updated when necessary based upon directory modification dates.  The car
of each element is the page name, the cdr of each element is the fully
qualified filename.  Use `wiki-existing-page-names' and
`wiki-existing-file-names' to get lists of page names or file names."
  (let* ((dirs wiki-directories)
         last-mod)
    (while dirs
      (let ((mod-time (nth 5 (file-attributes (car dirs)))))
        (if (or (null last-mod)
                (time-less-p last-mod mod-time))
            (setq last-mod mod-time)))
      (setq dirs (cdr dirs)))
    (if (not (or (null wiki-last-update)
                 (null last-mod)
                 (time-less-p wiki-last-update last-mod)))
        wiki-file-alist
      (setq wiki-last-update last-mod
	    wiki-file-alist (wiki-read-directories)))))

(defun wiki-read-directories ()
  "Return list of all files in `wiki-directories'.
Each element in the list is a cons cell.  The car holds the pagename,
the cdr holds the fully qualified filename. If set, `wiki-extension'
is appended to the filenames."
  (let ((dirs wiki-directories)
	(regexp (concat "^" wiki-name-regexp 
			(if wiki-extension (concat "\\." wiki-extension) "") "$"))
	result)
    (setq dirs wiki-directories)
    (while dirs
      (let ((files (mapcar (lambda (f)
			     (cons (file-name-nondirectory
				    (file-name-sans-extension f)) f))
			   (directory-files (car dirs) t regexp t))))
	(setq result (nconc files result)))
      (setq dirs (cdr dirs)))
    result))

(defun wiki-name-exists-p (name)
  "Return non-nil when NAME is an existing wiki-name."
  (assoc name (wiki-existing-names)))

(defun wiki-expand-name (name)
  "Return the expanded filename for NAME.
This relies on `wiki-existing-names'."
  (cdr (assoc name (wiki-existing-names))))

;; Following hyperlinks

(defun wiki-follow-name (name)
  "Follow the link NAME by invoking `wiki-follow-name-action'.
If NAME is part a key in the alist returned by `wiki-existing-names',
then the corresponding filename is used instead of NAME."
  (let ((file (cdr (assoc name (wiki-existing-names)))))
    (if file
	(funcall wiki-follow-name-action file)
      (funcall wiki-follow-name-action
	       (concat name (if wiki-extension (concat "." wiki-extension) ""))))))
  
(defun wiki-follow-name-at-point ()
  "Find wiki name at point.
See `wiki-name-p' and `wiki-follow-name'."
  (interactive)
  (if (wiki-name-p)
      (wiki-follow-name (match-string 0))
    (error "Point is not at a WikiName")))

(defun wiki-follow-name-at-mouse (event)
  "Find wiki name at the mouse position.
See `wiki-follow-name-at-point'."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (wiki-follow-name-at-point)))

(defun wiki-next-reference ()
  "Jump to next wiki name.
This modifies the data returned by `match-data'.
Returns the new position of point or nil.
See `wiki-name-p'."
  (interactive)
  (let ((case-fold-search nil)
	found match)
    (save-excursion
      (condition-case nil
	  ;; will cause an error in empty buffers
	  (forward-char 1)
	(error))
      (when (re-search-forward wiki-name-regexp nil t)
	(setq found (match-beginning 0)
	      match (match-data)))
      (while (and found (not (wiki-name-p 'shortcut)))
	(forward-char 1)
	(if (re-search-forward wiki-name-regexp nil t)
	    (setq found (match-beginning 0)
		  match (match-data))
	  (setq found nil
		match nil))))
    (set-match-data match)
    (when found
      (goto-char found))))

(defun wiki-previous-reference ()
  "Jump to previous wiki name.
See `wiki-name-p'."
  (interactive)
  (let ((case-fold-search nil)
	found)
    (save-excursion
      (save-match-data
	(setq found (re-search-backward wiki-name-regexp nil t))
	(while (and found (not (wiki-name-p 'shortcut)))
	  (forward-char -1)
	  (setq found (re-search-backward wiki-name-regexp nil t)))))
    (when found
      (goto-char found))))

;; Backlink and other searches

(defun wiki-backlink ()
  "Return all backlinks to the current page using `grep'."
  (interactive)
  (when (not grep-command)
    (grep-compute-defaults))
  (grep (concat grep-command
		(wiki-page-name)
		" *"))
  (set-buffer "*grep*"))

(defun wiki-backup ()
  "Run `diff-backup' on the current file."
  (interactive) 
  (diff-backup buffer-file-name))

;; Highlighting hyperlinks

(defun wiki-highlight-buffer ()
  "Highlight the buffer.
Delete all existing wiki highlighting using `wiki-delete-extents' and
call all functions in `wiki-highlight-buffer-hook'."
  (interactive)
  (wiki-delete-extents)
  (run-hooks 'wiki-highlight-buffer-hook))

(defun wiki-highlight-wiki-names ()
  "Highlight all WikiNames in the buffer.
This uses `wiki-highlight-match' to do the job.
The list of existing names is recomputed using `wiki-existing-names'."
  (interactive)
  (wiki-delete-extents)
  (save-excursion
    (goto-char (point-min))
    (when (wiki-name-p)
      (wiki-highlight-match))
    (while (wiki-next-reference)
      (wiki-highlight-match))))

(defun wiki-highlight-match ()
  "Highlight the latest match as a WikiName.
`wiki-name-p' is not called again to verify the latest match.
Existing WikiNames are highlighted using face `info-xref'."
  (save-match-data
    (let ((with-glyph (not (funcall wiki-highlight-name-exists
				    (match-string 0)))))
      (wiki-make-extent (match-beginning 0)
			(match-end 0)
			wiki-local-map
			with-glyph))))

(defun wiki-highlight-word-wrapper (&optional start end len)
  "Highlight the current word if it is a WikiName.
This function can be put on `after-change-functions'.
It calls `wiki-highlight-word' to do the job."
  (when start
    (wiki-highlight-word start))
  (when (= 0 len); for insertions
    (wiki-highlight-word end)))

(defun wiki-highlight-word (pos)
  "Highlight the current word if it is a WikiName.
This uses `wiki-highlight-match' to do the job.  POS specifies a buffer
position."
  (save-excursion
    (goto-char pos)
    (save-match-data
      (cond ((wiki-name-p); found a wiki name
	     (wiki-delete-extents (match-beginning 0) (match-end 0))
	     (wiki-highlight-match))
	    ;; The following code makes sure that when a WikiName is
	    ;; edited such that is no longer is a wiki name, the
	    ;; extent/overlay is removed.
	    ((thing-at-point-looking-at wiki-name-no-more)
	     (wiki-delete-extents (match-beginning 0) (match-end 0)))))))

;; Parsing all files into a directed graph

(defun wiki-parse-files ()
  "Return all pages and the links they contain in an alist.
Each element in the alist has the form
\(NAME LINK1 LINK2 ...)
See `wiki-parse-file'.  The list of existing names is recomputed using
`wiki-existing-file-names'."
  (mapcar (function wiki-parse-file) (wiki-existing-file-names)))

(defun wiki-parse-file (file)
  "Build a list of links for FILE.
Returns a list of the form
\(NAME LINK1 LINK2 ...)
See `wiki-parse-files'."
  (message "Parsing %s" file)
  (let ((page (list (file-name-nondirectory file))))
    (with-temp-buffer
      ;; fake an existing buffer-file-name in the temp buffer
      (let ((buffer-file-name file))
	(insert-file-contents file)
	(goto-char (point-min))
	(while (wiki-next-reference)
	  (let ((this (match-string 0)))
	    (when (and (wiki-name-exists-p this)
		       (not (member this page)))
	      (setq page (cons this page)))))))
    (reverse page)))

;; Filtering the directed graph

(defun wiki-filter (structure)
  "Filter STRUCTURE according to `wiki-include-function'."
  (if (eq wiki-include-function t)
      structure
    (wiki-filter-links
     (wiki-filter-pages
      (copy-alist structure)))))

(defun wiki-filter-pages (structure)
  "Filter pages structure according to `wiki-include-function'."
  (let ((pages structure)
	page)
    (while pages
      (setq page (car pages)
	    pages (cdr pages))
      (if (funcall wiki-include-function
		   (car page) structure)
	  (message "Keeping %s" (car page))
	(message "Filtering %s" (car page))
	(setq structure (delete page structure))
	;; restart!
	(setq pages structure)))
    structure))

(defun wiki-filter-links (structure)
  "Filter links to nonexisting pages from structure."
  (let ((pages structure)
	page)
    (while pages
      (setq page (car pages)
	    pages (cdr pages))
      (setcdr page (delq nil (mapcar (lambda (link)
				       (if (assoc link structure)
					   link
					 nil))
				     (cdr page)))))
    structure))

;; Example filtering functions

(defun wiki-significant-fan-out (name structure)
  "Return non-nil when `wiki-fan-out' is significant.
This is determined by `wiki-significant-fan-out'."
  (> (wiki-fan-out name structure) wiki-significant-fan-out))

(defun wiki-fan-out (name structure)
  "Return number of links pointing away from NAME.
This is calculated from STRUCTURE as returned by `wiki-parse-files'."
  (length (cdr (assoc name structure))))

;; Example applications of parsing and filtering

(defun wiki-list-by-fan-out ()
  "List the wiki site structure by fan-out."
  (interactive)
  (let ((graph (mapcar (lambda (page)
			 (cons (car page) (length (cdr page))))
		       (wiki-parse-files))))
    (message "Preparing...")
    (setq graph (sort graph
		      (lambda (p1 p2)
			(< (cdr p1) (cdr p2)))))
    (let ((buf (get-buffer-create "*wiki*")))
      (set-buffer buf)
      (erase-buffer)
      (pp graph buf)
      (emacs-lisp-mode)
      (wiki-mode 1)
      (switch-to-buffer buf)
      (message "Preparing...done"))))

;; Publishing

(defun wiki-publish-all (&optional arg)
  "Publish all wikis.
If the published wiki already exists, it is only overwritten if the wiki
is newer than the published copy.  When given the optional argument ARG,
all wikis are rewritten, no matter how recent they are.  The index file
is rewritten no matter what.
The list of existing names is recomputed using `wiki-existing-names'."
  (interactive "P")
  (save-some-buffers)
  (wiki-publish-files (wiki-existing-file-names) arg)
  (wiki-publish-index))

(defun dired-do-wiki-publish ()
  "Publish all marked files in a dired buffer."
  (interactive)
  (wiki-publish-files (dired-get-marked-files) t))

(defun wiki-publish-files (files force)
  "Publish all files in list FILES.
If the argument FORCE is nil, each file is only published
if it is newer than the published version.  If the argument
FORCE is non-nil, the file is published no matter what."
  (let (wiki-current-date file page)
    (while files
      (setq file (car files)
	    files (cdr files)
	    wiki-current-date (nth 5 (file-attributes file))
	    page (wiki-write-file-name
		       (concat (file-name-nondirectory (file-name-sans-extension file))
			       wiki-pub-file-name-suffix)))
      (when (or force (file-newer-than-file-p file page))
	(with-temp-buffer
	  (insert-file-contents file t)
	  (wiki-publish))))))

(defun wiki-publish ()
  "Publish current wiki buffer as an HTML file.
The file will be created in `wiki-pub-directory'.  You can
publish several files at once from a dired buffer using
`dired-do-wiki-publish', or you can publish all files using
`wiki-publish-all'."
  (interactive)
  (let ((file-name (file-name-sans-extension buffer-file-name))
	(content (buffer-substring (point-min) (point-max))))
    (message "Publishing %s" file-name)
    (with-temp-buffer
      ;; fake an existing buffer-file-name in the temp buffer
      (let ((buffer-file-name file-name))
	(let ((start (point))
	      end)
	  (insert content)
	  (setq end (point-marker))
	  (wiki-pub-apply-rules start end))
	(wiki-write-file (concat (wiki-page-name)
				 wiki-pub-file-name-suffix))))))

;; Index page

(defun wiki-publish-index ()
  "Publish an index of all wikis."
  (interactive)
  (with-temp-buffer
    (let ((buffer-file-name wiki-index-file-name))
      (wiki-insert-index (wiki-existing-page-names))
      (wiki-publish))))

(defun wiki-insert-index (files)
  "Insert a list of all FILES."
  (let (file)
    (setq files (sort files 'string-lessp))
    (while files
      (setq file (car files)
	    files (cdr files))
      (insert "* " file "\n"))))

;; Writing files

(defun wiki-write-file (name)
  "Write current buffer to file NAME in `wiki-pub-directory'."
  (let ((backup-inhibited t))
    (write-file (wiki-write-file-name name))))

(defun wiki-write-file-name (name)
  "Expand file name NAME in `wiki-pub-directory'."
  (if wiki-pub-directory
      (expand-file-name name wiki-pub-directory)
    (error "Publishing is disabled")))

;; Publishing: HTML Tags helper functions

(defvar wiki-current-date nil
  "The modification time of the file being published.
This is bound by `wiki-publish-files' to the modification time returned
by `file-attributes'.  Note that publishing the current buffer using
`wiki-publish' will not bind the variable `wiki-current-date', therefore
the function `wiki-current-date' will return the current date.")

(defun wiki-current-date ()
  "Insert the current date using `wiki-date-format'.
If bound, the variable `wiki-current-date' will be used instead of the
current date.  It is usually the modification time of the file."
  (format-time-string wiki-date-format wiki-current-date))

(defun wiki-replace-links ()
  "Replace wiki names with HTML links."
  (while (wiki-next-reference)
    (let ((this (match-string 0)))
      (if (assoc this (wiki-existing-names))
	  (replace-match
	   (concat "<a href=\"" this wiki-pub-file-name-suffix
		   "\">" this "</a>") t)
	(replace-match
	 (concat this "<a href=\"" wiki-maintainer "\">?</a>") t)))))

(defun wiki-pub-apply-rules (start end)
  "Replace wiki markup with publishing markup.
The standard publishing markup is HTML, but this can be changed.  The
markup is produced by applying all the rules in the variable
`wiki-pub-rules'."
  (save-restriction
    (narrow-to-region start end)
    (let ((case-fold-search nil)
	  (rules wiki-pub-rules)
	  rule)
      (while rules
	(setq rule (car rules)
	      rules (cdr rules))
	(goto-char (point-min))
	(wiki-pub-apply-rule rule)))))

(defun wiki-pub-apply-rule (rule)
  "Apply RULE.
See `wiki-pub-rules'."
  (if (functionp rule)
      (funcall rule)
    (let ((action (car rule)))
      (cond ((functionp action)
	     (funcall action)
	     (insert (wiki-pub-rule-effect rule)))
	    ((stringp action)
	     (while (re-search-forward action nil t)
	       (replace-match (wiki-pub-rule-effect rule) t)))))))

(defun wiki-pub-rule-effect (rule)
  "Return the string to use for RULE.
See `wiki-pub-rules'."
  (let ((effect (cdr rule)))
    (if (functionp effect)
	(funcall effect)
      effect)))

;; Viewing the result

(defun wiki-view-published ()
  "Switch to the published version of the current buffer."
  (interactive)
  (find-file (wiki-write-file-name 
	      (concat (wiki-page-name) 
		      wiki-pub-file-name-suffix))))

;; Emacs/XEmacs compatibility layer

(defun wiki-make-extent (from to map with-glyph)
  "Make an extent for the range [FROM, TO) in the current buffer.
MAP is the local keymap to use, if any.
WITH-GLYPH non-nil will add a question-mark after the extent.
XEmacs uses `make-extent', Emacs uses `make-overlay'."
  ;; I don't use (fboundp 'make-extent) because of (require 'lucid)
  (if (featurep 'xemacs)
      ;; Extents for XEmacs
      (let ((extent (make-extent from to)))
	(set-extent-property extent 'face 'info-xref)
	(set-extent-property extent 'mouse-face 'highlight)
	(when map
	  (set-extent-property extent 'keymap map))
	(set-extent-property extent 'evaporate t)
	(set-extent-property extent 'wikiname t)
	(when with-glyph
	  (set-extent-property extent 'end-glyph (make-glyph '("?"))))
	extent)
    ;; Overlays for Emacs
    (let ((overlay (make-overlay from to)))
      (overlay-put overlay 'face 'info-xref)
      (overlay-put overlay 'mouse-face 'highlight)
      (when map
	(overlay-put overlay 'local-map map))
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'wikiname t)
      (when with-glyph
	(overlay-put overlay 'after-string "?"))
      overlay)))

(defun wiki-delete-extents (&optional start end)
  "Delete all extents/overlays created by `wiki-make-extent'.
If optional arguments START and END are given, only the overlays in that
region will be deleted.  XEmacs uses extents, Emacs uses overlays."
  (if (featurep 'xemacs)
      (let ((extents (extent-list nil start end))
	    extent)
	(while extents
	  (setq extent (car extents)
		extents (cdr extents))
	  (when (extent-property extent 'wikiname)
	    (delete-extent extent))))
    (let ((overlays (overlays-in (or start (point-min))
				 (or end (point-max))))
	  overlay)
      (while overlays
	(setq overlay (car overlays)
	      overlays (cdr overlays))
	(when (overlay-get overlay 'wikiname)
	  (delete-overlay overlay))))))

(unless (fboundp 'time-less-p)
  (defun time-less-p (t1 t2)
    "Say whether time T1 is less than time T2."
    (or (< (car t1) (car t2))
	(and (= (car t1) (car t2))
	     (< (nth 1 t1) (nth 1 t2))))))

(provide 'wiki)

;;; wiki.el ends here
