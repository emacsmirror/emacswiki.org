;;; tjic-blog.el
;;
;;  Copyright (C) 2002-2003 Travis J.I. Corcoran
;;
;;
;; Author: Travis J.I. Corcoran <tjic_emacs@tjic.com>
;; Version: $Header: /var/cvsroot/tjiclisp/tjic-blog.el,v 1.3 2003/05/02 16:41:24 tjic Exp $
;; Keywords: website, tjic, html, php, blog
;;
;; This is free software
;;
;;
;; TABLE OF CONTENTS
;; -----------------
;; 0. table of contents
;; 1. introduction
;; 2. setup
;; 3. use
;; 4. theory of operation
;; 5. bugs
;;
;;
;; INTRODUCTION
;; ------------
;; The most up to date version of this package can be found at:
;;         http://www.tjic.com/computers/tjic-blog.el
;;
;;
;; SETUP
;; -----
;;
;; (1) save this file to some place in your emacs load-path 
;;      (if you don't know what this means, type 
;;         M-x describe-variable [ RETURN ] load-path [ RETURN ]
;;      )
;;
;; (2) download and save files tjicutil-funcs.el and tjic-website.el
;;       in the same directory.
;;
;; (3) in your .emacs put 
;;      (require 'tjic-blog "tjic-blog.el")
;;
;; (4) create a valid HTML/PHP/what-have-you file that will hold your
;;     blog, and make sure that it has the following lines:
;;     <!-- UPDATES HERE -->
;;     <!-- END OF BLOG HERE -->
;;
;; (5) in your .emacs put the following, replacing the ... with
;;     appropriate customizations.  Read the defvar statements below
;;     to figure out what each variable does and what plausible values
;;     might be.
;;
;;     (defvar tjic-blog-local-location ... )
;;     (defvar tjic-blog-whatsnew-file ... )
;;     (defvar tjic-blog-secret-begin ... )
;;     (defvar tjic-blog-secret-end ... )
;;     (defvar tjic-blog-reading-begin ... )
;;     (defvar tjic-blog-reading-end ... )
;;     (defvar tjic-blog-listening-begin ... )
;;     (defvar tjic-blog-listening-end ... )
;;     (defvar tjic-blog-quoted-region-begin ... )
;;     (defvar tjic-blog-quoted-region-end ... )
;;
;; (6) OPTIONAL: If the HTML comments in step 4 are not to your
;;     liking, you may use some other comments in your HTML file and then
;;     redefine the default values in your .emacs:
;;
;;     (defvar tjic-blog-whatsnew-editpt ... )
;;     (defvar tjic-blog-endpt ... )
;;
;; USE
;; ---
;;
;; (1) To post a new blog entry, type C-x M-b
;; (2) You may now type a blog entry.
;;
;; optional steps:
;;   (3a) Hit C-c C-p to toggle the "private" boolean field.
;;   (3b) add text after the "reading: " prompt, for inclusion in your blog entry.
;;   (3c) add text after the "listening: " prompt, for inclusion in your blog entry.
;;
;; (4) Hit C-c C-c when done.  This will post your new entry.
;;
;;
;; THEORY OF OPERATION
;; -------------------
;;  
;;  tjic-blog.el assumes that you have a HTML/PHP/whatever file
;;  locally, and that you edit this file and then scp it to your webserver.
;;  
;;  This package provides a function to open a new buffer for editting
;;  a blog post, then provides another function that "posts" your new
;;  entry.  Posting consists of:
;;
;;  (1) massaging the newly typed text to include display frippery
;;      before and after the timestamp, before and after comments about
;;      what books one is reading, etc.
;;
;;  (2) pasting the massaged text into the HTML/PHP/whatever file at an appropriate pt.
;;
;;  (3) scp-ing the blog file to the webserver.
;;
;; 
;;  BUGS
;;  ----
;;
;;  - scp step should be customizable: maybe just ftp for some people?
;;  - merely waiting a fixed number of seconds for scp stinks
;;  - error messages for trying to tjic-website-copyfile-to-server could be better
;;  - need documentation of other funcs:
;;      + tjic-website-copyfile-to-server
;;      + tjic-blog-ullist-region
;;      + tjic-blog-move-to-archive
;;      + tjic-blog-quote-region


;;----------
;; Dependencies:
;;

(require 'tjicutil-funcs "tjicutil-funcs.el")
(require 'tjic-website "tjic-website.el")


;;----------
;; VARS: files
;;

(defvar tjic-blog-file-root "/home/tjic/personal/www/" 
  "root of blog files and blog TOC file")

(defvar tjic-blog-file "blog/blog.php"
  "Specifies the file, relative to 'tjic-blog-file-root' which should
have blog entries added")

(defvar tjic-blog-toc-file "title.inc"
  "Specifies the file, relative to 'tjic-blog-file-root'
which should have blog entries added")

;;----------
;; VARS: edit point tags inside files
;;

(defvar tjic-blog-whatsnew-editpt "<!-- UPDATES HERE -->"
  "Specifies where in tjic-blog-whatsnew-file new entries should be written")

(defvar tjic-blog-endpt "<!-- END OF BLOG HERE -->"
  "Specifies where in tjic-blog-whatsnew-file new entries should be written")

(defvar tjic-blog-toc-editpt "<!-- BLOG TOC UPDATES HERE -->"
  "Specifies where in tjic-blog-file new entries should be written")

(defvar tjic-blog-editpt "<!-- UPDATES HERE -->"
  "Specifies where in tjic-blog-file new entries should be written")


;;----------
;; VARS: calendar feature (NOT YET IMPLEMENTED);;
;;

(defvar tjic-blog-use-cal nil
  "should the blog have a calendar table of contents?")

(defvar tjic-blog-cal-program "/usr/bin/cal"
  "path of 'cal' program")

(defvar tjic-blog-cal-begin "<!-- CAL STARTS HERE -->")
(defvar tjic-blog-cal-end "<!-- CAL ENDS HERE -->")




;;----------
;; VARS: HTML to insert
;;

(defvar tjic-blog-datestamp 
  "<!-- -------------------------------------- SERIAL(%d_%b_%Y, %!) -->
   <?php blogentry_begin(\"%d %b %Y\", %!); ?>"

  "Specifies the text to be inserted before each blog entry.  Note
that emacs function 'format-time-string' will be run on this string,
to substitute in timestamp values for special tokens.  Read the
docstring on that func for more details, but, as an example:
	%d is the day of the month
	%b is the abbreviated month name
	%Y is the year
Note that after 'format-time-string' is run, any instances of '%!'
will be replaced with a serial number that is monotonically 
increasing during each calendar day, and gets reset to 0 at the 
begining of each new day")

; - secret 
(defvar tjic-blog-secret-begin ""
  "specifies the text that comes before a secret entry")

(defvar tjic-blog-secret-end ""
  "specifies the text that comes after a secret entry")

; - reading
(defvar tjic-blog-reading-begin ""
  "specifies the text that comes before what I'm currently reading")

(defvar tjic-blog-reading-end ""
  "specifies the text that comes after what I'm currently reading")

; - listening
(defvar tjic-blog-listening-begin ""
  "specifies the text that comes before what I'm currently listening to")

(defvar tjic-blog-listening-end ""
  "specifies the text that comes after what I'm currently listening to")

; - quoted

(defvar tjic-blog-quoted-region-begin ""
  "specifies the text that comes before a quoted region")

(defvar tjic-blog-quoted-region-end ""
  "specifies the text that comes after a quoted region")

;; TJIC's setting of above vars
(setq tjic-blog-secret-begin   "<? if (Is_InnerCircle()) { blogentry_secret_begin();  ?>")
(setq tjic-blog-secret-end     "<? blogentry_secret_end(); } ?>")
(setq tjic-blog-quoted-region-begin "<? Quote_begin();  ?>\n")
(setq tjic-blog-quoted-region-end   "\n<? Quote_end();  ?>\n")
(setq tjic-blog-reading-begin   "<? reading_begin(); ?>")
(setq tjic-blog-reading-end     "<? reading_end(); ?>\n")
(setq tjic-blog-listening-begin "<? listening_begin(); ?>")
(setq tjic-blog-listening-end   "<? listening_end(); ?>\n")

;;;----------------------------------------------------------------------
;;; support privately readable blog entries
;;;
;;; NOTE: privateP bit exists *only* as a text representation in the
;;; "*blog*" buffer
;;;----------------------------------------------------------------------


;; general tools for getting/setting fields

(defvar tjic-blog-header-separator
  "--text follows this line--" 
  "Line used to separate headers from text in messages being composed.")

(defun tjic-blog-set (field value)
  ""
  (save-excursion
	(goto-char (point-min))

	;; remove old entry, if needed
	(let ((endlimit (search-forward tjic-blog-header-separator)))
	  (goto-char (point-min))
	  (if (search-forward "private: " endlimit)
		  (progn
			(beginning-of-line)
			(kill-line 1)
			(newline)
			(previous-line 1))))

	;; insert current
	(insert (concat field ": " value))))

(defun tjic-blog-get (field)
  "Given a field title, find the field before the seperator, and return 
the text from the colon to the linefeed.  Return nil if no field."
  (save-excursion
	(goto-char (point-min))

 	(let ((endlimit (search-forward tjic-blog-header-separator))
		  (foundp)
		  (beginpt))
	  
	  (goto-char (point-min))
	  (setq foundp (search-forward (concat field ": ") endlimit t))
	  (if foundp
		  (progn 
			(setq beginpt (point))
			(end-of-line)
			(buffer-substring beginpt (point)))
		nil))))

(defun tjic-blog-get-private ()
  "Return the contents of the 'private:' field as either t or nil"
  (equal (tjic-blog-get "private") "t"))

(defun tjic-blog-header-private-update (newState newEntry)
  "updates the textual representation showing wether the blog entry is private"
  (tjic-blog-set "private" (if newState "t" "nil")))

(defun tjic-blog-header-private-toggle ()
  ""
  (interactive)
  (tjic-blog-header-private-update 
   (not (tjic-blog-get-private))
   nil))

(defun tjic-blog-header-goto (field)
  "go to field"
  (goto-char (point-min))
  (let ((endlimit (search-forward tjic-blog-header-separator)))
	(goto-char (point-min))
	(search-forward (concat field ":") endlimit)))

(defun tjic-blog-header-goto-reading ()
  "" 
  (interactive)
  (tjic-blog-header-goto "reading"))

(defun tjic-blog-header-goto-listening ()
  ""
  (interactive)
  (tjic-blog-header-goto "listening"))

;; (defmacro tjic-swap1 (a b)
;;   ""
;;   '(progn
;; 	 (setq a 1)
;; 	 (setq b 2)))
;; 
;; (defmacro tjic-swap1 (a b)
;;   ""
;;   (list  'progn 
;; 		 (list 'setq a 1)
;; 		 (list 'setq b 2)))
;; 

(defmacro tjic-swap1 (a b)
  ""
  (list  'let '((tmp)) 
		 (list 'setq 'tmp a)
		 (list 'setq a b)
		 (list 'setq b 'tmp)
))

(defun tjic-blog-pretty-url ()
  "Assuming point is at begining of an URL, pretty it up.
Returns pt at end of newly inserted text."
  (interactive)
  (let ((pta (point))
		(url)
		(endpt))
	(save-excursion
	  (end-of-line)
	  (setq url (buffer-substring pta (point)))
	  (goto-char pta)
	  (insert "<a href=\"")
	  (end-of-line)
	  (insert "\">")
	  (insert url)
	  (insert "</a>")
	  (setq endpt (point)))
	endpt))

(defun tjic-blog-modify-region (prefix 
										postfix 
										linesp 
										seperator
										&optional prettyurlsp)

  "Given point and mark, prefix region with text 'PREFIX', suffix it
with text 'POSTFIX', and seperate either lines (if LINESP is true) or
else pars with SEPERATOR.  If PRETTYURLSP, then invoke
tjic-blog-pretty-url on any urls found in region.
Does not make assumptions about whether the pt is at top or bottom."

  (save-excursion
	(let ((pta (point))
		  (ptb (mark))
		  (begin-mark)
		  (end-mark)
		  (tmp))

	  (if (< ptb pta)
		  (tjic-swap1 pta ptb))

	  (setq end-mark (copy-marker ptb))

	  ;; insert prefix text
	  (goto-char pta)
	  (beginning-of-line)
	  (insert prefix)
	  (newline)


	  ;; put in seperators 
	  (indent-rigidly pta ptb -1000)
	  (setq begin-mark (point))
	  (while (< (point) end-mark)
		(insert seperator)
		(if linesp
			(progn (next-line 1)
				   (beginning-of-line))
		  (forward-paragraph)))
	  (indent-rigidly begin-mark end-mark 4)

	  ;; pretty up urls if required
	  (if prettyurlsp
		  (progn
			(goto-char pta)

			(while (search-forward "http://" end-mark t)
			  (progn
				(goto-char (match-beginning 0))
				(goto-char (tjic-blog-pretty-url))))))

	  ;;(skip-syntax-forward "^-")

	  ;; insert suffix text
	  (goto-char end-mark)
	  (beginning-of-line)
	  (insert postfix))))

(defun tjic-blog-quote-region ()
  "Given point and mark, prefix region with some quote/indent text,
suffix it with the inverse function, and add par markers as approp
with in.  Does not make assumptions about whether the pt is at top or bottom."
  (interactive)
  (tjic-blog-modify-region
     tjic-blog-quoted-region-begin
	 tjic-blog-quoted-region-end
	 nil
	 "<p>"
	 t))


(defun tjic-blog-ullist-region ()
  "Given point and mark, turn region into unordered list.
Does not make assumptions about whether the pt is at top or bottom."
  (interactive)
  (tjic-blog-modify-region
     "<ul>"
     "</ul>"
	 t
	 "<li>"))

(defun tjic-blog-entry ()
  "Create a new buffer for editting a new blog entry"
  (interactive)
  ;; prep buffer
  ;;
  (switch-to-buffer (get-buffer-create "*blog*"))
  (erase-buffer)
  (goto-char (point-min))

  (insert "private: nil")
  (end-of-line)
  (newline)

  (insert "reading:")
  (end-of-line)
  (newline)

  (insert "listening:")
  (end-of-line)
  (newline)


  (insert tjic-blog-header-separator)

  (newline)

  (tjic-blog-mode)
)

(global-set-key "\C-x\M-b"  'tjic-blog-entry)

(defun tjic-format-serial-number (string ser)
  ;; BUG: will DTWT on "%%!"
  "replace all '%!'s in STRING with SER"
  (tjic-strings-replace-X-with-Y string "%!" (int-to-string ser)))
						   

(defun tjic-get-todays-serialnumber ()
  "returns an integer for the blog entry serial number.
First post of the day gets '0', each entry post thereafter
gets an incremented number.

BUG: there is too tight an interaction between this and the
theoretically user-configurable variable 'tjic-blog-datestamp'
"

  (save-excursion
	(set-buffer
	 (find-file-noselect 
	  (concat tjic-blog-file-root tjic-blog-file)))


	(goto-char (point-min))
	(let ((search-string (concat "<!-- --+ SERIAL("
								 (format-time-string "%d_%b_%Y"))))
	  (if (re-search-forward search-string nil t)
										; found
		  (progn
			(re-search-forward "[0-9]+")
			(+ 1 (string-to-int (buffer-substring (nth 0 (match-data))
												  (nth 1 (match-data))))))
										; not found
		0))))
	  


(defun tjic-blog-post ()
  "post blog entry to website"
  (interactive)
  (let ((private (tjic-blog-get-private))
		(reading (tjic-blog-get "reading"))
		(listening (tjic-blog-get "listening"))
		(blog-text (buffer-substring (progn 
									   (goto-char (point-min))
									   (+ 1 (search-forward tjic-blog-header-separator)))
									 (point-max)))
		(blog-entry-buffer (current-buffer)))

	;; switch to buffer holding blog
	(set-buffer
	 (find-file-noselect 
	  (concat tjic-blog-file-root tjic-blog-file)))
	;; edit it.
	(save-excursion
	  (goto-char (point-min))
	  (search-forward tjic-blog-editpt)
	  (newline)
	  (if private (insert tjic-blog-secret-begin))
	  (newline)

	  (let ((ser (tjic-get-todays-serialnumber)))
		(insert (tjic-format-serial-number
				 (format-time-string tjic-blog-datestamp (current-time))
				 ser))
		(newline)
		(insert "<br>")
		(newline)

		(if reading 
			(insert (concat tjic-blog-reading-begin 
							reading
							tjic-blog-reading-end)))
		(if listening 
			(insert (concat tjic-blog-listening-begin 
							listening
							tjic-blog-listening-end)))
		(if (or reading listening)
			(insert "<br>"))

		(insert blog-text)
		(newline)

		(if private (insert tjic-blog-secret-end))

		;; insert calendar entry if needed
		
		(if tjic-blog-use-cal
			(progn
			  (goto-char (point-min))
			  (re-search-forward tjic-blog-cal-begin)
			  (let ((beg-cal-delim   (nth 1 (match-data))))
				(re-search-forward tjic-blog-cal-end)
				(if (> (+ beg-cal-delim 2) 
					   (nth 0 (match-data)))
					;; no cal yet exists
					tjic-blog-cal-insert-new)
				;; POST-CONDITION: cal exists
			  (goto-char (point-min))
			  (re-search-forward tjic-blog-cal-begin))))))

		
				
	  (save-buffer)
	  (tjic-website-copyfile-to-server nil)

	;; switch back to buffer holding new entry.
	;; mark it completed
	(set-buffer blog-entry-buffer)
	(set-buffer-modified-p nil)
	(bury-buffer)))

(defun tjic-blog-cal-insertnew (&optional pre-str post-str)
  ""
  (interactive)

  (if (not pre-str) (setq pre-str ""))
  (if (not post-str) (setq post-str ""))


  (insert pre-str)
  (insert "<table border=0>")
  (save-excursion
	(call-process 
	 tjic-blog-cal-program 
	 nil
	 t
	 nil
	 (format-time-string "%m")
	 (format-time-string "%Y"))
	(insert "</table>")
	(insert post-str))
  (kill-line 2)
  )



(defun tjic-blog-util-fullfile-name (filename)
  "given just file name, find fully qualified filename"

  (let* ((local-to-remote-map (tjic-assoc-both
							   (file-name-directory (buffer-file-name))
							   tjic-website-local-machine-remote-map
							   'tjic-str2-is-prefix-of-str1) )
		 (local-root (car local-to-remote-map))
		 (local-wrt-root (tjic-str2-in-excess-of-str1 local-root (buffer-file-name)))

		 (remote-full (cdr local-to-remote-map))
		 (remote-host (car remote-full))
		 (remote-root (cdr remote-full)))
	))

(defun tjic-blog-move-to-archive ()
  "move current blog page to a new archive page, update blog TOC"
  (interactive)
  (let ((b)
		(e)
		(page-header)
		(page-footer)

 		(begin-month)
		(begin-yr)

		(end-month)
		(end-yr)

		(new-archive-file)
		(block-begin-date)

		(header)
		(footer)
		(blog-contents))

	;; switch to buffer holding blog
	(set-buffer
	 (find-file-noselect  (concat tjic-blog-file-root tjic-blog-file)))

	;; find begin date of this blog
	(goto-char (point-max))
	(re-search-backward "<?php blogentry_begin(")
	(forward-word 4)
	(setq b (+ 1 (point)))
	(forward-word 1)
	(setq begin-month (buffer-substring b (point)))
	(setq b (+ 1 (point)))
	(forward-word 1)
	(setq begin-yr (buffer-substring b (point)))
	(setq block-begin-date (concat begin-month begin-yr))

	;; using begin date, and end date of 'now', verify that 'new'
	;; archive file does not already exist
	(setq end-month (format-time-string "%b"))
	(setq end-yr    (format-time-string "%Y"))

	(setq new-archive-file (concat (tjic-strings-remove-suffix 
									tjic-blog-file ".php") 
								   "_"
								   (downcase block-begin-date)
								   "_"
								   (downcase end-month)
								   end-yr
								   ".php"))


	(if (file-exists-p (concat tjic-blog-file-root new-archive-file))
		(error "archive file by that name already exists"))

	;; copy style of this blog
	(goto-char (point-min))
	(re-search-forward tjic-blog-whatsnew-editpt)
	(setq header (buffer-substring (point-min) (point)))
	(re-search-forward tjic-blog-endpt)
	(beginning-of-line)
	(setq footer (buffer-substring (point) (point-max)))

	;; grab contents of current blog, delete
	(save-excursion
	  (goto-char (point-min))
	  (search-forward tjic-blog-editpt)
	  (next-line 1)
	  (setq b (point))
	  (search-forward tjic-blog-endpt)
	  (beginning-of-line)
	  (setq e (point))
	  (setq blog-contents (buffer-substring b e))
	  (delete-region b e))
	(save-buffer)
	(tjic-website-copyfile-to-server nil)

	;; switch to new buffer for archive
	(set-buffer
	 (find-file-noselect (concat tjic-blog-file-root new-archive-file)))
	(insert header)
	(insert blog-contents)
	(insert footer)
	(save-buffer)
	(tjic-website-copyfile-to-server nil)

	;; edit TOC (which is a PHP function; we're editting PHP here)
	(set-buffer
	 (find-file-noselect 
	  (concat tjic-blog-file-root tjic-blog-toc-file)))
	(goto-char (point-min))
	(re-search-forward  tjic-blog-toc-editpt)
	(end-of-line)
	(newline)
	(insert (concat "        "        
					"echo \"<a href=\\\"./"
					new-archive-file
					"\\\">"
					begin-month " "
					begin-yr
					" - "
					end-month " "
					end-yr
					"</a><br>\";"
					))
	(save-buffer)
	(tjic-website-copyfile-to-server nil)

	))

  


(defvar tjic-blog-mode-hook nil "")


(defun tjic-blog-postfacto-reading (reading-str)
  "hack up the existing blog file with a 'reading' entry
BUGS: could perhaps search for 'tjic-blog-datestamp' before 
inserting"
  (interactive "sReading: ")
  (insert (concat tjic-blog-reading-begin 
				  reading-str
				  tjic-blog-reading-end
				  "<br>")))

;;
;; keymap
;;
(defvar tjic-blog-mode-map nil)

(if tjic-blog-mode-map 
    nil
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map text-mode-map) 
	(define-key map "\C-c\C-c" 'tjic-blog-post)
	(define-key map "\C-c\C-n" 'tjic-insert-Nick-url)
	(define-key map "\C-c\C-p" 'tjic-blog-header-private-toggle)
	(define-key map "\C-c\C-q" 'tjic-blog-quote-region)
	(define-key map "\C-c\C-l" 'tjic-blog-ullist-region)
	(define-key map "\C-c\C-u" 'tjic-blog-pretty-url)
	(define-key map "\C-c\C-fr" 'tjic-blog-header-goto-reading)
	(define-key map "\C-c\C-fl" 'tjic-blog-header-goto-listening)
	(setq tjic-blog-mode-map map)))



(defun tjic-blog-mode ()
  "This mode allows the creation and posting of a single blog entry.
It is based on text-mode, with a few additions.

The following commands navigate to the headers:

	\\[tjic-blog-header-goto-reading] - go to the 'reading' commentary field.
	\\[tjic-blog-header-goto-listening] - go to the 'listening' commentary field.

The following command changes the values stored in the headers:

    \\[tjic-blog-header-private-toggle] - toggles the 'private' field.

The following commands edit sections of the buffer:

	\\[tjic-blog-quote-region] - seperates pars with seperators, prepends and postpends.
	\\[tjic-blog-ullist-region] - turns region into a <ul>...</ul> list.
	\\[tjic-blog-pretty-url] - edits a URL to be an HTML HREF, using the URL as text.

Other:

    \\[tjic-blog-post] - posts the blog entry.
"
  (interactive)


  (auto-fill-mode)

  (kill-all-local-variables)
  (use-local-map tjic-blog-mode-map)

  ;; use the abbreviation table from text-mode
  (setq local-abbrev-table text-mode-abbrev-table)

  (setq mode-name "tjic-blog-mode")
  (setq major-mode 'tjic-blog-mode)
  (run-hooks tjic-blog-mode-hook))



(defun tjic-insert-Nick-url ()
  ""
  (interactive)
  (insert "<a href=\"http://www.polarangel.org\">Nick</a>"))


; process-connection-type
; (list-processes)

(defun tjic-kill-all-processes ()
  "kill all processes in emacs; does not kill associated buffers
3 Apr 2002; TJIC"
  (interactive)
  (while (process-list)
	(kill-process (car (process-list)))))

;;
;; local-abbrev-table

(provide 'tjic-blog)
;;; tjic-blog.el ends here
