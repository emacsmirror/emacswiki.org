;;; mailrc.el --- edit mailrc aliases

;; Author: Joakim Hove <joakim.hove@phys.ntnu.no>
;; Keywords: mailrc, adressbook

;; Copyright (C) 2000 Joakim Hove

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with your copy of Emacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Installation:
;; -------------
;; Mailrc-mode is intended to be used in conjunction with
;; message-mode for composing messages, I therefor suggest
;; the following installation procedure:
;;
;; 1. Byte compile mailrc.el with M-x byte-compile-file, and 
;;    move the resulting mailrc.elc to the same directory as 
;;    you have message.elc.
;;
;;    You can of course put the mailrc.elc anywhere in your 
;;    emacs load path. If you want to add a directory to your
;;    emacs load path you can do that with the following in your
;;    .emacs file:
;;	  
;;	    (setq load-path (cons "~/personal-lisp" load-path))
;;	
;;    This statement will add the directory ~/personal-lisp to 
;;    (the front of) your load-path list.
;;
;; 
;; 2. Add the following autoload statement to your .gnus or
;;    .emacs file:
;; 
;;	 (autoload 'mailrc-mode "mailrc" "Load the file \"mailrc.el(c)\" when the command mailrc-mode is invoked." t) 
;;
;;
;; 3. Finally I suggest to add a keybinding like the following, 
;;    so that mailrc-mode can be quickly invoked from message-mode:
;;    
;;    (add-hook 'gnus-started-hook '(lambda () 
;;    		(define-key message-mode-map "\C-ca" 'mailrc-mode)))
;;
;;
;; Then you should be ready to test it, type M-x mailrc-mode from
;; any mode, and hopefully mailrc-mode starts up. Finally you can go 
;; to message-mode to compose a message and try the keybinding defined
;; under 3. above.
;;
;; I hope it works for you - "Joakim Hove <hove@phys.ntnu.no>"

;;; Code:

(defgroup mailrc nil 
  "mailrc-mode for editing mailrc aliases"
  :group 'message)


(defcustom mailrc-global-mailrc-file "~/.mailrc"
"Name of the file containing mail aliases to be used in conjunction with mailrc-mode.
The file should be given with full path, otherwise emacs will not find if not started
in the same directory as the file lives.

This file may contain further references to other files, these references must be 
given with a relative path originating from the location of this file.

Typical content of the this file might be:

 source aliases/friends
 source aliases/work
 alias joe     \"Joe Smith <joe@company.com>\"
 alias bill    \"Bill Jones <bill@ucla.edu>\"
 alias friends joe,bill

Default value is \"~/.mailrc\"."
  :group 'mailrc
  :type '(file :must-match t))


(defcustom mailrc-functional-mode-regexp "\\(message-mode\\|mail-mode\\)"
"Regular expression containing the name of the modes where alias expansion is used.
Default value is \"\\(message-mode\\|mail-mode\\)\".

If mailrc-mode is invoked from a mode which does *not* match this regexp no alias
expansion is offered."
  :group 'mailrc
  :type  'regexp)


(defcustom mailrc-postinit-hook nil
"Hook to run after mailrc-mode has successfully loaded"
  :group 'mailrc
  :type  'hook)



;; These are global variabales used across the various mailrc-functions. They are
;; all given sensible values before use - so the defvar -> nil below is just to avoid 
;; compilation warnings.

(defvar mailrc-running-xemacs              (string-match "XEmacs\\|Lucid" emacs-version))
(defvar mailrc-mode-map                    nil)
(defvar mailrc-filter-mode-map             nil)
(defvar mailrc-currently-composing-message nil)
(defvar mailrc-modified-files              nil)
(defvar mailrc-mail-buffer                 nil)
(defvar mailrc-view-buffer                 nil)
(defvar mailrc-hash                        nil)
(defvar mailrc-alias-list                  nil)
(defvar mailrc-expanded                    nil)
(defvar mailrc-max-alias-width             nil)
(defvar mailrc-file-list                   nil)
(defvar mailrc-comments-list               nil)
(defvar mailrc-file-source-hash            nil)
(defvar mailrc-aliaslist-loaded            nil)
(defvar mailrc-aliaslist-loadtime          nil)
(defvar mailrc-alias-regexp                nil)
(defvar mailrc-expansion-regexp            nil)
(defvar mailrc-version                     0.5)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <utility functions>
;; These are simple general functions used by the mailrc spesific
;; functions further down. If you have suggestions to improvements
;; on these functions I would be more than happy to accept them.
;; Joakim Hove - hove@phys.ntnu.no
;;
;;
;; Cristoph Conrad have contributed the functions:
;; remove-element-from-list
;; insert-element-in-list
;; nthcar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; From: "Stefan Monnier <foo@acm.com>" <monnier@flint.cs.yale.edu>
(defun remove-space (string)
  (if (string-match "^[ \t]*\\(.*[^ \t]\\)[ \t]*$" string)
      (match-string 1 string) ""))


(defun nthcar( n list )
  "Return first N elements of LIST."
  (reverse (nthcdr (- (length list) n) (reverse list))))

(defun remove-element-from-list (list N)
  "Remove element at position N in LIST."
  (append (nthcar N list) (nthcdr (1+ N) list)))

(defun insert-element-in-list (list new-element N)
  "Insert NEW-ELEMENT before element at position N in LIST."
  (append (nthcar N list) (list new-element) (nthcdr N list)))

(defun list-contains (list element)
  "Return t if list contains element - otherwise nil"
  (let ((contains))
    (dolist (e list)
      (if (string= e element)
	  (setq contains 't)
	)
      )
    contains
    )
  )




(defun current-line ()
  "Return the vertical position of point..."
  (interactive)
  (beginning-of-line)
  (1+ (count-lines 1 (point)))
  )


(defun safe-cons (list e)
  (if (not list)
      (cons e ())
    (cons e list)
    )
  )
  
    


;; </utility functions>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <interactive functions>

(setq debug-on-error nil)
(defun mailrc-eol-point ()
  (end-of-line)
  (point))


(defun mailrc-no-message-error ()
  (message "This function only works when mailrc-mode was initially invoked from a buffer in %s mode" mailrc-functional-mode-regexp)
)

;; The call to clear limits ensures that these four functions
;; all operate on the complete alias list - not necessarily the smartest?
(defun mailrc-narrow-expansion-substring ()
  (interactive)
  (mailrc-clear-limits)
  (let ((expansion-regexp (regexp-quote (read-from-minibuffer "Give string which should be part of expansion: "))))
    (if (> (mailrc-match-count nil expansion-regexp) 0)
	(progn
	  (setq mailrc-expansion-regexp expansion-regexp)
	  (mailrc-print-buffer))
      (message "There were no expansions containing the substring \"%s\"" expansion-regexp))))


(defun mailrc-narrow-expansion-regexp ()
  (interactive)
  (mailrc-clear-limits)
  (let ((expansion-regexp (read-from-minibuffer "Give regexp to match against expansion: ")))
    (if (> (mailrc-match-count nil expansion-regexp) 0)
	(progn
	  (setq mailrc-expansion-regexp expansion-regexp)
	  (mailrc-print-buffer))
      (message "There were no expansions mathcing the regexp \"%s\"" expansion-regexp))))


(defun mailrc-narrow-alias-substring ()
  (interactive)
  (mailrc-clear-limits)
  (let ((alias-regexp (regexp-quote (read-from-minibuffer "Give string which should be part of alias: "))))
    (if (> (mailrc-match-count alias-regexp nil) 0)
	(progn
	  (setq mailrc-alias-regexp alias-regexp)
	  (mailrc-print-buffer))
      (message "There were no aliases containing the substring \"%s\"" alias-regexp))))


(defun mailrc-narrow-alias-regexp ()
  (interactive)
  (mailrc-clear-limits)
  (let ((alias-regexp (read-from-minibuffer "Give regexp to match against alias: ")))
    (if (> (mailrc-match-count alias-regexp nil) 0)
	(progn
	  (setq mailrc-alias-regexp alias-regexp)
	  (mailrc-print-buffer))
      (message "There were no aliases matching the regexp \"%s\"" alias-regexp))))

(defun mailrc-clear-limits ()
  (setq mailrc-alias-regexp nil)
  (setq mailrc-expansion-regexp nil))


(defun mailrc-expand-buffer ()
  (interactive)
  (mailrc-clear-limits)
  (mailrc-print-buffer))



(defun mailrc-insert-alias-at-line ()
  "To be used in mailrc-mode. Inserts (and expands) the alias at the current line
of the mailrc buffer in the mail window."
  (interactive)
  (if mailrc-currently-composing-message 
      (progn
	(mailrc-save-files)
	(beginning-of-line)
	(re-search-forward "^\\([^\\( \\|\t\\)]+\\)" (point-max) t)
	(let ((alias (match-string 1)))
	  (set-buffer mailrc-mail-buffer)
	  (end-of-line)
	  (let ((start-point (point)))
	    (beginning-of-line)
	    (if (re-search-forward "@" start-point t)
		(progn
		  (save-excursion
		    (if (re-search-forward "," (point-max) t) (delete-region (1- (point)) (mailrc-eol-point)))
		    )
		  (end-of-line)
		  (re-search-backward "[^\\( \\|\t\\)]" (point-min) t)
		  (forward-char 1)
		  (insert ",\n    ")
		  )
	      (progn
		(beginning-of-line)
		(skip-chars-forward "^:")
		(delete-region (+ 2 (point)) (mailrc-eol-point)))
	      )
	    )
	  (insert alias)
	  (expand-abbrev)
	  )
	(switch-to-buffer mailrc-view-buffer)
	(next-line 1)
	(beginning-of-line)
	)
    (mailrc-no-message-error)
    )
  )


(defun mailrc-insert-alias-at-line-and-quit ()
"To be used in mailrc-mode. Inserts (and expands) the alias at the current line
 the mailrc buffer in the mail window, and disposes of the mailrc buffer/window."
(interactive)
(if mailrc-currently-composing-message
    (progn
      (mailrc-insert-alias-at-line)
      (mailrc-quit)
      (mailrc-next-header)
      )
  (mailrc-no-message-error)
  )
)


(defun mailrc-insert-alias-at-line-and-resume ()
  "To be used in mailrc-mode. Inserts (and expands) the alias at the current line
 the mailrc buffer in the mail window, and jumps to message buffer/window for 
continued editing of the mail message."
  (interactive)
  (mailrc-insert-alias-at-line)
  (other-window 1)
  (mailrc-next-header)
)


(defun mailrc-next-header ()
  (if mailrc-currently-composing-message
      (if (re-search-forward "^\\w+:" (point-max) t)
	  (forward-char 1)
	(message "Could not find the next header ...")
	)
    )
  )
  


(defun mailrc-quit ()
  "Quits mailrc-mode. Offering to save to disk if the aliases have been
changed. Killing temporary buffers/windows and jumping to the next header
if we are currently composing a mail-message."
  (interactive)
  (if mailrc-modified-files
      (if (y-or-n-p "Mail aliases have been modifed - save? ") (mailrc-save-files))
    )
  (switch-to-buffer mailrc-mail-buffer)
  (delete-other-windows)
  (kill-buffer mailrc-view-buffer)
  )



(defun mailrc-edit-alias-at-line ()
"Edit the alias defined at the current line. If the present alias is 
 the form :

alias joe \"Joe Smith <joe@mail.com>\"

i.e. with a real name followed by a mail adress in <...> you are offered
to edit real name and email adress seperately. Does unfortunately *not* 
allow for changing the alias to use. In that case you will have to delete
the current alias, and then add a new one afterwards."
(interactive)
(let ((alias (mailrc-get-alias-at-line)))
  (if alias 
      (progn
	(let* ((value (nth 0 (mailrc-gethash alias mailrc-hash)))
	       (file  (nth 2 (mailrc-gethash alias mailrc-hash)))
	       (match-index (string-match "^\\([^<>]\\)+<\\([^ <>]\\)+>$" value))
	       (newexpansion)
	       (newvalue))
	  (if match-index
	      (progn
		(let* ((match-index (string-match "<" value))
		       (old-real-name (substring value 0 (- match-index 1)))
		       (old-email-adress (substring value (+ 1 match-index) -1))
		       (real-name (read-from-minibuffer    (format " Real name for [alias \"%s\"] : " alias) old-real-name nil nil nil old-real-name))
		       (email-adress (read-from-minibuffer (format " e-mail adress for [alias \"%s\"] : " alias) old-email-adress nil nil nil old-email-adress)))
		  (if (and (> (string-width real-name) 0) (> (string-width email-adress) 0))
		      (setq newvalue (concat real-name " <" email-adress ">"))
		    (progn
		      (if (equal (string-width real-name) 0) 
			  (setq newvalue email-adress)
			(setq newvalue real-name))
		      )
		    )
		  )
		)
	    (setq newvalue (read-from-minibuffer (format " Expansion for [alias \"%s\"] : " alias) value nil nil nil value))
	    )
	  (mailrc-modified-alias alias)
	  (mailrc-sethash alias (cons newvalue (list "" file)) mailrc-hash)
	  (mailrc-redisplay-alias-at-line alias)
	  )
	)
    (progn
      (ding)
      (message "No alias is defined on the current line ")
      )
    )
  )
)

  

(defun mailrc-modified-file (file)
  (if (not (list-contains mailrc-modified-files file))
      (setq mailrc-modified-files (safe-cons mailrc-modified-files file))
    )
  )


(defun mailrc-modified-alias (alias)
  (let ((alias-file (nth 2 (mailrc-gethash alias mailrc-hash))))
    (mailrc-modified-file alias-file)
    )
  )





(defun mailrc-delete-alias-at-line ()
  "Deletes the alias at the current line i the mailrc buffer. Actual 
deletion from disk is not before a call to \"mailrc-save-files\""
  (interactive)
  (let* ((alias    (mailrc-get-alias-at-line))
	 (alias-nr (mailrc-alias-number alias)))
    (setq mailrc-alias-list (remove-element-from-list mailrc-alias-list alias-nr))
    (let ((final-line (current-line)))
      (mailrc-print-buffer)
      (goto-line final-line)
      )
    (mailrc-modified-alias alias)
    )
  )

(defun mailrc-make-alist (list)
;; For some reason the function (completing-read) wants
;; an _alist_ to chose completions from. This function
;; takes the list (e1 e2 e3 ..) , and returns the alist
;; ((e1 "cdr-alist") (e2 "cdr-alist") (e3 "cdr-alist") ....)
  (let ((alist '()))
    (dolist (e list)
	    (setq alist (cons (cons e (list "cdr-alist")) alist))
	    )
    alist
    )
  )


(defun mailrc-new-alias ()
  "Inserts a new alias. Prompts for alias to use and the
expansion. After the new alias has been entered the window
containing the aliases is redrawn - and the cursor positioned at the
new alias. 

The new alias is *not* written to disk before \"mailrc-save-files\" 
is called."
  (interactive)
  (let* ((new-alias (mailrc-unused-alias  "Alias to use: "))
	 (new-value (read-from-minibuffer (format "Expansion for %s: " new-alias)))
	 (new-file  mailrc-global-mailrc-file)
	 (new-expansion "Not yeat mailrc-expanded"))
    (if (> (length mailrc-file-list) 1)
	(setq new-file  (completing-read (format "File to store %s in : " new-alias) (mailrc-make-alist mailrc-file-list))))
    (mailrc-new-alias-internal new-alias new-value new-file new-expansion)
    (mailrc-print-buffer)
    (beginning-of-buffer)
    (search-forward new-alias (point-max) t)
    )
  )


(defun mailrc-new-alias-internal (new-alias new-value new-file new-expansion)
  (mailrc-sethash new-alias (cons new-value (list new-expansion new-file)) mailrc-hash)
  (setq mailrc-alias-list (mailrc-sort-alias-list (cons new-alias mailrc-alias-list)))
  (mailrc-modified-alias new-alias)
  )



(defun mailrc-toggle-expanded-view ()
	"Toggles whether \"indirect aliases\" should be mailrc-expanded or not. 
Say for instance your .mailrc file contains:

alias joe     \"Joe Smith <joes@mail.com>\"
alias bill    \"Bill Jefferson <bill@ibm.com>\"
alias friends joe,bill

Now the alias \"friends\" can be displayed as either 1 or 2 below:

	1: friends       joe, bill
	2: friends       Joe Smith <joes@mail.com>, Bill Jefferson <bill@ibm.com>.

This function toggles between these two alternatives."
  (interactive)
  (if mailrc-expanded (setq mailrc-expanded nil) (setq mailrc-expanded t))
  (let ((line-number (current-line)))
    (mailrc-print-buffer)
    (goto-line line-number)
    )
  )


(defun mailrc-save-file (file)
;; This function saves the alias file "file" to disk - returning 't if an 
;; actual save has taken place, and nil otherwise.
  (save-excursion
    (let ((doit 't))
      (if (list-contains mailrc-comments-list file)
	  (if (not (y-or-n-p (format "File \"%s\" contains comments - these will be lost when saving. Save anyway? " file)))
	      (setq doit nil)
	    )
	)
      (if doit
	  (progn
	    (if (file-writable-p file) 
		(progn
		  (let ((mailrc-buffer (find-file-noselect file)))
		    (set-buffer mailrc-buffer)
		    (beginning-of-buffer)
		    (delete-region (point-min) (point-max))
		    
		    (let* ((max-alias-width (mailrc-max-alias-width))
			   (expansion-column (+ max-alias-width 8)))
		      (dolist (source-file (mailrc-gethash file mailrc-file-source-hash))
			(insert "source")
			(indent-to-column expansion-column)
			(insert source-file "\n")
			)
		      (dolist (alias mailrc-alias-list)
			(let ((value      (nth 0 (mailrc-gethash alias mailrc-hash)))
			      (alias-file (nth 2 (mailrc-gethash alias mailrc-hash))))
			  (if (string= file alias-file)
			      (progn
				(insert "alias " alias)
				(indent-to-column expansion-column)
				(insert "\""value"\"" "\n")
				)
			    )
			  )
			)
		      )
		    (save-buffer)
		    (mail-abbrevs-setup)
		    )
		  t
		  )
	      (progn
		(mailrc-wait-for-return (format "Can not write to file \"%s\"" file))
		nil
		)
	      )
	    )
	nil
	)
      )
    )
  )

  
(defun mailrc-save-files ()
"This function writes the content of the mailrc-buffer back to disk.
Because the actual expansion of mail aliases in message-mode or mail-mode
is done after parsing the ~/.mailrc file - and unfortunately not from the
hash built up in mailrc-mode - this function must be called before continuing 
with editing a message.

If you edit an alias, the file containing your alias will be appended
to a list (\"mailrc-modified-files\"), then if you continue to edit a
mail-message this function will be called automatically - if you quit
mailrc-mode without expanding any alias in a mail-message you will be
queried wether to save changes to disk."

  (interactive)
  (if mailrc-modified-files
      (progn
	(let ((new-mfiles))
	  (mapcar '(lambda (file) 
		     (if (not (mailrc-save-file file))
			 (setq new-mfiles (safe-cons new-mfiles file))
		       )
		     ) mailrc-modified-files)
	  (setq mailrc-modified-files new-mfiles)
	  )
	(mailrc-touch-global-mailrc-file)
	)
    (message "Aliases unchanged - nothing saved")
    )
  )


(defun mailrc-touch-global-mailrc-file ()
  ;;
  ;; What I really want is somethin like unix "touch" on the ~/.mailrc file
  ;; to force a reread of the aliases - however ...
  ;;
  (save-excursion
    (let ((tmp-buffer (find-file-noselect mailrc-global-mailrc-file)))
      (set-buffer tmp-buffer)
      (set-buffer-modified-p 't)
      (save-buffer tmp-buffer)
      )
    )
  )


(defun mailrc-quote-regexp (regexp)
  (concat "^" (regexp-quote regexp)))


(defun mailrc-alias-match (alias alias-regexp)
  (if alias-regexp
      (string-match alias-regexp alias) 't))


(defun mailrc-expansion-match (alias expansion-regexp)
  (if expansion-regexp
      (string-match expansion-regexp (mailrc-expansion alias))
    't))


(defun mailrc-print-buffer ()
  "This functions prints out the mailrc buffer in nice way"
  (mail-abbrevs-setup)
  (pop-to-buffer mailrc-view-buffer)
  (beginning-of-buffer)
  (toggle-read-only -1)
  (delete-region (point-min) (point-max))
  (let ((max-alias-width (mailrc-max-alias-width)))
    (dolist (alias mailrc-alias-list)
      (if (mailrc-alias-match alias mailrc-alias-regexp)
	  (if (mailrc-expansion-match alias mailrc-expansion-regexp)
	      (mailrc-print-alias-line alias max-alias-width)))
      )
    )
  (toggle-read-only 1)
  (beginning-of-buffer)
  )



;; Er litt For restriktiv:
;; To: Knut@ihf.nlh.no,
;;     ver[TAB]
;;
;; Tar IKKE tak i "ver" ---
;;


(defun message-tab ()
  ;; exgnus
  "Expand group names in Newsgroups and Followup-To headers.
Do a `tab-to-tab-stop' if not in those headers.

This routine is redefined in the exgnus package, as <tab> now lists
possible completions.  Se documentation in the beginning of the
exgnus source file."

  (interactive)
  (if (mail-abbrev-in-expansion-header-p) 
      (mailrc-message-tab)
    (tab-to-tab-stop)))

;;  (if (let ((mail-abbrev-mode-regexp message-newgroups-header-regexp))
;;	  (mail-abbrev-in-expansion-header-p))
;;	(message-expand-group)
;;    (mailrc-message-tab))


;;(defun message-tab ()
;;  ;; exgnus
;;  "Expand group names in Newsgroups and Followup-To headers.
;;Do a `tab-to-tab-stop' if not in those headers.
;;
;;This routine is redefined in the exgnus package, as <tab> now lists
;;possible completions.  Se documentation in the beginning of the
;;exgnus source file."
;;
;;;;
;;;; Spiser [TAB] i *hele* message bufferet.
;;;;
;;
;;  (interactive)
;;  (if (let ((mail-abbrev-mode-regexp message-newgroups-header-regexp))
;;	  (mail-abbrev-in-expansion-header-p))
;;	(message-expand-group)
;;    (mailrc-message-tab)))



(defun mailrc-alias-substring ()
  (let* ((b (save-excursion
	      (save-restriction
		(narrow-to-region
		 (save-excursion
		   (beginning-of-line)
		   (skip-chars-forward "^\\(:\\|,\\)")
		   (1+ (point)))
		 (point))
		(skip-chars-backward "^, \t\n") (point))))
	 (completion-ignore-case t)
	 (string (buffer-substring b (progn (skip-chars-forward "^,\t\n ")
					    (point))))
	 )
    string
    )
  )


(defun mailrc-match-count (alias-regexp expansion-regexp)
  (mailrc-init-hash)
  (let ((match-count 0))
    (dolist (alias mailrc-alias-list)
      (if (and (mailrc-alias-match alias alias-regexp) (mailrc-expansion-match alias expansion-regexp))
	  (setq match-count (1+ match-count))))
    match-count))


(defun mailrc-tab-expand-alias (alias)
  (expand-abbrev))


(defun mailrc-complete-alias (alias-substring)
  (let ((complete-alias))
    (dolist (alias mailrc-alias-list)
      (if (mailrc-alias-match alias (mailrc-quote-regexp alias-substring)) (setq complete-alias alias)))
    (insert (substring complete-alias (length alias-substring))))
  )


(defun mailrc-message-tab ()
  (let ((alias-substring (mailrc-alias-substring)))
    (if alias-substring
	(let* ((substring-regexp (mailrc-quote-regexp alias-substring))
	       (match-count      (mailrc-match-count substring-regexp nil)))
	  (if (> match-count 0)
	      (if (= match-count 1)
		  ;; Found only one alias in the list which mathced the given characters ....
		     (if (mailrc-gethash alias-substring mailrc-hash)
			 (mailrc-tab-expand-alias alias-substring)          ;; It is complete.
			 (mailrc-complete-alias alias-substring))           ;; It is unique - but not complete.
		;;
		;; More than one alias matching ....
		(mailrc-mode (concat "^" alias-substring)))
	    (message "%s does not match any alias - sorry" alias-substring))))))


;;Make it conditional whether to jump to next header or not ....




(defun mailrc-redisplay-alias-buffer ()
  (interactive)
  (mailrc-print-buffer)
  )

;; </interactive functions>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;(defun mailrc-alias-at-current-line ()
;;  (save-excursion
;;    (beginning-of-line)
;;    (let ((p0 (point)))
;;	(search-forward " ")
;;	(buffer-substring-no-properties p0 (1- (point))))
;;    )
;;  )


(defun mailrc-get-alias-at-line ()
  (end-of-line)
  (let ((end-of-line-point (point)))
    (beginning-of-line)
    (if (re-search-forward "^\\([^\\( \\|\t\\)]+\\)" end-of-line-point t)
	(match-string 1)
      nil
      )
    )
  )

	    

(defun mailrc-redisplay-alias-at-line (alias)
  (let ((alias (mailrc-get-alias-at-line)))
    (if alias
	(progn
	  (toggle-read-only -1)
	  (beginning-of-line)
	  (kill-line 1)
	  (mailrc-print-alias-line alias (max mailrc-max-alias-width (string-width alias)))
	  (previous-line 1)
	  (toggle-read-only 1)
	  )
      )
    )
  )




(defun mailrc-alias-number (alias)
  (let ((alias-nr)
	(index 0))
    (dolist (a mailrc-alias-list)
      (if (string= alias a)
	  (setq alias-nr index))
      (setq index (1+ index)))
    alias-nr))
  

	      

(defun mailrc-expand-mail-abbrev (alias)
  (if mailrc-running-xemacs (format "%s" (abbrev-expansion alias mail-aliases))
    (format "%s" (abbrev-expansion alias mail-abbrevs)))
  )


(defun mailrc-expansion (alias)
  (nth 1 (mailrc-gethash alias mailrc-hash))
  )



(defun mailrc-print-alias-line (alias max-alias-width)
  (let ((expansion (format "%s" (if mailrc-expanded (mailrc-expand-mail-abbrev alias)
				  (nth 0 (mailrc-gethash alias mailrc-hash)))))
	(expansion-width (- (frame-width) (+ max-alias-width 5)))
	(file (nth 2 (mailrc-gethash alias mailrc-hash)))
	(expansion-column (+ max-alias-width 2)))
    (if (> (string-width (format "%s" expansion)) (- expansion-width 2))
	(setq expansion (concat (substring (format "%s" expansion) 0 (- expansion-width 3)) "...")))
    (let ((P1 (point)))
      (insert alias " ")
      (put-text-property P1 (- (point) 1) 'face 'bold)
      )
    (indent-to-column expansion-column)
    (if mailrc-expanded 
	(insert expansion "\n")
      (progn
	(let* ((expansion-list (split-string expansion "[ ]*,[ ]*"))
	       (current-pos (point))
	       (joined-expansion "")
	       (current-expansion-nr 0)
	       (current-expansion (nth current-expansion-nr expansion-list))
	       (max-expansion-width (- (frame-width) (+ expansion-column 6)))
	       (highlight-list))
	  (while (and (< current-expansion-nr (length expansion-list)) (< (+ (string-width joined-expansion) (string-width current-expansion)) max-expansion-width))
	    (if (> (string-width joined-expansion) 0)
		(setq joined-expansion (concat joined-expansion ", " current-expansion))
	      (setq joined-expansion current-expansion))
	    (if (mailrc-gethash current-expansion mailrc-hash)
		(setq highlight-list (cons (list current-pos (+ current-pos (string-width current-expansion))) highlight-list))
	      )
	    (setq current-pos (+ current-pos (string-width current-expansion) 2))
	    (setq current-expansion-nr (1+ current-expansion-nr))
	    (setq current-expansion (nth current-expansion-nr expansion-list))
	    )
	  (if (< current-expansion-nr (length expansion-list))
	      (setq joined-expansion (concat joined-expansion " ...")))
	  (insert joined-expansion "\n")
	  (let ((end-of-line-point (point)))
	    (dolist (hl highlight-list)
	      (if (< (nth 1 hl) end-of-line-point)
		  (put-text-property (nth 0 hl) (nth 1 hl) 'face 'font-lock-warning-face)
		)
	      )
	    )
	  )
	)
      )
    )
  )
	      



(defun mailrc-max-alias-width ()
  (let ((max-alias-width 0))
    (dolist (alias mailrc-alias-list)
      (if (< max-alias-width (string-width alias))
	  (setq max-alias-width (string-width alias))
	)
      )
    (setq mailrc-max-alias-width max-alias-width)
    max-alias-width
    )
  )


	    


(defun mailrc-load-mailrc-file (file)
  (message "Loading \"%s\" ...." file)
  (save-excursion
    (if (list-contains mailrc-file-list file)
	(progn
	  (ding)
	  (mailrc-wait-for-return (format "Circualar sourcing of file \"%s\"" file))
	  )
      (progn
	(if (file-readable-p file)
	    (progn
	      (setq mailrc-view-buffer "*mailrc*")  
	      (let ((mailrc-buffer (find-file-noselect file)))
		(get-buffer-create "*tmp*")
		(set-buffer "*tmp*")
		(delete-region (point-min) (point-max))
		(insert-file file)
		(untabify (point-min) (point-max))
		(beginning-of-buffer)
		(end-of-line)
		(let ((end-of-line-point (point)))
		  (while (< (point) (point-max))
		    (beginning-of-line)
		    (while (re-search-forward "\\\\\\(\\( \\|\t\\)*\\)$" end-of-line-point t)
		      (re-search-backward "[^\\( \\|\t\\)]")
		      (delete-char 1)
		      (re-search-backward "[^\\( \\|\t\\)]" (point-min) t)
		      (forward-char 1)
		      (kill-line 1)
		      (let ((P1 (point)))
			(if (re-search-forward "[^\\( \\|\t\\)]" (point-max) t)
			    (progn
			      (backward-char 2)
			      (delete-region P1 (point))
			      )
			  )
			(setq P1 (point))
			(end-of-line)
			(setq end-of-line-point (point))
			(goto-char P1)
			)         
		      )
		    (next-line 1)
		    (end-of-line)
		    (setq end-of-line-point (point))
		    )
		  )
		(beginning-of-buffer)
		(if (search-forward "#" (point-max) t)
		    (setq mailrc-comments-list (safe-cons mailrc-comments-list file))
		  )
		(beginning-of-buffer)
		
		;;Denne REGEXP MISTER EN DEL ....
		(while (re-search-forward "^alias\\( \\|\t\\)+\\([^\\( \\|\t\\)]+\\)\\(\\( \\|\\)+\\)\"\\(.+\\)\"" (point-max) t)
		  (let* ((alias (match-string 2))
			 (value (remove-space (match-string 5)))
			 (expansion (mailrc-expand-mail-abbrev alias)))
		    (mailrc-sethash alias (cons value (list expansion file)) mailrc-hash)
		    (setq mailrc-alias-list (cons alias mailrc-alias-list))
		    )
		  )
		(setq mailrc-file-list (safe-cons mailrc-file-list file))
		;; Is it is possible that there is some confusion as to wether paths are
		;; included or not ???

		;; Got to nest up all the source <filename>
		;; directives in the mailrc-files.
		(beginning-of-buffer)
		(let ((source-list)
		      (path (file-name-directory file)))
		  (while (re-search-forward "^source\\(\\ \\)+\"?\\(\\([/~A-Z0-9a-z_.-]\\)+\\)\"?$" (point-max) t)
		    (let ((source-file (match-string 2)))
		      (if (not (file-name-absolute-p source-file))
			  (setq source-file (concat path source-file)))
		      (setq source-list (safe-cons source-list source-file))
		      )
		    )
		  (mailrc-sethash file source-list mailrc-file-source-hash)
		  (dolist (source-file source-list)
		    (mailrc-load-mailrc-file source-file)
		    )
		  )
		)
	      )
	  (progn
	    (ding)
	    (if (file-exists-p file)
		(mailrc-wait-for-return (format "Can not read sourcefile \"%s\" " file))
	      (mailrc-wait-for-return (format "Can not find sourcefile \"%s\" " file))
	      )
	    )
	  )
	)
      )
    )
  )


;; This function is intented to display a message
;; which the user must acknowledge by pressing <return>.
(defun mailrc-wait-for-return (message)
  (read-from-minibuffer (format "%s <return> " message))
)

(defun mailrc-time> (t1 t2)
  (let ((gt))
    (if (> (car t1) (car t2))
	(setq gt 't)
      (if (= (car t1) (car t2))
	  (if (> (nth 1 t1) (nth 1 t2))
	      (setq gt 't))))
    gt)
  )



(defun mailrc-reload-required ()
  (let ((reload-required))
    (if mailrc-aliaslist-loaded
	(dolist (file mailrc-file-list)
	  (if (mailrc-time> (nth 5 (file-attributes file)) mailrc-aliaslist-loadtime) 
	      (setq reload-required 't))
	  )
      (setq reload-required 't))
    reload-required))
    


(defun mailrc-init-hash ()
  (if (mailrc-reload-required)
      (progn
	(setq mailrc-hash (mailrc-make-hashtable 4096))
	(setq mailrc-alias-list nil)

	(setq mailrc-file-list nil)
	(setq mailrc-file-source-hash (mailrc-make-hashtable 32))

	(setq mailrc-comments-list nil)
	(mailrc-load-mailrc-file mailrc-global-mailrc-file)
	(if mailrc-comments-list
	    (mailrc-wait-for-return "Comment(s) starting with \"#\" were found - these will be lost if you edit/save the aliases.")
	  )
	(setq mailrc-alias-list (mailrc-sort-alias-list mailrc-alias-list))

	(setq mailrc-aliaslist-loaded 't)
	(let ((ct (current-time)))
	  (setq mailrc-aliaslist-loadtime (list (car ct) (nth 1 ct))))
	(message "Aliases read from %s " mailrc-file-list)
	)
    )
  )


(defun mailrc-clear ()
  (interactive)
  (setq mailrc-aliaslist-loaded nil))



(defun mailrc-new-alias-file ()
  "This function asks for the name of a new alias file. The file may already exist, 
or it may be a new file. If it is an existing file it is loaded and the aliases
redisplayed."
  (interactive)
  (let* ((mailrc-path (file-name-directory mailrc-global-mailrc-file))
	 (new-file (read-from-minibuffer (format "Name of new alias file (relative to \"%s\"): " mailrc-path))))
    (if (list-contains mailrc-file-list new-file)
	(mailrc-wait-for-return (format "The file \"%s\" is already sourced" new-file))
      (progn
	(message "%s will be sourced from %s" new-file mailrc-global-mailrc-file)
	(let ((old-source-list (mailrc-gethash mailrc-global-mailrc-file mailrc-file-source-hash)))
	  (if old-source-list 
	      (setq old-source-list (cons new-file old-source-list ))
	    (setq old-source-list (cons new-file ())))
	  (mailrc-sethash mailrc-global-mailrc-file old-source-list mailrc-file-source-hash)
	  (mailrc-modified-file mailrc-global-mailrc-file)
	  )
	(if (file-exists-p (concat mailrc-path new-file))
	    (progn
	      (mailrc-load-mailrc-file (concat mailrc-path new-file))
	      (setq mailrc-alias-list (mailrc-sort-alias-list mailrc-alias-list))
	      (mailrc-print-buffer)
	      )
	  )
	)
      )
    )
  )




(defun mailrc-sort-alias-list (list)
  (sort list 'mailrc-alias-compare)
)



(defun mailrc-alias-compare2 (alias1 alias2)
  (let* ((list1 (mailrc-gethash alias1 mailrc-hash))
	 (list2 (mailrc-gethash alias2 mailrc-hash))
	 (file1  (nth 2 list1))
	 (file2  (nth 2 list2))
	 )
    (if (string= file1 file2)
	(string< (downcase alias1) (downcase alias2))
      (string< (downcase file1) (downcase file2))
      )
    )
  )





(defun mailrc-alias-compare (alias1 alias2)
  (string< (downcase alias1) (downcase alias2))
)




(defun mailrc-mode (&optional alias-regexp expansion-regexp)
  "
Mailrc mode - a simple mode to mantain and update the ~/.mailrc file of mail aliases.
-------------------------------------------------------------------------------------
The following six functions are designed for maintaining the ~/.mailrc file:

 * mailrc-edit-alias-at-line   	     [e]  : edit the alias at the current line
 * mailrc-delete-alias-at-line 	     [d]  : delete the alias at the current line
 * mailrc-new-alias            	     [n]  : add a new alias 
 * mailrc-new-alias-file       	     [f]  : add a new file with aliases
 * mailrc-save-files           	     [s]  : save the mailrc file to disk
 * mailrc-toggle-expanded-view 	     [ ]  : toggle expansion of aliases containg aliases
 * mailrc-narrow-expansion-substring [:f] : Only include expansions which contain substring...
 * mailrc-narrow-expansion-regexp    [:F] : Only include expansions which mach regexp ...
 * mailrc-narrow-alias-substring     [:a] : Only include aliases which contain substring...
 * mailrc-narrow-alias-regexp        [:A] : Only include aliases which mach regexp ...
 * mailrc-quit                       [q]  : quit mailrc-mode, offering to save if needed 

The characters in [ ] show the default keybindings. The following
functions are to be used when mailrc-mode is invoked from a message
buffer.

 * mailrc-insert-alias-at-line-and-quit [RET] : 
 ----------------------------------------------
	       Expand the alias at the current line in the mail buffer and quit 
	       this mailrc-mode.

 * mailrc-insert-alias-at-line [a] : 
 -----------------------------------
	       Add the alias at the current line in the mail buffer and continue
	       in mailrc-mode.

 * mailrc-insert-alias-at-line-and-resume [c]:
 ---------------------------------------------
	       Add the alias at the current line in the mail buffer and continue
	       mail editing, while leaving the *mailrc* buffer/window open.

Limitations:
============

The buffer is read-only, so you are not allowed to edit aliases manually, 
i.e. without using the functions listed above. If that is what you wish 
you'd better simply edit the ~/.mailrc the normal way and forget about this 
mode :)

Comments in the source files are ignored, that is they give no headache on
parsing the aliases, but they are NOT written back to disk again. You are
given a warning though, when your comments are about to disappear.

Important variables:
====================
mailrc-global-mailrc-file     
mailrc-functional-mode-regexp

Joakim Hove - hove@phys.ntnu.no
"
  (interactive)
  (if (string-match mailrc-functional-mode-regexp (format "%s" major-mode))
      (setq mailrc-currently-composing-message t)
    (setq mailrc-currently-composing-message nil)
    )
  (setq mailrc-mail-buffer (current-buffer))
  
  (mail-abbrevs-setup)
  (mailrc-init-hash)
  (setq mailrc-max-alias-width (mailrc-max-alias-width))
  (setq mailrc-expanded nil)
  (setq mailrc-alias-regexp alias-regexp)
  (setq mailrc-expansion-regexp expansion-regexp)
  (mailrc-print-buffer)
  (setq mailrc-modified-files nil)
  
  (setq mailrc-mode-map        (make-sparse-keymap))
  (setq mailrc-filter-mode-map (make-sparse-keymap))
  (define-key mailrc-mode-map  " "      'mailrc-toggle-expanded-view)
  (define-key mailrc-mode-map  "e"      'mailrc-edit-alias-at-line)
  (define-key mailrc-mode-map  "d"      'mailrc-delete-alias-at-line)
  (define-key mailrc-mode-map  "n"      'mailrc-new-alias)
  (define-key mailrc-mode-map  "f"      'mailrc-new-alias-file) 
  (define-key mailrc-mode-map  "s"      'mailrc-save-files)
  (define-key mailrc-mode-map  "a"      'mailrc-insert-alias-at-line)
  (define-key mailrc-mode-map  "\r"     'mailrc-insert-alias-at-line-and-quit)
  (define-key mailrc-mode-map  "c"      'mailrc-insert-alias-at-line-and-resume)
  (define-key mailrc-mode-map  "q"      'mailrc-quit)
  (define-key mailrc-mode-map  "r"      'mailrc-redisplay-alias-buffer)
  (define-key mailrc-mode-map  ":"       mailrc-filter-mode-map)


  (define-key mailrc-filter-mode-map "f" 'mailrc-narrow-expansion-substring)
  (define-key mailrc-filter-mode-map "F" 'mailrc-narrow-expansion-regexp)
  (define-key mailrc-filter-mode-map "a" 'mailrc-narrow-alias-substring)
  (define-key mailrc-filter-mode-map "A" 'mailrc-narrow-alias-regexp)
  (define-key mailrc-filter-mode-map " " 'mailrc-expand-buffer)
  (use-local-map mailrc-mode-map)
  (setq major-mode 'mailrc-mode)
  (setq mode-name "mailrc")
  (run-hooks 'mailrc-postinit-hook)
)





(defun mailrc-unused-alias (prompt)
  (interactive)
  (let ((new-alias)
	(expansion))
    (while (not new-alias)
      (setq new-alias (read-from-minibuffer prompt))
      (let ((expansion-list (mailrc-gethash new-alias mailrc-hash)))
	(if expansion-list
	    (progn
	      (ding)
	      (mailrc-wait-for-return (format "Alias \"%s\" already in use:  \"%s -> %s\"" new-alias new-alias (nth 1 expansion-list)))
	      (setq new-alias nil)))))
    (if (string= new-alias "")
	(setq new-alias nil))
    new-alias))


(defun mailrc-add-sender()
  (interactive)
  (let ((sender (mail-fetch-field "from")))
    (if sender
	(progn
	  (let* ((new-alias (mailrc-unused-alias (format "Alias to use for \"%s\": " sender)))
		 (new-file  (completing-read     (format "File to store %s in : " new-alias) (mailrc-make-alist mailrc-file-list))))
	    (mailrc-new-alias-internal new-alias sender new-file sender)
	    (mailrc-save-files)
	    (message "Alias %s added to file %s " new-alias new-file)
	    )
	  )
      (mailrc-wait-for-return "This buffer does not contain a \"From: ...\" header")
      )
    )
  )


(defun mailrc-query (expansion)
  (mailrc-init-hash)
  (let ((found-alias-list)
	(regexp-address))

    (if (string-match ".+<\\([\\w\\.]+@[\\w\\.]\\)>$" expansion)
	(setq regexp-address (regexp-quote (match-string 0)))
      (setq regexp-address (regexp-quote expansion)))
    
    (dolist (alias mailrc-alias-list)
      (if (string-match regexp-address (nth 1 (mailrc-gethash alias mailrc-hash)))
	  (setq found-alias-list (cons alias found-alias-list))
	)
      )
    (if found-alias-list
	(message "The adress: %s is defined in alias(es): %s" expansion found-alias-list)
      (message "The adress: %s is not defined as an alias" expansion))
    )
  )
    
	
(defun mailrc-query-sender ()
  (interactive)
  (mailrc-query (mail-fetch-field "from")))


(defun mailrc-query-adress ()
  (interactive)
  (mailrc-query (read-from-minibuffer "Look for aliases containing : ")))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  



;; The following code has been ripped straight out of the gnus sources, 
;; and renamed replaced s/gnus/mailrc/. This is done so that mailrc-mode
;; should be independent of gnus.

(defun mailrc-make-hashtable (&optional hashsize)
	    (make-vector (if hashsize (max (mailrc-create-hash-size hashsize) 256) 256) 0))

;; Make a number that is suitable for hashing; bigger than MIN and
;; equal to some 2^x.  Many machines (such as sparcs) do not have a
;; hardware modulo operation, so they implement it in software.  On
;; many sparcs over 50% of the time to intern is spent in the modulo.
;; Yes, it's slower than actually computing the hash from the string!
;; So we use powers of 2 so people can optimize the modulo to a mask.
(defun mailrc-create-hash-size (min)
	    (let ((i 1))
	      (while (< i min)
		(setq i (* 2 i)))
	      i))


(defmacro mailrc-gethash (string hashtable)
	    "Get hash value of STRING in HASHTABLE."
	    `(symbol-value (intern-soft ,string ,hashtable)))


(defmacro mailrc-sethash (string value hashtable)
	    "Set hash value.  Arguments are STRING, VALUE, and HASHTABLE."
	    `(set (intern ,string ,hashtable) ,value))
(put 'mailrc-sethash 'edebug-form-spec '(form form form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mailrc)
;;; mailrc.el ends here
