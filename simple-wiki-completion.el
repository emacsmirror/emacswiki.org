;;; simple-wiki-completion.el ---
;; Time-stamp: <2003-03-02 22:53:34 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: simple-wiki-completion.el
;; Package: simple-wiki-completion
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords:
;; Version: 1.0.7
;; Author's homepage: http://gnufans.net/~deego
;; For latest version:
(defconst simple-wiki-completion-home-page
  "http://gnufans.net/~deego")

;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 


;; INSTRUCTIONS: Simply try out one of the example functions 
;; like M-x swc-emacswiki-browse. 

;; Don't forget to get the LATEST http-get.el, http-post.el,
;; simple-wiki-edit.el and simple-wiki.el from
;; http://www.emacswiki.org/elisp/index.html


; change
;;1.07
;; -change to swc-browse so that it is more flexible (Todo completion on nicknmae)
;; -added Pierre Gaston wiki 
;;1.06
;; - added swc-nick-current 
;; - redefine the open function binding to take advantage of completionsw
;; - redefine the follow function binding to take advantage of completions
;; - move buffer renaming in a hook to keep working with open and follow functions

;;TODO
;; make nice variables to be able to define a wiki 
;;  (ie, url, http version proxy ...)

;; See also:

;;==========================================
;;; Requires:

(eval-when-compile (require 'cl))
(require 'http-get);
(require 'http-post)
(require 'simple-wiki-edit)
(require 'simple-wiki)

;; Code:
;;; Real Code:

(defvar simple-wiki-completion-version "1.0.7")

(defvar swc-completions nil)

(defcustom simple-wiki-completion-ignore-case t
  "" )

(defcustom  swc-wikis
  '(
    ("ew"
     "http://www.emacswiki.org/cgi-bin/wiki.pl?action=browse&raw=2&id="
     "http://www.emacswiki.org/cgi-bin/wiki.pl?action=index&raw=1"
     )
   ("om"
     "http://www.emacswiki.org/cgi-bin/oddmuse.pl?action=browse&raw=2&id="
     "http://www.emacswiki.org/cgi-bin/oddmuse.pl?action=index&raw=1"
     )
   ("pierre"
     "http://www.initialsdc.net/cgi-bin/wiki.pl?action=browse&raw=2&id="
     "http://www.initialsdc.net/cgi-bin/wiki.pl?action=index&raw=1"
     )
   
   )
  "" )

(defvar swc-pages nil
  "Not to be confused with `swc-pages-completion'.
Is a list of the form 
 ((code1 ((pg1) (pg2) (pg3...))  (code2 .....)) "
  )

(defvar swc-nick-current nil
  "the current nick name, local to a buffer"
  )

;;redefine the open function to take advantage of completions
(define-key simple-wiki-edit-mode-map (kbd "C-c C-o") 'swc-open)
(define-key simple-wiki-edit-mode-map (kbd "C-c C-g") 'swc-follow)

;; hooks for renaming the buffers  and setting current wiki nickname
(add-hook 'simple-wiki-edit-mode-hook 'rename-hook)
              

(defun rename-hook () 
  (set (make-local-variable 'swc-nick-current) nick)
                (when url
		  (let* (
			 (simple-wiki-url url) 
			 (buffname 
			  (concat (upcase nick) ":" (simple-wiki-page))
			  )
			 )
		    (if (get-buffer buffname) 
			(kill-buffer buffname)
		      )
		    (rename-buffer buffname)
		    ) 
		  )
)

(defun swc-completions-nullify ()
  (interactive)
  (setq swc-pages nil))



(defun swc-completions-make (nick)
  "retrieve the index page associated with nick and build the completion list"
  ;remove existing completions
  (let ((tail swc-pages))
    (while tail
      (if (equal (car (car tail)) nick)
	  (setq swc-pages (delq (car tail) swc-pages)))
      (setq tail (cdr tail)))
    )
  ;look for index page associated with nick
  (let (refpage (wikis  swc-wikis) )
    (while (and wikis (null refpage))
	(setq refpage (third (assoc nick swc-wikis)))
        (setq wikis (cdr wikis))
	)
  (if (null refpage)
      nil
    (let (proc pages  (progress 60))     
      (setq proc (http-get refpage nil  'swc-dumb-sentinel  simple-wiki-http-version))
          (set (make-local-variable 'content) "")
	  ;;add a hook to force the connection to close
	 ( if (= simple-wiki-http-version 1.1)
	  (add-hook 'http-filter-post-insert-hook 'swc-index-filter-hook t t)
      )
	;; wait for the process to end
	;; or wait  60 seconds
        (while (and (eq (process-status proc) 'open)  (> progress 0))
	   (sit-for 1)
	  ;; (setq status  (process-status proc))
	   (setq progress (1- progress) )
	   (message (format "Building completion list %d " progress ))
	  )
	

	;;parse the entries
	(setq pages 
	      (mapcar
	       (lambda (arg) (list arg))
	       (split-string 
		(buffer-string))))
       ;; get rid of thebuffer
      (kill-buffer (process-buffer proc))
      (push (list nick pages) swc-pages)))
    )
  )
  

(defun swc-dumb-sentinel (proc mess) 
  "dumb sentinel used only to keep the buffer hidden"
  ()
  )

(defun swc-index-filter-hook ()
  "not really a filter this hook close the connection if the http1.1 transfer
is not closed"
  ;;this make the asumptions that connection which are not closed are chunked
  ;;(setq content (concat content (buffer-substring (process-mark proc) (point))))
   
       (unless (string= "close" (cdr (assoc "Connection" http-headers)))
	 (unless (string= "chunked" (cdr (assoc "Transfer-Encoding" http-headers)))
	   (delete-process proc)
	   )
	 )
       
       )


(defun swc-completions-get (nick)
  (let ((assoced (assoc nick swc-pages)))
    (unless assoced (swc-completions-make nick))
    (setq assoced (assoc nick swc-pages))
    (second assoced)))

(defvar swc-nick-current nil)
(defvar swc-savefn-current nil)

(defvar swc-tmp-pages nil "temporary variable. ")
(defvar swc-pages-completion nil
  "Within each buffer, this variable shall be bound to a list of all
pages, so dynamic completion works while editing. 
Not to be confused with `swc-pages'
")
(make-variable-buffer-local 'swc-pages-completion)

;;open redefined to take advantage of the completion
(defun swc-open (&optional page)
  "Open a new page on the same wiki."
  (interactive)
  (let* ((nick swc-nick-current)
	 (pages (ignore-errors (swc-completions-get nick)))
	   (completion-ignore-case simple-wiki-completion-ignore-case)
	   (page (completing-read "Page: " pages)))
    (simple-wiki-edit (simple-wiki-link page) simple-wiki-save-function)
   
   )
)
;; follow redefined 
(defun swc-follow ()
  (interactive)
  (let ((nick swc-nick-current))
       (call-interactively 'simple-wiki-follow)
  )
)

(defun swc-browse  (&optional nick page)
   (interactive)
    (if (not nick)
	(setq nick (read-from-minibuffer "Nickname :")))
    
    (if (not page)
	(setq page (let* (
	   (pages (ignore-errors (swc-completions-get nick)))
	   (completion-ignore-case simple-wiki-completion-ignore-case)
	   (matched (completing-read "Page: " pages)))
		    (setq swc-tmp-pages (mapcar 'car pages))
		    matched
		    )))
    
    (simple-wiki-edit 
     (concat 
      (second (assoc nick swc-wikis))
      page)
     (or swc-savefn-current 'swc-usemod-wiki-save)
     )
    (setq swc-pages-completion swc-tmp-pages))
  


;;;###autoload
(defun swc-emacswiki-browse  ()
  (interactive)
    (swc-browse "ew")
    )


(defun swc-oddmuse-browse  ()
  (interactive)
    (swc-browse "om")
    )

(defun swc-pierre-browse  ()
  (interactive)
    (swc-browse "moi")
     )


(defcustom swc-summary-default "*"
  "")


(defun swc-usemod-wiki-save ()
  "Save the current page to a UseMod wiki."
  (let ( (nick swc-nick-current)
	(url simple-wiki-url)
	(save-func simple-wiki-save-function))
    (switch-to-buffer
     (process-buffer
      (http-post
       (simple-wiki-save-link)
       (list (cons "title" (simple-wiki-page))
	     (cons "summary" 
		   (setq swc-summary-default
			 (read-from-minibuffer "Summary: " "*")))
	     '("raw" . "2")
	     (cons "username" (apply 'concat (split-string user-full-name)))
	     (cons "text" (buffer-string))
	     (cons "recent_edit" (simple-wiki-minor-value)))
       simple-wiki-content-type)))
    (simple-wiki-edit-mode)
    (set (make-local-variable 'simple-wiki-url) url)
    (set (make-local-variable 'simple-wiki-save-function) save-func)))

  
  
(provide 'simple-wiki-completion)
(run-hooks 'simple-wiki-completion-after-load-hooks)



;;; simple-wiki-completion.el ends here
