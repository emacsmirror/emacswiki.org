;;; simple-wiki-recent-changes.el --- view recent changes 

;; Copyright (C)  2003  Pierre Gaston

;; Author: Pierre Gaston <pgas@intracom.gr>
;; Maintainer: 
;; Version: 1.0.1
;; Keywords: hypermedia
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?SimpleWikiEditMode

;; This file is not part of GNU Emacs.

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

;; Commentary:
;;
;; process the recent change page of some usemod wikis
;;
;; Usage example:
;; in your .emacs add
;; (require 'simple-wiki-recent-changes)
;; (defun emacswiki-rc ()
;;   (interactive)
;;   (simple-wiki-rc "http://www.emacswiki.org/cgi-bin/wiki.pl" 'swc-usemod-wiki-save  1)
;;  )
;;then M-x emacswiki-rc RET for Recent Changes during last day
;;other example
;;(defun gnufans-rc ()
;;   (interactive)
;;   (simple-wiki-rc "http://www.gnufans.net/cgi-bin/fsedu.pl" 'swc-usemod-wiki-save 7)
;;  )
;; swc-usemod-wiki-save comes from simple-wiki-completion.el
;; and is the function that posts the change

;;; Code:


(require 'http-get)
(require 'simple-wiki-edit )

(defvar simple-wiki-rc-previous-chunk ""
   "Used to deal with incomplete lines when processing")

;;we defined a mode to override the save function
(define-derived-mode simple-wiki-recent-changes-mode simple-wiki-edit-mode  "Wiki-Recent"
  "display recent changes'.

\\{simple-wiki-recent-changes-mode-map}"
  )



(defun simple-wiki-rc (base save-func &optional days)
  "present a simple recent changes list" 
   (let* ( 
	  (url	(format "%s%s%d" base "?action=rc&days=" days))
	  (save-key-list (where-is-internal 'simple-wiki-save simple-wiki-edit-mode-map ) )
	  )
    
     (http-get url)
     (simple-wiki-recent-changes-mode)

     (add-hook 'http-filter-pre-insert-hook 'simple-wiki-rc-filter nil t)
   
     ;; redefine save keys for this mode
     (while save-key-list 
       (define-key simple-wiki-recent-changes-mode-map (car save-key-list) 'simple-wiki-rc-save)
       (setq save-key-list (cdr save-key-list))
       )

     ;;change the url so that simple-wiki-follow works 
     (set (make-local-variable 'simple-wiki-url) (concat base "?action=browse&raw=2&id="))
     (set (make-local-variable 'simple-wiki-save-function) (symbol-value 'save-func)) 
     
     (set (make-local-variable 'simple-wiki-time) (current-time))
     (set (make-local-variable 'simple-wiki-rc-previous-chunk) "")

 )
)

(defun simple-wiki-rc-save ()
  (interactive)
  (error "Use simple-wiki-browse if you want to edit RecentChanges")
)


(defun simple-wiki-rc-filter () 
   "filter for recent change lines"
   (let* ( (output (concat  simple-wiki-rc-previous-chunk string) ) (list-lines (split-string output "[\n\r]+")) )
      (setq string "")
     
 
     (while (cdr list-lines) 
       (if (string-match "<li><a.*(diff)</a>.*<a.*>\\(.*\\)</a>\\(.*\\)([0-9]* <a" (car list-lines))
           (progn
	     (setq string (concat 
			 string 
			 (match-string 1 (car list-lines)) 
			 " " 
			 (match-string 2 (car list-lines))
			 " "
			 )
		   )

	     (if (string-match "<strong>\\(.*\\)</strong>" (car list-lines))    
		 (setq string (concat 
			       string 
			       (match-string 1 (car list-lines)) 
			       )
		       )
	       )
	     
	      
	      (if (string-match "\\. \\. \\. \\. \\. <a.*>\\(.*\\)</a>" (car list-lines))
		 (setq string (concat 
			       string 
			       "\n            . . . . "
			       (match-string 1 (car list-lines)) 
			       )
		       )
		  (if (string-match "\\. \\. \\. \\. \\. \\(.*\\)" (car list-lines)) 
		      (setq string (concat 
				    string 
				    "\n            . . . . "
				    (match-string 1 (car list-lines)) 
				    )
			    )
		    )
	       )
	      (setq string (concat  string "\n"))
	      )
	 
	 )
       (if  (string-match "<p><strong>\\(.*\\)</strong>" (car list-lines))   
		 (setq string (concat 
			       string 
			       "\n"
			       (match-string 1 (car list-lines)) 
			       "\n\n"
			       )
		       )
	       )
       (if (string-match "<h2>\\(Updates in the last.*\\)</h2>" (car list-lines)) 
	    (setq string (concat 
			       string 
			       (match-string 1 (car list-lines)) 
			       "\n"
			       )
		       )
	 )

        (if (string-match "List new changes starting from</a>\\(.*\\)<br>" (car list-lines)) 
	    (setq string (concat 
			       string 
			       "List new changes starting from: "
			       (match-string 1 (car list-lines)) 
			       "\n"
			       )
		       )
	 )
       (setq list-lines (cdr list-lines))
      
       )
     (setq simple-wiki-rc-previous-chunk (car list-lines))
     
     
     )
)

(provide 'simple-wiki-recent-changes)

;;; simple-wiki-recent-changes.el ends here
