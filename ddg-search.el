;;; ddg-search.el --- 
;; 
;; Filename: ddg-search.el
;; Description: 
;; Author: Christian Giménez
;; Maintainer: 
;; Created: sáb mar  2 20:33:12 2013 (-0300)
;; Version: 
;; Last-Updated: jue mar 21 02:12:50 2013 (-0300)
;;           By: Christian
;;     Update #: 39
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   Cannot open load file: ddg-search.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 21-Mar-2013    Christian  
;;    Last-Updated: jue mar 21 01:57:40 2013 (-0300) #37 (Christian)
;;    Adding `duckduckgo-web' for searching in the web and
;;    `duckduckgo-emacswiki' for searching inside EmacsWiki using 
;;    DuckDuckGo.
;; 20-Mar-2013    Christian  
;;    Last-Updated: mié mar 20 02:19:36 2013 (-0300) #34 (Christian)
;;    The first revision.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'ddg)
(require 'ddg-mode)

(defconst ddg-result-buffer-name "*DuckDuckGo!*"
  "This is the DDG. buffer name.")

(defun duckduckgo (term)
  "Search using DuckDuckGo."
  (interactive "MSearch? ")

  (with-current-buffer (get-buffer-create ddg-result-buffer-name)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))    
      (ddg-mode)
      (insert "Query: " term "\n")))  
  (ddg-search-asyn term 'ddg-show-results))

(defun duckduckgo-web (term)
  "Search in the web using `browse-url'."
  (interactive "MSearch on the web?")
  (browse-url (concat "https://duckduckgo.com/?q="
		      (url-hexify-string term)))
  )

(defun duckduckgo-emacswiki (term)
  "Search inside emacswiki site using `browse-url'."
  (interactive "MSearch inside EmacsWiki.org? ")
  (duckduckgo-web (concat "site:emacswiki.org " term)))

(defun ddg-show-results (list)
  "Show all the result from the LIST."
  (switch-to-buffer-other-window ddg-result-buffer-name)
  (with-current-buffer ddg-result-buffer-name
    (let ((inhibit-read-only t)
	  )
      (goto-char (point-max))
      (insert
       (let ((cat (cdr (assoc 'Type list))))
	 (cond 
	  ((equal cat "A")
	   "Article")
	  ((equal cat "D")
	   "Disambiguation")
	  ((equal cat "C")
	   "Category")
	  ((equal cat "N")
	   "Name")
	  ((equal cat "E")
	   "Exclusive")
	  (t
	   "Nothing")	
	  )       
	 )


       "\n\nAnswer:  " (cdr (assoc 'Answer list))     

       ;; Answer
       "\n\nImage:   " (cdr (assoc 'Image list))
       "\nType:      " (cdr (assoc 'AnswerType list))
       
       ;; Abstract
       (if (equal (cdr (assoc 'Abstract list)) "")
	   ""
	 (concat
	  "\n\n** Abstract:"
	  "\nText:   " (cdr (assoc 'AbstractText list))
	  "\nSource: " (cdr (assoc 'AbstractSource list))
	  "\nURL:    " (cdr (assoc 'AbstractURL list))
	  )
	 )
       ;; Definition
       (if (equal (cdr (assoc 'Definition list)) "")
	   ""
	 (concat 
	  "\n\n** Definition:"
	  "\nDef.:   " (cdr (assoc 'Definition list))
	  "\nSource: " (cdr (assoc 'DefinitionSource list))
	  "\nURL:    " (cdr (assoc 'DefinitionURL list))
	  )	
	 )
       ;; Results
       "\n\n** Results:\n"
       (ddg-format-results (cdr (assoc 'Results list)))
       )
      )
    )
  )

(defun ddg-format-results (ddg-results)
  "Parse all the results from the Result field in DDG-RESULTS. 
DDG-RESULTS is an array of the Results that comes from a parsed JSON answer."
  (let* ((str-out "")
	 (i 0)
	 (arr_size (length ddg-results))
	 )
    (while (< i arr_size)
      (let ((res (aref ddg-results i))
	    )
	(setq str-out (concat str-out 
			      "Result: "
			      (ddg-format-one-result res)
			      "\n"))
	)
      (setq i (+ i 1))
      )

    str-out
    )
  )

(defun ddg-format-one-result (res)
  "Format only one result element. RES is only one JSON parsed result."
  (let ((text (cdr (assoc 'Text res)))
	(first-url (cdr (assoc 'FirstURL res)))
	(result (cdr (assoc 'Result res))))
    (format "%s --> %s" text first-url)))


(provide 'ddg-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ddg-search.el ends here
