;;; ddg.el --- DuckDuckGo API for ELisp. 
;; 
;; Filename: ddg.el
;; Description: DuckDuckGo API for ELisp. 
;; Author: Christian Giménez
;; Maintainer: 
;; Created: sáb mar  2 18:43:33 2013 (-0300)
;; Version: 
;; Last-Updated: sáb mar  2 19:46:50 2013 (-0300)
;;           By: Christian
;;     Update #: 29
;; URL: http://emacswiki.org/emacs/ddg.el
;; Doc URL: http://emacswiki.org/emacs/DuckDuckGo-el
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; DuckDuckGo API for consulting this fantastic search engine.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 2-Mar-2013    Christian  
;;    Last-Updated: sáb mar  2 19:45:59 2013 (-0300) #27 (Christian)
;;    Added URLs.
;; 2-Mar-2013    Christian  
;;    Last-Updated: sáb mar  2 19:44:46 2013 (-0300) #25 (Christian)
;;    My first revision. At so far it works.
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

(require 'json)
(require 'url)

(defgroup DuckDuckGo nil "DuckDuckGo"
  :group 'applications
  :version "23.0"
  :tag "DuckDuckGo"
  )

(defcustom ddg-duckduckgo-url "http://api.duckduckgo.com"
  "This is the URL where the query will be sended."
  :group 'DuckDuckGo
  :type 'string)

					; ********************
					; Main functions

(defun ddg-search (term)
  "Search for a term in the DuckDuckGo search engine.

Returns a list with parsed results."  
  
  (condition-case nil
      (json-read-from-string (ddg-send-search-query term))
    (error nil);; We don't want errors by processing bad strings... just return nil.
    )
  )

(defun ddg-search-asyn (term function)
  "Same as `ddg-search' but asynchronous.

FUNCTION will be called as soon as the search finishes must recieve one parameter a string containing the results."
  (ddg-send-search-query-asyn term function)
  )


					; ********************
					; Internal functions

(defun ddg-send-search-query-asyn (term function)
  "Send the search query and return the results.

This functions works *asynchronously*.

FUNCTION will be called with the result as parameter when the search is finished."
  (let ((url-request-method "GET")
	(url-request-data "")
	(url-request-extra-headers
	 '(("Accept-Language" . "en")
	   ("Accept-Charset" . "utf-8")))
	(get-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "q" term)
			  (cons "format" "json")
			  )			      
		    "&"))
	)
    (url-retrieve (concat ddg-duckduckgo-url "/?" get-data) 'ddg-url-handler (list function))
    )	  
  )

(defun ddg-url-handler (state function)
  "Handler function for `ddg-send-search-query-asyn'. 
It deletes the HTTP header and parse the JSON code. The call the function FUNCTION with the result as a parameter."
  (let ((results "")) ;; Make sure that the string is not deleted with the buffer.
    (ddg-delete-http-header)
    (condition-case nil
	(setq results (json-read))
      (error nil))
    (kill-buffer)
    (apply function (list results))
    )
  )

(defun ddg-send-search-query (term)
  "Send the search query and return the results.

This functions works *synchronously*."
  (let ((url-request-method "GET")
	(url-request-data "")
	(url-request-extra-headers
	 '(("Accept-Language" . "en")
	   ("Accept-Charset" . "utf-8")))
	(get-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "q" term)
			  (cons "format" "json")
			  )			      
		    "&"))
	)
    (with-current-buffer (url-retrieve-synchronously (concat ddg-duckduckgo-url "/?" get-data))
      (let ((results "")) ;; Make sure that the string is not deleted with the buffer.
	(ddg-delete-http-header)
	(setq results (buffer-string))
    	(kill-buffer)
    	results 
    	)
      )    
    )	   
  )

(defun ddg-delete-http-header ()
  "Delete the HTTP header in the current buffer."
  (goto-char (point-min))
  (let ((beg (point-min))
	(end (search-forward "\n\n" nil t)))	     
    (when end 
      (delete-region beg end)
      )
    )
  )

(provide 'ddg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ddg.el ends here
