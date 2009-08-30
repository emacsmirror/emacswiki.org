;;; autoinfo.el --- show automatic information for the current selection

;; Copyright (C) 2008  

;; Author:  Tamas Patrovics
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This minor-mode monitors if something is selected in the buffer
;; where it is turned on and if the user is idle it fetches
;; information about the selection in the background and shows it in a
;; tooltip.
;; 
;; I added an example function which looks up using google the
;; definition of the word you select. It's not polished, since it's
;; only a quickly implemented example.
;; 

;; Usage:
 
;; Turn on autoinfo-mode, select something either with the keyboard or
;; with the mouse and wait for the results.

;; Tested on Emacs 22.

;;; Code:

(defvar autoinfo-min-query-length 3
  "The minimum length of query for which information is fetched.")

(defvar autoinfo-max-query-length 20
  "The maximum length of query for which information is fetched.")

(defvar autoinfo-idle-period 1
  "Idle period in seconds after which information is fetched for the selection.")

(defvar autoinfo-fetch-info-function 'autoinfo-fetch-google-definition
  "Function called with one argument (the query) when information is requested.
It must return a value which can be handled by `autoinfo-show-result-function'.")

(defvar autoinfo-show-result-function 'autoinfo-show-result-in-tooltip
  "Function called with one argument (the query) when information is requested.")

;;----------------------------------------------------------------------

(defvar autoinfo-timer nil
  "Timer monitoring the selection.")

(defvar autoinfo-last-query ""
  "The last query used.")


(define-minor-mode autoinfo-mode
  "Show automatic information for the current selection."
  nil 
  " AutoInfo"
  nil
  (autoinfo-toggle))

  
(defun autoinfo-toggle ()
  "Setup hooks according to variable `auto-info'."
  (if autoinfo-mode
      (setq autoinfo-timer (run-with-idle-timer autoinfo-idle-period t 
                                                'autoinfo-check-selection))
    (cancel-timer autoinfo-timer)
    (setq autoinfo-timer nil)))


(defun autoinfo-check-selection ()
  "Fetch info for the selection if any."
  (when (and autoinfo-mode
             (mark t))
    (let ((region-length (- (region-end) (region-beginning))))
      (if (and (>= region-length autoinfo-min-query-length)
               (<= region-length autoinfo-max-query-length))
          (let ((query (buffer-substring (region-beginning) (region-end))))
            (unless (equal query autoinfo-last-query)
              (setq autoinfo-last-query query)
              (funcall autoinfo-fetch-info-function query)))))))


(defun autoinfo-fetch-google-definition (query)
  "Fetch definition from Google for QUERY."
  (message "Fetching definition from Google...")
  (condition-case err
      (url-retrieve (concat "http://www.google.com/search?q=define:" (url-hexify-string query))
                    'autoinfo-handle-google-response)
    (error
     (message "Error when getting info: %s" (error-message-string err)))))
  

(defun autoinfo-handle-google-response (status)
  "Handle response returned by Google."
  (message "")
  (let ((response (buffer-string)))
    (funcall autoinfo-show-result-function
             (if (string-match "Definitions of.*?\\(<li>.*?\\)<br>" response)
                 (let ((results (match-string 1 response)))
                   (concat "Definitions by Google:\n"
                           (replace-regexp-in-string "<li>" "\n- " results)))

               "No definition found"))))
       

(defun autoinfo-show-result-in-tooltip (result)
  "Show the retrieved information in a tooltip."
  (tooltip-show result))
  

(provide 'autoinfo)
;;; autoinfo.el ends here
