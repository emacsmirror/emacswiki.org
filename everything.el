;;; everything.el --- Bridge to Windows desktop-search engine Everything

;; Copyright (C) 2012 Martin Weinig

;; Author: Martin Weinig
;; Keywords: tools,windows
;; Version: 0.1
;; Filename: everything.el
;; Compatibility: GNU Emacs 23.x

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with everything.el.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;;   Everything is a very fast and lightweight desktop search-engine
;;   for Windows for finding files scattered around your hard discs
;;   (see http://www.voidtools.com).
;;   The package everything.el provides a simple interface to access
;;   Everything's search power via its ftp interface within Emacs.
;;
;;   Everything.el can also integrate itself into find-file-at-point
;;   aka ffap. So, whenever all other methods of ffap fail, let everything
;;   try finding the file at point. Best used in combination with
;;   the exquicit Icicles package.
;;
;;   Features that are required: `cl'.
;;   Features that might be required: `ffap'.
;;
;;   Basic steps to setup:
;;
;;    (add-to-list 'load-path "~/path/to/everything.el")
;;    (setq everything-ffap-integration nil) ;; to disable ffap integration
;;    (require 'everything)
;;
;;
;;   Interesting variables:
;;
;;    As everything.el connects to Everything's ftp interface the following
;;    variables are self-explanatory:
;;
;;    `everything-host'             defaults to 127.0.0.1
;;    `everything-port'             defaults to 21
;;    `everything-user'             defaults to anonymous
;;    `everything-pass'             defaults to nil
;;    `everything-query-limit'      defaults to #xffffffff for the maximum number of returned filenames
;;    `everything-ffap-integration' flag to integrate everything into ffap, defaults to t
;;
;;
;;   Interactive commands are:
;;
;;    M-x everything
;;
;;        Prompts for a query and let you choose
;;        one of the files returned by Everything and opens it.
;;
;;
;;  Todo:
;;  + make it possible to call everything-toggle-xyz via shortcuts while typing the query
;;  + integrate icicle-define-command to be able to use icicles action subsystem
;;

;;; Code:

;;;; User Options
(defgroup everything nil
  "Bridge to the Windows desktop search-engine everything. See http://www.voidtools.com."
  :group 'external
  :prefix "everything-")

(defcustom everything-host "127.0.0.1" "Ftp server address of Everything. Defaults to 127.0.0.1."
  :group 'everything
  :type 'string)

(defcustom everything-port 21 "Everythings ftp server port. Defaults to 21."
  :group 'everything
  :type 'integer)

(defcustom everything-user "anonymous" "Login name."
  :group 'everything
  :type 'string)

(defcustom everything-pass nil "Password."
  :group 'everything
  :type 'string)

(defcustom everything-query-limit #xffffffff "Maximum number of filenames returned."
  :group 'everything
  :type 'integer)

(defcustom everything-ffap-integration t "Integrate everything into ffap."
  :group 'everything
  :type 'boolean)


;;; System Variables

(defvar everything-matchcase nil "Tells everything to use case sensitve searching.")
(defvar everything-matchwholeword nil "Tells everything to match only whole words.")
(defvar everything-matchpath nil "Tells everything to include matching the file's path.")
(defvar everything-wait 100 "Time in milliseconds to wait for responses.")
(defvar everything-post-process-hook nil "Functions that post-process the buffer `everything-result-buffer'
after the server finished sending the query result.
Usefull i.e. for replacing all pathnames with their subst-counterparts. A hook can assume `everything-default-buffer' being
the current buffer. A hook can not assume that point is at any specific location.")
(defvar everything-response-counter 0)
(defvar everything-response-handler nil "Function to handle server responses.")
(defvar everything-response-finished nil "Indicates if a response is finished, so emacs can stop waiting for responses.
Must be set by `everything-response-handler'.")
(defvar everything-response-status 0 "The status code returned by the server.")
(defvar everything-response-numlines 0 "The number of files/paths returned from the server.
This is the sum of `everything-response-numfolders' and `everything-response-numfiles'")
(defvar everything-response-numfolders 0 "The number of folders returned.")
(defvar everything-response-numfiles 0 "The number of files returned.")
(defvar everything-result-buffer "*everything*" "Name of buffer to write the query response to.
After successfully fetching the matching filenames, this buffer holds one filename per line.
See `everything-post-process-hook' to post-process this buffer.")
(defvar everything-log-buffer "*everything-log*" "Name of buffer to write log messages to.")
(defvar everything-log nil "Switch to enable log buffer.")


(require 'cl)  ; find

;;;; User Interface

;;;###autoload
(defalias 'everything 'everything-find-file)

;;;###autoload
(defun everything-find-file ()
  "Prompt for a search string, let the user choose one of the returned files and
open it."
  (interactive)
  (find-file (everything-find-prompt)))


(defun everything-toggle-case ()
  (interactive)
  (setq everything-matchcase (not everything-matchcase)))

(defun everything-toggle-wholeword ()
  (interactive)
  (setq everything-matchwholeword (not everything-matchwholeword)))

(defun everything-toggle-path ()
  (interactive)
  (setq everything-matchpath (not everything-matchpath)))


;;; Internal Code

(defun everything-set-passwd ()
  (setq everything-pass (read-passwd "Password: " nil)))

(defun everything-find-prompt ()
  "Prompt for a query and return the chosen filename."
  (let ((query (read-from-minibuffer "Query everything: ")))
    (unless (string= query "")
      (everything-select query))))

(defun everything-select (query)
  "Run the query query and return the chosen file.
If query is already an existing file, return it without running a query."
  (if (file-exists-p query)
      query
    (let ((files (everything-locate query)))
      (cond ((eq (length files) 0)
	     (progn (message "No matches.")
		    nil))
	    ((eq (length files) 1)
	     (car files))
	    (t
	     (completing-read (format "Select from query '%s' (%s matches): "
				   query
				   (length files))
			   files nil nil (when (eq (length files) 1)
					   (car files))))))))

(defun everything-locate (query)
  "Run a query via Everything and return a list of files, nil
otherwise."
  (everything-ftp-query query
			everything-host
			everything-port
			everything-user
			everything-query-limit
			everything-matchcase
			everything-matchwholeword
			everything-matchpath)
  (save-excursion
    (set-buffer (get-buffer-create everything-result-buffer))
    (goto-char (point-min))
    (while (search-forward "\\" nil t)  ;; we dont want crappy backslashes in out path names
      (replace-match "/" nil t))
    (run-hooks 'everything-post-process-hook)
    (split-string
     (buffer-substring-no-properties (point-min) (point-max)) "\n" t)))


(defun everything-is-running ()
  "Check if Everything is running."
  (find "Everything.exe"
	(mapcar (lambda (p) (cdr (assoc 'comm (process-attributes p))))
		(list-system-processes))
	:test 'string=))


;;; ftp related code
;; TODO: refactor my first attempts whith elisp networking

(defun everything-filter-fnc (proc response)
  "Filter function "
  (setq everything-response-counter (1+ everything-response-counter))
  (setq everything-response-status (string-to-number response))
  (if everything-response-handler
      (funcall everything-response-handler response everything-response-counter)))


(defun everything-ftp-send-request (proc request responsehandler)
  "Send the request request to the network process proc."
  (setq everything-response-handler responsehandler)
  (setq everything-response-finished nil)
  (setq everything-response-counter 0)
  (everything-log (concat request "\n"))
  (unless (equal (process-status proc) 'open)
    (error "Connection closed unexpectedly"))
  (process-send-string proc (concat request "\n"))
  (while (not everything-response-finished)
    (sleep-for 0 everything-wait))
  (cond ((= everything-response-status 331)
	 (let ((pwd (if everything-pass
			everything-pass
		      (everything-set-passwd))))
	   (everything-ftp-send-request proc (concat "PASS " pwd) 'generic-response-handler)))
	((= everything-response-status 530)
	 (error "Login or password incorrect. Try setting everything-user and everything-pass."))))

(defun files-response-handler (response responsecounter)
  (when (= responsecounter 1)
    (let* ((sep (search "\n" response))
	   (statusline (substring response 0 sep)))
      (everything-log (concat statusline "\n"))
      (everything-savenumfiles statusline)
      (setq response (substring response (1+ (search "\n" response))))))    ;; delete status line
  (let ((numlines (save-excursion
		    (set-buffer (get-buffer-create everything-result-buffer))
		    (insert response)
		    (count-lines (point-min) (point-max)))))
    (when (>= numlines everything-response-numlines)
    (setq everything-response-finished t))))


(defun generic-response-handler (response responsecounter)
  (everything-log response)
  (setq everything-response-finished t))

(defun everything-savenumfiles (statusline)
  (let ((lst (split-string statusline " ")))
    (setq everything-response-numlines (string-to-number (elt lst 2)))
    (setq everything-response-numfolders (string-to-number (elt lst 3)))
    (setq everything-response-numfiles (string-to-number (elt lst 4)))))

(defun everything-log (message)
  (when everything-log
    (save-excursion
      (set-buffer (get-buffer-create everything-log-buffer))
      (goto-char (point-max))
      (insert message))))

;; Everything's ftp query syntax
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; QUERY SYNTAX
;; QUERY <offset> <max> <match_case> <match_whole_word> <match_path> <search_string> <CRLF>
;; QUERY REPLY
;; 200 <offset> <count> <numfolders> <numfiles> <CRLF>
(defun everything-ftp-query (query host service user maxfiles matchcase matchwholeword matchpath)
  (unless (everything-is-running)
    (error "Everything is not running."))
  (setq everything-response-handler nil)
  (when (get-buffer everything-result-buffer)
    (kill-buffer everything-result-buffer))
  (let ((proc nil))
    (condition-case ex
	(setq proc (make-network-process :name "everythingftp"
					 :host host
					 :service service
					 :filter 'everything-filter-fnc))
      ('error (error (format "Failed to connect to Everything's ftp server at %s:%i. Is the server running?" host service))))
    (sleep-for 0 everything-wait)
    (unwind-protect
	(condition-case ex
	    (progn (everything-ftp-send-request proc (concat "USER " user) 'generic-response-handler)
		   (everything-ftp-send-request proc (format "QUERY 0 %i %i %i %i %s"
						 maxfiles
					    (if matchcase 1 0)
					    (if matchwholeword 1 0)
					    (if matchpath 1 0)
					    query)
			       'files-response-handler)
		   (everything-ftp-send-request proc "QUIT" 'generic-response-handler)
		   (delete-process proc))
	  ('error (progn
		    (when proc
		      (everything-ftp-send-request proc "QUIT" 'generic-response-handler) ; try to shutdown gracefully
		      (delete-process proc))
		    (error (cadr ex))))))))



;;;; ffap integration

(defun everything-ffap-guesser (file)
  "Called by ffap whenever all other methods of finding the file at point fail."
;; For everything to work we must clean the file at point, that means, removing
;; leading ../ and ..\ etc.
  (let* ((query (replace-regexp-in-string "\\.\\." "" file))  ;; remove ..
	 (query (replace-regexp-in-string "//" "" query))     ;; remove //
	 (query (replace-regexp-in-string "\\\\" "" query))   ;; remove \\
	 ;; (query (replace-regexp-in-string "/" "\\" query))    ;; replace / into \
	 (query (if (string-match "^[/\\]" query) (substring query 1) query))) ;; remove starting / or \
    (everything-select query)))

(when everything-ffap-integration
  (when (require 'ffap)
;; Integrate everything into ffap.
;; So, whenever all other methods in ffap-alist fail, let everything
;; try to find the file at point.
;; This is done by appending `everything-ffap-guesser' to the end of `ffap-alist'.
    (setf (cdr (last ffap-alist)) '(("\\.*\\'" . everything-ffap-guesser)))))


(provide 'everything)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; everything.el ends here
