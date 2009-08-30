;;; tjic-website.el
;;
;; Copyright (C) 2002-2003 Travis J.I. Corcoran
;;
;; This is free software
;;
;; Author: Travis J.I. Corcoran <tjic_emacs@tjic.com>
;; Version: $Header: /var/cvsroot/tjiclisp/tjic-website.el
;; Keywords: website, tjic, html, php, blog

;;; Commentary:

;; 0. TABLE OF CONTENTS
;; --------------------
;; 0. table of contents
;; 1. introduction
;; 2. setup
;; 3. use
;; 4. bugs
;;
;;
;; 1. INTRODUCTION
;; ---------------
;; This package provides utilities for maintaining a remote website.
;;
;; The most up to date version of this package can be found at:
;;         http://www.tjic.com/computers/tjic-website.el
;;
;;
;; 2. SETUP
;; --------
;;
;; (1) save this file to some place in your emacs load-path 
;;      (if you don't know what this means, type 
;;         M-x describe-variable [ RETURN ] load-path [ RETURN ]
;;      )
;;
;; (2) download and save files tjicutil-funcs.el 
;;       in the same directory.
;;
;; (3) in your .emacs put 
;;      (require 'tjic-website "tjic-website.el")
;;
;; (4) in your .emacs put the following, replacing the ... with
;;     appropriate customizations.  Read the defvar statements below
;;     to figure out what each variable does and what plausible values
;;     might be.
;;
;;     (defvar  tjic-website-local-machine-remote-map ... )
;;     (defvar  tjic-website-filecopy-function ... )
;;
;; 3. USE
;; ------
;;
;; (1) When visiting a file that you want copied to your remote
;; webserver
;;
;;
;; 4. BUGS
;; -------
;;


;;----------
;; Dependencies:
;;

(require 'tjicutil-funcs "tjicutil-funcs.el")


;;----------
;; Anti-Dependencies:
;;

(provide 'tjic-website)

;; variables
;;
;;

;; user-settable
;;

(defvar tjic-website-local-machine-remote-map nil
  "Specifies a mapping from local location of files to machine and
remote location.  Used by tjic-copy-website-to-server.  When a file is
copied by tjic-copy-website-to-server, the function looks in this
list, finds the matching entry, and then copies the file to the
specified remote machine, to the specified remote dir.

Format: an alist with a a local path prefix as the key, and a 3 element
list (machine name | user name | remote path prefix) on the right.

Example: 

  '(\"/home/foobar/www\" . (\"foobar.com\" \"tjic\"  \"/home/foobar/www/docs\"))
")

(defvar tjic-website-scp-sleep 5
  "Specifies how long to sleep before giving pwd")

(defvar tjic-website-filecopy-function 'tjic-website-ftp-file
  "The function that should be used to copy a file from local
machine to remote machine.  Two reasonable options are
 'tjic-website-ftp-file and 'tjic-website-scp-file")


(setq  ange-ftp-ftp-program-args (cons "-p"  ange-ftp-ftp-program-args))

;; internal
;;
(defvar tjic-copy-proc-buf " tjic-copy-website-to-server"
  "An internal variable specifying buffer for scp output")

(defvar tjic-website-scp-pwd nil
  "An internal variable specifying scp pwd for webserver")


(defun tjic-website-copyfile-to-server (update-whats-new-raw)
  "copy current webfile to server
   If prefix specified, update the what's new page.
   BUGS: should yell if error."
  (interactive "P" )

  ;; find remote machine and remote path
  ;;

  (let* ((local-to-remote-map (tjic-assoc-both
							   (file-name-directory (buffer-file-name))
							   tjic-website-local-machine-remote-map
							   'tjic-str2-is-prefix-of-str1) )
		 (local-root (progn (if (not local-to-remote-map) (error "ERROR - tjic-website.el -  file not in a website"))
							(car local-to-remote-map)))
		 (local-wrt-root (tjic-str2-in-excess-of-str1 local-root (buffer-file-name)))

		 
		 (remote-full (cdr local-to-remote-map))
		 (remote-host (nth 0 remote-full))
		 (remote-user (nth 1 remote-full))
		 (remote-root (nth 2 remote-full)))
							   
	;;
	;; verify correctness
	;;
	(if (or (not local-root)
			(not remote-host)
			(not remote-root))
		(error "ERROR - tjic-website.el -  current buffer not visiting local website"))

	;; copy to remote host
	;;
	(let* ((remote-path-and-file (concat remote-root local-wrt-root))
		   (remote-path (car (tjic-strings-tokenize-from-end remote-path-and-file "/")))
		   (remote-file (cdr (tjic-strings-tokenize-from-end remote-path-and-file "/"))))

	  (apply tjic-website-filecopy-function
			 (buffer-file-name)
			 remote-host 
			 remote-user
			 remote-path
			 remote-file)
	  )
	  
	;; update what's new page
	;;
	(if (not (eq update-whats-new-raw nil))
		(tjic-website-update-whatsnew-page (buffer-file-name)  
										   local-root
										   remote-host
										   remote-root))))

(defun tjic-website-update-whatsnew-page (newly-editted-file local-root remote-host remote-root)
  "Assumes that a file named 'whats_new.php' exists locally, at
LOCAL-ROOT.  Edits this file to have a timestamp and a relative href
link to the NEWLY-EDITTED-FILE.  Scps it to REMOTE-HOST and places it
at REMOTE-ROOT."

  ;; Find the name of this web page, as a human would refer to it.
  ;; (e.g. "Books", "Dogs", etc.)
  ;; If this page is an index page, then use the dir name.
  ;; Thus:
  ;;   foo/dogs.php  --> dogs
  ;;   foo/index.php --> foo
  (let* ((nam (file-name-nondirectory newly-editted-file))
		 (human-readable-pagename (if (tjic-str2-is-suffix-of-str1 nam ".php")
									  (tjic-strings-remove-suffix nam ".php")
									nam)))

	(if (string= human-readable-pagename "index")
		(progn
		  (setq human-readable-pagename (file-name-directory newly-editted-file))
		  (setq human-readable-pagename (substring human-readable-pagename 
												   0
												   (- (length human-readable-pagename) 1)))
		  (setq human-readable-pagename (tjic-strings-find-suffix-after-delim
										 human-readable-pagename "/"))))
			  
	;; Find relative-url-from-whatsnew, so that the whatsnew file can
	;; have an approp href.
	;; 
	(let ((relative-url-from-whatsnew
		   (concat "." (tjic-str2-in-excess-of-str1
						local-root
						newly-editted-file)))
		   (full-whatsnew-file (concat local-root "whats_new.php"))
		   (comment (read-from-minibuffer "whats-new comment: ")))

	  ;; actually edit the 'whats new' file
	  ;;
	  (set-buffer
	   (find-file-noselect full-whatsnew-file ))

	  (save-excursion
		(goto-char (point-min))
		(search-forward tjic-blog-whatsnew-editpt)
		(newline)
		(insert (concat "			<li>"
						(format-time-string "%d %b %Y" (current-time))
						": "
						comment
						" <a href=\""
						relative-url-from-whatsnew
						"\">"
						human-readable-pagename
						"</a>."))
		(save-buffer)

		;; scp the file over
		;;
		(apply tjic-website-filecopy-function
			   full-whatsnew-file
			   remote-host 
			   remote-root (list "/whats_new.php"))))))

; test:
;  (tjic-website-update-whatsnew-page "/home/tjic/personal/www/index.php" "/home/tjic/personal/www/" "tjic.com" "/home/tjic/www")

(defun tjic-website-ftp-file (local-file 
							  remote-host 
							  remote-user
							  remote-path
							  remote-file)
  "ftp LOCAL-FILE to REMOTE-HOST, at location REMOTE-PATH / REMOTE-FILE"

  (ange-ftp-copy-file local-file
					  (concat "/" remote-user "@" remote-host ":"
							  remote-path "/" ) t)
)

(defun tjic-website-scp-file (local-file 
							  remote-host
							  remote-user
							  remote-path
							  remote-file)
  "scp a file to REMOTE-HOST, at location REMOTE-PATH / REMOTE-FILE"
  ;; setup output buffer for 'scp'
  (save-excursion
	(set-buffer (get-buffer-create tjic-copy-proc-buf))
	(erase-buffer))


  (let ((outputbuffer tjic-copy-proc-buf)
		(proc nil)
		(process-connection-type t)
		(human-readable-pagename))


	(progn
	  (setq proc (start-process-shell-command
				  "scp-proc";; emacs process-object name
				  (get-buffer-create outputbuffer);; output buffer
				  "/usr/bin/scp";; execable
				  local-file
				  ;; remote file
				  (concat remote-host
						  ":"
						  (concat remote-path "/" remote-file))))
	  (if (not tjic-website-scp-pwd)
		  (setq tjic-website-scp-pwd (read-passwd "password for scp: ")))
	  ;; very important - if we don't sleep, scp isn't ready for the pwd.
	  (sleep-for tjic-website-scp-sleep)

	  (process-send-string proc (concat tjic-website-scp-pwd "\n")))))

(global-set-key "\C-xw"  'tjic-website-copyfile-to-server)

;;; tjic-website.el ends here
