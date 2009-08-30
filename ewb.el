;;; ewb.el --- Emacs Web Browser

;; Copyright (C) 2002  Personal Sovereignty Foundation

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Author: Patrick Anderson (concat (nreverse (string-to-list "gro.scimococe@tunwo")))

;; Keywords: browser, web, net, XHTML/HTTP/TCP

;;; Installation:
;;   Add this to your .emacs
;; (autoload 'ewb "ewb" "emacs web browser" t)

;;; Commentary:
;; For entertainment purposes only

;;; Usage:
;; M-x ewb RET
;; C-h m for keystroke help

;;; Todo:
;; use http-get.el, http-post.el, htmlr.el
;; <script>Can we use font-lock and narrow these code regions?</script>
;; Text and image data is incrementally (stealthily) downloaded.
;; Download queue is also background managed.


;;; Version:
;;   .001 -- Brewing a new mix...
;;   Researching htmlr.el, http-get.el, xml.el, nxml, asynch.el

;;   0 -- end of life
;;   ewb has overgrown design limitations.
;;   The replacement for ewb will parse XHTML directly.

;;   -.05
;;   fixed font-lock errors
;;   local files should work (use `ewb-local')

;;   -.09 --
;;   Much more responsive because i'm using the idea from
;;   http://emacswiki.org/cgi-bin/wiki.pl?PrettyLambda to piggy-back
;;   onto font-lock.
;;   Can now view local files, but cannot follow local links
;;   killing stale ewb-connection when entering ewb-get.

;;   -.1a -- Fixed dangerous sentinel code (when ewb-connection finally exits, the formatting was being applied to whichever buffer had focus.)
;;   -.1 --- Sending a more complete request - sourceforge.net works, but replies with gzipped content.  patware.freeshell.org works but ewb-connection hangs.  now using HTTP/1.1
;;   -.2a -- Removed referer string.
;;   -.2 --- Sending referer. Changed version numbering. ewb-href now accepts <a name=>. need to move to state-machine parser.
;;   -.3 --- Prefix arg or file extensions suppress formatting. write bookmarks to file, customizeable
;;   -.4 --- added ewb-revert [(g)], minibuffer state messages, display ewb-url above header, mouse-face, help-echo
;;   -.5 --- first alpha.





;;; Customizations:
(defgroup ewb nil "Emacs Web Browser"
  :group 'hypermedia)

(defcustom ewb-suppressed-extensions 
  "\.el$\\|\.txt$"
  "Don't parse these extensions."
  :group 'ewb)

(defcustom ewb-bookmark-file
  "~/.ewb/bookmarks"
  "Bookmark file"
  :group 'ewb)

(defcustom ewb-history-file
  "~/.ewb/history"
  "History file"
  :group 'ewb)

(defcustom ewb-invisible-markup
  t
  "Make markup invisible?"
  :type 'boolean
  :group 'ewb)

(defcustom ewb-markup-face
  'file-name-shadow
  "Face for markup if `ewb-invisible-markup' is not set."
  :type 'face
  :group 'ewb)

(defface ewb-note-face
  '((t (:height 0.5)))
  "Face for markup if `ewb-invisible-markup' is not set."
  :group 'ewb)

;; Code:
(defun ewb (url &optional prefix)
  "Emacs Web Browser.
\\{ewb-mode-map}"
  (interactive "sURL: ")
  (string-match "\\(http://\\)?\\(.*\\)" url)
  (setq url (concat "http://" (match-string 2 url)))
  (ewb-parse-url url))

(defun ewb-local (file)
  "Start `ewb' on a local file."
  (interactive "fFile: ")
  (let ((file (file-relative-name file "/")))
	(cd (concat "/" (file-name-directory file)))
	(ewb-parse-url (concat "file://" "/" file))))

(defvar ewb-mode-map (make-sparse-keymap))

(define-key ewb-mode-map [(n)] 'pan-up)
(define-key ewb-mode-map [(down)] 'pan-up)
(define-key ewb-mode-map [(p)] 'pan-down)
(define-key ewb-mode-map [(up)] 'pan-down)

(define-key ewb-mode-map [(C)]
  (lambda (dir)
	(interactive "DDownload to: ")
	(cd dir)
	(start-process "wget" "*wget*" "wget" "-nv"
				   (message (progn
							  (re-search-backward ewb-href)
							  (match-string 1))))))

(define-key ewb-mode-map [(b)] 'ewb-bookmarks)
(defun ewb-bookmarks ()
  (interactive)
  (find-file ewb-bookmark-file))
(defun ewb-bookmark-add ()
  (interactive)
  (message (concat ewb-url "\nAdded to " ewb-bookmark-file))
  (write-region (concat ewb-url "\n") nil ewb-bookmark-file t))
(define-key ewb-mode-map [(B)] 'ewb-bookmark-add)

(define-key ewb-mode-map [(o)] 'ewb)
(define-key ewb-mode-map [(g)] 'ewb-revert)
(defun ewb-revert ()
  (interactive)
  (ewb ewb-url)
  (setq ewb-history (cdr ewb-history)))

(define-key ewb-mode-map [(9)] 'ewb-next)
(defun ewb-next ()
  "jump to next link"
  (interactive)
  (re-search-forward ewb-href)
  (message (match-string 1)))

(define-key ewb-mode-map [(shift 9)] 'ewb-prev)
(defun ewb-prev ()
  "jump to prev link"
  (interactive)
  (re-search-backward ewb-href)
  (message (match-string 1)))

(define-key ewb-mode-map [(13)] 'ewb-follow)
(defun ewb-follow ()
  "follow this link"
  (interactive)
  (re-search-backward ewb-href)
  (ewb-parse-url (replace-regexp-in-string "%20" " " (match-string 1))))

(define-key ewb-mode-map [(mouse-2)] 'ewb-follow-mouse)
(defun ewb-follow-mouse (event &optional univ)
  (interactive "e\nP")
  (goto-char (posn-point (event-end event)))
  (ewb-follow))

(define-key ewb-mode-map [(s)] 'ewb-search)
(defun ewb-search (str)
  "use google"
  (interactive "sFor: ")
  (ewb (concat "www.google.com/search?q=" (dired-replace-in-string " " "+" str))))

(define-key ewb-mode-map [(q)] 'bury-buffer)
(define-key ewb-mode-map " " 'scroll-up)
(define-key ewb-mode-map [(u)] 'scroll-down)
(define-key ewb-mode-map [(h)] (lambda () (interactive) (mapcar 'print ewb-history)))

(define-key ewb-mode-map [(meta left)] 'ewb-back)
(define-key ewb-mode-map [(backspace)] 'ewb-back)
(defun ewb-back ()
  "go back in history"
  (interactive)
  (if ewb-history
	  (progn
		(setq ewb-history (cdr ewb-history))
		(let ((cur (car ewb-history)))
		  (setq ewb-history (cdr ewb-history)) ;remove self, since ewb-get adds
		  (ewb cur)))))

;;(defvar ewb-href "<a[^>]*?\\(?:href\\|name\\)=\"?\\([^\">]+\\)[^>]*?>")
;;(defvar ewb-href "<a\\(.\\|\n\\)*?\\(?:href\\|name\\)=\"?\\([^\">]+\\)\\(.\\|\n\\)*?>")
(defvar ewb-href "<a\\(.\\|\n\\)*?\\(href\\|name\\)=\"?\\([^\">]+\\)[^>]*>")
(defvar ewb-history nil)
(defvar ewb-host nil)
(defvar ewb-proto nil)
(defvar ewb-dir nil)
(defvar ewb-url nil)
(defvar ewb-connection nil)

(defun ewb-mode ()
  "emacs web browser mode. \\{ewb-mode-map}"
  (interactive)
  (kill-all-local-variables)
										;  (setq buffer-read-only t)
  (use-local-map ewb-mode-map)
  (setq mode-name "ewb")
  (setq major-mode 'ewb-mode)
  (add-to-invisibility-spec 'ewb)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ewb-font-lock-keywords t t nil nil)) ; nil nil ((?_ . "w")))))
  (run-hooks 'ewb-mode-hook))

(defconst ewb-font-lock-keywords
  (eval-when-compile
    (list
  	 `("\\(<br/?>\\|<p/?>\\|<hr/?>\\|</h[1-9]>\\|</li>\\|</title>\\)[^\n]"
   	   (1
		(progn
		  (backward-char)
		  (insert "\n")
		  ewb-markup-font)))

 	 `("<em>\\(.\\|\n\\)*?</em>" (0 (progn 'ewb-note-face)))

 	 `("<i>\\(.\\|\n\\)*?</i>\\|<cite>\\(.\\|\n\\)*?</cite>" (0 'italic))

 	 `("<b>\\(.\\|\n\\)*?</b>"
  	   (0
		(progn
		  'bold)))

;;  	 `("<i>.*?</i>" (0 'italic))
;;  	 `("<i>.*?</i>" (0 'italic))
;;  	 `("<i>.*?</i>" (0 'italic))
;;  	 `("<i>.*?</i>" (0 'italic))

 	 `("<\\(.\\|\n\\)*?>\\|&....;"
  	   (0 (progn
			(if ewb-invisible-markup
				(let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
				  (overlay-put overlay 'invisible 'ewb)
				  (overlay-put overlay 'intangible 'ewb)))
			ewb-markup-face)))

  	 `("<img .*?src=\"\\([^\"]*?\\)\"[^>]*>[^Z]"
   	   (1
		(progn
;;		  (edebug-br)
		  (backward-char)
		  (insert "Z")
		  (if (= 0 (length ewb-host)) ;;if we're browsing locally
			  (insert-image (create-image (match-string 1))))
;; 		  (if ewb-invisible-markup
;; 			  (insert (match-string 1)))
		  ewb-markup-font)))

 	 `(,(concat ewb-href "\\(\\(.\\|\n\\)*?\\)</a>")
  	   (4 (progn
  			(let (( overlay (make-overlay (match-beginning 4) (match-end 4))))
  			  (overlay-put overlay 'mouse-face 'highlight)
  			  (overlay-put overlay 'help-echo (match-string 1)))
			font-lock-keyword-face)))

 	 `("&nbsp;[^ ]"
  	   (0
		(progn
		  (backward-char)
		  (insert " ")
		  font-lock-string-face)))

 	 `("&quot;[^\"]"
  	   (0
		(progn
		  (backward-char)
		  (insert "\"")
		  font-lock-string-face)))

	 (cons (regexp-opt
			'("HTTP/1.1" "Date:" "Server:" "Keep-Alive:" "Connection:" "Transfer-Encoding:" "Content-Type:")
			'words)
		   'font-lock-builtin-face)
	 )))

(defun ewb-parse-url (url)
  "An optional PROTO looks like http: or file:
An optional HOST looks like //(a.b.c)?
An optional DIR looks like /?a.b/c.d/
An optional FILE looks like a.b"
  ;;             1=proto:?,          2=//host?       3=/dir/*               5=file?
  (string-match "\\(http:\\|file:\\)?\\(//[^/\n]*\\)?\\(\\([^/\n]*/\\)*\\)\\(.*\\)" url)

  (let* ((proto (match-string 1 url))
		 (host (match-string 2 url))
		 (dir (match-string 3 url))
		 (file (match-string 5 url))
		 (buf (get-buffer-create "*Emacs Web Browser*")))

	(if (> (length proto) 0)
		(setq ewb-proto proto))

	(if (> (length host) 0)
		(progn
		  (string-match "\\(//\\)?\\(.*\\)" host)
		  (setq host (match-string 2 host)) ;;strip leading // if present
		  (setq ewb-host host))) ;;this will be nil for file:

	(if (> (length dir) 0) ;;if dir is full,
		(setq ewb-dir dir) ;;update cwd
	  (if (= 0 (length file)) ;;else, if file is empty
		  (setq ewb-dir "/"))) ;;make dir root
;; 	(if dir
;; 		(if root
;; 			(setq ewb-dir (concat "/" dir))
;; 		  (progn 
;; ;;			(setq dir (file-relative-name dir ewb-dir))
;; 			(setq ewb-dir (concat ewb-dir dir))))
;; 	  (if (not file)
;; 		  (setq ewb-dir "/" )))

;; 	(cd ewb-dir)
;; 	(setq ewb-dir (file-relative-name ewb-dir "/"))

	(setq ewb-url (concat ewb-proto "//" ewb-host ewb-dir file))
	(if (file-exists-p ewb-history-file)
	  (write-region (concat ewb-url "\n") nil ewb-history-file t))
	(setq ewb-history (cons ewb-url ewb-history))
	(switch-to-buffer buf)
	(erase-buffer)
	(insert (concat ewb-url "\n"))
	(overlay-put (make-overlay 1 (+ (length ewb-url) 1)) 'face 'font-lock-constant-face)

	(if (> (length ewb-host) 0)
		(ewb-get ewb-host ewb-dir file buf)
	  (progn
;;		(cd ewb-dir)
		(insert-file-contents (concat ewb-dir file))))

	(ewb-mode)))

(defun ewb-get (host dir file buf)
  "get over http"
  (message (concat "downloading " ewb-url "..."))

  (if ewb-connection
	  (progn
		(delete-process ewb-connection)
		(setq ewb-connection nil)))

  (setq ewb-connection (open-network-stream "ewb-connection" buf host 80))
  (process-send-string
   ewb-connection
   (concat
	"GET "
	(replace-regexp-in-string " " "%20" (concat dir file))
	" HTTP/1.1\r\n"
	"Host: " host "\r\n"
	"User-Agent: Emacs Web Browser\r\n"
	"Accept: */*\r\n"
	"Accept-Language: *\r\n"
	"Accept-Encoding: gzip, deflate, compress\r\n"
	"Accept-Charset: ISO-8859-1, utf-8\r\n"
	"Keep-Alive: 300\r\n"
	"Connection: keep-alive\r\n"
	"\r\n")))


(provide 'ewb)
;;; ewb.el ends here
