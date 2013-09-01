;;; gnus-spotlight.el --- Search mail with Spotlight

;; Copyright (C) 2005
;; Yoshida Masato <yoshidam@yoshidam.net>
;; Copyright (C) 2000, 2001, 2002, 2003, 2004
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Yoshida Masato <yoshidam@yoshidam.net>
;; Keywords: mail searching spotlight

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This file defines the command to search mails and persistent
;; articles with Mac OS X Tiger's Spotlight and to browse its results with Gnus.
;;
;; This code is derived from gnus-namazu.el.

;;; Install:

;;      (require 'gnus-spotlight)
;;      (gnus-spotlight-insinuate)

;;; Code:

;;(eval-when-compile (require 'cl))
(require 'nnoo)
(require 'nnheader)
(require 'nnmail)
(require 'gnus-sum)

;; To suppress byte-compile warning.
(eval-when-compile
  (defvar nnml-directory)
  (defvar nnmh-directory))


(defgroup gnus-spotlight nil
  "Search nnmh and nnml groups in Gnus with Spotlight."
  :group 'spotlight
  :group 'gnus
  :prefix "gnus-spotlight-")

(defcustom gnus-spotlight-command
  "/usr/bin/mdfind"
  "*Name of the executable file of Spotlight."
  :type 'string
  :group 'gnus-spotlight)

(defcustom gnus-spotlight-additional-arguments nil
  "*Additional arguments of Spotlight."
  :type '(repeat string)
  :group 'gnus-spotlight)

(defcustom gnus-spotlight-field-keywords
  '("MDItemContentCreationDate" "MDItemAuthors" "MDItemAuthorEmailAddresses"
    "MDItemTitle" "MDItemRecipientEmailAddresses" "MDItemRecipients"
    "MDItemTextContent"
    )
  "*List of keywords to do field-search."
  :type '(repeat string)
  :group 'gnus-spotlight)

(defcustom gnus-spotlight-coding-system
  'utf-8
  "*Coding system for Spotlight process."
  :type 'coding-system
  :group 'gnus-spotlight)

(defcustom gnus-spotlight-case-sensitive-filesystem
  t
  "*Non-nil means that the using file system distinguishes cases of characters."
  :type 'boolean
  :group 'gnus-spotlight)

(defcustom gnus-spotlight-command-prefix nil
  "*Prefix command, 
if set '(\"ssh\" \"-x\" \"host\"),
then execute \"ssh -x host spotlight ...\""
  :type '(repeat string)
  :group 'gnus-spotlight)

(defcustom gnus-spotlight-mail-spool
  (expand-file-name "~/Mail/")
  "*Mail spool path."
  :type 'string
  :group 'gnus-spotlight)

;;; Internal Variable:
(defconst gnus-spotlight/group-name-regexp "\\`nnvirtual:spotlight-search\\?")

;; Multibyte group name:
(and
 (fboundp 'gnus-group-decoded-name)
 (let ((gnus-group-name-charset-group-alist
	(list (cons gnus-spotlight/group-name-regexp gnus-spotlight-coding-system)))
       (query (decode-coding-string (string 27 36 66 52 65 59 122 27 40 66)
				    'iso-2022-7bit)))
   (not (string-match query
		      (gnus-summary-buffer-name
		       (encode-coding-string
			(concat "nnvirtual:spotlight-search?query=" query)
			gnus-spotlight-coding-system)))))
 (let (current-load-list)
   (defadvice gnus-summary-buffer-name
     (before gnus-spotlight-summary-buffer-name activate compile)
     "Advised by `gnus-spotlight' to handle encoded group names."
     (ad-set-arg 0 (gnus-group-decoded-name (ad-get-arg 0))))))

(defmacro gnus-spotlight/make-article (group number)
  `(cons ,group ,number))
(defmacro gnus-spotlight/article-group  (x) `(car ,x))
(defmacro gnus-spotlight/article-number (x) `(cdr ,x))

(defsubst gnus-spotlight/indexed-servers ()
  "Choice appropriate servers from opened ones, and return thier list."
  (append
   (gnus-servers-using-backend 'nnml)
   (gnus-servers-using-backend 'nnmh)))

(defun gnus-spotlight/setup ()
  (and (boundp 'gnus-group-name-charset-group-alist)
       (not (member (cons gnus-spotlight/group-name-regexp
			  gnus-spotlight-coding-system)
		    gnus-group-name-charset-group-alist))
       (let ((pair (assoc gnus-spotlight/group-name-regexp
			  gnus-group-name-charset-group-alist)))
	 (if pair
	     (setcdr pair gnus-spotlight-coding-system)
	   (push (cons gnus-spotlight/group-name-regexp
		       gnus-spotlight-coding-system)
		 gnus-group-name-charset-group-alist))))
  )

(defun gnus-spotlight/server-directory (server)
  "Return the top directory of the server SERVER."
  (and (memq (car server) '(nnml nnmh))
       (nnoo-change-server (car server) (nth 1 server) (nthcdr 2 server))
       (file-name-as-directory
	(expand-file-name (if (eq 'nnml (car server))
			      nnml-directory
			    nnmh-directory)))))

(defsubst gnus-spotlight/call-spotlight (query)
  (let ((coding-system-for-read gnus-spotlight-coding-system)
	(coding-system-for-write gnus-spotlight-coding-system)
	(default-process-coding-system
	  (cons gnus-spotlight-coding-system gnus-spotlight-coding-system))
	program-coding-system-alist
	(file-name-coding-system gnus-spotlight-coding-system))
    (if gnus-spotlight-command-prefix
	(apply 'call-process
	       (append
		(list (car gnus-spotlight-command-prefix))
		'(nil t nil)
		(cdr gnus-spotlight-command-prefix)
		`(,gnus-spotlight-command
		  "-onlyin" ,gnus-spotlight-mail-spool
		  ,@gnus-spotlight-additional-arguments
		  ,query)))
      (apply 'call-process
	     `(,gnus-spotlight-command
	       nil			; input from /dev/null
	       t			; output
	       nil			; don't redisplay
               "-onlyin" ,gnus-spotlight-mail-spool
	       ,@gnus-spotlight-additional-arguments
	       ,query)))))

(defvar gnus-spotlight/directory-table nil)
(defun gnus-spotlight/make-directory-table (&optional force)
  (interactive (list t))
  (unless (and (not force)
	       gnus-spotlight/directory-table
	       (eq gnus-spotlight-case-sensitive-filesystem
		   (car gnus-spotlight/directory-table)))
    (let ((table (make-vector (length gnus-newsrc-hashtb) 0))
	  cache agent alist dir method)
      (mapatoms
       (lambda (group)
	 (unless (gnus-ephemeral-group-p (setq group (symbol-name group)))
	   (when (file-directory-p
		  (setq dir (file-name-as-directory
			     (gnus-cache-file-name group ""))))
	     (push (cons dir group) cache))
	   (when (memq (car (setq method (gnus-find-method-for-group group)))
		       '(nnml nnmh))
	     (when (file-directory-p
		    (setq dir (nnmail-group-pathname
			       (gnus-group-short-name group)
			       (gnus-spotlight/server-directory method))))
	       (push (cons dir group) alist)))))
       gnus-newsrc-hashtb)
      (dolist (pair (nconc agent cache alist))
	(set (intern (if gnus-spotlight-case-sensitive-filesystem
			 (car pair)
		       (downcase (car pair)))
		     table)
	     (cdr pair)))
      (setq gnus-spotlight/directory-table
	    (cons gnus-spotlight-case-sensitive-filesystem table)))))

(defun gnus-spotlight/search (groups query)
  (gnus-spotlight/make-directory-table)
  (with-temp-buffer
    (let ((exit-status (gnus-spotlight/call-spotlight query)))
      (unless (zerop exit-status)
	(error "Spotlight finished abnormally: %d" exit-status)))
    (goto-char (point-min))
    (let (articles group)
      (while (not (eobp))
	(setq group (buffer-substring-no-properties
		     (point)
		     (progn
		       (end-of-line)
		       ;; NOTE: Only numeric characters are permitted
		       ;; as file names of articles.
		       (skip-chars-backward "0-9")
		       (point))))
	(and
         (setq group
               (symbol-value
                (intern-soft (if gnus-spotlight-case-sensitive-filesystem
                                 group
                               (downcase group))
                             (cdr gnus-spotlight/directory-table))))
         (or (not groups)
             (member group groups))
         (push (gnus-spotlight/make-article
                group
                (string-to-number
                 (buffer-substring-no-properties (point)
                                                 (point-at-eol))))
               articles))
	(forward-line 1))
      (nreverse articles))))

;;; User Interface:
(defun gnus-spotlight/get-target-groups ()
  (cond
   ((eq major-mode 'gnus-group-mode)
    ;; In Group buffer.
    (cond
     (current-prefix-arg
      (gnus-group-process-prefix current-prefix-arg))
     (gnus-group-marked
      (prog1 gnus-group-marked (gnus-group-unmark-all-groups)))))
   ((eq major-mode 'gnus-summary-mode)
    ;; In Summary buffer.
    (if current-prefix-arg
	(list (gnus-read-group "Group: "))
      (if (and
	   (gnus-ephemeral-group-p gnus-newsgroup-name)
	   (string-match gnus-spotlight/group-name-regexp gnus-newsgroup-name))
	  (cadr (assq 'gnus-spotlight-target-groups
		      (gnus-info-method (gnus-get-info gnus-newsgroup-name))))
	(list gnus-newsgroup-name))))))

(defun gnus-spotlight/get-current-query ()
  (and (eq major-mode 'gnus-summary-mode)
       (gnus-ephemeral-group-p gnus-newsgroup-name)
       (string-match gnus-spotlight/group-name-regexp gnus-newsgroup-name)
       (cadr (assq 'gnus-spotlight-current-query
		   (gnus-info-method (gnus-get-info gnus-newsgroup-name))))))

(defvar gnus-spotlight/read-query-original-buffer nil)
(defvar gnus-spotlight/read-query-prompt nil)
(defvar gnus-spotlight/read-query-history nil)

(defun gnus-spotlight/get-current-subject ()
  (and gnus-spotlight/read-query-original-buffer
       (bufferp gnus-spotlight/read-query-original-buffer)
       (with-current-buffer gnus-spotlight/read-query-original-buffer
	 (when (eq major-mode 'gnus-summary-mode)
	   (let ((s (gnus-summary-article-subject)))
	     ;; Remove typically prefixes of mailing lists.
	     (when (string-match
		    "^\\(\\[[^]]*[0-9]+\\]\\|([^)]*[0-9]+)\\)\\s-*" s)
	       (setq s (substring s (match-end 0))))
	     (when (string-match
		    "^\\(Re\\(\\^?\\([0-9]+\\|\\[[0-9]+\\]\\)\\)?:\\s-*\\)+" s)
	       (setq s (substring s (match-end 0))))
	     (when (string-match "\\s-*(\\(re\\|was\\)\\b" s)
	       (setq s (substring s 0 (match-beginning 0))))
	     s)))))

(defun gnus-spotlight/get-current-from ()
  (and gnus-spotlight/read-query-original-buffer
       (bufferp gnus-spotlight/read-query-original-buffer)
       (with-current-buffer gnus-spotlight/read-query-original-buffer
	 (when (eq major-mode 'gnus-summary-mode)
	   (cadr (mail-extract-address-components
		  (mail-header-from
		   (gnus-summary-article-header))))))))

(defun gnus-spotlight/get-current-from-name ()
  (and gnus-spotlight/read-query-original-buffer
       (bufferp gnus-spotlight/read-query-original-buffer)
       (with-current-buffer gnus-spotlight/read-query-original-buffer
	 (when (eq major-mode 'gnus-summary-mode)
	   (car (mail-extract-address-components
                 (mail-header-from
                  (gnus-summary-article-header))))))))

(defun gnus-spotlight/get-current-to ()
  (and gnus-spotlight/read-query-original-buffer
       (bufferp gnus-spotlight/read-query-original-buffer)
       (with-current-buffer gnus-spotlight/read-query-original-buffer
	 (when (eq major-mode 'gnus-summary-mode)
	   (cadr (mail-extract-address-components
		  (cdr (assq 'To (mail-header-extra
				  (gnus-summary-article-header))))))))))

(defun gnus-spotlight/get-current-to-name ()
  (and gnus-spotlight/read-query-original-buffer
       (bufferp gnus-spotlight/read-query-original-buffer)
       (with-current-buffer gnus-spotlight/read-query-original-buffer
	 (when (eq major-mode 'gnus-summary-mode)
	   (car (mail-extract-address-components
                 (cdr (assq 'To (mail-header-extra
                                 (gnus-summary-article-header))))))))))

(defmacro gnus-spotlight/minibuffer-prompt-end ()
  (if (fboundp 'minibuffer-prompt-end)
      '(minibuffer-prompt-end)
    '(point-min)))

(defun gnus-spotlight/message (string &rest arguments)
  (let* ((s1 (concat
	      gnus-spotlight/read-query-prompt
	      (buffer-substring (gnus-spotlight/minibuffer-prompt-end)
				(point-max))))
	 (s2 (apply (function format) string arguments))
	 (w (- (window-width)
	       (string-width s1)
	       (string-width s2)
	       1)))
    (message (if (>= w 0)
		 (concat s1 (make-string w ?\ ) s2)
	       s2))
    (if (sit-for 0.3) (message s1))
    s2))

(defun gnus-spotlight/complete-query ()
  (interactive)
  (let ((pos (point)))
    (cond
     ((and (re-search-backward "k\\([A-Za-z]*\\)" nil t)
	   (= pos (match-end 0)))
      (let* ((partial (match-string 1))
	     (completions
	      (all-completions
	       partial
	       (mapcar 'list gnus-spotlight-field-keywords))))
	(cond
	 ((null completions)
	  (gnus-spotlight/message "No completions of %s" partial))
	 ((= 1 (length completions))
	  (goto-char (match-beginning 1))
	  (delete-region (match-beginning 1) (match-end 1))
	  (insert (car completions) "==")
	  (setq pos (point))
	  (gnus-spotlight/message "Completed"))
	 (t
	  (let ((x (try-completion partial (mapcar 'list completions))))
	    (if (string= x partial)
		(if (and (eq last-command
			     'gnus-spotlight/field-keyword-completion)
			 completion-auto-help)
		    (with-output-to-temp-buffer "*Completions*"
		      (display-completion-list completions))
		  (gnus-spotlight/message "Sole completion"))
	      (goto-char (match-beginning 1))
	      (delete-region (match-beginning 1) (match-end 1))
	      (insert x)
	      (setq pos (point))))))))
     ((and (looking-at "kMDItemTitle==")
	   (= pos (match-end 0)))
      (let ((s (gnus-spotlight/get-current-subject)))
	(when s
	  (goto-char pos)
	  (insert "\"" s "\"")
	  (setq pos (point)))))
     ((and (looking-at "kMDItemAuthorEmailAddresses==")
	   (= pos (match-end 0)))
      (let ((f (gnus-spotlight/get-current-from)))
	(when f
	  (goto-char pos)
	  (insert "\"" f "\"")
	  (setq pos (point)))))
     ((and (looking-at "kMDItemAuthors==")
	   (= pos (match-end 0)))
      (let ((f (gnus-spotlight/get-current-from-name)))
	(when f
	  (goto-char pos)
	  (insert "\"" f "\"")
	  (setq pos (point)))))
     ((and (looking-at "kMDItemRecipientEmailAddresses==")
	   (= pos (match-end 0)))
      (let ((to (gnus-spotlight/get-current-to)))
	(when to
	  (goto-char pos)
	  (insert "\"" to "\"")
	  (setq pos (point)))))
     ((and (looking-at "kMDItemRecipients==")
	   (= pos (match-end 0)))
      (let ((to (gnus-spotlight/get-current-to-name)))
	(when to
	  (goto-char pos)
	  (insert "\"" to "\"")
	  (setq pos (point))))))
    (goto-char pos)))

(defvar gnus-spotlight/read-query-map
  (let ((keymap (copy-keymap minibuffer-local-map)))
    (define-key keymap "\t" 'gnus-spotlight/complete-query)
    keymap))

(defun gnus-spotlight/read-query (prompt &optional initial)
  (let ((gnus-spotlight/read-query-original-buffer (current-buffer))
	(gnus-spotlight/read-query-prompt prompt))
    (unless initial
      (when (setq initial (gnus-spotlight/get-current-query))
	(setq initial (cons initial 0))))
    (read-from-minibuffer prompt initial gnus-spotlight/read-query-map nil
			  'gnus-spotlight/read-query-history)))


(defun gnus-spotlight/truncate-article-list (articles)
  (let ((hit (length articles)))
    (when (and gnus-large-newsgroup
	       (> hit gnus-large-newsgroup))
      (let* ((cursor-in-echo-area nil)
	     (input (read-from-minibuffer
		     (format "\
Too many articles were retrieved.  How many articles (max %d): "
			     hit)
		     (cons (number-to-string gnus-large-newsgroup) 0))))
	(unless (string-match "\\`[ \t]*\\'" input)
	  (setcdr (nthcdr (min (1- (string-to-number input)) hit) articles)
		  nil)))))
  articles)

;;;###autoload
(defun gnus-spotlight-search (groups query)
  "Search QUERY through GROUPS with Spotlight,
and make a virtual group contains its results."
  (interactive
   (list
    (gnus-spotlight/get-target-groups)
    (gnus-spotlight/read-query "Enter query: ")))
  (gnus-spotlight/setup)
  (let ((articles (gnus-spotlight/search groups query)))
    (if articles
	(let ((real-groups groups)
	      (vgroup
	       (apply (function format)
		      "nnvirtual:spotlight-search?query=%s&groups=%s&id=%d%d%d"
		      query
		      (if groups (mapconcat 'identity groups ",") "ALL")
		      (current-time))))
	  (gnus-spotlight/truncate-article-list articles)
	  (unless real-groups
	    (dolist (a articles)
	      (add-to-list 'real-groups (gnus-spotlight/article-group a))))
	  ;; Generate virtual group which includes all results.
	  (when (fboundp 'gnus-group-decoded-name)
	    (setq vgroup
		  (encode-coding-string vgroup gnus-spotlight-coding-system)))
	  (setq vgroup
		(gnus-group-read-ephemeral-group
		 vgroup
		 `(nnvirtual ,vgroup
			     (nnvirtual-component-groups ,real-groups)
			     (gnus-spotlight-target-groups ,groups)
			     (gnus-spotlight-current-query ,query))
		 t (cons (current-buffer) (current-window-configuration)) t))
	  ;; Generate new summary buffer which contains search results.
	  (gnus-group-read-group
	   t t vgroup
	   (sort (delq nil ;; Ad-hoc fix, to avoid wrong-type-argument error.
		       (mapcar
			(lambda (a)
			  (nnvirtual-reverse-map-article
			   (gnus-spotlight/article-group a)
			   (gnus-spotlight/article-number a)))
			articles))
		 '<)))
      (message "No entry."))))


;;;###autoload
(defun gnus-spotlight-insinuate ()
  (add-hook
   'gnus-group-mode-hook
   (lambda ()
     (define-key gnus-group-mode-map "\C-c\C-n" 'gnus-spotlight-search)))
  (add-hook
   'gnus-summary-mode-hook
   (lambda ()
     (define-key gnus-summary-mode-map "\C-c\C-n" 'gnus-spotlight-search))))

(provide 'gnus-spotlight)

;;; gnus-spotlight.el ends here.
