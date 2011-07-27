;;; dict.el --- Emacs interface to dict client
;;
;; $Id: dict.el,v 1.43 2004/03/04 17:38:14 fagot Exp $
;;

;; Copyright (c) 2002, 2003, 2004 Max Vasin
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary
;;
;; dict.el is an Emacs wrapper around `dict' command to provide an easy and 
;; comfortable (from my point of view) access to the dictd server from the Emacs.
;; The package was written and tested under GNU Emacs 21 only but I hope it should
;; work under other Emacs versions as well. dict.el depends on bash or compatible shell
;; (I haven't tested it with other shells), cut, sed, awk, and dict. A coding convertion
;; program can be used to use automatic recoding thus allowing databases in different
;; codings.
;;
;; The package provides several key bindings, which are customisation variables,
;; so you can change them easily without recompiling the package:
;;     1. `C-c d d' for running dict with options defined by customisation
;;        variables described below.
;;     2. `C-c d r' for running dict on region as a single word.
;;     3. `C-c d m' for running dict on every word in the region.
;;     4. `C-c d s' for running dict to perform search on the given server.
;;     5. `C-c d S' to view similar words for the last dict run.
;;
;; Descriptions of all customisation variables are given below in their
;; definitions, and of cause you can find them in the customisation buffer
;; (External->Dict).
;;
;; I hope you find the program useful. And also I would like to know your
;; opinion about the program, improvement suggestions and of cause bug reports.
;; Mail them to <max-appolo@mail.ru>

;;; Code:

(require 'cl)

(defgroup Dict nil
  "Browse DICT dictionaries."
  :prefix "dict-"
  :group 'external)

;;;;
;;;; Definitions of dict client parameter variables
;;;;

(defcustom dict-servers '("dict.org" "alt0.dict.org" "alt1.dict.org" "alt2.dict.org")
  "Specifies the hostname for the  DICT  server.
If IP lookup for a server expands to a list of IP addresses (as dict.org
does currently), then each IP will be tried in the order listed."
  :type '(repeat (string :tag "Server name"))
  :group 'Dict)

;; Forward declarations
(defcustom dict-databases nil
  "Foo."
  :type 'string
  :group 'Dict)

(defcustom dict-strategies nil
  "Bar."
  :type 'string
  :group 'Dict)

(defcustom dict-service ""
  "Specifies the port or service for connections.
The default is 2628, as specified in the DICT Protocol
RFC."
  :type 'string
  :group 'Dict)

(defcustom dict-match nil
  "Instead of printing a definition, perform a match using the specified strategy."
  :type 'boolean
  :group 'Dict)

(defcustom dict-nocorrect nil
  "Disable spelling correction.
Usually, if a definition is requested and the word cannot be found,
spelling correction is requested from the server, and a list of
possible words are provided.  This option disables the generation of
this list."
  :type 'boolean
  :group 'Dict)

(defcustom dict-noauth nil
  "Disable authentication (i.e., don't send an AUTH command)."
  :type 'boolean
  :group 'Dict)

(defcustom dict-user ""
  "Specifies the username for authentication."
  :type 'string
  :group 'Dict)

(defcustom dict-key ""
  "Specifies the shared secret for authentication."
  :type 'string
  :group 'Dict)

(defcustom dict-pipesize 256
  "Specify the buffer size for pipelineing commands.
The default is 256, which should be sufficient for general tasks and
be below the MTU for most transport media.  Larger values may provide
faster or slower throughput, depending on MTU.  If the buffer is too
small, requests will be serialised.  Values less than 0 and greater
than one million are silently changed to something more reasonable."
  :type 'integer
  :group 'Dict)

(defcustom dict-original-server ""
  "Specifies original server name for the `dict-on-server' function."
  :type 'string
  :group 'Dict)

(defcustom dict-client ""
  "Specifies additional text to be sent using the CLIENT command."
  :type 'string
  :group 'Dict)

(defcustom dict-always-quote-terms nil
  "If t dict.el will always quote terms."
  :type 'boolean
  :group 'Dict)

(defcustom dict-show-one-window nil
  "If t dict.el will show one window (i.e. without splitting)."
  :type 'boolean
  :group 'Dict)

(defcustom dict-character-recoding-map nil
  "Specifies recoding command for given dictionary."
  :tag "DICT Character Recoding Map"
  :type '(repeat (list :tag "Dict servers"
		  (string :tag "Server name")
		  (repeat :tag "Database recoding mappings" 
			  (list :tag "Database"
			   (regexp :tag "Database name")
			   (string :tag "Recoding command")))))
  :group 'Dict)

;;;;
;;;; Key binding customisation variables and helper functions
;;;;

(defun dict-set-key-binding (key value)
  "Stub for setting KEY binding to VALUE."
  (set-default key value))

(defun dict-set-enable-key-bindings (key value)
  "Stub for setting KEY binding to VALUE."
  (set-default key value))

(defun dict-mode-set-key-binding (key value)
  "Stub for setting KEY binding to VALUE."
  (set-default key value))

(defcustom dict-enable-key-bindings nil
  "Enables key bindings for dict.el commands."
  :type 'boolean
  :group 'Dict
  :set 'dict-set-enable-key-bindings
  :require 'dict)

(defcustom dict-key-binding "\\C-cdd"
  "Specifies a key binding to run dict and display the results in the Emacs buffer."
  :type 'string
  :group 'Dict
  :set 'dict-set-key-binding
  :require 'dict)

(defcustom dict-region-key-binding "\\C-cdr"
  "Specifies a key binding to run dict on the region and display the results in the Emacs buffer."
  :type 'string
  :group 'Dict
  :set 'dict-set-key-binding
  :require 'dict)

(defcustom dict-multiple-key-binding "\\C-cdm"
  "Run dict on region.
Specifies a key binding to run dict on every word from the region
and display the results in the Emacs buffer."
  :type 'string
  :group 'Dict
  :set 'dict-set-key-binding
  :require 'dict)

(defcustom dict-on-server-key-binding "\\C-cds"
  "Run dict on server.
Specifies a key binding to run dict to search word on the given
server and display the results in the Emacs buffer."
  :type 'string
  :group 'Dict
  :set 'dict-set-key-binding
  :require 'dict)

(defcustom dict-similar-words-key-binding "\\C-cdS"
  "Specifies a key binding to show similar words."
  :tag "Show similar words"
  :type 'string
  :group 'Dict-Mode
  :set 'dict-set-key-binding
  :require 'dict)

(defgroup Dict-Mode nil
  "DICT-mode key bindings"
  :tag "DICT-mode"
  :prefix "dict-mode-"
  :group 'Dict)

(defcustom dict-mode-key-binding "d"
  "Specifies a key binding to run dict and display the results in the Emacs buffer."
  :tag "DICT"
  :type 'string
  :group 'Dict-Mode
  :set 'dict-mode-set-key-binding
  :require 'dict)

(defcustom dict-mode-region-key-binding "r"
  "Specifies a key binding to run dict on the region and display the results in the Emacs buffer."
  :tag "DICT region"
  :type 'string
  :group 'Dict-Mode
  :set 'dict-mode-set-key-binding
  :require 'dict)

(defcustom dict-mode-multiple-key-binding "m"
  "Run dict on every word in region.
Specifies a key binding to run dict on every word from the region and
display the results in the Emacs buffer."
  :tag "DICT multiple"
  :type 'string
  :group 'Dict-Mode
  :set 'dict-mode-set-key-binding
  :require 'dict)

(defcustom dict-mode-on-server-key-binding "s"
  "Specifies a key binding to run dict to search word on the given server and display the results in the Emacs buffer."
  :tag "DICT on server"
  :type 'string
  :group 'Dict-Mode
  :set 'dict-mode-set-key-binding
  :require 'dict)

(defcustom dict-mode-similar-words-key-binding "S"
  "Specifies a key binding to show similar words."
  :tag "Show similar words"
  :type 'string
  :group 'Dict-Mode
  :set 'dict-mode-set-key-binding
  :require 'dict)

(defcustom dict-buffer-coding-system nil
  "Specifies coding system to use in dict buffer."
  :tag "Input coding system for DICT buffer"
  :type 'string
  :group 'Dict-Mode)

(defvar dict-similar-buffer nil)

;;;;
;;;; Service functions
;;;;

(defun dict-get-databases (host)
  "Get a list of available databases."
  (let* ((dbinfo-string (shell-command-to-string (format "dict -h %s -D 2> /dev/null | awk 'BEGIN { print \"(\"; } \
/^[ ]+/ { match($0, /^[ ]+([a-z0-9]+)[ ]+(.+)/, r); print \"(\\\"\" r[1] \"\\\"\" \" \\\"\" r[2]\"\\\")\"; } \
END { print \")\" }'" host)))
	 (dbinfo (read dbinfo-string))
	 (dbnames (mapcar 'car dbinfo))
	 (dbdecss (mapcar 'cadr dbinfo)))
    `(,dbnames ,dbdecss)))

(defun dict-get-strategies (host)
  "Get a list of strategies."
  (let* ((stratsinfo-string (shell-command-to-string (format "dict -h %s -S 2> /dev/null | awk 'BEGIN { print \"(\"; } \
/^[ ]+/ { match($0, /^[ ]+([a-z0-9]+)[ ]+(.+)/, r); print \"(\\\"\" r[1] \"\\\"\" \" \\\"\" r[2]\"\\\")\"; } \
END { print \")\" }'" host)))
	 (stratsinfo (read stratsinfo-string))
	 (stnames (mapcar 'car stratsinfo))
	 (stdecss (mapcar 'cadr stratsinfo)))
    `(,stnames ,stdecss)))

(defun dict-generate-constant (value tag)
  "Generate constant for customisation type of VALUE with TAG."
  `(const :tag ,tag ,value))

(defun dict-get-database-names (host)
  "Get a list of available database names."
  (mapcar 'symbol-name
	  (read (concat "(" (shell-command-to-string 
			     (format "dict -h %s -D 2> /dev/null | cut -f 2 -d ' ' | sed -e '1 d'" host host)) ")"))))

(defcustom dict-strategies  (mapcar (lambda (h) (list h nil)) dict-servers)
  "Specify a matching strategy.
By default, the server default match strategy is used.  This is
usually \"exact\" for definitions, and some form of
spelling-correction strategy for matches (\".\" fromthe DICT
protocol). The available strategies are dependent on the server
implemenation."
  :type `(list :tag "Server" ,@(mapcar
		  (lambda (h) 
		    `(list (const ,h)
			   (choice :tag "Strategies" ,@(apply 'mapcar* 'dict-generate-constant (dict-get-strategies h)) 
				   (const :tag "default" nil))))
		  dict-servers))
  :group 'Dict)

(defcustom dict-databases (mapcar (lambda (h) (list h (dict-get-database-names h))) dict-servers)
  "Specifies a specific database to search.
The default is to search all databases (a * from the DICT
protocol).  Note that a ! in the DICT protocol means to search all of
the databases until a match is found, and then stop searching."
  :type `(list :tag "Server" ,@(mapcar
				(lambda (h)
				  `(list (const ,h)
					 (set :tag "Databases" ,@(apply 'mapcar* 'dict-generate-constant 
									(dict-get-databases h)))))
				dict-servers))
  :group 'Dict)

(defun dict-generate-options-list (prefix seq)
  "Generate a list of options of the form `PREFIX SEQ[0] PREFIX SEQ[1] ...'."
  (if (null seq)
      ""
      (concat prefix (car seq) (dict-generate-options-list prefix (cdr seq)))))

(defsubst dict-nes (string)
  "T if STRING is not empty."
  (not (string= string "")))

(defun dict-generate-options (database host)
  "Generate dict's command line options based on the parameter variables' values, DATABASE and HOST"
  (concat
   (if (dict-nes dict-service) (concat " --port " dict-service) "")
   (if dict-match " --match" "")
   (if (cadr (assoc host dict-strategies)) (concat " --strategy " (cadr (assoc host dict-strategies)) ""))
   (if dict-nocorrect " --nocorrect ")
   (if dict-noauth " --noauth" "")
   (if (dict-nes dict-user) (concat " --user " dict-user) "")
   (if (dict-nes dict-key) (concat " --key " dict-key) "")
   (if (not (= dict-pipesize 256)) (concat " --pipesize " (number-to-string dict-pipesize)) "")
   (if (dict-nes dict-client) (concat " --client " dict-client) "")
   (concat " --database " database)
   (concat " --host " host)
   " --pager -" ; force dict to not use pager
   " "))

(defun dict-newline-to-space (string)
  "Replace newline with space in STRING."
  (let ((result (make-string (length string) ?x)))
    (dotimes (i (length string) 1)
      (aset result i (if (char-equal (aref string i) ?\n) ?\  (aref string i))))
    result))

(defun dict-reduce-spaces (string)
  "Replace multiple sequencial whitespaces in STRING with one whitespace."
  (if (not (string-match "[ \t][ \t]+" string))
      string
    (dict-reduce-spaces (replace-match " " t "\\&" string nil))))

(defsubst dict-normalise-request (request)
  "Replace newlines and multiple spaces with one space in the REQUEST."
  (dict-reduce-spaces (dict-newline-to-space request)))

(defun dict-quote (word)
  "Quote WORD if necessary."
  (if dict-always-quote-terms
      (if (or (and (eq (aref word 0) ?\")
		   (eq (aref word (- (length word) 1)) ?\"))
	      (and (eq (aref word 0) ?\')
		   (eq (aref word (- (length word) 1)) ?\')))
	  word
	(concat "'" word "'"))
    word))

(defun dict-generate-command (word database host)
  "Generate dict command to search in the given DATABASE and HOST."
  (let ((recoding-command (or 
			   (cadr (assoc database (cadr (assoc host dict-character-recoding-map))))
			   (cadr (assoc "*"  (cadr (assoc host dict-character-recoding-map)))))))
    (if recoding-command
	(format "dict %s %s | %s" (dict-generate-options database host) (dict-quote word) recoding-command)
    (format "dict %s %s" (dict-generate-options database host) (dict-quote word)))))

(defun dict-get-answer (what)
  "Recieve the answer for WHAT from the dict and insert in ther buffer."
  (let* ((word (dict-normalise-request what))
	 (buffer-name (concat "*DICT " word "*"))
	 (similar-buffer-name (concat "*DICT " word " (similar) *"))
	 (buffer (or (get-buffer buffer-name) (generate-new-buffer buffer-name)))
	 (similar-buffer (or (get-buffer similar-buffer-name) (generate-new-buffer similar-buffer-name)))
	 (coding-system-for-read dict-buffer-coding-system)
	 (coding-system-for-write dict-buffer-coding-system))
    (setq dict-similar-buffer similar-buffer)
    (save-current-buffer
      (set-buffer similar-buffer)
      (kill-region (point-min) (point-max))
      (set-buffer buffer)
      (kill-region (point-min) (point-max))
      (make-local-variable 'dict-similar-buffer)
      (setq dict-similar-buffer similar-buffer)
      (dict-mode))
    (message "Invoking dict %s in the background" word)
    (mapcar (lambda (host)
	      (mapcar (lambda (database)
			(set-process-sentinel
			 (start-process "dict" (generate-new-buffer "dict") "sh" "-c" 
					(dict-generate-command word database host))
			 (dict-make-sentinel-with-buffer buffer)))
		      (if (cadr (assoc host dict-databases)) (cadr (assoc host dict-databases)) 
			(dict-get-database-names host))))
	    dict-servers)))

(defun dict-add-result-to-buffer (result-buffer output-buffer)
  "Add dict's answer from RESULT-BUFFER to OUTPUT-BUFFER."
  ;; Preformat data in the result buffer
  (set-buffer result-buffer)
  (goto-char (point-min))
  (kill-line 2)

  ;; Insert answer into the output buffer
  (set-buffer output-buffer)
  (goto-char (point-max))
  (insert-buffer-substring result-buffer)
  (kill-buffer result-buffer)
  (goto-char (point-min))
  (display-buffer output-buffer)
  (when dict-show-one-window 
    (switch-to-buffer output-buffer)
    (delete-other-windows)))

(defun dict-get-error-message (string)
  "Returns error message, cutting ', perhaps you mean:' from the STRING."
  (if (char-equal (elt string (- (length string) 1)) ?:)
      (substring string 0 (- (length string) (length ", perhaps you mean:")))
    string))

(defun dict-make-sentinel-with-buffer (buffer)
  "Make process sentinel to write result to BUFFER."
  (lexical-let ((output-buffer buffer))
    (lambda (process msg)
      (let ((process-buffer (process-buffer process)))
	(cond
	 ((string= "finished\n" msg)
	  (save-excursion
	    (set-buffer process-buffer)
	    (set-buffer-modified-p nil)
	    (beginning-of-buffer)
	    (if (string= (buffer-substring-no-properties (point-min) 25) "No definitions found for")

		;; dict didn't find word in the given database
		(let ((error-message (dict-get-error-message 
				      (buffer-substring-no-properties (point-min) (- (search-forward "\n") 1)))))
		  (let ((similar-words (buffer-substring-no-properties (point) (- (point-max) 1))))
		    (kill-buffer (current-buffer))
		    (set-buffer output-buffer)
		    (set-buffer dict-similar-buffer)
		    (insert similar-words)
		    (message error-message)))

	      ;; ok we've got an answer
	      (dict-add-result-to-buffer process-buffer output-buffer))))
	 ((string-match "exited abnormally with code" msg)
	  (message msg)))))))

(defsubst dict-default-dict-entry ()
  "Make a guess at a default dict entry.
This guess is based on the text surrounding the cursor."
  (let (word)
    (save-excursion
      (setq word (current-word))
      (cond ((null word) "")
	    ((string-match "[._]+$" word)
	     (substring word 0 (match-beginning 0)))
	    (t word)))))


;;;;
;;;; Lookup functions
;;;;

(defun dict (word)
  "Lookup a WORD in the dictionary."
  (interactive (list (let* ((default-entry (dict-default-dict-entry))
	     (input (read-string
		     (format "Dict entry%s: "
			     (if (string= default-entry "")
				 ""
			       (format " (default %s)" default-entry))))))
	(if (string= input "")
	    (if (string= default-entry "")
		(error "No dict args given") default-entry) input))))
  (dict-get-answer word))

(defun dict-region (from to)
  "Lookup a region (FROM, TO) in the dictionary."
  (interactive (list (region-beginning)
		     (region-end)))
  (dict (concat "\"" (buffer-substring-no-properties from to) "\"")))

(defun dict-multiple (from to)
  "Lookup every word from the region (FROM, TO) in the dictionary."
  (interactive (list (region-beginning)
		     (region-end)))
  (dict (buffer-substring-no-properties from to)))

(defun dict-on-server (word server)
  "Lookup a WORD in the dictionary on the given SERVER."
  (interactive (list
		(let* ((default-entry (dict-default-dict-entry))
		       (input (read-string
			       (format "Dict entry%s: "
				       (if (string= default-entry "")
					   ""
					 (format " (default %s)" default-entry))))))
		  (if (string= input "")
		      (if (string= default-entry "")
			  (error "No dict args given") default-entry) input))
		(read-string "DICT server: " nil)))
  (if (not (string= server ""))
      (let ((dict-servers `(,server)))
	(dict word))
    (dict word)))

(defun dict-show-similar ()
  "Show list of similar words."
  (interactive)
  (when (bufferp dict-similar-buffer)
    (display-buffer dict-similar-buffer)))

(defun dict-set-key-binding (key value)
  "Set KEY binding customisation variable to VALUE."
  (let ((result (set-default key value)))
    (set-default 'dict-enable-key-bindings t)
    (dict-update-key-bindings)
    result))

(defun dict-set-enable-key-bindings (key value)
  "Set KEY to VALUE and update dict key bindings."
  (let ((result (set-default key value)))
    (dict-update-key-bindings)
    result))

(defun dict-process-key-binding (string)
  "Process a STRING representing a key binding to allow easy key binding customisation."
  (read (concat "\"" string "\"")))

(defvar dict-mode-keymap (make-sparse-keymap))

(defun dict-mode-set-key-binding (key value)
  "Set KEY binding customisation variable to VALUE (for DICT-mode)."
  (let ((result (set-default key value)))
    (dict-mode-update-key-bindings)
    result))

(defun dict-mode ()
  (interactive)
  (use-local-map dict-mode-keymap)
  (setq mode-name "DICT")
  (setq major-mode 'dict-mode))

(defun dict-update-key-bindings ()
  "Update dict key bindings."
  (when dict-enable-key-bindings
    ;; Setup global key binding `C-c d d' for running dict...
    (global-set-key (dict-process-key-binding dict-key-binding) 'dict)
    ;; ... `C-c d r' for running dict on the region...
    (global-set-key (dict-process-key-binding dict-region-key-binding) 'dict-region)
    ;; ... `C-c d m' for running dict on every word in the region...
    (global-set-key (dict-process-key-binding dict-multiple-key-binding) 'dict-multiple)
    ;; ... `C-c d s' for running dict to perform search on the given server.
    (global-set-key (dict-process-key-binding dict-on-server-key-binding) 'dict-on-server)
    ;; ... `S' to show similar words.
    (global-set-key (dict-process-key-binding dict-similar-words-key-binding) 'dict-show-similar)))


(defun dict-mode-update-key-bindings ()
  "Update dict key bindings for DICT-mode."
  ;; Setup DICT-mode key binding `d' for running dict...
  (define-key dict-mode-keymap (dict-process-key-binding dict-mode-key-binding) 'dict)
  ;; ... `r' for running dict on the region...
  (define-key dict-mode-keymap (dict-process-key-binding dict-mode-region-key-binding) 'dict-region)
  ;; ... `m' for running dict on every word in the region...
  (define-key dict-mode-keymap (dict-process-key-binding dict-mode-multiple-key-binding) 'dict-multiple)
  ;; ... `s' for running dict to perform search on the given server...
  (define-key dict-mode-keymap (dict-process-key-binding dict-mode-on-server-key-binding) 'dict-on-server)
  ;; ... `S' to show similar words.
  (define-key dict-mode-keymap (dict-process-key-binding dict-mode-similar-words-key-binding) 'dict-show-similar))

;;;;
;;;; Informational functions
;;;;

(defun dict-version ()
  "Display dict version information."
  (interactive)
  (shell-command "dict --version"))

(defconst dict-version
  "$Revision: 1.43 $"
  "Version number for 'dict' package.")

(defun dict-version-number ()
  "Return 'dict' version number."
  (string-match "[0123456789.]+" dict-version)
  (match-string 0 dict-version))

(defun dict-display-version ()
  "Display 'dict.el' version."
  (interactive)
  (message "dict.el version <%s>." (dict-version-number)))

(dict-update-key-bindings)
(dict-mode-update-key-bindings)

(provide 'dict)

;;; dict.el ends here
