;;; hachette.el
;; Lookup a word on the Hachette's online french dictionary.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp Archive Entry
;; Filename: hachette.el
;; Author: Jean-Philippe Theberge (jphi21@users.sourceforge.net)
;; Version: 1.11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Created: 24/02/2000 - update: 08/11/2001
;; Keywords: Dictionaire, Hachette, French dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (c) 1998 - 2001 Free Software Foundation, Inc.
;;
;; This file is not part of GNU Emacs. :-(
;;
;; This file is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;; 
;; Initialy based on dict-web.el by Eric Marsden (but now very different)
;;
;; The definition come from the online dictionary at
;; http://www.francophonie.hachette-livre.fr
;; and is copyrighted by them.
;;
;; Version History
;; 1.0   Initial implementation
;; 1.1   Cleanup and commented for gnu.emacs.source posting
;; 1.2   Removed dependencies on w3
;; 1.3   Some bad codes reworked
;; 1.4   Some debuging.  Change in hachette-fetch-word-at-point
;; 1.5   Addition of hachette-hexify-string to encode the url.
;; 1.5.1 Quelques modification mineures.

;; 1.6   Modification pour le cas ou mule n'est pas installe
;;       Changement de la regexp pour reconnaitre la notice de copyright.
;;       Support pour le serveur proxy.

;; 1.7   Perfectionnement de la regexp pour font-lock
;;       Language selection for user interaction.

;; 1.8   Put back w3 support (optional).
;;       w3 user can fetch definition asynchronously.
;;       Encore une modif a la regexp pour font-lock.
;;       Customize support.

;; 1.8.1 Modification to the copyright notice
;;       (Thanks to Hrvoje Niksic)

;; 1.9 Passage through 'checkdoc' to fix the package auto
;;      documentation.  Added the hash *hachette-memoize* to remember
;;      already fetched definitions.  (Thanks to Thomas F. Burdick)

;; 1.10 add memorisation of the list of words (so there is no
;;       connection to the server if you search a word again).

;; 1.11 Use iso2isosgml in emacs21 (old replace-from-table function
;;       keeped for backward compat) remove the prompt for killing the
;;       def window with 'q' but it may still be used by setting
;;       hachette-confirm-window-killing to 't'.  The y-or-n function
;;       may also be configured with hachette-yes-or-no-function
;;       
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TO DO
;;
;; + Merge with other dictionary package to make it more generic?
;; + Improve the language recognition.
;; + Make the definition buffer prettier (some images, maybe?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To use:
;;
;; M-x hachette
;; Prompt for a word (suggesting word at point)
;;
;; M-x hachette-fetch-word-at-point
;; Lookup word at point without prompting.
;; If hachette-use-language-guessing is t and
;; the buffer seem to be in english,
;; using dict-web is proposed.

;;; Code:
(require 'derived)
(require 'cl)

(defconst hachette-version "1.12")

(defgroup hachette nil
  "Lookup a word on the Hachette's online french dictionary."
  :group 'hypermedia)

(defcustom hachette-use-language-guessing nil
  "Try to guess the buffer language and use another package (like dict-web) if it seem more appropriate.  (in development)."
  :type 'boolean
  :group 'hachette)

(defcustom hachette-proxy-port nil
  "Proxy port number, (or nil)."
  :type 'integer
  :group 'hachette)

(defcustom hachette-proxy-host nil
  "Proxy hostname (or nil)."
  :type 'string
  :group 'hachette)

(defcustom hachette-speak-french t
  "Use french for prompt and other user interaction."
  :type 'boolean
  :group 'hachette)

(defcustom hachette-use-w3 nil
  "Use the url package from Bill Perry's W3."
  :type 'boolean
  :group 'hachette)

(defcustom hachette-be-asynchronous t
  "If w3 is used, url fetching can be asynchronous."
  :type 'boolean
  :group 'hachette)

(defcustom hachette-confirm-window-killing nil
  "if t confirm hachette definition window killing"
  :type 'boolean
  :group 'hachette)

(defcustom hachette-yes-or-no-function 'y-or-n-p
  "Function to kill the definition window (if hachette-confirm-window-killing is t)"
  :type 'symbol
  :group 'hachette)

(if hachette-use-w3
    (if (locate-library "url")
	(progn
	  (require 'url)
	  (setq url-be-asynchronous hachette-be-asynchronous)
	  (defun url-http-user-agent-string ()
	    (if (or (eq url-privacy-level 'paranoid)
		    (and (listp url-privacy-level)
			 (memq 'agent url-privacy-level)))
		""
	      (format "User-Agent: %s/%s URL/%s hachette.el/%s %s\r\n"
		      url-package-name url-package-version
		      url-version hachette-version
		      (cond
		       ((and url-os-type url-system-type)
			(concat " (" url-os-type "; " url-system-type ")"))
		       ((or url-os-type url-system-type)
			(concat " (" (or url-system-type url-os-type) ")"))
		       (t ""))))))
      (progn
       (message (if hachette-speak-french "Je ne trouve pas la librairie w3" "Unable to find w3"))
       (setq hachette-use-w3 nil))))

(defvar *hachette-memoize-D-hash* 
  (make-hash-table 
   :test 'eq))

(defvar *hachette-memoize-L-hash* 
  (make-hash-table 
   :test 'eq))

(defvar emacs21p (>= emacs-major-version 21))

(if (not emacs21p)
    (defconst sgml-entities
      ;; this will be obsolete in emacs21
      ;; use iso-iso2sgml.
      '(("&Agrave;" "À")
	("&Acirc;" "Â")
	("&Auml;" "Ä")
	("&Ccedil;" "Ç")
	("&Egrave;" "È")
	("&Ecirc;" "Ê")
	("&Eacute;" "É")
	("&Euml;" "Ë")
	("&Igrave;" "Ì")
	("&Iuml;" "Ï")
	("&Ograve;" "Ò")
	("&Ocirc;" "Ô")
	("&Ouml;" "Ö")
	("&Ugrave;" "Ù")
	("&Ucirc;" "Û")
	("&Uuml;" "Ü")
	("&agrave;" "à")
	("&acirc;" "â")
	("&auml;" "ä")
	("&ccedil;" "ç")
	("&egrave;" "è")
	("&eacute;" "é")
	("&ecirc;" "ê")
	("&euml;" "ë")
	("&igrave;" "ì")
	("&icirc;" "î")
	("&iuml;" "ï")
	("&ograve;" "ò")
	("&ocirc;" "ô")
	("&ouml;" "ö")
	("&ugrave;" "ù")
	("&ucirc;" "û")
	("&uuml;" "ü")
	("&#171;" "«")
	("&#187;" "»")
	("&copy;" "©"))))

(defconst hachette-server "www.francophonie.hachette-livre.fr")

(defun hachette-show-version ()
  "Show hachette version number."
  (interactive)
  (message "Hachette.el version %s" hachette-version))

(if (not hachette-use-w3)
    (defconst url-unreserved-chars
      '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
	   ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
	   ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
	   ?$ ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\) ?,)
      "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from draft-fielding-url-syntax-02.txt - check your local
internet drafts directory for a copy.
(taken from url.el by William M. Perry)"))
;"
(define-derived-mode hachette-definition-mode text-mode "Hachette"
  "Major mode to display definition fetched from the Hachette online dictionary.
Special commands:
\\{hachette-definition-mode-map}"
  (setq hachette-definition-font-lock-keywords
	(list
	 '("^[0-9]+\." . font-lock-keyword-face)
	 '("^\\*\\* \\([a-z]*\\) " 1 font-lock-function-name-face)
	 '("\\(||\\|--\\)" . font-lock-function-name-face)
	 '(" \\(MATH\\|TECH\\|SPORT\\|SC NAT\\|GRAM\\|DR\\|MAR\\|PALEONT\\|LOG\\|syn\\.\\|fig\\.\\|ant\\.\\|Loc\\.\\|ASTROL?\\|Ellipt\\.\\|Anc\\.\\|Encycl\\.\\|n\\. +Fam\\.\\|Litt\\.\\|n\\. +[mf]\\.\\) " . font-lock-reference-face)
	 '("^ *\\(Dict\\(.\\|
\\)*interdite\\.\\)" 1 font-lock-comment-face prepend))) ; Merci a Edric Ellis!
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(hachette-definition-font-lock-keywords nil t)))

(define-key hachette-definition-mode-map [return] 'hachette-fetch-word-at-point)
(define-key hachette-definition-mode-map "q" 'hachette-kill-buffer-and-window)

(defun hachette (word)
  "Lookup a WORD on the Hachette's online french dictionary."
  (interactive (list (read-string 
		      (if hachette-speak-french "Mot: " "Word: ")
		      (current-word))))
  (let ((wordL (hachette-fetch-wordL word)))
    (cond ((equal (length wordL) 1)(hachette-fetch-def (cdar wordL)))
	  ((null wordL)(message (if hachette-speak-french "Aucune definition trouvée." "No definition found.")))
	  (t (progn
	       (require 'widget)
	       (require 'cl)
	       (pop-to-buffer "*Hachette-liste*")
	       (erase-buffer)
	       (widget-minor-mode 1)
	       (mapcar
		(lambda (x)
		  (lexical-let ((x x))
		    (widget-create 'push-button
				   :notify (lambda  (widget &rest ignore)
					     (kill-buffer "*Hachette-liste*")
					     (hachette-fetch-def (cdr x)))
				   (car x))
		    (widget-insert "\n")))
		wordL)
	       (widget-setup))))))

(defun hachette-insert-url-contents (host path)
  "Send a http request to HOST for the page PATH and insert the contents at point."
  (ignore-errors (kill-buffer "*hachette-temp-buffer*"))
  (if hachette-use-w3
      (url-insert-file-contents (concat "http://" host "/" path))
    (let* ((coding-system-for-read 'binary)
	   (coding-system-for-write 'binary)
	   (http (open-network-stream
		  "hachette-retrieval-process"
		  "*hachette-temp-buffer*"
		  (if hachette-proxy-host hachette-proxy-host host)
		  (if hachette-proxy-port hachette-proxy-port 80)))
	   (pbuf (process-buffer http)))
      (process-send-string
       http (concat
	     "GET "
	     (if hachette-proxy-host
		 (concat "http://" host "/")
	       "/")
	     path " HTTP/1.0\r\n\r\n"))
      (while (eq (process-status http) 'open)
	(sleep-for 1))
      (insert-buffer pbuf)
      (kill-buffer pbuf))))

(defun hachette-fetch-word-at-point ()
  "Lookup the word at point on the Hachette's online french dictionary."
  (interactive)
  (require 'thingatpt)
  (let ((word (thing-at-point 'word)))
    (if (null word) (error (if hachette-speak-french "Selectionez un mot!" "No word selected")))
    (if (and hachette-use-language-guessing
	     (locate-library "dict-web")
	     (equal (guess-buffer-language) "en"))
	(if (y-or-n-p (if hachette-speak-french "Le buffer me semble en anglais, utiliser dict-web a la place? "
			"It seem the buffer is in english.  Do you want to use dict-web instead? "))
	    (progn
	      (require 'dict-web)
	      (dict word))
	  (hachette word))
      (hachette word))))

(defun guess-buffer-language ()
  ;; This defun is very basic and must be improved.
  "Try to guess if the buffer is in french or english."
  (save-excursion
    (goto-char (point-min))
    (let* ((french-stop-words  " \\(et\\|ou\\|[ld]es\\|que\\) ")
	   (english-stop-words " \\(of\\|the\\|and\\|or\\) ")
	   (en (string-to-number (count-matches english-stop-words)))
	   (fr (string-to-number (count-matches french-stop-words))))
      (if (> en fr)
	  "en"
	"fr"))))

(defun hachette-hexify-string (str)
  "Convert reserved char to hex (for the url).
Argument STR string to convert."
  (mapconcat
   (function
    (lambda (char)
      (if (not (memq char url-unreserved-chars))
	  (if (< char 16)
	      (upcase (format "%%0%x" char))
	    (upcase (format "%%%x" char)))
	(char-to-string char))))
   (if (featurep 'mule)
       (encode-coding-string str 'iso-8859-1)
     str) ""))

(defun hachette-fetch-wordL (word)
  "Return an Alist of words matching WORD and the id required to fetch the definition."
  (with-temp-buffer
    (let* ((key (intern word))
	   (lst (gethash key *hachette-memoize-L-hash*)))
      (if lst lst
	(hachette-insert-url-contents hachette-server
				      (concat "/cgi-bin/hysearch2?V="
					      (hachette-hexify-string word)
					      "&E=1"))
	(setq url-list nil)
	(let ((url-list nil)
	      (word-list nil))
	  (unsgmlify)
	  (set-buffer-modified-p nil)
	  (goto-char (point-min))
	  (while (re-search-forward "sgmlex2\\?\\(.*\\)\"" nil t)
	    (setq url-list (cons (buffer-substring (match-beginning 1)(match-end 1)) url-list)))
	  (goto-char (point-min))
	  (while (re-search-forward "border=0> \\(.*\\)</a><br>" nil t)
	    (setq word-list (cons (buffer-substring (match-beginning 1)(match-end 1)) word-list)))
	  (let ((L (map 'list (lambda (x y) (cons x y)) word-list url-list)))

	    (setf (gethash key *hachette-memoize-L-hash*) L)
	    L))))))


(defun hachette-fetch-def (id)
  "Fetch a definition from the dictionary.
Argument ID is send to the CGI-script."
  (pop-to-buffer "*Hachette*")
  (setq buffer-read-only nil)
  (hachette-definition-mode)
  (erase-buffer)
  (let ((url (concat "/cgi-bin/sgmlex2?" id)))
    (let* ((key (intern url))
	   (string (gethash key *hachette-memoize-D-hash*)))
      (if string (insert-string string)
	(progn
	  (hachette-insert-url-contents hachette-server url)
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    (when (re-search-forward "<strong>" nil t)
	      (delete-region (point-min) (match-beginning 0)))
	    (when (re-search-forward "target=liste>" nil t)
	      (delete-region (point-min) (match-end 0)))
	    (insert "** ")
	    (while (re-search-forward "</?\\(font\\|a\\)[^>]*>" nil t)
	      (delete-region (match-beginning 0)(match-end 0)))
	    (goto-char (point-min))
	    (while (re-search-forward "\n" nil t)
	      (replace-match " "))
	    (goto-char (point-min))
	    (while (re-search-forward "<strong>" nil t)
	      (replace-match "\n\n"))
	    (goto-char (point-min))
	    (when (re-search-forward "\\(<p>&nbsp;\\)+" nil t)
	      (replace-match "\n\n\n"))
	    (goto-char (point-min))
	    (while (re-search-forward "<[^>]+>" nil t)
	      (delete-region (match-beginning 0)(match-end 0)))
	    (fill-region (point-min) (point-max))
	    (replace-from-table sgml-entities)
	    (setf (gethash key *hachette-memoize-D-hash*)
		  (buffer-string)))))))
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (turn-on-font-lock))

(defun hachette-kill-buffer-and-window ()
  "Kill the current buffer and delete the selected window."
  (interactive)
  (if (or (not hachette-confirm-window-killing)
	  (funcall hachette-yes-or-no-function (if hachette-speak-french "Quitter? " "Quit? ")))
      (let ((buffer (current-buffer)))
	(delete-window (selected-window))
	(kill-buffer buffer))))

(if (not emacs21p)
    (defun replace-from-table (lst)
      "Use the alist to search and replace a whole buffer.
Argument LST Associative list of string and replacement-string."
      (let ((case-fold-search nil))
	(cond ((null lst) lst)
	      (t (progn
		   (let ((pos (point-marker)))
		     (goto-char (point-min))
		     (while (re-search-forward (car (car lst)) nil t)
		       (replace-match (car (cdr (car lst))) t t))
		     (goto-char pos))
		   (replace-from-table (cdr lst))))))))

(defun unsgmlify ()
  (if emacs21p (iso-sgml2iso (point-min)(point-max))
    (replace-from-table sgml-entities)))


(provide 'hachette)

;;; hachette.el ends here
