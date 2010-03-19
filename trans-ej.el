;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;; trans-ej.el --- English-Japanese translator using web translation service

;; Copyright (c) 2009, 2010  S. Irie

;; Author: S. Irie
;; Maintainer: S. Irie
;; Keywords: multilingual, translation

(defconst trans-ej-version "0.1.1")

;; This program is free software.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This program provides some interactive functions in order that we can
;; easily request the text translation between English and Japanese to
;; multiple Web translation services. These functions were implemented
;; using `url-retrieve', so the responses will be displayed asynchronously
;; in the popup window.

;; This program is tested on GNU Emacs 22, 23.

;;
;; Installation:
;;
;; First, save this file as trans-ej.el and byte-compile in a directory
;; that is listed in load-path.
;;
;; Put the following in your .emacs file:
;;
;;   (require 'trans-ej)
;;
;; To translate the buffer text, select region you desire, and just type as:
;;
;;   M-x trans-ej
;;
;; Have fun!
;;

;;; ChangeLog:

;; 2010-03-18  S. Irie
;;        * Version 0.1.1
;;        * Bug fix
;; 2010-03-18  S. Irie
;;        * Version 0.1.0
;;        * trans-ej-mode: Added major mode for displaying the results
;;        * Changed to make hyperlinks to each Web site
;;        * Changed to propertize error messages by `shadow' face
;;        * Separated `trans-ej-make-request-data' from `trans-ej-string-1'
;; 2010-03-17  S. Irie
;;        * Version 0.0.7
;;        * trans-ej: Changed first argument to STRING
;;        * Added many user commands:
;;           `trans-ej-en2ja-string', `trans-ej-ja2en-string', `trans-ej-string'
;;           `trans-ej-en2ja-buffer', `trans-ej-ja2en-buffer', `trans-ej-buffer'
;;           `trans-ej-en2ja', `trans-ej-ja2en'
;; 2010-03-17  S. Irie
;;        * Version 0.0.6
;;        * trans-ej: Modified `interactive' form to ask buffer name
;;        * Wrote commentary
;;        * Added/modified docstrings
;;        * Deleted support for Translated.net
;; 2009-11-24  S. Irie
;;        * Version 0.0.5
;;        * Added mechanism which automatically uncomment according to major mode
;;        * Added supports for OCN translation and Fresheye translation
;; 2009-11-23  S. Irie
;;        * Version 0.0.4
;;        * Added mechanism that join-lines can be performed before requests
;;        * Added supports for Yahoo! BABEL FISH, MyMemory Translated.net,
;;          Translated.net and livedoor translate
;; 2009-11-21  S. Irie
;;        * Version 0.0.3
;;        * Multiple asynchronous translation
;;        * Added new functions `trans-ej-string-1', `trans-ej-string-all'
;;        * Added support for Google translate
;; 2009-11-21  S. Irie
;;        * Version 0.0.2
;;        * Added prototype of `trans-ej-string-1'
;; 2009-11-20  S. Irie
;;        * Version 0.0.1
;;        * Earliest version
;;        * Suppots for Excite translation and Yahoo!JP translation
;;        * User commands `trans-ej', `trans-ej-region', etc.

;;; ToDo:

;;; Code:

(require 'url-util)
(require 'button)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar trans-ej-site-orders-alist
  '(((en ja) excite yahoojp ocn livedoor fresheye google)
    ((ja en) excite yahoojp ocn livedoor fresheye google))
  "Alist which specifies the displaying order of translation results.
Each elemant is a list like ((SOURCE TARGET) SITE-SYMBOLS...).

SOURCE and TARGET are symbols, which represent the source language and
the target language, respectively.

SITE-SYMBOLS are symbols representing the Web site, and used as the first
argument of `trans-ej-string-1' in turn.")

(defvar trans-ej-selected-dictionaries-alist
  '((mymemory . "All"))
  "Alist which specifies the dictionary used in each Web service.
Each element is a cons cell like (SITE-SYMBOL . DICT-NAME).

SITE-SYMBOL is a symbol representing the Web site, and used as the first
argument of `trans-ej-string-1'.

DICT-NAME is a string which specifies the category of dictionary, and
must be included in `trans-ej-dictionaries-selections-alist'.")

(defvar trans-ej-result-buffer-name "*Translation (%s -> %s)*"
  "Format string specifying the name of result buffer. This must have
two `%s' in order to specify the positions in which language keys will
be inserted as few letters, where the first one and second one represent
source language and target language, respectively.")

(defvar trans-ej-decode-response-debug nil
  "If non-nil, dump the whole of returned HTML when the translated text
cannot be recognized.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Site profiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar trans-ej-languages-alist
  '((en "English"
	("\\([^\\.?!: \t\n][])\"']?\\)[ \t]*\n[ \t]*\\([^ \t\n]\\)" . "\\1 \\2"))
    (ja "Japanese"
	("\\([^。？！ \t\n][」）]?\\)[ \t]*\n[ \t]*\\([^ \t\n]\\)" . "\\1\\2")))
  "Alist of specifications of the supported languages. Each element is a
list as (KEY NAME JOIN-LINES), where the first, second and third elements
are a symbol, string and cons cell, respectively.

JOIN-LINES has car and cdr as regular expressions, and is used to connect
the sentences which split into multiple lines before sending to the servers.
See `trans-ej-site-prof-alist' for details.")

(defvar trans-ej-site-profs-alist
  '(
    ;; Excite Japan
    ((excite en ja)
     "Excite Japan translation (powered by BizLingo)"
     "http://www.excite.co.jp/world/english/"
     "wb_lp=ENJA&before=%s"
     japanese-shift-jis-dos
     (("<textarea [^>]*name=\"after\"[^>]*>\\([^<]*\\)</textarea>" . 1)))
    ((excite ja en)
     "Excite Japan translation (powered by BizLingo)"
     "http://www.excite.co.jp/world/english/"
     "wb_lp=JAEN&before=%s"
     japanese-shift-jis-dos
     (("<textarea [^>]*name=\"after\"[^>]*>\\([^<]*\\)</textarea>" . 1)))
    ;; Yahoo! JAPAN
    ((yahoojp en ja)
     "Yahoo! JAPAN honyaku (powered by CROSS LANGUAGE)"
     "http://honyaku.yahoo.co.jp/transtext"
     "eid=CR-EJ&text=%s&both=TH"
     utf-8-dos
     (("<textarea [^>]*name=\"trn_text\"[^>]*>\\([^<]*\\)</textarea>" . 1)))
    ((yahoojp ja en)
     "Yahoo! JAPAN honyaku (powered by CROSS LANGUAGE)"
     "http://honyaku.yahoo.co.jp/transtext"
     "eid=CR-JE&text=%s&both=TH"
     utf-8-dos
     (("<textarea [^>]*name=\"trn_text\"[^>]*>\\([^<]*\\)</textarea>" . 1)))
    ;; OCN
    ((ocn en ja)
     "OCN translation (powered by KODENSHA)"
     "http://cgi01.ocn.ne.jp/cgi-bin/translation/index.cgi"
     "langpair=enja&sourceText=%s"
     utf-8-dos
     (("<TEXTAREA [^>]*NAME=\"responseText\"[^>]*>\\([^<]*\\)</TEXTAREA>" . 1)))
    ((ocn ja en)
     "OCN translation (powered by KODENSHA)"
     "http://cgi01.ocn.ne.jp/cgi-bin/translation/index.cgi"
     "langpair=jaen&sourceText=%s"
     utf-8-dos
     (("<TEXTAREA [^>]*NAME=\"responseText\"[^>]*>\\([^<]*\\)</TEXTAREA>" . 1)))
    ;; livedoor
    ((livedoor en ja)
     "livedoor translate (powered by Amikai)"
     "http://translate.livedoor.com/"
     "trns_type=1,2&src_text=%s"
     utf-8-dos
     (("<textarea [^>]*name=\"tar_text\"[^>]*>\\([^<]*\\)</textarea>" . 1)))
    ((livedoor ja en)
     "livedoor translate (powered by Amikai)"
     "http://translate.livedoor.com/"
     "trns_type=2,1&src_text=%s"
     utf-8-dos
     (("<textarea [^>]*name=\"tar_text\"[^>]*>\\([^<]*\\)</textarea>" . 1)))
    ;; Fresheye
    ((fresheye en ja)
     "Fresheye translation"
     "http://mt.fresheye.com/ft_result.cgi"
     "e=EJ&gen_text=%s"
     utf-8-dos
     (("<TEXTAREA [^>]*name=\"gen_text2\"[^>]*>\\([^<]*\\)</TEXTAREA>" . 1)))
    ((fresheye ja en )
     "Fresheye translation"
     "http://mt.fresheye.com/ft_result.cgi"
     "e=JE&gen_text=%s"
     utf-8-dos
     (("<TEXTAREA [^>]*name=\"gen_text2\"[^>]*>\\([^<]*\\)</TEXTAREA>" . 1))
     (join-lines))
    ;; Google
    ((google en ja)
     "Google translate"
     "http://translate.google.com/"
     "sl=en&tl=ja&text=%s"
     sjis-unix
     (("<span [^>]*id=result_box[^>]*>\\(\\(<span [^>]*>\\([^<]\\|<br>\\)*</span>\\)+\\)</span>" . 1))
     (join-lines
      ("\\(^\\|\\s-\\)[`']\\(\\([^ \t\n']+ \\)*[^ \t\n']+\\)'\\(\\s.\\|[,.]\\|\\s-\\|$\\)" . "\\1\"\\2\"\\4")))
    ((google ja en)
     "Google translate"
     "http://translate.google.com/"
     "sl=ja&tl=en&text=%s"
     utf-8-unix
     (("<span [^>]*id=result_box[^>]*>\\(\\(<span [^>]*>\\([^<]\\|<br>\\)*</span>\\)+\\)</span>" . 1))
     (join-lines))
    ;; MyMemory Translated.net
    ((mymemory en ja)
     "MyMemory Translated.net"
     "http://mymemory.translated.net/s.php"
     "sl=en-GB&tl=ja-JA&q=%s"
     utf-8-dos
     (("<h2>Computer translation</h2>\\(\\(.*\n\\)*.*\\)<h2>" . 1)
      ("</td>[ \t\n]*<td [^>]*>\\(\\(.*\n\\)*.*\\)</td>[ \t\n]*<td " . 1))
     (join-lines))
    ((mymemory ja en)
     "MyMemory Translated.net"
     "http://mymemory.translated.net/s.php"
     "sl=ja-JA&tl=en-GB&q=%s"
     utf-8-dos
     (("<h2>Computer translation</h2>\\(\\(.*\n\\)*.*\\)<h2>" . 1)
      ("</td>[ \t\n]*<td [^>]*>\\(\\(.*\n\\)*.*\\)</td>[ \t\n]*<td " . 1))
     (join-lines))
    ;; Yahoo! BABEL FISH
    ((babelfish en ja)
     "Yahoo! BABEL FISH (powered by SYSTRAN)"
     "http://babelfish.yahoo.com/translate_txt"
     "lp=en_ja&trtext=%s&ei=UTF-8"
     utf-8-dos
     (("<div id=\"result\">\\(<div style=[^>]*>\\([^<]*\\)</div>+\\)</div>" . 2))
     (join-lines
      ;; Remove quote characters or replace them with brackets because BABEL FISH
      ;; cannot correctly treat them at all.
      ("\\(^\\|\\s-\\)\"\\([A-Za-z0-9._-]+\\)\"\\(\\s.\\|[,.]\\|\\s-\\|$\\)" . "\\1\\2\\3")
      ("\\(^\\|\\s-\\)[`']\\([A-Za-z0-9._-]+\\)'\\(\\s.\\|[,.]\\|\\s-\\|$\\)" . "\\1\\2\\3")
      ("\\(^\\|\\s-\\)\"\\(\\([^ \t\n\"]+ \\)*[^ \t\n\"]+\\)\"\\(\\s.\\|[,.]\\|\\s-\\|$\\)" . "\\1[\\2]\\4")
      ("\\(^\\|\\s-\\)[`']\\(\\([^ \t\n']+ \\)*[^ \t\n']+\\)'\\(\\s.\\|[,.]\\|\\s-\\|$\\)" . "\\1[\\2]\\4")))
    ((babelfish ja en)
     "Yahoo! BABEL FISH (powered by SYSTRAN)"
     "http://babelfish.yahoo.com/translate_txt"
     "lp=ja_en&trtext=%s&ei=UTF-8"
     utf-8-dos
     (("<div id=\"result\">\\(<div style=[^>]*>\\([^<]*\\)</div>+\\)</div>" . 2))
     (join-lines))
    )
  "Alist saying how to request the translation and parse the response
to/from each of the translation services. Each element is a list as
\(KEY DESCRIPTION URL REQUEST-FORMAT CODING-SYSTEM FILTERS BEFORE-REPLACES).

KEY is a list of three elements as (SITE FROM TO), which have the same
meanings as the leading three arguments of `trans-ej-string-1'.

DESCRIPTION is a string which displayed with the translation result.

URL is a string which contains hostname and also the path (the value of
`action' attribute in <form> tag).

REQUEST-FORMAT is the query string such as \"var1=foo&var2=bar&text=%s\",
where `%s' represents the position in which the original text will be
inserted.

CODING-SYSTEM is a symbol that the text is encoded/decoded by this encoding.

FILTERS is a list describes how to extract the translation string from HTML
of response. See `trans-ej-decode-response' for details.

BEFORE-REPLACES is a list which describes how to modify original text before
sending in order to be correctly translated. The elements are symbols or
cons cells, and processed in order of them. The symbol `join-lines' means
that sentences split into multiple lines will be connected. If cons cell,
its car and cdr are regular expressions, which are used for the arguments of
`re-search-forward' and `replace-match', respectively.")

(defvar trans-ej-dictionaries-selections-alist
  '((mymemory
     "sj"
     ("Accounting" . "Accounting")
     ("Aerospace" . "Aerospace")
     ("Archeology" . "Archeology")
     ("Architecture" . "architecture")
     ("Art" . "Art")
     ("Astronomy" . "Astronomy")
     ("Automotive Industry" . "Automotive_Industry")
     ("Banking" . "Banking")
     ("Chemical" . "Chemical")
     ("Civil Engineering" . "Civil_Engineering")
     ("Computer Science" . "Computer_Science")
     ("Credit Management" . "Credit_Management")
     ("Culinary" . "Culinary")
     ("Finances" . "Finances")
     ("Forestry" . "Forestry")
     ("General" . "General")
     ("History" . "History")
     ("Insurance" . "Insurance")
     ("Legal and Notarial" . "Legal_and_Notarial")
     ("Literary Translations" . "Literary_Translations")
     ("Marketing" . "Marketing")
     ("Mathematics and Physics" . "Matematics_and_Physics")
     ("Mechanical" . "Mechanical")
     ("Medical" . "Medical")
     ("Music" . "Music")
     ("Nautica" . "Nautica")
     ("Pharmaceuticals" . "Pharmaceuticals")
     ("Religion" . "Religion")
     ("Science" . "Science")
     ("Social Science" . "Social_Science")
     ("Tourism" . "Tourism")
     ("All" . "all")))
  "Alist of the definition of dictionaries name for each Web service.
Each element is a list like (SITE-SYMBOL PARAM-NAME . DICT-ALIST).

SITE-SYMBOL is a symbol representing the Web site, and used as the first
argument of `trans-ej-string-1'.

PARAM-NAME is a string which specifies the parameter name used for HTTP
requests.

DICT-ALIST is an alist of the dictionary names used for HTTP requests.
The keys and values of this alist must be short strings.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trans-ej-encode-url (string coding-system)
  "This is the analogue of `url-hexify-string', but STRING is converted
to CODING-SYSTEM."
  (mapconcat (lambda (byte)
	       (if (memq byte url-unreserved-chars)
		   (char-to-string byte)
		 (format "%%%02x" byte)))
	     (encode-coding-string string coding-system)
	     ""))

(defun trans-ej-decode-response (coding-system filters)
  "Return a new string that is decoded and extracted from URI-encoded
string in current buffer by using FILTERS. FILTERS must be given by
the list of pairs as ((REGEXP1 . SUBEXP1) (REGEXP2 . SUBEXP2) ...),
where REGEXPs and SUBEXPs are used for the arguments of `string-match' and
`match-string', respectively.

First, decode all of the buffer string from the coding system specified by
the first argument. Then, apply FILTERS in order of its elements by performing
the matching for REGEXP in the string and leaving only the matching string
corresponding to SUBEXP. Finally, get rid of the unnecessary tags and decode
the URI-encoded characters."
  (let ((str (decode-coding-string (buffer-string) coding-system)))
    (while (and str filters) ;; apply filters
      (setq str (and (string-match (caar filters) str)
		     (match-string (cdar filters) str)))
      (setq filters (cdr filters)))
    (cond
     (str
      (with-temp-buffer
	(insert str)
	(goto-char (point-min))
	(while (re-search-forward "\n?<br>" nil t)
	  (replace-match "\n")) ;; replace <br> tag by \n
	(goto-char (point-min))
	(while (re-search-forward "<[^>]+>" nil t)
	  (replace-match ""))	;; get rid of other tags
	(goto-char (point-min))
	(while (re-search-forward "&#\\([0-9]+\\);" nil t)
	  (let ((chr (string-to-number (match-string 1))))
	    (replace-match (or (cdr (assq chr
					  '((160 . " ")))) ;; google returns this code. why?
			       (string chr))
			   t t)))
	(goto-char (point-min))
	(while (re-search-forward "&\\([a-z]+\\);" nil t)
	  (let ((replace (cdr (assoc (match-string 1)
				     '(("quot" . "\"")
				       ("amp" . "&")
				       ("lt" . "<")
				       ("gt" . ">"))))))
	    (when replace
	      (replace-match replace))))
	(buffer-string)))
     (trans-ej-decode-response-debug
      (decode-coding-string (buffer-string) coding-system)))))

(defun trans-ej-strip-empty-lines (string)
  "Remove the empty lines at the both ends of STRING."
  (with-temp-buffer
    (insert string)
    (let ((tail (skip-chars-backward "\n")))
      (goto-char (point-min))
      (skip-chars-forward "\n")
      (buffer-substring (point) (+ (point-max) tail)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main part
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trans-ej-make-request-data (string site from to)
  "Return a string to add to the end of URL as the request parameters.

STRING is a source text.

SITE, FROM and TO must be symbols. See `trans-ej-site-profs-alist' for
details. For example, to translate from English to Japanese by using Yahoo!
JAPAN, call as (trans-ej-string-1 \"foo is a bar.\" 'yahoojp 'en 'ja).

Note that this function should be called from the buffer which STRING
is included in, otherwise auto-uncomment mechanism can't correctly work."
  (let ((site-prof (cdr (assoc (list site from to) trans-ej-site-profs-alist))))
    (unless site-prof
      (error "Your desired translation cannot be done. (%s: %s -> %s)"
	     site from to))
    (let ((pos 0))
      (while (setq pos (string-match "^[ \t]*\\s<+" string pos))
	(setq string (replace-match "" t t string)))) ;; auto-uncomment
    (let* ((format-string (nth 2 site-prof))
	   (coding-system (nth 3 site-prof))
	   (before-replaces (nth 5 site-prof))
	   (retval (format format-string
			   (trans-ej-encode-url
			    (with-temp-buffer
			      (insert string)
			      (mapc (lambda (replace)
				      (when (eq replace 'join-lines)
					(setq replace
					      (nth 2 (assq from trans-ej-languages-alist))))
				      (goto-char (point-min))
				      (while (re-search-forward (car replace) nil t)
					(replace-match (cdr replace) t)))
				    before-replaces)
			      (buffer-string))
			    coding-system)))
	   (dicts (cdr (assq site trans-ej-dictionaries-selections-alist))))
      (when dicts
	(let ((dict (cdr (assoc (cdr (assq site
					   trans-ej-selected-dictionaries-alist))
				(cdr dicts)))))
	  (when dict
	    (setq retval (concat retval "&" (car dicts) "=" dict)))))
      retval)))

(defun trans-ej-string-1 (string site from to &optional callback cbargs)
  "Translate STRING between languages specified by FROM and TO.
If optional arguments are given, CALLBACK with CBARGS.

SITE, FROM and TO must be symbols. See `trans-ej-site-profs-alist' for
details. For example, to translate from English to Japanese by using Yahoo!
JAPAN, call as (trans-ej-string-1 \"foo is a bar.\" 'yahoojp 'en 'ja).

If optional arguments are not given, retrieve the response synchronously
and return the translation as a string. Otherwise, CALLBACK is called when
the translation has been retrieved and extracted. It is called as
\(apply CALLBACK STRING STATUS CBARGS). STRING is the translation.
Concerning STATUS, this function is the same as `url-retrieve'.

Note that this function should be called from the buffer which STRING
is included in, otherwise auto-uncomment mechanism can't correctly work."
  (let* ((url-request-data (trans-ej-make-request-data string site from to))
	 (site-prof (cdr (assoc (list site from to) trans-ej-site-profs-alist)))
	 (url (nth 1 site-prof))
	 (url-request-method "POST")
	 (url-request-extra-headers
	  '(("Content-Type" . "application/x-www-form-urlencoded")))
	 (coding-system (nth 3 site-prof))
	 (filters (nth 4 site-prof)))
    (save-excursion
      (if callback
	  (url-retrieve url
			(lambda (status decode coding-system filters callback cbargs)
			  (apply callback
				 (funcall decode coding-system filters)
				 status cbargs))
			(list #'trans-ej-decode-response coding-system filters
			      callback cbargs))
	(with-current-buffer (url-retrieve-synchronously url)
	  (trans-ej-decode-response coding-system filters))))))

;; Translate using multiple Web sites

(define-derived-mode trans-ej-mode fundamental-mode "Trans-EJ"
  "Major mode for Trans-EJ English-Japanese translator."
  (use-local-map button-buffer-map)
  (set (make-local-variable 'truncate-partial-width-windows) nil))

(define-button-type 'trans-ej-browse
  'follow-link t
  'action (lambda (button)
	    (let* ((args (button-get button 'trans-ej-browse-args))
		   (site-prof (cdr (assoc (cdr args) trans-ej-site-profs-alist))))
	      (with-current-buffer (button-get button 'trans-ej-current-buffer)
		(browse-url (concat (nth 1 site-prof)
				    "?"
				    (apply #'trans-ej-make-request-data args))))))
  'help-echo (purecopy "mouse-2, RET: View this Web site in a browser"))

(put (button-category-symbol 'trans-ej-browse) 'face 'link)

(defun trans-ej-result-buffer-setup (from to)
  "Create the buffer, if necessary, which name is specified by
`trans-ej-result-buffer-name' for FROM and TO and display it.
The window is not selected."
  (let ((buf (get-buffer-create
	      (format trans-ej-result-buffer-name from to))))
    (with-current-buffer buf
      (erase-buffer)
      (trans-ej-mode)
      (display-buffer buf))
    buf))

(defvar trans-ej-wait-responses-list nil
  "This variable is internally used for counting the translation
finished. Don't change directly." )

(defun trans-ej-string-all-callback (str status buf beg end msg)
  "This function is internally used for displaying the results of
multiple asynchronous translations. Don't call directly."
  (with-current-buffer buf
    (delete-region beg end)
    (goto-char beg)
    (while status
      (let* ((key (pop status))
	     (val (pop status)))
	(when (eq key :error)
	  (insert (format "%s: %s\n" (car val) (cadr val))))))
    (if str
	(insert (trans-ej-strip-empty-lines str) "\n")
      (insert (propertize "error: Couldn't recognize translation.\n" 'face 'shadow)))
    (set-marker beg nil)
    (set-marker end nil))
  (unless (setq trans-ej-wait-responses-list
		(cdr trans-ej-wait-responses-list))
    (message (concat msg "done"))))

(defun trans-ej-string-all (string from to)
  "Translate STRING between languages specified by FROM and TO.
Request the translation to multiple WEB translation services and perform
asynchronously. The results are inserted into the buffer specified by
`trans-ej-result-buffer-name', which displayed in other window.

The order of the requests to server and displaying in the result buffer
can be specified by `trans-ej-site-orders-alist'."
  (let ((curbuf (current-buffer))
	(buf (trans-ej-result-buffer-setup from to))
	(sites (cdr (assoc (list from to) trans-ej-site-orders-alist)))
	(msg (format "Translating into %s..."
		     (cadr (assq to trans-ej-languages-alist))))
	beg end)
    (setq trans-ej-wait-responses-list sites)
    (mapc (lambda (site)
	    (with-current-buffer buf
	      (insert "●")
	      (let ((site-prof (cdr (assoc (list site from to)
					   trans-ej-site-profs-alist))))
		(if site-prof
		    (insert-text-button (nth 0 site-prof)
					'type 'trans-ej-browse
					'trans-ej-current-buffer curbuf
					'trans-ej-browse-args
					(list string site from to))
		  (insert (propertize "Unknown service" 'face 'shadow))))
	      (insert "\n\n")
	      (setq beg (point-marker))
	      (insert msg "\n")
	      (setq end (point-marker))
	      (insert "\n"))
	    (condition-case err
		;; The following code must be placed on the outside of
		;;`with-current-buffer' in order that the auto-uncomment
		;; mechanism works correctly.
		(trans-ej-string-1 string site from to
				   'trans-ej-string-all-callback
				   (list buf beg end msg))
	      (error (with-current-buffer buf
		       (delete-region beg end)
		       (goto-char beg)
		       (insert (propertize (format "%s: %s\n\n" (car err) (cadr err))
					   'face 'shadow)))
		     (set-marker beg nil)
		     (set-marker end nil)
		     (setq trans-ej-wait-responses-list
			   (cdr trans-ej-wait-responses-list)))))
	  sites)
    (if trans-ej-wait-responses-list
	(message msg)
      (message "Failed to translate"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trans-ej-en2ja-string (string)
  "Translate STRING from English to Japanese."
  (interactive "sString to translate (en => ja): ")
  (trans-ej-string-all string 'en 'ja))

(defun trans-ej-ja2en-string (string)
  "Translate STRING from Japanese to English."
  (interactive "sString to translate (ja => en): ")
  (trans-ej-string-all string 'ja 'en))

(defun trans-ej-string (string)
  "Translate STRING between English and Japanese."
  (interactive "sString to translate: ")
  (funcall
   (if (memq t (mapcar (lambda (c) (> c 255)) string))
       'trans-ej-ja2en-string
     'trans-ej-en2ja-string)
   string))

(defun trans-ej-en2ja-region (start end)
  "Translate string in region from English to Japanese."
  (interactive "r")
  (if (or (not mark-active)
	  (eq start end))
      (error "Region is not selected."))
  (trans-ej-en2ja-string (buffer-substring-no-properties start end)))

(defun trans-ej-ja2en-region (start end)
  "Translate string in region from Japanese to English."
  (interactive "r")
  (if (or (not mark-active)
	  (eq start end))
      (error "Region is not selected."))
  (trans-ej-ja2en-string (buffer-substring-no-properties start end)))

(defun trans-ej-region (start end)
  "Translate string in region between English and Japanese."
  (interactive "r")
  (if (or (not mark-active)
	  (eq start end))
      (error "Region is not selected."))
  (trans-ej-string (buffer-substring-no-properties start end)))

(defun trans-ej-en2ja-buffer (&optional buffer-or-name)
  "Translate the whole of buffer string specified by BUFFER-OR-NAME
from English to Japanese.

Omitting BUFFER-OR-NAME means translate current buffer."
  (interactive "bBuffer to translate (en => ja): ")
  (with-current-buffer (or buffer-or-name (current-buffer))
    (trans-ej-en2ja-string (buffer-string))))

(defun trans-ej-ja2en-buffer (&optional buffer-or-name)
  "Translate the whole of buffer string specified by BUFFER-OR-NAME
from Japanese to English.

Omitting BUFFER-OR-NAME means translate current buffer."
  (interactive "bBuffer to translate (ja => en): ")
  (with-current-buffer (or buffer-or-name (current-buffer))
    (trans-ej-ja2en-string (buffer-string))))

(defun trans-ej-buffer (&optional buffer-or-name)
  "Translate the whole of buffer string specified by BUFFER-OR-NAME
between English and Japanese.

Omitting BUFFER-OR-NAME means translate current buffer."
  (interactive "bBuffer to translate: ")
  (with-current-buffer (or buffer-or-name (current-buffer))
    (trans-ej-string (buffer-string))))

(defun trans-ej-en2ja (&optional string)
  "Translate STRING from English to Japanese.
If mark is active, translate the string in the region."
  (interactive
   (list (if mark-active
	     (buffer-substring-no-properties (point) (mark))
	   (read-string "String to translate (en => ja): "))))
  (trans-ej-en2ja-string string))

(defun trans-ej-ja2en (&optional string)
  "Translate STRING from Japanese to English.
If mark is active, translate the string in the region."
  (interactive
   (list (if mark-active
	     (buffer-substring-no-properties (point) (mark))
	   (read-string "String to translate (ja => en): "))))
  (trans-ej-ja2en-string string))

(defun trans-ej (&optional string)
  "Translate STRING between English and Japanese.
If mark is active, translate the string in the region."
  (interactive
   (list (if mark-active
	     (buffer-substring-no-properties (point) (mark))
	   (read-string "String to translate: "))))
  (trans-ej-string string))

(provide 'trans-ej)

;;;
;;; trans-ej.el ends here
