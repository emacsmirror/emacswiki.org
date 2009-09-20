;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is mon-url-utils.el
;;; ================================================================
;;; DESCRIPTION:
;;; Provides utilities to call urls for data lookup and modify web/internet 
;;; related contents of buffer.
;;;
;;; CONSTANTS:
;;; `*mon-tld-list*'
;;;
;;; VARIABLES:
;;; `*hexcolor-keywords*'
;;; 
;;; ALIASES:
;;; `mon-search-wiki' ->  `mon-search-wikipedia'
;;;
;;; SUBST:
;;; `mon-tld-tld', `mon-tld-name' 
;;;
;;; MACROS:
;;;
;;; FUNCTIONS:►►►
;;; `mon-htmlfontify-buffer-to-firefox',`mon-htmlfontify-region-to-firefox'
;;; `hexcolour-add-to-font-lock', `mon-search-ulan', `mon-search-ulan-for-name',
;;; `mon-search-wikipedia', `mon-search-loc', `mon-search-bnf',
;;; `mon-insert-dbc-link', `mon-insert-dbc-doc-link', `mon-wrap-all-urls',
;;; `mon-wrap-one-url', `mon-wrap-url',`mon-wrap-span', 
;;; `mon-make-html-table-string' 
;;; `mon-make-html-table',`mon-tld-find-tld', `mon-tld-find-name',
;;; `mon-tld',
;;; FUNCTIONS:◄◄◄
;;;
;;; RENAMED: 
;;; `hexcolour-keywords' -> `*hexcolor-keywords*'
;;; `hexcolor-add-to-font-lock' -> `hexcolour-add-to-font-lock'
;;;
;;; RENAMED: Dave Pearson's tld-* 
;;; (URL `http://www.davep.org/emacs/tld.el')
;;; `tld'           -> `mon-tld'
;;; `tld-find-name' -> `mon-tld-find-name'
;;;                 -> `mon-tld-find-tld'
;;; `tld-name'      -> `mon-tld-name'
;;; `tld-tld'       -> `mon-tld-tld'
;;; `tld-list'      -> `*mon-tld-list*'
;;; `tld-list'      -> `*mon-tld-list*'
;;;
;;; MOVED: 
;;; `mon-get-image-dimensions'    -> ./mon-rename-image-utils.el
;;; `mon-get-image-dimensions-im' -> ./mon-rename-image-utils.el
;;; `mon-get-image-md5'           -> ./mon-rename-image-utils.el
;;;
;;; REQUIRES:
;;; `mon-htmlfontify-buffer-to-firefox'  -> ./mon-dir-locals-alist.el <- *ebay-images-temp-path*
;;; `mon-htmlfontify-region-to-firefox'  -> ./mon-dir-locals-alist.el <- *ebay-images-temp-path*
;;; `mon-htmlfontify-buffer-to-firefox'  -> html-fontify.el
;;; `mon-htmlfontify-region-to-firefox'  -> html-fontify.el
;;; `hexcolor-add-to-font-lock'          -> `css-mode-hook' ;when active
;;;  mon-tld-xxx functions               -> cl
;;;
;;; TODO:
;;; Adjust `mon-search-ulan', `mon-search-ulan-for-name' to retrieve url (a)synchronously. 
;;;
;;; NOTES:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; FILE-CREATED:
;;; <Timestamp: Tuesday June 16, 2009 @ 08:39.52 PM - by MON KEY>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Copyright (C) 2009 by MON KEY 
;;; ==============================
;;; CODE:

;;; ==============================
;;; Needed in mon-tld-xxx functions.
(eval-when-compile (require 'cl))

;;; ==============================
(require 'htmlfontify)
;;; ==============================

;;; ==============================
;;; CREATED: <Timestamp: Tuesday June 16, 2009 @ 08:28.49 PM - by MON KEY>
;;; COURTESY: Thierry Volpiatto  HIS: tv-utils.el WAS: `tv-htmlfontify-buffer-to-firefox'
;;; REQUIRES: `htmlfontify.el'
(defun mon-htmlfontify-buffer-to-firefox ()
  "Converts fontified buffer to an html file with Firefox.\n
See also; `mon-htmlfontify-region-to-firefox', `*emacs2html-temp*'."
  (interactive)
  (let ((fname (concat *emacs2html-temp* "/" (symbol-name (gensym "emacs2firefox")) ".html")))
    (htmlfontify-buffer)
    (with-current-buffer (current-buffer)
      (write-file fname))
    (browse-url-firefox (format "file://%s" fname))))

;;; ==============================
;;; CREATED: <Timestamp: Tuesday June 16, 2009 @ 08:28.49 PM - by MON KEY>
;;; COURTESY: Thierry Volpiatto HIS: tv-utils.el WAS: `tv-htmlfontify-region-to-firefox'
;;; REQUIRES: `htmlfontify.el'
(defun mon-htmlfontify-region-to-firefox (beg end)
  "Converts fontified region to an html file with Firefox.\n
See also; `mon-htmlfontify-region-to-firefox', `*emacs2html-temp*'."
  (interactive "r")
  (let ((fname (concat *emacs2html-temp* "/"(symbol-name (gensym "emacs2firefox")) ".html"))
        (buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf beg end)
      (htmlfontify-buffer)
      (write-file fname))
    (browse-url-firefox (format "file://%s" fname))))

;;; ==============================
;;; COURTESY: Xah Lee
;;; (URL `http://xahlee.org/emacs/emacs_html.html')
(defvar *hexcolor-keywords* 'nil
  "Keywords for fontification of hex code color values \(e.g. CSS\).\n
See also; `hexcolor-add-to-font-lock'.")
;;
(when (not (bound-and-true-p *hexcolor-keywords*))
(setq *hexcolor-keywords*
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background 
                     (match-string-no-properties 0))))))))

;;; ==============================
(defun hexcolor-add-to-font-lock ()
  "Add font-lock keywords for hex code color values for fontification.\n
See also; `*hexcolor-keywords*'."
  (font-lock-add-keywords nil *hexcolor-keywords*))
;;
;(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'naf-mode-hook 'hexcolor-add-to-font-lock)

;;; ==============================
;;; NOTE:
;;; Maybe better to use firefox for ULAN - conkeror doesn't scroll well :(
;;; Or, better even, just retrieve url synchronously. 
;;; CREATED: <Timestamp: Friday February 13, 2009 @ 07:01.59 PM - by MON KEY>
(defun mon-search-ulan (&optional uq)
  "Open the ULAN in a browser. When UQ is non-nil search the ULAN artist name. 
When the mark is active search ULAN for name in region. Attempts to rotate
the nameform intelligently to catch one of three different configurations:\n
A\) Lastname \(Firstname\) 
B\) Lastname, Firstname 
C\) Firstname Lastname\n
A, B, and C  are each transformed to ==> Lastname%2C+Fristname and then wrapped 
for ULAN query to \(URL `http://www.getty.edu/vow/ULANFullDisplay?find='\).\n When region
is not active and UQ is nil open a blank ULAN query in browser:\n
\(URL `http://www.getty.edu/research/conducting_research/vocabularies/ulan/'\)\n
`mon-search-ulan-for-name' for an interactive version which automatically
defaults to a prompt for name to search.\n
See also; `mon-search-wikipedia'\nUsed in `naf-mode'."
  (interactive "P")
  (let* ((uqp (if (and uq)
		  t nil))
	 (prompt-url (when (and uqp)
		       (concat "http://www.getty.edu/vow/ULANServlet?english=Y&find="
			       (replace-regexp-in-string ", " "%2C+"
							 (read-string "Artist Name to Query \"Lastname, Firstname\"?: "))
			       "&role=&page=1&nation=")))
	 (region-url (when (and transient-mark-mode mark-active)
		       (buffer-substring-no-properties (region-beginning) (region-end))))
	 (test-region (when (and region-url)
			(cond
			 ((string-match "\\(\\([A-Z][a-z]+\\)\\([: :](\\)\\([A-Z][a-z]+\\)\\()\\)\\)" region-url) 
			  (concat (match-string 2 region-url) "%2C+"  (match-string 4 region-url)))
			 ((string-match "\\(\\([A-Z][a-z]+\\)\\(,[: :]\\)\\([A-Z][a-z]+\\)\\)" region-url)
			  (concat (match-string 2 region-url) "%2C+" (match-string 4 region-url)))
			 ((string-match "\\(\\([A-Z][a-z]+\\)\\([: :]\\)\\([A-Z][a-z]+\\)\\)" region-url)
			  (concat (match-string 4 region-url) "%2C+" (match-string 2 region-url))))))
	 (build-url (when (and test-region)
		      (concat
		       "http://www.getty.edu/vow/ULANServlet?english=Y&find="
		       test-region
		       "&role=&page=1&nation=")))
	 (ulan-url (cond
		    ((and build-url)
		     build-url)
		    ((and prompt-url)
		     prompt-url)
		    ((and (not build-url) (not uqp))
		     "http://www.getty.edu/research/conducting_research/vocabularies/ulan/"))))
    ;;    (browse-url ulan-url)))  ;See NOTE: above.
    (browse-url-firefox ulan-url)))

;;;test-me; Lastname (Firstname)
;;;test-me; Lastname, Firstname
;;;test-me; Firstname Lastname
;;;test-me; Pyle (Howard)
;;;test-me; Pyle, Howard
;;;test-me; Howard Pyle

;;; ==============================
(defun mon-search-ulan-for-name ()
  "Interactive version of `mon-search-ulan'.
Default behavior is to prompt for name to search.\n
See also; `mon-search-wikipedia'.\nUsed in `naf-mode'."
  (interactive)
  (mon-search-ulan t))

;;; ==============================
;;; COURTESY: Xah Lee
;;; (URL `http://xahlee.org/emacs/emacs_lookup_ref.html')
(defun mon-search-wikipedia ()
  "Look up the words in Wikipedia (URL `http//:www.Wikipedia.com')
Generates a url, with active region (a phrase), lookup that phrase
and switches to browser.\n\nUsed in `naf-mode'.
See also; `mon-search-ulan', `mon-search-ulan-for-name',
`mon-search-loc', `mon-search-bnf'."
 (interactive)
 (let (myword myurl)
   (setq myword
         (if (and transient-mark-mode mark-active)
	     (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))
   (setq myword (replace-regexp-in-string " " "_" myword))
   (setq myurl (concat "http://en.wikipedia.org/wiki/" myword))
   (browse-url myurl)))
;;
(defalias 'mon-search-wiki 'mon-search-wikipedia)

;;; ==============================
;;; `mon-search-loc' needs to take arguments to build the search.
;;; SEE: (find-file "../naf-mode/a1-working-notes.el")
(defun mon-search-loc  ()
  "Open the LOC Authority Headings Search Page in the default browser. 
\(URL `http://authorities.loc.gov/cgi-bin/Pwebrecon.cgi?DB=local&PAGE=First')
See also; `mon-search-ulan', `mon-search-ulan-for-name', `mon-search-bnf',
`mon-search-wikipedia'.\nUsed in `naf-mode'."
  (interactive)
  (browse-url "http://authorities.loc.gov/cgi-bin/Pwebrecon.cgi?DB=local&PAGE=First"))

;;; ==============================
;;; `mon-search-bnf' needs to take arguments to build the search.
;;; SEE: (find-file "../naf-mode/a1-working-notes.el")
(defun mon-search-bnf  ()
  "Open the BNF Authority Headings Search Page in the default browser. 
\(URL `http://catalogue.bnf.fr/jsp/recherche_autorites_bnf.jsp?host=catalogue')
See also; `mon-search-ulan', `mon-search-ulan-for-name', `mon-search-loc',
`mon-search-wikipedia'.\nUsed in `naf-mode'."
  (interactive)
  (browse-url "http://catalogue.bnf.fr/jsp/recherche_autorites_bnf.jsp?host=catalogue"))

;;; ==============================
(defun mon-insert-dbc-link ()
  "Insert a vanilla (relative path) href template at point.
Inserts the following:\n
<a class=\"link_green_bold\" href=\"../insert-path-here\" \"> insert-link-text </a>\n
Link will be colored according to to DBC .css for link_gree_bold.\n
See also; `mon-insert-dbc-doc-link' for a pre-formatted href to doc-detail page.
Used in `naf-mode'."
  (interactive)
  (insert "<a class=\"link_green_bold\" href=\"../insert-path-here\" \"> insert-link-text </a>"))

;;; =======================
;;; Default URL generated for Derby City Prints.
(defun mon-insert-dbc-doc-link (&optional doc-num doc-type link-text)
  "Insert a vanilla \(relative path\) href template at point. 
Default url generated for DBC.
Called interactively prompts for:
DOC-NUM (a 3-4 digit number), default is \"####\";
DOC-TYPE (artist, author, brand, book, people), default is \"naf-type\";
LINK-TEXT (text with .css class \"link_green_bold\") default is \" insert link text \".
Inserts:\n
<a class=\"link_green_bold\" href=\"../doc-details-DOC-NUM-DOC-TYPE.htm\" \">LINK-TEXT</a>\n
Link will be coded according to to DBC .css for link_gree_bold.\n
See also; `mon-insert-dbc-link' which inserts a vanilla href link formatted for a
DBC page URL.\nUsed in `naf-mode'."
(interactive "n3-4 digit document number:\nsDoc's NAF type:\nsText for link title:")
  (let* ((dn
	  (if (and doc-num)
	      doc-num
	    "####"))
	 (dt (if (and doc-type)
		 (downcase doc-type)
	       "naf-type"))
	 (lt (if (and link-text)
		 link-text
	       " insert link text "))
	 (dbc-url 
          (format 
           "<a class=\"link_green_bold\" href=\"../doc-details-%d-%s.htm\" \">%s</a>" 
           dn dt lt)))
    (insert  dbc-url)))

;;; ==============================
;;; NOTE: not 100% correct yet because doesn't detect pre-existing wrapped urls
;;; in _some_ buffer locations.
;;; CREATED: <Timestamp: Saturday April 18, 2009 @ 06:51.27 PM - by MON KEY>
(defun mon-wrap-all-urls ()
  "Wraps all URLs in buffer _after point_ with (URL `*').
Conditional prefix matching regexps in `*mon-wrap-url-schemes*' global.
Won't replace recursively on pre-existing wrapped URLs.\n
See also; `thing-at-point-url-at-point', `mon-wrap-url', `mon-wrap-text', 
`mon-wrap-span', `mon-wrap-selection', `mon-wrap-with'."
  (interactive)
  (save-excursion
    ;;(goto-char (point-min)) -not working
    (while
	(if (search-forward-regexp *mon-wrap-url-schemes* nil t)
	    (let* ((bnd-start (car (bounds-of-thing-at-point 'url)))
		   (bnd-pre (- bnd-start 6))
		   (url-targ (thing-at-point-url-at-point))
		   (url-rep (concat "(URL `" url-targ "')")))
	      (cond
	       ((< bnd-pre 0)
		(replace-string url-targ url-rep)
		(skip-syntax-forward "^w"))
	       ((not (string-match-p "(URL `" (buffer-substring bnd-pre bnd-start)))
		(skip-syntax-backward "^-")
		(replace-string url-targ url-rep)
		(skip-syntax-forward "^w"))))))))

;;;test-me;http://www.somethign.xomthing.com/blotto
;;;test-me;ftp://some.site.com
;;;test-me;http://www.somethign.xomthing.com/blamop
;;;test-me;mailto:stan@derbycityprints.com

;;; ==============================
;;; TODO: Add ability evaluate the region programatically and otherwise.
;;; MODIFICATIONS: <Timestamp: Monday June 29, 2009 @ 06:22.48 PM - by MON KEY>
(defun mon-wrap-one-url () ;;(&optional start end insertp)
  "Wrap 1\(one\)the URL  _after point_ with (URL `*').
Conditional prefix matching regexps in `*mon-wrap-url-schemes*' global.
Won't replace recursively on pre-existing wrapped URLs.\n
See also; `mon-wrap-all-urls', `thing-at-point-url-at-point',
`mon-wrap-url', `mon-wrap-text', `mon-wrap-span', `mon-wrap-with'
`mon-wrap-selection'."
  (interactive)
  (save-excursion
    (let* ((url-bnds (bounds-of-thing-at-point 'url))
           (bnd-start (car url-bnds))
           (bnd-end (cdr url-bnds))
           (rep-url))
      (setq rep-url (concat 
                     "(URL `" 
                     (buffer-substring-no-properties bnd-start bnd-end)
                     "')"))
      (goto-char bnd-start)
      (delete-region bnd-start bnd-end)
      (insert rep-url))))

;;;test-me;http://www.somethign.xomthing.com/blotto
;;;test-me;ftp://some.site.com
;;;test-me;http://www.somethign.xomthing.com/blamop
;;;test-me;mailto:stan@derbycityprints.com

;;; ==============================
;;; COURTESY: Xah Lee
(defun mon-wrap-url ()
  "Make thing at cursor point into an HTML link.\n
EXAMPLE:\n http://wikipedia.org\nbecomes:\n
<a href=\"http://en.wikipedia.org/\">http://wikipedia.org/</a>\n
Or:\n'test' becomes <a href=\"test\">test</a>\n
See also; `mon-wrap-all-urls', `mon-wrap-one-url', `*mon-wrap-url-schemes*',
`thing-at-point-url-at-point',`mon-wrap-text', `mon-wrap-span',
`mon-wrap-selection', `mon-wrap-with'."
  (interactive)
  (re-search-backward "[\n\t ()]" nil t)
  (looking-at "[\n\t ()]?\\([^\n\t ()]+\\)")
  (let (
        (p1 (match-beginning 1))
        (p2 (match-end 1))
        (url (match-string 1))
        )
    (delete-region p1 p2)
    (goto-char p1)
    (insert "<a href=\"" url "\">" url "</a>" )))

;;; http://sosof.org/
;;; => <a href="http://sosof.org/">http://sosof.org/</a>

;;; `mon-wrap-text' is defined in mon-utils.el
;;; ==============================
;;; COURTESY: Xah Lee
(defun mon-wrap-span ()
  "Wrap a HTML <span> tag around current word or region.
Uses DBC's link_green_bold CSS.\n\nEXAMPLE:
.link_green_bold {
	font-family: Verdana, Arial, Helvetica, sans-serif;
	font-size: 9px;
	color: #5A8F5D;
	font-weight:bold;
}\n
See also; `mon-wrap-selection', `mon-wrap-text', `mon-wrap-with',
`mon-wrap-url', `mon-wrap-all-urls'."
  (interactive)
  (mon-wrap-text "<span class=\"link_green_bold\">" "</span>"))

;;; ==============================
;; (add-hook 'html-mode-hook
;; (lambda ()
;; (define-key html-mode-map "\M-5" 'wrap-url)))
;;  (global-set-key (kbd "<f6>") 'wrap-span-xnt)
;;; ==============================

;;; ==============================
(defun mon-make-html-table-string (textblock delim)
  "Turn a text string into an HTML table. 
Helper function for `mon-make-html-table' which see."
  (let ()
    (setq textblock (replace-regexp-in-string delim "</td><td>" textblock))
    (setq textblock (replace-regexp-in-string "\n" "</td></tr>\n<tr><td>" textblock))
    (setq textblock (substring textblock 0 -8)) ;; delet the beginning "<tr><td>" in last line
    (concat "<table border=\"1\" cellpadding=\"5\" cellspacing=\"0\">\n<tr><td>" textblock "</table>")
    ))

;;; ==============================
(defun mon-make-html-table (sep)
  "Turn the current paragraph into a HTML table.
Where \"current paragraph\" has empty lines before and after the block of 
text after point.\n
EXAMPLE:
With \"-\" as separator transforms this:\n
a-b-c\n  1-2-3\n  this-and-that\n
Into the following html table:\n
<table border=\"1\" cellpadding=\"5\" cellspacing=\"0\">
 <tr><td>a</td><td>b</td><td>c</td></tr>
 <tr><td>1</td><td>2</td><td>3</td></tr>
 <tr><td>this</td><td>and</td><td>that</td></tr>\n </table>\n
See also; `mon-make-html-table-string'."
  (interactive "sEnter string pattern for column separation:")
  (let (bds p1 p2 myStr)
    (setq bds (bounds-of-thing-at-point 'paragraph))
    (setq p1 (+ (car bds) 1))
    (setq p2 (cdr bds))
    (setq myStr (buffer-substring-no-properties p1 p2))
    (delete-region p1 p2)
    (insert (mon-make-html-table-string myStr sep) "\n")))

;;; ==============================
;;; NOTE: This is buggy on the w32 paths.
;;; USE: wget.el instead. (locate-library "wget") 
;;; CREATED: <Timestamp: Tuesday June 30, 2009 @ 02:30.21 PM - by MON KEY>
(defun mon-fetch-rfc (rfc-num)
"Fetches an RFC with RFC-NUM with wget.
NOTE: This is buggy with w32 paths."
(interactive "sRFC number :")
  (let* ((the-rfc rfc-num)
         (fetch-from (format 
                      "http://tools.ietf.org/rfc/rfc%s.txt" the-rfc)))
    (shell-command  (format "wget %s" fetch-from))))

;;;test-me;(mon-fetch-rfc 2616)

;;; ==============================
;;; COURTESY: Stefan Reichoer <stefan@xsteve.at> HIS: .emacs 
;;; ==============================
;; ;;; wget
;; (add-site-lisp-load-path "wget/")
;; (autoload 'wget "wget" "wget interface for Emacs." t)
;; (autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
;; (load "w3m-wget" t)

;; ;;The file is downloaded to the folder wget-download-directory (= ~/download)
;; (defun wget-open-downloaded-file ()
;;   (let* ((dir  (cdr (assoc proc wget-process-dir-alist)))
;;          (file (or (cdr (assoc proc wget-process-saved-alist))
;;                    (wget-process-file-name proc)))
;;          (full-file-name (expand-file-name file dir)))
;;     (message "downloaded %s" full-file-name)
;;     (find-file full-file-name)))

;; ;; Open the file after the download
;; (add-hook 'wget-after-hook 'wget-open-downloaded-file)
;;; ==============================
;;; ==============================

;;; ==============================
;;; COURTESY: Dave Pearson <davep@davep.org> HIS: tld.el 
;;; (URL `http://www.davep.org/emacs/tld.el')
;;; VERSION: $Revision: 1.5 $ - Copyright 2000-2008 - GPLv2
;;; TLD -> Top Level Domain names
;;; A TLD lookup tool tld.el provides a command for looking up TLDs, either by searching for a
;;; specific TLD or by searching country names. One command is provided: `tld'.
;;;
;;; NOTE: that, to some degree, this code duplicates the functionality
;;; provided by `what-domain' (a command that is part of emacs). tld.el
;;; differs slightly in that it allows for both TLD and country name
;;; searches. Also, compared to emacs 20.7, the list of TLDs is more complete
;;; (autoload 'tld "tld" "Perform a TLD lookup" t)
;;; ==============================
;;; CREATED: <Timestamp: Tuesday May 19, 2009 @ 12:11.29 PM - by MON KEY>
;;; MODIFICATIONS: altered the original tld alist keys to reflect the those of
;;; mail-extr.el (e.g. notes pertaining to `what-doman')
;;; FROM: mail-extr.el
;;; Keep in mind that the country abbreviations follow ISO-3166.  There is
;;; a U.S. FIPS that specifies a different set of two-letter country
;;; abbreviations. Updated by the RIPE Network Coordination Centre.
;;; Source: ISO 3166 Maintenance Agency - Latest change: 2007/11/15
;;; (URL `http://www.iso.org/iso/en/prods-services/iso3166ma/02iso-3166-code-lists/list-en1-semic.txt')
;;; (URL `http://www.iana.org/domain-names.htm')
;;; (URL `http://www.iana.org/cctld/cctld-whois.htm')
;;; ==============================
;;; WAS: `tld-list' -> `*mon-tld-list*'
(defconst *mon-tld-list*
  '(;; ISO 3166 codes:
    ("AC" . "Ascension Island")  
    ("AD" . "Andorra")
    ("AE" . "United Arab Emirates")
    ("AF" . "Afghanistan")
    ("AG" . "Antigua and Barbuda")
    ("AI" . "Anguilla")
    ("AL" . "Albania")
    ("AM" . "Armenia")
    ("AN" . "Netherlands Antilles")
    ("AO" . "Angola")
    ("AQ" . "Antarctica")		;Continent
    ("AR" . "Argentina")		;"Argentine Republic"
    ("AS" . "American Samoa")
    ("AT" . "Austria")			;"The Republic of %s"
    ("AU" . "Australia")
    ("AW" . "Aruba")
    ("AX" . "Aland Islands")
    ("AZ" . "Azerbaijan")
    ("BA" . "Bosnia-Herzegovina")
    ("BB" . "Barbados")
    ("BD" . "Bangladesh")
    ("BE" . "Belgium")			;"The Kingdom of %s"
    ("BF" . "Burkina Faso")
    ("BG" . "Bulgaria")
    ("BH" . "Bahrain")
    ("BI" . "Burundi")
    ("BJ" . "Benin")
    ("BL" . "Saint Barthelemy")
    ("BM" . "Bermuda")
    ("BN" . "Brunei Darussalam")
    ("BO" . "Bolivia")			;"Republic of %s"
    ("BR" . "Brazil")			;"The Federative Republic of %s"
    ("BS" . "Bahamas")
    ("BT" . "Bhutan")
    ("BV" . "Bouvet Island")
    ("BW" . "Botswana")
    ("BY" . "Belarus")
    ("BZ" . "Belize")
    ("CA" . "Canada")
    ("CC" . "Cocos (Keeling) Islands")
    ("CD" . "Congo")			;"The Democratic Republic of the %s"
    ("CF" . "Central African Republic")
    ("CG" . "Congo")
    ("CH" . "Switzerland")		;"The Swiss Confederation"
    ("CI" . "Ivory Coast")		;Cote D'ivoire
    ("CK" . "Cook Islands")
    ("CL" . "Chile")			;"The Republic of %s"
    ("CM" . "Cameroon")			;In .fr domain
    ("CN" . "China")			;"The People's Republic of %s"
    ("CO" . "Colombia")
    ("CR" . "Costa Rica")		;"The Republic of %s"
    ("CU" . "Cuba")
    ("CV" . "Cape Verde")
    ("CX" . "Christmas Island")
    ("CY" . "Cyprus")
    ("CZ" . "Czech Republic")
    ("DE" . "Germany")
    ("DJ" . "Djibouti")
    ("DK" . "Denmark")
    ("DM" . "Dominica")
    ("DO" . "Dominican Republic")	;"The %s"
    ("DZ" . "Algeria")
    ("EC" . "Ecuador")			;"The Republic of %s"
    ("EE" . "Estonia")
    ("EG" . "Egypt")			;"The Arab Republic of %s"
    ("EH" . "Western Sahara")
    ("ER" . "Eritrea")
    ("ES" . "Spain")			;"The Kingdom of %s"
    ("ET" . "Ethiopia")
    ("EU" . "European Union")
    ("FI" . "Finland")			;"The Republic of %s"
    ("FJ" . "Fiji")
    ("FK" . "Falkland Islands (Malvinas)")
    ("FM" . "Micronesia")		;"Federated States of %s"
    ("FO" . "Faroe Islands")
    ("FR" . "France")
    ("GA" . "Gabon")
    ("GB" . "United Kingdom")
    ("GD" . "Grenada")
    ("GE" . "Georgia")
    ("GF" . "French Guiana")
    ("GG" . "Guernsey")
    ("GH" . "Ghana")
    ("GI" . "Gibraltar")
    ("GL" . "Greenland")
    ("GM" . "Gambia")
    ("GN" . "Guinea")
    ("GP" . "Guadeloupe (Fr.)")
    ("GQ" . "Equatorial Guinea")
    ("GR" . "Greece")			;"The Hellenic Republic (%s)"
    ("GS" . "South Georgia and The South Sandwich Islands")
    ("GT" . "Guatemala")
    ("GU" . "Guam (U.S.)")
    ("GW" . "Guinea-Bissau")
    ("GY" . "Guyana")
    ("HK" . "Hong Kong")
    ("HM" . "Heard Island and Mcdonald Islands")
    ("HN" . "Honduras")
    ("HR" . "Croatia")			;"Croatia (Hrvatska)"
    ("HT" . "Haiti")
    ("HU" . "Hungary")			;"The Hungarian Republic"
    ("ID" . "Indonesia")
    ("IE" . "Ireland")
    ("IL" . "Israel")		    ;"The State of %s"
    ("IM" . "Isle of Man")	    ;"The %s") ; NOT in ISO 3166-1 of 2001-02-26
    ("IN" . "India")		    ;"The Republic of %s"
    ("IO" . "British Indian Ocean Territory")
    ("IQ" . "Iraq")
    ("IR" . "Iran")			;"Islamic Republic of %s"
    ("IS" . "Iceland")			;"The Republic of %s"
    ("IT" . "Italy")			;"The Italian Republic"
    ("JE" . "Jersey")
    ("JM" . "Jamaica")
    ("JO" . "Jordan")
    ("JP" . "Japan")
    ("KE" . "Kenya")
    ("KG" . "Kyrgyzstan")
    ("KH" . "Cambodia")
    ("KI" . "Kiribati")
    ("KM" . "Comoros")
    ("KN" . "Saint Kitts and Nevis")
    ("KP" . "Korea (North)")	       ;"Democratic People's Republic of Korea"
    ("KR" . "Korea (South)")	       ;"Republic of Korea"
    ("KW" . "Kuwait")
    ("KY" . "Cayman Islands")
    ("KZ" . "Kazakhstan")
    ("LA" . "Lao People's Democratic Republic")
    ("LB" . "Lebanon")
    ("LC" . "Saint Lucia")
    ("LI" . "Liechtenstein")
    ("LK" . "Sri Lanka")	     ;"The Democratic Socialist Republic of %s"
    ("LR" . "Liberia")
    ("LS" . "Lesotho")
    ("LT" . "Lithuania")
    ("LU" . "Luxembourg")
    ("LV" . "Latvia")
    ("LY" . "Libyan Arab Jamahiriya")
    ("MA" . "Morocco")
    ("MC" . "Monaco")
    ("MD" . "Moldova")			;"The Republic of %s"
    ("ME" . "Montenegro")
    ("MF" . "Saint Martin (French part)")
    ("MG" . "Madagascar")
    ("MH" . "Marshall Islands")
    ("MK" . "Macedonia")		;"The Former Yugoslav Republic of %s"
    ("ML" . "Mali")
    ("MM" . "Myanmar")
    ("MN" . "Mongolia")
    ("MO" . "Macao")
    ("MP" . "Northern Mariana Islands")
    ("MQ" . "Martinique")
    ("MR" . "Mauritania")
    ("MS" . "Montserrat")
    ("MT" . "Malta")
    ("MU" . "Mauritius")
    ("MV" . "Maldives")
    ("MW" . "Malawi")
    ("MX" . "Mexico")			;"The United Mexican States"
    ("MY" . "Malaysia")
    ("MZ" . "Mozambique")
    ("NA" . "Namibia")
    ("NC" . "New Caledonia (Fr.)")
    ("NE" . "Niger")			;In .fr domain
    ("NF" . "Norfolk Island")
    ("NG" . "Nigeria")
    ("NI" . "Nicaragua")		;"The Republic of %s"
    ("NL" . "Netherlands")		;"The Kingdom of the %s"
    ("NO" . "Norway")			;"The Kingdom of %s"
    ("NP" . "Nepal")			; Via .in domain
    ("NR" . "Nauru")
    ("NU" . "Niue")
    ("NZ" . "New Zealand")
    ("OM" . "Oman")
    ("PA" . "Panama")
    ("PE" . "Peru")
    ("PF" . "French Polynesia")
    ("PG" . "Papua New Guinea")
    ("PH" . "Philippines")		;"The Republic of the %s"
    ("PK" . "Pakistan")
    ("PL" . "Poland")
    ("PM" . "Saint Pierre and Miquelon")
    ("PN" . "Pitcairn")
    ("PR" . "Puerto Rico (U.S.)")
    ("PS" . "Palestinian Territory, Occupied")
    ("PT" . "Portugal")			;"The Portuguese Republic"
    ("PW" . "Palau")
    ("PY" . "Paraguay")
    ("QA" . "Qatar")
    ("RE" . "Reunion (Fr.)")		;In .fr domain
    ("RO" . "Romania")
    ("RS" . "Serbia")
    ("RU" . "Russia")			;"Russian Federation"
    ("RW" . "Rwanda")
    ("SA" . "Saudi Arabia")
    ("SB" . "Solomon Islands")
    ("SC" . "Seychelles")
    ("SD" . "Sudan")
    ("SE" . "Sweden")			;"The Kingdom of %s"
    ("SG" . "Singapore")		;"The Republic of %s"
    ("SH" . "Saint Helena")
    ("SI" . "Slovenia")
    ("SJ" . "Svalbard and Jan Mayen")	;In .no domain
    ("SK" . "Slovakia")			;"The Slovak Republic"
    ("SL" . "Sierra Leone")
    ("SM" . "San Marino")
    ("SN" . "Senegal")
    ("SO" . "Somalia")
    ("SR" . "Suriname")
    ("ST" . "Sao Tome and Principe")
    ("SU" . "U.S.S.R.")		     ;"The Union of Soviet Socialist Republics"
    ("SV" . "El Salvador")
    ("SY" . "Syrian Arab Republic")
    ("SZ" . "Swaziland")
    ("TC" . "Turks and Caicos Islands")
    ("TD" . "Chad")
    ("TF" . "French Southern Territories")
    ("TG" . "Togo")
    ("TH" . "Thailand")			;"The Kingdom of %s"
    ("TJ" . "Tajikistan")
    ("TK" . "Tokelau")
    ("TL" . "East Timor")
    ("TM" . "Turkmenistan")
    ("TN" . "Tunisia")
    ("TO" . "Tonga")
    ("TP" . "East Timor")
    ("TR" . "Turkey")			;"The Republic of %s"
    ("TT" . "Trinidad and Tobago")
    ("TV" . "Tuvalu")
    ("TW" . "Taiwan")			;"%s, Province of China"
    ("TZ" . "Tanzania"	)		;"United Republic of %s"
    ("UA" . "Ukraine")
    ("UG" . "Uganda")
    ("UK" . "United Kingdom") ;	"The %s of Great Britain and Northern Ireland"
    ("UM" . "United States Minor Outlying Islands")
    ("US" . "United States")		;"The %s of America"
    ("UY" . "Uruguay")			;"The Eastern Republic of %s"
    ("UZ" . "Uzbekistan")
    ("VA" . "Holy See (Vatican City State)")
    ("VC" . "Saint Vincent and the Grenadines")
    ("VE" . "Venezuela")		;"The Republic of %s"
    ("VG" . "Virgin Islands, British")
    ("VI" . "Virgin Islands, U.S.")
    ("VN" . "Vietnam")
    ("VU" . "Vanuatu")
    ("WF" . "Wallis and Futuna")
    ("WS" . "Samoa")
    ("YE" . "Yemen")
    ("YT" . "Mayotte")
    ("YU" . "Yugoslavia")		;"Yugoslavia, AKA Serbia-Montenegro"
    ("ZA" . "South Africa")		;"The Republic of %s"
    ("ZM" . "Zambia")
    ("ZW" . "Zimbabwe")			;"Republic of %s"
    ;; GENERIC DOMAINS:
    ;;("bitnet" t		"Because It's Time NET"
    ;;("nato" t		"North Atlantic Treaty Organization"
    ("AERO" . "Air Transport Industry")
    ("ASIA" . "Pan-Asia and Asia Pacific community")
    ("BIZ" . "Businesses")
    ("CAT" . "Catalan language and culture")
    ("COM" . "Commercial")
    ("COOP" . "Cooperative Associations")
    ("INFO" . "Info")
    ("JOBS" . "Employment")
    ("MOBI" . "Mobile products")
    ("MUSEUM" . "Museums")
    ("NAME" . "Individuals")
    ("NET" . "Network")
    ("ORG" . "Non-profit Organization")
    ("PRO" . "Credentialed professionals")
    ("TEL" . "Contact data")
    ("TRAVEL" . "Travel industry")
    ("GOV" . "United States Government")
    ("EDU" . "Educational")
    ("MIL" . "United States Military")
    ("INT" . "International Treaties")
    ("UUCP" . "Unix to Unix CoPy")
    ("ARPA" . "Advanced Research Projects Agency (U.S. DoD)"))
  "Association list of TLDs per ISO-3166 codes.
NOTE: Country abbreviations are per ISO 3166 spec. There is an U.S. FIPS that
specifies a different set of two-letter country abbreviations.
Updated by the RIPE Network Coordination Centre. 
SOURCE: ISO 3166 Maintenance Agency - LATEST-CHANGE: 2007-11-15.
\(URL `http://www.iso.org/iso/en/prods-services/iso3166ma/02iso-3166-code-lists/list-en1-semic.txt');
\(URL `http://www.iana.org/domain-names.htm');
\(URL `http://www.iana.org/cctld/cctld-whois.htm').\n
See also; `mon-tld-find-name', `mon-tld-tld', `mon-tld-find-tld', `mon-tld-name', `mon-tld'.")
;;
;;; WAS: `tld-tld' -> `mon-tld-tld'
(defsubst mon-tld-tld (tld)
  "Return the TLD portion of a TLD pair.\n
See also; `mon-tld-find-name',`mon-tld-find-tld', `mon-tld-name',
`mon-tld',`*mon-tld-list*'."
  (car tld))
;;; WAS: `tld-name' -> `mon-tld-name'
(defsubst mon-tld-name (tld)
  "Return the name portion of a TLD pair.\n
See also; `mon-tld-find-name', `mon-tld-tld', `mon-tld-find-tld',
`mon-tld',`*mon-tld-list*'."
  (cdr tld))
;;
;;; WAS: `mon-tld-find-tld'
(defun mon-tld-find-tld (tld)
  "Lookup a TLD. If found a (TLD . NAME) pair is returned.\n
See also; `mon-tld-find-name', `mon-tld-tld',`mon-tld-name', `mon-tld',
`*mon-tld-list*'.."
  (assoc (upcase tld) *mon-tld-list*))
;;
;;; WAS: `tld-find-name' -> `mon-tld-find-name'
(defun mon-tld-find-name (name)
  "Lookup a TLD name. Returns a list of hits.\n
See also; `mon-tld-tld', `mon-tld-find-tld', `mon-tld-name', `mon-tld', 
,`*mon-tld-list*'."
  (let ((case-fold-search t))
    (loop for tld in mon-tld-list
          when (string-match name (mon-tld-name tld))
          collect tld)))
;;
;;; WAS: `tld' -> `mon-tld'
;;;###autoload
(defun mon-tld (search)
  "Search the TLD list.\n
See also; `mon-tld-find-name', `mon-tld-tld', `mon-tld-find-tld', `mon-tld-name',
`*mon-tld-list*'."
  (interactive "sSearch: ")
  (let* ((tld-lookup (string= (substring search 0 1) "."))
         (result     (if tld-lookup (mon-tld-find-tld (substring search 1)) (mon-tld-find-name search))))
    (if result
        (flet ((message-tld (tld)
                 (message "%s is %s" (mon-tld-tld tld) (mon-tld-name tld))))
          (if tld-lookup
              (message-tld result)
            (if (= (length result) 1)
                (message-tld (car result))
              (with-output-to-temp-buffer "*tld*"
                (princ "TLD    Name\n====== ========================================\n\n")
                (loop for tld in result
                      do (princ (format "%-6s %s\n" (mon-tld-tld tld) (mon-tld-name tld))))))))
      ;; If nothing was found and it wasn't a tld-lookup but it looks like
      ;; it might be a TLD re-submit it with a leading dot.
      (if (and (not tld-lookup) (< (length search) 7))
          (mon-tld (concat "." search))
        (error "No TLD match found")))))

;;; ==============================
(provide 'mon-url-utils)
;;; ==============================

;;; ==============================
;;; mon-url-utils.el ends here
;;; EOF
