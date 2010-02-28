;;; mediawiki.el --- mediawiki frontend

;; Copyright (C) 2008, 2009, 2010 Mark A. Hershberger

;; Original Authors: Jerry <unidevel@yahoo.com.cn>, Chong Yidong <cyd at stupidchicken com> for wikipedia.el
;; Author: Mark A. Hershberger <mah@everybody.org>
;; Version: 2.0
;; Created: Sep 17 2004
;; Keywords: mediawiki wikipedia network wiki
;; URL: http://launchpad.net/mediawiki-el
;; Last Modified: <2010-02-27 16:20:41 mah>

(defconst mediawiki-version "2.0"
  "Current version of mediawiki.el")

;; This file is NOT (yet) part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This version of mediawiki.el represents a merging of
;; wikipedia-mode.el (maintained by Uwe Brauer <oub at mat.ucm.es>)
;; from http://www.emacswiki.org/emacs/wikipedia-mode.el for its
;; font-lock code, menu, draft mode, replying and convenience
;; functions to produce mediawiki.el 2.0.

;;; Installation

;; If you use ELPA (http://tromey.com/elpa), you can install via the
;; M-x package-list-packages interface. This is preferrable as you
;; will have access to updates automatically.

;; Otherwise, just make sure this file is in your load-path (usually
;; ~/.emacs.d is included) and put (require 'mediawiki.el) in your
;; ~/.emacs or ~/.emacs.d/init.el file.

;;; Howto:
;;  M-x customize-group RET mediawiki RET
;;  *dink* *dink*
;;  M-x mediawiki-site RET Wikipedia RET
;;
;; Open a wiki file:    M-x mediawiki-open
;; Save a wiki buffer:  C-x C-s
;; Save a wiki buffer with a different name:  C-x C-w

;;; TODO:
;;  * Optionally use org-mode formatting for editing and translate
;;    that to mw
;;  * Move url-* methods to url-http
;;  * Use the MW API to support searching, etc.
;;  * Clean up and thoroughly test imported wikimedia.el code
;;  * Improve language support.  Currently there is a toggle for
;;    English or German.  This should probably just be replaced with
;;    customizable words given MediaWiki's wide language support.

;;; History

;; From the News section of wikipedia.el comes this bit, kept here for
;; reference later.
;;     (4) "Draft", "send" and "reply" (for discussion pages)
;;         abilities `based' on ideas of John Wigleys remember.el: see
;;         the functions wikipedia-draft-*
;;         RATIONALE: This comes handy in 2 situations
;;            1. You are editing articles which various authors (this I
;;               think is the usual case), you then want not to submit
;;               your edit immediately but want to copy it somewhere and
;;               to continue later. You can use the following functions
;;               for doing that:
;;               wikipedia-draft-buffer \C-c\C-b
;;               wikipedia-draft-region \C-c\C-r
;;               then the buffer/region will be appended to the
;;               wikipedia-draft-data-file (default is
;;               "~/Wiki/discussions/draft.wiki", which you can visit via
;;               wikipedia-draft-view-draft) and it will be
;;               surrounded by the ^L marks in order to set a page.
;;               moreover on top on that a section header == will be
;;               inserted, which consists of the Word Draft, a subject
;;               you are asked for and a date stamp.
;;
;;               Another possibility consists in using the function
;;               wikipedia-draft, bound to \C-c \C-m then a new buffer
;;               will opened already in wikipedia mode. You edit and then
;;               either can send the content of the buffer to the
;;               wikipedia-draft-data-file in the same manner as
;;               described above using the function
;;               wikipedia-draft-buffer (bound to \C-c\C-k)
;;
;;               BACK: In order to copy/send the content of temporary
;;               buffer or of a page in the wikipedia-draft-data-file
;;               back in to your wikipedia file, use the function
;;               wikipedia-send-draft-to-mozex bound to "\C-c\C-c". You
;;               will be asked to which buffer to copy your text!
;;
;;
;;            2. You want to reply  in a discussion page to a specific
;;               contribution, you can use either the function
;;
;;               \\[wikipedia-reply-at-point-simple] bound to [(meta shift r)]
;;               which inserts a newline, a hline, and the signature of
;;               the author. Or can use
;;               \\[wikipedia-draft-reply] bound  [(meta r)]
;;               which does the same as wikipedia-reply-at-point-simple
;;               but in a temporary draft buffer.
;;
;;               BACK: In order to copy/send the content of that buffer
;;               back in to your wikipedia file, use the function
;;               \\[wikipedia-send-draft-to-mozex] bound to "\C-c\C-c". You
;;               will be asked to which buffer to copy your text! If
;;               you want a copy to be send to your draft file, use
;;               the variable  wikipedia-draft-send-archive
;;

;;; Code:

(require 'url-http)
(require 'mml)
(require 'mm-url)

(when (not (fboundp 'url-user-for-url))
  (require 'url-parse)
  (defmacro url-bit-for-url (method lookfor url)
    (when (fboundp 'auth-source-user-or-password)
      `(let* ((urlobj (url-generic-parse-url url))
              (bit (funcall ,method urlobj))
              (methods (list 'url-recreate-url
                             'url-host)))
         (while (and (not bit) (> (length methods) 0))
           (setq bit
                 (auth-source-user-or-password
                  ,lookfor (funcall (pop methods) urlobj) (url-type urlobj))))
         bit)))

  (defun url-user-for-url (url)
    "Attempt to use .authinfo to find a user for this URL."
    (url-bit-for-url 'url-user "login" url))

  (defun url-password-for-url (url)
    "Attempt to use .authinfo to find a password for this URL."
    (url-bit-for-url 'url-password "password" url)))

(when (not (fboundp 'url-hexify-string))
  (defconst url-unreserved-chars '( ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j
    ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z ?A ?B ?C ?D
    ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X
    ?Y ?Z ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?- ?_ ?. ?! ?~ ?* ?' ?\(
    ?\))
    "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from RFC 2396.")

  (defun url-hexify-string (string)
  "Return a new string that is STRING URI-encoded.
First, STRING is converted to utf-8, if necessary.  Then, for each
character in the utf-8 string, those found in `url-unreserved-chars'
are left as-is, all others are represented as a three-character
string: \"%\" followed by two lowercase hex digits."
  (mapconcat (lambda (byte)
               (if (memq byte url-unreserved-chars)
                   (char-to-string byte)
                 (format "%%%02x" byte)))
             (string-to-list string)
             "")))

(defvar url-http-get-post-process 'url-http-response-post-process)
(defun url-http-get (url &optional headers bufname callback cbargs)
  "Convenience method to use method 'GET' to retrieve URL"
  (let* ((url-request-extra-headers (if headers headers
                                      (if url-request-extra-headers
                                          url-request-extra-headers
                                        (cons nil nil))))
         (url-request-method "GET"))

    (when (url-basic-auth url)
      (add-to-list 'url-request-extra-headers
                   (cons "Authorization" (url-basic-auth url))))

    (if callback
        (url-retrieve url url-http-get-post-process
                      (list bufname callback cbargs))
      (with-current-buffer (url-retrieve-synchronously url)
        (funcall url-http-get-post-process bufname)))))

(defvar url-http-post-post-process 'url-http-response-post-process)
(defun url-http-post (url parameters &optional multipart headers bufname
                          callback cbargs)
  "Convenience method to use method 'POST' to retrieve URL"

  (let* ((url-request-extra-headers
          (if headers headers
            (if url-request-extra-headers url-request-extra-headers
              (cons nil nil))))
         (boundary (int-to-string (random)))
         (cs 'utf-8)
         (content-type
          (if multipart
              (concat "multipart/form-data, boundary=" boundary)
            (format "application/x-www-form-urlencoded; charset=%s" cs)))
         (url-request-method "POST")
         (url-request-coding-system cs)
         (url-request-data
          (if multipart
              (mm-url-encode-multipart-form-data
               parameters boundary)
            (mm-url-encode-www-form-urlencoded parameters))))
    (mapc
     (lambda (pair)
       (let ((key (car pair))
             (val (cdr pair)))
         (if (assoc key url-request-extra-headers)
             (setcdr (assoc key url-request-extra-headers) val)
           (add-to-list 'url-request-extra-headers
                        (cons key val)))))
     (list
      (cons "Connection" "close")
      (cons "Content-Type" content-type)))

    (message "Posting to: %s" url)
    (if callback
        (url-retrieve url url-http-post-post-process
                      (list bufname callback cbargs))
      (with-current-buffer (url-retrieve-synchronously url)
        (funcall url-http-post-post-process bufname)))))

(defun url-http-response-post-process (status &optional bufname
                                              callback cbargs)
  "Post process on HTTP response."
  (let ((kill-this-buffer (current-buffer)))
    (when (and (integerp status) (not (< status 300)))
      (kill-buffer kill-this-buffer)
      (error "Oops! Invalid status: %d" status))

    (when (not url-http-end-of-headers)
      (kill-buffer kill-this-buffer)
      (error "Oops! Don't see end of headers!"))

    (goto-char url-http-end-of-headers)
    (forward-line)

    (let ((str (decode-coding-string
                (buffer-substring-no-properties (point) (point-max))
                'utf-8)))
      (kill-buffer (current-buffer))
      (when bufname
        (set-buffer bufname)
        (insert str)
        (goto-char (point-min))
        (set-buffer-modified-p nil))
      (when callback
        (apply callback (list str cbargs)))
      (when (not (or callback bufname))
        str))))

(defun mm-url-encode-multipart-form-data (pairs &optional boundary)
  "Return PAIRS encoded in multipart/form-data."
  ;; RFC1867

  ;; Get a good boundary
  (unless boundary
    (setq boundary (mml-compute-boundary '())))

  (concat

   ;; Start with the boundary
   "--" boundary "\r\n"

   ;; Create name value pairs
   (mapconcat

    ;; Delete any returned items that are empty
    (delq nil
          (lambda (data)
            (when (car data)
              ;; For each pair
              (concat

               ;; Encode the name
               "Content-Disposition: form-data; name=\""
               (car data) "\"\r\n"
               "Content-Type: text/plain; charset=utf-8\r\n"
               "Content-Transfer-Encoding: binary\r\n\r\n"

               (cond ((stringp (cdr data))
                      (cdr data))
                     ((integerp (cdr data))
                      (int-to-string (cdr data))))

               "\r\n"))))

    ;; use the boundary as a separator
    pairs (concat "--" boundary "\r\n"))

   ;; put a boundary at the end.
   "--" boundary "--\r\n"))

(defgroup mediawiki nil
  "A mode for editting pages on MediaWiki sites."
  :tag "MediaWiki"
  :group 'applications)

(defcustom mediawiki-site-default "Wikipedia"
  "The default mediawiki site to point to.  Set here for the
default and use `mediawiki-site' to set it per-session
later."
  :type 'string
  :tag "MediaWiki Site Default"
  :group 'mediawiki)

(defcustom mediawiki-site-alist '(("Wikipedia"
                                   "http://en.wikipedia.org/wiki/index.php"
                                   "username"
                                   "password"
				   "Main Page"))
  "A list of MediaWiki websites."
  :group 'mediawiki
  :type '(alist :tag "Site Name"
                :key-type (string :tag "Site Name")
                :value-type (list :tag "Parameters"
                                  (string :tag "URL")
                                  (string :tag "Username")
                                  (string :tag "Password")
                                  (string :tag "First Page"
                                          :description "First page to open when `mediawiki-site' is called for this site"))))

(defvar mediawiki-enumerate-with-terminate-paragraph nil
"*Before insert enumerate/itemize do \\[mediawiki-terminate-paragraph].")

(defvar mediawiki-english-or-german t
  "*Variable in order to set the english (t) or german (nil) environment.")

(defvar mediawiki-user-simplify-signature t
  "*Simple varible in order to threat complicated signatures of users, which uses
fonts and other makeup.")

(defgroup mediawiki-draft nil
  "A mode to mediawiki-draft information."
  :group 'mediawiki)

;;; User Variables:

(defcustom mediawiki-draft-mode-hook nil
  "*Functions run upon entering mediawiki-draft-mode."
  :type 'hook
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-filter-functions nil
  "*Functions run to filter mediawiki-draft data.
All functions are run in the mediawiki-draft buffer."
  :type 'hook
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-handler-functions '(mediawiki-draft-append-to-file)
  "*Functions run to process mediawiki-draft data.
Each function is called with the current buffer narrowed to what the
user wants mediawiki-drafted.
If any function returns non-nil, the data is assumed to have been
recorded somewhere by that function. "
  :type 'hook
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-data-file "~/Wiki/discussions/draft.wiki"
  "*The file in which to store the wikipedia drafts."
  :type 'file
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-reply-register ?M
  "The register in which the window configuration is stored."
  :type 'character
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-page ?S		;Version:1.37
  "The register in which the a page of the wiki draft file is stored."
  :type 'character
  :group 'mediawiki-draft)


(defcustom mediawiki-draft-leader-text "== "
  "*The text used to begin each mediawiki-draft item."
  :type 'string
  :group 'mediawiki-draft)

(defvar mediawiki-reply-with-hline nil
"*Whether to use a hline as a header seperator in the reply.")

(defvar mediawiki-reply-with-quote nil
  "*Whether to use a quotation tempalate or not.")

(defvar mediawiki-imenu-generic-expression
  (list '(nil "^==+ *\\(.*[^\n=]\\)==+" 1))
  "Imenu expression for `mediawiki-mode'.  See `imenu-generic-expression'.")

(defvar mediawiki-login-success "pt-logout"
  "A string that should be present on login success on all
mediawiki sites.")

(defvar mediawiki-permission-denied
  "[^;]The action you have requested is limited"
  "A string that will indicate permission has been denied, Note
that it should not match the mediawiki.el file itself since it
is sometimes put on MediaWiki sites.")

(defvar mediawiki-site nil
  "The current mediawiki site from `mediawiki-site-alist'.  If
not set, defaults to `mediawiki-site-default'.")

(defvar mediawiki-argument-pattern "?title=%s&action=%s"
  "Format of the string to append to URLs.  Two string arguments
are expected: first is a title and then an action.")

(defvar mediawiki-URI-pattern
  "http://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/"
  "Pattern matching a URI like this:
	http://mediawiki.sf.net/index.php
Password not support yet")

(defvar mediawiki-page-uri nil
  "The URI of the page corresponding to the current buffer, thus defining
the base URI of the wiki engine as well as group and page name.")

(defvar mediawiki-page-title nil
  "The title of the page corresponding to the current buffer")

(defvar mediawiki-page-ring nil
  "Ring that holds names of buffers we navigate through.")

(defvar mediawiki-page-ring-index 0)

(defvar font-mediawiki-sedate-face 'font-mediawiki-sedate-face
  "Face to use for mediawiki  minor keywords.")
(defvar font-mediawiki-italic-face 'font-mediawiki-italic-face
  "Face to use for mediawiki italics.")
(defvar font-mediawiki-bold-face 'font-mediawiki-bold-face
  "Face to use for mediawiki bolds.")
(defvar font-mediawiki-math-face 'font-mediawiki-math-face
  "Face to use for mediawiki math environments.")
(defvar font-mediawiki-string-face 'font-mediawiki-string-face
  "Face to use for strings.")
(defvar font-mediawiki-verbatim-face 'font-mediawiki-verbatim-face
  "Face to use for text in verbatim macros or environments.")

(defface font-mediawiki-bold-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in bold."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-italic-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :slant custom-face-attributes) '(:slant italic))
		    (t '(:italic t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in italic."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-math-face
  (let ((font (cond ((assq :inherit custom-face-attributes)
		     '(:inherit underline))
		    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight math."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-sedate-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark))  (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "DimGray"))
    (((class color) (background dark))  (:foreground "LightGray"))
   ;;;(t (:underline t))
    )
  "Face used to highlight sedate stuff."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-string-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :slant custom-face-attributes) '(:slant italic))
		    (t '(:italic t)))))
    `((((type tty) (class color))
       (:foreground "green"))
      (((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "RosyBrown"))
      (((class color) (background dark))
       (:foreground "LightSalmon"))
      (t (,@font))))
  "Face used to highlight strings."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-warning-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
    `((((class grayscale)(background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale)(background dark))
       (:foreground "LightGray" ,@font))
      (((class color)(background light))
       (:foreground "red" ,@font))
      (((class color)(background dark))
       (:foreground "red" ,@font))
      (t (,@font))))
  "Face for important keywords."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-verbatim-face
  (let ((font (if (and (assq :inherit custom-face-attributes)
		       (if (fboundp 'find-face)
			   (find-face 'fixed-pitch)
			 (facep 'fixed-pitch)))
		  '(:inherit fixed-pitch)
		'(:family "courier"))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown" ,@font))
      (((class color) (background dark))
       (:foreground "burlywood" ,@font))
      (t (,@font))))
  "Face used to highlight TeX verbatim environments."
  :group 'font-mediawiki-highlighting-faces)

(defvar mediawiki-simple-tags
  '("b" "big" "blockquote" "br" "caption" "code" "center" "cite" "del"
    "dfn" "dl" "em" "i" "ins" "kbd" "math" "nowiki" "ol" "pre" "samp"
    "small" "strike" "strong" "sub" "sup" "tt" "u" "ul" "var")
  "Tags that do not accept arguments.")

(defvar mediawiki-complex-tags
  '("a" "div" "font" "table" "td" "th" "tr")
  "Tags that accept arguments.")

(defvar mediawiki-url-protocols
  '("ftp" "gopher" "http" "https" "mailto" "news")
  "Valid protocols for URLs in Wikipedia articles.")

(defvar mediawiki-draft-buffer "*MW-Draft*"
  "The name of the wikipedia-draft (temporary) data entry buffer.")

(defvar mediawiki-edit-form-vars nil)

(defvar mediawiki-font-lock-keywords
  (list

   ;; Apostrophe-style text markup
   (cons "''''\\([^']\\|[^']'\\)*?\\(''''\\|\n\n\\)"
         'font-lock-builtin-face)
   (cons "'''\\([^']\\|[^']'\\)*?\\('''\\|\n\n\\)"
                                        ;'font-lock-builtin-face)
         'font-mediawiki-bold-face)
   (cons "''\\([^']\\|[^']'\\)*?\\(''\\|\n\n\\)"
         'font-mediawiki-italic-face)

   ;; Headers and dividers
   (list "^\\(==+\\)\\(.*\\)\\(\\1\\)"
         '(1 font-lock-builtin-face)
                                        ;'(2 mediawiki-header-face)
         '(2 font-mediawiki-sedate-face)
         '(3 font-lock-builtin-face))
   (cons "^-----*" 'font-lock-builtin-face)

   ;; Bare URLs and ISBNs
   (cons (concat "\\(^\\| \\)" (regexp-opt mediawiki-url-protocols t)
                 "://[-A-Za-z0-9._\/~%+&#?!=()@]+")
         'font-lock-variable-name-face)
   (cons "\\(^\\| \\)ISBN [-0-9A-Z]+" 'font-lock-variable-name-face)

   ;; Colon indentation, lists, definitions, and tables
   (cons "^\\(:+\\|[*#]+\\||[}-]?\\|{|\\)" 'font-lock-builtin-face)
   (list "^\\(;\\)\\([^:\n]*\\)\\(:?\\)"
         '(1 font-lock-builtin-face)
         '(2 font-lock-keyword-face)
         '(3 font-lock-builtin-face))

   ;; Tags and comments
   (list (concat "\\(</?\\)"
                 (regexp-opt mediawiki-simple-tags t) "\\(>\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-function-name-face t t)
         '(3 font-lock-builtin-face t t))
   (list (concat "\\(</?\\)"
                 (regexp-opt mediawiki-complex-tags t)
                 "\\(\\(?: \\(?:[^\"'/><]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?\\)\\(>\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-function-name-face t t)
         '(3 font-lock-keyword-face t t)
         '(4 font-lock-builtin-face t t))
   (cons (concat "<!-- \\([^->]\\|>\\|-\\([^-]\\|-[^>]\\)\\)*-->")
         '(0 font-lock-comment-face t t))

   ;; External Links
   (list (concat "\\(\\[\\)\\(\\(?:"
                 (regexp-opt mediawiki-url-protocols)
                 "\\)://[-A-Za-z0-9._\/~%-+&#?!=()@]+\\)\\(\\(?: [^]\n]*\\)?\\)\\(\\]\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-variable-name-face t t)
         '(3 font-lock-keyword-face t t)
         '(4 font-lock-builtin-face t t))

   ;; Wiki links
   '("\\(\\[\\[\\)\\([^]\n|]*\\)\\(|?\\)\\([^]\n]*\\)\\(\\]\\]\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t)
     (4 font-lock-keyword-face t t)
     (5 font-lock-builtin-face t t))

   ;; Wiki variables
   '("\\({{\\)\\(.+?\\)\\(}}\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t))

   ;; Character entity references
   (cons "&#?[a-zA-Z0-9]+;" '(0 font-lock-type-face t t))

   ;; Preformatted text
   (cons "^ .*$" '(0 font-lock-constant-face t t))

   ;; Math environment (uniform highlight only, no TeX markup)
   (list "<math>\\(\\(\n?.\\)*\\)</math>"
         '(1 font-lock-keyword-face t t))))

(defvar mediawiki-draft-send-archive t
  "*Archive the reply.")

(defvar mediawiki-draft-mode-map ())

(defun mediawiki-make-url (title action &optional sitename)
  (format (concat (mediawiki-site-url (or sitename mediawiki-site))
                  mediawiki-argument-pattern)
	  (url-hexify-string title)
	  action))

(defun mediawiki-open (name)
  "Open a wiki page specified by NAME from the mediawiki engine"
  (interactive "sWiki Page: ")
  (when (or (not (stringp name))
            (string-equal "" name))
    (error "Need to specify a name."))
  (mediawiki-edit name))

(defun mediawiki-reload ()
  (interactive)
  (let ((title mediawiki-page-title))
    (if title
	(mediawiki-open title)
      (error "Error: %s is not a mediawiki document." (buffer-name)))))

(defun mediawiki-edit (title)
  "Edit wiki file with the name of title"
  (when (not (ring-p mediawiki-page-ring))
    (setq mediawiki-page-ring (make-ring 30)))

  (ring-insert mediawiki-page-ring
               (get-buffer-create
                (concat mediawiki-site ": " title)))
  (mediawiki-get title (ring-ref mediawiki-page-ring 0) mediawiki-site))

(defun mediawiki-get-edit-form-vars (str bufname)
  "Extract the form variables from a page.  This should only be
called from a buffer in mediawiki-mode as the variables it sets
there will be local to that buffer."

  ;; Find the form
  (if (string-match
       "<form [^>]*id=[\"']editform['\"][^>]*>" str)

      (let* ((start-form (match-end 0))
             (end-form (when (string-match "</form>" str start-form)
                         (match-beginning 0)))
             (form (substring str start-form end-form))
             (start (string-match
                     "<input \\([^>]*name=[\"']\\([^\"']+\\)['\"][^>]*\\)>"
                     form)))

        ;; clear the list of (buffer-local) form variables. Have to
        ;; switch like this because we're in a buffer containing the
        ;; results of the post.
        (with-current-buffer bufname
          (setq mediawiki-edit-form-vars '(nil)))

        ;; Continue until we can't find any more input elements
        (while start

          ;; First, capture the place where we'll start next.  Have
          ;; to do this here since match-end doesn't seem to let you
          ;; specify the string you were matching against, unlike
          ;; match-string
          (setq start (match-end 0))

          ;; Capture the string that defines this element
          (let ((el (match-string 1 form))
                ;; get the element name
                (el-name (match-string 2 form)))

            ;; figure out if this is a submit button and skip it if it is.
            (if (and (not (string-match "type=[\"']submit['\"]" el))
                     (string-match "value=[\"']\\([^\"']*\\)['\"]" el))

              ;; set the buffer-local form variables
              (with-current-buffer bufname
                (add-to-list 'mediawiki-edit-form-vars
                             (cons el-name (match-string 1 el))))))
          (setq start
                (string-match
                 "<input \\([^>]*name=[\"']\\([^\"']+\\)['\"][^>]*\\)>"
                 form start)))
        (pop-to-buffer bufname))
    (if (string-match mediawiki-permission-denied str)
        (if (mediawiki-logged-in-p)
            (error "Permission Denied.")
          (error "Log in first and try again."))
      (error "Permission Denied. Login and try again"))
    (error "No edit form found!")))

(defun mediawiki-logged-in-p ()
  "Returns t if we are logged in already."
  (not (eq nil mediawiki-site)))         ; FIXME should check cookies

(defun mediawiki-get (title bufname site)
  (let ((page-uri (mediawiki-make-url title "raw")))

    (with-current-buffer bufname
      (delete-region (point-min) (point-max))
      (mediawiki-mode)
      (setq mediawiki-site site)
      (setq mediawiki-page-title title)
      (setq mediawiki-page-uri page-uri)

      (setq url-request-extra-headers
            (list (cons "Connection" "close"))))

    (url-http-get page-uri         ; Source URI
                  nil              ; headers
                  bufname          ; output buffer
                  ;; callback on result buf
                  (lambda (str title)
                    (url-http-get
                     (mediawiki-make-url title "edit")
                     nil (get-buffer-create " *mediawiki-form*")
                     'mediawiki-get-edit-form-vars
                     (current-buffer))
                    (set-buffer-file-coding-system 'utf-8)
                    (goto-char (point-min))
                    (pop-to-buffer bufname)
                    (set-buffer-modified-p nil)
                    (setq buffer-undo-list t)
                    (buffer-enable-undo))
                  title)))

(defun mediawiki-save (&optional summary)
  (interactive "sSummary: ")
  (if mediawiki-page-title
      (mediawiki-save-page
       mediawiki-page-title
       summary
       (buffer-substring-no-properties (point-min) (point-max)))
    (error "Error: %s is not a mediawiki document." (buffer-name))))

(defun mediawiki-save-and-bury (&optional summary)
  (interactive "sSummary: ")
  (mediawiki-save summary)
  (bury-buffer))

;; (defun mediawiki-save-as (&optional title summary)
;;   (interactive "sTitle: "
;;                "sSummary: ")
;;   (when (and title (not (string= title "")))
;;     (mediawiki-save-page title summary (buffer-string-no-properties))
;;     (mediawiki-open title)))

(defun mediawiki-site-extract (sitename index)
  (let ((bit (nth index (assoc sitename mediawiki-site-alist))))
    (cond
     ((string-match "[^ \t\n]" bit) bit)
     (nil))))

(defun mediawiki-site-url (sitename)
  "Get the url for a given site."
  (mediawiki-site-extract sitename 1))

(defun mediawiki-site-username (sitename)
  "Get the username for a given site."
  (or (mediawiki-site-extract sitename 2)
      (url-user-for-url (mediawiki-site-url sitename))))

(defun mediawiki-site-password (sitename)
  "Get the password for a given site."
  (or (mediawiki-site-extract sitename 3)
      (url-user-for-url (mediawiki-site-url sitename))))

(defun mediawiki-site-first-page (sitename)
  "Get the password for a given site."
  (mediawiki-site-extract sitename 4))

(defun mediawiki-do-logout ()
  (interactive)
  (let ((url-http-get-post-process (lambda (url &optional headers
                                                bufname callback cbargs))))
    (url-http-get
     (mediawiki-make-url "Special:UserLogout" "" mediawiki-site))))

(defun mediawiki-do-login (&optional sitename username password)
  "Use USERNAME and PASSWORD to log into the MediaWiki site and
get a cookie."
  (interactive)
  (when (not sitename)
    (setq sitename (mediawiki-prompt-for-site)))

  (setq mediawiki-site nil)             ; This wil be set once we are
                                        ; logged in

  ;; Possibly save info once we have it, eh?
  (let ((url (or (mediawiki-site-url sitename)
                 (read-string "URL: ")))
        (user (or (mediawiki-site-username sitename)
                  username
                  (read-string "Username: ")))
        (pass (or (mediawiki-site-password sitename)
                  password
                  (read-passwd "Password: "))))

    ;; Set the wpName and wpPassword on the proper submit page.
    (url-http-post (mediawiki-make-url
                    "Special:Userlogin" "submitlogin" sitename)
                   (list (cons "wpName" user)
                         (cons "wpLoginattempt" "Log in")
                         (cons "wpPassword" pass))
                   nil nil nil
                   'mediawiki-check-login
                   (list sitename))))

(defun mediawiki-check-login (str args)
  "Check that the password was accepted."
  (if (not (string-match mediawiki-login-success str))
      (error "Invalid Login!")
    (setq mediawiki-site (car args))
    (message (format "Login to MediaWiki site '%s' successful."
                     mediawiki-site))
    (when (mediawiki-site-first-page mediawiki-site)
      (mediawiki-open (mediawiki-site-first-page mediawiki-site)))))

(defun mediawiki-save-page (&optional title summary content)
  "Save the current page to a MediaWiki wiki."
  (let ((page-uri (mediawiki-make-url title "submit&externaledit=true"
                                      mediawiki-site))
        (document (if content content
                      (buffer-string)))
        (args mediawiki-edit-form-vars))

    (mapc
     (lambda (pair)
       (let ((key (car pair))
             (val (cdr pair)))
         (if (assoc key args)
             (setcdr (assoc key args) val)
           (add-to-list 'args
                        (cons key val)))))
     (list (cons "wpSummary" summary)
           (cons "wpTextbox1" content)
           (cons "wpSave" 1)))

    (setq url-request-extra-headers
          (list (cons "Connection" "Close")))

    (url-http-post
     page-uri  ;; Get destination
     args      ;; Form elements
     t)        ;; This is multipart

    (set-buffer-modified-p nil)))

(defun mediawiki-browse (&optional buf)
  "Open the buffer BUF in a browser. If BUF is not given,
the current buffer is used."
  (interactive)
  (if mediawiki-page-title
      (browse-url (mediawiki-make-url mediawiki-page-title "view"))))

(defun mediawiki-prompt-for-site ()
  (let* ((prompt (concat "Sitename"
                         (when mediawiki-site
                           (format " (default %s)" mediawiki-site))
                         ": "))
         (answer (completing-read prompt mediawiki-site-alist nil t)))
    (if (string= "" answer)
        mediawiki-site
      answer)))


(defun mediawiki-site (&optional site)
  "Set up mediawiki.el for a site.  Without an argument, use
`mediawiki-site-default'.  Interactively, prompt for a site."
  (interactive)
  (when (not site)
    (setq site (mediawiki-prompt-for-site)))
  (when (or (eq nil mediawiki-site)
            (not (string-equal site mediawiki-site)))
    (mediawiki-do-login site)))

(defun mediawiki-open-page-at-point ()
  "Open a new buffer with the page at point."
  (interactive)
  (mediawiki-open (mediawiki-page-at-point)))

(defun mediawiki-page-at-point ()
  "Return the page name under point.  Typically, this means
anything enclosed in [[PAGE]]."
  (let ((pos (point))
        (eol (point-at-eol))
        (bol (point-at-bol))
        page)
    (save-excursion
      (let* ((start  (when (search-backward "[[" bol t)
                       (+ (point) 2)))
             (end    (when (search-forward "]]" eol t)
                       (- (point) 2)))
             (middle (progn
                       (goto-char start)
                       (when (search-forward  "|" end t)
                         (1- (point))))))
        (when (and
               (not (eq nil start))
               (not (eq nil end))
               (<= pos end)
               (>= pos start))
          (buffer-substring-no-properties
           start (or middle end)))))))

(defmacro mediawiki-goto-relative-page (direction)
  `(let ((buff (ring-ref mediawiki-page-ring
                        (setq mediawiki-page-ring-index
                              (,direction mediawiki-page-ring-index 1)))))
     (while (not (buffer-live-p buff))
       (setq buff
             (ring-ref mediawiki-page-ring
                       (setq mediawiki-page-ring-index
                             (,direction mediawiki-page-ring-index 1)))))
     (pop-to-buffer buff)))

(defun mediawiki-next-header ()
  "Move point to the end of the next section header."
  (interactive)
  (let ((oldpoint (point)))
    (end-of-line)
    (if (re-search-forward "\\(^==+\\).*\\1" (point-max) t)
        (beginning-of-line)
      (goto-char oldpoint)
      (message "No section headers after point."))))

(defun mediawiki-prev-header ()
  "Move point to the start of the previous section header."
  (interactive)
  (unless (re-search-backward "\\(^==+\\).*\\1" (point-min) t)
    (message "No section headers before point.")))

(defun mediawiki-terminate-paragraph ()	;Version:1.58
  "In a list, start a new list item. In a paragraph, start a new
paragraph; if the current paragraph is colon indented, the new
paragraph will be indented in the same way."
  (interactive)
  (let (indent-chars)
    (save-excursion
      (beginning-of-line)
      (while (cond ((looking-at "^$") nil)
                   ((looking-at "^\\(\\(?: \\|:+\\|[#*]+\\) *\\)")
                    (setq indent-chars (match-string 1)) nil)
                   ((eq (point) (point-min)) nil)
                   ((progn (forward-line -1) t)))
        t))
    (newline) (if (not indent-chars) (newline)
		(insert indent-chars))))

(defun mediawiki-terminate-paragraph-and-indent ()
  "In a list, start a new list item. In a paragraph, start a new
paragraph but *,# will be ignored; if the current paragraph is colon
; indented, the new paragraph will be indented in the same way."
  (interactive)
  (let (indent-chars)
    (save-excursion
      (beginning-of-line)
      (while (cond ((looking-at "^$") nil)
                   ((looking-at "^\\(\\(?: \\|:+\\) *\\)")
                    (setq indent-chars (match-string 1)) nil)
                   ((eq (point) (point-min)) nil)
                   ((progn (forward-line -1) t)))
        t))
    (newline)
    (if (not indent-chars) (newline)
      (insert indent-chars))))


(defun mediawiki-link-fill-nobreak-p ()
  "When filling, don't break the line for preformatted (fixed-width)
text or inside a Wiki link.  See `fill-nobreak-predicate'."
  (save-excursion
    (let ((pos (point)))
      (or (eq (char-after (line-beginning-position)) ? )
          (if (re-search-backward "\\[\\[" (line-beginning-position) t)
              ;; Break if the link is really really long.
              ;; You often get this with captioned images.
              (null (or (> (- pos (point)) fill-column)
                        (re-search-forward "\\]\\]" pos t))))))))

(defun mediawiki-fill-article ()
  "Fill the entire article."
  (interactive)
  (save-excursion
    (fill-region (point-min) (point-max))))

(defun mediawiki-unfill-article ()
  "Undo filling, deleting stand-alone newlines (newlines that do not
end paragraphs, list entries, etc.)"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ".\\(\n\\)\\([^# *;:|!\n]\\|----\\)" nil t)
      (replace-match " " nil nil nil 1)))
  (message "Stand-alone newlines deleted"))

(defun mediawiki-draft-reply ()
  "Open a temporary buffer in mediawiki mode for editing an
mediawiki draft, with an arbitrary piece of data. After finishing
the editing |]]:either use \"C-c C-k\" \\[mediawiki-draft-buffer]
to send the data into the mediawiki-draft-data-file, or send the
buffer \"C-c\C-c\", to the current article. Check the variable
mediawiki-draft-send-archive."
  (interactive)
  (mediawiki-reply-at-point-simple)
  (beginning-of-line 1)
  (kill-line nil)
  (save-excursion
	(window-configuration-to-register mediawiki-draft-register)
	(let ((buf (get-buffer-create mediawiki-draft-buffer)))
	  (switch-to-buffer-other-window buf)
	  (mediawiki-mode)
	  (if mediawiki-reply-with-quote
              (progn
		(insert "{{Quotation|")
		(yank)
		(insert "'''Re: ")
		(insert-register mediawiki-draft-reply-register 1)
		(insert "''' |~~~~}}")
		(backward-char 7))
            (when mediawiki-reply-with-hline
              (insert "----")
              (newline 1))
            (yank)
            (end-of-line 1))
	  (message " C-c C-k sends to draft, C-c C-c sends to org buffer."))))

(defun mediawiki-reply-at-point-simple ()
  "Very simple function to reply to posts in the discussion forum. You have to set
the point around the signature, then the functions inserts the following
:'''Re: [[User:foo]]'''."
  (interactive)
  (beginning-of-line 1)
  (if mediawiki-english-or-german
      (progn
        (search-forward "(UTC)")
        (search-backward "[[User:"))
    (search-forward "(CET)")
    (search-backward "[[Benutzer:"))
  (if mediawiki-user-simplify-signature
      (mark-word 2)
    (mark-word 3))
  (copy-to-register mediawiki-draft-reply-register (region-beginning) (region-end) nil)
  (end-of-line 1)
  (mediawiki-terminate-paragraph-and-indent)
  (insert ":'''Re: ")
  (insert-register mediawiki-draft-reply-register 1)
  (if mediawiki-user-simplify-signature
      (insert "|]]''' ")
    (insert "]]''' ")))

(defun mediawiki-goto-previous-page ()
  "Pop up the previous page being editted."
  (interactive)
  (mediawiki-goto-relative-page -))

(defun mediawiki-goto-next-page ()
  "Pop up the previous page being editted."
  (interactive)
  (mediawiki-goto-relative-page +))

(defun mediawiki-goto-next-link ()
  (interactive)
  (let ((end (re-search-forward "\\[\\[.+\\]\\]")))
    (when end
      (let ((start (match-beginning 0)))
        (goto-char (+ start 2))))))

(defun mediawiki-goto-prev-link ()
  (interactive)
  (let ((start (re-search-backward "\\[\\[.+\\]\\]")))
    (when start
      (let ((start (match-beginning 0)))
        (goto-char (+ start 2))))))

(defvar wikipedia-enumerate-with-terminate-paragraph nil
"*Before insert enumerate/itemize do \\[wikipedia-terminate-paragraph].")

(defun mediawiki-insert-enumerate ()
"Primitive Function for inserting enumerated items, check the
variable wikipedia-enumerate-with-terminate-paragraph. Note however
that the function \\[wikipedia-terminate-paragraph] does not work very
well will longlines-mode."
  (interactive)
  (if mediawiki-enumerate-with-terminate-paragraph
      (progn
        (mediawiki-terminate-paragraph)
        (insert "#"))
    (newline nil)
    (insert ":#")))

(defun mediawiki-insert-itemize ()
  "Primitive Function for inserting no enumerated items, check
the variable mediawiki-enumerate-with-terminate-paragraph. Note
however that the function \\[mediawiki-terminate-paragraph] does
not work very well will longlines-mode."
  (interactive)
  (if mediawiki-enumerate-with-terminate-paragraph
      (progn
        (mediawiki-terminate-paragraph)
        (insert "*"))
    (newline nil)
    (insert ":*")))

(defun mediawiki-insert-strong-emphasis ()
  "Insert strong emphasis italics via four
apostrophes (e.g. ''''FOO''''.) When mark is active, surrounds
region."
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
          (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
      (save-excursion
        (goto-char (point))
        (insert "''''")
        (goto-char (mark))
        (insert "''''"))
    (insert "'''' ''''")
    (backward-char 5)))

(defun mediawiki-insert-bold ()
  "Insert bold via three apostrophes (e.g. '''FOO'''.)
When mark is active, surrounds region."
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
          (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
      (save-excursion
        (goto-char (point))
        (insert "'''")
        (goto-char (mark))
        (insert "'''"))
    (insert "''' '''")
    (backward-char 4)))

(defun mediawiki-insert-italics ()
  "Insert bold via TWO apostrophes (e.g. ''FOO''.) When mark is active,
surrounds region."
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
          (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
      (save-excursion
        (goto-char (point))
        (insert "''")
        (goto-char (mark))
        (insert "''"))
    (insert "'' ''")
    (backward-char 3)))

(defun mediawiki-insert-quotation-with-signature ()
  "Insert bold via TWO apostrophes (e.g. ''FOO''.) When mark is active,
surrounds region."
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
          (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
      (save-excursion
        (goto-char (point))
        (insert "{{Quotation|}}")
        (goto-char (mark))
        (insert "{{~~~~}}"))
    (insert "{{Quotation| }}{{~~~~}}")
    (backward-sexp 1)
    (backward-char 3)))

(defun mediawiki-insert-quotation ()
  "Quotation box of the form {{Quotation}}{{}}. When mark is active,
surrounds region."
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
          (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
      (save-excursion
        (goto-char (point))
        (insert "{{Quotation|}}")
        (goto-char (mark))
        (insert "{{}}"))
    (insert "{{Quotation|}}{{ }}")
    (backward-char 3)))

(defun mediawiki-insert-bible-verse-template ()
  "Insert a template for the quotation of bible verses."
  (interactive)
  (insert "({{niv|")
  (let ((name    (read-string "Name: ")))
    (insert (concat name "|"))
    (let ((verse (read-string "Verse: ")))
      (insert (concat verse "|" name " " verse "}})")))))

(defun mediawiki-insert-user ()
  "Inserts, interactively a user name [[User:foo]]"
  (interactive)
  (if mediawiki-english-or-german
      (let ((user (read-string "Name of user: " )))
        (insert (concat "[[User:" user "|" user "]]"))))
  (let ((user (read-string "Name des Benutzers: " )))
    (insert (concat "[[Benutzer:" user "|" user "]]"))))

(defun mediawiki-insert-reply-prefix ()
  "Quotation box of the form {{Quotation}}{{}}. When mark is active,
surrounds region."
  (interactive)
  (beginning-of-line 1)
  (search-forward "[[")
  (backward-char 2)
  (mark-sexp 1)
  (copy-to-register mediawiki-draft-reply-register (region-beginning) (region-end) nil)
  (end-of-line 1)
  (mediawiki-terminate-paragraph)
  (beginning-of-line 1)
  (kill-line nil)
  (insert "----")
  (newline 1)
  (yank)
  (insert ":'''Re: ")
  (insert-register mediawiki-draft-reply-register 1)
  (insert "''' ")
  (end-of-line 1))

(defun mediawiki-insert-header ()
  "Insert subheader  via  == (e.g. == FOO ==.)"
  (interactive)
  (insert "==   ==")
  (backward-char 4))

(defun mediawiki-insert-link ()
  "Insert link via [[ (e.g. [[FOO]].) When mark is active, surround region."
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
          (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
      (save-excursion
        (goto-char (point))
        (insert "]]")
        (goto-char (mark))
        (insert "[["))
    (insert "[[ ]]")
    (backward-char 3)))

(defun mediawiki-insert-link-www ()
  "Insert link via [[ (e.g. [http://FOO].) When mark is active, surround region."
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
          (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
      (save-excursion
        (goto-char (point))
        (insert "]")
        (goto-char (mark))
        (insert "[http://"))
    (insert "[http:// ]")
    (backward-char 2)))

(defun mediawiki-insert-image ()
  "Insert link image  [[ (e.g. [[Image:FOO]].) Check the variable
mediawiki-english-or-german. When mark is active, surround region."
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
          (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
      (save-excursion
        (goto-char (point))
        (insert "]]")
        (goto-char (mark))
        (if mediawiki-english-or-german
            (insert "[[Image:")
          (insert "[[Bild:")))
    (if mediawiki-english-or-german
        (insert "[[Image: ]]")
      (insert "[[Bild: ]]"))
    (backward-char 3)))

(defun mediawiki-insert-audio ()
  "Insert link image  [[ (e.g. [[Image:FOO]].) Check the variable
mediawiki-english-or-german. When mark is active, surround region."
  (interactive)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
          (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
      (save-excursion
        (goto-char (point))
        (insert "]]")
        (goto-char (mark))
        (if mediawiki-english-or-german
            (insert "[[Media:")
          (insert "[[Bild:"))
    (if mediawiki-english-or-german
        (insert "[[Media: ]]")
      (insert "[[Bild: ]]"))
    (backward-char 3)))

(defun mediawiki-insert-signature ()
  "Insert \"~~~~:\"  "
  (interactive)
  (insert "~~~~: "))

(defun mediawiki-insert-hline ()
  "Insert \"----\"  "
  (interactive)
  (insert "\n----\n"))

(defun mediawiki-unfill-paragraph-or-region ()
  "Unfill region, this function does NOT explicitly search for \"soft newlines\"
as does mediawiki-unfill-region."
  (interactive)
  (set (make-local-variable 'paragraph-start) "[ \t\n\f]")
  (set (make-local-variable 'paragraph-start)
       "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$")
  (set-fill-prefix)
  (beginning-of-line 1)

  (if use-hard-newlines
      (progn
        (set (make-local-variable 'use-hard-newlines) nil)
        (set (make-local-variable 'sentence-end-double-space) t))
    (set (make-local-variable 'sentence-end-double-space) nil)
    (set (make-local-variable 'use-hard-newlines) t))
  (let ((fill-column (point-max)))
    (if (fboundp 'fill-paragraph-or-region)
        (fill-paragraph-or-region nil)
      (fill-paragraph nil))))

(defun mediawiki-start-paragraph ()
  (interactive)
  (set (make-local-variable 'paragraph-start)
       "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$"))

(defun mediawiki-hardlines ()
"Set use-hard-newlines to NIL."
  (interactive)
  (setq use-hard-newlines nil))

(defun mediawiki-next-long-line ()
  "Move forward to the next long line with column-width greater
than `fill-column'.

TODO: When function reaches end of buffer, save-excursion to
starting point. Generalise to make `previous-long-line'."
  (interactive)
  ;; global-variable: fill-column
  (if (= (forward-line) 0)
	  (let ((line-length
			 (save-excursion
			   (end-of-line)
			   (current-column))))
		(if (<= line-length fill-column)
			(mediawiki-next-long-line)
		  (message "Long line found")))
	;; Stop, end of buffer reached.
 	(error "Long line not found")))

(defun mediawiki-unfill-paragraph-simple ()
  "A very simple function for unfilling a paragraph."
  (interactive)
  (if (functionp 'filladapt-mode)
      (filladapt-mode nil))
  (let ((fill-column (point-max)))
    (fill-paragraph nil)
    (if (functionp 'filladapt-mode)
        (filladapt-mode nil))))

;; See http://staff.science.uva.nl/~dominik/Tools/outline-magic.el
(defun mediawiki-outline-magic-keys ()
  (interactive)
  (unless  (featurep 'xemacs)
    (local-set-key [(shift iso-lefttab)] 'outline-cycle)
    (local-set-key [iso-left-tab] 'outline-cycle))
  (local-set-key [(meta left)]  'outline-promote)
  (local-set-key [(meta right)] 'outline-demote)
  (local-set-key [(shift return)] 'newline-and-indent)
  (local-set-key [(control left)]  'mediawiki-simple-outline-promote)
  (local-set-key [(control right)] 'mediawiki-simple-outline-demote)
  (local-set-key [(control up)] 'outline-move-subtree-up)
  (local-set-key [(control down)] 'outline-move-subtree-down))
(add-hook 'mediawiki-mode-hook (lambda () (outline-minor-mode nil)))
(add-hook 'outline-minor-mode-hook 'mediawiki-outline-magic-keys)

(defun mediawiki-enhance-indent ()
  (interactive)
  (string-rectangle (region-beginning) (region-end) ":"))

(defun mediawiki-yank-prefix ()
  (interactive)
  (string-rectangle (region-beginning) (region-end) ":"))

(defun mediawiki-simple-outline-promote ()
  "Function simple deletes \"=\" and the end and the beginning of line,
does not promote the whole tree!"
  (interactive)
  (save-excursion
    (progn
      (beginning-of-line 1)
      (search-forward "=")
      (delete-char 1 nil)
      (end-of-line 1)
      (search-backward "=")
      (delete-char 1 nil))))

(defun mediawiki-simple-outline-demote ()
  "Function simple adds \"=\" and the end and the beginning of line,
does not promote the whole tree!"
  (interactive)
  (save-excursion
    (progn
      (beginning-of-line 1)
      (search-forward "=")
      (insert "=")
      (end-of-line 1)
      (search-backward "=")
      (insert "="))))

(defun mediawiki-rename-buffer ()
  "Make sure that the option UNIQUE is used."
  (interactive)
  (rename-buffer (read-string "Name of new buffer (unique): " ) 1))

(defsubst mediawiki-draft-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defsubst mediawiki-draft-mail-date (&optional rfc822-p)
  "Return a simple date.  Nothing fancy."
  (if rfc822-p
      (format-time-string "%a, %e %b %Y %T %z" (current-time))
    (format-time-string "%c" (current-time))))

(defun mediawiki-draft-buffer-desc ()
  "Using the first line of the current buffer, create a short description."
  (buffer-substring (point-min)
		    (save-excursion
		      (goto-char (point-min))
		      (end-of-line)
		      (if (> (- (point) (point-min)) 60)
			  (goto-char (+ (point-min) 60)))
		      (point))))

(defun mediawiki-draft-append-to-file ()
  "Add a header together with a subject to the text and add it to the
draft file. It might be better if longlines-mode is off."
  (let ((text (buffer-string))
        (desc (mediawiki-draft-buffer-desc)))
    (with-temp-buffer
      (insert (concat "\n\n"  mediawiki-draft-leader-text "Draft: "
                      (read-string "Enter Subject: ") " "
                      (current-time-string) " "
                      mediawiki-draft-leader-text
                      "\n\n\f\n\n" text "\n\f\n"))
      (if (not (bolp))
          (insert "\n\n"))
      (if (find-buffer-visiting mediawiki-draft-data-file)
          (let ((mediawiki-draft-text (buffer-string)))
            (set-buffer (get-file-buffer mediawiki-draft-data-file))
            (save-excursion
              (goto-char (point-max))
              (insert (concat "\n" mediawiki-draft-text "\n"))
              (save-buffer)))
        (append-to-file (point-min) (point-max) mediawiki-draft-data-file))))))

;;;###autoload
(defun mediawiki-draft ()
  "Open a temporary buffer in wikipedia mode for editing an wikipedia
 draft, which an arbitrary piece of data. After finishing the editing
 either use C-c C-k \\[mediawiki-draft-buffer] to send the data into
 the mediawiki-draft-data-file, or send  the buffer using C-x C-s
\\[mediawiki-save]  and insert it later into a wikipedia article."
  (interactive)
  (window-configuration-to-register mediawiki-draft-register)
  (let ((buf (get-buffer-create mediawiki-draft-buffer)))
    (switch-to-buffer-other-window buf)
    (mediawiki-mode)
    (message " C-c C-k sends to draft file, C-c C-c sends to org buffer.")))

;;;###autoload
(defun mediawiki-draft-page ()
  (interactive)
  (mark-page)
  (copy-region-as-kill (region-beginning) (region-end))
  (mediawiki-draft)
  (yank nil))

(defun mediawiki-draft-region (&optional beg end)
  "Mediawiki-Draft the data from BEG to END.
If called from within the mediawiki-draft buffer, BEG and END are ignored,
and the entire buffer will be mediawiki-drafted.  If called from any other
buffer, that region, plus any context information specific to that
region, will be mediawiki-drafted."
  (interactive)
  (let ((b (or beg (min (point) (or (mark) (point-min)))))
	(e (or end (max (point) (or (mark) (point-max))))))
    (save-restriction
      (narrow-to-region b e)
      (run-hook-with-args-until-success 'mediawiki-draft-handler-functions)
    (when (equal mediawiki-draft-buffer (buffer-name))
      (kill-buffer (current-buffer))
      (jump-to-register mediawiki-draft-register)))))

;;;###autoload
(defun mediawiki-draft-buffer ()
  "Mediawiki-draft-buffer sends the contents of the current (temporary)
buffer to the mediawiki-draft-buffer, see the variable
mediawiki-draft-data-file."
  (interactive)
  (mediawiki-draft-region  (point-min) (point-max)))

(defun mediawiki-draft-clipboard ()
  "Mediawiki-Draft the contents of the current clipboard.
Most useful for mediawiki-drafting things from Netscape or other X Windows
application."
  (interactive)
  (with-temp-buffer
    (insert (x-get-clipboard))
    (run-hook-with-args-until-success 'mediawiki-draft-handler-functions)))

(defun mediawiki-draft-view-draft ()
  "Simple shortcut to visit the file, which contains the wikipedia drafts."
  (interactive)
  (find-file mediawiki-draft-data-file))

(defun mediawiki-mark-section ()
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward (concat  "== "  "[a-z,A-z \t]*" " =="))
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward (concat "== "  "[a-z,A-z \t]*" " "))
  (if (featurep 'xemacs)
      (zmacs-activate-region)))

(defun mediawiki-mark-signature ()
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward "]]") ;;[[ ]]
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward "[[")
  (when (featurep 'xemacs)
    (zmacs-activate-region)))

(defun mediawiki-draft-copy-page-to-register ()
  "Copy a page via the mediawiki-draft-register."
  (interactive)
  (save-excursion
    (narrow-to-page nil)
    (copy-to-register mediawiki-draft-page (point-min) (point-max) nil)
    (message "draft page copied to wikipedia register mediawiki-draft-page.")
    (widen)))

(defun mediawiki-draft-yank-page-to-register ()
  "Insert a page via the mediawiki-draft-register."
  (interactive)
  (insert-register mediawiki-draft-page nil))

(defun mediawiki-draft-send (target-buffer)
  "Copy the current page from the mediawiki draft file to
TARGET-BUFFER.  Check the variable mediawiki-draft-send-archive.
If it is t, then additionally the text will be archived in the
draft.wiki file. Check longlines-mode, it might be better if it
is set off."
  (interactive "bTarget buffer: ")
  (let ((src-buf (current-buffer)))
	(mediawiki-draft-copy-page-to-register)
	(switch-to-buffer target-buffer)
	(end-of-line 1)
	(newline 1)
    (mediawiki-draft-yank-page-to-register)
	(message "The page has been sent (copied) to the mozex file!")
	(switch-to-buffer "*MW-Draft*")
    (when mediawiki-draft-send-archive
	  (let ((text (buffer-string))
			(desc (mediawiki-draft-buffer-desc)))
		(with-temp-buffer
		  (insert (concat "\n\n" mediawiki-draft-leader-text)
		  (insert-register mediawiki-draft-reply-register 1)
		  (insert (concat " " (current-time-string) " " mediawiki-draft-leader-text  "\n\n\f\n\n"
                                  text "\n\f\n"))
		  (if (not (bolp))
			  (insert "\n\n"))
		  (if (find-buffer-visiting mediawiki-draft-data-file)
			  (let ((mediawiki-draft-text (buffer-string)))
				(set-buffer (get-file-buffer mediawiki-draft-data-file))
				(save-excursion
				  (goto-char (point-max))
				  (insert (concat "\n" mediawiki-draft-text "\n"))
				  (save-buffer)))
			(append-to-file (point-min) (point-max) mediawiki-draft-data-file)))))
    (when (equal mediawiki-draft-buffer (buffer-name))
	  (kill-buffer (current-buffer)))
	(switch-to-buffer target-buffer))))

(define-derived-mode mediawiki-draft-mode text-mode "MW-Draft"
  "Major mode for output from \\[mediawiki-draft].
\\<mediawiki-draft-mode-map> This buffer is used to collect data that
you want mediawiki-draft.  Just hit \\[mediawiki-draft-region] when
you're done entering, and it will go ahead and file the data for
latter retrieval, and possible indexing.
\\{mediawiki-draft-mode-map}"
  (kill-all-local-variables)
  (indented-text-mode)
  (define-key mediawiki-draft-mode-map "\C-c\C-k" 'mediawiki-draft-buffer)
  (define-key mediawiki-draft-mode-map "\C-c\C-d" 'mediawiki-draft-buffer))

(define-derived-mode mediawiki-mode text-mode "MW"
  "Major mode for editing articles written in the markup language
used by Mediawiki.

Wikipedia articles are usually unfilled: newline characters are not
used for breaking paragraphs into lines. Unfortunately, Emacs does not
handle word wrapping yet. As a workaround, wikipedia-mode turns on
longlines-mode automatically. In case something goes wrong, the
following commands may come in handy:

\\[mediawiki-fill-article] fills the buffer.
\\[mediawiki-unfill-article] unfills the buffer.

Be warned that function can be dead  slow, better use mediawiki-unfill-paragraph-or-region.
\\[mediawiki-unfill-paragraph-or-region] unfills the paragraph
\\[mediawiki-unfill-paragraph-simple] doehe same but simpler.

The following commands put in markup structures.
\\[mediawiki-insert-strong-emphasis] inserts italics
\\[mediawiki-insert-bold] inserts bold text
\\[mediawiki-insert-italics] italics
\\[mediawiki-insert-header] header
\\[mediawiki-insert-link] inserts a link

The following commands are also defined:
\\[mediawiki-insert-user] inserts user name
\\[mediawiki-insert-signature] inserts ~~~~
\\[mediawiki-insert-enumerate] inserts enumerate type structures
\\[mediawiki-insert-itemize] inserts itemize type structures
\\[mediawiki-insert-hline] inserts a hline

The draft functionality
\\[mediawiki-draft]
\\[mediawiki-draft-region]
\\[mediawiki-draft-view-draft]
\\[mediawiki-draft-page]
\\[mediawiki-draft-buffer]

Replying and sending functionality
\\[mediawiki-reply-at-point-simple]
\\[mediawiki-draft-reply]

The register functionality
\\[mediawiki-copy-page-to-register]
\\[defun mediawiki-insert-page-to-register]

Some simple editing commands.
\\[mediawiki-enhance-indent]
\\[mediawiki-yank-prefix]
\\[mediawiki-unfill-paragraph-or-region]

\\[mediawiki-terminate-paragraph]     starts a new list item or paragraph in a context-aware manner.
\\[mediawiki-next-header]     moves to the next (sub)section header.
\\[mediawiki-prev-header]     moves to the previous (sub)section header."

  (make-local-variable 'change-major-mode-hook)
  (make-local-variable 'mediawiki-page-title)
  (make-local-variable 'mediawiki-site)
  (make-local-variable 'mediawiki-edit-form-vars)
  (set (make-local-variable 'adaptive-fill-regexp) "[ ]*")
  (set (make-local-variable 'comment-start-skip) "\\(?:<!\\)?-- *")
  (set (make-local-variable 'comment-end-skip) " *--\\([ \n]*>\\)?")
  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")
  (set (make-local-variable 'paragraph-start)
       "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$")
  (set (make-local-variable 'sentence-end-double-space) nil)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'font-lock-defaults)
       '(mediawiki-font-lock-keywords t nil nil nil))
  (set (make-local-variable 'fill-nobreak-predicate)
       'mediawiki-link-fill-nobreak-p)
  (set (make-local-variable 'auto-fill-inhibit-regexp) "^[ *#:|;]")

  ;; Support for outline-minor-mode. No key conflicts, so we'll use
  ;; the normal outline-mode prefix.
  (set (make-local-variable 'outline-regexp) "==+")
  (set (make-local-variable 'outline-minor-mode-prefix) "\C-c\C-o")
; (set (make-local-variable 'outline-regexp) "=+")
; (set (make-local-variable 'outline-regexp) ":")

  ;; Turn on the Imenu automatically.
  (when menu-bar-mode
    (set (make-local-variable 'imenu-generic-expression)
         mediawiki-imenu-generic-expression)
    (imenu-add-to-menubar "Contents"))

  (let ((map (make-sparse-keymap "mediawiki")))
    (define-key mediawiki-mode-map [menu-bar mediawiki]
      (cons "MediaWiki" map))
    (define-key map [unfill-article]
      '("Unfill article" . mediawiki-unfill-article))
    (define-key map [fill-article]
      '("Fill article" . mediawiki-fill-article))
    (define-key map [separator-fill] '("--"))
    (define-key map [next-header]
      '("Next header" . mediawiki-next-header))
    (define-key map [prev-header]
      '("Previous header" . mediawiki-prev-header))
    (define-key map [separator-header] '("--"))
    (define-key map [outline]
      '("Toggle Outline Mode..." . outline-minor-mode))

    (modify-syntax-entry ?< "(>" mediawiki-mode-syntax-table)
    (modify-syntax-entry ?> ")<" mediawiki-mode-syntax-table)

    (define-key mediawiki-mode-map "\M-n" 'mediawiki-next-header)
    (define-key mediawiki-mode-map "\C-c\C-n" 'mediawiki-next-long-line)
    (define-key mediawiki-mode-map "\M-p" 'mediawiki-prev-header)
    (define-key mediawiki-mode-map [(meta down)] 'mediawiki-next-header)
    (define-key mediawiki-mode-map [(meta up)]   'mediawiki-prev-header)
    (define-key mediawiki-mode-map "\C-j" 'mediawiki-terminate-paragraph)

    (define-key mediawiki-mode-map "\C-c\C-q" 'mediawiki-unfill-article)
    (define-key mediawiki-mode-map "\C-c\M-q" 'mediawiki-fill-article)
    (define-key mediawiki-mode-map "\M-u" 'mediawiki-unfill-paragraph-or-region)
    (define-key mediawiki-mode-map "\C-c\C-u" 'mediawiki-unfill-paragraph-simple)
    (define-key mediawiki-mode-map "\C-c\C-f\C-s" 'mediawiki-insert-strong-emphasis)
    (define-key mediawiki-mode-map "\C-c\C-f\C-b" 'mediawiki-insert-bold)
    (define-key mediawiki-mode-map "\C-c\C-f\C-i" 'mediawiki-insert-italics)
    (define-key mediawiki-mode-map "\C-c\C-f\C-h" 'mediawiki-insert-header)
    (define-key mediawiki-mode-map "\C-c\C-f\C-l" 'mediawiki-insert-link)
    (define-key mediawiki-mode-map "\C-c\C-f\C-u" 'mediawiki-insert-user)
    (define-key mediawiki-mode-map "\C-c\C-f\C-q" 'mediawiki-insert-quotation)
    (define-key mediawiki-mode-map "\C-c\C-f\C-v" 'mediawiki-insert-bible-verse-template)
    (define-key mediawiki-mode-map "\C-c\C-w" 'mediawiki-insert-signature)
    (define-key mediawiki-mode-map "\C-c\C-h" 'mediawiki-insert-hline)
    (define-key mediawiki-mode-map [(meta f7)] 'mediawiki-draft)
    (define-key mediawiki-mode-map [(meta f8)] 'mediawiki-reply-at-point-simple)
    (define-key mediawiki-mode-map [(meta f9)] 'mediawiki-draft-view-draft)
    (define-key mediawiki-mode-map "\C-c\C-r" 'mediawiki-reply-at-point-simple)
    (define-key mediawiki-mode-map "\C-cr" 'mediawiki-draft-region)
    (define-key mediawiki-mode-map [(meta r)] 'mediawiki-draft-reply)
    (define-key mediawiki-mode-map "\C-c\C-m" 'mediawiki-draft)
    (define-key mediawiki-mode-map "\C-c\C-b" 'mediawiki-draft-region)
    (define-key mediawiki-mode-map "\C-c\C-d" 'mediawiki-draft-buffer)
    (define-key mediawiki-mode-map "\C-c\C-k" 'mediawiki-draft-buffer)
    (define-key mediawiki-mode-map "\C-c\C-p" 'mediawiki-draft-copy-page-to-register)
    (define-key mediawiki-mode-map "\C-c\C-c" 'mediawiki-draft-send)
    (define-key mediawiki-mode-map "\C-c\C-s" 'mediawiki-draft-yank-page-to-register)

    (define-key mediawiki-mode-map [(control meta prior)] 'mediawiki-enhance-indent)
    (define-key mediawiki-mode-map [(control meta next)] 'mediawiki-yank-prefix)
    (define-key mediawiki-mode-map [(meta return)] 'mediawiki-insert-enumerate)
    (define-key mediawiki-mode-map [(meta control return)] 'mediawiki-insert-enumerate-nonewline)
    ;; private setting
    (define-key mediawiki-mode-map [(shift return)] 'newline-and-indent)
    (define-key mediawiki-mode-map "\C-\\" 'mediawiki-insert-itemize)
    (define-key mediawiki-mode-map [(control return)] 'mediawiki-insert-itemize)
    (define-key mediawiki-mode-map "\C-ca" 'auto-capitalize-mode)
    (define-key mediawiki-mode-map "\C-ci" 'set-input-method)
    (define-key mediawiki-mode-map "\C-ct" 'toggle-input-method)

    (define-key mediawiki-mode-map [(backtab)] 'mediawiki-goto-prev-link)
    (define-key mediawiki-mode-map [(tab)]     'mediawiki-goto-next-link)
    (define-key mediawiki-mode-map "\M-g"      'mediawiki-reload)
    (define-key mediawiki-mode-map "\C-x\C-s"  'mediawiki-save)
    (define-key mediawiki-mode-map "\C-c\C-c"  'mediawiki-save-and-bury)
    (define-key mediawiki-mode-map "\C-x\C-w"  'mediawiki-save-as)
    (define-key mediawiki-mode-map "\C-c\C-o"  'mediawiki-open)
    (define-key mediawiki-mode-map "\M-p"
      'mediawiki-goto-previous-page)
    (define-key mediawiki-mode-map "\M-n"      'mediawiki-goto-next-page)
    (define-key mediawiki-mode-map [(control return)]
      'mediawiki-open-page-at-point)))

;; (defvar mw-pagelist-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (suppress-keymap map)
;;     (define-key map [(return)] 'mw-pl-goto-page-at-point)
;;     (define-key map "n"        'mw-pl-page-down)
;;     (define-key map "C-v"      'mw-pl-page-down)
;;     (define-key map [(next)]  'mw-pl-page-down)
;;     (define-key map "p"        'mw-pl-page-up)
;;     (define-key map "M-v"      'mw-pl-page-up)
;;     (define-key map [(prior)]  'mw-pl-page-up)))

;; (define-derived-mode mw-pagelist-mode special-mode "MW-PageList")

(provide 'mediawiki)

;; Local Variables:
;; time-stamp-pattern: "20/^;; Last Modified: <%%>$"
;; End:

;;; mediawiki.el ends here
