;;; golb.el --- A simple blog and webpage generator

;; Copyright (C) 2006  Jorgen Schaefer

;; Version: 1.0
;; Keywords: web
;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl/download/golb.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301  USA

;;; Commentary:

;; Golb is a very simple homepage and weblog generator. Basically it
;; provides a simple function which creates an html file from
;; templates for you, and possibly also a weblog index file and rss
;; feed.

;; When running M-x bolg in a file, this will create an associated
;; .html file. Your file itself should consist only of the body of a
;; real HTML file. Ideally, it should begin with the title of the page
;; in <h1>...</h1> tags, followed by a lead paragraph in <p>...</p>
;; tags. The file can be ended by a time stamp, which is not
;; considered to be part of the body itself. These parts are then
;; aggregated into an html file.

;; Files considered to be weblog files should be named YYYY-MM-DD.*.
;; They are treated like other files, except that they also cause an
;; index file and rss file to be generated.

;; To use it, put `golb' in your `after-save-hook'.

;; Well, read the docstrings and just try it.

;;; Code:

;; For the time stamp variables
(require 'time-stamp)

(defgroup golb nil
  "A simple home page and blog generator."
  :prefix "golb-"
  :group 'applications)

(defcustom golb-page-regexp (expand-file-name "~/public_html/.*\\.ht\\'")
  "*A regular expression matching files you want to golb."
  :type 'regexp
  :group 'golb)

(defcustom golb-weblog-regexp (expand-file-name "~/public_html/weblog/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].*\\.ht\\'")
  "*A regular expression matching files in your weblog."
  :type 'regexp
  :group 'golb)

(defcustom golb-weblog-file-regexp "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].*\\.ht\\'"
  "*A regular expression matching weblog file names.
This must not include the directory."
  :type 'regexp
  :group 'golb)

(defcustom golb-weblog-index-file-name "index.html"
  "*The file name for the weblog index file."
  :type 'string
  :group 'golb)

(defcustom golb-page-template-header "~/public_html/templates/header.html"
  "*The header template for pages.
This is prepended to all pages."
  :type 'file
  :group 'golb)

(defcustom golb-page-template-footer "~/public_html/templates/footer.html"
  "*The footer template for pages.
This is appended to all pages."
  :type 'file
  :group 'golb)

(defcustom golb-section-list nil
  "*A list of lists mapping directories to section names.
The first item is a regexp matched against the directory, while
the second item is the section name. If the directory is not
found here, underlines are replaced by spaces and words
capitalized."
  :type '(repeat (list regexp string))
  :group 'golb)

(defcustom golb-beautify-list '(("<current-year />" (lambda ()
                                                      (format-time-string "%Y")))
                                ("<current-time />" (lambda ()
                                                      (current-time-string)))
                                ("\\.\\.\\." "…")
                                ("---" "&#8212;")
                                ("\\([^!]\\)--\\([^>]\\)" "\\1&#8211;\\2"))
  "*A list of tags for pages.
This list consists of lists whose first element are a regular
expression, and whose second element are a replacement or a
function that returns a replacement."
  :type '(repeat (list regexp (choice function
                                      string)))
  :group 'golb)

(defcustom golb-weblog-index-full-entry-count 5
  "*How many entries the web log index page will list in the long format."
  :type 'integer
  :group 'golb)

(defcustom golb-weblog-index-template-header "~/public_html/templates/index-header.html"
  "*The header template for the weblog index page.
This is prepended to the weblog index page."
  :type 'file
  :group 'golb)

(defcustom golb-weblog-index-template-separator "~/public_html/templates/index-separator.html"
  "*The separator template for the weblog index page.
This is inserted between the full entries and the short entries
on the weblog index page."
  :type 'file
  :group 'golb)

(defcustom golb-weblog-index-template-footer "~/public_html/templates/index-footer.html"
  "*The footer template for the weblog index page.
This is appended to the weblog index page."
  :type 'file
  :group 'golb)

(defcustom golb-weblog-index-short-function 'golb-weblog-index-short
  "*The function to generate a short index entry.
It is passed a page. See `golb-parse-buffer' for a description of
pages."
  :type 'function
  :group 'golb)

(defcustom golb-weblog-index-long-function 'golb-weblog-index-long
  "*The function to generate a long index entry.
It is passed a page. See `golb-parse-buffer' for a description of
pages."
  :type 'function
  :group 'golb)

(defcustom golb-weblog-rss-file-name "index.rss"
  "*The file name for the rss feed."
  :type 'string
  :group 'golb)

(defcustom golb-weblog-rss-count 23
  "*The number of entries in the RSS feed."
  :type 'integer
  :group 'golb)

(defcustom golb-weblog-rss-title "My Little Weblog"
  "*The title of the weblog for the RSS feed."
  :type 'string
  :group 'golb)

(defcustom golb-weblog-rss-url "http://localhost/weblog/"
  "*The URL for the weblog for the RSS feed."
  :type 'string
  :group 'golb)

(defcustom golb-weblog-rss-description "The little weblog of me, myself and I."
  "*The weblog description for the RSS feed."
  :type 'string
  :group 'golb)

(defcustom golb-weblog-rss-language "en"
  "*The language of the weblog for the RSS feed."
  :type 'string
  :group 'golb)


;;;;;;;;;;;;;;;;;;;;;
;;; The main function

(defun golb ()
  "Generate web page stuff.
If the current file is a web page prototype (as per
`golb-page-regexp'), generate the page itself. If it's a weblog
file (as per `golb-weblog-regexp'), also generate the index and
rss files."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (and file
               (string-match golb-page-regexp file))
      (golb-generate-page)
      (when (string-match golb-weblog-regexp file)
        (golb-generate-weblog)))))


(put 'golb-with-file 'lisp-indent-function 1)
(defmacro golb-with-file (file &rest body)
  "Run BODY in a buffer visiting FILE.
If the buffer was not open before calling this macro, it's killed
afterwards."
  `(let* ((XXfile ,file)
          (XXexistsp (get-file-buffer XXfile))
          (XXbuf (find-file-noselect XXfile)))
     (unwind-protect
         (with-current-buffer XXbuf
           (save-excursion
             (save-restriction
               (widen)
               ,@body)))
       (when (not XXexistsp)
         (kill-buffer XXbuf)))))


;;;;;;;;;;;;;;;;;;;
;;; Page Generation

(defun golb-generate-page ()
  "Generate a single page."
  (interactive)
  (let ((page (golb-parse-buffer t)))
    (golb-with-file (golb-page-html-name page)
      (delete-region (point-min)
                     (point-max))
      (insert-file-contents golb-page-template-header)
      (goto-char (point-max))
      (insert (golb-page-body page))
      (insert-file-contents golb-page-template-footer)
      (golb-beautify-buffer page)
      (save-buffer))))


;;;;;;;;;;
;;; Weblog

(defun golb-generate-weblog ()
  "Generate the weblog in the current directory."
  (interactive)
  (let ((pages (golb-weblog-pages)))
    (golb-weblog-generate-index pages)
    (golb-weblog-generate-rss pages)))


;;;;;;;;;;;;;;;;
;;; Weblog Index

(defun golb-weblog-generate-index (pages)
  "Generate the index for the weblog.
PAGES is a list of pages in the weblog, as returned by
`golb-weblog-pages'. The `golb-weblog-index-file-name' will be written
with the last `golb-weblog-index-full-entry-count' entries in full, and
the remaining entries as headlines only. Also see
`golb-weblog-index-template-header', `golb-weblog-index-template-separator',
and `golb-weblog-index-template-footer'."
  (golb-with-file golb-weblog-index-file-name
    (delete-region (point-min)
                   (point-max))
    (insert-file-contents golb-weblog-index-template-header)
    (goto-char (point-max))
    (let ((count 0))
      (mapc (lambda (page)
              (when (= count golb-weblog-index-full-entry-count)
                (insert-file-contents golb-weblog-index-template-separator)
                (goto-char (point-max)))
              (if (< count golb-weblog-index-full-entry-count)
                  (funcall golb-weblog-index-long-function page)
                (funcall golb-weblog-index-short-function page))
              (setq count (+ count 1)))
            pages))
    (insert-file-contents golb-weblog-index-template-footer)
    (golb-beautify-buffer)
    (save-buffer)))

(defun golb-weblog-index-long (page)
  "Emit a long article entry.
This includes the intro."
  (let ((file-time (golb-page-file-time page)))
    (insert "<h3 class=\"article\">" (golb-page-title page) "</h3>\n"
            (format-time-string
             (format "<div class=\"articledate\">%%A, %%e%s of %%B %%Y</div>\n"
                     (let ((day (nth 3 (decode-time file-time))))
                       (cond
                        ((= day 1) "st")
                        ((= day 2) "nd")
                        ((= day 3) "rd")
                        (t "th"))))
             file-time)
            (golb-page-intro page) "\n"
            "<p><a href=\"" (golb-page-html-name page) "\">"
            "Read this article…</a></p>\n\n")))

(defun golb-weblog-index-short (page)
  "Emit a short article entry."
  (insert "  <li><a href=\"" (golb-page-html-name page) "\">"
          (format-time-string "%e. %B %Y" (golb-page-file-time page))
          ": " (golb-page-title page)
          "</li>\n"))


;;;;;;;;;;;;;;
;;; Weblog RSS

(defun golb-weblog-generate-rss (pages)
  "Generate the RSS file for the weblog.
PAGES is a list of pages in the weblog, as returned by
`golb-weblog-pages'. This will write the last `golb-weblog-rss-count'
pages as RSS entries to `golb-weblog-rss-file-name'"
  (golb-with-file golb-weblog-rss-file-name
    (delete-region (point-min)
                   (point-max))
    (golb-weblog-rss-emit pages)
    (golb-beautify-buffer)
    (save-buffer)))

(defun golb-weblog-rss-emit (pages)
  "Emit an RSS page."
  ;; This really ought to use something like SXML. I apologize to the
  ;; reader.
  (insert "<?xml version=\"1.0\"?>\n"
          "<!-- generator=\"golb.el\" -->\n"
          "<rss version=\"2.0\"\n"
          "     xmlns:content=\"http://purl.org/rss/1.0/modules/content/\"\n"
          "     xmlns:wfw=\"http://wellformedweb.org/CommentAPI/\"\n"
          "     xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n"
          "  <channel>\n"
          "    <title>" golb-weblog-rss-title "</title>\n"
          "    <link>" golb-weblog-rss-url "</link>\n"
          "    <description>" golb-weblog-rss-description "</description>\n"
          "    <pubDate>" (format-time-string "%a, %e %b %Y %T %z") "</pubDate>\n"
          "    <generator>http://nowhere-sadly/golb.el</generator>\n"
          "    <language>" golb-weblog-rss-language "</language>\n"
          "\n")
  (let ((count 0))
    (catch 'done
      (mapc (lambda (page)
              (when (>= count golb-weblog-rss-count)
                (throw 'done))
              (insert "    <item>\n"
                      "      <title>" (golb-page-title page) "</title>\n"
                      "      <link>" (golb-page-html-name page) "</link>\n"
                      ;; "      <comments>" "</comments>\n"
                      "      <pubDate>" (format-time-string "%a, %e %b %Y %T %z"
                                                            (golb-page-file-time page))
                      "</pubDate>\n"
                      ;; "      <category>" "</category>\n"
                      "      <guid isPermaLink=\"true\">" (concat golb-weblog-rss-url
                                                                  (golb-page-html-name page))
                      "</guid>\n"
                      "      <description><![CDATA["
                      (golb-page-intro page)
                      "]]></description>\n"
                      "    </item>\n"
                      ))
            pages)))
  (insert "  </channel>\n"
          "</rss>"))


;;;;;;;;;;;;;;;;
;;; Markup rules

(defun golb-beautify-buffer (&optional page)
  "Beautify this buffer according to `golb-beautify-list'.
If PAGE is given, the additional rules <page-title />,
<page-section /> and <page-time-stamp /> are available."
  (mapc (lambda (replacement)
          (let ((regex (car replacement))
                (string (cadr replacement)))
            (when (functionp string)
              (setq string (funcall string)))
            (when string
              (goto-char (point-min))
              (let ((case-fold-search nil))
                (while (re-search-forward regex nil t)
                  (replace-match string t))))))
        (append golb-beautify-list
                (when page
                  `(("<page-title />" ,(golb-page-title page))
                    ("<page-time-stamp />" ,(golb-page-time-stamp page))
                    ("<page-section />" ,(golb-page-section page)))))))


;;;;;;;;;;;;;;;;
;;; Weblog files

(defun golb-weblog-pages ()
  "Return a list of pages for the weblog
See `golb-page-buffer' for an explanation of pages."
  (reverse
   (mapcar 'golb-parse-file
           (directory-files "." nil golb-weblog-file-regexp))))

(defun golb-parse-file (file &optional bodyp)
  "Return a parsed FILE.
If BODYP is non-nil, the body is also included."
  (golb-with-file file
    (golb-parse-buffer bodyp)))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Single file parsing

(defun golb-parse-buffer (&optional bodyp)
  "Return a vector describing the current buffer.
A golb buffer consists of a title in <h1> tags, a paragraph in
<p> tags being the intro, and possibly a time stamp, and the
body, being the whole page except for the time stamp, but only if
BODYP is non-nil.

Use `golb-page-file-name', `golb-page-title', `golb-page-intro',
`golb-page-time-stamp', `golb-page-body', `golb-page-html-name',
`golb-page-file-time', and `golb-page-section' to work with this
vector."
  (let ((title nil)
        (intro nil)
        (time-stamp nil)
        (body nil))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "<h1>\\(.*?\\)</h1>" nil t)
          (setq title (match-string-no-properties 1))
        (error "This file does not have a title"
               (buffer-file-name)))
      (if (re-search-forward "<p>\\(.\\|\n\\)*?</p>" nil t)
          (setq intro (match-string-no-properties 0))
        (error "This file does not have any paragraph"
               (buffer-file-name)))
      (if (re-search-forward (format "%s.*%s"
                                     (regexp-quote time-stamp-start)
                                     (regexp-quote time-stamp-end))
                             nil t)
          (setq time-stamp (match-string-no-properties 0)
                body (when bodyp
                       (buffer-substring-no-properties (point-min)
                                                       (match-beginning 0))))
        (setq body (when bodyp
                     (buffer-string))))
      (vector (buffer-file-name)
              title
              intro
              time-stamp
              body))))

(defun golb-page-file-name (page)
  "Return the file name of PAGE."
  (aref page 0))

(defun golb-page-title (page)
  "Return the title of PAGE."
  (aref page 1))

(defun golb-page-intro (page)
  "Return the intro of PAGE."
  (aref page 2))

(defun golb-page-time-stamp (page)
  "Return the time stamp of PAGE."
  (aref page 3))

(defun golb-page-body (page)
  "Return body of PAGE."
  (aref page 4))

(defun golb-page-html-name (page)
  "Return the file name of the HTML file for PAGE."
  (concat (file-name-sans-extension
           (file-name-nondirectory
            (golb-page-file-name page)))
          ".html"))

(defun golb-page-file-time (page)
  "Return the time specified by the file name of PAGE."
  (let ((file (file-name-nondirectory (golb-page-file-name page))))
    (when (string-match "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)"
                        file)
      (encode-time 0 0 0
                   (string-to-number (match-string 3 file))
                   (string-to-number (match-string 2 file))
                   (string-to-number (match-string 1 file))))))

(defun golb-page-section (page)
  "Return the section of PAGE."
  (let ((file (golb-page-file-name page)))
    (when (string-match "/\\([^/]*\\)/[^/]*$" file)
      (golb-section (match-string 1 file)))))

(defun golb-section (section)
  "Translate section names.
Underscores are changed to spaces, and all words are
capitalized."
  (catch 'return
    (mapc (lambda (translation)
            (when (string-match (car translation)
                                section)
              (throw 'return (cadr translation))))
          golb-section-list)
    (with-temp-buffer
      (insert section)
      (goto-char (point-min))
      (while (search-forward "_" nil t)
        (replace-match " "))
      (capitalize-region (point-min)
                         (point-max))
      (buffer-string))))

(provide 'golb)
;;; golb.el ends here
