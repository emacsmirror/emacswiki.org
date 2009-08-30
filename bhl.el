;;; bhl.el --- From (P)lain text to (H)tml and (L)aTeX.
;; Time-stamp: <2006-01-18 17:16:50 guerry>
 
;; Copyright (C) 2002 2003 2004 2005 2006 Bastien Guerry

;; Emacs Lisp Archive Entry
;; Filename: bhl.el
;; Author: Bastien Guerry <bzg AT altern DOT org>
;; Maintainer: Bastien Guerry <bzg AT altern DOT org>
;; Version: 1.8rc2
;; Revised: 18/01/2006
;; Created: 16/11/2002
;; Keywords: convert plain text html latex sgml linuxdoc texinfo
;; Description: convert raw text into HTML/SGML/LaTeX/Texinfo/Text
;; URL: http://www.nongnu.org/bhl/

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; BHL mode is a mode wich enables you to convert plain text files into
;; HTML, LaTeX and SGML (Linuxdoc) files.  This is a simple mode, with
;; simple conversion functions, dedicated to simple source files.

;; Stacey Marshall <stacey DOT marshall AT sun DOT com> added the
;; support for bhl2wiki

;;; Neighbours:

;; emacs-wiki.el with latex-markup.el, html-markup.el and texinfo-markup.el
;; also muse-mode.el : see http://www.mwolson.org/projects/MuseMode.html
;; See the WikiDiscussion on http://www.emacswiki.org.

;;; Installation:

;; With `bhl.tar.gz':
;;   [your_shell]$ tar zxvf bhl-*.tar.gz
;;   [your_shell]$ make && make install
;;   [your_shell]$ make info && make install-info
;;   [your_shell]$ make pdf
;;   [your_shell]$ make html

;; With: `bhl.el':
;;   Just compile `bhl.el' and put it in your load-path.

;; Add to your `~/.emacs.el':
;;   (autoload 'bhl-mode "bhl" "BHL Mode" t)
;;   (add-to-list 'auto-mode-alist '("\\.bhl$" . bhl-mode))

;;; Usage:

;; `bhl2html'..................[C-c C-w]     -> convert BHL to HTML
;; `bhl2latex'.................[C-c C-l]     -> convert BHL to LaTeX
;; `bhl2sgml'..................[C-c C-s]     -> convert BHL to SGML (Linuxdoc)
;; `bhl2txt'...................[C-c C-d]     -> convert BHL to TXT
;; `bhl2wiki'..................[C-c C-k]     -> convert BHL to WIKI
;; `bhl2texinfo'...............[C-c C-o]     -> convert BHL to Texinfo

;; `bhl-insert-toc'............[C-c C-c t]   -> insert the table of contents
;; `bhl-insert-url'............[C-c C-c h]   -> insert an URL
;; `bhl-insert-image'..........[C-c C-c i]   -> insert an image
;; `bhl-insert-verbatim'.......[C-c C-c v]   -> insert a verbatim environment
;; `bhl-insert-minipage'.......[C-c C-c m]   -> insert a minipage
;; `bhl-insert-tab'............[M-TAB]       -> insert a rigid tab

;; `bhl-change-font-bold'......[C-c C-f C-b] -> change font (bold)
;; `bhl-change-font-emphasis'..[C-c C-f C-e] -> change font (emphasis)
;; `bhl-change-font-underline'.[C-c C-f C-u] -> change font (underline)
;; `bhl-change-font-truetype'..[C-c C-f C-t] -> change font (truetype)
;; `bhl-change-font-bolditalic'[C-c C-f C-_] -> change font (bold-italic)
;; `bhl-change-font-normal'....[C-c C-f C-n] -> change font (normal)

;; `bhl-show-toc'..............[C-c C-t]     -> generate a browsable table of contents
;; `bhl-update-toc'............[C-c M-t]     -> update the prefix labels of sections
;; `bhl-guess-style'...........[C-c M-s]     -> guess the sectioning style of buffer
;; `bhl-show-lol'..............[C-c C-/]     -> generate a browsable list of links

;; `bhl-goto-next-section'.....[C-c C-n]     -> go to the next section
;; `bhl-goto-previous-section'.[C-c C-p]     -> go to the previous section
;; `bhl-goto-next-url-or-wiki'.[C-TAB]       -> go to the next URL or WikiName

;; `bhl-show-version'..........[C-c C-v]     -> display the BHL mode version number

;;; Known problems:

;; * BHL can't update sections if there are only (sub)subsections.

;;; Acknowledgments:

;; Thanks to Thierry Stoehr, Serge Basterot, Christoph Conrad, Peter
;; Kindermann, Mario Lang, Tim Cross.  Special thanks to Daniel
;; P. Katz for the bhl2xxx-batch-wrapper function and other contribs.

;;; Summary:

;; Page 1  - Variables and constants
;; Page 2  - Custom
;; Page 3  - Faces
;; Page 4  - Mode
;; Page 5  - Menu
;; Page 6  - Miscellaneous
;; Page 7  - Table of content
;; Page 8  - Submit bug report
;; Page 9  - bhl2xxx

;;; Code:

(eval-when-compile
  (mapcar 'require '(custom cl easymenu derived mouse font-lock))
  (unless (featurep 'xemacs)
    (require 'texnfo-upd)
    (require 'footnote)))


;;;; 1 - CONSTANTS

(defconst bhl-startup-message-lines
  '("Please use \\[bhl-submit-bug-report] to report bugs."
    "BHL comes with ABSOLUTELY NO WARRANTY."
    "Thanks for using the BHL mode!")
  "Lines to be displayed with `bhl-show-version'.")

(defconst bhl-help-address "bastien1NOSPAM@free.fr"
  "The address of the current maintainer.")

;;;; Regexp constants

(defconst bhl-wiki-names-regexp
  "\\<[A-Z][a-z]+\\([A-Z][a-z]+\\)+\\>"
  "Match a WikiName.")

(defconst bhl-generic-url-regexp
  "\\(url:\\|https?://\\|file:///\\|telnet:\\|mime:\\|s?news:\\|ftp://\\|mailto:\\)\\([^ [\n\t<]+\\>/?\\)"
  "Match a generic URL.")

(defconst bhl-url-regexp
  (concat "\\(\\(" bhl-generic-url-regexp "\\)\\|\\[\\[\\(\\(mailto:\\)?\\([^]\n\t]+\\>/?\\)\\)\\]\\[\\(\\<[^]\t]+\\)\\]\\]\\)")
  "Match a BHL URL.  Designed to work with `bhl-convert-url'.

* When matching a raw URL:
    (match-string 0) : return the whole URL.
    (match-string 3) : return the prefix of the URL (http:// of ftp:// or ...).
    (match-string 4) : return the rest of the URL.

* When matching an URL put into square brackets:
    (match-string 0) : return the URL and its name into brackets.
    (match-string 5) : return only the URL.

  - If the URL is of the form: \"mailto:bastien1NOSPAM@free.fr\":
      (match-string 6) : return \"mailto:\".
      (match-string 7) : return the email.

  - Else:
      (match-string 7) : return the URL.

  (match-string 8) : return the name of the URL.")

(defconst bhl-img-regexp
  "\\[\\[\\([^] \n\t]+\.\\(BMP\\|EPS\\|GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|NG\\|PM\\|S\\)\\|TIFF\\|X\\(?:[BP]M\\)\\|bmp\\|eps\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|ng\\|pm\\|s\\)\\|tiff\\|x\\(?:[bp]m\\)\\)\\)\\]\\]"
  "Match images.")	   ; `image-file-name-regexp' does not suffice

(defconst bhl-title-regexp
  "\\`[ \t\n\r]*\\([^\n]+\\)$"
  "Match the title.")

(defconst bhl-hr-regexp
  "^\\([<>-]\\)---+\\(\\[\\(\\([0-9]+\\)[^] ]*\\)\\]\\)?"
  "Match a horizontal rule.")

(defconst bhl-hr1-regexp
  "^\\([<>=]\\)===+\\(\\[\\(\\([0-9]+\\)[^] ]*\\)\\]\\)?"
  "Match a horizontal rule.")

(defconst bhl-hr2-regexp
  "^\\([<>/]\\)///+\\(\\[\\(\\([0-9]+\\)[^] ]*\\)\\]\\)?"
  "Match a horizontal rule.")

(defconst bhl-minipage-regexp
  "^\\([<>%]\\)\\([-%]\\)%+\\(\\[\\([^] ]+\\)\\]\\)?"
  "Match a starting minipage environment.")

(defconst bhl-list-regexp
  "^\t\t?\t?\t?\t?\\([-*o]\\|[0-9]+\\.\\) "
  "Match lists items.")

(defconst bhl-descrip-regexp
  "^\\(\t+\\)\\([^\n-]+\\) -- "
  "Match items of lists of descriptions.")

(defconst bhl-list-regexp-list
  '(("^\\(\t\t\t\t\t\\)\\([-*o]\\|[0-9]+\.\\) "
     "^\t\t?\t?\t?\\([-*o]\\|[0-9]+\.\\) \\|^[^\n\t ]\\|\\'")
    ("^\\(\t\t\t\t\\)\\([-*o]\\|[0-9]+\.\\) "
     "^\t\t?\t?\\([-*o]\\|[0-9]+\.\\) \\|^[^\n\t ]\\|\\'")
    ("^\\(\t\t\t\\)\\([-*o]\\|[0-9]+\.\\) "
     "^\t\t?\\([-*o]\\|[0-9]+\.\\) \\|^[^\n\t ]\\|\\'")
    ("^\\(\t\t\\)\\([-*o]\\|[0-9]+\.\\) "
     "^\t\\([-*o]\\|[0-9]+\.\\) \\|^[^\n\t ]\\|\\'")
    ("^\\(\t\\)\\([-*o]\\|[0-9]+\.\\) "
     "^[^\n\t ]\\|\\'"))
  "A list of regexp that match list items of third, second and first level.")

(defconst bhl-list-syntax-alist
  '((html ("<ul>" "<li>" "</ul>")
	  ("<ol>" "<li>" "</ol>")
	  ("<dl>" "<dt>" "</dt><dd>" "</dl>"))
    (wiki ("" "   *" "")
	  ("" "   1" "")
	  ("" "   $ " ": " ""))
    (latex ("\\begin{itemize}" "\\\\item " "\\end{itemize}")
	   ("\\begin{enumerate}" "\\\\item " "\\end{enumerate}")
	   ("\\begin{description}" "\\item [" "] " "\\end{description}"))
    (sgml ("<p><itemize>" "<item>" "</itemize>")
	  ("<p><enum>" "<item>" "</enum>")
	  ("<p><descrip>" "<tag>" "</tag> " "</descrip>"))
    (texi ("@itemize @minus" "@item\n" "@end itemize")
	  ("@enumerate" "@item\n" "@end enumerate")
	  ("@table @emph" "@item " "\n" "@end table")))
  "Alist of syntactic elements composing unordered, ordered and description lists.")

;;; Miscellaneous variables

(defvar bhl-local-lol-list nil)
(defvar bhl-local-latex-class "")
(defvar bhl-local-latex-class-options nil)
(defvar bhl-local-html-style "")
(defvar bhl-local-lang "")
(defvar bhl-local-texi-titlepage-style "")

(defvar bhl-mode-map (make-keymap)
  "Keymap for the BHL major mode.")

(defvar bhl-toc-mode-map (make-keymap)
   "Keymap for the BHL toc mode.")

(defvar bhl-lol-mode-map (make-keymap)
   "Keymap for the BHL lol mode.")

(defvar bhl-version "1.7.1"
  "The current version of the BHL mode.")

(defvar bhl-popup-menu-map
  (if (current-local-map)
      (copy-keymap (current-local-map))
    (make-keymap))
  "A popup menu for BHL.")

(defvar bhl-toc-temporary-depth nil
  "The depth of toc set into the *toc* buffer.")

(defvar bhl-toc-point-list nil
  "The list of points used to browse the toc.")

(defvar bhl-tpl nil
  "The list of point positions corresponding to the list of (sub)sections.
See `bhl-tsl'.")

(defvar bhl-tsl nil
  "The list of sections required to build and browse the toc.
See `bhl-tpl'.")

(defvar bhl-font-lock-keywords nil
  "Keywords to be fontified in the BHL mode.")

(defvar bhl-conversion-log nil
  "This variable stores the log for each conversion.")

(defvar bhl-xxx-conversions-list nil
  "Alist of conversion functions.")

(defvar bhl-tag-regexp-list nil
  "Tags elements understood by BHL.")

(defconst bhl-tag-regexp-list0
  '("[^\\]\\(__\\([^ \t\r\n\"_][^_]*\\)__\\)"
    "[^\\]\\(==\\([^ \t\r\n=\"][^=]*\\)==\\)"
    "[^_\\]\\(_\\([^ \t\r\n_\"][^_]*\\)_\\)[^_]"
    "[^\\]\\(\\*\\([^ \t\r\n*\"][^*]*\\)\\*\\)")
  "Tags elements understood by BHL.")

(defconst bhl-tag-regexp-list1
  '("[^\\]\\(__\\([^ \t\r\n\"_][^\n\r_]*\\)__\\)"
    "[^\\]\\(==\\([^ \t\r\n=\"][^\n\r=]*\\)==\\)"
    "[^_\\]\\(_\\([^ \t\r\n_\"][^\n\r_]*\\)_\\)[^_]"
    "[^\\]\\(\\*\\([^ \t\r\n*\"][^\n\r*]*\\)\\*\\)")
  "Tags elements understood by BHL.")

(defconst bhl-tag-syntax-alist
  '((html ("<u>" "</u>") ("<tt>" "</tt>")
	  ("<em>" "</em>") ("<b>" "</b>"))
    (latex ("\\underline{" "}") ("\\texttt{" "}")
	   ("\\emph{" "}") ("\\textbf{" "}"))
    (sgml ("<it>" "</it>") ("<tt>" "</tt>")
	  ("<em>" "</em>") ("<bf>" "</bf>"))
    (texi ("@i{" "}") ("@t{" "}")
	  ("@emph{" "}") ("@strong{" "}"))
    (wiki ("<u>" "</u>") ("<tt>" "</tt>")
	  ("<em>" "</em>") ("*" "*"))
    (txt ("" "") ("" "")
	 ("" "") ("" "")))
  "Alist of font beautifiers tags corresponding to each conversion format.")

(defconst bhl-section-syntax-alist
  '((html ("<h1>" "</h1>") ("<h2>" "</h2>") ("<h3>" "</h3>"))
    (latex ("\\section{" "}") ("\\subsection{" "}")
	   ("\\subsubsection{" "}"))
    (latex-nonum ("\\section\*{" "}") ("\\subsection\*{" "}")
		 ("\\subsubsection\*{" "}"))
    (sgml ("<sect>" "") ("<sect1>" "") ("<sect2>" ""))
    (texi ("@chapter " "") ("@section " "") ("@subsection " ""))
    (wiki ("---+ " "") ("---++ " "") ("---+++ " ""))
    (txt ("= " " =") ("== " " ==") ("=== " " ===")))
  "Alist of sections tags corresponding  to each conversion format.")

(defvar bhl-sectioning-regexp-list nil
  "Alist of regexp that match sections and subsections.
the first element matches the prefix of a sections title.
the second element matches the prefix of a subsections title.
the third element matches the prefix of a subsections title.")

;; punctuation marks

(defconst bhl-fr-punctuation
  '(("\\(:\\)[ \t\n]" "&nbsp;:" "~:" "@ :")
    ("\\(\\?\\)[ \t\n]" "&nbsp;?" "~?" "@ ?")
    ("\\(!\\)[ \t\n]" "&nbsp;!" "~!" "@ !")
    ("\\(;\\)[ \t\n]" "&nbsp;;" "~;" "@ ;"))
  "A list of punctuation marks respecting the french typographic conventions.")

(defconst bhl-de-punctuation nil
  "A list of punctuation marks respecting the german typographic conventions.")

(defconst bhl-en-punctuation nil
  "A list of punctuation marks respecting the english typographic conventions.")

;; Quotation marks

(defconst bhl-en-quotation-marks
  '(("[ \'\n\t\(]\\(«\\)"  "&ldquo;" "``" "``")
    ("[ \'\n\t\(]\\(\"\\)" "&ldquo;" "``" "``")
    ("\\(»\\)[ \n\t,\.:;\?!\)]" "&rdquo;" "''" "''")
    ("\\(\"\\)[ \n\t,\.:;\?!\)]" "&rdquo;" "''" "''"))
  "A list of quote chars to convert.")

(defconst bhl-de-quotation-marks
  '(("[ \'\n\t\(]\\(«\\)"  "&ldquo;" "``" "``")
    ("[ \'\n\t\(]\\(\"\\)" "&ldquo;" "``" "``")
    ("\\(»\\)[ \n\t,\.:;\?!\)]" "&rdquo;" "''" "''")
    ("\\(\"\\)[ \n\t,\.:;\?!\)]" "&rdquo;" "''" "''"))
  "A list of quote chars to convert.")
    
(defconst bhl-fr-quotation-marks
  '(("[ \'\n\t\(]\\(«\\)" "&laquo;&nbsp;" "«~" "``")
    ("[ \'\n\t\(]\\(\"\\)" "&laquo;&nbsp;" "«~" "``")
    ("\\(»\\)[ \n\t,\.:;\?!\)]" "&nbsp;&raquo;" "~»" "''")
    ("\\(\"\\)[ \n\t,\.:;\?!\)]" "&nbsp;&raquo;" "~»" "''"))
  "A list of quote characters to convert.")
    
;; Special characters

(defconst bhl-fr-special-chars
  '(("<"  "&lsaquo;" "<" "<")
    (">"  "&rsaquo;" ">" ">")
    ("oe" "&oelig;" "\\oe{}" "@oe{}")
    ("OE" "&OElig;" "\\OE{}" "@OE{}")
    ("ae" "&aelig;" "\\ae{}" "@ae{}")
    ("AE" "&AElig;" "\\AE{}" "@EA{}"))
  "A list of strings to convert into french ligatured characters.")

(defconst bhl-de-special-chars
  '(("<"  "&lsaquo;" "<" "<")
    (">"  "&rsaquo;" ">" ">")
    ("oe" "&ouml;" "ö" "@\"o")
    ("Oe" "&Ouml;" "Ö" "@\"O")
    ("ae" "&auml;" "ä" "ae")
    ("Ae" "&Auml;" "Ä" "Ae")
    ("Ue" "&Uuml;" "Ü" "Ue")
    ("ß"  "&szlig;" "ß" "@ss{}"))
  "A list of strings to convert into german ligatured characters.")

(defconst bhl-en-special-chars
  '(("<"  "&lsaquo;" "<" "<")
    (">"  "&rsaquo;" ">" ">"))
  "A list of strings to convert into english ligatured characters.")

(defconst bhl-latex-escaped-chars
  '("$" "&" "%" "{" "}"))

(defconst bhl-texi-escaped-chars
  '("@" "{" "}"))

(defconst bhl-escapable-chars
  '("*" "_" "#" "\\"))


;;;; 2 - CUSTOM

(defgroup bhl nil
  "BHL mode: convert plain TXT to HTML, LaTeX and SGML."
  :group 'wp
  :link '(custom-manual "(bhl)Top")
  :link '(url-link "http://www.nongnu.org/bhl")
  :prefix "bhl-")

(defgroup bhl-wiki nil
  "BHL as a local wiki."
  :group 'bhl)

(defgroup bhl2html nil
  "Customize the `bhl2html' output."
  :group 'bhl)

(defgroup bhl2latex nil
  "Customize the `bhl2latex' output."
  :group 'bhl)

(defgroup bhl2texinfo nil
  "Customize the `bhl2texinfo' output."
  :group 'bhl)

(defgroup bhl2sgml nil
  "Customize the `bhl2sgml' output."
  :group 'bhl)

(defgroup bhl2txt nil
  "Customize the `bhl2txt' output."
  :group 'bhl)

(defgroup bhl2wiki nil
  "Customize the `bhl2wiki' output."
  :group 'bhl)

(defgroup bhl-toc nil
  "How to handle the table of contents."
  :group 'bhl)

(defgroup bhl-tables nil
  "How BHL must build tables."
  :group 'bhl)

(defgroup bhl-faces nil
  "*BHL faces used in BHL mode."
  :group 'faces
  :group 'bhl)

;;;; Main custom

(defcustom bhl-mode-hook '(turn-on-auto-fill)
  "Normal hook run when entering BHL mode."
  :type '(hook)
  :group 'bhl)

(defcustom bhl-after-conversion-hook '(bhl-initialize-properties)
  "Hook run after any conversion.
The default hook is `bhl-initialize-properties'.
If you remove this function, the conversion properties of the
last converted buffer will be the default conversion properties
for the next converted buffer."
  :type '(hook)
  :group 'bhl)

(defcustom bhl-autoguess-style-flag t
  "Non-nil means autoguess the sectioning style.
When you find a file, BHL tries to guess its sectioning style
and sets the value of `bhl-sectioning-default-style' to the value
of the file's style.

See `bhl-guess-style' and `bhl-sectioning-default-style'."
  :type '(boolean)
  :link '(custom-manual "(bhl)Sections")
  :group 'bhl)

(defcustom bhl-sectioning-default-style 'num
  "The style of sections and (sub)subsections in the BHL file.
NUM indicates numerical style (e.g. \"1.2.\").
ALPHA indicates alphabetical style (e.g. \"A.B.\").
ASTER indicates that you use asteriks (e.g. \"*\").
EQUAL-SIGN indicates that you use equal-sign (e.g. \"=\").
MY indicates your own style, as defined by `bhl-my-sectioning-regexp-list'."
  :type '(radio (const :tag "Numerical" num)
		(const :tag "Alphabetical" alpha)
		(const :tag "Asterisks" aster)
		(const :tag "Equal signs" equal-sign)
		(const :tag "My own style" my))
  :link '(custom-manual "(bhl)Sections")
  :group 'bhl)

(defcustom bhl-my-sectioning-regexp-list
  '("[0-9]+\\. " "[0-9]+\\.[0-9]+\\. " "[0-9]+\\.[0-9]+\\.[0-9]+\\. ")
  "Your own list of regexp that match (sub)sections' prefix.

The default value for this list is equal to the value of
`bhl-sectioning-regexp-list'.

Please pay a special attention to whitespaces.
Don't use any subexpression.
Don't use the \{.\} construct.

If you want to choose your own list for the current sectioning
style, set `bhl-sectioning-default-style' to 'my.  Remind that you cannot use
`bhl-update-toc' anymore if you select your own sectioning style."
  :type '(list (regexp :tag "Section      ")
	       (regexp :tag "Subsection   ")
	       (regexp :tag "Subsubsection"))
  :link '(custom-manual "(bhl)Sections")
  :group 'bhl)

(defcustom bhl-verbatim-ignore
  '(tag comment list description table url wikiname
	images special-char tex-label footnote quote)
  "A list of non-converted elements inside verbatim regions.
Here are the relevant symbols that you can insert in this list:

  tag          : *word* and the like.
  comment      : #comment stings
  list         : * lists
  description  : Description --
  table        : | tables |
  url          : any kind of URL
  wikiname     : WikiNames
  images       : [[image.jpg]]
  special-char : e.g. ligatured \"oe\"
  tex-label    : LaTeX, TeX and LaTeX2e
  footnote     : footnote like this one[1]
  quote        : [tab]A quoted sentence"
  :type '(repeat (symbol :tag "Ignore "))
  :link '(custom-manual "(bhl)Environments")
  :group 'bhl)

(defcustom bhl-ignored-regexps nil
  "A list of regexps.
The lines matching these regexps are automatically skipped."
  :type '(repeat (regexp))
  :group 'bhl)

(defcustom bhl-tags-overlap-flag t
  "Non-nil means that you can use tags on multiple lines."
  :type '(boolean)
  :group 'bhl)

(defcustom bhl-i18n-conventions '("en" t t t)
  "English, french and german conventions.
These conventions are relative to the punctuation,
the quotation marks and some special characters."
  :type '(list (radio :tag "Language"
		      (const :tag "English" "en")
		      (const :tag "French" "fr")
		      (const :tag "German" "de"))
	       (boolean :tag "Punctuation     " :indent 2
			:help-echo "Non-nil means follow punctuation conventions.")
	       (boolean :tag "Quotation marks " :indent 2
			:help-echo "Non-nil means convert quotation marks.")
	       (boolean :tag "Special chars   " :indent 2
			:help-echo "Non-nil means convert special characters."))
  :link '(custom-manual "(bhl)Global options")
  :group 'bhl)

(defcustom bhl-browse-url-function 'browse-url
  "Function to call to browse a URL."
  :type '(function)
  :group 'bhl)

(defcustom bhl-tab-width 3
  "*The default width of the tab character."
  :type '(integer)
  :group 'bhl)

;; BHL as local Wiki

(defcustom bhl-is-a-local-wiki-flag nil
  "Non-nil means that MixedCase words are wikinames."
  :type '(boolean)
  :group 'bhl-wiki)

(defcustom bhl-default-wikifiles-extension ".bhl"
  "The default extension for new files created when following a wiki name."
  :type '(string :tag "Extension ")
  :group 'bhl-wiki)

(defcustom bhl-downcase-wikifiles-names-flag nil
  "Non-nil means that BHL downcases the name of a wiki file."
  :type '(boolean)
  :group 'bhl-wiki)

(defcustom bhl-non-wiki-names-list nil
  "A list of strings that are NOT WikiNames."
  :type '(repeat (string :tag "Non-wiki name "))
  :group 'bhl-wiki)

;;;; Customize the table of contents

(defcustom bhl-default-toc-depth 3
  "*The default depth of the table of contents."
  :type '(integer)
  :link '(custom-manual "(bhl)The table of contents")
  :group 'bhl-toc)

(defcustom bhl-intro-toc "--- Table of contents"
  "*A string inserted just before the table of contents."
  :type '(string)
  :group 'bhl-toc)

(defcustom bhl-end-toc "--- End of the table of contents"
  "*A string inserted just after the table of contents."
  :type '(string)
  :group 'bhl-toc)

(defcustom bhl-toc-location "top"
  "*The place into which the table of contents is inserted."
  :type '(radio (const :tag "Juste after title" "top")
		(const :tag "Juste before end" "bottom")
		(other :tag "On the point" "point"))
  :link '(custom-manual "(bhl)The table of contents")
  :group 'bhl-toc)

;;;; Customize the conversion of tables

(defcustom bhl-table-location "htbp"
  "*The location of the table.
\"here top bottom page\" means put the table here or at the top
of the page or at the bottom of the page or on a separate page."
  :type '(radio (const :tag "here" "h")
		(const :tag "top" "t")
		(const :tag "bottom" "b")
		(const :tag "page" "p")
		(other :tag "here top bottom page" "htbp"))
  :group 'bhl-tables)

(defcustom bhl-table-align "center"
  "*How to align each table."
  :type '(radio (const "left")
		(const "center")
		(const "right"))
  :group 'bhl-tables)

(defcustom bhl-table-cell-align "c"
  "*How to align each cell in a table."
  :type '(radio (const :tag "left" "l")
		(const :tag "center" "c")
		(other :tag "right" "r"))
  :group 'bhl-tables)

;;;; Customize the HTML output.

(defcustom bhl-after-html-conversion-hook nil
  "Hook run after the conversion into HTML.
This hook is run just before `bhl-after-conversion-hook'."
  :type '(hook)
  :group 'bhl2html)

(defcustom bhl2html-properties-list '(t t nil t t)
  "List of properties of the `bhl2html' conversion."
  :type '(list
	  (boolean :tag "Check dubious tags         ")
	  (boolean :tag "Insert sections' prefix    ")
	  (boolean :tag "Ask caption for tables     ")
	  (boolean :tag "Use i18n conventions       ")
	  (boolean :tag "Convert toc                "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2html)

(defcustom bhl-html-conversions-list '(t t t t t t t t nil t t t t t)
  "*A list of conversion functions to perform with `bhl2html'.
Each element of this list corresponds to a conversion function."
  :type '(list
	  (boolean :tag "Convert verbatim regions ")
	  (boolean :tag "Convert horizontal rules ")
	  (boolean :tag "Convert images           ")
	  (boolean :tag "Convert URLs             ")
	  (boolean :tag "Convert definition lists ")
	  (boolean :tag "Convert normal list      ")
	  (boolean :tag "Convert tables           ")
	  (boolean :tag "Convert LaTeX labels     "
		   :help-echo "Convert \"LaTeX\", \"LaTeX2e\" and \"TeX\" properly.")
	  (boolean :tag "Convert footnotes        ")
	  (boolean :tag "Convert minipage         "
		   :help-echo "Minipages are tables in HTML and minipages in LaTeX.")
	  (boolean :tag "Convert comments         ")
	  (boolean :tag "Convert escape sequences ")
	  (boolean :tag "Convert WikiNames        ")
	  (boolean :tag "Convert quote            "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2html)

(defcustom bhl-html-title-tags '("<h1 align=\"center\">" "</h1>")
  "A list of the opening and closing HTML tags for the title."
  :type '(list (string :tag "Opening tag")
	       (string :tag "Closing tag"))
  :group 'bhl2html)

(defcustom bhl-html-subtitle-tags '("<h2 align=\"center\">" "</h2>")
  "A list of the opening and closing HTML tags for the subtitle."
  :type '(list (string :tag "Opening tag")
	       (string :tag "Closing tag"))
  :group 'bhl2html)

(defcustom bhl-html-meta-alist '(("generator" . "bhl2html"))
  "*A list of META tags.
The first field is the value of \"NAME\".
The second field is the value of \"CONTENT\".

Example:

author    (in the first field [key])
Monique   (in the second field [value])

outputs

<meta name=\"author\" content=\"Monique\">"
  :type '(alist :key-type
		(string
		 :tag "Name"
		 :help-echo "Insert the \"NAME\" value of the META tag.")
		:value-type
		(string
		 :tag "Content"
		 :help-echo "Insert the \"CONTENT\" value of the META tag."))
  :group 'bhl2html)

(defcustom bhl-html-link-alist '(("generator-home" . "http://www.nongnu.org/bhl"))
  "*A list of LINK tags.
The first field is the value of \"REL\".
The second field is the value of \"HREF\".

Example:

alternate    (in the first field [key])
indexbis.html   (in the second field [value])

outputs

<link rel=\"alternate\" href=\"indexbis.html\">"
  :type '(alist :key-type
		(string
		 :tag "REL"
		 :help-echo "Insert the \"REL\" value of the LINK tag.")
		:value-type
		(string
		 :tag "HREF"
		 :help-echo "Insert the \"HREF\" value of the LINK tag."))
  :group 'bhl2html)

(defcustom bhl-html-doctype
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">"
  "*The DOCTYPE description of the HTML document."
  :type '(string)
  :group 'bhl2html)

(defcustom bhl-html-content-type
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">"
  "*The META tag which describes the content type of the HTML document."
  :type '(string)
  :group 'bhl2html)

(defcustom bhl-html-default-style ""
  "*The style to be inserted in the header of the HTML output."
  :type '(string)
  :group 'bhl2html)

(defcustom bhl-html-table-param-alist
  '(("cellpadding" . "3")
    ("cellspacing" . "0")
    ("border" . "1"))
  "*Alist of parameters for the conversion of tables.
The first field is the name of the parameter, the second field
specifies its value.

Example:

\"cellpadding\" (in the first field [key])
\"3\"   (in the second field [value])

outputs

<table cellpadding=\"3\" ...>"
  :type '(alist :key-type (string
			   :tag "Property"
			   :help-echo "Insert the property name.")
		:value-type (string
			     :tag "Value"
			     :help-echo "Insert the value of the property."))
  :group 'bhl2html)

(defcustom bhl-html-para-align "none"
  "*How to align paragraphs."
  :type '(radio (const "left")
		(const "center")
		(const "justify")
		(const "right")
		(const "none"))
  :group 'bhl2html)

(defcustom bhl-html-img-align "center"
  "*How to align each image."
  :type '(radio (const "left")
		(const "center")
		(const "right"))
  :group 'bhl2html)

(defcustom bhl-html-list-item-is-para-flag t
  "*Non-nil means that a list item is a paragraph."
  :type '(boolean)
  :group 'bhl2html)

(defcustom bhl-html-footer
  ""
  "*The footer is to be inserted at the end of the HTML output."
  :type '(string)
  :group 'bhl2html)

;;;; Customize the LaTeX output.

(defcustom bhl-after-latex-conversion-hook nil
  "Hook run after the conversion into LaTeX.
This hook is run just before `bhl-after-conversion-hook'."
  :type '(hook)
  :group 'bhl2latex)

(defcustom bhl2latex-properties-list '(t t nil t t)
  "List of properties of the `bhl2latex' conversion."
  :type '(list
	  (boolean :tag "Check dubious tags         ")
	  (boolean :tag "Insert sections' prefix    ")
	  (boolean :tag "Ask caption for tables     ")
	  (boolean :tag "Use i18n conventions       ")
	  (boolean :tag "Convert toc                "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2latex)

(defcustom bhl-latex-conversions-list
  '(t t t t t t t t nil t t t t t)
  "*A list of conversion functions to perform with `bhl2latex'.
Each element of this list corresponds to a conversion function."
  :type '(list
	  (boolean :tag "Convert verbatim regions ")
	  (boolean :tag "Convert horizontal rules ")
	  (boolean :tag "Convert images           ")
	  (boolean :tag "Convert URLs             ")
	  (boolean :tag "Convert definition lists ")
	  (boolean :tag "Convert normal list      ")
	  (boolean :tag "Convert tables           ")
	  (boolean :tag "Convert LaTeX labels     "
		   :help-echo "Convert \"LaTeX\", \"LaTeX2e\" and \"TeX\" properly.")
	  (boolean :tag "Convert footnotes        ")
	  (boolean :tag "Convert minipage         "
		   :help-echo "Minipages are tables in HTML and minipages in LaTeX.")
	  (boolean :tag "Convert comments         ")
	  (boolean :tag "Convert escape sequences ")
  	  (boolean :tag "Convert WikiNames        ")
  	  (boolean :tag "Convert quote            "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2latex)

(defcustom bhl-latex-packages-alist
  '(("inputenc" . "latin1")
    ("fontenc" . "T1")
    ("url" . ""))
  "*Alist of packages to be included in the LaTeX header.
The first field is the name of the package, the second field
specifies optional elements.

Example:

inputenc (in the first field [key])
latin1   (in the second field [value])

outputs

\\usepackage[latin1]{inputenc}"
  :type '(alist :key-type (string
			   :tag "Package"
			   :help-echo "Insert the name of the LaTeX package.")
		:value-type (string
			     :tag "Options"
			     :help-echo "Insert the values of the package options."))
  :group 'bhl2latex)

(defcustom bhl-latex-default-class "article"
  "*The document's class for the LaTeX output."
  :type '(string)
  :group 'bhl2latex)

(defcustom bhl-latex-default-class-options '("12pt" "a4paper")
  "*The default options for the \documentclass command in the LaTeX output."
  :type '(repeat (string :tag "Option "))
  :group 'bhl2latex)

(defcustom bhl-latex-extra-preambles nil
  "*A list of lines to be included in the LaTeX header."
  :type '(repeat (string :tag "Preamble line"))
  :group 'bhl2latex)
 
(defcustom bhl-latex-extra-body nil
  "*A list of lines to be included before the first section."
  :type '(repeat (string :tag "Pre-body line"))
  :group 'bhl2latex)

(defcustom bhl-latex-footer
  ""
  "*The footer is to be inserted at the end of the HTML output."
  :type '(string)
  :group 'bhl2latex)

;; Customize the SGML output

(defcustom bhl-after-sgml-conversion-hook nil
  "Hook run after the conversion into SGML.
This hook is run just before `bhl-after-conversion-hook'."
  :type '(hook)
  :group 'bhl2sgml)

(defcustom bhl2sgml-properties-list '(t nil t)
  "List of properties of the `bhl2sgml' conversion."
  :type '(list
	  (boolean :tag "Check dubious tags      ")
	  (boolean :tag "Ask caption for tables  ")
	  (boolean :tag "Use i18n conventions    "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2sgml)

(defcustom bhl-sgml-conversions-list
  '(t t t t t t t t nil t t t t t)
  "*A list of conversion functions to perform with `bhl2sgml'.
Each element of this list corresponds to a conversion function."
  :type '(list
	  (boolean :tag "Convert verbatim regions ")
	  (boolean :tag "Convert horizontal rules ")
	  (boolean :tag "Convert images           ")
	  (boolean :tag "Convert URLs             ")
	  (boolean :tag "Convert definition lists ")
	  (boolean :tag "Convert normal list      ")
	  (boolean :tag "Convert tables           ")
	  (boolean :tag "Convert LaTeX labels     "
		   :help-echo "Convert \"LaTeX\", \"LaTeX2e\" and \"TeX\" properly.")
	  (boolean :tag "Convert footnotes        ")
	  (boolean :tag "Convert minipage         "
		   :help-echo "Minipages are tables in HTML and minipages in LaTeX.")
	  (boolean :tag "Convert comments         ")
	  (boolean :tag "Convert escape sequences ")
	  (boolean :tag "Convert WikiNames        ")
	  (boolean :tag "Convert quote            "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2sgml)

(defcustom bhl-sgml-minipage-tag "<code>"
  "The style of any minipage in the SGML output.
The default value is \"<code>\".  This means that a
minipage is formatted as an example of code."
  :type '(string)
  :group 'bhl2sgml)

(defcustom bhl-sgml-list-item-is-para-flag t
  "*Non-nil means that a list item is a paragraph."
  :type '(boolean)
  :group 'bhl2sgml)

(defcustom bhl-sgml-footer
  ""
  "*The footer is to be inserted at the end of the HTML output."
  :type '(string)
  :group 'bhl2sgml)

;; Customize the TEXI output

(defcustom bhl-after-texi-conversion-hook
  '(texinfo-every-node-update
    bhl-texi-make-menu)
  "Hook run after the conversion into TEXI.
This hook is run just before `bhl-after-conversion-hook'."
  :type '(hook)
  :group 'bhl2texinfo)

(defcustom bhl2texi-properties-list '(t t t)
  "List of properties of the `bhl2texinfo' conversion."
  :type '(list (boolean :tag "Check dubious tags         ")
	       (boolean :tag "Use i18n conventions       ")
	       (boolean :tag "Convert toc                "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2texinfo)

(defcustom bhl-texi-conversions-list
  '(t t t t t t nil t t t t t t t)
  "*A list of conversion functions to perform with `bhl2html'.
Each element of this list corresponds to a conversion function."
  :type '(list
	  (boolean :tag "Convert verbatim regions ")
	  (boolean :tag "Convert horizontal rules ")
	  (boolean :tag "Convert images           ")
	  (boolean :tag "Convert URLs             ")
	  (boolean :tag "Convert definition lists ")
	  (boolean :tag "Convert normal list      ")
	  (boolean :tag "Convert tables           ")
	  (boolean :tag "Convert LaTeX labels     "
		   :help-echo "Convert \"LaTeX\", \"LaTeX2e\" and \"TeX\" properly.")
	  (boolean :tag "Convert footnotes        ")
	  (boolean :tag "Convert minipage         "
		   :help-echo "Minipages are tables in HTML and minipages in LaTeX.")
	  (boolean :tag "Convert comments         "
		   :help-echo "In TEXI output, converting comments means deleting them.")
	  (boolean :tag "Convert escape sequences ")
	  (boolean :tag "Convert WikiNames        ")
	  (boolean :tag "Convert quote            "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2texinfo)

(defcustom bhl-texi-setchapternewpage-flag "on"
  "Non-nil means that chapter are inserted into a new page."
  :type '(radio (const "on")
		(const "off"))
  :group 'bhl2texinfo)

(defcustom bhl-texi-titlepage-style 'classical
  "The style of the titlepage in the TEXI output."
  :type '(radio (const :tag "Classical" classical)
                (const :tag "Centered" centered))
  :group 'bhl2texinfo)

(defcustom bhl-texi-centered-minipage-tag "display"
  "The style of the centered minipage in the TEXI output.
The default value is \"display\".  This means that a so-called
centered minipage is formatted as an example without special font."
  :type '(string)
  :group 'bhl2texinfo)

(defcustom bhl-texi-table-tag "verbatim"
  "The tag for the conversion of tables in the TEXI output.
The default value is \"verbatim\".  If you want to skip tables
when converting into TEXI, you should set this to \"ignore\"."
  :type '(radio (const "verbatim")
		(const "ignore"))
  :group 'bhl2texinfo)

(defcustom bhl-texi-footer
  ""
  "*The footer is to be inserted at the end of the HTML output."
  :type '(string)
  :group 'bhl2texinfo)

;; Customize the TXT output

(defcustom bhl-after-txt-conversion-hook '(bhl-tab-to-spc)
  "Hook run after the conversion into TXT.
This hook is run just before `bhl-after-conversion-hook'."
  :type '(hook)
  :group 'bhl2txt)

(defcustom bhl2txt-properties-list '(t t t)
  "List of properties of the `bhl2txt' conversion."
  :type '(list (boolean :tag "Center title and sections ")
	       (boolean :tag "Convert toc               ")
	       (boolean :tag "Convert tabs to spaces    "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2txt)

(defcustom bhl-txt-conversions-list
  '(t t t t nil nil t nil nil t t t t nil)
  "*A list of conversion functions to perform with `bhl2html'.
Each element of this list corresponds to a conversion function."
  :type '(list
	  (boolean :tag "Convert verbatim regions ")
	  (boolean :tag "Convert horizontal rules ")
	  (boolean :tag "Convert images           ")
	  (boolean :tag "Convert URLs             ")
	  (boolean :tag "Convert definition lists ")
	  (boolean :tag "Convert normal list      ")
	  (boolean :tag "Convert tables           ")
	  (boolean :tag "Convert LaTeX labels     "
		   :help-echo "Convert \"LaTeX\", \"LaTeX2e\" and \"TeX\" properly.")
	  (boolean :tag "Convert footnotes        ")
	  (boolean :tag "Convert minipage         "
		   :help-echo "Minipages are tables in HTML and minipages in LaTeX.")
	  (boolean :tag "Convert comments         "
		   :help-echo "In TXT output, converting comments means deleting them.")
	  (boolean :tag "Convert escape sequences ")
	  (boolean :tag "Convert WikiNames        ")
	  (boolean :tag "Convert quote            "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2txt)

;; Customize the WIKI output

(defcustom bhl-after-wiki-conversion-hook '(bhl-tab-to-spc)
  "Hook run after the conversion into WIKI.
This hook is run just before `bhl-after-conversion-hook'."
  :type '(hook)
  :group 'bhl2wiki)

(defcustom bhl2wiki-properties-list '(t t t)
  "List of properties of the `bhl2wiki' conversion."
  :type '(list (boolean :tag "Insert Sections' Prefix   ")
	       (boolean :tag "Convert toc               ")
	       (boolean :tag "Convert tabs to spaces    "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2wiki)

(defcustom bhl-wiki-conversions-list
  '(t t t t nil nil t nil nil t t t t nil)
  "*A list of conversion functions to perform with `bhl2html'.
Each element of this list corresponds to a conversion function."
  :type '(list
	  (boolean :tag "Convert verbatim regions ")
	  (boolean :tag "Convert horizontal rules ")
	  (boolean :tag "Convert images           ")
	  (boolean :tag "Convert URLs             ")
	  (boolean :tag "Convert definition lists ")
	  (boolean :tag "Convert normal list      ")
	  (boolean :tag "Convert tables           ")
	  (boolean :tag "Convert LaTeX labels     "
		   :help-echo "Convert \"LaTeX\", \"LaTeX2e\" and \"TeX\" properly.")
	  (boolean :tag "Convert footnotes        ")
	  (boolean :tag "Convert minipage         "
		   :help-echo "Minipages are tables in HTML and minipages in LaTeX.")
	  (boolean :tag "Convert comments         ")
	  (boolean :tag "Convert escape sequences ")
	  (boolean :tag "Convert WikiNames        ")
	  (boolean :tag "Convert quote            "))
  :link '(custom-manual "(bhl)Properties of conversion")
  :group 'bhl2wiki)

(defcustom bhl-wiki-footer
  ""
  "*The footer is to be inserted at the end of the WIKI output."
  :type '(string)
  :group 'bhl2html)

;; Set up after custom

(defvar bhl-sectioning-style-alist
  `((num ("[0-9]+\\. "
	  "[0-9]+\\.[0-9]+\\. "
	  "[0-9]+\\.[0-9]+\\.[0-9]+\\. "))
    (alpha ("[A-Z]\\. "
	    "[A-Z]\\.[A-Z]\\. "
	    "[A-Z]\\.[A-Z]\\.[A-Z]\\. "))
    (aster ("\\* " "\\*\\* " "\\*\\*\\* "))
    (equal-sign ("= " "== " "=== "))
    (my ,bhl-my-sectioning-regexp-list)))


;;;; 3 - FACES

(defface bhl-img-face
  '((t (:foreground "aquamarine4")))
  "*The BHL face for images."
  :group 'bhl-faces)

(defface bhl-wiki-face
  '((t (:foreground "aquamarine4")))
  "*The BHL face for WikiNames."
  :group 'bhl-faces)

(defface bhl-url-face
  '((t (:foreground "aquamarine4")))
  "*The BHL face for URL."
  :group 'bhl-faces)

(defface bhl-comment-face
  '((t (:foreground "darkcyan")))
  "*The BHL face for comments."
  :group 'bhl-faces)

(defface bhl-list-face
  '((t (:foreground "royalblue")))
  "*The BHL face for list separators."
  :group 'bhl-faces)

(defface bhl-descrip-face
  '((t (:foreground "royalblue")))
  "*The BHL face for descriptive list separators."
  :group 'bhl-faces)

(defface bhl-hr-face
  '((t (:foreground "darkred")))
  "*The BHL face for horizontal rule."
  :group 'bhl-faces)

(defface bhl-toc-face
  '((t (:foreground "darkcyan")))
  "*The BHL face for the table of contents separators."
  :group 'bhl-faces)

(defface bhl-title-face
  '((t (:weight bold :height 1.9)))
  "*The BHL face for the title."
  :group 'bhl-faces)

(defface bhl-section-face
  '((t (:weight bold :height 1.6)))
  "*The BHL face for the sections."
  :group 'bhl-faces)

(defface bhl-subsection-face
  '((t (:weight bold :height 1.3)))
  "*The BHL face for the subsections."
  :group 'bhl-faces)

(defface bhl-subsubsection-face
  '((t (:weight bold)))
  "*The BHL face for the subsubsections."
  :group 'bhl-faces)

(defface bhl-tt-face
  '((t (:foreground "darkred")))
  "*The BHL face for truetype font text."
  :group 'bhl-faces)

(defface bhl-underline-face
  '((t (:underline t)))
  "*The BHL face for underlined text."
  :group 'bhl-faces)

(defface bhl-bold-face
  '((t (:weight bold)))
  "*The BHL face for bold text."
  :group 'bhl-faces)

(defface bhl-italic-face
  '((t (:slant italic)))
  "*The BHL face for italic text."
  :group 'bhl-faces)

(defface bhl-special-face
  '((t (:foreground "NavajoWhite1")))
  "*The BHL face for minipage and verbatim environment."
  :group 'bhl-faces)

(defvar bhl-img-face 'bhl-img-face
  "*The BHL face for images.")

(defvar bhl-list-face 'bhl-list-face
  "*The BHL face for list separators.")

(defvar bhl-descrip-face 'bhl-descrip-face
  "*The BHL face for descriptive list separators.")

(defvar bhl-hr-face 'bhl-hr-face
  "*The BHL face for horizontal rule.")

(defvar bhl-toc-face 'bhl-toc-face
  "*The BHL face for the table of contents separators.")

(defvar bhl-title-face 'bhl-title-face
  "*The BHL face for the title.")

(defvar bhl-section-face 'bhl-section-face
  "*The BHL face for the sections.")

(defvar bhl-subsection-face 'bhl-subsection-face
  "*The BHL face for the subsections.")

(defvar bhl-subsubsection-face 'bhl-subsubsection-face
  "*The BHL face pour les subsubsections.")

(defvar bhl-tt-face 'bhl-tt-face
  "*The BHL face for truetype font text.")

(defvar bhl-special-face 'bhl-special-face
  "*The BHL face for minipage and verbatim environment.")

(defvar bhl-underline-face 'bhl-underline-face
  "*The BHL face for underlined text.")

(defvar bhl-italic-face 'bhl-italic-face
  "*The BHL face for italic text.")

(defvar bhl-bold-face 'bhl-bold-face
   "*The BHL face for bold text.")


;;;; 4 - MODE

;; XEmacs compatibility issues
(defalias 'bhl-match-string
  (if (fboundp 'match-string-no-properties)
      'match-string-no-properties
    'match-string))

(defun bhl-replace-regexp-in-string (regexp rep string &optional
					    fixedcase literal subexp start)
  "Replace every occurrence for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the `match-data' at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\""
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	(when (= me mb) (setq me (min l (1+ mb))))
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb)
			  matches)))
	(setq start me))
      (setq matches (cons (substring string start l) matches))
      (apply #'concat (nreverse matches)))))

(defsubst bhl-mark-active ()
  "Return non-nil if the mark is active."
  (if (featurep 'xemacs)
      (region-active-p)
    mark-active))

(defsubst bhl-add-to-conversion-log (text)
  "Add TEXT to the conversions list."
  (setq bhl-conversion-log
	(add-to-list 'bhl-conversion-log text)))

(defsubst bhl-string-to-anchor (str)
  "Convert the string STR into an anchor (shorter)."
  (substring str 0 (if (< (length str) 10) (length str) 10)))

(defsubst bhl-cleanup-cell (cell)
  "Clean up a CELL by stripping leading and closing tabs/whitespaces."
  (funcall 'bhl-replace-regexp-in-string "^[ \t]*" ""
	   (funcall 'bhl-replace-regexp-in-string "[ \t]*$" "" cell)))

(defsubst bhl-subst-char-in-string (fromchar tochar string)
  "Convert FROMCHAR to TOCHAR in STRING."
  (let ((cpt 0))
    (while (< cpt (length string))
      (when (equal (aref string cpt) fromchar)
	(aset string cpt tochar))
      (setq cpt (1+ cpt))))
  string)

(defsubst bhl-shrink-window-to-fit ()
  "Shrink the current window to fit."
  (or (< (- (- (window-height)
	       (count-lines (point-min) (point-max))) 2) 0)
      (shrink-window
       (- (- (window-height)
	     (count-lines (point-min) (point-max))) 2))))

(defsubst bhl-indent-line ()
  "Indent line function."
  (interactive)
  (let (column)
    (save-excursion
      (forward-line -1)
      (cond ((looking-at "\t[0-9]")
	     (skip-chars-forward " o\t1234567890.*")
	     (setq column (current-column)))
	    ((looking-at "\t")
	     (skip-chars-forward " o\t*")
	     (setq column (current-column)))
	    (nil (setq column (current-column)))))
    (if column (indent-to column))))

;; Convert TABS into SPACES in the TXT output.
(defsubst bhl-tab-to-spc ()
  "Replace tabs with whitespaces."
  (when (get 'txt 'bhl-tas)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ ]*\\(\t\\)" nil t)
	(replace-match "   " t t nil 1)
	(goto-char (point-min))))))

(defun bhl-texi-make-menu ()
  "Make menus in the TEXI output."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward
	  "^@\\(chapter\\|section\\|subsection\\)" nil t)
    (goto-char (match-beginning 0))
    (save-match-data
      (save-excursion (texinfo-make-menu)))
    (goto-char (match-end 0)))
  (goto-char (point-min)))

(defsubst bhl-texi-toggle-accents ()
  "Toggle accents in the TEXI output."
  (interactive)
  (goto-char (point-min))
  (let ((turn (re-search-forward
	       "\\(@\'\\|@`\\|@^\\)" nil t))
	(accents '(("é" "@'e")
		   ("è" "@`e")
		   ("à" "@`a")
		   ("ù" "@^u")
		   ("ê" "@^e")
		   ("ô" "@^o")
		   ("î" "@^{@dotless{i}}")
		   ("â" "@^a")
		   ("ù" "@`u")
		   ("ç" "@,{c}"))))
    (while (car accents)
      (goto-char (point-min))
      (while (search-forward
	      (if turn (cadr (car accents))
		(car (car accents))) nil t)
	(unless (or (get-text-property (match-beginning 0) 'category)
		    (save-excursion
		      (beginning-of-line)
		      (looking-at "@node")))
	  (replace-match (if turn (car (car accents))
			   (cadr (car accents))))))
      (setq accents (cdr accents)))))

(defsubst bhl2html-set-default-plist ()
  "Set the plist of the `bhl2html' conversion."
  (put 'html 'list-item-is-para bhl-html-list-item-is-para-flag)
  (put 'html 'conversions-list bhl-html-conversions-list)
  (put 'html 'lang (car bhl-i18n-conventions))
  (put 'html 'after-conversion-hook 'bhl-after-html-conversion-hook)
  (put 'html 'bhl-check
       (nth 0 bhl2html-properties-list))
  (put 'html 'bhl-prefix
       (nth 1 bhl2html-properties-list))
  (put 'html 'bhl-caption
       (nth 2 bhl2html-properties-list))
  (put 'html 'bhl-i18n
       (nth 3 bhl2html-properties-list))
  (put 'html 'bhl-toc
       (nth 4 bhl2html-properties-list)))
  
(defsubst bhl2latex-set-default-plist ()
  "Set the plist of the `bhl2latex' conversion."
  (put 'latex 'conversions-list bhl-latex-conversions-list)
  (put 'latex 'lang (car bhl-i18n-conventions))
  (put 'latex 'after-conversion-hook 'bhl-after-latex-conversion-hook)
  (put 'latex 'bhl-check
       (nth 0 bhl2latex-properties-list))
  (put 'latex 'bhl-prefix
       (nth 1 bhl2latex-properties-list))
  (put 'latex 'bhl-caption
       (nth 2 bhl2latex-properties-list))
  (put 'latex 'bhl-i18n
       (nth 3 bhl2latex-properties-list))
  (put 'latex 'bhl-toc
       (nth 4 bhl2latex-properties-list)))
  
(defsubst bhl2sgml-set-default-plist ()
  "Set the plist of the `bhl2sgml' conversion."
  (put 'sgml 'list-item-is-para bhl-sgml-list-item-is-para-flag)
  (put 'sgml 'conversions-list bhl-sgml-conversions-list)
  (put 'sgml 'lang (car bhl-i18n-conventions))
  (put 'sgml 'after-conversion-hook 'bhl-after-sgml-conversion-hook)
  (put 'sgml 'bhl-check
       (nth 0 bhl2sgml-properties-list))
  (put 'sgml 'bhl-caption
       (nth 1 bhl2sgml-properties-list))
  (put 'sgml 'bhl-i18n
       (nth 2 bhl2sgml-properties-list)))

(defsubst bhl2texi-set-default-plist ()
  "Set the plist of the `bhl2texinfo' conversion."
  (put 'texi 'conversions-list bhl-texi-conversions-list)
  (put 'texi 'lang (car bhl-i18n-conventions))
  (put 'texi 'after-conversion-hook 'bhl-after-texi-conversion-hook)
  (put 'texi 'bhl-check
       (nth 0 bhl2texi-properties-list))
  (put 'texi 'bhl-i18n
       (nth 1 bhl2texi-properties-list))
  (put 'texi 'bhl-toc
       (nth 2 bhl2texi-properties-list)))

(defsubst bhl2txt-set-default-plist ()
  "Set the plist of the `bhl2txt' conversion."
  (put 'txt 'conversions-list bhl-txt-conversions-list)
  (put 'txt 'after-conversion-hook 'bhl-after-txt-conversion-hook)
  (put 'txt 'bhl-center
       (car bhl2txt-properties-list))
  (put 'txt 'bhl-toc
       (nth 1 bhl2txt-properties-list))
  (put 'txt 'bhl-tas
       (nth 2 bhl2txt-properties-list)))
  
(defsubst bhl2wiki-set-default-plist ()
  "Set the plist of the `bhl2wiki' conversion."
  (put 'wiki 'conversions-list bhl-wiki-conversions-list)
  (put 'wiki 'after-conversion-hook 'bhl-after-wiki-conversion-hook)
  (put 'wiki 'bhl-prefix
       (nth 0 bhl2html-properties-list))
  (put 'wiki 'bhl-toc
       (nth 1 bhl2wiki-properties-list))
  (put 'wiki 'bhl-tas
       (nth 2 bhl2wiki-properties-list)))

;; Sectioning style
(defun bhl-set-sectioning-regexp-list ()
  "Set `bhl-sectioning-regexp-list'."
  (setq bhl-sectioning-regexp-list
	(if (eq bhl-sectioning-default-style 'my)
	    bhl-my-sectioning-regexp-list
	  (cadr (assq bhl-sectioning-default-style
		      bhl-sectioning-style-alist))))
  (bhl-set-font-lock-keywords))

(defun bhl-guess-style ()
  "Guess the sectioning style of the current buffer.
Set `bhl-sectioning-default-style' to it."
  (interactive)
  (setq bhl-sectioning-default-style
	(bhl-sectioning-guess-style))
  (bhl-set-sectioning-regexp-list))

(defun bhl-sectioning-guess-style ()
  "Return the sectioning style of the current buffer."
  (let (endstyle)
    (mapcar
     (lambda (style)
       (unless endstyle
	 (when (save-excursion
		 (re-search-forward
		  (concat "^" (caadr (assq style bhl-sectioning-style-alist)))
		  nil t))
	   (setq endstyle style))))
     '(num alpha aster equal-sign my))
    (or endstyle bhl-sectioning-default-style)))

;; Fontlock keywords
(defsubst bhl-set-font-lock-keywords ()
  "Set font-lock keywords.
It is necessary to set keywords each time the style of sectioning is changed."
  (setq bhl-font-lock-keywords
	(list
	 (cons bhl-title-regexp bhl-title-face)
	 (cons bhl-list-regexp bhl-list-face)
	 (cons bhl-img-regexp bhl-img-face)
	 (list bhl-descrip-regexp 0 bhl-descrip-face 'append)
	 (cons bhl-hr-regexp bhl-hr-face)
	 (cons bhl-hr1-regexp bhl-hr-face)
	 (cons bhl-hr2-regexp bhl-hr-face)
	 (cons (concat "^" (nth 0 bhl-sectioning-regexp-list) ".*$")
	       bhl-section-face)
	 (cons (concat "^" (nth 1 bhl-sectioning-regexp-list) ".*$")
	       bhl-subsection-face)
	 (cons (concat "^" (nth 2 bhl-sectioning-regexp-list) ".*$")
	       bhl-subsubsection-face)
	 (list (nth 0 bhl-tag-regexp-list) 1 bhl-underline-face 'append)
	 (list (nth 1 bhl-tag-regexp-list) 1 bhl-tt-face 'append)
	 (list (nth 2 bhl-tag-regexp-list) 1 bhl-italic-face 'append)
	 (list (nth 3 bhl-tag-regexp-list) 1 bhl-bold-face 'append)
	 (cons bhl-minipage-regexp bhl-special-face)
	 (cons "^--$" bhl-special-face)
	 (cons bhl-intro-toc bhl-toc-face)
	 (cons bhl-end-toc bhl-toc-face)
	 'bhl-comment-highlight
	 'bhl-quote-highlight
	 'bhl-url-highlight
	 'bhl-generic-url-highlight))
  (font-lock-mode 1))

(defsubst bhl-wiki-set-font-lock-keywords ()
  "Set font-lock keywords for BHL as a local wiki.
See also `bhl-is-a-local-wiki-flag'."
  (add-to-list 'bhl-font-lock-keywords
	       'bhl-wiki-names-highlight
	       'bhl-non-wiki-names-unhighlight)
  (font-lock-mode 1))

(defsubst bhl-toggle-wiki ()
  "Toggle `bhl-is-a-local-wiki-flag'."
  (interactive)
  (setq bhl-is-a-local-wiki-flag (not bhl-is-a-local-wiki-flag))
  (if bhl-is-a-local-wiki-flag
      (bhl-wiki-set-font-lock-keywords)
    (progn (setq bhl-font-lock-keywords
		 (delq 'bhl-non-wiki-names-unhighlight
		       (delq 'bhl-wiki-names-highlight
			     bhl-font-lock-keywords)))
	   (font-lock-mode 1))))

(defsubst bhl-toggle-tags-overlap ()
  "Toggle `bhl-tags-overlap-flag'."
  (interactive)
  (setq bhl-tags-overlap-flag (not bhl-tags-overlap-flag))
  (if bhl-tags-overlap-flag
      (setq bhl-tag-regexp-list bhl-tag-regexp-list0)
    (setq bhl-tag-regexp-list bhl-tag-regexp-list1))
  (if bhl-is-a-local-wiki-flag
      (bhl-wiki-set-font-lock-keywords)
    (bhl-set-font-lock-keywords))
  (bhl-mode))
  
(defun bhl-sectioning-switch-style (&optional style)
  "Switch the current sectioning style to STYLE."
  (interactive)
  (let ((sty (or style (intern (completing-read
				"Switch to style: "
				'(("num" 1)
				  ("alpha" 2)
				  ("aster" 3)
				  ("equal-sign" 4))
				nil t)))))
    (bhl-update-toc sty)
    (setq bhl-sectioning-default-style sty)
    (bhl-set-sectioning-regexp-list)
    (bhl-set-font-lock-keywords)
    (when (eq bhl-is-a-local-wiki-flag t)
	(bhl-wiki-set-font-lock-keywords))
    (message "The style of sectioning has been set to \"%s\"."
	     (symbol-name sty))))

(defsubst bhl-initialize-properties ()
  "Initialize the properties of conversions."
  (bhl2html-set-default-plist)
  (bhl2latex-set-default-plist)
  (bhl2sgml-set-default-plist)
  (bhl2txt-set-default-plist)
  (bhl2wiki-set-default-plist)
  (bhl2texi-set-default-plist))

(easy-menu-define bhl-menu bhl-mode-map
  "Menu of the BHL mode"
  '("BHL"
    ;; the main conversion commands
    ("bhl2html"
     ["Convert into HTML" bhl2html]
     "---"
     ["Check dubious tags" (bhl-toggle-property 'bhl-check 'html)
      :style toggle :selected (get 'html 'bhl-check)]
     ["Prefix sections" (bhl-toggle-property 'bhl-prefix 'html)
      :style toggle :selected (get 'html 'bhl-prefix)]
     ["Use i18n conventions" (bhl-toggle-property 'bhl-i18n 'html)
      :style toggle :selected (get 'html 'bhl-i18n)]
     ["Ask tables caption" (bhl-toggle-property 'bhl-caption 'html)
      :style toggle :selected (get 'html 'bhl-caption)]
     ["Convert toc" (bhl-toggle-property 'bhl-toc 'html)
      :style toggle :selected (get 'html 'bhl-toc)])
    ("bhl2latex"
     ["Convert into LaTeX" bhl2latex]
     "---"
     ["Check dubious tags" (bhl-toggle-property 'bhl-check 'latex)
      :style toggle :selected (get 'latex 'bhl-check)]
     ["Prefix sections" (bhl-toggle-property 'bhl-prefix 'latex)
      :style toggle :selected (get 'latex 'bhl-prefix)]
     ["Use i18n conventions" (bhl-toggle-property 'bhl-i18n 'latex)
      :style toggle :selected (get 'latex 'bhl-i18n)]
     ["Ask tables caption" (bhl-toggle-property 'bhl-caption 'latex)
      :style toggle :selected (get 'latex 'bhl-caption)]
     ["Convert toc" (bhl-toggle-property 'bhl-toc 'latex)
      :style toggle :selected (get 'latex 'bhl-toc)])
    ("bhl2texinfo"
     ["Convert into Texinfo" bhl2texinfo]
     "---"
     ["Check dubious tags" (bhl-toggle-property 'bhl-check 'texi)
      :style toggle :selected (get 'texi 'bhl-check)]
     ["Use i18n conventions" (bhl-toggle-property 'bhl-i18n 'texi)
      :style toggle :selected (get 'texi 'bhl-i18n)]
     ["Convert toc" (bhl-toggle-property 'bhl-toc 'texi)
      :style toggle :selected (get 'texi 'bhl-toc)])
    ("bhl2sgml"
     ["Convert into SGML" bhl2sgml]
     "---"
     ["Check dubious tags" (bhl-toggle-property 'bhl-check 'sgml)
      :style toggle :selected (get 'sgml 'bhl-check)]
     ["Use i18n conventions" (bhl-toggle-property 'bhl-i18n 'sgml)
      :style toggle :selected (get 'sgml 'bhl-i18n)]
     ["Ask tables caption" (bhl-toggle-property 'bhl-caption 'sgml)
      :style toggle :selected (get 'sgml 'bhl-caption)])
    ("bhl2txt"
     ["Convert into TXT" bhl2txt]
     "---"
     ["Center sections" (bhl-toggle-property 'bhl-center 'txt)
      :style toggle :selected (get 'txt 'bhl-center)]
     ["Tabs are spaces" (bhl-toggle-property 'bhl-tas 'txt)
      :style toggle :selected (get 'txt 'bhl-tas)]
     ["Convert toc" (bhl-toggle-property 'bhl-toc 'txt)
      :style toggle :selected (get 'txt 'bhl-toc)])
    ("bhl2wiki"
     ["Convert into WIKI" bhl2wiki]
     "---"
     ["Center sections" (bhl-toggle-property 'bhl-center 'wiki)
      :style toggle :selected (get 'wiki 'bhl-center)]
     ["Tabs are spaces" (bhl-toggle-property 'bhl-tas 'wiki)
      :style toggle :selected (get 'wiki 'bhl-tas)]
     ["Convert toc" (bhl-toggle-property 'bhl-toc 'wiki)
      :style toggle :selected (get 'wiki 'bhl-toc)])
    "---"
    ;; insert commands
    ("Insert"
     ["Insert toc" bhl-insert-toc t]
     ["Insert lol" bhl-insert-lol t]
     ["Insert URL" bhl-insert-url t]
     ["Insert image" bhl-insert-image t]
     ["Insert minipage" bhl-insert-minipage t]
     ["Insert verbatim" bhl-insert-verbatim t]
     ["Insert hr" bhl-insert-hr t])
    ;; changing font commands
    ("Change font"
     ["Bold" bhl-change-font-bold t]
     ["Emphasis" bhl-change-font-emphasis t]
     ["Underline" bhl-change-font-underline t]
     ["TrueType" bhl-change-font-truetype t]
     ["Bold italic" bhl-change-font-bolditalic t]
     "---"
     ["Normal" bhl-change-font-normal t])
    ;; browse the source
    ("Go to"
     ["Next section" bhl-goto-next-section t]
     ["Previous section" bhl-goto-previous-section t]
     ["Next URL or WikiName" bhl-goto-next-url-or-wiki t])
    "---"
    ;; handling the table of contents
    ["Browse the lol" bhl-show-lol t]
    ["Browse the toc" bhl-show-toc t]
    ["Update toc" bhl-update-toc t]
    ("Toc location"
     ["Toc at the top" (setq bhl-toc-location "top")
      :style radio :selected (equal bhl-toc-location "top")]
     ["Toc on the point" (setq bhl-toc-location "point")
      :style radio :selected (equal bhl-toc-location "point")]
     ["Toc at the bottom" (setq bhl-toc-location "bottom")
      :style radio :selected (equal bhl-toc-location "bottom")])
    ("Toc depth"
     ["Sections only" (setq bhl-default-toc-depth 1)
      :style radio :selected (equal bhl-default-toc-depth 1)]
     ["Sections and subsections" (setq bhl-default-toc-depth 2)
      :style radio :selected (equal bhl-default-toc-depth 2)]
     ["Maximum depth [3]" (setq bhl-default-toc-depth 3)
      :style radio :selected (equal bhl-default-toc-depth 3)])
    ;; miscellaneous
    "---"
    ["BHL Wiki" bhl-toggle-wiki
     :style radio :selected (eq bhl-is-a-local-wiki-flag t)]
    ["Tags overlap" bhl-toggle-tags-overlap
     :style radio :selected (eq bhl-tags-overlap-flag t)]
    ("Sectioning style"
     ["Guess" bhl-guess-style t]
     "---"
     ["Numerical" (bhl-sectioning-switch-style 'num)
      :style radio :selected (eq bhl-sectioning-default-style 'num)]
     ["Alphabetical" (bhl-sectioning-switch-style 'alpha)
      :style radio :selected (eq bhl-sectioning-default-style 'alpha)]
     ["Asterisks" (bhl-sectioning-switch-style 'aster)
      :style radio :selected (eq bhl-sectioning-default-style 'aster)]
     ["Equal signs" (bhl-sectioning-switch-style 'equal-sign)
      :style radio :selected (eq bhl-sectioning-default-style 'equal-sign)]
     ["My style" (progn (setq bhl-sectioning-default-style 'my)
			(bhl-set-sectioning-regexp-list)
			(bhl-set-font-lock-keywords)
			(when (eq bhl-is-a-local-wiki-flag t)
			  (bhl-wiki-set-font-lock-keywords))
			(message "The style of sectioning has been set to \"my\"."))
      :style radio :selected (eq bhl-sectioning-default-style 'my)])
    ("I18n conventions"
     ["English" (setcar bhl-i18n-conventions "en")
      :style radio :selected (equal (car bhl-i18n-conventions) "en")]
     ["French" (setcar bhl-i18n-conventions "fr")
      :style radio :selected (equal (car bhl-i18n-conventions) "fr")]
     ["German" (setcar bhl-i18n-conventions "de")
      :style radio :selected (equal (car bhl-i18n-conventions) "de")])
    "---"
    ["Mail buffer" bhl-compose-mail]
    "---"
    ;; miscellaneous (again)
    ["View last conversion log" bhl-view-log t]
    ["Submit bug report" bhl-submit-bug-report t]
    ["Customize BHL" (customize-group "bhl") t]
    ["BHL info manual" (info "bhl.info")]
    ["BHL version" bhl-show-version t]))

;;;###autoload
(define-derived-mode bhl-mode text-mode "BHL"
  "B - Brute, H - HTML, L - LaTeX.

The BHL mode enables you to convert plain text files into
HTML, LaTeX and SGML files.  This is a simple mode, with simple
conversion functions and converts only simple source files.
\\<bhl-mode-map>
+ Sections:
===========

1.<SPC>     : insert a section.
1.1.<SPC>   : insert a subsection.
1.1.1.<SPC> : insert a subsubsection.

A.<SPC>     : insert a section.
A.A.<SPC>   : insert a subsection.
A.A.A.<SPC> : insert a subsubsection.

+ Font change:
==============

*a word*   : bold
_a word_   : emphasis
_*a word*_ : bold-italic
__a word__ : underline
==a word== : truetype

--
This part of text will be inserted as verbatim text.
--

%%[2cm]
This part of text will be put inside a minipage
which width is 2cm (size is optional).
%%

+ Key bindings:
===============

\\[bhl2html] : convert the buffer into HTML.
\\[bhl2sgml] : convert the buffer into SGML (Linuxdoc).
\\[bhl2latex] : convert the buffer into LaTeX.
\\[bhl2texinfo] : convert the buffer into TEXI.
\\[bhl2txt] : convert the buffer into TXT.
\\[bhl2wiki] : convert the buffer into WIKI.

\\[bhl-insert-url] : ask for an URL and its name, then insert it.
\\[bhl-insert-image] : ask for an image file name, then insert it.
\\[bhl-insert-hr] : insert a horizontal rule.
\\[bhl-insert-minipage] : insert a minipage.
\\[bhl-insert-verbatim] : insert a verbatim environment.
\\[bhl-insert-toc] : insert the table of contents.
   Take the toc depth as an optional argument.
\\[bhl-insert-tab] : insert a tab rigidly.

\\[bhl-comment-region] : comment the region
\\[universal-argument] \\[bhl-comment-region] : uncomment the region.

\\[bhl-view-log] : view the log of the last conversion.
\\[bhl-compose-mail] : compose mail with the result of bhl2txt on the current buffer.

\\[bhl-show-toc] : browse the table of contents.
\\[bhl-update-toc] : update the sections' prefix.
\\[bhl-show-lol] : browse the list of links.

\\[bhl-goto-next-section] : go to the next section.
\\[universal-argument] \\[bhl-goto-next-section] : go to the next section or (sub)subsection.
\\[bhl-goto-previous-section] : go to the previous section.
\\[universal-argument] \\[bhl-goto-previous-section] : go to the previous section or (sub)subsection.
\\[bhl-goto-next-url-or-wiki] : go to the next URL or WikiName.

\\[bhl-submit-bug-report] : submit a bug report.
\\[bhl-show-version] : display the current version of BHL.

+ List separators:
==================

TAB(S) and \"*\" or \"-\" or \"o\" and a whitespace: a non-ordered list item.
TAB(S) and 0-9 and dot and a whitespace: an ordered list item.
TAB and a definition term and \"--\" and a whitespace: a definition list item.

+ List examples:
================

   * Here is
      - a simple
      - nested list
         o with three levels
         o of nested items.
      1. Here is...
      2. ...an ordered list
   * inside another list.

   A term -- and its definition.
   Another term -- and its definition.

+ bhl-mode-map:
===============

\\{bhl-mode-map}"
  (kill-all-local-variables)
  (use-local-map bhl-mode-map)
  (easy-menu-add bhl-menu)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'indent-line-function)
  (setq	bhl-tag-regexp-list (if bhl-tags-overlap-flag
				bhl-tag-regexp-list0
			      bhl-tag-regexp-list1))
  (when bhl-autoguess-style-flag
    (bhl-guess-style))
  (bhl-set-sectioning-regexp-list)
  (when (eq bhl-is-a-local-wiki-flag t)
    (bhl-wiki-set-font-lock-keywords))
  (bhl-initialize-properties)
  (setq indent-line-function 'bhl-indent-line
	tab-width bhl-tab-width
	font-lock-defaults '(bhl-font-lock-keywords t)
	font-lock-multiline t
	mode-name "BHL"
	major-mode 'bhl-mode)
  (run-hooks 'bhl-mode-hook))

;; Conversion commands
(define-key bhl-mode-map [(control c) (control w)] 'bhl2html)
(define-key bhl-mode-map [(control c) (control l)] 'bhl2latex)
(define-key bhl-mode-map [(control c) (control s)] 'bhl2sgml)
(define-key bhl-mode-map [(control c) (control d)] 'bhl2txt)
(define-key bhl-mode-map [(control c) (control k)] 'bhl2wiki)
(define-key bhl-mode-map [(control c) (control o)] 'bhl2texinfo)

;; Toc/lol commands
(define-key bhl-mode-map [(control c) (control t)] 'bhl-show-toc)
(define-key bhl-mode-map [(control c) (meta t)] 'bhl-update-toc)
(define-key bhl-mode-map [(control c) (meta s)] 'bhl-guess-style)
(define-key bhl-mode-map [(control c) (control /)] 'bhl-show-lol)

;; Insert
(define-key bhl-mode-map [(control c) (control c) ?t] 'bhl-insert-toc)
(define-key bhl-mode-map [(control c) (control c) ?l] 'bhl-insert-lol)
(define-key bhl-mode-map [(control c) (control c) ?h] 'bhl-insert-url)
(define-key bhl-mode-map [(control c) (control c) ?i] 'bhl-insert-image)
(define-key bhl-mode-map [(control c) (control c) ?m] 'bhl-insert-minipage)
(define-key bhl-mode-map [(control c) (control c) ?v] 'bhl-insert-verbatim)
(define-key bhl-mode-map [(control c) (control c) ?r] 'bhl-insert-hr)
(define-key bhl-mode-map [(meta tab)] 'bhl-insert-tab)

;; Font commands
(define-key bhl-mode-map
  [(control c) ?\;] 'bhl-comment-region)
(define-key bhl-mode-map
  [(control c) (control f) (control b)] 'bhl-change-font-bold)
(define-key bhl-mode-map
  [(control c) (control f) (control e)] 'bhl-change-font-emphasis)
(define-key bhl-mode-map
  [(control c) (control f) (control n)] 'bhl-change-font-normal)
(define-key bhl-mode-map
  [(control c) (control f) (control u)] 'bhl-change-font-underline)
(define-key bhl-mode-map
  [(control c) (control f) (control t)] 'bhl-change-font-truetype)
(define-key bhl-mode-map
  [(control c) (control f) (control _)] 'bhl-change-font-bolditalic)

;; Move inside your document
(define-key bhl-mode-map [?\r] 'newline-and-indent)
(define-key bhl-mode-map [(control c) (control n)] 'bhl-goto-next-section)
(define-key bhl-mode-map [(control c) (control p)] 'bhl-goto-previous-section)
(define-key bhl-mode-map [(control tab)] 'bhl-goto-next-url-or-wiki)

;; Other
(define-key bhl-mode-map [(control down-mouse-3)] 'bhl-popup-menu)
(define-key bhl-mode-map [(control c) (control v)] 'bhl-show-version)
(define-key bhl-mode-map [(control c) (control x) ?m] 'bhl-compose-mail)

;; M-x bhl = M-x bhl-mode
(defalias 'bhl 'bhl-mode)


;;;; 5 - MENU

;;Pop up menu
(defsubst bhl-popup-menu ()
  "Pop up a menu."
  (interactive)
  (eval (x-popup-menu t bhl-popup-menu-map)))

(define-key bhl-popup-menu-map [bhl-compose-mail]
  '("Mail buffer" . bhl-compose-mail))

(define-key bhl-popup-menu-map [bhl-show-lol]
  '("Show lol" . bhl-show-lol))

(define-key bhl-popup-menu-map [bhl-show-toc]
  '("Show toc" . bhl-show-toc))

(define-key bhl-popup-menu-map [bhl2txt]
  '("bhl2txt" . bhl2txt))

(define-key bhl-popup-menu-map [bhl2wiki]
  '("bhl2wiki" . bhl2wiki))

(define-key bhl-popup-menu-map [bhl2sgml]
  '("bhl2sgml" . bhl2html))

(define-key bhl-popup-menu-map [bhl2texinfo]
  '("bhl2texinfo" . bhl2texinfo))

(define-key bhl-popup-menu-map [bhl2latex]
  '("bhl2latex" . bhl2latex))

(define-key bhl-popup-menu-map [bhl2html]
  '("bhl2html" . bhl2html))


;;;; 6 - OTHER FUNCTIONS

;; Ignore some lines
(defun bhl-ignore-regexp-line ()
  "Ignore some lines matching regexp."
  (goto-char (point-min))
  (mapcar (lambda (regexp)
	    (save-excursion
	      (let ((kill-whole-line t))
		(while (re-search-forward regexp nil t)
		  (beginning-of-line)
		  (kill-line)))))
	  bhl-ignored-regexps))

;;Font handling:
(defun bhl-change-font (tagbeg tagend)
  "Insert TAGBEG and TAGEND at the beginning/end of a region."
  (let* ((beg (if (bhl-mark-active) (mark) (point)))
	 (end (point))
	 (temp beg))
    (when (< end beg)
      (setq beg end end temp))
    (goto-char beg)
    (while
	(memq t (mapcar (lambda (char)
			  (equal (char-before (point))
				 char))
			'(?* ?= ?_)))
      (delete-char -1)
      (setq end (1- end)))
    (insert tagbeg)
    (goto-char (+ (length tagbeg) end))
    (while
	(memq t (mapcar (lambda (char)
			  (equal (char-after (point))
				 char))
			'(?* ?= ?_)))
      (delete-char 1))
    (save-excursion (insert tagend))
    (setq mark-active nil)))

(defun bhl-change-font-bold ()
  "Change font to bold."
  (interactive)
  (bhl-change-font "*" "*"))

(defun bhl-change-font-emphasis ()
  "Change font to emphasis."
  (interactive)
  (bhl-change-font "_" "_"))

(defun bhl-change-font-truetype ()
  "Change font to truetype."
  (interactive)
  (bhl-change-font "==" "=="))

(defun bhl-change-font-underline ()
  "Change font to underline."
  (interactive)
  (bhl-change-font "__" "__"))

(defun bhl-change-font-bolditalic ()
  "Change font to bold-italic."
  (interactive)
  (bhl-change-font "_*" "*_"))

(defun bhl-change-font-normal ()
  "Strip any font beautifier."
  (interactive)
  (bhl-change-font "" ""))

;; Sectioning

;; Move inside the BHL source
(defun bhl-goto-next-section (&optional arg)
  "Go to the title of the next section.
If ARG is non-nil, go to the title of the next section or (sub)subsection."
  (interactive "P")
  (let ((reg (if arg (concat "^" (nth 0 bhl-sectioning-regexp-list) ".*$"
			     "\\|^" (nth 1 bhl-sectioning-regexp-list) ".*$"
			     "\\|^" (nth 2 bhl-sectioning-regexp-list) ".*$")
	       (concat "^" (nth 0 bhl-sectioning-regexp-list) ".*$"))))
    (re-search-forward reg nil t)))

(defun bhl-goto-previous-section (&optional arg)
  "Go to the title of the previous section.
If ARG is non-nil, go to the title of the previous section or (sub)subsection."
  (interactive "P")
  (let ((reg (if arg (concat "^" (nth 0 bhl-sectioning-regexp-list) ".*$"
			     "\\|^" (nth 1 bhl-sectioning-regexp-list) ".*$"
			     "\\|^" (nth 2 bhl-sectioning-regexp-list) ".*$")
	       (concat "^" (nth 0 bhl-sectioning-regexp-list) ".*$"))))
    (re-search-backward reg nil t)))

;; Follow URLs and Wiki links
(defun bhl-goto-next-url-or-wiki ()
  "Go to the next URL or WikiName."
  (interactive)
  (let ((case-fold-search)
	(reg (concat bhl-url-regexp "\\|"
		     bhl-wiki-names-regexp)))
    (if (looking-at reg)
	(forward-char))
    (or (re-search-forward reg nil t)
	(progn (goto-char (point-min))
	       (re-search-forward reg nil t))))
  (goto-char (match-beginning 0)))

(defun bhl-url-at-point ()
  "Return non-nil if a URL is at point."
  (save-excursion
    (skip-chars-backward "^'\"<>[{}( \t\n")
    (looking-at bhl-generic-url-regexp)))

(defun bhl-visit-url (link)
  "Visit the URL named LINK."
  (funcall bhl-browse-url-function link))

(defun bhl-follow-url-at-point ()
  "Visit the link at point."
  (interactive)
  (when (bhl-url-at-point)
    (cond ((equal (substring (match-string 0) 0 7) "mailto:")
	   (compose-mail (substring (match-string 0) 7)))
	  (t (bhl-visit-url (or (match-string 5) (match-string 0)))))))

(defun bhl-follow-url-at-mouse (event)
  "According to EVENT, visit the link at point."
  (interactive "e")
  (save-excursion
    (when (fboundp 'posn-window)
      (set-buffer (window-buffer (posn-window (event-start event))))
      (goto-char (posn-point (event-start event))))
    (when (bhl-url-at-point)
      (cond ((equal (substring (match-string 0) 0 7) "mailto:")
	     (compose-mail (substring (match-string 0) 7)))
	    (t (bhl-visit-url (or (match-string 5) (match-string 0))))))))

;; Follow WikiNames
(defun bhl-wiki-name-at-point ()
  "Return non-nil if a URL is at point."
  (save-excursion
    (let (case-fold-search)
      (skip-chars-backward "^'\"<>[{}( \t\n")
      (looking-at bhl-wiki-names-regexp))))

(defun bhl-follow-wiki-name-at-point ()
  "Visit the WikiName at point."
  (interactive)
  (when (bhl-wiki-name-at-point)
    (find-file (concat (if bhl-downcase-wikifiles-names-flag
			   (downcase (match-string 0))
			 (match-string 0))
		       bhl-default-wikifiles-extension))
    (delete-other-windows)))

(defun bhl-follow-wiki-name-at-mouse (event)
  "Visit the WikiName at EVENT."
  (interactive "e")
  (save-excursion
    (when (fboundp 'posn-window)
      (set-buffer (window-buffer (posn-window (event-start event))))
      (goto-char (posn-point (event-start event))))
    (when (bhl-wiki-name-at-point)
      (find-file (concat (if bhl-downcase-wikifiles-names-flag
			     (downcase (match-string 0))
			   (match-string 0))
			 bhl-default-wikifiles-extension)))))

(defsubst bhl-wiki-names-highlight (limit)
  "Highlight WikiNames in the BHL buffer limited to LIMIT."
  (let ((map (copy-keymap (current-local-map)))
	case-fold-search)
    (define-key map [(mouse-2)] 'bhl-follow-wiki-name-at-mouse)
    (define-key map [(return)] 'bhl-follow-wiki-name-at-point)
    (while (re-search-forward bhl-wiki-names-regexp limit t)
      (or (member (match-string 0) bhl-non-wiki-names-list)
	  (add-text-properties (match-beginning 0) (match-end 0)
			       `(face bhl-wiki-face
				      rear-nonsticky t
				      local-map ,map mouse-face highlight help-echo
				      "mouse-2: follow WikiName"))))))

(defsubst bhl-non-wiki-names-unhighlight (limit)
  "Unhighlight non-wiki-names found in `bhl-non-wiki-names-list'.
Search is bounded to LIMIT."
  (let (case-fold-search)
    (while (re-search-forward bhl-wiki-names-regexp limit t)
      (when (member (match-string 0) bhl-non-wiki-names-list)
	(remove-text-properties (match-beginning 0) (match-end 0)
				`(face rear-nonsticky local-map
				       mouse-face help-echo))))))

;; Toggle properties
(defun bhl-toggle-property (prop symb)
  "Toggle the property PROP in SYMB."
  (put symb prop (not (get symb prop))))

;; Show BHL version
(defun bhl-show-version ()
  "Display version infos about BHL."
  (interactive)
  (message "BHL mode %s, Copyright (C) 2003 Bastien Guerry; type C-h m for help."
	   bhl-version)
  (if (sit-for 3)
      (let ((lines bhl-startup-message-lines))
	(while (and (sit-for 3) lines)
	  (message (substitute-command-keys (car lines)))
	  (setq lines (cdr lines)))))
  (message ""))

;; Don't INDENT paragraphs. Restrict indentation to lists.
(defsubst bhl-insert-tab ()
  "Tabulation for the BHL mode.
Use this function to indent list items.
Don't indent paragraphs."
  (interactive)
  (indent-to (+ (current-column) tab-width)))

(defun bhl-compose-mail ()
  "Mail the content of the BHL buffer in plain text."
  (interactive)
  (bhl2txt)
  (save-buffer)
  (let ((file (buffer-file-name)))
    (compose-mail)
    (save-excursion
      (goto-char (point-max))
      (setq tab-width 3)
      (insert-file-contents file))))

;; Check for dubious comments
(defun bhl-check-comment (arg)
  "Check for dubious comment strings in the ARG output."
  (let ((regexp (cond ((eq arg 'latex) "%+")
		      ((eq arg 'texi) "@c[ \t\n]")
		      (t "<!--\\|-->"))))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (bhl-add-to-conversion-log
       (format "Dubious comment \"%s\" at point %s."
	       (match-string 0)
	       (match-beginning 0))))))

;; Check for dubious tags (in verbatim environments)
(defun bhl-check-existing-tags (arg)
  "Check if there is no tag of the ARG format in the source file.
Especially need for verbatim environments."
  (save-excursion
    (goto-char (point-min))
    (let ((reg (cond
		((eq arg 'latex) "\\\\\\(\\w+\\){\\(\\w+\\)}")
		((eq arg 'texi) "@\\(\\w+\\){\\(\\w+\\)}")
		(t "<\\(/?[^>\t\n]+\\)>")))
	  (rep (cond ((eq arg 'latex) "\$\\\\backslash\$\\1\\\\{\\2\\\\}")
		     ((eq arg 'texi) "@@\\1@{\\2@}")
		     (t "&lsaquo;\\1&rsaquo;"))))
      (query-replace-regexp reg rep))))

;; Highlighting functions
(defsubst bhl-comment-highlight (limit)
  "Highlight comment strings limited to LIMIT."
  (while (re-search-forward "^#\\(.*\\)$" limit t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(face bhl-comment-face
				category comment)))
  (while (re-search-forward "[^\\]\\(#.*\\)$" limit t)
    (add-text-properties (match-beginning 1) (match-end 1)
			 '(face bhl-comment-face
				category comment))))

(defsubst bhl-quote-highlight (limit)
  "Highlight quote strings limited to LIMIT."
  (while (re-search-forward "^\t.*$" limit t)
    (unless (string-match " --\\|\t+\*\\|\t+[0-9]+\. \\|\t+- \\|\t+o " (match-string 0))
      (add-text-properties (match-beginning 0) (match-end 0)
			   '(category quote)))))

(defsubst bhl-url-highlight (limit)
  "Highlight URLs in the BHL buffer limited to LIMIT."
  (while (re-search-forward bhl-url-regexp limit t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(face bhl-url-face category link rear-nonsticky t))))

(defsubst bhl-generic-url-highlight (limit)
  "Highlight URLs in the BHL buffer limited to LIMIT."
  (let ((map (copy-keymap (current-local-map))))
    (define-key map [(mouse-2)] 'bhl-follow-url-at-mouse)
    (define-key map [(return)] 'bhl-follow-url-at-point)
    (while (re-search-forward bhl-generic-url-regexp limit t)
      (add-text-properties (match-beginning 0) (match-end 0)
			   `(category link rear-nonsticky t
				      local-map ,map mouse-face
				      highlight help-echo
				      "mouse-2: follow URL")))))

(defsubst bhl-highlight-toc-buffer ()
  "Highlight the *toc* buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward bhl-intro-toc nil t)
    (add-text-properties (match-beginning 0)
			 (match-end 0)
			 '(face bold))
    (while (re-search-forward
	    (concat "^[ ]*\\("
		    (nth 0 bhl-sectioning-regexp-list) "\\|"
		    (nth 1 bhl-sectioning-regexp-list) "\\|"
		    (nth 2 bhl-sectioning-regexp-list)
		    "\\)\\(.*\\)$") nil t)
      (add-text-properties
       (match-beginning 2) (match-end 2)
       '(mouse-face highlight help-echo
		    "mouse-2: go to this section")))))

;; Convert quote chars in HTML and SGML
(defun bhl-convert-quote-char ()
  "Convert the quote character in HTML and SGML."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<\\|>" nil t)
      (if (equal (match-string 0) "<")
	  (replace-match "&lsaquo;")
	(replace-match "&rsaquo;")))))

;; Handle footnotes
(defun bhl-convert-footnotes (arg)
  "Convert footnotes into ARG format."
  (let ((ftn-regexp
	 (concat (regexp-quote footnote-start-tag)
		 (eval (intern
			(concat "footnote-"
				(symbol-name footnote-style)
				"-regexp")))
		 (regexp-quote footnote-end-tag))))
    (goto-char (point-min))
    (when (and (re-search-forward "^Footnotes.*$" nil t)
	       (eq arg 'html))
      (replace-match (concat "<p><b>" (match-string 0) "</b>") t t))
    (while (re-search-forward (concat "\\(" ftn-regexp "\\)[ ]*") nil t)
      (let* ((num (match-string 1)))
	(when (eq arg 'html)
	  (replace-match (concat "<p><a name=\"" num
				 "\" href=\"#ftn" num "\">"
				 num "</a>&nbsp;&nbsp;")))
	(funcall 'bhl-replace-footnote num arg
		 (bhl-subst-char-in-string
		  ?\n ?  (buffer-substring-no-properties
			  (point)
			  (progn (re-search-forward "\\[\\|^$" nil t)
				 (goto-char (1- (match-beginning 0)))))))))
    (when (or (eq arg 'latex) (eq arg 'texi))
      (bhl-skip-footnotes))
    (bhl-add-to-conversion-log "Footnotes conversion.")))

(defun bhl-skip-footnotes (&optional arg)
  "Skip footnotes according to ARG format."
  (save-excursion
    (goto-char (point-min))
    (when arg
      (while (re-search-forward
	      (concat (regexp-quote footnote-start-tag)
		      (eval (intern
			     (concat "footnote-"
				     (symbol-name footnote-style)
				     "-regexp")))
		      (regexp-quote footnote-end-tag)) nil t)
	(replace-match "")))
    (goto-char (point-min))
    (when (re-search-forward "^Footnotes.*$" nil t)
      (kill-region (match-beginning 0) (point-max)))))

(defun bhl-replace-footnote (num arg content)
  "Replace the footnote number NUM into ARG format with CONTENT."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (regexp-quote num) nil t)
    (unless (let ((cat (get-text-property (match-beginning 0) 'category)))
	      (or (and (eq cat 'verbatim)
		       (memq 'footnote bhl-verbatim-ignore))
		  (eq cat 'link)
		  (eq cat 'comment)))
      (cond ((eq arg 'latex)
	     (let ((section
		    (save-match-data
		      (save-excursion
			(beginning-of-line)
			(looking-at (mapconcat
				     (lambda (str) str)
				     bhl-sectioning-regexp-list "\\|"))))))
	       (replace-match (concat (if section "\\protect" "")
				      "\\footnote{" content "}") t t)))
	    ((eq arg 'texi)
	     (replace-match (concat "@footnote{" content "}") t t))
	    ((eq arg 'html)
	     (replace-match
	      (concat "<sup><a name=\"ftn" (match-string 0)
		      "\" href=\"#" (match-string 0) "\">"
		      (match-string 0) "</a></sup>")))))))

(defun bhl-convert-quote (arg)
  "Convert quote environment into ARG format."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\t.*$" nil t)
      (let ((result (bhl-match-string 0))
	    (loc (match-beginning 0))
	    (intro-tag (cond ((eq arg 'latex) "\n\\begin{quote}")
			     ((eq arg 'html) "\n<blockquote>")
			     ((eq arg 'wiki) "\n<blockquote>")
			     ((eq arg 'texi) "\n@quotation")
			     ((eq arg 'sgml) "\n<quote>")))
	    (end-tag (cond ((eq arg 'latex) "\\end{quote}\n")
			   ((eq arg 'html) "</blockquote>\n")
			   ((eq arg 'wiki) "</blockquote>\n")
			   ((eq arg 'texi) "@end quotation\n")
			   ((eq arg 'sgml) "</quote>\n"))))
	(if (not (or (string-match " --\\|\t+\\* \\|\t+[0-9]+\. \\|\t+- \\|\t+o " result)
		     (and (memq 'quote bhl-verbatim-ignore)
			  (get-text-property loc 'category) 'verbatim)))
	    (progn (forward-line -1)
		   (end-of-line)
		   (insert intro-tag)
		   (re-search-forward "^$\\|^--$\\|^[<>%][-%]%+.*$" nil t)
		   (beginning-of-line)
		   (insert end-tag))
	  (re-search-forward "^$" nil t))))))

;; convert verbatim, minipages and horizontal rules
(defun bhl-convert-verbatim (arg)
  "Convert verbatim environment into ARG format."
  (goto-char (point-min))
  (while (re-search-forward "^--$" nil t)
    (bhl-convert-special arg
			 '("" "<pre>" "<p><verb>" "\\begin{verbatim}" "@verbatim")
			 '("" "</pre>" "</verb>" "\\end{verbatim}" "@end verbatim")
			 "--" "--")))

(defun bhl-convert-minipage (arg)
  "Convert minipage into ARG format."
  (goto-char (point-min))
  (while (re-search-forward bhl-minipage-regexp nil t)
    (let ((border (if (equal (match-string 2) "-")
		      (cond ((eq arg 'texi) "@cartouche\n")
			    ((or (eq arg 'html)  (eq arg 'wiki))
			     " border: 1pt solid\;")
			    ((eq arg 'latex) "\\fbox{")
			    (t "")) ""))
	  (end-border (cond ((and (eq arg 'latex)
				  (equal (match-string 2) "-")) "}")
			    ((and (eq arg 'texi)
				  (equal (match-string 2) "-"))
			     "\n@end cartouche")))
	  (bhl-intro-minipage (regexp-quote (match-string 0)))
	  justify width intro-minipage end-minipage)
      (cond ((eq arg 'latex)
	     (cond ((equal (match-string 1) "<")
		    (setq justify "flushleft"))
		   ((equal (match-string 1) ">")
		    (setq justify "flushright"))
		   (t (setq justify "center")))
	     (if (match-string 4) (setq width (match-string 4))
	       (setq width "\\textwidth")))
	    ((or (eq arg 'html) (eq arg 'wiki))
	     (cond ((equal (match-string 1) "<")
		    (setq justify "left"))
		   ((equal (match-string 1) ">")
		    (setq justify "right"))
		   (t (setq justify "center")))
	     (if (match-string 4) (setq width (match-string 4))
	       (setq width "100%")))
	    ((eq arg 'texi)
	     (cond ((equal (match-string 1) "<")
		    (setq justify "flushleft"))
		   ((equal (match-string 1) ">")
		    (setq justify "flushright"))
		   (t (setq justify bhl-texi-centered-minipage-tag)))
	     (if (match-string 4) (setq width (match-string 4))
	       (setq width "100%"))))
      (setq intro-minipage
	    `("" ,(concat "<div align=\"" justify "\">\n"
			  "<div style=\"padding: 2mm; width: "
			  width ";" border "\" align=\"justify\">")
	      ,(concat "<p>" bhl-sgml-minipage-tag)
	      ,(concat
		"\\vspace{.3cm}\n\\begin{" justify
		"}\n" border
		"\\begin{minipage}[c]{" width "}")
	      ,(concat border "@" justify)))
      (setq end-minipage `("" "</div>\n</div>" "</code>"
			   ,(concat "\\end{minipage}" end-border
				    "\n\\end{" justify "}\n\\vspace{.3cm}")
			   ,(concat "@end " justify end-border)))
      (bhl-convert-special arg intro-minipage
			   end-minipage bhl-intro-minipage "^%%+"))))

(defun bhl-convert-hr (arg)
  "Convert a row of dashes into an horizontal rule.
ARG is the output format."
  (goto-char (point-min))
  (while (re-search-forward bhl-hr-regexp nil t)
    (let (justify width)
      (cond ((equal (match-string 1) "<")
	     (setq justify "left"))
	    ((equal (match-string 1) ">")
	     (setq justify "right"))
	    (t (setq justify "center")))
      (cond ((or (eq arg 'txt) (eq arg 'wiki))
	     (if (match-string 4)
		 (setq width (string-to-number (match-string 4)))
	       (setq width (or fill-column 70)))
	     (let (replacmt)
	       (if (equal justify "right")
		   (setq replacmt (concat (make-string (- (or fill-column 70) width) 32)
					  (make-string width ?-)))
		 (setq replacmt (make-string (or width fill-column 70) ?-)))
	       (replace-match replacmt)
	       (when (equal justify "center")
		 (center-line))))
	    ((eq arg 'latex)
	     (setq width (or (match-string 3) "\\\\textwidth"))
	     (let (flush)
	       (setq flush (if (equal justify "center") "" "flush"))
	       (replace-match
		(concat "\\\\begin{" flush justify "}\n\\\\rule{"
			width "}{1pt}\n\\\\end{" flush justify"}"))))
	    ((eq arg 'html)
	     (setq width (or (match-string 3) "100%"))
	     (replace-match (concat "<hr style=\"height: 1px; width: "
				    width ";\" align=\"" justify "\">")))
	    (t (replace-match ""))))))

(defun bhl-convert-hr1 (arg)
  "Convert a row of dashes into an horizontal rule.
ARG is the output format."
  (goto-char (point-min))
  (while (re-search-forward bhl-hr1-regexp nil t)
    (let (justify width)
      (cond ((equal (match-string 1) "<")
	     (setq justify "left"))
	    ((equal (match-string 1) ">")
	     (setq justify "right"))
	    (t (setq justify "center")))
      (cond ((or (eq arg 'txt) (eq arg 'wiki))
	     (if (match-string 4)
		 (setq width (string-to-number (match-string 4)))
	       (setq width (or fill-column 70)))
	     (let (replacmt)
	       (if (equal justify "right")
		   (setq replacmt (concat (make-string (- (or fill-column 70) width) 32)
					  (make-string width ?-)))
		 (setq replacmt (make-string (or width fill-column 70) ?-)))
	       (replace-match replacmt)
	       (when (equal justify "center")
		 (center-line))))
	    ((eq arg 'latex)
	     (setq width (or (match-string 3) "\\\\textwidth"))
	     (let (flush)
	       (setq flush (if (equal justify "center") "" "flush"))
	       (replace-match
		(concat "\\\\begin{" flush justify "}\n\\\\rule{"
			width "}{5pt}\n\\\\end{" flush justify"}"))))
	    ((eq arg 'html)
	     (setq width (or (match-string 3) "100%"))
	     (replace-match (concat "<hr style=\"height: 5px; width: "
				    width ";\" align=\"" justify "\">")))
	    (t (replace-match ""))))))

(defun bhl-convert-hr2 (arg)
  "Convert a raw of dashes into an horizontal rule.
ARG is the output format."
  (goto-char (point-min))
  (while (re-search-forward bhl-hr2-regexp nil t)
    (let (justify width)
      (cond ((equal (match-string 1) "<")
	     (setq justify "left"))
	    ((equal (match-string 1) ">")
	     (setq justify "right"))
	    (t (setq justify "center")))
      (cond ((or (eq arg 'txt) (eq arg 'wiki))
	     (if (match-string 4)
		 (setq width (string-to-number (match-string 4)))
	       (setq width (or fill-column 70)))
	     (let (replacmt)
	       (if (equal justify "right")
		   (setq replacmt (concat (make-string (- (or fill-column 70) width) 32)
					  (make-string width ?-)))
		 (setq replacmt (make-string (or width fill-column 70) ?-)))
	       (replace-match replacmt)
	       (when (equal justify "center")
		 (center-line))))
	    ((eq arg 'latex)
	     (setq width (or (match-string 3) "\\\\textwidth"))
	     (let (flush)
	       (setq flush (if (equal justify "center") "" "flush"))
	       (replace-match
		(concat "\\\\begin{" flush justify "}\n\\\\rule{"
			width "}{10pt}\n\\\\end{" flush justify"}"))))
	    ((eq arg 'html)
	     (setq width (or (match-string 3) "100%"))
	     (replace-match (concat "<hr style=\"height: 10px; width: "
				    width ";\" align=\"" justify "\">")))
	    (t (replace-match ""))))))

(defun bhl-convert-special (arg intro-tag end-tag mark-beg mark-end)
  "Convert a specific region into ARG format.
INTRO-TAG is a list of TXT, HTML, SGML and LaTeX tags to
insert at the beginning of the region.
END-TAG is a list of TXT, HTML, SGML and LaTeX tags  to
insert at the end of the region.
MARK-BEG is the delimiter of the beginning of the region.
MARK-END is the delimiter of the end of the region."
  (save-excursion
    (goto-char (point-min))
    (let ((prefix (cond ((eq arg 'txt) (nth 0 intro-tag))
			((eq arg 'html) (nth 1 intro-tag))
			((eq arg 'wiki) (nth 1 intro-tag)) ;; wiki as html
			((eq arg 'sgml) (nth 2 intro-tag))
			((eq arg 'latex) (nth 3 intro-tag))
			((eq arg 'texi) (nth 4 intro-tag))))
	  (suffix (cond ((eq arg 'txt) (nth 0 end-tag))
			((eq arg 'html) (nth 1 end-tag))
			((eq arg 'wiki) (nth 1 end-tag)) ;; same as html
			((eq arg 'sgml) (nth 2 end-tag))
			((eq arg 'latex) (nth 3 end-tag))
			((eq arg 'texi) (nth 4 end-tag)))))
      (when (re-search-forward (concat "^\\(" mark-beg "\\)$") nil t)
	(replace-match prefix t t nil 1))
      (when (re-search-forward (concat "^\\(" mark-end "\\)$") nil t)
	(replace-match suffix t t nil 1)))))

;; Insertion functions
(defun bhl-insert-special (arg)
  "Insert a special environment, according to ARG.
When ARG is a dash, insert a horizontal rule.
When ARG is \"%\", insert a minipage."
  (let ((mk "")
	(align (completing-read
		"Align: " '(("left" 1) ("center" 2) ("right" 3))
		nil t "center"))
	(width (read-from-minibuffer "Width: ")))
    (cond ((equal align "left") (setq mk "<"))
	  ((equal align "center") (setq mk arg))
	  ((equal align "right") (setq mk ">")))
    (insert (concat mk (make-string 3 (string-to-char arg))
		    (if (equal width "") "\n" (concat "[" width "]\n"))))))

(defun bhl-insert-url ()
  "Read an URL and its name from the minibuffer."
  (interactive)
  (let ((url (read-from-minibuffer "URL: " ""))
	(name (read-from-minibuffer "Name: ")))
    (if (equal name "")
	(insert url)
      (insert "[[" url "][" name "]]"))
    (message "URL \"%s\" named \"%s\" inserted." url name)))

(defun bhl-insert-image (filename)
  "Read a FILENAME to insert as an image."
  (interactive "FInsert image: ")
  (if (file-directory-p filename)
      (signal 'file-error (list "Opening input file" "file is a directory"
				filename))
    (insert "[[" (file-name-nondirectory filename) "]]")
    (message "Image \"%s\" inserted." (file-name-nondirectory filename))))

(defun bhl-insert-hr ()
  "Insert a horizontal rule."
  (interactive)
  (bhl-insert-special "-")
  (message "Horizontal rule inserted."))

(defun bhl-insert-minipage ()
  "Insert a minipage."
  (interactive)
  (bhl-insert-special "%")
  (save-excursion
    (insert "\n%%%%"))
  (message "Minipage inserted."))

(defun bhl-insert-verbatim ()
  "Insert a verbatim environment."
  (interactive)
  (insert "--\n")
  (save-excursion
    (insert "--"))
  (message "Verbatim environment inserted."))

;; Core of the conversion
(defsubst bhl-replace-section (arg sec beg end &optional num ctr)
  "Replace section's labels with the right tags (HTML or LaTeX).
Here are the arguments:

ARG: the output format;
SEC: a regexp which matches the section;
BEG: a string to be inserted just before the title of the section;
END: a string to be inserted just after the title of the section;

NUM (optional): if nil, no prefix is used for sections' title.
CTR (optional): if t, center line."
  (goto-char (point-min))
  (while (re-search-forward (concat "\\(^" sec "\\)\\(.*$\\)") nil t)
    (let ((str (match-string 0))
	  (strnonum (match-string 2)))
      (replace-match
       (concat beg (if num (match-string 1) "")
	       strnonum end) t t)
      (when ctr (center-line))
      (cond ((eq arg 'html)
	     (beginning-of-line)
	     (insert "<a name=\"" (bhl-string-to-anchor str)
		     "\"></a>\n"))
	    ((eq arg 'texi)
	     (beginning-of-line)
	     (insert "@node " strnonum "\n"))))))

(defsubst bhl-replace-tag (word beg end)
  "Replace tags (HTML or LaTeX).
WORD is the string to be replaced.
BEG and END are the beginning end ending tags."
  (goto-char (point-min))
  (while (re-search-forward word nil t)
    (unless (or (and (text-property-any (match-beginning 2) (match-end 2)
					'category 'verbatim)
		     (memq 'tag bhl-verbatim-ignore))
		(text-property-any (match-beginning 2) (match-end 2)
				   'category 'link)
		(text-property-any (match-beginning 2) (match-end 2)
				   'category 'comment))
      (replace-match (concat beg (match-string 2) end)
		     t t nil 1))))

;; A generic conversion function for punctuation and special characters.
;; By default, special chars are not converted in verbatim environments.
(defun bhl-convert-char (arg lst)
  "Convert into the ARG format some strings from LST."
  (let ((cpt 0) case-fold-search)
    (while (nth cpt lst)
      (goto-char (point-min))
      (while (re-search-forward (car (nth cpt lst)) nil t)
	(unless (let ((cat (get-text-property (match-beginning 0) 'category)))
		  (or (and (eq cat 'verbatim)
			   (memq 'special-char bhl-verbatim-ignore))
		      (eq cat 'link)
		      (eq cat 'comment)))
	  (replace-match
	   (cond ((eq arg 'texi)
		  (cadddr (nth cpt lst)))
		 ((eq arg 'latex)
		  (caddr (nth cpt lst)))
		 ((or (eq arg 'html) (eq arg 'sgml))
		  (if (and (eq arg 'sgml)
			   (or (equal (match-string 0) "oe")
			       (equal (match-string 0) "OE")))
		      (match-string 0)
		    (cadr (nth cpt lst))))
		 (t (match-string 0)))
	   t t nil (if (match-string 1) 1 0))))
      (setq cpt (1+ cpt)))))

(defun bhl-convert-tex (arg)
  "Convert \"LaTeX\", \"LaTeX2e\" and \"TeX\" strings into ARG format."
  (goto-char (point-min))
  (while (re-search-forward "\\<TeX\\>" nil t)
    (unless (get-text-property (1+ (match-beginning 0)) 'category)
      (cond ((eq arg 'texi) (replace-match "@TeX{}" t t))
	    ((eq arg 'latex) (replace-match "\\TeX{}" t t))
	    ((eq arg 'html) (replace-match "T<small>E</small>X" t t))
	    (t (match-string 0) t t))))
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (re-search-forward "\\<\\(LaTeX\\)\\(2\\)?\\(e\\)?\\>" nil t)
      (unless (let ((cat (get-text-property (match-beginning 1) 'category)))
		(or (and (eq cat 'verbatim)
			 (memq 'tex-label bhl-verbatim-ignore))
		    (eq cat 'link)
		    (eq cat 'comment)))
	(replace-match (cond ((eq arg 'latex)
			      (concat "\\LaTeX" (match-string 3) "{}"))
			     ((eq arg 'texi)
			      (concat "La@TeX{}" (if (match-string 3)
						     (concat (match-string 2)
							     (match-string 3)))))
			     ((eq arg 'html)
			      (concat "L<sup><small>A</small></sup>T<small>E</small>X"
				      (if (match-string 3)
					  (concat (match-string 2) "&epsilon"))))
			     (t (match-string 0))) t t)))))

(defun bhl-convert-img (arg)
  "Convert inserted images into ARG format."
  (goto-char (point-min))
  (while (re-search-forward bhl-img-regexp nil t)
    (unless (let ((cat (get-text-property (1- (match-beginning 0))
					  'category)))
	      (or (and (eq cat 'verbatim)
		       (memq 'images bhl-verbatim-ignore))
		  (eq cat 'comment)))
      (replace-match
       (cond ((or (eq arg 'html) (eq arg 'wiki))
	      (concat "<p><img alt=\"" (match-string 1)
		      "\" src=\"" (match-string 1) "\" align=\""
		      bhl-html-img-align "\">"))
	     ((eq arg 'texi) (concat "@c " (match-string 1)))
	     ((eq arg 'latex) (concat "% " (match-string 1)))
	     ((eq arg 'sgml) (concat "<!-- " (match-string 1) " -->"))
	     (t "")) t t))))

(defun bhl-convert-list (arg)
  "Convert lists into ARG format."
  (let ((regexp-list
	 (if (eq arg 'texi)
	     (reverse bhl-list-regexp-list)
	   bhl-list-regexp-list)))
    (while (car regexp-list)
      (let ((regexp1 (caar regexp-list))
	    (regexp2 (cadar regexp-list)))
	(bhl-convert-list-intern arg regexp1 regexp2)
	(setq regexp-list (cdr regexp-list))))))

(defun bhl-convert-list-intern (arg regexp1 regexp2)
  "Convert list in ARG format using REGEXP1 and REGEXP2."
  (goto-char (point-min))
  (while (re-search-forward regexp1 nil t)
    (unless (let ((cat (get-text-property (match-beginning 0) 'category)))
	      (or (and (eq cat 'verbatim)
		       (memq 'list bhl-verbatim-ignore))
		  (eq cat 'comment)))
      (beginning-of-line)
      (let* ((tab (if (eq arg 'texi) ""
		    (bhl-subst-char-in-string
		     ?\t ?  (substring (match-string 1) 1))))
	     (conversion (assq arg bhl-list-syntax-alist))
	     (syntax (if (string-match "[0-9]" (match-string 2))
			 (caddr conversion)
		       (cadr conversion))))
	(insert (concat tab (car syntax) "\n"))
	(while (re-search-forward
		regexp1
		(save-excursion
		  (forward-char 1)
		  (re-search-forward regexp2 nil t)
		  (match-beginning 0)) t)
	  (let ((para (when (and (or (eq arg 'html)
				     (eq arg 'sgml))
				 (get arg 'list-item-is-para)) "<p>")))
	    (replace-match (concat (unless (eq arg 'texi) " ")
				   tab (cadr syntax) para))))
	(forward-char 1)
	(re-search-forward regexp2 nil t)
	(beginning-of-line)
	(skip-chars-backward "\n\t ")
	(insert (concat "\n" tab (caddr syntax)))))))

(defun bhl-convert-description (arg)
  "Convert description lists into ARG format."
  (goto-char (point-min))
  (let ((syntax (cadddr (assq arg bhl-list-syntax-alist))))
    (while (re-search-forward "^\\(\t+\\)\\([^\n-]+\\) -- " nil t)
      (unless (let ((cat (get-text-property (match-beginning 0) 'category)))
		(or (and (eq cat 'verbatim)
			 (memq 'description bhl-verbatim-ignore))
		    (eq cat 'comment)))
	(let ((tab (if (eq arg 'texi) ""
		     (bhl-subst-char-in-string
		      ?\t ?  (substring (match-string 1) 1)))))
	  (beginning-of-line)
	  (insert (concat tab (car syntax) "\n"))
	  (while (re-search-forward
		  bhl-descrip-regexp
		  (save-match-data
		    (save-excursion
		      (forward-char 1)
		      (re-search-forward "^[\\'«\"&*_=a-zA-Z0-9<]" nil t))) t)
	    (replace-match
	     (concat (if (eq arg 'texi) "" " ") tab (cadr syntax)
		     (match-string 2)
		     (caddr syntax)
		     (when (and (or (eq arg 'html)
				    (eq arg 'sgml))
				(get arg 'list-item-is-para)) "<p>")) t t))
	  (forward-char 1)
	  (re-search-forward "^[\t ]*$\\|\\'" nil t)
	  (beginning-of-line)
	  (skip-chars-backward "\n\t ")
	  (insert (concat "\n" tab (cadddr syntax))))))))

(defun bhl-convert-url (&optional arg)
  "Convert an URL.  An optional argument ARG specifies the output format."
  (goto-char (point-min))
  (while (re-search-forward bhl-url-regexp nil t)
    (unless (or (and (memq 'url bhl-verbatim-ignore)
		     (get-text-property (1- (match-beginning 0))
					'category) 'verbatim)
		(eq  (get-text-property (1- (match-beginning 0))
					'category) 'comment))
      (cond
       ((eq arg 'txt)
	(cond ((equal (match-string 3) "mailto:")
	       (replace-match (match-string 4) nil t))
	      ((equal (match-string 3) "url:")
	       (replace-match (match-string 4) nil t))
	      ((equal (match-string 6) "mailto:")
	       (replace-match (concat (match-string 7) " ("
				      (match-string 8) ")") nil t))
	      ((match-string 8)
	       (replace-match (concat (match-string 5) " ("
				      (match-string 8) ")") nil t))
	      (t (replace-match (match-string 1) nil t))))
       ((eq arg 'html)
	(cond ((equal (match-string 3) "mailto:")
	       (replace-match
		(concat "<a href=\""
			(match-string 1) "\">"
			(match-string 4) "</a>") nil t))
	      ((equal (match-string 3) "url:")
	       (replace-match
		(concat "<a href=\""
			(match-string 4) "\">"
			(match-string 4) "</a>") nil t))
	      ((replace-match
		(concat "<a href=\""
			(or (match-string 5)
			    (match-string 1)) "\">"
			    (or (match-string 8)
				(match-string 1))"</a>") nil t))))
       ((eq arg 'texi)
	(cond ((equal "url:" (match-string 3))
	       (replace-match (concat "@uref{" (match-string 4) "}")))
	      ((replace-match
		(concat "@uref{" (or (match-string 5)
				     (match-string 1))
			(if (match-string 8)
			    (concat "," (match-string 8) "}") "}"))))))
       ((eq arg 'sgml)
	(cond ((equal "url:" (match-string 3))
	       (replace-match
		(concat "<htmlurl url=\""
			(match-string 4) "\" name=\""
			(match-string 4) "\">")))
	      ((replace-match
		(concat "<htmlurl url=\"" (or (match-string 5)
					      (match-string 1))
			"\" name=\"" (or (match-string 8)
					 (match-string 1)) "\">")))))
       ((eq arg 'latex)
	(cond ((equal (match-string 3) "url:")
	       (replace-match
		(concat "\\url{" (match-string 4) "}") nil t))
	      ((equal (match-string 3) "mailto:")
	       (replace-match
		(concat "\\url{" (match-string 4) "}") nil t))
	      ((equal (match-string 6) "mailto:")
	       (replace-match
		(concat (match-string 8) " (\\url{"
			(match-string 7) "})") nil t))
	      ((match-string 8)
	       (replace-match
		(concat (match-string 8) " (\\url{"
			(match-string 5) "})") nil t))
	      (t (replace-match (concat "\\url{"
					(match-string 1) "}")
				nil t))))))))

;; Map tables
(defsubst bhl-map-table ()
  "Map and delete tables."
  (let ((beg (point))
	(limit (save-excursion (re-search-forward "^[^|]" nil t)))
	table)
    (while (re-search-forward "^|.*$" limit t)
      (setq table (append table (list (split-string (bhl-match-string 0) "|")))))
    (kill-region beg (point))
    table))

(defun bhl-table-beg (arg &optional table caption)
  "Create in ARG format the TABLE beginning tag with CAPTION."
  (cond ((eq arg 'html)
	 (concat
	  "<TABLE align=\"" bhl-table-align "\" "
	  "summary=\"" (or caption "No summary") "\" "
	  (mapconcat
	   (lambda (str)
	     (concat (car str) "=\"" (cdr str) "\""))
	   bhl-html-table-param-alist
	   " ") ">\n"))
	((eq arg 'latex)
	 (concat "\\begin{table}[" bhl-table-location "]\n"
		 (cond ((equal bhl-table-align "left") "\\flushleft\n")
		       ((equal bhl-table-align "center") "\\centering\n")
		       ((equal bhl-table-align "right") "\\flushright\n"))
		 "\\begin{tabular}{"
		 (make-string (length (car table))
			      (string-to-char bhl-table-cell-align))
		 "}\n"))
	((eq arg 'sgml)
	 (concat "<p><table loc=\"" bhl-table-location "\">\n"
		 "<tabular ca=\""
		 (make-string (length (car table))
			      (string-to-char bhl-table-cell-align))
		 "\">\n"))))

(defsubst bhl-convert-table (arg)
  "Convert tables into ARG format."
  (cond ((eq arg 'html)
	 (bhl-html-convert-table))
	((eq arg 'texi)
	 (bhl-texi-convert-table))
	((eq arg 'txt)
	 (bhl-txt-convert-table))
	((eq arg 'wiki)
	 (bhl-wiki-convert-table))
	(t (bhl-xxx-convert-table arg))))

(defun bhl-texi-convert-table ()
  "Convert tables into TEXI format."
  (bhl-txt-convert-table)
  (goto-char (point-min))
  (while (re-search-forward "^|" nil t)
    (unless (let ((cat (get-text-property
			(match-beginning 0) 'category)))
	      (and (eq cat 'verbatim)
		   (memq 'table bhl-verbatim-ignore)))
      (goto-char (match-beginning 0))
      (insert "@" bhl-texi-table-tag "\n")
      (re-search-forward "^[ \t]*$" nil t)
      (insert "@end " bhl-texi-table-tag "\n"))))

(defun bhl-txt-convert-table ()
  "Convert tables into TXT format."
  (goto-char (point-min))
  (save-excursion
    (while (re-search-forward "|[ \t]*@" nil t)
      (unless (let ((cat (get-text-property
			  (match-beginning 0) 'category)))
		(and (eq cat 'verbatim)
		     (memq 'table bhl-verbatim-ignore)))
	(replace-match (bhl-subst-char-in-string ?@ ?  (bhl-match-string 0)) t t))))
  (save-excursion
    (while (re-search-forward "@[ \t]*|" nil t)
      (unless (let ((cat (get-text-property
			  (match-beginning 0) 'category)))
		(and (eq cat 'verbatim)
		     (memq 'table bhl-verbatim-ignore)))
	(replace-match (bhl-subst-char-in-string ?@ ?  (bhl-match-string 0)) t t)))))

;; Wiki tables are rather like text tables only the * char marks *headings*.
(defun bhl-wiki-convert-table ()
  "Convert tables into WIKI format."
  (goto-char (point-min))
  (save-excursion
    (while (re-search-forward "|[ \t]*@" nil t)
      (unless (let ((cat (get-text-property
			  (match-beginning 0) 'category)))
		(and (eq cat 'verbatim)
		     (memq 'table bhl-verbatim-ignore)))
	(replace-match (bhl-subst-char-in-string ?@ ?* (bhl-match-string 0)) t t))))
  (save-excursion
    (while (re-search-forward "@[ \t]*|" nil t)
      (unless (let ((cat (get-text-property
			  (match-beginning 0) 'category)))
		(and (eq cat 'verbatim)
		     (memq 'table bhl-verbatim-ignore)))
	(replace-match (bhl-subst-char-in-string ?@ ?* (bhl-match-string 0)) t t)))))

(defun bhl-html-convert-table ()
  "Convert tables into HTML format."
  (goto-char (point-min))
  (let* ((cell-align
	  (cond ((equal bhl-table-cell-align "l") "left")
		((equal bhl-table-cell-align "c") "center")
		((equal bhl-table-cell-align "r") "right")))
	 (tdbeg (concat "<td align=\"" cell-align "\">"))
	 (thbeg (concat "<th align=\"" cell-align "\">")))
    (while (re-search-forward "^|" nil t)
      (unless (let ((cat (get-text-property (match-beginning 0) 'category)))
		(and (eq cat 'verbatim)
		     (memq 'table bhl-verbatim-ignore)))
	(beginning-of-line)
	(let* ((caption
		(when (get 'html 'bhl-caption)
		  (read-from-minibuffer "Caption: ")))
	       (tbl (bhl-map-table))
	       (tablebeg (bhl-table-beg 'html tbl caption)))
	  (insert tablebeg)
	  (when caption
	    (insert "<caption>" caption "</caption>\n"))
	  (while (car tbl)
	    (insert " <tr>\n")
	    (mapcar
	     (lambda (str)
	       (insert
		(concat
		 "  "
		 (if (string-match "[ \t]*\\(@.*\\)@.*" str)
		     (progn (aset str (match-beginning 1) ?  )
			    (aset str (match-end 1) ?  )
			    (concat thbeg (bhl-cleanup-cell str)
				    "</th>\n"))
		   (concat tdbeg (bhl-cleanup-cell str)
			   "</td>\n")))))
	     (car tbl))
	    (insert " </tr>\n")
	    (setq tbl (cdr tbl))))
	(insert "</table>\n")))))

(defun bhl-xxx-convert-table (arg)
  "Convert tables into ARG format."
  (goto-char (point-min))
  (let ((sep (if (eq arg 'latex) "&" "|"))
	(endline (if (eq arg 'latex) "\\\\" "@")))
    (while (re-search-forward "^|" nil t)
      (unless (let ((cat (get-text-property (match-beginning 0) 'category)))
		(and (eq cat 'verbatim)
		     (memq 'table bhl-verbatim-ignore)))
	(beginning-of-line)
	(let* ((caption
		(if (get arg 'bhl-caption)
		    (read-from-minibuffer "Caption: ")
		  "No summary" ))
	       (tbl (bhl-map-table)))
	  (insert (bhl-table-beg arg tbl))
	  (mapcar
	   (lambda (str)
	     (let (header)
	       (insert (mapconcat
			(lambda (strg)
			  (if (string-match "[ \t]*\\(@.*\\)@.*" strg)
			      (progn
				(aset strg (match-beginning 1) ?  )
				(aset strg (match-end 1) ?  )
				(setq header (cond ((eq arg 'latex) " \\hline\n")
						   ((eq arg 'sgml) "<hline>\n"))))
			    (setq header "\n"))
			  (bhl-cleanup-cell strg))
			str sep) endline header))) tbl)
	  (setq tbl (cdr tbl))
	  (insert
	   (cond ((eq arg 'latex)
		  (concat "\\end{tabular}\n"
			  (when caption
			    (concat "\\caption{" caption "}\n"))
			  "\\end{table}\n"))
		 (t (concat
		     "</tabular>\n"
		     (when caption
		       (concat "<caption>" caption "</caption>\n"))
		     "</table>\n")))))))))

(defun bhl-html-insert-preamble  (maintitle title subtitle author style lang encoding)
  "Insert the preambule of the HTML output.
MAINTITLE TITLE SUBTITLE AUTHOR STYLE LANG and ENCODING
are passed to this function."
  (insert bhl-html-doctype "\n<html lang=\""
	  (or lang (car bhl-i18n-conventions)) "\">\n<head>\n")
  (if encoding
      (insert "<meta http-equiv=\"Content-Type\" content=\"text/html; charset="
	      encoding "\">\n")
    (insert bhl-html-content-type "\n"))
  (insert "<meta name=\"author\" content=\"" author "\">\n")
  (let ((cpt 0))
    (while (< cpt (safe-length bhl-html-meta-alist))
      (insert "<meta name=\"" (car (nth cpt bhl-html-meta-alist))
	      "\" content=\"" (cdr (nth cpt bhl-html-meta-alist)) "\">\n")
      (setq cpt (1+ cpt))))
  (let ((cpt 0))
    (while (< cpt (safe-length bhl-html-link-alist))
      (insert "<link rel=\"" (car (nth cpt bhl-html-link-alist))
	      "\" href=\"" (cdr (nth cpt bhl-html-link-alist)) "\">\n")
      (setq cpt (1+ cpt))))
  (if style
      (insert "<link href=\"" style
	      "\" rel=\"stylesheet\" type=\"text/css\">\n")
    (when (not (equal bhl-html-default-style ""))
      (insert bhl-html-default-style "\n")))
  (insert "<title>" title "</title>\n</head>\n\n<body>\n")
  (search-forward maintitle nil t)
  (replace-match (concat (car bhl-html-title-tags)
			 title (cadr bhl-html-title-tags) "\n") t t)
  (when subtitle
    (insert (car bhl-html-subtitle-tags) subtitle
	    (cadr bhl-html-subtitle-tags))))

(defun bhl-protect-chars (chars)
  "Escape some CHARS in the LaTeX output."
  (goto-char (point-min))
  (let ((repl (if (eq chars bhl-latex-escaped-chars)
		  "\\" "@")))
    (while (car chars)
      (save-excursion
	(while (re-search-forward (concat "\\(.\\|^\\)\\("
					  (regexp-quote (car chars))
					  "\\)") nil t)
	  (unless (let ((cat (get-text-property (match-beginning 1) 'category)))
		    (or (eq cat 'verbatim)
			(eq cat 'comment)
                        (save-excursion
                          (beginning-of-line)
                          (string-match bhl-minipage-regexp (match-string 0)))))
	    (replace-match (if (equal (match-string 1) "\\") (match-string 2)
			     (concat (match-string 1) repl
				   (match-string 2))) t t))))
      (setq chars (cdr chars)))))

(defun bhl-latex-insert-preamble (maintitle title author date class options package)
  "Insert the preambule of the LaTeX output.
MAINTITLE TITLE AUTHOR DATE CLASS OPTIONS and PACKAGE are passed to
this options."
  (let ((local-class (or class bhl-latex-default-class))
	(local-options (or options bhl-latex-default-class-options)))
    (search-forward maintitle nil t)
    (replace-match "")
    (insert "\\documentclass["
	    (mapconcat (lambda (option) option)
		       local-options ",")
	    "]{" local-class "}\n")
    (let ((cpt 0))
      (while (< cpt (safe-length bhl-latex-packages-alist))
	(insert "\\usepackage")
	(unless (equal "" (cdr (nth cpt bhl-latex-packages-alist)))
	  (insert "[" (cdr (nth cpt bhl-latex-packages-alist)) "]"))
	(insert "{" (car (nth cpt bhl-latex-packages-alist)) "}\n")
	(setq cpt (1+ cpt))))
    (or (not package) (insert package))
    (dolist (line bhl-latex-extra-preambles)
      (insert line "\n"))
    (insert "\n\\begin{document}\n")
    (insert "\n\\title{" title "}")
    (insert "\n\\author{" author "}")
    (when date
      (insert "\n\\date{" date "}"))
    (insert "\n\\maketitle\n")))

(defun bhl-sgml-insert-preamble (maintitle title author)
  "Insert the preambule of the SGML output.
MAINTITLE TITLE and AUTHOR are passed to this function."
  (search-forward maintitle nil t)
  (replace-match "")
  (insert "<!DOCTYPE linuxdoc SYSTEM>\n<article>\n")
  (insert "<title>" title "</title>\n")
  (insert "<author>" author
	  "</author>\n\n")
  (insert "<abstract>")
  (save-excursion
    (re-search-forward (concat "^" (nth 0 bhl-sectioning-regexp-list)) nil t)
    (forward-line -1)
    (insert "</abstract>\n\n")))

(defun bhl-texi-insert-preamble (maintitle title subtitle author date lang encoding)
  "Insert the preambule of the TEXI output.
MAINTITLE TITLE SUBTITLE AUTHOR DATE LANG and ENCODING
are passed to this function."
  (search-forward maintitle nil t)
  (replace-match "")
  (insert "\\input texinfo @c -*-texinfo-*-\n")
  (insert "\n@c %**start of header\n")
  (insert "@setfilename "
	  (concat (file-name-sans-extension
		   (file-name-nondirectory buffer-file-name))
		  ".info" "\n"))
  (insert "@setchapternewpage " bhl-texi-setchapternewpage-flag "\n")
  (insert "@settitle " title "\n")
  (insert "@set AUTHOR " author "\n")
  (when date (insert "@set UPDATED " date "\n"))
  (insert "@documentlanguage " (or lang (car bhl-i18n-conventions)) "\n")
  (when encoding (insert "@documentencoding " encoding "\n"))
  (insert "@c %**end of header\n\n")
  (insert "@titlepage\n")
  (let ((titlepage-style
	 (if (not (equal bhl-local-texi-titlepage-style ""))
	     bhl-local-texi-titlepage-style
	   bhl-texi-titlepage-style)))
    (if (eq titlepage-style 'classical)
	(progn
	  (insert "@title " title "\n")
	  (when subtitle (insert "@subtitle " subtitle "\n"))
	  (insert "@author Copyright @copyright{} "
		  (format-time-string "%Y") " @value{AUTHOR}")
	  (insert (if date " - @value{UPDATED}\n" "\n")))
      (progn
	(insert "@center @titlefont{" title "}\n@sp 10\n")
	(when subtitle (insert "@center " subtitle "\n@sp 1\n"))
	(insert "@center @value{AUTHOR}\n@sp 1")
	(insert (if date "\n@center @value{UPDATED}\n" "\n"))
	(insert "@page\n@vskip 0pt plus 1filll\n")
	(insert "Copyright @copyright{} "
		(format-time-string "%Y")
		" @value{AUTHOR}\n"))))
  (insert "@end titlepage\n\n")
  (insert "@ifnottex\n@node top\n")
  (insert "@top " title "\n@end ifnottex\n\n"))

(defun bhl-parse-title (arg)
  "Find the title of the document for the ARG format."
  (if (save-excursion
	(re-search-forward (concat "^#!" (symbol-name arg)
				   "-title!\\(.*\\)") nil t))
      (bhl-match-string 1)
    (if (save-excursion
	  (re-search-forward "^#!title!\\(.*\\)" nil t))
	(bhl-match-string 1)
      (save-excursion
	(re-search-forward bhl-title-regexp nil t))
      (bhl-match-string 1))))

(defun bhl-insert-preamble (arg)
  "Insert the header of `bhl2html', `bhl2latex' and `bhl2sgml'.
ARG defines the output format."
  (goto-char (point-min))
  (let (bhl-local-author
	bhl-local-date
	bhl-local-subtitle
	bhl-local-lang
	bhl-local-encoding
	bhl-local-html-style
	bhl-local-latex-class
	bhl-local-latex-class-options
	(bhl-local-latex-package (bhl-parse-latex-package))
	(bhl-main-title
	 (save-excursion
	   (re-search-forward bhl-title-regexp nil t)
	   (bhl-match-string 1)))
	(bhl-local-title (bhl-parse-title arg)))
    (bhl-parse-header)
    (unless bhl-local-author (setq bhl-local-author user-full-name))
    (bhl-parse-specifications arg)
    (cond ((eq arg 'html)
	   (bhl-html-insert-preamble
	    bhl-main-title bhl-local-title bhl-local-subtitle
	    bhl-local-author bhl-local-html-style
	    bhl-local-lang bhl-local-encoding))
	  ((eq arg 'latex)
	   (bhl-latex-insert-preamble
	    bhl-main-title bhl-local-title bhl-local-author bhl-local-date
	    bhl-local-latex-class bhl-local-latex-class-options bhl-local-latex-package))
	  ((eq arg 'sgml)
	   (bhl-sgml-insert-preamble
	    bhl-main-title bhl-local-title bhl-local-author))
	  ((eq arg 'texi)
	   (bhl-texi-insert-preamble
	    bhl-main-title bhl-local-title bhl-local-subtitle
	    bhl-local-author bhl-local-date
	    bhl-local-lang bhl-local-encoding))
	  ((eq arg 'wiki)
	   (insert "* ") (end-of-line) (insert " *")
	     (when (get arg 'bhl-center) (center-line))
	     (when bhl-local-subtitle
	       (insert "\n\n" (concat "* " bhl-local-subtitle " *"))
	       (when (get arg 'bhl-center) (center-line))))
	  (t (insert "%% ") (end-of-line) (insert " %%")
	     (when (get arg 'bhl-center) (center-line))
	     (when bhl-local-subtitle
	       (insert "\n\n" (concat "% " bhl-local-subtitle " %"))
	       (when (get arg 'bhl-center) (center-line)))))))


;;;; 7 - TABLE OF CONTENTS

;; Make the table of content
(defun bhl-make-toc ()
  "Create the table of contents.
Create the list of section in `bhl-tsl'
and the list of corresponding points in `bhl-tpl'."
  (setq bhl-tsl nil)
  (setq bhl-tpl nil)
  (save-excursion
    (let ((position (point)))
      (goto-char (point-min))
      (while (re-search-forward
	      (concat "^" (nth 0 bhl-sectioning-regexp-list) ".*$") nil t)
	(setq bhl-tsl (append bhl-tsl (list (bhl-match-string 0))))
	(setq bhl-tpl (append bhl-tpl (list (match-end 0))))
	(let ((limit (save-excursion
		       (re-search-forward
			(concat "^" (nth 0 bhl-sectioning-regexp-list)
				".*$\\|\\'") nil t)))
	      ssection  ppection)
	  (while (re-search-forward
		  (concat "^" (nth 1 bhl-sectioning-regexp-list) ".*$")
		  limit t)
 	    (setq ssection (append ssection (list (bhl-match-string 0))))
 	    (setq ppection (append ppection (list (match-end 0))))
	    (let ((limit2 (save-excursion
			    (re-search-forward
			     (concat "^" (nth 1 bhl-sectioning-regexp-list) ".*$\\|^"
				     (nth 0 bhl-sectioning-regexp-list)
				     ".*$\\|\\'") nil t)))
		  sssection pppection)
	      (while (re-search-forward
		      (concat "^" (nth 2 bhl-sectioning-regexp-list) ".*$")
		      limit2 t)
		(setq sssection (append sssection (list (bhl-match-string 0))))
		(setq pppection (append pppection (list (match-end 0)))))
	      (unless (not sssection)
		(setq ssection (append ssection (list sssection)))
		(setq ppection (append ppection (list pppection))))))
	  (unless (not ssection)
	    (setq bhl-tsl (append bhl-tsl (list ssection)))
	    (setq bhl-tpl (append bhl-tpl (list ppection))))))))
  (null bhl-tsl))

;; Convert the sections' prefix according to the target style
(defsubst bhl-cvt-sec (arg convert)
  "Convert ARG according to the CONVERT style."
  (let ((style (or convert bhl-sectioning-default-style)))
    (cond ((eq style 'num)
	   (concat (number-to-string arg) "."))
	  ((eq style 'alpha)
	   (concat (char-to-string (+ 64 arg)) "."))
	  ((eq style 'aster) "*")
	  ((eq style 'equal-sign) "="))))

;; Update the table of content
(defun bhl-update-toc (&optional convert)
  "Update and maybe CONVERT the table of contents."
  (interactive)
  (unless (eq bhl-sectioning-default-style 'my)
    (save-excursion
      (let ((scpt 0)
	    (repl ""))
	(goto-char (point-min))
	(while (re-search-forward
		(concat "^" (nth 0 bhl-sectioning-regexp-list)
			"\\(.*\\)$") nil t)
	  (setq scpt (1+ scpt))
	  (setq repl (bhl-cvt-sec scpt convert))
	  (replace-match (concat repl " " (match-string 1)) t t)
	  (let ((sscpt 0)
		(rrepl "")
		(limit (save-excursion
			 (save-match-data
			   (re-search-forward
			    (concat "^" (nth 0 bhl-sectioning-regexp-list)
				    ".*$\\|\\'") nil t)))))
	    (while (re-search-forward
		    (concat "^" (nth 1 bhl-sectioning-regexp-list)
			    "\\(.*\\)$") limit t)
	      (setq sscpt (1+ sscpt))
	      (setq rrepl (bhl-cvt-sec sscpt convert))
	      (replace-match (concat repl rrepl " " (match-string 1)) t t)
	      (let ((ssscpt 0)
		    (rrrepl "")
		    (limit2 (save-excursion
			      (save-match-data
				(re-search-forward
				 (concat "^" (nth 1 bhl-sectioning-regexp-list)
					 ".*$\\|^" (nth 0 bhl-sectioning-regexp-list)
					 ".*$\\|\\'") nil t)))))
		(while (re-search-forward
			(concat "^" (nth 2 bhl-sectioning-regexp-list)
				"\\(.*\\)$") limit2 t)
		  (setq ssscpt (1+ ssscpt))
		  (setq rrrepl (bhl-cvt-sec ssscpt convert))
		  (replace-match (concat repl rrepl rrrepl
					 " " (match-string 1)) t t))))))))
    (message "Table of contents updated.")))

;; View log in a new buffer
(defun bhl-view-log ()
  "Display the log messages of the last ARG conversion."
  (interactive)
  (with-output-to-temp-buffer "*Log*"
    (princ "BHL conversion log:\n")
    (princ "===================\n\n")
    (let ((cpt 0))
      (when (equal bhl-conversion-log nil)
	(princ "* Last conversion correctly achieved."))
      (while (nth cpt bhl-conversion-log)
	(princ (concat "* " (nth cpt bhl-conversion-log) "\n"))
	(setq cpt (1+ cpt))))))

(defun bhl-parse-point-toc (toc depth)
  "Parse TOC with DEPTH into a list of points."
  (mapcar (lambda (elem)
	    (if (listp elem)
		(unless (< (1- depth) 1)
		  (bhl-parse-point-toc elem (1- depth)))
	      (setq bhl-toc-point-list
		    (append bhl-toc-point-list (list elem)))))
	  toc))

(defun bhl-generate-toc (depth &optional html)
  "Insert a toc of DEPTH, possibly in HTML format."
  (when html (insert "<ul>\n"))
  (setq bhl-toc-point-list nil)
  (bhl-parse-point-toc bhl-tpl depth)
  (bhl-parse-toc bhl-tsl depth 0 html)
  (when html (insert "</ul>\n")))

(defun bhl-parse-toc (toc depth indent html)
  "Parse TOC with DEPTH and INDENT, possibly in HTML format."
  (mapcar
   (lambda (elem)
     (if (listp elem)
	 (unless (< (1- depth) 1)
	   (when html (insert (concat (make-string (* 2 indent) 32)
				      " <ul>\n")))
	   (bhl-parse-toc elem (1- depth) (1+ indent) html)
	   (when html (insert (concat (make-string (* 2 indent) 32)
				      " </ul>\n"))))
       (insert (if html (concat (make-string (* 2 indent) 32)
				" <li><A href=\"#"
				(bhl-string-to-anchor elem)
				"\">" elem "</a></li>\n")
		 (concat (make-string (1+ (* 2 indent)) 32)
			 elem "\n"))))) toc))

(define-key bhl-toc-mode-map [(mouse-2)] 'bhl-toc-mouse-visit)
(define-key bhl-toc-mode-map [?\r] 'bhl-quit-temp-buffer)
(define-key bhl-toc-mode-map [(left)] 'bhl-visit-location-previous)
(define-key bhl-toc-mode-map [(right)] 'bhl-visit-location-next)
(define-key bhl-toc-mode-map [(up)] 'bhl-visit-location-previous)
(define-key bhl-toc-mode-map [(down)] 'bhl-visit-location-next)
(define-key bhl-toc-mode-map "q" 'bhl-quit-temp-buffer)
(define-key bhl-toc-mode-map "f" 'bhl-quit-temp-buffer)
(define-key bhl-toc-mode-map "v" 'bhl-quit-temp-buffer)
(define-key bhl-toc-mode-map "n" 'bhl-visit-location-next)
(define-key bhl-toc-mode-map "p" 'bhl-visit-location-previous)
(define-key bhl-toc-mode-map "<" 'bhl-visit-location-top)
(define-key bhl-toc-mode-map ">" 'bhl-visit-location-bottom)
(define-key bhl-toc-mode-map "?" 'bhl-toc-show-help)
(define-key bhl-toc-mode-map "1" 'bhl-show-toc-1)
(define-key bhl-toc-mode-map "2" 'bhl-show-toc-2)
(define-key bhl-toc-mode-map "3" 'bhl-show-toc-3)
(define-key bhl-toc-mode-map "i" 'bhl-toc-insert-toc)

;; BHL TOC menu
(easy-menu-define bhl-toc-menu bhl-toc-mode-map
  "Menu of the BHL minor mode"
  '("Toc"
    ["Next item" bhl-visit-location-next t]
    ["Previous item" bhl-visit-location-previous t]
    ["Top" bhl-visit-location-top t]
    ["Bottom" bhl-visit-location-bottom t]
    ["Follow" bhl-quit-temp-buffer t]
    "---"
    ("Toc depth"
     ["Sections" bhl-show-toc-1 t]
     ["Subsections" bhl-show-toc-2 t]
     ["Subsubsections" bhl-show-toc-3 t])
    "---"
    ["Quick help" bhl-toc-show-help t]))

;; BHL TOC mode
(defun bhl-toc-mode ()
  "A major mode for browsing the table of content of a BHL document.

\\{bhl-toc-mode-map}"
  (kill-all-local-variables)
  (use-local-map bhl-toc-mode-map)
  (easy-menu-add bhl-toc-menu)
  (setq mode-name "BHL toc")
  (setq major-mode 'bhl-toc-mode))

(defun bhl-show-toc (&optional depth)
  "Create a new buffer *toc* with a browsable table of contents.
The optional argument DEPTH specifies the toc depth."
  (interactive "P")
  (bhl-update-toc)
  (if (not (bhl-make-toc))
      (progn
	(switch-to-buffer-other-window (get-buffer-create "*toc*"))
	(bhl-toc-mode)
	(setq buffer-read-only nil)
	(kill-region (point-min) (point-max))
	(insert bhl-intro-toc " (? for quick help)\n\n")
	(bhl-generate-toc
	 (or depth bhl-default-toc-depth))
	(bhl-highlight-toc-buffer)
	(setq buffer-read-only t)
	(setq bhl-toc-temporary-depth
	      (or depth bhl-default-toc-depth))
	(bhl-shrink-window-to-fit)
	(goto-line 3))
    (message "There is no (sub)section in this buffer.")))

(defun bhl-toc-mouse-visit (event)
  "Visit the item that is clicked on with EVENT."
  (interactive "e")
  (mouse-set-point event)
  (bhl-visit-location (event-end event)))

(defun bhl-visit-location (arg)
  "Visit the toc/lol item of the current line.
ARG is the toc location."
  (cond ((eq arg 'top) (goto-line 3))
	((eq arg 'bottom)
	 (goto-char (point-max))
	 (forward-line -1))
	((numberp arg) (forward-line arg)))
  (beginning-of-line)
  (let* ((frombuffer (buffer-name))
	 (nombre
	  (if (equal frombuffer "*toc*")
	      (nth (- (string-to-number
		       (substring (what-line) 5)) 3)
		   bhl-toc-point-list)
	    (progn (beginning-of-line)
		   (re-search-forward "\\([0-9]+\\) : " nil t)
		   (string-to-int (bhl-match-string 1))))))
    (when (numberp nombre)
      (other-window 1)
      (goto-char nombre)
      (recenter 0)
      (switch-to-buffer-other-window frombuffer))))

(defun bhl-skip-toc (&optional arg)
  "Convert the toc according to the ARG output format.
With no ARG, erase the toc in the source file."
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((beg (if (search-forward bhl-intro-toc nil t)
		     (match-beginning 0)))
	    (end (if (search-forward bhl-end-toc nil t)
		     (progn (forward-line 1) (point)))))
	(if (not (eq beg end))
	    (progn
	      (kill-region beg end)
	      (cond ((and (eq arg 'texi)
			  (get arg 'bhl-toc))
		     (insert "@contents\n"))
		    ((and (eq arg 'txt)
			  (get arg 'bhl-toc))
		     (bhl-generate-toc bhl-default-toc-depth))
		    ((and (eq arg 'wiki)
			  (get arg 'bhl-toc))	;; wiki creates its own TOC
		     (insert bhl-intro-toc "\n\n" "%TOC%\n\n" 
			     bhl-end-toc "\n"))
		    ((and (eq arg 'html)
			  (get arg 'bhl-toc))
		     (bhl-generate-toc bhl-default-toc-depth t))
		    ((and (eq arg 'latex)
			  (get arg 'bhl-prefix)
			  (get arg 'bhl-toc))
		     (insert "\\setcounter{tocdepth}{"
			     (number-to-string bhl-default-toc-depth)
			     "}\n"  "\\tableofcontents\n")))))))))

(defsubst bhl-insert-lol ()
  "Insert the list of links at point."
  (interactive)
  (if (setq bhl-local-lol-list (bhl-map-lol))
      (let ((lol bhl-local-lol-list))
	(while (car lol)
	  (insert (cdar lol) "\n")
	  (setq lol (cdr lol))))
    (message "There is no link in this buffer.")))

(defun bhl-insert-toc (&optional depth)
  "Insert the table of contents in the source file.
Optional argument DEPTH is the toc depth.
Toc place is defined in `bhl-toc-location'."
  (interactive "P")
  (if (not (bhl-make-toc))
      (progn
	(bhl-update-toc)
	(cond ((equal bhl-toc-location "top")
	       (save-excursion
		 (goto-char (point-min))
		 (re-search-forward bhl-title-regexp nil t)
	     (forward-line 1)
	     (bhl-skip-toc)
	     (forward-line 1)
	     (insert bhl-intro-toc "\n\n")
	     (bhl-generate-toc (or depth bhl-default-toc-depth))
	     (insert "\n" bhl-end-toc "\n")))
	  ((equal bhl-toc-location "bottom")
	   (save-excursion
	     (bhl-skip-toc)
	     (goto-char (point-max))
	     (insert bhl-intro-toc "\n\n")
	     (bhl-generate-toc (or depth bhl-default-toc-depth))
	     (insert "\n" bhl-end-toc)))
	  ((equal bhl-toc-location "point")
	   (save-excursion
	     (bhl-skip-toc)
	     (insert bhl-intro-toc "\n\n")
	     (bhl-generate-toc (or depth bhl-default-toc-depth))
	     (insert "\n" bhl-end-toc))))
    (message (concat "Table of content inserted at the "
		     bhl-toc-location ".")))
    (message "There is no (sub)section in this buffer.")))

(defun bhl-toc-insert-toc ()
  "Insert the toc from the *toc* buffer."
  (interactive)
  (bhl-quit-temp-buffer)
  (bhl-insert-toc bhl-toc-temporary-depth))

(defun bhl-show-toc-1 ()
  "Display the toc in the *toc* buffer, DEPTH set to one."
  (interactive)
  (bhl-quit-temp-buffer)
  (bhl-show-toc 1))

(defun bhl-show-toc-2 ()
  "Display the toc in the *toc* buffer, DEPTH set to two."
  (interactive)
  (bhl-quit-temp-buffer)
  (bhl-show-toc 2))

(defun bhl-show-toc-3 ()
  "Display the toc in the *toc* buffer, DEPTH set to three."
  (interactive)
  (bhl-quit-temp-buffer)
  (bhl-show-toc 3))

(defun bhl-toc-show-help ()
  "Display a quick help for the browsable toc."
  (interactive)
  (message "[n] - next | [p] - previous | [<] - begin | [>] - end | [123] - depth | [i] insert toc"))

(defun bhl-visit-location-next ()
  "Visit the next item of the toc/lol buffer."
  (interactive)
  (bhl-visit-location 1))

(defun bhl-visit-location-previous ()
  "Visit the previous item of the toc/lol buffer."
  (interactive)
  (bhl-visit-location -1))

(defun bhl-visit-location-top ()
  "Visit the first item of the toc/lol buffer."
  (interactive)
  (bhl-visit-location 'top))

(defun bhl-visit-location-bottom ()
  "Visit the last item of the toc/lol buffer."
  (interactive)
  (bhl-visit-location 'bottom))

(defun bhl-quit-temp-buffer ()
  "Quit the current (top/lol) buffer."
  (interactive)
  (kill-this-buffer)
  (other-window 1)
  (delete-other-windows))

(define-key bhl-lol-mode-map [(left)] 'bhl-visit-location-previous)
(define-key bhl-lol-mode-map [(right)] 'bhl-visit-location-next)
(define-key bhl-lol-mode-map [(up)] 'bhl-visit-location-previous)
(define-key bhl-lol-mode-map [(down)] 'bhl-visit-location-next)
(define-key bhl-lol-mode-map "q" 'bhl-quit-temp-buffer)
(define-key bhl-lol-mode-map "f" 'bhl-quit-temp-buffer)
(define-key bhl-lol-mode-map "v" 'bhl-quit-temp-buffer)
(define-key bhl-lol-mode-map "n" 'bhl-visit-location-next)
(define-key bhl-lol-mode-map "p" 'bhl-visit-location-previous)
(define-key bhl-lol-mode-map "<" 'bhl-visit-location-top)
(define-key bhl-lol-mode-map ">" 'bhl-visit-location-bottom)
(define-key bhl-lol-mode-map "?" 'bhl-lol-show-help)
(define-key bhl-lol-mode-map "i" 'bhl-lol-insert-lol)

(defsubst bhl-lol-insert-lol ()
  "Insert the list of links from the *lol* buffer."
  (interactive)
  (bhl-quit-temp-buffer)
  (bhl-insert-lol))

(defsubst bhl-lol-show-help ()
  "Show help in the bhl *lol* buffer."
  (interactive)
  (message "[n] - next | [p] - previous | [<] - begin | [>] - end | [i] insert lol | [q] quit"))

;; Lol menu
(easy-menu-define bhl-lol-menu bhl-lol-mode-map
  "Menu of the BHL minor mode"
  '("Lol"
    ["Next item" bhl-visit-location-next t]
    ["Previous item" bhl-visit-location-previous t]
    ["Top" bhl-visit-location-top t]
    ["Bottom" bhl-visit-location-bottom t]
    ["Follow" bhl-quit-temp-buffer t]
    "---"
    ["Quick help" bhl-lol-show-help t]))

;; Lol mode - browse the links of your document
(defun bhl-lol-mode ()
  "A major mode for browsing the list of links (lol) of a BHL document.

\\{bhl-lol-mode-map}"
  (kill-all-local-variables)
  (use-local-map bhl-lol-mode-map)
  (easy-menu-add bhl-lol-menu)
  (setq mode-name "BHL lol")
  (setq major-mode 'bhl-lol-mode))

(defun bhl-show-lol ()
  "Show the lol of the current BHL file."
  (interactive)
  (if (setq bhl-local-lol-list (bhl-map-lol))
      (progn
	(switch-to-buffer-other-window (get-buffer-create "*lol*"))
	(bhl-lol-mode)
	(setq buffer-read-only nil)
	(kill-region (point-min) (point-max))
	(insert "--- List of links (? for quick help)\n\n")
	(save-excursion
	  (add-text-properties (goto-char (point-min))
			       (re-search-forward "ks " nil t)
			       '(face bold)))
	(let* ((lol bhl-local-lol-list)
	       (size-max (length (int-to-string (caar (reverse lol))))))
	  (while (car lol)
	    (insert (make-string
		     (- size-max (length (int-to-string (caar lol)))) ?  )
		    (int-to-string (caar lol))
		    " : " (cdar lol) "\n")
	    (setq lol (cdr lol))))
	(bhl-highlight-url-lol-buffer)
	(bhl-highlight-wiki-lol-buffer)
	(setq buffer-read-only t)
	(bhl-shrink-window-to-fit)
	(goto-line 3))
    (message "There is no link in this buffer.")))

(defun bhl-highlight-url-lol-buffer ()
  "Highlight URLs in the *Lol* buffer."
  (goto-char (point-min))
  (let ((map (copy-keymap (current-local-map))))
    (define-key map [(mouse-2)] 'bhl-follow-url-at-mouse)
    (define-key map [(return)] 'bhl-follow-url-at-point)
    (while (re-search-forward (concat "^[ ]*\\([0-9]+\\) : \\("
				      bhl-generic-url-regexp "\\)$") nil t)
      (add-text-properties (match-beginning 1)
			   (match-end 1)
			   '(face bhl-url-face))
      (add-text-properties (match-beginning 2) (match-end 2)
			   `(category link local-map ,map mouse-face
				      highlight help-echo
				      "mouse-2: follow URL")))))

(defun bhl-highlight-wiki-lol-buffer ()
  "Highlight WikiNames in the *Lol* buffer."
  (goto-char (point-min))
  (let ((map (copy-keymap (current-local-map))))
    (define-key map [(mouse-2)] 'bhl-follow-wiki-name-at-mouse)
    (define-key map [(return)] 'bhl-follow-wiki-name-at-point)
    (while (re-search-forward (concat "^[ ]*\\([0-9]+\\) : \\("
				      bhl-wiki-names-regexp "\\)$") nil t)
      (add-text-properties (match-beginning 1)
			   (match-end 1)
			   '(face bhl-wiki-face))
      (add-text-properties (match-beginning 2) (match-end 2)
			   `(category link local-map ,map mouse-face
				      highlight help-echo
				      "mouse-2: follow WikiName")))))

(defun bhl-map-lol ()
  "Map lol and return a list of links."
  (let (lol-list case-fold-search)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat bhl-generic-url-regexp "\\|"
					bhl-wiki-names-regexp) nil t)
	(add-to-list 'lol-list
		     (cons (match-beginning 0)
			   (bhl-match-string 0)))))
    (reverse lol-list)))


;;;; 8 - Submit bug report
(defun bhl-submit-bug-report ()
  "Submit a bug report on BHL Mode."
  (interactive)
  (and
   (y-or-n-p "Do you want to submit a report on BHL Mode? ")
   (require 'reporter "reporter" t)
   (reporter-submit-bug-report
    bhl-help-address
    (concat "BHL Mode " bhl-version)
    (list
     ;; report all important variables
     'bhl-mode-hook
     'bhl-sectioning-default-style
     'bhl-verbatim-ignore
     'bhl-my-sectioning-regexp-list
     'bhl2html-properties-list
     'bhl2latex-properties-list
     'bhl2sgml-properties-list
     'bhl2txt-properties-list
     'bhl2wiki-properties-list
     'bhl-html-conversions-list
     'bhl-latex-conversions-list
     'bhl-sgml-conversions-list
     'bhl-txt-conversions-list
     'bhl-i18n-conventions
     'bhl-tab-width
     'bhl-default-toc-depth
     'bhl-toc-location
     'bhl-table-location
     'bhl-table-align
     'bhl-table-cell-align
     'bhl-html-table-param-alist
     'bhl-html-para-align
     'bhl-html-img-align
     'bhl-html-meta-alist
     'bhl-latex-packages-alist
     'bhl-latex-default-class
     'bhl-latex-default-class-options)
    nil
    nil
    "Hi Bastien,\n\n")))


;;;; 9 - BHL2XXX

(defun bhl2html ()
  "Convert the BHL source file into a HTML file."
  (interactive)
  (bhl2xxx 'html))

(defun bhl2latex ()
  "Convert the BHL source file into a LaTeX file."
  (interactive)
  (bhl2xxx 'latex))

(defun bhl2sgml ()
  "Convert the BHL source file into a SGML (Linuxdoc) file."
  (interactive)
  (bhl2xxx 'sgml))

(defun bhl2txt ()
  "Convert the BHL source file into a TXT file.
Strip font tags and URL/IMG square brackets."
  (interactive)
  (bhl2xxx 'txt))

(defun bhl2wiki ()
  "Convert the BHL source file into a WIKI file."
  (interactive)
  (bhl2xxx 'wiki))

(defun bhl2texinfo ()
  "Convert the BHL source file into a TEXINFO file."
  (interactive)
  (bhl2xxx 'texi))

;; Perform initializations
(defun bhl-convert-init (arg)
  "Initial conversion into ARG format."
  (save-buffer)
  (setq bhl-conversion-log nil)
  (setq bhl-xxx-conversions-list
	(get arg 'conversions-list))
  (let ((bhl-source-file buffer-file-name)
	(bhl-workfile-name
	 (concat (file-name-sans-extension buffer-file-name)
		 (cond ((eq arg 'latex) ".tex")
		       (t (concat "." (symbol-name arg)))))))
    (find-file-other-window bhl-workfile-name)
    (kill-region (point-min) (point-max))
    (setq tab-width 3)
    (insert-file-contents bhl-source-file)))

;; Add properties into the working buffer.
(defun bhl-add-text-properties (arg)
  "Add some text properties to the conversion buffer.
ARG is the output format."
  (goto-char (point-min))
  (save-excursion
    (while (re-search-forward "^--$" nil t)
      (add-text-properties (match-end 0)
			   (progn (re-search-forward "^--$" nil t)
				  (match-beginning 0))
			   '(category verbatim))))
  (save-excursion
    (let (case-fold-search)
      (while (re-search-forward
	      (concat bhl-img-regexp "\\|"
		      bhl-generic-url-regexp "\\|"
		      bhl-wiki-names-regexp) nil t)
	(add-text-properties (match-beginning 0) (match-end 0)
			     '(category link)))))
  (save-excursion
    (while (re-search-forward "[^\\]\\(#.*\\)$" nil t)
      (add-text-properties (match-beginning 1) (match-end 1)
			   '(category comment))))
  (when (eq arg 'latex)
    (save-excursion
      (while (re-search-forward bhl-minipage-regexp nil t)
	(add-text-properties (match-beginning 0) (match-end 0)
			     '(category comment)))))
  (when (eq arg 'texi)
    (save-excursion
      (while (re-search-forward "^|" nil t)
	(unless (eq (get-text-property (match-beginning 0) 'category) 'verbatim)
	  (add-text-properties (match-beginning 0)
			       (re-search-forward "^[ \t]*$" nil t)
			       '(category table)))))))

(defun bhl-convert-misc-1 (arg)
  "Miscellaneous conversions into ARG format.
This conversions are: special char, verbatim, horizontal rules
and LaTeX/TeX labels."
  (when (get arg 'bhl-i18n)
    (when (nth 1 bhl-i18n-conventions)
      (bhl-convert-char
       arg (eval (intern (concat "bhl-"
				 (car bhl-i18n-conventions)
				 "-punctuation")))))
    (when (nth 2 bhl-i18n-conventions)
      (bhl-convert-char arg (eval (intern (concat "bhl-" (car bhl-i18n-conventions)
				 "-quotation-marks")))))
    (when (nth 3 bhl-i18n-conventions)
      (bhl-convert-char
       arg (eval (intern (concat "bhl-" (car bhl-i18n-conventions)
				 "-special-chars")))))
    (bhl-add-to-conversion-log
     (concat "I18n conventions: " (car bhl-i18n-conventions) ".")))
  (if (get arg 'bhl-check)
      (progn (bhl-check-existing-tags arg)
	     (bhl-check-comment arg))
    (bhl-add-to-conversion-log
     "No check of dubious tags."))
  (when (nth 13 bhl-xxx-conversions-list)
    (bhl-convert-quote arg))
  (when (nth 0 bhl-xxx-conversions-list)
    (bhl-convert-verbatim arg))
  (when (nth 1 bhl-xxx-conversions-list)
    (bhl-convert-hr arg))
  (when (nth 1 bhl-xxx-conversions-list)
    (bhl-convert-hr1 arg))
  (when (nth 1 bhl-xxx-conversions-list)
    (bhl-convert-hr2 arg))
  (when (nth 7 bhl-xxx-conversions-list)
    (bhl-convert-tex arg)
    (bhl-add-to-conversion-log
     "LaTeX and TeX labels conversion."))
  (when (nth 8 bhl-xxx-conversions-list)
    (condition-case nil
	(progn (require 'footnote)
	       (if (eq arg 'sgml)
		   (bhl-skip-footnotes arg)
		 (bhl-convert-footnotes arg)))
      (error (bhl-add-to-conversion-log
              "BHL was unable to convert footnotes."))))
  (when (nth 9 bhl-xxx-conversions-list)
    (bhl-convert-minipage arg)))

(defun bhl-convert-misc-2 (arg)
  "Miscellaneous conversions into ARG format.
This conversion are: images, URLs, description lists, lists, tables."
  (when (nth 2 bhl-xxx-conversions-list)
    (bhl-convert-img arg))
  (when (and (nth 12 bhl-xxx-conversions-list)
	     bhl-is-a-local-wiki-flag)
    (bhl-convert-wiki-names arg)) ; sm XXX
  (when (nth 3 bhl-xxx-conversions-list)
    (bhl-convert-url arg)
    (when (eq arg 'latex)
      (bhl-add-to-conversion-log
       "Please check that you have included the URL package.")))
  (when (nth 4 bhl-xxx-conversions-list)
    (bhl-convert-description arg))
  (when (nth 5 bhl-xxx-conversions-list)
    (bhl-convert-list arg))
  (when (nth 10 bhl-xxx-conversions-list)
    (bhl-convert-comment arg))
  (when (nth 11 bhl-xxx-conversions-list)
    (bhl-convert-escape-char arg))
  (when (nth 6 bhl-xxx-conversions-list)
    (when (not (get arg 'bhl-caption))
      (bhl-add-to-conversion-log
       "No caption was asked for tables."))
    (bhl-convert-table arg)))

;; Convert paragraphs
(defun bhl-convert-para (arg)
  "Convert paragraphs into ARG format."
  (save-excursion
    (goto-char (point-min))
    (when (eq arg 'sgml)
      (re-search-forward "</abstract>" nil t)
      (end-of-line))
    (while (re-search-forward "^[\\'«\"&*_=a-zA-Z0-9@]" nil t)
      (goto-char (match-beginning 0))
      (if (not (or (eq (get-text-property (match-beginning 0) 'category) 'verbatim)
		   (save-excursion (forward-line -1)
				   (looking-at "<\\|[a-zA-Z0-9]"))
		   (memq t (mapcar (lambda (str) (looking-at str))
				   bhl-sectioning-regexp-list))))
	  (save-match-data
	    (cond ((or (eq arg 'sgml)
		       (equal bhl-html-para-align "none"))
		   (insert "<p>\n"))
		  (t (insert "\n<p align=\""
			     bhl-html-para-align "\">")))
	    (re-search-forward "^[ \t]*$" nil t)
	    (insert "</p>\n"))
	(forward-line 1)))))

;; Generic function for sections and tags conversion
(defun bhl-convert-sec-tag (arg)
  "Convert sections and tags into ARG format."
  (when (eq arg 'latex)
    (dolist (line bhl-latex-extra-body)
      (insert line "\n")))
  (let ((bhl-tag-list
	 (cdr (assq arg bhl-tag-syntax-alist)))
	(bhl-sec-list
	 (cdr (assq (if (and (eq arg 'latex)
			     (not (get arg 'bhl-prefix)))
			'latex-nonum arg)
		    bhl-section-syntax-alist))))
    (let ((cpt 0))
      (while (< cpt (length bhl-tag-list))
	(let ((mt (nth cpt bhl-tag-regexp-list))
	      (db (car (nth cpt bhl-tag-list)))
	      (fn (cadr (nth cpt bhl-tag-list))))
	  (bhl-replace-tag mt db fn))
	(setq cpt (1+ cpt))))
    (let ((cpt 0))
      (while (< cpt (length bhl-sec-list))
	(let ((mt (nth cpt bhl-sectioning-regexp-list))
	      (db (car (nth cpt bhl-sec-list)))
	      (fn (cadr (nth cpt bhl-sec-list))))
	  (bhl-replace-section arg mt db fn
			       (when (eq arg 'html)
				 (get arg 'bhl-prefix))
			       (when (eq arg 'txt)
				 (get arg 'bhl-center))))
	(setq cpt (1+ cpt))))))

;; Handling comments
(defun bhl-convert-comment (arg)
  "Convert comment into ARG format."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
	    (concat "\\([^\\\"]\\)\\(#\\)\\(.*\\)$") nil t)
      (unless (let ((cat (get-text-property (match-beginning 0) 'category)))
		(and (eq cat 'verbatim)
		     (memq 'comment bhl-verbatim-ignore)))
	(bhl-add-to-conversion-log
	 (format "Comment \"%s...\" converted."
		 (substring (match-string 3) 0
			    (if (< (length (match-string 3)) 20)
				(length (match-string 3))
			      20))))
	(cond ((eq arg 'latex)
	       (replace-match "%" t t nil 2))
	      ((eq arg 'texi)
	       (replace-match "@c " t t nil 2))
	      ((or (eq arg 'txt) (eq arg 'wiki))
	       (replace-match ""))
	      (t (replace-match
		  (concat (match-string 1)
			  "<!-- " (match-string 3) " -->") t t)))))))

;; (Un)comment regions
(defun bhl-comment-region (&optional prefix)
  "Comment the region or uncomment it when PREFIX."
  (interactive "P")
  (let* ((beg (region-beginning))
	 (end (region-end)))
    (save-excursion
      (goto-char beg)
      (if prefix
	  (while (re-search-forward "^#\\( \\)?" end t)
	    (setq end (- end (if (match-string 1) 2 1)))
	    (replace-match ""))
	(while (re-search-forward "^[^\n\r]" end t)
	  (setq end (+ 2 end))
	  (replace-match (concat "# " (match-string 0)) t t))))))

(defsubst bhl-hide-comment (&optional arg)
  "Hide comments according to ARG format.
With a prefix argument, unhide comments."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (let ((func (if arg 'remove-text-properties
		  'add-text-properties)))
      (while (re-search-forward "#.*$" nil t)
	(funcall func (match-beginning 0)
		 (match-end 0) '(invisible t))))))

;; Convert escape characters (attention escaping \ for bhl2latex)
(defun bhl-convert-escape-char (arg)
  "Convert escape characters for ARG format."
  (save-excursion
    (mapcar
     '(lambda (str)
	(goto-char (point-min))
	(while (search-forward (concat "\\" str) nil t)
	  (let ((cat (get-text-property (match-beginning 0) 'category)))
	    (unless (or (eq cat 'comment)
			(eq cat 'verbatim)
			(and (eq arg 'latex)
			     (or (equal str "_")
				 (equal str "#"))))
	      (bhl-add-to-conversion-log
	       (format "Escape character at point %d deleted."
		       (match-beginning 0)))
	      (cond ((eq arg 'latex)
		     (if (equal (match-string 0) "\\\\")
			 (replace-match "$\\backslash$" t t)
		       (replace-match (substring str (1- (safe-length str))) t t)))
		    (t (unless (equal (match-string 0) "\\\\")
			 (replace-match (substring str (1- (safe-length str))) t t))))))))
     bhl-escapable-chars)))

;; Convert WikiNames
(defun bhl-convert-wiki-names (arg)
  "Convert WikiNames according to the ARG format."
  (save-excursion
    (goto-char (point-min))
    (let (case-fold-search)
      (while (re-search-forward bhl-wiki-names-regexp nil t)
	(unless (let ((cat (get-text-property (1- (match-beginning 0))
					      'category)))
		  (or (member (match-string 0) bhl-non-wiki-names-list)
		      (and (eq cat 'verbatim)
			   (memq 'wikiname bhl-verbatim-ignore))
		      (eq cat 'comment)))
	  (let* ((wikiname (match-string 0))
		 (dwikiname (if bhl-downcase-wikifiles-names-flag
				(downcase wikiname)
			      wikiname)))
	    (cond ((eq arg 'html)
		   (replace-match (concat "<a href=\"" dwikiname ".html\">"
					  wikiname "</a>") t t))
		  ((eq arg 'texi)
		   (replace-match (concat "@emph{" wikiname "}") t t))
		  ((eq arg 'txt)
		   (replace-match wikiname t t))
		  ((eq arg 'latex)
		   (replace-match (concat "\\emph{" wikiname "}") t t))
		  ((eq arg 'sgml)
		   (replace-match (concat "<htmlurl url=\"" dwikiname ".html\" name=\""
					  wikiname "\">") t t)))))))))

;; Convert some special chars in the title strings
(defun bhl-clean-title (arg title-regexp)
  "Clean the title according to the ARG format and the TITLE-REGEXP."
  (save-excursion
    (goto-char (point-min))
    (let ((string (prog2 (re-search-forward title-regexp nil t)
		      (bhl-match-string 1)
		    (replace-match "" t t nil 1)))
	  (repl-alist (cond ((eq arg 'latex) '(("\\*" . "*")
					       ("&" . "\\&")
					       ("$" . "\\$")
					       ("%" . "\\%")
					       ("{" . "\\{")
					       ("}" . "\\}")))
			    ((eq arg 'texi) '(("\\*" . "*")
					      ("\\_" . "_")
					      ("\\#" . "#")
					      ("é" "@'e")
					      ("è" "@`e")
					      ("à" "@`a")
					      ("ù" "@^u")
					      ("ê" "@^e")
					      ("ô" "@^o")
					      ("î" "@^{@dotless{i}}")
					      ("â" "@^a")
					      ("ù" "@`u")
					      ("ç" "@,{c}")
					      ("@" . "@@")
					      ("{" . "@{")
					      ("}" . "@}")))
			    (t '(("\\*" . "*")
				 ("\\_" . "_")
				 ("\\#" . "#"))))))
      (while (car repl-alist)
	(setq string (bhl-replace-regexp-in-string
		      (regexp-quote (caar repl-alist))
		      (cdar repl-alist)
		      string t t))
	(setq repl-alist (cdr repl-alist)))
      (insert string))))

(defun bhl-clean-title-main (arg)
  "Clean up each title string according to the ARG format."
  (mapcar '(lambda (regexp) (bhl-clean-title arg regexp))
	  `(,bhl-title-regexp
	    "^#!texi-title!\\(.*\\)$"
	    "^#!latex-title!\\(.*\\)$"
	    "^#!html-title!\\(.*\\)$"
	    "^#!sgml-title!\\(.*\\)$"
	    "^#!wiki-title!\\(.*\\)$"
	    "^#!title!\\(.*\\)$"
	    "^#!subtitle!\\(.*\\)$")))

;; Parse some informations concerning current buffer
(defun bhl-parse-header (&optional nodelete)
  "Parse global informations concerning the current file.
Don't delete lines concerning headers if NODELETE is non-nil."
  (save-excursion
    (mapcar
     (lambda (elem)
       (goto-char (point-min))
       (when (re-search-forward
	      (concat "^#!\\(" elem "\\)![ \t]*\\(.*\\)$")  nil t)
	 (let ((match (bhl-match-string 2)))
	   (set (intern (concat "bhl-local-" elem))
		(if (and (equal elem "date")
			 (string-match "%" match))
		    (format-time-string match)
		  match)))))
     '("date" "author" "title" "subtitle" "lang" "encoding")))
  (when bhl-local-lang
    (setcar bhl-i18n-conventions bhl-local-lang)))
    
;; Convert specification strings
(defun bhl-parse-specifications (arg)
  "Parse specifications strings for ARG output.
A specification string looks like:

#!html!check:noprefix:nocaption:toc

In this example, the `bhl2html' conversion checks for dubious tags,
delete sections' prefix, does not ask for tables' caption and insert
a table of content."
  (save-excursion
    (goto-char (point-min))
    (save-excursion
      (re-search-forward (concat "^#!" (symbol-name arg) "!\\(.*\\)$") nil t)
      (let ((string (bhl-match-string 1)))
	(mapcar (lambda (str)
		  (mapcar (lambda (spec)
			    (cond ((equal str spec)
				   (put arg (intern (concat "bhl-" spec)) t))
				  ((equal str (concat "no" spec))
				   (put arg (intern (concat "bhl-" spec)) nil))
				  (t nil)))
			  '("check" "prefix" "caption" "center" "toc" "i18n" "tas")))
		(split-string string ":"))))
    (when (and (eq arg 'latex)
	       (save-excursion
		 (re-search-forward "^#!latex-class!\\(.*\\)$" nil t)))
      (setq bhl-local-latex-class (bhl-match-string 1)))
    (when (and (eq arg 'latex)
	       (save-excursion
		 (re-search-forward "^#!latex-options!\\(.*\\)$" nil t)))
	(setq bhl-local-latex-class-options
	      (split-string (bhl-match-string 1) ":")))
    (when (and (eq arg 'texi)
	       (save-excursion
		 (re-search-forward "^#!texi-style!\\(.*\\)$" nil t)))
	(setq bhl-local-texi-titlepage-style
	      (intern (bhl-match-string 1))))
    (when (and (eq arg 'html)
	       (save-excursion
		 (re-search-forward "^#!html-style!\\(.*\\)$" nil t)))
      (setq bhl-local-html-style (bhl-match-string 1)))))

(defun bhl-parse-latex-package ()
  "Parse package specifications for the bhl2latex conversion."
  (interactive)
  (when (save-excursion
	  (re-search-forward "^#!latex-packages!\\(.*\\)$" nil t))
    (let ((result "")
	  (str (split-string (bhl-match-string 1) ":")))
      (mapcar
       (lambda (strg)
	 (if (string-match "\\[.*\\]" strg)
	     (setq result
		   (concat result "\\usepackage"
			   (substring strg
				      (match-beginning 0)
				      (match-end 0))
			   "{" (if (equal (match-end 0) (length strg))
				   (substring strg 0 (match-beginning 0))
				 (substring strg (match-end 0))) "}\n"))
	   (setq result
		 (concat result "\\usepackage{" strg "}\n"))))
       str) result)))

;; A wrapper function designed to allow the use of BHL in scripts.
;; Note that Emacs has no very useful and obvious way to read
;; characters from stdin into a buffer, so it will be the caller's job
;; to create a temp file if it wants to be able to take stdin input.
(defun bhl2xxx-batch-wrapper (file format &optional outstream)
  "Convert FILE into FORMAT and send the format to the OUTSTREAM."
  (find-file file)
  (bhl-mode)
  (bhl2xxx format)
  (princ (buffer-string) outstream)
  (terpri))

;; A generic function that converts BHL source files.
(defun bhl2xxx (arg)
  "Convert the BHL source file into a HTML, SGML or LaTeX file.
ARG is the output format.  OFFLINE is non-nil when BHL is called in batch
mode."
  (bhl-convert-init arg)
  (bhl-add-text-properties arg)
  (bhl-clean-title-main arg)
  (bhl-insert-preamble arg)
  (narrow-to-region (if (eq arg 'sgml)
			(search-forward "</abstract>")
		      (point)) (point-max))
  (bhl-ignore-regexp-line)
  (when (eq arg 'latex)
    (bhl-protect-chars bhl-latex-escaped-chars))
  (when (eq arg 'texi)
    (bhl-protect-chars bhl-texi-escaped-chars))
  (bhl-convert-misc-1 arg)
  (when (or (eq arg 'html) (eq arg 'sgml))
    (condition-case nil
	(bhl-convert-para arg)
      (error (bhl-add-to-conversion-log
	      "BHL was unable to convert paragraphs properly."))))
  (bhl-make-toc)
  (bhl-update-toc)
  (bhl-skip-toc arg)
  (bhl-convert-sec-tag arg)
  (bhl-convert-misc-2 arg)
  (bhl-add-to-conversion-log
   (concat "Sections' prefix: "
	   (symbol-name (get arg 'bhl-prefix)) "."))
  (widen)
  (goto-char (point-max))
  (insert (cond ((eq arg 'latex)  (concat bhl-latex-footer "\n\n\n\\end{document}"))
		((eq arg 'html)  (concat bhl-html-footer "\n\n</body>\n</html>"))
		((eq arg 'sgml)  (concat bhl-sgml-footer "\n</article>"))
		((eq arg 'wiki)  (concat bhl-wiki-footer ""))
		((eq arg 'texi) (concat bhl-texi-footer "\n\n@bye"))
		(t  "")))
  (run-hooks (get arg 'after-conversion-hook))
  (run-hooks 'bhl-after-conversion-hook)
  (goto-char (point-min))
  (message "Converting into %s format...done. "
	   (symbol-name arg)))

(provide 'bhl)

;;; bhl.el ends here
