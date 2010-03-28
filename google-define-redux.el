;;; google-define-redux.el --- extends google-define.el 
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright (c) 2007,2008,2009,2010 Jeremy English <jhe@jeremyenglish.org>
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: google-define-redux.el
;; AUTHOR: MON KEY, Jeremy English
;; MAINTAINER: MON KEY
;; CREATED: 
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; google-define-redux provides extensions for google-define.el
;; Extends google-define with fontification of definition buffer.
;; No longer relies on `with-output-to-temp-buffer'
;; Adds parsing tokens for programatically extracting definitions.
;
;; FUNCTIONS:►►►
;; `google-define-get-command', `google-define-parse-buffer', `google-define',
;; `google-define-font-lock', `google-define-kill-def-buffers', 
;; `google-define-word-at-point'
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; FACES:
;; `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
;; `gg-def-inition',
;;
;; CONSTANTS:
;; `*google-define-html-entry-table*'
;;
;; VARIABLES:
;; `*google-define-view-map*', `*url-get-buffer*', 
;; `*google-define-buffer-suffix*'
;;
;;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; TODO:
;; Integrate Wordnet features. 
;; :SEE (URL `http://wordnet.cs.princeton.edu')
;;
;; Google's "define:<SOME-WORD>" returns wordnet definitions.  These appear in
;; the `*google-define-get-buffer*' returned by `google-define-get-command' as:
;; <a href="/url?q=http://wordnetweb.princeton.edu/perl/webwn{WORDNET-PARAMS}>
;; It might be interesting to integrate/adapt William Xu's wordnet interface: 
;; :SEE (URL `http://xwl.appspot.com/ref/wordnet.el')
;; :SEE (URL `http://github.com/xwl/xwl-elisp/raw/master/wordnet.el')
;; Or maybe Henry G. Weller's adaptation of above for use w/ `org-mode':
;; :SEE (URL `http://www.emacswiki.org/emacs/wn-org.el')
;; using the REST
;;
;; NOTES:
;; The Required functions listed below are also provided in a dedicated library
;; with :FILE google-define-redux-supplemental.el Though this may not always be
;; current and it is recommended to load the required libraries instead.
;; 
;; SNIPPETS:
;;
;; REQUIRES:
;; `mon-g2be'                         <- :FILE mon-utils.el
;; `mon-string-justify-left'          <- :FILE mon-utils.el
;; `mon-buffer-exists-p'              <- :FILE mon-utils.el
;; `mon-buffer-exists-so-kill'        <- :FILE mon-utils.el
;; `mon-help-temp-docstring-display'  <- :FILE mon-doc-help-utils.el
;; `mon-help-KEY-tag'                 <- :FILE mon-doc-help-utils.el
;;
;;
;; THIRD-PARTY-CODE:
;; Following are the modifications made to Jeremy English's google-define.el 
;; :SEE (URL `http://www.emacswiki.org/emacs/google-define')
;; :SEE (URL `http://www.emacswiki.org/emacs/google-define.el')
;; SEE BOF for original versions.
;; These are provided verbatim:
;; `google-define-word-at-point'
;; `*google-define-html-entry-table*'
;; Following functions are modified:
;; `google-define-get-command'
;; `google-define-parse-buffer'
;; `google-define'
;; Following functions were replaced with inline forms:
;; `google-define-replace-unicode'
;; `google-define-replace-html'
;; `google-define-replace-string'
;; `google-define-ascii-entry'
;; `google-define-number-entry'
;;
;; URL: "http://www.emacswiki.org/emacs/google-define-redux.el"
;; FIRST-PUBLISHED: <Timestamp: #{2010-02-04T21:39:15-05:00Z}#{10055} - by MON>
;;
;; EMACSWIKI: 
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-02-03T13:41:11-05:00Z}#{10053} - by MON>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

(require 'font-lock)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-20T11:44:49-04:00Z}#{10116} - by MON KEY>
(defgroup google-define-redux nil
  "*Extensions for `google-define'.\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers', `mon-string-justify-left'
`*google-define-buffer-suffix*' `*google-define-get-buffer*',
`*google-define-html-entry-table*', `*google-define-get-command',
`gg-def-base', `gg-def-num', `gg-def-delim', `gg-def-inition', `gg-def-defined'.\n►►►"
  :group 'google-define
  :prefix "google-define-"
  :link '(url-link :tag "EmacsWiki" "http://www.emacswiki.org/emacs/google-define-redux.el"))
;;
;;; :TEST-ME (symbol-plist 'google-define-redux)
;;; :TEST-ME (documentation-property 'google-define-redux 'group-documentation)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-20T11:44:49-04:00Z}#{10116} - by MON KEY>
(defgroup google-define-redux-faces nil
  "*Face definitions for google-define-redux extensions to google-define.\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-num', `gg-def-inition',
`gg-def-defined', `mon-doc-help-utils-faces'.\n►►►"
  :group 'google-define-redux
  :group 'faces
  :group  (or (when (featurep 'mon-doc-help-utils)  'mon-doc-help-utils-faces) nil)
  :prefix "gg-def-")
;;
;;; :TEST-ME (symbol-plist 'google-define-redux-faces)
;;; :TEST-ME (documentation-property 'google-define-redux-faces 'group-documentation)

;;; ==============================
;;; :COURTESY Jeremy English :HIS google-define.el 
;;; :MODIFICATIONS <Timestamp: #{2010-03-20T12:34:07-04:00Z}#{10116} - by MON KEY>
;;;  Replaced Character literal for SOFT HYPHEN (173, #o255, #xad) with hex representation.
;;; :CREATED <Timestamp: #{2010-03-20T12:15:24-04:00Z}#{10116} - by MON KEY>
(unless (bound-and-true-p *google-define-html-entry-table*)
(defconst *google-define-html-entry-table*
  `(("&#34;"  "&quot;" "\"")  ("&#38;"  "&amp;" "&")    ("&#39;" "&yow;" "'")
    ("&#62;"  "&gt;" ">")     ("&#160;" "&nbsp;" " ")   ("&#161;" "&iexcl;" "¡")
    ("&#162;" "&cent;" "¢")   ("&#163;" "&pound;" "£")  ("&#164;" "&curren;" "¤")
    ("&#165;" "&yen;" "¥")    ("&#166;" "&brvbar;" "¦") ("&#167;" "&sect;" "§")
    ("&#168;" "&uml;" "¨")    ("&#169;" "&copy;" "©")   ("&#170;" "&ordf;" "ª")
    ("&#171;" "&laquo;" "«")  ("&#172;" "&not;" "¬")    ("&#173;" "&shy;" "\xad") ;<- :CHANGED
    ("&#174;" "&reg;" "®")    ("&#175;" "&macr;" "¯")   ("&#176;" "&deg;" "°")
    ("&#177;" "&plusmn;" "±") ("&#178;" "&sup2;" "²")   ("&#179;" "&sup3;" "³")
    ("&#180;" "&acute;" "´")  ("&#181;" "&micro;" "µ")  ("&#182;" "&para;" "¶")
    ("&#183;" "&middot;" "·") ("&#184;" "&cedil;" "¸")  ("&#185;" "&sup1;" "¹")
    ("&#186;" "&ordm;" "º")   ("&#187;" "&raquo;" "»")  ("&#188;" "&frac14;" "¼")
    ("&#189;" "&frac12;" "½") ("&#190;" "&frac34;" "¾") ("&#191;" "&iquest;" "¿")
    ("&#192;" "&Agrave;" "À") ("&#193;" "&Aacute;" "Á") ("&#194;" "&Acirc;" "Â")
    ("&#195;" "&Atilde;" "Ã") ("&#196;" "&Auml;" "Ä")   ("&#197;" "&Aring;" "Å")
    ("&#198;" "&AElig;" "Æ")  ("&#199;" "&Ccedil;" "Ç") ("&#200;" "&Egrave;" "È")
    ("&#201;" "&Eacute;" "É") ("&#202;" "&Ecirc;" "Ê")  ("&#203;" "&Euml;" "Ë")
    ("&#204;" "&Igrave;" "Ì") ("&#205;" "&Iacute;" "Í") ("&#206;" "&Icirc;" "Î")
    ("&#207;" "&Iuml;" "Ï")   ("&#208;" "&ETH;" "Ð")    ("&#209;" "&Ntilde;" "Ñ")
    ("&#210;" "&Ograve;" "Ò") ("&#211;" "&Oacute;" "Ó") ("&#212;" "&Ocirc;" "Ô")
    ("&#213;" "&Otilde;" "Õ") ("&#214;" "&Ouml;" "Ö")   ("&#215;" "&times;" "×")
    ("&#216;" "&Oslash;" "Ø") ("&#217;" "&Ugrave;" "Ù") ("&#218;" "&Uacute;" "Ú")
    ("&#219;" "&Ucirc;" "Û")  ("&#220;" "&Uuml;" "Ü")   ("&#221;" "&Yacute;" "Ý")
    ("&#222;" "&THORN;" "Þ")  ("&#223;" "&szlig;" "ß")  ("&#224;" "&agrave;" "à")
    ("&#225;" "&aacute;" "á") ("&#226;" "&acirc;" "â")  ("&#227;" "&atilde;" "ã")
    ("&#228;" "&auml;" "ä")   ("&#229;" "&aring;" "å")  ("&#230;" "&aelig;" "æ")
    ("&#231;" "&ccedil;" "ç") ("&#232;" "&egrave;" "è") ("&#233;" "&eacute;" "é")
    ("&#234;" "&ecirc;" "ê")  ("&#235;" "&euml;" "ë")   ("&#236;" "&igrave;" "ì")
    ("&#237;" "&iacute;" "í") ("&#238;" "&icirc;" "î")  ("&#239;" "&iuml;" "ï")
    ("&#240;" "&eth;" "ð")    ("&#241;" "&ntilde;" "ñ") ("&#242;" "&ograve;" "ò")
    ("&#243;" "&oacute;" "ó") ("&#244;" "&ocirc;" "ô")  ("&#245;" "&otilde;" "õ")
    ("&#246;" "&ouml;" "ö")   ("&#247;" "&divide;" "÷") ("&#248;" "&oslash;" "ø")
    ("&#249;" "&ugrave;" "ù") ("&#250;" "&uacute;" "ú") ("&#251;" "&ucirc;" "û")
    ("&#252;" "&uuml;" "ü")   ("&#253;" "&yacute;" "ý") ("&#254;" "&thorn;" "þ")
    ("&#255;" "&yuml;" "ÿ")   ("&#60;" "&lt;" "<"))
   "
:CALLED-BY `*regexp-clean-url-utf-escape*', `*regexp-clean-html-escape*'
:SEE-ALSO `*mon-wrap-url-schemes*', `*regexp-clean-xml-parse*',
`*regexp-percent-encoding-reserved-chars*', `*regexp-clean-url-utf-escape*',
`*regexp-clean-ulan-diacritics*', `*regexp-cp1252-to-latin1*'.\n►►►")
) ;; :CLOSE unless

;;; ==============================
;;; :GOOGLE-DEFINE-ADDITIONS-EXTENSIONS-MODIFICATIONS

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-03T16:20:02-05:00Z}#{10053} - by MON>
(defvar *google-define-view-map* (cons (string-to-char "Q") 'View-kill-and-leave)
  "Keybinding for `view-mode' in `google-define' help buffers.
:SEE-ALSO `*google-define-get-command', `*google-define-html-entry-table*'\n.►►►")
;;
;;; :TEST-ME *google-define-view-map* 
;;;(progn (makunbound '*google-define-view-map*) (unintern '*google-define-view-map*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-03T13:43:28-05:00Z}#{10053} - by MON>
(defvar *google-define-get-buffer* nil 
  "*Name of buffer `google-define-get-command' should return results in.
:SEE-ALSO `*google-define-buffer-suffix*'.\n►►►")
;;
(unless (bound-and-true-p *google-define-get-buffer*)
  (setq *google-define-get-buffer* (symbol-name '*google-define-get-buffer*)))
;;
;;; :TEST-ME *google-define-get-buffer*
;;;(progn (makunbound '*google-define-get-buffer*) (unintern '*google-define-get-buffer*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T14:55:41-05:00Z}#{10055} - by MON>
(defvar *google-define-buffer-suffix* '("*" . ":gg-definition*")
  "*Prefix and suffix to attach to defition buffers created with `google-define'.\n
:CALLED-BY `google-define-kill-def-buffers' when cleaning up definition buffers.\n
:SEE-ALSO `*google-define-get-buffer*' `*google-define-html-entry-table*',
`*google-define-get-command'.\n►►►")
;;
;;; :TEST-ME *google-define-buffer-suffix*
;;;(progn (makunbound '*google-define-buffer-suffix*) (unintern '*google-define-buffer-suffix*) )


;;; ==============================
;;; `mon-help-KEY-tag' <- :FILE mon-doc-help-utils.el
;;; :CREATED <Timestamp: #{2010-02-04T20:22:51-05:00Z}#{10055} - by MON>
(defface gg-def-base
    (if (featurep 'mon-doc-help-utils)
        '((t :inherit mon-help-KEY-tag))
        ;; Else
        '(( ((class color) (min-colors 88)) 
           (:foreground "light steel blue" :weight extrabold)) )
        )
  "*Base face for font-locking buffers returned by `google-define'.\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition', `mon-doc-help-utils-faces'.\n►►►"
  :group 'google-define-redux-faces)
;;
;;; :TEST-ME (describe-face 'gg-def-base)
;;;(progn (makunbound 'gg-def-base) (unintern 'gg-def-base) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T16:06:13-05:00Z}#{10055} - by MON KEY>
(defface gg-def-delim 
    '((t :inherit gg-def-base :foreground "sky blue"))
  "*Provides fontlocking of space seperated delmiter charcaters.\n
Delimiters characters include: ►, |, ◄ preceded and followed by whitespace e.g.:
 \" ► \" \" | \" \" ◄ \"\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►"
  :group 'google-define-redux-faces)
;;
;;; :TEST-ME (describe-face 'gg-def-delim)
;;;(progn (makunbound 'gg-def-delim) (unintern 'gg-def-delim) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T16:06:16-05:00Z}#{10055} - by MON KEY>
(defface gg-def-num 
    '((t :inherit gg-def-base  :foreground "powder blue"))
  "*Provides fontlocking of enumerated numbers preceding a definition.\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►"
  :group 'google-define-redux-faces)
;;
;;; :TEST-ME (describe-face 'gg-def-num)
;;;(progn (makunbound 'gg-def-num) (unintern 'gg-def-num) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T16:06:20-05:00Z}#{10055} - by MON KEY>
(defface gg-def-inition
    ;; :WAS '( (((class color) (min-colors 88)) (:foreground "wheat1" :weight normal)) )
    '((t :inherit gg-def-base  :foreground "wheat1" :weight normal))
  "*Provides fontlocking of definition content e.g. anything after \"    | \".\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►"
  :group 'google-define-redux-faces)
;;
;;; :TEST-ME (describe-face 'gg-def-inition)
;;;(progn (makunbound 'gg-def-inition) (unintern 'gg-def-inition) )

;;; ==============================
;;; :TODO See notes below about matching alternate reasonable definition suffixes.
;;; :CREATED <Timestamp: #{2010-02-05T16:05:47-05:00Z}#{10055} - by MON KEY>
(defface gg-def-defined 
    '((t :inherit gg-def-base :foreground "cadet blue"))
  "*Provides fontlocking of word defined by definition.\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►"
  :group 'google-define-redux-faces)
;;
;;; :TEST-ME (describe-face 'gg-def-defined)
;;;(progn (makunbound 'gg-def-defined) (unintern 'gg-def-defined) )

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-04T21:28:01-05:00Z}#{10055} - by MON>
(defun google-define-get-command (host path)
  "Retrieve google defnitions to process-buffer `*google-define-get-buffer*'.\n
:SEE-ALSO `google-define', `google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers'.\n►►►"
  (let* ((timeout 180)
         (port 80) ;; http
         (post-cmd
          (concat "GET " path " HTTP/1.0\r\n"
                  "Host: " host "\r\n"
                  "User-Agent: Emacs\r\n"
                  ;; (concat "User-Agent: Mozilla/5.0 "
                  ;;         "(Windows; U; Windows NT 5.0; en-US; rv:1.8.1.1) "
                  ;;         "Gecko/20061204 Firefox/2.0.0.1\r\n")
                  (concat "Accept: text/xml,application/xml,application/"
                          "xhtml+xml,text/html;q=0.9,text/plain;q=0.8,"
                          "image/png,*/*;q=0.5\r\n")
                  "Accept-Language: en-us,en;q=0.5\r\n"
                  "Accept-Encoding: gzip,deflate\r\n"
                  "Accept-Charset: ISO-8859-1;q=0.7,*;q=0.7\r\n"
                  "\r\n"))
         proc buf)
    (progn
      (when (get-buffer *google-define-get-buffer*)
        (kill-buffer *google-define-get-buffer*))
      (setq proc (open-network-stream "url-get-command"
                                      *google-define-get-buffer*
                                      host
                                      port))
      (setq buf (process-buffer proc))
      (process-send-string proc post-cmd)
      (message "Getting information from %s: waiting for response..." host)
      (while (equal (process-status proc) 'open)
        (unless (accept-process-output proc timeout)
          (delete-process proc)
          (error "Server error: timed out while waiting!")))
      (message "Response received: processing..."))
    buf))

;;; :TEST-ME (google-define-get-command )

;;; ==============================
;;; REQUIRES `mon-g2be' mon-string-justify-left'
;;; :MODIFICATIONS <Timestamp: #{2010-02-03T13:41:22-05:00Z}#{10053} - by MON>
(defun google-define-parse-buffer (search-word def-disp-buffer)
  "Pull all of the definitions out of the data returned from google.\n
Print in a temp-buffer, parse, and convert for presentation.\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-font-lock', `google-define-kill-def-buffers',
`mon-string-justify-left' `*google-define-buffer-suffix*'
`*google-define-get-buffer*', `*google-define-html-entry-table*',
`*google-define-get-command'.\n►►►"
  (let* ((count 0)
         (accum-buffer ;; <- *standard-output* is here
          (prog1 
              def-disp-buffer
            (when (buffer-live-p (get-buffer def-disp-buffer))
              (with-current-buffer (get-buffer def-disp-buffer)
                (let ((buffer-read-only nil))
                  (erase-buffer))))))
         ;; :NOTE `accum-buffer' _should_ be empty, don't bother checking.
         (standard-output (get-buffer-create accum-buffer)))
    (princ (format "Definitions for: %s\n\n" search-word))
    
    ;; (with-current-buffer  ??
    (set-buffer 
     ;; :NOTE "http://www.google.com/search?num=100&hl=en&q=define%3A%22big%22&btnG=Search"
     ;; e.g. equivalent to entering at th gg search form: define:"big"
     (google-define-get-command                 
      "www.google.com" 
      (concat "/search?num=100&hl=en&q=define%3A%22"
              (replace-regexp-in-string " +" "+" search-word)
              "%22&btnG=Search")))
    (unwind-protect
         (progn
           (mon-g2be) ;; :WAS (goto-char (point-min))
           (while (search-forward-regexp "<li>\\([^<]+\\)" nil t)
             (incf count)
             (let ((definition 
                    (replace-regexp-in-string "\\(\n\\|\r\\|\t\\)" "" (match-string 1))))
               (princ ;; <- spitting to our *Definitions:<SEARCH-WORD>* buffer.
                (with-temp-buffer
                  (let ((>pnt (make-marker))
                        (<pnt (make-marker))
                        (fill-column 68)
                        (fill-prefix "    | ")
                        (gdrh ;; :WAS `google-define-replace-html'
                         #'(lambda ()
                             (dolist (x *google-define-html-entry-table*)
                               (let ((ascii (caddr x))
                                     (gd-rps ;; :WAS `google-define-replace-string'
                                      #'(lambda (frm-str to-str) 
                                                 (progn 
                                                   (mon-g2be) ;; mon-g2be <- mon-utils.el
                                                   (while (search-forward frm-str nil t)
                                                     (replace-match to-str nil t))))))
                                 (funcall gd-rps (car x) ascii)
                                 (funcall gd-rps (cadr x) ascii)))))
                        (gdru ;; :WAS `google-define-replace-unicode'
                         #'(lambda ()
                             (mon-g2be)
                             (while (search-forward-regexp "&#\\([0-9]+\\);" nil t)
                               (let* ((gdru-ucs (string-to-number (match-string 1)))
                                      (gdru-rep (char-to-string (or (decode-char 'ucs gdru-ucs) ?~))))
                                 (replace-match gdru-rep nil t))))))
                    (save-excursion              
                      (insert (format "%3d ►\n" count))               
                      (set-marker >pnt (point))
                      (insert ;; :NOTE `mon-string-justify-left' <- :FILE mon-utils.el
                       (replace-regexp-in-string  
                        "^    " "    | " (mon-string-justify-left definition 64 4)))
                      (set-marker <pnt (point))
                      (insert "\n    ◄\n\n"))
                    (fill-region-as-paragraph >pnt <pnt)
                    (funcall gdrh)  ;; :WAS (google-define-replace-html)
                    (funcall gdru)) ;; :WAS  (google-define-replace-unicode))
                  (buffer-substring (buffer-end 0) (buffer-end 1)))
                (get-buffer accum-buffer))))
           (set-buffer accum-buffer)) ; :CLOSE progn

      ;;;;;;;;;;;;;;
      ;; Close current TCP process of current `google-define-get-command' buffer.
      ;; (process-buffer (process-buffer "url-get-command"))
      ;; "url-get-command"
      ;; :WAS (delete-process nil))
      ;; (delete-process (process-buffer "url-get-command"))
      ;;;;;;;;;;;;;;
      
      ;; Make _sure_ we closed the process.
      (when (get-buffer *google-define-get-buffer*)
        (kill-buffer *google-define-get-buffer*))
      ) ;; :CLOSE unwind-protect

    (setq accum-buffer 
          (with-current-buffer accum-buffer 
            (google-define-font-lock search-word)
            (buffer-substring (buffer-end 0) (buffer-end 1))))
    
    ;; Put name of `accum-buffer' on the `count' var and kill that buffer too. 
    (setq count (current-buffer))
    (when (get-buffer count) (kill-buffer count))
    accum-buffer))

;;; ==============================
;;; :TODO The word definition fontlocking should also find the prefix of word 
;;; e.g. affixed -> "ed\\_w" "ing\\_w" "ing\\_w" ".tion\\_w" "edness\\_w"
;;; :CREATED <Timestamp: #{2010-02-04T21:33:14-05:00Z}#{10055} - by MON>
(defun google-define-font-lock (search-word)
  "Add fontification text-properties to the definitions.\n
Fontify with the following faces:
 `gg-def-base', `gg-def-num', `gg-def-delim',
 `gg-def-inition', `gg-def-defined'\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers'.\n►►►"
  (let* ((help-props
          `(("^Definitions for:" 0 (face gg-def-base)) 
            ("^ +\\([0-9]\\{1,2\\}\\) " 1 (face gg-def-num))
            (" \\(►\\||\\|◄\\) ?" 1 (face gg-def-delim))
            ("^    | \\(.*\\)$" 1 (face gg-def-inition))
            (,(concat " \\(" search-word "\\)[\\[:punct:]\\[:blank:]\n]") 1 (face gg-def-defined)))
             ))
      (mapc #'(lambda (x)
                ;; :WAS (goto-char (buffer-end 0))
                (mon-g2be) ;; <- mon-utils.el
                (while (search-forward-regexp  (elt x 0) nil t)
                  (add-text-properties 
                   (match-beginning (elt x 1)) (match-end (elt x 1)) 
                   (elt x 2))))
            help-props))) ;; (font-lock-fontify-buffer)))

;;; ==============================
;;; :REQUIRES `mon-help-temp-docstring-display' <- :FILE mon-doc-help-utils.el
;;; :CREATED <Timestamp: #{2010-02-03T13:41:17-05:00Z}#{10053} - by MON>
(defun google-define (search-word &optional intrp)
  "Ask Google for the definition of a word.\n
Return and display definition in new buffer other-window.\n
Buffer name constructed by prepending car of `*google-define-buffer-suffix*'
to SEARCH-WORD and appending cdr of `*google-define-buffer-suffix*' e.g.:
\n *damned:gg-definitions*\n
When called-interactively prompt for SEARCH-WORD with \"Define: \"
If there is a word at point us it as default value for prompt.\n
:EXAMPLE\n\n\(google-define \"define\"\)\n
\(google-define nil t\)define\n
:SEE-ALSO `google-define-font-lock', `google-define-parse-buffer',
`google-define-kill-def-buffers', `google-define-get-command',
`mon-help-temp-docstring-display',`*google-define-view-map*' `gg-def-base',
`gg-def-num', `gg-def-delim', `gg-def-inition', `gg-def-defined'.\n►►►"
  (interactive "i\np")
  (let* ((sw (cond (intrp (read-from-minibuffer "Define: "
                                                (thing-at-point 'word)))
                   (search-word search-word)))
         (header (concat "Definitions for " sw))
         (dfb (concat (car *google-define-buffer-suffix*)
                      sw
                      (cdr *google-define-buffer-suffix*)))
         (defs (google-define-parse-buffer sw dfb)))
    (prog1 (message header)
      (mon-help-temp-docstring-display defs dfb t))))
      
;;
;;; :TEST-ME (google-define "define")
;;; :TEST-ME (google-define nil t)define

;;; ==============================
;;; :REQUIRES `mon-buffer-exists-p'
;;; :REQUIRES `mon-buffer-exists-so-kill'
;;; :CREATED <Timestamp: #{2010-02-05T14:45:57-05:00Z}#{10055} - by MON>
(defun google-define-kill-def-buffers (&optional intrp)
  "Kill all google define buffers with `*google-define-buffer-suffix*'.\n
Return list of buffer names killed.\n
When called-interactively message with the buffers killed.\n
:EXAMPLE\n\n\(save-window-excursion
  \(dolist \(i '\(\"damned\" \"data\" \"delta\"\) 
           \(google-define-kill-def-buffers t\)\)
    \(google-define i\)\)\)\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers', `mon-buffer-exists-p',
`mon-buffer-exists-so-kill'.\n►►►"
  (let ((buffs (mapcar #'(lambda (b) (buffer-name (get-buffer b))) (buffer-list)))
        (def-buffs))
    (mapc #'(lambda (mb) 
              (when ;; (string-match ".*:gg-definition\\*" mb)
                  (string-match (concat ".*" (cdr *google-define-buffer-suffix*)) mb)
                (push mb def-buffs)))
          buffs)
    (setq buffs nil)  
    (mapc #'(lambda (kb) 
              (when (mon-buffer-exists-so-kill kb)
                (push kb buffs)))
          def-buffs)
    (if intrp 
        (message (mapconcat #'(lambda (ded) (format "Killed buffer %s" ded))
                            buffs "\n"))
        buffs)))
;;
;;; :TEST-ME (google-define-kill-def-buffers t) 
;;; :TEST-ME (save-window-excursion
;;;            (dolist (i '("damned" "data" "delta") 
;;;                     (google-define-kill-def-buffers t))
;;;              (google-define i)))

;;; ==============================
(provide 'google-define-redux)
;;; ==============================

;;; ================================================================
;;; google-define-redux.el ends here
;;; EOF

;;; ==============================
;;; :NOTE simplified version of `google-define-replace-html' with all functions inlined.
;;; :CREATED <Timestamp: #{2010-03-20T12:16:04-04:00Z}#{10116} - by MON KEY>
;;; (defun google-define-replace-html ()
;;;   (dolist (x *google-define-html-entry-table*)
;;;     (let ((ascii (caddr x))
;;;           (gd-rps #'(lambda (frm-str to-str) 
;;;                       (progn 
;;;                         (mon-g2be) ;; mon-g2be <- mon-utils.el
;;;                         (while (search-forward frm-str nil t)
;;;                           (replace-match to-str nil t))))))
;;;       (funcall gd-rps (car x) ascii)
;;;       (funcall gd-rps (cadr x) ascii))))
;;; ==============================

;;; ==============================
;;; `google-define-replace-html' as lambda form:
;;; (let ((gd-nam ;; :WAS `google-define-name-entry'
;;;        #'(lambda (nam-entry) (cadr nam-entry)))
;;;       (gd-num ;; :WAS `google-define-number-entry'
;;;        #'(lambda (num-entry) (car num-entry)))
;;;       (gd-asc ;; :WAS google-define-ascii-entry
;;;        #'(lambda (asc-entry) (caddr asc-entry))))
;;;
;;;            (google-define-ascii-entry x)))
;;;            (funcall asc-entry x)))
;;;
;;;       (google-define-replace-string (google-define-number-entry x) ascii)
;;;       (google-define-replace-string (funcall gd-num x) ascii)
;;;       (google-define-replace-string (car x) ascii)
;;;
;;;       (google-define-replace-string (google-define-name-entry x) ascii)
;;;       (google-define-replace-string (funcall gd-nam x) ascii)
;;;       (google-define-replace-string (cadr x) ascii)
;;; ==============================

;;; ==============================
;;; :COURTESY Jeremy English :HIS google-define.el 
;;; (defun google-define-number-entry (entry)
;;;   (car entry))
;;; 
;;; (defun google-define-name-entry (entry)
;;;   (cadr entry))
;;; 
;;; (defun google-define-ascii-entry (entry)
;;;   (cadr (cdr entry)))
;;; 
;;; (defun google-define-replace-string (from-string to-string)
;;;   (goto-char (point-min))
;;;   (while (search-forward from-string nil t)
;;;     (replace-match to-string nil t)))
;;; 
;;; (defun google-define-replace-html ()
;;;   (dolist (x *google-define-html-entry-table*)
;;;     (let ((ascii (google-define-ascii-entry x)))
;;;       (google-define-replace-string
;;;        (google-define-number-entry x) ascii)
;;;       (google-define-replace-string
;;;        (google-define-name-entry x) ascii))))
;;;
;;; (defun google-define-replace-unicode ()
;;;   (goto-char (point-min))
;;;   (while (search-forward-regexp "&#\\([0-9]+\\);" nil t)
;;;     (let* ((ucs (string-to-number (match-string 1)))
;;;            (rep (char-to-string (or (decode-char 'ucs ucs) ?~))))
;;;     (replace-match rep nil t))))
;;;
;;; (defun google-define-word-at-point ()
;;;  (let ((word-at-point (thing-at-point 'word)))
;;;    (set-text-properties 0 (length word-at-point) nil word-at-point)
;;;    word-at-point))
;;; ==============================

