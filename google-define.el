;;; -*- indent-tabs-mode:nil -*-
;;;; Copyright (c) 2007,2008,2009 Jeremy English <jhe@jeremyenglish.org>
;;;;
;;;; Permission to use, copy, modify, distribute, and sell this
;;;; software and its documentation for any purpose is hereby granted
;;;; without fee, provided that the above copyright notice appear in
;;;; all copies and that both that copyright notice and this
;;;; permission notice appear in supporting documentation.  No
;;;; representations are made about the suitability of this software
;;;; for any purpose.  It is provided "as is" without express or
;;;; implied warranty.
;;;;
;;;; Created: 08-February-2007
;;;;
;;;;
;;;; Pulls definitions from google and displays them in a buffer.
;;;;
;;;; google-define is the command to call to get a new definition.
;;;;
;;;; 2007-07-05 You can now search for definitions of terms such as
;;;; pig latin
;;;;
;;;; 2007-11-09 Kevin Brubeck Unhammer changed
;;;; google-define-parse-buffer so that it would use a temporary
;;;; buffer. Now google-define is a more helpful citizen, 'q' will
;;;; close the buffer and it doesn't steal focus.
;;;;
;;;; Since the major mode was just used to change the font colors it
;;;; was dropped and a call to font-lock-fontify-buffer sets the
;;;; colors now.
;;;;
;;;; 2008-01-09 Unicode support was added.
;;;;
;;;; 2008-01-25 I Replaced google-define-plus-space with
;;;; replace-regexp-in-string. Also, trimmed down
;;;; google-define-parse-buffer a bit.
;;;;
;;;; 2009-01-07 Bjørn Arild Mæland made a change that includes the
;;;; searched word in the buffer name, so that recurring definitions
;;;; will not make previous searches disappear.
;;;;
;;;; 2009-05-24 Added support for Aquamacs
;;;;
;;;; 2009-08-12 Bartuer sent in a patch for multiple language output.

(require 'font-lock)

(defconst *google-define-html-entry-table*
  (list
   '("&#34;" "&quot;" "\"")   '("&#38;" "&amp;" "&")     '("&#39;" "&yow;" "'")
   '("&#62;" "&gt;" ">")      '("&#160;" "&nbsp;" " ")   '("&#161;" "&iexcl;" "¡")
   '("&#162;" "&cent;" "¢")   '("&#163;" "&pound;" "£")  '("&#164;" "&curren;" "¤")
   '("&#165;" "&yen;" "¥")    '("&#166;" "&brvbar;" "¦") '("&#167;" "&sect;" "§")
   '("&#168;" "&uml;" "¨")    '("&#169;" "&copy;" "©")   '("&#170;" "&ordf;" "ª")
   '("&#171;" "&laquo;" "«")  '("&#172;" "&not;" "¬")    '("&#173;" "&shy;" "­")
   '("&#174;" "&reg;" "®")    '("&#175;" "&macr;" "¯")   '("&#176;" "&deg;" "°")
   '("&#177;" "&plusmn;" "±") '("&#178;" "&sup2;" "²")   '("&#179;" "&sup3;" "³")
   '("&#180;" "&acute;" "´")  '("&#181;" "&micro;" "µ")  '("&#182;" "&para;" "¶")
   '("&#183;" "&middot;" "·") '("&#184;" "&cedil;" "¸")  '("&#185;" "&sup1;" "¹")
   '("&#186;" "&ordm;" "º")   '("&#187;" "&raquo;" "»")  '("&#188;" "&frac14;" "¼")
   '("&#189;" "&frac12;" "½") '("&#190;" "&frac34;" "¾") '("&#191;" "&iquest;" "¿")
   '("&#192;" "&Agrave;" "À") '("&#193;" "&Aacute;" "Á") '("&#194;" "&Acirc;" "Â")
   '("&#195;" "&Atilde;" "Ã") '("&#196;" "&Auml;" "Ä")   '("&#197;" "&Aring;" "Å")
   '("&#198;" "&AElig;" "Æ")  '("&#199;" "&Ccedil;" "Ç") '("&#200;" "&Egrave;" "È")
   '("&#201;" "&Eacute;" "É") '("&#202;" "&Ecirc;" "Ê")  '("&#203;" "&Euml;" "Ë")
   '("&#204;" "&Igrave;" "Ì") '("&#205;" "&Iacute;" "Í") '("&#206;" "&Icirc;" "Î")
   '("&#207;" "&Iuml;" "Ï")   '("&#208;" "&ETH;" "Ð")    '("&#209;" "&Ntilde;" "Ñ")
   '("&#210;" "&Ograve;" "Ò") '("&#211;" "&Oacute;" "Ó") '("&#212;" "&Ocirc;" "Ô")
   '("&#213;" "&Otilde;" "Õ") '("&#214;" "&Ouml;" "Ö")   '("&#215;" "&times;" "×")
   '("&#216;" "&Oslash;" "Ø") '("&#217;" "&Ugrave;" "Ù") '("&#218;" "&Uacute;" "Ú")
   '("&#219;" "&Ucirc;" "Û")  '("&#220;" "&Uuml;" "Ü")   '("&#221;" "&Yacute;" "Ý")
   '("&#222;" "&THORN;" "Þ")  '("&#223;" "&szlig;" "ß")  '("&#224;" "&agrave;" "à")
   '("&#225;" "&aacute;" "á") '("&#226;" "&acirc;" "â")  '("&#227;" "&atilde;" "ã")
   '("&#228;" "&auml;" "ä")   '("&#229;" "&aring;" "å")  '("&#230;" "&aelig;" "æ")
   '("&#231;" "&ccedil;" "ç") '("&#232;" "&egrave;" "è") '("&#233;" "&eacute;" "é")
   '("&#234;" "&ecirc;" "ê")  '("&#235;" "&euml;" "ë")   '("&#236;" "&igrave;" "ì")
   '("&#237;" "&iacute;" "í") '("&#238;" "&icirc;" "î")  '("&#239;" "&iuml;" "ï")
   '("&#240;" "&eth;" "ð")    '("&#241;" "&ntilde;" "ñ") '("&#242;" "&ograve;" "ò")
   '("&#243;" "&oacute;" "ó") '("&#244;" "&ocirc;" "ô")  '("&#245;" "&otilde;" "õ")
   '("&#246;" "&ouml;" "ö")   '("&#247;" "&divide;" "÷") '("&#248;" "&oslash;" "ø")
   '("&#249;" "&ugrave;" "ù") '("&#250;" "&uacute;" "ú") '("&#251;" "&ucirc;" "û")
   '("&#252;" "&uuml;" "ü")   '("&#253;" "&yacute;" "ý") '("&#254;" "&thorn;" "þ")
   '("&#255;" "&yuml;" "ÿ")   '("&#60;" "&lt;" "<")))

(defun google-define-number-entry (entry)
  (car entry))

(defun google-define-name-entry (entry)
  (cadr entry))

(defun google-define-ascii-entry (entry)
  (cadr (cdr entry)))

(defun google-define-replace-string (from-string to-string)
  (goto-char (point-min))
  (while (search-forward from-string nil t)
    (replace-match to-string nil t)))

(defun google-define-replace-html ()
  (dolist (x *google-define-html-entry-table*)
    (let ((ascii (google-define-ascii-entry x)))
      (google-define-replace-string
       (google-define-number-entry x) ascii)
      (google-define-replace-string
       (google-define-name-entry x) ascii))))

(defun google-define-replace-unicode ()
  (goto-char (point-min))
  (while (search-forward-regexp "&#\\([0-9]+\\);" nil t)
    (let* ((ucs (string-to-number (match-string 1)))
          (rep (char-to-string (or (decode-char 'ucs ucs) ?~))))
    (replace-match rep nil t))))

(defun google-define-get-command (host path)
  (let* ((timeout 180)
         (port 80) ;http
         (post-cmd
          (concat "GET " path " HTTP/1.0\r\n"
                  "Host: " host "\r\n"
                  "User-Agent: Emacs\r\n"
                  "Accept: text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\r\n"
                  "Accept-Language: en-us,en;q=0.5\r\n"
                  "Accept-Encoding: gzip,deflate\r\n"
                  "Accept-Charset: ISO-8859-1;q=0.7,*;q=0.7\r\n"
                  "\r\n"))
         proc buf)

    (unwind-protect
        (progn
          (setq proc (open-network-stream "url-get-command"
                                          "*url-get-buffer*"
                                          host
                                          port)
                buf (process-buffer proc))
          (process-send-string proc post-cmd)
          (message "Getting information from %s: waiting for response..." host)

          (while (equal (process-status proc) 'open)
            (unless (accept-process-output proc timeout)
              (delete-process proc)
              (error "Server error: timed out while waiting!")))

          (message "Response received: processing...")))

    ;; unwind-protect
    buf))

(defun google-define-parse-buffer (search-word data-buffer)
 "Pull all of the definitions out of the data returned from
google, and print in a temp-buffer"
 (let ((count 0)
       (header (concat "Definitions for " search-word))
       (temp-buffer-show-hook #'google-define-font-lock)
       (continue t))
   (with-output-to-temp-buffer 
       (concat "*Definitions: " search-word "*")
     (princ (concat header "\n\n"))
     (set-buffer data-buffer)
     (goto-char (point-min))
     (search-forward-regexp "<div id=prs>" nil t) ;;Move to the div containing the definitions
     (search-forward-regexp "<b>[^<]+" nil t) ;;Skip the "Web"
     (search-forward-regexp "<b>[^<]+" nil t) ;;Skip the bold definitions title
     (while (and (search-forward-regexp "\\(<[^>]+>\\)\\([^<]+\\)" nil t) continue)
       (let ((tag (match-string 1))
             (definition 
               (replace-regexp-in-string "\\(\n\\|\r\\|\t\\)" "" 
                                         (match-string 2))))
         (princ
          (with-temp-buffer
            (setf fill-prefix "     ")
            (save-excursion
              ;;(insert (format "%s\n" tag))
              (cond
               ((string-equal tag "<b>")
                (setq count 0)
                (insert (format "%s\n\n" definition)))
               ((string-equal tag "<li>")
                (setq count (+ 1 count))
                (insert (format "%3d. %s\n\n" count definition)))
               ((string-equal tag "<p>")
                (setq continue nil))))
            (fill-paragraph nil)
            (google-define-replace-html)
            (google-define-replace-unicode)
            (buffer-string))))))
   (message header)))

(defun google-define ()
  "Ask google for the definition of a word.

If we have a current region use it's value as the default."
  (interactive)
  (let* ((search-word
          (read-from-minibuffer "Define: "
                                (thing-at-point 'word)))
        (data-buffer
         (google-define-get-command "www.google.com"
                          (concat
                           "/search?num=100&hl=en&defl=all&q=define%3A%22"
                           (replace-regexp-in-string " +" "\+" search-word)
                 "%22&btnG=Search"))))
    (google-define-parse-buffer search-word data-buffer)
    (kill-buffer data-buffer)))

(defconst google-define-font-lock-keywords
  (list
   (list "^Definitions.for.+$" '(0 font-lock-function-name-face))
   (list "^[A-Z].+$" '(0 font-lock-variable-name-face))
   (list "^\\s-+.+$" '(0 font-lock-string-face))))

(defun google-define-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(google-define-font-lock-keywords t))
  (font-lock-fontify-buffer))

(provide 'google-define)

