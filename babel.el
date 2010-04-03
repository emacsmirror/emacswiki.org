;;; babel.el --- interface to web translation services such as Babelfish
;; $Id: babel.el,v 1.22 2008/11/09 19:49:35 juergen Exp $
;;;
;;;Note, the following program will produce error
;;;"Babelfish HTML has changed ; please look for a new version of babel.el" as of 4/2010
;;;
;;; Author: Eric Marsden <emarsden@laas.fr>
;;;         Juergen Hoetzel <juergen@hoetzel.info> 
;;; Keywords: translation, web
;;; Copyright: (C) 1999-2001 Eric Marsden
;;;                2005-2008 Juergen Hoetzel
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Please send suggestions and bug reports to <emarsden@laas.fr>. 
;; The latest version of this package should be available at
;;
;;     <URL:http://www.hoetzel.info/Hacking/emacs/babel.el>

;;; Commentary:

;;; Overview ==========================================================
;;
;; This module provides an Emacs interface to different translation
;; services available on the Internet. You give it a word or paragraph
;; to translate and select the source and destination languages, and
;; it connects to the translation server, retrieves the data, and
;; presents it in a special *babel* buffer. Currently the following
;; backends are available:
;;
;;  * the Babelfish service at babelfish.yahoo.com 
;;  * the Google service at translate.google.com
;;  * the Transparent Language motor at FreeTranslation.com

;;
;; Entry points: either 'M-x babel', which prompts for a phrase, a
;; language pair and a backend, or 'M-x babel-region', which prompts
;; for a language pair and backend, then translates the currently
;; selected region, and 'M-x babel-buffer' to translate the current
;; buffer.
;;

;; If you ask for a language combination which several backends could
;; translate, babel.el will allow you to choose which backend to
;; use. Since most servers have limits on the quantity of text
;; translated, babel.el will split long requests into translatable
;; chunks and submit them sequentially.
;;
;; Please note that the washing process (which takes the raw HTML
;; returned by a translation server and attempts to extract the useful
;; information) is fragile, and can easily be broken by a change in
;; the server's output format. In that case, check whether a new
;; version is available (and if not, warn me; I don't translate into
;; Welsh very often).
;;
;; Also note that by accessing an online translation service you are
;; bound by its Terms and Conditions; in particular
;; FreeTranslation.com is for "personal, non-commercial use only".
;;
;;
;; Installation ========================================================
;;
;; Place this file in a directory in your load-path (to see a list of
;; appropriate directories, type 'C-h v load-path RET'). Optionally
;; byte-compile the file (for example using the 'B' key when the
;; cursor is on the filename in a dired buffer). Then add the
;; following lines to your ~/.emacs.el initialization file:
;;
;;   (autoload 'babel "babel"
;;     "Use a web translation service to translate the message MSG." t)
;;   (autoload 'babel-region "babel"
;;     "Use a web translation service to translate the current region." t)
;;   (autoload 'babel-as-string "babel"
;;     "Use a web translation service to translate MSG, returning a string." t)
;;   (autoload 'babel-buffer "babel"
;;     "Use a web translation service to translate the current buffer." t)
;;
;; babel.el requires emacs >= 22 
;;
;;
;; Backend information =================================================
;;
;; A babel backend named <zob> must provide three functions:
;;
;;    (babel-<zob>-translation from to)
;;
;;    where FROM and TO are three-letter language abbreviations from
;;    the alist `babel-languages'. This should return non-nil if the
;;    backend is capable of translating between these two languages.
;;
;;    (babel-<zob>-fetch msg from to)
;;
;;    where FROM and TO are as above, and MSG is the text to
;;    translate. Connect to the appropriate server and fetch the raw
;;    HTML corresponding to the request.
;;
;;    (babel-<zob>-wash)
;;
;;    When called on a buffer containing the raw HTML provided by the
;;    server, remove all the uninteresting text and HTML markup.
;;
;; I would be glad to incorporate backends for new translation servers
;; which are accessible to the general public. List of translation
;; engines and multilingual dictionaries at
;; <URL:http://funsan.biomed.mcgill.ca/~funnell/language.html>.
;;
;;
;; <URL:http://www.xmethods.net/sd/BabelFishService.wsdl>
;;
;; babel.el was inspired by a posting to the ding list by Steinar Bang
;; <sb@metis.no>. Morten Eriksen <mortene@sim.no> provided several
;; patches to improve InterTrans washing. Thanks to Per Abrahamsen and
;; Thomas Lofgren for pointing out a bug in the keymap code. Matt
;; Hodges <pczmph@unix.ccc.nottingham.ac.uk> suggested ignoring case
;; on completion. Colin Marquardt suggested
;; `babel-preferred-to-language'. David Masterson suggested adding a
;; menu item.
;;
;; User quotes: Dieses ist die größte Sache seit geschnittenem Brot.
;;                 -- Stainless Steel Rat <ratinox@peorth.gweep.net> 

;;; History

;;    0.7  * error handling if no backend is available for translating
;;           the supplied languages
;;	   * rely on url-* functions (for charset decoding) on GNU emacs
;;         * increased chunk size for better performance
;;         * added support for all Google languages
;;         * `babel-region' with prefix argument inserts the translation 
;;            output at point.

;;    0.6  * get rid of w3-region (implementend basic html entity parsing)
;;         * get rid of w3-form-encode-xwfu (using mm-url-form-encode-xwfu)
;;         * no character classes in regex (for xemacs compatibility)   
;;         * default backend: Google 

;;    0.5: * Fixed Google and Babelfish backends

;;    0.4: * revised FreeTranslation backend

;;;   0.3: * removed non-working backends: systran, intertrans, leo, e-PROMPT
;;;        * added Google backend
;;;        * revised UTF-8 handling
;;;        * Added customizable variables: babel-preferred-to-language, babel-preferred-from-language
;;;        * revised history handling 
;;;        * added helper function: babel-wash-regex

;;; Code:

(require 'cl)
(require 'mm-url)
(require 'easymenu)

;; xemacs compatibility
(eval-and-compile
  (when (featurep 'xemacs)
    (defun url-retrieve-synchronously (url)
      (save-excursion 
	(cdr (url-retrieve url))))))

;; ======================================================================
;;; Customizables
;; ======================================================================
(defgroup babel nil
  "provides an Emacs interface to different translation services available on the Internet"
  :group 'applications)


(defconst babel-version 0.7
  "The version number of babel.el")

(defconst babel-languages
  '(("Arabic" . "ar")
    ("Bulgarian" . "bg")
    ("Catalan" . "ca")
    ("Czech" . "cs")
    ("Danish" . "da")
    ("German" . "de")
    ("Greek" . "el")
    ("English" . "en")
    ("Spanish" . "es")
    ("Finnish" . "fi")
    ("French" . "fr")
    ("Hindi" . "hi")
    ("Croatian" . "hr")
    ("Indonesian" . "id")
    ("Italian" . "it")
    ("Hebrew" . "iw")
    ("ja" . "Japanese")
    ("Korean" . "ko")
    ("lt" . "Lithuanian")
    ("Latvian" . "lv")
    ("nl" . "Dutch")
    ("Norwegian" . "no")
    ("pl" . "Polish")
    ("Portuguese" . "pt")
    ("ro" . "Romanian")
    ("Russian" . "ru")
    ("sk" . "Slovak")
    ("Slovenian" . "sl")
    ("sr" . "Serbian")
    ("Swedish" . "sv")
    ("tl" . "Filipino")
    ("Ukrainian" . "uk")
    ("Vietnamese" . "vi")
    ("Chinese (Simplified)" . "zh-CN")
    ("Chinese" . "zh-CN")
    ("Chinese (Traditional)" . "zh-TW")))

(defcustom babel-preferred-to-language "German"
  "*Default target translation language.
This must be the long name of one of the languages in the alist"
  :type `(choice ,@(mapcar (lambda (s) `(const ,(car s))) babel-languages))
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq babel-to-history (list value))) 
  :group 'babel)

(defcustom babel-preferred-from-language "English"
  "*Default target translation language.
This must be the long name of one of the languages in the alist"
  :type `(choice ,@(mapcar (lambda (s) `(const ,(car s))) babel-languages))
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq babel-from-history (list value)))
  :group 'babel)

(defvar babel-to-history (list babel-preferred-to-language))
(defvar babel-from-history (list babel-preferred-to-language))
(defvar babel-backend-history (list))

(defvar babel-mode-hook nil)

(defvar babel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")     #'bury-buffer)
    (define-key map (kbd "SPC")   #'scroll-up)
    (define-key map (kbd "DEL")   #'scroll-down)
    (define-key map (kbd "<")   #'beginning-of-buffer)
    (define-key map (kbd ">")   #'end-of-buffer)
    (define-key map (kbd "s")   #'isearch-forward)
    (define-key map (kbd "r")   #'isearch-backward)
    (define-key map (kbd "h")   #'describe-mode)
    map)
  "Keymap used in Babel mode.")

(defvar babel-backends
  '(("Google" . google)
    ("Babelfish at Yahoo" . fish)
    ("FreeTranslation" . free))
  "List of backends for babel translations.")

(defun babel-sentence-end()		
  "portability function. emacs 22.0.50 introduced sentence-end
function, not available on other emacsen"
  (if (fboundp 'sentence-end)
      (sentence-end)
    sentence-end))	

;; xemacs compatibility
(eval-and-compile
  (if (featurep 'xemacs)
      ;; XEmacs
      (defun babel-url-retrieve (url)
	"Retrieve URL and decode"
	(let ((current (current-buffer))
	      (tmp (url-retrieve-synchronously url)))
	  (with-current-buffer tmp
	    ;;shrug: we asume utf8 
	    ;; (mm-decode-coding-region (point-min) (point-max) 'utf-8)
	    (copy-to-buffer current (point-min) (point-max)))))
    ;; GNUs Emacs
    (require 'url-handlers)
    (defun babel-url-retrieve (url)
      (let ((tmp (url-retrieve-synchronously url)))
	(url-insert tmp)))))

(defun babel-wash-regex (regex)
  "Extract the useful information from the HTML returned by fetch function
translated text should be inside parenthesized expression in regex" 
  (goto-char (point-min))
  (if (search-forward-regexp regex (point-max) t)
      (progn
	(delete-region (match-end 1) (point-max))
	(delete-region (point-min) (match-beginning 1))
	t)))

;;;###autoload
(defun babel (msg &optional no-display)
  "Use a web translation service to translate the message MSG.
Display the result in a buffer *babel* unless the optional argument
NO-DISPLAY is nil."
  (interactive "sTranslate phrase: ")
  (let* ((completion-ignore-case t)
         (from-suggest (or (first babel-from-history) (caar babel-languages)))
         (from-long
          (completing-read "Translate from: "
                           babel-languages nil t
                           (cons from-suggest 0)
                           'babel-from-history))
         (to-avail (remove* from-long babel-languages
                            :test #'(lambda (a b) (string= a (car b)))))
         (to-suggest (or (first 
			  (remove* from-long babel-to-history 
				   :test #'string=))
			 (caar to-avail)))
         (to-long
          (completing-read "Translate to: " to-avail nil t
                           (cons to-suggest 0)
                           'babel-to-history))
         (from (cdr (assoc from-long babel-languages)))
         (to   (cdr (assoc to-long babel-languages)))
         (backends (babel-get-backends from to)))
    (if (not backends)
	(error "No Backend available for translating %s to %s" 
	       from-long to-long)
      (let* ((backend-str
	     (completing-read "Using translation service: "
			      backends nil t 
			      (cons (or (member (first babel-backend-history) 
						backends) (caar backends)) 0) 
			      'babel-backend-history))
	     (backend (symbol-name (cdr (assoc backend-str babel-backends))))
	     (fetcher (intern (concat "babel-" backend "-fetch")))
	     (washer  (intern (concat "babel-" backend "-wash")))
	     (chunks (babel-chunkify msg 7000))
	     (translated-chunks '())
	     (view-read-only nil))
	(loop for chunk in chunks 
	      do (push (babel-work chunk from to fetcher washer)
		       translated-chunks))
	(if no-display
	    (apply #'concat (nreverse translated-chunks))
	  (with-output-to-temp-buffer "*babel*"
	    (message "Translating...")
	    (loop for tc in (nreverse translated-chunks)
		       do (princ tc))
	    (save-excursion
	      (set-buffer "*babel*")
		   (babel-mode))
	    (message "Translating...done")))))))

;;;###autoload
(defun babel-region (start end &optional arg)
  "Use a web translation service to translate the current region.
With prefix argument, insert the translation output at point."
  (interactive "r\nP")
  (if arg
      (insert (babel (buffer-substring-no-properties start end) t))
    (babel (buffer-substring-no-properties start end))))

;;;###autoload
(defun babel-as-string (msg)
  "Use a web translation service to translate MSG, returning a string."
  (interactive "sTranslate phrase: ")
  (babel msg t))

;; suggested by Djalil Chafai <djalil@free.fr>
;;
;;;###autoload
(defun babel-buffer ()
  "Use a web translation service to translate the current buffer.
Default is to present the translated text in a *babel* buffer.
With a prefix argument, replace the current buffer contents by the
translated text."
  (interactive)
  (let (pos)
    (cond (prefix-arg
           (setq pos (point-max))
           (goto-char pos)
           (insert
            (babel-as-string
             (buffer-substring-no-properties (point-min) (point-max))))
           (delete-region (point-min) pos))
          (t
           (babel-region (point-min) (point-max))))))

(defun babel-work (msg from to fetcher washer)
  (save-excursion
      (set-buffer (get-buffer-create " *babelurl*"))
      (erase-buffer)
      (funcall fetcher (babel-preprocess msg) from to)
      (funcall washer)
      (babel-postprocess)
      (babel-simple-html-parse)
      (babel-display)
      (buffer-substring-no-properties (point-min) (point-max))))

(defun babel-get-backends (from to)
  "Return a list of those backends which are capable of translating
language FROM into language TO."
  (loop for b in babel-backends
        for name = (symbol-name (cdr b))
        for translator = (intern (concat "babel-" name "-translation"))
        for translatable = (funcall translator from to)
        if translatable collect b))


(defconst babel-html-entity-regex 
  "&\\(#\\([0-9]+\\)\\|\\([a-zA-Z]+\\)\\);")

(defun babel-decode-html-entitiy (str)
  (if (and str (string-match babel-html-entity-regex
			     str))
      (if (string= (substring str 1 2) "#")
	  ;TODO: xemacs 
	  (if (not (featurep 'xemacs))
	      (let ((number (match-string-no-properties 2 str)))
		(decode-char 'ucs (string-to-number number)))
	    str)
	(let ((letter (match-string-no-properties 3 str)))
	  (cond ((string= "gt" letter) ">")
		((string= "lt" letter) "<")
		(t "?"))))))

(defun babel-display ()
  "Parse and display the region of this for basic HTML entities."
  (save-excursion
    (goto-char (point-min))
    (while (and (< (point) (point-max)) (search-forward-regexp 
					 babel-html-entity-regex 
					 (point-max) t))
      (let* ((start (match-beginning 0))
	     (end (match-end 0))
	     (entity (buffer-substring start end))
	     (replacement (babel-decode-html-entitiy entity)))
	(delete-region start end)
	(insert replacement)))))

(defun babel-mode ()
  (interactive)
  (use-local-map babel-mode-map)
  (setq major-mode 'babel-mode
        mode-name "Babel")
  (run-hooks 'babel-mode-hook))


(cond ((fboundp 'string-make-unibyte)
       (fset 'babel-make-unibyte #'string-make-unibyte))
      ((fboundp 'string-as-unibyte)
       (fset 'babel-make-unibyte #'string-as-unibyte))
      (t
       (fset 'babel-make-unibyte #'identity)))

;; from nnweb.el, with added `string-make-unibyte'.
(defun babel-form-encode (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (lambda (data)
     (concat (mm-url-form-encode-xwfu (babel-make-unibyte (car data))) "="
             (mm-url-form-encode-xwfu (babel-make-unibyte (cdr data)))))
   pairs "&"))

;; We mark paragraph endings with a special token, so that we can
;; recover a little information on the original message's format after
;; translation and washing and rendering. Should really be using
;; `paragraph-start' and `paragraph-separate' here, but we no longer
;; have any information on the major-mode of the buffer that STR was
;; ripped from.
;;
;; This kludge depends on the fact that all the translation motors
;; seem to leave words they don't know how to translate alone, passing
;; them through untouched.
(defun babel-preprocess (str)
  (while (string-match "\n\n\\|^\\s-+$" str)
    (setq str (replace-match " FLOBSiCLE " nil t str)))
  str)

;; decode paragraph endings in current buffer
(defun babel-postprocess ()
  (goto-char (point-min))
  (while (search-forward "FLOBSiCLE" nil t)
    (replace-match "\n<p>" nil t)))

(defun babel-simple-html-parse () 
  "Replace basic html markup"
  (goto-char (point-min))
  (while (re-search-forward  "<\\(br\\|p\\)/?>" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (while (re-search-forward  "^[ \t]+"  nil t)
    (replace-match "")))

;; split STR into chunks of around LENGTH characters, trying to
;; maintain sentence structure (this is used to send big requests in
;; several batches, because otherwise the motors cut off the
;; translation).
(defun babel-chunkify (str chunksize)
  (let ((start 0)
        (pos 0)
        (chunks '()))
    (while (setq pos (string-match (babel-sentence-end) str pos))
      (incf pos)
      (when (> (- pos start) chunksize)
        (push (substring str start pos) chunks)
        (setq start pos)))
    (when (/= start (length str))
      (push (substring str start) chunks))
    (nreverse chunks)))

;;;###autoload
(defun babel-version (&optional here)
  "Show the version number of babel in the minibuffer.
If optional argument HERE is non-nil, insert version number at point."
  (interactive "P")
  (let ((version-string 
         (format "Babel version %s" babel-version)))
    (if here 
        (insert version-string)
      (if (interactive-p)
          (message "%s" version-string)
        version-string))))


;; Babelfish-specific functions ================================================
;;
;; Babelfish (which uses the SysTran engine) is only able to translate
;; between a limited number of languages.

;; translation from generic  names to Babelfish 2-letter names
(defconst babel-fish-languages
  '(("en" . "en")
    ("de" . "de")
    ("it" . "it")
    ("pt" . "pt")
    ("es" . "es")
    ("fr" . "fr")))

;; those inter-language translations that Babelfish is capable of
(defconst babel-fish-translations
  '("en_fr" "en_de" "en_it" "en_pt" "en_es" "fr_en" "de_en" "it_en"
    "es_en" "pt_en"))

;; if Babelfish is able to translate from language FROM to language
;; TO, then return the corresponding string, otherwise return nil
(defun babel-fish-translation (from to)
  (let* ((fromb (cdr (assoc from babel-fish-languages)))
         (tob   (cdr (assoc to babel-fish-languages)))
         (comb (and fromb tob (concat fromb "_" tob))))
    (find comb babel-fish-translations :test #'string=)))

(defun babel-fish-fetch (msg from to)
  "Connect to the Babelfish server and request the translation."
  (let ((translation (babel-fish-translation from to)))
    (unless translation
      (error "Babelfish can't translate from %s to %s" from to))
    (let* ((pairs `(("trtext" . ,(mm-encode-coding-string msg 'utf-8))
		    ("lp" . ,translation)
		    ("ei" . "UTF-8")
		    ("doit" . "done")
		    ("fr" . "bf-res")
		    ("intl" . "1")
		    ("tt" . "urltext")
		    ("btnTrTxt" . "Translate")))
	   (url-request-data (babel-form-encode pairs))
             (url-request-method "POST")
             (url-request-extra-headers
              '(("Content-Type" . "application/x-www-form-urlencoded"))))
      (babel-url-retrieve "http://babelfish.yahoo.com/translate_txt" ))))

(defun babel-fish-wash ()
  "Extract the useful information from the HTML returned by Babelfish."
  (if (not (babel-wash-regex "<div id=\"result\"><div style=\"padding:[0-9.]*em;\">\\([^<]*\\)</div></div>"))
      (error "Babelfish HTML has changed ; please look for a new version of babel.el")))
		       


;; FreeTranslation.com stuff ===========================================

;; translation from  generic letter names to FreeTranslation names
(defconst babel-free-languages
  '(("en" . "English")
    ("de" . "German")
    ("it" . "Italian")
    ("nl" . "Dutch")
    ("pt" . "Portuguese")
    ("es" . "Spanish")
    ("no" . "Norwegian")
    ("ru" . "Russian")  
    ("zh-CN" . "SimplifiedChinese")
    ("zh-TW" . "TraditionalChinese")
    ("fr" . "French")))

;; those inter-language translations that FreeTranslation is capable of
(defconst babel-free-translations
  '("English/Spanish" "English/French" "English/German" "English/Italian" "English/Dutch" "English/Portuguese"
    "English/Russian" "English/Norwegian" "English/SimplifiedChinese" "English/TraditionalChinese" "Spanish/English" 
    "French/English" "German/English" "Italian/English" "Dutch/English" "Portuguese/English"))

(defun babel-free-translation (from to)
  (let* ((ffrom (cdr (assoc from babel-free-languages)))
         (fto   (cdr (assoc to babel-free-languages)))
         (trans (concat ffrom "/" fto)))
    (find trans babel-free-translations :test #'string=)))

(defun babel-free-fetch (msg from to)
  "Connect to the FreeTranslation server and request the translation."
  (let ((coding-system-for-read 'utf-8)
	(translation (babel-free-translation from to))
	(url "http://ets.freetranslation.com/"))
    (unless translation
      (error "FreeTranslation can't translate from %s to %s" from to))
    (let* ((pairs `(("sequence"  . "core")
                    ("mode"      . "html")
                    ("template"  . "results_en-us.htm")
                    ("srctext"   . ,msg)
		    ("charset"   . "UTF-8")
                    ("language"  . ,translation)))
           (url-request-data (babel-form-encode pairs))
	   (url-mime-accept-string "text/html")
           (url-request-method "POST")
	   (url-privacy-level '(email agent))
	   (url-mime-charset-string "utf-8")
           (url-request-extra-headers
            '(("Content-Type" . "application/x-www-form-urlencoded")
	      ("Referer" . "http://ets.freetranslation.com/"))))
      (babel-url-retrieve url))))

(defun babel-free-wash ()
  "Extract the useful information from the HTML returned by FreeTranslation."
  ;;; <textarea name="dsttext" cols="40" rows="6">hello together</textarea><br />
  (if (not (babel-wash-regex "<textarea name=\"dsttext\"[^>]+>\\([^<]*\\)</textarea>"))
      (error "FreeTranslations HTML has changed ; please look for a new version of babel.el")))


;; Google stuff ===========================================

;; Google supports all languages
(defconst babel-google-languages
  babel-languages)

(defun babel-google-translation (from to)
  ;; Google can always translate in both directions
  (find to babel-google-languages 
	:test '(lambda (st el) 
		 (string= (cdr el) st))))

(defun babel-google-fetch (msg from to)
  "Connect to google server and request the translation."
  ;; Google can always translate in both directions
  (if (not (find to babel-google-languages 
	    :test '(lambda (st el) 
		     (string= (cdr el) st))))
      (error "Google can't translate from %s to %s" from to)
    (let* ((pairs `(("text"       . ,(mm-encode-coding-string msg 'utf-8))
                    ("hl"         . "en")
                    ("Language"   . "English")
		    ("ie"         . "UTF-8")
		    ("oe"         . "UTF-8")
		    ("sl" . ,from)
		    ("tl" . ,to)))
           (url-request-data (babel-form-encode pairs))
	   (url-request-method "POST")
	   (url-request-extra-headers
	    '(("Content-Type" . "application/x-www-form-urlencoded"))))
      (babel-url-retrieve "http://translate.google.com/translate_t" ))))

(defun babel-google-wash ()
  "Extract the useful information from the HTML returned by google."
  (if (not (babel-wash-regex "<div id=result_box dir=\"[^\"]*\">\\(.*\\)</div></td></tr><tr><td[ \t]*class=submitcell>"))
      (error "Google HTML has changed ; please look for a new version of babel.el")))
  
;; TODO: ecs.freetranslation.com

;; (defun babel-debug ()
;;   (let ((buf (get-buffer-create "*babel-debug*")))
;;     (set-buffer buf)
;;     (babel-free-fetch "state mechanisms are too busy" "eng" "ger")))


(easy-menu-add-item nil '("tools") ["Babel Translation" babel t]) 

(provide 'babel)

;; babel.el ends here
