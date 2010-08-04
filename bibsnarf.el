;;; bibsnarf --- get BibTeX format bibliography entries from the net

;; Author: Hans Halvorson (hhalvors -at- princeton -dot- edu) 
;; Version: 0.2 
;; Time-stamp: <2007-04-25 04:50:23 hhalvors>

;;; Commentary: 
;;
;; This program searches simultaneously through several bibliography
;; databases, and produces a summary of the results.  You can then
;; view records (using "v"), append records to other buffers (usingz
;; "c"), or append records into local BibTeX files (using "f").
;;
;; As of January 26, 2007, the supported search backends are:
;;
;;   MathSciNet:   www.ams.org/mathscinet (requires a subscription)
;;   citebase:     www.citebase.org
;;   math arxive:  front.math.ucdavis.edu
;;   spires-hep:   www.slac.stanford.edu/spires/
;;
;; In theory, it is easy to add any backend for which a single URL 
;; returns an html page with BibTeX formatted entries.
;; If you know of any such additional backends, please email the maintainer.
;;
;; To start a search: M-x bibsnarf RET 
;; You will then be queried for Author and Title (you need only enter one of the two fields).
;; If everything goes well, in a few short seconds you will 
;; find yourself in a buffer that summarizes your results, one line
;; per result.  The following keybindings apply in the summary buffer:
;;
;;   v   - view the full BibTeX entry for the record at the point
;;   SPC - ditto
;;   RET - ditto
;;   f   - append the full BibTeX of the record at the point to some local file
;;         (*CAUTION*: if you have made modifications to the file and have not saved, then you will 
;;         lose your changes!  For that case, you should instead use "a"
;;   c   - append the full BibTeX of the record at the point to some buffer
;;
;;  At present, all search results are dumped into a single buffer.  But it would not be diffcult
;;  to arrange things differently.
;;
;; 
;; Technical discussion: Here is what actually happens after you ask bibsnarf to search:
;; 1. bibsnarf figures out which urls to get
;; 2. bibsnarf calls an external browser (e.g. links) to download the urls.  
;;    (Why not a native emacs browser, e.g. w3?  For one, w3 is no longer maintained.  
;;    Second, using an external browser allows us to do asynchronous downloads.  Maybe that is also 
;;    possible with w3; I don't know.)
;; 3. bibsnarf processes each downloaded html page in two stages: (a) It cleans out the html cruft, 
;;    hopefully (!) resulting in a valid BibTeX file. (b) It parses the BibTeX file by turning each 
;;    valid entry into a hash table whose keys are field names and whose values are field contents, and 
;;    then it makes a big hash table called *bib* whose keys are random, and whose values are the entry 
;;    hash tables. into a hash table called '*bib*'.  Question: would it be better just to store the entries 
;;    in a list?  Does hashing provide any advantage ;; in this case?  
;; 4. bibsnarf writes out a summary string for each entry in *bib* in the buffer *bibsnarf results*.
;;

;; requires an external web client: e.g., links, w3m, lynx

;; TO DO: use async from url.el

;; TO DO: add SciTation support, eprintweb.org support

;; Acknowledgments: Much of the code is inspired by Eric Marsden's
;; watson.el, and some is borrowed from Nevin Kapur's bibfind.el.

;;; Code:

(require 'mm-url)
(require 'bibtex)
(require 'cl)

(defvar bsn-summary-mode-map nil
   "Keymap for the bsn-summary buffer.")

(if bsn-summary-mode-map
    ()
  (setq bsn-summary-mode-map (make-keymap))
  (suppress-keymap bsn-summary-mode-map)
  ;; basic commands
  (define-key bsn-summary-mode-map "v"    'bsn-summary-view)
  (define-key bsn-summary-mode-map " "    'bsn-summary-view)
  (define-key bsn-summary-mode-map [return] 'bsn-summary-view)
  (define-key bsn-summary-mode-map "f"    'bsn-summary-file)
  (define-key bsn-summary-mode-map "c"    'bsn-summary-copy)
)

(defgroup bibsnarf nil
  "BibSnarf: snarf a bibliography from the internet."
  :group 'tools)

(defconst bibsnarf-version "0.1"
  "bibsnarf version.")

(defvar bsn-frame-parameters '((menu-bar-lines . 0))
  "*Parameters to use when putting *bibsnarf results* buffer in a new frame.")

(defvar bsn-frame)

(defcustom bsn-backends '(msn arxive citebase spires)
  "A list of backends to search.  Currently supported: msn arxive citebase spires."
  :group 'bibsnarf)


(defcustom bsn-notify-method 'bully
  "*How to select the *bibsnarf results* buffer once it is ready.
Possible values are:
   * 'newframe: create a dedicated new frame (see `bsn-frame-parameters')
   * 'pushy: make *bibsnarf results* the current buffer in the current window
   * 'bully: make *bibsnarf results* the current buffer and only window
   * 'aggressive: make *bibsnarf results* the current buffer in the other window
   * 'friendly: display *bibsnarf results* in the other window but don't make current
   * 'polite: don't display *bibsnarf results*, but print message and beep when ready
   * 'quiet: like `polite', but don't beep
   * 'meek: make no indication that *bibsnarf results* is ready
You can also use a lisp expression that evaluates to your own notification method -- e.g. you can use \"man-notify-method\"."
  :group 'bibsnarf)


(defun bsn-executable-exists-p (exe &rest args)
  "Test whether the executable EXE exists.
Try to execute it with CALL-PROCESS with arguments ARGS,
returning its return status or nil if not found."
  (condition-case nil
      (apply #'call-process exe nil nil nil args)
    (file-error nil)))

(defvar bsn-get-command
  ;; please send me any others which might be useful
  (cond ((bsn-executable-exists-p "links" "-version") "links -source")
	((bsn-executable-exists-p "wget" "--version") "wget --output-document=-")
	((bsn-executable-exists-p "lynx" "-help") "lynx -source")
	((bsn-executable-exists-p "w3m") "w3m -dump_source")
	(t ""))
	)

;; To Do: could we also invoke Firefox or Opera with -noraise flag?

(defvar bsn-timeout 15
  "*Number of seconds to wait before assuming a request is late.
Bibsnarf will activate its buffer (according to the value of the
variable `bsn-notify-method') when all queries have completed
or after this number of seconds. If you have a slow Internet
connection it may be useful to set this to a higher value
than the default, particularly if you are using bl in
synchronous mode.")

(defvar bsn-frame)
(defvar bsn-arguments "")
(defvar bsn-match-count "0")
(defvar bsn-timer nil)
(defvar bsn-tick 0)
(defvar bsn-jobs (make-hash-table))
(defvar *bib* (make-hash-table :test #'equal))

(defvar bsn-extract-hook nil
  "*Hook to run before a search-engine specific extraction function.")

;; placate the byte-compiler
(defvar bsn-buffer-url nil)

(defun bibsnarf ()
  (interactive)
  (let* ((author (read-string "Author: "))
	 (title (read-string "Title: ")))
    (bsn-init)
    (message "Looking for matches...")
    (dolist (backend bsn-backends)
      (bsn-fetch backend author title))))

(defun bsn-init ()
  (let ((buf (get-buffer "*bibsnarf results*")))
    (setq bsn-frame (selected-frame))
    (setq bsn-match-count "0")
    (clrhash *bib*)                    ; remove previous matches
    (clrhash bsn-jobs)               ; remove any uncompleted async jobs
    (setq bsn-tick 0)
    (bsn-timer-start)
    (when buf                          ; update the output buffer: ??
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer))))

;;; Main async fetch function
(defun bsn-fetch (backend author title)
    (let* ((components (split-string bsn-get-command)) ; splits on whitespace
	   (bsn-get-program (car components))          ; e.g. "links"
	   (bsn-get-arguments (cdr components))        ; e.g. "-source"
	   (backend-str (symbol-name backend))
	   (extractor (intern (concat "bsn-" backend-str "-extract")))
	   (urler (intern (concat "bsn-" (symbol-name backend) "-url")))
	   (url (funcall urler author title))
	   )
      (apply #'bsn-add-job
	     `(lambda ()
                  (run-hooks 'bsn-extract-hook)
		  (,extractor)
		  (bsn-display-matches))
	     bsn-get-program
	     (append bsn-get-arguments (list url))
	     )
      )
    )

(defun bsn-add-job (handler job &rest args)
  (let* ((buf (generate-new-buffer " *bsn-async*"))
         (proc (apply #'start-process "bsn-async" buf job args)))
    (setf (gethash proc bsn-jobs) handler)
    (set-process-sentinel proc #'bsn-sentinel)))

(defun bsn-sentinel (process event)
  (when (eq (process-status process) 'exit)
    (let ((buf (process-buffer process))
          (handler (gethash process bsn-jobs)))
      (when handler
        (save-excursion
          (unwind-protect
              (progn
              (set-buffer buf)
              (funcall handler))
          (remhash process bsn-jobs)
          (kill-buffer buf)))))))


;; (verbatim from watson.el) this is run regularly while a bsn search
;; is in progress. If all the requests have been treated, we switch to
;; the *bibsnarf results* buffer (using user-customizable method from
;; `bsn-notify-method'). If `bsn-timeout' seconds have passed without
;; all requests having finished, we display the buffer anyway. If no
;; matches have been found at timeout, display a "no matches" message.
(defun bsn-timer-function (&rest ignore)
  (incf bsn-tick)
  (cond ((zerop (hash-table-count bsn-jobs))
         (bsn-timer-stop)
         (bsn-notify-when-ready))
        ((> bsn-tick bsn-timeout)
         (bsn-timer-stop)
         (if (zerop (hash-table-count *bib*))
             (message "bib search: no matches found after %s seconds" bsn-timeout)
           (bsn-notify-when-ready)))))

(defun bsn-timer-start ()
  (setq bsn-timer (run-with-timer 1 1 #'bsn-timer-function)))

(defun bsn-timer-stop ()
  (cancel-timer bsn-timer)
  (setq bsn-timer nil))

;; adapted from `Man-notify-method' in man.el. 
(defun bsn-notify-when-ready ()
  "Notify the user when the *bibsnarf results* buffer is ready.
See the variable `bsn-notify-method' for the different notification behaviors."
  (cond
   ((eq bsn-notify-method 'newframe)
    ;; Since we run asynchronously, perhaps while Emacs is waiting
    ;; for input, we must not leave a different buffer current.  We
    ;; can't rely on the editor combibsnarfd loop to reselect the
    ;; selected window's buffer.
    (save-excursion
      (let ((frame (make-frame bsn-frame-parameters)))
        (set-window-buffer (frame-selected-window frame) "*bibsnarf results*")
        (set-window-dedicated-p (frame-selected-window frame) t))))
   ((eq bsn-notify-method 'pushy)
    (switch-to-buffer "*bibsnarf results*"))
   ((eq bsn-notify-method 'bully)
    (and window-system
         (frame-live-p bsn-frame)
         (select-frame bsn-frame))
    (pop-to-buffer "*bibsnarf results*")
    (delete-other-windows))
   ((eq bsn-notify-method 'aggressive)
    (and window-system
         (frame-live-p bsn-frame)
         (select-frame bsn-frame))
    (pop-to-buffer "*bibsnarf results*"))
   ((eq bsn-notify-method 'friendly)
    (and window-system
         (frame-live-p bsn-frame)
         (select-frame bsn-frame))
    (display-buffer "*bibsnarf results*" 'not-this-window))
   ((eq bsn-notify-method 'polite)
    (beep)
    (message "bibsnarf search results are not ready"))
   ((eq bsn-notify-method 'quiet)
    (message "bibsnarf search results are not ready"))
   ((or (eq bsn-notify-method 'meek)
        t)
    (message ""))))

;;;; SECTION: BACKENDS:: these are the databases we will search through

;;; MathSciNet
;; N.B.: msn wants "Surname, First"
;; should we query for author firstname and author surname separately??

(defun bsn-msn-url (author title)
  "What's the URL for MathSciNet."
  (let* ((pairs `(("bdlback" . "r=1")
		  ("dr" . "all")
		  ("l" . "20")
		  ("pg3" . "TI")
		  ("s3" . ,title)
		  ("pg4" . "ICN")
		  ("s4" . ,author)
		  ("fn" . "130")
		  ("fmt" . "bibtex")
		  ("bdlall" . "Retrieve+All"))))
	 (concat "http://www.ams.org/mathscinet/search/publications.html?" (mm-url-encode-www-form-urlencoded pairs))))


;;; Zentralblatt Mathematik
;;; As of Jan 2007, ZBM limits to only 3 results for non-subscribers

(defun bsn-zbm-url (author title) 
  "What's the URL for ZBM?"
  (let* ((pairs `(("ti" . ,title)
		  ("au" . ,author)
		  ("type" . "bibtex")
		  ("format" . "short")
		  ("maxdocs" . "20"))))
    (concat "http://www.emis.de/cgi-bin/zmen/ZMATH/en/quick.html?" (mm-url-encode-www-form-urlencoded pairs))))

;;; MRL support
;;; As of Jan 2007, MRL seems to be defunct

(defun bsn-mrl-url (author title)
  "What's the URL for mrl?"
  (let* ((pairs `(("s3" . ,author)
		  ("pg3" . "ICN")
		  ("s4" . ,title)
		  ("pg4" . "TI")
		  ("s5" . "")
		  ("pg5" . "JOUR")
		  ("format" . "bibtex")
		  ("Search" . "Search"))))
    (concat "http://www.ams.org/mrlookup?" (mm-url-encode-www-form-urlencoded pairs))))

(defun bsn-msn-wash ()
  "Wash the output returned."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "^@" nil t)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</pre>" nil t)
      (delete-region (point-max) (match-beginning 0)))
    ;; Handle the case when nothing matches
    (goto-char (point-min))
    (when (re-search-forward "No Records Selected" nil t)
      (replace-match "@Comment No matches")
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</title>" nil t)
      (delete-region (point-max) (match-beginning 0)))))

(defun bsn-msn-extract ()
  "Extract BibTeX entries from the html source delivered by MathSciNet."
  ;; TO DO: informative error messages
  (bsn-msn-wash)          ; the result of this should be a valid BibTeX file
  ; (bibtex-validate)
  (bsn-read *bib* t) ; read these entries into the hashtable *bib* using "randomly" generated keys
  (message "Successfully parsed MSN results buffer.") ; debugging
  )

;; Note: arxive puts an extra pair of brackets around titles
;; (presumably to maintain capitalization); we want to strip these.

(defun bsn-arxive-wash ()
  "Clean up the ArXive buffer."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (when (re-search-forward "<dt><pre>@" nil t)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</pre>" nil t)
      (delete-region (point-max) (match-beginning 0)))
    (goto-char (point-min))
    (while (re-search-forward
	    (concat "^<dt><pre>\\|^</pre></td>\\|<a href" "=.*?>\\|</a>")   ;; syntactic hack to allow posting on emacswiki
	    nil t)
      (replace-match ""))
    ;; Handle the case when no matches are found
    (goto-char (point-min))
    (when (re-search-forward "Query not found" nil t)
      (replace-match "@Comment No matches")
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</title>" nil t)
      (delete-region (point-max) (match-beginning 0)))))

(defun bsn-arxive-extract ()
  (bsn-arxive-wash)
  (bsn-read *bib* t) ; optional t to assign random external keys
  (message "Successfully parsed ARXIVE results buffer.")
  )

(defun bsn-citebase-extract ()
  (bsn-read *bib* t)
  (message "Successfully parsed CITEBASE results buffer.")
  )

(defun bsn-spires-extract ()
  (bsn-read *bib* t)
  (message "Successfully parsed SPIRES results buffer.")
  )

;;; math arXive support

(defun bsn-arxive-url (author title)
  (concat "http://front.math.ucdavis.edu/search?" 
	  "a=" (mm-url-form-encode-xwfu author)
	      "&"
	  "t=" (mm-url-form-encode-xwfu title)
	  "&"
	  "s=BibTeX"))

;;; CITEBASE support

(defun bsn-citebase-wash ()
  t)

(defun bsn-citebase-url (author title)
  (let* ((pairs `(("submitted" . "Search")
		  ("author" . ,author)
		  ("format" . "BibTeX")
		  ("yearfrom" . "")
		  ("maxrows" . "100")
		  ("order" . "DESC")
		  ("yearuntil" . "")
		  ("publication" . "")
		  ("title" . ,title)
		  ("type" . "metadata")
		  ("rank" . "accession") ; in what order should we list?
		  )))
    (concat "http://www.citebase.org/search?" (mm-url-encode-www-form-urlencoded pairs))))

;; no html washing necessary for citebase

;;; SPIRES-HEP support

;; Note that the SPIRES server is quite slow ....  maybe don't search
;; it if you're in a hurry

(defun bsn-spires-rawcmd (author title)
  (cond
   ((zerop (length author)) (concat "T+" title))
   ((zerop (length title)) (concat "A+" author))
   (t (concat "A+" author "+and+" "T+" title))))

(defun bsn-simple-encode (pairs)
  (cond
   ((not pairs) nil)
   ((= (length pairs) 1) (concat (caar pairs) "=" (cdar pairs)))
   (t (concat (bsn-simple-encode (list (car pairs))) "&" (bsn-simple-encode (cdr pairs))))))

(defun bsn-spires-url (author title)
  (let* ((rawcmd (bsn-spires-rawcmd author title))
	 (pairs `(("rawcmd" . ,rawcmd)
		  ("FORMAT" . "wwwbriefbibtex")
		  ("SEQUENCE" . ""))))
  (concat "http://www.slac.stanford.edu/spires/find/hep/www?" (bsn-simple-encode pairs))))

(defun bsn-spires-wash ()
  "Wash the output returned."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (re-search-forward "<!-- START RESULTS -->" nil t)
    (delete-region (point-min) (match-end 0))
    (goto-char (point-max))
    (re-search-backward "<!-- END RESULTS -->" nil t)
    (delete-region (point-max) (match-beginning 0))
    (goto-char (point-min))
    (while (search-forward-regexp "<[a-z]+>\\|</[a-z]+>")
      (replace-match ""))
    ;; TO DO: Handle the case when nothing matches
    )
  )

(defun bsn-wash (backend)
  (cond
   ((string= backend "msn") (lambda () (bsn-msn-wash)))
   ((string= backend "arxive") (lambda () (bsn-arxive-wash)))
   ((string= backend "spires") (lambda () (bsn-spires-wash)))
   (t (lambda () t))))

(defun bsn-gethash (key hash)
  ; a gethash function that ignores the case of the key, and that
  ; returns an empty string if there is nothing that matches (because
  ; we cannot use 'truncate-string-to-width' on 'nil'.
  (or (gethash key hash) (gethash (upcase key) hash) (gethash (downcase key) hash) ""))
  
(defun bsn-record-summary-string (hash)
    (let* ((author (truncate-string-to-width (bsn-gethash "author" hash) 20)) 
	   (title (truncate-string-to-width (bsn-gethash "title" hash) 40))
	   (title (remove ?\{ (remove ?\} title)))
	   (journal (truncate-string-to-width (bsn-gethash "journal" hash) 25))
	   (volume (bsn-gethash "volume" hash))
	   (year (bsn-gethash "year" hash))
	   (pages (bsn-gethash "pages" hash)))
      (format "  %-25s  %-45s %-28s : %-10s (%s)"  (concat "[ " author " ]") (concat "\"" title "\"") journal pages year)))

(defun bsn-display-match (hash)
  (let (first start local-start end)
    (goto-char (point-max))
    (setq first (point))
    (setq start (point))
    ; (if (gethash key *bib*)   ; debug
    (insert (bsn-record-summary-string hash))
    (setq end (point))
    (add-text-properties start end '(face bsn-summary-default-face))    ;;; FIX
    (insert "\n")))

(defvar helper nil)

(defun bsn-really-display-matches ()
  (let ((buf (get-buffer-create "*bibsnarf results*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (set (make-local-variable 'helper) (make-hash-table :test #'equal))     ; aux will keep track of where entries are inserted      
      (loop for k being the hash-keys in *bib* using (hash-value v) do
	    (puthash (line-number-at-pos) k helper)
	    (bsn-display-match v))
      (goto-char (point-min))
      (bsn-summary-mode)
      (set-buffer-modified-p nil))))

(defun bsn-display-matches ()
  ;; display them, avoiding locking up emacs if possible. For some
  ;; reason the XEmacs #'run-with-idle-timer doesn't work.
  (if (and (fboundp 'run-with-idle-timer)
           (not (featurep 'xemacs)))
      (run-with-idle-timer 1 nil #'bsn-really-display-matches)
    (bsn-really-display-matches)))


;;;; Reconstitute BibTeX format entries from hash ;;;;;;;;;;;;;;;;;;

(defun bsn-print-bibtex-string (key bibhash)
  (let ((record (gethash key bibhash)))
    (concat (format "@%s{ %s,\n" (gethash "=type=" record) (gethash "=key=" record))
    (substring 
     (apply #'concat
	    (loop for k being the hash-keys in record using (hash-value v)
		  collect (unless (or (equal k "=key=") (equal k "=type="))
			    (format "%15s = {%s},\n" k v))
		  )
	   ) 0 -2)
    "\n}"
    )
   )
  )

(defun bsn-print-bibtex (key bibhash)
  (insert (concat "\n" (bsn-print-bibtex-string key bibhash) "\n\n")))


;;;;;;; actions in the summary buffer

(defun bsn-display-record (key bibhash)
  (let ((origin (buffer-name)))
    (pop-to-buffer "*bibsnarfed record*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (bibtex-mode)
    (goto-char (point-min))
    (bsn-print-bibtex key bibhash)
    (shrink-window-if-larger-than-buffer)  ; FIXME: this needs to be recalibrated for each record
    (goto-char (point-min))
    (setq buffer-read-only t)
    (pop-to-buffer origin)
    ))

(defun bsn-display-rec (line-no bibhash)
  ;; give line number in summary buffer and relevant bibhash, display the full BibTeX record
  (if (gethash line-no helper)   ; check first that we have a valid line number
      (let ((key (gethash line-no helper)))
	(bsn-display-record key bibhash)
	(message ""))   ; hack to erase previous error messages
  (message "No record here to view.")))
  
(defun bsn-file-record (key bibhash)
  (interactive)
  (let* ((record (gethash key bibhash))
	 (file (read-file-name (format "Append record \"%s\" to file: " (gethash "=key=" record))))
	 (bibstring (concat "\n\n" (bsn-print-bibtex-string key bibhash))))
    (write-region bibstring nil file t)
    (message (format "Record \"%s\" appended to %s" key file))))

(defun bsn-file-rec (line-no bibhash)
  (interactive)
  (if (gethash line-no helper)
      (let ((key (gethash line-no helper)))
	(bsn-file-record key bibhash)
	)
    (message "No record here to file.")))

(defun bsn-summary-file ()
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (bsn-file-rec line-no *bib*)))

(defun bsn-summary-view ()
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (bsn-display-rec line-no *bib*)))


;; the following is similar to 'append-to-buffer' in simple.el, but
;; that takes a region as input

(defun bsn-copy-record (key bibhash)
  (interactive)
  (let* ((record (gethash key bibhash))
	 (buffer (read-buffer (format "Append record \"%s\" to buffer: " (gethash "=key=" record)) (other-buffer (current-buffer) t)))
	 (oldbuf (current-buffer))
	 (bibstring (concat "\n\n" (bsn-print-bibtex-string key bibhash))))
    (save-excursion
      (let* ((append-to (get-buffer-create buffer))
	     (windows (get-buffer-window-list append-to t t))
	     point)
	(set-buffer append-to)
	(setq point (point))
	(barf-if-buffer-read-only)
	(insert bibstring)
	(dolist (window windows)
	  (when (= (window-point window) point)
	    (set-window-point window (point))))))))

(defun bsn-copy-rec (line-no bibhash)
  (interactive)
  (if (gethash line-no helper)
      (let ((key (gethash line-no helper)))
	(bsn-copy-record key bibhash)
	)
    (message "No record here to copy.")))

(defun bsn-summary-copy ()
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (bsn-copy-rec line-no *bib*)))

(defvar helper (make-hash-table :test #'equal)) ; buffer local variable to keep association of bibrecords and line numbers

(defun bsn-summary-mode ()
; TO DO: modeline
    (setq major-mode 'bsn-summary-mode)
    (setq mode-name "bibsnarf summary")
    (use-local-map bsn-summary-mode-map)
    )

(defun bsn-print-summary (bib-hash buffer-name)
  (pop-to-buffer buffer-name)
  (setq buffer-read-only nil)
  (erase-buffer)
  (loop for k being the hash-keys in bib-hash using (hash-value v)
	do 
	(insert (format "  [ %s ] %s\n" (cl-gethash "author" v) (cl-gethash "title" v))))
    (setq buffer-read-only t))



;; TO DO: underline the record at which the point is located

(defgroup bsn-summary-faces nil
  "BibSnarf, Faces of summary buffer."
  :prefix "bsn-highlight-"
  :group 'bibsnarf)

(defface bsn-summary-default-face
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "GreenYellow"))
    (((class color)
      (background light))
     (:foreground "green4")))
  "Face used for displaying bibliography record summaries."
  :group 'bibsnarf)

;;; BibTeX parsing

;; compare the following with bibtex-parse-entry (in bibtex.el), which
;; converts the entry to an association list.  This is just like that,
;; except that we store the entry in a hash table.

(defvar bounds nil)

(defun bsn-parse-entry (&optional content)
  "Parse entry at point, return a hash table.
The hash table elements have the form (FIELD . TEXT), where FIELD
and TEXT are strings, and FIELD can also be the special strings
\"=type=\" and \"=key=\".  For the FIELD \"=key=\" TEXT may be
nil.  Remove \"OPT\" and \"ALT\" from FIELD.  Move point to the
end of the last field.  If optional arg CONTENT is non-nil
extract content of text fields."
  (let (alist (entry (make-hash-table :test #'equal :size 14)))
    (when (looking-at bibtex-entry-maybe-empty-head)
      (puthash "=type=" (bibtex-type-in-head) entry)
      (puthash "=key=" (bibtex-key-in-head) entry)
      (goto-char (match-end 0))
      (while (setq bounds (bibtex-parse-field))
	(puthash (bibtex-name-in-field bounds t)
		 (bibtex-text-in-field-bounds bounds content)
		 entry)
	(goto-char (bibtex-end-of-field bounds))))
    entry))


(defun bsn-read (output &optional random)
  "Read bibliography into an already existing hash table OUTPUT.
If optional argument is set to t, then we generate hash keys from
the current time (to avoid conflicts with previously added
entries)."
  (goto-char (point-min))
  (while (bibtex-skip-to-valid-entry)
    (let* ((internal-key (bibtex-key-in-head))
	   (external-key (if random (current-time)
		 internal-key)))
    (puthash external-key (bsn-parse-entry t) output)))
  output)

(provide 'bibsnarf)


;;;;; TO DO: journal shortname display replacement strings; e.g. "Communications in Mathematical Physics" ==> "CMP"

;; (defun bsn-summary-dump ()
;;   "Dump the entire bibliography into a file."
;;   (interactive)
;;   ; query for filename
;; )

; (defun bsn-copy-record (line-no bibhash)
;  (interactive ;; destination bibliography

; (defun bsn-delete-record (line-no bibhash)
;  (interactive)


;;; end of file bibsnarf.el
