;;; bibretrieve.el --- Retrieve BibTeX entries from the web.

;; Copyright (C) 2012  Antonio Sartori

;; Author: Antonio Sartori <anto_sart -at- yahoo.it>
;; Version: 0.1
;; Time-stamp: <2012-03-24 09:06:42 asartori>
;; URL: http://www.math.uni-bonn.de/people/sartori/materiale/bibretrieve.el

;; Keywords: tex, tools

;; This program is free software; you can redistribute it and/or modify
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

;; Description:

;; This program searches the web for BibTeX entries matching the given
;; Author end/or Title.  It displays then a selection buffer, that permits
;; to examine and choose entries to add to the bibliography file, or to
;; insert into the current buffer.

;; The function is called through "M-x bibretrieve".  Then it prompts for
;; author and title.  For an advanced use, that permits to select which
;; backend to use, call it with "C-u M-x bibretrieve".

;; The configuration is done with the variable bibretrieve-backends, that
;; is an alist with pairs containing the backend to use and the timeout
;; for it.

;; The url is retrieved via mm-url.  You may want to customize the
;; variable mm-url-use-external and mm-url-program.

;; Supported backends are at the moment
;; ArXiv:              "http://adsabs.harvard.edu"
;; MathSciNet (msn):   "http://www.ams.org/mathscinet"
;; MrLookup (mrl):     "http://www.ams.org/mrlookup"
;; Citebase:           "http://www.citebase.org"
;; Inspire:            "http://inspirehep.net"

;; To create a new backend, it is enough to define a new function
;; "bibretrieve-BACKEND-create-url" that takes as input author and title
;; and returns a url that, when retrieved, gives some bibtex entries.  It
;; is then necessary to advise bibretrieve of the new backend, adding it
;; to the list "bibretrieve-installed-backends".

;; Installation:

;; For an user-installation, it should be sufficient to:

;; 1) Put this source file "bibretrieve.el" in some folder, for
;;    example in "~/.emacs.d/", and add this folder to the Emacs path
;;    (if it is not already there):

;;    	(add-to-list 'load-path "~/.emacs.d/bibretrieve")

;; 2) Add an instruction to the file "~/.emacs" to load BibRetrieve;
;;    something like that does the trick:

;; 	(load "~/.emacs.d/bibretrieve")

;; 3) (If you want) do a minimal configuration (see below).

;; 4) Enjoy BibRetrieve calling "M-x bibretrieve", or "C-u M-x
;;    bibretrieve" from Emacs.

;; BibRetrieve can be configured with customize.
;; Anyway, it could be easier to add some line to the file "~/.emacs".

;; To configure the backends used, set the variable
;; "bibretrieve-backends".  This is an alist with askeys the names of the
;; backends and as values the timeouts.  The default configuration is:

;; 	 (setq bibretrieve-backends '(("msn" . 10) ("arxiv" . 5) ("citbase" . 5)))

;; Urls are get through mm-url.  As default, this works through the
;; library url.  If you want to use an external program, like wget or
;; curl, set the variable "mm-url-program" to the program and the variable
;; "mm-url-use-external" to t.

;; With Aquamacs, I got problems using mm-url with the default library
;; url.  I suggest to set the variables of mm-url as:
;;        (setq mm-url-use-external t)
;;     	  (setq mm-url-program "curl")

;; Acknowledgments: This program has been inspired by bibsnarf.  The
;; functions that create the urls for most backends are taken from
;; there.  This program uses the library mm-url.  This programs also uses
;; lot of function of RefTeX.  The selection process is entirely based on
;; reftex-sel.  Many function have also been adapted from there.

;;; Code:

(provide 'bibretrieve)

(eval-when-compile (require 'cl))
(require 'reftex)
(require 'reftex-cite)
(require 'reftex-sel)
(require 'mm-url)

(defconst bibretrieve-version "0.1"
  "Version string for BibRetrieve.")

(defgroup bibretrieve nil
  "BibRetrieve: Retrieve bibliography entries from the internet."
  :group 'tools)

(defvar bibretrieve-installed-backends '("msn" "mrl" "arxiv" "citebase" "inspire")
  "List of installed backends for BibRetrieve.")

(defcustom bibretrieve-backends '(("msn" . 10) ("arxiv" . 5) ("citebase" . 5))
  "Backends customization variable for BibRetrieve.

Backends to use for the search, together with a timeout
for the research on every backend.
Timeout should be an integer number of seconds."
  :type '(alist :key-type string :value-type integer)
  :options bibretrieve-installed-backends
  :group 'bibretrieve)

(defvar bibretrieve-select-map nil
  "Keymap used for *RefTeX Select* buffer, when selecting a BibTeX entry.
This keymap can be used to configure the BibTeX selection process which is
started with the command \\[bibretrieve-get].")

;; Prompt and help string for citation selection
(defconst bibretrieve-select-prompt
  "Select: [n]ext [p]revious a[g]ain [r]efine [f]ull_entry [q]uit RET [?]Help+more")

;; Adapted from RefTeX
(defconst bibretrieve-select-help
  " n / p      Go to next/previous entry (Cursor motion works as well).
 g / r      Start over with new search / Refine with additional regexp.
 SPC        Show full database entry in other window.
 f          Toggle follow mode: Other window will follow with full db entry.
 .          Show current append point.
 q          Quit.
 TAB        Enter citation key with completion.
 RET        Accept current entry (also on mouse-2), and append it to default BibTeX file.
 m / u      Mark/Unmark the entry.
 e / E      Append all (marked/unmarked) entries to default BibTeX file.
 a / A      Put all (marked) entries into current buffer.")

;; Copied from bibsnarf
(defun bibretrieve-msn-create-url (author title)
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

;; The url given is no more active
;; Copied from bibsnarf
(defun bibretrieve-zbm-create-url (author title)
  (let* ((pairs `(("ti" . ,title)
		  ("au" . ,author)
		  ("type" . "bibtex")
		  ("format" . "short")
		  ("maxdocs" . "20"))))
    (concat "http://www.emis.de/cgi-bin/zmen/ZMATH/en/quick.html?" (mm-url-encode-www-form-urlencoded pairs))))

;; Copied from bibsnarf
(defun bibretrieve-mrl-create-url (author title)
  (let* ((pairs `(("s3" . ,author)
		  ("pg3" . "ICN")
		  ("s4" . ,title)
		  ("pg4" . "TI")
		  ("s5" . "")
		  ("pg5" . "JOUR")
		  ("format" . "bibtex")
		  ("Search" . "Search"))))
    (concat "http://www.ams.org/mrlookup?" (mm-url-encode-www-form-urlencoded pairs))))

;; Copied from bibsnarf
(defun bibretrieve-arxiv-create-url (author title)
  (concat "http://adsabs.harvard.edu/cgi-bin/nph-abs_connect?db_key=PRE&"
	  "&aut_req=YES&aut_logic=AND&author=" (mm-url-form-encode-xwfu author)
	  "&ttl_req=YES&ttl_logic=AND&title=" (mm-url-form-encode-xwfu title)
	  "&data_type=BIBTEX"))

;; Modified from bibsnarf
(defun bibretrieve-citebase-create-url (author title)
  (let* ((query (concat author " " title))
	 (pairs `(("submitted" . "Search")
		  ("query" . ,query)
		  ("format" . "BibTeX")
		  ("maxrows" . "100")
		  ("order" . "DESC")
		  ("rank" . "1000") ; in what order should we list?
		  )))
    (concat "http://www.citebase.org/search?" (mm-url-encode-www-form-urlencoded pairs))))

;; Copied from bibsnarf
;; No more used
(defun bibretrieve-spires-rawcmd (author title)
  (cond
   ((zerop (length author)) (concat "T+" title))
   ((zerop (length title)) (concat "A+" author))
   (t (concat "A+" author "+and+" "T+" title))))

;; Copied from bibsnarf
;; No more used
(defun bibretrieve-spires-create-url (author title)
  (let* ((rawcmd (bibretrieve-spires-rawcmd author title))
	 (pairs `(("rawcmd" . ,rawcmd)
		  ("FORMAT" . "wwwbriefbibtex")
		  ("SEQUENCE" . ""))))
  (concat "http://www.slac.stanford.edu/spires/find/hep/www?" (mm-url-encode-www-form-urlencoded pairs))))

(defun bibretrieve-inspire-create-url (author title)
  (let* ((pairs `(("ln" . "en")
		  ("as" . "1")
		  ("m1" . "a")
		  ("op1" . "a")
		  ("m2" . "a")
		  ("op2" . "a")
		  ("m3" . "a")
		  ("action_search" . "Search")
		  ("sf" . "year")
		  ("so" . "d")
		  ("sc" . "0")
		  ("p1" . ,author)
		  ("f1" . "author")
		  ("p2" . ,title)
		  ("f2" . "title")
		  ("of" . "hx")
		  ("rg" . "100"))))
  (concat "http://inspirehep.net/search?" (mm-url-encode-www-form-urlencoded pairs))))

(defun bibretrieve-http (url)
  "Retrieve URL and return the buffer, using mm-url."
  (let ((buffer (generate-new-buffer (generate-new-buffer-name "bibretrieve-results-"))))
    (with-current-buffer buffer
      (message "Retrieving %s" url)
      (mm-url-insert-file-contents url))
    buffer))
;;   (if bibretrieve-executable
;;     (let ((buffer (generate-new-buffer (generate-new-buffer-name "bibretrieve-results-"))))
;;       (message "Retrieving with %s %s" bibretrieve-executable url)
;;       (call-process bibretrieve-executable nil buffer nil url)
;;       buffer)
;;     (message "Retrieving %s" url)
;;     (url-retrieve-synchronously url)))

(defun bibretrieve-retrieve (author title backends &optional newtimeout)
  "Search AUTHOR and TITLE on BACKENDS.
If NEWTIMEOUT is given, this replaces the timeout for all backends.
Returns buffer with results."
  (let (failed not-found var buffers)
    (dolist (backend backends)
      (let* ((timeout (or (or newtimeout (cdr (assoc backend bibretrieve-backends))) "0"))
	    (function-backend (intern (concat "bibretrieve-" backend "-create-url")))
	    buffer)
	(if (functionp function-backend)
	    (progn (setq buffer (with-timeout (timeout) (bibretrieve-http (funcall function-backend author title))))
		   (if (bufferp buffer)
		       (add-to-list 'buffers buffer)
		     (add-to-list 'failed backend)))
	  (add-to-list 'not-found backend))))
    (when failed
	(message (concat "Following backends failed: " (mapconcat 'identity failed " "))))
    (when not-found
      (message (concat "Following backends don't exist: " (mapconcat 'identity not-found " "))))
    buffers))

(defun bibretrieve-prompt-and-retrieve (&optional arg)
  "Prompts for author and title and retrieves.
ARG is the optional arg."
  (let* ((author (read-string "Author: "))
	 (title (read-string "Title: "))
	 backend backends timeout)
    (when arg
      (if (integerp arg)
	  (setq timeout arg)
	(progn (setq backend (completing-read "Backend to use: [defaults] " (append bibretrieve-installed-backends '("DEFAULTS" "ALL")) nil t nil nil "DEFAULTS"))
	       (setq timeout (read-string "Timeout (seconds): [5] " nil nil "5")))))
    (setq backends
	  (cond ((or (not backend) (equal backend "DEFAULTS"))
		 (mapcar 'car bibretrieve-backends))
		((equal backend "ALL")
		 'bibretrieve-installed-backends)
		(t
		 `(,backend))))
    (bibretrieve-retrieve author title backends timeout))
  )

(defun bibretrieve-find-bibliography-file ()
   "Find the bibliography file.
Try to find some \bibliography declaration in the current buffer."
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	(goto-char (point-min))
	(if (re-search-forward "^[^%]*\\\\bibliography{\\(.*?\\)[ \\t\\n]*[,}]+" nil t)
	    (let ((file-string (match-string 1)))
	      (if (not (string-match ".*\\.bib$" file-string))
		  (concat file-string ".bib")
		  (concat file-string nil)
		  )
	      )
	  )
	)
      )
    )
  )

(defun bibretrieve-find-default-bibliography-file ()
   "Find a default bibliography file to write entries in.
Try with a \bibliography in the current buffer
or if the current buffer is a bib buffer,
else return nil."
  (or (bibretrieve-find-bibliography-file)
      (if buffer-file-name
	  (and (string-match ".*\\.bib$" buffer-file-name)
	       (buffer-file-name)))))

;; Copied from RefTeX
;; Limit FOUND-LIST with more regular expressions
;; Returns a string with all matching bibliography items
(defun bibretrieve-extract-bib-items (all &optional marked complement)
    (setq all (delq nil
                    (mapcar
                     (lambda (x)
                       (if marked
                           (if (or (and (assoc x marked) (not complement))
                                   (and (not (assoc x marked)) complement))
                               (cdr (assoc "&entry" x))
                             nil)
                         (cdr (assoc "&entry" x))))
                     all)))
    (mapconcat 'identity all "\n\n")
    )

(defun bibretrieve-write-bib-items-bibliography (all bibfile marked complement)
  "Append item to file.

From ALL, append to a promped file (BIBFILE is the default one) MARKED entries (or unmarked, if COMPLEMENT is t)."
  (let ((file (read-file-name (if bibfile (concat "Bibfile: [" bibfile "] ") "Bibfile: ") default-directory bibfile)))
    (if (find-file-other-window file)
	(save-excursion
	  (goto-char (point-max))
	  (insert "\n")
	  (insert (bibretrieve-extract-bib-items all marked complement))
	  (insert "\n")
	  (save-buffer)
	  file
	  )
      (error "Invalid file"))))

;; Callback function to be called from the bibliography selection, in
;; order to display context.
(defun bibretrieve-selection-callback (data ignore no-revisit)
  (let ((win (selected-window))
;        (key (reftex-get-bib-field "&key" data))
;        bibfile-list item bibtype)
	(origin (buffer-name)))
    (pop-to-buffer "*BibRetrieve Record*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (bibtex-mode)
    (goto-char (point-min))
    (insert (reftex-get-bib-field "&entry" data))
    ;;    (shrink-window-if-larger-than-buffer)  ; FIXME: this needs to be recalibrated for each record
    (goto-char (point-min))
    (setq buffer-read-only t)
    (pop-to-buffer origin)
    )
  )


;; Modified version of reftex-extract-bib-entries
;; It does not ask for a REGEXP, but it selects all
(defun bibretrieve-extract-bib-entries (buffers)
  "Extract bib entries which match regexps from BUFFERS.
BUFFERS is a list of buffers or file names.
Return list with entries.\""
  (let* (        (buffer-list (if (listp buffers) buffers (list buffers)))
                 found-list entry buffer1 buffer alist
                 key-point start-point end-point default)

    (save-excursion
      (save-window-excursion

        ;; Walk through all bibtex files
        (while buffer-list
          (setq buffer (car buffer-list)
                buffer-list (cdr buffer-list))
          (if (and (bufferp buffer)
                   (buffer-live-p buffer))
              (setq buffer1 buffer)
            (setq buffer1 (reftex-get-file-buffer-force
                           buffer (not reftex-keep-temporary-buffers))))
          (if (not buffer1)
              (message "No such BibTeX file %s (ignored)" buffer)
            (message "Scanning bibliography database %s" buffer1)
	    (unless (verify-visited-file-modtime buffer1)
		 (when (y-or-n-p
			(format "File %s changed on disk.  Reread from disk? "
				(file-name-nondirectory
				 (buffer-file-name buffer1))))
		   (with-current-buffer buffer1 (revert-buffer t t)))))

          (set-buffer buffer1)
          (reftex-with-special-syntax-for-bib
           (save-excursion
             (goto-char (point-min))
             (while (re-search-forward "=" nil t)
               (catch 'search-again
                 (setq key-point (point))
                 (unless (re-search-backward "\\(\\`\\|[\n\r]\\)[ \t]*\
@\\(\\(?:\\w\\|\\s_\\)+\\)[ \t\n\r]*[{(]" nil t)
                   (throw 'search-again nil))
                 (setq start-point (point))
                 (goto-char (match-end 0))
                 (condition-case nil
                     (up-list 1)
                   (error (goto-char key-point)
                          (throw 'search-again nil)))
                 (setq end-point (point))

                 ;; Ignore @string, @comment and @c entries or things
                 ;; outside entries
                 (when (or (string= (downcase (match-string 2)) "string")
                           (string= (downcase (match-string 2)) "comment")
                           (string= (downcase (match-string 2)) "c")
                           (< (point) key-point)) ; this means match not in {}
                   (goto-char key-point)
                   (throw 'search-again nil))

                 ;; Well, we have got a match
                 ;;(setq entry (concat
                 ;;             (buffer-substring start-point (point)) "\n"))
                 (setq entry (buffer-substring start-point (point)))

                 (setq alist (reftex-parse-bibtex-entry
                              nil start-point end-point))
                 (push (cons "&entry" entry) alist)

                 ;; check for crossref entries
                 (if (assoc "crossref" alist)
                     (setq alist
                           (append
                            alist (reftex-get-crossref-alist alist))))

                 ;; format the entry
                 (push (cons "&formatted" (reftex-format-bib-entry alist))
                       alist)

                 ;; make key the first element
                 (push (reftex-get-bib-field "&key" alist) alist)

                 ;; add it to the list
                 (push alist found-list)))))
          (reftex-kill-temporary-buffers))))
    (setq found-list (nreverse found-list))

    ;; Sorting
    (cond
     ((eq 'author reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-author))
     ((eq 'year   reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-year))
     ((eq 'reverse-year reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-year-reverse))
     (t found-list))))


;; Modified version of reftex-offer-bib-menu
(defun bibretrieve-offer-bib-menu (&optional arg)
  "Offer bib menu and return list of selected items.
ARG is the optional argument."

  (let ((bibfile (bibretrieve-find-default-bibliography-file))
        found-list rtn key data selected-entries)
    (while
        (not
         (catch 'done
           ;; Retrieve and scan entries
           (setq found-list (bibretrieve-extract-bib-entries (bibretrieve-prompt-and-retrieve arg)))

           (unless found-list
             (error "Sorry, no matches found"))

          ;; Remember where we came from
          (setq reftex-call-back-to-this-buffer (current-buffer))
          (set-marker reftex-select-return-marker (point))

          ;; Offer selection
          (save-window-excursion
            (delete-other-windows)
            (let ((major-mode 'reftex-select-bib-mode))
              (reftex-kill-buffer "*RefTeX Select*")
              (switch-to-buffer-other-window "*RefTeX Select*")
              (unless (eq major-mode 'reftex-select-bib-mode)
                (reftex-select-bib-mode))
              (let ((buffer-read-only nil))
                (erase-buffer)
                (reftex-insert-bib-matches found-list)))
            (setq buffer-read-only t)
            (if (= 0 (buffer-size))
                (error "No matches found"))
            (setq truncate-lines t)
            (goto-char 1)
            (while t
              (setq rtn
                    (reftex-select-item
                     bibretrieve-select-prompt
                     bibretrieve-select-help
                     reftex-select-bib-map
                     nil
                     'bibretrieve-selection-callback nil))
              (setq key (car rtn)
                    data (nth 1 rtn))
              (unless key (throw 'done t))
              (cond
               ((eq key ?g)
                ;; Start over
                (throw 'done nil))
               ((eq key ?r)
                ;; Restrict with new regular expression
                (setq found-list (reftex-restrict-bib-matches found-list))
                (let ((buffer-read-only nil))
                  (erase-buffer)
                  (reftex-insert-bib-matches found-list))
                (goto-char 1))
               ((eq key ?A)
                ;; Take all
                (setq selected-entries found-list)
                (throw 'done t))
               ((eq key ?a)
                ;; Take all marked
		;; If nothing is marked, then mark current selection
		(if (not reftex-select-marked)
		    (reftex-select-mark))
                (setq selected-entries (mapcar 'car (nreverse reftex-select-marked)))
                (throw 'done t))
               ((eq key ?e)
                ;; Take all marked and append them
                (let ((file (bibretrieve-write-bib-items-bibliography found-list bibfile reftex-select-marked nil)))
		  (when file
		    (setq selected-entries
			  (concat "BibTeX entries appended to " file))
		    (throw 'done t)))
		(message "File not found, nothing done"))
               ((eq key ?E)
                ;; Take all unmarked and append them
                (let ((file (bibretrieve-write-bib-items-bibliography found-list bibfile reftex-select-marked 'complement)))
		  (when file
		    (setq selected-entries
			  (concat "BibTeX entries appended to " file))
		    (throw 'done t)))
		(message "File not found, nothing done"))
               ((or (eq key ?\C-m)
                    (eq key 'return))
                ;; Take selected
		;; If nothing is marked, then mark current selection
		(let ((marked reftex-select-marked))
		    (unless marked (reftex-select-mark))
		    (let ((file (bibretrieve-write-bib-items-bibliography found-list bibfile reftex-select-marked nil)))
		      (when file
			(setq selected-entries (concat "BibTeX entries appended to " file))
			(throw 'done t)))
		    (unless marked (reftex-select-unmark)))
		(message "File not found, nothing done. Press q to exit."))
               ((stringp key)
                ;; Got this one with completion
                (setq selected-entries key)
                (throw 'done t))
               (t
                (ding))))))))
    selected-entries))




;; Get records from the web and insert them in the bibliography

;; Adapted from RefTeX
;;;###autoload
(defun bibretrieve ()
  "Search the web for bibliography entries.

After prompting for author and title, searches on the web, using the
backends specified by the customization variable
`bibretrieve-backends'.  A selection process (using RefTeX Selection)
allows to select entries to add to the current buffer or to a
bibliography file.

 When called with a `C-u' prefix, permits to select the backend and the
 timeout for the search."

  (interactive)

  ;; check for recursive edit
  (reftex-check-recursive-edit)

  ;; This function may also be called outside reftex-mode.
  ;; Thus look for the scanning info only if in reftex-mode.

  (when reftex-mode
    (reftex-access-scan-info nil))

  ;; Call bibretrieve-do-retrieve, but protected
  (unwind-protect
      (bibretrieve-do-retrieve current-prefix-arg)
    (progn
      (reftex-kill-temporary-buffers)
      (reftex-kill-buffer "*BibRetrieve Record*")
      (reftex-kill-buffer "*RefTeX Select*"))))

;; Adapted from RefTeX
(defun bibretrieve-do-retrieve (&optional arg)
  "This really does the work of bibretrieve.
ARG is the optional argument."

  (let ((selected-entries (bibretrieve-offer-bib-menu arg)))

    (set-marker reftex-select-return-marker nil)

    (if (stringp selected-entries)
      (message selected-entries)
      (if (not selected-entries)
	  (message "Quit")
 	(insert (bibretrieve-extract-bib-items selected-entries))
	)
      )
    ))


;;; bibretrieve.el ends here
