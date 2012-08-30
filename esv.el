;;; esv.el --- Support for Crossway's ESV API in Emacs

;; Copyright (C) 2008-2012 Charles Sebold

;; Author: Charles Sebold <csebold@gmail.com>
;; Created: 1 Jan 2008
;; Version: 1.27
;; Keywords: comm, hypermedia

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; if not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Latest version should be available at:
;;;    <URL:http://www.emacswiki.org/cgi-bin/wiki/EsvMode>
;;;
;;; To use this, you must customize or otherwise set the variable
;;; ESV-KEY!  Otherwise by default the ESV API handlers will not call out
;;; to the ESV website.  At the minimum, do this:
;;;
;;;   M-x customize-variable RET esv-key RET
;;;
;;; And set it to "Non-keyed usage" (IP) if you're just going to use this
;;; for personal use in Emacs.  Details of the ESV license for this
;;; information can be found at http://www.esvapi.org/ and I recommend
;;; looking it over.
;;;
;;; This package consists of two functionalities:  one is to recognize
;;; passages in your buffers and make it easy for you to look them up in
;;; the ESV (that's esv-mode), and one is a way to retrieve ESV passages
;;; using their web API and display them internally in Emacs.  This can
;;; be used to get individual passages (which you can specify or retrieve
;;; from your text using esv-mode), or you can go through a daily reading
;;; plan (great for those of us who live in Emacs).
;;;
;;; To use this package, you can save this file somewhere in your
;;; load-path and put the following in your .emacs at a minimum:
;;;
;;;   (require 'esv)
;;;   ; the following keys should be mapped to whatever works best for
;;;   ; you:
;;;   ; C-c e looks up a passage and displays it in a pop-up window
;;;   (define-key global-map [(control c) ?e] 'esv-passage)
;;;   ; C-c i inserts an ESV passage in plain-text format at point
;;;   (define-key global-map [(control c) ?i] 'esv-insert-passage)
;;;   ; If you don't want to use customize, you can set this for casual
;;;   ; usage (but read http://www.esvapi.org/ for license):
;;;   (setq esv-key "IP")
;;;
;;; To make esv-mode work whenever text-mode does, you can put this in
;;; your .emacs as well:
;;;
;;;   (add-hook 'text-mode-hook 'turn-on-esv-mode)
;;;
;;; If you only want it in certain files, you can add it to the first
;;; line or the local variables (see (info "(emacs)File Variables") for
;;; more information):
;;;
;;;     -*- mode: text; mode: esv; -*-
;;;
;;; or
;;;
;;;     Local Variables:
;;;     mode: esv;
;;;     End:
;;;
;;; However, note the warning about imposing your individual preferences
;;; on files at the end of the info node above.
;;;
;;; The ESV API is documented at:
;;; http://www.esvapi.org/v2/rest/

;;; Code:

(require 'xml)
(require 'url)
(require 'calendar)

(defgroup esv nil
  "ESV Web service customization group.")

(defcustom esv-key nil
  "This should be a string with your ESV API key in it, \"IP\" if your
use qualifies you for it, or \"TEST\" if you're just testing it.
You have to customize this to use esv.el, so you are making a
conscious decision as to whether you are abiding by Crossway's
terms of service.  See http://www.esvapi.org/ for more details."
  :type '(choice (const :tag "Default, will not work." nil)
                 (const :tag "Testing value" "TEST")
                 (const :tag "Non-keyed usage" "IP")
                 (string :tag "Your API key"))
  :group 'esv)

(defcustom esv-output-format "crossway-xml-1.0"
  "The output format from the ESV API.  esv.el can parse the default,
or you may just want to do it yourself."
  :type '(choice (const :tag "Crossway XML 1.0" "crossway-xml-1.0")
                 (const :tag "HTML" nil)
                 (const :tag "Plain text" "plain-text"))
  :group 'esv)

(defcustom esv-include-simple-entities "true"
  "Should XML output from the ESV API include simplified entities?"
  :type '(choice (const :tag "Yes" "true")
                 (const :tag "No" "false"))
  :group 'esv)

(defcustom esv-include-footnotes "true"
  "Should ESV output include footnotes? (Not fully implemented.)"
  :type '(choice (const :tag "Yes" "true")
                 (const :tag "No" "false"))
  :group 'esv)

(defcustom esv-include-passage-horizontal-lines "false"
  "Should ESV output include lines above passages in plain text?"
  :type '(choice (const :tag "Yes" "true")
                 (const :tag "No" "false"))
  :group 'esv)

(defcustom esv-include-heading-horizontal-lines "false"
  "Should ESV output include lines above headings in plain text?"
  :type '(choice (const :tag "Yes" "true")
                 (const :tag "No" "false"))
  :group 'esv)

(defcustom esv-include-headings "false"
  "Should ESV output include section headings?"
  :type '(choice (const :tag "Yes" "true")
                 (const :tag "No" "false"))
  :group 'esv)

(defcustom esv-include-subheadings "true"
  "Should ESV output include subheadings?"
  :type '(choice (const :tag "Yes" "true")
                 (const :tag "No" "false"))
  :group 'esv)

(defcustom esv-start-date nil
  "When should an ESV reading plan start?"
  :type '(choice (const :tag "Default, January 1" nil)
                 (string :tag "Date in YYYY-MM-DD format"))
  :group 'esv)

(defcustom esv-reading-plan "one-year-tract"
  "Which ESV reading plan will you use?"
  :type '(choice
          (const :tag "Book of Common Prayer Daily Office Lectionary" "bcp")
          (const :tag "ESV Study Bible" "esv-study-bible")
          (const :tag "Every Day in the Word" "every-day-in-the-word")
          (const :tag "Literary Study Bible" "lsb")
          (const :tag "One Year Tract (M'Cheyne)" "one-year-tract")
          (const :tag "Outreach" "outreach")
          (const :tag "Outreach NT" "outreach-nt")
          (const :tag "Through the Bible in a Year" "through-the-bible")
          )
  :group 'esv)

(defcustom esv-mode-browse-reference 'esv-passage
  "What function will you use to browse ESV passages in ESV minor mode?"
  :type '(choice (const :tag "Internal (esv.el)" esv-passage)
                 (const :tag "External (uses browse-url)" esv-external-browse-reference)
                 (sexp :tag "Function (must accept text of reference as argument)"))
  :group 'esv)

(defface esv-mode-reference
  '((((class color) (background light)) (:foreground "Purple" :underline t))
    (((class color) (background dark)) (:foreground "Cyan" :underline t))
    (t (:underline t)))
  "Face for Bible references in `esv-mode'."
  :group 'esv)

(defvar esv-url-base "http://www.esvapi.org/v2/rest/"
  "First part of the URL used to connect to the ESV web API.")
(defvar esv-browse-base "http://www.gnpcb.org/esv/search/?q="
  "First part of hte URL used to display passages on the ESV website.")

(defvar bible-abbrevs
  '("Genesis" "Gen." "Gen" "Ge." "Ge"
    "Exodus" "Exo." "Ex." "Exo" "Ex"
    "Leviticus" "Lev." "Lev" "Le." "Le"
    "Numbers" "Num." "Num" "Nu." "Nu"
    "Deuteronomy" "Deut." "Deut" "Deu." "Deu" "De." "De"
    "Joshua" "Josh." "Josh" "Jos." "Jos"
    "Judges" "Judg." "Judg" "Jud." "Jud"
    "Ruth" "Rut." "Rut" "Ru." "Ru"
    "1 Samuel" "1 Sam." "1 Sam" "1 Sa." "1 Sa" "1Sam." "1Sam" "1Sa." "1Sa"
    "2 Samuel" "2 Sam." "2 Sam" "2 Sa." "2 Sa" "2Sam." "2Sam" "2Sa." "2Sa"
    "1 Kings" "1 Kin." "1 Kin" "1 Ki." "1 Ki" "1Kin." "1Kin" "1Ki." "1Ki"
    "2 Kings" "2 Kin." "2 Kin" "2 Ki." "2 Ki" "2Kin." "2Kin" "2Ki." "2Ki"
    "1 Chronicles" "1 Chr." "1 Chr" "1 Ch." "1 Ch" "1Chr." "1Chr" "1Ch." "1Ch"
    "2 Chronicles" "2 Chr." "2 Chr" "2 Ch." "2 Ch" "2Chr." "2Chr" "2Ch." "2Ch"
    "Ezra" "Ezr." "Ezr"
    "Nehemiah" "Neh." "Neh" "Ne." "Ne"
    "Esther" "Esth." "Esth" "Est." "Est" "Es." "Es"
    "Job"
    "Psalms" "Psalm" "Psa." "Psa" "Ps." "Ps"
    "Proverbs" "Prov." "Prov" "Pro." "Pro" "Pr." "Pr"
    "Ecclesiastes" "Eccl." "Eccl" "Ecc." "Ecc" "Ec." "Ec"
    "Song of Songs" "Song of Solomon" "Song" "Son." "Son" "So." "So"
    "Isaiah" "Isa." "Isa" "Is." "Is"
    "Jeremiah" "Jer." "Jer" "Je." "Je"
    "Lamentations" "Lam." "Lam" "La." "La"
    "Ezekiel" "Ezek." "Ezek" "Eze." "Eze"
    "Daniel" "Dan." "Dan" "Da." "Da"
    "Hosea" "Hos." "Hos" "Ho." "Ho"
    "Joel"
    "Amos" "Amo." "Amo" "Am." "Am"
    "Obadiah" "Obad." "Obad" "Oba." "Oba" "Ob." "Ob"
    "Jonah" "Jon." "Jon"
    "Micah" "Mic." "Mic" "Mi." "Mi"
    "Nahum" "Nah." "Nah" "Na." "Na"
    "Habakkuk" "Hab." "Hab"
    "Zephaniah" "Zeph." "Zeph" "Zep." "Zep"
    "Haggai" "Hag." "Hag"
    "Zechariah" "Zech." "Zech" "Zec." "Zec"
    "Malachi" "Mal." "Mal"
    "Matthew" "Matt." "Matt" "Mat." "Mat"
    "Mark" "Mar." "Mar"
    "Luke" "Luk." "Luk" "Lu." "Lu"
    "John" "Joh." "Joh" "Jn." "Jn"
    "Acts of the Apostles" "Acts" "Act." "Act" "Ac." "Ac"
    "Romans" "Rom." "Rom" "Ro." "Ro"
    "1 Corinthians" "1 Cor." "1 Cor" "1 Co." "1 Co" "1Cor." "1Cor" "1Co." "1Co"
    "2 Corinthians" "2 Cor." "2 Cor" "2 Co." "2 Co" "2Cor." "2Cor" "2Co." "2Co"
    "Galatians" "Gal." "Gal" "Ga." "Ga"
    "Ephesians" "Eph." "Eph" "Ep." "Ep"
    "Philippians" "Phil." "Phil"
    "Colossians" "Col." "Col" "Co." "Co"
    "1 Timothy" "1 Tim." "1 Tim" "1 Ti." "1 Ti" "1Tim." "1Tim" "1Ti." "1Ti"
    "2 Timothy" "2 Tim." "2 Tim" "2 Ti." "2 Ti" "2Tim." "2Tim" "2Ti." "2Ti"
    "Titus" "Tit." "Tit"
    "Philemon" "Phlm." "Phlm" "Phl." "Phl"
    "1 Thessalonians" "1 Thess." "1 Thess" "1 Thes." "1 Thes" "1 The." "1 The"
                      "1 Th." "1 Th" "1Thess." "1Thess" "1Thes." "1Thes" "1The."
                      "1The" "1Th." "1Th"
    "2 Thessalonians" "2 Thess." "2 Thess" "2 Thes." "2 Thes" "2 The." "2 The"
                      "2 Th." "2 Th" "2Thess." "2Thess" "2Thes." "2Thes" "2The."
                      "2The" "2Th." "2Th"
    "Hebrews" "Hebr." "Hebr" "Heb." "Heb" "He." "He"
    "James" "Jam." "Jam" "Jas." "Jas" "Ja." "Ja"
    "1 Peter" "1 Pet." "1 Pet" "1 Pe." "1 Pe" "1Pet." "1Pet" "1Pe." "1Pe"
    "2 Peter" "2 Pet." "2 Pet" "2 Pe." "2 Pe" "2Pet." "2Pet" "2Pe." "2Pe"
    "1 John" "1 Joh." "1 Joh" "1 Jo." "1 Jo" "1 Jn." "1 Jn"
             "1Joh." "1Joh" "1Jo." "1Jo" "1Jn." "1Jn"
    "2 John" "2 Joh." "2 Joh" "2 Jo." "2 Jo" "2 Jn." "2 Jn"
             "2Joh." "2Joh" "2Jo." "2Jo" "2Jn." "2Jn"
    "3 John" "3 Joh." "3 Joh" "3 Jo." "3 Jo" "3 Jn." "3 Jn"
             "3Joh." "3Joh" "3Jo." "3Jo" "3Jn." "3Jn"
    "Jude" "Jud." "Jud"
    "Revelation" "Rev." "Rev" "Re." "Re"
    "Apocalypse" "Apoc." "Apoc" "Apo." "Apo" "Ap." "Ap")
  "All the book abbreviations I can think of in Bible references.")

(defvar bible-ref-regexp
  (eval-when-compile
    ; need to add the empty-match word boundaries around this regexp FIXME
    (concat "\\<" (regexp-opt bible-abbrevs t) "[ \t\n]*[0-9]+\\(?:[-0-9, :]*[0-9]\\)?\\>"))
  "Regular expression compiled from possible book abbreviation in Bible references.")

(defvar esv-footnote-entities
  '(("&emacron;" . "ē"))
  "List of entities and replacement characters.")

(defvar esv-mode nil
  "Variable controlling `esv-mode', a minor mode in which Bible
references are highlighted and clickable.")

(defvar esv-mode-map (make-sparse-keymap)
  "Keymap for `esv-mode'.")

(define-key esv-mode-map [mouse-2] 'esv-mode-mouse-2)
(define-key esv-mode-map [(control c) ??] 'esv-mode-lookup)

(defun esv-reference-cleanup (ref-string)
  "Returns REF-STRING without carriage returns or extra spaces, useful
when creating a reference to call out to the web API."
  (with-temp-buffer
    (insert ref-string)
    (goto-char (point-min))
    (while (re-search-forward "\n\\|\t" nil t)
      (replace-match ""))
    (while (re-search-forward " \\{2,\\}" nil t)
      (replace-match " "))
    (buffer-string)))

(defun esv-url-create (ref-string)
  "Returns REF-STRING without carriage returns and with spaces converted
to + signs, useful when creating a URL to lookup on the ESV website."
  (with-temp-buffer
    (insert esv-browse-base)
    (insert ref-string)
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward " " nil t)
      (replace-match "+"))
    (buffer-string)))

(defun turn-on-esv-mode ()
  "Unconditionally turn on `esv-mode'."
  (esv-mode 1))

(define-minor-mode esv-mode " ESV" esv-mode-map)

(defvar esv-mode-hook nil
  "Normal hook run when entering esv-mode.")

(defun esv-mode (&optional arg)
  "Toggle `esv-mode'.  Positive prefix argument turns the mode on
unconditionally.  Negative or zero prefix argument turns the mode
off."
  (interactive)
  (setq esv-mode
        (if arg (> (prefix-numeric-value arg) 0)
          (not esv-mode)))
  (if esv-mode
      (progn
        (when (fboundp 'font-lock-add-keywords)
          (font-lock-add-keywords nil esv-font-lock-keywords)
          (font-lock-fontify-buffer))
        (run-hooks 'esv-mode-hook))
    (when (fboundp 'font-lock-remove-keywords)
      (font-lock-remove-keywords nil esv-font-lock-keywords)
      (font-lock-fontify-buffer))
    (force-mode-line-update 'all)))

(put 'esv-mode :included t)
(put 'esv-mode :menu-tag "ESV Mode")
(put 'esv-mode :global nil)
; FIXME: modeline indicator isn't working
(put 'esv-mode :lighter " ESV")
(put 'esv-mode :keymap esv-mode-map)

(defvar esv-display-mode-hook nil
  "Normal hook run when entering esv-display-mode.")

(define-derived-mode esv-display-mode view-mode "ESV-Display"
  "Major mode for displaying ESV passages."
  (setq esv-display-mode-map (make-sparse-keymap))
  (set-keymap-parent esv-display-mode-map view-mode-map)
  (define-key esv-display-mode-map "q" 'esv-display-mode-quit)
  (view-mode -1)
  (use-local-map esv-display-mode-map)
  (run-hooks 'esv-display-mode-hook)
  :group 'esv)

(defun esv-display-mode-quit ()
  "Make ESV display window go away."
  (interactive)
  (if (one-window-p)
      (bury-buffer)
    (delete-window)))

; not right
(defgroup esv-display-mode-faces nil
  "Faces used for displaying ESV files."
  :group 'faces)

; possibly not right
(defvar esv-display-mode-font-lock-keywords
  (eval-when-compile
    (list
     ; bible references
     (list (eval bible-ref-regexp) '(0 font-lock-variable-name-face))
     ; inline verse numbers
     (list "\\[[0-9]+\\]" '(0 font-lock-reference-face))))
  "Highlighting rules for esv-display-mode.")

(defconst esv-font-lock-keywords
  (list 
   '(esv-clickable-refs (0 'esv-mode-reference t)))
  "Font-lock-keywords to be added when esv-mode is active.")

(defun esv-clickable-refs (limit)
  "Font-lock function which finds Bible references and makes them
clickable."
  (if (re-search-forward (eval bible-ref-regexp) limit t)
      (progn
        (add-text-properties (match-beginning 0) (match-end 0)
                             (list 'mouse-face 'highlight
;                                   'font-lock-multiline t
                                   'esv-reference
                                   (esv-reference-cleanup (match-string 0))))
        t)))

(defun esv-mode-mouse-2 (event arg)
  "Fetch ESV text for reference under the mouse click."
  (interactive "e\nP")
  (let (my-passage)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (goto-char (posn-point (event-end event)))
      (setq my-passage (get-text-property (point) 'esv-reference)))
    (if my-passage
        (progn
          (select-window (posn-window (event-end event)))
          (esv-browse-reference my-passage))
      (mouse-yank-at-click event arg))))

(defun esv-mode-lookup ()
  "Lookup ESV passage at POINT."
  (interactive)
  (let ((my-reference (get-text-property (point) 'esv-reference)))
    (if my-reference
        (esv-browse-reference my-reference))))

(defun esv-browse-reference (reference)
  "Wrapper function to call out to ESV website and lookup REFERENCE."
  (eval (list esv-mode-browse-reference reference)))

(defun esv-external-browse-reference (reference)
  "Wrapper function to call standard Emacs browser function for
REFERENCE."
  (message "Linking to ESV website for %s..." reference)
  (browse-url (esv-url-create reference)))

(defun esv-get-query (query-alist)
  "Parse QUERY-ALIST into a URL, call the ESV web API to retrieve the
data requested, and return the actual result.  This is the function that
absolutely requires the variable `esv-key' to be set to something
useful."
  (if (or (assq 'key query-alist)
          esv-key)
      (let ((passage-query (assq 'passage-query query-alist))
            (passage ""))
        (if passage-query
            (progn
              (setq passage
                    (with-temp-buffer
                      (insert (cadr passage-query))
                      (goto-char (point-min))
                      (while (re-search-forward " " nil t)
                        (replace-match "%20"))
                      (buffer-string)))
              (setq query-alist (assq-delete-all 'passage-query query-alist))
              (push (list 'passage-query passage) query-alist)))
        (switch-to-buffer
         (url-retrieve-synchronously
          (esv-query query-alist)))
        (prog1
            (buffer-substring
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "\n\n" nil t)
               (point))
             (point-max))
          (kill-buffer nil)))
    (error "No ESV key to use, please customize the ESV group and select a key.")))

(defun esv-query (query-alist)
  "Parse QUERY-ALIST and return the URL used to connect to the ESV web
API."
  (let ((orig-alist (copy-alist query-alist)))
    (concat
     (or (and
          (assq 'url-base query-alist)
          (cadr (assq 'url-base query-alist)))
         esv-url-base)
     (cond
      ((assq 'passage-query query-alist)
       "passageQuery?")
      ((assq 'reading-plan-query query-alist)
       "readingPlanQuery?")
      ((assq 'query query-alist)
       "query?"))
     (or (and
          (assq 'key query-alist)
          (cadr (assq 'key query-alist)))
         (concat "key=" esv-key))
     "&output-format="
     (let ((my-output-format
            (or (and
                 (assq 'output-format query-alist)
                 (cadr (assq 'output-format query-alist)))
                esv-output-format)))
           (if (string= my-output-format "crossway-xml-1.0")
               (unless (assq 'include-simple-entities query-alist)
                 (push (list 'include-simple-entities esv-include-simple-entities) query-alist)))
           my-output-format)
     (progn
       (dolist (this-value '(url-base key output-format reading-plan-query
                                      passage-query query))
         (setq query-alist (assq-delete-all this-value query-alist)))
       (let ((value-list ""))
         (dolist (this-value query-alist)
           (if (cadr this-value)
               (setq value-list
                     (concat value-list
                             "&"
                             (symbol-name (car this-value))
                             "="
                             (cadr this-value)))))
         value-list))
     (cond
      ((assq 'passage-query orig-alist)
       (concat "&passage=" (cadr (assq 'passage-query orig-alist))))))))

(defun esv-xml-to-list (xml)
  "Wrapper function to call `xml-parse-region' and return a list of
elements."
  (with-temp-buffer
    (insert xml)
    (xml-parse-region (point-min) (point-max))))

(defun esv-walk-tree (esv-xml)
  "Process XML returned when `esv-output-format' is \"crossway-xml-1.0\"
into propertized text suitable for display in `esv-display-mode'."
  (with-temp-buffer
    (dolist (cb esv-xml)
      (if (eq (car cb) 'crossway-bible)
          (dolist (passage cb)
            (cond
             ((and (listp passage)
                   (eq (car passage) 'passage))
              (dolist (content passage)
                (cond
                 ((and (listp content)
                       (eq (car content) 'reference))
                  (insert (propertize (car (last content))
                                      'face 'font-lock-variable-name-face)
                          "\n"))
                 ((and (listp content)
                       (eq (car content) 'content))
                  (dolist (verse-unit content)
                    (if (and (listp verse-unit)
                             (eq (car verse-unit) 'verse-unit))
                        (dolist (verse-item verse-unit)
                          (cond
                           ((and (listp verse-item)
                                 (eq (car verse-item) 'verse-num))
                            (insert (propertize
                                     (concat " [" (car (last verse-item)) "] ")
                                     'face 'font-lock-variable-name-face)))
                           ((and (listp verse-item)
                                 (eq (car verse-item) 'woc))
                            (dolist (woc-elem (cdr verse-item))
                              (if (stringp woc-elem)
                                  (insert woc-elem))))
                           ; thanks to Ben from edginet.org for this
                           ; otherwise the Divine Name gets lost
                           ((and (listp verse-item)
                                 (eq (car verse-item) 'span))
                            (dolist (span-elem (cdr verse-item))
                              (if (stringp span-elem)
                                  (insert span-elem))))
                           ((stringp verse-item)
                            (insert verse-item))))))))))
             ((and (listp passage)
                   (eq (car passage) 'copyright))
              (insert (propertize (car (last passage))
                                  'face 'font-lock-doc-face)))
             ((stringp passage)
              (insert passage))))))
    (fill-region (point-min) (point-max))
    (buffer-string)))

(defun esv-passage (reference)
  "Fetch and display Bible REFERENCE from the ESV web API.

This is the primary interactive function in esv.el.  REFERENCE can be
any well-formed Bible reference."
  (interactive "MESV reference: ")
  (switch-to-buffer-other-window
   (generate-new-buffer (concat "*ESV: " reference "*")))
  (insert (esv-walk-tree
           (esv-xml-to-list
            (esv-get-query (list (list 'passage-query reference))))))
  (goto-char (point-min))
  (toggle-read-only 1)
  (esv-display-mode))

(defun esv-region (start end)
  "Fetch and display Bible reference in region from START to END
from the ESV web API."
  (interactive "r")
  (esv-passage (buffer-substring-no-properties start end)))

(defun esv-insert-passage (reference)
  "Insert ESV passage REFERENCE at point.

Instead of using the built-in Crossway-XML-1.0 parser, this ignores
`esv-output-format' and sets it to \"plain-text\", which lets the ESV
website format the text according to its standards.  `my-fill-column'
will determine the maximum line-length returned."
  (interactive "MESV reference: ")
  (let ((my-fill-column fill-column)
        (start (point)))
    (insert
     (esv-get-query
      (list
       (list
        'output-format "plain-text")
       (list
        'passage-query reference)
       (list 'include-footnotes
             esv-include-footnotes)
       (list 'include-passage-horizontal-lines
             esv-include-passage-horizontal-lines)
       (list 'include-heading-horizontal-lines
             esv-include-heading-horizontal-lines)
       (list 'include-headings
             esv-include-headings)
       (list 'include-subheadings
             esv-include-subheadings)
       (list 'line-length
             (format "%d" my-fill-column)))))
    (save-excursion
      (narrow-to-region start (point))
      (dolist (i esv-footnote-entities)
        (goto-char (point-min))
        (while (re-search-forward (regexp-quote (car i)) nil t)
          (replace-match (cdr i) t t)))
      (widen))))

(defun esv-calendar-return-time ()
  "Ask for a specific date using `calendar-read-date'."
  (let ((my-date (calendar-read-date)))
    (apply 'encode-time
           (append (list 1 1 1
                         (cadr my-date)
                         (car my-date))
                   (cddr my-date)))))

(defun esv-reading-plan (&optional date-string)
  "Fetches and displays today's Bible passages from the ESV web API.

Use prefix argument to specify date.  If you started a reading plan on
some day other than January 1, you can customize that with
`esv-start-date'.  Defaults to M'Cheyne's reading plan but you can
change that by customizing `esv-reading-plan'."
  (interactive "P")
  (if date-string
      (setq date-string
            (format-time-string "%Y-%m-%d" 
                                (esv-calendar-return-time)))
    (setq date-string (format-time-string "%Y-%m-%d")))
  (switch-to-buffer-other-window
   (generate-new-buffer (concat "*ESV: " date-string "*")))
  (insert (esv-walk-tree
           (esv-xml-to-list
            (esv-get-query (list
                            (list 'reading-plan-query t)
                            (list 'start-date esv-start-date)
                            (list 'date date-string))))))
  (goto-char (point-min))
  (toggle-read-only 1)
  (esv-display-mode))

(defalias 'esv 'esv-passage)

(provide 'esv)

;;; esv.el ends here
