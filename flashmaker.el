;;; flashmaker.el -- created flashcard decks using existing dicitonary files

;; Copyright (C) 2002  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.0
;; Keywords: i18n, vocab
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?FlashMaker

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides a number of wizards to help you create
;; flashcard decks based on a number of existing dictionaries.
;; currently supported are all dictionaries in the KANJIDIC and EDICT
;; format, ie. KANJIDIC, EDICT, ENUMDIC, JDDICT, COMPDIC.

;; Use `flashmaker-kanjidic-filter' to create a new flashcard deck
;; based on a KANJIDIC format file.

;; Use `flashmaker-edict-filter' to create a new flashcard deck based
;; on a KANJIDIC format file, and an EDICT format file.  The KANJIDIC
;; file will be used to classify all characters, and then the
;; expressions in the EDICT file will be filtered according to this
;; classification, thus producing a subset of the EDICT File limited
;; to the kanji you want to practice.

;; When reading the files, the encoding can be problematic.  If your
;; Emacs does not recognize EUC-JP automatically, I recommend the
;; following addition to your init file:

;; (add-to-list 'auto-coding-alist '("edict\\'" . euc-jp))
;; (add-to-list 'auto-coding-alist '("kanjidic\\'" . euc-jp))

;; When saving the files, I recommend the coding system iso-2022-jp.

;; In debian, use "apt-get install kanjidic" for a large dictionary of
;; Japanese kanji including a document explaining the format of the
;; dictionary file.

;; You can use flashcard decks to practice using flashcard.el by
;; Jorgen Schaefer.

;;; TODO:

;; Add support for XML based dictionaries.

;;; History:

;; This code is based on the parsing code of kdic.el 1.7.1.

;;; Code

(require 'widget)
(eval-when-compile
  (require 'wid-edit))

;;; Internal stuff

(defvar flashmaker-list nil
  "The list of entries read while making a flashcard deck.")

;;; Accessing dictionary entries

(defun flashmaker-key (entry)
  "Return the key of ENTRY.
This is a multi-byte string."
  (nth 0 entry))

(defun flashmaker-meanings (entry)
  "Return the meanings of ENTRY.
This is a list of strings."
  (nth 1 entry))

(defun flashmaker-readings (entry)
  "Return the readings of ENTRY.
This is a list of multi-byte strings or nil."
  (nth 2 entry))

(defun flashmaker-index (entry)
  "Return the index of ENTRY.
This is a number or nil."
  (nth 3 entry))

(defun flashmaker-set-index (entry val)
  "Set the index of ENTRY to VAL.
This must be a number or nil."
  (setcar (nthcdr 3 entry) val))

(defun flashmaker-entry (key)
  "Return the entry from `flashmaker-list' for KEY."
  (assoc key flashmaker-list))

;;; Parsing the dictionary

(defun flashmaker-parse (file parse-entry-func &optional args)
  "Parse FILE and return the entries in a list.
You might want to bind `coding-system-for-read' before calling this function.
For every line, PARSE-ENTRY-FUNC is called with ARGS for arguments."
  (let (result entry)
    (with-temp-buffer
      (insert-file-contents file)
      (flashmaker-parse-entries file (buffer-size) parse-entry-func args))))

(defun flashmaker-parse-entries (file total parse-entry-func &optional args)
  "Parse the lines of the current buffer.
FILE is the filename.
TOTAL indicates the buffer size.
For every line, PARSE-ENTRY-FUNC is called with ARGS for arguments."
  (let (entry result)
    (while (not (eobp))
      (setq entry (apply 'funcall parse-entry-func args))
      (when entry
	(setq result (cons entry result)))
      (forward-line)
      (message "Parsing %s...%d%%" file (/ (* 100.0 (point)) total)))
    (message "Parsing %s...done" file)
    (nreverse result)))

;;; KANJIDIC parsing

(defun flashmaker-parse-kanjidic-entry (&optional index-regexp)
  "Parse current line of kanjidic dictionaries.
With an optional INDEX-REGEXP, the index will be set to the first group
in the regexp, converted to a number, if matched."
  (when (looking-at "\\sw+")
    (let ((key (match-string 0))
	  (start (point))
	  (end (line-end-position))
	  meanings
	  readings
	  index)
      ;; several meanings in braces
      (while (re-search-forward "{\\([^}]*\\)}" end t)
	(setq meanings (cons (match-string 1) meanings)))
      (when meanings
	;; readings in katakana and hiragana
	(goto-char start)
	(while (re-search-forward " \\([^A-Z0-9{ ]+\\)" end t)
	  (setq readings (cons (match-string 1) readings)))
	;; index -- if enabled
	(when index-regexp
	  (goto-char start)
	  (when (re-search-forward index-regexp end t)
	    (setq index (string-to-number (match-string 1))))))
      (when (and key meanings)
	(list key
	      (nreverse meanings) 
	      (nreverse readings)
	      index)))))

(defun flashmaker-parse-kanjidic-entry-string (str &optional regexp)
  "Parse STR for testing."
  (with-temp-buffer
    (insert str)
    (beginning-of-line)
    (flashmaker-parse-kanjidic-entry regexp)))

(assert (and (equal (flashmaker-parse-kanjidic-entry-string
		     "鉤 6e6c U9264 N4841 B167 S13 I8a5.17 P1-8-5 O1939 Wgu Ygou1 XJ03343 V6250 Q8712.0 MN40319 MP11.0518 コウ ク かぎ {hook} {barb} {gaff} {[brackets]}")
		    '("鉤" ("hook" "barb" "gaff" "[brackets]") ("コウ" "ク" "かぎ") nil))
	     (equal (flashmaker-parse-kanjidic-entry-string
		     "腕 4f53 U8155 N3786 B130 S12 G8 H1006 F1076 P1-4-8 L1418 K1114 I4b8.6 Wwan Ywan4 IN1299 V4837 DR3956 DK687 DO1180 Q7321.2 MN29631 MP9.0337 E1945 ワン うで {arm} {ability} {talent} " " IN\\([0-9]+\\) ")
		    '("腕" ("arm" "ability" "talent") ("ワン" "うで") 1299))))

(when nil
  (setq flashmaker-list (flashmaker-parse "kanjidic" 'flashmaker-parse-kanjidic-entry))
  (flashmaker-key (car flashmaker-list))
  ;; Note that many kanji have no meanings!
  (length flashmaker-list))

;;; EDICT parsing

(defun flashmaker-parse-edict-entry ()
  "Parse current line of edict dictionaries.
Extra meanings are ignored"
  (when (looking-at "\\(\\w+\\) +\\(\\[\\(.*\\)\\] +\\)?/\\(.*\\)/")
    (let* ((key (match-string 1))
	   (start (point))
	   (end (line-end-position))
	   (readings (and (match-string 3) (list (match-string 3))))
	   meanings)
      ;; meanings separated by "/" + replace "_" with " "
      (dolist (meaning (mapcar (lambda (s)
				 (mapconcat 'identity
					    (split-string s "_") " "))
			       (split-string (match-string 4) "/")))
	(when (string-match "^\\(([^ \t]+)\\s-*\\)+\\(.*\\)" meaning)
	  (setq meaning (match-string 2 meaning)))
	(when (not (string= "" meaning))
	  (setq meanings (cons meaning meanings))))
      (when (and key meanings)
	(list key
	      (nreverse meanings)
	      readings
	      nil)))))

(defun flashmaker-parse-edict-entry-string (str)
  "Parse STR for testing."
  (with-temp-buffer
    (insert str)
    (beginning-of-line)
    (flashmaker-parse-edict-entry)))

(assert (and (equal (flashmaker-parse-edict-entry-string
		     ;;; one meaning
		     "いけいけ /bitch/")
		    '("いけいけ" ("bitch") nil nil))
	     (equal (flashmaker-parse-edict-entry-string
		     ;; several meanings plus parens
		     "いくつもの /many/a (great) number of/")
		    '("いくつもの" ("many" "a (great) number of") nil nil))
	     (equal (flashmaker-parse-edict-entry-string
		     "いく /(v5k-s) (X) (col) to come/to orgasm/")
		    ;; several extras
		    '("いく" ("to come" "to orgasm") nil nil))
	     (equal (flashmaker-parse-edict-entry-string
		     "いじいじ /(adv,n) reserved/timid/servile/unable to be honest/")
		    ;; comma separated extras
		    '("いじいじ" ("reserved" "timid" "servile" "unable to be honest") nil nil))
	     (equal (flashmaker-parse-edict-entry-string
		     "あれ /(int,n) (1) that/that thing/(2) (X) (col) genitals/(3) menses/(P)/")
		    ;; unumerated meanings, common use
		    '("あれ" ("that" "that thing" "genitals" "menses") nil nil))))

(when nil
  (setq flashmaker-list (flashmaker-parse "/usr/share/edict/edict" 'flashmaker-parse-edict-entry))
  (flashmaker-key (car flashmaker-list))
  ;; Note that a few entries do not consist of word syntax characters
  (length flashmaker-list))

;;; Sorting the parsed dictionary

(defun flashmaker-index-sort (a b)
  "Predicate to sort two entries A and B by index as returned by `flashmaker-index'.
Return t if the first element is less than the second."
  (let ((x (flashmaker-index a))
	(y (flashmaker-index b)))
    (or (and x (not y))
	(and x y (< x y)))))

;;; Variables for the wizards

(defvar flashmaker-wizard-kanjidic nil)
(defvar flashmaker-wizard-edict nil)
(defvar flashmaker-wizard-index-regexp nil)
(defvar flashmaker-wizard-file nil)
(defvar flashmaker-wizard-limit nil)

;;; EDICT filtering

(defun flashmaker-edict-filter ()
  "Interactively filter a word list using a second dictionary.
Use this to sort a dictionary in EDICT format according to information
gleaned from a second dictionary in KANJIDIC format.  One example is to
extract all kanji from the KANJIDIC with grade 1 and filter all words
from the EDICT with only grade 1 kanjis.  The information required is
entered interactively in a separate buffer.

Note that due to the stripping away of information from the EDICT file,
entries that differ in reading but not in kanji and meanings will appear
as duplicates.  No attempt is made to prevent these -- pass the result
through the uniq command-line utility to remove them.  There are also
entries with the same kanji and slightly different meanings depending
on the reading.  These are not merged."
  (interactive)
  (switch-to-buffer "*Flashmaker EDICT filter*")
  (kill-all-local-variables)
  (make-local-variable 'flashmaker-wizard-kanjidic)
  (make-local-variable 'flashmaker-wizard-edict)
  (make-local-variable 'flashmaker-wizard-index-regexp)
  (make-local-variable 'flashmaker-wizard-limit)
  (make-local-variable 'flashmaker-wizard-file)
  ;; (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (widget-insert
     "Welcome to the EDICT Filter Wizard\n"
     "\n"
     "Use M-ESC to complete filenames.\n"
     "\n"
     "The KANJIDIC will be used to create a mapping table.\n"
     "\n")
    (setq flashmaker-wizard-kanjidic
	  (widget-create 'file :must-match t "/usr/share/edict/kandidoc"))
    (widget-insert
     "\n"
     "Every kanji from the KANJIDIC will be mapped to an index.  The index\n"
     "Is determined from the KANJIDIC file.  Please choose one of the\n"
     "following indexes.\n"
     "\n")
    (setq flashmaker-wizard-index-regexp
	  (widget-create
	   'menu-choice
	   :value " G\\([0-9]+\\) "
	   :tag "Index to use"
	   '(const :tag "Jouyou grade level" " G\\([0-9]+\\) ")
	   '(const :tag "Frequency count" " F\\([0-9]+\\) ")
	   '(const :tag "Stroke count" " S\\([0-9]+\\) ")
	   '(const :tag "Radical number (Bushu)" " B\\([0-9]+\\) ")
	   '(const :tag "Spahn & Hadamitzky index" " IN\\([0-9]+\\) ")
	   '(const :tag "Henshall index" " E\\([0-9]+\\) ")
	   '(const :tag "Gakken Kanji Dictionary index" " K\\([0-9]+\\) ")
	   '(const :tag "Heisig index" " L\\([0-9]+\\) ")
	   '(const :tag "O'Neill index" " O\\([0-9]+\\) ")
	   '(const :tag "New Nelson index" " V\\([0-9]+\\) ")
	   '(const :tag "Nelson index" " N\\([0-9]+\\) ")

	   '(const :tag "Halpern index" " H\\([0-9]+\\) ")
	   'regexp))
    (widget-insert
     "\n"
     "If you want, you can limit the entries used by providing a max.\n"
     "index.  A good value might be 50, 100, 200, or 500 -- depending on\n"
     "how good you are.  Note that some indexes are very special.  The\n"
     "Jouyou grade level, for example, only has levels 1 to 8, so if you\n"
     "choose the Jouyou grade level above, you should provide a very small\n"
     "limit here, eg. 1, 2, or 3.\n"
     "\n")
    (setq flashmaker-wizard-limit
	  (widget-create 'number 2))
    (widget-insert
     "\n"
     "The EDICT file contains translations for a string.  The table\n"
     "derived from the KANJIDIC will be used to assign a score to every\n"
     "entry in the EDICT file.  In fact, every kanji in the entry will\n"
     "be looked up in the table; the highest index found will be used for\n"
     "the entire entry.  This allows to sort the entries by the most\n"
     "difficult or the least frequent kanji in the entry.\n"
     "\n")
    (setq flashmaker-wizard-edict
	  (widget-create 'file :must-match t "/usr/share/edict/edict"))
    (widget-insert
     "\n"
     "The resulting flashcard deck will contain all the entries of the\n"
     "EDICT file, sorted by the index given above.  It will be very big.\n"
     "Please give the name of the flashcard deck file to write.\n"
     "\n")
    (setq flashmaker-wizard-file
	  (widget-create 'file "~/sorted-edict-flashcard"))
    (widget-insert
     "\n"
     "The entire process will take a long time.  If you want to abort\n"
     "now, just kill the buffer.  Should Emacs ask you which coding system\n"
     "to use when saving the file, and you have no preferences, then choose\n"
     "iso-2022-jp.\n"
     "\n")
    (widget-create
     'push-button
     :notify (lambda (&rest ignore)
	       (flashmaker-edict-do-filter
		(widget-value flashmaker-wizard-kanjidic)
		(widget-value flashmaker-wizard-edict)
		(widget-value flashmaker-wizard-index-regexp)
		(widget-value flashmaker-wizard-limit)
		(widget-value flashmaker-wizard-file)))
     "Finish"))
  (use-local-map widget-keymap)
  (widget-setup))

(defvar flashmaker-edict-filter-table nil
  "This is an alist of elements (KEY . VALUE).
KEY is is a character and VALUE the corresponding index.
Used by `flashmaker-edict-do-filter'.")

(defconst flashmaker-max-index 99999
  "Max. index for `flashmaker-edict-filter-index'.")

(defun flashmaker-edict-filter-index (str)
  "Return the highest index for STR according to `flashmaker-edict-filter-table'."
  (let ((val (apply 'max
		    (mapcar
		     (lambda (c)
		       (or (cdr (assq c flashmaker-edict-filter-table))
			   flashmaker-max-index))
		     (string-to-list str)))))
    val))

(defun flashmaker-edict-do-filter (kanjidoc edict index-regexp limit file)
  "Does the work for `flashmaker-edict-filter'."
  (when (string= kanjidoc edict)
    (error "The KANJIDOC and the EDICT file are the same"))
  ;; Parse the kanjidic without filtering
  (let ((list (flashmaker-parse kanjidoc
				'flashmaker-parse-kanjidic-entry
				(list index-regexp))))
    (message "Limitting the kanjis used...")
    (when limit
      (let (result)
	(dolist (entry list)
	  (when (and (flashmaker-index entry)
		     (<= (flashmaker-index entry) limit))
	    (setq result (cons entry result))))
	(setq list result)))
    (message "Creating mapping table...")
    (setq flashmaker-edict-filter-table
	  (mapcar (lambda (entry)
		    (cons (string-to-char (flashmaker-key entry))
			  (flashmaker-index entry)))
		  list))
    ;; Parse edict without filtering
    (setq list (flashmaker-parse edict
				 'flashmaker-parse-edict-entry))
    (message "Replacing index...")
    (let (result index)
      (dolist (entry list)
	(setq index (flashmaker-edict-filter-index (flashmaker-key entry)))
	(when (< index flashmaker-max-index)
	  (flashmaker-set-index entry index)
	  (setq result (cons entry result))))
      (setq list (nreverse result)))
    (message "Sorting list...")
    (setq list (sort list 'flashmaker-index-sort))
    (message "Saving %s..." file)
    (flashmaker-save file list))
  (message "Creating filter flashcard deck...done"))

;;; KANJIDIC filtering

(defun flashmaker-kanjidic-filter ()
  "Interactively filter a dictionary in the KANJIDIC format.
This creates a new flashcard deck with a subset of the entries in the
KANJIDIC file."
  (interactive)
  (switch-to-buffer "*Flashmaker KANJIDIC filter*")
  (kill-all-local-variables)
  (make-local-variable 'flashmaker-wizard-kanjidic)
  (make-local-variable 'flashmaker-wizard-index-regexp)
  (make-local-variable 'flashmaker-wizard-file)
  (make-local-variable 'flashmaker-wizard-limit)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (widget-insert
     "Welcome to the KANJIDIC Filter Wizard\n"
     "\n"
     "Use M-ESC to complete filenames.\n"
     "\n"
     "The KANJIDIC will be read and a subset of its entries will be used\n"
     "for the flashcard deck.\n"
     "\n")
    (setq flashmaker-wizard-kanjidic
	  (widget-create 'file :must-match t "/usr/share/edict/kandidoc"))
    (widget-insert
     "\n"
     "The entries in the KANJIDIC file can be sorted according to an index.\n"
     "This is because the entries already contain the index positions for a\n"
     "variety of dictionaries.  Choose one of the indexes below, by which\n"
     "the entries should be sorted.\n"
     "\n")
    (setq flashmaker-wizard-index-regexp
	  (widget-create
	   'menu-choice
	   :value " G\\([0-9]+\\) "
	   :tag "Index regexp"
	   '(const :tag "No index" nil)
	   '(const :tag "Jouyou grade level" " G\\([0-9]+\\) ")
	   '(const :tag "Frequency count" " F\\([0-9]+\\) ")
	   '(const :tag "Stroke count" " S\\([0-9]+\\) ")
	   '(const :tag "Radical number (Bushu)" " B\\([0-9]+\\) ")
	   '(const :tag "Spahn & Hadamitzky index" " IN\\([0-9]+\\) ")
	   '(const :tag "Henshall index" " E\\([0-9]+\\) ")
	   '(const :tag "Gakken Kanji Dictionary index" " K\\([0-9]+\\) ")
	   '(const :tag "Heisig index" " L\\([0-9]+\\) ")
	   '(const :tag "O'Neill index" " O\\([0-9]+\\) ")
	   '(const :tag "New Nelson index" " V\\([0-9]+\\) ")
	   '(const :tag "Nelson index" " N\\([0-9]+\\) ")
	   '(const :tag "Halpern index" " H\\([0-9]+\\) ")
	   'regexp))
    (widget-insert
     "\n"
     "If you want, you can limit the entries written by providing a max.\n"
     "index.  A good value might be 50, 100, 200, or 500 -- depending on\n"
     "how good you are.  Note that some indexes are very special.  The\n"
     "Jouyou grade level, for example, only has levels 1 to 8, so if you\n"
     "choose the Jouyou grade level above, you should provide a very small\n"
     "limit here, eg. 1, 2, or 3.\n"
     "\n")
    (setq flashmaker-wizard-limit
	  (widget-create 'number 2))
    (widget-insert
     "\n"
     "The resulting flashcard deck will contain all the entries of the\n"
     "KANJIDIC file, sorted by the index given above.  It will be big.\n"
     "Please give the name of the flashcard deck file to write.\n"
     "\n")
    (setq flashmaker-wizard-file
	  (widget-create 'file "~/sorted-kanjidic-flashcard"))
    (widget-insert
     "\n"
     "The entire process will take a a few seconds.  If you want to abort\n"
     "now, just kill the buffer.  Should Emacs ask you which coding system\n"
     "to use when saving the file, and you have no preferences, then choose\n"
     "iso-2022-jp.\n"
     "\n")
    (widget-create
     'push-button
     :notify (lambda (&rest ignore)
	       (flashmaker-kanjidic-do-filter
		(widget-value flashmaker-wizard-kanjidic)
		(widget-value flashmaker-wizard-index-regexp)
		(widget-value flashmaker-wizard-limit)
		(widget-value flashmaker-wizard-file)))
     "Finish"))
  (use-local-map widget-keymap)
  (widget-setup))

(defun flashmaker-kanjidic-do-filter (kanjidoc index-regexp limit file)
  "Does the work for `flashmaker-edict-filter'."
  ;; Parse the kanjidic without filtering
  (let ((list (flashmaker-parse kanjidoc
				'flashmaker-parse-kanjidic-entry
				(list index-regexp))))
    (message "Sorting list...")
    (when limit
      (let (result)
	(dolist (entry list)
	  (when (and (flashmaker-index entry)
		     (<= (flashmaker-index entry) limit))
	    (setq result (cons entry result))))
	(setq list result)))
    (setq list (sort list 'flashmaker-index-sort))
    (message "Saving %s..." file)
    (flashmaker-save file list))
  (message "Creating filter flashcard deck...done"))

;;; Exporting a flashcard file

(defun flashmaker-save (file list &optional reverse)
  "Export LIST to FILE.
When called with the optional argument REVERSE, or using
a prefix argument, the entries are written in reverse."
  (setq reverse (or reverse current-prefix-arg))
  (setq test list)
  (with-temp-buffer
    ;; using dolist in here makes this undebuggable using edebug.
    (let ((entries list)
	  entry)
      (while entries
	(setq entry (car entries)
	      entries (cdr entries))
	(if reverse
	    (insert (format "%s : %s\n"
			    (mapconcat 'identity
				       (flashmaker-meanings entry)
				       ", ")
			    (flashmaker-key entry)))
	  (insert (format "%s : %s\n"
			  (flashmaker-key entry)
			  (mapconcat 'identity
				     (flashmaker-meanings entry)
				     ", "))))))
    (write-file file)))

(provide 'flashmaker)

;;; flashmaker.el ends here
