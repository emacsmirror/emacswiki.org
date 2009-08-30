;;; kdic.el -- Practice your Kanji using a dictionary.

;; Copyright (C) 2001, 2002  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.7.1
;; Keywords: i18n
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?KanjiDictionary

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

;; Use M-x kdic to start a multiple choice practice session.  The data
;; is derived from a dictionary.  The dictionary is not included.  You
;; will have to get it yourself.  Any file in the kanjidic or edict
;; file format will do.

;; In debian, use "apt-get install kanjidic" for a large dictionary of
;; Japanese kanji including a document explaining the format of the
;; dictionary file.  Set `kdic-dictionary' and
;; `kdic-encoding'accordingly.

;; Inspired by kdrill, a drill program for kanji chars under X writte by
;; Philip Brown <phil@bolthole.com>.  The kdrill homepage can be found
;; here: http://www.bolthole.com/kdrill/.

;; Installation: Copy kdic.el into a directory on your load-path, and
;; add the following line to your ~/.emacs file:
;; (autoload 'kdic "kdic" "Practice kanji and vocabulary." t)

;; You can select subsets of the `kdic-dictionary' by customizing the
;; variables `kdic-filter' and `kdic-index-regexp'.  Here are two
;; examples of how to set it up.  First use M-x custom-group RET kdic
;; RET to customize the appropriate group, and then set the variables
;; as suggested:

;; Example 1:

;; Set kdic-index-regexp to "Jouyou grade level" and set kdic-filter
;; to "Distinct values" and choose the value "1".  This will drill you
;; on the 80 easiest kanji -- the ones having a jouyou grade level of
;; 1.

;; Example 2:

;; Set kdic-index-regexp to "Spahn & Hadamitzky index" and set
;; kdic-filter to "Grouping", using sorting function
;; "kdic-index-sort", group size 50, and group number 1.  This will
;; drill you on the first 50 kanji from the Spahn & Hadamitzky index
;; (which happens to be the book I use).

;; When you are happy with the selection, set `kdic-cache' to a
;; filename to use as a cache.  This makes parsing and filtering of
;; the original dictionary unnecessary, speeding things up when
;; starting.  Just delete the file when you want to use another
;; dictionary, or when you change the filter.

;; Advanced usage:

;; Filtering wizard: `kdic-word-filter' is a wizard that will help you
;; combine two dictionaries.  This allows you to sort the kanji in the
;; kanjidic file as described above, and then sort the entries in the
;; edict file according to the kanji with the highest number.  Thus
;; you can produce a list of words to learn that only require the
;; simplest kanji!  Example:

;; Sort the kanji in the kanjidic file according to the "Jouyou grade
;; level", and choose group number 1 of size 100.  This will sort the
;; edict files such that words with only Jouyou grade level 1 come
;; first.  From these, the first 100 words will be used.

;; Flashcard export:

;; Flashcard.el by Jorgen Schaefer offers a doctor interface instead
;; of a multiple choice test.  You can export the current vocabulary
;; into a flashcard file using `kdic-to-flashcard'.

;;; Code

(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(defgroup kdic nil
  "Kanji Dictionary is a package to help you train Kanji or other
characters.  The main point of using kdic is to use an existing
dictionary.")

(defcustom kdic-dictionary "/usr/share/edict/kanjidic.gz"
  "*Dictionary in a recognized format.
Dictionaries are available here:
ftp://ftp.cc.monash.edu.au/pub/nihongo/00INDEX.htm
If the dictionary file is compressed, make sure `auto-compression-mode'
is on.  See http://www.emacswiki.org/cgi-bin/wiki.pl?KanjiDictionary
for a list of supported dictionaries.  See `kdic-parse-entry' for the
parsing code.

If you are practicing, I recommend the KANJIDIC file.  It contains lots
of extra information for each kanji, such that you sort them by
difficulty or frequency.  See variable `kdic-filter' for more information.

If you are fluent, I recommend the EDICT file.  It has a huge word-list.
Based on KANJIDIC you can sort the words in an EDICT file by difficulty
as shown on the kdic.el homepage:
http://www.emacswiki.org/cgi-bin/wiki.pl?KanjiDictionary

Be sure to run `kdic-setup' with a prefix argument after setting this
variable."
  :type 'file
  :group 'kdic)

(defcustom kdic-cache nil
  "If nil, don not cache the parsed and filtered dictionary.
If a filename, try to load it.  Only if loading it fails will
`kdic-dictionary' be parsed and filtered.  The result will be saved in
the cache.

In order to ignore the cache, set this variable to nil.  Be sure to run
`kdic-setup' with a prefix argument after setting this variable."
  :type '(choice (const :tag "No cache" nil)
		 file)
  :group 'kdic)

(defcustom kdic-encoding 'euc-jp
  "Character encoding used for kdic files.
The files are `kdic-dictionary',`kdic-cache', and `kdic-missed-file'."
  :type '(restricted-sexp :match-alternatives (coding-system-p))
  :group 'kdic)

(defcustom kdic-filter nil
  "Filter to use after parsing the file.

If nil, no filtering will be performed.  Note that if you filter by
information that the current dictionary does not provide, all entries
will be filtered away.  Be sure to reset this variable to nil if you
want to disable filtering.

If a list of numbers, then only entries with an index in that list will
be retained.  The index of an entry is determined using
`kdic-index-regexp'.

If you are using the Jouyou grades as an index, for example, 1 through 6
indicate Jouyou grades 1-6.  8 indicates general-use characters.  9
indicates Jinmeiyou \(\"for use in names\") characters.  If an element
in the list is nil, it will match kanjis with an index of nil.

Here's how many kanji there are on the first levels if you are using the
kanjidic file as recommended in the `kdic-dictionary' doc-string.

Level 1:  80
Level 2: 160
Level 3: 200
Level 4: 200

If a list containing a function and two numbers (FUNC SIZE NUMBER), then
FUNC will be used to sort the entries, SIZE indicates the number of
entries per group, and NUMBER indicates the group that will be used.

One possible value could therefore be '(kdic-index-sort 50 1) to use the
first 50 kanji.  Depending on `kdic-index-regexp' those will be the most
frequent, the first in the Hadamitzky book, or whatever.

Be sure to run `kdic-setup' with a prefix argument after setting this
variable."
  :type '(choice 
	  (const :tag "No filtering" nil)
	  (repeat :tag "Distinct values" 
		  :value (1)
		  integer)
	  (list :tag "Grouping"
		:value (kdic-index-sort 50 1)
		(function :tag "Sorting function")
		(integer :tag "Group size")
		(integer :tag "Group number")))
  :group 'kdic)

(defcustom kdic-index-regexp " G\\([0-9]+\\) "
  "Regular expression used to determine the index of an entry.
The index can be used to sort the entries.  The first subgroup
of the regexp will be converted to a number and stored as the
index.  Specify `kdic-index-sort' as the sorting function for
variable `kdic-filter', which see.  If nil, the index will not
be computed."
  :type '(choice (const :tag "Do not compute index" nil)
		 (const :tag "Jouyou grade level" " G\\([0-9]+\\) ")
		 (const :tag "Frequency count" " F\\([0-9]+\\) ")
		 (const :tag "Stroke count" " S\\([0-9]+\\) ")
		 (const :tag "Radical number (Bushu)" " B\\([0-9]+\\) ")
		 (const :tag "Spahn & Hadamitzky index" " IN\\([0-9]+\\) ")
		 (const :tag "Henshall index" " E\\([0-9]+\\) ")
		 (const :tag "Gakken Kanji Dictionary index" " K\\([0-9]+\\) ")
		 (const :tag "Heisig index" " L\\([0-9]+\\) ")
		 (const :tag "O'Neill index" " O\\([0-9]+\\) ")
		 (const :tag "New Nelson index" " V\\([0-9]+\\) ")
		 (const :tag "Nelson index" " N\\([0-9]+\\) ")
		 (const :tag "Halpern index" " H\\([0-9]+\\) ")
		 regexp)
  :group 'kdic)

(defcustom kdic-missed-file "~/kdic.log"
  "File that stores the list of kanji missed.
The file saves one key per line.
The file will be read using `kdic-encoding'."
  :type 'file
  :group 'kdic)

(defcustom kdic-missed-ratio 25
  "*Ratio of missed vs. other kanjis in percent.
This indicates the percent chance of using a kanji from the missed list,
`kdic-missed-list'.  A value of 100 means that only the kanji in the
missed list will be used.  A value of 0 means that the kanji in the
missed list will not appear more often than other kanji.

When you are missing a lot of kanji, it might make sense to increase
this value.  If that doesn't help, however, you might be better of
choosing a smaller group using variable `kdic-filter'."
  :type 'integer
  :group 'kdic)

(defcustom kdic-number-of-answers 5
  "*The number of answers offered per question."
  :type 'integer
  :group 'kdic)

(defcustom kdic-ask-for 'meaning
  "*Indicates the kind of questions asked.
One of:
  meaning -- the kanji is presented and the meaning is asked for
  kanji -- the meaning is presented and the kanji is asked for
  reading -- the kanji is presented and the reading is asked for"
  :type '(choice (const :tag "Ask for the meaning of a kanji" meaning)
		 (const :tag "Ask for the kanji matching some meanings" kanji)
		 (const :tag "Ask for the readings of a kanji" reading)
		 (const :tag "Switch between all of them, at random" random))
  :group 'kdic)

(defcustom kdic-ask-for-one-of nil
  "*When non-nil, the answers will contain only one reading or meaning.
If a certain word has several meanings, only one of the will be shown
as a possible answer.  This will make it more difficult."
  :type 'boolean
  :group 'kdic)

(defcustom kdic-hook nil
  "Hook to run as the kdic buffer is prepared.
Note that this hook will not be run after questions are inserted into
the buffer.  This hook will only run once at the start of the game."
  :type 'hook
  :options '(kdik-small-frame-big-font)
  :group 'kdic)

(defcustom kdic-correct-hook nil
  "Hook to run after a question is answered correctly.
`kdic-correct-answer' holds the correct answer.  Use the various
accessor functions to get to the information you need:
`kdic-key', `kdic-meanings', and `kdic-readings' are probably the
most important ones."
  :type 'hook
  :group 'kdic)

;;; Internal stuff

(defvar kdic-missed-list nil
  "List of entries missed.
Duplicates are allowed in the list.  Every entry will indicate one
miss.  Upon startup, the list will be read from `kdic-missed-file'.")

(defvar kdic-list nil
  "The parsed and filtered dictionary.")

(defvar kdic-current-question nil
  "Variable used for the current question.")

(defvar kdic-correct-answer nil
  "Variable used for the correct answer.")

(defvar kdic-answer-count nil
  "Variable used to enumerate the answers.")

;;; Possible values for the hook

(defun kdik-small-frame-big-font ()
  "Create a new, small frame with a big font.
Idea if you usually work using a small font such as 7x14 but
when it comes to kanji you'd like to use 24x24."
  (let ((fontset (create-fontset-from-fontset-spec
		  (concat "-adobe-courier-*-r-*-*-40-*-*-*-*-*-*-*,"
			  "japanese-jisx0212:-*-*-*-*-*-*-40-*-*-*-*-*-jisx0212.1990-*")
		  nil 'noerror)))
    (select-frame (make-frame
		   `((width . 30)
		     (height . 15)
		     (font . ,fontset))))))

;;; Accessing dictionary entries

(defun kdic-key (entry)
  "Return the key of ENTRY.
This is a multi-byte string."
  (nth 0 entry))

(defun kdic-meanings (entry)
  "Return the meanings of ENTRY.
This is a string."
  (kdic-format (nth 1 entry)))

(defun kdic-readings (entry)
  "Return the readings of ENTRY.
This is a multi-byte string or nil."
  (kdic-format (nth 2 entry)))

(defun kdic-index (entry)
  "Return the index of ENTRY.
This is a number or nil.
See `kdic-index-regexp'."
  (nth 3 entry))

(defun kdic-set-index (entry val)
  "Set the index of ENTRY to VAL.
This must be a number or nil."
  (setcar (nthcdr 3 entry) val))

(defun kdic-entry (key)
  "Return the entry from `kdic-list' for KEY."
  (assoc key kdic-list))

(defun kdic-format (list)
  "Return a string for LIST.
If `kdic-ask-for-one-of' is non-nil, return a random element of LIST.
If `kdic-ask-for-one-of' is nil, return all elements of LIST
concatenated."
  (if kdic-ask-for-one-of
      (nth (random (length list)) list)
    (mapconcat (function identity) list " / ")))

;;; Parsing the dictionary

(defun kdic-parse ()
  "Parse `kdic-dictionary' and return it.
You can use function `kdic-filter' to filter the entries returned."
  (let ((coding-system-for-read kdic-encoding)
	result entry)
    ; (set-buffer (get-buffer-create "*kdic dictionary*"))
    ; (erase-buffer)
    (with-temp-buffer
      (insert-file-contents kdic-dictionary)
      (kdic-parse-entries (buffer-size)))))

(defun kdic-parse-entries (total)
  "Parse the lines of the current buffer."
  (let ((entry (kdic-parse-entry))
	result)
    (while (not (eobp))
      (when entry
	(setq result (cons entry result)))
      (setq entry (kdic-parse-entry))
      (forward-line)
      (message "Parsing...%d%%" (/ (* 100.0 (point)) total)))
    (message "Parsing...done")
    (nreverse result)))

(defun kdic-parse-entry ()
  "Parse current line of the dictionary."
  (when (looking-at "\\sw+")
    (let ((key (match-string 0))
	  (start (point))
	  (end (line-end-position))
	  meanings
	  readings
	  index)
      (if (looking-at "\\(.*\\) \\[\\(.*\\)\\] /\\(.*\\)/")
	  ;; EDICT format
	  (setq key (match-string 1)
		readings (list (match-string 2))
		;; meanings separated by "/" + replace "_" with " "
		meanings (mapcar
			  (lambda (s)
			    (mapconcat (function identity)
				       (split-string s "_") " "))
			  (split-string (match-string 3) "/")))
	;; KANJIDOC format
	;; several meanings in braces
	(while (re-search-forward "{\\([^}]*\\)}" end t)
	  (setq meanings (cons (match-string 1) meanings)))
	(when meanings
	  ;; readings in katakana and hiragana
	  (goto-char start)
	  (while (re-search-forward " \\([^A-Z0-9{ ]+\\)" end t)
	    (setq readings (cons (match-string 1) readings)))
	  ;; index -- if enabled
	  (when kdic-index-regexp
	    (goto-char start)
	    (when (re-search-forward kdic-index-regexp end t)
	      (setq index (string-to-number (match-string 1)))))))
      (when (and key meanings)
	(list key
	      (nreverse meanings) 
	      (nreverse readings)
	      index)))))

;;; Caching

(defun kdic-read-cache ()
  "Read the cache specified in `kdic-cache'."
  (when (and kdic-cache
	     (file-readable-p kdic-cache))
    (let ((coding-system-for-read kdic-encoding)
	  result)
      (with-temp-buffer
	(message "Reading cache...")
	(insert-file-contents kdic-cache)
	(goto-char (point-min))
	(setq result (read (current-buffer)))
	(message "Reading cache...done"))
      result)))

(defun kdic-write-cache ()
  "Write the cache specified in `kdic-cache'."
  (when (and kdic-list
	     kdic-cache
	     (file-writable-p kdic-cache))
    (message "Updating cache...")
    (with-temp-buffer
      (prin1 kdic-list (current-buffer))
      (let ((coding-system-for-write kdic-encoding))
	(write-region (point-min) (point-max) kdic-cache)))
    (message "Updating cache...done")))

;;; Filtering the parsed dictionary

(defun kdic-index-sort (a b)
  "Predicate to sort two entries A and B by index as returned by `kdic-index'."
  (let ((x (kdic-index a))
	(y (kdic-index b)))
    (or (and x (not y))
	(and x y (< x y)))))

(defun kdic-filter (dic)
  "Filter DIC according to variable `kdic-filter'."
  (message "Filtering...")
  (let (result)
    (cond ((null kdic-filter)
	   (setq result dic))
	  ((functionp (car kdic-filter))
	   (let ((func (nth 0 kdic-filter))
		 (size (nth 1 kdic-filter))
		 (num (nth 2 kdic-filter))
		 end)
	     (setq dic (sort dic func))
	     ;;find end of group, set tail to nil
	     (setq end (nthcdr (1- (* size num)) dic))
	     (setcdr end nil)
	     ;; skip until start of group
	     (setq result (nthcdr (* size (1- num)) dic))))
	  (t
	   ;; filter according to index, copying the list
	   (while dic
	     (when (memq (kdic-index (car dic)) kdic-filter)
	       (setq result (cons (car dic) result)))
	     (setq dic (cdr dic)))
	   (setq result (nreverse result))))
    (message "Filtering...done")
    result))

;;; Preparing a sorted word list based on kanji information

(defvar kdic-wizard-kanjidic nil)
(defvar kdic-wizard-edict nil)
(defvar kdic-wizard-index-regexp nil)
(defvar kdic-wizard-group-size nil)
(defvar kdic-wizard-group-number nil)

(defun kdic-word-filter ()
  "Interactively filter a word list using a second dictionary.
Use this to sort a dictionary in EDICT format according to information
gleaned from a second dictionary in KANJIDIC format.  One example is to
extract all kanji from the KANJIDIC with grade 1 and filter all words
from the EDICT with only grade 1 kanjis.  The information required is
entered interactively in a separate buffer."
  (interactive)
  (switch-to-buffer "*Kdic word filter*")
  (kill-all-local-variables)
  (make-local-variable 'kdic-wizard-kanjidic)
  (make-local-variable 'kdic-wizard-edict)
  (make-local-variable 'kdic-wizard-index-regexp)
  (make-local-variable 'kdic-wizard-group-size)
  (make-local-variable 'kdic-wizard-group-number)
  ;; (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (widget-insert
     "Welcome to the Word Filter Wizard\n"
     "\n"
     "The KANJIDIC will be used to create a mapping table."
     "\n")
    (setq kdic-wizard-kanjidic
	  (widget-create 'file :must-match t kdic-dictionary))
    (widget-insert
     "\n"
     "Every kanji from the KANJIDIC will be mapped to an index.  The index\n"
     "Is determined from the KANJIDIC file using a regular expression.  The\n"
     "regular expression specifies which part of the entry to use as index.\n"
     "Please choose one of the following regular expression.\n"
     "\n")
    (setq kdic-wizard-index-regexp
	  (apply 'widget-create
		 (append `(menu-choice
			   :value ,(or kdic-index-regexp " G\\([0-9]+\\) ")
			   :tag "Index regexp")
			 (cdr (get 'kdic-index-regexp 'custom-type)))))
    (widget-insert
     "\n"
     "The EDICT file contains translations for a string.  The table\n"
     "derived from the KANJIDIC will be used to assign a score to every\n"
     "entry in the EDICT file.  In fact, every kanji in the entry will\n"
     "be looked up in the table; the highest index found will be used for\n"
     "the entire entry.  This allows to sort the entries by the most\n"
     "difficult or the least frequent kanji in the entry.\n"
     "\n")
    (setq kdic-wizard-edict
	  (widget-create 'file :must-match t kdic-dictionary))
    (widget-insert
     "\n"
     "After sorting the EDICT file, it will be split into groups.  Please\n"
     "indicate how big these groups should be and which group you want to\n"
     "retain.\n"
     "\n"
     "If you wish to practive on the first 100 entries, for example, choose\n"
     "a group size of 100 and choose group number 1.\n"
     "\n")
    (setq kdic-wizard-group-size
	  (widget-create 'integer :tag "Group size" 100))
    (setq kdic-wizard-group-number
	  (widget-create 'integer :tag "Group number" 1))
    (widget-insert "\n")
    (widget-create
     'push-button
     :notify (lambda (&rest ignore)
	       (kdic-word-do-filter (widget-value kdic-wizard-kanjidic)
				    (widget-value kdic-wizard-edict)
				    (widget-value kdic-wizard-index-regexp)
				    (widget-value kdic-wizard-group-size)
				    (widget-value kdic-wizard-group-number)))
     "Finish"))
  (use-local-map widget-keymap)
  (widget-setup))

(defvar kdic-word-filter-table nil
  "This is an alist of elements (KEY . VALUE).
KEY is is a character and VALUE the corresponding index.")

(defun kdic-word-filter-index (str)
  "Return the highest index for STR according to a `kdic-word-filter-table'."
  (let ((val (apply 'max
		    (mapcar
		     (lambda (c)
		       (or (cdr (assq c kdic-word-filter-table)) 0))
		     (string-to-list str)))))
    (when (> val 0)
      val)))

(defun kdic-word-do-filter (kanjidoc edict index-regexp group-size group-number)
  "Does the work for `kdic-word-do-filter'."
  (when (string= kanjidoc edict)
    (error "The KANJIDOC and the EDICT file are the same"))
  (when (not kdic-cache)
    (error "Please set kdic-cache in order to save the result"))
  ;; Parse the kanjidic without filtering
  (let* ((kdic-dictionary kanjidoc)
	 (kdic-cache nil)
	 (kdic-filter nil)
	 (kdic-index-regexp index-regexp)
	 (klist (kdic-parse)))
    (message "Creating mapping table...")
    (setq kdic-word-filter-table
	  (mapcar (lambda (entry) 
		    (cons (string-to-char (kdic-key entry))
			  (or (kdic-index entry) 99999)))
		  klist)))
  ;; Parse edict without filtering
  (let* ((kdic-dictionary edict)
	 (kdic-cache nil)
	 (kdic-filter nil)
	 (kdic-index-regexp nil)
	 (klist (kdic-parse))
	 (entries klist)
	 entry)
    (message "Replacing index...")
    (setq test klist)
    (while entries
      (setq entry (car entries)
	    entries (cdr entries))
      (kdic-set-index entry (kdic-word-filter-index (kdic-key entry))))
    (setq kdic-filter `(kdic-index-sort ,group-size ,group-number)
	  kdic-list (kdic-filter klist)))
  (kdic-write-cache)
  (message "Creating word filter...done"))

;;; Exporting a flashcard file

(defun kdic-to-flashcard (file &optional reverse)
  "Export the current vocabulary to FILE.
When called with the optional argument REVERSE, or using
a prefix argument, the entries are written in reverse."
  (interactive "FFlashcard file: ")
  (kdic-setup)
  (setq reverse (or reverse current-prefix-arg))
  (with-temp-buffer
    (dolist (entry kdic-list)
      (if reverse
	  (insert (format "%s : %s\n" (kdic-meanings entry) (kdic-key entry)))
	(insert (format "%s : %s\n" (kdic-key entry) (kdic-meanings entry)))))
    (write-file file)))

;;; Missed characters

(defun kdic-read-missed ()
  "Read missed characters from `kdic-missed-file'.
The list is stored in `kdic-missed-list'."
  (when (file-readable-p kdic-missed-file)
    (let ((coding-system-for-read kdic-encoding)
	  result)
      (with-temp-buffer
	(insert-file-contents kdic-missed-file)
	;; remove missed key no longer found in the list
	(delq nil
	      (mapcar (function kdic-entry)
		      (split-string (buffer-string))))))))

(defun kdic-missed (entry)
  "Add ENTRY to the list of missed entries, `kdic-missed-list'."
  (setq kdic-missed-list (cons entry kdic-missed-list)))

(defun kdic-knew (entry)
  "Remove ENTRY from the list of missed entries, `kdic-missed-list'."
  (let ((l kdic-missed-list))
    (if (eq entry (car l))
	(setq kdic-missed-list (cdr kdic-missed-list))
      (while l
	(if (not (eq entry (car (cdr l))))
	    (setq l (cdr l))
	  ;; modify kdic-missed-list in place, end loop.
	  (setcdr l (cdr (cdr l)))
	  (setq l nil)))
      kdic-missed-list)))

(defun kdic-save-missed ()
  "Save the missed list."
  (with-temp-buffer
      (unless (null kdic-missed-list)
	(insert (mapconcat (function kdic-key)
			   kdic-missed-list
			   "\n")))
      (let ((coding-system-for-write kdic-encoding))
	(write-region (point-min) (point-max) kdic-missed-file))))

;;; Preparing a question

(defun kdic-get-question ()
  "Return a list of entries from `kdic-list'.
The first entry is the one to be asked for, the others are to be
presented as wrong alternatives.  At this point all entries are
equivalent -- they all contain kanji and meanings.  Wether to ask
for kanji or meaning is irrelevant at this point.  The important
point is wether the correct answer is from `kdic-missed-list' or
from `kdic-list'."
  (let ((l (length kdic-list))
	answers)
    (when (< l kdic-number-of-answers)
      (error "Not enough entries in kdic-list"))
    (setq answers (list
		   (if (and (not (null kdic-missed-list))
			    (< (random 100) kdic-missed-ratio))
		       ;; a random question from the missed list
		       (nth (random (length kdic-missed-list)) kdic-missed-list)
		     ;; a random question from the full list
		     (nth (random l) kdic-list))))
    (while (< (length answers) kdic-number-of-answers)
      (let ((entry (nth (random l) kdic-list)))
	(when (not (memq entry answers))
	  (setq answers (cons entry answers)))))
    (nreverse answers)))

;;; Ask a question

(defvar kdic-window-config nil
  "The window configuration once the quiz has started.
If it is not restored, then creating a new window such as
the Help window will be irreversible -- even if you close
it, it will reappear when the next question is asked.")

(defun kdic-question-setup ()
  "Insert the question into the current buffer."
  (setq kdic-current-question (kdic-get-question))
  (setq kdic-correct-answer (car kdic-current-question))
  ;; jumble answers by sorting with random predicate
  (setq kdic-current-question
	(sort kdic-current-question
	      (lambda (a b)
		(= 1 (random 2)))))
  (kdic-buffer-setup)
  (setq kdic-window-config (current-window-configuration)))

(defun kdic-buffer-setup ()
  "Erase buffer and fill with question and possible answers."
  (when (window-configuration-p kdic-window-config)
    (set-window-configuration kdic-window-config))
  (erase-buffer)
  (let ((kdic-ask-for kdic-ask-for));; protect the real setting
    (when (eq kdic-ask-for 'random);; set temporarily if random
      (setq kdic-ask-for (nth (random 3) '(meaning kanji reading))))
    (kdic-print-question kdic-correct-answer)
    (setq kdic-answer-count 1)
    (mapcar (function kdic-print-answer)
	    kdic-current-question)))

(defun kdic-print-question (entry)
  "Print ENTRY as a question."
  (insert "Missed: " (number-to-string (length kdic-missed-list)) "\n")
  (if (memq entry kdic-missed-list)
      (insert "You have been asked this question before.\n\n")
    (insert "A new question.\n\n"))
  (insert "What is the correct answer for: ")
  (cond ((eq kdic-ask-for 'meaning)
	 (insert (kdic-key entry)))
	((eq kdic-ask-for 'reading)
	 (insert (kdic-key entry)))
	(t
	 (insert (kdic-meanings entry))))
  (insert "\n\n"))

(defun kdic-print-answer (entry)
  "Print ENTRY as an answer."
  (insert (number-to-string kdic-answer-count) ". ")
  (setq kdic-answer-count (1+ kdic-answer-count))
  (cond ((eq kdic-ask-for 'meaning)
	 (insert (kdic-meanings entry)))
	((eq kdic-ask-for 'reading)
	 (insert (kdic-readings entry)))
	(t
	 (insert (kdic-key entry))))
  (insert "\n"))

(defun kdic-process-answer (num)
  "Check wether number NUM is the correct answer.
Process the missed list accordingly.  If you got an answer wrong that
counts as not knowing the answer to the question twice, and not knowing
what the answer actually means: three misses.  Therefore both are added
to the missed list, `kdic-missed-list'.  Return non-nil if the question
was answered correctly."
  (let ((answer (nth (1- num) kdic-current-question)))
    (if (eq answer kdic-correct-answer)
	(progn
	  (kdic-knew answer)
	  (save-excursion
	    (save-restriction
	      (run-hooks 'kdic-correct-hook)))
	  t)
      (kdic-missed kdic-correct-answer)
      (kdic-missed kdic-correct-answer)
      (kdic-missed answer)
      nil)))

(defun kdic-after (option options)
  "Return the next value after OPTION in the list OPTIONS."
  (let ((result  (cadr (memq option options))))
    (or result (car options))))

(defun kdic-quiz ()
  "Ask one question until answered."
  (kdic-question-setup)
  (let (done quit)
    (while (not done)
      (let ((answer 
	     (if (< kdic-number-of-answers 10)
		 (read-key-sequence
		  (format "Your answer [1-%sqrdDmn?]: "
			  kdic-number-of-answers))
	       (read-from-minibuffer
		"Your answer or one of [qrdDmn?]: "))))
	(cond ((or (string= answer "?")
		   (string= answer "h"))
	       (kdic-help))
	      ((string= answer "q")
	       (setq done t quit t))
	      ((string= answer "r")
	       (setq kdic-missed-ratio
		     (string-to-number
		      (read-from-minibuffer
		       "Percent change for a missed one: "
		       (number-to-string kdic-missed-ratio)))))
	      ((string= answer "d")
	       (setq kdic-ask-for
		     (kdic-after kdic-ask-for '(meaning kanji reading)))
	       ;; redisplay
	       (kdic-buffer-setup))
	      ((string= answer "D")
	       (setq kdic-ask-for 'random)
	       ;; redisplay
	       (kdic-buffer-setup))
	      ((string= answer "m")
	       (setq kdic-ask-for-one-of (not kdic-ask-for-one-of))
	       ;; redisplay
	       (kdic-buffer-setup))
	      ((string= answer "n")
	       (setq kdic-number-of-answers
		     (string-to-number
		      (read-from-minibuffer
		       "Number of answers given: "
		       (number-to-string kdic-number-of-answers)))))
	      (t
	       (setq done (kdic-process-answer (string-to-number answer)))
	       (if done
		   (message "Correct!")
		 (message "Wrong answer")
		 ;; redisplay (number of wrong misses has changed)
		 (kdic-buffer-setup))
	       (sit-for 1)))))
    (not quit)))

(defun kdic-setup (&optional force)
  "Setup function.
Setup is skipped if the files have already been parsed.
Use the optional FORCE argument to re-parse all files.
If possible, the cache is read using `kdic-read-cache'.
If necessary, the cache is updated using `kdic-write-cache'."
  (interactive "P")
  (when (and (not force)
	     (null kdic-list))
    (setq kdic-list (kdic-read-cache)))
  (when (or force (null kdic-list))
    (setq kdic-list (kdic-filter (kdic-parse)))
    (kdic-write-cache))
  (unless kdic-list
    (error "kdic-list is empty, check variable kdic-filter or the cache"))
  (when (or force (null kdic-missed-list))
    (setq kdic-missed-list (kdic-read-missed))))

(defun kdic-quit ()
  "Quit the game.
Save the `kdic-missed-list'."
  (kdic-save-missed))

;;; Main Entry Point

(defun kdic-help ()
  "`kdic' is giving you a multiple choice test.
From the list of answers, choose the correct one by typing its
number and hitting RET.  Instead of typing a number, the following
commands are available as well:

q - quit (this will save the list of missed questions)
r - set ratio of missed questions (an integer between 0 and 100)
d - change display, cycling through the available options
D - change display at random
m - toggle the display of multiple meanings or readings
n - set number of possible answers (a integer larger than 0)"
  (describe-function 'kdic-help))

(defun kdic ()
  "Start to practice some kanji."
  (interactive)
  (let ((buf (get-buffer-create "*kdic*")))
    (switch-to-buffer buf)
    (run-hooks 'kdic-hook)
    (kdic-setup)
    (while (kdic-quiz)
      (erase-buffer))
    (kdic-quit)))
    
(provide 'kdic)

;;; kdic.el ends here
