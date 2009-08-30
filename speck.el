;;; speck.el --- minor mode for spell checking

;; Copyright (C) 2006, 2007, 2008 Martin Rudalics

;; Time-stamp: "2008-07-26 10:08:25 martin"
;; Author: Martin Rudalics <rudalics@gmx.at>
;; Keywords: spell checking

;; This program is free software: you can redistribute it and/or modify
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

;; Speck is a minor mode for "specking" - spell-checking text displayed
;; in Emacs windows.  Invoke the command `speck-mode' to toggle specking
;; of all windows showing the current buffer.

;; _____________________________________________________________________________
;;                                                                              
;;;			       General options					
;; _____________________________________________________________________________
;;                                                                              
(defgroup speck nil
  "Another interface to Aspell and Ispell."
  :version "23.1"
  :group 'applications)

(defcustom speck-aspell-program
  (locate-file "aspell" exec-path exec-suffixes 'file-executable-p)
  "File name of Aspell program."
  :type '(choice (const :tag "Invalid" nil) (file :tag "File"))
  :group 'speck-aspell)

(defsubst speck-aspell-executable-p ()
  "Return non-nil when `speck-aspell-program' appears executable."
  (and (stringp speck-aspell-program)
       (file-executable-p speck-aspell-program)))

(defcustom speck-ispell-program
  (locate-file "ispell" exec-path exec-suffixes 'file-executable-p)
  "File name of Ispell program."
  :type '(choice (const :tag "Invalid" nil) (file :tag "File"))
  :group 'speck-ispell)

(defsubst speck-ispell-executable-p ()
  "Return non-nil when `speck-ispell-program' appears executable."
  (and (stringp speck-ispell-program)
       (file-executable-p speck-ispell-program)))

(defcustom speck-engine
  (cond
   ((speck-aspell-executable-p) 'Aspell)
   ((speck-ispell-executable-p) 'Ispell))
  "Spell checker engine used by Speck.
If you have installed both Aspell and Ispell on your system, you
can specify your preferences here.

If the value of this variable is `Invalid' \(nil), Speck was not
able to locate a suitable engine on your system.  In this case
spell checking will not work and you should install either Ispell
or Aspell or make sure that Emacs can find that program.

Don't set this to `Invalid' yourself - it will break Speck."
  :type '(choice (const :tag "Invalid" nil)
		 (const Aspell)
		 (const Ispell))
  :group 'speck)

(defcustom speck-delay 0.5
  "Time in seconds to wait before specking.
Start specking after Emacs has been idle for that many seconds."
  :type 'number
  :group 'speck)

(defcustom speck-pause 0.1
  "Time in seconds to pause specking.
Give other timers a chance to run while specking."
  :type 'number
  :group 'speck)

(defcustom speck-chunk-at-point 'commands
  "Whether the chunk-at-point should be specked.
The \"chunk-at-point\" is the string of non-whitespace characters
around `point'.  When `point' is between two whitespace
characters \(spaces, tabs, and newlines) chunk-at-point is
undefined.

Choices are:

 never ..... never speck chunk-at-point.

 commands .. do not speck chunk-at-point after executing a
             command in `speck-chunk-at-point-commands'.

 always .... always speck chunk-at-point."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Commands" commands)
		 (const :tag "Always" t))
  :group 'speck)

(defcustom speck-chunk-at-point-commands
  '(self-insert-command
    backward-char forward-char
    delete-char delete-backward-char
    backward-delete-char-untabify
    transpose-chars)
  "Commands that inhibit specking chunk-at-point.
This list is consulted if and only if `speck-chunk-at-point' has
been set to `commands'."
  :type '(repeat (symbol))
  :group 'speck)

(defcustom speck-syntactic nil
  "Non-nil means highlight misspelled words in comments or strings only.
Options are to highlight text anywhere in the buffer, text in
comments only, text in strings only, or text in comments or
strings.

The preferred way to set this option is by adding

    (set (make-local-variable 'speck-syntactic) t)

to a major mode's hook."
  :type '(choice (const :tag "Any" nil)
		 (const :tag "Comments" 'comments)
		 (const :tag "Strings" 'strings)
		 (const :tag "Comments or Strings" t))
  :group 'speck)

(defcustom speck-face-inhibit-list nil
  "List of faces that inhibit specking.
If this list is not empty, a word is not marked as misspelled if
the face text property of its first character contains an element
of this list.  The recommended way to set this variable is via a
major mode hook.  The following code asserts that in
`emacs-lisp-mode' text displayed with `font-lock-comment-face' or
`font-lock-doc-face' can be marked, while text displayed with
`font-lock-constant-face' cannot be marked as misspelled.

    (add-hook
     'emacs-lisp-mode-hook
     '(lambda ()
        (set (make-local-variable 'speck-syntactic) t)
        (set (make-local-variable 'speck-face-inhibit-list)
	      '(font-lock-string-face font-lock-constant-face))))"
  :type '(repeat face)
  :group 'speck)

(defcustom speck-doublets nil
  "Non-nil means highlight doublets.
A doublet is the second of two identical symbols \(where symbols
are sequences of characters with word or symbol syntax) starting
with a word character and separated by whitespace only.  Doublets
may not contain misspelled words and are highlighted with the
face `speck-doublet'."
  :type 'boolean
  :group 'speck)

;; Maybe the following should default to nil.
(defcustom speck-self-insert-inherit nil
  "How self-inserting characters inherit speck multi properties.
Speck multi properties specify whether text shall be specked at
all and which dictionary shall be used for specking.

Choices are:

 off \(nil) ... use the standard inheritance mechanism which, by
 default, inherits any such properties from the preceding
 character.

 line ... inherit from preceding character unless the character
 is inserted at the beginning of a line in which case properties
 are inherited from the following character if it exists and is
 not a whitespace character.

 white ... inherit from preceding character unless there is none
 or it is a whitespace character in which case properties are
 inherited from the following character unless there is none or
 it is a whitespace character.

Whitespace characters in this context are spaces, tabs and
newlines, regardless of what the current `syntax-table' says
about them.  If the given choice can't be applied, the standard
inheritance mechanism is used."
  :type '(choice (const :tag "Off" nil)
		 (const :tag "Line" line)
		 (const :tag "White" white))
  :group 'speck)

(defcustom speck-replace-query nil
  "When non-nil query for further occurrences after correcting a word.
The commands to correct a word are `speck-popup-menu-previous',
`speck-popup-menu-next', `speck-replace-previous' and
`speck-replace-next'."
  :type 'boolean
  :group 'speck)

(defcustom speck-replace-preserve-point 'within
  "Where to move cursor within replaced text.
Options are:

 before ... before replaced text

 within ... at same offset from begin of or after replaced text

 after .... after replaced text"
  :type '(choice (const :tag "Before" before)
		 (const :tag "Within" within)
		 (const :tag "After" after))
  :group 'speck)

(defcustom speck-lighter t
  "When non-nil display a string in the mode-line when specking.
Compare `speck-mode-line-specking' and `speck-mode-line-specked'."
  :type 'boolean
  :group 'speck)

(defcustom speck-mode-line-specking t
  "String displayed in mode-line while specking window.
Should contain a leading space.  Selecting \"dictionary\" \(t)
here means display the name of the dictionary valid for the next
character inserted at `window-point'.  In that case \"--\" or
\"==\" mean the respective character is not specked."
  :type '(choice (const :tag "Dictionary" t) string)
  :group 'speck)

(defcustom speck-mode-line-specked t
  "String displayed in mode-line after window has been specked.
Should contain a leading space.  Selecting \"dictionary\" \(t)
here means display the name of the dictionary valid for the next
character inserted at `window-point'.  In that case \"--\" or
\"==\" mean the respective character is not specked."
  :type '(choice (const :tag "Dictionary" t) string)
  :group 'speck)

(add-to-list 'text-property-default-nonsticky '(specked . t))

(defvar speck-delay-timer nil)

(defvar speck-pause-timer nil)

(defvar speck-buffer-list nil
  "List of buffers managed by Speck.")

(defvar speck-window-list nil
  "List of windows that should be specked.")

(defvar speck-stop nil
  "Non-nil when input has arrived during specking.")

(defvar speck-break nil
  "Break specking window when this is non-nil.")

(defvar speck-ppss-at nil)

(defvar speck-ppss nil)

(defvar speck-nospeck-buffer nil
  "Buffer where specking the word at `point' is suspended.
When Emacs is idle and this variable is non-nil it must denote
the current buffer.  Note: The value of `speck-nospeck-at' is
considered meaningful if and only if this is non-nil.")

(defvar speck-nospeck-at nil
  "Value of `window-point' during suspension of speck.")

(defvar speck-suspension-list nil
  "List of windows where specking is suspended.
A window is on this list when everything but the word around
`point' is specked, and `speck-chunk-at-point' settings inhibit
further specking.  This avoids that `speck-window' gets executed
over and over without any progress thus needlessly wasting
resources.  Invariantly, when Emacs is idle, any window on this
list must show the current buffer.

A window is put on this list by `speck-window' and removed by
`speck-desuspend' which is called by `pre-command-hook'.")

(defvar speck-marker (make-marker)
  "Marker used during querying.")

(defvar speck-marker-window nil
  "Window where `speck-marker' was set.")

(defvar speck-replace-history ()
  "Speck replacements history.")

(defvar speck-dictionary nil
  "Default dictionary used for specking this buffer \(a symbol).")
(make-variable-buffer-local 'speck-dictionary)

(defvar speck-process nil
  "Spell-checking process associated with current buffer.")
;; I don't recall why this is buffer-local, maybe there was a reason.
(make-variable-buffer-local 'speck-process)

(defvar speck-process-argument-alist nil
  "List associating each speck process with its arguments.
The key of each element of this list is a process.  The value of
an element is the list of arguments supplied when calling the
process.  When `speck-start-process' is called it checks whether
a process with the supplied arguments exists already.  If such a
process exists it will reuse that process rather than starting a
new one.")

(defvar speck-process-buffer-alist nil
  "List associating each speck process with the buffers that use it.
The key of each element of this list is a process.  The value is
a list of buffers sharing that process.  Whenever
`speck-start-process' decides that it may reuse a running process
it appends the current buffer to the value for that process.
Otherwise it creates a new process and entry.  When the list of
buffers associated with a process becomes empty that process is
eventually deleted.

Note: The buffers on this list are the \"normal\" buffers whose
spelling is checked, not the process buffers.")

(defvar speck-process-dictionary-alist nil
  "List associating each speck process with a dictionary.
The key of each element of this list is a process.  The value is
the dictionary supplied when calling the process.

This variable is buffer-local for the following reason: All text
regions of a buffer with a given dictionary text-property share
the same process since we do not recognize any other
text-property.  Hence to find out whether a process for a
dictionary text-property exists, we search for an association in
this list first.")
(make-variable-buffer-local 'speck-process-dictionary-alist)

(defvar speck-hash-table nil
  "The local hash-table for this buffer.")
(make-variable-buffer-local 'speck-hash-table)

(defvar speck-multi-pre-property nil
  "Variable holding multi property for next command.")

(defvar speck-multi-post-property nil
  "Variable holding multi property for this command.")

(defvar speck-retain-local-variables nil
  "Non-nil means activating speck should retain local variables.
Bound locally by `speck-buffer' and `speck-filter-mode'.")

(defvar speck-multi nil
  "Non-nil means check for multiple languages.")
;; `speck-multi' may be set during `insert-file-contents' and must
;; survive any subsequent `kill-all-local-variables' when a major mode
;; is chosen; therefore it must be `permanent-local'.  It will also
;; continue to live after specking has been deactivated in a buffer and
;; is needed when you save or kill the buffer.
(make-variable-buffer-local 'speck-multi)
(put 'speck-multi 'permanent-local t)

(defun speck-after-change-major-mode ()
  ;; Customize this eventually ...
  (when speck-multi (speck-mode 1)))

(defun speck-add-after-change-major-mode (value)
  ;; ... and implicitly that.
  (add-hook 'after-change-major-mode-hook 'speck-after-change-major-mode))

(defconst speck-multi-format-alist-entry
  '(speck
    "Speck multi format"
    ;; Note: Never anchor the following regexp at BOL.  `revert-buffer'
    ;; may leave the stuff at BOB unchanged and _not_ start searching
    ;; from BOB.  Note too: We allow 12 characters for the
    ;; `comment-start' sequence here.  Change this number if a mode
    ;; needs it.
    ".\\{,12\\}<<speck-bof:speck-\\(?:\\(eof\\)\\|nil>>\\)"
    speck-multi-read
    speck-multi-write
    nil speck-add-after-change-major-mode t) ; See above ....
  "Entry suitable for `format-alist'.
`speck-multi-style' adds/removes this entry to/from `format-alist'.")

(defcustom speck-multi-style nil
  "Style used to write multi-dictionary information to files.

The following choices are available:

None ......... Do not read or write multi-dictionary information.

End of file .. Write multi-dictionary information at end of file.

Standard ..... Intersperse multi-dictionary information with text.

When you choose \"None\" no multi-dictionary annotations are read
even if the file contains them.  The other two options read
annotations as they are found on the file but differ in the way
they write them out.  The \"Standard\" method puts strings
\(\"<<speck-foo:\" and \"speck-foo>>\") around every single
stretch of text checked in the language \"foo\".  If these
strings confuse other applications operating on that file, use
the \"End of file\" method which puts all annotations on a
single, commented-out line at the end of the file.

Note 1: Neither \"Standard\" nor \"End of file\" will handle
insertion of parts of files into a buffer correctly.  If you want
to insert a subsection of a file containing multi-dictionary
information it's strongly recommended to visit that file in
another buffer first and subsequently yank the desired part(s) to
the final target.

Note 2: The \"End of file\" method may fail to install
multi-dictionary information properly if you decode a file
differently than you encoded it.  Hence, if you want to preserve
multi-dictionary information when switching to another coding
system you should \(temporarily) use the \"Standard\" method.

Note 3: Setting this option to \"Standard\" or \"End of file\"
adds a suitable entry to `format-alist'.  Implicitly, this will
add a \(commented-out) line at file-beginning when writing the
buffer to the visited file.  Resetting this option to \"None\"
removes the entry from `format-alist' but doesn't remove the line
and the corresponding annotations from the file until you
explicitly save the buffer."
  :type '(choice (const :tag "None" nil)
		 (const :tag "End of file" eof)
		 (const :tag "Standard" t))
  :set #'(lambda (symbol value)
	   (set-default symbol value)
	   (setq format-alist
		 (delete speck-multi-format-alist-entry format-alist))
	   (when value
	     (add-to-list 'format-alist speck-multi-format-alist-entry)))
  :group 'speck)

(defcustom speck-dictionary-names-alist nil
  "List associating a positive integer with a dictionary name.
The option permits to choose the language for `speck-multi-set'
by entering the associated integer as prefix argument.

Do not set up an association for zero or a negative value here
since these are not handled correctly."
  :type '(repeat
	  (cons :format "%v\n"
		(integer :format "%v" :size 2)
		(string :tag "Dictionary Name:" :format "  %t  %v\n" :size 5)))
  :group 'speck)

(defcustom speck-auto-correct-case nil
  "Non-nil means auto-correct case of misspelled word.

Options are:

 Never ... Do not auto-correct case.

 Two ..... Auto-correct if and only if down-casing the second
           character yields a correct word.

 One ..... Auto-correct if there's a unique correct word
           differing in case only.

 Always .. Auto-correct if speck finds a correct word differing
           in case only \(not recommended, use \"One\" unless
           that's too slow on your system\).

See also `speck-auto-correct-minimum-length'."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Two" two)
		 (const :tag "One" one)
		 (const :tag "Always" t))
  :group 'speck)

(defcustom speck-auto-correct-minimum-length 3
  "Minimum length of words that should be auto-corrected.
Setting this to zero will attempt to auto-correct all words.  If
the \"-W\" option of `ispell-extra-args' provides a larger value
than this, there won't be any misspelling and consequently no
auto-correction."
  :type 'integer
  :group 'speck)

(defcustom speck-email-citations "[>}|]"
  "Check citations in Email mode.
A value of nil means check citations as any other text.  A face
value means don't check text displayed with that face.  A regexp
means don't check text when the beginning of the line where the
text appears matches that regular expression."
  :type '(choice (const :tag "Never" nil)
		 (face :face "Face" font-lock-comment-face)
		 (regexp :tag "Regexp" "[>}|]"))
  :group 'speck)

;; _____________________________________________________________________________
;; 										
;;;				    Faces					
;; _____________________________________________________________________________
;; 										
(defgroup speck-faces nil
  "Faces."
  :group 'speck)

(defface speck-guess
  '((((class color)) :underline "red")
    (t :underline t))
  "Face for highlighting misspelled words with guesses."
  :group 'speck-faces)

(defface speck-miss
  '((((class color)) :underline "orange")
    (t :underline t))
  "Face for highlighting misspelled words without guesses."
  :group 'speck-faces)

(defface speck-doublet
  '((((class color)) :underline "brown")
    (t :underline t))
  "Face for highlighting doublets.
A doublet is the second of two identical symbols (sequences of
characters with word- or symbol syntax) starting with a word
charactetr and separated by whitespace only.  Doublets are
highlighted if and only if the variable `speck-doublets' is
non-nil.  Symbols containing misspelled words are not considered
doublets."
  :group 'speck-faces)

(defface speck-mouse
  '((((class color)) :background "thistle")
    (t :underline t))
  "Face for highlighting misspelled word when the mouse is over it."
  :group 'speck-faces)

(defface speck-query
  '((((class color)) :background "yellow")
    (t :underline t))
  "Face for highlighting word in queries."
  :group 'speck-faces)

(defface speck-mode-line-specking
  '((((class color)) :foreground "orange")
    (t nil))
  "Face for highlighting `speck-mode-line-specking' string."
  :group 'speck)

(defface speck-mode-line-specked
  '((((class color)) :foreground "green")
    (t nil))
  "Face for highlighting `speck-mode-line-specked' string."
  :group 'speck)

;; _____________________________________________________________________________
;; 										
;;;				Aspell group					
;; _____________________________________________________________________________
;; 										
(defgroup speck-aspell nil
  "Aspell related options."
  :group 'speck)

(defcustom speck-aspell-home-dir nil
  "Directory where personal word lists reside.
Default uses the default proposed by Aspell.  File means you have
to explicitly specify the directory where your personal word
lists reside."
  ;; Current Directory means Aspell shall look for the word lists in the
  ;; directory of the file it's checking (doesn't work here and can't be
  ;; tested).
  :type '(choice (const :tag "Default" nil)
		 (file :tag "File"))
  ;; 		 (const :tag "Current Directory" t))
  :group 'speck-aspell)

(defcustom speck-personal-dictionary-file nil
  "Relative name of personal dictionary file.
If this option is non-nil Speck asks Aspell to search for a file
of this name in the directory specified by
`speck-aspell-home-dir'.  The default value leaves it to Aspell
to determine the name of this file.  When working with multiple
dictionaries it seems better to leave this option alone.
Otherwise Aspell won't be able to add a word to the word list
corresponding to the respective dictionary."
  :type '(choice (const :tag "Default" nil)
		 (file :tag "File"))
  :group 'speck-aspell)

(defvar speck-aspell-dictionary-directory
  (when (speck-aspell-executable-p)
    (with-temp-buffer
      (call-process speck-aspell-program nil t nil "config" "dict-dir")
      (car (split-string (buffer-string)))))
  "Directory where Aspell's dictionaries reside.")

(defvar speck-aspell-data-directory
  (when (speck-aspell-executable-p)
    (with-temp-buffer
      (call-process speck-aspell-program nil t nil "config" "data-dir")
      (car (split-string (buffer-string)))))
  "Directory where Aspell's data files reside.")

(defun speck-aspell-dictionary-names ()
  "Return list of Aspell's dictionary names."
  (when (speck-aspell-executable-p)
    (split-string
     (with-temp-buffer
       (call-process speck-aspell-program nil t nil "dicts")
       (buffer-substring (point-min) (point-max)))
     "[ \t\n\f]" t)))

(defvar speck-aspell-dictionary-names (speck-aspell-dictionary-names)
  "List of Aspell's dictionary names.")

(defvar speck-aspell-non-dictionary-names nil
  "List of dictionary names asked for but not provided by Aspell.")

(defvar speck-aspell-dictionary-names-history (speck-aspell-dictionary-names)
  "History of entered Aspell dictionary names.")

(defun speck-aspell-default-dictionary-name ()
  "Return name of Aspell's default dictionary."
  (when (speck-aspell-executable-p)
    (with-temp-buffer
      (call-process speck-aspell-program nil t nil "config" "lang")
      (goto-char (point-min))
      (buffer-substring (point-min) (line-end-position)))))

(defun speck-aspell-aliases ()
  "Return alist with known aliases for Aspell's dictionaries."
  (when speck-aspell-dictionary-directory
    (let ((alias-files (file-expand-wildcards
			(concat speck-aspell-dictionary-directory "/*.alias")))
	  aliases)
      (dolist (alias-file alias-files aliases)
	(with-temp-buffer
	  (insert-file-contents alias-file)
	  (when (re-search-forward "^add \\([^.]+\\)\\.multi" nil t)
	    (let* ((alias (file-name-sans-extension
			   (file-name-nondirectory alias-file)))
		   (name (match-string-no-properties 1))
		   (entry (assoc name aliases)))
	      (if entry
		  (setcdr entry (cons alias (cdr entry)))
		(setq aliases (cons (cons name (list alias)) aliases))))))))))

(defun speck-aspell-charset (name)
  "Return Aspell's coding system (aka charset)."
  (setq name (if (string-match "\\(^[a-zA-Z]+\\)[_-]" name)
		 (match-string-no-properties 1 name)
	       name))
  (let ((data-file (concat speck-aspell-data-directory "/" name ".dat")))
    (when (file-exists-p data-file)
      (with-temp-buffer
	(insert-file-contents data-file)
	(goto-char (point-min))
	(when (re-search-forward "^charset\\s-+" nil t)
	  (let ((charset (buffer-substring (match-end 0) (line-end-position))))
	    (when (string-match "\\(iso\\)\\([^-].*\\)$" charset)
	      ;; iso... should become iso-...
	      (setq charset
		    (concat
		     (match-string-no-properties 1 charset) "-"
		     (match-string-no-properties 2 charset))))
	    (intern charset)))))))

(defun speck-aspell-dictionary-names-and-aliases ()
  "Return names of all installed Aspell dictionaries with aliases."
  (let ((aliases (speck-aspell-aliases))
	names)
    (dolist (name speck-aspell-dictionary-names)
      (setq names
	    (cons (list name (cdr (assoc name aliases)))
		  names)))
    ;; could sort that here
    (nreverse names)))

(defcustom speck-aspell-default-dictionary-name
  (speck-aspell-default-dictionary-name)
  "Name of Aspell default dictionary.
The default dictionary is used for specking a buffer unless you
specify another dictionary via `speck-buffer' or a file-local
variable

The list of dictionaries is constructed from the dictionaries
installed in Aspell's dictionary
directory (`speck-aspell-dictionary-directory') by calling
`speck-aspell-program'.  The standard value is the initial value
provided by Aspell.

In addition to the name of the dictionary we display also its
known aliases (in parentheses)."
  :type `(radio
	  :indent 2
	  ,@(mapcar
	     (lambda (name)
	       (list 'const
		     :format
		     (concat
		      "%v"
		      (when (nth 1 name)
			(let ((aliases (car (nth 1 name))))
			  (dolist (alias (cdr (nth 1 name)) aliases)
			    (setq aliases (concat aliases ", " alias)))
			  (concat "  (" aliases ")")))
		      "\n")
		     (car name)))
	     (speck-aspell-dictionary-names-and-aliases)))
  :group 'speck-aspell)

(defun speck-aspell-languages ()
  "Return list of Aspell's languages."
  (when (speck-aspell-executable-p)
    (let (language languages)
      (with-temp-buffer
	;; Aspell returns this list sorted alphabetically.
	(call-process speck-aspell-program nil t nil "dicts")
	(goto-char (point-min))
	(while (not (eobp))
	  ;; The first two letters constitute the language code.
	  (setq language (buffer-substring-no-properties
			  (point) (+ (point) 2)))
	  (unless (member language languages)
	    (push language languages))
	  (forward-line)))
      (nreverse languages))))

(defun speck-aspell-language-options ()
  "Return initial value for variable `speck-aspell-language-options'."
  (let (options)
    (nreverse
     (dolist (language (speck-aspell-languages) options)
       (push (list
	      language
	      (speck-aspell-charset language)
	      nil
	      (when (string-match "^de" language) t))
	     options)))))

(defcustom speck-aspell-language-options (speck-aspell-language-options)
  "Aspell language options.

Its value is a list of five entries for each language recognized
by Speck.

\(1) The two letter ISO-639 language code.

\(2) The coding system for sending text to and receiving text
     from the Aspell process.

\(3) A minimum word length which tells Aspell to ignore words
     shorter than that.

\(4) A run-together words entry telling Aspell the maximum number
     of words that can be strung together \(where the value t
     means no limit).

\(5) Extra Arguments passed to Aspell.

Specifying a value of \"None\" \(nil) for \(2--4) means do not
pass a value for this option to Aspell."
  :type '(repeat
	  (list :tag "" :format "%v"
		(string :tag "Language" :format "%t: %v\n" :size 2)
		(choice :tag "Coding System" :format "%t: %[Choice%] %v\n"
			(const :tag "None" nil)
			(coding-system :tag "Coding System" :size 10 :value utf-8)
			;; Leave this in: it might be useful when a user changes
			;; this option and installs a new dictionary later.
			(const :tag "Aspell" t))
		(choice :tag "Minimum Word Length" :format "%t: %[Choice%] %v\n"
			(const :tag "None" :format "%t" nil)
			(integer :tag "Length" :format "%t: %v " :size 2))
		(choice :tag "Run-together Words" :format "%t: %[Choice%] %v\n"
			(const :tag "None" :format "%t" nil)
			(integer :tag "Maximum Number" :format "Number: %v " :size 2)
			(const :tag "Unlimited" :format "%t" t))
		(repeat :tag "Extra Arguments"
			(string :format "%v\n" :size 40))))
  :group 'speck-aspell)

(defcustom speck-aspell-coding-system nil
  "Language independent coding system for communicating with Aspell.
`None' means accept the setting from
`speck-aspell-language-options'.  Specifying a value here will
override those settings.  If communication with Aspell appears
broken and the version of Aspell is 0.60 setting this to `utf-8'
might help."
  :type '(choice (cons :tag "None" nil)
		 (coding-system :tag "Coding System" utf-8))
  :group 'speck-aspell)

(defcustom speck-aspell-minimum-word-length nil
  "Language independent minimum word length.
`None' means accept the setting from
`speck-aspell-language-options'.  Specifying a value here will
override those settings."
  :type '(choice (const :tag "None" nil)
		 (integer :tag "Length" :format "%t: %v " :size 2))
  :group 'speck-aspell)

(defcustom speck-aspell-maximum-run-together nil
  "Language independent maximum number of run-together words.
`None' means accept the setting from
`speck-aspell-language-options'.  Specifying a value here will
override those settings."
  :type '(choice (const :tag "None" :format "%t" nil)
		 (integer :tag "Maximum Number" :format "Number: %v " :size 2)
		 (const :tag "Unlimited" :format "%t" t))
  :group 'speck-aspell)

(defcustom speck-aspell-extra-arguments nil
  "Language independent arguments passed to Aspell process.
These arguments are passed to Aspell regardless of any other
arguments.  Language dependent arguments can be supplied by
customizing `speck-aspell-language-options'."
  :type '(repeat (string :tag "Argument" :format "%t: %v\n" :size 40))
  :group 'speck-aspell)

(defcustom speck-aspell-suggestion-mode "ultra"
  "Aspell suggestion mode.
The default value does not pass a suggestion mode to the Aspell
process.  On slow systems `Ultra' is recommended.  With Aspell
0.60 the method `Fast' is identical to `Ultra'.  You have to
\(re-)activate `speck-mode' for this option to take effect."
  :type '(choice (const :tag "None" nil)
		 (const :tag "Ultra" "ultra")
		 (const :tag "Fast" "fast")
		 (const :tag "Normal" "normal")
		 (const :tag "Slow" "slow")
		 (const :tag "Bad Spellers" "bad-spellers"))
  :group 'speck-aspell)

;; IIUC this is only provided by Aspell.
(defcustom speck-filter-mode 'URL
  "Speck filter mode.
Probably supported by Aspell only."
  :type '(choice 
	  (const None)
	  (const URL)
	  (const Email)
	  (const SGML)
	  (const TeX))
  :group 'speck-aspell)
(make-variable-buffer-local 'speck-filter-mode)

;; _____________________________________________________________________________
;; 										
;;;			    Ispell group					
;; _____________________________________________________________________________
;; 										
(defgroup speck-ispell nil
  "Ispell related options."
  :group 'speck)

(defun speck-ispell-vv ()
  "Return Ispell settings.
Return value is a list with the following elements:

0 ... library directory
1 ... default dictionary"
  (condition-case nil
      (when (speck-ispell-executable-p)
	(with-temp-buffer
	  (let (v1 v2)
	    (call-process speck-ispell-program nil t nil "-vv")
	    (goto-char (point-min))
	    (when (re-search-forward "LIBDIR = \\\"\\([^ \t\n]*\\)\\\"" nil t)
	      (setq v1 (buffer-substring (match-beginning 1) (match-end 1))))
	    (goto-char (point-min))
	    (when (re-search-forward "DEFHASH = \\\"\\([^ \t\n]*\\)\\.hash?\\\"" nil t)
	      (setq v2 (buffer-substring (match-beginning 1) (match-end 1))))
	    (list v1 v2))))
    (error nil)))

(defvar speck-ispell-vv (speck-ispell-vv)
  "Value returned by `speck-ispell-vv'.")

(defcustom speck-ispell-library-directory
  (or (and speck-ispell-vv
	   (file-directory-p (nth 0 speck-ispell-vv))
	   (nth 0 speck-ispell-vv))
      (and speck-ispell-program
	   (file-directory-p (file-name-directory speck-ispell-program))
	   (file-name-directory speck-ispell-program)))
  "Name of Ispell library directory.
This should name the directory where the Ispell dictionaries
reside."
  :type '(choice (const :tag "Invalid" nil)
		 (file :tag "File"))
  :group 'speck-ispell)

(defcustom speck-iso-639-1-alist
  '(("bg" . "bulgarian") ("ca" . "catalan") ("cs" . "czech")
    ("da" . "danish") ("de" . "deutsch") ("de" . "german")
    ("el" . "greek") ("en" . "english") ("eo" . "esperanto")
    ("es" . "spanish") ("fi" . "finnish") ("fr" . "francais")
    ("fr" . "french") ("hu" . "hungarian") ("it" . "italiano")
    ("it" . "italian") ("la" . "latin") ("nl" . "dutch")
    ("no" . "norwegian") ("pl" . "polish") ("pt" . "portuguese")
    ("ro" . "romanian") ("ru" . "russian") ("sh" . "serbo-croatian")
    ("sk" . "slovak") ("sv" . "swedish") ("tr" . "turkish"))
  "List associating ISO-639-1 language codes with language names.
This list should ideally provide associations for all languages
handled by Ispell.  The language code is displayed in the
mode-line provided Speck finds the appropriate association.
Otherwise Speck will display the first two characters of the
dictionary name.  This list is not needed for the Aspell
interface."
  :type '(repeat
	  (cons :format "%v\n"
		(string :format " %v" :size 2)
		(string :format " %v" :size 20)))
  :group 'speck-ispell)

(defsubst speck-ispell-binary-directory ()
  "Return directory component of `speck-ispell-program'."
  (when speck-ispell-program
    (file-name-directory speck-ispell-program)))

(defun speck-ispell-dictionary-alist ()
  "Return alist of language codes and names of installed Ispell dictionaries."
  (cond
   ((and speck-ispell-library-directory
	 (file-exists-p speck-ispell-library-directory)
	 (let ((names (directory-files
		       speck-ispell-library-directory nil "\\.hash?$"))
	       alist aname)
	   (dolist (name names)
	     (setq aname (downcase (file-name-sans-extension name)))
	     (setq alist
		   (cons (or (rassoc aname speck-iso-639-1-alist)
			     (cons (substring aname 0 2) aname))
			 alist)))
	   (nreverse alist))))
   ((and (speck-ispell-binary-directory)
	 (file-exists-p (speck-ispell-binary-directory)))
    ;; This assumes that speck-ispell-binary-directory contains directories with
    ;; each directory containing the respective .hash files.
    (let ((files (directory-files (speck-ispell-binary-directory) t))
	  alist aname)
      (dolist (file files)
	(when (and (not (string-equal file "."))
		   (not (string-equal file ".."))
		   (file-directory-p file))
	  (let ((names (directory-files file nil "\\.hash?$")))
	    (dolist (name names)
	      (setq aname (downcase (file-name-sans-extension name)))
	      (setq alist
		    (cons (or (rassoc aname speck-iso-639-1-alist)
			      (cons (substring aname 0 2) aname))
			  alist))))))
      (nreverse alist)))))

;; We could introduce an update function for this.  This would be useful
;; when a new dictionary is installed.
(defcustom speck-ispell-dictionary-alist (speck-ispell-dictionary-alist)
  "List associating a language code with Ispell dictionary names.
This list is generated from the option `speck-iso-639-1-alist'
and the dictionaries found in `speck-ispell-library-directory'.

If this list contains two or more entries for the same language
code, Speck may display the wrong language code in the mode-line.
Hence, you should make sure that every language code occurs once
only in this list.  The preferred way to do this is by adding a
corresponding association to `speck-iso-639-1-alist'."
  :type '(repeat
	  (cons :format "%v\n"
		;; We could use a symbol here.
		(string :format " %v" :size 2)
		(string :format " %v" :size 20)))
  :group 'speck-ispell)

(defun speck-ispell-dictionary-names ()
  "Return list of Ispell's dictionary names."
  (let (list)
    (dolist (entry speck-ispell-dictionary-alist)
      (setq list (cons (car entry) list)))
    (nreverse list)))

(defvar speck-ispell-dictionary-names (speck-ispell-dictionary-names)
  "List of Ispell's dictionary names.")

(defvar speck-ispell-non-dictionary-names nil
  "List of dictionary names asked for but not provided by Ispell.")

(defvar speck-ispell-dictionary-names-history (speck-ispell-dictionary-names)
  "History of entered Ispell dictionary names.")

(defcustom speck-ispell-default-dictionary-name
  (when speck-ispell-vv
    (let ((name (nth 1 speck-ispell-vv)))
      (car (rassoc name speck-ispell-dictionary-alist))))
  "Name of Ispell default dictionary.
The default dictionary is used for specking a buffer unless you
specify another dictionary via `speck-buffer' or a file-local
variable."
  :type `(radio
	  :indent 2
	  ,@(mapcar
	     (lambda (entry)
	       (list 'const :format "%v \n" (car entry)))
	     speck-ispell-dictionary-alist))
  :group 'speck-ispell)

(defcustom speck-ispell-language-options
  '(("de" iso-8859-1 nil t)
    ("en" iso-8859-1 nil nil)
    ("fr" iso-8859-1 nil nil)
    ("it" iso-8859-1 nil nil)
    ("ru" koi8-r nil nil))
    "Ispell language options.
Its value should be a list of five entries for each language.

\(1) The two letter ISO-639-1 language code.  For a
     correspondence of this code to the names of dictionaries in
     `speck-ispell-library-directory' confer the option
     `speck-ispell-dictionary-alist'.

\(2) The coding system for sending text to and receiving text
     from the Ispell process for this language.

\(3) A minimum word length which tells Ispell to ignore words
     shorter than that.

\(4) A run-together words flag telling Ispell whether words can
     be strung together.

\(5) Extra Arguments passed to Ispell.

Specifying \"None\" \(nil) for \(2--4) means do not pass a value
for this option to Ispell."
  :type '(repeat
	  (list :tag "" :format "%v"
		(string :tag "Language" :format "%t: %v\n" :size 2)
		(choice :tag "Coding System" :format "%t: %[Choice%] %v\n"
			(const :tag "None" nil)
			(coding-system :tag "Coding System" :size 10 :value utf-8)
			;; Leave this in: it might be useful when a user changes
			;; this option and installs a new dictionary later.
			(const :tag "Ispell" t))
		(choice :tag "Minimum Word Length" :format "%t: %[Choice%] %v\n"
			(const :tag "None" :format "%t" nil)
			(integer :tag "Length" :format "%t: %v " :size 2))
		(boolean :tag "Run-together Words")
		(repeat :tag "Extra Arguments"
			(string :format "%v\n" :size 40))))
  :group 'speck-ispell)

(defcustom speck-ispell-coding-system nil
  "Language independent coding system for communicating with Ispell.
`None' means accept the setting from
`speck-ispell-language-options'.  Specifying a value here will
override those settings."
  :type '(choice (cons :tag "None" nil)
		 (coding-system :tag "Coding System" utf-8))
  :group 'speck-ispell)

(defcustom speck-ispell-minimum-word-length nil
  "Language independent minimum word length.
`None' means accept the setting from
`speck-ispell-language-options'.  Specifying a value here will
override those settings."
  :type '(choice (const :tag "None" nil)
		 (integer :tag "Length" :format "%t: %v " :size 2))
  :group 'speck-ispell)

(defcustom speck-ispell-run-together nil
  "Language independent maximum number of run-together words.
`None' means accept the setting from
`speck-ispell-language-options'.  Specifying a value here will
override those settings."
  :type 'boolean
  :group 'speck-ispell)

(defcustom speck-ispell-extra-arguments nil
  "Language independent arguments passed to Ispell process.
These arguments are passed to Ispell regardless of any other
arguments.  Language dependent arguments can be supplied by
customizing `speck-ispell-language-options'."
  :type '(repeat (string :tag "Argument" :format "%t: %v\n" :size 40))
  :group 'speck-ispell)

(defun speck-ispell-start-process ()
  "Start Ispell process."
  ;; `speck-dictionary' is the language code.
  (let* ((code-name (symbol-name speck-dictionary))
	 (dictionary-name
	  (cdr (assoc code-name speck-ispell-dictionary-alist)))
	 (options (assoc code-name speck-ispell-language-options))
	 (coding-system (nth 1 options))
	 (minimum-word-length
	  ;; A value specified in `speck-ispell-minimum-word-length' overrides
	  ;; anything else.
	  (or speck-ispell-minimum-word-length
	      (when options (nth 2 options))))
	 (run-together
	  ;; A value specified in `speck-aspell-maximum-run-together' overrides
	  ;; anything else.
	  (or speck-ispell-run-together (nth 3 options)))
	 (extra-arguments (nth 4 options))
	 (arguments
	  (append
	   ;; Pipe option and dictionary-name.
	   (list "-a" "-d" dictionary-name)
	   ;; Minimum word length.
	   (when minimum-word-length
	     (list (concat "-W" (number-to-string minimum-word-length))))
	   ;; Run-together words.
	   (if run-together (list "-C") (list "-B"))
	   extra-arguments
	   speck-ispell-extra-arguments))
	 (process (rassoc arguments speck-process-argument-alist))
	 ;; An options should exist when a process exists, but be paranoid here.
	 (options (when process (assq (car process) speck-process-buffer-alist)))
	 process-connection-type)
    (if (and process options)
	;; Process and options exist.
	(progn
	  (setq speck-process (car process))
	  (setcdr options (cons (current-buffer) (cdr options))))
      ;; No suitable process exists.
     (let ((default-directory
	     (if (and (file-directory-p default-directory)
		      (file-readable-p default-directory))
		 ;; Defend against bad `default-directory'.
		 default-directory
	       (expand-file-name "~/"))))
       (setq speck-process
	     (apply 'start-process "speck"
		    (generate-new-buffer " *speck-process-buffer*")
		    speck-ispell-program arguments)))
      (setq speck-process-buffer-alist
	    (cons (cons speck-process (list (current-buffer)))
		  speck-process-buffer-alist))
      (setq speck-process-argument-alist
	    (cons (cons speck-process arguments)
		  speck-process-argument-alist))
      (setq speck-process-dictionary-alist
	    ;; Buffer local.
	    (cons (cons speck-process speck-dictionary)
		  speck-process-dictionary-alist))
      (set-process-query-on-exit-flag speck-process nil)
      (when coding-system
	(set-process-coding-system
	 speck-process coding-system coding-system)))))


;; _____________________________________________________________________________
;; 										
;;;				Files						
;; _____________________________________________________________________________
;; 										
(defgroup speck-save nil
  "Saving speck specifications to visited file."
  :group 'speck)

(defcustom speck-save-ask 'ask
  "Save/restore speck specifications to/from visited file.
When this option equals \"Never\" \(nil) Speck never saves or
restores specifications.  When the option equals \"Ask\" \('ask)
Speck may ask you once and only once in the lifetime of a buffer
whether specifications shall be saved or restored.  When the
option is \"Silently\" \(t) Speck is allowed to silently save and
restore specifications.

Specifications affected by this option are local words and
options, as well as multi-dictionary properties.  Settings for
this option override any existing settings for
`speck-multi-style', `speck-save-words' and
`speck-save-options'."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Ask" ask)
		 (const :tag "Silently" t))
  :group 'speck-save)
;; `speck-save-ask' may be consulted during `insert-file-contents' and should
;; survive any subsequent `kill-all-local-variables'.  It will also continue to
;; live after specking has been deactivated in a buffer and is needed when you
;; save or kill the buffer.
(make-variable-buffer-local 'speck-save-ask)
(put 'speck-save-ask 'permanent-local t)

(defvar speck-save-confirmed 'undecided
  "Whether saving and restoring has been confirmed.
The purpose of this variable is to ask only once for each buffer
whether specifications shall be saved/restored from/to the
visited file.  If its value is nil, Speck has asked this question
and the user said no.  If the value is 'undecided Speck hasn't
asked yet.  If the value is t speck has asked and the user has
said yes or the actual value of`speck-save-ask' allows to
save/restore silently.  Any value but undecided can't be changed
again during the lifetime of the buffer.")
;; `speck-save-confirmed' may be consulted during `insert-file-contents'
;; and should survive any subsequent `kill-all-local-variables';
;; otherwise the `speck-save-confirm' question may be asked again and
;; again.  It will also continue to live after specking has been
;; deactivated in a buffer and is needed when you save or kill the
;; buffer.
(make-variable-buffer-local 'speck-save-confirmed)
(put 'speck-save-confirmed 'permanent-local t)

(defun speck-save-confirm ()
  "Confirm save/restore of specifications to/from file.
This function should ask the user a question once and only once
in the lifetime of a buffer.  It asks if and only if the option
`speck-save-ask' is set to 'ask, the buffer is visiting a file,
and Speck has to save or restore a specification like a local
word, option, or text property.

This function should be always called _last_ in conjuncts.
Otherwise you might end up asking the question when it shouldn't
be asked."
  (or (eq speck-save-confirmed t)
      (and (eq speck-save-ask t) (setq speck-save-confirmed t))
      (and speck-save-ask buffer-file-name
	   speck-save-confirmed ; hence it's yet 'undecided
	   (setq speck-save-confirmed
		 (y-or-n-p
		  "Process buffer-local specifications for specking? ")))))

(defcustom speck-save-permanent 'adjust
  "Whether specification changes are permanent.
Specifications affected by this option are those Speck may save
to file like local words and options or multi-dictionary
properties.

If this option is nil, changing such a specification will (1)
make the current buffer appear modified if it was unmodified so
far and (2) can be undone via \\[undo].  Note, however, that if
Speck appends a word to the Local Words list of a buffer and you
manually undo that addition together with other ones, Speck will
not be aware of that and continue to consider the word as
correct.

If this option is non-nil, changing a specification will not make
the current buffer appear modified and cannot be undone.  If this
option is `adjust' undoable buffer changes following the affected
Local section are adjusted properly so that undoing them later is
done correctly."
  :type '(choice (const nil)
		 (const t)
		 (const :tag "Adjust" adjust))
  :group 'speck-save)
;; Must be buffer-local.
(make-variable-buffer-local 'speck-save-permanent)

(defcustom speck-save-words t
  "Non-nil means save and restore word lists from and to file.
Speck permits to save word lists to the file visited by a buffer
and restore these lists when you visit the file again.  For this
purpose words are inserted in the Local Words Section \(LWS) of
the file.  The LWS is usually located near the end of the file
\(but _before_ any other \"Local\" section.  When this option is
t and you activate `speck-mode' it will search for a line
matching `speck-save-words-regexp' within 3000 characters from
the end of the buffer and process this and all preceding lines
matching that expression.  Word lists are saved according to the
actual setting of `speck-save-permanent'.

When this option is nil, local words are not restored and/or
saved.  Any LWS present on the file remains unaffected.  Observe
that this option is respected if and only if `speck-save-ask'
does not equal \"Never\" \(nil)."
  :type 'boolean
  :group 'speck-save)

(defcustom speck-save-words-regexp
  "LocalWords\\|Local Words"
  "Regexp for buffer local word list.
The regexp must match within 3000 characters from the end of the
buffer.  Ispell looks by default for \"LocalWords: \".

Speck automatically adds a colon at the end of this.  For
dictionary specific word lists it also searches for a dictionary
code enclosed in parentheses and the colon."
  :type 'regexp
  :group 'speck-save)

(defcustom speck-save-words-string
  "Local Words"
  "String inserted before members of buffer local word list.
The value specified here will be used unless an entry with
another value matched by `speck-save-words-regexp' is on the
file.  `ispell' inserts by default \"LocalWords: \".

Speck automatically adds a colon at the end of this.  For
dictionary specific word lists it adds a space, the dictionary
code within parentheses, and the colon."
  :type 'string
  :group 'speck-save)

(defcustom speck-save-options t
  "Non-nil means save and restore options from and to file.
Speck permits to save options like the current dictionary or
filter mode to the file visited by a buffer and restore these
options buffer-locally when you visit the file again.  For this
purpose options are inserted in the file after any Local Words
Section \(LWS) but before any Local Variables section of the
file.  When you activate `speck-mode' it will search for options
within 3000 characters from the end of the buffer and restore all
options it finds in this area.

When this option is nil options are never processed and/or saved.
Any options present on the file remain unaffected.  This option
is ignored when `speck-save-ask' equals \"Never\" \(nil)."
  :type 'boolean
  :group 'speck-save)

(defcustom speck-save-dictionary-regexp
  "Local IspellDict:\\|Local Dictionary:"
  "Regexp for buffer local dictionary."
  :type 'regexp
  :group 'speck-save)

(defcustom speck-save-dictionary-string
  "Local Dictionary:"
  "String for buffer local dictionary."
  :type 'string
  :group 'speck-save)

(defvar speck-saved-dictionary nil
  "When non-nil dictionary to be saved before killing the buffer.")
(make-variable-buffer-local 'speck-saved-dictionary)

(defcustom speck-save-filter-mode-regexp
  "Local IspellParsing:\\|Local Filter Mode:"
  "Regexp for buffer local mode filter."
  :type 'regexp
  :group 'speck-save)

(defcustom speck-save-filter-mode-string
  "Local Filter Mode:"
  "String for buffer local mode filter."
  :type 'string
  :group 'speck-save)

(defvar speck-saved-filter-mode nil
  "Non-nil when filter mode shall be saved before killing the buffer.")
(make-variable-buffer-local 'speck-saved-filter-mode)

(defvar speck-kill-buffer-query-list nil
  "List of buffers that must be queried before killing.")

(defvar speck-kill-buffer-query nil
  "Whether we must query the user when this buffer is killed.")
(make-variable-buffer-local 'speck-kill-buffer-query)

;; _____________________________________________________________________________
;; 										
;;;			       Keymaps						
;; _____________________________________________________________________________
;; 										
(defun speck-make-mode-map (map)
  "Assign `speck-mode-keys' to MAP which should be `speck-mode-map'."
  (when (boundp 'speck-mode-keys)
    (define-key map (nth 0 speck-mode-keys) 'speck-popup-menu-previous)
    (define-key map (nth 1 speck-mode-keys) 'speck-popup-menu-next)
    (define-key map (nth 2 speck-mode-keys) 'speck-replace-previous)
    (define-key map (nth 3 speck-mode-keys) 'speck-replace-next)
    (define-key map (nth 4 speck-mode-keys) 'speck-add-previous)
    (define-key map (nth 5 speck-mode-keys) 'speck-add-next)
    (define-key map (nth 6 speck-mode-keys) 'speck-region)
    (define-key map (nth 7 speck-mode-keys) 'speck-change-aspell-dictionary)
    (define-key map (nth 8 speck-mode-keys) 'speck-multi-set)
    (define-key map (nth 9 speck-mode-keys) 'speck-set-filter-mode)))

(defvar speck-mode-map
  (let ((map (make-sparse-keymap)))
    (speck-make-mode-map map)
    map)
  "Keymap used by `speck-mode'.  `speck-make-mode-map' fills it.")

(defcustom speck-mode-keys
  '([(control ?\.)] [(control meta ?\.)]
    [(control ?\,)] [(control meta ?\,)]
    [(control ?\+)] [(control meta ?\+)]
    [(control ?\!)] [(control meta ?\!)]
    [(control ?\?)] [(control meta ?\?)])
  "Keys used by `speck-mode'."
  :type
  '(list
    (key-sequence
     :tag "Popup menu at previous word" :format "\n  %t %v\n\n"
     :value '[(control ?\.)] :size 20)
    (key-sequence
     :tag "Popup menu at next word    " :format "  %t %v\n\n"
     :value '[(control meta ?\.)] :size 20)
    (key-sequence
     :tag "Replace previous word      " :format "  %t %v\n\n"
     :value '[(control ?\,)] :size 20)
    (key-sequence
     :tag "Replace next word          " :format "  %t %v\n\n"
     :value '[(control meta ?\,)] :size 20)
    (key-sequence
     :tag "Accept previous word       " :format "  %t %v\n\n"
     :value '[(control ?\+)] :size 20)
    (key-sequence
     :tag "Accept next word           " :format "  %t %v\n\n"
     :value '[(control meta ?\+)] :size 20)
    (key-sequence
     :tag "Spell-check region         " :format "  %t %v\n\n"
     :value '[(control ?\!)] :size 20)
    (key-sequence
     :tag "Change dictionary          " :format "  %t %v\n\n"
     :value '[(control meta ?\!)] :size 20)
    (key-sequence
     :tag "Set language               " :format "  %t %v\n\n"
     :value '[(control ?\?)] :size 20)
    (key-sequence
     :tag "Set option                 " :format "  %t %v\n\n"
     :value '[(control meta ?\?)] :size 20))
  :set #'(lambda (symbol value)
	   (when (and (boundp 'speck-mode-map)
		      ;; Paranoia.
		      (boundp 'speck-mode-keys)
		      (listp speck-mode-keys))
	     (dolist (key speck-mode-keys)
	       (define-key speck-mode-map key nil)))
	   (set-default symbol value)
	   (when (boundp 'speck-mode-map)
	     (speck-make-mode-map speck-mode-map)))
  :group 'speck)

(defun speck-assign-keys-to-map (map keys)
  "Assign KEYS to MAP.
MAP must be a keymap, KEYS a list of (command . key) pairs."
  (dolist (pair keys)
    (define-key map (cdr pair) (car pair))))

(defcustom speck-replace-keys
  '((help . [(control ?\?)])
    (help . [(control ?\h)])
    (help . [f1])
    (help . [help])
    (accept . [(control ?\!)])
    (accept-and-quit . [(control ?\.)])
    (reject-and-quit . [(control ?\-)])
    (reject-and-quit . [(control ?\g)])
    (reject-and-quit . [(control ?\])])
    (reject-and-quit . [escape])
    (forward . [(control ?\,)])
    (backward . [(control meta ?\,)]))
  "Keys used by `speck-mode' during replacement."
  :type
  '(repeat
    (cons :format "%v"
	  (choice :format " %[Command%] %v"
		  (const :format "help           " help)
		  (const :format "accept         " accept)
		  (const :format "accept-and-quit" accept-and-quit)
		  (const :format "reject-and-quit" reject-and-quit)
		  (const :format "forward        " forward)
		  (const :format "backward       " backward))
	  (key-sequence :format "    Key: %v\n\n" :size 20)))
  :set #'(lambda (symbol value)
	   ;; Don't "and" these.
	   (when (boundp 'speck-replace-map)
	     (when (boundp 'speck-replace-keys)
	       (dolist (pair speck-replace-keys)
		 ;; Reset them all.
		 (define-key speck-replace-map (cdr pair) nil))))
	   (set-default symbol value)
	   (when (boundp 'speck-replace-map)
	     (speck-assign-keys-to-map speck-replace-map speck-replace-keys)))
  :group 'speck)

(defvar speck-replace-map
  (let ((map (make-sparse-keymap)))
    (speck-assign-keys-to-map map speck-replace-keys)
    map)
  "Dummy keymap for `speck-replace'.")

(defcustom speck-replace-query-keys
  '((help . [(control ?\?)])
    (help . [(control ?\h)])
    (help . [f1])
    (help . [help])
    (accept . [(control ?\!)])
    (accept . [?\ ])
    (accept . [return])
    (accept-and-quit . [(control ?\.)])
    (reject . [(control ?\-)])
    (reject-and-quit . [(control ?\g)])
    (reject-and-quit . [(control ?\])])
    (reject-and-quit . [escape])
    (forward . [(control ?\,)])
    (forward . [tab])
    (backward . [(control meta ?\,)])
    (backward . [(shift tab)]))
  "Keys used by `speck-mode' during query replacement."
  :type
  '(repeat
    (cons :format "%v"
	  (choice :format " %[Command%] %v"
		  (const :format "help           " help)
		  (const :format "accept         " accept)
		  (const :format "accept-and-quit" accept-and-quit)
		  (const :format "reject         " reject)
		  (const :format "reject-and-quit" reject-and-quit)
		  (const :format "forward        " forward)
		  (const :format "backward       " backward))
	  (key-sequence :format "    Key: %v\n\n" :size 20)))
  :set #'(lambda (symbol value)
	   ;; Don't "and" these.
	   (when (boundp 'speck-replace-query-map)
	     (when (boundp 'speck-replace-query-keys)
	       (dolist (pair speck-replace-query-keys)
		 ;; Reset them all.
		 (define-key speck-replace-query-map (cdr pair) nil))))
	   (set-default symbol value)
	   (when (boundp 'speck-replace-query-map)
	     (speck-assign-keys-to-map
	      speck-replace-query-map speck-replace-query-keys)))
  :group 'speck)

(defvar speck-replace-query-map
  (let ((map (make-sparse-keymap)))
    (speck-assign-keys-to-map map speck-replace-query-keys)
    map)
  "Dummy keymap for `speck-replace-query'.")

;; _____________________________________________________________________________
;; 										
;;;			       Macros						
;; _____________________________________________________________________________
;; 										
(eval-when-compile
  (require 'cl)

  (defmacro with-buffer-unmodified (&rest body)
    "Eval BODY, preserving the current buffer's modified state."
    (declare (debug t))
    (let ((modified (make-symbol "modified")))
      `(let ((,modified (buffer-modified-p)))
	 (unwind-protect
	     (progn ,@body)
	   (unless ,modified
	     (restore-buffer-modified-p nil))))))

  ;; Save match-data maybe .............
  (defmacro with-buffer-prepared-for-specking (&rest body)
    "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
    (declare (debug t))
    `(with-buffer-unmodified
      (let ((buffer-undo-list t)
	    (inhibit-read-only t)
	    (inhibit-point-motion-hooks t)
	    (inhibit-modification-hooks t)
	    (inhibit-field-text-motion t)
	    first-change-hook after-change-functions
	    deactivate-mark buffer-file-name buffer-file-truename)
	,@body))))

;; _____________________________________________________________________________
;; 										
;;;				Mode						
;; _____________________________________________________________________________
;; 										
(defun speck-lighter ()
  "Speck lighter."
  (let (prop prop-insert)
    (propertize
     (if (stringp speck-mode-line-specking)
	 speck-mode-line-specking
       (setq prop (or (speck-get-speck (window-point))
		      speck-dictionary))
       (setq prop-insert
	     (cond
	      ((and speck-multi-pre-property
		    (not (eq speck-multi-pre-property t)))
	       ;; We've just set up a new dictionary and there's no such
	       ;; character yet.  Let's be imaginative.
	       speck-multi-pre-property)
	      ((and speck-self-insert-inherit
		    (not (eobp))
		    (not (memq (char-after) '(?\  ?\n ?\t)))
		    (or (bobp)
			(and (eq speck-self-insert-inherit 'line)
			     (eq (char-before) ?\n))
			(and (eq speck-self-insert-inherit 'white)
			     (memq (char-before) '(?\  ?\n ?\t)))))
	       (speck-get-speck (point)))
	      ((not (bobp))
	       ;; People should not change the stickyness of the speck
	       ;; property.
	       (or (speck-get-speck (1- (point)))
		   speck-dictionary))))
       (concat
	" ["
	(if (and prop-insert (not (eq prop prop-insert)))
	    (symbol-name prop-insert)
	  (symbol-name (or prop speck-dictionary)))
	"]"))
     'face (if (memq (selected-window) speck-window-list)
	       'speck-mode-line-specking
	     'speck-mode-line-specked))))

;;;###autoload
(define-minor-mode speck-mode
  "Toggle `speck-mode'.
With prefix ARG, turn speck-mode on if and only if ARG is
positive.  Turning on speck-mode will spell-check (\"speck\") all
windows showing the current buffer.

Global bindings \(customizable via `speck-mode-keys').

\\{speck-mode-map}"
  :group 'speck
  :init-value nil
  :lighter (:eval (when speck-lighter (speck-lighter)))
  :keymap speck-mode-map
  :require 'speck
  (if speck-mode
      (speck-activate)
    (speck-deactivate)))

;;;###autoload
(defun speck-buffer (&optional arg)
  "Toggle `speck-mode' for current buffer.
With non-numeric prefix argument ARG prompt for \(new)
dictionary.  With prefix argument ARG zero use the default
dictionary.  With ARG any other number use the corresponding
entry from `speck-dictionary-names-alist'."
  (interactive "P")
  (require 'speck)
  (cond
   ((not arg)
    ;; With no argument toggle `speck-mode' respecting any existing
    ;; value for (file-)local dictionary.
    (if speck-mode
	(speck-mode 0)
      (speck-mode 1)))
   ((not (numberp arg))
    ;; With non-numeric argument prompt for dictionary, this may
    ;; override any existing (file-)local value for `speck-dictionary'.
    (let* ((dictionary-names
	    (cond
	     ((eq speck-engine 'Aspell)
	      speck-aspell-dictionary-names)
	     ((eq speck-engine 'Ispell)
	      speck-ispell-dictionary-names)))
	   (dictionary-names-history
	    (cond
	     ((eq speck-engine 'Aspell)
	      speck-aspell-dictionary-names-history)
	     ((eq speck-engine 'Ispell)
	      speck-ispell-dictionary-names-history)))
	   (dictionary-name
	    (completing-read
	     (concat
	      "Enter " (when speck-mode "new ")
	      "dictionary name (RET for default, SPC to complete): ")
	     (mapcar 'list (cons "default" dictionary-names))
	     nil t nil 'dictionary-names-history))
	   (default-dictionary-name
	     (cond
	      ((eq speck-engine 'Aspell)
	       speck-aspell-default-dictionary-name)
	      ((eq speck-engine 'Ispell)
	       speck-ispell-default-dictionary-name)))
	   dictionary)
      (if (or (string-equal dictionary-name "")
	      (string-equal dictionary-name "default"))
	  (setq dictionary (intern default-dictionary-name))
	(setq dictionary (intern dictionary-name)))
      (if speck-mode
	  ;; Retain all local variable values but that of
	  ;; `speck-dictionary'.
	  (if (eq dictionary speck-dictionary)
	      (message "Dictionary \"%s\" unchanged" dictionary)
	    (speck-deactivate)
	    (setq speck-dictionary dictionary)
	    (let ((speck-retain-local-variables t))
	      (speck-mode)))
	(setq speck-dictionary dictionary)
	(speck-mode))))
   ((zerop arg)
    ;; With argument zero force use of default dictionary (may override
    ;; file-local value).
    (let ((dictionary
	   (cond
	    ((eq speck-engine 'Aspell)
	     (intern speck-aspell-default-dictionary-name))
	    ((eq speck-engine 'Ispell)
	     (intern speck-ispell-default-dictionary-name)))))
      (if speck-mode
	  (if (eq dictionary speck-dictionary)
	      (message "Dictionary \"%s\" unchanged" dictionary)
	    (speck-deactivate)
	    (setq speck-dictionary dictionary)
	    (let ((speck-retain-local-variables t))
	      (speck-mode)))
	(setq speck-dictionary dictionary)
	(speck-mode))))
   (t
    ;; With any other argument try association list (may override
    ;; file-local value).
    (let* ((association (assoc arg speck-dictionary-names-alist))
	   (dictionary-name (when association (cdr association)))
	   (dictionary (when dictionary-name (intern dictionary-name)))
	   (dictionary-names
	    (cond
	     ((eq speck-engine 'Aspell)
	      speck-aspell-dictionary-names)
	     ((eq speck-engine 'Ispell)
	      speck-ispell-dictionary-names))))
      (if dictionary
	  (if (member dictionary-name dictionary-names)
	      (if speck-mode
		  (if (eq dictionary speck-dictionary)
		      (message "Dictionary \"%s\" unchanged" dictionary)
		    (speck-deactivate)
		    (setq speck-dictionary dictionary)
		    (let ((speck-retain-local-variables t))
		      (speck-mode)))
		(setq speck-dictionary dictionary)
		(speck-mode))
	    (message "No such dictionary \"%s\"" dictionary-name))
	(message "No association for argument \"%s\"" arg))))))

(defsubst speck-remove-all-properties ()
  (with-buffer-prepared-for-specking
   (speck-remove-property (point-min) (point-max))))

(defun speck-activate ()
  "Activate specking for current buffer."
  (save-restriction
    (widen)
    ;; Remove text properties and overlays (paranoia).
    (speck-remove-all-properties)
    (speck-delete-overlays))
  (unless speck-retain-local-variables
    ;; Confirmation.  We may set this iff it has not been already set to
    ;; avoid that user gets asked twice.
    (unless (local-variable-p 'speck-save-confirmed)
      (setq speck-save-confirmed
	    (or (eq speck-save-ask t) 'undecided)))
    ;; Install hash-table first `speck-restore-words' may fill it
    ;; afterwards.
    (setq speck-hash-table nil)
    ;; Local Words:
    (when (and speck-save-ask speck-save-words) (speck-restore-words))
    ;; Local Options.
    (when (and speck-save-ask speck-save-options) (speck-restore-options)))
  (unless (and (local-variable-p 'speck-dictionary) speck-dictionary)
    (setq speck-dictionary
	  (or  speck-saved-dictionary				; Saved value.
	       (cond
		((eq speck-engine 'Aspell)
		 (intern speck-aspell-default-dictionary-name))
		((eq speck-engine 'Ispell)
		 (intern speck-ispell-default-dictionary-name))))))
  (setq speck-saved-dictionary speck-dictionary)
  ;; Filter-mode.
  (unless (local-variable-p 'speck-filter-mode)
    (setq speck-filter-mode
	  ;; Use either saved or default value. 
	  (or speck-saved-filter-mode speck-filter-mode)))
  (setq speck-saved-filter-mode speck-filter-mode)
  (when (eq speck-filter-mode 'Email)
    ;; Set up regions in Email mode.
    (speck-email-region))
  ;; Inform user.
  (if (eq speck-filter-mode 'None)
      (message "Using dictionary \"%s\"" speck-dictionary)
    (message "Using dictionary \"%s\" and filter \"%s\""
	     speck-dictionary speck-filter-mode))

  (setq speck-process-dictionary-alist nil)
  ;; Start `speck-process'.
  (speck-start-process)

  ;; Set mode variable, is this needed ????
  (set (make-local-variable 'speck-mode) t)
  ;; Add current buffer to speck's buffers.
  (unless (memq (current-buffer) speck-buffer-list)
    (setq speck-buffer-list
	  (cons (current-buffer) speck-buffer-list)))
  ;; Add all windows showing current buffer to speck's windows.
  (dolist (window (get-buffer-window-list (current-buffer) t))
    (speck-window-add window))
  ;; Add buffer-local hooks
  (add-hook 'after-change-functions 'speck-after-change nil t)
  (add-hook 'window-scroll-functions 'speck-window-change nil t)
  (add-hook 'before-revert-hook 'speck-before-revert nil t)
  (add-hook 'after-save-hook 'speck-after-save nil t)
  ;; The following two are mainly useful for deleting speck's processes.
  (add-hook 'change-major-mode-hook 'speck-deactivate nil t)
  (add-hook 'after-change-major-mode-hook 'speck-after-change-major-mode nil t)
  (add-hook 'kill-buffer-hook 'speck-deactivate nil t)
  ;; Add global hooks.  The `window-size-change-functions' stuff should
  ;; be covered by `speck-configuration-change' but leave it in for now.
  (add-hook 'window-size-change-functions 'speck-frame-change)
  (add-hook 'window-configuration-change-hook 'speck-configuration-change)
  (add-hook 'redisplay-end-trigger-functions 'speck-redisplay-end-trigger)
  ;; Activate `speck-delay-timer' - an idle timer called each time Emacs
  ;; has been idle for `speck-delay' seconds.
  (unless speck-delay-timer
    (setq speck-delay-timer
	  (run-with-idle-timer speck-delay t 'speck-windows)))
  ;; Create `speck-pause-timer' - an idle timer called iff Emacs has
  ;; been idle for `speck-pause' seconds.  This timer is activated in
  ;; `speck-windows'.
  (unless speck-pause-timer
    (setq speck-pause-timer (timer-create))
    (timer-set-function speck-pause-timer 'speck-windows '(t)))
  ;; Reset `speck-suspension-list'.
  (setq speck-suspension-list nil))

(defun speck-deactivate ()
  "Deactivate specking for current buffer."
  (when speck-process
    ;; Delete process.  This is the main reason why `speck-deactivate'
    ;; is on `kill-buffer-hook'.
    (speck-delete-process)
    (setq speck-process nil))
  (setq speck-mode nil)
  ;; Remove current buffer from speck's buffers.
  (setq speck-buffer-list (delq (current-buffer) speck-buffer-list))
  ;; Remove all windows showing current buffer from speck's windows.
  (dolist (window (get-buffer-window-list (current-buffer)))
    (speck-window-remove window))
  ;; Remove text properties and overlays.
  (save-restriction
    (widen)
    (speck-remove-all-properties)
    (speck-delete-overlays))
  ;; Remove buffer-local hooks.
  (remove-hook 'after-change-functions 'speck-after-change t)
  (remove-hook 'window-scroll-functions 'speck-window-change t)
  (remove-hook 'before-revert-hook 'speck-before-revert t)
  (remove-hook 'after-save-hook 'speck-after-save t)
  (remove-hook 'change-major-mode-hook 'speck-deactivate t)
  (remove-hook 'after-change-major-mode-hook 'speck-after-change-major-mode t)
  (remove-hook 'kill-buffer-hook 'speck-deactivate t)
  ;; Remove global hooks and timers iff `speck-buffer-list' has become
  ;; empty.
  (unless speck-buffer-list
    ;; Remove global hooks.
    (remove-hook 'window-size-change-functions 'speck-frame-change)
    (remove-hook 'window-configuration-change-hook 'speck-configuration-change)
    (remove-hook 'redisplay-end-trigger-functions 'speck-redisplay-end-trigger)
    ;; Cancel `speck-delay-timer'.
    (when speck-delay-timer
      (cancel-timer speck-delay-timer)
      (setq speck-delay-timer nil))
    ;; Cancel `speck-pause-timer'.
    (when speck-pause-timer
      (cancel-timer speck-pause-timer)
      (setq speck-pause-timer nil)))
  (message "Speck-mode turned off"))

;; _____________________________________________________________________________
;; 										
;;;			      utility functions					
;; _____________________________________________________________________________
;; 										
(defun speck-window-add (window)
  "Add WINDOW to `speck-window-list'."
  (unless (memq window speck-window-list)
    (with-current-buffer (window-buffer window)
      (when speck-mode
	(setq speck-window-list
	      (cons window speck-window-list))
	(force-mode-line-update)))))

(defun speck-window-remove (window)
  "Remove WINDOW from `speck-window-list'."
  (setq speck-window-list
	(delq window speck-window-list))
  (force-mode-line-update))

(defun speck-marker-goto ()
  "Go to `speck-marker' and make it nil."
  (when (and (markerp speck-marker) (marker-position speck-marker)
	     (window-live-p speck-marker-window))
    (select-window speck-marker-window)
    (goto-char speck-marker)
    (set-marker speck-marker nil)))

;; _____________________________________________________________________________
;; 										
;;;				  overlays					
;; _____________________________________________________________________________
;; 										
(defvar speck-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-3] 'speck-mouse-popup-menu)
    map)
  "Speck mouse map.")

(defun speck-make-overlay (from to face)
  "Highlight region from FROM to TO with face FACE."
  (let ((overlay (make-overlay from to)))
    (overlay-put overlay 'specky t)
    (overlay-put overlay 'face face)
    (when (memq face '(speck-guess speck-miss))
      (overlay-put overlay 'keymap speck-overlay-map)
      ;; No help-echo here since I don't know in advance which key
      ;; `speck-mouse-popup-menu' is bound to.
      (overlay-put overlay 'mouse-face 'speck-mouse))
      (setq speck-break t)))

(defun speck-delete-overlays (&optional beg end)
  "Delete all speck overlays overlapping the region."
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (when (< end beg) (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (overlay-recenter end)
    ;; The following is not overl(a)y fast.
    (dolist (overlay (overlays-in beg end))
      (when (overlay-get overlay 'specky)
	(delete-overlay overlay)))))

(defun speck-delete-doublet-overlays (&optional beg end)
  "Delete all speck overlays overlapping the region."
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (when (< end beg) (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (overlay-recenter end)
    ;; The following is not overl(a)y fast.
    (dolist (overlay (overlays-in beg end))
      (when (eq (overlay-get overlay 'face) 'speck-doublet)
	(delete-overlay overlay)))))

(defun speck-overlay-at-point (&optional at faces)
  "Return speck overlay at point.
Optional argument AT non-nil means return overlay at position AT.
Optional argument FACES non-nil means return overlay if and only
if it has a face property in that list."
  (setq at (or at (point)))
  (let ((overlay (cdr (get-char-property-and-overlay at 'specky))))
    (when (or (not faces)
	      (and overlay (memq (overlay-get overlay 'face) faces)))
      overlay)))

(defun speck-next-overlay (&optional arg faces)
  "Get first speck overlay ending after `point'.
Optional argument ARG non-nil means return ARGth overlay after
`point'.  Optional argument FACES non-nil means return overlay if
and only if it has a face property in that list."
  (save-excursion
    (setq arg (or arg 1))
    (let ((overlay (speck-overlay-at-point nil faces)))
      (unless (and overlay
		   (or (= arg 1)
		       (progn
			 (setq arg (1- arg))
			 (goto-char (overlay-end overlay))
			 (setq overlay nil))))
	(save-restriction
	  (narrow-to-region (point) (window-end))
	  (while (and (not overlay) (< (point) (point-max)) (>= arg 0))
	    (goto-char (next-overlay-change (point)))
	    (setq overlay (speck-overlay-at-point nil faces))
	    (when (and overlay (> arg 1))
	      (setq overlay nil)
	      (setq arg (1- arg))))))
      overlay)))

(defun speck-previous-overlay (&optional arg faces)
  "Get first speck overlay starting before `point'.
Optional argument ARG non-nil means return ARGth overlay before
`point'.  Optional argument FACES non-nil means return overlay if
and only if it has a face property in that list."
  (save-excursion
    (setq arg (or arg 1))
    (let ((overlay (speck-overlay-at-point nil faces)))
      (unless (and overlay
		   (or (< (overlay-start overlay) (point))
		       (setq overlay nil))
		   (or (= arg 1)
		       (progn
			 (setq arg (1- arg))
			 (goto-char (overlay-start overlay))
			 (setq overlay nil))))
	(save-restriction
	  (narrow-to-region (window-start) (point))
	  (while (and (not overlay) (> (point) (point-min)) (>= arg 0))
	    (goto-char (previous-overlay-change (point)))
	    (setq overlay (speck-overlay-at-point nil faces))
	    (when (and overlay (> arg 1))
	      (setq overlay nil)
	      (setq arg (1- arg))))))
      overlay)))

;; _____________________________________________________________________________
;; 										
;;;				    hooks					
;; _____________________________________________________________________________
;; 										
(defvar speck-auto-correct-after-change t)

(defun speck-put-speck (from to prop)
  "Assign speck property PROP to text from FROM to TO."
  (put-text-property from to 'speck prop))

(defun speck-get-speck (at)
  "Return speck property at AT."
  (get-text-property at 'speck))

(defun speck-after-change (start end old-len)
  "Speck after a text change.
START, END, and OLD-LEN have the usual meanings."
  (when speck-mode
    (if (> end start)
	;; Insertion.
	(with-buffer-prepared-for-specking
	 (put-text-property start end 'specked 'fresh)
	 (if speck-multi-post-property
	     ;; Assign `speck-multi-post-property' to inserted text.
	     (progn
	       (speck-kill-buffer-add)
	       (if (eq speck-multi-post-property 'default)
		   (remove-text-properties start end '(speck nil))
		 (put-text-property start end 'speck speck-multi-post-property)))
	   (let ((prop
		  ;; Check whether we should inherit speck property from
		  ;; following character.
		  (and speck-self-insert-inherit
		       ;; Inherit for self-insertions only.
		       (eq this-command 'self-insert-command)
		       ;; Inherit iff the last command was not a simple
		       ;; text changing command.
		       (not (memq last-command
				  '(self-insert-command
				    delete-char delete-backward-char
				    backward-delete-char-untabify transpose-chars)))
		       ;; There must be a following character ...
		       (/= end (point-max))
		       ;; ... and it must not be whitespace.
		       (not (memq (char-after end) '(?\  ?\n ?\t)))
		       (or (= start (point-min))
			   (and (eq speck-self-insert-inherit 'line)
				(eq (char-before start) ?\n))
			   (and (eq speck-self-insert-inherit 'white)
				(memq (char-before start) '(?\  ?\n ?\t))))
		       (speck-get-speck end))))
	     (when prop (speck-put-speck start end prop)))))
      ;; Deletion.
      (setq start (max (1- start) (point-min)))
      (setq end (min (1+ end) (point-max)))
      (with-buffer-prepared-for-specking
       (put-text-property start end 'specked 'fresh)))
    (speck-delete-overlays start end)
    (dolist (window (get-buffer-window-list (current-buffer)))
      (speck-window-add window))
    (when speck-auto-correct-after-change
      (speck-auto-correct-after-change))))

(defun speck-window-change (window start)
  "Speck after WINDOW changes, START is ignored."
  (speck-window-add window))

(defun speck-frame-change (frame)
  "Speck after FRAME changes."
  (dolist (window (window-list frame))
    (speck-window-add window)))

(defun speck-configuration-change ()
  "Speck after configuration changes."
  (dolist (window (window-list (selected-frame)))
    (speck-window-add window)))

(defun speck-redisplay-end-trigger (window start)
  "Speck after redisplay end triggers on WINDOW.  START is ignored."
  (speck-window-add window))

(defvar speck-reverting nil
  "Non-nil when buffer is reverted.")
;; We make this permanently buffer-local to assure that any error that
;; might occur during `revert-buffer' dropping the call for
;; `speck-after-revert' and thus not resetting this variable affects
;; other buffers.
(make-variable-buffer-local 'speck-reverting)
(put 'speck-reverting 'permanent-local t)

(defun speck-before-revert ()
  "Set `speck-reverting' before reverting buffer."
  (when speck-mode
    (add-hook 'after-revert-hook 'speck-after-revert t t)
    ;; `speck-reverting' is permanently buffer-local.
    (setq speck-reverting t)))

(defun speck-after-revert ()
  "Reset `speck-reverting' after reverting buffer."
  (remove-hook 'after-revert-hook 'speck-after-revert t)
  ;; `speck-reverting' is permanently buffer-local.
  (setq speck-reverting nil))

(defun speck-after-save ()
  "Function run by `after-save-hook'.
- Remove `fresh' specked properties from buffer.  Needed to avoid
  surprising behavior of `speck-auto-correct-case'.

- Remove `speck-kill-buffer-query' for this buffer since the
  local words were saved anyway when saving the buffer."
  (with-buffer-prepared-for-specking
   ;; Remove 'fresh properties from current buffer.
   (save-excursion
     (save-restriction
       (widen)
       (let (from)
	 (while (setq from (text-property-any
			    (point) (point-max) 'specked 'fresh))
	   (goto-char (next-single-property-change
		       from 'specked nil (point-max)))
	   (speck-remove-property from (point)))))))
  ;; Reset `speck-kill-buffer-query'.
  (when speck-kill-buffer-query ; When non-nil this is buffer-local.
    (setq speck-kill-buffer-query nil)
    (setq speck-kill-buffer-query-list
	  (delq (current-buffer) speck-kill-buffer-query-list))))

;; _____________________________________________________________________________
;; 										
;;;				 hash tables					
;; _____________________________________________________________________________
;; 										

;; For each key (which is actually a word rejected by the spell-checking
;; process) we store one of the following informations:
;; nil - buffer-local, do not write ("accept for session")
;; t - file-local, do write ("accept for file")
;; a list of symbols (dictionaries) - text-local, do write with dictionary
;; ("accept for file with dictionary")

(defun speck-hash-get (word)
  (when speck-hash-table
    (gethash word speck-hash-table)))

(defun speck-hash-restore (word value)
  "Restore hash table when reading in Local Words."
  (unless speck-hash-table
    (setq speck-hash-table (make-hash-table :test 'equal)))
  (let ((hash (speck-hash-get word)))
    (cond
     ((eq value 'buffer)
      ;; A buffer value can't override anything.
      (unless hash (puthash word value speck-hash-table)))
     ((eq value 'file)
      ;; A `file' value overrides anything else.
      (puthash word value speck-hash-table))
     ((consp hash)
      ;; We have a dictionary entry, add new one (intern `value').
      (puthash word (cons (intern value) hash) speck-hash-table))
     (t
      ;; Make this the only entry (intern `value').
      (puthash word (list (intern value)) speck-hash-table)))))

(defun speck-hash-put (word value)
  "Add word to `speck-hash-table' and maybe save it to file."
  (unless speck-hash-table
    (setq speck-hash-table (make-hash-table :test 'equal)))
  (let ((hash (speck-hash-get word)))
    (when (and (not (eq value 'buffer))
	       speck-save-ask speck-save-words speck-save-permanent)
      ;; Make sure a word section is written before buffer is killed.
      (speck-kill-buffer-add))
    (cond
     ((eq value 'buffer)
      ;; A buffer value can't override anything.
      (unless hash (puthash word value speck-hash-table)))
     ((eq value 'file)
      ;; A `file' value overrides anything else.
      (puthash word value speck-hash-table)
      (speck-save-word word))
     ((consp hash)
      ;; We already have a dictionary entry, add new one.
      (puthash word (cons value hash) speck-hash-table)
      (speck-save-word word value))
     (t
      ;; Make a new dictionary entry.
      (puthash word (list value) speck-hash-table)
      (speck-save-word word value)))))

;; _____________________________________________________________________________
;; 										
;;;			     process management					
;; _____________________________________________________________________________
;; 										
(defun speck-start-process ()
  (cond
   ((eq speck-engine 'Aspell)
    (if (speck-aspell-executable-p)
	(speck-aspell-start-process)
      (speck-mode -1)
      (error "Aspell not executable")))
   ((eq speck-engine 'Ispell)
    (if (speck-ispell-executable-p)
	(speck-ispell-start-process)
      (speck-mode -1)
      (error "Ispell not executable")))
   (t
    ;; Turn specking off.
    (speck-mode -1)
    (error "Invalid default checker"))))

(defun speck-aspell-start-process ()
  "Start Aspell process."
  ;; `speck-dictionary' must be a symbol denoting a valid dictionary.
  (let* ((dictionary-name (symbol-name speck-dictionary))
	 ;; The first two characters are the language code, the
	 ;; remainder are regions, accents, ...
	 (entry (assoc (substring dictionary-name 0 2)
		       speck-aspell-language-options))
	 (coding-system
	  ;; A value specified in `speck-aspell-coding-system' overrides
	  ;; everything else.  Note: Most problems in our communcation
	  ;; with Aspell will stem from what we set up here.
	  (or speck-aspell-coding-system
	      (when entry
		(if (eq (nth 1 entry) t)
		    ;; Obtain coding system from Aspell.
		    (speck-aspell-charset entry)
		  ;; Obtain coding system from option.
		  (nth 1 entry)))))
	 (minimum-word-length
	  ;; A value specified in `speck-aspell-minimum-word-length'
	  ;; overrides anything else.
	  (or speck-aspell-minimum-word-length
	      (when entry (nth 2 entry))))
	 (run-together
	  ;; A value specified in `speck-aspell-maximum-run-together' overrides
	  ;; anything else.
	  (or speck-aspell-maximum-run-together
	      (when entry (nth 3 entry))))
	 (extra-arguments (when entry (nth 4 entry)))
	 (arguments
	  (append
	   ;; Pipe option and dictionary-name.
	   (list "-a" "-d" dictionary-name)
	   ;; Coding system.
	   (when coding-system
	     (list (concat "--encoding=" (symbol-name coding-system))))
	   ;; Minimum word length.
	   (when minimum-word-length
	     (list (concat "--ignore=" (number-to-string minimum-word-length))))
	   ;; Run-together words.
	   (if run-together (list "-C") (list "-B"))
	   (when (numberp run-together)
	     (list (concat "--run-together-limit=" (number-to-string run-together))))
	   ;; Filter-mode, aspell wants it downcased.
	   (unless (eq speck-filter-mode 'URL)
	     (list (concat "--mode=" (downcase (symbol-name speck-filter-mode)))))

	   ;; The following code doesn't work on my system when multiple
	   ;; dictionaries are involved, for some strange reason Emacs decides
	   ;; to set buffer-file-name to nil for the second call and Aspell
	   ;; seems to get confused.  
	   ;; 	   (cond
	   ;; 	    ((and (eq speck-aspell-home-dir t)
	   ;; 		  (let* ((dir-name
	   ;; 			  (when buffer-file-name
	   ;; 			    (directory-file-name
	   ;; 			     (file-name-directory buffer-file-name)))))
	   ;; 		    (when dir-name
	   ;; 		      (list "--home-dir"
	   ;; 			    (convert-standard-filename dir-name))))))

	   ;; Specify directory where personal word lists reside.
	   (when speck-aspell-home-dir
	     (list "--home-dir"
		   (convert-standard-filename speck-aspell-home-dir)))
	   ;; The name of the personal word list (dictionary file).
	   (when speck-personal-dictionary-file
	     (list "--personal" speck-personal-dictionary-file))
	   ;; The following is the Aspell standard, hence it's commented out.
	   ;; 	   (list "--personal" (concat dictionary-name ".pws")))
	   (when speck-aspell-suggestion-mode
	     (list (concat "--sug-mode=" speck-aspell-suggestion-mode)))
	   extra-arguments
	   speck-aspell-extra-arguments))
	 (process (rassoc arguments speck-process-argument-alist))
	 ;; An entry should exist when a process exists, but be paranoid here.
	 (entry (when process (assq (car process) speck-process-buffer-alist)))
	 process-connection-type)
    (if (and process entry)
	;; Process and entry exist.
	(progn
	  (setq speck-process (car process))
	  (setcdr entry (cons (current-buffer) (cdr entry))))
      ;; No suitable process exists.
     (let ((default-directory
	     (if (and (file-directory-p default-directory)
		      (file-readable-p default-directory))
		 ;; Defend against bad `default-directory'.
		 default-directory
	       (expand-file-name "~/"))))
       (setq speck-process
	     (apply 'start-process "speck"
		    (generate-new-buffer " *speck-process-buffer*")
		    speck-aspell-program arguments)))
      (setq speck-process-buffer-alist
	    (cons (cons speck-process (list (current-buffer)))
		  speck-process-buffer-alist))
      (setq speck-process-argument-alist
	    (cons (cons speck-process arguments)
		  speck-process-argument-alist))
      (setq speck-process-dictionary-alist
	    ;; Buffer local.
	    (cons (cons speck-process speck-dictionary)
		  speck-process-dictionary-alist))
      (set-process-query-on-exit-flag speck-process nil)
      (when coding-system
	(set-process-coding-system
	 speck-process coding-system coding-system)))))

(defun speck-delete-process ()
  "Delete any `speck-process' associated with current buffer.
Do not delete such a process if another buffer still needs it."
  (dolist (buffer-entry speck-process-buffer-alist)
    (setcdr buffer-entry (delq (current-buffer) (cdr buffer-entry)))
    (unless (cdr buffer-entry)
      (setq speck-process-buffer-alist
	    (assq-delete-all
	     (car buffer-entry) speck-process-buffer-alist))
      (setq speck-process-argument-alist
	    (assq-delete-all
	     (car buffer-entry) speck-process-argument-alist))
      ;; Kill process-buffer.
      (kill-buffer (process-buffer (car buffer-entry)))
      ;; Likely kill-buffer should have done that already:
      (delete-process (car buffer-entry))))
  (setq speck-process-dictionary-alist nil)
  (setq speck-process nil))

(defun speck-chunk ()
  "Send a line-like object to `speck-process'."
  (let* ((process speck-process)
	 (process-buffer (process-buffer speck-process))
	 (string
	  (concat "^" (buffer-substring-no-properties
		       (point-min) (point-max))
		  "\n"))
	 (bol (point-min))
	 (eol (point-max))
	 (old bol)
	 at length from to face hash)
    (speck-delete-overlays (point-min) (point-max))
    (with-current-buffer process-buffer
      ;; Erasing the buffer does not give any guarantee that process
      ;; feeds us with some stale information, hence let's accept some
      ;; output before.  But this 0.01 should be made customizable.
      (accept-process-output speck-process 0.01)
      (erase-buffer)
      (process-send-string process "!\n")
      (process-send-string process string)
      (while (and (not (speck-stop))
		  (progn
		    (accept-process-output process 0.01)
		    (goto-char (point-max))
		    ;; Aspell appends an empty line, wait till it's
		    ;; here.
		    (not (looking-back "^\n")))))
      (goto-char (point-min)))
    (while (and (not (speck-stop))
		(with-current-buffer process-buffer
		  (and (re-search-forward "\\(^& \\)\\|\\(^# \\)" nil t)
		       (cond
			((match-beginning 1)	; &
			 (setq length (skip-chars-forward "^ "))
			 (forward-char)
			 (re-search-forward " " nil t)
			 (setq at (point))
			 (re-search-forward ":" nil t)
			 (setq from (+ (string-to-number
					(buffer-substring-no-properties
					 at (1- (point))))
				       bol -1))
			 ;; The `eol' stuff should work around Ispell
			 ;; failing to report our positions and
			 ;; lengths when coding-systems mismatch.
			 (setq to (min (+ from length) eol))
			 (setq face 'speck-guess))
			((match-beginning 2)	; #
			 (setq length (skip-chars-forward "^ "))
			 (forward-char)
			 (setq at (point))
			 ;; Stop before space or newline character.
			 (skip-chars-forward "^ \n")
			 (setq from (+ (string-to-number
					(buffer-substring-no-properties
					 at (point)))
				       bol -1))
			 ;; See above.
			 (setq to (min (+ from length) eol))
			 (setq face 'speck-miss))))))
      (cond
       ((and speck-face-inhibit-list
	     (let ((faces (get-text-property from 'face)))
	       ;; Inhibit specking this word if (one of) its face(s)
	       ;; at the first char is in `speck-face-inhibit-list'.
	       (cond
		((not faces)
		 nil)
		((listp faces)
		 ;; We have a list of face properties.
		 (catch 'found
		   (dolist (face faces)
		     (when (memq face speck-face-inhibit-list)
		       (throw 'found t)))))
		(t				; atom
		 (memq faces speck-face-inhibit-list)))))
	(speck-put-specked old to))
       ((and (eq speck-filter-mode 'Email)
	     speck-email-citations
	     (if (facep speck-email-citations)
		 ;; Face.
		 (let ((faces (get-text-property from 'face)))
		   ;; Inhibit specking this word if (one of) its
		   ;; face(s) at the first char is in
		   ;; `speck-face-inhibit-list'.
		   (cond
		    ((not faces)
		     nil)
		    ((listp faces)
		     ;; We have a list of face properties.
		     (catch 'found
		       (dolist (face faces)
			 (when (eq face speck-email-citations)
			   (throw 'found t)))))
		    (t				; atom
		     (eq faces speck-email-citations))))
	       ;; Regexp
	       (save-excursion
		 (save-restriction
		   (widen)
		   (beginning-of-line)
		   (looking-at speck-email-citations)))))
	(speck-put-property old to))
       ((and speck-hash-table
	     (setq hash (speck-hash-get
			 (buffer-substring-no-properties from to)))
	     (or (memq hash '(buffer file))
		 (if (speck-get-speck from)
		     (memq (speck-get-speck from) hash)
		   (memq speck-dictionary hash))))
	;; Don't put overlay if the word is in the hash table.
	(speck-put-specked old to))
       ((and (eq (current-buffer) speck-nospeck-buffer)
	     (<= from speck-nospeck-at)
	     (<= speck-nospeck-at to))
	;; We are not allowed to put an overlay here
	(speck-put-specked old from)
	(speck-remove-property from to))
       ((and speck-auto-correct-case
	     ;; Proceed iff we have guesses.
	     (eq face 'speck-guess)
	     (text-property-any from to 'specked 'fresh)
	     (speck-auto-correct-case
	      old from to (buffer-substring-no-properties from to))))
       (t
	;; Put property and overlay.
	(speck-put-specked old to from)
	(speck-make-overlay from to face)))
      ;; The following is a pain, needed to handle Ispell's failure to
      ;; get coding-system.
      (setq old to))
    (unless speck-stop
      (speck-put-specked old (point-max)))))

(defun speck-word (from to word &optional multi)
  "Send a word-like object to `speck-process' and return list of guesses."
  (let* ((dictionary (when multi (speck-get-speck from)))
	 (process
	  (if dictionary
	      (car (rassq dictionary speck-process-dictionary-alist))
	    speck-process))
	 guesses)
    (unless process
      ;; Should not occur.
      (let ((speck-dictionary dictionary)
	    speck-process)
	(speck-start-process)
	(setq process speck-process)))
    (with-current-buffer (process-buffer process)
      (erase-buffer)
      (process-send-string process (concat "^" word "\n"))
      (while (and (not quit-flag)
		  (progn
		    (accept-process-output process 0.01)
		    (goto-char (point-max))
		    ;; Aspell appends an empty line, wait till it's here.
		    (not (looking-back "^\n")))))
      (goto-char (point-min))
      (when (and (re-search-forward "^& " nil t)
		 (not (zerop (skip-chars-forward "^ ")))
		 (re-search-forward ": "))
	(while (re-search-forward "\\(.*?\\)\\(?:, \\|\n\n\\)" nil t)
	  (setq guesses (cons (match-string-no-properties 1) guesses)))
	(when guesses (nreverse guesses))))))

(defun speck-send-replacement (misspelled replacement)
  "Tell Aspell that MISSPELLED should be spelled REPLACEMENT."
  ;; This doesn't seem to work in spite of the fact that I ask Aspell to
  ;; save the dictionary afterwards.
  (process-send-string
   speck-process (concat "$$ra " misspelled "," replacement "\n"))
  (process-send-string speck-process "#\n"))

;; _____________________________________________________________________________
;; 										
;;;				   windows					
;; _____________________________________________________________________________
;; 										
(defsubst speck-stop ()
  (or speck-stop (setq speck-stop (input-pending-p))))

(defun speck-respeck (delay)
  "Speck again after DELAY seconds."
  (timer-set-idle-time speck-pause-timer (current-idle-time))
  (timer-inc-time speck-pause-timer (or delay 0))
  (timer-activate-when-idle speck-pause-timer t))

(defun speck-desuspend ()
  "De-suspend all suspended window.
This function must be executed by `pre-command-hook'."
  (setq speck-suspension-list nil)
  (remove-hook 'pre-command-hook 'speck-desuspend))

(defun speck-windows (&optional pause)
  "Speck windows on `speck-window-list'.
Works correctly if and only if the optional argument PAUSE is nil
when triggered by `speck-delay-timer' and non-nil when triggered
by `speck-pause-timer'."
  (unless pause
    ;; When `pause' is nil cancel `speck-pause-timer' (in pathological
    ;; cases this might interfere with the current call).
    (cancel-timer speck-pause-timer))
  ;; Continue iff there's something to speck and we don't interfere with
  ;; more important activities.
  (cond
   ((or (input-pending-p)	  ; (active-minibuffer-window) ; do we ned this?
	executing-kbd-macro defining-kbd-macro)
    ;; Pause by `speck-delay' seconds (maybe the list above should be
    ;; extended).
    (speck-respeck speck-delay))
   ;; Test selected window first.
   ((and (not (memq (selected-window) speck-suspension-list))
	 ;; Do not reset `speck-nospeck-buffer' as long as the selected
	 ;; window is suspended.
	 (progn
	   (setq speck-nospeck-buffer nil)
	   (memq (selected-window) speck-window-list))
	 (or (let ((buffer (window-buffer)))
	       (and (local-variable-p 'speck-mode buffer)
		    (buffer-local-value 'speck-mode buffer)))
	     ;; The selected window is not suitable for specking, remove
	     ;; it from `speck-window-list' (could it ever get there?).
	     (and (speck-window-remove (selected-window)) nil)))
    ;; Auto correction, must be improved, currently speck-fresh is never
    ;; reset.
    ;;     (when speck-fresh (speck-auto-correct))
    (with-buffer-prepared-for-specking
     ;; The selected window is suitable for specking, test for nospeck area.
     (when (and speck-process (not (process-get speck-process 'preempted)))
       (let (minibuffer-auto-raise message-log-max)
	 (save-excursion
	   (speck-window t))))))
   ((let ((window
	   (catch 'found
	     ;; Scan `speck-window-list'
	     (dolist (window speck-window-list)
	       (unless (memq window speck-suspension-list)
		 (if (and (window-live-p window)
			  (let ((buffer (window-buffer window)))
			    (and (local-variable-p 'speck-mode buffer)
				 (buffer-local-value 'speck-mode buffer))))
		     ;; `window' is suitable for specking, return it.
		     (throw 'found window)
		   ;; `window' is not suitable for specking, remove it
		   ;; from `speck-window-list'.
		   (speck-window-remove window)))))))
      ;; Speck `window'.
      (with-selected-window window
	;; Do this after we selected the window and thus implicitly made
	;; its buffer current.
	(when (and speck-process (not (process-get speck-process 'preempted))) 
	  (with-buffer-prepared-for-specking
	   (let (minibuffer-auto-raise message-log-max)
	     (save-excursion
	       (speck-window)))))))))
  (when speck-window-list
    ;; Pause by `speck-pause' seconds.
    (speck-respeck speck-pause)))

(defun speck-window (&optional selected)
  "Speck selected window.
Optional argument SELECTED is non-nil when selected window is the
window actually selected by the user.

This function must be called `with-buffer-prepared-for-specking'
and within a `save-excursion'."
  (let ((at (window-point))
	(window-start (window-start))
	(window-end (window-end))
	line-start line-end)
    ;; Note: `selected' => `speck-nospeck-buffer' is either nil or
    ;; `current-buffer'.
    (setq speck-stop nil)
    (setq speck-break nil)
    (setq speck-ppss-at nil)
    ;; In user-selected window try to speck line around `at'.
    (when (and selected (not (speck-stop)))
      (if (bolp)
	  ;; When at `bolp' speck last chunk on preceding line.
	  (forward-line -1)
	;; Speck last chunk on present line.
	(when (or (eq speck-chunk-at-point t)
		  (and (eq speck-chunk-at-point 'commands)
		       (memq last-command speck-chunk-at-point-commands)))
	  (setq speck-nospeck-buffer (current-buffer))
	  (setq speck-nospeck-at (window-point))))
      (save-excursion
	(save-restriction
	  (let (temp)
	    (narrow-to-region
	     (progn
	       (skip-chars-backward " \t")
	       (setq temp (point))
	       (skip-chars-backward "^ \n\t\f")
	       (point))
	     (progn
	       (goto-char temp)
	       (skip-chars-forward "^\n\t\f")
	       (point)))
	    (goto-char (point-min))
	    ;; `speck-chunks' would be more intuitive here but we would
	    ;; have to set up things for that, hence stick to
	    ;; `speck-line'.
	    (speck-line))))
      (when (and speck-nospeck-buffer
		 (or speck-stop
		     (and (or (= speck-nospeck-at window-start)
			      (eq (get-text-property
				   (1- speck-nospeck-at) 'specked) t))
			  (or (= speck-nospeck-at window-end)
			      (eq (get-text-property
				   speck-nospeck-at 'specked) t)))))
	;; Reset this, we won't need it in this round.
	(setq speck-nospeck-buffer nil)))
    ;; Reset again.
    (setq speck-ppss-at nil)
    ;; Speck window.
    (unless (or (speck-stop) speck-break)
      (save-restriction
	(narrow-to-region window-start window-end)
	(goto-char (point-min))
	(while (and (not (speck-stop)) (not (eobp)))
	  (speck-line))))
    (unless (or speck-stop speck-break)
      (if (eq (current-buffer) speck-nospeck-buffer)
	  (progn
	    ;; Suspend specking this window.
	    (setq speck-suspension-list
		  (cons (selected-window) speck-suspension-list))
	    (add-hook 'pre-command-hook 'speck-desuspend))
	;; Nothing to speck in this window, remove it from
	;; `speck-window-list'.
	(speck-window-remove (selected-window))))))

(defun speck-chunks ()
  "Process a contiguous set of chunks."
  (let (old)
    (goto-char (point-min))
    (while (and (not (speck-stop)) (not (eobp)))
      (setq old (point))
      (skip-chars-forward " \t")
      (speck-put-property old (point))
      (unless (eobp)
	(save-restriction
	  (narrow-to-region
	   (point)
	   (progn
	     (skip-chars-forward "^ \t")
	     (point)))
	  ;; We have isolated a chunk.
	  (if speck-multi
	      (speck-multi-chunk)
	    (speck-chunk)))))))

(defun speck-multi-chunk ()
  "Process a chunk possibly composed from multi language chunks."
  (goto-char (point-min))
  (while (not (eobp))
    (save-restriction
      (narrow-to-region
       (point)
       (next-single-property-change (point) 'speck nil (point-max)))
      ;; Check language.
      (let* ((dictionary (speck-get-speck (point-min)))
	     (speck-process
	      (if dictionary
		  (if (memq dictionary '(-- ==))
		      'ignore
		    (car (rassq dictionary speck-process-dictionary-alist)))
		speck-process))
	     (speck-dictionary (or dictionary speck-dictionary)))
	(cond
	 ((eq speck-process 'ignore)
	  (speck-put-property (point-min) (point-max)))
	 (speck-process
	  (speck-chunk))
	 ;; Using `member' seems ugly.  But the following check is
	 ;; needed only for the first time the property is encountered
	 ;; in a buffer.
	 ((member (symbol-name speck-dictionary)
		  (cond
		   ((eq speck-engine 'Aspell)
		    speck-aspell-dictionary-names)
		   ((eq speck-engine 'Ispell)
		    speck-ispell-dictionary-names)))
	  (speck-start-process)
	  (speck-chunk))
	 ;; This check is needed more often, but it should be
	 ;; exceptional using an uninstalled dictionary.
	 ((member (symbol-name speck-dictionary)
		  (cond
		   ((eq speck-engine 'Aspell)
		    speck-aspell-non-dictionary-names)
		   ((eq speck-engine 'Ispell)
		    speck-ispell-non-dictionary-names)))
	  ;; We already know that this dictionary doesn't exist.
	  (speck-put-property (point-min) (point-max)))
	 (t
	  ;; We encounter this dictionary for the first time, it's
	  ;; probably from an imported file and we have to warn the user
	  ;; that the dictionary doesn't exist.
	  (speck-put-property (point-min) (point-max))
	  (cond
	   ((eq speck-engine 'Aspell)
	    (setq speck-aspell-non-dictionary-names
		  (cons (symbol-name speck-dictionary)
			speck-aspell-non-dictionary-names)))
	   ((eq speck-engine 'Ispell)
	    (setq speck-ispell-non-dictionary-names
		(cons (symbol-name speck-dictionary)
		      speck-ispell-non-dictionary-names))))
	  (save-restriction
	    (widen)
	    ;; Should become an error, maybe ... On the other hand we
	    ;; might want to continue checking the rest of the text if
	    ;; it has a valid dictionary.
	    (message "No such dictionary \"%s\"" speck-dictionary)
	    (ding) (sit-for 3)))))
      (goto-char (point-max)))))

(defun speck-line ()
  "Speck line."
  (interactive)
  (let (old)
    (unless (eolp)
      (save-restriction
	(narrow-to-region (line-beginning-position) (line-end-position))
	(goto-char (point-min))
	(while (and (not (speck-stop))
		    (setq old (text-property-not-all
			       (point) (point-max) 'specked t)))
	  (goto-char old)
	  (save-restriction
	    ;; Narrow down to chunk around `old'.
	    (narrow-to-region
	     (progn
	       (skip-chars-backward "^ \t") (point))
	     (progn
	       (goto-char
		(or (text-property-any old (point-max) 'specked t)
		    (point-max)))
	       (skip-chars-forward "^ \t") (point)))
	    (cond
	     ((save-excursion
		(goto-char (point-min))
		(skip-chars-forward " \t")
		(eobp))
	      ;; Only whitespace here.
	      (speck-put-property (point-min) (point-max)))
	     (speck-syntactic
	      ;; Speck comments and/or strings only.
	      (goto-char (point-min))
	      (save-restriction
		(widen)
		(if speck-ppss-at
		    ;; Parse from `speck-ppss-at'.
		    (setq speck-ppss
			  (parse-partial-sexp
			   speck-ppss-at (point) nil nil speck-ppss))
		  ;; Use `syntax-ppss'.
		  (setq speck-ppss (syntax-ppss))))
	      (setq speck-ppss-at (point))
	      (unless (or (and (nth 3 speck-ppss)
			       (memq speck-syntactic '(strings t)))
			  (and (nth 4 speck-ppss)
			       (memq speck-syntactic '(comments t))))
		;; Find end of comment or string.
		(setq speck-ppss
		      (parse-partial-sexp
		       speck-ppss-at (point-max) nil nil
		       speck-ppss 'syntax-table))
		(setq speck-ppss-at (point))
		(speck-put-property old (point)))
	      (while (and (not (speck-stop)) (not (eobp)))
		(save-restriction
		  (let ((in-string (nth 3 speck-ppss))
			(in-comment (nth 4 speck-ppss)))
		    (narrow-to-region
		     speck-ppss-at
		     (progn
		       (setq speck-ppss
			     (parse-partial-sexp
			      speck-ppss-at (point-max) nil nil
			      speck-ppss 'syntax-table))
		       (setq speck-ppss-at (point))))
		    (if (or (and in-string
				 (memq speck-syntactic '(strings t)))
			    (and in-comment
				 (memq speck-syntactic '(comments t))))
			(speck-chunks)
		      (speck-put-property (point-min) (point-max)))))
		(setq old (point))
		(setq speck-ppss
		      (parse-partial-sexp
		       speck-ppss-at (point-max) nil nil
		       speck-ppss 'syntax-table))
		(setq speck-ppss-at (point))
		(unless speck-stop
		  (speck-put-property old (point)))))
	     ;; Speck entire line.
	     (t (speck-chunks)))
	    (goto-char (point-max))))))
    (setq old (point))
    (forward-line)
    ;; Speck newline.
    (unless speck-stop
      (speck-put-property old (point)))))

(defsubst speck-put-property (from to)
  (put-text-property from to 'specked t))

(defsubst speck-remove-property (from to)
  (remove-text-properties from to '(specked nil)))

;; The following three items stolen from ps-print.el.
(defalias 'speck-jitify 'jit-lock-fontify-now)
(defalias 'speck-lazify 'lazy-lock-fontify-region)

;; Stefan's idea of doing this.
(defsubst speck-ensure-fontified (start end)
  (cond
   ((and (boundp 'jit-lock-mode) (symbol-value 'jit-lock-mode))
    (speck-jitify start end))
   ((and (boundp 'lazy-lock-mode) (symbol-value 'lazy-lock-mode))
    (speck-lazify start end))))

(defun speck-syntactic-p ()
  "Return t when character at `point' may be syntactically checked."
  (and (or (not speck-syntactic)
	   (let ((parse-state (syntax-ppss)))
	     (or (and (nth 3 parse-state)
		      (memq speck-syntactic '(strings t)))
		 (and (nth 4 parse-state)
		      (memq speck-syntactic '(comments t))))))
       (or (not speck-face-inhibit-list)
	   (progn
	     (unless (get-text-property (point) 'fontified)
	       (speck-ensure-fontified
		(line-beginning-position) (line-end-position))
	     nil))
	   (let ((faces (get-text-property (point) 'face)))
	     ;; Inhibit specking this word if (one of) its face(s) is in
	     ;; `speck-face-inhibit-list'.
	     (cond
	      ((not faces))
	      ((listp faces)
	       ;; We have a list of face properties.
	       (catch 'found
		 (dolist (face faces t)
		   (when (memq face speck-face-inhibit-list)
			 (throw 'found nil)))))
	      (t ; Atom.
	       (not (memq faces speck-face-inhibit-list))))))))

(defun speck-put-specked (old to &optional from)
  "Put `specked' property from OLD to TO.
Optional argument FROM means there is a misspelled word from FROM
till TO."
  ;; Must be called `with-buffer-prepared-for-specking'.
  (if speck-doublets
    (let ((start old)
	  (end to)
	  too prev prev-from prev-to next next-from next-to
	  nospeck-from nospeck-to)
      (save-excursion
	(save-restriction
	  (widen)
	  ;; Adjust region around `old'.
	  (goto-char old)
	  (when (and (skip-chars-forward " \t\n\f")
		     (not (zerop (skip-chars-backward " \t\n\f")))
		     (setq too (point))
		     ;; We consider symbol syntax only.
		     (not (zerop (skip-syntax-backward "w_")))
		     (not (text-property-not-all (point) old 'specked t))
		     (not (get-char-property (point) 'specky))
		     (= (next-single-char-property-change
			 (point) 'specky nil too) too))
	    (setq start (point)))
	  ;; Adjust region around `to'.
	  (goto-char to)
	  (when (and (not (zerop (skip-chars-forward " \t\n\f")))
		     (not (zerop (skip-syntax-forward "w_"))))
	    (if from
		(progn
		  (unless (or (text-property-not-all to (point) 'specked t)
			      (zerop (skip-chars-forward " \t\n\f")) (eobp)
			      (progn
				(setq too (point))
				(zerop (skip-syntax-forward "w_"))))
		    ;; Remove any doublet overlays here.
		    (speck-delete-doublet-overlays too (point)))
		  (goto-char from)
		  (skip-chars-backward " \t\n\f")
		  (setq end (point)))
	      (setq end (point))))
	  ;; Now scan region from `start' till `end'.
	  (speck-delete-doublet-overlays old end)
	  (narrow-to-region start end)
	  (goto-char start)
	  ;; A doublet _must_ start with a word character.
	  (skip-syntax-forward "^w")
	  (setq prev-from (point))
	  (skip-syntax-forward "w_")
	  (setq prev-to (point))
	  (setq prev (buffer-substring-no-properties prev-from prev-to))
	  (while (not (eobp))
	    (if (and (not (string-equal prev ""))
		     (not (zerop (skip-chars-forward " \t\n\f")))
		     (setq next-from (point))
		     (skip-syntax-forward "w_")
		     (setq next-to (point))
		     (setq next (buffer-substring-no-properties
				 next-from next-to))
		     (not (string-equal next "")))
		(progn
		  (when (and (string-equal prev next)
			     (or (not (eq (current-buffer) speck-nospeck-buffer))
				 (> next-from speck-nospeck-at)
				 (< next-to speck-nospeck-at)
				 (progn
				   (setq nospeck-from next-from)
				   (setq nospeck-to next-to)
				   nil)))
		    (speck-make-overlay next-from next-to 'speck-doublet))
		  (setq prev-from next-from)
		  (setq prev-to next-to)
		  (setq prev next))
	      (skip-syntax-forward "^w")
	      (setq prev-from (point))
	      (skip-syntax-forward "w_")
	      (setq prev-to (point))
	      (setq prev (buffer-substring-no-properties prev-from prev-to))))))
      (if nospeck-from
	  (progn
	    (speck-put-property old nospeck-from)
	    (speck-put-property nospeck-to to))
	(speck-put-property old to)))
    ;; Put the text property.
    (speck-put-property old to)))

;; _____________________________________________________________________________
;; 										
;;;				adding words					
;; _____________________________________________________________________________
;; 										
(defun speck-add-cleanup (overlay from to word)
  "Cleanup after WORD has been added.
OVERLAY is the overlay covering WORD, FROM and TO its boundaries."
  (with-buffer-prepared-for-specking
   (delete-overlay overlay)
   (speck-remove-property from to)
   (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       ;; Should we relocate all overlays here?
       (unless (get-char-property (point) 'specky)
	 (goto-char (or (next-single-char-property-change (point) 'specky)
			(point-max)))
	 (let (property)
	   (while (not (eobp))
	     (setq from (point))
	     (setq to (or (next-single-char-property-change from 'specky)
			  (point-max)))
	     (setq overlay (cdr (get-char-property-and-overlay from 'specky)))
	     (when (and overlay
			(string-equal (buffer-substring-no-properties from to)
				      word))
	       (delete-overlay overlay)
	       (speck-remove-property from to))
	     (goto-char (or (next-single-char-property-change (point) 'specky)
			    (point-max)))))))))
  ;; Add buffer to speck's windows.
  (dolist (window (get-buffer-window-list (current-buffer)))
    (speck-window-add window)))

(defun speck-add-word (overlay)
  "Add word covered by OVERLAY to dictionary or list."
  (interactive)
  (let* ((from (overlay-start overlay))
	 (to (overlay-end overlay))
	 (word (buffer-substring-no-properties from to))
	 (face (overlay-get overlay 'face))
	 (hash (speck-hash-get word))
	 ;; We need the correct personal word list (pws) and thus have
	 ;; to find the Aspell process adminstrating it.  It's not
	 ;; entirely clear what happens when two processes
	 ;; intermittently modify the same pws (there's been some
	 ;; discussion on thread safety) but our approach of sharing one
	 ;; process for all regions with the same dictionary should
	 ;; render this less dramatic within one and the same Emacs
	 ;; session.
	 (dictionary (speck-get-speck from))
	 (speck-process
	  (if dictionary
	      (car (rassq dictionary speck-process-dictionary-alist))
	    speck-process))
	 (speck-dictionary (or dictionary speck-dictionary)))
    (when overlay
      (overlay-put overlay 'face 'speck-query)
      (message (concat
		(format "\"p\" add `%s' personally" word)
		(unless (string-equal word (downcase word))
		  (format " (\"l\" adds `%s')" (downcase word)))
		;; Suppress the following when an entry already exists.
		(unless hash (format ", \"b\" add for buffer"))
		(when (and speck-save-confirmed ; may be yet 'undecided
			   speck-save-words buffer-file-name)
		  (format ", \"f\" file, \"d\" dictionary \"%s\""
			  (or dictionary speck-dictionary)))))
      (unwind-protect
	  (let* ((char (read-event))
		 (key (vector char))
		 (case-fold-search t))
	    (cond
	     ((and (integerp char)
		   (or (char-equal char ?p) (char-equal char ?*)))
	      (process-send-string speck-process (concat "*" word "\n"))
	      (process-send-string speck-process "#\n")
	      (speck-add-cleanup overlay from to word))
	     ((and (integerp char)
		   (or (char-equal char ?l) (char-equal char ?&)))
	      (process-send-string speck-process (concat "&" word "\n"))
	      (process-send-string speck-process "#\n")
	      (speck-add-cleanup overlay from to word))
	     ((and (integerp char) (char-equal char ?b) (not hash))
	      (speck-hash-put word 'buffer)
	      (speck-add-cleanup overlay from to word))
	     ((and (integerp char) (char-equal char ?f)
		   speck-save-words (speck-save-confirm))
	      (speck-hash-put word 'file)
	      (speck-add-cleanup overlay from to word))
	     ((and (integerp char) (char-equal char ?d)
		   speck-save-words (speck-save-confirm))
	      (speck-hash-put
	       word (or dictionary speck-dictionary))
	      (speck-add-cleanup overlay from to word))
	     (t
	      (setq this-command 'mode-exited)
	      (setq unread-command-events
		    (append (listify-key-sequence key)
			    unread-command-events)))))
	;; Restore previous face.
	(when (overlayp overlay)
	  (overlay-put overlay 'face face))))))

(defun speck-add-previous (&optional arg)
  "Add previous highlighted word on selected window.
With ARG n do this for nth highlighted word preceding `point'."
  (interactive "p")
  (let ((overlay (speck-previous-overlay (or arg 1) '(speck-guess speck-miss))))
    (if overlay
	(speck-add-word overlay)
      (let (message-log-max)
	(message "Not found ...")
	(ding)))))

(defun speck-add-next (&optional arg)
  "Add next highlighted word on selected window.
With ARG n do this for nth highlighted word following `point'."
  (interactive "p")
  (let ((overlay (speck-next-overlay (or arg 1) '(speck-guess speck-miss))))
    (if overlay
	(speck-add-word overlay)
      (let (message-log-max)
	(message "Not found ...")
	(ding)))))

;; _____________________________________________________________________________
;; 										
;;;				 popup menus					
;; _____________________________________________________________________________
;; 										
(defun speck-menu-tail (lower buffer dictionary)
  "Precalculated tail for popup menu."
  (append
   (list
    ""
    (cons "---" "---")
    (cons "Read from minibuffer" 'minibuffer)
    (cons "Add to personal dictionary" 'personal))
   (when lower (list (cons "Add lower-case version" 'lower)))
   (when buffer (list (cons "Add to word-list of buffer" 'buffer)))
   (when (and speck-save-confirmed buffer-file-name speck-save-words)
     (list (cons "Add to general word-list of file" 'file)))
   (when (and speck-save-confirmed buffer-file-name speck-save-words)
     (list (cons "Add to dictionary word-list of file" 'dictionary)))))

(defun speck-popup-menu (posn &optional faces)
  "Pop up speck menu at position POSN."
  (let ((overlay (speck-overlay-at-point nil faces))
	(process speck-process))
    (when (and overlay process)
      ;; Preempt `speck-process' and unwind-protect the following to
      ;; assert that preemption is cancelled (we do this to avoid that
      ;; specking continues during popups).
      (process-put process 'preempted t)
      (unwind-protect
	  (let* ((from (overlay-start overlay))
		 (to (overlay-end overlay))
		 (word (buffer-substring-no-properties from to))
		 (guesses
		  (let (list)
		    (nreverse
		     (dolist (item (speck-word from to word speck-multi) list)
		       (setq list (cons (cons item item) list))))))
		 (property (when speck-multi (speck-get-speck from)))
		 (property-name (when property (symbol-name property)))
		 (hash (speck-hash-get word))
		 (speck-replace-query speck-replace-query)
		 (replace (x-popup-menu
			   posn
			   ;; Put dictionary in menu (the user should
			   ;; not have to guess which language is used).
			   (list
			    (if property
				(concat word "  [" property-name "]")
			      word)
			    (cons "" guesses)
			    (speck-menu-tail
			     (not (string-equal word (downcase word)))
			     (not hash)
			     (or property-name
				 (symbol-name speck-dictionary)))
			    )))
		 (speck-process
		  (if property
		      (car (rassq property speck-process-dictionary-alist))
		    speck-process))
		 (speck-dictionary (or property speck-dictionary)))
	    (while (eq replace 'query)
	      (setq speck-replace-query (not speck-replace-query))
	      (setq replace (x-popup-menu
			     posn
			     (list
			      (if property
				  (concat word " (" property-name ")")
				word)
			      (cons "" guesses)
			      (speck-menu-tail
			       (not (string-equal word (downcase word)))
			       (not hash)
			       (or property-name speck-dictionary))
			      ))))
	    (when (eq replace 'minibuffer)
	      (setq replace
		    (read-from-minibuffer
		     "Replace word: " word minibuffer-local-map nil
		     'speck-replace-history word t)))
	    (cond
	     ((memq replace '(personal lower))
	      (if (eq replace 'personal)
		  (process-send-string speck-process (concat "*" word "\n"))
		(process-send-string
		 speck-process (concat "&" word "\n")))
	      (process-send-string speck-process "#\n")
	      (speck-add-cleanup overlay from to word))
	     ((and (eq replace 'buffer) (not hash))
	      (speck-hash-put word 'buffer)
	      (speck-add-cleanup overlay from to word))
	     ((eq replace 'file)
	      (speck-hash-put word 'file)
	      (speck-add-cleanup overlay from to word))
	     ((eq replace 'dictionary)
	      (speck-hash-put word (or property speck-dictionary))
	      (speck-add-cleanup overlay from to word))
	     (replace
	      (unless (atom replace)
		(setq replace (car replace)))
	      (speck-replace-word from to word replace overlay property)
	      (when speck-replace-query
		(speck-replace-query
		 (downcase word) replace (or property speck-dictionary))))))
	(process-put process 'preempted nil)))))

(defun speck-popup-menu-at-point (&optional at point)
  "Pop up speck menu.
Optional arguments AT and POINT if set mean popup menu at position AT
and return to position POINT afterwards.  At least one letter of the
incorrect word must appear at the right of `point'."
  (interactive)
  (set-marker speck-marker (or point (point)))
  (setq speck-marker-window (selected-window)) ; <-----
  (when at (goto-char at))
  (let ((posn (posn-at-point)))
    ;; Always jump back to `speck-marker'.
    (unwind-protect
	(speck-popup-menu
	 (list (list (car (posn-x-y posn)) (cdr (posn-x-y posn)))
	       (posn-window posn)))
      (speck-marker-goto))))

(defun speck-popup-menu-previous (&optional arg)
  "Popup menu for previous word with guesses or miss.
With ARG n do this for nth such word preceding `point'."
  (interactive "p")
  (let ((overlay (speck-previous-overlay (or arg 1) '(speck-guess speck-miss))))
    (if overlay
	(speck-popup-menu-at-point (overlay-start overlay) (point))
      (let (message-log-max)
	(message "Not found ...")
	(ding)))))

(defun speck-popup-menu-next (&optional arg)
  "Popup menu for next word with guesses or miss.
With ARG n do this for nth such word following `point'."
  (interactive "p")
  (let ((overlay (speck-next-overlay (or arg 1) '(speck-guess speck-miss))))
    (if overlay
	(speck-popup-menu-at-point (overlay-start overlay) (point))
      (let (message-log-max)
	(message "Not found ...")
	(ding)))))

(defun speck-mouse-popup-menu (event)
  "Pop up speck menu at mouse-position.
Should be bound to a click event."
  (interactive "e")
  (set-marker speck-marker (window-point) (current-buffer))
  (setq speck-marker-window (selected-window))
  (mouse-set-point event)
  ;; Always jump back to `speck-marker'.
  (unwind-protect
      (speck-popup-menu event)
    (speck-marker-goto)))

;; _____________________________________________________________________________
;; 										
;;;				   replace					
;; _____________________________________________________________________________
;; 										
(defun speck-key-help (command keys suffix)
  "Return string of keys in KEYS executing COMMAND.
KEYS must be either `speck-replace-keys' or
`speck-replace-query-keys'."
  (let ((string ""))
    (dolist (key keys)
      (when (eq command (car key))
	(setq string
	      (concat
	       string
	       (unless (string-equal string "") ", ") ; Looks better.
	       (key-description (cdr key))))))
    (if (string-equal string "")
	""
      (concat "  " string suffix)))) ; Prefix this with two spaces.

(defun speck-keys-help (keys &optional first)
  "Return a readable list of keybindings for help."
  (concat
   ;; Use a fixed list of commands here, it's simpler.  Yes we do
   ;; allocate string space here, but after all this should be used only
   ;; sporadically.
   (speck-key-help 'accept keys
		   (concat "  to accept the replacement and "
			   (if first "query further occurrences" "continue querying") "\n"))
   (speck-key-help 'accept-and-quit keys
		   "  to accept the replacement and quit querying\n")
   (speck-key-help 'reject keys
		   "  to reject the replacement and continue querying\n")
   (speck-key-help 'reject-and-quit keys
		   "  to reject the replacement and quit querying\n")
   (speck-key-help 'forward keys "  to display the next replacement\n")
   (speck-key-help 'backward keys "  to display the previous replacement\n")
   (speck-key-help 'help keys "  to display this help\n")))

(defun speck-replace-word (from to word replace &optional overlay property)
  "Replace WORD within FROM and TO by REPLACE.
Optional OVERLAY non-nil means remove that overlay.  PROPERTY non-nil
means put this property on REPLACE."
  (let (move-to)
    (when overlay
      (delete-overlay overlay))
    (when (and (eq (marker-buffer speck-marker)
		   (current-buffer))
	       (<= from speck-marker)
	       (<= speck-marker to))
      (cond
       ((eq speck-replace-preserve-point 'before)
	(setq move-to from))
       ((and (eq speck-replace-preserve-point 'within)
	     (<= from speck-marker)
	     (<= speck-marker to)
	     (< (- speck-marker from)
		(length replace)))
	(setq move-to (marker-position speck-marker)))
       (t (setq move-to (+ from (length replace))))))
    (delete-region from to)
    (goto-char from)
    (insert replace)
    (when property
      (speck-put-speck from (point) property))
    (when move-to
      (set-marker speck-marker move-to))
    ;; The following never worked here.  Maybe I misunderstand this
    ;; completely.
    ;;     (speck-send-replacement word replace)
    ))

(defun speck-replace-put-overlay (overlay from to offset replace)
  "Put OVERLAY and goto FROM or TO."
  (if offset
      (cond
       ((eq speck-replace-preserve-point 'before)
	(overlay-put overlay 'display replace)
	(goto-char from))
       ((and (eq speck-replace-preserve-point 'within)
	     (< offset (length replace)))
	(overlay-put
	 overlay 'display
	 (concat (substring replace 0 offset)
		 (propertize
		  (substring replace offset (1+ offset)) 'cursor t)
		 (substring replace (1+ offset))))
	(goto-char from))
       (t
	(overlay-put overlay 'display replace)
	(goto-char to)))
    (overlay-put overlay 'display replace)))

(defun speck-replace (overlay)
  "Replace word covered by `OVERLAY' with corrections."
  (let ((process speck-process))
    (when (and overlay process)
      (process-put process 'preempted t)
      (unwind-protect
	  (let* ((from (overlay-start overlay))
		 (property (when speck-multi (speck-get-speck from)))
		 (to (overlay-end overlay))
		 (offset (when (and (< from (point))
				    (< (point) to))
			   ;; Offset of `point' wrt `from'.
			   (- (point) from)))
		 (word (buffer-substring-no-properties from to))
		 (guesses (speck-word from to word speck-multi))
		 (text
		  ;; We can't use any "`" or "'" here, these characters
		  ;; may be part of the word or the replacement.  Hence
		  ;; entirely rely on faces (`speck-query') to set them
		  ;; apart from the rest.
		  (substitute-command-keys
		   (concat 
		    "Replace %s"
		    (when property " [%s]")
		    " with %s ?  Type \\<speck-replace-map>\\[help] for help."))))
	    (if (null guesses)
		(progn
		  (message "No corrections found")
		  (ding))
	      (let* ((replace (car guesses))
		     (guess-vector (vconcat guesses))
		     (guess-index 0)
		     (guess-max (1- (length guess-vector)))
		     (def 'forward)
		     change query key)
		(set-marker speck-marker (point))
		(setq speck-marker-window (selected-window)) ; <----
		(speck-replace-put-overlay overlay from to offset replace)
		(overlay-put overlay 'face 'speck-query)
		(unwind-protect
		    (progn
		      (while (memq def '(forward backward help))
			(let ((message-log-max nil))
			  ;; This message is also needed to avoid
			  ;; echoing typed characters in the echo area
			  ;; (see replace.el).
			  (if property
			      (message
			       text (propertize word 'face 'speck-query)
			       property (propertize replace 'face 'speck-query))
			    (message
			     text (propertize word 'face 'speck-query)
			     (propertize replace 'face 'speck-query))))
			(setq key (vector (read-event)))
			(setq def (lookup-key speck-replace-map key))
			(cond
			 ((eq def 'accept)
			  (setq change t)
			  (setq query t))
			 ((eq def 'accept-and-quit)
			  (setq change t))
			 ((memq def '(reject reject-and-quit)))
			 ((eq def 'forward)
			  (setq guess-index
				(if (= guess-index guess-max) 0 (1+ guess-index)))
			  (setq replace (aref guess-vector guess-index))
			  (speck-replace-put-overlay overlay from to offset replace))
			 ((eq def 'backward)
			  (setq guess-index
				(if (zerop guess-index) guess-max (1- guess-index)))
			  (setq replace (aref guess-vector guess-index))
			  (speck-replace-put-overlay overlay from to offset replace))
			 ((eq def 'help)
			  (with-output-to-temp-buffer "*Help*"
			    (princ
			     (concat
			      "Replace `" word "' with `" replace "'?  Type\n\n"
			      (speck-keys-help speck-replace-keys t)
			      "\nAnything else will accept the replacement and reread as command.\n"))
			    (with-current-buffer standard-output
			      (help-mode))))
			 (t
			  ;; The mode-exited stuff is not clean but
			  ;; let's try doing this as in `query-replace'.
			  (setq this-command 'mode-exited)
			  (setq unread-command-events
				(append (listify-key-sequence key)
					unread-command-events))
			  (setq change t)))))
		  (cond
		   (change
		    (speck-replace-word from to word replace overlay property))
		   ((overlayp overlay)
		    ;; Restore overlay properties.
		    (overlay-put overlay 'display nil) ; Silly
		    (overlay-put overlay 'face 'speck-guess)))
		  (when (and query speck-replace-query)
		    (speck-replace-query
		     (downcase word) replace (or property speck-dictionary)))
		  (speck-marker-goto)))))
	(process-put process 'preempted nil)))))

(defun speck-replace-previous (&optional arg)
  "Correct previous word with guesses in place.
With ARG n do this for nth such word preceding `point'."
  (interactive "p")
  (let ((overlay (speck-previous-overlay (or arg 1) '(speck-guess))))
    (if overlay
	(speck-replace overlay)
      (let (message-log-max)
	(message "Not found ...")
	(ding)))))

(defun speck-replace-next (&optional arg)
  "Correct next word with guesses in place.
With ARG n do this for nth such word following `point'."
  (interactive "p")
  (let ((overlay (speck-next-overlay (or arg 1) '(speck-guess))))
    (if overlay
	(speck-replace overlay)
      (let (message-log-max)
	(message "Not found ...")
	(ding)))))

(defun speck-replace-query (word replace original-property)
  "Query replace further occurrences of WORD by something like REPLACE.
ORIGINAL-PROPERTY must match the speck property of the occurrence."
  (let ((regexp (concat "\\<" (regexp-quote word) "\\>"))
	(query t)
	(case-fold-search t)
	(text
	 (substitute-command-keys
	  "Replace `%s' with `%s'?  Type \\<speck-replace-query-map>\\[help] for help.")))
    ;; Consider widening here.
    ;; Consider using `undo-boundary' here.
    (goto-char (point-min))
    (while (and query (not (eobp))
		(re-search-forward regexp nil t))
      (let* ((from (match-beginning 0))
	     (property (when speck-multi (speck-get-speck from)))
	     (to (match-end 0))
	     (word (buffer-substring-no-properties from to))
	     (begin (line-beginning-position))
	     (end (line-beginning-position 2))
	     guesses tail)
	(when (and (if property
		       (equal property original-property)
		     (eq speck-dictionary original-property))
		   (not (and query-replace-skip-read-only
			     ;; Ignore matches with read-only property.
			     (text-property-not-all
			      (match-beginning 0) (match-end 0)
			      'read-only nil)))
		   (save-excursion
		     (and (goto-char from) (speck-syntactic-p)
			  ;; The following ignores matches when ISPELL
			  ;; doesn't deliver guesses.  This shouldn't
			  ;; occur.
			  (consp (setq guesses (speck-word from to word property))))))
	  (when (setq tail (member-ignore-case replace guesses))
	    ;; REPLACE is in `guesses'.
	    (unless (eq guesses tail)
	      ;; Move REPLACE to head of list.
	      (setq guesses
		    (cons (car tail)
			  (delete (car tail) guesses)))))
	  (let* ((replace (car guesses))
		 (reps-vector (vconcat guesses))
		 (reps-index 0)
		 (reps-max (1- (length reps-vector)))
		 (overlay (or (speck-overlay-at-point
			       from '(speck-guess speck-miss))
			      (make-overlay from to)))
		 (def 'forward)
		 change key)
	    (overlay-put overlay 'specky t)
	    (overlay-put overlay 'display replace)
	    (overlay-put overlay 'face 'speck-query)
	    (unwind-protect
		(while (memq def '(forward backward help))
		  (setq query nil)
		  (setq def nil)
		  (let ((message-log-max nil))
		    ;; This message is needed to avoid echoing typed
		    ;; characters in the echo area (see replace.el).
		    (message text word replace))
		  (setq key (vector (read-event)))
		  (setq def (lookup-key speck-replace-query-map key))
		  (cond
		   ((eq def 'accept)
		    (setq change t)
		    (setq query t))
		   ((eq def 'accept-and-quit)
		    (setq change t))
		   ((eq def 'reject)
		    (setq query t))
		   ((eq def 'reject-and-quit))
		   ((eq def 'forward)
		    (setq reps-index
			  (if (= reps-index reps-max) 0 (1+ reps-index)))
		    (setq replace (aref reps-vector reps-index))
		    (overlay-put overlay 'display replace))
		   ((eq def 'backward)
		    (setq reps-index
			  (if (zerop reps-index) reps-max (1- reps-index)))
		    (setq replace (aref reps-vector reps-index))
		    (overlay-put overlay 'display replace))
		   ((eq def 'help)
		    (with-output-to-temp-buffer "*Help*"
		      (princ
		       (concat
			"Replace `" word "' with `" replace "'?  Type\n\n"
			(speck-keys-help speck-replace-query-keys)
			"\nAnything else will accept the replacement and reread as command.\n"))
		      (with-current-buffer standard-output
			(help-mode))))
		   (t
		    ;; The mode-exited stuff is not clean but let's try
		    ;; doing this as in `query-replace'.
		    (setq this-command 'mode-exited)
		    (setq unread-command-events
			  (append (listify-key-sequence key)
				  unread-command-events))
		    (setq change t))))
	      (cond
	       (change
		(speck-replace-word from to word replace overlay property))
	       ((overlayp overlay)
		;; Install or restore overlay properties.
		(overlay-put overlay 'display nil) ; Silly
		(overlay-put overlay 'face 'speck-guess)))
	      (unless query (speck-marker-goto)))))))))

;; _____________________________________________________________________________
;; 										
;;;			       region specking					
;; _____________________________________________________________________________
;; 										
(defun speck-region () ; Put this somewhere else <--------
  "Speck region (in `transient-mark-mode' when the mark is active) or current buffer (otherwise)."
  (interactive)
  (let (start end)
    (setq speck-suspension-list nil)
    (save-restriction
      (widen)
      (if (and transient-mark-mode mark-active)
	  (progn
	    (setq start (min (point) (mark)))
	    (setq end (max (point) (mark))))
	(setq start (point-min))
	(setq end (point-max)))
      (speck-remove-all-properties)
      (speck-delete-overlays))))

(defun speck-change-aspell-dictionary ()
  "Change Aspell dictionary."
  (interactive)
  (speck-buffer '-))

(defvar speck-filter-mode-history nil)

(defun speck-set-filter-mode ()
  "Set Speck's filter mode."
  (interactive)
  (let ((filter-mode
	 (completing-read
	  (concat
	   "Enter filter-mode (RET for default, SPC to complete): ")
	  (list "Default" "None" "URL" "Email" "SGML" "TeX")
	  nil t nil 'speck-filter-mode-history)))
    (if (or (string-equal filter-mode "")
	    (string-equal filter-mode "Default"))
	;; Consider resetting to saved value here, maybe with prefix argument.
	(setq filter-mode (default-value 'speck-filter-mode))
      (setq filter-mode (intern filter-mode)))
    (if (eq filter-mode speck-filter-mode)
	(message "Filter-mode \"%s\" unchanged" filter-mode)
      (setq speck-filter-mode filter-mode)
      (when speck-mode
	(speck-deactivate)
	(setq speck-retain-local-variables t)
	(speck-activate)))))

;; _____________________________________________________________________________
;; 										
;;;			       multi-dictionary					
;; _____________________________________________________________________________
;; 										
(defun speck-multi-set-region (from to &optional dictionary)
  "Set 'speck property for region between FROM and TO to DICTIONARY.
If DICTIONARY is nil or omitted remove speck property from region." 
  (if speck-save-permanent
      ;; Make this a permanent change, not undoable.
      (with-buffer-prepared-for-specking
       (if dictionary
	   (progn
	     (setq speck-multi t)
	     (speck-put-speck from to dictionary))
	 (remove-text-properties from to '(speck nil)))
       (remove-text-properties from to '(specked nil))
       ;; Assert this gets saved before the buffer gets killed.
       (speck-kill-buffer-add))
    (if dictionary
	(progn
	  (setq speck-multi t)
	  (speck-put-speck from to dictionary))
      (remove-text-properties from to '(speck nil)))
    (with-buffer-prepared-for-specking
     (remove-text-properties from to '(specked nil))))
  (speck-delete-overlays from to)
  (dolist (window (get-buffer-window-list (current-buffer)))
    (speck-window-add window)))

;; `speck-multi-set' permits to specify the dictionary - actually a
;; multi property since we permit `--' here too - for the next
;; self-insertion.  For this purpose it assigns the property to
;; `speck-multi-pre-property' and adds `speck-multi-pre-command' to
;; `pre-command-hook'.  `speck-multi-pre-command' copies the property to
;; `speck-multi-post-property', adds `speck-multi-post-command' to
;; `post-command-hook', and removes itself from `pre-command-hook'.  Any
;; self-insertion command executed now will find the property in
;; `speck-multi-post-property' and assign its value to the text it
;; inserts.  `speck-multi-post-command' will reset
;; `speck-multi-post-property' to nil regardless of whether a property
;; was assigned and remove itself from `post-command-hook'.  Simpler
;; solutions welcome.
(defun speck-multi-pre-command ()
  "Set up speck multi property for this command."
  (setq speck-multi-post-property speck-multi-pre-property)
  (setq speck-multi-pre-property nil)
  (add-hook 'post-command-hook 'speck-multi-post-command nil t)
  (remove-hook 'pre-command-hook 'speck-multi-pre-command t))

(defun speck-multi-post-command ()
  "Remove speck multi property."
  (setq speck-multi-post-property nil)
  (remove-hook 'post-command-hook 'speck-multi-post-command t))

(defun speck-multi-set (arg)
  "Set dictionary for part of current buffer.

With prefix ARG set proceed as follows: If ARG is not a number
deactivate specking.  If ARG is zero use the default dictionary.
For any other number check `speck-dictionary-names-alist' whether
an entry for that number exists.  If an entry exists use the
associated dictionary.  Otherwise prompt the user for a
dictionary.

In `transient-mark-mode', if the mark is active this command sets
the dictionary for the region.  Otherwise, this command will set
the dictionary for text that will be inserted by the next
\(interactively called) command."
  (interactive "P")
  (let ((at (point))
	(dictionary-names
	 (cond
	  ((eq speck-engine 'Aspell)
	   speck-aspell-dictionary-names)
	  ((eq speck-engine 'Ispell)
	   speck-ispell-dictionary-names)))
	from to dictionary-name)
    (if (and transient-mark-mode mark-active)
	(progn
	  ;; If the mark is active use the region.
	  (setq from (min (point) (mark)))
	  (setq to (max (point) (mark))))
      ;; Otherwise set this up for things to come.
      (setq speck-multi-pre-property t))
    (cond
     ((and arg (not (numberp arg)))
      ;; Set to no-check.
      (if speck-multi-pre-property
	  (setq speck-multi-pre-property '--)
	(speck-multi-set-region from to '--))
      (message "Reset to no-check"))
     ((and arg (zerop arg))
      ;; Reset to default.
      (if speck-multi-pre-property
	  (setq speck-multi-pre-property 'default)
	(speck-multi-set-region from to))
      (message "Reset to default"))
     ((and arg
	   (setq dictionary-name
		 (cdr (assoc arg speck-dictionary-names-alist))))
      ;; We have an entry for ARG in `speck-dictionary-names-alist', use
      ;; associated dictionary-name.
      (if (member dictionary-name dictionary-names)
	  (let ((dictionary (intern dictionary-name)))
	    (if speck-multi-pre-property
		(setq speck-multi-pre-property dictionary)
	      (speck-multi-set-region from to dictionary))
	    (message "Set to dictionary \"%s\"" dictionary-name))
	(setq speck-multi-pre-property nil)
	(message "Can't find dictionary \"%s\"" dictionary-name)))
     (t
      (setq dictionary-name (read-from-minibuffer "Dictionary: "))
      (cond
       ((or (not dictionary-name) (string-equal dictionary-name ""))
	;; Reset to default.
	(if speck-multi-pre-property
	    (setq speck-multi-pre-property 'default)
	  (speck-multi-set-region from to))
	(message "Reset to default"))
       ((string-match "--" dictionary-name)
	;; Set to no-check.
	(if speck-multi-pre-property
	    (setq speck-multi-pre-property '--)
	  (speck-multi-set-region from to '--))
	(message "Set to no-check"))
       ((member dictionary-name dictionary-names)
	(let ((dictionary (intern dictionary-name)))
	  ;; Set dictionary for region.
	  (if speck-multi-pre-property
	      (setq speck-multi-pre-property dictionary)
	    (speck-multi-set-region from to dictionary)
	    (message "Set to dictionary \"%s\"" dictionary-name))))
       (t
	(setq speck-multi-pre-property nil)
	(message "Can't find dictionary \"%s\"" dictionary-name)))))
    (when speck-multi-pre-property
      (setq speck-multi t)
      ;; We can't do this earlier since `read-from-minibuffer' would
      ;; execute it.
      (add-hook 'pre-command-hook 'speck-multi-pre-command nil t))))

;; Observe: Inserting _parts_ of a file into another file will certainly
;; cause troubles (although the 'eof approach could handle it - we would
;; have to go to the file, look whether it contains the speck-eof line,
;; and propertize the inserted text accordingly).
(defun speck-multi-write (from to buffer)
  "Return list of annotations for encoding the region.
FROM and TO denote start and end of the region for buffer BUFFER."
  (let ((from (or from (point-min)))
	(to (or to (point-max)))
	property list)
    (cond
     ((or (not speck-multi) (not (speck-save-confirm))))
     ((eq speck-multi-style 'eof)
      ;; This is certainly wrong for `to' /= `point-max'.
      (let ((start from))
	(save-excursion
	  (save-restriction
	    (narrow-to-region from to)
	    ;; Create `list' as (3 7 "fr" 234 256 "it" ...) that is a
	    ;; list of <start end dictionary> triples.  Use strings to
	    ;; encode language properties in order to `read' back and
	    ;; subsequently `intern' them in a simple fashion.
	    (while (setq from (text-property-not-all from to 'speck nil))
	      (setq property (speck-get-speck from))
	      (if (eq property '==)
		  ;; Do not record `==' properties (we do record `--'
		  ;; properties, though).
		  (goto-char
		   (setq from (or (next-single-property-change from 'speck)
				  to)))
		(setq list (cons from list))
		(setq list (cons
			    (progn
			      (goto-char
			       (setq from (or (next-single-property-change
					       from 'speck)
					      to)))
			      from)
			    list))
		(setq list (cons property list))))
	    (when list
	      (setq list
		    (list
		     (cons
		      to ; The position passed to `write-region-annotate-functions' ...
		      (concat			; ... and the string.
		       ;; The following will add a newline at eob unless
		       ;; there's one.
		       (unless (save-excursion (goto-char to) (bolp)) "\n")
		       ;; Insert a comment starter, if possible, to
		       ;; avoid that other applications choke at our
		       ;; properties.
		       (or comment-start "")
		       ;; Add a `print'ed version of `list'.
		       "<<speck-eof"
		       (let (print-level print-length)
			 (prin1-to-string (nreverse list)))
		       ;; Mark this appropriately.
		       "speck-eof>>"
		       ;; Insert a comment ender or a newline
		       (cond
			((string-equal comment-end "") "\n")
			((string-equal (substring comment-end -1) "\n")
			 ;; Is this case reasonable?
			 comment-end)
			(t (concat comment-end "\n"))))))))))))
     (speck-multi-style
      (save-excursion
	(save-restriction
	  (narrow-to-region from to)
	  (while (setq from (text-property-not-all from to 'speck nil))
	    (setq property (speck-get-speck from))
	    (if (eq property '==)
		(goto-char
		 (setq from (or (next-single-property-change from 'speck)
				to)))
	      (setq property (symbol-name property))
	      ;; Disallow colons in Aspell's dictionary names.
	      (setq list (cons (cons from (concat "<<speck-" property ":"))
			       list))
	      (goto-char
	       (setq from (or (next-single-property-change from 'speck)
			      to)))
	      (setq list (cons (cons from (concat "speck-" property ">>"))
			       list))))
	  (when list
	    (setq list (cons (cons (point-max) "<<speck-nil:speck-nil>>") list)))))))
    (when list
      ;; This should eliminate ourselves when `list' is empty.
      ;; Hopefully, a language called `nil' will not be invented ever.
      (cons
       (cons
	(point-min)
	(concat
	 (when comment-start
	   (concat comment-start " "))
	 (if (eq speck-multi-style 'eof)
	     "<<speck-bof:speck-eof>>"
	   "<<speck-bof:speck-nil>>")
	 (when comment-end
	   (concat " " comment-end))
	 (when (or (not comment-end)
		   (not (string-equal comment-end "\n")))
	   "\n")))
       (nreverse list)))))

;;;###autoload
(defun speck-multi-read (begin end)
  "Convert annotations to properties.
BEGIN and END denote the region to convert."
  (with-buffer-unmodified
   ;; We cannot generally flush `buffer-undo-list' but we can avoid
   ;; making an entry when it's nil, the following must be largely
   ;; handled by `format-decode' or `insert-file-contents'.
   (let ((offset (- begin (point-min)))
	 (undo-list buffer-undo-list)
	 (inhibit-read-only t)
	 (inhibit-point-motion-hooks t)
	 (inhibit-modification-hooks t)
	 (inhibit-field-text-motion t)
	 first-change-hook after-change-functions
	 style)
     (save-excursion
       (cond
	((and (goto-char begin)
	      (not (looking-at
		    ".\\{,12\\}<<speck-bof:speck-\\(?:\\(eof\\)\\|nil>>\\)")))
	 ;; This should not occur.
	 (error "Speck annotations corrupted"))
	((and (setq style (match-beginning 1))
	      (not (speck-save-confirm))
	      (if (yes-or-no-p "Modifying this buffer will lose speck multi information, continue? ")
		  (save-restriction
		    (narrow-to-region begin end)
		    ;; Note: We do have to delete the line containing
		    ;; our header here since otherwise format will ask
		    ;; us forever.  If you want to see this line you
		    ;; have to use `find-file-literally' instead.
		    (delete-region begin (line-beginning-position 2))
		    (point-max))
		;; Better signal an error and quit.
		(error "Canceled"))))
	;; This is a fresh addition to assert that the user gets asked
	;; in the speck-nil case as well.
	((and (not style)
	      (not (speck-save-confirm)))
	 (if (yes-or-no-p "Modifying this buffer will lose speck multi information, continue? ")
	     (save-restriction
	       (narrow-to-region begin end)
	       ;; Note: We do have to delete the line containing our
	       ;; header here since otherwise format will ask us
	       ;; forever.  If you want to see this line you have to use
	       ;; `find-file-literally' instead.
	       (delete-region begin (line-beginning-position 2))
	       (point-max))
	   (error "Canceled")))
	(t
	 (save-restriction
	   (narrow-to-region begin end)
	   ;; Delete this line.
	   (delete-region begin (line-beginning-position 2))
	   (setq speck-multi t)
	   (add-to-list 'buffer-file-format 'speck)
	   (if style
	       ;; eof case
	       (let (list)
		 (goto-char (point-max))
		 ;; We should be right after <<speck-eof(...)speck-eof>> here.
		 (if (re-search-backward "speck-eof>>" nil t)
		     (progn
		       (backward-sexp)
		       (setq list (read (current-buffer)))
		       ;; If someone modified our text she's on her own.
		       (delete-region
			(line-beginning-position) (line-beginning-position 2))
		       (if (zerop offset)
			   ;; The "normal" or "reverting" case.
			   (while list
			     (speck-put-speck
			      (pop list) (pop list) (pop list)))
			 ;; The "insert-file" not at `point-min' case.
			 (while list
			   (speck-put-speck
			    (+ (pop list) offset) (+ (pop list) offset)
			    (pop list)))))
		   (error "Speck annotations corrupted")))
	     ;; nil case
	     (let (from property)
	       (goto-char (point-min))
	       ;; Consider [:alpha:] here.
	       (while (re-search-forward "<<speck-\\([a-zA-Z-_0-9]+\\):" nil t)
		 (setq property (match-string-no-properties 1))
		 (delete-region (match-beginning 0) (point))
		 (setq from (point))
		 (if (re-search-forward (concat "speck-" property ">>"))
		     (progn
		       (delete-region (match-beginning 0) (point))
		       (speck-put-speck from (point) (intern property)))
		   ;; The following message might not be very consolidating.
		   (error
		    (concat "Missing speck-" property ">>"))))))
	   ;; Reset `buffer-undo-list', if possible.
	   (unless undo-list (setq buffer-undo-list nil))
	   (point-max))))))))

;; _____________________________________________________________________________
;; 										
;;;			    case auto-correction				
;; _____________________________________________________________________________
;; 										
(defun speck-auto-correct-case (old from to word)
  "Auto-correct case."
  (let (corrected guesses)
    (with-current-buffer (process-buffer speck-process)
      ;; `point' must be after the colon.
      (forward-char)
      (while (re-search-forward "\\(.*?\\)\\(?:, \\|\n\n\\)" nil t)
	(setq guesses (cons (match-string-no-properties 1) guesses)))
      (setq guesses (nreverse guesses)))
    (cond
     ((eq speck-auto-correct-case 'two)
      (when (and (> (- to from) 1)
		 ;; Replace `word' if downcasing its second letter
		 ;; yields a word in `guesses'.
		 (not (equal (aref word 0) (downcase (aref word 0))))
		 (not (equal (aref word 1) (downcase (aref word 1)))))
	(let ((replace (concat
			(substring word 0 1)
			(downcase (substring word 1 2))
			(substring word 2))))
	  (when (catch 'found
		  (dolist (guess guesses)
		    (when (string-equal replace guess)
		      (throw 'found t))))
	    (speck-replace-word
	     from to word replace nil
	     (when speck-multi (speck-get-speck from)))
	    (setq corrected t)))))
     ((eq speck-auto-correct-case 'one)
      ;; Replace `word' if changing its case yields a unique word in
      ;; `guesses'.
      (let ((cased (downcase word))
	    replace)
	(when (catch 'failed
		(dolist (guess guesses replace)
		  (when (eq (compare-strings
			     cased 0 nil guess 0 nil t)
			    t)
		    (if replace
			(throw 'failed nil)
		      (setq replace guess)))))
	  (speck-replace-word
	   from to word replace nil
	   (when speck-multi (speck-get-speck from)))
	  (setq corrected t))))
     ((eq speck-auto-correct-case t)
      ;; Replace `word' if changing its case yields a word in
      ;; `guesses'.
      (let* ((cased (downcase word))
	     (replace
	      (catch 'found
		(dolist (guess guesses)
		  (when (eq (compare-strings
			     cased 0 nil guess 0 nil t)
			    t)
		    (throw 'found guess))))))
	(when replace
	  (speck-replace-word
	   from to word replace nil
	   (when speck-multi (speck-get-speck from)))
	  (setq corrected t)))))
    (when corrected
      (speck-put-property old to))
    corrected))

;; _____________________________________________________________________________
;; 										
;;;		     auto-correct after a buffer change				
;; _____________________________________________________________________________
;; 										
(defvar speck-auto-correct-regexp
  "\\(?:\\sw\\(#\\)[st]\\>\\)")

(defvar speck-auto-correct-replace
  (vector "" "'"))

(defun speck-auto-correct-after-change ()
  (save-excursion
    (when (and (eq this-command 'self-insert-command)
	       (looking-back speck-auto-correct-regexp))
      (cond
       ((match-beginning 1)
	(replace-match (aref speck-auto-correct-replace 1) nil nil nil 1))))))

;; _____________________________________________________________________________
;; 										
;;;			    miscellaneous					
;; _____________________________________________________________________________
;; 										
(defvar message-signature-separator)

(defun speck-email-region ()
  "Reset regions for mail mode."
  (interactive)
  (let* ((body-start
	  (save-excursion
	    (goto-char (point-min))
	    (re-search-forward
	     (concat "^" (regexp-quote mail-header-separator) "$") nil t)
	    (point)))
	 (subject-start
	  (when (> body-start (point-min))
	    (save-excursion
	      (goto-char (point-min))
	      (when (re-search-forward "^Subject:" nil t)
		(point)))))
	 (subject-end
	  (when subject-start
	    (save-excursion
	      (goto-char subject-start)
	      (line-end-position))))
	 (body-end
	  (save-excursion
	    (goto-char (point-max))
	    (re-search-backward message-signature-separator nil t)
	    (point))))
    (if subject-end
	(progn
	  (speck-multi-set-region (point-min) subject-start '==)
	  (speck-multi-set-region subject-end body-start '==))
      (speck-multi-set-region (point-min) body-start '==))
    (speck-multi-set-region body-end (point-max) '==)))

(defun speck-set-html-mode ()
  (interactive)
  (with-current-buffer (process-buffer speck-process)
    (erase-buffer))
  (process-send-string speck-process "$$cr sug-mode\n")
  (accept-process-output speck-process 0.1))

(defun speck-get-option (option)
  (interactive "sOption: \n")
  (unwind-protect
      (progn
	(process-put speck-process 'preempted t)
	(with-current-buffer (process-buffer speck-process)
	  (erase-buffer))
	(process-send-string speck-process (concat "$$cr " option "\n"))
	(accept-process-output speck-process 0.1)
	(with-current-buffer (process-buffer speck-process)
	  (message
	   "Value of %s is: %s" option
	   (buffer-substring
	    (point-min)
	    (progn
	      (goto-char (point-max)) (skip-chars-backward "\n") (point))))))
    (process-put speck-process 'preempted nil)))

(defun speck-set-option (option value)
  (interactive "sOption: \nsValue: ")
  (unwind-protect
      (progn
	(process-put speck-process 'preempted t)
	(with-current-buffer (process-buffer speck-process)
	  (erase-buffer))
	(process-send-string speck-process (concat "$$cs " option "," value "\n"))
	(process-send-string speck-process (concat "$$cr " option "\n"))
	(accept-process-output speck-process 0.1)
	(with-current-buffer (process-buffer speck-process)
	  (message
	   "Value of %s set to: %s" option
	   (buffer-substring
	    (point-min)
	    (progn
	      (goto-char (point-max)) (skip-chars-backward "\n") (point))))))
    (process-put speck-process 'preempted nil)))

;; _____________________________________________________________________________
;; 										
;;;		       saving things to files					
;; _____________________________________________________________________________
;; 										

;; The following may be asked even _after_ the user has answered "no" in
;; a "Buffer foo.changed modified; kill anyway? (yes or no) yes"
;; dialogue.  There doesn't seeem a better solution than asking twice in
;; that case.  Unfortunately a file change might not even occur.
(defun speck-kill-buffer-query ()
  "Try to save local specifications if `speck-kill-buffer-query' is non-nil."
  (when (and speck-kill-buffer-query (speck-save-confirm))
    (set-buffer-modified-p t)
    (save-buffer))
  (setq speck-kill-buffer-query nil)
  (setq speck-kill-buffer-query-list
	(delq (current-buffer) speck-kill-buffer-query-list))
  (remove-hook 'kill-buffer-query-functions 'speck-kill-buffer-query t)
  t)

(defun speck-kill-emacs-query ()
  "Assert all involved buffers are queried before killing Emacs.
This assures that local specifications get saved before Emacs is killed.
Called by `kill-emacs-query-functions'."
  (dolist (buffer speck-kill-buffer-query-list)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(speck-kill-buffer-query))))
  t)

(defun speck-kill-buffer-add ()
  "Assert that current buffer is queried before getting killed."
  (when (and speck-save-ask (not speck-kill-buffer-query))
    (add-to-list 'buffer-file-format 'speck)
    (setq speck-kill-buffer-query t)
    (unless (memq (current-buffer) speck-kill-buffer-query-list)
      (setq speck-kill-buffer-query-list
	    (cons (current-buffer) speck-kill-buffer-query-list)))
    (add-hook 'kill-buffer-query-functions 'speck-kill-buffer-query nil t)
    (add-hook 'kill-emacs-query-functions 'speck-kill-emacs-query)))

;; Parts of the following function were stolen from erc.el. 
(defun speck-insert-string-noundo (string)
  "As `insert' but don't allow undoing this and update `buffer-undo-list'.
All items in the buffer-undo-list referencing a position after `point'
are updated so that they can be undone without undoing the present
insertion."
  (let ((amount (length string)))
    (with-buffer-prepared-for-specking
     (insert string))
    (unless (or (zerop amount) (atom buffer-undo-list))
      (dolist (entry buffer-undo-list)
	(cond
	 ((and (integerp entry) (< (point) entry))
	  ;; POSITION
	  (incf entry amount))
	 ((or (atom entry) (eq (car entry) t) (markerp (car entry)))
	  ;; undo boundary: `nil'
	  ;; change from "unmodified" status: (t HIGH . LOW)
	  ;; marker adjustment: (MARKER . DISTANCE)
	  nil)
	 ((and (integerp (car entry)) (< (point) (car entry)))
	  ;; insertion: (BEG . END)
	  (incf (car entry) amount)
	  (incf (cdr entry) amount))
	 ((and (stringp (car entry)) (< (point) (cdr entry)))
	  ;; deletion: (TEXT . POSITION)
	  (incf (cdr entry) (if (natnump (cdr entry)) amount (- amount))))
	 ((and (null (car entry)) (< (point) (car (nthcdr 3 entry))))
	  ;; text property modification: (nil PROPERTY VALUE BEG . END)
	  (let ((cons (nthcdr 3 entry)))
	    (incf (car cons) amount)
	    (incf (cdr cons) amount))))))))

(defun speck-save-string (string)
  "Insert STRING with `specked' and `==' properties set."
  (let ((at (point)))
    (cond
     ((eq speck-save-permanent t)
      (with-buffer-prepared-for-specking
       (insert string)))
     ((eq speck-save-permanent 'adjust)
      (speck-insert-string-noundo string))
     (t
      (insert string)))
    (with-buffer-prepared-for-specking
     ;; Never put this on the undo list.
     (add-text-properties at (point) '(specked t speck ==)))))

(defun speck-save-word (word &optional dictionary)
  "Save WORD in Local Words section.
Optional argument DICTIONARY means save in dictionary specific section."
  (when (and speck-save-words (speck-save-confirm))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-max))
	(let ((dictionary-name (when dictionary (symbol-name dictionary)))
	      at string found case-fold-search)
	  ;; Search for Local Words section.  The beginning of the last
	  ;; line of that section must appear within the 3000 characters
	  ;; limit.
	  (cond
	   ((re-search-backward speck-save-words-regexp (- (point-max) 3000) t)
	    ;; `string' records any comment-start plus "local-words"
	    ;; string.
	    (let ((string (buffer-substring-no-properties
			   (line-beginning-position) (match-end 0))))
	      (setq at (line-beginning-position))
	      (if dictionary-name
		  ;; Search for an entry for this dictionary.
		  (let ((regexp (concat
				 "[ \t]+(" (regexp-quote dictionary-name) "):")))
		    (while (and (progn
				  (goto-char (match-end 0))
				  (not (setq found (looking-at regexp))))
				(progn
				  (beginning-of-line)
				  (re-search-backward
				   speck-save-words-regexp
				   (line-beginning-position 0) t))))
		    (if found
			;; Corresponding dictionary entry found.
			(if (<= (+ (- (line-end-position)
				      (line-beginning-position))
				   ;; 80 is hard-coded here.
				   (current-column) 1 (length word)) 80)
			    ;; Insert `word' before first entry on this
			    ;; line.
			    (progn
			      (goto-char (match-end 0))
			      (skip-chars-forward " \t")
			      (speck-save-string (concat word " ")))
			  ;; Add word on new line.
			  (forward-line)
			  (speck-save-string (concat
					      string " (" dictionary-name "): " word
					      (when (stringp comment-end)
						comment-end)))
			  ;; Assert terminating newline.
			  (unless (bolp) (speck-save-string "\n")))
		      ;; No dictionary entry found, add one at end of
		      ;; local words section.
		      (goto-char at)
		      ;; The following was `forward-line' before which
		      ;; added the dictionary entry after an existing
		      ;; file entry.  Don't do that (for the moment,
		      ;; rather add it before the last dictionary entry,
		      ;; if there's one).
		      (beginning-of-line)
		      (speck-save-string
		       (concat string " (" dictionary-name "): "
			       word (when (stringp comment-end) comment-end)))
		      ;; Assert terminating newline.
		      (unless (bolp) (speck-save-string "\n"))))
		;; No dictionary.
		(while (and (progn
			      (goto-char (match-end 0))
			      (not (setq found (looking-at ":"))))
			    (progn
			      (beginning-of-line)
			      (re-search-backward
			       speck-save-words-regexp
			       (line-beginning-position 0) t))))
		(if found
		    ;; Corresponding dictionary entry found.
		    (if (<= (+ (- (line-end-position) (line-beginning-position))
			       ;; 80 is hard-coded here.
			       (current-column) 1 (length word)) 80)
			;; Insert `word' before first entry on this
			;; line.
			(progn
			  (goto-char (match-end 0))
			  (skip-chars-forward " \t")
			  (speck-save-string (concat word " ")))
		      ;; Add word on new line.
		      (forward-line)
		      (speck-save-string
		       (concat string ": " word
			       (when (stringp comment-end) comment-end)))
		      ;; Assert terminating newline.
		      (unless (bolp) (speck-save-string "\n")))
		  ;; No dictionary entry found, add one at end of local
		  ;; words section.
		  (goto-char at)
		  (forward-line)
		  (speck-save-string
		   (concat string ": " word
			   (when (stringp comment-end) comment-end)))
		  ;; Assert terminating newline.
		  (unless (bolp) (speck-save-string "\n"))))
	      t))
	   ;; Go to minimum of known local positions.
	   ((let* ((at (speck-min-of-list-and-point-max
			(speck-local-positions))))
	      (goto-char at)
	      (speck-save-string (if (bolp) "\n" "\n\n"))
	      (speck-save-string
	       (concat speck-save-words-string
		       (when dictionary-name (concat " (" dictionary-name ")"))
		       ": " word "\n\n"))
	      (forward-line -2)
	      (when comment-start
		(let ((comment-style 'plain))
		  (comment-region
		   (line-beginning-position) (line-end-position))))
	      t))
	   (t (error "Failed to add word"))))))))

(defun speck-save-words-sort ()
  "Write entire Local Words section from `speck-hash-table'."
  (interactive)
  (let* ((comment-style 'plain)
	 (comment-before (or comment-start ""))
	 (comment-after comment-end)
	 base-list other-lists string from at)
    (maphash
     (lambda (word value)
       (cond
	((eq value 'file)
	 (setq base-list (cons word base-list)))
	((listp value)
	 (let* ((dictionary (car value))
		(list (assoc dictionary other-lists)))
	   (if list
	       (setcdr list (cons word (cdr list)))
	     (setq other-lists
		   (cons (list dictionary word) other-lists)))))))
     speck-hash-table)
    (goto-char (point-max))

    ;; Apparently this will insert words somewhere at point-max,
    ;; probably after any local sections, fix that.
    (let ((area (speck-local-positions))
	  to)
      (if (nth 1 area)
	  ;; Local Words section exists.
	  (let ((regexp
		 (concat "\\(?:" speck-save-words-regexp
			 "\\)\\(?::\\|[ \t]*([^)]+):\\)"))
		(bound (line-beginning-position 0))
		case-fold-search)
	    (setq to (point))
	    (while (re-search-backward regexp bound t)
	      ;; Search first entry.
	      (setq bound (line-beginning-position 0)))
	    ;; Delete all entries.
	    (delete-region (line-beginning-position) to)
	    (setq from (point)))
	;; No Local Words section exists.
	(setq from (speck-min-of-list-and-point-max
		    (speck-local-positions)))))
    ;; Insert new one.
    (when base-list
      (save-restriction
	(narrow-to-region from from)
	(princ (sort base-list 'string-lessp) (current-buffer))
	(delete-char -1)
	(insert "\n")
	(goto-char (point-min))
	(delete-char 1)
	(let* ((string (concat speck-save-words-string ": "))
	       (fill-column (min 40 (- 80 (length string)))))
	  (fill-region (point-min) (point-max))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (insert string)
	    (comment-region
	     (line-beginning-position) (line-end-position))
	    (forward-line)))))
    (when other-lists
      (dolist (list other-lists)
	(save-restriction
	  (narrow-to-region (point) (point))
	  (princ (sort (cdr list) 'string-lessp) (current-buffer))
	  (delete-char -1)
	  (insert "\n")
	  (goto-char (point-min))
	  (delete-char 1)
	  (let* ((string
		  (concat speck-save-words-string " (" (car list) "): "))
		 (fill-column (min 40 (- 80 (length string)))))
	    (fill-region (point-min) (point-max))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (insert string)
	      (comment-region
	       (line-beginning-position) (line-end-position))
	      (forward-line))))))
    ;; The following is not needed for a final writing, but for doing
    ;; this interactively.
    (when (< from (point))
      (with-buffer-prepared-for-specking
       (add-text-properties from (point) '(specked t speck ==))))))

(defun speck-restore-words (&optional placebo)
  "Restore Local Words.
Optional argument `placebo' non-nil means just assign speck properties."
  (save-excursion
    (save-restriction
      (widen)
      (let ((bound (- (point-max) 3000))
	    (regexp speck-save-words-regexp)
	    case-fold-search dictionary from to)
	(goto-char (point-max))
	(while (and speck-save-confirmed
		    (re-search-backward regexp bound t)
		    (speck-save-confirm))
	  (setq regexp
		(regexp-quote (buffer-substring-no-properties
			       (line-beginning-position) (match-end 0))))
	  ;; Set `from' and `bound' for next search, and `to' unless set.
	  (setq from (line-beginning-position))
	  (unless to (setq to (line-beginning-position 2)))
	  (setq bound (line-beginning-position 0))
	  (save-restriction
	    (narrow-to-region (match-end 0) (line-end-position))
	    (goto-char (point-max))
	    (when (looking-back (concat comment-end-skip "[ \t]*"))
	      ;; Strip comment end sequence.
	      (narrow-to-region (point-min) (match-beginning 0)))
	    (goto-char (point-min))
	    ;; Check for dictionary.
	    (cond
	     ((looking-at ":[ \t]+")
	      (setq dictionary nil))
	     ((looking-at "[ \t]+(\\(\\w\\w[^)]*\\)):[ \t]+")
	      ;; No dictionary.
	      (setq dictionary (match-string-no-properties 1)))
	     (t (error "Malformed local words section")))
	    (goto-char (match-end 0))
	    (unless placebo
	      ;; Process remainder of line.
	      (while (re-search-forward "[^ \t\f]+" nil t)
		(speck-hash-restore
		 (buffer-substring-no-properties
		  (match-beginning 0) (match-end 0))
		 (or dictionary 'file)))))
	  ;; Search again.
	  (goto-char from))
	(when to
	  ;; Mark local words as specked and ignorable.
	  (with-buffer-prepared-for-specking
	   (add-text-properties from to '(specked t speck ==))))))))

(defun speck-local-positions ()
  "Get positions of file local specifications for current buffer.
Return value is a list of six elements:
 0. Position of last page break.
 1. Beginning of line following Local Words section.
 2. Start of Local Dictionary line.
 3. Start of Local Filter Mode line.
 4. Start of Local Variables section.
 5. Position before comments within 3000 characters limit or `point-max'
    (nil if at least one of the preceding elements is non-nil)."
  (let ((case-fold-search t)
	;; case-folding is needed because `hack-local-variables' seeems
	;; to allow it, it's not awfully nice, though.
	(regexp
	 (concat "\\(^\^L\\)"
		 "\\|\\(" speck-save-words-regexp "\\)"
		 "\\|\\(" speck-save-dictionary-regexp "\\)"
		 "\\|\\(" speck-save-filter-mode-regexp "\\)"
		 "\\|\\(Local " "Variables:\\)"))
	(bound (max (- (point-max) 3000) (point-min)))
	page-break words dictionary filter-mode variables state)
    (save-excursion
      (save-restriction
	(widen)
	(narrow-to-region bound (point-max))
	(goto-char (point-max))
	(set-match-data nil)
	(while (and (not (and words dictionary filter-mode variables))
		    (re-search-backward regexp nil t))
	  (cond
	   ((match-beginning 1)
	    ;; A page-break outside any comment or string.
	    (unless (or page-break
			(let ((state (syntax-ppss (point))))
			  (or (nth 3 state) (nth 4 state))))
	      (setq page-break (line-beginning-position 2))))
	   ((match-beginning 2)
	    (unless words
	      (setq words (line-beginning-position 2))))
	   ((match-beginning 3)
	    (setq dictionary (line-beginning-position)))
	   ((match-beginning 4)
	    (setq filter-mode (line-beginning-position)))
	   ((match-beginning 5)
	    (setq variables (line-beginning-position)))))
	(if (or page-break words dictionary filter-mode variables)
	    (list page-break words dictionary filter-mode variables nil)
	  (widen)
	  (narrow-to-region bound (point-max))
	  (goto-char (point-max))
	  (forward-comment (- (buffer-size)))
	  ;; Are we always outside a comment here?
	  (skip-chars-forward " \n\t\f")
	  (skip-chars-backward " \t")
	  (list nil nil nil nil nil
		(if (and (bolp)
			 ;; Avoid messing up an existing comment.
			 (not (nth 4 (syntax-ppss (point)))))
		    (point)
		  ;; Before all comments at buffer-end within 3000
		  ;; characters limit (hopefully).
		  (point-max))))))))

(defun speck-min-of-list-and-point-max (list)
  "Return minimum of elements of list LIST.
Ignores elements whose value is nil.  When all elemensts are nil
return `point-max' of current buffer."
  (let ((min (point-max)))
    (dolist (elt list min)
      (when elt (setq min (min elt min))))))

(defun speck-save-options ()
  "Save speck options."
  (unless (eq speck-dictionary speck-saved-dictionary)
    (speck-save-variable
     speck-save-dictionary-regexp speck-save-dictionary-string
     speck-dictionary 2)
    (setq speck-saved-dictionary speck-dictionary))
  (unless (eq speck-filter-mode speck-saved-filter-mode)
    (speck-save-variable
     speck-save-filter-mode-regexp speck-save-filter-mode-string
     (symbol-name speck-filter-mode) 3)
    (setq speck-saved-filter-mode speck-filter-mode)))

(defun speck-save-variable (regexp string value elt)
  "Save local variable.
REGEXP must denote the corresponding \"Local ...\" text, STRING
the \"Local ...\" string to insert, VALUE the value, and ELT the
element position \(counting from zero\) returned by
`speck-local-positions'."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((area (speck-local-positions))
	     (min-of (speck-min-of-list-and-point-max area))
	     at)
	(narrow-to-region
	 (or (nth elt area) min-of)
	 (if (nth elt area)
	     (save-excursion
	       (goto-char (nth elt area))
	       (line-beginning-position 2))
	   min-of))
	(goto-char (point-min))
	(if (re-search-forward regexp nil t)
	    ;; Replace old value.
	    (progn
	      (delete-region
	       (progn (skip-chars-forward " \t") (point))
	       (progn (skip-chars-forward "^ \t\n") (point)))
	      (setq at (point))
	      (insert value)
	      (with-buffer-prepared-for-specking
	       (speck-multi-set-region at (point) '--)))
	  ;; Insert new value
	  (setq at (point))
	  (insert string " " value)
	  (let* ((comment-style 'plain)
		 (comment-start (or comment-start " ")))
	    (comment-region (line-beginning-position) (point))
	    (unless (bolp) (insert "\n"))
	    (with-buffer-prepared-for-specking
	     (speck-multi-set-region at (point) '--))))))))

(defun speck-restore-options (&optional placebo)
  "Restore variables from file.
Optional argument `placebo' non-nil means just assign speck
properties."
  ;; placebo non-nil might clash with confirming, not serious but ...
  (let ((bound (- (point-max) 3000))
	dictionary filter-mode)
    (save-excursion
      ;; Dictionary.
      (goto-char (point-max))
      (when (and speck-save-confirmed
		 (re-search-backward
		  (concat "\\(?:" speck-save-dictionary-regexp
			  "\\) *\\([^ \t\f\n\"]+\\)") bound t)
		 (speck-save-confirm))
	(with-buffer-prepared-for-specking
	 (add-text-properties
	  (line-beginning-position) (line-end-position)
	  '(specked t speck ==)))
	(unless placebo
	  (setq dictionary (match-string-no-properties 1))
	  (cond
	   ((assoc dictionary (speck-aspell-dictionary-names-and-aliases))
	    (setq speck-saved-dictionary (intern dictionary)))
	   ((catch 'found
	      (dolist (dictionary (speck-aspell-dictionary-names-and-aliases))
		(when (member speck-saved-dictionary (nth 2 dictionary))
		  (setq speck-saved-dictionary (intern (nth 0 dictionary)))
		  (throw 'found t)))))
	   ((assoc dictionary (speck-ispell-dictionary-alist))
	    (setq speck-saved-dictionary (intern dictionary)))
	   ((let ((entry (rassoc dictionary (speck-ispell-dictionary-alist))))
	      (when entry
		(setq speck-saved-dictionary (intern (car entry))))))
	   (t
	    (message "Invalid local dictionary %s" dictionary)))))
      ;; Filter-mode.
      (goto-char (point-max))
      (when (and speck-save-confirmed
		 (re-search-backward
		  (concat "\\(?:" speck-save-filter-mode-regexp
			  "\\) *\\([^ \t\f\n\"]+\\)") bound t)
		 (speck-save-confirm))
	(with-buffer-prepared-for-specking
	 (add-text-properties
	  (line-beginning-position) (line-end-position)
	  '(specked t speck ==)))
	(unless placebo
	  (setq speck-saved-filter-mode (intern (match-string-no-properties 1))))))))

;;;				 provide us					
(provide 'speck)

;;; speck.el ends here
