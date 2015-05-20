;;; apu.el --- Apropos Unicode characters.
;;
;; Filename: apu.el
;; Description: Apropos Unicode characters.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2015, Drew Adams, all rights reserved.
;; Created: Thu May  7 14:08:38 2015 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Tue May 19 21:48:40 2015 (-0700)
;;           By: dradams
;;     Update #: 497
;; URL: http://www.emacswiki.org/apu.el
;; Doc URL: http://www.emacswiki.org/AproposUnicode
;; Other URL: http://en.wikipedia.org/wiki/The_World_of_Apu ;-)
;; Keywords: unicode, characters, encoding, commands, ucs-names
;; Compatibility: GNU Emacs: 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `button', `cl', `cl-lib', `descr-text', `descr-text+', `gv',
;;   `help-fns', `help-fns+', `help-mode', `info', `macroexp',
;;   `naked', `wid-edit', `wid-edit+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Apropos Unicode characters.
;;
;;  Command `apropos-unicode' shows you the Unicode characters that
;;  match an apropos pattern you specify: a regexp or a
;;  space-separated list of words.  The characters whose names match
;;  are shown in a help buffer, along with the names and code points
;;  (decimal and hex).
;;
;;  In the help buffer, you can use these keys to act on the character
;;  described on the current line:
;;
;;   * `RET' or `mouse-2' - see detailed information about it.
;;   * `^' - insert it in the buffer where you invoked
;;           `apropos-unicode'.
;;   * `c' - define a command to insert the character, having the same
;;           name.  (You need library `ucs-cmds.el' for this.)
;;   * `i' - google for more information about it.
;;   * `k' - globally bind a key to insert it.
;;   * `l' - locally bind a key to insert it.
;;   * `z' - show it in a zoomed tooltip.
;;   * `C-y' - copy it to the `kill-ring'.
;;   * `M-y' - copy it to the secondary selection.
;;
;;  You can use options `apu-match-words-exactly-flag' and
;;  `apu-match-two-or-more-words-flag' to specify your preference for
;;  the kind of word matching to use by default.  You can match each
;;  word or only any two or more words.  If matching each word, you
;;  can match them as substrings or as full words.  You can use `C-c
;;  n' to refresh the matches, cycling among these word-match methods.
;;
;;  You can sort the list of matches by any of the columns, up or
;;  down, by clicking its heading.
;;
;;
;;  Commands defined here:
;;
;;    `apropos-unicode', `apu-char-codepoint-at-point',
;;    `apu-char-name-at-point', `apu-chars',
;;    `apu-chars-matching-full-words',
;;    `apu-chars-matching-two-or-more-words',
;;    `apu-chars-matching-words-as-substrings', `apu-chars-narrow',
;;    `apu-chars-next-match-method', `apu-copy-char-at-point-as-kill',
;;    `apu-copy-char-here-as-kill',
;;    `apu-copy-char-at-point-to-second-sel',
;;    `apu-copy-char-here-to-second-sel', `apu-define-insert-command',
;;    `apu-global-set-insertion-key', `apu-google-char',
;;    `apu-local-set-insertion-key', `apu-mode',
;;    `apu-chars-refresh-matching-as-substrings',
;;    `apu-chars-refresh-matching-full-words',
;;    `apu-chars-refresh-matching-two-or-more-words',
;;    `apu-chars-refresh-with-next-match-method',
;;    `apu-show-char-details', `apu-zoom-char-here',
;;    `apu-zoom-char-at-point'.
;;
;;  User options defined here:
;;
;;    `apu-match-only-displayable-chars-flag',
;;    `apu-match-two-or-more-words-flag',
;;    `apu-match-words-exactly-flag', `apu-synonyms'.
;;
;;  Non-interactive functions defined here:
;;
;;    `apu-char-at-point', `apu-char-displayable-p', `apu-char-here',
;;    `apu-char-name-here', `apu-char-string-here',
;;    `apu-chars-read-pattern-arg', `apu-compute-matches',
;;    `apu-copy-char-to-second-sel', `apu-filter',
;;    `apu-full-word-match', `apu-make-tablist-entry',
;;    `apu-match-type-msg', `apu-print', `apu-remove-if-not',
;;    `apu-sort-char', `apu-substring-match', `apu-tablist-entries'.
;;
;;  Internal variables defined here:
;;
;;    `apu--buffer-invoked-from', `apu-latest-pattern-set',
;;    `apu--matches', `apu--match-two-or-more', `apu--match-type',
;;    `apu--match-words-exactly', `apu--orig-buffer', `apu--patterns'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015/05/17 dadams
;;     Added: apu-latest-pattern-set, apu--match-type, apu--patterns, apu-chars-next-match-method,
;;            apu-chars-matching-full-words, apu-chars-matching-two-or-more-words,
;;            apu-chars-matching-words-as-substrings, apu-chars-refresh-with-next-match-method,
;;            apu-chars-refresh-matching-full-words, apu-chars-refresh-matching-two-or-more-words,
;;            apu-chars-read-pattern-arg, apu-print, apu-compute-matches, apu-filter, apu-chars-narrow.
;;     Bind apu-chars-refresh* to C-c (n|s|w|2).
;;     Renamed apu--insertion-buffer to apu--buffer-invoked-from.
;;     Removed apu-revert-buffer.
;;     apu-chars: Factored code out, to new functions.  Removed arg.
;;     apu-match-type-msg: Test apu--match-type.
;;     apu-tablist-entries: Use apu-compute-matches.
;; 2015/05/15 dadams
;;     Added: apu-match-only-displayable-chars-flag, apu-match-words-exactly-flag, apu-synonyms,
;;            apu--match-two-or-more, apu--match-words-exactly, apu--pattern, apu-full-word-match
;;            apu-sort-char, apu-tablist-entries, apu-char-displayable-p, apu-make-tablist-entry,
;;            apu-revert-buffer, apu-match-type-msg.
;;     Renamed: apu-match-word-pairs-only-flag to apu-match-two-or-more-words-flag,
;;              apu-match to apu-substring-match,
;;              apu-(orig|insertion)-buffer to apu--(orig|insertion)-buffer.
;;     apu-mode: Derive from tabulated-list-mode, not special-mode, so no longer usable for Emacs 23.
;;     apu-mode-map:
;;       Bind apu-copy-char-here-as-kill to C-y, not M-w, so can copy region normally.
;;       Bind g to apu-revert-buffer, i to apu-google-char.
;;     apu-chars: Respect apu--match-words-exactly.
;; 2015/05/12 dadams
;;     apu-show-char-details: Corrected position to bol.
;; 2015/05/10 dadams
;;     Added: apu-zoom-char-here, apu-zoom-char-at-point.  Bound apu-zoom-char-here to z.
;;     apu-chars: Use delete-if-not correctly.
;;     apu-show-char-details: Use describe-char, not what-cursor-position.
;; 2015/05/09 dadams
;;     Added: apu-match, apu-match-word-pairs-only-flag, defgroup, and apu-delete-if-not.
;;     apu-chars: Respect apu-match-word-pairs-only-flag: Match all words by default.
;; 2015/05/08 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'descr-text+ nil t) ; Soft-requires `help-fns+.el'. Help on `keymap' prop for `C-u C-x ='.

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup apu nil
  "Apropos Unicode characters."
  :prefix "apu-"
  :group 'i18n :group 'help :group 'matching :group 'editing :group 'convenience
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
apu.el bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "http://www.emacswiki.org/apu.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/AproposUnicode")
  :link '(emacs-commentary-link :tag "Commentary" "apu"))

(defcustom apu-match-only-displayable-chars-flag t
  "Non-nil means filter out char not displayable in the current font."
  :type 'boolean :group 'apu)

(defcustom apu-match-two-or-more-words-flag nil
  "Non-nil means match a word-list pattern like ordinary `apropos' commands.
A nil value requires each word in the list to be matched.  A non-nil
value does not require each word to be matched.  It requires only each
pair of words in the list to be matched.

For example, if non-nil then instead of matching each of the words
`greek', `small', and `letter' in any order, it actually matches each
pair of these words (in both pair orders), so you get some results
that match only two of the three words.  This is probably not what you
want in most cases, so nil is the default value."
  :type 'boolean :group 'apu)

(defcustom apu-match-words-exactly-flag nil
  "Non-nil means match the words in a word-list pattern exactly.
If nil, use substring matching for them (like the `apropos' commands)."
  :type 'boolean :group 'apu)

(defcustom apu-synonyms ()
;;;   '(("lowercase" "small letter")
;;;     ("uppercase" "capital letter")
;;;     ("brace"     "curly bracket"))
  "List of synonyms when `apu-match-two-or-more-words-flag' is non-nil.
Used only for matching only two or more words, not when matching each
word input.

Each entry is a list of two or more names (strings) that you might
want to treat as synonyms for matching.  At least one of the strings
should match a character name.

However, the more you define synonyms for convenience the more
performance is impacted."
  :type '(alist :key-type string :value-type (cons string sexp))
  :group 'apu)

;;; Global variables -------------------------------------------------

(defvar apu-latest-pattern-set ()
  "Latest set of patterns used for matching by `apu-chars'.")

(defvar apu--match-type 'MATCH-WORDS-AS-SUBSTRINGS ; @@@ GLOBAL
  "How the current `apu-char*' command matches a word-list pattern.
This has no effect when the current input pattern is a regexp.

Possible values and their behaviors:
* `MATCH-WORDS-AS-SUBSTRINGS': like nil `apu-match-words-exactly-flag'
* `MATCH-WORDS-EXACTLY':   like non-nil `apu-match-words-exactly-flag'
* `MATCH-TWO-OR-MORE': like non-nil `apu-match-two-or-more-words-flag'

Any other value acts like `MATCH-WORDS-AS-SUBSTRINGS'")

(defvar apu--orig-buffer nil
  "Buffer current when `apu-chars' was last invoked.")

;;; Buffer-local variables -------------------------------------------

(defvar apu--buffer-invoked-from nil
  "Buffer current when `apu-chars' was invoked to produce this output.
Used in a list buffer to point to where it was invoked.")
(make-variable-buffer-local 'apu--buffer-invoked-from)
(put 'apu--buffer-invoked-from 'permanent-local t)

(defvar apu--matches ()
  "Result of matching character names in `apu--buffer-invoked-from'.
A cons whose car is the maximum width of the matching character names
and whose cdr is an alist of matches having the form of `ucs-names'.")
(make-variable-buffer-local 'apu--matches)
(put 'apu--matches 'permanent-local t)  ; Use only in list-output buffer.

(defvar apu--match-two-or-more apu-match-two-or-more-words-flag
  "Whether currently matching word-list elements by pairs.
Default value is from `apu-match-two-or-more-words-flag'.")
(make-variable-buffer-local 'apu--match-two-or-more)
(put 'apu--match-two-or-more 'permanent-local t)

(defvar apu--match-words-exactly apu-match-words-exactly-flag
  "Whether currently matching word-list elements exactly.
Default value is from `apu-match-words-exactly-flag'.")
(make-variable-buffer-local 'apu--match-words-exactly)
(put 'apu--match-words-exactly 'permanent-local t)

(defvar apu--patterns ()
  "Patterns currently used by `apropos-unicode' to match Unicode chars.")
(make-variable-buffer-local 'apu--patterns)
(put 'apu--patterns 'permanent-local t)

;;; Commands ---------------------------------------------------------

;;;###autoload
(defun apu-chars-next-match-method ()
  "Cycle among the methods of matching character names."
  (interactive)
  (call-interactively
   (case apu--match-type
     (MATCH-WORDS-AS-SUBSTRINGS #'apu-chars-matching-two-or-more-words)
     (MATCH-TWO-OR-MORE         #'apu-chars-matching-full-words)
     (t                         #'apu-chars-matching-words-as-substrings))))

;;;###autoload
(defun apu-chars-matching-full-words ()
  "Show all Unicode chars whose names match a pattern you type.
Same as `apropos-unicode' with a non-nil value of
`apu-match-words-exactly-flag': match each entry of a word-list
pattern as a full word."
  (interactive)
  (setq apu-latest-pattern-set  (list (apu-chars-read-pattern-arg)))
  (let ((list-buf  (get-buffer-create (format "*`%S' Matches*" apu-latest-pattern-set))))
    (add-to-list 'apu--pats+bufs (cons apu-latest-pattern-set list-buf))
    (with-current-buffer list-buf (setq apu--patterns apu-latest-pattern-set)))
  (setq apu--match-type  'MATCH-WORDS-EXACTLY)
  (apu-chars))

;;;###autoload
(defun apu-chars-matching-two-or-more-words ()
  "Show all Unicode chars whose names match a pattern you type.
Same as `apropos-unicode' with a nil value of
`apu-match-two-or-more-words-flag': match two or more entries of a
word-list pattern, as a full word."
  (interactive)
  (setq apu-latest-pattern-set  (list (apu-chars-read-pattern-arg)))
  (let ((list-buf  (get-buffer-create (format "*`%S' Matches*" apu-latest-pattern-set))))
    (add-to-list 'apu--pats+bufs (cons apu-latest-pattern-set list-buf))
    (with-current-buffer list-buf (setq apu--patterns apu-latest-pattern-set)))
  (setq apu--match-type  'MATCH-TWO-OR-MORE)
  (apu-chars))

;;;###autoload
(defun apu-chars-matching-words-as-substrings ()
  "Show all Unicode chars whose names match a pattern you type.
Same as `apropos-unicode' with a nil value of
`apu-match-words-exactly-flag': match each entry of a word-list
pattern as a substring."
  (interactive)
  (setq apu-latest-pattern-set  (list (apu-chars-read-pattern-arg)))
  (let ((list-buf  (get-buffer-create (format "*`%S' Matches*" apu-latest-pattern-set))))
    (add-to-list 'apu--pats+bufs (cons apu-latest-pattern-set list-buf))
    (with-current-buffer list-buf (setq apu--patterns apu-latest-pattern-set)))
  (setq apu--match-type  'MATCH-WORDS-AS-SUBSTRINGS)
  (apu-chars))

(defun apu-chars-refresh-with-next-match-method ()
  "Refresh matches for the same pattern, but using the next matching method."
  (interactive)
  (call-interactively
   (case apu--match-type
     (MATCH-WORDS-AS-SUBSTRINGS #'apu-chars-refresh-matching-two-or-more-words)
     (MATCH-TWO-OR-MORE         #'apu-chars-refresh-matching-full-words)
     (t                         #'apu-chars-refresh-matching-as-substrings))))

(defun apu-chars-refresh-matching-full-words ()
  "Refresh matches for the same pattern, but match full words.
I.e., match again, as if `apu-match-words-exactly-flag' were non-nil.
Does nothing if current pattern is a regexp instead of a word list."
  (interactive)
  (setq apu--match-type  'MATCH-WORDS-EXACTLY)
  (with-current-buffer apu--buffer-invoked-from (apu-print))
  (apu-match-type-msg))

(defun apu-chars-refresh-matching-as-substrings ()
  "Refresh matches for the same pattern, but match as substrings.
I.e., match again, as if `apu-match-words-exactly-flag' were nil.
Does nothing if current pattern is a regexp instead of a word list."
  (interactive)
  (setq apu--match-type  'MATCH-WORDS-AS-SUBSTRINGS)
  (with-current-buffer apu--buffer-invoked-from (apu-print))
  (apu-match-type-msg))

(defun apu-chars-refresh-matching-two-or-more-words ()
  "Refresh matches for the same pattern, but match two or more words.
I.e., match again, as if `apu-match-two-or-more-words-flag' were t.
Does nothing if current pattern is a regexp instead of a word list."
  (interactive)
  (setq apu--match-type  'MATCH-TWO-OR-MORE)
  (with-current-buffer apu--buffer-invoked-from (apu-print))
  (apu-match-type-msg))

(define-derived-mode apu-mode tabulated-list-mode "Apropos Unicode"
  "Major mode for `apropos-unicode' output.
\\{apu-mode-map}"
  (update-glyphless-char-display 'glyphless-char-display-control glyphless-char-display-control))

(when (featurep 'ucs-cmds)
  (define-key apu-mode-map "c"         'apu-define-insert-command))
(define-key apu-mode-map   "i"         'apu-google-char)
(define-key apu-mode-map   "k"         'apu-global-set-insertion-key)
(define-key apu-mode-map   "l"         'apu-local-set-insertion-key)
(define-key apu-mode-map   "z"         'apu-zoom-char-here)
(define-key apu-mode-map   "^"         'apu-insert-char)
(define-key apu-mode-map (kbd "C-c n") 'apu-chars-refresh-with-next-match-method)
(define-key apu-mode-map (kbd "C-c s") 'apu-chars-refresh-matching-as-substrings)
(define-key apu-mode-map (kbd "C-c w") 'apu-chars-refresh-matching-full-words)
(define-key apu-mode-map (kbd "C-c 2") 'apu-chars-refresh-matching-two-or-more-words)
(define-key apu-mode-map (kbd "RET")   'apu-show-char-details)
(define-key apu-mode-map [mouse-2]     'apu-show-char-details)
(define-key apu-mode-map (kbd "C-y")   'apu-copy-char-here-as-kill)
(when (featurep 'second-sel)
  (define-key apu-mode-map (kbd "M-y") 'apu-copy-char-here-to-second-sel))

;;;###autoload
(defun apu-char-name-at-point (&optional position msgp) ; Not bound.
  "Return the name of the Unicode character at point, or nil if none.
Non-nil POSITION means use the character at POSITION."
  (interactive "d\np")
  (apu-char-at-point 'name position msgp))

;;;###autoload
(defun apu-char-codepoint-at-point (&optional position msgp) ; Not bound.
  "Return the codepoint of the Unicode char at point, or nil if none.
Non-nil POSITION means use the character at POSITION."
  (interactive "d\np")
  (apu-char-at-point 'code position msgp))

(defun apu-char-at-point (return-type position msgp)
  "Return the name or codepoint of the Unicode char at point."
  (let* ((name+code  (rassq (char-after position) (ucs-names)))
         (name       (car name+code))
         (code       (cdr name+code)))
    (unless name (error "No Unicode char here"))
    (prog1 (if (eq return-type 'name) name code)
      (when msgp (message "Char: `%s', Codepoint: `%d' (`%#x')" name code code)))))

(defun apu-char-here ()
  "Return the Unicode character described on this line."
  (string-to-char (apu-char-string-here)))

(defun apu-char-string-here ()
  "Return the Unicode character described on this line, as a string."
  (buffer-substring (line-beginning-position) (1+ (line-beginning-position))))

;;;###autoload
(defun apu-copy-char-at-point-as-kill (&optional msgp) ; Not bound.
  "Copy the character at point to the `kill-ring'."
  (interactive "p")
  (let ((strg  (string (char-after))))
    (kill-new strg)
    (when msgp (message "Copied char `%s' to kill ring" strg))))

(defun apu-copy-char-here-as-kill (&optional msgp)
  "Copy the Unicode character described on this line to the `kill-ring'."
  (interactive "p")
  (let ((strg  (string (apu-char-here))))
    (kill-new strg)
    (when msgp (message "Copied char `%s' to kill ring" strg))))

;;;###autoload
(defun apu-copy-char-at-point-to-second-sel (&optional msgp) ; Not bound.
  "Copy the character at point to the secondary selection.
If you have library `second-sel.el' then also copy it to the
`secondary-selection-ring'."
  (interactive "p")
  (apu-copy-char-to-second-sel (point) msgp))

(defun apu-copy-char-here-to-second-sel (&optional msgp)
  "Copy Unicode char described on this line to the secondary selection.
If you have library `second-sel.el' then also copy it to the
`secondary-selection-ring'."
  (interactive "p")
  (apu-copy-char-to-second-sel (line-beginning-position) msgp))

(defun apu-copy-char-to-second-sel (position msgp)
  "Copy char at POSITION in current buffer to secondary selection.
If you have library `second-sel.el' then this also copies it to the
`secondary-selection-ring'."
  (let* ((char  (char-after position))
         (strg  (string char)))
    (x-set-selection 'SECONDARY strg)
    (if mouse-secondary-overlay
        (move-overlay mouse-secondary-overlay position (1+ position) (current-buffer))
      (setq mouse-secondary-overlay  (make-overlay position (1+ position) (current-buffer)))
      (overlay-put mouse-secondary-overlay 'face 'secondary-selection))
    (when (require 'second-sel nil t) (add-secondary-to-ring strg))
    (when msgp (message "Copied char `%s' to secondary selection ring" strg))))

(defun apu-define-insert-command ()
  "Define a command that inserts the character described on this line.
The command name is the lowercase Unicode character name, with spaces
 replaced by hyphens.
This command requires library `ucs-cmds.el'."
  (interactive)
  (unless (require 'ucs-cmds nil t) (error "This command requires library `ucs-cmds.el'"))
  (ucsc-define-char-insert-cmd (apu-char-here) 'MSG))

(defun apu-global-set-insertion-key (key &optional msgp)
  "Globally bind a key to insert the character described on this line."
  (interactive "KKey to bind globally: \np")
  (let ((char  (apu-char-string-here)))
    (global-set-key key char)
    (when msgp (message "`%s' will now insert `%s' globally" (key-description key) char))))

(defun apu-google-char (&optional msgp)
  "Google the Unicode character described on this line."
  (interactive "p")
  (browse-url (format "https://www.google.com/search?ion=1&q=%s"
                      (url-hexify-string (concat "UNICODE " (apu-char-name-here))))))

(defun apu-char-name-here ()
  "Return the name of the Unicode char described on this line, as a string."
  (car (rassq (apu-char-here) (ucs-names))))

(defun apu-insert-char (buffer &optional msgp)
  "Insert the Unicode character described on this line at point in BUFFER.
By default, BUFFER is the buffer that was current when
`apropos-unicode' was invoked.  With a prefix arg you are prompted for
the buffer to use instead."
  (interactive (list (if current-prefix-arg
                         (read-buffer "Insert in buffer: " apu--buffer-invoked-from 'REQUIRE-MATCH)
                       apu--buffer-invoked-from)
                     t))
  (let ((char  (apu-char-string-here)))
    (with-current-buffer buffer (insert char))
    (when msgp (message "Inserted `%s' in buffer `%s'" char (buffer-name buffer)))))

(defun apu-local-set-insertion-key (key &optional msgp)
  "Locally bind a key to insert the character described on this line."
  (interactive "KKey to bind locally: \np")
  (let ((char  (apu-char-string-here)))
    (local-set-key key char)
    (when msgp (message "`%s' will now insert `%s' locally" (key-description key) char))))

(defun apu-show-char-details (&optional event)
  "Show details about Unicode character described on this line."
  (interactive (list last-nonmenu-event))
  (run-hooks 'mouse-leave-buffer-hook)
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (save-excursion (goto-char (posn-point (event-start event)))
                    (describe-char (line-beginning-position)))))

(defun apu-zoom-char-here (&optional height)
  "Show the char described on the current line in a zoomed tooltip.
With a numerical prefix arg, show it that many times larger."
  (interactive (list (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))))
  (unless height (setq height  1))
  (x-show-tip (propertize (apu-char-string-here)
                          'face `(:foreground "red" :height ,(* 200 height)))))

;;;###autoload
(defun apu-zoom-char-at-point (&optional height position)
  "Show the Unicode char at point in a zoomed tooltip.
With a numerical prefix arg, show it that many times larger.
Non-nil POSITION means use the character at POSITION."
  (interactive (list (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))))
  (unless height (setq height  1))
  (x-show-tip (propertize (char-to-string (char-after position))
                          'face `(:foreground "red" :height ,(* 200 height)))))

;;;###autoload
(defalias 'apropos-unicode 'apu-chars)
;;;###autoload
(defun apu-chars ()
  "Show all Unicode chars whose names match PATTERN.
PATTERN is as for command `apropos': a word, a list of words
 \(separated by spaces), or a regexp (using some regexp special
 characters).  If it is a word, search for matches for that word as a
 substring.  If it is a list of words, search for matches for any two
 \(or more) of those words.

The output is in `apu-mode'.

Non-nil option `apu-match-only-displayable-chars-flag' means match
only characters that are `char-displayable-p' in the buffer where
`apu-chars' is invoked.  This is the default.  A nil value means match
all Unicode characters.

Non-nil option `apu-match-words-exactly-flag' means each word in a
word-list pattern must match exactly, as a full word.  A nil
value (the default) means each word must match only as a substring.

Non-nil option `apu-match-two-or-more-words-flag' means only two or
more words of a word-list pattern must match (exactly, as full words).
A nil value (the default) means *each* word must match.  A non-nil
value gives the same behavior as the ordinary `apropos' commands.

For example, if `apu-match-two-or-more-words-flag' is non-nil then
instead of trying to match each of the words `greek', `small', and
`letter' in any order, the command tries to match only each pair of
these words (in both pair orders), so you get some results that match
only two of the three words.  This might not be what you want in most
cases.

You can use `C-c n' to refresh the matches using the next match type.
Instead of cycling among match types this way, you can change the
match type directly during refresh, as follows:

 C-c s\t- Use substring matching
 C-c 2\t- Use pairwise full-word matching (match two or more words)
 C-c w\t- Use full-word matching for each list entry

See also these global commands, which use the different match
methods (without changing the option values):
`apu-chars-matching-words-as-substrings'
`apu-chars-matching-full-words' `apu-chars-matching-two-or-more-words'

Simple tips for matching some common Unicode character names:
* You can match chars that have a given base char, such as `e', by
  using a regexp ` \(BASE-CHAR \|$\)'.  That matches BASE-CHAR after a
  `SPC' char and before a `SPC' char or at the end of the line.
* You can use `small letter' to match lowercase letters, and `capital
  letter' to match capital letters.  Just `small' matches lots of
  chars that are not letters.  Just `capital' matches chars that
  include capital letters that serve as math symbols and such."
  (interactive (prog1 ()
                 (setq apu-latest-pattern-set  (list (apu-chars-read-pattern-arg)))
                 (let ((list-buf  (get-buffer-create (format "*`%S' Matches*"
                                                             apu-latest-pattern-set))))
                   (add-to-list 'apu--pats+bufs (cons apu-latest-pattern-set list-buf))
                   (with-current-buffer list-buf (setq apu--patterns apu-latest-pattern-set)))))
  (setq apu--orig-buffer  (current-buffer)
        apu--refresh-p    t)
  (when (called-interactively-p 'interactive)
    (setq apu--match-type  (if apu-match-two-or-more-words-flag
                               'MATCH-TWO-OR-MORE
                             (if apu-match-words-exactly-flag
                                 'MATCH-WORDS-EXACTLY
                               'MATCH-WORDS-AS-SUBSTRINGS))))
  (apu-print)
  (apu-match-type-msg))

(defun apu-chars-read-pattern-arg ()
  "Read a pattern arg for `apu-chars*' commands."
  (let ((pat  (read-string "Search for Unicode char (word list or regexp): ")))
    (if (string-equal (regexp-quote pat) pat)
        (or (split-string pat "[ \t]+" t)  (user-error "No word list given"))
      pat)))

(defun apu-match-type-msg ()
  "Message stating current match type."
  (message (case apu--match-type
             (MATCH-TWO-OR-MORE    "Matching at least TWO entries as full words now")
             (MATCH-WORDS-EXACTLY  "Matching EACH entry as a FULL WORD now")
             (t                    "Matching EACH entry as a SUBSTRING now"))))

(defun apu-print ()
  "Show matches in current buffer for the apropos Unicode commands."
  (let* ((bufs  apu--pats+bufs)
         buf)
    (while (and bufs  (or (not (equal apu-latest-pattern-set (caar bufs)))
                          (not (buffer-live-p (cdar bufs)))))
      (setq bufs  (cdr bufs)))
    (unless bufs (error "No match")) ; Just punting.  This should not happen (?).
    (setq buf  (cdar bufs))
    (with-help-window (buffer-name buf)
      (with-current-buffer buf
        (apu-mode)
        (goto-char (point-min))
        (setq case-fold-search          t
              apu--buffer-invoked-from  apu--orig-buffer
              tabulated-list-format     (vector '("Ch"       2 apu-sort-char)
                                                ;; Use 30 as a default name width.
                                                `("Name"     ,(or (car apu--matches)  30) t)
                                                '("Decimal"  7 apu-sort-char :right-align t)
                                                '("Hex"      8 apu-sort-char :right-align t))
              tabulated-list-sort-key   '("Decimal")
              tabulated-list-entries    #'apu-tablist-entries)
        (tabulated-list-print t)
        (tabulated-list-init-header)))))

(defun apu-tablist-entries ()     ; Invoked in the list-output buffer.
  "Function value for `tabulated-list-entries'.
See `apu-make-tablist-entry'."
  (when apu--refresh-p (apu-compute-matches))
  (mapcar #'apu-make-tablist-entry (cdr apu--matches)))

(defun apu-make-tablist-entry (char+code)
  "Return a tablist entry for CHAR+CODE.
This is a list (CODE [CHAR NAME DEC HEX]), where:
CODE is the character (an integer),
CHAR is its string representation,
NAME is its name,
DEC is its decimal representation,
HEX is its hexadecimal representation.

CHAR, NAME, DEC, and HEX are strings."
  (list (cdr char+code) (vector (char-to-string (cdr char+code)) (car char+code)
                                (format "%6d" (cdr char+code)) (format "%#8x" (cdr char+code)))))

(defun apu-compute-matches ()     ; Invoked in the list-output buffer.
  "Compute matches for apropos Unicode commands."
  (let ((patterns  apu--patterns))

    (message "Matching `%s'..." patterns)
    (ucs-names)
    (let* ((case-fold-search  t)
           (max-char          0)
           (chars+codes       ()))
      (dolist (pat  patterns)
        (setq chars+codes  (apu-filter pat chars+codes))
        (when apu-match-only-displayable-chars-flag
          (setq chars+codes  (apu-delete-if-not #'apu-char-displayable-p chars+codes)))
        (unless chars+codes (error "No characters match `%S'" patterns)))
      (dolist (char+code  chars+codes) (setq max-char  (max max-char (string-width (car char+code)))))
      (message "Matching `%s'...done" patterns)
      (setq apu--matches  (cons max-char chars+codes)))))

(defun apu-filter (pattern chars+codes)
  "Try to match PATTERN agains each element of alist CHARS+CODES.
PATTERN is as for `apu-chars'.
CHARS+CODES has the same form as `ucs-names'.
If CHARS+CODES is nil then match against `ucs-names'."
  (let ((match-fn  (if (eq apu--match-type 'MATCH-WORDS-EXACTLY)
                       #'apu-full-word-match
                     #'apu-substring-match)))
  (cond ((or (atom pattern)  (eq apu--match-type 'MATCH-TWO-OR-MORE))
         (require 'apropos) ; `apropos-parse-pattern', `apropos-regexp'.
         (let ((apropos-synonyms  apu-synonyms)) (apropos-parse-pattern pattern))
         (apu-remove-if-not (apply-partially #'apu-substring-match apropos-regexp)
                            (or chars+codes  ucs-names)))
        (t ; List of words, to be matched as full words or substrings.
         (let ((chs+cds  ())
               (first    (car pattern)))
           (dolist (c.c  (or chars+codes  ucs-names))
             (when (funcall match-fn first c.c) (push c.c chs+cds)))
           (dolist (word  (cdr pattern))
             (setq chs+cds  (apu-delete-if-not (apply-partially match-fn word) chs+cds)))
           chs+cds)))))

(defun apu-chars-narrow (pattern)
  "Narrow matches in current buffer to those also matching another PATTERN.
You are prompted for the PATTERN, which is as for `apu-chars'."
  (interactive (list (apu-chars-read-pattern-arg)))
  (let* ((orig-pats   apu--patterns)
         (bufs-entry  (rassoc (current-buffer) (with-current-buffer apu--buffer-invoked-from
                                                 apu--pats+bufs))))
    (add-to-list 'apu--patterns pattern 'APPEND)
    (when bufs-entry (setcar bufs-entry apu--patterns))) ; Update the entry in `apu--pats+bufs'.
  (let ((newbufname  (format "*`%S' Matches*" apu--patterns)))
    (when (get-buffer newbufname)
      (let ((kill-buffer-query-functions  ()))
        (kill-buffer (format "*`%S' Matches*" apu--patterns)))))
  (rename-buffer (format "*`%S' Matches*" apu--patterns))
  (let ((case-fold-search  t)
        (orig-chars+codes  (cdr apu--matches))
        (max-char          0)
        (match-fn          (if (eq apu--match-type 'MATCH-WORDS-EXACTLY)
                               #'apu-full-word-match
                             #'apu-substring-match)))
    (setcdr apu--matches (apu-filter pattern (cdr apu--matches)))
    (unless (cdr apu--matches) (error "No characters match `%s'" pattern))
    (dolist (char+code  (cdr apu--matches))
      (setq max-char  (max max-char (string-width (car char+code)))))
    (message "Matching `%s'...done" pattern)
    (setcar apu--matches max-char))
  (setq apu-latest-pattern-set  apu--patterns) ; For `apu-print'.
  (let ((apu--refresh-p  nil)) (with-current-buffer apu--buffer-invoked-from (apu-print)))
  (apu-match-type-msg))

(defun apu-full-word-match (pattern char+code)
  "Return non-nil if PATTERN matches the car of cons CHAR+CODE as a word.
PATTERN is assumed not to contain any regexp special characters."
  (let ((case-fold-search  t))
    (ignore-errors (string-match-p (format "\\b%s\\b" pattern) (car char+code)))))

(defun apu-substring-match (pattern char+code)
  "Return non-nil if regexp PATTERN matches the car of cons CHAR+CODE."
  (let ((case-fold-search  t))
    (ignore-errors (string-match-p pattern (car char+code)))))

(defun apu-sort-char (entry1 entry2)
  "Return non-nil if ENTRY1 > ENTRY2.
Each is a string representation of a number."
  (> (car entry1) (car entry2)))

;; Same as `icicle-remove-if-not' etc.
(defun apu-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ())) (dolist (x xs) (when (funcall pred x) (push x result))) (nreverse result)))

(defun apu-delete-if-not (predicate xs)
  "Remove all elements of list XS that satisfy PREDICATE.
This operation is destructive, reusing conses of XS whenever possible."
  (while (and xs  (not (funcall predicate (car xs))))
    (setq xs  (cdr xs)))
  (let ((cl-p  xs))
    (while (cdr cl-p)
      (if (not (funcall predicate (cadr cl-p))) (setcdr cl-p (cddr cl-p)) (setq cl-p  (cdr cl-p)))))
  xs)

(defun apu-char-displayable-p (char+code)
  "Invoke `char-displayable-p' on the cdr of CHAR+CODE."
  (char-displayable-p (cdr char+code)))

;;;;;;;;;;;;;;;;;;;;;;

(provide 'apu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apu.el ends here
