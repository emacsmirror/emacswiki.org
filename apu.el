;;; apu.el --- Apropos Unicode characters. -*- lexical-binding:t -*-
;;
;; Filename: apu.el
;; Description: Apropos Unicode characters.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2015-2018, Drew Adams, all rights reserved.
;; Created: Thu May  7 14:08:38 2015 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Feb 25 11:03:47 2018 (-0800)
;;           By: dradams
;;     Update #: 832
;; URL: https://www.emacswiki.org/emacs/download/apu.el
;; Doc URL: https://www.emacswiki.org/emacs/AproposUnicode
;; Other URL: https://en.wikipedia.org/wiki/The_World_of_Apu ;-)
;; Keywords: unicode, characters, encoding, commands, ucs-names
;; Compatibility: GNU Emacs: 24.x, 25.x, 26.x
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
;;  Command `apropos-char' (aka `apropos-unicode', aka `apu-chars')
;;  describes the Unicode characters that match an apropos pattern you
;;  specify: a regexp or a space-separated list of words.  The
;;  characters whose names or old names match are shown in a help
;;  buffer, along with the names (or old names) and code points
;;  (decimal and hex).
;;
;;  Command `describe-chars-in-region' (aka `apu-chars-in-region')
;;  describes the Unicode characters that are in the region.  By
;;  default, it shows each distinct character only once.  With a
;;  prefix argument it has a line describing each occurrence of each
;;  character in the region.
;;
;;  For each of these commmands, in the help buffer describing the
;;  characters you can use these keys to act on the character
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
;;  You can sort the list of matches by any of the columns, up or
;;  down, by clicking its heading.
;;
;;  For command `apropos-unicode', you can use options
;;  `apu-match-words-exactly-flag' and
;;  `apu-match-two-or-more-words-flag' to specify your preference for
;;  the kind of word matching to use by default.  You can match each
;;  word or only any two or more words.  If matching each word, you
;;  can match them as substrings or as full words.  You can use `C-c
;;  n' to refresh the matches, cycling among these word-match methods.
;;
;;  Non-`nil' option `apu-match-only-displayable-chars-flag' means
;;  that commands such as `apropos-unicode' display only Unicode chars
;;  that can be displayed in the current context.  Non-displayable
;;  chars are those that do not have displayable glyphs, in general,
;;  and those for which you do not have a font installed that can
;;  display them.  Displayability of a character is determined by
;;  standard Emacs function `char-displayable-p'.
;;
;;  NOTE:
;;
;;    Starting with Emacs 25, `char-displayable-p' can be EXTREMELY
;;    SLOW if the new variable `inhibit-compacting-font-caches' is
;;    `nil', which it is by default, and if you have many, or large,
;;    fonts installed.  For this reason the default value of
;;    `apu-match-only-displayable-chars-flag' is `nil' for Emacs 25
;;    and later.  This seems to be the case if you have MS Windows
;;    TrueType fonts installed, for instance.
;;
;;    If you want to be able to exclude non-displayable chars in Emacs
;;    25+, then set `apu-match-only-displayable-chars-flag' to
;;    non-`nil'.  If you find that this makes APU commands such as
;;    `apropos-char' extremely slow then set variable
;;    `inhibit-compacting-font-caches' to `t'.
;;
;;
;;  Commands defined here:
;;
;;    `apropos-char', `apu-chars-in-region', `apropos-unicode',
;;    `apu-char-codepoint-at-point', `apu-char-name-at-point',
;;    `apu-chars', `apu-chars-matching-full-words',
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
;;    `apu-chars-refresh-with-next-match-method', `apu-revert-buffer',
;;    `apu-show-char-details', `apu-zoom-char-here',
;;    `apu-zoom-char-at-point', `describe-chars-in-region'.
;;
;;  User options defined here:
;;
;;    `apu-match-only-displayable-chars-flag',
;;    `apu-match-two-or-more-words-flag',
;;    `apu-match-words-exactly-flag', `apu-synonyms'.
;;
;;  Non-interactive functions defined here:
;;
;;    `apu-add-to-pats+bufs', `apu-buf-name-for-matching',
;;    `apu-char-at-point', `apu-char-displayable-p', `apu-char-here',
;;    `apu-char-name', `apu-char-names', `apu-char-name-here',
;;    `apu-char-string-here', `apu-chars-narrow-1',
;;    `apu-chars-read-pattern-arg', `apu-compute-matches',
;;    `apu-copy-char-to-second-sel', `apu-filter',
;;    `apu-full-word-match', `apu-get-a-hash-key',
;;    `apu-get-hash-keys', `apu-hash-table-to-alist',
;;    `apu-make-tablist-entry', `apu-match-type-msg',
;;    `apu-print-apropos-matches', `apu-print-chars',
;;    `apu-remove-if-not', `apu-sort-char', `apu-substring-match',
;;    `apu-tablist-match-entries'.
;;
;;  Internal variables defined here:
;;
;;    `apu--buffer-invoked-from', `apu-latest-pattern-set',
;;    `apu--matches', `apu--match-two-or-more', `apu--match-type',
;;    `apu--match-words-exactly', `apu--orig-buffer',
;;    `apu--pats+bufs', `apu--patterns', `apu--patterns-not',
;;    `apu--refresh-p', `apu--unnamed-chars'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2018/02/25 dadams
;;     Added: apu-revert-buffer (bound to `g' in apu-mode-map).
;;     apu-match-only-displayable-chars-flag and commentary: Mention Emacs 25+ issues.
;;     apu-mode: Add some info to doc string.
;; 2018/02/20 dadams
;;     Added: apu-char-names, apu-hash-table-to-alist, apu-get-a-hash-key, apu-get-hash-keys.
;;     Use lexical binding: set file-local variable lexical-binding to t.
;;     apu-match-only-displayable-chars-flag:
;;       Because of Emacs bug #30539, change default value to nil for Emacs 25+, at least temporarily.
;;     apu-char-at-point, apu-char-name-here: Use apu-char-name - do not invoke ucs-names.
;;     apu-char-at-point: Make POSITION and MSGP optional.
;;     apu-google-char: Removed unused arg MSGP.
;;     apu-char-name: Added optional arg PREFER-OLD-NAME-P.
;;     apu-compute-matches: Use apu-char-displayable-p only once, and last (because slow).
;;     apu-filter: Handle hash-table version of ucs-names.
;;     apu-chars-narrow-1: Removed unused args orig-names+codes and match-fn.
;; 2016/12/10 dadams
;;     apu-copy-char-to-second-sel: x-set-selection -> gui-set-selection for Emacs 25+.
;; 2015/06/21 dadams
;;     Added: apu--patterns-not, apu-add-to-pats+bufs, apu-buf-name-for-matching, apu-chars-narrow-1,
;;            apu-delete-if, apu-remove-if.
;;     apu--pats+bufs, apu-latest-pattern-set: Added exclusion patterns.
;;     apu-chars-matching-*, apropos-char:
;;       apu--patterns is now just the car of apu-latest-pattern-set.  apu-latest-pattern-set includes
;;       place for exclusions.  Use apu-buf-name-for-matching and apu-add-to-pats+bufs.
;;     apu-compute-matches: Handle apu--patterns-not also.
;;     apu-filter: Added optional arg NOTP.
;;     apu-chars-read-pattern-arg: Added optional arg PREFIX.
;;     apu-print-chars: Increased default width of name field to 40.
;;     Bind * and - to apu-chars-narrow and apu-chars-narrow-not.
;;     apu-tablist-match-entries: Restored 6/09 definition, so keep old names too.
;;     apu-make-tablist-entry: Let CHAR be a cons (CHAR-NAME . CHAR-CODE) too, to handle old names.
;; 2015/06/10 dadams
;;     Added: apu-char-name.
;;     apu-tablist-match-entries:
;        Fixed regression from yesterday: use only cdrs - apu-make-tablist-entry takes only a CHAR.
;; 2015/06/09 dadams
;;     Added: apu-chars-in-region (aka describe-chars-in-region), apu-print-chars, apu--unnamed-chars.
;;     Renamed: apu-print to apu-print-apropos-matches,
;;              apu-tablist-entries to apu-tablist-match-entries.
;;     Made apu-chars an alias of apropos-char, instead of the reverse.
;;     apu-make-tablist-entry: Change parameter from CHAR+CODE to just the code (called CHAR).
;;     apu-chars-next-match-method, apu-chars-matching-*, apu-chars-refresh-*, apu-chars-narrow:
;;       Raise error if not in an apropos-char buffer.
;;     apu-char-string-here: Handle eob case.
;;     Require cl-lib.el, for cl-delete-duplicates.
;; 2015/06/07 dadams
;;     Added apropos-char (another alias for apu-chars).
;; 2015/05/26 dadams
;;     Added (forgot): defvars for apu--pats+bufs, apu--refresh-p.
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; case
(require 'cl-lib) ;; cl-delete-duplicates
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
  :link '(url-link :tag "Download" "https://www.emacswiki.org/emacs/download/apu.el")
  :link '(url-link :tag "Description" "https://www.emacswiki.org/emacs/AproposUnicode")
  :link '(emacs-commentary-link :tag "Commentary" "apu"))

;; Emacs bug #30539: If `apu-match-only-displayable-chars-flag' is non-nil and you have many fonts
;; installed then `apropos-char' (aka `apu-chars') queries can be very long.  Until that bug is fixed,
;; the default value will be nil for Emacs 25+.
;;
(defcustom apu-match-only-displayable-chars-flag (< emacs-major-version 25)
  "Non-nil means filter out chars not displayable (`char-displayable-p').
NOTE:

Starting with Emacs 25, `char-displayable-p' can be extremely slow if
the new variable `inhibit-compacting-font-caches' is nil, which it is
by default, and if you have many, or large, fonts installed.  For this
reason the default value of `apu-match-only-displayable-chars-flag' is
nil for Emacs 25 and later.  This seems to be the case if you have MS
Windows TrueType fonts installed, for instance.

If you want to be able to exclude non-displayable characters in Emacs
25+, then set `apu-match-only-displayable-chars-flag' to non-nil.  If
you find that this makes APU commands such as `apropos-char' extremely
slow then set variable `inhibit-compacting-font-caches' to `t'."
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
  "Latest set of patterns used for matching by `apropos-char'.
It is a list of two pattern lists: (INCLUDES EXCLUDES).
INCLUDES are patterns that must be matched.
EXCLUDES are patterns that must not be matched.")

(defvar apu--match-type 'MATCH-WORDS-AS-SUBSTRINGS
  "How the current `apu-char*' command matches a word-list pattern.
This has no effect when the current input pattern is a regexp.

Possible values and their behaviors:
* `MATCH-WORDS-AS-SUBSTRINGS': like nil `apu-match-words-exactly-flag'
* `MATCH-WORDS-EXACTLY':   like non-nil `apu-match-words-exactly-flag'
* `MATCH-TWO-OR-MORE': like non-nil `apu-match-two-or-more-words-flag'

Any other value acts like `MATCH-WORDS-AS-SUBSTRINGS'")

(defvar apu--orig-buffer nil
  "Buffer current when `apropos-char' was last invoked.")

;; A buffer where `apu-chars*' is invoked can have multiple list buffers, which show matches for
;; different sets of patterns.  `apu--pattern' is local to the list buffer that shows the matches.
(defvar apu--pats+bufs ()
  "Alist of patterns and their list buffers.
Each entry has form (PATTERN PATTERN-NOT . BUFFER), where
the buffer-local values of `apu--patterns' and `apu--patterns-not' in
BUFFER are PATTERN and PATTERN-NOT, respectively.")

(defvar apu--refresh-p nil
  "Non-nil means that `apu-tablist-match-entries' recomputes matches.")


;;; Buffer-local variables -------------------------------------------

(defvar apu--buffer-invoked-from nil
  "Buffer current when `apropos-char' was invoked to produce this output.
Used in a list buffer to point to where it was invoked.")
(make-variable-buffer-local 'apu--buffer-invoked-from)
(put 'apu--buffer-invoked-from 'permanent-local t)

(defvar apu--matches ()
  "Result of matching character names in `apu--buffer-invoked-from'.
A cons whose car is the maximum width of the matching character names
and whose cdr is an alist of (CHAR-NAME . CHAR-CODE) pairs.")
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

(defvar apu--patterns-not ()
  "Patterns currently used by `apropos-unicode' to exclude Unicode chars.")
(make-variable-buffer-local 'apu--patterns-not)
(put 'apu--patterns-not 'permanent-local t)

(defvar apu--unnamed-chars ()
  "Chars not recognized as Unicode.
They are for the last apu command associated with this output buffer.")
(make-variable-buffer-local 'apu--unnamed-chars)
(put 'apu--unnamed-chars 'permanent-local t)


;;; Functions ---------------------------------------------------------

;;;###autoload
(defun apu-chars-next-match-method ()   ; Not bound by default.
  "Cycle among the methods of matching character names."
  (interactive)
  (unless apu--buffer-invoked-from (error "Not an `apropos-char' buffer"))
  (call-interactively
   (case apu--match-type
     (MATCH-WORDS-AS-SUBSTRINGS #'apu-chars-matching-two-or-more-words)
     (MATCH-TWO-OR-MORE         #'apu-chars-matching-full-words)
     (t                         #'apu-chars-matching-words-as-substrings))))

;;;###autoload
(defun apu-chars-matching-full-words () ; Not bound by default.
  "Show all Unicode chars whose names match a pattern you type.
Same as `apropos-char' with a non-nil value of
`apu-match-words-exactly-flag': match each entry of a word-list
pattern as a full word."
  (interactive)
  (unless apu--buffer-invoked-from (error "Not an `apropos-char' buffer"))
  (setq apu-latest-pattern-set  (list (list (apu-chars-read-pattern-arg)) nil))
  (let ((list-buf  (get-buffer-create (apu-buf-name-for-matching (car apu-latest-pattern-set)
                                                                 (cdr apu-latest-pattern-set)))))
    (apu-add-to-pats+bufs (cons apu-latest-pattern-set list-buf))
    (with-current-buffer list-buf (setq apu--patterns  (car apu-latest-pattern-set))))
  (setq apu--match-type  'MATCH-WORDS-EXACTLY)
  (apu-chars))

;;;###autoload
(defun apu-chars-matching-two-or-more-words () ; Not bound by default.
  "Show all Unicode chars whose names match a pattern you type.
Same as `apropos-char' with a nil value of
`apu-match-two-or-more-words-flag': match two or more entries of a
word-list pattern, as a full word."
  (interactive)
  (unless apu--buffer-invoked-from (error "Not an `apropos-char' buffer"))
  (setq apu-latest-pattern-set  (list (list (apu-chars-read-pattern-arg)) nil))
  (let ((list-buf  (get-buffer-create (apu-buf-name-for-matching (car apu-latest-pattern-set)
                                                                 (cdr apu-latest-pattern-set)))))
    (apu-add-to-pats+bufs (cons apu-latest-pattern-set list-buf))
    (with-current-buffer list-buf (setq apu--patterns (car apu-latest-pattern-set))))
  (setq apu--match-type  'MATCH-TWO-OR-MORE)
  (apu-chars))

;;;###autoload
(defun apu-chars-matching-words-as-substrings () ; Not bound by default.
  "Show all Unicode chars whose names match a pattern you type.
Same as `apropos-char' with a nil value of
`apu-match-words-exactly-flag': match each entry of a word-list
pattern as a substring."
  (interactive)
  (unless apu--buffer-invoked-from (error "Not an `apropos-char' buffer"))
  (setq apu-latest-pattern-set  (list (list (apu-chars-read-pattern-arg)) nil))
  (let ((list-buf  (get-buffer-create (apu-buf-name-for-matching (car apu-latest-pattern-set)
                                                                 (cdr apu-latest-pattern-set)))))
    (apu-add-to-pats+bufs (cons apu-latest-pattern-set list-buf))
    (with-current-buffer list-buf (setq apu--patterns (car apu-latest-pattern-set))))
  (setq apu--match-type  'MATCH-WORDS-AS-SUBSTRINGS)
  (apu-chars))

(defun apu-chars-refresh-with-next-match-method () ; Bound to `C-c n'.
  "Refresh matches for the same pattern, but using the next matching method."
  (interactive)
  (unless apu--buffer-invoked-from (error "Not an `apropos-char' buffer"))
  (call-interactively
   (case apu--match-type
     (MATCH-WORDS-AS-SUBSTRINGS #'apu-chars-refresh-matching-two-or-more-words)
     (MATCH-TWO-OR-MORE         #'apu-chars-refresh-matching-full-words)
     (t                         #'apu-chars-refresh-matching-as-substrings))))

(defun apu-chars-refresh-matching-full-words () ; Bound to `C-c w'.
  "Refresh matches for the same pattern, but match full words.
I.e., match again, as if `apu-match-words-exactly-flag' were non-nil.
Does nothing if current pattern is a regexp instead of a word list."
  (interactive)
  (unless apu--buffer-invoked-from (error "Not an `apropos-char' buffer"))
  (setq apu--match-type  'MATCH-WORDS-EXACTLY)
  (with-current-buffer apu--buffer-invoked-from (apu-print-apropos-matches))
  (apu-match-type-msg))

(defun apu-chars-refresh-matching-as-substrings () ; Bound to `C-c s'.
  "Refresh matches for the same pattern, but match as substrings.
I.e., match again, as if `apu-match-words-exactly-flag' were nil.
Does nothing if current pattern is a regexp instead of a word list."
  (interactive)
  (unless apu--buffer-invoked-from (error "Not an `apropos-char' buffer"))
  (setq apu--match-type  'MATCH-WORDS-AS-SUBSTRINGS)
  (with-current-buffer apu--buffer-invoked-from (apu-print-apropos-matches))
  (apu-match-type-msg))

(defun apu-chars-refresh-matching-two-or-more-words () ; Bound to `C-c 2'.
  "Refresh matches for the same pattern, but match two or more words.
I.e., match again, as if `apu-match-two-or-more-words-flag' were t.
Does nothing if current pattern is a regexp instead of a word list."
  (interactive)
  (unless apu--buffer-invoked-from (error "Not an `apropos-char' buffer"))
  (setq apu--match-type  'MATCH-TWO-OR-MORE)
  (with-current-buffer apu--buffer-invoked-from (apu-print-apropos-matches))
  (apu-match-type-msg))

(defun apu-revert-buffer ()
  "Revert current APU buffer.
This will cause any partly elided (`...') char names to be shown
completely"
  (interactive)
  (unless (derived-mode-p 'apu-mode) (error "The current buffer is not in APU mode"))
  (run-hooks 'apu-revert-hook)
  (with-current-buffer apu--buffer-invoked-from (apu-print-apropos-matches)))

(define-derived-mode apu-mode tabulated-list-mode "Apropos Unicode"
  "Major mode for `apropos-char' output.
If some char names are partly elided (`...') you can use \\<apu-mode-map>`\\[apu-revert-buffer]' to
refresh the display to show the full names.  Using any of the
match-refreshing commands (`\\[apu-chars-refresh-matching-as-substrings]', \
`\\[apu-chars-refresh-with-next-match-method]', `\\[apu-chars-refresh-matching-full-words]', and \
`\\[apu-chars-refresh-matching-two-or-more-words]')
also shows the full names.

See command `apropos-char' for more information.

\\{apu-mode-map}"
  (update-glyphless-char-display 'glyphless-char-display-control glyphless-char-display-control))

(when (featurep 'ucs-cmds)
  (define-key apu-mode-map "c"         'apu-define-insert-command))
(define-key apu-mode-map   "g"         'apu-revert-buffer)
(define-key apu-mode-map   "i"         'apu-google-char)
(define-key apu-mode-map   "k"         'apu-global-set-insertion-key)
(define-key apu-mode-map   "l"         'apu-local-set-insertion-key)
(define-key apu-mode-map   "z"         'apu-zoom-char-here)
(define-key apu-mode-map   "^"         'apu-insert-char)
(define-key apu-mode-map   "*"         'apu-chars-narrow)
(define-key apu-mode-map   "-"         'apu-chars-narrow-not)
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
(defun apu-char-name-at-point (&optional position msgp) ; Not bound by default.
  "Return the name of the Unicode character at point, or nil if none.
Non-nil POSITION means use the character at POSITION."
  (interactive "d\np")
  (apu-char-at-point 'name position msgp))

;;;###autoload
(defun apu-char-codepoint-at-point (&optional position msgp) ; Not bound by default.
  "Return the codepoint of the Unicode char at point, or nil if none.
Non-nil POSITION means use the character at POSITION."
  (interactive "d\np")
  (apu-char-at-point 'code position msgp))

(defun apu-char-at-point (return-type &optional position msgp)
  "Return the name or codepoint of the Unicode char at POSITION.
Return the name If RETURN-TYPE is `name'; otherwise, the codepoint.
POSITION defaults to point.
Non-nil MSGP means echo the name and code point."
  (let ((char  (char-after position))
        name)
    (prog1 (if (eq return-type 'name)
               (or (setq name  (apu-char-name char))  (error "No Unicode char here"))
             char)
      (when msgp
        (message "Char: `%s', Codepoint: `%d' (`%#x')"
                 (or name  (apu-char-name char)  (error "No Unicode char here"))
                 char
                 char)))))

(defun apu-char-here ()
  "Return the Unicode character described on this line."
  (string-to-char (apu-char-string-here)))

(defun apu-char-string-here ()
  "Return the Unicode character described on this line, as a string.
If at end of buffer and beginning of line, return the character
described on the previous line."
  (when (= (point-min) (point-max)) (error "No characters in this buffer"))
  (when (eobp) (backward-char))
  (buffer-substring (line-beginning-position) (1+ (line-beginning-position))))

;;;###autoload
(defun apu-copy-char-at-point-as-kill (&optional msgp) ; Not bound by default.
  "Copy the character at point to the `kill-ring'."
  (interactive "p")
  (let ((strg  (string (char-after))))
    (kill-new strg)
    (when msgp (message "Copied char `%s' to kill ring" strg))))

(defun apu-copy-char-here-as-kill (&optional msgp) ; Bound to `C-y'.
  "Copy the Unicode character described on this line to the `kill-ring'."
  (interactive "p")
  (let ((strg  (string (apu-char-here))))
    (kill-new strg)
    (when msgp (message "Copied char `%s' to kill ring" strg))))

;;;###autoload
(defun apu-copy-char-at-point-to-second-sel (&optional msgp) ; Not bound by default.
  "Copy the character at point to the secondary selection.
If you have library `second-sel.el' then also copy it to the
`secondary-selection-ring'."
  (interactive "p")
  (apu-copy-char-to-second-sel (point) msgp))

(defun apu-copy-char-here-to-second-sel (&optional msgp) ; Bound to `M-y'.
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
    (if (fboundp 'gui-set-selection)
        (gui-set-selection 'SECONDARY strg) ; Emacs 25.1+.
      (x-set-selection 'SECONDARY strg))
    (if mouse-secondary-overlay
        (move-overlay mouse-secondary-overlay position (1+ position) (current-buffer))
      (setq mouse-secondary-overlay  (make-overlay position (1+ position) (current-buffer)))
      (overlay-put mouse-secondary-overlay 'face 'secondary-selection))
    (when (require 'second-sel nil t) (add-secondary-to-ring strg))
    (when msgp (message "Copied char `%s' to secondary selection%s"
                        strg (if (require 'second-sel nil t) " ring" "")))))

(defun apu-define-insert-command () ; Bound to `c'.
  "Define a command that inserts the character described on this line.
The command name is the lowercase Unicode character name, with spaces
 replaced by hyphens.
This command requires library `ucs-cmds.el'."
  (interactive)
  (unless (require 'ucs-cmds nil t) (error "This command requires library `ucs-cmds.el'"))
  (ucsc-define-char-insert-cmd (apu-char-here) 'MSG))

(defun apu-global-set-insertion-key (key &optional msgp) ; Bound to `k'.
  "Globally bind a key to insert the character described on this line."
  (interactive "KKey to bind globally: \np")
  (let ((char  (apu-char-string-here)))
    (global-set-key key char)
    (when msgp (message "`%s' will now insert `%s' globally" (key-description key) char))))

(defun apu-google-char () ; Bound to `i'.
  "Google the Unicode character described on this line."
  (interactive "p")
  (browse-url (format "https://www.google.com/search?ion=1&q=%s"
                      (url-hexify-string (concat "UNICODE " (apu-char-name-here))))))

(defun apu-char-name-here ()
  "Return the name of the Unicode char described on this line, as a string."
  (apu-char-name (apu-char-here)))

(defun apu-insert-char (buffer &optional msgp) ; Bound to `^'.
  "Insert the Unicode character described on this line at point in BUFFER.
By default, BUFFER is the buffer that was current when
`apropos-char' was invoked.  With a prefix arg you are prompted for
the buffer to use instead."
  (interactive (list (if current-prefix-arg
                         (read-buffer "Insert in buffer: " apu--buffer-invoked-from 'REQUIRE-MATCH)
                       apu--buffer-invoked-from)
                     t))
  (let ((char  (apu-char-string-here)))
    (with-current-buffer buffer (insert char))
    (when msgp (message "Inserted `%s' in buffer `%s'" char (buffer-name buffer)))))

(defun apu-local-set-insertion-key (key &optional msgp) ; Bound to `l'.
  "Locally bind a key to insert the character described on this line."
  (interactive "KKey to bind locally: \np")
  (let ((char  (apu-char-string-here)))
    (local-set-key key char)
    (when msgp (message "`%s' will now insert `%s' locally" (key-description key) char))))

(defun apu-show-char-details (&optional event) ; Bound to `RET', `mouse-2'.
  "Show details about Unicode character described on this line."
  (interactive (list last-nonmenu-event))
  (run-hooks 'mouse-leave-buffer-hook)
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (save-excursion (goto-char (posn-point (event-start event)))
                    (describe-char (line-beginning-position)))))

(defun apu-zoom-char-here (&optional height) ; Bound to `z'.
  "Show the char described on the current line in a zoomed tooltip.
With a numerical prefix arg, show it that many times larger."
  (interactive (list (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))))
  (unless height (setq height  1))
  (x-show-tip (propertize (apu-char-string-here)
                          'face `(:foreground "red" :height ,(* 200 height)))))

;;;###autoload
(defun apu-zoom-char-at-point (&optional height position) ; Not bound by default.
  "Show the Unicode char at point in a zoomed tooltip.
With a numerical prefix arg, show it that many times larger.
Non-nil POSITION means use the character at POSITION."
  (interactive (list (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))))
  (unless height (setq height  1))
  (x-show-tip (propertize (char-to-string (char-after position))
                          'face `(:foreground "red" :height ,(* 200 height)))))

;;;###autoload
(defalias 'describe-chars-in-region 'apu-chars-in-region)
;;;###autoload
(defun apu-chars-in-region (start end &optional include-dups) ; Not bound by default.
  "Describe the Unicode characters in the region.
By default, list each distinct char only once.  With a prefix arg,
list a given char once for each of its occurrences in the region.
The character descriptions are presented in `apu-mode'."
  (interactive "r\nP")
  (setq apu--orig-buffer  (current-buffer)
        apu--refresh-p    t)
  (let ((chars  (buffer-substring start end)))
    (unless include-dups (setq chars  (cl-delete-duplicates chars :from-end t)))
    (apu-print-chars chars (generate-new-buffer-name "*Unicode Text in Region*"))
    (when apu--unnamed-chars
      (display-message-or-buffer
       (format "The following chars are not recognized as Unicode:\n%s"
               (mapconcat #'char-to-string (nreverse apu--unnamed-chars) "\n"))))))

(defun apu-print-chars (characters buffer-name)
  "Show information about Unicode CHARACTERS, in buffer BUFFER-NAME."
  (with-help-window buffer-name
    (with-current-buffer buffer-name
      (apu-mode)
      (goto-char (point-min))
      (setq case-fold-search          t
            tabulated-list-format     (vector '("Ch"       2 apu-sort-char)
                                              ;; Use 30 as a default name width.
                                              `("Name"     ,(or (car apu--matches)  30) t)
                                              '("Decimal"  7 apu-sort-char :right-align t)
                                              '("Hex"      8 apu-sort-char :right-align t))
            tabulated-list-sort-key   nil
            tabulated-list-entries    (delq nil (mapcar #'apu-make-tablist-entry characters)))
      (tabulated-list-print t)
      (tabulated-list-init-header))))

;;;###autoload
(defalias 'apropos-unicode 'apropos-char)
;;;###autoload
(defalias 'apu-chars 'apropos-char)
;;;###autoload
(defun apropos-char ()                  ; Not bound by default.
  "Show all Unicode chars whose names match a pattern.
The pattern is as for command `apropos': a word, a list of words
 \(separated by spaces), or a regexp (using some regexp special
 characters).  If it is a word, search for matches for that word as a
 substring.  If it is a list of words, search for matches for any two
 \(or more) of those words.

The character descriptions are presented in `apu-mode'.

You can provide additional patterns, to narrow the set of displayed
characters to those that also match the additional patterns or to
those that do not match them.  `*' and `-' do this, respectively.

Non-nil option `apu-match-only-displayable-chars-flag' means match
only characters that are `char-displayable-p' in the buffer where
`apropos-char' is invoked.  This is the default.  A nil value means
match all Unicode characters.

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
  include capital letters that serve as math symbols and such.

\\{apu-mode-map}"
  (interactive (prog1 ()
                 (setq apu-latest-pattern-set  (list (list (apu-chars-read-pattern-arg)) nil))
                 (let ((list-buf  (get-buffer-create
                                   (apu-buf-name-for-matching (car apu-latest-pattern-set)
                                                              (cdr apu-latest-pattern-set)))))
                   (apu-add-to-pats+bufs (cons apu-latest-pattern-set list-buf))
                   (with-current-buffer list-buf (setq apu--patterns (car apu-latest-pattern-set))))))
  (setq apu--orig-buffer  (current-buffer)
        apu--refresh-p    t)
  (when (called-interactively-p 'interactive)
    (setq apu--match-type  (if apu-match-two-or-more-words-flag
                               'MATCH-TWO-OR-MORE
                             (if apu-match-words-exactly-flag
                                 'MATCH-WORDS-EXACTLY
                               'MATCH-WORDS-AS-SUBSTRINGS))))
  (apu-print-apropos-matches)
  (apu-match-type-msg))

(defun apu-add-to-pats+bufs (entry)
  "Add ENTRY to `apu--pats+bufs'.  Remove entries for killed buffers."
  (add-to-list 'apu--pats+bufs entry)
  (setq apu--pats+bufs  (apu-delete-if-not (lambda (p.b) (buffer-live-p (cdr p.b))) apu--pats+bufs)))

(defun apu-buf-name-for-matching (include exclude)
  "Return a buffer name reflecting patterns INCLUDE and EXCLUDE."
  (format "*`%S' but NOT `%S'*" include exclude))

(defun apu-chars-read-pattern-arg (&optional prefix)
  "Read a pattern arg for `apu-chars*' commands."
  (let ((pat  (read-string (format "%sUnicode name pattern (word list or regexp): " (or prefix "")))))
    (if (string-equal (regexp-quote pat) pat)
        (or (split-string pat "[ \t]+" t)  (user-error "No word list given"))
      pat)))

(defun apu-match-type-msg ()
  "Message stating current match type."
  (message (case apu--match-type
             (MATCH-TWO-OR-MORE    "Matching at least TWO entries as full words now")
             (MATCH-WORDS-EXACTLY  "Matching EACH entry as a FULL WORD now")
             (t                    "Matching EACH entry as a SUBSTRING now"))))

(defun apu-print-apropos-matches ()
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
              tabulated-list-entries    #'apu-tablist-match-entries)
        (tabulated-list-print t)
        (tabulated-list-init-header)))))

(defun apu-tablist-match-entries ()     ; Invoked in the list-output buffer.
  "Function value for `tabulated-list-entries'.
See `apu-make-tablist-entry'."
  (when apu--refresh-p (apu-compute-matches))
  (mapcar #'apu-make-tablist-entry (cdr apu--matches)))

(defun apu-make-tablist-entry (char)
  "Return a tablist entry for CHAR.
CHAR is either a Unicode code point or a cons (CHAR-NAME . CHAR-CODE).
CHAR-NAME is the `name' or `old-name' property of the character, and
CHAR-CODE is its code point.  (For `ucs-names', property `old-name' is
not used.)

This  is a list (CHAR [GLYPH NAME DEC HEX]), where:
CODE  is the character (an integer),
GLYPH is its string representation,
NAME  is its name or old name,
DEC   is its decimal representation,
HEX   is its hexadecimal representation.

GLYPH, NAME, DEC, and HEX are strings.

If CHAR is not recognized then it is added to the buffer-local list
`apu--unnamed-chars'.  This list of chars is then displayed."
  (let ((name  (if (consp char) (car char) (apu-char-name char)))
        (code  (if (consp char) (cdr char) char)))
    (if name
        (list code (vector (char-to-string code) name (format "%6d" code) (format "%#8x" code)))
      (push code apu--unnamed-chars)
      nil)))

(defun apu-hash-table-to-alist (hash-table)
  "Create and return an alist created from HASH-TABLE.
The order of alist entries is undefined, but it seems to be the same
as the order of hash-table entries (which seems to be the order in
which the entries were added to the table)."
  (let ((al  ()))
    (maphash (lambda (key val) (push (cons key val) al)) hash-table)
    (nreverse al)))

(defun apu-get-hash-keys (value hash-table &optional value-test-function)
  "Return a list of keys associated with VALUE in HASH-TABLE.
Optional arg VALUE-TEST-FUNCTION (default `equal') is the equality
predicate used to compare values."
  (setq value-test-function  (or value-test-function  #'equal))
  (let ((keys  ()))
    (maphash (lambda (key val)
               (when (funcall value-test-function val value)
                 (push key keys)))
             hash-table)
    keys))

(defun apu-get-a-hash-key (value hash-table &optional value-test-function)
  "Return a hash key associated with VALUE in HASH-TABLE.
If there is more than one such key then it is undefined which is
returned.
Optional arg VALUE-TEST-FUNCTION (default `equal') is the equality
predicate used to compare values."
  (setq value-test-function  (or value-test-function  #'equal))
  (catch 'get-a-hash-key
    (maphash (lambda (key val)
               (when (funcall value-test-function val value)
                 (throw 'get-a-hash-key key)))
             hash-table)
    nil))

(defun apu-char-names (character)
  "Return a list of the names for CHARACTER."
  (if (hash-table-p (ucs-names))
      (apu-get-hash-keys character (ucs-names))
    (mapcar #'car (ucs-names))))

(defun apu-char-name (character &optional prefer-old-name-p)
  "Return the name of CHARACTER, or nil if it has no name.
This is Unicode property `name' if there is one, or property
 `old-name' if not, or nil if neither.
Non-nil optional arg PREFER-OLD-NAME-P means reverse the priority,
 returning the old name if there is one."
  (if prefer-old-name-p
      (or (get-char-code-property character 'old-name)  (get-char-code-property character 'name))
    (or (get-char-code-property character 'name)  (get-char-code-property character 'old-name))))

(defun apu-compute-matches ()     ; Invoked in the list-output buffer.
  "Compute matches for apropos Unicode commands."
  (let ((patterns      apu--patterns)
        (patterns-not  apu--patterns-not))
    (message "Matching `%s' and not `%s'..." patterns patterns-not)
    (ucs-names)
    (let* ((case-fold-search  t)
           (max-char          0)
           (names+codes       ()))
      (dolist (pat  patterns) (setq names+codes  (apu-filter pat names+codes)))
      (unless names+codes (error "No characters match patterns specified"))
      (dolist (pat  patterns-not) (setq names+codes  (apu-filter pat names+codes 'NOT)))
      (unless names+codes (error "No characters match patterns specified"))
      (when apu-match-only-displayable-chars-flag
        (setq names+codes  (apu-delete-if-not #'apu-char-displayable-p names+codes)))
      (unless names+codes (error "No characters match patterns specified"))
      (dolist (char+code  names+codes) (setq max-char  (max max-char (string-width (car char+code)))))
      (message "Matching `%s'...done" patterns)
      (setq apu--matches  (cons max-char names+codes)))))

(defun apu-filter (pattern names+codes &optional notp)
  "Try to match PATTERN against each element of alist NAMES+CODES.
Return filtered list.
PATTERN is as for `apropos-char'.
NAMES+CODES is an alist of conses (CHAR-NAME . CHAR-CODE).
If NAMES+CODES is nil then match against an alist derived from
`ucs-names'.

Non-nil optional arg NOTP means exclude, instead of include, matches
for PATTERN."
  (let ((match-fn     (if (eq apu--match-type 'MATCH-WORDS-EXACTLY)
                          #'apu-full-word-match
                        #'apu-substring-match))
        (rem-filt-fn  (if notp #'apu-remove-if #'apu-remove-if-not))
        (del-filt-fn  (if notp #'apu-delete-if #'apu-delete-if-not)))
    (setq names+codes  (or names+codes
                           (if (hash-table-p (ucs-names))
                               (apu-hash-table-to-alist ucs-names)
                             ucs-names)))
    (cond ((or (atom pattern)  (eq apu--match-type 'MATCH-TWO-OR-MORE))
           (require 'apropos) ; `apropos-parse-pattern', `apropos-regexp'.
           (let ((apropos-synonyms  apu-synonyms)) (apropos-parse-pattern pattern))
           (funcall rem-filt-fn (apply-partially #'apu-substring-match apropos-regexp) names+codes))
          (t ; List of words, to be matched as full words or substrings.
           (let ((chs+cds  ())
                 (first    (car pattern)))
             (dolist (c.c  names+codes)
               (if notp
                   (unless (funcall match-fn first c.c) (push c.c chs+cds))
                 (when (funcall match-fn first c.c) (push c.c chs+cds))))
             (dolist (word  (cdr pattern))
               (setq chs+cds  (funcall del-filt-fn (apply-partially match-fn word) chs+cds)))
             chs+cds)))))

(defun apu-chars-narrow (pattern)       ; Bound to `*'.
  "Narrow matches in current buffer to those also matching another PATTERN.
You are prompted for the PATTERN, which is as for `apropos-char'."
  (interactive (list (apu-chars-read-pattern-arg "Include only ")))
  (apu-chars-narrow-1 pattern))

(defun apu-chars-narrow-not (pattern)   ; Bound to `-'.
  "Narrow matches in current buffer to those not matching another PATTERN.
You are prompted for the PATTERN, which is as for `apropos-char'."
  (interactive (list (apu-chars-read-pattern-arg "Exclude ")))
  (apu-chars-narrow-1 pattern 'NOT))

(defun apu-chars-narrow-1 (pattern &optional notp)
  "Helper for `apu-chars-narrow' and `apu-chars-narrow-not'."
  (unless apu--buffer-invoked-from (error "Not an `apropos-char' buffer"))
  (let ((orig-pats   (if notp 'apu--patterns-not 'apu--patterns))
        (bufs-entry  (rassoc (current-buffer)
                             (with-current-buffer apu--buffer-invoked-from apu--pats+bufs))))
    (add-to-list orig-pats pattern 'APPEND)
    (when bufs-entry (if notp
                         (setcdr (car bufs-entry) (list apu--patterns-not))
                       (setcar (car bufs-entry) apu--patterns)))) ; Update entry in `apu--pats+bufs'.
  (let ((newbufname  (apu-buf-name-for-matching apu--patterns apu--patterns-not)))
    (when (get-buffer newbufname)
      (let ((kill-buffer-query-functions  ())) (kill-buffer newbufname)))
    (rename-buffer newbufname))
  (let ((case-fold-search  t)
        (max-char          0)
        ;; (orig-names+codes  (cdr apu--matches))
        ;; (match-fn          (if (eq apu--match-type 'MATCH-WORDS-EXACTLY)
        ;;                        #'apu-full-word-match
        ;;                      #'apu-substring-match))
        )
    (setcdr apu--matches (apu-filter pattern (cdr apu--matches) notp))
    (unless (cdr apu--matches) (error "No characters match patterns specified"))
    (dolist (char+code  (cdr apu--matches))
      (setq max-char  (max max-char (string-width (car char+code)))))
    (message "Matching `%s'...done" pattern)
    (setcar apu--matches max-char))
  (setq apu-latest-pattern-set  (list apu--patterns apu--patterns-not)) ; For `apu-print-apropos-matches'.
  (let ((apu--refresh-p  nil))
    (with-current-buffer apu--buffer-invoked-from (apu-print-apropos-matches)))
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
  "Return t if the char code of ENTRY1 is greater than that of ENTRY2.
Each arg has the form of the elements of `tabulated-list-entries'.
The car of each arg is the character codepoint, which is compared."
  (> (car entry1) (car entry2)))

;; Same as `icicle-remove-if' etc.
(defun apu-remove-if (pred xs)
  "A copy of list XS with only elements that do not satisfy predicate PRED."
  (let ((result  ())) (dolist (x xs) (unless (funcall pred x) (push x result))) (nreverse result)))

(defun apu-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ())) (dolist (x xs) (when (funcall pred x) (push x result))) (nreverse result)))

(defun apu-delete-if (predicate xs)
  "Remove all elements of list XS that satisfy PREDICATE.
This operation is destructive, reusing conses of XS whenever possible."
  (while (and xs  (funcall predicate (car xs)))
    (setq xs  (cdr xs)))
  (let ((cl-p  xs))
    (while (cdr cl-p)
      (if (funcall predicate (cadr cl-p)) (setcdr cl-p (cddr cl-p)) (setq cl-p  (cdr cl-p)))))
  xs)

(defun apu-delete-if-not (predicate xs)
  "Remove all elements of list XS that do not satisfy PREDICATE.
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
