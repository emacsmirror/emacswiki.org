;;; synonyms.el --- Look up synonyms for a word or phrase in a thesaurus.
;;
;; Filename: synonyms.el
;; Description: Look up synonyms for a word or phrase in a thesaurus.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005-2012, Drew Adams, all rights reserved.
;; Created: Tue Dec 20 14:39:26 2005
;; Version: 1.0
;; Last-Updated: Thu Aug 23 17:04:10 2012 (-0700)
;;           By: dradams
;;     Update #: 2505
;; URL: http://www.emacswiki.org/emacs-en/synonyms.el
;; Doc URL: http://www.emacswiki.org/emacs/ThesauriAndSynonyms
;; Keywords: text, dictionary, thesaurus, spelling, apropos, help
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Look up synonyms for a word or phrase in a thesaurus.
;;
;;
;;  Getting Started
;;  ---------------
;;
;;  To use library Synonyms, you will need the Moby Thesaurus II file,
;;  `mthesaur.txt', available here:
;;
;;    ftp://ibiblio.org/pub/docs/books/gutenberg/etext02/mthes10.zip
;;
;;  Put this in your initialization file (~/.emacs):
;;
;;    ;; The file names are absolute, not relative, locations
;;    ;;     - e.g. /foobar/mthesaur.txt.cache, not mthesaur.txt.cache
;;    (setq synonyms-file        <name & location of mthesaur.txt>)
;;    (setq synonyms-cache-file  <name & location of your cache file>)
;;    (require 'synonyms)
;;
;;  As an alternative to the first two lines, you can use Customize to
;;  set `synonyms-file' and `synonyms-cache-file' persistently.  The
;;  second of these files is created by this library, to serve as a
;;  synonym cache for completion.
;;
;;  The main command is `synonyms'.  It prompts you for a word or
;;  phrase to look up in the thesaurus.  The synonyms found are then
;;  displayed in buffer *Synonyms*.  For example, `M-x synonyms RET
;;  democracy' displays synonyms for `democracy'.
;;
;;  If you do not define `synonyms-file' and `synonyms-cache-file'
;;  prior to using command `synonyms', that command will prompt you to
;;  define them.  If you want to use the same values during subsequent
;;  Emacs sessions, then you should use `M-x customize-option' to save
;;  those newly defined values.
;;
;;
;;  Some Definitions
;;  ----------------
;;
;;  The thesaurus is divided into "entries", which are like glossary
;;  entries: each entry is followed by associated words and phrases,
;;  which, for lack of a better word, I refer to as "synonyms".  For
;;  example, `democracy' is an entry, and it is followed by its
;;  synonyms.  Some synonyms are not also entries.  For example,
;;  `patriarchy' is in the thesaurus as a synonym but not as an entry.
;;
;;  Note: What I call "synonyms" here are not necessarily synonyms, in
;;  the sense of having the same or even similar meanings.  They are
;;  simply terms collected together with the same thesaurus entry
;;  because they are related in some way - the grouping is what
;;  defines their relation.
;;
;;  In Moby Thesaurus II, the meanings of synonyms in the same group
;;  do have something in common, but this might be simply the fact
;;  that they are terms of a similar kind.  For example, the
;;  "synonyms" following the `democracy' thesaurus entry are words
;;  such as `dictatorship' and `autocracy'.  These are different forms
;;  of the same general thing: government - they are certainly not
;;  synonymous with each other or with the entry `democracy'.
;;
;;
;;  Searching the Thesaurus
;;  -----------------------
;;
;;  The default input value for command `synonyms' is the word under
;;  the cursor. Alternatively, if a region is active and you are in
;;  Transient Mark mode (recommended), then it is the text in the
;;  region (selection).
;;
;;  Your input is actually treated as a regular expression (regexp),
;;  so you can also input patterns like `for.*ion', which will match
;;  thesaurus entries `formation', `formulation', `fornication',
;;  `fortification', and `forward motion'.  Note that the last of
;;  these is a phrase rather than a single word.
;;
;;  Using a regexp as input is a powerful way to search, but be aware
;;  that it can be costly in CPU time and computer memory if the
;;  regexp is not appropriate.  The regexp `.*' will, for example,
;;  likely use up available memory before being able to return the
;;  entire thesaurus (it's very large).  You can always use `C-g' to
;;  interrupt a thesaurus search if you mistakenly use an inefficient
;;  regexp.
;;
;;
;;  Using a Prefix Argument To Do More
;;  ----------------------------------
;;
;;  You can use a prefix argument to modify searching and the
;;  presentation of search results, as follows:
;;
;;    `C-u'     - Search for additional synonyms, in two senses:
;;
;;                1) Return also synonyms that are matched partially
;;                   by the input.
;;
;;                2) Search the entire thesaurus for input matches,
;;                   even if the input matches a thesaurus entry.
;;
;;    `M--'     - Append the search results to any previous search
;;                results, in buffer *Synonyms*.  (Normally, the new
;;                results replace any previous results.)
;;
;;    `C-u C-u' - `C-u' plus `M--': Search more and append results.
;;
;;  If you find yourself often using a particular prefix argument (for
;;  example, to append results), then you might want to instead change
;;  the default behavior to reflect this preference.  Options
;;  `synonyms-match-more-flag' and `synonyms-append-result-flag'
;;  correspond to using `C-u' and `M--', respectively.  In fact, a
;;  prefix argument simply toggles the value of the corresponding
;;  option for the duration of the command.  So, for example, if
;;  `synonyms-append-result-flag' is t and you use `M--', then results
;;  will not be appended.
;;
;;  When partially matching input (`C-u', sense #1), complete synonyms
;;  are matched against your input.  This means that you generally
;;  need not add a preceding or trailing `.*' to try to match a
;;  complete synonym.  For example, input `format' will match the
;;  complete synonyms `conformation', `efformation', `format',
;;  `formation', `formative', `formational', `information',
;;  `informative', `informational', `malformation', `deformation',
;;  `reformation', `transformation', `reformatory', and so on - there
;;  is no need to input `.*format.*' to match the same synonyms.
;;
;;  To better understand the meaning of #2 above for `C-u' (to
;;  continue the search even if your input matches an entry), try, for
;;  example, `C-u M-x synonyms RET widespread'.  You'll see not only
;;  the main synonyms listed for `widespread' as an entry, but also
;;  lots of different meanings of `widespread', judging by the entries
;;  for which it is listed as a synonym:
;;
;;    `accepted', `ample', `broad', `broadcast', `capacious',
;;    `catholic', `commodious', `commonness', `conventional',
;;    `currency', `current', `customary', `deep', `deltoid',
;;    `diffuse', `discrete', `dispersed', `disseminated',
;;    `dissipated', `distributed', `epidemic', `established',
;;    `everyday', `expansive', `extended', `extensive', `familiar',
;;    `fan shaped', `far flung', `far reaching', `flaring', `full',
;;    `general', `indiscriminate', `infinite', `large scale',
;;    `liberal', `normal', `normality', `open', `ordinary',
;;    `outstretched', `pervasive', `popular', `prescribed',
;;    `prescriptive', `prevailing', `prevalence', `prevalent',
;;    `public', `rampant', `received', `regnant', `regular',
;;    `regulation', `reign', `rife', `roomy', `ruling', `run',
;;    `scattered', `set', spacious`', `sparse', `splay', `sporadic',
;;    `sprawling', `spread', `standard', `stock', `straggling',
;;    `stretched out', `sweeping', `time-honored', `traditional',
;;    `universal', `usual', `vast', `voluminous', `wholesale', `wide
;;    open', `wide', and `wonted'.
;;
;;  These are just the entries! Each of these is of course followed by
;;  its own synonyms - perhaps 100 or 300, including `widespread'.
;;
;;  This list of entries is not the same list as the synonyms for
;;  entry `widespread'.  There are words and phrases here that are not
;;  in the latter list, and vice versa.  For example, the former (but
;;  not the latter) list includes `full'; the latter (but not the
;;  former) list includes `wide-reaching'.
;;
;;  The latter are the words most closely related to `widespread'.
;;  The list above are the other thesaurus entries (corresponding to
;;  main categories) to which `widespread' is most closely related.
;;  Looking at all of the synonym groups in which `widespread' appears
;;  can tell you additional information about its meanings - and it
;;  can provide additional synonyms for `widespread'.
;;
;;
;;  Using Completion with Synonyms
;;  ------------------------------
;;
;;  You can complete words and phrases in the minibuffer, as input to
;;  command `synonyms'.  You can use library Synonyms together with
;;  library Icicles to complete a partial word in a text buffer into a
;;  word or phrase in the thesaurus.  If you use both libraries then
;;  load Icicles after Synonyms.  For more information on Icicles, see
;;  `http://www.emacswiki.org/cgi-bin/wiki/icicles.el'.
;;
;;  ** Minibuffer Input Completion **
;;
;;  You can enter any text to match against thesaurus synonyms.  When
;;  you are prompted by command `synonyms' to enter this text, you can
;;  also use input completion to complete to a thesaurus synonym.
;;  That is, even though you can enter any text (including a regexp),
;;  completion will only complete to synonyms in the thesaurus.
;;
;;  If you load library Icicles, then a more powerful version of
;;  command `synonyms' is used.  In particular, it lets you:
;;
;;   - Use `S-TAB' during completion to see the list of all synonyms
;;     (thesaurus terms) that match your minibuffer input so far.
;;
;;   - Use `[next]', and `[prior]' (usually keys `Page Down' and `Page
;;     Up') during completion to cycle through the completion
;;     candidates (synonyms) that match your input.
;;
;;   - Use `C-o', `C-[next]', and `[C-prior]' during completion to
;;     display the synonyms of the current completion candidate.
;;
;;  ** Completing Buffer Text Using the Thesaurus **
;;
;;  Icicles also provides two commands for using completion to insert
;;  thesaurus entries in a buffer:
;;
;;   - `icicle-complete-thesaurus-entry' completes a word in a text
;;     buffer to any word or phrase in the thesaurus.  I bind it to
;;     `C-c /'.
;;
;;   - `icicle-insert-thesaurus-entry' inserts thesaurus words and
;;     phrases in a text buffer.  It is a multi-command, which means
;;     that, within a single call to it, you can insert any number of
;;     thesaurus entries, in succession.  If you want to, you can
;;     write an entire book using a single call to
;;     `icicle-insert-thesaurus-entry'!
;;
;;
;;  Browsing the Thesaurus
;;  ----------------------
;;
;;  Besides using command `synonyms' to search for synonyms, you can
;;  use Synonyms to browse the thesaurus.  This is really just the
;;  same thing, but key and mouse bindings are provided in buffer
;;  *Synonyms*, so you need not input anything - just point and click
;;  the hyperlinks.  Buffer *Synonyms* is in Synonyms major mode,
;;  which provides a few additional features.
;;
;;  You can still choose to search for additional synonyms or append
;;  search results, without bothering with a prefix argument, by using
;;  modifier keys (Control, Meta) with a mouse click.
;;
;;  Another way of browsing is to revisit previous search-result
;;  pages.  You can do this using commands `synonyms-history-backward'
;;  and `synonyms-history-forward'.  In buffer *Synonyms*, these are
;;  bound to the following key sequences, for convenience:
;;
;;    `l', `p', `mouse-4' - `synonyms-history-backward'
;;    `r', `n', `mouse-5' - `synonyms-history-forward'
;;
;;  The `l' and `r' bindings correspond to the history bindings in
;;  Info.  The `p' and `n' bindings stand for "previous" and "next".
;;  The bindings to additional mouse buttons correspond to typical
;;  bindings for Back and Forward in Web browsers.
;;
;;  In addition to these bindings, the same history commands can be
;;  accessed by clicking links [Back] and [Forward] with `mouse-2'.
;;
;;  If you have previously used the append option (via, for example,
;;  `M-mouse2'), so that there are multiple search results in buffer
;;  *Synonyms*, then using a history command simply takes you to the
;;  preceding (for [Back]) or following (for [Forward]) result in the
;;  buffer, measured from the current cursor position.  Depending on
;;  the cursor position, this might be different from the previous or
;;  next search made previously.
;;
;;  This is for convenience, but it is also more efficient in the case
;;  of a regexp search that takes a long time.  Except for this
;;  special treatment of appended results, whenever you navigate the
;;  search-results history you are actually searching again for a
;;  synonym you sought previously.  The case of appended results is
;;  analogous to accessing a Web browser cache when navigating the
;;  history.
;;
;;  You can of course use modifier keys (Control, Meta) while you
;;  click links [Back] and [Forward], to impose their usual behavior:
;;  search for additional synonyms or append search results, or both.
;;
;;  Finally, some people prefer menus, so there is a Synonyms menu-bar
;;  menu when you are in Synonyms mode, complete with all of the
;;  functionalities described above.
;;
;;  For more information on the browsing possibilities in buffer
;;  *Synonyms*, use `?' in Synonyms mode.
;;
;;
;;  Dictionary Definitions, Antonyms, etc.
;;  --------------------------------------
;;
;;  Synonyms works with a large but simple database of groups of words
;;  and phrases that are synonyms of each other.  This database does
;;  not provide definitions of words or phrases; it simply groups
;;  them.  Command `synonym-definition' (aka `dictionary-definition')
;;  lets you look up a word or phrase (or a regexp) using one or more
;;  dictionaries on the Web.  That is usually the best source for this
;;  kind of information, but you obviously need an Internet connection
;;  to use this command.
;;
;;  Options (variables) `synonyms-dictionary-url' and
;;  `synonyms-dictionary-alternate-url' are URLs you can set to point
;;  to the dictionaries of your choice.  The default value of
;;  `synonyms-dictionary-alternate-url' looks up the search term in
;;  multiple dictionaries, and it lets you use wildcards.  Use `C-h v
;;  synonyms-dictionary-alternate-url' for more information.  The
;;  default value of `synonyms-dictionary-url' usually provides a
;;  quicker answer.  Both of these URLs also give you access to
;;  additional information about the search term (antonyms, etymology,
;;  even pronunciation).
;;
;;  In buffer *Synonyms*, you can simply hit `d' followed by `RET' or
;;  `mouse-2' to look up a term that is in the buffer.  Just as for
;;  looking up a synonym by clicking `mouse-2', if you select text
;;  (region), then that text is looked up.
;;
;;
;;  A Cache File of Synonyms
;;  ------------------------
;;
;;  The very first time you use Synonyms, a large list of synonyms
;;  will be compiled and written to a cache file.  This is slow - it
;;  takes 2-3 minutes - but it is only a one-time cost.  From then on,
;;  whenever you first use Synonyms during an Emacs session, the cache
;;  file will be read (quickly), to create the list of synonyms that
;;  are used for minibuffer completion.
;;
;;
;;  Using Other Thesauri, Dictionaries, and so on - CSV data
;;  --------------------------------------------------------
;;
;;  There is nothing in library Synonyms that ties it to the Moby
;;  Thesaurus II thesaurus.  All of its functionality will work with
;;  any file of comma-separated values.  Each line of such a file is
;;  interpreted as a synonym group, as understood here, and the first
;;  word or phrase on each line is interpreted as a thesaurus entry,
;;  as understood here.  This means only that search results are
;;  organized into sections with entry headers.
;;
;;  If, for example, you had a CSV file of personal contacts, where
;;  the first term in each line was a last name or a company name,
;;  then you could use library Synonyms to query it, producing the
;;  same kind of output as for the thesaurus.
;;
;;  One thing to keep in mind if you try to use library Synonyms with
;;  a different CSV file is that there are several different CSV-file
;;  syntaxes.  The one that Synonyms is built to use is a simple one,
;;  with no quote marks around entries and no embedded quote marks
;;  within entries.
;;
;;  Similarly, there is nothing here that limits the functionality to
;;  English.  If you had a thesaurus in another language, it should
;;  work as well.
;;
;;  Currently, Synonyms works with a single raw synonyms file
;;  (thesaurus) and a corresponding single cache file (for
;;  completion).  However, it would be easy to extend the
;;  functionality to use multiple thesauri or, in general, multiple
;;  CSV files.  Suggestions of requirements (e.g. ways to select a
;;  thesaurus for particular passages of text) are welcome.
 
;;
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Faces defined here -
;;
;;    `synonyms-heading', `synonyms-search-text',
;;    `synonyms-mouse-face'.
;;
;;
;;  User options (variables) defined here -
;;
;;    `synonyms-append-result-flag', `synonyms-cache-file',
;;    `synonyms-file', `synonyms-fill-column',
;;    `synonyms-match-more-flag', `synonyms-mode-hook',
;;    `synonyms-use-cygwin-flag'.
;;
;;  Commands defined here -
;;
;;    `dictionary-definition', `synonyms', `synonyms-append-result',
;;    `synonyms-append-result-no-read', `synonyms-definition',
;;    `synonyms-definition-mouse', `synonyms-definition-no-read',
;;    `synonyms-ensure-synonyms-read-from-cache',
;;    `synonyms-history-backward', `synonyms-history-forward',
;;    `synonyms-make-obarray', `synonyms-match-more',
;;    `synonyms-match-more-no-read',
;;    `synonyms-match-more+append-result',
;;    `synonyms-match-more+append-result-no-read', `synonyms-mode',
;;    `synonyms-mouse', `synonyms-mouse-append-result',
;;    `synonyms-mouse-match-more',
;;    `synonyms-mouse-match-more+append-result', `synonyms-no-read',
;;    `synonyms-write-synonyms-to-cache'.
;;
;;  Non-interactive functions defined here -
;;
;;    `synonyms-action', `synonyms-add-history-links',
;;    `synonyms-default-regexp', `synonyms-define-cache-file',
;;    `synonyms-define-synonyms-file', `synonyms-format-entries',
;;    `synonyms-format-entry', `synonyms-format-finish',
;;    `synonyms-format-synonyms',
;;    `synonyms-hack-backslashes-if-cygwin', `synonyms-lookup',
;;    `synonyms-nearest-word', `synonyms-file-readable-p',
;;    `synonyms-search-entries', `synonyms-search-synonyms',
;;    `synonyms-show-synonyms', `synonyms-file-writable-p'.
;;
;;  Internal variables defined here -
;;
;;    `synonyms-history', `synonyms-history-forward',
;;    `synonyms-list-for-obarray', `synonyms-mode-map',
;;    `synonyms-obarray', `synonyms-search-text'.
;;
;;  Key bindings made here - see `synonyms-mode'.  All key bindings
;;  are local to Synonyms mode; no global bindings are made here.
 
;;
;;
;;  Acknowledgements
;;  ----------------
;;
;;  The basic functionality provided here was derived from library
;;  `mthesaur.el', by Tad Ashlock <taashlo@cyberdude.com>.  That
;;  library, in turn, was inspired by library `thesaurus.el', by Ray
;;  Nickson.  Thanks also to those who sent helpful bug reports.
;;
;;
;;  Note on MS Windows Emacs 20 and Cygwin `grep'
;;  ---------------------------------------------
;;
;;  There is apparently a bug in the Emacs (at least versions 20-22) C
;;  code that implements function `call-process' on MS Windows.  When
;;  using native Windows Emacs with Cygwin commands, such as `grep',
;;  the C code removes a level of backslashes in some cases, so string
;;  arguments supplied to `call-process' need to have twice as many
;;  backslashes as they should need in those cases.  It is for this
;;  reason that option `synonyms-use-cygwin-flag' is supplied here.
;;  When that option is non-nil, backslashes in regexps are hacked to
;;  do the right thing.  (In Emacs 20, this means doubling the
;;  backslashes; in Emacs 21-22, this means doubling them unless there
;;  are spaces in the search string.)
;;
;;
;;  Maybe To Do?
;;  ------------
;;
;;  1. It would be ideal to have not only synonym information but also
;;     definitions, antonyms, more general and more specific terms,
;;     filtering by part of speech (verb vs adjective etc.), and so
;;     on.  A good example of what I'd really like to have is provided
;;     by the free Windows program WordWeb (available here:
;;     http://wordweb.info/).  Combining that functionality with
;;     Icicles completion features would provide a great tool, IMO.
;;
;;     `synonyms-definition*' goes a long way toward providing this,
;;     and perhaps it is the best way to go, since there is so much
;;     more definitional info on the Web.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;; 2012/03/02 dadams
;;     Require cl.el at compile time only for Emacs 20.
;; 2011/07/30 dadams
;;     Moved Icicles code to icicles-cmd2.el.  Removed soft-require of icicles.el.
;; 2011/02/11 dadams
;;     Better defaults for faces, for dark backgrounds.
;; 2011/01/04 dadams
;;     Added autoload cookies (for defgroup, defface, defcustom, and commands).
;; 2010/08/20 dadams
;;     synonyms - non-Icicles version: Made ARG optional too.
;;     synonyms(-no-read|-history-(backward|forward)):
;;       Use ARG, not current-prefix-arg.
;; 2010/01/12 dadams
;;     synonyms-history-(backward|forward): save-excursion + set-buffer -> with-current-buffer.
;; 2007/12/05 dadams
;;     synonyms-obarray: Removed * doc-string prefix.
;; 2007/02/10 dadams
;;     icicle-sort-case-insensitively -> icicle-case-insensitive-string-less-p.
;; 2006/12/22 dadams
;;     Renamed group synonyms to Synonyms.  :group 'icicles -> :group 'Icicles.
;; 2006/03/31 dadams
;;     synonyms-write-synonyms-to-cache: Use prin1 instead of pp.
;; 2006/03/17 dadams
;;     synonyms-file-(read|writ)able-p: Put non-empty string condition first.
;; 2006/03/14 dadams
;;     synonyms-file-(read|writ)able-p: Make sure also not a directory.
;; 2006/03/12 dadams
;;     synonyms-ensure-synonyms-read-from-cache, synonyms-define-synonyms-file: 
;;       Set synonyms(-cache)-file to expanded version.
;; 2006/03/01 dadams
;;     Updated Commentary to mention Icicles completion of synonyms.
;; 2006/02/02 dadams
;;     synonyms-define-cache-file: Fixed typo.
;; 2006/01/28 dadams
;;     synonyms-define-cache-file: wrap file-name-directory in expand-file-name.
;; 2006/01/19 dadams
;;     synonyms-format-finish: Minor tweak to regexp: space and tab, but not newline or formfeed.
;; 2006/01/18 dadams
;;     Added dictionary definition lookup:
;;       Added: synonyms-dictionary(-alternate)-url, synonyms-definition*.
;;       Bound synonyms-definition-*. 
;; 2006/01/14 dadams
;;     Bug fixes -
;;     Make sure file name is expanded (thanks to Nikos Apostolakis): 
;;       synonyms-search-(entries|synonyms): Expand file name.
;;       synonyms-define-*-file: Set variable after expanding file name.
;;       synonyms-format-entry, synonyms-history-*, synonyms-add-history-links:
;;         Raise error if search finds nothing.
;;     synonyms-hack-backslashes-if-cygwin: Don't double if spaces and not Emacs 20.
;;       Renamed synonyms-double-backslashes-if-cygwin to synonyms-hack-backslashes-if-cygwin.
;;     synonyms-mode-map: swapped bindings for C-mouse-2 and C-down-mouse-2, for Emacs 22.
;; 2006/01/11 dadams
;;     Fixed typo: require 'synonyms. (Thanks to Nikos Apostolakis.)
;; 2006/01/07 dadams
;;     Added :link.
;; 2006/01/04 dadams
;;     synonyms-format-finish: Don't skip numbered header, so highlight multiple synonyms in entry.
;; 2006/01/02 dadams
;;     Added: synonyms-define-cache-file, synonyms-define-synonyms-file,
;;            synonyms-file-readable-p, synonyms-file-writable-p.
;;     synonyms-make-obarray: Use synonyms-define-synonyms-file.
;;                            Use synonyms-mode, to get modified syntax (bug fix).
;;     synonyms-write-synonyms-to-cache: Use synonyms-define-cache-file.
;;     synonyms-ensure-synonyms-read-from-cache: Use synonyms-file-readable-p.
;;     synonyms(-cache)-file: Use empty string as initial value.
;;     Thanks to Alex Schroeder [alex@emacswiki.org] for suggestion to prompt for file names.
;; 2005/12/31 dadams
;;     Added menu-bar Synonyms menu.
;;     Renamed synonyms-read-synonyms-from-cache to synonyms-ensure-synonyms-read-from-cache.
;;       Call it from synonyms, not from synonyms-mode.
;;     Defined synonyms-mode-map per convention.
;;     synonyms-match-more, synonyms-append-result, synonyms-match-more+append-result:
;;       Use synonyms, not synonyms-no-read.
;;     Added: synonyms-*-no-read.  Bound those, not the new read versions.
;; 2005/12/29 dadams
;;     Treat modifiers with clicks on [Back] and [Forward] links.
;;     synonyms-history-(backward|forward): Add prefix arg.  Bind options.
;;     synonyms-mode, synonyms-lookup: Disable undo.
;; 2005/12/28 dadams
;;     Added: synonyms-history-(backward|forward), synonyms-add-history-links, synonyms-link.
;;     Added: [Back] and [Forward] links.
;;     synonyms-show-synonyms: Put cursor on first synonym.
;;     synonyms-mouse, synonyms-lookup: Removed save-excursion.
;;     synonyms-mouse: Treat clicks on [Back] and [Forward] links too.
;;     synonyms-format-finish: Added save-excursion for last part: filling and adding mouse-face.
;;     synonyms-nearest-word: Remove text properties.
;;     synonyms: Use synonym-action.
;;     synonyms-lookup: When no synonyms found, remove search-text from history.
;;     Require cl.el when compile.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (when (< emacs-major-version 21) (require 'cl))) ;; push, pop

(require 'thingatpt nil t)  ;; (no error if not found): word-at-point
(when (and (require 'thingatpt+ nil t);; (no error if not found): word-nearest-point
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))

;; Note: You might get byte-compiler warnings that variables `appendp'
;;       and `morep' are free: .  This is OK.

;;;;;;;;;;;;;;;;;;;;;;;;;



 
;;; Faces (alphabetical) -----------------------------------

;;;###autoload
(defgroup Synonyms nil
  "Commands to look up synonyms in a thesaurus."
  :prefix "synonyms-"
  :group 'convenience :group 'help :group 'apropos :group 'matching
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
synonyms.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/synonyms.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/cgi-bin/wiki/Synonyms")
  :link '(emacs-commentary-link :tag "Commentary" "synonyms"))

;;;###autoload
(defface synonyms-heading '((((background dark)) (:foreground "Yellow"))
                            (t (:foreground "Blue")))
  "*Face for different synonym types."
  :group 'Synonyms :group 'faces)

;;;###autoload
(defface synonyms-search-text '((t (:foreground "Red")))
  "*Face for the term whose synonyms were sought."
  :group 'Synonyms :group 'faces)

;;;###autoload
(defface synonyms-link '((((background dark)) (:foreground "Yellow" :underline t))
                         (t (:foreground "Blue" :underline t)))
  "*Face for history links."
  :group 'Synonyms :group 'faces)

;;;###autoload
(defface synonyms-mouse-face '((((background dark)) (:background "DarkCyan"))
                               (t (:background "Cyan")))
  "*Mouse face for the term whose synonyms were sought."
  :group 'Synonyms :group 'faces)



 
;;; User Options (alphabetical) ----------------------------

;;;###autoload
(defcustom synonyms-append-result-flag nil
  "*t means that `synonyms' appends search result to previous results.
No other value, besides t, has this effect.

This can be overridden by using a negative prefix argument,
for example, `M--'.  If you use `C-u C-u', then both this and
`synonyms-match-more-flag' are overridden."
  :type 'boolean :group 'Synonyms)

;;;###autoload
(defcustom synonyms-cache-file ""
  "*Location to write cache file containing synonyms.
Written to save the list of synonyms used for completion.
This is an absolute (complete-path) location, including the file name."
  :type '(file :must-match t) :group 'Synonyms)

;;;###autoload
(defcustom synonyms-file ""
  "*Location of thesaurus file `mthesaur.txt'.
This is an absolute (complete-path) location, including the file name."
  :type '(file :must-match t) :group 'Synonyms)

;;;###autoload
(defcustom synonyms-fill-column 80
  "*Synonyms* buffer text is wrapped (filled) to this many columns."
  :type 'integer :group 'Synonyms)

;;;###autoload
(defcustom synonyms-match-more-flag nil
  "*t means additional thesaurus entries can be matched by `synonyms'.
No other value, besides t, has this effect.

A value of t means two things:
 1) Input can match parts of synonyms, in addition to whole synonyms.
 2) All synonyms are shown, even if input matches a thesaurus entry.

This can be overridden by using a positive prefix argument,
  for example, `C-u'.  If you use `C-u C-u', then both this and
`synonyms-append-result-flag' are overridden."
  :type 'boolean :group 'Synonyms)

;;;###autoload
(defcustom synonyms-mode-hook nil
  "*Normal hook run when entering Thesaurus mode."
  :type 'hook :group 'Synonyms)

;;;###autoload
(defcustom synonyms-use-cygwin-flag nil
  "*Non-nil means to double backslashes in arguments to `call-process'.
There is apparently a bug in the Emacs (at least versions 20-22) C
code that implements function `call-process' on MS Windows.  When
using native Windows Emacs with Cygwin commands, such as `grep', the C
code removes a level of backslashes, so string arguments supplied to
`call-process' need to have twice as many backslashes as they should
need.  If you are using Emacs on Windows and Cygwin `grep', then you
probably will want to use a non-nil value for
`synonyms-use-cygwin-flag'."
  :type 'boolean :group 'Synonyms)

;;;###autoload
(defcustom synonyms-dictionary-url "http://dictionary.reference.com/search?q="
  "*URL of a Web dictionary lookup.  Text to look up is appended to this.
See also `synonyms-dictionaries-url'."
  :type 'string :group 'Synonyms)

;;;###autoload
(defcustom synonyms-dictionary-alternate-url "http://www.onelook.com/?ls=b&w="
  "*URL of a Web dictionary lookup.  Text to look up is appended to this.
The default value, \"http://www.onelook.com/?ls=b&w=\" lets you use `?'
and `*' as wildcards in the terms you look up.  These are not used as
regexp wildcards, however.  `?' stands for any single character, and
`*' stands for any sequence of characters.  In terms of regexp syntax,
`?' here is equivalent to the regexp `.', and `*' is equivalent to the
regexp `.*'.  See http://www.onelook.com/?c=faq#patterns for more
information on the allowed wildcard patterns.
See also `synonyms-dictionary-url'."
  :type 'string :group 'Synonyms)
 
;;; Internal variables (alphabetical) ----------------------

(defvar synonyms-history nil "Minibuffer history list for thesaurus lookup.")

(defvar synonyms-history-forward nil
  "Minibuffer history list for thesaurus lookup using `synonyms-history-backward'.")

(defvar synonyms-list-for-obarray nil "List of synonyms to be used for completion")

(defvar synonyms-mode-map nil "Keymap for `synonyms-mode'.")

(unless synonyms-mode-map
  (let ((map  (make-sparse-keymap "Synonyms")))
    (define-key map [(?d) (mouse-2)] 'synonyms-definition-mouse)
    (define-key map "d\r"            'synonyms-definition-no-read)
    (define-key map "s"              'synonyms)
    (define-key map [S-return]       'synonyms)
    (define-key map "\r"             'synonyms-no-read)
    (define-key map [C-return]       'synonyms-match-more-no-read)
    (define-key map [M-return]       'synonyms-append-result-no-read)
    (define-key map [C-M-return]     'synonyms-match-more+append-result-no-read)
    (define-key map [mouse-2]        'synonyms-mouse)
    (define-key map [C-mouse-2]      'undefined)
    (define-key map [C-down-mouse-2] 'synonyms-mouse-match-more) ; Get rid of `facemenu-mouse-menu'
    (define-key map [M-mouse-2]      'synonyms-mouse-append-result)
    (define-key map [C-M-mouse-2]    'synonyms-mouse-match-more+append-result)
    (define-key map "l"              'synonyms-history-backward) ; As in Info
    (define-key map "p"              'synonyms-history-backward) ; As in previous
    (define-key map "r"              'synonyms-history-forward) ; As in Info
    (define-key map "n"              'synonyms-history-forward) ; As in next
    (define-key map [mouse-4]        'synonyms-history-backward)
    (define-key map [mouse-5]        'synonyms-history-forward)
    (define-key map " "              'scroll-up) ; SPC
    (define-key map "\^?"            'scroll-down) ; DEL
    (define-key map "?"              'describe-mode)
    (define-key map "q"              'quit-window)
    (define-key map [menu-bar]             (make-sparse-keymap))
    (define-key map [menu-bar synonyms]    (cons "Synonyms" map))
    (define-key map [synonyms-help]        '("Help" . describe-mode))
    (define-key map [synonyms-separator-2] '("--"))
    (define-key map [synonyms-next]        '("Show Next" . synonyms-history-forward))
    (put 'synonyms-history-forward 'menu-enable 'synonyms-history-forward)
    (define-key map [synonyms-previous]    '("Show Previous" . synonyms-history-backward))
    (put 'synonyms-history-backward 'menu-enable '(and synonyms-history (cdr synonyms-history)))
    (define-key map [synonyms-separator]   '("--"))
    (define-key map [synonyms-more-append]
      '("Find (Max), Append Results" . synonyms-match-more+append-result))
    (define-key map [synonyms-append]
      '("Find, Append Results" . synonyms-append-result))
    (define-key map [synonyms-more]        '("Find (Max)" . synonyms-match-more))
    (define-key map [synonyms-synonyms]    '("Find" . synonyms))
    (setq synonyms-mode-map  map)))

;; 103307 is the smallest prime > 103304, which is the number of synonyms.
(defvar synonyms-obarray (make-vector 103307 0)
  "Obarray of synonyms.  Used for completion.")

(defvar synonyms-search-text nil "Current text being looked up (matched).")



 
;;; Functions ----------------------------------------------

;;;###autoload
(define-derived-mode synonyms-mode text-mode "Synonyms"
  "Major mode for browsing thesaurus entries (synonyms).
Like Text mode but with these additional key bindings:

 \\<synonyms-mode-map>\\[synonyms-mouse],     \\[synonyms-no-read],     \\[synonyms] - \
Look up synonyms for a word or phrase
 \\[synonyms-mouse-match-more],   \\[synonyms-match-more]   - Like \\[synonyms-no-read], but \
try to match more terms
 \\[synonyms-mouse-append-result],   \\[synonyms-append-result]   - Like \\[synonyms-no-read], but \
add result to previous result
 \\[synonyms-mouse-match-more+append-result], \\[synonyms-match-more+append-result] - Like \
\\[synonyms-match-more] and \\[synonyms-append-result] combined

 \\[scroll-up] - Scroll down through the buffer of synonyms
 \\[scroll-down] - Scroll up through the buffer of synonyms
 \\[describe-mode]   - Display this help
 \\[quit-window]   - Quit Synonyms mode

Of the various key bindings that look up synonyms, the most flexible
is \\[synonyms] - it prompts you for the search string to match.  This
can be a regular expression (regexp).  The other lookup bindings are
for convenience - just click.

In Synonyms mode, Transient Mark mode is enabled.

Options `synonyms-match-more-flag' and `synonyms-append-result-flag'
affect synonym matching and the results.  For convenience, \\[synonyms-mouse-match-more],
\\[synonyms-mouse-append-result], and \\[synonyms-mouse-match-more+append-result] \
toggle the effect of those options for the
duration of the command.

Note that even though Synonyms mode is similar to Text mode, buffer
`*Synonyms*' is read-only, by default - use `C-x C-q' to toggle.

Turning on Synonyms mode runs the normal hooks `text-mode-hook' and
`synonyms-mode-hook' (in that order)."

  ;; Synonyms to account for:
  ;; `$', `1', `0': $100-a-plate dinner; `2': catch-22, V-2; `3': 3-D; `9': strontium 90.
  ;; To match `$', you will of course need to escape it: `\$'.
  (modify-syntax-entry ?- "w" synonyms-mode-syntax-table) ; Make hyphen (-) a word character.
  (modify-syntax-entry ?1 "w" synonyms-mode-syntax-table) ; Make numerals 1,2,3,9,0 word characters.
  (modify-syntax-entry ?2 "w" synonyms-mode-syntax-table)
  (modify-syntax-entry ?3 "w" synonyms-mode-syntax-table)
  (modify-syntax-entry ?9 "w" synonyms-mode-syntax-table)
  (modify-syntax-entry ?0 "w" synonyms-mode-syntax-table)
  (modify-syntax-entry ?$ "w" synonyms-mode-syntax-table) ; Make dollar ($) a word character.
  (buffer-disable-undo)
  (setq fill-column  synonyms-fill-column)
  (set (make-local-variable 'transient-mark-mode) t))

;;;###autoload
(defun synonyms-ensure-synonyms-read-from-cache ()
  "Ensure synonyms are in `synonyms-obarray', from `synonyms-cache-file'.
If this file does not yet exist, then it and the obarray are created.
Creating the obarray for the first time takes 2-3 minutes.
This does nothing if the obarray is already complete."
  (interactive)
  (unless (intern-soft "synonym" synonyms-obarray) ; Do nothing if already complete.
    (setq synonyms-list-for-obarray  () ; Just to make sure.
          synonyms-cache-file        (expand-file-name synonyms-cache-file))
    (if (synonyms-file-readable-p synonyms-cache-file)
        (let ((list-buf  (find-file-noselect synonyms-cache-file 'nowarn 'raw))
              (obarray   synonyms-obarray))
          (unwind-protect
               (setq synonyms-list-for-obarray  (read list-buf))
            (kill-buffer list-buf)))
      (synonyms-make-obarray)           ; Create obarray from scratch
      (synonyms-write-synonyms-to-cache)))) ; and write it out, for next time.

;;;###autoload
(defun synonyms-make-obarray ()
  "Fill `synonyms-obarray' with the available synonyms."
  (interactive)
  (unless (intern-soft "synonym" synonyms-obarray) ; Do nothing if already complete.
    (synonyms-define-synonyms-file)
    (with-temp-message "Building synonyms list for completion.  This will take a few minutes..."
      (let ((thesaurus-buf  (find-file-noselect synonyms-file 'nowarn 'raw))
            synonym)
        (unwind-protect
             (save-current-buffer
               (set-buffer thesaurus-buf)
               (goto-char (point-min))
               (synonyms-mode)          ; To use the modified syntax table.
               (while (re-search-forward "\\(\\(\\w\\|[ ]\\)+\\)\\(,\\|$\\)" nil t)
                 (setq synonym  (buffer-substring (match-beginning 1) (match-end 1)))
                 (intern synonym synonyms-obarray)))
          (kill-buffer thesaurus-buf))))))

(defun synonyms-define-synonyms-file ()
  "Prompt user to define `synonyms-file', unless it is readable."
  (setq synonyms-file  (expand-file-name synonyms-file))
  (unless (synonyms-file-readable-p synonyms-file)
    (while (not (synonyms-file-readable-p synonyms-file))
      (setq synonyms-file  (read-file-name "Thesaurus file: " nil nil 'confirm "mthesaur.txt")))
    (custom-set-variables (list 'synonyms-file
                                (setq synonyms-file  (expand-file-name synonyms-file))
                                'now))))

;;;###autoload
(defun synonyms-write-synonyms-to-cache ()
  "Write synonyms in `synonyms-obarray' to file `synonyms-cache-file'."
  (interactive)
  (synonyms-define-cache-file)
  (with-temp-message "Writing synonyms cache file..."
    (with-temp-file synonyms-cache-file
      (mapatoms (lambda (symb) (push symb synonyms-list-for-obarray)) synonyms-obarray)
      (prin1 synonyms-list-for-obarray (current-buffer)))))

(defun synonyms-define-cache-file ()
  "Prompt user to define `synonyms-cache-file', unless it is writable."
  (unless (synonyms-file-writable-p synonyms-cache-file)
    (while (not (synonyms-file-writable-p synonyms-cache-file))
      (setq synonyms-cache-file
            (read-file-name "Cache file: "
                            (expand-file-name (file-name-directory synonyms-file)) nil nil
                            (concat (file-name-nondirectory synonyms-file) ".cache"))))
    (custom-set-variables (list 'synonyms-cache-file
                                (setq synonyms-cache-file  (expand-file-name synonyms-cache-file))
                                'now))))

(defun synonyms-file-readable-p (file)
  "Return non-nil if FILE (a string) names a readable file."
  (and (not (string= "" file)) (file-readable-p file) (not (file-directory-p file))))

(defun synonyms-file-writable-p (file)
  "Return non-nil if FILE (a string) names a writable file."
  (and (not (string= "" file)) (file-writable-p file) (not (file-directory-p file))))

(defun synonyms (&optional arg regexp)
  "Show synonyms that match a regular expression (e.g. a word or phrase).
You are prompted for the regexp.  By default, it is the text
of the region, if it is active and `transient-mark-mode' is enabled,
or the nearest word to the cursor, if not.

Option `synonyms-match-more-flag' non-nil means additional thesaurus
  entries can be matched.  This can be more time-consuming.  It means
  two things:

  1) Input can match parts of synonyms, in addition to whole synonyms.
  2) All synonyms are shown, even if input matches a thesaurus entry.

Option `synonyms-append-result-flag' non-nil means to append search
  result to previous results.

A prefix argument toggles the meaning of each of those options for the
duration of the command:

  If `C-u' or `C-u C-u', then toggle `synonyms-match-more-flag'.
  If negative or `C-u C-u', then toggle `synonyms-append-result-flag'.

\(`C-u C-u' thus means toggle both options.)

When called from Lisp, optional second argument REGEXP is the regexp
to match (no prompting)."
  (interactive "P")
  (synonyms-ensure-synonyms-read-from-cache) ; Fill `synonyms-obarray', for use in completion.
  (let* ((num-arg              (prefix-numeric-value arg))
         (morep                (eq synonyms-match-more-flag (atom arg)))
         (appendp              (eq synonyms-append-result-flag (and (wholenump num-arg)
                                                                    (/= 16 num-arg))))
         (default-search-text  (or regexp (synonyms-default-regexp)))
         (search-text          (or regexp
                                   (let ((case-fold-search  t)) ; Case-insensitive completion.
                                     (completing-read
                                      "Show synonyms for word or phrase (regexp): "
                                      synonyms-obarray nil nil nil 'synonyms-history
                                      default-search-text)))))
    (synonyms-action search-text)))

(defun synonyms-action (search-text)
  "Helper function for command `synonyms'.
APPENDP and MOREP are free here."
  (setq synonyms-search-text  search-text) ; Save it.
  (when (string= "" search-text) (error "No text to look up"))
  (unless (member search-text synonyms-history) (push search-text synonyms-history))
  ;; Change `.' to `[^,]' in `search-text', so we don't mix terms.
  (setq search-text  (replace-regexp-in-string "\\." "[^,]" search-text nil t))
  (synonyms-lookup search-text (and (boundp 'appendp) appendp) (and (boundp 'morep) morep)))

;;;###autoload
(defun synonyms-no-read (arg)
  "Same as command `synonyms', but uses the default input text (regexp)."
  (interactive "P")
  (let* ((num-arg      (prefix-numeric-value arg))
         (morep        (eq synonyms-match-more-flag (atom arg)))
         (appendp      (eq synonyms-append-result-flag (and (wholenump num-arg) (/= 16 num-arg))))
         (search-text  (synonyms-default-regexp)))
    (setq synonyms-search-text  search-text) ; Save it.
    (when (string= "" search-text) (error "No text to look up"))
    (unless (member search-text synonyms-history) (push search-text synonyms-history))
    ;; Change `.' to `[^,]' in `search-text', so we don't mix terms.
    (setq search-text  (replace-regexp-in-string "\\." "[^,]" search-text nil t))
    (synonyms-lookup search-text appendp morep)))

;;;###autoload
(defun synonyms-match-more ()
  "Same as using `synonyms' with `synonyms-match-more-flag' = t."
  (interactive)
  (let ((synonyms-match-more-flag  t))
    (synonyms)))

;;;###autoload
(defun synonyms-match-more-no-read (arg)
  "Same as using `synonyms' with `synonyms-match-more-flag' = t."
  (interactive "P")
  (let ((synonyms-match-more-flag  t))
    (synonyms-no-read arg)))

;;;###autoload
(defun synonyms-append-result ()
  "Same as using `synonyms' with `synonyms-append-result-flag' = t."
  (interactive)
  (let ((synonyms-append-result-flag  t))
    (synonyms)))

;;;###autoload
(defun synonyms-append-result-no-read (arg)
  "Same as using `synonyms' with `synonyms-append-result-flag' = t."
  (interactive "P")
  (let ((synonyms-append-result-flag  t))
    (synonyms-no-read arg)))

;;;###autoload
(defun synonyms-match-more+append-result ()
  "Like `synonyms-match-more-flag' = `synonyms-append-result-flag' = t."
  (interactive)
  (let ((synonyms-match-more-flag     t)
        (synonyms-append-result-flag  t))
    (synonyms)))

;;;###autoload
(defun synonyms-match-more+append-result-no-read (arg)
  "Like `synonyms-match-more-flag' = `synonyms-append-result-flag' = t."
  (interactive "P")
  (let ((synonyms-match-more-flag     t)
        (synonyms-append-result-flag  t))
    (synonyms-no-read arg)))

;;;###autoload
(defun synonyms-mouse (event arg)
  "Show synonyms that match a regular expression (e.g. a word or phrase).
The regexp to match is the synonym or region clicked with mouse-2.  If
the region is active, but a synonym elsewhere is clicked, that synonym
is used, not the selected text.

You can either click a listed synonym, to see its synonyms, or select
one or more words and click the selection, to see matching synonyms.
To quickly select a series of words: double-click mouse-1 to select
the first word, then click mouse-3 to extend the selection to the last
word.

Selection is useful when you want to see synonyms of a similar term.
For example, instead of clicking the listed synonym `bleeding heart', you
might select `heart' and click that.

The prefix argument acts the same as for command `synonyms'.

If you click a history link with mouse-2, previously retrieved search
results are revisited."
  (interactive "e\nP")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (let ((beg     (region-beginning))
        (end     (region-end))
        (active  mark-active))
    (goto-char (posn-point (event-end event)))
    (cond ((get-text-property (point) 'back-link) (synonyms-history-backward nil))
          ((get-text-property (point) 'forward-link) (synonyms-history-forward nil))
          (t (if (and active (> (point) beg) (< (point) end))
                 (goto-char end)
               (deactivate-mark))       ; User did not click inside region, so deactivate it.
             (synonyms-no-read arg)))))

;;;###autoload
(defun synonyms-mouse-match-more (event arg)
  "Same as `synonyms-mouse' with `synonyms-match-more-flag' = t."
  (interactive "e\nP")
  (let ((synonyms-match-more-flag  t))
    (synonyms-mouse event arg)))

;;;###autoload
(defun synonyms-mouse-append-result (event arg)
  "Same as `synonyms-mouse' with `synonyms-append-result-flag' = t."
  (interactive "e\nP")
  (let ((synonyms-append-result-flag  t))
    (synonyms-mouse event arg)))

;;;###autoload
(defun synonyms-mouse-match-more+append-result (event arg)
  "Like `synonyms-match-more-flag' = `synonyms-append-result-flag' = t."
  (interactive "e\nP")
  (let ((synonyms-match-more-flag     t)
        (synonyms-append-result-flag  t))
    (synonyms-mouse event arg)))

(defun synonyms-default-regexp ()
  "Return the default regexp for `synonym' and `synonyms-mouse'.
If the region is active in `transient-mark-mode', use its text.
Else, if this is *Synonyms* buffer, use the synonym under the cursor.
Else use the word nearest the cursor.

An active region has no effect except in `transient-mark-mode'."
  (if (and mark-active transient-mark-mode) ; Use region text, if active.
      (buffer-substring-no-properties (point) (mark))
    (if (eq major-mode 'synonyms-mode)  ; Use mouse-face text, if in synonyms-mode.
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end  (point)
                  beg  (1+ (point))))
          (when (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
            (setq end  (1- (point))
                  beg  (point)))
          (if (null beg)
              (synonyms-nearest-word)   ; Punt - no mouse-face, for some reason.
            (setq beg  (previous-single-property-change beg 'mouse-face)
                  end  (or (next-single-property-change end 'mouse-face) (point-max)))
            (replace-regexp-in-string   ; Replace newlines with spaces, except at the
             "\\(^ \\| $\\)" ""         ; beginning and end.
             (replace-regexp-in-string "[\n]" " " (buffer-substring-no-properties beg end) nil t)
             nil t)))
      (synonyms-nearest-word))))

(defun synonyms-nearest-word ()
  "Word nearest the cursor."
  (let ((word  (if (fboundp 'word-nearest-point)
                   (word-nearest-point) ; In `thingatpt+.el'.
                 (word-at-point))))     ; In `thingatpt.el'.
    (set-text-properties 0 (length word) nil word) ; Remove all text properties.
    word))

(defun synonyms-lookup (search-text appendp morep)
  "Search the thesaurus for SEARCH-TEXT.
APPEND-P non-nil means to append search result to previous results.
MORE-P non-nil means additional thesaurus entries can be matched."
  (save-selected-window
    (with-temp-message
        (format "Looking up %s synonyms for \"%s\"%s..." (if morep "(max)" "")
                (replace-regexp-in-string (regexp-quote "[^,]") "." search-text nil t)
                (if appendp " (appending)" ""))
      (let ((temp-buf  (generate-new-buffer " *Temp*")))
        (unwind-protect
             (progn
               (set-buffer temp-buf)
               (buffer-disable-undo)    ; Make sure (should already be, because of *Temp* name).
               (erase-buffer)
               (let ((entry-p  (synonyms-search-entries search-text temp-buf morep)))
                 ;; For `morep' search, we don't stop even if we find an entry.
                 (unless (if morep
                             (or (synonyms-search-synonyms search-text temp-buf t) entry-p)
                           (or entry-p (synonyms-search-synonyms search-text temp-buf nil)))
                   (pop synonyms-history) ; Remove it from search history, so we don't try again.
                   (error "No synonyms found for `%s'" search-text))
                 (let ((results-buf  (get-buffer-create "*Synonyms*")))
                   (synonyms-format-synonyms search-text morep)
                   (synonyms-show-synonyms temp-buf results-buf appendp)
                   (message nil))))
          (kill-buffer temp-buf))))))

(defun synonyms-search-entries (search-text buf morep)
  "Search thesaurus entries (headings) for SEARCH-TEXT.
Put result in buffer BUF.
MORE-P non-nil means additional thesaurus entries can be matched."
  (call-process "grep" nil buf nil "-i" (synonyms-hack-backslashes-if-cygwin
                                         (if morep
                                             (format "^\\w*%s\\w*," search-text)
                                           (format "^%s," search-text)))
                (expand-file-name synonyms-file))
  (not (zerop (count-lines (point-min) (point-max)))))

(defun synonyms-search-synonyms (search-text buf morep)
  "Search thesaurus body for SEARCH-TEXT.
SEARCH-TEXT is a regexp that can match any part of a thesaurus term.
Put result in buffer BUF.
MORE-P non-nil means additional thesaurus entries can be matched."
  (call-process "grep" nil buf nil "-i" (synonyms-hack-backslashes-if-cygwin
                                         (if morep
                                             (format ",\\w*%s\\w*\\(,\\|$\\)" search-text)
                                           (format ",%s\\(,\\|$\\)" search-text)))
                (expand-file-name synonyms-file))
  (not (zerop (count-lines (point-min) (point-max)))))

(defun synonyms-hack-backslashes-if-cygwin (string)
  "Double each backslash in STRING, unless it contains SPC characters.
More precisely, if `synonyms-use-cygwin-flag' is non-nil and this is
Emacs 20 or there are no spaces in STRING, then double any backslashes
in STRING.

This is an ugly hack made necessary because of bugs in Emacs C code."
  (when (and synonyms-use-cygwin-flag
             (or (= emacs-major-version 20) (not (string-match " " string))))
    (setq string  (replace-regexp-in-string "[\\]" "\\\\" string nil t)))  
  string)

(defun synonyms-format-synonyms (search-text morep)
  "Format synonyms that match SEARCH-TEXT.
MORE-P non-nil means additional thesaurus entries can be matched."
  (goto-char (point-min))
  (let ((entries-count  (count-lines (point-min) (point-max))))
    (if (= entries-count 1)
        (synonyms-format-entry search-text t morep)
      (synonyms-format-entries search-text entries-count morep))
    (synonyms-format-finish search-text morep)))

(defun synonyms-format-entry (search-text single-p morep)
  "Format a single thesaurus entry that matches SEARCH-TEXT.
SINGLE-P non-nil means there is only one entry."
  (beginning-of-line)
  (let ((beg      (point))
        (orig     (if morep             ; Use saved search text.
                      (format "\\w*\\(%s\\)\\w*" synonyms-search-text)
                    (format "\\(%s\\)" synonyms-search-text)))
        (entry-p  nil)
        term end)
    (when single-p (insert "Synonyms for "))
    (setq term  (point))
    (when (looking-at orig) (setq entry-p t))
    (unless (search-forward "," nil t) (error "Bad thesaurus file - no commas"))
    (setq end  (match-beginning 0))
    (replace-match ":\n\n" nil t)
    (cond (single-p
           (add-text-properties beg term '(face synonyms-heading))
           (add-text-properties term end (if entry-p
                                             '(face synonyms-search-text
                                               mouse-face synonyms-mouse-face)
                                           '(face synonyms-heading)))
           (add-text-properties end (+ 2 end) '(face synonyms-heading)))
          (t
           (add-text-properties beg (1+ end) '(face synonyms-heading))
           (save-excursion
             (forward-line -2)
             (save-restriction
               (narrow-to-region (point) (save-excursion (end-of-line) (backward-char) (point)))
               (unless (search-forward ". " nil t)
                 (error "Badly formatted numeric entry - no period"))
               (add-text-properties (point) (point-max) '(mouse-face synonyms-mouse-face))
               (when (looking-at orig)
                 (add-text-properties (match-beginning 1) (match-end 1)
                                      '(face synonyms-search-text)))))))
    (forward-line)))

(defun synonyms-format-entries (search-text entries-count morep)
  "Format thesaurus entries that have synonyms matching SEARCH TEXT.
ENTRIES-COUNT is the number of entries.
MORE-P non-nil means additional thesaurus entries can be matched."
  (let ((countdown  entries-count)
        (beg        (point))
        (part1      "Synonyms for ")
        (part2      ":\n"))
    (insert part1 synonyms-search-text part2)
    (add-text-properties beg (setq beg  (+ beg (length part1))) '(face synonyms-heading))
    (add-text-properties beg (setq beg  (+ beg (length synonyms-search-text)))
                         '(face synonyms-search-text mouse-face synonyms-mouse-face))
    (add-text-properties beg (+ beg (length part2)) '(face synonyms-heading))
    (while (> countdown 0)
      (setq countdown  (1- countdown))
      (insert (format "\n\%s. " (- entries-count countdown)))
      (synonyms-format-entry search-text nil morep))))

(defun synonyms-format-finish (search-text morep)
  "Finish formatting synonyms matching SEARCH-TEXT.
MORE-P non-nil means additional thesaurus entries can be matched."
  ;; Put a space after each comma.
  (goto-char (point-min))
  (forward-line)                        ; First line might have [^,] in it.
  (while (search-forward "," nil t) (replace-match ", " nil t))
  (goto-char (point-min))
  (let ((case-fold-search       t)
        (new-search-text        (if morep
                                    (format "\\(^\\|, \\)\\w*\\(%s\\)\\w*\\($\\|,\\)" search-text)
                                  (format "\\(^\\|, \\)\\(%s\\)\\($\\|,\\)" search-text)))
        (no-numbered-headers-p  (not (re-search-forward "^[0-9]+[.]" nil t))))
    (goto-char (point-min))
    (forward-line)
    (while (re-search-forward new-search-text nil t)
      (add-text-properties (match-beginning 2) (match-end 2) '(face synonyms-search-text))
      (goto-char (match-end 2)))
    ;; Do `synonyms-mode' here too, so hyphen will be a word char.
    ;; IS THERE A WAY TO DO A LET to change the syntax of hyphen, instead of entering mode?
    (synonyms-mode)
    (save-excursion
      (goto-char (point-min))
      (forward-line)
      (while (re-search-forward "\\(^\\|, \\)\\(\\(\\w\\|[\\t ]\\)+\\)\\($\\|,\\)" nil t)
        (add-text-properties (match-beginning 2) (match-end 2) '(mouse-face synonyms-mouse-face))
        (goto-char (match-end 2)))
      (fill-region (point-min) (point-max)))
    (synonyms-add-history-links)))

(defun synonyms-add-history-links ()
  "Add Back and Forward chronological navigation links"
  (save-excursion
    (unless (re-search-backward "Synonyms for") (error "No \"Synonyms for\" text"))
    (end-of-line)
    (insert (make-string (- fill-column 16 (point)) ?\ ) "[")
    (let ((beg      (point))
          (Back     "Back")
          (spacer   "]  [")
          (Forward  "Forward"))
      (insert Back)
      (add-text-properties beg (point)
                           '(face synonyms-link mouse-face synonyms-mouse-face back-link t
                             help-echo "mouse-2, RET: Go backward in synonyms search history"))
      (insert spacer Forward)
      (add-text-properties (+ beg (length Back) (length spacer)) (point)
                           '(face synonyms-link mouse-face synonyms-mouse-face forward-link t
                             help-echo "mouse-2, RET: Go forward in synonyms search history"))
      (insert "]"))))

(defun synonyms-show-synonyms (temp-buf results-buf appendp)
  "Display search results from buffer TEMP-BUF in buffer RESULTS-BUF.
If APPEND-P is non-nil and RESULTS-BUF is not empty, then insert a
separator line between previous search results and the current results."
  (set-buffer results-buf)
  (setq buffer-read-only  nil)
  (unless (= (point-min) (point-max))
    (if (not appendp)
        (erase-buffer)
      (goto-char (point-max))
      (let ((beg  (point)))
        (insert "\n" (make-string (1- (window-width)) ?_) "\n\n\n")
        (add-text-properties beg (point) '(face synonyms-heading)))))
  (let ((start-result  (point)))
    (insert-buffer temp-buf)
    (select-window (display-buffer results-buf))
    (goto-char start-result)
    (forward-line 2)                    ; Put cursor on first synonym.
    (when (looking-at "^[0-9]. ") (goto-char (match-end 0)))
    (recenter 2)
    (synonyms-mode)
    (setq buffer-read-only  t)))

;;;###autoload
(defun synonyms-history-backward (arg)
  "Run `synonyms' on a previous argument, moving backward in the history.
A prefix argument has the same meaning as for command `synonyms'."
  (interactive "P")
  (unless (cdr synonyms-history) (error "Cannot move backward in history"))
  (push (pop synonyms-history) synonyms-history-forward) ; Put current on forward list.
  (let* ((num-arg  (prefix-numeric-value arg))
         (morep    (eq synonyms-match-more-flag (atom arg)))
         (appendp  (eq synonyms-append-result-flag (and (wholenump num-arg) (/= 16 num-arg)))))
    
    ;; Visit last.  If *Synonyms* has appended search results, go to the previous one, from (point).
    (if (not (get-buffer "*Synonyms*"))
        (synonyms-action (car synonyms-history))
      (let ((divider  (with-current-buffer "*Synonyms*" (re-search-backward "^___" nil t))))
        (if (not divider)
            (synonyms-action (car synonyms-history))
          (set-buffer "*Synonyms*")
          (goto-char divider)
          (unless (re-search-backward "^Synonyms for \\([^:]+\\):" nil t)
            (error "Cannot find previous synonyms page"))
          (goto-char (match-beginning 1))
          (recenter 0)
          (message "%s" (buffer-substring (match-beginning 1) (match-end 1))))))))

;;;###autoload
(defun synonyms-history-forward (arg)
  "Run `synonyms' on a previous argument, moving forward in the history.
A prefix argument has the same meaning as for command `synonyms'."
  (interactive "P")
  (unless synonyms-history-forward (error "Cannot move forward in history"))
  (push (pop synonyms-history-forward) synonyms-history) ; Put current on backward list.
  (let* ((num-arg  (prefix-numeric-value arg))
         (morep    (eq synonyms-match-more-flag (atom arg)))
         (appendp  (eq synonyms-append-result-flag (and (wholenump num-arg) (/= 16 num-arg)))))

    ;; Visit current.  If *Synonyms* has appended search results, go to the next one, from (point).
    (if (not (get-buffer "*Synonyms*"))
        (synonyms-action (car synonyms-history))
      (let ((divider  (with-current-buffer "*Synonyms*" (re-search-forward "^___" nil t))))
        (if (not divider)
            (synonyms-action (car synonyms-history))
          (set-buffer "*Synonyms*")
          (goto-char divider)
          (unless (re-search-forward "^Synonyms for \\([^:]+\\):" nil t)
            (error "Cannot find next synonyms page"))
          (goto-char (match-beginning 1))
          (recenter 0)
          (message "%s" (buffer-substring (match-beginning 1) (match-end 1))))))))

;;;###autoload
(defalias 'dictionary-definition 'synonyms-definition)
;;;###autoload
(defun synonyms-definition (search-text alternate-p)
  "Look up the definition of a word or phrase using online dictionaries.
The dictionary used is `synonyms-dictionary-url'.
With prefix arg, look up the definition in the alternate dictionary,
`synonyms-dictionary-alternate-url'."
  (interactive (list (completing-read "Look up definition of word or phrase (regexp): "
                                      synonyms-obarray nil nil nil 'synonyms-history
                                      (synonyms-default-regexp))
                     current-prefix-arg))
  (synonyms-ensure-synonyms-read-from-cache) ; Fill `synonyms-obarray', for use in completion.
  (browse-url (concat  (if alternate-p synonyms-dictionary-alternate-url synonyms-dictionary-url)
                       search-text)))

;;;###autoload
(defun synonyms-definition-no-read (alternate-p)
  "Look up the definition of a word or phrase using online dictionaries.
The dictionary used is `synonyms-dictionary-url'.
With prefix arg, look up the definition in the alternate dictionary,
`synonyms-dictionary-alternate-url'."
  (interactive "P")
  (synonyms-definition (synonyms-default-regexp) alternate-p))

;;;###autoload
(defun synonyms-definition-mouse (event alternate-p)
  "Look up the definition of a word or phrase using online dictionaries.
The dictionary used is `synonyms-dictionary-url'.
With prefix arg, look up the definition in the alternate dictionary,
`synonyms-dictionary-alternate-url'."
  (interactive "e\nP")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (let ((beg     (region-beginning))
        (end     (region-end))
        (active  mark-active))
    (goto-char (posn-point (event-end event)))
    (cond ((get-text-property (point) 'back-link) (synonyms-history-backward nil))
          ((get-text-property (point) 'forward-link) (synonyms-history-forward nil))
          (t (if (and active (> (point) beg) (< (point) end))
                 (goto-char end)
               (deactivate-mark))       ; User did not click inside region, so deactivate it.
             (synonyms-definition (synonyms-default-regexp) alternate-p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'synonyms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; synonyms.el ends here
