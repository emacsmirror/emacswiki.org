;;; isearch+.el --- Extensions to `isearch.el' (incremental search).
;;
;; Filename: isearch+.el
;; Description: Extensions to `isearch.el' (incremental search).
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2015, Drew Adams, all rights reserved.
;; Created: Fri Dec 15 10:44:14 1995
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Fri Mar 27 14:48:14 2015 (-0700)
;;           By: dradams
;;     Update #: 3469
;; URL: http://www.emacswiki.org/isearch+.el
;; Doc URL: http://www.emacswiki.org/IsearchPlus
;; Keywords: help, matching, internal, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `frame-fns', `misc-cmds', `misc-fns', `strings',
;;   `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `isearch.el' (incremental search).
;;
;;  The Isearch+ libraries are these:
;;
;;  `isearch+.el' (this file)    - Various extensions to `isearch.el'.
;;                                 Can be used with Emacs 20 or later.
;;  `isearch-prop.el' (optional) - Commands to search within contexts,
;;                                 which are character-property zones:
;;                                 spans of text that have certain
;;                                 text or overlay properties.  Can be
;;                                 Used with Emacs 23 or later.
;;
;;  You can use either of the Isearch+ files without the other, but I
;;  recommend that you use them together.
;;
;;
;;  This file should be loaded *AFTER* loading the standard GNU file
;;  `isearch.el'.  So in your `~/.emacs' file, do this:
;;
;;  (eval-after-load "isearch" '(require 'isearch+))
;;
;;  Library `isearch-prop.el' is optional.  If you do not want to use
;;  it then do not put it in your `load-path'.  If it is in your
;;  `load-path' then it will automatically be loaded when you load
;;  library `isearch+.el'.
;;
;;  More description below - see Overview of Features.
;;
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Overview of Features")
;;  (@> "Change log")
;;  (@> "Faces and Variables")
;;  (@> "Keys and Hooks")
;;  (@> "Macros")
;;  (@> "Commands")
;;  (@> "Non-Interactive Functions")
;;
;;
;;  Commands defined here:
;;
;;    `isearchp-act-on-demand' (Emacs 22+),
;;    `isearchp-append-register', `isearch-char-by-name' (Emacs
;;    23-24.3), `isearchp-cycle-mismatch-removal',
;;    `isearchp-eval-sexp-and-insert' (Emacs 22+),
;;    `isearchp-fontify-buffer-now', `isearchp-init-edit',
;;    `isearchp-open-recursive-edit' (Emacs 22+),
;;    `isearchp-remove-failed-part' (Emacs 22+),
;;    `isearchp-retrieve-last-quit-search',
;;    `isearchp-set-region-around-search-target',
;;    `isearchp-toggle-literal-replacement' (Emacs 22+),
;;    `isearchp-toggle-option-toggle',
;;    `isearchp-toggle-regexp-quote-yank',
;;    `isearchp-toggle-search-invisible',
;;    `isearchp-toggle-set-region', `isearchp-yank-char' (Emacs 22+),
;;    `isearchp-yank-line' (Emacs 22+),
;;    `isearchp-yank-sexp-symbol-or-char' (Emacs 22+),
;;    `isearchp-yank-sexp-symbol-or-char-1' (Emacs 22+),
;;    `isearchp-yank-symbol-or-char' (Emacs 22+),
;;    `isearchp-yank-symbol-or-char-1' (Emacs 22+),
;;    `isearchp-yank-word-or-char' (Emacs 22+).
;;
;;  User options defined here:
;;
;;    `isearchp-case-fold', `isearchp-deactivate-region-flag' (Emacs
;;    24.3+), `isearchp-drop-mismatch',
;;    `isearchp-initiate-edit-commands' (Emacs 22+),
;;    `isearchp-mouse-2-flag', `isearchp-on-demand-action-function'
;;    (Emacs 22+), `isearchp-regexp-quote-yank-flag',
;;    `isearchp-restrict-to-region-flag' (Emacs 24.3+),
;;    `isearchp-set-region-flag', `isearchp-toggle-option-flag'.
;;
;;  Faces defined here:
;;
;;    `isearch-fail'.
;;
;;  Macros defined here:
;;
;;    `isearchp-user-error'.
;;
;;  Non-interactive functions defined here:
;;
;;    `isearchp-barf-if-use-minibuffer', `isearchp-fail-pos',
;;    `isearchp-highlight-lighter', `isearchp-message-prefix',
;;    `isearchp-message-suffix', `isearchp-read-face-names',
;;    `isearchp-read-face-names--read', `isearchp-read-sexps',
;;    `isearchp-remove-duplicates', `isearchp-remove-mismatch',
;;    `isearchp-repeat-command', `isearchp-replace-fixed-case-p'
;;    (Emacs 22+), `isearchp-replace-match' (Emacs 22+),
;;    `isearchp-replace-multiple' (Emacs 22+),
;;    `isearchp-replace-on-demand' (Emacs 22+),
;;    `isearchp-reset-noprompt-action-fn', `isearchp-set-region',
;;    `isearchp-set-sel-and-yank',
;;    `isearchp-update-edit-init-commands' (Emacs 22+).
;;
;;  Internal variables defined here:
;;
;;    `isearchp-last-non-nil-invisible',
;;    `isearchp-last-quit-regexp-search', `isearchp-last-quit-search',
;;    `isearchp-nomodify-action-hook' (Emacs 22+),
;;    `isearchp-noprompt-action-function', `isearchp-pref-arg',
;;    `isearchp-reg-beg', `isearchp-reg-end',
;;    `isearchp-replace-literally' (Emacs 22+), `isearchp-replacement'
;;    (Emacs 22+), `isearchp-win-pt-line', `isearch-update-post-hook'
;;    (Emacs 20-21).
;;
;;
;;  ***** NOTE: The following macros and functions defined in
;;              `isearch.el' have been REDEFINED OR ADVISED HERE:
;;
;;  `isearch-abort'       - Save search string when `C-g'.
;;  `isearch-cancel'      - Restore cursor position relative to window.
;;  `isearch-dehighlight' - Remove unused arg, for Emacs 20.
;;  `isearch-edit-string' - Put point at mismatch position.
;;  `isearch-lazy-highlight-search' - Can limit to region (24.3+)
;;  `isearch-lazy-highlight-update' - Can limit to region (24.3+)
;;  `isearch-mode'        - Save cursor position relative to window.
;;  `isearch-mode-help'   - End isearch.  List bindings.
;;  `isearch-message'     - Highlight failed part of search string in
;;                          echo area, in face `isearch-fail'.
;;  `isearch-message-prefix' - Highlight prompt keywords:
;;                             wrapped, regexp, word, multi
;;  `isearch-mouse-2'     - Respect `isearchp-mouse-2-flag'(Emacs 21+)
;;  `isearch-search'      - Can limit to active region (Emacs 24.3+)
;;  `isearch-repeat'      - Can limit to active region (Emacs 24.3+)
;;  `isearch-printing-char' - Respect option `isearchp-drop-mismatch'
;;  `isearch-toggle-case-fold' - Respect `isearchp-toggle-option-flag'
;;                               Show case sensitivity in mode-line.
;;                               Message.
;;  `isearch-toggle-invisible' - Respect `isearchp-toggle-option-flag'
;;                               Message.
;;  `isearch-toggle-word' - Message, and turn off regexp search.
;;  `isearch-update' - Run `isearch-update-post-hook' (Emacs 20-21).
;;                   - Run `isearchp-noprompt-action-function' and
;;                     `isearchp-nomodify-action-hook' (Emacs 22+).
;;  `isearch-yank-string' - Respect `isearchp-regexp-quote-yank-flag'.
;;  `with-isearch-suspended' - Add `catch': update `isearch-success'.
;;
;;
;;  ***** NOTE: The following internal variables defined in
;;              `isearch.el' have been REDEFINED HERE:
;;
;;  `isearch-invisible'   - defined for Emacs<24.4 & added doc string.
;;
;;
;;  Keys bound in `isearch-mode-map' here:
;;
;;    `C-`'        `isearchp-toggle-regexp-quote-yank'
;;    `C-+'        `isearchp-toggle-search-invisible'
;;    `C-_'        `isearchp-yank-symbol-or-char' (Emacs 22+)
;;    `C-('        `isearchp-yank-sexp-symbol-or-char' (Emacs 22+)
;;    `C-SPC'      `isearchp-toggle-set-region'
;;    `C-end'      `goto-longest-line' (requires `misc-cmds.el')
;;    `C-h'        `isearch-mode-help'
;;    `C-x n'      `isearchp-toggle-region-restriction' (Emacs 24.3+)
;;    `C-x o'      `isearchp-open-recursive-edit' (Emacs 22+)
;;    `C-x r g'    `isearchp-append-register'
;;    `C-x 8 RET'  `isearch-char-by-name' (Emacs 23-24.3)
;;    `C-y C-_'    `isearchp-yank-symbol-or-char' (Emacs 22+)
;;    `C-y C-('    `isearchp-yank-sexp-symbol-or-char' (Emacs 22+)
;;    `C-y C-2'    `isearch-yank-secondary'
;;    `C-y C-c'    `isearchp-yank-char' (Emacs 22+)
;;    `C-y C-e'    `isearchp-yank-line'
;;    `C-y C-w'    `isearchp-yank-word-or-char' (Emacs 22+)
;;    `C-y C-y'    `isearch-yank-kill'
;;    `C-y M-y'    `isearch-yank-pop' (Emacs 24+)
;;    `C-z'        `isearchp-yank-char' (Emacs 22+)
;;    `M-c'        `isearch-toggle-case-fold'
;;    `M-e'        `isearch-edit-string'
;;    `M-g'        `isearchp-retrieve-last-quit-search'
;;    `M-k'        `isearchp-cycle-mismatch-removal'
;;    `M-r'        `isearch-toggle-regexp'
;;    `M-s i'      `isearch-toggle-invisible'
;;    `M-s w'      `isearch-toggle-word'
;;    `M-w'        `isearchp-kill-ring-save'
;;    `C-M-l'      `isearchp-remove-failed-part' (Emacs 22+)
;;    `C-M-y'      `isearch-yank-secondary'
;;    `C-M-RET'    `isearchp-act-on-demand' (Emacs 22+)
;;    `C-M-tab'    `isearch-complete' (on MS Windows)
;;    `next'       `isearch-repeat-forward'
;;    `prior'      `isearch-repeat-backward'
;;
;;
;;  User option `isearchp-initiate-edit-commands' causes certain keys
;;  not to exit Isearch but rather to edit the search string.
;;  Customize it to `nil' if you do not want this behavior at all.
;;
;;
;;  The following bindings are made here for incremental search edit
;;  mode:
;;
;;    `C-x 8 RET'  `insert-char' (Emacs 23+)
;;    `C-M-tab'    `isearch-complete-edit' (MS Windows only)
 
;;(@* "Overview of Features")
;;
;;; Overview of Features ---------------------------------------------
;;
;;  * Case-sensitivity is indicated in the mode line minor-mode
;;    lighter: `ISEARCH' for case-insensitive; `Isearch' for
;;    case-sensitive.
;;
;;  * Highlighting of the mode-line minor-mode lighter when search has
;;    wrapped around (Emacs 24+ only).
;;
;;  * Highlighting of parts of the prompt, to indicate the type of
;;    search: regexp, word, multiple-buffer, and whether searching has
;;    wrapped around the buffer (Emacs 22+ only).
;;
;;  * Optional limiting of search to the active region, controlled by
;;    option `isearchp-restrict-to-region-flag'.  Deactivation of the
;;    active region, controlled by option
;;    `isearchp-deactivate-region-flag'.  Both of these are available
;;    for Emacs 24.3 and later.  You can use `C-x n' (command
;;    `isearchp-toggle-region-restriction') during search to toggle
;;    `isearchp-restrict-to-region-flag'.
;;
;;    NOTE: For search to be limited to the active region in Info, you
;;    must also use library `info+.el'.
;;
;;  * Option and commands to let you select the last target occurrence
;;    (set the region around it):
;;
;;    - Option `isearchp-set-region-flag' - Non-`nil' means
;;      automatically set the region around the last search target.
;;    - Command `isearchp-toggle-set-region', bound to `C-SPC' during
;;      isearch - toggle `isearchp-set-region-flag'.
;;    - Command `isearchp-set-region-around-search-target' - manually
;;      set the region around the last search target.
;;
;;  * When you visit a search hit, you can perform an action on it.
;;    Use `C-M-RET' (command `isearchp-act-on-demand' - Emacs 22+
;;    only) to invoke the action function that is the value of option
;;    `isearchp-on-demand-action-function'.  That function is passed
;;    the current search-hit string and its start and end positions in
;;    the buffer.  Search moves to the next hit in the same search
;;    direction, so just repeating `C-M-RET' carries out the action on
;;    subsequent hits.
;;
;;  * The default value of `isearchp-on-demand-action-function' is
;;    function `isearchp-replace-on-demand', which replaces the search
;;    hit.  This means that you can replace (or delete) chosen search
;;    hits on demand.
;;
;;    By default, the replacement string is empty, so with no prefix
;;    argument the action is to delete the search hit (replace it with
;;    nothing).
;;
;;    With a prefix arg, `isearchp-replace-on-demand' prompts for the
;;    replacement, which is used thereafter until you again use a
;;    prefix arg.  Since you can use a prefix arg at any time, you can
;;    provide different replacements for different search hits.  When
;;    prompted, if you clear the minibuffer and hit `RET', hit
;;    replacement just becomes search-hit deletion.
;;
;;    . With a plain prefix arg (`C-u') or a numeric prefix arg of
;;      value 1 (e.g. `C-1'), `isearchp-replace-on-demand' replaces
;;      only the current search hit.
;;
;;    . With a negative prefix arg (e.g. `M--'),
;;      `isearchp-replace-on-demand' changes searching so that it also
;;      replaces.  That is, the search key (e.g., `C-s') then acts the
;;      same as `C-M-RET'.  (You can cancel this by using a
;;      non-negative prefix arg or by quitting and restarting
;;      Isearch.)
;;
;;    . With a positive prefix arg N (e.g. `C-8' or `C-u 200'),
;;      `isearchp-replace-on-demand' replaces N search hits (but it
;;      stops at the search limit, if reached).
;;
;;    . With a zero prefix arg (e.g. `C-0),
;;      `isearchp-replace-on-demand' replaces *all* remaining search
;;      hits (up to the search limit).
;;
;;    (NOTE: To use a prefix arg within Isearch, you must set
;;    `isearch-allow-prefix' (if available) or `isearch-allow-scroll'
;;    to non-`nil'.)
;;
;;  * When you use on-demand replacement (with `C-M-RET') the
;;    replacement text can be either inserted literally, as is, or
;;    interpreted as in `query-replace-regexp'.  In the latter case,
;;    you can use `\&', `\=\N', `\#', `\,' and `\?'.
;;
;;    For example, suppose you use a regexp-search pattern of
;;    `\(e\)\|a' and a replacement pattern of `\,(if \1 "a" "e")'.
;;    Each `C-M-RET' will then swap `e' for `a' and vice versa.
;;
;;    See the doc for `query-replace-regexp' and node `Regexp Replace'
;;    of the Emacs manual for more information.
;;
;;    (Note that `\?' is supported, but it is not very useful in this
;;    context, because it prompts you to edit the result each time you
;;    hit `C-M-RET'.  Instead, use `C-u C-M-RET' whenever you want to
;;    change (edit) the replacement pattern.)
;;
;;  * You can use `C-M-`' (`isearchp-toggle-literal-replacement')
;;    anytime during Isearch to toggle whether replacement text is
;;    used literally or interpreted per the special regexp-replacement
;;    constructs.
;;
;;    Note that the use of the special regexp replacement patterns is
;;    unrelated to the kind of incremental search: literal string
;;    search or regexp search.  Just remember that the way to switch
;;    on/off the special behavior of `\&' and so on is to use `C-M-`'.
;;
;;  * The value of variable `isearchp-noprompt-action-function' is a
;;    function that is invoked automatically, after you visit each
;;    search hit.  The function is called with no arguments.  It
;;    cannot use the minibuffer, but it can modify buffer contents.
;;    The variable is reset to `nil' when you quit Isearch.  As an
;;    example of use, command `isearchp-replace-on-demand' with a
;;    negative prefix arg sets this to `isearchp-replace-match', which
;;    causes automatic replacement each time you visit a search hit.
;;
;;  * Hook `isearchp-nomodify-action-hook' (Emacs 22+ only) is also
;;    run after each search visit.  Its functions also must accept the
;;    same arguments as `isearchp-act-on-demand'.  The functions can
;;    use the minibuffer, but they must not update the buffer text (in
;;    a way noticeable by Isearch), or else that will likely lead to a
;;    call-stack overflow.  This is because they are called with
;;    Isearch suspended during `isearch-update' (which can itself be
;;    invoked by the action...).
;;
;;  * Option (`isearchp-regexp-quote-yank-flag') and command
;;    (`isearchp-toggle-regexp-quote-yank', bound to `C-`') to toggle
;;    quoting (escaping) of regexp special characters.  With escaping
;;    turned off, you can yank text such as `^\*.*' without it being
;;    transformed to `\^\\\*\.\*'.

;;  * `M-:' (`isearchp-eval-sexp-and-insert') prompts you for a Lisp
;;    sexp, evaluates it, and appends the value to the search string.
;;    This is useful, for example, to use `rx' or another
;;    regexp-creation helper to create a regexp search pattern.
;;
;;    For example: `C-M-s M-: (rx (and line-start (1+ (in "("))))'
;;    searches using the result of that `rx' sexp, which is "^(+".
;;    (The double-quote chars are removed.)
;;
;;    Remember too that you can use `C-u M-:' after `M-e'.  That
;;    inserts the sexp value into the minibuffer, where you are
;;    editing the search string.  Use this when you do not want to
;;    simply append the sexp value to the search string, but instead
;;    you want to do some editing of it or the rest of the search
;;    string.
;;
;;  * `M-g' (`isearchp-retrieve-last-quit-search') yanks the last
;;    successful search string (regexp or plain) from when you last
;;    hit `C-g' in Isearch.  Sometimes you search for something but
;;    abandon the search - you just want to check the locations of
;;    something, without staying at any of them.  Afterward, if you
;;    want to find them again, use `M-g'.  This yanks that search
;;    string, so you can append it to whatever you are already
;;    searching for.
;;
;;  * `C-x r g' (`isearchp-append-register') appends the contents of a
;;    register to the search string.  You are prompted for the
;;    register to use.  This is the same key that is bound globally to
;;    `insert-register'.  If you want this key to instead exit Isearch
;;    and insert the register in the buffer, then define this key in
;;    `isearch-mode-map' as `nil' (i.e., unbind it), and optionally
;;    bind `isearchp-append-register' to a different key in
;;    `isearch-mode-map'.
;;
;;  * `C-M-l' (`isearchp-remove-failed-part') removes the failed part
;;     of the search string, if any.
;;
;;  * `C-M-y' (`isearch-yank-secondary') yanks the secondary selection
;;    into the search string, if you also use library `second-sel.el'.
;;
;;  * `C-z' (`isearchp-yank-char') yanks successive characters onto
;;    the search string.  This command is also bound to `C-y C-c'.
;;
;;  * `C-_' (`isearchp-yank-symbol-or-char') yanks successive symbols
;;    (or words or subwords or chars) into the search string.
;;
;;  * `C-(' (`isearchp-yank-sexp-symbol-or-char') yanks successive
;;    sexps (or symbols or words or subwords or chars) into the search
;;    string.
;;
;;  * `M-w' (`isearchp-kill-ring-save') copies the current search
;;    string to the kill ring.  You can then, for example, use `C-s
;;    M-y' to search for the same thing in another Emacs session.
;;
;;    (I use this all the time, but you might not use multiple Emacs
;;    sessions.)  Note that if you did not have this feature then you
;;    would need to select the search-string text (in the text buffer
;;    or in the `M-e' Isearch edit buffer) and copy it to the kill
;;    ring. (Note: `M-w' used to toggle word search, but
;;    `isearch-toggle-word' is now `M-s w'.)
;;
;;  * All commands that yank text onto the search string are bound to
;;    keys with prefix `C-y' (in addition to any other Isearch
;;    bindings):
;;
;;      `C-y C-_'   isearchp-yank-symbol-or-char
;;      `C-y C-('   isearchp-yank-sexp-symbol-or-char
;;      `C-y C-2'   isearch-yank-secondary
;;      `C-y C-c'   isearchp-yank-char
;;      `C-y C-e'   isearchp-yank-line
;;      `C-y C-w'   isearchp-yank-word-or-char
;;      `C-y C-y'   isearch-yank-kill
;;      `C-y M-y'   isearch-yank-pop
;;
;;    You can repeat any of these for which it makes sense (i.e., all
;;    except `isearch-yank-secondary', `isearch-yank-kill', and
;;    `isearch-yank-pop') by just repeating the last key.  For
;;    example: `C-y C-e C-e C-e' adds the text up to the end of three
;;    lines.
;;
;;  * `C-x 8 RET' (`isearch-char-by-name') reads the name of a Unicode
;;    character with completion and appends it to the search string.
;;    Same thing when editing the search string (i.e., after `M-e').
;;    This is part of GNU Emacs starting with Emacs 24.4.
;;
;;  * `C-x o' (`isearchp-open-recursive-edit') opens a recursive
;;    editing session, where you can do anything you like (including
;;    search for something different).  Using `C-M-c' closes the
;;    recursive editing session and resumes the search (from the
;;    current position where you hit `C-M-c').
;;
;;  * Highlighting of the mismatched portion of your search string in
;;    the minibuffer.  This is the portion that is removed if you do
;;    `C-g', or removed/replaced automatically if you use `M-k' (see
;;    next).  I added this feature to GNU Emacs 23.1.
;;
;;  * `C-g' after successfully finding matches restores not only the
;;    original position but also its relative position in the window.
;;    IOW, you get back to what you saw before searching.  Fixes Emacs
;;    bug #12253 for Isearch.
;;
;;  * `M-k' (`isearchp-cycle-mismatch-removal') cycles automatic
;;    removal or replacement of the input portion that does not match,
;;    bound to .  The behavior is controlled by the value of option
;;    `isearchp-drop-mismatch':
;;
;;    `replace-last' - Your current input replaces the last mismatched
;;                     text.  You can always see your last input, even
;;                     if it is a mismatch.  And it is available for
;;                     editing using `M-e'.
;;    `nil'          - Your current input is appended, even if the
;;                     previous input has a mismatched portion.
;;    anything else  - Your current input is ignored (removed) if it
;;                     causes a mismatch.  The search string always
;;                     has successful matches.
;;
;;  * Non-`nil' option `isearchp-toggle-option-flag', which you can
;;    toggle using `M-s v' (`isearchp-toggle-option-toggle'),
;;    determines whether commands that toggle behavior also toggle an
;;    associated user option.  For such commands, a prefix argument
;;    flips the behavior, as if `isearchp-toggle-option-flag' were
;;    toggled temporarily.  Currently this feature applies to toggles
;;    `M-c' (case-sensitivity) and `M-s i' (matching hidden text).
;;
;;  * `M-c' (`isearch-toggle-case-fold') toggles case sensitivity.  If
;;    option `isearchp-toggle-option-flag' is non-`nil' then it
;;    toggles option `isearchp-case-fold' to change the sensitivity
;;    from now on.  Otherwise, the option value is not changed, so the
;;    effect is for the current search only.
;;
;;  * `M-s i' (`isearch-toggle-invisible') toggles invisible-text
;;    sensitivity.  If option `isearchp-toggle-option-flag' is
;;    non-`nil' then it toggles option `search-invisible' to change
;;    the sensitivity from now on.  Otherwise, the option value is not
;;    changed, so the effect is for the current search only.
;;
;;  * `C-+' (`isearchp-toggle-search-invisible') toggles the value of
;;    option `search-invisible'.  The effect is like that of `M-s i'
;;    with no prefix argument and with non-`nil'
;;    `isearchp-toggle-option-flag'.
;;
;;  * Other bindings during Isearch:
;;
;;    - `next', `prior' repeat the last Isearch forward and backward
;;      (easier than using the chords `C-s', `C-r').
;;    - `C-end' - go to the longest line.  Repeat to go to the longest
;;      line following that one in the buffer.  As usual, `C-g' puts
;;      you back where you started.  This binding is made only if you
;;      also use `misc-cmds.el'.
;;    - `C-h' provides help on Isearch while searsching.  This library
;;      also redefines `isearch-mode-help' so that it lists all
;;      Isearch bindings and ends Isearch properly.
;;
;;  * `M-e' (`isearch-edit-string') automatically puts the cursor at
;;    the first mismatch position in the search string, for easy
;;    editing.  Whereas `C-g' (see also `M-k') removes all of the
;;    mismatch, this feature lets you change or insert a character or
;;    two, without losing the rest of the search string.
;;
;;  * A user option, `isearchp-initiate-edit-commands', that specifies
;;    commands whose keys will not exit Isearch but will instead
;;    initiate editing of the search string.  For example, if
;;    `backward-char' is included in the list then `C-b' and `left'
;;    will just move the cursor backward over the search string so you
;;    can change, delete, or insert chars in the middle somewhere.
;;    This makes the search string more minibuffer-like.
;;
;;  * You can, by default, select text with the mouse, then hit `C-s'
;;    etc. to search for it.  This is controlled by user option
;;    `isearchp-mouse-2-flag'.
;;
;;  If you have Emacs 23 or later then I recommend that you also use
;;  the companion library, `isearch-prop.el'.  If it is in your
;;  `load-path' then it will be loaded by `isearch+.el'.  It lets you
;;  limit incremental searching to contexts that you define.
;;
;;  Example: search within zones having a `face' text property with a
;;  value of `font-lock-comment-face' or `font-lock-string-face'.
;;  Search overlays or text properties.
;;
;;  Besides relying on existing text properties such as `face' for
;;  contexts to search, you can use command
;;  `isearchp-put-prop-on-region' to add any text property to the
;;  region.  This gives you an easy way to set up contexts for
;;  text-property search.  For property `face', empty input to
;;  `isearchp-put-prop-on-region' removes all faces from the region.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;(@* "Change log")
;;
;; 2015/03/27 dadams
;;     Added: isearchp-remove-failed-part.  Bound to C-M-l.
;; 2015/02/23 dadams
;;     Added: isearchp-eval-sexp-and-insert.  Bound to M-:.
;; 2014/10/08 dadams
;;     Added: isearchp-append-register.  Bound to C-x r g.
;; 2014/09/03 dadams
;;     Changed C-c binding for isearchp-yank-char to C-z.
;; 2014/09/02 dadams
;;     isearchp-replace-match: Temporary (?) fix for braindead Emacs 24.4 regression (bug #18388).
;; 2014/04/15 dadams
;;     isearch-printing-char: Update version test for Emacs 24.4 pretest - use version<.
;; 2014/01/10 dadams
;;     isearch-mouse-2: Do not call isearchp-set-sel-and-yank unless mark is defined.
;;     isearchp-set-sel-and-yank: No-op unless mark is defined.
;; 2013/12/16 dadams
;;     isearch-mode: Update for vanilla Emacs 24: set isearch--saved-overriding-local-map.  (Bug#16035)
;; 2013/10/16 dadams
;;     with-isearch-suspended: Do not (goto-char old-other-end) after BODY if we just replaced text.
;; 2013/10/15 dadams
;;     Added: isearchp-toggle-literal-replacement, isearchp-user-error, isearchp-replace-fixed-case-p,
;;            isearchp-replace-match, isearchp-pref-arg, isearchp-replace-literally.
;;     Removed: isearchp-replace-string.
;;     isearchp-act-on-demand: Bind isearchp-pref-arg to prefix arg.
;;                             Call isearchp-on-demand-action-function with no args.
;;     isearch-update: Call isearchp-nomodify-action-hook and isearchp-noprompt-action-function with no args.
;;     isearchp-replace-on-demand:
;;       Removed the args - none now.  Use (match-string 0) instead of STRING arg.
;;       Use isearchp-pref-arg, not current-prefix-arg.  Use isearchp-replace-match, not *-replace-string.
;;       Reset replace-count where applicable.  Set this-command to isearchp-act-on-demand at end.
;;     isearchp-replace-multiple:
;;       Use replace-count, not COUNT.  Use isearchp-replace-match instead of deleting and inserting.
;;     Bind isearchp-toggle-literal-replacement to C-M-`.
;; 2013/10/11 dadams
;;     Added: isearchp-replace-multiple.
;;     isearch-mode: Add hooks: isearch-(pre|post)-command-hook, for new Emacs 24.4 snapshot.
;;     with-isearch-suspended: It is for all versions now.  Added catch.  Put first in file.
;;     isearchp-replace-on-demand: Plain C-u: replace only current. N>0: replace N. N=0: replace all.
;;     isearch-forward: Updated doc string.
;; 2013/10/06 dadams
;;     Define isearch-update-post-hook only if not already defined.
;;     isearchp-update-post-hook -> isearch-update-post-hook.
;;     isearchp-replace-on-demand: Use <, not <= (typo).
;; 2013/10/03 dadams
;;     Added: isearchp-nomodify-action-hook, isearchp-noprompt-action-function,
;;            isearchp-on-demand-action-function, isearchp-act-on-demand, isearchp-replace-on-demand,
;;            isearchp-replacement, isearchp-replace-string, isearchp-update-post-hook (renamed from
;;            isearch-*), isearchp-barf-if-use-minibuffer, isearchp-reset-noprompt-action-fn.
;;     Removed defadvice for isearch-update.
;;     Added redefinition of isearch-update.
;;     Added redefinition of isearch-dehighlight, for Emacs 20.
;;     Bind isearchp-act-on-demand to C-M-return.
;; 2013/09/30 dadams
;;     Do not soft-require isearch-prop.el unless Emacs 23+.
;; 2013/09/12 dadams
;;     isearchp-reg-(beg|end): Changed default value to nil.
;      isearch-mode: save-restriction and widen, to get region limits.
;;     isearch-search, isearch-repeat, isearch-lazy-highlight-search, isearch-lazy-highlight-update:
;;       handle null isearchp-reg-(beg|end) case per vanilla.
;; 2013/09/10 dadams
;;     Added support for limiting search to active region (Emacs 24.3+):
;;       Added: isearchp-deactivate-region-flag, isearchp-restrict-to-region-flag, isearchp-reg-beg,
;;              isearchp-reg-end, isearchp-toggle-region-restriction.
;;       Added redefinitions: isearch-search, isearch-repeat, isearch-lazy-highlight-search,
;;             isearch-lazy-highlight-update.
;;       Bound isearchp-toggle-region-restriction to C-x n.
;;       isearch-mode: Save isearchp-reg-beg|end.  Deactivate region.
;;     isearch-forward: Updated doc string.
;; 2013/09/08 dadams
;;     Moved all character-property code to new library isearch-prop.el.  Soft-require it.
;;       Moved: *-char-prop*, *-put-prop-on-region, *-some, *-filter-predicate-orig.
;; 2013/09/06 dadams
;;     isearchp-put-prop-on-region: Restore buffer-modified-p after adding property.
;;     isearchp-char-prop-1: Show message only when already in isearch-mode.
;; 2013/06/29 dadams
;;     isearchp-drop-mismatch: Removed quote in const.
;; 2013/06/28 dadams
;;     Bind C-x 8 RET even for Emacs 24.4+ (where it is true by default), because we set C-x to nil.
;; 2013/06/27 dadams
;;     Renamed: isearchp-toggle-invisible to isearchp-toggle-search-invisible.
;;     Added: isearchp-toggle-option-flag, isearchp-toggle-option-toggle, isearchp-case-fold,
;;            isearchp-last-non-nil-case-fold, isearch-toggle-invisible (redef).
;;     Added defvar for isearch-invisible, for older Emacs versions and to provide doc string.
;;     isearchp-toggle-invisible:
;;       Respect isearchp-toggle-option-flag.  Set isearch-invisible, isearch-success, isearch-adjusted.
;;     isearch-toggle-case-fold:
;;       Handle like isearch-toggle-invisible: respect isearchp-toggle-option-flag for isearchp-case-fold.
;;     Bind isearch-toggle-invisible to M-s i, as in vanilla Emacs 24.4.
;;     Bind isearchp-toggle-option-toggle to M-s v.
;;     isearch-mode: Updated per Emacs 24 dev version: bind isearch-invisible.
;;     isearch-forward: Updated doc string.
;;     isearch-printing-char: Put back version with no args for Emacs < 24.4.
;; 2013/06/25 dadams
;;     Updated some wrt vanilla isearch.el.
;;       Replaced isearch-insert-char-by-name with isearch-char-by-name (which now has optional args).
;;       Updated isearch-printing-char:  now has optional args.
;;       Removed mention of isearch-nonincremental-exit-minibuffer (obsolete in Emacs 24.4+) in doc strings.
;;       Mention isearch-allow-prefix (new in Emacs 24.4) in doc strings that mention isearch-allow-scroll.
;;       isearchp-char-prop-filter-pred: Allow also for isearch-invisible (new).
;;     isearchp-toggle-invisible: Better message - show current value.
;; 2013/05/31 dadams
;;     Require cl.el at compile time, for case.
;; 2013/05/13 dadams
;;     isearchp-highlight-lighter: Use face isearchp-wrapped only if defined (Emacs 22+). 
;; 2013/04/10 dadams
;;     Define with-isearch-suspended for Emacs 24.3 too.  Apparently it did not make it into the release.
;; 2013/03/30 dadams
;;     Added: isearchp-yank-char, isearchp-yank-word-or-char, isearchp-yank-line, isearchp-repeat-command.
;;     Renamed isearchp-yank(-sexp)-symbol-or-char to isearchp-yank(-sexp)-symbol-or-char-1.
;;     isearchp-yank(-sexp)-symbol-or-char (new): Redefined as repeatable, using *-1 helper.
;;     Bind isearch-toggle-case-fold to M-c, not C-c.  Bind isearchp-yank-char to C-c.
;;     Bind isearchp-yank-(char|line|(word|symbol|sexp-symbol)-or-char) to C-y + control char.
;;     Do not bind vanilla isearch-yank commands to C-y prefix.
;;
;; 2013/03/29 dadams
;;     Added: isearchp-kill-ring-save.  Bind it to M-w, instead of isearch-toggle-word (which is now M-s w).
;;     Bind isearch-toggle-word to M-s w for Emacs < 23.
;;     Renamed: isearchp-with-search-suspended to with-isearch-suspended,
;;              isearchp-insert-char-by-name to isearch-insert-char-by-name.
;;     For Emacs 24.3+, do not define with-isearch-suspended or isearch-insert-char-by-name
;;       (vanilla has same definitions).  Do not duplicate key binding for isearch-insert-char-by-name.
;;     Make C-y a prefix key, and put all yank commands on it:
;;       C-y C-_   isearchp-yank-symbol-or-char
;;       C-y C-(   isearchp-yank-sexp-symbol-or-char
;;       C-y C-2   isearch-yank-secondary
;;       C-y C-c   isearchp-yank-char
;;       C-y C-e   isearchp-yank-line
;;       C-y C-w   isearchp-yank-word-or-char
;;       C-y C-y   isearch-yank-kill
;;       C-y M-y   isearch-yank-pop
;;     Moved key bindings and hooks to the end of the file.
;; 2013/01/28 dadams
;;     Advise isearch-forward to add Isearch+ doc.
;; 2013/01/16 dadams
;;     New feature: C-g restores window position of start.  Fixes Emacs bug #12253.
;;       Added redefinitions of isearch-cancel, isearch-mode.  Added: isearchp-win-pt-line.
;; 2012/12/15 dadams
;;     Added redefinition of isearch-printing-char.
;;     Renamed/replaced: isearchp-toggle-mismatch-removal with isearchp-cycle-mismatch-removal,
;;                       isearchp-mismatch-removal-flag   with isearchp-drop-mismatch.
;;     isearchp-cycle-mismatch-removal, isearchp-drop-mismatch: Handle replace-last case.
;; 2012/12/13 dadams
;;     Advise: isearch-update (Emacs 20-23).
;;     Added: isearchp-toggle-mismatch-removal, isearchp-mismatch-removal-flag,
;;            isearchp-remove-mismatch, isearchp-open-recursive-edit.
;;     Bind isearchp-toggle-mismatch-removal to M-k, isearchp-open-recursive-edit to C-x o.
;;     Put isearchp-highlight-lighter on isearch-update-post-hook for Emacs 20-23 also.
;;     Updated to fit Juri's vanilla Emacs version of macro isearchp-use-new-search-string:
;;       Renamed: isearchp-use-new-search-string: to isearchp-with-search-suspended,
;;                isearchp-read-unicode-char to isearchp-insert-char-by-name.
;;       Updated isearch-edit-string, isearchp-insert-char-by-name to use new macro definition.
;; 2012/12/12 dadams
;;     Bind C-x and C-x 8 to nil in isearch-mode-map, for Emacs 23.
;; 2012/12/09 dadams
;;     Added: Macro isearchp-use-new-search-string (Emacs 22+) - factored from isearch-edit-string.
;;            isearchp-read-unicode-char (Emacs 23+).
;;     Bind C-x 8 RET in isearch-mode-map and minibuffer-local-isearch-map.
;;     isearch-edit-string:
;;       Define using isearchp-use-new-search-string.
;;       Update per Emacs 24: Save/restore *-case-fold-search. Bind history-add-new-input to nil.
;; 2012/10/09 dadams
;;     isearchp-read-face-names: Bind icicle-multi-completing-p to t.
;; 2012/09/30 dadams
;;     Added: isearchp-last-quit(-regexp)-search, isearchp-retrieve-last-quit-search,
;;            redefinition of isearch-abort.
;;     Bound isearchp-retrieve-last-quit-search to M-g.
;; 2012/08/27 dadams
;;     isearch(p)-message-(prefix|suffix): Emacs 24.2 turned out to use the same code as 24.1.
;; 2012/08/12 dadams
;;     isearch-edit-string: isearchp-fail-pos -> (or (isearch-fail-pos) (length isearch-string)).
;; 2012/08/08 dadams
;;     Added: isearchp-message-prefix, isearchp-message-suffix.  Use everywhere in place of vanilla.
;;     isearch-message, isearch-fail-pos:
;;       Redefine only for Emacs 22 & 23.  Use vanilla defs (Emacs 24, but with Emacs 22/23 vars).
;;       Removed isearchp-fail-pos - replaced it with isearch-fail-pos.
;;     isearch-message-prefix: Added redefinition for Emacs 24.2+ (changed arglist).
;; 2012/02/08 dadams
;;     isearchp-remove-duplicates: Redefined to use a hash table.
;; 2012/01/11 dadams
;;     Added isearch-message-prefix (redefinition).
;;     Added faces: isearchp-(wrapped|regexp|word|multi).
;;     isearchp-highlight-lighter: Propertize lighter if wrapped.
;; 2011/12/01 dadams
;;     isearchp-toggle-(invisible|regexp-quote-yank|set-region|case-fold):
;;       Added sit-for after message.
;;     isearchp-toggle-(regexp-quote-yank|set-region): Added isearch-update after message + sit-for.
;;     isearch-toggle-word: Redefine even for Emacs versions that have it, to get message etc.
;;                          Added message and sit-for.
;; 2011/11/14 dadams
;;     Bind switch-frame event to ignore in Isearch.
;;     Added and commented out: isearchp-switch-frame-or-exit.
;; 2011/11/13 dadams
;;     Added: isearchp-set-sel-and-yank.
;;     isearch-mouse-2: Use isearchp-set-sel-and-yank, even for nil case.
;;                      Don't require transient-mark-mode.
;; 2011/11/11 dadams
;;     Added defgroup for isearch-plus.  Added: isearchp-mouse-2-flag.
;;     Added redefinition of isearch-mouse-2 that respects isearchp-mouse-2-flag.
;;       And it works for Windows too and all Emacs versions.
;;     Bind mouse-2 to isearch-mouse-2 and down-mouse-2 to ignore.
;;     isearchp-highlight-lighter: Delete (isearch-mode isearch-mode) also from alist.
;;     isearch-yank-string: Updated wrt Emacs 24 code.
;; 2011/09/25 dadams
;;     Added: isearchp-put-prop-on-region.
;;     isearchp-read-face-names: Added optional args empty-means-none-p, only-one-p.
;;     isearchp-read-sexps: Added optional arg only-one-p.
;; 2011/09/23 dadams
;;     Added (renamed from icicle- versions): isearchp-char-prop-default-match-fn,
;;                                            isearchp-char-prop-matches-p, isearchp-some.
;;     isearchp-char-prop-1: Use isearchp-read-sexps, not icicle-sexp-list.
;;     isearchp-char-prop-filter-pred: Use isearchp-char-prop-matches-p,
;;                                     isearchp-char-prop-default-match-fn, not icicle-*.
;; 2011/09/22 dadams
;;     Added: isearchp-char-prop-(backward|forward)(-regexp), isearchp-fontify-buffer-now,
;;            isearchp-char-prop-(1|end|filter-pred), isearchp-char-properties-in-buffer,
;;            isearchp-read-face-names, isearchp-read-face-names--read, isearchp-read-sexps,
;;            isearchp-remove-duplicates, isearchp-char-prop-prop, isearchp-char-prop-type,
;;            isearchp-char-prop-values, isearchp-filter-predicate-orig.
;;     Renamed: set-region-around-search-target to isearchp-set-region-around-search-target.
;;     Bound isearchp-char-prop-forward(-regexp) to C-t, C-M-t.
;;     Define keys here, instead of on isearch-mode-hook.  So we rely on eval-after-load.
;;     Changed key for isearch-toggle-regexp to same as vanilla Emacs: M-r.
;; 2011/09/12 dadams
;;     isearchp-fail-pos: Replaced isearch-message* with isearch-string*.  Thx to Juri Linkov.
;; 2011/09/08 dadams
;;     Added isearchp-init-edit (from anonymous fn), so can see it in keymap help.
;; 2011/07/07 dadams
;;     Added: isearchp-highlight-lighter, isearch-toggle-case-fold (redefinition).
;;     Put isearchp-highlight-lighter on isearch-update-post-hook.
;; 2011/06/03 dadams
;;     isearchp-initiate-edit-commands: Added left-word.
;; 2011/05/27 dadams
;;     Added: isearchp-initiate-edit-commands, isearchp-update-edit-init-commands.
;; 2011/05/16 dadams
;;     Added: isearchp-fail-pos, redefinition of isearch-edit-string.
;;     Removed: isearchp-goto-success-end (not needed - go there by default now).
;; 2011/01/04 dadams
;;     Added autoload cookies for defcustom and commands.
;; 2010/12/05 dadams
;;     Added: isearchp-toggle-invisible, isearchp-last-non-nil-invisible.
;; 2010/10/18 dadams
;;     isearch-mode-hook: Protect isearchp-goto-success-end with fboundp.
;; 2010/06/23 dadams
;;     Added: isearchp-yank(-sexp)-symbol-or-char.  Bound to C-_, C-(.
;; 2010/04/22 dadams
;;     Added: isearchp-toggle-regexp-quote-yank, isearchp-regexp-quote-yank-flag,
;;            isearch-yank-string (redefinition).
;; 2009/06/09 dadams
;;     Bind isearch-repeat-(forward|backward) to (next|prior) in isearch-mode-map.
;; 2008/11/10 dadams
;;     Added: isearchp-goto-success-end.
;; 2008/05/25 dadams
;;     Don't add C-M-tab to isearch-mode-map if already defined.
;; 2008/05/24 dadams
;;     Don't bind C-j to isearch-edit-string.  Bind M-e to isearch-edit-string (for Emacs 20).
;; 2008/02/28 dadams
;;     isearch-message: Protect from Emacs 21 also.
;; 2008/02/24 dadams
;;     isearch-message:
;;       Juri's fix for M-r (was losing failed text) and C-M-s [a-z] (highlighted only ]).
;; 2008/02/23 dadams
;;     isearch-message:
;;       isearch-fail face: Provide better defaults.
;;       Juri's fix for M-p: Use isearch-message for succ-msg, if diff from first msg of
;;         isearch-cmds (isearch-edit-string sets it).
;; 2007/09/10 dadams
;;     Bound goto-longest-line to C-s C-end.  Added soft require of misc-cmds.el.
;; 2007/09/07 dadams
;;     isearch-message:
;;       regexp-quote succ-msg. put-text-property, not propertize, for trailing whitespace.
;; 2007/07/10 dadams
;;     isearchp-set-region: Do nothing unless transient-mark-mode.
;; 2007/02/02 dadams
;;     isearch-message: Fixed when succ-msg matches whole isearch-message (no highlight).
;; 2007/01/23 dadams
;;     isearch-message: For Emacs 22+ only.
;; 2006/12/12 dadams
;;     Added isearch-toggle-word (from Juri Linkov), and bound it.
;; 2006/10/28 dadams
;;     Added: isearch-fail, isearch-message (redefinition).
;; 2006/07/30 dadams
;;     Added: set-region-around-search-target.
;; 2006/07/29 dadams
;;     Added: isearchp-toggle-set-region,isearchp-set-region(-flag). Thx to Andreas Roehler
;; 2006/01/24 dadams
;;     On MS Windows, bind isearch-complete* to C-tab.
;; 1999/03/17 dadams
;;     Updated to corrspond to Emacs 34.1 version.
;; 1996/04/24 dadams
;;     Added redefinition of isearch-search.  Require cl.el.
;; 1995/12/28 dadams
;;     Changed isearch-edit-string binding.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; case

 ;; Cannot do (require 'isearch), because `isearch.el' does no `provide'.
 ;; We  do not want to do a (load-library "isearch"), because it would not
 ;; allow doing (eval-after-load "isearch" '(progn (require 'isearch+)))

(when (> emacs-major-version 22) (require 'isearch-prop nil t)) ;; (no error if not found)

(require 'misc-cmds nil t) ;; (no error if not found): goto-longest-line


;; Quiet the byte compiler.
(defvar bidi-display-reordering)         ; Emacs 24+, built-in.
(defvar disable-point-adjustment)        ; Built-in, Emacs 22+.
(defvar eval-expression-debug-on-error)  ; In `simple.el', Emacs 22+.
(defvar icicle-WYSIWYG-Completions-flag) ; In `icicles-opt.el'.
(defvar isearch-error)                   ; In `isearch.el'.
(defvar isearch-filter-predicate)        ; In `isearch.el' (Emacs 24+).
(defvar isearchp-initiate-edit-commands) ; Here (Emacs 22+).
(defvar isearch-invalid-regexp)          ; In `isearch.el' (Emacs 20-21).
(defvar isearch-last-case-fold-search)   ; In `isearch.el'.
(defvar isearch-lax-whitespace)          ; In `isearch.el' (Emacs 24.3+).
(defvar isearch-lazy-highlight)          ; In `isearch.el' (Emacs 21+).
(defvar isearch-lazy-highlight-case-fold-search) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-end)      ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-end-limit) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-forward)  ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-last-string) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-lax-whitespace) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-overlays) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-regexp)   ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-regexp-lax-whitespace) ; In `isearch.el' (Emacs 24.3+).
(defvar isearch-lazy-highlight-start)    ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-start-limit) ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-timer)    ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-window)   ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-word)     ; In `isearch.el' (Emacs 24+).
(defvar isearch-lazy-highlight-wrapped)  ; In `isearch.el' (Emacs 24+).
(defvar isearch-message-function)        ; In `isearch.el' (Emacs 24+).
(defvar isearch-message-prefix-add)      ; In `isearch.el'.
(defvar isearch-new-message)             ; In `with-isearch-suspended' (here).
(defvar isearch-new-string)              ; In `with-isearch-suspended' (here).
(defvar isearch-original-minibuffer-message-timeout) ; In `isearch.el'.
(defvar isearch-push-state-function)     ; In `isearch.el'.
(defvar isearch--saved-overriding-local-map) ; In `isearch.el'.
(defvar isearch-start-hscroll)           ; In `isearch.el'.
(defvar isearch-within-brackets)         ; In `isearch.el'.
(defvar isearch-wrap-function)           ; In `isearch.el'.
(defvar isearchp-deactivate-region-flag) ; Here (Emacs 24.3+).
(defvar isearchp-nomodify-action-hook)   ; Here (Emacs 22+).
(defvar isearchp-on-demand-action-function) ; Here (Emacs 22+).
(defvar isearchp-replacement)            ; Here (Emacs 22+).
(defvar isearchp-restrict-to-region-flag) ; Here (Emacs 24.3+).
(defvar last-repeatable-command)         ; In `repeat.el'.
(defvar lazy-highlight-face)             ; In `isearch.el' (Emacs 24+).
(defvar lazy-highlight-interval)         ; In `isearch.el' (Emacs 24+).
(defvar lazy-highlight-max-at-a-time)    ; In `isearch.el' (Emacs 24+).
(defvar minibuffer-message-timeout)      ; In Emacs C code.
(defvar multi-isearch-next-buffer-current-function) ; In `isearch.el'.
(defvar replace-count)                  ; In `replace.el'.
(defvar subword-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Faces and Variables")

;;; Faces and Variables ----------------------------------------------
(defgroup isearch-plus nil
  "Isearch enhancements."
  :prefix "isearchp-" :group 'isearch
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
Isearch+ bug: \
&body=Describe bug here, starting with `emacs -Q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "http://www.emacswiki.org/isearch+.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/IsearchPlus")
  :link '(emacs-commentary-link :tag "Commentary" "isearch+"))

(when (> emacs-major-version 21)        ; Emacs 22+
  (defface isearch-fail
      '((((class color) (min-colors 88) (background dark))
         (:foreground "white" :background "#22225F5F2222")) ; a dark green
        (((class color) (min-colors 88) (background light))
         (:foreground "Black" :background "Plum"))
        (((class color) (min-colors 8)) (:background "red"))
        (((type tty) (class mono)) :inverse-video t)
        (t :background "gray"))
    "*Face for highlighting failed part in Isearch echo-area message."
    :group 'isearch-plus)
  (defface isearchp-wrapped
      '((((class color) (min-colors 88)) (:foreground "DeepPink"))
        (t :underline t))
    "*Face for highlighting wrapped-search indicator in Isearch echo-area message."
    :group 'isearch-plus)
  (defface isearchp-regexp
      '((((class color) (min-colors 8)) (:foreground "Firebrick"))
        (t :underline t))
    "*Face for highlighting regexp-search indicator in Isearch echo-area message."
    :group 'isearch-plus)
  (defface isearchp-word
      '((((class color) (min-colors 8)) (:foreground "DarkGreen"))
        (t :underline t))
    "*Face for highlighting word-search indicator in Isearch echo-area message."
    :group 'isearch-plus)
  (defface isearchp-multi
      '((((class color) (min-colors 8)) (:foreground "DarkViolet"))
        (t :underline t))
    "*Face for highlighting multi-buffer indicator in Isearch echo-area message."
    :group 'isearch-plus))

(defcustom isearchp-case-fold nil
  "*Whether incremental search is case sensitive.
nil   means search is always case sensitive
t     means search is never  case sensitive
`yes' means search case-sensitivity follows option `search-upper-case'"
  :type '(choice
          (const :tag "Case sensitive"                      nil)
          (const :tag "Respect option `search-upper-case'"  t)
          (const :tag "Case insensitive"                    yes))
  :group 'isearch-plus)

;;;###autoload
(defcustom isearchp-drop-mismatch nil
  "*Non-nil means remove or replace a search-string mismatch.
There are three possible values:

`replace-last' - Replace the last mismatch in the search string with
                 the latest input (e.g., replace the last typed char
                 or last yanked text).
nil            - Never remove mismatched text from the search string.
anything else  - Always remove mismatched text from the search string.

* Vanilla Isearch has the behavior of a nil value.

* Non-nil, non-`replace-last' means the search string never contains
  mismatched characters.

* `replace-last' means you see only the latest mismatched input, and
  it is available for editing, using \\<isearch-mode-map>`\\[isearch-edit-string]'.

You can cycle among the three possible values using \
`\\[isearchp-cycle-mismatch-removal]'."
  :type '(choice
          (const :tag "Replace last mismatch"  replace-last)
          (const :tag "Never remove mismatch"  nil)
          (other :tag "Always remove mismatch" t))
  :group 'isearch-plus)

(when (fboundp 'isearch-unread-key-sequence) ; Emacs 22+

  (defun isearchp-init-edit (&rest ignored)
    "Invoke current key sequence, but after calling `isearch-edit-string'."
    (interactive)
    (isearch-unread-key-sequence
     (listify-key-sequence (this-command-keys)))
    (isearch-edit-string))

  (defun isearchp-update-edit-init-commands ()
    "Make `isearchp-initiate-edit-commands' edit the search string."
    (dolist (cmd  isearchp-initiate-edit-commands)
      (substitute-key-definition cmd 'isearchp-init-edit isearch-mode-map (current-global-map))))

  ;; No autoload cookie - need function `isearchp-update-edit-init-commands'.
  (defcustom isearchp-initiate-edit-commands
    '(backward-char                     ; `C-b'
      left-char                         ; `left' (Emacs 24+)
      ;; backward-delete-char                ; `DEL'
      ;; backward-delete-char-untabify       ; `DEL'
      ;; backward-kill-paragraph             ; `C-backspace'
      ;; backward-kill-sentence              ; `C-x DEL'
      ;; backward-kill-sexp                  ; `C-M-backspace'
      ;; backward-kill-word                  ; `M-DEL'
      ;; backward-list                       ; `C-M-p'
      ;; backward-page                       ; `C-x ['
      ;; backward-paragraph                  ; `C-up', `M-{'
      ;; backward-sentence                   ; `M-a'
      backward-sexp                     ; `C-M-b', `C-M-left'
      ;; backward-to-indentation             ; Not bound by default
      ;; backward-up-list                    ; `C-M-u', `C-M-up'
      backward-word                     ; `M-b', `M-left'
      left-word                         ; `C-left'
      ;; delete-backward-char
      ;; kill-backward-up-list               ; Not bound by default
      ;; beginning-of-buffer                 ; `M-<', `C-home'
      ;; beginning-of-defun                  ; `C-M-a', `C-M-home',
      ;; beginning-of-line                   ; `C-a', `home'
      ;; beginning-of-line+                  ; `C-a', `home'
      ;; beginning-of-line-text              ; Not bound by default
      ;; beginning-of-visual-line            ; `C-a', `home'
      )
    "*Commands whose key bindings initiate Isearch edit.
When invoked by a key sequence, Isearch edits the search string,
applying the command to it immediately.

Commands you might want to include here are typically commands that
move point to the left, possibly deleting text along the way.

Set this to `nil' if you always want all such commands to exit Isearch
and act on the buffer text."
    :set #'(lambda (sym defs)
             (custom-set-default sym defs)
             (isearchp-update-edit-init-commands))
    :type '(repeat (restricted-sexp :tag "Command"
                    ;; Use `symbolp' instead of `functionp' or `fboundp', in
                    ;; case the library defining the function is not loaded.
                    :match-alternatives (symbolp) :value ignore))
    :group 'isearch-plus))

(when (or (> emacs-major-version 24)    ; Emacs 24.3+
          (and (= emacs-major-version 24)  (> emacs-minor-version 2)))
  (defcustom isearchp-deactivate-region-flag t
    "*Non-nil means isearching deactivates the region.
See also option `isearchp-restrict-to-region-flag'."
    :type 'boolean :group 'isearch-plus)

  (defcustom isearchp-restrict-to-region-flag t
    "*Non-nil means restrict isearching to the active region.
See also option `isearchp-deactivate-region-flag'."
    :type 'boolean :group 'isearch-plus))

;;;###autoload
(defcustom isearchp-mouse-2-flag t
  "*Non-nil means clicking `mouse-2' during Isearch yanks the selection.
In that case, you can select text with the mouse, then hit `C-s' to
search for it.

If the value is nil, yank only if the `mouse-2' click is in the echo
area.  If not in the echo area, invoke whatever `mouse-2' is bound to
outside of Isearch."
  :type 'boolean :group 'isearch-plus)

(when (> emacs-major-version 21)
  (defcustom isearchp-on-demand-action-function 'isearchp-replace-on-demand
    "*Function invoked by command `isearchp-act-on-demand'.
It is called with no arguments.

It can access the raw prefix argument used for command
`isearchp-act-on-demand' as the value of variable `isearchp-pref-arg'.

The default value, `isearchp-replace-on-demand', replaces the search
hit with the value of `isearchp-replacement'."
    :group 'isearch-plus :type 'function))

;;;###autoload
(defcustom isearchp-regexp-quote-yank-flag t
  "*Non-nil means escape special chars in text yanked for a regexp isearch.
You can toggle this with `isearchp-toggle-regexp-quote-yank', bound to
`C-`' during isearch."
  :type 'boolean :group 'isearch-plus)

;;;###autoload
(defcustom isearchp-set-region-flag nil
  "*Non-nil means set region around search target.
This is used only for Transient Mark mode.
You can toggle this with `isearchp-toggle-set-region', bound to
`C-SPC' during isearch."
  :type 'boolean :group 'isearch-plus)

;;;###autoload
(defcustom isearchp-toggle-option-flag nil
  "*Non-nil means Isearch toggling commands can affect option values.
If nil, the option value remains unchanged - the effect is temporary.

Applies to toggle commands for behavior that has an associated user
option.  Currently this means `M-s i' (`isearch-toggle-invisible') and
`M-c' (`isearch-toggle-case-fold')."
  :type 'boolean :group 'isearch-plus)

(defvar isearchp-last-non-nil-case-fold (or isearchp-case-fold  t)
  "Last non-nil value of option `isearchp-case-fold'.")

(defvar isearchp-last-non-nil-invisible (or search-invisible  'open)
  "Last non-nil value of option `search-invisible'.")

(defvar isearchp-last-quit-search nil
  "Last successful search string when you hit `C-g' to quit Isearch.")

(defvar isearchp-last-quit-regexp-search nil
  "Last successful search regexp when you hit `C-g' to quit regexp Isearch.")

(when (> emacs-major-version 21)
  (defvar isearchp-nomodify-action-hook nil
    "Functions invoked after visiting and highlighting each search hit.
Each function is invoked passing no arguments.  It can access the
current search hit using the match data.

NOTE: The functions must not update the buffer text (in a way
noticeable by Isearch), or else they will likely lead to a call-stack
overflow.  This is because they are called with Isearch suspended
during `isearch-update' (which can itself be invoked by the
action...)."))

(defvar isearchp-noprompt-action-function nil
  "Function invoked after visiting and highlighting each search hit.
This is reset to nil when you quit Isearch.

The function cannot use the minibuffer.  It is called with no
arguments.  It can access the current search hit using the match
data.")

(defvar isearchp-pref-arg  nil
  "Raw prefix arg value when you invoked `isearchp-act-on-demand'.")

(defvar isearchp-reg-beg nil            ; Used only for Emacs 24.3+
  "Beginning of the nonempty active region or nil.
If `isearchp-restrict-to-region-flag' then the former.
Set when Isearch is started.")

(defvar isearchp-reg-end nil            ; Used only for Emacs 24.3+
  "End of the nonempty active region or nil.
If `isearchp-restrict-to-region-flag' then the former.
Set when Isearch is started.")

(when (> emacs-major-version 21)

  (defvar isearchp-replace-literally nil  ; Toggle using `M-`'.
    "Non-nil means to treat replacement text literally.
Otherwise (nil), interpret `\\' specially in replacement text, as in
the LITERAL argument to `replace-match'.
You can use `M-`' to toggle this anytime during Isearch.")

  (defvar isearchp-replacement ""
    "Replacement string used by `isearchp-replace-on-demand'."))

(unless (boundp 'isearch-update-post-hook)
  (defvar isearch-update-post-hook ()
    "Function(s) called after each Isearch command.
More precisely, called at the end of `isearch-update'."))

(defvar isearchp-win-pt-line nil
  "Line number of point before searching, relative to `window-start'.")

;; Vanilla - no-op, but with a doc string.
(defvar isearch-invisible  search-invisible
  "Whether or not to search invisible text.
Values are the same as for option `search-invisible'.
This variable has an effect only for the current search.")
 
;;(@* "Macros")

;;; Macros -----------------------------------------------------------

(defmacro isearchp-user-error (&rest args)
  "`user-error' if defined, otherwise `error'."
  `(if (fboundp 'user-error) (user-error ,@args) (error ,@args)))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Add `catch', and update `isearch-success' with thrown value.
;; 2. Provided for Emacs 22 through 24.2, anyway.
;;
;; (when (and (> emacs-major-version 21)   ; Emacs 22 through 24.2
;;            (or (< emacs-major-version 24)  (and (= emacs-major-version 24)  (< emacs-minor-version 4))))
;;
(defmacro with-isearch-suspended (&rest body)
  "Exit Isearch mode, run BODY, and reinvoke the pending search.
BODY can involve use of the minibuffer, including recursive minibuffers.

You can update the global isearch variables by setting new values to
`isearch-new-string', `isearch-new-message', `isearch-new-forward',
`isearch-new-word', `isearch-new-case-fold'.

If BODY `throw's a non-nil value, it is taken as the position from
which to resume searching.  Otherwise, searching resumes where it was
suspended."
  ;; This code is very hairy for several reasons, explained in the code.
  ;; Mainly, `isearch-mode' must be terminated while suspended and then restarted.
  ;; If there were a way to catch any change of buffer from the minibuffer, this could be
  ;; simplified greatly.
  ;;
  ;; This code does not back up the search point. Should it, for use with `isearch-edit-string'?
  `(let ((newpoint
          (catch 'with-isearch-suspended
            (condition-case nil
                (progn
                  (let ((enable-recursive-minibuffers  t)
                        (isearch-nonincremental        isearch-nonincremental)
                        ;; Locally bind all isearch global vars to protect them from recursive isearching.
                        ;; isearch-string -message and -forward are not bound, so they can be changed.
                        ;; Instead, save the values.
                        (isearch-new-string            isearch-string)
                        (isearch-new-message           isearch-message)
                        (isearch-new-forward           isearch-forward)
                        (isearch-new-word              isearch-word)
                        (isearch-new-case-fold         isearch-case-fold-search)
                        (isearch-regexp                isearch-regexp)
                        (isearch-op-fun                isearch-op-fun)
                        (isearch-cmds                  isearch-cmds)
                        (isearch-success               isearch-success)
                        (isearch-wrapped               isearch-wrapped)
                        (isearch-barrier               isearch-barrier)
                        (isearch-adjusted              isearch-adjusted)
                        (isearch-yank-flag             isearch-yank-flag)
                        (isearch-error                 isearch-error)
                        ;; Do not bind this.  We want `isearch-search', below, to set it.  And the old value
                        ;; does not matter after that.
                        ;; (isearch-other-end         isearch-other-end)
                        ;;
                        ;; Perhaps some of these other variables should be bound for a shorter period,
                        ;; ending before the next isearch-search.  But there doesn't seem to be a real bug,
                        ;; so let's not risk it now.
                        (isearch-opoint                isearch-opoint)
                        (isearch-slow-terminal-mode    isearch-slow-terminal-mode)
                        (isearch-small-window          isearch-small-window)
                        (isearch-recursive-edit        isearch-recursive-edit)
                        ;; Save the current configuration so we can restore it.
                        (isearch-window-configuration  (current-window-configuration))
                        ;; This could protect the index of the search rings, but we can't reliably count the
                        ;; number of typed `M-p' in `read-from-minibuffer' to adjust the index accordingly.
                        ;; So when the following is commented out, `isearch-mode' below resets the index to
                        ;; the predictable value nil.
                        ;; (search-ring-yank-pointer        search-ring-yank-pointer)
                        ;; (regexp-search-ring-yank-pointer regexp-search-ring-yank-pointer)

                        ;; Temporarily restore `minibuffer-message-timeout'.
                        (minibuffer-message-timeout    isearch-original-minibuffer-message-timeout)
                        (isearch-original-minibuffer-message-timeout
                         isearch-original-minibuffer-message-timeout)
                        old-point  old-other-end)
                    ;; Suspend isearching until BODY is done.  This is so that the user can do anything
                    ;; without failure, like switch buffers and start another isearch, and return.
                    (condition-case nil (isearch-done t t) (exit nil)) ; was recursive editing
                    ;; Save old point and `isearch-other-end' before reading from minibuffer, which can
                    ;; change their values.
                    (setq old-point      (point)
                          old-other-end  isearch-other-end)
                    (unwind-protect (progn ,@body)
                      ;; Set point at the start (end) of old match if forward (backward), so after exiting
                      ;; minibuffer isearch resumes at the start (end) of this match and can find it again.
                      (when (and old-other-end  (eq old-point (point))  (eq isearch-forward
                                                                            isearch-new-forward)
                                 (not (eq last-command 'isearchp-act-on-demand)))
                        (goto-char old-other-end))
                      ;; Always resume isearching by restarting it.
                      (isearch-mode isearch-forward isearch-regexp isearch-op-fun nil isearch-word)
                      ;; Copy new local values to isearch globals
                      (setq isearch-string            isearch-new-string
                            isearch-message           isearch-new-message
                            isearch-forward           isearch-new-forward
                            isearch-word              isearch-new-word
                            isearch-case-fold-search  isearch-new-case-fold))
                    ;; Empty `isearch-string' means use default.
                    (when (= 0 (length isearch-string))
                      (setq isearch-string   (or (car (if isearch-regexp regexp-search-ring search-ring))
                                                 "")
                            isearch-message  (mapconcat 'isearch-text-char-description
                                                        isearch-string ""))
                      ;; After taking the last element, adjust ring to previous one.
                      (isearch-ring-adjust1 nil)))
                  ;; This used to push the state as of before this `C-s', but it adds an inconsistent state
                  ;; where some of the variables are from the previous search (e.g. `isearch-success'), and
                  ;; some of the variables were just entered from the minibuffer (e.g. `isearch-string').
                  ;; (isearch-push-state)

                  (isearch-search)      ; Reinvoke the pending search.
                  (isearch-push-state)  ; Push the correct state.
                  (isearch-update)
                  (when isearch-nonincremental
                    ;; (sit-for 1) ;; needed if `isearch-done' does: (message "")
                    (isearch-done)
                    ;; The search-done message is confusing when the string is empty, so erase it.
                    (when (equal isearch-string "") (message ""))))
              ;; Handle `abort-recursive-edit' outside of `let' to restore outside global values.
              (quit (isearch-abort)))
            nil)))
    (when newpoint (setq isearch-success  newpoint))))
 
;;(@* "Commands")

;;; Commands ---------------------------------------------------------

(when (> emacs-major-version 21)        ; Emacs 22+, for `with-isearch-suspended'.

  (defun isearchp-eval-sexp-and-insert ()
    "Prompt for Lisp sexp, eval it, and append value to the search string."
    (interactive)
    (with-isearch-suspended
        (let ((sexp  (read-from-minibuffer "Eval: "
                                           nil icicle-read-expression-map t 'read-expression-history)))
          (message "Evaluating...")
          (if (or (not (boundp 'eval-expression-debug-on-error))
                  (null eval-expression-debug-on-error))
              (setq values  (cons (eval sexp) values))
            (let ((old-value  (make-symbol "t"))
                  new-value)
              ;; Bind `debug-on-error' to something unique so that we can detect when evaled code changes it.
              (let ((debug-on-error  old-value))
                (setq values     (cons (eval sexp) values)
                      new-value  debug-on-error))
              ;; If eval'd code changed value of `debug-on-error', propagate change to the global binding.
              (unless (eq old-value new-value) (setq debug-on-error  new-value))))
          (setq isearch-new-string  (concat isearch-string (prin1-to-string (car values) 'NOESCAPE))))))

  (defun isearchp-act-on-demand (arg)   ; Bound to `C-M-RET' in `isearch-mode-map'.
    "Invoke the value of `isearchp-on-demand-action-function'.
This suspends Isearch, performs the action, then reinvokes Isearch.
By default, replace the search hit - see `isearchp-replace-on-demand'.
Bound to `\\<isearch-mode-map>\\[isearchp-act-on-demand]' during Isearch."
    (interactive "P")
    (when (and isearch-success  (not isearch-error)  (not isearch-just-started))
      (with-isearch-suspended
          (let ((isearchp-pref-arg  arg)) (funcall isearchp-on-demand-action-function)))))

  (defun isearchp-remove-failed-part () ; Bound to `' in `isearch-mode-map'.
    "Remove the failed part of the search string, if any."
    (interactive)
    (with-isearch-suspended
        (setq isearch-new-string  (substring isearch-string 0 (or (isearch-fail-pos)
                                                                  (length isearch-string)))
              isearch-new-message (mapconcat 'isearch-text-char-description isearch-new-string ""))))
  )

;;;###autoload
(defun isearchp-cycle-mismatch-removal () ; Bound to `M-k' in `isearch-mode-map'.
  "Cycle option `isearchp-drop-mismatch'."
  (interactive)
  (setq isearchp-drop-mismatch  (case isearchp-drop-mismatch
                                  (replace-last  nil)
                                  ((nil)         t)
                                  (otherwise     'replace-last)))
  (if (and isearchp-drop-mismatch  (not (eq 'replace-last isearchp-drop-mismatch)))
      (add-hook 'isearch-update-post-hook 'isearchp-remove-mismatch)
    (remove-hook 'isearch-update-post-hook 'isearchp-remove-mismatch))
  (case isearchp-drop-mismatch
    (replace-last  (message "Automatic REPLACEMENT of last mismatched input is now ON"))
    ((nil)         (message "Automatic removal of mismatched input is now OFF"))
    (otherwise     (message "Automatic removal of ALL mismatched input is now ON")))
  (sit-for 1)
  (isearch-update))

(defun isearchp-remove-mismatch ()
  "Remove the mismatched portion of the search string."
  (while (or (not isearch-success)  (if (boundp 'isearch-error)
                                        isearch-error
                                      isearch-invalid-regexp))
    (isearch-pop-state))
  (remove-hook 'isearch-update-post-hook 'isearchp-remove-mismatch)
  (isearch-update)
  (add-hook 'isearch-update-post-hook 'isearchp-remove-mismatch))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Respect `isearchp-toggle-option-flag', possibly toggling option `search-invisible'.
;; 2. Added prefix arg to flip handling of `isearchp-toggle-option-flag'.
;;
;;;###autoload
(defun isearch-toggle-invisible (flip)  ; Bound to `M-s i'.
  "Toggle searching in invisible text on or off.
If `isearchp-toggle-option-flag' is non-nil then toggle the value of
option `search-invisible'.  If it is nil then toggle the behavior only
temporarily, so that the option value is unchanged for subsequent
searches.

A prefix argument flips the sense of the last paragraph, so that the
option is updated only if `isearchp-toggle-option-flag' is nil instead
of non-nil.

To use a prefix argument you must set either `isearch-allow-scroll' or
`isearch-allow-prefix' (if available) to non-nil.  Otherwise, a prefix
arg during Isearch exits Isearch.

When toggling invisible searching on, restores the last behavior
according to option `search-invisible': t or `open'."
  (interactive "P")
  (let ((current-only-p  (or (and (not isearchp-toggle-option-flag)  (not flip))
                             (and isearchp-toggle-option-flag  flip))))
    (if current-only-p
        (setq isearch-invisible  (if isearch-invisible nil (or search-invisible  'open)))
      (when search-invisible (setq isearchp-last-non-nil-invisible  search-invisible))
      (setq search-invisible   (if search-invisible nil isearchp-last-non-nil-invisible)
            isearch-invisible  search-invisible))
    (let ((message-log-max  nil))
      (message "%s%s [match %s text%s]" (isearch-message-prefix nil isearch-nonincremental)
               isearch-message (if isearch-invisible "INvisible" "only VISIBLE")
               (if (not current-only-p) " FROM NOW ON" ""))))
  (setq isearch-success   t
        isearch-adjusted  t)
  (sit-for 1)
  (isearch-update))

;;;###autoload
(defun isearchp-toggle-search-invisible () ; Bound to `C-+' in `isearch-mode-map'.
  "Toggle the value of user option `search-invisible'.
Toggles between nil and the last non-nil value."
  (interactive)
  (when search-invisible (setq isearchp-last-non-nil-invisible  search-invisible))
  (setq search-invisible   (if search-invisible nil isearchp-last-non-nil-invisible)
        isearch-invisible  search-invisible)
  (message "Option `search-invisible' is now `%s'" (case search-invisible
                                                     (open  'OPEN)
                                                     ((nil) 'OFF)
                                                     (t     'ON)))
  (setq isearch-success   t
        isearch-adjusted  t)
  (sit-for 1)
  (isearch-update))

;;;###autoload
(defun isearchp-toggle-option-toggle () ; Bound to `M-s v' in `isearch-mode-map'.
  "Toggle the value of option `isearchp-toggle-option-flag'."
  (interactive)
  (setq isearchp-toggle-option-flag  (not isearchp-toggle-option-flag))
  (message "Option `isearchp-toggle-option-flag' is now %s" (if isearchp-toggle-option-flag 'ON 'OFF))
  (sit-for 1))

;;;###autoload
(defun isearchp-toggle-regexp-quote-yank () ; Bound to `C-`' in `isearch-mode-map'.
  "Toggle `isearchp-regexp-quote-yank-flag'."
  (interactive)
  (setq isearchp-regexp-quote-yank-flag  (not isearchp-regexp-quote-yank-flag))
  (message "Escaping regexp special chars for yank is now %s" (if isearchp-regexp-quote-yank-flag 'ON 'OFF))
  (sit-for 1)
  (isearch-update))

;;;###autoload
(defun isearchp-toggle-set-region ()    ; Bound to `C-SPC' in `isearch-mode-map'.
  "Toggle `isearchp-set-region-flag'."
  (interactive)
  (setq isearchp-set-region-flag  (not isearchp-set-region-flag))
  (message "Setting region around search target is now %s" (if isearchp-set-region-flag 'ON 'OFF))
  (sit-for 1)
  (isearch-update))

(when (or (> emacs-major-version 24)    ; Emacs 24.3+
          (and (= emacs-major-version 24)  (> emacs-minor-version 2)))
  (defun isearchp-toggle-region-restriction () ; Bound to `C-x n' in `isearch-mode-map'.
    "Toggle option `isearchp-restrict-to-region-flag'."
    (interactive)
    (setq isearchp-restrict-to-region-flag  (not isearchp-restrict-to-region-flag))
    (message "Restricting search to active region is now %s" (if isearchp-restrict-to-region-flag 'ON 'OFF))
    (sit-for 1)
    (isearch-update)))


;; REPLACE ORIGINAL in `isearch.el' (Emacs 22+).
;;
;; 1. Turn off `isearch-regexp' when `isearch-word'.
;; 2. Show message about new state.
;;
;; From Juri Linkov, 2006-10-29, to emacs-devel@gnu.org
;; From Stefan Monnier, 2006-11-23, to help-gnu-emacs@gnu.org
;;
(defun isearch-toggle-word ()           ; Bound to `M-s w' in `isearch-mode-map'.
  "Toggle word searching on or off."
  ;; The status stack is left unchanged.
  (interactive)
  (setq isearch-word  (not isearch-word))
  (when isearch-word (setq isearch-regexp  nil)) ; Added to Juri's code by Stefan.
  (setq isearch-success   t
        isearch-adjusted  t)
  (message "Whole word search is now %s" (if isearch-word 'ON 'OFF))
  (sit-for 1)
  (isearch-update))

;;;###autoload
(defun isearchp-set-region-around-search-target ()
  "Set the region around the last search or query-replace target."
  (interactive)
  (case last-command
    ((isearch-forward isearch-backward isearch-forward-regexp isearch-backward-regexp)
     (push-mark isearch-other-end t 'activate))
    (t (push-mark (match-beginning 0) t 'activate)))
  (setq deactivate-mark  nil))

(defun isearchp-message-prefix (&optional arg1 arg2 arg3)
  "Version of `isearch-message-prefix' that works for all Emacs releases."
  (if (or (< emacs-major-version 24)
          (and (= emacs-major-version 24)  (< emacs-minor-version 3)
               (not (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+" emacs-version))))
      (isearch-message-prefix arg1 arg2 arg3) ; Emacs 20 through 24.2.
    (isearch-message-prefix arg1 arg2))) ; Emacs 24.1.N and 24.3+

(defun isearchp-message-suffix (&optional arg1 arg2)
  "Version of `isearch-message-suffix' that works for all Emacs releases."
  (if (or (< emacs-major-version 24)
          (and (= emacs-major-version 24)  (< emacs-minor-version 3)
               (not (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+" emacs-version))))
      (isearch-message-suffix arg1 arg2) ; Emacs 20 through 24.2.
    (isearch-message-suffix arg1)))     ; Emacs 24.1.N and  24.3+


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Respect `isearchp-toggle-option-flag', possibly toggling option `isearchp-case-fold'.
;; 2. Added prefix arg to flip handling of `isearchp-toggle-option-flag'.
;; 3. Update minor-mode mode-line lighter to reflect case sensitivity.
;;
;;;###autoload
(defun isearch-toggle-case-fold (flip) ; Bound to `M-c' in `isearch-mode-map'.
  "Toggle case sensitivity on or off during incremental searching.
The minor-mode lighter shows `ISEARCH' for case-insensitive, `Isearch'
for case-sensitive.

If `isearchp-toggle-option-flag' is non-nil then toggle the value of
option `isearchp-case-fold'.  If it is nil then toggle the behavior
only temporarily, so that the option value is unchanged for subsequent
searches.

A prefix argument flips the sense of the last paragraph, so that the
option is updated only if `isearchp-toggle-option-flag' is nil instead
of non-nil.

To use a prefix argument you must set either `isearch-allow-scroll' or
`isearch-allow-prefix' (if available) to non-nil.  Otherwise, a prefix
arg during Isearch exits Isearch.

When toggling case-sensitive searching on, restores the last behavior
according to option `isearchp-case-fold': t or `yes'."
  (interactive "P")
  (let ((current-only-p  (or (and (not isearchp-toggle-option-flag)  (not flip))
                             (and isearchp-toggle-option-flag  flip))))
    (if current-only-p
        (setq isearch-case-fold-search  (if isearch-case-fold-search
                                            nil
                                          (or isearchp-case-fold  t)))
      (when isearchp-case-fold (setq isearchp-last-non-nil-case-fold  isearchp-case-fold))
      (setq isearchp-case-fold   (if isearchp-case-fold nil isearchp-last-non-nil-case-fold)
            isearch-case-fold-search  isearchp-case-fold))
    (let ((message-log-max  nil))
      (message "%s%s [case %ssensitive%s]" (isearchp-message-prefix nil nil isearch-nonincremental)
               isearch-message (if isearch-case-fold-search "IN" "")
               (if (not current-only-p) " FROM NOW ON" ""))))
  (setq isearch-success   t
        isearch-adjusted  t)
  (isearchp-highlight-lighter)
  (sit-for 1)
  (isearch-update))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Ends isearch: does `isearch-done' and `isearch-clean-overlays'
;;    instead of `isearch-update'.
;; 2. Lists isearch bindings too.
;;;###autoload
(defun isearch-mode-help ()             ; Bound to `C-h' in `isearch-mode-map'.
  "Display information on interactive search in buffer *Help*."
  (interactive)
  (describe-function 'isearch-forward)
  (isearch-done)
  (isearch-clean-overlays)
  (with-current-buffer "*Help*"
    (goto-char (point-max))
    (let ((buffer-read-only  nil)) (insert (substitute-command-keys "

Bindings in Isearch minor mode:
------------------------------

\\{isearch-mode-map}")))))


(when (> emacs-major-version 21)        ; Emacs 22+

  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; 1. Start with point at the mismatch position - use `isearchp-message-prefix'.
  ;; 2. Use macro `with-isearch-suspended'.
  ;;
  (defun isearch-edit-string ()         ; Bound to `M-e' in `isearch-mode-map'.
    "Edit the search string in the minibuffer.
The following additional command keys are active while editing.
\\<minibuffer-local-isearch-map>
\\[exit-minibuffer] to resume incremental searching with the edited string.
\\[isearch-forward-exit-minibuffer] to resume isearching forward.
\\[isearch-reverse-exit-minibuffer] to resume isearching backward.
\\[isearch-complete-edit] to complete the search string using the search ring.
\\[insert-char] to insert a Unicode character by name (with completion)."
    (interactive)
    (with-isearch-suspended
        (let* ((message-log-max            nil)
               ;; Do not add a new search string to the search ring here in `read-from-minibuffer'.
               ;; It should be added only by `isearch-update-ring', called from `isearch-done'.
               (history-add-new-input      nil)
               (minibuffer-history-symbol  nil)) ; Workaround for some incompatibility with `gmhist'.
          ;; FREE VARS here: `isearch-new-string', `isearch-new-message'.
          ;; Bound in `with-isearch-suspended'.
          (setq isearch-new-string   (read-from-minibuffer
                                      (isearchp-message-prefix nil nil isearch-nonincremental)
                                      (cons isearch-string (1+ (or (isearch-fail-pos)
                                                                   (length isearch-string))))
                                      minibuffer-local-isearch-map  nil
                                      (if isearch-regexp
                                          (cons 'regexp-search-ring
                                                (1+ (or regexp-search-ring-yank-pointer  -1)))
                                        (cons 'search-ring (1+ (or search-ring-yank-pointer  -1))))
                                      nil t)
                isearch-new-message  (mapconcat 'isearch-text-char-description isearch-new-string
                                                "")))))

  ;; Suggested by Juri Linkov: http://lists.gnu.org/archive/html/emacs-devel/2012-12/msg00281.html.
  ;;
  (defun isearchp-open-recursive-edit () ; Bound to `C-x o' in `isearch-mode-map'.
    "Invoke the editor command loop recursively, during Isearch.
Use `\\[exit-recursive-edit]' to end the recursive edit and resume searching from there.
Or use `abort-recursive-edit' to exit the recursive edit and cancel the previous search."
    (interactive)
    (with-isearch-suspended (recursive-edit))))

(when (< emacs-major-version 22)        ; Emacs 20-21.
  ;; Parts of the definition were taken from later Emacs versions, but are compatible with Emacs 20.
  (defun isearch-edit-string ()
    "Edit the search string in the minibuffer.
The following additional command keys are active while editing.
\\<minibuffer-local-isearch-map>
\\[exit-minibuffer] to resume incremental searching with the edited string.
\\[isearch-nonincremental-exit-minibuffer] to do one nonincremental search.
\\[isearch-forward-exit-minibuffer] to resume isearching forward.
\\[isearch-reverse-exit-minibuffer] to resume isearching backward.
\\[isearch-complete-edit] to complete the search string using the search ring.
\\<isearch-mode-map>
If first char entered is \\[isearch-yank-word], then do word search instead."
    ;; This code is very hairy for several reasons, explained in the code.
    ;; Mainly, `isearch-mode' must be terminated while editing and then restarted.
    ;; If there were a way to catch any change of buffer from the minibuffer, this could be
    ;; simplified greatly.
    ;; This code does not back up the search point. Should it, for use with `isearch-edit-string'?
    (interactive)
    (condition-case nil
        (progn (let ((isearch-nonincremental        isearch-nonincremental)
                     ;; Locally bind all isearch global variables to protect them from recursive
                     ;; isearching.  Do not bind `isearch-string', `isearch-message', and
                     ;; `isearch-forward', so they can be changed.  Instead, save their values.
                     (isearch-new-string            isearch-string)
                     (isearch-new-message           isearch-message)
                     (isearch-new-forward           isearch-forward)
                     (isearch-new-word              isearch-word)
                     (isearch-regexp                isearch-regexp)
                     (isearch-op-fun                isearch-op-fun)
                     (isearch-cmds                  isearch-cmds)
                     (isearch-success               isearch-success)
                     (isearch-wrapped               isearch-wrapped)
                     (isearch-barrier               isearch-barrier)
                     (isearch-adjusted              isearch-adjusted)
                     (isearch-yank-flag             isearch-yank-flag)
                     (isearch-invalid-regexp        isearch-invalid-regexp)
                     (isearch-within-brackets       isearch-within-brackets)
            ;;; Do not bind this.  We want isearch-search, below, to set it.
            ;;; And the old value won't matter after that.
            ;;;	     (isearch-other-end             isearch-other-end)
            ;;; Perhaps some of these other variables should be bound for a
            ;;; shorter period, ending before the next isearch-search.
            ;;; But there doesn't seem to be a real bug, so let's not risk it now.
                     (isearch-opoint                isearch-opoint)
                     (isearch-slow-terminal-mode    isearch-slow-terminal-mode)
                     (isearch-small-window          isearch-small-window)
                     (isearch-recursive-edit        isearch-recursive-edit)
                     ;; Save current configuration so we can restore it here.
                     (isearch-window-configuration  (current-window-configuration))
                     old-point old-other-end)
                 ;; Actually terminate isearching until editing is done.
                 ;; This is so that the user can do anything without failure,
                 ;; like switch buffers and start another isearch, and return.
                 (condition-case nil (isearch-done t t) (exit nil)) ; was recursive editing
                 ;; Save old point and `isearch-other-end' before reading from minibuffer, which
                 ;; can change their values.
                 (setq old-point      (point)
                       old-other-end  isearch-other-end)
                 (unwind-protect
                      (let* ((message-log-max            nil)
                             ;; Binding `minibuffer-history-symbol' to nil is a workaround for some
                             ;; incompatibility with `gmhist'.
                             (minibuffer-history-symbol  nil))
                        (setq isearch-new-string   (read-from-minibuffer
                                                    (isearchp-message-prefix nil nil
                                                                             isearch-nonincremental)
                                                    (cons isearch-string
                                                          (1+ (length isearch-string)))
                                                    minibuffer-local-isearch-map nil
                                                    (if isearch-regexp
                                                        (cons
                                                         'regexp-search-ring
                                                         (1+ (or regexp-search-ring-yank-pointer
                                                                 -1)))
                                                      (cons 'search-ring
                                                            (1+ (or search-ring-yank-pointer  -1))))
                                                    nil t)
                              isearch-new-message  (mapconcat 'isearch-text-char-description
                                                              isearch-new-string "")))
                   ;; Set point at the start (end) of old match if forward (backward),
                   ;; so after exiting minibuffer isearch resumes at the start (end)
                   ;; of this match and can find it again.
                   (if (and old-other-end  (eq old-point (point))  (eq isearch-forward
                                                                       isearch-new-forward))
                       (goto-char old-other-end))
                   ;; Always resume isearching by restarting it.
                   (isearch-mode isearch-forward isearch-regexp isearch-op-fun nil isearch-word)
                   ;; Copy new local values to isearch globals
                   (setq isearch-string   isearch-new-string
                         isearch-message  isearch-new-message
                         isearch-forward  isearch-new-forward
                         isearch-word     isearch-new-word))
                 ;; Empty isearch-string means use default.
                 (if (= 0 (length isearch-string))
                     (setq isearch-string   (or (car (if isearch-regexp
                                                         regexp-search-ring
                                                       search-ring))
                                                "")
                           isearch-message  (mapconcat 'isearch-text-char-description
                                                       isearch-string ""))
                   ;; This used to set the last search string, but it is not right to do that here.
                   ;; Only the string actually used should be saved.
                   ))

               ;; This used to push the state as of before this `C-s', but it adds an inconsistent
               ;; state where some of variables are from the previous search (e.g.
               ;; `isearch-success'), and some of variables are just entered from the minibuffer
               ;; (e.g. `isearch-string').
               ;; (isearch-push-state)

               (isearch-search)         ; Reinvoke the pending search.
               (isearch-push-state)     ; Push the correct state.
               (isearch-update)
               (when isearch-nonincremental
                 ;; (sit-for 1) ;; needed if isearch-done does: (message "")
                 (isearch-done)
                 ;; The search done message is confusing when the string
                 ;; is empty, so erase it.
                 (if (equal isearch-string "")
                     (message ""))))
      ;; Handle `abort-recursive-edit' outside of let to restore outside global values.
      (quit (isearch-abort)))))

(when (and (> emacs-major-version 22)   ; Emacs 23 (bc supports Unicode) through 24.2
           (or (< emacs-major-version 24)  (and (= emacs-major-version 24)  (< emacs-minor-version 4))))
  (defun isearch-char-by-name (&optional count)
    "Read a character by its Unicode name and append it to the search string.
Completion is available as in `read-char-by-name', used by `insert-char'.
With a numeric prefix arg, append that many copies of the character."
    (interactive "p")
    (with-isearch-suspended
        (let ((char  (read-char-by-name "Append char to search string (Unicode name or hex): ")))
          (when char
            (let ((string  (if (and (integerp count)  (> count 1))
                               (make-string count char)
                             (char-to-string char))))
              (setq isearch-new-string   (concat isearch-string string)
                    isearch-new-message  (concat isearch-message (mapconcat 'isearch-text-char-description
                                                                            string "")))))))))

(when (fboundp 'isearch-yank-internal)  ; Emacs 22+
  (defun isearchp-yank-char ()          ; Bound to `C-z' and `C-y C-c' in `isearch-mode-map'.
    "Yank next character from buffer onto search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearch-yank-char)) 

  (defun isearchp-yank-word-or-char ()  ; Bound to `C-w' and `C-y C-w' in `isearch-mode-map'.
    "Yank next word or character from buffer onto search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearch-yank-word-or-char)) 

  (defun isearchp-yank-line ()          ; Bound to `C-y C-e' in `isearch-mode-map'.
    "Yank text from buffer up to end of line onto search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearch-yank-line)) 

  (defun isearchp-yank-symbol-or-char-1 ()
    "Helper for `isearchp-yank-symbol-or-char'.
Not intended/needed as a user command."
    (interactive)
    (isearch-yank-internal
     (lambda ()
       (if (or (memq (char-syntax (or (char-after)  0)) '(?w ?_))
               (memq (char-syntax (or (char-after (1+ (point)))  0)) '(?w ?_)))
           (if (and (boundp 'subword-mode)  subword-mode) (subword-forward 1) (forward-symbol 1))
         (forward-char 1))
       (point))))

  (defun isearchp-yank-symbol-or-char () ; Bound to `C-_' and `C-y C-_' in `isearch-mode-map'.
    "Yank char, subword, word, or symbol from buffer into search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearchp-yank-symbol-or-char-1))

  (defun isearchp-yank-sexp-symbol-or-char-1 ()
    "Helper function for `isearchp-yank-sexp-symbol-or-char'.
Not intended/needed as a user command."
    (interactive)
    (isearch-yank-internal
     (lambda ()
       (if (or (= (char-syntax (or (char-after)  0)) ?\( )
               (= (char-syntax (or (char-after (1+ (point)))  0)) ?\( ))
           (forward-sexp 1)
         (if (or (memq (char-syntax (or (char-after)  0)) '(?w ?_))
                 (memq (char-syntax (or (char-after (1+ (point)))  0)) '(?w ?_)))
             (if (and (boundp 'subword-mode)  subword-mode) (subword-forward 1) (forward-symbol 1))
           (forward-char 1)))
       (point))))

  (defun isearchp-yank-sexp-symbol-or-char () ; Bound to `C-(' and `C-y C-(' in `isearch-mode-map'.
    "Yank sexp, symbol, subword, word, or char into search string.
You can repeat this by hitting the last key again..."
    (interactive)
    (require 'repeat nil t)
    (isearchp-repeat-command 'isearchp-yank-sexp-symbol-or-char-1)))

(defun isearchp-kill-ring-save ()       ; Bound to `M-w' in `isearch-mode-map'.
  "Copy the current search string to the kill ring.
For example, you can then use `C-s M-y' to search for the same thing
in another Emacs session."
  (interactive)
  (kill-new isearch-string)
  (let ((message-log-max  nil)) (message "Copied search string as kill"))
  (sit-for 1)
  (isearch-update))

(defun isearchp-append-register ()      ; Bound to `C-x r g', the same as `insert-register' globally.
  "Insert register contents at point in search string.
You are prompted for the register to use."
  (interactive)
  (let ((current-prefix-arg  t)
        string)
    (with-temp-buffer
      (call-interactively 'insert-register)
      (setq string  (buffer-substring (point-min) (point-max))))
    (isearch-yank-string string)))

(defun isearchp-retrieve-last-quit-search () ; Bound to `M-g' in `isearch-mode-map'.
  "Insert last successful search string from when you hit `C-g' in Isearch.
Bound to `\\<isearch-mode-map>\\[isearchp-retrieve-last-quit-search]' during Isearch."
  (interactive)
  (cond ((and isearch-regexp  isearchp-last-quit-regexp-search)
         (let ((isearchp-regexp-quote-yank-flag  nil))
           (isearch-yank-string isearchp-last-quit-regexp-search)))
        (isearchp-last-quit-search
         (isearch-yank-string isearchp-last-quit-search))))

(when (> emacs-major-version 20)
  (defun isearchp-fontify-buffer-now ()
    "Fontify buffer completely, right now.
This differs from `font-lock-fontify-buffer', which is lazy and does
not necessarily fontify the whole buffer."
    (interactive)
    (jit-lock-fontify-now)))

;;; $$$$$$ No longer used.  `M-e' puts point at this position automatically.
;;;   (defun isearchp-goto-success-end ()   ; `M-e' in `minibuffer-local-isearch-map'.
;;;     "Go to end of search string text that matches."
;;;     (interactive)
;;;     (goto-char (point-max))
;;;     (let ((cmds  isearch-cmds)
;;;           succ-msg)
;;;       (when (or (not isearch-success)  isearch-error)
;;;         (while (or (not (isearch-success-state (car cmds)))  (isearch-error-state (car cmds)))
;;;           (pop cmds))
;;;         (setq succ-msg  (and cmds  (isearch-message-state (car cmds))))
;;;         (backward-char (- (length isearch-string) (length succ-msg)))))))
 
;;(@* "Non-Interactive Functions")

;;; Non-Interactive Functions


;; REPLACE ORIGINAL in `isearch.el' (Emacs 20 only).
;;
;; Remove unused arg.  Add doc string.
;;
(defun isearch-dehighlight (&rest __)
  "Delete `isearch-overlay'."
  (when isearch-overlay (delete-overlay isearch-overlay)))


;; ADVISE `isearch-update' to run `isearch-update-post-hook', in Emacs 20-21.
(when (< emacs-major-version 22)
  (defadvice isearch-update (after run-isearch-update-post-hook activate)
    "Run `isearch-update-post-hook' at the end."
    (run-hooks 'isearch-update-post-hook)))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Run `isearchp-nomodify-action-hook' if successful (Emacs 22+).
;; Call `isearchp-noprompt-action-function' if successful.
;;
(when (> emacs-major-version 21)        ; Emacs 22+, for `with-isearch-suspended'.
  (defun isearch-update ()
    "This is called after every isearch command, to update the display.
After visiting a search hit, run `isearchp-nomodify-action-hook'
 (Emacs 22+) and invoke `isearchp-noprompt-action-function'.
At the end, run `isearch-update-post-hook'."
    (unless (or unread-command-events  executing-kbd-macro)
      (unless (input-pending-p)
        (if (and (boundp 'isearch-message-function)  isearch-message-function)
            (funcall isearch-message-function)
          (isearch-message)))
      (if (and isearch-slow-terminal-mode  (not (or isearch-small-window
                                                    (pos-visible-in-window-p))))
          (let ((found-point  (point)))
            (setq isearch-small-window  t)
            (move-to-window-line 0)
            (let ((window-min-height  1))
              (split-window nil (if (< search-slow-window-lines 0)
                                    (1+ (- search-slow-window-lines))
                                  (- (window-height) (1+ search-slow-window-lines)))))
            (if (not (< search-slow-window-lines 0))
                (other-window 1)
              (vertical-motion (- 1 search-slow-window-lines))
              (set-window-start (next-window) (point))
              (set-window-hscroll (next-window) (window-hscroll))
              (set-window-hscroll (selected-window) 0))
            (goto-char found-point))
        (let ((current-scroll  (window-hscroll))) ; Keep same hscrolling as at search start.
          (set-window-hscroll (selected-window) isearch-start-hscroll)
          (unless (pos-visible-in-window-p)
            (set-window-hscroll (selected-window) current-scroll))))
      (if (not isearch-other-end)
          (isearch-dehighlight)
        (if (< isearch-other-end (point)) ; Just use `isearch-forward'?
            (isearch-highlight isearch-other-end (point))
          (isearch-highlight (point) isearch-other-end))
        (when (and (> emacs-major-version 21)  isearchp-nomodify-action-hook)
          (with-isearch-suspended (run-hooks 'isearchp-nomodify-action-hook)))
        (when isearchp-noprompt-action-function
          (unwind-protect
               (progn (add-hook 'minibuffer-setup-hook 'isearchp-barf-if-use-minibuffer)
                      (funcall isearchp-noprompt-action-function))
            (remove-hook 'minibuffer-setup-hook 'isearchp-barf-if-use-minibuffer)))))
    (setq  isearch-adjusted   nil
           isearch-yank-flag  nil
           ;; quit-flag       nil  ; No, not for `isearch-mode'.
           )
    (when (and (boundp 'isearch-lazy-highlight)  isearch-lazy-highlight)
      (isearch-lazy-highlight-new-loop))
    ;; Prevent point moving to end of composition when part of it has just been searched.
    (when (boundp 'disable-point-adjustment) (setq disable-point-adjustment  t))
    (run-hooks 'isearch-update-post-hook)))

(defun isearchp-barf-if-use-minibuffer ()
  (error "Tried to use minibuffer in `isearchp-noprompt-action-function'"))

(defun isearchp-reset-noprompt-action-fn () (setq isearchp-noprompt-action-function nil))
(add-hook 'isearch-mode-end-hook 'isearchp-reset-noprompt-action-fn)


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Save `isearchp-win-pt-line'.
;; 2. Save `isearchp-reg-beg' and `isearchp-reg-end'.  (Used only for Emacs 24.3+.)
;; 3. Deactivate region (Emacs 24.3+ only).
;;
(defun isearch-mode (forward &optional regexp op-fun recursive-edit word)
  "Start Isearch minor mode.  Called by `isearch-forward' and similar.
Deactivate the region, if active.
Non-nil argument FORWARD means search in the forward direction.
Non-nil argument REGEXP means regular expression search.
Argument OP-FUN is a function to be called after each input character
 is processed.  (It is not called after chars that exit the search.)
Non-nil RECURSIVE-EDIT means this function behaves modally.  It does
 not return to the calling function until the search is completed.
 It enters a recursive edit, which it exits that search is finished.
Argument WORD, if t, means search for a sequence of words, ignoring
 punctuation.  If a function, the function is called to convert the
 search string to a regexp that is used for regexp searching."
  (setq isearch-forward                  forward ; Initialize global vars.
        isearch-regexp                   regexp
        isearch-word                     word
        isearch-op-fun                   op-fun
        isearch-last-case-fold-search    isearch-case-fold-search
        isearch-case-fold-search         case-fold-search
        isearch-invisible                search-invisible
        isearch-string                   ""
        isearch-message                  ""
        isearch-cmds                     ()
        isearch-success                  t
        isearch-wrapped                  nil
        isearch-barrier                  (point)
        isearch-adjusted                 nil
        isearch-yank-flag                nil
        isearch-invalid-regexp           nil ; Only for Emacs < 22.
        isearch-within-brackets          nil ; Only for Emacs < 22.
        isearch-error                    nil
        isearch-slow-terminal-mode       (and (<= baud-rate search-slow-speed)
                                              (> (window-height) (* 4 (abs search-slow-window-lines))))
        isearch-other-end                nil
        isearch-small-window             nil
        isearch-just-started             t
        isearch-start-hscroll            (window-hscroll)
        isearch-opoint                   (point)
        isearchp-win-pt-line             (- (line-number-at-pos) (line-number-at-pos (window-start)))
        isearchp-reg-beg                 (save-restriction
                                           (widen)
                                           (if (and (boundp 'isearchp-restrict-to-region-flag)
                                                    isearchp-restrict-to-region-flag
                                                    (use-region-p))
                                               (region-beginning)
                                             nil))
        isearchp-reg-end                 (save-restriction
                                           (widen)
                                           (if (and (boundp 'isearchp-restrict-to-region-flag)
                                                    isearchp-restrict-to-region-flag
                                                    (use-region-p))
                                               (region-end)
                                             nil))
        search-ring-yank-pointer         nil
        isearch-opened-overlays          ()
        isearch-input-method-function    input-method-function
        isearch-input-method-local-p     (local-variable-p 'input-method-function)
        regexp-search-ring-yank-pointer  nil
        ;; Save original value of `minibuffer-message-timeout'.
        ;; Then reset it to nil, so Isearch messages do not time-out.
        isearch-original-minibuffer-message-timeout (and (boundp 'minibuffer-message-timeout)
                                                         minibuffer-message-timeout)
        minibuffer-message-timeout       nil)
  (when (and (boundp 'isearchp-deactivate-region-flag)  isearchp-deactivate-region-flag) ; Emacs 24.3+
    (deactivate-mark))
  ;; Bypass input method while reading key.  When a user types a printable char, appropriate
  ;; input method is turned on in minibuffer to read multibyte characters.
  (unless isearch-input-method-local-p (make-local-variable 'input-method-function))
  (setq input-method-function  nil)
  (looking-at "")
  (setq isearch-window-configuration  (if isearch-slow-terminal-mode (current-window-configuration) nil))
  ;; Maybe make minibuffer frame visible and/or raise it.
  (let ((frame  (window-frame (minibuffer-window))))
    (unless (memq (frame-live-p frame) '(nil t))
      (unless (frame-visible-p frame) (make-frame-visible frame))
      (when minibuffer-auto-raise (raise-frame frame))))
  (setq isearch-mode  " Isearch")       ; forward? regexp?
  (force-mode-line-update)
  (setq overriding-terminal-local-map  isearch-mode-map)
  (run-hooks 'isearch-mode-hook)
  ;; Remember the initial map, possibly modified by external packages in `isearch-mode-hook'.  (Bug#16035)
  (when (boundp 'isearch--saved-overriding-local-map)
    (setq isearch--saved-overriding-local-map overriding-terminal-local-map))
  ;; Pushing initial state used to be before running `isearch-mode-hook', but a hook might set
  ;; `isearch-push-state-function' used in `isearch-push-state' to save mode-specific initial state.
  ;; (Bug#4994)
  (isearch-push-state)
  (isearch-update)
  (when (fboundp 'isearch-pre-command-hook) ; Emacs 24.4+
    (add-hook 'pre-command-hook  'isearch-pre-command-hook  nil t)
    (add-hook 'post-command-hook 'isearch-post-command-hook nil t))
  (add-hook 'mouse-leave-buffer-hook 'isearch-done)
  (add-hook 'kbd-macro-termination-hook 'isearch-done)
  ;; `isearch-mode' can be made modal (in the sense of not returning to the calling function until
  ;; searching is completed) by entering a recursive-edit and exiting it when done isearching.
  (when recursive-edit (let ((isearch-recursive-edit t)) (recursive-edit)))
  isearch-success)


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Restore cursor position relative to window (`isearchp-win-pt-line').  Fixes Emacs bug #12253.
;;
(cond ((or (> emacs-major-version 23)   ; Emacs 23.2+
           (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
       (defun isearch-cancel ()
         "Terminate the search and go back to the starting point."
         (interactive)
         (if (and isearch-push-state-function  isearch-cmds)
             ;; For defined push-state function, restore the first state.
             ;; This calls pop-state function and restores original point.
             (let ((isearch-cmds  (last isearch-cmds)))
               (if (fboundp 'isearch--set-state)
                   (isearch--set-state (car isearch-cmds)) ; Emacs 24.3+.
                 (isearch-top-state))   ; Emacs 23.2 to 24.2.
               (when isearchp-win-pt-line (recenter isearchp-win-pt-line)))
           (goto-char isearch-opoint)
           (when isearchp-win-pt-line (recenter isearchp-win-pt-line)))
         (isearch-done t)
         (isearch-clean-overlays)
         (signal 'quit nil)))
      (t                                ; Emacs 20 to 23.1.
       (defun isearch-cancel ()
         "Terminate the search and go back to the starting point."
         (interactive)
         (when (and (fboundp 'isearch-pop-fun-state) ; Emacs 22+.
                    (functionp (isearch-pop-fun-state (car (last isearch-cmds)))))
           (funcall (isearch-pop-fun-state (car (last isearch-cmds))) (car (last isearch-cmds))))
         (goto-char isearch-opoint)
         (when isearchp-win-pt-line (recenter isearchp-win-pt-line))
         (isearch-done t)
         (isearch-clean-overlays)
         (signal 'quit nil))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Save last successful search string or regexp as `isearchp-last-quit-search' or
;; `isearchp-last-quit-regexp-search', for retrieval via `isearchp-retrieve-last-quit-search'.
;;
(defun isearch-abort ()
  "Abort incremental search mode if searching is successful, signaling quit.
Otherwise, revert to previous successful search and continue searching.
Save last successful search string or regexp for later retrieval
 during Isearch, using \\<isearch-mode-map>`\\[isearchp-retrieve-last-quit-search]'.
Use `isearch-exit' to quit without signaling."
  (interactive)
  ;; (ding)  signal instead below, if quitting
  (discard-input)
  (if (and isearch-success
           (or (not (boundp 'isearch-error))  (not isearch-error))) ; Emacs 24+
      ;; If search is successful and has no incomplete regexp, move back to starting point and quit.
      (progn (setq isearch-success  nil)
             (set (if isearch-regexp 'isearchp-last-quit-regexp-search 'isearchp-last-quit-search)
                  isearch-string)
             ;; Exit isearch and pass on quit signal.
             (if (fboundp 'isearch-cancel) ; Emacs 22+
                 (isearch-cancel)
               (goto-char isearch-opoint) ; Emacs 20-21
               (isearch-done t)
               (isearch-clean-overlays)
               (signal 'quit nil)))
    ;; If search is failing, or has an incomplete regexp, rub out until it is once more successful.
    (while (or (not isearch-success)  (if (boundp 'isearch-error)
                                          isearch-error
                                        isearch-invalid-regexp))
      (isearch-pop-state))
    (isearch-update)))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Respect `isearchp-regexp-quote-yank-flag'.
;;
(defun isearch-yank-string (string)
  "Yank STRING into Isearch search string."
  ;; Downcase the string if not supposed to case-fold yanked strings.
  (if (and isearch-case-fold-search  (eq 'not-yanks search-upper-case))
      (setq string  (downcase string)))
  (when (and isearch-regexp  isearchp-regexp-quote-yank-flag) (setq string  (regexp-quote string)))
  (setq isearch-yank-flag  t)           ; Don't move cursor in reverse search.
  (if (fboundp 'isearch-process-search-string) ; Emacs 24
      (isearch-process-search-string string (mapconcat 'isearch-text-char-description string ""))
    (setq isearch-string   (concat isearch-string string)
          isearch-message  (concat isearch-message (mapconcat 'isearch-text-char-description string
                                                              "")))
    (isearch-search-and-update)))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. Respect `isearchp-mouse-2-flag'.
;;
;; 2. Works for older Emacs versions too: Set X selection, so `x-get-selection' returns non-nil.
;;
(defun isearch-mouse-2 (click)          ; Bound to `mouse-2' in `isearch-mode-map'.
  "Handle `mouse-2' in Isearch mode.
If `isearchp-mouse-2-flag' is non-nil, yank the X selection.
If `isearchp-mouse-2-flag' is nil, yank it only if the `mouse-2' click
is in the echo area.  Otherwise, invoke whatever `mouse-2' is bound to
outside of Isearch."
  (interactive "e")
  ;; For both the nil and non-nil `isearchp-mouse-2-flag' cases we need to explicitly set the X
  ;; selection, otherwise things won't work for older Emacs versions and depending on your
  ;; platform.  If not for that need, in Emacs 24+ we could simply use this for the non-nil case,
  ;; and make no change at all for the nil case:
  ;;
  ;; (let ((select-active-regions  t))
  ;;   (deactivate-mark)
  ;;   (isearch-yank-x-selection))
  ;;
  (if (and isearchp-mouse-2-flag  (mark))
      (when (/= (region-beginning) (region-end)) (isearchp-set-sel-and-yank))
    (let ((win                            (posn-window (event-start click)))
          (overriding-terminal-local-map  nil)
          (binding                        (key-binding (this-command-keys-vector) t)))
      (if (and (window-minibuffer-p win)  (not (minibuffer-window-active-p win)) ; In echo area
               (mark))
          (isearchp-set-sel-and-yank)
        (when (functionp binding) (call-interactively binding))))))

(defun isearchp-set-sel-and-yank ()
  "Set X selection and yank it into echo area."
  (when (mark)
    (x-set-selection 'PRIMARY (buffer-substring-no-properties (region-beginning) (region-end)))
    (deactivate-mark)
    (isearch-yank-x-selection)))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; 1. If `isearchp-drop-mismatch' is `replace-last' then remove the last mismatched input.
;; 2. Use ?\ , not ?\s, so compatible with older Emacs versions.
;;
(if (or (> emacs-major-version 24)  (and (= emacs-major-version 24)
                                         (not (version< emacs-version "24.3.50"))))
    (defun isearch-printing-char (&optional char count)
      "Append ordinary printing character CHAR to the search string, then search.
CHAR defaults to the last printing character typed.
With a numeric prefix arg, append that many copies of CHAR."
      (interactive (list last-command-event (prefix-numeric-value current-prefix-arg)))
      (when (eq isearchp-drop-mismatch 'replace-last)
        (while (or (not isearch-success)  (if (boundp 'isearch-error) isearch-error isearch-invalid-regexp))
          (isearch-pop-state)))
      (let ((char  (or char  last-command-event)))
        (when (= char ?\S-\ ) (setq char  ?\  ))
        (if current-input-method
            (isearch-process-search-multibyte-characters char count)
          (isearch-process-search-char char count))))

  (defun isearch-printing-char ()       ; Emacs < 24.4
    "Add ordinary printing character to the search string, then search."
    (interactive)
    (when (eq isearchp-drop-mismatch 'replace-last)
      (while (or (not isearch-success)  (if (boundp 'isearch-error) isearch-error isearch-invalid-regexp))
        (isearch-pop-state)))
    (let ((char  last-command-event))
      (when (= char ?\S-\ ) (setq char  ?\  ))
      (if current-input-method
          (isearch-process-search-multibyte-characters char)
        (isearch-process-search-char char)))))



;; $$$$$$
;; (when (> emacs-major-version 21)        ; Emacs 22+
;;   (defun isearch-message (&optional c-q-hack ellipsis)
;;     ;; Generate and print the message string.
;;     (let ((cursor-in-echo-area ellipsis)
;;           (cmds isearch-cmds)
;;           succ-msg m)
;;       (while (not (isearch-success-state (car cmds))) (pop cmds))
;;       (setq succ-msg  (if (equal (isearch-message-state (car isearch-cmds)) isearch-message)
;;                           (and cmds  (isearch-message-state (car cmds)))
;;                         isearch-message))
;;       (setq m  (concat
;;                 (isearchp-message-prefix c-q-hack ellipsis isearch-nonincremental)
;;                 succ-msg
;;                 (and (not isearch-success)
;;                      (string-match (regexp-quote succ-msg) isearch-message)
;;                      (not (string= succ-msg isearch-message))
;;                      (propertize (substring isearch-message (match-end 0))
;;                                  'face 'isearch-fail))))
;;       (when (and (not isearch-success)  (string-match " +$" m))
;;         (put-text-property (match-beginning 0) (length m) 'face 'trailing-whitespace m))
;;       (setq m  (concat m (isearchp-message-suffix c-q-hack ellipsis)))
;;       (if c-q-hack m (let ((message-log-max nil)) (message "%s" m))))))



;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Highlight failed part of search string in echo area, in face `isearch-fail'.
;; (Same as what I added to vanilla Emacs 24+.)
;;
(when (or (= emacs-major-version 22)  (= emacs-major-version 23)) ; Emacs 22 & 23.
  (defun isearch-message (&optional c-q-hack ellipsis)
    ;; Generate and print the message string.
    (let ((cursor-in-echo-area  ellipsis)
          (msg                  isearch-message)
          (fail-pos             (isearch-fail-pos t)))
      ;; Highlight failed part
      (when fail-pos
        (setq msg  (copy-sequence msg))
        (add-text-properties fail-pos (length msg) '(face isearch-fail) msg)
        (when (string-match " +$" msg)  ; Highlight trailing whitespace
          (add-text-properties (match-beginning 0) (match-end 0) '(face trailing-whitespace) msg)))
      (setq msg  (concat (isearch-message-prefix c-q-hack ellipsis isearch-nonincremental)
                         msg
                         (isearch-message-suffix c-q-hack ellipsis)))
      (if c-q-hack  msg  (let ((message-log-max  nil)) (message "%s" msg)))))

  (defun isearch-fail-pos (&optional msg)
    "Return position of first mismatch in search string, or nil if none.
If MSG is non-nil, use `isearch-message', otherwise `isearch-string'."
    (let ((cmds      isearch-cmds)
          (curr-msg  (if msg isearch-message isearch-string))
          succ-msg)
      (when (or (not isearch-success)  isearch-error)
        (while (or (not (isearch-success-state (car cmds)))  (isearch-error-state (car cmds)))
          (pop cmds))
        (setq succ-msg  (and cmds  (if msg
                                       (isearch-message-state (car cmds))
                                     (isearch-string-state (car cmds)))))
        (if (and (stringp succ-msg)
                 (< (length succ-msg) (length curr-msg))
                 (equal succ-msg (substring curr-msg 0 (length succ-msg))))
            (length succ-msg)
          0)))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Highlight message according to search characteristics.
;;
(when (and (> emacs-major-version 21)   ; Emacs 22 through Emacs 24.2
           (or (< emacs-major-version 24)  (and (= emacs-major-version 24)  (< emacs-minor-version 3))))
  (defun isearch-message-prefix (&optional _c-q-hack ellipsis nonincremental)
    ;; If about to search, and previous search regexp was invalid, check that it still is.
    ;; If it is valid now, let the message we display while searching say that it is valid.
    (and isearch-error  ellipsis  (condition-case ()
                                      (progn (re-search-forward isearch-string (point) t)
                                             (setq isearch-error  nil))
                                    (error nil)))
    ;; If currently failing, display no ellipsis.
    (unless isearch-success (setq ellipsis  nil))
    (let ((m (concat (and (not isearch-success)  (propertize "failing " 'face 'minibuffer-prompt))
                     (and isearch-adjusted  (propertize "pending " 'face 'minibuffer-prompt))
                     (and isearch-wrapped
                          (not isearch-wrap-function)
                          (if isearch-forward (> (point) isearch-opoint) (< (point) isearch-opoint))
                          (propertize "over" 'face 'isearchp-wrapped))
                     (and isearch-wrapped  (propertize "wrapped " 'face 'isearchp-wrapped))
                     (and isearch-word  (propertize "word " 'face 'isearchp-word))
                     (and isearch-regexp  (propertize "regexp " 'face 'isearchp-regexp))
                     (and (boundp 'multi-isearch-next-buffer-current-function) ; Emacs 23+
                          multi-isearch-next-buffer-current-function
                          (propertize "multi " 'face 'isearchp-multi))
                     (and (boundp 'isearch-message-prefix-add) ; Emacs 23+
                          isearch-message-prefix-add
                          (propertize isearch-message-prefix-add 'face 'minibuffer-prompt))
                     (propertize (if nonincremental "search" "I-search") 'face 'minibuffer-prompt)
                     (and (not isearch-forward)  (propertize " backward" 'face 'minibuffer-prompt))
                     (propertize (if (and (boundp 'bidi-display-reordering) ; Emacs 24+
                                          current-input-method)
                                     ;; Input methods for RTL languages use RTL chars for their
                                     ;; title.  That messes up display of search text after prompt.
                                     (bidi-string-mark-left-to-right
                                      (concat " [" current-input-method-title "]: "))
                                   ": ")
                                 'face 'minibuffer-prompt))))
      (concat (upcase (substring m 0 1)) (substring m 1)))))


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Highlight message according to search characteristics.
;;
(when (and (> emacs-major-version 23)   ; Emacs 24.3+
           (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 2))))
  (defun isearch-message-prefix (&optional ellipsis nonincremental)
    ;; If about to search, and previous search regexp was invalid,
    ;; check that it still is.  If it is valid now,
    ;; let the message we display while searching say that it is valid.
    (and isearch-error  ellipsis  (condition-case ()
                                      (progn (re-search-forward isearch-string (point) t)
                                             (setq isearch-error  nil))
                                    (error nil)))
    ;; If currently failing, display no ellipsis.
    (unless isearch-success (setq ellipsis  nil))
    (let ((m (concat (and (not isearch-success)  (propertize "failing " 'face 'minibuffer-prompt))
                     (and isearch-adjusted  (propertize "pending " 'face 'minibuffer-prompt))
                     (and isearch-wrapped
                          (not isearch-wrap-function)
                          (if isearch-forward (> (point) isearch-opoint) (< (point) isearch-opoint))
                          (propertize "over" 'face 'isearchp-wrapped))
                     (and isearch-wrapped  (propertize "wrapped " 'face 'isearchp-wrapped))
                     (and isearch-word
                          (propertize (or (and (symbolp isearch-word)
                                               (get isearch-word 'isearch-message-prefix))
                                          "word ")
                                      'face 'isearchp-word))
                     (and isearch-regexp  (propertize "regexp " 'face 'isearchp-regexp))
                     (and multi-isearch-next-buffer-current-function
                          (propertize "multi " 'face 'isearchp-multi))
                     (and isearch-message-prefix-add
                          (propertize isearch-message-prefix-add 'face 'minibuffer-prompt))
                     (propertize (if nonincremental "search" "I-search") 'face 'minibuffer-prompt)
                     (and (not isearch-forward)  (propertize " backward" 'face 'minibuffer-prompt))
                     (propertize (if current-input-method
                                     ;; Input methods for RTL languages use RTL chars for their
                                     ;; title.  That messes up display of search text after prompt.
                                     (bidi-string-mark-left-to-right
                                      (concat " [" current-input-method-title "]: "))
                                   ": ")
                                 'face 'minibuffer-prompt))))
      (concat (upcase (substring m 0 1)) (substring m 1)))))

;;; Replacement on demand.  Emacs 22+
(when (> emacs-major-version 21)

  (defun isearchp-replace-on-demand ()
    "Replace current search hit by the value of `isearchp-replacement'.
This is the default value of `isearchp-on-demand-action-function'.
Isearch is not exited after replacing.

By default, `isearchp-replacement' is \"\", so with no prefix arg, by
default, this just deletes the search hit.

With a prefix arg, you are prompted for the replacement text, which
updates `isearchp-replacement'.  (If you clear the minibuffer and hit
`RET', then the search hit is just deleted.)

* A plain prefix arg (`C-u'), or `C-1', means replace only the current
  search hit.

* A negative prefix arg (e.g. `C--') makes search keys (e.g. `C-s')
  replace search hits you visit.  They thus act the same as `C-M-RET'.

* A positive prefix arg N means replace N search hits (but stop at the
  search limit).

* A zero prefix arg (e.g. `C-0') means replace *all* remaining search
  hits (up to the search limit).

To use this command with a prefix arg, you must set
 `isearch-allow-prefix' or `isearch-allow-scroll' to non-nil.

You can use `C-M-`' anytime during Isearch to toggle whether the
replacement text is taken literally or interpreted as using the
special regexp replacement constructs.  These are the same as in
`query-replace-regexp': `\&`, `\=\N', `\#', and `\,' (but not `\?')."
    (let ((numarg  (and isearchp-pref-arg  (prefix-numeric-value isearchp-pref-arg))))
      (when isearchp-pref-arg
        (when (consp isearchp-pref-arg) (setq numarg 1)) ; Treat plain `C-u' like `C-1'.
        (setq isearchp-replacement
              (read-string (format "%sReplacement: "
                                   (cond ((> numarg 0) "")
                                         ((zerop numarg) "Replace ALL, to the limit.  ")
                                         (t "Replace just by VISITING.  ")))
                           nil nil
                           (or (and (not (equal isearchp-replacement ""))  isearchp-replacement)
                               (match-string 0)))))
      (cond ((or (not isearchp-pref-arg) (= 1 numarg))
             (unless (eq this-command 'isearchp-act-on-demand) (setq replace-count  0))
             (isearchp-replace-match))
            ((natnump numarg)
             (isearchp-replace-multiple numarg))
            (t
             (setq replace-count  0)
             (setq isearchp-noprompt-action-function 'isearchp-replace-match))))
    (setq this-command  'isearchp-act-on-demand))

  ;; $$$$$$ TO DO: The cursor is left at the right place, but when resume search it resumes from the end
  ;;               of the last search hit (not the last replacement).
  (defun isearchp-replace-multiple (arg)
    "Replace ARG search hits, but stopping at the search limit.
If ARG is 0 then replace *all* remaining search hits, up to the limit."
    (let ((replace-count  0))
      (if (and (not (zerop arg))  (>= replace-count arg))
          (throw 'with-isearch-suspended (point))
        (while (and isearch-success  (or (zerop arg)  (< replace-count arg)))
          (isearchp-replace-match)
          (when (or (zerop arg)  (< replace-count arg))
            (isearch-resume isearch-string isearch-regexp isearch-word isearch-forward isearch-message
                            `,isearch-case-fold-search)))
        (throw 'with-isearch-suspended (point))))) ; If hit limit, put point at end of last hit.

  (defun isearchp-replace-match ()
    "Replace current match with `isearchp-replacement', interpreting escapes.
Treat the replacement string as does `query-replace-regexp'."
    (let ((compiled                      (save-match-data
                                           (query-replace-compile-replacement
                                            isearchp-replacement (not isearchp-replace-literally))))
          (enable-recursive-minibuffers  t)) ; So we can read input from \?.
      (condition-case isearchp-replace-match
          (replace-match-maybe-edit
           (if (consp compiled)
               (funcall (car compiled) (cdr compiled) (setq replace-count  (1+ replace-count)))
             compiled)
           (isearchp-replace-fixed-case-p (match-string 0)) isearchp-replace-literally nil (match-data))
        (wrong-number-of-arguments
         (condition-case isearchp-replace-match-2
             (replace-match-maybe-edit
              (if (consp compiled)
                  (funcall (car compiled) (cdr compiled) (setq replace-count  (1+ replace-count)))
                compiled)
              (isearchp-replace-fixed-case-p (match-string 0)) isearchp-replace-literally nil (match-data)
              nil)                      ; BACKWARD parameter for Emacs 24.4+ - see bug #18388
           (buffer-read-only (ding) (isearchp-user-error "Buffer is read-only"))
           (error (isearchp-user-error "No match for `%s'" isearchp-replacement))))
        (buffer-read-only (ding) (isearchp-user-error "Buffer is read-only"))
        (error (isearchp-user-error "No match for `%s'" isearchp-replacement)))))

  (defun isearchp-replace-fixed-case-p (from)
    "Return non-nil if FROM should be replaced without transferring case.
FROM is a string or nil.  If FROM is nil, then return nil.
Retuns non-nil if FROM is a string and one of the following holds:
 * FROM is not all lowercase
 * `case-replace' or `case-fold-search' is nil"
    (and from  (not (and case-fold-search  case-replace  (string= from (downcase from))))))

  (defun isearchp-toggle-literal-replacement () ; Bound to `C-M-`' in `isearch-mode-map'.
    "Toggle escaping of regexp special chars in replacement text.
This toggles variable `isearchp-replace-literally'.
Bound to `C-M-`' during Isearch."
    (interactive)
    (setq isearchp-replace-literally  (not isearchp-replace-literally))
    (message "Replacement of text literally is now %s" (if isearchp-replace-literally "ON" "OFF"))))


;;; Support for limiting search to active region.
;;;
(when (or (> emacs-major-version 24)    ; Emacs 24.3+
          (and (= emacs-major-version 24)  (> emacs-minor-version 2)))


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; 1. Pass `isearchp-reg-(beg|end)', not nil, as BOUND arg to `isearch-search-string'.
  ;; 2. Instead of testing bobp|eobp, test whether point is beyond `isearchp-reg-(beg|end)'.
  ;; 3. Added doc string.
  ;;
  (defun isearch-search ()
    "Search using the current search string."
    (if isearch-message-function (funcall isearch-message-function nil t) (isearch-message nil t))
    (when (and (eq isearch-case-fold-search t)  search-upper-case)
      (setq isearch-case-fold-search  (isearch-no-upper-case-p isearch-string isearch-regexp)))
    (condition-case lossage
        (let ((inhibit-point-motion-hooks  isearch-invisible)
              (inhibit-quit                nil)
              (case-fold-search            isearch-case-fold-search)
              (search-invisible            isearch-invisible)
              (retry                       t))
          (setq isearch-error  nil)
          (while retry
            (setq isearch-success  (isearch-search-string isearch-string (if isearch-forward
                                                                             isearchp-reg-end
                                                                           isearchp-reg-beg)
                                                          t))
            ;; Clear RETRY unless the search predicate says to skip this search hit.
            (when (or (not isearch-success)
                      (if isearch-forward
                          (or (eobp)  (and isearchp-reg-end  (> (point) isearchp-reg-end)))
                        (or (bobp)  (and isearchp-reg-beg  (< (point) isearchp-reg-beg))))
                      (= (match-beginning 0) (match-end 0))
                      (funcall isearch-filter-predicate (match-beginning 0) (match-end 0)))
              (setq retry  nil)))
          (setq isearch-just-started  nil)
          (when isearch-success
            (setq isearch-other-end  (if isearch-forward (match-beginning 0) (match-end 0)))))
      (quit           (isearch-unread ?\C-g) (setq isearch-success  nil))
      (invalid-regexp (setq isearch-error  (cadr lossage))
                      (cond ((string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid " isearch-error)
                             (setq isearch-error  "incomplete input"))
                            ((and (not isearch-regexp)
                                  (string-match "\\`Regular expression too big" isearch-error))
                             (cond (isearch-word (setq isearch-error  "Too many words"))
                                   ((and isearch-lax-whitespace  search-whitespace-regexp)
                                    (setq isearch-error  "Too many spaces for whitespace matching"))))))
      (search-failed  (setq isearch-success  nil
                            isearch-error    (nth 2 lossage)))
      (error          (setq isearch-error  (format "%s" lossage)))) ; Stack overflow in regexp search.
    (if isearch-success
        nil
      (and (isearch--state-success  (car isearch-cmds)) ; Failed this time after succeeding last time.
           (ding))
      (when (functionp (isearch--state-pop-fun (car isearch-cmds)))
        (funcall (isearch--state-pop-fun (car isearch-cmds)) (car isearch-cmds)))
      (goto-char (isearch--state-point (car isearch-cmds)))))


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; 1. Instead of just `isearch-success', test also that point is within `isearchp-reg-(beg|end)'.
  ;; 2. Goto char `isearchp-reg-(beg|end)', not `point-min|max'.
  ;; 3. Instead of testing bobp|eobp, test whether point is beyond `isearchp-reg-(beg|end)'.
  ;; 4. Added doc string.
  ;;
  (defun isearch-repeat (direction)
    "Utility for `isearch-repeat-forward' and `isearch-repeat--backward'."
    (if (eq isearch-forward (eq direction 'forward))
        (if (equal isearch-string "")   ; `C-s' in forward or `C-r' in reverse.
            ;; If search string is empty, use last one.
            (if (null (if isearch-regexp regexp-search-ring search-ring))
                (setq isearch-error  "No previous search string")
              (setq isearch-string            (car (if isearch-regexp regexp-search-ring search-ring))
                    isearch-message           (mapconcat 'isearch-text-char-description isearch-string "")
                    isearch-case-fold-search  isearch-last-case-fold-search)
              (isearch-ring-adjust1 nil)) ; After taking the last element, adjust ring to previous one.
          (or (and isearch-success      ; If already have what to search for, repeat it.
                   (if isearch-forward
                       (or (not isearchp-reg-end)  (<= (point) isearchp-reg-end))
                     (or (not isearchp-reg-beg)  (>= (point) isearchp-reg-beg))))
              (progn (setq isearch-wrapped  t) ; Set isearch-wrapped before calling isearch-wrap-function
                     (if isearch-wrap-function
                         (funcall isearch-wrap-function)
                       (goto-char (if isearch-forward
                                      (or isearchp-reg-beg  (point-min))
                                    (or isearchp-reg-end  (point-max))))))))
      (setq isearch-forward  (not isearch-forward) ; C-s in reverse or C-r in forward, change direction.
            isearch-success  t))
    (setq isearch-barrier  (point))     ; For subsequent \| if regexp.
    (if (equal isearch-string "")
        (setq isearch-success  t)
      (if (and isearch-success  (equal (point) isearch-other-end)  (not isearch-just-started))
          ;; If repeating a search that found an empty string, ensure that we advance.
          (if (if isearch-forward
                  (or (eobp)  (and isearchp-reg-end  (> (point) isearchp-reg-end)))
                (or (bobp)  (and isearchp-reg-beg  (< (point) isearchp-reg-beg))))
              ;; If there's nowhere to advance to, fail (and wrap next time).
              (progn (setq isearch-success  nil) (ding))
            (forward-char (if isearch-forward 1 -1))
            (isearch-search))
        (isearch-search)))
    (isearch-push-state)
    (isearch-update))


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; Use `isearchp-reg-(beg|end)', not point-min|max.
  ;;
  (defun isearch-lazy-highlight-search ()
    "Search ahead for the next or previous match, for lazy highlighting.
Attempt to do the search exactly the way the pending Isearch would."
    (condition-case nil
        (let ((case-fold-search               isearch-lazy-highlight-case-fold-search)
              (isearch-regexp                 isearch-lazy-highlight-regexp)
              (isearch-word                   isearch-lazy-highlight-word)
              (isearch-lax-whitespace         isearch-lazy-highlight-lax-whitespace)
              (isearch-regexp-lax-whitespace  isearch-lazy-highlight-regexp-lax-whitespace)
              (isearch-forward                isearch-lazy-highlight-forward)
              (search-invisible               nil) ; Do not match invisible text.
              (retry                          t)
              (success                        nil)
              (bound
               (if isearch-lazy-highlight-forward
                   (min (or isearch-lazy-highlight-end-limit  isearchp-reg-end  (point-max))
                        (if isearch-lazy-highlight-wrapped isearch-lazy-highlight-start (window-end)))
                 (max (or isearch-lazy-highlight-start-limit  isearchp-reg-beg  (point-min))
                      (if isearch-lazy-highlight-wrapped isearch-lazy-highlight-end (window-start))))))
          (while retry                  ; Use a loop, like in `isearch-search'.
            (setq success  (isearch-search-string isearch-lazy-highlight-last-string bound t))
            ;; Clear RETRY unless the search predicate says to skip this search hit.
            (if (or (not success)
                    (= (point) bound)   ; like (bobp) (eobp) in `isearch-search'.
                    (= (match-beginning 0) (match-end 0))
                    (funcall isearch-filter-predicate (match-beginning 0) (match-end 0)))
                (setq retry  nil)))
          success)
      (error nil)))


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; Use `isearchp-reg-(beg|end)', not point-min|max.
  ;;
  (defun isearch-lazy-highlight-update ()
    "Update highlighting of other matches for current search."
    (let ((max      lazy-highlight-max-at-a-time)
          (looping  t)
          nomore)
      (with-local-quit
        (save-selected-window
          (when (and (window-live-p isearch-lazy-highlight-window)
                     (not (eq (selected-window) isearch-lazy-highlight-window)))
            (select-window isearch-lazy-highlight-window))
          (save-excursion
            (save-match-data
              (goto-char (if isearch-lazy-highlight-forward
                             isearch-lazy-highlight-end
                           isearch-lazy-highlight-start))
              (while looping
                (let ((found  (isearch-lazy-highlight-search)))
                  (when max
                    (setq max  (1- max))
                    (when (<= max 0) (setq looping  nil)))
                  (when found
                    (let ((mb  (match-beginning 0))
                          (me  (match-end 0)))
                      (if (= mb me)     ; Zero-length match
                          (if isearch-lazy-highlight-forward
                              (if (= mb (if isearch-lazy-highlight-wrapped
                                            isearch-lazy-highlight-start
                                          (window-end)))
                                  (setq found  nil)
                                (forward-char 1))
                            (if (= mb (if isearch-lazy-highlight-wrapped
                                          isearch-lazy-highlight-end
                                        (window-start)))
                                (setq found  nil)
                              (forward-char -1)))
                        (let ((ov  (make-overlay mb me))) ; Non-zero-length match
                          (push ov isearch-lazy-highlight-overlays)
                          ;; 1000 is higher than ediff's 100+, but lower than isearch main overlay's 1001
                          (overlay-put ov 'priority 1000)
                          (overlay-put ov 'face lazy-highlight-face)
                          (overlay-put ov 'window (selected-window))))
                      ;; Remember current point for next call of `isearch-lazy-highlight-update' when
                      ;; `lazy-highlight-max-at-a-time' is too small.
                      (if isearch-lazy-highlight-forward
                          (setq isearch-lazy-highlight-end  (point))
                        (setq isearch-lazy-highlight-start  (point)))))
                  (unless found         ; Not found or zero-length match at the search bound
                    (if isearch-lazy-highlight-wrapped
                        (setq looping  nil
                              nomore   t)
                      (setq isearch-lazy-highlight-wrapped  t)
                      (if isearch-lazy-highlight-forward
                          (progn (setq isearch-lazy-highlight-end  (window-start))
                                 (goto-char
                                  (max (or isearch-lazy-highlight-start-limit  isearchp-reg-beg  (point-min))
                                       (window-start))))
                        (setq isearch-lazy-highlight-start  (window-end))
                        (goto-char (min (or isearch-lazy-highlight-end-limit  isearchp-reg-end  (point-max))
                                        (window-end))))))))
              (unless nomore
                (setq isearch-lazy-highlight-timer  (run-at-time lazy-highlight-interval nil
                                                                 'isearch-lazy-highlight-update)))))))))

  )

(defun isearchp-read-face-names  (&optional empty-means-none-p only-one-p)
  "Read face names with completion, and return a list of their symbols.
If user hits `RET' with empty input immediately, then include all
faces.  Otherwise, read faces one by one, until user hits `RET' twice
consecutively.

Non-nil optional arg EMPTY-MEANS-NONE-P means return nil (no face
names) for empty user input.

Non-nil optional arg ONLY-ONE-P means read only one face name and
return its symbol."
  (let ((icicle-multi-completing-p                   t)
        (icicle-list-nth-parts-join-string           ": ")
        (icicle-list-join-string                     ": ")
        (icicle-list-use-nth-parts                   '(1))
        (icicle-proxy-candidates
         (and (boundp 'icicle-add-proxy-candidates-flag)  icicle-add-proxy-candidates-flag
              (append (and (fboundp 'eyedrop-face-at-point)  (list "*point face name*"))
                      (let ((ipc  ()))
                        (mapatoms
                         (lambda (cand)
                           (when (and (user-variable-p cand)  (eq (get cand 'custom-type) 'face))
                             (push `,(concat "'" (symbol-name cand) "'") ipc))))
                        ipc))))
        (face-cands                                  (mapcar
                                                      (if (and (boundp 'icicle-mode)  icicle-mode)
                                                          #'icicle-make-face-candidate
                                                        (lambda (face) (list (symbol-name face))))
                                                      (face-list)))
        (faces                                       ())
        (prompt1                                     "Face (RET for each, empty input to finish): ")
        (prompt2                                     "Face: ")
        (icicle-unpropertize-completion-result-flag  t)
        face)
    (when (and (boundp 'icicle-mode)  icicle-mode)
      (put-text-property 0 1 'icicle-fancy-candidates t prompt1)
      (put-text-property 0 1 'icicle-fancy-candidates t prompt2))
    (setq face  (isearchp-read-face-names--read prompt1 face-cands))
    (if (and empty-means-none-p  (string= "" face))
        ()
      (if only-one-p
          face
        (if (string= "" face)
            (setq faces  (face-list))
          (setq face-cands  (delete (assoc face face-cands) face-cands))
          (while (not (string= "" face))
            (add-to-list 'faces (intern face))
            (setq face        (isearchp-read-face-names--read prompt2 face-cands)
                  face-cands  (delete (assoc face face-cands) face-cands)))
          (nreverse faces))))))

(defun isearchp-read-face-names--read (prompt candidates)
  "Read a face name using PROMPT and face-name completion CANDIDATES."
  (if (and (boundp 'icicle-mode)  icicle-mode)
      (icicle-transform-multi-completion
       (completing-read
        prompt candidates nil (not (stringp icicle-WYSIWYG-Completions-flag)) nil
        (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history)))
    (completing-read prompt candidates nil t nil 'face-name-history)))

(defun isearchp-read-sexps  (&optional only-one-p)
  "Read sexps with completion, and return them as a list.
Read sexps one by one, until user hits `RET' twice consecutively.
Non-nil ONLY-ONE-P means read only one sexp and return it."
  (let ((sexp-cands                         (mapcar #'list (isearchp-remove-duplicates
                                                            read-expression-history)))
        (sexps                              ())
        (prompt1                            "Sexp (RET for each, empty input to finish): ")
        (prompt2                            "Sexp: ")
        sexp)
    (setq sexp        (completing-read (if only-one-p prompt2 prompt1) sexp-cands
                                       nil nil nil 'read-expression-history)
          sexp-cands  (delete (assoc sexp sexp-cands) sexp-cands))
    (if only-one-p
        (car (read-from-string sexp))
      (while (not (string= "" sexp))
        (add-to-list 'sexps sexp)
        (setq sexp        (completing-read prompt2 sexp-cands nil nil nil 'read-expression-history)
              sexp-cands  (delete (assoc sexp sexp-cands) sexp-cands)))
      (prog1 (setq sexps  (nreverse (delete "" sexps)) ; Return the list of sexps.
                   sexps  (mapcar (lambda (sx) (car (read-from-string sx))) sexps))
        (when (interactive-p) (message "Sexps: %S" sexps))))))

(when (and (fboundp 'cl-puthash)  (not (fboundp 'puthash))) ; Emacs 20 with `cl-extra.el' loaded.
  (defalias 'puthash 'cl-puthash))

;; Same as `icicle-remove-duplicates'.
(if (fboundp 'puthash)                  ; Emacs 21+, or Emacs 20 with `cl-extra.el' loaded.
    (defun isearchp-remove-duplicates (sequence &optional test)
      "Copy of SEQUENCE with duplicate elements removed.
Optional arg TEST is the test function.  If nil, test with `equal'.
See `make-hash-table' for possible values of TEST."
      (setq test  (or test  #'equal))
      (let ((htable  (make-hash-table :test test)))
        (loop for elt in sequence
              unless (gethash elt htable)
              do     (puthash elt elt htable)
              finally return (loop for i being the hash-values in htable collect i))))

  (defun isearchp-remove-duplicates (list &optional use-eq)
    "Copy of LIST with duplicate elements removed.
Test using `equal' by default, or `eq' if optional USE-EQ is non-nil."
    (let ((tail  list)
          new)
      (while tail
        (unless (if use-eq (memq (car tail) new) (member (car tail) new))
          (push (car tail) new))
        (pop tail))
      (nreverse new))))

(defun isearchp-repeat-command (command)
  "Repeat COMMAND."
  (let ((repeat-message-function  'ignore))
    (setq last-repeatable-command  command)
    (repeat nil)))
 
(defadvice isearch-forward (before isearch+-doc activate)
  "
Isearch Plus
============

Options
-------
`isearchp-case-fold'\t- search is case sensitive?
`isearchp-set-region-flag'\t- select last search target?
`isearchp-restrict-to-region-flag'\t- restrict search to region?
`isearchp-deactivate-region-flag'\t- search deactivates region?
`isearchp-ignore-comments-flag'\t- ignore THINGs in comments? [*]
`isearchp-mouse-2-flag'\t- `mouse-2' anywhere yanks the selection?
`isearchp-regexp-quote-yank-flag'\t- regexp-quote yanked text?
`isearchp-toggle-option-flag'\t- toggling toggles options too?
`isearchp-drop-mismatch'\t- handling input after search mismatch
`isearchp-initiate-edit-commands'\t- keys that edit, not exit

 [*] Requires library `isearch-prop.el'.

Commands
--------
\\<isearch-mode-map>\\[isearchp-open-recursive-edit]\t- \
invoke Emacs command loop recursively, during Isearch

\\[isearchp-kill-ring-save]\t- copy current search string to kill ring
\\[isearchp-yank-char]\t- yank a char from buffer onto search string
\\[isearchp-yank-word-or-char]\t- yank a word or char from buffer onto search string
\\[isearchp-yank-symbol-or-char]\t- yank a symbol or char from buffer onto search string
\\[isearchp-yank-sexp-symbol-or-char]\t- yank sexp, symbol, or char from buffer onto search string
\\[isearchp-yank-line]\t- yank text up to end of line onto search string
\\[isearchp-retrieve-last-quit-search]\t- insert successful search string from when you hit `C-g'
\\[isearch-char-by-name]\t- add a Unicode char to search string by Unicode name

\\[isearchp-cycle-mismatch-removal]\t- cycle option `isearchp-drop-mismatch'
\\[isearch-toggle-case-fold]\t- toggle case-sensitivity (for current search or more: `C-u')
\\[isearchp-toggle-search-invisible]\t- toggle searching invisible text
\\[isearch-toggle-invisible]\t- toggle searching invisible text, for current search or more
\\[isearchp-toggle-option-toggle]\t- toggle option `isearchp-toggle-option-flag'
\\[isearchp-toggle-region-restriction]\t- toggle restricting search to active region
\\[isearchp-toggle-set-region]\t- toggle setting region around search target
\\[isearchp-toggle-regexp-quote-yank]\t- toggle quoting (escaping) of regexp special characters

Commands that Require Library `isearch-prop.el'
----------------------------------------------- 

\\[isearchp-property-forward]\t- search for a character (overlay or text) property
\\[isearchp-property-forward-regexp]\t- regexp-search for a character (overlay or text) property
\\[isearchp-toggle-complementing-domain]\t- toggle searching complements of normal search contexts
\\[isearchp-toggle-ignoring-comments]\t- toggle ignoring comments for `isearchp-thing'
\\[isearchp-hide/show-comments]\t- hide or (`C-u') show comments

Other Isearch+ Commands
-----------------------
\\[isearchp-fontify-buffer-now]\t- fontify whole buffer
\\[isearchp-set-region-around-search-target]\t- select last search

Other Isearch+ Commands that Require Library `isearch-prop.el'
--------------------------------------------------------------

\\[isearchp-put-prop-on-region]\t- add a text property to region
\\[isearchp-add-regexp-as-property]\t- add prop to regexp matches
\\[isearchp-regexp-context-search]\t- search regexp contexts
\\[isearchp-regexp-define-contexts]\t- define regexp contexts

\\[isearchp-imenu] \t- search Emacs-Lisp definitions
\\[isearchp-imenu-command] \t- search Emacs command definitions
\\[isearchp-imenu-non-interactive-function] \t- search non-commands
\\[isearchp-imenu-macro] \t- search Emacs-Lisp macro definitions

\\[isearchp-thing]\t- search THING search contexts
\\[isearchp-thing-define-contexts]\t- define THING contexts
\\[isearchp-previous-visible-thing]\t- go to previous visible THING
\\[isearchp-next-visible-thing]\t- go to next visible THING")
 
;;(@* "Keys and Hooks")

;;; Keys and Hooks ---------------------------------------------------

(define-key isearch-mode-map [mouse-2]            'isearch-mouse-2)
;; Must not be just `nil'.  Need to override a global binding such as `mouse-flash-position-or-M-x'.
(define-key isearch-mode-map [down-mouse-2]       'ignore)

;; Must not be just `nil'.  Otherwise, if click `mouse-2' in a standalone minibuffer frame then
;; the `switch-frame' event exits Isearch and the following `down-mouse-2' invokes, e.g.,
;; `mouse-flash-position-or-M-x'.
(define-key isearch-mode-map [switch-frame]       'ignore)

;;; Use this instead of `ignore' for `switch-frame', if you want it to exit Isearch when you switch
;;; to any frame other than a standalone minibuffer frame.
;;; (defun isearchp-switch-frame-or-exit ()
;;;   "Return nil if switch to minibuffer frame.  Else exit Isearch.
;;; Bind to `switch-frame' event."
;;;   (interactive)
;;;   (let* ((vec   (this-command-keys-vector))
;;;          (evnt  (aref vec 0)))
;;;     (unless (and (consp evnt)  (eq 'switch-frame (car evnt))
;;;                  (cadr evnt)   (window-minibuffer-p (frame-selected-window (cadr evnt))))
;;;       (isearch-done)
;;;       (isearch-clean-overlays))))

;;; (define-key isearch-mode-map [switch-frame]    'isearchp-switch-frame-or-exit)

(define-key isearch-mode-map [(control ?+)]       'isearchp-toggle-search-invisible)
(define-key isearch-mode-map [(control ?`)]       'isearchp-toggle-regexp-quote-yank)
(define-key isearch-mode-map [(control ? )]       'isearchp-toggle-set-region)
(define-key isearch-mode-map "\C-h"               'isearch-mode-help)

;; An alternative to binding `isearch-edit-string' (but less flexible):
;; (setq search-exit-option  'edit) ; M- = edit search string, not exit.

(when (fboundp 'isearchp-eval-sexp-and-insert)
  (define-key isearch-mode-map "\M-:"             'isearchp-eval-sexp-and-insert))
(define-key isearch-mode-map (kbd "C-M-`")        'isearchp-toggle-literal-replacement)
(define-key isearch-mode-map "\M-c"               'isearch-toggle-case-fold)
;; This one is needed only for Emacs 20.  It is automatic after release 20.
(define-key isearch-mode-map "\M-e"               'isearch-edit-string)
(define-key isearch-mode-map "\M-g"               'isearchp-retrieve-last-quit-search)
(define-key isearch-mode-map "\M-k"               'isearchp-cycle-mismatch-removal)
;; This one is needed only for Emacs 20.  It is automatic after release 20.
(define-key isearch-mode-map "\M-r"               'isearch-toggle-regexp)
(define-key isearch-mode-map "\M-si"              'isearch-toggle-invisible)
(define-key isearch-mode-map "\M-sv"              'isearchp-toggle-option-toggle)
(when (< emacs-major-version 23)
  (define-key isearch-mode-map "\M-sw"            'isearch-toggle-word))
(define-key isearch-mode-map "\M-w"               'isearchp-kill-ring-save)
(when (fboundp 'isearch-yank-internal)
  (define-key isearch-mode-map "\C-z"             'isearchp-yank-char)
  (define-key isearch-mode-map "\C-_"             'isearchp-yank-symbol-or-char)
  (define-key isearch-mode-map [(control ?\()]    'isearchp-yank-sexp-symbol-or-char))
(when (and (fboundp 'goto-longest-line)  window-system) ; Defined in `misc-cmds.el'
  (define-key isearch-mode-map [(control end)]    'goto-longest-line))
(define-key isearch-mode-map [next]               'isearch-repeat-forward)
(define-key isearch-mode-map [prior]              'isearch-repeat-backward)
(when (and (eq system-type 'windows-nt) ; Windows uses M-TAB for something else.
           (not (lookup-key isearch-mode-map [C-M-tab])))
  (define-key isearch-mode-map [C-M-tab]          'isearch-complete))
(when (> emacs-major-version 21)
  (define-key isearch-mode-map "\C-x"             nil)
  (when (or (> emacs-major-version 24)  ; Emacs 24.3+
            (and (= emacs-major-version 24)  (> emacs-minor-version 2)))
    (define-key isearch-mode-map "\C-xn"          'isearchp-toggle-region-restriction)) ; `n'arrow to region
  (define-key isearch-mode-map "\C-xo"            'isearchp-open-recursive-edit) ; `o'pen edit session
  ;; Do this even for Emacs 24.4+ (where it is true by default), because we set `C-x' to nil.
  (when (> emacs-major-version 22)      ; Emacs 23+ (supports Unicode)
    (define-key isearch-mode-map "\C-x8"          nil)
    (define-key isearch-mode-map "\C-x8\r"        'isearch-char-by-name)))

(define-key isearch-mode-map "\C-xrg" 'isearchp-append-register)

(define-key isearch-mode-map "\C-y"               nil) ; Put all yanking commands on prefix `C-y'.
(when (fboundp 'isearch-yank-internal)
  (define-key isearch-mode-map (kbd "C-y C-c")    'isearchp-yank-char)
  (define-key isearch-mode-map (kbd "C-y C-e")    'isearchp-yank-line)
  (define-key isearch-mode-map (kbd "C-y C-w")    'isearchp-yank-word-or-char)
  (define-key isearch-mode-map (kbd "C-y C-_")    'isearchp-yank-symbol-or-char)
  (define-key isearch-mode-map (kbd "C-y C-(")    'isearchp-yank-sexp-symbol-or-char))
(eval-after-load "second-sel"
  '(progn
    (define-key isearch-mode-map (kbd "C-y C-2")  'isearch-yank-secondary)
    (define-key isearch-mode-map (kbd "C-M-y")    'isearch-yank-secondary)))
(define-key isearch-mode-map "\C-y\C-y"           'isearch-yank-kill)
(define-key isearch-mode-map "\C-y\M-g"           'isearchp-retrieve-last-quit-search)
(when (fboundp 'isearch-yank-pop)
  (define-key isearch-mode-map "\C-y\M-y"         'isearch-yank-pop)) ; It is also just `M-y'.

(when (fboundp 'isearchp-act-on-demand) ; Emacs 22+
  (define-key isearch-mode-map (kbd "C-M-<return>") 'isearchp-act-on-demand))

(when (fboundp 'isearchp-remove-failed-part) ; Emacs 22+
  (define-key isearch-mode-map (kbd "C-M-l")      'isearchp-remove-failed-part))

(when (and (eq system-type 'windows-nt) ; Windows uses M-TAB for something else.
           (not (lookup-key minibuffer-local-isearch-map [C-M-tab])))
  (define-key minibuffer-local-isearch-map [C-M-tab] 'isearch-complete-edit))
(when (> emacs-major-version 22)
  (define-key minibuffer-local-isearch-map "\C-x8\r" 'insert-char))

(defun isearchp-set-region ()
  "Set the region around the search target, if `isearchp-set-region-flag'.
This is used only for Transient Mark mode."
  (when (and isearchp-set-region-flag  transient-mark-mode)
    (push-mark isearch-other-end t 'activate)))

(add-hook 'isearch-mode-end-hook 'isearchp-set-region)

(defun isearchp-highlight-lighter ()
  "Update minor-mode mode-line lighter to reflect case sensitivity."
  (let ((case-fold-search  isearch-case-fold-search))
    (when (and (eq case-fold-search t)  search-upper-case)
      (setq case-fold-search  (isearch-no-upper-case-p isearch-string isearch-regexp)))
    ;; Vanilla Isearch uses the symbol `isearch-mode', hence the first of these.
    (setq minor-mode-alist  (delete '(isearch-mode isearch-mode) minor-mode-alist)
          minor-mode-alist  (delete '(isearch-mode " ISEARCH")   minor-mode-alist)
          minor-mode-alist  (delete '(isearch-mode " Isearch")   minor-mode-alist))
    (let ((lighter  (if case-fold-search " ISEARCH" " Isearch")))
      (add-to-list
       'minor-mode-alist
       `(isearch-mode ,(if (and isearch-wrapped  (facep 'isearchp-wrapped)) ;Emacs 22+
                           (propertize lighter 'face 'isearchp-wrapped)
                           lighter)))))
  (condition-case nil
      (if (fboundp 'redisplay) (redisplay t) (force-mode-line-update t))
    (error nil)))

(add-hook 'isearch-update-post-hook 'isearchp-highlight-lighter)

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'isearch+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch+.el ends here
