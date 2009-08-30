;;; icicles-xmas.el --- xemacs port (beta) of icicles.el


;;; icicles.el --- Minibuffer input completion and cycling.
;;
;; Filename: icicles.el
;; Description: Minibuffer completion and cycling.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2006, Drew Adams, all rights reserved.
;; Created: Tue Aug  1 14:21:16 1995
;; Version: 22.0
;; Last-Updated: Sat Feb 25 21:55:28 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 15616
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `bookmark', `cl', `color-theme',
;;   `cus-face', `easymenu', `hexrgb', `misc-fns', `pp', `recentf',
;;   `thingatpt', `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Minibuffer input completion and cycling of completion candidates.
;;
;;  Input completion takes as input a string and returns a name that
;;  contains the input string.  This library enables minibuffer
;;  cycling of completion candidates, and provides additional support
;;  for input completion.
;;
;;  Two kinds of completion are offered here, which are distinguished
;;  by how the input string is matched against the completed name:
;;
;;   - Prefix completion - The input string is a prefix of the
;;                         completed name.  This is the usual Emacs
;;                         completion.
;;
;;   - Apropos completion - The input string is a regular expression
;;                          that matches somewhere (anywhere) within
;;                          the completed name.  You can think of the
;;                          name as having been returned by `apropos'
;;                          (except it also works for file and buffer
;;                          names).
;;
;;  See also: Library `icicles-menu.el', which lets you execute menu
;;            commands, cycling and completing them.  It enhances the
;;            power of library `icicles.el'.
;;
;;
;;  To use this library:
;;
;;    Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;      (require 'icicles) ; Load this library.
;;
;;    In Emacs 21 and later, Icicle mode will automatically be turned
;;    on when the library is loaded.  You can prevent that, if you
;;    wish, by setting option `icicle-mode' to nil in your init file,
;;    before loading the library:
;;
;;      (setq icicle-mode nil) ; Prevent turning on Icicle mode.
;;      (require 'icicles)
;;
;;    Alternatively, you can customize option `icicle-mode' to nil.
;;
;;    In Emacs 20, Icicle mode is off, by default, after you load the
;;    library.  You can explicitly turn it on in your init file.
;;
;;      (require 'icicles)
;;      (icicle-mode 1)    ; Turn on Icicle mode.
;;
;;    After startup, you can turn Icicle mode on or off at any time
;;    interactively, using command `icy-mode' (aka `icicle-mode' -
;;    prefix `icy' is unique to this command, so it is easier to
;;    complete).
;;
;;    Note: If you turn on Icicle mode in your init file, it's best to
;;          do that as late as possible - after you or any libraries
;;          that you load do any key binding.  This is because Icicles
;;          uses the current global key bindings to determine which
;;          keys to bind for minibuffer completion and cycling.  To
;;          pick up the latest bindings at any time, you can of course
;;          enter Icicle mode interactively using command `icy-mode'
;;          (if necessary, exit, then re-enter).

;;
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Key bindings defined here: see "Key Bindings", below.
;;
;;  Macros defined here:
;;
;;    `icicle-define-command', `icicle-define-file-command'.
;; 
;;  Commands defined here -
;;
;;   Commands to be used mainly at top level:
;;
;;    `icicle-add-buffer-candidate', `icicle-add-buffer-config',
;;    `icicle-apropos', `icicle-apropos-command',
;;    `icicle-apropos-function', `icicle-apropos-option',
;;    `icicle-apropos-variable', `icicle-apropos-zippy',
;;    `icicle-bookmark', `icicle-buffer', `icicle-buffer-config',
;;    `icicle-buffer-list', `icicle-buffer-other-window',
;;    `icicle-color-theme', `icicle-compilation-search',
;;    `icicle-clear-option', `icicle-dabbrev-completion',
;;    `icicle-delete-file', `icicle-execute-extended-command',
;;    `icicle-find-file', `icicle-find-file-other-window',
;;    `icicle-font', `icicle-frame-bg', `icicle-frame-fg',
;;    `icicle-history', `icicle-lisp-complete-symbol', `icicle-mode',
;;    `icy-mode', `icicle-recent-file',
;;    `icicle-recent-file-other-window',
;;    `icicle-remove-buffer-candidate', `icicle-remove-buffer-config',
;;    `icicle-reset-option-to-nil', `icicle-search',
;;    `icicle-set-option-to-t', `icicle-toggle-ignored-extensions',
;;    `icicle-toggle-sorting', `toggle-icicle-ignored-extensions',
;;    `toggle-icicle-sorting',
;;
;;   Commands to be used mainly in the minibuffer or *Completions*:
;; 
;;    `icicle-abort-minibuffer-input', `icicle-apropos-complete',
;;    `icicle-apropos-complete-and-exit',
;;    `icicle-backward-delete-char-untabify',
;;    `icicle-backward-kill-paragraph',
;;    `icicle-backward-kill-sentence', `icicle-backward-kill-sexp',
;;    `icicle-backward-kill-word', `icicle-candidate-action',
;;    `icicle-candidate-set-complement',
;;    `icicle-candidate-set-define',
;;    `icicle-candidate-set-difference',
;;    `icicle-candidate-set-intersection',
;;    `icicle-candidate-set-retrieve', `icicle-candidate-set-save',
;;    `icicle-candidate-set-swap', `icicle-candidate-set-union',
;;    `icicle-choose-completion-string', `icicle-completing-read',
;;    `icicle-completion-help', `icicle-customize-apropos',
;;    `icicle-customize-apropos-faces',
;;    `icicle-customize-apropos-groups',
;;    `icicle-customize-apropos-options',
;;    `icicle-delete-backward-char', `icicle-delete-windows-on',
;;    `icicle-doc', `icicle-erase-minibuffer',
;;    `icicle-exit-minibuffer', `icicle-fundoc',
;;    `icicle-help-on-candidate', `icicle-insert-string-at-point',
;;    `icicle-isearch-complete', `icicle-keep-only-past-inputs',
;;    `icicle-kill-line', `icicle-kill-paragraph',
;;    `icicle-kill-region', `icicle-kill-region-wimpy',
;;    `icicle-kill-sentence', `icicle-kill-sexp', `icicle-kill-word',
;;    `icicle-minibuffer-complete-and-exit',
;;    `icicle-mouse-candidate-action',
;;    `icicle-mouse-choose-completion',
;;    `icicle-move-to-next-completion',
;;    `icicle-move-to-previous-completion',
;;    `icicle-narrow-candidates', `icicle-next-apropos-candidate',
;;    `icicle-next-apropos-candidate-action', `icicle-next-line',
;;    `icicle-next-prefix-candidate',
;;    `icicle-next-prefix-candidate-action', `icicle-prefix-complete',
;;    `icicle-previous-apropos-candidate',
;;    `icicle-previous-apropos-candidate-action',
;;    `icicle-previous-line', `icicle-previous-prefix-candidate',
;;    `icicle-previous-prefix-candidate-action',
;;    `icicle-repeat-complex-command', `icicle-retrieve-last-input',
;;    `icicle-self-insert', `icicle-switch-to-Completions-buf',
;;    `icicle-switch-to-completions', `icicle-switch-to-minibuffer',
;;    `icicle-transpose-chars', `icicle-transpose-sexps',
;;    `icicle-transpose-words', `icicle-vardoc', `icicle-yank',
;;    `icicle-yank-pop', `old-completing-read',
;;    `old-choose-completion-string', `old-completion-setup-function',
;;    `old-exit-minibuffer', `old-minibuffer-complete-and-exit',
;;    `old-read-file-name', `old-switch-to-completions'.
;;
;;  Faces defined here (in Custom group `icicles'):
;;
;;    `icicle-complete-input', `icicle-historical-candidate',
;;    `icicle-prompt-suffix', `icicle-root-highlight-Completions',
;;    `icicle-root-highlight-minibuffer'.
;;
;;  User options defined here (in Custom group `icicles'):
;;
;;    `icicle-buffer-extras', `icicle-buffer-match-regexp',
;;    `icicle-buffer-no-match-regexp', `icicle-buffer-predicate',
;;    `icicle-buffer-require-match-flag' `icicle-buffer-sort',
;;    `icicle-change-region-background-flag',
;;    `icicle-completion-nospace-flag',
;;    `icicle-cycle-into-subdirs-flag',
;;    `icicle-default-thing-insertion'
;;    `icicle-incremental-completion-flag',
;;    `icicle-inhibit-reminder-prompt-flag', `icicle-init-value-flag',
;;    `icicle-list-join-string', `icicle-sort-function',
;;    `icicle-mark-position-in-candidate',
;;    `icicle-minibuffer-setup-hook', `icicle-mode',
;;    `icicle-point-position-in-candidate',
;;    `icicle-redefine-standard-commands-flag',
;;    `icicle-regexp-search-ring-max', `icicle-region-background',
;;    `icicle-require-match-flag', `icicle-search-ring-max',
;;    `icicle-show-Completions-initially-flag',
;;    `icicle-thing-at-point-functions', `icicle-word-completion-key'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-activate-mark', `icicle-apropos-candidates',
;;    `icicle-apropos-complete-1', `icicle-binary-option-p',
;;    `icicle-bind-completion-keys', `icicle-bind-isearch-keys',
;;    `icicle-buffer-sort-*...*-last', `icicle-candidate-set-1',
;;    `icicle-cancel-*Help*-redirection', `icicle-completing-p',
;;    `icicle-completion-setup-function',
;;    `icicle-current-completion-in-Completions',
;;    `icicle-delete-file-or-directory', `icicle-delete-if',
;;    `icicle-delete-if-not', `icicle-display-Completions',
;;    `icicle-display-candidates-in-Completions',
;;    `icicle-execute-extended-command-1', `icicle-file-directory-p',
;;    `icicle-file-name-apropos-candidates',
;;    `icicle-file-name-directory-w-default',
;;    `icicle-file-name-input-p', `icicle-file-name-nondirectory',
;;    `icicle-file-name-prefix-candidates', `icicle-filter-alist',
;;    `icicle-filter-wo-input', `icicle-frames-on',
;;    `icicle-highlight-complete-input',
;;    `icicle-increment-cand-nb+signal-end',
;;    `icicle-increment-color-hue', `icicle-increment-color-value',
;;    `icicle-insert-thing', `icicle-isearch-resume',
;;    `icicle-minibuffer-contents', `icicle-minibuffer-prompt-end',
;;    `icicle-minibuffer-setup', `icicle-msg-maybe-in-minibuffer',
;;    `icicle-nb-of-candidate-in-Completions',
;;    `icicle-next-candidate', `icicle-place-cursor',
;;    `icicle-place-overlay', `icicle-prefix-candidates',
;;    `icicle-read-file-name', `icicle-read-from-minibuffer',
;;    `icicle-read-from-minibuf-nil-default', `icicle-read-string',
;;    `icicle-rebind-completion-maps', `icicle-recompute-candidates',
;;    `icicle-redefine-standard-commands',
;;    `icicle-redefine-std-completion-fns',
;;    `icicle-redefine-standard-options', `icicle-remap',
;;    `icicle-remove-dots', `icicle-remove-duplicates',
;;    `icicle-remove-property', `icicle-reset-icicle-completing-p',
;;    `icicle-restore-completion-keys', `icicle-restore-region-face',
;;    `icicle-restore-standard-commands',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-restore-standard-options',
;;    `icicle-run-icicle-post-command-hook',
;;    `icicle-run-icicle-pre-command-hook',
;;    `icicle-save-or-restore-input',
;;    `icicle-scroll-or-update-Completions',
;;    `icicle-select-minibuffer-contents' `icicle-set-calling-cmd',
;;    `icicle-set-difference', `icicle-set-intersection',
;;    `icicle-set-union', `icicle-sort-and-strip-ignored',
;;    `icicle-sort-case-insensitively', `icicle-sort-dirs-last',
;;    `icicle-undo-std-completion-faces', `icicle-unmap',
;;    `icicle-unsorted-apropos-candidates',
;;    `icicle-unsorted-file-name-apropos-candidates',
;;    `icicle-unsorted-file-name-prefix-candidates',
;;    `icicle-unsorted-prefix-candidates',
;;    `icicle-update-completions',
;;    `icicle-update-ignored-extensions-regexp'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-candidate-nb', `icicle-cmd-calling-for-completion',
;;    `icicle-complete-input-overlay', `icicle-completion-candidates'
;;    `icicle-completion-help-string',
;;    `icicle-current-completion-candidate-overlay',
;;    `icicle-current-input', `icicle-default-directory',
;;    `icicle-default-thing-insertion-flipped-p',
;;    `icicle-extra-candidates', `icicle-icicle-completing-p',
;;    `icicle-icompleting-p', `icicle-ignored-extensions',
;;    `icicle-ignored-extensions-regexp',
;;    `icicle-incremental-completion-p', `icicle-initial-value',
;;    `icicle-insert-string-at-pt-end',
;;    `icicle-insert-string-at-pt-start',
;;    `icicle-last-completion-candidate',
;;    `icicle-last-completion-command', `icicle-last-input',
;;    `icicle-last-sort-function', `icicle-menu-items-alist',
;;    `icicle-mode-map', `icicle-must-match-regexp',
;;    `icicle-must-not-match-regexp', `icicle-must-pass-predicate',
;;    `icicle-nb-of-other-cycle-candidates',
;;    `icicle-post-command-hook', `icicle-pre-command-hook',
;;    `icicle-prompt', `icicle-prompt-suffix',
;;    `icicle-saved-completion-candidates',
;;    `icicle-saved-ignored-extensions',
;;    `icicle-saved-regexp-search-ring-max',
;;    `icicle-saved-region-background',
;;    `icicle-saved-search-ring-max', `icicle-successive-grab-count',
;;    `icicle-thing-at-pt-fns-pointer'.
;;
;;  Emacs functions defined here for older Emacs versions.
;;
;;    `select-frame-set-input-focus'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `completing-read'              - (See below and doc string.)
;;  `exit-minibuffer'              - Remove *Completion* window.
;;  `minibuffer-complete-and-exit' - Remove *Completion* window.
;;  `read-file-name'               - (See below and doc string.)
;;  `read-from-minibuffer'         - (See below and doc string.)
;;  `read-string'                  - (See below and doc string.)
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have
;;              been REDEFINED HERE:
;;
;;  `choose-completion-string' -
;;     Don't exit minibuffer after `lisp-complete-symbol' completion.
;;  `completion-setup-function' - 1. Put faces on inserted string(s).
;;                                2. Help on help.
;;  `switch-to-completions' - Always selects *Completions* window.
;;
;;  `next-history-element' (advised only) - 
;;     Depending on `icicle-init-value-flag', select minibuffer
;;     contents.
;;
;;  `repeat-complex-command' - Use `completing-read' to read command.
;;
;;
;;  ***** NOTE: The following functions defined in `mouse.el' have
;;              been REDEFINED HERE:
;;
;;  `mouse-choose-completion' - Return the number of the completion.
;;
;;
;;  ***** NOTE: The following functions defined in `dabbrev.el' have
;;              been REDEFINED HERE:
;;
;;  `dabbrev-completion' - Use Icicles completion when you repeat
;;                         (`M-C-/').

;;
;;
;;  Nutshell View
;;  -------------
;;
;;  Load this library and turn on Icicle mode (command `icy-mode').
;;  You're good to go.  Here's a sample of what you get:
;;
;;   M-x tool [next]
;;
;;  That is, hit the [next] key, which is often labeled "Page Down".
;;
;;   M-x ediff-toggle-use-toolbar [next]
;;   M-x scroll-bar-toolkit-scroll [next]
;;   M-x tool-bar-mode [next]
;;   M-x tooltip-mode [next]
;;   M-x ediff-toggle-use-toolbar ; Back to the beginning
;;
;;  Keys [next] and [prior] ("Page Up") cycle among all of the
;;  commands that contain (match) the minibuffer input - `tool', in
;;  this case.  Just hit `RET' (Return) when you get to the command
;;  you want.
;;
;;  You can use a regular expression, to narrow the field of matching
;;  inputs:
;;
;;   M-x ise.+char [next]
;;   M-x isearch-*-char [next]
;;   M-x isearch-delete-char [next]
;;   ...
;;
;;  You can display all of the matches for the current minibuffer
;;  input, in the *Completions* buffer, with `S-TAB' (Shift TAB).  So,
;;  for instance, `S-TAB' with `M-x ise.+char' in the minibuffer
;;  displays all commands whose names contain `ise' followed
;;  (somewhere) by `char'.
;;
;;  You can get normal, prefix completion, instead of this "apropos
;;  completion", by using the [up] and [down] arrow keys and `TAB'
;;  instead of [next], [prior] and `S-TAB'.
;;
;;  All of this works not only for the input of commands, with `M-x',
;;  but for the input of nearly anything.  For instance, you can use
;;  `C-x b' (`switch-to-buffer') and cycle among buffer names.  Or use
;;  `C-h v' (`describe-variable') and cycle among variable names.
;;
;;  Whenever you're in Icicle mode, you see "Icy" in the mode-line.

;;
;;
;;  Background on Input Completion
;;  ------------------------------
;;
;;  When you are prompted in the minibuffer to enter something, you
;;  are sometimes presented with a default value.  This might be
;;  automatically inserted after the prompt, initially.  If not, you
;;  can retrieve the default value yourself, using `M-n' (Emacs 21 or
;;  later).
;;
;;  Often, there is more than one reasonable default value that might
;;  make sense.  Depending on what you're being asked to enter, these
;;  "candidate default values" might be command names, buffer names,
;;  existing file names, variable names, and so on.
;;
;;  For most Emacs functions that prompt you and ask for input, the
;;  person who wrote the function decided on the reasonable set of
;;  default values, and passed these to an "input-completing function"
;;  such as `completing-read' or `read-file-name', which prompts you
;;  and reads your input.  The programmer also decided whether you
;;  will be *required* to pick one of the default values or will be
;;  free to enter something else.  The programmer might also have told
;;  the input-completing function to require your input to pass some
;;  special test (predicate).
;;
;;  So, how do you get access to those default values, in order to
;;  choose one?  You hit certain keys to "complete" the current
;;  contents of the minibuffer (excluding the prompt).  This current,
;;  partial input is considered as a prefix of one of the default
;;  values, and it is completed in the minibuffer to the entire
;;  default value.
;;
;;  Keys `TAB', `RET' (Return), and `SPC' (Space) perform different
;;  degrees of this "prefix completion".  If you type a prefix of one
;;  of the default values, you can complete the value this way in the
;;  minibuffer, and then enter it, with `RET'.
;;
;;  But if your partial input matches the prefix of more than one
;;  default value, then completion pops up the list of all matching
;;  completions for you to choose from (in buffer *Completions*).  You
;;  choose a candidate by clicking it with `mouse-2' or placing the
;;  cursor on it and hitting `RET'.
;;
;;  Because this is the way you access the default values supplied to
;;  an input-completing function, I call those values
;;  "prefix-completion candidates".  If there is no partial input yet,
;;  then the entire list of default values supplied to the
;;  input-completing function appears in the popup *Completions*
;;  buffer.  See the Emacs manual (`C-h i') for more on this general
;;  mechanism of prefix completion (called simply "completion" there).
;;
;;  Calls to `completing-read' and `read-file-name' are not the only
;;  places where its functionality is used.  When you use `M-x'
;;  (command `execute-extended-command'), completion is also
;;  available.

;;
;;
;;  Icicles Improves Input Completion: (1) Cycling Completions
;;  ----------------------------------------------------------
;;
;;  This library lets you also use the [up] and [down] arrow keys, (or
;;  `C-p' and `C-n') to cycle through the list of candidate prefix
;;  completions that match whatever input is present in the minibuffer
;;  (or all candidate default values, if there is no current input).
;;  Each candidate replaces the partial input in the the minibuffer,
;;  in turn.  The prefix (root) that was completed is underlined in
;;  the minibuffer completion candidate.
;;
;;  Suppose you do `C-x b' (`switch-to-buffer' command).  You can use
;;  `C-n' until the right buffer name appears in the minibuffer, then
;;  hit `RET'.  Or you can type some text that begins one or more of
;;  the buffer names, and then use `C-n' to cycle among those names
;;  that match that prefix.  If there are many candidates, typing part
;;  of the name to narrow the field can save time.
;;
;;  Or, suppose you do `C-h v' (`describe-variable') and type `cal'.
;;  Use `C-n' to cycle among all variables that start with `cal',
;;  until you find the one you want (then hit `RET').
;;
;;  In other words, the current partial input in the minibuffer
;;  determines a matching set of default values, and those are the
;;  values that you can cycle through.  You can at any time erase or
;;  change the partial input - the list of matching candidates
;;  automatically reflects the change.
;;
;;  This also means that it's good to have a quick way to clear the
;;  minibuffer of any input, so this library also provides key
;;  bindings [M-S-backspace] and [M-S-delete] (Meta + Shift +
;;  Backspace or Delete) to do that.  (I also use [C-M-backspace]
;;  (Control + Meta + Backspace), which I have bound globally to
;;  `backward-kill-sexp'.  It is not bound here, however.)
;;
;;  A visible and audible signal lets you know when you have reached
;;  one end of the list of completion candidates, but you can of
;;  course continue to cycle.
;;
;;  If the completion candidates are already displayed in buffer
;;  *Completions* when you try to cycle among them (because you hit
;;  `TAB'), then the current candidate is highlighted in *Completions*
;;  as you access it in the minibuffer with the [up] and [down] arrow
;;  keys.  If you change the minibuffer input, then the *Completions*
;;  list is updated accordingly, to reflect the new set of matching
;;  candidates.  The root that was completed (the minibuffer input) is
;;  highlighted in each candidate of the *Completions* display.  The
;;  *Completions* window is automatically scrolled as needed, to show
;;  the current candidate.
;;
;;  You can use `completing-read' and `read-file-name' to define your
;;  own commands, enabling them to take advantage of Icicles
;;  completion and cycling.  The definition of command
;;  `icicle-recent-file' is a good model to follow.  Emacs has a
;;  `recentf-mode' that lets you open recently accessed files.  But
;;  this mode makes you open a file using a menu interface.  Command
;;  `icicle-recent-file' lets you use the usual `find-file' minibuffer
;;  interface, with completion and cycling among your recent files.
;;  See Note to Programmers, below, for more on defining your own
;;  commands with `completing-read' and `read-file-name'.

;;
;;
;;  Traversing Minibuffer Histories
;;  -------------------------------
;;
;;  Perhaps you are already used to accessing past inputs using the
;;  [down] and [up] arrow keys, `C-n' and `C-p', `M-n' and `M-p', or
;;  [next] and [prior].  If not, try it.  You can go backward and
;;  forward in the minibuffer histories (there are different history
;;  lists for different kinds of input).  You can't really cycle them,
;;  but when you get to one end you can reverse the direction.
;;
;;  Anyway, the input-cycling behavior that Icicles offers is in
;;  addition to this traversal of histories.  Since there are, by
;;  default, several extra pairs of keys used for history traversal,
;;  rebinding some of them to use for Icicles completion is no real
;;  loss.
;;
;;  By default, Icicles rebinds all of the key sequences that you
;;  normally use for commands `next-line' and `previous-line', so that
;;  they perform prefix-completion cycling in the minibuffer.  In
;;  vanilla Emacs, this means keys [down], [up], `C-n', and `C-p'.
;;
;;  Icicles also rebinds [next] and [prior] for apropos-completion
;;  cycling (see below).  You still have `M-n' and `M-p' available to
;;  access past inputs (history).  And the rebindings are only for
;;  minibuffer input; global bindings are not affected.
;;
;;  You can at any time switch back and forth between input-history
;;  traversal (`M-n', `M-p') and prefix completion (`C-n', `C-p' or
;;  [down], [up]).
;;
;;  See Also: Icicles Improves Input Completion: (11) History
;;  Enhancements, below, for new ways to use the standard history
;;  lists with Icicles.

;;
;;
;;  Icicles Improves Input Completion: (2) Apropos Completions
;;  ----------------------------------------------------------
;;
;;  There is a second way that you can use Icicles to complete the
;;  partial input in the minibuffer.  Instead of considering the
;;  string of input characters to be the prefix of various complete
;;  names, you can look for names that match that string anywhere.
;;
;;  This is similar in effect to using command `apropos' to find
;;  "apropos completions" of a string (except it also works for file
;;  and buffer names), so that's the term I use for this.  The more
;;  correct characterization of this is that of the previous
;;  paragraph, however: names that match the given string.
;;
;;  Just as with prefix completion, Icicles lets you cycle among the
;;  apropos candidates.  To do this, you use keys [next] and [prior]
;;  (or `C-v' and `M-v').  The root that was completed is underlined
;;  in the minibuffer completion candidate.
;;
;;  For example, suppose you use `M-x' to enter a command.  You don't
;;  remember the exact command name, but it has something to do with
;;  lines, so you do `M-x line', then hit [next] repeatedly, until you
;;  see the right "line" command - `transpose-lines', perhaps.  Prefix
;;  completion cannot find this command, because "line" is not a
;;  prefix of "transpose-lines".
;;
;;  Because `M-x' expects a command name, only command names are
;;  inserted into the minibuffer as the apropos-completion candidates
;;  for `M-x'.  Likewise, in other contexts, where names of other
;;  kinds of object are expected, apropos completion inserts only
;;  names of objects of the appropriate type.  Prefix completion works
;;  the same way.
;;
;;  For example, using [next] and [prior] with `C-x b at' lets you
;;  cycle through all buffers (such as `*scratch*') that have "at" in
;;  their name - only buffer names appear as candidates.
;;
;;  Another example: Suppose you are in Info, reading the Emacs-Lisp
;;  manual, and you want to go to a node (manual section) that
;;  discusses regular expressions (regexps). You could search through
;;  the Table of Contents, or you could search through the Index or
;;  index topics (`i'), or you could search through the manual text
;;  from the beginning (`s').  Or, you could type `g' to use command
;;  `Info-goto-node', type the word `regexp' in the minibuffer, and
;;  then use [next] or [prior] to cycle among all Info nodes with
;;  `regexp' in their name.
;;
;;  Apropos completion uses a regular expression (regexp) as its input
;;  string.  You can type `M-x \bes', for instance, to find commands
;;  with "es" at the start of a word within the command name - it will
;;  find `eshell-test' and `color-theme-blue-eshell', but not
;;  `count-lines': "es" does not start a word in `count-lines'.
;;  Similarly, for file names, buffer names, and so on.
;;
;;  Prefix completion is actually a special case of apropos
;;  completion, where the regexp starts with "^".  (That is not how it
;;  is implemented, however.)
;;
;;  What if you want to see the list of all completion candidates that
;;  match the minibuffer input? Instead of cycling candidates blindly,
;;  just hit `S-TAB' (Shift TAB) at any time to display the matching
;;  candidates in pop-up buffer *Completions*.
;;
;;  Everything said above about the *Completions* buffer for prefix
;;  completion is also true for apropos completion.  It is updated to
;;  reflect that set of candidates, and the current completion is
;;  highlighted.  The root that was completed is highlighted in each
;;  candidate (first occurrence only).  Root highlighting is more
;;  important in the case of apropos completion, because the match
;;  position is different in different candidates.  In the case of
;;  apropos completion, the root is not the input string, taken
;;  literally, but the part of a candidate that the input matches.
;;  See "Icicles Improves Input Completion: (3) *Completions*
;;  Display", below, for additional ways to use the minibuffer with
;;  `*Completions*'.
;;
;;  Regexp matching is perhaps the most powerful feature of Icicles.
;;  Enjoy!  You can at any time switch back and forth between prefix
;;  completion ([down], [up]), apropos completion ([next], [prior]),
;;  and history traversal (`M-n', `M-p').

;;
;;
;;  What About Special-Character Conflicts?
;;  ---------------------------------------
;;
;;  Regular-expression syntax treats some characters specially, but
;;  some of these special characters have another special meaning in
;;  Emacs when used with file-name inputs.  What about the conflict
;;  between interpreting characters such as `$', `\', `.', `?', and
;;  `*' as 1) regexp special characters and 2) special characters for
;;  file-name input?  For example, when inputting a file name, should
;;  `*' be treated as a regexp multiple-occurrences operator or a
;;  file-name wildcard?
;;
;;  In Emacs file-name input:
;;
;;  - `$' can be used to prefix environment variables.
;;
;;  - `*' and `?' can be used as wildcards, effectively inputting
;;    multiple file names at once.
;;
;;  - `.' and `..' can be used to navigate a directory hierarchy.
;;
;;  - `\' is a directory separator, like `/', on MS Windows, at least.
;;
;;  Icicles handles the conflict by interpreting such characters as
;;  regexp special characters only during input completion and cycling
;;  - and then only if you do not escape them (with `\').  If present
;;  in the input when you finally accept it (using `RET'), they take
;;  on their normal Emacs meanings for file-name input:
;;  environment-variable prefix, wildcard, directory abbreviation, or
;;  directory separator.
;;
;;  That is, whenever there is a potential conflict of interpretation,
;;  the regexp meaning is used for completion and cycling, and the
;;  standard interpretation for file-name input is used for accepting
;;  the input.  So, for example, to get the wildcard interpretation of
;;  `*', just forego regexp completion and cycling.  And vice versa:
;;  forego the wildcard interpretation to use regexp completion and
;;  cycling.
;;
;;  Note: Because `?' is useful in regexp syntax, the standard Emacs
;;        minibuffer binding of `?', which just displays the
;;        completion-candidates list, is not used in Icicles.  In
;;        Icicles, `?' self-inserts in the minibuffer, like any other
;;        printable character.  (Use `TAB' or `S-TAB' to display the
;;        list.)  In standard Emacs, you must quote `?' or
;;        copy-and-paste it, to insert it in the minibuffer for use as
;;        a file-name wildcard.
;;
;;  The interpretation conflict for `\' (on MS Windows) is not really
;;  a problem, anyway.  Although you cannot use a backslash (`\') as a
;;  directory separator during completion and cycling, you can always
;;  use a slash (`/') instead - even on MS Windows.  Just break with
;;  MS-Windows syntax, and get in the habit of using `/' as the
;;  directory-separator character.
;;
;;  Even if you use only slash, not backslash, as a directory
;;  separator when inputting, however, it's possible that you could
;;  run into some trouble (on MS Windows) - you might (knowingly or
;;  not) use `\' as a directory separator in the values of environment
;;  variables that you use as part of file-name input.  Because the
;;  expanded input is treated as a regexp by apropos completion, you
;;  should use only prefix completion with input that includes
;;  environment variables, if their expansions include backslashes.
;;
;;  The interpretation conflict for `$' is also not a real problem.
;;  You can get the effect of both interpretations of `$' at the same
;;  time, because Icicles recognizes that `$' at the end of input
;;  cannot be an environment-variable prefix.  This means, for
;;  example, that you can use a pattern such as `$HOME.*t$' to match
;;  the files in your home directory whose names end in `t'.
;;
;;  Tip: Because slash (`/') is about the only non-word syntax
;;       character that is likely to appear in file-name completions,
;;       you can usually use `\W$' to match only directories (by
;;       matching the `/' at the end of their names).

;;
;;
;;  Alternative Libraries: Other Methods of Choosing Default Values
;;  ---------------------------------------------------------------
;;
;;  There are other libraries that give you alternative ways to pick a
;;  candidate default value.  There are, for instance, many libraries
;;  that provide ways to switch buffers.  Some of these present
;;  candidates in the minibuffer and choose one as soon as you type
;;  enough of its name to select it unambiguously - without your
;;  needing to confirm your choice (with `RET', for example).  Library
;;  `ido.el' is an example of such a library.  Choosing without
;;  confirming can be very quick, but I prefer to confirm a choice.
;;
;;  In any case, you can also use Icicles to choose without
;;  confirming, if you wish - see "Icicles Improves Input Completion:
;;  (6) Multi-Commands".  See also "Exiting the Minibuffer Without
;;  Confirmation" for how to obtain the complete-and-exits behavior of
;;  library `iswitchb.el'.
;;
;;  The main reason I prefer Icicles is because of its generality.
;;  You use the same input, cycling, and completion method for
;;  everything.  There is no need to be familiar with one method for
;;  switching buffers, another method for choosing a command, another
;;  for choosing a variable, and so on.  Library `ido.el' is quite
;;  general too, but perhaps a little less so.
;;
;;  Also, I like to be able to edit the value in the minibuffer.  For
;;  instance, in a situation where you are not required to enter one
;;  of the default values (e.g. no REQUIRE-MATCH argument to
;;  `completing-read'), you can use completion to retrieve a default
;;  value that is similar to what you want to enter, then edit it and
;;  hit `RET' to submit the actual value you want.  Library `ido.el'
;;  does have an "edit" command or mode, but I find Icicles better for
;;  letting me edit input.
;;
;;  Icicles has many additional features that are not available in
;;  other libraries (see below), but its main advantage is its
;;  generality: you use the same user interface for input of any kind.

;;
;;
;;  Exiting the Minibuffer Without Confirmation: `S-RET'
;;  ----------------------------------------------------
;;
;;  Normally, if you exit the minibuffer with input that only
;;  partially matches a completion candidate, the value you input is
;;  exactly what you typed.  That is, exiting does not automatically
;;  complete your input - what you type is what you input.  This is
;;  desirable most of the time, because it lets you input a value that
;;  does not correspond to any of the completion candidates.  This is
;;  how, for instance, you can use `C-x C-f' to open a new file or
;;  `C-x b' to create a new buffer.
;;
;;  However, some people prefer to limit input to the available
;;  completion candidates.  This can be handy in the case of switching
;;  to a buffer, for instance.  If you have a buffer named
;;  `new-ideas.txt', you might like to be able to type only `new'
;;  followed by `RET', and not have to first complete the input text.
;;  This is the behavior of libraries `ido.el' and `iswitchb.el'.
;;
;;  It is the command you use that decides whether `RET' first
;;  completes your input before exiting the minibuffer.  This is done
;;  in the command definition by providing a non-nil REQUIRE-MATCH
;;  argument to function `completing-read', which prompts you and
;;  reads your input, possibly completing it.
;;
;;  If you use standard Emacs command `switch-to-buffer', `RET' does
;;  not behave this way; it simply accepts your input, `new', and
;;  creates a new buffer with that name.  By default, command
;;  `icicle-buffer' behaves the same way.  However, you can obtain the
;;  complete-and-exit behavior with `icicle-buffer' by setting option
;;  `icicle-buffer-require-match-flag' to `partial-match-ok'.  This
;;  value overrides the REQUIRE-MATCH argument to `completing-read',
;;  in effect forcing it to `t'.
;;
;;  Whenever completion *requires* a match against one of the
;;  completion candidates (typically, an existing file or buffer
;;  name), you can complete and exit the minibuffer all at once, with
;;  only partial input in the minibuffer, by using `RET'.  But what
;;  about apropos completion?  Simply use `S-RET' instead of `RET':
;;  `RET' is standard in Emacs and uses prefix completion; `S-RET' is
;;  specific to Icicles and uses apropos completion.  For example, you
;;  can type `idea' followed by `S-RET' to switch to buffer
;;  `new-ideas.txt'.

;;
;;
;;  Icicles Improves Input Completion: (3) *Completions* Display
;;  ------------------------------------------------------------
;;
;;  Icicles also adds a few enhancements to the *Completions* display,
;;  for convenience.  The following apply whenever buffer
;;  *Completions* is displayed:
;;
;;  1. When you cycle completions in the minibuffer, the current
;;     candidate is highlighted in *Completions*.
;;
;;  2. You can use the [insert] key to move back and forth between the
;;     minibuffer and *Completions*.  In each direction, the current
;;     candidate is tracked in the destination buffer.  For example,
;;     if the candidate in the minibuffer is `foobar', after you hit
;;     [insert] the cursor is on `foobar' in *Completions*.  In the
;;     other direction, if the cursor is on `foobar' in *Completions*,
;;     after you hit [insert] the current input in the minibuffer is
;;     `foobar'.
;;
;;  3. In buffer *Completions*, you can use the arrow keys to navigate
;;     among the candidate completions.  The current candidate (under
;;     the cursor) is highlighted.
;;
;;  4. *Completions* can also serve as a new kind of icompletion help
;;     - see "Icicles Improves Input Completion: (4) Icompletion",
;;     below.
;;
;;  5. You can choose multiple candidates during completion, by
;;     clicking them with `mouse-2' while holding the Control key
;;     pressed.  See section "Icicles Improves Input Completion: (6)
;;     Multi-Commands", below.

;;
;;
;;  Icicles Improves Input Completion: (4) Icompletion
;;  --------------------------------------------------
;;
;;  Emacs incremental completion, or icompletion, provided by standard
;;  library `icomplete.el', displays matching prefix completions in
;;  the minibuffer.  This display is updated incrementally as you type
;;  characters.  In spite of the name, icompletion does not, in fact,
;;  provide any completion; it provides completion help, letting you
;;  know which (prefix) completions are available.
;;
;;  Icicles enhances Emacs icompletion in two ways:
;;
;;  1. It works with library `icomplete+.el' to provide minibuffer
;;     feedback on the number of completion candidates.
;;
;;  2. It provides a new kind of icompletion using buffer
;;     *Completions*.
;;
;;  Library `icomplete+.el' enhances `icomplete.el' in various ways.
;;  One of these ways is to complement Icicles by displaying the
;;  number of other prefix-completion candidates when cycling.  This
;;  number is displayed whenever you change direction when cycling.
;;  For example:
;;
;;      M-x forward-line   [Matched]  (13 more)
;;
;;  (Reminder: There is no icompletion for file-name completion - see
;;  standard library `icomplete.el'.)
;;
;;  Buffer *Completions* shows you the current set of candidates for
;;  either prefix or apropos completion.  If user option
;;  `icicle-incremental-completion-flag' is non-nil, then
;;  *Completions* is automatically updated when you change your input
;;  in the minibuffer - that is, with each character that you type or
;;  delete.
;;
;;  The particular non-nil value determines when *Completions* is
;;  displayed and updated.  The default value, t, means that
;;  *Completions* is only updated if it is already displayed.  Use t
;;  if you don't want *Completions* to be too intrusive but you want
;;  it to provide the most help when you ask for help (via `TAB' or
;;  `S-TAB').
;;
;;  Any other non-nil value displays and updates *Completions*
;;  whenever there is more than one completion candidate.  That can be
;;  more helpful, but it can also be more distracting.  A value of nil
;;  turns off automatic updating altogether - *Completions* is then
;;  only displayed upon demand.  I find that t represents a good
;;  compromise, providing help when I ask for it, and then continuing
;;  to help until I've finished choosing a candidate.
;;
;;  There are several advantages of using *Completions* for
;;  icompletion, as opposed to the minibuffer:
;;
;;  1. Many more candidates can be displayed in *Completions* than can
;;     be displayed by standard icompletion, which uses the minibuffer
;;     for feedback.
;;
;;  2. Standard icompletion provides feedback only on matches for
;;     prefix completion.  If you use both standard icompletion and
;;     Icicles icompletion, you can have incremental help for both
;;     prefix-completion and apropos-completion at the same time, one
;;     in the minibuffer and the other in *Completions*.
;;
;;  3. The other Icicles *Completions* features are available for the
;;     current set of matching candidates: cycling, highlighting of
;;     match root, highlighting of previously used candidates, and so
;;     on.  See "Icicles Improves Input Completion: (3) *Completions*
;;     Display", above.

;;
;;
;;  Icicles Improves Input Completion: (5) Help on Candidates
;;  ---------------------------------------------------------
;;
;;  While you are cycling among completion candidates, you can
;;  simultaneously display help on each candidate or any given
;;  candidate.  To show help on each candidate as you cycle, press and
;;  hold the Control key while using the vertical arrow keys (for
;;  prefix completion) or the Page Up/Down keys (for apropos
;;  completion).  To show help on any individual candidate, just
;;  navigate to it (cycling or using completion), and hit `C-RET' - or
;;  press Control and click it with `mouse-2' (`C-mouse-2') in buffer
;;  *Completions*.
;;
;;  For example, if you use `C-[next]' (apropos completion) to cycle
;;  among commands to execute with `M-x', the documentation for each
;;  command is displayed in the *Help* buffer as the candidate appears
;;  in the minibuffer.  As another example, if you cycle among buffers
;;  with `C-[down]' (prefix completion) for `C-x b', the major and
;;  minor modes of each candidate buffer are described in buffer
;;  *Help* as the buffer name appears in the minibuffer.
;;
;;  You can use this functionality as a kind of expanded `apropos'
;;  function.  As an example, type `C-h v out', then type `S-TAB' to
;;  display all variables that match "out" (in buffer *Completions*).
;;  Then use `C-[next]' to cycle among those variables, displaying
;;  their documentation in the *Help* buffer as they appear one by one
;;  in the minibuffer.  Or click individual variable names with
;;  `C-mouse-2', to display their documentation.  The standard
;;  `apropos' commands show only the first doc-string line; Icicles
;;  shows the complete doc string.
;;
;;  This can be handy, for instance, when you are unsure which of
;;  several similarly named candidates to choose.  Seeing a
;;  candidate's documentation along with its name can help you decide.
;;
;;  This also works with menu items, if you load library
;;  `icicles-menu.el' as well as `icicles.el'.  As you cycle among
;;  matching menu items, the corresponding command documentation is
;;  displayed in *Help*.
;;
;;  For more information about the types of candidates and their
;;  associated documentation, see the documentation for command
;;  `icicle-help-on-candidate'.  This command is bound to `C-[help]',
;;  `C-[f1]', and `C-RET' (by default).
;;
;;  If you use one-buffer-per-frame (`pop-up-frames' non-nil), then
;;  displaying *Help* in one frame can interfere with viewing
;;  *Completions* in another.  For that reason, the *Completions*
;;  frame is raised to the front.  Also, if user option
;;  `icicle-Completions-frame-at-right-flag' is non-nil, then the
;;  *Completions* frame is moved to the right, out of the way, when
;;  you access *Help*.
;;
;;  Note: There are also Icicles replacements for the usual `apropos'
;;  commands.  They act the same, but they also let you see the list
;;  of regexp matches incrementally (as with all Icicles commands),
;;  using `S-TAB'.  If you also use my library `apropos-fn+var.el',
;;  then these Icicles commands take advantage of the apropos
;;  enhancements in that library.  The Icicles apropos commands are:
;;  `icicle-apropos', `icicle-apropos-command',
;;  `icicle-apropos-function', `icicle-apropos-option',
;;  `icicle-apropos-variable', and `icicle-apropos-zippy'.

;;
;;
;;  Icicles Improves Input Completion: (6) Multi-Commands
;;  -----------------------------------------------------
;;
;;  Certain Icicles commands let you make multiple input choices in a
;;  single command execution.  In effect, you can choose multiple
;;  items from a set of choices, using buffer *Completions* as a
;;  multiple-choice "menu".  (It's not necessary to display
;;  *Completions*, however.)
;;
;;  I call such multiple-choice commands "multi-commands".  When a
;;  multi-command prompts you for input, instead of making a single
;;  choice and pressing `RET' to confirm it, you can choose any number
;;  of completion candidates, using `C-RET' (or `C-mouse-2') for each.
;;  You can thus act on multiple candidates, or even multiple times on
;;  the same candidate, during the same execution of the command.
;;
;;  For example, command `icicle-delete-file' lets you delete any
;;  files that match your minibuffer input - all in the same command
;;  execution.  If you type no input, then all files in the current
;;  directory match, and you can delete any number of them.  If you
;;  type `~$' and hit `S-TAB' (`apropos-complete'), then all files
;;  that end in `~' match, and you can delete any number of them.
;;  Similarly, command `icicle-buffer-other-window' lets you display
;;  any number of buffers, and so on.
;;
;;  You make multiple choices this way by cycling through the
;;  candidate completions, as usual, and hitting `C-RET' whenever you
;;  want to choose (act on) the current cycle candidate.  Or, just
;;  press and hold Control while clicking each candidate with
;;  `mouse-2'.
;;
;;  This is the same thing that you do to access help on a candidate
;;  (see previous section).  A multi-command is just any command that
;;  has a special action defined for use with `C-RET' on the current
;;  cycle candidate.  If no such special action is defined, then help
;;  on the candidate is displayed - displaying help is just the
;;  default action, used when no other action is defined.
;;
;;  Similarly, you can use `C-[next]', `C-[prior]', `C-[down]', and
;;  `C-[up]' to both choose (that is, act on) the current candidate
;;  and move forward or backward to the next successive candidate.
;;  You can thus just hold down the Control key while cycling, to act
;;  on each candidate in turn, if you want.
;;
;;  Instead of, or in addition to, cycling, you can use completion to
;;  get to a particular candidate you want.  No matter how a candidate
;;  is made current, you can choose the current candidate (perform the
;;  action on it) using `C-RET', `C-mouse-2', `C-[next]', and so on.
;;
;;  As always, hitting `RET' (or `S-RET') ends the command.  For most
;;  multi-commands, hitting `RET' performs the same action as `C-RET',
;;  but it is possible to have a command that acts differently for
;;  `RET' and `C-RET'.  That is the case, for instance, when help is
;;  displayed via `C-RET'.
;;
;;  You can use `C-g' to exit a multi-command at any time, without
;;  making a final choice using `RET'.  If the actions performed by a
;;  multi-command are easily reversible, `C-g' will often restore
;;  things to the way they were before performing the actions.
;;
;;  If you are an Emacs-Lisp programmer and you want to write your own
;;  multi-command, just make the command do this:
;;
;;  1. Call `completing-read' or `read-file-name', and perform some
;;     action on the completed input.
;;
;;  2. Bind `icicle-candidate-action-fn' to a function that performs
;;     an action on a completion candidate - possibly the same action.
;;
;;  #1 just lets people use the command normally, to perform the #1
;;  action on a completion candidate entered with `RET'.  Because of
;;  #2, people can perform the #2 action on any completion candidates,
;;  while still continuing to cycle or complete candidates.
;;  Typically, the actions for #1 and #2 are the same, but nothing
;;  prevents you from using different actions.
;;
;;  When internal variable `icicle-candidate-action-fn' is not bound,
;;  the default action is performed: display help on the current
;;  candidate.
;;
;;  See section "Defining Icicles Commands", below, for an easy way to
;;  define your own multi-commands.  It introduces simple-to-use
;;  macros that take implement both #1 and #2 for you.
;;
;;  Note: As a user, you can also cycle among elements of a set,
;;        performing actions, if you use my libraries `doremi.el',
;;        `doremi-cmd.el', and `doremi-frm.el'.  Like Icicles, DoReMi
;;        lets you see the effect of a choice immediately, whenever
;;        you make changes.  Each library has its own advantages and
;;        special uses. Advantages of Icicles include:
;;
;;        - completion to candidate values
;;        - restoration after making changes, letting you preview
;;          changes without actually applying them
;;
;;        The latter assumes that action commands have been defined
;;        with proper restoration clauses - see "Defining Icicles
;;        Commands", below.

;;
;;
;;  Icicles Improves Input Completion: (7) Choose All Candidates
;;  ------------------------------------------------------------
;;
;;  The previous section describes how you can use `C-RET' to choose
;;  (act on) multiple completion candidates, individually.  If you
;;  hold down the Control key while you cycle through the candidates,
;;  you can run through each of them, one by one.  Command
;;  `icicle-all-candidates-action', which is bound to `C-!' in the
;;  minibuffer, is just a shorthand way of doing that: act on all
;;  candidates that match the current input.
;;
;;  All multi-commands let you use `C-!' in this way.  Whenever a
;;  command defines a special action for `C-RET' to perform on the
;;  current completion candidate, you can use `C-!' to perform it on
;;  all candidates at once.
;;
;;  Perhaps you already use `% m' (command `dired-mark-files-regexp')
;;  in Dired to mark all files that match a given regular expression,
;;  and then operate on all of the marked files in some way.  When you
;;  execute a multi-command, `C-!' lets you do something similar.  It
;;  applies `icicle-candidate-action-fn' to each completion that
;;  matches (apropos or prefix) the current input in the minibuffer.
;;
;;  Most top-level Icicles commands are multi-commands.  Command
;;  `icicle-delete-file' is an example.  Instead of entering a file
;;  name at the prompt (e.g. using completion or cycling), you can
;;  type a regular expression, use `S-TAB' to see all matching files,
;;  and then use `C-!' to delete all of them at once.
;;
;;  You get the idea: Use the minibuffer to determine a set of objects
;;  by pattern matching, and then act on all elements of the set.
;;
;;  If you are an Emacs-Lisp programmer, you can define your own
;;  multi-commands.  Section "Defining Icicles Commands", below,
;;  explains how to do that.  You define a function that acts on a
;;  single object, and then use that function in a multi-command to
;;  act on either a single object or multiple objects.  There are lots
;;  of possible applications.
;;
;;  As a illustration of what is involved, here is the definition of a
;;  command similar to `icicle-delete-file':
;;
;;  (defun delete-one-or-more-files ()
;;    "Delete one or more files that match the current input."
;;    (interactive)
;;    (let* ((icicle-candidate-action-fn
;;            'my-delete-file-or-directory) ; Action #2
;;           (the-file
;;            (condition-case fail
;;                (completing-read
;;                 "Delete file: "
;;                 (mapcar #'list (directory-files default-directory))
;;                 nil t)
;;              (error (error-message-string fail)))))
;;      (when the-file
;;        (icicle-delete-file-or-directory the-file)))) ; Action #1
;;
;;  Here, the function that acts on a single object (file) is
;;  `my-delete-file-or-directory'.  It is called on the result of
;;  `completing-read' (action #1), and it is also bound to
;;  `icicle-candidate-action-fn' (action #2), so that it will be
;;  applied to the current candidate via `C-RET'.
;;
;;  As illustrated by this definition, the logic of a multi-command
;;  implementation is a bit complex.  There is, fortunately, an easier
;;  way to define such a command, as explained in section "Defining
;;  Icicles Commands", below.
;;
;;  Command `icicle-all-candidates-action' reports on the objects that
;;  were not acted upon successfully (in buffer *Help*).  For this
;;  reporting, the function bound to `icicle-candidate-action-fn'
;;  (e.g. `my-delete-file-or-directory', above) should return nil for
;;  success and non-nil (for example, an error message) for failure,
;;  whatever "success" and "failure" might mean in the context of use.

;;
;;
;;  Icicles Improves Input Completion: (8) Progressive Completion
;;  -------------------------------------------------------------
;;
;;  The best way to explain this feature is to use a familiar analogy.
;;  Unix or GNU/Linux command `grep' takes a regular-expression
;;  argument, and matches it against lines in files.  A common idiom
;;  that people use is to chain, or cascade, multiple calls to `grep',
;;  using the output of one as the input to the next.  For example:
;;
;;    grep plant *.txt | grep food | grep mineral
;;
;;  The output of the search for "plant" is used as the input for the
;;  search for "food", and the output of that search serves as the
;;  input for the search for "mineral".  The order of the three
;;  component searches can make a difference in terms of performance,
;;  but not in terms of the result, which is always the set of lines
;;  in files *.txt that match "plant" AND "food" AND "mineral", in any
;;  order.  Each of the `grep' operations defines a set of matches,
;;  and the chain of `grep' operations effects the intersection of
;;  those sets.
;;
;;  Of course, you could try to accomplish the same thing with a
;;  single call to `grep' using a complex regexp.  But why would you?
;;
;;  The same idea is behind the Icicles feature of progressive
;;  completion: instead of trying to come up with a complex regexp
;;  that does what you want, try getting there a step at a time:
;;
;;   1. Match an input regexp against the set of all possible
;;      completions.
;;
;;   2. Narrow the set of matched candidates by matching them against
;;      another input regexp.
;;
;;   3. Narrow those results down by matching them against a third
;;      input regexp.
;;
;;   4... And so on.
;;
;;  During completion, `M-*' is bound in the minibuffer to command
;;  `icicle-narrow-candidates', which prompts for a new regexp and
;;  matches it against the current set of completion candidates.
;;  Example:
;;
;;   1. `C-x C-f a S-TAB' displays file names that contain `a'.
;;
;;   2. `M-* ph S-TAB' narrows that set of file names to those that
;;      also contain `ph'.
;;
;;   3. `M-* bet S-TAB' narrows the set of matching names further, to
;;      those that also contain `bet'.
;;
;;  You get the idea.  This feature is both very simple to use and
;;  very useful.  It's easy to appreciate using multiple simple
;;  matching steps (regexp or not) instead of a single regexp.  Try it
;;  once, and you'll be hooked.
;;
;;  `M-*' works with both prefix completion and apropos completion.
;;  You can first use `TAB' to require the target to start with some
;;  string, and then use `M-*' to specify other patterns that parts of
;;  it must also match.  However, it of course makes no sense to use
;;  `TAB' instead of `S-TAB' after you use `M-*': once you've said
;;  that the target must start with "fo" there is no sense saying that
;;  it also starts with "ti"!
;;
;;  For lack of a better name, I'm calling this feature "progressive
;;  completion".  If the name "incremental completion" (= icompletion)
;;  were not already taken to mean incremental completion *help*
;;  (which performs no completion), then that might be a good name for
;;  this.  This might also be called "stepped", "cascaded", or
;;  "piecewise" completion.
;;
;;  Another possible name for it would be "multiple completion", but I
;;  use that to stand for simultaneous (parallel) completion of
;;  multiple parts of a compound target, which is something different
;;  (see Icicles Improves Input Completion (14) Multi-Completions,
;;  below).  Progressive completion is a set of mini-completions that
;;  are wired in series, not in parallel.

;;
;;
;;  Icicles Improves Input Completion: (9) Candidate Sets
;;  -----------------------------------------------------
;;
;;  Whereas `C-RET' acts on individual objects, `C-!' (see section
;;  "Icicles Improves Input Completion: (7) Choose All Candidates")
;;  acts on an entire set of objects at once, via their names: the set
;;  of all current completion candidates.  There are additional
;;  Icicles commands that act, not on individual completion
;;  candidates, but on one or more sets of completion candidates.
;;
;;  One of these is `M-*', which effectively narrows the set of
;;  completion candidates by taking the intersection of the candidate
;;  sets defined by various input regexps.  See "Icicles Improves
;;  Input Completion: (8) Progressive Completion".
;;
;;  This section presents some more Icicles commands that act on sets
;;  of completion candidates.  The basic idea is that you can perform
;;  set operations using the current set of completion candidates,
;;  changing it into a different set.  The available set-operation
;;  commands presented here are these:
;;
;;  * `icicle-candidate-set-save', bound to `C->'.  Save the current
;;    set of completion candidates, for use in a subsequent set
;;    operation (see below).
;;
;;  * `icicle-candidate-set-retrieve', bound to `C-<'.  Retrieve the
;;    saved set of completion candidates, making it the current set.
;;
;;  * `icicle-candidate-set-swap', bound to `C-%'.  Swap the saved and
;;    current sets of completion candidates.
;;
;;  * `icicle-candidate-set-define', bound to `C-:'.  Define the
;;    current set of completion candidates by evaluating an input
;;    sexpr.  The sexpr must evaluate to a list of strings, such as is
;;    returned by `all-completions'.  You can use this to substitute
;;    any list of strings, and then operate on them as completions,
;;    using any Icicles functionalities.  Keep in mind, however, that
;;    the completions must be of the proper type for the context in
;;    which they are used.  For example, if you are executing a
;;    command, they must be command names.
;;
;;  * `icicle-candidate-set-complement', bound to `C-~'.  Complement
;;    the current set of candidates: replace the current candidate set
;;    with its set complement.  This means all possible completions of
;;    the appropriate type that do *not* match the current input.
;;
;;  * `icicle-candidate-set-union', bound to `C-+'.  Replace the
;;    current candidate set by its union with the saved set of
;;    candidates.
;;
;;  * `icicle-candidate-set-difference', bound to `C--'.  Replace the
;;    current candidate set by its set difference with the saved set
;;    of candidates.  That is, the saved candidates are subtracted
;;    from the current candidates, and the result becomes the current
;;    candidate set.  To obtain the opposite set difference,
;;    subtracting the current candidates from the saved candidates,
;;    just use `icicle-candidate-set-swap' followed by
;;    `icicle-candidate-set-difference'.
;;
;;  * `icicle-candidate-set-intersection', bound to `C-*'.  Replace
;;    the current candidate set by its intersection with the saved set
;;    of candidates.  Unlike the set intersection provided by `M-*',
;;    `C-*' is, in itself, a one-time operation.  `M-*' can be
;;    repeated, using the previous intersection as one of the sets to
;;    be intersected in a new operation.  Both `C-*' and `M-*' use the
;;    current set of matching candidates as one of the sets being
;;    intersected.  But `M-*' reads another input regexp to define the
;;    other set to be intersected, whereas `C-*' uses the saved
;;    candidates set as the other set.  `M-*' is useful for chaining,
;;    to achieve progressive approximation.  `C-*' is useful to
;;    perform an intersection on a set from a previous input reading.
;;
;;  You can operate on or choose from all input values in the set
;;  resulting from any of these set operations.  For example, you can
;;  use `C-~' to see the list of objects that do not match the current
;;  input, to cycle among those objects, or to operate on any or all
;;  of them.  Use `C-~' at any time to switch to the complement of the
;;  current set of candidates.
;;
;;  Example: To cycle through all files whose names do not end in
;;           `el', you can do the following:
;;
;;  1. Use `C-f' to read a file name.
;;  2. Type `el$'.
;;  3. Use `S-TAB' to show the matching files.
;;  4. Use `C-~' to flip to the complement: files not ending in `el'.
;;  5. Use [next] or [prior] to cycle among the new set of candidates.
;;
;;  A minibuffer message briefly confirms each of the set operations.
;;
;;  When buffer *Completions* is displayed, the union, difference, and
;;  intersection commands scroll the buffer when repeated, just like
;;  `TAB' and `S-TAB' do.  Repeating `icicle-candidate-set-complement'
;;  complements the complement, of course, giving the original set.
;;
;;  Once you have established a set of completion candidates using any
;;  of the candidate-set commands, you can cycle among the candidates
;;  of that set using either prefix or apropos cycling (that is,
;;  [next]/[prior] or [down]/[up]).  However, switching from prefix to
;;  apropos cycling (or completion), or vice versa, establishes a new
;;  completion set of the appropriate type, as usual.  Switching
;;  completion type signifies that you are finished with the specially
;;  defined completion set, and you want to redefine it using apropos
;;  or prefix cycling or completion.
;;
;;  Note: Prefix icompletion (`icomplete.el' or `icomplete+.el') does
;;        not take into account the candidate set resulting from a set
;;        operation: it always displays the normal set of prefix
;;        completions in the minibuffer.
;;
;;  You might have noticed that, as a mnemonic, the keys bound to the
;;  various set operations use the corresponding binary arithmetic or
;;  Boolean operators: `~' (unary negation) for complement (not); `*'
;;  (multiplication) for intersection (and); `+' (addition) for union
;;  (or); and `-' (subtraction) for difference.
;;
;;  For other examples of using set operations, see also:
;;
;;   * "Icicles Improves Input Completion: (8) Progressive Completion"
;;   * "Icicles Improves Input Completion: (11) History Enhancements" 
;;   * "Icicles Improves Input Completion: (12) Search Enhancements"

;;
;;
;;  Defining Icicles Commands
;;  -------------------------
;;
;;  This section is for Emacs-Lisp programmers.
;;
;;  Defining a command that uses Icicles completion and cycling is
;;  simple: just call `completing-read' or `read-file-name' to read
;;  input, then act on that input.  Here, for instance, is a simple
;;  command that reads a font name and then changes the selected frame
;;  to use that font.  Completion and cycling are available, using all
;;  available font names as the pool of candidates.
;;
;;  (defun change-font ()
;;    "Change font of selected frame."
;;    (modify-frame-parameters
;;     (selected-frame)
;;     (list (cons 'font (completing-read
;;                        "Font: " (mapcar #'list (x-list-fonts "*"))
;;                        nil t)))))
;;
;;  Nothing could be simpler - just use `completing-read'!
;;
;;  However, what if you want to define a multi-command - that is, a
;;  command that takes advantage of an action function when cycling
;;  candidates, as described in sections "Icicles Improves Input
;;  Completion: (6) Multi-Commands" and "(7) Choose All Candidates",
;;  above?  In that case, things get much trickier.
;;
;;  Here's a definition of command `change-font' that takes advantage
;;  of an action function when cycling candidates:
;;
;;  1  (defun change-font ()
;;  2    "Change font of current frame."
;;  3    (interactive)
;;  4   (let* ((orig-frame (selected-frame))
;;  5          (orig-font (frame-parameter nil 'font))
;;  6          (icicle-candidate-action-fn
;;  7           ;; Perform the action on a candidate, without leaving
;;  8           ;; `completing-read'.  You can do this over and over.
;;  9           (lambda (font)
;;  10             (modify-frame-parameters orig-frame
;;  11                                      (list (cons 'font font))))))
;;  12     (condition-case nil
;;  13         (modify-frame-parameters
;;  14          orig-frame
;;  15          (list
;;  16           (cons 'font
;;  17                 ;; Perform the action on your final choice.
;;  18                 (completing-read
;;  19                  "Font: "
;;  20                  (mapcar #'list (x-list-fonts "*")) nil t))))
;;  21       ((quit error)
;;  22        (modify-frame-parameters
;;  23         orig-frame
;;  24         (list (cons 'font orig-font)))))))
;;
;;  As you can see, there is a lot more going on here than in the
;;  previous version.  These are the points to keep in mind, when
;;  defining such a command:
;;
;;  1. Save anything you need to restore, so you can, in effect, undo
;;     the action in case of `C-g' (lines 4-5).
;;
;;  2. Bind `icicle-candidate-action-fn' to the action to perform
;;     (line 3).
;;
;;  3. Perform the action, using `completing-read' to provide the
;;     target candidate (lines 13-20).  Do this in the body of a
;;     `condition-case' (lines 12-24).
;;
;;  4. Restore the original context in the error-handling part of the
;;     `condition-case' (lines 22-24).  Include `quit' in the
;;     error-type list.
;;
;;  The above definition is not quite complete.  To let
;;  `icicle-all-candidates' be able to report on failures, the
;;  `icicle-candidate-action-fn' code should also trap errors and
;;  return nil as an error indicator.
;;
;;  In fact, things can get even hairier (much hairier) still, if the
;;  function at the core of your command does things like create a new
;;  frame - especially on MS Windows, with its click-to-focus window
;;  manager.  The action of `change-font' doesn't do that, but if it
;;  did, you would need to redirect the focus back to the minibuffer
;;  frame, using `select-frame-set-input-focus'.  As an illustration
;;  of what's involved, here's a definition that would deal with such
;;  problems.  It also traps `icicle-candidate-action-fn' errors,
;;  returning nil to report success and the error message to report
;;  failure.
;;
;;  (defun change-font ()
;;    "Change font of current frame."
;;    (interactive)
;;    (let* ((orig-buff (current-buffer))
;;           (orig-window (selected-window))
;;           (orig-frame (selected-frame))
;;           (orig-font (frame-parameter nil 'font))
;;           (icicle-candidate-action-fn
;;            (lambda (candidate)
;;              (condition-case action-fn-return
;;                  (progn
;;                    (modify-frame-parameters
;;                     orig-frame (list (cons 'font candidate)))
;;                    (select-frame-set-input-focus
;;                     (window-frame (minibuffer-window)))
;;                    nil) ; Return nil to report success.
;;                ;; Return error message to report error.
;;                (error (error-message-string action-fn-return))))))
;;      (condition-case act-on-choice
;;          (modify-frame-parameters
;;           orig-frame
;;           (list (cons 'font
;;                       (completing-read
;;                        "Font: " (mapcar #'list (x-list-fonts "*"))
;;                        nil t nil nil nil nil))))
;;        (quit (switch-to-buffer orig-buff)
;;              (modify-frame-parameters
;;               orig-frame (list (cons 'font orig-font))))
;;        (error (switch-to-buffer orig-buff)
;;               (modify-frame-parameters
;;                orig-frame (list (cons 'font orig-font)))
;;               (error (error-message-string act-on-choice))))))
;;
;;  That's a lot of (error-prone) work!  You obviously don't want to
;;  be doing that a lot.  Fortunately, help is on the way: macro
;;  `icicle-define-command'.  Here is how it could be used to define
;;  `change-font'.  The resulting generated code is similar to the
;;  mess shown above.
;;
;;  1  (icicle-define-command
;;  2   change-font "Change font of current frame."
;;  3   (lambda (font)
;;  4     (modify-frame-parameters orig-frame
;;  5                              (list (cons 'font font))))
;;  6   "Font: " (mapcar #'list (x-list-fonts "*"))
;;  7   nil t nil nil nil nil
;;  8   ((orig-frame (selected-frame))
;;  9    (orig-font (frame-parameter nil 'font)))
;;  10  nil
;;  11  (modify-frame-parameters orig-frame
;;  12                           (list (cons 'font orig-font)))
;;  13  nil)
;;
;;  That might not look very readable, but it is straightforward to
;;  use `icicle-define-command'.  The arguments to it are as follows:
;;
;;  Command name    (line 2)
;;  Doc string      (line 2)
;;  Action function (lines 3-5)
;;  Args passed to `completing-read' (lines 6-7)
;;  Additional bindings (lines 8-9)
;;  Additional initialization code (line 10)
;;  "Undo" code to run in case of error or quit (lines 11-12)
;;  Additional code to run at the end (line 13)
;;
;;  The following bindings are pre-included - you can refer to them in
;;  the command body:
;;
;;   `orig-buff'   is bound to (current-buffer)
;;   `orig-window' is bound to (selected-window)
;;
;;  Before running any "undo" code that you supply, the original
;;  buffer is restored, in case of error or user quit.
;;
;;  Most of the arguments to `icicle-define-command' are optional.  In
;;  this case, optional arguments were provided to save (lines 8-9)
;;  and then restore (lines 11-12) the original font and frame.
;;
;;  If the action function that you use to define a multi-command acts
;;  on a file name or a directory name, then you will want to use
;;  `icicle-define-file-command', instead of `icicle-define-command'.
;;  It defines commands that use `read-file-name', rather than
;;  `completing-read', to read their input.  As an example of its use,
;;  here is the definition of `icicle-find-file':
;;
;;  (icicle-define-file-command
;;   icicle-find-file "Visit a file or directory."
;;   find-file "File or directory: ")
;;
;;  The arguments to `icicle-define-file-command' are the same as
;;  those to `icicle-define-command', except for arguments that are
;;  passed to `read-file-name' instead of `completing-read'.
;;
;;  Several top-level Icicles commands have been defined here using
;;  `icicle-define-command' and `icicle-define-file-command'.  You can
;;  use their definitions as models.
;;
;;   `icicle-bookmark'     - jump to a bookmark
;;   `icicle-buffer'       - switch to another buffer
;;   `icicle-clear-option' - set the value of a binary option to nil
;;   `icicle-color-theme'  - change color theme
;;   `icicle-delete-file'  - delete a file or directory
;;   `icicle-doc'          - display the doc of a function or variable
;;   `icicle-execute-extended-command' -
;;                           a multi-command version of `M-x'
;;   `icicle-find-file'    - open a file or directory
;;   `icicle-font'         - change the frame font
;;   `icicle-frame-bg'     - change the frame background color
;;   `icicle-frame-fg'     - change the frame foreground color
;;   `icicle-fundoc'       - display the doc of a function
;;   `icicle-recent-file'  - open a recently used file
;;   `icicle-set-option-to-t' - set the value of a binary option to t
;;   `icicle-toggle-option' - toggle the value of a binary option
;;   `icicle-vardoc'       - display the doc of a variable
;;
;;
;;  See also: library `synonyms.el', which uses macro
;;  `icicle-define-file-command' to define command `synonyms'.  This
;;  command lets you use Icicles completion on input regexps when you
;;  search a thesaurus.

;;
;;
;;  Icicles Improves Input Completion: (10) Global Filters
;;  -----------------------------------------------------
;;
;;  Which completion candidates get displayed?  To review:
;;
;;  1. The domain of discourse, that is, all possible candidates, is
;;     determined by the arguments to `completing-read',
;;     `read-file-name', or `M-x'.
;;
;;  2. You type something in the minibuffer.  This narrows the
;;     possible candidates to those that match your input.  Matching
;;     can be prefix-matching or apropos-matching.
;;
;;  Wouldn't it sometimes be useful to filter #1 in a global way,
;;  before filtering it with your input (#2)?  Functions
;;  `completing-read' and `read-file-name' take a predicate argument,
;;  so that can be used for global filtering.  However, those
;;  functions are usually called from some command, and it would also
;;  be useful to give end users, not just programmers, some way to
;;  globally filter candidates.
;;
;;  For example, if you have a command, like `icicle-buffer', that
;;  reads a buffer name and displays the buffer, some users might
;;  always be interested only in buffers that are associated with
;;  files.  They don't want to see possible candidates like
;;  `*scratch*' and `*Messages*'.  What they need is a way to apply a
;;  global predicate that limits candidates to file-buffer names - but
;;  they don't have access to the call to `completing-read' that is
;;  inside the command definition.
;;
;;  For this reason, some global filtering variables are provided by
;;  Icicles:
;;
;;    `icicle-must-match-regexp', `icicle-must-not-match-regexp',
;;    `icicle-must-pass-predicate', `icicle-extra-candidates'.
;;
;;  The first and second of these are regexps that candidates must
;;  match and must not match, respectively, in order for them to be
;;  displayed.  The third is a predicate that candidates must satisfy.
;;  The fourth is a list of extra candidates to display.  Any of the
;;  filters can be nil, in which case it has no effect.
;;
;;  Variable `icicle-extra-candidates' is not really a "filter".  It
;;  does not restrict the set of possible candidates - rather, it
;;  extends that set.
;;
;;  These global variables are internal variables - they are not meant
;;  to be customized.  If you are not an Emacs-Lisp programmer, you
;;  will not use these variables, but some commands that you use might
;;  provide corresponding global-filter user options.  Icicles
;;  provides user options for command `icicle-buffer', for example:
;;
;;    `icicle-buffer-match-regexp'    - Regexp that buffers must match
;;    `icicle-buffer-no-match-regexp' - Regexp buffers must not match
;;    `icicle-buffer-predicate'       - Predicate buffer must satisfy
;;    `icicle-buffer-extras'          - Extra buffers to display
;;
;;  You might, for instance, customize `icicle-buffer-no-match-regexp'
;;  to not display file-buffers whose names end in `.elc', and
;;  customize `icicle-buffer-predicate' to show only buffers that are
;;  associated with files.  The former would use a value of "\\.elc$",
;;  and the latter would use a value such as this:
;;
;;     (lambda (bufname) (buffer-file-name (get-buffer bufname)))."
;;
;;  If you, as a programmer, write a command, and you want to expose
;;  global filters to users of the command, you should:
;;
;;  1. Create corresponding user options that can be customized.
;;  2. Bind the user options to the corresponding filtering variables.
;;
;;  If you use `icicle-define-command' or `icicle-define-file-command'
;;  to define a command (recommended), then you can simply pass the
;;  filter-variable bindings as part of the BINDINGS argument.
;;
;;  For example, here is the core definition of `icicle-buffer':
;;
;;   (icicle-define-command
;;    icicle-buffer                          ; Command name
;;    "Switch to a different buffer."        ; Doc string
;;    switch-to-buffer                       ; Action function
;;    "Switch to buffer: "                   ; completing-read args
;;    (mapcar (lambda (buf) (list (buffer-name buf)))
;;            (buffer-list))
;;    nil nil (buffer-name (if (fboundp 'another-buffer)
;;                             (another-buffer nil t)
;;                           (other-buffer (current-buffer))))
;;    nil nil nil
;;    ;; Filter bindings
;;    ((icicle-must-match-regexp icicle-buffer-match-regexp)
;;     (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
;;     (icicle-must-pass-predicate icicle-buffer-predicate)
;;     (icicle-extra-candidates icicle-buffer-extras)
;;     (icicle-sort-function icicle-buffer-sort)))

;;
;;
;;  Icicles Improves Input Completion: (11) History Enhancements
;;  ------------------------------------------------------------
;;
;;  One obvious advantage Icicles provides for working with minibuffer
;;  histories is display of the previous inputs that match your
;;  current input.  In vanilla Emacs, the history lists are never
;;  shown as such; you can access previous inputs only one at a time,
;;  in order (with `M-p').  This sounds like a minor advantage, but it
;;  is actually quite helpful in practice.  Among other things, it
;;  means you can work with long history lists in a practical way.
;;
;;  In addition, whenever you display candidate completions in buffer
;;  *Completions*, those that you have used previously are highlighted
;;  slightly, so you can more easily recognize them.  This is another,
;;  minor way in which Icicles helps you with minibuffer histories.
;;
;;  There are two more important ways in which Icicles enhances use of
;;  minibuffer histories.
;;
;;  1. Command `icicle-history' (`M-h' in the minibuffer) matches the
;;     current input against the minibuffer history.
;;
;;  2. Command `icicle-keep-only-past-inputs' (`M-pause' in the
;;     minibuffer) restricts the current explicit set of completion
;;     candidates to those that you have already used previously.  In
;;     other words, it keeps only those candidates that are
;;     highlighted.  To use `M-pause', you must first have used `TAB'
;;     or `S-TAB' to establish an explicit candidate set.
;;
;;  Both `M-h' and `M-pause' can be used toward the same end.  They
;;  both work for all input types.  They both use the appropriate
;;  history list for the current command.  They both provide apropos
;;  completion and cycling for the minibuffer history (as well as
;;  prefix completion, of course).  Use them as another way to search
;;  through a history list or complete to one of its elements.
;;
;;  For example, If you use `C-x C-f' to find a file, and then use
;;  `M-h' or `M-pause', the completion candidates will be the names of
;;  files that you have previously accessed (file names you have input
;;  in the minibuffer), and which match the current minibuffer input.
;;
;;  `M-h' lets you complete your input against the minibuffer input
;;  history.  `M-pause' lets you restrict the current explicit set of
;;  completion candidates to those that are also in the minibuffer
;;  history.
;;
;;  They provide similar functionality in different ways.  The
;;  difference is that `M-pause' takes the current set of matching
;;  candidates into account.  It is a completion-candidates set
;;  operation, similar to those described in section "Icicles Improves
;;  Input Completion: (9) Candidate Sets", above.
;;
;;  This means, in particular, that with `M-pause' you can first
;;  perform set operations on the set of candidates, and then use that
;;  result to restrict the history search.  For example, you can first
;;  complement the candidate set using `C-~', then use `M-pause' to
;;  restrict those candidates to matches in the history list.  In this
;;  way, you avoid including matches from the original match set when
;;  searching the history.
;;
;;  For example: `C-x C-f foo.*\.c$' defines the candidate set as all
;;  files whose names start with `foo' and have extension `c'.  `C-~'
;;  then defines the candidate set as all files whose names are *not*
;;  like that.  `M-pause' then restricts those file-name candidates to
;;  names that you have used before.
;;
;;  A consequence of this difference is that using `TAB' or `S-TAB'
;;  after `M-pause' abandons use of the minibuffer history and starts
;;  a new set of completion candidates.  It simply completes the
;;  current input in the context of the current command; `TAB' and
;;  `S-TAB' have nothing to do with the minibuffer history in this
;;  case.  Using `TAB' or `S-TAB' after `M-h', however, re-completes
;;  your input against the current history list.
;;
;;  Another consequence is that you can use [down] on the candidates
;;  displayed by `M-h', but not on those displayed by `M-pause'.  For
;;  example, to cycle through the doc for each variable that starts
;;  with `icicle-' which you have previously input, you can use `C-h v
;;  icicle- TAB', then repeatedly use [down].
;;
;;  Also, file-name and directory-name completion works differently in
;;  these two commands.  By default, the current directory is (as
;;  always) inserted into the minibuffer by commands such as
;;  `find-file', so either `M-h' or `M-pause' after `C-x C-f' will
;;  match previously input file names from the current directory.
;; 
;;  However, in the case of `M-h', the entire minibuffer input is
;;  matched against the history list, which is a list of absolute file
;;  names.  `M-pause' works only with the current candidate set,
;;  which, if you have already used `TAB' or `S-TAB' in the current
;;  directory, is a set of relative file names in that directory.
;;
;;  This difference has a consequence for apropos (regexp) completion
;;  with `M-h'.  It means that to match a file name using a substring
;;  you must, in the minibuffer, either not specify a directory (erase
;;  it) or explicitly use `.*' before the file-name substring.
;;
;;  For example, with `M-h', `/foo/bar/lph' will not apropos-match the
;;  previously input file name `/foo/bar/alphabet-soup.el'; you should
;;  use either `/foo/bar/.*lph' or `lph' (no directory).
;;
;;  In the case of `M-pause', however, the input is matched against
;;  the history list as restricted by the existing completion list.
;;  And, since apropos file-name completion uses only the relative
;;  file name, without the directory name, as a regexp, the candidate
;;  list that is restricted has already matched the input regexp.  The
;;  action of `M-pause' is simply to filter the list of candidates,
;;  keeping those that are in the history list.  This means that, with
;;  `M-pause', the input `/foo/bar/lph' will match against the
;;  previously input file name `/foo/bar/alphabet-soup.el'.
;;
;;  If this all sounds confusing, just give it a try; it is much
;;  harder to describe than it is to experience.

;;
;;
;;  Icicles Improves Input Completion: (12) Search Enhancements
;;  -----------------------------------------------------------
;;
;;  There are two, unrelated enhancements that Icicles provides for
;;  searching:
;;
;;  - An extension to incremental search that lets you use Icicles
;;    completion against previous search strings.
;;
;;  - A top-level command, `icicle-search', that provides an entirely
;;    new and different (wierd?!) way for you to search.
;;
;;  1. Isearch completion against the search history
;;
;;  When you search incrementally (`C-s'), Emacs (21 or later) lets
;;  you complete your input to a string you have looked for
;;  previously.  In Icicle mode, this feature is enhanced so that you
;;  can use all of the completion behavior provided by Icicles.
;;
;;  In vanilla Emacs, you use `M-TAB' to complete against the search
;;  ring (that is, the search history).  In Icicles, you use `S-TAB'
;;  (`icicle-isearch-complete') to do this - that's what Icicles users
;;  are in the habit of using for (apropos) completion.  They are also
;;  in the habit of using `TAB' for prefix completion, but in Isearch
;;  `TAB' inserts a tab, which is a useful character to include in
;;  search strings.
;;
;;  When you use `S-TAB' while searching, Isearch exits momentarily,
;;  giving way to Icicles completion in the minibuffer (Isearch
;;  actually uses the echo area, not the minibuffer).  You can then
;;  use either `S-TAB' or `TAB' to complete your search string.  After
;;  you finish completing (e.g. by hitting `RET'), Isearch resumes
;;  with the new, completed search string.  It's pretty seamless, and
;;  easier to try than to describe.
;;
;;  One reminder: Using `S-TAB' vs `TAB' for completion against
;;  previous search strings has nothing to do with regexp vs
;;  non-regexp searching.  You can of course use either kind of
;;  searching before or after having used either kind of completion.
;;  Isearch uses different search rings for regexp and non-regexp
;;  searching.  The kind of search in progress (regexp or not) at the
;;  moment when you call for completion determines which search ring
;;  provides the candidates for completion.
;;
;;  2. Command `icicle-search'
;;
;;  The idea behind `icicle-search' is this: Regular expressions are
;;  powerful for searching, but they can also be cumbersome sometimes.
;;  Why not use one simple regexp to set up a set of candidates and
;;  then use a second simple regexp to filter those candidates?
;;
;;  That's how it works.  You enter a regexp, then use `S-TAB' to see
;;  all matches for the regexp in the region (or in the whole buffer,
;;  if there is no active region).  The matches appear as completion
;;  candidates in buffer *Completions*.  As usual, you can complete to
;;  a candidate, or cycle among candidates to choose one.
;;
;;  More usefully, you can type a different regexp in the minibuffer,
;;  and use the various apropos-completion commands to narrow the list
;;  of candidates.  Instead of trying to come up with a head-numbing
;;  regexp expression, it's often easy to get the same result with two
;;  easy regexps!
;;
;;  Most useful of all: You can use the various candidate-action
;;  commands to show the occurrence of each match in the original
;;  buffer you are searching.  You can, for instance, use `C-RET' to
;;  show the occurrence of the current candidate.  Or, you can use
;;  `C-mouse-2', `C-prior', `C-next', `C-up', and `C-down'.  For more
;;  information on these, see section "Icicles Improves Input
;;  Completion: (6) Multi-Commands" and (7) Choose All Candidates.
;;
;;  The initial regexp you use can be anything - it need not match a
;;  single, complete line of text.  It is used over and over in the
;;  region you search, to partition it into matching strings.
;;
;;  `icicle-search' will never replace incremental search - either
;;  regexp or literal string search, but in some cases it can be quite
;;  handy.  Think of it as another tool to add to your search-tool
;;  belt, along with `grep', `occur', and the others.  It does take a
;;  little getting used to, I admit.  Remember, in particular, that
;;  the original regexp you input cannot be changed, without
;;  re-executing `icicle-search'.
;;
;;  And remember too that `C-l' is your friend, to clear the
;;  minibuffer during cycling, retrieving your last real input.  Use
;;  it to modify your second regexp on the fly - the one that filters
;;  the original candidate list.  Have fun!
;;
;;  Oh - And don't forget that you can do things like take the
;;  complement of your fine-tuning regexp matches, within the context
;;  of your coarse-tuning matches (see "Icicles Improves Input
;;  Completion: (9) Candidate Sets", above).  For example, use
;;  `^.*defun.*$' as the main regexp, to find all lines containing
;;  `defun'.  Then type `icicle' to match only the lines with `defun'
;;  that also contain `icicle'.  Then complement (`C-~') that set, to
;;  see the lines that contain `defun' but not `icicle'.
;;
;;  And you can then save that set of matches, and then subtract it
;;  from another set of matches in a different search...  You get the
;;  idea.
;;
;;  When performing set operations combined with `icicle-search', keep
;;  in mind that the saved set does not include any position
;;  information - it is only a set of matching strings.  So, in
;;  particular, a set-union operation (`C-+') is not useful with
;;  `icicle-search' (adding a saved set of strings without positions
;;  is useless).  Still, you can do things like match lines that
;;  contain `defun' followed somewhere by `()', and then subtract the
;;  (saved) set of lines in the same region that contain `icicle'.
;;  Try it in this buffer, using regexps `.*icicle.*$' and
;;  `^*.defun.*().*$'.
;;
;;  One more reminder: When you save a set of completion candidates
;;  (`C->'), make sure you actually have a set of candidates to save!
;;  It is not enough to just enter a regexp at the `icicle-search'
;;  prompt.  You must also use some Icicles command, such as `TAB',
;;  `S-TAB', [next], [down], etc. to tell Icicles how to create the
;;  candidate set - how to match the regexp.

;;
;;
;;  Icicles Improves Input Completion: (13) Compile/Grep Search
;;  -----------------------------------------------------------
;;
;;  In a compilation-results buffer, such as `*grep*', command
;;  `icicle-compilation-search' can be useful for searching among the
;;  result set (hits).  It is the same function as `icicle-search',
;;  except that it calls `compile-goto-error' as the
;;  completion-candidate action function.  That is, you can use
;;  `C-RET', `C-mouse-2', `C-prior', `C-next', `C-up', and `C-down' to
;;  display the target corresponding to each line in the compilation
;;  buffer that matches a regexp.  As for `icicle-search', you can
;;  further narrow the match candidates by typing a second regexp to
;;  search for among the first matches.
;;
;;  Altogether, using this with `grep' gives you two or three levels
;;  of regexp searching: 1) the `grep' regexp, 2) the major
;;  `icicle-search' regexp, and optionally 3) a refining
;;  `icicle-search' regexp.
;;
;;  Note: If you use a non-nil value for `pop-up-frames' on MS
;;  Windows, you will likely run into the Emacs bug described in
;;  section "Note on Non-Nil `pop-up-frames' on MS Windows", below:
;;  When cycling, if the target buffers do not already exist, they
;;  will be opened in new frames, and this will take the focus away
;;  from the compilation-results buffer.  You will need to reselect
;;  the compilation-results buffer manually.  Once the new frames have
;;  been created, there is no problem cycling among the hits.

;;
;;
;;  Icicles Improves Input Completion: (14) Multi-Completions
;;  ---------------------------------------------------------
;;
;;  Function `completing-read' takes a TABLE argument that represents
;;  the set of possible completions to be matched.  TABLE can take
;;  several forms, one of which is an alist of items, where each item
;;  has the form (COMPLETION . VALUE), where COMPLETION is a string.
;;
;;  Icicles extends COMPLETION, here, accepting not only a string, but
;;  a list of strings - so we can speak of a "multi-completion", that
;;  is, a completion that is composed of multiple parts.
;;
;;  The multiple parts are joined together by placing the string
;;  `icicle-list-join-string' between them, pairwise.  By default,
;;  this separator is a newline.  When completing, users can use a
;;  regexp input that takes this separator into account by matching
;;  it, thus, in effect, matching against the multiple parts. A single
;;  regexp is still used, but you can think of it as using multiple
;;  regexps to match the multiple completions.
;;
;;  As a simple example, command `icicle-vardoc' uses COMPLETION
;;  candidates that are each a list of two strings: a variable name
;;  and its doc string.  When you use `icicle-vardoc', you can thus
;;  match a regexp against the overall completion that is composed of
;;  these two strings joined together by a newline.  You can think,
;;  however, in terms of matching against each string separately and
;;  also matching the newline that joins them.
;;
;;  For example, you can use this input with `S-TAB' to match all
;;  variables with `toggle' somewhere in their documentation:
;;
;;    .*
;;    toggle
;;
;;  That is, `.*' followed by a newline, followed by `toggle'.  You
;;  use `C-q C-j' to input the newline, as usual.  Actually, in this
;;  case, just a newline followed by `toggle' will act the same way.
;;
;;  Similarly, you can use the following input to match all variables
;;  with `dired' somewhere in their name and `list' somewhere in their
;;  documentation:
;;
;;    dired.*
;;    list
;;
;;  That is, `dired.*' followed by a newline, followed by `list'.
;;
;;  (You can use commands `icicle-vardoc', `icicle-fundoc', and
;;  `icicle-doc' to search the names and documentation of variables,
;;  functions, or both.  These are all multi-commands, so you can use
;;  `C-next' to display the documentation of each match, in turn.)
;;
;;  As an Emacs-Lisp programmer, you can define other Icicles commands
;;  that use multi-completions.  Depending on your needs, you can of
;;  course bind `icicle-list-join-string' to a different separator.
;;
;;  Note that there is (only) a superficial similarity between Icicles
;;  multi-completion and the functionality provided by function
;;  `completing-read-multiple' of standard library `crm.el'.  The
;;  latter lets you complete multiple strings in the minibuffer, one
;;  at a time.  It involves ordinary Emacs prefix completion, and it
;;  uses the same set of completion candidates for each of the strings
;;  in the input.
;;
;;  By contrast, Icicles multi-completion completes each part of your
;;  input against a different set of completion candidates.  For
;;  example, when you use `icicle-vardoc', it completes the
;;  variable-name part of your input against the names of defined
;;  variables, and the variable-description part against the doc
;;  strings of defined variables.  `completing-read-multiple' lets you
;;  complete several different variable names at the same minibuffer
;;  prompt, but they each complete against the same set of variable
;;  names.

;;
;;
;;  Icicles Improves Input Completion: (15) Completion in Other Buffer
;;  ------------------------------------------------------------------
;;
;;  In addition to input completion, you can use Icicles to complete
;;  words and symbols in other buffers besides the minibuffer.
;;  Icicles enhances:
;;
;;  1) Lisp symbol completion via `ESC-TAB' (`lisp-complete-symbol')
;;
;;  2) word completion using the dynamic abbreviation of standard
;;     Emacs library `dabbrev.el'
;;
;;  Because these enhancements use Icicles completion, you must use
;;  `RET' (or `S-RET') to confirm completion.  This is one difference
;;  in completion behavior that you will notice.  The other difference
;;  is that you have all Icicles features available to you: apropos
;;  (regexp) completion, cycling of candidates, and so on.
;;
;;  Library `dabbrev.el' lets you type a few characters in a buffer
;;  and then prefix-complete them (in the same buffer) to a full word
;;  or symbol name.  The completion candidates come from words or
;;  symbol names in buffers that you are editing.  This functionality
;;  is called "dynamic abbreviation", though that is not a very good
;;  term for it (words are completed, not abbreviated, dynamically).
;;
;;  In Emacs, there are two ways to "dynamically abbreviate" text:
;;
;;  1) `M-/' (command `dabbrev-expand') completes to a candidate word.
;;     Repeating it replaces the completion with a different one -
;;     that is, it cycles candidates in the text buffer (not in the
;;     minibuffer).
;;
;;  2) `M-C-/' (command `dabbrev-completion') completes to the common
;;     root of all completion candidates.  Repeating it displays
;;     buffer *Completions* for you to choose a candidate.  However,
;;     in this case, there is no way to cycle among the candidates.
;;
;;  If there are many candidate completions, then cycling among them
;;  with `M-/' can be tedious.  You can use `M-C-/' to complete to a
;;  common root, thus narrowing the set of candidates, but then you
;;  lose the ability to cycle among them.
;;
;;  If user option `icicle-redefine-standard-commands-flag' is non-nil
;;  (default value), then Icicles redefines `dabbrev-completion' (it
;;  does not change `dabbrev-expand') so that it uses Icicles
;;  completion when you repeat `M-C-/'.  (Before repeating `M-C-/',
;;  the common root is completed as usual.)  You can then use any
;;  Icicles features, such as apropos completion and candidate
;;  cycling.

;;
;;
;;  Inserting Text Found Near the Cursor
;;  ------------------------------------
;;
;;  Some Emacs commands provide, as the default value for minibuffer
;;  input, a word or other text at the cursor position (point).  You
;;  can insert this default value in the minibuffer with `M-n'.
;;  Icicles option `icicle-init-value-flag' can be used to
;;  automatically insert the default value into the minibuffer as an
;;  initial value.
;;
;;  Sometimes you would like to use the text at point, but the command
;;  asking for input does not let you retrieve that text as the
;;  default value.  For example, if the text at point is a file name,
;;  you might like to use it with `C-x f' to open that file.  Or, if
;;  the text is a URL, you might want to visit it using a Web browser.
;;
;;  Some libraries, such as `ffap.el', have as their specific purpose
;;  to let you do this.  "Ffap" stands for `find-file-at-point', the
;;  main command in the library.  It tries to interpret the text at
;;  point and "do the right thing" with it: visit a file, open a URL
;;  in a Web browser, and so on.
;;
;;  If you like, you can use library `ffap.el' with Icicles.  All
;;  Icicles features are then available during file-name and URL
;;  completion.  And if you like `ffap.el', you might also like to try
;;  my extension library `ffap-.el'.
;;
;;  However, I personally don't like some of the ffap behavior.  I
;;  like to control which buffer text I use as minibuffer input and
;;  how that text should be interpreted (file name, URL, and so on).
;;
;;  Icicles provides a simpler way to do this: Just use `M-.' when in
;;  the minibuffer.  It grabs text at or near the cursor and yanks it
;;  into the minibuffer.  Successive uses of `M-.' grab and insert
;;  either alternative bits of text (different text "things") or
;;  successive bits of text.
;;
;;  Option `icicle-thing-at-point-functions' controls which text at or
;;  near the cursor `M-.' inserts into the minibuffer.  It is a cons
;;  cell.
;;
;;  The car of `icicle-thing-at-point-functions' is a list of
;;  functions that grab different kinds of strings at or near point.
;;  By default, there are three functions, which grab 1) the symbol or
;;  file name, 2) the word, 3) the URL at point.  Any number of
;;  functions can be used.  They are used in sequence by `M-.'.
;;
;;  The cdr of `icicle-thing-at-point-functions' is a function that
;;  advances point one text thing.  Each time command `M-.' is used
;;  successively, this is called to grab more things of text (of the
;;  same kind).  By default, successive words are grabbed.
;;
;;  If either the car or cdr is empty, then the other alone determines
;;  the behavior of `M-.'.  Otherwise, option
;;  `icicle-default-thing-insertion' determines whether the car or cdr
;;  is used by `icicle-insert-string-at-point'.
;;
;;  For example, if you set `icicle-default-thing-insertion' to
;;  `more-of-the-same', then repeated use of `M-.' inserts successive
;;  words into the minibuffer.  If you set
;;  `icicle-default-thing-insertion' to `alternatives', then repeated
;;  use of `M-.' inserts a different kind of thing at point: file
;;  name, word, or URL.
;;  
;;  You can use `C-u M-.' at any time to temporarily override the
;;  value of `icicle-default-thing-insertion'.  If you use a numeric
;;  prefix argument N (not just plain `C-u'), then it is the same as
;;  using `M-.' N times with `more-of-the-same' as the value of
;;  `icicle-default-thing-insertion'.  If the numeric argument is
;;  negative, however, then text is grabbed to the left of the cursor,
;;  instead of to the right.
;;
;;  In the case of `alternatives', there are only three possibilities,
;;  by default.  The first function in the list grabs text that has
;;  the syntax of an Emacs-Lisp symbol name, which in practice can
;;  also be a file name or a URL - it can include characters such as
;;  -, /, +, ., :, @, and _.  The second function in the list grabs a
;;  word, which includes letters, ' and -.  The third function grabs a
;;  URL, adding prefix "http://" if needed.  These are the functions
;;  used by default, but you can add to them or replace them.  If you
;;  use my library `thingatpt+.el', then the cursor need not be
;;  exactly on the text - the nearest symbol or word is grabbed.

;;
;;
;;  Icicles Redefines Some Standard Commands
;;  ----------------------------------------
;;
;;  If user option `icicle-redefine-standard-commands-flag' is
;;  non-nil, then Icicles automatically redefines a few standard Emacs
;;  commands when you are in Icicle mode, enhancing them to use
;;  Icicles completion:
;;
;;    `customize-apropos', `customize-apropos-faces',
;;    `customize-apropos-groups', `customize-apropos-options',
;;    `dabbrev-completion', `lisp-complete-symbol',
;;    `repeat-complex-command'.
;;
;;  When you exit Icicle mode, the pre-Icicles definitions are
;;  restored.

;;
;;
;;  Customization and General Tips
;;  ------------------------------
;;
;;  Icicles works especially well with Delete Selection mode, which I
;;  use and recommend.  (Likewise, for PC selection mode, which uses
;;  Delete Selection mode.)  In Delete Selection mode, whenever the
;;  region (selection) is active (highlighted), you can simply type to
;;  replace text in the region, or hit `DEL' (Backspace) or `C-d'
;;  (Delete) to delete the region.
;;
;;  However, library `delsel.el', which provides Delete Selection
;;  mode, binds keys in minibuffer maps that are also bound by
;;  Icicles.  For this reason, if you use both Icicles and Delete
;;  Selection mode, you must turn on Icicle mode after you turn on
;;  Delete Selection mode.  If you forget to do this, you will notice
;;  that `C-g' does not abort minibuffer input.  The remedy is simply
;;  to turn Icicle mode off, then on again.
;;
;;  There are several Icicles user options, and you can also use various
;;  standard user options, including Icomplete options, that control
;;  various aspects of completion.
;;
;;  * Case sensitivity: Standard user options `completion-ignore-case'
;;    and `read-file-name-completion-ignore-case' (for Emacs 21 and
;;    later) control whether completion distinguishes uppercase and
;;    lowercase letters.
;;
;;  * User options `icicle-point-position-in-candidate',
;;    `icicle-mark-position-in-candidate', and
;;    `icicle-change-region-background-flag', and face
;;    `icicle-region-background', are all used to define the region
;;    (the selected text) when cycling completion candidates.  The
;;    region is active, so you can easily delete it or replace it.
;;
;;  * User option `icicle-point-position-in-candidate' defines the
;;    minibuffer cursor position (point) while cycling candidate
;;    completions.  By default, the cursor is placed at the end of the
;;    root being completed.  You can instead place it at the root
;;    beginning or at the beginning or end of the complete minibuffer
;;    input.  For file-name input, the beginning of minibuffer input
;;    starts after the directory name (which is inserted
;;    automatically).
;;
;;  * Similarly, user option `icicle-mark-position-in-candidate'
;;    defines the position of the mark; by default, it is at the end
;;    of the input.  Together, these two options control the size and
;;    placement of the region in a flexible way.  You can make the
;;    region include all of the input, only the root, from beginning
;;    to root, or from root to end.  You can put the cursor at either
;;    end of the region.  You can get rid of the region altogether, by
;;    making point and mark coincide (at any of the possible
;;    positions).
;;
;;  * Because the region background color is often quite different
;;    from the frame background color (in order to have it stand out),
;;    it can be a bit hard to read the completion candidates when the
;;    region is highlighted during input cycling.  If user option
;;    `icicle-change-region-background-flag' is non-nil, however, then
;;    the region background is changed to a color that differs only
;;    slightly from the frame background, making it easier to read the
;;    completion candidates.  The actual background color used is the
;;    value of `icicle-region-background', which you can customize.
;;    If you make this color the same as the frame background, then
;;    the region background is, in effect, invisible.
;;
;;  * The default value of `icicle-change-region-background-flag' is
;;    determined by the current value of `delete-selection-mode', that
;;    is, whether or not Delete Selection mode is enabled, when
;;    `icicles.el' is loaded.  For this reason, if you use Delete
;;    Selection mode and you want the region background to change in
;;    the minibuffer, you should either turn on Delete Selection mode
;;    before loading `icicles.el' or explicitly customize
;;    `icicle-change-region-background-flag' to non-nil.
;;
;;  * User option `icicle-init-value-flag' controls the treatment of a
;;    default value for minibuffer input.  This includes not only
;;    functions that read input with completion (`completing-read',
;;    `read-file-name'), but also other input-reading functions:
;;    `read-from-minibuffer' and `read-string'.  Non-nil means to
;;    automatically insert the default value into the minibuffer as an
;;    initial value.  Standard Emacs behavior is for the default value
;;    not to be inserted.  I prefer to have it inserted, as I often
;;    use the default value (perhaps editing it).  The option is nil
;;    by default only because people are not used to the (better)
;;    behavior of `insert'.  I recommend that you try `insert' for a
;;    while, before giving up on it.  If you leave this as nil,
;;    remember that you can always insert the default value manually
;;    with `M-n'.
;;
;;  * The particular non-nil value of `icicle-init-value-flag'
;;    controls whether or not the initial value is preselected, and,
;;    if preselected, where to leave the cursor: at the beginning or
;;    end of the value.  Preselecting the value can be useful in
;;    Delete Selection mode or PC Selection mode, because it makes it
;;    easy to replace that value by typing characters, or delete it by
;;    hitting `DEL' (Backspace) or `C-d' (Delete).  However, all of
;;    the initial input is lost if you type or hit `C-d' or `DEL',
;;    which is inconvenient if you want to edit it only slightly.
;;
;;  * User option `icicle-thing-at-point-functions' controls which
;;    text at or near the cursor `M-.' inserts into the minibuffer.
;;    Option `icicle-default-thing-insertion' determines whether
;;    repeating `M-.' inserts different alternatives (file name, word,
;;    etc.) or inserts more of the same (words, by default).  See
;;    "Inserting Text Found Near the Cursor", above.
;;
;;  * Faces `icicle-root-highlight-minibuffer' and
;;    `icicle-root-highlight-Completions' are used to highlight the
;;    root being completed, in the minibuffer and in buffer
;;    *Completions*, respectively.  Face `icicle-complete-input' is
;;    used to highlight minibuffer input when it is complete.  You can
;;    customize these, as well as the other Icicles faces.
;;
;;  * User option `icicle-incremental-completion-flag' controls
;;    whether or not *Completions* is updated incrementally
;;    (icompletion) as you type.  For more information, see "Icicles
;;    Improves Input Completion: (4) Icompletion", above.
;;
;;  * User option `icicle-show-Completions-initially-flag' controls
;;    whether or not buffer *Completions* is shown initially, without
;;    your needing to hit `TAB' or `S-TAB' to show it.  The default
;;    value is nil, meaning that *Completions* is not shown until you
;;    hit `TAB' or `S-TAB'.  More typical than setting this option to
;;    non-nil globally is to bind it to non-nil in Lisp code, to
;;    display *Completions* as a menu. For example, pass a non-nil
;;    binding to `icicle-define-command', to create a command that
;;    displays a multiple-choice menu.
;;
;;  * User option `icicle-Completions-frame-at-right-flag' controls
;;    whether `icicle-candidate-action' moves the frame showing buffer
;;    *Completions* to the right, out of the way of other frames.
;;    This can be useful if you use one-buffer-per-frame (non-nil
;;    `pop-up-frames').  In that case, I recommend that you also try
;;    my library `oneonone.el'.  See section "Note on Non-Nil
;;    `pop-up-frames' on MS Windows", below, for more advice about
;;    non-nil `pop-up-frames'.
;;
;;  * User option `icicle-sort-function' controls the order of
;;    completion candidates during cycling and in buffer
;;    *Completions*.  If nil, then no sorting is done.  If non-nil,
;;    then the value must be a string-comparison function - the
;;    function is passed to the standard function `sort' to do the
;;    sorting.  The default value for `icicle-sort-function' is
;;    `string-lessp', which sorts alphabetically.  You can toggle
;;    sorting at any time, using command `icicle-toggle-sorting'.  If
;;    you are an Emacs-Lisp programmer and you write new commands
;;    using Icicles functionalities, you can bind this variable
;;    temporarily to any sort function you need.
;;
;;  * User option `icicle-completion-nospace-flag' can be used to
;;    control ignoring of completion candidates that start with a
;;    space unless the input to be completed also starts with a space.
;;    The effect is similar to that of the NOSPACE argument to
;;    `all-completions'.
;;
;;  * User option `icicle-redefine-standard-commands-flag' controls
;;    whether Icicles redefines some standard commands, enhancing them
;;    to use Icicles completion.  A non-nil value causes redefinition.
;;
;;  * User options `icicle-buffer-match-regexp',
;;    `icicle-buffer-no-match-regexp', `icicle-buffer-predicate', and
;;    `icicle-buffer-extras' determine the behavior of commands
;;    `icicle-buffer' and `icicle-buffer-other-window'.  They
;;    determine the set of buffer-name candidates initially available
;;    for completion.  The first three restrict this set to names that
;;    satisfy the properties they specify.  Option
;;    `icicle-buffer-extras' lets you add additional buffer names to
;;    the set of candidates, after restriction by the other options.
;;    Since these are user options, they provide an additional, more
;;    static way to filter the set of candidates.  Typing input
;;    (e.g. a regexp) then dynamically filters the result of applying
;;    the filter options.
;;
;;  * User option `icicle-buffer-sort' is a predicate used to sort
;;    buffer-name candidates in commands `icicle-buffer' and
;;    `icicle-buffer-other-window'.  One possible value is
;;    `icicle-buffer-sort-*...*-last', which sorts names of internal
;;    buffers, which begin with `*', after other buffer names.
;;
;;  * User option `icicle-buffer-configs' is a list of named
;;    configurations of options `icicle-buffer-match-regexp',
;;    `icicle-buffer-no-match-regexp', `icicle-buffer-predicate',
;;    `icicle-buffer-extras', and `icicle-buffer-sort'.  You use
;;    command `icicle-buffer-config' to choose one of the
;;    configurations to be current.  You can use commands
;;    `icicle-add-buffer-config' and `icicle-remove-buffer-config' to
;;    add and remove configurations from the list.
;;
;;    Example: A configuration such as the following, named "Files and
;;    Scratch", defines `icicle-buffer-predicate' to display only file
;;    buffers, and it defines `icicle-buffer-extras' to include the
;;    extra buffer `*scratch*':
;;
;;     ("Files and Scratch" nil nil
;;      (lambda (bufname) (buffer-file-name (get-buffer bufname)))
;;      ("*scratch*") icicle-sort-function)
;;
;;    The idea of buffer-option configurations was borrowed from
;;    library `bs.el', by Olaf Sylvester <olaf@geekware.de>.
;;
;;  * User options `icicle-require-match-flag' and
;;    `icicle-buffer-require-match-flag' let you override the value of
;;    the REQUIRE-MATCH argument provided to `completing-read' or
;;    `read-file-name'.  They are mainly provided for use (binding) in
;;    `icicle-define-command' and `icicle-define-file-command', but
;;    you may also use them globally, if you wish.
;;
;;    A typical use is made in the definition of command
;;    `icicle-buffer': `icicle-buffer-require-match-flag' is used to
;;    bind `icicle-require-match-flag', so that you can, for example,
;;    match only existing buffers and be able to match on partial
;;    input without explicitly completing (hitting `TAB' or `S-TAB').
;;    Simply set the option to `partial-match-ok' to get this
;;    behavior.  To apropos-complete and exit the minibuffer, use
;;    `S-RET' instead of `RET'.  See "Exiting the Minibuffer Without
;;    Confirmation: `S-RET'", above, for more information.
;;
;;  * User option `icicle-list-join-string' is described in section
;;    "Icicles Improves Input Completion: (14) Multi-Completions",
;;    above.  It is the separator string that joins together the parts
;;    of multi-completions.
;;
;;  * User options `icicle-regexp-search-ring-max' and
;;    `icicle-search-ring-max' act as `regexp-search-ring-max' and
;;    `search-ring-max', respectively, when you are in Icicle mode.
;;    (When you exit Icicle mode, `regexp-search-ring-max' and
;;    `search-ring-max' are restored.)  The reason for having these
;;    options is that with Icicles you will likely want to use a much
;;    longer search history.  By default, these are as large as
;;    possible (virtually unlimited).
;;
;;    Suggestion: If you use library `savehist.el' (recommended),
;;    customize `savehist-additional-variables' to include variables
;;    `search-ring' and `regexp-search-ring', so that your search
;;    histories will be saved between Emacs sessions.
;;
;;    Note: You can clear (empty) a given search history with command
;;    `icicle-clear-option'.  For example, to clear the
;;    regular-expression search history, do this:
;;
;;      `C-u M-x icicle-clear-option RET regexp-search-ring RET'
;;
;;    (The `C-u' is needed because this variable is not a user
;;    option.)  If you use my library `misc-cmds.el', you can clear
;;    search histories easier, using commands `clear-search-history',
;;    `clear-regexp-search-history', and `clear-search-histories'.
;;
;;  * Non-nil user option `icicle-inhibit-reminder-prompt-flag'
;;    inhibits the addition of a reminder about Icicles bindings to
;;    the minibuffer prompt.  If nil (the default value), then a
;;    reminder such as "(<S-tab>, TAB: list, C-h: help)" is added to
;;    the prompt (if the window is wide enough).  If you are already
;;    used to using Icicles, you might want to set this option to t to
;;    save some space in the minibuffer.

;;
;;
;;  File-Name and Directory-Name Completion Tips
;;  --------------------------------------------
;;
;;  * Function `icicle-sort-dirs-last' is provided as a possible value
;;    for user option `icicle-sort-function'.  It treats file and
;;    directory names specially, sorting directory names after file
;;    names; otherwise, it is the same as `string-lessp'.  (You can of
;;    course reach directory names before, instead of after, file
;;    names, by using [up] and [prior] instead of [down] and [next].)
;;
;;  * User option `icicle-cycle-into-subdirs-flag' controls whether or
;;    not minibuffer-input cycling explores subdirectories.  By
;;    default, it is nil, meaning that cycling does not descend into
;;    subdirectories.
;;
;;    non-nil - When this option is non-nil, you might want to use a
;;          function such as `icicle-sort-dirs-last' for option
;;          `icicle-sort-function', to prevent cycling depth first
;;          into the subdirectories.
;;
;;    nil - When this option is nil, you can still choose to cycle
;;          into a given directory (which is why nil is the default
;;          value).  When cycling reaches a candidate directory that
;;          you want to cycle through, just: 1) move the cursor
;;          (e.g. `C-e'), 2) hit `TAB' or `S-TAB' to "complete" the
;;          candidate, and then 3) use any of the cycle keys, such as
;;          [up], to cycle within the candidate directory.
;;
;;          Although the candidate directory was already completed by
;;          cycling, moving the cursor and explicitly "completing" it
;;          tells Icicles that you want to treat the candidate in the
;;          minibuffer as real input, just as if you had typed it, not
;;          merely as a cycling candidate.
;;
;;  * You can use `..' during completion to access a parent directory,
;;    and you can use `/' and `~/' to shadow input to the left.  There
;;    is currently no special treatment of MS Windows drive letters
;;    (e.g. `C:') - I use Cygwin on Windows.
;;
;;  * Standard user option `completion-ignored-extensions' controls
;;    which file names are ignored for completion and completion
;;    cycling.  You can toggle this ignoring at any time, with
;;    `icicle-toggle-ignored-extensions', bound to `C-.' in the
;;    minibuffer.
;;
;;  * Remember that you can use a regular expression to
;;    apropos-complete file names.  This is a powerful feature.  Do
;;    not confuse its use with the ability to use shell wildcards to
;;    access multiple files at once.  For example, if you use `C-x 4 f
;;    *.el RET', then all files with suffix `el' will be opened.
;;    Regexp matching is used only for apropos (not prefix) completion
;;    and cycling.  See section "What About Special-Character
;;    Conflicts?", above.
;;
;;  * You can use `$' for both environment variables and as a regexp
;;    special character.  For example, you can use a pattern such as
;;    `$HOME.*t$' to match the files in your home directory (`$HOME')
;;    whose names end in `t'.
;;
;;  * You can use the idiom `\W$' as input to match only directories,
;;    when a command asks for a file or directory name.  The `\W' says
;;    to match any non word-syntax character.  The `$' says to match
;;    this at the end of the name.  This works because directory names
;;    appear as completion candidates with a trailing slash (`/'), and
;;    slash (`/') is about the only non word-syntax character that is
;;    likely to appear in file-name completions.
;;
;;  * Only the file name itself, not the directory portion, is used
;;    for matching.  The behavior is thus the same whether or not the
;;    directory is present in the minibuffer. If you prefer, you can
;;    delete the directory first, using `M-S-backspace' (the
;;    `default-directory' is used, by default).
;;
;;    This means, in particular, that you can use apropos completion
;;    to match a substring, without needing to prefix the substring
;;    with `.*' in the minibuffer.  For example, to match file
;;    `favorite-foo-file.bar' in directory `/some/path/to/my/', you
;;    need not use `/some/path/to/my/.*foo'; it is sufficient to use
;;    either `foo' or `/some/path/to/my/foo'.
;;
;;  * You can use library `ffap.el', if you like, with Icicles, to
;;    pick up the file, directory, or URL name under the cursor.  All
;;    Icicles features are available during file-name and URL
;;    completion.  If you like `ffap.el', you might also like to try
;;    my extension library `ffap-.el'.
;;
;;  See also section "Customization and General Tips", above, for
;;  general tips about using Icicles.  Many of those tips apply also
;;  to file-name and directory-name completion.

;;
;;
;;  Key Bindings
;;  ------------
;;
;;  *** Icicles does not change any of your global key bindings. ***
;;
;;  There is an exception: Icicles adds a few menu items to a couple
;;  of your menu-bar menus.  These are enabled only when you are in
;;  Icicle mode.  Other than that, Icicles binds keys in its own
;;  Icicle-mode keymap, and it binds keys in minibuffer-local keymaps.
;;
;;  In Icicle mode, various Icicles commands are added to menu-bar
;;  menus.  File commands are added to the File menu, and so on.
;;  Those that don't belong naturally to any existing menu-bar menu
;;  are added to a new Icicles menu.  Whatever the menu they appear
;;  in, Icicles menu items are enabled only when Icicle mode is
;;  active.  Those that are in a menu other than the Icicles menu have
;;  "[Icy]" prefixed to their name.
;;
;;  In addition to menu-bar bindings, the following key bindings are
;;  made for the minibuffer completion keymaps.  They are in effect
;;  whenever you are using the minibuffer for input with completion
;;  (e.g. `completing-read', `read-file-name', `M-x').
;;
;;    Keys bound globally to `next-line' and `previous-line' are bound
;;    to `icicle-next-prefix-candidate' and
;;    `icicle-previous-prefix-candidate'.  Those are the commands
;;    that cycle candidate prefix completions.  By default, this means
;;    keys [down], [up], `C-n', and `C-p'.
;;
;;    Keys bound globally to `scroll-up' and `scroll-down' are bound
;;    to `icicle-next-apropos-candidate' and
;;    `icicle-previous-apropos-candidate'.  Those are the commands
;;    that cycle candidate apropos completions.  By default, this
;;    means keys [next], [prior], `C-v', and `M-v'.
;;
;;    Keys bound globally to `help-command' (`C-h', `f1') are bound to
;;    `icicle-completion-help': Pop up a *Help* buffer with
;;    information on using completion.
;;
;;    Keys bound globally to commands that perform simple text
;;    insertion, deletion, and transposition operations - commands
;;    such as `self-insert-command' - are bound to Icicles versions of
;;    those commands that do the same thing but also provide apropos
;;    icompletion.  This includes keys such as `C-d', `M-d', `C-y',
;;    `C-k', and `C-w' (and lots more).  See section "Icicles Improves
;;    Input Completion: (4) Icompletion", above.
;;
;;    [insert] - `icicle-switch-to-Completions-buf': Move cursor to
;;               the current candidate in buffer *Completions*.
;;
;;    `TAB'    - `icicle-prefix-complete': Complete current input in
;;               minibuffer, as a prefix.  If there is more than one
;;               prefix-completion candidate, display them in buffer
;;               *Completions*, highlighting the common prefix.  This
;;               replaces `minibuffer-complete'.
;;
;;    `M-SPC' - `icicle-prefix-word-complete': Complete current input
;;               in minibuffer, as a prefix, a single word at a time.
;;               This replaces `minibuffer-complete-word'.  In fact,
;;               it is the value of `icicle-word-completion-key' that
;;               is bound to this command; `M-SPC' is the default
;;               value of this user option.
;;
;;    `SPC'    - `icicle-self-insert' (see above): Insert a space.
;;
;;    [S-tab]  - `icicle-apropos-complete': Like `TAB', but use
;;               apropos completion.
;;
;;  The following minibuffer binding is made to clear minibuffer
;;  input.  It has no special relation to completion or completion
;;  cycling, except that it is handy for editing and removing
;;  completions (e.g. default or initial values) in the minibuffer:
;;
;;    [M-S-backspace] and [M-S-delete] - `icicle-erase-minibuffer'
;;
;;  The following minibuffer binding can be used to get rid of a
;;  completion inserted during cycling, and retrieve the last real
;;  input:
;;
;;    `C-l' - `icicle-retrieve-last-input'
;;
;;  `C-l' also has another use: You can use it to retrieve your last
;;  input in case you never actually entered that input (via `RET').
;;  For example, suppose that you used `C-h v RET hook' to examine
;;  various hook variables, and you did this using`C-next' to display
;;  their documentation.  If you finished the command by just typing
;;  `C-g', then your input (`hook') was never really entered, so it is
;;  not available via the minibuffer history (`M-p').  You can
;;  retrieve it with `C-l', to use it again, in your next command.
;;
;;  You of course have the standard access to the minibuffer history,
;;  via `M-p', `M-n', `M-r', and `M-s'.  In addition to these, the
;;  following minibuffer bindings let you use apropos completion on
;;  the current minibuffer history list.  For explanation, see
;;  "Icicles Improves Input Completion: (11) History Enhancements",
;;  above.
;;
;;    `M-h'     - `icicle-history'
;;    `M-pause' - `icicle-keep-only-past-inputs'
;;
;;  The following minibuffer bindings let you act on candidate
;;  completions.  For explanation, see "Icicles Improves Input
;;  Completion: (6) Multi-Commands" and "(7) Choose All Candidates",
;;  above.
;;
;;    `C-RET'     - `icicle-candidate-action': current candidate
;;    `C-!'       - `icicle-all-candidates-action': all candidates
;;    `C-up'      - `icicle-previous-prefix-candidate-action'
;;    `C-down'    - `icicle-next-prefix-candidate-action'
;;    `C-prior'   - `icicle-previous-apropos-candidate-action'
;;    `C-next'    - `icicle-next-apropos-candidate-action'
;;    `C-mouse-2' - `icicle-mouse-candidate-action': clicked candidate
;;
;;  (The binding for `icicle-mouse-candidate-action' is actually in
;;  the *Completions* buffer.)
;;
;;  The following minibuffer bindings let you perform set operations
;;  on sets of completion candidates.  For explanation, see section
;;  "Icicles Improves Input Completion: (9) Candidate Sets".
;;
;;    `C-~'     - `icicle-candidate-set-complement'
;;    `C--'     - `icicle-candidate-set-difference'
;;    `C-+'     - `icicle-candidate-set-union'
;;    `C-*'     - `icicle-candidate-set-intersection'
;;    `C->'     - `icicle-candidate-set-save': save current set
;;    `C-<'     - `icicle-candidate-set-retrieve': retrieve saved set
;;    `C-%'     - `icicle-candidate-set-swap': swap saved and current
;;    `C-:'     - `icicle-candidate-set-define': define current (Lisp)
;;
;;  The following minibuffer bindings let you toggle Icicles options.
;;
;;    `C-,'     - `icicle-toggle-sorting'
;;    `C-.'     - `icicle-toggle-ignored-extensions': file extensions
;;
;;  The following bindings are made for `completion-list-mode', that
;;  is, for buffer *Completions*, which shows the list of candidate
;;  completions:
;;
;;    [left], [right] - Navigate backward & forward among candidates
;;    [up], [down]    - Navigate up & down among candidates
;;    [insert]        - `icicle-switch-to-minibuffer':
;;                        Move cursor to the minibuffer, with the
;;                        current *Completions* candidate as input
;;    `C-g', `q'      - `icicle-abort-minibuffer-input'
;;    `C-mouse-2'     - `icicle-mouse-candidate-action'
;;
;;  If you are used to using [down], [up], `C-n', and `C-p' for the
;;  minibuffer history, you can restore those bindings and bind the
;;  corresponding Icicles commands to different keys.  See the
;;  definition of function `icicle-rebind-completion-maps' for the
;;  relevant code.
;;
;;  Other, suggested key bindings - put this in your ~/.emacs file:
;;
;;    ;; Search - use `C-c C-s' or some other key
;;    (global-set-key "\C-c\C-s" 'icicle-search)
;;    (define-key compilation-minor-mode-map "\C-c\C-s"
;;                'icicle-compilation-search)
;;
;;    ;; Replacements for `switch-to-buffer(-other-window)'.
;;    (define-key ctl-x-map   "b" 'icicle-buffer)
;;    (define-key ctl-x-4-map "b" 'icicle-buffer-other-window)
;;
;;    ;; Replacements for `find-file(-other-window)'.
;;    (define-key ctl-x-map   "\C-f" 'icicle-find-file)
;;    (define-key ctl-x-4-map "f"    'icicle-find-file-other-window)
;;
;;    ;; Multi-command version of `M-x'.
;;    (global-set-key "\M-x" 'icicle-execute-extended-command)

;;
;;
;;  Note to Programmers
;;  -------------------
;;
;;  1. Function `icicle-next-candidate' is a general framework for
;;     letting users cycle completions of partial input strings.  I
;;     use it to define the cycling behavior for both prefix and
;;     apropos completions.  You can use it to easily define other,
;;     application-specific input matching/completion/cycling
;;     behavior.  Just supply it with a function that takes the
;;     current partial user input (a string) and returns a list of
;;     candidate completions, however those might be defined.
;;
;;  2. Use an input-completion read function, such as
;;     `completing-read' or `read-file-name', when you read input!
;;     There is almost never a reason not to use an input-completion
;;     function when reading user input - especially considering that
;;     you need not always provide a REQUIRE-MATCH argument.
;;
;;     Try also to find an appropriate PREDICATE argument, and a good
;;     set of default values to pass to `completing-read' as its TABLE
;;     argument.  Too often, I think, we use an overly general TABLE
;;     argument, such as the `obarray', and we don't provide a (good)
;;     PREDICATE.  Using an input-completion function with an
;;     appropriate candidate completion list and predicate can help
;;     users considerably.  I'm as guilty of TABLE and PREDICATE
;;     laziness as anyone, by the way.
;;
;;  3. Another of my libraries that can help programmers provide
;;     default values is `thingatpt+.el'.  It provides functions for
;;     picking up symbols, sexps, numbers, words, and other sorts of
;;     thing near the text cursor (`point').

;;
;;
;;  La Petite Histoire
;;  ------------------
;;
;;  1. This library started out life as `elect-mbuf.el', by Hans Koomen.
;;
;;    Original posting:
;;    From koomen@cs.rochester.edu Mon Jun 19 19:27:58 1989
;;    To: info-gnu-emacs@prep.ai.mit.edu
;;    Cc: Hans <Koomen@cs.rochester.edu>
;;    Subject: elect-mbuf.el
;;    Date: Tue, 13 Jun 89 15:17:07 -0400
;;
;;  2. I hacked and enhanced the library in various relatively minor ways
;;  over the years, maintaining it as `elect-mbuf.el' - see details
;;  under "Change log", below.
;;
;;  I did not change the main functionality of the library during this
;;  period: it always cycled the COMPLETE list of (prefix) completion
;;  candidates passed to `completing-read'; it did not update the
;;  candidate list based on the current minibuffer contents.
;;
;;  So, for instance, if you had `M-x for' in the minibuffer, `C-n'
;;  would cycle among ALL Emacs commands, not just those that start
;;  with "for".  I used the library this way for fifteen years without
;;  thinking much about this behavior or the code behind it.
;;
;;  3. In July 2005, Lennart Borgman gave `elect-mbuf.el' a quick try,
;;  and intuitively expected to see behavior along the lines that you
;;  see now for prefix completion:
;;
;;  a. `C-n' should cycle completions relative to the current input,
;;     not all completions supplied to `completing-read'.
;;  b. If buffer *Completions* is displayed, `C-n' should highlight
;;     the current candidate there.
;;
;;  Good idea Lennart (<lennart.borgman.073@student.lu.se>).  So I
;;  implemented that behavior, and renamed the library "Icicles" (for,
;;  I suppose, "input cycles" or some such - or because it's "cool").
;;
;;  4. The code changes I made to implement #3 (completion cycling
;;  relative to current input) made me realize that other completion
;;  matchings could be implemented in a similar way.  Prefix
;;  completion (the completion provided by Emacs) is handy, but it is
;;  also sometimes a bit limited.  The idea of apropos completion
;;  occurred to me, and I implemented that as well.
;;
;;  5. I extended the library quite a bit more, in terms of
;;  convenience (highlighting, treatment of buffer *Completions*,...,
;;  but also in terms of functionality.  In particular, it now treats
;;  file names too.  And, because Emacs 21 and later versions use
;;  `read-file-name' for `find-file' and so on, Icicles now treats
;;  `read-file-name' the same as `completing-read'.
;;
;;  6. On another suggestion from LennartBorgman, I made Icicles take
;;  advantage of Delete Selection mode.  And I finally implemented it
;;  as a minor mode.
;;
;;  7, 8, 9,...  One thing has led to another, and I've just kept
;;  adding features.  Feature creep, I guess.  But the more I play
;;  with Icicles, the more I imagine new ways it might be made more
;;  useful.

;;
;;
;;  Note on Non-Nil `pop-up-frames' on MS Windows
;;  ---------------------------------------------
;;
;;  If you use `pop-up-frames' = t, like I do, you might have noticed
;;  that Emacs completion does not play well with using separate
;;  frames for each buffer.  In particular, it does not play well with
;;  having a separate frame for buffer *Completions*.  When you try to
;;  complete input using `TAB', a new frame is created for buffer
;;  *Completions*, and, at least on MS Windows, it is selected, taking
;;  the input focus away from the original frame's minibuffer!
;;
;;  This means that, once the *Completions* buffer has been displayed
;;  in a separate frame, you cannot, for instance, cycle completion
;;  candidates, without first reselecting the original frame manually.
;;  You cannot even use normal completion - you cannot add text in the
;;  minibuffer, or delete text there, because the minibuffer in the
;;  original frame no longer has the input focus.  Bummer.
;;
;;  In general, Emacs does not play too well with one-buffer-per-frame
;;  (`pop-up-frames' = t), and this is a good example of that general
;;  problem.
;;
;;  I reported this Emacs bug.  It should be corrected in Emacs 22.x.
;;
;;  I don't have this problem of loss of frame input focus in my own
;;  setup, even though I use `pop-up-frames' = t, because I use my
;;  library `oneonone.el'.  (Try it!)  If you need a solution while
;;  waiting for Emacs 22, you can try doing something similar to what
;;  I do in `oneonone.el':
;;
;;  1. Use dedicated frames for both *Completions* and the minibuffer.
;;
;;  2. Display buffer *Completions* using a special-display function
;;  that explicitly redirects the input focus from the *Completions*
;;  frame back to the minibuffer frame.

;;
;;
;;  Maybe To Do?
;;  ------------
;;
;;  1. Write a command to use Icicles to navigate among tags. 
;;
;;  2. Consider doing the key binding & unbinding differently - copy
;;     standard bindings when enter `icicle-mode' and restore them
;;     when leave it?
;;
;;  3. Replace redefinition of `exit-minibuffer' and/or
;;     `minibuffer-complete-and-exit' with icicles-only commands.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/02/25 dadams
;;     Added: icicle-narrow-candidates (bound to M-*), icicle-icicle-completing-p,
;;            icicle-set-calling-cmd, icicle-reset-icicle-completing-p, 
;;            icicle-run-icicle-(pre|post)-command-hook.
;;     Add all hooks in icicle-mode only, except for minibuffer-local hooks (pre- and post-command).
;;     Remove all hooks when exit Icicle mode.
;;     icicle-completing-read, icicle-read-file-name:
;;       Add catch icicle-read-top.  Set icicle-icicle-completing-p.
;;       Separate case of not Icicle mode from other no-prompt cases.
;;     Reordered some groups of functions.
;; 2006/02/24 dadams
;;     icicle-candidate-set-1: Treat empty set.
;; 2006/02/21 dadams
;;     icicle-prefix-complete: Implemented icompletion here, as in icicle-apropos-complete-1.
;;     icicle-call-then-update-Completions:
;;       Use icicle-last-completion-command, not icicle-apropos-complete.
;;     Renamed icicle-apropos-icompleting-p to icicle-icompleting-p.
;;     Added: icicle-(kill|delete)(-backward)-*, icicle-yank etc.  Bound them.
;;     Added: icicle-call-then-update-Completions.
;;     Added: icicle-incremental-completion-p.
;;       Use in place of icicle-incremental-completion-flag everywhere.
;;       Upgrade from t in icicle-display-candidates-in-Completions.
;;       Reset in icicle-minibuffer-setup.
;;     icicle-isearch-complete: Use search-ring symbol as history arg to completing-read.
;;     icicle-display-candidates-in-Completions, icicle-keep-only-past-inputs, icicle-history:
;;       Ensure that minibuffer-history-variable is a list.
;;     Fixed typos: icicle-keep-past-inputs -> icicle-keep-only-past-inputs.
;; 2006/02/20 dadams
;;     icicle-insert-string-at-point: Treat negative prefix arg.
;;     Added: icicle-signum.
;;     icicle-insert-thing: Remove text properties of string to insert.
;; 2006/02/19 dadams
;;     icicle-thing-at-point-functions: Added function to grab successive text.
;;     icicle-insert-string-at-point: Treat successive-grab fn and prefix arg.
;;     Added: icicle-default-thing-insertion, icicle-default-thing-insertion-flipped-p,
;;            icicle-insert-string-at-pt-(start|end), icicle-successive-grab-count,
;;            icicle-insert-thing.
;;     Renamed: icicle-insert-string-near-point to icicle-insert-string-at-point.
;; 2006/02/18 dadams
;;     icicle-retrieve-last-input: Don't reset icicle-last-completion-command unless interactive.
;;     icicle-candidate-set-complement, icicle-keep-only-past-inputs:
;;       Use icicle-retrieve-last-input.
;;     icicle-keep-only-past-inputs: Rewrote modeled on icicle-apropos-complete:
;;       Take into account singleton and empty candidate set.
;;       Provide input to icicle-display-ca*.
;;       Set icicle-last-completion-command.
;;     icicle-history: Force redisplay of *Completions*.  Don't set this-command.
;;     icicle-completing-read: Ensure icicle-initial-value is not nil.
;;     icicle-save-or-restore-input: Don't restore empty input.
;;     icicle-recompute-candidates:
;;       Don't recompute if last completion cmd was icicle-keep-only-past-inputs.
;;     Added: icicle-historical-candidate, icicle-keep-only-past-inputs.
;;     icicle-display-candidates-in-Completions: Use icicle-historical-candidate.
;;     Bind icicle-keep-only-past-inputs to M-pause in minibuffer completion maps.
;; 2006/02/17 dadams
;;     Added: icicle-complete-input-overlay, icicle-highlight-complete-input,
;;            icicle-complete-input.
;;     icicle-(prefix|apropos)-complete(-1): Use icicle-highlight-complete-input.
;;     Added icicle-inhibit-reminder-prompt-flag.  Thx to Jonathan Simms for the suggestion.
;;     icicle-completing-read, icicle-read-file-name: Use icicle-inhibit-reminder-prompt-flag.
;; 2006/02/12 dadams
;;     icicle-read-string: Finished bug fix of 2/11.  More thx to Andrey Zhdanov.
;; 2006/02/11 dadams
;;     icicle-insert-string-near-point: Always start with first function.
;;     read-from-minibuffer: Bug fix: don't use def if init is consp.  Thx to Andrey Zhdanov.
;; 2006/02/09 dadams
;;     Added: icicle-insert-string-near-point, icicle-thing-at-point-functions,
;;            icicle-thing-at-pt-fns-pointer.  Bound icicle-insert-string-near-point.
;;     Added Commentary section "Inserting Text Found Near the Cursor".
;;     Require: thingatpt+.el, thingatpt.el.
;;     Bug fix: icicle-execute-extended-command(-1): Take care of last-command and this-command.
;; 2006/02/08 dadams
;;     icicle-completing-read: Treat consp case of initial-input.
;;     icicle-read-file-name: Fixed bug introduced 02/02 - don't ensure initial-input is not null.
;; 2006/02/07 dadams
;;     Bug fix: Files menu find-file stuff was bound to *recent-file* commands.
;; 2006/02/03 dadams
;;     icicle-init-value-flag: Use nil as the default value.
;;     Added: icicle-read-from-minibuffer, icicle-read-string.
;;              Use in icicle-(redefine|restore)-standard-commands.
;; 2006/02/02 dadams
;;     icicle-completing-read, read-file-name:
;;       Respect icicle-init-value-flag only if default value is not nil.
;;     read-file-name: Ensure initial-value is not null.
;;                     Initialize icicle-initial-value.
;;                     Respect icicle-init-value-flag.
;; 2006/01/29 dadams
;;     icicle-completing-read, icicle-read-file-name: Remove binding of ESC-TAB.
;;     icicle-lisp-complete-symbol: Enable recursive minibuffers if in minibuffer.
;;     Commentary: Combine lisp-complete-symbol with dabbrev.
;;     Updated bindings listed in icicle-completion-help-string.
;; 2006/01/28 dadams
;;     New feature: icicle-lisp-complete-symbol (added).  Added to Commentary and moved section.
;;     Corrected fix of 2005/12/14:
;;       icicle-minibuffer-setup: save region background at recursion level 1.
;;       icicle-saved-region-background: defvar to nil.
;;     Added: icicle-increment-color-hue.  Use in icicle-region-background.
;;     Added: icicle-(re)set-option-to-(nil|t), icicle-clear-option, icicle-toggle-option,
;;            icicle-binary-option-p.
;; 2006/01/26 dadams
;;     Added: icicle(-saved)(-regexp)-search-ring-max, icicle-(redefine|restore)-standard-options.
;;     icicle-mode: Use icicle-(redefine|restore)-standard-options.
;;                  Use icicle-(redefine|restore)-standard-commands for Emacs 21+ also (forgot?).
;;     icicle-(redefine|restore)-*: Use defalias, not fset.
;; 2006/01/24 dadams
;;     New feature: icicle-isearch-complete.
;;       Added: icicle-isearch-complete, icicle-isearch-resume, icicle-bind-isearch-keys.
;;       icicle-mode: add/remove isearch-mode-hook.
;;     Minor bug fix: initial value was treated as icicle-last-completion-candidate.
;;       Added: icicle-initial-value.
;;       icicle-completing-read, icicle-read-file-name: Set icicle-initial-value,
;;         not icicle-last-completion-candidate.
;;       icicle-next-candidate:
;;         Initialize icicle-last-completion-candidate to icicle-initial-value.
;;       icicle-save-or-restore-input:
;;         Don't change icicle-current-input if = icicle-initial-value.
;;       Renamed: icicle-init-value to icicle-init-value-flag.
;; 2006/01/23 dadams
;;     Use command remapping for self-insert-command (only) in Emacs 22.
;;     Changed icicle-(re|un)map to defsubst.
;;     Removed Commentary section on icicle-execute-extended-command.
;;     icicle-apropos-complete-and-exit, icicle-apropos-complete-1:
;;       Use flag icicle-apropos-complete-and-exit-p to suppress minibuffer-message.
;; 2006/01/22 dadams
;;     Added: icicle-execute-extended-command*.
;;     completing-read, icicle-read-file-name:
;;       Corrected nil case for icicle-require-match-flag (bug fix).
;;       Hard-code bindings, instead of using \\[...], so the simpler bindings are shown.
;;     Changed C-o to C-RET for consistency (C-o still works too).
;;       icicle-(bind|restore)-completion-keys: Added C-RET binding. 
;; 2006/01/21 dadams
;;     icicle-mouse-choose-completion: Don't save selected window if it's *Completions*.
;;     Added more Commentary about icicle-retrieve-last-input.
;; 2006/01/20 dadams
;;     icicle-sort-and-strip-ignored: Don't ignore names if only ignored extensions match.
;;     Added: icicle-apropos-complete-and-exit.  Bound it in icicle-rebind-completion-maps.
;;     icicle-minibuffer-setup: Don't reset icicle-require-match-flag to nil.
;;     icicle-apropos-complete: Return the list of candidates.
;; 2006/01/19 dadams
;;     Added: icicle(-buffer)-require-match-flag.  Thanks to Mathias Dahl for feedback.
;;            Use in completing-read, read-file-name, and icicle-minibuffer-setup.
;;     Re-alphabetized defcustoms.
;; 2006/01/07 dadams
;;     Added :link.
;; 2005/12/31 dadams
;;     Added: icicle-fix-default-directory.
;;     icicle-read-file-name: Use icicle-fix-default-directory hack to fix bug.
;; 2005/12/26 dadams
;;     Added icicle-sort-case-insensitively.
;;     Added more parent groups for icicles group.
;; 2005/12/14 dadams
;;     icicle-minibuffer-setup: Only save region background when at top level.
;;     Added: icicle-Completions-frame-at-right-flag.  Use in icicle-candidate-action.
;;     Added: defvars for font-lock-keyword-face, font-lock-function-name-face.
;; 2005/12/09 dadams
;;     Fontify icicle-define* in emacs-lisp-mode.
;; 2005/12/02 dadams
;;     Added: icicle-customize-apropos*.  Use in icicle-(redefine|restore)-standard-commands.
;; 2005/12/01 dadams
;;     Added: icicle-repeat-complex-command, icicle-redefine-standard-commands-flag,
;;            icicle-(redefine|restore)-standard-commands.
;; 2005/11/30 dadams
;;     Added: icicle-apropos-zippy.
;;     icicle-apropos-command, icicle-apropos-variable: Corrected completing-read for do-all arg.
;;     icicle-apropos-command, icicle-apropos-option: My version must not respect apropos-do-all.
;; 2005/11/29 dadams
;;     Added: icicle-apropos*.
;;     icicle-help-on-candidate: Treat plists.  Message "No help" is the default.
;; 2005/11/25 dadams
;;     Added: icicle-dabbrev-completion.
;;     Renamed all names with "Completions" to use "Completions", for coherence with XEmacs port.
;; 2005/11/24 dadams
;;     icicle-mouse-choose-completion: Delete *Completions* window systematically.
;; 2005/11/21 dadams
;;     icicle-delete-windows-on: Avoid error Attempt to delete minibuffer or sole ordinary window.
;;     icicle-prefix-complete, icicle-apropos-complete-1, icicle-next-candidate:
;;       Use icicle-delete-windows-on, not delete-window.
;;     icicle-candidate-set-save: Use map in doc string.
;;     icicle-compilation-search: Tidied up doc string.
;;     Use #' for clarity.
;; 2005/11/20 dadams
;;     icicle-completing-read: Added treatment of completions that are lists of strings.
;;     Updated Commentary with new section on completions that are lists.
;;     Added: icicle-list-join-string, icicle-doc, icicle-fundoc, icicle-vardoc.
;; 2005/11/15 dadams
;;     Temporarily removed defadvice of next-history-element for Emacs 22.  Bug reported.
;;     icicle-minibuffer-prompt-end: Changed from defsubst to defun.
;; 2005/11/13 dadams
;;     icicle-mouse-candidate-action: buffer-substring -> buffer-substring-no-properties.
;;     icicle-completing-read: Bind, don't set, minibuffer-completion-table.
;;     icicle-buffer*: Use other buffer for DEF, not INIT-VALUE.
;;     Added: icicle-preselect-init-value-flag, icicle-(add|remove)-buffer-*,
;;            icicle-read-from-minibuf-nil-default, icicle-buffer-list,
;;            icicle-select-minibuffer-contents, icicle-completing-p.
;;     icicle-minibuffer-setup: Select minibuf contents if icicle-preselect-init-value-flag.
;;                              Only display *Completions* if icicle-completing-p.
;;     Advised next-history-element.
;; 2005/11/11 dadams
;;     Added: icicle-show-*Completions*-initially-flag, icicle-display-*Completions*.
;;     icicle-minibuffer-setup: If icicle-show-*Completions*-initially-flag, display it.
;; 2005/11/09 dadams
;;     Added: icicle-mouse-candidate-action.  Bind in icicle-rebind-completion-maps.
;;     icicle-buffer(-other-window): Use buffer-name-history as HIST arg to completing-read.
;; 2005/11/08 dadams
;;     Add/remove hook icicle-cancel-*Help*-redirection in icicle-mode, not at top level.
;;     Removed icicle-reset-icicle-menu-items-alist.
;;     Reset icicle-menu-items-alist in icicle-execute-menu-command of icicles-menu.el.
;; 2005/11/06 dadams
;;     Include minibuffer-local-filename-completion-map (to be added to Emacs 22).
;; 2005/11/05 dadams
;;     icicle-display-candidates-in-*Completions*: Don't try to highlight root if it is "".
;;     icicle-help-on-candidate: Test null, not boundp icicle-menu-items-alist.
;;                               If menu item's command is a lambda, set cand-symb to nil.
;;     icicle-mode: Use icicle-reset-icicle-menu-items-alist on minibuffer-exit-hook.
;;     Added: icicle-reset-icicle-menu-items-alist.
;;     Added defvar for icicle-menu-items-alist.
;;     Added byte-compiler comments and defvars to quiet byte-compiler.
;; 2005/11/04 dadams
;;     icicle-display-candidates-in-*Completions:
;;       Bug fix - use (functionp minibuffer-completion-table), not (icicle-file-name-input-p).
;; 2005/11/03 dadams
;;     Added: icicle-filter-wo-input and vars icicle-must-*, icicle-extra*, icicle-buffer-*,
;;            icicle-buffer-config*, icicle-buffer-sort*.
;;     icicle-unsorted-*: Use icicle-filter-wo-input and icicle-extra-candidates.
;;     Added Commentary section Icicles Improves Input Completion: (10) Global Filters.
;;     icicle-buffer* commands: Added filter bindings.
;;     icicle-define(-file)-command: Minor bug fix: Ensure buffer is live before switching back.
;; 2005/11/01 dadams
;;     Added: icicle-must(-not)-match-regexp.  Use in icicle-unsorted-*-candidates.
;; 2005/10/31 dadams
;;     Added: icicle-use-default-as-init-value-flag.  Use it in completing-read.
;;     icicle-find-file*: Minor bug fix - REQUIRE-MATCH should be nil.
;; 2005/10/29 dadams
;;     icicle-display-candidates-in-*Completions: Minor bug fix - wrap in save-window-excursion.
;;     icicle-minibuffer-contents: Minor bug fix - do nothing if file & user erased minibuffer.
;;     Menu-bar menus: Enable Icicles menu items only in Icicle mode.
;;                     Put search stuff on Search menu, if available.
;;                     Use "[Icy]" prefix for Icicles items in menus other than "Icicles".
;; 2005/10/28 dadams
;;     Added: icicle-define-file-command.
;;            Use it to define icicle-delete-file, icicle-find-file*.
;;     icicle-(next|previous)-(apropos|prefix)-candidate-action:
;;       Do action before moving to next|prev.
;;     icicle-candidate-action: Raise *Completions* frame, to keep it on top.
;; 2005/10/27 dadams
;;     Added: icicle-define-command, icicle-find-file*, select-frame-set-input-focus.
;;     Redefined using icicle-define-command: icicle-bookmark, icicle-buffer*, 
;;       icicle-color-theme, icicle-delete-file, icicle-find-file*, icicle-font,
;;       icicle-frame-*, icicle-recent-file*. 
;;     icicle-all-candidates-action: Report failures, not successes.  Use error msg.
;;     Added Commentary sections: What About Special-Character Conflicts?,
;;                                Defining Icicles Commands.
;;     Commentary section "Act on All Candidates": Added delete-one-or-more-files example.
;;     Added icicle-find-file* to menu-bar menus.
;;     Inactivated top-level menu items when minibuffer is active.
;;     Renamed: icicle-delete-file-1 to icicle-delete-file-or-directory.
;; 2005/10/25 dadams
;;     Thanks to Lennart Borgman for suggestion about select-frame-set-input-focus.
;; 2005/10/24 dadams
;;     icicle-search:
;;       1) Bug fix - need to have mouse-choose-completion set icicle-candidate-nb.
;;       2) Show error message.
;;     Default (reset) value of icicle-candidate-nb is now nil, not -1.
;;     Added: icicle-mouse-choose-completion, icicle-nb-of-candidate-in-*Completions*.
;;     icicle-move-to-(next|previous)-completion, icicle-increment-cand-nb+signal-end:
;;       Reset candidate number to 0 if nil.
;;     icicle-(redefine|restore)-std-completion-fns: Use icicle-mouse-choose-completion.
;; 2005/10/23 dadams
;;     Added: icicle-mode-map.
;;     icicle-(bind|restore)-completion-keys: Updated menu-bar menu.
;;     icicle-compilation-search: Error if not in a compilation buffer.
;; 2005/10/21 dadams
;;     icicle-remove-duplicates: redefined.
;; 2005/10/18 dadams
;;     icicle-file-name-input-p doc string:
;;       Mention why don't use minibuffer-completing-file-name.
;; 2005/10/16 dadams
;;     Added: icicle-compilation-search, icicle-search-hook.
;;     icicle-search: Run icicle-search-hook. Added optional sit-for-period arg.
;;     icicle-mode: Added list of top-level commands to doc string.
;;     icicle-scroll-or-update-*Completions*: Added msg arg - only display msg if don't scroll.
;; 2005/10/14 dadams
;;     Allow for multisets of candidates.
;;     Added: icicle-search, icicle-completion-nospace-flag, icicle-candidate-nb,
;;            icicle-filter-alist, icicle-increment-cand-nb+signal-end.
;;     Commentary: Updated for icicle-search.
;;     icicle-next-candidate: Major rewrite.  Uses icicle-candidate-nb,
;;       icicle-increment-cand-nb+signal-end, icicle-move-to-next-completion.
;;     Use icicle-completion-nospace-flag in calls to all-completions.
;;     icicle-previous-(apropos|prefix)-candidate,
;;       icicle-(next|previous)-(apropos|prefix)-candidate-action: Added optional arg.
;;     icicle-apropos-complete-1, icicle-next-candidate, icicle-recompute-candidates:
;;       Added *-action commands to memq test.
;;     icicle-move-to-next-completion: Added optional no-minibuffer-follow-p arg.
;;     icicle-scroll-or-update-*Completions*: Update display even if handle-switch-frame.
;; 2005/10/12 dadams
;;     Added: icicle-redefine-std-completion-fns, icicle-restore-std-completion-fns,
;;            icicle-delete-windows-on, icicle-frames-on.
;;     icicle-mode: Use icicle-redefine-std-completion-fns, icicle-restore-std-completion-fns.
;;     Renamed to use icicle- prefix: choose-completion-string, completing-read,
;;       completion-setup-function, exit-minibuffer, minibuffer-complete-and-exit,
;;       read-file-name, switch-to-completions.  Added these and also old- versions.
;;     icicle-history: Treat file names also.
;;     remove-windows-on -> icicle-delete-windows-on.
;; 2005/10/11 dadams
;;     Added: icicle-history, icicle-scroll-or-update-*Completions*,
;;            icicle-undo-std-completion-faces.
;;     Minor bug fixes:
;;       icicle-remove-dots: Also match against "." and ".." (lack of slash in Emacs 21+).
;;       icicle-save-or-*:
;;         Don't reset to last input if icicle-last-completion-candidate is "".
;;         Update icicle-last-completion-candidate also to use current input.
;;       Reset icicle-last-input in icicle-minibuffer-setup, not in completing-read and
;;         read-file-name.
;;       icicle-display-candidates-in-*Completions*, icicle-next-candidate:
;;         Put candidate in consp before applying predicate.
;;       icicle-recompute-candidates: Don't recompute unless icicle-last-completion-command.
;;       icicle-retrieve-last-input: Use icicle-current-input, not icicle-last-input.
;;       icicle-self-insert: Update icicle-current-input and set this-command to
;;         icicle-apropos-complete.
;;       icicle-apropos-complete: Use error-message-string.
;;       icicle-apropos-complete-1:
;;         Protect icicle-file-directory-p with icicle-file-name-input-p.
;;         Unconditionally update icicle-last-completion-command.
;;     Moved undoing of std completion faces to icicle-mode.
;;     Use icicle-scroll-or-update-*Completions* in icicle-candidate-set-1.
;; 2005/10/06 dadams
;;     icicle-prefix-complete, icicle-apropos-complete-1: 
;;       Removed vestigial slash cruft - should have been removed in 2005/09/01 fix.
;;     Added: icicle-remove-dots.  Use in icicle-save-or-restore-input.
;; 2005/10/05 dadams
;;     icicle-msg-maybe-in-minibuffer: use format-string arg.
;; 2005/10/04 dadams
;;     Replace use of minibuffer-completion-help by icicle-apropos-complete.
;;     Added: icicle-recent-file*, icicle-toggle-ignored-extensions icicle-update-completions, 
;;            icicle-msg-maybe-in-minibuffer, icicle-saved-ignored-extensions.
;;     Bound icicle-toggle-*.
;;     icicle-toggle-sorting: Use icicle-update-completions, icicle-msg-maybe-in-minibuffer.
;;     icicle-sort-and-strip-ignored:
;;       icicle-ignored-extensions-regexp nil means nothing is ignored.
;;     Reorder key bindings, so prompt shows S-tab, not S-iso-lefttab (!).
;;     icicle-next-candidate: Fixed code to highlight candidate in *Completions*: restriction.
;; 2005/10/03 dadams
;;     Regexps can now use backslash (it is no longer a directory separator on MS Windows).
;;       icicle-minibuffer-contents, icicle-file-name-directory-w-default:
;;         Escape backslash, so not used as directory separator on MS Windows.
;;       Added: icicle-apropos-complete-1, icicle-file-name-nondirectory.
;;       icicle-apropos-complete: Use icicle-apropos-complete-1.
;;                                Treat regexp error via message.
;;       Use icicle-file-name-nondirectory everywhere, instead of file-name-nondirectory.
;;     Can now use "?" for regexps; it no longer shows completion list.
;;     Do icicle-update-ignored-extensions-regexp inside icicle-minibuffer-setup.
;;     Added and bound: icicle-retrieve-last-input.
;;     Updated icicle-completion-help-string with recent bindings.
;;     Renamed: icicle-last-command to icicle-last-completion-command (to reflect use),
;;              icicle-candidate-set-restore to icicle-candidate-set-retrieve.
;; 2005/10/01 dadams
;;     Added: icicle-candidate-set-(define|restore|swap).
;;     Changed binding of icicle-candidate-set-save to C->.  Bound new commands.
;; 2005/10/01 dadams
;;     Major rewrite to add set operations: complement, difference, union, intersection.
;;       Added: icicle-completion-candidates, icicle-current-input, icicle-candidate-set-*,
;;              icicle-set-*, icicle-save-or-restore-input, icicle-recompute-candidates.
;;       Bound icicle-candidate-set* and added Commentary for Candidate Sets.
;;       icicle-(apropos|prefix)-complete: Update icicle-completion-candidates, only as needed.
;;       icicle-next-candidate:
;;         Reverse candidates only if switched to opposite-direction command of same type.
;;         Likewise, for refresh of *Completions*.
;;         Protect put-text-property for root (e.g. no match for complement).
;;       icicle-(apropos|prefix)-complete, icicle-prefix-word-complete, icicle-next-candidate:
;;         use icicle-completion-candidates.
;;       icicle-all-candidates-action: Use icicle-completion-candidates,
;;         not icicle-apropos-complete.
;;       icicle-display-candidates-in-*Completions*: Removed first arg (candidates).
;;                                                   Update icicle-completion-candidates.
;;    icicle-all-candidates-action: 
;;      Use icicle-completion-candidates, so act on completions of either kind.
;; 2005/09/30 dadams
;;     Commented out resetting of minibuffer-completion-table to nil for icompletion.
;;       Thanks to Andrey (susuman@hotmail.com) for bug report on M-x M-r problem.
;; 2005/09/27 dadams
;;     icicle-(bind|restore)-completion-keys: Bound [S-iso-lefttab] like [S-tab].
;; 2005/09/26 dadams
;;     Bug fix: Changed "\C-!"  to [(control ?!)].  (Changed others similarly.)
;;     Bound [S-iso-lefttab] like [S-tab].
;; 2005/09/16 dadams
;;     Added: icicle-all-candidates-action, icicle-delete-file*, 
;;     icicle-rebind-completion-maps: Bound icicle-all-candidates-action to C-!.
;;     icicle-(apropos|prefix)-complete: Return candidates list.
;;     icicle-bookmark, icicle-buffer*, icicle-color-theme, icicle-font, icicle-frame*:
;;       Return t for success, nil for failure.
;;     Commentary: Added section (7) Choose All Candidates.
;; 2005/09/14 dadams
;;     icicle-rebind-completion-maps: Bound TAB and S-TAB for navigation.
;;     icicle-move-to-(next|previous)-completion, icicle-(next|previous)-line: Wrap around.
;; 2005/09/13 dadams
;;     Major rewrite of file treatment, to treat directory candidates similarly to files.
;;     Added: icicle-default-directory, icicle-file-directory-p, icicle-sort-function,
;;            icicle-toggle-sorting, toggle-icicle-sorting.
;;     Use icicle-file-directory-p everywhere, except choose-completion-string.
;;     Removed: icicle-nondirectory-*.
;;     icicle-next-candidate: If not icicle-cycle-into-subdirs-flag, then use relative
;;       file/dir name, not nondirectory part.
;;     icicle-(apropos|prefix)-complete: Set icicle-default-directory if sole completion
;;       is a subdirectory.
;;     icicle-sort-and-strip-ignored: Removed optional arg and treatment of subdirs.
;;     icicle-next-(apropos|prefix)-candidate, icicle-(apropos|prefix)-complete:
;;       Don't treat icicle-cycle-into-subdirs-flag here.
;;     icicle-(apropos|prefix)-complete, icicle-next-candidate:
;;       Set icicle-default-directory, if directory candidate.
;;     icicle-minibuffer-setup: Set icicle-default-directory.
;;     icicle-apropos-complete: Different message if icicle-apropos-icompleting-p.
;;     icicle-sort-dirs-last: Treat other kinds of candidates, besides files and dirs.
;;     Commentary and doc strings: Updated for icicle-sort-function, icicle-cycle-into-subdirs.
;;     Let delete-selection mode work with icicle-self-insert.
;;     icicle-display-candidates-in-*Completions*: Make *Completions* read-only.
;; 2005/09/09 dadams
;;     choose-completion-string: bug fix for Emacs 21.3.1 - use Emacs 20 version for 21.3.1.
;; 2005/09/08 dadams
;;     completion-setup-function: bug fix for Emacs 21.3.1 - use Emacs 20 version for 21.3.1.
;;     Added: icicle-remap, icicle-unmap, icicle-(bind|restore)-completion-keys.
;;     completing-read: Do not append suffix if not in Icicle mode.
;;     icicle-rebind-completion-maps: Clean-up.  Use icicle-(bind|restore)-completion-keys.
;;                                    Don't (suppress-keymap completion-list-mode-map).
;; 2005/09/06 dadams
;;     Provided apropos icompletion.
;;     Added: icicle-self-insert, icicle-incremental-completion-flag,
;;            icicle-apropos-icompleting-p.
;;     icicle-apropos-complete: Respect icicle-apropos-icompleting-p.
;;     Commentary: Updated Icompletion and Customization sections.  Added Apropos Icompletion.
;;     Changed default value of icicle-word-completion-key to M-SPC.
;;     icicle-rebind-completion-maps: Bind icicle-self-insert. Use self-insert for SPC.
;;                                    Updated icicle-completion-help-string.
;;                                    Treat menu-bar menu for the minibuffer.
;;     completion-setup-function: Only add instruction2 when icicle-mode.
;;     icicle-display-candidates-in-*Completions*: Use save-restriction.
;;     icicle-minibuffer-contents: Allow for mixing $ of environment vars with $ of regexps.
;; 2005/09/02 dadams
;;     Added: icicle-bookmark, icicle-buffer(-other-window), icicle-candidate-action,
;;            icicle-candidate-action-fn, icicle-color-theme(s), icicle-font,
;;            icicle-frame-(b|f)g,
;;     Renamed: icicle-(next|previous)-(apropos|prefix)-*-help to
;;              icicle-(next|previous)-(apropos|prefix)-*-action.
;;     icicle-(apropos|prefix)-complete: set icicle-last-completion-candidate.
;;     In renamed functions: use icicle-candidate-action, not icicle-help-on-candidate.
;;     icicle-rebind-completion-maps: Bound C-o to icicle-candidate-action.
;;     Added Commentary section on actions on candidates.
;;     icicle-move-to-next-completion: Test line num, not char position (fix).
;;     icicle-previous-line: 3 or 4, not 4 or 5 (fix).
;; 2005/09/01 dadams
;;     Fixed major bug: file-name completion did not work at all in non-MS Windows!
;;       icicle-file-name-(apropos|prefix)-candidates:
;;         Removed code for case where input starts with "/".
;;       icicle-nondirectory-file-name-(apropos|prefix)-candidates:
;;         Removed code for case where input starts with "/". Bind default-directory.
;;       icicle-(apropos|prefix)-complete: Treat case when icicle-cycle-into-subdirs-flag = nil.
;;     icicle-next-candidate: Took out code that moved point when line is too long.
;;     icicle-minibuffer-setup: Reset icicle-prompt.
;; 2005/08/23 dadams
;;     Added: icicle-help-on-candidate, icicle-cancel-*Help*-redirection,
;;            icicle-(previous|next)-(prefix|apropos)-candidate-help.  Bound them all.
;;     icicle-rebind-completion-maps: Bound icicle-help-on-candidate,
;;            icicle-(previous|next)-(prefix|apropos)-candidate-help.
;; 2005/08/22 dadams
;;     Unconditionally require cl.el when compile (because of case function).
;; 2005/08/19 dadams
;;     Renamed icicle-cursor-position-in-candidate to icicle-point-position-in-candidate.
;;     Added: icicle-mark-position-in-candidate, icicle-minibuffer-prompt-end.
;;     icicle-place-cursor: Position both point and mark.
;;     icicle-point-position-in-candidate: Change values from bob, eob to input-start/end.
;;     Removed: icicle-select-rest-of-completion-flag - use inequality test on point and mark.
;;     Updated commentary.
;; 2005/08/16 dadams
;;     Minbuffer messages: differentiate prefix from apropos completion.
;;     completing-read, read-file-name: Append icicle-prompt-suffix for Emacs 20 (oversight).
;; 2005/08/15 dadams
;;     Bug fix: Only use face-spec-reset-face if the target faces are defined.
;;     read-file-name: bug fix - Use condition-case to get the correct number of args for
;;       old-read-file-name. Thanks to Mathias Dahl for both bug reports.
;; 2005/08/14 dadams
;;     icicle-place-cursor: Narrow region to exclude minibuffer-prompt.
;; 2005/08/13 dadams
;;     Add regexp support (had removed it when introduced highlighting).
;;       icicle-next-candidate: Added regexp-p arg.  Use it in icicle-next-apropos-candidate.
;;       icicle-place-cursor: Use regexp search.  For root-start, go to match-beginning.
;;       icicle-unsorted-file-name-apropos-candidates: Don't use regexp-quote.
;;     icicle-switch-to-*Completions*: Search in restriction of mouse-face zone; repeat.
;;       Treat file case (use nondirectory part).
;;       Bind case-fold-search.
;;     Protect (aref <input> 0) against empty string.
;;     member -> memq, for symbols.
;; 2005/08/12 dadams
;;     Added: icicle-word-completion-key, icy-mode, icicle-insert-a-space.
;;     icicle-rebind-completion-maps: Use icicle-word-completion-key and
;;       icicle-insert-a-space.
;;     completing-read, icicle-rebind-completion-maps: Corrected bindings in doc string.
;; 2005/07/29 dadams
;;     Added: icicle-change-region-background-flag, icicle-increment-color-value,
;;            icicle-region-background, icicle-saved-region-background,
;;            icicle-restore-region-face.
;;     Added icicle-restore-region-face to minibuffer-exit-hook.
;;     Require hexrgb.el.
;;     Removed: icicle-select-rest-of-completion.
;;     icicle-minibuffer-setup: Save icicle-saved-region-background and use
;;       icicle-region-background.
;; 2005/07/28 dadams
;;     Added: icicle-*Completions*-instruction-*.
;;     completion-setup-function:
;;       Use icicle-*Completions*-instruction-*.
;;       Remove "?" from instruction2. Put both instructions on same line.
;;       Use put-text-property, not *-w-face*.
;;     ------
;;     Move all completion stuff here, from simple+.el:
;;       choose-completion-string, completion-setup-function, switch-to-completions.
;;     Wrap *Completions* window deletion in save-selected-window (minor bug).
;;     Added icicle-prefix-word-complete, and bound it to SPC.
;;     completion-setup-function:
;;       Renamed icicle-completing-read-prompt-suffix to icicle-prompt-suffix.
;; 2005/07/27 dadams
;;     Renamed: icicle-completing-read-prompt* to icicle-prompt*.
;;     Added: read-file-name, face icicle-completing-read-prompt-suffix,
;;            icicle-remove-property, icicle-select-rest-of-completion (simple, for now).
;;     completing-read: Apply faces to prompt.
;;     icicle-place-cursor: Use icicle-select-rest-of-completion.
;;     Added (if icicle-mode (icicle-mode 1)) at end.
;;     Reworded Commentary in terms of "input completion", not just `completing-read'.
;; 2005/07/26 dadams
;;     rebind-minibuffer-completion-maps: Minor bug fix.
;;     icicle-mode: Added " Icy" to mode line.
;;     Wrapped Emacs 21 version of icicle-mode (with define-minor-mode) in (eval (quote...)),
;;       so byte-compiling in Emacs 20 will produce a *.elc that works in Emacs 21.
;; 2005/07/25 dadams
;;     Added: icicle-mode, icicle-*-hook, icicle-minibuffer-setup, icicle-activate-mark.
;;     rebind-minibuffer-completion-maps: Restore bindings when exit Icicle mode.
;;       Added argument.  Pick up everything bound to `help-command'.  Updated doc string.
;;       Message only when mode is turned on.
;; 2005/07/24 dadams
;;     Now leave region from end of root to end of completion, so you can easily replace
;;       it, especially if you use delete-selection mode.  (Suggestion by Lennart Borgman.)
;;     Added: icicle-select-rest-of-completion-flag.
;;     icicle-place-cursor: Create active region if icicle-select-rest-of-completion-flag.
;;     icicle-completion-help: Removed icicle-abort-minibuffer-input.
;;     icicle-abort-minibuffer-input: Removed obsolete code & comment on icomplete-inhibit.
;; 2005/07/22 dadams
;;     Major fixup: treat file and directory names well, respect std user options, more.
;;     Renamed:
;;        icicle-(next|previous)?-completion-candidate to icicle-*-prefix-candidate(s),
;;       icicle*filename* to icicle*file-name*,
;;       icicle-descend-into-subdirs to icicle-cycle-into-subdirs-flag.
;;     Added: icicle-file-name-apropos-candidates, icicle-file-name-directory-w-default,
;;            icicle-file-name-input-p, icicle-file-name-prefix-candidates,
;;            icicle-nondirectory-file-name-apropos-candidates,
;;            icicle-nondirectory-file-name-prefix-candidates, icicle-sort-dirs-last,
;;            icicle-unsorted-apropos-candidates,
;;            icicle-unsorted-file-name-apropos-candidates,
;;            icicle-unsorted-file-name-prefix-candidates,
;;            icicle-unsorted-prefix-candidates, icicle-last-command.
;;     Respect insert-default-directory and completion-auto-help.
;;     Use minibuffer-message instead of message.
;;     Commentary: Added Customization & Tips section.
;;     completing-read: Updated doc string.  Save icicle-last-input.
;;                      Reset icicle-nb-of-other-cycle-candidates.
;;     icicle-next-*-candidate: Branch to file-specific functions.
;;     icicle-*-candidates: Use icicle-unsorted-*-candidates.
;;     icicle-next-candidate: Delete *Completions* window if no candidates.
;;                            Use icicle-file-name-directory instead of file-name-directory.
;;     icicle-minibuffer-contents: Use substitute-in-file-name.
;;     icicle-*-complete:
;;       Treat slashed file names (e.g. "/foo").
;;       Use icicle-file-name-*-candidates, icicle-file-name-directory-w-default for files.
;;       Added messages [No completion], [Sole completion], [Complete, but not unique].
;;       Use icicle-last-command for repetition test. And set it.
;;     icicle-rebind-completion-maps: Updated icicle-completion-help-string and message.
;; 2005/07/21 dadams
;;     icicle-apropos-candidates: Use, not apropos, but delete-if-not on string-match.
;;                                Treat files too.
;;     Removed icicle-intersection.
;;     Added: icicle-descend-into-subdirs.
;;     icicle-sort-and-strip-ignored: Use icicle-descend-into-subdirs.
;;                                    Don't use "." and "..".
;;     icicle-next-candidate: File names w/o dir.
;;                            Use regexp-quote on root for underlining file-name root.
;;                            Insert directory name for file.
;;     icicle-place-cursor: Search past dir, then search for file-name w/o dir.
;;     icicle-prefix-complete, icicle-apropos-complete, icicle-switch-to-*Completions*:
;;       Use icicle-minibuffer-contents.
;;     icicle-prefix-complete, icicle-apropos-complete: Insert dir when single candidate.
;;     icicle-display-candidates-in-*Completions*: Underline file-name w/o dir.
;; 2005/07/20 dadams
;;     icicle-next-candidate, icicle-display-candidates-in-*Completions*:
;;       Use set-buffer-modified-p.
;;     icicle-next-candidate: Use ding when hit end of cycle.
;;     Added: icicle-cursor-position-in-candidate, icicle-place-cursor.
;;            Use them in icicle-next-candidate to position cursor.
;;     Added: defgroup icicles.
;; 2005/07/19 dadams
;;     Initialize icicle-ignored-*.
;;     Added: icicle-nb-of-other-cycle-candidates, icicle-minibuffer-contents.
;;     completing-read: Reset icicle-last-completion-candidate to nil.
;;     icicle-next-candidate: Use icicle-minibuffer-contents.
;;       Save icicle-nb-of-other-cycle-candidates for icomplete-completions (icomplete+).
;;       Use copy of "next" string because we change its text properties.
;;       Use regexp-quote for underlined root.  Use put-text-property, so works in Emacs 20.
;;       Update *Completions*, even if last command is repeated.
;;     icicle-*-complete: Complete rewrite.
;;     icicle-display-candidates-in-*Completions*: Do even if last command is repeated.
;; 2005/07/18 dadams
;;     icicle-display-*: Highlight only first occurrence in each candidate.
;;     icicle-next-candidate: Use completion-ignore-case.
;; 2005/07/17 dadams
;;     Treat file names also.
;;     Added: icicle-delete-if*, and use instead of delete-if-*.  Removed require cl.el.
;;     Added: icicle-ignored-extensions*, icicle-sort-and-strip-ignored,
;;            icicle-filename-input-p, icicle-update-ignored-extensions-regexp,
;;            icicle-prefix-complete.  Bound icicle-prefix-complete.
;;     Use icicle-update-ignored-extensions-regexp as minibuffer-setup-hook.
;;     icicle-*-candidates: Use icicle-sort-and-strip-ignored.
;;     icicle-next-candidate, icicle-display-candidates-in-*Completions*:
;;       Don't use predicate on file-name candidates (icicle-filename-input-p).
;;     icicle-next-candidate: Use read-file-name-completion-ignore-case (Emacs 22) and
;;       file-name-nondirectory.
;;     icicle-apropos-complete: Return t/nil. Treat single candidate as no-op.
;;     Reset std completions-* faces, so they don't interfere with apropos highlighting.
;; 2005/07/16 dadams
;;     Added: icicle-display-*, icicle-apropos-complete.
;;     Use icicle-display-* in icicle-next-candidate and icicle-apropos-complete.
;;     Bound icicle-apropos-complete to S-tab in completion maps.
;;     icicle-switch-to-*Completions*: Move to start of candidate.
;;                                     Highlight candidate, not regexp.
;;     icicle-next-candidate: Underline the root that was completed.
;;     Added: faces icicle-root-highlight-*.  Removed: faces: icicle-completion-help*.
;;     Removed (not used): require of strings.el.
;;     Commentary: Added Nutshell View.
;; 2005/07/15 dadams
;;     Renamed: icicle-completion-help+ to icicle-completion-help.
;;     Replaced: icicle-delete-lines by icicle-erase-minibuffer (new definition).
;;     icicle-next-candidate: Wrapped display-* and re-search-forward in condition-case.
;;                            Use icicle-place-overlay.
;;     Changed icicle-completion-help bindings to [f1].
;;     Added: icicle-*-line, icicle-switch-to-*, icicle-move-to-*-completion,
;;            icicle-current-completion-in-*Completions*, icicle-place-overlay.
;;     Added bindings for icicle-*-line, icicle-switch-to-*, icicle-move-to-*.
;;     Bound q to icicle-abort-minibuffer-input in completion-list-mode-map.
;;     icicle-completing-read-prompt-suffix: Mention both [f1] and ?.
;;     Removed: icicle-fit-frame.
;;     Commentary: Added How...Improves...(4).  Updated Key Bindings.
;; 2005/07/14 dadams
;;     icicle-next-candidate:
;;       Update *Completions*, if displayed, to reflect current candidates, but don't do it
;;         if this-command = last-command.  Reverse list as needed, to keep same order.
;;       Ensure current *Completions* choice shows in window (recenter as needed).
;;       For highlighting: search with re-search-forward to be sure to get the right one.
;;       Took test for presence of predicate out of loop.
;;     Commentary: Added Note on pop-up-frames = t.
;; 2005/07/13 dadams
;;     Rewrote icicle-apropos-candidates.  Added: icicle-intersection.
;; 2005/07/12 dadams
;;     Added: icicle-(next|previous)-apropos-candidate, icicle-next-candidate,
;;            icicle-apropos-candidates, icicle-completion-candidates.
;;     Bound: icicle-next-apropos-candidate, icicle-previous-apropos-candidate.
;;     Renamed: icicle-completion-help-(title-)face: Removed "-face".
;;     icicle-next-completion-candidate: Redefined to use icicle-next-candidate.
;;     icicle-rebind-completion-maps: Updated text to mention apropos completion.
;;     icicle-completion-help+: Use icicle-abort-minibuffer-input, not abort-recursive-edit.
;; 2005/07/10 dadams
;;     First version of icicles.el (previously called elect-mbuf.el).
;;     Renamed: minibuffer-completion-help-string -> icicle-completion-help-string,
;;       completing-read-prompt -> icicle-completing-read-prompt,
;;       completing-read-prompt-suffix -> icicle-completing-read-prompt-suffix,
;;       mbuf-completion-help-face -> icicle-completion-help-face,
;;       mbuf-completion-help-title-face -> icicle-completion-help-title-face,
;;       minibuffer-last-default -> icicle-last-completion-candidate,
;;       command-calling-for-completion -> icicle-cmd-calling-for-completion,
;;       minibuffer-completion-help+ -> icicle-completion-help+,
;;       abort-minibuffer-input -> icicle-abort-minibuffer-input,
;;       next-default-input -> icicle-next-completion-candidate,
;;       previous-default-input -> icicle-previous-completion-candidate,
;;       rebind-minibuffer-completion-maps -> icicle-rebind-completion-maps,
;;     Added: minibuffer-complete-and-exit, icicle-fit-frame, icicle-last-input.
;;     Moved delete-lines here from and renamed to icicle-delete-lines.
;;     Removed: mod+ (unused).
;;     icicle-completion-help+: Use *Help*, not *Completions*.  Don't show completions.
;;     icicle-next-completion-candidate: Use insert, not insert-string.
;;     icicle-rebind-completion-maps: Made it interactive.
;; 2005/07/09 dadams
;;     Removed: buffer-alist (not used).
;; 2005/05/15 dadams
;;     Renamed: flash-ding-minibuffer-frame to 1on1-flash-ding-minibuffer-frame.
;; 2005/05/10 dadams
;;     Hacked completing-read to remove *Completions* window at end if
;;       require-match is non-nil.  (Don't know why/when this became a problem.)
;; 2004/09/21 dadams
;;     Updated to work in Emacs 21 (and 20):
;;       next-default-input uses delete-minibuffer-contents for 21, but
;;          erase-buffer for 20.
;;       minibuffer-completion-help+: bind inhibit-read-only to t around
;;       erase-buffer.
;; 2001/01/10 dadams
;;     Protected remove-windows-on via fboundp.
;; 1999/09/03 dadams
;;     1. Added: mbuf-completion-help-face, mbuf-completion-help-title-face.
;;     2. minibuffer-completion-help+: use mbuf-*-face's instead of hard-coding.
;;     3. minibuffer-completion-help-string, completing-read-prompt-suffix:
;;          defconst -> defvar.
;; 1999/08/26 dadams
;;     Protected faces via boundp.
;; 1999/04/13 dadams
;;     Bound delete-lines to M-S-DEL and M-S-backspace.
;; 1999/03/17 dadams
;;     protect calls with test fboundp.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/03/26 dadams
;;     minibuffer-completion-help+: concat -> concat-w-faces (color).
;; 1995/12/20 dadams
;;     exit-minibuffer: Iconify *Completion* frame.
;; 1995/12/15 dadams
;;     1. abort-minibuffer-input: Reset minibuffer-completion-table to avoid
;;        icompletion.
;;     2. Defined replacement exit-minibuffer to do the same as #1.
;; 1995/12/01 dadams
;;     1) abort-minibuffer-input: Incorporated delete-selection-mode code.
;;     2) rebind-minibuffer-completion-maps: Added C-g bindings for
;;          minibuffer-local-map, minibuffer-local-ns-map,
;;          minibuffer-local-isearch-map.
;; 1995/10/25 dadams
;;     Put defvar of minibuffer-completion-help-string after do
;;     rebind-minibuffer-completion-maps, so its doc string gives bindings.
;; 1995/10/24 dadams
;;     Mention ESC-TAB completion in completing-read.
;; 1995/10/17 dadams
;;     Let minibuffer use ESC-TAB for completion (of Lisp symbols etc.)
;;     1) completing-read: Minibuffer adopts current buffer's ESC-TAB binding.
;;     2) Added command-calling-for-completion to memorize current command
;;        (done in completion-setup-hook).
;; 1995/09/12 dadams
;;     1) Added abort-minibuffer-input.
;;     2) Define C-g as abort-minibuffer-input in completion-list-mode-map
;;        and minibuffer-local-* maps.
;;     3) No self-insertion for completion-list-mode-map.
;; 1995/08/16 dadams
;;     next-default-input: Fixed bug - skip over repeated alist entries.
;; 1995/08/10 dadams
;;     1) Rewrote minibuffer-completion-help+: Provide help even if no completions.
;;     2) So, added minibuffer-completion-help-string.
;;     3) `?' defined correctly for minibuffer-local-must-match-map.
;; 1995/08/08 dadams
;;     next-default-input: error msg: no hard coding of key seq.
;; 1995/08/02 dadams
;;     Major rewrite.
;;     1) No reminders in prompts.
;;     2) Added minibuffer-completion-help+ to provide help info for *Completions*.
;;
;; Log for functions that were previously in simple+.el:
;;       choose-completion-string, completion-setup-function, switch-to-completions.
;; 2005/07/28 dadams
;;     completion-setup-function:
;;       Renamed icicle-completing-read-prompt-suffix to icicle-prompt-suffix.
;; 2005/07/15 dadams
;;     choose-completion-string, completion-setup-function: Updated for Emacs 21+.
;; 2005/07/10 dadams
;;     Renamed: command-calling-for-completion -> icicle-cmd-calling-for-completion.
;; 2004/09/21 dadams
;;     Only redefine choose-completion-string if prior to Emacs 21.
;; 1999/03/17 dadams
;;     1. choose-completion-string: Added doc string.  Updated to correspond to
;;        Emacs 34.1 version.
;;     2. completion-setup-function: diff prompt setups.  face1 & face2 tests.
;;     3. Added: switch-to-completions.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
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
                                  ;; plus, for Emacs < 21: dolist, pop, push
                                  ;; plus, for Emacs < 20: when, unless


(require 'overlay)
(require 'thingatpt)

;;(require 'hexrgb nil t)     ;; (no error if not found): hexrgb-color-values-to-hex,
                            ;; hexrgb-increment-(red|green|blue), hexrgb-rgb-to-hsv,
                            ;; hexrgb-color-values-to-hex, hexrgb-hsv-to-rgb
;;(require 'misc-fns nil t)   ;; (no error if not found) another-buffer



;; Byte-compiling this file, you will likely get some error or warning messages. All of the
;; following are benign.  They are due to differences between different versions of Emacs.
;;
;; Compiling in Emacs 22:
;;
;; Warning: `directory-sep-char' is an obsolete variable (as of Emacs 21.1); do not use it.
;; Warning: `make-local-hook' is an obsolete function (as of Emacs 21.1); not necessary any more.
;;
;; Compiling in Emacs 20:
;;
;; describe-mode called with 1 argument, but accepts only 0
;; The following functions are not known to be defined:
;;   minibuffer-prompt-end, delete-minibuffer-contents, face-spec-reset-face, set-face-attribute,
;;   recentf-mode, minibufferp, field-string, minibuffer-contents, display-mouse-p, propertize,
;;   minibuffer-contents-no-properties
;;
;; You might also get a warning about x-focus-frame or w32-focus-frame not being defined.



;;; Defvars to quiet byte-compilers (Emacs 20 - 22)

(defvar directory-sep-char)
(defvar recentf-list)
(defvar partial-completion-mode)
(defvar completion-root-regexp)
(defvar minibuffer-prompt-properties)
(defvar minibuffer-local-filename-completion-map)
(defvar dabbrev-case-fold-search)
(defvar dabbrev-upcase-means-case-search)
(defvar dabbrev--last-obarray)
(defvar dabbrev--last-completion-buffer)
(defvar dabbrev--last-abbreviation)
(defvar dabbrev--check-other-buffers)
(defvar dabbrev-case-replace)
(defvar dabbrev--last-obarray)


(defun completion-setup-function () nil)


(or (fboundp 'subst-char-in-string)
    (defun subst-char-in-string (fromchar tochar string &optional inplace)
      "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
      (let ((i (length string))
            (newstr (if inplace string (copy-sequence string))))
        (while (> i 0)
          (setq i (1- i))
          (if (eq (aref newstr i) fromchar)
              (aset newstr i tochar)))
        newstr)))


(defalias 'minibuffer-message 'temp-minibuffer-message)
;;;;;;;;;;;;;




;;; Faces (alphabetical) -----------------------------------

(defgroup icicles nil
    "Minibuffer input completion and cycling of completion candidates."
      :prefix "icicle-"
      :group 'completion :group 'convenience)

(defface icicle-complete-input '((t (:foreground "dark green")))
  "*Face used to highlight input when it is complete."
  :group 'icicles :group 'faces)

(defface icicle-Completions-instruction-1 '((t (:foreground "blue")))
  "*Face used to highlight first line of *Completions* buffer."
  :group 'icicles :group 'faces)

(defface icicle-Completions-instruction-2 '((t (:foreground "red")))
  "*Face used to highlight second line of *Completions* buffer."
  :group 'icicles :group 'faces)

(defface icicle-historical-candidate '((t (:foreground "red")))
  "*Face used to highlight *Completions* candidates that have been used."
  :group 'icicles :group 'faces)

; Value is from `custom-button-pressed-face', with :foreground from `minibuffer-prompt'.
(defface icicle-prompt-suffix
    '((((type x w32 mac) (class color))
       (:box (:line-width 2 :style pressed-button) :foreground "dark blue"))
        ;;; :background "lightgrey" :foreground "black"
      (t (:inverse-video t)))
  "*Face used to highlight `icicle-prompt-suffix'."
  :group 'icicles :group 'faces)

(defface icicle-root-highlight-Completions '((t (:foreground "Blue")))
  "*Face used to highlight root that was completed, in minibuffer."
  :group 'icicles :group 'faces)

(defface icicle-root-highlight-minibuffer '((t (:underline t)))
  "*Face used to highlight root that was completed, in *Completions*."
  :group 'icicles :group 'faces)




;;; User Options (alphabetical, except for dependencies) ---

;; Replace this list by your favorite color themes. Each must be the name of a defined function.
;; By default, this includes all color themes defined globally (variable `color-themes').
;;;###autoload
(defcustom icicle-color-themes nil
  ;;(and (require 'color-theme nil t)
  ;;     (delq 'bury-buffer
  ;;           (mapcar (lambda (entry) (list (symbol-name (car entry)))) color-themes)))
  "*List of color themes to cycle through using `M-x icicle-color-theme'."
  :type 'hook :group 'icicles)

;;;###autoload
(defcustom icicle-completion-nospace-flag t
  "*Non-nil means ignore completion candidates that start with a space
unless the input to be completed also starts with a space.
This corresponds roughly to the NOSPACE argument to `all-completions'.
Note: Some Icicles functionalities ignore the value of this option."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-cycle-into-subdirs-flag nil
  "*Non-nil means minibuffer-input cycling explores subdirectories.
If this is non-nil, then you might want to use a function such as
`icicle-sort-dirs-last' for option `icicle-sort-function', to prevent
cycling into subdirectories depth first."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-incremental-completion-flag t
  "*Non-nil means update *Completions* buffer incrementally, as you type.
t means do nothing if *Completions* is not already displayed.
Non-nil and non-t means display *Completions* and update it."
  :type 'boolean :group 'icicles)

(defcustom icicle-inhibit-reminder-prompt-flag nil
  "*Non-nil means do not add reminder to Icicles prompt.
Nil means add a reminder like this: (<S-tab>, TAB: list, C-h: help),
if space permits."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-init-value-flag nil
  "*Non-nil means to use default value as init value when reading input.
This is used by `completing-read', `read-file-name', `read-string',
and `read-from-minibuffer'.  When the default-value argument to one of
these functions is non-nil and the initial-input argument is nil or
\"\", the default value is inserted in the minibuffer as the initial
input.

This has the advantage of not requiring you to use `M-n' to retrieve
the default value.  It has the disadvantage of making you empty the
minibuffer if you do not want to use or edit the default value.

The particular non-nil value determines whether or not the value is
preselected and, if preselected, where the cursor is left: at the
beginning or end of the value.  Possible values:

  nil               - Do not insert default value.
  `insert'          - Insert default value (leave cursor at end).
  `preselect-start' - Insert and preselect default value;
                      leave cursor at beginning.
  `preselect-end'   - Insert and preselect default value;
                      leave cursor at end.

My own preference is `insert'.  This is not the value by default only
because people are not used to it.  I recommend that you try `insert'
for a while, before giving up on it.

Preselection can be useful in Delete Selection mode or PC Selection
mode.  It makes it easy to replace the value by typing characters, or
delete it by hitting `C-d' or `DEL' (backspace).  However, all of the
initial input is lost if you type or hit `C-d' or `DEL'.  That is
inconvenient if you want to keep most of it and edit it only slightly."
  :type '(choice
          (const :tag "Do not insert default value as initial value"     nil)
          (const :tag "Insert (and leave cursor at end)"                 insert)
          (const :tag "Insert, preselect, and leave cursor at beginning" preselect-start)
          (const :tag "Insert, preselect, and leave cursor at end"       preselect-end))
  :group 'icicles)

;;;###autoload
(defcustom icicle-list-join-string "\n"
  "String joining items in a completion that is a list of strings.
When a completion candidate is a list of strings, this string is
used to join the strings in the list, for display and matching
purposes.")


;;;###autoload
(defcustom icicle-mark-position-in-candidate 'input-end
  "*Position of mark when you cycle through completion candidates.
Possible values are those for `icicle-point-position-in-candidate'."
  :type '(choice
          (const :tag "Leave mark at the beginning of the minibuffer input" input-start)
          (const :tag "Leave mark at the end of the minibuffer input" input-end)
          (const :tag "Leave mark at the beginning of the completion root" root-start)
          (const :tag "Leave mark at the end of the completion root" root-end))
  :group 'icicles)

;; Inspired from `icomplete-minibuffer-setup-hook'.
;;;###autoload
(defcustom icicle-minibuffer-setup-hook nil
  "*Functions run at the end of minibuffer setup for `icicle-mode'."
  :type 'hook :group 'icicles)

(unless (fboundp 'define-minor-mode)
  (defcustom icicle-mode nil            ; Emacs 20 only
    "Toggle minibuffer input completion and cycling.
Setting this variable directly does not take effect;
use either \\[customize] or command `icicle-mode'."
    :set (lambda (symbol value) (icicle-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean
    :group 'icicles
    :require 'icicles))

;;;###autoload
(defcustom icicle-point-position-in-candidate 'root-end
  "*Position of cursor when you cycle through completion candidates.
Possible values are:
 `input-start': beginning of the minibuffer input
 `input-end':   end of the minibuffer input
 `root-start':  beginning of the completion root
 `root-end':    end of the completion root
When input is expected to be a file name, `input-start' is just after
the directory, which is added automatically during completion cycling.
See also `icicle-mark-position-in-candidate'."
  :type '(choice
          (const :tag "Leave cursor at the beginning of the minibuffer input" input-start)
          (const :tag "Leave cursor at the end of the minibuffer input" input-end)
          (const :tag "Leave cursor at the beginning of the completion root" root-start)
          (const :tag "Leave cursor at the end of the completion root" root-end))
  :group 'icicles)

;;;###autoload
(defcustom icicle-change-region-background-flag
  (and (not (eq icicle-point-position-in-candidate icicle-mark-position-in-candidate))
       (boundp 'delete-selection-mode)
       delete-selection-mode)
  "*Non-nil means the region background is changed during input.
The background is changed to differ only slightly from the minibuffer
background, by default.  The actual region background color used is
`icicle-region-background'"
  :type 'boolean :group 'icicles)

;; This is essentially a version of `doremi-increment-color-component' for value only.
(defun icicle-increment-color-value (color increment)
  "Increase value component (brightness) of COLOR by INCREMENT."
  (unless (string-match "#" color)      ; Convert color name to #hhh...
    (setq color (hexrgb-color-values-to-hex (x-color-values color))))
  ;; Convert RGB to HSV
  (let* ((rgb (x-color-values color))
         (red   (/ (float (nth 0 rgb)) 65535.0)) ; Convert from 0-65535 to 0.0-1.0
         (green (/ (float (nth 1 rgb)) 65535.0))
         (blue  (/ (float (nth 2 rgb)) 65535.0))
         (hsv (hexrgb-rgb-to-hsv red green blue))
         (hue        (nth 0 hsv))
         (saturation (nth 1 hsv))
         (value      (nth 2 hsv)))
    (setq value (+ value (/ increment 100.0)))
    (when (> value 1.0) (setq value (1- value)))
    (hexrgb-color-values-to-hex (mapcar (lambda (x) (floor (* x 65535.0)))
                                        (hexrgb-hsv-to-rgb hue saturation value)))))

;;;###autoload
(defcustom icicle-redefine-standard-commands-flag t
  "*Non-nil means Icicle mode redefines some standard Emacs commands."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-region-background
  (if (featurep 'hexrgb)
      (icicle-increment-color-value     ; Use a color slightly darker than frame background.
       (or (and (boundp '1on1-active-minibuffer-frame-background)
                1on1-active-minibuffer-frame-background) ; In `oneonone.el'.
           (cdr (assq 'background-color (frame-parameters)))
           (face-background 'primary-selection))
       -8)
    (face-background 'default)) ; Invisible, if no `hexrgb.el'.
  "*Background color to use for region during minibuffer cycling."
  :type 'string :group 'icicles)

;;;###autoload
(defcustom icicle-require-match-flag nil
  "*Control REQUIRE-MATCH arg to `completing-read' and `read-file-name'.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
      `icicle-define-command' and `icicle-define-file-command'.
      You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"  nil)
          (const :tag "Do not require a match"            no-match-required)
          (const :tag "Require a partial match, with RET" partial-match-ok)
          (const :tag "Require a full match"              full-match-required))
  :group 'icicles)

;;;###autoload
(defcustom icicle-search-hook nil
  "*List of hook functions run by `icicle-search' (see `run-hooks')."
  :type 'hook :group 'icicles)

(defcustom icicle-search-ring-max 100
  "*Icicles version of `search-ring-max'."
  :type 'integer :group 'icicles)

;;;###autoload
(defcustom icicle-regexp-search-ring-max 100
  "*Icicles version of `regexp-search-ring-max'."
  :type 'integer :group 'icicles)

;;;###autoload
(defcustom icicle-show-Completions-initially-flag nil
  "*Non-nil means to show buffer *Completions* with no user input.
nil means that *Completions* is shown upon demand, via `TAB' or
`S-TAB'."
  :type 'boolean :group 'icicles)

;;;###autoload
(defcustom icicle-sort-function 'string-lessp
  "*Comparison function passed to `sort', to sort completion candidates.
This sorting determines the order of candidates when cycling and their
order in buffer *Completions*.  If the value nil, no sorting is done.

when `icicle-cycle-into-subdirs-flag' is non-nil, you might want to
use a function such as `icicle-sort-dirs-last' for this option, to
prevent cycling into subdirectories depth first."
  :type 'function :group 'icicles)



;;;###autoload
(defcustom icicle-thing-at-point-functions
  ;; Lisp symbol or file name, word, url.
  (list
   (lambda () 
     (if (region-active-p) (buffer-substring (region-beginning) (region-end))
       (thing-at-point 'symbol)))
   (lambda () (thing-at-point 'filename))
   (lambda () (thing-at-point 'word))
  'thing-at-point-url-at-point)
  "*List of functions that return a string at or near the cursor.
By default:
  The first returns a symbol or file name.
  The second returns a word.
  The third returns a URL.
Any number of functions may be used.  They are used in sequence by
command `icicle-insert-string-near-point'."
  :type '(repeat function) :group 'icicles)

;;;###autoload
(defcustom icicle-word-completion-key [(meta ?\ )]
  "*Key sequence to use for minibuffer word completion.
The value has the same form as a key-sequence arg to `define-key'.

Because file names, in particular, can contain spaces, some people
prefer this to be a non-printable key sequence, such as `M-SPC'.  This
is the default value in Icicles.

But because the spacebar is such a convenient key to hit, other people
prefer to use `SPC' for word completion, and to insert a space some
other way.  The usual way to do that is via `C-q SPC', but command
`icicle-insert-a-space' is provided for convenience.  You can bind
this to `M-SPC', for instance, in `minibuffer-local-completion-map',
`minibuffer-local-completion-map', and
`minibuffer-local-must-match-map'."
  :type 'sexp :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-match-regexp nil
  "*Nil or a regexp that buffer-name completion candidates must match.
If nil, then this does nothing.  If a regexp, then show only
candidates that match it (and match the user input).
See also `icicle-buffer-no-match-regexp'."
  :type '(choice (const :tag "None" nil) regexp) :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-no-match-regexp nil
  "*Nil or a regexp that buffer-name completion candidates must not match.
If nil, then this does nothing.  If a regexp, then show only
candidates that do not match it.
See also `icicle-buffer-match-regexp'."
  :type '(choice (const :tag "None" nil) regexp) :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-predicate nil
  "*Nil or a predicate that buffer-name candidates must satisfy.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a candidate, and only candidates that satisfy the predicate
are displayed.  For example, this value will show only buffers that
are associated with files:

  (lambda (bufname) (buffer-file-name (get-buffer bufname)))."
  :type '(choice (const :tag "None" nil) function) :group 'icicles)


;;;###autoload
(defcustom icicle-buffer-require-match-flag nil
  "*Override `icicle-require-match-flag' for `icicle-buffer*' commands.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
      `icicle-define-command' and `icicle-define-file-command'.
      You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"  nil)
          (const :tag "Do not require a match"            no-match-required)
          (const :tag "Require a partial match, with RET" partial-match-ok)
          (const :tag "Require a full match"              full-match-required))
  :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-extras nil
  "*List of additional buffer-name candidates added to the normal list.
List elements are strings."
  :type '(repeat string) :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-sort nil
  "*Nil or a sort function for buffer names.
Examples of sort functions are `icicle-buffer-sort-*...*-last' and
`string<'.  If nil, then buffer names are not sorted.  Option
`icicle-sort-function' is bound to `icicle-buffer-sort' by command
`icicle-buffer'."
  :type 'function :group 'icicles)

;;;###autoload
(defcustom icicle-buffer-configs
  `(("All" nil nil nil nil ,icicle-sort-function)
    ("Files" nil nil (lambda (bufname) (buffer-file-name (get-buffer bufname))) nil
     ,icicle-sort-function)
    ("Files and Scratch" nil nil (lambda (bufname) (buffer-file-name (get-buffer bufname)))
     ("*scratch*") ,icicle-sort-function)
    ("All, *...* Buffers Last" nil nil nil nil icicle-buffer-sort-*...*-last))
  "*List of option configurations available for `icicle-buffer-config'.
The form is (CONFIG...), where CONFIG is a list of these items:

 - Configuration name                    (string)
 - `icicle-buffer-match-regexp' value    (regexp string)
 - `icicle-buffer-no-match-regexp' value (regexp string)
 - `icicle-buffer-predicate' value       (function)
 - `icicle-buffer-extras' value          (list of strings)
 - `icicle-buffer-sort' value            (function)

A configuration describes which buffer names are displayed during
completion and their order."
  :type '(repeat (list
                  string                ; Configuration name
                  (choice (const :tag "None" nil) string) ; Match regexp
                  (choice (const :tag "None" nil) string) ; No-match regexp
                  (choice (const :tag "None" nil) function) ; Predicate
                  (choice (const :tag "None" nil) (repeat string)) ; Extras list
                  (choice (const :tag "None" nil) function))) ; Sort function
  :group 'icicles)

(defun icicle-buffer-sort-*...*-last (buf1 buf2)
  "Returns non-nil if BUF1 is `string<' BUF2 or only BUF2 starts with \"*\"."
  (let ((b1 (if completion-ignore-case (downcase buf1) buf1))
        (b2 (if completion-ignore-case (downcase buf2) buf2)))
    (if (string-match "^\\*" b1)
        (and (string-match "^\\*" b2) (string< b1 b2))
      (or (string-match "^\\*" b2) (string< b1 b2)))))




;;; Internal variables (alphabetical) ----------------------

(defvar icicle-icompleting-p nil
  "Internal flag: non-nil when editing text in minibuffer.
This is really non-nil when inside simple character-editing commands
such as `icicle-self-insert' and `icicle-delete-backward-char'.")


(defvar icicle-candidate-action-fn nil
  "Function to be applied to current completion candidate.
For `icicle-all-candidates-action' to be able to report successes,
this should return non-nil for \"success\" and nil for \"failure\".")

(defvar icicle-candidate-nb nil
  "Current completion candidate number, or nil if not cycling candidates.
Numbering starts at zero.")

(defvar icicle-cmd-calling-for-completion 'ignore
  "Last command causing display of list of possible completions.")


(defvar icicle-completion-candidates nil "Current list of completion candidates.")

(defvar icicle-completion-help-string ""
  "Description of minibuffer bindings.")

(defvar icicle-must-match-regexp nil
  "Nil or a regexp that completion candidates must match.
If nil, then this does nothing.  If a regexp (string), then show only
candidates that match it (and match the user input).
See also `icicle-must-not-match-regexp'.")

(defvar icicle-must-not-match-regexp nil
  "Nil or a regexp that completion candidates must not match.
If nil, then this does nothing.  If a regexp (string), then show only
candidates that do not match it.
See also `icicle-must-match-regexp'.")

(defvar icicle-extra-candidates nil
  "A list of extra completion candidates (strings).")

(defvar icicle-must-pass-predicate nil
  "Nil or a predicate that completion candidates must satisfy.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a candidate, and only candidates that satisfy the predicate
are displayed.")

(defvar icicle-current-completion-candidate-overlay nil
  "Overlay used to highlight current completion candidate.")

(defvar icicle-complete-input-overlay nil
  "Overlay used to highlight minibuffer input when it is complete.")

(defvar icicle-current-input "" "Current minibuffer input.")

(defvar icicle-default-directory default-directory
  "Local copy of `default-directory'.
Set whenever minibuffer is entered or input is completed.")

(defvar icicle-default-thing-insertion-flipped-p nil
  "Non-nil means a previous `M-.' in this succession was used with `C-u'.
This means that the meaning of `icicle-default-thing-insertion' has
been reversed.")

(defvar icicle-icicle-completing-p nil
  "Non-nil means that input currently uses Icicles completion.
This is used to distinguish direct calls to `old-completing-read' and
`old-read-file-name' from calls to them from the Icicles versions.
Such direct calls occur, for instance, from an `interactive' spec,
such as `(interactive \"bBuffer: \")'.")

(defvar icicle-incremental-completion-p nil
  "Takes the place of `icicle-incremental-completion-flag' during input.
The program updates this to `always' from `t' after *Completions* has
been displayed.")

(defvar icicle-ignored-extensions completion-ignored-extensions
  "Copy of `completion-ignored-extensions', serving as a control flag.
When `completion-ignored-extensions' changes, we remake
`icicle-ignored-extensions-regexp'.")

(defvar icicle-ignored-extensions-regexp
  (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|")
          "\\)\\'")
  "Regular expression matching ignored file extensions.
If this is nil, then no file extensions are ignored.
The ignored file extensions come from `completion-ignored-extensions'.")

(defvar icicle-initial-value ""
  "Initial value used in minibuffer completion.
Any function that reads from the minibuffer and accepts a default
value or initial value should, before reading, put that value in
`icicle-initial-value'.  For example, `completing-read' does that.")

(defvar icicle-insert-string-at-pt-start 0
  "Position of start of text `icicle-insert-string-at-point' inserted.")

(defvar icicle-insert-string-at-pt-end 0
  "Position of end of text `icicle-insert-string-at-point' inserted.")

(defvar icicle-last-completion-candidate ""
  "Last completion candidate used in minibuffer completion.
Any function that reads from the minibuffer and accepts a default
value or initial value should, before reading, put that value in
`icicle-last-completion-candidate'.  For example, `completing-read'
does that.")

;; This is used to be able to ignore `handle-switch-frame'.
(defvar icicle-last-completion-command nil "Last completion command used.")

(defvar icicle-last-input "" "Last minibuffer input.")

(defvar icicle-last-sort-function (or icicle-sort-function 'string-lessp)
  "Local copy of `icicle-sort-function', so we can restore it.")

(defvar icicle-mode-map nil
  "Keymap for Icicle mode.
These are top-level key bindings.
See also `icicle-rebind-completion-maps' for minibuffer bindings.")

(or icicle-mode-map
    (let ((map (make-sparse-keymap "Icicles")))
      (setq icicle-mode-map (make-sparse-keymap))
      (define-key icicle-mode-map [menu-bar] (make-sparse-keymap))
      (define-key icicle-mode-map [menu-bar icicles] (cons "Icicles" map))

      (define-key map [icicle-mode] '("Turn Off Icicle Mode" . icicle-mode))
      (define-key map [icicle-help] '("Help" . icicle-completion-help))
      (define-key map [icicle-separator-last] '("--"))
      (cond ((boundp 'menu-bar-frames-menu) ; Defined in `menu-bar+.el'.
             (define-key menu-bar-frames-menu [icicle-separator-frame] '("--"))
             (define-key menu-bar-frames-menu [icicle-font] '("[Icy] Change Font" . icicle-font))
             (define-key menu-bar-frames-menu [icicle-frame-fg]
               '("[Icy] Change Foreground..." . icicle-frame-fg))
             (define-key menu-bar-frames-menu [icicle-frame-bg]
               '("[Icy] Change Background..." . icicle-frame-bg)))
            (t
             (define-key map [icicle-font] '("Change Font of Frame..." . icicle-font))
             (define-key map [icicle-frame-fg]
               '("Change Foreground of Frame..." . icicle-frame-fg))
             (define-key map [icicle-frame-bg]
               '("Change Background of Frame..." . icicle-frame-bg))
             (define-key map [icicle-separator-frame] '("--"))))
      (put 'icicle-font 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
      (put 'icicle-frame-bg 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
      (put 'icicle-frame-fg 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
      (define-key map [icicle-toggle-sorting] '("Toggle Sorting" . icicle-toggle-sorting))
      (define-key map [icicle-toggle-ignore]
        '("Toggle Ignored Extensions" . icicle-toggle-ignored-extensions))
      (define-key map [icicle-separator-toggle] '("--"))

      (define-key map [icicle-color-theme] '("Choose Color Theme..." . icicle-color-theme))
      (put 'icicle-color-theme 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
      (define-key map [icicle-separator-misc] '("--"))
      (cond ((boundp 'menu-bar-file-menu) ; Use File menu, if available.
             (define-key menu-bar-file-menu [icicle-file-separator] '("--"))
             (define-key menu-bar-file-menu [icicle-delete-file]
               '("[Icy] Delete File..." . icicle-delete-file))
             (when (condition-case nil (require 'recentf) (error nil))
               (define-key menu-bar-file-menu [icicle-recent-file-other-window]
                 '("[Icy] Open Recent File Other Window..." . icicle-recent-file-other-window))
               (define-key menu-bar-file-menu [icicle-recent-file]
                 '("[Icy] Open Recent File..." . icicle-recent-file)))
             (define-key menu-bar-file-menu [icicle-recent-file-other-window]
               '("[Icy] Open File or Directory Other Window..." . icicle-find-file-other-window))
             (define-key menu-bar-file-menu [icicle-recent-file]
               '("[Icy] Open File or Directory..." . icicle-find-file)))             
            (t
             (define-key map [icicle-delete-file] '("Delete File..." . icicle-delete-file))
             (when (condition-case nil (require 'recentf) (error nil))
               (define-key map [icicle-recent-file-other-window]
                 '("Open Recent File Other Window..." . icicle-recent-file-other-window))
               (define-key map [icicle-recent-file]
                 '("Open Recent File..." . icicle-recent-file)))
             (define-key map [icicle-recent-file-other-window]
               '("Open File or Directory Other Window..." . icicle-find-file-other-window))
             (define-key map [icicle-recent-file]
               '("Open File or Directory ..." . icicle-find-file))))
      (put 'icicle-delete-file 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
      (put 'icicle-find-file 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
      (put 'icicle-find-file-other-window 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
      (when (condition-case nil (require 'recentf) (error nil))
        (put 'icicle-recent-file 'menu-enable
             '(and icicle-mode
               (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
        (put 'icicle-recent-file-other-window 'menu-enable
             '(and icicle-mode
               (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
      (define-key map [icicle-buffer-other-window]
        '("Switch To Buffer Other Window..." . icicle-buffer-other-window))
      (put 'icicle-buffer-other-window 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
      (define-key map [icicle-buffer] '("Switch To Buffer..." . icicle-buffer))
      (put 'icicle-buffer 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
      (cond ((not (boundp 'menu-bar-bookmark-map)) ; Use Bookmarks menu, if available.
             (define-key map [icicle-bookmark] '("Jump To Bookmark..." . icicle-bookmark))
             (define-key map [icicle-separator-bookmark-buffer] '("--")))
            (t
             (require 'bookmark)             ; `bookmark-buffer-name' is not autoloaded.
             (define-key menu-bar-bookmark-map [icicle-bookmark]
               '("[Icy] Jump to Bookmark Using Icicles..." . icicle-bookmark))))
      (put 'icicle-bookmark 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))

      (cond ((boundp 'menu-bar-search-menu) ; Use Search menu, if available.
             (define-key menu-bar-search-menu [icicle-separator-search] '("--"))
             (define-key menu-bar-search-menu [icicle-compilation-search]
               '("[Icy] Search Compilation/Grep Hits (Regexp)..." . icicle-compilation-search))
             (define-key menu-bar-search-menu [icicle-search]
               '("[Icy] Search (Regexp)..." . icicle-search)))
            (t
             (define-key map [icicle-compilation-search]
               '("Search Compilation/Grep Hits (Regexp)..." . icicle-compilation-search))
             (define-key map [icicle-search] '("Search (Regexp)..." . icicle-search))))
      (put 'icicle-compilation-search 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
             (condition-case nil (eq (current-buffer) (compilation-find-buffer)) (error nil))))
      (put 'icicle-search 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))

      (push (cons 'icicle-mode icicle-mode-map) minor-mode-map-alist)))

(defvar icicle-nb-of-other-cycle-candidates 0
  "Number of other candidates available for cycling.
This is for use by other libraries, in particular, `icomplete+.el'.")

;; Inspired from `icomplete-post-command-hook'.
(defvar icicle-post-command-hook nil
  "Functions added to `post-command-hook' when in `icicle-mode' .
Use command `icicle-mode' to set this up properly.")

;; Inspired from `icomplete-pre-command-hook'.  There is none, by default.
(defvar icicle-pre-command-hook nil
  "Functions added to `pre-command-hook' when in `icicle-mode' .
Use command `icicle-mode' to set this up properly.")

(defvar icicle-prompt "")

(defvar icicle-prompt-suffix ""
  "String to append to the input-completion prompt, if there is room.
Intended to remind you how to obtain help on input completion.")

(defvar icicle-saved-completion-candidates nil
  "Completion candidates user saved using `icicle-candidate-set-save'.")

(defvar icicle-saved-ignored-extensions nil
  "Local copy of `icicle-ignored-extensions', so we can restore it.")

(defvar icicle-saved-regexp-search-ring-max regexp-search-ring-max
  "Saved value of `search-ring-max', so it can be restored.")

(defvar icicle-saved-region-background nil
  "Background of `region' face.  Saved so it can be restored.")

(defvar icicle-saved-search-ring-max search-ring-max
  "Saved value of `search-ring-max', so it can be restored.")

(defvar icicle-thing-at-pt-fns-pointer 0
  "Index into `icicle-thing-at-point-functions' of current function.")

(defvar icicle-menu-items-alist nil)    ; Defined in `icicles-menu.el'.

(defvar icicle-successive-grab-count 0
  "Number of text things to be grabbed by next `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]'.")

(defvar icicle-thing-at-pt-fns-pointer 0
  "Current index into the car of `icicle-thing-at-point-functions'.
This points to the current function in the list.")


;;; Macros  ------------------------------------------------

(defmacro icicle-define-command
    (command doc-string function
     prompt table &optional predicate require-match initial-input hist def inherit-input-method
     bindings first-sexp undo-sexp last-sexp)
  ;; Hard-code these in doc string, because \\[...] prefers ASCII
  ;; `C-RET'   instead of `\\[icicle-candidate-action]'
  ;; `C-next'  instead of `\\[icicle-next-prefix-candidate-action]'
  ;; `C-prior' instead of `\\[icicle-previous-prefix-candidate-action]'
  ;; `next'    instead of `\\[icicle-next-apropos-candidate-action]'
  ;; `prior'   instead of `\\[icicle-previous-apropos-candidate-action]'
  "Define COMMAND with DOC-STRING based on FUNCTION.
COMMAND is a symbol.  DOC-STRING is a string.
FUNCTION is a function that takes one argument, read as input.
  (If the argument to FUNCTION is a file name or directory name, then
  use macro `icicle-define-file-command', instead.)

BINDINGS is a list of `let*' bindings added around the command code.
  The following bindings are pre-included - you can refer to them in
  the command body (including in FIRST-SEXP, LAST-SEXP, UNDO-SEXP).

  `orig-buff'   is bound to (current-buffer)
  `orig-window' is bound to (selected-window)

In case of error or user quit, the original buffer is restored.

FIRST-SEXP is a sexp evaluated before the main body of the command.
UNDO-SEXP is a sexp evaluated in case of error or if the user quits.
LAST-SEXP is a sexp evaluated after the main body of the command.

Other arguments are as for `completing-read'.

In order, the created command does this:

 - Uses DOC-STRING, with information about Icicles bindings appended.
 - Binds BINDINGS for the rest of the command.
 - Evaluates FIRST-SEXP.
 - Reads input with `completing-read', using PROMPT, TABLE, PREDICATE,
   REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and INHERIT-INPUT-METHOD.
 - Calls FUNCTION on the input that was read.
 - Evaluates UNDO-SEXP in case of error or if the user quits.
 - Evaluates LAST-SEXP.

The created command also binds `icicle-candidate-action-fn' to a
function that calls FUNCTION on the current completion candidate."
  `(defun ,command ()
    ,(concat doc-string "\n\nRead input, then "
             (and (symbolp function) (concat "call `" (symbol-name function) "' to "))
             "act on it.

Input-candidate completion and cycling are available.  While cycling,
these keys act on the current candidate:

\\<minibuffer-local-completion-map>\
`C-RET'   - Act on current completion candidate only
`C-next'  - Act, then move to next \
prefix-completion candidate
`C-prior' - Act, then move to previous \
prefix-completion candidate
`next'    - Act, then move to next \
apropos-completion candidate
`prior'   - Act, then move to previous \
apropos-completion candidate
`\\[icicle-all-candidates-action]'   - Act on *all* candidates, successively (careful!)

Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit.
This is an Icicles command - see `icicle-mode'.")
    (interactive)
    (let* ((orig-buff (current-buffer))
           (orig-window (selected-window))
           ,@bindings
           (icicle-candidate-action-fn
            (lambda (candidate)
              (condition-case action-fn-return
                  (progn
                    (condition-case in-action-fn
                        (funcall ',function candidate)
                      (error (unless (string= "Cannot switch buffers in minibuffer window"
                                              (error-message-string in-action-fn))
                               (error (error-message-string in-action-fn)))
                             (select-frame-set-input-focus (window-frame orig-window))
                             (funcall ',function candidate)))
                    (select-frame-set-input-focus (window-frame (minibuffer-window)))
                    nil)                ; Return nil for success.
                (error (error-message-string action-fn-return)))))) ; Return error msg.
      
      ,first-sexp
      (condition-case act-on-choice
          (funcall ',function (completing-read ,prompt ,table ,predicate ,require-match
                                               ,initial-input ,hist ,def ,inherit-input-method))
        (quit (when (buffer-live-p orig-buff) (switch-to-buffer orig-buff)) ,undo-sexp)
        (error (when (buffer-live-p orig-buff) (switch-to-buffer orig-buff)) ,undo-sexp
               (error (error-message-string act-on-choice))))
      ,last-sexp)))

(defmacro icicle-define-file-command
    (command doc-string function
     prompt &optional dir default-filename require-match initial-input predicate
     bindings first-sexp undo-sexp last-sexp)
  ;; Hard-code these in doc string, because \\[...] prefers ASCII
  ;; `C-RET'   instead of `\\[icicle-candidate-action]'
  ;; `C-next'  instead of `\\[icicle-next-prefix-candidate-action]'
  ;; `C-prior' instead of `\\[icicle-previous-prefix-candidate-action]'
  ;; `next'    instead of `\\[icicle-next-apropos-candidate-action]'
  ;; `prior'   instead of `\\[icicle-previous-apropos-candidate-action]'
  "Define COMMAND with DOC-STRING based on FUNCTION.
COMMAND is a symbol.  DOC-STRING is a string.
FUNCTION is a function that takes one file-name or directory-name
argument, read as input.  (Use macro `icicle-define-command' for a
FUNCTION whose argument is not a file or directory name.)

BINDINGS is a list of `let*' bindings added around the command code.
  The following bindings are pre-included - you can refer to them in
  the command body (including in FIRST-SEXP, LAST-SEXP, UNDO-SEXP).

  `orig-buff'   is bound to (current-buffer)
  `orig-window' is bound to (selected-window)

In case of error or user quit, the original buffer is restored.

FIRST-SEXP is a sexp evaluated before the main body of the command.
UNDO-SEXP is a sexp evaluated in case of error or if the user quits.
LAST-SEXP is a sexp evaluated after the main body of the command.

Other arguments are as for `read-file-name'.

In order, the created command does this:

 - Uses DOC-STRING, with information about Icicles bindings appended.
 - Binds BINDINGS for the rest of the command.
 - Evaluates FIRST-SEXP.
 - Reads input with `read-file-name', using PROMPT, DIR,
   DEFAULT-FILENAME, REQUIRE-MATCH, INITIAL-INPUT, and PREDICATE.
 - Calls FUNCTION on the input that was read.
 - Evaluates UNDO-SEXP in case of error or if the user quits.
 - Evaluates LAST-SEXP.

The created command also binds `icicle-candidate-action-fn' to a
function that calls FUNCTION on the current completion candidate."
  `(defun ,command ()
    ,(concat doc-string "\n\nRead input, then "
             (and (symbolp function) (concat "call `" (symbol-name function) "' to "))
             "act on it.

Input-candidate completion and cycling are available.  While cycling,
these keys act on the current candidate:

\\<minibuffer-local-completion-map>\
`C-RET'   - Act on current completion candidate only
`C-next'  - Act, then move to next \
prefix-completion candidate
`C-prior' - Act, then move to previous \
prefix-completion candidate
`next'    - Act, then move to next \
apropos-completion candidate
`prior'   - Act, then move to previous \
apropos-completion candidate
`\\[icicle-all-candidates-action]'   - Act on *all* candidates, successively (careful!)

Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit.
This is an Icicles command - see `icicle-mode'.")
    (interactive)
    (let* ((orig-buff (current-buffer))
           (orig-window (selected-window))
           ,@bindings
           (icicle-candidate-action-fn
            (lambda (candidate)
	      (let ((default-directory (icicle-file-name-directory-w-default icicle-current-input)))
		(condition-case action-fn-return
		    (progn
		      (condition-case in-action-fn
                        (funcall ',function candidate)
			(error (unless (string= "Cannot switch buffers in minibuffer window"
						(error-message-string in-action-fn))
				 (error (error-message-string in-action-fn)))
			       (select-frame-set-input-focus (window-frame orig-window))
			       (funcall ',function candidate)))
		      (select-frame-set-input-focus (window-frame (minibuffer-window)))
		      nil)                ; Return nil for success.
		  (error (error-message-string action-fn-return))))))) ; Return error msg.
      
      ,first-sexp
      (condition-case act-on-choice
          (funcall
           ',function
           (if (< emacs-major-version 21) ; No predicate arg for Emacs 20.
               (read-file-name ,prompt ,dir ,default-filename ,require-match ,initial-input)
             (read-file-name ,prompt ,dir ,default-filename ,require-match
                             ,initial-input ,predicate)))
        (quit  (when (buffer-live-p orig-buff) (switch-to-buffer orig-buff)) ,undo-sexp)
        (error (when (buffer-live-p orig-buff) (switch-to-buffer orig-buff)) ,undo-sexp
               (error (error-message-string act-on-choice))))
      ,last-sexp)))

(unless (fboundp 'select-frame-set-input-focus) ; Defined in Emacs 22.
  (defun select-frame-set-input-focus (frame)
    "Select FRAME, raise it, and set input focus, if possible."
    (select-frame frame)
    (raise-frame frame)
    ;; Ensure, if possible, that frame gets input focus.
    (cond ((eq window-system 'x) (x-focus-frame frame))
	  ((eq window-system 'w32) (w32-focus-frame frame)))
    (cond (focus-follows-mouse (set-mouse-position (selected-frame) (1- (frame-width)) 0)))))


;;; Commands -----------------------------------------------


;; Redefined standard commands..............................


;;; REPLACE ORIGINAL `exit-minibuffer' (built-in function), 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Removes *Completion* window.
;;; 
(or (fboundp 'old-exit-minibuffer)
(fset 'old-exit-minibuffer (symbol-function 'exit-minibuffer)))

;;;###autoload
(defun icicle-exit-minibuffer ()        ; Bound to `C-m' and `\n'.
  "Terminate this minibuffer argument.
Removes *Completions* window."
  ;; Bound to `C-m' and `\n' in `minibuffer-local-completion-map'.
  (interactive)
  (icicle-delete-windows-on "*Completions*")
  (old-exit-minibuffer))


;;; REPLACE ORIGINAL `minibuffer-complete-and-exit' (built-in function), 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Removes *Completion* window.
;;;
(or (fboundp 'old-minibuffer-complete-and-exit)
(fset 'old-minibuffer-complete-and-exit (symbol-function 'minibuffer-complete-and-exit)))

;;;###autoload
(defun icicle-minibuffer-complete-and-exit ()
  "If the minibuffer contents is a valid completion, then exit.
Otherwise try to complete it.  If completion leads to a valid completion,
a repetition of this command will exit.
Removes *Completions* window."
  ;; Bound to `C-m' and `\n' in `minibuffer-local-must-match-map'.
  (interactive)
  (save-excursion (icicle-delete-windows-on "*Completions*"))
  (old-minibuffer-complete-and-exit))


;;; REPLACE ORIGINAL `switch-to-completions' defined in `simple.el', 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects *Completions* window even if on another frame.
;;;
(or (fboundp 'old-switch-to-completions)
(fset 'old-switch-to-completions (symbol-function 'switch-to-completions)))

;;;###autoload
(defun icicle-switch-to-completions ()
  "Select the completion list window, *Completions*."
  (interactive)
  ;; Make sure we have a completions window.
  (or (get-buffer-window "*Completions*") (minibuffer-completion-help))
  (let ((window (get-buffer-window "*Completions*" 0))) ; Added 0 arg.
    (when window
      (select-window window)
      (goto-char (point-min))
      (forward-line 3))))

(defun icicle-select-minibuffer-contents ()
  "Select minibuffer contents and leave point at its beginning."
  (set-mark (if (eq 'preselect-start icicle-init-value-flag) (point-max) (point-min)))
  (goto-char (if (fboundp 'minibuffer-prompt-end)
                 (minibuffer-prompt-end)
               (if (eq 'preselect-start icicle-init-value-flag) (point-min) (point-max)))))

;; $$$ TEMPORARILY cannot use this in Emacs 22 if, for example, also use my library `replace+.el'.
;; Bug reported 2005-11-14.  Bad interaction between byte-compiling and defadvice in Emacs 22.
;; 
(when (< emacs-major-version 22)
  (defadvice next-history-element (after icicle-select-minibuffer-contents activate)
    "Select minibuffer contents and leave point at its beginning."
  (when (and icicle-mode (memq icicle-init-value-flag '(preselect-start preselect-end)))
      (icicle-select-minibuffer-contents)
      (setq deactivate-mark nil))))


;; Icicle commands..........................................

;;; Minibuffer editing commands  . . . . . . . . . . . . . .

;;;###autoload
(defun icicle-backward-delete-char-untabify (n &optional killflag)
  "`backward-delete-char-untabify' + update *Completions* with matches.
See description of `backward-delete-char-untabify'."
  (interactive "*p\nP")
  (icicle-call-then-update-Completions #'backward-delete-char-untabify n killflag))

;;;###autoload
(defun icicle-delete-backward-char (n &optional killflag)
  "`delete-backward-char' and update *Completions* with input matches.
See description of `delete-backward-char'."
  (interactive "*p\nP")
  (icicle-call-then-update-Completions #'delete-backward-char n killflag))

;;;###autoload
(defun icicle-backward-kill-word (arg)
  "`backward-kill-word' and update *Completions* with input matches.
See description of `backward-kill-word'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-word arg))

;;;###autoload
(defun icicle-kill-word (arg)
  "`kill-word' and update *Completions* with regexp input matches.
See description of `kill-word'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-word arg))

;;;###autoload
(defun icicle-backward-kill-sexp (arg)
  "`backward-kill-sexp' and update *Completions* with input matches.
See description of `backward-kill-sexp'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-sexp arg))

;;;###autoload
(defun icicle-kill-sexp (arg)
  "`kill-sexp' and update *Completions* with regexp input matches.
See description of `kill-sexp'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-sexp arg))

;;;###autoload
(defun icicle-backward-kill-sentence (arg)
  "`backward-kill-sentence' and update *Completions* with input matches.
See description of `backward-kill-sentence'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-sentence arg))

;;;###autoload
(defun icicle-kill-sentence (arg)
  "`kill-sentence' and update *Completions* with regexp input matches.
See description of `kill-sentence'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-sentence arg))

;;;###autoload
(defun icicle-backward-kill-paragraph (arg)
  "`backward-kill-paragraph' and update *Completions* with input matches.
See description of `backward-kill-paragraph'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-paragraph arg))

;;;###autoload
(defun icicle-kill-paragraph (arg)
  "`kill-paragraph' and update *Completions* with regexp input matches.
See description of `kill-paragraph'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-paragraph arg))

;;;###autoload
(defun icicle-kill-line (arg)
  "`kill-line' and update *Completions* with regexp input matches.
See description of `kill-line'."
  (interactive "P")
  (icicle-call-then-update-Completions #'kill-line arg))

;;;###autoload
(defun icicle-kill-region (beg end)     ; Don't bother with Emacs 22 optional 3rd arg.
  "`kill-region' and update *Completions* with regexp input matches.
See description of `kill-region'."
  (interactive "r")
  (icicle-call-then-update-Completions #'kill-region beg end))

;;;###autoload
(when (fboundp 'kill-region-wimpy)
  (defun icicle-kill-region-wimpy (beg end)
    "`kill-region-wimpy' and update *Completions* with input matches.
See description of `kill-region-wimpy'."
    (interactive "r")
    (icicle-call-then-update-Completions #'kill-region-wimpy beg end)))

;;;###autoload
(defun icicle-transpose-chars (arg)
  "`transpose-chars' and update *Completions* with regexp input matches.
See description of `transpose-chars'."
  (interactive "*P")
  (icicle-call-then-update-Completions #'transpose-chars arg))

;;;###autoload
(defun icicle-transpose-words (arg)
  "`transpose-words' and update *Completions* with regexp input matches.
See description of `transpose-words'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'transpose-words arg))

;;;###autoload
(defun icicle-transpose-sexps (arg)
  "`transpose-sexps' and update *Completions* with regexp input matches.
See description of `transpose-sexps'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'transpose-sexps arg))

;;;###autoload
(defun icicle-yank (arg)
  "`yank' and update *Completions* with regexp input matches.
See description of `yank'."
  (interactive "*P")
  (icicle-call-then-update-Completions #'yank arg))

;;;###autoload
(defun icicle-yank-pop (arg)
  "`yank-pop' and update *Completions* with regexp input matches.
See description of `yank-pop'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'yank-pop arg))

;; Commands to be used mainly in minibuffer . . . . . . . . 

;;;###autoload
(defun icicle-self-insert (n)
  "`self-insert' and update *Completions* with regexp input matches.
See description of `self-insert'."
  (interactive "p")
  (icicle-call-then-update-Completions #'self-insert-command n))

;; Make delete-selection mode recognize self-insertion, so it replaces region text.
;;(put 'icicle-self-insert 'pending-delete t)

;;;###autoload
(defun icicle-insert-a-space ()
  "Insert a space.
For convenience in the minibuffer - does the same thing as `C-q SPC'.
To use this, bind it to some key sequence in keymaps
`minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', and
`minibuffer-local-must-match-map'."
  (interactive) (insert ?\ ))

;; $$$ Probably need to do something to work around problem of Windows
;; selecting the new frame, when `pop-up-frames' is non-nil.  Need to
;; redirect focus back to the frame with the minibuffer.  Leave it as
;; is, for now, in hopes Emacs will eventually fix this.
;;
;;;###autoload
(defun icicle-completion-help ()
  "Describe minibuffer bindings for completion."
  (interactive)
  (with-output-to-temp-buffer "*Help*" (princ icicle-completion-help-string)))

;;;###autoload
(defun icicle-abort-minibuffer-input ()
  "Abort minibuffer input.
Remove \"*Completions*\" frame, if any, before aborting minibuffer
input via `abort-recursive-edit'."
  (interactive)
  (icicle-delete-windows-on "*Completions*")
  (abort-recursive-edit))

;; This is just the macro expansion of the following:
;; `(def-completion-wrapper icicle-abort-minibuffer-input :minibuffer-separator)'.
;; Taken from the definition of `def-completion-wrapper' in `completion.el'.
(put 'icicle-abort-minibuffer-input 'completion-function 'use-completion-minibuffer-separator)


;;;###autoload
(defun icicle-apropos-complete-and-exit ()
  "If the minibuffer contents is a valid apropos completion, then exit.
Otherwise try to complete it.  If completion leads to a valid
completion, then exit.
This is to `minibuffer-complete-and-exit' as `icicle-apropos-complete'
is to `minibuffer-complete'.  That is, it is the regexp-match version."
  ;; Bound to `S-RET' in `minibuffer-local-must-match-map'.
  (interactive)
  (let* ((icicle-apropos-complete-and-exit-p t) ; Suppress "[Sole apropos completion]" msg & wait.
         (candidates (icicle-apropos-complete)))
    (when (and candidates (null (cdr candidates))) ; Single candidate.
      (old-exit-minibuffer))))

;;;###autoload
(defun icicle-retrieve-last-input ()
  "Put the last real input into the minibuffer.
Use this to replace a completion candidate inserted during cycling."
  (interactive)
  (icicle-erase-minibuffer)
  (insert (if (and (icicle-file-name-input-p) insert-default-directory)
	      (expand-file-name icicle-current-input
				(icicle-file-name-directory-w-default icicle-current-input))
	    icicle-current-input))
  (when (interactive-p) (setq icicle-last-completion-command nil))
  (icicle-place-cursor icicle-current-input))


;;;###autoload
(defun icicle-previous-prefix-candidate (&optional nth)
  "Replace input by NTH previous prefix completion for an input.
Default value of NTH is 1, meaning use the previous prefix completion.
Negative NTH means use a subsequent, not previous, prefix completion."
  (interactive)
  (setq nth (or nth 1))
  (icicle-next-prefix-candidate (- nth)))

;;;###autoload
(defun icicle-next-prefix-candidate (&optional nth)
  "Replace input by NTH next prefix completion for an input.
Default value of NTH is 1, meaning use the next prefix completion.
Negative NTH means use a previous, not subsequent, prefix completion."
  (interactive)
  (icicle-next-candidate nth (if (icicle-file-name-input-p)
                                 'icicle-file-name-prefix-candidates
                               'icicle-prefix-candidates)))

;;;###autoload
(defun icicle-previous-apropos-candidate (&optional nth)
  "Replace input by NTH previous apropos completion for an input.
Default value of NTH is 1, meaning use the previous apropos completion.
Negative NTH means use a subsequent, not previous, apropos completion."
  (interactive)
  (setq nth (or nth 1))
  (icicle-next-apropos-candidate (- nth)))

;;;###autoload
(defun icicle-next-apropos-candidate (&optional nth)
  "Replace input by NTH next apropos completion for an input.
Default value of NTH is 1, meaning use the next apropos completion.
Negative NTH means use a previous, not subsequent, apropos completion."
  (interactive)
  (icicle-next-candidate nth (if (icicle-file-name-input-p)
                                 'icicle-file-name-apropos-candidates
                               'icicle-apropos-candidates)
                         'regexp-p))

;;;###autoload
(defun icicle-previous-prefix-candidate-action (&optional nth)
  "`icicle-candidate-action', then `icicle-previous-prefix-candidate'.
Optional argument NTH is as for `icicle-previous-prefix-candidate'"
  (interactive)
  (icicle-candidate-action)
  (icicle-previous-prefix-candidate nth))

;;;###autoload
(defun icicle-next-prefix-candidate-action (&optional nth)
  "`icicle-candidate-action', then `icicle-next-prefix-candidate'.
Optional argument NTH is as for `icicle-next-prefix-candidate'"
  (interactive)
  (icicle-candidate-action)
  (icicle-next-prefix-candidate nth))

;;;###autoload
(defun icicle-previous-apropos-candidate-action (&optional nth)
  "`icicle-candidate-action', then `icicle-previous-apropos-candidate'.
Optional argument NTH is as for `icicle-previous-apropos-candidate'"
  (interactive)
  (icicle-candidate-action)
  (icicle-previous-apropos-candidate nth))

;;;###autoload
(defun icicle-next-apropos-candidate-action (&optional nth)
  "`icicle-candidate-action', then `icicle-next-apropos-candidate'.
Optional argument NTH is as for `icicle-next-apropos-candidate'"
  (interactive)
  (icicle-candidate-action)
  (icicle-next-apropos-candidate nth))

;;;###autoload
(defun icicle-prefix-complete ()
  "Complete the minibuffer contents as far as possible, as a prefix.
If no characters can be completed, display the possible completions.
Scroll *Completions* window if this command is repeated.
Candidate completions are appropriate names whose prefix is the
minibuffer input, where appropriateness is determined by the context
\(command, variable, and so on).
Return nil if there is no valid completion.
Otherwise, return the list of completion candidates."
  (interactive)
  (setq icicle-current-input
        (if (memq last-command '(icicle-next-apropos-candidate icicle-previous-apropos-candidate
                                 icicle-next-prefix-candidate icicle-previous-prefix-candidate))
            icicle-last-input
          (icicle-minibuffer-contents)))
  (let ((common (try-completion icicle-current-input minibuffer-completion-table
                                minibuffer-completion-predicate)))
    (unless (and (string= icicle-current-input icicle-last-input)
                 (memq last-command '(icicle-prefix-complete icicle-candidate-set-complement)))
      (setq icicle-completion-candidates (if (icicle-file-name-input-p)
                                             (icicle-file-name-prefix-candidates
                                              icicle-current-input)
                                           (icicle-prefix-candidates icicle-current-input))))
    (icicle-save-or-restore-input)
    (cond ((null icicle-completion-candidates)
           (setq icicle-nb-of-other-cycle-candidates 0)
           (save-selected-window (icicle-delete-windows-on "*Completions*"))
           (minibuffer-message "  [No prefix completions]"))
          ((null (cdr icicle-completion-candidates)) ;Single candidate. Update minibuffer.
           (setq icicle-nb-of-other-cycle-candidates 0)
	   (unless icicle-icompleting-p
	     (icicle-erase-minibuffer)
	     (insert (setq icicle-last-completion-candidate
			   (if (and (icicle-file-name-input-p) insert-default-directory)
			       (expand-file-name (car icicle-completion-candidates)
						 (icicle-file-name-directory-w-default
						  icicle-current-input))
			     (car icicle-completion-candidates))))
	     (when (icicle-file-directory-p icicle-last-completion-candidate)
	       (setq icicle-default-directory icicle-last-completion-candidate)))
           (save-selected-window (icicle-delete-windows-on "*Completions*"))
	   (icicle-highlight-complete-input)
           (if icicle-icompleting-p
               (minibuffer-message (format "  [One prefix completion: %s]"
                                           (car icicle-completion-candidates)))
	     
             (minibuffer-message "  [Sole prefix completion]")))
          (t                            ; Multiple candidates.
	   (if icicle-icompleting-p
               (icicle-display-candidates-in-Completions common)
	     (icicle-erase-minibuffer)
	     (insert common)              ; Update minibuffer.
           (when (icicle-file-directory-p icicle-last-completion-candidate)
             (setq icicle-default-directory icicle-last-completion-candidate))
	   (when (member common icicle-completion-candidates)
	     (icicle-highlight-complete-input))
           (cond ((get-buffer-window "*Completions*" 0)
		  (if (and (eq icicle-last-completion-command 'icicle-prefix-complete)
			   (memq last-command '(icicle-prefix-complete handle-switch-frame)))
		      ;; Second TAB in a row.  Scroll window around.
		      (save-selected-window
			(select-window (get-buffer-window "*Completions*" 0))
			(condition-case nil
			    (scroll-up nil)
			  (end-of-buffer (progn (sit-for 0) (goto-char (point-min))))))
		    ;; Did something else (e.g. changed input).  Update the display.
		    (icicle-display-candidates-in-Completions common)))
		 ;; No window yet.  If 2nd TAB or no chars can be completed, show window.
		 (t
		  (cond ((and (memq last-command '(icicle-prefix-complete handle-switch-frame))
			      (eq icicle-last-completion-command 'icicle-prefix-complete)
			      completion-auto-help)
			 (icicle-display-candidates-in-Completions common))
			((member common icicle-completion-candidates)
			 (minibuffer-message "  [Complete, but not unique]"))
			((and (string= common icicle-current-input) completion-auto-help)
			 (icicle-display-candidates-in-Completions common))))))))
    (setq icicle-last-completion-command this-command)
    icicle-completion-candidates))

;;;###autoload
(defun icicle-prefix-word-complete ()
  "Complete the minibuffer contents at most a single word.
After one word is completed as much as possible, a space or hyphen
is added, provided that matches some possible completion.
Return nil if there is no valid completion, else t.
Candidate completions are appropriate names whose prefix is the
minibuffer input, where appropriateness is determined by the context
\(command, variable, and so on)."
  (interactive)
  (setq icicle-current-input
        (if (memq last-command '(icicle-next-apropos-candidate icicle-previous-apropos-candidate
                                 icicle-next-prefix-candidate icicle-previous-prefix-candidate))
            icicle-last-input
          (icicle-minibuffer-contents)))
  (let ((return-value (minibuffer-complete-word)))
    (setq icicle-completion-candidates
          (if (icicle-file-name-input-p)
              (icicle-file-name-prefix-candidates icicle-current-input)
            (icicle-prefix-candidates icicle-current-input)))
    (when (get-buffer-window "*Completions*" 0)
      (icicle-display-candidates-in-Completions icicle-current-input))
    (setq icicle-last-completion-command this-command)
    return-value))

;;;###autoload
(defun icicle-apropos-complete ()
  "Complete the minibuffer contents as far as possible.
This uses \"apropos completion\", defined as follows:
A completion contains the minibuffer input somewhere, as a substring.
Display a list of possible completions in buffer *Completions*.
Scroll *Completions* window if this command is repeated.
Candidate completions are appropriate names that match the current
input, taken as a regular expression, where appropriateness is
determined by the context (command, variable, and so on).
Return nil if there is no valid completion.
Otherwise, return the list of completion candidates."
  (interactive)
  (let* ((error-msg nil)
	(candidates
	 (condition-case lossage
	     (icicle-apropos-complete-1)
	   (invalid-regexp
	    (setq error-msg (car (cdr lossage)))
	    ;;(setq icicle-within-brackets (string-match "\\`Unmatched \\[" error-msg))
	    (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid " error-msg)
	      (setq error-msg "incomplete input")))
	   (error (setq error-msg (error-message-string lossage))))))
    (when error-msg (minibuffer-message (concat "  " error-msg)))
    candidates))

(defun icicle-apropos-complete-1 ()
  "Helper function for `icicle-apropos-complete'.
This does everything, except deal with regexp-match errors.
Returns the list of completion candidates."
  (setq icicle-current-input
        (if (memq last-command
                  '(icicle-next-apropos-candidate icicle-previous-apropos-candidate
                    icicle-next-apropos-candidate-action icicle-previous-apropos-candidate-action
                    icicle-next-prefix-candidate icicle-previous-prefix-candidate
                    icicle-next-prefix-candidate-action icicle-previous-prefix-candidate-action))
            icicle-last-input
          (icicle-minibuffer-contents)))
  (unless (and (string= icicle-current-input icicle-last-input)
               (memq last-command '(icicle-apropos-complete icicle-candidate-set-complement)))
    (setq icicle-completion-candidates (if (icicle-file-name-input-p)
                                           (icicle-file-name-apropos-candidates
                                            icicle-current-input)
                                         (icicle-apropos-candidates icicle-current-input))))
  (icicle-save-or-restore-input)
  (cond ((null icicle-completion-candidates)
         (setq icicle-nb-of-other-cycle-candidates 0)
         (save-selected-window (icicle-delete-windows-on "*Completions*"))
         (minibuffer-message "  [No apropos completion]"))
        ((null (cdr icicle-completion-candidates)) ;Single candidate. Update minibuffer.
         (setq icicle-nb-of-other-cycle-candidates 0)
         (unless icicle-icompleting-p
           (icicle-erase-minibuffer)
           (insert (setq icicle-last-completion-candidate
                         (if (and (icicle-file-name-input-p) insert-default-directory)
                             (expand-file-name (car icicle-completion-candidates)
                                               (icicle-file-name-directory-w-default
                                                icicle-current-input))
                           (car icicle-completion-candidates))))
           (when (icicle-file-directory-p icicle-last-completion-candidate)
             (setq icicle-default-directory icicle-last-completion-candidate)))
         (save-selected-window (icicle-delete-windows-on "*Completions*"))
	 (unless (boundp 'icicle-apropos-complete-and-exit-p)
           (icicle-highlight-complete-input)
	   (if icicle-icompleting-p
	       (minibuffer-message (format "  [One apropos completion: %s]"
						(car icicle-completion-candidates)))
	     (minibuffer-message "  [Sole apropos completion]"))))
	(t                              ; Multiple candidates.
         (if icicle-icompleting-p
             (icicle-display-candidates-in-Completions icicle-current-input)
           (icicle-erase-minibuffer)
           (insert icicle-current-input) ; Update minibuffer.
           (when (and (icicle-file-name-input-p)
                      (icicle-file-directory-p icicle-last-completion-candidate))
             (setq icicle-default-directory icicle-last-completion-candidate))
	   (when (member icicle-current-input icicle-completion-candidates)
             (icicle-highlight-complete-input))
           (if (get-buffer-window "*Completions*" 0)
               (if (and (eq icicle-last-completion-command 'icicle-apropos-complete)
                        (memq last-command '(icicle-apropos-complete handle-switch-frame)))
                   ;; Second `S-TAB' in a row.  Scroll window around.
                   (save-selected-window
                     (select-window (get-buffer-window "*Completions*" 0))
                     (condition-case nil
                         (scroll-up nil)
                       (end-of-buffer (progn (sit-for 0) (goto-char (point-min))))))
                 ;; Did something else (e.g. changed input).  Update the display.
                 (icicle-display-candidates-in-Completions icicle-current-input))
             ;; No window yet.  Show window.
             (icicle-display-candidates-in-Completions icicle-current-input)))))
  (setq icicle-last-completion-command this-command)
  icicle-completion-candidates)

;;;###autoload
(defun icicle-switch-to-Completions-buf ()
  "Select the completion list window.
The cursor is placed on the first occurrence of the current minibuffer
content.  You can use \\<completion-list-mode-map>\
`\\[icicle-switch-to-minibuffer]' to get back to the minibuffer."
  (interactive)
  (setq icicle-current-input (icicle-minibuffer-contents))
  (let ((window (get-buffer-window "*Completions*" t))
        (search-fn 'search-forward))
    (unless window                      ; Make sure we have a completions window.
      (icicle-apropos-complete)
      (setq window (get-buffer-window "*Completions*" t)
            search-fn 're-search-forward)) ; Use regexp search: input is not yet complete.
    (when window
      (select-window window)
      (let ((case-fold-search completion-ignore-case))
        (goto-char (point-min))
        (when (icicle-file-name-input-p)
          (setq icicle-current-input (icicle-file-name-nondirectory icicle-current-input)))
        (when (and (eq icicle-last-completion-command 'icicle-apropos-complete)
                   (not (memq last-command
                              '(icicle-previous-apropos-candidate icicle-next-apropos-candidate
                                icicle-previous-prefix-candidate icicle-next-prefix-candidate))))
          (setq search-fn 're-search-forward)) ; Use regexp search: input is not yet complete.
        (while (and (not (eobp))
                    (save-restriction
                      (narrow-to-region (point) (next-single-property-change (point) 'list-mode-item
                                                                             nil (point-max)))
                      (not (funcall search-fn icicle-current-input nil 'leave-at-end)))))
        (unless (eobp)
          (goto-char (match-beginning 0)))))))
          

;;;###autoload
(defun icicle-switch-to-minibuffer ()
  "Select the active minibuffer window.
The current candidate in *Completions* (under the cursor) is inserted
into the minibuffer as the current input.  You can use \\<minibuffer-local-completion-map>\
`\\[icicle-switch-to-Completions-buf]'
to switch to the *Completions* window."
  (interactive)
  (let (base-size)
    (when (active-minibuffer-window)
      (let ((completion (icicle-current-completion-in-Completions)))
	(select-window (active-minibuffer-window))
	(set-buffer (window-buffer (active-minibuffer-window)))
	;;(goto-char (icicle-minibuffer-prompt-end))
	(if (or (eq minibuffer-completion-table 'read-file-name-internal)
		(eq minibuffer-completion-table 'ffap-read-file-or-url-internal))
	    (setq base-size (save-excursion
			      (goto-char (point-max))
			      (skip-chars-backward (format "^%c" directory-sep-char))
			      (- (point) (point-min))))
	  (setq base-size 0))
	(if base-size
	    (delete-region (+ base-size (point-min)) (point)))
	(insert completion)))))

(defun icicle-current-completion-in-Completions ()
  "The completion candidate under the cursor in buffer *Completions*.
The name is returned as a string.
This must be called from buffer *Completions*."
  ;; This code comes from `choose-completion'.
  (let (beg end extent)
    (setq extent (extent-at (point) (current-buffer) 'list-mode-item))
    (if (and (not (eobp)) extent)
	(setq beg (extent-start-position extent) 
	      end (extent-end-position extent)))
    (when (null beg) (error "No completion here"))
    (buffer-substring beg end)))

;;;###autoload
(defun icicle-erase-minibuffer ()
  "Delete all user input in the minibuffer."
  (interactive)
  (if (fboundp 'delete-minibuffer-contents) (delete-minibuffer-contents) (erase-buffer)))


;; Replaces `previous-completion' (defined in `simple.el').
;;;###autoload
(defun icicle-move-to-previous-completion (n)
  "Move to the previous item in the completion list."
  (interactive "p")
  (setq n (or n 0))
  (icicle-move-to-next-completion (- n)))


;; Replaces `next-completion' (defined in `simple.el').
;; This is the same code, except:
;; 1. This highlights the current candidate.
;; 2. This wraps around from first to last and last to first.
;;;###autoload
(defun icicle-move-to-next-completion (n &optional no-minibuffer-follow-p)
  "Move to the next item in the completion list.
With prefix argument N, move N items (negative N means move backward).
Optional second argument, if non-nil, means do not copy the completion
back to the minibuffer."
  (interactive "p")
  (setq n (or n 0))
  (if (not (extent-at (point) (current-buffer) 'list-mode-item))
      (goto-char (next-single-property-change (point) 'list-mode-item)))
  (while (and (> n 0) (not (eobp)))
    (let ((extent (extent-at (point) (current-buffer) 'list-mode-item))
	  (end (point-max)))
      ;; If in a completion, move to the end of it.
      (if extent (goto-char (extent-end-position extent)))
      ;; Move to start of next one.
      (or (extent-at (point) (current-buffer) 'list-mode-item)
	  (goto-char (next-single-property-change (point) 'list-mode-item
						  nil end))))
    (setq n (1- n)))
  (while (and (< n 0) (not (bobp)))
    (let ((extent (extent-at (point) (current-buffer) 'list-mode-item))
	  (end (point-min)))
      ;; If in a completion, move to the start of it.
      (if extent (goto-char (extent-start-position extent)))
      ;; Move to the start of that one.
      (if (setq extent (extent-at (point) (current-buffer) 'list-mode-item
				  nil 'before))
	  (goto-char (extent-start-position extent))
	(goto-char (previous-single-property-change
		    (point) 'list-mode-item nil end))
	(if (setq extent (extent-at (point) (current-buffer) 'list-mode-item
				    nil 'before))
	    (goto-char (extent-start-position extent)))))
    (setq n (1+ n)))
  (when (extent-at (point) (current-buffer) 'list-mode-item)
    (icicle-place-overlay (point) (extent-end-position (extent-at (point) (current-buffer) 'list-mode-item))))
    
  (unless no-minibuffer-follow-p
    (save-excursion (save-window-excursion (icicle-switch-to-minibuffer)))))

;;;###autoload
(defun icicle-previous-line ()
  "Move up a line, in *Completions* buffer.  Wrap around first to last."
  (interactive)
  (let ((bolp-at-start (bolp)))
    (if (> (count-lines 1 (point)) (if bolp-at-start 3 4))
        (icicle-move-to-previous-completion 2)
      (goto-char (point-max))
      (icicle-move-to-previous-completion 1)
      (if bolp-at-start
          (while (not (bolp)) (icicle-move-to-previous-completion 1))
        (while (bolp) (icicle-move-to-previous-completion 1))))))

;;;###autoload
(defun icicle-next-line ()
  "Move down a line, in *Completions* buffer.  Wrap around last to first."
  (interactive)
  (let ((num-lines (- (count-lines (point-min) (point-max)) 1))
        (bolp-at-start (bolp)))
    (cond ((< (count-lines 1 (point)) (if bolp-at-start num-lines (1+ num-lines)))
           (icicle-move-to-next-completion 2)
           (when (and (bolp) (not bolp-at-start)) (icicle-move-to-next-completion 1)))
          (t
           (goto-char (point-min))
           (icicle-move-to-next-completion 1)
           (if bolp-at-start
               (while (not (bolp))
                 (icicle-move-to-next-completion 1))
             (while (bolp) (icicle-move-to-next-completion 1)))))))

;;;###autoload
(defun icicle-all-candidates-action ()
  "Take action on all completion candidates.
Apply `icicle-candidate-action-fn' to each completion candidate that
matches the current input (a regular expression), successively.
The candidates that were not successfully acted upon are listed in
buffer *Help*."
  (interactive)
  (unless icicle-candidate-action-fn (error "No action.  `icicle-candidate-action-fn' is nil."))
  (let ((candidates icicle-completion-candidates)
        (failures nil))
    (while candidates
      (let ((error-msg (condition-case act-on-each
                           (funcall icicle-candidate-action-fn (car candidates))
                         (error (error-message-string act-on-each)))))
        (when error-msg
          (setq failures (cons (cons (car candidates) error-msg) failures)))
        (setq candidates (cdr candidates))))
    (when failures
      (with-output-to-temp-buffer "*Help*"
        (princ "Action failures:")(terpri)(terpri)
        (mapcar (lambda (entry)
                  (princ (car entry)) (princ ":") (terpri) (princ "  ")
                  (princ (cdr entry)) (terpri))
                failures))))
  (icicle-abort-minibuffer-input))

;;;###autoload
(defun icicle-candidate-action ()
  "Take action on the current minibuffer-completion candidate.
If `icicle-candidate-action-fn' is non-nil, it is a function to apply
to the current candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'."
  (interactive)
  (if icicle-candidate-action-fn
      (funcall icicle-candidate-action-fn icicle-last-completion-candidate)
    (icicle-help-on-candidate))
  ;; Raise *Completions* frame, if displayed.  This helps keep *Completions* on top.
  (let ((compl-win (get-buffer-window "*Completions*" 'visible)))
    (when compl-win (save-window-excursion (select-window compl-win) (raise-frame)))))

;;;###autoload
(defun icicle-mouse-candidate-action (event)
  "Take action on the minibuffer-completion candidate clicked by mouse.
If `icicle-candidate-action-fn' is non-nil, it is a function to apply
to the clicked candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((posn-win (event-window event))
	(pos (event-point event))
        (posn-col (event-x event))
        (posn-row (event-y event))
        choice)
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (save-excursion
	(goto-char pos)
	(let (beg 
	      end
	      (extent (extent-at (point) (current-buffer) 'list-mode-item)))
	  (if (and (not (eobp)) extent)
	      (setq beg (extent-start-position extent) 
		    end (extent-end-position extent))) 
	  (if (null beg)
	      (error "No completion here"))
	  (setq choice (buffer-substring beg end)))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions
                               (event-point event)))
    (setq icicle-last-completion-candidate choice)
    (if icicle-candidate-action-fn
        (funcall icicle-candidate-action-fn icicle-last-completion-candidate)
      (icicle-help-on-candidate))
    ;; Raise *Completions* frame, if displayed.  This helps keep *Completions* on top.
    (let ((compl-win (get-buffer-window "*Completions*" 'visible)))
      (when compl-win
        (save-window-excursion
          (select-window compl-win)
          (raise-frame)
          ;; Do this because `icicle-candidate-action-fn' calls `select-frame-set-input-focus',
          ;; which can position mouse pointer on minibuffer frame.
          (set-mouse-position (selected-window) posn-col posn-row))))))



;;;###autoload
(defun icicle-narrow-candidates ()
  "Narrow the set completion candidates using another input regexp.
This, in effect, performs a set intersection operation on 1) the set
of candidates in effect before the operation and 2) the set of
candidates that match the current input.  You can repeatedly use this
command to continue intersecting candidate sets, progressively
narrowing the set of matches."
  ;; Not available for `old-completing-read' and `old-read-file-file-name', which can still be
  ;; called in Icicle mode by, for instance, an `interactive' spec (e.g. (interactive "bBuffer: ")).
  ;; Otherwise, we would throw to a non-existant catch.
  (interactive)
  (if (not icicle-icicle-completing-p)
      (minibuffer-message "  [Narrowing not available]")
    (let ((enable-recursive-minibuffers t)
          (icicle-inhibit-reminder-prompt-flag t)
	  (default-directory (icicle-file-name-directory-w-default icicle-current-input)))
      (cond ((null icicle-completion-candidates)
             (error
              (substitute-command-keys
               "No completion candidates.  Did you use `\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete]' or `\\[icicle-apropos-complete]'?")))
            ((null (cdr icicle-completion-candidates))
             (minibuffer-message "  [Sole completion]")
	     (if (icicle-file-name-input-p)
		 (throw 'icicle-read-top (expand-file-name (car icicle-completion-candidates)))
	       (throw 'icicle-read-top (car icicle-completion-candidates))))
            (t
	     (if (icicle-file-name-input-p)
		 (throw 'icicle-read-top
			(expand-file-name 
			 (completing-read "Match also (regexp): "
					  (mapcar #'list icicle-completion-candidates))))
	       (throw 'icicle-read-top
		      (completing-read "Match also (regexp): "
				       (mapcar #'list icicle-completion-candidates)))))))))

;;;###autoload
(defun icicle-candidate-set-complement ()
  "Complement the set of current completion candidates.
The new set of candidates is the set of `all-completions' minus the
set of candidates prior to executing this command - that is, all
possible completions of the appropriate type, except for those that
are in the current set of completions."
  (interactive)
  (setq icicle-completion-candidates
        (icicle-set-difference 
         (all-completions "" minibuffer-completion-table minibuffer-completion-predicate)
                          ;;icicle-completion-nospace-flag)
         icicle-completion-candidates))
  (when (icicle-file-name-input-p)
    (setq icicle-completion-candidates
          (icicle-sort-and-strip-ignored icicle-completion-candidates)))
  (when (get-buffer-window "*Completions*" 0)
    (icicle-display-candidates-in-Completions icicle-current-input))
  (icicle-retrieve-last-input)
  (minibuffer-message "  [set of candidates COMPLEMENTED]"))

;;;###autoload
(defun icicle-candidate-set-save ()
  "Save the set of current completion candidates, for later recall.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]')."
  (interactive)
  (setq icicle-saved-completion-candidates icicle-completion-candidates)
  (minibuffer-message "  [Current candidates SAVED]"))

;;;###autoload
(defun icicle-candidate-set-retrieve ()
  "Retrieve the saved set of completion candidates, making it current."
  (interactive)
  (setq icicle-completion-candidates icicle-saved-completion-candidates)
  (minibuffer-message "  [Saved completion candidates RESTORED]"))

;;;###autoload
(defun icicle-candidate-set-swap ()
  "Swap the saved set and current sets of completion candidates."
  (interactive)
  (setq icicle-saved-completion-candidates
        (prog1 icicle-completion-candidates
          (setq icicle-completion-candidates icicle-saved-completion-candidates)))
  (minibuffer-message "  [saved set of candidates SWAPPED with current]"))

;;;###autoload
(defun icicle-candidate-set-define ()
  "Define the set of current completion candidates by evalating a sexpr.
The sexpr must evaluate to a list of strings, such as is returned by
`all-completions'."
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (sexpr (eval-minibuffer "Eval: ")))
    (setq icicle-completion-candidates sexpr))
  (icicle-display-candidates-in-Completions "")
  (minibuffer-message (format "  [List of completion candidates DEFINED: %S]"
                              icicle-completion-candidates)))

;;;###autoload
(defun icicle-candidate-set-difference ()
  "Take the set difference between the current and saved candidates.
The new set of candidates is the set of candidates prior to executing
this command minus the saved set of candidates."
  (interactive)
  (icicle-candidate-set-1 'icicle-set-difference "  [saved set of candidates SUBTRACTED]"))

;;;###autoload
(defun icicle-candidate-set-union ()
  "Take the set union between the current and saved candidates.
The new set of candidates is the union of the saved set of candidates
and the set of candidates prior to executing this command."
  (interactive)
  (icicle-candidate-set-1 'icicle-set-union "  [saved set of candidates ADDED]"))

;;;###autoload
(defun icicle-candidate-set-intersection ()
  "Take the set intersection between the current and saved candidates.
The new set of candidates is the intersection of the saved set of
candidates and the set of candidates prior to executing this command."
  (interactive)
  (icicle-candidate-set-1 'icicle-set-intersection
                          "  [INTERSECTION of saved and current sets of candidates]"))

;;;###autoload
(defun icicle-help-on-candidate ()
  "Display help on current minibuffer-completion candidate.
The help displayed depends on the type of candidate, as follows:

menu item - the corresponding command is described using
`describe-function' (available only if `icicles-menu.el' is loaded)
command or other function  - described using `describe-function'
user option or other variable - described using `describe-variable'
face - described using `describe-face'
buffer name - modes described using `describe-mode' (Emacs > 20)

For any other candidate, `icicle-completion-help' is called to display
general Icicles help."
  (interactive)
  (let ((frame-with-focus (selected-frame))
        (cand-symb (intern-soft icicle-last-completion-candidate)))
    ;; Use command associated with a menu item.  `icicle-menu-items-alist' is set in
    ;; `icicles-menu.el'.  If non-nil, then `icicle-execute-menu-command' is being called.
    (save-selected-window
      (when (consp icicle-menu-items-alist) ; This is a call to `icicle-execute-menu-command'.
	(setq cand-symb (cdr (assoc icicle-last-completion-candidate icicle-menu-items-alist)))
	(while (consp cand-symb) (setq cand-symb (car cand-symb)))
	(unless (symbolp cand-symb) (setq cand-symb nil))) ; Menu item with lambda definition.
      (cond (cand-symb
	     (cond ((functionp cand-symb) (describe-function cand-symb))
		   ((boundp cand-symb) (describe-variable cand-symb))
		   ((facep cand-symb) (hyper-describe-face cand-symb))
		   (t (icicle-msg-maybe-in-minibuffer "No help"))))
	    (t                            ; Not a symbol - treat string itself.
	     (cond ((and (bufferp (get-buffer icicle-last-completion-candidate))
			 (condition-case nil ; Emacs 21+ `describe-mode' takes arg; not Emacs 20
			     (describe-mode icicle-last-completion-candidate)
			   (wrong-number-of-arguments nil))))
		   (t
		    (icicle-msg-maybe-in-minibuffer "No help")))))
      )
    (message nil)))                        ; Let minibuffer contents show immmediately.



;; Commands to be used mainly at top level  . . . . . . . .

(defalias 'icy-mode 'icicle-mode)

;; Main command.  Inspired from `icomplete-mode'.
;;;###autoload
(if (fboundp 'define-minor-mode)
    ;; Emacs 21+ ------------
    (eval '(define-minor-mode icicle-mode
            "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.

Icicle mode binds several keys in the minibuffer.  For more
information, use `\\<minibuffer-local-completion-map>\\[icicle-completion-help]' when the \
minibuffer is active (e.g. `\\[execute-extended-command] \\[icicle-completion-help]').

The following commands are also available in Icicle mode, and are
intended for top-level-use.

`icicle-bookmark'                  - jump to a bookmark
`icicle-buffer'                    - switch to a different buffer
`icicle-buffer-other-window'
`icicle-color-theme'               - change color theme
`icicle-completion-help'           - give bindings for completion
`icicle-delete-file'               - delete a file or directory
`icicle-doc'                       - show doc of function or variable
`icicle-find-file'                 - open a file or directory
`icicle-font'                      - change font of current frame
`icicle-frame-bg'                  - change background of frame
`icicle-frame-fg'                  - change foreground of frame
`icicle-fundoc'                    - show the doc of a function
`icicle-mode'                      - toggle icicle mode
`icicle-recent-file'               - open a recently used file
`icicle-recent-file-other-window'
`icicle-search'                    - search for a regexp match
`icicle-toggle-ignored-extensions' - toggle respect of
`completion-ignored-extensions'
`icicle-toggle-sorting'            - toggle sorting of completions
`icicle-vardoc'                    - show the doc of a variable

In a compilation-mode buffer, such as `*grep*', this is useful:
`icicle-compilation-search'        - `icicle-search' and show hits"
            :global t :group 'icicles :lighter " Icy" :init-value t
            (cond (icicle-mode
                   (add-hook 'minibuffer-setup-hook 'icicle-minibuffer-setup)
		   (add-hook 'minibuffer-exit-hook  'icicle-reset-icicle-completing-p)
		   (add-hook 'isearch-mode-hook     'icicle-bind-isearch-keys)
		   (add-hook 'completion-setup-hook 'icicle-set-calling-cmd 'append)
		   (icicle-redefine-standard-options)
                   (icicle-undo-std-completion-faces)
                   (icicle-redefine-std-completion-fns))
                  (t
                   (remove-hook 'minibuffer-setup-hook 'icicle-minibuffer-setup)
		   (remove-hook 'minibuffer-exit-hook  'icicle-reset-icicle-completing-p)
		   (remove-hook 'isearch-mode-hook     'icicle-bind-isearch-keys)
		   (remove-hook 'completion-setup-hook 'icicle-set-calling-cmd)
		   (icicle-restore-standard-options)
                   ;; $$$ Should restore standard completion faces here.
                   (icicle-restore-std-completion-fns)))
            (message "Turning %s Icicle mode..." (if icicle-mode "ON" "OFF"))
            (icicle-rebind-completion-maps icicle-mode)
            (message "Turning %s Icicle mode...done" (if icicle-mode "ON" "OFF"))))

  ;; Emacs 20 ------------
  (defun icicle-mode (&optional arg)
    "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.

Icicle mode binds several keys in the minibuffer.  For more
information, use `\\<minibuffer-local-completion-map>\\[icicle-completion-help]' when the \
minibuffer is active (e.g. `\\[execute-extended-command] \\[icicle-completion-help]').

The following commands are also available in Icicle mode, and are
intended for top-level-use.

`icicle-bookmark'                  - jump to a bookmark
`icicle-buffer'                    - switch to a different buffer
`icicle-buffer-other-window'
`icicle-color-theme'               - change color theme
`icicle-completion-help'           - give bindings for completion
`icicle-delete-file'               - delete a file or directory
`icicle-doc'                       - show doc of function or variable
`icicle-find-file'                 - open a file or directory
`icicle-font'                      - change font of current frame
`icicle-frame-bg'                  - change background of frame
`icicle-frame-fg'                  - change foreground of frame
`icicle-fundoc'                    - show the doc of a function
`icicle-mode'                      - toggle icicle mode
`icicle-recent-file'               - open a recently used file
`icicle-recent-file-other-window'
`icicle-search'                    - search for a regexp match
`icicle-toggle-ignored-extensions' - toggle respect of
`completion-ignored-extensions'
`icicle-toggle-sorting'            - toggle sorting of completions
`icicle-vardoc'                    - show the doc of a variable

In a compilation-mode buffer, such as `*grep*', this is useful:
`icicle-compilation-search'        - `icicle-search' and show hits"
    (interactive "P")
    (setq icicle-mode (if arg (> (prefix-numeric-value arg) 0) (not icicle-mode)))
    (icicle-rebind-completion-maps icicle-mode)
    (cond (icicle-mode
           ;; This is not really necessary after first time - no great loss.
           (add-hook 'minibuffer-setup-hook 'icicle-minibuffer-setup)
	   (add-hook 'minibuffer-exit-hook  'icicle-reset-icicle-completing-p)
           (icicle-redefine-std-completion-fns)
	   (add-hook 'isearch-mode-hook     'icicle-bind-isearch-keys)
	   (add-hook 'completion-setup-hook 'icicle-set-calling-cmd 'append)
	   (icicle-redefine-standard-options)
           (message "Icicle mode is now ON"))
          (t
           (remove-hook 'minibuffer-setup-hook 'icicle-minibuffer-setup)
	    (remove-hook 'minibuffer-exit-hook 'icicle-reset-icicle-completing-p)
           (icicle-restore-std-completion-fns)
	   (remove-hook 'isearch-mode-hook     'icicle-bind-isearch-keys)
	   (remove-hook 'completion-setup-hook 'icicle-set-calling-cmd)
	   (icicle-restore-standard-options)
           (message "Icicle mode is now OFF"))))
  (add-to-list 'minor-mode-alist '(icicle-mode " Icy")))

;; This is only used in Emacs 22+, but we define it always anyway.
(defun icicle-undo-std-completion-faces ()
  "Get rid of standard completion-root highlighting in *Completions*."
  ;; Do this because the standard Emacs 22 highlighting can interfere with
  ;; apropos-completion highlighting.
  (when (fboundp 'face-spec-reset-face)
    (when (facep 'completions-common-part)
      (face-spec-reset-face 'completions-common-part)
      (set-face-attribute 'completions-common-part nil :inherit nil))
    (when (facep 'completions-first-difference)
      (face-spec-reset-face 'completions-first-difference)
      (set-face-attribute 'completions-first-difference nil :inherit nil))))

(defun icicle-redefine-std-completion-fns ()
  "Replace standard completion functions with versions for Icicle mode."
  (when (fboundp 'icicle-completing-read)
    (defalias 'exit-minibuffer              (symbol-function 'icicle-exit-minibuffer))
    (defalias 'minibuffer-complete-and-exit (symbol-function 'icicle-minibuffer-complete-and-exit))
    (defalias 'switch-to-completions        (symbol-function 'icicle-switch-to-completions))
    (defalias 'choose-completion-string     (symbol-function 'icicle-choose-completion-string))
    ;;(defalias 'list-mode-item-mouse-selected(symbol-function 'icicle-mouse-choose-completion))
    (defalias 'completion-setup-function    (symbol-function 'icicle-completion-setup-function))
    (defalias 'completing-read              (symbol-function 'icicle-completing-read))
    (defalias 'read-file-name               (symbol-function 'icicle-read-file-name))
    (defalias 'dabbrev-completion           (symbol-function 'icicle-dabbrev-completion))
    (defalias 'repeat-complex-command       (symbol-function 'icicle-repeat-complex-command))
    (defalias 'lisp-complete-symbol         (symbol-function 'icicle-lisp-complete-symbol))
    (defalias 'read-from-minibuffer         (symbol-function 'icicle-read-from-minibuffer))
    (defalias 'read-string                  (symbol-function 'icicle-read-string))))

(defun icicle-restore-std-completion-fns ()
  "Restore standard completion functions replaced in Icicle mode."
  (when (fboundp 'old-completing-read)
    (defalias 'exit-minibuffer              (symbol-function 'old-exit-minibuffer))
    (defalias 'minibuffer-complete-and-exit (symbol-function 'old-minibuffer-complete-and-exit))
    (defalias 'switch-to-completions        (symbol-function 'old-switch-to-completions))
    (defalias 'choose-completion-string     (symbol-function 'old-choose-completion-string))
    ;;(defalias 'list-mode-item-mouse-selected(symbol-function 'old-mouse-choose-completion))
    (defalias 'completion-setup-function    (symbol-function 'old-completion-setup-function))
    (defalias 'completing-read              (symbol-function 'old-completing-read))
    (defalias 'read-file-name               (symbol-function 'old-read-file-name))
    (defalias 'dabbrev-completion           (symbol-function 'old-dabbrev-completion))
    (defalias 'repeat-complex-command       (symbol-function 'old-repeat-complex-command))
    (defalias 'lisp-complete-symbol         (symbol-function 'old-lisp-complete-symbol))
    (defalias 'read-from-minibuffer         (symbol-function 'old-read-from-minibuffer))
    (defalias 'read-string                  (symbol-function 'old-read-string))))


(defun icicle-redefine-standard-options ()
  "Replace certain standard Emacs options with Icicles versions."
  (when (boundp 'icicle-search-ring-max)
    (setq icicle-saved-search-ring-max        search-ring-max ; Save it.
          search-ring-max                     icicle-search-ring-max)
    (setq icicle-saved-regexp-search-ring-max regexp-search-ring-max ; Save it.
          regexp-search-ring-max              icicle-regexp-search-ring-max)))

(defun icicle-restore-standard-options ()
  "Restore standard Emacs options replaced in Icicle mode."
  (when (boundp 'icicle-saved-search-ring-max)
    (setq search-ring-max        icicle-saved-search-ring-max)
    (setq regexp-search-ring-max icicle-saved-regexp-search-ring-max)))

(icicle-define-command icicle-bookmark  ; Command name
                       "Jump to a bookmark." ; Doc string
                       bookmark-jump    ; Function to perform the action
                       "Bookmark: " (mapcar #'list (bookmark-all-names)) ; completing-read args
                       nil t (or (and (boundp 'bookmark-current-bookmark)
                                      bookmark-current-bookmark)
                                 (bookmark-buffer-name)))

(icicle-define-command icicle-buffer    ; Command name
                       "Switch to a different buffer.
These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-match-regexp'    - Regexp that buffers must match
 `icicle-buffer-no-match-regexp' - Regexp buffers must not match
 `icicle-buffer-predicate'       - Predicate buffer must satisfy
 `icicle-buffer-extras'          - Extra buffers to display
 `icicle-buffer-sort'            - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).
See also command `icicle-buffer-config'." ; Doc string
                       switch-to-buffer ; Function to perform the action
                       "Switch to buffer: " ; completing-read args
                       (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
                       nil nil (buffer-name (if (fboundp 'another-buffer)
                                                (another-buffer nil t)
                                              (other-buffer (current-buffer))))
                       'buffer-history nil nil
                       ((icicle-must-match-regexp icicle-buffer-match-regexp) ; Filter bindings
                        (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
                        (icicle-must-pass-predicate icicle-buffer-predicate)
                        (icicle-extra-candidates icicle-buffer-extras)
                        (icicle-sort-function icicle-buffer-sort)
			(icicle-require-match-flag icicle-buffer-require-match-flag)))

(icicle-define-command icicle-buffer-other-window ; Command name
                       "Switch to a different buffer in another window.
These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-match-regexp'    - Regexp that buffers must match
 `icicle-buffer-no-match-regexp' - Regexp buffers must not match
 `icicle-buffer-predicate'       - Predicate buffer must satisfy
 `icicle-buffer-extras'          - Extra buffers to display
 `icicle-buffer-sort'            - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).
See also command `icicle-buffer-config'" ; Doc string
                       switch-to-buffer-other-window ; Function to perform the action
                       "Switch to buffer: " ; completing-read args
                       (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
                       nil nil (buffer-name (if (fboundp 'another-buffer)
                                                (another-buffer nil t)
                                              (other-buffer (current-buffer))))
                       'buffer-name-history nil nil
                       ((icicle-must-match-regexp icicle-buffer-match-regexp) ; Filter bindings
                        (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
                        (icicle-must-pass-predicate icicle-buffer-predicate)
                        (icicle-extra-candidates icicle-buffer-extras)
                        (icicle-sort-function icicle-buffer-sort)
			(icicle-require-match-flag icicle-buffer-require-match-flag)))

(icicle-define-command icicle-add-buffer-candidate ; Command name
                       "Add buffer as an always-show completion candidate.
This just adds the buffer to `icicle-buffer-extras'." ; Doc string
                       (lambda (buf)
                         (add-to-list 'icicle-buffer-extras buf) ; Action function
                         (customize-save-variable 'icicle-buffer-extras icicle-buffer-extras)
                         (message "Buffer `%s' added to always-show buffers" buf))
                       "Buffer candidate to show always: " ; completing-read args
                       (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
                       nil nil nil 'buffer-name-history
                       (buffer-name (if (fboundp 'another-buffer)
                                        (another-buffer nil t)
                                      (other-buffer (current-buffer)))))

(icicle-define-command icicle-remove-buffer-candidate ; Command name
                       "Remove buffer as an always-show completion candidate.
This just removes the buffer from `icicle-buffer-extras'." ; Doc string
                       (lambda (buf)    ; Action function
                         (setq icicle-buffer-extras (delete buf icicle-buffer-extras))
                         (customize-save-variable 'icicle-buffer-extras icicle-buffer-extras)
                         (message "Buffer `%s' removed from always-show buffers" buf))
                       "Remove buffer from always-show list: " ; completing-read args
                       (mapcar #'list icicle-buffer-extras)
                       nil t nil 'buffer-name-history
                       (car icicle-buffer-extras))

(icicle-define-command icicle-buffer-config ; Command name
                       "Choose a configuration of user options for `icicle-buffer'.
See user option `icicle-buffer-configs'.  See also commands
`icicle-add-buffer-config' and `icicle-remove-buffer-config'." ; Doc string
                       (lambda (config-name) ; Function to perform the action
                         (let ((config (assoc config-name icicle-buffer-configs)))
                           (setq icicle-buffer-match-regexp (elt config 1))
                           (setq icicle-buffer-no-match-regexp (elt config 2))
                           (setq icicle-buffer-predicate (elt config 3))
                           (setq icicle-buffer-extras (elt config 4))
                           (setq icicle-buffer-sort (elt config 5))))
                       "Configuration: " icicle-buffer-configs nil t) ; completing-read args

(defun icicle-add-buffer-config ()
  "Add buffer configuration to `icicle-buffer-configs'.
You are prompted for the buffer configuration components.
For the list of extra buffers to always display, you can choose them
using `C-mouse-2', `C-RET', and so on, just as you would make any
Icicles multiple choice."
  (interactive)
  (let ((name (read-from-minibuffer "Add buffer configuration.  Name: "))
        (match-regexp (icicle-read-from-minibuf-nil-default
                       "Regexp to match: " nil nil nil nil icicle-buffer-match-regexp))
        (nomatch-regexp (icicle-read-from-minibuf-nil-default
                         "Regexp not to match: " nil nil nil nil icicle-buffer-no-match-regexp))
        (pred (icicle-read-from-minibuf-nil-default "Predicate to satify: " nil nil nil nil
                                                    icicle-buffer-predicate))
        (extras (progn (message "Choose extra buffers to show...") (sit-for 1)
                       (icicle-buffer-list)))
        (sort-fn (icicle-read-from-minibuf-nil-default "Sort function: " nil nil t nil
                                                       (symbol-name icicle-buffer-sort))))
    (add-to-list 'icicle-buffer-configs
                 (list name match-regexp nomatch-regexp pred extras sort-fn))
    (customize-save-variable 'icicle-buffer-configs icicle-buffer-configs)
    (message "Buffer configuration `%s' added" (caar icicle-buffer-configs))))

(defun icicle-read-from-minibuf-nil-default (prompt &optional initial-contents keymap read hist
                                             default-value inherit-input-method)
  "Like `read-from-minibuffer', but return nil for empty input.
Args are as for `read-from-minibuffer'.
If nothing is input, then nil is returned."
  (let ((input (read-from-minibuffer prompt initial-contents keymap nil hist default-value
                                     inherit-input-method)))
    (if (string= "" input)
        nil
      (if read
          (car (read-from-string input))
        input))))

(icicle-define-command icicle-buffer-list ; Command name
                       "Choose a list of buffer names.
The list of names (strings) is returned." ; Doc string
                       (lambda (name) (push name buf-names)) ; Function to perform the action
                       "Choose buffer (`RET' when done): " ; completing-read args
                       (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
                       nil nil nil 'buffer-name-history nil nil
                       ((buf-names nil)) ; Filter bindings
                       nil nil
                       (delete "" buf-names))

(icicle-define-command icicle-remove-buffer-config ; Command name
                       "Remove buffer configuration from `icicle-buffer-configs'." ; Doc string
                       (lambda (config-name) ; Action function
                         (setq icicle-buffer-configs
                               (delete (assoc config-name icicle-buffer-configs)
                                       icicle-buffer-configs))
                         (customize-save-variable 'icicle-buffer-configs icicle-buffer-configs)
                         (message "Buffer configuration `%s' removed" config-name))
                       "Remove buffer configuration: " ; completing-read args
                       (mapcar (lambda (config) (list (car config))) icicle-buffer-configs)
                       nil t nil nil (caar icicle-buffer-configs))

(icicle-define-command icicle-color-theme ; Command name
                       "Change color theme. ; Doc string
To use this command, you must have loaded library `color-theme.el',
available from http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme." ; Doc string
                       (lambda (theme) (funcall (intern theme))) ; Action - just call the theme.
                       "Theme: " icicle-color-themes nil t) ; completing-read args

(icicle-define-file-command icicle-delete-file ; Command name
                            "Delete a file or directory." ; Doc string
                            icicle-delete-file-or-directory ; Function to perform the action
                            "Delete file or directory: " ; read-file-name args
                            default-directory nil t)

(defun icicle-delete-file-or-directory (file)
  "Delete file (or directory) FILE."
  (condition-case i-delete-file
      (if (eq t (car (file-attributes file)))
          (delete-directory file)
        (delete-file file))
    (error (message (error-message-string i-delete-file))
           (error (error-message-string i-delete-file)))))

(icicle-define-file-command icicle-find-file ; Command name
                            "Visit a file or directory." ; Doc string
                            find-file   ; Function to perform the action
                            "File or directory: ") ; read-file-name args

(icicle-define-file-command icicle-find-file-other-window ; Command name
                            "Visit a file or directory in another window." ; Doc string
                            find-file-other-window ; Function to perform the action
                            "File or directory: ") ; read-file-name args

(icicle-define-command icicle-font      ; Command name
                       "Change font of current frame." ; Doc string
                       (lambda (font)   ; Function to perform the action
                         (modify-frame-parameters orig-frame (list (cons 'font font))))
                       "Font: " (mapcar #'list (list-fonts "*")) ; completing-read args
                       nil t nil nil nil nil
                       ((orig-frame (selected-frame)) ; Additional bindings
                        (orig-font (frame-parameter nil 'font)))
                       nil              ; Additional code at beginning
                       (modify-frame-parameters orig-frame ; Undo code
                                                (list (cons 'font orig-font)))
                       nil)             ; Additional code at end

(icicle-define-command icicle-frame-bg  ; Command name
                       "Change background of current frame." ; Doc string
                       (lambda (color)  ; Function to perform the action
                         (modify-frame-parameters orig-frame
                                                  (list (cons '[default background] color))))
                       "Background color:: " (read-color-completion-table)
                       nil t nil nil nil nil ; completing-read args
                       ((orig-frame (selected-frame)) ; Additional bindings
                        (orig-bg (frame-parameter nil '[default background])))
                       nil              ; Additional code at beginning
                       (modify-frame-parameters orig-frame ; Undo code
                                                (list (cons '[default background] orig-bg)))
                       nil)             ; Additional code at end

(icicle-define-command icicle-frame-fg  ; Command name
                       "Change foreground of current frame." ; Doc string
                       (lambda (color)  ; Function to perform the action
                         (modify-frame-parameters orig-frame
                                                  (list (cons '[default foreground] color))))
                       "Foreground color:: " (read-color-completion-table)
                       nil t nil nil nil nil ; completing-read args
                       ((orig-frame (selected-frame)) ; Additional bindings
                        (orig-bg (frame-parameter nil '[default foreground])))
                       nil              ; Additional code at beginning
                       (modify-frame-parameters orig-frame ; Undo code
                                                (list (cons '[default foreground] orig-bg)))
                       nil)             ; Additional code at end

(icicle-define-command icicle-recent-file ; Command name
                       "Open a recently used file." ; Doc string
                       find-file        ; Function to perform the action
                       "Recent file: " (mapcar (lambda(x) (cons x 0)) file-name-history) ; completing-read args
                       nil t nil 'file-name-history nil nil
                       nil              ; Additional bindings
                       nil)

(icicle-define-command icicle-recent-file-other-window ; Command name
                       "Open a recently used file in another window." ; Doc string
                       find-file-other-window ; Function to perform the action
                       "Recent file: " (mapcar (lambda(x) (cons x 0)) file-name-history) ; completing-read args
                       nil t nil 'file-name-history nil nil
                       nil              ; Additional bindings
                       nil)

(icicle-define-command
 icicle-vardoc                          ; Command name
 "Choose a variable description.
Each candidate for completion is a variable name plus its
documentation.  They are separated by `icicle-list-join-string' (a 
newline, by default).  You can match an input regexp against the
variable name or the documentation or both.
For example:
 Use input \".*
toggle\" with \\<minibuffer-local-completion-map>`\\[icicle-apropos-complete]' to match all \
variables with
 \"toggle\" in their documentation (use `C-q C-j' to input the newline).
 Use input \"dired.*
list\" to match all variables with \"dired\" in their name and \"list\"
 in their documentation."               ; Doc string
 (lambda (entry) (with-output-to-temp-buffer "*Help*" (princ entry))) ; Action function
 "Choose variable description (`RET' when done): " ; completing-read args
 (let ((result nil))                    ; TABLE arg is an alist whose items are ((symb doc)).
   (mapatoms (lambda (symb)             ; That is, each completion candidate is a list of strings.
               (when (boundp symb)
                 (push (list (list (symbol-name symb)
                                   (documentation-property symb 'variable-documentation)))
                       result))))
   result)
 nil nil nil nil nil nil nil
 (message "Gathering variable descriptions..."))   ; First code

(icicle-define-command
 icicle-fundoc                          ; Command name
 "Choose a function description.
Each candidate for completion is a function name plus its
documentation.  They are separated by `icicle-list-join-string' (a 
newline, by default).  You can match an input regexp against the
function name or the documentation or both.  For example, use input
\"dired.*
.*switch\" with \\<minibuffer-local-completion-map>`\\[icicle-apropos-complete]' to match all \
functions with \"dired\" in their
name and \"switch\" in their documentation (use `C-q C-j' to input the
newline)."                              ; Doc string
 (lambda (entry) (with-output-to-temp-buffer "*Help*" (princ entry))) ; Action function
 "Choose function description (`RET' when done): " ; completing-read args
 (let ((result nil))                    ; TABLE arg is an alist whose items are ((symb doc)).
   (mapatoms (lambda (symb)             ; That is, each completion candidate is a list of strings.
               (when (fboundp symb)
                 (push (list (list (symbol-name symb) (documentation symb))) result))))
   result)
 nil nil nil nil nil nil nil
 (message "Gathering function descriptions..."))   ; First code

;; $$$ Extend to faces too?  Other objects too?
(icicle-define-command
 icicle-doc                             ; Command name
 "Choose documentation for a function or variable.
Each candidate for completion is the description of a function or
variable.  Displays the documentation and returns the function or
variable (a symbol)."                   ; Doc string
 (lambda (entry)                        ; Action function: display the doc.
   (let ((fn-or-var (cdr (assoc entry result))))
     (when (boundp fn-or-var) (describe-variable fn-or-var))
     (when (fboundp fn-or-var) (describe-function fn-or-var))
     fn-or-var))                        ; Return the function or variable (symbol).
 "Choose documentation (`RET' when done): " ; completing-read args
 (let (doc)
   (mapatoms (lambda (symb)             ; TABLE arg is an alist whose items are (doc . symb).
               (when (fboundp symb)     ; That is, the completions are the doc strings.
                 (setq doc (documentation symb))
                 (when (and (stringp doc) (> (length doc) 0)) (push (cons doc symb) result)))
               (when (boundp symb)
                 (setq doc (documentation-property symb 'variable-documentation))
                 (when (and (stringp doc) (> (length doc) 0))
                   (push (cons (documentation-property symb 'variable-documentation) symb)
                         result)))))
   result)
 nil nil nil nil nil nil
 ((result nil))
 (message "Gathering documentation..."))   ; First code

;;;###autoload
(defun icicle-isearch-complete ()
  "Complete the search string using candidates from the search ring."
  (interactive)
  (isearch-done 'nopush)
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
         (completion (completing-read "Complete search string: "
                                      (mapcar #'list (icicle-remove-duplicates ring))
                                      nil nil isearch-string
				      (if isearch-regexp 'regexp-search-ring 'search-ring))))
    (setq isearch-string completion)
    (icicle-isearch-resume isearch-string isearch-regexp isearch-word isearch-forward
                           (mapconcat 'isearch-text-char-description isearch-string "")
                           nil)))

(defun icicle-isearch-resume (search regexp word forward message case-fold)
  "Resume an incremental search.
SEARCH is the string or regexp searched for.
REGEXP non-nil means the resumed search was a regexp search.
WORD non-nil means resume a word search.
FORWARD non-nil means resume a forward search.
MESSAGE is the echo-area message recorded for the search resumed.
CASE-FOLD non-nil means the search was case-insensitive."
  (isearch-mode forward regexp nil nil word)
  (setq isearch-string search
	isearch-message message
	isearch-case-fold-search case-fold)
  (isearch-search-and-update))

(defun icicle-bind-isearch-keys ()
  "Bind `S-TAB' in Isearch maps.  Use in `isearch-mode-hook'."
  (define-key isearch-mode-map [(shift tab)] 'icicle-isearch-complete)
  (define-key minibuffer-local-isearch-map [(shift tab)] 'isearch-complete-edit))

;;;###autoload
(defun icicle-search (beg end &optional sit-for-period)
  "Search for a regexp match, with completion and completion cycling.
The active region is searched, or, if none, the buffer is searched.
You are prompted for a regexp.  Matches become available as completion
candidates.  You can, for instance, use apropos completion to filter
the candidates using a different regexp.

`\\<minibuffer-local-completion-map>\\[icicle-next-apropos-candidate]', \
`\\[icicle-previous-apropos-candidate]', `\\[icicle-next-prefix-candidate]', and \
`\\[icicle-previous-prefix-candidate]' can be used to choose a match.
`\\[icicle-candidate-action]' will move the cursor to the match occurrence.

To see each occurrence in the original buffer as you cycle among
candidates, you can use `\\[icicle-next-apropos-candidate-action]', \
`\\[icicle-previous-apropos-candidate-action]', `\\[icicle-next-prefix-candidate-action]', and \
`\\[icicle-previous-prefix-candidate-action]'.

Note: This command temporarily overrides user option
`icicle-completion-nospace-flag', binding it to nil.  This means that
candidates with initial spaces can be matched.

Optional argument SIT-FOR-PERIOD is the number of seconds to pause to
show cursor at match location.  By default, it is 2 seconds."
  (interactive
   (if (or (null (mark)) (= (point) (mark)) (not mark-active))
       (list (point-min) (point-max))
     (if (< (point) (mark))
         (list (point) (mark))
       (list (mark) (point)))))
  (setq sit-for-period (or sit-for-period 2))
  (let ((icicle-sort-function nil)
        (icicle-completion-nospace-flag nil)
        (orig-point (point))
        (orig-window (selected-window))
        (regexp (read-from-minibuffer "Search for (regexp): " nil nil nil 'regexp-history))
        (search-candidates nil))
    (save-excursion
      (goto-char beg)                     
      (while (and beg (< beg end))
        (setq beg (re-search-forward regexp end t))
        (when beg
          (push (cons (buffer-substring-no-properties (match-beginning 0) (match-end 0))
                      beg)              ; (strg . pos)
                search-candidates))))
    (setq search-candidates (nreverse search-candidates))
    (let ((icicle-candidate-action-fn
           (lambda (string)
             (condition-case nil
                 (progn
                   ;; Highlight current candidate in *Completions*.
                   (let ((compl-win (get-buffer-window "*Completions*" t))
                         curr-candidate-pos)
                     (when compl-win
                       (save-window-excursion
                         (select-window compl-win)
                         (let ((case-fold-search completion-ignore-case))
                           (goto-char (point-min))
                           ;;(forward-line 3)
                           (icicle-move-to-next-completion icicle-candidate-nb t)
                           (set-buffer-modified-p nil)
                           (setq curr-candidate-pos (point))))
                       (set-window-point compl-win curr-candidate-pos)))
                   ;; Move cursor to match in original buffer.
                   (let ((position
                          (and icicle-candidate-nb
                               (cdr (elt (icicle-filter-alist icicle-completion-candidates
                                                              search-candidates)
                                         icicle-candidate-nb)))))
                     (unless position (error "No such occurrence"))
                     (save-selected-window
                       (select-window orig-window)
                       (goto-char position)
                       (sit-for sit-for-period)
                       (run-hooks 'icicle-search-hook))
                     t))                ; Return non-nil for success.
               (error nil)))))          ; Return nil for failure.
      (condition-case failure
          (let* ((string (completing-read "Choose an occurrence: " search-candidates nil t))
                 (position (cdr (elt (icicle-filter-alist icicle-completion-candidates
                                                          search-candidates)
                                     icicle-candidate-nb))))
            (unless position (error "No such occurrence"))
            (goto-char position)
            (run-hooks 'icicle-search-hook))
        (quit (goto-char orig-point))
        (error (goto-char orig-point) (error (error-message-string failure)))))))

(defun icicle-filter-alist (filter-keys alist)
  "Filter ALIST, keeping items whose cars match FILTER-KEYS, in order."
  (let ((copy-alist alist)
        (result nil)
        key+val)
    (dolist (cand filter-keys)
      (when (setq key+val (assoc cand copy-alist))
        (push key+val result)
        (setq copy-alist (cdr (member key+val copy-alist)))))
    (nreverse result)))

;;;###autoload
(defun icicle-compilation-search (beg end)
  "Like `icicle-search', but shows the matching compilation-buffer
hit.  Use this in a compilation buffer, such as `*grep*', searching
for a regexp as with `icicle-search'.  Use `C-RET' or `C-mouse-2' to
show the target-buffer hit corresponding to the current completion
candidate.  Use `C-next', `C-prior', `C-down', or `C-up' to cycle
among the target-buffer hits.

As for `icicle-search', you can further narrow the match candidates
by typing a second regexp to search for among the first matches.

Altogether, using this with `grep' gives you two or three levels of
regexp searching: 1) the `grep' regexp, 2) the major `icicle-search'
regexp, and optionally 3) the refining `icicle-search' regexp."
  (interactive
   (if (or (null (mark)) (= (point) (mark)) (not mark-active))
       (list (point-min) (point-max))
     (if (< (point) (mark))
         (list (point) (mark))
       (list (mark) (point)))))
  (unless
      (condition-case nil
          (eq (current-buffer) (compilation-find-buffer))
        (error nil))
    (error "Current buffer must be a compilation buffer"))
  (let ((orig-search-hook icicle-search-hook))
    (add-hook 'icicle-search-hook 'compile-goto-error)
    (icicle-search beg end 0)
    (remove-hook 'icicle-search-hook 'compile-goto-error)))


(defalias 'toggle-icicle-sorting 'icicle-toggle-sorting)

;;;###autoload
(defun icicle-toggle-sorting ()
  "Toggle sorting of minibuffer completion candidates.
When sorting is active, comparison is done by `icicle-sort-function'."
  (interactive)
  (if icicle-sort-function
      (setq icicle-last-sort-function icicle-sort-function ; Save it, for restoring.
            icicle-sort-function      nil)
    (setq icicle-sort-function icicle-last-sort-function)) ; Restore it.
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-sort-function
                                      "Completion-candidate sorting is now ON"
                                    "Completion-candidate sorting is now OFF")))


(defalias 'toggle-icicle-ignored-extensions 'icicle-toggle-ignored-extensions)

;;;###autoload
(defun icicle-toggle-ignored-extensions ()
  "Toggle respect of `completion-ignored-extensions'."
  (interactive)
  (if (consp completion-ignored-extensions)
      (setq icicle-saved-ignored-extensions  completion-ignored-extensions ; Save it.
            completion-ignored-extensions    nil
            icicle-ignored-extensions-regexp nil)
    (setq completion-ignored-extensions icicle-saved-ignored-extensions) ; Restore it.
    (setq icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
          (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|") "\\)\\'")))
  ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
  ;; `completion-ignored-extensions' changes.
  (setq icicle-ignored-extensions completion-ignored-extensions)
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if completion-ignored-extensions
                                      "Ignoring selected file extensions is now ON"
                                    "Ignoring selected file extensions is now OFF")))

;;;###autoload
(defun icicle-insert-string-near-point ()
  "Insert the next kind of string near the cursor.
This should be called from the minibuffer.  Each time it is called
consecutively, the next function in `icicle-thing-at-point-functions'
is used to retrieve a string near the cursor, and that string
replaces the content of the minibuffer."
  (interactive)
  (when icicle-thing-at-point-functions
    (setq icicle-thing-at-pt-fns-pointer
          (if (eq last-command this-command)
              (mod (1+ icicle-thing-at-pt-fns-pointer)
                   (length icicle-thing-at-point-functions))
            0))
    (let ((string nil))
      (save-excursion
        (set-buffer (cadr (buffer-list)))
        (setq string
              (funcall (nth icicle-thing-at-pt-fns-pointer icicle-thing-at-point-functions))))
      (when (and string (not (string= "" string)))
        (icicle-erase-minibuffer)
        (insert string)))))


;;;###autoload
(defun icicle-keep-only-past-inputs ()
  "Reduce completion candidates to those that have been used previously.
This filters the set of current completion candidates, keeping those
that been used before."
  (interactive)
  (if (null icicle-completion-candidates)
      (minibuffer-message "  [No completion candidates to filter]")
    (when (and (symbolp minibuffer-history-variable)
               (consp (symbol-value minibuffer-history-variable)))
      (setq icicle-completion-candidates
            (icicle-delete-if-not
             (lambda (cand)
               (member (if (icicle-file-name-input-p)
                           (expand-file-name cand (icicle-file-name-directory-w-default cand))
                         cand)
                       (symbol-value minibuffer-history-variable)))
             icicle-completion-candidates))
      (cond ((null icicle-completion-candidates)
             (save-selected-window (icicle-delete-windows-on "*Completions*"))
             (minibuffer-message "  [None of the completions has been used before]"))
            (t
             (setq icicle-current-input
                   (if (memq last-command
                             '(icicle-next-apropos-candidate icicle-previous-apropos-candidate
                               icicle-next-prefix-candidate  icicle-previous-prefix-candidate))
                       icicle-last-input
                     (icicle-minibuffer-contents)))
             (icicle-retrieve-last-input)
             (cond ((null icicle-completion-candidates)
                    (setq icicle-nb-of-other-cycle-candidates 0)
                    (save-selected-window (icicle-delete-windows-on "*Completions*"))
                    (minibuffer-message "  [No matching history element]"))
                   ((null (cdr icicle-completion-candidates)) ; Single candidate. Update minibuffer.
                    (setq icicle-nb-of-other-cycle-candidates 0)
                    (icicle-erase-minibuffer)
                    (insert (setq icicle-last-completion-candidate
                                  (if (and (icicle-file-name-input-p) insert-default-directory)
                                      (expand-file-name (car icicle-completion-candidates)
                                                        (icicle-file-name-directory-w-default
                                                         (car icicle-completion-candidates)))
                                    (car icicle-completion-candidates))))
                    (save-selected-window (icicle-delete-windows-on "*Completions*"))
                    (icicle-highlight-complete-input)
                    (minibuffer-message (format "  [One matching history element]")))
                   (t
                    (when (member icicle-current-input icicle-completion-candidates)
                      (icicle-highlight-complete-input))
                    (if (get-buffer-window "*Completions*" 0)
                        (if (and (eq icicle-last-completion-command 'icicle-keep-only-past-inputs)
                                 (memq last-command
                                       '(icicle-keep-only-past-inputs handle-switch-frame)))
                            ;; Second `S-TAB' in a row.  Scroll window around.
                            (save-selected-window
                              (select-window (get-buffer-window "*Completions*" 0))
                              (condition-case nil
                                  (scroll-up nil)
                                (end-of-buffer (goto-char (point-min)) (forward-line 3))))
                          ;; Did something else (e.g. changed input).  Update the display.
                          (icicle-display-candidates-in-Completions icicle-current-input))
                      ;; No window yet.  Show window.
                      (icicle-display-candidates-in-Completions icicle-current-input))
                    (save-window-excursion
                      (select-window (active-minibuffer-window))
                      (minibuffer-message "  [filtered to (matching) historical candidates]"))))
             (setq icicle-last-completion-command this-command))))
    icicle-completion-candidates))


;;;###autoload
(defun icicle-history ()
  "Access the appropriate history list using completion or cycling.
The current minibuffer input is interpreted as a regexp and matched
against items in the history list in use for the current command.

Note:

If the required input is a file or directory name, then the entire
minibuffer input is what is matched against the history list.  The
reason for this is that file names in the history list are absolute.
This is unlike the case for normal file-name completion, which assumes
the default directory.

Keep this in mind for apropos (regexp) completion; it means that to
match a file-name using a substring you must, in the minibuffer,
either not specify a directory or explicitly use \".*\" before the
file-name substring.

For example, `/foo/bar/lph' will not apropos-match the previously
input file name `/foo/bar/alphabet-soup.el'; you should use either
`/foo/bar/.*lph' or `lph' (no directory).

This also represents a difference in behavior compared to the similar
command `icicle-keep-only-past-inputs' (\\<minibuffer-local-completion-map>\
\\[icicle-keep-only-past-inputs] in the minibuffer).
That command simply filters the current set of completion candidates,
which in the case of file-name completion is a set of relative file
names."
  (interactive)
  (when (icicle-file-name-input-p) (setq minibuffer-completion-predicate nil))
  (when (arrayp minibuffer-completion-table)
    (setq minibuffer-completion-predicate
          `(lambda (elt) (funcall ',minibuffer-completion-predicate (intern (car elt))))))
  (when (and (symbolp minibuffer-history-variable)
             (consp (symbol-value minibuffer-history-variable)))
    (setq minibuffer-completion-table
          (mapcar #'list (icicle-remove-duplicates (symbol-value minibuffer-history-variable)))))
  (setq icicle-last-completion-command "") ; Force redisplay of *Completions* even if displayed.
  (setq icicle-current-input
        (if (memq last-command
                  '(icicle-next-apropos-candidate icicle-previous-apropos-candidate
						  icicle-next-prefix-candidate  icicle-previous-prefix-candidate))
            icicle-last-input
          (icicle-minibuffer-contents)))
  (icicle-retrieve-last-input)
  (icicle-apropos-complete))

;; Borrowed from `ps-print.el'
(defun icicle-remove-duplicates (list)
  "Copy of LIST with duplicate elements removed.  Tested with `equal'."
  (let ((tail list)
        new)
    (while tail
      (unless (member (car tail) new) (push (car tail) new))
      (pop tail))
    (nreverse new)))

(defun icicle-delete-windows-on (buffer) ; From `remove-windows-on' in `frame-cmds.el'.
  "Delete all windows showing BUFFER."
  (interactive
   (list (read-buffer "Remove all windows showing buffer: " (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))     ; Convert to buffer.
  (when buffer                          ; Do nothing if null BUFFER.
    ;; Avoid error message "Attempt to delete minibuffer or sole ordinary window".
    (let ((frames (icicle-frames-on buffer t)))
      (unless (and frames (null (cdr frames)) ; One frame shows *Completions*.
                   (cdr (assoc 'minibuffer (frame-parameters (car frames)))) ; Has a minibuffer.
                   (save-window-excursion
                     (select-frame (car frames))
                     (one-window-p t 'selected-frame))) ; Only one window.
        (dolist (fr frames)
          (delete-window (get-buffer-window buffer t)))))))




;;; Noninteractive Functions -------------------------------


;;; Redefined standard functions............................


;;; REPLACE ORIGINAL `choose-completion-string' in `simple.el', 
;;; saving it for restoration when you toggle `icicle-mode'.
;;; 
;;; Don't exit minibuffer if this is just a `lisp-complete-symbol' completion.
;;; Free variable `completion-reference-buffer' is defined in `simple.el'.
;;;
(or (fboundp 'old-choose-completion-string)
(fset 'old-choose-completion-string (symbol-function 'choose-completion-string)))

;;;###autoload
(defun icicle-choose-completion-string (choice &optional buffer base-size)
  "Switch to BUFFER and insert the completion choice CHOICE.
BASE-SIZE, if non-nil, says how many characters of BUFFER's text
to keep.  If it is nil, we call `choose-completion-delete-max-match'
to decide what to delete.
If BUFFER is the minibuffer, then exit the minibuffer, unless one of
the following is true:
   - it is reading a file name and CHOICE is a directory
   - `completion-no-auto-exit' is non-nil
   - this is just a `lisp-complete-symbol' completion."
  (let ((buffer (or buffer completion-reference-buffer))) ; In `simple.el'.
    ;; If BUFFER is a minibuffer, barf unless it's currently active.
    (when (and (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
	       (or (not (active-minibuffer-window))
		   (not (equal buffer (window-buffer (active-minibuffer-window))))))
      (error "Minibuffer is not active for completion"))
    ;; Insert the completion into the buffer where completion was requested.
    (set-buffer buffer)
    (if (or (eq minibuffer-completion-table 'read-file-name-internal)
	    (eq minibuffer-completion-table 'ffap-read-file-or-url-internal))
	(setq base-size (save-excursion
			  (goto-char (point-max))
			  (skip-chars-backward (format "^%c" directory-sep-char))
			  (- (point) (point-min))))
      (setq base-size 0))
    (if base-size
	(delete-region (+ base-size (point-min)) (point-max))
      (choose-completion-delete-max-match choice))
    (insert choice)
    (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
    ;; Update point in the window that BUFFER is showing in.
    (let ((window (get-buffer-window buffer t)))
      (set-window-point window (point)))
    ;; If completing for the minibuffer, exit it with this choice,
    ;; unless this was a `lisp-complete-symbol' completion.
    (and (not completion-no-auto-exit)
	 (equal buffer (window-buffer (minibuffer-window)))
	 minibuffer-completion-table
	 (not (eq 'lisp-complete-symbol icicle-cmd-calling-for-completion))
	 ;; If this is reading a file name, and the file name chosen
	 ;; is a directory, don't exit the minibuffer.
	 (if (and (eq minibuffer-completion-table 'read-file-name-internal)
		  (file-directory-p (buffer-string)))
	     (select-window (active-minibuffer-window))
	   (exit-minibuffer)))))


;;; REPLACE ORIGINAL `mouse-choose-completion' in `mouse.el', 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Return the number of the completion.
;;;
(or (fboundp 'old-mouse-choose-completion)
(fset 'old-mouse-choose-completion (symbol-function 'list-mode-item-mouse-selected)))

;;;###autoload
(defun icicle-mouse-choose-completion (event)
  "Click a completion candidate in buffer `*Completions*', to choose it.
Returns the number of the candidate - 0 for first, 1 for second, ..."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (let ((buffer (window-buffer))
        choice
	(pos (event-point event))
	base-size)
    (save-excursion
      (set-buffer (window-buffer (event-window event)))
      (if completion-reference-buffer
	  (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
	(goto-char pos)
	(let (beg 
	      end
	      (extent (extent-at (point) (current-buffer) 'list-mode-item)))
	  (if (and (not (eobp)) extent)
	      (setq beg (extent-start-position extent) 
		    end (extent-end-position extent))) 
	  (if (null beg)
	      (error "No completion here"))
	  (setq choice (buffer-substring beg end)))))
    (save-selected-window (icicle-delete-windows-on "*Completions*"))
    (setq icicle-candidate-nb
          (icicle-nb-of-candidate-in-Completions pos))
    (choose-completion-string choice buffer base-size)))
  
(defun icicle-nb-of-candidate-in-Completions (position)
  "Return number of completion candidate at POSITION in *Completions*."
  (let ((compl-buf (get-buffer "*Completions*")))
    (unless compl-buf (error "No *Completions* buffer"))
    (save-window-excursion
      (set-buffer compl-buf)
      (let ((cand-nb 0)
            last-pos)
        (goto-char position)
        (setq last-pos (point))
        (while (and (not (eq (point) 1)) (<= (point) last-pos))
          (icicle-move-to-next-completion -1 t)
          (setq cand-nb (1+ cand-nb))
          (setq last-pos (min last-pos (point))))
        (set-buffer-modified-p nil)
        (1- cand-nb)))))



;;; REPLACE ORIGINAL `completion-setup-function' in `simple.el', 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; 1. Put faces on inserted strings.  2. Help on help.
;;;
(or (fboundp 'old-completion-setup-function)
(fset 'old-completion-setup-function (symbol-function 'completion-setup-function)))

;;;###autoload
(defun icicle-completion-setup-function ()
  "Set up for completion.  This goes in `completion-setup-hook'
so it is called after completion-list buffer text is written.
Put faces on inserted string(s). Provide help on help."
  (save-excursion
    (let* ((mainbuf (current-buffer))
	   (instruction1 (if window-system         ; We have a mouse.
			     (substitute-command-keys
			      "Click \\<completion-list-mode-map>\
\\[mouse-choose-completion] on a completion to select it.  ")
			   (substitute-command-keys ; No mouse.
			    "In this buffer, type \\<completion-list-mode-map>\
\\[choose-completion] to select the completion near point.  ")))
	   (instruction2
	    (and icicle-mode
		 (substitute-command-keys
		  "(\\<minibuffer-local-completion-map>\\[icicle-completion-help]: \
help) "))))
      (set-buffer standard-output)
      (completion-list-mode)
      (make-local-variable 'completion-reference-buffer)
      (setq completion-reference-buffer mainbuf)
      (if (eq minibuffer-completion-table 'read-file-name-internal)
	  ;; For file name completion,
	  ;; use the number of chars before the start of the
	  ;; last file name component.
	  (setq completion-base-size (save-excursion
				       (set-buffer mainbuf)
				       (goto-char (point-max))
				       (skip-chars-backward (format "^%c" directory-sep-char))
				       (- (point) (point-min))))
	;; Otherwise, in minibuffer, the whole input is being completed.
	(save-match-data
	  (if (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name mainbuf))
	      (setq completion-base-size 0))))
      (goto-char (point-min))
      (put-text-property 0 (length instruction1) 'face 'icicle-Completions-instruction-1
			 instruction1)
      (when instruction2
	(put-text-property 0 (length instruction2) 'face 'icicle-Completions-instruction-2
			   instruction2))
      (insert (concat instruction1 instruction2 "\n\n"))
      (forward-line 1)
      (while (re-search-forward "[^ \t\n]+\\( [^ \t\n]+\\)*" nil t)
	(let ((beg (match-beginning 0))
	      (end (point)))
;;;Emacs20 (when completion-fixup-function (funcall completion-fixup-function))
	  (put-text-property beg (point) 'mouse-face 'highlight)
	  (goto-char end))))))



;;; REPLACE ORIGINAL `completing-read' (built-in function), 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Allows for completions that are lists of strings.
;;; Appends `icicle-prompt-suffix' if resulting prompt is not too long.
;;; Removes *Completions* window.
;;;
(or (fboundp 'old-completing-read)
(fset 'old-completing-read (symbol-function 'completing-read)))

;;;###autoload
(defun icicle-completing-read
  (prompt table &optional predicate require-match initial-input hist def inherit-input-method)
  "Read string in minibuffer, with completion and cycling of completions.
Type `\\[exit-minibuffer]' to end your input.

Prefix completion via \\<minibuffer-local-completion-map>\
`\\[icicle-prefix-word-complete]' (word) and `\\[icicle-prefix-complete]' (full).
Apropos (regexp) completion via `\\[icicle-apropos-complete]'.

Prefix cycling of candidate completions via `\\[icicle-previous-prefix-candidate]' and \
`\\[icicle-next-prefix-candidate]'.
Apropos cycling of candidate completions via `\\[icicle-previous-apropos-candidate]' and \
`\\[icicle-next-apropos-candidate]'.
Cycling of past minibuffer inputs via `\\[previous-history-element]' and \
`\\[next-history-element]'.
Searching through input history via `\\[previous-matching-history-element]' \
and `\\[next-matching-history-element]'.
Case is ignored if `completion-ignore-case' is non-nil.  For file-name
  completion, `read-file-name-completion-ignore-case' is used instead.
For file-name completion, cycling into subdirectories is determined by
  `icicle-cycle-into-subdirs-flag'.
Position of the cursor (point) and the mark during completion cycling
  is determined by `icicle-point-position-in-candidate' and
  `icicle-mark-position-in-candidate', respectively.
Highlighting of the completion root during cycling is determined by
  `icicle-root-highlight-minibuffer' and
  `icicle-root-highlight-Completions'.

Use `\\[icicle-completion-help]' during completion for more information on completion and key
bindings in Icicle mode.

Args: PROMPT, TABLE, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST:

PROMPT is a string to prompt with; normally ends in a colon and space.

TABLE is an alist whose elements' cars are strings, or an obarray.

PREDICATE limits completion to a subset of TABLE.
See `try-completion' and `all-completions' for more details on
completion, TABLE, PREDICATE.

If REQUIRE-MATCH is non-nil, you are not allowed to exit unless the
input is (or completes to) an element of TABLE or is null.  If it is
also not `t', `\\[exit-minibuffer]' doesn't exit if it effects non-null
completion.  If the input is null, `completing-read' returns an empty
string, regardless of the value of REQUIRE-MATCH.

If option `icicle-require-match-flag' is non-nil, it overrides the
value of REQUIRE-MATCH.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
If it is (STRING . POSITION), the initial input is STRING, but point
is placed POSITION characters into the string.

HIST, if non-nil, specifies a history list, and optionally the initial
position in the list.  It can be a symbol, which is the history list
variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  In
that case, HISTVAR is the history list variable to use, and HISTPOS is
the initial position (the position in the list which INITIAL-INPUT
corresponds to).  Positions are counted starting from 1 at the
beginning of the list.

DEF, if non-nil, is the default value.

Non-nil `icicle-init-value-flag' means that when DEF is non-nil and
INITIAL-INPUT is nil or \"\", DEF is inserted in the minibuffer as the
INITIAL-INPUT.  The particular non-nil value determines whether or not
the value is preselected and, if preselected, where the cursor is left
\(at the beginning or end of the value).

If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits the
current input method and the setting of `enable-multibyte-characters'.

Completion ignores case when`completion-ignore-case' is non-nil."
  (unless initial-input (setq initial-input ""))
  (if (consp initial-input)
      (setq icicle-initial-value (car initial-input))
    (setq initial-input        (format "%s" initial-input) ; Convert symbol to string
          icicle-initial-value (or initial-input "")))
  (setq icicle-nb-of-other-cycle-candidates 0)

  ;; Maybe use DEF for INITIAL-INPUT also.
  (when (and icicle-init-value-flag def (stringp initial-input) (string= "" initial-input))
    (setq initial-input def))

  ;; Override REQUIRE-MATCH as needed.
  (setq require-match (case icicle-require-match-flag
                        ((nil) require-match)
                        (no-match-required nil)
                        (partial-match-ok t)
                        (full-match-required 'full-match-required)))

  (let ((minibuffer-completion-table table)
        result)

    ;; Extension: candidate is a list of strings.  Used for multi-completion.
    (when (and (consp table) (consp (car table)) (consp (caar table)))
      (setq minibuffer-completion-table
            (setq table
                  (mapcar
                   (lambda (entry) 
                     (cons (concat (mapconcat #'identity (car entry) icicle-list-join-string)
                                   icicle-list-join-string)
                           (cdr entry)))
                   table))))

    ;; Append suffix if prompt is not too long.
    ;; Use face on suffix if (boundp 'minibuffer-prompt-properties).
    (cond ((or (not icicle-mode)
	       (> (length icicle-initial-value)
		  (- (window-width (minibuffer-window)) (length prompt))))
	   (setq icicle-icicle-completing-p t)
	   (setq icicle-prompt prompt)	; No room to add suffix.
	   (setq result (catch 'icicle-read-top
			  (old-completing-read icicle-prompt table predicate require-match
					       initial-input hist def))))
	  (t				; Append suffix to prompt.
	   (setq icicle-icicle-completing-p t)
	   (setq icicle-prompt
		 (if (fboundp 'propertize)
		     (concat (propertize prompt 'face 'blue)
			     ;;(propertize icicle-prompt-suffix 'face 'icicle-prompt-suffix)
			     "")
		   (concat prompt icicle-prompt-suffix "  ")))
	   (let ((minibuffer-prompt-properties
		  (and (boundp 'minibuffer-prompt-properties) ; Emacs 21+ only
		       (icicle-remove-property 'face minibuffer-prompt-properties))))
	     (setq result
		   (catch 'icicle-read-top
		     (old-completing-read icicle-prompt table predicate require-match
					  initial-input hist def))))))
    ;; HACK.  Without this, when REQUIRE-MATCH is non-nil, *Completions* window
    ;; does not disappear.
    (when require-match (icicle-delete-windows-on "*Completions*"))
    (set-text-properties 0 (length result) nil result)
    result))



;;; REPLACE ORIGINAL `read-file-name' (built-in function), 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Lets minibuffer use the current buffer's binding for `ESC-TAB'.
;;; Appends `icicle-prompt-suffix' if resulting prompt is not too long.
;;; Removes *Completions* window.
;;;
(or (fboundp 'old-read-file-name)
(fset 'old-read-file-name (symbol-function 'read-file-name)))

;;;###autoload
(defun icicle-read-file-name (prompt &optional dir default-filename
                              require-match initial-input predicate)
  "Read file name, prompting with prompt and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default the name to DEFAULT-FILENAME if user exits the minibuffer with
the same non-empty string that was inserted by this function.
 (If DEFAULT-FILENAME is omitted, the visited file name is used,
  but if INITIAL-INPUT is specified, that combined with DIR is used.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg REQUIRE-MATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL-INPUT specifies text to start with.
If optional sixth arg PREDICATE is non-nil, possible completions and
 the resulting file name must satisfy `(funcall predicate NAME)'.
 This argument is only available starting with Emacs 21.
DIR should be an absolute directory name.  It defaults to the value of
`default-directory'.

Non-nil `icicle-init-value-flag' means that when DEFAULT-FILENAME is
non-nil and INITIAL-INPUT is nil or \"\", DEFAULT-FILENAME is inserted
in the minibuffer as the INITIAL-INPUT.  The particular non-nil value
determines whether or not the value is preselected and, if
preselected, where the cursor is left \(at the beginning or end of the
value).

If option `icicle-require-match-flag' is non-nil, it overrides the
value of REQUIRE-MATCH.

If this command was invoked with the mouse, use a file dialog box if
`use-dialog-box' is non-nil, and the window system or X toolkit in use
provides a file dialog box.

Removes *Completions* window when done.

See also `read-file-name-completion-ignore-case'
and `read-file-name-function'."
  (setq icicle-initial-value                  (or initial-input "")
        icicle-nb-of-other-cycle-candidates 0)
  (icicle-fix-default-directory)        ; Make sure there are no backslashes in it.

  ;; Maybe use DEFAULT-FILENAME for INITIAL-INPUT also, after removing the directory part.
  ;; Note that if DEFAULT-FILENAME is null, then we let INITIAL-INPUT remain null too.
  (when (and icicle-init-value-flag default-filename (string= "" icicle-initial-value))
    (setq initial-input (file-name-nondirectory default-filename)))

  ;; Override REQUIRE-MATCH as needed.
  (setq require-match (case icicle-require-match-flag
                        ((nil) require-match)
                        (no-match-required nil)
                        (partial-match-ok t)
                        (full-match-required 'full-match-required)))
  (let (result)
    ;; Append suffix if prompt is not too long.
    ;; Use face on suffix if (boundp 'minibuffer-prompt-properties).
    (cond ((> (length initial-input)
              (- (window-width (minibuffer-window)) (length prompt)))
	   (setq icicle-icicle-completing-p t)
           (setq icicle-prompt prompt)  ; No room to add suffix.
           (condition-case nil          ; If Emacs 22+, use predicate arg.
               (setq result
                     (catch 'icicle-read-top
		       (old-read-file-name icicle-prompt dir default-filename
					   require-match initial-input predicate)))
             (wrong-number-of-arguments
              (setq result
                    (catch 'icicle-read-top
		      (old-read-file-name icicle-prompt dir default-filename
					  require-match initial-input))))))
          (t                            ; Append suffix to prompt.
	   (setq icicle-icicle-completing-p t)
           (setq icicle-prompt
                 (if (fboundp 'propertize)
                     (concat (propertize prompt 'face 'blue)
                             ;;(propertize icicle-prompt-suffix 'face 'icicle-prompt-suffix)
                             "")
                   (concat prompt icicle-prompt-suffix "  ")))
           (let ((minibuffer-prompt-properties ; If Emacs 22+, use pred and suffix face.
                  (and (boundp 'minibuffer-prompt-properties)
                       (icicle-remove-property 'face minibuffer-prompt-properties))))
             (condition-case nil
                 (setq result
                       (catch 'icicle-read-top
			 (old-read-file-name icicle-prompt dir default-filename
					     require-match initial-input predicate)))
               (wrong-number-of-arguments
                (setq result
                      (catch 'icicle-read-top
			(old-read-file-name icicle-prompt dir default-filename
					    require-match initial-input))))))))
    ;; HACK.  Without this, when REQUIRE-MATCH is non-nil, *Completions* window
    ;; does not disappear.
    (when require-match (icicle-delete-windows-on "*Completions*"))
    result))

(defun icicle-fix-default-directory ()
  "Convert backslashes in `default-directory' to slashes."
;; This is a hack.  If you do `C-x 4 f' from a standalone minibuffer
;; frame, `default-directory' on MS Windows has this form:
;; `C:\some-dir/'.  There is a backslash character in the string.  This
;; is not a problem for standard Emacs, but it is a problem for Icicles,
;; because we interpret backslashes using regexp syntax - they are not
;; file separators for Icicles.  So, we call `substitute-in-file-name' to
;; change all backslashes in `default-directory' to slashes.  This
;; shouldn't hurt, because `default-directory' is an absolute directory
;; name - it doesn't contain environment variables.  For example, we
;; convert `C:\some-dir/' to `c:/some-directory/'."
  (setq default-directory (substitute-in-file-name default-directory)))

(defun icicle-remove-property (prop plist)
  "Remove propery PROP from property-list PLIST, non-destructively."
  (let ((cpy plist)
        (result nil))
    (while cpy
      (unless (eq prop (car cpy)) (setq result `(,(cadr cpy) ,(car cpy) ,@result)))
      (setq cpy (cddr cpy)))
    (nreverse result)))



;;; REPLACE ORIGINAL `read-from-minibuffer' (built-in function), 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Respect `icicle-init-value-flag'.
;;;
(or (fboundp 'old-read-from-minibuffer)
(fset 'old-read-from-minibuffer (symbol-function 'read-from-minibuffer)))

;;;###autoload
(defun icicle-read-from-minibuffer (prompt &optional initial-contents keymap read hist
                                    abbrev-table default-value)
  "Read a string from the minibuffer, prompting with string PROMPT.
The optional second arg INITIAL-CONTENTS is an alternative to
  DEFAULT-VALUE.  Vanilla Emacs considers it to be obsolete, but
  Icicles does not.  It is discussed in more detail below.
Third arg KEYMAP is a keymap to use while reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a Lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HIST, if non-nil, specifies a history list and optionally
  the initial position in the list.  It can be a symbol, which is the
  history list variable to use, or it can be a cons cell
  (HISTVAR . HISTPOS).  In that case, HISTVAR is the history list variable
  to use, and HISTPOS is the initial position for use by the minibuffer
  history commands.  For consistency, you should also specify that
  element of the history as the value of INITIAL-CONTENTS.  Positions
  are counted starting from 1 at the beginning of the list.
Sixth arg DEFAULT-VALUE is the default value.  If non-nil, it is available
  for history commands; but, unless READ is non-nil, `read-from-minibuffer'
  does NOT return DEFAULT-VALUE if the user enters empty input!  It returns
  the empty string.
Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'.
Eighth arg KEEP-ALL, if non-nil, says to put all inputs in the history list,
 even empty or duplicate inputs.  This is available starting with Emacs 22.
If the variable `minibuffer-allow-text-properties' is non-nil,
 then the string which is returned includes whatever text properties
 were present in the minibuffer.  Otherwise the value has no text properties.

Non-nil `icicle-init-value-flag' means that when DEFAULT-VALUE is
non-nil and INITIAL-CONTENTS is nil or \"\", DEFAULT-VALUE is inserted
in the minibuffer as the INITIAL-CONTENTS.  The particular non-nil
value determines whether or not the value is preselected and, if
preselected, where the cursor is left \(at the beginning or end of the
value).

The remainder of this documentation string describes the
INITIAL-CONTENTS argument in more detail.  If non-nil,
INITIAL-CONTENTS is a string to be inserted into the minibuffer before
reading input.  Normally, point is put at the end of that string.
However, if INITIAL-CONTENTS is (STRING . POSITION), the initial input
is STRING, but point is placed at _one-indexed_ position POSITION in
the minibuffer.  Any integer value less than or equal to one puts
point at the beginning of the string.  *Note* that this behavior
differs from the way such arguments are used in `completing-read' and
some related functions, which use zero-indexing for POSITION."
  (unless initial-contents (setq initial-contents "")) 
  ;; Maybe use DEFAULT-VALUE for INITIAL-CONTENTS also.
  (when (and icicle-init-value-flag default-value (stringp initial-contents)
             (string= "" initial-contents))
    (setq initial-contents default-value))
  (if (< emacs-major-version 22)
      (old-read-from-minibuffer prompt initial-contents keymap read hist
                                abbrev-table default-value)
    (old-read-from-minibuffer prompt initial-contents keymap read hist
                              abbrev-table default-value)))



;;; REPLACE ORIGINAL `read-string' (built-in function), 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Respect `icicle-init-value-flag'.
;;;
(or (fboundp 'old-read-string)
(fset 'old-read-string (symbol-function 'read-string)))

;;;###autoload
(defun icicle-read-string (prompt &optional initial-input history
                           default-value)
  "Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
  Vanilla Emacs considers it to be obsolete, but Icicles does not.  It
  behaves as in `read-from-minibuffer'.  See the documentation string
  of `read-from-minibuffer' for details.
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
  See `read-from-minibuffer' for details of HISTORY argument.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history commands, and as the value to return if the user enters
 the empty string.
Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of enable-multibyte-characters."
  (let ((value (read-from-minibuffer prompt initial-input nil nil
                                     history default-value)))
    (if (and default-value (equal value "")) default-value value)))



;;; REPLACE ORIGINAL `dabbrev-completion' defined in `dabbrev.el', 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects *Completions* window even if on another frame.
;;;
(or (fboundp 'old-dabbrev-completion)
(fset 'old-dabbrev-completion (symbol-function 'dabbrev-completion)))

;;;###autoload
(defun icicle-dabbrev-completion (&optional arg)
  "Completion on current word.
Like \\[dabbrev-expand], but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by
`dabbrev-friend-buffer-function', to find the completions.

If the prefix argument is 16 (which comes from `C-u C-u'), then it
searches *all* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already."
  (interactive "*P")
  (unless (featurep 'dabbrev)
    (require 'dabbrev)
    (icicle-mode 1))                    ; Redefine `dabbrev-completion' to Icicles version.
  (dabbrev--reset-global-variables)
  (let* ((dabbrev-check-other-buffers (and arg t))
         (dabbrev-check-all-buffers (and arg (= (prefix-numeric-value arg) 16)))
         (abbrev (dabbrev--abbrev-at-point))
         (ignore-case-p (and (if (eq dabbrev-case-fold-search 'case-fold-search)
                                 case-fold-search
                               dabbrev-case-fold-search)
                             (or (not dabbrev-upcase-means-case-search)
                                 (string= abbrev (downcase abbrev)))))
         (my-obarray dabbrev--last-obarray)
         init)
    ;; If new abbreviation to expand, then expand it.
    (save-excursion
      (unless (and (null arg)
                   my-obarray
                   (or (eq dabbrev--last-completion-buffer (current-buffer))
                       (and (window-minibuffer-p (selected-window))
                            (eq dabbrev--last-completion-buffer
                                (dabbrev--minibuffer-origin))))
                   dabbrev--last-abbreviation
                   (>= (length abbrev) (length dabbrev--last-abbreviation))
                   (string= dabbrev--last-abbreviation
                            (substring abbrev 0 (length dabbrev--last-abbreviation)))
                   (setq init (try-completion abbrev my-obarray)))
        (setq dabbrev--last-abbreviation abbrev)
        (let ((completion-list (dabbrev--find-all-expansions abbrev ignore-case-p))
              (completion-ignore-case ignore-case-p))
          ;; Make an obarray with all expansions
          (setq my-obarray (make-vector (length completion-list) 0))
          (unless (> (length my-obarray) 0)
            (error "No dynamic expansion for \"%s\" found%s" abbrev
                   (if dabbrev--check-other-buffers "" " in this-buffer")))
          (dolist (string completion-list)
            (cond ((or (not ignore-case-p) (not dabbrev-case-replace))
                   (intern string my-obarray))
                  ((string= abbrev (upcase abbrev))
                   (intern (upcase string) my-obarray))
                  ((string= (substring abbrev 0 1) (upcase (substring abbrev 0 1)))
                   (intern (capitalize string) my-obarray))
                  (t (intern (downcase string) my-obarray))))             
          (setq dabbrev--last-obarray my-obarray)
          (setq dabbrev--last-completion-buffer (current-buffer))
          ;; Find the longest common string.
          (setq init (try-completion abbrev my-obarray)))))
    ;; Let the user choose between the expansions
    (unless (stringp init) (setq init abbrev))
    (cond
      ;; Complete text up through the common root.
      ((and (not (string-equal init ""))
            (not (string-equal (downcase init) (downcase abbrev))))
       (if (> (length (all-completions init my-obarray)) 1)
           (message "Use `%s' again to complete further"
                    (key-description (this-command-keys)))
         (message "Completed (no other completions)"))
       (if (< emacs-major-version 21)
           (dabbrev--substitute-expansion nil abbrev init)
         (dabbrev--substitute-expansion nil abbrev init nil))
       (when (window-minibuffer-p (selected-window)) (message nil))) ; $$$ NEEDED?
      (t
       ;; String is a common root already.  Use Icicles completion.
       (message "Making completion list...")
       (search-backward abbrev)
       (replace-match "")
       (condition-case nil
           (let* ((icicle-show-Completions-initially-flag t)
                  (icicle-incremental-completion-p 'display)
                  (minibuffer-completion-table my-obarray)
                  (choice (completing-read "Complete: "
                                           my-obarray nil t init nil init)))
             (when choice (insert choice)))
         (quit (insert abbrev)))))))



;;; REPLACE ORIGINAL `lisp-complete-symbol' defined in `lisp.el', 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects *Completions* window even if on another frame.
;;;
(or (fboundp 'old-lisp-complete-symbol)
(fset 'old-lisp-complete-symbol (symbol-function 'lisp-complete-symbol)))

;;;###autoload
(defun icicle-lisp-complete-symbol ()
  "Complete the Lisp symbol preceding point against known Lisp symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

The context determines which symbols are considered.
If the symbol starts just after an open-parenthesis, only symbols
with function definitions are considered.  Otherwise, all symbols with
function definitions, values or properties are considered."
  (interactive)
  (let* ((end (point))
	 (buffer-syntax (syntax-table))
	 (beg (unwind-protect
		  (save-excursion
		    (if emacs-lisp-mode-syntax-table
			(set-syntax-table emacs-lisp-mode-syntax-table))
		    (backward-sexp 1)
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point))
		(set-syntax-table buffer-syntax)))
	 (pattern (buffer-substring beg end))
	 (predicate
	  (if (eq (char-after (1- beg)) ?\()
	      'fboundp
	    #'(lambda (sym)
		(or (boundp sym) (fboundp sym)
		    (symbol-plist sym)))))
         (enable-recursive-minibuffers (active-minibuffer-window))
         (completion (completing-read "Complete Lisp symbol: "
                                      obarray predicate t pattern nil)))
    (delete-region beg end)
    (insert completion)))



;;; REPLACE ORIGINAL `repeat-complex-command' defined in `simple.el', 
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the command to repeat, letting you
;;; use `S-TAB' and `TAB' to see the history list and `C-,' to toggle
;;; sorting that display.
;;;
(or (fboundp 'old-repeat-complex-command)
(fset 'old-repeat-complex-command (symbol-function 'repeat-complex-command)))

;;;###autoload
(defun icicle-repeat-complex-command (arg)
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Lisp form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous command
it is added to the front of the command history.
You can use the minibuffer history commands \\<minibuffer-local-map>\\[next-history-element] and \
\\[previous-history-element]
to get different commands to edit and resubmit.

Use `S-TAB', [next], and [prior], to match regexp input - this gives
you the functionality of `repeat-matching-complex-command'."
  (interactive "p")
  (let ((elt (nth (1- arg) command-history))
        (icicle-sort-function nil)
        newcmd)
    (if elt
        (progn
          (setq newcmd
                (let ((print-level nil)
                      (minibuffer-history-position arg)
                      (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
                  (unwind-protect
                      (read (completing-read
                             "Redo: " (mapcar (lambda (entry) (list (prin1-to-string entry)))
                                              command-history)
                             nil nil (prin1-to-string elt) (cons 'command-history arg)
                             (prin1-to-string elt)))

                    ;; If command was added to command-history as a
                    ;; string, get rid of that.  We want only
                    ;; evaluable expressions there.
                    (if (stringp (car command-history))
                        (setq command-history (cdr command-history))))))

          ;; If command to be redone does not match front of history,
          ;; add it to the history.
          (or (equal newcmd (car command-history))
              (setq command-history (cons newcmd command-history)))
          (eval newcmd))
      (if command-history
          (error "Argument %d is beyond length of command history" arg)
        (error "There are no previous complex commands to repeat")))))

;;; Icicle functions - completion display (not cycling).....

(defun icicle-display-candidates-in-Completions (root &optional reverse-p)
  "Refresh *Completions*, updating it to reflect the current candidates.
ROOT is the root string that the candidates are completions of.
REVERSE-P non-nil means display the candidates in reverse order.
If `completion-auto-help' is nil, do nothing."
  ;; Pred is special if minibuffer-completion-table is a function.
  (when (and (not (functionp minibuffer-completion-table))
             (functionp minibuffer-completion-predicate))
    (setq icicle-completion-candidates
          (icicle-delete-if-not
           (lambda (cand)
             (funcall minibuffer-completion-predicate
                      (if (arrayp minibuffer-completion-table) (intern cand) (list cand))))
           icicle-completion-candidates)))
  (when (eq t icicle-incremental-completion-p) (setq icicle-incremental-completion-p 'always))
  (with-output-to-temp-buffer "*Completions*"
    ;; `condition-case' shouldn't be needed, but it prevents an "End of buffer"
    ;; message from `display-completion-list' on Emacs 22.
    (condition-case nil
        (display-completion-list ;;(if reverse-p
                                  ;;   (reverse icicle-completion-candidates)
                                   icicle-completion-candidates)
      ;;)
      (error nil)))

  (save-excursion
    (save-window-excursion
      (set-buffer (get-buffer "*Completions*"))
      (let ((buffer-read-only nil)
	    (eob (point-max))
	    (case-fold-search completion-ignore-case))
	(goto-char (point-min))
	(forward-line 2)
	(while (not (eobp))
	  (let ((beg (goto-char (next-single-property-change (point)
							     'mouse-face nil eob)))
		(end (goto-char (next-single-property-change (point)
							     'mouse-face nil eob))))
	    (goto-char beg)
	    ;; Highlight candidates that have been used previously.
            (when (and (symbolp minibuffer-history-variable)
                       (consp (symbol-value minibuffer-history-variable))
                       (member (icicle-current-completion-in-Completions)
                               (symbol-value minibuffer-history-variable)))
              (put-text-property
               (point) (next-single-property-change (point) 'mouse-face nil end)
               'face 'icicle-historical-candidate))

            ;; Highlight the root that was completed, inside each completion.
            (unless (string= "" root)
	      (save-excursion
		(save-restriction
		  (narrow-to-region beg end) ; Search within the completion candidate.
		  (when (re-search-forward (if (icicle-file-name-input-p)
					       (icicle-file-name-nondirectory root)
					     root)
					   nil t)
		    (put-text-property (match-beginning 0) (point)
				       'face 'icicle-root-highlight-Completions)))))
	    (goto-char end))))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)))
  (message nil))                        ; Clear out any "Looking for..."

(defun icicle-place-cursor (input)
  "Position point and mark with respect to the minibuffer candidate.
Positions are `icicle-point-position-in-candidate' and
`icicle-mark-position-in-candidate', respectively.
INPUT is the current user input, that is, the completion root."
  (let ((case-fold-search completion-ignore-case)
        input-start-position)
    (goto-char (icicle-minibuffer-prompt-end))
    (setq input-start-position (point))
    (when (and (icicle-file-name-input-p) insert-default-directory)
      (search-forward (icicle-file-name-directory-w-default input))
      (setq input-start-position (point))) ; Skip directory.
    ;; Locate completion root within current completion candidate.
    (when (or (memq icicle-point-position-in-candidate '(root-start root-end))
              (memq icicle-mark-position-in-candidate '(root-start root-end)))
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (point-max)) ; Search within the completion candidate.
          (re-search-forward (if (icicle-file-name-input-p)
                                 (icicle-file-name-nondirectory input)
                               input)
                             nil t))))
    ;; Position point.
    (case icicle-point-position-in-candidate
      (input-start (goto-char input-start-position))
      (input-end (goto-char (point-max)))
      (root-start (goto-char (match-beginning 0)))
      (root-end (goto-char (match-end 0))))
    ))
    ;; Position mark.
    ;;;(unless (eq icicle-point-position-in-candidate icicle-mark-position-in-candidate)
    ;;;  (push-mark (case icicle-mark-position-in-candidate
    ;;;               (input-start input-start-position)
    ;;;               (input-end (point-max))
    ;;;               (root-start (match-beginning 0))
    ;;;               (root-end (match-end 0)))
    ;;;             'nomsg
    ;;;             t))))
    ;;;(zmacs-activate-region)))

(defun icicle-minibuffer-prompt-end ()
  "Version of `minibuffer-prompt-end' that works for Emacs 20 and later."
  (if (fboundp 'minibuffer-prompt-end) (minibuffer-prompt-end) (point-min)))


;;; Icicle functions - Icicle mode..........................

(defun icicle-rebind-completion-maps (turn-on-p)
  "Rebind minibuffer completion maps to be able to cycle completions.
Also, update the bindings in the minibuffer-completion help variables.

This is called by `icicle-mode'.  When in Icicle mode, all keys that
are globally bound to `next-line' are rebound in the minibuffer to
`icicle-next-prefix-candidate', for minibuffer completion purposes.
Similarly for other keys."
  (cond
    (turn-on-p                          ; TURN IT ON ********************************

     ;; `minibuffer-local-map': default minibuffer map.
     (if (> emacs-major-version 21)
         (define-key minibuffer-local-map [menu-bar minibuf quit]
           (list 'menu-item "Quit" 'icicle-abort-minibuffer-input
                 :help "Abort input and exit minibuffer"))
       (define-key minibuffer-local-map [menu-bar minibuf quit]
         (cons "Quit" 'icicle-abort-minibuffer-input)))
     (define-key minibuffer-local-map [(control ?g)]           'icicle-abort-minibuffer-input)
     (define-key minibuffer-local-map [(meta shift backspace)] 'icicle-erase-minibuffer)
     (define-key minibuffer-local-map [(meta shift delete)]    'icicle-erase-minibuffer)
     (define-key minibuffer-local-map [(meta ?.)]              'icicle-insert-string-near-point)

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     ;;(define-key minibuffer-local-ns-map [(control ?g)]  'icicle-abort-minibuffer-input)
     ;;(define-key minibuffer-local-ns-map [(meta shift backspace)] 'icicle-erase-minibuffer)
     ;;(define-key minibuffer-local-ns-map [(meta shift delete)]    'icicle-erase-minibuffer)

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     (define-key minibuffer-local-isearch-map [(control ?g)]  'icicle-abort-minibuffer-input)
     (define-key minibuffer-local-isearch-map [(meta shift backspace)] 'icicle-erase-minibuffer)
     (define-key minibuffer-local-isearch-map [(meta shift delete)]    'icicle-erase-minibuffer)
     (define-key minibuffer-local-isearch-map [(meta ?.)]              'icicle-insert-string-near-point)

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-bind-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-filename-completion-map': file-name completion map (Emacs 22).
     (when (boundp 'minibuffer-local-filename-completion-map)
       (icicle-bind-completion-keys minibuffer-local-filename-completion-map))

     ;; `minibuffer-local-must-match-map': must-match map.
     (icicle-bind-completion-keys minibuffer-local-must-match-map)
     (define-key minibuffer-local-must-match-map [(shift return)] 'icicle-apropos-complete-and-exit)
     
     ;; `completion-list-mode-map': map for *Completions* buffer.
     ;; Abort on `C-g' or `q'.  Switch to minibuffer on [insert].  Do not allow normal input.
     (define-key completion-list-mode-map [(control ?g)]   'icicle-abort-minibuffer-input)
     (define-key completion-list-mode-map "q"              'icicle-abort-minibuffer-input)
     (define-key completion-list-mode-map [insert]         'icicle-switch-to-minibuffer)
     ;;(define-key completion-list-mode-map [down]           'icicle-next-line)
     ;;(define-key completion-list-mode-map [up]             'icicle-previous-line)
     ;;;;(define-key completion-list-mode-map [S-iso-lefttab]  'icicle-move-to-previous-completion)
     ;;(define-key completion-list-mode-map [(shift tab)]    'icicle-move-to-previous-completion)
     ;;(define-key completion-list-mode-map [left]           'icicle-move-to-previous-completion)
     ;;(define-key completion-list-mode-map [(control ?i)]   'icicle-move-to-next-completion)
     ;;(define-key completion-list-mode-map [tab]            'icicle-move-to-next-completion)
     ;;(define-key completion-list-mode-map [right]          'icicle-move-to-next-completion)
     (define-key completion-list-mode-map [(control button2)] 'icicle-mouse-candidate-action)
     ;;(define-key completion-list-mode-map [C-mouse-2]      nil)
     ;; (suppress-keymap completion-list-mode-map) ; Inhibit character self-insertion.
     )


    (t                                  ; TURN IT OFF *******************************

     ;; `minibuffer-local-map': default minibuffer map.
     (if (> emacs-major-version 21)
         (define-key minibuffer-local-map [menu-bar minibuf quit]
           (list 'menu-item "Quit" 'keyboard-escape-quit
                 :help "Abort input and exit minibuffer"))
       (define-key minibuffer-local-map [menu-bar minibuf quit]
         (cons "Quit" 'keyboard-escape-quit)))
     (define-key minibuffer-local-map [(control ?g)]             'abort-recursive-edit)
     (define-key minibuffer-local-map [(meta shift backspace)] nil)
     (define-key minibuffer-local-map [(meta shift delete)]    nil)
     (define-key minibuffer-local-map [(meta ?.)]     nil)

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     ;;(define-key minibuffer-local-ns-map [(control ?g)]          'abort-recursive-edit)
     ;;(define-key minibuffer-local-ns-map [(meta shift backspace)] nil)
     ;;(define-key minibuffer-local-ns-map [(meta shift delete)]    nil)

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     (define-key minibuffer-local-isearch-map [(control ?g)]     'abort-recursive-edit)
     (define-key minibuffer-local-isearch-map [(meta shift backspace)] nil)
     (define-key minibuffer-local-isearch-map [(meta shift delete)]    nil)
     (define-key minibuffer-local-isearch-map [(meta ?.)]     nil)

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-restore-completion-keys minibuffer-local-completion-map)
     

     ;; `minibuffer-local-filename-completion-map': file-name completion map.
     (when (boundp 'minibuffer-local-filename-completion-map)
       (icicle-restore-completion-keys minibuffer-local-filename-completion-map))

     ;; `minibuffer-local-must-match-map': must-match map.
     (icicle-restore-completion-keys minibuffer-local-must-match-map)
     (define-key minibuffer-local-must-match-map [(shift return)] nil)

     ;; `completion-list-mode-map': map for *Completions* buffer.
     (define-key completion-list-mode-map [(control ?g)]         nil)
     (define-key completion-list-mode-map "q"                    nil)
     (define-key completion-list-mode-map [insert]               nil)
     ;;(define-key completion-list-mode-map [down]                 nil)
     ;;(define-key completion-list-mode-map [up]                   nil)
     ;;(define-key completion-list-mode-map [left]                 'previous-completion)
     ;;(define-key completion-list-mode-map [right]                'next-completion)
     ;;(define-key completion-list-mode-map [(shift tab)]          nil)
     ;;(define-key completion-list-mode-map [tab]                  nil)
     ;;(define-key completion-list-mode-map [(control ?i)]         nil)
     (define-key completion-list-mode-map [(control button2)]    nil)
     ;;(define-key completion-list-mode-map [C-down-mouse-2]       (if (boundp 'facemenu-mouse-menu)
       ;;                                                              facemenu-mouse-menu
         ;;                                                          facemenu-menu))))
     ))

  ;; Update the bindings within the help string.
  (setq icicle-completion-help-string
        (substitute-command-keys
         "\\<minibuffer-local-completion-map>                        \
Minibuffer Completion
                        ---------------------

Minibuffer input can be completed in several ways.
These are the main actions and their key bindings.

 * Display this help.				\\[icicle-completion-help]

 * Complete the current input in the minibuffer.
        Prefix completion:
           A word at a time                     \\[icicle-prefix-complete]
           As much as possible                  \\[icicle-prefix-word-complete]
        Apropos (regexp) completion:            \\[icicle-apropos-complete]

 * Choose a completion candidate.
	Cycle among prefix completions:		\\[icicle-next-prefix-candidate], \
\\[icicle-previous-prefix-candidate]
	Cycle among apropos completions:	\\[icicle-next-apropos-candidate], \
\\[icicle-previous-apropos-candidate]

 * Retrieve your last real input.               \\[icicle-retrieve-last-input]

 * Act on completion candidates (show help, if no action is defined).
	Current candidate:			\\[icicle-candidate-action], \
\\<completion-list-mode-map>\\[icicle-mouse-candidate-action]\\<minibuffer-local-completion-map>
	Next, previous prefix candidate:	\\[icicle-next-prefix-candidate-action], \
\\[icicle-previous-prefix-candidate-action]
	Next, previous apropos candidate:	\\[icicle-next-apropos-candidate-action], \
\\[icicle-previous-apropos-candidate-action]
        All candidates at once                  \\[icicle-all-candidates-action]
	Show help on current candidate:         \\[icicle-help-on-candidate]

 * Perform set operations on candidate sets.
        Set complement                          \\[icicle-candidate-set-complement]
        Set difference                          \\[icicle-candidate-set-difference]
        Set union                               \\[icicle-candidate-set-union]
        Set intersection                        \\[icicle-candidate-set-intersection]
        Save current set                        \\[icicle-candidate-set-save]
        Retrieve saved set                      \\[icicle-candidate-set-retrieve]
        Swap current and saved sets             \\[icicle-candidate-set-swap]
        Define current set by evalling sexpr    \\[icicle-candidate-set-define]
        Restrict candidates to history items:   \\[icicle-keep-only-past-inputs]

 * Display completions for current input, in buffer *Completions*.
        Show completion candidates:
           Prefix completion			\\[icicle-prefix-complete] (twice)
           Apropos completion			\\[icicle-apropos-complete]
        Move between minibuffer and list:	\\<completion-list-mode-map>\
\\[icicle-switch-to-minibuffer]
        Move among completion candidates:	\\[next-line], \\[previous-line], \
\\[icicle-move-to-next-completion], \\[icicle-move-to-previous-completion]
        Choose a completion candidate:		\\[choose-completion], \
\\[mouse-choose-completion]

 * Toggle some user options.
        Toggle ignoring certain file extensions \\<minibuffer-local-completion-map>\
\\[icicle-toggle-ignored-extensions]
        Toggle sorting completion candidates    \\[icicle-toggle-sorting]

 * Choose a previous input from the minibuffer history.
        Apropos-complete against history items: \\[icicle-history], \
\\[icicle-keep-only-past-inputs]
        Restrict candidates to history items:   \\[icicle-keep-only-past-inputs]
	Cycle among minibuffer history items:	\\[next-history-element], \
\\[previous-history-element]
	Search among minibuffer history items:	\
\\[next-matching-history-element], \\[previous-matching-history-element]

 * Manipulate your input.  You can modify it, before committing it.
        Erase (clear) input:			\\[icicle-erase-minibuffer]
        Abandon input:				\\[icicle-abort-minibuffer-input]
        Send input to Emacs:			\\[exit-minibuffer]

Remember: You can always input any character that is bound to a
          command (e.g. \\[icicle-prefix-complete]) \
by preceding it with \\<global-map>\\[quoted-insert].

User options controlling minibuffer completion and cycling:

 * `completion-ignore-case', `read-file-name-completion-ignore-case'
                                          - Case sensitivity?
 * `icicle-change-region-background-flag' - Change region color?
 * `icicle-color-themes'                  - For `icicle-color-theme'
 * `icicle-completion-nospace-flag'       - Ignore space at start?
 * `icicle-Completions-frame-at-right-flag'- *Completions* at right?
 * `icicle-cycle-into-subdirs-flag'       - Explore subdirectories?
 * `icicle-incremental-completion-flag'   - *Completions* icompletion?
 * `icicle-init-value-flag'               - Use default as init value?
 * `icicle-list-join-string'              - Multi-completion join
 * `icicle-mark-position-in-candidate'    - Mark position in cycling
 * `icicle-minibuffer-setup-hook'         - Functions run after setup
 * `icicle-change-region-background-flag' - Change region color?
 * `icicle-region-background'             - Background for region
 * `icicle-require-match-flag'            - Override REQUIRE-MATCH?
 * `icicle-search-ring-max', `icicle-regexp-search-ring-max'
                                          - Search ring sizes
 * `icicle-show-Completions-initially-flag'- Show *Completions* first?
 * `icicle-sort-function'                 - Sort completion candidates
 * `icicle-word-completion-key'           - Keys for word completion

A couple of the Icicles faces you can customize:

 * `icicle-root-highlight-minibuffer'    - Highlight root (minibuffer)
 * `icicle-root-highlight-Completions'   - Same, but in *Completions*

----------------------------------------------------------------------

These are all of the minibuffer bindings during completion:

\\{minibuffer-local-completion-map}---------------------------------------\
-------------------------------
"))

  (setq icicle-prompt-suffix
        (substitute-command-keys
         " (\\<minibuffer-local-completion-map>\\[icicle-apropos-complete], \
\\[icicle-prefix-complete]: list, \\[icicle-completion-help]: help) "))
  (when (and (interactive-p) turn-on-p)
    (message (substitute-command-keys
              "Use `\\<minibuffer-local-completion-map>\
\\[icicle-completion-help]' in minibuffer for help."))))

(defun icicle-bind-completion-keys (map)
  "Bind keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Menu-bar Minibuf menu.
  ;; Remap some commands bound globally.

  (icicle-remap 'self-insert-command 'icicle-self-insert map)
  (icicle-remap 'backward-delete-char-untabify 'icicle-backward-delete-char-untabify map)
  (icicle-remap 'delete-backward-char          'icicle-delete-backward-char map)
  (icicle-remap 'backward-kill-word            'icicle-backward-kill-word map)
  (icicle-remap 'kill-word                     'icicle-kill-word map)
  (icicle-remap 'backward-kill-sexp            'icicle-backward-kill-sexp map)
  (icicle-remap 'kill-sexp                     'icicle-kill-sexp map)
  (icicle-remap 'backward-kill-sentence        'icicle-backward-kill-sentence map)
  (icicle-remap 'kill-sentence                 'icicle-kill-sentence map)
  (icicle-remap 'backward-kill-paragraph       'icicle-backward-kill-paragraph map)
  (icicle-remap 'kill-paragraph                'icicle-kill-paragraph map)
  (icicle-remap 'kill-line                     'icicle-kill-line map)
  (icicle-remap 'kill-region                   'icicle-kill-region map)
  (icicle-remap 'kill-region-wimpy             'icicle-kill-region-wimpy map)
  (icicle-remap 'transpose-chars               'icicle-transpose-chars map)
  (icicle-remap 'transpose-words               'icicle-transpose-words map)
  (icicle-remap 'transpose-sexps               'icicle-transpose-sexps map)
  (icicle-remap 'transpose-yank                'icicle-transpose-yank map)
  (icicle-remap 'transpose-yank-pop            'icicle-transpose-yank-pop  map)
  (icicle-remap 'help-command                  'icicle-completion-help map)
  (icicle-remap 'previous-line                 'icicle-previous-prefix-candidate map)
  (icicle-remap 'next-line                     'icicle-next-prefix-candidate map)
  (icicle-remap 'scroll-up-command             'icicle-next-apropos-candidate map)
  (icicle-remap 'scroll-down-command           'icicle-previous-apropos-candidate map)
  (icicle-remap 'backward-paragraph            'icicle-previous-prefix-candidate-action map)
  (icicle-remap 'forward-paragraph             'icicle-next-prefix-candidate-action map)
  (icicle-remap 'scroll-right                  'icicle-previous-apropos-candidate-action map)
  (icicle-remap 'scroll-left                   'icicle-next-apropos-candidate-action map)

  ;; Bind some additional keys.
  (define-key map icicle-word-completion-key 'icicle-prefix-word-complete)
  (define-key map [(meta shift backspace)]   'icicle-erase-minibuffer)
  (define-key map [(meta shift delete)]      'icicle-erase-minibuffer)
  (define-key map [(meta ?h)]                'icicle-history)
  (define-key map [(meta pause)]             'icicle-keep-only-past-inputs)
  (define-key map [(control help)]           'icicle-help-on-candidate)
  (define-key map [(control f1)]             'icicle-help-on-candidate)
  (define-key map [(control return)]         'icicle-candidate-action)
  (define-key map [(control ?o)]             'icicle-candidate-action)
  (define-key map [(control ?!)]             'icicle-all-candidates-action)
  (define-key map [(shift tab)]              'icicle-apropos-complete)
  (define-key map [(control ?i)]             'icicle-prefix-complete)
  (define-key map [tab]                      'icicle-prefix-complete)
  (define-key map [(meta control ?/)]        'icicle-prefix-complete) ; For `dabbrev.el'.
  (define-key map [insert]                   'icicle-switch-to-Completions-buf)
  ;; `minibuffer-completion-help' got wiped out by remap for self-insert.
  (define-key map "?"                        'icicle-self-insert)
  (define-key map [(control ?g)]             'icicle-abort-minibuffer-input)
  (define-key map [(control ?l)]             'icicle-retrieve-last-input)
  (define-key map " "                        'icicle-self-insert)
  (define-key map [(control ?~)]             'icicle-candidate-set-complement)
  (define-key map [(control ?-)]             'icicle-candidate-set-difference)
  (define-key map [(control ?+)]             'icicle-candidate-set-union)
  (define-key map [(control ?*)]             'icicle-candidate-set-intersection)
  (define-key map [(control ?>)]             'icicle-candidate-set-save)
  (define-key map [(control ?<)]             'icicle-candidate-set-retrieve)
  (define-key map [(control ?%)]             'icicle-candidate-set-swap)
  (define-key map [(control ?:)]             'icicle-candidate-set-define)
  (define-key map [(control ?,)]             'icicle-toggle-sorting)
  (define-key map [(control ?.)]             'icicle-toggle-ignored-extensions)
  (define-key map [(meta ?.)]                'icicle-insert-string-near-point)
  (define-key map [(meta ?*)]                'icicle-narrow-candidates))

(defun icicle-restore-completion-keys (map)
  "Restore standard keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Restore remapped commands.
  (icicle-unmap 'self-insert-command map)
  (icicle-unmap 'backward-delete-char-untabify map)
  (icicle-unmap 'delete-backward-char          map)
  (icicle-unmap 'backward-kill-word            map)
  (icicle-unmap 'kill-word                     map)
  (icicle-unmap 'backward-kill-sexp            map)
  (icicle-unmap 'kill-sexp                     map)
  (icicle-unmap 'backward-kill-sentence        map)
  (icicle-unmap 'kill-sentence                 map)
  (icicle-unmap 'backward-kill-paragraph       map)
  (icicle-unmap 'kill-paragraph                map)
  (icicle-unmap 'kill-line                     map)
  (icicle-unmap 'kill-region                   map)
  (icicle-unmap 'kill-region-wimpy             map)
  (icicle-unmap 'transpose-chars               map)
  (icicle-unmap 'transpose-words               map)
  (icicle-unmap 'transpose-sexps               map)
  (icicle-unmap 'transpose-yank                map)
  (icicle-unmap 'transpose-yank-pop            map)
  (icicle-unmap 'help-command                  map)
  (icicle-unmap 'previous-line                 map)
  (icicle-unmap 'next-line                     map)
  (icicle-unmap 'scroll-up-command             map)
  (icicle-unmap 'scroll-down-command           map)
  (icicle-unmap 'backward-paragraph            map)
  (icicle-unmap 'forward-paragraph             map)
  (icicle-unmap 'scroll-right                  map)
  (icicle-unmap 'scroll-left                   map)

  ;; Restore additional bindings.
  (define-key map icicle-word-completion-key nil) ; Do first, so can be rebound, as needed.
  (define-key map [(meta shift backspace)]   nil)
  (define-key map [(meta shift delete)]      nil)
  (define-key map [(meta ?h)]                nil)
  (define-key map [(meta pause)]             nil)
  (define-key map [(control help)]           nil)
  (define-key map [(control f1)]             nil)
  (define-key map [(control ?l)]             nil)
  (define-key map [(control return)]         nil)
  (define-key map [(control ?o)]             nil)
  (define-key map [(control ?!)]             nil)
  ;(define-key map [S-iso-lefttab]            nil)
  (define-key map [(shift tab)]              nil)
  (define-key map [(control ?~)]             nil)
  (define-key map [(control ?-)]             nil)
  (define-key map [(control ?+)]             nil)
  (define-key map [(control ?*)]             nil)
  (define-key map [(control ?>)]             nil)
  (define-key map [(control ?<)]             nil)
  (define-key map [(control ?%)]             nil)
  (define-key map [(control ?:)]             nil)
  (define-key map [(control ?,)]             nil)
  (define-key map [(control ?.)]             nil)
  (define-key map [(meta ?.)]                nil)
  (define-key map [(meta ?*)]                nil)
  (define-key map "?"                        'minibuffer-completion-help)
  (define-key map [(control ?i)]             'minibuffer-complete)
  (define-key map [tab]                      'minibuffer-complete)
  (define-key map [(meta control ?/)]        nil)
  (define-key map [(control ?g)]             'minibuffer-keyboard-quit)
  (define-key map " "                        'minibuffer-complete-word)
  ;;(define-key map [(meta ?n)]                'next-history-element)
  ;;(define-key map [down]                     'next-history-element)
  ;;(define-key map [next]                     'next-history-element)
  ;;(define-key map [(meta ?p)]                'previous-history-element)
  ;;(define-key map [up]                       'previous-history-element)
  (define-key map [prior]                    'switch-to-completions)
  (define-key map [(meta ?v)]                'switch-to-completions))


(defun icicle-remap (from to map)
  "Remap command FROM to command TO in keymap MAP.
For all versions of Emacs >= 20."
  (dolist (key (where-is-internal from global-map))
    (define-key map key to)))

(defun icicle-unmap (from map)
  "Unmap command FROM: remove its binding from keymap MAP.
For all versions of Emacs >= 20."
  (dolist (key (where-is-internal from global-map))
    (define-key map key nil)))

;; Inspired from `icomplete-minibuffer-setup'.
;; We change the region background here dynamically.
;; It would be better to just use a buffer-local face, but those don't yet exist.
;;
(defun icicle-minibuffer-setup ()
  "Run in minibuffer on activation, to enable completion cycling.
Usually run by inclusion in `minibuffer-setup-hook'."
  (cond ((and icicle-mode
              (window-minibuffer-p (selected-window))
              (not executing-kbd-macro))
	 (unless (fboundp 'define-minor-mode) (make-local-hook 'pre-command-hook))
	 (add-hook 'pre-command-hook
		   (function (lambda () (run-hooks 'icicle-pre-command-hook)))
		   nil t)
	 (unless (fboundp 'define-minor-mode) (make-local-hook 'post-command-hook))
	 (add-hook 'post-command-hook
		   (function (lambda () (run-hooks 'icicle-post-command-hook)))
		   nil t)
         (setq icicle-saved-region-background (face-background 'primary-selection))
         (when icicle-change-region-background-flag
           (set-face-background 'primary-selection icicle-region-background))
         ;; Reset prompt, because some commands (e.g. `find-file') don't use `read-file-name'
         ;; or `completing-read'.  Reset other stuff too.
         (setq icicle-prompt                  ""
               icicle-default-directory       default-directory
               icicle-last-completion-command nil
               icicle-last-input              ""
               icicle-candidate-nb            nil
	       icicle-incremental-completion-p icicle-incremental-completion-flag)
         (icicle-update-ignored-extensions-regexp)
         (when (memq icicle-init-value-flag '(preselect-start preselect-end))
           (icicle-select-minibuffer-contents))
         (when (and icicle-show-Completions-initially-flag (icicle-completing-p))
           (icicle-display-Completions))
	 (run-hooks 'icicle-minibuffer-setup-hook))))


(defun icicle-reset-icicle-completing-p ()
  "Don't assume that completion uses Icicles.
Used in `minibuffer-exit-hook'.
Internal flag `icicle-icicle-completing-p' is used in
`icicle-narrow-candidates'."
  (setq icicle-icicle-completing-p nil))

(defun icicle-set-calling-cmd ()
  "Remember last command that called for completion.
Used in `completion-setup-hook'."
  (setq icicle-cmd-calling-for-completion this-command))

(defun icicle-update-ignored-extensions-regexp ()
  "Update ignored extensions if `completion-ignored-extensions' changed."
  (when (and (icicle-file-name-input-p)
             (not (equal icicle-ignored-extensions completion-ignored-extensions)))
    (if (consp completion-ignored-extensions)
	(setq icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
	      (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|") "\\)\\'"))
      (setq icicle-ignored-extensions-regexp nil))
    ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
    ;; `completion-ignored-extensions' changes.
    (setq icicle-ignored-extensions completion-ignored-extensions)))

(defun icicle-completing-p ()
  "Non-nil if reading minibuffer input with completion."
  (not (null minibuffer-completion-table)))

;; We change the region background here dynamically.
;; It would be better to just use a buffer-local face, but those don't yet exist.
(defun icicle-restore-region-face ()
  "Restore region face.  It was changed during minibuffer activity
if `icicle-change-region-background-flag' is non-nil."
  (when icicle-change-region-background-flag
    (set-face-background 'primary-selection icicle-saved-region-background)))
;;(add-hook 'minibuffer-exit-hook 'icicle-restore-region-face)
;;(add-hook 'minibuffer-exit-hook 'zmacs-deactivate-region)

;;(defun icicle-activate-mark ()
;;  "Prevent region from being deactivated.  Use in `icicle-post-command-hook'."
;;  (when (and (window-minibuffer-p (selected-window)) (not executing-kbd-macro))
;;    (setq deactivate-mark nil)))
;;(add-hook 'icicle-post-command-hook 'icicle-activate-mark 'append)


;;; Icicle functions - prefix completion cycling............

(defun icicle-prefix-candidates (input)
  "List of candidate prefix completions for the current partial INPUT.
INPUT is a string.  Each candidate is a string."
  (if icicle-sort-function
      (sort (icicle-unsorted-prefix-candidates input) icicle-sort-function)
    (icicle-unsorted-prefix-candidates input)))

(defun icicle-unsorted-prefix-candidates (input)
  "Unsorted list of prefix completions for the current partial INPUT."
  (append icicle-extra-candidates
          (icicle-delete-if-not
           (lambda (cand) (let ((case-fold-search completion-ignore-case))
                            (icicle-filter-wo-input cand)))
           (all-completions input minibuffer-completion-table minibuffer-completion-predicate))))
                            ;;icicle-completion-nospace-flag))))

(defun icicle-file-name-prefix-candidates (input)
  "List of prefix completions for partial file name INPUT.
INPUT is a string.
Candidates can be directories.  Each candidate is a string."
  (let ((default-directory (icicle-file-name-directory-w-default input)))
    (icicle-sort-and-strip-ignored
     (icicle-unsorted-file-name-prefix-candidates
      (or (icicle-file-name-nondirectory input) "")))))

(defun icicle-unsorted-file-name-prefix-candidates (input)
  "Unsorted list of prefix completions for the current file-name INPUT."
  (let ((slashed-p (and (> (length input) 0) (eq ?/ (aref input 0)))))
    (when slashed-p (setq input (substring input 1)))
    (append icicle-extra-candidates
            (icicle-delete-if-not
             (lambda (cand) (let ((case-fold-search completion-ignore-case))
                              (if (member cand '("../" "./"))
                                  (member input '(".." ".")) ; Prevent "" from matching "../"
                                (and (string-match (concat "^" (regexp-quote input)) cand)
                                     (icicle-filter-wo-input cand)))))
             (all-completions input minibuffer-completion-table
                              (if slashed-p "/" default-directory))))))
                              ;;icicle-completion-nospace-flag)))))


;;; Icicle functions - apropos completion cycling...........

(defun icicle-apropos-candidates (input)
  "List of candidate apropos completions for the current partial INPUT.
INPUT is a string.  Each candidate is a string."
  (if icicle-sort-function
      (sort (icicle-unsorted-apropos-candidates input) icicle-sort-function)
    (icicle-unsorted-apropos-candidates input)))

(defun icicle-unsorted-apropos-candidates (input)
  "Unsorted list of apropos completions for the current partial INPUT."
  (append icicle-extra-candidates
          (icicle-delete-if-not
           (lambda (cand) (let ((case-fold-search completion-ignore-case))
                            (and (string-match input cand) (icicle-filter-wo-input cand))))
           (all-completions ""
                            minibuffer-completion-table
                            minibuffer-completion-predicate))))
                            ;;;icicle-completion-nospace-flag))))

(defun icicle-file-name-apropos-candidates (input)
  "List of apropos completions for partial file-name INPUT.
INPUT is a string.
Candidates can be directories.  Each candidate is a string."
  (let ((default-directory (icicle-file-name-directory-w-default input)))
    (icicle-sort-and-strip-ignored
     (icicle-unsorted-file-name-apropos-candidates
      (or (icicle-file-name-nondirectory input) "")))))

(defun icicle-unsorted-file-name-apropos-candidates (input)
  "Unsorted list of apropos completions for the partial file-name INPUT."
  (let ((slashed-p (and (> (length input) 0) (eq ?/ (aref input 0)))))
    (when slashed-p (setq input (substring input 1)))
    (append icicle-extra-candidates
            (icicle-delete-if-not
             (lambda (cand) (let ((case-fold-search completion-ignore-case))
                              (if (member cand '("../" "./")) ; Prevent "" from matching "../"
                                  (member input '(".." "."))
                                (and (string-match input cand) (icicle-filter-wo-input cand)))))
             (all-completions "" minibuffer-completion-table
                              (if slashed-p "/" default-directory))))))
                              ;;;icicle-completion-nospace-flag)))))


;; Icicle functions - common helper functions...............

;; Main function - used by `icicle-next-prefix-candidate', `icicle-next-apropos-candidate'.
(defun icicle-next-candidate (nth candidates-fn &optional regexp-p)
  "Replace input by NTH next or previous completion for an input.
Default value of NTH is 1, meaning use the next completion.
Negative NTH means use a previous, not subsequent, completion.

CANDIDATES-FN is a function that returns the list of candidate
completions for its argument, the current partial input (a string).
Optional arg REGEXP-P non-nil means that CANDIDATES-FN uses regexp
  matching. This is used to highlight the appropriate matching root."
  (unless (stringp icicle-last-completion-candidate)
    (setq icicle-last-completion-candidate ""))
  (setq nth (or nth 1))
  (setq icicle-current-input (icicle-minibuffer-contents))
  (icicle-save-or-restore-input)
  (when (icicle-file-directory-p icicle-current-input)
    (setq icicle-default-directory icicle-current-input))
  (icicle-recompute-candidates nth candidates-fn)
  (cond ((null icicle-completion-candidates)
         (save-selected-window (icicle-delete-windows-on "*Completions*"))
         (minibuffer-message "  [No completion]"))
        (t
         (icicle-erase-minibuffer)
         (let ((nb-cands (length icicle-completion-candidates))
               (unit (if (and (integerp nth) (> 0 nth)) 1 -1))
               next)
           ;; So `icomplete+' can append the number of other candidates to the minibuffer.
           (when icicle-completion-candidates
             (setq icicle-nb-of-other-cycle-candidates (1- nb-cands)))
           (icicle-increment-cand-nb+signal-end nth nb-cands)
           (setq next (elt icicle-completion-candidates icicle-candidate-nb))
           (while (null next)           ; Skip null candidates.
             (icicle-increment-cand-nb+signal-end unit nb-cands)
             (setq next (elt icicle-completion-candidates icicle-candidate-nb)))
           ;; Filter with predicate
           (when (and (not (icicle-file-name-input-p)) ; Pred is special for files.
                      minibuffer-completion-predicate)
             (while (not (condition-case nil
                             (funcall minibuffer-completion-predicate
                                      (if (arrayp minibuffer-completion-table)
                                          (intern next) ; obarray of symbols.
                                        (list next))) ; List of strings (sym names).
                           (error nil)))
               (icicle-increment-cand-nb+signal-end unit nb-cands)
               (setq next (elt icicle-completion-candidates icicle-candidate-nb))
               (while (null next)       ; Skip null candidates.
                 (icicle-increment-cand-nb+signal-end unit nb-cands)
                 (setq next (elt icicle-completion-candidates icicle-candidate-nb)))))
           ;; Reset last candidate.  Need a copy, because we change its text properties.
           (setq icicle-last-completion-candidate (copy-sequence next))

           ;; Underline the root that was completed, in the minibuffer.
           (let ((case-fold-search completion-ignore-case)
                 (inp (if (icicle-file-name-input-p)
                          (icicle-file-name-nondirectory icicle-current-input)
                        icicle-current-input))
                 indx)
             (unless regexp-p (setq inp (regexp-quote inp)))
             (setq indx (string-match inp icicle-last-completion-candidate))
             (when indx
               (put-text-property indx (match-end 0) 'face 'icicle-root-highlight-minibuffer
                                  icicle-last-completion-candidate)))
           (insert (if (and (icicle-file-name-input-p) insert-default-directory)
                       (icicle-file-name-directory-w-default icicle-current-input)
                     "")
                   icicle-last-completion-candidate)
           (icicle-place-cursor icicle-current-input)

           ;; Highlight current completion candidate, if *Completions* is displayed.
           (when (get-buffer-window "*Completions*" t)

             ;; Refresh *Completions*, updating it to reflect the current candidates.
             (unless
                 (or
                  (and (memq this-command
                             '(icicle-next-apropos-candidate
                               icicle-previous-apropos-candidate
                               icicle-next-apropos-candidate-action
                               icicle-previous-apropos-candidate-action))
                       (memq last-command
                             '(icicle-next-apropos-candidate
                               icicle-previous-apropos-candidate
                               icicle-next-apropos-candidate-action
                               icicle-previous-apropos-candidate-action
                               icicle-candidate-action)))
                  (and (memq this-command
                             '(icicle-next-prefix-candidate
                               icicle-previous-prefix-candidate
                               icicle-next-prefix-candidate-action
                               icicle-previous-prefix-candidate-action))
                       (memq last-command
                             '(icicle-next-prefix-candidate
                               icicle-previous-prefix-candidate
                               icicle-next-prefix-candidate-action
                               icicle-previous-prefix-candidate-action
                               icicle-candidate-action))))
               (icicle-display-candidates-in-Completions icicle-current-input
                                                           (not (and (integerp nth) (> 0 nth)))))
             ;; Highlight current candidate in *Completions*.
             (let ((compl-win (get-buffer-window "*Completions*" t))
                   curr-candidate-pos)
               (save-window-excursion
                 (select-window compl-win)
                 (let ((case-fold-search completion-ignore-case))
                   (goto-char (point-min))
                   (forward-line 3)
                   (icicle-move-to-next-completion icicle-candidate-nb t)
                   (set-buffer-modified-p nil)
                   (setq curr-candidate-pos (point))))
               (set-window-point compl-win curr-candidate-pos)))))))

(defun icicle-save-or-restore-input ()
  "Save current minibuffer input or restore last input.
If value in minibuffer now is `icicle-last-completion-candidate',
then it is not a real input, so restore last real input.
Otherwise, save current value as last input."
  (if (and (not (string= "" icicle-last-input)) ; Don't restore empty input.
           (not (string= "" icicle-last-completion-candidate))
           (string= (if (icicle-file-name-input-p)
                        (icicle-remove-dots icicle-last-completion-candidate)
                      icicle-last-completion-candidate)
                    ;; If not `icicle-cycle-into-subdirs-flag', then compare with relative
                    ;; file name, which, in the case of a directory, is the directory name.
                    ;; Otherwise, compare with nondirectory part of name, which, in the
                    ;; case of a directory, is "".
                    (if (icicle-file-name-input-p)
                        (if icicle-cycle-into-subdirs-flag
                            (icicle-file-name-nondirectory icicle-current-input)
                          (icicle-remove-dots
                           (file-relative-name icicle-current-input icicle-default-directory)))
                      icicle-current-input))
	   (not (string= icicle-current-input icicle-last-completion-candidate)))
      (setq icicle-current-input icicle-last-input) ; Restore last real input.
    (setq icicle-last-completion-candidate icicle-current-input ; Update to use current input.
          icicle-last-input icicle-current-input)))

(defun icicle-remove-dots (filename)
  "Strip leading string through last ../ or ./ from FILENAME."
  (let ((newname filename))
    (while
        (or (string-match "\\.\\./" newname)
            (string-match "\\./" newname)
            ;; Emacs 21+ `file-relative-name' returns ".." and "." (no slash) for "" first arg
            (string-match "^\\.\\.$" newname) 
            (string-match "^\\.$" newname))
      (setq newname (substring newname (match-end 0))))
    newname))

(defun icicle-recompute-candidates (nth candidates-fn)
  "Recompute `icicle-completion-candidates', if needed.
If buffer *Completions* is already displayed, it is updated.
This does nothing, unless the user changed the minibuffer input or the
completion type has changed (from apropos to prefix or vice versa).
Argument NTH is passed to `icicle-display-candidates-in-Completions'.
Argument CANDIDATES-FN is a function that recomputes the candidates."
  (unless (and icicle-last-completion-command
               (string= icicle-current-input icicle-last-input) ; No change in user input.
               ;; No change in completion type: apropos vs prefix.
               (or (and (memq icicle-last-completion-command
                              '(icicle-apropos-complete icicle-candidate-set-complement
							icicle-keep-only-past-inputs))
                        (memq this-command '(icicle-apropos-complete
                                             icicle-next-apropos-candidate
                                             icicle-next-apropos-candidate-action
                                             icicle-previous-apropos-candidate
                                             icicle-previous-apropos-candidate-action)))
                   (and (memq icicle-last-completion-command
                              '(icicle-prefix-complete icicle-candidate-set-complement))
                        (memq this-command '(icicle-prefix-complete
                                             icicle-next-prefix-candidate
                                             icicle-next-prefix-candidate-action
                                             icicle-previous-prefix-candidate
                                             icicle-previous-prefix-candidate-action)))))
    ;; Set `icicle-last-completion-command', to record new completion type.
    (case this-command
      ((icicle-next-prefix-candidate
        icicle-previous-prefix-candidate
        icicle-next-prefix-candidate-action
        icicle-previous-prefix-candidate-action)
       (setq icicle-last-completion-command 'icicle-prefix-complete))
      ((icicle-next-apropos-candidate
        icicle-previous-apropos-candidate
        icicle-next-apropos-candidate-action
        icicle-previous-apropos-candidate-action)
       (setq icicle-last-completion-command 'icicle-apropos-complete)))
    ;; Recompute and redisplay completion candidates.  Reset candidate number.
    (setq icicle-completion-candidates (funcall candidates-fn icicle-current-input)
          icicle-candidate-nb          nil)
    (when (get-buffer-window "*Completions*" 0)
      (icicle-display-candidates-in-Completions icicle-current-input (not (and (integerp nth) (> 0 nth)))))))

(defun icicle-increment-cand-nb+signal-end (incr max)
  "Increment candidate number by INCR modulo MAX, and signal end of cycle."
  (if icicle-candidate-nb
      (setq icicle-candidate-nb (+ incr icicle-candidate-nb))
    (setq icicle-candidate-nb 0))       ; Reset.
  (setq icicle-candidate-nb (mod icicle-candidate-nb max))
  (when (and (= 0 icicle-candidate-nb)  ; Signal end of cycle.
             (eq last-command this-command))
    (let ((visible-bell t)) (ding) (setq visible-bell nil) (ding))))

(defun icicle-place-overlay (start end)
  "Put completion-candidate overlay between START and END.
START and END are in the current buffer.
Overlay `icicle-current-completion-candidate-overlay' is created with
`highlight' face, unless it exists."
  (if icicle-current-completion-candidate-overlay ; Don't recreate if exists.
      (move-overlay icicle-current-completion-candidate-overlay start end (current-buffer))
    (setq icicle-current-completion-candidate-overlay (make-overlay start end))
    (overlay-put icicle-current-completion-candidate-overlay 'face 'highlight)))

(defun icicle-sort-and-strip-ignored (candidates)
  "Remove file names with ignored extensions, and \".\".  Sort CANDIDATES.
If `icicle-sort-function' is nil, then do not sort."
  (let* ((pred1 (lambda (cand) (or (string-match icicle-ignored-extensions-regexp cand)
                                 (string= "./" cand))))
         (pred2 (lambda (cand) (string= "./" cand)))
         (new-candidates (icicle-delete-if (if icicle-ignored-extensions-regexp pred1 pred2)
                                           candidates)))
    ;; If the only candidates have ignored extensions, use them.
    (unless new-candidates (setq new-candidates (icicle-delete-if pred2 candidates)))
  (if icicle-sort-function
        (sort new-candidates icicle-sort-function)
      new-candidates)))

(defun icicle-file-name-directory-w-default (file)
  "Like `file-name-directory', but return `default-directory', not nil.
Does not treat backslash as a directory separator, even on MS Windows."
  (let ((escaped-file (subst-char-in-string ?\\ ?\a file)))
    (or (file-name-directory escaped-file) default-directory)))

(defun icicle-file-name-nondirectory (file)
  "Like `file-name-nondirectory', but does not treat backslash specially.
That is, backslash is never treated as a directory separator."
  (let ((escaped-file (subst-char-in-string ?\\ ?\a file)))
    (subst-char-in-string ?\a ?\\ (file-name-nondirectory escaped-file))))

(defun icicle-file-name-input-p ()
  "Return non-nil if expected input is a file name.
This is used, instead of variable `minibuffer-completing-file-name',
because we sometimes complete against an explicit alist of file names,
even in the overall context of file-name input.  In that case, we do
not want to use file-name completion.  An example of this is
completing against a history list of file names, using
`icicle-history'."
  (and (symbolp minibuffer-completion-table) (stringp minibuffer-completion-predicate)))

(defun icicle-sort-dirs-last (name1 name2)
  "Non-nil if NAME1 is a file and NAME2 is a dir, or `string-lessp'.
This can be used as the value for `icicle-sort-function'.
It is especially useful when `icicle-cycle-into-subdirs-flag' is
non-nil.  Otherwise, cycling into subdirectories is depth-first, not
breadth-first."
  (if (icicle-file-name-input-p)
      (let ((name1-dir-p (icicle-file-directory-p name1))
            (name2-dir-p (icicle-file-directory-p name2)))
        (if (or (and name1-dir-p name2-dir-p) ; Both or neither are directories.
                (not (or name1-dir-p name2-dir-p)))
            (string-lessp name1 name2)  ; Compare equals.
          name2-dir-p))                 ; Files come before directories.
    (string-lessp name1 name2)))

(defun icicle-sort-case-insensitively (string1 string2)
  "Like `string-lessp', but case is ignored, so `A' = `a' , and so on."
  (string-lessp (upcase string1) (upcase string2)))

(defun icicle-file-directory-p (file)
  "Local, faster replacement for `file-directory-p'.
This does not do all of the file-handler processing that 
`file-directory-p' does, so it is not a general replacement."
  (and (stringp file) (string= file (icicle-file-name-directory-w-default file))))

(defun icicle-minibuffer-contents ()
  "Return the user minibuffer input as a string, without text-properties.
The current buffer must be a minibuffer."
  (let ((input (if (fboundp 'minibuffer-contents-no-properties)
                   (minibuffer-contents-no-properties) ; e.g. Emacs 22
                 (buffer-substring-no-properties (point-min) (point-max))))) ; e.g. Emacs 20
    (when (and (icicle-file-name-input-p)
               (not (string= "" input))) ; Do nothing if user deleted everything in minibuffer.
      (let ((last-char ""))
        (when (string= "$" (substring input (1- (length input)) (length input)))
          (setq last-char "$"
                input (substring input 0 (1- (length input)))))
        (setq input
              (save-match-data
                (concat (subst-char-in-string
                         ?\a ?\\
                         (condition-case nil
                             (substitute-in-file-name
                              (subst-char-in-string ?\\ ?\a input 'in-place))
                           (error input))
                         'in-place)
                        last-char)))))
    input))

(defun icicle-filter-wo-input (candidate)
  "Filter completion CANDIDATE using regexps and predicate.
This filtering is in addition to matching user input."
  (and (or (not icicle-must-match-regexp)
           (string-match icicle-must-match-regexp candidate))
       (or (not icicle-must-not-match-regexp)
           (not (string-match icicle-must-not-match-regexp candidate)))
       (or (not icicle-must-pass-predicate)
           (funcall icicle-must-pass-predicate candidate))))

(defun icicle-update-completions ()
  "Update completions list.  Update display too, if shown."
  (setq icicle-completion-candidates
        (funcall (case icicle-last-completion-command
                   (icicle-prefix-complete (if (icicle-file-name-input-p)
                                               #'icicle-file-name-prefix-candidates
                                             #'icicle-prefix-candidates))
                   (t (if (icicle-file-name-input-p)
                          #'icicle-file-name-apropos-candidates
                        #'icicle-apropos-candidates)))
                 icicle-current-input))
  (when (get-buffer-window "*Completions*" 0)
    (icicle-display-candidates-in-Completions icicle-current-input)))

(defun icicle-msg-maybe-in-minibuffer (format-string &rest args)
  "Display FORMAT-STRING as a message.
If called with the minibuffer active, this is done using `message'.
Otherwise, it is done using `minibuffer-message'."
  (if (active-minibuffer-window)
      (minibuffer-message (apply #'format (concat "  [" format-string "]") args))
    (apply #'message format-string args)))

(defun icicle-delete-if (pred inlist)
  "A copy of list INLIST with no elements that satisfy predicate PRED."
  (let ((outlist nil))
    (dolist (o inlist) (unless (funcall pred o) (push o outlist)))
    (nreverse outlist)))

(defun icicle-delete-if-not (pred inlist)
  "A copy of list INLIST with only elements that satisfy predicate PRED."
  (let ((outlist nil))
    (dolist (o inlist) (when (funcall pred o) (push o outlist)))
    (nreverse outlist)))

(defun icicle-frames-on (buffer &optional frame) ; From `frames-on' in `frame-fns.el'.
  "List of all live frames showing BUFFER (a buffer or its name).
The optional FRAME argument is as for function `get-buffer-window'."
  (filtered-frame-list (function (lambda (fr) (get-buffer-window buffer fr)))))

(defun icicle-candidate-set-1 (set-fn msg)
  "Helper function for defining Icicle set commands.
SET-FN is the function to apply to the current and saved candidates.
MESSAGE is the confirmation message to display in the minibuffer."
  (setq icicle-completion-candidates
        (funcall set-fn icicle-completion-candidates icicle-saved-completion-candidates))
  (if (null icicle-completion-candidates)
      (minibuffer-message "  [EMPTY SET]")
    (when (icicle-file-name-input-p)
      (setq icicle-completion-candidates
	    (icicle-sort-and-strip-ignored icicle-completion-candidates)))
    (icicle-scroll-or-update-Completions msg)))

(defun icicle-scroll-or-update-Completions (msg)
  "Scroll *Completions* if this command was repeated; else update it."
  (if (get-buffer-window "*Completions*" 0)
      (if (eq last-command this-command)
          ;; User repeated the command.  Scroll window around.
          (save-selected-window
            (select-window (get-buffer-window "*Completions*" 0))
            (condition-case nil
                (scroll-up nil)
              (end-of-buffer (goto-char (point-min)) (forward-line 3))))
        ;; User did something else (e.g. changed input).  Update the display.
        (icicle-display-candidates-in-Completions icicle-current-input)
        (minibuffer-message msg))
    ;; No window yet.  Show window.
    (icicle-display-candidates-in-Completions icicle-current-input)
    (minibuffer-message msg)))

(defun icicle-display-Completions ()
  "Display *Completions* buffer."
  (let ((completions (all-completions "" minibuffer-completion-table
                                      minibuffer-completion-predicate)))
                                      ;;;icicle-completion-nospace-flag)))
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list
       (if icicle-sort-function (sort completions icicle-sort-function) completions)))))

;; From `cl-seq.el', function `union', without keyword treatment.
;; Same as `simple-set-union' in `misc-fns.el'.
(defun icicle-set-union (list1 list2)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  This is a non-destructive function; it copies the data if
necessary."
  (cond ((null list1) list2)
        ((null list2) list1)
	((equal list1 list2) list1)
	(t
	 (or (>= (length list1) (length list2))
	     (setq list1 (prog1 list2 (setq list2 list1)))) ; Swap them.
	 (while list2
           (unless (member (car list2) list1)
               (setq list1 (cons (car list2) list1)))
	   (setq list2 (cdr list2)))
	 list1)))

;; From `cl-seq.el', function `intersection', without keyword treatment.
;; Same as `simple-set-intersection' in `misc-fns.el'.
(defun icicle-set-intersection (list1 list2)
  "Set intersection of lists LIST1 and LIST2.
This is a non-destructive operation: it copies the data if necessary."
  (and list1 list2
       (if (equal list1 list2)
           list1
	 (let ((result nil))
           (unless (>= (length list1) (length list2))
             (setq list1 (prog1 list2 (setq list2 list1)))) ; Swap them.
           (while list2
             (when (member (car list2) list1)
               (setq result (cons (car list2) result)))
             (setq list2 (cdr list2)))
           result))))

;; From `cl-seq.el', function `set-difference', without keyword treatment.
;; Same as `simple-set-difference' in `misc-fns.el'.
(defun icicle-set-difference (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all items that appear in LIST1 but not LIST2.
This is non-destructive; it makes a copy of the data if necessary, to
avoid corrupting the original LIST1 and LIST2."
  (if (or (null list1) (null list2)) list1
    (let ((result nil))
      (while list1
        (unless (member (car list1) list2) (setq result (cons (car list1) result)))
        (setq list1 (cdr list1)))
      result)))


(defun display-completion-list (completions &rest cl-keys)
  "Display the list of completions, COMPLETIONS, using `standard-output'.
Each element may be just a symbol or string or may be a list of two
 strings to be printed as if concatenated.
Frob a mousable extent onto each completion.  This extent has properties
 'mouse-face (so it highlights when the mouse passes over it) and
 'list-mode-item (so it can be located).

Keywords:
  :activate-callback (default is `default-choose-completion')
    See `add-list-mode-item'.
  :user-data
    Value passed to activation callback.
  :window-width
    If non-nil, width to use in displaying the list, instead of the
    actual window's width.
  :window-height
    If non-nil, use no more than this many lines, and extend line width as
    necessary.
  :help-string (default is the value of `completion-default-help-string')
    Form to evaluate to get a string to insert at the beginning of
    the completion list buffer.  This is evaluated when that buffer
    is the current buffer and after it has been put into
    completion-list-mode.
  :reference-buffer (default is the current buffer)
    This specifies the value of `completion-reference-buffer' in
    the completion buffer.  This specifies the buffer (normally a
    minibuffer) that `default-choose-completion' will insert the
    completion into.

At the end, run the normal hook `completion-setup-hook'.
It can find the completion buffer in `standard-output'.
If `completion-highlight-first-word-only' is non-nil, then only the start
 of the string is highlighted."
   ;; #### I18N3 should set standard-output to be (temporarily)
   ;; output-translating.
  (cl-parsing-keywords
      ((:activate-callback 'default-choose-completion)
       :user-data
       :reference-buffer
       (:help-string completion-default-help-string)
       (:completion-string "Possible completions are:")
       :window-width
       :window-height)
      ()
    (let ((old-buffer (current-buffer))
	  (bufferp (bufferp standard-output)))
      (if bufferp
	  (set-buffer standard-output))
      (if (null completions)
	  (princ (gettext
		  "There are no possible completions of what you have typed."))
	(let ((win-width
	       (or cl-window-width
		   (if bufferp
		       ;; We have to use last-nonminibuf-frame here
		       ;; and not selected-frame because if a
		       ;; minibuffer-only frame is being used it will
		       ;; be the selected-frame at the point this is
		       ;; run.  We keep the selected-frame call around
		       ;; just in case.
               (window-width (get-lru-window (last-nonminibuf-frame)))
		     80))))
	  (let ((count 0)
		(max-width 0)
		old-max-width)
	    ;; Find longest completion
	    (let ((tail completions))
	      (while tail
		(let* ((elt (car tail))
		       (len (cond ((stringp elt)
				   (length elt))
				  ((and (consp elt)
					(stringp (car elt))
					(stringp (car (cdr elt))))
				   (+ (length (car elt))
				      (length (car (cdr elt)))))
				  (t
				   (signal 'wrong-type-argument
					   (list 'stringp elt))))))
		  (if (> len max-width)
		      (setq max-width len))
		  (setq count (1+ count)
			tail (cdr tail)))))
        
	    (setq max-width (+ 2 max-width)) ; at least two chars between cols
	    (setq old-max-width max-width)
	    (let ((rows (let ((cols (min (/ win-width max-width) count)))
			  (if (<= cols 1)
			      count
			    (progn
			      ;; re-space the columns
			      (setq max-width (/ win-width cols))
			      (if (/= (% count cols) 0) ; want ceiling...
				  (1+ (/ count cols))
                                (/ count cols))))))
		  (cols (min (/ win-width max-width) count)))
	      (when (eq cols 0) (setq cols 1))
	      (when
		  (and cl-window-height
		       (> rows cl-window-height))
		(setq max-width old-max-width)
		(setq rows cl-window-height))
	      (when (and (stringp cl-completion-string)
			 (> (length cl-completion-string) 0))
		(princ (gettext cl-completion-string))
		(terpri))
	      (let ((tail completions)
		    (r 0)
		    (regexp-string
		     (if (eq t
			     completion-highlight-first-word-only)
			 "[ \t]"
		       completion-highlight-first-word-only)))
		(while (< r rows)
		  (and (> r 0) (terpri))
		  (let ((indent 0)
			(column 0)
			(tail2 tail)
			(c 0))
		    (while (and (car tail2) (< c cols))
		      (let ((elt (car tail2)))
			(if (/= indent 0)
			    (if bufferp
				(indent-to indent 2)
                              (while (progn (write-char ?\ )
                                            (setq column (1+ column))
                                            (< column indent)))))
			(setq indent (+ indent max-width))
			(let ((start (point))
			      end)
			  ;; Frob some mousable extents in there too!
			  (if (consp elt)
			      (progn
				(princ (car elt))
				(princ (car (cdr elt)))
				(or bufferp
				    (setq column
					  (+ column
					     (length (car elt))
					     (length (car (cdr elt)))))))
			    (progn
			      (princ elt)
			      (or bufferp
				  (setq column (+ column (length
							  elt))))))
			  (add-list-mode-item
			   start
			   (progn
			     (setq end (point))
			     (or
			      (and completion-highlight-first-word-only
				   (goto-char start)
				   (re-search-forward regexp-string end t)
				   (match-beginning 0))
			      end))
			   nil cl-activate-callback cl-user-data)
			  (goto-char end)))
		      (setq tail2 (cdr tail2)
			    c (1+ c)))
		    (setq tail (nthcdr cols tail)
			  r (1+ r)))))))))
      (if bufferp
	  (set-buffer old-buffer)))
    (save-excursion
      (let ((mainbuf (or cl-reference-buffer (current-buffer))))
	(set-buffer standard-output)
	(completion-list-mode)
	(make-local-variable 'completion-reference-buffer)
	(setq completion-reference-buffer mainbuf)
;;; The value 0 is right in most cases, but not for file name completion.
;;; so this has to be turned off.
;;;      (setq completion-base-size 0)
	(goto-char (point-min))
	(let ((buffer-read-only nil))
	  (insert (eval cl-help-string)))
	  ;; unnecessary FSFmacs crock
	  ;;(forward-line 1)
	  ;;(while (re-search-forward "[^ \t\n]+\\( [^ \t\n]+\\)*" nil t)
	  ;;	  (let ((beg (match-beginning 0))
	  ;;		(end (point)))
	  ;;	    (if completion-fixup-function
	  ;;		(funcall completion-fixup-function))
	  ;;	    (put-text-property beg (point) 'mouse-face 'highlight)
	  ;;	    (put-text-property beg (point) 'list-mode-item t)
	  ;;	    (goto-char end)))))
	))
    (save-excursion
      (set-buffer standard-output)
      (run-hooks 'completion-setup-hook))))



(defun icicle-highlight-complete-input ()
  "Highlight minibuffer input, showing that it is a sole completion.
Overlay `icicle-complete-input-overlay' is created with `match' face,
unless it exists."
  (let ((case-fold-search completion-ignore-case)
        input-start-position)
    (save-excursion
      (goto-char (icicle-minibuffer-prompt-end))
      (setq input-start-position (point))
      (when (and (icicle-file-name-input-p) insert-default-directory)
        (search-forward (icicle-file-name-directory-w-default (icicle-minibuffer-contents)))
        (setq input-start-position (point))) ; Skip directory.
      (if icicle-complete-input-overlay ; Don't recreate if exists.
          (move-overlay icicle-complete-input-overlay
                        input-start-position (point-max) (current-buffer))
        (setq icicle-complete-input-overlay (make-overlay input-start-position (point-max)))
        (overlay-put icicle-complete-input-overlay 'face 'icicle-complete-input)))))

(defun icicle-call-then-update-Completions (fn &rest args)
  "Call FN with ARGS, then update *Completions* with input matches."
  (apply fn args)
  (setq icicle-current-input (icicle-minibuffer-contents))
  (when (and icicle-incremental-completion-p
             (or (get-buffer-window "*Completions*" 0) ; Already displayed
                 (not (eq t icicle-incremental-completion-p))))
    (let ((icicle-icompleting-p t))
      (setq this-command (if (eq 'icicle-prefix-complete icicle-last-completion-command)
                             'icicle-prefix-complete
                           'icicle-apropos-complete))
      (funcall this-command))))


;; Apparently, this is needed if the initial value is non-nil.
;; Otherwise, the lighter shows the mode as on, but it is not on.
(if icicle-mode (icicle-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-xmas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-xmas.el ends here
