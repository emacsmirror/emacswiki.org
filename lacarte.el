;;; lacarte.el --- Execute menu items as commands, with completion.  -*- lexical-binding:t -*-
;;
;; Filename: lacarte.el
;; Description: Execute menu items as commands, with completion.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2005-2022, Drew Adams, all rights reserved.
;; Created: Fri Aug 12 17:18:02 2005
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Jan 20 15:04:44 2022 (-0800)
;;           By: dradams
;;     Update #: 1117
;; URL: https://www.emacswiki.org/emacs/download/lacarte.el
;; Doc URL: https://www.emacswiki.org/emacs/LaCarte
;; Keywords: menu-bar, menu, command, help, abbrev, minibuffer, keys,
;;           completion, matching, local, internal, extensions,
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `sortie'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Q. When is a menu not a menu?  A. When it's a la carte.
;;
;;  Library La Carte lets you execute menu-bar menu commands from the
;;  keyboard, with completion.  Use it as an alternative to vanilla
;;  Emacs menu-bar access, including the default `F10' behavior of
;;  `menu-bar-open' and that of standard library `tmm.el'.
;;
;;  Use the keyboard to access any menu item, without knowing where it
;;  is or what its full name is.  Type part of its name and use
;;  completion to get the rest: the complete path and item name.  When
;;  you choose a menu-item candidate, the corresponding command is
;;  executed.
;;
;;  Completion candidates have this form:
;;
;;    menu > submenu > subsubmenu > ... > menu item
;;
;;  For example:
;;
;;    File > Open Recent > Cleanup list
;;    File > Open Recent > Edit list...
;;
;;
;;  Put this in your init file (~/.emacs):
;;
;;    (require 'lacarte)
;;
;;  Suggested key bindings:
;;
;;    (global-set-key [?\e ?\M-x] 'lacarte-execute-command)
;;    (global-set-key [?\M-`]     'lacarte-execute-menu-command)
;;    (global-set-key [f10]       'lacarte-execute-menu-command)
;;
;;  The latter two replace standard bindings for `tmm-menubar' and
;;  `menu-bar-open', respectively.
;;
;;  To really take advantage of La Carte, use it together with
;;  Icicles.  Icicles is not required to be able to use La Carte, but
;;  it enhances the functionality of `lacarte.el' considerably.  By
;;  default in Icicle mode uses those suggested La Carte key bindings.
;;  (Note: `lacarte.el' was originally called `icicles-menu.el'.)
;;  Icicles is here: https://www.emacswiki.org/emacs/Icicles.
;;
;;  If you use MS Windows keyboard accelerators, consider using
;;  `lacarte-remove-w32-keybd-accelerators' as the value of
;;  `lacarte-convert-menu-item-function'.  It removes any unescaped
;;  `&' characters (indicating an accelerator) from the menu items.
;;  One library that adds keyboard accelerators to your menu items is
;;  `menuacc.el', by Lennart Borgman (< l e n n a r t . b o r g m a n
;;  @ g m a i l . c o m >).
;;
;;
;;  Commands defined here:
;;
;;    `lacarte-execute-command', `lacarte-execute-menu-command'.
;;
;;  User options defined here:
;;
;;    `lacarte-convert-menu-item-function',
;;    `lacarte-completion-styles' (Emacs 23+),
;;    `lacarte-default-sort-function', `lacarte-menu-separator'.
;;
;;  Faces defined here:
;;
;;    `lacarte-shortcut'.
;;
;;  Non-interactive functions defined here:
;;
;;    `lacarte-add-if-menu-item', `lacarte-by-depth-p',
;;    `lacarte-by-length-p', `lacarte-collection-function',
;;    `lacarte-depth', `lacarte-escape-w32-accel',
;;    `lacarte-get-a-menu-item-alist',
;;    `lacarte-get-a-menu-item-alist-1',
;;    `lacarte-get-a-menu-item-alist-22+',
;;    `lacarte-get-a-menu-item-alist-pre-22',
;;    `lacarte-get-overall-menu-item-alist',
;;    `lacarte-key-description', `lacarte-menu-first-p',
;;    `lacarte-propertize', `lacarte-read-menu-command.',
;;    `lacarte-remove-w32-keybd-accelerators',
;;    `lacarte-sort-alphabetically',
;;    `lacarte-sort-by-last-use-as-input', `lacarte-sort-by-length',
;;    `lacarte-sort-fn-chooser', `lacarte-sort-menus-first',
;;    `lacarte-string-match-p'.
;;
;;  Internal variables defined here:
;;
;;    `lacarte-completion-keymap', `lacarte-history',
;;    `lacarte-menu-items-alist'.
;;
;;
;;  Getting Started
;;  ---------------
;;
;;  In your init file (`~/.emacs'), bind `ESC M-x' as suggested above:
;;
;;    (global-set-key [?\e ?\M-x] 'lacarte-execute-command)
;;
;;  Type `ESC M-x' (or `ESC ESC x', which is the same thing).  You're
;;  prompted for a command or a menu command to execute.  Each menu
;;  item's full name, for completion, has its parent menu names as
;;  prefixes.
;;
;;
;;  Commands and Menu Commands
;;  --------------------------
;;
;;  Use `lacarte-execute-command' if you don't care whether a command
;;  is on a menu.  Then, if you want a command that affects a buffer,
;;  just type `buf'.
;;
;;  Consider also replacing the standard bindings of `tmm-menu' and
;;  `menu-bar-open':
;;
;;    (global-set-key [?\M-`] 'lacarte-execute-command)
;;    (global-set-key [f10]   'lacarte-execute-command)
;;
;;  `lacarte-execute-menu-command' uses only menu commands.
;;  `lacarte-execute-command' lets you choose among ordinary Emacs
;;  commands, in addition to menu commands.  You can use a prefix arg
;;  with `lacarte-execute-command' to get the same effect as
;;  `lacarte-execute-menu-command'.
;;
;;  You can use a prefix arg with `lacarte-execute-menu-command' to
;;  have it offer only items from specific current menu-bar menus:
;;
;;  * No prefix arg       - all menu-bar menus
;;  * Positive prefix arg - major-mode menu
;;  * Zero prefix arg     - global menus
;;  * Negative prefix arg - minor-mode menus
;;
;;
;;  Completion Candidate Sorting and Cycling
;;  ----------------------------------------
;;
;;  If you also use library `sortie.el', or if you use Icicles, then:
;;
;;  * You can change the sorting of completion candidates on the fly,
;;    with `C-,' (by default).  Available sort orders include by menu
;;    depth, by menu length, by last use as minibuffer input,
;;    alphabetical, and (for `lacarte-execute-command' only) menu
;;    items first.  If you use Icicles then additional sort orders are
;;    available.
;;
;;  * You can cycle among matching candidates.  If you don't use
;;    Icicles then set option `completion-cycle-threshold' to non-nil
;;    to be able to cycle.  With vanilla Emacs (but not with Icicles),
;;    cycling and showing all matching completion candidates are
;;    mutually exclusive - `completion-cycle-threshold' controls
;;    whether and how many candidates to cycle among.
;;
;;
;;  Menu Organization Can Help You Find a Command
;;  ---------------------------------------------
;;
;;  Unlike commands listed in a flat `*Apropos*' page, menu items are
;;  organized - grouped logically by common area of application
;;  (`File', `Edit',...).  This grouping is also available when
;;  cycling among completion candidates (depending on the sort order).
;;
;;  Suppose you want to execute a command that puts the cursor at the
;;  end of a buffer, but you don't remember its name, what menu it
;;  might be a part of, or where it might appear in that (possibly
;;  complex) menu.  You type `ESC M-x' and then type `buffer' at the
;;  prompt.  Then cycle through all menu items that contain the word
;;  `buffer'.
;;
;;  There are lots of such menu items.  But (again, depending on the
;;  sort order) all items from the same menu (e.g. `File') are grouped
;;  together.  You cycle quickly (not reading) to the `Edit' menu,
;;  because you guess that moving the cursor has more to do with
;;  editing than with file operations, tool use, buffer choice, help,
;;  etc.  Then you cycle more slowly among the `buffer' menu items in
;;  the `Edit' menu.  You quickly find `Edit > Go To > Goto End of
;;  Buffer'.
;;
;;  
;;  Completion Matching
;;  -------------------
;;
;;  During completion you can match the separator of menu components,
;;  which is the value of option `lacarte-menu-separator', and which
;;  defaults to ` > '.
;;
;;  As usual (with Emacs 23 or later), you can use completion styles
;;  to control how completion candidates are matched by your
;;  minibuffer input.
;;
;;  If you don't use Icicles then completion matching uses option
;;  `lacarte-completion-styles', not standard option
;;  `completion-styles'.  (Emacs 23 and later only.)  By default this
;;  includes style `flex' (Emacs 27+) or `basic' (Emacs 20-26).  You
;;  can use any list of styles you like, but some might not be so
;;  useful.  The `initials' style, for example, is useless, because it
;;  hardcodes a hyphen (`-') as the separator - see Emacs bug #17559.
;;
;;  If you use Icicles then more powerful matching is available.  You
;;  can of course just use vanilla Emacs matching, with its
;;  `completion-styles'.  But you can also use other matching methods,
;;  including regular-expression matching and various kinds of fuzzy
;;  matching.
;;
;;  With regexp matching (which Icicles calls "apropos" matching) you
;;  can type pattern `^e.+buff', and then quickly cycle to `Edit > Go
;;  To > Goto End of Buffer'.  Or type `.*print.*buf', to choose from
;;  the menu commands that match `print' followed somewhere by `buf'.
;;  And in particular, with regexp matching, you can use `^' and `$'
;;  to directly match the first and last menu components,
;;  respectively.  If you know how to use regexps, you can easily and
;;  quickly get to a menu command you want, or at least narrow the
;;  list of candidates for completion and cycling.
;;
;;  If you use Icicles you can also use progressive completion, which
;;  means matching multiple patterns.  You specify the patterns
;;  progressively, but they are matched in all possible orders.  For
;;  example, if you want a menu command that has to do with buffers
;;  and highlighting, type `buf M-SPC hig S-TAB'.  (`M-SPC', not
;;  `SPC', is the default key for separating match patterns, because
;;  space characters are often part of completion candidates.)
;;
;;  And if you use Icicles you can prune matches not only by adding
;;  patterns to match, but also by adding patterns to NOT match.
;;  (Icicles calls this "chipping away the non-elephant".)
;;
;;  See also:
;;
;;  * https://www.emacswiki.org/emacs/Icicles_-_Progressive_Completion
;;  * https://www.emacswiki.org/emacs/Icicles_-_Nutshell_View#ChippingAway
;;
;;  
;;  Additional Benefits of Using Icicles with La Carte
;;  --------------------------------------------------
;;
;;  * When you cycle to a candidate menu item, or you complete to one
;;    (entirely), the Emacs command associated with the menu item is
;;    shown in the mode-line of buffer `*Completions*'.
;;
;;  * You can use `M-h' to complete your minibuffer input against
;;    commands, including menu-item commands, that you've entered
;;    previously.  (You can of course also use the standard history
;;    keys, such as `M-p' and `M-r', to access these commands.)
;;
;;  * You can display the complete documentation (doc string) for the
;;    command corresponding to each menu item, as the item appears in
;;    the minibuffer.  To do this, just cycle menu-item candidates
;;    using `C-down' or `C-next', instead of `[down]' or `[next]'.
;;    The documentation appears in buffer `*Help*'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  https://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@> "Change log")
;;  (@> "User Options")
;;  (@> "Internal Variables")
;;  (@> "Functions")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;(@* "Change log")
;;
;; 2022/01/20 dadams
;;     Added: lacarte-completion-keymap.
;;            Make SPC self-inserting in it.  Inherit from minibuffer-local-must-match-map.
;; 2022/01/14 dadams
;;     Support lexical-binding and on-demand sorting.  (Emacs 24+)
;;      Soft-reguire sortie.el.
;;      Added: lacarte-by-depth-p, lacarte-by-length-p, lacarte-collection-function,
;;        lacarte-default-sort-function, lacarte-depth, lacarte-menu-separator, lacarte-read-menu-command,
;;        lacarte-sort-alphabetically, lacarte-sort-by-last-use-as-input, lacarte-sort-by-length,
;;        lacarte-sort-fn-chooser, lacarte-sort-menus-first.
;;      lacarte-execute(-menu)-command:
;;        Use lacarte-menu-first-p, lacarte-menu-separator, lacarte-read-menu-command.
;;        Add to icicle-sort-orders-alist: lacarte-by-depth-p, lacarte-by-length-p.
;;      lacarte-add-if-menu-item, lacarte-menu-first-p: Use lacarte-menu-separator.
;;      lacarte-execute-menu-command: Prompt indicates map types.
;; 2021/12/30 dadams
;;     Added lacarte-completion-styles.
;;     lacarte-execute(-menu)-command: Respect lacarte-completion-styles.
;; 2021/12/29 dadams
;;     lacarte-add-if-menu-item: Add ?\000 char only if in Icicle mode.
;; 2014/11/28 dadams
;;     lacarte-get-a-menu-item-alist-22+: Do not try to handle non-keymap as a keymap.
;; 2014/02/01 dadams
;;     Added: lacarte-key-description, lacarte-propertize, face lacarte-shortcut.
;;     lacarte-add-if-menu-item, lacarte-get-a-menu-item-alist-pre-22:
;;       Use lacarte-key-description, not key-description.  Use face lacarte-shortcut for key shortcuts.
;;     lacarte-execute-command: Stop icicle-special-candidate-regexp at ?\000 char (before key shortcut).
;;     lacarte-add-if-menu-item: Add ?\000 char before key shortcut (so not highlighted by Icicles).
;; 2013/07/09 dadams
;;     Updated for recent Emacs versions.  Corrections.
;;       Added: lacarte-add-if-menu-item,lacarte-get-a-menu-item-alist-22+,
;;              lacarte-get-a-menu-item-alist-pre-22.
;;       lacarte-get-a-menu-item-alist-1: defalias to one of lacarte-get-a-menu-item-alist-*22*.
;;     lacarte-execute(-menu)-command: Run menu-bar-update-hook.
;;     lacarte-get-overall-menu-item-alist: Simplified.
;;     lacarte-get-a-menu-item-alist-pre-22: Set composite-name to nil when should not add item.
;;                                           Removed handling of nested keymap (irrelevant for pre-22).
;; 2013/07/08 dadams
;;     lacarte-get-overall-menu-item-alist: Protect using (lookup-key ... [menu-bar]).
;; 2013/07/04 dadams
;;     lacarte-get-a-menu-item-alist-1:
;;       After recursing on nested keymap, set SCAN to its cdr.  Thx to Michael Heerdegen.
;; 2013/06/14 dadams
;;     lacarte-get-a-menu-item-alist-1: Corrected - was cdring twice for atomic car scan.
;; 2012/10/28 dadams
;;     lacarte-get-a-menu-item-alist-1:
;;       Handle Emacs 24+ nested keymap (from multiple-keymap inheritance).
;; 2012/10/15 dadams
;;     lacarte-get-a-menu-item-alist-1: Add entry for separator form (menu-item "--..." . WHATEVER).
;; 2012/09/14 dadams
;;     lacarte-execute-menu-command, lacarte-get-overall-menu-item-alist:
;;       Added prefix arg treatment (arg MAPS), so you can choose keymaps.
;; 2012/09/13 dadams
;;     Added: lacarte-string-match-p.
;;     lacarte-get-overall-menu-item-alist: Use lookup-key, not assq.
;;     lacarte-execute-command: Prepend dotted cons, not two-elt list, for lacarte-menu-first-p entry.
;;     lacarte-menu-first-p: Corrected to sort alphabetically in menus and non-menus.
;; 2011/11/28 dadams
;;     lacarte-get-a-menu-item-alist-1:
;;       Added optional DONE arg, to handle recursive structures.  Thx to Michael Heerdegen.
;; 2011/10/30 dadams
;;     lacarte-get-a-menu-item-alist-1:
;;       Add keys using internal-where-is, not cached key string.  Thx to Michael Heerdegen.
;; 2011/01/04 dadams
;;     Added autoload cookies for defgroup, defcustom, and commands.
;; 2010/06/26 dadams
;;    lacarte-execute-command: Protected Icicles vars with boundp.  Thx to Alexey Romanov.
;; 2010/05/11 dadams
;;     lacarte-get-a-menu-item-alist-1: Add keyboard shortcuts to item names.
;;     Applied Icicles renamings (belatedly):
;;       icicle-sort-functions-alist to icicle-sort-orders-alist,
;;       icicle-sort-function to icicle-sort-comparer.
;; 2009/12/25 dadams
;;     Added: lacarte-execute-command, lacarte-menu-first-p.
;;     lacarte-get-a-menu-item-alist-1: Handle :filter (e.g. File > Open Recent submenus).
;;     lacarte-execute-menu-command:
;;       Just let-bind lacarte-menu-items-alist - don't use unwind-protect.
;;     lacarte-get-overall-menu-item-alist: Reset lacarte-menu-items-alist to nil.
;;     lacarte-get-a-menu-item-alist: Set to the return value.
;; 2009/07/29 dadams
;;     Added: lacarte-history.
;;     lacarte-execute-menu-command:
;;       Use lacarte-history as the history list.  Use strict completion.
;; 2009/07/26 dadams
;;     lacarte-execute-menu-command: Use icicle-interactive-history as the history list.
;; 2008/08/28 dadams
;;     Renamed from alacarte to lacarte.  Confusion with alacarte Ubuntu source package.
;; 2008/05/21 dadams
;;     Renamed library icicles-menu.el to alacarte.el.
;;     alacarte-execute-menu-command: Case-insensitive completion, by default.
;; 2008/05/20 dadams
;;     icicle-get-a-menu-item-alist-1: Don't add non-selectable item to alist.
;; 2006/12/22 dadams
;;     icicle-convert-menu-item-function: Use choice as :type, allowing nil.
;;     :group 'icicles -> :group 'Icicles.
;; 2006/10/16 dadams
;;     icicle-get-overall-menu-item-alist: Include minor-mode keymaps.
;; 2006/03/16 dadams
;;     Added to Commentary.
;; 2006/02/18 dadams
;;     icicle-execute-menu-command: \s -> \\s.  (Thx to dslcustomer-211-74.vivodi.gr.)
;; 2006/01/07 dadams
;;     Added :link for sending bug reports.
;; 2006/01/06 dadams
;;     Changed defgroup to icicles-menu from icicles.
;;     Added :link.
;; 2005/11/08 dadams
;;     icicle-execute-menu-command:
;;       Reset icicle-menu-items-alist in unwind-protect.
;;       Fix for dynamic menus Select and Paste, Buffers, and Frames:
;;         Treat special cases of last-command-event.
;;     icicle-get-overall-menu-item-alist: setq result of sort.
;; 2005/11/05 dadams
;;     Replaced icicle-menu-items with icicle-menu-items-alist (no need for both).
;;     icicle-execute-menu-command: Set, don't bind icicle-menu-items-alist.
;; 2005/08/23 dadams
;;     icicle-execute-menu-command: renamed alist to icicle-menu-items-alist, so can
;;       refer to it unambiguously in icicle-help-on-candidate (in icicles.el).
;; 2005/08/19 dadams
;;     Added: icicle-convert-menu-item-function, icicle-remove-w32-keybd-accelerators,
;;            icicle-escape-w32-accel.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

(unless (fboundp 'replace-regexp-in-string) (require 'subr-21 nil t))

(when (fboundp 'advice-member-p)        ; Emacs 24+
  (require 'sortie nil t)) ;; (no error if not found):
;; sorti-bind-cycle-key-and-complete, sorti-current-order, sorti-sort-function-chooser,
;; sorti-sort-orders-alist, sorti-sort-orders-ring

;; Quiet the byte-compiler (e.g. for older Emacs versions).
(defvar lacarte-completion-styles)
(defvar icicle-mode)
(defvar icicle-sort-comparer)
(defvar icicle-sort-orders-alist)
(defvar sorti-current-order)

;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "User Options")

;;; User Options and Faces ---------------------------------

;;;###autoload
(defgroup lacarte nil
  "Execute menu items as commands, with completion."
  :prefix "lacarte-" :group 'menu
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=
lacarte.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download" "https://www.emacswiki.org/emacs/download/lacarte.el")
  :link '(url-link :tag "Description" "https://www.emacswiki.org/emacs/LaCarte")
  :link '(emacs-commentary-link :tag "Commentary" "lacarte.el"))

;;;###autoload
(defcustom lacarte-convert-menu-item-function nil
  "*Function to call to convert a menu item.
Used by `lacarte-execute-menu-command'.  A typical use would be to
remove the `&' characters used in MS Windows menus to define keyboard
accelerators.  See `lacarte-remove-w32-keybd-accelerators'."
  :type '(choice (const :tag "None" nil) function) :group 'lacarte)

(when (boundp 'completion-styles)       ; Emacs 23+

  (defcustom lacarte-completion-styles `(,(if (assq 'flex completion-styles-alist)
                                              'flex  ; Emacs 27+
                                            'basic)) ; Emacs < 27
;;;     ;; Don't include `initials', as that hard-codes `-' as the separator.
;;;     `(,@(and (assq 'substring completion-styles-alist) ; Emacs 24+
;;;              '(substring))
;;;       ,@(and (assq 'flex completion-styles-alist)
;;;              '(flex))                   ; Emacs 27+
;;;       basic partial-completion emacs22)
    "Value of `completion-styles' used during La Carte commands.
The default value includes only the `flex' style (Emacs 27 and later)
or the `basic' style (prior to Emacs 27)."
    :type '(repeat symbol) :group 'lacarte)
  )

;;;###autoload
(defcustom lacarte-default-sort-function #'lacarte-sort-alphabetically
  "Default sort function for La Carte.
If neither `sortie.el' nor `icicles.el' is used then this is the only
sort function.

The function must accept, as its only argument, a list of string
candidates to sort.  It must return a list of sorted candidates."
  :type 'function :group 'lacarte)

;;;###autoload
(defcustom lacarte-menu-separator ">"
  "String that separates menu or menu-item names in a menu path.
It should neither begin nor end with SPC chars.
The actual separator is this surrounded by one or more SPC chars."
  :type 'string :group 'lacarte)

(defface lacarte-shortcut               ; Same grays as for `shadow'.
    '((((background dark)) (:foreground "gray70"))
      (t (:foreground "gray50")))
  "*Face used to highlight key binding of menu item `*Completions*'."
  :group 'Icicles-Completions-Display :group 'faces)
 
;;; Internal Variables -------------------------------------

(defvar lacarte-history nil "History for menu items read using La Carte completion.")

;; This is used also in `icicle-help-on-candidate', which is defined in Icicles
;; (library `icicles-mcmd.el').
(defvar lacarte-menu-items-alist nil
  "Alist of pairs (MENU-ITEM . COMMAND).
The pairs are defined by the current local and global keymaps.
MENU-ITEM is a menu item, with ancestor-menu prefixes.
  Example: `(\"Files > Insert File...\" . insert-file)'.
COMMAND is the command  bound to the menu item.")
 
;;; Functions -------------------------------

;;;###autoload
(defun lacarte-execute-command (&optional no-commands-p)
  "Execute a menu-bar menu command or an ordinary command.
Type a menu item or a command name.  Completion is available.
With a prefix arg, only menu items are available.
Completion is not case-sensitive.  However, if you use Icicles, then
you can use `C-A' in the minibuffer to toggle case-sensitivity.

If you use Icicles, then you can also sort the completion candidates
in different ways, using `C-,'.  With Icicles, by default menu items
are sorted before non-menu commands, and menu items are highlighted
using face `icicle-special-candidate'.

When called from Lisp, non-nil NO-COMMANDS-P means only menu items are
available."
  (interactive "P")
  (run-hooks 'menu-bar-update-hook)
  (let ((lacarte-menu-items-alist         (lacarte-get-overall-menu-item-alist))
        (completion-styles                lacarte-completion-styles)
        (completion-ignore-case           t) ; Not case-sensitive, by default.
        ;; ?\000 prevents the key shortcut from being highlighted with face `icicle-special-candidate'.
        (icicle-special-candidate-regexp  (and (not no-commands-p)
					       (concat ".* " lacarte-menu-separator " [^?\000]*")))
        (icicle-sort-orders-alist         (and (boundp 'icicle-sort-orders-alist)
                                               (append (and (not no-commands-p)
                                                            '(("menu items first" . lacarte-menu-first-p)))
                                                       '(("by depth"         . lacarte-by-depth-p)
                                                         ("by length"        . lacarte-by-length-p))
                                                       icicle-sort-orders-alist)))
        (icicle-sort-comparer             (and (boundp 'icicle-sort-comparer)
                                               (if no-commands-p
                                                   'icicle-case-string-less-p
                                                 'lacarte-menu-first-p)))
        choice cmd)
    (unless no-commands-p
      (mapatoms (lambda (symb)
                  (when (commandp symb)
                    (push (cons (symbol-name symb) symb) lacarte-menu-items-alist)))))
    (setq choice  (lacarte-read-menu-command (if no-commands-p "Menu command: " "Command: ")
                                             (not no-commands-p))
          cmd     (cdr (assoc choice lacarte-menu-items-alist)))
    (unless cmd (error "No such menu command"))
    ;; Treat special cases of `last-command-event', reconstructing it for
    ;; menu items that get their meaning from the click itself.
    (cond ((eq cmd 'menu-bar-select-buffer)
           (string-match (concat lacarte-menu-separator "\\s-+\\(.+\\)\\s-+\\*?%?\\s-+\\S-*\\s-*$") choice)
           (setq choice  (substring choice (match-beginning 1) (match-end 1)))
           (when (string-match "  \\*?%?" choice)
             (setq choice  (substring choice 0 (match-beginning 0))))
           (setq last-command-event  choice))
          ((eq cmd 'menu-bar-select-yank)
           (string-match "Edit > Select and Paste > \\(.*\\)$" choice)
           (setq last-command-event  (substring choice (match-beginning 1) (match-end 1))))
          ((eq cmd 'menu-bar-select-frame)
           (string-match (concat " " lacarte-menu-separator
				 "\\s-[^" lacarte-menu-separator "]+" lacarte-menu-separator
				 "\\s-+\\(.+\\)$")
                         choice)
           (setq choice              (substring choice (match-beginning 1) (match-end 1))
                 last-command-event  choice)))
    (call-interactively cmd)))

;; Same as `icicle-string-match-p' in `icicles-fn.el'.
(if (fboundp 'string-match-p)
    (defalias 'lacarte-string-match-p 'string-match-p) ; Emacs 23+
  (defun lacarte-string-match-p (regexp string &optional start)
    "Like `string-match', but this saves and restores the match data."
    (save-match-data (string-match regexp string start))))

(defun lacarte-menu-first-p (s1 s2)
  "Return non-nil if S1 is a menu and S2 isn't, or S1 < S2 alphabetically."
  (if (lacarte-string-match-p lacarte-menu-separator s1)
      (or (not (lacarte-string-match-p lacarte-menu-separator s2))  (string-lessp s1 s2))
    (and (not (lacarte-string-match-p lacarte-menu-separator s2))  (string-lessp s1 s2))))

;; Same as `icicle-propertize', in `icicles-fn.el'.
(defun lacarte-propertize (object &rest properties)
  "Like `propertize', but for all Emacs versions.
If OBJECT is not a string, then use `prin1-to-string' to get a string."
  (let ((new  (if (stringp object) (copy-sequence object) (prin1-to-string object))))
    (add-text-properties 0 (length new) properties new)
    new))

(defun lacarte-key-description (keys &optional prefix angles)
  "`icicle-key-description', if Icicles is loaded; else `key-description'.
`icicle-key-description' removes any angle brackets, unless ANGLES is
non-nil."
  (if (fboundp 'icicle-key-description)
      (icicle-key-description keys prefix angles)
    (key-description keys prefix)))

;;;###autoload
(defun lacarte-execute-menu-command (maps)
  "Execute a menu-bar menu command.
Type a menu item.  Completion is available.

A prefix argument controls which menus are available:

* None: current major mode, global, and minor-mode keymaps.
* Positive (including plain `C-u'): current major mode keymap.
* Zero (e.g., `C-0'): current global keymap.
* Negative (e.g., `C--'): current minor mode keymaps.

Completion is not case-sensitive.  However, if you use Icicles, then
you can use `C-A' in the minibuffer to toggle case-sensitivity.
If you use Icicles, then you can also sort the completion candidates
in different ways, using `C-,'."
  (interactive
   (cond ((not current-prefix-arg)                        '((local global minor)))
         ((> (prefix-numeric-value current-prefix-arg) 0) '((local)))
         ((= (prefix-numeric-value current-prefix-arg) 0) '((global)))
         ((< (prefix-numeric-value current-prefix-arg) 0) '((minor)))))
  (run-hooks 'menu-bar-update-hook)
  (let* ((lacarte-menu-items-alist  (lacarte-get-overall-menu-item-alist maps))
         (completion-styles         lacarte-completion-styles)
         (completion-ignore-case    t) ; Not case-sensitive, by default.
         (icicle-sort-orders-alist  (append '(("by depth"  . lacarte-by-depth-p)
                                              ("by length" . lacarte-by-length-p))
                                            (and (boundp 'icicle-sort-orders-alist)
                                                 icicle-sort-orders-alist)))
         (icicle-sort-comparer      'icicle-case-string-less-p)
         (choice                    (lacarte-read-menu-command
                                     (let ((map  (car maps)))
                                       (format "Menu command%s: " (if (cadr maps)
                                                                      ""
                                                                    (if (eq map 'local)
                                                                        " (local)"
                                                                      (if (eq map 'global)
                                                                          " (global)"
                                                                        " (minor modes)")))))))
         (cmd                       (cdr (assoc choice lacarte-menu-items-alist))))
    (unless cmd (error "No such menu command"))
    ;; Treat special cases of `last-command-event', reconstructing it for
    ;; menu items that get their meaning from the click itself.
    (cond ((eq cmd 'menu-bar-select-buffer)
           (string-match (concat lacarte-menu-separator "\\s-*\\(.+\\)\\s-+\\*?%?\\s-+\\S-*\\s-*$") choice)
           (setq choice  (substring choice (match-beginning 1) (match-end 1)))
           (when (string-match "  \\*?%?" choice) (setq choice  (substring choice 0 (match-beginning 0))))
           (setq last-command-event  choice))
          ((eq cmd 'menu-bar-select-yank)
           (string-match "Edit > Select and Paste > \\(.*\\)$" choice)
           (setq last-command-event  (substring choice (match-beginning 1) (match-end 1))))
          ((eq cmd 'menu-bar-select-frame)
           (string-match (concat " " lacarte-menu-separator
				 "\\s-[^" lacarte-menu-separator "]+" lacarte-menu-separator
				 "\\s-+\\(.+\\)$")
                         choice)
           (setq choice              (substring choice (match-beginning 1) (match-end 1))
                 last-command-event  choice)))
    (call-interactively cmd)))

(defvar lacarte-completion-keymap nil
  "Completion keymap used by La Carte.
`minibuffer-local-must-match-map', but with `SPC' self-inserting.")
(unless lacarte-completion-keymap
  (let ((map  (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'self-insert-command)
    ;; (define-key map (kbd "?")   'self-insert-command)
    (set-keymap-parent map minibuffer-local-must-match-map)
    (setq lacarte-completion-keymap  map)))

(defun lacarte-read-menu-command (prompt &optional nonmenu-ok-p)
  "Read a menu command with completion.
Optional NONMENU-OK-P means include nonmenu commands as candidates."
  (let ((minibuffer-local-must-match-map  lacarte-completion-keymap))
    (if (or (and (boundp 'icicle-mode)  icicle-mode)
            (not (featurep 'sortie)))
        ;; Icicles sorting or no sorting.
        (completing-read prompt lacarte-menu-items-alist nil t nil 'lacarte-history)
      (minibuffer-with-setup-hook #'sorti-bind-cycle-key-and-complete
        (let ((sorti-current-order          'order1)
              (sorti-sort-function-chooser  'lacarte-sort-fn-chooser)
              (sorti-sort-orders-alist      `(,@(and nonmenu-ok-p  '((order0 . "menu items first")))
                                              (order1 . "alphabetical")
                                              (order2 . "by depth")
                                              (order3 . "by length")
                                              (order4 . "by last use as input")))
              (sorti-sort-orders-ring       (let ((rng  (make-ring (if nonmenu-ok-p 5 4))))
                                              (ring-insert rng 'order4)
                                              (ring-insert rng 'order3)
                                              (ring-insert rng 'order2)
                                              (ring-insert rng 'order1)
                                              (when nonmenu-ok-p (ring-insert rng 'order0))
                                              rng)))
          (completing-read prompt (lacarte-collection-function lacarte-menu-items-alist)
                           nil t nil 'lacarte-history))))))

(defun lacarte-collection-function (candidates)
  "La Carte collection function, which provides metadata for sorting.
Sorting is per the current value of `lacarte-sort-fn-chooser'.
Sorting is available only if library `sortie.el' is used."
  (if (and (boundp 'lexical-binding)  lexical-binding)
      (lambda (string pred action)
        (if (eq action 'metadata)
            (let ((order  (lacarte-sort-fn-chooser)))
              `(metadata ,@(and order
                                `((display-sort-function . ,order)
                                  (cycle-sort-function   . ,order)))))
          (complete-with-action action candidates string pred)))
    `(lambda (string pred action)
       (if (eq action 'metadata)
           (let ((order  (lacarte-sort-fn-chooser)))
             `(metadata ,@(and order
                               `((display-sort-function . ,order)
                                 (cycle-sort-function   . ,order)))))
         (complete-with-action action ',candidates string pred)))))

;;; Don't bother with `case' or `cl-case'.  Too much trouble to support for multiple Emacs releases.
;;;
;;; (eval-when-compile (or (require 'cl-lib nil t) (require 'cl))) ;; case
;;;
;;; (defun lacarte-sort-fn-chooser ()
;;;   "Return sort function for current value of `sorti-current-order'."
;;;   (case (and (boundp 'sorti-current-order)  sorti-current-order)
;;;     (order0 'lacarte-menu-first-p)
;;;     (order1 'lacarte-sort-alphabetically)
;;;     (order2 'lacarte-sort-by-depth)
;;;     (order3 'lacarte-sort-by-length)
;;;     (order4 'lacarte-sort-by-last-use-as-input)
;;;     (t      lacarte-default-sort-function)))

(defun lacarte-sort-fn-chooser ()
  "Return sort function for current value of `sorti-current-order'."
  (let ((order  (and (boundp 'sorti-current-order)  sorti-current-order)))
    (cond ((eq order 'order0) 'lacarte-sort-menus-first)
          ((eq order 'order1) 'lacarte-sort-alphabetically)
          ((eq order 'order2) 'lacarte-sort-by-depth)
          ((eq order 'order3) 'lacarte-sort-by-length)
          ((eq order 'order4) 'lacarte-sort-by-last-use-as-input)
          (t lacarte-default-sort-function))))

(defun lacarte-sort-menus-first (candidates)
  "Sort CANDIDATES that are menus before those that are command names.
And sort the menus and the nonmenus alphabetically.
This is used only if you use library `sortie.el'."
  (let ((cands  (copy-sequence candidates))) (sort cands #'lacarte-menu-first-p)))

(defun lacarte-sort-alphabetically (candidates)
  "Sort CANDIDATES alphabetically.
This is used only if you use library `sortie.el'."
  (let ((cands  (copy-sequence candidates))) (sort cands #'string<)))

(defun lacarte-sort-by-length (candidates)
  "Sort CANDIDATES by their length.
This is used only if you use library `sortie.el'."
  (let ((cands  (copy-sequence candidates))) (sort cands #'lacarte-by-length-p)))

(defun lacarte-by-length-p (s1 s2)
  "True if length of menu item s1 is less than that of item s2."
  (< (length s1) (length s2)))

(defun lacarte-sort-by-depth (candidates)
  "Sort menu-path CANDIDATES by their depth.
Depth is the number of `lacarte-menu-separator's they contain.
This is used only if you use library `sortie.el'."
  (let ((cands  (copy-sequence candidates))) (sort cands #'lacarte-by-depth-p)))

(defun lacarte-by-depth-p (s1 s2)
  "True if depth of menu item s1 is less than that of item s2."
  (< (lacarte-depth s1) (lacarte-depth s2)))

(defun lacarte-depth (menu-path)
  "Return the number of `lacarte-menu-separator's in `MENU-PATH.
This can return the wrong number if any menu component itself contains
`lacarte-menu-separator'.  This could happen, for example, with a
menu such as `Buffers', where a buffer name could be anything."
  (let ((sep    (regexp-quote lacarte-menu-separator))
	(index  0)
        (count  0))
    (while (string-match sep menu-path index)
      (setq count  (1+ count)
	    index  (match-end 0)))
    count))

(defun lacarte-sort-by-last-use-as-input (candidates)
  "Sort menu-path CANDIDATES by their last use as input.
A previously used candidate sorts before one that's never been used.
any two unused candidates are sorted alphabetically.
This is used only if you use library `sortie.el'."
  (let ((cands  (copy-sequence candidates))) (sort cands #'lacarte-by-last-input-p)))


(defun lacarte-by-last-input-p (s1 s2)
  "True if menu item s1 was input more recently than item s2."
  (let ((hist     (and (symbolp minibuffer-history-variable) (boundp minibuffer-history-variable)
                       (symbol-value minibuffer-history-variable)))
        (s1-tail  ())
        (s2-tail  ()))
    (if (not (consp hist))
        (string-lessp s1 s2)
      (setq s1-tail  (member s1 hist)
            s2-tail  (member s2 hist))
      (cond ((and s1-tail  s2-tail)  (>= (length s1-tail) (length s2-tail)))
            (s1-tail                 t)
            (s2-tail                 nil)
            (t                       (string-lessp s1 s2))))))

(defun lacarte-get-overall-menu-item-alist (&optional maps)
  "Alist formed from menu items in current active keymaps.
See `lacarte-get-a-menu-item-alist' for the alist structure.

Optional argument MAPS is a list specifying which keymaps to use: it
can contain the symbols `local', `global', and `minor', mean the
current local map, current global map, and all current minor maps.

As a side effect, this function modifies `lacarte-menu-items-alist'
temporarily, then resets it to ()."
  (unless maps (setq maps  '(local global minor)))
  (let* ((lacarte-menu-items-alist  lacarte-menu-items-alist)
         (alist
          (lacarte-get-a-menu-item-alist ; This modifies `lacarte-menu-items-alist'.
           (lookup-key
            (cons 'keymap (append (and (memq 'local maps)  (current-local-map))
                                  (apply #'append (and (memq 'minor maps)  (current-minor-mode-maps)))
                                  (and (memq 'global maps)  (current-global-map))))
            [menu-bar]))))
    alist))

(defun lacarte-get-a-menu-item-alist (keymap)
  "Alist of pairs (MENU-ITEM . COMMAND) defined by KEYMAP.
KEYMAP is any keymap that has menu items.
MENU-ITEM is a menu item, with ancestor-menu prefixes.
  Example: `(\"Files > Insert File...\" . insert-file)'.
COMMAND is the command  bound to the menu item.
Returns `lacarte-menu-items-alist' which it modifies."
  (setq lacarte-menu-items-alist  ())
  (lacarte-get-a-menu-item-alist-1 keymap)
  (setq lacarte-menu-items-alist  (nreverse lacarte-menu-items-alist)))

(defalias 'lacarte-get-a-menu-item-alist-1 (if (fboundp 'map-keymap)
                                               'lacarte-get-a-menu-item-alist-22+
                                             'lacarte-get-a-menu-item-alist-pre-22))

(defun lacarte-get-a-menu-item-alist-22+ (keymap &optional root done)
  "Add menu items for KEYMAP to `lacarte-menu-items-alist'.
ROOT is the accumulated part of a menu item so far.
DONE is the alist of accumulated completion candidates so far.
Returns `lacarte-menu-items-alist', which it modifies."
  (when (keymapp keymap)                ; Ignore `nil', in particular.
    (map-keymap (lambda (event binding) (lacarte-add-if-menu-item event binding root done)) keymap))
  lacarte-menu-items-alist)

;;; Free vars here: ROOT, DONE.  Bound in `lacarte-get-a-menu-item-alist'.
(defun lacarte-add-if-menu-item (_event binding root done)
  "Update `lacarte-menu-items-alist' to reflect BINDING.
ROOT is the accumulated part of a menu item so far.
DONE is the alist of accumulated completion candidates so far.
Ignore events that do not belong to menu-bar menus."
  (let ((bndg            binding)
        (composite-name  nil))
    ;; Get REAL-BINDING for the menu item.
    (cond
      ;; (menu-item ITEM-STRING): non-selectable item - skip it.
      ((and (eq 'menu-item (car-safe bndg))  (null (cdr-safe (cdr-safe bndg))))
       (setq bndg  nil))                ; So `keymapp' test, below, fails.
    
      ;; (ITEM-STRING): non-selectable item - skip it.
      ((and (stringp (car-safe bndg))  (null (cdr-safe bndg)))
       (setq bndg  nil))                ; So `keymapp' test, below, fails.
    
      ;; (menu-item "--..." . WHATEVER): separator - skip it.
      ;; Users can use `easy-menu-define' with an item such as ["--" nil], which produces
      ;; (menu-item "--" nil)
      ((and (eq 'menu-item (car-safe bndg))
            (stringp (car-safe (cdr-safe bndg)))
            (string-match "\\`--" (car-safe (cdr-safe bndg))))
       (setq bndg  nil))

      ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES), with `:filter'
      ((and (eq 'menu-item (car-safe bndg))
            (member :filter (cdr (cddr bndg))))
       (let ((filt  (cadr (member :filter (cdr (cddr bndg))))))
         (setq composite-name
               (concat root (and root  (concat " " lacarte-menu-separator " ")) (eval (cadr bndg))))
         ;; Used to concat also the cached key, but Emacs abandoned this in Emacs 23.
         ;; (let ((keys  (car-safe (cdr-safe (cdr-safe (cdr-safe bndg))))))
         ;;  (and (consp keys)  (stringp (cdr keys))  (cdr keys)))))
         (setq bndg  (if (functionp filt) ; Apply the filter to REAL-BINDING.
                         (funcall filt (car (cddr bndg)))
                       (car (cddr bndg))))))

      ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES)
      ((eq 'menu-item (car-safe bndg))
       (let ((enable-condition  (memq ':enable (cdr-safe (cdr-safe (cdr-safe bndg))))))
         (if (or (not enable-condition)
                 (condition-case nil    ; Don't enable if we can't check the condition.
                     (eval (cadr enable-condition))
                   (error nil)))
             (progn
               (setq composite-name  (concat root
                                             (and root  (concat " " lacarte-menu-separator " "))
                                             (eval (cadr bndg))))
               (setq bndg   (car-safe (cdr-safe (cdr-safe bndg)))))
           (setq bndg  nil))))

      ;; (ITEM-STRING . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
      ((stringp (car-safe bndg))
       (setq composite-name  (concat root
                                     (and root  (concat " " lacarte-menu-separator " "))
                                     (eval (car bndg))))
       (setq bndg   (cdr bndg))
       ;; Skip HELP-STRING
       (when (stringp (car-safe bndg)) (setq bndg  (cdr bndg)))
       ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
       (when (and (consp bndg)  (consp (car bndg)))
         ;; Used to use the cached key, but Emacs abandoned this in Emacs 23.
         ;; (when (stringp (cdar bndg))
         ;;   (setq composite-name  (concat composite-name (cdar bndg))))
         (setq bndg  (cdr bndg)))))

    ;; If REAL-BINDING is a keymap then recurse on it.
    (when (keymapp bndg)
      ;; Follow indirections to ultimate symbol naming a command.
      (while (and (symbolp bndg)  (fboundp bndg)  (keymapp (symbol-function bndg)))
        (setq bndg  (symbol-function bndg)))
      (unless (memq bndg done)
        (if (eq 'keymap (car-safe bndg))
            (lacarte-get-a-menu-item-alist-1 bndg composite-name (cons bndg done))
          (lacarte-get-a-menu-item-alist-1 (symbol-function bndg) composite-name (cons bndg done)))))
  
    ;; Add menu item + command pair to `lacarte-menu-items-alist' alist.
    ;; Don't add it if `composite-name' is nil - that's a non-selectable item.
    (when (and root  composite-name  (not (keymapp bndg)))
      (setq lacarte-menu-items-alist
            (cons (cons (concat (if (and (functionp lacarte-convert-menu-item-function)
                                         (stringp composite-name)) ; Could be nil
                                    (funcall lacarte-convert-menu-item-function composite-name)
                                  composite-name)
                                ;; Add key description, if bound to a key.
                                (let ((key  (and bndg  (where-is-internal bndg nil t))))
                                  ;; Hidden ?\000 char to prevent Icicles from highlighting shortcut too.
                                  (and key  (concat (and (fboundp 'icicle-mode)  icicle-mode
                                                         (lacarte-propertize "?\000" 'invisible t))
                                                    (lacarte-propertize
                                                     (format " (%s)" (lacarte-key-description key))
                                                     'face 'lacarte-shortcut)))))
                        bndg)
                  lacarte-menu-items-alist)))))

(defun lacarte-get-a-menu-item-alist-pre-22 (keymap &optional root done)
  "Add menu items for KEYMAP to `lacarte-menu-items-alist'.
ROOT is the accumulated part of a menu item so far.
DONE is the alist of accumulated completion candidates so far.
Returns `lacarte-menu-items-alist', which it modifies."
  (let ((scan            keymap)
        (composite-name  nil))
    (while (consp scan)
      (if (atom (car scan))
          (setq scan  (cdr scan))
        (let ((defn  (cdr (car scan))))
          ;; Get REAL-BINDING for the menu item.
          (cond
            ;; (menu-item ITEM-STRING): non-selectable item - skip it.
            ((and (eq 'menu-item (car-safe defn))
                  (null (cdr-safe (cdr-safe defn))))
             (setq defn            nil
                   composite-name  nil)) ; So we do not add it.

            ;; (ITEM-STRING): non-selectable item - skip it.
            ((and (stringp (car-safe defn))  (null (cdr-safe defn)))
             (setq defn            nil
                   composite-name  nil)) ; So we do not add it.

            ;; (menu-item "--..." . WHATEVER): separator - skip it.
            ;; Users can use `easy-menu-define' with an item such as ["--" nil], which produces
            ;; (menu-item "--" nil)
            ((and (eq 'menu-item (car-safe defn))
                  (stringp (car-safe (cdr-safe defn)))
                  (string-match "\\`--" (car-safe (cdr-safe defn))))
             (setq defn            nil
                   composite-name  nil)) ; So we do not add it.

            ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES), with `:filter'
            ((and (eq 'menu-item (car-safe defn))
                  (member :filter (cdr (cddr defn))))
             (let ((filt  (cadr (member :filter (cdr (cddr defn))))))
               (setq composite-name  (concat root (and root  " > ") (eval (cadr defn))))
               ;; Used to concat also the cached key, but Emacs abandoned this in Emacs 23.
               ;; (let ((keys  (car-safe (cdr-safe (cdr-safe (cdr-safe defn))))))
               ;;  (and (consp keys)  (stringp (cdr keys))  (cdr keys)))))
               (setq defn  (if (functionp filt) ; Apply the filter to REAL-BINDING.
                               (funcall filt (car (cddr defn)))
                             (car (cddr defn))))))

            ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES)
            ((eq 'menu-item (car-safe defn))
             (setq composite-name
                   (concat root (and root  " > ") (eval (cadr defn))))
             ;; Used to concat also the cached key, but Emacs abandoned this in Emacs 23.
             ;; (let ((keys  (car-safe (cdr-safe (cdr-safe (cdr-safe defn))))))
             ;;   (and (consp keys)  (stringp (cdr keys))  (cdr keys)))))
             (setq defn  (car (cddr defn))))

            ;; (ITEM-STRING . REAL-BINDING) or
            ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
            ((stringp (car-safe defn))
             (setq composite-name  (concat root (and root  " > ") (eval (car defn)))
                   defn            (cdr defn))
             ;; Skip HELP-STRING
             (when (stringp (car-safe defn)) (setq defn  (cdr defn)))
             ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
             (when (and (consp defn)  (consp (car defn)))
               ;; Used to use the cached key, but Emacs abandoned this in Emacs 23.
               ;; (when (stringp (cdar defn))
               ;;   (setq composite-name  (concat composite-name (cdar defn))))
               (setq defn  (cdr defn)))))

          ;; If REAL-BINDING is a keymap, then recurse on it.
          (when (keymapp defn)
            ;; Follow indirections to ultimate symbol naming a command.
            (while (and (symbolp defn)  (fboundp defn)  (keymapp (symbol-function defn)))
              (setq defn  (symbol-function defn)))
            (unless (memq defn done)
              (if (eq 'keymap (car-safe defn))
                  (lacarte-get-a-menu-item-alist-1 (cdr defn) composite-name (cons defn done))
                (lacarte-get-a-menu-item-alist-1 (symbol-function defn)
                                                 composite-name
                                                 (cons defn done)))))

          ;; Add menu item + command pair to `lacarte-menu-items-alist' alist.
          ;; Do not add it if COMPOSITE-NAME is nil - that's a non-selectable item.
          ;; Do not add it if DEFN is a keymap.
          (when (and root  composite-name  (not (keymapp defn)))
            (setq lacarte-menu-items-alist
                  (cons
                   (cons (concat (if (and (functionp lacarte-convert-menu-item-function)
                                          (stringp composite-name)) ; Could be nil
                                     (funcall lacarte-convert-menu-item-function composite-name)
                                   composite-name)
                                 ;; Add key description, if bound to a key.
                                 (let ((key  (where-is-internal defn nil t)))
                                   (and key  (lacarte-propertize
                                              (format " (%s)" (lacarte-key-description key))
                                              'face 'lacarte-shortcut))))
                         defn)
                   lacarte-menu-items-alist))))
        (when (consp scan) (setq scan  (cdr scan)))))
    lacarte-menu-items-alist))

(when (fboundp 'replace-regexp-in-string) ; Emacs 22+
  (defun lacarte-remove-w32-keybd-accelerators (menu-item)
    "Remove `&' characters that define keyboard accelerators in MS Windows.
\"&&\" is an escaped `&' - it is replaced by a single `&'.
This is a candidate value for `lacarte-convert-menu-item-function'."
    (replace-regexp-in-string "&&?" 'lacarte-escape-w32-accel menu-item)))

(defun lacarte-escape-w32-accel (match-string)
  "If STRING is \"&&\", then return \"&\".  Else return \"\"."
  (if (> (length match-string) 1)  "&"  ""))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'lacarte)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lacarte.el ends here
