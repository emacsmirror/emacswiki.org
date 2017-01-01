;;; lacarte.el --- Execute menu items as commands, with completion.
;;
;; Filename: lacarte.el
;; Description: Execute menu items as commands, with completion.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2005-2017, Drew Adams, all rights reserved.
;; Created: Fri Aug 12 17:18:02 2005
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 10:38:34 2017 (-0800)
;;           By: dradams
;;     Update #: 926
;; URL: http://www.emacswiki.org/lacarte.el
;; Doc URL: http://www.emacswiki.org/LaCarte
;; Keywords: menu-bar, menu, command, help, abbrev, minibuffer, keys,
;;           completion, matching, local, internal, extensions,
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `subr-21'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Q. When is a menu not a menu?  A. When it's a la carte.
;;
;;  Library La Carte lets you execute menu items as commands, with
;;  completion.  You can use it as an alternative to standard library
;;  `tmm.el'.
;;
;;  Type a menu item.  Completion is available.  Completion candidates
;;  are of the form menu > submenu > subsubmenu > ... > menu item.
;;  For example:
;;
;;    File > Open Recent > Cleanup list
;;    File > Open Recent > Edit list...
;;
;;  When you choose a menu-item candidate, the corresponding command
;;  is executed.
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
;;  (The latter two replace standard bindings for `tmm-menubar'.  On
;;  MS Windows, `f10' is normally bound to `menu-bar-open', which uses
;;  the Windows native keyboard access to menus.)
;;
;;  To really take advantage of La Carte, use it together with
;;  Icicles.  Icicles is not required to be able to use La Carte, but
;;  it enhances the functionality of `lacarte.el' considerably.
;;  (Note: `lacarte.el' was originally called `icicles-menu.el'.)
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
;;    `lacarte-convert-menu-item-function'.
;;
;;  Faces defined here:
;;
;;    `lacarte-shortcut'.
;;
;;  Non-interactive functions defined here:
;;
;;    `lacarte-add-if-menu-item', `lacarte-escape-w32-accel',
;;    `lacarte-get-a-menu-item-alist',
;;    `lacarte-get-a-menu-item-alist-1',
;;    `lacarte-get-a-menu-item-alist-22+',
;;    `lacarte-get-a-menu-item-alist-pre-22',
;;    `lacarte-get-overall-menu-item-alist',
;;    `lacarte-key-description', `lacarte-menu-first-p',
;;    `lacarte-propertize', `lacarte-remove-w32-keybd-accelerators',
;;    `lacarte-string-match-p'.
;;
;;  Internal variables defined here:
;;
;;    `lacarte-history', `lacarte-menu-items-alist'.
;;
;;
;;  Getting Started
;;  ---------------
;;
;;  In your init file (`~/.emacs'), bind `ESC M-x' as suggested above:
;;
;;    (global-set-key [?\e ?\M-x] 'lacarte-execute-command)
;;
;;  Type `ESC M-x' (or `ESC ESC x', which is the same thing).  You are
;;  prompted for a command or menu command to execute.  Just start
;;  typing its name.  Each menu item's full name, for completion, has
;;  its parent menu names as prefixes.
;;
;;  ESC M-x
;;  Command:
;;  Command: t [TAB]
;;  Command: Tools >
;;  Command: Tools > Compa [TAB]
;;  Command: Tools > Compare (Ediff) > Two F [TAB]
;;  Command: Tools > Compare (Ediff) > Two Files... [RET]
;;
;;
;;  Not Just for Wimps and Noobs Anymore
;;  ------------------------------------
;;
;;  *You* don't use menus.  Nah, they're too slow!  Only newbies and
;;  wimps use menus.  Well not any more.  Use the keyboard to access
;;  any menu item, without knowing where it is or what its full name
;;  is.  Type just part of its name and use completion to get the
;;  rest: the complete path and item name.
;;
;;
;;  Commands and Menu Commands
;;  --------------------------
;;
;;  You can bind either `lacarte-execute-menu-command' or
;;  `lacarte-execute-command' to a key such as `ESC M-x'.
;;
;;  `lacarte-execute-menu-command' uses only menu commands.
;;  `lacarte-execute-command' lets you choose among ordinary Emacs
;;  commands, in addition to menu commands.  You can use a prefix arg
;;  with `lacarte-execute-command' to get the same effect as
;;  `lacarte-execute-menu-command'.
;;
;;  Use `lacarte-execute-command' if you don't care whether a command
;;  is on a menu.  Then, if you want a command that affects a buffer,
;;  just type `buf'.  This is especially useful if you use Icicles -
;;  see below.
;;
;;  You can use a prefix arg with `lacarte-execute-menu-command' to
;;  have it offer only items from specific keymaps: the local (major
;;  mode) keymap, the global keymap, or the minor-mode keymaps.
;;
;;  By default, in Icicle mode, `ESC M-x' is bound to
;;  `lacarte-execute-command', and `M-`' is bound to
;;  `lacarte-execute-menu-command'.
;;
;;
;;  Icicles Enhances Dining A La Carte
;;  ----------------------------------
;;
;;  Use Icicles with La Carte to get more power and convenience.
;;
;;  It is Icicles that lets you choose menu items a la carte, in fact.
;;  That is, you can access them directly, wherever they might be in
;;  the menu hierachy.  Without Icicles, you are limited to choosing
;;  items by their menu-hierarchy prefixes, and you must complete the
;;  entire menu prefix to the item, from the top of the menu on down.
;;  With Icicles, you can directly match any parts of a menu item and
;;  its hierarchy path.  Icicles is here:
;;  http://www.emacswiki.org/Icicles.
;;
;;  Type any part of a menu-item, then use the Page Up and Page Down
;;  keys (`prior' and `next') to cycle through all menu commands that
;;  contain the text you typed somewhere in their name.  You can match
;;  within any menu or within all menus; that is, you can match any
;;  part(s) of the menu-hierachy prefix.
;;
;;  You can use `S-TAB' to show and choose from all such "apropos
;;  completions", just as you normally use `TAB' to show all prefix
;;  completions (that is, ordinary completions).  Vanilla, prefix
;;  completion is still available using `TAB', and you can cycle
;;  through the prefix completions using the arrow keys.
;;
;;  You can use Icicles "progressive completion" to match multiple
;;  parts of a menu item separately, in any order.  For example, if
;;  you want a menu command that has to do with buffers and
;;  highlighting, type `buf M-SPC hig S-TAB'.
;;
;;  Icicles apropos completion also lets you type a regular expression
;;  (regexp) - it is matched against all of the possible menu items.
;;  So, for instance, you could type `^e.+buff [next] [next]...' to
;;  quickly cycle to menu command `Edit > Go To > Goto End of Buffer'.
;;  Or type `.*print.*buf S-TAB' to choose from the list of all menu
;;  commands that match `print' followed somewhere by `buf'.
;;
;;  If you know how to use regexps, you can easily and quickly get to
;;  a menu command you want, or at least narrow the list of candidates
;;  for completion and cycling.
;;
;;  Additional benefits of using Icicles with La Carte:
;;
;;  * When you cycle to a candidate menu item, or you complete to one
;;    (entirely), the Emacs command associated with the menu item is
;;    shown in the mode line of buffer `*Completions*'.
;;
;;  * You can use `M-h' to complete your minibuffer input against
;;    commands, including menu-item commands, that you have entered
;;    previously.  You can also use the standard history keys
;;    (e.g. `M-p', `M-r') to access these commands.
;;
;;
;;  Menu Organization Helps You Find a Command
;;  ------------------------------------------
;;
;;  Unlike commands listed in a flat `*Apropos*' page, menu items are
;;  organized, grouped logically by common area of application
;;  (`File', `Edit',...).  This grouping is also available when
;;  cycling completion candidates using Icicles, and you can take
;;  advantage of it to hasten your search for the right command.
;;
;;  You want to execute a command that puts the cursor at the end of a
;;  buffer, but you don't remember its name, what menu it might be a
;;  part of, or where it might appear in that (possibly complex) menu.
;;  With Icicles and La Carte, you type `ESC M-x' and then type
;;  `buffer' at the prompt.  You use the Page Up and Page Down keys to
;;  cycle through all menu items that contain the word `buffer'.
;;
;;  There are lots of such menu items.  But all items from the same
;;  menu (e.g. `File') are grouped together.  You cycle quickly (not
;;  reading) to the `Edit' menu, because you guess that moving the
;;  cursor has more to do with editing than with file operations, tool
;;  use, buffer choice, help, etc.  Then you cycle more slowly among
;;  the `buffer' menu items in the `Edit' menu.  You quickly find
;;  `Edit > Go To > Goto End of Buffer'.  QED.
;;
;;
;;  Learn About Menu Items By Exploring Them
;;  ----------------------------------------
;;
;;  With Icicles, you can display the complete documentation (doc
;;  string) for the command corresponding to each menu item, as the
;;  item appears in the minibuffer.  To do this, just cycle menu-item
;;  candidates using `C-down' or `C-next', instead of `[down]' or
;;  `[next]'.  The documentation appears in buffer `*Help*'.
;;
;;  In sum, if you use La Carte, you will want to use it with Icicles!
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://www.emacswiki.org/emacs/download/linkd.el.
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
          "http://www.emacswiki.org/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/lacarte.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/LaCarte")
  :link '(emacs-commentary-link :tag "Commentary" "lacarte.el"))

;;;###autoload
(defcustom lacarte-convert-menu-item-function nil
  "*Function to call to convert a menu item.
Used by `lacarte-execute-menu-command'.  A typical use would be to
remove the `&' characters used in MS Windows menus to define keyboard
accelerators.  See `lacarte-remove-w32-keybd-accelerators'."
  :type '(choice (const :tag "None" nil) function) :group 'lacarte)

;;;###autoload
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
using face `icicle-special-candidate'."
  (interactive "P")
  (run-hooks 'menu-bar-update-hook)
  (let ((lacarte-menu-items-alist         (lacarte-get-overall-menu-item-alist))
        (completion-ignore-case           t) ; Not case-sensitive, by default.
        ;; ?\000 prevents the key shortcut from being highlighted with face `icicle-special-candidate'.
        (icicle-special-candidate-regexp  (and (not no-commands-p)  ".* > [^?\000]*"))
        (icicle-sort-orders-alist         (and (boundp 'icicle-sort-orders-alist)
                                               (if no-commands-p
                                                   icicle-sort-orders-alist
                                                 (cons (cons "menu items first" 'lacarte-menu-first-p)
                                                       icicle-sort-orders-alist))))
        (icicle-sort-comparer             (and (boundp 'icicle-sort-comparer)  (if no-commands-p
                                                                                   icicle-sort-comparer
                                                                                 'lacarte-menu-first-p)))
        choice cmd)
    (unless no-commands-p
      (mapatoms (lambda (symb)
                  (when (commandp symb)
                    (push (cons (symbol-name symb) symb) lacarte-menu-items-alist)))))
    (setq choice  (completing-read (if no-commands-p "Menu command: " "Command: ")
                                   lacarte-menu-items-alist nil t nil 'lacarte-history)
          cmd     (cdr (assoc choice lacarte-menu-items-alist)))
    (unless cmd (error "No such menu command"))
    ;; Treat special cases of `last-command-event', reconstructing it for
    ;; menu items that get their meaning from the click itself.
    (cond ((eq cmd 'menu-bar-select-buffer)
           (string-match " >\\s-+\\(.+\\)\\s-+\\*?%?\\s-+\\S-*\\s-*$" choice)
           (setq choice  (substring choice (match-beginning 1) (match-end 1)))
           (when (string-match "  \\*?%?" choice)
             (setq choice  (substring choice 0 (match-beginning 0))))
           (setq last-command-event  choice))
          ((eq cmd 'menu-bar-select-yank)
           (string-match "Edit > Select and Paste > \\(.*\\)$" choice)
           (setq last-command-event  (substring choice (match-beginning 1) (match-end 1))))
          ((eq cmd 'menu-bar-select-frame)
           (string-match " >\\s-[^>]+>\\s-+\\(.+\\)$" choice)
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
  "Return non-nil if S1 is a menu item and S2 is not."
  (if (lacarte-string-match-p " > " s1)
      (or (not (lacarte-string-match-p " > " s2))  (string-lessp s1 s2))
    (and (not (lacarte-string-match-p " > " s2))  (string-lessp s1 s2))))

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
         (completion-ignore-case    t)  ; Not case-sensitive, by default.
         (menu-item                 (completing-read "Menu command: " lacarte-menu-items-alist
                                                     nil t nil 'lacarte-history))
         (cmd                       (cdr (assoc menu-item lacarte-menu-items-alist))))
    (unless cmd (error "No such menu command"))
    ;; Treat special cases of `last-command-event', reconstructing it for
    ;; menu items that get their meaning from the click itself.
    (cond ((eq cmd 'menu-bar-select-buffer)
           (string-match " >\\s-+\\(.+\\)\\s-+\\*?%?\\s-+\\S-*\\s-*$"
                         menu-item)
           (setq menu-item  (substring menu-item (match-beginning 1) (match-end 1)))
           (when (string-match "  \\*?%?" menu-item)
             (setq menu-item  (substring menu-item 0 (match-beginning 0))))
           (setq last-command-event  menu-item))
          ((eq cmd 'menu-bar-select-yank)
           (string-match "Edit > Select and Paste > \\(.*\\)$" menu-item)
           (setq last-command-event  (substring menu-item (match-beginning 1) (match-end 1))))
          ((eq cmd 'menu-bar-select-frame)
           (string-match " >\\s-[^>]+>\\s-+\\(.+\\)$" menu-item)
           (setq menu-item           (substring menu-item (match-beginning 1) (match-end 1))
                 last-command-event  menu-item)))
    (call-interactively cmd)))

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
(defun lacarte-add-if-menu-item (event binding root done)
  "Update `lacarte-menu-items-alist' to reflect EVENT and its BINDING.
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
               (concat root (and root  " > ") (eval (cadr bndg))))
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
               (setq composite-name  (concat root (and root  " > ") (eval (cadr bndg))))
               (setq bndg   (car-safe (cdr-safe (cdr-safe bndg)))))
           (setq bndg  nil))))

      ;; (ITEM-STRING . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
      ((stringp (car-safe bndg))
       (setq composite-name  (concat root (and root  " > ") (eval (car bndg))))
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
                                  (and key  (concat (lacarte-propertize "?\000" 'invisible t)
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

(defun lacarte-remove-w32-keybd-accelerators (menu-item)
  "Remove `&' characters that define keyboard accelerators in MS Windows.
\"&&\" is an escaped `&' - it is replaced by a single `&'.
This is a candidate value for `lacarte-convert-menu-item-function'."
  (replace-regexp-in-string "&&?" 'lacarte-escape-w32-accel menu-item))

(defun lacarte-escape-w32-accel (match-string)
  "If STRING is \"&&\", then return \"&\".  Else return \"\"."
  (if (> (length match-string) 1)  "&"  ""))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'lacarte)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lacarte.el ends here
