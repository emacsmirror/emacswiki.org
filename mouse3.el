;;; mouse3.el --- Customizable behavior for `mouse-3'.
;;
;; Filename: mouse3.el
;; Description: Customizable behavior for `mouse-3'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2010-2014, Drew Adams, all rights reserved.
;; Created: Tue Nov 30 15:22:56 2010 (-0800)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon Jul 21 13:57:19 2014 (-0700)
;;           By: dradams
;;     Update #: 1742
;; URL: http://www.emacswiki.org/mouse3.el
;; Doc URL: http://www.emacswiki.org/Mouse3
;; Keywords: mouse menu keymap kill rectangle region
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   `naked'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This library lets you customize the behavior of `mouse-3' in
;; several ways.
;;
;; It redefines standard command `mouse-save-then-kill' in a trivial
;; way to give you custom behavior for a second `mouse-3' click at the
;; same spot.
;;
;; Vanilla Emacs hard-wires the behavior to kill or delete the region
;; (depending on the value of `mouse-drag-copy-region').  That action
;; can be handy, but it is sometimes inappropriate (e.g., in a
;; read-only buffer).  In any case, it is only one possible action;
;; there are often other actions on the selected text that you might
;; want to take instead.
;;
;; The redefined `mouse-save-then-kill' command in `mouse3.el' just
;; uses function `mouse3-second-click-command' to handle a second
;; click at the same spot.  That function returns the command that
;; `mouse-save-then-kill' invokes: either the command that is the
;; value of variable `mouse3-save-then-kill-command' or, if that is
;; nil the command that is the value of user option
;; `mouse3-second-click-default-command'.
;;
;; Special contexts can bind variable `mouse3-save-then-kill-command'
;; to provide particular behavior.  For example, Icicles does this
;; during minibuffer completion so that a second `mouse-3' at the same
;; spot in buffer `*Completions*' marks the selected completions
;; (saves them for later reuse).
;;
;; You can use option `mouse3-second-click-default-command' to
;; customize the behavior to either pop up a menu or invoke any
;; command you choose.  The default value is `mouse3-popup-menu',
;; which pops up a menu.  To obtain the vanilla Emacs behavior,
;; customize the option value to command `mouse3-kill/delete-region'.
;;
;; Option `mouse3-double-click-command' associates a command with a
;; `mouse-3' double-click.  The default value is command
;; `mouse3-kill/delete-region', so with the default setup you can kill
;; or delete the selected text by double-clicking `mouse-3' instead of
;; single clicking.  In other words, in the default setup you have two
;; possibilities:
;;
;;  2nd single-click: Pop up a menu so you can choose how to act on
;;                    the selected text.
;;
;;  double-click:     Kill or delete the selection, according to
;;                    standard option `mouse-drag-copy-region'.
;;
;; If you choose to customize one of these two options,
;; `mouse3-second-click-default-command' or
;; `mouse3-double-click-command', then you will probably want to
;; customize both of them.
;;
;; To make either a single-click or a double-click do nothing,
;; customize the appropriate option to the command `ignore'.
;;
;; Note:
;;
;; I do not recommend that you try reversing the option values from
;; their defaults, so that a double-click pops up a menu and the
;; second single-click at the same spot deletes the selected text.
;; That does not work well in general because of a flaw (or a feature,
;; depending on your view) in the Emacs design.
;;
;; Here's the problem: When you double-click to get the menu, Emacs
;; first sends a single-click event, before the double-click event.
;; (That's the flaw/feature.)  So if you double-click at the same spot
;; then the selection is first deleted, before the menu pops up.
;;
;; If you double-click at a different spot then this does not happen,
;; but instead the selection is extended or reduced to match the
;; double-click position.  The selection might then not be the region
;; you want the menu items to act on.  Things work pretty well,
;; however, if you start selecting by double-clicking `mouse-1', click
;; `mouse-3' here and there to extend, and finally double-click
;; `mouse-3' near the end of the selection, because in that case the
;; double-click position does not extend the selection any more.
;;
;; In sum, in general I do not recommend that you reverse the default
;; values.  If you want `mouse-3' to pop up a menu, it is better to
;; either (a) use the default setup so that the menu pops up on the
;; second single-click, not on a double-click or (b) let
;; `mouse3-double-click-command' pop up the menu, but set
;; `mouse3-second-click-default-command' to a command other than
;; `mouse3-kill/delete-region'.
;;
;;
;; Customized Pop-Up Menu
;; ----------------------
;;
;; If you choose for `mouse-3' to pop up a menu then you can customize
;; that menu using various user options.  The command that pops up the
;; menu is `mouse3-popup-menu'.  See the documentation for that
;; command for more explanation of the use of options.
;;
;; A fairly complete default menu is provided out of the box, so you
;; do not need to customize anything unless you want to.
;;
;; `mouse3-popup-menu' ultimately invokes standard function
;; `x-popup-menu' to do its work.  The menu definition used by
;; `x-popup-menu' can take two alternative forms, which are quite
;; different.  Which form you choose to use is controlled by option
;; `mouse3-popup-x-popup-panes-flag'.
;;
;; If that option is nil (the default value), you can use keymaps and
;; extended menu items to define the popup menu.  This is recommended.
;; If the value is non-nil then you can use options
;; `mouse3-region-popup-x-popup-panes' and
;; `mouse3-noregion-popup-x-popup-panes' to make use of the
;; alternative, `x-popup-menu'-specific form.
;;
;; That alternative form is easy to use, but it does not offer you all
;; of the possibilities that a standard menu definition does.  In
;; particular, it does not let you provide keywords such as `:visible'
;; and `:enable' that control the display and makeup of submenus and
;; menu items.  Examples of both methods are provided in this file.
;;
;; The default behavior takes advantage of keywords to dynamically
;; remove or disable submenus and menu items that are inappropriate
;; for the current context.  For example, it removes or disables
;; buffer-modifying items and submenus if the current buffer is
;; read-only.
;;
;; The rest of the description here assumes that
;; `mouse3-popup-x-popup-panes-flag' is nil (the default value and
;; recommended).
;;
;; You can optionally include, at the beginning of the menu, a submenu
;; that has, as its own submenus, the global menus that are currently
;; available.  (These are the same menus that are popped up by
;; `C-mouse-3'.)  This is controlled by option
;; `mouse3-popup-include-global-menus-flag'.  If the menu bar is not
;; shown currently, then these submenus are the menu-bar menus.
;; Otherwise they are the major-mode menus.
;;
;; For example, if the menu bar is showing, then in Dired mode the
;; first submenu is `Dired by name', which itself has submenus
;; `Operate', `Mark', `Regexp', `Immediate', and `Subdir'.
;;
;; You define the rest of the popup menu (other than the global part)
;; using submenu keymaps and `menu-item' bindings (extended menu
;; items).  You do this by customizing options
;; `mouse3-region-popup-entries' and `mouse3-noregion-popup-entries'.
;; You can reuse existing keymaps or create menu items and submenus
;; from scratch.  See the documentation for
;; `mouse3-(no)region-popup-entries'.  If you reuse existing keymaps
;; you can add their menu items either as a submenu or as individual
;; items.
;;
;;
;; Mode-Specific Popup Menu
;; ------------------------
;;
;; You can provide mode-specific behavior by either replacing the
;; default `mouse-3' popup menu or augmenting it with mode-specific
;; entries.
;;
;; `mouse3.el' provides useful behavior for Dired mode.  You can do
;; the same for other modes you use.  Simple example code is also
;; provided here for Picture mode.  The menu implementation for these
;; two modes is different, to give you an idea of what is possible.
;; For Dired mode, `mouse3-region-popup-entries' is used.  For Picture
;; mode, `mouse3-region-popup-x-popup-panes' is used, and both
;; `mouse3-popup-x-popup-panes-flag' and
;; `mouse3-region-popup-x-popup-panes' are made buffer-local.
;;
;; The example code for Picture mode provides actions on the rectangle
;; defined by the region: items such as `Draw Rectangle' and `Clear
;; Rectangle'.
;;
;; Let's look at the behavior for Dired mode in more detail.  The
;; vanilla Emacs behavior just raises an error, because Dired is
;; read-only.  Why not let a second `mouse-3' click at the same spot
;; do something wrt the selected file and dir names?  Two obvious
;; possibilities come to mind: toggle whether each file/dir is marked,
;; or pop up a menu that lets you act in various ways on each of the
;; selected files and dirs.
;;
;; Option `mouse3-dired-function' lets you choose between these two
;; behaviors.  The default value is `mouse3-dired-use-menu', which
;; means to pop up a menu.  This is just like the default popup menu
;; except that it has an additional submenu, `Selected Files', that is
;; Dired-specific.  The first submenu is the global major-mode menu,
;; `Dired by name'.  The second, `Selected Files', has items that act
;; on the files and directories that are selected (in the region):
;;
;;  `Mark'
;;  `Unmark'
;;  `Toggle Marked/Unmarked'
;;  `Flag for Deletion'
;;  `Stop Using Menu'
;;
;; That last item just switches from having `mouse-3' pop up a menu to
;; having it toggle the markings of the selected files (the
;; alternative behavior of `mouse3-dired-function').
;;
;; If you also use library Dired+ (`dired+.el'), which I recommend,
;; then that last menu item is not present, and when the region is
;; empty you get a different popup menu which pertains only to the
;; file where you clicked `mouse-3'.
;;
;;
;; Context-Specific Behavior (for Emacs-Lisp Programmers)
;; ------------------------------------------------------
;;
;; As mentioned above, Icicles provides an example of a program
;; imposing context-specific behavior for the second `mouse-3' click
;; at the same spot.  The context in this case is (a) completion and
;; (b) clicking in buffer `*Completions*'.  For that, Icicles sets a
;; buffer-local value of variable `mouse3-save-then-kill-command'.
;;
;; If you write Emacs-Lisp code, note that this is an example where
;; the text is a set of entries in tabular format (columns).  Each
;; `*Completions*' entry (candidate) is defined by its `mouse-face'
;; property, not by its text.  For example, it is not delimited by
;; whitespace (completion candidates can contain spaces and newlines).
;; A context-specific function picks up the set of selected
;; completions as a list.
;;
;; Similar opportunities can exist for other tabular or line-list
;; data: `*Buffer List*', compile/grep output, Info breadcrumbs,...
;; Use your imagination.  The Dired example is typical here: the
;; region sweeps out text linearly, but the only thing we are really
;; interested in are the file and subdirectory names that are inside
;; the region.
;;
;; -------------------------------------------------------------------
;;
;;
;; User options defined here:
;;
;;   `mouse3-dired-function', `mouse3-double-click-command',
;;   `mouse3-noregion-popup-entries',
;;   `mouse3-noregion-popup-x-popup-panes',
;;   `mouse3-picture-mode-x-popup-panes',
;;   `mouse3-popup-include-global-menus-flag',
;;   `mouse3-popup-x-popup-panes-flag', `mouse3-region-popup-entries',
;;   `mouse3-region-popup-x-popup-panes',
;;   
;;   `mouse3-second-click-default-command'.
;;
;; Commands defined here:
;;
;;   `mouse3-dired', `mouse3-dired-flag-region-files-for-deletion',
;;   `mouse3-dired-mark-region-files', `mouse3-dired-other-window',
;;   `mouse3-dired-toggle-marks-in-region',
;;   `mouse3-dired-toggle-marks-in-region-from-mouse',
;;   `mouse3-dired-unmark-region-files', `mouse3-dired-use-menu',
;;   `mouse3-dired-use-toggle-marks', `mouse3-kill/delete-region',
;;   `mouse3-popup-menu', `mouse3-pp-eval-sexp'.
;;
;; Non-interactive functions defined here:
;;
;;   `mouse3-dired-add-region-menu',
;;   `mouse3-dired-set-to-toggle-marks',
;;   `mouse3-dired-this-file-marked-p',
;;   `mouse3-dired-this-file-unmarked-p',
;;   `mouse3-dired-toggle-marks-in-region', `mouse3-file-or-dir',
;;   `mouse3-nonempty-region-p', `mouse3-region-popup-choice',
;;   `mouse3-region-popup-choice-1',
;;   `mouse3-region-popup-custom-entries',
;;   `mouse3-second-click-command'.
;;
;; Internal variables defined here:
;;
;;   `mouse3-noregion-popup-misc-submenu',
;;   `mouse3-region-popup-change-text-submenu',
;;   `mouse3-region-popup-check-convert-submenu',
;;   `mouse3-region-popup-copy-submenu',
;;   `mouse3-region-popup-highlight-submenu',
;;   `mouse3-region-popup-misc-submenu',
;;   `mouse3-region-popup-print-submenu',
;;   `mouse3-region-popup-rectangle-submenu',
;;   `mouse3-region-popup-register-submenu',
;;   `mouse3-region-popup-remove/replace-items',
;;   `mouse3-region-popup-remove/replace-rect-submenu',
;;   `mouse3-save-then-kill-command'.
;;
;;
;;  ***** NOTE: The following functions defined in `mouse.el' have
;;              been REDEFINED HERE:
;;
;;  `mouse-save-then-kill' - Uses `mouse3-second-click-command' to
;;                           define second `mouse-3' click behavior.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/07/21 dadams
;;     mouse3-region-popup-x-popup-panes:
;;       Added items: String (Insert), Numbers (insert), Rectangular Region.
;; 2014/04/15 dadams
;;     mouse3-noregion-popup-misc-submenu: Update version test for Emacs 24.4 pretest - use version<.
;; 2014/02/26 dadams
;;     mouse3-noregion-popup-misc-submenu, mouse3-noregion-popup-x-popup-panes: Use hlt-(un)highlight-symbol.
;; 2014/02/24 dadams
;;     mouse3-noregion-popup-x-popup-panes, mouse3-noregion-popup-misc-submenu:
;;       Corrected for face-at-point < 24.4.
;; 2014/02/21 dadams
;;     INCOMPATIBLE CHANGE: IF you previously customized one of the options
;;       mouse3-region-popup-include-global-menus-flag, mouse3-region-popup-include-global-menus-flag,
;;       or mouse3-region-popup-x-popup-panes-flag, THEN port your customizations to the options they were
;;       renamed to (same names, but without -region).
;;     Added: mouse3-dired, mouse3-dired-other-window, mouse3-pp-eval-sexp, mouse3-file-or-dir,
;;            mouse3-noregion-popup-entries, mouse3-noregion-popup-misc-submenu,
;;            mouse3-noregion-popup-x-popup-panes.
;;     Renamed: mouse3-region-popup-menu to mouse3-popup-menu,
;;              mouse3-region-popup-include-global-menus-flag to mouse3-popup-include-global-menus-flag
;;              mouse3-region-popup-x-popup-panes-flag to mouse3-popup-x-popup-panes-flag.
;;     mouse3-popup-menu: Use mouse3-popup-custom-entries, to handle also mouse3-noregion-popup-entries.
;; 2014/02/17 dadams
;;     Renamed menu item Open in Browser to Render in Browser.
;; 2014/01/30 dadams
;;     mouse3-region-popup-menu:
;;       Use t, not EVENT as arg to x-popup-menu (see Emacs bug #16565).  Thx to Michael Heerdegen.
;; 2013/11/26 dadams
;;     *-region-popup-x-popup-panes, *-region-popup-copy-submenu, *-region-popup-highlight-submenu:
;;       Added hlt-(copy|yank)-props.
;;     mouse3-region-popup-remove/replace-items: Added hlt-yank-props.
;; 2013/07/24 dadams
;;     mouse3-nonempty-region-p: Simplified and require also transient-mark-mode.
;; 2013/07/23 dadams
;;     mouse3-dired-this-file-(un)marked-p: Use regexp-quote before concat ^ to front of string.
;; 2012/06/18 dadams
;;     mouse3-region-popup-menu: Use format-mode-line on mode-name.
;; 2011/12/19 dadams
;;     mouse3-dired-(un)mark-region-files, mouse3-dired-flag-region-files-for-deletion:
;;       Use line-(beginning|end)-position, not (beginning|end)-of-line + point.
;; 2011/12/09 dadams
;;     Removed mouse3-region-popup-count-submenu.
;;     mouse3-region-popup-x-popup-panes:
;;       Use count-(words|lines)-region.  Use call-interactively and sleep-for.
;;     mouse3-region-popup-misc-submenu: Added count-(words|lines)-region, similarly.
;;     mouse3-region-popup-entries: Removed mouse3-region-popup-count-submenu..
;; 2011/10/07 dadams
;;     Added soft require of naked.el.
;;     mouse3-region-popup-(remove/replace-items|rectangle-submenu): Use naked-key-description if available.
;; 2011/02/25 dadams
;;     mouse3-region-popup-x-popup-panes, mouse3-picture-mode-x-popup-panes:
;;       Distinguish separator choice by making command choice be non-nil.
;;       Removed tests that were ineffectual at compile/load time.
;; 2011/01/07 dadams
;;     Added: mouse3-double-click-command.
;;     mouse3-second-click-default-command: Removed the :set.
;; 2011/01/02 dadams
;;     Changed :group to mouse3 (added).
;;     Removed: mouse3-dired-region-menu (replaced by mouse3-dired-add-region-menu, a complete rewrite).
;;     Added: mouse3-picture-mode-x-popup-panes, mouse3-nonempty-region-p,
;;            mouse3-dired-(set-to-toggle-marks|this-file-(un)marked-p|add-region-menu|function|
;;                          this-file-(un)marked-p|toggle-marks-in-region).
;;     Added: mouse3-region-popup-(
;;              include-global-menus-flag|custom-entries|entries|remove/replace-items|
;;              change-text-submenu|check-convert-submenu|copy-submenu|count-submenu|highlight-submenu|
;;              misc-submenu|print-submenu|rectangle-submenu|register-submenu|remove/replace-rect-submenu|
;;              choice(-1))|x-popup-panes-flag.
;;     Renamed: mouse3-region-popup-submenus to mouse3-region-popup-x-popup-panes.
;;     Renamed: mouse3-dired-toggle-marks-in-region to mouse3-dired-toggle-marks-in-region-from-mouse.
;;       Rewrote to use new mouse3-dired-toggle-marks-in-region, which is a helper borrowed from Dired+.
;;     Renamed: mouse3-picture-mode-submenu to mouse3-picture-mode-x-popup-panes.
;;              Added boundp for picture-killed-rectangle.
;;     mouse3-region-popup-menu:
;;       Rewrote to respect mouse3-region-popup-x-popup-panes-flag and use new menu constants & options.
;;     mouse3-dired-use-menu: Do not remove Dired+ bindings.
;;                            Add mouse3-dired-add-region-menu to dired-after-readin-hook.
;;     mouse3-dired-use-toggle-marks: Do not remove Dired+ bindings.  Do not do any remove-hook.
;;                                    Use dired-after-load-hook, not dired-mode-hook.
;;     mouse3-dired-*-region-files*: Imported new versions from Dired+.
;;     In (picture|org)-mode-hook: Bound mouse3-region-popup-x-popup-panes-flag to t.  Comment-out org stuff.
;; 2010/12/02 dadams
;;     Removed: mouse3-org-region-menu, mouse3-picture-rectangle-menu.
;;     Mode add-hook's: Add mode-specific submenu to mouse3-region-popup-submenus, instead of
;;       setting a local value for mouse3-save-then-kill-command.
;;     mouse-save-then-kill: Emacs 20/21 fix: Always copy as kill (no mouse-drag-copy-region).
;; 2010/11/30 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'naked nil t) ;; (no error if not found): naked-key-description

;; Quiet the byte-compiler.
(defvar picture-killed-rectangle)
(defvar mouse-drag-copy-region)         ; For Emacs < 22.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defgroup mouse3 nil
  "Behaviors for `mouse-3' 2nd click at same spot, including popup menu."
  :prefix "mouse3-"
  :group 'mouse
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=mouse3.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs version and mouse3.el `Update #'."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/mouse3.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Mouse3")
  :link '(emacs-commentary-link :tag "Doc" "mouse3"))

(defun mouse3-nonempty-region-p ()
  "Return non-nil if region is active and non-empty."
  (and transient-mark-mode  mark-active  (> (region-end) (region-beginning))))

(defun mouse3-dired (&optional other-window)
  "Invoke Dired for file or directory named under mouse pointer.
Assumes that there is a file or dir name at that position."
  (interactive)
  (when (listp last-nonmenu-event)      ; Emacs 20, at least: ensure mouse event.
    (save-excursion
      (mouse-set-point last-nonmenu-event)
      (funcall (if other-window #'dired-other-window #'dired)
               (file-name-directory
                (abbreviate-file-name (expand-file-name (mouse3-file-or-dir))))))))

(defun mouse3-dired-other-window ()
  "Same as `mouse3-dired', but use other window."
  (interactive)
  (mouse3-dired 'OTHER-WINDOW))

(defun mouse3-file-or-dir ()
  "Return file or dir name at point.  Raise error if none."
  (let ((guess  (ffap-guesser)))
    (unless (and guess  (or (ffap-file-remote-p guess)
                            (file-directory-p (abbreviate-file-name (expand-file-name guess)))
                            (file-regular-p guess)))
      (error "No file or dir name under mouse pointer"))
    guess))

(defun mouse3-pp-eval-sexp ()
  "Evaluate the Lisp sexp at the mouse pointer and pretty-print the value."
  (interactive)
  (when (listp last-nonmenu-event)      ; Emacs 20, at least: ensure mouse event.
    (pp-eval-expression (save-excursion (mouse-set-point last-nonmenu-event) (sexp-at-point)))
    (sleep-for 3)))

(defvar mouse3-save-then-kill-command nil
  "Command used for a 2nd `mouse-3' click at the same location, or nil.
The command must accept 2 args: mouse click event and prefix arg.
If non-nil, the command is used in priority over the value of user
option `mouse3-second-click-default-command'.  If nil, this var has no
effect on `mouse-3' behavior.")

(defcustom mouse3-second-click-default-command 'mouse3-popup-menu
  "*Command used for a second `mouse-3' click at the same location.
The command must accept 2 args: mouse click event and prefix arg.

This is a default value, which can be programmatically overridden in
various contexts.  This option is used only if variable
`mouse3-save-then-kill-command' is nil.

Two particular values:
 `mouse3-popup-menu': Pop up a menu of actions on the region.
 `mouse3-kill/delete-region': Kill or delete the region, according to
                              `mouse-drag-copy-region'.

See also `mouse3-double-click-command'.  You will probably want to
customize these two options together.  To make either a no-op, set the
value to command `ignore'.

Note that setting `mouse3-double-click-command' to `mouse3-popup-menu'
and `mouse3-second-click-default-command' to
`mouse3-kill/delete-region' is not recommended, because in Emacs a
double-click event is always preceded automatically by the associated
single-click event.  See `(elisp) Repeat Events'."
  :type '(choice
          (const :tag "Menu" :value mouse3-popup-menu)
          (const :tag "Kill/delete, per `mouse-drag-copy-region'" :value mouse3-kill/delete-region)
          (restricted-sexp :tag "Other Command (two args)" :match-alternatives (commandp)))
;;; $$$$$$  :set (lambda (symbol value)
;;;          (set symbol value)
;;;          (if (eq value 'mouse3-popup-menu)
;;;              (global-set-key [double-mouse-3] 'mouse3-kill/delete-region)
;;;            (global-set-key [double-mouse-3] nil)))
  :initialize 'custom-initialize-set
  :group 'mouse3)

(defcustom mouse3-double-click-command 'mouse3-kill/delete-region
  "*Command used for a `mouse-3' double-click.
The command must accept 2 args: mouse click event and prefix arg.

Two particular values:
 `mouse3-popup-menu':  Pop up a menu of actions on the region.
 `mouse3-kill/delete-region': Kill or delete the region, according to
                              `mouse-drag-copy-region'.

See also `mouse3-second-click-default-command'.  You will probably
want to customize these two options together.  To make either a no-op,
set the value to command `ignore'.

Note that setting `mouse3-double-click-command' to `mouse3-popup-menu'
and `mouse3-second-click-default-command' to
`mouse3-kill/delete-region' is not recommended, because in Emacs a
double-click event is always preceded automatically by the associated
single-click event.  See `(elisp) Repeat Events'."
  :type '(choice
          (const :tag "Kill/delete, per `mouse-drag-copy-region'" :value mouse3-kill/delete-region)
          (const :tag "Menu" :value mouse3-popup-menu)
          (restricted-sexp :tag "Other Command (two args)" :match-alternatives (commandp)))
  :set (lambda (symbol value)
         (set symbol value)
         (global-set-key [double-mouse-3] value)
         (global-set-key [left-fringe double-mouse-3] value)
         (global-set-key [right-fringe double-mouse-3] value))
  :initialize 'custom-initialize-set
  :group 'mouse3)

;;;###autoload
(defcustom mouse3-popup-include-global-menus-flag t
  "*Non-nil means `mouse-3' menu includes major-mode or menu-bar menus.
When non-nil:
 If the menu bar is visible then include the major-mode menus.
 Otherwise, include the menu-bar menus.

This option has no effect unless `mouse3-popup-x-popup-panes-flag' is
nil, and it has no effect before Emacs 23."
  :type 'boolean :group 'mouse3)

;;;###autoload
(defcustom mouse3-popup-x-popup-panes-flag nil
  "*Non-nil means use `mouse3-(no)region-popup-x-popup-panes'.
If nil, or if both `mouse3-region-popup-x-popup-panes' and
`mouse3-noregion-popup-x-popup-panes' are nil, then use
`mouse3-region-popup-entries' instead."
  :type 'boolean :group 'mouse3)


;; I guess there is no way to turn stuff off in this case, when buffer is read-only.
;; Another good reason to use the standard format instead.
;;;###autoload
(defcustom mouse3-region-popup-x-popup-panes
  `(("Remove/Replace"
     ,@`(("Kill"                                . kill-region)
         ("Delete"                              . delete-region)
         ("Yank (Replace)"                      . (lambda (start end)
                                                    "Replace selected text by last text killed."
                                                    (interactive "r")
                                                    (when (string= (buffer-substring-no-properties
                                                                    (point) (mark))
                                                                   (car kill-ring))
                                                      (current-kill 1))
                                                    (delete-region start end)
                                                    (yank)))
         ,@(and (fboundp 'hlt-highlight-region) ; Defined in `highlight.el'.
                '(("Yank Copied Text Properties" . hlt-yank-props)))
         ("--")
         ("Kill Rectangle"                      . kill-rectangle)
         ("Delete Rectangle"                    . delete-rectangle)
         ;; This will raise an error if `killed-rectangle' is not defined.
         ("Yank Rectangle (Replace)"
          . (lambda (start end)
              "Replace the selected rectangle by the last rectangle killed."
              (interactive "r")
              (delete-rectangle start end)
              (exchange-point-and-mark)
              (yank-rectangle)))
         ("Clear Rectangle (Replace)"           . clear-rectangle)
         ("String Rectangle (Replace)"          . string-rectangle)
         ("Replace Rectangle from Register"
          . (lambda (start end)
              "Replace the selected rectangle by the contents of a register you name.
Note that the rectangle currently selected is first killed.  You can
restore it by yanking."
              (interactive "r")
              (kill-rectangle start end)
              (exchange-point-and-mark)
              (condition-case nil
                  (call-interactively #'insert-register)
                (error (exchange-point-and-mark) (yank-rectangle)))))))
    ("Copy"
     ("Copy as Kill"                            . kill-ring-save)
     ("Copy to Register"                        . copy-to-register)
     ,@(and (fboundp 'hlt-highlight-region) ; Defined in `highlight.el'.
            '(("Copy Text Properties"           . hlt-copy-props)))
     ("--")
     ("Copy Rectangle to Register"              . copy-rectangle-to-register))
    ("Register"
     ("Copy to..."                              . copy-to-register)
     ("Delete to..."
      . (lambda (register start end)
          "Delete the selected text, and copy it to a register you name."
          (interactive "cDelete region to register: \nr")
          (copy-to-register register start end t)))
     ("Append to..."                            . append-to-register)
     ("Prepend to..."                           . prepend-to-register)
     ("--")
     ("Copy Rectangle to..."                    . copy-rectangle-to-register)
     ("Delete Rectangle to..."
      . (lambda (register start end)
          "Delete the selected rectangle, and copy it to a register you name."
          (interactive "cDelete rectangle to register: \nr")
          (copy-rectangle-to-register register start end t))))
    ("Rectangle"
     ("Kill"                                    . kill-rectangle)
     ("Delete"                                  . delete-rectangle)
     ("Open"                                    . open-rectangle)
     ;; This will raise an error if `killed-rectangle' is not defined.
     ("Yank (Replace)"
      . (lambda (start end)
          "Replace the selected rectangle by the last rectangle killed."
          (interactive "r")
          (delete-rectangle start end)
          (exchange-point-and-mark)
          (yank-rectangle)))
     ("Clear (Replace)"                         . clear-rectangle)
     ("String (Replace)"                        . string-rectangle)
     ,@`,(and (fboundp 'string-insert-rectangle) ; Emacs 24.4+
              '(("String (Insert)"              . string-insert-rectangle)))
     ,@`,(and (fboundp 'rectangle-number-lines) ; Emacs 24.4+
              '(("Numbers (Insert)"             . rectangle-number-lines)))
     ,@`,(and (fboundp 'delimit-columns-rectangle) ; Emacs 21+.
              '(("Delimit Columns"              . delimit-columns-rectangle)))
     ,@`,(and (fboundp 'rectangle-mark-mode) ; Emacs 24.4+
              '(("Rectangular Region"           . rectangle-mark-mode)))
     ("--")
     ("Delete to Register"
      . (lambda (register start end)
          "Delete the selected rectangle, and copy it to a register you name."
          (interactive "cDelete rectangle to register: \nr")
          (copy-rectangle-to-register register start end t)))
     ("Replace from Register"
      . (lambda (start end)
          "Replace the selected rectangle by the contents of a register you name.
Note that the rectangle currently selected is first killed.  You can
restore it by yanking."
          (interactive "r")
          (kill-rectangle start end)
          (exchange-point-and-mark)
          (condition-case nil
              (call-interactively #'insert-register)
            (error (exchange-point-and-mark) (yank-rectangle)))))
     ("Copy to Register"                        . copy-rectangle-to-register))
    ("Change Text"
     ;; These two will appear only if `boxquote.el' was already loaded.
     ,@`,(and (fboundp 'boxquote-region) ; Defined in `boxquote.el'.
              '(("Boxquote"                     . boxquote-region)))
     ,@`,(and (fboundp 'boxquote-unbox-region) ; Defined in `boxquote.el'.
              '(("Unboxquote"                   . boxquote-unbox-region)))

     ,@`,(and (fboundp 'delimit-columns-rectangle) ; Emacs 21+.
              '(("Delimit Columns"              . delimit-columns-region)))
     ,@`,(if (fboundp 'comment-or-uncomment-region)
             '(("Comment/Uncomment"             . comment-or-uncomment-region))
             '(("Comment"                       . comment-region)
               ("Uncomment"                     . uncomment-region)))
     ("--")
     ("Fill"                                    . fill-region)
     ("Fill as Paragraph"                       . fill-region-as-paragraph)
     ("Canonically Space"                       . canonically-space-region)
     ("Indent"                                  . indent-region)
     ("--")
     ("Capitalize"                              . capitalize-region)
     ("Upcase"                                  . upcase-region)
     ("Downcase"                                . downcase-region)
     ;; This will appear only if library `unaccent.el' was already loaded.
     ,@`,(and (fboundp 'unaccent-region) ; Defined in `unaccent.el'.
              '(("Remove Accents"               . unaccent-region)))
     ("--")
     ("Center"                                  . center-region)
     ("Reverse Line Order"                      . reverse-region))
    ("Check, Correct, Convert"
     ("Ispell"                                  . ispell-region)
     ("Flyspell"                                . flyspell-region)
     ,@`,(and (fboundp 'whitespace-cleanup-region) ; Defined in `whitespace.el'.  Emacs 22+.
              '(("Check Whitespace"             . whitespace-report-region)
                ("Clean Up Whitespace"          . whitespace-cleanup-region)))
     ("Printify"                                . printify-region)
     ("PR Printify"                             . pr-printify-region)
     ("Compose Characters"                      . compose-region)
     ("Decompose Characters"                    . decompose-region)
     ("--")
     ("Encode using Coding System"              . encode-coding-region)
     ("Decode using Coding System"              . decode-coding-region)
     ("Encode using Format"                     . format-encode-region)
     ("Decode using Format"                     . format-decode-region)
     ,@`,(and (fboundp 'yenc-decode-region) ; Defined in `yenc.el'.  Emacs 22+.
              '(("Decode Yenc"                  . yenc-decode-region)))
     ("--")
     ("EPA Encrypt"                             . epa-encrypt-region)
     ("EPA Decrypt"                             . epa-decrypt-region)
     ("PGG Encrypt"                             . pgg-encrypt-region)
     ("PGG Decrypt"                             . pgg-decrypt-region))
    ;; This will appear only if library `highlight.el' was already loaded.
    ,@(and (fboundp 'hlt-highlight-region) ; Defined in `highlight.el'.
           '(("Highlight"
              ("Highlight"                      . hlt-highlight-region)
              ("Highlight Regexp"               . hlt-highlight-regexp-region)
              ("Unhighlight"                    . hlt-unhighlight-region)
              ("Unhighlight for Face"           . hlt-unhighlight-region-for-face)
              ("--")
              ("Copy Text Properties"           . hlt-copy-props)
              ("Yank Copied Text Properties"    . hlt-yank-props))))
    ("Print"
     ("PostScript Print"                        . ps-print-region)
     ("PostScript Print with Faces"             . ps-print-region-with-faces)
     ("PostScript Preview"                      . pr-ps-region-preview)
     ("PostScript Number of Pages"              . ps-nb-pages-region)
     ("--")
     ("Print to Text Printer"                   . pr-txt-region)
     ("Print to Text Printer (`lpr')"           . lpr-region)
     ("Print with Paging (`pr')"                . print-region)
     ;; These will appear only if library `ebnf2ps.el' was already loaded.
     ,@`,(and (fboundp 'ebnf-print-region) ; Defined in `ebnf2ps.el'.
              '(("--")
                ("BNF PostScript Analyze"       . ebnf-syntax-region)))
     ,@`,(and (fboundp 'ebnf-print-region) ; Defined in `ebnf2ps.el'.
              '(("BNF PostScript Print "        . ebnf-print-region)))
     ,@`,(and (fboundp 'ebnf-print-region) ; Defined in `ebnf2ps.el'.
              '(("BNF PostScript Save"          . ebnf-eps-region))))
    ("Misc"
     ,@`,(and (fboundp 'count-words-region) ; Emacs 24+
              '(("Count Lines, Words, Chars"
                 . (lambda ()
                     (interactive)
                     (call-interactively #'count-words-region)
                     (sleep-for 3)))))
     ,@`,(and (not (fboundp 'count-words-region)) ; Emacs < 24
              '(("Count Lines and Chars"
                 . (lambda ()
                     (interactive)
                     (call-interactively #'count-lines-region)
                     (sleep-for 3)))))
     ("Narrow"                                  . narrow-to-region)
     ("Eval"                                    . eval-region)
     ("Key-Macro on Region Lines"               . apply-macro-to-region-lines)
     ("Shell Command"                           . shell-command-on-region)
     ("Write to File"                           . write-region)
     ;; This will appear only if library `bookmark+-1.el' was already loaded.
     ,@`,(and (fboundp 'bmkp-set-autonamed-regexp-region) ; Defined in `bookmark+-1.el'.
              '(("Create Bookmarks Matching"    . bmkp-set-autonamed-regexp-region)))
     ;; This will appear only if library `bookmark+-lit.el' was already loaded.
     ,@`,(and (fboundp 'bmkp-light-bookmarks-in-region) ; Defined in `bookmark+-lit.el'.
              '(("Highlight Bookmarks"          . bmkp-light-bookmarks-in-region)))
     ,@`,(and (fboundp 'browse-url-of-region) ; Defined in `browse-url.el'.
              '(("Render in Browser"            . browse-url-of-region)))))
  "*Submenus of `mouse-3' `Region' popup menu.
Used only if `mouse3-popup-x-popup-panes-flag' is non-nil.

A list of `x-popup-menu' pane menus, where each has the form
 (TITLE ITEM1 ITEM2...), with each ITEM a string or a cons cell
 (STRING . VALUE).  See `x-popup-menu'.

If you want to use features offered by extended menu items, then do
not use this option.  Instead, set option
`mouse3-popup-x-popup-panes-flag' to nil and use option
`mouse3-region-popup-entries' to define the menu."
  :type '(alist
          :key-type   (string   :tag "Submenu Name")
          :value-type (repeat
                       (choice
                        (cons :tag "Item"
                         (string :tag "Name")
                         ;; This is more correct but gives `mismatch' in Emacs < version 24:
                         ;; (restricted-sexp :tag "Command" :match-alternatives (commandp))
                         (restricted-sexp :tag "Command"
                          :match-alternatives ((lambda (x) (not (null x)))) :value ignore))
                        (list :tag "Separator" (const "--")))))
  :group 'mouse3)

;; I guess there is no way to turn stuff off in this case, when buffer is read-only.
;; Another good reason to use the standard format instead.
(defcustom mouse3-noregion-popup-x-popup-panes
  `(("Thing at Pointer"
     ,@(and (fboundp 'goto-address-find-address-at-point) ; `:visible'
            '(("Send Email" . (lambda ()
                                (interactive)
                                (save-excursion
                                  (when (listp last-nonmenu-event)
                                    (mouse-set-point last-nonmenu-event)
                                    (unless (goto-address-find-address-at-point)
                                      (error "No email address under mouse pointer"))
                                    (goto-address-at-point)))))))
     ("Open URL in Browser" . browse-url-at-mouse)
     ("Visit File" . ffap-at-mouse)
     ("Visit File in Other Window" . (lambda ()
                                       (interactive)
                                       (save-excursion
                                         (when (listp last-nonmenu-event)
                                           (mouse-set-point last-nonmenu-event)
                                           (find-file-other-window (mouse3-file-or-dir))))))
     ("Dired" . mouse3-dired)
     ("Dired in Other Window" . mouse3-dired-other-window)
     ,@(and (fboundp 'describe-file)    ; `:visible' - defined in `help-fns+.el'.
            '(("Describe File" . (lambda ()
                                   (interactive)
                                   (save-excursion
                                     (when (listp last-nonmenu-event)
                                       (mouse-set-point last-nonmenu-event)
                                       (describe-file (mouse3-file-or-dir))))))))
     ("Describe Function" . (lambda ()
                              (interactive)
                              (save-excursion
                                (when (listp last-nonmenu-event)
                                  (mouse-set-point last-nonmenu-event)
                                  (let ((fn  (or (let ((sy  (symbol-at-point))) (and (fboundp sy)  sy))
                                                 (function-called-at-point))))
                                    (unless fn (error "No function name under mouse pointer"))
                                    (describe-function fn))))))
     ("Show Code Defining Function" . (lambda ()
                                        (interactive)
                                        (save-excursion
                                          (when (listp last-nonmenu-event)
                                            (mouse-set-point last-nonmenu-event)
                                            (unless (function-at-point)
                                              (error "No function name under mouse pointer"))
                                            (find-function-at-point)))))
     ("Describe Variable" . (lambda ()
                              (interactive)
                              (save-excursion
                                (when (listp last-nonmenu-event)
                                  (mouse-set-point last-nonmenu-event)
                                  (let ((var  (variable-at-point)))
                                    (when (numberp var) (error "No variable name under mouse pointer"))
                                    (describe-variable var))))))
     ("Show Code Defining Variable" . (lambda ()
                                        (interactive)
                                        (save-excursion
                                          (when (listp last-nonmenu-event)
                                            (mouse-set-point last-nonmenu-event)
                                            (when (numberp (variable-at-point))
                                              (error "No variable name under mouse pointer"))
                                            (find-variable-at-point)))))
     ("Describe Face" . (lambda ()
                          (interactive)
                          (save-excursion
                            (when (listp last-nonmenu-event)
                              (mouse-set-point last-nonmenu-event)
                              (let ((face  (or (and (fboundp 'face-at-point)
                                                    (or (condition-case nil
                                                            (face-at-point 'FROM-TEXT-TOO) ; Emacs 24.4+
                                                          (error nil))
                                                        (face-at-point))) ; Emacs 23.1-24.3.
                                               (and (facep (symbol-at-point))
                                                    (symbol-at-point))
                                               (let ((faceprop   (or (get-char-property (point) 'read-face-name)
                                                                     (get-char-property (point) 'face))))
                                                 (cond ((facep faceprop) faceprop)
                                                       ((and (listp faceprop)
                                                             ;; Don't treat an attribute spec as a list of faces.
                                                             (not (keywordp (car faceprop)))
                                                             (not (memq (car faceprop)
                                                                        '(foreground-color background-color))))
                                                        (car faceprop)))))))
                                (unless face (error "No face name under mouse pointer"))
                                (describe-face face))))))
     ,@(and (fboundp 'describe-package) ; `:visible'
            '(("Describe Package" . (lambda ()
                                      (interactive)
                                      (save-excursion
                                        (when (listp last-nonmenu-event)
                                          (mouse-set-point last-nonmenu-event)
                                          (let ((pkg  (or (symbol-at-point)
                                                          (function-called-at-point))))
                                            (unless (memq pkg (append (mapcar 'car package-alist)
                                                                      (mapcar 'car package-archive-contents)
                                                                      (mapcar 'car package--builtins)))
                                              (error "No package name under mouse pointer"))
                                            (describe-package pkg))))))))
     ("Describe Text Properties" . (lambda ()
                                     (interactive)
                                     (save-excursion
                                       (when (listp last-nonmenu-event)
                                         (mouse-set-point last-nonmenu-event)
                                         (if (fboundp 'describe-text-properties)
                                             (describe-text-properties (point))
                                           (list-text-properties-at (point)))))))
     ("--")
     ,@(and (fboundp 'hlt-highlight-symbol) ; `:visible'
            '(("Highlight Symbol"  . hlt-highlight-symbol)))
     ,@(and (fboundp 'hlt-unhighlight-symbol) ; `:visible'
            '(("Unhighlight Symbol" . hlt-unhighlight-symbol)))
     ,@(and (fboundp 'hi-lock-face-symbol-at-point) ; `:visible'
            '(("Hi-Lock Symbol" . (lambda ()
                                    (interactive)
                                    (save-excursion
                                      (when (listp last-nonmenu-event)
                                        (mouse-set-point last-nonmenu-event)
                                        (unless (symbol-at-point) (error "No symbol under mouse pointer"))
                                        (hi-lock-face-symbol-at-point)))))))
     ,@(and (fboundp 'hi-lock-unface-buffer) ; `:visible'
            (boundp 'hi-lock-interactive-patterns) ; `:enable'
            '(("Un-Hi-Lock Symbol" . (lambda ()
                                       (interactive)
                                       (save-excursion
                                         (when (listp last-nonmenu-event)
                                           (mouse-set-point last-nonmenu-event)
                                           (unless (symbol-at-point) (error "No symbol under mouse pointer"))
                                           (hi-lock-unface-buffer
                                            (car (assoc (find-tag-default-as-symbol-regexp)
                                                        hi-lock-interactive-patterns)))))))))
     ("--")
     ,@(and (fboundp 'info-lookup-symbol) ; `:visible'
            '(("Look Up Symbol in Manual" . (lambda ()
                                              (interactive)
                                              (save-excursion
                                                (when (listp last-nonmenu-event)
                                                  (mouse-set-point last-nonmenu-event)
                                                  (let ((symb  (symbol-at-point)))
                                                    (unless symb (error "No symbol under mouse pointer"))
                                                    (info-lookup-symbol symb))))))))
     ,@(and (fboundp 'isearch-forward-symbol-at-point) ; `:visible' - Emacs 24.4+
            '(("Search for Symbol" . (lambda ()
                                       (interactive)
                                       (deactivate-mark) ; Must do this first.
                                       (save-excursion
                                         (when (listp last-nonmenu-event)
                                           (mouse-set-point last-nonmenu-event)
                                           (let ((symb  (symbol-at-point)))
                                             (unless symb (error "No symbol under mouse pointer"))
                                             (isearch-forward-symbol-at-point))))))))
     ("Eval & Pretty-Print Lisp Sexp" . mouse3-pp-eval-sexp)))
  "*Submenus of `mouse-3' `No Region' popup menu.
Used only if `mouse3-popup-x-popup-panes-flag' is non-nil.

A list of `x-popup-menu' pane menus, where each has the form
 (TITLE ITEM1 ITEM2...), with each ITEM a string or a cons cell
 (STRING . VALUE).  See `x-popup-menu'.

If you want to use features offered by extended menu items, then do
not use this option.  Instead, set option
`mouse3-popup-x-popup-panes-flag' to nil and use option
`mouse3-noregion-popup-entries' to define the menu."
  :type '(alist
          :key-type   (string   :tag "Submenu Name")
          :value-type (repeat
                       (choice
                        (cons :tag "Item"
                         (string :tag "Name")
                         ;; This is more correct but gives `mismatch' in Emacs < version 24:
                         ;; (restricted-sexp :tag "Command" :match-alternatives (commandp))
                         (restricted-sexp :tag "Command"
                          :match-alternatives ((lambda (x) (not (null x)))) :value ignore))
                        (list :tag "Separator" (const "--")))))
  :group 'mouse3)

(defconst mouse3-region-popup-remove/replace-items ; These are individual menu items: no submenu.
    `((kill           menu-item "Kill"   kill-region
       :visible (and (not buffer-read-only)  (mouse3-nonempty-region-p)))
      (delete         menu-item "Delete" delete-region
       :visible (and (not buffer-read-only)  (mouse3-nonempty-region-p)))
      (yank           menu-item "Yank (Replace)" (lambda (start end)
                                                   "Replace selected text by last text killed."
                                                   (interactive "r")
                                                   (when (string= (buffer-substring-no-properties
                                                                   (point) (mark))
                                                                  (car kill-ring))
                                                     (current-kill 1))
                                                   (delete-region start end)
                                                   (yank))
       :keys ,(if (fboundp 'naked-key-description)
                  (naked-key-description (car (where-is-internal 'yank)))
                  (key-description (car (where-is-internal 'yank)))) ; "C-y"
       :help "Replace selected text by last text killed."
       :visible (not buffer-read-only)
       :enable (and kill-ring  (mouse3-nonempty-region-p)))
      (hlt-yank-props menu-item "Yank Copied Text Properties" hlt-yank-props ; Defined in `highlight.el'.
       :visible (and (not buffer-read-only)  (fboundp 'hlt-yank-props)  hlt-copied-props)))
  "Menu items for removing or replacing the mouse selection.")

(defconst mouse3-region-popup-remove/replace-rect-submenu
    '(remove/replace-rect-menu
      menu-item
      "Remove/Replace Rectangle"
      (keymap
       (kill-rect   menu-item "Kill Rectangle" kill-rectangle)
       (delete-rect menu-item "Delete Rectangle" delete-rectangle)
       (yank-rect   menu-item "Yank Rectangle (Replace)"
        (lambda (start end)
          "Replace the selected rectangle by the last rectangle killed."
          (interactive "r")
          (delete-rectangle start end)
          (exchange-point-and-mark)
          (yank-rectangle))
        :help "Replace the selected rectangle by the last rectangle killed."
        :visible (and (boundp 'killed-rectangle)  killed-rectangle))
       (clear-rect  menu-item "Clear Rectangle (Replace)" clear-rectangle)
       (string-rect menu-item "String Rectangle (Replace)" string-rectangle)
       (yank-rect   menu-item "Replace Rectangle from Register"
        (lambda (start end)
          "Replace the selected rectangle by the contents of a register you name.
Note that the rectangle currently selected is first killed.  You can
restore it by yanking."
          (interactive "r")
          (kill-rectangle start end)
          (exchange-point-and-mark)
          (condition-case nil
              (call-interactively #'insert-register)
            (error (exchange-point-and-mark) (yank-rectangle))))
        "Replace selected rectangle by a register you name.  The rectangle is killed."))
      ;; Disable this submenu if you cannot edit the buffer or the region is empty.
      :enable (and (not buffer-read-only)  (mouse3-nonempty-region-p)))
  "Submenu for removing or replacing the rectangle selected by the mouse.")

(defconst mouse3-region-popup-copy-submenu
    '(copy-menu
      menu-item
      "Copy"
      (keymap
       (copy-as-kill          menu-item "Copy as Kill" kill-ring-save)
       (copy-to-register      menu-item "Copy to Register" copy-to-register)
       (sep-copy-props "--")
       (hlt-copy-props        menu-item "Copy Text Properties" hlt-copy-props
        :visible (fboundp 'hlt-copy-props)) ; Defined in `highlight.el'.
       (hlt-yank-props        menu-item "Yank Copied Text Properties" hlt-yank-props
        :visible (fboundp 'hlt-yank-props) ; Defined in `highlight.el'.
        :enable (and (not buffer-read-only)  hlt-copied-props))
       (sep-copy-register "--")
       (copy-rect-to-register menu-item "Copy Rectangle to Register" copy-rectangle-to-register))
      :enable (mouse3-nonempty-region-p)) ; Disable this submenu if the region is empty.
  "Submenu for copying the mouse selection.")

(defconst mouse3-region-popup-register-submenu
    '(to-register-menu
      menu-item
      "Register"
      (keymap
       (copy                       menu-item "Copy to..." copy-to-register)
       (delete                     menu-item "Delete to..."
        (lambda (register start end)
          "Delete the selected text, and copy it to a register you name."
          (interactive "cDelete region to register: \nr")
          (copy-to-register register start end t))
        :visible (not buffer-read-only)
        :help "Delete the selected text, and copy it to a register that you name.")
       (append                     menu-item "Append to..." append-to-register)
       (prepend                    menu-item "Prepend to..." prepend-to-register)
       (sep-to-register "--")
       (copy-rectangle-to-register menu-item "Copy Rectangle to..." copy-rectangle-to-register)
       (copy-rectangle-to-register menu-item "Delete Rectangle to..."
        (lambda (register start end)
          "Delete the selected rectangle, and copy it to a register that you name."
          (interactive "cDelete rectangle to register: \nr")
          (copy-rectangle-to-register register start end t))
        :visible (not buffer-read-only)
        :help "Delete the selected rectangle, and copy it to a register that you name."))
      :enable (mouse3-nonempty-region-p)) ; Disable this submenu if the region is empty.
  "Submenu for putting the mouse selection in a register.")

(defconst mouse3-region-popup-rectangle-submenu
    `(rectangle-menu
      menu-item
      "Rectangle"
      (keymap
       (kill-rectangle               menu-item "Kill"   kill-rectangle   :visible (not buffer-read-only))
       (delete-rectangle             menu-item "Delete" delete-rectangle :visible (not buffer-read-only))
       (open-rectangle               menu-item "Open"   open-rectangle   :visible (not buffer-read-only))
       (yank-rectangle               menu-item "Yank (Replace)"
        (lambda (start end)
          "Replace the selected rectangle by the last rectangle killed."
          (interactive "r")
          (delete-rectangle start end)
          (exchange-point-and-mark)
          (yank-rectangle))
        :enable (and (boundp 'killed-rectangle)  killed-rectangle)
        :visible (not buffer-read-only)
        :keys ,(if (fboundp 'naked-key-description)
                   (naked-key-description (car (where-is-internal 'yank-rectangle)))
                   (key-description (car (where-is-internal 'yank-rectangle)))) ; "<M-S-insert>"
        :help "Replace the selected rectangle by the last rectangle killed.")
       (clear-rectangle              menu-item "Clear (Replace)"  clear-rectangle
        :visible (not buffer-read-only))
       (string-rectangle             menu-item "String (Replace)" string-rectangle
        :visible (not buffer-read-only))
       (string-insert-rect           menu-item "String (Insert)" string-insert-rectangle
        :visible (fboundp 'string-insert-rectangle)) ; Emacs 24.4+
       (rect-number-lines            menu-item "Numbers (Insert)" rectangle-number-lines
        :visible (fboundp 'rectangle-number-lines)) ; Emacs 24.4+
       (delimit-columns-rectangle    menu-item "Delimit Columns"  delimit-columns-rectangle
        :visible (and (fboundp 'delimit-columns-rectangle)  (not buffer-read-only))) ; Emacs 21+.
       (rectangle-mark-mode          menu-item "Rectangular Region"  rectangle-mark-mode
        :visible (fboundp 'rectangle-mark-mode)) ; Emacs 24.4+.
       (sep-rectangle                menu-item "--" nil :visible (not buffer-read-only))
       (delete-rectangle-to-register menu-item "Delete to Register"
        (lambda (register start end)
          "Delete the selected rectangle, and copy it to a register you name."
          (interactive "cDelete rectangle to register: \nr")
          (copy-rectangle-to-register register start end t))
        :visible (not buffer-read-only)
        :help "Delete the selected rectangle, and copy it to a register you name.")
       (yank-rectangle               menu-item "Replace from Register"
        (lambda (start end)
          "Replace the selected rectangle by the contents of a register you name.
Note that the rectangle currently selected is first killed.  You can
restore it by yanking."
          (interactive "r")
          (kill-rectangle start end)
          (exchange-point-and-mark)
          (condition-case nil
              (call-interactively #'insert-register)
            (error (exchange-point-and-mark) (yank-rectangle))))
        :visible (not buffer-read-only)
        :help "Replace selected rectangle with register you name (rect. is killed).")
       ;; This is the only item that should show when the buffer is read-only.
       (copy-rectangle-to-register   menu-item "Copy to Register" copy-rectangle-to-register))
      :enable (mouse3-nonempty-region-p)) ; Disable this submenu if the region is empty.
  "Submenu for operations on the rectangle selected by the mouse.")

(defconst mouse3-region-popup-change-text-submenu
    '(change-text-menu
      menu-item
      "Change Text"
      (keymap
       (boxquote-region             menu-item "Boxquote" boxquote-region ; Defined in `boxquote.el'.
        :visible (fboundp 'boxquote-region))
       (boxquote-unbox-region       menu-item "Unboxquote" boxquote-unbox-region ; Defined in `boxquote.el'.
        :visible (fboundp 'boxquote-unbox-region))
       (delimit-columns-region      menu-item "Delimit Columns" delimit-columns-region
        :visible (fboundp 'delimit-columns-rectangle)) ; Emacs 21+.
       (comment-or-uncomment-region menu-item "Comment/Uncomment" comment-or-uncomment-region
        :visible (fboundp 'comment-or-uncomment-region))
       (comment-region              menu-item "Comment" comment-region
        :visible (not (fboundp 'comment-or-uncomment-region)))
       (uncomment-region            menu-item "Uncomment" uncomment-region
        :visible (not (fboundp 'comment-or-uncomment-region)))
       (sep-fill "--")
       (fill                        menu-item "Fill" fill-region)
       (fill-as-para                menu-item "Fill as Paragraph" fill-region-as-paragraph)
       (canonically-space           menu-item "Canonically Space" canonically-space-region)
       (indent                      menu-item "Indent" indent-region)
       (sep-word-case "--")
       (capitalize                  menu-item "Capitalize" capitalize-region)
       (upcase                      menu-item "Upcase" upcase-region)
       (downcase                    menu-item "Downcase" downcase-region)
       (unaccent-region             menu-item "Remove Accents" unaccent-region
        :visible (fboundp 'unaccent-region))
       (sep-lines "--")
       (center-region               menu-item "Center" center-region)
       (reverse-region              menu-item "Reverse Line Order" reverse-region))
      ;; Disable this submenu if you cannot edit the buffer or the region is empty.
      :enable (and (not buffer-read-only)  (mouse3-nonempty-region-p)))
  "Submenu for operations on the mouse selection that change the text.")

(defconst mouse3-region-popup-check-convert-submenu
    '(check-convert-menu
      menu-item "Check, Correct, Convert"
      (keymap
       (ispell-region             menu-item "Ispell" ispell-region)
       (flyspell-region           menu-item "Flyspell" flyspell-region)
       (whitespace-report-region  menu-item "Check Whitespace" whitespace-report-region
        :visible (fboundp 'whitespace-cleanup-region))
       (whitespace-cleanup-region menu-item "Clean Up Whitespace" whitespace-cleanup-region
        :visible (and (fboundp 'whitespace-cleanup-region)  (not buffer-read-only)))
       (printify-region           menu-item "Printify" printify-region
        :visible (not buffer-read-only))
       (pr-printify-region        menu-item "PR Printify" pr-printify-region
        :visible (not buffer-read-only))
       (compose-region            menu-item "Compose Characters" compose-region
        :visible (not buffer-read-only))
       (decompose-region          menu-item "Decompose Characters" decompose-region
        :visible (not buffer-read-only))
       (sep-encode                menu-item "--" nil
        :visible (not buffer-read-only))
       (encode-coding-region      menu-item "Encode using Coding System" encode-coding-region
        :visible (not buffer-read-only))
       (decode-coding-region      menu-item "Decode using Coding System" decode-coding-region
        :visible (not buffer-read-only))
       (format-encode-region      menu-item "Encode using Format" format-encode-region
        :visible (not buffer-read-only))
       (format-decode-region      menu-item "Decode using Format" format-decode-region
        :visible (not buffer-read-only))
       (yenc-decode-region        menu-item "Decode Yenc" yenc-decode-region
        :visible (and (fboundp 'yenc-decode-region)  (not buffer-read-only)))
       (sep-encrypt               menu-item "--" nil :visible (not buffer-read-only))
       (epa-encrypt-region        menu-item "EPA Encrypt" epa-encrypt-region
        :visible (not buffer-read-only))
       (epa-decrypt-region        menu-item "EPA Decrypt" epa-decrypt-region
        :visible (not buffer-read-only))
       (pgg-encrypt-region        menu-item "PGG Encrypt" pgg-encrypt-region
        :visible (not buffer-read-only))
       (pgg-decrypt-region        menu-item "PGG Decrypt" pgg-decrypt-region
        :visible (not buffer-read-only)))
      :enable (mouse3-nonempty-region-p)) ; Disable this submenu if the region is empty.
  "Submenu for operations that check, correct, or convert mouse-selection text.")

(defconst mouse3-region-popup-highlight-submenu
    '(hlt-highlight-menu
      menu-item "Highlight"
      (keymap
       (hlt-highlight-region            menu-item "Highlight" hlt-highlight-region)
       (hlt-highlight-regexp-region     menu-item "Highlight Regexp" hlt-highlight-regexp-region)
       (hlt-unhighlight-region          menu-item "Unhighlight" hlt-unhighlight-region)
       (hlt-unhighlight-region-for-face menu-item "Unhighlight for Face" hlt-unhighlight-region-for-face)
       (sep-copy-props "--")
       (hlt-copy-props                  menu-item "Copy Text Properties" hlt-copy-props
        :visible (fboundp 'hlt-copy-props)) ; Defined in `highlight.el'.
       (hlt-yank-props                  menu-item "Yank Copied Text Properties" hlt-yank-props
        :visible (fboundp 'hlt-yank-props) ; Defined in `highlight.el'.
        :enable (and (not buffer-read-only)  hlt-copied-props)))
      :enable (mouse3-nonempty-region-p)) ; Disable this submenu if the region is empty.
  "Submenu for highlighting or unhighlighting text in the mouse selection.")

(defconst mouse3-region-popup-print-submenu
    '(print-menu
      menu-item "Print"
      (keymap
       (ps-print-region            menu-item "PostScript Print" ps-print-region)
       (ps-print-region-with-faces menu-item "PostScript Print with Faces" ps-print-region-with-faces)
       (pr-ps-region-preview       menu-item "PostScript Preview" pr-ps-region-preview)
       (ps-nb-pages-region         menu-item "PostScript Number of Pages" ps-nb-pages-region)
       (sep-print "--")
       (pr-txt-region              menu-item "Print to Text Printer" pr-txt-region)
       (lpr-region                 menu-item "Print to Text Printer (`lpr')" lpr-region)
       (sep-bnf "--" :visible (fboundp 'ebnf-print-region))
       (ebnf-syntax-region         menu-item "BNF PostScript Analyze" ebnf-syntax-region
        :visible (fboundp 'ebnf-print-region))
       (ebnf-print-region          menu-item "BNF PostScript Print " ebnf-print-region
        :visible (fboundp 'ebnf-print-region))
       (ebnf-eps-region            menu-item "BNF PostScript Save" ebnf-eps-region
        :visible (fboundp 'ebnf-print-region)))
      :enable (mouse3-nonempty-region-p)) ; Disable this submenu if the region is empty.
  "Submenu for printing the mouse selection.")

(defconst mouse3-region-popup-misc-submenu
    '(misc-menu
      menu-item "Misc"
      (keymap
       (count-words-region menu-item "Count Lines, Words, Chars"
        (lambda ()
          (interactive)
          (call-interactively #'count-words-region)
          (sleep-for 3))
        :visible (fboundp 'count-words-region)) ; Emacs 24+
       (count-lines-region menu-item "Count Lines and Chars"
        (lambda ()
          (interactive)
          (call-interactively #'count-lines-region)
          (sleep-for 3))
        :visible (not (fboundp 'count-words-region))) ; Emacs < 24
       (narrow-to-region            menu-item "Narrow" narrow-to-region)
       (eval-region                 menu-item "Eval" eval-region)
       (apply-macro-to-region-lines menu-item "Key-Macro on Region Lines" apply-macro-to-region-lines
        :enable last-kbd-macro
        :visible (not buffer-read-only))
       (shell-command-on-region     menu-item "Shell Command" shell-command-on-region)
       (write-region                menu-item "Write to File" write-region)
       (bmkp-set-autonamed-regexp   menu-item "Create Bookmarks Matching" bmkp-set-autonamed-regexp-region
        :visible (fboundp 'bmkp-set-autonamed-regexp-region)) ; Defined in `bookmark+-1.el'.
       (bmkp-light-bookmarks        menu-item "Highlight Bookmarks" bmkp-light-bookmarks-in-region
        :visible (fboundp 'bmkp-light-bookmarks-in-region)) ; Defined in `bookmark+-lit.el'.
       (browse-url-of-region        menu-item "Render in Browser" browse-url-of-region
        :visible (fboundp 'browse-url-of-region))) ; Defined in `browse-url.el'.
      :enable (mouse3-nonempty-region-p)) ; Disable this submenu if the region is empty.
  "Submenu for miscellaneous operations on the mouse selection.")

(defconst mouse3-noregion-popup-misc-submenu
    '(misc-menu
      menu-item "Thing at Pointer"
      (keymap
       (send-email menu-item "Send Email" goto-address-at-point
        :enable (save-excursion
                  (mouse-set-point last-nonmenu-event)
                  (goto-address-find-address-at-point))
        :visible (fboundp 'goto-address-find-address-at-point))
       (browse-url menu-item "Open URL in Browser" browse-url-at-mouse
        :enable (save-excursion
                  (mouse-set-point last-nonmenu-event)
                  (if (or (> emacs-major-version 24) ; Do not use `browse-url-url-at-point'.
                          (and (= emacs-major-version 24)  (not (version< emacs-version "24.3.50"))))
                      (thing-at-point 'url t)
                    (thing-at-point 'url))))
       (find-file menu-item "Visit File" ffap-at-mouse
        :enable (condition-case nil (mouse3-file-or-dir) (error nil)))
       (find-file-other menu-item "Visit File in Other Window" (lambda ()
                                                                 (interactive)
                                                                 (save-excursion
                                                                   (mouse-set-point last-nonmenu-event)
                                                                   (find-file-other-window (ffap-guesser))))
        :enable (condition-case nil (mouse3-file-or-dir) (error nil)))
       (dired menu-item "Dired" mouse3-dired
        :enable (condition-case nil (mouse3-file-or-dir) (error nil)))
       (dired-other menu-item "Dired in Other Window" mouse3-dired-other-window
        :enable (condition-case nil (mouse3-file-or-dir) (error nil)))
       (sep-describe "--")
       (describe-file menu-item "Describe File" (lambda ()
                                                  (interactive)
                                                  (save-excursion
                                                    (mouse-set-point last-nonmenu-event)
                                                    (describe-file (ffap-guesser))))
        :enable (condition-case nil (mouse3-file-or-dir) (error nil))
        :visible (fboundp 'describe-file)) ; Defined in `help-fns+.el'.
       (describe-function menu-item "Describe Function" (lambda ()
                                                          (interactive)
                                                          (save-excursion
                                                            (mouse-set-point last-nonmenu-event)
                                                            (describe-function
                                                             (or (let ((sy  (symbol-at-point)))
                                                                   (and (fboundp sy)  sy))
                                                                 (function-called-at-point)))))
        :enable (or (fboundp (symbol-at-point))  (function-called-at-point)))
       (find-function menu-item "Show Code Defining Function" (lambda ()
                                                                (interactive)
                                                                (save-excursion
                                                                  (mouse-set-point last-nonmenu-event)
                                                                  (find-function-at-point)))
        :enable (function-called-at-point))
       (describe-variable menu-item "Describe Variable" (lambda ()
                                                          (interactive)
                                                          (save-excursion
                                                            (mouse-set-point last-nonmenu-event)
                                                            (describe-variable (variable-at-point))))
        :enable (not (numberp (variable-at-point))))
       (find-variable menu-item "Show Code Defining Variable" (lambda ()
                                                                (interactive)
                                                                (save-excursion
                                                                  (mouse-set-point last-nonmenu-event)
                                                                  (find-variable-at-point)))
        :enable (not (numberp (variable-at-point))))
       (describe-face menu-item "Describe Face"
        (lambda ()
          (interactive)
          (save-excursion
            (mouse-set-point last-nonmenu-event)
            (describe-face
             (or (and (fboundp 'face-at-point)  (or (condition-case nil
                                                        (face-at-point 'FROM-TEXT-TOO) ; Emacs 24.4+
                                                      (error nil))
                                                    (face-at-point))) ; Emacs 23.1-24.3.
                 (and (facep (symbol-at-point))
                      (symbol-at-point))
                 (let ((faceprop   (or (get-char-property (point) 'read-face-name)
                                       (get-char-property (point) 'face))))
                   (cond ((facep faceprop) faceprop)
                         ((and (listp faceprop)
                               ;; Don't treat an attribute spec as a list of faces.
                               (not (keywordp (car faceprop)))
                               (not (memq (car faceprop) '(foreground-color background-color))))
                          (car faceprop))))))))
        :enable (or
                 (and (fboundp 'face-at-point)  (or (condition-case nil
                                                        (face-at-point 'FROM-TEXT-TOO) ; Emacs 24.4+
                                                      (error nil))
                                                 (face-at-point))) ; Emacs 23.1-24.3.
                 (and (facep (symbol-at-point))  (symbol-at-point))
                 (let ((faceprop   (or (get-char-property (point) 'read-face-name)
                                       (get-char-property (point) 'face))))
                   (cond ((facep faceprop) faceprop)
                         ((and (listp faceprop)
                               ;; Don't treat an attribute spec as a list of faces.
                               (not (keywordp (car faceprop)))
                               (not (memq (car faceprop) '(foreground-color background-color))))
                          (car faceprop))))))
       (describe-package menu-item "Describe Package" (lambda ()
                                                        (interactive)
                                                        (save-excursion
                                                          (mouse-set-point last-nonmenu-event)
                                                          (and (fboundp 'describe-package)
                                                               (describe-package
                                                                (or (symbol-at-point)
                                                                    (function-called-at-point))))))
        :enable (and
                 (boundp 'package--initialized)
                 package--initialized
                 (let ((guess  (or (symbol-at-point)  (function-called-at-point))))
                   (memq guess (append (mapcar 'car package-alist) (mapcar 'car package-archive-contents)
                                       (mapcar 'car package--builtins)))))
        :visible (fboundp 'describe-package))
       (describe-text-properties menu-item "Describe Text Properties"
        (lambda ()
          (interactive)
          (save-excursion
            (mouse-set-point last-nonmenu-event)
            (if (fboundp 'describe-text-properties)
                (describe-text-properties (point))
              (list-text-properties-at (point))))))
       (sep-highlight "--")
       (hlt-highlight menu-item "Highlight Symbol" hlt-highlight-symbol
        :enable (symbol-at-point)
        :visible (fboundp 'hlt-highlight-symbol)) ; In `highlight.el'.
       (hlt-unhighlight menu-item "Unhighlight Symbol" hlt-unhighlight-symbol
        :enable (symbol-at-point)
        :visible (fboundp 'hlt-unhighlight-symbol)) ; In `highlight.el'.
       (hi-lock-symbol menu-item "Hi-Lock Symbol"  (lambda ()
                                                     (interactive)
                                                     (save-excursion
                                                       (mouse-set-point last-nonmenu-event)
                                                       (hi-lock-face-symbol-at-point)))
        :visible (fboundp 'hi-lock-face-symbol-at-point))
       (un-hi-lock-symbol menu-item "Un-Hi-Lock Symbol" (lambda ()
                                                          (interactive)
                                                          (save-excursion
                                                            (mouse-set-point last-nonmenu-event)
                                                            (hi-lock-unface-buffer
                                                             (car (assoc (find-tag-default-as-symbol-regexp)
                                                                         hi-lock-interactive-patterns)))))
        :enable (and (boundp 'hi-lock-interactive-patterns)
                 (save-excursion (mouse-set-point last-nonmenu-event)
                                 (assoc (find-tag-default-as-symbol-regexp)
                                        hi-lock-interactive-patterns)))
        ;; Do not use `hi-lock-unface-buffer' here.  It is defined in releases before `*-at-point' is defined.
        :visible (fboundp 'hi-lock-face-symbol-at-point))
       (sep-misc "--")
       (info-lookup-symbol menu-item "Look Up Symbol in Manual" (lambda ()
                                                                  (interactive)
                                                                  (save-excursion
                                                                    (mouse-set-point last-nonmenu-event)
                                                                    (info-lookup-symbol (symbol-at-point))))
        :enable (symbol-at-point)
        :visible (fboundp 'info-lookup-symbol))
       (isearch-for-symbol menu-item "Search for Symbol" (lambda ()
                                                           (interactive)
                                                           (deactivate-mark) ; Must do this first.
                                                           (save-excursion
                                                             (mouse-set-point last-nonmenu-event)
                                                             (isearch-forward-symbol-at-point)))
        :enable (find-tag-default-bounds)
        :visible (fboundp 'isearch-forward-symbol-at-point))
       (eval-sexp menu-item "Eval & Pretty-Print Lisp Sexp" mouse3-pp-eval-sexp
        :enable (or (derived-mode-p 'emacs-lisp-mode)  (derived-mode-p 'lisp-mode)))
       )
      :enable (not (mouse3-nonempty-region-p))) ; Enable this submenu if the region is empty.
  "Submenu for miscellaneous operations when mouse selection is empty.")

(defcustom mouse3-region-popup-entries `(,@mouse3-region-popup-remove/replace-items
                                         (sep1-std-entries menu-item "--" nil
                                          :visible (not buffer-read-only))
                                         ,mouse3-region-popup-remove/replace-rect-submenu
                                         ,mouse3-region-popup-copy-submenu
                                         ,mouse3-region-popup-register-submenu
                                         ,mouse3-region-popup-rectangle-submenu
                                         ,mouse3-region-popup-change-text-submenu
                                         ,mouse3-region-popup-check-convert-submenu
                                         ,(and (fboundp 'hlt-highlight-region)
                                               mouse3-region-popup-highlight-submenu)
                                         ,mouse3-region-popup-print-submenu
                                         ,mouse3-region-popup-misc-submenu
                                         )
  "*Entries for `mouse-3' popup menu when region is active and nonempty.
The option value is a list.  Each element defines a submenu or a menu
item.  A null element (`nil') is ignored.

Used only if `mouse3-popup-x-popup-panes-flag' is nil.

If `mouse3-popup-include-global-menus-flag' is non-nil then a global
submenu is added at the beginning of the popup menu, before the
entries from `mouse3-region-popup-entries'.

Several alternative entry formats are available.  When customizing,
choose an alternative in the Customize `Value Menu'.

In this description:
 SYMBOL      is a symbol identifying the menu entry.
 `menu-item' is just that text, literally.
 NAME        is a string naming the menu item or submenu.
 COMMAND     is the command to be invoked by an item.
 MENU-KEYMAP is a menu keymap or a var whose value is a menu keymap.
 KEYWORDS    is a property list of menu keywords (`:enable',
             `:visible', `:filter', `:keys', etc.).

1. Single menu item.  For a selectable item, use
   (SYMBOL menu-item NAME COMMAND . KEYWORDS).  For a non-selectable
   item such as a separator, use (SYMBOL NAME) or
   (SYMBOL menu-item NAME nil . KEYWORDS).

2. Items taken from a menu-keymap variable, such as
   `menu-bar-edit-menu'.  Just use the name of the variable (a
   symbol).  The items appear at the top level of the popup menu, not
   in a submenu.

3. Submenu.  Use (SYMBOL menu-item NAME MENU-KEYMAP . KEYWORDS) or
   (SYMBOL NAME . MENU-KEYMAP).  Remember that MENU-KEYMAP can also be
   a variable (symbol) whose value is a menu keymap.

All of these are standard menu elements, with the exception of the use
of a keymap variable to represent its value.

See also:
 * (elisp) Format of Keymaps
 * (elisp) Classifying Events
 * (elisp) Extended Menu Items

Example submenu element:
 (edit menu-item \"Edit\" menu-bar-edit-menu)

Example selectable menu-item element:
 (kill menu-item \"Kill\"   kill-region
       :visible (and (not buffer-read-only)
                     (mouse3-nonempty-region-p)))

\(`mouse3-nonempty-region-p' returns non-nil if the region is active
and not empty.)"
  ;; Could define this `:type' and so reuse the definition for both `*-region-*' and `*-noregion-*'.
  :type  '(repeat
           (choice
            ;; These could be combined, but it's better for users to see separate choices.
            (restricted-sexp
             :tag "Submenu (SYMBOL menu-item NAME MENU-KEYMAP . KEYWORDS) or (SYMBOL NAME . MENU-KEYMAP)"
             :match-alternatives
             ((lambda (x)
                (and (consp x)  (symbolp (car x))
                     (or (and (stringp (cadr x))  (cddr x)) ; (SYMBOL NAME . MENU-KEYMAP)
                         ;; (SYMBOL menu-item NAME MENU-KEYMAP . KEYWORDS)
                         (and (eq 'menu-item (cadr x))
                              (stringp (car (cddr x)))
                              (or (keymapp  (car (cdr (cddr x)))) ; Can be a keymap var.
                                  (and (symbolp (car (cdr (cddr x))))
                                       (boundp (car (cdr (cddr x))))
                                       (keymapp (symbol-value (car (cdr (cddr x)))))))))))
              'nil))
            (restricted-sexp
             :tag "Items from a keymap variable's value."
             :match-alternatives ((lambda (x) (and (symbolp x)  (keymapp (symbol-value x))))
                                  'nil))
            (restricted-sexp
             :tag "Selectable item (SYMBOL menu-item NAME COMMAND . KEYWORDS)"
             :match-alternatives ((lambda (x) (and (consp x)  (symbolp (car x))
                                                   (eq 'menu-item (cadr x))
                                                   (stringp (car (cddr x)))
                                                   (commandp (car (cdr (cddr x))))))
                                  'nil))
            (restricted-sexp
             :tag "Non-selectable item (SYMBOL NAME) or (SYMBOL menu-item NAME nil . KEYWORDS)"
             :match-alternatives ((lambda (x) (and (consp x)  (symbolp (car x))
                                                   (or (and (stringp (cadr x))  (null (caddr x)))
                                                       (and (eq 'menu-item (cadr x))
                                                            (stringp (car (cddr x)))
                                                            (null (car (cdr (cddr x))))))))
                                  'nil))))
  :group 'mouse3)

(defcustom mouse3-noregion-popup-entries `(,mouse3-noregion-popup-misc-submenu)
  "*Entries for the `mouse-3' popup menu when no nonempty active region.
Other than the use context, this has the same description as
`mouse3-noregion-popup-entries' - which see."
  ;; Could define this `:type' and so reuse the definition for both `*-region-*' and `*-noregion-*'.
  :type  '(repeat
           (choice
            ;; These could be combined, but it's better for users to see separate choices.
            (restricted-sexp
             :tag "Submenu (SYMBOL menu-item NAME MENU-KEYMAP . KEYWORDS) or (SYMBOL NAME . MENU-KEYMAP)"
             :match-alternatives
             ((lambda (x)
                (and (consp x)  (symbolp (car x))
                     (or (and (stringp (cadr x))  (cddr x)) ; (SYMBOL NAME . MENU-KEYMAP)
                         ;; (SYMBOL menu-item NAME MENU-KEYMAP . KEYWORDS)
                         (and (eq 'menu-item (cadr x))
                              (stringp (car (cddr x)))
                              (or (keymapp  (car (cdr (cddr x)))) ; Can be a keymap var.
                                  (and (symbolp (car (cdr (cddr x))))
                                       (boundp (car (cdr (cddr x))))
                                       (keymapp (symbol-value (car (cdr (cddr x)))))))))))
              'nil))
            (restricted-sexp
             :tag "Items from a keymap variable's value."
             :match-alternatives ((lambda (x) (and (symbolp x)  (keymapp (symbol-value x))))
                                  'nil))
            (restricted-sexp
             :tag "Selectable item (SYMBOL menu-item NAME COMMAND . KEYWORDS)"
             :match-alternatives ((lambda (x) (and (consp x)  (symbolp (car x))
                                                   (eq 'menu-item (cadr x))
                                                   (stringp (car (cddr x)))
                                                   (commandp (car (cdr (cddr x))))))
                                  'nil))
            (restricted-sexp
             :tag "Non-selectable item (SYMBOL NAME) or (SYMBOL menu-item NAME nil . KEYWORDS)"
             :match-alternatives ((lambda (x) (and (consp x)  (symbolp (car x))
                                                   (or (and (stringp (cadr x))  (null (caddr x)))
                                                       (and (eq 'menu-item (cadr x))
                                                            (stringp (car (cddr x)))
                                                            (null (car (cdr (cddr x))))))))
                                  'nil))))
  :group 'mouse3)

(defun mouse3-region-popup-custom-entries ()
  "In `mouse3-region-popup-entries', replace keymap vars by their values."
  (let ((new  ()))
    (dolist (jj  mouse3-region-popup-entries)
      (cond ((and (symbolp jj)  (keymapp (symbol-value jj))) ; Just a keymap var.
             (setq jj  (symbol-value jj))
             (dolist (ii  jj) (push ii new)))
            ;; (SYMBOL menu-item NAME MENU-KEYMAP . KEYWORDS), with a keymap var.
            ((and (consp jj)  (symbolp (car jj))  (eq 'menu-item (cadr jj))
                  (stringp (car (cddr jj))) (symbolp (car (cdr (cddr jj))))
                  (not (commandp (car (cdr (cddr jj))))) (boundp (car (cdr (cddr jj))))
                  (keymapp (symbol-value (car (cdr (cddr jj))))))
             (setq jj  `(,(car jj) menu-item ,(car (cddr jj))
                         ,(symbol-value (car (cdr (cddr jj)))) ; Replace keymap var by its value.
                         ,@(cdr (cdr (cddr jj))))) ; Keywords.
             (push jj new))
            ((and (consp jj)  (symbolp (car jj))  (stringp (cadr jj)) ; (SYMBOL NAME . MENU-KEYMAP)
                  (symbolp (cddr jj)) (boundp (cddr jj)) (keymapp (symbol-value (cddr jj))))
             (setq jj  `(,(car jj) ,(cadr jj) ,@(symbol-value (cddr jj)))) ; Replace keymap var by value.
             (push jj new))
            (t (push jj new))))
    (nreverse new)))

(defun mouse3-second-click-command ()
  "Command used for a second `mouse-3' click at the same location.
The command must accept 2 args: mouse click event and prefix arg.
Return the value of `mouse3-save-then-kill-command' if non-nil, else
return the value of `mouse3-second-click-default-command'."
  (or mouse3-save-then-kill-command
      mouse3-second-click-default-command
      'mouse3-kill/delete-region))      ; Should never happen, since option cannot be customized to nil.

;;;###autoload
(defun mouse3-kill/delete-region (event killp)
  "Delete the active region.  Kill it if KILLP is non-nil.
Kill it anyway if `mouse-drag-copy-region' is non-nil.
For Emacs prior to Emacs 22, always kill region."
  (interactive "e\nP")
  (let* ((posn         (event-start event))
         (window       (posn-window posn))
         (buf          (window-buffer window))
         (mark-active  t))              ; Just to be sure.
    (with-current-buffer buf
      (if (or killp (and (boundp 'mouse-drag-copy-region)  mouse-drag-copy-region))
          (kill-region (region-beginning) (region-end))
        (delete-region (region-beginning) (region-end))))))

;;;###autoload
(defun mouse3-popup-menu (event ignored)
  "Pop up a `Region' menu of actions for the selected text.
See options `mouse3-region-popup-entries',
`mouse3-popup-x-popup-panes-flag', and
`mouse3-region-popup-x-popup-panes'.

You have two alternatives, which correspond to the possible values of
option `mouse3-popup-x-popup-panes-flag' (and to the possibilities for
the MENU argument of function `x-popup-menu'):

1. nil (recommended) - Define the menu as a keymap, using submenu
   keymaps and `menu-item' bindings.

2. non-nil - Define the menu using (non-keymap) `x-popup-menu' panes
   lists, `mouse3-region-popup-x-popup-panes' and
   `mouse3-noregion-popup-x-popup-panes'.

The first alternative lets you make use of keywords such as `:enable'
and `:visible', and it lets you reuse existing menu keymaps.

The second alternative allows easy customization of individual menu
items, but it does not let you use menu keywords or reuse existing
keymaps.

For the first alternative, in addition to the items and submenus
supplied by options `mouse3-region-popup-entries' and
`mouse3-noregion-popup-entries', a submenu is added at the beginning
of the popup menu for either (a) the menu-bar menus (if the menu bar
is not visible) or (b) the major-mode menus.  (This is only for Emacs
23+.)"
  (interactive "e\nP")
  (sit-for 0)
  (let* ((menus   (if (and mouse3-popup-x-popup-panes-flag  (or mouse3-region-popup-x-popup-panes
                                                                mouse3-noregion-popup-x-popup-panes))
                      `(,(if (mouse3-nonempty-region-p) "Region" "No Region")
                         ,@(if (mouse3-nonempty-region-p)
                               mouse3-region-popup-x-popup-panes
                               mouse3-noregion-popup-x-popup-panes))

                    `((keymap ,(if (mouse3-nonempty-region-p) "Region" "No Region")

                       ;; Global menus - Emacs 23+ only.
                       ,@(and mouse3-popup-include-global-menus-flag  (fboundp 'mouse-menu-bar-map)
                              (if (zerop (or (frame-parameter nil 'menu-bar-lines) 0))
                                  `((menu-bar-maps "Menu Bar" ,@(mouse-menu-bar-map)))
                                ;; Alternative: a `@' prefix in the name makes Emacs splice in the
                                ;; major-mode menu instead of having a submenu with the major-mode name.
                                ;; `((major-mode-map "@" ,@(mouse-menu-major-mode-map)))
                                `((major-mode-map ,(format-mode-line mode-name)
                                   ,@(mouse-menu-major-mode-map)))))
                       ,@(and mouse3-popup-include-global-menus-flag  (fboundp 'mouse-menu-bar-map)
                              '((sep1-global "--")))

                       ;; Entries from `mouse3-(no)region-popup-entries'.
                       ,@(if (mouse3-nonempty-region-p)
                             (mouse3-popup-custom-entries)
                             (mouse3-popup-custom-entries 'NOREGION))))))
         (choice  (x-popup-menu t menus)))
    (mouse3-region-popup-choice menus choice)))

(defun mouse3-popup-custom-entries (&optional no-region-p)
  "In `mouse3-(no)region-popup-entries', replace keymap vars by their values.
Non-nil NO-REGION-P means use `mouse3-noregion-popup-entries', otherwise use
`mouse3-region-popup-entries'"
  (let ((new  ()))
    (dolist (jj  (if no-region-p mouse3-noregion-popup-entries mouse3-region-popup-entries))
      (cond ((and (symbolp jj)  (keymapp (symbol-value jj))) ; Just a keymap var.
             (setq jj  (symbol-value jj))
             (dolist (ii  jj) (push ii new)))
            ;; (SYMBOL menu-item NAME MENU-KEYMAP . KEYWORDS), with a keymap var.
            ((and (consp jj)  (symbolp (car jj))  (eq 'menu-item (cadr jj))
                  (stringp (car (cddr jj))) (symbolp (car (cdr (cddr jj))))
                  (not (commandp (car (cdr (cddr jj))))) (boundp (car (cdr (cddr jj))))
                  (keymapp (symbol-value (car (cdr (cddr jj))))))
             (setq jj  `(,(car jj) menu-item ,(car (cddr jj))
                         ,(symbol-value (car (cdr (cddr jj)))) ; Replace keymap var by its value.
                         ,@(cdr (cdr (cddr jj))))) ; Keywords.
             (push jj new))
            ((and (consp jj)  (symbolp (car jj))  (stringp (cadr jj)) ; (SYMBOL NAME . MENU-KEYMAP)
                  (symbolp (cddr jj)) (boundp (cddr jj)) (keymapp (symbol-value (cddr jj))))
             (setq jj  `(,(car jj) ,(cadr jj) ,@(symbol-value (cddr jj)))) ; Replace keymap var by value.
             (push jj new))
            (t (push jj new))))
    (nreverse new)))

(defun mouse3-region-popup-choice (menus choice)
  "Invoke the command from MENUS that is represented by user's CHOICE.
MENUS is a list that is acceptable as the second argument for
`x-popup-menu'.  That is, it is one of the following, where MENU-TITLE
is the menu title and PANE-TITLE is a submenu title.

* a keymap - MENU-TITLE is its `keymap-prompt'
* a list of keymaps - MENU-TITLE is the first keymap's `keymap-prompt'
* a menu of multiple panes, which has this form: (MENU-TITLE PANE...),
  where each PANE has this form: (PANE-TITLE ITEM...),
  where each ITEM has one of these forms:
  - STRING - an unselectable menu item
  - (STRING . COMMAND) - a selectable item that invokes COMMAND"
  (catch 'mouse3-region-popup-choice (mouse3-region-popup-choice-1 menus choice)))

(defun mouse3-region-popup-choice-1 (menus choice)
  "Helper function for `mouse3-region-popup-choice'."
  (cond((keymapp menus)
         ;; Look up each ITEM-LIST entry in keymap MENUS.
         ;;   If what is found is a keymap, use that as MENUS for next iteration.
         ;;   If what is found is a command, invoke it (done).
         (let (binding)
           (while choice
             (setq binding  (lookup-key menus (vector (car choice))))
             (cond ((keymapp binding)
                    (setq menus   binding
                          choice  (cdr choice)))
                   ((commandp binding)
                    (throw 'mouse3-region-popup-choice (call-interactively binding))) ; You only get one.
                   (t (error "`mouse3-region-popup-choice', Not a command: %s" binding))))))
        ((consp menus)                  ; A list of keymaps or panes.
         (dolist (menu  menus)
           (if (keymapp menu)
               (mouse3-region-popup-choice-1 menu choice)
             (when choice               ; MENU is a pane.
               (throw 'mouse3-region-popup-choice (call-interactively choice)))))))) ; You only get one.


;; REPLACE ORIGINAL in `mouse.el'.
;;
;; Use `mouse3-second-click-command' to determine the action for a second `mouse-3' click.
;;
;;;###autoload
(defun mouse-save-then-kill (click &optional prefix)
  "Like vanilla `mouse-save-then-kill', but uses `mouse3-second-click-command'."
  (interactive "e\nP")
  (mouse-minibuffer-check click)
  (let* ((posn          (event-start click))
         (click-pt      (posn-point posn))
         (window        (posn-window posn))
         (buf           (window-buffer window))
         (this-command  this-command)   ; Don't let subsequent kill command append to this one.
         ;; Check whether the user has multi-clicked to select words/lines.
         (click-count   (if (and (eq mouse-selection-click-count-buffer buf)
                                 (with-current-buffer buf (mark t)))
                            mouse-selection-click-count
                          0)))
    (cond ((not (numberp click-pt)) nil)
          ((and (eq last-command 'mouse-save-then-kill) ; User clicked without moving point.
                (eq click-pt mouse-save-then-kill-posn)
                (eq window (selected-window)))
           (funcall (mouse3-second-click-command) click prefix)
           (setq mouse-selection-click-count  0
                 mouse-save-then-kill-posn    nil))
          ;; If there is a suitable region, adjust it by moving the closest end to CLICK-PT.
          ((or (with-current-buffer buf (and transient-mark-mode  mark-active))
               (and (eq window (selected-window))
                    (mark t)
                    (or (and (eq last-command 'mouse-save-then-kill)
                             mouse-save-then-kill-posn)
                        (and (memq last-command '(mouse-drag-region mouse-set-region))
                             (or mark-even-if-inactive (not transient-mark-mode))))))
           (select-window window)
           (let* ((range  (mouse-start-end click-pt click-pt click-count)))
             (if (< (abs (- click-pt (mark t))) (abs (- click-pt (point))))
                 (set-mark (car range))
               (goto-char (nth 1 range)))
             (setq deactivate-mark  nil)
             (mouse-set-region-1)
             ;; Previous region was copied to kill-ring, so replace with adjusted region.
             (if (boundp 'mouse-drag-copy-region)
                 (when mouse-drag-copy-region ; Emacs 22+.
                   (kill-new (filter-buffer-substring (mark t) (point)) t))
               (kill-new (buffer-substring (point) (mark t)) t)) ; Emacs 20 & 21.
             (setq mouse-save-then-kill-posn  click-pt))) ; Repeated `mouse-3' kills the region.
          (t                            ; Set the mark where point is and move to CLICK-PT.
           (select-window window)
           (mouse-set-mark-fast click)
           (let ((before-scroll (with-current-buffer buf point-before-scroll)))
             (when before-scroll (goto-char before-scroll)))
           (exchange-point-and-mark)
           (mouse-set-region-1)
           ;; Previous region was copied to kill-ring, so replace with adjusted region.
           (if (boundp 'mouse-drag-copy-region)
               (when mouse-drag-copy-region ; Emacs 22+.
                 (kill-new (filter-buffer-substring (mark t) (point))))
             (kill-new (buffer-substring (point) (mark t)))) ; Emacs 20 & 21.
           (setq mouse-save-then-kill-posn  click-pt)))))
 
;;; Behavior for particular modes.

;;; Dired mode.

;;;###autoload
(defun mouse3-dired-use-menu ()
  "Make a second `mouse-3' click at the same place pop up a menu in Dired."
  (interactive)
  (remove-hook 'dired-after-readin-hook 'mouse3-dired-set-to-toggle-marks)
  (add-hook 'dired-after-readin-hook 'mouse3-dired-add-region-menu))

(defun mouse3-dired-add-region-menu ()
  "Add a `Selected Files' submenu to `mouse-3' pop-up menu.
Provides commands to act on the selected files and directories."
  (set (make-local-variable 'mouse3-popup-x-popup-panes-flag) nil)
  (let ((default-entries  mouse3-region-popup-entries))
    (set (make-local-variable 'mouse3-region-popup-entries)
         `((dired-menu
            "Selected Files" keymap
            (mark-region   menu-item "Mark"                   mouse3-dired-mark-region-files)
            (unmark-region menu-item "Unmark"                 mouse3-dired-unmark-region-files)
            (toggle-marked menu-item "Toggle Marked/Unmarked" mouse3-dired-toggle-marks-in-region-from-mouse)
            (flag          menu-item "Flag for Deletion"      mouse3-dired-flag-region-files-for-deletion)
            ;; If not using Dired+, include an item to turn off menu.
            ,@(and (eq (lookup-key dired-mode-map [mouse-3]) 'mouse-save-then-kill)
                   '((sep1-dired "--")
                     (turn-off-menu menu-item "Stop Using Menu" mouse3-dired-use-toggle-marks))))
           ,@default-entries))))

;; This needs to come after the definitions of `mouse3-dired-use-menu' and `mouse3-dired-add-region-menu'.
;;;###autoload
(defcustom mouse3-dired-function 'mouse3-dired-use-menu
  "*Fuction to call to update `dired-after-readin-hook' for `mouse-3' behavior."
  :type '(choice
          (function :tag "Use menu"     'mouse3-dired-use-menu)
          (function :tag "Toggle marks" 'mouse3-dired-use-toggle-marks)
          (const    :tag "No special behavior" :value nil))
  :set #'(lambda (sym defs)
           (custom-set-default sym defs)
           (if (functionp mouse3-dired-function)
               (funcall mouse3-dired-function)
             (remove-hook 'dired-after-readin-hook 'mouse3-dired-set-to-toggle-marks)
             (remove-hook 'dired-after-readin-hook 'mouse3-dired-add-region-menu))
           (when (eq major-mode 'dired-mode) (revert-buffer))) ; Refresh to get new binding.
  :initialize #'custom-initialize-reset
  :group 'mouse3)

;;;###autoload
(defun mouse3-dired-use-toggle-marks ()
  "Make a second `mouse-3' click at the same place toggle marks in Dired.
If you use Dired+ (`dired+.el') then this is a no-op."
  (interactive)
  (if (not (featurep 'dired+))
      (add-hook 'dired-after-readin-hook 'mouse3-dired-set-to-toggle-marks)
    (ding) (message "Canceled.  Use the popup menu in Dired+.")))

(defun mouse3-dired-set-to-toggle-marks ()
  "Set `mouse3-save-then-kill-command' to toggle Dired marks."
  (set (make-local-variable 'mouse3-save-then-kill-command) 'mouse3-dired-toggle-marks-in-region-from-mouse))

;;;###autoload
(defun mouse3-dired-toggle-marks-in-region-from-mouse (ignore1 ignore2)
  "Toggle marked and unmarked files and directories in region."
  (interactive "e\nP")
  (mouse3-dired-toggle-marks-in-region (region-beginning) (region-end)))


;;;-------------------------------------------------------------------
;;; The next 6 functions are borrowed from `dired+.el'.
;;; Only the prefix was changed, from `diredp-' to `mouse3-dired-'.

;;;###autoload
(defun mouse3-dired-toggle-marks-in-region (start end) ; Same as `diredp-toggle-marks-in-region.
  "Toggle marks in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (if (not (fboundp 'dired-toggle-marks))
          ;; Pre-Emacs 22.  Use bol, eol.  If details hidden, show first.
          (let ((details-hidden-p  (and (boundp 'dired-details-state)
                                        (eq 'hidden dired-details-state))))
            (widen)
            (when details-hidden-p (dired-details-show))
            (goto-char start)
            (setq start  (line-beginning-position))
            (goto-char end)
            (setq end    (line-end-position))
            (narrow-to-region start end)
            (dired-do-toggle)
            (when details-hidden-p (dired-details-hide)))
        (narrow-to-region start end)
        (dired-toggle-marks))))
  (when (and (get-buffer-window (current-buffer)) (fboundp 'fit-frame-if-one-window))
    (fit-frame-if-one-window)))

(defun mouse3-dired-this-file-marked-p (&optional mark-char) ; Same as `diredp-this-file-marked-p'.
  "Return non-nil if the file on this line is marked.
Optional arg MARK-CHAR is the type of mark to check.
 If nil, then if the file has any mark, including `D', it is marked."
  (and (dired-get-filename t t)
       (save-excursion (beginning-of-line)
                       (if mark-char
                           (looking-at (concat "^" (regexp-quote (char-to-string mark-char))))
                         (not (looking-at "^ "))))))

(defun mouse3-dired-this-file-unmarked-p (&optional mark-char) ; Same as `diredp-this-file-unmarked-p'.
  "Return non-nil if the file on this line is unmarked.
Optional arg MARK-CHAR is the type of mark to check.
 If nil, then if the file has no mark, including `D', it is unmarked.
 If non-nil, then it is unmarked for MARK-CHAR if it has no mark or
 it has any mark except MARK-CHAR."
  (and (dired-get-filename t t)
       (save-excursion (beginning-of-line)
                       (if mark-char
                           (not (looking-at (concat "^" (regexp-quote (char-to-string mark-char)))))
                         (looking-at "^ ")))))

;;;###autoload
(defun mouse3-dired-mark-region-files (&optional unmark-p) ; Same as `diredp-mark-region-files'.
  "Mark all of the files in the current region (if it is active).
With non-nil prefix arg, unmark them instead."
  (interactive "P")
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (line-beginning-position))
          end  (save-excursion (goto-char end) (line-end-position)))
    (let ((dired-marker-char  (if unmark-p ?\040 dired-marker-char)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg) (mouse3-dired-this-file-unmarked-p))
                     "region file"))))

;;;###autoload
(defun mouse3-dired-unmark-region-files (&optional mark-p) ; Same as `diredp-unmark-region-files'.
  "Unmark all of the files in the current region (if it is active).
With non-nil prefix arg, mark them instead."
  (interactive "P")
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (line-beginning-position))
          end  (save-excursion (goto-char end) (line-end-position)))
    (let ((dired-marker-char  (if mark-p dired-marker-char ?\040)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg) (mouse3-dired-this-file-marked-p))
                     "region file"))))

;;;###autoload
(defun mouse3-dired-flag-region-files-for-deletion () ; Same as `diredp-flag-region-files-for-deletion'.
  "Flag all of the files in the current region (if it is active) for deletion."
  (interactive)
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (line-beginning-position))
          end  (save-excursion (goto-char end) (line-end-position)))
    (let ((dired-marker-char  dired-del-marker))
      (dired-mark-if (and (<= (point) end) (>= (point) beg) (mouse3-dired-this-file-unmarked-p ?\D))
                     "region file"))))
 
;;; Picture mode.
;;;
;;; This code shows an example of using a (local) non-nil `mouse3-popup-x-popup-panes-flag'.

;;;###autoload
(defcustom mouse3-picture-mode-x-popup-panes
  `,@`(("Clear Rectangle"             . picture-clear-rectangle)
       ("Kill Rectangle"
        . (lambda (start end)
            "Kill the selected rectangle.
You can yank it using \\<picture-mode-map>`\\[picture-yank-rectangle]'."
            (interactive "r")
            (picture-clear-rectangle start end 'KILLP)))
       ("Clear Rectangle to Register" . picture-clear-rectangle-to-register)
       ("Draw Rectangle"              . picture-draw-rectangle)
       ;; This will raise an error if `picture-killed-rectangle' is not defined.
       ("Yank Picture Rectangle (Replace)" . picture-yank-rectangle)
       ("Yank Rectangle from Register (Replace)"
        . (lambda ()
            "Replace the selected rectangle by the contents of a register you name."
            (interactive)
            (exchange-point-and-mark)
            (call-interactively #'picture-yank-rectangle-from-register))))
  "*Picture mode submenu of popup menu for `mouse-3'."
  :type '(repeat
          (choice
           (cons :tag "Item"
            (string :tag "Name")
            ;; This is more correct but gives `mismatch' in Emacs < version 24:
            ;; (restricted-sexp :tag "Command" :match-alternatives (commandp))
            (restricted-sexp :tag "Command"
             :match-alternatives ((lambda (x) (not (null x)))) :value ignore))
           (list :tag "Separator" (const "--"))))
  :group 'mouse)

(add-hook
 'picture-mode-hook
 (lambda ()
   (set (make-local-variable 'mouse3-popup-x-popup-panes-flag) t)
   (set (make-local-variable 'mouse3-region-popup-x-popup-panes)
        (cons `("Picture Mode" ,@mouse3-picture-mode-x-popup-panes)
              (copy-tree mouse3-region-popup-x-popup-panes)))))


;;; Org mode - example popup menu.

;;; ;; TO-DO: Use a keymap, not x-popup panes, to at least recuperate any standard Org mode menus.
;;; (add-hook 'org-mode-hook
;;;           (lambda ()
;;;             (set (make-local-variable 'mouse3-popup-x-popup-panes-flag) t)
;;;             (set (make-local-variable 'mouse3-region-popup-x-popup-panes)
;;;                  (cons '("Org Mode"
;;;                          ("Shift Time"          . org-timer-change-times-in-region)
;;;                          ("Convert to ASCII"    . org-replace-region-by-ascii)
;;;                          ("Convert to HTML"     . org-replace-region-by-html)
;;;                          ("Convert to DocBook"  . org-replace-region-by-docbook)
;;;                          ("Convert to LaTeX"    . org-replace-region-by-latex))
;;;                        (copy-tree mouse3-region-popup-x-popup-panes)))))



;;; --------------------------------------------------
;;;
;;; An example of binding variable `mouse3-save-then-kill-command', taken from Icicles.
;;; Without the variable this definition would need to duplicate all of the
;;; `mouse-save-then-kill' code, changing just one line of it.
;;;
;;; (defun icicle-mouse-save-then-kill (click &optional arg) ; `mouse-3' in *Completions*.
;;;   "`mouse-save-then-kill', but click same place saves selected candidates."
;;;   (interactive "e\nP")
;;;   (let ((mouse3-save-then-kill-command
;;;          `(lambda (event prefix-arg)
;;;             (icicle-mouse-candidate-set-save-more nil ,arg))))
;;;     (mouse-save-then-kill click))
;;;   (setq this-command  'mouse-save-then-kill))

;;;;;;;;;;;;;;;;;;;;;;

(provide 'mouse3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mouse3.el ends here
