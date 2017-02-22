;;; emacs-init.el --- Drew Adams's Emacs init file.
;;
;; Filename: _emacs
;; Description: Emacs init file for use with libraries from Drew Adams
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1995-2017, Drew Adams, all rights reserved.
;; Created: Tue Sep 12 15:54:33 1995
;; Version: 0
;; Package-Requires: ((start "0"))
;; Last-Updated: Tue Feb 21 16:38:14 2017 (-0800)
;;           By: dradams
;;     Update #: 2186
;; URL: https://www.emacswiki.org/emacs/download/emacs-init.el
;; Keywords: init, .emacs, _emacs, dotemacs
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `advice', `advice-preload', `ange-ftp', `apropos', `apropos+',
;;   `apropos-fn+var', `assoc', `autofit-frame', `avoid',
;;   `backquote', `bookmark', `bookmark+', `bookmark+-1',
;;   `bookmark+-bmu', `bookmark+-key', `bookmark+-lit',
;;   `browse-kill-ring', `browse-kill-ring+', `buff-menu+',
;;   `chistory', `cl', `color-moccur', `comint', `compile',
;;   `compile+20', `compile-20', `cus-edit', `cus-edit+', `cus-face',
;;   `cus-load', `cus-start', `cus-theme', `custom', `cygwin-mount',
;;   `dired', `dired+', `dired-aux', `dired-details',
;;   `dired-details+', `dired-sort-menu', `dired-sort-menu+',
;;   `dired-x', `doremi', `doremi-cmd', `doremi-frm', `easymenu',
;;   `ediff', `ediff+', `ediff-diff', `ediff-help', `ediff-init',
;;   `ediff-merg', `ediff-mult', `ediff-util', `ediff-wind',
;;   `el-swank-fuzzy', `em-joc', `emacsbug', `eshell-auto',
;;   `eyedropper', `facemenu', `facemenu+', `faces', `faces+',
;;   `ffap', `ffap-', `files+', `find-dired', `find-dired+',
;;   `finder', `finder+', `finder-inf', `fit-frame', `flx',
;;   `font-lock', `font-lock-menus', `frame-cmds', `frame-fns',
;;   `fuzzy', `fuzzy-match', `header2', `help+20', `hexrgb',
;;   `highlight', `highlight-chars', `icicles', `icicles-cmd1',
;;   `icicles-cmd2', `icicles-face', `icicles-fn', `icicles-mcmd',
;;   `icicles-mode', `icicles-opt', `icicles-var', `icomplete',
;;   `icomplete+', `image-dired', `image-file', `imenu', `imenu+',
;;   `info', `info+20', `isearch+', `iso-transl', `kmacro',
;;   `lacarte', `levenshtein', `lib-requires', `lisp-mnt',
;;   `loadhist', `local-lpr', `local-ps-print', `lpr', `ls-lisp',
;;   `ls-lisp+', `ls-lisp-verbosity', `menu-bar', `menu-bar+',
;;   `misc-cmds', `misc-fns', `moccur-edit', `mouse', `mouse+',
;;   `mouse3', `mwheel', `naked', `occur-schroeder', `oneonone',
;;   `package', `paren', `pcmpl-auto', `pp', `pp+', `pp-c-l',
;;   `printing', `ps-print', `regexp-opt', `replace+', `ring',
;;   `ring+', `savehist-20+', `second-sel', `sendmail', `setup',
;;   `setup-cygwin', `setup-keys', `simple+', `speedbar', `start',
;;   `start-opt', `strings', `subr+', `subr-21', `swiss-move',
;;   `synonyms', `thing-cmds', `thingatpt', `thingatpt+',
;;   `thumb-frm', `time-date', `timer', `timer+', `unaccent', `vc',
;;   `vc+', `vc-', `vc-hooks', `vc-hooks+', `w32-browser',
;;   `w32browser-dlgopen', `wid-edit', `wid-edit+', `widget',
;;   `wimpy-del', `window+', `zones', `zoom-frm'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Emacs init file that uses Drew Adams' Emacs libraries.  This
;;    file and Drew's libraries can be used with GNU Emacs 20, 21, or
;;    22.
;;
;;  You can use this model ~/.emacs as a guide.  It also contains info
;;  on customization options and overriding settings made by my Emacs
;;  Lisp libraries.
;;
;;  Many of my customizations have worked with Emacs versions prior to
;;  20, as well.  I have used them on Windows 9x/NT/XP, GNU/Linux, and
;;  Unix.
;;
;;  This file also provides an introduction to using Emacs - see HELP
;;  WITH EMACS, below.
;;
;;  NOTE: It is usually better to start with vanilla Emacs (an empty
;;  .emacs file) and add things one at a time than it is to start with
;;  a .emacs file from someone else.  If nothing else, it is easier to
;;  debug problems that might arise.  So treat this as a guide only.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  INSTALLATION
;;
;;  Make a copy of this file in your home directory, and modify that
;;  as needed.  It can be named `_emacs' or `.emacs'.  It becomes your
;;  initialization file, loaded automatically whenever you start
;;  Emacs.  You can edit this, in order to customize Emacs.  Detailed
;;  instructions for doing so can be found below.
;;
;;    MICROSOFT WINDOWS - It's probably better to use the name
;;    `_emacs' for this file, not `.emacs'.  If you want to use the
;;    name `.emacs', you can use Emacs itself to rename it. (Windows
;;    Explorer won't let you rename the file `.emacs'.)  To rename the
;;    file in Emacs: Visit the directory (via `C-x d') and rename the
;;    file via `r' (`dired-rename-this-file') in the directory buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DREW'S LISP LIBRARY
;;
;;  The Emacs Lisp files in the directory named by the Emacs variable
;;  `drews-lisp-dir' (defined in this file) modify the default
;;  behavior of Emacs considerably.  You might not appreciate every
;;  such customization.
;;
;;  Try getting to know Emacs in the customized form provided here,
;;  before changing things.  You'll find extensive comments in my
;;  source (Emacs Lisp) files that should help you understand how
;;  things work, and how best to change things to suit your needs.
;;  The place to start, in order to understand my code and to change
;;  it, is the rest of the commentary in this file.
;;
;;  The names of my Lisp files follow the following convention.  Files
;;  that replace standard GNU Emacs Lisp files keep their original
;;  names (examples: `appt.el', `delsel.el').  Names of files that are
;;  extensions of standard files are suffixed by `-' or `+', depending
;;  on whether they are to be loaded just before or just after the
;;  corresponding standard files.  For example, my file `compile-.el'
;;  is to be loaded before the standard file `compile.el'.  My file
;;  `compile+.el' is to be loaded after `compile.el'.  The `+' case is
;;  more common than the `-' case.
;;
;;  Some customizations offer more functionality on Unix
;;  platforms.  Some offer more functionality on Windows.  To get the
;;  most out of Emacs on Windows, use it with Cygwin (www.cygwin.com).
;;
;;  Most of my libraries require others to function.  Look for
;;  `(require...)'  and `(load...)'  calls in the source code (*.el
;;  files) to see which other libraries are needed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CUSTOMIZING EMACS
;;
;;  This initialization file modifies basic Emacs in several ways.
;;  You may want to modify it further to suit your own tastes.  Each
;;  of the customizations done here is explained below, together with
;;  how to inhibit it.  The customizations made via libraries loaded
;;  here are explained in those library files themselves; the starting
;;  points are library files `start.el' and `start-opt.el'.
;;
;;  You can always start Emacs with no customizations at all (not even
;;  your `.emacs' file is loaded) via the command line option `-q'.
;;
;;  You need to know the following to customize things further:
;;
;;  1. Initialization files are automatically loaded in this order:
;;     1) A site library, `site-start.el', if there is one.  You can
;;        inhibit this via command line option `-no-site-file' when
;;        starting Emacs.
;;     2) Your initialization file, `.emacs' (this file).
;;     3) A site library, `default.el', if there is one.  You can
;;        inhibit this by setting variable `inhibit-default-init' to
;;        non-nil.
;;
;;  2. The variable `load-path' determines the order of searching
;;     libraries for files to load.  If you want to load your own file
;;     in place of a file having the same name in another library,
;;     then its directory should come before the other library in the
;;     list `load-path'.  If you want to make sure another library is
;;     already loaded when you load a file, then that file should
;;     require or load it as follows: (require 'library) or
;;     (load-library "library").
;;
;;  3. Before changing anything, check what existing settings do.  To
;;     know what a variable is/does, place the cursor on its name and
;;     type `C-h v'.  To know what a command or other function does,
;;     place the cursor on its name and type `C-h f'.
;;
;;  4. SETTING VARIABLES - You can use `defvar' to
;;     set a variable *before* loading a library that might also set
;;     it with `defvar': (defvar some-var some-value).  Only the first
;;     `defvar' executed assigns a value to the variable.  You can use
;;     `defconst' or `setq' to set a variable unconditionally: (setq
;;     some-var some-value) or (defconst some-var some-value).  Each
;;     call to one of these functions assigns a new value.  Try using
;;     `defvar' first to customize things; resort to `defconst' only
;;     if `defvar' doesn't seem to have any effect (either because a
;;     previous `defvar' was done or a subsequent `setq' or `defconst'
;;     was done).  Some variables have different (local) values in
;;     different buffers.  To set a variable's default (global) value
;;     for all buffers, use `setq-default': (setq some-var
;;     default-value).  An alternative to using `defvar' is using the
;;     menubar `Help > Customize' menu (this automatically modifies
;;     your Emacs init file).
;;
;;  5. See the Emacs manual, section `Customization' (especially
;;     subsection `Customization') for more information on customizing
;;     Emacs, including examples: `C-h i'; choose `Emacs'; choose
;;     `Customization' in the main menu, under heading `Advanced
;;     Features'; choose node
;;    `Init File'.
;;
;;  6. On Unix, you can predefine certain aspects of the display via
;;     the file `.Xdefaults' in your home director (`~').  Here is one
;;     suggestion (add these or similar lines to `~/.Xdefaults'):
;;
;;       emacs.Background:   LightBlue
;;       emacs.Foreground:   Black
;;       emacs.cursorColor:  Red
;;       emacs.pointerColor: Red
;;
;;       Emacs*popup.font: -*-*-*-*-*-21-*-*-*-*-*-iso8859-1
;;       Emacs*menubar.font: -*-*-*-*-*-21-*-*-*-*-*-iso8859-1
;;       Emacs*popup.background: Pink
;;       Emacs*menubar.background: Pink
;;       Emacs*menubar.buttonForeground: Blue
;;
;;     For more information on the `.Xdefaults' file, see the Emacs
;;     manual (`C-h i'), node `Resources X'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  HELP WITH EMACS
;;
;;  This section provides general info on using Emacs.  For more info,
;;  type `Control-h i' in Emacs to consult the Emacs user manual.
;;
;;  NOTATION - In Emacs documentation, "C-" means (press and hold) the
;;  Control key; "M-" means the Alt key (called "Meta"); "DEL" means
;;  the Backspace key; "RET" means the Return or Enter key; "SPC"
;;  means the space bar, "ESC" means the Escape key.  A combination
;;  like "C-M-" (or "M-C") means press and hold both Control and Meta
;;  keys.
;;
;;  ABORTING - You can use `C-g' to abort any action you have
;;  started.  For some actions, you may need to repeat this.  If even
;;  this doesn't clear things up entirely, then try `C-]', which
;;  should do the trick.
;;
;;  HELP - For help with Emacs, use the `Help' menu, submenu
;;  `Describe'.  Item `This...'  in this menu (shortcut: `C-h RET')
;;  combines many of the other items into one.  It lets you
;;  mouse-click anything, type keys, or choose a menu item to get help
;;  on the object clicked, the keys typed, or the menu item chosen.
;;
;;  Some of the menus and key bindings used here are different from
;;  those described in the standard Emacs manual, which is available
;;  via the `Help' menu, item `Info' (shortcut: `C-h i'), `Emacs'.
;;  All other documentary help should be up-to-date and accurate.
;;
;;  COMMANDS - You can enter any command, whether bound to a key
;;  sequence or not, via `M-x'.  Previously entered commands are
;;  available for editing and re-execution via `C-p' (previous) and
;;  `C-n' (next).  See also `C-x ESC ESC' for more advanced command
;;  editing.
;;
;;  INPUT - Some actions require you to input some text in the
;;  "minibuffer" (where you enter commands).  Default text (usually
;;  the text surrounding the cursor) may already be provided there,
;;  for possible modification.  You can remove this quickly, if you
;;  like, via `M-C-DEL' or `C-x DEL'.  Use `RET' to confirm and enter
;;  the input you want.
;;
;;    For example, the command `grep' (menu `Search', item 'Grep', or
;;    just `M-x grep') expects you to input text to search for, and
;;    file name patterns for files to search.  By default, this
;;    command puts the text found around the cursor into the
;;    minibuffer as the text to look for.  If that's not what you
;;    want, type `M-C-DEL' (or just repeated `DEL's) to remove it,
;;    then type the text and file name patterns you want and confirm
;;    with `RET'.
;;
;;
;;  EMACS JARGON
;;
;;  Here is a translation to/from Emacs-speak:
;;
;;         common term       Emacs term
;;         -----------       ----------
;;         selection         region
;;         cut               kill
;;         paste             yank
;;         window            frame
;;
;;  The (text) "cursor" is the place where you insert typed text; it
;;  is rectangular, by default.  The (mouse) "pointer" shows the mouse
;;  position; it is an arrow, by default.  The terms "point" and
;;  "mark" are discussed below.
;;
;;  In Emacs terms, a "window" is a frame pane, that is, a sub-frame.
;;  The "mode-line" is the text at the bottom border of a window.
;;
;;  The "minibuffer" is the special buffer for entering commands.  If
;;  you use my library `oneonone.el' and option
;;  `1on1-minibuffer-frame-flag' is non-nil, then the minibuffer
;;  appears as a separate frame.  Otherwise, it the minibuffer
;;  typically appears at the bottom of each frame.
;;
;;
;;  BUFFER MODES
;;
;;  Each buffer has its own editing mode (key bindings etc).
;;  Information on a buffer's current mode is available via the `Help'
;;  menu, item `Describe Mode' (shortcuts: `C-h m' or `C-h RET' click
;;  buffer).  In the menu bar, all menus to the left of the separator
;;  `||' are specific to the current mode; the menus to the right
;;  (`Buffers', `Files', `Tools', `Edit', `Search', `Frames', `Help')
;;  are common to all modes.  (See also `M-x describe-menubar'.)
;;
;;
;;  UNDO
;;
;;  You can undo anything via the `Edit' menu, item `Undo'
;;  (shortcut:`C-/').  This can be repeated any number of times.  Do
;;  anything else to stop undoing.  Doing undo after that redoes what
;;  you undid (because it undoes the undoing!).  You can tell when
;;  you've undone all unsaved changes, because the lower left of the
;;  mode line shows `--' instead of `**'.  You can quickly undo all
;;  unsaved changes via `M-x revert-buffer'.
;;
;;
;;  MOUSE
;;
;;  You can use the mouse as usual: press MB1 (left mouse button) and
;;  drag to define the selection, MB2 (center mouse button) to paste.
;;  MB3 (right mouse button) extends the selection (twice=kills it) or
;;  pops up a context-appropriate menu, depending on the current
;;  buffer.  Check the Emacs manual to learn about additional
;;  available mouse actions.  In particular, a secondary selection is
;;  also available by pressing and holding the Meta key (or `M-C-')
;;  during mouse actions.  For example, press and hold `M-' while
;;  dragging the mouse to define the secondary selection; press and
;;  hold `M-' and click MB2 to paste it.  It's quite useful to have
;;  two separate selections to paste etc.
;;
;;
;;  SEARCH / REPLACE
;;
;;  You can use the `Search' menu for this.  However, incremental
;;  searching is generally better to use (more convenient and more
;;  powerful).  It is available via `C-s' and `C-r'; for more
;;  information, type `C-s C-h'.  Other search options are available,
;;  including regular expression (wildcard) searches and searching
;;  (`M-x grep') or replacing (`Q' in Dired) in multiple files.
;;
;;
;;  REGION (SELECTION)
;;
;;  The text cursor (not the mouse pointer) defines the position of
;;  the "point", which is one end of the region.  The other end is the
;;  "mark" position.  You can set the point by clicking MB1 or using
;;  the arrow keys.  You can set the mark in any of these ways:
;;
;;     - `C-SPC' (point stays where it was)
;;     - dragging the point with MB1 (old point becomes the mark)
;;     - clicking MB3 (becomes the point; old point becomes the mark)
;;
;;  If you define the region without using the mouse, it may be
;;  invisible; you can make it visible via `C-x C-x', which also swaps
;;  the point and the mark.
;;
;;
;;  FRAME COMMANDS
;;
;;  Use the `Frames' menu to manipulate frames.
;;
;;    `Resize This Frame' (`C-x C-_') resizes the current frame to try
;;                 to fit all the text of the current window.
;;
;;    `Hide Frames / Show Buffers' (`M-C-z') hides all of
;;                 the frames except one, which is then iconified.
;;                 This is especially useful with Microsoft Windows
;;                 prior to XP, because, unlike `Iconify All Frames'
;;                 (see next), it places only a single icon in the
;;                 task bar.
;;
;;                 Memorize the binding of this command: `M-C-z'; you
;;                 use it again to reverse the action.  After hiding
;;                 everything, when you click the single icon it opens
;;                 a single frame (which may not have a menu bar).
;;                 You then type `M-c-z' to get access to the other
;;                 frames (via the displayed Buffer List).
;;
;;                 An alternative shortcut for this command is
;;                 `[C-mouse-1]' in the minibuffer; that is, clicking
;;                 mouse-1 in the minibuffer while pressing the
;;                 CONTROL key.
;;
;;    `Iconify All Frames' iconifies each frame separately.
;;
;;    `Tile Frames Horizontally', `Tile Frames Vertically' do what
;;                 they say.  Use them, for example, before comparing
;;                 buffers using `Tools > Compare'.
;;
;;  Use the `Buffers' menu to raise individual frames (and buffers).
;;  Use the `Files' menu to delete, duplicate and split frames.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; Code:




;;; TO COMMENT OUT SEVERAL LINES:
;;;  1. Place cursor at start of first line.
;;;  2. Click MB3 at start of line following last line.
;;;     (These actions define the lines to act on as a region.)
;;;  3. Type `C-u 3' (to use three semicolons).

;;; TO UNCOMMENT SEVERAL LINES:
;;;  1. Place cursor at start of first line.
;;;  2. Click MB3 at start of line following last line.
;;;     (These actions define the lines to act on as a region.)
;;;  3. Type `M- -3' (to remove three semicolons) - Note: *hyphen* (minus) 3.


;;; WINDOWS/UNIX:
;;; The expression (eq system-type 'windows-nt) is non-nil if Emacs is
;;; running on Windows.  Some libraries are only available for
;;; Windows.  Sometimes a path is different for Windows.
;;; For example:
;;;   (if (eq system-type 'windows-nt)        ; Test
;;;       "C:\\drews-lisp-20"                 ; Windows path
;;;     "~/drews-lisp-20")                    ; non-Windows path


;;; COMMENT THESE OUT IF YOU DO NOT WANT TO DEBUG EMACS
(setq debug-on-error t)
(setq byte-compile-debug t)

;;; UNCOMMENT THIS & REPLACE "XXX" HERE BY YOUR USER (LOGIN) NAME.
;;; This inhibits the initial startup echo area message.
;;; (setq inhibit-startup-echo-area-message "XXX")


;;; UNCOMMENT AND CHANGE *ONE* OF THESE, IF DEFAULT FRAME HEIGHT IS INAPPROPRIATE.
;;; Maximum height for new frames.
;;; (defvar fit-frame-max-height-percent 82) ; no more than 82% of display height.
;;; (defvar fit-frame-max-height 48)         ; no more than 48 characters high.


;;; UNCOMMENT AND CHANGE *ONE* OF THESE, IF DEFAULT FRAME WIDTH IS INAPPROPRIATE.
;;; Maximum width for new frames.
;;; (defvar fit-frame-max-width-percent 94) ; no more than 94% of display width.
;;; (defvar fit-frame-max-width 120)        ; no more than 120 characters wide.


;;; COMMENT THIS OUT IF YOU DO *NOT* WANT MAXIMUM BUFFER HIGHLIGHTING.
(defconst font-lock-maximum-decoration t)


;;; COMMENT OUT IF YOU DO *NOT* WANT THE GIVEN EFFECT.
(setq inhibit-startup-message t)        ; Do without annoying startup msg.
(put 'scroll-left               'disabled nil)
(put 'eval-expression           'disabled nil) ; Enable eval of Lisp sexps.
(put 'narrow-to-region          'disabled nil) ; Enable region narrowing & widening.
(put 'narrow-to-page            'disabled nil) ; Enable region narrowing & widening.
(put 'downcase-region           'disabled nil) ; Enable case changes of region text.
(put 'upcase-region             'disabled nil) ;   "     "
(put 'capitalize-region         'disabled nil) ;   "     "
(put 'dired-find-alternate-file 'disabled nil)
(auto-compression-mode 1)               ; Auto decompress compressed files.
(delete-selection-mode t)               ; Use delete-selection mode.
(setq message-log-max 2000)

(defvar mouse-1-click-follows-link)     ; Quiet the byte-compiler.
(when (boundp 'mouse-1-click-follows-link) ; Do not use mouse-1 to follow links.
  (setq mouse-1-click-follows-link nil))   ; Others to consider: 100, `double'.  Default is bad.

;;; Restore sane mouse behavior before Emacs 23.3 / Emacs 24 "improvement" to fit X Window.
(global-set-key [mouse-2] 'mouse-yank-at-click)

;;; COMMENT THIS OUT IF YOU DO *NOT* WANT "SPECIAL" BUFFERS TO BE IN
;;; SEPARATE FRAMES ("Special" buffers are those, such as *grep*,
;;; whose names are within '*'s.) If `special-display-regexps' is
;;; non-nil, then special buffers are in dedicated frames (cannot
;;; dissociate the buffer and its frame).
(when (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
  (defconst special-display-regexps '("[ ]?[*][^*]+[*]")))


;;; MODIFY FRAME FITTING -
;;; Libraries `autofit-frame.el' and `fit-frame.el' are loaded in
;;; library `start.el'.  Together with library `start-opt.el' (loaded
;;; below), they set up automatic fitting of frames to the selected
;;; window/buffer.  You can inhibit frame fitting at 3 levels:
;;;
;;;  - inhibit automatic fitting of one-window frames
;;;  - inhibit fitting of a new frame when it is created
;;;  - inhibit all frame fitting, even manual fitting via `C-x C-_'
;;;
;;; UNCOMMENT TO *INHIBIT* AUTOMATIC FITTING OF ONE-WINDOW FRAMES.
;;; (defvar autofit-frames-flag nil)
;;;
;;; UNCOMMENT TO *INHIBIT* ALL FRAME FITTING (even via `C-x C-_').
;;; (defvar fit-frame-inhibit-fitting-flag t)
;;;
;;; Library `start-opt.el' is loaded below.  It causes new frames to be
;;; fitted by doing: (add-hook 'after-make-frame-functions 'fit-frame).
;;;
;;; TO *INHIBIT* FITTING *NEW* FRAMES UPON CREATION, UNCOMMENT THIS
;;; AND MOVE IT AFTER (require 'start-opt), BELOW.
;;; (remove-hook 'after-make-frame-functions 'fit-frame)

;;; UNCOMMENT IF YOU DO *NOT* WANT TO USE `delete-windows-for'.
;;; Non-nil => Use `delete-windows-for' in place of `delete-window'.
;;; (defvar sub-delete-windows-for nil)

;;; UNCOMMENT IF YOU DO *NOT* WANT TO USE `query-replace-w-options'.
;;; Non-nil => Use `query-replace-w-options' in place of `query-replace'.
;;; (defvar sub-query-replace-w-options nil)

;;; UNCOMMENT IF YOU DO *NOT* WANT TO USE `kill-buffer-and-its-windows'.
;;; Non-nil => Use `kill-buffer-and-its-windows' in place of `kill-buffer'.
;;; (defvar sub-kill-buffer-and-its-windows nil)


;;; CHANGE THIS TO REFLECT THE ADDRESS OF DREW'S LISP LIBRARY AT YOUR SITE.
(defvar drews-lisp-dir
  (if (eq system-type 'windows-nt)      ; Windows
      "C:\\drews-lisp-20"
    "~/drews-lisp-20")
  "Address of Drew's lisp libraries.")


;;; ADD DREW'S LISP LIBRARY TO YOUR `load-path'.
(setq load-path (append (list drews-lisp-dir) load-path)) ; Drew's lisp library


;;; COMMENT OUT OR UNCOMMENT ANY YOU LIKE - Some contributed libraries
(setq load-path (append
                 (list (concat drews-lisp-dir "/CONTRIB"))
                 (list (concat drews-lisp-dir "/CONTRIB/tty-color")) ; Fixes to tty-color
                 ;; (list (concat drews-lisp-dir "/CONTRIB/ps-print")) ; PostScript printing
                 ;; (list (concat drews-lisp-dir "/CONTRIB/ebnf2ps")) ; BNF to PostScript
                 ;; (list (concat drews-lisp-dir "/CONTRIB/gnuserv")) ; Emacs server/client
                 load-path))


;;; PLACE YOUR OWN LISP LIBRARY HERE, IF ANY:
;;; (setq load-path (append (list "my-path/my-lib-dir") load-path))


;;; UNCOMMENT THIS IF YOU USE EMACS 20 AND YOU WANT TO BE ABLE TO INPUT TEXT IN OTHER LANGUAGES.
;;; CHANGE PATH AS NEEDED
;;; (if (and (eq system-type 'windows-nt) (< emacs-major-version 21))
;;;     (let ((default-directory "C:\\Emacs-20.7/leim"))
;;;       (quail-update-leim-list-file "C:\\Emacs-20.7/leim")))


;;; COMPLETE THE ADDRESS OF grep COMMAND, OR JUST USE "grep -n" IF
;;; PROGRAM grep CAN BE FOUND VIA YOUR PATH VARIABLE.
(if (< emacs-major-version 21) (defvar grep-command "grep -n "))



;;; PLACE YOUR `DEFVAR' VARIABLE SETTINGS HERE, IF ANY.



;;; LOAD DREW'S LIBRARY FILES from `drews-lisp-dir'.
;;; Library file `start.elc' loads *lots* of others.  In particular, it
;;; loads `oneonone.elc' and `setup-keys.elc', which define
;;; default frame configurations and key bindings.  You can see
;;; (roughly) which library files have been loaded at any time via
;;; `C-h v features'.
(require 'start)

;;; UNCOMMENT THIS AND CHANGE TO CORRECT PATHS for ghostview and
;;; ghostscript, as appropriate.  Local printing settings for
;;; `printing.el' from Vinicius Jose Latorre <vinicius@cpqd.com.br>.
;;; You can set different values for Windows and UNIX.  See
;;; `printing.el'.
;;; (cond ((eq system-type 'windows-nt)
;;;        (setq pr-path-alist
;;;              '((windows     "c:/Program Files" PATH ghostview ghostscript)
;;;                (ghostview   "c:/Program Files/Ghostgum/gsview")
;;;                (ghostscript "c:/gs/gs8.11/bin")
;;;                (cygwin windows)))
;;;        (setq pr-txt-name          'home)
;;;        (setq pr-txt-printer-alist '((home "" nil "PRN") ; Local printer
;;;                                     (4op1135e nil nil ; Network printer
;;;                                      "/D:\\\\st-psrv1\\4op1135e")
;;;                                     (4op1135b nil nil ; Network printer
;;;                                      "/D:\\\\st-psrv1\\4op1135b")
;;;                                     (4op1107a nil nil ; Network printer
;;;                                      "/D:\\\\ST-PRINT\\4op1107a")
;;;                                     (4op1235b nil nil ; Network printer
;;;                                      "/D:\\\\st-psrv1\\4op1235b")
;;;                                     (4op1207a nil nil ; Network printer
;;;                                      "/D:\\\\ST-PRINT\\4op1207a")))
;;;        (setq pr-ps-name           '4op1135e)
;;;        (setq pr-ps-printer-alist  '((home "" nil "" "PRN" (ps-spool-duplex)) ; Local printer
;;;                                     (4op1135e "print" nil "/D:" ; Network printer
;;;                                      "\\\\st-psrv1\\4op1135e"
;;;                                      (ps-spool-duplex . t))
;;;                                     (4op1135b-B/W "gsprint" ("-all" "-twoup") "-printer "
;;;                                      "\\\\st-psrv1\\4op1135b")
;;;                                     (4op1135b "print" nil "/D:" ; Network printer
;;;                                      "\\\\st-psrv1\\4op1135b"
;;;                                      (ps-spool-duplex . t))
;;;                                     (4op1107a "print" nil "/D:" ; Network printer
;;;                                      "\\\\ST-PRINT\\4op1107a"
;;;                                      (ps-spool-duplex . t))
;;;                                     (4op1235b "print" nil "/D:" ; Network printer
;;;                                      "\\\\st-psrv1\\4op1235b"
;;;                                      (ps-spool-duplex . t))
;;;                                     (4op1207a "print" nil "/D:" ; Network printer
;;;                                      "\\\\ST-PRINT\\4op1207a"
;;;                                      (ps-spool-duplex . t))))
;;;        (setq pr-temp-dir "c:/Documents and Settings/who-you-are/Local Settings/Temp") ; Temp
;;;        ;;;(setq pr-delete-temp-file nil)                ; don't delete temp file
;;;        (setq pr-faces-p nil)            ; Do not print using Emacs face attributes
;;;        (setq ps-end-with-control-d nil) ; Do not insert ^D (blank page) at end of PostScript.
;;;        (when (fboundp 'pr-update-menus) (pr-update-menus t)))) ; Update menu w these changes
;;; (global-set-key "\C-ci"  'pr-interface)
;;; (global-set-key "\C-cbp" 'pr-ps-buffer-print)
;;; (global-set-key "\C-cbx" 'pr-ps-buffer-preview)
;;; (global-set-key "\C-cbb" 'pr-ps-buffer-using-ghostscript)
;;; (global-set-key "\C-cb1f"
;;;                 #'(lambda () (interactive) (pr-ps-buffer-ps-print 1 t))) ; Prompt for file
;;; (global-set-key "\C-cb1p" #'(lambda () (interactive) (pr-ps-buffer-ps-print 1 nil)))
;;; (global-set-key "\C-crp" 'pr-ps-region-print)
;;; (global-set-key "\C-crx" 'pr-ps-region-preview)
;;; (global-set-key "\C-crr" 'pr-ps-region-using-ghostscript)
;;; (global-set-key "\C-cr1f" #'(lambda () (interactive) (pr-ps-buffer-ps-print 1 t)))
;;; (global-set-key "\C-cr1p" #'(lambda () (interactive) (pr-ps-buffer-ps-print 1 nil)))



;;; GNUSERV EMACS SERVER.  UNCOMMENT THIS IF YOU HAVE GNUSERV AND YOU WANT TO USE IT.
;;; Start gnuserv server, so you can edit files using gnuclient.
;;; (require 'gnuserv)
;;; (server-start)


;;; COMMENT THIS OUT IF YOU DO *NOT* HAVE OR USE CYGWIN
(when (and (eq system-type 'windows-nt)  (require 'cygwin-mount nil t))
  (require 'setup-cygwin nil t))

;;; COMMENT THIS OUT IF YOU WANT MS Windows, NOT Emacs, to use `M-TAB' or `M-S-TAB'.
(when (fboundp 'w32-register-hot-key)
  (w32-register-hot-key [M-tab])
  (w32-register-hot-key [M-S-tab]))

;;; COMMENT THIS OUT IF YOU DO *NOT* WANT THE CUSTOMIZATIONS IN library `start-opt'.
;;; The following setup assignments are done in file `start-opt.elc'.
;;; Action to change these should be taken *after* loading it.
;;; (See file `start-opt.el' for more detail.)
;;;
;;; 1. Some standard faces are redefined: highlight, modeline, region,
;;;    secondary-selection, query-replace, isearch, ediff-*-face-*.
;;; 2. Searching is made case-sensitive by default, but `C-c' while
;;;    searching (`C-s') toggles case-sensitivity.
;;;    To inhibit this, do (setq-default case-fold-search t).
;;; 3. DEL (backspace) removes the current selection, and typing replaces it.
;;;    To inhibit this, do (delete-selection-mode -1).
;;; 4. Coloring (font-locking) is the default in all buffers.
;;;    To inhibit this, do (global-font-lock-mode nil)
;;; 5. Indenting uses only spaces, not TABs.
;;;    To inhibit this, do (setq-default indent-tabs-mode t).
;;; 6. The default mode for buffers is `indented-text-mode'.
;;;    To inhibit this, do (setq default-major-mode 'fundamental-mode)
;;; 7. Text mode uses auto-fill, by default.
;;;    To inhibit this, do (remove-hook 'text-mode-hook 'turn-on-auto-fill).
;;; 8. Newly created frames are fitted to their buffer/window.  To inhibit this, do
;;;    (remove-hook 'after-make-frame-functions 'fit-frame)
;;; 9. One-window frames containing "temporary" buffers (e.g. *Help*) are
;;;    automatically fit.  To inhibit this, do
;;;    (remove-hook 'temp-buffer-show-hook 'fit-frame-if-one-window)

(require 'start-opt nil t)              ; Optional startup assignments.

;;; COMMENT THIS OUT IF YOU DO *NOT* WANT THE WINDOW-MANAGER
;;; "Minimize" BUTTON TO THUMBIFY INSTEAD OF ICONIFY.
(when (and (eq system-type 'windows-nt) (fboundp 'thumfr-thumbify-frame-upon-event))
  (define-key special-event-map [iconify-frame] 'thumfr-thumbify-frame-upon-event))


;;; UNCOMMENT THIS IF YOU WANT TO ENTER THE DEBUGGER FOR SOME ERRORS, BUT NOT OTHERS.
;;; See `fdb.el'.
;;; (progn (require 'fdb)(setq debug-on-error t))


;;; DIARY FOR USE WITH CALENDAR AND APPOINTMENTS:
;;; IF YOU DO *NOT* WANT TO USE A DIARY, THEN UNCOMMENT THESE LINES:
;;; (setq view-diary-entries-initially nil)
;;; (setq mark-diary-entries-in-calendar nil)
;;;
;;; IF YOU *DO* WANT TO USE A DIARY, THEN CREATE A FILE NAMED `diary'
;;; IN YOUR HOME DIRECTORY.
;;;
;;; For more info on the calendar, the diary and appointments, see the
;;; Emacs manual (`C-h i', then choose `Calendar/Diary' in the menu).



;;; PLACE YOUR `SETQ', AND `DEFCONST' VARIABLE SETTINGS HERE, IF ANY.



;;;
;;; ******************************************************************


;;; A HACK FOR WINDOWS - COMMENT THIS OUT IF YOU DO *NOT* USE MS WINDOWS
(when (and (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
           (eq system-type 'windows-nt) (fboundp 'rename-frame))
  (add-hook 'window-setup-hook 'rename-frame)) ; Defined in `frame-cmds.el'.





;;; ******************************************************************
;;; IMPORTANT - DO THE REST *LAST*
;;; ******************************************************************

;;; UNCOMMENT and define variable `custom-file' to load your custom
;;; file.  This tells Customize to put your customizations in this
;;; file, not in your `~/.emacs'.  This keeps your init file free of
;;; Customize stuff.

;;; (setq custom-file  YOUR-CUSTOM-FILE-LOCATION) ; An absolute file name.
;;; (load-file custom-file)


;;; ******************************************************************
;;; Load Icicles and turn on Icicle mode AFTER loading your
;;; `custom-file', so Icicles will pick up certain option values, such
;;; as `icicle-touche-pas-aux-menus-flag', and will correctly pick up
;;; all current key definitions, bind the mouse wheel etc.

(require 'icicles nil t)
(when (fboundp 'icicle-mode) (icicle-mode 1)) ; Defined in `icicles.el'.

;;; UNCOMMENT TO TELL CUSTOMIZE THAT THE PRESENT STATE IS THE BASE-LINE:
;;; Consider current option values as unchanged (pseudo-saved).
;;; This function is defined in `cus-edit+.el'.
;;; (when (fboundp 'customize-consider-all-unchanged)
;;;   (customize-consider-all-unchanged))

;;; UNCOMMENT THIS IF YOU WANT AUTOMATIC CUSTOMIZE UPDATING,
;;; so it takes into account changes made outside Customize,
;;; considering them to "set" the preferences.
;;; (when (fboundp 'customize-toggle-outside-change-updates)
;;;   (customize-toggle-outside-change-updates 99))


;;; ******************************************************************
;;;;;;;;;;;;;;;;;;;;;;;

(provide 'emacs-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-init.el ends here
