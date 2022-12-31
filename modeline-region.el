;;; modeline-region.el --- Show region information in the mode-line.  -*- lexical-binding: t -*-
;;
;; Filename: modeline-region.el
;; Description: Show region information in the mode-line.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006-2023, Drew Adams, all rights reserved.
;; Created: Thu Nov  4 19:58:03 2021 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sat Dec 31 14:11:42 2022 (-0800)
;;           By: dradams
;;     Update #: 366
;; URL: https://www.emacswiki.org/emacs/modeline-region.el
;; Doc URL: https://www.emacswiki.org/emacs/ModeLineRegion
;; Keywords: mode-line, region, faces, help, column
;; Compatibility: GNU Emacs: 24.x, 25.x, 26.x, 27.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `auth-source', `avoid', `backquote',
;;   `bookmark', `bookmark+', `bookmark+-1', `bookmark+-bmu',
;;   `bookmark+-key', `bookmark+-lit', `button', `bytecomp', `cconv',
;;   `cl', `cl-generic', `cl-lib', `cl-macs', `cmds-menu',
;;   `col-highlight', `color', `crosshairs', `custom', `doremi',
;;   `doremi-frm', `easymenu', `eieio', `eieio-core',
;;   `eieio-loaddefs', `epg-config', `facemenu', `facemenu+',
;;   `faces', `faces+', `fit-frame', `font-lock', `font-lock+',
;;   `font-lock-menus', `frame-cmds', `frame-fns', `gv', `help+',
;;   `help-fns', `help-fns+', `help-macro', `help-macro+',
;;   `help-mode', `hexrgb', `highlight', `hl-line', `hl-line+',
;;   `info', `info+', `isearch+', `isearch-prop', `kmacro',
;;   `macroexp', `menu-bar', `menu-bar+', `misc-cmds', `misc-fns',
;;   `mwheel', `naked', `package', `palette', `password-cache', `pp',
;;   `pp+', `radix-tree', `rect', `replace', `ring', `second-sel',
;;   `seq', `strings', `syntax', `tabulated-list', `text-mode',
;;   `thingatpt', `thingatpt+', `timer', `url-handlers', `url-parse',
;;   `url-vars', `vline', `w32browser-dlgopen', `wid-edit',
;;   `wid-edit+', `widget', `zones'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Show region information in the mode-line.
;;
;;  More description below.
;;
;;
;;(@> "Index")
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  https://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@> "Things Defined Here")
;;  (@> "Documentation")
;;    (@> "Purpose and Getting Started")
;;    (@> "General Behavior")
;;    (@> "Controlling What Region Info To Show")
;;    (@> "Use Mode-Line Menus To Change Behavior")
;;    (@> "Highlight Large Column Numbers")
;;    (@> "Show That Additional Commands Are Regionable")
 
;;(@* "Things Defined Here")
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Faces defined here:
;;
;;    `mlr-column-warning', `mlr-region', `mlr-region-acting-on'.
;;    
;;  User options defined here:
;;
;;    `mlr-column-limit', `mlr-count-partial-words-flag',
;;    `mlr-empty-region-flag', `mlr-init-menu-on-load-flag',
;;    `mlr-non-rectangle-style', `mlr-rectangle-style',
;;    `mlr-region-style', `mlr-use-property-mlr-acts-on-flag'.
;;
;;  Commands defined here:
;;
;;    `global-modeline-region-mode', `mlr-choose-region-style',
;;    `mlr-count-rectangle-contents', `modeline-region-mode',
;;    `mlr-toggle-non-rectangle-style', `mlr-toggle-rectangle-style',
;;    `mlr--advice-4', `mlr--advice-11', `mlr--advice-12',
;;    `mlr--advice-13', `mlr--advice-14', `mlr--advice-15',
;;    `mlr--advice-16', `mlr--advice-18', `mlr--advice-19',
;;    `mlr--advice-20', `mlr--advice-21'.
;;
;;  Non-interactive functions defined here:
;;
;;    `mlr--advice-1', `mlr--advice-2', `mlr--advice-3',
;;    `mlr--advice-10', `mlr--advice-17',
;;    `mlr-set-default-mode-line-position', `mlr-show-region-p',
;;    `turn-on-modeline-region-mode'.
;;
;;  Constants defined here:
;;
;;    `mlr--region-style-alist', `mlr--region-style-default'.
;;
;;  Internal variables defined here:
;;
;;    `mlr-bytes-format', `mlr-chars-format',
;;    `mlr-lines+chars-format', `mlr-lines+words+chars-format',
;;    `mlr--mlp-is-set-up-p', `mlr--orig-mlp-local-p',
;;    `mlr--orig-mode-line-position', `mlr-menu', `mlr-rect-p',
;;    `mlr-region-acting-on', `mlr-rows+cols-format',
;;    `mlr-rows+cols+words+chars-format', `modeline-region-mode-map'.
  
;;(@* "Documentation")
;;
;;  Documentation
;;  -------------
;;
;;(@* "Purpose and Getting Started")
;;  ** Purpose and Getting Started **
;;
;;  Minor mode `modeline-region-mode' and its globalized version
;;  `global-modeline-region-mode' enhance the buffer size indication
;;  in the mode-line, that is, what is provided by vanilla option
;;  `mode-line-position' with `size-indication-mode' turned on.
;;  (These modes automatically turn on `size-indication-mode'.)
;;
;;  Region information is shown in the mode-line when the region is
;;  active -- you can show the the region size in various ways.
;;
;;  To use this library, put this file in a directory in your
;;  `load-path', and add this to your init file:
;;
;;    (require 'modeline-region)
;;
;;  If you want to turn on this showing of active-region information
;;  in the mode-line globally (in all buffers) then add this as well:
;;
;;    (global-modeline-region-mode 1)
;;
;;  Otherwise, to turn it on/off in a given buffer or globally, use
;;  command `modeline-region-mode' or `global-modeline-region-mode',
;;  respectively.
;;
;;  (This library supersedes my older library `modeline-posn.el'.
;;  This one is more correct, more featureful, and cleaner.)
;;
;;(@* "General Behavior")
;;  ** General Behavior **
;;
;;  Whether an empty active region is indicated by these modes is
;;  controlled by option `mlr-empty-region-flag'.  By default an empty
;;  region is indicated, to avert you to the fact that you're acting
;;  on an empty region.
;;
;;  When these modes are on, information about the region size is
;;  shown in the mode-line using face `mlr-region', by default.  By
;;  default, this face looks the same as face `region'.
;;
;;  But when you use a command that's known to act on the active
;;  region, the mode-line indication instead uses face
;;  `mlr-region-acting-on', to draw attention to this fact.
;;
;;  By a "command that acts on the active region" I mean a command
;;  that's "regionable": it acts on the region only when it's active.
;;  It typically acts on the full buffer, or the text from point to
;;  the end of the buffer, when the region's not active.
;;
;;  And by "act" I mean the command is in some way region-aware,
;;  region-reactive, or region-sensitive.
;;
;;  (A command that always acts on the region, whether or not it's
;;  active, can be called just "regional"; it's not "regionable".
;;  There's no need to avert you to its acting on the region.)
;;
;;  Some specific commands and non-interactive functions are advised
;;  in `modeline-region-mode', to realize this feature of indicating
;;  that they act specially on the region.  These include replacement
;;  commands.
;;
;;  If you use library `isearch+.el' then you can restrict Isearch
;;  commands and replacement commands to the active region (this is
;;  controlled by option `isearchp-restrict-to-region-flag').  Then,
;;  because such commands act on the region, the mode-line indication
;;  uses face `mlr-region-acting-on'.
;;
;;  (Advice is also provided for the merely "regional" commands
;;  `prepend-to-buffer', `append-to-buffer', `copy-to-buffer',
;;  `append-to-file', and `write-region', but it isn't used.  To
;;  highlight the use of those commands as if they were "regionable",
;;  uncomment the code defining and using their advice.)
;;
;;  You can show that other functions act on the region, besides those
;;  handled that way by default -
;;  see (@> "Show That Additional Commands Are Regionable").
;;
;;(@* "Controlling What Region Info To Show")
;;  ** Controlling What Region Info To Show **
;;
;;  These user options control what region size information is shown
;;  in the mode-line:
;;
;;  * `mlr-region-style' - Show the size as the number of characters,
;;    bytes, lines & characters, rows &columns, or arbitrary info you
;;    choose.  For arbitrary info, you provide a format string and a
;;    Lisp sexp whose evaluation returns the value you want to show
;;    using that format.  (Arbitrary means arbitrary: the value
;;    displayed need not have anything to do with region size.)
;;
;;    Use options `mlr-non-rectangle-style' and `mlr-rectangle-style'
;;    to specify just what to show for lines & characters and rows &
;;    columns, respectively.
;;
;;  * `mlr-non-rectangle-style' - Show the number of lines & chars, or
;;    that plus the number of words in the region.
;;
;;  * `mlr-rectangle-style' - Show the number of rectangle rows &
;;    columns, or that plus the number of words and characters.
;;
;;  * `mlr-count-partial-words-flag' - Whether to count words that
;;    straddle rectangle rows.  By default they're not counted.
;;
;;(@* "Use Mode-Line Menus To Change Behavior")
;;  ** Use Mode-Line Menus To Change Behavior **
;;
;;  When `modeline-region-mode' or `global-modeline-region-mode' is
;;  enabled, the menu shown when you click the mode-line size fields
;;  lets you do the following, regardless of whether the region is
;;  currently active.  This is in addition to the usual operations of
;;  toggling display of column and line numbers, and toggling
;;  indication of buffer size:
;;
;;   * `+ Region Info' - Command `mlr-toggle-non-rectangle-style':
;;     toggle the value of option `mlr-non-rectangle-style'.  This
;;     shows or hides the number of words in the region (in addition
;;     to the number of lines & characters).
;;
;;   * `+ Rectangle Info' - Command `mlr-toggle-rectangle-style':
;;     toggle the value of option `mlr-rectangle-style'.  This shows
;;     or hides the number of words and characters in the rectangle
;;     (in addition to the number of rows & columns).
;;  
;;   * `Choose Region Style' - Command `mlr-choose-region-style':
;;     Choose what to show in the mode-line when the region is active.
;;     That is, change the value of option `mlr-region-style'.  (The
;;     value isn't saved.  Use `M-x customize-option' to save it.)
;;
;;  When the region is active the menu also includes this item:
;;
;;   * `Count Region Contents' - Command `count-words-region': Echo
;;     the number of lines, words, and characters in the region.
;;
;;  When the active region is rectangular the menu also includes this
;;  item:
;;
;;   * `Count Rectangle Contents' - Command
;;     `mlr-count-rectangle-contents': Echo the number of rows,
;;     columns, words, and characters in the rectangle.  By default,
;;     the count excludes words that straddle the rectangle row
;;     limits; with a prefix arg those partial words are counted.
;;
;;  These items are also added to the mode-line menu for the size
;;  fields:
;;
;;   * `Show Region Info Here' - Command `modeline-region-mode'.
;;
;;   * `Show Region Info Globally' - Command
;;     `global-modeline-region-mode'.
;;
;;  This means that you can use the mode-line size menus to toggle
;;  showing region info on and off - no need to invoke the mode
;;  commands from the keyboard to do that.
;;
;;  All of these improvements to the mode-line size field menus are
;;  provided automatically.
;;
;;  By default this is done just by loading this library.  But if you
;;  prefer not to have that done except on demand, then customize
;;  option `mlr-init-menu-on-load-flag' to `nil'.  If you do that then
;;  the menus will only be enhanced when you invoke
;;  `modeline-region-mode' or `global-modeline-region-mode' for the
;;  first time.
;;
;;(@* "Highlight Large Column Numbers")
;;  ** Highlight Large Column Numbers **
;;
;;  An unrelated additional mode-line enhancement is provided by
;;  option `mlr-column-limit'.  This highlights the current-column
;;  number in the mode-line whenever it's greater than the option
;;  value, letting you know when you've gone past a certain number of
;;  characters in any line.  Turn this option off if you don't want
;;  this behavior.
;;
;;(@* "Show That Additional Commands Are Regionable")
;;  ** Show That Additional Commands Are Regionable **
;;
;;  If you want additional commands to use face
;;  `mlr-region-acting-on', to draw attention to the fact that they're
;;  currently acting on the active region, there are two ways to make
;;  this happen.  You can use either approach for any command - in
;;  other words, you can mix and match if you like.
;;
;;  1. Put non-nil property `mlr-acts-on' on the command symbol:
;;
;;       (put 'my-cmd 'mlr-acts-on t)
;;
;;  2. Advise the command, binding buffer-local variable
;;    `mlr-region-acting-on' to the value returned by `use-region-p'
;;    around the command's invocation.  Then add your advice function
;;    to hook `modeline-region-mode-hook'.
;;
;;  #1 is the simplest approach, and is likely all you need.  To make
;;  use of it, turn on option `mlr-use-property-mlr-acts-on-flag'.
;;
;;  [That option is off by default because you might never want to
;;  indicate additional regionable commands.  Also, `pre-command-hook'
;;  and `post-command-hook' are used when it's on, to check the
;;  current command for property `mlr-acts-on'.  Those hooks can
;;  sometimes slow things down or be a bother in other ways.  (That
;;  drawback isn't specific to this library.)]
;;
;;  #2 is a bit more complicated, but it gives you additional control
;;  of the mode-line behavior.  You can also advise non-interactive
;;  functions (method #1 works only for commands).
;;
;;  Here's an example of #2:
;;
;;    (advice-add 'my-cmd :around #'my-advice)
;;
;;    (defun my-advice (function &rest args)
;;      "Make mode-line show that FUNCTION acts on the active region."
;;      (cond (modeline-region-mode
;;             (let ((mlr-region-acting-on  (use-region-p)))
;;               (apply function args)))
;;            (t (apply function args))))
;;
;;    (add-hook 'modeline-region-mode-hook 'my-advice)
;;
;;  You can use the advice functions `mlr--advice-*' defined here as
;;  models, except that you'll want to make your advice functions
;;  conditional on whether `modeline-region-mode' is enabled, as in
;;  the above example.
;;
;;  [The `modeline-region-mode' code itself adds and removes the advice
;;  functions defined here, depending on whether the mode is enabled.
;;  For example, like the `my-advice' example above, function
;;  `mlr--advice-3' binds `mlr-region-acting-on' to (use-region-p),
;;  but it does that unconditionally, since it's used only when the
;;  mode is enabled.]
;;
;;  In your advice function, binding or setting `mlr-region-acting-on'
;;  to (use-region-p) is generally all that's needed, but in some
;;  cases you'll want an `:around' advice to also contain an
;;  `interactive' spec that's specific to the advised function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2021/11/25 dadams
;;     mlr-count-rectangle-contents: Use rectangle-dimensions for Emacs 27+.
;;     mlr--region-style-default: Correct number of rows.
;; 2021/11/23 dadams
;;     Added: mlr--advice-20, mlr--advice-21.
;;     modeline-region-mode: Provide advice for occur and shell-command-on-region.
;;     mlr--advice-(10-16): Removed unnecessary (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg).
;; 2021/11/21 dadams
;;     Added option mlr-use-property-mlr-acts-on-flag and functions mlr-pre-cmd, mlr-post-cmd.
;;     modeline-region-mode: Add&remove pre&post command hooks if mlr-use-property-mlr-acts-on-flag.
;;     Improved documentation in Commentary.
;; 2021/11/15 dadams
;;     mlr-set-default-mode-line-position: Removed redundant ignore-errors around mlr-show-region-p.
;; 2021/11/12 dadams
;;     Added: mlr-init-menu-on-load-flag, mlr-set-default-mode-line-position, mlr--mlp-is-set-up-p.
;;     modeline-region-mode:
;;       Set up mode-line-position menus only if not yet done.  Do it using
;;       mlr-set-default-mode-line-position.  Use mode-line-percent-position if defined (Emacs 26+).
;;     mlr--orig-mode-line-position: Initialize to mode-line-position value, not nil.
;;     mlr-menu: Add items (global-)modeline-region-mode.  Remove :visible.  Set :enable to
;;               (and size-indication-mode modeline-region-mode) or
;;               (and size-indication-mode  (or modeline-region-mode  global-modeline-region-mode)).
;; 2021/11/09 dadams
;;     Added: mlr--orig-mlp-local-p.
;;     mlr--orig-mode-line-position, mlr-region-acting-on: Use defvar-local.
;;     modeline-region-mode: Restore mode-line-position, local or global (use mlr--orig-mlp-local-p).
;;     (global-)modeline-region-mode: Added autoload cookie.
;;     mlr--region-style-alist: Removed unneeded autoload cookie.
;;     Removed unused mlr--set-var.
;; 2021/11/07 dadams
;;     mlr-show-region-p: If empty region then return nil if mouse-1 is pressed.
;;     mlr-menu: Reordered, to put Showing items together.
;; 2021/11/05 dadams
;;     Created, based on previous library modeline-posn.el - its change log is below...
;;
;; ----------
;;
;; modeline-posn.el change log:
;;
;; 2021/10/27 dadams
;;     isearch-mode advice: Added boundp for isearchp-reg-beg, just to avoid byte-compile warning.
;; 2021/10/26 dadams
;;     isearch-mode advice: Use isearchp-reg-beg, not use-region-p.
;; 2021/10/17 dadams
;;     Removed support for Emacs 22 and 23.
;;       modelinepos-show-region-p: Removed Emacs 24+ test.
;;       mode-line-position: Removed Emacs 22 version.
;;       use-empty-active-region, use-region-p: Removed (Emacs 22 only).
;;     Require rect.el for Emacs 26+.
;;     Added: modelinepos-toggle-region-style, modelinepos-toggle-rectangle-style, modelinepos-menu.
;;     Bind modelinepos-menu to [mode-line down-mouse-1].
;; 2021/10/16 dadams
;;     Added: modelinepos-(bytes|chars)-format, modelinepos-(lines|rows)+cols(+words+chars)-format.
;;     modelinepos-style(-default): Use those new vars, instead of hardcoding the formats.
;; 2021/10/10 dadams
;;     count-words-rectangle: Finished correction to count.
;; 2021/10/09 dadams
;;     Added: modelinepos-region-style, count-words-rectangle, modelinepos-style-default.
;;     modelinepos-style: Support modelinepos-region-style.  For non-rect, put lines before cols.
;;     modelinepos-rectangle-style: Use quote, not backquote.
;;     count-words-rectangle: (partial) correction to count.
;; 2021/09/23 dadams
;;     Added: modelinepos-rectangle-style.
;;     modelinepos-style: Added support for modelinepos-rectangle-style (words & chars in rectangle).
;;                        Fixed typo that prevented Bytes from working.
;; 2021/08/25 dadams
;;     mode-line-position: Updated for option column-number-indicator-zero-based (new in Emacs 26).
;; 2020/10/15 dadams
;;     Removed advice for replace-dehighlight.  Do it in around advice for perform-replace.
;;     perform-replace advice: Change to around advice, and bind, don't set,
;;       modelinepos-region-acting-on.
;; 2017/02/27 dadams
;;     modelinepos-style: Wrap calls to rectangle--pos-cols with save-excursion (Emacs bug #25777).
;; 2017/02/19 dadams
;;     isearch-query-replace-regexp:
;;       Check that isearchp-reg-beg is boundp before using it in remove-hook.  Thx to Yuri D'Elia.
;;       Ref: https://lists.gnu.org/archive/html/emacs-devel/2017-02/msg00637.html
;; 2017/02/18 dadams
;;     rectangle-number-lines: Typo rectange -> rectangle.  Thx to Charles Roelli.
;; 2017/01/14 dadams
;;     Added vacuous defvars for isearchp-reg-end, isearchp-restrict-to-region-flag.
;; 2017/01/08 dadams
;;     modelinepos-style: Simplified. 
;; 2017/01/07 dadams
;;     modelinepos-style: Added style # bytes.
;; 2014/07/21 dadams
;;     Added: modelinepos-rect-p, cua-rectangle-mark-mode, rectangle-number-lines,
;;            rectangle-mark-mode, string-insert-rectangle, string-rectangle.
;;     modelinepos-style: Respect modelinepos-rect-p, showing rows & cols.
;;     register-read-with-preview: Bind modelinepos-rect-p.
;;                                 Added copy-rectangle-to-register to cms affected.
;; 2014/07/15 dadams
;;     Advise functions append-to-buffer, prepend-to-buffer, copy-to-buffer, append-to-file,
;;       register-read-with-preview, copy-to-register, append-to-register, prepend-to-register,
;;       write-region.
;; 2014/01/18 dadams
;;     Added: modelinepos-region-acting-on (face and var), 
;;            use-region-p (Emacs 22), use-empty-active-region (Emacs 22).
;;     Renamed: modelinepos-empty-region-p to modelinepos-show-region-p.
;;     modelinepos-show-region-p: Wrapped in condition-case, to ignore errors.
;;     mode-line-position (Emacs 23+):
;;       Show modelinepos-region-acting-on highlight (cmd restricted to region).
;;     Advise Isearch and replacement functions:
;;       keep-lines-read-args, map-query-replace-regexp, perform-replace, query-replace-read-args,
;;       query-replace-read-from, query-replace-read-to, replace-dehighlight.
;; 2013/11/23 dadams
;;     Added: modelinepos-empty-region-p.
;;     Use modelinepos-empty-region-p to decide whether region is empty.
;;     modelinepos-empty-region-flag: Change default value to t.
;; 2013/04/19 dadams
;;     Added: modelinepos-empty-region-flag.  Use it in mode-line-position.
;; 2013/02/01 dadams
;;     Do not show size of active region in mode line if it is empty.
;; 2012/05/25 dadams
;;     Added face modelinepos-region and option modelinepos-style.
;;     Updated mode-line-position accordingly.  Thx to Jonathan Kotta for the suggestion.
;; 2011/01/04 dadams
;;     Added autoload cookies for defface, defcustom, and command.
;; 2009/06/11 dadams
;;     Added Emacs 23 version.
;; 2007/04/02 dadams
;;     Added modelinepos-column-warning.  Thx to AmitPatel for the suggestion.
;; 2006/09/14 dadams
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

(when (> emacs-major-version 25) (require 'rect)) ; extract-rectangle-bounds

(unless (featurep 'rect) ;; Emacs 24: need to load it explicitly.
  (require 'rect)) ;; rectangle-mark-mode, rectangle-number-lines,
  ;; rectangle--default-line-number-format, rectangle--pos-cols, rectangle--string-erase-preview,
  ;; rectangle--string-preview

(require 'isearch+ nil t) ;; isearchp-reg-beg, isearchp-reg-end, isearchp-restrict-to-region-flag

;; Quiet the byte-compiler.
;;
(defvar cua-rectangle-mark-mode)          ; In `cua-rect.el'
(defvar isearchp-reg-beg)                    ; In `isearch+.el'
(defvar isearchp-reg-end)                    ; In `isearch+.el'
(defvar isearchp-restrict-to-region-flag)    ; In `isearch+.el'
(defvar mlr-rectangle-style)                 ; Here, for Emacs 26+
(defvar rectangle--inhibit-region-highlight) ; In `rect.el' for Emacs 25+
(defvar rectangle-mark-mode)                 ; In `rect.el'
(defvar rectangle--string-preview-state)     ; In `rect.el' for Emacs 25+
(defvar rectangle--string-preview-window)    ; In `rect.el' for Emacs 25+
(defvar string-rectangle-history)            ; In `rect.el' for Emacs 25+

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup modeline-region ()
  "Show region information in the mode-line.
More generally, enhance `mode-line-position'."
  :prefix 'mlr :group 'Modeline :group 'Convenience :group 'Help)

;;;###autoload
(defface mlr-column-warning '((t (:foreground "Red")))
  "Face used to highlight the modeline column number.
This is used when the current column number is greater than
`mlr-column-limit'.
Used by `modeline-region-mode'."
  :group 'modeline-region :group 'faces)

;;;###autoload
(defface mlr-region '((t :inherit region))
  "Face used to highlight the modeline position and size when
the region is active.
Used by `modeline-region-mode'."
  :group 'modeline-region :group 'faces)

;;;###autoload
(defface mlr-region-acting-on '((t (:inherit region :box (:line-width 3 :color "Red"))))
  "Face for modeline position & size when a command acts on active region.
Used by `modeline-region-mode'."
  :group 'modeline-region :group 'faces)

;;;###autoload
(defcustom mlr-column-limit 70
  "Current column greater than this means highlight column in mode-line.
Used by `modeline-region-mode'."
  :group 'modeline-region
  :type '(choice
          (const   :tag "NO limit: don't highlight column number of long lines" nil)
          (integer :tag "Maximum unhighlighted current-column number")))

;;;###autoload
(defcustom mlr-count-partial-words-flag nil
  "Non-nil means count words that straddle rectangle rows.
By default (nil value), count only words that are entirely within the
rectangle.
Used by `modeline-region-mode'."
    :group 'modeline-region :type 'boolean)

;;;###autoload
(defcustom mlr-empty-region-flag t
  "Non-nil means indicate an active region even when empty.
Used by `modeline-region-mode'."
  :group 'modeline-region :type 'boolean)

;;;###autoload
(defcustom mlr-init-menu-on-load-flag t
  "Non-nil means set up menus when `modeline-region.el' is loaded.
That is, when you load the library initialize `mode-line-position'
menus to include items relevant to `modeline-region-mode'.

If nil then the menus don't reflect such items until you invoke that
command or its globalized version, `global-modeline-region-mode'"
  :group 'modeline-region :type 'boolean)

;;;###autoload
(defcustom mlr-non-rectangle-style 'lines+chars
  "Mode-line info about region size, except when rectangular.
Used by `modeline-region-mode'."
  :group 'modeline-region
  :type '(choice
          (const :tag "Lines and characters"    lines+chars)
          (const :tag "Lines, words, and chars" lines+words+chars)))

(when (fboundp 'extract-rectangle-bounds) ; Emacs 26+

  (defcustom mlr-rectangle-style 'rows+cols
    "Mode-line info about region size when a rectangle is selected.
Used by `modeline-region-mode'."
    :group 'modeline-region
    :type '(choice
            (const :tag "Rows and columns"                rows+cols)
            (const :tag "Rows, columns, words, and chars" rows+cols+words+chars)))

  (defun mlr-toggle-rectangle-style (&optional msgp)
    "Toggle value of option `mlr-rectangle-style'."
    (interactive "p")
    (setq mlr-rectangle-style  (if (eq mlr-rectangle-style 'rows+cols)
                                   'rows+cols+words+chars
                                 'rows+cols))
    (force-mode-line-update 'ALL)
    (when msgp (message "`mlr-rectangle-style' is now `%s'" mlr-rectangle-style)))

  (defun mlr-count-rectangle-contents (start end &optional count-partial-words-p msgp)
    "List the number of rows, columns, words, and chars for a rectangle.
Return a list of the number of rows (lines), columns (chars per row),
words, and chars.  If called interactively, echo those numbers.

Option `mlr-count-partial-words-flag' controls whether to count words
that straddle the beginning or end of a rectangle row.  A prefix arg
flips the behavior specified by the option value.

When called from Lisp:

* START and END are the top-left and bottom-right corner positions.
* Non-nil COUNT-PARTIAL-WORDS-P means count partial words (option
  `mlr-count-partial-words-flag'is ignored).
* Non-nil MSGP means echo the counts."
    (interactive
     (list (region-beginning)
           (region-end)
           (if current-prefix-arg (not mlr-count-partial-words-flag) mlr-count-partial-words-flag)
           t))
    (declare-function rectangle--pos-cols "rect.el" (start end &optional window) 'FILEONLY)
    (declare-function rectangle--string-preview "rect.el" () 'FILEONLY)
    (declare-function rectangle--default-line-number-format "rect.el" (start end start-at) 'FILEONLY)
    (let* ((dims    (and (fboundp 'rectangle-dimensions) ; Emacs 27+
                         (rectangle-dimensions start end)))
           (rows    (if dims
                        (cdr dims)
                      (1+ (abs (- (line-number-at-pos end) (line-number-at-pos start))))))
           (cols    (if dims
                        (car dims)
                      (let ((rpc  (save-excursion (rectangle--pos-cols start end))))
                        (abs (- (car rpc) (cdr rpc))))))
           (words   0)
           (chars   0)
           (bounds  (extract-rectangle-bounds start end))
           this-ws beg end)
      (dolist (beg+end  bounds)
        (setq beg      (car beg+end)
              end      (cdr beg+end)
              this-ws  (count-words beg end)
              words    (+ words this-ws)
              chars    (+ chars (- end beg)))
        (unless count-partial-words-p
          (when (and (not (zerop this-ws))
                     (char-after (1- beg))  (equal '(2) (syntax-after (1- beg)))
                     (char-after beg)       (equal '(2) (syntax-after beg)))
            (setq words    (1- words)
                  this-ws  (1- this-ws)))
          (when (and (not (zerop this-ws))
                     (char-after (1- end))  (equal '(2) (syntax-after (1- end)))
                     (char-after end)       (equal '(2) (syntax-after     end)))
            (setq words  (1- words)))))
      (when msgp
        (message "Rectangle has %d row%s, %d colum%s, %d word%s, and %d char%s."
                 rows  (if (= rows 1)  "" "s")
                 cols  (if (= cols 1)  "" "s")
                 words (if (= words 1) "" "s")
                 chars (if (= chars 1) "" "s")))
      (list rows cols words chars)))
  )

(defvar mlr-chars-format " %d Chars"
  "Format string to indicate the number of characters.
Used for `mlr-region-style'.
It should start with a SPC char and expect that single value.")

(defvar mlr-bytes-format " %d Bytes"
  "Format string to indicate the number of bytes.
Used for `mlr-region-style'.
It should start with a SPC char and expect that single value.")

(defvar mlr-lines+chars-format " %d lines, %d chars"
  "Format string for the number of lines and chars in region.
Used for `mlr-non-rectangle-style'.
It should start with a SPC char and expect those two input values.")

(defvar mlr-lines+words+chars-format " %d lines, %d words, %d chars"
  "Format string for the number of lines, words, and chars in region.
Used for `mlr-non-rectangle-style'.
It should start with a SPC char and expect those thres input values.")

(defvar mlr--mlp-is-set-up-p nil
  "Non-nil means `mode-line-position' menus have been set up.
That is, they include entries for `modeline-region-mode'.
\(This set-up only needs to be done once.)")

(defvar-local mlr--orig-mlp-local-p nil
  "(local-variable-p 'mode-line-position) before `modeline-region-mode'.")

(defvar-local mlr--orig-mode-line-position mode-line-position
  "Value of `mode-line-position' before turning on `modeline-region-mode'.")

(defvar mlr-rect-p nil
  "Non-nil means the current command is a rectangle command.")

(defvar-local mlr-region-acting-on nil
  "Non-nil means that Emacs is currently acting on the active region.
It is the responsibility of an individual command to manage the value.
This can be done by advising the command to bind this variable or, if
option `mlr-use-property-mlr-acts-on-flag' is non-nil, by putting a
non-nil property `mlr-acts-on' on the command symbol.")

(defvar mlr-rows+cols-format " %d rows, %d cols"
  "Format string for the number of rows and columns in rectangle.
Used for `mlr-rectangle-style' (Emacs 26+).
It should start with a SPC char and expect those two input values.")

(defvar mlr-rows+cols+words+chars-format " %d rows, %d cols, %d words, %d chars"
  "Format string for the number of rows, columns, words, and chars.
Used for `mlr-rectangle-style' (Emacs 26+).
It should start with a SPC char and expect those four input values.")

;;;###autoload
(defconst mlr--region-style-default
  '(;; Format string
    (if mlr-rect-p
        (if (and (boundp 'mlr-rectangle-style)
                 (eq 'rows+cols+words+chars mlr-rectangle-style))
            mlr-rows+cols+words+chars-format ; " %d rows, %d cols, %d words, %d chars"
          mlr-rows+cols-format)              ;" %d rows, %d cols"
      (if (eq 'lines+words+chars mlr-non-rectangle-style)
          mlr-lines+words+chars-format ; " %d lines, %d words, %d chars"
        mlr-lines+chars-format))       ; " %d lines, %d chars"

    ;; Rows (rectangle) / Lines (region)
    (if mlr-rect-p
        (1+ (abs (- (line-number-at-pos (region-end)) ; Rows (rectangle)
                    (line-number-at-pos (region-beginning)))))
      (count-lines (region-beginning) (region-end))) ; Lines (region)

    ;; Columns (rectangle) / Chars (short region) or Words (full region)
    (if mlr-rect-p
        (let ((rpc  (save-excursion (rectangle--pos-cols (region-beginning) (region-end)))))
          (abs (- (car rpc) (cdr rpc)))) ; Columns (rectangle)
      (if (eq 'lines+words+chars mlr-non-rectangle-style)
          (count-words (region-beginning) (region-end)) ; Words (full region)
        (- (region-end) (region-beginning)))) ; Chars (short region)

    ;; Words (rectangle) / Chars (full region)
    (if (and mlr-rect-p
             (boundp 'mlr-rectangle-style)
             (eq 'rows+cols+words+chars mlr-rectangle-style))
        (let ((words  0)
              beg end this-ws)
          (dolist (beg+end  (extract-rectangle-bounds (region-beginning) (region-end)))
            (setq beg      (car beg+end)
	          end      (cdr beg+end)
	          this-ws  (count-words beg end)
	          words    (+ words this-ws))
            (unless mlr-count-partial-words-flag
              (when (and (not (zerop this-ws))
		         (char-after (1- beg))  (equal '(2) (syntax-after (1- beg)))
		         (char-after beg)       (equal '(2) (syntax-after beg)))
	        (setq words    (1- words)
                      this-ws  (1- this-ws)))
              (when (and (not (zerop this-ws))
		         (char-after (1- end))  (equal '(2) (syntax-after (1- end)))
		         (char-after end)       (equal '(2) (syntax-after     end)))
	        (setq words  (1- words)))))
          words)                        ; Words (rectangle)
      (and (eq 'lines+words+chars mlr-non-rectangle-style)
           (- (region-end) (region-beginning)))) ; Chars (full region)

    ;; Chars (rect)
    (and mlr-rect-p
         (boundp 'mlr-rectangle-style)
         (eq 'rows+cols+words+chars mlr-rectangle-style)
         (let ((chars  0)
               beg end)
           (dolist (beg+end  (extract-rectangle-bounds (region-beginning) (region-end)))
             (setq beg    (car beg+end)
                   end    (cdr beg+end)
                   chars  (+ chars (- end beg))))
	   chars)))                     ; Chars (rectangle)
  "Default value for option `mlr-region-style'.
It corresponds to the Customize `Value Menu' choice
`Lines (& words) & chars / rows & columns (& words & chars)'.")

;; This constant value must be identical to the code used in the `:type' spec
;; of the `mlr-region-style' `defcustom'.
;;
(defconst mlr--region-style-alist
  `((chars (mlr-chars-format
            (if (and mlr-rect-p  (fboundp 'extract-rectangle-bounds)) ; Emacs 26+
                (let ((chars  0)
                      beg end)
                  (dolist (beg+end  (extract-rectangle-bounds (region-beginning) (region-end)))
                    (setq beg    (car beg+end)
                          end    (cdr beg+end)
                          chars  (+ chars (- end beg))))
                  chars)
              (- (region-end) (region-beginning)))))
    (bytes (mlr-bytes-format
            (if (and mlr-rect-p  (fboundp 'extract-rectangle-bounds)) ; Emacs 26+
                (let ((bytes  0)
                      beg end)
                  (dolist (beg+end  (extract-rectangle-bounds (region-beginning) (region-end)))
                    (setq beg    (car beg+end)
                          end    (cdr beg+end)
                          bytes  (+ bytes (string-bytes (buffer-substring-no-properties beg end)))))
                  bytes)
              (string-bytes (buffer-substring-no-properties (region-beginning) (region-end))))))
    (lines/rows\ &\ chars/cols ,mlr--region-style-default))
  "Alist for `mlr-choose-region-style'.")

;;;###autoload
(defvar modeline-region-mode-map (make-sparse-keymap)
  "Keymap for minor mode `modeline-region-mode'")

(defun mlr-choose-region-style ()
  "Change option `mlr-region-style' to a style you choose.
The option value is changed, but it's not saved.
Use `\\[customize-option]' to save it."
  (interactive)
  (let ((new  (completing-read "Change mode-line region to style: " mlr--region-style-alist nil t)))
    (customize-set-variable 'mlr-region-style (cadr (assq (intern new) mlr--region-style-alist)))))

;;;###autoload
(defcustom mlr-region-style mlr--region-style-default
  "Mode-line info about the active region.
Choose a style from the `Value Menu':

 * `Characters'.  Number of characters in region or rectangle.

 * `Bytes'.  Number of bytes in region or rectangle.

 * `Lines (& words) & chars / rows & columns (& words & chars)'.
   For a regular region, you see lines and characters.
   For a rectangular region you see rows and columns.

   This can optionally include words for region and words & chars for
   rectangle: see options `mlr-non-rectangle-style' and
   `mlr-rectangle-style' (for Emacs 26+).

   To change the concrete formatting used, change variables
   `mlr-lines+chars-format', `mlr-lines+words+chars-format',
   `mlr-rows+cols-format', and `mlr-rows+cols+words+chars-format'.

 * `Customized format'.  An arbitrary format you specify."
  :group 'modeline-region
  :type
  `(choice

    (const :tag "Characters"
           (mlr-chars-format
            (if (and mlr-rect-p  (fboundp 'extract-rectangle-bounds)) ; Emacs 26+
                (let ((chars  0)
                      beg end)
                  (dolist (beg+end  (extract-rectangle-bounds (region-beginning) (region-end)))
                    (setq beg    (car beg+end)
                          end    (cdr beg+end)
                          chars  (+ chars (- end beg))))
                  chars)
              (- (region-end) (region-beginning)))))

    ;; Should we use this instead, for calculating bytes?  It can sometimes be costly.
    ;; See https://emacs.stackexchange.com/a/29912/105.
    ;; (const :tag "Bytes: \"_ bytes\""
    ;;  (" %d bytes"
    ;;   (if (fboundp 'bufferpos-to-filepos) ; Emacs 25+
    ;;       (- (bufferpos-to-filepos (region-end) 'exact)
    ;;          (bufferpos-to-filepos (region-beginning) 'exact))
    ;;     (string-bytes (buffer-substring-no-propertiesw (region-beginning) (region-end))))))
    ;;
    (const :tag "Bytes"
           (mlr-bytes-format
            (if (and mlr-rect-p  (fboundp 'extract-rectangle-bounds)) ; Emacs 26+
                (let ((bytes  0)
                      beg end)
                  (dolist (beg+end  (extract-rectangle-bounds (region-beginning) (region-end)))
                    (setq beg    (car beg+end)
                          end    (cdr beg+end)
                          bytes  (+ bytes (string-bytes (buffer-substring-no-properties beg end)))))
                  bytes)
              (string-bytes (buffer-substring-no-properties (region-beginning) (region-end))))))

    ;; This is just the single value that's the value of `mlr--region-style-default'.
    (restricted-sexp :tag "Lines (& words) & chars / rows & columns (& words & chars)"
                     :match-alternatives ((lambda (val) (equal val mlr--region-style-default))))

    (list :tag "Customized format"
          (string :tag "Format string")
          (repeat :inline t (sexp :tag "Sexp argument for format string")))))

;;;###autoload
(defcustom mlr-use-property-mlr-acts-on-flag nil
  "Whether property `mlr-acts-on' says a command acts on active region.
This affects the behavior of minor mode `modeline-region-mode'.

A non-nil value of property `mlr-acts-on' on a command symbol means
highlight the mode-line when the command is invoked, to indicate that
the command acts on the active region.

Use the Customize UI or function `customize-set-variable', not `setq',
to change the value of this option during an Emacs session.  This
turns off `global-modeline-region-mode' everywhere and reinitializes
command `modeline-region-mode', so that when you use that command
again it respects the new value of the option properly.

NOTE: It's easy to put property `mlr-acts-on' on a command symbol.
But it's generally better, and more flexible, to instead advise the
command, binding buffer-local variable `mlr-region-acting-on' to the
value returned by `use-region-p', for the duration of the command.
This is because the put-property-on-symbol method makes use of `pre-'
and `post-command-hook', which can sometimes slow things down or
otherwise be a bother."
  :group 'modeline-region :type 'boolean
  :set (lambda (sym defs)
         (custom-set-default sym defs)
	 (when (boundp 'global-modeline-region-mode)
	   (global-modeline-region-mode -1))))

(defun mlr-show-region-p ()
  "Whether to show mode-line region info and highlighting.
Return non-nil if the region is active and nonempty, or emptiness is
OK.  Option `mlr-empty-region-flag' non-nil means emptiness is OK.

More precisely, return non-nil for the active region if either of
these conditions is true:
 * The region is not empty.
 * Option `mlr-empty-region-flag' is non-nil, and the last input did
   not use `mouse-1'.

The `mouse-1' test prevents highlighting the mode line whenever you
press `mouse-1' without dragging at least one character."
  (ignore-errors (and (region-active-p)
                      (or (> (region-end) (region-beginning))
                          (and mlr-empty-region-flag
                               (not (eq 'down-mouse-1 (car-safe last-input-event)))
                               (not (mouse-movement-p last-input-event)))))))

;;; Add commands to mode-line menu.
;;;
(defvar mlr-menu (let ((map  (make-sparse-keymap "Position/Size Information")))
                   (when (fboundp 'mlr-count-rectangle-contents) ; Emacs 26+
                     (define-key map [mlr-count-rectangle-contents]
                       '(menu-item "Count Rectangle Contents" mlr-count-rectangle-contents
                                   :help "Show number of rows, columns, words, and chars in rectangle"
                                   :enable (and size-indication-mode
                                                modeline-region-mode
                                                mlr-rect-p))))
                   (define-key map [count-words-region]
                     '(menu-item "Count Region Contents" count-words-region
                                 :help "Show number of lines, words, and chars in active region"
                                 :enable (and size-indication-mode
                                              modeline-region-mode
                                              (use-region-p))))
                   (define-key map [mlr-choose-region-style]
                     '(menu-item "Choose Region Style" mlr-choose-region-style
                                 :enable (and size-indication-mode
                                              (or modeline-region-mode
                                                  global-modeline-region-mode))
                                 :help "Change the value of option `mlr-region-style'"))
                   (when (fboundp 'mlr-toggle-rectangle-style) ; Emacs 26+
                     (define-key map [mlr-toggle-rectangle-style]
                       '(menu-item "+ Rectangle Info" mlr-toggle-rectangle-style
                                   :help "Toggle value of option `mlr-rectangle-style'"
                                   :enable (and size-indication-mode
                                                (or modeline-region-mode
                                                    global-modeline-region-mode))
                                   :button (:toggle . (eq 'rows+cols+words+chars
                                                          mlr-rectangle-style)))))
                   (define-key map [mlr-toggle-non-rectangle-style]
                     '(menu-item "+ Region Info" mlr-toggle-non-rectangle-style
                                 :help "Toggle value of option `mlr-non-rectangle-style'"
                                 :enable (and size-indication-mode
                                              (or modeline-region-mode
                                                  global-modeline-region-mode))
                                 :button (:toggle . (eq 'lines+words+chars
                                                        mlr-non-rectangle-style))))
                   (define-key map [separator-region-info] '("--"))
                   (define-key map [global-modeline-region-mode]
                     '(menu-item "Show Region Info Globally" global-modeline-region-mode
                                 :help "Toggle `global-modeline-region-mode'"
                                 :enable size-indication-mode
                                 :button (:toggle . global-modeline-region-mode)))
                   (define-key map [modeline-region-mode]
                     '(menu-item "Show Region Info Here" modeline-region-mode
                                 :help "Toggle `modeline-region-mode'"
                                 :enable size-indication-mode
                                 :button (:toggle . modeline-region-mode)))
                   (define-key map [size-indication-mode]
                     '(menu-item "Show Size Indication" size-indication-mode
                                 :help "Toggle displaying a size indication in the mode-line"
                                 :button (:toggle . size-indication-mode)))
                   (define-key map [line-number-mode]
                     '(menu-item "Show Line Numbers" line-number-mode
                                 :help "Toggle displaying line numbers in the mode-line"
                                 :button (:toggle . line-number-mode)))
                   (define-key map [column-number-mode]
                     '(menu-item "Show Column Numbers" column-number-mode
                                 :help "Toggle displaying column numbers in the mode-line"
                                 :button (:toggle . column-number-mode)))
                   (define-key map [mode-line down-mouse-1] map)
                   map)
  "Menu keymap for `modeline-region.el' features.
Used in place of `mode-line-column-line-number-mode-map'.")

(defun mlr-toggle-non-rectangle-style (&optional msgp)
  "Toggle value of option `mlr-non-rectangle-style'."
  (interactive "p")
  (setq mlr-non-rectangle-style  (if (eq mlr-non-rectangle-style 'lines+chars)
                                     'lines+words+chars
                                   'lines+chars))
  (force-mode-line-update 'ALL)
  (when msgp (message "`mlr-non-rectangle-style' is now `%s'" mlr-non-rectangle-style)))

(defun mlr-set-default-mode-line-position ()
  "Set up default `mode-line-position' menus for `modeline-region-mode'.
Sets `mlr--mlp-is-set-up-p' to t, to show the menus have been set up."
  (setq-default mode-line-position
                '(:eval
                  `((:propertize
                     (if (boundp 'mode-line-percent-position) mode-line-percent-position (-3 "p"))
                     local-map mlr-menu
                     mouse-face mode-line-highlight
                     help-echo "Buffer position, mouse-1: Line/col menu")
                    ;; We might be able to remove one or more of these `ignore-error's,
                    ;; but it seems better to keep them, at least for now.
                    (size-indication-mode
                     (8 ,(propertize
                          (if (and modeline-region-mode
                                   (or mlr-region-acting-on  (mlr-show-region-p)))
                              (or (ignore-errors
                                    (apply #'format (mapcar #'eval mlr-region-style)))
                                  "")
                            " of %I")
                          'face (and modeline-region-mode
                                     (if mlr-region-acting-on
                                         'mlr-region-acting-on
                                       (and (mlr-show-region-p)  'mlr-region)))
                          'local-map mlr-menu
                          'mouse-face 'mode-line-highlight
                          'help-echo "Buffer position, mouse-1: Line/col menu")))
                    (line-number-mode
                     ((column-number-mode ; Line-number mode & column-number-mode
                       (column-number-indicator-zero-based
                        (10 ,(propertize
                              " (%l,%c)"
                              'face (and modeline-region-mode
                                         mlr-column-limit
                                         (> (current-column) mlr-column-limit)
                                         'mlr-column-warning)
                              'local-map mlr-menu
                              'mouse-face 'mode-line-highlight
                              'help-echo "Line and column, mouse-1: Line/col menu"))
                        (10 ,(propertize
                              " (%l,%C)"
                              'face (and modeline-region-mode
                                         mlr-column-limit
                                         (> (current-column) mlr-column-limit)
                                         'mlr-column-warning)
                              'local-map mlr-menu
                              'mouse-face 'mode-line-highlight
                              'help-echo "Line number, mouse-1: Line/col menu")))
                       (6 ,(propertize
                            " L%l"
                            'local-map mlr-menu
                            'mouse-face 'mode-line-highlight
                            'help-echo "Line number, mouse-1: Line/col menu"))))
                     ((column-number-mode ; Column-number-mode only, not line-number mode
                       (column-number-indicator-zero-based
                        (5 ,(propertize
                             " C%c"
                             'face (and modeline-region-mode
                                        mlr-column-limit
                                        (> (current-column) mlr-column-limit)
                                        'mlr-column-warning)
                             'local-map mlr-menu
                             'mouse-face 'mode-line-highlight
                             'help-echo "Column number, mouse-1: Line/col menu"))
                        (5 ,(propertize
                             " C%C"
                             'face (and modeline-region-mode
                                        mlr-column-limit
                                        (> (current-column) mlr-column-limit)
                                        'mlr-column-warning)
                             'local-map mlr-menu
                             'mouse-face 'mode-line-highlight
                             'help-echo "Column number, mouse-1: Line/col menu")))))))))
  (setq mlr--mlp-is-set-up-p  t)) ; Flag to indicate this has been done.

;; Set up the menus when this library is loaded (default behavior).
;;
(when mlr-init-menu-on-load-flag (mlr-set-default-mode-line-position))

(defun mlr-pre-cmd ()
  "Update `mlr-region-acting-on' per property `mlr-acting-on'.
Does nothing if `mlr-use-property-mlr-acts-on-flag' is nil."
  (when mlr-use-property-mlr-acts-on-flag
    (setq mlr-region-acting-on  (and (symbolp this-command)  (get this-command 'mlr-acts-on)))
    (force-mode-line-update)))

(defun mlr-post-cmd ()
  "Reset `mlr-region-acting-on' if property `mlr-acting-on' was used."
  (when (and (symbolp this-command)  (get this-command 'mlr-acts-on))
    (setq mlr-region-acting-on  nil)
    (force-mode-line-update)))

;;;###autoload
(define-minor-mode modeline-region-mode
  "Toggle showing region information in the mode-line.
The information shown depends on options `mlr-region-style',
`mlr-non-rectangle-style' and `mlr-rectangle-style' (for Emacs 26+).

\(This mode automatically turns on `size-indication-mode'.)"
  :group 'modeline-region :init-value nil :keymap modeline-region-mode-map
  (cond (modeline-region-mode
         (size-indication-mode 1)
         (unless mlr--mlp-is-set-up-p (mlr-set-default-mode-line-position))
         (setq mlr--orig-mlp-local-p  (local-variable-p 'mode-line-position))
         (make-local-variable 'mode-line-position)
         (setq mlr--orig-mode-line-position  mode-line-position)
	 (when mlr-use-property-mlr-acts-on-flag
           (add-hook 'pre-command-hook 'mlr-pre-cmd nil 'LOCAL)
           (add-hook 'post-command-hook 'mlr-post-cmd nil 'LOCAL))
         (advice-add 'replace-dehighlight :after #'mlr--advice-1)
         (advice-add 'query-replace-read-args :around #'mlr--advice-2)
         (advice-add 'query-replace-read-to :around #'mlr--advice-2)
         (advice-add 'perform-replace :around #'mlr--advice-2)
         (advice-add 'query-replace-read-from :around #'mlr--advice-3)
         (advice-add 'keep-lines-read-args :around #'mlr--advice-3)
         (advice-add 'occur :around #'mlr--advice-21)
         (advice-add 'map-query-replace-regexp :around #'mlr--advice-4)
         ;;; (advice-add 'prepend-to-buffer :around 'mlr--advice-5)
         ;;; (advice-add 'append-to-buffer :around 'mlr--advice-6)
         ;;; (advice-add 'copy-to-buffer :around 'mlr--advice-7)
         ;;; (advice-add 'append-to-file :around 'mlr--advice-8)
         ;;; (advice-add 'write-region :around 'mlr--advice-9)
         (if (fboundp 'register-read-with-preview)
             ;; Emacs 24.4+.  Used by all register-reading cmds, but restrict highlighting
             ;;               to those affecting the region.
             (advice-add 'register-read-with-preview :around 'mlr--advice-10)
           ;; Emacs 24.3 - no `register-read-with-preview'.
           (advice-add 'copy-to-register :around 'mlr--advice-11)
           (advice-add 'prepend-to-register :around 'mlr--advice-12)
           (advice-add 'append-to-register :around 'mlr--advice-13))
         (advice-add 'string-rectangle :around 'mlr--advice-14)
         (advice-add 'string-insert-rectangle :around 'mlr--advice-15)
         (advice-add 'rectangle-number-lines :around 'mlr--advice-16)
         ;; These change the mode-line style to rectangular.
         ;; (Don't show the barred face just because the rectangle is active.)
         (when (fboundp 'rectangle-mark-mode) ; Emacs 24.4+
           (add-hook 'rectangle-mark-mode-hook (lambda () (setq mlr-rect-p  rectangle-mark-mode))))
         (when (fboundp 'cua-rectangle-mark-mode) ; Emacs 24.4+, but see Emacs bug #18047
           (add-hook 'cua-rectangle-mark-mode-hook (lambda () (setq mlr-rect-p  cua-rectangle-mark-mode))))
         (advice-add 'isearch-mode :after 'mlr--advice-17)
         (advice-add 'isearch-query-replace :around 'mlr--advice-18)
         (advice-add 'isearch-query-replace-regexp :around 'mlr--advice-19)
         (advice-add 'shell-command-on-region :around 'mlr--advice-20))
        (t
         (if mlr--orig-mlp-local-p
             (setq-local mode-line-position  mlr--orig-mode-line-position)
           (set (kill-local-variable 'mode-line-position) mlr--orig-mode-line-position))
	 (when mlr-use-property-mlr-acts-on-flag
           (remove-hook 'pre-command-hook 'mlr-pre-cmd 'LOCAL)
           (remove-hook 'post-command-hook 'mlr-post-cmd 'LOCAL))
         (advice-remove 'replace-dehighlight #'mlr--advice-1)
         (advice-remove 'query-replace-read-args #'mlr--advice-2)
         (advice-remove 'query-replace-read-to #'mlr--advice-2)
         (advice-remove 'perform-replace #'mlr--advice-2)
         (advice-remove 'query-replace-read-from #'mlr--advice-3)
         (advice-remove 'keep-lines-read-args #'mlr--advice-3)
         (advice-remove 'occur #'mlr--advice-21)
         (advice-remove 'map-query-replace-regexp #'mlr--advice-4)
         ;;; (advice-remove 'prepend-to-buffer 'mlr--advice-5)
         ;;; (advice-remove 'append-to-buffer 'mlr--advice-6)
         ;;; (advice-remove 'copy-to-buffer 'mlr--advice-7)
         ;;; (advice-remove 'append-to-file 'mlr--advice-8)
         ;;; (advice-remove 'write-region 'mlr--advice-9)
         (if (fboundp 'register-read-with-preview)
             ;; Emacs 24.4+.  Used by all register-reading cmds, but restrict highlighting
             ;;               to those affecting the region.
             (advice-remove 'register-read-with-preview 'mlr--advice-10)
           ;; Emacs 24.3 - no `register-read-with-preview'.
           (advice-remove 'copy-to-register 'mlr--advice-11)
           (advice-remove 'prepend-to-register 'mlr--advice-12)
           (advice-remove 'append-to-register 'mlr--advice-13))
         (advice-remove 'string-rectangle 'mlr--advice-14)
         (advice-remove 'string-insert-rectangle 'mlr--advice-15)
         (advice-remove 'rectangle-number-lines 'mlr--advice-16)
         ;; These change the mode-line style to rectangular.
         ;; (Don't show the barred face just because the rectangle is active.)
         (when (fboundp 'rectangle-mark-mode) ; Emacs 24.4+
           (remove-hook 'rectangle-mark-mode-hook  (lambda () (setq mlr-rect-p  rectangle-mark-mode))))
         (when (fboundp 'cua-rectangle-mark-mode) ; Emacs 24.4+, but see Emacs bug #18047
           (remove-hook 'cua-rectangle-mark-mode-hook
                        (lambda () (setq mlr-rect-p  cua-rectangle-mark-mode))))
         (advice-remove 'isearch-mode 'mlr--advice-17)
         (advice-remove 'isearch-query-replace 'mlr--advice-18)
         (advice-remove 'isearch-query-replace-regexp 'mlr--advice-19)
         (advice-remove 'shell-command-on-region 'mlr--advice-20))))

(defun turn-on-modeline-region-mode ()
  "Turn on `modeline-region-mode'."
  (modeline-region-mode))

;;;###autoload
(define-globalized-minor-mode global-modeline-region-mode modeline-region-mode
  turn-on-modeline-region-mode)
 

;;; ADVISED FUNCTIONS.
;;;
;;; `rectangle-mark-mode', `cua-rectangle-mark-mode', and some other functions
;;; are advised to set `mlr-rect-p' to non-nil, to turn on the mode-line info
;;; and highlighting for a rectangle.  Function `replace-dehighlight' is advised
;;; to reset `mlr-rect-p' to nil, because the replacement function acting on the
;;; rectangle is finished.
;;;
;;; Some functions are advised, to set `mlr-region-acting-on' to non-nil, to turn
;;; on the mode-line info and highlighting for the (non-rectangular) region.
;;; When this variable is non-nil, the face used for this in the mode-line is
;;; face `mlr-region-acting-on'. not `mlr-region'.  This is typically used to
;;; indicate that a command is acting on the region, not the whole buffer.


;;; Functions from `replace.el' ------------------------------------------------
;;; They are loaded by default.  (`replace.el' has no `provide'.)

(defun mlr--advice-1 ()
  "Turn off `mlr-region-acting-on' when replacement commands are finished.
There is no hook, so we advise `replace-dehighlight'."
  (setq mlr-region-acting-on  nil))

(defun mlr--advice-2 (orig-fun &rest args)
  "Turn on mode-line region highlighting for the act of (query-)replacing.
This applies to `query-replace', `query-replace-regexp',
`replace-string', `replace-regexp', and`isearch-query-replace'.  It is
done by advising `query-replace-read-to' and `perform-replace'."
  (let (;; (icicle-change-region-background-flag  nil)
        (mlr-region-acting-on  (or (use-region-p)
                                   (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))))
    (apply orig-fun args)))

(defun mlr--advice-3 (orig-fun &rest args)
  "Turn on mode-line region highlighting for the act of (query-)replacing.
This applies to `keep-lines', `flush-lines', `how-many', and
`query-replace-read-args' (thus also `query-replace',
`query-replace-regexp', `replace-string', and `replace-regexp').  It
is done by advising `query-replace-read-from' and
`keep-lines-read-args'."
  (let (;; (icicle-change-region-background-flag  nil)
        (mlr-region-acting-on  (use-region-p)))
    (apply orig-fun args)))

(defun mlr--advice-4 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `map-query-replace-regexp'."
  (interactive
   (let* (;; (icicle-change-region-background-flag  nil)
          (mlr-region-acting-on  (use-region-p))
          (from                  (read-regexp "Map query replace (regexp): " nil
                                              query-replace-from-history-variable))
          (to                    (read-from-minibuffer
                                  (format "Query replace %s with (space-separated strings): "
                                          (query-replace-descr from))
                                  nil nil nil query-replace-to-history-variable from t)))
     (list from
           to
           (and current-prefix-arg  (prefix-numeric-value current-prefix-arg))
           (and transient-mark-mode  mark-active  (region-beginning))
           (and transient-mark-mode  mark-active  (region-end)))))
  (apply orig-fun args))

(defun mlr--advice-21 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `occur'."
  (interactive
   (let (;; (icicle-change-region-background-flag  nil)
         (mlr-region-acting-on  (use-region-p)))
     (nconc (occur-read-primary-args)
            (and (use-region-p) (list (region-bounds))))))
  (declare-function region-bounds "simple.el" () 'FILEONLY)
  (apply orig-fun args))


;;; Commands from `simple.el' and `files.el' -----------------------------------
;;; They are loaded by default.  (`files.el' has no `provide'.)

;;; (defun mlr--advice-5 (orig-fun &rest args)
;;;   "Turn on mode-line region highlighting for `prepend-to-buffer'."
;;;   (interactive
;;;    (let (;; (icicle-change-region-background-flag  nil)
;;;          (mlr-region-acting-on  (use-region-p)))
;;;      (list (read-buffer "Prepend to buffer: " (other-buffer (current-buffer) t))
;;;            (region-beginning) (region-end))))
;;;   (apply orig-fun args))

;;; (defun mlr--advice-6 (orig-fun &rest args)
;;;   "Turn on mode-line region highlighting for `append-to-buffer'."
;;;   (interactive
;;;    (let (;; (icicle-change-region-background-flag  nil)
;;;          (mlr-region-acting-on  (use-region-p)))
;;;      (list (read-buffer "Append to buffer: " (other-buffer (current-buffer) t))
;;;            (region-beginning) (region-end))))
;;;   (apply orig-fun args))

;;; (defun mlr--advice-7 (orig-fun &rest args)
;;;   "Turn on mode-line region highlighting for `copy-to-buffer'."
;;;   (interactive
;;;    (let (;; (icicle-change-region-background-flag  nil)
;;;          (mlr-region-acting-on  (use-region-p)))
;;;      (list (read-buffer "Copy to buffer: " (other-buffer (current-buffer) t))
;;;            (region-beginning) (region-end))))
;;;   (apply orig-fun args))

;;; (defun mlr--advice-8 (orig-fun &rest args)
;;;   "Turn on mode-line region highlighting for `append-to-file'."
;;;   (interactive
;;;    (let (;; (icicle-change-region-background-flag  nil)
;;;          (mlr-region-acting-on  (use-region-p)))
;;;      (list (region-beginning)
;;;            (region-end)
;;;            (read-file-name "Append to file: "))))
;;;   (apply orig-fun args))


;; Built-in functions (from C code). -------------------------------------------

;;; (defun mlr--advice-9 (orig-fun &rest args)
;;;   "Turn on mode-line region highlighting for `write-region'."
;;;   (interactive
;;;    (let (;; (icicle-change-region-background-flag  nil)
;;;          (mlr-region-acting-on  (use-region-p))
;;;      (list (region-beginning)
;;;            (region-end)
;;;            (read-file-name "Write region to file: ")
;;;            nil
;;;            nil
;;;            nil
;;;            t)))
;;;   (apply orig-fun args))


;;; Functions from `register.el'. ----------------------------------------------

(if (fboundp 'register-read-with-preview)

    ;; Emacs 24.4+.  Used by all register-reading cmds, but restrict highlighting
    ;;               to those affecting region.
    (defun mlr--advice-10 (orig-fun &rest args)
      "Turn on mode-line region highlighting.
This applies to the register-reading commands that affect the region:
`copy-to-register', `append-to-register', `prepend-to-register',
`copy-rectangle-to-register'.

For `copy-rectangle-to-register', also set `mlr-rect-p' to non-nil."
      (let (;; (icicle-change-region-background-flag  nil)
            (mlr-region-acting-on  (and (use-region-p)
                                        (member this-command '(copy-to-register
                                                               append-to-register
                                                               prepend-to-register
                                                               copy-rectangle-to-register))))
            (mlr-rect-p            (member this-command '(copy-rectangle-to-register))))
        (apply orig-fun args)))

  ;; Emacs 24.3 - no `register-read-with-preview'.------------

  (defun mlr--advice-11 (orig-fun &rest args)
    "Turn on mode-line region highlighting for `copy-to-register'."
    (interactive
     (let (;; (icicle-change-region-background-flag  nil)
           (mlr-region-acting-on  (use-region-p)))
       (list (read-char "Copy to register: ")
             (region-beginning)
             (region-end)
             current-prefix-arg)))
    (apply orig-fun args))

  (defun mlr--advice-12 (orig-fun &rest args)
    "Turn on mode-line region highlighting for `prepend-to-register'."
    (interactive
     (let (;; (icicle-change-region-background-flag  nil)
           (mlr-region-acting-on  (use-region-p)))
       (list (read-char "Prepend to register: ")
             (region-beginning)
             (region-end)
             current-prefix-arg)))
    (apply orig-fun args))

  (defun mlr--advice-13 (orig-fun &rest args)
    "Turn on mode-line region highlighting for `append-to-register'."
    (interactive
     (let (;; (icicle-change-region-background-flag  nil)
           (mlr-region-acting-on  (use-region-p)))
       (list (read-char "Append to register: ")
             (region-beginning)
             (region-end)
             current-prefix-arg)))
    (apply orig-fun args))

  )


;;; Functions from `rect.el'. --------------------------------------------------

(defun mlr--advice-14 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `string-rectangle'."
  (interactive
   (let (;; (icicle-change-region-background-flag  nil)
         (mlr-region-acting-on  (use-region-p))
         (mlr-rect-p            t))
     (make-local-variable 'rectangle--string-preview-state)
     (make-local-variable 'rectangle--inhibit-region-highlight)
     (let* ((buf                                  (current-buffer))
            (win                                  (and (eq (window-buffer) buf)  (selected-window)))
            (start                                (region-beginning))
            (end                                  (region-end))
            (rectangle--string-preview-state      `(nil ,start ,end))
            ;; Rectangular-region highlighting doesn't work well in the presence of preview overlays.
            ;; We could try to make it work better, but it's easier to just disable it temporarily.
            (rectangle--inhibit-region-highlight  t))
       (barf-if-buffer-read-only)
       (list start
             end
             (if (fboundp 'rectangle--string-erase-preview) ; Emacs 25+
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (setq rectangle--string-preview-window  win)
                       (add-hook 'minibuffer-exit-hook #'rectangle--string-erase-preview nil t)
                       (add-hook 'post-command-hook #'rectangle--string-preview nil t))
                   (read-string (format "String rectangle (default %s): "
                                        (or (car string-rectangle-history)  ""))
                                nil 'string-rectangle-history (car string-rectangle-history)))
               (read-string (format "String rectangle (default %s): "
                                    (or (car string-rectangle-history)  ""))
                            nil 'string-rectangle-history (car string-rectangle-history)))))))
  (apply orig-fun args))

(defun mlr--advice-15 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `string-insert-rectangle'."
  (interactive
   (let (;; (icicle-change-region-background-flag  nil)
         (mlr-region-acting-on  (use-region-p))
         (mlr-rect-p            t))
     (barf-if-buffer-read-only)
     (list (region-beginning)
           (region-end)
           (read-string (format "String insert rectangle (default %s): "
                                (or (car string-rectangle-history)  ""))
                        nil 'string-rectangle-history (car string-rectangle-history)))))
  (apply orig-fun args))

(defun mlr--advice-16 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `rectangle-number-lines'."
  (interactive
   (let (;; (icicle-change-region-background-flag  nil)
         (mlr-region-acting-on  (use-region-p))
         (mlr-rect-p            t))
     (if current-prefix-arg
         (let* ((start     (region-beginning))
                (end       (region-end))
                (start-at  (read-number "Number to count from: " 1)))
           (list start end
                 start-at
                 (read-string "Format string: "
                              (rectangle--default-line-number-format start end start-at))))
       ;; (redisplay 'FORCE) (sit-for 0.7)
       (list (region-beginning) (region-end) 1 nil))))
  (apply orig-fun args))


;;; Functions from `isearch.el' ------------------------------------------------
;;; They are loaded by default.  (`isearch.el' has no `provide'.)

;; Library `isearch+.el' lets you restrict Isearch to the active region.

(defun mlr--advice-17 (&rest _args)
  "Turn on mode-line region highlighting for `isearch-mode'."
  (setq mlr-region-acting-on  (and (boundp 'isearchp-restrict-to-region-flag)
                                   isearchp-restrict-to-region-flag
                                   (boundp 'isearchp-reg-beg) ; Just to avoid byte-compile warning.
                                   isearchp-reg-beg))
  (add-hook 'isearch-mode-end-hook (lambda () (setq mlr-region-acting-on  nil))))

(defun mlr--advice-18 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `isearch-query-replace'.
This transfers the region restriction and its mode-line highlighting
from Isearch to query-replacing."
  (interactive
   (progn
     (setq mlr-region-acting-on  (and (boundp 'isearchp-restrict-to-region-flag)
                                      isearchp-restrict-to-region-flag
                                      (or (use-region-p)
                                          (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))))
     (list current-prefix-arg)))
  (when (and (boundp 'isearchp-reg-beg)
             isearchp-reg-beg
             (boundp 'isearchp-reg-end)
             isearchp-reg-end
             (boundp 'isearchp-restrict-to-region-flag)
             isearchp-restrict-to-region-flag) ; Activate region used for Isearch.
    (goto-char isearchp-reg-beg)
    (push-mark isearchp-reg-end t 'ACTIVATE))
  (apply orig-fun args))

(defun mlr--advice-19 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `isearch-query-replace-regexp'.
This transfers the region restriction and its mode-line highlighting
from Isearch to query-replacing."
  (interactive
   (progn
     (setq mlr-region-acting-on  (and (boundp 'isearchp-restrict-to-region-flag)
                                      isearchp-restrict-to-region-flag
                                      (or (use-region-p)
                                          (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))))
     (list current-prefix-arg)))
  (when (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg ; Activate region used for Isearch.
             (boundp 'isearchp-reg-end)  isearchp-reg-end
             (boundp 'isearchp-restrict-to-region-flag)  isearchp-restrict-to-region-flag)
    (goto-char isearchp-reg-beg)
    (push-mark isearchp-reg-end t 'ACTIVATE))
  (apply orig-fun args))

(defun mlr--advice-20 (orig-fun &rest args)
  "Mode-line region highlight for `shell-command-on-region' with prefix arg."
  (interactive
   (let (;; (icicle-change-region-background-flag  nil)
         (mlr-region-acting-on  current-prefix-arg)
         string)
     (unless (mark) (user-error "The mark is not set now, so there is no region"))
     (setq string  (read-shell-command "Shell command on region: "))
     (list (region-beginning) (region-end) string current-prefix-arg current-prefix-arg
	   shell-command-default-error-buffer t (region-noncontiguous-p))))
  (declare-function region-noncontiguous-p "simple.el" () 'FILEONLY)
  (apply orig-fun args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'modeline-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modeline-region.el ends here
