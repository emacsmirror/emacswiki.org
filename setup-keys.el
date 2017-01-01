;;; setup-keys.el --- Some key bindings.
;;
;; Filename: setup-keys.el
;; Description: Some key bindings.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1999-2017, Drew Adams, all rights reserved.
;; Created: Fri Apr  2 12:34:20 1999
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 11:33:08 2017 (-0800)
;;           By: dradams
;;     Update #: 1332
;; URL: http://www.emacswiki.org/setup-keys.el
;; Keywords: mouse, keyboard, menus, menu-bar
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `cl', `cus-theme', `doremi',
;;   `doremi-cmd', `doremi-frm', `easymenu', `eyedropper', `faces',
;;   `faces+', `fit-frame', `frame-cmds', `frame-fns', `help+20',
;;   `hexrgb', `highlight', `info', `info+20', `isearch+',
;;   `iso-transl', `menu-bar', `menu-bar+', `misc-cmds', `misc-fns',
;;   `mouse', `mouse+', `mwheel', `naked', `pp', `pp+', `replace+',
;;   `ring', `second-sel', `strings', `thingatpt', `thingatpt+',
;;   `unaccent', `w32browser-dlgopen', `wid-edit', `wid-edit+',
;;   `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Some key bindings.
;;
;;  Think of this library more as an extension to your init file
;;  (~/.emacs) than as a true library.  It makes changes to your Emacs
;;  key bindings.  If you want only some of the bindings that are
;;  defined here, then either modify this file for your own use or
;;  load it and then modify selected bindings afterward.
;;
;;  The user options defined here are not customizable using Customize
;;  (they are not defined using `defcustom').  They are used only
;;  once, when this file is loaded - it makes no sense to change their
;;  values after this file is loaded.  To change their behavior from
;;  the default, set them in your init file before loading this
;;  library.
;;
;;  For example, if you do not want to substitute command
;;  `kill-buffer-and-its-windows' for command `kill-buffer' in all
;;  interactive uses, then put this in your init file *before* loading
;;  library `setup-keys':
;;
;;  (setq sub-kill-buffer-and-its-windows nil) ; Keep `kill-buffer'.
;;
;;  If you also load library `menu-bar+', then load it *before*
;;  loading library `setup-keys'.
;;
;;  User options defined here:
;;
;;    `sub-*-of-line', `sub-delete-windows-for',
;;    `sub-kill-buffer-and-its-windows', `sub-pp-evals',
;;    `sub-query-replace-w-options', `sub-quit-window-delete',
;;    `sub-recenter-top-bottom', `sub-transpose-sexps'.
;;
;;  Other variables defined here:
;;
;;    `comparison-map', `doremi-map'.
;;
;;  Functions defined here:
;;
;;    `remap-command'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/11/02 dadams
;;     Added: sub-transpose-sexps.
;;     Remap transpose-sexps to reversible-transpose-sexps, if sub-transpose-sexps.
;; 2016/09/18 dadams
;;     Applied renaming of secondary-dwim to secondary-yank|select|move|swap.
;; 2016/07/19 dadams
;;     Bound M-m to to-indentation-repeat-backward and M-n to to-indentation-repeat-forward.
;; 2016/01/24 dadams
;;     Bound C-x 5 1 to tear-off-window.
;; 2015/06/30 dadams
;;     Changed highlight-symbol-* bindings to f9 from f11.
;;     Replaced f12 by f8.
;; 2015/04/02 dadams
;;     Corrected command names for ni-narrow-to-*.
;; 2015/03/15 dadams
;;     Added: remap-command, sub-quit-window-delete.
;;     Remap quit-window to quit-window-delete, if sub-quit-window-delete.
;; 2014/11/28 dadams
;;     Bind compare-windows-repeat instead of compare-windows, if available.
;; 2014/10/29 dadams
;;     Bind (next|previous)-buffer-repeat.
;; 2014/05/23 dadams
;;     Bind narrow-indirect.el commands.
;; 2014/05/19 dadams
;;     If use mouse+.el then get rid of Emacs 24+ minibuffer.el's mouse-1 in echo area.
;;     Consolidate two eval-after-load's for mouse+.
;; 2014/03/12 dadams
;;     Bind C-M-^ to up-list, i.e., forward direction.
;; 2013/11/18 dadams
;;     Bind C-x C-; to comment-region-lines instead of comment-region.
;;     Do not require simple+.el.
;; 2013/11/07 dadams
;;     Bind hlt-highlight-enclosing-list to C-M-S.
;; 2013/10/23 dadams
;;     Bind C-x t s to either doremi-custom-themes+ or doremi-color-themes+.
;; 2013/09/15 dadams
;;     Do not bind help-on-click/key here.  Do not require help+(20).el for that.
;; 2013/09/01 dadams
;;     Added remapping of undo to undo-repeat.
;; 2013/08/23 dadams
;;     Soft-require highlight-symbol.el (Emacs 22+).  Bind its commands to f11 (+ modifiers).
;; 2013/07/25 dadams
;;     Invoke find-function-setup-keys.
;; 2013/07/05 dadams
;;     Bind move-frame-to-screen-top-left to C-S-home.
;; 2013/04/21 dadams
;;     Bind zoom-in/out to C-x +, C-x -, C-x =, C-x 0.
;; 2013/03/06 dadams
;;     Bind C-x C-M-SPC to set-secondary-start, C-x C-M-RET to secondary-save-then-kill.
;; 2013/01/17 dadams
;;     Added bindings for move-frame-to-screen-(top|bottom|left|right).
;; 2013/01/02 dadams
;;     Bound C-o also in minibuffer-(inactive-mode|local-(isearch|shell-command))-map.
;; 2012/12/24 dadams
;;     Added bindings for visual-line-mode line movements.
;; 2012/08/27 dadams
;;     Treat Emacs 24+ insert-char the same as ucs-insert (old name).
;; 2012/07/08 dadams
;;     Bind C-mouse-1 to ignore, so don't see error msg on up event.
;; 2012/07/02 dadams
;;     Bind find-library-other-window to C-x 4 l.
;; 2012/06/02 dadams
;;     If ucs-cmds.el is loaded, bind C-x 8 RET to ucsc-insert, if Emacs 23+.
;; 2011/11/12 dadams
;;     Vars sub-*: Removed (when (fboundp '*)...) wrapper - define always.  But mention in doc
;;       string that has no effect unless library loaded.
;; 2011/07/25 dadams
;;     Use eval-after-load where appropriate (e.g. instead of featurep/fboundp/boundp).
;; 2010/04/22 dadams
;;     Bound C-M-y to isearch-yank-secondary in isearch-mode-map.
;; 2010/02/24 dadams
;;     Bound C-; to iedit-mode, globally and in isearch-mode-map.
;; 2010/02/20 dadams
;;     Bound framemove keys: M-S-(up|down|left|right).
;; 2009/11/07 dadams
;;     Bound doremi-face-bg+ to k.  Applied doremi cmd renamings (added +).
;; 2009/08/26 dadams
;;     Changed binding of region-to-file from `C-x a' to C-x M-f, due to abbrev keys conflict.
;; 2009/07/26 dadams
;;     Fixed typo: prev-buffer -> previous-buffer.
;; 2009/06/25 dadams
;;     Use renaming: yank-secondary-or-swap-w-region to secondary-dwim.
;; 2009/06/11 dadams
;;     Bind zoom-(in|out), not zoom-frm-(in|out).
;;     Don't bind M-s if Emacs 23+.
;; 2009/05/17 dadams
;;     Updated to reflect thumb-frm.el name changes.
;; 2009/04/08 dadams
;;     Use revert-buffer-no-confirm, if defined.
;; 2009/04/06 dadams
;;     Changed binding of revert-buffer from S-f1 to f5 (a la MS Windows).
;; 2009/01/06 dadams
;;     Move delete-window from C-x C-a to C-x C-z, to avoid conflict with gud.
;;      Replaces std binding for iconify-or-deiconify-frame.
;; 2008/11/08 dadams
;;     Bind swiss-move-line-up/down to S-prior/S-next.
;; 2008/10/19 dadams
;;     Bind mouse-2 in ctl-x-map to ignore, so hlt-highlighter works on down-mouse-2.
;; 2008/08/17 dadams
;;     Made zoom-frm-(in|out) bindings portable, and make C- bindings work for Emacs 20, 21.
;; 2008/08/14 dadams
;;     Bound C-x C-a to delete-window, so you can do it with one hand.
;; 2008/08/07 dadams
;;     Bound zoom-frm-(in|out) to C-wheel-(down|up).
;; 2008/08/06 dadams
;;     Bind bm-toggle, bm-next, bm-previous to S-f3, C-f3, M-f3, not C-f3, f3, S-f3.
;; 2008/05/23 dadams
;;     Bound yank-pop-commands to M-y.  Soft-require second-sel.el.
;;     Soft-require second-sel.el.
;; 2008/05/06 dadams
;;     Renamed yank-secondary-or-convert-primary to yank-secondary-or-swap-w-region.
;; 2008/05/03 dadams
;;     Bind yank-secondary-or-convert-primary, not yank-secondary, to C-M-y.
;; 2008/03/19 dadams
;;     Don't bind C-o in completion maps if they inherit from minibuffer-local-map.
;; 2008/03/06 dadams
;;     Removed binding for iconify-everything - too easy to hit by mistake.
;; 2008/03/02 dadams
;;     Removed describe-file binding (done now in help-fns+.el and help+20.el).
;; 2007/12/14 dadams
;;     Require help+20.el for Emacs 20.  Require (new) help+.el for Emacss 22.
;; 2007/12/02 dadams
;;     Bound describe-face to C-h C-M-f.
;; 2007/11/21 dadams
;;     Bound C-o in minibuffer maps to 1on1-fit-minibuffer-frame.
;; 2007/11/06 dadams
;;     Added: sub-recenter-top-bottom.  Substituted recenter-top-bottom for recenter binding.
;; 2007/11/01 dadams
;;     Changed C-x t w from doremi-frame-width to doremi-window-height.
;; 2007/10/13 dadams
;;     Bound bm.el keys to [f3] with modifiers.
;; 2007/09/28 dadams
;;     Bound lisp-spell-symbol to M-#.
;; 2007/09/24 dadams
;;     Bound mark-buffer-(before|after)-point.
;; 2007/09/19 dadams
;;     Removed bindings for goto-previous(-global)-mark.
;; 2007/07/15 dadams
;;     Changed binding of cycle-thing-region to M-@.
;; 2007/06/04 dadams
;;     Removed sub-customize-other window (RMS's bug fix was implemented).
;; 2007/06/02 dadams
;;     Renamed: highlight to hlt-highlight,
;;              highlight-(highlighter|eraser) to hlt-(highlighter|eraser),
;;              highlight-(next|previous)-* to hlt-(next|previous)-*.
;; 2007/04/06 dadams
;;     Changed bindings of fisheye-(next|previous)-frame.
;; 2007/04/02 dadams
;;     Bound goto-longest-line to C-x L.
;; 2007/03/17 dadams
;;     Bound highlight-(next|previous)-highlight, highlight-eraser.
;; 2007/03/16 dadams
;;     Bound highlight-with-marker.  Protect with fboundp.
;; 2007/02/03 dadams
;;     Fixed mark-ring binding.  Thx to Fidel Salas.
;; 2007/01/27 dadams
;;     Bound S-down-mouse-2 to mouse-scan-lines-or-M-:, instead of mouse-M-:.
;; 2007/01/13 dadams
;;     Bound C-h M-f to describe-file.
;; 2006/11/04 dadams
;;     Bound S-down-mouse-2 to mouse-M-:.
;; 2006/11/03 dadams
;;     Bound down-mouse-2 to mouse-flash-position-or-M-x, not mouse-flash-position.
;; 2006/09/12 dadams
;;     Replaced [pause] by [f12] for C-x 8 synonym.  ([pause] is used by Icicles.)
;; 2006/09/08 dadams
;;     Bound crosshairs-mode to C-+.
;; 2006/08/22 dadams
;;     Replaced sub-remove-window by sub-delete-windows-for.
;; 2006/08/15 dadams
;;     Removed C-f1 binding of kill-buffer.
;; 2006/08/11 dadams
;;     Bound mouse-flash-position to down-mouse-2.
;; 2006/07/30 dadams
;;     Added bindings for mark-thing and cycle-thing-region.
;; 2006/05/26 dadams
;;     Clarified commentary.
;;     foldout-mouse-modifiers: Use setq, not defvar.
;; 2006/05/16 dadams
;;     Removed Icicles bindings - use new library icicles-keys.el instead.
;; 2006/03/01 dadams
;;     Bound icicle-complete-thesaurus-entry to C-c /.
;; 2006/01/24 dadams
;;     Bound icicle-execute-extended-command.
;; 2006/01/19 dadams
;;     Added sub-*-of-line.  Use it with move-*-of-line, if Emacs 22.
;; 2006/01/04 dadams
;;     Bound other-window-or-frame to C-x o.
;; 2005/12/13 dadams
;;     Bound delete-other-frames to C-x 4 1.
;; 2005/12/03 dadams
;;     Changed bindings of thumb-frm.el commands and show-hide.
;; 2005/11/18 dadams
;;     Bound mouse-4 to help-go-back in help-mode-map, and M-` to icicle-execute-menu-command.
;; 2005/10/27 dadams
;;     Renamed sub-icicle-buffer to sub-icicle-commands.  Bound icicle-find-file*.
;; 2005/10/16 dadams
;;     Bound icicle-compilation-search in compilation-minor-mode-map.  Bound grep in grep.
;; 2005/09/02 dadams
;;     Added sub-icicle-buffer, and substituted for switch-to-buffer*.
;;     Protected sub-* by fboundp.
;; 2005/08/13 dadams
;;     Added binding for icicle-execute-menu-command.
;; 2005/08/02 dadams
;;     Added binding for doremi-all-faces-fg.
;; 2005/05/29 dadams
;;     Added move-frame-* bindings.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2005/05/06 dadams
;;     Added commented-out binding for thumbify-frame-upon-event, as a model.
;; 2005/01/26 dadams
;;     Commented out (w32-register-hot-key [A-tab]).
;;     Removed ###autoload for defvars.
;; 2005/01/20 dadams
;;     Removed sub-exit-with-confirmation.
;; 2005/01/09 dadams
;;     Renamed: doremi-bg-rgb to doremi-bg, doremi-face-fg-rgb to doremi-face-fg.
;;     Changed some doremi bindings.
;; 2005/01/02 dadams
;;     Changed binding of region-to-file because `C-x w' conflicted with hi-lock.
;;     Added bindings for doremi-mark and doremi-global-mark; changed for doremi-bookmarks.
;; 2004/12/28 dadams
;;     Added doremi-face-fg-rgb binding.  Changed binding: doremi-font.
;; 2004/12/26 dadams
;;     Added thumb-frm.el bindings.
;; 2004/11/24 dadams
;;     (w32-register-hot-key [A-tab])
;; 2004/11/21 dadams
;;     Added [C-pause], [M-pause] bindings for prev-buffer, next-buffer.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/11/16 dadams
;;     Added substitute-key-definition '*-of-line '*-of-line+.
;; 2004/10/13 dadams
;;     These were added for Emacs 21 also: replace+.el, buff-menu+.el.
;; 2004/10/02 dadams
;;     Per request by RMS:
;;     Renamed grow-frame-height and grow-frame-width to enlarge-frame
;;     and enlarge-frame-horizontally, respectively.
;;     Added shrink-frame and shrink-frame-horizontally.
;; 2004/09/21 dadams
;;     Don't require stuff that's not yet ready or inappropriate for Emacs 21.
;; 2004/09/11 dadams
;;     Bound vertical-line mouse commands.
;;     Added bindings for commands in doremi-frm.el.
;; 2004/08/26 dadams
;;     Changed C-M-up/down/left/right to enlarge-frame* and shrink-frame*.
;; 2004/04/28 dadams
;;     Added C-M-home and C-M-end bindings for beginning- and end-of-defun.
;; 1999/09/03 dadams
;;     Added sub-pp-evals.  Use it to replace eval-* with pp-eval-*.
;; 1999/09/02 dadams
;;     1. Added binding for show-hide.
;;     2. Added vars: sub-exit-with-confirmation,
;;        sub-kill-buffer-and-its-windows, sub-pp-evals,
;;        sub-query-replace-w-options, sub-remove-window.  Use to
;;        protect substitute-key-definition's.
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

(require 'frame-cmds nil t) ;; (no error if not found): delete-other-frames,
                            ;; delete-windows-for, enlarge-frame*,
                            ;; iconify-everything, iconify/map-frame, move-frame-*,
                            ;; mouse-iconify/map-frame, mouse-remove-window,
                            ;; mouse-show-hide-mark-unmark, other-window-or-frame,
                            ;; show-*Help*-buffer, show-hide, shrink-frame*,
                            ;; tear-off-window
(require 'mouse+ nil t) ;; (no error if not found): mouse-tear-off-window, mouse-flash-position
(require 'highlight nil t) ;; (no error if not found): hlt-highlight, hlt-highlighter,
                           ;; hlt-eraser, hlt-(next|previous)-highlight
(when (fboundp 'define-minor-mode) ;; (no error if not found): *-at-point,
  (require 'highlight-symbol nil t)) ;; *-(next|prev), *-query-replace

(require 'misc-cmds nil t) ;; (no error if not found): beginning-of-line+,
                           ;; end-of-line+, goto-longest-line, kill-buffer-and-its-windows,
                           ;; mark-buffer-after-point, mark-buffer-before-point,
                           ;; recenter-top-bottom, region-to-buffer, region-to-file,
                           ;; to-indentation-repeat-backward, to-indentation-repeat-forward,
                           ;; undo-repeat
(require 'second-sel nil t) ;; (no error if not found): secondary-yank|select|move|swap,
                            ;; isearch-yank-secondary, yank-pop-commands,
                            ;; isearch-yank-secondary, set-secondary-start,
                            ;; secondary-save-then-kill
(require 'pp+ nil t) ;; (no error if not found): pp-eval-expression
(require 'fit-frame nil t) ;; (no error if not found):
                           ;; fit-frame, fit-frame-or-mouse-drag-vertical-line
(require 'doremi-frm nil t) ;; (no error if not found): doremi-bg+, doremi-face-fg+,
                            ;; doremi-font+, doremi-frame-font-size+, doremi-frame-configs+,
                            ;; doremi-frame-height+, doremi-frame-horizontally+,
                            ;; doremi-frame-vertically+
(require 'doremi-cmd nil t) ;; (no error if not found): doremi-buffers+, doremi-bookmarks+,
                            ;; doremi-color-themes+, doremi-custom-themes+

(when (< emacs-major-version 21)
  ;; (require 'help+20 nil t) ;; (no error if not found): help-on-click/key
  (require 'unaccent nil t)) ;; (no error if not found): unaccent-region, unaccent-word
;;; (when (> emacs-major-version 21)
;;;   (require 'help+ nil t)) ;; (no error if not found): help-on-click/key
(require 'replace+ nil t)   ;; (no error if not found): query-replace-w-options

;; Quiet the byte compiler.
(defvar grep-mode-map)                  ; Defined in `grep.el'.
(defvar mouse-wheel-down-event)         ; Defined in `mwheel.el'.
(defvar mouse-wheel-up-event)           ; Defined in `mwheel.el'.

;;;-----------------------------

(defun remap-command (old new map &optional oldmap)
  "Bind command NEW in MAP to all keys currently bound to OLD.
If command remapping is available, use that.  Otherwise, bind NEW to
whatever OLD is bound to in MAP, or in OLDMAP, if provided."
  (if (fboundp 'command-remapping)
      (define-key map (vector 'remap old) new) ; Ignore OLDMAP for Emacs 22.
    (substitute-key-definition old new map oldmap)))

(when (boundp 'help-mode-map) (define-key help-mode-map [mouse-4] 'help-go-back))
(when (boundp 'grep-mode-map) (define-key grep-mode-map "g" 'grep)) ; Emacs 22

(autoload 'compare-windows "compare-w"
  "Compare text in current window with text in next window." t)
(autoload 'ediff-buffers "ediff"
  "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B." t)
(autoload 'ediff-files "ediff"
  "Run Ediff on a pair of files, FILE-A and FILE-B." t)
(autoload 'kill-rectangle "rect"
  "Delete rectangle with corners at point & mark; save as last killed." t)
(autoload 'fill-individual-paragraphs "fill"
  "Fill paragraphs of uniform indentation within the region." t)
(autoload 'ispell-complete-word "ispell"
  "Complete word using letters at point to word beginning using `look'." t)
(autoload 'forward-whitespace "thingatpt" nil t)
(autoload 'forward-symbol "thingatpt" nil t)
(autoload 'dired-jump-other-window "dired-x"
  "Jump to dired buffer corresponding to current buffer, in new window." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't let Windows grap ALT-TAB:
;; (when (eq system-type 'windows-nt) (w32-register-hot-key [A-tab]))

;; Additional definitions for some standard mouse commands:
;; SGI does not pass all ALT-mouse stuff thru to Emacs, so use C-M-mouse also:
(global-set-key [C-M-mouse-1] 'mouse-start-secondary) ; In `mouse.el'.            `C-M-mouse-1'
(global-set-key [C-M-drag-mouse-1] 'mouse-set-secondary) ; In `mouse.el'.
(global-set-key [C-M-down-mouse-1] 'mouse-drag-secondary) ; In `mouse.el'.
(global-set-key [C-M-mouse-3] 'mouse-secondary-save-then-kill) ; `second-sel.el'. `C-M-mouse-3'
(global-set-key [C-M-mouse-2] 'mouse-yank-secondary) ; `mouse+.el' or `mouse.el'  `C-M-mouse-2'

(global-set-key "\C-\M-^" 'up-list)                                              ; `C-M-^'

(eval-after-load "mouse+"
  '(progn                               ; Highlight yank position or call `M-x' in echo area.
    (global-set-key [down-mouse-2]   'mouse-flash-position-or-M-x)               ; `mouse-2'
    ;; Highlight line or `M-:'.
    (global-set-key [S-down-mouse-2] 'mouse-scan-lines-or-M-:)                   ; `S-mouse-2'
    (global-set-key [mode-line C-mouse-1] 'mouse-tear-off-window)
    (when (> emacs-major-version 23)
      (define-key minibuffer-inactive-mode-map [down-mouse-1] nil)               ; `mouse-1'
      (define-key minibuffer-inactive-mode-map [mouse-1] nil))))                 ; in echo area

(eval-after-load "second-sel"
  '(progn
    (global-set-key (kbd "C-M-y")  (if (fboundp 'secondary-yank|select|move|swap)
                                       'secondary-yank|select|move|swap
                                     'secondary-dwim))                           ; `C-M-y'
    (define-key esc-map "y"                     'yank-pop-commands)              ; `M-y'
    (define-key isearch-mode-map (kbd "C-M-y")  'isearch-yank-secondary)         ; `C-M-y'
    (global-set-key (kbd "C-x C-M-SPC")         'set-secondary-start)            ;`C-x C-M-SPC'
    (global-set-key (kbd "C-x C-M-<return>")    'secondary-save-then-kill)))     ;`C-x C-M-RET'

(eval-after-load "narrow-indirect"
  '(progn
    (define-key ctl-x-4-map "nd" 'ni-narrow-to-defun-indirect-other-window)      ; `C-x 4 n d'
    (define-key ctl-x-4-map "nn" 'ni-narrow-to-region-indirect-other-window)     ; `C-x 4 n n'
    (define-key ctl-x-4-map "np" 'ni-narrow-to-page-indirect-other-window)))     ; `C-x 4 n p'

;; Because C-M- is being used for secondary.
(eval-after-load "foldout" '(setq foldout-mouse-modifiers '(meta shift)))

(eval-after-load "oneonone"                                                      ; `C-o'
  '(when (framep 1on1-minibuffer-frame) ; Standalone minibuffer frame.
    (define-key minibuffer-local-map "\C-o" '1on1-fit-minibuffer-frame)
    (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
      (define-key minibuffer-local-must-match-map "\C-o" '1on1-fit-minibuffer-frame)
      (define-key minibuffer-local-completion-map "\C-o" '1on1-fit-minibuffer-frame))
    (when (boundp 'minibuffer-local-filename-completion-map)
      (define-key minibuffer-local-filename-completion-map "\C-o"
        '1on1-fit-minibuffer-frame))
    (when (boundp 'minibuffer-local-must-match-filename-map) ; Emacs 22
      (define-key minibuffer-local-must-match-filename-map "\C-o"
        '1on1-fit-minibuffer-frame))
    (when (boundp 'minibuffer-local-filename-must-match-map) ; Emacs 23+
      (define-key minibuffer-local-filename-must-match-map "\C-o"
        '1on1-fit-minibuffer-frame))
    (when (boundp 'minibuffer-local-isearch-map)
      (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-isearch-map))
        (define-key minibuffer-local-isearch-map "\C-o" '1on1-fit-minibuffer-frame)))
    (when (boundp 'minibuffer-local-shell-command-map)
      (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-shell-command-map))
        (define-key minibuffer-local-shell-command-map "\C-o" '1on1-fit-minibuffer-frame)))
    (when (boundp 'minibuffer-inactive-mode-map)
      (define-key minibuffer-inactive-mode-map "\C-o" '1on1-fit-minibuffer-frame))))

(eval-after-load "frame-cmds"
  '(progn
    (global-set-key [(meta up)]             'move-frame-up)                      ; `M-up'
    (global-set-key [(meta down)]           'move-frame-down)                    ; `M-down'
    (global-set-key [(meta left)]           'move-frame-left)                    ; `M-left'
    (global-set-key [(meta right)]          'move-frame-right)                   ; `M-right'
    (global-set-key [(meta shift ?v)]       'move-frame-to-screen-top)           ; `M-S-v'
    (global-set-key [(control shift ?v)]    'move-frame-to-screen-bottom)        ; `C-S-v'
    (global-set-key [(control shift prior)] 'move-frame-to-screen-left)          ; `C-S-prior'
    (global-set-key [(control shift next)]  'move-frame-to-screen-right)         ; `C-S-next'
    (global-set-key [(control shift home)]  'move-frame-to-screen-top-left)      ; `C-S-home'
    (global-set-key [(control meta up)]     'shrink-frame)                       ; `C-M-up'
    (global-set-key [(control meta down)]   'enlarge-frame)                      ; `C-M-down'
    (global-set-key [(control meta left)]   'shrink-frame-horizontally)          ; `C-M-left'
    (global-set-key [(control meta right)]  'enlarge-frame-horizontally)         ; `C-M-right'
    ;; Replaces`iconify-or-deiconify-frame'.
    (global-set-key [(control ?z)] 'iconify/map-frame)                           ; `C-z'
    ;; $$$$ (global-set-key [(control ?x) (control ?z)] 'iconify-everything)
    (global-set-key [(shift control meta ?z)] 'show-hide)                        ; `C-M-S-z'
    (global-set-key [C-down-mouse-1]        'mouse-show-hide-mark-unmark)        ; `C-mouse-1'
    (global-set-key [C-mouse-1]            'ignore)
    (global-set-key [S-down-mouse-1]       nil) ; Get rid of `mouse-set-font'.   ; `S-mouse-1'
    ;;(global-set-key [vertical-line mouse-1] 'ignore)
    (global-set-key [vertical-line C-down-mouse-1] 'show-hide)
    ;;(global-set-key [vertical-line C-mouse-1] 'ignore)
    (global-set-key [vertical-line S-down-mouse-1] 'iconify-everything)
    ;;(global-set-key [vertical-line S-mouse-1] 'ignore)
    ;; [mode-line mouse-3] as deletion (Emacs std) is too hazardous.  Iconify instead.
    (global-set-key [mode-line mouse-3]     'mouse-iconify/map-frame)
    (global-set-key [mode-line C-mouse-3]   'mouse-remove-window)
    (define-key ctl-x-5-map "1"             'tear-off-window)))

(eval-after-load "framemove"
  '(progn
    (global-set-key [(shift meta up)]    'fm-up-frame)                           ; `M-S-up'
    (global-set-key [(shift meta down)]  'fm-down-frame)                         ; `M-S-down'
    (global-set-key [(shift meta left)]  'fm-left-frame)                         ; `M-S-left'
    (global-set-key [(shift meta right)] 'fm-right-frame)))                      ; `M-S-right'

(eval-after-load "zoom-frm"             ; `zoom-frm.el' requires `frame-cmds.el'.
  '(progn
    (global-set-key [S-mouse-1] 'zoom-in)                                     ; `S-mouse-1'
    (global-set-key [C-S-mouse-1] 'zoom-out)                                  ; `C-S-mouse-1'
    (global-set-key (if (boundp 'mouse-wheel-down-event)                      ; `C-mouse-wheel'
                        (vector (list 'control mouse-wheel-down-event))
                      [C-mouse-wheel])  ; Emacs 20, 21
     'zoom-in)
    (when (boundp 'mouse-wheel-up-event)
      (global-set-key (vector (list 'control mouse-wheel-up-event)) 'zoom-out))

    (when (fboundp 'text-scale-adjust)  ; Emacs 23+
      (define-key ctl-x-map [(control ?+)] 'zoom-in/out)                         ; `C-x +'
      (define-key ctl-x-map [(control ?-)] 'zoom-in/out)                         ; `C-x -'
      (define-key ctl-x-map [(control ?=)] 'zoom-in/out)                         ; `C-x ='
      (define-key ctl-x-map [(control ?0)] 'zoom-in/out))))                      ; `C-x 0'

;;;   ;; These [nil] bindings are no doubt a HACK, based on an undocumented handy "feature".
;;;   ;; (This works in Emacs 19.34.6, but it doesn't work in Emacs 20.6.)
;;;   ;; In Windows, at least, such a key sequence [nil...] occurs if you click in the
;;;   ;; lower right corner, between the scroll bar and the mode-line.
;;;   (global-set-key [nil down-mouse-1] 'fit-frame)
;;;   (global-set-key [nil mouse-1] 'ignore)
;;;   (global-set-key [nil C-down-mouse-1] 'show-hide)
;;;   (global-set-key [nil C-mouse-1] 'ignore)
;;;   (global-set-key [nil S-down-mouse-1] 'iconify-everything)
;;;   (global-set-key [nil S-mouse-1] 'ignore)

;; These are defined in `fit-frame.el'.
(eval-after-load "fit-frame"
  '(progn
    (global-set-key [(control ?x) (control ?_)] 'fit-frame)                      ; `C-x C-_'
    (global-set-key [vertical-line down-mouse-1] 'fit-frame-or-mouse-drag-vertical-line)))

(eval-after-load "iedit"
  '(progn
    (define-key global-map       (kbd "C-;") 'iedit-mode)                        ; `C-;'
    (define-key isearch-mode-map (kbd "C-;") 'iedit-mode)))

;;; Put *Help* buffer in `help-minor-mode'.
;;(save-excursion (set-buffer (get-buffer-create "*Help*")) (help-minor-mode 1))

;;;; Help mouse-menu.
;;(when (fboundp 'help-mouse-menu)        ; In `help-minor.el'.
;;  (unless (lookup-key (current-global-map) [C-M-S-down-mouse-1])
;;    (global-set-key [C-M-S-down-mouse-1] 'help-mouse-menu)
;;    (global-set-key [C-M-S-mouse-1] 'ignore))
;;  (unless (lookup-key (current-global-map) [C-M-S-down-mouse-2])
;;    (global-set-key [C-M-S-down-mouse-2] 'help-mouse-menu)
;;    (global-set-key [C-M-S-mouse-2] 'ignore))
;;  (unless (lookup-key (current-global-map) [C-M-S-down-mouse-3])
;;    (global-set-key [C-M-S-down-mouse-3] 'help-mouse-menu)
;;    (global-set-key [C-M-S-mouse-3] 'ignore)))

;; Comparisons: windows, buffers, files.
(global-set-key [(control meta ?=)] (if (fboundp 'compare-windows-repeat)
                                        'compare-windows-repeat ; In `misc-cmds.el'.
                                      'compare-windows)) ; In `compare-w.el'.
(defvar comparison-map (lookup-key global-map [?\C-=])
  "Prefix keymap for comparison commands.")
(unless (keymapp comparison-map)
  (setq comparison-map (make-sparse-keymap))
  (global-set-key [(control ?=)] comparison-map)
  (define-key comparison-map "b" 'ediff-buffers) ; In `ediff.el'.                ; `C-= b'
  (define-key comparison-map "e" 'ediff-files) ; In `ediff.el'.                  ; `C-= e'
  (define-key comparison-map "f" 'ediff-files) ; In `ediff.el'.                  ; `C-= f'
  (define-key comparison-map "d" 'diff) ; In `diff+.el'.                         ; `C-= d'
  (define-key comparison-map "w" (if (fboundp 'compare-windows-repeat)           ; `C-= w'
                                     'compare-windows-repeat ; In `misc-cmds.el'.
                                   'compare-windows))) ; In `compare-w.el'.

;; Completions (non-minibuffer).
;(global-set-key "\M-\r" 'complete)      ; Defined in `completion.el':
;(global-set-key [?\C-\r] 'complete)
;(define-key function-key-map [C-return] [?\C-\r])

(eval-after-load "fill"
  '(define-key text-mode-map [(meta ?j)] 'fill-individual-paragraphs))           ; `M-j'

(eval-after-load "ispell"
  '(global-set-key [(meta ?$)] 'ispell-complete-word))                           ; `M-$'

(eval-after-load "thingatpt"
  '(progn
    (global-set-key [(meta ?_)] 'forward-whitespace)                             ; `M-_'
    ;; Emacs 23 co-opts `M-s-' as a prefix key.
    (unless (lookup-key global-map [(meta ?s)])                                  ; `M-s-'
      (global-set-key [(meta ?s)] 'forward-symbol)))) ; Defined in `thingatpt.el'

;; These replace the bindings for `mark-sexp' and `mark-word'.  Defined in `thing-cmds.el'.
(eval-after-load "thing-cmds"
  '(progn
    (global-set-key [(control meta ? )] 'mark-thing)                             ; `C-M-SPC'
    (global-set-key [(meta ?@)] 'cycle-thing-region)))                           ; `M-@'

(eval-after-load "crosshairs"
  '(global-set-key [(control ?+)] 'crosshairs-mode))                             ; `C-+'

(eval-after-load "unaccent"
  '(progn
    (global-set-key [(meta ?\")] 'unaccent-word)                                 ; `M-"'
    (define-key ctl-x-map [\"] 'unaccent-region)))                               ; `C-x "'

;;; Do Re Mi commands
(eval-after-load "doremi-frm"
  '(progn
    (unless (fboundp 'doremi-prefix)
      (defalias 'doremi-prefix (make-sparse-keymap))
      (defvar doremi-map (symbol-function 'doremi-prefix)
        "Keymap for Do Re Mi commands."))
    (define-key global-map "\C-xt" 'doremi-prefix)
    (define-key doremi-map "a" 'doremi-all-faces-fg+) ; "All"                    `C-x t a'
    (define-key doremi-map "c" 'doremi-bg+) ; "Color"                            `C-x t c'
    (define-key doremi-map "f" 'doremi-face-fg+) ; Face"                         `C-x t f'
    (define-key doremi-map "h" 'doremi-frame-height+) ; Height                   `C-x t h'
    (define-key doremi-map "k" 'doremi-face-bg+) ; bacKground"                   `C-x t k'
    (define-key doremi-map "t" 'doremi-font+) ; "Typeface"                       `C-x t t'
    (define-key doremi-map "u" 'doremi-frame-configs+) ; "Undo"                  `C-x t u'
    (define-key doremi-map "x" 'doremi-frame-horizontally+) ; X (abscissa)       `C-x t x'
    (define-key doremi-map "y" 'doremi-frame-vertically+)   ; Y (ordinate)       `C-x t y'
    (define-key doremi-map "z" 'doremi-frame-font-size+))) ; "Zoom"              `C-x t z'

(eval-after-load "doremi-cmd"
  '(progn
    (unless (fboundp 'doremi-prefix)
      (defalias 'doremi-prefix (make-sparse-keymap))
      (defvar doremi-map (symbol-function 'doremi-prefix)
        "Keymap for Do Re Mi commands."))
    (define-key global-map "\C-xt"  'doremi-prefix)
    (define-key doremi-map "b" 'doremi-buffers+) ; Buffer                        `C-x t b'
    (define-key doremi-map "g" 'doremi-global-marks+) ; Global mark              `C-x t g'
    (define-key doremi-map "m" 'doremi-marks+) ; Mark                            `C-x t m'
    (define-key doremi-map "r" 'doremi-bookmarks+) ; `r' for Reading books       `C-x t r'
    (define-key doremi-map "s" (if (fboundp 'doremi-custom-themes+)
                                   'doremi-custom-themes+
                                 'doremi-color-themes+)) ; `s' for color Schemes `C-x t s'
    (define-key doremi-map "w" 'doremi-window-height+))) ; Window                `C-x t w'

(eval-after-load "frame-cmds"
  '(progn
    (unless (fboundp 'doremi-prefix)
      (defalias 'doremi-prefix (make-sparse-keymap))
      (defvar doremi-map (symbol-function 'doremi-prefix)
        "Keymap for Do Re Mi commands."))
    (define-key global-map "\C-xt"  'doremi-prefix)
    (define-key doremi-map "." 'save-frame-config)))                           ; `C-x t .'

(eval-after-load "thumb-frm"
  '(progn
    (global-set-key [(shift mouse-3)]         'thumfr-toggle-thumbnail-frame)  ; `S-mouse-3'
    (global-set-key [(shift control mouse-3)] 'thumfr-thumbify-other-frames)   ; `C-S-mouse-3'
    (global-set-key [(shift control ?z)]      'thumfr-thumbify-other-frames)   ; `C-S-z'
    (global-set-key [(shift control ?n)]      'thumfr-fisheye-next-frame)      ; `C-S-n'
    (global-set-key [(shift control ?p)]      'thumfr-fisheye-previous-frame)  ; `C-S-p'
    (global-set-key [(control meta ?z)]    'thumfr-really-iconify-or-deiconify-frame) ; `C-M-z'
    ;; `e' for eye (fisheye)
    (define-key global-map "\C-xte" 'thumfr-doremi-thumbnail-frames+)          ; `C-x t e'
    ;; Make window-manager "minimize" button thumbify instead of iconify.
    ;; (define-key special-event-map [iconify-frame] 'thumfr-thumbify-frame-upon-event)
    ))

(eval-after-load "ucs-cmds"
  '(when (> emacs-major-version 22)     ; Need Emacs 23+ version of `insert-char'/`ucs-insert'.
    (when (commandp 'insert-char)       ; `ucs-insert' renamed to `insert-char' in Emacs 24.
      (define-key global-map [remap insert-char] 'ucsc-insert))
    (when (fboundp 'ucs-insert)
      (define-key global-map [remap ucs-insert] 'ucsc-insert))))

(define-key help-map "\C-\M-f" 'describe-face)                                 ; `C-h C-M-f'

;; `C-x' stuff.
;;
;; So you can do it with one hand.
(define-key ctl-x-map [(control ?z)] 'delete-window)                           ; `C-x C-z'

(eval-after-load "misc-cmds"
  '(progn
    (define-key ctl-x-map [home] 'mark-buffer-before-point)                    ; `C-x home'
    (define-key ctl-x-map [end]  'mark-buffer-after-point)                     ; `C-x end'
    (define-key ctl-x-map [(control ?\;)] 'comment-region-lines)               ; `C-x C-;'
    (define-key ctl-x-map "\M-f" 'region-to-file)                              ; `C-x M-f'
    (define-key ctl-x-map "L"    'goto-longest-line)                           ; `C-x L'
    (when (fboundp 'undo-repeat) (global-set-key [remap undo] 'undo-repeat))   ; `C-x u' etc.
    (when (fboundp 'next-buffer-repeat)
      (global-set-key [remap previous-buffer] 'previous-buffer-repeat)         ; `C-x left'
      (global-set-key [remap next-buffer]     'next-buffer-repeat))))          ; `C-x right'
  
;; In `chistory.el'.
(define-key ctl-x-map [(meta ?x)] 'repeat-matching-complex-command)            ; `C-x M-x'
(define-key ctl-x-map "c" 'font-lock-mode)                                     ; `C-x c'

(eval-after-load "frame-cmds"
  '(define-key ctl-x-map "o" 'other-window-or-frame))                          ; `C-x o'

(eval-after-load "highlight"
  '(progn
    (define-key ctl-x-map [(control ?y)] 'hlt-highlight)                       ; `C-x C-y'
    (define-key ctl-x-map [(down-mouse-2)] 'hlt-highlighter)                   ; `C-x mouse-2'
    (define-key ctl-x-map [(mouse-2)] 'ignore)
    (define-key ctl-x-map [(S-down-mouse-2)] 'hlt-eraser)                     ; `C-x S-mouse-2'
    (when (fboundp 'next-single-char-property-change) ; Emacs 21+
      (global-set-key [(shift control ?p)] 'hlt-previous-highlight)            ; `C-S-p'
      (global-set-key [(shift control ?n)] 'hlt-next-highlight))               ; `C-S-n'
    (global-set-key [(control meta shift ?s)] 'hlt-highlight-enclosing-list))) ; `C-M-S'

(eval-after-load "highlight-symbol"
  '(progn
    (global-set-key [(control f9)] 'highlight-symbol-at-point)                 ; `C-f9'
    (global-set-key [f9]           'highlight-symbol-next)                     ; `f9'
    (global-set-key [(shift f9)]   'highlight-symbol-prev)                     ; `S-f9'
    (global-set-key [(meta f3)]    'highlight-symbol-query-replace)))          ; `M-f9'

(eval-after-load "dired-x"
  '(progn
    (define-key ctl-x-map   [(control ?j)] 'dired-jump)                        ; `C-x j'
    (define-key ctl-x-4-map [(control ?j)] 'dired-jump-other-window)))         ; `C-x 4 j'

(eval-after-load "frame-cmds"
  '(progn
    (define-key ctl-x-4-map "1" 'delete-other-frames)                          ; `C-x 4 1'
    (define-key ctl-x-5-map "h" 'show-*Help*-buffer)))                         ; `C-x 5 h'

(eval-after-load "find-func+"           ; Emacs 22+
  '(define-key ctl-x-4-map "l" 'find-library-other-window))                    ; `C-x 4 l'
(find-function-setup-keys)  ;; C-x F, C-x 4 F, C-x 5 F, C-x K, C-x V, C-x 4 V, C-x 5 V

;; [f1] function key.
;;; (eval-after-load "help+"
;;;   ;; Standard binding is `help-command'
;;;   '(global-set-key [f1] 'help-on-click/key))                                   ; `f1'
;;; (eval-after-load "help+20"
;;;   '(global-set-key [f1] 'help-on-click/key))

(eval-after-load "misc-cmds"
  '(global-set-key [C-S-f1] 'region-to-buffer))                                ; `C-S-f1'

(global-set-key [M-S-f1] 'insert-buffer) ; Defined in `simple.el'.             ; `M-S-f1'
;; Defined in `font-lock.el'
(global-set-key [C-M-f1] 'font-lock-fontify-buffer)                            ; `C-M-f1'
(global-set-key [C-M-S-f1] 'rename-buffer)                                     ; `C-M-S-f1'

;; [f3] function key.
(eval-after-load "bm"
  '(progn
    (global-set-key (kbd "<S-f3>") 'bm-toggle)                                 ; `S-f3'
    (global-set-key (kbd "<C-f3>") 'bm-next)                                   ; `C-f3'
    (global-set-key (kbd "<M-f3>") 'bm-previous)))                             ; `M-f3'

;; [f5] function key - a la MS Windows.
(global-set-key [f5] 'revert-buffer)                                           ; `f5'
(eval-after-load "misc-cmds"
  '(global-set-key [f5] 'revert-buffer-no-confirm))                            ; `f5'

;; [insert] key.  [C-insert] is `kill-ring-save'.  [S-insert] is `yank'.
(global-set-key [M-insert] 'yank-pop) ; Defined in `simple.el'.                ; `M-insert'
(global-set-key [C-S-insert] 'insert-buffer) ; Defined in `simple.el'.         ; `C-S-insert'
(global-set-key [M-S-insert] 'yank-rectangle)                                  ; `M-S-insert'
(global-set-key [C-M-insert] 'lisp-complete-symbol)                            ; `C-M-insert'
(global-set-key [C-M-S-insert] 'insert-file)                                   ; `C-M-S-insert'

(eval-after-load "fuzzy-match"
  '(global-set-key "\M-#" 'lisp-spell-symbol))                                 ; `M-#'

;; [delete] key.
(global-set-key [delete] 'kill-line)    ; Defined in `simple.el'.                `delete'
(global-set-key [C-delete] 'kill-paragraph) ; Defined in `paragraphs.el'.        `C-delete'
(global-set-key [M-delete] 'kill-ring-save) ; Defined in `simple.el'.            `M-delete'
; Emacs standard: [S-delete] is `kill-region'.
(global-set-key [C-S-delete] 'append-next-kill) ; Defined in `simple.el'.        `C-S-delete'
(global-set-key [M-S-delete] 'kill-rectangle) ; Defined in `rect.el'.            `M-S-delete'
(global-set-key [C-M-delete] 'kill-sexp) ; Defined in `lisp.el'.                 `C-M-delete'
(global-set-key [C-M-S-delete] 'append-to-register) ; Defined in `register.el'.  `C-M-S-delete'

;; [backspace] key.
(global-set-key [C-backspace] 'backward-kill-paragraph) ; In `paragraphs.el'.    `C-backspace'

(eval-after-load "misc-cmds"
  '(global-set-key [C-S-backspace] 'region-to-file))                        ; `C-S-backspace'

(global-set-key [M-S-backspace] 'clear-rectangle) ; Defined in `rect.el'.     `M-S-backspace'
; This was standard in Emacs 20:
(global-set-key [C-M-backspace] 'backward-kill-sexp) ; In  `lisp.el'.         `C-M-backspace'
(global-set-key [C-M-S-backspace] 'copy-to-register) ; In `register.el'.      `C-M-S-backspace'

;; [pause] / [break] key:
;; NOTE: On Windows, [C-pause] is considered to be [C-cancel].  Still true for XP?

;; Better than the standard bindings `C-x <right>' and `C-x <right>',
;; because you can hold these down to repeat: cycle through buffers.
(when (fboundp 'next-buffer)            ; Emacs 21+.
  (global-set-key [C-pause] 'previous-buffer)                                   ; `C-pause'
  (global-set-key [M-pause] 'next-buffer))                                      ; `M-pause'

;; `iso-transl.el' is needed to use an ISO prefix key (e.g. `C-x 8'. [f8]).
;; It defines `key-translation-map'.
(require 'iso-transl)
;;;@@@Emacs20 ;; This lets users do `[f8] C-h]' for help on ISO chars.
;;;@@@Emacs20 (autoload 'help-iso-prefix "help+"
;;;@@@Emacs20   "Show commands bound to ISO (pseudo-)prefix key sequences." t)

  ;; Make [f8] key be a synonym for `C-x 8'. (Use [f8] as a compose key.)
(define-key key-translation-map [f8]   ; See `iso-transl.el'.                   ; `f8'
  (lookup-key key-translation-map "\C-x8"))

;; Make [f8] key be a synonym for `C-x 8' for isearch too.
;; This lets you search for accented chars using [f8].
(define-key isearch-mode-map [f8] nil)
;;;@@@Emacs20 ;; [f8] C-h and C-x 8 C-h  :=  Help for [f8] and C-x 8 prefixes:
;;;@@@Emacs20 (global-set-key (vector 'f8 help-char) 'help-iso-prefix) ; In `help.el'.
;;;@@@Emacs20 (define-key ctl-x-map "8\C-h" 'help-iso-prefix) ; Defined in `help.el'.

;; These *declp* commands are defined in `misc-cmds.el'.
;(global-set-key [C-print] 'declp-buffer) ; Print buffer. (defsubst)
;(global-set-key [M-print] 'pr-declp-buffer) ; Print buffer via `pr'.
;(global-set-key [C-M-print] 'declp-buffer-w-switches) ; User `declp' switches.
;(global-set-key [S-print] 'transpose-paragraphs) ; Defined in `paragraphs.el'
;(global-set-key [C-S-print] 'declp-region) ; Print region. (defsubst)
;(global-set-key [M-S-print] 'pr-declp-region) ; Print region via `pr'.
;(global-set-key [C-M-S-print] 'declp-region-w-switches) ; `declp' + switches.

;; [home], [end], [prior], and [next] keys.  These are used, unmodified, by
;; `s-region-move', which is defined in `s-region+.el'.  The following commands,
;; except `forward-page' & `backward-page', are also defined in `s-region+.el'.
;(global-set-key [C-prior] 'backward-page) ; Defined in `page.el'.
;(global-set-key [C-previous] 'backward-page)
;(global-set-key [C-next] 'forward-page) ; Defined in `page.el'.
;(global-set-key [next] 'scroll-up-windowful) ; Defined in `s-region+.el'.
;(global-set-key [previous] 'scroll-down-windowful) ; Defined in `s-region+.el'.
;(global-set-key [prior] 'scroll-down-windowful)
;(global-set-key [M-next] 'scroll-other-window-up-windowful) ; In `s-region+.el'
;(global-set-key [M-previous] 'scroll-other-window-down-windowful)
;(global-set-key [M-prior] 'scroll-other-window-down-windowful) ; `s-region+.el'
;(global-set-key [home] 'goto-point-min) ; (defsubst) Defined in `s-region+.el'.
;(global-set-key [end] 'goto-point-max) ; (defsubst) Defined in `s-region+.el'.
;(global-set-key [M-home] 'bob-other-window) ; (defsubst) In `s-region+.el'.
;(global-set-key [M-end] 'eob-other-window) ; (defsubst) In `s-region+.el'.
;(s-region-bind (list [prior] [C-prior] [M-prior]))

(eval-after-load "swiss-move"
  '(progn
    (global-set-key [S-prior] 'swiss-move-line-up)                            ; `S-prior'
    (global-set-key [S-next]  'swiss-move-line-down)))                        ; `S-next'

;; [up], [down], [left], [right] keys.
(global-set-key [S-down] (lambda () (interactive) (scroll-up 1)))             ; `S-down'
(global-set-key [S-up] (lambda () (interactive) (scroll-down 1)))             ; `S-up'
;;(global-set-key [M-up] (lookup-key esc-map "p")) ; Probably not defined.
;;(global-set-key [M-down] (lookup-key esc-map "n")) ; Probably not defined.
;;(global-set-key [M-left] (lookup-key esc-map "b")) ; Predefined.
;;(global-set-key [M-right] (lookup-key esc-map "f")) ; Predefined.
(global-set-key [C-M-home] 'beginning-of-defun)                               ; `C-M-home'
(global-set-key [C-M-end] 'end-of-defun)                                      ; `C-M-end'

(eval-after-load "misc-cmds"
  '(progn
    (global-set-key "\M-m" 'to-indentation-repeat-backward)                   ; `M-m'
    (global-set-key "\M-n" 'to-indentation-repeat-forward)))                  ; `M-n'

;;;-----------REPLACEMENT BINDINGS------------------------------------

(defvar sub-*-of-line t
  "*Non-nil means remap `*-of-line' commands to `*-of-line+' globally.
This applies to `move-to-(beginning|end)-of-line', if defined, or to
`(beginning|end)-of-line', otherwise.
This has no effect unless you use library `misc-cmds.el'.")

(defvar sub-delete-windows-for t
  "*Non-nil means remap `delete-window' to `delete-windows-for' globally.
This has no effect unless you use library `frame-cmds.el'.")

(defvar sub-kill-buffer-and-its-windows t
  "*Non-nil means remap `kill-buffer' to `kill-buffer-and-its-windows' globally.
This has no effect unless you use library `misc-cmds.el'.")

(defvar sub-pp-evals t
  "*Non-nil means remap `eval-*' commands to `pp-eval-*' globally.
Thus, `pp-eval-expression' replaces `eval-expression' and
`pp-eval-last-sexp' replaces `eval-last-sexp'.
This has no effect unless you use library `pp+.el'.")

(defvar sub-query-replace-w-options t
  "*Non-nil means remap `query-replace' to `query-replace-w-options' globally.
This has no effect unless you use library `replace+.el'.")

(defvar sub-quit-window-delete (fboundp 'quit-restore-window) ; Emacs 24.3+
  "*Non-nil means remap `quit-window' to `quit-window-delete' globally.
This has no effect unless you use library `misc-cmds.el' and Emacs
24.4 or later.")

(defvar sub-recenter-top-bottom t
  "*Non-nil means remap `recenter' to `sub-recenter-top-bottom' globally.
This has no effect unless you use library `misc-cmds.el'.")

(defvar sub-transpose-sexps t
  "*Non-nil means remap `transpose-sexps' to `reversible-transpose-sexps'.
This has no effect unless you use library `misc-cmds.el'.")


;;; Do these all *after* load `menu-bar+.el', since that sets original bindings.

(eval-after-load "frame-cmds"
  '(when sub-delete-windows-for
    (remap-command 'delete-window 'delete-windows-for global-map)))
(eval-after-load "replace+"
  '(when sub-query-replace-w-options
    (remap-command 'query-replace 'query-replace-w-options global-map)))
(eval-after-load "misc-cmds"
  '(when sub-kill-buffer-and-its-windows
    (remap-command 'kill-buffer 'kill-buffer-and-its-windows global-map)))
(eval-after-load "pp+"
  '(when sub-pp-evals
    (remap-command 'eval-last-sexp 'pp-eval-last-sexp global-map)
    (remap-command 'eval-expression 'pp-eval-expression global-map)))
(when (fboundp 'buffer-menu)
  (remap-command 'list-buffers 'buffer-menu global-map)) ; In `buff-menu+.el'.
(eval-after-load "misc-cmds"
  '(progn
    (when sub-*-of-line
      (cond ((fboundp 'move-beginning-of-line)
             (remap-command 'move-beginning-of-line 'beginning-of-line+ global-map)
             (remap-command 'move-end-of-line 'end-of-line+ global-map))
            (t
             (remap-command 'beginning-of-line 'beginning-of-line+ global-map)
             (remap-command 'end-of-line 'end-of-line+ global-map)))
      (when (boundp 'visual-line-mode-map)
        (define-key visual-line-mode-map [remap move-beginning-of-line] nil)
        (define-key visual-line-mode-map [remap move-end-of-line]       nil)
        (define-key visual-line-mode-map [home] 'beginning-of-line+)
        (define-key visual-line-mode-map [end]  'end-of-line+)
        (define-key visual-line-mode-map "\C-a" 'beginning-of-visual-line+)
        (define-key visual-line-mode-map "\C-e" 'end-of-visual-line+)))
    (when sub-recenter-top-bottom
      (remap-command 'recenter 'recenter-top-bottom global-map))
    (when sub-transpose-sexps
      (remap-command 'transpose-sexps 'reversible-transpose-sexps global-map))))
(eval-after-load "misc-cmds"
  '(when sub-quit-window-delete
    (remap-command 'quit-window 'quit-window-delete global-map)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-keys.el ends here
