;;; setup-keys.el --- Some key bindings.
;;
;; Filename: setup-keys.el
;; Description: Some key bindings.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2009, Drew Adams, all rights reserved.
;; Created: Fri Apr  2 12:34:20 1999
;; Version: 21.1
;; Last-Updated: Sat Nov  7 16:16:16 2009 (-0700)
;;           By: dradams
;;     Update #: 969
;; URL: http://www.emacswiki.org/cgi-bin/wiki/setup-keys.el
;; Keywords: mouse, keyboard, menus, menu-bar
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `cl', `color-theme', `cus-face',
;;   `doremi', `doremi-cmd', `doremi-frm', `easymenu', `eyedropper',
;;   `faces', `faces+', `fit-frame', `frame-cmds', `frame-fns',
;;   `help+20', `hexrgb', `highlight', `info', `info+', `isearch+',
;;   `iso-transl', `menu-bar', `menu-bar+', `misc-cmds', `misc-fns',
;;   `mouse', `mouse+', `mwheel', `pp', `pp+', `replace+', `ring',
;;   `ring+', `second-sel', `simple+', `strings', `thingatpt',
;;   `thingatpt+', `unaccent', `w32browser-dlgopen', `wid-edit',
;;   `wid-edit+', `widget'.
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
;;  library.  For example, if you do not want to substitute command
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
;;    `sub-query-replace-w-options', `sub-recenter-top-bottom'.
;;
;;  Other variables defined here:
;;
;;    `comparison-map', `doremi-map'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
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

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless
(require 'frame-cmds nil t) ;; (no error if not found): delete-other-frames,
                            ;; delete-windows-for, enlarge-frame*,
                            ;; iconify-everything, iconify/map-frame, move-frame-*,
                            ;; mouse-iconify/map-frame, mouse-remove-window,
                            ;; mouse-show-hide-mark-unmark, other-window-or-frame,
                            ;; show-*Help*-buffer, show-hide, shrink-frame*
(require 'mouse+ nil t) ;; (no error if not found): mouse-tear-off-window, mouse-flash-position
(require 'simple+ nil t) ;; (no error if not found): comment-region
(require 'highlight nil t) ;; (no error if not found): hlt-highlight, hlt-highlighter,
                           ;; hlt-eraser, hlt-(next|previous)-highlight
(require 'misc-cmds nil t) ;; (no error if not found): beginning-of-line+,
                           ;; end-of-line+, goto-longest-line, kill-buffer-and-its-windows,
                           ;; mark-buffer-after-point, mark-buffer-before-point,
                           ;; recenter-top-bottom, region-to-buffer, region-to-file
(require 'second-sel nil t) ;; (no error if not found): secondary-dwim
(require 'pp+ nil t) ;; (no error if not found): pp-eval-expression
(require 'fit-frame nil t) ;; (no error if not found):
                           ;; fit-frame, fit-frame-or-mouse-drag-vertical-line
(require 'doremi-frm nil t) ;; (no error if not found): doremi-bg+, doremi-face-fg+,
                            ;; doremi-font+, doremi-frame-font-size+, doremi-frame-configs+,
                            ;; doremi-frame-height+, doremi-frame-horizontally+,
                            ;; doremi-frame-vertically+
(require 'doremi-cmd nil t) ;; (no error if not found): doremi-buffers+, doremi-bookmarks+,
                            ;; doremi-color-themes+,

(when (< emacs-major-version 21)
  (require 'help+20 nil t) ;; (no error if not found): help-on-click/key
  (require 'unaccent nil t)) ;; (no error if not found): unaccent-region, unaccent-word
(when (> emacs-major-version 21)
  (require 'help+ nil t)) ;; (no error if not found): help-on-click/key
(require 'replace+ nil t)   ;; (no error if not found): query-replace-w-options

;; Quiet the byte compiler.
(defvar grep-mode-map)                  ; Defined in `grep.el'.
(defvar mouse-wheel-down-event)         ; Defined in `mwheel.el'.
(defvar mouse-wheel-up-event)           ; Defined in `mwheel.el'.

;;;-----------------------------

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
(global-set-key [C-M-mouse-1] 'mouse-start-secondary) ; In `mouse.el'.
(global-set-key [C-M-drag-mouse-1] 'mouse-set-secondary) ; In `mouse.el'.
(global-set-key [C-M-down-mouse-1] 'mouse-drag-secondary) ; In `mouse.el'.
(global-set-key [C-M-mouse-3] 'mouse-secondary-save-then-kill) ; In `second-sel.el'.
(global-set-key [C-M-mouse-2] 'mouse-yank-secondary) ; In `mouse+.el' or `mouse.el'.

(when (fboundp 'mouse-flash-position-or-M-x) ; Defined in `mouse+.el'. Highlight yank position
  (global-set-key [down-mouse-2] 'mouse-flash-position-or-M-x) ; or call `M-x' in echo area.
  (global-set-key [S-down-mouse-2] 'mouse-scan-lines-or-M-:)) ; Highlight line or `M-:'.

(when (fboundp 'secondary-dwim)         ; Defined in `second-sel.el'.
  (global-set-key [(control meta ?y)] 'secondary-dwim)
  (define-key esc-map "y" 'yank-pop-commands))

;; Because M-C is being used for secondary.
(setq foldout-mouse-modifiers '(meta shift)) ; Defined in `foldout.el'.

(when (and (boundp '1on1-minibuffer-frame) ; Defined in `oneonone.el'.
           (framep 1on1-minibuffer-frame))
  (define-key minibuffer-local-map "\C-o" '1on1-fit-minibuffer-frame)
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key minibuffer-local-must-match-map "\C-o" '1on1-fit-minibuffer-frame)
    (define-key minibuffer-local-completion-map "\C-o" '1on1-fit-minibuffer-frame)))

;; These are defined in `frame-cmds.el'.
(when (featurep 'frame-cmds)
  (global-set-key [(meta up)]    'move-frame-up)
  (global-set-key [(meta down)]  'move-frame-down)
  (global-set-key [(meta left)]  'move-frame-left)
  (global-set-key [(meta right)] 'move-frame-right)
  (global-set-key [(control meta up)]    'shrink-frame)
  (global-set-key [(control meta down)]  'enlarge-frame)
  (global-set-key [(control meta left)]  'shrink-frame-horizontally)
  (global-set-key [(control meta right)] 'enlarge-frame-horizontally)
  (global-set-key [(control ?z)] 'iconify/map-frame) ; Replaces`iconify-or-deiconify-frame'.
  ;; $$$$ (global-set-key [(control ?x) (control ?z)] 'iconify-everything)
  (global-set-key [(shift control meta ?z)] 'show-hide)
  (global-set-key [C-down-mouse-1] 'mouse-show-hide-mark-unmark)
  (global-set-key [S-down-mouse-1] nil) ; Get rid of `mouse-set-font'.
  
  ;;(global-set-key [vertical-line mouse-1] 'ignore)
  (global-set-key [vertical-line C-down-mouse-1] 'show-hide)
  ;;(global-set-key [vertical-line C-mouse-1] 'ignore)
  (global-set-key [vertical-line S-down-mouse-1] 'iconify-everything)
  ;;(global-set-key [vertical-line S-mouse-1] 'ignore)

  ;; [mode-line mouse-3] as deletion (Emacs std) is too hazardous.  Iconify instead.
  (global-set-key [mode-line mouse-3] 'mouse-iconify/map-frame)
  (global-set-key [mode-line C-mouse-3] 'mouse-remove-window)
  )

(when (featurep 'zoom-frm)              ; `zoom-frm.el' requires `frame-cmds.el'.
  (global-set-key [S-mouse-1] 'zoom-in)
  (global-set-key [C-S-mouse-1] 'zoom-out)
  (global-set-key (if (boundp 'mouse-wheel-down-event)
                      (vector (list 'control mouse-wheel-down-event))
                    [C-mouse-wheel])    ; Emacs 20, 21
                  'zoom-in)
  (when (boundp 'mouse-wheel-up-event)
    (global-set-key (vector (list 'control mouse-wheel-up-event)) 'zoom-out)))

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

(when (featurep 'mouse+)
  (global-set-key [mode-line C-mouse-1] 'mouse-tear-off-window))

;; These are defined in `fit-frame.el'.
(when (featurep 'fit-frame)
  (global-set-key [(control ?x) (control ?_)] 'fit-frame)
  (global-set-key [vertical-line down-mouse-1] 'fit-frame-or-mouse-drag-vertical-line))

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
(global-set-key [(control meta ?=)] 'compare-windows) ; Defined in `compare-w.el'.
(defvar comparison-map (lookup-key global-map [?\C-=])
  "Prefix keymap for comparison commands.")
(unless (keymapp comparison-map)
  (setq comparison-map (make-sparse-keymap))
  (global-set-key [(control ?=)] comparison-map)
  (define-key comparison-map "b" 'ediff-buffers) ; Defined in `ediff.el'.
  (define-key comparison-map "e" 'ediff-files) ; Defined in `ediff.el'.
  (define-key comparison-map "f" 'ediff-files) ; Defined in `ediff.el'.
  (define-key comparison-map "d" 'diff) ; Defined in `diff+.el'.
  (define-key comparison-map "w" 'compare-windows) ; Defined in `compare-w.el'.
  )

;; Completions (non-minibuffer).
;(global-set-key "\M-\r" 'complete)      ; Defined in `completion.el':
;(global-set-key [?\C-\r] 'complete)
;(define-key function-key-map [C-return] [?\C-\r])

(define-key text-mode-map [(meta ?j)] 'fill-individual-paragraphs) ; Defined in `fill.el'.
(global-set-key [(meta ?$)] 'ispell-complete-word)
(global-set-key [(meta ?_)] 'forward-whitespace) ; Defined in `thingatpt.el'
(unless (lookup-key global-map [(meta ?s)]) ; Emacs 23 co-opts `M-s-' as a prefix key.
  (global-set-key [(meta ?s)] 'forward-symbol)) ; Defined in `thingatpt.el'

;; These replace the bindings for `mark-sexp' and `mark-word'.  Defined in `thing-cmds.el'.
(global-set-key [(control meta ? )] 'mark-thing)
(global-set-key [(meta ?@)] 'cycle-thing-region)

(when (fboundp 'crosshairs-mode)
  (global-set-key [(control ?+)] 'crosshairs-mode)) ;  Defined in `crosshairs.el'.

(when (featurep 'unaccent)
  (global-set-key [(meta ?\")] 'unaccent-word) ; Defined in `unaccent.el'.
  (define-key ctl-x-map [\"] 'unaccent-region)) ; Defined in `unaccent.el'.

;;; Do Re Mi commands
(when (featurep 'doremi-frm)
  (unless (fboundp 'doremi-prefix)
    (defalias 'doremi-prefix (make-sparse-keymap))
    (defvar doremi-map (symbol-function 'doremi-prefix)
      "Keymap for Do Re Mi commands."))
  (define-key global-map "\C-xt" 'doremi-prefix)
  (define-key doremi-map "a" 'doremi-all-faces-fg+) ; "All"
  (define-key doremi-map "c" 'doremi-bg+) ; "Color"
  (define-key doremi-map "f" 'doremi-face-fg+) ; Face"
  (define-key doremi-map "h" 'doremi-frame-height+)
  (define-key doremi-map "k" 'doremi-face-bg+) ; bacKground"
  (define-key doremi-map "t" 'doremi-font+) ; "Typeface"
  (define-key doremi-map "u" 'doremi-frame-configs+) ; "Undo"
  (define-key doremi-map "x" 'doremi-frame-horizontally+)
  (define-key doremi-map "y" 'doremi-frame-vertically+)
  (define-key doremi-map "z" 'doremi-frame-font-size+)) ; "Zoom"
(when (featurep 'doremi-cmd)
  (unless (fboundp 'doremi-prefix)
    (defalias 'doremi-prefix (make-sparse-keymap))
    (defvar doremi-map (symbol-function 'doremi-prefix)
      "Keymap for Do Re Mi commands."))
  (define-key global-map "\C-xt"  'doremi-prefix)
  (define-key doremi-map "b" 'doremi-buffers+)
  (define-key doremi-map "g" 'doremi-global-marks+)
  (define-key doremi-map "m" 'doremi-marks+)
  (define-key doremi-map "r" 'doremi-bookmarks+) ; `r' for Reading books
  (define-key doremi-map "s" 'doremi-color-themes+) ; `s' for color Schemes
  (define-key doremi-map "w" 'doremi-window-height+))

(when (featurep 'frame-cmds)
  (unless (fboundp 'doremi-prefix)
    (defalias 'doremi-prefix (make-sparse-keymap))
    (defvar doremi-map (symbol-function 'doremi-prefix)
      "Keymap for Do Re Mi commands."))
  (define-key global-map "\C-xt"  'doremi-prefix)
  (define-key doremi-map "." 'save-frame-config))
(when (fboundp 'thumfr-thumbify-other-frames)  ; These are defined in `thumb-frm.el'
  (global-set-key [(shift mouse-3)]         'thumfr-toggle-thumbnail-frame)
  (global-set-key [(shift control mouse-3)] 'thumfr-thumbify-other-frames)
  (global-set-key [(shift control ?z)]      'thumfr-thumbify-other-frames)
  (global-set-key [(shift control ?n)]      'thumfr-fisheye-next-frame)
  (global-set-key [(shift control ?p)]      'thumfr-fisheye-previous-frame)
  (global-set-key [(control meta ?z)]       'thumfr-really-iconify-or-deiconify-frame)
  (define-key global-map "\C-xte" 'thumfr-doremi-thumbnail-frames+) ; `e' for eye (fisheye)
  ;; Make window-manager "minimize" button thumbify instead of iconify.
  ;; (define-key special-event-map [iconify-frame] 'thumfr-thumbify-frame-upon-event)
  )

(define-key help-map "\C-\M-f" 'describe-face)

;; `C-x' stuff.
(define-key ctl-x-map [(control ?z)] 'delete-window) ; So you can do it with one hand.
(define-key ctl-x-map [(control ?\;)] 'comment-region) ; Defined in `simple+.el'.
(define-key ctl-x-map [home] 'mark-buffer-before-point) ; Defined in `misc-cmds.el'.
(define-key ctl-x-map [end]  'mark-buffer-after-point) ; Defined in `misc-cmds.el'.
(define-key ctl-x-map "\M-f" 'region-to-file) ; Defined in `misc-cmds.el'.
(define-key ctl-x-map [(meta ?x)] 'repeat-matching-complex-command) ; `chistory.el'.
(define-key ctl-x-map "c" 'font-lock-mode)
(define-key ctl-x-map "L" 'goto-longest-line) ; Defined in `misc-cmds.el'.
(when (fboundp 'other-window-or-frame)
  (define-key ctl-x-map "o" 'other-window-or-frame)) ; Defined in `frame-cmds.el'.
(when (fboundp 'hlt-highlighter)  ; Defined in `highlight.el'.
  (define-key ctl-x-map [(control ?y)] 'hlt-highlight)
  (define-key ctl-x-map [(down-mouse-2)] 'hlt-highlighter)
  (define-key ctl-x-map [(mouse-2)] 'ignore)
  (define-key ctl-x-map [(S-down-mouse-2)] 'hlt-eraser)
  (when (fboundp 'next-single-char-property-change) ; Emacs 21+
    (global-set-key [(shift control ?p)] 'hlt-previous-highlight)
    (global-set-key [(shift control ?n)] 'hlt-next-highlight)))
(define-key ctl-x-map [(control ?j)] 'dired-jump) ; Defined in `dired-x.el'.

(define-key ctl-x-4-map [(control ?j)] 'dired-jump-other-window) ; In `dired-x.el'.
(when (fboundp 'delete-other-frames)    ; Defined in `frame-cmds.el'.
  (define-key ctl-x-4-map "1" 'delete-other-frames)
  (define-key ctl-x-5-map "h" 'show-*Help*-buffer)) ; Defined in `frame-cmds.el'.

;; [f1] function key.
(when (fboundp 'help-on-click/key)
  (global-set-key [f1] 'help-on-click/key)) ; Standard binding is `help-command'
(global-set-key [C-S-f1] 'region-to-buffer) ; Defined in `misc-cmds.el'.
(global-set-key [M-S-f1] 'insert-buffer) ; Defined in `simple.el'.
(global-set-key [C-M-f1] 'font-lock-fontify-buffer) ; Defined in `font-lock.el'
(global-set-key [C-M-S-f1] 'rename-buffer)

;; [f3] function key.
(when (fboundp 'bm-toggle)
  (global-set-key (kbd "<S-f3>") 'bm-toggle)
  (global-set-key (kbd "<C-f3>") 'bm-next)
  (global-set-key (kbd "<M-f3>") 'bm-previous))

;; [f5] function key - a la MS Windows.
(if (fboundp 'revert-buffer-no-confirm) ; Defined in `misc-cmds.el'.
    (global-set-key [f5] 'revert-buffer-no-confirm)
  (global-set-key [f5] 'revert-buffer))

;; [insert] key.  [C-insert] is `kill-ring-save'.  [S-insert] is `yank'.
(global-set-key [M-insert] 'yank-pop) ; Defined in `simple.el'.
(global-set-key [C-S-insert] 'insert-buffer) ; Defined in `simple.el'.
(global-set-key [M-S-insert] 'yank-rectangle)
(global-set-key [C-M-insert] 'lisp-complete-symbol)
(when (fboundp 'lisp-spell-symbol)      ; Defined in `fuzzy-match.el'.
  (global-set-key "\M-#" 'lisp-spell-symbol))
(global-set-key [C-M-S-insert] 'insert-file)

;; [delete] key.
(global-set-key [delete] 'kill-line)    ; Defined in `simple.el'.
(global-set-key [C-delete] 'kill-paragraph) ; Defined in `paragraphs.el'.
(global-set-key [M-delete] 'kill-ring-save) ; Defined in `simple.el'.
; Emacs standard: [S-delete] is `kill-region'.
(global-set-key [C-S-delete] 'append-next-kill) ; Defined in `simple.el'.
(global-set-key [M-S-delete] 'kill-rectangle) ; Defined in `rect.el'.
(global-set-key [C-M-delete] 'kill-sexp) ; Defined in `lisp.el'.
(global-set-key [C-M-S-delete] 'append-to-register) ; Defined in `register.el'.

;; [backspace] key.
(global-set-key [C-backspace] 'backward-kill-paragraph) ; In `paragraphs.el'.
(global-set-key [C-S-backspace] 'region-to-file) ; Defined in `misc-cmds.el'.
(global-set-key [M-S-backspace] 'clear-rectangle) ; Defined in `rect.el'.
; This was standard in Emacs 20:
(global-set-key [C-M-backspace] 'backward-kill-sexp) ; In  `lisp.el'.
(global-set-key [C-M-S-backspace] 'copy-to-register) ; In `register.el'.

;; [pause] / [break] key:
;; NOTE: On Windows, [C-pause] is considered to be [C-cancel].  Still true for XP?

;; Better than the standard bindings `C-x <right>' and `C-x <right>',
;; because you can hold these down to repeat: cycle through buffers.
(when (> emacs-major-version 21)
  (global-set-key [C-pause] 'previous-buffer)
  (global-set-key [M-pause] 'next-buffer))

;; `iso-transl.el' is needed to use an ISO prefix key (e.g. `C-x 8'. [f12]).
(require 'iso-transl)                   ; Defines `key-translation-map'.
;;;@@@Emacs20 ;; This lets users do `[f12] C-h]' for help on ISO chars.
;;;@@@Emacs20 (autoload 'help-iso-prefix "help+"
;;;@@@Emacs20   "Show commands bound to ISO (pseudo-)prefix key sequences." t)

;; Make [f12] key be a synonym for `C-x 8'. (Use [f12] as a compose key.)
(define-key key-translation-map [f12] ; See `iso-transl.el'.
  (lookup-key key-translation-map "\C-x8"))
;; Make [f12] key be a synonym for `C-x 8' for isearch too.
;; This lets you search for accented chars using [f12].
(define-key isearch-mode-map [f12] nil)
;;;@@@Emacs20 ;; [f12] C-h and C-x 8 C-h  :=  Help for [f12] and C-x 8 prefixes:
;;;@@@Emacs20 (global-set-key (vector 'f12 help-char) 'help-iso-prefix) ; In `help.el'.
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

(when (fboundp 'swiss-move-line-up)     ; Defined in `swiss-move.el'.
  (global-set-key [S-prior] 'swiss-move-line-up)
  (global-set-key [S-next]  'swiss-move-line-down))

;; [up], [down], [left], [right] keys.
(global-set-key [S-down] '(lambda () (interactive)(scroll-up 1)))
(global-set-key [S-up] '(lambda () (interactive)(scroll-down 1)))
;;(global-set-key [M-up] (lookup-key esc-map "p")) ; Probably not defined.
;;(global-set-key [M-down] (lookup-key esc-map "n")) ; Probably not defined.
;;(global-set-key [M-left] (lookup-key esc-map "b")) ; Predefined.
;;(global-set-key [M-right] (lookup-key esc-map "f")) ; Predefined.
(global-set-key [C-M-home] 'beginning-of-defun)
(global-set-key [C-M-end] 'end-of-defun)


;;;-----------REPLACEMENT BINDINGS------------------------------------

(defvar sub-*-of-line t
  "*Non-nil means `substitute-key-definition' of `*-of-line' commands
by `*-of-line+', everywhere.
This applies to `move-to-(beginning|end)-of-line', if defined, or to
`(beginning|end)-of-line', otherwise.")

(defvar sub-recenter-top-bottom t
  "*Non-nil means `substitute-key-definition' of `recenter' command
by `sub-recenter-top-bottom', everywhere.")

(when (fboundp 'kill-buffer-and-its-windows) ; Defined in `misc-cmds.el'.
  (defvar sub-kill-buffer-and-its-windows t
    "*Non-nil means `substitute-key-definition' of `kill-buffer' command
by `kill-buffer-and-its-windows', everywhere."))

(defvar sub-pp-evals t
  "*Non-nil means `substitute-key-definition' of `eval-*' commands
by `pp-eval-*', everywhere.  Thus, `pp-eval-expression' replaces
`eval-expression' and `pp-eval-last-sexp' replaces `eval-last-sexp'")

(when (fboundp 'query-replace-w-options) ; Defined in `replace+.el'.
  (defvar sub-query-replace-w-options t
    "*Non-nil means `substitute-key-definition' of `query-replace' command
by `query-replace-w-options', everywhere."))

(when (fboundp 'delete-windows-for) ; Defined in `frame-cmds.el'.
  (defvar sub-delete-windows-for t
    "*Non-nil means `substitute-key-definition' of `delete-window' command
by `delete-windows-for', everywhere."))


;; Do this *after* load `menu-bar+.el', since that sets original bindings.
(when (and (fboundp 'delete-windows-for) sub-delete-windows-for)
  (substitute-key-definition 'delete-window ; Defined in `frame-cmds.el'.
                             'delete-windows-for global-map))
(when (and (fboundp 'query-replace-w-options) sub-query-replace-w-options)
  (substitute-key-definition 'query-replace ; Defined in `replace+.el'.
                             'query-replace-w-options global-map))
(when (and (fboundp 'kill-buffer-and-its-windows) sub-kill-buffer-and-its-windows)
  (substitute-key-definition 'kill-buffer ; Defined in `misc-cmds.el'.
                             'kill-buffer-and-its-windows global-map))
(when sub-pp-evals
  (substitute-key-definition 'eval-last-sexp
                             'pp-eval-last-sexp global-map) ; In `pp.el'.
  (substitute-key-definition 'eval-expression
                             'pp-eval-expression global-map)) ; In `pp+.el'.

(when (fboundp 'buffer-menu)
  (substitute-key-definition 'list-buffers ; Redefined in `buff-menu+.el'.
                             'buffer-menu global-map))

(when (and sub-*-of-line (fboundp 'beginning-of-line+)) ; Defined in `misc-cmds.el'.
  (cond ((fboundp 'move-beginning-of-line)
         (substitute-key-definition 'move-beginning-of-line 'beginning-of-line+ global-map)
         (substitute-key-definition 'move-end-of-line 'end-of-line+ global-map))
        (t
         (substitute-key-definition 'beginning-of-line 'beginning-of-line+ global-map)
         (substitute-key-definition 'end-of-line 'end-of-line+ global-map))))

(when (and sub-recenter-top-bottom (fboundp 'recenter-top-bottom)) ; Defined in `misc-cmds.el'.
  (substitute-key-definition 'recenter 'recenter-top-bottom global-map))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-keys.el ends here
