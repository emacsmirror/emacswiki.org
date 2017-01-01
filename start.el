;;; start.el --- Main Emacs startup file: require/autoload other files.
;;
;; Filename: start.el
;; Description: Main Emacs startup file: require/autoload other files.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1995-2017, Drew Adams, all rights reserved.
;; Created: Wed Aug  2 11:12:24 1995
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 11:38:36 2017 (-0800)
;;           By: dradams
;;     Update #: 3064
;; URL: http://www.emacswiki.org/start.el
;; Keywords: abbrev, internal, local, init
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `apropos-fn+var', `assoc',
;;   `autofit-frame', `avoid', `bookmark', `bookmark+',
;;   `bookmark+-1', `bookmark+-bmu', `bookmark+-key',
;;   `bookmark+-lit', `browse-kill-ring', `browse-kill-ring+',
;;   `buff-menu+', `cl', `color-moccur', `compile', `compile+20',
;;   `compile-20', `cus-edit', `cus-edit+', `cus-face', `cus-load',
;;   `cus-start', `cus-theme', `custom', `dired', `dired+',
;;   `dired-aux', `dired-details', `dired-details+',
;;   `dired-sort-menu', `dired-sort-menu+', `dired-x', `doremi',
;;   `doremi-cmd', `doremi-frm', `easymenu', `ediff', `ediff+',
;;   `ediff-diff', `ediff-help', `ediff-init', `ediff-merg',
;;   `ediff-mult', `ediff-util', `ediff-wind', `em-joc', `emacsbug',
;;   `eshell-auto', `eyedropper', `facemenu', `facemenu+', `faces',
;;   `faces+', `ffap', `files+', `find-dired', `find-dired+',
;;   `finder', `finder+', `finder-inf', `fit-frame', `font-lock',
;;   `font-lock-menus', `frame-cmds', `frame-fns', `fuzzy-match',
;;   `header2', `help+20', `hexrgb', `highlight', `highlight-chars',
;;   `icomplete', `icomplete+', `image-dired', `image-file', `imenu',
;;   `imenu+', `info', `info+20', `isearch+', `iso-transl',
;;   `lacarte', `lib-requires', `lisp-mnt', `loadhist', `local-lpr',
;;   `local-ps-print', `lpr', `ls-lisp', `ls-lisp+',
;;   `ls-lisp-verbosity', `menu-bar', `menu-bar+', `misc-cmds',
;;   `misc-fns', `moccur-edit', `mouse', `mouse+', `mwheel', `naked',
;;   `occur-schroeder', `oneonone', `paren', `pcmpl-auto', `pp',
;;   `pp+', `pp-c-l', `printing', `ps-print', `replace+', `ring',
;;   `ring+', `savehist-20+', `second-sel', `sendmail', `setup',
;;   `setup-keys', `simple+', `speedbar', `start', `strings',
;;   `subr+', `subr-21', `swiss-move', `synonyms', `thing-cmds',
;;   `thingatpt', `thingatpt+', `thumb-frm', `time-date', `timer',
;;   `timer+', `unaccent', `vc', `vc+', `vc-', `vc-hooks',
;;   `vc-hooks+', `w32-browser', `w32browser-dlgopen', `wid-edit',
;;   `wid-edit+', `widget', `window+', `zones', `zoom-frm'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Drew Adams's Emacs startup file: requires/autoloads other files.
;;
;;  This file basically just does `require' and `autoload'.
;;
;;  The files `setup.el', `oneonone.el', and `setup-keys.el', required
;;  here, are companion files that do fairly essential
;;  configuration/customization of the editor.
;;
;;  The file `start-opt.el', not required here, is another companion
;;  file that does other recommended configuration/customization.  If
;;  you decide to load it, that should be done after loading
;;  `start.el'.
;;
;;  Some other files that are required here are extensions to standard
;;  Emacs files that each need to be loaded either before or after the
;;  corresponding standard file.  Their names reflect this: the
;;  standard file name is used, with a `-' (`+') appended if the file
;;  is to be loaded before (after) the standard file.  For example,
;;  `foo-.el' is to be loaded before the standard file `foo.el',
;;  `foo+.el' is to be loaded after `foo.el'.
;;
;;  NOTE: Order of loading here works.  Other orders not guaranteed:).
;;
;;
;;  New user option (variable) defined here: `use-dired-x-p'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Change Log:
;;
;; 2016/07/01 dadams
;;     Do not require bm.el (Bookmark+ does everything it does, and better.)
;; 2015/09/20 dadams
;;     Soft-require flx.el.
;; 2015/08/20 dadams
;;     Soft-require find-dired+.el (again) - moved require just before dired+.el.
;; 2015/08/16 dadams
;;     Soft-require zones.el.
;;     No longer soft-require wide-n.el (obsolete - merged into zones.el).
;; 2015/03/02 dadams
;;     Soft-require echo-bell.el.
;; 2014/05/27 dadams
;;     Soft-require subr+.el.
;; 2014/05/14 dadams
;;     Soft-require highlight-chars.el instead of autoloading.
;;     Soft-require narrow-indirect.el.
;; 2014/05/04 dadams
;;     Use new library info+20.el for Emacs prior to 23.  info+.el is now only for Emacs 23 and later.
;; 2014/02/11 dadams
;;     Autoload of insert-time-string: swapped last two args.  See Emacs bug #16725.
;; 2013/08/23 dadams
;;     Soft-require highlight-symbol.el (Emacs 22+).
;; 2013/07/20 dadams
;;     Do not (soft)-require dired-details+.el if (fboundp 'dired-hide-details-mode) - Emacs 24.4.
;; 2013/07/02 dadams
;;     Load bookmark+.el (and bm.el and tabbar.el, though not necessary) after `menu-bar+.el.
;; 2013/06/06 dadams
;;     Added autoload for auto-update-file-header.
;; 2013/02/21 dadams
;;     Autoload register-list.el.
;; 2012/11/16 dadams
;;     New library highlight-chars.el replaces obsolete show-wspace.el.
;; 2012/11/14 dadams
;;     More autoloads for show-wspace.el commands.
;; 2012/11/13 dadams
;;     font-lock-menus.el replaces font-menus-da.el.
;; 2012/10/23 dadams
;;     Applied renaming: imenu-add-defs-to-menubar to imenup-add-defs-to-menubar.
;; 2012/08/26 dadams
;;     Soft require font-menus-da.el if possible (fixes font-menus.el for Emacs 24).
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;;     Explicitly require thingatpt.el, before thingatpt+.el.  (Removed eval-after-load thingatpt.el.)
;; 2012/08/11 dadams
;;     Do not require frame+.el if window+.el was loaded.
;; 2012/08/10 dadams
;;     Updated names of show-wspace commands.
;; 2012/06/02 dadams
;;     Soft require ucs-cmds.el (Emacs 23+).
;; 2012/05/07 dadams
;;     Do not require tabbar-mode for Emacs 21.
;; 2012/02/28 dadams
;;     Don't bother requiring calendar stuff and mkhtml anymore.
;; 2012/02/26 dadams
;;     Removed soft require of Icicles.
;;     Added soft require of pretty-lambdada.
;; 2011/11/30 dadams
;;     Added soft require of descr-text+.el (new library).
;; 2011/09/27 dadams
;;     Use eval-after-load for isearch+.el.
;;     Require color-moccur only for Emacs 22+.  It needs ibuffer-unmark-all.
;; 2011/07/25 dadams
;;     Moved to start-opt.el: Savehist settings, color-moccur settings.
;; 2011/05/10 dadams
;;     Call thgcmd-bind-keys to bind thing-cmds.el default keys.
;; 2010/11/20 dadams
;;     Removed soft require of font-menus.el for Emacs 24+.  It uses var font-lock-defaults-alist.
;;     Removed load-library of my ls-lisp.el.
;; 2010/04/21 dadams
;;     Soft-require wide-n.el.
;; 2010/02/26 dadams
;;     Soft-require iedit.el.  Autoload insert-time-string.el.
;; 2010/02/20 dadams
;;     Soft-require framemove.el.
;; 2010/01/12 dadams
;;     Commented out *scratch* buffer text hack.
;; 2009/11/28 dadams
;;     Typo: '' -> '.
;; 2009/10/25 dadams
;;     Renamings from lib-requires.el: libreq-(requires-(tree|list)|insert-lib-requires-as-comment).
;; 2009/10/11 dadams
;;     Autoload wdired.el and bind to C-x C-q.
;; 2009/08/30 dadams
;;     Removed compile-time require of cl.el for when, unless.
;; 2009/06/17 dadams
;;     Soft-require face-remap+.el for Emacs 23.
;; 2009/06/11 dadams
;;     Soft-require bindings+.el for Emacs 23 also, not just 22.
;; 2009/05-23 dadams
;;     Added boxquote stuff (finally).
;; 2009/04/19 dadams
;;     Moved require of wid-edit+.el and cus-edit+.el forward, so they are loaded before Icicles.
;; 2009/04/18 dadams
;;     Don't load if no graphics display: autofit-frame, oneonone.el, thumb-frm.el, zoom-frm.el.
;; 2009/03/08 dadams
;;     Make require's of compile+.el, grep+.el, and diff-mode- be soft require's.
;; 2009/02/16 dadams
;;     Added soft require of hl-spotlight.el.
;; 2008/12/17 dadams
;;     No soft require of bm.el if Emacs 21.
;; 2008/11/08 dadams
;;     Soft-require swiss-move.el, highlight-context-line.el (commented out).
;; 2008/09/07 dadams
;;     Require find-func+.el after find-func.el is loaded.
;; 2008/09/03 dadams
;;     Wrap soft require of crosshairs.el in condition-case.  Remove soft require of column-marker.el.
;; 2008/08/28 dadams
;;     alacarte.el renamed lacarte.el.
;; 2008/07/18 dadams
;;     Soft-require w32-browser.el if on Windows.
;; 2008/05/23 dadams
;;     Soft-require second-sel.el.
;; 2008/05/20 dadams
;;     Require help+20.el for Emacs 21 also.
;; 2008/03/21 dadams
;;     Soft require finder+.el
;; 2008/03/11 dadams
;;     Soft require tabbar.el.
;; 2008/02/29 dadams
;;     Soft require ls-lisp+, not files+.  Load FJW's ls-lisp before that.
;; 2007/12/27 dadams
;;     Require font-menus.el before facemenu+.el.
;; 2007/12/22 dadams
;;     Soft require wid-edit+.el.
;; 2007/12/14 dadams
;;     Require help+20 for Emacs 20, help+ for Emacs 22.
;; 2007/12/02 dadams
;;     Require help+, help-mode+, and help-fns+ before menu-bar+.
;; 2007/11/02 dadams
;;     Require thumb-frm.el before menu-bar+.el.
;; 2007/10/27 dadams
;;     Soft require help-fns+.el.  Made require of help-mode+.el also be soft.
;; 2007/10/13 dadams
;;     Soft require bm.el.
;; 2007/10/06 dadams
;;     Require doremi* & w32browser-dlgopen before compile+20, because that loads highlight,
;;       which loads menu-bar+.
;; 2007/10/04 dadams
;;     Require fuzzy-match.el.  Require bindings+.el (Emacs 22).
;; 2007/08/14 dadams
;;     Change eq emacs 22 to > emacs 21.
;; 2007/08/11 dadams
;;     Load replace+.el after menu-bar+.el.
;; 2007/07/11 dadams
;;     Use savehist-mode, if defined (for latest version).
;; 2007/06/30 dadams
;;     Soft require of column-marker.el as precondition for require of crosshairs.el.
;; 2007/03/25 dadams
;;     Require font-lock+.el.
;; 2007/03/08 dadams
;;     Autoload linum.el.
;; 2007/02/08 dadams
;;     Soft require of pp-c-l.el.
;; 2007/02/03 dadams
;;     Require savehist-20+.el regardless of Emacs version.  Use savehist-load, to load histories.
;; 2007/01/16 dadams
;;     Soft-require linkd.el.
;; 2006/09/15 dadams
;;     Protected dired-sort-menu+.el and browse-kill-ring+el by requiring parents.
;; 2006/09/14 dadams
;;     Require modeline-posn.el.
;; 2006/09/08 dadams
;;     Require 'crosshairs (Emacs 22), not hl-line+.el and idle-column.el.
;; 2006/09/04 dadams
;;     Require hl-line+.el and idle-column.el (Emacs 22).
;; 2006/07/30 dadams
;;     Replaced autoloads of thingatpt+.el with require of it and thing-cmds.el.
;; 2006/03/03 dadams
;;     Corrected require: apropos-function.el -> apropos-fn+var.el.
;; 2006/01/28 dadams
;;     Require: ediff+.el, dired-details+.el, savehist-20+.el.
;; 2005/12/31 dadams
;;     Require cl.el if load FJW's ls-lisp.el (Emacs 20).
;; 2005/12/26 dadams
;;     Require synonyms.el.
;; 2005/12/20 dadams
;;     Require dired-details+.el.
;; 2005/12/03 dadams
;;     Require apropos-fn+var.el.
;; 2005/08/13 dadams
;;     Require icicles-menu.el.
;; 2005/07/10 dadams
;;     elect-mbuf.el -> icicles.el.
;; 2005/07/07 dadams
;;     Require dired-sort-menu+.el, not dired-sort-menu.el.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2005/05/09 dadams
;;     Renamed setup-frames.el to oneonone.el.
;; 2005/04/21 dadams
;;     Require color-moccur and moccur-edit.
;; 2004/12/30 dadams
;;     Autoload lib-require.el.
;; 2004/12/10 dadams
;;     Require thumb-frm.el.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/11/16 dadams
;;     Renamed old compile+.el to compile+20.el, compile-.el to compile-20.el.
;;     Require for Emacs 21: grep-.el, compile+.el (new version).
;; 2004/10/13 dadams
;;     Require replace+.el and buff-menu+.el for Emacs 21 also.
;; 2004/10/07 dadams
;;     Renamed resize* to fit*.
;;     Added require of tool-bar+.el.
;; 2004/10/03 dadams
;;     Updated for Emacs 21.
;;       Don't load libraries that haven't yet been tested for Emacs 21.
;;       Added hack for Emacs 21: Put something in *scratch* buffer to avoid
;;         getting a header there.
;;       NOTE: Byte-compiled delsel.el is different for Emacs 21 & Emacs 20;
;;             the two versions are incompatible.
;;     No longer require setup-info.el here (info+.el will load it).
;;     Soft requires when possible.
;;     Moved dired-sort-menu.el after dired+.el
;; 2004/09/10 dadams
;;     Replaced dlgopen.el with w32browser-dlgopen.el
;; 2004/06/01 dadams
;;     Renamed shrink-* to resize-*
;; 2001/01/03 dadams
;;     Require vc-hooks+ (vs eval-after-load, because vc-hooks is pre-loaded).
;; 2000/12/07 dadams
;;     1. Added requires of shrink-fit-all.el, font-menus.el.
;;     2. Added autoload of cursors.el.
;; 2000/11/09 dadams
;;     Changed require training-cc to autoload
;; 2000/11/07 dadams
;;     Added requires: local-ps-print, training-cc, printing.
;; 2000/11/01 15:46:04  dadams
;;     Put imenu-add-defs-to-menubar inside condition-case, in c-mode-*-hook.
;; 2000/09/27 dadams
;;     Require bookmark+.el.
;; 1999/10/07 dadams
;;     1. Require cal-opts.el.
;;     2. Require calendar+.el after load cal-french & diary-lib.
;; 1999/09/03 dadams
;;     Require pp+.el.
;; 1999/09/03 dadams
;;     Require dired+.el (not just eval-after-load).
;; 1999/09/02 dadams
;;     Require menu-bar+.el before setup-keys.el.
;; 1999/08/30 dadams
;;     Require imenu+.el.
;; 1999/04/14 dadams
;;     Added require: setup-info.el.
;; 1999/04/09 dadams
;;     Added: require highlight.el.
;; 1999/04/06 dadams
;;     Added: require compile+.
;; 1999/04/02 dadams
;;     Added requires: setup-frames, setup-keys.
;; 1999/04/01 dadams
;;     Added: require 'find-dired+, autoload make-regexp.
;; 1999/03/23 dadams
;;     Require dired+.el.
;; 1999/03/17 dadams
;;     Update to version 19.34.1:
;;     1. Added require: frame-fns, help+.
;;     2. Changed require to eval-after-load: vc+.
;;     3. Added autoload: erase-nonempty-inactive-minibuffer,
;;        update-file-autoloads, unaccent-word, unaccent-region, unaccent-char,
;;        kill-region-wimpy, auto-make-header, make-header.
;; 1996/07/15 dadams
;;     eval-after-load: timer+.el.
;; 1996/07/11 dadams
;;     Require novice+.el after load novice.el.
;; 1996/04/18 dadams
;;     Added use-dired-x-p.  Set up dired-x when use-dired-x-p.
;; 1996/04/15 dadams
;;     Require frame+.el.
;; 1996/04/12 dadams
;;     Require simple+.el.
;; 1996/03/08 dadams
;;     Require menu-bar+.el.
;; 1996/02/14 dadams
;;     Autoload thingatpt+.el for more fns.
;; 1996/02/02 dadams
;;     Require window+.el, replace+.el.
;; 1995/12/15 dadams
;;     Require dired+.el.
;; 1995/12/05 dadams
;;     Require cl.el.
;; 1995/10/17 dadams
;;     Require icomplete+.el.
;; 1995/09/13 dadams
;;     Load info+.el.
;; 1995/09/11 dadams
;;     Require buff-menu+.el.
;; 1995/09/11 dadams
;;     Require vc+.el
;; 1995/09/04 dadams
;;     Require iso-transl.el.
;; 1995/08/18 dadams
;;     Added imenu-add-defs-to-menubar, and put it on c-mode hook.
;; 1995/08/11 dadams
;;     Require files+.el.
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

;;;;;;;;;;;;;;;;;;;;;;

(provide 'start)
(require 'start) ;; Ensure this file is loaded before compile it.

;;;;;;;;;;;;;;;;;;;;;;


;; ;;; Hack needed if you load `start-opt.el', because it does this:
;; ;;;    (add-hook 'lisp-mode-hook             'auto-make-header)
;; ;;; Without this hack, you get a lisp header from header2.el
;; ;;; in the *scratch* buffer!
;; (when (>= emacs-major-version 21)
;;   (save-excursion
;;     (set-buffer (get-buffer-create "*scratch*"))
;;     (insert "Evaluate Emacs Lisp expressions here. `C-h m' for details\n\n\n\n\n")))

(when (> emacs-major-version 21) (require 'bindings+ nil t)) ; Minor-mode menus in mode line.
(require 'misc-fns nil t)               ; Miscellaneous non-interactive functions
(require 'simple+ nil t)                ; Corrections, extensions.
(autoload 'libreq-requires-tree "lib-requires" ; Track Emacs-Lisp library dependencies.
  "The libraries `require'd by LIBRARY, as a tree." t)
(autoload 'libreq-requires-list "lib-requires"
  "The libraries ultimately `require'd by LIBRARY, as a flat list." t)
(autoload 'libreq-insert-lib-requires-as-comment "lib-requires"
  "Insert a comment listing all libraries ultimately required by LIBRARY." t)
(require 'frame-cmds nil t)             ; Frame and window commands.
(when (fboundp 'window-inside-pixel-edges) (require 'framemove nil t)) ; Move frame focus.
(when (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
  (require 'autofit-frame nil t))       ; Automatically fit frames to sole window.
(unless (> emacs-major-version 22)
  (autoload 'wdired-change-to-wdired-mode "wdired") ; Easily rename files etc.
  (add-hook 'dired-load-hook (lambda ()
                               (define-key dired-mode-map "\C-x\C-q" 'wdired-change-to-wdired-mode))))
(when (fboundp 'text-scale-increase)    ; Emacs 23+
  (require 'face-remap+ nil t))         ; Resize window/frame when scale text.
(when (> emacs-major-version 22)        ; Need Emacs 23+ version of `ucs-insert' (`C-x 8 RET').
  (require 'ucs-cmds nil t))            ; Enhanced `C-x 8 RET'.
(require 'zones nil t)                  ; Ring/stack of buffer zones (including narrowings).
;;; ;; Use my update to Francis Wright's version of `ls-lisp.el', if available.
;;; (when (and (< emacs-major-version 21) (memq system-type '(windows-nt ms-dos macos)))
;;;   ;; If you don't have my version, from http://www.emacswiki.org/emacs/ls-lisp.el, and you use
;;;   ;; Emacs 20, then you will need to do this, because FJW's `ls-lisp.el' uses `mapc':
;;;   ;;(require 'cl)
;;;   (load-library "ls-lisp")) ; Don't use `require', so get FJW's version, not vanilla version.
(unless (and (memq system-type '(windows-nt ms-dos macos)) ; Redefinitions.
             (require 'ls-lisp+ nil t)) ; This loads `files+.el'.
  (require 'files+ nil t))
(require 'wid-edit+ nil t)              ; Extensions to `wid-edit.el'.
(require 'cus-edit+ nil t)              ; Extensions to `cus-edit.el'.

(when (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
  (require 'oneonone nil t))            ; Default frame configuration.
(when (boundp 'mode-line-position)      ; Emacs 22+ - Mode line position highlighting.
  (require 'modeline-posn nil t))
(require 'window+ nil t)                ; Corrections.
(unless (featurep 'window+) (require 'frame+ nil t)) ; Corrections.
;; (require 'apropos)                      ; My version.
(require 'paren)                        ; Highlight matching parens (standard GNU).
(when window-system
  (autoload 'paren-activate "mic-paren" "" t) ; Turns on alternative paren highlighting.
  (autoload 'paren-deactivate "mic-paren" "" t) ; Turns it off.
  (autoload 'paren-toggle-matching-paired-delimiter "mic-paren" "" t) ; For LaTeX etc: $...$
  (autoload 'paren-toggle-matching-quoted-paren "mic-paren" "" t) ; Toggle highlighting escaped parens.
  (autoload 'paren-toggle-open-paren-context "mic-paren" "" t)
  (defvar paren-sexp-mode t)
  (defvar paren-message-linefeed-display "^J")) ; Toggle in/out context for open paren.
(require 'finder+ nil t)                ; Extensions to `finder.el'.
(require 'iso-transl nil t)             ; Keyboard input definitions for ISO 8859/1.
(require 'eshell-auto nil t)            ; Eshell
(require 'em-joc nil t)                 ; Extensions to eshell
(require 'pcmpl-auto nil t)             ; Pcomplete

;; `ls-lisp-verbosity.el' is essentially `leo-toggle-ls-lisp-verbosity'
;;     from (http://www.emacswiki.org/LsLispToggleVerbosity)
(require 'ls-lisp-verbosity nil t)

;; Dired-X:
(defvar use-dired-x-p t "*Non-nil means that `dired-x' package is to be loaded.
See the Dired-X Info pages (type \\[info]) for information on this package.")
(require 'dired-x nil t)
(when use-dired-x-p
  (add-hook 'dired-load-hook            ; To be done before dired is loaded.
            (function (lambda ()
                        ;; Bind `dired-x-find-file' in place of `find-file'.
                        (setq dired-x-hands-off-my-keys nil)
                        (load "dired-x")
                        ;; Set other dired-x variables here.  For example:
                        ;; (setq dired-guess-shell-gnutar "gtar")
                        (setq dired-omit-files
                              (concat dired-omit-files "\\|^RCS$\\|^SCCS$"))
                        (setq dired-x-hands-off-my-keys nil))))
;;;  (add-hook 'dired-mode-hook
;;;            (function (lambda ()
;;;                        ;; Set dired-x buffer-local variables here.  E.g.:
;;;                        ;; (setq dired-omit-files-p t))))
  )

(require 'frame-fns nil t)              ; Non-interactive frame and window functions.
(require 'buff-menu+ nil t)             ; My replacements.
(require 'misc-cmds nil t)              ; Miscellaneous commands.
(require 'second-sel nil t)             ; Secondary selection commands.
(when (< emacs-major-version 21)
  (require 'vc-hooks+ nil t)            ; VC fixes and options.
  (eval-after-load "vc" '(require 'vc+ nil t))) ; VC fixes and options.

;; Load `doremi', `w32browser-dlgopen.el', `thumb-frm', `help+20', `help+', and `help-fns+' before
;; `menu-bar+', which references them.  Must load them before `compile+20', which requires
;; `highlight', which requires `menu-bar+'.
(autoload 'define-doremi "doremi-mac" "Define a Do Re Mi command." nil 'macro)
(require 'doremi-frm nil t)             ; Dynamic adjustment of frame properties.  Loads `doremi.el'.
(require 'doremi-cmd nil t)             ; Other Do Re Mi commands.
(when (eq system-type 'windows-nt)
  (require 'w32browser-dlgopen nil t))  ; MS Windows Open file dialog box.
(when (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
  (require 'thumb-frm nil t))           ; Fisheye view and thumbnail frames.
(when (< emacs-major-version 22)
  (require 'help+20 nil t))             ; Extensions to `help.el'.
(when (> emacs-major-version 21)
  (require 'help+ nil t)                ; Extensions to `help.el'.
  (require 'help-mode+ nil t)           ; Extensions to `help-mode.el'.
  (require 'help-fns+ nil t)            ; Extensions to `help-fns.el'.
  (require 'descr-text+ nil t))         ; Extensions to `descr-text.el'.
(if (< emacs-major-version 22)
    (require 'compile+20 nil t)         ; Highlighting, etc.
  (require 'compile+ nil t)             ; Highlighting.
  (require 'grep+ nil t))               ; Highlighting.
(require 'highlight nil t)              ; Highlighting commands. Also loads `menu-bar+.el'.

(require 'menu-bar+ nil t)              ; Extensions to `menu-bar.el'.  Also loads `info+.el'.

;; Load `bookmark+.el' after `menu-bar+.el.
(require 'bookmark+ nil t)              ; Extensions to `bookmark.el'.
;; (when (> emacs-major-version 21) (require 'bm nil t)) ; Visible bookmarks.
(when (> emacs-major-version 21) (require 'tabbar nil t)) ; Tab bar.

(eval-after-load "pp" '(require 'pp+ nil t)) ; Extensions to `pp.el'.
(eval-after-load "isearch" '(require 'isearch+ nil t)) ; Extensions to `isearch.el'.
;; $$$$$$(require 'isearch+ nil t)    ; Extensions to `isearch.el'.
(require 'occur-schroeder nil t)        ; Occur alternative & isearch option.

(load-library "delsel")                 ; Load my version, which works with `completion.el' etc.

(require 'thingatpt)                    ; Basic thing-at-point.
(when (and (require 'thingatpt+ nil t)  ; Thing-at-point extensions - load this after `thingatpt.el'.
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))
(when (require 'thing-cmds nil t)       ; Thing-at-point commands and default key bindings.
  (thgcmd-bind-keys))

(require 'subr+ nil t)                  ; Extensions to `subr.el'.  String splitting.
(when (fboundp 'clone-indirect-buffer) (require 'narrow-indirect nil t)) ; Emacs 22+

(require 'imenu+ nil t)                 ; Extensions to `imenu.el'.
(autoload 'imenu-create-hierarchical-index "hier-imenu" ; Hierarchical imenu for HTML, Tex
  "Generate an alist for imenu from a buffer with hierarchical structure.")
(add-hook 'tex-mode-hook 'imenu-add-menubar-index)
(autoload 'hier-imenu-dtd-setup "hier-imenu"
  "Scan buffer for a DTD and set `hier-imenu' parameters accordingly.")
(add-hook 'sgml-mode-hook 'hier-imenu-dtd-setup)

;; Moccur - like occur, but for multiple buffers.
(setq moccur-use-ee nil)
(setq moccur-split-word t)
;;(setq *moccur-buffer-name-exclusion-list* '(".+TAGS.+" "*Completions*" "*Messages*"))
(setq dmoccur-use-list t)
(setq dmoccur-list '(("dir" default-directory (".*") dir)))

;; Unfortunately, this requires `cl.el' at runtime.  Comment this out if you do not want that.
(when (> emacs-major-version 21) (require 'color-moccur nil t)) ; Needs `ibuffer-unmark-all'.
(require 'moccur-edit nil t)            ; Edit files by editing the Moccur buffer.

(when (fboundp 'define-minor-mode) (require 'linkd nil t)) ; Hyperlinks.

(autoload 'ispell-word "ispell" "Check spelling of word at or before point" t)
(autoload 'ispell-complete-word "ispell" "Complete word at or before point" t)
(autoload 'ispell-region "ispell" "Check spelling of every word in the region" t)
(autoload 'ispell-buffer "ispell" "Check spelling of every word in the buffer" t)

(autoload 'make-regexp "make-regexp" "Return a regexp to match a string item in STRINGS.")
(autoload 'make-regexps "make-regexp" "Return a regexp to REGEXPS.")

(autoload 'apropos-user-options "apropos+" "Show user options that match REGEXP." t)
(autoload 'erase-nonempty-inactive-minibuffer "strings"
  "Erase the minibuffer, if inactive and `minibuffer-empty-p'." t)
(autoload 'update-file-autoloads "autoload+"
  "Update the autoloads for FILE in `generated-autoload-file'" t)

(autoload 'unaccent-word "unaccent"
  "Move curseur forward NUM (prefix arg) words, removing accents." t)
(autoload 'unaccent-region "unaccent"
  "Replace accented chars between START and END by unaccented chars." t)
(autoload 'unaccent-char "unaccent"
  "Replace accented char at curser by corresponding unaccented char(s)." t)

(autoload 'kill-region-wimpy "wimpy-del" "Kill region, with confirmation if > `wimpy-delete-size'." t)
(autoload 'clipboard-kill-region-wimpy "wimpy-del"
  "Kill region, with confirmation if > `wimpy-delete-size'." t)

(autoload 'auto-update-file-header "header2" "Update file header if file is modified.")
(autoload 'auto-make-header "header2"
  "Call `make-header' if current buffer is empty and is a file buffer.")
(autoload 'update-file-header "header2" "Update file header." t)
(autoload 'make-header "header2" "Insert (mode-dependent) header comment at beginning of file." t)
(autoload 'make-revision "header2" "Prepare for a new history comment." t)

(autoload 'boxquote-title  "boxquote" "Set the title of the current boxquote." t)
(autoload 'boxquote-region "boxquote" "Draw a box around the region." t)
(autoload 'boxquote-unbox-region "boxquote" "Remove a boxquote created by `boxquote-region'." t)
(autoload 'boxquote-unbox "boxquote" "Remove the boxquote that contains `point'." t)
(autoload 'boxquote-buffer "boxquote" "Apply `boxquote-region' to a buffer." t)
(autoload 'boxquote-insert-file "boxquote" "Insert a file, box-quoted." t)
(autoload 'boxquote-insert-buffer "boxquote" "Insert contents of a buffer, box-quoted." t)
(autoload 'boxquote-kill-ring-save "boxquote" "Like `kill-ring-save' but remembers a title." t)
(autoload 'boxquote-yank "boxquote" "`yank', but box-quote the yanked text." t)
(autoload 'boxquote-defun "boxquote" "Box-quote the current defun." t)
(autoload 'boxquote-paragraph "boxquote" "Box-quote the current paragraph." t)
(autoload 'boxquote-boxquote "boxquote" "Box-quote the current boxquote." t)
(autoload 'boxquote-describe-function "boxquote" "Box-quote the output of `describe-function'." t)
(autoload 'boxquote-describe-variable "boxquote" "Box-quote the output of `describe-variable'." t)
(autoload 'boxquote-describe-key "boxquote" "Box-quote the output of `describe-key'." t)
(autoload 'boxquote-where-is "boxquote" "Box-quote the output of `where-is'." t)
(autoload 'boxquote-text "boxquote" "Insert the given TEXT, box-quoted." t)
(autoload 'boxquote-kill "boxquote" "Kill the boxquote and its contents." t)
(autoload 'boxquote-fill-paragraph "boxquote" "`fill-paragraph' inside a boxquote." t)
(autoload 'boxquote-narrow-to-boxquote-content "boxquote" "Narrow to the current boxquote." t)
(autoload 'boxquote-shell-command "boxquote" "Box-quote the output of `shell-command'." t)

(when (> emacs-major-version 21)
  (condition-case nil                   ; Highlight line and col when idle.
      (require 'crosshairs nil t)       ; Hard-requires `hl-line+.el', `col-highlight.el', `vline.el'.
    (error nil)))
(when (> emacs-major-version 21)
  (require 'hl-spotlight nil t))        ; Extends `hl-line.el' by spotlighting lines.
(require 'pp-c-l nil t)                 ; Pretty display of `^L' characters.
(when (> emacs-major-version 21)        ; Lambda symbol (char) replaces `lambda' text.
  (require 'pretty-lambdada nil t)
  (when (fboundp 'pretty-lambda-for-modes) (pretty-lambda-for-modes)))
(when (> emacs-major-version 21) (require 'echo-bell nil t)) ; Visual bell in echo area.

(require 'setup nil t)                  ; Startup assignments and such.

(require 'local-lpr nil t)              ; Local settings for `lpr.el'.
(require 'local-ps-print nil t)         ; Local settings for PostScript printing.
(require 'printing nil t)               ; Printing from menu bar etc.

;; `font(-lock)-menus': Additional font menus.  Load before `facemenu+'.
(require 'font-lock-menus nil t)        ; My version - works also for Emacs 24.
(when (and (not (featurep 'font-lock-menus))
           (< emacs-major-version 24))  ; Uses `font-lock-defaults-alist', removed from Emacs 24.
  (require 'font-menus nil t))
(require 'facemenu+ nil t)              ; New Text Properties menu items.
(when (> emacs-major-version 21)
  (require 'font-lock+ nil t))             ; Enhancements to `font-lock.el'.
(when (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
  (require 'zoom-frm nil t))            ; Zoom frame, changing its font size.
(eval-after-load "info"
  '(if (> emacs-major-version 22)
    (require 'info+ nil t)
    (require 'info+20 nil t)))          ; Extensions to `info.el'.
(require 'fuzzy-match nil t)
(require 'swiss-move nil t)             ; Binary-search scrolling.
;; Load `menu-bar+.el' before `setup-keys.el'.  Load `setup-keys.el' before `replace+.el'.
(require 'setup-keys nil t)             ; Key bindings.
(require 'replace+ nil t)               ; Redefinitions, corrections.
(require 'apropos-fn+var nil t)         ; Extensions to `apropos.el'.

;; Removed soft require of Icicles.  Load it separately, preferably after you load your `custom-file'.
;; (require 'icicles nil t)                ; Minibuffer completion, completion-candidate cycling.

(require 'lacarte nil t)                ; Menu-bar menu-command completion and execution via keyboard.
(require 'synonyms nil t)               ; Thesaurus.
(when (> emacs-major-version 20) (require 'tool-bar+ nil t)) ; Extensions to `tool-bar.el'.
(require 'icomplete+ nil t)             ; Sorted, colored icompletion
;;; (when (< emacs-major-version 21)
;;;   (require 'mkhtml nil t)               ; Create HTML from Emacs buffers/files.
;;;   (require 'cal-opts nil t)             ; Calendar and diary options.
;;;   (eval-after-load "calendar" '(require 'calendar+ nil t)) ; Calendar, diary, appointment stuff.
;;;   (eval-after-load "cal-french" '(require 'calendar+ nil t))
;;;   (eval-after-load "diary-lib" '(require 'calendar+ nil t)))
(cond ((> emacs-major-version 21)
       (eval-after-load "diff" '(require 'diff+ nil t)) ; Extensions to `diff.el'.
       (require 'diff-mode- nil t))     ; Extensions to `diff-mode.el'.
      ((= emacs-major-version 21)
       (require 'diff-mode- nil t)      ; Extensions to `diff-mode.el'.
       (require 'diff-mode))            ; Doesn't get loaded by `diff', apparently.
      (t
       (eval-after-load "diff" '(require 'diff+20 nil t)))) ; Extensions to `diff.el'.
(require 'ediff+ nil t)                 ; Extensions to Ediff - toggle case sensitivity.
;; Note: `find-dired+.el' loads `dired+.el'.
(require 'find-dired+ nil t)            ; Improvements.
(require 'dired+ nil t)                 ; Extensions to Dired.
(when (eq system-type 'windows-nt) (require 'w32-browser nil t)) ; Even if `dired+.el' not loaded.
(when (require 'dired-sort-menu nil t)
  (require 'dired-sort-menu+ nil t))    ; Menu/dialogue for dired sort options
(unless (fboundp 'dired-hide-details-mode)
  (require 'dired-details+ nil t))      ; Enhancements to `dired-details.el'.
(require 'savehist-20+ nil t)           ; Save your history lists.
(eval-after-load "faces" '(require 'faces+ nil t)) ; Extensions to `faces.el'.
(when (> emacs-major-version 22)
  (autoload 'menu-bar-read-lispref "info+" "Access the Emacs Lisp manual via `Info'." t)
  (autoload 'info-emacs-manual "info+" "Access the Emacs manual via \"Info\"." t))
(when (< emacs-major-version 23)
  (autoload 'menu-bar-read-lispref "info+20" "Access the Emacs Lisp manual via `Info'." t)
  (autoload 'info-emacs-manual "info+20" "Access the Emacs manual via \"Info\"." t))
(eval-after-load "macros" '(require 'macros+ nil t)) ; Extensions to `macros.el'.
(require 'mouse+ nil t)                       ; Extensions to `mouse.el'.
(when (> emacs-major-version 21) (require 'iedit nil t))
(eval-after-load "novice" '(require 'novice+ nil t)) ; Extensions to `novice.el'.
(when (< emacs-major-version 21)
  (eval-after-load "options" '(require 'options+ nil t))) ; Extensions to `options.el'.
;(eval-after-load "outline"
;  '(progn (require 'outline+)(require 'foldout))) ; Extensions to `outline.el'.
(eval-after-load "ring" '(require 'ring+ nil t)) ; Extensions to `ring.el'.
(unless (> emacs-major-version 19)
  (eval-after-load "sort" '(require 'sort+))) ; Extensions to `sort.el'.
(when (< emacs-major-version 21)
  (eval-after-load "timer" '(require 'timer+ nil t))) ; Extensions to `timer.el'.
;(eval-after-load "c-mode" '(require 'cc-mode+)) ; Extensions to `cc-mode.el'.
;(eval-after-load "cc-mode" '(require 'cc-mode+))
(when (< emacs-major-version 21)
  (eval-after-load "cc-mode"
    '(progn (require 'imenu+ nil t)
      (add-hook 'c-mode-common-hook
       (lambda () (condition-case nil (imenup-add-defs-to-menubar) (error nil)))))))
(when (> emacs-major-version 21) (eval-after-load "find-func" '(require 'find-func+ nil t)))
;(eval-after-load "vc" '(require 'vc+)) ; Extensions to `vc.el'.
(autoload 'insert-time-string "insert-time-string" "Insert current time at point." t)
(autoload 'display-line-numbers "line-num"
  "Temporarily display line numbers in left margin of current buffer." t)
(autoload 'setup-training-cc "training-cc" "Set up for code display with projector." t)

(when (> emacs-major-version 21)
  (autoload 'register-list "register-list" "Display a list of registers." t))

(require 'highlight-chars nil t)
;;; (autoload 'toggle-highlight-hard-hyphens "highlight-chars"
;;;   "Toggle highlighting of hard hyphen characters." t)
;;; (autoload 'hc-toggle-highlight-hard-hyphens "highlight-chars"
;;;   "Toggle highlighting of hard hyphen characters." t)
;;; (autoload 'toggle-highlight-hard-spaces "highlight-chars"
;;;   "Toggle highlighting of non-breaking space characters." t)
;;; (autoload 'hc-toggle-highlight-hard-spaces "highlight-chars"
;;;   "Toggle highlighting of non-breaking space characters." t)
;;; (autoload 'toggle-highlight-other-chars "highlight-chars"
;;;   "Toggle highlighting of chars in `hc-other-chars'." t)
;;; (autoload 'hc-toggle-highlight-other-chars "highlight-chars"
;;;   "Toggle highlighting of chars in `hc-other-chars'." t)
;;; (autoload 'toggle-highlight-tabs    "highlight-chars" "Toggle highlighting of TAB characters." t)
;;; (autoload 'hc-toggle-highlight-tabs "highlight-chars" "Toggle highlighting of TAB characters." t)
;;; (autoload 'toggle-highlight-trailing-whitespace "highlight-chars"
;;;   "Toggle highlighting of trailing whitespace." t)
;;; (autoload 'hc-toggle-highlight-trailing-whitespace "highlight-chars"
;;;   "Toggle highlighting of trailing whitespace." t)

(when (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 2)))
  (require 'flx nil t));; Emacs 24.3+ - requires `cl-lib.el' for `cl-loop', `cl-incf', `cl-cddar'.

(autoload 'joc-cursor-type-set-hook "cursors" "Make cursor reflect insert/overwrite mode." t)

(autoload 'ascii-on        "ascii" "Turn on ASCII code display."   t)
(autoload 'ascii-off       "ascii" "Turn off ASCII code display."  t)
(autoload 'ascii-display   "ascii" "Toggle ASCII code display."    t)
(autoload 'ascii-customize "ascii" "Customize ASCII code display." t)
(eval-after-load "ascii" '(progn (copy-face 'region 'ascii-ascii-face)
                                 (copy-face 'secondary-selection 'ascii-non-ascii-face)))

(autoload 'blank-mode-on "blank-mode" "Turn on blank visualization."   t)
(autoload 'blank-mode-off "blank-mode" "Turn off blank visualization."  t)
(autoload 'blank-mode "blank-mode" "Toggle blank visualization."    t)
(autoload 'blank-mode-customize "blank-mode" "Customize blank visualization." t)

(autoload 'setnu-mode "setnu+" "Toggle setnu-mode on/off." t)
(autoload 'linum-mode "linum" "Toggle display of line numbers." t)
(when (require 'browse-kill-ring nil t) (require 'browse-kill-ring+ nil t))

(when (> emacs-major-version 20) (autoload 'column-marker-1 "column-marker" "Highlight a column." t))

(when (fboundp 'define-minor-mode) (require 'highlight-symbol nil t))

;; (require 'highlight-context-line nil t)

;;; Define some aliases so can find toggle commands easily.
(defalias 'toggle-abbrev-mode 'abbrev-mode)
(defalias 'toggle-auto-fill-mode 'auto-fill-mode)
(defalias 'toggle-auto-lower-mode 'auto-lower-mode)
(defalias 'toggle-auto-raise-mode 'auto-raise-mode)
(defalias 'toggle-auto-save-mode 'auto-save-mode)
(defalias 'toggle-binary-overwrite-mode 'binary-overwrite-mode)
(defalias 'toggle-compilation-minor-mode 'compilation-minor-mode) ; (defsubst)
(defalias 'toggle-delete-selection-mode 'delete-selection-mode) ; `delsel.el'.
(defalias 'toggle-double-mode 'double-mode)
(defalias 'toggle-enable-flow-control 'enable-flow-control)
(defalias 'toggle-font-lock-mode 'font-lock-mode)
(defalias 'toggle-hide-ifdef-mode 'hide-ifdef-mode)
(defalias 'toggle-iso-accents-mode 'iso-accents-mode)
(defalias 'toggle-menu-bar-mode 'menu-bar-mode)
(defalias 'toggle-overwrite-mode 'overwrite-mode)
(defalias 'toggle-scroll-bar-mode 'scroll-bar-mode)
(defalias 'toggle-standard-display-european 'standard-display-european)
(defalias 'toggle-transient-mark-mode 'transient-mark-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; start.el ends here
