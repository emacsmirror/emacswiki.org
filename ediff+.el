;;; ediff+.el --- Enhancements to Ediff
;;
;; Filename: ediff+.el
;; Description: Enhancements to Ediff
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005-2012, Drew Adams, all rights reserved.
;; Created: Thu Jan 26 11:14:34 2006
;; Version: 20
;; Last-Updated: Sun Jan  1 14:25:11 2012 (-0800)
;;           By: dradams
;;     Update #: 118
;; URL: http://www.emacswiki.org/cgi-bin/wiki/ediff+.el
;; Keywords: comparing, merging, patching, version control
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `ediff-diff', `ediff-help', `ediff-init', `ediff-merg',
;;   `ediff-mult', `ediff-util', `ediff-wind'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Ediff enhancements added by this library: Toggle case-sensitivity.
;;
;;  This feature is now available in GNU Emacs 22, so this library is
;;  useful mainly for older versions of Emacs.
;;
;;  Commands defined here:
;;
;;    `ediff-toggle-ignore-case'.
;;
;;  Internal variables defined here:
;;
;;    `ediff-ignore-case'.
;;
;;
;;  ***** NOTE: The following functions defined in `ediff-util.el' have
;;              been REDEFINED HERE:
;;
;;  `ediff-setup-keymap' - Include `#c'.
;;
;;
;;  ***** NOTE: The following functions defined in `ediff-help.el' have
;;              been REDEFINED HERE:
;;
;;  `ediff-help-for-quick-help' (Emacs 20, 21 only).
;;
;;
;;  ***** NOTE: The following constants defined in `ediff-help.el' have
;;              been REDEFINED HERE:
;;
;;  `ediff-long-help-message-compare2',
;;  `ediff-long-help-message-compare3',
;;  `ediff-long-help-message-merge',
;;  `ediff-long-help-message-narrow2',
;;  `ediff-long-help-message-word-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/12/11 dadams
;;     ediff-help-for-quick-help: Define it only for Emacs < 22.  Distinguish $ cases.
;; 2011/01/04 dadams
;;     Added autoload cookies for commands.
;; 2008/04/24 dadams
;;     ediff-setup-keymap: Updated for Emacs 23.
;; 2006/03/02 dadams
;;     Mention that this feature is now in Emacs 22 (as of late February).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'ediff-wind) ;; ediff-defvar-local
(require 'ediff-util) ;; ediff-setup-keymap
(require 'ediff-help) ;; ediff-help-for-quick-help, ediff-long-help-message-compare2,
                      ;; ediff-long-help-message-compare3, ediff-long-help-message-merge,
                      ;; ediff-long-help-message-narrow2, ediff-long-help-message-word-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; $$$ Plus need to update `ediff-help.el' accordingly.


(ediff-defvar-local ediff-ignore-case nil
  "*If t, skip over difference regions that differ only in letter case.
This variable can be set either in .emacs or toggled interactively.
Use `setq-default' if setting it in .emacs")


;;;###autoload
(defun ediff-toggle-ignore-case ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (unless (string= "diff" ediff-diff-program)
    (error "Cannot toggle ignoring case unless `ediff-diff-program' is \"diff\"."))
  (setq ediff-ignore-case (not ediff-ignore-case))
  (cond (ediff-ignore-case
         (setq ediff-diff-options (concat " -i " ediff-diff-options))
         (ediff-update-diffs)
         (message "Skipping regions that differ only in case"))
        (t
         (when (string-match " -i " ediff-diff-options)
           (setq ediff-diff-options
                 (concat (substring ediff-diff-options 0 (match-beginning 0))
                         (substring ediff-diff-options
                                    (match-end 0) (length ediff-diff-options)))))
         (ediff-update-diffs)
         (message "Skipping over case differences turned OFF"))))



;; REPLACE ORIGINAL in `ediff-util.el'
;; Works for older Emacs versions also.
;;
(defun ediff-setup-keymap ()
  "Set up the keymap used in the control buffer of Ediff."
  (setq ediff-mode-map (make-sparse-keymap))
  (suppress-keymap ediff-mode-map)
  (define-key ediff-mode-map
    (if (or (and (boundp 'ediff-emacs-p) ediff-emacs-p) ; Emacs <23
            (featurep 'emacs))          ; Emacs 23
        [mouse-2] [button2]) 'ediff-help-for-quick-help)
  (define-key ediff-mode-map "\C-m"  'ediff-help-for-quick-help)

  (define-key ediff-mode-map "p" 'ediff-previous-difference)
  (define-key ediff-mode-map "\C-?" 'ediff-previous-difference)
  (define-key ediff-mode-map [delete] 'ediff-previous-difference)
  (define-key ediff-mode-map "\C-h" (if ediff-no-emacs-help-in-control-buffer
                                        'ediff-previous-difference nil))
  ;; must come after C-h, or else C-h wipes out backspace's binding in XEmacs
  (define-key ediff-mode-map [backspace] 'ediff-previous-difference)
  (define-key ediff-mode-map "n" 'ediff-next-difference)
  (define-key ediff-mode-map " " 'ediff-next-difference)
  (define-key ediff-mode-map "j" 'ediff-jump-to-difference)
  (define-key ediff-mode-map "g"  nil)
  (define-key ediff-mode-map "ga" 'ediff-jump-to-difference-at-point)
  (define-key ediff-mode-map "gb" 'ediff-jump-to-difference-at-point)
  (define-key ediff-mode-map "q" 'ediff-quit)
  (define-key ediff-mode-map "D" 'ediff-show-diff-output)
  (define-key ediff-mode-map "z" 'ediff-suspend)
  (define-key ediff-mode-map "\C-l" 'ediff-recenter)
  (define-key ediff-mode-map "|" 'ediff-toggle-split)
  (define-key ediff-mode-map "h" 'ediff-toggle-hilit)
  (or ediff-word-mode
      (define-key ediff-mode-map "@" 'ediff-toggle-autorefine))
  (if ediff-narrow-job
      (define-key ediff-mode-map "%" 'ediff-toggle-narrow-region))
  (define-key ediff-mode-map "~" 'ediff-swap-buffers)
  (define-key ediff-mode-map "v" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "\C-v" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "^" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "\M-v" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "V" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "<" 'ediff-scroll-horizontally)
  (define-key ediff-mode-map ">" 'ediff-scroll-horizontally)
  (define-key ediff-mode-map "i" 'ediff-status-info)
  (define-key ediff-mode-map "E" 'ediff-documentation)
  (define-key ediff-mode-map "?" 'ediff-toggle-help)
  (define-key ediff-mode-map "!" 'ediff-update-diffs)
  (define-key ediff-mode-map "M" 'ediff-show-current-session-meta-buffer)
  (define-key ediff-mode-map "R" 'ediff-show-registry)
  (or ediff-word-mode
      (define-key ediff-mode-map "*" 'ediff-make-or-kill-fine-diffs))
  (define-key ediff-mode-map "a"  nil)
  (define-key ediff-mode-map "b"  nil)
  (define-key ediff-mode-map "r"  nil)
  (cond (ediff-merge-job
	 ;; Will barf if no ancestor
	 (define-key ediff-mode-map "/" 'ediff-show-ancestor)
	 ;; In merging, we allow only A->C and B->C copying.
	 (define-key ediff-mode-map "a" 'ediff-copy-A-to-C)
	 (define-key ediff-mode-map "b" 'ediff-copy-B-to-C)
	 (define-key ediff-mode-map "r" 'ediff-restore-diff-in-merge-buffer)
	 (define-key ediff-mode-map "s" 'ediff-shrink-window-C)
	 (define-key ediff-mode-map "+" 'ediff-combine-diffs)
	 (define-key ediff-mode-map "$"  nil)
	 (define-key ediff-mode-map "$$" 'ediff-toggle-show-clashes-only)
	 (define-key ediff-mode-map "$*" 'ediff-toggle-skip-changed-regions)
	 (define-key ediff-mode-map "&" 'ediff-re-merge))
	(ediff-3way-comparison-job
	 (define-key ediff-mode-map "ab" 'ediff-copy-A-to-B)
	 (define-key ediff-mode-map "ba" 'ediff-copy-B-to-A)
	 (define-key ediff-mode-map "ac" 'ediff-copy-A-to-C)
	 (define-key ediff-mode-map "bc" 'ediff-copy-B-to-C)
	 (define-key ediff-mode-map "c" nil)
	 (define-key ediff-mode-map "ca" 'ediff-copy-C-to-A)
	 (define-key ediff-mode-map "cb" 'ediff-copy-C-to-B)
	 (define-key ediff-mode-map "ra" 'ediff-restore-diff)
	 (define-key ediff-mode-map "rb" 'ediff-restore-diff)
	 (define-key ediff-mode-map "rc" 'ediff-restore-diff)
	 (define-key ediff-mode-map "C"  'ediff-toggle-read-only))
	(t ; 2-way comparison
	 (define-key ediff-mode-map "a"  'ediff-copy-A-to-B)
	 (define-key ediff-mode-map "b"  'ediff-copy-B-to-A)
	 (define-key ediff-mode-map "ra" 'ediff-restore-diff)
	 (define-key ediff-mode-map "rb" 'ediff-restore-diff))
	) ; cond
  (define-key ediff-mode-map "G" 'ediff-submit-report)
  (define-key ediff-mode-map "#"  nil)
  (define-key ediff-mode-map "#h"  'ediff-toggle-regexp-match)
  (define-key ediff-mode-map "#f"  'ediff-toggle-regexp-match)
  (define-key ediff-mode-map "#c"  'ediff-toggle-ignore-case)
  (or ediff-word-mode
      (define-key ediff-mode-map "##"  'ediff-toggle-skip-similar))
  (define-key ediff-mode-map "o"   nil)
  (define-key ediff-mode-map "A"  'ediff-toggle-read-only)
  (define-key ediff-mode-map "B"  'ediff-toggle-read-only)
  (define-key ediff-mode-map "w"   nil)
  (define-key ediff-mode-map "wa"  'ediff-save-buffer)
  (define-key ediff-mode-map "wb"  'ediff-save-buffer)
  (define-key ediff-mode-map "wd"  'ediff-save-buffer)
  (define-key ediff-mode-map "="   'ediff-inferior-compare-regions)
  (if (and (fboundp 'ediff-show-patch-diagnostics)
           (or (not (fboundp 'ediff-patch-job)) (ediff-patch-job)))
      (define-key ediff-mode-map "P"  'ediff-show-patch-diagnostics))
  (if ediff-3way-job
      (progn
        (define-key ediff-mode-map "wc" 'ediff-save-buffer)
        (define-key ediff-mode-map "gc" 'ediff-jump-to-difference-at-point)
        ))

  (define-key ediff-mode-map "m" 'ediff-toggle-wide-display)

  ;; Allow ediff-mode-map to be referenced indirectly
  (fset 'ediff-mode-map ediff-mode-map)
  (run-hooks 'ediff-keymap-setup-hook))


;; REPLACE ORIGINAL in `ediff-help.el'
;; Include `#c'.
;;
(defconst ediff-long-help-message-compare2
  "
p,DEL -previous diff |     | -vert/horiz split   |a/b -copy A/B's region to B/A
n,SPC -next diff     |     h -hilighting         | rx -restore buf X's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions
  C-l -recenter      |    #c -ignore case        |
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
    ~ -swap variants |     m -wide display       |
"
  "Help message usually used for 2-way comparison.
Normally, not a user option. See `ediff-help-message' for details.")


;; REPLACE ORIGINAL in `ediff-help.el'
;; Include `#c'.
;;
(defconst ediff-long-help-message-compare3
  "
p,DEL -previous diff |     | -vert/horiz split   | xy -copy buf X's region to Y
n,SPC -next diff     |     h -hilighting         | rx -restore buf X's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions
  C-l -recenter      |    #c -ignore case        |
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
    ~ -rotate buffers|     m -wide display       |
"
  "Help message usually used for 3-way comparison.
Normally, not a user option. See `ediff-help-message' for details.")


;; REPLACE ORIGINAL in `ediff-help.el'
;; Include `#c'.
;;
(defconst ediff-long-help-message-narrow2
  "
p,DEL -previous diff |     | -vert/horiz split   |a/b -copy A/B's region to B/A
n,SPC -next diff     |     h -hilighting         | rx -restore buf X's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions
  C-l -recenter      |    #c -ignore case        |  % -narrow/widen buffs
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
    ~ -swap variants |     m -wide display       |
"
  "Help message when comparing windows or regions line-by-line.
Normally, not a user option. See `ediff-help-message' for details.")


;; REPLACE ORIGINAL in `ediff-help.el'
;; Include `#c'.
;;
(defconst ediff-long-help-message-word-mode
  "
p,DEL -previous diff |     | -vert/horiz split   | xy -copy buf X's region to Y
n,SPC -next diff     |     h -hilighting         | rx -restore buf X's old diff
    j -jump to diff  |                           |
   gx -goto X's point|                           |  ! -recompute diffs
  C-l -recenter      |    #c -ignore case        |  % -narrow/widen buffs
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
    ~ -swap variants |     m -wide display       |
"
  "Help message when comparing windows or regions word-by-word.
Normally, not a user option. See `ediff-help-message' for details.")


;; REPLACE ORIGINAL in `ediff-help.el'
;; Include `#c'.
;;
(defconst ediff-long-help-message-merge
  "
p,DEL -previous diff |     | -vert/horiz split   |  x -copy buf X's region to C
n,SPC -next diff     |     h -hilighting         |  r -restore buf C's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions
  C-l -recenter      |    #c -ignore case        |  + -combine diff regions
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
    ~ -swap variants |     m -wide display       |  / -show ancestor buff
                     |     s -shrink window C    |  & -merge w/new default
                     |     $ -show clashes only  |
"
  "Help message during merging.
Normally, not a user option. See `ediff-help-message' for details.")


;; REPLACE ORIGINAL in `ediff-help.el'
;;
;; Include `#c'.
;;
;;;###autoload
(when (< emacs-major-version 22)
  (defun ediff-help-for-quick-help ()
    "Explain Ediff commands in more detail."
    (interactive)
    (ediff-barf-if-not-control-buffer)
    (let ((pos  (ediff-event-point last-command-event))
          overl cmd)
      (if ediff-xemacs-p
          (setq overl  (extent-at pos (current-buffer) 'ediff-help-info)
                cmd    (ediff-overlay-get overl 'ediff-help-info))
        (setq cmd (car (mapcar (function (lambda (elt)
                                 (overlay-get elt 'ediff-help-info)))
                               (overlays-at pos)))))
      (if (not (stringp cmd))
          (error "Hmm... I don't see an Ediff command around here..."))
      (ediff-documentation "Quick Help Commands")
      (let ((case-fold-search nil))
        (cond ((string= cmd "?") (re-search-forward "^`\\?'"))
              ((string= cmd "G") (re-search-forward "^`G'"))
              ((string= cmd "E") (re-search-forward "^`E'"))
              ((string= cmd "wd") (re-search-forward "^`wd'"))
              ((string= cmd "wx") (re-search-forward "^`wa'"))
              ((string= cmd "a/b") (re-search-forward "^`a'"))
              ((string= cmd "x") (re-search-forward "^`a'"))
              ((string= cmd "xy") (re-search-forward "^`ab'"))
              ((string= cmd "p,DEL") (re-search-forward "^`p'"))
              ((string= cmd "n,SPC") (re-search-forward "^`n'"))
              ((string= cmd "j") (re-search-forward "^`j'"))
              ((string= cmd "gx") (re-search-forward "^`ga'"))
              ((string= cmd "!") (re-search-forward "^`!'"))
              ((string= cmd "*") (re-search-forward "^`\\*'"))
              ((string= cmd "m") (re-search-forward "^`m'"))
              ((string= cmd "|") (re-search-forward "^`|'"))
              ((string= cmd "@") (re-search-forward "^`@'"))
              ((string= cmd "h") (re-search-forward "^`h'"))
              ((string= cmd "r") (re-search-forward "^`r'"))
              ((string= cmd "rx") (re-search-forward "^`ra'"))
              ((string= cmd "##") (re-search-forward "^`##'"))
              ((string= cmd "#c") (re-search-forward "^`#c'"))
              ((string= cmd "#f/#h") (re-search-forward "^`#f'"))
              ((string= cmd "X") (re-search-forward "^`A'"))
              ((string= cmd "v/V") (re-search-forward "^`v'"))
              ((string= cmd "</>") (re-search-forward "^`<'"))
              ((string= cmd "~") (re-search-forward "^`~'"))
              ((string= cmd "i") (re-search-forward "^`i'"))
              ((string= cmd "D") (re-search-forward "^`D'"))
              ((string= cmd "R") (re-search-forward "^`R'"))
              ((string= cmd "M") (re-search-forward "^`M'"))
              ((string= cmd "z/q") (re-search-forward "^`z'"))
              ((string= cmd "%") (re-search-forward "^`%'"))
              ((string= cmd "C-l") (re-search-forward "^`C-l'"))
              ((and (> emacs-major-version 20) (string= cmd "$$"))
               (re-search-forward "^`\\$\\$'"))
              ((and (> emacs-major-version 20) (string= cmd "$*"))
               (re-search-forward "^`\\$\\*'"))
              ((and (< emacs-major-version 21) (string= cmd "$"))
               (re-search-forward "^`\\$'"))
              ((string= cmd "/") (re-search-forward "^`/'"))
              ((string= cmd "&") (re-search-forward "^`&'"))
              ((string= cmd "s") (re-search-forward "^`s'"))
              ((string= cmd "+") (re-search-forward "^`\\+'"))
              ((string= cmd "=") (re-search-forward "^`='"))
              (t (error "Undocumented command! Type `G' in Ediff Control Panel \
to drop a note to the Ediff maintainer")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ediff+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ediff+.el ends here
