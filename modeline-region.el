;;; modeline-region.el --- Show region information in the mode-line.  -*- lexical-binding: t -*-
;;
;; Filename: modeline-region.el
;; Description: Show region information in the mode-line.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2021, Drew Adams, all rights reserved.
;; Created: Thu Nov  4 19:58:03 2021 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Nov  7 21:17:06 2021 (-0800)
;;           By: dradams
;;     Update #: 123
;; URL: https://www.emacswiki.org/emacs/modeline-region.el
;; Doc URL: https://www.emacswiki.org/emacs/ModeLineRegion
;; Keywords: mode-line, region, faces, help, column
;; Compatibility: GNU Emacs: 24.x, 25.x, 26.x, 27.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `backquote', `bookmark',
;;   `bookmark+', `bookmark+-1', `bookmark+-bmu', `bookmark+-key',
;;   `bookmark+-lit', `button', `bytecomp', `cconv', `cl', `cl-lib',
;;   `cmds-menu', `col-highlight', `color', `crosshairs', `custom',
;;   `doremi', `doremi-frm', `easymenu', `facemenu', `facemenu+',
;;   `faces', `faces+', `fit-frame', `font-lock', `font-lock+',
;;   `font-lock-menus', `frame-cmds', `frame-fns', `gv', `help+',
;;   `help-fns', `help-fns+', `help-macro', `help-macro+',
;;   `help-mode', `hexrgb', `highlight', `hl-line', `hl-line+',
;;   `info', `info+', `isearch+', `isearch-prop', `kmacro',
;;   `macroexp', `menu-bar', `menu-bar+', `misc-cmds', `misc-fns',
;;   `mwheel', `naked', `palette', `pp', `pp+', `radix-tree', `rect',
;;   `replace', `ring', `second-sel', `strings', `syntax',
;;   `text-mode', `thingatpt', `thingatpt+', `timer', `vline',
;;   `w32browser-dlgopen', `wid-edit', `wid-edit+', `widget',
;;   `zones'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Show region information in the mode-line.
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
;;  Whether an empty active region is indicated by these modes is
;;  controlled by option `mlr-empty-region-flag'.  By default an empty
;;  region is indicated, to avert you to the fact that you're acting
;;  on an empty region.
;;
;;  When these modes are on, information about the region size is
;;  shown in the mode-line using face `mlr-region', by default.  By
;;  default, this face looks the same as face `region'.
;;
;;  But when you use a command that acts on the region, the mode-line
;;  indication instead uses face `mlr-region-acting-on', to draw
;;  attention to this fact.
;;
;;  In particular, if you use library `isearch+.el' then you can
;;  restrict Isearch commands and replacement commands to the active
;;  region (this is controlled by option
;;  `isearchp-restrict-to-region-flag').  Then, because such commands
;;  act on the region the mode-line indication uses face
;;  `mlr-region-acting-on'.
;;
;;  When mode `modeline-region-mode' or `global-modeline-region-mode'
;;  is on, the menu shown when you click the mode-line position or
;;  size fields lets you do the following, regardless of whether the
;;  region is currently active, in addition to the usual operations of
;;  toggling showing of column and line numbers and indication size:
;;
;;   * Toggle `Showing More Region Info' - Command
;;     `mlr-toggle-non-rectangle-style': toggle the value of option
;;     `mlr-non-rectangle-style'.
;;
;;   * Toggle `Showing More Rectangle Info' - Command
;;     `mlr-toggle-rectangle-style': toggle the value of option
;;     `mlr-rectangle-style'.
;;  
;;   * `Choose Region Style' - Command `mlr-choose-region-style':
;;     Choose what to show in the mode-line when the region is active.
;;     That is, change option `mlr-region-style' to a style you
;;     choose.  The option value is changed, but it's not saved.  Use
;;     `M-x customize-option' to save it.
;;
;;  When the region is active the menu also includes this item:
;;
;;   * `Count Region Contents' - Command `count-words-region': Echo
;;     the number of lines, words, and chars in the region.
;;
;;  When the active region is rectangular the menu also includes this
;;  item:
;;
;;   * `Count Rectangle Contents' - Command
;;     `mlr-count-rectangle-contents': Echo the number of rows,
;;     columns, words, and chars in the rectangle.  By default, the
;;     count excludes words that straddle the rectangle row limits;
;;     with a prefix arg those words are counted.
;;
;;  _____
;;
;;  An unrelated additional enhancement is provided by option
;;  `mlr-column-limit'.  This highlights the current-column number in
;;  the mode-line whenever it's greater than the option value, letting
;;  you know when you've gone past a certain number of characters in
;;  any line.
;;
;;  _____
;;
;;  This library supersedes my older library `modeline-posn.el'.  This
;;  one is more correct, more featureful, and cleaner.
 
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
;;    `mlr-empty-region-flag', `mlr-non-rectangle-style',
;;    `mlr-rectangle-style', `mlr-region-style'.
;;
;;  Commands defined here:
;;
;;    `global-modeline-region-mode', `mlr-choose-region-style',
;;    `mlr-count-rectangle-contents', `modeline-region-mode',
;;    `mlr-toggle-non-rectangle-style', `mlr-toggle-rectangle-style',
;;    `mlr--advice-4', `mlr--advice-5', `mlr--advice-6',
;;    `mlr--advice-7', `mlr--advice-8', `mlr--advice-9',
;;    `mlr--advice-11', `mlr--advice-12', `mlr--advice-13',
;;    `mlr--advice-14', `mlr--advice-15', `mlr--advice-16',
;;    `mlr--advice-18', `mlr--advice-19'.
;;
;;  Non-interactive functions defined here:
;;
;;    `mlr--advice-1', `mlr--advice-2', `mlr--advice-3',
;;    `mlr--advice-10', `mlr--advice-17', `mlr--set-var',
;;    `mlr-show-region-p', `turn-on-modeline-region-mode'.
;;
;;  Constants defined here:
;;
;;    `mlr--region-style-alist', `mlr--region-style-default'.
;;
;;  Internal variables defined here:
;;
;;    `mlr-bytes-format', `mlr-chars-format',
;;    `mlr-lines+chars-format', `mlr-lines+words+chars-format',
;;    `mlr-rect-p', `mlr-region-acting-on', `mlr-rows+cols-format',
;;    `mlr-rows+cols+words+chars-format', `mlr-menu',
;;    `modeline-region-mode-map'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2021/11/07 dadams
;;     mlr-show-region-p: If empty region then return nil if mouse-1 is pressed.
;;     mlr-menu: Reordered, to put Showing items together.
;; 2021/11/05 dadams
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
(defvar mlr--orig-mode-line-position)        ; Here
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
`mlr-column-limit'."
  :group 'modeline-region :group 'faces)

;;;###autoload
(defface mlr-region '((t :inherit region))
  "Face used to highlight the modeline position and size when
the region is active."
  :group 'modeline-region :group 'faces)

;;;###autoload
(defface mlr-region-acting-on '((t (:inherit region :box (:line-width 3 :color "Red"))))
  "Face for modeline position & size when a command acts on active region."
  :group 'modeline-region :group 'faces)

(defvar mlr-rect-p nil
  "Non-nil means the current command is a rectangle command.")

(defvar mlr-region-acting-on nil
  "Non-nil means that a command is acting on the active region.
It is the responsibility of individual commands to manage the value.")
(make-variable-buffer-local 'mlr-region-acting-on)

;;;###autoload
(defcustom mlr-column-limit 70
  "Current column greater than this means highlight column in mode-line."
  :group 'modeline-region
  :type '(choice
          (const   :tag "NO limit: don't highlight column number of long lines" nil)
          (integer :tag "Maximum unhighlighted current-column number")))

;;;###autoload
(defcustom mlr-count-partial-words-flag nil
  "Non-nil means count words that straddle rectangle rows.
By default (nil value), count only words that are entirely within the
rectangle."
    :group 'modeline-region :type 'boolean)

;;;###autoload
(defcustom mlr-empty-region-flag t
  "Non-nil means indicate an active region even when empty."
  :group 'modeline-region :type 'boolean)

;;;###autoload
(defcustom mlr-non-rectangle-style 'lines+chars
  "Mode-line info about region size, except when rectangular."
  :group 'modeline-region
  :type '(choice
          (const :tag "Lines and characters"    lines+chars)
          (const :tag "Lines, words, and chars" lines+words+chars)))

(defun mlr-toggle-non-rectangle-style (&optional msgp)
  "Toggle value of option `mlr-non-rectangle-style'."
  (interactive "p")
  (setq mlr-non-rectangle-style  (if (eq mlr-non-rectangle-style 'lines+chars)
                                     'lines+words+chars
                                   'lines+chars))
  (force-mode-line-update 'ALL)
  (when msgp (message "`mlr-non-rectangle-style' is now `%s'" mlr-non-rectangle-style)))

(when (fboundp 'extract-rectangle-bounds) ; Emacs 26+

  (defun mlr-count-rectangle-contents (start end &optional count-partial-words-p msgp)
    "List the number of rows, columns, words, and chars for a rectangle.
Return a list of the number of rows (lines), columns (chars per row),
words, and chars.  If called interactively, echo those numbers.

Option `mlr-count-partial-words-flag' controls whether to count words
that straddle the beginning or end of a rectangle row.  A prefix arg
flips the behavior specified by the option value.

When called from Lisp:

* START and END are the limits of the active region.
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
    (let ((rows    (count-lines start end))
          (cols    (let ((rpc  (save-excursion
                                 (rectangle--pos-cols (region-beginning) (region-end)))))
                     (abs (- (car rpc) (cdr rpc)))))
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

  (defcustom mlr-rectangle-style 'rows+cols
    "Mode-line info about region size when a rectangle is selected."
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
    (count-lines (region-beginning) (region-end)) ; Rows (rectangle) or lines (region)

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
;;;###autoload
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
                                   :visible mlr-rect-p)))
                   (define-key map [count-words-region]
                     '(menu-item "Count Region Contents" count-words-region
                                 :help "Show number of lines, words, and chars in active region"
                                 :visible (use-region-p)))
                   (define-key map [mlr-choose-region-style]
                     '(menu-item "Choose Region Style" mlr-choose-region-style
                                 :help "Change the value of option `mlr-region-style'"))
                   (when (fboundp 'mlr-toggle-rectangle-style) ; Emacs 26+
                     (define-key map [mlr-toggle-rectangle-style]
                       '(menu-item "Showing More Rectangle Info" mlr-toggle-rectangle-style
                                   :help "Toggle value of option `mlr-rectangle-style'"
                                   :enable size-indication-mode
                                   :button (:toggle . (eq 'rows+cols+words+chars
                                                          mlr-rectangle-style)))))
                   (define-key map [mlr-toggle-non-rectangle-style]
                     '(menu-item "Showing More Region Info" mlr-toggle-non-rectangle-style
                                 :help "Toggle value of option `mlr-non-rectangle-style'"
                                 :enable size-indication-mode
                                 :button (:toggle . (eq 'lines+words+chars
                                                        mlr-non-rectangle-style))))
                   (define-key map [size-indication-mode]
                     '(menu-item "Showing Size Indication" size-indication-mode
                                 :help "Toggle displaying a size indication in the mode-line"
                                 :button (:toggle . size-indication-mode)))
                   (define-key map [line-number-mode]
                     '(menu-item "Showing Line Numbers" line-number-mode
                                 :help "Toggle displaying line numbers in the mode-line"
                                 :button (:toggle . line-number-mode)))
                   (define-key map [column-number-mode]
                     '(menu-item "Showing Column Numbers" column-number-mode
                                 :help "Toggle displaying column numbers in the mode-line"
                                 :button (:toggle . column-number-mode)))
                   (define-key map [mode-line down-mouse-1] map)
                   map)
  "Menu keymap for `modeline-region.el' features.
Used in place of `mode-line-column-line-number-mode-map'.")

;;;###autoload
(defvar modeline-region-mode-map (make-sparse-keymap)
  "Keymap for minor mode `modeline-region-mode'")

(defun mlr--set-var (variable value)
  "Return a function that restores VARIABLE to VALUE."
  (lambda () (set variable  value)))

(define-minor-mode modeline-region-mode
  "Toggle showing region information in the mode-line.
The information shown depends on options `mlr-region-style',
`mlr-non-rectangle-style' and `mlr-rectangle-style' (for Emacs 26+).

\(This mode automatically turns on `size-indication-mode'.)"
  nil nil nil :keymap 'modeline-region-mode-map
  (cond (modeline-region-mode
         (size-indication-mode 1)
         (make-local-variable 'mode-line-position)
         (setq-local mlr--orig-mode-line-position  mode-line-position)
         ;; Use region size if region is active.
         ;; Highlight line & column indicator if column > `mlr-column-limit'.
         (setq-local mode-line-position
                     '(:eval
                       `((-3 ,(propertize
                               "%p"
                               'local-map mlr-menu
                               'mouse-face 'mode-line-highlight
                               'help-echo "Buffer position, mouse-1: Line/col menu"))
                         ;; We might be able to remove one or more of these `ignore-error's,
                         ;; but it seems better to keep them, at least for now.
                         (size-indication-mode
                          (8 ,(propertize
                               (if (or mlr-region-acting-on  (ignore-errors (mlr-show-region-p)))
                                   (or (ignore-errors
                                         (apply #'format (mapcar #'eval mlr-region-style)))
                                       "")
                                 " of %I")
                               'face (if mlr-region-acting-on
                                         'mlr-region-acting-on
                                       (and (ignore-errors (mlr-show-region-p))
                                            'mlr-region))
                               'local-map mlr-menu
                               'mouse-face 'mode-line-highlight
                               'help-echo "Buffer position, mouse-1: Line/col menu")))
                         (line-number-mode
                          ((column-number-mode ; Line-number mode & column-number-mode
                            (column-number-indicator-zero-based
                             (10 ,(propertize
                                   " (%l,%c)"
                                   'face (and mlr-column-limit
                                              (> (current-column) mlr-column-limit)
                                              'mlr-column-warning)
                                   'local-map mlr-menu
                                   'mouse-face 'mode-line-highlight
                                   'help-echo "Line and column, mouse-1: Line/col menu"))
                             (10 ,(propertize
                                   " (%l,%C)"
                                   'face (and mlr-column-limit
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
                                  'face (and mlr-column-limit
                                             (> (current-column) mlr-column-limit)
                                             'mlr-column-warning)
                                  'local-map mlr-menu
                                  'mouse-face 'mode-line-highlight
                                  'help-echo "Column number, mouse-1: Line/col menu"))
                             (5 ,(propertize
                                  " C%C"
                                  'face (and mlr-column-limit
                                             (> (current-column) mlr-column-limit)
                                             'mlr-column-warning)
                                  'local-map mlr-menu
                                  'mouse-face 'mode-line-highlight
                                  'help-echo "Column number, mouse-1: Line/col menu")))))))))
         (advice-add 'replace-dehighlight :after #'mlr--advice-1)
         (advice-add 'query-replace-read-args :around #'mlr--advice-2)
         (advice-add 'query-replace-read-to :around #'mlr--advice-2)
         (advice-add 'perform-replace :around #'mlr--advice-2)
         (advice-add 'query-replace-read-from :around #'mlr--advice-3)
         (advice-add 'keep-lines-read-args :around #'mlr--advice-3)
         (advice-add 'map-query-replace-regexp :around #'mlr--advice-4)
         (advice-add 'prepend-to-buffer :around 'mlr--advice-5)
         (advice-add 'append-to-buffer :around 'mlr--advice-6)
         (advice-add 'copy-to-buffer :around 'mlr--advice-7)
         (advice-add 'append-to-file :around 'mlr--advice-8)
         (advice-add 'write-region :around 'mlr--advice-9)
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
         (advice-add 'isearch-query-replace-regexp :around 'mlr--advice-19))
        (t
         (make-local-variable 'mlr--orig-mode-line-position)
         (setq-local mode-line-position  mlr--orig-mode-line-position)
         (advice-remove 'replace-dehighlight #'mlr--advice-1)
         (advice-remove 'query-replace-read-args #'mlr--advice-2)
         (advice-remove 'query-replace-read-to #'mlr--advice-2)
         (advice-remove 'perform-replace #'mlr--advice-2)
         (advice-remove 'query-replace-read-from #'mlr--advice-3)
         (advice-remove 'keep-lines-read-args #'mlr--advice-3)
         (advice-remove 'map-query-replace-regexp #'mlr--advice-4)
         (advice-remove 'prepend-to-buffer 'mlr--advice-5)
         (advice-remove 'append-to-buffer 'mlr--advice-6)
         (advice-remove 'copy-to-buffer 'mlr--advice-7)
         (advice-remove 'append-to-file 'mlr--advice-8)
         (advice-remove 'write-region 'mlr--advice-9)
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
         (advice-remove 'isearch-query-replace-regexp 'mlr--advice-19))))

(defun turn-on-modeline-region-mode ()
  "Turn on `modeline-region-mode'."
  (modeline-region-mode))

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
;;; Some functions are advised to set `mlr-region-acting-on' to non-nil, to turn
;;; on the mode-line info and highlighting for the (non-rectangular) region.
;;; When this variable is non-nil, the face used for this in the mode-line is
;;; `mlr-region-acting-on'. not `mlr-region'.  This is typically used to
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
This applies to `query-replace-regexp-eval', `keep-lines',
`flush-lines', and `how-many'.  It is done by advising
`query-replace-read-from' and `keep-lines-read-args'."
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


;;; Commands from `simple.el' and `files.el' -----------------------------------
;;; They are loaded by default.  (`files.el' has no `provide'.)

(defun mlr--advice-5 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `prepend-to-buffer'."
  (interactive
   (let (;; (icicle-change-region-background-flag  nil)
         (mlr-region-acting-on  (use-region-p)))
     (list (read-buffer "Prepend to buffer: " (other-buffer (current-buffer) t))
           (region-beginning) (region-end))))
  (apply orig-fun args))

(defun mlr--advice-6 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `append-to-buffer'."
  (interactive
   (let (;; (icicle-change-region-background-flag  nil)
         (mlr-region-acting-on  (use-region-p)))
     (list (read-buffer "Append to buffer: " (other-buffer (current-buffer) t))
           (region-beginning) (region-end))))
  (apply orig-fun args))

(defun mlr--advice-7 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `copy-to-buffer'."
  (interactive
   (let (;; (icicle-change-region-background-flag  nil)
         (mlr-region-acting-on  (use-region-p)))
     (list (read-buffer "Copy to buffer: " (other-buffer (current-buffer) t))
           (region-beginning) (region-end))))
  (apply orig-fun args))

(defun mlr--advice-8 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `append-to-file'."
  (interactive
   (let (;; (icicle-change-region-background-flag  nil)
         (mlr-region-acting-on  (use-region-p)))
     (list (region-beginning)
           (region-end)
           (read-file-name "Append to file: "))))
  (apply orig-fun args))


;; Built-in functions (from C code). -------------------------------------------

(defun mlr--advice-9 (orig-fun &rest args)
  "Turn on mode-line region highlighting for `write-region'."
  (interactive
   (let (;; (icicle-change-region-background-flag  nil)
         (mlr-region-acting-on  (or (use-region-p)
                                    (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))))
     (list (region-beginning)
           (region-end)
           (read-file-name "Write region to file: ")
           nil
           nil
           nil
           t)))
  (apply orig-fun args))


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
            (mlr-region-acting-on  (and (or (use-region-p)
                                            (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))
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
           (mlr-region-acting-on  (or (use-region-p)
                                      (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))))
       (list (read-char "Copy to register: ")
             (region-beginning)
             (region-end)
             current-prefix-arg)))
    (apply orig-fun args))

  (defun mlr--advice-12 (orig-fun &rest args)
    "Turn on mode-line region highlighting for `prepend-to-register'."
    (interactive
     (let (;; (icicle-change-region-background-flag  nil)
           (mlr-region-acting-on  (or (use-region-p)
                                      (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))))
       (list (read-char "Prepend to register: ")
             (region-beginning)
             (region-end)
             current-prefix-arg)))
    (apply orig-fun args))

  (defun mlr--advice-13 (orig-fun &rest args)
    "Turn on mode-line region highlighting for `append-to-register'."
    (interactive
     (let (;; (icicle-change-region-background-flag  nil)
           (mlr-region-acting-on  (or (use-region-p)
                                      (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))))
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
         (mlr-region-acting-on  (or (use-region-p)
                                    (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg)))
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
         (mlr-region-acting-on  (or (use-region-p)
                                    (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg)))
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
         (mlr-region-acting-on  (or (use-region-p)
                                    (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'modeline-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modeline-region.el ends here
