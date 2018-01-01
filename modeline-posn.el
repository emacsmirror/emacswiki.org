;;; modeline-posn.el --- Set up `mode-line-position'.
;;
;; Filename: modeline-posn.el
;; Description: Set up `mode-line-position'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2006-2018, Drew Adams, all rights reserved.
;; Created: Thu Sep 14 08:15:39 2006
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon Jan  1 15:01:52 2018 (-0800)
;;           By: dradams
;;     Update #: 846
;; URL: https://www.emacswiki.org/emacs/download/modeline-posn.el
;; Doc URL: https://www.emacswiki.org/emacs/ModeLinePosition
;; Keywords: mode-line, region, column
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Change variable `mode-line-position' so that the following changes
;;  are made to the mode line:
;;
;;  1. Highlight the column number when the current column is greater
;;     than limit `modelinepos-column-limit'.  Face
;;     `modelinepos-column-warning' is used for the highlighting.
;;
;;  2. Make `size-indication-mode' show the size of the region (or the
;;     shape of the selected rectangle), instead of the buffer size,
;;     whenever the region is active.
;;
;;  3. Make `size-indication-mode' show that the current command acts
;;     on the active region or acts specially because the region is
;;     now active (Emacs 23+).
;;
;;  For #2: When the region is active, the mode line displays some
;;  information that you can customize - see option
;;  `modelinepos-style'.  Customization choices for this include (a)
;;  the number of chars, (b) the number of bytes, (c) the number of
;;  chars and number of lines (or the number of rows and number of
;;  columns, if a rectangle is selected), and (d) anything else you
;;  might want.  Choice (c) is the default.
;;
;;  For (d), you provide a `format' expression as separate components:
;;  the format string and the sexp arguments to be evaluated and
;;  plugged into the string.  The number of sexp args depends on the
;;  format string that you use: one for each `%' construct.
;;
;;  Choice (d) is provided so that you can choose alternative
;;  formatting styles.  For example, instead of ` 256 ch, 13 l', you
;;  could show ` (256 chars, 13 lines)'.  But (d) can really show
;;  information at all.  It need not have anything to do with the
;;  region, but it is nevertheless shown when the region is active.
;;
;;  Option `modelinepos-empty-region-flag' determines whether to show
;;  the active-region indication when the active region is empty.  By
;;  default it is t, meaning indicate an empty active region.
;;
;;  For #3, certain standard Emacs commands act on the active region
;;  or restrict their scope to it.  Some of these are handled here by
;;  highlighting the region indication in the mode line specially
;;  (using face `modelinepos-region-acting-on' instead of face
;;  `modelinepos-region').  The region-restricted commands defined in
;;  standard library `replace.el' are handled this way, for example.
;;
;;  If you also use my library `isearch+.el', which I recommend, then
;;  (for Emacs 24.3+) Isearch commands too can optionally be
;;  restricted to the active region, and they too are handled by the
;;  special mode-line highlighting.  This includes `M-%' and `C-M-%'
;;  during Isearch, which invoke query-replacement.  Not only is the
;;  Isearch region indicated in the mode line, but the
;;  query-replacement command is invoked from Isearch with that region
;;  active, so it too is limited to that scope.
;;
;;  Note: Loading this library changes the default definition of
;;        `mode-line-position'.
;;
;;  To use this library, put this in your Emacs init file (~/.emacs):
;;
;;    (require 'modeline-posn)
;;
;;  To show the column number highlighting, turn on Column Number
;;  mode.  You can do that in your Emacs init file this way:
;;
;;    (column-number-mode 1)
;;
;;  To show the buffer and region size indication in the mode line,
;;  turn on Size Indication.  You can do that in your Emacs init file
;;  this way:
;;
;;    (size-indication-mode 1) ; Turn on Size Indication mode.
;;
;;  You can customize `modelinepos-column-limit' or bind it to
;;  different values for different modes.
;;
;;
;;  Faces defined here:
;;
;;    `modelinepos-column-warning', `modelinepos-region',
;;    `modelinepos-region-acting-on' (Emacs 23+).
;;
;;  User options defined here:
;;
;;    `modelinepos-column-limit', `modelinepos-empty-region-flag',
;;    `modelinepos-style', `use-empty-active-region' (Emacs < 23).
;;
;;  Non-interactive functions defined here:
;;
;;    `modelinepos-show-region-p', `use-region-p' (Emacs < 23).
;;
;;  Non-option variables defined here:
;;
;;    `modelinepos-rect-p', `modelinepos-region-acting-on' (Emacs
;;    23+).
;;
;;  
;;  ***** NOTE: The following built-in functions have 
;;              been ADVISED HERE:
;;
;;    `write-region'.
;;
;;
;;  ***** NOTE: The following variables defined in `bindings.el' have
;;              been REDEFINED HERE:
;;
;;    `mode-line-position'.
;;
;;
;;  ***** NOTE: The following commands defined in `files.el' have
;;              been REDEFINED HERE:
;;
;;    `append-to-file'.
;;
;;
;;  ***** NOTE: The following functions defined in `isearch+.el' have
;;              been ADVISED HERE:
;;
;;    `isearch-mode' (Emacs 24.3+).
;;
;;
;;  ***** NOTE: The following functions defined in `isearch.el' have
;;              been ADVISED HERE, if you use library `isearch+.el':
;;
;;    `isearch-query-replace' (Emacs 24.3+),
;;    `isearch-query-replace-regexp' (Emacs 24.3+).
;;
;;
;;  ***** NOTE: The following functions defined in `rect.el' have
;;              been ADVISED HERE:
;;
;;    `cua-rectangle-mark-mode', `rectangle-number-lines' (Emacs 24+),
;;    `rectangle-mark-mode' (Emacs 24.4+), `string-insert-rectangle',
;;    `string-rectangle'.
;;
;;
;;  ***** NOTE: The following functions defined in `replace.el' have
;;              been ADVISED HERE:
;;
;;    `keep-lines-read-args', `map-query-replace-regexp',
;;    `perform-replace', `query-replace-read-args',
;;    `query-replace-read-from', `query-replace-read-to',
;;    `replace-dehighlight'.
;;
;;
;;  ***** NOTE: The following functions defined in `register.el' have
;;              been ADVISED HERE:
;;
;;    `append-to-register', `copy-to-register', `prepend-to-register',
;;    `register-read-with-preview'.
;;
;;
;;  ***** NOTE: The following commands defined in `simple.el' have
;;              been REDEFINED HERE:
;;
;;    `size-indication-mode'.
;;
;;
;;  ***** NOTE: The following commands defined in `simple.el' have
;;              been ADVISED HERE:
;;
;;     `append-to-buffer', `copy-to-buffer', `prepend-to-buffer'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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
;;     Added: modelinepos-rect-p, cua-rectangle-mark-mode, rectangle-number-lines, rectangle-mark-mode,
;;            string-insert-rectangle, string-rectangle.
;;     modelinepos-style: Respect modelinepos-rect-p, showing rows & cols.
;;     register-read-with-preview: Bind modelinepos-rect-p.  Added copy-rectangle-to-register to cms affected.
;; 2014/07/15 dadams
;;     Advise functions append-to-buffer, prepend-to-buffer, copy-to-buffer, append-to-file,
;;       register-read-with-preview, copy-to-register, append-to-register, prepend-to-register, write-region.
;; 2014/01/18 dadams
;;     Added: modelinepos-region-acting-on (face and var), 
;;            use-region-p (Emacs 22), use-empty-active-region (Emacs 22).
;;     Renamed: modelinepos-empty-region-p to modelinepos-show-region-p.
;;     modelinepos-show-region-p: Wrapped in condition-case, to ignore errors.
;;     mode-line-position (Emacs 23+): Show modelinepos-region-acting-on highlight (cmd restricted to region).
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
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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

;; Quiet the byte-compiler.

(defvar isearchp-reg-beg)                 ; In `isearch+.el'
(defvar isearchp-reg-end)                 ; In `isearch+.el'
(defvar isearchp-restrict-to-region-flag) ; In `isearch+.el'
(defvar rectangle--string-preview-window) ; In `rect.el' for Emacs 25+
(defvar use-empty-active-region)          ; In `simple.el' for Emacs 23+

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defface modelinepos-column-warning '((t (:foreground "Red")))
  "*Face used to highlight the modeline column number.
This is used when the current column number is greater than
`modelinepos-column-limit'."
  :group 'Modeline :group 'Convenience :group 'Help :group 'faces)

;;;###autoload
(defface modelinepos-region '((t :inherit region))
  "*Face used to highlight the modeline position and size when
the region is active."
  :group 'Modeline :group 'Convenience :group 'Help :group 'faces)

;;;###autoload
(defface modelinepos-region-acting-on '((t (:inherit region :box (:line-width 3 :color "Red"))))
  "*Face for modeline position & size when a command acts on active region.
\(Not used for Emacs 22)."
  :group 'Modeline :group 'Convenience :group 'Help :group 'faces)

(defvar modelinepos-rect-p nil
  "Non-nil means the current command is a rectangle command.")

(defvar modelinepos-region-acting-on nil
  "Non-nil means that a command is acting on the active region.
It is the responsibility of individual commands to manage the value.
\(Not used for Emacs 22).")
(make-variable-buffer-local 'modelinepos-region-acting-on)

;;;###autoload
(defcustom modelinepos-column-limit 70
  "*Current column greater than this means highlight column in mode-line."
  :type 'integer :group 'Modeline :group 'Convenience :group 'Help)

;;;###autoload
(defcustom modelinepos-empty-region-flag t
  "*Non-nil means indicate an active region even when empty."
  :type 'boolean :group 'Modeline :group 'Convenience :group 'Help)

;;;###autoload
(defcustom modelinepos-style '((if modelinepos-rect-p " %d rows, %d cols" " %d ch, %d l")
                               (if modelinepos-rect-p
                                   (count-lines (region-beginning) (region-end))
                                 (abs (- (mark t) (point))))
                               (if modelinepos-rect-p
                                   (if (fboundp 'rectangle--pos-cols) ; Emacs 25+
                                       (let ((rpc  (save-excursion
                                                     (rectangle--pos-cols (region-beginning) (region-end)))))
                                         (abs (- (car rpc) (cdr rpc))))
                                     (let ((start  (region-beginning))
                                           (end    (region-end))
                                           startcol endcol)
                                       (save-excursion
                                         (goto-char start)
                                         (setq startcol   (current-column))
                                         (beginning-of-line)
                                         (goto-char end)
                                         (setq endcol  (current-column))
                                         (when (< endcol startcol) ; Ensure start column is the left one.
                                           (let ((col  startcol))
                                             (setq startcol  endcol
                                                   endcol    col)))
                                         (abs (- startcol endcol)))))
                                 (count-lines (mark t) (point))))
  "*What info to include about the region size, in mode-line.
Value `chars+lines' means print the number of characters and lines or,
if a rectangle command is invoked, the number of rows and columns.

In general, the value is a format string followed by however many
sexps the string expects as arguments."
  :type '(choice
          (const :tag "Characters: \"_ chars\""
           (" %d chars" (abs (- (mark t) (point)))))
          ;; Should we use this instead?  It can sometimes be costly.
          ;; See https://emacs.stackexchange.com/a/29912/105.
          ;; (const :tag "Bytes: \"_ bytes\""
          ;;  (" %d bytes"
          ;;   (if (fboundp 'bufferpos-to-filepos) ; Emacs 25+
          ;;       (- (bufferpos-to-filepos (region-end) 'exact)
          ;;          (bufferpos-to-filepos (region-beginning) 'exact))
          ;;     (string-bytes (buffer-substring-no-propertiesw (region-beginning) (region-end))))))
          (const :tag "Bytes: \"_ bytes\""
           (" %d bytes" (string-bytes (buffer-substring-no-propertiesw (region-beginning) (region-end)))))
          (const :tag "Chars & Lines: \"_ ch, _ l\" or Rows & Columns: \"_ rows, _ cols\""
           ((if modelinepos-rect-p " %d rows, %d cols" " %d ch, %d l")
            (if modelinepos-rect-p
                (count-lines (region-beginning) (region-end))
              (abs (- (mark t) (point))))
            (if modelinepos-rect-p
                (if (fboundp 'rectangle--pos-cols) ; Emacs 25+
                    (let ((rpc  (save-excursion (rectangle--pos-cols (region-beginning) (region-end)))))
                      (abs (- (car rpc) (cdr rpc))))
                  (let ((start  (region-beginning))
                        (end    (region-end))
                        startcol endcol)
                    (save-excursion
                      (goto-char start)
                      (setq startcol   (current-column))
                      (beginning-of-line)
                      (goto-char end)
                      (setq endcol  (current-column))
                      (when (< endcol startcol) ; Ensure start column is the left one.
                        (let ((col  startcol))
                          (setq startcol  endcol
                                endcol    col)))
                      (abs (- startcol endcol)))))
              (count-lines (mark t) (point)))))
          (list :tag "Customized format"
           (string :tag "Format string")
           (repeat :inline t (sexp :tag "Sexp argument for format string"))))
  :group 'Modeline :group 'Convenience :group 'Help)

;; REPLACES ORIGINAL defined in `simple.el'.
;;
;; Doc string updated to mention region size indication.
;;
;; Added groups `Modeline', `Convenience', and `Help'.
;;
;;;###autoload
(define-minor-mode size-indication-mode
    "Toggle Size Indication mode.
With arg, turn Size Indication mode on iff arg is positive.
When Size Indication mode is enabled, the buffer or region size
appears in the mode line.  If Transient Mark mode is enabled, the
region size is shown; otherwise, the size of the accessible part
of the buffer is shown."
  :global t :group 'editing-basics :group 'Modeline
  :group 'Convenience :group 'Help)

(defun modelinepos-show-region-p ()
  "Return non-nil if region is active and nonempty, or emptiness is OK.
Option `modelinepos-empty-region-flag' non-nil means emptiness is OK.

But do not return non-nil if the condition is true but you are
selecting with the mouse.  This is to prevent highlighting the mode
line whenever you press `mouse-1' without dragging at least one
character."
  ;; Fragile hack: Starting with Emacs 24, the region is considered empty as soon as
  ;; you press `mouse-1' (`down-mouse-1').  That causes modeline highlighting each time
  ;; you just click `mouse-1', i.e., without dragging it.
  ;;
  ;; The hack is to check whether `echo-keystrokes' is 0.  `mouse-drag-track' binds
  ;; `echo-keystrokes' to 0, and that seems to be the only way to tell whether we are
  ;; in `mouse-drag-track'.  If the Emacs code for that changes then this might break.
  ;;
  (condition-case nil                   ; Ignore errors, just in case.
      (and transient-mark-mode  mark-active
           (or (if (> emacs-major-version 23)
                   (and (not (eq 0 echo-keystrokes))  modelinepos-empty-region-flag)
                 modelinepos-empty-region-flag)
               (/= (region-beginning) (region-end))))
    (error nil)))



;; REPLACES ORIGINAL defined in `bindings.el'.
;;
;; Use region size if region is active.
;;
;; Highlight line & column indicator if column > `modelinepos-column-limit'.
;;
(unless (> emacs-major-version 22)
  (setq-default mode-line-position
                '(:eval
                  (let ((help-echo "mouse-1: select (drag to resize), mouse-2: \
delete others, mouse-3: delete this"))
                    `((-3 ,(propertize "%p" 'help-echo help-echo))
                      (size-indication-mode
                       (8 ,(propertize
                            (if (modelinepos-show-region-p)
                                (apply #'format (mapcar #'eval modelinepos-style))
                              " of %I")
                            'face (and (modelinepos-show-region-p)  'modelinepos-region)
                            'help-echo help-echo)))
                      (line-number-mode
                       ((column-number-mode
                         (10 ,(propertize
                               " (%l,%c)"
                               'face (and (> (current-column)
                                             modelinepos-column-limit)
                                          'modelinepos-column-warning)
                               'help-echo help-echo))
                         (6 ,(propertize " L%l" 'help-echo help-echo))))
                       ((column-number-mode
                         (5 ,(propertize
                              " C%c"
                              'face (and (> (current-column)
                                            modelinepos-column-limit)
                                         'modelinepos-column-warning)
                              'help-echo help-echo))))))))))



;; REPLACES ORIGINAL defined in `bindings.el'.
;;
;; Use region size if region is active.
;;
;; Highlight line & column indicator if column > `modelinepos-column-limit'.
;;
(when (> emacs-major-version 22)
  (setq-default mode-line-position
                '(:eval
                  `((-3 ,(propertize "%p"
                                     'local-map mode-line-column-line-number-mode-map
                                     'mouse-face 'mode-line-highlight
                                     'help-echo "Buffer position, mouse-1: Line/col menu"))
                    ;; We might be able to remove one or more of these `condition-case's, but it seems
                    ;; better to keep them, at least for now.
                    (size-indication-mode
                     (8 ,(propertize
                          (if (or modelinepos-region-acting-on
                                  (condition-case nil
                                      (modelinepos-show-region-p)
                                    (error nil)))
                              (condition-case nil
                                  (apply #'format (mapcar #'eval modelinepos-style))
                                (error ""))
                            " of %I")
                          'face (if modelinepos-region-acting-on
                                    'modelinepos-region-acting-on
                                  (and (condition-case nil
                                           (modelinepos-show-region-p)
                                         (error nil))
                                       'modelinepos-region))
                          'local-map mode-line-column-line-number-mode-map
                          'mouse-face 'mode-line-highlight
                          'help-echo "Buffer position, mouse-1: Line/col menu")))
                    (line-number-mode
                     ((column-number-mode
                       (10 ,(propertize
                             " (%l,%c)"
                             'face (and (> (current-column)
                                           modelinepos-column-limit)
                                        'modelinepos-column-warning)
                             'local-map mode-line-column-line-number-mode-map
                             'mouse-face 'mode-line-highlight
                             'help-echo "Line and column, mouse-1: Line/col menu"))
                       (6 ,(propertize
                            " L%l"
                            'local-map mode-line-column-line-number-mode-map
                            'mouse-face 'mode-line-highlight
                            'help-echo "Line number, mouse-1: Line/col menu"))))
                     ((column-number-mode
                       (5 ,(propertize
                            " C%c"
                            'face (and (> (current-column)
                                          modelinepos-column-limit)
                                       'modelinepos-column-warning)
                            'local-map mode-line-column-line-number-mode-map
                            'mouse-face 'mode-line-highlight
                            'help-echo "Column number, mouse-1: Line/col menu")))))))))
 

;;; Advise some standard functions, so they use `modelinepos-region-acting-on' during
;;; initial input when they act on the active region.


;; `use-region-p': For Emacs 22 only.  Provide the standard Emacs 23+ definition.  Cleaned up the doc string.
(unless (fboundp 'use-region-p)

  (defcustom use-empty-active-region nil
    "Whether region-aware commands should act on empty regions.
Region-aware commands are those that act differently depending on
whether or not the region is active and Transient Mark mode is
enabled.
If nil, these commands treat empty regions as inactive.
If non-nil, these commands treat an active region as such, even if it
is empty."
    :type 'boolean :group 'editing-basics)

  (defun use-region-p ()
    "Return t if the region is active and it is appropriate to act on it.
This is used by commands that act specially on the region under
Transient Mark mode.

The return value is t if Transient Mark mode is enabled and the
mark is active; furthermore, if `use-empty-active-region' is nil,
the region must not be empty.  Otherwise, the return value is nil.

For some commands, it may be appropriate to ignore the value of
`use-empty-active-region'; in that case, use `region-active-p'."
    (and transient-mark-mode
         mark-active
         (or use-empty-active-region  (> (region-end) (region-beginning))))))


;;; Functions from `replace.el' (loaded by default, with no `provide').

;; This one works for `query-replace', `query-replace-regexp', `replace-string',
;; and `replace-regexp'.
(defadvice query-replace-read-args (around bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
        (modelinepos-region-acting-on          (or (use-region-p)
                                                   (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))))
    ad-do-it))

;; This one is for `isearch-query-replace'.
(defadvice query-replace-read-to (around bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
        (modelinepos-region-acting-on          (or (use-region-p)
                                                   (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))))
    ad-do-it))

;; This one is for `query-replace-regexp-eval'.
;; We don't really need the second part of the `(or...)', but could just use `(use-region-p)'.
(defadvice query-replace-read-from (around bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
        (modelinepos-region-acting-on          (or (use-region-p)
                                                   (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))))
    ad-do-it))

;; This one works for `keep-lines', `flush-lines', and `how-many'.
;; We don't really need the second part of the `(or...)', but could just use `(use-region-p)'.
(defadvice keep-lines-read-args (around bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
        (modelinepos-region-acting-on          (or (use-region-p)
                                                   (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))))
    ad-do-it))

;; Advise interactive part of `map-query-replace-regexp'.
;; We don't really need the second part of the `(or...)', but could just use `(use-region-p)'.
(defadvice map-query-replace-regexp (before bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (interactive
   (let* ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
          (modelinepos-region-acting-on            (or (use-region-p)
                                                       (and (boundp 'isearchp-reg-beg) isearchp-reg-beg)))
          (from                                  (read-regexp "Map query replace (regexp): " nil
                                                              query-replace-from-history-variable))
          (to                                    (read-from-minibuffer
                                                  (format
                                                   "Query replace %s with (space-separated strings): "
                                                   (query-replace-descr from))
                                                  nil nil nil
                                                  query-replace-to-history-variable from t)))
     (list from
           to
           (and current-prefix-arg
                (prefix-numeric-value current-prefix-arg))
           (if (and transient-mark-mode mark-active)
               (region-beginning))
           (if (and transient-mark-mode mark-active)
               (region-end))))))

;; Turn on highlighting for act of (query-)replacing.
(defadvice perform-replace (before bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (setq modelinepos-region-acting-on  (or (use-region-p)  (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))))

;; Turn it off after highlighting for replacement commands.  There is no hook, so use `replace-dehighlight'.
(defadvice replace-dehighlight (after bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (setq modelinepos-region-acting-on  nil))


;;; Commands from `simple.el' and `files.el' (loaded by default; `files.el' has no `provide').

;; We don't really need the second part of the `(or...)', but could just use `(use-region-p)'.
(defadvice prepend-to-buffer (around bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (interactive
   (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
         (modelinepos-region-acting-on          (or (use-region-p)
                                                    (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))))
     (list (read-buffer "Prepend to buffer: " (other-buffer (current-buffer) t))
           (region-beginning) (region-end))))
  ad-do-it)

;; We don't really need the second part of the `(or...)', but could just use `(use-region-p)'.
(defadvice append-to-buffer (around bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (interactive
   (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
         (modelinepos-region-acting-on          (or (use-region-p)
                                                    (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))))
     (list (read-buffer "Append to buffer: " (other-buffer (current-buffer) t))
           (region-beginning) (region-end))))
  ad-do-it)

;; We don't really need the second part of the `(or...)', but could just use `(use-region-p)'.
(defadvice copy-to-buffer (around bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (interactive
   (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
         (modelinepos-region-acting-on          (or (use-region-p)
                                                    (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))))
     (list (read-buffer "Copy to buffer: " (other-buffer (current-buffer) t))
           (region-beginning) (region-end))))
  ad-do-it)

;; We don't really need the second part of the `(or...)', but could just use `(use-region-p)'.
(defadvice append-to-file (around bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (interactive
   (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
         (modelinepos-region-acting-on          (or (use-region-p)
                                                    (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))))
     (list (region-beginning)
           (region-end)
           (read-file-name "Append to file: "))))
  ad-do-it)


;; Built-in functions (from C code): 

;; We don't really need the second part of the `(or...)', but could just use `(use-region-p)'.
;;
(defadvice write-region (around bind-modelinepos-region-acting-on activate)
  "\(Not used for Emacs 22.)"
  (interactive
   (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
         (modelinepos-region-acting-on          (or (use-region-p)
                                                    (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))))
     (list (region-beginning)
           (region-end)
           (read-file-name "Write region to file: ")
           nil
           nil
           nil
           t)))
  ad-do-it)


;;; Functions from `register.el'.

(if (fboundp 'register-read-with-preview)
    ;;  Emacs 24.4+.  Used by all register-reading cmds, but restrict highlighting to those affecting region.
    (defadvice register-read-with-preview (around bind-modelinepos-region-acting-on activate)
    (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' for minibuffer input.
          (modelinepos-region-acting-on          (and (or (use-region-p)
                                                          (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))
                                                      (member this-command '(copy-to-register
                                                                             append-to-register
                                                                             prepend-to-register
                                                                             copy-rectangle-to-register))))
          (modelinepos-rect-p                    (member this-command '(copy-rectangle-to-register))))
      ad-do-it))

  ;; Emacs 23-24.3 - no `register-read-with-preview'.
  (defadvice copy-to-register (around bind-modelinepos-region-acting-on activate)
    "\(Not used for Emacs 22.)"
    (interactive
     (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
           (modelinepos-region-acting-on          (or (use-region-p)
                                                      (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))))
       (list (read-char "Copy to register: ")
             (region-beginning)
             (region-end)
             current-prefix-arg)))
    ad-do-it)

  (defadvice append-to-register (around bind-modelinepos-region-acting-on activate)
    "\(Not used for Emacs 22.)"
    (interactive
     (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
           (modelinepos-region-acting-on          (or (use-region-p)
                                                      (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))))
       (list (read-char "Append to register: ")
             (region-beginning)
             (region-end)
             current-prefix-arg)))
    ad-do-it)

  (defadvice prepend-to-register (around bind-modelinepos-region-acting-on activate)
    "\(Not used for Emacs 22.)"
    (interactive
     (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
           (modelinepos-region-acting-on          (or (use-region-p)
                                                      (and (boundp 'isearchp-reg-beg) isearchp-reg-beg))))
       (list (read-char "Prepend to register: ")
             (region-beginning)
             (region-end)
             current-prefix-arg)))
    ad-do-it))


;;; Functions from `rect.el'.

(defadvice string-rectangle (around bind-modelinepos-region-acting-on activate)
  (interactive
   (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
         (modelinepos-region-acting-on          (or (use-region-p)
                                                    (and (boundp 'isearchp-reg-beg) isearchp-reg-beg)))
         (modelinepos-rect-p                    t))
     (make-local-variable 'rectangle--string-preview-state)
     (make-local-variable 'rectangle--inhibit-region-highlight)
     (let* ((buf                                  (current-buffer))
            (win                                  (and (eq (window-buffer) buf) (selected-window)))
            (start                                (region-beginning))
            (end                                  (region-end))
            (rectangle--string-preview-state      `(nil ,start ,end))
            ;; Rectangle-region highlighting does not work well in the presence of preview overlays.
            ;; We could try to make it work better, but it is easier to just disable it temporarily.
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
                                        (or (car string-rectangle-history) ""))
                                nil 'string-rectangle-history (car string-rectangle-history)))
               (read-string (format "String rectangle (default %s): "
                                    (or (car string-rectangle-history) ""))
                            nil 'string-rectangle-history (car string-rectangle-history)))))))
  ad-do-it)

(defadvice string-insert-rectangle (around bind-modelinepos-region-acting-on activate)
  (interactive
   (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
         (modelinepos-region-acting-on          (or (use-region-p)
                                                    (and (boundp 'isearchp-reg-beg) isearchp-reg-beg)))
         (modelinepos-rect-p                    t))
     (barf-if-buffer-read-only)
     (list (region-beginning)
           (region-end)
           (read-string (format "String insert rectangle (default %s): " (or (car string-rectangle-history) ""))
                        nil 'string-rectangle-history (car string-rectangle-history)))))
  ad-do-it)

(when (fboundp 'rectangle-number-lines) ; Emacs 24+
  (defadvice rectangle-number-lines (around bind-modelinepos-region-acting-on activate)
    (interactive
     (let ((icicle-change-region-background-flag  nil) ; Inhibit changing face `region' during minibuffer input.
           (modelinepos-region-acting-on          (or (use-region-p)
                                                      (and (boundp 'isearchp-reg-beg) isearchp-reg-beg)))
           (modelinepos-rect-p                    t))
       (if current-prefix-arg
           (let* ((start     (region-beginning))
                  (end       (region-end))
                  (start-at  (read-number "Number to count from: " 1)))
             (list start end
                   start-at
                   (read-string "Format string: " (rectangle--default-line-number-format start end start-at))))
         ;; (redisplay 'FORCE) (sit-for 0.7)
         (list (region-beginning) (region-end) 1 nil))))
    ad-do-it))

(when (fboundp 'rectangle-mark-mode)    ; Emacs 24.4+
  (defadvice rectangle-mark-mode (after bind-modelinepos-region-acting-on activate)
    (setq modelinepos-region-acting-on  rectangle-mark-mode
          modelinepos-rect-p            rectangle-mark-mode)  
    (redisplay 'force)))

(when (fboundp 'cua-rectangle-mark-mode)    ; Emacs 24.4+, but see Emacs bug #18047
  (defadvice cua-rectangle-mark-mode (after bind-modelinepos-region-acting-on activate)
    (setq modelinepos-region-acting-on  cua-rectangle-mark-mode
          modelinepos-rect-p            cua-rectangle-mark-mode)  
    (redisplay 'force)))


;;; Functions from `isearch.el' (loaded by default, with no `provide').

;; Library `isearch+.el' lets you restrict Isearch to the active region.

(defadvice isearch-mode (before bind-modelinepos-region-acting-on activate)
  "\(Used only for Emacs 24.3 and later.)"
  (setq modelinepos-region-acting-on  (and (boundp 'isearchp-restrict-to-region-flag)
                                           isearchp-restrict-to-region-flag
                                           (use-region-p)))
  (add-hook 'isearch-mode-end-hook  (lambda () (setq modelinepos-region-acting-on  nil))))

;; Transfer the region restriction and its mode-line highlighting from Isearch to the replacement command.
(defadvice isearch-query-replace (around bind-modelinepos-region-acting-on activate)
  "\(Used only for Emacs 24.3 and later.)"
  (interactive
   (progn
     (setq modelinepos-region-acting-on  (and (boundp 'isearchp-restrict-to-region-flag)
                                              (or (use-region-p)
                                                  (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))))
     (when (boundp 'isearchp-restrict-to-region-flag)
       (add-hook 'isearch-mode-end-hook
                 `(lambda () (setq modelinepos-region-acting-on  ',modelinepos-region-acting-on))))
     (list current-prefix-arg)))
  (unwind-protect
       (progn
         (when (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg ; Activate region used for Isearch.
                    (boundp 'isearchp-reg-end)  isearchp-reg-end
                    (boundp 'isearchp-restrict-to-region-flag)
                    isearchp-restrict-to-region-flag)
           (goto-char isearchp-reg-beg)
           (push-mark isearchp-reg-end t 'ACTIVATE))
         ad-do-it)
    (when (boundp 'isearchp-restrict-to-region-flag)
      (remove-hook 'isearch-mode-end-hook
                   `(lambda () (setq modelinepos-region-acting-on  ',modelinepos-region-acting-on))))))

;; Transfer the region restriction and its mode-line highlighting from Isearch to the replacement command.
(defadvice isearch-query-replace-regexp (around bind-modelinepos-region-acting-on activate)
  "\(Used only for Emacs 24.3 and later.)"
  (interactive
   (progn
     (setq modelinepos-region-acting-on  (and (boundp 'isearchp-restrict-to-region-flag)
                                              (or (use-region-p)
                                                  (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg))))
     (when (boundp 'isearchp-restrict-to-region-flag)
       (add-hook 'isearch-mode-end-hook
                 `(lambda () (setq modelinepos-region-acting-on  ',modelinepos-region-acting-on))))
     (list current-prefix-arg)))
  (unwind-protect
       (progn
         (when (and (boundp 'isearchp-reg-beg)  isearchp-reg-beg ; Activate region used for Isearch.
                    (boundp 'isearchp-reg-end)  isearchp-reg-end
                    (boundp 'isearchp-restrict-to-region-flag)
                    isearchp-restrict-to-region-flag)
           (goto-char isearchp-reg-beg)
           (push-mark isearchp-reg-end t 'ACTIVATE))
         ad-do-it)
    (when (boundp 'isearchp-restrict-to-region-flag)
      (remove-hook 'isearch-mode-end-hook
                   `(lambda () (setq modelinepos-region-acting-on  ',(and (boundp 'isearchp-reg-beg)
                                                                     isearchp-reg-beg)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'modeline-posn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modeline-posn.el ends here
