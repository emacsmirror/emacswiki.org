;;; fill-column-indicator.el --- indicate the fill column with a thin line

;; Copyright (c) 2011 Alp Aker 

;; Author: Alp Aker <aker@pitt.edu>
;; Version: 0.29
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Many modern editors and IDEs can graphically indicate the location of the
;; fill column, either by shading the area to the right of the fill column or
;; by drawing a thin line (in design parlance, a `rule') down the length of
;; the editing window.  Fill-column-indicator implements both of these
;; facilities in Emacs. 

;; Installation
;; ============

;; Put this file in your load path and put 
;; 
;;   (require 'fill-column-indicator)
;;
;; in your .emacs.  

;; Usage
;; =====

;; To toggle graphical indication of the fill column in a buffer, use the
;; command `fci-mode'.

;; Fill-column-indicator has two modes of operation:

;; (1) In shading mode, it shades the portion of the window to the right of
;;     the fill column.  This is the default.

;; (2) In rule mode, it draws a thin line at the fill column. (For rule mode
;;     to operate correctly, some font configuration may be required.  This
;;     is especially likely under v22.  See below, under Troubleshooting.)

;; The mode of operation is controlled by the variable `fci-style'; the
;; recognized values are 'shading and 'rule.  If you'd like fci-mode to use a
;; rule by default, put
;;
;;   (setq fci-style 'rule) 
;;
;; in your .emacs.  (Buffer-local values of fci-style are supported.)

;; To change to the color of the shading, adjust the attributes of
;; `fcr-shading-face'.  To change the color of the rule, set `fcr-rule-color'
;; to the desired color.

;; Some further considerations to be aware of:

;; o Fci-mode uses the value of fill-column in effect when it is enabled.  If
;;   you change the fill column using `set-fill-column', it will
;;   automatically adjust the fill-column indicator.  If you change the value
;;   of fill-column manually, then you must reset fci-mode to update the
;;   indicator.

;; o Fci-mode will not work on character terminals. 

;; o Under v23 and later, it is recommended that `line-move-visual' be set to
;;   nil while using fci-mode.  By default, fci-mode sets it to nil when
;;   evnabled and restores it to its previous value when disabled.  This
;;   feature can be turned off by putting
;;
;;     (setq fci-handle-line-move-visual nil)
;;
;;   in your .emacs.  (It is recommended that you not do this.)

;; Customization
;; =============

;; See the documentation of `fci-style', `fci-shading-face',
;; `fci-rule-character', `fci-rule-color', `fci-fallback-character', and
;; `fci-handle-line-move-visual'.  These options are all available via the
;; Customization menu (under Convenience).

;; Troubleshooting
;; ===============

;; o If the fill-column indication is misaligned on some lines but otherwise
;;   looks normal, then you're most likely not displaying the buffer contents
;;   with a monospaced font.  Check whether one of the lines in question has
;;   a non-ascii character that's wider or shorter than the normal character
;;   width.  Also, be aware that certain font-lock themes set some faces so
;;   that they look monospaced but aren't quite so.

;; o The buffer will look odd if you have `truncate-lines' set to nil.  If
;;   you have a suggestion as to what the right behavior should be in this
;;   case, please let me know.

;; o If the rule is a dashed (rather than uninterrupted) line, and the buffer
;;   is unibyte, then that is the intended behavior.

;; o Any of the following is most likely due to a font configuration problem:
;; 
;;   - Enabling fci-mode increases line spacing.
;;   - The rule appears dashed in multibyte buffers.
;;   - The rule is too thick (has the width of a normal column).
;;   - The rule appears as a series of boxes.
;;
;;   See the next section for solutions. 

;; Font Selection
;; ==== =========

;; The following remarks are pitched at an introductory level, for
;; inexperienced users; apologies if others find that tedious.

;; To draw a rule, fill-column-indicator uses non-ascii whitespace
;; characters; it depends on Emacs to find a font that can display them and
;; that has the same character height as the default font in use.  Several of
;; the problems described in the previous section arise when when an
;; inappropriate font is selected.  

;; A quick and sure (but non-ideal) fix is to draw the rule using ascii
;; characters, by setting `fci-rule-character' to ?| (for example).

;; If you'd like the rule to be properly drawn with non-ascii characters, the
;; solution is explicitly to request a particular font family.  To see which
;; font families Emacs recognizes, evaluate the following form:
;;
;;   (insert (mapconcat #'identity (font-family-list) "\n"))
;;
;; On v22 use:
;;
;;   (insert (mapconcat #'car (x-font-family-list) "\n"))

;; You can then ask fill-column-indicator to use a specific font by setting
;; the variable `fci-font' to the name of a font family (the value should be
;; a string). You might have to try various options before finding one that
;; displays well on screen.

;; If explicitly specifying the font family doesn't work, then it might be
;; that the fontset in use specifies an inappropriate font for thin space
;; characters, in which case the fontset specification needs to be modified
;; directly.  Get a list of possible font choices (distinct from font
;; *families*) by evaluating the form:
;;
;;   (insert (mapconcat #'identity (x-list-fonts "-*-iso10646-1") "\n"))
;;
;; When there are multiple options for a font, choose the one that is
;; "medium-normal-normal" or "medium-r-normal".

;; Now tell the fontset you're using to use that font for a thin space:
;;
;;   (set-fontset-font "fontset-foo" #x200A
;;      "-apple-DejaVu_Sans-medium-normal-normal--0-0-0-0-m-0-iso10646-1")
;;
;; On v22, use 342378 instead of x200A. (If you don't know which fontset
;; to modify, use M-x list-fontsets to see the possibilities.)

;; Known Issues
;; ===== ======

;; o The rule extends only to end of the buffer contents (as opposed
;;   to running the full length of the editing window).

;; o If the buffer contents do not end in a newline, then the rule
;;   extends only to the penultimate line.

;; Todo
;; ====

;; o Play well with outline minor mode and other forms of folding
;;   (nxml-outln, etc.).

;;; Code:

;;; Version 

(defconst fci->22
  (cond
   ((< 22 emacs-major-version)
    t)
   ((= 22 emacs-major-version)
    nil)
   (t
    (error "Fill-column-indicator requires version 22 or later")))
  "Records the version fill-column-indicator is running on.")

;;; Customization Menu Options

(defgroup fill-column-indicator nil
 "Graphically indicate the fill-column."
 :tag "Fill-Column Indicator"
 :group 'convenience)

(defcustom fci-style 'shading
  "How fci-mode should indicate the fill-column.
If `shading', each line will be shaded past the fill column.  If
`rule', a thin rule (line) will be drawn at the fill column.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :tag "Fill-Column Style"
  :group 'fill-column-indicator
  :type '(choice (symbol :tag "Shading" 'shading)
                 (symbol :tag "Rule (a line)" 'rule)))

(defcustom fci-handle-line-move-visual (< 22 emacs-major-version)
 "Whether fci-mode should set line-move-visual to nil while enabled.
If non-nil, fci-mode will set line-move-visual to nil in buffers in which it  
is enabled, and restore line-move-visual to its previous value when disabled. 

Leaving this option set to the default value is recommended."
 :group 'fill-column-indicator
 :tag "Locally set line-move-visual to nil during fci-mode"
 :type 'boolean)

(defcustom fci-rule-color nil
 "Color used to indicate the fill-column with a rule.
If nil, fill-column-indicator makes a sensible choice.  On v23 and
later, buffer-local values are supported.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
 :group 'fill-column-indicator
 :tag "Fill-column rule color"
 :type '(choice (const :tag "Let fci-mode choose" nil)
                (color :tag "Specify a color")))

(defface fci-shading-face '((((background light)) (:background "#e5e5e5")) 
                            (((background dark)) (:background "#7f7f7f")))
 "Face used by fci-mode to color the area past the fill column."
 :group 'fill-column-indicator
 :tag "Fill-column shading color")

(defcustom fci-rule-character nil
 "Character used to indicate the fill-column rule.
If nil, fill-column-indicator tries various defaults, falling
back on `fci-fallback-character' if all else fails.  Buffer-local
values are supported.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
 :group 'fill-column-indicator
 :tag "Fill-column rule string"
 :type '(choice (const :tag "Let fci-mode choose" nil)
                (character :tag "Specify a character")))

(defcustom fci-fallback-character ?|
 "The character fill-column-indicator uses in extremis.
If `fci-indicator-character' is nil and none of the default
options are displayable, then this character is used instead.  In
unibyte buffers it is always used.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
 :group 'fill-column-indicator
 :tag "Fill-column rule fallback character"
 :type 'character)

;;; Other Options

;; For v23-4, internal encoding is utf-8.  For v22, use mule-unicode. 
(defconst fci-defaults (if fci->22
                          '(#x200A #x2009 #x2008)
                        '(342378 342377 342376))
 "Characters fill-column-indicator tries by default to display the rule.
If `fci-rule-character' is non-nil, that value is used instead. The
value of this variable should be a list of characters.

Changes do not take effect until the mode function `fci-mode' is
run.

For reference, the Unicode names of the characters in the default
value are 'hair space', 'thin space', and 'punctuation space'.")

(defconst fci-font nil
 "Font family used to display the fill-column rule.
If non-nil, it should be a string that is a valid value for
the :family attribute of a face.

Changes do not take effect until the mode function `fci-mode' is
run.")

;;; Internal Variables

;; Stores the value of fill-column for our use.  
(defvar fci-column nil)
(make-variable-buffer-local 'fci-column)

;; If we nix line-move-visual in a buffer, we save its prior state here.
(defvar fci-saved-line-move-visual nil)
(make-variable-buffer-local 'fci-saved-line-move-visual)

;; Records whether fci-mode needs to restore an already existing buffer
;; display table.
(defvar fci-prior-display-table t)
(make-variable-buffer-local 'fci-prior-display-table)

;; The string we use to hide the rule on lines that extend past the
;; fill-column.  For v23-4, given in utf-8; for v22, in mule-unicode.
(defconst fci-fake-newline
  (if fci->22
      (char-to-string #xE001)
    (char-to-string 315425)))

(defface fci-rule-face '((t ()))
 "Face used by fci-mode to draw the rule.
Don't set the attributes of this face directly.  Use the
variables `fci-rule-color' and `fci-font' instead.")

;; Used by fci-after-change-function to call the right redrawing function
;; (shading or rule).  Set when fci-mode is called.
(defvar fci-put-overlays-function nil)
(make-variable-buffer-local 'fci-put-overlays-function)

;; After initialization, holds the propertized string used to display the
;; end-of-line glyphs by the functions that deal with the stretch glyph
;; display bug.
(defvar fci-secondary-line-end nil)
(make-variable-buffer-local 'fci-secondary-line-end)

;;; Mode Definition

;; Overview of how it works: The basic strategy is to set the buffer's local
;; display table so that the newline character displays as a space followed
;; either by a rule glyph and the newline (in rule mode) or by a shaded
;; newline (in shading mode).  Each newline that falls short of the
;; fill-column then receives an overlay with a space-width display
;; specification that stretches the space character by the amount necessary
;; to make the indicator appear at the fill column.

(define-minor-mode fci-mode
  "Toggle fci mode on and off.
In fci mode, a thin line (a `rule') is drawn in the editing
window to indicate the location of the fill column.

With prefix ARG, turn fci mode on if and only if ARG is positive.

The following options are available via the customization menu:
`fci-handle-line-move-visual', `fci-rule-color', `fci-rule-character',
and`fci-fallback-character'.

For further options, as well as tips for troubleshooting
unexpected behavior, see the comments in fill-column-indicator.el."

  nil nil nil

      (if fci-mode
          ;; Enabling
          (progn
            ;; If we throw an error later in the initialization process and
            ;; disable the mode, we need the data recorded by the next two
            ;; forms to restore the buffer state, so they come first.
            (unless buffer-display-table
              (setq buffer-display-table (make-display-table)
                    fci-prior-display-table nil))
            (when (and fci-handle-line-move-visual
                       (boundp 'line-move-visual))
              (make-local-variable 'line-move-visual)
              (setq fci-saved-line-move-visual line-move-visual
                    line-move-visual nil))
            (setq fci-column fill-column)
            (aset buffer-display-table (string-to-char fci-fake-newline) [10])
            (cond 
             ((eq fci-style 'rule)
              (let ((char (fci-make-rule-char)))
                (aset buffer-display-table
                      10
                      (vector 32 (make-glyph-code char 'fci-rule-face)  10))
                (fci-set-face-attributes char)
                (setq fci-put-overlays-function #'fci-put-overlays-rule
                      fci-put-secondaries-function #'fci-put-secondaries-rule
                      fci-secondary-line-end (concat 
                                              (propertize (char-to-string char) 
                                                          'face
                                                          'fci-rule-face)
                                              fci-fake-newline))))
             ((eq fci-style 'shading)
              (aset buffer-display-table 
                    10 
                    (vector 32 (make-glyph-code 10 'fci-shading-face)))
              (setq fci-put-overlays-function #'fci-put-overlays-shading
                    fci-put-secondaries-function #'fci-put-secondaries-shading
                    fci-secondary-line-end (propertize fci-fake-newline
                                                       'face
                                                       'fci-shading-face)))
             (t
              (fci-mode -1)
              (error "Unrecognized value of `fci-style'")))
            (add-hook 'after-change-functions 'fci-after-change-function nil t)
            (ad-enable-advice 'set-fill-column 'after 'fill-column-indicator)
            (ad-activate 'set-fill-column)
            ;; In case we were already in fci-mode and are resetting the
            ;; rule, clear out any existing overlays.
            (fci-delete-overlays-buffer)
            (fci-put-overlays-buffer)
            ;; The Nextstep (Mac OS with Cocoa API) port's rendering of
            ;; stretch glyphs is buggy, so we need a workaround for that
            ;; case.
            (when (eq window-system 'ns)
              (add-hook 'post-command-hook 'fci-post-command-check nil t)
              (add-hook 'window-scroll-functions 'fci-window-scroll-check nil t)
              (when (< 0 (window-hscroll))
                (fci-put-secondaries (window-start) (window-end)))))

        ;; Disabling
        (if fci-prior-display-table
            (progn
              (aset buffer-display-table 10 nil)
              (aset buffer-display-table (string-to-char fci-fake-newline) nil))
          (setq buffer-display-table nil))
        (setq fci-column nil)
        (when fci-handle-line-move-visual
          (setq line-move-visual fci-saved-line-move-visual
                fci-saved-line-move-visual nil))
        (ad-disable-advice 'set-fill-column 'after 'fill-column-indicator)
        (ad-activate 'set-fill-column)
        (remove-hook 'after-change-functions 'fci-after-change-function t)
        (remove-hook 'post-command-hook 'fci-post-command-check  t)
        (remove-hook 'window-scroll-functions 'fci-window-scroll-check  t)
        (fci-delete-overlays-buffer)))

;;; Initialization

;; Called by the mode function to set fci-glpyh.
(defun fci-make-rule-char ()
 (or
  (if fci-rule-character
      ;; User specified a char.  Check to see that it's the right type and
      ;; clean up if not.
      (if (characterp fci-rule-character)
          fci-rule-character
        (fci-mode -1)
        (error "Value of `fci-rule-character' is not a character"))
    ;; Otherwise, try the defaults.
    (catch 'result
      (dolist (c fci-defaults)
        (when (char-displayable-p c)
          (throw 'result c)))))
  fci-fallback-character))
 
(defun fci-set-face-attributes (char)
 (let* ((color (if fci-rule-color
                   ;; User specified a color.  Check that it's valid.
                   (if (color-defined-p fci-rule-color)
                       fci-rule-color
                     (fci-mode -1)
                     (error 
                      "Value of `fci-rule-color' is not a recognized color"))
                 ;; Otherwise, choose an appropriate color.  
                 (if (equal (frame-parameter (selected-frame) 'background-mode)
                            'light)
                     "#cccccc" 
                   "#7f7f7f")))
        ;; If the rule char is whitespace, color is a background
        ;; color.  Otherwise, it's a foreground color.
        (color-prop (if (= 32 (char-syntax char))
                        `(:background ,color :foreground unspecified)
                      `(:foreground ,color :background unspecified)))
        ;; If fci-font is specified, try to use that.
        (family (if fci-font
                    (if (assoc fci-font
                               (face-valid-attribute-values :family))
                        `(:family ,fci-font)
                      (fci-mode -1)
                      (error 
                       "Value of `fci-font' is not a valid font family"))))
        ;; Avoid inheriting these properties from font-lock.
        (weight-slant '(:weight normal :slant normal))
        ;; All together now.
        (props (append color-prop family weight-slant)))
   ;; Under 23-4, locally set fci-face to these props.  Under 22,
   ;; face-remapping is not supported, so simply set the face attributes.
   (if fci->22
       (face-remap-set-base 'fci-rule-face props)
     (apply #'set-face-attribute 'fci-rule-face nil props))))

;;; Functions That Call Setting and Unsetting

;; The main entry-point.  This is put in after-change-functions and locally
;; redraws the indicator after each buffer change.  Note that we redraw an
;; extra preceding line.  Motivation: at the beginning of a line,
;; insert-before-markers will grab the end marker of the overlay on the
;; previous line, in which case we need to reset the previous line's overlay
;; as well.  Unconditionally redoing the previous line is the fastest way to
;; handle that case.  (Using a hook on the overlay is conceptually tidier
;; but incurs the overhead of multiple extra lisp function calls.)
(defun fci-after-change-function (start end unused)
  (save-match-data
    (save-excursion
      ;; Make sure our bounds span at least whole lines.
      (goto-char start)
      (setq start (line-beginning-position 0))
      (goto-char end)
      (setq end (line-beginning-position 2))
      ;; Clear any existing overlays.
      (fci-delete-overlays-region start end)
      ;; Then set the fill-column indicator in that region.
      (funcall fci-put-overlays-function start end))))

(defun fci-put-overlays-buffer ()
  (overlay-recenter (point-max))
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (funcall fci-put-overlays-function (point-min) (point-max))))))

(defun fci-delete-overlays-buffer ()
 (save-excursion
   (save-restriction
     (widen)
     (fci-delete-overlays-region (point-min) (point-max)))))

;; Automatically reset the rule after changes to fill-column.
(defadvice set-fill-column (after fill-column-indicator)
 (when (and ad-return-value
            fci-mode)
   (fci-mode 1)))

;;; Functions that Set and Unset the Rule

(defun fci-delete-overlays-region (start end)
 (mapc #'(lambda (x) (if (eq (overlay-get x 'category) 'fci)
                         (delete-overlay x)))
       (overlays-in start end)))

(defun fci-put-overlays-rule (start end)
 (goto-char start)
 (let (o)
   (while (search-forward "\n" end t)
     (goto-char (match-beginning 0))
     (setq o (make-overlay (match-beginning 0)
                           (match-end 0)))
     (overlay-put o 'category 'fci)
     (if (< (current-column) fci-column)
         (overlay-put o 
                      'display
                      (list 'space-width (- fci-column (current-column))))
       (overlay-put o 'display fci-fake-newline))
     (goto-char (match-end 0)))))

(defun fci-put-overlays-shading (start end) 
  (goto-char start)
  (let (o)
    (while (search-forward "\n" end t)
      (goto-char (match-beginning 0))
      (if (< (current-column) fci-column)
          (progn
            (setq o (make-overlay (point) (match-end 0)))
            (overlay-put o
                         'display
                         (list 'space-width (- fci-column (current-column)))))
        (move-to-column fci-column)
        (setq o (make-overlay (point) (match-end 0)))
        (overlay-put o 'face 'fci-shading-face))
      (overlay-put o 'category 'fci)
      (goto-char (match-end 0)))))

;;; Hack for Display Bug

;; A bug in in the rendering of stretch glyphs on the NS (Mac OS X with Cocoa
;; API) port can cause display artifacts to appear when horizontal scrolling
;; is in effect.  To avoid this, we use the following hack.  When a window is
;; horizontally scrolled, secondary overlays are placed over the
;; space-[rule-]newline glyphs on lines that appear within the window.  These
;; overlays have a display spec that replaces the newline glyphs with a
;; string of the same visual appearance, designed to avoid triggering the
;; rendering bug.  When horizontal scrolling returns to normal we delete
;; any secondaries in the vicinity of the window content; remaining
;; secondaries are deleted during the next idle period. (Having many overlays
;; of this secondary type can slow vertical navigation.)

;; For each fci-mode buffer, holds the timer object used to clean out
;; secondary overlays.
(defvar fci-idle-timer nil)
(make-variable-buffer-local 'fci-idle-timer)

;; For each fci-mode buffer, holds the list of windows in which that buffer
;; is horizontally scrolled.
(defvar fci-hscroll-list nil)
(make-variable-buffer-local 'fci-hscroll-list)

;; Set by fci-post-command-check to delete stray secondaries during idle
;; time.
(defun fci-idle-delete-secondaries (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (fci-delete-secondaries (point-min) (point-max))))
  (setq fci-idle-timer nil))

(defsubst fci-delete-secondaries (s e)
 (mapc #'(lambda (x) (if (overlay-get x 'fci2)
                         (delete-overlay x)))
       (overlays-in s e)))

(defun fci-put-secondaries (start end)
  (save-excursion
    (goto-char start)
    (let (o)
      ;; Because of the way overlay lookup works, it's faster just to delete
      ;; all the secondaries between start and end and reset them all, rather
      ;; than checking each line as we go to see if one is needed.
      (fci-delete-secondaries start end)
      (while (search-forward "\n" end t)
        (goto-char (match-beginning 0))
        (when (< (current-column) fci-column)
          (setq o (make-overlay (match-beginning 0) (match-end 0)))
          (overlay-put o 'category 'fci)
          (overlay-put o 'fci2 t)
          (overlay-put o 'priority 1)
          (overlay-put o 'display (concat 
                                   (make-string (- fci-column (current-column))
                                                32)
                                   fci-secondary-line-end)))
        (goto-char (match-end 0))))))

(defun fci-window-scroll-check (win start)
 (when (memq (selected-window) fci-hscroll-list)
   (fci-put-secondaries start (line-beginning-position (window-height)))))

(defun fci-post-command-check ()
  (if (memq (selected-window) fci-hscroll-list)
      ;; We can't depend on window-hscroll being up-to-date when this is
      ;; called.  So we err on the side of caution when removing the
      ;; secondaries.
      (if (and (= 0 (window-hscroll))
               (< (current-column) (- (window-width) hscroll-margin)))
          (progn
            (setq fci-hscroll-list (delq (selected-window) fci-hscroll-list))
            ;; If no windows displaying the buffer are hscrolled, delete the
            ;; secondaries.
            (unless fci-hscroll-list
              (fci-delete-secondaries (max (point-min) (- (window-start) 1000))
                                      (min (point-max) (+ (window-end) 1000)))
              (setq fci-idle-timer
                    (run-with-idle-timer 3
                                         nil
                                         'fci-idle-delete-secondaries
                                         (current-buffer)))))
        ;; Some kind of further bug prevents full return from hscrolling in a
        ;; few cases, so we have to manually return.
        (when (and auto-hscroll-mode
                   (< (point) (+ (line-beginning-position) 
                                 (window-hscroll)
                                 hscroll-margin)))
          (set-window-hscroll (selected-window) 0)))
    (when (or (< 0 (window-hscroll))
            ;; Again, window-hscroll might not be accurate, so be liberal 
            ;; when deciding whether to place the secondaries.
            (<= (- (window-width) hscroll-margin) (current-column)))
      (setq fci-hscroll-list (cons (selected-window) fci-hscroll-list))
      (when fci-idle-timer
        (cancel-timer fci-idle-timer)
        (setq fci-idle-timer nil))
      (fci-put-secondaries (window-start) (window-end)))))

(provide 'fill-column-indicator)

;;; fill-column-indicator.el ends here
