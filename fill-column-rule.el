;;; fill-column-rule.el --- indicate the fill column with a thin line

;; Copyright (c) 2011 Alp Aker

;; Author: Alp Aker <aker@pitt.edu>
;; Version: 0.21
;; Keywords: convenience, tools

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

;; Many modern editors and IDEs indicate the location of the fill column with
;; a thin line (in design parlance, a `rule') running the length of the
;; editing window.  Fill-column-rule implements this facility in Emacs.

;; Installation
;; ============

;; Put this file somewhere in your load path and put
;;
;;   (require 'fill-column-rule)
;;
;; in your .emacs.  To toggle display of the fill-column rule in a buffer,
;; use the command `fcr-mode'.

;; Under v23 and later, it is recommended that you set line-move-visual to
;; nil when using fcr-mode.  To ensure that that happens add the following to
;; your .emacs:
;;
;;   (setq fcr-handle-line-move-visual t)
;;
;; Then line-move-visual will be automatically set to nil when fcr-mode is
;; enabled and restored to its previous value when fcr-mode is disabled.

;; Customization
;; =============

;; See the documentation of the variables `fcr-handle-line-move-visual',
;; `fcr-rule-character', `fcr-color', and `fcr-fallback-character', or use the
;; Customization menu (under Convenience).

;; Troubleshooting
;; ===============

;; o If the fill-column rule is misaligned on some lines but otherwise
;;   looks normal, then you're most likely not displaying the buffer
;;   contents with a monospaced font.  Note that certain font-lock
;;   themes set some faces so that they look monospaced aren't quite
;;   so.  You can tell if that's the source of the problem by turning
;;   off font-lock and resetting fcr-mode.

;; o If the fill-column rule is a dashed (rather than uninterrupted) line,
;;   and the buffer is unibyte, then that is the intended behavior.  If the
;;   buffer is multibyte, the issue is one of font selection; see the next
;;   section.

;; o If activating fcr-mode increases the line spacing in the window, then
;;   you need to change the font options for fcr-mode. See the next section.

;; o If the fill-column rule looks too thick (say, has the width of a normal
;;   column) or is a series of boxes, then the problem is most likely one of
;;   font selection.  Again, see the next section.

;; Font Selection
;; ==== =========

;; The following remarks are pitched at an introductory level, for
;; inexperienced users; apologies if others find that tedious.

;; Fill-column-rule draws the rule using non-ascii whitespace characters; it
;; depends on Emacs to find a font that can display them and that has the
;; same character height as the default font in use.  Several of the problems
;; described in the previous section arise when it can't do so.

;; The solution is explicitly to request a particular font family.  To see
;; which font families Emacs recognizes, evaluate the following form:
;;
;;   (insert (mapconcat 'identity (font-family-list) "\n"))
;;
;; On v22 use:
;;
;;   (insert (mapconcat 'car (x-font-family-list) "\n"))

;; You can then ask fill-column-rule to use a specific font by setting the
;; variable `fcr-font' to the name of a font family (the value should be a
;; string). You might have to try various options before finding one that
;; displays well on screen.

;; If explicitly specifying the font family doesn't work, then it might be
;; that the fontset in use specifies an inappropriate font for thin space
;; characters, in which case the fontset specification needs to be modified
;; directly.  Get a list of possible font choices (distinct from font
;; *families*) by evaluating the form:
;;
;;   (insert (mapconcat 'identity (x-list-fonts "-*-iso10646-1") "\n"))
;;
;; When there are multiple options for a font, choose the one that is
;; "medium-normal-normal" or "medium-r-normal".

;; Now tell the fontset to use that font for a thin space:
;;
;;   (set-fontset-font "fontset-foo" #x200A
;;      "-apple-DejaVu_Sans-medium-normal-normal--0-0-0-0-m-0-iso10646-1")
;;
;; On v22, use 342378 instead of #x200A. (If you don't know which fontset to
;; modify, use M-x list-fontsets to see the possibilities.)

;; Known Issues
;; ===== ======

;; o The rule extends only to end of the buffer contents (as opposed
;;   to running the full length of the editing window).

;; o If the buffer contents do not end in a newline, then the rule
;;   extends only to the penultimate line.

;; Todo
;; ====

;; o Play well with outline minor mode and other forms of folding
;;   (nxhtml mode, etc.).

;;; Code:

(unless window-system
  (error "Fill-column-rule only works on window systems"))

;;; Version 

(defconst fcr->22
  (cond
   ((string-match "^2[34]" emacs-version)
    t)
   ((string-match "^22" emacs-version)
    nil)
   (t
    (error "Fill-column-rule requires version 22 or later")))
  "Records whether fill-column-rule is running on v22 or on a more recent version.")

;;; Customization Menu Options

(defgroup fill-column-rule nil
 "Indicate the fill-column with a thin line (a `rule')."
 :tag "Fill-Column Rule"
 :group 'tools
 :group 'convenience)

(defcustom fcr-handle-line-move-visual nil
 "Whether fcr-mode should set line-move-visual to nil while enabled.
If non-nil, fcr-mode will set line-move-visual to nil in buffers in which it
is enabled, and restore line-move-visual to its previous value when disabled.

This option is set to nil by default, but it is recommended that it be t."
 :group 'fill-column-rule
 :tag "Locally set line-move-visual to nil during fcr-mode"
 :type 'boolean)

(defcustom fcr-color nil
 "Color used for the fill-column rule.
If nil, fill-column-rule makes a sensible choice.  On v23 and
later, buffer-local values are supported.

Changes to this variable do not take effect until the mode
function `fcr-mode' is run."
 :group 'fill-column-rule
 :tag "Fill-column rule color"
 :type '(choice (const :tag "Let fcr-mode choose" nil)
                (color :tag "Specify a color")))

(defcustom fcr-rule-character nil
 "Character used to indicate the fill-column rule.
If nil, fill-column-rule tries various defaults, falling back on
`fcr-fallback-character' if all else fails.

Changes to this variable do not take effect until the mode
function `fcr-mode' is run."
 :group 'fill-column-rule
 :tag "Fill-column rule string"
 :type '(choice (const :tag "Let fcr-mode choose" nil)
                (character :tag "Specify a character")))

(defcustom fcr-fallback-character ?|
 "The character fill-column-rule uses in extremis.
If `fcr-rule-character' is nil and none of the default options
are displayable, then this character is used instead.  It is always
used in unibyte buffers.

Changes to this variable do not take effect until the mode
function `fcr-mode' is run."
 :group 'fill-column-rule
 :tag "Fill-column rule fallback string"
 :type 'character)

;;; Other Options

;; For v23-4, internal encoding is utf-8.  For v22, use mule-unicode.
(defconst fcr-defaults (if fcr->22
                          '(#x200A #x2009 #x2008)
                        '(342378 342377 342376))
 "Characters fill-column-rule tries by default to display the rule.
If `fcr-rule-character' is non-nil, that value is used instead. The
value of this variable should be a list of characters.

Changes do not take effect until the mode function `fcr-mode' is
run.

For reference, the Unicode names of the characters in the default
value are 'hair space', 'thin space', and 'punctuation space'.")

(defconst fcr-font nil
 "Font family used to display the fill-column rule.
If non-nil, it should be a string that is a valid value for
the :family attribute of a face.")

;;; Internal Variables

;; If we nix line-move-visual in a buffer, we save its prior state here.
(defvar fcr-saved-line-move-visual nil)
(make-variable-buffer-local 'fcr-saved-line-move-visual)

;; Records whether fcr-mode needs to restore an already existing buffer
;; display table.
(defvar fcr-prior-buffer-display-table t)
(make-variable-buffer-local 'fcr-prior-buffer-display-table)

;; The string we use to hide the rule on lines that extend past the
;; fill-column.  For v23-4, given in utf-8; for v22, in mule-unicode.
(defconst fcr-fake-newline
  (if fcr->22
      (char-to-string #xE001)
    (char-to-string 315425)))

(defface fcr-face '((t ()))
 "Face used by fcr-mode to display the fill-column rule.
Don't set the attributes of this face directly.  Use the
variables `fcr-color' and `fcr-font' instead.")

;; See below, in the section Advised Mode-Specific Filling Functions.
(defvar fcr-fill-column nil)

;; After initialization, holds the propertized string used to display the
;; rule by the functions that deal with the horizontal scrolling bug.
(defvar fcr-rule-glyph nil)
(make-variable-buffer-local 'fcr-rule-glyph)

;;; Mode Definition

;; Overview of how it works: The core mechanism sets the buffer's local
;; display table to display the newline character as a sequence of three
;; glyphs--a space, the character that represents the rule, and the newline
;; itself.  Each newline that falls short of the fill-column then receives an
;; overlay with a space-factor display specification that stretches the space
;; character by the amount necessary to make the rule appear at the fill
;; column.  On lines that overshoot the fill-column, we don't want the rule
;; to appear at all, so we put an overlay with a replacing display
;; specification that makes a meaningless character from the Private Use Area
;; of the Unicode BMP appear instead of the newline; the local display table
;; is set so that this character displays as a newline, without the
;; space or rule glyphs.  (We also need a workaround for a display bug.  See
;; the last section of this file for an explanation.)

(define-minor-mode fcr-mode
 "Toggle fcr mode on and off.
In fcr mode, a thin line (a `rule') is drawn in the editing
window to indicate the location of the fill column.

With prefix ARG, turn fcr mode on if and only if ARG is positive.

The following options are available via the customization menu:
`fcr-handle-line-move-visual', `fcr-color', `fcr-rule-character',
and`fcr-fallback-character'.

For further options, as well as tips for troubleshooting
unexpected behavior, see the comments in fill-column-rule.el."

 nil nil nil

 (if fcr-mode
     ;; Enabling
     (progn
       ;; If we throw an error later in the initialization process and
       ;; disable the mode, we need the data recorded by the next two forms
       ;; to restore the buffer state, so they come first.
       (unless buffer-display-table
         (setq buffer-display-table (make-display-table)
               fcr-prior-display-table nil))
       (when (and fcr-handle-line-move-visual
                  (boundp line-move-visual))
         (make-local-variable 'line-move-visual)
         (setq fcr-saved-line-move-visual line-move-visual
               line-move-visual nil))
       (aset buffer-display-table (string-to-char fcr-fake-newline) [10])
       (let ((char (fcr-make-rule-char)))
         (aset buffer-display-table
               10
               (vector 32 (make-glyph-code char 'fcr-face)  10))
         (fcr-set-face-attributes char) 
         (setq fcr-rule-glyph (propertize (char-to-string char) 
                                          'face
                                          'fcr-face)))
       (add-hook 'after-change-functions 'fcr-after-change-function nil t)
       (ad-enable-regexp "fill-column-rule")
       (ad-activate-regexp "fill-column-rule")
       ;; In case we were already in fcr-mode and are resetting the
       ;; rule, clear out any existing overlays.
       (fcr-delete-overlays-buffer)
       (fcr-put-overlays-buffer)
       ;; These are for the hscroll hack.
       (add-hook 'post-command-hook 'fcr-post-command-check nil t)
       (add-hook 'window-scroll-functions 'fcr-window-scroll-check nil t))

   ;; Disabling
   (if fcr-prior-display-table
       ;; Note that we don't bother resetting the display table slot for the
       ;; fake newline, since the assumption is that we have free use of that
       ;; char.
       (aset buffer-display-table 10 nil)
     (setq buffer-display-table nil))
   (when fcr-handle-line-move-visual
     (setq line-move-visual fcr-saved-line-move-visual
           fcr-saved-line-move-visual nil))
   (remove-hook 'after-change-functions 'fcr-after-change-function t)
   (ad-disable-regexp "fill-column-rule")
   (ad-activate-regexp "fill-column-rule")
   (fcr-delete-overlays-buffer)
   (remove-hook 'post-command-hook 'fcr-post-command-check  t)
   (remove-hook 'window-scroll-functions 'fcr-window-scroll-check  t)))

;;; Initialization

;; Called by the mode function to set fcr-glpyh.
(defun fcr-make-rule-char ()
 (or
  (cond
   ;; User specified a char.
   (fcr-rule-character
    ;; Check to see that it's the right type and clean up if not.
    (if (characterp fcr-rule-character)
        fcr-rule-character
      (fcr-mode -1)
      (error "Value of `fcr-rule-character' is not a character")))
   ;; We're in a unibyte buffer.  Use the fallback character.
   ((not enable-multibyte-characters)
    fcr-fallback-character)
   ;; Otherwise, try to use one of defaults.
   (t
    (catch 'result
      (dolist (c fcr-defaults)
        (when (char-displayable-p c)
          (throw 'result c))))))
  fcr-fallback-character))

(defun fcr-set-face-attributes (char)
 (let* ((color (if fcr-color
                   ;; User specified a color.  If it's not
                   ;; recognized, object.
                   (if (color-defined-p fcr-color)
                       fcr-color
                     (fcr-mode -1)
                     (error "Value of `fcr-color' is not a recognized color"))
                 ;; No user color.  We're on a window system, so we probably
                 ;; have all the X11 gray colors; if not, the Emacs we're
                 ;; running is recent enough to remap them to some color
                 ;; that is supported.
                 (if (equal (frame-parameter (selected-frame)
                                             'background-mode)
                                'light)
                     "gray80" "gray50")))

        ;; If the rule char is a form of whitespace, color is a
        ;; background color.  Otherwise, it's a foreground color.
        (color-prop (if color
                        (if (= 32 (char-syntax char))
                            `(:background ,color :foreground unspecified)
                          `(:foreground ,color :background unspecified))))

        ;; If fcr-font is specified, try to use that.
        (family (if fcr-font
                    (if (assoc fcr-font
                               (face-valid-attribute-values :family))
                        `(:family ,fcr-font)
                      (fcr-mode -1)
                      (error "Value of `fcr-font' is not a valid font family"))))

        ;; Avoid inheriting these properties from font-lock.
        (weight-slant '(:weight normal :slant normal))

        ;; All together now.
        (props (append color-prop family weight-slant)))
   ;; Under 23-4, locally set fcr-face to these props.  Under 22,
   ;; face-remapping is not supported, so simply set the face attributes.
   (if fcr->22
       (face-remap-set-base 'fcr-face props)
     (apply 'set-face-attribute 'fcr-face nil props))))

;;; Functions That Call Setting and Unsetting

;; The main entry-point.  This is put in after-change-functions and
;; locally redraws the rule after each buffer change.
(defun fcr-after-change-function (start end unused)
 (save-excursion
   ;; Make sure our bounds span at least whole lines.
   (goto-char start)
   ;; NB: We move start backwards one more line than would seem
   ;; required.  Motivation: at the beginning of a line,
   ;; insert-before-markers will grab the end marker of the overlay on the
   ;; previous line, and so we need to reset the previous line's overlay as
   ;; well.  Unconditionally redoing the previous line is simplest and
   ;; fastest way to handle that case.  (Using a hook on the overlay is
   ;; conceptually tidier but incurs the overhead of multiple extra lisp
   ;; function calls.)
   (setq start (line-beginning-position 0))
   (goto-char end)
   (setq end (line-beginning-position 2))
   ;; Clear any existing overlays.
   (fcr-delete-overlays-region start end)
   ;; Then set the fill-column rule in that region.
   (fcr-put-overlays-region start end)))

(defun fcr-put-overlays-buffer ()
 (overlay-recenter (point-max))
 (save-excursion
   (save-restriction
     (widen)
     (fcr-put-overlays-region (point-min) (point-max))))
 (overlay-recenter (ceiling (* 0.5 (point-max)))))

(defun fcr-delete-overlays-buffer ()
 (save-excursion
   (save-restriction
     (widen)
     (fcr-delete-overlays-region (point-min) (point-max)))))

;;; Functions that Set and Unset the Rule

(defun fcr-delete-overlays-region (start end)
 (mapc (lambda (x) (if (eq (overlay-get x 'category) 'fcr)
                       (delete-overlay x)))
       (overlays-in start end)))

(defun fcr-put-overlays-region (start end)
 (goto-char start)
 ;; If the standard value of fill-column is shadowed, the value we
 ;; want is in fcr-fill-column (see the next section).
 (let ((col (or fcr-fill-column fill-column))
       o)
   (while (search-forward "\n" end t)
     (goto-char (match-beginning 0))
     (setq o (make-overlay (match-beginning 0)
                           (match-end 0)))
     (overlay-put o 'category 'fcr)
     (if (< (current-column) col)
         (overlay-put o
                      'display
                      (list 'space-width (- col (current-column))))
       (overlay-put o 'display fcr-fake-newline))
     (goto-char (match-end 0)))))

;;; Advised Mode-specific Filling Functions

;; Some progmodes bind fill-column to a different value during their filling
;; routines.  We need access to the shadowed value, so we store it
;; temporarily in fcr-fill-column.

(defadvice lisp-fill-paragraph (around fill-column-rule)
 (let ((fcr-fill-column fill-column))
   ad-do-it))

(defadvice ada-fill-comment-paragraph (around fill-column-rule)
 (let ((fcr-fill-column fill-column))
   ad-do-it))

(defadvice delphi-fill-comment (around fill-column-rule)
 (let ((fcr-fill-column fill-column))
   ad-do-it))

(defadvice idlwave-indent-line (around fill-column-rule)
 (let ((fcr-fill-column fill-column))
   ad-do-it))

;;; Misc

;; Automatically reset the rule after changes to fill-column.
(defadvice set-fill-column (after fill-column-rule)
 (if (and ad-return-value
          fcr-mode)
     (fcr-mode 1)))

;;; Hack for Display Bug

;; A apparent bug in Emacs's rendering (at least on the NS--i.e.,
;; MacOs--port) leads the modification of the display table spec for the
;; newline character to cause display artifacts to appear when horizontal
;; scrolling is in effect.  To avoid this, we use the following hack.  When a
;; window is horizontally scrolled, secondary overlays are placed over the
;; space-rule-newline trio on lines that appear within the window.  These
;; overlays have a display spec that replaces the newline glyph trio with a
;; string of the same visual appearance, designed to avoid triggering the
;; rendering bug.  When horizontally scrolling returns to normal we delete
;; any secondaries in the vicinity of the window contents (having many
;; overlays of this type in a buffer can slow vertical navigation); remaining
;; secondaries are deleted during the next idle period.

;; For each fcr-mode buffer, holds the timer object used to clean out
;; secondary overlays.
(defvar fcr-idle-timer nil)
(make-variable-buffer-local 'fcr-idle-timer)

;; For each fcr-mode buffer, holds the list of windows in which that buffer
;; is horizontally scrolled.
(defvar fcr-hscroll-list nil)
(make-variable-buffer-local 'fcr-hscroll-list)

;; Set by fcr-post-command-check to delete stray secondaries during idle
;; time.
(defun fcr-idle-delete-secondaries (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (fcr-delete-secondaries (point-min) (point-max))))
 (setq fcr-idle-timer nil))

(defsubst fcr-delete-secondaries (s e)
 (mapc (lambda (x) (if (overlay-get x 'fcr2)
                       (delete-overlay x)))
       (overlays-in s e)))

(defsubst fcr-put-secondaries (start end)
 (save-excursion
   (goto-char start)
   (let ((col (or fcr-fill-column fill-column))
         o)
     ;; Because of the way overlay lookup works, it's faster to delete all
     ;; the secondaries between start and end and reset them, rather than
     ;; checking each line as we go to see if a secondary is already present
     (fcr-delete-secondaries start end)
     (while (search-forward "\n" end t)
       (goto-char (match-beginning 0))
       (if (< (current-column) col)
           (progn
             (setq o (make-overlay (match-beginning 0)
                                   (match-end 0)))
             (overlay-put o 'category 'fcr)
             (overlay-put o 'fcr2 t)
             (overlay-put o 'priority 1)
             (overlay-put o
                          'display
                          (concat
                           (make-string (- fill-column (current-column)) 32)
                           fcr-rule-glyph
                           fcr-fake-newline))))
       (goto-char (match-end 0))))))

(defun fcr-window-scroll-check (win start)
 (when (memq (selected-window) fcr-hscroll-list)
   (fcr-put-secondaries start (line-beginning-position (window-height)))))

(defun fcr-post-command-check ()
 (if (and (memq (selected-window) fcr-hscroll-list)
          ;; We can't depend on window-hscroll being up-to-date when this is
          ;; called.  So don't rely on it at all, because we need to err on
          ;; the side of caution when removing the secondaries.
          auto-hscroll-mode
          (< (current-column) (- (window-width) hscroll-margin)))
     (progn
       (setq fcr-hscroll-list (delq (selected-window) fcr-hscroll-list))
       (set-window-hscroll (selected-window) 0)
       ;; If no windows displaying the buffer are hscrolled, delete the
       ;; secondaries.
       (unless fcr-hscroll-list
         (fcr-delete-secondaries (max (point-min) (- (window-start) 1000))
                                 (min (point-max) (+ (window-end) 1000)))
         (setq fcr-idle-timer
               (run-with-idle-timer 3
                                    nil
                                    'fcr-idle-delete-secondaries
                                    (current-buffer)))))
   (if (or (< 0 (window-hscroll))
           ;; Again, window-hscroll might not be accurate, so check manually
           ;; as well.
           (and auto-hscroll-mode
                (<= (- (window-width) hscroll-margin) (current-column))
                (< (window-width) (+ (current-column)
                                     (- (line-end-position) (point))))))
       (progn
         (setq fcr-hscroll-list (cons (selected-window) fcr-hscroll-list))
         (when fcr-idle-timer
           (cancel-timer fcr-idle-timer)
           (setq fcr-idle-timer nil))
         (fcr-put-secondaries (window-start) (window-end))))))

(provide 'fill-column-rule)

;;; fill-column-rule.el ends here
