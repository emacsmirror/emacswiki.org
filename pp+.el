;;; pp+.el --- Extensions to `pp.el'.
;;
;; Filename: pp+.el
;; Description: Extensions to `pp.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1999-2017, Drew Adams, all rights reserved.
;; Created: Fri Sep  3 13:45:40 1999
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 11:17:09 2017 (-0800)
;;           By: dradams
;;     Update #: 419
;; URL: http://www.emacswiki.org/pp%2b.el
;; Doc URL: http://emacswiki.org/EvaluatingExpressions
;; Keywords: lisp
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `pp'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `pp.el'.
;;
;;  Features:
;;
;;   * You can optionally show the result of pretty-printing in a
;;     tooltip at point, by customizing option `pp-max-tooltip-size'
;;     (Emacs 24+).
;;
;;   * You can use a zero prefix argument (e.g. `M-o') with
;;     `pp-eval-last-sexp' (`C-x C-e') or `pp-eval-expression', to
;;     swap the use of a tooltip defined by option
;;     `pp-max-tooltip-size'.  (Emacs 24+)
;;
;;   * There are additional commands that are versions of
;;     `pp-eval-last-sexp' and `pp-eval-expression' that always or
;;     never use a tooltip.  (Emacs 24+)
;;
;;   * Pretty-printing respects options
;;     `pp-eval-expression-print-length' and
;;     `pp-eval-expression-print-level', which act like `print-length'
;;     and `print-level', but only for pretty-printing.
;;
;;   * The buffer displaying the pretty-printed result is in
;;     `emacs-lisp-mode' (and is fontified accordingly), but without
;;     having run `emacs-lisp-mode-hook' or `change-major-mode-hook'.
;;
;;   * Command `pp-eval-expression' is enhanced in these ways:
;;
;;      - Option `eval-expression-debug-on-error' is respected.
;;
;;      - With no prefix argument, option `pp-max-tooltip-size' is
;;        respected. If a tooltip is not used then if the value fits
;;        on one line (frame width) it is shown in the echo area.
;;        Otherwise, it is shown in buffer *Pp Eval Output*'.  (Emacs
;;        24+)
;;
;;      - With a zero prefix arg, the use of a tooltip according to
;;        `pp-max-tooltip-size' is swapped: if that option is `nil'
;;        then a tooltip is used, and if non-`nil' a tooltip is not
;;        used.  (Emacs 24+)
;;
;;      - With non-zero prefix argument, the value is inserted into
;;        the current buffer at point. With a negative prefix arg, if
;;        the value is a string, then it is inserted without being
;;        enclosed in double-quotes (").
;;
;;      - Completion is available, using keymap
;;        `pp-read-expression-map', which is like
;;        `read-expression-map' but with some Emacs-Lisp key bindings.
;;
;;   * Command `pp-eval-last-sexp' is enhanced in these ways (Emacs
;;     24+):
;;
;;      - With a zero prefix arg, the use of a tooltip according to
;;        `pp-max-tooltip-size' is swapped: if that option is `nil'
;;        then a tooltip is used, and if non-`nil' a tooltip is not
;;        used.
;;
;;      - With a non-zero prefix arg, the value is inserted into the
;;        current buffer at point.
;;
;;   * Alternative commands are defined that use a tooltip whenever
;;     possible, or that never use a tooltip (they ignore option
;;     `pp-max-tooltip-size'): `pp-eval-expression-with-tooltip',
;;     `pp-eval-expression-without-tooltip',
;;     `pp-eval-last-sexp-with-tooltip', and
;;     `pp-eval-last-sexp-without-tooltip' (Emacs 24+).
;;
;; 
;;
;;  Suggested binding: use `M-:' for `pp-eval-expression'.
;;
;;    (global-set-key [remap eval-expression] 'pp-eval-expression)
;;
;;
;;  User options defined here:
;;
;;    `pp-eval-expression-print-length',
;;    `pp-eval-expression-print-level', `pp-max-tooltip-size' (Emacs
;;    24+).
;;
;;  Faces defined here:
;;
;;    `pp-tooltip' (Emacs 24+).
;;
;;  Commands defined here:
;;
;;    `pp-eval-expression-with-tooltip' (Emacs 24+),
;;    `pp-eval-expression-without-tooltip' (Emacs 24+),
;;    `pp-eval-last-sexp-with-tooltip' (Emacs 24+),
;;    `pp-eval-last-sexp-without-tooltip' (Emacs 24+).
;;
;;  Non-interactive functions defined here:
;;
;;    `pp-expression-size', `pp-read--expression' (Emacs 24.4+),
;;    `pp-show-tooltip' (Emacs 24+), `pp-tooltip-show' (Emacs 24+).
;;
;;  Variables defined here:
;;
;;    `pp-read-expression-map'.
;;
;;
;;  ***** NOTE: The following functions defined in `pp.el' have
;;              been REDEFINED HERE:
;;
;;    `pp-display-expression', `pp-eval-expression',
;;    `pp-eval-last-sexp' (Emacs 23+).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/11/14 dadams
;;     pp-max-tooltip-size: Mention in doc that if point is off-screen then tooltip not used.
;;     pp-display-expression: If point is off-screen then do not try to use tooltip.
;; 2016/05/30 dadams
;;     pp-eval-last-sexp: Do not define for Emacs < 23.
;; 2016/05/21 dadams
;;     Added: pp-max-tooltip-size, face pp-tooltip, pp-show-tooltip,
;;            pp-tooltip-show, pp-expression-size, pp-eval-expression-with(out)-tooltip,
;;            pp-eval-last-sexp-with(out)-tooltip, (redefinition of) pp-eval-last-sexp.
;;     pp-display-expression: Use tooltip if pp-max-tooltip-size says to.  Added autoload cookie.
;;     pp-eval-expression: Added SWAP-TOOLTIP optional arg.
;;                         Use pp-read--expression (forgot to use it on 2015-04-18).
;;     pp-read--expression: Updated per Emacs 25 - use add-function.
;; 2016/05/01 dadams
;;     pp-eval-expression: Return result of evaluation.
;; 2015/04/18 dadms
;;     Added: pp-read--expression.
;;     pp-eval-expression: Updated for Emacs 24.4+: use pp-read--expression.
;; 2013/12/03 dadams
;;     pp-read-expression-map: Swap TAB and M-TAB, so 1st completes Lisp symbols.
;; 2013/02/15 dadams
;;     pp-eval-expression: Bind deactivate-mark to nil, like fix for Emacs bug #13724.
;; 2012/07/22 dadams
;;     pp-display-expression:
;;       Do not try to select old-window if it is no longer live.
;;       Use backquote + comma, to replace free var.
;; 2011/01/04 dadams
;;     Added autoload cookies for defcustom.
;; 2010/06/21 dadams
;;     pp-display-expression: Set the hooks locally instead of let-binding them, to
;;       avoid msg "Making change-major-mode-hook buffer-local while locally
;;       let-bound!" - suggestion from Stefan M.
;; 2010/01/12 dadams
;;     Added: pp-display-expression (redefinition).
;;     pp-eval-expression: Use pp-display-expression.
;; 2008/04/14 dadams
;;     pp-eval-expression: Treat negative prefix arg.
;; 2008/04/13 dadams
;;     pp-eval-expression:
;;       Treat prefix arg (added optional arg).
;;       Respect eval-expression-debug-on-error and pp-eval-expression-print-*.
;;     Added: pp-eval-expression-print-(length|level), pp-read-expression-map.
;; 2007/02/04 dadams
;;     pp-eval-expression: Undid 01/05 workaround, because RMS fixed vanilla Emacs.
;; 2007/01/05 dadams
;;     pp-eval-expression: Adjusted to fit Emacs 22 definition, which doesn't eval.
;; 2006/01/29 dadams
;;     pp-eval-expression: Use read-from-minibuffer.
;; 2005/01/04 dadams
;;     Set buffer-read-only to nil.
;; 2004/03/24 dadams
;;     pp-eval-expression: Added call to font-lock-fontify-buffer.
;
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

(require 'pp)

;;; Quiet the byte compiler.
(defvar eldoc-documentation-function)   ; In `eldoc.el' (Emacs 23+)
(defvar eval-expression-debug-on-error)
(defvar lexical-binding)                ; Emacs 24+
(defvar pp-max-tooltip-size)            ; Here, for Emacs 24+
(defvar tooltip-frame-parameters)       ; Emacs 22+
(defvar tooltip-hide-delay)             ; Emacs 22+
(defvar tooltip-x-offset)               ; Emacs 22+
(defvar tooltip-y-offset)               ; Emacs 22+
(defvar x-max-tooltip-size)             ; Emacs 22+

;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar pp-read-expression-map nil
  "`read-expression-map' with some Emacs-Lisp key bindings.")
(unless pp-read-expression-map
  (let ((map  (make-sparse-keymap)))
    (define-key map "\M-\t" 'lisp-indent-line)
    (define-key map "\t" 'lisp-complete-symbol)
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\e\t" 'lisp-indent-line)
    (define-key map "\e\C-x" 'eval-defun)
    (define-key map "\e\C-q" 'indent-pp-sexp)
    ;;(define-key map "\177" 'backward-delete-char-untabify)
    (set-keymap-parent map minibuffer-local-map)
    (setq pp-read-expression-map  map)))

;;;###autoload
(defcustom pp-eval-expression-print-length nil
  "*Value for `print-length' while printing value in `pp-eval-expression'.
A value of nil means no limit."
  :group 'pp :group 'lisp :type '(choice (const :tag "No Limit" nil) integer))

;;;###autoload
(defcustom pp-eval-expression-print-level nil
  "*Value for `print-level' while printing value in `pp-eval-expression'.
A value of nil means no limit."
  :group 'pp :group 'lisp :type '(choice (const :tag "No Limit" nil) integer))

;; Only difference is `pp-read-expression-map' instead of `read-expression-map'.
(when (fboundp 'read--expression)       ; Emacs 24.4+

  (defun pp-read--expression (prompt &optional initial-contents)
    (let ((minibuffer-completing-symbol t))
      (minibuffer-with-setup-hook
       (lambda ()                       ; Vanilla Emacs FIXME: call `emacs-lisp-mode'?
         (add-function :before-until (local 'eldoc-documentation-function)
                       #'elisp-eldoc-documentation-function)
         (add-hook 'completion-at-point-functions
                   #'elisp-completion-at-point nil t)
         (run-hooks 'eval-expression-minibuffer-setup-hook))
       (read-from-minibuffer prompt initial-contents
                             pp-read-expression-map t 'read-expression-history)))))

(when (fboundp 'window-inside-absolute-pixel-edges) ; Emacs 24+

  (defcustom pp-max-tooltip-size nil
    "Max size for showing pretty-printed values in a tooltip at point.
The value can be:

* A cons that has the same form as `x-max-tooltip-size':
  (WIDTH . HEIGHT), where WIDTH is the max width of the tooltip in
  chars and HEIGHT is the max height in chars.  If the value to be
  printed cannot fit in a tooltip this large then it is shown in
  buffer `*Pp Eval Output*'.

* nil, meaning never use a tooltip.

* t, meaning use a tooltip but clip the value to `x-max-tooltip-size'.

Note: Regardless of the option value, a tooltip is not used if point
is off-screen when pretty-printing is called for.  This can happen,
for instance, if pretty-printing the result of evaluating an
expression that moves point off-screen."
    :type `(choice
            (const :tag "Do not use a tooltip" nil)
            (const :tag "Always use a tooltip, and clip value if too big" t)
            (cons  :tag "Use a tooltip only if smaller than WIDTH x HEIGHT"
             (integer :tag "Width (characters)"  :value ,(car x-max-tooltip-size))
             (integer :tag "Height (characters)" :value ,(cdr x-max-tooltip-size))))
    :group 'pp :group 'lisp)

  (defface pp-tooltip
      '((((class color))
         :background "lightyellow"
         :foreground "black"
         :family     "Courier")
        (t
         :family     "Courier"))
    "Face for `pp-show-tooltip'."
    :group 'tooltip)

  (defun pp-tooltip-show (text &optional use-echo-area face)
    "Show TEXT according to option `pp-max-tooltip-size'.
Optional arg USE-ECHO-AREA non-nil means show TEXT in the echo area
 instead of in a tooltip.
Optional arg FACE defaults to `pp-tooltip'."
    (setq face  (or face  'pp-tooltip))
    (if use-echo-area                   ; Not used in code here.
        (tooltip-show-help-non-mode text)
      (condition-case error
          (let ((params  (copy-sequence tooltip-frame-parameters))
                (fg      (face-attribute face :foreground))
                (bg      (face-attribute face :background)))
            (when (stringp fg)
              (setq params  (tooltip-set-param params 'foreground-color fg)
                    params  (tooltip-set-param params 'border-color     fg)))
            (when (stringp bg)
              (setq params  (tooltip-set-param params 'background-color bg)))
            (x-show-tip (propertize text 'face face)
                        (selected-frame)
                        params
                        tooltip-hide-delay
                        tooltip-x-offset
                        tooltip-y-offset))
        (error (message "Error while displaying tooltip: %s" error)
               (sit-for 1)
               (message "%s" text)))))

  (defun pp-show-tooltip (value)
    "Show Lisp VALUE in a tooltip at point using face `pp-tooltip'."
    (let* ((posn-at-pt                (posn-at-point))
           (x-y                       (and posn-at-pt  (posn-x-y posn-at-pt)))
           (win-edges                 (and x-y  (window-inside-absolute-pixel-edges)))
           (left                      (and x-y  (+ (car x-y) (car win-edges))))
           (top                       (and x-y  (+ (cdr x-y) (cadr win-edges))))
           (tooltip-frame-parameters  `((name . "tooltip")
                                        (internal-border-width . 2)
                                        (border-width . 1)
                                        (left ,@ left)
                                        (top  ,@ top))))
      (pp-tooltip-show (pp-to-string value))
      value))

  (defun pp-expression-size (value buffer)
    "Return the size in characters needed to pretty-print Lisp VALUE.
Pretty-print VALUE in BUFFER in a temporary invisible frame, then
return the value of `posn-col-row'.  If the value is non-`nil', it is
a cons (COLUMNS . ROWS) of the rectangle needed for the result of
pretty-printing."
    (let ((buf  (get-buffer-create buffer))
          posn)
      (with-current-buffer buf
        (erase-buffer)
        (let* ((special-display-buffer-names  ())
               (special-display-regexps       ())
               (invis-frame                   (make-frame '((visibility . nil))))
               (win                           (display-buffer buf nil invis-frame)))
          (pp value buf)
          (with-selected-frame invis-frame
            (select-window win)
            (goto-char (point-max))
            (setq posn  (posn-at-point))
            (prog1 (and posn  (posn-col-row posn))
              (delete-frame)))))))

  (defun pp-eval-expression-with-tooltip (expression &optional insert-value)
    "This is `pp-eval-expression', but using a tooltip when possible.
This is `pp-eval-expression' with `pp-max-tooltip-size' bound to
`x-max-tooltip-size'.  A printed value larger than this is shown in
buffer `*Pp Eval Output*'."
    (interactive
     (list (if (fboundp 'pp-read--expression)
               (pp-read--expression "Eval: ")
             (read-from-minibuffer "Eval: " nil pp-read-expression-map t 'read-expression-history))
           current-prefix-arg))
    (let ((pp-max-tooltip-size  x-max-tooltip-size))
      (pp-eval-expression expression insert-value)))

  (defun pp-eval-expression-without-tooltip (expression &optional insert-value)
    "This is `pp-eval-expression', but never using a tooltip.
This is `pp-eval-expression' with `pp-max-tooltip-size' bound to `nil'."
    (interactive
     (list (if (fboundp 'pp-read--expression)
               (pp-read--expression "Eval: ")
             (read-from-minibuffer "Eval: " nil pp-read-expression-map t 'read-expression-history))
           current-prefix-arg))
    (let ((pp-max-tooltip-size  nil))
      (pp-eval-expression expression insert-value)))

  (defun pp-eval-last-sexp-with-tooltip (arg)
    "Run `pp-eval-expression-with-tooltip' on the sexp before point.
This is `pp-eval-last-sexp' with `pp-max-tooltip-size' bound to
`x-max-tooltip-size'.  A printed value larger than this is shown in
buffer `*Pp Eval Output*'."
    (interactive "P")
    (if arg
        (insert (pp-to-string (eval (pp-last-sexp) lexical-binding)))
      (let ((pp-max-tooltip-size  x-max-tooltip-size))
        (pp-eval-expression (pp-last-sexp)))))

  (defun pp-eval-last-sexp-without-tooltip (arg)
    "Run `pp-eval-expression-without-tooltip' on the sexp before point.
This is `pp-eval-expression' with `pp-max-tooltip-size' bound to `nil'."
    (interactive "P")
    (if arg
        (insert (pp-to-string (eval (pp-last-sexp) lexical-binding)))
      (let ((pp-max-tooltip-size  nil))
        (pp-eval-expression (pp-last-sexp)))))

  )


;; REPLACES ORIGINAL in `pp.el':
;; 1. Read with completion, using `pp-read-expression-map'.
;; 2. Progress message added.
;; 3. Added optional args: insertion behavior and swapping use of a tooltip.
;; 4. Respect `pp-eval-expression-print-length', `pp-eval-expression-print-level',
;;    `pp-max-tooltip-size', and `eval-expression-debug-on-error'.
;; 5. Adjusted to work in different Emacs releases.
;; 6. Return result of evaluation (it is also the car of variable `values').
;;
;;;###autoload
(defun pp-eval-expression (expression &optional insert-value swap-tooltip)
  "Read an Emacs-Lisp sexp, evaluate it, and pretty-print its value.
Add the value to the front of the variable `values'.
With no prefix arg, respect `pp-max-tooltip-size'.  If a tooltip is
 not used then if the value fits on one line (frame width) show it in
 the echo area.  Otherwise, show it in buffer `*Pp Eval Output*'.
With a zero prefix arg, this swaps the use of a tooltip according to
 `pp-max-tooltip-size': if that option is nil then a tooltip is used,
 and if non-nil a tooltip is not used.
With a non-zero prefix arg, insert the value into the current buffer
 at point.  If the prefix arg is negative and the value is a string
 then insert it into the buffer without double-quotes (`\"').

For Emacs prior to 24.4, the command has no support for a tooltip.
A zero prefix arg is then treated the same as a positive prefix arg.

Non-interactively:
 * Non-nil SWAP-TOOLTIP means swap the use of a tooltip.
 * Non-nil INSERT-VALUE is treated like a non-zero raw prefix arg:
   insert the value in the buffer (sans quotes if negative).

This command respects user options `pp-eval-expression-print-length',
`pp-eval-expression-print-level', `pp-max-tooltip-size', and
`eval-expression-debug-on-error'.

Emacs-Lisp mode completion and indentation bindings are in effect."
  (interactive
   (list (if (fboundp 'pp-read--expression)
             (pp-read--expression "Eval: ")
           (read-from-minibuffer "Eval: " nil pp-read-expression-map t 'read-expression-history))
         current-prefix-arg
         (zerop (prefix-numeric-value current-prefix-arg))))
  (if (and swap-tooltip  (fboundp 'pp-eval-expression-with-tooltip)) ; Emacs 24.4+
      (if (not pp-max-tooltip-size)
          (pp-eval-expression-with-tooltip expression)
        (pp-eval-expression-without-tooltip expression))
    (message "Evaluating...")
    (if (or (not (boundp 'eval-expression-debug-on-error))
            (null eval-expression-debug-on-error))
        (setq values  (cons (if (boundp 'lexical-binding) ; Emacs 24+
                                (eval expression lexical-binding)
                              (eval expression))
                            values))
      (let ((old-value  (make-symbol "t"))
            new-value)
        ;; Bind `debug-on-error' to something unique so that we can
        ;; detect when evaled code changes it.
        (let ((debug-on-error  old-value))
          (setq values     (cons (eval expression) values)
                new-value  debug-on-error))
        ;; If evaled code has changed the value of `debug-on-error',
        ;; propagate that change to the global binding.
        (unless (eq old-value new-value)
          (setq debug-on-error  new-value))))
    (let ((print-length     pp-eval-expression-print-length)
          (print-level      pp-eval-expression-print-level)
          (deactivate-mark  nil))
      (cond (insert-value
             (message "Evaluating...done. Value inserted.")
             (setq insert-value  (prefix-numeric-value insert-value))
             (if (or (not (stringp (car values))) (wholenump insert-value))
                 (pp (car values) (current-buffer))
               (princ (car values) (current-buffer))))
            (t (pp-display-expression (car values) "*Pp Eval Output*")))
      (car values))))


;; REPLACES ORIGINAL in `pp.el':
;; 1. Added optional arg: swapping use of a tooltip.
;; 4. Respect `pp-eval-expression-print-length', `pp-eval-expression-print-level',
;;    `pp-max-tooltip-size', and `eval-expression-debug-on-error'.
;; 5. Adjusted to work in different Emacs releases.
;;
(when (fboundp 'pp-last-sexp)           ; Emacs 23+

  (defun pp-eval-last-sexp (insert-value &optional swap-tooltip)
    "Run `pp-eval-expression' on sexp before point.
With a zero prefix arg, this swaps the use of a tooltip according to
 `pp-max-tooltip-size': if that option is nil then a tooltip is used,
 and if non-nil a tooltip is not used.
With a non-zero prefix arg, pretty-print the value into the current
 buffer.
Ignores leading comment characters.

For Emacs prior to 24.4, the command has no support for a tooltip.
A zero prefix arg is then treated the same as a positive prefix arg.

Non-interactively:
 * Non-nil SWAP-TOOLTIP means swap the use of a tooltip.
 * Non-nil INSERT-VALUE is treated like a non-zero prefix arg:
   insert the value in the buffer.

This command respects user options `pp-eval-expression-print-length',
`pp-eval-expression-print-level', `pp-max-tooltip-size', and
`eval-expression-debug-on-error'."
    (interactive (list current-prefix-arg 
                       (zerop (prefix-numeric-value current-prefix-arg))))
    (if swap-tooltip
        (if (not pp-max-tooltip-size)
            (pp-eval-last-sexp-with-tooltip nil)
          (pp-eval-last-sexp-without-tooltip nil))
      (if insert-value
          (insert (pp-to-string (eval (pp-last-sexp) lexical-binding)))
        (pp-eval-expression (pp-last-sexp)))))
  )


;; REPLACES ORIGINAL in `pp.el':
;;
;; 1. Respects `pp-max-tooltip-size'.
;; 2. Use no `emacs-lisp-mode-hook' or `change-major-mode-hook'.
;; 3. Call `font-lock-fontify-buffer'.
;;
;;;###autoload
(defun pp-display-expression (expression out-buffer-name)
  "Prettify and show EXPRESSION, respecting option `pp-max-tooltip-size'.
If `pp-max-tooltip-size' is non-`nil' then show it with a tooltip.
Else, if there is room then show it in the echo-area.
Else show it in buffer OUT-BUFFER-NAME."
  (let* ((use-tooltip  (and (boundp 'pp-max-tooltip-size)  (eq pp-max-tooltip-size t))) ; Emacs 24+
         (sexp-size    (and (fboundp 'pp-show-tooltip) ; Emacs 24+
                            (not use-tooltip)
                            pp-max-tooltip-size
                            (pp-expression-size expression out-buffer-name)))
         (use-tooltip  (or use-tooltip  (and sexp-size
                                             (<= (car sexp-size) (car pp-max-tooltip-size))
                                             (<= (cdr sexp-size) (cdr pp-max-tooltip-size))))))
    (if (and use-tooltip  (posn-at-point)) ; Ensure that point is on-screen now.
        (progn (pp-show-tooltip expression) (message nil))
      (let* ((old-show-function  temp-buffer-show-function)
             (temp-buffer-show-function
              `(lambda (buf)
                (with-current-buffer buf
                  (goto-char (point-min))
                  (end-of-line 1)
                  (if (or (< (1+ (point)) (point-max))
                          (>= (- (point) (point-min)) (frame-width)))
                      (let ((temp-buffer-show-function  ',old-show-function)
                            (old-selected               (selected-window))
                            (window                     (display-buffer buf)))
                        (goto-char (point-min)) ; Expected by some hooks ...
                        (make-frame-visible (window-frame window))
                        (unwind-protect
                             (progn (select-window window)
                                    (run-hooks 'temp-buffer-show-hook))
                          (when (window-live-p old-selected)
                            (select-window old-selected))
                          (message "Evaluating...done.  See buffer `%s'."
                                   out-buffer-name)))
                    (message "%s" (buffer-substring (point-min) (point))))))))
        (with-output-to-temp-buffer out-buffer-name
          (pp expression)
          (with-current-buffer standard-output
            (setq buffer-read-only  nil)
            ;; Avoid `let'-binding because `change-major-mode-hook' is local.  IOW, avoid runtime
            ;; message: "Making change-major-mode-hook buffer-local while locally let-bound!"
            ;; Suggestion from Stefan M.: Set these hooks instead of binding, because they are not
            ;; permanent-local.  They are emptied and repopulated as needed by `emacs-lisp-mode'.
            (set (make-local-variable 'emacs-lisp-mode-hook) nil)
            (set (make-local-variable 'change-major-mode-hook) nil)
            (emacs-lisp-mode)
            (set (make-local-variable 'font-lock-verbose) nil)
            (font-lock-fontify-buffer)))))))

;;;;;;;;;;;;;;;;;;

(provide 'pp+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pp+.el ends here
