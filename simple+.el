;;; simple+.el --- Extensions to standard library `simple.el'.
;;
;; Filename: simple+.el
;; Description: Extensions to standard library `simple.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2017, Drew Adams, all rights reserved.
;; Created: Fri Apr 12 10:56:45 1996
;; Version: 0
;; Package-Requires: ((strings "0"))
;;; Last-Updated: Sun Jan  1 11:36:03 2017 (-0800)
;;           By: dradams
;;     Update #: 497
;; URL: http://www.emacswiki.org/simple%2b.el
;; Keywords: internal, lisp, extensions, abbrev
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to standard library `simple.el'.
;;
;;  See also library `icicles.el' for extensions to `simple.el' that
;;  concern input completion in the minibuffer.  Those extensions used
;;  to be in this library, but they are used by `icicles.el', so they
;;  have been moved there.
;;
;;  Things you might want to do:
;;
;;  * Turn on `hl-line-mode' in compilation and grep buffers:
;;
;;    (add-hook 'next-error-hook 'next-error-buffer-hl-line)
;;
;;  * Change the fringe indicator for `next-error':
;;
;;    (add-hook 'next-error-hook 'next-error-fringe-setup)
;;
;;
;;  Commands defined here:
;;
;;    `set-any-variable'.
;;
;;  Non-interactive functions defined here:
;;
;;    `next-error-buffer-hl-line', `next-error-fringe-setup',
;;    `read-var-and-value'.
;;
;;  Internal variables defined here:
;;
;;    `next-error-fringe-indicator', `set-any-variable-value-history'.
;;
;;
;;  ***** NOTE: The following user options defined in `simple.el' have
;;              been REDEFINED HERE:
;;
;;    `next-error-highlight', `next-error-highlight-no-select'
;;                            - New value: `until-move' (persistent).
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have
;;              been REDEFINED HERE:
;;
;;    `edit-and-eval-command' - Bug fix for < Emacs 21: Adds COMMAND
;;                              as a command to `command-history', not
;;                              as a string.
;;
;;    `kill-new'              - Bug fix for < Emacs 21: be sure
;;                              `kill-ring' is non-empty before trying
;;                              to replace kill.
;;
;;    `next-error'            - `C-u C-u' just deletes highlighting
;;
;;  ***** NOTE: This EMACS PRIMITIVE has been REDEFINED HERE:
;;
;;    `set-variable' - Uses `read-var-and-value' to get args.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015/06/16 dadams
;;     Added: next-error-buffer-hl-line, next-error-fringe-setup,
;;            next-error-fringe-indicator.
;; 2015/06/13 dadams
;;     next-error: next-error-recenter is not defined for Emacs 22, so use boundp.
;; 2012/02/03 dadams
;;     read-var-and-value:
;;       Added optional arg BUFFER.  Use default-value if var is not local.
;;     set(-any)-variable:
;;       Add BUFFER arg in call to read-var-and-value (only for Icicles).
;;       Set default value, if not MAKE-LOCAL.
;; 2011/05/16 dadams
;;     Added redefinition of kill-new for Emacs 20.
;; 2011/01/04 dadams
;;     Added autoload cookie for command set-any-variable.
;; 2009-02-22 dadams
;;     Added: next-error(-highlight(-no-select)).
;; 2006/09/15 dadams
;;     set-variable: Protect custom-variable-p if not defined.
;; 2006/08/03 dadams
;;     No longer initialize kill-ring to ("") - no longer needed, and can cause problems.
;; 2005-10-31 dadams
;;     Added: set-any-variable-value-history, read-var-and-value, set*-variable.
;; 2005/10/03 dadams
;;     Removed require of icomplete+.el (no longer redefines read-from-minibuffer).
;; 2005/07/28 dadams
;;     Moved all completion stuff to icicles.el:
;;       choose-completion-string, completion-setup-function, switch-to-completions.
;;     No longer require icicles.el.
;;     Removed: completion-reference-buffer.
;;     completion-setup-function:
;;       Renamed icicle-completing-read-prompt-suffix to icicle-prompt-suffix.
;; 2005/07/15 dadams
;;     choose-completion-string, completion-setup-function: Updated for Emacs 21+.
;; 2005/07/10 dadams
;;     Renamed: command-calling-for-completion -> icicle-cmd-calling-for-completion.
;; 2004/09/21 dadams
;;     Only redefine edit-and-eval-command & choose-completion-string if prior to Emacs 21.
;; 1999/03/17 dadams
;;     1. choose-completion-string: Added doc string.  Updated to correspond to
;;        Emacs 34.1 version.
;;     2. completion-setup-function: diff prompt setups.  face1 & face2 tests.
;;     3. Added: switch-to-completions.
;; 1996/06/14 dadams
;;     kill-ring: Bug fix: `mouse-save-then-kill' expects a consp, so ensure this.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
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

;; Cannot do (require 'simple), because `simple.el' does no `provide'.
;; Don't want to do a (load-library "simple") either, because it wouldn't
;; allow doing (eval-after-load "simple" '(require 'simple+))

(eval-when-compile (when (< emacs-major-version 21) (require 'cl))) ;; push, pop

(require 'strings) ;; read-any-variable

(when (< emacs-major-version 22)        ; Quiet the byte-compiler.
  (defvar compilation-highlight-overlay)
  (defvar fringe-indicator-alist)
  (defvar next-error-function)
  (defvar next-error-last-buffer)
  (defvar next-error-recenter))

;;;;;;;;;;;;;;;;;;;;



;; REPLACE ORIGINAL in `simple.el'.
;;
;; Added new value `until-move'.  It is used in `compile+.el'.
;;
(when (> emacs-major-version 21)
  (defcustom next-error-highlight 0.5
    "*Highlighting of the current locus in selected source buffer.
Highlighting means use face `next-error' or show a fringe arrow.
Value:
* `fringe-arrow' means indicate the locus using a fringe arrow (which
  is moved when the locus is moved).
* `until-move' means highlight the locus until it is moved.
* A number means highlight the locus for that many seconds, or until
  the next command is executed.
* t means highlight the locus until it is moved or the next command is
  executed.
* nil means do not highlight the locus at all."
    :type '(choice
            (number :tag "Highlight locus for specified time")
            (const :tag  "Highlight locus until move"                 until-move)
            (const :tag  "Highlight locus until move or next command" t)
            (const :tag  "Do not highlight locus"                     nil)
            (const :tag  "Indicate locus using fringe arrow"          fringe-arrow))
    :group 'next-error
    :version "22.1"))



;; REPLACE ORIGINAL in `simple.el'.
;;
;; Added new value `until-move'.  It is used in `compile+.el'.
;;
(when (> emacs-major-version 21)
  (defcustom next-error-highlight-no-select 0.5
    "*Highlighting of locations in `next-error-no-select'.
Highlighting means use face `next-error' or show a fringe arrow.
Value:
* `fringe-arrow' means indicate the locus using a fringe arrow (which
  is moved when the locus is moved)..
* `until-move' means highlight the locus until it is moved.
* A number means highlight the locus for that many seconds, or until
  the next command is executed.
* t means highlight the locus until it is moved or the next command is
  executed.
* nil means do not highlight the locus at all."
    :type '(choice
            (number :tag "Highlight locus for specified time")
            (const :tag  "Highlight locus until move"                 until-move)
            (const :tag  "Highlight locus until move or next command" t)
            (const :tag  "Do not highlight locus"                     nil)
            (const :tag  "Indicate locus using fringe arrow"          fringe-arrow))
    :group 'next-error
    :version "22.1"))



;; REPLACE ORIGINAL in `simple.el'.
;;
;; `C-u C-u' means delete the highlight overlay.
;;
(when (> emacs-major-version 21)
  (defun next-error (&optional arg reset)
    "Visit next `next-error' message and corresponding source code.
If all the error messages parsed so far have been processed already,
the message buffer is checked for new ones.

A numeric prefix ARG specifies how many error messages to move;
 negative means move back to previous error messages.
Plain `\\[universal-argument]' means reparse the error message buffer and start at the
first error.
`C-u C-u' means just delete the highlight overlay (if any).

The RESET argument specifies that we should restart from the beginning.

`\\[next-error]' normally uses the most recently started
compilation, grep, or occur buffer.  It can also operate on any
buffer with output from the \\[compile], \\[grep] commands, or,
more generally, on any buffer in Compilation mode or with
Compilation Minor mode enabled, or any buffer in which
`next-error-function' is bound to an appropriate function.
To specify use of a particular buffer for error messages, type
`\\[next-error]' in that buffer when it is the only one displayed
in the current frame.

Once `\\[next-error]' has chosen the buffer for error messages, it
runs `next-error-hook' with `run-hooks', and stays with that buffer
until you use it in some other buffer which uses Compilation mode
or Compilation Minor mode.

See variables `compilation-parse-errors-function' and
\`compilation-error-regexp-alist' for customization ideas."
    (interactive "P")
    (if (and (consp arg) (= 16 (prefix-numeric-value arg))
             (boundp 'compilation-highlight-overlay)
             compilation-highlight-overlay)
        (delete-overlay compilation-highlight-overlay)
      (when (consp arg) (setq reset  t
                              arg    nil))
      (when (setq next-error-last-buffer  (next-error-find-buffer))
        ;; we know here that next-error-function is a valid symbol we can funcall
        (with-current-buffer next-error-last-buffer
          (funcall next-error-function (prefix-numeric-value arg) reset)
          (when (and (boundp 'next-error-recenter)  next-error-recenter)
            (recenter next-error-recenter))
          (run-hooks 'next-error-hook))))))

;; Indicating the current error in the compilation/grep buffer.

(when (fboundp 'hl-line-mode)

  (defun next-error-buffer-hl-line ()
    "Turn on `hl-line-mode' in buffer `next-error-last-buffer'.
To turn it off: `M-x hl-line-mode' in the compilation/grep buffer."
    (when (and next-error-last-buffer  (buffer-live-p next-error-last-buffer))
      (with-current-buffer next-error-last-buffer
        (hl-line-mode 1))))

  ;; (add-hook 'next-error-hook 'next-error-buffer-hl-line)

  )

(when (> emacs-major-version 21)

  (defvar next-error-fringe-indicator 'filled-rectangle
    "Fringe indicator to use for `next-error' in compilation/grep buffer.
The indicator is set to the value of `next-error-fringe-indicator'.")

  (defun next-error-fringe-setup ()
    "Set the fringe indicator for `next-error' in compilation/grep buffer."
    (with-current-buffer next-error-last-buffer
      (unless (eq next-error-fringe-indicator
                  (cdr (assq 'overlay-arrow fringe-indicator-alist)))
        (setq fringe-indicator-alist
              (cons `(overlay-arrow . ,next-error-fringe-indicator)
                    fringe-indicator-alist)))))

  ;; (add-hook 'next-error-hook 'next-error-fringe-setup)

  )



;; REPLACE ORIGINAL in `simple.el'.
;;
;; Original was bugged: it added COMMAND as a string to
;; `command-history'.  This version adds it as a command.
;; This was fixed in Emacs 21.
;;
(when (< emacs-major-version 21)
  (defun edit-and-eval-command (prompt command)
    "Prompting with PROMPT, let user edit COMMAND and eval result.
COMMAND is a Lisp expression.  Let user edit that expression in
the minibuffer, then read and evaluate the result."
    (let* ((minibuffer-history-sexp-flag  t)
           (command                       (read-from-minibuffer
                                           prompt (prin1-to-string command)
                                           read-expression-map t '(command-history . 1))))
      ;; If command was added to `command-history' as a string,
      ;; get rid of that.  We want only evaluable expressions there.
      (when (stringp (car command-history)) (pop command-history))
      ;; If command to be redone does not match front of `command-history',
      ;; add it to `command-history'.
      (unless (equal command (car command-history)) (push command command-history))
      (eval command))))


(defvar set-any-variable-value-history nil
  "History of values entered with `set-any-variable'.")


;; Helper function for `set*-variable'.
;; Inspired from original `set-variable', with these changes:
;; Use READ-VAR-FN and SET-VAR-HIST-VAR.  Use current value as default value.
;;
(defun read-var-and-value (read-var-fn set-var-hist-var make-local-p &optional buffer)
  "Read a variable name and value.
READ-VAR-FN is a function to read the variable name.
SET-VAR-HIST-VAR is a variable holding a history of variable values.
MAKE-LOCAL-P non-nil means the variable is to be local.
Optional arg BUFFER is the buffer used to determine the current value
of the variable, which is used as the default value when reading the new value."
  (let* ((var                   (funcall read-var-fn "Set variable: "))
         (current-val           (format "%S"
                                        (if (fboundp 'buffer-local-value) ; Emacs 22+.
                                            (buffer-local-value var (or buffer
                                                                        (current-buffer)))
                                          (if (member var (buffer-local-variables buffer))
                                              (with-current-buffer (or buffer
                                                                       (current-buffer))
                                                (symbol-value var))
                                            (default-value var)))))
         (minibuffer-help-form  '(describe-variable var))
         (prompt                (format "Set %s%s to value: " var
                                        (cond ((local-variable-p var) " (buffer-local)")
                                              ((or make-local-p
                                                   (local-variable-if-set-p var))
                                               " buffer-locally")
                                              (t " globally"))))
         (prop                  (get var 'variable-interactive))
         (val                   (if prop
                                    ;; Use VAR's `variable-interactive' property
                                    ;; as an interactive spec for prompting.
                                    (call-interactively `(lambda (arg)
                                                          (interactive ,prop) arg))
                                  (read (read-string prompt current-val set-var-hist-var
                                                     current-val)))))
    (list var val make-local-p)))



;; REPLACE ORIGINAL (built-in).
;;
;; Uses `read-var-and-value' to get args interactively.
;;
;;;###autoload
(defun set-variable (variable value &optional make-local)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
When using this interactively, enter a Lisp object for VALUE.
If you want VALUE to be a string, you must surround it with doublequotes.
VALUE is used literally, not evaluated.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read VALUE.

If VARIABLE has been defined with `defcustom', then the type information
in the definition is used to check that VALUE is valid.

With a prefix argument, set VARIABLE to VALUE buffer-locally."
  (interactive (read-var-and-value 'read-variable
                                   'set-variable-value-history
                                   current-prefix-arg
                                   (and (boundp 'icicle-pre-minibuffer-buffer)
                                        icicle-mode
                                        icicle-pre-minibuffer-buffer)))
  (and (or (not (fboundp 'custom-variable-p)) (custom-variable-p variable))
       (not (get variable 'custom-type))
       (custom-load-symbol variable))
  (let ((type  (get variable 'custom-type)))
    (when type
      ;; Match with custom type.
      (require 'cus-edit)
      (setq type  (widget-convert type))
      (unless (widget-apply type :match value)
        (error "Value `%S' does not match type %S of %S" value (car type) variable))))
  (if make-local
      (set (make-local-variable variable) value)
    (set-default variable value))
  ;; Force a thorough redisplay for the case that the variable
  ;; has an effect on the display, like `tab-width' has.
  (force-mode-line-update))

;;;###autoload
(defun set-any-variable (variable value &optional make-local)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
VARIABLE can be any Lisp variable, unlike `set-variable', where it
must be a user option.  Enter VALUE in Lisp syntax.  For example, if
you want VALUE to be a string, you must surround it with doublequotes.
VALUE is used literally, not evaluated.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read value.

If VARIABLE has been defined with `defcustom', then the type information
in the definition is used to check that VALUE is valid.

With a prefix argument, set VARIABLE to VALUE buffer-locally."
  (interactive (read-var-and-value 'read-any-variable
                                   'set-any-variable-value-history
                                   current-prefix-arg
                                   (and (boundp 'icicle-pre-minibuffer-buffer)
                                        icicle-mode
                                        icicle-pre-minibuffer-buffer)))
  (and (custom-variable-p variable)
       (not (get variable 'custom-type))
       (custom-load-symbol variable))
  (let ((type  (get variable 'custom-type)))
    (when type
      ;; Match with custom type.
      (require 'cus-edit)
      (setq type  (widget-convert type))
      (unless (widget-apply type :match value)
        (error "Value `%S' does not match type %S of %S" value (car type) variable))))
  (if make-local
      (set (make-local-variable variable) value)
    (set-default variable value))
  ;; Force a thorough redisplay for the case that the variable
  ;; has an effect on the display, like `tab-width' has.
  (force-mode-line-update))



;; REPLACE ORIGINAL in `simple.el'.
;;
;; Just updates 20.3 with version from 20.6.1: corrects deletion of multiple.
;;
(when (string< emacs-version "20.6.1")
  (defun comment-region (beg end &optional arg)
    "Comment or uncomment each line in the region.
With just C-u prefix arg, uncomment each line in region.
Numeric prefix arg ARG means use ARG comment characters.
If ARG is negative, delete that many comment characters instead.
Comments are terminated on each line, even for syntax in which newline does
not end the comment.  Blank lines do not get comments."
    ;; if someone wants it to only put a comment-start at the beginning and
    ;; comment-end at the end then typing it, C-x C-x, closing it, C-x C-x
    ;; is easy enough.  No option is made here for other than commenting
    ;; every line.
    (interactive "r\nP")
    (or comment-start (error "No comment syntax is defined"))
    (when (> beg end) (let (mid)
                        (setq mid  beg
                              beg  end
                              end  mid)))
    (save-excursion
      (save-restriction
        (let ((cs  comment-start)
              (ce  comment-end)
              (cp  (and comment-padding (make-string comment-padding ? )))
              numarg)
          (if (consp arg)
              (setq numarg  t)
            (setq numarg  (prefix-numeric-value arg))
            ;; For positive arg > 1, replicate the comment delims now,
            ;; then insert the replicated strings just once.
            (while (> numarg 1)
              (setq cs      (concat cs comment-start)
                    ce      (concat ce comment-end)
                    numarg  (1- numarg))))
          ;; Loop over all lines from BEG to END.
          (narrow-to-region beg end)
          (goto-char beg)
          (if (or (eq numarg t) (< numarg 0))
              (while (not (eobp))
                (let (found-comment)
                  ;; Delete comment start from beginning of line.
                  (if (eq numarg t)
                      (while (looking-at (regexp-quote cs))
                        (setq found-comment  t)
                        (delete-char (length cs)))
                    (let ((count  numarg))
                      (while (and (> 1 (setq count  (1+ count)))
                                  (looking-at (regexp-quote cs)))
                        (setq found-comment  t)
                        (delete-char (length cs)))))
                  ;; Delete comment padding from beginning of line
                  (when (and found-comment comment-padding (looking-at (regexp-quote cp)))
                    (delete-char comment-padding))
                  ;; Delete comment end from end of line.
                  (if (string= "" ce)
                      nil
                    (if (eq numarg t)
                        (progn
                          (end-of-line)
                          ;; This is questionable if comment-end ends in
                          ;; whitespace.  That is pretty brain-damaged,
                          ;; though.
                          (while (progn (skip-chars-backward " \t")
                                        (and (>= (- (point) (point-min)) (length ce))
                                             (save-excursion
                                               (backward-char (length ce))
                                               (looking-at (regexp-quote ce)))))
                            (delete-char (- (length ce)))))
                      (let ((count  numarg))
                        (while (> 1 (setq count  (1+ count)))
                          (end-of-line)
                          ;; this is questionable if comment-end ends in whitespace
                          ;; that is pretty brain-damaged though
                          (skip-chars-backward " \t")
                          (if (>= (- (point) (point-min)) (length ce))
                              (save-excursion
                                (backward-char (length ce))
                                (if (looking-at (regexp-quote ce))
                                    (delete-char (length ce)))))))))
                  (forward-line 1)))

            (when comment-padding (setq cs  (concat cs cp)))
            (while (not (eobp))
              ;; Insert at beginning and at end.
              (if (looking-at "[ \t]*$") ()
                (insert cs)
                (if (string= "" ce) ()
                  (end-of-line)
                  (insert ce)))
              (search-forward "\n" nil 'move))))))))



;; REPLACE ORIGINAL in `simple.el'.
;;
;; Fixes bug when REPLACE is non-nil but `kill-ring' is nil.
;;
(when (< emacs-major-version 21)
  (defun kill-new (string &optional replace)
    "Make STRING the latest kill in the kill ring.
Set the kill-ring-yank pointer to point to it.
If `interprogram-cut-function' is non-nil, apply it to STRING.
Optional second argument REPLACE non-nil means that STRING will replace
the front of the kill ring, rather than being added to the list."
    (and (fboundp 'menu-bar-update-yank-menu)
         (menu-bar-update-yank-menu string (and replace (car kill-ring))))
    (if (and replace kill-ring)         ; Bug fix: only if non-nil `kill-ring'.
        (setcar kill-ring string)
      (setq kill-ring  (cons string kill-ring))
      (when (> (length kill-ring) kill-ring-max)
        (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
    (setq kill-ring-yank-pointer  kill-ring)
    (when interprogram-cut-function
      (funcall interprogram-cut-function string (not replace)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'simple+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple+.el ends here
