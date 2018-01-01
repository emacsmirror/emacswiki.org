;;; macros+.el --- Extensions to `macros.el'.
;;
;; Filename: macros+.el
;; Description: Extensions to `macros.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2018, Drew Adams, all rights reserved.
;; Created: Mon Apr 22 08:42:10 1996
;; Version: 0
;; Package-Requires: ()
;;; Last-Updated: Mon Jan  1 14:49:00 2018 (-0800)
;;           By: dradams
;;     Update #: 196
;; URL: https://www.emacswiki.org/emacs/download/macros%2b.el
;; Keywords: abbrev, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `macros', `macros+', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `macros.el'.
;;
;;
;;  ***** NOTE: The following functions defined in `macros.el' have
;;              been REDEFINED HERE:
;;
;;  `apply-macro-to-region-lines' -
;;     Make sure that `delete-selection-mode' is deactivated.  (See
;;     `delsel.el'.)  Otherwise, character self-insertion by the
;;     keyboard macro could cause the region to be deleted (replaced
;;     by the inserted text) when `apply-macro-to-region-lines' was
;;     finished (e.g. via a delete-selection `pre-command-hook').
;;
;;  `insert-kbd-macro', `name-last-kbd-macro' -
;;     These functions now use `completing-read' in interactive spec,
;;     with, as default, `symbol-nearest-point'.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `macros.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "macros" '(require 'macros+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/08/21 dadams
;;     Call tap-put-thing-at-point-props after load thingatpt+.el.
;; 2012/08/18 dadams
;;     Invoke tap-define-aliases-wo-prefix if thingatpt+.el is loaded.
;; 2012/02/25 dadams
;;     Removed soft require of Icicles.
;; 2007/05/18 dadams
;;     Require cl only at compile time (for Emacs < 21).
;; 2005/10/31 dadams
;;     Use nil as init-value arg in calls to completing-read, everywhere.
;; 2004/09/21 dadams
;;     apply-macro-to-region-lines: Updated for Emacs 21.
;; 1997/03/21 dadams
;;     Updated to 19.34 via distrib diffs:
;;     name-last-kbd-macro: Error if no command name given.
;;     insert-kbd-macro: 1. Treat vectors too.  2. Treat chars C-\, M-C-\.
;; 1996/07/15 dadams
;;     Added redefinition of name-last-kbd-macro.
;; 1996/07/11 dadams
;;     Added redefinition of insert-kbd-macro that uses completing-read with
;;     symbol-nearest-point.
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


(eval-when-compile
 (when (< emacs-major-version 21) (require 'cl))) ;; pop
                                    ;; (plus, for Emacs <20: when, unless)

;; Cannot do (require 'macros) prior to version 20, because `macros.el'
;; does no `provide'.  Don't want to do a (load-library "macros") either,
;; for prior versions, because it wouldn't allow doing
;; (eval-after-load "macros" '(progn (require 'macros+)))
(when (>= emacs-major-version 20) (require 'macros))

(require 'thingatpt nil t);; (no error if not found): symbol-at-point

(when (and (require 'thingatpt+ nil t);; (no error if not found)
           (fboundp 'tap-put-thing-at-point-props)) ; >= 2012-08-21
  (tap-define-aliases-wo-prefix)
  (tap-put-thing-at-point-props))
 ;; symbol-nearest-point

;; Free variable here: LAST-KBD-MACRO

;;;;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL in `macros.el':
;; Uses `completing-read' in interactive spec, with `symbol-nearest-point' as
;; default.
;;;###autoload
(defun name-last-kbd-macro (symbol)
  "Assign a name to the last keyboard macro defined.
Argument SYMBOL is the name to define.  SYMBOL's function definition
becomes the keyboard macro string.  Such a \"function\" cannot be
called from Lisp, but it is a valid editor command."
  (interactive
   (let ((symb (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                     ((fboundp 'symbol-at-point) (symbol-at-point))
                     (t nil)))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Name for last keyboard macro: "
                                    obarray nil nil nil
                                    nil (symbol-name symb) t)))))
  (unless last-kbd-macro (error "No keyboard macro defined"))
  (when (and (fboundp symbol) (not (stringp (symbol-function symbol)))
             (not (vectorp (symbol-function symbol))))
    (error "Function `%s' is already defined and is not a keyboard macro"
           symbol))
  (if (string-equal symbol "")
      (error "No command name given"))
  (fset symbol last-kbd-macro))


;; REPLACES ORIGINAL in `macros.el':
;; Uses `completing-read' in interactive spec, with `symbol-nearest-point'
;; as default.
;;;###autoload
(defun insert-kbd-macro (macroname &optional keys)
  "Insert in buffer the definition of kbd macro MACRONAME, as Lisp code.
Optional second arg KEYS means also record the keys it is on
\(this is the prefix argument, when called interactively).

This Lisp code will, when executed, define the keyboard macro with the
same definition it has now.  If you say to record the keys, the Lisp
code will also rebind those keys to the macro.  Only global key
bindings are recorded since executing this Lisp code always makes
global bindings.

To save a keyboard macro, visit a file of Lisp code such as your
`~/.emacs', use this command, and then save the file."
  (interactive
   (let ((symb (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                     ((fboundp 'symbol-at-point) (symbol-at-point))
                     (t nil)))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Insert keyboard macro (name): " obarray
                                    'commandp nil nil
                                    nil (symbol-name symb) t))
           current-prefix-arg)))
  (let (definition)
    (if (string= "" (symbol-name macroname))
        (progn (setq macroname 'last-kbd-macro)
               (setq definition last-kbd-macro)
               (insert "(setq "))
      (setq definition (symbol-function macroname))
      (insert "(fset '"))
    (prin1 macroname (current-buffer)) (insert "\n   ")
    (if (stringp definition)
        (let ((beg (point)) end)
          (prin1 definition (current-buffer))
          (setq end (point-marker))
          (goto-char beg)
          (while (< (point) end)
            (let ((char (following-char)))
              (cond ((= char 0)
                     (delete-region (point) (1+ (point)))
                     (insert "\\C-@"))
                    ((< char 27)
                     (delete-region (point) (1+ (point)))
                     (insert "\\C-" (+ 96 char)))
                    ((= char ?\C-\\)
                     (delete-region (point) (1+ (point)))
                     (insert "\\C-\\\\"))
                    ((< char 32)
                     (delete-region (point) (1+ (point)))
                     (insert "\\C-" (+ 64 char)))
                    ((< char 127)
                     (forward-char 1))
                    ((= char 127)
                     (delete-region (point) (1+ (point)))
                     (insert "\\C-?"))
                    ((= char 128)
                     (delete-region (point) (1+ (point)))
                     (insert "\\M-\\C-@"))
                    ((= char (aref "\M-\C-\\" 0))
                     (delete-region (point) (1+ (point)))
                     (insert "\\M-\\C-\\\\"))
                    ((< char 155)
                     (delete-region (point) (1+ (point)))
                     (insert "\\M-\\C-" (- char 32)))
                    ((< char 160)
                     (delete-region (point) (1+ (point)))
                     (insert "\\M-\\C-" (- char 64)))
                    ((= char (aref "\M-\\" 0))
                     (delete-region (point) (1+ (point)))
                     (insert "\\M-\\\\"))
                    ((< char 255)
                     (delete-region (point) (1+ (point)))
                     (insert "\\M-" (- char 128)))
                    ((= char 255)
                     (delete-region (point) (1+ (point)))
                     (insert "\\M-\\C-?"))))))
      (if (vectorp definition)
          (let ((len (length definition)) (i 0) char mods)
            (while (< i len)
              (insert (if (zerop i) ?\[ ?\ ))
              (setq char (aref definition i)
                    i (1+ i))
              (cond ((not (numberp char))
                     (prin1 char (current-buffer)))
                    (t
                     (insert "?")
                     (setq mods (event-modifiers char)
                           char (event-basic-type char))
                     (while mods
                       (cond ((eq (car mods) 'control)
                              (insert "\\C-"))
                             ((eq (car mods) 'meta)
                              (insert "\\M-"))
                             ((eq (car mods) 'hyper)
                              (insert "\\H-"))
                             ((eq (car mods) 'super)
                              (insert "\\s-"))
                             ((eq (car mods) 'alt)
                              (insert "\\A-"))
                             ((and (eq (car mods) 'shift)
                                   (>= char ?a)
                                   (<= char ?z))
                              (setq char (upcase char)))
                             ((eq (car mods) 'shift)
                              (insert "\\S-")))
                       (setq mods (cdr mods)))
                     (cond ((= char ?\\)
                            (insert "\\\\"))
                           ((= char ?\")
                            (insert "\\\""))
                           ((= char ?\;)
                            (insert "\\;"))
                           ((= char 127)
                            (insert "\\C-?"))
                           ((< char 127)
                            (insert char))
                           (t (insert "\\" (format "%o" char)))))))
            (insert ?\]))
        (prin1 definition (current-buffer))))
    (insert ")\n")
    (when keys
      (let ((keys (where-is-internal macroname '(keymap))))
        (while keys
          (insert "(global-set-key ")
          (prin1 (car keys) (current-buffer)) (insert " '")
          (prin1 macroname (current-buffer)) (insert ")\n")
          (pop keys))))))


;; REPLACES ORIGINAL in `macros.el':
;; Make sure that `delete-selection-mode' is deactivated.  (See file
;; `delsel.el'.)  Otherwise, character self-insertion by the keyboard
;; macro could cause the region to be deleted (replaced by the
;; inserted text) when `apply-macro-to-region-lines' was finished
;; (e.g. via a delete-selection `pre-command-hook').
;;;###autoload
(defun apply-macro-to-region-lines (top bottom &optional macro)
  "For each complete line between point and mark, move to the beginning
of the line, and run the last keyboard macro.

When called from lisp, this function takes two arguments TOP and
BOTTOM, describing the current region.  TOP must be before BOTTOM.
The optional third argument MACRO specifies a keyboard macro to
execute.

This is useful for quoting or unquoting included text, adding and
removing comments, or producing tables where the entries are regular.

For example, in Usenet articles, sections of text quoted from another
author are indented, or have each line start with `>'.  To quote a
section of text, define a keyboard macro which inserts `>', put point
and mark at opposite ends of the quoted section, and use
`\\[apply-macro-to-region-lines]' to mark the entire section.

Suppose you wanted to build a keyword table in C where each entry
looked like this:

    { \"foo\", foo_data, foo_function },
    { \"bar\", bar_data, bar_function },
    { \"baz\", baz_data, baz_function },

You could enter the names in this format:

    foo
    bar
    baz

and write a macro to massage a word into a table entry:

    \\C-x (
       \\M-d { \"\\C-y\", \\C-y_data, \\C-y_function },
    \\C-x )

and then select the region of un-tablified names and use
`\\[apply-macro-to-region-lines]' to build the table from the names."
  (interactive "r")
  (unless macro
    (unless last-kbd-macro (error "No keyboard macro has been defined"))
    (setq macro last-kbd-macro))
  ;; Make sure that `delete-selection-mode' is deactivated.  (See
  ;; `delsel.el'.)  Otherwise, character self-insertion by the
  ;; keyboard macro could cause the region to be deleted (replaced by
  ;; the inserted text) when `apply-macro-to-region-lines' was
  ;; finished (e.g. via a delete-selection `pre-command-hook').
  (let ((delete-selection-mode nil))
    (save-excursion
      (let ((end-marker (progn (goto-char bottom) (beginning-of-line)
                               (point-marker)))
            next-line-marker)
        (goto-char top)
        (unless (bolp) (forward-line 1))
        (setq next-line-marker (point-marker))
        (while (< next-line-marker end-marker)
          (goto-char next-line-marker)
          (save-excursion (forward-line 1) (set-marker next-line-marker
                                                       (point)))
          (save-excursion
            (let ((mark-active nil))
              (execute-kbd-macro (or macro last-kbd-macro)))))
        (set-marker end-marker nil)
        (set-marker next-line-marker nil)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'macros+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macros+.el ends here
