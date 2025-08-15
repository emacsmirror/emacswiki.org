;;; apropos-fn+var.el --- Apropos for functions and variables  -*- lexical-binding: t -*-
;;
;; Filename: apropos-fn.el
;; Description: Apropos for functions and variables
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2018, Drew Adams, all rights reserved.
;; Created: Mon Nov 28 15:41:09 2005
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Fri Aug 15 11:35:55 2025 (-0700)
;;           By: dradams
;;     Update #: 462
;; URL: https://www.emacswiki.org/emacs/download/apropos-fn%2bvar.el
;; Keywords: apropos
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `button', `naked'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Standard `apropos' commands `apropos-variable' and
;;  `apropos-command' do not distinguish, by command name, between the
;;  different types of target object (but you can do that via `C-u').
;;  This library provides individual `apropos' commands for user
;;  options, variables in general (not just options), and functions in
;;  general (not just commands).  
;;
;;  Commands defined here:
;;
;;    `apropos-function', `apropos-option', `apropos-variable'.
;;
;;  Faces defined here:
;;
;;    `apropos-option-button' (Emacs 22-24.3).
;;
;;  Button types defined here:
;;
;;    `apropos-user-option' (Emacs 22-24.3).
;;
;;
;;  ***** NOTE: The following functions defined in `apropos.el' have
;;              been REDEFINED HERE:
;;
;;  `apropos-variable' - See above (the standard command does what
;;                       `apropos-option' does here).
;;  `apropos-print'    - Identify user options with label `Option'.
;;                       Use `naked-key-description', if available.
;;
;;
;;  Acknowledgment: Slightly different versions of `apropos-function'
;;  and `apropos-variable' were posted by Kevin Rodgers to
;;  bug-gnu-emacs, Tue, 06 Sep 2005 14:34:54 -0600.  Kevin didn't
;;  actually redefine `apropos-variable' (he would never do that ;-)),
;;  but he provided the new definition.  I redefined `apropos-print'
;;  (and added button type `apropos-user-option' for Emacs < 24.4).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2025/08/15 dadams
;;     Added lexical-binding cookie.
;;     apropos-print: Added version for Emacs 30+.
;; 2015/04/25 dadams
;;     Removed: apropos-print--1 (Emacs 24.4+ does not use with-help-window after all.)
;;     Renamed per Emacs 24.4+: face apropos-option to apropos-option-button,
;;                              button type apropos-option to apropos-user-option.
;;       Define them only for Emacs 22-24.3.
;;     apropos-print: Cleanup for Emacs 22-23.  Redid Emacs 24+ version to not use apropos-print--1.
;;     Removed commented-out button-type definitions.
;; 2014/05/04 dadams
;;     Added: apropos-print--1 - factored out from apropos-print after next update:
;;     apropos-print: Updated for Emacs 24.4: Use with-help-window if defined.  See bug #17109.
;; 2013/10/27 dadams
;;     apropos-print: Updated for Emacs 24.4+: apropos-macrop -> macrop.
;; 2012/05/11 dadams
;;     apropos-print: Updated for Emacs 24.
;; 2012/03/31 dadams
;;     Button apropos-option: Added properties face and apropos-short-label (same as var).
;; 2011/10/07 dadams
;;     Added soft require of naked.el.
;;     apropos-print: Use naked-key-description if available.
;; 2011/03/31 dadams
;;     apropos-print: Added Emacs 24+ version.
;; 2006/03/03 dadams
;;     Updated to latest Emacs 22 CVS version:
;;       apropos-orig-regexp was renamed to apropos-pattern.
;;       apropos-print now has an additional optional arg.
;; 2006/02/25 dadams
;;     apropos-variable: Added ignored optional arg, for compatibility.
;; 2005/11/29 dadams
;;     Added redefinition of apropos-print and button type apropos-option.
;;     Made arg to apropos-function and apropos-variable mandatory.
;; 2005/11/28 dadams
;;     Redefined apropos-variable. Defined apropos-option as old version.
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'apropos)

(require 'naked nil t) ;; (no error if not found): naked-key-description

;; Quiet byte compiler
(defvar apropos-compact-layout)
(eval-when-compile (defvar apropos-item)) ; Emacs 22+ (but really should be lexical in Emacs 30+)
(defvar apropos-keybinding-face) ; Emacs < 22
(defvar apropos-label-face) ; Emacs < 22
(defvar apropos-symbol-face) ; Emacs < 22
(defvar apropos-multi-type)
(defvar apropos-pattern)
(defvar apropos-sort-by-scores)
(defvar help-window-select)             ; `help.el', Emacs 23+

;;;;;;;;;;;;;;;;;;;;;;;;

(if (< emacs-major-version 22)

    (defun apropos-function (pattern)
      "Show functions that match PATTERN (a regular expression).
This includes functions that are not commands."
      (interactive "i")                 ; Ignored when interactive
      (if (interactive-p)
          (let ((apropos-do-all  t)) (call-interactively 'apropos-command))
        (apropos-command pattern t)))

  (defun apropos-function (pattern)
    "Show functions that match PATTERN.
This includes functions that are not commands.
PATTERN can be a word, a list of words (separated by spaces),
or a regexp (using some regexp special characters).  If it is a word,
search for matches for that word as a substring.  If it is a list of words,
search for matches for any two (or more) of those words.

When called from a Lisp program, a string PATTERN is used as a regexp,
while a list of strings is used as a word list."
    (interactive "i")                   ; Ignored when interactive
    (if (interactive-p)
        (let ((apropos-do-all  t)) (call-interactively 'apropos-command))
      (apropos-command pattern t)))
  )


;;; REPLACE ORIGINAL defined in `apropos.el'.
;;; Allow for non user-option variables too.
;;; Rename original command as `apropos-option'.
;;;
(unless (fboundp 'apropos-option) (fset 'apropos-option (symbol-function 'apropos-variable)))

(if (< emacs-major-version 22)

    (defun apropos-variable (pattern &optional _ignore)
      "Show variables that match PATTERN (a regular expression).
This includes variables that are not user options."
      (interactive "i")                 ; Ignored when interactive
      (if (interactive-p)
          (let ((apropos-do-all  t)) (call-interactively 'apropos-option))
        (apropos-option pattern t)))

  (defun apropos-variable (pattern &optional _ignore)
    "Show variables that match PATTERN.
This includes variables that are not user options.
PATTERN can be a word, a list of words (separated by spaces),
or a regexp (using some regexp special characters).  If it is a word,
search for matches for that word as a substring.  If it is a list of
words, search for matches for any two (or more) of those words."
    (interactive "i")                   ; Ignored when interactive
    (if (interactive-p)
        (let ((apropos-do-all  t)) (call-interactively 'apropos-option))
      (apropos-option pattern t)))
  )


;;; REPLACE ORIGINAL defined in `apropos.el'.
;;;
;;; 1. Use label "Option" for user options.
;;; 2. Use `naked-key-description' if available.
;;;
(cond ((< emacs-major-version 22)       ; Emacs 20 and 21.
       (defun apropos-print (do-keys spacing)
         "Output result of apropos searching into buffer `*Apropos*'.
The value of `apropos-accumulator' is the list of items to output.
Each element should have the format (SYMBOL FN-DOC VAR-DOC [PLIST-DOC]).
The return value is the list that was in `apropos-accumulator', sorted
alphabetically by symbol name; but this function also sets
`apropos-accumulator' to nil before returning."
         (if (null apropos-accumulator)
             (message "No apropos matches for `%s'" apropos-regexp)
           (setq apropos-accumulator  (sort apropos-accumulator (lambda (a b)
                                                                  (string-lessp (car a) (car b)))))
           (and apropos-label-face
                (symbolp apropos-label-face)
                (setq apropos-label-face  `(face ,apropos-label-face mouse-face highlight)))
           (with-output-to-temp-buffer "*Apropos*"
             (let ((p            apropos-accumulator)
                   (old-buffer   (current-buffer))
                   symbol  item  point1  point2)
               (set-buffer standard-output)
               (apropos-mode)
               (when window-system
                 (insert "If you move the mouse over text that changes color,\n"
                         (substitute-command-keys
                          "you can click \\[apropos-mouse-follow] to get more information.\n")))
               (insert (substitute-command-keys
                        "In this buffer, type \\[apropos-follow] to get full documentation.\n\n"))
               (while (consp p)
                 (or (not spacing)  (bobp)  (terpri))
                 (setq apropos-item  (car p)
                       symbol        (car apropos-item)
                       p             (cdr p)
                       point1        (point))
                 (princ symbol)         ; print symbol name
                 (setq point2  (point))
                 (and do-keys ; Calculate key-bindings if we want them.
                      (commandp symbol)
                      (indent-to 30 1)
                      (if (let ((keys  (save-excursion (set-buffer old-buffer)
                                                       (where-is-internal symbol)))
                                filtered)
                            ;; Copy over the list of key sequences,
                            ;; omitting any that contain a buffer or a frame.
                            (while keys
                              (let ((key  (car keys))
                                    (i    0)
                                    loser)
                                (while (< i (length key))
                                  (when (or (framep (aref key i))  (bufferp (aref key i)))
                                    (setq loser  t))
                                  (setq i  (1+ i)))
                                (unless loser (setq filtered  (cons key filtered))))
                              (setq keys  (cdr keys)))
                            (setq item  filtered))
                          ;; Convert the remaining keys to a string and insert.
                          (insert (mapconcat (lambda (key)
                                               (setq key  (condition-case ()
                                                              (if (fboundp 'naked-key-description)
                                                                  (naked-key-description key)
                                                                (key-description key))
                                                            (error)))
                                               (when apropos-keybinding-face
                                                 (put-text-property 0 (length key)
                                                                    'face apropos-keybinding-face
                                                                    key))
                                               key)
                                             item
                                             ", "))
                        (insert "M-x")
                        (put-text-property (- (point) 3) (point) 'face apropos-keybinding-face)
                        (insert " " (symbol-name symbol) " ")
                        (insert "RET")
                        (put-text-property (- (point) 3) (point) 'face apropos-keybinding-face)))
                 (terpri)
                 ;; only now so we don't propagate text attributes all over
                 (put-text-property point1 point2 'item
                                    (if (eval `(or ,@(cdr apropos-item)))
                                        (car apropos-item)
                                      apropos-item))
                 (when apropos-symbol-face
                   (put-text-property point1 point2 'face apropos-symbol-face))
                 (apropos-print-doc 'describe-function 1 (if (commandp symbol)
                                                             "Command"
                                                           (if (apropos-macrop symbol)
                                                               "Macro"
                                                             "Function"))
                                    t)
                 ;; We used to use `customize-variable-other-window' instead for a customizable
                 ;; variable, but that is slow.  It is better to show an ordinary help buffer
                 ;; and let the user click on the customization button in that buffer, if he
                 ;; wants to.  Likewise for `customize-face-other-window'.
                 (apropos-print-doc 'describe-variable 2 (if (user-variable-p symbol)
                                                             "Option"
                                                           "Variable")
                                    t)
                 (apropos-print-doc 'customize-group-other-window 6 "Group" t)
                 (apropos-print-doc 'describe-face 5 "Face" t)
                 (apropos-print-doc 'widget-browse-other-window 4 "Widget" t)
                 (apropos-print-doc 'apropos-describe-plist 3 "Plist" nil))
               (setq buffer-read-only  t))))
         (prog1 apropos-accumulator (setq apropos-accumulator  ())))) ; Permit gc.

      ((< emacs-major-version 24)       ; Emacs 22 and 23.
       (defun apropos-print (do-keys spacing &optional text nosubst)
         "Output result of apropos searching into buffer `*Apropos*'.
The value of `apropos-accumulator' is the list of items to output.
Each element should have the format
 (SYMBOL SCORE FN-DOC VAR-DOC [PLIST-DOC WIDGET-DOC FACE-DOC GROUP-DOC]).
The return value is the list that was in `apropos-accumulator', sorted
alphabetically by symbol name; but this function also sets
`apropos-accumulator' to nil before returning.

If SPACING is non-nil, it should be a string; separate items with that string.
If non-nil TEXT is a string that will be printed as a heading."
         (if (null apropos-accumulator)
             (message "No apropos matches for `%s'" apropos-pattern)
           (setq apropos-accumulator  (sort apropos-accumulator
                                            (lambda (a b)
                                              ;; Don't sort by score if user can't see the score.
                                              ;; It would be confusing.  -- rms.
                                              (if apropos-sort-by-scores
                                                  (or (> (cadr a) (cadr b))
                                                      (and (= (cadr a) (cadr b))
                                                           (string-lessp (car a) (car b))))
                                                (string-lessp (car a) (car b))))))
           (with-output-to-temp-buffer "*Apropos*"
             (let ((p           apropos-accumulator)
                   (old-buffer  (current-buffer))
                   symbol  item)
               (set-buffer standard-output)
               (apropos-mode)
               (when (display-mouse-p)
                 (insert "If moving the mouse over text changes the text's color, "
                         "you can click\n"
                         "mouse-2 or use `RET' on that text to get more information.\n"))
               (insert "In this buffer, go to the name of the command, function, or variable,\n"
                       (substitute-command-keys
                        "and type \\[apropos-follow] to get full documentation.\n\n"))
               (when text (insert text "\n\n"))
               (dolist (apropos-item p)
                 (when (and spacing  (not (bobp))) (princ spacing))
                 (setq symbol  (car apropos-item))
                 ;; Insert dummy score element for backwards compatibility with 21.x
                 ;; `apropos-item' format.
                 (unless (numberp (cadr apropos-item))
                   (setq apropos-item  (cons (car apropos-item) (cons nil (cdr apropos-item)))))
                 (insert-text-button (symbol-name symbol) 'type 'apropos-symbol
                                     'skip (and (boundp 'apropos-multi-type)  apropos-multi-type)
                                     ;; Cannot use default, since user may have changed the var.
                                     'face apropos-symbol-face)
                 (when (and (eq apropos-sort-by-scores 'verbose)  (cadr apropos-item))
                   (insert " (" (number-to-string (cadr apropos-item)) ") "))
                 ;; Calculate key-bindings if we want them.
                 (unless (and (boundp 'apropos-compact-layout)  apropos-compact-layout)
                   (and do-keys
                        (commandp symbol)
                        (not (eq symbol 'self-insert-command))
                        (indent-to 30 1)
                        (if (let ((keys  (with-current-buffer old-buffer (where-is-internal symbol)))
                                  filtered)
                              ;; Copy over the list of key sequences,
                              ;; omitting any that contain a buffer or a frame.
                              ;; FIXME: Why omit keys that contain buffers and
                              ;; frames?  This looks like a bad workaround rather
                              ;; than a proper fix.  Does anybod know what problem
                              ;; this is trying to address?  --Stef
                              (dolist (key keys)
                                (let ((i  0)
                                      loser)
                                  (while (< i (length key))
                                    (when (or (framep (aref key i))  (bufferp (aref key i)))
                                      (setq loser  t))
                                    (setq i  (1+ i)))
                                  (unless loser (push key filtered))))
                              (setq item  filtered))
                            ;; Convert the remaining keys to a string and insert.
                            (insert (mapconcat
                                     (lambda (key)
                                       (setq key  (condition-case ()
                                                      (if (fboundp 'naked-key-description)
                                                          (naked-key-description key)
                                                        (key-description key))
                                                    (error)))
                                       (when apropos-keybinding-face
                                         (put-text-property 0 (length key)
                                                            'face apropos-keybinding-face
                                                            key))
                                       key)
                                     item
                                     ", "))
                          (insert "M-x ... RET")
                          (when apropos-keybinding-face
                            (put-text-property (- (point) 11) (- (point) 8)
                                               'face apropos-keybinding-face)
                            (put-text-property (- (point) 3) (point)
                                               'face apropos-keybinding-face))))
                   (terpri))
                 (apropos-print-doc 2 (if (commandp symbol)
                                          'apropos-command
                                        (if (apropos-macrop symbol)
                                            'apropos-macro
                                          'apropos-function))
                                    (not nosubst))
                 (apropos-print-doc 3 (if (user-variable-p symbol)
                                          'apropos-user-option
                                        'apropos-variable)
                                    (not nosubst))
                 (apropos-print-doc 7 'apropos-group t)
                 (apropos-print-doc 6 'apropos-face t)
                 (apropos-print-doc 5 'apropos-widget t)
                 (apropos-print-doc 4 'apropos-plist nil))
               (set (make-local-variable 'truncate-partial-width-windows) t)
               (set (make-local-variable 'truncate-lines) t)
               (setq buffer-read-only  t))))
         (prog1 apropos-accumulator (setq apropos-accumulator  ())))) ; Permit gc.

      ((< emacs-major-version 30)       ; Emacs 24-29.
       (defun apropos-print (do-keys spacing &optional text nosubst)
         "Output result of apropos searching into buffer `*Apropos*'.
The value of `apropos-accumulator' is the list of items to output.
Each element should have the format
 (SYMBOL SCORE FN-DOC VAR-DOC [PLIST-DOC WIDGET-DOC FACE-DOC GROUP-DOC]).
The return value is the list that was in `apropos-accumulator', sorted
alphabetically by symbol name; but this function also sets
`apropos-accumulator' to nil before returning.

If SPACING is non-nil, it should be a string; separate items with that string.
If non-nil, TEXT is a string that will be printed as a heading."
         (if (null apropos-accumulator)
             (message "No apropos matches for `%s'" apropos-pattern)
           (setq apropos-accumulator  (sort apropos-accumulator
                                            (lambda (a b)
                                              (if apropos-sort-by-scores
                                                  (or (> (cadr a) (cadr b))
                                                      (and (= (cadr a) (cadr b))
                                                           (string-lessp (car a) (car b))))
                                                (string-lessp (car a) (car b))))))
           (with-output-to-temp-buffer "*Apropos*"
             (let ((p                  apropos-accumulator)
                   (old-buffer         (current-buffer))
                   (inhibit-read-only  t)
                   (button-end         0)
                   symbol item)
               (set-buffer standard-output)
               (apropos-mode)
               (insert (substitute-command-keys "Type \\[apropos-follow] on ")
                       (if apropos-multi-type "a type label" "an entry")
                       " to view its full documentation.\n\n")
               (when text (insert text "\n\n"))
               (dolist (apropos-item p)
                 (when (and spacing  (not (bobp))) (princ spacing))
                 (setq symbol  (car apropos-item))
                 ;; Insert dummy score element for backwards compatibility with 21.x
                 ;; `apropos-item' format.
                 (unless (numberp (cadr apropos-item))
                   (setq apropos-item  (cons (car apropos-item) (cons nil (cdr apropos-item)))))
                 (when (= (point) button-end) (terpri))
                 (insert-text-button (symbol-name symbol)     'type 'apropos-symbol
                                     'skip apropos-multi-type 'face 'apropos-symbol)
                 (setq button-end  (point))
                 (when (and (eq apropos-sort-by-scores 'verbose)  (cadr apropos-item))
                   (insert " (" (number-to-string (cadr apropos-item)) ") "))
                 ;; Calculate key-bindings if we want them.
                 (unless apropos-compact-layout
                   (and do-keys
                        (commandp symbol)
                        (not (eq symbol 'self-insert-command))
                        (indent-to 30 1)
                        (if (let ((keys  (with-current-buffer old-buffer (where-is-internal symbol)))
                                  filtered)
                              ;; Copy over the list of key sequences,
                              ;; omitting any that contain a buffer or a frame.
                              ;; FIXME: Why omit keys that contain buffers and
                              ;; frames?  This looks like a bad workaround rather
                              ;; than a proper fix.  Does anybody know what problem
                              ;; this is trying to address?  --Stef
                              (dolist (key keys)
                                (let ((i  0)
                                      loser)
                                  (while (< i (length key))
                                    (when (or (framep (aref key i))  (bufferp (aref key i)))
                                      (setq loser  t))
                                    (setq i  (1+ i)))
                                  (unless loser (push key filtered))))
                              (setq item  filtered))
                            ;; Convert the remaining keys to a string and insert.
                            (insert (mapconcat
                                     (lambda (key)
                                       (setq key  (condition-case ()
                                                      (if (fboundp 'naked-key-description)
                                                          (naked-key-description key)
                                                        (key-description key))
                                                    (error)))
                                       (put-text-property 0 (length key) 'face 'apropos-keybinding
                                                          key)
                                       key)
                                     item
                                     ", "))
                          (insert "M-x ... RET")
                          (put-text-property (- (point) 11) (- (point) 8) 'face 'apropos-keybinding)
                          (put-text-property (- (point) 3) (point) 'face 'apropos-keybinding)))
                   (terpri))
                 (apropos-print-doc 2
                                    (if (commandp symbol)
                                        'apropos-command
                                      ;; Emacs 24.4 moved `apropos-macrop' to `macrop'.
                                      (if (if (fboundp 'macrop) (macrop symbol) (apropos-macrop symbol))
                                          'apropos-macro
                                        'apropos-function))
                                    (not nosubst))
                 (apropos-print-doc 3
                                    (if (custom-variable-p symbol)
                                        'apropos-user-option
                                      'apropos-variable)
                                    (not nosubst))
                 (apropos-print-doc 7 'apropos-group t)
                 (apropos-print-doc 6 'apropos-face t)
                 (apropos-print-doc 5 'apropos-widget t)
                 (apropos-print-doc 4 'apropos-plist nil))
               (set (make-local-variable 'truncate-partial-width-windows) t)
               (set (make-local-variable 'truncate-lines) t))))
         (prog1 apropos-accumulator (setq apropos-accumulator  ())))) ; Permit gc.

      (t                                ; Emacs 30+
       (defun apropos-print (do-keys spacing &optional text nosubst)
         "Output result of apropos searching into buffer `*Apropos*'.
The value of `apropos-accumulator' is the list of items to output.
Each element should have the format
 (SYMBOL SCORE FN-DOC VAR-DOC [PLIST-DOC WIDGET-DOC FACE-DOC GROUP-DOC]).
The return value is the list that was in `apropos-accumulator', sorted
alphabetically by symbol name; but this function also sets
`apropos-accumulator' to nil before returning.

If SPACING is non-nil, it should be a string; separate items with that string.
If non-nil, TEXT is a string that will be printed as a heading."
         (if (null apropos-accumulator)
             (message "No apropos matches for `%s'" apropos-pattern)
           (setq apropos-accumulator  (sort apropos-accumulator
                                            (lambda (a b)
                                              (if apropos-sort-by-scores
                                                  (or (> (cadr a) (cadr b))
                                                      (and (= (cadr a) (cadr b))
                                                           (string-lessp (car a) (car b))))
                                                (string-lessp (car a) (car b))))))
           (with-output-to-temp-buffer "*Apropos*"
             (let ((p                  apropos-accumulator)
                   (old-buffer         (current-buffer))
                   (inhibit-read-only  t)
                   (button-end         0)
                   (first              t)
                   symbol item)
               (set-buffer standard-output)
               (apropos-mode)
               (apropos--preamble text)
               (dolist (apropos-item p)
                 (if (and spacing  (not first)) (princ spacing) (setq first  nil))
                 (setq symbol  (car apropos-item))
                 ;; Insert dummy score element for backwards compatibility with 21.x
                 ;; `apropos-item' format.
                 (unless (numberp (cadr apropos-item))
                   (setq apropos-item  (cons (car apropos-item) (cons nil (cdr apropos-item)))))
                 (when (= (point) button-end) (terpri))
                 (insert-text-button (symbol-name symbol) 'type          'apropos-symbol
                                     'skip          apropos-multi-type
                                     'face          'apropos-symbol
                                     'outline-level 1)
                 (setq button-end  (point))
                 (when (and (eq apropos-sort-by-scores 'verbose)  (cadr apropos-item))
                   (insert " (" (number-to-string (cadr apropos-item)) ") "))
                 ;; Calculate key-bindings if we want them.
                 (unless apropos-compact-layout
                   (and do-keys
                        (commandp symbol)
                        (not (eq symbol 'self-insert-command))
                        (indent-to 30 1)
                        (if (let ((keys  (with-current-buffer old-buffer (where-is-internal symbol)))
                                  filtered)
                              ;; Copy over the list of key sequences,
                              ;; omitting any that contain a buffer or a frame.
                              ;; FIXME: Why omit keys that contain buffers and
                              ;; frames?  This looks like a bad workaround rather
                              ;; than a proper fix.  Does anybody know what problem
                              ;; this is trying to address?  --Stef
                              (dolist (key keys)
                                (let ((i  0)
                                      loser)
                                  (while (< i (length key))
                                    (when (or (framep (aref key i))  (bufferp (aref key i)))
                                      (setq loser  t))
                                    (setq i  (1+ i)))
                                  (unless loser (push key filtered))))
                              (setq item  filtered))
                            ;; Convert the remaining keys to a string and insert.
                            (insert (mapconcat
                                     (lambda (key)
                                       (setq key  (condition-case ()
                                                      (if (fboundp 'naked-key-description)
                                                          (naked-key-description key)
                                                        (key-description key))
                                                    (error)))
                                       (put-text-property 0 (length key) 'face 'apropos-keybinding key)
                                       key)
                                     item
                                     ", "))
                          (insert "M-x ... RET")
                          (put-text-property (- (point) 11) (- (point) 8) 'face 'apropos-keybinding)
                          (put-text-property (- (point) 3) (point) 'face 'apropos-keybinding)))
                   (terpri))
                 (apropos-print-doc apropos-item
                                    2
                                    (if (commandp symbol)
                                        'apropos-command
                                      (if (macrop symbol) 'apropos-macro 'apropos-function))
                                    (not nosubst))
                 (apropos-print-doc apropos-item
                                    3
                                    (if (custom-variable-p symbol)
                                        'apropos-user-option
                                      'apropos-variable)
                                    (not nosubst))
                 ;; Insert an excerpt of variable values.
                 (when (boundp symbol)
                   (insert "  Value: ")
                   (let* ((print-escape-newlines  t)
                          (value                  (prin1-to-string (symbol-value symbol)))
                          (truncated              (truncate-string-to-width
                                                   value (- (window-width) 20) nil nil t)))
                     (insert truncated)
                     (unless (equal value truncated)
                       (buttonize-region (1- (point)) (point) (lambda (_) (message "Value: %s" value))))
                     (insert "\n")))
                 (apropos-print-doc apropos-item 7 'apropos-group t)
                 (apropos-print-doc apropos-item 6 'apropos-face t)
                 (apropos-print-doc apropos-item 5 'apropos-widget t)
                 (apropos-print-doc apropos-item 4 'apropos-plist nil))
               (set (make-local-variable 'truncate-partial-width-windows) t)
               (set (make-local-variable 'truncate-lines) t)))
           (when help-window-select (select-window (get-buffer-window "*Apropos*"))))
         (prog1 apropos-accumulator (setq apropos-accumulator  ())))) ; Permit gc.

      )

(when (and (> emacs-major-version 21)  (or (< emacs-major-version 24)
                                           (and (= emacs-major-version 24)
                                                (< emacs-minor-version 4))))
  (defface apropos-user-option-button '((t (:inherit font-lock-variable-name-face)))
    "Face used for option names in Apropos buffers."
    :group 'apropos)

  (define-button-type 'apropos-user-option
      'apropos-label "Option"
      'apropos-short-label "o"
      'face 'apropos-user-option-button
      'help-echo "mouse-2, RET: Display more help on this user option (variable)"
      'follow-link t
      'action (lambda (button) (describe-variable (button-get button 'apropos-symbol))))

  )

;;;;;;;;;;;;;;;;;;;;

(provide 'apropos-fn+var)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apropos-fn.el ends here
