;;; hi-var.el --- Highlight Emacs-Lisp variables. -*- lexical-binding:t -*-
;;
;; Filename: hi-var.el
;; Description: Highlight Emacs-Lisp variables.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2021-2022, Drew Adams, all rights reserved.
;; Created: Mon Jan 17 11:16:28 2022 (-0800)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Wed Jan 19 10:49:04 2022 (-0800)
;;           By: dradams
;;     Update #: 167
;; URL: https://www.emacswiki.org/emacs/download/hi-var.el
;; Doc URL: https://emacswiki.org/emacs/HighlightElispVariables
;; Keywords: highlight, lisp, variables, lexical, file-local
;; Compatibility: GNU Emacs: 24.x, 25.x, 26.x, 27.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Highlight Emacs-Lisp variables.
;;
;;  `hi-var-mode' is a minor mode that highlights, in the current
;;  buffer, symbols that are known to be Emacs-Lisp variables.
;;
;;  Command `hi-var-mode' toggles the highlighting on/off.  The buffer
;;  should be in Emacs-Lisp mode.
;;
;;  `hi-var-mode' uses face `hivar-options' to highlight user options,
;;  and face `hivar-dynamic-vars' for other dynamic variables (also
;;  called "special" variables).  It uses face `hivar-file-local-vars'
;;  to file-local variables, and face `hivar-buffer-local-vars' for
;;  other buffer-local variables.
;;
;;  It merges `hivar-options' or `hivar-dynamic-vars' with
;;  `hivar-file-local-vars' or `hivar-buffer-local-vars', so you can
;;  tell when a dynamic var is buffer- or file-local.  By default, the
;;  former pair of faces use an underline and the latter pair use an
;;  overline.
;;
;;  If you don't want to highlight all of these types of variables,
;;  you can use option `hivar-highlighting' to choose the types you do
;;  want to highlight.
;;
;;  The simple built-in test `special-variable-p' is used, to tell
;;  whether a variable is dynamically bound.  That test is not 100%
;;  reliable.  It doesn't respect vacuous `defvar' sexps, which
;;  declare a variable to be special in a given context, without
;;  assigning a value to the variable.  Instead, it uses `defvar',
;;  `defconst', and `defcustom' sexps with a value arg present.
;;
;;  See also the related library `hl-defined.el', which highlights
;;  defined or undefined Emacs-Lisp symbols (functions, variables, or
;;  both).  You can use these two libraries together - command
;;  `hdefd-highlight-mode' to highlight functions and/or variables,
;;  and command `hi-var-mode' to highlight dynamic variables (with a
;;  different face).
;;
;;
;;  Put this in your init file, after adding `hi-var.el' to your
;;  `load-path':
;;
;;    (require 'hi-var)
;;
;;  If you want to turn on this highlighting automatically whenever
;;  you enter Emacs-Lisp mode then you can do this in your init file:
;;
;;    (require 'hi-var)
;;    (add-hook 'emacs-lisp-mode-hook 'hi-var-mode 'APPEND)
;;
;;  NOTE:
;;    Library `hi-var.el' supersedes library `dyna-show.el', which
;;    highlights dynamic variables but doesn't highlight file-local
;;    and buffer-local variables.
;;
;;
;;  Faces defined here:
;;
;;    `hivar-buffer-local-vars', `hivar-dynamic-vars',
;;    `hivar-file-local-vars', `hivar-options'.
;;
;;  Commands defined here:
;;
;;    `hi-var-mode'.
;;
;;  Non-interactive functions defined here:
;;
;;    `hivar-highlight-dynamic', `hivar-highlight-file/buf-local'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2022/01/19 dadams
;;     Created (from dyna-show.el).
;; 2022/01/17 dadams
;;     dyna-highlight: Move over strings, like symbols, with forward-sexp.
;; 2021/09/01 dadams
;;     Added face dyna-options.
;;     dyna-highlight: Highlight options with face dyna-options.
;; 2021/08/15 dadams
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

;; Quiet the byte-compiler.

(defvar hdefd-highlight-mode)
(declare-function hdefd-highlight-mode "hl-defined.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgroup hi-var nil
  "Highlight variables in Emacs-Lisp mode, according to their type.
Highlight user options, other dynamic (\"special\") vars, and
file-local vars."
  :prefix "hivar-"
  :group 'editing :group 'faces :group 'matching :group 'font-lock :group 'programming
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
hi-var.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
                   "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
                   "https://www.emacswiki.org/emacs/download/hi-var.el")
  :link '(url-link :tag "Description" "https://emacswiki.org/emacs/HighlightDynamicVariables")
  :link '(emacs-commentary-link :tag "Commentary" "hi-var"))

;;;###autoload
(defcustom hivar-highlighting '(buffer-local file-local options dynamic)
  "Types of variables to highlight.
If you change the option value when `hi-var-mode' is enabled, then
toggle it off and on again to pick up the new value.

The value is a list of up to four variable types.  The possible types:

* Buffer-local variables          (Lisp value `buffer-local')
* File-local variables            (Lisp value `file-local')
* Dynamic(\"special\") variables  (Lisp value `dynamic')
* User options                    (Lisp value `options')

File-local vars are highlighted as buffer-local, if file-local is not
specified but buffer-local is.

User options are highlighted as dynamic, if options is not specified
by dynamic is.

\(If the list length is greater than four, only the first four types
are counted,)"
  :group 'hi-var :type '(set (const :tag "Buffer-local variables"          buffer-local)
                             (const :tag "File-local variables"            file-local)
                             (const :tag "Dynamic (\"special\") variables" dynamic)
                             (const :tag "User options"                    options)))

(defface hivar-buffer-local-vars
  '((((class color) (min-colors 88))
     (:overline "DarkGray"))
    (t :overline t))
  "Face used to highlight buffer-local Emacs-Lisp variables.
This is merged with that of `hivar-dynamic-vars' or `hivar-options',
if appropriate."
  :group 'matching :group 'font-lock :group 'programming :group 'faces)

(defface hivar-dynamic-vars
  '((((class color) (min-colors 88))
     (:underline (:color "Red" :style wave)))
    (t :underline t))
  "Face used to highlight dynamic (\"special\") Emacs-Lisp variables.
This is merged with that of `hivar-buffer-local-vars' or
`hivar-file-local-vars', if appropriate."
  :group 'matching :group 'font-lock :group 'programming :group 'faces)

(defface hivar-file-local-vars
  '((((class color) (min-colors 88))
     (:overline "Orange"))
    (t :overline t))
  "Face used to highlight file-local Emacs-Lisp variables.
\(A file-local variable is also buffer-locak when visiting its buffer.)
This is merged with that of `hivar-dynamic-vars' or `hivar-options',
if appropriate."
  :group 'matching :group 'font-lock :group 'programming :group 'faces)

(defface hivar-options
  '((((class color) (min-colors 88))
     (:underline (:color "Green" :style wave)))
    (t :underline t))
  "Face used to highlight user options.
This is merged with that of `hivar-buffer-local-vars' or
`hivar-file-local-vars', if appropriate."
  :group 'matching :group 'font-lock :group 'programming :group 'faces)


(if (boundp 'hdefd--face)
    (defvaralias 'hivar--face 'hdefd--face nil)
  (defvar hivar--face))

;;;###autoload
(define-minor-mode hi-var-mode
  "Toggle highlighting Emacs-Lisp variables in the buffer.
The current buffer should be in Emacs-Lisp mode.
With prefix ARG, turn the mode on if ARG is positive, off otherwise.

Highlighting uses faces `hivar-buffer-local-vars',
`hivar-dynamic-vars', `hivar-file-local-vars', and `hivar-options', to
highlight buffer-local, dynamic (\"special\"), file-local, and
user-option variables, respectively.

Face `hivar-options' has priority over `hivar-dynamic-vars'.  Face
`hivar-file-local-vars' has priority over `hivar-buffer-local-vars'.
Face `hivar-options' or `hivar-dynamic-vars' is merged (combined) with
`hivar-file-local-vars' or `hivar-buffer-local-vars', when a varis
both dynamic and either file-local or buffer-local."
  :group 'hi-var
  (cond ((not hi-var-mode)
         (font-lock-remove-keywords
          nil '((hivar-highlight-file/buf-local (0 hivar--face prepend))))
         (font-lock-remove-keywords
          nil '((hivar-highlight-dynamic (0 hivar--face prepend)))))
        (t
         (cond ((or (not (boundp 'hdefd-highlight-mode))  (not  hdefd-highlight-mode))
                (font-lock-add-keywords
                 nil '((hivar-highlight-file/buf-local (0 hivar--face prepend))) 'APPEND)
                (font-lock-add-keywords
                 nil '((hivar-highlight-dynamic (0 hivar--face prepend))) 'APPEND))
               (t
                (hdefd-highlight-mode 'toggle)
                (font-lock-add-keywords
                 nil '((hivar-highlight-file/buf-local (0 hivar--face prepend))) 'APPEND)
                (font-lock-add-keywords
                 nil '((hivar-highlight-dynamic (0 hivar--face prepend))) 'APPEND)
                (hdefd-highlight-mode 'toggle)))))
  (when font-lock-mode (font-lock-mode -1))
  (font-lock-mode 1)
  (when (called-interactively-p 'interactive)
    (message "Highlighting Emacs-Lisp variables is now %s" (if hi-var-mode "ON" "OFF"))))

(defun hivar-highlight-dynamic (_limit)
  "Highlight Emacs-Lisp dynamic variables.
Highlight user options with face `hivar-options'.
Highlight other dynamic variables with face `hivar-dynamic-vars'.

Used as a font-lock MATCHER function for `hi-var-mode'.

This function is a no-op if `hivar-highlighting' specifies neither
`dynamic' nor `options'."
  (let ((opoint  (point))
        (found   nil)
        (dyn-p   (memq 'dynamic hivar-highlighting))
        (opt-p   (memq 'options hivar-highlighting)))
    (when (or dyn-p  opt-p)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (while (and (not found)  (not (eobp)))
          (cond ((condition-case nil
                     (save-excursion
                       (skip-chars-forward "' \t\n")
                       (setq opoint  (point))
                       (let ((obj  (read (current-buffer))))
                         (and (symbolp obj)
                              (special-variable-p obj)
                              (not (memq obj '(nil t)))
                              (progn (set-match-data (list opoint (point)))
                                     (setq hivar--face  (if (and opt-p
                                                                 (custom-variable-p obj))
                                                            'hivar-options
                                                          (and dyn-p  'hivar-dynamic-vars)))
                                     t))))
                   (error nil))
                 (forward-sexp 1)
                 (setq opoint  (point)
                       found   t))
                (t
                 (if (looking-at "\\(\\sw\\|\\s_\\|[\"]\\)") ; Avoid going inside a string.
                     (forward-sexp 1)
                   (forward-char 1)))))
        found))))

(defun hivar-highlight-file/buf-local (_limit)
  "Highlight Emacs-Lisp buffer-local and file-local vars.
Highlight buffer-local vars with face `hivar-buffer-local-vars'.
Highlight file-local vars with face `hivar-file-local-vars'.

Used as a font-lock MATCHER function for `hi-var-mode'.

This function is a no-op if `hivar-highlighting' specifies neither
`buffer-local' nor `file-local'."
  (let ((opoint      (point))
        (found       nil)
        (buf-loc-p   (memq 'buffer-local hivar-highlighting))
        (file-loc-p  (memq 'file-local hivar-highlighting)))
    (when (or buf-loc-p  file-loc-p)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (while (and (not found)  (not (eobp)))
          (cond ((condition-case ()
                     (save-excursion
                       (skip-chars-forward "' \t\n")
                       (setq opoint  (point))
                       (let ((obj  (read (current-buffer))))
                         (and (symbolp obj)
                              (progn (set-match-data (list opoint (point)))
                                     (setq hivar--face  (or (and file-loc-p
                                                                 (assq obj file-local-variables-alist)
                                                                 'hivar-file-local-vars)
                                                            (and buf-loc-p
                                                                 (assq obj (buffer-local-variables))
                                                                 'hivar-buffer-local-vars)))
                                     t))))
                   (error nil))
                 (forward-sexp 1)
                 (setq opoint  (point)
                       found   t))
                (t
                 (if (looking-at "\\(\\sw\\|\\s_\\|[\"]\\)") ; Avoid going inside a string.
                     (forward-sexp 1)
                   (forward-char 1)))))
        found))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hi-var)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hi-var.el ends here
