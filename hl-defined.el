;;; hl-defined.el --- Highlight defined or undefined symbols in Emacs-Lisp.
;;
;; Filename: hl-defined.el
;; Description: Highlight defined or undefined symbols in Emacs-Lisp.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2013-2014, Drew Adams, all rights reserved.
;; Created: Sat Aug 17 13:59:36 2013 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Dec 26 09:25:47 2013 (-0800)
;;           By: dradams
;;     Update #: 301
;; URL: http://www.emacswiki.org/hl-defined.el
;; Doc URL: http://emacswiki.org/HighlightLispFunctions
;; Keywords: highlight, lisp, functions
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Highlight defined or undefined symbols in Emacs-Lisp.
;;
;;  `hdefd-highlight-mode' is a minor mode that highlights, in the
;;  current buffer, symbols that are known to be defined as Emacs-Lisp
;;  functions or variables or both.  Alternatively, it can highlight
;;  symbols that are not known to be defined as functions or
;;  variables.
;;
;;  The current buffer should be in Emacs-Lisp mode.
;;
;;  Command `hdefd-highlight-mode' toggles highlighting on/off.  The
;;  highlighting respects option `hdefd-highlight-type'.
;;
;;  Command `hdefd-cycle' cycles highlighting among the available
;;  types and off, as follows: functions & variables > functions >
;;  variables > undefined > off .  It does this by changing the
;;  current value of option `hdefd-highlight-type'.
;;
;;  You can of course customize the faces used for highlighting.  You
;;  might want, for instance, to have face `hdefd-functions' inherit
;;  from face `font-lock-function-name-face', and `hdefd-variables'
;;  inherit from `font-lock-variable-name-face'.  This is not the
;;  default because I don't find it so useful.
;;
;;
;;  Put this in your init file:
;;
;;    (require 'hl-defined)
;;
;;  If you want to turn on this highlighting automatically whenever
;;  you enter Emacs-Lisp mode then you can do this in your init file:
;;
;;    (require 'hl-defined)
;;    (add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)
;;
;;  User option `hdefd-highlight-type' controls what gets highlighted.
;;
;;
;;  Faces defined here:
;;
;;    `hdefd-functions', `hdefd-variables', `hdefd-undefined'.
;;
;;  User options defined here:
;;
;;    `hdefd-highlight-type'.
;;
;;  Commands defined here:
;;
;;    `hdefd-highlight-mode'.
;;
;;  Non-interactive functions defined here:
;;
;;    `hdefd-highlight'.
;;
;;  Internal variables defined here:
;;
;;    `hdefd-face'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2013/08/19 dadams
;;     Created from highlight-fns.el (obsolete - replaced by this).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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

(eval-when-compile (require 'cl)) ;; case

(defgroup Highlight-Defined nil
  "Various enhancements to Dired."
  :prefix "hdefd-" :group 'matching :group 'font-lock :group 'programming
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
hl-defined.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/hl-defined.el")
  :link '(url-link :tag "Description" "http://emacswiki.org/HighlightLispFunctions")
  :link '(emacs-commentary-link :tag "Commentary" "hl-defined"))

(defface hdefd-functions
    '((t (:foreground "#00006DE06DE0"))) ; Like `font-lock-constant-face'.  Do not inherit.
  "Face used to highlight Emacs-Lisp functions."
  :group 'Highlight-Defined :group 'faces)

(defface hdefd-variables
    '((t (:foreground "Orchid"))) ; Like `font-lock-builtin-face'.  Do not inherit.
  "Face used to highlight Emacs-Lisp variables."
  :group 'Highlight-Defined :group 'faces)

(defface hdefd-undefined
    '((t (:foreground "Orange")))
  "Face used to highlight undefined Emacs-Lisp symbols."
  :group 'Highlight-Defined :group 'faces)

(defcustom hdefd-highlight-type 'fns-and-vars
  "Type of highlighting to be done by `hdefd-highlight-mode'.
If the value is `undefined', highlight symbols not known to be defined
as a function or a variable, using face `hdefd-undefined'.
Otherwise, highlight defined symbols.

If highlighting defined symbols and a function and a variable have the
same name then:
* The name is highlighted.
* If the option value means that function names are highlighted then
  the name is highlighted with face `hdefd-functions' (even if the
  occurrence is in fact used as a variable).
* If the value means that only variable names are highlighted then the
  name is highlighted with face `hdefd-variables' (even if it is used
  as a function)."
  :type '(choice
          (const :tag "Functions and variables" fns-and-vars)
          (const :tag "Functions"               functions)
          (const :tag "Variables"               variables)
          (const :tag "Undefined symbols"       undefined))
  :group 'Highlight-Defined)

(defvar hdefd-face nil
  "Symbol for face to use by `hdefd-highlight-mode'.")

(define-minor-mode hdefd-highlight-mode
    "Toggle highlighting defined or undefined symbols in the buffer.
The current buffer should be in Emacs-Lisp mode.
With prefix ARG, turn the mode on if ARG is positive, off otherwise.

Highlighting is governed by option `hdefd-highlight-type': either
undefined symbols or defined symbols: functions or variables or both."
  :group 'Highlight-Defined
  (if hdefd-highlight-mode
      (font-lock-add-keywords nil '((hdefd-highlight . hdefd-face)) 'APPEND)
    (font-lock-remove-keywords nil '((hdefd-highlight . hdefd-face))))
  (when font-lock-mode (font-lock-mode -1))
  (font-lock-mode 1)
  (when (if (> emacs-major-version 22)
            (called-interactively-p 'interactive)
          (called-interactively-p))
    (message "Highlighting Emacs-Lisp %s is now %s."
             (case hdefd-highlight-type
               (fns-and-vars "FUNCTIONS and VARIABLES")
               (functions    "FUNCTIONS")
               (variables    "VARIABLES")
               (undefined    "UNDEFINED symbols"))
             (if hdefd-highlight-mode "ON" "OFF"))))

(defun hdefd-cycle ()
  "Cycle highlighting via `hdefd-highlight-mode'.
Cycle among the possible values of option `hdefd-highlight-type' and off."
  (interactive)
  (setq hdefd-highlight-type  (if (not hdefd-highlight-mode)
                                  'fns-and-vars
                                (case hdefd-highlight-type
                                  (fns-and-vars  'functions)
                                  (functions     'variables)
                                  (variables     'undefined)
                                  (undefined     nil)
                                  ((nil)         'fns-and-vars))))
  (if hdefd-highlight-type
      (unless hdefd-highlight-mode (hdefd-highlight-mode 1))
    (setq hdefd-highlight-type  'fns-and-vars)
    (hdefd-highlight-mode -1))
  (message "Highlighting Emacs-Lisp %s is now %s."
           (case hdefd-highlight-type
             (fns-and-vars (if hdefd-highlight-mode "FUNCTIONS and VARIABLES" "symbols"))
             (functions    "FUNCTIONS")
             (variables    "VARIABLES")
             (undefined    "UNDEFINED symbols"))
           (if hdefd-highlight-mode "ON" "OFF"))
  (font-lock-fontify-buffer))

(defun hdefd-highlight (_limit)
  "Highlight Emacs-Lisp functions and/or variables.
Use as a font-lock MATCHER function for `hdefd-highlight-mode'."
  ;; If your code uses any of these variable names then too bad - they will be highlighted:
  ;; `hdefd-found', `hdefd-highlight-type', `hdefd-obj', and `hdefd-opoint'.
  (let ((hdefd-opoint  (point))
        (hdefd-found   nil))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (while (and (not hdefd-found)  (not (eobp)))
        (cond ((condition-case ()
                   (save-excursion
                     (skip-chars-forward "'")
                     (setq hdefd-opoint  (point))
                     (let ((hdefd-obj  (read (current-buffer))))
                       (and (symbolp hdefd-obj)
                            (not (memq hdefd-obj '(nil t)))
                            (not (keywordp hdefd-obj))
                            (or (and (memq hdefd-highlight-type '(fns-and-vars functions))
                                     (fboundp hdefd-obj))
                                (and (memq hdefd-highlight-type '(fns-and-vars variables))
                                     (boundp hdefd-obj))
                                (and (eq hdefd-highlight-type 'undefined)
                                     (not (fboundp hdefd-obj))
                                     (not (boundp  hdefd-obj))))
                            (progn (set-match-data (list hdefd-opoint (point)))
                                   (setq hdefd-face
                                         (if (and (fboundp hdefd-obj)
                                                  (memq hdefd-highlight-type
                                                        '(fns-and-vars functions)))
                                             'hdefd-functions
                                           (if (eq hdefd-highlight-type 'undefined)
                                               'hdefd-undefined
                                             'hdefd-variables)))
                                   t))))
                 (error nil))
               (forward-sexp 1)
               (setq hdefd-opoint  (point)
                     hdefd-found   t))
              (t
               (if (looking-at "\\(\\sw\\|\\s_\\)")
                   (forward-sexp 1)
                 (forward-char 1)))))
      hdefd-found)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hl-defined)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hl-defined.el ends here
