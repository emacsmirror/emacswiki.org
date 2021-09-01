;;; dyna-show.el --- Highlight dynamic variables in Emacs-Lisp -*- lexical-binding:t -*-
;; 
;; Filename: dyna-show.el
;; Description: Highlight dynamic ("special") variables in Emacs-Lisp.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2021, Drew Adams, all rights reserved.
;; Created: Sat Aug 14 19:28:17 2021 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Wed Sep  1 14:26:29 2021 (-0700)
;;           By: dradams
;;     Update #: 76
;; URL: https://www.emacswiki.org/emacs/download/dyna-show.el
;; Doc URL: https://emacswiki.org/emacs/HighlightDynamicVariables
;; Keywords: highlight, lisp, variables, lexical
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
;;    Highlight dynamic ("special") variables in Emacs-Lisp.
;;
;;  `dyna-show-mode' is a minor mode that highlights, in the current
;;  buffer, symbols that are known to be Emacs-Lisp dynamic variables.
;;  It uses faces `dyna-options' for user options and `dyna-variables'
;;  for other dynamic variables.  The command toggles the highlighting
;;  on/off.  The buffer should be in Emacs-Lisp mode.
;;
;;  The simple built-in test `special-variable-p' is used.  That test
;;  is not 100% reliable.  It doesn't respect vacuous `defvar' sexps,
;;  which declare a variable to be special in a given context, without
;;  assigning a value to the variable.  Instead, it uses `defvar',
;;  `defconst', and `defcustom' sexps with a value arg present.
;;
;;  In addition to that limitation, if a function has the same name as
;;  a dynamic variable, then its occurrences are also highlighted, as
;;  if they were occurrences of the variable.
;;
;;  See also the related library `hl-defined.el', which highlights
;;  defined or undefined Emacs-Lisp symbols (functions, variables, or
;;  both).  You can use these two libraries together - command
;;  `hdefd-highlight-mode' to highlight functions and/or variables,
;;  and command `dyna-show-mode' to highlight dynamic variables (with
;;  a different face).
;;
;;
;;  Put this in your init file, after adding `dyna-show.el' to your
;;  `load-path':
;;
;;    (require 'dyna-show)
;;
;;  If you want to turn on this highlighting automatically whenever
;;  you enter Emacs-Lisp mode then you can do this in your init file:
;;
;;    (require 'dyna-show)
;;    (add-hook 'emacs-lisp-mode-hook 'dyna-show-mode 'APPEND)
;;
;;
;;  Faces defined here:
;;
;;    `dyna-options', `dyna-variables'.
;;
;;  Commands defined here:
;;
;;    `dyna-show-mode'.
;;
;;  Non-interactive functions defined here:
;;
;;    `dyna-highlight'.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
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


(defgroup dyna-show nil
  "Highlight dynamic (\"special\") variables in Emacs Lisp."
  :prefix "dyna-"
  :group 'editing :group 'faces :group 'matching :group 'font-lock :group 'programming
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
dyna-show.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/dyna-show.el")
  :link '(url-link :tag "Description" "https://emacswiki.org/emacs/HighlightDynamicVariables")
  :link '(emacs-commentary-link :tag "Commentary" "dyna-show"))

(defface dyna-options
    '((((class color) (min-colors 88))
         (:underline (:color "Green" :style wave)))
      (t :underline t))
  "Face used to highlight user options."
  :group 'matching :group 'font-lock :group 'programming :group 'faces)

(defface dyna-variables
    '((((class color) (min-colors 88))
         (:underline (:color "Red" :style wave)))
      (t :underline t))
  "Face used to highlight dynamic (\"special\") Emacs-Lisp variables."
  :group 'matching :group 'font-lock :group 'programming :group 'faces)


(if (boundp 'hdefd--face)
    (defvaralias 'dyna--face 'hdefd--face nil)
  (defvar dyna--face))

;;;###autoload
(define-minor-mode dyna-show-mode
  "Toggle highlighting dynamic (\"special\") variables in the buffer.
The current buffer should be in Emacs-Lisp mode.
With prefix ARG, turn the mode on if ARG is positive, off otherwise.

Highlighting uses face `dyna-variables'."
  :group 'dyna-show
  (if (not dyna-show-mode)
      (font-lock-remove-keywords nil '((dyna-highlight . dyna--face)))
    (if (or (not (boundp 'hdefd-highlight-mode))  (not  hdefd-highlight-mode))
        (font-lock-add-keywords nil '((dyna-highlight . dyna--face)) 'APPEND)
      (hdefd-highlight-mode 'toggle)
      (font-lock-add-keywords nil '((dyna-highlight . dyna--face)) 'APPEND)
      (hdefd-highlight-mode 'toggle)))
  (when font-lock-mode (font-lock-mode -1))
  (font-lock-mode 1)
  (when (called-interactively-p 'interactive)
    (message "Highlighting Emacs-Lisp dynamic variables is now %s."
             (if dyna-show-mode "ON" "OFF"))))

(defun dyna-highlight (_limit)
  "Highlight Emacs-Lisp dynamic variables.
User options are highlighted with face `dyna-options'.
Other dynamic variables are highlighted with face `dyna-variables'.

Used as a font-lock MATCHER function for `dyna-show-mode'."
  (let ((dyna--opoint  (point))
        (dyna--found   nil))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (while (and (not dyna--found)  (not (eobp)))
        (cond ((condition-case ()
                   (save-excursion
                     (skip-chars-forward "' \t\n")
                     (setq dyna--opoint  (point))
                     (let ((dyna--obj  (read (current-buffer))))
                       (and (symbolp dyna--obj)
                            (special-variable-p dyna--obj)
                            (not (memq dyna--obj '(nil t)))
                            (progn (set-match-data (list dyna--opoint (point)))
                                   (setq dyna--face  (if (custom-variable-p dyna--obj)
                                                         'dyna-options
                                                       'dyna-variables))
                                   t))))
                 (error nil))
               (forward-sexp 1)
               (setq dyna--opoint  (point)
                     dyna--found   t))
              (t
               (if (looking-at "\\(\\sw\\|\\s_\\)")
                   (forward-sexp 1)
                 (forward-char 1)))))
      dyna--found)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dyna-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dyna-show.el ends here
