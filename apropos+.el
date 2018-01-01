;;; apropos+.el --- Extensions to `apropos.el'
;;
;; Filename: apropos+.el
;; Description: Extensions to `apropos.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2018, Drew Adams, all rights reserved.
;; Created: Thu Jun 22 15:07:30 2000
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon Jan  1 09:20:16 2018 (-0800)
;;           By: dradams
;;     Update #: 85
;; URL: https://www.emacswiki.org/emacs/download/apropos%2b.el
;; Keywords: help
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `apropos'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Commands defined here:
;;
;;    `apropos-local-value', `apropos-local-variable',
;;    `apropos-user-option' (Emacs < 24.4).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2017/06/18 dadams
;;     Added: apropos-local-value, apropos-local-variable.
;;     Renamed apropos-user-options to apropos-user-option.  Define it only
;;       if it does not already exist.  Removed autoload cookie for it.
;; 2007/11/27 dadams
;;     apropos-user-options: If available, use icicle-read-string-completing.
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

(require 'apropos)

;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apropos-local-variable (pattern &optional buffer)
  "Show buffer-local variables that match PATTERN.
Optional arg BUFFER (default: current buffer) is the buffer to check.

The output includes variables that are not yet set in BUFFER, but that
will be buffer-local when set."
  (interactive (list (if (fboundp 'apropos-read-pattern)
                         (apropos-read-pattern "buffer-local variable")
                       (read-string "Apropos buffer-local variable: "))))
  (unless buffer (setq buffer  (current-buffer)))
  (apropos-command pattern nil
                   (lambda (symbol)
                     (and (local-variable-if-set-p symbol)
                          (get symbol 'variable-documentation)))))

(defun apropos-local-value (pattern &optional buffer)
  "Show buffer-local variables whose values match PATTERN.
This is like `apropos-value', but for only buffer-local variables.

Optional arg BUFFER (default: current buffer) is the buffer to check."
  (interactive (list (if (fboundp 'apropos-read-pattern)
                         (apropos-read-pattern "value of buffer-local variable")
                       (read-string "Apropos value of buffer-local variable: "))))
  (unless buffer (setq buffer  (current-buffer)))
  (if (fboundp 'apropos-parse-pattern)
      (apropos-parse-pattern pattern)   ; Emacs 21+
    (setq apropos-regexp  pattern))     ; Emacs 20
  (setq apropos-accumulator  ())
  (let ((var             nil))
    (mapatoms
     (lambda (symb)
       (unless (memq symb '(apropos-regexp apropos-pattern apropos-all-words-regexp
                            apropos-words apropos-all-words apropos-accumulator symb var))
         (setq var  (apropos-value-internal 'local-variable-if-set-p symb 'symbol-value)))
       (when (and (fboundp 'apropos-false-hit-str)  (apropos-false-hit-str var))
         (setq var nil))
       (when var
         (setq apropos-accumulator (cons (if (> emacs-major-version 20)
                                             (list symb (apropos-score-str var) nil var)
                                           (list symb nil var))
                                         apropos-accumulator))))))
  (let ((apropos-multi-type  nil))
    (if (> emacs-major-version 20)
        (apropos-print
         nil "\n----------------\n"
         (format "Buffer `%s' has the following local variables\nmatching %s`%s':"
                 (buffer-name buffer)
                 (if (consp pattern) "keywords " "")
                 pattern))
      (apropos-print nil "\n----------------\n"))))

(unless (fboundp 'apropos-user-option)  ; For Emacs prior to 24.4 only.
  (defun apropos-user-option (regexp)
    "Show user variables that match REGEXP."
    (interactive
     (list (if (fboundp 'icicle-read-string-completing)
               (icicle-read-string-completing "Apropos user options (regexp): ")
             (read-string "Apropos user options (regexp): "))))
    (let ((apropos-do-all nil))
      (apropos-variable regexp))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'apropos+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apropos+.el ends here
