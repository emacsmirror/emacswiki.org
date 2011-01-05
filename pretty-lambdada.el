;;; pretty-lambdada.el --- Show the word `lambda' as the Greek letter.
;;
;; Filename: pretty-lambdada.el
;; Description: Show the word `lambda' as the Greek letter.
;; Author: Drew Adams
;;         See http://www.emacswiki.org/emacs/PrettyLambda for the original
;;         code snippet and its history.
;; Maintainer: Drew Adams
;; Copyright (C) 2009-2011, Drew Adams, all rights reserved.
;; Created: Sun Jun 14 11:07:04 2009 (-0700)
;; Version: 22.0
;; Last-Updated: Tue Jan  4 13:18:23 2011 (-0800)
;;           By: dradams
;;     Update #: 141
;; URL: http://www.emacswiki.org/cgi-bin/wiki/pretty-lambdada.el
;; Keywords: convenience display
;; Compatibility: GNU Emacs: 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Whenever "lambda" appears as a separate word, it is displayed using the
;;  Greek letter.
;;
;;  Put this in your init file (~/.emacs), to turn this display on for
;;  the modes in `pretty-lambda-auto-modes', which by default are the
;;  usual Lisp modes:
;;
;;   (require 'pretty-lambdada)
;;   (pretty-lambda-for-modes)
;;
;;  You can toggle pretty-lambda display on/off in any buffer, using
;;  command `pretty-lambda-mode'.  Use `global-pretty-lambda-mode' to
;;  toggle the display in all buffers.
;;
;;  Three alternative ways to turn on pretty-lambda display for a
;;  specific buffer mode:
;;
;;  1. (add-hook 'my-mode-hook 'turn-on-pretty-lambda-mode)
;;  2. (pretty-lambda 'my-mode)
;;  3. (add-hook 'my-mode-hook 'pretty-lambda)
;;
;;  The first way uses minor mode `pretty-lambda-mode', so you can
;;  easily toggle pretty-lambda display.  The last two just turn on
;;  the display.  To turn it off, use `turn-off-pretty-lambda-mode'.
;;
;;
;;  User options defined here:
;;
;;    `global-pretty-lambda-mode', `pretty-lambda-auto-modes',
;;    `pretty-lambda-mode'.
;;
;;  Commands defined here:
;;
;;    `global-pretty-lambda-mode', `pretty-lambda-for-modes',
;;    `pretty-lambda-mode'
;;
;;  Non-interactive functions defined here:
;;
;;    `pretty-lambda', `turn-off-pretty-lambda-mode',
;;    `turn-on-pretty-lambda-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/04 dadams
;;     Added autoload cookies (for defgroup, defcustom, and commands).
;; 2009/06/14 dadams
;;     Added: group pretty-lambda, (global-)pretty-lambda-mode,
;;            pretty-lambda-for-modes, pretty-lambda-auto-modes,
;;            turn-(on|off)-pretty-lambda-mode.
;;     pretty-lambda: Added optional MODE arg (required arg in some existing code).
;;     Created from code snippet at http://www.emacswiki.org/emacs/PrettyLambda.
;;
;;     Called this pretty-lambdada.el, where the last "da" is Drew Adams.
;;     Luke Gorrie has already used the name pretty-lambda.el:
;;     http://fresh.homeunix.net/~luke/misc/emacs/pretty-lambda.el
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

;;;###autoload
(defgroup pretty-lambda nil
  "Display of the word `lambda' as the Greek character."
    :group 'convenience :group 'programming)

;;;###autoload
(defcustom pretty-lambda-auto-modes
  '(lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "*Modes affected by `pretty-lambda-for-modes'."
  :type '(repeat symbol) :group 'pretty-lambda)

;;;###autoload
(defun pretty-lambda-for-modes (&optional turn-off)
  "Use `pretty-lambda-mode' for modes in `pretty-lambda-auto-modes'.
`C-u' to turn off."
  (interactive "P")
  (let (hook-var)
    (cond (turn-off
           (dolist (m  pretty-lambda-auto-modes)
             (remove-hook (setq hook-var (intern (concat (symbol-name m) "-hook")))
                          'turn-on-pretty-lambda-mode)
             (add-hook hook-var 'turn-off-pretty-lambda-mode))
           (when (memq major-mode pretty-lambda-auto-modes)
             (turn-off-pretty-lambda-mode))) ; Current buffer
          (t
           (dolist (m  pretty-lambda-auto-modes)
             (remove-hook (setq hook-var (intern (concat (symbol-name m) "-hook")))
                          'turn-off-pretty-lambda-mode)
             (add-hook hook-var 'turn-on-pretty-lambda-mode))
           (when (memq major-mode pretty-lambda-auto-modes)
             (turn-on-pretty-lambda-mode)))))) ; Current buffer

;;;###autoload
(define-minor-mode pretty-lambda-mode
    "Buffer-local minor mode to display the word `lambda' as the Greek letter.
With ARG, turn mode on if ARG is positive, off otherwise."
  :init-value nil
  (cond (pretty-lambda-mode
         (pretty-lambda)
         (font-lock-fontify-buffer))
        (t
         (font-lock-remove-keywords
          nil `(("\\<lambda\\>"
                 (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                           ,(make-char 'greek-iso8859-7 107))
                           nil)))))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward "\\<lambda\\>" nil t)
             (decompose-region (match-beginning 0) (match-end 0)))))))

;;;###autoload
(define-globalized-minor-mode global-pretty-lambda-mode
    pretty-lambda-mode turn-on-pretty-lambda-mode
    "Global minor mode to display the word `lambda' as the Greek letter.
With ARG, turn mode on if ARG is positive, off otherwise.")

;; This was originally from <URL: http://www.emacswiki.org/emacs/PrettyLambda>.
;; See that page for the history of this code snippet.  I just added MODE as an
;; optional argument.
(defun pretty-lambda (&optional mode)
  "Display the word `lambda' as the Greek letter.
Non-nil optional arg means use pretty-lambda display in that MODE.
nil means use pretty-lambda display for the current mode."
  (font-lock-add-keywords
   mode `(("\\<lambda\\>"
   (0 (progn (compose-region (match-beginning 0) (match-end 0)
        ,(make-char 'greek-iso8859-7 107))
      nil))))))

(defun turn-on-pretty-lambda-mode  () (pretty-lambda-mode  1))
(defun turn-off-pretty-lambda-mode () (pretty-lambda-mode -1))

;;;;

(provide 'pretty-lambdada)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pretty-lambdada.el ends here
