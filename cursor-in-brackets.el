;;; cursor-in-brackets.el --- move cursor in brackets/quotes when right bracket/quote is inserted

;;; Copyright (C) 2013 yascentur

;; Author:   yascentur <screenname at gmail dot com>
;; Keywords: cursor bracket quote
;; Version:  1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The `cursor-in-brackets-mode` is a minor mode
;; that moves a cursor in brackets/quotes
;; when a right bracket/quote is inserted after a left bracket/quote.
;; Supported brackets and quotes are listed in the following.

;; - ()
;; - {}
;; - []
;; - <>
;; - ""
;; - ''
;; - `'
;; - ``
;; - $$

;; The `cursor-in-brackets-mode` also supports region-based bracketing.
;; When a right bracket is inserted with a `transient-mark-mode` region,
;; the region becomes bracked and a cursor is moved before the right bracket.
;; When a left bracket is inserted instead,
;; the region becomes bracked and a cursor is moved after the left bracket.

;;; Usage:

;;     (require 'cursor-in-brackets)
;;     (global-cursor-in-brackets-mode t)

;;; Code:

(defgroup cursor-in-brackets-mode nil
  "Cursor-in-brackets mode"
  :group  'convenience
  :prefix "cursor-in-brackets-mode-")

(defun cursor-in-brackets-mode-insert-right-bracket-in-general (left-brackets right-bracket)
  "Insert right bracket in general"
  (if (and transient-mark-mode mark-active)
      (let ((beginning (min (point) (mark)))
            (end (max (point) (mark))))
        (goto-char beginning)
        (insert (car left-brackets))
        (goto-char (1+ end))
        (insert right-bracket)
        (backward-char))
    (insert right-bracket)
    (when (memq (char-before (1- (point))) left-brackets)
      (backward-char))))

(defun cursor-in-brackets-mode-insert-left-bracket-in-general (left-bracket right-bracket)
  "Insert left bracket in general"
  (if (and transient-mark-mode mark-active)
      (let ((beginning (min (point) (mark)))
            (end (max (point) (mark))))
        (goto-char end)
        (insert right-bracket)
        (goto-char beginning)
        (insert left-bracket))
    (insert left-bracket)))

(defun cursor-in-brackets-mode-insert-right-parenthesis ()
  "Insert right parenthesis"
  (interactive)
  (cursor-in-brackets-mode-insert-right-bracket-in-general '(?\() ?\)))
(defun cursor-in-brackets-mode-insert-right-brace ()
  "Insert right brace"
  (interactive)
  (cursor-in-brackets-mode-insert-right-bracket-in-general '(?{) ?}))
(defun cursor-in-brackets-mode-insert-right-bracket ()
  "Insert right bracket"
  (interactive)
  (cursor-in-brackets-mode-insert-right-bracket-in-general '(?\[) ?\]))
(defun cursor-in-brackets-mode-insert-right-angle ()
  "Insert right angle"
  (interactive)
  (cursor-in-brackets-mode-insert-right-bracket-in-general '(?<) ?>))

(defun cursor-in-brackets-mode-insert-left-parenthesis ()
  "Insert left parenthesis"
  (interactive)
  (cursor-in-brackets-mode-insert-left-bracket-in-general ?\( ?\)))
(defun cursor-in-brackets-mode-insert-left-brace ()
  "Insert left brace"
  (interactive)
  (cursor-in-brackets-mode-insert-left-bracket-in-general ?{ ?}))
(defun cursor-in-brackets-mode-insert-left-bracket ()
  "Insert left bracket"
  (interactive)
  (cursor-in-brackets-mode-insert-left-bracket-in-general ?\[ ?\]))
(defun cursor-in-brackets-mode-insert-left-angle ()
  "Insert left angle"
  (interactive)
  (cursor-in-brackets-mode-insert-left-bracket-in-general ?< ?>))

(defun cursor-in-brackets-mode-insert-double-quote ()
  "Insert double quote"
  (interactive)
  (cursor-in-brackets-mode-insert-right-bracket-in-general '(?\") ?\"))
(defun cursor-in-brackets-mode-insert-single-quote ()
  "Insert single quote"
  (interactive)
  (cursor-in-brackets-mode-insert-right-bracket-in-general '(?' ?`) ?'))
(defun cursor-in-brackets-mode-insert-grave ()
  "Insert grave"
  (interactive)
  (cursor-in-brackets-mode-insert-right-bracket-in-general '(?`) ?`))
(defun cursor-in-brackets-mode-insert-dollar ()
  "Insert doller"
  (interactive)
  (cursor-in-brackets-mode-insert-right-bracket-in-general '(?$) ?$))

(defun cursor-in-brackets-mode-init-map ()
  "Initialize map"
  (define-key cursor-in-brackets-mode-map
    "\)" 'cursor-in-brackets-mode-insert-right-parenthesis)
  (define-key cursor-in-brackets-mode-map
    "}"  'cursor-in-brackets-mode-insert-right-brace)
  (define-key cursor-in-brackets-mode-map
    "\]" 'cursor-in-brackets-mode-insert-right-bracket)
  (define-key cursor-in-brackets-mode-map
    ">"  'cursor-in-brackets-mode-insert-right-angle)
  (define-key cursor-in-brackets-mode-map
    "\(" 'cursor-in-brackets-mode-insert-left-parenthesis)
  (define-key cursor-in-brackets-mode-map
    "{"  'cursor-in-brackets-mode-insert-left-brace)
  (define-key cursor-in-brackets-mode-map
    "\[" 'cursor-in-brackets-mode-insert-left-bracket)
  (define-key cursor-in-brackets-mode-map
    "<"  'cursor-in-brackets-mode-insert-left-angle)
  (define-key cursor-in-brackets-mode-map
    "\"" 'cursor-in-brackets-mode-insert-double-quote)
  (define-key cursor-in-brackets-mode-map
    "'"  'cursor-in-brackets-mode-insert-single-quote)
  (define-key cursor-in-brackets-mode-map
    "`"  'cursor-in-brackets-mode-insert-grave)
  (define-key cursor-in-brackets-mode-map
    "$"  'cursor-in-brackets-mode-insert-dollar))

(define-minor-mode cursor-in-brackets-mode
  "Cursor-in-brackets mode"
  :group   'cursor-in-brackets-mode
  :lighter " CB"
  :keymap  (make-sparse-keymap)
  (when cursor-in-brackets-mode
      (cursor-in-brackets-mode-init-map)))

(define-global-minor-mode global-cursor-in-brackets-mode
  cursor-in-brackets-mode
  (lambda ()
    (cursor-in-brackets-mode t)))

(provide 'cursor-in-brackets)

;;; cursor-in-brackets.el ends here
