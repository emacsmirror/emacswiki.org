;;; thingatpt-ext.el --- Thing at point extensions
;;
;; Author: Henry G. Weller <hweller0@gmail.com>
;;         with bits contributed by Toby Cubit
;;         and from thing-opt.el by MATSUYAMA Tomohiro
;;         and from http://www.emacswiki.org
;; Maintainer: Henry G. Weller
;;
;; Created: Tue Mar 17 10:48:35 2009 (+0000)
;; Version: 0.1
;; Last-Updated: Tue Mar 17 10:48:35 2009 (+0000)
;;           By: Henry Weller
;;     Update #: 0
;; URL: http://www.emacswiki.org/emacs/thingatpt-ext.el
;; Keywords: completion, ui, user interface, sources
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;; ----------------------------------------------------------------------------
;;
;;; Commentary:
;;
;;  Lots of additions to the `thingatpt' package.
;;
;; ----------------------------------------------------------------------------
;;; Change Log:
;;
;; Version 0.1
;; * initial release
;;
;; ----------------------------------------------------------------------------
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
;; ----------------------------------------------------------------------------
;;; Code:

(provide 'thingatpt-ext)
(require 'thingatpt)

;; ----------------------------------------------------------------------------
;;; Useful support functions

;;;  Generic forward operation on the thing type provided
(defun forward-op-thing (thing n)
  "Move point forward N things (backward if N is negative).
Requires `bounds-of-thing-at-point' to be defined for the thing type."
  (if (null n)
      (setq n 1))
  (let (bounds)
    (dotimes (i (abs n))
      (while (or (null (setq bounds
                             (bounds-of-thing-at-point thing)))
                 (= (if (< n 0) (car bounds) (cdr bounds)) (point)))
        (if (< n 0) (backward-char) (forward-char)))
      (goto-char (if (< n 0) (car bounds) (cdr bounds))))))

;;;  Return bounds of a `thing' delimited by the delimiter provided
(defun thing-at-point-bounds-of-delimited-thing-at-point (del)
  "A `thing-at-point-bounds-of' function for single character delimited
expressions
e.g.
   Delimited expression   DEL
   \"...\"                  \"\\\"\"
   \'...\'                  \'\\'\'
   $...$                  \"$\"
The return is a pair of buffer positions (START . END), or nil if
no recognised delimited expression at or surrounding the point is found."
  (let ((regexp
         (concat "\\" del "+\\(\\(?:\\\\\\" del "\\|[^" del "]\\)+?\\)\\" del)))
    (save-excursion
      (while (and (search-backward del nil t) ;; DEL not preceded by \
                  (eq ?\\ (char-before))))
      (if (looking-at regexp)
          (cons (match-beginning 0) (match-end 0))
        nil))))

;;;  Return bounds of a `thing' delimited by the delimiter pair provided
(defun thing-at-point-bounds-of-pair-delimited-thing-at-point (ldel rdel)
  "A `thing-at-point-bounds-of' function for delimited expressions
e.g.
   Delimited expression   LDEL       RDEL
   (...)                  \"\\(\"       \"\\)\"
   [...]                  \"\\\\\\ [\"    \"\\\\\\ ]\"

The return is a pair of buffer positions (START . END), or nil if
no recognised pair-delimited expression at or surrounding point is found.
Delimited expressions may not be nested."
  (if (thing-at-point-looking-at (concat ldel "\\(.\\|\n\\)*?" rdel))
      (cons (match-beginning 0) (match-end 0))
    nil))

;;;  Goto the begining of the list which starts with the delimiter provided
(defun thing-at-point-find-list-beginning (ldel)
  "Reposition point at the start of the list.
Return point, or nil if original point was not in a list."
  (ignore-errors
    (let ((done nil)
          (here (point)))
      ;; Is current character a the start of the list?
      (if (looking-at ldel)
          (point)
        ;; Search backward for the start of the list that contains the cursor
        (while (and (re-search-backward ldel nil t)
                    (not (setq done
                               (< here (save-excursion
                                         (forward-sexp)
                                         (point)))))))
        (if done
            (point)
          (goto-char here)
          nil)))))

;;;  Return the bounds of the list delimited by the delimiter pair provided
(defun thing-at-point-bounds-of-list-at-point (ldel rdel)
  "A `thing-at-point-bounds-of' function for lists
e.g.
   List types                        LDEL       RDEL
   (...(...)(...)...)                \"\\(\"       \"\\)\"
   [...[...][...]...]                \"\\\\\\ [\"    \"\\\\\\ ]\"

The return is a pair of buffer positions (START . END), or nil if
no recognised nested expression at or surrounding the point is found."
  (ignore-errors
    (let (beg end)
      (save-excursion
        (setq beg (thing-at-point-find-list-beginning ldel))
        (forward-sexp)
        (setq end (point)))
      (cons beg end))))

;; ----------------------------------------------------------------------------
;;; Enhanced list-thing types

;;;  Change the list-thing type to allow selection from anywhere inside the list
;;  and handle the nesting level.  Assumes the list is "(" ")" delimited
(put 'list 'bounds-of-thing-at-point
     (lambda () (thing-at-point-bounds-of-list-at-point "(" ")")))

;;;  Reuse the standard `forward-list' which takes the current character as the
;;  delimiter type for searching forward N lists
(put 'list 'forward-op (lambda (n) (ignore-errors (forward-list n))))

;;;  Create a thing type for a "{" "}" delimited list -- a block
(put 'block 'bounds-of-thing-at-point
     (lambda () (thing-at-point-bounds-of-list-at-point "{" "}")))

;;;  Reuse the standard `forward-list' which takes the current character as the
;;  delimiter type for searching forward N blocks
(put 'block 'forward-op (lambda (n) (ignore-errors (forward-list n))))

;; ----------------------------------------------------------------------------
;;; Enhanced stirng-thing types

;;;  Add `forward-op' to the filename thing (should already be in thingatpt.el)
(put 'filename 'forward-op (lambda (n) (forward-op-thing 'filename n)))

;;;  Create a thing type for double-quote delimited "string"
(put 'string 'bounds-of-thing-at-point
     (lambda () (thing-at-point-bounds-of-delimited-thing-at-point "\"")))

;; ----------------------------------------------------------------------------
;;; Create a thing type for a word containing a regexp

(defvar thing-at-point-word-regexp-chars "[:alnum:].*^$"
  "Characters allowable in a word optionally containing a regexp.")

(put 'word-regexp 'end-op
     (lambda ()
       (re-search-forward
        (concat "\\=[" thing-at-point-word-regexp-chars "]*") nil t)))

(put 'word-regexp 'beginning-op
     (lambda ()
       (if (re-search-backward
            (concat "[^" thing-at-point-word-regexp-chars "]") nil t)
           (forward-char)
         (goto-char (point-min)))))

(put 'word-regexp 'forward-op (lambda (n) (forward-op-thing 'word-regexp n)))

;; ----------------------------------------------------------------------------
;;; Define functions for 'syntax' thing
;; (set of characters the same syntax group)

(defun syntax-forward-syntax (&optional arg)
  "Move ARG times to start of a set of the same syntax characters."
  (setq arg (or arg 1))
  (while (and (> arg 0)
              (not (eobp))
              (skip-syntax-forward (string (char-syntax (char-after)))))
    (setq arg (1- arg)))
  (while (and (< arg 0)
              (not (bobp))
              (skip-syntax-backward (string (char-syntax (char-before)))))
    (setq arg (1+ arg))))

(put 'syntax 'forward-op 'syntax-forward-syntax)

;; ----------------------------------------------------------------------------
;;; Create a forward-op for the `defun' thing (should already be in thingatpt.el)

(defun forward-defun (&optional arg)
  "Move the `point' forward ARG `defun's"
  (interactive "p")
  (if (null arg)
      (setq arg 1))
  (ignore-errors
    (cond
     ((< arg 0)
      (beginning-of-defun (- arg)))
     ((> arg 0)
      (end-of-defun arg)))))

;; ----------------------------------------------------------------------------
;;; thingatpt-ext.el ends here
