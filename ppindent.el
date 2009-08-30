;;; ppindent.el --- Indents C preprocessor directives

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Craig McDaniel <craigmcd@gmail.com>
;; Keywords: languages, c
;; Maintainer: Craig McDaniel <craigmcd@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; PPINDENT indents C-preprocessor statements depending on the current
;; #if..., #el.., #endif level. It only modifies lines with "#" as the
;; first non-blank character and therefore does not conflict with the
;; normal C indentation. Unlike a true indent, it inserts the
;; requisite number of spaces after, rather than before the "#"
;; character. This type of preprocesor indent is commonly used to
;; provide the most compatibility with different C-compilers.
;;
;; Example:

;;        #ifdef WOO
;;        ....
;;        #if defined(X) && !defined(Y)
;;        #ifdef Q8
;;        ...
;;        #else
;;        ...
;;        ....
;;        #elif defined (Z)
;;        ....
;;        #endif
;;        #endif
;;        #endif
          
;; After "M-x ppindent-c" becomes:
          
;;        #ifdef WOO
;;        ....
;;        #  if defined(X) && !defined(Y)
;;        #    ifdef Q8
;;        ...
;;        #    else
;;        ...
;;        ....
;;        #    elif defined (Z)
;;        ....
;;        #    endif
;;        #  endif
;;        #endif

;; Two functions are provided: PPINDENT-C indents as described
;; above. PPINDENT-H does not indent the first level, assuming that
;; .h/.hpp files use an #ifdef guard around the entire file.

;; You can customize PPINDENT-INCREMENT if you want to use something
;; other than 2 spaces for the indent.

;;; History:

;; 2007-01-19 WCM Initial version

;;; Code:

(provide 'ppindent)

(defgroup pp-indent nil
  "Indent C preproccessor directives."
  :group 'c)

(defcustom ppindent-increment 2
  "Number of spaces per indention level.

Used in C pre-processor indent functions ppindent-c and ppindent-h"
  :type 'number
  :group 'pp-indent)

(defun starts-withp (str prefix)
  "str starts with prefix"
  (eql (compare-strings prefix nil nil str nil (length prefix)) t))

(defun my-make-string (length init)
  "just like make-string, but makes an empty string if length is negative"
  (when (minusp length)
    (setf length 0))
  (make-string length init))

(defun ppindent-aux (start)
  (let ((cnt start))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#[ \t]*\\(.*\\)" nil t)
      (cond ((starts-withp (match-string-no-properties 1) "if")
             (replace-match (concat "#" (my-make-string cnt ?\s) "\\1"))
             (incf cnt ppindent-increment))
            ((starts-withp (match-string-no-properties 1) "el")
             (when (< (- cnt ppindent-increment) start)
               (throw 'err `(,(line-number-at-pos) "Unmatched #else or #elif")))
             (replace-match (concat "#" (my-make-string
                                         (- cnt ppindent-increment)
                                         ?\s) "\\1")))
            ((starts-withp (match-string-no-properties 1) "endif")
             (when (< (- cnt ppindent-increment) start)
               (throw 'err `(,(line-number-at-pos) "Unmatched #endif")))
             (decf cnt ppindent-increment)
             (replace-match (concat "#" (my-make-string cnt ?\s) "\\1")))
            (t
             (replace-match (concat "#" (my-make-string cnt ?\s) "\\1")))))))

(defun ppindent-buffer (start)
  (let ((result (catch 'err (save-excursion (ppindent-aux start)))))
    (when result
      (goto-line (car result))
      (error "Error: %s" (cadr result)))))

(defun ppindent-c ()
  "Indent all C pre-processor statements"
  (interactive)
  (ppindent-buffer 0))

(defun ppindent-h ()
  "Indent C pre-processor statements, keeping first level #ifdef unindented"
  (interactive)
  (ppindent-buffer (- ppindent-increment)))

