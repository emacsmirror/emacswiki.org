;;; d-insert-assert.el --- Automagical verbose assert insertion for D Programming Language
;; $Id: d-insert-assert.el,v 1.6 2007/02/07 14:38:17 rubikitch Exp $

;; Copyright (C) 2007  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: D, convenience, languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Make D language's assert expression verbose automatically when
;; saving file.  If you are familiar with xUnit, you may write many
;; assertEquals expressions.
;;
;;   assertEquals(EXPECTED, ACTUAL);
;;
;; The D language provides assert expression, but not assertEquals.
;; This small elisp provides assertEquals emulation. To use this,
;; you must write assert expression for equality:
;;
;;   assert(EXPECTED == ACTUAL);
;;
;; When saving D source file, this elisp transforms assert equality
;; expression into verbose format. For example:
;;
;; BEFORE
;;   assert(100 == a+8);
;; AFTER
;;   assert(100 == a+8, format("\n<%s> expected but was\n<%s>.\n", 100, a+8));
;;
;; If you edit EXPECTED or ACTUAL, you NEED NOT EDIT format's arguments.
;; If you edit the above verbose assert statement such as:
;;
;;   assert(1 == a+8, format("\n<%s> expected but was\n<%s>.\n", 100, a+8));
;;
;; The elisp automatically adjusts it!
;;
;;   assert(1 == a+8, format("\n<%s> expected but was\n<%s>.\n", 1, a+8));
;;
;; So the elisp is DRY(Don't repeat yourself) compliant.

;;; Installation:

;; (require 'd-insert-assert)
;; (d-insert-assert-setup)

;;; History:

;; $Log: d-insert-assert.el,v $
;; Revision 1.6  2007/02/07 14:38:17  rubikitch
;; fixed `replace-match' error.
;;
;; Revision 1.5  2007/02/06 16:40:35  rubikitch
;; split assert statement.
;; removed test cases. (moved to el4r script)
;; more strict regexp.
;;
;; Revision 1.4  2007/02/05 22:10:54  rubikitch
;; newline handling
;;
;; Revision 1.3  2007/01/24 14:43:00  rubikitch
;; avoid excess import std.string
;;
;; Revision 1.2  2007/01/23 19:23:29  rubikitch
;; added Commentary and docstring.
;;
;; Revision 1.1  2007/01/23 16:25:35  rubikitch
;; Initial revision
;;

;;; Code:

(require 'cc-mode)

(defun d-insert-assert-setup ()
  "Install Automagical verbose assert insertion for D Programming Language."
  (add-hook 'before-save-hook 'd-insert-assert-before-save-hook-function))
    
(defun d-insert-assert-before-save-hook-function ()
  (when (eq major-mode 'd-mode)
    (d-transform-assert-equal)))

(defun d-transform-assert-equal ()
  (save-excursion
    (d-transform-assert-equal-1)
    (d-transform-assert-equal-2)
    (d-transform-assert-equal-check-import-std-string)))

(defun d-transform-assert-equal-check-import-std-string ()
  (goto-char (point-min))
  (when (search-forward "format(" nil t)
    (goto-char (point-min))
    (unless (re-search-forward "^ *\\(private *\\)?import std.string;" nil t)
      (insert "private import std.string;\n"))))

(defvar d-transform-assert-format-regexp "format(\"\\\\n<%s> expected but was\\\\n<%s>.\\\\n\"")
(defun d-transform-assert-equal-1 ()
  ;; assert(expected == actual);
  (goto-char (point-min))
  (while (re-search-forward "^ *assert( *\\(.+?\\) *== *\\(.+?\\) *);$" nil t)
    (unless (save-match-data (string-match
                                d-transform-assert-format-regexp
                                (match-string 2)))
      (backward-char 2)
      (insert ",\n")
      (save-match-data (c-indent-command))
      (insert "format(\"\\n<%s> expected but was\\n<%s>.\\n\", "
              (match-string 1)
              ", "
              (match-string 2)
              ")"))))

(defun d-transform-assert-equal-2 ()
  ;; assert(expected == actual,\n? format("\n<%s> expected but was\n<%s>.\n", expected, actual));
  (goto-char (point-min))
  (while (re-search-forward
          (concat "^ *assert( *\\(.+?\\) *== *\\(.+?\\) *,\n? *"
                  d-transform-assert-format-regexp ", \\(.+\\)));$")
          nil t)
    (let ((expected (match-string 1))
          (actual   (match-string 2)))
      (replace-match (concat expected ", " actual) nil t nil 3))))

;; test function
(defun d-transform-assert-equal-test (s)
   (with-current-buffer (get-buffer-create "*test*")
     (erase-buffer)
     (insert s)
     (d-transform-assert-equal)
     (display-buffer (current-buffer))))
(defun d-transform-assert-equal-test-value (s)
   (with-current-buffer (get-buffer-create "*test*")
     (erase-buffer)
     (insert s)
     (d-transform-assert-equal)
     (buffer-string)))

;; test script (find-myerfile "test-d-insert-assert.el.rb")

(provide 'd-insert-assert)

;; How to save (DO NOT REMOVE!!)
;; (let ((oddmuse-wiki "EmacsWiki")(oddmuse-page-name "d-insert-assert.el")) (call-interactively 'oddmuse-post))
;;; d-insert-assert.el ends here
