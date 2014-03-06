;;; init-thing-edit.el --- Thing edit

;; Filename: init-thing-edit.el
;; Description: Thing edit
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-28 07:46:40
;; Version: 0.1
;; Last-Updated: 2013-12-28 07:46:40
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-thing-edit.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Thing edit
;;

;;; Installation:
;;
;; Put init-thing-edit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-thing-edit)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-thing-edit RET
;;

;;; Change log:
;;
;; 2013/12/28
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

(require 'one-key)
(require 'thing-edit)
(require 'thing-edit-extension)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Thing-Edit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar one-key-menu-thing-edit-alist nil
  "The `one-key' menu alist for THING-EDIT.")

(setq one-key-menu-thing-edit-alist
      '(
        ;; Copy.
        (("w" . "Copy Word") . thing-copy-word)
        (("s" . "Copy Symbol") . thing-copy-symbol)
        (("m" . "Copy Email") . thing-copy-email)
        (("f" . "Copy Filename") . thing-copy-filename)
        (("u" . "Copy URL") . thing-copy-url)
        (("x" . "Copy Sexp") . thing-copy-sexp)
        (("g" . "Copy Page") . thing-copy-page)
        (("t" . "Copy Sentence") . thing-copy-sentence)
        (("o" . "Copy Whitespace") . thing-copy-whitespace)
        (("i" . "Copy List") . thing-copy-list)
        (("c" . "Copy Comment") . thing-copy-comment)
        (("h" . "Copy Function") . thing-copy-defun)
        (("p" . "Copy Parentheses") . thing-copy-parentheses)
        (("l" . "Copy Line") . thing-copy-line)
        (("a" . "Copy To Line Begin") . thing-copy-to-line-beginning)
        (("e" . "Copy To Line End") . thing-copy-to-line-end)
        ;; Copy.
        (("W" . "Paste Word") . thing-paste-word)
        (("S" . "Paste Symbol") . thing-paste-symbol)
        (("M" . "Paste Email") . thing-paste-email)
        (("F" . "Paste Filename") . thing-paste-filename)
        (("U" . "Paste URL") . thing-paste-url)
        (("X" . "Paste Sexp") . thing-paste-sexp)
        (("G" . "Paste Page") . thing-paste-page)
        (("T" . "Paste Sentence") . thing-paste-sentence)
        (("O" . "Paste Whitespace") . thing-paste-whitespace)
        (("I" . "Paste List") . thing-paste-list)
        (("C" . "Paste Comment") . thing-paste-comment)
        (("H" . "Paste Function") . thing-paste-defun)
        (("P" . "Paste Parentheses") . thing-paste-parentheses)
        (("L" . "Paste Line") . thing-paste-line)
        (("A" . "Paste To Line Begin") . thing-paste-to-line-beginning)
        (("E" . "Paste To Line End") . thing-paste-to-line-end)
        ))

(defun one-key-menu-thing-edit ()
  "The `one-key' menu for THING-EDIT."
  (interactive)
  (one-key-menu "THING-EDIT" one-key-menu-thing-edit-alist t))

(provide 'init-thing-edit)

;;; init-thing-edit.el ends here
