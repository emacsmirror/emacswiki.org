;;; really-simple-wiki.el --- edit local raw wiki pages

;; Copyright (C) 2002, 2003, 2004  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;;         D. Goel <deego@gnufans.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.2
;; Keywords: hypermedia
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?SimpleWikiEditMode

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Use `really-simple-wiki-mode' to edit raw wiki pages.  This is
;; useful for temp files when editing textareas in w3m, for example.
;; Here is how to do that:
;;
;; (add-to-list 'auto-mode-alist '("w3mtmp" . really-simple-wiki-mode))

;;; Code:

(defvar really-simple-wiki-link-pattern
  "\\<[A-Z\xc0-\xde]+[a-z\xdf-\xff]+\\([A-Z\xc0-\xde]+[a-z\xdf-\xff]*\\)+\\>"
  "The pattern used for finding WikiName.")


(define-derived-mode really-simple-wiki-mode text-mode "Wiki"
  "Simple mode to edit wiki pages.

\\{simple-wiki-mode-map}"
  (font-lock-add-keywords
   nil
   '(("^ .+?$" . font-lock-comment-face)
     ("<\\(/?[a-z]+\\)" 1 font-lock-function-name-face)
     ("^[*#]\\([*#]+\\)" . font-lock-constant-face)
     ("^\\([*#]\\)[^*#]" 1 font-lock-builtin-face)))
  (font-lock-add-keywords
   nil
   (list (cons (symbol-value 'really-simple-wiki-link-pattern)
	       'font-lock-keyword-face)))
  (font-lock-mode 1)
  (goto-address)
  (set (make-local-variable 'sgml-tag-alist)
       `(("b") ("code") ("em") ("i") ("strong") ("nowiki")
	 ("pre" \n) ("tt") ("u")))
  (set (make-local-variable 'skeleton-transformation) 'identity)
  (setq indent-tabs-mode nil))

(provide 'really-simple-wiki)

;;; really-simple-wiki.el ends here
