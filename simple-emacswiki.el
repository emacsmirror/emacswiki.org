;;; simple-emacswiki.el --- edit emacswiki.org within Emacs

;; Copyright (C) 2003  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.0
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

;; Use `emacswiki-browse' to download a page from the Emacs Wiki.

;;; Code:

(require 'simple-wiki-edit)
(require 'http-post)

(defun usemod-wiki-save ()
  "Save the current page to a UseMod wiki."
  (let ((url simple-wiki-url)
	(save-func simple-wiki-save-function))
    (switch-to-buffer
     (process-buffer
      (http-post
       (simple-wiki-save-link)
       (list (cons "title" (simple-wiki-page))
	     (cons "summary" (read-from-minibuffer "Summary: " "*"))
	     '("raw" . "2")
	     (cons "username" (apply 'concat (split-string user-full-name)))
	     (cons "text" (buffer-string)))
       'iso-8859-1)))
    (simple-wiki-edit-mode)
    (set (make-local-variable 'simple-wiki-url) url)
    (set (make-local-variable 'simple-wiki-save-function) save-func)))

(defun emacswiki-browse (page)
  "Browse the Emacs Wiki."
  (interactive "sPage: ")
  (simple-wiki-edit
   (concat
    "http://www.emacswiki.org/cgi-bin/wiki.pl?action=browse&raw=2&id="
    page)
   'usemod-wiki-save))

(provide 'simple-emacswiki)

;;; simple-emacswiki.el ends here
