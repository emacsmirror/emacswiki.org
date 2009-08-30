;;; unparen.el --- show elisp code without parenthesis'

;; Copyright (C) 2002  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.0
;; Keywords: convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?UnParenMode

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

;; `unparen-mode' toggles the display of parens.  While they are
;; hidden away, the buffer is read-only, because currently this code
;; does now really add and remove parens -- it just makes them
;; invisible.  That would be a nice feature, though.  At the moment,
;; this minor mode is mainly useful to show lisp code to non-lisp
;; coders affraid of parens.

;;; Code:

(define-minor-mode unparen-mode
  "Make all parens invisible, unless they are quoted."
  nil "()"  nil
  (if unparen-mode
      (unparen)
    (unparen-disable)))

(defun unparen ()
  "Hide all parens and make buffer read-only."
  (goto-char (point-min))
  (while (re-search-forward "[()]" nil t)
    (if (and (eq ?' (char-before (match-beginning 0)))
	     (eq ?\( (char-after (match-beginning 0))))
	(progn
	  (goto-char (match-beginning 0))
	  (forward-sexp))
      (put-text-property (match-beginning 0)
			 (match-end 0)
			 'invisible
			 'unparen)))
  (toggle-read-only 1))

(defun unparen-disable ()
  "Make all parens visible and buffer editable."
  (toggle-read-only -1)
  (remove-text-properties (point-min)
			  (point-max)
			  '(invisible nil)))

;;; unparen.el ends here
