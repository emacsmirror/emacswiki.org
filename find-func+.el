;;; find-func+.el --- Extensions to `find-func.el'.
;;
;; Filename: find-func+.el
;; Description: Extensions to `find-func.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2008-2014, Drew Adams, all rights reserved.
;; Created: Sun Sep  7 14:17:06 2008 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Dec 26 08:58:26 2013 (-0800)
;;           By: dradams
;;     Update #: 36
;; URL: http://www.emacswiki.org/find-func+.el
;; Keywords: emacs-lisp, functions, variables
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to `find-func.el'.
;;
;;  Commands defined here:
;;
;;    `find-library-other-window'.
;;
;;
;;  Suggested key binding (`C-x 4 l'):
;;
;;    (define-key ctl-x-4-map "l" 'find-library-other-window)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/06/15 dadams
;;     find-library-other-window: Require find-func.el.
;; 2008/09/07 dadams
;;     Created.  Added find-library-other-window.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun find-library-other-window (library)
  "Find the Emacs-Lisp source of LIBRARY in another window."
  (interactive
   (progn (require 'find-func)
          (let* ((path  (cons (or find-function-source-path load-path)
                              (find-library-suffixes)))
                 (def   (if (eq (function-called-at-point) 'require)
                            (save-excursion (backward-up-list)
                                            (forward-char)
                                            (backward-sexp -2)
                                            (thing-at-point 'symbol))
                          (thing-at-point 'symbol))))
            (when def (setq def  (and (locate-file-completion def path 'test) def)))
            (list (completing-read "Library name: " 'locate-file-completion
                                   path nil nil nil def)))))
  (let ((buf  (find-file-noselect (find-library-name library))))
    (pop-to-buffer buf 'other-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'find-func+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-func+.el ends here
