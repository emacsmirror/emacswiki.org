;;; find-library.el --- Find emacs-lisp library with completion

;; Copyright (C) 2001, 2002  Free Software Foundation, Inc.

;; Author: Kahlil (Kal) HODGSON <dorge@tpg.com.au>
;; X-URL: http://www.emacswiki.org/elisp/find-library.el
;; Keywords: lisp, help

;; This file is NOT part of GNU Emacs.

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A real fast way to track down library files in your load-path.
;; Basically I've found NSF's fff.el a little buggy and TM's
;; find-library.el way to slow.  This is a quick hack that uses Gareth
;; Rees's `read-library' package. This all of course assumes you are
;; only interested in files in your load-path.

;;; Code:

(require 'read-library)

;;;###autoload
(defun find-library (library)
  "Load the library named LIBRARY.
This is an interface to the function `load'.
When called interactively, prompt with completion."
  (interactive (list (read-library "Find library: ")))
  (find-file (locate-library (concat library ".el") t)))

(defun find-library-other-window (library)
  "Load the library named LIBRARY.
This is an interface to the function `load'.
When called interactively, prompt with completion."
  (interactive (list (read-library "Find library: ")))
  (find-file-other-window (locate-library (concat library ".el") t)))

(defun find-library-other-frame (library)
  "Load the library named LIBRARY.
This is an interface to the function `load'.
When called interactively, prompt with completion."
  (interactive (list (read-library "Find library: ")))
  (find-file-other-frame (locate-library (concat library ".el") t)))

;; analogous to find-function and find-variable
(define-key ctl-x-map   "L" 'find-library)
(define-key ctl-x-4-map "L" 'find-library-other-window)
(define-key ctl-x-5-map "L" 'find-library-other-frame)

(provide 'find-library)
;;; find-library.el ends here
