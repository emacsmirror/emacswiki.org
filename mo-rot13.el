;;; mo-rot13.el --- convert text buffer to and from ROT13

;; This file is NOT part of Emacs.

;; Copyright (C) 2009 Matthew Ozor <matthew.ozor@gmail.com>
;; Filename: mo-rot13.el
;; Version: $Revision: 1.0 $
;; EmacsWiki version: %Id: 5%
;; Author: Matthew Ozor <matthew.ozor@gmail.com>
;; Created: 2009-06-16

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:

;; Add new menu items and new commands to convert text to and from
;; ROT13 encoding.

;; Vital for any coder :)
;; The function `mo-rot13-region' uses a lookup table to convert
;; marked region to or from ROT13.
;; The function `mo-rot13-buffer' expands on mo-rot13-region
;; The function `mo-rot13-word' expands on mo-rot13-region
;; The function `mo-rot13-sentence' expands on mo-rot13-region
;; The function `mo-rot13-line' expands on mo-rot13-region

;;; History:

;; Written on June 16, 2009 by Matthew Ozor.

;; Changes on August 15, 2009 by Aaron S. Hawley:
;; Leverage rot13.el library that comes with Emacs.
;; Handle case where no word at point.
;; Run M-x checkdoc to fix coding style.

;;; Code:

(require 'rot13)

(defun mo-rot13-buffer ()
  "Convert whole buffer to and from ROT13."
  (interactive "*")
  (rot13 (current-buffer) (point-min) (point-max)))

(defun mo-rot13-word ()
  "Convert word at current point to and from ROT13."
  (interactive "*")
  (let ((bds (bounds-of-thing-at-point 'word)))
    (if (null bds)
	(message "No word at point")
      (save-excursion
	(rot13-region (car bds) (cdr bds))))))

(defun mo-rot13-sentence ()
  "Convert sentence at current point to and from ROT13."
  (interactive "*")
  (let ((bds (bounds-of-thing-at-point 'sentence)))
    (save-excursion
      (rot13-region (car bds) (cdr bds)))))

(defun mo-rot13-line ()
  "Convert line at current point to and from ROT13."
  (interactive "*")
  (let ((bds (bounds-of-thing-at-point 'line)))
    (save-excursion
      (rot13-region (car bds) (cdr bds)))))

(defvar menu-bar-rot13-menu (make-sparse-keymap "ROT13"))

(define-key menu-bar-encode-menu [ROT13]
  (list 'menu-item "ROT13" menu-bar-rot13-menu))

(define-key menu-bar-rot13-menu [word]
  '(menu-item "Word" mo-rot13-word
	      :help "Convert word to ROT13"))
(define-key menu-bar-rot13-menu [sentence]
  '(menu-item "Sentence" mo-rot13-sentence
	      :help "Convert sentence to ROT13"))
(define-key menu-bar-rot13-menu [line]
  '(menu-item "Line" mo-rot13-line
	      :help "Convert line to ROT13"))
(define-key menu-bar-rot13-menu [buffer]
  '(menu-item "Buffer" mo-rot13-buffer
	      :help "Convert buffer to ROT13"))
(define-key menu-bar-rot13-menu [region]
  '(menu-item "Region" rot13-region
	      :enable mark-active
	      :help "Convert region to ROT13"))

(provide 'mo-rot13)
;;; mo-rot13.el ends here
