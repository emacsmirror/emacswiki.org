;;; iswitchb-highlight.el --- Highlighting extension for iswitchb

;; Copyright (C) 2005 Peter K. Lee

;; Author: Peter K. Lee <saint@ c o r e n o v a .com>
;; Keywords; iswitchb, highlight

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

;;;_ + Commentary:

;; highlight elements in the iswitchb buffer list based on matching
;; MODE-NAME
;;
;; Example: 
;;   by default, the following MODES are highlighted w/in the iswitchb
;;   buffer list
;;
;;   Dired
;;   Planner
;;   Fundamental
;;   Completions

;;;_ + History:

;; 1.0 - 6/1/2005 - birthday

;;; Code:

(require 'iswitchb)

(defface saint/iswitchb-highlight-1-face
  '((t (:foreground "IndianRed1"))) 
  "*Face used to highlight terms in the iswitchb buffer")
(defface saint/iswitchb-highlight-2-face
  '((t (:foreground "DarkSeaGreen3"))) 
  "*Face used to highlight terms in the iswitchb buffer")
(defface saint/iswitchb-highlight-3-face
  '((t (:foreground "BrightBlack"))) 
  "*Face used to highlight terms in the iswitchb buffer")

(defcustom saint/iswitchb-highlight-modes-alist
  '(("Dired"       . 1)
	("Planner"     . 2)
    ("Fundamental" . 3)
    ("Completions" . 3))
  "*List specifying the mode name to face mapping for iswitchb.
Each entry specifies a map and is a list of the form of:
\(MODE-REGEXP FACE-NUMBER\) Resolves FACE-NUMBER to
saint/iswitchb-highlight-FACE-NUMBER-face."
  :type '(repeat sexp)
  :group 'iswitchb)

(defcustom saint/iswitchb-highlight-modes t
  "*Non-nil means that `iswitchb' will highlight matching modes as defined by
`saint/iswitchb-highlight-modes-list'."
  :type 'boolean
  :group 'iswitchb)

(defun saint/iswitchb-highlight-buflist ()
  "Highlight the buffer list based on the major mode that the buffer
is in.  This is a function to be hooked on to
`iswitchb-make-buflist-hook'.  Loops through `iswitchb-temp-buflist'
and retrieves the mode-name of each buffer.  Compares the buffer name
to the `saint/iswitch-highlight-modes-list' and if there's a match,
then replace the text-property of the buffer name string in the
buflist."
  (let ((my-highlight-buflist 
         (mapcar 
          (lambda (buf)
            (save-excursion
              (set-buffer buf)
              (let* ((match
                      (delq nil
                            (mapcar 
                             (lambda (x)
                               (if (and (consp x)
                                        (string-match (car x) mode-name))
                                   (cdr x)))
                             saint/iswitchb-highlight-modes-alist)))
                     (face-num (if (and match
                                        (and (listp match)))
                                   (car match)
                                 0)))
                (cond 
                 ((= face-num 1) (propertize buf 'face 'saint/iswitchb-highlight-1-face))
                 ((= face-num 2) (propertize buf 'face 'saint/iswitchb-highlight-2-face))
                 ((= face-num 3) (propertize buf 'face 'saint/iswitchb-highlight-3-face))
                 (t buf)
                 ))))
          iswitchb-temp-buflist)))
    (setq iswitchb-temp-buflist my-highlight-buflist)))

(add-hook 'iswitchb-make-buflist-hook 'saint/iswitchb-highlight-buflist)

(provide 'iswitchb-highlight)

;;; iswitchb-highlight.el ends here

