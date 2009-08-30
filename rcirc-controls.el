;;;; rcirc-controls.el -- control sequences
;; Copyright 2008  Alex Schroeder

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Thanks:
;;
;; Neale Pickett <neale@woozle.org>
;;    Much improved parser, addition of background colors, corrected
;;    number->color mapping, expand to full 16 mirc colors

;;; Code:

(require 'rcirc)

(defvar rcirc-color-vector ["white" "black" "blue" "green"
                            "red" "brown" "purple" "orange"
                            "yellow" "light green" "dark cyan" "cyan"
                            "light blue" "pink" "gray" "light gray"]
  "Vector of color names for the numbers 0-15.")

(defun rcirc-color-of-string (s)
  (cond
   ((null s) "default")
   ((equal s "") "default")
   ('t (aref rcirc-color-vector (mod (string-to-number s) 16)))))

(defun rcirc-markup-colors (&rest ignore)
  (while (re-search-forward "\C-c\\([0-9][0-9]?\\)?\\(,[0-9][0-9]?\\)?\\([^\C-c\n]*\\)" nil t)
    (let ((fg-color-name (rcirc-color-of-string (match-string 1)))
          (bg-color-name (rcirc-color-of-string (substring (or (match-string 2) ",") 1))))
      (rcirc-add-face (match-beginning 3) (match-end 3)
                      (list (cons 'foreground-color fg-color-name)
                            (cons 'background-color bg-color-name)))
      (delete-region (match-beginning 0) (match-beginning 3)))))

(add-to-list 'rcirc-markup-text-functions 'rcirc-markup-colors)

(defvar rcirc-control-alist '(("\C-b" . bold) ("\C-_" . underline)
			      ("\C-v" . inverse))
  "Alist of control sequences and faces to use.")

(defun rcirc-markup-controls (&rest ignore)
  (dolist (item rcirc-control-alist)
    (while (re-search-forward (concat (car item) "\\(.*?\\)" (car item)) nil t)
      (replace-match (match-string 1))
      (rcirc-add-face (match-beginning 0) (match-end 0) (cdr item)))))

(add-to-list 'rcirc-markup-text-functions 'rcirc-markup-controls)

(unless (facep 'inverse)
  (make-face 'inverse)
  (set-face-inverse-video-p 'inverse t))

(provide 'rcirc-controls)

;;; rcirc-controls.el ends here
