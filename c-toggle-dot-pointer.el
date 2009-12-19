;;; c-toggle-dot-pointer.el --- Functions for toggling a variable in a C function between pointer and non-pointer

;; Copyright (C) 2006  Marc Abramowitz

;; Author: Marc Abramowitz (http://marc.abramowitz.info)
;; Keywords: c languages

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

;; Kind of hard to describe - try it! :-) Basically, if you're on a "*",
;; it will delete the "*" and then change all references using that variable
;; in the function from "->" to ".". If you're not on a "*", then it will add
;; a "*" and change references within the function form using "." to "->".

;;; Code:

(defun c-toggle-dot-pointer ()
  (interactive)
  (cond
   ((looking-at "\\.")   (replace-string "." "->" nil (point) (+ (point) 1)))
   ((looking-at "\\->")  (replace-string "->" "." nil (point) (+ (point) 2)))))

(defun c-toggle-dot-pointer-in-defun ()
  (interactive)
  (atomic-change-group
    (save-excursion
      (let ((is-ptr nil)
            (var-under-cursor)
            (end-of-func))
        (when (eq (char-after) ?*)
          (delete-char 1)
          (setf is-ptr t))
        (if (looking-at "[a-zA-Z0-9_]+")
        ;then
          (setf var-under-cursor (match-string 0))
        ;else
          (error "not variable"))
        (save-excursion (c-end-of-defun) (setf end-of-func (point)))
        (if is-ptr
        ;then
          (replace-string (concat var-under-cursor "->")
                          (concat var-under-cursor ".")
                          nil
                          (point)
                          end-of-func)
        ;else
          (insert "*")
          (replace-string (concat var-under-cursor ".")
                          (concat var-under-cursor "->")
                          nil
                          (point)
                          end-of-func))))))

(provide 'c-toggle-dot-pointer)
;;; c-toggle-dot-pointer.el ends here
