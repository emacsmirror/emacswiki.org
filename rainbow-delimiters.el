;;; rainbow-delimiters.el --- Colorize nested delimiters: () [] {}

;; Copyright (C) 2010  Jeremy Rayman
;; Copyright (C) 2009  Mark Triggs

;; Author: Mark Triggs <mst@dishevelled.net>, additions by
;;       Jeremy Rayman, and help from Alex Osborne <ato@meshy.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation:

;; Place rainbow-delimiters.el on your emacs load-path.

;; (require 'rainbow-delimiters)
;; M-x rainbow-delimiters-mode

;; To customize the colors, edit `*rainbow-delimiters-faces*'.
;; A customize interface will be added soon.

;;; Commentary:

;; This is a "rainbow parentheses" mode which includes support for
;; parens "()", brackets "[]", and braces "{}". It conveys nesting
;; depth by using a new color from *rainbow-delimiters-faces* for each
;; for each nested level. It correctly colorizes statements of the
;; same depth - if two statements are the same level, they will be the
;; same color. The user can change the variable
;; `*rainbow-delimiter-faces*' to set their own color choices.
;; Emacs customize dialog support is forthcoming.

;; Each set of delimiters "()[]{}" is colored independently from one
;; another. This means that color chosen is based on the nesting depth
;; _solely of that type of delimiter_. Parens, brackets, and braces
;; all maintain their colors depth, meaning even a bracket nested
;; within several levels of parentheses will start with the topmost
;; (lightest) color, and each nested bracket will then move to the
;; next color.

;; Default colors come from ZenBurn vim/emacs theme, available here:
;; http://slinky.imukuppi.org/zenburnpage/

;; Mark Triggs (with contributions from Alex Osborne) did the hard
;; parts in this code. This is a fork of their work to add several new
;; features. The original rainbow-parens.el is available at:
;; http://dishevelled.net/elisp/rainbow-parens.el

;; Bug reports about this code should be sent to Jeremy Rayman
;; <jeremy.rayman.public@gmail.com>. Please do not send bug reports to
;; the maintainers of rainbow-parens.el unless the bug is present in
;; their original version. Thank you.

;;; TODO:
;; - Add customize-interface for changing delimiter colors
;; - Use a separate *rainbow-______-face* for each type
;;   of delimiter to allow independent color choices for each one
;; - Improve performance on very delimiter-heavy code (performance is
;;   already good for most files including most lisp files)
;; - Add <> support with a toggle to colorize nested tags (XML, HTML)
;;   vs colorizing nested angle brackets like we do with the other
;;   types of delimiter.

;;; Code:

(require 'paredit)
(require 'cl)

;;; TODO: Customize interface:
;; (defgroup rainbow-delimiters nil
;;   "Color each nested set of delimiters differently to visually communicate nesting level.") ; should rainbow-delimiters belong to a :group?

(defun rainbow-delimiters-face-from-colour (colour)
  (let ((face (make-face (intern (concat "rainbow-delimiters-" colour "-face")))))
    (set-face-foreground face colour)
    face))

(defvar *rainbow-delimiters-faces*
  ;; First item is outermost parentheses color.
  `[,(rainbow-delimiters-face-from-colour "grey55")
    ,(rainbow-delimiters-face-from-colour "#7F9F7F")
    ,(rainbow-delimiters-face-from-colour "#8CD0D3")
    ,(rainbow-delimiters-face-from-colour "#DCA3A3")
    ,(rainbow-delimiters-face-from-colour "#385F38")
    ,(rainbow-delimiters-face-from-colour "#F0DFAF")
    ,(rainbow-delimiters-face-from-colour "#BCA3A3")
    ,(rainbow-delimiters-face-from-colour "#C0BED1")
    ,(rainbow-delimiters-face-from-colour "#FFCFAF")
    ,(rainbow-delimiters-face-from-colour "#F0EFD0")
    ,(rainbow-delimiters-face-from-colour "#F0DFAF")
    ,(rainbow-delimiters-face-from-colour "#DFCFAF")]
  "Faces to use in coloring parentheses, brackets, and braces. Begins with the outermost color.

The number of colors listed is variable and more colors may be added to support arbitrary nesting depth.")

(defun rainbow-delimiters-this-paren-nesting ()
  (let ((point (point))
        (depth 0))
    (while (ignore-errors
             (setq point (scan-lists point -1 1)))
      (when (= (char-after point) 40)
        (setq depth (1+ depth))))
    depth))

(defun rainbow-delimiters-this-bracket-nesting ()
  (let ((point (point))
        (depth 0))
    (while (ignore-errors
             (setq point (scan-lists point -1 1)))
      (when (= (char-after point) 91)
        (setq depth (1+ depth))))
    depth))

(defun rainbow-delimiters-this-brace-nesting ()
  (let ((point (point))
        (depth 0))
    (while (ignore-errors
             (setq point (scan-lists point -1 1)))
      (when (= (char-after point) 123)
        (setq depth (1+ depth))))
    depth))


(defun rainbow-delimiters-face-for-depth (n)
  (aref *rainbow-delimiters-faces*
        (mod n (length *rainbow-delimiters-faces*))))


(defun rainbow-delimiters-apply (point face)
  (let* ((os (overlays-at point))
         (o (or (some (lambda (o)
                        (and (eq (overlay-get o 'type) 'rainbow-delimiter)
                             o))
                      os)
                (make-overlay point (1+ point) nil t nil))))
    (overlay-put o 'type 'rainbow-delimiter)
    (overlay-put o 'face face)
    (overlay-put o 'evaporate t)))



(defun rainbow-delimiters-boring-delimiter-p ()
  (or (paredit-in-string-p)
      (paredit-in-comment-p)))


(defun rainbow-delimiters-skip-boring (bound)
  (while (and (< (point) bound)
              (rainbow-delimiters-boring-delimiter-p))
    (forward-char 1)))


(defun rainbow-delimiters-fontify (beg end)
  (save-excursion
    (goto-char beg)
    (rainbow-delimiters-skip-boring end)
    (let* ((paren-depth (rainbow-delimiters-this-paren-nesting))
           (bracket-depth (rainbow-delimiters-this-bracket-nesting))
           (brace-depth (rainbow-delimiters-this-brace-nesting)))
      (while (< (point) end)
        (rainbow-delimiters-skip-boring end)
        (cond ((= (char-after (point)) 40) ; (
               (rainbow-delimiters-apply (point)
                                    (rainbow-delimiters-face-for-depth paren-depth))
               (setq paren-depth (1+ paren-depth)))
              ((= (char-after (point)) 41) ; )
               (setq paren-depth (1- paren-depth))
               (rainbow-delimiters-apply (point)
                                    (rainbow-delimiters-face-for-depth paren-depth)))
              ((= (char-after (point)) 91) ; [
               (rainbow-delimiters-apply (point)
                                    (rainbow-delimiters-face-for-depth bracket-depth))
               (setq bracket-depth (1+ bracket-depth)))
              ((= (char-after (point)) 93) ; ]
               (setq bracket-depth (1- bracket-depth))
               (rainbow-delimiters-apply (point)
                                    (rainbow-delimiters-face-for-depth bracket-depth)))
              ((= (char-after (point)) 123) ; {
               (rainbow-delimiters-apply (point)
                                    (rainbow-delimiters-face-for-depth brace-depth))
               (setq brace-depth (1+ brace-depth)))
              ((= (char-after (point)) 125) ; }
               (setq brace-depth (1- brace-depth))
               (rainbow-delimiters-apply (point)
                                    (rainbow-delimiters-face-for-depth brace-depth)))
                                        ; < 60
                                        ; > 62
              )
        (forward-char 1)))))


(defun rainbow-delimiters-unfontify (beg end)
  (mapc #'(lambda (o)
            (when (eq (overlay-get o 'type) 'rainbow-delimiter)
              (delete-overlay o)))
        (overlays-in beg end)))


(define-minor-mode rainbow-delimiters-mode
  "Color each nested set of delimiters differently to visually communicate nesting level. Supports (), [], {}."

  nil " R" nil
  (cond ((not rainbow-delimiters-mode)
         (jit-lock-unregister 'rainbow-delimiters-fontify)
         (rainbow-delimiters-unfontify (point-min) (point-max)))
        (t (jit-lock-register 'rainbow-delimiters-fontify))))


(provide 'rainbow-delimiters)

;;; Other possible delimiter colors to use: (wide-gamut)
;; ,(rainbow-delimiters-face-from-colour "#7f7f7f")
;; ,(rainbow-delimiters-face-from-colour "#7f7f91")
;; ,(rainbow-delimiters-face-from-colour "#7f7fa1")
;; ,(rainbow-delimiters-face-from-colour "#95aabc")
;; ,(rainbow-delimiters-face-from-colour "#9d9d9d")
;; ,(rainbow-delimiters-face-from-colour "#9d9d9d")
;; ,(rainbow-delimiters-face-from-colour "#8d929b")
;; ,(rainbow-delimiters-face-from-colour "#7f8f9d")
;; ,(rainbow-delimiters-face-from-colour "#73818c")
;; ,(rainbow-delimiters-face-from-colour "#91b3d0")
;; ,(rainbow-delimiters-face-from-colour "#91b3d0")
;; ,(rainbow-delimiters-face-from-colour "#91b3d0")
;; ,(rainbow-delimiters-face-from-colour "#7ea7c9")
;; ,(rainbow-delimiters-face-from-colour "#6b9ac2")
;; ,(rainbow-delimiters-face-from-colour "gray100")
;; ,(rainbow-delimiters-face-from-colour "gray85")
;; ,(rainbow-delimiters-face-from-colour "gray70")
;; ,(rainbow-delimiters-face-from-colour "#6093be")
;; ,(rainbow-delimiters-face-from-colour "#588dba")

;;; Excellent set of colors for wide gamut screens:
   ;; [,(rainbow-delimiters-face-from-colour "grey55")
   ;;  ,(rainbow-delimiters-face-from-colour "#7f967f")
   ;;  ,(rainbow-delimiters-face-from-colour "#7199a1")
   ;;  ,(rainbow-delimiters-face-from-colour "#917f7f")
   ;;  ,(rainbow-delimiters-face-from-colour "#91937f")
   ;;  ,(rainbow-delimiters-face-from-colour "#7f9691")
   ;;  ,(rainbow-delimiters-face-from-colour "#949191")
   ;;  ,(rainbow-delimiters-face-from-colour "#919991")
   ;;  ,(rainbow-delimiters-face-from-colour "#949194")
   ;;  ,(rainbow-delimiters-face-from-colour "#949494")]

;;; rainbow-delimiters.el ends here
