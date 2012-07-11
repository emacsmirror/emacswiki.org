;;; relative-linum.el -- show relative line number and allow offset jumping

;; Version: 1.0
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Mostly taken from http://stackoverflow.com/a/7882046

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary

;; Relative line numbers, Vim style.

;; This will modify the linum (line numbering) behaviour in Emacs. To
;; enable it in a single buffer, do `M-x linum-mode` or execute
;; (linum-mode 1).

;; To enable this globally, add this to your .emacs:
;; (global-linum-mode t)

(defvar relative-linum-format-string "%3d")

(add-hook 'linum-before-numbering-hook 'relative-linum-get-format-string)

(defun relative-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
                             (count-lines (point-min) (point-max))))))
         (format (concat "%" (number-to-string width) "d")))
    (setq relative-linum-format-string format)))

(defvar relative-linum-current-line-number 0)

(setq linum-format 'relative-linum-relative-line-numbers)

(defun relative-linum-relative-line-numbers (line-number)
  (let ((offset (- line-number relative-linum-current-line-number)))
    (propertize (format relative-linum-format-string offset) 'face 'linum)))

(defadvice linum-update (around relative-linum-update)
  (let ((relative-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(defun relative-linum-jump (offset)
  "Move OFFSET lines up or down from the current line."
  (interactive "nLine offset: ")
  (forward-line offset))

(global-set-key (kbd "C-S-n") 'relative-linum-jump)

(provide 'relative-linum)
