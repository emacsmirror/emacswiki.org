;;; css-sort-buffer.el --- Sort CSS buffer

;; Filename: css-sort-buffer.el
;; Description: Sort CSS buffer
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-07-13 08:59:01
;; Version: 0.2
;; Last-Updated: 2018-07-13 12:29:49
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/css-sort-buffer.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Sort CSS buffer just need one command `css-sort-buffer'.
;;

;;; Installation:
;;
;; Put css-sort-buffer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'css-sort-buffer)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET css-sort-buffer RET
;;

;;; Change log:
;;
;; 2018/07/13
;;      * First released.
;;      * Adjust order of `line-height' attributable.
;;

;;; Acknowledgements:
;;
;; https://github.com/diiq/css-sort.el is awesome!
;;

;;; TODO
;;
;;
;;

;;; Require

;;; Code:

(setq css-sort-attributes-order
      '(
        "content"
        "display"
        "position"
        "font"
        "font-family"
        "font-size"
        "font-weight"
        "color"
        "background"
        "background-color"
        "background-image"
        "background-repeat"
        "background-position"
        "top"
        "bottom"
        "left"
        "right"
        "width"
        "line-width"
        "height"
        "line-height"
        "min-width"
        "min-height"
        "max-width"
        "max-height"
        "padding"
        "padding-top"
        "padding-bottom"
        "padding-left"
        "padding-right"
        "float"
        "clear"
        "flex-direction"
        "visibility"
        "opacity"
        "margin"
        "margin-top"
        "margin-bottom"
        "margin-left"
        "margin-right"
        "border"
        "border-radius"
        "border-top"
        "border-bottom"
        "border-left"
        "border-right"
        "border-width"
        "border-height"
        "border-top-width"
        "border-bottom-width"
        "border-left-width"
        "border-right-width"
        "border-style"
        "border-top-style"
        "border-bottom-style"
        "border-left-style"
        "border-right-style"
        "border-color"
        "border-top-color"
        "border-bottom-color"
        "border-left-color"
        "border-right-color"
        "box-shadow"
        "outline"
        "cursor"
        "overflow"
        "list-style"
        "list-style-type"
        "caption-side"
        "table-layout"
        "border-collapse"
        "border-spacing"
        "empty-cells"
        "vertical-align"
        "text-align"
        "text-indent"
        "text-transform"
        "text-decoration"
        "transform"
        "word-spacing"
        "letter-spacing"
        "white-space"
        "z-index"
        "align-items"
        "justify-content"
        "quotes"))

(defun css-sort-index (object list)
  "return the index of object in list"
  (let ((counter 0)
        (found nil))
    (catch 'finished
      (dolist (listelement list counter)
        (if (equal object listelement)
            (progn
              (setq found t)
              (throw 'finished counter))
          ;; else increment counter
          (incf counter)))
      ;; if we found it return counter otherwise return nil
      (if found counter nil))))

                                        ; (css-sort-index 'a '(b c a d)) => 2
                                        ; (css-sort-index "background" css-sort-attributes-order)

(defun css-sort-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

                                        ; (css-sort-chomp "    alan  ricky    ") => "alan  ricky"

(defun css-sort-attribute-from-line (line)
  (css-sort-chomp (nth 0 (split-string line ":"))))

                                        ; (css-attribute-from-line "    color: symople;") => "color"

(defun css-sort-attribute-index (line)
  (or (css-sort-index
       (css-sort-attribute-from-line line) css-sort-attributes-order)
      -1))

                                        ; (css-sort-attribute-index "    color: aslfa") => 70

(defun css-attribute-sort-compare (a b)
  (< (css-sort-attribute-index a)
     (css-sort-attribute-index b)))

                                        ; (css-attribute-sort-compare "    position: symfo" "   background: qwants") => 't

(defun css-sort-lines-in-region (start end)
  (split-string (buffer-substring start end) "[\n]"))


(defun css-sort-beginning-of-attribute-block (start)
  (goto-char start)
  (search-backward "{")
  (forward-line 1)
  (beginning-of-line)
  (point))

                                        ; { (beginning-of-attribute-block (point)) => 4044 or something

(defun css-sort-end-of-attribute-block (start)
  (goto-char start)
  (re-search-forward "[{}]")
  (forward-line -1)
  (end-of-line)
  (point))

(defun css-sort-buffer ()
  "css-sort is cool, this function sort whole css file,
don't need user sort css attributable line by line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\s-+{" (point-max) t)
      (save-excursion
        (let* ((current (point))
               (start (css-sort-beginning-of-attribute-block current))
               (end (css-sort-end-of-attribute-block current))
               (lines (css-sort-lines-in-region start end))
               (sorted-lines (sort lines #'css-attribute-sort-compare)))
          (delete-region start end)
          (save-excursion
            (goto-char start)
            (insert (mapconcat 'identity sorted-lines "\n"))))))))

(provide 'css-sort-buffer)

;;; css-sort-buffer.el ends here
