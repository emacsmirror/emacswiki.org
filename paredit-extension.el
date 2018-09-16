;;; paredit-extension.el --- Simple extension base on paredit.el

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008 ~ 2018, Andy Stewart, all rights reserved.
;; Created: 2008-07-28 16:32:52
;; Version: 1.0
;; Last-Updated: 2018-09-16 10:24:50
;; URL: not distributed yet
;; Keywords: paredit
;; Compatibility: GNU Emacs 23.0.60.1 ~ GNU Emacs 27.0.50

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

;; Features that might be required by this library:
;;
;;  `paredit' `lazycat-toolkit' `thingatpt'
;;

;;; Installation:
;;
;; Copy paredit-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'paredit-extension)
;;
;; No need more

;;; Commentary:
;;
;; This extension is base on paredit.el of Taylor R. Campbell.
;;

;;; Change log:
;;
;; 2018/09/16
;;      * Adjust <% regexp of `paredit-web-mode-kill'.
;;
;; 2018/09/15
;;      * `paredit-web-mode-kill' support rails template now.
;;
;; 2018/08/26
;;      * Add new function `paredit-match-paren'.
;;
;; 2018/08/21
;;      * Fix bug of `paredit-web-mode-kill' not kill tag attribute correctly.
;;
;; 2018/08/06
;;      * Improve function `paredit-ruby-mode-kill' that reindent line if rest line start with ruby keywords.
;;
;; 2018/07/11
;;      * Add new function `paredit-web-mode-kill' and `paredit-ruby-mode-kill'.
;;      * Add new function `paredit-common-mode-kill' and refactry `paredit-kill+'.
;;
;; 2018/06/26
;;      * Add `python-mode' in black list of `paredit-open-curly-smart'
;;
;; 2017/02/24
;;      * Fixed `paredit-open-curly-smart' bug that indent wrong column.
;;
;; 2017/02/23
;;      * Add `paredit-open-curly-smart'.
;;
;; 2014/03/16
;;      * Add `paredit-kill+'.
;;      * Add `paredit-duplicate-closest-sexp'.
;;
;; 2008/07/28
;;      * First release.
;;

;;; Acknowledgments:
;;
;;      Taylor R. Campbell      <campbell@mumble.net>   for paredit.el
;;

;;; TODO
;;
;; None
;;

;;; Require

(require 'thingatpt)
(require 'lazycat-toolkit)
(require 'paredit)

;;; Code:

(defun paredit-comment-list ()
  "Comment current list."
  (interactive)
  (save-excursion
    (paredit-comment-list-base)))

(defun paredit-comment-list-and-newline ()
  "Comment current list and newline."
  (interactive)
  (paredit-comment-list-base)
  (newline-and-indent))

(defun paredit-comment-list-base ()
  "Base function that comment current list."
  (if (paredit-in-string-p)
      (goto-char (car (paredit-string-start+end-points))))
  (comment-or-uncomment-region
   (save-excursion
     (backward-up-list)
     (point))
   (progn
     (up-list)
     (point))))

(defun paredit-close-round-and-newline+ ()
  "Make paredit can jump and indent behind close quotation when cursor in quotations."
  (interactive)
  (if (paredit-in-string-p)
      (progn
        (goto-char (1+ (cdr (paredit-string-start+end-points))))
        (newline-and-indent))
    (paredit-close-round-and-newline)))

(defun paredit-forward-sexp-and-newline ()
  "Forward sexp and newline.
Newline current point if current sexp is last one."
  (interactive)
  (let ((original-position (point)))
    (unless
        ;; Forward sexp.
        (ignore-errors
          (forward-sexp 1)
          t)
      ;; Otherwise return original position.
      (goto-char original-position))
    ;; Newline and indent.
    (when (looking-at "\\s-*;.*$")
      (end-of-line))
    (newline-and-indent)))

(defun paredit-move-list-forward ()
  "Move current list across forward."
  (interactive)
  (let (text
        init-point
        excursion)
    ;; remember init position
    (setq init-point (point))
    ;; adjust position when in string
    (if (paredit-in-string-p)
        (goto-char (car (paredit-string-start+end-points))))
    ;; get list
    (setq text (delete-and-extract-region
                (progn
                  (backward-up-list)
                  (setq excursion (- init-point (point)))
                  (point))
                (progn
                  (forward-list)
                  (point))))
    ;; clean whitespace before move
    (if (eolp)
        (delete-chars-hungry-forward)
      (delete-chars-hungry-backward))
    ;; jump to paste position
    (paredit-forward)
    ;; adjust position before paste
    (if (eolp)
        (newline-and-indent)
      (insert " "))
    ;; insert list and resume init position before move
    (insert text)
    (backward-list)
    (forward-char excursion)
    ))

(defun paredit-move-list-backward ()
  "Move current list across backward"
  (interactive)
  (let (text
        init-point
        excursion)
    ;; remember init position
    (setq init-point (point))
    ;; adjust position when in string
    (if (paredit-in-string-p)
        (goto-char (car (paredit-string-start+end-points))))
    ;; get list
    (setq text (delete-and-extract-region
                (progn
                  (backward-up-list)
                  (setq excursion (- init-point (point)))
                  (point))
                (progn
                  (forward-list)
                  (point))))
    ;; clean whitespace before move
    (if (eolp)
        (delete-chars-hungry-backward)
      (delete-chars-hungry-forward))
    ;; jump to paste position
    (paredit-backward)
    ;; adjust position before paste
    (if (colp)
        (progn
          (beginning-of-line)
          (open-line 1)
          (indent-according-to-mode))
      (insert " ")
      (backward-char +1))
    ;; insert list and resume init position before move
    (insert text)
    (backward-list)
    (forward-char excursion)
    ))

(defun paredit-insert-quote (arg)
  "Insert quote."
  (interactive "p")
  (if (or (> arg 1) (paredit-in-comment-p) (paredit-in-string-p))
      (self-insert-command (or arg 1))
    (self-insert-command 1)
    (cond
     ((or
       (eq major-mode 'emacs-lisp-mode)
       (eq major-mode 'lisp-mode)
       (eq major-mode 'scheme-mode)
       (eq major-mode 'lisp-interaction-mode)
       (eq major-mode 'org-mode))
      (progn
        (insert "'")
        (backward-char)))
     ((or
       (eq major-mode 'haskell-mode))
      (progn
        (insert "`")
        (backward-char))))))

(defun paredit-splice-sexp+ (&optional argument)
  "This function extension `paredit-splice-sexp'.
Will delete blank line after execute `paredit-splice-sexp'."
  (interactive "P")
  (paredit-splice-sexp argument)
  (when (blank-line-p)
    (call-interactively 'move-beginning-of-line)
    (kill-line)
    (back-to-indentation)))

(defun paredit-blank-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun paredit-kill-blank-line-and-reindent ()
  (interactive)
  (kill-region (beginning-of-thing 'line) (end-of-thing 'line))
  (back-to-indentation))

(defun paredit-kill+ ()
  "It's annoying that we need re-indent line after we delete blank line with `paredit-kill'.
`paredt-kill+' fixed this problem.

If current mode is `web-mode', use `paredit-web-mode-kill' instead `paredit-kill' for smarter kill operation."
  (interactive)
  (cond ((eq major-mode 'web-mode)
         (paredit-web-mode-kill))
        ((eq major-mode 'ruby-mode)
         (paredit-ruby-mode-kill))
        (t
         (paredit-common-mode-kill))))

(defun paredit-common-mode-kill ()
  (interactive)
  (if (paredit-blank-line-p)
      (paredit-kill-blank-line-and-reindent)
    (paredit-kill)))

(defun paredit-web-mode-kill ()
  "It's a smarter kill function for `web-mode'.

If current line is blank line, re-indent line after kill whole line.
If point in string area, kill string content like `paredit-kill' do.
If point in tag area, kill nearest tag attribute around point.
If point in <% ... %>, kill rails code.
Otherwise, do `paredit-kill'."
  (interactive)
  (if (paredit-blank-line-p)
      (paredit-kill-blank-line-and-reindent)
    (cond ((paredit-in-string-p)
           (paredit-kill))
          (t
           (let (char-count-before-kill
                 char-count-after-kill
                 kill-start-point)
             ;; Try do `web-mode-attribute-kill'.
             (setq char-count-before-kill (- (point-max) (point-min)))
             (web-mode-attribute-kill)
             (setq char-count-after-kill (- (point-max) (point-min)))
             ;; Try continue if nothing change after `web-mode-attribute-kill'.
             (when (equal char-count-before-kill char-count-after-kill)
               ;; Do `paredit-kill' if point at front of <%.
               (if (looking-at "\\(\\s-+<%\\|<%\\)")
                   (paredit-kill)
                 (setq kill-start-point (point))
                 ;; Kill content in <% ... %> if found %> in rest line.
                 (if (search-forward-regexp
                      ".*\\(%>\\)"
                      (save-excursion
                        (end-of-line)
                        (point))
                      t)
                     (progn
                       (backward-char (length (substring-no-properties (match-string 1))))
                       (kill-region kill-start-point (point)))
                   ;; Do `paredit-kill' last.
                   (paredit-kill)))
               ))))))

(defun paredit-ruby-mode-kill ()
  "It's a smarter kill function for `ruby-mode'.

If current line is blank line, re-indent line after kill whole line.

If current line is not blank, do `paredit-kill' first, re-indent line if rest line start with ruby keywords.
"
  (interactive)
  (if (paredit-blank-line-p)
      (paredit-kill-blank-line-and-reindent)
    ;; Do `paredit-kill' first.
    (paredit-kill)

    ;; Re-indent current line if line start with ruby keywords.
    (when (let (in-beginning-block-p
                in-end-block-p
                current-symbol)
            (save-excursion
              (back-to-indentation)
              (ignore-errors (setq current-symbol (buffer-substring-no-properties (beginning-of-thing 'symbol) (end-of-thing 'symbol))))
              (setq in-beginning-block-p (member current-symbol '("class" "module" "else" "def" "if" "unless" "case" "while" "until" "for" "begin" "do")))
              (setq in-end-block-p (member current-symbol '("end")))

              (or in-beginning-block-p in-end-block-p)))
      (indent-for-tab-command))))

(defun paredit--is-at-start-of-sexp ()
  (and (looking-at "(\\|\\[")
       (not (nth 3 (syntax-ppss)))   ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun paredit-duplicate-closest-sexp ()
  (interactive)
  ;; skips to start of current sexp
  (while (not (paredit--is-at-start-of-sexp))
    (paredit-backward))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

(defun paredit-open-curly-smart ()
  (interactive)
  (if (or
       (eq major-mode 'rjsx-mode)
       (eq major-mode 'js-mode)
       (eq major-mode 'js2-mode)
       (eq major-mode 'ruby-mode)
       (eq major-mode 'python-mode)
       (eq major-mode 'lua-mode)
       )
      ;; Just do same as `paredit-open-curly' in some mode.
      (paredit-open-curly)
    ;; Otherwise do something smart operation.
    (paredit-open-curly)
    (indent-according-to-mode)
    (comment-indent-new-line)
    (open-newline-above 1)))

(defun paredit-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((or (paredit-in-comment-p)
             (paredit-in-string-p))
         (self-insert-command (or arg 1)))
        ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
         (forward-list))
        ((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
         (backward-list))
        (t (self-insert-command (or arg 1)))))

(provide 'paredit-extension)

;;; paredit-extension.el ends here

;;; LocalWords:  uncomment
