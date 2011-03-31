;;; himark.el --- marking text by highlighting

;; Copyright (C) 1999 by Roland Winkler

;; Emacs Lisp Archive Entry
;; Author: Roland Winkler <Roland.Winkler@physik.uni-erlangen.de>
;;         based on ideas by Ehud Karni <ehud@unix.simonwiesel.co.il>
;; Keywords: faces
;; Version: 1.0
;; Time-stamp: <2001-02-14 11:28:04 winkler>

;; himark.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; himark.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; himark.el provides marking of text in a buffer by highlighting.
;; Unlike transient-mark-mode the marking is permanent and can be
;; accumulated. One can mark the region, the occurances of a regexp
;; or a rectangle. The markings in a buffer can be removed with
;; himark-unset.

;; For me, a typical application is when I am studying in one or
;; more emacs buffers the output of some other programs which
;; contain many lines and columns of numbers, numbers and more
;; numbers, and I need to find and compare the important ones. Or I
;; might like to see how often I have used some words in a piece of
;; text.

;; Comments / suggestions welcome!

;;; Code:

(provide 'himark)

(defvar himark-overlay-list nil
  "List of himark overlays (himark-unset deletes them).
This variable's value is local in all buffers.")
(make-variable-buffer-local 'himark-overlay-list)

(defvar himark-overlay-face 'highlight
  "Name of face (quoted symbol) to use for himark.")

(defun himark-unset ()
  "Remove himark overlay in this buffer."
  (interactive)
  (while himark-overlay-list
    (delete-overlay (car himark-overlay-list))
    (setq himark-overlay-list (cdr himark-overlay-list))))

(defun himark-regexp (regexp &optional start end)
  "Highlight all occurrence of REGEXP. By default operate on the
content of the buffer. Optional arguments START, END delimit the
region."
  (interactive (let (start end)
                 ;; must handle region before reading minibuffer input
                 ;; otherwise the region might have changed
                 (when (and transient-mark-mode mark-active)
                   (setq start (region-beginning)
                         end (region-end))
                   (deactivate-mark))
                 (list (read-string "regexp to himark: ")
                       start end)))
  (if (not start) (setq start (point-min)))
  (if (not end)   (setq end   (point-max)))
  (save-excursion
    (goto-char start)
    (while (re-search-forward regexp end t)
      (himark-region (match-beginning 0) (match-end 0)))))

(defun himark-region (start end)
  "Highlight region between START and END."
  (interactive "r")
  (if (and (interactive-p) transient-mark-mode mark-active)
      (deactivate-mark))
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face himark-overlay-face)
    (overlay-put ov 'priority 98)
    (setq himark-overlay-list (append (list ov) himark-overlay-list))))

(defun himark-rectangle (start end)
  "Highlight contents of rectangle between mark and point."
  (interactive "r")
  (if (and (interactive-p) transient-mark-mode mark-active)
      (deactivate-mark))
  (operate-on-rectangle 'himark-rectangle-line start end nil))

(defun himark-rectangle-line (startpos begextra endextra)
  (himark-region startpos (point)))

;; By default operate-on-rectangle is not available
(autoload 'operate-on-rectangle "rect"
          "Call FUNCTION for each line of rectangle.")

;;; himark.el ends here
