;;; compile-20.el --- Extensions to `compile.el'.
;;
;; Filename: compile-20.el
;; Description: Extensions to `compile.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1999-2017, Drew Adams, all rights reserved.
;; Created: Thu Sep  2 13:39:51 1999
;; Version: 0
;; Last-Updated: Tue Feb 21 16:07:42 2017 (-0800)
;;           By: dradams
;;     Update #: 198
;; URL: https://www.emacswiki.org/emacs/download/compile-20.el
;; Doc URL: http://www.emacswiki.org/GrepPlus
;; Keywords: tools, processes
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x
;;
;; Features that might be required by this library:
;;
;;   `fit-frame', `font-lock', `misc-fns'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `compile.el'.
;;
;;  See also the companion file `compile+.el'.
;;        `compile-20.el' should be loaded before `compile.el'.
;;        `compile+20.el' should be loaded after `compile.el'.
;;
;;
;;  New functions defined here:
;;        `compile-mode-summary', `fontify-buffer'
;;
;;
;;  ***** NOTE: The following variables defined in `compile.el'
;;              have been REDEFINED HERE:
;;
;;  `compile-auto-highlight' - Set to t, instead of nil.
;;  `compilation-minor-mode-map' -
;;    1. Full key map.  Unused keys bound to `compile-mode-summary'.
;;    2. Additional keys defined here: \r, \^?, ?, a, A, c, C,
;;       f, F, g, G, h, H, k, K, m, M, n, N, p, P, q, Q, r, R, {, },
;;       ;, M-;.
;;
;;  Functions `fontify-buffer' and `fit-1-window-frames-on'
;;  (defined in `fit-frame.el') are added here to
;;  `compilation-finish-functions'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2006/12/11 dadams
;;     Bound: remove-grep-comments, toggle-grep-comments.
;; 2005/12/03 dadams
;;     compilation-minor-mode-map: Bound q, Q to quit-window.
;; 2005/01/25 dadams
;;     Removed ###autoload on defvar.
;; 2004/10/08 dadams
;;     Renamed resize-1-window-frames-on to fit-1-window-frames-on, and
;;       moved it here from fit-frame.el.
;; 2004/06/01 dadams
;;     Renamed shrink-wrap-1-window-frames-on to resize-1-window-frames-on
;; 2004/03/16 dadams
;;     Added fontify-buffer and added it to compilation-finish-functions
;; 2000/09/27 dadams
;;     Updated for Emacs 20.7:
;;     1. Added: compile-auto-highlight, shrink-wrap-1-window-frames-on
;;     2. add-hook compilation-finish-functions shrink-wrap-1-window-frames-on
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'fit-frame nil t) ;; (no error if not found): fit-frame
(require 'misc-fns nil t)  ;; (no error if not found): fontify-buffer
(require 'font-lock nil t) ;; (no error if not found): font-lock-fontify-buffer

;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar compile-auto-highlight t)

(when (fboundp 'font-lock-fontify-buffer)
  (unless (fboundp 'fontify-buffer)     ; In `misc-fns.el'.
    (defun fontify-buffer (buffer &rest ignore)
      (save-excursion (set-buffer buffer)
                      (font-lock-fontify-buffer))))
  (add-hook 'compilation-finish-functions 'fontify-buffer))

(when (fboundp 'fit-frame)
  (defun fit-1-window-frames-on (buf &optional ignored)
    "Resize buffer BUF's one-window frame(s) to fit the buffer.
Usable, e.g., as a member of `compilation-finish-functions'."
    ;; Optional arg IGNORED is ignored.
    ;; It is for compatibility with `compilation-finish-functions'.
    (let ((frs (1-window-frames-on buf)))
      (while frs
        (fit-frame (car frs))           ; Defined in `fit-frame.el'.
        (setq frs (cdr frs)))))
  (add-hook 'compilation-finish-functions 'fit-1-window-frames-on))


;;;###autoload
(defun compile-mode-summary ()
  "Display brief help message for Compile Mode."
  (interactive)
  (message
   (concat
    (substitute-command-keys
     "\\[describe-mode]= help,  \\[compile-goto-error] & \
\\[compile-mouse-goto-error]= this error,  \\[next-error]= next error,  \
\\[kill-compilation]= kill,  \\[grep]= grep,  \\[compile]= compile,  \
\\[recompile]= recompile"))))




;; REPLACES ORIGINAL in `compile.el':
;; 1. Full key map (not sparse), with "unused" keys bound
;;    to `compile-mode-summary'.
;; 2. Additional keys defined here: \r, \^?, ?, a, A, c, C,
;;    f, F, g, G, h, H, k, K, m, M, n, N, p, P, r, R, {, }, ;, M-;.
(defvar compilation-minor-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'compile-goto-error) ; RET
    (define-key map " " 'scroll-up)     ; SPC
    (define-key map "\^?" 'scroll-down) ; DEL
    (define-key map "?" 'describe-mode) ; Defined in `help.el'.
    (define-key map "a" 'first-error)
    (define-key map "b" 'compile-mode-summary)
    (define-key map "c" 'compile)
    (define-key map "d" 'compile-mode-summary)
    (define-key map "e" 'compile-mode-summary)
    (define-key map "f" 'compile-goto-error)
    (define-key map "g" 'grep)
    (define-key map "h" 'describe-mode) ; Defined in `help.el'.
    (define-key map "i" 'compile-mode-summary)
    (define-key map "j" 'compile-mode-summary)
    (define-key map "k" 'kill-compilation)
    (define-key map "l" 'compile-mode-summary)
    (define-key map "m" 'compile)       ; Make.
    (define-key map "n" 'next-error)
    (define-key map "o" 'compile-mode-summary)
    (define-key map "p" 'previous-error)
    (define-key map "q" 'quit-window)
    (define-key map "r" 'recompile)
    (define-key map "s" 'compile-mode-summary)
    (define-key map "t" 'compile-mode-summary)
    (define-key map "u" 'compile-mode-summary)
    (define-key map "v" 'compile-mode-summary)
    (define-key map "w" 'compile-mode-summary)
    (define-key map "x" 'compile-mode-summary)
    (define-key map "y" 'compile-mode-summary)
    (define-key map "z" 'compile-mode-summary)
    (define-key map "A" 'first-error)
    (define-key map "B" 'compile-mode-summary)
    (define-key map "C" 'compile)
    (define-key map "D" 'compile-mode-summary)
    (define-key map "E" 'compile-mode-summary)
    (define-key map "F" 'compile-goto-error)
    (define-key map "G" 'grep)
    (define-key map "H" 'describe-mode) ; Defined in `help.el'.
    (define-key map "I" 'compile-mode-summary)
    (define-key map "J" 'compile-mode-summary)
    (define-key map "K" 'kill-compilation)
    (define-key map "L" 'compile-mode-summary)
    (define-key map "M" 'compile)       ; Make
    (define-key map "N" 'next-error)
    (define-key map "O" 'compile-mode-summary)
    (define-key map "P" 'previous-error)
    (define-key map "Q" 'quit-window)
    (define-key map "R" 'recompile)
    (define-key map "S" 'compile-mode-summary)
    (define-key map "T" 'compile-mode-summary)
    (define-key map "U" 'compile-mode-summary)
    (define-key map "V" 'compile-mode-summary)
    (define-key map "W" 'compile-mode-summary)
    (define-key map "X" 'compile-mode-summary)
    (define-key map "Y" 'compile-mode-summary)
    (define-key map "Z" 'compile-mode-summary)
    (define-key map [mouse-2] 'compile-mouse-goto-error)
    (define-key map "\C-c\C-c" 'compile-goto-error)
    (define-key map "\C-m" 'compile-goto-error)
    (define-key map "\C-c\C-k" 'kill-compilation)
    (define-key map "\M-n" 'compilation-next-error)
    (define-key map "\M-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    (define-key map "{"    'compilation-previous-file)
    (define-key map "}"    'compilation-next-file)
    (define-key map ";"    'remove-grep-comments)
    (define-key map "\M-;" 'toggle-grep-comments)
    map)
  "Keymap for `compilation-minor-mode'.")

;;;;;;;;;;;;;;;;;;

(provide 'compile-20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile-20.el ends here
