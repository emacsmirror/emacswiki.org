;;; ergo-movement-mode.el --- Ergonomic keybindings for cursor movement
;;
;; Copyright (C) 2009-2010 Teemu Likonen

;; Author: Teemu Likonen <tlikonen@iki.fi>
;; Updated: 20100116

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Ergo Movement mode is a global minor mode which defines ergonomic
;; keybindings for cursor movement. See the function documentation
;; string below for more information.
;;
;; The movement keys are inspired by Xah Lee's Ergoemacs keybindings:
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html

;; Installation:
;;
;; Put this file somewhere in your load-path. You can add an autoload
;; function to your ~/.emacs file
;;
;;     (autoload 'ergo-movement-mode "ergo-movement-mode"
;;       "Ergonomic keybindings for cursor movement" 'interactive)
;;
;; Or if you want to turn on the mode automatically when Emacs is
;; started put these lines in your ~/.emacs file:
;;
;;     (require 'ergo-movement-mode)
;;     (ergo-movement-mode 1)

;;; Code:

(defvar ergo-movement-mode-map
  (let ((map (make-sparse-keymap))
        ;; "keydefs" is an alist of (KEY . ACTION) elements. KEY is a
        ;; keybinding string. ACTION can be a keybinding string or a
        ;; symbol referring to a command. Keybinding strings use the
        ;; format known by the "kbd" macro.
        (keydefs '(
                   ;; indent-new-comment-line -> backward-char
                   ("M-j" . "C-b")
                   ("M-J" . indent-new-comment-line)
                   ;; downcase-word -> forward-char
                   ("M-l" . "C-f")
                   ("M-L" . downcase-word)
                   ;; tab-to-tab-stop -> previous-line
                   ("M-i" . "C-p")
                   ("M-I" . tab-to-tab-stop)
                   ;; kill-sentence -> next-line
                   ("M-k" . "C-n")
                   ("M-K" . kill-sentence)
                   ;; indent-new-comment-line -> backward-word
                   ("C-M-j" . "M-b")
                   ;; reposition-window -> forward-word
                   ("C-M-l" . "M-f")
                   ("C-M-S-l" . reposition-window)
                   ))
        key def)

   (dolist (keydef keydefs)
     (setq key (eval `(kbd ,(car keydef)))
           def (cdr keydef))
     (cond ((commandp def t)
            (define-key map key def))
           ((stringp def)
            (define-key map key
              `(lambda () (interactive)
                 (call-interactively
                  (key-binding ,(read-kbd-macro def))))))))
   map))


;;;###autoload
(define-minor-mode ergo-movement-mode
  "Ergonomic keybindings for cursor movement

Ergo Movement mode is a global minor mode which defines ergonomic
keybindings for cursor movement. This is suitable for QWERTY
keyboard.

             i      =          C-p
    M-     j k l    =    C-b   C-n   C-f

    C-M-   j   l    =    M-b         M-f

The original bindings of the above movement commands are kept
untouched. The new bindings override other commands though. Those
commands are resurrected through shifted versions of their
original keybindings.

\\{ergo-movement-mode-map}"

  :global t
  :lighter ""
  :keymap ergo-movement-mode-map
  )


(provide 'ergo-movement-mode)
