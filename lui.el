;;; lui.el --- Linewise User Interface

;; Copyright (C) 2004  Jorgen Schaefer <forcer@forcix.cx>

;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; Keywords: application, user-interface

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; Lui is a simple extensible mode that allows any program to present
;; a userfriendly interface to the user. This interface is closely
;; oriented on the user interface of ERC.

;; To use, simply call lui-mode with a function to handle user input
;; in your mode function. Don't kill all local variables afterwards,
;; lui-mode does that for you!

;; Now your function should be called with user input, and you can
;; call `lui-insert' and `lui-prompt' at will.

;; That's it.

;;; Code:

(defvar lui-version "0.9"
  "Lui version string.")

(defgroup lui nil
  "The Linewise User Interface."
  :prefix "lui-"
  :group 'applications)

(defcustom lui-input-ring-size 32
  "Size of input history ring."
  :type 'integer
  :group 'lui)

(defcustom lui-mode-hook nil
  "The hook run after `lui-mode' has been set up."
  :type 'hook
  :options '(lui-add-scroll-to-bottom)
  :group 'lui)

(defcustom lui-insert-hook nil
  "This hook is run narrowed to every line that is inserted into the
buffer."
  :type 'hook
  :group 'lui)

;;; Variables:

(defvar lui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'lui-send-input)
    (define-key map (kbd "M-p") 'lui-previous-input)
    (define-key map (kbd "M-n") 'lui-next-input)
    map)
  "The keymap for the LUI mode.")

(defvar lui-input-function nil
  "This function is called when the user types something to the
program, with the single argument being the string the user typed.")
(make-variable-buffer-local 'lui-input-function)

(defvar lui-input-marker nil
  "Marks where the user input starts.")
(make-variable-buffer-local 'lui-input-marker)

(defvar lui-output-marker nil
  "Marks where the program output ends.")
(make-variable-buffer-local 'lui-output-marker)

(defvar lui-input-ring nil
  "The input ring of lui.")
(make-variable-buffer-local 'lui-input-ring)

(defvar lui-input-ring-index nil
  "The index into the input ring.")
(make-variable-buffer-local 'lui-input-ring-index)

;;; Code:
(defun lui-mode (input-func)
  "A mode for Linewise User Interaction. This mode will set up a
shell-style user interface, especially taylored for Emacs programs
that need to interact with the user.

\\{lui-mode-map}"
  (interactive "aInput function: ")
  (kill-all-local-variables)
  (setq major-mode 'lui-mode
        mode-name "LUI")
  (use-local-map lui-mode-map)
  ;; Buffer-local variables
  (setq lui-input-function input-func
        lui-input-marker (make-marker)
        lui-output-marker (make-marker)
        lui-input-ring (make-ring lui-input-ring-size)
        lui-input-ring-index nil)
  (set-marker lui-input-marker (point-max))
  (set-marker lui-output-marker (point-max))
  ;; Mode hook
  (run-hooks 'lui-mode-hook))

(defun lui-prompt (str)
  "Set STR as the prompt in the current LUI buffer."
  (save-restriction
    (widen)
    (delete-region lui-output-marker lui-input-marker)
    (goto-char lui-output-marker)
    (insert str)
    (set-marker lui-input-marker (point))
    (let ((ov (make-overlay lui-output-marker lui-input-marker)))
      (overlay-put ov 'inhibit-line-move-field-capture t)
      (overlay-put ov 'field 'lui-prompt)
      (overlay-put ov 'evaporate 'lui-prompt))))

(defun lui-insert (str)
  "Insert STR as output in the LUI buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char lui-output-marker)
      (let ((from (point))
            (to nil))
        (insert str "\n")
        (setq to (point))
        (set-marker lui-output-marker (point))
        (mapc (lambda (ov)
                (move-overlay ov lui-output-marker lui-input-marker))
              (overlays-in lui-output-marker lui-input-marker))
        (narrow-to-region from to)
        (run-hooks 'lui-insert-hook)
        (when (< lui-input-marker lui-output-marker)
          (set-marker lui-input-marker lui-output-marker)))))
  (goto-char lui-input-marker))

(defun lui-send-input ()
  "Send the current input to the LUI application."
  (interactive)
  (if (< (point) lui-input-marker)
      (self-insert-command 1)
    (let ((input (buffer-substring lui-input-marker (point-max))))
      (delete-region lui-input-marker (point-max))
      (ring-insert lui-input-ring input)
      (setq lui-input-ring-index nil)
      (funcall lui-input-function input))))

(defun lui-previous-input ()
  "Replace the current input with the previous one from the history."
  (interactive)
  (when (> (ring-length lui-input-ring) 0)
    (if (and lui-input-ring-index
             (= (1- (ring-length lui-input-ring))
                lui-input-ring-index))
        ;; last item - insert a single empty line
        (progn
          (lui-replace-input "")
          (setq lui-input-ring-index nil))
      ;; If any input is left, store it in the input ring
      (when (and (null lui-input-ring-index)
                 (> (point-max) lui-input-marker))
        (ring-insert lui-input-ring
                     (buffer-substring lui-input-marker (point-max)))
        (setq lui-input-ring-index 0))
      ;; Increment the index
      (setq lui-input-ring-index
            (if lui-input-ring-index
                (ring-plus1 lui-input-ring-index (ring-length lui-input-ring))
              0))
      ;; And insert the last input
      (lui-replace-input (ring-ref lui-input-ring lui-input-ring-index)))))

(defun lui-next-input ()
  "Replace the current input with the next one from the history."
  (interactive)
  (when (> (ring-length lui-input-ring) 0)
    (if (and lui-input-ring-index
             (= 0 lui-input-ring-index))
        (progn
          (lui-replace-input "")
          (setq lui-input-ring-index nil))
      (setq lui-input-ring-index (ring-minus1 (or lui-input-ring-index 0)
                                              (ring-length lui-input-ring)))
      (lui-replace-input (ring-ref lui-input-ring lui-input-ring-index)))))

(defun lui-replace-input (str)
  "Replace the current input with STR."
  (goto-char lui-input-marker)
  (delete-region lui-input-marker (point-max))
  (insert str))

;; Scroll-to-bottom
(defun lui-add-scroll-to-bottom ()
  "Add this to `lui-mode-hook' to recenter output at the bottom of the
window.

This works whenever scrolling happens, so it's added to
`window-scroll-functions'."
  (add-hook 'window-scroll-functions 'lui-scroll-to-bottom nil t))

(defun lui-scroll-to-bottom (window display-start)
  "Recenter WINDOW so that point is on the last line.

This is added to `window-scroll-functions' by
`lui-add-scroll-to-bottom'.

The code is shamelessly taken (but adapted) from ERC."
  (when (and window
             (window-live-p window)
             (>= (point) lui-input-marker)
    (let ((resize-mini-windows nil))
      (save-selected-window
        (select-window window)
        (save-restriction
          (widen)
          (when (>= (point) lui-input-marker)
            (save-excursion
              (recenter -1)
              (sit-for 0))))))))

(provide 'lui)
;;; lui.el ends here
