;;; auto-indent.el -- automatically indent source code

;; Copyright (C) 2009,2010,2011 Caleb Reach

;; Version: 0.2
;; Author:  Caleb Reach <jtxx000 [at] gmail.com>
;; URL: http://codphilosophy.com/software/auto-indent.el
;;      http://www.emacswiki.org/emacs/auto-indent.el

;; This file is *NOT* part of GNU Emacs.

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

;;; Commentary:

;; auto-indent-mode keeps source code indented automatically.  When
;; enabled, various editing commands are rebound to variants that
;; treat a newline along with any following indentation as a single
;; unit and make sure that the current line is correctly indented.
;; If, for example, you have
;;
;;    (defun foo ()|)                                              [1]
;;
;; (where | is the cursor) and press <return>, you get
;;
;;    (defun foo ()                                                [2]
;;      |)
;;
;; If you then press <backspace>, you get [1] again.  C-o and C-d
;; behave in the same manner.
;;
;; In the same way that moving up and down lines with the point at the
;; end of the line warps the point at the last character of the line,
;; moving up and down lines with auto-indent-mode enabled and the
;; point at the beginning of the line warps the point to the
;; first non-whitespace character.
;;
;; Yanks are also automatically indented.

;; If you don't like this mode, check out this unrelated
;; implementation of the same concept:
;;   http://www.emacswiki.org/emacs/auto-indent-mode.el

(defvar auto-indent/delete-char-function 'delete-char)
(make-variable-buffer-local 'auto-indent/delete-char-function)
(defvar auto-indent/backward-delete-char-function 'backward-delete-char-untabify)
(make-variable-buffer-local 'auto-indent/backward-delete-char-function)

(defun auto-indent/indent-if-needed ()
  (unless (auto-indent/is-whitespace (line-beginning-position) (line-end-position))
    (indent-according-to-mode)))

(defun auto-indent/point-at-beginning-of-line-text ()
  (<= (point)
      (save-excursion
        (back-to-indentation)
        (point))))

(defun auto-indent/backward-char ()
  (interactive)
  (when (auto-indent/point-at-beginning-of-line-text)
    (beginning-of-line))
  (backward-char))

(defun auto-indent/delete-char ()
  (interactive)
  (if (= (point) (line-end-position))
      (progn (delete-indentation t)
             (save-restriction
               (narrow-to-region (point-at-bol) (point-at-eol))
               (delete-trailing-whitespace))
             (indent-according-to-mode))
    (apply auto-indent/delete-char-function '(1))))

(defun auto-indent/backward-delete-char ()
  (interactive)
  (if (auto-indent/point-at-beginning-of-line-text)
      (progn (delete-indentation)
             (save-restriction
               (narrow-to-region (point-at-bol) (point-at-eol))
               (delete-trailing-whitespace))
             (indent-according-to-mode))
    (apply auto-indent/backward-delete-char-function '(1))))

(defun auto-indent/open-line ()
  (interactive)
  (save-excursion
    (newline)
    (auto-indent/indent-if-needed)))

(defun auto-indent/yank ()
  (interactive)
  (setq this-command 'yank)
  (yank)
  (indent-region (mark t) (point)))

(defun auto-indent/yank-pop ()
  (interactive)
  (setq this-command 'yank)
  (yank-pop)
  (indent-region (mark t) (point)))

(defvar auto-indent/last-line)
(make-variable-buffer-local 'auto-indent/last-line)

(defvar auto-indent/line-change-hook '())

(defun auto-indent/pre-command ()
  (set-marker auto-indent/last-line (line-beginning-position)))

(defun auto-indent/is-whitespace (x y &optional no-empty)
  (string-match
   (concat "^[ \t]"
           (if no-empty "+" "*")
           "$")
   (buffer-substring x y)))

(defun auto-indent/post-command ()
  (unless undo-in-progress
    (let ((undo-list buffer-undo-list)
          (in-undo-block (car buffer-undo-list)))
      (let ((mod (buffer-modified-p))
            (buffer-undo-list (if in-undo-block
                                  buffer-undo-list
                                (cdr buffer-undo-list)))
            (inhibit-read-only t)
            (inhibit-point-motion-hooks t)
            before-change-functions
            after-change-functions
            deactivate-mark
            buffer-file-name
            buffer-file-truename)
        (when (and (/= auto-indent/last-line (line-beginning-position))
                   (not (memq this-command '(undo redo))))
          (save-excursion
            (goto-char auto-indent/last-line)
            (if (auto-indent/is-whitespace (point) (line-end-position) t)
                (delete-region (point) (line-end-position))))
          (if (auto-indent/is-whitespace (line-beginning-position) (point))
              (if (auto-indent/is-whitespace (line-beginning-position) (line-end-position))
                  (indent-according-to-mode)
                (back-to-indentation)))
          (run-hooks 'auto-indent/line-change-hook))
        (and (not mod)
             (buffer-modified-p)
             (set-buffer-modified-p nil))
        (setq undo-list (if in-undo-block
                            buffer-undo-list
                          (cons nil buffer-undo-list))))
      (setq buffer-undo-list undo-list))))

(define-minor-mode auto-indent-mode
  "Minor mode for automatically indenting source code"
  :keymap
  '(([left]      . auto-indent/backward-char)
    ("\C-b"      . auto-indent/backward-char)
    ([return]    . newline-and-indent)
    ([delete]    . auto-indent/delete-char)
    ("\C-d"      . auto-indent/delete-char)
    ([backspace] . auto-indent/backward-delete-char)
    ([home]      . back-to-indentation)
    ("\C-o"      . auto-indent/open-line)
    ("\C-y"      . auto-indent/yank)
    ("\M-y"      . auto-indent/yank-pop))
  (if auto-indent-mode
      (progn
        (setq auto-indent/last-line (make-marker))
        (add-hook 'pre-command-hook 'auto-indent/pre-command nil t)
        (add-hook 'post-command-hook 'auto-indent/post-command nil t)
        (auto-indent/pre-command))
    (remove-hook 'pre-command-hook 'auto-indent/pre-command t)
    (remove-hook 'post-command-hook 'auto-indent/post-command t)))

(provide 'auto-indent)
