;;; doit-mode.el --- A major mode for organizing tasks.

;; Copyright (C) 2004, 2005, 2006, 2007 Yoni Rabkin
;; <yoni-r@actcom.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; `doit-mode' is how I organize my daily tasks evolved into an Emacs
;; major mode.  The word 'doit' has no relation to the Dutch coin.

;;; Installation:

;; (add-to-list 'load-path "/WHERE/IT/IS/AT/")
;; (require 'doit-mode)

;;; Code:

(defface doit-mode-active-face
  '((((class color) (background dark))
     (:foreground "firebrick"))
    (((class color) (background light))
     (:foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "black")))
  "Face for active tasks."
  :group 'doit-mode)

(defface doit-mode-inactive-face
  '((((class color) (background dark))
     (:foreground "honeydew4"))
    (((class color) (background light))
     (:foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "black")))
  "Face for inactive tasks."
  :group 'doit-mode)

(defface doit-mode-inactive-bullet-face
  '((((class color) (background dark))
     (:foreground "honeydew3"))
    (((class color) (background light))
     (:foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "black")))
  "Face for inactive bullet points."
  :group 'doit-mode)

(defconst doit-mode-font-lock-keywords
  '(("\\(;+.*\\)$" (1 'font-lock-comment-face))
    ("^\\(*)\\)" (1 'doit-mode-active-face))
    ("^\\(-)\\)\\(.*\\)$"
     (1 'doit-mode-inactive-bullet-face)
     (2 'doit-mode-inactive-face)))
  "Basic font locking rules used in `doit-mode'.")

(defvar doit-mode-hook 
  nil
  "Major mode hook for `doit-mode'.")

(defvar doit-mode-map
  (let ((doit-mode-map (make-sparse-keymap)))
    (set-keymap-parent doit-mode-map text-mode-map)
    (define-key doit-mode-map "\C-ca" 'doit-mode-activate)
    (define-key doit-mode-map "\C-cd" 'doit-mode-inactivate)
    (define-key doit-mode-map "\C-cy" 'doit-mode-insert-header)
    doit-mode-map)
  "Keymap used in `doit-mode'.")

(defvar doit-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    ;; place modifications here
    st)
  "Syntax table used while in `doit-mode'.")

(add-to-list 'auto-mode-alist '("\\.doit\\'" . doit-mode))

(defun doit-mode-activate ()
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (when (looking-at "\-\)")
      (delete-char 1)
      (insert "*"))))

(defun doit-mode-inactivate ()
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (when (looking-at "\*\)")
      (delete-char 1)
      (insert "-")
      (kill-region (point-at-bol) (point-at-eol))
      (delete-char 1)
      (goto-char (point-max))
      (yank)
      (insert (message " %s\n" (current-time-string))))))

(defun doit-mode-insert-header (str)
  (interactive "sheader text: ")
  (let ((banner ";;; --------------------------------------------"))
    (insert (concat banner "\n;;; " str "\n" banner "\n\n"))))

;; nice idea, probably best to implement a minor mode for
;; calendar-mode to do this.
;;
;; (defun doit-mode-insert-date ()
;;   (interactive)
;;   (add-to-list 'calendar-mode-hook 'doit-mode-calendar-hook-function))

(defun doit-mode ()
  "Major mode for editing Doit files.
Special commands: \\{doit-mode-map}
Turning on doit-mode runs the hook `doit-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map doit-mode-map)
  (set-syntax-table doit-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'font-lock-defaults)
  (setq indent-line-function 'indent-relative-maybe)
  (setq mode-name "Doit")
  (setq major-mode 'doit-mode)
  (setq font-lock-defaults '(doit-mode-font-lock-keywords))
  (run-mode-hooks 'doit-mode-hook))

(provide 'doit-mode)

;;; doit-mode.el ends here
