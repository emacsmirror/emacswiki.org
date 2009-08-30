;;; seinquote.el --- Seinfeld Quotes
;;
;; Filename: seinquote.el
;; Description: 
;; Author: Eli Segal
;; Maintainer: Eli Segal <eli.segal@gmail.com>
;; Copyright (C) 2007, Eli Segal, all rights reserved.
;; Version: 0.1
;; URL: http://www.emacswiki.org/cgi-bin/emacs-en/RockEm
;; Keywords: seinfeld quotes
;; Compatibility: GNU Emacs 22.x
;;
;;; Commentary:
;;
;; Show you a rnadom quote from the seinfeld TV show.
;; All the quote were fetch from http://www.pkmeco.com/seinfeld/
;;
;;  to use you need to add the following line to your init file:
;;  (require 'seinquote)
;;
;;  To activate it just run:
;;  M-x seinquote
;;
;;
;; While in the seinquote window you have the following options:
;; q - Quit.
;; r - Reload another random quote.
;;
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

(defvar seinquote-window-old-config nil
  "Window configuration before starting seinquote.")
(make-variable-frame-local 'seinquote-window-old-config)

(defvar seinquote-mode-font-lock
  (list
   '("^\\( -.*\\)$"  1 font-lock-constant-face))
  "Default font lock expressions.")

(defvar seinquote-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'seinquote-kill)
    (define-key map "r" 'seinquote-display-quote)
   map)
  "Keymap of `seinquote-mode'.")

(defun seinquote-restore-window-config()
"Restore window configuration on the current frame."
  (when seinquote-window-old-config
    (set-window-configuration seinquote-window-old-config)
    (setq seinquote-window-old-config nil)))

(defun seinquote-kill()
  "Bury buffer and return window's order to its old state."
  (interactive)
  (bury-buffer (current-buffer))
  (seinquote-restore-window-config))

(defun seinquote-mode()
  (interactive)
  (kill-all-local-variables)
  (use-local-map seinquote-mode-map)
  (setq major-mode 'seinqoute-mode
	mode-name "SeinQuote"
	buffer-read-only t
	font-lock-defaults '(seinquote-mode-font-lock t))
  (run-mode-hooks 'seinquote-mode-hook))

(defun seinquote-get-random-quote() 
  "Load the file and return a random quote from it."
  (with-temp-buffer 
    (insert-file-contents "seinquotes.txt")
    (goto-char (point-min))
    (setq n (random 1399))
    (while (> n 0)
      (setq n (- n 1))
      (search-forward "---"))
    (next-line)
    (setq q-begin (line-beginning-position))
    (search-forward "---")
    (previous-line)
    (setq q-end (line-end-position))
    (setq quote (buffer-substring q-begin q-end))))

(defun seinquote-display-quote() 
  "Display the random quote in the current buffer."
  (interactive)
  (let* ((inhibit-read-only t))
    (erase-buffer)
    (insert (seinquote-get-random-quote))
    (fit-window-to-buffer)))

(defun seinquote()
  (interactive)
  (unless (string= "*Seinfeld Quote*" (buffer-name))
    (setq seinquote-window-old-config (current-window-configuration))
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer (get-buffer-create "*Seinfeld Quote*"))
    (seinquote-mode)
    (seinquote-display-quote)))

(provide 'seinquote)      

