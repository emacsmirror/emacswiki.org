;;; duck.el --- A few utilities for Scheme programming

;; Copyright (C) 2005  Jorgen Schaefer

;; Version: 1.0
;; Keywords: Scheme, programming
;; Author: Jorgen Schaefer
;; URL: http://www.emacswiki.org/elisp/duck.el

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

;; This is a collection of simple utility functions for Scheme
;; programming. It was born when the present author noticed that he
;; used almost none of the functions offered in quack.el by Neil W.
;; Van Dyke.

;;; Code:

;; This needs to be loaded, as we overwrite their `run-scheme'
(require 'cmuscheme)

(defcustom duck-scheme-commands '("scheme48" "scsh")
  "*The possible commands to run as Scheme interpreters.
This is used by `run-scheme' for the completion."
  :type 'string
  :group 'scheme)

(defun run-scheme (&optional cmd)
  "Run an inferior Scheme process, input and output via buffer *scheme*.
If there is a process already running in *scheme*, switch to that buffer.
If no process is running, ask for a Scheme interpreter to run.
Allow completion on `duck-scheme-commands'.
Runs the hooks `inferior-scheme-mode-hook' (after the
`comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive)
  (let ((cmd (when (not (comint-check-proc "*scheme*"))
               (or cmd
                   (duck-completing-read (format "Run Scheme (default %S): "
                                                 (car duck-scheme-commands))
                                         duck-scheme-commands
                                         (car duck-scheme-commands))))))
    (when cmd
      (let ((cmdlist (scheme-args-to-list cmd)))
        (set-buffer (apply 'make-comint "scheme" (car cmdlist)
                           nil (cdr cmdlist)))
        (inferior-scheme-mode)))
    (setq scheme-program-name cmd)
    (setq scheme-buffer "*scheme*")
    (pop-to-buffer "*scheme*")))

(defun duck-tidy-buffer ()
  "Tidy up the code in the current buffer."
  (interactive)
  (save-excursion
    ;; Remove multiple empty lines
    (goto-char (point-min))
    (while (re-search-forward "\n\n\\(\n+\\)" nil t)
      (replace-match "\n\n"))
    ;; Remove trailing whitespace
    (delete-trailing-whitespace)
    ;; Remove blank lines at top ...
    (goto-char (point-min))
    (while (looking-at "\n")
      (delete-char 1))
    ;; ... and bottom
    (goto-char (1- (point-max)))
    (while (looking-at "\n")
      (delete-char 1)
      (backward-char))
    ;; Ensure last char being newline
    (goto-char (point-max))
    (when (not (looking-at "\n"))
      (insert "\n"))
    ;; Reindent buffer
    (indent-region (point-min) (point-max))
    ;; Expand tabs
    (untabify (point-min) (point-max))))

(defun duck-completing-read (prompt table default)
  "Like `completing-read', only allow spaces in the input."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map
                       minibuffer-local-completion-map)
    (define-key map (kbd "<SPC>") 'self-insert-command)
    (let ((minibuffer-local-completion-map map))
      (completing-read prompt table nil nil nil nil default))))

(provide 'duck)
;;; duck.el ends here
