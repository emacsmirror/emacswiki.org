;;; smart-dabbrev.el --- smarter dabbrev-expand

;; Copyright (C) 2008  Tamas Patrovics

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; If there were successful expansions in the current buffer then
;; offer those first, instead of starting all over again.

;;; Code:


(require 'dabbrev)


(defvar sda-completions nil
  "List of completions for the current buffer found by dabbrev.")

(make-variable-buffer-local 'sda-completions)


(defvar sda-current-completion nil
  "The currently used completion from `sda-completions'.")

(defvar sda-completion-prefix nil
  "The currently used completion prefix.")

(defvar sda-completion-end nil
  "The end position of completion prefix.")

(defvar sda-dabbrev-skipped nil
  "Indicates we're skipping standard dabbrev behavior.")


;; make sure dabbrev--goto-start-of-abbrev can run
(dabbrev--reset-global-variables)


(defadvice dabbrev--reset-global-variables
  (around sda-dabbrev--reset-global-variables activate)
  "We control the intial value of `dabbrev--last-table' for
  `dabbrev-expand'."
  ;; prevent overwriting of the global value
  (let ((dabbrev--last-table dabbrev--last-table))
    ad-do-it))
  

(defun sda-find-completion ()
  (while (and sda-current-completion
              (or (>= (length sda-completion-prefix)
                      (length (car sda-current-completion)))
                  (not (equal (substring (car sda-current-completion)
                                         0 (length sda-completion-prefix))
                              sda-completion-prefix))))
    (setq sda-current-completion (cdr sda-current-completion)))

  (when sda-current-completion
    (insert (substring (car sda-current-completion)
                       (length sda-completion-prefix)))
    (setq sda-completions (delete (car sda-current-completion)
                                  sda-completions))
    (push (car sda-current-completion) sda-completions)
    (push (car sda-current-completion) dabbrev--last-table)
    (message "Found previously used completion.")
    (setq sda-current-completion (cdr sda-current-completion))
    t))


(defun sda-record-completion-from-dabbrev ()
  (when (and dabbrev--last-expansion
             ;; let space expansion alone
             (not space-expansion))
    (setq sda-completions (delete dabbrev--last-expansion
                                  sda-completions))
    (push dabbrev--last-expansion sda-completions)))


(defadvice dabbrev-expand (around sda-dabbrev-expand activate)
  (let ((space-expansion (eq (preceding-char) ?\s)))
    (if (eq last-command this-command)
        (if sda-dabbrev-skipped
            (progn
              (delete-region sda-completion-end (point))
              (unless (sda-find-completion)
                (setq sda-dabbrev-skipped nil)
                ;; we have to fool dabbrev, so that it thinks it's the first
                ;; working on the problem
                (setq last-command 'dummy)
                ad-do-it
                (sda-record-completion-from-dabbrev)))

          ad-do-it
          (sda-record-completion-from-dabbrev))
  
      (setq sda-completion-end (point))
      (setq sda-completion-prefix (buffer-substring-no-properties
                                   (save-excursion
                                     (dabbrev--goto-start-of-abbrev)
                                     (point))
                                   (point)))

      (setq sda-current-completion sda-completions)
      (setq sda-dabbrev-skipped t)
      ;; we also manipulate it, so keep it clean
      (setq dabbrev--last-table nil)

      (unless (and (not space-expansion)
                   (sda-find-completion))
        (setq sda-dabbrev-skipped nil)
        ad-do-it
        (sda-record-completion-from-dabbrev)))))


(provide 'smart-dabbrev)
;;; smart-dabbrev.el ends here
