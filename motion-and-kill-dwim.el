;;; motion-and-kill-dwim.el --- Motion and kill DWIM commands

;; Copyright (C) 2007  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 01 Aug 2007
;; Version: 1.0
;; Keywords: motion kill dwim

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

;; A set of motion and kill do-what-I-mean commands.
;;
;; Here's my setup:
;;
;; (global-set-key [(meta down)] 'forward-paragraph-dwim)
;; (global-set-key [(control down)] 'forward-block-dwim)
;; (global-set-key [(control meta down)] 'up-list)
;;
;; (global-set-key [(meta up)] 'backward-paragraph-dwim)
;; (global-set-key [(control up)] 'backward-block-dwim)
;; (global-set-key [(control meta up)] 'backward-up-list)
;;
;; (global-set-key [(meta right)] 'forward-word-dwim)
;; (global-set-key [(control right)] 'forward-word-section-dwim)
;; (global-set-key [(shift right)] 'forward-word-end-dwim)
;; (global-set-key [(shift meta right)] 'forward-to-char-dwim)
;; (global-set-key [(control meta right)] 'forward-sexp)
;;
;; (global-set-key [(meta left)] 'backward-word-dwim)
;; (global-set-key [(control left)] 'backward-word-section-dwim)
;; (global-set-key [(shift left)] 'backward-word-end-dwim)
;; (global-set-key [(shift meta left)] 'backward-to-char-dwim)
;; (global-set-key [(control meta left)] 'backward-sexp)
;;
;; (global-set-key [(meta delete)] 'forward-kill-dwim)
;; (global-set-key [(control delete)] 'forward-kill-section-dwim)
;; (global-set-key [(shift meta delete)] 'kill-to-char-dwim)
;;
;; (global-set-key [(meta backspace)] 'backward-kill-dwim)
;; (global-set-key [(control backspace)] 'backward-kill-section-dwim)
;; (global-set-key [(shift meta backspace)] 'backward-kill-to-char-dwim)
;;
;; (global-set-key "\C-w" 'kill-region-dwim)
;; (global-set-key "\M-w" 'copy-region-as-kill-dwim)
;;
;;; Change log:
;;
;; 01 Aug 2007 -- v1.0
;;                Initial creation

;;; Code:

;; XEmacs and old GNU Emacs compatibility function

(unless (fboundp 'looking-back)
  (defun looking-back (regexp &optional limit greedy)
    "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying a minimum
starting position, to avoid checking matches that would start
before LIMIT.

If GREEDY is non-nil, extend the match backwards as far as possible,
stopping when a single additional previous character cannot be part
of a match for REGEXP."
    (let ((start (point))
          (pos
           (save-excursion
             (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
                  (point)))))
      (if (and greedy pos)
          (save-restriction
            (narrow-to-region (point-min) start)
            (while (and (> pos (point-min))
                        (save-excursion
                          (goto-char pos)
                          (backward-char 1)
                          (looking-at (concat "\\(?:" regexp "\\)\\'"))))
              (setq pos (1- pos)))
            (save-excursion
              (goto-char pos)
              (looking-at (concat "\\(?:" regexp "\\)\\'")))))
      (not (null pos)))))

;; Helper functions

(defun makd-mark-active ()
  "Is region active, GNU-Emacs & XEmacs."
  (if (fboundp 'region-active-p)
      (region-active-p)
    (and transient-mark-mode mark-active)))

(defun makd-dotimes (n form)
  "Optionally do a form a number of times."
  (unless n (setq n 1))
  (while (> n 0)
    (eval form)
    (setq n (1- n))))

;; Motion

(defun forward-word-dwim (&optional n)
  "Like forward-word, but stops at beginning of words.
With argument, do this that many times"
  (interactive "p")
  (setq zmacs-region-stays t)
  (makd-dotimes n '(progn (when (looking-at "\\(\\sw\\|\\s_\\)")
                            (skip-syntax-forward "w_"))
                          (skip-syntax-forward "^w_"))))

(defun forward-word-section-dwim (&optional n)
  "Like forward-word, but only goes over alphanumerics.
With argument, do this that many times"
  (interactive "p")
  (setq zmacs-region-stays t)
  (makd-dotimes n '(progn (when (looking-at "[a-zA-Z0-9]")
                            (skip-chars-forward "a-zA-Z0-9"))
                          (skip-chars-forward "^a-zA-Z0-9"))))

(defun forward-word-end-dwim (&optional n)
  "Forward to end of word.
With argument, do this that many times"
  (interactive "p")
  (setq zmacs-region-stays t)
  (makd-dotimes n '(progn (unless (looking-at "\\(\\sw\\|\\s_\\)")
                            (skip-syntax-forward "^w_"))
                          (skip-syntax-forward "w_"))))

(defun forward-to-char-dwim (n char)
  "Move forward to CHAR.
With argument, do this that many times"
  (interactive "p\ncForward to char: ")
  (unless (and n (> n 0)) (setq n 1))
  (setq zmacs-region-stays t)
  (let ((case-fold-search nil))
    (forward-char)
    (search-forward (char-to-string char) nil nil n))
  (backward-char))

(defun forward-paragraph-dwim (&optional n)
  "Like forward-paragraph, but goes to next non-blank line.
With argument, do this that many times"
  (interactive "p")
  (setq zmacs-region-stays t)
  (beginning-of-line)
  (makd-dotimes n '(progn (when (re-search-forward "^\\s-*$" nil 'go)
                            (re-search-forward "[^ \t\f\n]" nil 'go)
                            (beginning-of-line)))))

(defun forward-block-dwim (&optional n)
  "Goes forward to next line at the same or less indentation.
With argument, do this that many times"
  (interactive "p")
  (setq zmacs-region-stays t)
  (makd-dotimes n '(progn (back-to-indentation)
                          (let ((col (current-column)) done)
                            (while (not done)
                              (if (eobp)
                                  (setq done t)
                                (forward-line 1)
                                (back-to-indentation)
                                (when (and (<= (current-column) col)
                                           (not (looking-at "$")))
                                  (setq done t)
                                  (beginning-of-line))))))))

(defun backward-word-dwim (&optional n)
  "Like backward-word, but stops at beginning of words.
With argument, do this that many times"
  (interactive "p")
  (setq zmacs-region-stays t)
  (makd-dotimes n '(progn (unless (looking-back "\\(\\sw\\|\\s_\\)")
                            (skip-syntax-backward "^w_"))
                          (skip-syntax-backward "w_"))))

(defun backward-word-section-dwim (&optional n)
  "Like backward-word, but only goes over alphanumerics.
With argument, do this that many times"
  (interactive "p")
  (setq zmacs-region-stays t)
  (makd-dotimes n '(progn (when (looking-back "[^a-zA-Z0-9]")
                            (skip-chars-backward "^a-zA-Z0-9"))
                          (skip-chars-backward "a-zA-Z0-9"))))

(defun backward-word-end-dwim (&optional n)
  "Backward to end of word.
With argument, do this that many times"
  (interactive "p")
  (setq zmacs-region-stays t)
  (makd-dotimes n '(progn (when(looking-back "\\(\\sw\\|\\s_\\)")
                            (skip-syntax-backward "w_"))
                          (skip-syntax-backward "^w_"))))

(defun backward-to-char-dwim (n char)
  "Move backward to CHAR.
With argument, do this that many times"
  (interactive "p\ncBackward to char: ")
  (unless (and n (> n 0)) (setq n 1))
  (setq zmacs-region-stays t)
  (let ((case-fold-search nil))
    (search-backward (char-to-string char) nil nil n)))

(defun backward-paragraph-dwim (&optional n)
  "Go to first line after previous blank line.
With argument, do this that many times"
  (interactive "p")
  (setq zmacs-region-stays t)
  (beginning-of-line)
  (makd-dotimes n '(progn (re-search-backward "[^ \t\f\n]" nil 'go)
                          (when (re-search-backward "^\\s-*$" nil 'go)
                            (next-line 1))
                          (beginning-of-line))))

(defun backward-block-dwim (&optional n)
  "Goes backward to previous line at the same or less indentation.
With argument, do this that many times"
  (interactive "p")
  (setq zmacs-region-stays t)
  (makd-dotimes n '(progn (back-to-indentation)
                          (let ((col (current-column)) done)
                            (while (not done)
                              (if (bobp)
                                  (setq done t)
                                (forward-line -1)
                                (back-to-indentation)
                                (when (and (<= (current-column) col)
                                           (not (looking-at "$")))
                                  (setq done t)
                                  (beginning-of-line))))))))

;; Kill

(defun forward-kill-dwim (&optional n)
  "Smart kill forward.
1. If region is active, kill it
2. Else if at the beginning of a word, kill the word and trailing whitespace
3. Else if in the middle of a word, kill the rest of the word
4. Else if looking at whitespace, kill whitespace forward
5. Else if looking at punctuation, kill punctuation forward
6. Else if looking at an open bracket/brace/paren, kill sexp forward
7. Else if looking at a quotation mark, kill quoted text
8. Else kill next char
With argument, do this that many times"
  (interactive "p")
  (if (makd-mark-active)
      (kill-region (region-beginning) (region-end))
    (makd-dotimes n '(kill-region (point)
                                  (progn
                                    (cond ((looking-at "\\<\\(\\sw\\|\\s_\\)")
                                           (skip-syntax-forward "w_")
                                           (skip-syntax-forward " "))
                                          ((looking-at "\\(\\sw\\|\\s_\\)")
                                           (skip-syntax-forward "w_"))
                                          ((looking-at "\\s ")
                                           (skip-syntax-forward " "))
                                          ((looking-at "\\s.")
                                           (skip-syntax-forward "."))
                                          ((looking-at "\\s(")
                                           (forward-sexp))
                                          ((looking-at "\\s\"")
                                           (forward-char)
                                           (skip-syntax-forward "^\"")
                                           (forward-char))
                                          (t
                                           (forward-char)))
                                    (point))))))

(defun forward-kill-section-dwim (&optional n)
  "Forward kill pieces of words.
1. If looking at a alphanumeric, kill alphas and trailing underscores
2. Else if looking at an underscore, kill underscores and trailing alphas
3. Else kill next char
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(kill-region (point)
                                (progn
                                  (cond ((looking-at "[a-zA-Z0-9]")
                                         (skip-chars-forward "a-zA-Z0-9")
                                         (skip-chars-forward "_"))
                                        ((looking-at "_")
                                         (skip-chars-forward "_")
                                         (skip-chars-forward "a-zA-Z0-9"))
                                        (t
                                         (forward-char)))
                                  (point)))))

(defun backward-kill-dwim (&optional n)
  "Smart kill backward.
1. If region is active, kill it
2. Else if looking back at whitespace, kill backward whitespace and word
3. Else if at the end of a word, kill backward word and whitespace
   (unless there is only whitespace left on the line)
4. Else if in the middle of a word, kill backward word
5. Else if looking at punctuation, kill backward punctuation
6. Else if looking at an close bracket/brace/paren, kill backward sexp
7. Else if looking at a quotation mark, kill backward quoted text
8. Else kill previous char
With argument, do this that many times"
  (interactive "p")
  (if (makd-mark-active)
      (kill-region (region-beginning) (region-end))
    (makd-dotimes n '(kill-region (point)
                                  (progn
                                    (cond ((looking-back "\\s ")
                                           (skip-syntax-backward " ")
                                           (when (looking-back "\\(\\sw\\|\\s_\\)")
                                             (skip-syntax-backward "w_")))
                                          ((looking-back "\\(\\sw\\|\\s_\\)\\>")
                                           (skip-syntax-backward "w_")
                                           (unless (looking-back "^\\s +")
                                             (skip-syntax-backward " ")))
                                          ((looking-back "\\(\\sw\\|\\s_\\)")
                                           (skip-syntax-backward "w_"))
                                          ((looking-back "\\s.")
                                           (skip-syntax-backward "."))
                                          ((looking-back "\\s)")
                                           (backward-sexp))
                                          ((looking-back "\\s\"")
                                           (backward-char)
                                           (skip-syntax-backward "^\"")
                                           (backward-char))
                                          (t
                                           (backward-char)))
                                    (point))))))

(defun backward-kill-section-dwim (&optional n)
  "Backward kill pieces of words.
1. If looking at an underscore, kill backward underscores and alphanumerics
2. Else if looking at a alphanumeric, kill backward alphas and underscores
3. Else kill previous char
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(kill-region (point)
                                (progn
                                  (cond ((looking-back "_")
                                         (skip-chars-backward "_")
                                         (skip-chars-backward "a-zA-Z0-9"))
                                        ((looking-back "[a-zA-Z0-9]")
                                         (skip-chars-backward "a-zA-Z0-9")
                                         (skip-chars-backward "_"))
                                        (t
                                         (backward-char)))
                                  (point)))))

;; Kill to char

(defun kill-to-char-dwim (n char)
  "Kill up to CHAR.
With argument, do this that many times"
  (interactive "p\ncKill to char: ")
  (unless (and n (> n 0)) (setq n 1))
  (let ((case-fold-search nil))
    (kill-region (point) (progn
                           (forward-char)
                           (search-forward (char-to-string char) nil nil n)
                           (backward-char)
                           (point)))))

(defun backward-kill-to-char-dwim (n char)
  "Kill backwards up to CHAR.
With argument, do this that many times"
  (interactive "p\ncBackward kill to char: ")
  (unless (and n (> n 0)) (setq n 1))
  (let ((case-fold-search nil))
    (kill-region (point) (progn
                           (search-backward (char-to-string char) nil nil n)
                           (forward-char)
                           (point)))))

;; Copy/kill region

(defun copy-region-as-kill-dwim ()
  "When called interactively with no active region, copy current line."
  (interactive)
  (let ((beg (if (makd-mark-active) (region-beginning) (point-at-bol)))
        (end (if (makd-mark-active) (region-end) (point-at-bol 2))))
    (copy-region-as-kill beg end)))

(defun kill-region-dwim ()
  "When called interactively with no active region, kill current line."
  (interactive)
  (let ((beg (if (makd-mark-active) (region-beginning) (point-at-bol)))
        (end (if (makd-mark-active) (region-end) (point-at-bol 2))))
    (kill-region beg end)))

(provide 'motion-and-kill-dwim)
;;; motion-and-kill-dwim.el ends here
