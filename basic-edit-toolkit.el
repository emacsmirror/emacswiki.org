;;; basic-edit-toolkit.el --- Basic edit toolkit.

;; Filename: basic-edit-toolkit.el
;; Description: Basic edit toolkit.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-07 20:56:08
;; Version: 0.1
;; Last-Updated: 2009-02-07 20:56:08
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/basic-edit-toolkit.el
;; Keywords: edit, toolkit
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Basic edit toolkit.
;;
;; This is my basic edit toolkit that separate from `lazycat-toolkit'.
;;

;;; Installation:
;;
;; Put basic-edit-toolkit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'basic-edit-toolkit)
;;
;; No need more.

;;; Change log:
;;
;; 2009/02/07
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defun open-newline-above (arg)
  "Move to the previous line (like vi) and then opens a line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))

(defun open-newline-below (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (call-interactively 'next-line arg)
  (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))

(defun insert-line-number (beg end &optional start-line)
  "Insert line numbers into buffer."
  (interactive "r")
  (save-excursion
    (let ((max (count-lines beg end))
          (line (or start-line 1))
          (counter 1))
      (goto-char beg)
      (while (<= counter max)
        (insert (format "%0d " line))
        (beginning-of-line 2)
        (incf line)
        (incf counter)))))

(defun insert-line-number+ ()
  "Insert line number into buffer."
  (interactive)
  (if mark-active
      (insert-line-number (region-beginning) (region-end) (read-number "Start line: "))
    (insert-line-number (point-min) (point-max))))

(defun strip-blank-lines()
  "Strip all blank lines in select area of buffer,
if not select any area, then strip all blank lines of buffer."
  (interactive)
  (strip-regular-expression-string "^[ \t]*\n")
  (message "Have strip blanks line. ^_^"))

(defun strip-line-number()
  "Strip all line number in select area of buffer,
if not select any area, then strip all line number of buffer."
  (interactive)
  (strip-regular-expression-string "^[0-9]+")
  (message "Have strip line number. ^_^"))

(defun strip-regular-expression-string (regular-expression)
  "Strip all string that match REGULAR-EXPRESSION in select area of buffer.
If not select any area, then strip current buffer"
  (interactive)
  (let ((begin (point-min))             ;initialization make select all buffer
        (end (point-max)))
    (if mark-active                     ;if have select some area of buffer, then strip this area
        (setq begin (region-beginning)
              end (region-end)))
    (save-excursion                                              ;save position
      (goto-char end)                                            ;goto end position
      (while (and (> (point) begin)                              ;when above beginning position
                  (re-search-backward regular-expression nil t)) ;and find string that match regular expression
        (replace-match "" t t)))))                               ;replace target string with null

(defun comment-part-move-up (n)
  "Move comment part up."
  (interactive "p")
  (comment-part-move (or (- n) -1)))

(defun comment-part-move-down (n)
  "Move comment part down."
  (interactive "p")
  (comment-part-move (or n 1)))

(defun comment-part-move (&optional n)
  "Move comment part."
  (or n (setq n 1))
  (let (cmt-current cmt-another cs-current cs-another)
    ;; If current line have comment, paste it.
    (setq cmt-current (comment-paste))
    (when cmt-current
      (setq cs-current (current-column)))
    ;; If another line have comment, paste it.
    (forward-line n)
    (setq cmt-another (comment-paste))
    (when cmt-another
      (setq cs-another (current-column)))
    ;; Paste another comment in current line.
    (forward-line (- n))
    (when cmt-another
      (if cs-current
          (move-to-column cs-current t)
        (end-of-line))
      (insert cmt-another))
    ;; Paste current comment in another line.
    (forward-line n)
    (when cmt-current
      (if cs-another
          (move-to-column cs-another t)
        (end-of-line))
      (insert cmt-current))
    ;; Indent comment, from up to down.
    (if (> n 0)
        (progn                          ;comment move down
          (forward-line (- n))
          (if cmt-another (comment-indent))
          (forward-line n)
          (if cmt-current (comment-indent)))
      (if cmt-current (comment-indent)) ;comment move up
      (save-excursion
        (forward-line (- n))
        (if cmt-another (comment-indent))))))

(defun comment-copy (arg)
  "Copy the first comment on this line, if any.
With prefix ARG, copy comments on that many lines starting with this one."
  (interactive "P")
  (comment-normalize-vars)
  (dotimes (_ (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
        (when cs
          (goto-char cs)
          (skip-syntax-backward " ")
          (setq cs (point))
          (comment-forward)
          (kill-ring-save cs (if (bolp) (1- (point)) (point)))
          (indent-according-to-mode))))
    (if arg (forward-line 1))))

(defun comment-paste ()
  "Paste comment part of current line.
If have return comment, otherwise return nil."
  (let (cs ce cmt)
    (setq cs (comment-on-line-p))
    (if cs                              ;If have comment start position
        (progn
          (goto-char cs)
          (skip-syntax-backward " ")
          (setq cs (point))
          (comment-forward)
          (setq ce (if (bolp) (1- (point)) (point))) ;get comment end position
          (setq cmt (buffer-substring cs ce))        ;get comment
          (kill-region cs ce)                        ;kill region between comment start and end
          (goto-char cs)                             ;revert position
          cmt)
      nil)))

(defun comment-on-line-p ()
  "Whether have comment part on current line.
If have comment return COMMENT-START, otherwise return nil."
  (save-excursion
    (beginning-of-line)
    (comment-search-forward (line-end-position) t)))

(defun comment-dwim-next-line (&optional reversed)
  "Move to next line and comment dwim.
Optional argument REVERSED default is move next line, if reversed is non-nil move previous line."
  (interactive)
  (if reversed
      (call-interactively 'previous-line)
    (call-interactively 'next-line))
  (call-interactively 'comment-dwim))

(defun comment-dwim-prev-line ()
  "Move to previous line and comment dwim."
  (interactive)
  (comment-dwim-next-line 't))

(defun indent-buffer ()
  "Automatic format current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

(defun indent-comment-buffer ()
  "Indent comment of buffer."
  (interactive)
  (indent-comment-region (point-min) (point-max)))

(defun indent-comment-region (start end)
  "Indent region."
  (interactive "r")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (while (< (point) end)
      (if (comment-search-forward end t)
          (comment-indent)
        (goto-char end)))))

(defun move-text-internal (arg)
  "Move region (transient-mark-mode active) or current line."
  (let ((remember-point (point)))
    ;; Can't get correct effect of `transpose-lines'
    ;; when `point-max' is not at beginning of line
    ;; So fix this bug.
    (goto-char (point-max))
    (if (not (bolp)) (newline))         ;add newline to fix
    (goto-char remember-point)
    ;; logic code start
    (cond ((and mark-active transient-mark-mode)
           (if (> (point) (mark))
               (exchange-point-and-mark))
           (let ((column (current-column))
                 (text (delete-and-extract-region (point) (mark))))
             (forward-line arg)
             (move-to-column column t)
             (set-mark (point))
             (insert text)
             (exchange-point-and-mark)
             (setq deactivate-mark nil)))
          (t
           (let ((column (current-column)))
             (beginning-of-line)
             (when (or (> arg 0) (not (bobp)))
               (forward-line 1)
               (when (or (< arg 0) (not (eobp)))
                 (transpose-lines arg))
               (forward-line -1))
             (move-to-column column t))
           ))))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun move-text-down (arg)
  "Move region (transient-mar-mode active) or current line (ARG lines) down."
  (interactive "*p")
  (move-text-internal arg))

(defun duplicate-line-or-region-above (&optional reverse)
  "Duplicate current line or region above.
By default, duplicate current line above.
If mark is activate, duplicate region lines above.
Default duplicate above, unless option REVERSE is non-nil."
  (interactive)
  (let ((origianl-column (current-column))
        duplicate-content)
    (if mark-active
        ;; If mark active.
        (let ((region-start-pos (region-beginning))
              (region-end-pos (region-end)))
          ;; Set duplicate start line position.
          (setq region-start-pos (progn
                                   (goto-char region-start-pos)
                                   (line-beginning-position)))
          ;; Set duplicate end line position.
          (setq region-end-pos (progn
                                 (goto-char region-end-pos)
                                 (line-end-position)))
          ;; Get duplicate content.
          (setq duplicate-content (buffer-substring region-start-pos region-end-pos))
          (if reverse
              ;; Go to next line after duplicate end position.
              (progn
                (goto-char region-end-pos)
                (forward-line +1))
            ;; Otherwise go to duplicate start position.
            (goto-char region-start-pos)))
      ;; Otherwise set duplicate content equal current line.
      (setq duplicate-content (buffer-substring
                               (line-beginning-position)
                               (line-end-position)))
      ;; Just move next line when `reverse' is non-nil.
      (and reverse (forward-line 1))
      ;; Move to beginning of line.
      (beginning-of-line))
    ;; Open one line.
    (open-line 1)
    ;; Insert duplicate content and revert column.
    (insert duplicate-content)
    (move-to-column origianl-column t)))

(defun duplicate-line-or-region-below ()
  "Duplicate current line or region below.
By default, duplicate current line below.
If mark is activate, duplicate region lines below."
  (interactive)
  (duplicate-line-or-region-above t))

(defun duplicate-line-above-comment (&optional reverse)
  "Duplicate current line above, and comment current line."
  (interactive)
  (if reverse
      (duplicate-line-or-region-below)
    (duplicate-line-or-region-above))
  (save-excursion
    (if reverse
        (forward-line -1)
      (forward-line +1))
    (comment-or-uncomment-region+)))

(defun duplicate-line-below-comment ()
  "Duplicate current line below, and comment current line."
  (interactive)
  (duplicate-line-above-comment t))

(defun comment-or-uncomment-region+ ()
  "This function is to comment or uncomment a line or a region."
  (interactive)
  (let (beg end)
    (if mark-active
        (progn
          (setq beg (region-beginning))
          (setq end (region-end)))
      (setq beg (line-beginning-position))
      (setq end (line-end-position)))
    (save-excursion
      (comment-or-uncomment-region beg end))))

(defun upcase-char (arg)
  "Uppercase for character."
  (interactive "P")
  (upcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

(defun downcase-char (arg)
  "Downcase for character."
  (interactive "P")
  (downcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

(defun kill-syntax-forward (&optional arg)
  "Kill ARG set of syntax characters after point."
  (interactive "p")
  (let ((arg (or arg 1))
        (inc (if (and arg (< arg 0)) 1 -1))
        (opoint (point)))
    (while (or                          ;(not (= arg 0)) ;; This condition is implied.
            (and (> arg 0) (not (eobp)))
            (and (< arg 0) (not (bobp))))
      (if (> arg 0)
          (skip-syntax-forward (string (char-syntax (char-after))))
        (skip-syntax-backward (string (char-syntax (char-before)))))
      (setq arg (+ arg inc)))
    (if (and (> arg 0) (eobp))
        (message "End of buffer"))
    (if (and (< arg 0) (bobp))
        (message "Beginning of buffer"))
    (kill-region opoint (point))))

(defun kill-syntax-backward (&optional arg)
  "Kill ARG set of syntax characters preceding point."
  (interactive "p")
  (kill-syntax-forward (- 0 (or arg 1))))

(defun mark-line ()
  "Mark one whole line, similar to `mark-paragraph'."
  (interactive)
  (beginning-of-line)
  (if mark-active
      (exchange-point-and-mark)
    (push-mark nil nil t))
  (forward-line)
  (exchange-point-and-mark))

(defun kill-and-join-forward (&optional arg)
  "Delete empty line in select region."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn
        (forward-char 1)
        (just-one-space 0)
        (backward-char 1)
        (kill-line arg))
    (kill-line arg)))

(defun delete-chars-hungry-forward (&optional reverse)
  "Delete chars forward use `hungry' style.
Optional argument REVERSE default is delete forward, if reverse is non-nil delete backward."
  (delete-region
   (point)
   (progn
     (if reverse
         (skip-chars-backward " \t\n\r")
       (skip-chars-forward " \t\n\r"))
     (point))))

(defun delete-chars-hungry-backward ()
  "Delete chars backward use `hungry' style."
  (delete-chars-hungry-forward t))

(defun reverse-chars-in-region (start end)
  "Reverse the region character by character without reversing lines."
  (interactive "r")
  (let ((str (buffer-substring start end)))
    (delete-region start end)
    (dolist (line (split-string str "\n"))
      (let ((chars (mapcar (lambda (c)
                             (or (matching-paren c)
                                 c))
                           (reverse (append line nil)))))
        (when chars
          (apply 'insert chars))
        (newline)))))

(defun underline-line-with (char)
  "Insert some char below at current line."
  (interactive "cType one char: ")
  (save-excursion
    (let ((length (- (point-at-eol) (point-at-bol))))
      (end-of-line)
      (insert "\n")
      (insert (make-string length char)))))

(defun prettyfy-string (string &optional after)
  "Strip starting and ending whitespace and pretty `STRING'.
Replace any chars after AFTER with '...'.
Argument STRING the string that need pretty."
  (let ((replace-map (list
                      (cons "^[ \t]*" "")
                      (cons "[ \t]*$" "")
                      (cons (concat "^\\(.\\{"
                                    (or (number-to-string after) "10")
                                    "\\}\\).*")
                            "\\1..."))))
    (dolist (replace replace-map)
      (when (string-match (car replace) string)
        (setq string (replace-match (cdr replace) nil nil string))))
    string))

(defun forward-button-with-line-begin ()
  "Move to next button with line begin."
  (interactive)
  (call-interactively 'forward-button)
  (while (not (bolp))
    (call-interactively 'forward-button)))

(defun backward-button-with-line-begin ()
  "Move to previous button with line begin."
  (interactive)
  (call-interactively 'backward-button)
  (while (not (bolp))
    (call-interactively 'backward-button)))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(\\|\\s\{\\|\\s\[")
         (forward-list))
        ((looking-back "\\s\)\\|\\s\}\\|\\s\\]")
         (backward-list))
        (t (self-insert-command (or arg 1)))))

(defun goto-column (number)
  "Untabify, and go to a column NUMBER within the current line (0 is beginning of the line)."
  (interactive "nColumn number: ")
  (move-to-column number t))

(defun only-comment-p ()
  "Return t if only comment in current line.
Otherwise return nil."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward comment-start (line-end-position) t)
        (progn
          (backward-char (length comment-start))
          (equal (point)
                 (progn
                   (back-to-indentation)
                   (point))))
      nil)))

(defun zap-back-to-char (arg char)
  "No need to enter C-- to zap back."
  (interactive "p\ncZap back to char: ")
  (zap-to-char (- arg) char))

(defun region-or-buffer-limits ()
  "Return the start and end of the region as a list, smallest first.
If the region is not active or empty, then bob and eob are used."
  (if (or (not mark-active) (null (mark)) (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))

(defun goto-longest-line (beg end)
  "Goto the longest line of current buffer."
  (interactive `,(region-or-buffer-limits))
  (when (= beg end) (error "The buffer is empty"))
  (when (eq this-command last-command) (forward-line 1) (setq beg (point)))
  (goto-char beg)
  (let* ((start-line (line-number-at-pos))
         (max-width 0)
         (line start-line)
         long-lines col)
    (when (eobp) (error "End of buffer"))
    (while (and (not (eobp)) (or (not mark-active) (< (point) end)))
      (end-of-line)
      (setq col (current-column))
      (when (>= col max-width)
        (if (= col max-width)
            (setq long-lines (cons line long-lines))
          (setq long-lines (list line)))
        (setq max-width col))
      (forward-line 1)
      (setq line (1+ line)))
    (setq long-lines (nreverse long-lines))
    (let ((lines long-lines))
      (while (and lines (> start-line (car lines))) (pop lines))
      (goto-line (or (car lines) start-line)))
    (when (interactive-p)
      (let ((others (cdr long-lines)))
        (message
         "Line %d: %d chars%s (%d lines measured)"
         (car long-lines) max-width
         (concat (and others (format ", Others: {%s}"
                                     (mapconcat (lambda (line) (format "%d" line))
                                                (cdr long-lines) ", "))))
         (- line start-line))))
    (list (car long-lines) max-width (cdr long-lines) (- line start-line))))

(defun current-line-move-to-top()
  "Move current line to top of buffer."
  (interactive)
  (recenter 0))

(defun remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun point-stack-push ()
  "Push current point in stack."
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  "Pop point from stack."
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))))

(defun count-words ()
  "Count the number of word in buffer, include Chinese."
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (if mark-active
        (setq begin (region-beginning)
              end (region-end)))
    (count-ce-words begin end)))

(defun count-ce-words (beg end)
  "Count Chinese and English words in marked region."
  (interactive "r")
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end))
    (setq total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (CN: %d, EN: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))

(defun goto-percent (percent)
  "Goto PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(defun replace-match+ (object match-str replace-str)
  "Replace `MATCH-STR' of `OBJECT' with `REPLACE-STR'."
  (string-match match-str object)
  (replace-match replace-str nil nil object 0))

(provide 'basic-edit-toolkit)

;;; basic-edit-toolkit.el ends here
