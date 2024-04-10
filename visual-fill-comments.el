;;; visual-fill-comments.el --- Auto-wrap comments without modifying the buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Phil Sainty
;; Inspired by visual-fill.el by Stefan Monnier
;; Version: 0.4.1
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This `visual-fill-comments-mode' minor mode visually re-formats long comment
;; lines via jit-lock (hence without modifying the buffer), wrapping word-wise
;; at the edge of the window in order to improve the comment readability.
;;
;; Comment lines matching `visual-fill-comments-regexp' are processed.
;;
;; When `visual-fill-comments-dynamic' is non-nil, window width changes will
;; cause comments to be re-wrapped accordingly.
;;
;; Related modes and/or GNU ELPA packages:
;; - `visual-line-mode'
;; - `adaptive-wrap-prefix-mode'
;; - `visual-fill-mode'

;;; Code:

(defvar visual-fill-comments-column #'visual-fill-comments-window-width-min
  "Determines the column at which comments will be wrapped.

Valid values are any positive integer; a symbol (which will be
called as a function with no arguments to obtain a value; or
nil (which means use the current `fill-column').

Some useful function symbols are `window-max-chars-per-line', and
the related functions `visual-fill-comments-window-width-min' and
`visual-fill-comments-window-width-max'.")

(defvar visual-fill-comments-column-min 40
  "If non-nil, limits the value of `visual-fill-comments-column'.")

(defvar visual-fill-comments-column-max 100
  "If non-nil, limits the value of `visual-fill-comments-column'.")

(defvar visual-fill-comments-regexp
  ;; n.b. Parens in the examples represent match subgroups.
  (rx (or
       ;; (<prefix>) (//)<comment>
       ;;             // [continues]
       (seq (group-n 1
              (opt (minimal-match (zero-or-more not-newline))
                   (not (any ":")))) ;; Ignore "://" (probable URL).
            (group-n 2
              "//" (zero-or-more " ")))
       ;; /**
       ;; (*) <comment>
       ;;  *  [continues]
       ;; (*) (@param type $foo) <comment>
       ;;  *                     [continues]
       ;; (*) (@return type) <comment>
       ;;  *                 [continues]
       ;;  */
       (seq (group-n 2
              (zero-or-more " ") "*" (zero-or-more " "))
            (opt (or (seq "@param" (one-or-more " ")
                          (opt (group (one-or-more (not (any " ")))
                                      (one-or-more " ")))
                          "$" (one-or-more (not (any " ")))
                          (one-or-more " "))
                     (seq "@return" (one-or-more " ")
                          (opt (group (one-or-more (not (any " ")))
                                      (one-or-more " ")))))))))
  "Regexp matching a buffer line with a comment.

The start of the match must be the start of the line.  The end of
the match must be within the comment.  Subgroup 1 matches any
prefix to the comment (to be replaced by equivalent spaces on
subsequent continuation lines).  Subgroup 2 contains the comment
syntax (to be repeated on each continuation line, following the
prefix indentation).  The remainder of the pattern matches any
suffix to the comment syntax (to be replaced by equivalent spaces
on subsequent continuation lines, following the comment syntax).

When `adaptive-fill-mode' is non-nil, `adaptive-fill-regexp' will
also be matched at the end of this regexp, potentially extending
the extent of the suffix.

\(Remember that you can repeat explicit group numbers when matching
multiple sets of alternatives.)")

(defvar visual-fill-comments-dynamic t
  "Whether to automatically reformat when the window width changes.")

(defvar-local visual-fill-comments--column nil)

(defun visual-fill-comments--jit (start end)
  "Apply visual comment display properties in specified region."
  (visual-fill-comments--cleanup start end)
  (goto-char start)
  (forward-line 0)
  (let ((comregexp (if adaptive-fill-mode
                       (concat "\\(?:" visual-fill-comments-regexp "\\)"
                               "\\(?:" adaptive-fill-regexp "\\)?")
                     visual-fill-comments-regexp))
        (fillcol (or visual-fill-comments--column
                     (setq-local visual-fill-comments--column
                                 (visual-fill-comments-column))))
        comstart columns linestart longcomment maxpos minpos
        prefix suffix replacement)
    ;; Process the specified region.
    (while (< (point) end)
      (when (looking-at comregexp)
        (setq linestart (point)
              prefix (make-string (length (match-string 1)) ?\s)
              comstart (match-string 2)
              suffix (make-string (- (match-end 0) (match-end 2)) ?\s)
              columns (- fillcol (- (match-end 0) (match-beginning 0)))
              longcomment (format "%s.\\{%d\\}"
                                  (regexp-quote (match-string 0)) columns)
              replacement nil)
        (catch 'done
          (unless (> columns 0)
            (throw 'done t))
          (while (looking-at longcomment)
            (setq maxpos (match-end 0))
            (goto-char maxpos)
            ;; Confirm we're inside a comment.
            (unless (nth 4 (syntax-ppss))
              (throw 'done t))
            ;; Don't break within a word.
            (skip-chars-backward " ")
            (or (looking-at " +")
                (re-search-backward " +" linestart 'noerror)
                (throw 'done t))
            (setq minpos (match-end 0))
            ;; Make sure we can break safely.
            (when (>= (- maxpos minpos) columns)
              ;; We can't break this line, so let it run long and break at the
              ;; next opportunity.
              (goto-char maxpos)
              (unless (re-search-forward " +" (line-end-position) 'noerror)
                (throw 'done t))
              (skip-chars-backward " "))
            ;; Calculate the replacement for this line.
            (unless replacement
              (setq replacement (concat "\n" prefix comstart suffix)
                    longcomment (format ".\\{%d\\}" columns)))
            ;; Add the text properties.
            (add-text-properties (point) (match-end 0)
                                 (list 'visual-fill-comment t
                                       'display replacement))
            ;; Continue in this line.
            (goto-char (match-end 0)))))
      ;; Process the next line.
      (forward-line 1))))

(defun visual-fill-comments--cleanup (start end)
  "Remove the visual comment display properties."
  (while (and (< start end)
              (setq start (text-property-any start end
                                             'visual-fill-comment t)))
    (remove-text-properties
     start
     (setq start (or (text-property-not-all start end
                                            'visual-fill-comment t)
                     end))
     '(visual-fill-comment nil display nil))))

(defun visual-fill-comments-window-width-min ()
  "Return the minimum window width for windows displaying the current buffer."
  (if-let ((bufs (get-buffer-window-list)))
      (apply #'min (mapcar #'window-max-chars-per-line bufs))
    (or visual-fill-comments--column
        fill-column)))

(defun visual-fill-comments-window-width-max ()
  "Return the maximum window width for windows displaying the current buffer."
  (if-let ((bufs (get-buffer-window-list)))
      (apply #'max (mapcar #'window-max-chars-per-line bufs))
    (or visual-fill-comments--column
        fill-column)))

(defun visual-fill-comments-column ()
  "Return the column at which to wrap long comment lines."
  (max visual-fill-comments-column-min
       (min visual-fill-comments-column-max
            (cond ((numberp visual-fill-comments-column)
                   visual-fill-comments-column)
                  ((functionp visual-fill-comments-column)
                   (funcall visual-fill-comments-column))
                  (t
                   fill-column)))))

(defun visual-fill-comments--window-configuration-change-handler ()
  "Buffer-local `window-configuration-change-hook' handler."
  ;; Called with the buffer's window selected.
  (when visual-fill-comments-dynamic
    (unless (eql visual-fill-comments--column
                 (visual-fill-comments-column))
      (visual-fill-comments-mode 1))))

;;;###autoload
(define-minor-mode visual-fill-comments-mode
  "Auto-refill comments without modifying the buffer."
  :lighter " VF*"
  :global nil
  (jit-lock-unregister #'visual-fill-comments--jit)
  (with-silent-modifications
    (visual-fill-comments--cleanup (point-min) (point-max)))
  (if visual-fill-comments-mode
      ;; Enable.
      (progn
        (jit-lock-register #'visual-fill-comments--jit)
        (setq-local visual-fill-comments--column (visual-fill-comments-column))
        (add-hook 'window-configuration-change-hook
                  'visual-fill-comments--window-configuration-change-handler
                  nil :local))
    ;; Disable.
    (setq-local visual-fill-comments--column nil)
    (remove-hook 'window-configuration-change-hook
                 'visual-fill-comments--window-configuration-change-handler
                 :local)))

(provide 'visual-fill-comments)
;;; visual-fill-comments.el ends here
