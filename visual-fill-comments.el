;;; visual-fill-comments.el --- Auto-wrap comments without modifying the buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Phil Sainty
;; Inspired by visual-fill.el by Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 0.3
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

;; This `visual-fill-comments-mode' minor mode visually reformats long comment
;; lines via jit-lock (hence without modifying the buffer), wrapping word-wise
;; at `fill-column' in order to improve the comment readability.
;;
;; Comments matching `visual-fill-comments-regexp' (see which) are processed.
;;
;; Related modes and/or GNU ELPA packages:
;; - `visual-line-mode'
;; - `adaptive-wrap-prefix-mode'
;; - `visual-fill-mode'

;;; Code:

(defvar visual-fill-comments-regexp
  (rx (or (seq (group-n 1 (minimal-match (zero-or-more not-newline)))
               (group-n 2 "//" (zero-or-more " ")))
          (seq (group-n 2 (zero-or-more " ") "*" (zero-or-more " "))
               (group-n 3 (opt (or (regexp "@param +\\([^ ]+ +\\)?\\$[^ ]+ +")
                                   (regexp adaptive-fill-regexp)))))))
  "Regexp matching a buffer line with a comment.

The start of the match must be the start of the line.  The end of
the match must be within the comment.  Subgroup 1 matches any
prefix to the comment (to be replaced by equivalent spaces on
subsequent continuation lines).  Subgroup 2 contains the comment
syntax (to be repeated on each continuation line, following the
prefix indentation).  Subgroup 3 matches any suffix to the
comment syntax (to be replaced by equivalent spaces on subsequent
continuation lines, following the comment syntax).

\(Remember that you can repeat explicit group numbers when matching
multiple sets of alternatives.)")

(defun visual-fill-comments--jit (start end)
  "Apply visual comment display properties in specified region."
  (visual-fill-comments--cleanup start end)
  (goto-char start)
  (forward-line 0)
  (let (comstart columns linestart longcomment maxpos minpos
                 prefix suffix replacement)
    ;; Process the specified region.
    (while (< (point) end)
      (when (looking-at visual-fill-comments-regexp)
        (setq linestart (point)
              prefix (make-string (length (match-string 1)) ?\s)
              comstart (match-string 2)
              suffix (make-string (length (match-string 3)) ?\s)
              columns (- fill-column (- (match-end 0) (match-beginning 0)))
              longcomment (format "%s.\\{%d\\}"
                                  (regexp-quote (match-string 0)) columns)
              replacement nil)
        (catch 'done
          ;; (message "%S" (list linestart prefix comstart suffix columns longcomment))
          ;; (throw 'done t)
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

;;;###autoload
(define-minor-mode visual-fill-comments-mode
  "Auto-refill comments without modifying the buffer."
  :lighter " VF*"
  :global nil
  (jit-lock-unregister #'visual-fill-comments--jit)
  (with-silent-modifications
    (visual-fill-comments--cleanup (point-min) (point-max)))
  (when visual-fill-comments-mode
    (jit-lock-register #'visual-fill-comments--jit)))

(provide 'visual-fill-comments)
;;; visual-fill-comments.el ends here
