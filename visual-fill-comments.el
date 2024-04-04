;;; visual-fill-comments.el --- Auto-refill comments without modifying the buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Free Software Foundation, Inc.

;; Author: Phil Sainty
;; Adapted from visual-fill.el by Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 0.1
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

;; This `visual-fill-comments-mode' minor mode visually fills comments within
;; jit-lock, hence without modifying the buffer.  Combined with the normal
;; line-wrapping this performs a kind of "auto refill" which can be more or
;; less sophisticated depending on the line-wrapping used.
;;
;; Currently this is hard-coded only for /* ... */ block comments.
;; See below: (commentstart " * ")
;;
;; Related modes and/or GNU ELPA packages:
;; - `visual-line-mode'
;; - `adaptive-wrap-prefix-mode'
;; - `visual-fill-mode'

;;; Code:

(defun visual-fill-comments--cleanup (start end)
  (while (and (< start end)
              (setq start (text-property-any start end
                                             'visual-fill-comment t)))
    (remove-text-properties
     start
     (setq start (or (text-property-not-all start end
                                            'visual-fill-comment t)
                     end))
     '(visual-fill-comment nil display nil))))

(defun visual-fill-comments--jit (start end)
  (visual-fill-comments--cleanup start end)
  (goto-char start)
  (forward-line 0)
  (let ((comstart " * ")
        columns linestart longcomment maxpos minpos prefix prefixlen replacement)
    ;; Process the specified region.
    (while (< (point) end)
      (setq linestart (point)
            columns (- fill-column (length comstart))
            longcomment (format "%s.\\{%d\\}"
                                (regexp-quote comstart) columns)
            replacement nil)
      (catch 'done
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
          ;; Calculate the prefix for this line.
          (unless replacement
            (setq prefix (if adaptive-fill-mode
                             (save-excursion
                               (forward-line 0)
                               (save-match-data
                                 (if (looking-at adaptive-fill-regexp)
                                     (make-string (- (match-end 0)
                                                     (point)
                                                     (length comstart))
                                                  ?\s)
                                   ""))))
                  prefixlen (length prefix)
                  replacement (concat "\n" comstart prefix)))
          ;; Make sure we can break safely.
          (when (>= (- maxpos minpos)
                    (- fill-column prefixlen (length comstart)))
            ;; We can't break this line, so let it run long and break at the
            ;; next opportunity.
            (goto-char maxpos)
            (unless (re-search-forward " +" (line-end-position) 'noerror)
              (throw 'done t))
            (skip-chars-backward " "))
          ;; Add the text properties.
          (add-text-properties (point) (match-end 0)
                               (list 'visual-fill-comment t
                                     'display replacement))
          ;; Continue in this line, taking the prefix into account.
          (goto-char (match-end 0))
          (setq columns (- fill-column prefixlen)
                longcomment (format ".\\{%d\\}" columns))))
      ;; Process the next line.
      (forward-line 1))))

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
