;;; sql-upcase-mode.el --- Automatically upcase SQL keywords as you type

;; Author: Phil S.
;; URL: http://www.emacswiki.org/emacs/SqlUpcaseMode
;; Keywords: abbrev, convenience, languages
;; Created: 9 May 2016
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.1

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

;; `sql-upcase-mode' converts SQL keywords to upper-case as you type
;; or otherwise insert text in the buffer -- for instance, killing and
;; yanking an entire SQL query would upcase all keywords in that query.
;;
;; It utilises the product-specific regexps defined by sql.el, and thus
;; will upcase only the keywords defined for the buffer's `sql-product'.
;; (Note that `sql-mode' buffers default to the `ansi' product.)
;;
;; You can enable it via `sql-mode-hook' and/or `sql-interactive-mode-hook':
;;
;; (add-hook 'sql-mode-hook 'sql-upcase-mode)
;; (add-hook 'sql-interactive-mode-hook 'sql-upcase-mode)

;;; Change Log:
;;
;; 0.1 - Initial release to EmacsWiki.

;;; Code:

;;;###autoload
(define-minor-mode sql-upcase-mode
  "Automatically upcase SQL keywords as text is inserted in the buffer.

Intended to be enabled via `sql-mode-hook' and/or `sql-interactive-mode-hook'.

Note that this can be a little aggressive in `sql-interactive-mode'. Although
output from the inferior process is ignored, all other text changes to the
buffer are processed (e.g. cycling through the command history)."
  :lighter " sql^"
  (if sql-upcase-mode
      (progn
        (add-hook 'after-change-functions 'sql-upcase-keywords nil :local)
        (when (derived-mode-p 'sql-interactive-mode)
          (add-hook 'comint-preoutput-filter-functions
                    'sql-upcase-comint-preoutput nil :local)))
    ;; Disable.
    (remove-hook 'after-change-functions 'sql-upcase-keywords :local)
    (when (derived-mode-p 'sql-interactive-mode)
      (remove-hook 'comint-preoutput-filter-functions
                   'sql-upcase-comint-preoutput :local))))

(defvar sql-upcase-mixed-case nil
  "If nil, `sql-upcase-keywords' looks only for lower-case keywords,
and mixed-case keywords are ignored.

If non-nil, then mixed-case keywords will also be upcased.")

(defvar sql-upcase-regions)

(defvar sql-upcase-inhibited nil
  "Set non-nil to prevent `sql-upcase-keywords' from acting.")

(defvar-local sql-upcase-comint-output nil)

(defun sql-upcase-comint-preoutput (output)
  "Inhibit `sql-upcase-keywords' for comint process output.

Called via `comint-preoutput-filter-functions'."
  (setq sql-upcase-comint-output t)
  output)

(defun sql-upcase-keywords (beginning end old-len)
  "Automatically upcase SQL keywords and builtin function names.

If `sql-upcase-mixed-case' is non-nil, then only lower-case keywords
will be processed, and mixed-case keywords will be ignored.

Triggered by `after-change-functions' (see which regarding the
function arguments), and utilising the product-specific font-lock
keywords specified in `sql-product-alist'."
  (when (eq old-len 0) ; The text change was an insertion.
    (if sql-upcase-comint-output
        ;; The current input is output from comint, so ignore it and
        ;; just reset this flag.
        (setq sql-upcase-comint-output nil)
      ;; User-generated input.
      (unless (or undo-in-progress
                  sql-upcase-inhibited)
        (let ((sql-upcase-regions nil)
              (case-fold-search sql-upcase-mixed-case))
          (save-excursion
            ;; Any errors must be handled, otherwise we will be removed
            ;; automatically from `after-change-functions'.
            (with-demoted-errors "sql-upcase-keywords error: %S"
              ;; Process all keywords affected by the inserted text.
              ;;
              ;; The "two steps forward one step back" approach of looking
              ;; for the ENDs of keywords in the text and then testing the
              ;; preceding words may seem odd, but one of the primary use-cases
              ;; is as-you-type processing, in which case the text inserts we
              ;; are looking at are typically single self-insert characters,
              ;; and we are actually searching just that single character to
              ;; see if it is a keyword-ending character, in order that we can
              ;; upcase the previously-entered keyword before it.
              (goto-char beginning)
              (while (and (< (point) end)
                          (re-search-forward "[\t\n\r ();]" end :noerror))
                (and
                 ;; ...if the preceding character is of word syntax...
                 (eq (char-syntax (char-before (1- (point)))) ?w)
                 ;; ...and we're not inside a string or a comment...
                 (let ((syn (syntax-ppss)))
                   (not (or (nth 3 syn) ; string
                            (nth 4 syn)))) ; comment
                 ;; Try to match the preceding word against the SQL keywords.
                 (sql-upcase-match-keyword)))))
          ;; Upcase the matched regions (if any)
          (when sql-upcase-regions
            (undo-boundary) ;; now that save-excursion has returned
            (mapc (lambda (r) (upcase-region (car r) (cdr r)))
                  sql-upcase-regions)))))))

;; Silence byte-compiler warnings.
(defvar sql-product)
(defvar sql-ansi-statement-starters)
(declare-function sql-get-product-feature "sql")

(defun sql-upcase-match-keyword ()
  "Matches a keyword for `sql-upcase-keywords'.

Tests whether the preceding word:

1) is itself preceded by (only) whitespace or (
2a) matches the regexp for a keyword
2b) matches the regexp for a builtin, followed by ("
  (save-excursion
    (and
     (catch 'matched
       (let ((inhibit-field-text-motion t)) ;; for comint
         (forward-word -1)
         (unless (bolp)
           (forward-char -1)))
       ;; Try to match a keyword using the regexps for this SQL product.
       (let* ((prefix "\\(?:^\\|[[:space:](]\\)") ; precedes a keyword
              ;; Build regexp for statement starters.
              ;; FIXME: Generate once only, as a buffer-local var?
              (statements ;; n.b. each of these is already a regexp
               (delq nil (list (sql-get-product-feature sql-product :statement)
                               sql-ansi-statement-starters)))
              (statements-regexp
               (concat "\\(?:" (mapconcat 'identity statements "\\|") "\\)")))
         ;; Check statement starters first
         (if (looking-at (concat prefix statements-regexp))
             (throw 'matched t)
           ;; Otherwise process the product's font-lock keywords.
           ;; TODO: I'm not sure that `font-lock-builtin-face' can be assumed
           ;; to just be functions. (e.g. SET is not seen as a keyword.)
           (dolist (keywords (sql-get-product-feature sql-product :font-lock))
             (when (or (and (eq (cdr keywords) 'font-lock-keyword-face)
                            (looking-at (concat prefix (car keywords))))
                       (and (eq (cdr keywords) 'font-lock-builtin-face)
                            (looking-at (concat prefix (car keywords) "("))))
               (throw 'matched t))))))
     ;; If `sql-upcase-mixed-case' is non-nil then check for at least one
     ;; lower-case character in the matched region, as otherwise the upcase
     ;; will be a no-op (but stored as a change in the buffer undo list).
     (or (not sql-upcase-mixed-case)
         (save-match-data
           (let ((case-fold-search nil))
             (re-search-forward "[[:lower:]]" (match-end 0) :noerror))))
     ;; Store the matched keyword region for subsequent upcasing.
     (push (cons (match-beginning 0) (match-end 0))
           sql-upcase-regions))))

(provide 'sql-upcase-mode)
;;; sql-upcase-mode.el ends here
