;;; sql-upcase.el --- Upcase SQL keywords -*- lexical-binding: t -*-

;; Author: Phil S.
;; URL: https://www.emacswiki.org/emacs/SqlUpcase
;; Keywords: abbrev, convenience, languages
;; Created: 9 May 2016
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.2

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
;;
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
;;
;; Note that, by default, only lower-case keywords are processed.
;; To handle mixed-case keywords as well, customize `sql-upcase-mixed-case'.


;;; Change Log:
;;
;; 0.2 - Added `sql-upcase-region' and `sql-upcase-buffer' commands.
;;     - Rename from sql-upcase-mode.el to sql-upcase.el.
;;     - Match both boundaries of a keyword, to avoid false-positives.
;;     - Constrain 'in string' and 'in comment' tests to current query.
;;     - Fixed error with whitespace at beginning of buffer.
;; 0.1 - Initial release to EmacsWiki.


;;; Code:

(require 'sql)

(defcustom sql-upcase-mixed-case nil
  "If nil, `sql-upcase-keywords' looks only for lower-case keywords,
and mixed-case keywords are ignored.

If non-nil, then mixed-case keywords will also be upcased."
  :type '(choice (const :tag "Lower-case only" nil)
                 (const :tag "Both lower- and mixed-case" t))
  :group 'SQL)

(defvar sql-upcase-boundary "[\t\n\r ();]"
  "Regexp matching a character which can precede or follow a keyword.

In addition we match \"^\" at the start of a keyword; and `sql-upcase-region'
and `sql-upcase-buffer' both match \"\\'\" at the end of a keyword.")

(defvar sql-upcase-inhibited nil
  "Set non-nil to prevent `sql-upcase-keywords' from acting.")

(defvar-local sql-upcase-comint-output nil)

(defvar sql-upcase-regions)

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

;;;###autoload
(defun sql-upcase-region (beginning end)
  "Upcase SQL keywords within the marked region.

Keywords overlapping BEGINNING will be upcased.
Keywords overlapping END will not be upcased."
  (interactive "*r")
  (save-excursion
    ;; Avoid upcasing a word preceding the region.
    (goto-char beginning)
    (and (not (eobp))
         (looking-at sql-upcase-boundary)
         (setq beginning (1+ beginning)))
    ;; Allow upcasing the final word in the region.
    (goto-char end)
    (and (not (eobp))
         (looking-at sql-upcase-boundary)
         (setq end (1+ end))))
  ;; Make an exception if the last character of the buffer is the last
  ;; character of a keyword.  Normally we require a trailing boundary
  ;; character matching `sql-upcase-boundary', but for this command we
  ;; will also treat the end of the buffer as a boundary.
  (let ((sql-upcase-boundary
         (concat "\\(?:\\'\\|" sql-upcase-boundary "\\)")))
    ;; Call our `after-change-functions' handler.
    (sql-upcase-keywords beginning end 0)))

;;;###autoload
(defun sql-upcase-buffer ()
  "Upcase all SQL keywords in the buffer."
  (interactive)
  (sql-upcase-region (point-min) (point-max)))

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
                          (re-search-forward sql-upcase-boundary end :noerror))
                (save-excursion
                  (goto-char (match-beginning 0))
                  (and (not (bobp))
                       ;; ...if the preceding character is of word syntax...
                       (eq (char-syntax (char-before)) ?w)
                       ;; ...and we're not inside a string or a comment...
                       (let* ((sep (if sql-prompt-regexp
                                       (concat
                                        "\\`\\|\\(?:" sql-prompt-regexp "\\)")
                                     "\\`"))
                              ;; TODO: Use (sql-beginning-of-statement 1)?
                              ;; Might error out. Should call that only once
                              ;; (and only if needed), and cache the result.
                              (from (save-excursion
                                      (re-search-backward sep nil :noerror)
                                      (or (match-end 0) (point-min))))
                              (syn (parse-partial-sexp from (point))))
                         (not (or (nth 3 syn) ; string
                                  (nth 4 syn)))) ; comment
                       ;; Try to match the preceding word as a SQL keyword.
                       (sql-upcase-match-keyword))))))
          ;; Upcase the matched regions (if any)
          (when sql-upcase-regions
            (undo-boundary) ;; now that save-excursion has returned
            (mapc (lambda (r) (upcase-region (car r) (cdr r)))
                  sql-upcase-regions)))))))

(defun sql-upcase-match-keyword ()
  "Matches a keyword for `sql-upcase-keywords'.

Tests whether the preceding word:

1) is itself preceded by (only) whitespace or (
2a) matches the regexp for a keyword
2b) matches the regexp for a builtin, followed by ("
  (and (catch 'matched
         (let ((inhibit-field-text-motion t)) ;; for comint
           (forward-word -1)
           (unless (bolp)
             (forward-char -1)))
         ;; Try to match a keyword using the regexps for this SQL product.
         (let* ((before (concat "\\(?:^\\|" sql-upcase-boundary "\\)"))
                (after sql-upcase-boundary)
                ;; Build regexp for statement starters.
                ;; FIXME: Generate once only, as a buffer-local var?
                (statements ;; n.b. each of these is already a regexp
                 (delq nil
                       (list (sql-get-product-feature sql-product :statement)
                             (unless (eq sql-product 'ansi)
                               (sql-get-product-feature 'ansi :statement)))))
                (statements-regexp
                 (concat "\\(?:" (mapconcat 'identity statements "\\|") "\\)")))
           ;; Check statement starters first
           (if (looking-at (concat before statements-regexp after))
               (throw 'matched t)
             ;; Otherwise process the product's font-lock keywords.
             ;; TODO: I'm not sure that `font-lock-builtin-face' can be assumed
             ;; to just be functions. (e.g. SET is not seen as a keyword.)
             (dolist (keywords (sql-get-product-feature sql-product :font-lock))
               (when (or (and (eq (cdr keywords) 'font-lock-keyword-face)
                              (looking-at (concat before (car keywords) after)))
                         (and (eq (cdr keywords) 'font-lock-builtin-face)
                              (looking-at (concat before (car keywords) "("))))
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
             sql-upcase-regions)))

(provide 'sql-upcase)
;;; sql-upcase.el ends here
