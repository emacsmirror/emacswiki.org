;;; over-long-line-mode.el --- Stops minified code bringing Emacs to its knees.
;;
;; Author: Phil S.
;; URL: http://www.emacswiki.org/emacs/OverLongLineMode
;; Created: 12 Jan 2016
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.3

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version. See <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Many Emacs modes struggle with buffers which contain excessively long lines,
;; and may consequently cause unacceptable performance issues.
;;
;; This is commonly on account of 'minified' code (i.e. code has been compacted
;; into the smallest file size possible, which often entails removing newlines
;; should they not be strictly necessary). These kinds of files are typically
;; not intended to be edited, so not providing the usual editing mode in these
;; cases will rarely be an issue.
;;
;; When such files are detected, we invoke `over-long-line-mode'. This mode is
;; almost equivalent to `fundamental-mode', and hence has a minimal affect on
;; performance in the buffer.
;;
;; The variables `over-long-line-mode-target-modes', `over-long-line-threshold',
;; `over-long-line-max-lines', and `over-long-line-mode-inhibited' determine
;; whether this mode will be invoked for a given file.  The tests are made after
;; `set-auto-mode' has set the normal major mode.

;;; Change Log:
;;
;; 0.3 - Defer to a 'mode' local variable.
;; 0.2 - Initial release to EmacsWiki.

;;; Code:

(defvar over-long-line-mode-target-modes
  '(prog-mode css-mode)
  "`over-long-line-mode' affects only these modes and their derivatives.

Our primary use-case is minified programming code, so `prog-mode' covers
most cases, but there are some exceptions to this.")

(defvar over-long-line-threshold 250
  "Number of columns after which the normal mode for a file will not be
used, unless it is specified as a local variable.

`over-long-line-mode' will be used instead in these circumstances.

See `over-long-line-detected-p' for details.")

(defvar over-long-line-max-lines 20
  "Number of non-blank, non-comment lines to test for excessive length.

See `over-long-line-detected-p' for details.")

(defvar over-long-line-mode-inhibited nil
  "Set non-nil to prevent `over-long-line-mode' from being triggered.

May be used globally or buffer-locally as required.")

(defvar-local over-long-line-original-mode nil
  "Stores the original `major-mode' value.")
(put 'over-long-line-original-mode 'permanent-local t)

(defun over-long-line-detected-p ()
  "Following any initial comments and blank lines, the next N lines of the
buffer will be tested for excessive length (where \"excessive\" means above
`over-long-line-threshold', and N is `over-long-line-max-lines').

Returns non-nil if any such excessive-length line is detected."
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (comment-forward)) ;; clears whitespace at minimum
      (catch 'excessive
        (while (< count over-long-line-max-lines)
          (if (> (- (line-end-position 1) (point))
                 over-long-line-threshold)
              (throw 'excessive t)
            (forward-line)
            (setq count (1+ count))))))))

(define-derived-mode over-long-line-mode nil "Over-long lines"
  "This mode is used if line lengths exceed `over-long-line-threshold'.

Many Emacs modes struggle with buffers which contain excessively long lines,
and may consequently cause unacceptable performance issues.

This is commonly on account of 'minified' code (i.e. code has been compacted
into the smallest file size possible, which often entails removing newlines
should they not be strictly necessary). These kinds of files are typically
not intended to be edited, so not providing the usual editing mode in these
cases will rarely be an issue.

When such files are detected, we invoke this mode. This happens after
`set-auto-mode' has set the major mode, should the selected major mode be a
member (or derivative of a member) of `over-long-line-mode-target-modes'.

By default this mode is essentially equivalent to `fundamental-mode', and
exists mainly to provide information to the user as to why the expected mode
was not used.

To revert to the original mode despite any potential performance issues,
type \\[over-long-line-mode-revert].

Alternatively, the normal mode can be manually invoked, or a file-local
`over-long-line-mode-inhibited' may be set non-nil to ensure that
the normal mode is always used for the file in question."
  (setq font-lock-mode 0)
  (message "Changed to %s (from %s) on account of line length. %s to revert."
           major-mode
           over-long-line-original-mode
           (substitute-command-keys "\\[over-long-line-mode-revert]")))

(defun over-long-line-mode-revert ()
  "Call the `major-mode' which was selected by `set-auto-mode'
before `over-long-line-mode' was called to replace it."
  (interactive)
  (if (bound-and-true-p over-long-line-original-mode)
      (funcall over-long-line-original-mode)
    (error "Original mode unknown.")))

(define-key over-long-line-mode-map (kbd "C-c C-c")
  'over-long-line-mode-revert)

(defadvice hack-local-variables (after over-long-line-mode--local-variables)
  "Ensure that `over-long-line-mode' defers to local variable mode declarations.

This advice acts after the initial MODE-ONLY call to `hack-local-variables',
and ensures that we honour a 'mode' local variable, never changing to
`over-long-line-mode' in that scenario."
  (when (ad-get-arg 0) ; MODE-ONLY argument to `hack-local-variables'
    (unless ad-return-value ; No local variable 'mode' was found
      (setq-local over-long-line-mode-inhibited t))))
(ad-activate 'hack-local-variables)

(defadvice set-auto-mode (after over-long-line-mode--set-auto-mode)
  "Maybe change to `over-long-line-mode' for files with very long lines.

This advice acts after `set-auto-mode' has set the buffer's major mode.

We can't act before this point, because some major modes must be exempt from
`over-long-line-mode' (binary file modes, for example).  Instead, we only act
when the selected major mode is a member of `over-long-line-mode-target-modes',
or a derivative thereof.

`over-long-line-detected-p' then determines whether the mode change is needed.

If buffer-local `over-long-line-mode-inhibited' is non-nil, then do nothing."
  (unless (bound-and-true-p over-long-line-mode-inhibited)
    (when (and (apply 'derived-mode-p over-long-line-mode-target-modes)
               (over-long-line-detected-p))
      (setq over-long-line-original-mode major-mode)
      (over-long-line-mode))))
(ad-activate 'set-auto-mode)

(provide 'over-long-line-mode)

;;; over-long-line-mode.el ends here
