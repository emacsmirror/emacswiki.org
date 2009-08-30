;;; gb-po-mode.el ---  po-mode extensions and fixes

;; Copyright (C) 2009  Georg Brandl <georg at python org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This extension for po-mode does four things:
;;
;; * On edit, po-mode inserts a "<" at the end of the msgstr. This mark is made
;;   read-only, so that you can neither delete it accidentally nor insert text
;;   after it.
;;
;; * After edit, msgstrs are automatically re-wrapped to a width specified by
;;   `po-wrap-width' (but no extraneous newlines are introduced).
;;
;; * The handling of the window of the subedit buffer (which is the buffer where
;;   you edit a msgstr) is improved.
;;
;;   The exact behavior is controlled by `po-po-to-edit-buffer':
;;   If it is nil, the subedit buffer will appear in a small window at the
;;   top of the window showing the po file.  Its height will adapt to the
;;   number of lines in the msgstr.
;;   If it is non-nil, the subedit buffer will be shown using `pop-to-buffer'
;;   like with plain po-mode, but the behavior on exiting the subedit
;;   window is more like what you would expect.
;;
;; * Finally, the fuzzy mark on an entry is removed after a successful edit.

;; Usage: just load this file after loading po-mode, e.g. with
;;
;;   (eval-after-load 'po-mode '(load "gb-po-mode"))

;;; Code:

;;; new custom entries

(defcustom po-pop-to-edit-buffer nil
  "*Use `pop-to-buffer' for switching to the buffer where a message is edited.
If nil, vertically split off a small editing window."
  :type 'boolean
  :group 'po)

(defcustom po-wrap-width 80
  "*Width to which to wrap msgstrs."
  :type 'integer
  :group 'po)


;;; implement automatic wrapping of msgstrs after edit

(defun po-eval-requoted (form prefix obsolete)
  "Eval FORM, which inserts a string, and return the string fully requoted.
If PREFIX, precede the result with its contents.  If OBSOLETE, comment all
generated lines in the returned string.  Evaluating FORM should insert the
wanted string in the buffer which is current at the time of evaluation.
If FORM is itself a string, then this string is used for insertion."
  (po-with-temp-buffer
    (if (stringp form)
        (insert form)
      (push-mark)
      (eval form))
    ;; quote all problematic characters in the msgstr
    (goto-char (point-min))
    (while (re-search-forward "[\"\a\b\f\n\r\t\\]" nil t)
      (cond ((eq (preceding-char) ?\") (replace-match "\\\"" t t))
            ((eq (preceding-char) ?\a) (replace-match "\\a" t t))
            ((eq (preceding-char) ?\b) (replace-match "\\b" t t))
            ((eq (preceding-char) ?\f) (replace-match "\\f" t t))
            ((eq (preceding-char) ?\n) (replace-match "\\n" t t))
            ((eq (preceding-char) ?\r) (replace-match "\\r" t t))
            ((eq (preceding-char) ?\t) (replace-match "\\t" t t))
            ((eq (preceding-char) ?\\) (replace-match "\\\\" t t))))
    ;; now go about rewrapping the msgstr
    (goto-char (point-min))
    (while (not (eobp))
      ;; start each line with a quote
      (insert "\"")
      ;; see where to set next linebreak
      (let* ((start-of-line (point))
             ;; calculate position before which break has to occur;
             ;; subtract 2 for the opening and closing quote
             (max-end-of-line (+ start-of-line (- po-wrap-width 2))))
        ;; break at \n, no matter where it is on the line
        (if (re-search-forward "\\\\n" max-end-of-line 'move-to-limit)
            ;; insert quote if not already at end of text
            (unless (eobp) (insert "\"\n"))
          ;; no \n found within a line, and point is at max. end of line now
          ;; if already at end-of-buffer, do nothing
          (when (not (eobp))
            ;; search backwards for a space to break after
            (if (re-search-backward " " start-of-line t)
                ;; when found a space, break after it
                (forward-char 1))
            ;; in any case, break at point
            (insert "\"\n")))))
    ;; insert the finishing quote
    (insert "\"\n")
    ;; remove last newline if no prefix given
    (if (not prefix) (backward-delete-char 1))
    (goto-char (point-min))
    (if prefix (insert prefix " "))
    ;; insert proper "" indicator for multi-line msgstrs
    (if (save-excursion (re-search-forward "[^\n]\n+[^\n]" nil t))
        (insert "\"\"\n"))
    (if obsolete
        (progn
          (goto-char (point-min))
          (while (not (eobp))
            (or (eq (following-char) ?\n) (insert "#~ "))
            (search-forward "\n"))))
    (buffer-string)))

;;; remove fuzzy mark after editing an entry

(defadvice po-subedit-exit (after po-remove-fuzzy-after-edit activate)
  "Removes fuzzy mark after successful edit."
  (po-decrease-type-counter)
  (po-delete-attribute "fuzzy")
  (po-current-entry)
  (po-increase-type-counter))

;;; implement better subedit window logic

(defadvice po-edit-string (around po-subedit-window-logic-begin activate)
  "Implements better subedit window logic."
  (let ((orig-pop-to-buffer (symbol-function 'pop-to-buffer)))
    (defun pop-to-buffer (buffer)
      (if po-pop-to-edit-buffer
          (funcall orig-pop-to-buffer buffer)
        ;; "string" is an argument of po-edit-string (the msgstr)
        (let ((newline-count (count-if (lambda (c) (eq c 10)) string)))
          (split-window-vertically (+ newline-count 6))
          (switch-to-buffer buffer))))
    (unwind-protect
        ad-do-it
      (fset 'pop-to-buffer orig-pop-to-buffer))))

(defadvice po-subedit-abort (around po-subedit-window-logic-end activate)
  "Implements better subedit window logic."
  (let ((orig-switch-to-buffer (symbol-function 'switch-to-buffer)))
    (defun switch-to-buffer (buffer)
      (let ((buffer-window (get-buffer-window buffer)))
        (if po-pop-to-edit-buffer
            (if buffer-window
                (select-window buffer-window)
              (funcall orig-switch-to-buffer buffer))
          (delete-window))))
    (unwind-protect
        ad-do-it
      (fset 'switch-to-buffer orig-switch-to-buffer))))

;;; use a better end marker for the msgstr

(add-hook 'po-subedit-mode-hook
          (lambda ()
            ;; don't display a header line
            (setq header-line-format nil)
            (save-excursion
              (goto-char (point-max))
              ;; delete the one inserted by po-edit-string
              (backward-delete-char 1)
              ;; insert a read-only one
              (insert #("<" 0 1 (read-only t))))))

(defadvice po-subedit-exit (before po-remove-readonly-end-marker activate)
  ;; the read-only property can only be removed when `inhibit-read-only' is t
  (let ((old-inhibit-read-only inhibit-read-only))
    (setq inhibit-read-only t)
    (remove-text-properties (1- (point-max)) (point-max) '(read-only t))
    (setq inhibit-read-only old-inhibit-read-only)))
