;;; mirror-mode.el --- insert parens and string quotes in pairs

;; Copyright (C) 2009  David Shilvock

;; Author: David Shilvock <davels@shaw.ca>
;; Keywords: tools
;; $Id: mirror-mode.el 291 2009-10-21 21:48:21Z dave $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; Provides a simple minor mode to insert parens and string quotes in pairs.  It
;; can mirror the following keys: { } ( ) [ ] < > " '
;;
;; Set `mirror-keys-to-bind' if you want to limit which keys are setup as mirror
;; keys.  In most cases you'll also want to make this a local variable first.
;; Use (mirror-mode) to toggle the mode.  If `mirror-wrap-region' is non-nil
;; (the default) then typing a mirror key when the region is active will wrap
;; the region in the appropriate characters.
;;
;; Much of this code is ripped from js2-mode.

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar mirror-auto-indent nil
  "Control automatic indentation after closing parens.")

(defvar mirror-escape-quotes t
  "Non-nil to enable automatic quote-escaping inside strings.")

(defvar mirror-triple-quote-modes
  '(python-mode)
  "Major modes that use triple quote strings")

;; { } ( ) [ ] < > " '
(defvar mirror-keys-to-bind '( ?{ ?} ?\( ?\) ?[ ?] ?\" ?\' )
  "List of mirror keys to activate.
Can be any of { } ( ) [ ] < > \" \'")

(defvar mirror-wrap-region t
  "Non-nil to enable warpping the active region when typing miror keys.")

;; === Internals

(defvar mirror-mode-string " {}")

(defun mirror-call-default-key-binding (keychar)
  "Call the binding normally associated with KEYCHAR."
  (let ((mirror-mode nil)
        (last-command-event keychar))
    (call-interactively (key-binding (string keychar)))))

(defun mirror-key-binding (keychar binding)
  "Call a mirror key binding or the default binding."
  (if (memq keychar mirror-keys-to-bind)
      (call-interactively binding)
    (mirror-call-default-key-binding keychar)))

(defmacro mirror-create-binding (map key binding)
  `(let ((def #'(lambda()
                  "Maybe insert the mirrored key"
                  (interactive) (mirror-key-binding
                                 (aref ,key 0) ,binding))))
     (define-key ,map (kbd ,key) def)))

(defvar mirror-mode-map
  (let ((map (make-sparse-keymap)))
    (mirror-create-binding map "{" 'mirror-match-curly)
    (mirror-create-binding map "}" 'mirror-magic-close-paren)
    (mirror-create-binding map "(" 'mirror-match-paren)
    (mirror-create-binding map ")" 'mirror-magic-close-paren)
    (mirror-create-binding map "[" 'mirror-match-bracket)
    (mirror-create-binding map "]" 'mirror-magic-close-paren)
    (mirror-create-binding map "<" 'mirror-match-angle)
    (mirror-create-binding map ">" 'mirror-magic-close-paren)    
    (mirror-create-binding map "\"" 'mirror-match-double-quote)
    (mirror-create-binding map "'" 'mirror-match-single-quote)
    map))

(define-minor-mode mirror-mode
  "Insert paren and string characters in pairs."
  nil
  mirror-mode-string
  mirror-mode-map)

(defsubst mirror-same-line (pos)
  "Return t if POS is on the same line as current point."
  (and (>= pos (point-at-bol))
       (<= pos (point-at-eol))))

(defsubst mirror-inside-string ()
  "Return non-nil if inside a string.
Actually returns the quote character that begins the string."
   (let ((parse-state (save-excursion
                        (parse-partial-sexp (point-min) (point)))))
      (nth 3 parse-state)))

(defsubst mirror-inside-comment ()
  "Return non-nil if inside a comment."
  (let ((parse-state (save-excursion
                       (parse-partial-sexp (point-min) (point)))))
    (nth 4 parse-state)))

(defsubst mirror-inside-comment-or-string ()
  "Return non-nil if inside a comment or string."
  (or
   (let ((comment-start
          (save-excursion
            (goto-char (point-at-bol))
            (if (re-search-forward "//" (point-at-eol) t)
                (match-beginning 0)))))
     (and comment-start
          (<= comment-start (point))))
   (let ((parse-state (save-excursion
                        (parse-partial-sexp (point-min) (point)))))
     (or (nth 3 parse-state)
         (nth 4 parse-state)))))

(defsubst mirror-region-marked ()
  (and transient-mark-mode mark-active))

(defun mirror-wrap-region (wrap-char)
  "If region is active wrap it with WRAP-CHAR.
If WRAP-CHAR is a cons wrap with car/cdr of WRAP-CHAR on left/right. Returns
  non-nil if the region was wrapped."
  (if (and mirror-wrap-region
           (mirror-region-marked))
      (let ((leftpos (region-beginning))
            (rightpos (region-end))
            leftch rightch)
        (if (consp wrap-char)
            (setq leftch (car wrap-char)
                  rightch (cdr wrap-char))
          (setq leftch wrap-char
                rightch wrap-char))
        (save-excursion
          (goto-char rightpos)
          (insert rightch)
          (goto-char leftpos)
          (insert leftch))
        t)))

(defun mirror-match-curly ()
  "Insert matching curly-brace."
  (interactive)
  (unless (mirror-wrap-region (cons ?\{ ?\}))
    (mirror-call-default-key-binding ?\{)
    (unless (or (not (looking-at "\\s-*$"))
                (mirror-inside-comment-or-string))
      ;;    (undo-boundary)
      ;; absolutely mystifying bug:  when inserting the next "\n",
      ;; the buffer-undo-list is given two new entries:  the inserted range,
      ;; and the incorrect position of the point.  It's recorded incorrectly
      ;; as being before the opening "{", not after it.  But it's recorded
      ;; as the correct value if you're debugging `mirror-match-curly'
      ;; in edebug.  I have no idea why it's doing this, but incrementing
      ;; the inserted position fixes the problem, so that the undo takes us
      ;; back to just after the user-inserted "{".
      (insert "\n")
      ;;     (ignore-errors
      ;;       (incf (cadr buffer-undo-list)))
      (indent-according-to-mode)
      (save-excursion
        (insert "\n")
        (mirror-call-default-key-binding ?\})
      (if mirror-auto-indent
          (indent-according-to-mode))))))
  
(defun mirror-match-bracket ()
  "Insert matching bracket."
  (interactive)
  (unless (mirror-wrap-region (cons ?\[ ?\]))
    (mirror-call-default-key-binding ?\[)
    (unless (or (not (looking-at "\\s-*$"))
                (mirror-inside-comment-or-string))
      (save-excursion
        (mirror-call-default-key-binding ?\]))
      (if mirror-auto-indent
          (indent-according-to-mode)))))

(defun mirror-match-angle ()
  "Insert matching angle bracket."
  (interactive)
  (unless (mirror-wrap-region (cons ?\< ?\>))
    (mirror-call-default-key-binding ?\<)
    (unless (or (not (looking-at "\\s-*$"))
                (mirror-inside-comment-or-string))
      (save-excursion
        (mirror-call-default-key-binding ?\>))
      (if mirror-auto-indent
          (indent-according-to-mode)))))

(defun mirror-match-paren ()
  "Insert matching paren unless already inserted."
  (interactive)
  (unless (mirror-wrap-region (cons ?\( ?\)))
    (mirror-call-default-key-binding ?\()
    (unless (or (not (looking-at "\\s-*$"))
                (mirror-inside-comment-or-string))
      (save-excursion
        (mirror-call-default-key-binding ?\)))
      (if mirror-auto-indent
          (indent-according-to-mode)))))

(defun mirror-match-quote (quote-string)
  (cond
   ;; 1. inside a comment - don't do quote-matching, since we can't
   ;; reliably figure out if we're in a string inside the comment
   ((mirror-inside-comment)
    (insert quote-string))
   ;; 2. not in string => insert matched quotes
   ((not (mirror-inside-string))
    (insert quote-string)
    ;; 2b. exception:  if we're just before a word, don't double it.
    (unless (looking-at "[^][(){} \t\r\n]")
      (save-excursion
        ;; check for triple quote start
        (if (and (memq major-mode mirror-triple-quote-modes)
                 (looking-back (concat "\\([^" quote-string "]\\|\\`\\)"
                                       quote-string
                                       quote-string
                                       quote-string)))
            (insert quote-string quote-string))
        (insert quote-string))))
   ;; 3. maybe insert another quote or step over closing quote
   ((looking-at quote-string)
    (cond ((looking-back "[^\\]\\\\")
           (insert quote-string))
          (t
           (forward-char 1))))
   ;; 4. inside terminated string, escape quote (unless already escaped)
   ((and mirror-escape-quotes
         (save-excursion
           (save-match-data
             (re-search-forward quote-string (point-at-eol) t))))
    (insert (if (looking-back "[^\\]\\\\")
                quote-string
              (concat "\\" quote-string))))
   ;; 5. else terminate the string
   (t
    (insert quote-string))))

(defun mirror-match-single-quote ()
  "Insert matching single-quote."
  (interactive)
  (unless (mirror-wrap-region "'")
    (let ((parse-status (parse-partial-sexp (point-min) (point))))
      ;; don't match inside comments, since apostrophe is more common
      (if (nth 4 parse-status)
          (insert "'")
        (mirror-match-quote "'")))))

(defun mirror-match-double-quote ()
  "Insert matching double-quote."
  (interactive)
  (unless (mirror-wrap-region "\"")
    (mirror-match-quote "\"")))

(defun mirror-magic-close-paren ()
  "Skip over close-paren rather than inserting, where appropriate.
Uses some heuristics to try to figure out the right thing to do."
  (interactive)
  (let* ((parse-status (parse-partial-sexp (point-min) (point)))
         (open-pos (nth 1 parse-status))
         (close last-input-event)
         (open (cond
                ((eq close ?\))
                 ?\()
                ((eq close ?\])
                 ?\[)
                ((eq close ?\>)
                 ?\<)
                ((eq close ?})
                 ?{)
                (t nil))))
    (if (and (looking-at (string close))
             (eq open (char-after open-pos))
             (mirror-same-line open-pos))
        (forward-char 1)
      ;;(insert (string close))
      (mirror-call-default-key-binding close)
      )
    (blink-matching-open)))


(provide 'mirror-mode)
;;; mirror-mode.el ends here
