;;; help-mode+.el --- Extensions to `help-mode.el'
;;
;; Filename: help-mode+.el
;; Description: Extensions to `help-mode.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2004-2017, Drew Adams, all rights reserved.
;; Created: Sat Nov 06 15:14:12 2004
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Feb 23 07:35:42 2017 (-0800)
;;           By: dradams
;;     Update #: 204
;; URL: https://www.emacswiki.org/emacs/download/help-mode%2b.el
;; Doc URL: http://emacswiki.org/HelpPlus
;; Keywords: help
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `button', `help-mode', `view'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `help-mode.el'
;;
;;
;;  ***** NOTE: The following functions defined in `help-mode.el'
;;              have been REDEFINED HERE:
;;
;;  `help-make-xrefs' - Put symbol clause first, so cross-xref links
;;                      show doc for both fun and var, if available.
;;  `help-mode'       - If `one-window-p', then delete Help frame.
;;  `help-xref-on-pp' - Library names are buttonized.
;;
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'help-mode+)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/01/17 dadams
;;     Do not redefine help-mode for Emacs 24+.
;; 2012/10/25 dadams
;;     Added redefinition of help-make-xrefs.
;; 2011/01/04 dadams
;;     Removed autoload cookie from non-interactive function.
;; 2007/12/14 dadams
;;     Added redefinition of help-mode.
;;     Removed pop-to-help-buffer and help-origin-buffer to (new) help+.el.
;; 2006/07/11 dadams
;;     Added: help-origin-buffer, pop-to-help-toggle.  Bound latter to C-h C-o.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'help-mode)

;; Quiet the byte-compiler.
(defvar help-forward-label)
(defvar help-xref-override-view-map)

;;;;;;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL IN `help-mode.el'.
;;
;; Deletes frame if `one-window-p'.
;;
;;;###autoload
(when (< emacs-major-version 24)        ; Emacs 24+ does not use `view-mode', so no need.
  (defun help-mode ()
    "Major mode for viewing help text and navigating references in it.
Entry to this mode runs the normal hook `help-mode-hook'.
Commands:
\\{help-mode-map}"
    (interactive)
    (kill-all-local-variables)
    (use-local-map help-mode-map)
    (setq mode-name   "Help"
          major-mode  'help-mode)
    (view-mode)
    (make-local-variable 'view-no-disable-on-exit)
    (setq view-no-disable-on-exit  t
          view-exit-action         (lambda (buffer)
                                     (or (window-minibuffer-p (selected-window))
                                         (when (eq (window-buffer) (get-buffer "*Help*"))
                                           (if (one-window-p t)
                                               (delete-frame)
                                             (delete-window))))))
    (run-mode-hooks 'help-mode-hook)))


;; REPLACES ORIGINAL IN `help-mode.el'.
;;
;; Show all doc possible for a symbol that is any 2 or 3 of fn, var, and face.
;;
;; See Emacs bug #12686.
;;
;;;###autoload
(defun help-make-xrefs (&optional buffer)
  "Parse and hyperlink documentation cross-references in the given BUFFER.

Find cross-reference information in a buffer and activate such cross
references for selection with `help-follow'.  Cross-references have
the canonical form `...'  and the type of reference may be
disambiguated by the preceding word(s) used in
`help-xref-symbol-regexp'.  Faces only get cross-referenced if
preceded or followed by the word `face'.  Variables without
variable documentation do not get cross-referenced, unless
preceded by the word `variable' or `option'.

If the variable `help-xref-mule-regexp' is non-nil, find also
cross-reference information related to multilingual environment
\(e.g., coding-systems).  This variable is also used to disambiguate
the type of reference as the same way as `help-xref-symbol-regexp'.

A special reference `back' is made to return back through a stack of
help buffers.  Variable `help-back-label' specifies the text for
that."
  (interactive "b")
  (with-current-buffer (or buffer  (current-buffer))
    (save-excursion
      (goto-char (point-min))
      ;; Skip the header-type info, though it might be useful to parse
      ;; it at some stage (e.g. "function in `library'").
      (forward-paragraph)
      (let ((old-modified  (buffer-modified-p)))
        (let ((stab               (syntax-table))
              (case-fold-search   t)
              (inhibit-read-only  t))
          (set-syntax-table emacs-lisp-mode-syntax-table)
          ;; The following should probably be abstracted out.
          (unwind-protect
               (progn
                 ;; Info references
                 (save-excursion
                   (while (re-search-forward help-xref-info-regexp nil t)
                     (let ((data  (match-string 2)))
                       (save-match-data
                         (unless (string-match "^([^)]+)" data)
                           (setq data  (concat "(emacs)" data)))
                         (setq data ;; possible newlines if para filled
                               (replace-regexp-in-string "[ \t\n]+" " " data t t)))
                       (help-xref-button 2 'help-info data))))
                 ;; URLs
                 (save-excursion
                   (while (re-search-forward help-xref-url-regexp nil t)
                     (let ((data  (match-string 1)))
                       (help-xref-button 1 'help-url data))))
                 ;; Mule related keywords.  Do this before trying
                 ;; `help-xref-symbol-regexp' because some of Mule
                 ;; keywords have variable or function definitions.
                 (when help-xref-mule-regexp
                   (save-excursion
                     (while (re-search-forward help-xref-mule-regexp nil t)
                       (let* ((data  (match-string 7))
                              (sym   (intern-soft data)))
                         (cond ((match-string 3) ; coding system
                                (and sym
                                     (coding-system-p sym)
                                     (help-xref-button 6 'help-coding-system sym)))
                               ((match-string 4) ; input method
                                (and (assoc data input-method-alist)
                                     (help-xref-button 7 'help-input-method data)))
                               ((or (match-string 5)  (match-string 6)) ; charset
                                (and sym
                                     (charsetp sym)
                                     (help-xref-button 7 'help-character-set sym)))
                               ((assoc data input-method-alist)
                                (help-xref-button 7 'help-character-set data))
                               ((and sym  (coding-system-p sym))
                                (help-xref-button 7 'help-coding-system sym))
                               ((and sym  (charsetp sym))
                                (help-xref-button 7 'help-character-set sym)))))))
                 ;; Quoted symbols
                 (save-excursion
                   (while (re-search-forward help-xref-symbol-regexp nil t)
                     (let* ((data  (match-string 8))
                            (sym   (intern-soft data)))
                       (when sym
                         (cond ((or (and (or (boundp sym) ; var & fn
                                             (get sym 'variable-documentation))
                                         (fboundp sym))
                                    (and (fboundp sym) ; fn & face
                                         (facep sym))
                                    (and (or (boundp sym) ; var & face
                                             (get sym 'variable-documentation))
                                         (facep sym)))
                                ;; Var, function, or face -- doc all, if possible.
                                (help-xref-button 8 'help-symbol sym))
                               ((match-string 3) ; `variable' &c
                                (and (or (boundp sym) ; `variable' doesn't ensure
                                        ; it's actually bound
                                         (get sym 'variable-documentation))
                                     (help-xref-button 8 'help-variable sym)))
                               ((match-string 4) ; `function' &c
                                (and (fboundp sym) ; similarly
                                     (help-xref-button 8 'help-function sym)))
                               ((match-string 5) ; `face'
                                (and (facep sym)
                                     (help-xref-button 8 'help-face sym)))
                               ((match-string 6)) ; nothing for `symbol'
                               ((match-string 7)
                                ;; this used:
                                ;; #'(lambda (arg)
                                ;;     (let ((location  (find-function-noselect arg)))
                                ;;       (pop-to-buffer (car location))
                                ;; 	(goto-char (cdr location))))
                                (help-xref-button 8 'help-function-def sym))
                               ((and
                                 (facep sym)
                                 (save-match-data (looking-at "[ \t\n]+face\\W")))
                                (help-xref-button 8 'help-face sym))
                               ((and
                                 (or (boundp sym)  (get sym 'variable-documentation))
                                 (or (documentation-property sym 'variable-documentation)
                                     (if (or (> emacs-major-version 24)
                                             (and (= emacs-major-version 24)
                                                  (> emacs-minor-version 2)))
                                         (documentation-property (indirect-variable sym)
                                                                 'variable-documentation)
                                       (condition-case nil
                                           (documentation-property (indirect-variable sym)
                                                                   'variable-documentation)
                                         (cyclic-variable-indirection nil)))))
                                (help-xref-button 8 'help-variable sym))
                               ((fboundp sym)
                                (help-xref-button 8 'help-function sym)))))))
                 ;; An obvious case of a key substitution:
                 (save-excursion
                   (while (re-search-forward
                           ;; Assume command name is only word and symbol
                           ;; characters to get things like `use M-x foo->bar'.
                           ;; Command required to end with word constituent
                           ;; to avoid `.' at end of a sentence.
                           "\\<M-x\\s-+\\(\\sw\\(\\sw\\|\\s_\\)*\\sw\\)" nil t)
                     (let ((sym  (intern-soft (match-string 1))))
                       (when (fboundp sym) (help-xref-button 1 'help-function sym)))))
                 ;; Look for commands in whole keymap substitutions:
                 (save-excursion
                   ;; Make sure to find the first keymap.
                   (goto-char (point-min))
                   ;; Find a header and the column at which the command
                   ;; name will be found.

                   ;; If the keymap substitution isn't the last thing in
                   ;; the doc string, and if there is anything on the same
                   ;; line after it, this code won't recognize the end of it.
                   (while (re-search-forward "^key +binding\n\\(-+ +\\)-+\n\n"
                                             nil t)
                     (let ((col  (- (match-end 1) (match-beginning 1))))
                       (while (and (not (eobp))
                                   ;; Stop at a pair of blank lines.
                                   (not (looking-at "\n\\s-*\n")))
                         ;; Skip a single blank line.
                         (and (eolp)  (forward-line))
                         (end-of-line)
                         (skip-chars-backward "^ \t\n")
                         (when (and (>= (current-column) col)
                                    (looking-at "\\(\\sw\\|\\s_\\)+$"))
                           (let ((sym  (intern-soft (match-string 0))))
                             (when (fboundp sym) (help-xref-button 0 'help-function sym))))
                         (forward-line))))))
            (set-syntax-table stab))
          ;; Delete extraneous newlines at the end of the docstring
          (goto-char (point-max))
          (while (and (not (bobp))  (bolp)) (delete-char -1))
          (insert "\n")
          (when (or help-xref-stack  (and (boundp 'help-xref-forward-stack)
                                          help-xref-forward-stack))
            (insert "\n"))
          ;; Make a back-reference in this buffer if appropriate.
          (when help-xref-stack
            (help-insert-xref-button help-back-label 'help-back
                                     (current-buffer)))
          ;; Make a forward-reference in this buffer if appropriate.
          (when (and (boundp 'help-xref-forward-stack)  help-xref-forward-stack)
            (when help-xref-stack
              (insert "\t"))
            (help-insert-xref-button help-forward-label 'help-forward
                                     (current-buffer)))
          (when (or help-xref-stack  (and (boundp 'help-xref-forward-stack)
                                          help-xref-forward-stack))
            (insert "\n")))
        (when (< emacs-major-version 24)
          ;; View mode steals RET from us.
          (set (make-local-variable 'minor-mode-overriding-map-alist)
               (list (cons 'view-mode help-xref-override-view-map))))
        (when (or (> emacs-major-version 24)
                  (and (= emacs-major-version 24)  (> emacs-minor-version 2)))
          (set-buffer-modified-p old-modified))))))


;; REPLACES ORIGINAL IN `help-mode.el'.

;; Buttonizes names of libraries also.
;; To see the effect, try `C-h v features', and click on a library name.
;;
;; 2006-01-20: This no longer works, because the call to this function
;; from `describe-variable' was commented out in `help-fns.el'.
;;
(defun help-xref-on-pp (from to)
  "Add xrefs for symbols in `pp's output between FROM and TO."
  (if (> (- to from) 5000)
      nil
    (with-syntax-table emacs-lisp-mode-syntax-table
      (save-excursion
        (save-restriction
          (narrow-to-region from to)
          (goto-char (point-min))
          (condition-case nil
              (while (not (eobp))
                (cond
                  ((looking-at "\"") (forward-sexp 1))
                  ((looking-at "#<") (search-forward ">" nil 'move))
                  ((looking-at "\\(\\(\\sw\\|\\s_\\)+\\)")
                   (let* ((sym   (intern-soft (match-string 1)))
                          (type  (cond ((fboundp sym) 'help-function)
                                       ((or (memq sym '(t nil))  (keywordp sym))
                                        nil)
                                       ((and sym  (boundp sym))
                                        'help-variable)
                                       ((and sym  (locate-library (symbol-name sym)))
                                        'help-library))))
                     (when type (help-xref-button 1 type sym)))
                   (goto-char (match-end 1)))
                  (t (forward-char 1))))
            (error nil)))))))

(define-button-type 'help-library
  :supertype 'help-xref
  'help-function #'(lambda (x) (find-library (symbol-name x)))
  'help-echo (purecopy "mouse-2, RET: find this library"))

;; $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;; REPLACES ORIGINAL IN `help-mode.el'.
;; Provide a tooltip for whatever is under the mouse.
;; This can't be done here - the message needs to be done via an idle timer,
;; whenever mouse is over any name. Perhaps combine with eldoc.
;;
;; ;;;###autoload
;; (defun help-follow-mouse (click)
;;   "Follow the cross-reference that you CLICK on."
;;   (interactive "e")
;;   (let* ((start  (event-start click))
;;       (window (car start))
;;       (pos (car (cdr start))))
;;     (with-current-buffer (window-buffer window)
;;       (message "Display help on `%s'"
;;                (save-excursion
;;                  (goto-char pos) (skip-syntax-backward "w_")
;;                  (buffer-substring (point)
;;                                    (progn (skip-syntax-forward "w_")
;;                                           (point)))))
;;       (help-follow pos))))

;; After a certain idle time, use function `mouse-position', and pick
;; up the symbol under the pointer. Then display a message that
;; clicking mouse-2 will display help on the symbol.


;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'help-mode+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; help-mode+.el ends here
