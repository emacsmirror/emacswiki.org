;;; isearch+.el --- Extensions to `isearch.el'.
;;
;; Filename: isearch+.el
;; Description: Extensions to `isearch.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2009, Drew Adams, all rights reserved.
;; Created: Fri Dec 15 10:44:14 1995
;; Version: 21.0
;; Last-Updated: Sat Aug  1 15:34:38 2009 (-0700)
;;           By: dradams
;;     Update #: 421
;; URL: http://www.emacswiki.org/cgi-bin/wiki/isearch+.el
;; Keywords: help, matching, internal, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `frame-fns', `misc-cmds', `misc-fns'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `isearch.el'.
;;
;;  Commands defined here:
;;
;;    `isearchp-goto-success-end', `isearchp-toggle-set-region',
;;    `isearch-toggle-word', `set-region-around-search-target'.
;;
;;  Non-interactive functions defined here:
;;
;;    `isearchp-set-region'.
;;
;;  User options defined here:
;;
;;    `isearchp-set-region-flag'.
;;
;;  Faces defined here:
;;
;;    `isearch-fail'.
;;
;;
;;  ***** NOTE: The following functions defined in `isearch.el' have
;;              been REDEFINED HERE:
;;
;;  `isearch-mode-help' - Ends isearch.  Lists bindings.
;;  `isearch-message'   - Highlights failed part of search string in
;;                        echo area, in face `isearch-fail'.
;;
;;
;;  The following bindings are made here for incremental search mode
;;  (`C-s' prefix):
;;
;;    `C-SPC'      `isearchp-toggle-set-region'
;;    `C-c'        `isearch-toggle-case-fold'
;;    `C-h'        `isearch-mode-help'
;;    `C-t'        `isearch-toggle-regexp'
;;    `M-w'        `isearch-toggle-word'
;;    `C-end'      `goto-longest-line' (if defined)
;;    `C-M-tab'    `isearch-complete' (on MS Windows)
;;    `next'       `isearch-repeat-forward'
;;    `prior'      `isearch-repeat-backward'
;;
;;
;;  The following bindings are made here for incremental search edit
;;  mode:
;;
;;    `M-e'        `isearchp-goto-success-end' (Emacs 22+)
;;    `C-M-tab'    `isearch-complete-edit' (MS Windows only)
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `isearch.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "isearch" '(require 'isearch+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2009/06/09 dadams
;;     Bind isearch-repeat-(forward|backward) to (next|prior) in isearch-mode-map.
;; 2008/11/10 dadams
;;     Added: isearchp-goto-success-end.
;; 2008/05/25 dadams
;;     Don't add C-M-tab to isearch-mode-map if already defined.
;; 2008/05/24 dadams
;;     Don't bind C-j to isearch-edit-string.  Bind M-e to isearch-edit-string (for Emacs 20).
;; 2008/02/28 dadams
;;     isearch-message: Protect from Emacs 21 also.
;; 2008/02/24 dadams
;;     isearch-message:
;;       Juri's fix for M-r (was losing failed text) and C-M-s [a-z] (highlighted only ]).
;; 2008/02/23 dadams
;;     isearch-message:
;;       isearch-fail face: Provide better defaults.
;;       Juri's fix for M-p: Use isearch-message for succ-msg, if diff from first msg of
;;         isearch-cmds (isearch-edit-string sets it).
;; 2007/09/10 dadams
;;     Bound goto-longest-line to C-s C-end.  Added soft require of misc-cmds.el.
;; 2007/09/07 dadams
;;     isearch-message:
;;       regexp-quote succ-msg. put-text-property, not propertize, for trailing whitespace.
;; 2007/07/10 dadams
;;     isearchp-set-region: Do nothing unless transient-mark-mode.
;; 2007/02/02 dadams
;;     isearch-message: Fixed when succ-msg matches whole isearch-message (no highlight).
;; 2007/01/23 dadams
;;     isearch-message: For Emacs 22+ only.
;; 2006/12/12 dadams
;;     Added isearch-toggle-word (from Juri Linkov), and bound it.
;; 2006/10/28 dadams
;;     Added: isearch-fail, isearch-message (redefinition).
;; 2006/07/30 dadams
;;     Added: set-region-around-search-target.
;; 2006/07/29 dadams
;;     Added: isearchp-toggle-set-region,isearchp-set-region(-flag). Thx to Andreas Roehler
;; 2006/01/24 dadams
;;     On MS Windows, bind isearch-complete* to C-tab.
;; 1999/03/17 dadams
;;     Updated to corrspond to Emacs 34.1 version.
;; 1996/04/24 dadams
;;     Added redefinition of isearch-search.  Require cl.el.
;; 1995/12/28 dadams
;;     Changed isearch-edit-string binding.
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

 ;; Cannot do (require 'isearch), because `isearch.el' does no `provide'.
 ;; Don't want to do a (load-library "isearch") either, because it wouldn't
 ;; allow doing (eval-after-load "isearch" '(progn (require 'isearch+)))

(require 'misc-cmds nil t) ;; goto-longest-line

;;;;;;;;;;;;;;;;;;;;;;;;;

(when (> emacs-major-version 21)        ; Emacs 22
  (defface isearch-fail
      '((((class color) (min-colors 88) (background dark))
         (:foreground "white" :background "#22225F5F2222")) ; a dark green
        (((class color) (min-colors 88) (background light))
         (:foreground "Black" :background "Plum"))
        (((class color) (min-colors 8)) (:background "red"))
        (((type tty) (class mono)) :inverse-video t)
        (t :background "gray"))
    "Face for highlighting failed part in Isearch echo-area message."
    :group 'isearch))

(defcustom isearchp-set-region-flag nil
  "Non-nil means set region around search target.
This is used only for Transient Mark mode.
You can toggle this with `isearchp-toggle-set-region', bound to
`C-SPC' during isearch."
  :type 'boolean :group 'isearch)

(add-hook 'isearch-mode-end-hook 'isearchp-set-region)


(add-hook 'isearch-mode-hook
          (lambda ()
            (define-key isearch-mode-map [(control ? )] 'isearchp-toggle-set-region)
            (define-key isearch-mode-map "\C-h"         'isearch-mode-help)
            (define-key isearch-mode-map "\C-t"         'isearch-toggle-regexp)
            (define-key isearch-mode-map "\C-c"         'isearch-toggle-case-fold)
            ;; This one is needed only for Emacs 20.  It is automatic after release 20.
            (define-key isearch-mode-map "\M-e"         'isearch-edit-string)
            (define-key isearch-mode-map "\M-w"         'isearch-toggle-word)
            (when (and (fboundp 'goto-longest-line) window-system) ; Defined in `misc-cmds.el'
              (define-key isearch-mode-map [(control end)] 'goto-longest-line))
            (define-key isearch-mode-map [next]         'isearch-repeat-forward)
            (define-key isearch-mode-map [prior]        'isearch-repeat-backward)
            (define-key minibuffer-local-isearch-map "\M-e" 'isearchp-goto-success-end)
            (when (and (eq system-type 'windows-nt) ; Windows uses M-TAB for something else.
                       (not (lookup-key isearch-mode-map [C-M-tab])))
              (define-key isearch-mode-map [C-M-tab] 'isearch-complete))
            (when (and (eq system-type 'windows-nt) ; Windows uses M-TAB for something else.
                       (not (lookup-key minibuffer-local-isearch-map [C-M-tab])))
              (define-key minibuffer-local-isearch-map [C-M-tab] 'isearch-complete-edit))))

(defun isearchp-set-region ()
  "Set the region around the search target, if `isearchp-set-region-flag'.
This is used only for Transient Mark mode."
  (when (and isearchp-set-region-flag transient-mark-mode)
    (push-mark isearch-other-end t 'activate)))

(defun isearchp-toggle-set-region ()
  "Toggle `isearchp-set-region-flag'."
  (interactive)
  (setq isearchp-set-region-flag (not isearchp-set-region-flag))
  (if isearchp-set-region-flag
      (message "Setting region around search target is now ON")
    (message "Setting region around search target is now OFF")))

(defun set-region-around-search-target ()
  "Set the region around the last search or query-replace target."
  (interactive)
  (case last-command
    ((isearch-forward isearch-backward isearch-forward-regexp isearch-backward-regexp)
     (push-mark isearch-other-end t 'activate))
    (t (push-mark (match-beginning 0) t 'activate)))
  (setq deactivate-mark nil))

;; From Juri Linkov, 2006-10-29, to emacs-devel@gnu.org
;; From Stefan Monnier, 2006-11-23, to help-gnu-emacs@gnu.org
(unless (fboundp 'isearch-toggle-word)
  (defun isearch-toggle-word ()
    "Toggle word searching on or off."
    ;; The status stack is left unchanged.
    (interactive)
    (setq isearch-word (not isearch-word))
    (when isearch-word (setq isearch-regexp nil)) ; Added to Juri's code by Stefan.
    (setq isearch-success t isearch-adjusted t)
    (isearch-update)))



;; An alternative to binding `isearch-edit-string' (but less flexible):
;; (setq search-exit-option 'edit) ; M- = edit search string, not exit.


;; REPLACES ORIGINAL in `isearch.el':
;; 1. Ends isearch: does `isearch-done' and `isearch-clean-overlays'
;;    instead of `isearch-update'.
;; 2. Lists isearch bindings too.
;;;###autoload
(defun isearch-mode-help ()
  "Display information on interactive search in buffer *Help*."
  (interactive)
  (describe-function 'isearch-forward)
  (isearch-done)
  (isearch-clean-overlays)
  (save-excursion
    (set-buffer "*Help*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert (substitute-command-keys "

Bindings in Isearch minor mode:
------------------------------

\\{isearch-mode-map}")))))



;; REPLACES ORIGINAL in `isearch.el':
;; Highlights failed part of search string in echo area, in face `isearch-fail'.
;;
;; (when (> emacs-major-version 21)        ; Emacs 22.
;;   (defun isearch-message (&optional c-q-hack ellipsis)
;;     ;; Generate and print the message string.
;;     (let ((cursor-in-echo-area ellipsis)
;;           (cmds isearch-cmds)
;;           succ-msg m)
;;       (while (not (isearch-success-state (car cmds))) (pop cmds))
;;       (setq succ-msg (if (equal (isearch-message-state (car isearch-cmds)) isearch-message)
;;                          (and cmds (isearch-message-state (car cmds)))
;;                        isearch-message))
;;       (setq m (concat
;;                (isearch-message-prefix c-q-hack ellipsis isearch-nonincremental)
;;                succ-msg
;;                (and (not isearch-success)
;;                     (string-match (regexp-quote succ-msg) isearch-message)
;;                     (not (string= succ-msg isearch-message))
;;                     (propertize (substring isearch-message (match-end 0))
;;                                 'face 'isearch-fail))))
;;       (when (and (not isearch-success) (string-match " +$" m))
;;         (put-text-property (match-beginning 0) (length m) 'face 'trailing-whitespace m))
;;       (setq m (concat m (isearch-message-suffix c-q-hack ellipsis)))
;;       (if c-q-hack m (let ((message-log-max nil)) (message "%s" m))))))



(defvar isearch-error)                  ; Quite the byte-compiler.


;; REPLACES ORIGINAL in `isearch.el':
;; Highlights failed part of search string in echo area, in face `isearch-fail'.
;;
(when (> emacs-major-version 21)        ; Emacs 22.
  (defun isearch-message (&optional c-q-hack ellipsis)
    ;; Generate and print the message string.
    (let ((cursor-in-echo-area ellipsis)
          (m isearch-message)
          (cmds isearch-cmds)
          succ-msg)
      (when (or (not isearch-success) isearch-error)
        (while (or (not (isearch-success-state (car cmds))) (isearch-error-state (car cmds)))
          (pop cmds))
        (setq succ-msg  (and cmds (isearch-message-state (car cmds)))
              m         (copy-sequence m))
        (when (and (stringp succ-msg) ; Highlight failed part of input.
                   (< (length succ-msg) (length m)))
          (add-text-properties (length succ-msg) (length m) '(face isearch-fail) m))
        (when (string-match " +$" m)  ; Highlight trailing whitespace.
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(face trailing-whitespace) m)))
      (setq m (concat (isearch-message-prefix c-q-hack ellipsis isearch-nonincremental)
                      m
                      (isearch-message-suffix c-q-hack ellipsis)))
      (if c-q-hack m (let ((message-log-max nil)) (message "%s" m))))))

(when (fboundp 'isearch-success-state)  ; Emacs 22.
  (defun isearchp-goto-success-end ()   ; `M-e' in `minibuffer-local-isearch-map'.
    "Go to end of search string text that matches."
    (interactive)
    (goto-char (point-max))
    (let ((cmds  isearch-cmds)
          succ-msg)
      (when (or (not isearch-success) isearch-error)
        (while (or (not (isearch-success-state (car cmds))) (isearch-error-state (car cmds)))
          (pop cmds))
        (setq succ-msg  (and cmds (isearch-message-state (car cmds))))
        (backward-char (- (length isearch-string) (length succ-msg)))))))


;;;(require 'cl) ;; when, unless, cadr

;;;;; REPLACES ORIGINAL in `isearch.el':
;;;;; 1. Prevent null `isearch-string' from giving wrong-type-arg error.
;;;;;    This fixes a bug: C-M-s M-p C-s with no previous regexp search.
;;;;; 2. The general `error' handler shows the whole error message to
;;;;;    user (in `isearch-invalid-regexp').  The original version showed
;;;;;    just (cadr lossage).
;;;;;;###autoload
;;;(defun isearch-search ()
;;;  ;; Do the search with the current search string.
;;;  (isearch-message nil t)
;;;  (when (and (eq isearch-case-fold-search t) search-upper-case)
;;;    (setq isearch-case-fold-search
;;;          (isearch-no-upper-case-p isearch-string isearch-regexp)))
;;;  (condition-case lossage
;;;      (let ((inhibit-quit nil)
;;;         (case-fold-search isearch-case-fold-search))
;;;     (when isearch-regexp (setq isearch-invalid-regexp nil))
;;;     (setq isearch-within-brackets nil)
;;;        ;; Prevent a null isearch-string from giving a wrong-type-arg error.
;;;        (setq isearch-string (or isearch-string ""))
;;;     (setq isearch-success
;;;           (funcall (cond (isearch-word (if isearch-forward
;;;                                               'word-search-forward
;;;                                             'word-search-backward))
;;;                             (isearch-regexp (if isearch-forward
;;;                                                 're-search-forward
;;;                                               're-search-backward))
;;;                             (t (if isearch-forward 'search-forward
;;;                                  'search-backward)))
;;;                       isearch-string nil t))
;;;     (setq isearch-just-started nil)
;;;     (when isearch-success (setq isearch-other-end (if isearch-forward
;;;                                                          (match-beginning 0)
;;;                                                        (match-end 0)))))
;;;    (quit (isearch-unread ?\C-g) (setq isearch-success nil))
;;;    (invalid-regexp
;;;     (setq isearch-invalid-regexp (cadr lossage))
;;;     (setq isearch-within-brackets (string-match "\\`Unmatched \\["
;;;                                              isearch-invalid-regexp))
;;;     (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
;;;                         isearch-invalid-regexp)
;;;       (setq isearch-invalid-regexp "incomplete input")))
;;;    ;; Stack overflow in regexp search.  (original comment)
;;;    ;; The original version set `isearch-invalid-regexp' to: (cadr lossage).
;;;    ;; But this handler catches all errors, not just regexp stack overflow,
;;;    ;; so it may be best to show the whole error message to the user (in
;;;    ;; `isearch-invalid-regexp').
;;;    (error (setq isearch-invalid-regexp (format "%s" lossage))))
;;;  (unless isearch-success
;;;    ;; Ding if failed this time after succeeding last time.
;;;    (when (nth 3 (car isearch-cmds)) (ding))
;;;    (goto-char (nth 2 (car isearch-cmds)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'isearch+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch+.el ends here
