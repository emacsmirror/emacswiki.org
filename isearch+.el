;;; isearch+.el --- Extensions to `isearch.el'.
;;
;; Filename: isearch+.el
;; Description: Extensions to `isearch.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2011, Drew Adams, all rights reserved.
;; Created: Fri Dec 15 10:44:14 1995
;; Version: 21.0
;; Last-Updated: Tue May 17 08:08:45 2011 (-0700)
;;           By: dradams
;;     Update #: 599
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
;;    `isearchp-toggle-invisible',
;;    `isearchp-toggle-regexp-quote-yank',
;;    `isearchp-toggle-set-region', `isearch-toggle-word',
;;    `isearchp-yank-sexp-symbol-or-char',
;;    `isearchp-sexp-symbol-or-char',
;;    `set-region-around-search-target'.
;;
;;  User options defined here:
;;
;;    `isearchp-regexp-quote-yank-flag', `isearchp-set-region-flag'.
;;
;;  Faces defined here:
;;
;;    `isearch-fail'.
;;
;;  Non-interactive functions defined here:
;;
;;    `isearchp-fail-pos', `isearchp-set-region'.
;;
;;  Internal variables defined here:
;;
;;    `isearchp-last-non-nil-invisible'.
;;
;;
;;  ***** NOTE: The following functions defined in `isearch.el' have
;;              been REDEFINED HERE:
;;
;;  `isearch-edit-string' - Put point at mismatch position.
;;  `isearch-mode-help'   - End isearch.  List bindings.
;;  `isearch-message'     - Highlight failed part of search string in
;;                          echo area, in face `isearch-fail'.
;;  `isearch-yank-string' - Respect `isearchp-regexp-quote-yank-flag'
;;
;;
;;  The following bindings are made here for incremental search mode
;;  (`C-s' prefix):
;;
;;    `C-`'        `isearchp-toggle-regexp-quote-yank'
;;    `C-+'        `isearchp-toggle-invisible'
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
;;    `C-M-tab'    `isearch-complete-edit' (MS Windows only)
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `isearch.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "isearch" '(require 'isearch+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/05/16 dadams
;;     Added: isearchp-fail-pos, redefinition of isearch-edit-string.
;;     Removed: isearchp-goto-success-end (not needed - go there by default now).
;; 2011/01/04 dadams
;;     Added autoload cookies for defcustom and commands.
;; 2010/12/05 dadams
;;     Added: isearchp-toggle-invisible, isearchp-last-non-nil-invisible.
;; 2010/10/18 dadams
;;     isearch-mode-hook: Protect isearchp-goto-success-end with fboundp.
;; 2010/06/23 dadams
;;     Added: isearchp-yank(-sexp)-symbol-or-char.  Bound to C-_, C-(.
;; 2010/04/22 dadams
;;     Added: isearchp-toggle-regexp-quote-yank, isearchp-regexp-quote-yank-flag,
;;            isearch-yank-string (redefinition).
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


;; Quiet the byte compiler.
(defvar subword-mode)
(defvar isearch-error)                  ; Defined in `isearch.el'.
(defvar isearch-original-minibuffer-message-timeout) ; Defined in `isearch.el'.

;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar isearchp-last-non-nil-invisible (or search-invisible 'open)
  "Last non-nil value of `search-invisible'.")

(when (> emacs-major-version 21)        ; Emacs 22
  (defface isearch-fail
      '((((class color) (min-colors 88) (background dark))
         (:foreground "white" :background "#22225F5F2222")) ; a dark green
        (((class color) (min-colors 88) (background light))
         (:foreground "Black" :background "Plum"))
        (((class color) (min-colors 8)) (:background "red"))
        (((type tty) (class mono)) :inverse-video t)
        (t :background "gray"))
    "*Face for highlighting failed part in Isearch echo-area message."
    :group 'isearch))

;;;###autoload
(defcustom isearchp-regexp-quote-yank-flag t
  "*Non-nil means escape special chars in text yanked for a regexp isearch.
You can toggle this with `isearchp-toggle-regexp-quote-yank', bound to
`C-`' during isearch."
  :type 'boolean :group 'isearch)

;;;###autoload
(defcustom isearchp-set-region-flag nil
  "*Non-nil means set region around search target.
This is used only for Transient Mark mode.
You can toggle this with `isearchp-toggle-set-region', bound to
`C-SPC' during isearch."
  :type 'boolean :group 'isearch)

(add-hook 'isearch-mode-end-hook 'isearchp-set-region)


(add-hook 'isearch-mode-hook
          (lambda ()
            (define-key isearch-mode-map [(control ?+)] 'isearchp-toggle-invisible)
            (define-key isearch-mode-map [(control ?`)] 'isearchp-toggle-regexp-quote-yank)
            (define-key isearch-mode-map [(control ? )] 'isearchp-toggle-set-region)
            (define-key isearch-mode-map "\C-h"         'isearch-mode-help)
            (define-key isearch-mode-map "\C-t"         'isearch-toggle-regexp)
            (define-key isearch-mode-map "\C-c"         'isearch-toggle-case-fold)
            ;; This one is needed only for Emacs 20.  It is automatic after release 20.
            (define-key isearch-mode-map "\M-e"         'isearch-edit-string)
            (define-key isearch-mode-map "\M-w"         'isearch-toggle-word)
            (when (fboundp 'isearch-yank-internal)
              (define-key isearch-mode-map "\C-_"       'isearchp-yank-symbol-or-char)
              (define-key isearch-mode-map [(control ?\()]
                'isearchp-yank-sexp-symbol-or-char))
            (when (and (fboundp 'goto-longest-line) window-system) ; Defined in `misc-cmds.el'
              (define-key isearch-mode-map [(control end)] 'goto-longest-line))
            (define-key isearch-mode-map [next]         'isearch-repeat-forward)
            (define-key isearch-mode-map [prior]        'isearch-repeat-backward)
            (when (and (eq system-type 'windows-nt) ; Windows uses M-TAB for something else.
                       (not (lookup-key isearch-mode-map [C-M-tab])))
              (define-key isearch-mode-map [C-M-tab] 'isearch-complete))
            (when (and (eq system-type 'windows-nt) ; Windows uses M-TAB for something else.
                       (not (lookup-key minibuffer-local-isearch-map [C-M-tab])))
              (define-key minibuffer-local-isearch-map [C-M-tab] 'isearch-complete-edit))))

;;;###autoload
(defun isearchp-toggle-invisible ()
  "Toggle `search-invisible'."
  (interactive)
  (when search-invisible (setq isearchp-last-non-nil-invisible  search-invisible))
  (setq search-invisible  (if search-invisible nil isearchp-last-non-nil-invisible))
  (if search-invisible
      (message "Searching invisible text is now ON")
    (message "Searching invisible text is now OFF")))

;;;###autoload
(defun isearchp-toggle-regexp-quote-yank ()
  "Toggle `isearchp-regexp-quote-yank-flag'."
  (interactive)
  (setq isearchp-regexp-quote-yank-flag (not isearchp-regexp-quote-yank-flag))
  (if isearchp-regexp-quote-yank-flag
      (message "Escaping regexp special chars for yank is now ON")
    (message "Escaping regexp special chars for yank is now OFF")))

(defun isearchp-set-region ()
  "Set the region around the search target, if `isearchp-set-region-flag'.
This is used only for Transient Mark mode."
  (when (and isearchp-set-region-flag transient-mark-mode)
    (push-mark isearch-other-end t 'activate)))

;;;###autoload
(defun isearchp-toggle-set-region ()
  "Toggle `isearchp-set-region-flag'."
  (interactive)
  (setq isearchp-set-region-flag (not isearchp-set-region-flag))
  (if isearchp-set-region-flag
      (message "Setting region around search target is now ON")
    (message "Setting region around search target is now OFF")))

;;;###autoload
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


;; REPLACE ORIGINAL in `isearch.el'.
;;
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


;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Respect `isearchp-regexp-quote-yank-flag'.
;;
(defun isearch-yank-string (string)
  "Yank STRING into Isearch search string."
  ;; Downcase the string if not supposed to case-fold yanked strings.
  (if (and isearch-case-fold-search
	   (eq 'not-yanks search-upper-case))
      (setq string (downcase string)))
  (when (and isearch-regexp isearchp-regexp-quote-yank-flag)
    (setq string (regexp-quote string)))
  (setq isearch-string (concat isearch-string string)
	isearch-message
	(concat isearch-message
		(mapconcat 'isearch-text-char-description
			   string ""))
	;; Don't move cursor in reverse search.
	isearch-yank-flag t)
  (isearch-search-and-update))

(when (fboundp 'isearch-yank-internal) ; Emacs 22+
  (defun isearchp-yank-symbol-or-char ()
    "Yank char, subword, word, or symbol from buffer into search string."
    (interactive)
    (isearch-yank-internal
     (lambda ()
       (if (or (memq (char-syntax (or (char-after) 0)) '(?w ?_))
               (memq (char-syntax (or (char-after (1+ (point))) 0)) '(?w ?_)))
           (if (and (boundp 'subword-mode) subword-mode)
               (subword-forward 1)
             (forward-symbol 1))
         (forward-char 1))
       (point)))))

(when (fboundp 'isearch-yank-internal)  ; Emacs 22+
  (defun isearchp-yank-sexp-symbol-or-char ()
    "Yank sexp, symbol, or char from buffer into search string."
    (interactive)
    (isearch-yank-internal
     (lambda ()
       (if (or (= (char-syntax (or (char-after) 0)) ?\( )
               (= (char-syntax (or (char-after (1+ (point))) 0)) ?\( ))
           (forward-sexp 1)
         (if (or (memq (char-syntax (or (char-after) 0)) '(?w ?_))
                 (memq (char-syntax (or (char-after (1+ (point))) 0)) '(?w ?_)))
             (if (and (boundp 'subword-mode) subword-mode)
                 (subword-forward 1)
               (forward-symbol 1))
           (forward-char 1)))
       (point)))))


;; $$$$$$
;; (when (> emacs-major-version 21)        ; Emacs 22+
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



;; REPLACE ORIGINAL in `isearch.el'.
;;
;; Highlight failed part of search string in echo area, in face `isearch-fail'.
;;
(when (> emacs-major-version 21)        ; Emacs 22+
  (defun isearch-message (&optional c-q-hack ellipsis)
    ;; Generate and print the message string.
    (let ((cursor-in-echo-area  ellipsis)
          (msg                  isearch-message)
          (cmds                 isearch-cmds)
          succ-msg)
      (when (or (not isearch-success) isearch-error)
        (while (or (not (isearch-success-state (car cmds))) (isearch-error-state (car cmds)))
          (pop cmds))
        (setq succ-msg  (and cmds (isearch-message-state (car cmds)))
              msg       (copy-sequence msg))
        (when (and (stringp succ-msg)   ; Highlight failed part of input.
                   (< (length succ-msg) (length msg)))
          (add-text-properties (length succ-msg) (length msg) '(face isearch-fail) msg))
        (when (string-match " +$" msg)  ; Highlight trailing whitespace.
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(face trailing-whitespace) msg)))
      (setq msg  (concat (isearch-message-prefix c-q-hack ellipsis isearch-nonincremental)
                         msg
                         (isearch-message-suffix c-q-hack ellipsis)))
      (if c-q-hack msg (let ((message-log-max  nil)) (message "%s" msg)))))

;;; $$$$$$ No longer used.  `M-e' puts point at this position automatically.
;;;   (defun isearchp-goto-success-end ()   ; `M-e' in `minibuffer-local-isearch-map'.
;;;     "Go to end of search string text that matches."
;;;     (interactive)
;;;     (goto-char (point-max))
;;;     (let ((cmds  isearch-cmds)
;;;           succ-msg)
;;;       (when (or (not isearch-success) isearch-error)
;;;         (while (or (not (isearch-success-state (car cmds))) (isearch-error-state (car cmds)))
;;;           (pop cmds))
;;;         (setq succ-msg  (and cmds (isearch-message-state (car cmds))))
;;;         (backward-char (- (length isearch-string) (length succ-msg)))))))

  (defun isearchp-fail-pos ()
    "Position of first mismatch in search string, or its length if none."
    (let ((cmds  isearch-cmds)
          succ-msg)
      (if (and isearch-success (not isearch-error))
          (length isearch-message)
        (while (or (not (isearch-success-state (car cmds)))
                   (isearch-error-state (car cmds)))
          (pop cmds))
        (setq succ-msg  (and cmds (isearch-message-state (car cmds))))
        (if (and (stringp succ-msg)  (< (length succ-msg) (length isearch-message))
                 (equal succ-msg (substring isearch-message 0 (length succ-msg))))
            (length succ-msg)
          0))))


  ;; REPLACE ORIGINAL in `isearch.el'.
  ;;
  ;; Start with point at the mismatch position.
  ;;
  (defun isearch-edit-string ()
    "Edit the search string in the minibuffer.
The following additional command keys are active while editing.
\\<minibuffer-local-isearch-map>
\\[exit-minibuffer] to resume incremental searching with the edited string.
\\[isearch-nonincremental-exit-minibuffer] to do one nonincremental search.
\\[isearch-forward-exit-minibuffer] to resume isearching forward.
\\[isearch-reverse-exit-minibuffer] to resume isearching backward.
\\[isearch-complete-edit] to complete the search string using the search ring.
\\<isearch-mode-map>
If first char entered is \\[isearch-yank-word-or-char], then do word search instead."
    ;; This code is very hairy for several reasons, explained in the code.
    ;; Mainly, isearch-mode must be terminated while editing and then restarted.
    ;; If there were a way to catch any change of buffer from the minibuffer,
    ;; this could be simplified greatly.
    ;; Editing doesn't back up the search point.  Should it?
    (interactive)
    (condition-case nil
        (progn
          (let ((isearch-nonincremental isearch-nonincremental)
                ;; Locally bind all isearch global variables to protect them
                ;; from recursive isearching.
                ;; isearch-string -message and -forward are not bound
                ;; so they may be changed.  Instead, save the values.
                (isearch-new-string isearch-string)
                (isearch-new-message isearch-message)
                (isearch-new-forward isearch-forward)
                (isearch-new-word isearch-word)

                (isearch-regexp isearch-regexp)
                (isearch-op-fun isearch-op-fun)
                (isearch-cmds isearch-cmds)
                (isearch-success isearch-success)
                (isearch-wrapped isearch-wrapped)
                (isearch-barrier isearch-barrier)
                (isearch-adjusted isearch-adjusted)
                (isearch-yank-flag isearch-yank-flag)
                (isearch-error isearch-error)
  ;;; Don't bind this.  We want isearch-search, below, to set it.
  ;;; And the old value won't matter after that.
  ;;;	    (isearch-other-end isearch-other-end)
  ;;; Perhaps some of these other variables should be bound for a
  ;;; shorter period, ending before the next isearch-search.
  ;;; But there doesn't seem to be a real bug, so let's not risk it now.
                (isearch-opoint isearch-opoint)
                (isearch-slow-terminal-mode isearch-slow-terminal-mode)
                (isearch-small-window isearch-small-window)
                (isearch-recursive-edit isearch-recursive-edit)
                ;; Save current configuration so we can restore it here.
                (isearch-window-configuration (current-window-configuration))

                ;; Temporarily restore `minibuffer-message-timeout'.
                (minibuffer-message-timeout
                 isearch-original-minibuffer-message-timeout)
                (isearch-original-minibuffer-message-timeout
                 isearch-original-minibuffer-message-timeout)
                old-point old-other-end)

            ;; Actually terminate isearching until editing is done.
            ;; This is so that the user can do anything without failure,
            ;; like switch buffers and start another isearch, and return.
            (condition-case nil
                (isearch-done t t)
              (exit nil))               ; was recursive editing

            ;; Save old point and isearch-other-end before reading from minibuffer
            ;; that can change their values.
            (setq old-point (point) old-other-end isearch-other-end)

            (unwind-protect
                 (let* ((message-log-max nil)
                        ;; Binding minibuffer-history-symbol to nil is a work-around
                        ;; for some incompatibility with gmhist.
                        (minibuffer-history-symbol))
                   (setq isearch-new-string
                         (read-from-minibuffer
                          (isearch-message-prefix nil nil isearch-nonincremental)
                          (cons isearch-string (1+ (isearchp-fail-pos)))
                          minibuffer-local-isearch-map nil
                          (if isearch-regexp
                              (cons 'regexp-search-ring
                                    (1+ (or regexp-search-ring-yank-pointer -1)))
                            (cons 'search-ring
                                  (1+ (or search-ring-yank-pointer -1))))
                          nil t)
                         isearch-new-message
                         (mapconcat 'isearch-text-char-description
                                    isearch-new-string "")))

              ;; Set point at the start (end) of old match if forward (backward),
              ;; so after exiting minibuffer isearch resumes at the start (end)
              ;; of this match and can find it again.
              (if (and old-other-end (eq old-point (point))
                       (eq isearch-forward isearch-new-forward))
                  (goto-char old-other-end))

              ;; Always resume isearching by restarting it.
              (isearch-mode isearch-forward
                            isearch-regexp
                            isearch-op-fun
                            nil
                            isearch-word)

              ;; Copy new local values to isearch globals
              (setq isearch-string isearch-new-string
                    isearch-message isearch-new-message
                    isearch-forward isearch-new-forward
                    isearch-word isearch-new-word))

            ;; Empty isearch-string means use default.
            (if (= 0 (length isearch-string))
                (setq isearch-string (or (car (if isearch-regexp
                                                  regexp-search-ring
                                                search-ring))
                                         "")

                      isearch-message
                      (mapconcat 'isearch-text-char-description
                                 isearch-string ""))
              ;; This used to set the last search string,
              ;; but I think it is not right to do that here.
              ;; Only the string actually used should be saved.
              ))

          ;; This used to push the state as of before this C-s, but it adds
          ;; an inconsistent state where part of variables are from the
          ;; previous search (e.g. `isearch-success'), and part of variables
          ;; are just entered from the minibuffer (e.g. `isearch-string').
          ;; (isearch-push-state)

          ;; Reinvoke the pending search.
          (isearch-search)
          (isearch-push-state)          ; this pushes the correct state
          (isearch-update)
          (if isearch-nonincremental
              (progn
                ;; (sit-for 1) ;; needed if isearch-done does: (message "")
                (isearch-done)
                ;; The search done message is confusing when the string
                ;; is empty, so erase it.
                (if (equal isearch-string "")
                    (message "")))))

      (quit                             ; handle abort-recursive-edit
       (isearch-abort)))));; outside of let to restore outside global values

;;;(require 'cl) ;; when, unless, cadr

;;;;; REPLACE ORIGINAL in `isearch.el'.
;;;;;
;;;;; 1. Prevent null `isearch-string' from giving wrong-type-arg error.
;;;;;    This fixes a bug: C-M-s M-p C-s with no previous regexp search.
;;;;; 2. The general `error' handler shows the whole error message to
;;;;;    user (in `isearch-invalid-regexp').  The original version showed
;;;;;    just (cadr lossage).
;;;;;
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
