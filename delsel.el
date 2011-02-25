;;; delsel.el --- Delete the region (selection) upon char insertion or DEL.
;;
;; Filename: delsel.el
;; Description: Delete the region (selection) upon char insertion or DEL.
;; Author: Matthieu Devin <devin@lucid.com>, Drew Adams
;; Maintainer: D. ADAMS
;; Copyright (C) 1996-2011, Drew Adams, all rights reserved.
;; Copyright (C) 1992 Free Software Foundation, Inc.
;; Created: Fri Dec  1 13:51:31 1995
;; Version: 21.0
;; Last-Updated: Thu Feb 24 14:55:39 2011 (-0800)
;;           By: dradams
;;     Update #: 327
;; URL: http://www.emacswiki.org/cgi-bin/wiki/delsel.el
;; Keywords: abbrev, emulations, local, convenience
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Let DEL delete the region; let character insertion replace it.
;;
;;  Makes the active region be pending a deletion, meaning that text
;;  inserted while the region is active will replace the region
;;  contents, and that operations like `delete-backward-char' will
;;  delete the region.
;;
;;  `C-g' is bound here to `minibuffer-keyboard-quit' in each of the
;;  minibuffer-local-*-map's.
;;
;;
;;  Property `delete-selection':
;;
;;  Commands that delete the selection need a `delete-selection'
;;  property on their symbols. Commands that insert text but do not
;;  have this property do not delete the selection.  The property can
;;  be one of these values:

;;  `yank' - For commands that do a yank. Ensures that the region
;;           about to be deleted is not yanked.

;;  `supersede' - Delete the active region and ignore the current command:
;;           the command just deletes the region.
;;
;;  `kill' - `kill-region' is used on the selection, rather than
;;           `delete-region'.  Text selected with the mouse is
;;           typically yankable anyway.
;;
;;  anything else non-nil - Deletes the active region prior to
;;           executing the command, which inserts replacement
;;           text. This is the usual case.
;;
;;
;; The original author is Matthieu Devin <devin@lucid.com>.
;; This version was modified by Drew Adams.
;;
;; Main changes here from the original:
;; -----------------------------------
;;
;; 1. Added function `delete-selection-pre-hook-1'.  In fact,
;;    `delete-selection-pre-hook' was renamed to
;;    `delete-selection-pre-hook-1', and a new version of
;;    `delete-selection-pre-hook' was defined in terms of it.
;;    This allowed change #2 (next).
;; 2. Fixed bug: `completion.el' was making things like SPC and `.'
;;    lose on self insert here.
;; 3. Will now work in tandem with `completion.el':
;;    a. `delete-active-region': Deletes latest completion only.
;;       During completion, don't delete region when self-insert.
;;    b. `delete-selection-pre-hook': In case of completion, makes
;;       mark active.
;; 4. `minibuffer-keyboard-quit':
;;    Removes any windows showing *Completions* buffer.
;; 5. `delete-selection-mode': Informs user of new state.
;;
;; You might want to do something like the following in your .emacs:
;;
;;       (make-variable-buffer-local 'transient-mark-mode)
;;       (put 'transient-mark-mode 'permanent-local t)
;;       (setq-default transient-mark-mode t)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2011/01/03 dadams
;;     Added autoload cookie for command.
;; 2009/10/02 dadams
;;     minibuffer-keyboard-quit:
;;       Don't call delete-windows-on if no buffer.  Thx to Tetzlaff.
;; 2008/05/03 dadams
;;     delete-selection-pre-hook: defun, not defsubst.
;; 2007/09/04 dadams
;;     minibuffer-keyboard-quit: remove-windows-on -> delete-windows-on.
;;     Removed require of frame-cmds.el.
;; 2006/03/31 dadams
;;     No longer use display-in-minibuffer.
;; 2005/12/18 dadams
;;     Use minibuffer-prompt face.  Removed require of def-face-const.el.
;; 2005/12/03 dadams
;;     Removed require (soft) of completion.el.
;; 2004/11/12 dadams
;;     Changed so will work with either byte-compiler: Emacs 20 or 21.
;; 2004/09/21 dadams
;;     Updated to work with Emacs 21 (as well as Emacs 20).
;;     Must byte-compile each version separately, however.
;; 2004/08/09 dadams
;;     Some cleanup to agree more with Emacs 20 standard version.
;; 2001/01/10 dadams
;;     Protected remove-windows-on via fboundp.
;; 1999/03/17 dadams
;;     1. Updated to incorporate Emacs 34.1 version.
;;     2. Protected calls with test fboundp.
;; 1996/02/15 dadams
;;     delete-active-region: During completion, don't delete region
;;       when self-insert.
;; 1995/12/28 dadams
;;     Will now work in tandem with completion.el:
;;       delete-active-region: Deletes latest completion only.
;;       delete-selection-pre-hook: In case of completion, makes mark active.
;;       Added delete-selection-pre-hook.
;; 1995/12/08 dadams
;;     completion.el was making things like SPC and `.' lose on self insert here.
;;     (put 'completion-separator-self-insert-command 'delete-selection t)
;;     (put 'completion-separator-self-insert-autofilling 'delete-selection t)
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

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless


;;;;;;;;;;;;;;;;;;

;; Free variables here: CMPL-LAST-INSERT-LOCATION, CMPL-ORIGINAL-STRING,
;;                      COMPLETION-TO-ACCEPT
;; To quiet the byte compiler:
(defvar cmpl-last-insert-location -1)
(defvar cmpl-original-string nil)
(defvar completion-to-accept nil)

;;;;;;;;;;;;;;;;;;

;; This is defined in `faces.el', Emacs 22.  This definition is adapted to Emacs 20.
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "*Face for minibuffer prompts."
    :group 'basic-faces))


;;;###autoload
(defalias 'pending-delete-mode 'delete-selection-mode)

;; Macro `define-minor-mode' is not defined in Emacs 20, so in order
;; to be able to byte-compile this file in Emacs 20, prohibit
;; byte-compiling of the `define-minor-mode' call.
(if (fboundp 'define-minor-mode)

    ;; Emacs 21 ------------
    (eval '(define-minor-mode delete-selection-mode
            "Toggle Delete Selection mode.
With prefix ARG, turn Delete Selection mode on if and only if ARG is
positive.

When Delete Selection mode is enabled, Transient Mark mode is also
enabled and typed text replaces the selection if the selection is
active.  Otherwise, typed text is just inserted at point regardless of
any selection."
            :global t :group 'editing-basics
            (if (not delete-selection-mode)
                (remove-hook 'pre-command-hook 'delete-selection-pre-hook)
              (add-hook 'pre-command-hook 'delete-selection-pre-hook)
              (transient-mark-mode t))
            (when (interactive-p)
              (message "Delete Selection mode is now %s."
                       (if delete-selection-mode "ON" "OFF")))))

  ;; Emacs 20 ---------------
  (defun delete-selection-mode (&optional arg)
    "Toggle Delete Selection mode.
When ON, typed text replaces the selection if the selection is active,
and DEL deletes the selection.  When OFF, typed text is inserted at
point, as usual. Enabling Delete Selection mode also enables Transient
Mark mode.

Non-nil prefix ARG turns mode on if ARG is positive, else turns it off."
    (interactive "P")
    (setq delete-selection-mode (if arg
                                    (> (prefix-numeric-value arg) 0)
                                  (not delete-selection-mode)))
    (force-mode-line-update)
    (if (not delete-selection-mode)
        (remove-hook 'pre-command-hook 'delete-selection-pre-hook)
      (add-hook 'pre-command-hook 'delete-selection-pre-hook)
      (transient-mark-mode t))
    (when (interactive-p)
      (message "Delete Selection mode is now %s."
               (if delete-selection-mode "ON" "OFF")))))

;;;###autoload
(defcustom delete-selection-mode nil
  "*Non-nil means Delete Selection mode is enabled.
In this mode, when a region is highlighted, insertion commands first
delete the region, then insert. See command `delete-selection-mode'.
Setting this variable directly does not change the mode; instead, use
either \\[customize] or function `delete-selection-mode'."
  :set (lambda (symbol value) (delete-selection-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'editing-basics
  :require 'delsel)


;; CMPL-LAST-INSERT-LOCATION, CMPL-ORIGINAL-STRING and COMPLETION-TO-ACCEPT
;; are free here.
(defun delete-active-region (&optional killp)
  (cond ((and (eq last-command 'complete) ; See `completion.el'.
              (boundp 'cmpl-last-insert-location))
         ;; Don't delete region if a `self-insert-command'.
         ;; Delete it only if a supersede or a kill.
         (when (and (symbolp this-command)
                    (memq (get this-command 'delete-selection) '(supersede kill)))
           (delete-region (point) cmpl-last-insert-location) ; Free var here.
           (insert cmpl-original-string) ; Free var here.
           (setq completion-to-accept nil))) ; Free var here.
        (killp (kill-region (point) (mark)))
        (t (delete-region (point) (mark))))
  (deactivate-mark)
  t)

(defun delete-selection-pre-hook ()
  (if (and (eq last-command 'complete)  ; See `completion.el'.
           (boundp 'cmpl-last-insert-location))
      (let ((mark-active t)) (delete-selection-pre-hook-1))
    (delete-selection-pre-hook-1)))

(if (< emacs-major-version 21)
    (defun delete-selection-pre-hook-1 ()
      (when (and delete-selection-mode
                 (not buffer-read-only)
                 transient-mark-mode mark-active)
        (let ((type (and (symbolp this-command) (get this-command 'delete-selection))))
          (cond ((eq type 'kill)
                 (delete-active-region t))
                ((eq type 'yank)
                 ;; Before a yank command.  Make sure we don't yank the same
                 ;; region that we are going to delete.
                 ;; That would make yank a no-op.
                 (when (string= (buffer-substring-no-properties (point) (mark))
                                (car kill-ring))
                   (current-kill 1))
                 (delete-active-region))
                ((eq type 'supersede)
                 (delete-active-region)
                 (setq this-command 'ignore))
                (type (delete-active-region))))))
  (defun delete-selection-pre-hook-1 ()
    (when (and delete-selection-mode transient-mark-mode mark-active
               (not buffer-read-only))
      (let ((type (and (symbolp this-command)
                       (get this-command 'delete-selection))))
        (condition-case data
            (cond ((eq type 'kill)
                   (delete-active-region t))
                  ((eq type 'yank)
                   ;; Before a yank command,
                   ;; make sure we don't yank the same region
                   ;; that we are going to delete.
                   ;; That would make yank a no-op.
                   (when (string= (buffer-substring-no-properties (point) (mark))
                                  (car kill-ring))
                     (current-kill 1))
                   (delete-active-region))
                  ((eq type 'supersede)
                   (let ((empty-region (= (point) (mark))))
                     (delete-active-region)
                     (unless empty-region
                       (setq this-command 'ignore))))
                  (type
                   (delete-active-region)))
          (file-supersession
           ;; If ask-user-about-supersession-threat signals an error,
           ;; stop safe_run_hooks from clearing out pre-command-hook.
           (and (eq inhibit-quit 'pre-command-hook)
                (setq inhibit-quit 'delete-selection-dummy))
           (signal 'file-supersession (cdr data))))))))

(put 'self-insert-command 'delete-selection t)
(put 'self-insert-iso 'delete-selection t)

;; These are defined in `completion.el'.
(put 'completion-separator-self-insert-command 'delete-selection t)
(put 'completion-separator-self-insert-autofilling 'delete-selection t)

(put 'yank 'delete-selection 'yank)
(put 'clipboard-yank 'delete-selection 'yank)
(put 'insert-register 'delete-selection t)

(put 'delete-backward-char 'delete-selection 'supersede)
(put 'backward-delete-char-untabify 'delete-selection 'supersede)
(put 'delete-char 'delete-selection 'supersede)

(put 'newline-and-indent 'delete-selection t)
(put 'newline 'delete-selection t)
(put 'open-line 'delete-selection 'kill)

(put 'insert-parentheses 'delete-selection t)

;; This can be used to cancel a selection in the minibuffer without
;; aborting the minibuffer.
;;;###autoload
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key minibuffer-local-map "\C-g" 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map "\C-g" 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map "\C-g" 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map "\C-g" 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map "\C-g" 'minibuffer-keyboard-quit)

(unless (< emacs-major-version 21)
  (defun delsel-unload-hook ()
    (define-key minibuffer-local-map "\C-g" 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map "\C-g" 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map "\C-g" 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map "\C-g" 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map "\C-g" 'abort-recursive-edit)))

;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on Delete Selection mode if `delete-selection-mode' is non-nil
;; when this file is loaded.

(when delete-selection-mode (delete-selection-mode t))

(provide 'delsel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delsel.el ends here
