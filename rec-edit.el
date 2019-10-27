;;; rec-edit.el --- Recursive-edit enhancements. -*- lexical-binding:t -*-
;;
;; Filename: rec-edit.el
;; Description: Recursive-edit enhancements: `rec-edit-mode'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2019, Drew Adams, all rights reserved.
;; Created: Sun Oct 27 08:27:03 2019 (-0700)
;; Version: 2019.10.27
;; Package-Requires: ()
;; Last-Updated: Sun Oct 27 14:27:19 2019 (-0700)
;;           By: dradams
;;     Update #: 76
;; URL: https://www.emacswiki.org/emacs/download/rec-edit.el
;; Doc URL: https://www.emacswiki.org/emacs/RecursiveEdit#rec-edit.el
;; Keywords: recursive edit, mode-line
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Recursive-edit enhancements using minor mode `rec-edit-mode'.
;;
;;  Recursive editing is sometimes useful.  Its main disadvantage is
;;  not being aware that you've entered or exited a recursive edit.
;;
;;  This library makes it easier to use in these ways:
;;
;;   * Giving you a single key to enter and exit
;;
;;   * Optionally highlighting the mode-line indication, `[...]', to
;;     make clear that you are in a recursive edit.
;;
;;  The indicator highlighting uses four faces, successively, to help
;;  you see the current recursion depth: `red-modeline-1' through
;;  `red-modeline-4' for depths 1 through 4, then 5 through 8, etc.
;;
;;  Global minor mode defines key `C-M-c' (normally bound to
;;  `exit-recursive-edit') to command `red-recursive-edit', which
;;  invokes `recursive-edit' when at top level or when used with
;;  prefix arg.  Otherwise, it invokes `exit-recursive-edit'.
;;
;;  Non-nil option `red-highlight-modeline-flag' means that the
;;  mode-line indication of recursive editing (`[...]') is
;;  highlighted, with 4 different faces for different recursion
;;  levels.
;;
;;  Internal constant `red-ORIG-mode-line-modes' holds the original
;;  value of `mode-line-modes', that is, the value that is current
;;  when you load this library.  This is used to restore the default
;;  behavior when you exit `rec-edit-mode'.
;;
;;  (Nothing should ever change constant `red-ORIG-mode-line-modes'.
;;  But if it should get changed somehow you can always reset it to
;;  the initial value given to `mode-line-modes' in standard library
;;  `bindings.el'.)
;;
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Commands:
;;
;;    `rec-edit-mode', `red-recursive-edit'.
;;
;;  User option:
;;
;;    `red-highlight-modeline-flag'.
;;
;;  Faces:
;;
;;   `red-modeline-1', `red-modeline-2', `red-modeline-3',
;;   `red-modeline-4'.
;;
;;  Non-interactive functions:
;;
;;   `red-highlight-modeline', `red-unhighlight-modeline'.
;;
;;  Internal constant:
;;
;;    `red-ORIG-mode-line-modes'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2019/10/27 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;###autoload
(defgroup rec-edit nil
  "recursive-edit enhancements."
  :prefix "red-"
  :group 'editing-basics :group 'Modeline :group 'Convenience :group 'Help
  :link '(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
rec-edit.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download"
                   "https://www.emacswiki.org/emacs/download/rec-edit.el")
;;; @@@@@@
;;;   :link '(url-link :tag "Description"
;;;           "https://www.emacswiki.org/emacs/ChangingCursorDynamically")
  :link '(emacs-commentary-link :tag "Commentary" "rec-edit"))


(unless (boundp 'red-ORIG-mode-line-modes)
  (defconst red-ORIG-mode-line-modes mode-line-modes
    "Value of `mode-line-modes' current at first `rec-edit.el' load."))

(defface red-modeline-1 '((t (:foreground "Magenta" :background "Cyan")))
  "Face for mode-line recursive-edit indication level 1: [...].
Used also for levels 5, 9, 13, etc."
  :group 'rec-edit)

(defface red-modeline-2 '((t (:foreground "Yellow" :background "Red")))
  "Face for mode-line recursive-edit indication level 2: [[...]].
Used also for levels 6, 10, 14, etc."
  :group 'rec-edit)

(defface red-modeline-3 '((t (:foreground "Red" :background "Green")))
  "Face for mode-line recursive-edit indication level 3: [[[...]]].
Used also for levels 7, 11, 15, etc."
  :group 'rec-edit)

(defface red-modeline-4 '((t (:foreground "Cyan" :background "Magenta")))
  "Face for mode-line recursive-edit indication level 4: [[[[...]]]].
Used also for levels 8, 12, 16, etc."
  :group 'rec-edit)

(defvar rec-edit-mode-map (let ((map  (make-sparse-keymap)))
                            (define-key map (kbd "C-M-c") 'red-recursive-edit)
                            map)
  "Keymap for minor mode `rec-edit-mode'.")

(defun red-recursive-edit (arg)
  "DWIM command for `recursive-edit'.
If at top level or with a prefix arg (ARG), this is `recursive-edit'.
Otherwise, this is `exit-recursive-edit'."
  (interactive "P")
  (if (or (< (recursion-depth) 1)  arg) (recursive-edit) (exit-recursive-edit)))

(defcustom red-highlight-modeline-flag t
  "Non-nil means highlight the mode-line recursive-edit indication.
Faces `red-modeline-1' through `red-modeline-4' are used, in order.
The face sequence is repeated for recursion depths greater than 4."
  :group 'rec-edit :type 'boolean)

;;;###autoload
(define-minor-mode rec-edit-mode
  "Toggle recursive-edit enhancements.
When on, `C-M-c' can either enter or exit a recursive edit.
Entering means invoke `recursive-edit'.  Exiting means invoke
`exit-recursive-edit'.

 At top level, it toggles between entering and exiting.
 At deeper levels:
  * With no prefix arg, it exits.
  * With a prefix arg, it enters.

Non-nil option `red-highlight-modeline-flag' highlights the mode-line
recursive-edit indication."
  :init-value nil :global t
  :keymap rec-edit-mode-map :group 'rec-edit :require 'rec-edit
  :link '(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
rec-edit.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download"
                   "https://www.emacswiki.org/emacs/download/rec-edit.el")
;;; @@@@@@
;;;   :link '(url-link :tag "Description"
;;;           "https://www.emacswiki.org/emacs/ChangingCursorDynamically")
  :link '(emacs-commentary-link :tag "Commentary" "rec-edit")
  (if rec-edit-mode
      (when red-highlight-modeline-flag (red-highlight-modeline))
    (when red-highlight-modeline-flag (red-unhighlight-modeline)))
  (run-hooks 'rec-edit-mode-hook))

(defun red-highlight-modeline ()
  "Turn on highlighting of mode-line recursive-edit indication.
Highlighting uses one of the faces `red-modeline-1' through
`red-modeline-4', depending on the recursion depth.  The face sequence
`-1' through `-4' is repeated for recursion depths greater than 4."
  (setq-default mode-line-modes
                '(:eval
                  (let* ((modes  (copy-tree red-ORIG-mode-line-modes))
                         (last   (last modes 2))
                         (depth  (recursion-depth))
                         (dep    (1+ (% (1- depth) 4)))
                         (face   (if (> depth 0)
                                     (intern (concat "red-modeline-"
                                                     (number-to-string dep)))
                                   'default)))
                    (setcar modes
                            (propertize (car modes) 'face face))
                    (setcar (last modes 2)
                            (propertize (car (last modes 2)) 'face face))
                    modes))))

(defun red-unhighlight-modeline ()
  "Turn off highlighting of mode-line recursive-edit indication.
This works by resetting `mode-line-modes' to the value of variable
`red-ORIG-mode-line-modes', which is set when you load library
`rec-edit.el'."
  (setq-default mode-line-modes  red-ORIG-mode-line-modes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'rec-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rec-edit.el ends here
