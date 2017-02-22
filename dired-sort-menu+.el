;;; dired-sort-menu+.el --- Extensions to `dired-sort-menu.el'
;;
;; Filename: dired-sort-menu+.el
;; Description: Extensions to `dired-sort-menu.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2005-2017, Drew Adams, all rights reserved.
;; Created: Thu Jul 07 12:39:36 2005
;; Version: 0
;; Package-Requires: ((dired-sort-menu "0"))
;; Last-Updated: Tue Feb 21 16:30:46 2017 (-0800)
;;           By: dradams
;;     Update #: 135
;; URL: https://www.emacswiki.org/emacs/download/dired-sort-menu%2b.el
;; Doc URL: http://emacswiki.org/DiredSortMenu
;; Keywords: directories, diredp, dired
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `dired', `dired-sort-menu', `easymenu', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `dired-sort-menu.el'
;;
;; Francis J. Wright <F.J.Wright@maths.qmw.ac.uk> wrote library
;; `dired-sort-menu.el' (http://centaur.maths.qmw.ac.uk/Emacs/).
;; Library `dired-sort-menu+.el' modifies `dired-sort-menu.el' to play
;; better with other libraries from Drew Adams.
;;
;; Changes:
;;
;;   1. The toggles for reverse sorting, `ls-lisp-ignore-case' and
;;      `ls-lisp-dirs-first', are bound respectively to "a", "c", and
;;      "W" in the dired map, instead of "r", "c" and "b".
;;
;;   2. We don't define `dired-sort-menu-toggle-ignore-case' and
;;      `dired-sort-menu-toggle-dirs-first' unless they can be used.
;;
;;   3. `handle-delete-frame' is protected against nil `buffer-name'.
;;
;;
;;  ***** NOTE: The following functions defined in `dired.el' have
;;              been REDEFINED or ADVISED HERE:
;;
;;  `dired-sort-dialogue' -
;;    1. Fit frame.  2. Do not add `dired-sort-dialogue-auto-kill-1'
;;    to `kill-buffer-hook'.
;;  `dired-sort-dialogue-close' - Just `kill-buffer'.
;;  `handle-delete-frame' - Do nothing if `buffer-name' is nil.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/06/18 dadams
;;     Updated T prefix-key bindings, because added more in dired+.el.
;; 2011/04/19 dadams
;;     Restore Dired+ bindings on prefix key T.
;; 2011/04/16 dadams
;;     handle-delete-frame:
;;       Fix for lexbind Emacs 24: replace named arg EVENT by (ad-get-arg 0).
;; 2005/11/05 dadams
;;     Renamed dired+ stuff to have diredp- prefix.
;; 2005/11/02 dadams
;;     Restore dired+ bindings messed up by dired-sort-menu.el.
;;     Changed dired-sort-menu-toggle-reverse to "|" and
;;       dired-sort-menu-toggle-dirs-first to "/".
;; 2005/07-26 dadams
;;     Protected ls-lisp-var-p with fboundp.
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

(require 'dired-sort-menu)              ; dired-sort-menu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Keys ----------------------------------

;; Restore bindings set by `dired+.el'.
;; (They were changed by `dired-sort-menu.el'.)
;; There should be a better way to do this, but probably there isn't.
;;
;; Replaces `T' binding for `dired-sort-menu-swap-config' in `dired-sort-menu.el'.
;;
(when (fboundp 'diredp-rename-this-file)
  (define-key dired-mode-map "b"       'diredp-byte-compile-this-file)
  (define-key dired-mode-map "r"       'diredp-rename-this-file)
  (define-key dired-mode-map "T"       nil) ; For Emacs20
  (define-key dired-mode-map "T+"      'diredp-tag-this-file) ; `T +'
  (define-key dired-mode-map "T-"      'diredp-untag-this-file) ; `T -'
  (define-key dired-mode-map "T0"      'diredp-remove-all-tags-this-file) ; `T 0'
  (define-key dired-mode-map "Tc"      'diredp-copy-tags-this-file) ; `T c'
  (define-key dired-mode-map "Tp"      'diredp-paste-add-tags-this-file) ; `T p'
  (define-key dired-mode-map "Tq"      'diredp-paste-replace-tags-this-file) ; `T q'
  (define-key dired-mode-map "Tv"      'diredp-set-tag-value-this-file) ; `T v'
  (define-key dired-mode-map "T\M-w"   'diredp-copy-tags-this-file) ; `T M-w'
  (define-key dired-mode-map "T\C-y"   'diredp-paste-add-tags-this-file) ; `T C-y'
  (define-key dired-mode-map "T>+"     'diredp-do-tag) ; `T > +'
  (define-key dired-mode-map "T>-"     'diredp-do-untag) ; `T > -'
  (define-key dired-mode-map "T>0"     'diredp-do-remove-all-tags) ; `T > 0'
  (define-key dired-mode-map "T>p"     'diredp-do-paste-add-tags) ; `T > p'
  (define-key dired-mode-map "T>q"     'diredp-do-paste-replace-tags) ; `T > q'
  (define-key dired-mode-map "T>v"     'diredp-do-set-tag-value) ; `T > v'
  (define-key dired-mode-map "T>\C-y"  'diredp-do-paste-add-tags) ; `T > C-y'
  (define-key dired-mode-map "Tm%"     'diredp-mark-files-tagged-regexp) ; `T m %'
  (define-key dired-mode-map "Tm*"     'diredp-mark-files-tagged-all) ; `T m *'
  (define-key dired-mode-map "Tm+"     'diredp-mark-files-tagged-some) ; `T m +'
  (define-key dired-mode-map "Tm~*"    'diredp-mark-files-tagged-not-all) ; `T m ~ *'
  (define-key dired-mode-map "Tm~+"    'diredp-mark-files-tagged-none) ; `T m ~ +'
  (define-key dired-mode-map "Tu%"     'diredp-unmark-files-tagged-regexp) ; `T u %'
  (define-key dired-mode-map "Tu*"     'diredp-unmark-files-tagged-all) ; `T u *'
  (define-key dired-mode-map "Tu+"     'diredp-unmark-files-tagged-some) ; `T u +'
  (define-key dired-mode-map "Tu~*"    'diredp-unmark-files-tagged-not-all) ; `T u ~ *'
  (define-key dired-mode-map "Tu~+"    'diredp-unmark-files-tagged-none) ; `T u ~ +'
  )

;; Use "|", not "r".
(define-key dired-mode-map "|" 'dired-sort-menu-toggle-reverse)

;; Don't define it unless you can use it.
(when (and (fboundp 'ls-lisp-var-p)  (ls-lisp-var-p 'ls-lisp-ignore-case))
  (define-key dired-mode-map "c" 'dired-sort-menu-toggle-ignore-case))

;; 1. Use "/", not "b". 2. Don't define it unless you can use it.
(when (and (fboundp 'ls-lisp-var-p)  (ls-lisp-var-p 'ls-lisp-dirs-first))
  (define-key dired-mode-map "/" 'dired-sort-menu-toggle-dirs-first))


;; Remove from menu-bar "Immediate" submenu, and add it to "Dir" submenu.
(easy-menu-remove-item dired-mode-map '("menu-bar" "immediate") "Sort By")
(easy-menu-add-item dired-mode-map '("menu-bar" "subdir") dired-sort-menu
                    'revert-buffer)


;;; Functions -----------------------------


;; REPLACE ORIGINAL in `dired-sort-menu.el'.
;;
;; 1. Fit frame.
;; 2. Removed `dired-sort-dialogue-auto-kill-1' from `kill-buffer-hook'.
;;
;;;###autoload
(defun dired-sort-dialogue ()
  "A static dialogue version of the Dired sort menu.
This command *must* be run in the Dired buffer!"
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (error "This command may only be run in a Dired buffer"))
  (let
      ;; Must set these variables while still in the dired buffer!
      ((radio         (dired-sort-dialogue-choice))
       (reverse       (dired-sort-menu-switch-p "r"))
       (recursive     (dired-sort-menu-switch-p "R"))
       (dired-buffer  (current-buffer))
       ;; Suspend automatic mechanisms:
       window-configuration-change-hook kill-buffer-hook)

    ;; Check whether a dialogue buffer for this dired buffer is
    ;; already visible, and if so re-use its window:
    (let ((bufname  (dired-sort-dialogue-buffer-name))
          (bufs     (buffer-list)) buf
          (title    (concat "<" (buffer-name dired-buffer) ">")))
      (while (and bufs  (not (string= bufname (buffer-name (setq buf  (car bufs))))))
        (setq bufs  (cdr bufs)))
      (if bufs
          (progn
            (if (dired-sort-dialogue-own-frame-really)
                (progn
                  (select-frame (window-frame (get-buffer-window buf t)))
                  (raise-frame))
              (select-window (get-buffer-window buf t)))
            (set-window-dedicated-p (selected-window) nil)
            (kill-buffer buf))
        (if (dired-sort-dialogue-own-frame-really)
            ;; If room then put dialogue immediately to the right of
            ;; the dired frame, else at right edge of screen.
            (let* ((alist  (frame-parameters))
                   (top    (cdr (assq 'top alist))) ; pixels
                   (left   (cdr (assq 'left alist))) ; pixels
                   )
              ;; Allow form INTEGER or (+ INTEGER):
              (or (atom left)  (setq left  (cadr left)))
              ;; Set left of dialogue frame to avoid falling off right
              ;; of display:
              (setq left  (+ left (frame-pixel-width))
                    left  (if (> (+ left (* dired-sort-dialogue-width
                                            (frame-char-width)))
                                 (x-display-pixel-width))
                              -10
                            ;; (+ left (* 2 (cdr (assq 'border-width alist))))))
                            (+ left 10)))
              (select-frame (make-frame
                             `((title . ,title)
                               (top . ,top)
                               (left . ,left)
                               (width . ,dired-sort-dialogue-width)
                               (height . 22)
                               (minibuffer . nil)
                               (vertical-scroll-bars . nil)
                               (horizontal-scroll-bars . nil)
                               (unsplittable . nil)
                               (menu-bar-lines . 0)
                               ))))
          (split-window                 ; WINDOW SIZE HORIZONTAL
           nil (- (window-width) dired-sort-dialogue-width) t)
          (select-window (next-window))))
      (switch-to-buffer bufname)
      (set-window-dedicated-p (selected-window) t) ; can crash Emacs!
      (kill-all-local-variables)
      ;;       (or buffer-display-table
      ;;         (setq buffer-display-table
      ;;               (or standard-display-table  (make-display-table))))
      ;;       (set-display-table-slot buffer-display-table 0 ?_)
      (setq truncate-lines    t
            mode-line-format  title))

    (let ((inhibit-read-only  t))
      (erase-buffer))
    ;; Must set this only once in the dialogue buffer!
    (setq dired-sort-dialogue-dired-buffer  dired-buffer)

    (let ((start  (point)))
      (widget-insert "Dired Sort Options")
      (put-text-property start (point) 'face 'bold))
    (widget-insert " for\n<"
                   (buffer-name dired-buffer)
                   ">\n\n(Use any mouse button)\n\n ")
    (setq dired-sort-dialogue-radio-widget
          (eval `(widget-create
                  'radio-button-choice
                  :indent 1
                  :value radio
                  '(item :tag "Name" "")
                  '(item :tag "Time Modified" "t")
                  ,@(if (dired-sort-menu-active-p "S")
                        '('(item :tag "Size" "S")))
                  ,@(if (dired-sort-menu-active-p "X")
                        '('(item :tag "Extension" "X")))
                  ,@(if (dired-sort-menu-active-p "U")
                        '('(item :tag "Unsorted" "U")))
                  ,@(if (dired-sort-menu-active-p "c")
                        `('(item :tag
                            ,(if (or (not (eq system-type 'windows-nt))
                                     (dired-sort-menu-remote-p))
                                 "Time Changed"
                                 "Time Created") "c")))
                  ,@(if (and (dired-sort-menu-active-p "u")
                             (or (not (eq system-type 'windows-nt))
                                 (dired-sort-menu-remote-p)))
                        '('(item :tag "Time Accessed" "u")))
                  )))
    (widget-insert " _____________________\n\n ")
    (when (dired-sort-menu-active-p "r")
      (setq dired-sort-dialogue-reverse-widget
            (widget-create 'checkbox
                           :help-echo "Reverse the sort order"
                           reverse))
      (widget-insert " Reverse\n "))
    (when (dired-sort-menu-active-p "R")
      (setq dired-sort-dialogue-recursive-widget
            (widget-create 'checkbox
                           :help-echo "Recursively list all subdirectories"
                           recursive))
      (widget-insert " Recursive\n "))
    (when (ls-lisp-var-p 'ls-lisp-ignore-case)
      (setq dired-sort-dialogue-ignore-case-widget
            (widget-create 'checkbox
                           :help-echo "Ignore case when sorting"
                           ls-lisp-ignore-case))
      (widget-insert " Ignore Case\n "))
    (when (ls-lisp-var-p 'ls-lisp-dirs-first)
      (setq dired-sort-dialogue-dirs-first-widget
            (widget-create 'checkbox
                           :help-echo "Sort directories first"
                           ls-lisp-dirs-first))
      (widget-insert " Dirs First\n "))
    (widget-insert "_____________________\n\n ")
    (widget-create 'push-button
                   :notify 'dired-sort-dialogue-OK
                   :help-echo "Apply the settings and close the window"
                   "OK")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify 'dired-sort-dialogue-close
                   :help-echo "Close the window and ignore the settings"
                   "Cancel")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify 'dired-sort-dialogue-apply
                   :help-echo "Apply the settings without closing the window"
                   "Apply")
    (widget-setup)
    (goto-char (point-min))
    ;;      (use-local-map widget-keymap)
    ;;      (let ((map  (make-sparse-keymap)))
    ;;        (suppress-keymap map)
    ;;        (set-keymap-parent map widget-keymap)
    ;;        (define-key map [down-mouse-1] 'widget-button-click)
    ;;        (define-key map [down-mouse-3] 'widget-button-click)
    ;;        (use-local-map map))
    (let ((map  widget-keymap))
      ;; (define-key map [t] 'undefined)
      ;; (define-key map [tab] 'widget-forward)
      ;; (define-key map [return] 'widget-button-press)
      (define-key map [down-mouse-1] 'widget-button-click)
      (define-key map [down-mouse-3] 'widget-button-click)
      ;; (define-key map [escape] (lambda () (interactive)
      ;;                         (dired-sort-dialogue-close)))
      ;; (define-key map "\C-h" 'describe-bindings)
      (use-local-map map)))
  ;; D. Adams - added this line:
  (when (fboundp 'fit-frame) (fit-frame))

  ;; Set up these hooks here to avoid any possibility of causing
  ;; trouble if the dialogue facility is not used:

  ;; D. Adams - REMOVED - not needed if use my stuff.
  ;; (add-hook 'kill-buffer-hook 'dired-sort-dialogue-auto-kill-1)

  (add-hook 'window-configuration-change-hook
            'dired-sort-dialogue-auto-kill-2))


;; REPLACE ORIGINAL in `dired-sort-menu.el'.
;;
;; Redefined to just `kill-buffer'. My other libraries take care of the rest.
;;
;;;###autoload
(defun dired-sort-dialogue-close (&rest ignore)
  "Close the dired sort dialogue (ignoring the settings)."
  (kill-buffer (current-buffer)))
;;; (defun dired-sort-dialogue-close (&rest ignore)
;;;   "Close the dired sort dialogue (ignoring the settings)."
;;;   (let ((dired-buffer  dired-sort-dialogue-dired-buffer)
;;;     window-configuration-change-hook
;;;     kill-buffer-hook)
;;;     (set-window-dedicated-p (selected-window) nil)
;;;     (kill-buffer (current-buffer))
;;;     (if (dired-sort-dialogue-own-frame-really)
;;;     (delete-frame)
;;;       (or (one-window-p t)  (delete-window)))
;;;     (select-window (get-buffer-window dired-buffer))))


;; REPLACE ORIGINAL in `dired-sort-menu.el'.
;;
;; Protect in case `buffer-name' is nil.
;;
(defadvice handle-delete-frame
  (before handle-delete-frame-advice activate)
  "Kill dialogue buffer before killing its frame."
  (let* ((frame  (posn-window (event-start (ad-get-arg 0))))
         (buf    (car (buffer-list frame))))
    (when (and (buffer-name buf)
               (dired-sort-dialogue-buffer-p (buffer-name buf)))
      (set-window-dedicated-p (selected-window) nil)
      (kill-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired-sort-menu+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired-sort-menu+.el ends here
