;;; outline+.el --- Extensions to `outline.el'.
;;
;; Filename: outline+.el
;; Description: Extensions to `outline.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2015, Drew Adams, all rights reserved.
;; Created: Fri Jun 21 08:56:04 1996
;; Version: 20.0
;; Last-Updated: Thu Jan  1 11:08:17 2015 (-0800)
;;           By: dradams
;;     Update #: 336
;; URL: http://www.emacswiki.org/outline+.el
;; Keywords: abbrev, matching, local
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `outline'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `outline.el'.
;;
;;  Menu bar:
;;
;;  Reordered and renamed menu bar Outline Mode Hide and Show menus.
;;  Names of menu items now indicate whether the item applies locally
;;  or globally.  Global and local items are separated in the menu.
;;
;;      Renamings:
;;        Hide Leaves    -> Hide Entries                (local)
;;        Hide Body      -> Hide All But Headings       (global)
;;        Hide Subtree   -> Hide Tree                   (local)
;;        Hide Sublevels -> Hide All But Top N Headings (global)
;;        Hide Other     -> Hide All But Entry          (local)
;;        Show Branches  -> Show Headings               (local)
;;        Show Children  -> Show Headings N Deep        (local)
;;        Show Subtree   -> Show Tree                   (local)
;;
;;  Outline minor mode font locking:
;;
;;     See the new command `toggle-outline-minor-mode-font-lock',
;;     intended for use as both `outline-minor-mode-hook' and
;;     `outline-minor-mode-exit-hook'.
;;
;;
;;  New function defined here: `toggle-outline-minor-mode-font-lock'.
;;
;;  New variables defined here:
;;
;;     Var `outline-minor-mode-hook' was not declared in `outline.el'.
;;     It is declared here, along with `outline-minor-mode-exit-hook'.
;;
;;
;;  ***** NOTE: The following function defined in `vc.el' has been
;;              REDEFINED HERE:
;;
;;  `outline-minor-mode' - Call to `outline-minor-mode-exit-hook'.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `outline.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "outline" '(progn (require 'outline+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/04 dadams
;;     Removed autoload cookies from defvar.
;; 2006/03/30 dadams
;;     No longer use display-in-minibuffer.
;; 2005/12/30 dadams
;;     Added: minibuffer-prompt face.  Removed blue-foreground-face.
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

(require 'outline)
(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when

;;; You will get this:
;;;
;;; Compiling file outline+.el
;;;   ** the function outline-mode-font-lock-keywords is not known to be defined.

;;;;;;;;;;;;;;;;;;;;


;; This is defined in `faces.el', Emacs 22.  This definition is adapted to Emacs 20.
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "Face for minibuffer prompts."
    :group 'basic-faces))

(defvar outline-minor-mode-exit-hook nil
  "*Functions to be called when `outline-minor-mode' is exited.")

(defvar outline-minor-mode-hook nil
  "*Functions to be called when `outline-minor-mode' is entered.")


;; REPLACES ORIGINAL in `outline.el':
;; Added `outline-minor-mode-exit-hook'.
;;;###autoload
(defun outline-minor-mode (&optional arg)
  "Toggle Outline minor mode.
Non-nil prefix ARG turns mode on if ARG is positive, else turns off.
Runs `outline-minor-mode-hook' when Outline minor mode is entered.
Runs `outline-minor-mode-exit-hook' when Outline minor mode is exited.
See the command `outline-mode' for more information on this mode."
  (interactive "P")
  (setq outline-minor-mode (if (null arg)
                               (not outline-minor-mode)
                             (> (prefix-numeric-value arg) 0)))
  (setq selective-display outline-minor-mode)
  (if outline-minor-mode
      (run-hooks 'outline-minor-mode-hook)
    ;; Show all lines, getting rid of any ^M's.
    (outline-flag-region (point-min) (point-max) ?\n)
    (run-hooks 'outline-minor-mode-exit-hook))
  (set-buffer-modified-p (buffer-modified-p)))

;;;###autoload
(defun toggle-outline-minor-mode-font-lock ()
  "Toggle `font-lock-mode' for Outline minor mode.
Usable as `outline-minor-mode-hook' & `outline-minor-mode-exit-hook'.

As `outline-minor-mode-hook':
Highlight according to Outline minor mode.  If already highlit
according to some other mode, then require confirmation first.

As `outline-minor-mode-exit-hook':
Remove Outline minor mode highlighting, if any.
Then, upon confirmation, rehighlight according to the major mode."
  (interactive)
  (let* ((outline-keywords (and (fboundp 'outline-mode-font-lock-keywords)
                                (outline-mode-font-lock-keywords)))
         (outline-keywords-p (equal font-lock-keywords outline-keywords)))
    (if outline-minor-mode
        ;; Assume ENTERING outline minor mode.
        (when (and (not outline-keywords-p) ; Not already outline keywords.
                   (or (not font-lock-mode) ; OK to use outline highlighting.
                       (y-or-n-p "Use outline-minor-mode highlighting? ")))
          (setq font-lock-keywords outline-keywords)
          ;;;@@@Emacs20 (setq font-lock-no-comments t)
          (font-lock-mode -999)         ; Remove existing highlighting.
          (font-lock-mode 999))
      ;; Assume EXITING outline minor mode.
      (when (and font-lock-mode outline-keywords-p) ;  Outline highlit.
        (setq font-lock-keywords nil)   ; Reset keywords to those of major mode
        (font-lock-set-defaults)
        (font-lock-mode -999)           ; Remove existing highlighting.
        (when (y-or-n-p (format "Turn Font Lock mode ON in mode %s? "
                                mode-name))
          (font-lock-mode 999))))
    (message "Outline minor mode is now %s." (if outline-minor-mode "ON" "OFF"))))



;; Outline mode menu-bar menu.

(define-key outline-mode-menu-bar-map [hide]
  (cons "Hide" (make-sparse-keymap "Hide")))

(define-key outline-mode-menu-bar-map [hide hide-sublevels]
  '("Hide All But Top N Headings (global)" . hide-sublevels))
(define-key outline-mode-menu-bar-map [hide hide-body]
  '("Hide All But Headings       (global)" . hide-body))
(define-key outline-mode-menu-bar-map [hide hide-separator] '("--"))
(define-key outline-mode-menu-bar-map [hide hide-other]
  '("Hide All But Entry          (local)" . hide-other))
(define-key outline-mode-menu-bar-map [hide hide-entry]
  '("Hide Entry                  (local)" . hide-entry))
(define-key outline-mode-menu-bar-map [hide hide-leaves]
  '("Hide Entries                (local)" . hide-leaves))
(define-key outline-mode-menu-bar-map [hide hide-subtree]
  '("Hide Tree                   (local)" . hide-subtree))

(define-key outline-mode-menu-bar-map [show]
   (cons "Show" (make-sparse-keymap "Show")))

(define-key outline-mode-menu-bar-map [show show-all]
  '("Show All             (global)" . show-all))
(define-key outline-mode-menu-bar-map [show show-separator] '("--"))
(define-key outline-mode-menu-bar-map [show show-entry]
  '("Show Entry           (local)" . show-entry))
(define-key outline-mode-menu-bar-map [show show-children]
  '("Show Headings N Deep (local)" . show-children))
(define-key outline-mode-menu-bar-map [show show-branches]
  '("Show Headings        (local)" . show-branches))
(define-key outline-mode-menu-bar-map [show show-subtree]
  '("Show Tree            (local)" . show-subtree))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'outline+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; outline+.el ends here
