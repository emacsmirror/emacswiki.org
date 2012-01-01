;;; help-mode+.el --- Extensions to `help-mode.el'
;;
;; Filename: help-mode+.el
;; Description: Extensions to `help-mode.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2012, Drew Adams, all rights reserved.
;; Created: Sat Nov 06 15:14:12 2004
;; Version: 21.0
;; Last-Updated: Sun Jan  1 14:36:20 2012 (-0800)
;;           By: dradams
;;     Update #: 104
;; URL: http://www.emacswiki.org/cgi-bin/wiki/help-mode+.el
;; Keywords: help
;; Compatibility: GNU Emacs: 22.x, 23.x
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

;;;;;;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL IN `help-mode.el'.
;; Deletes frame if `one-window-p'.
;;
;;;###autoload
(defun help-mode ()
  "Major mode for viewing help text and navigating references in it.
Entry to this mode runs the normal hook `help-mode-hook'.
Commands:
\\{help-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map help-mode-map)
  (setq mode-name "Help")
  (setq major-mode 'help-mode)
  (view-mode)
  (make-local-variable 'view-no-disable-on-exit)
  (setq view-no-disable-on-exit t)
  (setq view-exit-action (lambda (buffer)
                           (or (window-minibuffer-p (selected-window))
                               (when (eq (window-buffer) (get-buffer "*Help*"))
                                 (if (one-window-p t)
                                     (delete-frame)
                                   (delete-window))))))
  (run-mode-hooks 'help-mode-hook))


;; REPLACES ORIGINAL IN `help-mode.el'.
;; Buttonizes names of libraries also.
;; To see the effect, try `C-h v features', and click on a library name.
;;
;; 2006-01-20: This no longer works, because the call to this function
;; from `describe-variable was commented out in `help-fns.el'.
;;
(defun help-xref-on-pp (from to)
  "Add xrefs for symbols in `pp's output between FROM and TO."
  (if (> (- to from) 5000) nil
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
                  (let* ((sym (intern-soft (match-string 1)))
                         (type (cond ((fboundp sym) 'help-function)
                                     ((or (memq sym '(t nil))
                                          (keywordp sym))
                                      nil)
                                     ((and sym (boundp sym))
                                      'help-variable)
                                     ((and sym (locate-library (symbol-name sym)))
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
;;   (let* ((start (event-start click))
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
