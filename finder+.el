;;; finder+.el --- Extensions to standard library finder.el
;;
;; Filename: finder+.el
;; Description: Extensions to standard library finder.el
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2008-2017, Drew Adams, all rights reserved.
;; Created: Wed Mar 12 10:00:16 2008 (Pacific Standard Time)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Wed Feb 22 17:53:09 2017 (-0800)
;;           By: dradams
;;     Update #: 124
;; URL: https://www.emacswiki.org/emacs/download/finder%2b.el
;; Doc URL: http://emacswiki.org/FinderMode
;; Keywords: help
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `emacsbug', `finder', `finder-inf', `lisp-mnt', `sendmail'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to standard library finder.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/01/15 dadams
;;     finder-mode: Call font-lock-refresh-defaults.
;; 2011/01/04 dadams
;;     Added autoload cookie for command.
;; 2010/08/28 dadams
;;     finder-exit, finder-mode: Redefine only for Emacs < 23.
;; 2010/02/28 dadams
;;     finder-commentary: Delete any trailing blank lines.
;; 2009/05/02 dadams
;;     finder-commentary: Fixed typo: foundp -> fboundp.
;; 2009/02/12 dadams
;;     Added redefinition of finder-commentary.  See also Emacs bug #2291.
;;     finder-mode: Protect finder-font-lock-keywords, run-mode-hooks (Emacs 20).
;; 2008/03/22 dadams
;;     finder-mode: Don't also use lisp font-locking.
;; 2008/03/21 dadams
;;     Require finder.el.
;;     copy-syntax-table -> make-syntax-table.
;;     Added: finder-font-lock-keywords.
;;     finder-exit: Removed unbound var window.
;;     Set font-lock-defaults instead of using font-lock-add-keywords.
;; 2008/03/14 dadams
;;     Added redefinition of finder-mode.  Added: finder-mode-syntax-table.
;; 2008/03/12 dadams
;;     Created, with redefinition of finder-exit.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'finder)

;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar finder-mode-syntax-table
  (let ((st (make-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\; ".   " st)
    st)
  "Syntax table used while in `finder-mode'.")

(defvar finder-font-lock-keywords
  '(("`\\([^']+\\)'" 1 font-lock-constant-face prepend))
  "Font-lock keywords for Finder mode.")

;; REPLACES ORIGINAL in `finder.el'.
;; Wraps `delete-window' in `condition-case'.
;; Kills also buffer `*Finder-package*'.
;;
(when (< emacs-major-version 23)
  (defun finder-exit ()
    "Exit Finder mode.
Delete the window and kill the buffer."
    (interactive)
    (condition-case nil (delete-window) (error nil))
    (when (get-buffer "*Finder*") (kill-buffer "*Finder*"))
    (when (get-buffer "*Finder-package*") (kill-buffer "*Finder-package*"))
    (when (get-buffer "*Finder Category*") (kill-buffer "*Finder Category*"))))


;; REPLACES ORIGINAL in `finder.el'.
;; Uses `finder-mode-syntax-table', not `emacs-lisp-mode-syntax-table'.
;; Adds font-lock keywords for `...' highlighting.
;;
(when (< emacs-major-version 23)
  (defun finder-mode ()
    "Major mode for browsing package documentation.
\\<finder-mode-map>
\\[finder-select]	more help for the item on the current line
\\[finder-exit]	exit Finder mode and kill the Finder buffer."
    (interactive)
    (kill-all-local-variables)
    (use-local-map finder-mode-map)
    (set-syntax-table finder-mode-syntax-table)
    (when (boundp 'finder-font-lock-keywords) ; Emacs 23.
      (setq font-lock-defaults '(finder-font-lock-keywords nil nil
                                 (("+-*/.<>=!?$%_&~^:@" . "w")) nil)))
    (when (fboundp 'font-lock-refresh-defaults) (font-lock-refresh-defaults))
    (setq mode-name "Finder")
    (setq major-mode 'finder-mode)
    (set (make-local-variable 'finder-headmark) nil)
    (when (and (fboundp 'run-mode-hooks) (boundp 'finder-mode-hook))
      (run-mode-hooks 'finder-mode-hook))))


;; REPLACES ORIGINAL in `finder.el'.
;; Names the buffer *Commentary, <file>*, not *Finder-package*.
;; Fits the frame and raises it afterward.
;;
;;;###autoload
(defun finder-commentary (file)
  "Display FILE's commentary section.
FILE should be in a form suitable for passing to `locate-library'."
  (interactive
   (list
    (cond ((fboundp 'locate-file-completion-table) ; Emacs 23.
           (completing-read "Library name: "
                            (apply-partially 'locate-file-completion-table
                                             (or find-function-source-path load-path)
                                             (find-library-suffixes))))
          ((fboundp 'locate-file-completion) ; Emacs 22.
           (completing-read "Library name: "
                            'locate-file-completion
                            (cons (or find-function-source-path load-path)
                                  (find-library-suffixes))))
          (t
           (read-string "Library name: ")))))
  (let ((str  (lm-commentary
               (if (fboundp 'find-library-name)
                   (find-library-name file)
                 (or (finder-find-library file)
                     (finder-find-library (concat file ".el"))
                     (error "Can't find library %s" file))))))
    (unless str	(error "No Commentary section in `%s'" file))
    (pop-to-buffer (concat "*Commentary, " (file-name-sans-extension file) "*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert str)
    (goto-char (point-min))
    (delete-blank-lines)
    (goto-char (point-max))
    (delete-blank-lines)
    (goto-char (point-min))
    (while (re-search-forward "^;+ ?" nil t) (replace-match "" nil nil))
    (goto-char (point-max))             ; Delete any trailing blank lines.
    (when (re-search-backward "\\S-" nil t)
      (forward-line 1)
      (delete-blank-lines))
    (goto-char (point-min))
    (when (and (fboundp 'make-text-button) ; Emacs 23.
               (get 'finder-xref 'button-category-symbol))
      (while (re-search-forward "\\<\\([-[:alnum:]]+\\.el\\)\\>" nil t)
        (if (locate-library (match-string 1))
            (make-text-button (match-beginning 1) (match-end 1)
                              'xref (match-string-no-properties 1)
                              'help-echo "Read this file's commentary"
                              :type 'finder-xref))))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (shrink-window-if-larger-than-buffer)
    (when (and (require 'fit-frame nil t) (require 'autofit-frame nil t))
      (fit-frame-if-one-window))
    (finder-mode)
    (finder-summary))
  (raise-frame))

;;;;;;;;;;;;;;;;;;;

(provide 'finder+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finder+.el ends here
