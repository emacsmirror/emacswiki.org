;;; diff+20.el --- Extensions to `diff.el' for Emacs 20 or older.
;;
;; Filename: diff+20.el
;; Description: Extensions to `diff.el' for Emacs 20 or older.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2015, Drew Adams, all rights reserved.
;; Created: Fri Mar 15 09:33:29 1996
;; Version: 0
;; Last-Updated: Thu Jan  1 10:33:50 2015 (-0800)
;;           By: dradams
;;     Update #: 635
;; URL: http://www.emacswiki.org/diff+20.el
;; Doc URL: http://www.emacswiki.org/DiffEnhancements
;; Keywords: data, matching, tools, unix, local
;; Compatibility: GNU Emacs: 20.x, 21.x
;;
;; Features that might be required by this library:
;;
;;   `compile', `diff', `diff+20'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `diff.el' for Emacs 20 or older.
;;    Buffer "*diff*" is highlighted.
;;
;;  Library `diff.el' changed significantly from Emacs 20 to Emacs 21.
;;  For Emacs 21 extensions see libraries `diff+.el' and
;;  `diff-mode-.el'.
;;
;;
;;  New faces defined here:
;;
;;    `diff-add', `diff-buffer-header', `diff-chg', `diff-chg-header',
;;    `diff-new-file', `diff-old-file'.
;;
;;
;;  New function defined here 20: `diff-font-lock-keywords'.
;;
;;
;;  ***** NOTE: The following function defined in `diff.el' has
;;              been REDEFINED HERE:
;;
;;    `diff' - Highlights "*diff*" buffer.
;;
;;
;;  This file should be loaded *after* loading the standard GNU file
;;  `diff.el'.  So, in your `~/.emacs' file, do this:
;;
;;    (eval-after-load "diff" '(require 'diff+20))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/04 dadams
;;     Added autoload cookies for defface and command.
;; 2008/01/17 dadams
;;     Removed soft require of icicles.el.
;; 2007/11/27 dadams
;;     diff: Use icicle-read-string-completing, if available.
;;     Soft require Icicles.
;; 2005/12/18 dadams
;;     Renamed faces without "-face".
;;     Use defface.  Removed require of def-face-const.el.
;; 2005/10/03 dadams
;;     Removed require of icomplete+.el (no longer redefines read-string).
;; 2004/11/09 dadams
;;     Renamed file from diff+.el.  That name is used for Emacs 21 version.
;; 2004/10/26 dadams
;;     Removed: diff-del-face (was unused).
;; 1999/09/03 dadams
;;     1. diff-font-lock-keywords: made it a user option.
;;     2. diff: Updated doc string to mention highlighting.
;; 1999/08/26 dadams
;;     Protected faces via boundp.
;; 1999/04/09 dadams
;;     1. Changed compilation-mode-hook to font-lock-fontify-buffer.
;;     2. Define faces here.  Changed diff-chg-header-face to red-foreground-face.
;;     3. diff-font-lock-keywords: Removed regexp for first 2 buffer lines.
;;        Changed diff-old/new-file-face to cover 3rd & 4th buffer lines.
;;     4. diff: Highlight lines 1 & 2, not 3 & 4, with blue-foreground-face.
;; 1999/04/01 dadams
;;     Added: (add-hook 'compilation-mode-hook 'turn-on-font-lock).
;; 1999/04/01 dadams
;;     Added: diff-buffer-header-face.
;; 1997/03/21 dadams
;;     Updated from version 19.34: diff: "diff" -> diff-command
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

(require 'compile) ;; compile-internal, compilation-finish-function
(require 'diff) ;; diff-old-temp-file, diff-new-temp-file, diff-command

;;;;;;;;;;;;;;;;;;;;;;;;



(add-hook 'compilation-mode-hook 'font-lock-fontify-buffer)

;;;###autoload
(defface diff-old-file
    '((((class color) (background dark))
       (:foreground "SlateBlue" :background "PaleGoldenrod"))
      (t (:foreground "PaleGoldenrod" :background "SlateBlue")))
  "*Face used to highlight `diff's \"old\" file argument.
as well as deleted lines ."
  :group 'diff)

;;;###autoload
(defface diff-new-file
    '((((class color) (background dark))
       (:foreground "DarkCyan" :background "PaleGoldenrod"))
      (t (:foreground "PaleGoldenrod" :background "DarkCyan")))
  "*Face used to highlight `diff's \"new\" file argument."
  :group 'diff)

;;;###autoload
(defface diff-add
    '((((class color) (background dark))
       (:foreground "DarkCyan" :background "PaleGoldenrod"))
      (t (:foreground "PaleGoldenrod" :background "DarkCyan")))
  "*Face used to highlight `diff' addition lines."
  :group 'diff)

;;;###autoload
(defface diff-chg
    '((((class color) (background dark))
       (:foreground "DarkMagenta" :background "PaleGoldenrod"))
      (t (:foreground "PaleGoldenrod" :background "DarkMagenta")))
  "*Face used to highlight `diff' change lines (`!')."
  :group 'diff)

;;;###autoload
(defface diff-chg-header '((t (:foreground "Red")))
  "*Face used to highlight `diff' change header lines."
  :group 'diff)

;;;###autoload
(defface diff-buffer-header '((t (:foreground "DarkMagenta")))
  "*Face used to highlight first two lines of `diff' buffer."
  :group 'diff)


;; Based partly on `diff-regexp-alist'.
;; Not set via `font-lock-set-defaults'.  `diff' uses `compilation-mode'.
;; `diff' temporarily binds `compilation-mode-font-lock-keywords' to this.
(defun diff-font-lock-keywords ()
  "*Expressions to highlight for `diff' in Compilation mode."
  '(
    ;; First 2 lines in buffer
    ("\\`.*\n.*" . 'diff-buffer-header)

    ;; Difference headers.
    ;; -u format: @@ -OLDSTART,OLDEND +NEWSTART,NEWEND @@
    ("^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@$" . 'diff-chg-header)
    ;; -c format: *** OLDSTART,OLDEND ****
    ("^\\*\\*\\*\\*\\(\\*\\)*$" . 'diff-chg-header)
    ("^\\*\\*\\* .*$" . 'diff-old-file)
    ;;            --- NEWSTART,NEWEND ----
    ("^--- .*$" . 'diff-new-file)
    ;; plain diff format: OLDSTART[,OLDEND]{a,d,c}NEWSTART[,NEWEND]
    ("^[0-9]+\\(,[0-9]+\\)?[adc][0-9]+\\(,[0-9]+\\)?$" . 'diff-chg-header)
    ;; -e (ed) format: OLDSTART[,OLDEND]{a,d,c}
    ("^[0-9]+\\(,[0-9]+\\)?[adc]$" . 'diff-chg-header)
    ;; -f format: {a,d,c}OLDSTART[ OLDEND]
    ;; -n format: {a,d,c}OLDSTART LINES-CHANGED
    ("^[adc][0-9]+\\( [0-9]+\\)?$" . 'diff-chg-header)

    ;; Difference lines.
    ;;     -c format.
    ("^\\(!\\) " 1 'diff-chg)
    ;;     -c, -u formats.
    ("^\\([+]\\)" 1 'diff-new-file)
    ("^\\(-\\)" 1 'diff-old-file)
    ;;     Default (no switches).
    ("^\\(>\\)[ \t]" 1 'diff-new-file)
    ("^\\(<\\)[ \t]" 1 'diff-old-file)
    ("^\\(---\\)$" 1 'diff-chg-header t)
    ))



;; REPLACES ORIGINAL in `diff.el':
;; *diff* buffer is highlighted.
;;;###autoload
(defun diff (old new &optional switches)
  "Find and display differences between OLD and NEW files.
Interactively the current buffer's file name is the default for NEW
and a backup file for NEW is the default for OLD.
With prefix arg, prompt for `diff' switches.
The *diff* buffer is highlighted with `font-lock-mode', using
`diff-font-lock-keywords'."
  (interactive
   (nconc
    (let (oldf newf)
      (nreverse
       (list
        (progn
          (setq newf (buffer-file-name))
          (setq newf (if (and newf (file-exists-p newf))
                         (read-file-name
                          (concat "Diff new (2nd) file: ("
                                  (file-name-nondirectory newf) ") ")
                          nil newf t)
                       (read-file-name "Diff new (2nd) file: " nil nil t))))
        (progn
          (setq oldf (file-newest-backup newf))
          (setq oldf (if (and oldf (file-exists-p oldf))
                         (read-file-name
                          (concat "Diff original (1st) file: ("
                                  (file-name-nondirectory oldf) ") ")
                          (file-name-directory oldf) oldf t)
                       (read-file-name "Diff original (1st) file: "
                                       (file-name-directory newf) nil t)))))))
    (and current-prefix-arg
         (list (if (fboundp 'icicle-read-string-completing)
                   (icicle-read-string-completing
                    "Diff switches: "
                    (if (stringp diff-switches)
                        diff-switches
                      (mapconcat 'identity diff-switches " "))
                    (lambda (c) (string-match "switches" (symbol-name c))))
                 (read-string "Diff switches: "
                              (if (stringp diff-switches)
                                  diff-switches
                                (mapconcat 'identity diff-switches " "))))))))
  (setq new (expand-file-name new))
  (setq old (expand-file-name old))
  (let ((old-alt (file-local-copy old))
        (new-alt (file-local-copy new))
        buf)
    ;; Set up font-lock mode to treat diff error messages.
    ;; Have to do it via an `flet', because `font-lock-eval-keywords' looks first
    ;; for a function definition.  Otherwise, could just bind a variable
    ;; `compilation-mode-font-lock-keywords' via a `let'.
    (flet ((compilation-mode-font-lock-keywords () (diff-font-lock-keywords)))
      (save-excursion
        (let ((compilation-process-setup-function 'diff-process-setup)
              (command
               (mapconcat 'identity
                          (append (list diff-command)
                                  ;; Use explicitly specified switches
                                  (if switches
                                      (if (consp switches)
                                          switches
                                        (list switches))
                                    ;; If not specified, use default.
                                    (if (consp diff-switches)
                                        diff-switches
                                      (list diff-switches)))
                                  (and (or old-alt new-alt)
                                       (list "-L" old "-L" new))
                                  (list
                                   (shell-quote-argument (or old-alt old)))
                                  (list
                                   (shell-quote-argument (or new-alt new))))
                          " ")))
          (setq buf (compile-internal command "No more differences" "Diff"
                                      'diff-parse-differences)) ; `compile.el'
          (pop-to-buffer buf)
          (set (make-local-variable 'diff-old-file) old)
          (set (make-local-variable 'diff-new-file) new)
          (set (make-local-variable 'diff-old-temp-file) old-alt)
          (set (make-local-variable 'diff-new-temp-file) new-alt)
          (set (make-local-variable 'compilation-finish-function) ;`compile.el'
               (if (not (equal "" switches))
                   (function
                    (lambda (buff msg)
                      (when diff-old-temp-file
                        (delete-file diff-old-temp-file))
                      (when diff-new-temp-file
                        (delete-file diff-new-temp-file))))
                 (function
                  (lambda (buff msg)
                    (when diff-old-temp-file
                      (delete-file diff-old-temp-file))
                    (when diff-new-temp-file
                      (delete-file diff-new-temp-file))))))
          (unless (fboundp 'start-process)
            (funcall compilation-finish-function nil nil))
          (font-lock-mode 99)
          buf)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'diff+20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diff+20.el ends here
