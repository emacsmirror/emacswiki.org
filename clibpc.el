;;; clibpc.el --- partial complete functions for c libraries

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: clib-complete.el,v 0.0 2007/06/11 16:22:34 ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Recently, I began to learn how to write gtk applications. GLib, gtk
;; and gnome function is sometimes to long to type which exactly like
;; elisp symbols, and I like use PC-lisp-complete-symbol to write
;; elisp. So I began to write some elisp function to extract functions
;; from gtk, GLib and gnome header files.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'clibpc)
;;   (add-hook 'c-mode-hook
;;             (lambda ()
;;               (define-key c-mode-map "\t" 'clibpc-complete-function)
;;               (eldoc-mode 1)
;;               (set (make-local-variable 'eldoc-documentation-function)
;;                    'clibpc-eldoc-function))))

;;; Code:

(provide 'clibpc)
(eval-when-compile
  (require 'cl))

(require 'complete)

(defvar clibpc-header-file-regexp "\\.h$"
  "The file match this regexp will be scaned.")

(defvar clibpc-include-path
  '("/usr/include/gtk-2.0"
    "/usr/include/glib-2.0"
    "/usr/include/libglade-2.0/")
  "The header file directories. The files that match
`clibpc-header-file-regexp' in these directory will all be scaned
by `clibpc-parse-header'.")

(defvar clibpc-obarray nil
  "All functions in lib. Every symbol in this obarray has a value
 (FILE . POS), which FILE and POS is the position of the symbol
declared in header files. The symbol may have a property document
which is the declaration of the function.")

(defvar clibpc-regexp-list
  '(("^\\(?:\\(?:G_CONST_RETURN\\|extern\\|const\\)\\s-+\\)?[a-zA-Z][_a-zA-Z0-9]*\
\\(?:\\s-*[*]*[ \t\n]+\\|\\s-+[*]*\\)\\([a-zA-Z][_a-zA-Z0-9]*\\)\\s-*(" . 1)
    ("^\\s-*#\\s-*define\\s-+\\([a-zA-Z][_a-zA-Z0-9]*\\)" . 1))
  "The line match this regexp will add to obarray.

The car part of cons cell is the regexp to search, and cdr part
is the parenthesized expression of the function name.")

(defvar clibpc-cache-file "~/.emacs.d/clibpc-symbols.el"
  "Save parsed data to this file.")

(defun clibpc-parse-header (file &optional arg)
  "Extract function name and declaration position using
`clibpc-regexp-list'. If arg in non-nil or call interactively, the
symbol extracted will not save to clibpc-cache-file. So you can add
symbol just for current session."
  (interactive "fHeader file: \nP")
  (or arg (setq arg (called-interactively-p)))
  (let (sym)
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (re clibpc-regexp-list)
        (goto-char (point-min))
        (while (re-search-forward (car re) nil t)
          ;; should I add some trick to remove typedef
          (setq sym (intern (match-string (cdr re)) clibpc-obarray))
          (set sym (cons file (line-beginning-position)))
          (and arg (put sym 'no-save arg)))))))

(defun clibpc-parse-directory (dir &optional non-recursively)
  "Extract function name and declaration position using
`clibpc-regexp-list' in DIR. If with prefix arg, the header file
just in this DIR will be read."
  (interactive "DInclude path: \nP")
  (mapc
   (lambda (f)
     (if (and (file-directory-p f) (not non-recursively))
         (clibpc-parse-directory f)
       (if (and (file-regular-p f)
                (string-match clibpc-header-file-regexp f))
           (clibpc-parse-header f (called-interactively-p)))))
   (directory-files dir t "[^.]$")))

(defun clibpc-symbol-document (symbol)
  (or (get symbol 'document)
      (with-temp-buffer
        (let ((file (symbol-value symbol)))
          (insert-file-contents (car file))
          (goto-char (cdr file))
          (put symbol 'document
               (replace-regexp-in-string
                "[ \t\n]\\{2,\\}" " "
                (buffer-substring (point)
                                  (if (looking-at "^\\s-*#define")
                                      (line-end-position)
                                    (progn (re-search-forward ")") (point))))))))))

(defun clibpc-eldoc-function ()
  (let (string symbol done)
    (save-excursion
      (while (not (or done symbol))
        (if (and (setq string (thing-at-point 'symbol))
                 (intern-soft string clibpc-obarray))
            (setq symbol (intern-soft string clibpc-obarray))
          (setq done (null (re-search-backward "\\>\\s-*(" (line-beginning-position) t))))))
    (when (and symbol (clibpc-symbol-document symbol))
      (message "[%s] %s"
               (file-name-nondirectory (car (symbol-value symbol)))
               (clibpc-symbol-document symbol)))))

(defun clibpc-describe-function (func)
  (interactive
   (let ((sym (thing-at-point 'symbol)))
     (list (completing-read
            (if (and sym (intern-soft sym clibpc-obarray))
                (format "Describe function(default %s): " sym)
              "Describe function: ")
            clibpc-obarray nil t nil nil sym))))
  (let ((symbol (intern-soft func clibpc-obarray))
        (inhibit-read-only t))
    (when symbol
      (save-excursion
        (with-output-to-temp-buffer (help-buffer)
          (require 'button)
          (princ func)
          (princ " is declared in `")
          (princ (file-name-nondirectory (car (symbol-value symbol))))
          (princ "'\n\n")
          (princ (clibpc-symbol-document symbol))
          (princ "\n\n")
          (with-current-buffer standard-output
            (goto-char (point-min))
            (make-text-button (progn (re-search-forward "`") (point))
                              (1- (line-end-position))
                              'file (symbol-value symbol)
                              'action (lambda (but)
                                        (let ((file (button-get but 'file)))
                                          (switch-to-buffer (find-file-noselect (car file)))
                                          (goto-char (cdr file)))))))))))

;; borrow from PC-lisp-complete-symbol
(defun clibpc-complete-function ()
  (interactive)
  (let* ((end (point))
         (beg (save-excursion
                (with-syntax-table lisp-mode-syntax-table
                  (backward-sexp 1)
                  (while (= (char-syntax (following-char)) ?\')
                    (forward-char 1))
                  (point))))
         (minibuffer-completion-table clibpc-obarray)
         (minibuffer-completion-predicate 'identity)
         (PC-not-minibuffer t))
    (if (equal last-command 'PC-lisp-complete-symbol)
        (PC-do-completion nil beg PC-lisp-complete-end t)
      (if PC-lisp-complete-end
          (move-marker PC-lisp-complete-end end)
        (setq PC-lisp-complete-end (copy-marker end t)))
      (PC-do-completion nil beg end t))))

(defun clibpc-save-symbols (file)
  (interactive
   (list (read-file-name "Save to: "
                         (file-name-directory clibpc-cache-file)
                         (file-name-nondirectory clibpc-cache-file))))
  (with-temp-buffer
    (insert "(")
    (mapatoms (lambda (s)
                (or (get s 'no-save)
                    (insert (format "(%S . %S)\n"
                                    s (symbol-value s)))))
              clibpc-obarray)
    (insert ")")
    (write-region (point-min) (point-max) file nil 'nomessage)))

(defun clibpc-load-symbols (file)
  (interactive
   (list (read-file-name "Save to: "
                         (file-name-directory clibpc-cache-file)
                         (file-name-nondirectory clibpc-cache-file) t)))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (dolist (sym (read (current-buffer)))
      (set (intern (symbol-name (car sym)) clibpc-obarray) (cdr sym)))))

(defun clibpc-rebuild-obarray ()
  (interactive)
  (setq clibpc-obarray (make-vector 1511 0))
  (message "Wait while building symbol table...")
  (dolist (dir clibpc-include-path)
    (and (file-exists-p dir) (clibpc-parse-directory dir)))
  (message "done!")
  (clibpc-save-symbols clibpc-cache-file))

(unless clibpc-obarray
  (if (and clibpc-cache-file (file-exists-p clibpc-cache-file))
      (progn
        (setq clibpc-obarray (make-vector 1511 0))
        (clibpc-load-symbols clibpc-cache-file))
    (clibpc-rebuild-obarray)))
        
;;; clibpc.el ends here
