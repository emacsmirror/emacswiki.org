;;; template-simple.el --- Simple template functions and utils

;; Copyright (C) 2007  Ye Wenbin

;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 21 Dec 2007
;; Version: 0.01
;; Keywords: tools, convenience
;; 
;; This file is part of PDE (Perl Development Environment).
;; But it is useful for generic programming.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * Why not template?
;;   A template.el is already exists, and it does everything well.
;;   But I hate to read the code to use it in my extension. I need
;;   simple thing to get work done. template-simple is designed
;;   to compatible with template. The two useful features are
;;   implemented, expand template in file and update file header.
;;   And with addtional, you can use this to write simple skeleton
;;   and tempo template. Or you can implement other expand function
;;   to expand the parsed templates.
;;
;; * Where to use it?
;;   You can use it with autoinsert, tempo, skeleton or other related
;;   extensions. I hope this help you to write template for tempo or
;;   skeleton without any knowledge with emacs lisp.
;;
;; * Tips
;;   If you don't like the (>>> and <<<) for open and close paren,
;;   you can overwrite it like file variable in template, for example:
;;
;;   (template-simple-expand
;;    ";; -*- template-parens: (\"{\" . \"}\"); template-expand-function: template-tempo-expand -*-
;;   (defun {p} ({p})
;;     \"{p}\"
;;     {p}
;;     )")
;;
;;   The template is expand by template-tempo-expand and use {} as paren inside
;;   template string.

;;; Dependencies:
;;  no extra libraries is required

;;; Installation:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'template-simple)
;;   
;;; Code:
(eval-when-compile
  (require 'cl))

 
;;; Customizable variables
(defgroup template-simple nil
  "Simple template functions and utils"
  :group 'abbrev
  :group 'convenience
  :group 'pde)

(defcustom template-directory-list
  (append '("~/.templates/")
          (if (boundp 'auto-insert-directory)
              (list auto-insert-directory)))
  "*Directory for lookup template files."
  :type '(repeat directory)
  :group 'template-simple)

(defcustom template-default-alist
  '(("dir" (file-name-directory template-file-name))
    ("file" (file-name-nondirectory template-file-name))
    ("file-sans" (file-name-sans-extension
                  (file-name-nondirectory template-file-name)))
    ("file-ext" (file-name-extension
                 (file-name-nondirectory template-file-name)))
    ("file-upcase" (upcase (file-name-sans-extension
                            (file-name-nondirectory template-file-name))))
    ("date" (format-time-string template-date-format))
    ("cdate" (let ((system-time-locale "C"))
               (format-time-string template-cdate-format)))
    ("iso-date" (format-time-string "%Y-%m-%d"))
    ("vc-date" (prog2
                   (set-time-zone-rule "UTC")
                   (format-time-string "%Y/%m/%d %T")
                 (set-time-zone-rule nil)))
    ("year" (format-time-string "%Y"))
    ("time" (format-time-string template-time-format))
    ("author" (or user-mail-address
                  (concat (user-login-name) "@" (system-name))))
    ("user-name" user-full-name)
    ("login-name" user-login-name)
    ("host-addr" (or mail-host-address (system-name))))
  "*Default expansion list"
  :type '(alist :key-type string :value-type sexp)
  :group 'template-simple)

(defcustom template-date-format "%d %b %Y"
  "*Date format for date in `template-default-alist'."
  :type 'string
  :group 'template-simple)

(defcustom template-cdate-format "%d %b %Y"
  "*Date format for date with `system-time-locale' has value \"C\""
  :type 'string
  :group 'template-simple)

(defcustom template-time-format "%T"
  "*Time format for time in `template-time-format'."
  :type 'string
  :group 'template-simple)

(defcustom template-header-regexp
  '(("@(#)\\([^ \t\n]+\\)" . 1)
    ("^\\([^ \t]\\{,3\\}[ \t]+\\)\\([^ \t\n][^ \t\n]*\\)[ \t]+--" . 2))
  "Alist of regexps matching the file name in the header.
`car' is a regexp to match file header, `cdr' indicate which part
to replace with the file name."
  :type '(alist :key-type regexp :value-type integer)
  :group 'template-simple)

(defcustom template-query t
  "*Non-nil means ask user before expand template or update header."
  :type 'boolean
  :group 'template-simple)

(defvar template-skeleton-alist
  '(("point" _))
  "*Translation between parsed template to skeleton element.")

(defvar template-tempo-alist
  '(("point" p)
    ("p" p))
  "*Translation between parsed template to tempo element.")

 
;;; Internal variables
(defvar template-expand-function 'template-tempo-expand
  "Functions to expand parsed template.")
(put 'template-expand-function 'safe-local-variable 'functionp)

(defvar template-parens (cons "(>>>" "<<<)")
  "Open and close parenthesis.")
(put 'template-parens 'safe-local-variable 'consp)

(defvar template-file-name nil
  "Internal variable: full name of the file when template expanded.")

 
;;; Core functions
(defun template-compile ()
  "Parse current buffer to parsed template.
The template can have a file variable line, which can override default
global variable `template-parens' and `template-expand-function'.
The program fragment is surrounded by `template-parens', the escape
char `\\' is used for escape the open parenthesis.
The text in the parentheseses are `read' into a list. For example:
  (template-compile-string
   \";; -*- template-parens: (\\\"{\\\" . \\\"}\\\") -*- 
   (defun {p} ({p})
    \\\"{(read-from-minibuffer \\\"Document: \\\")}\\\"
    )
   \")

  is compile to a list like this:
  (\" (defun \" (p) \" (\" (p) \")
    \\\"\" ((read-from-minibuffer \"Document: \")) \"\\\"
    )
   \")
"
  (save-excursion
    (let ((vars (hack-local-variables-prop-line))
          (beg (point-min))
          (template-parens template-parens)
          open close templates escape)
      (goto-char (point-min))
      (when vars
        (mapc (lambda (var) (set (car var) (cdr var))) vars)
        ;; delete the file variable line for template-simple only
        (forward-line 1)
        (delete-region (point-min) (point)))
      (setq open (regexp-quote (car template-parens))
            close (regexp-quote (cdr template-parens)))
      (while (re-search-forward open nil t)
        (setq escape nil)
        (when (looking-back (concat "\\([^\\]\\|\\`\\)\\([\\]+\\)" open))
          (setq escape (match-string 2))
          (replace-match (substring escape 0 (/ (length escape) 2))
                         nil t nil 2)
          (goto-char (match-end 0))
          ;; if length of escape is odd, just a normal string, continue
          (setq escape (= (% (length escape) 2) 1)))
        (unless escape
          ;; parse template expansion
          (let ((expansion-start (point))
                state done forms)
            (push (buffer-substring-no-properties beg (- (point) (length (car template-parens))))
                  templates)
            (with-syntax-table emacs-lisp-mode-syntax-table
              (while (not done)
                (if (re-search-forward close nil t)
                    (progn
                      (setq state (parse-partial-sexp expansion-start (point)))
                      (if (nth 3 state) ; if inside a string, continue
                          ()
                        (setq done t)))
                  (error "Unmatch parentheses for line %d"
                         (line-number-at-pos expansion-start)))))
            (setq beg (point))
            (save-excursion
              (save-restriction
                (narrow-to-region expansion-start
                                  (- beg (length (cdr template-parens))))
                (goto-char (point-min))
                (while (not (eobp))
                  (push (read (current-buffer)) forms))))
            (push (nreverse forms) templates))))
      (push (buffer-substring-no-properties (point) (point-max)) templates)
      (nreverse templates))))

(defun template-compile-string (str)
  (with-temp-buffer
    (insert str)
    (template-compile)))

 
;;; Expand functions
(defun template-normal-name (name)
  "Convert all kinds of symbol name to standard name."
  (replace-regexp-in-string "_" "-" (downcase (symbol-name name))))

(defun template-expansion (elem)
  "Lookup name in `template-default-alist'.
If the elem is a list with length more"
  (if (stringp elem)
      (list elem)
    (if (= (length elem) 1)
        (progn
          (setq elem (car elem))
          (list
           (cond ((symbolp elem)
                  (or (cadr (assoc (template-normal-name elem)
                                   template-default-alist))
                      (and (boundp elem) (symbol-value elem))
                      `(or (cadr (assoc (template-normal-name ',elem)
                                        template-default-alist))
                           (let ((str (read-from-minibuffer (format "Replace '%S' with: " ',elem))))
                             (add-to-list 'template-default-alist
                                          (list (template-normal-name ',elem) str))
                             str))))
                 ;; ignore integer
                 ((integerp elem) "")
                 (t elem))))
      elem)))

(defmacro define-template-expander (name alist &rest body)
  "Define a new type of `template-expand-function'.
NAME is used to create a function template-<NAME>-expand.
ALIST can be a symbol or a form to return a list of symbol table add
to template-default-alist.
BODY is the code to expand and insert the template. the value of
variable TEMPLATE is the translated template. The element of parsed
template is translated by `template-expansion'"
  (declare (debug t) (indent 2))
  `(defun ,(intern (format "template-%s-expand" name)) (template)
     ,(format "Expand template by %s" name)
     (let ((template-default-alist
            (append ,alist template-default-alist))
           ;; save global variable 
           (template-expand-function
            ',(intern (format "template-%s-expand" name))))
       (if (stringp template)
           (setq template (template-compile-string template)))
       (setq template (apply 'append (mapcar 'template-expansion template)))
       ,@body)))

(define-template-expander skeleton template-skeleton-alist
  (skeleton-insert (cons nil template)))

(autoload 'tempo-insert-template "tempo")
(define-template-expander tempo template-tempo-alist
  (let ((tempo-template template))
    (tempo-insert-template 'tempo-template nil)))
 
;;; Exported commands
(defun template-derive-template ()
  "Derive which template file should use for current buffer."
  (when buffer-file-name
    (let ((ext (or (file-name-extension buffer-file-name)
                   (file-name-nondirectory buffer-file-name))))
      (locate-file "TEMPLATE." template-directory-list
                   (list ext (concat ext ".tpl"))))))

;; (defun template-include (name)
;;   (let ((file (locate-file name template-directory-list)))
;;     (when file (template-simple-expand-template file))))

;;;###autoload
(defun template-simple-expand-template (file)
  "Expand template in file.
Parse the template to parsed templates with `template-compile'.
Use `template-expand-function' to expand the parsed template."
  (interactive
   (list
    (let ((def (template-derive-template))
          file)
      (and def (setq def (file-name-nondirectory def)))
      (setq file
            (completing-read
             (if def
                 (format "Insert template(default %s): " def)
               "Insert template: ")
             (apply 'append (mapcar 'directory-files template-directory-list))
             nil t nil nil def))
      (locate-file file template-directory-list))))
  (let ((template-expand-function template-expand-function))
    (template-simple-expand
     (with-temp-buffer
       (insert-file-contents file)
       (template-compile)))))

;;;###autoload
(defun template-simple-expand (template)
  "Expand string TEMPLATE.
Parse the template to parsed templates with `template-compile'.
Use `template-expand-function' to expand the parsed template."
  ;; in case the template-expand-function is overide in template
  (let ((template-file-name (or buffer-file-name
                                (concat (file-name-as-directory default-directory)
                                        (buffer-name))))
        (template-expand-function template-expand-function)
        err)
    (condition-case err
        (progn
          (if (stringp template)
              (setq template (template-compile-string template)))
          (funcall template-expand-function template))
      (error (message "%s: %s" (car err) (cdr err))))))
 
;;; Commands for write template to string
(defun template-kill-ring-save (beg end)
  "Stringfy text in region, `yank' to see it."
  (interactive "r")
  (kill-new (format "%S" (buffer-substring-no-properties beg end)) nil))
 
;;; Provide addtional command in template.el
(defun template-simple-update-header ()
  (interactive)
  (when buffer-file-name
    (save-excursion
      (goto-char (point-min))
      (let ((end (progn (forward-line 3) (point)))
                                        ; check only first 3 lines
            (alist template-header-regexp)
            (fn (file-name-sans-versions
                 (file-name-nondirectory buffer-file-name)))
            case-fold-search)
        (while alist
          (goto-char (point-min))
          (if (re-search-forward (caar alist) end t)
              (progn
                (when (not (string= (match-string (cdar alist)) fn))
                  (if (or (null template-query)
                          (y-or-n-p (format "Update file header %s to %s? "
                                            (match-string (cdar alist))
                                            fn)))
                      (replace-match fn nil t nil (cdar alist))))
                (setq alist nil))
            (setq alist (cdr alist)))))))
  ;; return nil for calling other functions
  nil)
;; Hope auto-insert can add a test for template-derive-template
(defun template-auto-insert ()
  (and (not buffer-read-only)
       (or (eq this-command 'template-auto-insert)
           (and (bobp) (eobp)))
       (let ((file (template-derive-template)))
         (when file
           (switch-to-buffer (current-buffer))
           (if (or (null template-query)
                   (y-or-n-p (format "Use template %s? " file)))
               (template-simple-expand-template file)))))
  nil)

(if (boundp 'write-file-functions)
    (add-hook 'write-file-functions 'template-simple-update-header)
  (add-hook 'write-file-hooks 'template-simple-update-header))

(let ((hook (if (boundp 'find-file-hook)
                'find-file-hook
              'find-file-hooks)))
  ;; make template-auto-insert the last, so session history
  ;; will not affect point set by template
  (add-hook hook 'template-auto-insert t)
  ;; make auto-insert lower priority
  (when (memq 'auto-insert (symbol-value hook))
    (remove-hook hook 'auto-insert)
    (add-hook hook 'auto-insert t)))

(provide 'template-simple)
;;; template-simple.el ends here
