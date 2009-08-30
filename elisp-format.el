;;; elisp-format.el --- Format elisp code

;; Filename: elisp-format.el
;; Description: Format elisp code
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2009 Andy Stewart, all rights reserved.
;; Created: 2009-01-20 16:31:45
;; Version: 0.5.7
;; Last-Updated: 2009-03-10 02:05:45
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/elisp-format.el
;; Keywords:
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `newcomment' `cl'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package is format elisp code.
;; This package is format by itself, so you can view format effect.
;;
;; Below are commands you can use:
;;
;; `elisp-format-region'
;;      Format region or defun.
;; `elisp-format-buffer'
;;      Format buffer.
;; `elisp-format-file'
;;      Format file.
;; `elisp-format-file-batch'
;;      Format file with `batch'.
;; `elisp-format-directory'
;;      Format recursive elisp files in directory.
;; `elisp-format-directory-batch'
;;      Format recursive elisp files in directory with `batch'.
;; `elisp-format-dired-mark-files'
;;      Format dired marked files.
;; `elisp-format-library'
;;      Format library.
;;
;; Tips:
;;
;; If current mark is active, command `elisp-format-region'
;; will format region you select, otherwise it will format
;; `defun' around point.
;;
;; If you want format many files, you can marked them in dired,
;; and use command `elisp-format-dired-mark-files' to format
;; marked files in dired.
;;
;; You can format special file through
;; command `elisp-format-file'.
;;
;; By default, when you format `huge' file, it will
;; hang emacs.
;; You can also use command `elisp-format-file-batch'
;; make format process at background.
;;
;; You also can use command `elisp-format-directory'
;; format all recursive elisp files in special directory.
;;
;; By default, when you use command `elisp-format-directory'
;; format too many elisp files, will hang emacs.
;; You can also use command `elisp-format-directory-batch'
;; make format process at background.
;;
;; If you're sure lazy, you can use command `elisp-format-library'
;; format special library and don't need input long file path.
;;
;; Note:
;;
;; I can't ensure this package can perfect work with all situations.
;; So please let me know if you have suggestion or bug.
;;

;;; Installation:
;;
;; Put elisp-format.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'elisp-format)
;;
;; No need more.

;;; Customize:
;;
;; `elisp-format-batch-program'
;; The program name for execute batch command.
;;
;; `elisp-format-column'
;; The column number to truncate long line.
;;
;; `elisp-format-indent-comment'
;; Whether indent comment.
;;
;; `elisp-format-dired-mark-files-confirm'
;; Whether confirmation is needed to format dired marked files.
;;
;; `elisp-format-newline-keyword-addons-list'
;; The list contain addons keywords for newline.
;;
;; `elisp-format-newline-keyword-except-list'
;; The list contain except keywords for newline.
;;
;; `elisp-format-split-subexp-keyword-addons-list'
;; The list contain addons keywords that will split it's sub-expression.
;;
;; `elisp-format-split-subexp-keyword-except-list'
;; The list contain except keywords that will split it's sub-expression.
;;
;; `elisp-format-split-subexp-keyword-keep-alist'
;; The alist contain keep keyword when split it's sub-expression.
;;
;; All of the above can customize by:
;;      M-x customize-group RET elisp-format RET
;;

;;; Change log:
;;
;; 2009/03/10
;;      * Fix the bug of
;;      `elisp-format-directory-batch' and
;;      `elisp-format-file-batch'.
;;      * Improve `font-lock-add-keywords' format process.
;;      * Setup 100 as default value of `elisp-format-column'.
;;
;; 2009/02/14
;;      * New command `elisp-format-file-batch'
;;        format elisp file with `batch'.
;;      * Improve `defalias' format process.
;;      * Improve `with-output-to-temp-buffer' format process.
;;      * Improve `lambda' format process.
;;      * Improve `featurep' format process.
;;      * Improve `loop' format process.
;;      * Improve `:keyword' format process.
;;      * Improve ,@ format process.
;;      * Improve same level expression format process.
;;      * Improve string format process.
;;      * Remove option `elisp-format-split-last-sexp-string'.
;;        not necessary.
;;      * Improve performance.
;;      * Fix many bugs.
;;      * Refactory code.
;;
;; 2009/02/11
;;      * Fix documentation about `batch' mode.
;;      * New command `elisp-format-directory-batch',
;;        format elisp files with `batch'.
;;
;; 2009/02/10
;;      * Improve function `elisp-format-directory'.
;;
;; 2009/02/05
;;      * Don't format 'hide' elisp file in `elisp-format-directory'.
;;
;; 2009/02/03
;;      * Rename `elisp-format-dired' to `elisp-format-dired-mark-files'.
;;      * Add new command `elisp-format-directory'.
;;      * Display spend time for format each elisp file.
;;
;; 2009/01/31
;;      * Check parentheses before format.
;;        Remove option `elisp-format-check-parens'.
;;      * Add new option `elisp-format-indent-comment'.
;;      * Fix bug.
;;
;; 2009/01/27
;;      * Join standalone ")".
;;      * Format comments.
;;      * Fix `undo-outer-limit' bug.
;;      * Add new option `elisp-format-split-subexp-keyword-keep-alist'.
;;        Remove option `elisp-format-split-subexp-keyword-keep-1-list'.
;;      * Split same level expression.
;;      * Don't newline build-in keyword when previous expression
;;        is lisp data type keyword (beginning with ':').
;;      * Newline value field of define keyword if value field is longer
;;        than `elisp-format-column', otherwise, don't split.
;;      * Fix comment bug in `elisp-format-split-keyword-internal'.
;;
;; 2009/01/25
;;      * Support nesting format.
;;      * Support split all sub-expression and except first one.
;;      * Fix bug.
;;      * Fix doc.
;;
;; 2009/01/20
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; Bug
;;
;;

;;; TODO
;;
;; Format long lines:
;;      When occur huge files (above 7000 lines) and *deep* code level,
;;      will got ugly *code column* at right side.
;;

;;; Require
(require 'newcomment)
(eval-when-compile
  (require 'cl))

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar elisp-format-buildin-keywords-regexp
  (let (keywords-alist keywords-list keywords-regexp)
    ;; Get value of `font-lock-keywords'
    ;; with `emacs-lisp-mode'.
    (with-temp-buffer
      ;; Load `emacs-lisp-mode' and font-lock.
      (emacs-lisp-mode)
      (setq font-lock-mode t)
      (font-lock-fontify-buffer)
      (setq keywords-alist (cadr font-lock-keywords)))
    ;; Get regexp string and add to `keywords-list'.
    (dolist (element keywords-alist)
      (setq element
            (if (car-safe element)
                (car element)
              element))
      (when (stringp element)
        ;; Remove "(" from front of regexp.
        (setq element (replace-regexp-in-string "^(" "" element))
        (push element keywords-list)))
    ;; Concat all regexp string in `keywords-list'.
    (dolist (element keywords-list)
      (setq keywords-regexp (concat keywords-regexp
                                    (cond ((eq element (first keywords-list))
                                           (format "\\(%s\\|" element))
                                          ((eq element (car (last
                                                             keywords-list)))
                                           (format "%s\\)" element))
                                          (t (format "%s\\|" element))))))
    ;; Return keywords regexp.
    keywords-regexp)
  "The regular expression that match `build-in' keywords.")

(defvar elisp-format-define-keyword-regexp nil
  "The regexp expression for `elisp-format-define-keyword-list'.")

(defvar elisp-format-newline-keyword-addons-regexp nil
  "The regular expression for `elisp-format-newline-keyword-addons-list'.")

(defvar elisp-format-split-subexp-keyword-addons-regexp nil
  "The regular expression for `elisp-format-split-subexp-keyword-addons-list'.")

(defvar elisp-format-debug-mode nil
  "Display details debug information when this option is `non-nil'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup elisp-format nil
  "Format elisp code."
  :group 'tools)

(defcustom elisp-format-batch-program "emacs"
  "The program name for execute batch command."
  :type 'string
  :group 'elisp-format)

(defcustom elisp-format-column 100
  "The column number to truncate long line."
  :type 'integer
  :group 'elisp-format)

(defcustom elisp-format-indent-comment t
  "Whether indent comment.
If `non-nil', will indent comment.
Default it t."
  :type 'boolean
  :group 'elisp-format)

(defcustom elisp-format-dired-mark-files-confirm t
  "Whether confirmation is needed to format dired mark files.
If `non-nil' will notify you before format dired mark files.
Default is t."
  :type 'boolean
  :group 'elisp-format)

(defcustom elisp-format-define-keyword-list
  '(
    ;; Generic.
    "defun" "defun*" "defsubst" "defmacro" "defadvice" "define-skeleton"
    "define-minor-mode" "define-global-minor-mode"
    "define-globalized-minor-mode" "define-derived-mode" "define-generic-mode"
    "define-compiler-macro" "define-modify-macro" "defsetf"
    "define-setf-expander" "define-method-combination" "defgeneric" "defmethod"
    "defalias"
    ;; Variable.
    "defvar" "defconst" "defconstant" "defcustom" "defparameter"
    "define-symbol-macro"
    ;; Types.
    "defgroup" "deftheme" "deftype" "defstruct" "defclass" "define-condition"
    "define-widget" "defface" "defpackage")
  "This list contain define-keywords for format.
Copy those value from `lisp-imenu-generic-expression' define.
Or have a exist variable contain those define-keywords?"
  :type 'list
  :set (lambda (symbol value)
         (set symbol value)
         (setq elisp-format-define-keyword-regexp (regexp-opt value)))
  :group 'elisp-format)

(defcustom elisp-format-newline-keyword-addons-list
  '("interactive" "setq" "set" "buffer-substring"
    "buffer-substring-no-properties")
  "This list contain addons keywords for newline.
The line beginning match keywords in this list will be newline."
  :type 'list
  :set (lambda (symbol value)
         (set symbol value)
         (setq elisp-format-newline-keyword-addons-regexp (regexp-opt value)))
  :group 'elisp-format)

(defcustom elisp-format-newline-keyword-except-list '()
  "The list contain except keywords for newline.
The line beginning match keywords in this list won't be newline."
  :type 'list
  :group 'elisp-format)

(defcustom elisp-format-split-subexp-keyword-addons-list
  '("and" "or" "buffer-substring" "buffer-substring-no-properties"
    "font-lock-add-keywords")
  "The list contain addons keywords that will split it's sub-expression."
  :type 'list
  :set (lambda (symbol value)
         (set symbol value)
         (setq elisp-format-split-subexp-keyword-addons-regexp (regexp-opt
                                                                value)))
  :group 'elisp-format)

(defcustom elisp-format-split-subexp-keyword-except-list
  '("provide" "require" "loop" "throw" "featurep")
  "The list contain except keywords that won't split it's sub-expression."
  :type 'list
  :group 'elisp-format)

(defcustom elisp-format-split-subexp-keyword-keep-alist
  '((1 . ("and" "or" "let" "let*" "while" "when" "catch" "unless" "if" "dolist"
          "dotimes" "lambda" "cond" "condition-case" "with-current-buffer"
          "with-temp-message" "with-selected-window"
          "with-output-to-temp-buffer" "with-selected-frame"))
    (2 . (
          ;; Generic.
          "defun" "defun*" "defsubst" "defmacro" "defadvice" "define-skeleton"
          "define-minor-mode" "define-global-minor-mode"
          "define-globalized-minor-mode" "define-derived-mode"
          "define-generic-mode" "define-compiler-macro" "define-modify-macro"
          "defsetf" "define-setf-expander" "define-method-combination"
          "defgeneric" "defmethod" "defalias"
          ;; Variable.
          "defvar" "defconst" "defconstant" "defcustom" "defparameter"
          "define-symbol-macro"
          ;; Types.
          "defgroup" "deftheme" "deftype" "defstruct" "defclass"
          "define-condition" "define-widget" "defface" "defpackage" ))
    (5 . ("loop")))
  "The list contain keywords that will be split it's sub-expression."
  :type 'alist
  :group 'elisp-format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elisp-format-region (&optional start end)
  "Format current region or buffer.
This function will format region from START to END.
Or try to format `defun' around point."
  (interactive)
  (let ((start-time (elisp-format-get-current-time)))
    ;; Display startup message.
    (message "Format %s ..." (buffer-name))
    ;; Check parentheses before format.
    (check-parens)
    ;; Format start.
    (save-excursion
      ;; Get format area.
      (unless (and start
                   end)
        ;; Get format area.
        (if mark-active
            ;; Get activate mark region.
            (progn
              (setq start (region-beginning))
              (setq end (region-end))
              (deactivate-mark))
          ;; Or get current function.
          (setq start
                (progn
                  (beginning-of-defun)
                  (point)))
          (setq end
                (progn
                  (end-of-defun)
                  (point)))))
      (setq end (copy-marker end))
      ;; Delete un-necessary whitespace.
      (elisp-format-delete-whitespace start end)
      (when elisp-format-debug-mode
        (message "`elisp-format-delete-whitespace' module completed."))
      ;; Split same level expression.
      (elisp-format-split-same-level-expression start end)
      (when elisp-format-debug-mode
        (message "`elisp-format-split-same-level-expression' module completed."))
      ;; Split list data type.
      (elisp-format-split-list-data-type start end)
      (when elisp-format-debug-mode
        (message "`elisp-format-split-list-data-type' module completed."))
      ;; Split keyword.
      (elisp-format-split-keyword start end)
      (when elisp-format-debug-mode
        (message "`elisp-format-split-keyword' module completed."))
      ;; Split keyword value.
      (elisp-format-split-define-assoc-value start end)
      (when elisp-format-debug-mode
        (message "`elisp-format-split-define-assoc-value' module completed."))
      ;; Split sub-expression.
      (elisp-format-split-subexp start end)
      (when elisp-format-debug-mode
        (message "`elisp-format-split-subexp' module completed."))
      ;; Split and indent lines.
      (goto-char start)
      (while (< (point) end)
        ;; Format non-blank line.
        (unless (and (bolp)
                     (eolp))
          ;; Split and indent lines.
          (elisp-format-split-and-indent))
        ;; Forward line.
        (forward-line +1))
      (when elisp-format-debug-mode
        (message "`elisp-format-split-and-indent' module completed."))
      ;; Join standalone ")".
      (elisp-format-join-close-parentheses)
      (when elisp-format-debug-mode
        (message "`elisp-format-join-close-parentheses' module completed."))
      ;; Indent comment.
      (if elisp-format-indent-comment
          (elisp-format-indent-comment-region start end))
      (when elisp-format-debug-mode
        (message "`elisp-format-indent-comment-region' module completed."))
      ;; Display completed message and spend time.
      (message "Format %s completed (%ss)." (buffer-name)
               (/ (- (elisp-format-get-current-time) start-time) 1000000)))))

(defun elisp-format-buffer ()
  "Format current buffer."
  (interactive)
  (elisp-format-region (point-min)
                       (point-max)))

(defun elisp-format-file (filename)
  "Format file with FILENAME."
  (interactive "fFile name: ")
  (with-current-buffer (find-file-noselect filename)
    (elisp-format-buffer)))

(defun elisp-format-file-batch (filename &optional surpress-popup-window)
  "Format elisp FILENAME.
But instead in `batch-mode'.
If SURPRESS-POPUP-WINDOW is non-nil, don't show output window."
  (interactive "fFile name: ")
  ;; Format elisp file.
  (elisp-format-batch-command "elisp-format-file-batch"
                              elisp-format-batch-program
                              (format
                               "-batch -l %s --eval=\"(progn (require 'elisp-format) (elisp-format-file \\\"%s\\\"))\""
                               ;; Use `find-library-name' to find load path.
                               (find-library-name "elisp-format") filename)
                              surpress-popup-window))

(defun elisp-format-directory (dir)
  "Format recursive elisp files under DIR."
  (interactive "DDirectory: ")
  (let ((suffix (format "^.*\\.el%s$" (regexp-opt load-file-rep-suffixes))))
    (dolist (file (directory-files dir t))
      (if (file-directory-p file)
          ;; Don't match . or .. directory.
          (unless (string-match "^\\.\\.?$" (file-name-nondirectory file))
            ;; Find files in sub-directory.
            (elisp-format-directory file))
        ;; Not backup file.
        (unless (string-match "^\\.?#" (file-name-nondirectory file))
          ;; Match elisp file or it's compress format.
          (if (string-match suffix (file-name-nondirectory file))
              (elisp-format-file file)))))))

(defun elisp-format-directory-batch (dir &optional surpress-popup-window)
  "Format recursive elisp files under DIR.
But instead in `batch-mode'.
If SURPRESS-POPUP-WINDOW is non-nil, don't show output window."
  (interactive "DDirectory: ")
  ;; Format elisp file under specify directory.
  (elisp-format-batch-command "elisp-format-directory-batch"
                              elisp-format-batch-program
                              (format
                               "-batch -l %s --eval=\"(progn (require 'elisp-format) (elisp-format-directory \\\"%s\\\"))\""
                               ;; Use `find-library-name' to find load path.
                               (find-library-name "elisp-format") dir)
                              surpress-popup-window))

(defun elisp-format-dired-mark-files ()
  "Format dired mark files."
  (interactive)
  (if (or (not elisp-format-dired-mark-files-confirm)
          (yes-or-no-p "Do you want format marked files? "))
      (dolist (filename (dired-get-marked-files))
        (elisp-format-file filename))))

(defun elisp-format-library (library)
  "Format LIBRARY."
  (interactive (list
                (let* ((dirs load-path)
                       (suffixes (find-library-suffixes)))
                  (completing-read "Library name: " (apply-partially
                                                     'locate-file-completion-table
                                                     dirs suffixes)))))
  (elisp-format-file (find-library-name library)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elisp-format-split-keyword-internal (subexp)
  "Split keyword.
SUBEXP is sub-expression number for regexp match."
  (let (match-beg-position match-length match-keyword)
    ;; Record match beginning position and length.
    ;; make below setup before `elisp-format-in-string-p'
    ;; or `elisp-format-in-comment-p' to get correct
    ;; match value.
    (setq match-beg-position (match-beginning subexp))
    (setq match-length (length (match-string subexp)))
    ;; Make sure jump to match beginning position first,
    ;; to avoid search in `string' area.
    (goto-char match-beg-position)
    ;; Newline handle.
    (cond
     ;; Move to next line when in comment area.
     ((or
       (elisp-format-in-comment-p)
       ;; or beginning of comment line.
       (elisp-format-beginning-of-comment-line-p))
      (forward-line +1))
     ;; Move to end position of string.
     ((elisp-format-in-string-p)
      (goto-char (elisp-format-string-end-position)))
     ;; Newline match keyword sexp.
     (t
      ;; Get match-keyword.
      (setq match-keyword (elisp-format-get-match-keyword))
      ;; Newline when `match-keyword' is not match
      ;; list `elisp-format-newline-except-keywords-list'.
      (unless (member match-keyword elisp-format-newline-keyword-except-list)
        ;; Newline beginning position when current
        ;; point not first non-blank character of line,
        ;; and previous subexp not lisp data type keyword.
        (unless (or (elisp-format-first-non-blank-of-line-p)
                    (elisp-format-prev-sexp-list-data-type-p))
          (newline))
        ;; Newline end position when current
        ;; sexp not last sexp of line.
        (forward-list)                  ;Move to after of ")"
        (unless (or (elisp-format-last-sexp-of-line-p))
          (newline))                    ;Newline when not last sexp of line.
        )
      ;; Jump to end position of match keyword,
      ;; for continue search next keyword.
      (goto-char match-beg-position)
      (forward-char match-length)))))

(defun elisp-format-split-subexp-internal (subexp)
  "Split sub-expression that match keyword.
SUBEXP is sub-expression number for regexp match."
  (let (match-beg-position match-length match-keyword subsexp-start subsexp-end
                           subsexp-keep (subsexp-counter 0))
    ;; Record match beginning position and length.
    ;; make below setup before `elisp-format-in-string-p'
    ;; or `elisp-format-in-comment-p' to get correct
    ;; match value.
    (setq match-beg-position (match-beginning subexp))
    (setq match-length (length (match-string subexp)))
    ;; Make sure jump to match beginning position first,
    ;; to avoid search in `string' area.
    (goto-char match-beg-position)
    ;; Newline handle.
    (cond
     ;; Move to next line when in comment area.
     ((or
       (elisp-format-in-comment-p)
       ;; or beginning of comment line.
       (elisp-format-beginning-of-comment-line-p))
      (forward-line +1))
     ;; Move to end position of string.
     ((elisp-format-in-string-p)
      (goto-char (elisp-format-string-end-position)))
     ;; Newline match keyword sexp.
     (t
      ;; Get beginning and subsexp-end position of subsexp.
      (setq subsexp-start match-beg-position)
      (forward-list)
      (setq subsexp-end (copy-marker (point)))
      ;; Get match-keyword.
      (goto-char subsexp-start)
      (setq match-keyword (elisp-format-get-match-keyword))
      ;; Don't match except keyword.
      (unless (member match-keyword
                      elisp-format-split-subexp-keyword-except-list)
        ;; Go beginning position of subsexp.
        (goto-char subsexp-start)
        (search-forward-regexp " \\|$" nil t)
        ;; Format sub-expression.
        (catch 'reach-last-sexp
          (while (< (point) subsexp-end) ;not out `subsexp-end'
            ;; Skip whitespace before keyword.
            (skip-chars-forward " \t\n")
            ;; Skip any comment between two sub-expression.
            (while (string-equal (string (char-after)) ";")
              (move-end-of-line 1)
              (skip-chars-forward " \t\n"))
            ;; Increment subsexp counter.
            (incf subsexp-counter)
            ;; Get keep status.
            (setq subsexp-keep (elisp-format-is-keep-subexp match-keyword
                                                            subsexp-counter))
            ;; Don't newline if match keep keyword.
            (unless (or subsexp-keep
                        (elisp-format-first-non-blank-of-line-p)
                        (elisp-format-prev-sexp-list-data-type-p))
              (newline))
            ;; Forward sexp.
            (unless
                ;; Try to forward sexp and newline.
                ;; or jump out `while' loop.
                (ignore-errors
                  (forward-sexp)
                  ;; Don't newline if match keep keyword.
                  (unless (or subsexp-keep
                              (elisp-format-last-sexp-of-line-p)
                              (elisp-format-prev-sexp-list-data-type-p))
                    (newline))
                  ;; Make sure `t' at last.
                  t)
              (throw 'reach-last-sexp "Can't find next sexp")))))
      ;; Move to end of next keyword for continue search.
      (goto-char subsexp-start)
      (search-forward-regexp " \\|$" nil t)))))

(defun elisp-format-split-and-indent ()
  "Split and indent lines."
  ;; Indent current line first.
  (funcall indent-line-function)
  ;; Split others.
  (let ((original-line (elisp-format-line-number)))
    (when elisp-format-debug-mode
      (message "%s" original-line))
    ;; Move to real end position of current line,
    ;; ignore trailing whitespace.
    (call-interactively 'move-end-of-line)
    (skip-chars-backward " \t")
    ;; Split current line when length
    ;; longer than `elisp-format-column'.
    (when (> (current-column) elisp-format-column)
      ;; Move to `elisp-format-column'.
      (move-to-column elisp-format-column t)
      (cond
       ;; In comment area.
       ((elisp-format-in-comment-p)
        ;; Return original line before parse
        ;; if current position is in comment.
        (goto-line original-line))
       ;; In string area.
       ((elisp-format-in-string-p)
        ;; Split current string.
        (elisp-format-split-string)
        (when elisp-format-debug-mode
          (message "Format string at %s completed." original-line)))
       ;; In code area.
       (t
        ;; Split current code.
        (elisp-format-split-code original-line)
        (when elisp-format-debug-mode
          (message "Format code at %s completed." original-line)))))))

(defun elisp-format-split-string ()
  "Split current string with appropriate column.
Default this function will split current line to make
end column less than value of `elisp-format-column'.
And this action just advice, it don't split deep
if current line can't split any more."
  (let (string-length indent-length)
    ;; Jump to start position of string.
    (goto-char (elisp-format-string-beg-position))
    ;; Newline and indent if
    ;; string is not first sexp,
    ;; or not first subsexp of current sexp.
    (unless (or (elisp-format-first-non-blank-of-line-p)
                (elisp-format-first-subsexp-of-sexp-p))
      (newline-and-indent))
    ;; Record indent length.
    (setq indent-length (- (point)
                           (line-beginning-position)))
    ;; Record string length.
    (forward-char +1)
    (setq string-length (elisp-format-string-length))
    ;; Jump to end position of string.
    (goto-char (elisp-format-string-end-position))
    ;; Parse deep if string is not last non-blank character of line.
    (unless (elisp-format-last-non-blank-of-line-p)
      ;; Parse deep if string end position is still
      ;; longer than `elisp-format-column'
      (if (> (+ indent-length string-length) elisp-format-column)
          ;; Newline string when string is not last sexp of line.
          (unless (elisp-format-last-sexp-of-line-p)
            (newline-and-indent)
            (forward-line -1))
        ;; Otherwise return previous line.
        (forward-line -1)))))

(defun elisp-format-split-code (original-line)
  "Split current code with appropriate column.
Default this function will split current line to make
end column less than value of `elisp-format-column'.
And this action just advice, it don't split deep
if current line can't split any more.
Argument ORIGINAL-LINE is position before parse."
  ;; Ignore trailing whitespace after `elisp-format-column'.
  (skip-chars-forward " \t")
  ;; Jump to start position of previous sexp.
  (search-backward-regexp " \\|^" nil t)
  (skip-chars-forward " \t")
  ;; Newline and indent if sexp
  ;; is not define-keyword association name,
  ;; and is not first non-blank character of line.
  (unless (or (elisp-format-define-keyword-assoc-name-p)
              (elisp-format-first-non-blank-of-line-p))
    (newline-and-indent))
  ;; Jump to end position of current sexp.
  (search-forward-regexp " \\|$" nil t)
  (skip-chars-backward " \t")
  ;; Parse deep if current sexp is not last
  ;; non-blank character of line.
  (unless (elisp-format-last-non-blank-of-line-p)
    (if (> (current-column) elisp-format-column)
        ;; Newline and indent if sexp end position
        ;; still after `elisp-format-column'.
        (progn
          (newline-and-indent)
          (forward-line -1))
      ;; Otherwise return original line before parse.
      (goto-line original-line))))

(defun elisp-format-line-number (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location."
  (let ((original-position (or pos
                               (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char original-position)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun elisp-format-delete-whitespace (&optional start end)
  "Delete un-necessary whitespace between START and END."
  ;; Get area.
  (or start
      (setq start (point-min)))
  (or end
      (setq end (point-max)))
  ;; Delete trailing whitespace.
  (delete-trailing-whitespace)
  ;; Delete whitespace between code lines.
  (let (search-start search-end)
    (goto-char start)
    (while (re-search-forward "\n\\s-+" end t)
      ;; Record `search-start' and `search-end'
      ;; before `elisp-format-in-string-p' for correct search.
      (setq search-start (match-beginning 0))
      (setq search-end (match-end 0))
      ;; Not in string.
      (unless (elisp-format-in-string-p)
        ;; Not comment at front or after
        ;; search data.
        (if (and
             (progn
               (goto-char search-start)
               (not (elisp-format-in-comment-p)))
             (progn
               (goto-char search-end)
               (forward-char +1)
               (not (elisp-format-in-comment-p))))
            ;; Join line.
            (progn
              (goto-char search-end)
              (join-line))
          ;; Move to `search-end'.
          (goto-char search-end)))))
  ;; Delete all whitespace after "(" in code area.
  (goto-char start)
  (while (re-search-forward "(\\s-+" end t)
    (unless (and (elisp-format-in-comment-p)
                 (elisp-format-in-string-p))
      ;; Remove whitespace after '('.
      (kill-region (1+ (match-beginning 0))
                   (match-end 0)))))

(defun elisp-format-split-subexp (&optional start end)
  "Split sub-expression format START to END."
  ;; Get area.
  (or start
      (setq start (point-min)))
  (or end
      (setq end (point-max)))
  ;; Split sub-expression.
  (goto-char start)
  (let ((search-regexp (concat "\\s-*\\([`',]?@?(+" "\\("
                               elisp-format-buildin-keywords-regexp "\\|"
                               elisp-format-split-subexp-keyword-addons-regexp
                               "\\)" "\\)\\b[^\\B\\s_-]")))
    (while (re-search-forward search-regexp end t)
      (elisp-format-split-subexp-internal 1))))

(defun elisp-format-split-define-assoc-value (&optional start end)
  "Split association value of keyword format START to END.
Keyword is match in `elisp-format-define-keyword-list'."
  ;; Get area.
  (or start
      (setq start (point-min)))
  (or end
      (setq end (point-max)))
  ;; Split association keyword value.
  (goto-char start)
  (let ((search-regexp (concat "\\s-*\\([`',]?@?(+"
                               elisp-format-define-keyword-regexp
                               "\\)\\b[^\\B\\s_-]")) search-start value-start
                               value-length)
    (while (re-search-forward search-regexp end t)
      (setq search-start (match-end 1))
      ;; Search deep when not in comment or string.
      (unless (and (elisp-format-in-comment-p)
                   (elisp-format-in-string-p))
        ;; Search deep.
        (unless
            ;; Use `ignore-errors' avoid reach last
            ;; sub-expression throw error.
            (ignore-errors
              (forward-sexp)
              (skip-chars-forward " \t\n")
              (setq value-start (point))
              (forward-sexp)
              (setq value-length (- (point) value-start))
              (goto-char value-start)
              ;; Newline if keyword value
              ;; not first non-blank character of line.
              ;; and value length longer than `elisp-format-column'.
              (if (and (not (elisp-format-first-non-blank-of-line-p))
                       (> (+ (current-column) value-length) elisp-format-column))
                  (newline)))
          ;; Otherwise goto search start.
          (goto-char search-start))))))

(defun elisp-format-split-same-level-expression (&optional start end)
  "Split same level expression from START to END."
  ;; Get area.
  (or start
      (setq start (point-min)))
  (or end
      (setq end (point-max)))
  ;; Split same level expression.
  (goto-char start)
  (let (search-end)
    (while (re-search-forward "\\()\\|\\]\\)\\(\\s-*\\)[`',]?@?\\((\\|\\[\\)"
                              end t)
      ;; Record search end before
      ;; `elisp-format-in-comment-p'
      ;; or
      ;; `elisp-format-in-string-p'.
      (setq search-end (match-end 2))
      ;; When not in comment or string.
      (unless (or (elisp-format-in-comment-p)
                  (elisp-format-in-string-p))
        ;; Newline second expression.
        (goto-char search-end)
        (newline)))))

(defun elisp-format-split-list-data-type (&optional start end)
  "Split list type data (:keyword) from START to END."
  ;; Get area.
  (or start
      (setq start (point-min)))
  (or end
      (setq end (point-max)))
  ;; Split list data type.
  (goto-char start)
  (let (search-beg search-end)
    (while (re-search-forward "'*\\B:+[^: \n]+\\b" end t)
      ;; Record search end before
      ;; `elisp-format-in-comment-p'
      ;; or
      ;; `elisp-format-in-string-p'.
      (setq search-beg (match-beginning 0))
      (setq search-end (match-end 0))
      ;; When not in comment or string.
      (unless (or (elisp-format-in-comment-p)
                  (elisp-format-in-string-p))
        ;; Jump to search beginning position.
        (goto-char search-beg)
        ;; Try to newline `:keyword'.
        (save-excursion
          ;; Not first non-blank of line
          ;; and not first subsexp of current sexp.
          (unless (or (elisp-format-first-non-blank-of-line-p)
                      (elisp-format-first-subsexp-of-sexp-p))
            ;; Newline `:keyword' if it is
            ;; second sub-sexp of current sexp.
            (ignore-errors
              (skip-chars-backward " \t")
              (backward-sexp)
              (unless (elisp-format-first-subsexp-of-sexp-p)
                (goto-char search-beg)
                (newline)))
            ;; Newline `:keyword' if next sexp
            ;; is also `:keyword'.
            (goto-char search-beg)
            (forward-sexp)
            (skip-chars-forward " \t\n")
            (when (looking-at "'*:")
              (goto-char search-beg)
              (newline))))
        ;; Newline list data type `value'.
        (forward-sexp)                  ;skip `:keyword'
        (skip-chars-forward " \t")      ;skip blank
        ;; Not `:keyword' at behind
        ;; and not last sexp.
        (unless (looking-at "'*\\(:\\|)\\)")
          (ignore-errors
            ;; Forward sexp.
            (forward-sexp)
            ;; Not last sexp.
            (unless (elisp-format-last-sexp-of-line-p)
              ;; Newline `:keyword' if next
              ;; subsexp is not last one in current sexp.
              (forward-sexp)
              (unless (elisp-format-last-sexp-of-line-p)
                (backward-sexp)
                (newline)))))
        ;; Jump to match end for continue search.
        (goto-char search-end)))))

(defun elisp-format-split-keyword (&optional start end)
  "Split keyword from START to END."
  ;; Get area.
  (or start
      (setq start (point-min)))
  (or end
      (setq end (point-max)))
  ;; Split keywords.
  (goto-char start)
  (let ((search-regexp (concat "\\s-*\\([`',]?@?(+" "\\("
                               elisp-format-buildin-keywords-regexp "\\|"
                               elisp-format-newline-keyword-addons-regexp "\\)"
                               "\\)\\b[^\\B\\s_-]")))
    (while (re-search-forward search-regexp end t)
      (elisp-format-split-keyword-internal 1))))

(defun elisp-format-join-close-parentheses (&optional start end)
  "Join standalone close parentheses from START to END."
  ;; Get area.
  (or start
      (setq start (point-min)))
  (or end
      (setq end (point-max)))
  ;; Join standalone close parentheses.
  (let (search-start search-end)
    (goto-char start)
    (while (re-search-forward "\\(\\s-*\n\\s-*\\))" end t)
      ;; Record `search-start' and `search-end'
      ;; before `elisp-format-in-string-p' for correct search.
      (setq search-start (match-beginning 0))
      (setq search-end (match-end 0))
      ;; Not in string.
      (unless (elisp-format-in-string-p)
        ;; Not comment at front or after
        ;; search data.
        (if
            (progn
              (goto-char search-start)
              (not (elisp-format-in-comment-p)))
            ;; Join line.
            (progn
              (goto-char search-end)
              (backward-char 1)
              (join-line))
          ;; Move to `search-end'.
          (goto-char search-end))))))

(defun elisp-format-indent-comment-region (&optional start end)
  "Indent comment format START to END."
  ;; Get area.
  (or start
      (setq start (point-min)))
  (or end
      (setq end (point-max)))
  ;; Indent area.
  (goto-char start)
  (while (< (point) end)
    (if (comment-search-forward end t)
        (comment-indent)
      (goto-char end))))

(defun elisp-format-get-match-keyword ()
  "Get match keyword after point."
  (save-excursion
    (search-forward "(")
    (symbol-name (symbol-at-point))))

(defun elisp-format-define-keyword-assoc-name-p ()
  "Return t if current string around point is association keyword name.
And keyword in match `elisp-format-define-keyword-list'."
  (let (define-keyword-name)
    (if (looking-back "^\\s-*(\\([^() ]+\\)\\s-+")
        (progn
          (setq define-keyword-name (match-string 1))
          (member define-keyword-name elisp-format-define-keyword-list))
      nil)))

(defun elisp-format-define-keyword-assoc-value-p ()
  "Return t if current string around point is association keyword value.
And keyword in match `elisp-format-define-keyword-list'."
  (save-excursion
    (let ((original-position (point)))
      (if ;; In define area.
          (beginning-of-defun)
          ;; In value field of define area.
          (progn
            (search-forward-regexp " \\|$" nil t)
            (forward-sexp 1)
            (skip-chars-forward " \t\n")
            (if (and (>= original-position (point))
                     (<= original-position
                         (progn
                           (forward-list)
                           (point))))
                t
              nil))
        nil))))

(defun elisp-format-first-non-blank-of-line-p ()
  "Return t if point is first non-blank character of line.
Otherwise return nil."
  (let ((current-point (point)))
    (save-excursion
      (back-to-indentation)
      (equal current-point (point)))))

(defun elisp-format-last-non-blank-of-line-p ()
  "Return t if point is last non-blank character of line.
Otherwise return nil."
  (let ((current-point (point)))
    (save-excursion
      (call-interactively 'move-end-of-line)
      (skip-chars-backward " \t")
      (equal current-point (point)))))

(defun elisp-format-first-subsexp-of-sexp-p ()
  "Return t if point is first sub-sexp of sexp.
Otherwise return nil."
  (looking-back "([ \t\n]*"))

(defun elisp-format-last-sexp-of-line-p ()
  "Return t if point is last sexp of line.
Otherwise return nil."
  (looking-at "\\([ \t\n)]*$\\|\\s-*;.*$\\)"))

(defun elisp-format-prev-sexp-list-data-type-p ()
  "Return t if previous sexp is keyword beginning with ':'.
Otherwise return nil."
  (save-excursion
    (ignore-errors
      (backward-sexp)
      (string-match "^:[^ ]+$"
                    (buffer-substring-no-properties
                     (point)
                     (progn
                       (search-forward-regexp " \\|$" nil t)
                       (skip-chars-backward " \t")
                       (point)))))))

(defun elisp-format-is-keep-subexp (keyword index)
  "Return `non-nil' if KEYWORD is keep keyword.
This function will search KEYWORD with INDEX
in `elisp-format-split-subexp-keyword-keep-list';
return `non-nil', when search match keyword.
Otherwise, return nil."
  (catch 'match
    (loop for (keep-index . keep-keyword-list) in
          elisp-format-split-subexp-keyword-keep-alist do
          (when (and (<= index keep-index)
                     (member keyword keep-keyword-list))
            (throw 'match t)))
    nil))

(defun elisp-format-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    ;; Calling PARSE-PARTIAL-SEXP will advance the point to its second
    ;; argument (unless parsing stops due to an error, but we assume it
    ;; won't in elisp-format-mode).
    (parse-partial-sexp (point) point)))

(defun elisp-format-in-string-p (&optional state)
  "True if the parse STATE is within a double-quote-delimited string.
If no parse state is supplied, compute one from the beginning of the
  defun to the point."
  ;; 3. non-nil if inside a string (the terminator character, really)
  (and (nth 3 (or state
                  (elisp-format-current-parse-state)))
       t))

(defun elisp-format-in-comment-p (&optional state)
  "True if parse state STATE is within a comment.
If no parse state is supplied, compute one from the beginning of the
  defun to the point."
  ;; 4. nil if outside a comment, t if inside a non-nestable comment,
  ;;    else an integer (the current comment nesting)
  (and (nth 4 (or state
                  (elisp-format-current-parse-state)))
       t))

(defun elisp-format-string-start+end-points (&optional state)
  "Return a cons of the points of open and close quotes of the string.
The string is determined from the parse state STATE, or the parse state
  from the beginning of the defun to the point.
This assumes that `elisp-format-in-string-p' has already returned true, i.e.
  that the point is already within a string."
  (save-excursion
    ;; 8. character address of start of comment or string; nil if not
    ;;    in one
    (let ((start (nth 8 (or state
                            (elisp-format-current-parse-state)))))
      (goto-char start)
      (forward-sexp 1)
      (cons start (1- (point))))))

(defun elisp-format-beginning-of-comment-line-p ()
  "Return t if current point is beginning of comment line.
And current line only have comment.
Otherwise return nil."
  (and (bolp)
       (looking-at "\\s-*;")))

(defun elisp-format-string-beg-position ()
  "Return the beginning position of string."
  (car (elisp-format-string-start+end-points)))

(defun elisp-format-string-end-position ()
  "Return the end position of string."
  (1+ (cdr (elisp-format-string-start+end-points))))

(defun elisp-format-string-length ()
  "Return string length."
  (- (elisp-format-string-end-position)
     (elisp-format-string-beg-position)))

(defun elisp-format-get-current-time ()
  "Get current time (microsecond)."
  (let ((time (current-time)))
    (+ (* 1000000 (string-to-number (concat (number-to-string (nth 0 time))
                                            (number-to-string (nth 1 time)))))
       (nth 2 time))))

(defun elisp-format-batch-command (name command command-args &optional surpress-popup-window)
  "Run special `batch' command.
NAME is sub-process buffer title.
COMMAND is command running under `batch'.
COMMAND-ARGS is command arguments for COMMAND.
If SURPRESS-POPUP-WINDOW is non-nil, don't show output window."
  (let* ((time-now (current-time))
         ;; Get unique buffer.
         (output-buffer (format "*%s<%s-%s-%s>" name (nth 0 time-now)
                                (nth 1 time-now)
                                (nth 2 time-now))))
    ;; Start sub-process to handle format work.
    (start-process-shell-command name output-buffer command command-args)
    ;; Popup window when option `surpress-popup-window' is nil.
    (unless surpress-popup-window
      (pop-to-buffer output-buffer))))

(provide 'elisp-format)

;;; elisp-format.el ends here

;;; LocalWords:  ss fFile DDirectory dirs pos args
