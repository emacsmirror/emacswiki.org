;;; qml-mode.el --- Mode for Qt QML file

;; Filename: qml-mode.el
;; Description: Mode for Qt QML file
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2013 ~ 2014, Andy Stewart, all rights reserved.
;; Created: 2013-12-31 21:23:56
;; Version: 0.3
;; Last-Updated: 2014-05-12 21:16:14
;;           By: Andres Gomez Garcia
;; URL: http://www.emacswiki.org/emacs/download/qml-mode.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;;
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
;; Mode for Qt QML file
;;

;;; Installation:
;;
;; Put qml-mode.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'qml-mode)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET qml-mode RET
;;

;;; Change log:
;;
;; 2014/05/12
;;     * Fixed qml-indent-line
;;     
;; 2014/04/10
;;      * Improve qml-font-lock-keywords.
;;
;; 2014/04/09
;;      * Derived-mode from text-mode, and not prog-mode.
;;      * Fixed syntax highlight and indent problem.
;;
;; 2014/01/01
;;      * Fixed keywords regexp
;;      * Fxied qml-indent-line
;;
;; 2013/12/31
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

(require 'css-mode)
(require 'js)

;;; Code:

(defcustom qml-mode-hook '()
  "Called upon entry into term mode.
This is run before the process is cranked up."
  :type 'hook
  :group 'qml-mode)

(defvar qml-indent-width 4)

(defconst qml-block-re "\\(^[ \t]*\\)\\([a-zA-Z0-9]*\\)[ \t]*[a-zA-Z0-9_]*[ \t]*.*{")

(defun qml-get-beg-of-block ()
  (save-excursion
    (when (re-search-backward qml-block-re nil t)
      (match-beginning 2)))
  )

(defun qml-get-end-of-block ()
  (save-excursion
    (when (re-search-backward qml-block-re nil t)
      (goto-char (match-end 0))
      (backward-char)
      (condition-case nil
          (save-restriction
            (forward-list)
            (point))
        (error nil))
      ))
  )

(defun qml-indent-line ()
  (let ((cur (point))
        (start (qml-get-beg-of-block))
        (end (qml-get-end-of-block))
        (cur-indent nil))
    (save-excursion
      (if (not (and start end (> cur start) (< cur end)))
          (progn
            (if start
                (goto-char start))
            (setq start (qml-get-beg-of-block))
            (setq end (qml-get-end-of-block))
            (while (and (not (eq start nil)) (not (eq end nil)) (not (and (> cur start) (< cur end))))
              (goto-char start)
              (setq start (qml-get-beg-of-block))
              (setq end (qml-get-end-of-block))
              )
            (if (or (eq start nil) (= (point) (point-min)))
                (progn
                  (goto-char (point-min))
                  (when (re-search-forward qml-block-re nil t)
                    (goto-char (match-beginning 2))
                    (setq start (point))
                    (goto-char (match-end 0))
                    (backward-char)
                    (condition-case nil
                        (save-restriction
                          (forward-list)
                          (setq end (point))
                          (setq cur-indent 0))
                      (error nil)))))))
      (if (not cur-indent)
          (progn
            (goto-char start)
            (setq cur-indent (current-indentation))
            (goto-char cur)
            (unless (string= (string (char-after (- (point) 1))) "{")
              (setq cur-indent (+ cur-indent tab-width))
              )
            )))
    (indent-line-to cur-indent)
    (if (string= (string (char-after (point))) "}")
        (indent-line-to (- cur-indent tab-width))
      )
    ))

(defvar qml-font-lock-keywords
  `(
    ;; Comment.
    ("/\\*.*\\*/\\|//.*"
     (0 font-lock-comment-face t t))
    ;; Constants.
    ("\\<\\(true\\|false\\)\\>"
     (0 font-lock-constant-face)
     )
    (":[ \t]?\\(-?[0-9\.]+\\)"
     (1 font-lock-constant-face)
     )
    ;; String.
    ("\"[^\"]*\""
     (0 font-lock-string-face))
    ;; Keyword.
    ("\\<\\(import\\|if\\|for\\|case\\|break\\|switch\\|else\\|[ \t]+if\\)\\>"
     (1 font-lock-keyword-face nil t))
    ;; Import
    ("\\(^import\\)[ \t]+\\([a-zA-Z\.]+\\)[ \t]+\\([0-9\.]+\\)"
     (1 font-lock-keyword-face nil t)
     (2 font-lock-function-name-face nil t)
     (3 font-lock-constant-face nil t)
     )
    ;; Element
    ("\\([A-Z][a-zA-Z0-9]*\\)[ \t]?{"
     (1 font-lock-function-name-face nil t))
    ;; Property keyword.
    ("\\(^[ \t]+property[ \t][a-zA-Z0-9_]+[ \t][a-zA-Z0-9_]+\\)"
     (0 font-lock-variable-name-face nil t))
    ;; Signal.
    ("\\(^[ \t]+signal[ \t][a-zA-Z0-9]+\\)"
     (0 font-lock-variable-name-face nil t))
    ;; Properties.
    ("\\([ \t]?[a-zA-Z0-9_\.]+\\):"
     (1 font-lock-variable-name-face nil t))
    ("\\<\\(anchors\\|margins\\)\\>"
     (1 font-lock-variable-name-face nil t))
    ;; Method
    ("\\<\\(function\\) +\\([a-z][a-zA-Z0-9]*\\)\\>"
     (1 font-lock-keyword-face nil t)
     (2 font-lock-function-name-face nil t))
    )
  "Keywords to highlight in `qml-mode'.")

(defvar qml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table))

;;;###autoload

(define-derived-mode qml-mode text-mode "QML"
  "Major mode for Qt declarative UI"
  (interactive)
  (set-syntax-table qml-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(qml-font-lock-keywords))
  (set (make-local-variable 'tab-width) qml-indent-width)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'qml-indent-line)
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) " */")
  (setq major-mode 'qml-mode)
  (setq mode-name "qml")

  (electric-indent-mode -1)

  (use-local-map qml-mode-map)
  (run-hooks 'qml-mode-hook)
  )

(defvar qml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-q") 'qml-indent-exp)
    map)
  "Keymap used by `qml-mode'.")

(defun qml-indent-exp ()
  (interactive)
  (save-excursion
    (indent-buffer))
  )

(provide 'qml-mode)

;;; qml-mode.el ends here
