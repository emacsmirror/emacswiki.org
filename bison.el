;;; bison.el --- Editing bison source code.

;; Filename: bison.el
;; Description: Editing bison source code.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-10-06 11:59:10
;; Version: 0.2
;; Last-Updated: 2018-10-15 21:45:29
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/bison.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
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
;; Editing bison source code.
;;

;;; Installation:
;;
;; Put bison.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'bison)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET bison RET
;;

;;; Change log:
;;
;; 2018/10/15
;;	* Highlight comment block.
;; 
;; 2018/10/06
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
(require 'derived)


;;; Code:
(defgroup bison nil
  "Editing bison files."
  :group 'bison)

(defface bison-font-lock-declare-delimiter-face
  '((t (:foreground "gray35")))
  "Color for declare delimiter."
  :group 'bison)

(defface bison-font-lock-pattern-delimiter-face
  '((t (:foreground "gray35")))
  "Color for pattern delimiter."
  :group 'bison)

(defface bison-font-lock-rule-face
  '((t (:foreground "Purple")))
  "Color for rule."
  :group 'bison)

(defface bison-font-lock-rule-content-face
  '((t (:foreground "gold3")))
  "Color for rule content"
  :group 'flex)

(defcustom bison-mode-hook '()
  "bison mode hook."
  :type 'hook
  :group 'bison)

(define-derived-mode bison-mode c-mode "Bison"
  "Major mode for editing bison files"
  ;; Try to set the indentation correctly.
  (setq-default c-basic-offset 4)
  (make-variable-buffer-local 'c-basic-offset)
  (c-set-offset 'knr-argdecl-intro 0)
  (make-variable-buffer-local 'c-offsets-alist)

  ;; Remove auto and hungry anything.
  (c-toggle-auto-hungry-state -1)
  (c-toggle-auto-state -1)
  (c-toggle-hungry-state -1)

  ;; Load keymap.
  (use-local-map bison-mode-map)
  (define-key bison-mode-map [tab] 'bison-indent-command)

  ;; Set comment strings.
  (setq comment-start "/*"
        comment-end "*/")

  ;; Highlight keywords.
  (bison-highlight-keywords)

  ;; Run hooks.
  (run-hooks 'bison-mode-hook)
  )

(defun bison-highlight-keywords ()
  "Highlight keywords."
  ;; Add keywords for highlight.
  (font-lock-add-keywords
   nil
   '(
     ("\\(^\\(%{\\|%}\\)\\)" 1 'bison-font-lock-declare-delimiter-face)
     ("\\(^%%\\)" 1 'bison-font-lock-pattern-delimiter-face)
     ("^\\(.*\\):.*\n|" 1 'bison-font-lock-rule-face)
     ("\\(%token\\|%union\\|%type\\|%left\\|%right\\|%nonassoc\\)" 1 'font-lock-keyword-face)
     ("^|\\(\\s-.*\\){" 1 'bison-font-lock-rule-content-face)
     ("^.*:\\s-\\(.*\\){" 1 'bison-font-lock-rule-content-face)
     ("/\\*\\s-.*\\*/" 0 'font-lock-comment-face)
     ))
  (set (make-local-variable 'font-lock-keywords-only) t)
  (font-lock-mode 1))

(defun bison-indent-command (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (c-indent-command)
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^\\s-*\\(%}\\|%{\\|%%\\)\\s-*")
          (let (start end)
            (setq start (point))
            (end-of-line)
            (setq end (point))
            (kill-region start
                         (save-excursion
                           (beginning-of-line)
                           (if (search-forward-regexp "\\s-*" end t)
                               (point)
                             start)
                           )))
        (c-indent-command)))))

(provide 'bison)

;;; bison.el ends here
