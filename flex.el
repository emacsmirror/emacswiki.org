;;; flex.el --- Flex mode

;; Filename: flex.el
;; Description: Flex mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-10-04 08:41:04
;; Version: 0.1
;; Last-Updated: 2018-10-04 08:41:04
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/flex.el
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
;; Flex mode
;;

;;; Installation:
;;
;; Put flex.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'flex)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET flex RET
;;

;;; Change log:
;;
;; 2018/10/04
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

(defgroup flex nil
  "Editing flex files."
  :group 'flex)

(defface flex-font-lock-declare-delimiter
  '((t (:foreground "gray35")))
  "Color for declare delimiter."
  :group 'flex)

(defface flex-font-lock-pattern-delimiter
  '((t (:foreground "gray35")))
  "Color for pattern delimiter."
  :group 'flex)

(defface flex-font-lock-pattern-content
  '((t (:foreground "gold")))
  "Color for pattern content"
  :group 'flex)

(defcustom color-rg-mode-hook '()
  "flex mode hook."
  :type 'hook
  :group 'flex)

(define-derived-mode flex-mode c-mode "Flex"
  "Major mode for editing flex files"
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
  (use-local-map flex-mode-map)
  (define-key flex-mode-map [tab] 'flex-indent-command)

  ;; Set comment strings.
  (setq comment-start "/*"
        comment-end "*/")

  ;; Highlight keywords.
  (flex-highlight-keywords)

  ;; Run hooks.
  (run-hooks 'color-rg-mode-hook)
  )

(defun flex-highlight-keywords ()
  "Highlight keywords."
  ;; Add keywords for highlight.
  (font-lock-add-keywords
   nil
   '(
     ("\\(%%\n\\)\\(\\(.+\n\\)+\\)\\(%%\\)" 1 'flex-font-lock-pattern-delimiter)
     ("\\(%%\n\\)\\(\\(.+\n\\)+\\)\\(%%\\)" 2 'flex-font-lock-pattern-content)
     ("\\(%%\n\\)\\(\\(.+\n\\)+\\)\\(%%\\)" 4 'flex-font-lock-pattern-delimiter)
     ("\\(%{\\)\\([^%{}]+\\)\\(%}\\)" 1 'flex-font-lock-declare-delimiter)
     ("\\(%{\\)\\([^%{}]+\\)\\(%}\\)" 3 'flex-font-lock-declare-delimiter)
     ))
  (set (make-local-variable 'font-lock-keywords-only) t)
  (font-lock-mode 1))

(defun flex-indent-command (&optional arg)
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

(provide 'flex)

;;; flex.el ends here
