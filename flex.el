;;; flex.el --- Flex mode

;; Filename: flex.el
;; Description: Flex mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-10-04 08:41:04
;; Version: 0.5
;; Last-Updated: 2018-10-15 21:45:03
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
;; 2018/10/15
;;	* Highlight comment block.
;; 
;; 2018/10/14
;;      * Use overlay instead regexp to match pattern content.
;;      * Adjust overlay regexp to match any pattern line.
;;      * Support | pattern.
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

(defface flex-font-lock-declare-delimiter-face
  '((t (:foreground "gray35")))
  "Color for declare delimiter."
  :group 'flex)

(defface flex-font-lock-pattern-delimiter-face
  '((t (:foreground "gray35")))
  "Color for pattern delimiter."
  :group 'flex)

(defface flex-font-lock-pattern-content-face
  '((t (:foreground "gold3")))
  "Color for pattern content"
  :group 'flex)

(defcustom flex-mode-hook '()
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

  ;; Highlight pattern overlays.
  (set (make-local-variable 'flex-pattern-overlays) nil)
  (flex-highlight-pattern-overlays)

  ;; Run hooks.
  (add-hook 'after-change-functions 'flex-update-pattern-overlays)
  (run-hooks 'flex-mode-hook)
  )

(defun flex-update-pattern-overlays (beg end leng-before)
  (when (derived-mode-p 'flex-mode)
    (let (current-point pattern-start pattern-end)
      (setq current-point (point))
      (save-excursion
        ;; Search pattern bound.
        (goto-char (point-min))
        (when (search-forward-regexp "^%%" nil t)
          (setq pattern-start (point)))
        (when (search-forward-regexp "^%%" nil t)
          (beginning-of-line)
          (setq pattern-end (point)))

        ;; Update pattern overlays when current edit point between pattern bound.
        (when (and pattern-start
                   pattern-end
                   (>= current-point pattern-start)
                   (<= current-point pattern-end))
          (flex-highlight-pattern-overlays)
          )))))

(defun flex-highlight-pattern-overlays ()
  ;; Clean pattern overlays.
  (when (and (boundp 'flex-pattern-overlays)
             flex-pattern-overlays)
    (dolist (overlay flex-pattern-overlays)
      (delete-overlay overlay))
    (set (make-local-variable 'flex-pattern-overlays) nil))

  ;; Search and update pattern overlay color.
  (let (pattern-start pattern-end)
    (save-excursion
      ;; Search pattern bound.
      (goto-char (point-min))
      (when (search-forward-regexp "^%%" nil t)
        (setq pattern-start (point)))
      (when (search-forward-regexp "^%%" nil t)
        (beginning-of-line)
        (setq pattern-end (point)))

      ;; Highlight patterns.
      (when (and pattern-start pattern-end)
        (goto-char pattern-start)
        (while (search-forward-regexp "^[^ \n]+[^{\n]+[{|]?" pattern-end t)
          (let (start end overlay)
            ;; Set end bound of pattern.
            (setq end
                  (if (or (string-suffix-p "{" (match-string 0))
                          (string-suffix-p "|" (match-string 0)))
                      ;; Backward one char if pattern end with char '{' or '|'
                      (- (point) 1)
                    ;; Otherwise record current point.
                    (point)))
            ;; Set start bound of pattern.
            (beginning-of-line)
            (setq start (point))
            ;; Add pattern overlay.
            (setq overlay (make-overlay start end))
            (overlay-put overlay 'face 'flex-font-lock-pattern-content-face)
            (add-to-list 'flex-pattern-overlays overlay)
            ;; Rest point to end of line to continue search.
            (end-of-line)
            ))))))

(defun flex-highlight-keywords ()
  "Highlight keywords."
  ;; Add keywords for highlight.
  (font-lock-add-keywords
   nil
   '(
     ("\\(^\\(%{\\|%}\\)\\)" 1 'flex-font-lock-declare-delimiter-face)
     ("\\(^%%\\)" 1 'flex-font-lock-pattern-delimiter-face)
     ("\\(%option\\|%x\\)" 1 'font-lock-keyword-face)
     ("/\\*\\s-.*\\*/" 0 'font-lock-comment-face)
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
