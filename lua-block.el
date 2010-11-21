;;; lua-block.el --- highlight matching block

;; Based on ruby-block.el by khiker

;; Copyright (C) 2010 by meegee
;; Copyright (C) 2007-2009  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;; Maintaner: meegee <themgzzy gmail com>

;; Keywords: languages, faces, lua


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
;; This is basically a refactored version of ruby-block.el by khiker.
;; Some stuff like repeat ... until statements are missing but it
;; works well enough.

;;; Usage:

;; Add this line to your .emacs
;;
;; (require 'lua-block)
;; (lua-block-mode t)
;;
;; In addition, you can also add one of these too:

;; ;; do overlay
;; (setq lua-block-highlight-toggle 'overlay)
;; ;; display to minibuffer
;; (setq lua-block-highlight-toggle 'minibuffer)
;; ;; display to minibuffer and do overlay
;; (setq lua-block-highlight-toggle t)
;;
;; Default is minibuffer and overlay.
;;
;; Tested on Emacs 22.3.2, Emacs 23.0.95.2 and Emacs 24.0.50.1

;;; Note:

;; lua-mode.el is necessary to use this package.

;;; Code:

(require 'lua-mode)

;; Variables:

(defconst lua-block-version "0.0.1"
  "Lua block package version.")

(defconst lua-block-keyword-list
  (list "end" "if" 
        "function" "do")
  "Keyword for highlighting.")

(defconst lua-block-keyword-regex
  "\\(end\\|if\\|function\\|do\\)"
  "Regular expression to look for correspondence.")

(defgroup lua-block nil
  "Lua block"
  :tag "Lua block"
  :group 'lua-block)

(defcustom lua-block-delay 0.50
  "*Time in seconds to delay before showing a matching paren."
  :type  'number
  :group 'lua-block)

(defcustom lua-block-highlight-face 'highlight
  "*Face for block highlighting."
  :type  'face
  :group 'lua-block)

(defcustom lua-block-highlight-toggle 't
  "*How do you display corresponding line.
Default is minibuffer and overlay.

The possible choice is as follows.

nil        => nothing
minibuffer => minibuffer
overlay    => overlay
t          => minibuffer and overlay"
  :type  '(choice (const :tag "nothing" nil)
                  (const :tag "minibuffer" minibuffer)
                  (const :tag "overlay" overlay)
                  (const :tag "minibuffer and overlay" t))
  :group 'lua-block)

(defvar lua-block-timer nil)

(defvar lua-block-highlight-overlay nil)


;; Functions:

(define-minor-mode lua-block-mode
  "In lua-mode, Displays the line where there is keyword corresponding
to END keyword. this is Minor mode for lua-mode only."
  :init-value t
  :global nil
  :keymap nil
  :lighter " LBlock"
  (if lua-block-mode
      (lua-block-start-timer)
    (lua-block-stop-timer)))

(defun lua-block-start-timer ()
  "start timer."
  (when lua-block-timer
    (cancel-timer lua-block-timer))
  (setq lua-block-timer
        (run-with-idle-timer lua-block-delay t 'lua-block-hook)))

(defun lua-block-stop-timer ()
  "stop timer."
  (when lua-block-timer
    (cancel-timer lua-block-timer)
    (setq lua-block-timer nil)))

(defun lua-block-hook ()
  "When Major-mode is lua-mode, this package is running."
  (if (eq major-mode 'lua-mode)
      (condition-case err
          (lua-block-function)
        (error
         (setq lua-block-mode nil)
         (message "Error: %S; lua-block-mode now disabled." err)))
    (setq lua-block-mode nil)))

(defun lua-block-get-line-start-pos ()
  (save-excursion
    (let ((xor '(lambda (a b) (and (or a b) (not (and a b)))))
          (point (point))
          (count 0))
      (while (and (not (funcall xor (bobp) (eolp)))
                  (> point (point-min)))
        (setq point (1- point))
        (goto-char (1- (point))))
      ;; delete linefeed of start point.
      (when (and (eolp) (>= (point-max) (1+ point)))
        (setq point (1+ point)))
      point)))

(defun lua-block-get-line-end-pos ()
  (save-excursion
    (let ((xor '(lambda (a b) (and (or a b) (not (and a b)))))
          (point (point)))
      (while (and (not (funcall xor (eobp) (eolp)))
                  (>= (point-max) point))
        (setq point (1+ point))
        (goto-char (1+ (point))))
      point)))

(defun lua-block-function ()
  "Point position's word decides behavior."
  (let ((current (current-word)))
    (setq current (car (member current lua-block-keyword-list)))
    (cond
     ;; not keyword
     ((null current)
      nil)
     ;; keyword "end"
     ((and (string= "end" current)
           (eq 'font-lock-keyword-face (get-text-property (point) 'face)))
      (let ((point (lua-block-get-corresponding-point))
            (slinep 0)(elinep 0))
        ;; get whole line(exists point). and, display minibuffer.
        (when (> point 0)
          (save-excursion
            (goto-char point)
            (setq slinep (lua-block-get-line-start-pos)
                  elinep (lua-block-get-line-end-pos)))
          ;; display line contents to minibuffer
          (when (or (eq lua-block-highlight-toggle t)
                    (eq lua-block-highlight-toggle 'minibuffer))
            (message "%d: %s" (1+ (count-lines (point-min) slinep))
                     (buffer-substring slinep elinep)))
          ;; do overlay.
          (when (or (eq lua-block-highlight-toggle t)
                    (eq lua-block-highlight-toggle 'overlay))
            (lua-block-do-highlight slinep elinep)))))
     ;; keyword except "end"
     (t
      nil))))

(defun lua-block-get-corresponding-point ()
  "Get point of corresponding line."
  (let ((orig-col (- (point) (lua-block-get-line-start-pos)))
        (recent-col (- (point) (lua-block-get-line-start-pos)))
        (count 1)(check t)(point 0)(face "")(string ""))
    (save-excursion
      (while check
        (if (re-search-backward lua-block-keyword-regex (point-min) t 1)
            (setq point  (match-beginning 1)
                  face   (get-text-property point 'face)
                  string (current-word))
          (setq point -1 face "" string "" check nil))
        (when (and (eq face 'font-lock-keyword-face)
                   (not (string= string "elseif"))
                   (member string lua-block-keyword-list)
                   ;; case: STMT if(or unless, while, untill) EXPR
                   (if (member string '("if" "while" "until"))
                       (let ((col (- point (lua-block-get-line-start-pos))))
                         (if (or (> (+ orig-col 3) col)
                                 (> (+ recent-col 3) col))
                             t nil))
                     t))
          (if (and (string= string "end") check)
              (setq count (1+ count)
                    recent-col (- (point) (lua-block-get-line-start-pos)))
            (setq count (1- count))))
        (when (= count 0)
          (setq check nil)))
      point)))

(defun lua-block-do-highlight (beg end)
  "Do overlay corresponding line."
  (if lua-block-highlight-overlay
      (move-overlay  lua-block-highlight-overlay beg end)
    (setq lua-block-highlight-overlay (make-overlay beg end)))
  (overlay-put lua-block-highlight-overlay
               'face lua-block-highlight-face)
  (add-hook 'pre-command-hook 'lua-block-highlight-done))

(defun lua-block-highlight-done ()
  "After do overlay, restore the line to original color."
  (remove-hook 'pre-command-hook 'lua-block-highlight-done)
  (if lua-block-highlight-overlay
      (delete-overlay lua-block-highlight-overlay)))

(defun lua-block-highlight-toggle ()
  "Switch on/off for lua-block-mode."
  (interactive)
  (if lua-block-highlight-toggle
      (setq lua-block-highlight-toggle nil)
    (setq lua-block-highlight-toggle t)))

(provide 'lua-block)

;;; lua-block.el ends here
