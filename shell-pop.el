;;; shell-pop.el --- Helps you pop up and pop out shell buffer easily.
;;; $Id: shell-pop.el,v 1.10 2010/02/21 14:19:26 kyagi Exp kyagi $

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; Author:        Kazuo YAGI <kyagi@1liner.jp>
;; Maintainer:    Kazuo YAGI <kyagi@1liner.jp>
;; Created:       2009-05-31
;; Last-Updated:  $Date: 2010/02/21 14:19:26 $
;; Revision:      $Revision: 1.10 $
;; Keywords:      shell, terminal, tools
;; Compatibility: GNU Emacs 23.x

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary;
;;
;; This is a utility which helps you pop up and pop out shell buffer
;; window easily. Just do M-x shell-pop, and it is strongly recommmended
;; to assign one hot-key to this function.
;;
;; I hope this is useful for you, and ENJOY YOUR HAPPY HACKING!
;;

;;; Configuration;
;;
;; You can choose your favorite internal mode such as `shell', `terminal',
;; `ansi-term', and `eshell'. Also you can use any shell such as
;; `/bin/bash', `/bin/tcsh', `/bin/zsh' as you like.
;;
;; A configuration sample for your .emacs is as follows.
;;
;; (require 'shell-pop)
;; (shell-pop-set-internal-mode "ansi-term")
;; (shell-pop-set-internal-mode-shell "/bin/zsh")
;; (global-set-key [f8] 'shell-pop)
;;
;; Besides, you can set the window height, the number for the percentage
;; for selected window.
;;
;; (shell-pop-set-window-height 60)
;;

;;; Update Info;
;;
;; $Log: shell-pop.el,v $
;; Revision 1.10  2010/02/21 14:19:26  kyagi
;; bug fix
;;
;; Revision 1.9  2010/02/21 14:03:43  kyagi
;; bug fix
;;
;; Revision 1.8  2010/02/21 12:55:28  kyagi
;; bug fix
;;
;; Revision 1.7  2010/02/21 11:29:56  kyagi
;; add a function shell-pop-set-window-position
;;
;; Revision 1.6  2010/02/21 11:25:01  kyagi
;; add a option shell-pop-window-position
;;

;;; Code:
(defvar shell-pop-last-buffer nil)
(defvar shell-pop-last-window nil)
(defvar shell-pop-window-height 30) ; percentage for shell-buffer window height
(defvar shell-pop-window-position "bottom")

(defvar shell-pop-internal-mode "shell")
(defvar shell-pop-internal-mode-buffer "*shell*")
(defvar shell-pop-internal-mode-func '(lambda () (shell)))
(defvar shell-pop-internal-mode-shell "/bin/bash")

(defvar shell-pop-internal-mode-list
  (list
    ; mode, buffer, function
    '("shell"     "*shell*"     '(lambda () (shell)))
    '("terminal"  "*terminal*"  '(lambda () (term shell-pop-internal-mode-shell)))
    '("ansi-term" "*ansi-term*" '(lambda () (ansi-term shell-pop-internal-mode-shell)))
    '("eshell"    "*eshell*"    '(lambda () (eshell)))))

(defun shell-pop-set-window-height (number)
  (interactive "nInput the number for the percentage of \
selected window height (10-100): ")
  (setq shell-pop-window-height number))

(defun shell-pop-set-window-position (position)
  (interactive "sInput the position for shell-pop (top|bottom): ")
  (setq shell-pop-window-position position))

(defun shell-pop-set-internal-mode (mode)
  (interactive "sInput your favorite mode (shell|terminal|ansi-term|eshell): ")
  (if (catch 'found
        (dolist (l shell-pop-internal-mode-list)
          (if (string-match mode (car l))
              (progn
                (setq shell-pop-internal-mode-buffer (nth 1 l))
                (setq shell-pop-internal-mode-func (nth 2 l))
                (throw 'found t)))))
      t
    nil))

(defun shell-pop-set-internal-mode-shell (shell)
  (interactive (list (read-from-minibuffer "Input your favorite shell:"
                                           shell-pop-internal-mode-shell)))
  (setq shell-pop-internal-mode-shell shell))

(defun shell-pop ()
  (interactive)
  (if (equal (buffer-name) shell-pop-internal-mode-buffer)
      (shell-pop-out)
    (shell-pop-up)))

(defun shell-pop-up ()
  (let ((w (get-buffer-window shell-pop-internal-mode-buffer)))
    (if w
        (select-window w)
      (progn
        ; save shell-pop-last-buffer and shell-pop-last-window to return
          (setq shell-pop-last-buffer (buffer-name))
          (setq shell-pop-last-window (selected-window))
          (if (not (eq shell-pop-window-height 100))
              (progn
                (split-window (selected-window)
                              (if (string= shell-pop-window-position "bottom")
                                  (round (* (window-height)
                                            (/ (- 100 shell-pop-window-height) 100.0)))
                                (round (* (window-height) (/ shell-pop-window-height 100.0)))))
                (if (string= shell-pop-window-position "bottom")
                    (other-window 1))))
          (if (not (get-buffer shell-pop-internal-mode-buffer))
              (funcall (eval shell-pop-internal-mode-func))
            (switch-to-buffer shell-pop-internal-mode-buffer))))))

(defun shell-pop-out ()
  (if (not (eq shell-pop-window-height 100))
      (progn
        (delete-window)
        (if (string= shell-pop-window-position "bottom")
            (select-window shell-pop-last-window))))
  (switch-to-buffer shell-pop-last-buffer))

(provide 'shell-pop)

;;; shell-pop.el ends here.
