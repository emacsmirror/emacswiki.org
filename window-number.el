;;; window-number.el --- Select windows by numbers.

;; Filename: window-number.el
;; Description: Select windows by numbers.
;; Author: Johann "Myrkraverk" Oskarsson <myrkraverk@users.sourceforge.net>
;; Maintainer: Johann "Myrkraverk" Oskarsson <myrkraverk@users.sourceforge.net>
;;             Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2004, Johann "Myrkraverk" Oskarsson, all rights reserved.
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2004
;; Version: 0.1
;; Last-Updated: 2014-01-01 22:25:03
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/window-number.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; `cl'
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
;; Window number mode allows you to select windows by numbers.
;;
;; `window-number-switch' same as `other-window' (C-x o) when windows less than three.
;; If have three windows (or more) showing, `window-number-switch' will
;; highlight window number at mode-line then prompt you input window number.
;;
;; I binding `window-number-switch' on 'C-x o' to instead `other-window'.
;;

;;; Installation:
;;
;; Put window-number.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'window-number)
;; (window-number-mode 1)
;;
;; No need more.

;;; Customize:
;;
;; `window-number-active-foreground'
;; `window-number-active-background'
;; `window-number-inactive-foreground'
;; `window-number-inactive-background'
;;
;; All of the above can customize by:
;;      M-x customize-group RET window-number RET
;;

;;; Change log:
;;
;; 2014/01/01
;;      * Add new function `window-number-switch'
;;      * Add group `window-number' and customize colors.
;;      * Highlight window number on mode-line when `window-number-switch' prompt input.
;;      * Use `completing-read' instead `read-string' for better input experience.
;;
;; 2004
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

(require 'cl)                           ; for set-difference and loop

;;; Code:

(defgroup window-number nil
  "Window number group")

(defcustom window-number-active-foreground "black"
  "The foreground color when window number active."
  :type 'string
  :group 'window-number)

(defcustom window-number-active-background "gold"
  "The background color when window number active."
  :type 'string
  :group 'window-number)

(defcustom window-number-inactive-foreground "white"
  "The foreground color when window number inactive."
  :type 'string
  :group 'window-number)

(defcustom window-number-inactive-background "darkred"
  "The background color when window number inactive."
  :type 'string
  :group 'window-number)

(defface window-number-face nil
  "The face used for the window number in the mode-line.")

(defun window-number-list ()
  "Returns a list of the windows, in fixed order and the
minibuffer (even if not active) last."
  (let* ((walk-windows-start
          (car (set-difference
                (window-list (selected-frame) t)
                (window-list (selected-frame) 1))))
         (walk-windows-current walk-windows-start)
         list)
    (while (progn
             (setq walk-windows-current
                   (next-window walk-windows-current t))
             (setq list (cons walk-windows-current list))
             (not (eq walk-windows-current walk-windows-start))))
    (reverse (cons (car list) (cdr list)))))

(defun window-number-switch ()
  "Call `other-window' when just two windows.
Prompt user input window number if have more windows."
  (interactive)
  (if (< (length (window-list)) 3)
      (call-interactively 'other-window)
    (window-number-set-active-color)
    (unwind-protect
        (let* ((window-numbers (number-sequence 1 (length (window-list))))
               (window-buffer-names (mapcar (lambda (x) (buffer-name (window-buffer x))) (window-number-list)))
               (completing-list (mapcar (lambda (x) (list (concat (number-to-string x) " <" (elt window-buffer-names (- x 1)) ">") x)) window-numbers))
               (current-window-index (1+ (position (selected-window) (window-number-list))))
               (next-window-index (if (= current-window-index (length (window-list))) 1 (+ current-window-index 1)))
               (select-index-string (completing-read (format "Window number (%s): " next-window-index) completing-list))
               )
          (window-number-select
           (if (string= select-index-string "")
               next-window-index
             (string-to-int select-index-string))))
      ;; Reset to inactive color if interactive is intercept by Ctrl+g
      (window-number-set-inactive-color)
      ))
  ;; Always reset to inactive color at end.
  (window-number-set-inactive-color))

(defun window-number-select (number)
  "Selects the nth window."
  (interactive "P")
  (if (integerp number)
      (let ((window (nth (1- number) (window-number-list))))
        (if (and window (or (not (window-minibuffer-p window))
                            (minibuffer-window-active-p window)))
            (select-window window)
          (error "No such window.")))))

(defun window-number-set-inactive-color ()
  (set-face-foreground 'window-number-face "white")
  (set-face-background 'window-number-face "darkred")
  (force-mode-line-update))

(defun window-number-set-active-color ()
  (set-face-foreground 'window-number-face "black")
  (set-face-background 'window-number-face "gold")
  (force-mode-line-update))

(defun window-number ()
  "Returns the the number of the current window."
  (length
   (memq (selected-window)
         (nreverse (window-number-list)))))

(defun window-number-string ()
  "Returns the string containing the number of the current window"
  (propertize
   (concat " [" (number-to-string (window-number)) "] ")
   'face
   'window-number-face))

(defvar window-number-mode-map nil
  "Keymap for the window number mode.")

(defvar window-number-meta-mode-map nil
  "Keymap for the window number meta mode.")

(defmacro window-number-define-keys (mode-map prefix)
  `(progn
     ,@(loop for number from 1 to 10 collect
             `(define-key ,mode-map
                (kbd ,(concat prefix (number-to-string
                                      (if (>= number 10) 0 number))))
                (lambda nil (interactive)
                  (window-number-select ,number))))))

                                        ; define C-x C-j 1 to switch to win 1, etc (C-x C-j 0 = win 10)
(unless window-number-mode-map
  (setq window-number-mode-map (make-sparse-keymap))
                                        ; space after C-j is important
  (window-number-define-keys window-number-mode-map "C-x C-j "))

                                        ; define M-1 to switch to win 1, etc (M-0 = win 10)
(unless window-number-meta-mode-map
  (setq window-number-meta-mode-map (make-sparse-keymap))
  (window-number-define-keys window-number-meta-mode-map "M-"))

(if (featurep 'xemacs)
    (define-minor-mode window-number-mode
      "A global minor mode that enables selection of windows
according to numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
      :global t
      :init-value nil
      :lighter " -?-"
      (window-number-set-inactive-color))

  (define-minor-mode window-number-mode
    "A global minor mode that enables selection of windows
according to numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
    :global t
    :init-value nil
    :lighter (:eval (window-number-string))
    (window-number-set-inactive-color)
    ))

(define-minor-mode window-number-meta-mode
  "A global minor mode that enables use of the M- prefix to
select windows, use `window-number-mode' to display the window
numbers in the mode-line."
  :global t
  :init-value nil)

(provide 'window-number)

;;; window-number.el ends here
