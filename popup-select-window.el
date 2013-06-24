;;; popup-select-window.el --- selecting a window by popup-menu*

;; Copyright (C) 2010-2012  HAMANO Kiyoto

;; Author: HAMANO Kiyoto <khiker.mail@gmail.com>
;; Keywords: popup, select-window, window

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extend a `select-window'.

;;; Requirement:
;;
;; * popup.el http://github.com/m2ym/popup-el

;;; Setting:
;;
;; 1. Download the `popup.el' and this file.
;; 2. Put your `load-path' directory to the `popup.el' and this file.
;; 3. Add following settings to your dot file (for example .emacs ...).
;;
;;   (require 'popup)
;;   (require 'popup-select-window)
;;   (global-set-key "\C-xo" 'popup-select-window)

;;; Extend Sample
;;
;; * Change highlight face
;;
;;   (setq popup-select-window-window-highlight-face
;;        '(:background "navy"))
;;   (setq popup-select-window-window-highlight-face
;;        '(:foreground "white" :background "navy"))
;;
;; * Disable modeline highlight
;;
;;   (setq popup-select-window-use-modeline-highlight nil)
;;
;; * Disable buffer highlight
;;
;;   (setq popup-select-window-use-buffer-highlight nil)
;;
;; * Change active modeline color to blue
;;
;;   (setq popup-select-window-active-modeline-bgcolor "blue")
;;
;; * Change nonactive modeline color to gray
;;
;;   (setq popup-select-window-inactive-modeline-bgcolor "gray")
;;
;; * Change minimum number that enable popup (specifiy window
;;   number. defaults 3).
;;
;;  (setq popup-select-window-popup-windows 1)

;;; Tested:
;;
;; * Emacs
;;   * 24.0.90
;; * popup.el
;;   * 0.4

;;; ChangeLog:
;;
;; * 0.1.1 (2013/06/24)
;;   Fixed the problem which `popup-select-window' does not works by the
;;   Emacs 24.3 later. And Changed the internal function/variable name.
;;   (This bug was reported by @lurdan. Thank you.)
;;
;; * 0.1.0 (2012/02/11)
;;   Added key configuration to select window by 1 key.
;;
;; * 0.0.5 (2012/02/11)
;;   Added a customizable variable
;;   `popup-select-window-popup-windows'. This variable specifies
;;   minumum number that enable popup.
;;
;; * 0.0.4 (2010/09/25)
;;   Modified a sample "Change highlight face".
;;
;; * 0.0.3 (2010/09/25)
;;   In `popup-select-window-buffer-highlight',
;;   Added a window id to overlay property `window'.
;;
;; * 0.0.2 (2010/09/25)
;;   When `C-g' was typed, and
;;   `popup-select-window-use-buffer-highlight' is t, do
;;   `select-window'.
;;
;; * 0.0.1 (2010/09/24)
;;   Initial version.

;;; Code:

(require 'popup)


;;; Variables:

(defconst popup-select-window-version "0.1.1"
  "Version of `popup-select-window'.")

(defvar popup-select-window-window-highlight-face 'highlight
  "*The face for selected window.")

(defvar popup-select-window-use-modeline-highlight t
  "*Non-nil means calling `popup-select-modeline-highlight' function.")

(defvar popup-select-window-use-buffer-highlight t
  "*Non-nil means calling `popup-select-buffer-highlight' function.")

(defvar popup-select-window-highlight-func 'popup-select-window-highlight
  "*The function for buffer (window) highlighting function.")

(defvar popup-select-window-active-modeline-bgcolor "red"
  "*Modeline color for the selected window.")

(defvar popup-select-window-inactive-modeline-bgcolor "gray30"
  "*Modeline color for not selected window.")

(defvar popup-select-window-popup-margin-left  2
  "*Value of `popup-menu*' margin-left.")

(defvar popup-select-window-popup-margin-right 3
  "*Value of `popup-menu*' margin-right.")

(defvar popup-select-window-popup-width 30
  "*Value of `popup-menu*' width.")

(defvar popup-select-window-popup-windows 3
  "*A number of windows that enable popup-select-window")

(defvar popup-select-window-popup-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap popup-menu-keymap)
    (define-key keymap "\C-n" 'popup-select-window-next)
    (define-key keymap "\C-p" 'popup-select-window-previous)
    (define-key keymap [down] 'popup-select-window-next)
    (define-key keymap [up]   'popup-select-window-previous)
    keymap)
    "*A keymap for `popup-menu*' of `popup-select-window'.")


(defvar popup-select-window--window-list-cache nil
  "Internal variable for `popup-select-window'.")

(defvar popup-select-window--window-overlay nil
  "Internal variable for `popup-select-window'.")


;;; Functions:

(defun popup-select-window ()
  "Interactive move window by `popup-menu*'."
  (interactive)
  (let ((wins
         (let ((c 0))
           (mapcar #'(lambda (x)
                       (prog1 (list c
                                    (buffer-name (window-buffer x))
                                    x)
                         (setq c (1+ c))))
                   (window-list))))
        (cwin (selected-window))
        (active-modeline (popup-select-window--bg-access-modeline))
        (inactive-modeline (popup-select-window--bg-access-modeline-inactive))
        (mhighlight popup-select-window-use-modeline-highlight)
        (bhighlight popup-select-window-use-buffer-highlight)
        (min-wins popup-select-window-popup-windows)
        (keymap (make-sparse-keymap))
        select buf)
    ;; If value of `popup-select-window-popup-windows' is invlaid (not
    ;; number or less than 1), set to display minimum division number of
    ;; the window (division number 1).
    (cond
     ((or (not (numberp min-wins))
          (>= 2 min-wins))
      (setq min-wins 1))
     (t
      (setq min-wins (1- min-wins))))
    ;; Set up keymap for selection.
    (cond
     ((> 26 (length wins))
      (setq keymap
            (progn
              (set-keymap-parent keymap popup-select-window-popup-keymap)
              (dotimes (i (length wins))
                (define-key keymap (format "%c" (+ ?a i))
                  'popup-select-window-select))
              keymap)))
     (t
      (setq keymap popup-select-window-popup-keymap)))
    ;; Now selects any window.
    (cond
     ((= (length wins) min-wins)
      (call-interactively 'other-window))
     ((> (length wins) min-wins)
      (unwind-protect
          (progn
            (when mhighlight
              (popup-select-window--bg-access-modeline
               popup-select-window-active-modeline-bgcolor)
              (popup-select-window--bg-access-modeline-inactive
               popup-select-window-inactive-modeline-bgcolor))
            (setq popup-select-window--window-list-cache wins
                  select
                  (popup-menu*
                   (mapcar #'(lambda (x)
                               (let ((num  (nth 0 x))
                                     (name (nth 1 x)))
                                 (propertize (format "(%c) %s" (+ ?a num) name)
                                             'index num)))
                           wins)
                   :width        popup-select-window-popup-width
                   :margin-left  popup-select-window-popup-margin-left
                   :margin-right popup-select-window-popup-margin-right
                   :keymap       keymap)))
        ;; delete overlay
        (when bhighlight
          (popup-select-window--delete-overlay))
        ;; recover modeline color
        (when mhighlight
          (popup-select-window--bg-access-modeline active-modeline)
          (popup-select-window--bg-access-modeline-inactive inactive-modeline)
          (when (and (numberp last-input-event) (= last-input-event ? ))
            (select-window cwin)))
        (setq popup-select-window--window-list-cache nil)
        (when select
          (select-window
           (popup-select-window--get-window
            (popup-select-window--get-index select) wins))))))))

(defun popup-select-window-select ()
  (interactive)
  ;; Variable `menu' is contents of popup.
  ;; See: `popup-menu-event-loop'
  (let* ((m (with-no-warnings menu))
         (lst (popup-list m))
         (ch (cond
              ((vectorp last-input-event)
               (car last-input-event))
              (t
               last-input-event)))
         num item)
    (when (and (>= ch ?a) (>= ?z ch))
      (setq num (- ch ?a))
      (popup-select-window--delete-overlay)
      (when popup-select-window-highlight-func
        (funcall popup-select-window-highlight-func
                 (popup-select-window--get-window num)))
      (when (setq item (popup-item-value-or-self (nth num lst)))
        (return item)))))

(defun popup-select-window-next ()
  (interactive)
  ;; Variable `menu' is contents of popup.
  ;; See: `popup-menu-event-loop'
  (let* ((m (with-no-warnings menu))
         (num (1+ (popup-cursor m)))
         (lst (popup-list m))
         (len (length lst))
         (offset (popup-offset m))
         item)
    (when (>= num len)
      (setq num 0))
    (setq item (popup-x-to-string (nth num lst))
          num (popup-select-window--get-index item))
    (popup-select-window--delete-overlay)
    (popup-next m)
    (when popup-select-window-highlight-func
      (funcall popup-select-window-highlight-func
               (popup-select-window--get-window num)))))

(defun popup-select-window-previous ()
  (interactive)
  ;; Variable `menu' is contents of popup.
  ;; See: `popup-menu-event-loop'
  (let* ((m (with-no-warnings menu))
         (num (1- (popup-cursor m)))
         (lst (popup-list m))
         (len (length lst))
         (offset (popup-offset m))
         item)
    (when (< num 0)
      (setq num (1- len)))
    (setq item (popup-x-to-string (nth num lst))
          num (popup-select-window--get-index item))
    (popup-select-window--delete-overlay)
    (popup-previous m)
    (when popup-select-window-highlight-func
      (funcall popup-select-window-highlight-func
               (popup-select-window--get-window num)))))

(defun popup-select-window-highlight (win)
  (when popup-select-window-use-modeline-highlight
    (popup-select-window--modeline-highlight win))
  (when popup-select-window-use-buffer-highlight
    (popup-select-window--buffer-highlight win)))

(defun popup-select-window--bg-access (target &optional face)
  (cond
   (face
    (set-face-background target face))
   (t
    (face-background target))))

(defun popup-select-window--bg-access-modeline (&optional face)
  (popup-select-window--bg-access 'mode-line face))

(defun popup-select-window--bg-access-modeline-inactive (&optional face)
  (popup-select-window--bg-access 'mode-line-inactive face))

(defun popup-select-window--get-index (item)
  (with-temp-buffer
    (erase-buffer)
    (insert item)
    (get-text-property (point-min) 'index)))

(defun popup-select-window--modeline-highlight (win)
  (select-window win))

(defun popup-select-window--buffer-highlight (win)
  (when (windowp win)
    (with-selected-window win
      (setq popup-select-window--window-overlay
            (make-overlay (window-start) (window-end)))
      (overlay-put popup-select-window--window-overlay 'window win)
      (overlay-put popup-select-window--window-overlay
                   'face popup-select-window-window-highlight-face))))

(defun popup-select-window--delete-overlay ()
  (when popup-select-window--window-overlay
    (delete-overlay popup-select-window--window-overlay)))

(defun popup-select-window--get-window (num &optional win)
  (nth 2 (assq num (if win win popup-select-window--window-list-cache))))


(provide 'popup-select-window)

;;; popup-select-window.el ends here
