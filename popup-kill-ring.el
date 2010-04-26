;;; popup-kill-ring.el --- interactively insert item from kill-ring

;; Copyright (C) 2010  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;; Keywords: popup, kill-ring, pos-tip

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

;; Manage your `kill-ring' (select and paste).

;;; Requirement:
;;
;; * popup.el   http://github.com/m2ym/auto-complete
;; * pos-tip.el http://www.emacswiki.org/emacs/PosTip

;;; Setting:
;;
;; 1. Download the `popup.el' and `pos-tip.el'.
;; 2. Put your `load-path' directory to the `popup.el' and `pos-tip.el'.
;; 3. Add following settings to your .emacs.
;;
;;   (require 'popup)
;;   (require 'pos-tip)
;;   (require 'popup-kill-ring)
;;
;;   (global-set-key "\M-y" 'popup-kill-ring) ; For example.

;;; ChangeLog:
;;
;; * 0.0.6
;;   `up' to `popup-kill-ring-popup-width'
;;   `down' to `popup-kill-ring-popup-width'
;;
;; * 0.0.5
;;   New variable `popup-kill-ring-kill-ring-show-func'
;;   New Variable `popup-kill-ring-keymap'
;;
;; * 0.0.4
;;   abolished the substring of menu item.
;;   set margin-right and width to `popup-menu*'
;;
;; * 0.0.3
;;   `pos-tip-show' argument `DY' to 0.
;;
;; * 0.0.2
;;   `with-no-warnings' for variable `menu'.
;;
;; * 0.0.1:
;;   Initial version.

;;; Code:

(require 'popup)
(require 'pos-tip)


;;; Variables:

(defconst popup-kill-ring-version "0.0.6"
  "Version of `popup-kill-ring'")


(defvar popup-kill-ring-popup-width 30
  "*Width of popup item.")

(defvar popup-kill-ring-popup-margin-right 5
  "*Width of `popup-menu*' margin-right.")

(defvar popup-kill-ring-timeout 1
  "*Time of displaying `pos-tip-show' help tooltip.")

(defvar popup-kill-ring-kill-ring-show-func 'popup-kill-ring-pos-tip-show
  "*Function of displaying the contents of `kill-ring'.
This function requires two arguments `str' and `pos'.
`str' is string of displaying. `pos' is point of displaying.
Default value is `popup-kill-ring-pos-tip-show'.")


(defvar popup-kill-ring-keymap (copy-keymap popup-menu-keymap))
;; key setting for `popup-menu*'.
(define-key popup-kill-ring-keymap "\C-n" 'popup-kill-ring-next)
(define-key popup-kill-ring-keymap "\C-p" 'popup-kill-ring-previous)
(define-key popup-kill-ring-keymap [down] 'popup-kill-ring-next)
(define-key popup-kill-ring-keymap [up] 'popup-kill-ring-previous)


;;; Functions:

(defun popup-kill-ring ()
  "Interactively insert selected item from `key-ring' by `popup.el'
and `pos-tip.el'"
  (interactive)
  (let* ((c 0)
         (kring  (mapcar #'(lambda (arg)
                             (with-temp-buffer
                               (erase-buffer)
                               (insert (replace-regexp-in-string
                                        "[ \t]+" " "
                                        (replace-regexp-in-string
                                         "\n" "" arg)))
                               (prog1 (format "%d: %s"
                                              c
                                              (buffer-substring-no-properties
                                               (point-min) (point-max)))
                                 (setq c (1+ c)))))
                         kill-ring))
         num item)
    (setq item (popup-menu* kring
                            :width popup-kill-ring-popup-width
                            :keymap popup-kill-ring-keymap
                            :margin-right popup-kill-ring-popup-margin-right
                            :scroll-bar t))
    (when item
      (when (string-match "^\\([0-9]*\\): " item)
        (setq num (string-to-number (match-string 1 item)))
        (insert (nth num kill-ring))))))

(defun popup-kill-ring-pos-tip-show (str pos)
  (when (eq window-system 'x)
    (pos-tip-show str nil pos nil 0 nil nil nil 0)))

(defun popup-kill-ring-next ()
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
    ;; Since the every time drawing is very heavy,
    ;; `pos-tip' help is displays when timeout occured.
    (with-timeout
        (popup-kill-ring-timeout
         (progn
           ;; display selected item of kill-ring by `pos-tip-show'
           (setq item (popup-x-to-string (nth num lst)))
           (when (string-match "^\\([0-9]*\\): " item)
             (setq num (string-to-number (match-string 1 item))))
           (when num
             (funcall popup-kill-ring-kill-ring-show-func
                      (format "%s" (nth num kill-ring))
                      (popup-child-point m offset)))))
      (pos-tip-hide)
      (popup-next m)
      ;; wait for timeout
      (sit-for (+ 0.5 popup-kill-ring-timeout)))))

(defun popup-kill-ring-previous ()
  (interactive)
  ;; Variable `menu' is contents of popup.
  ;; See: `popup-menu-event-loop'
  (let* ((m (with-no-warnings menu))
         (num (1- (popup-cursor m)))
         (lst (popup-list m))
         (len (length lst))
         (offset (popup-offset m))
         item)
    (when (>= num len)
      (setq num 0))
    ;; Since the every time drawing is very heavy,
    ;; `pos-tip' help is displays when timeout occured.
    (with-timeout
        (popup-kill-ring-timeout
         (progn
           ;; display selected item of kill-ring by `pos-tip-show'
           (setq item (popup-x-to-string (nth num lst)))
           (when (string-match "^\\([0-9]*\\): " item)
             (setq num (string-to-number (match-string 1 item))))
           (when num
             (funcall popup-kill-ring-kill-ring-show-func
                      (format "%s" (nth num kill-ring))
                      (popup-child-point m offset)))))
      (pos-tip-hide)
      (popup-previous m)
      ;; wait for timeout
      (sit-for (+ 0.5 popup-kill-ring-timeout)))))


(provide 'popup-kill-ring)

;;; popup-kill-ring.el ends here
