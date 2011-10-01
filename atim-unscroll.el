;;; atim-unscroll.el --- Minor mode for restoring buffer look as it was before scrolling.

;; Copyright (C) 2011 Andrey Timokhin, all rights reserved.

;; Author: Andrey Timokhin <andrey dot timokhin dot code at gmail dot com>
;; Created: Sun Aug 14 14:53:48 PDT 2011
;; Keywords: convenience, text editing

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;; 
;; The position of cursor and buffer look are automatically remembered
;; when a scrolling command is invoked (pg-down/pg-up, mouse wheel
;; scrolling, scroll-bar movement)
;;
;; This information is stored in a ring; you can go up and down this
;; ring restoring you position and text visibility before the scrolling
;; command with
;;
;; atim-unscroll-down (bound to [M-down])
;; atim-unscroll-up   (bound to [M-up])
;;
;; these commands travel the ring to older (..-down) and
;; newer (..-up) stored points.
;;


;;; Installation:
;;
;; 1) Put atim-unscroll.el to your load-path.
;;
;; 2) Add the following to your ~/.emacs startup file.
;;
;;   (require 'atim-unscroll)
;;   (atim-unscroll-global-mode) ;; to enable in all buffers
;; 
;;
;; 3) Optional step(s):
;;
;; a) To enable atim-unscroll in just some types
;; of buffers, comment out the `atim-unscroll-global-mode' and put
;; atim-unscroll-mode in some major-mode hook, like:
;;
;; (add-hook 'c-mode-common-hook '(lambda () (atim-unscroll-mode)))
;;
;; b) Alternatively, do use `atim-unscroll-global-mode' and create
;; *exceptions* using the `atim-unscroll-dont-activate' local
;; variable, like:
;;
;;(add-hook 'c-mode-common-hook
;;          '(lambda () (setq atim-unscroll-dont-activate t)))
;;

;;; Change log:
;;
;; 2011/08/14
;;      First released.
;;

;;; Acknowledgements:
;;
;; This mode was inspired by examples in Chapter 3 of Bob Glickstein's
;; "Writing GNU Emacs Extensions" (O'Reilly Media, 1997)


;;; Require
;;
;; cl
;;


;;; Code:
;;

(require 'cl) ;; need this for advising scroll functions

(defgroup atim-unscroll nil
  "Go to the position of cursor befor scrolling command"
  :group 'editing
  :version "23")

(defcustom atim-unscroll-max-ring-length 10
       "Length of the unscroll ring."
       :type  'integer
       :group 'atim-unscroll)

;; list of basic scrolling commands which trigger remebering of cursor
;; position via `advise'
;;
(setq atim-unscroll--advise-commands-list
      '(scroll-up
        scroll-down
        scroll-left
        scroll-right
        beginning-of-buffer
        end-of-buffer
        scroll-bar-toolkit-scroll))

;; these commands invoke the basic scrolling commands; 
;; they will have `unscrollable' property
;;
(setq atim-unscroll--unscrollable-commands-list
      (append '(mwheel-scroll
                cua-scroll-up
                cua-scroll-down)
              atim-unscroll--advise-commands-list))

;; mode-map
;;
(defvar atim-unscroll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-up]   'atim-unscroll-up)
    (define-key map [M-down] 'atim-unscroll-down)
    map)
  "Keymap for atim-unscroll minor mode.")


(defvar atim-unscroll-point
  nil
  "Text positions for next call of `atim-unscroll--unscroll'.")
(defvar atim-unscroll-window-start
  nil
  "Window start for next call of `atim-unscroll--unscroll'.")
(defvar atim-unscroll-hscroll
  nil
  "Window start for next call of `atim-unscroll--unscroll'.")
(defvar atim-unscroll-ring-pos
  0
  "Current position in the unscroll ring.")


(defun atim-unscroll-up ()
  "Revert to `atim-unscroll-point' and `atim-unscroll-window-start'
by going up in the unscroll ring"
  (interactive)
  (if (or (eq last-command 'atim-unscroll-up)
          (eq last-command 'atim-unscroll-down))
      (if (< 0 atim-unscroll-ring-pos)
          (setq atim-unscroll-ring-pos (1- atim-unscroll-ring-pos))
        (error "No previous marker in the unscroll ring"))
    (setq atim-unscroll-ring-pos 0))
  (atim-unscroll--unscroll))

(defun atim-unscroll-down ()
  "Revert to `atim-unscroll-point' and `atim-unscroll-window-start'
by going down in the unscroll ring"
  (interactive)
  (if (or (eq last-command 'atim-unscroll-up)
          (eq last-command 'atim-unscroll-down))
      (if (> (ring-length atim-unscroll-point) (1+ atim-unscroll-ring-pos))
          (setq atim-unscroll-ring-pos (1+ atim-unscroll-ring-pos))
        (error "No further marker in the unscroll ring"))
    (setq atim-unscroll-ring-pos 0))
  (atim-unscroll--unscroll))


(defun atim-unscroll--unscroll ()
  "Revert to `atim-unscroll-point' and `atim-unscroll-window-start'."
  (if (ring-empty-p atim-unscroll-point)
      (error "Unscroll ring is empty")
    (progn
      (goto-char (ring-ref atim-unscroll-point atim-unscroll-ring-pos))
      (set-window-start   nil (ring-ref atim-unscroll-window-start
                                        atim-unscroll-ring-pos))
      (set-window-hscroll nil (ring-ref atim-unscroll-hscroll
                                        atim-unscroll-ring-pos)))))

(defun atim-unscroll--maybe-remember ()
  "Remeber positions before scroll command."
  (if (not (get last-command 'unscrollable))
      (progn
        (ring-insert atim-unscroll-point (point-marker))
        (save-excursion
          (progn
            (goto-char (window-start))
            (ring-insert atim-unscroll-window-start (point-marker))))
        (ring-insert atim-unscroll-hscroll (window-hscroll)))))


;; globalized mode
;;
(define-globalized-minor-mode atim-unscroll-global-mode
  atim-unscroll-mode
  atim-unscroll-on)

(defun atim-unscroll-on ()
  (unless atim-unscroll-dont-activate (atim-unscroll-mode t)))

(defvar atim-unscroll-dont-activate nil
  "If non-nil function `atim-unscroll-global-mode' does not activate in buffer.")
(make-variable-buffer-local 'atim-unscroll-dont-activate)


;; mode definition
;;
(define-minor-mode atim-unscroll-mode
  "atim-unscroll minor mode.
\nKey bindings:
\\{atim-unscroll-mode-map}"
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " uscrl"
  ;; group
  :group 'atim-unscroll
  ;; The minor mode bindings.
  :keymap atim-unscroll-mode-map
  (if atim-unscroll-mode
      ;; activate mode
      (progn
        (make-variable-buffer-local 'atim-unscroll-point)
        (setq  atim-unscroll-point (make-ring atim-unscroll-max-ring-length))
        (make-variable-buffer-local 'atim-unscroll-window-start)
        (setq  atim-unscroll-window-start (make-ring atim-unscroll-max-ring-length))
        (make-variable-buffer-local 'atim-unscroll-hscroll)
        (setq  atim-unscroll-hscroll (make-ring atim-unscroll-max-ring-length))
        (make-variable-buffer-local 'atim-unscroll-ring-pos)
        (mapcar (lambda (x)
                  (put x 'unscrollable t))
                atim-unscroll--unscrollable-commands-list)
        (loop for f in atim-unscroll--advise-commands-list
              do (eval `(defadvice ,f (before
                                       atim-remember-for-unscroll
                                       activate
                                       compile)
                          "Remember where we started from, for atim-unscroll"
                          (atim-unscroll--maybe-remember)))))
    ;; deactivate mode
    (progn
        (kill-local-variable 'atim-unscroll-point)
        (kill-local-variable 'atim-unscroll-window-start)
        (kill-local-variable 'atim-unscroll-hscroll)
        (kill-local-variable 'atim-unscroll-ring-pos)
        (mapcar (lambda (x)
                  (put x 'unscrollable nil))
                atim-unscroll--unscrollable-commands-list)
        (loop for f in atim-unscroll--advise-commands-list
              do (eval `(ad-disable-advice f 'before 'atim-remember-for-unscroll)))
        (loop for f in atim-scroll-commands_list
              do (eval `(ad-activate f))))))


(provide 'atim-unscroll)

;;; atim-unscroll.el ends here
