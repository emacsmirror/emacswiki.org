;;; wcy-swbuff.el --- switch buffer quickly

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: ChunYe Wang <CharlesWang@peoplemail.com.cn>
;; Keywords: buffer, convenience
;; Compatibility: Emacs 20.7, Emacs 21, XEmacs

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 
;; Place this file somewhere in your `load-path', and add:

;; (require 'wcy-swbuffer)
;; (global-set-key (kbd "<C-tab>") 'wcy-switch-buffer-forward)
;; (global-set-key (kbd "<C-S-kp-tab>") 'wcy-switch-buffer-backward)
;; then you can use <C-tab> and <C-S-kp-tab> to switch buffer.
;;
;; 
;;; Code:

(defvar wcy-buffer-exclude-regexps 
  '("^ .*" "^\\*.*" )
  "if buffer name match the regexp, ignore it.")

(defvar wcy-switch-buffer-key 
  (kbd "<C-tab>")
  "default key bind for switch buffer.")
(defvar wcy-switch-buffer-active-buffer-face
  'highlight
  "default face for active buffer"
  )
(defvar wcy-switch-buffer-inactive-buffer-face
  'secondary-selection
  "default face for inactive buffer"
  )


;;cycly the list
;;;###autoload
(defun wcy-cycly-list(l)
  (append (cdr l) (list (car l))))
;;cycly the list reverse
;;;###autoload
(defun wcy-cycly-list-reverse(l)
  (append (last l) (reverse (cdr (reverse l)))))

;;;###autoload
(defun wcy-buffer-list ()
  (if (null wcy-buffer-exclude-regexps) 
      (buffer-list)
    (let ((regexp (mapconcat 'identity  wcy-buffer-exclude-regexps "\\|"))
          (buffer-list nil))
      (dolist (buffer (buffer-list))
        (if (not (string-match regexp (buffer-name buffer)))
            (setq buffer-list (append buffer-list (list buffer) nil))))
      buffer-list)))


;;;###autoload
(defun wcy-display-buffer-list (buffer-list)
  "display a buffer list in the echo area."
  (let ((other-buffer-name (mapconcat  'buffer-name (cdr buffer-list)  "|"))
        message-log-max) ;; disable *Message* log
    (if (> (length other-buffer-name)  (window-width))
        (let* ((half-length (- (/ (window-width) 2) 3)))
          (setq other-buffer-name
                (concat (substring other-buffer-name 0 half-length)
                        " ... "
                        (substring other-buffer-name 
                                   (- (length other-buffer-name) half-length))))))
    (message "%s" 
             (concat (propertize (buffer-name (car buffer-list)) 
                                 'face wcy-switch-buffer-active-buffer-face)
                     "|" 
                     (propertize other-buffer-name
                                 'face wcy-switch-buffer-inactive-buffer-face))))
  (switch-to-buffer (car buffer-list) t))


;;;###autoload
(defun wcy-switch-buffer (arg)
  "switch buffer with <C-tab> like in windows. 
if ARG is negative, switch backword, otherwise forward."
  (interactive "p")
  (let* ((cycle-function (if (> arg 0) 'wcy-cycly-list ;; cycle forward or backward
                           'wcy-cycly-list-reverse))
         (tmp-buffer-list (funcall cycle-function (wcy-buffer-list))) ;; a list of buffer object
         (exitflag nil)
         (oldbuffer (current-buffer))
         key)
    (wcy-display-buffer-list tmp-buffer-list)
    (while (null exitflag)
      ;; read a key 
      (setq key (read-key-sequence-vector nil))
      (let* ((func (key-binding key)))
;;        (setq last-input-char (aref key (1- (length key))))
        (setq last-command-event (aref key (1- (length key))))
        (cond 
         ((equal func 'wcy-switch-buffer-forward)
          (setq tmp-buffer-list (wcy-cycly-list tmp-buffer-list))
          (wcy-display-buffer-list tmp-buffer-list))
         ((equal func 'wcy-switch-buffer-backward)
          (setq tmp-buffer-list (wcy-cycly-list-reverse tmp-buffer-list))
          (wcy-display-buffer-list tmp-buffer-list))
         ((equal func 'keyboard-quit)
          (setq tmp-buffer-list (cons oldbuffer tmp-buffer-list))
          (setq exitflag 1))
         (t (setq exitflag t)))))
    ;; switch to the selected buffer.
    (let ((selected-buffer (car tmp-buffer-list)))
      (if (buffer-name selected-buffer)
          (switch-to-buffer selected-buffer)))
    ;; execute the last key.
    (or (eq 1 exitflag) 
        (let ((func (key-binding key)))
          (if func (call-interactively func))))
    nil))

;;;###autoload
(defun wcy-switch-buffer-forward ()
  (interactive)
  (wcy-switch-buffer 1))
;;;###autoload
(defun wcy-switch-buffer-backward ()
  (interactive)
  (wcy-switch-buffer -1))

    
(provide 'wcy-swbuff)
;;; wcy-swbuff.el ends here
