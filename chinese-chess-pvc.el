;;; chinese-chess-pvc.el --- a chinese chess computer player -*- coding: utf-8 -*-

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: chinese-chess-pvc.el,v 1.1 2007/02/15 08:10:35 ywb Exp ywb $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is NOT the computer player, it's just a front end of a ucci program.
;; You can get a ucci program from http://www.elephantbase.net/, then set
;; chinese-chess-pvc-program to the program.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'chinese-chess-pvc)

;;; Code:

(provide 'chinese-chess-pvc)
(eval-when-compile
  (require 'cl))
(require 'chinese-chess)

(defvar chinese-chess-pvc-program "eleeye.exe")
(defvar chinese-chess-pvc-engine nil)
(defvar chinese-chess-pvc-buffer "*chinese-chess-pvc*")
(defvar chinese-chess-old-filter nil)
(defvar chinese-chess-computer-type 'black)
(defvar chinese-chess-computer-time 3)
(defvar chinese-chess-pvc-no-move nil)

(defvar chinese-chess-pvc-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map chinese-chess-mode-map)
    (define-key map "B" 'chinese-chess-pvc-backward)
    (define-key map "T" 'chinese-chess-pvc-change-turn)
    (define-key map "Q" 'chinese-chess-pvc-quit)
    map))

(defun chinese-chess-pvc-change-turn ()
  (interactive)
  (setq chinese-chess-computer-type
        (chinese-chess-other-type
         chinese-chess-computer-type))
  (chinese-chess-pvc-move))

(defun chinese-chess-pvc-start-engine ()
  (make-comint-in-buffer "chinese-chess"
                         chinese-chess-pvc-buffer
                         chinese-chess-pvc-program)
  (setq chinese-chess-pvc-engine
        (get-buffer-process chinese-chess-pvc-buffer)
        chinese-chess-old-filter
        (process-filter chinese-chess-pvc-engine))
  (set-process-filter chinese-chess-pvc-engine
                      'chinese-chess-pvc-filter)
  (chinese-chess-pvc-send "ucci\n"))

(defun chinese-chess-pvc-send (str)
  (comint-send-string chinese-chess-pvc-engine str))

(defun chinese-chess-pvc-filter (proc string)
  (and (functionp chinese-chess-old-filter)
       (funcall chinese-chess-old-filter proc string))
  (dolist (line (split-string string "\n"))
    (cond ((string-match "^bestmove \\(\\sw+\\)" line)
           (let ((buf (chinese-chess-buffer)))
             (if (and buf (not chinese-chess-pvc-no-move))
                 (with-current-buffer buf
                   (chinese-chess-move (match-string 1 line))))))
          ((string-match "^nobestmove" line)
           ;; (chinese-chess-message "你太强了，我认输，请重新开始"))))))
           ))))

(defun chinese-chess-pvc-buttons ()
  (chinese-chess-put-buttons
   (cons "重新开始" 'chinese-chess-begin)
   (cons "停止思考" 'chinese-chess-pvc-stop)
   (cons "悔棋" 'chinese-chess-pvc-backward)
   (cons "交换" 'chinese-chess-pvc-change-turn)))

(defun chinese-chess-pvc-mode ()
  (interactive)
  (use-local-map chinese-chess-pvc-mode-map)
  (unless (and (processp chinese-chess-pvc-engine)
               (eq (process-status chinese-chess-pvc-engine) 'run))
    (chinese-chess-pvc-start-engine))
  (chinese-chess-pvc-buttons)
  (add-hook 'chinese-chess-begin-hook 'chinese-chess-pvc-mode)
  (add-hook 'chinese-chess-change-turn-hook 'chinese-chess-pvc-move)
  (chinese-chess-pvc-move))

(defun chinese-chess-pvc-quit ()
  (interactive)
  (use-local-map chinese-chess-mode-map)
  (remove-hook 'chinese-chess-change-turn-hook 'chinese-chess-pvc-move)
  (remove-hook 'chinese-chess-begin-hook 'chinese-chess-pvc-mode)
  (chinese-chess-put-buttons
   (cons "重新开始" 'chinese-chess-begin)
   (cons "悔棋" 'chinese-chess-backward))
  (with-current-buffer chinese-chess-pvc-buffer
    (chinese-chess-pvc-send "quit\n")
    (kill-buffer (current-buffer))))

(defun chinese-chess-pvc-move ()
  (let ((buf (chinese-chess-buffer)))
    (when buf
      (with-current-buffer buf
        (when (eq chinese-chess-next-type chinese-chess-computer-type)
          (setq chinese-chess-pvc-no-move nil)
          (chinese-chess-pvc-send
           (format "position fen %s\ngo time %d\n"
                   (chinese-chess-board-fen) 
                   chinese-chess-computer-time)))))))

(defun chinese-chess-pvc-backward ()
  (interactive)
  (let ((chinese-chess-change-turn-hook
         (remq 'chinese-chess-pvc-move chinese-chess-change-turn-hook)))
    (chinese-chess-pvc-stop)
    (setq chinese-chess-pvc-no-move t)
    (chinese-chess-backward)))

(defun chinese-chess-pvc-stop ()
  (interactive)
  (chinese-chess-pvc-send "stop\n"))

(defun chinese-chess-pvc-banmoves (str)
  (interactive "sMove: ")
  (chinese-chess-pvc-send (concat "banmoves " str "\n")))

;;; chinese-chess-pvc.el ends here
