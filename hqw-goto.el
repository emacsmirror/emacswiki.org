;;; hqw-goto.el -- A quick way to go to specific window line and position.
;; Time-stamp: <2012-01-31 16:11:16 hqwrong>

;; Copyright (C) 2011, 2012 王泓钦 <hq.wrong@gmail.com>

;; Author: 王泓钦(hqwrong)


     ;; This program is free software: you can redistribute it and/or modify
     ;; it under the terms of the GNU General Public License as published by
     ;; the Free Software Foundation, either version 3 of the License, or (at
     ;; your option) any later version.

     ;; This program is distributed in the hope that it will be useful, but
     ;; WITHOUT ANY WARRANTY; without even the implied warranty of
     ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     ;; General Public License for more details.

     ;; You should have received a copy of the GNU General Public License
     ;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;
;;; Commentary:
;; Put hqw-goto.el into your load path.And add following line to your .emacs
;;  (require 'hqw-goto)
;; Use `hqw-goto-line' to go to specific line,
;;   It will index each window line with a char on left fringe. Type a char to go to
;;   corresponding window line.
;; Use `hqw-goto-char' to go to specific char.
;;    It will prompt a char to search to. Repeating typing this char will move on until you type
;;    other keys.
;;    With C-u prefixed, it will highlight the search path.

;;; Code:
(defvar hqw-goto-line-keep-column
  t
  "when go to target line,whether to keep current column if possible")

(make-face 'hqw-goto-line-face)
(set-face-attribute 'hqw-goto-line-face nil
                    :foreground "#4271ae" :family "Sans Serif" :height 100 :foreground "deep sky blue"
                    :weight 'bold)

(defconst hqw-goto-line-candidates
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v 
       ?w ?x ?y ?z ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N
       ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z ?, ?. ?/ ?< ?> ?? ?[ ?] ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "The candidates shown on the left fringe.")

(defvar hqw-goto-line-table
  (make-char-table 'hqw-goto-line-table))

(let ((num 0))
   (dolist (char hqw-goto-line-candidates)
      (aset hqw-goto-line-table char num)
      (setq num (1+ num))))

(defun indicate-margin ()
   (save-excursion
      (let ((limit (window-end))
            (entry-point hqw-goto-line-candidates))    
         (move-to-window-line 0)
         (set-window-margins (selected-window) 2) ;this set immediately,not like set left-margin-width
         (while (and (not (eobp))
                     (<= (point) limit)
                     (not (null entry-point))) ;it can't replace `limit' by (window-end),for
                                        ;as point get near enough to bottom,it will scroll up.
            (let ((str (char-to-string (car entry-point)))
                  (ov (make-overlay (point) (point))))
               (overlay-put ov
                            'before-string
                            (propertize " "
                                        'display
                                        `((margin left-margin)
                                          ,(propertize str 'face 'hqw-goto-line-face))))
               (overlay-put ov
                            'name
                            'hqw-goto-ov)
               (vertical-motion 1) ;intead of (forward-line) ,for move a display line
               (setq entry-point (cdr entry-point)))))))

(defun hqw-goto-line (arg)
   "Goto specific window line. with prefix,you can choose which window to go. "
   (interactive "p")
   (defun hqw-go-to-line ()
      (indicate-margin)
      (call-interactively
       (function  
        (lambda (arg linum)
           (interactive "P\ncGo to Line:")
           (if (eq linum 7)             ;when you type C-g
                 (throw 'quit 'quit))
           (let ((column (current-column))
                 (target (aref hqw-goto-line-table linum)))
              (if (null target)
                    (message "Unknown Char")
                 (push-mark)
                 (move-to-window-line target)
                 (if hqw-goto-line-keep-column
                       (move-to-column column))))))))
   (defun after-goto ()
      (remove-overlays (window-start) (window-end) 'name 'hqw-goto-ov)
      (set-window-margins (selected-window) orin-margin))

   (let (
         (inhibit-quit t)    ; So (remove-overlays) would be
                                        ; executed,when type \C-g
         (count (1- arg))
         (orin-margin (car (window-margins))))
      (if (eq (catch 'quit
                 (progn (other-window count)
                        (hqw-go-to-line)))
              'quit)
            (progn (after-goto)
                   (other-window (- count)))
         (after-goto))))

(defun hqw-goto-char (arg char)
   "Move to specified char.
    if prefixed with C-u,then the mark will be actived.
    "
   (interactive "P\ncGoto Char: ")
  
   (defun iter-zap (count)
      ;; (if (< arg 0)
      ;;     (search-forward (char-to-string char) nil nil -1)
      ;;   (search-forward (char-to-string char) nil nil 1))
      (search-forward (char-to-string char) nil nil count)
      (setq c (read-event))
      (if (and (characterp c)
               (char-equal c char))
            (iter-zap (if (< arg 0) -1 1))
         (push c unread-command-events)))

   (push-mark)
   (when (consp arg)
      (setq mark-active t)
      (setq arg (if (< (car arg) 0)
                      -1
                   1)))
   (when (null arg)
      (setq arg 1))
   (when (eq arg '-)
      (setq arg -1))
   (setq inhibit-quit t) ;prevent \C-g quitting from minibuffer
                                        ;then,it will flow down the procedure
   (iter-zap arg))

(provide 'hqw-goto)

;;; hqw-goto.el ends here
