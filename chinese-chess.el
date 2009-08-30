;;; chinese-chess.el --- Chinese Chess Game   -*- coding: utf-8 -*-
;; Author: wenbinye@163.com
;; Version: $Id: chinese-chess.el,v 1.1 2007/02/15 08:10:32 ywb Exp ywb $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it unr the terms of the GNU General Public License as published by
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

;; 车--马--象--士--将--士--象--马--车
;;  |   |   |   | \ | / |   |   |   |
;;  +---+---+---+---+---+---+---+---+
;;  |   |   |   | / | \ |   |   |   |
;;  +--炮---+---+---+---+---+---炮--+
;;  |   |   |   |   |   |   |   |   |
;; 兵---+--兵---+--兵---+--兵---+--兵
;;  |   |   |   |   |   |   |   |   |
;;  +---+---+---+---+---+---+---+---+
;;  |                               |
;;  +---+---+---+---+---+---+---+---+
;;  |   |   |   |   |   |   |   |   |
;; 卒---+--卒---+--卒---+--卒---+--卒
;;  |   |   |   |   |   |   |   |   |
;;  +--炮---+---+---+---+---+---炮--+
;;  |   |   |   | \ | / |   |   |   |
;;  +---+---+---+---+---+---+---+---+
;;  |   |   |   | / | \ |   |   |   |
;; 車--馬--相--仕--帅--仕--相--馬--車

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (autoload 'chinese-chess "chinese-chess" "Play chinese chess game." t)

;;; Code:

(provide 'chinese-chess)
(eval-when-compile
  (require 'cl))
;; just for button-face
(require 'cus-edit)

(defvar chinese-chess-red-face '(:foreground "red"))
(defvar chinese-chess-black-face 'default)
(defvar chinese-chess-selected-face '(:background "cyan"))
(defvar chinese-chess-last-move-face '(:background "pink1"))

(defconst chinese-chess-width 9)
(defconst chinese-chess-height 10)
(defconst chinese-chess-col-offset 8)
(defconst chinese-chess-row-offset 5
  "First line lists buttons, Second line display state, the third
display chessmen")

(defvar chinese-chess-display-coords t)
(defvar chinese-chess-board nil)

(defvar chinese-chess-display
  `((r . ,(propertize "车" 'face chinese-chess-black-face))
    (n . ,(propertize "马" 'face chinese-chess-black-face))
    (b . ,(propertize "象" 'face chinese-chess-black-face))
    (a . ,(propertize "士" 'face chinese-chess-black-face))
    (k . ,(propertize "将" 'face chinese-chess-black-face))
    (p . ,(propertize "兵" 'face chinese-chess-black-face))
    (c . ,(propertize "炮" 'face chinese-chess-black-face))
    (R . ,(propertize "車" 'face chinese-chess-red-face))
    (N . ,(propertize "馬" 'face chinese-chess-red-face))
    (B . ,(propertize "相" 'face chinese-chess-red-face))
    (A . ,(propertize "仕" 'face chinese-chess-red-face))
    (K . ,(propertize "帅" 'face chinese-chess-red-face))
    (P . ,(propertize "卒" 'face chinese-chess-red-face))
    (C . ,(propertize "炮" 'face chinese-chess-red-face))))

(defvar chinese-chess-type-name
  '((red . "红方")
    (black . "黑方")))

(defvar chinese-chess-next-type 'red)
(defvar chinese-chess-selected-man nil)
(defvar chinese-chess-history nil)
(defvar chinese-chess-init-round nil)

(defvar chinese-chess-message-updated nil)
(defvar chinese-chess-message-interval 3)

(defvar chinese-chess-change-turn-hook nil)
(defvar chinese-chess-begin-hook nil)

(defvar chinese-chess-mode-map nil)
(defvar chinese-chess-mode-menu nil)
(unless chinese-chess-mode-map
  (setq chinese-chess-mode-map (make-sparse-keymap))
  (let ((map chinese-chess-mode-map))
    (define-key map "R" 'chinese-chess-begin)
    (define-key map "B" 'chinese-chess-backward)
    (define-key map "S" 'chinese-chess-save)
    (define-key map "T" 'chinese-chess-change-turn)
    (define-key map "L" 'chinese-chess-load)
    (define-key map "D" 'chinese-chess-puts-mode)
    (define-key map "F" 'chinese-chess-board-fen)
    (define-key map "m" 'chinese-chess-move)
    (define-key map "C" 'chinese-chess-toggle-coords)
    (define-key map (kbd "<down-mouse-1>") 'chinese-chess-select)
    (define-key map (kbd "<down-mouse-3>") 'chinese-chess-mouse-move)
    (define-key map (kbd "<mouse-1>") 'undefined)
    (define-key map (kbd "<mouse-2>") 'undefined)
    (define-key map (kbd "<mouse-3>") 'undefined)
    (define-key map (kbd "<mouse-4>") 'undefined)
    (define-key map (kbd "<mouse-5>") 'undefined)
    (define-key map (kbd "<drag-mouse-1>") 'undefined)
    (define-key map (kbd "<M-drag-mouse-1>") 'undefined)
    (define-key map (kbd "<M-down-mouse-1>") 'undefined)
    (define-key map (kbd "<M-mouse-1>") 'undefined)
    (define-key map (kbd "<M-mouse-2>") 'undefined)
    (define-key map (kbd "<M-mouse-3>") 'undefined)
    (define-key map (kbd "<double-mouse-1>") 'undefined))
  (easy-menu-define chinese-chess-mode-menu
    chinese-chess-mode-map
    "Menu for chinese chess"
    (cons "Chess"
          '(["Reset" chinese-chess-begin "R"]
            ["Back" chinese-chess-backward "B"]
            ["Save" chinese-chess-save "S"]
            ["Load" chinese-chess-load "L"]
            ["Turn" chinese-chess-type-turn "T"]
            ["Put" chinese-chess-puts-mode "D"]
            ["Coords" chinese-chess-toggle-coords "C"]))))

(defvar chinese-chess-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'push-button)
    (define-key map [mouse-1] 'push-button)
    (define-key map [?\t] 'forward-button)
    (define-key map "\e\t" 'backward-button)
    (define-key map [backtab] 'backward-button)
    map))

(defvar chinese-chess-buffer "*chess*" "buffer name of game")

(defsubst chinese-chess-other-type (type)
  (if (eq type 'black)
      'red 'black))

(defun chinese-chess-add-history (hist-var elem)
  (set hist-var (cons elem (symbol-value hist-var))))

(defun chinese-chess-round ()
  (+ chinese-chess-init-round
     (/ (length chinese-chess-history) 2)))

(defun chinese-chess-buffer ()
  (let ((buf (get-buffer chinese-chess-buffer)))
    (and (buffer-live-p buf)
         buf)))

;;{{{  function about chess board

;;{{{  These function will be changed if the board implement is change
(defsubst chinese-chess-row (row type)
  "For convention of row"
  (if (eq type 'red)
      (1+ (- chinese-chess-height row))
    row))

(defsubst chinese-chess-line2row (line)
  (+ (/ (- line chinese-chess-row-offset) 2) 1))

(defsubst chinese-chess-column2col (columnn)
  (+ (/ (- columnn chinese-chess-col-offset) 4) 1))

(defun chinese-chess-goto (row col)
  (goto-char (point-min))
  (forward-line (+ chinese-chess-row-offset (* 2 (1- row))))
  (move-to-column (+ chinese-chess-col-offset (* 4 (1- col)))))

(defun chinese-chess-draw-board ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (i chinese-chess-row-offset)
      (insert "\n"))
    (mapc (lambda (line)
            (insert-char ?  chinese-chess-col-offset)
            (insert " " line "\n"))
          (split-string
           "+---+---+---+---+---+---+---+---+
|   |   |   | \\ | / |   |   |   | 
+---+---+---+---+---+---+---+---+
|   |   |   | / | \\ |   |   |   | 
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   |   | 
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|                               |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   |   | 
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   |   | 
+---+---+---+---+---+---+---+---+
|   |   |   | \\ | / |   |   |   | 
+---+---+---+---+---+---+---+---+
|   |   |   | / | \\ |   |   |   | 
+---+---+---+---+---+---+---+---+" "\n"))
    (add-text-properties (point-min) (point)
                         '(read-only t rear-sticky t front-sticky t))
    ;; 可能将来会实现为着法增加注释的功能，所以保留一个区域可写
    (insert (propertize "\n" 'rear-nonsticky t))
    (chinese-chess-put-buttons
     (cons "重新开始" 'chinese-chess-begin)
     (cons "悔棋" 'chinese-chess-backward))
    (if chinese-chess-display-coords
        (chinese-chess-toggle-coords 1))))

(defun chinese-chess-toggle-coords (arg)
  (interactive "P")
  (let ((inhibit-read-only t))
    (setq chinese-chess-display-coords
          (if (null arg)
              (not chinese-chess-display-coords)
            (> (prefix-numeric-value arg) 0)))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- chinese-chess-row-offset))
      (if chinese-chess-display-coords
          (progn
            (delete-region (point) (progn (forward-line 1) (point)))
            (insert-char ?  chinese-chess-col-offset)
            (insert " a   b   c   d   e   f   g   h   i\n")
            (dotimes (i chinese-chess-height)
              (move-to-column (- chinese-chess-col-offset 2))
              (delete-char 1)
              (insert (number-to-string (- 9 i)))
              (forward-line 2)))
        (delete-region (point) (progn (forward-line 1) (1- (point))))
        (dotimes (i chinese-chess-height)
          (move-to-column (- chinese-chess-col-offset 2))
          (delete-char 1)
          (insert " ")
          (forward-line 2))))))

(defun chinese-chess-remove-man-at (row col)
  (chinese-chess-board-set row col nil)
  (let ((inhibit-read-only t))
    (save-excursion
      (chinese-chess-goto row col)
      (delete-char 1)
      (insert (if (= col 1) " +" "-+")))))

(defun chinese-chess-put-man (man &optional row col)
  (let ((inhibit-read-only t))
    (if row
        (chinese-chess-man-set man 'row row)
      (setq row (chinese-chess-man-get man 'row)))
    (if col
        (chinese-chess-man-set man 'col col)
      (setq col (chinese-chess-man-get man 'col)))
    (chinese-chess-goto row col)
    (if (chinese-chess-board-get row col)
        ;; (not (chinese-chess-ascii-p (char-after)))
        (chinese-chess-remove-man-at row col))
    (delete-char 2)
    (chinese-chess-board-set row col man)
    (insert (propertize (chinese-chess-man-get man 'display)
                        'chessman man))))

;;}}}

(defsubst chinese-chess-valid-row (row)
  (and (> row 0) (<= row chinese-chess-height)))
(defsubst chinese-chess-valid-col (col)
  (and (> col 0) (<= col chinese-chess-width)))

(defsubst chinese-chess-board-get (row col)
  (aref (aref chinese-chess-board (1- row)) (1- col)))
(defsubst chinese-chess-board-set (row col man)
  (aset (aref chinese-chess-board (1- row)) (1- col) man))

(defun chinese-chess-coords-at (pos)
  (save-excursion
    (goto-char pos)
    (setq row (chinese-chess-line2row (line-number-at-pos))
          col (chinese-chess-column2col (current-column)))
    (when (and (chinese-chess-valid-row row)
               (chinese-chess-valid-col col))
      (list row col))))

(defsubst chinese-chess-man-at (&optional pos)
  (get-text-property (or pos (point)) 'chessman))

(defsubst chinese-chess-remove-man (man)
  (chinese-chess-remove-man-at (chinese-chess-man-get man 'row)
                               (chinese-chess-man-get man 'col)))

(defun chinese-chess-reset-board ()
  (chinese-chess-draw-board)
  (setq chinese-chess-next-type
        (if (eq (default-value 'chinese-chess-next-type) 'black)
            'black 'red)
        chinese-chess-selected-man nil
        chinese-chess-history nil
        chinese-chess-init-round 1
        chinese-chess-board
        (apply 'vector
               (mapcar (lambda (i) (make-vector chinese-chess-width nil))
                       (number-sequence 1 chinese-chess-height)))))

(defun chinese-chess-read-fen (fen)
  (let ((data (split-string fen))
        (all (append "RNBAKPCrnbakpc" nil))
        (i 1)
        j men)
    (dolist (row (split-string (car data) "/"))
      (setq j 1)
      (dolist (c (append row nil))
        (if (and (>= c ?1) (<= c ?9))
            (setq j (+ j (- c ?0)))
          (if (member c all)
              (progn
                (push (list (intern (char-to-string c)) i j) men)
                (setq j (1+ j)))
            (error "Unknown chessman %c" c))))
      (setq i (1+ i)))
    (list men
          ;; next type
          (if (string= (cadr data) "w") 'red 'black)
          ;; round number
          (string-to-number (nth 5 data)))))

(defun chinese-chess-set-board (fen)
  (interactive "sFen: ")
  (chinese-chess-reset-board)
  (let ((board (chinese-chess-read-fen fen)))
    (mapc (lambda (args)
            (chinese-chess-put-man (apply 'chinese-chess-make-man args)))
          (car board))
    (setq chinese-chess-next-type (cadr board)
          chinese-chess-init-round (nth 2 board))
    (chinese-chess-message (chinese-chess-type-message))))
;;}}}

;;{{{  function abount chessman
(defun chinese-chess-make-man (name &optional row col)
  (if row
      (unless (chinese-chess-valid-row row)
        (error "Row(%d) out of board, should between 1-%d" row chinese-chess-height)))
  (if col
      (unless (chinese-chess-valid-col col)
        (error "Column(%d) out of board, should between 1-%d" col chinese-chess-width)))
  (list (cons 'type (if (member name '(r n b a k c p)) 'black 'red))
        (cons 'name name)
        (cons 'row row)
        (cons 'col col)
        (cons 'rule (intern-soft (concat "chinese-chess-rule-"
                                         (downcase (symbol-name name)))))
        (cons 'display (assoc-default name chinese-chess-display))))

(defalias 'chinese-chess-copy-man 'copy-alist)

(defsubst chinese-chess-man-get (man attr)
  (cdr (assoc attr man)))

(defsubst chinese-chess-man-set (man attr val)
  (setcdr (assoc attr man) val))
;;}}}

;;{{{  Rules
(defun chinese-chess-pos-type (row)
      (if (< row 6)
          'black 'red))

(defun chinese-chess-row-men (row from to)
  (if (< from to)
        (delq nil
              (mapcar
               (lambda (i)
                 (chinese-chess-board-get row i))
               (number-sequence (1+ from) (- to 1))))
    (chinese-chess-row-men row to from)))

(defun chinese-chess-col-men (col from to)
  (if (< from to)
      (delq nil
            (mapcar (lambda (i)
                      (chinese-chess-board-get i col))
                    (number-sequence (1+ from) (- to 1))))
    (chinese-chess-col-men col to from)))

(defun chinese-chess-rule-check (man row col)
  (let ((nman (chinese-chess-board-get row col)))
    (when nman
      (if (eq (chinese-chess-man-get nman 'type)
              (chinese-chess-man-get man 'type))
          (throw 'cannot-move 'same-type))))
  (let* ((king (car(chinese-chess-lookup-board 'k)))
         (men (chinese-chess-col-men (chinese-chess-man-get king 'col)
                                     (chinese-chess-man-get king 'row)
                                     (1+ chinese-chess-height))))
    (if (and (> (length men) 1)
             (eq (car men) man)
             (eq (chinese-chess-man-get (cadr men) 'name) 'K))
        (throw 'cannot-move 'king-meet))))

(defun chinese-chess-rule-r (man nrow ncol)
  (let ((row (chinese-chess-man-get man 'row))
        (col (chinese-chess-man-get man 'col)))
    (chinese-chess-rule-check man nrow ncol)
    (or (and (= row nrow)
             (= (length (chinese-chess-row-men row col ncol)) 0))
        (and (= col ncol)
             (= (length (chinese-chess-col-men col row nrow)) 0)))))

(defun chinese-chess-rule-a (man nrow ncol)
  (let ((row (chinese-chess-man-get man 'row))
        (col (chinese-chess-man-get man 'col)))
    (chinese-chess-rule-check man nrow ncol)
    (and (if (< (chinese-chess-man-get man 'row) 4)
             (< nrow 4)
           (> nrow (chinese-chess-row 4 'red)))
         (< ncol 7) (> ncol 3)
         (= (abs (- nrow row)) 1)
         (= (abs (- ncol col)) 1))))

(defun chinese-chess-rule-k (man nrow ncol)
  (let ((row (chinese-chess-man-get man 'row))
        (col (chinese-chess-man-get man 'col)))
    (chinese-chess-rule-check man nrow ncol)
    (and (if (< (chinese-chess-man-get man 'row) 4)
             (< nrow 4)
           (> nrow (chinese-chess-row 4 'red)))
         ;; not meet the king of other type
         (let (men)
           (if (eq (chinese-chess-pos-type nrow) 'black)
               (setq men (chinese-chess-col-men
                          ncol (max nrow row) (1+ chinese-chess-height)))
             (setq men (nreverse
                        (chinese-chess-col-men
                         ncol 0 (min nrow row)))))
           (if (not (member
                     (chinese-chess-man-get (car men) 'name)
                     '(k K)))
               t
             (throw 'cannot-move 'king-meet)))
         (< ncol 7) (> ncol 3)
         (or (and (= row nrow)
                  (= (abs (- ncol col)) 1))
             (and (= col ncol)
                  (= (abs (- nrow row)) 1))))))

(defun chinese-chess-rule-p (man nrow ncol)
  (let ((row (chinese-chess-man-get man 'row))
        (col (chinese-chess-man-get man 'col)))
    (chinese-chess-rule-check man nrow ncol)
    (if (eq (chinese-chess-pos-type
             (chinese-chess-man-get man 'row))
            (chinese-chess-man-get man 'type))
        (and (= col ncol)
             (= (if (eq (chinese-chess-man-get man 'type) 'black)
                    (- nrow (chinese-chess-man-get man 'row))
                  (- (chinese-chess-man-get man 'row) nrow)) 1))
      (let ((crow (if (eq (chinese-chess-man-get man 'type) 'black)
                      (- nrow (chinese-chess-man-get man 'row))
                    (- (chinese-chess-man-get man 'row) nrow))))
        (or (and (= crow 1) (= col ncol))
            (and (= crow 0) (= (abs (- col ncol)) 1)))))))

(defun chinese-chess-rule-b (man nrow ncol)
  (let ((row (chinese-chess-man-get man 'row))
        (col (chinese-chess-man-get man 'col)))
    (chinese-chess-rule-check man nrow ncol)
    (and (eq (chinese-chess-pos-type
              (chinese-chess-man-get man 'row))
             (chinese-chess-man-get man 'type))
         (= (abs (- row nrow)) 2)
         (= (abs (- col ncol)) 2)
         (null (chinese-chess-board-get (/ (+ row nrow) 2)
                                    (/ (+ col ncol) 2))))))

(defun chinese-chess-rule-n (man nrow ncol)
  (let ((row (chinese-chess-man-get man 'row))
        (col (chinese-chess-man-get man 'col)))
    (chinese-chess-rule-check man nrow ncol)
    (if (= (abs (- row nrow)) 2)
        (and (= (abs (- col ncol)) 1)
             (null (chinese-chess-board-get (/ (+ row nrow) 2) col)))
      (and (= (abs (- col ncol)) 2)
           (= (abs (- row nrow)) 1)
           (null (chinese-chess-board-get row (/ (+ col ncol) 2)))))))

(defun chinese-chess-rule-c (man nrow ncol)
  (let ((row (chinese-chess-man-get man 'row))
        (col (chinese-chess-man-get man 'col))
        step)
    (chinese-chess-rule-check man nrow ncol)
    (and (if (= row nrow)
             (setq step (length (chinese-chess-row-men row col ncol)))
           (if (= col ncol)
               (setq step (length (chinese-chess-col-men col row nrow)))))
         (or (and (= step 1)
                  (chinese-chess-board-get nrow ncol))
             (and (= step 0)
                  (null (chinese-chess-board-get nrow ncol)))))))

(defun chinese-chess-king-dangerous (type)
  (let ((other (chinese-chess-other-type type))
        (king (car (chinese-chess-lookup-board (if (eq type 'black) 'k 'K))))
        row col
        man)
    (setq row (chinese-chess-man-get king 'row)
          col (chinese-chess-man-get king 'col))
    (catch 'danger
      (dolist (r (number-sequence 1 chinese-chess-height))
        (dolist (c (number-sequence 1 chinese-chess-width))
          (setq man (chinese-chess-board-get r c))
          (catch 'cannot-move
            (if (and (eq (chinese-chess-man-get man 'type) other)
                     (member (chinese-chess-man-get man 'name)
                             '(r n c p R N C P))
                     (funcall (chinese-chess-man-get man 'rule)
                              man row col))
                (throw 'danger t))))))))

(defun chinese-chess-lookup-board (name &optional all rows cols)
  (let ((num (if all 2 1))
        (have 0)
        found man)
    (or rows (setq rows (number-sequence 1 chinese-chess-height)))
    (or cols (setq cols (number-sequence 1 chinese-chess-width)))
    (catch 'found
      (dolist (r rows)
        (dolist (c cols)
          (setq man (chinese-chess-board-get r c))
          (when (eq (chinese-chess-man-get man 'name) name)
            (setq have (1+ have))
            (push man found)
            (if (= have num)
                (throw 'found man))))))
    found))

;;}}}

(defun chinese-chess-put-buttons (&rest buttons)
  "BUTTON is a cons cell like (LABEL . ACTION)"
  (let ((inhibit-read-only t)
        action)
    (save-excursion
      (goto-char (point-min))
      (delete-region (point) (line-end-position))
      (dolist (but buttons)
        (insert-button (car but)
                       'keymap chinese-chess-button-map
                       'face 'custom-button
                       'action `(lambda (&rest ignore)
                                  (funcall ',(cdr but)))
                       'mouse-action
                       `(lambda (&rest ignore)
                          (with-current-buffer (get-buffer chinese-chess-buffer)
                            (funcall ',(cdr but)))))
        (insert "   "))
      (delete-char -3))))

;;{{{  functions about message
(defun chinese-chess-message (msg)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (point) (line-end-position))
      (insert msg)
      (setq chinese-chess-message-updated t))))

(defun chinese-chess-message-tempo (msg later-msg)
  (chinese-chess-message (propertize msg 'face 'font-lock-warning-face))
  (run-at-time chinese-chess-message-interval nil
               `(lambda ()
                  (let ((buf (get-buffer chinese-chess-buffer)))
                    (when (and buf (buffer-live-p buf))
                      (with-current-buffer buf
                        (unless chinese-chess-message-updated
                          (chinese-chess-message ,later-msg)))))))
  (setq chinese-chess-message-updated nil))

(defun chinese-chess-current-message ()
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (buffer-substring (point) (line-end-position))))

(defun chinese-chess-type-message ()
  (concat
   (assoc-default chinese-chess-next-type chinese-chess-type-name)
   "执棋"))
(defun chinese-chess-state-message ()
  (concat
   (format "回合 %d: " (chinese-chess-round))
   (chinese-chess-type-message)
   (propertize
    (cond ((chinese-chess-king-dangerous 'black)
           (format "  %s将军" (assoc-default 'red chinese-chess-type-name)))
          ((chinese-chess-king-dangerous 'red)
           (format "  %s将军" (assoc-default 'black chinese-chess-type-name)))
          (t ""))
    'face 'font-lock-warning-face)))
;;}}}

;;{{{  Commands
(defun chinese-chess-unselect-man ()
  (when chinese-chess-selected-man
    (goto-char (car chinese-chess-selected-man))
    (mapc 'delete-overlay (overlays-at (point)))
    (setq chinese-chess-selected-man nil)))

(defun chinese-chess-select-man (row col)
  (when (chinese-chess-board-get row col)
    (chinese-chess-goto row col)
    (let ((man (chinese-chess-board-get row col))
          (inhibit-read-only t))
      (if (eq (chinese-chess-man-get man 'type)
              chinese-chess-next-type)
          (progn
            (setq chinese-chess-selected-man
                  (cons (point-marker) man))
            (overlay-put (make-overlay (point) (1+ (point)))
                         'face chinese-chess-selected-face))
        (chinese-chess-message-tempo
         (format "%s走棋" (assoc-default chinese-chess-next-type
                                         chinese-chess-type-name))
         (chinese-chess-current-message))))))

(defun chinese-chess-show-last-movement ()
  (save-excursion
    (mapc 'delete-overlay
          (overlays-in (progn (goto-char (point-min))
                              (forward-line chinese-chess-row-offset)
                              (point))
                       (point-max)))
    (let ((step (car chinese-chess-history)))
      (chinese-chess-goto (car step) (cadr step))
      (overlay-put (make-overlay (point) (+ (point) 1))
                   'face chinese-chess-last-move-face)
      (chinese-chess-goto (nth 3 step) (nth 4 step))
      (overlay-put (make-overlay (point) (min (+ (point) 3)
                                              (line-end-position)))
                   'face chinese-chess-last-move-face))))

(defun chinese-chess-change-turn ()
  (interactive)
  (setq chinese-chess-next-type
        (if (eq chinese-chess-next-type 'black)
            'red 'black))
  (chinese-chess-message (chinese-chess-state-message))
  (run-hooks 'chinese-chess-change-turn-hook))

(defun chinese-chess-move-man (man nrow ncol)
  (let ((rule (chinese-chess-man-get man 'rule))
        (row (chinese-chess-man-get man 'row))
        (col (chinese-chess-man-get man 'col))
        reason)
    (setq reason
          (catch 'cannot-move
            (when (funcall rule man nrow ncol)
              (chinese-chess-add-history
               'chinese-chess-history
               (append (list nrow ncol
                             (chinese-chess-man-get man 'name)
                             row col)
                       (let ((removed (chinese-chess-board-get nrow ncol)))
                         (if removed
                             (list (chinese-chess-man-get removed 'name))))))
              (chinese-chess-remove-man man)
              (chinese-chess-put-man man nrow ncol)
              (chinese-chess-show-last-movement)
              (setq chinese-chess-selected-man nil)
              (chinese-chess-change-turn))))
    (cond ((eq reason 'same-type)
           (chinese-chess-message-tempo
            "不能吃自己的棋子"
            (chinese-chess-current-message)))
          ((eq reason 'king-meet)
           (chinese-chess-message-tempo
            "将军相会"
            (chinese-chess-current-message)))
          (t (message nil)))))

(defun chinese-chess-select (event)
  (interactive "e")
  (let ((pos (event-end event))
        coords)
    (with-selected-window (posn-window pos)
      (setq coords (chinese-chess-coords-at (posn-point pos)))
      (if chinese-chess-selected-man
          (chinese-chess-unselect-man))
      (when coords
        (apply 'chinese-chess-select-man coords)))))

(defun chinese-chess-mouse-move (event)
  (interactive "e")
  (let ((pos (event-end event))
        coords)
    (with-selected-window (posn-window pos)
      (if (null chinese-chess-selected-man)
          (chinese-chess-message-tempo
           "先选择棋子"
           (chinese-chess-current-message))
        (setq coords (chinese-chess-coords-at (posn-point pos)))
        (when coords
          (chinese-chess-move-man (cdr chinese-chess-selected-man)
                                  (car coords) (cadr coords)))))))

(defun chinese-chess-str-to-coords (str)
  (let ((row (- 10 (- (aref str 1) ?0)))
        (col (+ (- (aref str 0) ?a) 1)))
    (if (and (chinese-chess-valid-row row)
             (chinese-chess-valid-col col))
        (list row col))))

(defun chinese-chess-move (str)
  (interactive "sMove: ")
  (setq str (downcase str))
  (let ((select (chinese-chess-str-to-coords (substring str 0 2)))
        (move (chinese-chess-str-to-coords
               (substring str (if (= (aref str 3) ?-) 3 2))))
        man)
    (if (and select (setq man (apply 'chinese-chess-board-get select)))
        (if (eq (chinese-chess-man-get man 'type)
                chinese-chess-next-type)
            (chinese-chess-move-man man (car move) (cadr move))
          (chinese-chess-message-tempo
           (format "%s走棋" (assoc-default chinese-chess-next-type chinese-chess-type-name))
           (chinese-chess-current-message)))
      (chinese-chess-message-tempo "输入错误" (chinese-chess-current-message)))))

(defun chinese-chess-backward ()
  (interactive)
  (if chinese-chess-history
      (progn
        (chinese-chess-unselect-man)
        (let ((step (car chinese-chess-history))
              removed)
          (chinese-chess-remove-man-at (car step) (cadr step))
          (chinese-chess-put-man
           (chinese-chess-make-man (nth 2 step) (nth 3 step) (nth 4 step)))
          (if (setq removed (nth 5 step))
              (chinese-chess-put-man
               (chinese-chess-make-man removed (car step) (cadr step)))))
        (setq chinese-chess-history (cdr chinese-chess-history))
        (chinese-chess-change-turn))
    (chinese-chess-message-tempo "不能再悔棋了" (chinese-chess-current-message))))

(defun chinese-chess-save ()
  (interactive)
  (with-temp-buffer
    (let ((file (read-file-name "Save to file: "))
          (buf (get-buffer chinese-chess-buffer))
          (standard-output (current-buffer)))
      (when (and buf (buffer-live-p buf))
        (insert "(\n;; chinese-chess-board-fen\n")
        (insert (format "%S\n" (with-current-buffer buf
                                 (chinese-chess-board-fen))))
        (insert ";; chinese-chess-next-type\n")
        (pp (buffer-local-value 'chinese-chess-next-type buf))
        (insert "\n;; chinese-chess-history\n")
        (pp (buffer-local-value 'chinese-chess-history buf))
        (insert ")")
        (write-file file)))))

(defun chinese-chess-load ()
  (interactive)
  (let ((file (read-file-name "Load file: "))
        chessmen vars)
    (setq vars
          (with-temp-buffer
            (insert-file-contents file)
            (read (current-buffer))))
    (when vars
      (chinese-chess-set-board (car vars))
      (setq chinese-chess-next-type (cadr vars)
            chinese-chess-history (nth 2 vars)))))

(define-derived-mode chinese-chess-mode nil "Chinese-Chess"
  "Play chinese chess game. Left mouse key to choose a chessman, right
mouse key to move the selected chessman. That is it. Enjoy yourself!"
  (make-local-variable 'chinese-chess-next-type)
  (make-local-variable 'chinese-chess-selected-man)
  (make-local-variable 'chinese-chess-board)
  (make-local-variable 'chinese-chess-message-updated)
  (make-local-variable 'chinese-chess-history)
  (chinese-chess-set-board
   "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR w - - 0 1"))

(defun chinese-chess-begin ()
  (interactive)
  (with-current-buffer (get-buffer-create chinese-chess-buffer)
    (chinese-chess-mode)
    (run-hooks 'chinese-chess-begin-hook)
    (select-window (display-buffer (current-buffer)))))

(defalias 'chinese-chess  'chinese-chess-begin)

;;{{{  puts mode
(defun chinese-chess-puts-mode ()
  (interactive)
  (let ((inhibit-read-only t)
        man)
    (chinese-chess-reset-board)
    (chinese-chess-put-buttons
     (cons "结束" 'chinese-chess-puts-begin))
    (chinese-chess-message "")
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (dolist (type '(black red))
        (dolist (name '("r" "n" "b" "a" "k" "p" "c"))
          (setq man (chinese-chess-make-man (intern
                                             (if (eq type 'black)
                                                 name
                                               (upcase name)))))
          (insert (propertize (chinese-chess-man-get man 'display)
                              'chessman man)
                  "  "))
        (delete-char -2)
        (forward-line 1)))
    (define-key chinese-chess-mode-map [down-mouse-1] 'chinese-chess-puts-select)
    (define-key chinese-chess-mode-map [down-mouse-3] 'chinese-chess-puts-put)))

(defun chinese-chess-puts-begin ()
  (interactive)
  (let ((type (intern (completing-read "哪方先走: "
                                       (list "black" "red")
                                       nil t)))
        (inhibit-read-only t))
    (if chinese-chess-selected-man
        (chinese-chess-unselect-man))
    (setq chinese-chess-next-type type
          chinese-chess-selected-man nil
          chinese-chess-history nil)
    (chinese-chess-put-buttons
     (cons "重新开始" 'chinese-chess-begin)
     (cons "悔棋" 'chinese-chess-backward))
    (goto-char (point-min))
    (forward-line 2)
    (delete-region (point) (progn (forward-line 1)
                                  (line-end-position)))
    (insert "\n")
    (chinese-chess-message (chinese-chess-state-message))
    (define-key chinese-chess-mode-map [down-mouse-1] 'chinese-chess-select)
    (define-key chinese-chess-mode-map [down-mouse-3] 'chinese-chess-mouse-move)
    (run-hooks 'chinese-chess-begin-hook)))

(defun chinese-chess-puts-select (event)
  (interactive "e")
  (let ((pos (event-end event))
        (inhibit-read-only t))
    (with-selected-window (posn-window pos)
      (if chinese-chess-selected-man
          (chinese-chess-unselect-man))
      (goto-char (posn-point pos)) 
      (when (chinese-chess-man-at (point))
        (overlay-put (make-overlay (point) (1+ (point)))
                     'face chinese-chess-selected-face)
        (setq chinese-chess-selected-man
              (cons (point-marker) (chinese-chess-man-at (point))))))))

(defun chinese-chess-puts-put (event)
  (interactive "e")
  (let ((pos (event-end event))
        coords)
    (with-selected-window (posn-window pos)
      (if (null chinese-chess-selected-man)
          (chinese-chess-message-tempo
           "先选择棋子"
           (chinese-chess-current-message))
        (let ((man (cdr chinese-chess-selected-man)))
          (setq coords (chinese-chess-coords-at (posn-point pos)))
          (when (chinese-chess-man-get man 'row)
            (chinese-chess-remove-man man)
            (chinese-chess-man-set man 'row nil))
          (when coords
            (chinese-chess-put-man (chinese-chess-copy-man man)
                                   (car coords) (cadr coords))))))))
;;}}}

(defun chinese-chess-board-fen ()
  (let ((fen "") j)
    (dolist (i (number-sequence 1 chinese-chess-height))
      (setq j 0)
      (dolist (m (append (aref chinese-chess-board (1- i)) nil))
        (if (null m)
            (incf j)
          (setq fen (concat fen
                            (if (> j 0) (number-to-string j) "")
                            (symbol-name (chinese-chess-man-get m 'name))))
          (setq j 0)))
      (setq fen (concat fen (if (> j 0) (number-to-string j) "") "/")))
    (mapconcat 'identity
               (list (substring fen 0 -1)
                     (if (eq chinese-chess-next-type 'black) "b" "w")
                     "-" "-" "0"
                     (number-to-string (chinese-chess-round)))
               " ")))

(defun chinese-chess-copy-fen ()
  (interactive)
  (kill-new (chinese-chess-board-fen)))

;;}}}

;;; chess.el ends here

