;;; dabbrev-expand-multiple.el --- dabbrev-expand for multiple

;; Copyright (C) 2007  khiker

;; Author: khiker <khiker+elisp@gmail.com>
;;         plus   <MLB33828@nifty.com>

;; Keywords: dabbrev

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

;; Dynamic abbrev for multiple selection.
;; This package tested on Emacs 22.

;;; Installation:

;; Put this file into your load-path, and,
;; add following line to .emacs.
;;
;;   (require 'dabbrev-expand-multiple)
;;   (global-set-key "\M-/" 'dabbrev-expand-multiple)
;;
;; Please match key setting to your favor.

;;; Usage:

;; dabbrev-expand-multiple behaves as follows.
;;
;; (1) first, execute dabbrev-expand-multiple.
;;     and, this function behaves as well as normal dabbrev-expand.
;; (2) next, pushing the key that  starts dabbrev-expand,
;;     and, multiple option selection menu appears.
;; (*) The abbrev displayed at a time is three in default.
;; (*) pushing the key that starts dabbrev-expand-multiple, M-/, or,
;;     SPC, display next three.
;; (*) pushing x or Backspace, display previous three.

;;; Config example:

;; ;; setting abbrev displayed at a time to five.
;; (setq dabbrev-expand-multiple-select-keys '("a" "s" "d" "f" "g"))
;;
;; ;; The seconds in which tooltip is displayed.
;; (setq dabbrev-expand-multiple-tooltip-timeout 2000)
;; ;; setting to disappear at ten seconds.
;; (setq dabbrev-expand-multiple-tooltip-timeout 10)
;;
;; ;; config tooltip face.  reference : M-x list-colors-display
;; (setq dabbrev-expand-multiple-tooltip-params
;;       '((foreground-color . "grey75")
;;         (background-color . "navy blue")
;;         (border-color . "black")))
;;
;; ;; put highlight to first expanded string.
;; (setq dabbrev-expand-multiple-highlight-face 'highlight)
;;
;; ;; Face used when inline display.
;; (setq dabbrev-expand-multiple-inline-show-face 'underline)
;; ;; Change inline display face. (not use underline.)
;; (setq dabbrev-expand-multiple-inline-show-face nil)
;;
;; ;; use tooltip.
;; (setq dabbrev-expand-multiple-use-tooltip t)
;; ;; use inline display. (not use tooltip.)
;; (setq dabbrev-expand-multiple-use-tooltip nil)

;;; Note:

;; following functions was remodeled by based on functions of SKK.
;;
;; dabbrev-expand-multiple-in-minibuffer-p
;; dabbrev-expand-multiple-show-tooltip
;; dabbrev-expand-multiple-mouse-position
;; dabbrev-expand-multiple-inline-show
;; dabbrev-expand-multiple-inline-hide
;;
;; SKK has Copyright to SKK  Development Team.
;;
;; and, advice to dabbrev-expand-multiple was remodeled by
;; based on dabbrev-ja.el.
;; dabbrev-ja.el has Copyright to Mr. TSUCHIYA Masatoshi.

;;; Code:

(require 'dabbrev)


;; Variables:

(defconst dabbrev-expand-multiple-version "1.1.1"
  "dabbrev-expand-multiple's version")

(defgroup dabbrev-expand-multiple nil
  "dabbrev-expand for multiple"
  :tag "dabbrev-expand for multiple"
  :group 'dabbrev-expand-multiple)

(defcustom dabbrev-expand-multiple-select-keys
  '("a" "s" "d")
  "*Key config for selecting options."
  :type  '(repeat string)
  :group 'dabbrev-expand-multiple)

(defcustom dabbrev-expand-multiple-multi-selection-keys '("\M-/")
  "*Key config for moving to multiple option displaying menu."
  :type  '(repeat string)
  :group 'dabbrev-expand-multiple)

(defcustom dabbrev-expand-multiple-next-keys '("\M-/" " ")
  "*Key config for moving to next complementarity list."
  :type  '(repeat string)
  :group 'dabbrev-expand-multiple)

(defcustom dabbrev-expand-multiple-previous-keys '("x" "\177")
  "*Key config for moving to previous complementarity list."
  :type  '(repeat string)
  :group 'dabbrev-expand-multiple)

(defcustom dabbrev-expand-multiple-tooltip-timeout 2000
  "*Seconds for displaying tooltip."
  :type  'number
  :group 'dabbrev-expand-multiple)

(defcustom dabbrev-expand-multiple-tooltip-params nil
  "*Face config for tooltip."
  :type 'boolean
  :group 'dabbrev-expand-multiple)

(defcustom dabbrev-expand-multiple-highlight-face 'highlight
  "*Face to highlight frist time expanded string."
  :type 'face
  :group 'dabbrev-expand-multiple)

(defcustom dabbrev-expand-multiple-inline-show-face 'underline
  "*A Variable to appoint a face when display a option in inline.
By skk-inline-show-face of SKK."
  :type '(radio (face  :tag "Specify face")
                (const :tag "Use default face attribute of candidate string."))
  :group 'dabbrev-expand-multiple)

(defcustom dabbrev-expand-multiple-use-tooltip t
  "*Non-nil means to use tooltip."
  :type 'boolean
  :group 'dabbrev-expand-multiple)

(defvar dabbrev-expand-multiple-inline-overlay nil)
(defvar dabbrev-expand-multiple-last-key nil)


;; Functions:

(defun dabbrev-expand-multiple ()
  "The function that do dynamic abbrev expansion for multiple selection.

When you executed the function, the function behaves as well as normal
dabbrev-expand. It complement only one candidate.
If that candidate is not something that you want, It displays multiple
selection by pushing `M-/' or key that launch dabbrev-expand-multiple.

The abbrev displayed at a time is three in default.
This number is controled by `dabbrev-expand-multiple-select-keys'.

style format is prepared three types.
- minibuffer
- inline
- tooltip
if variable `dabbrev-expand-multiple-use-tooltip' is non-nil,
display by tooltip.
Nil means displaying inline.
If variable is nil, And point is minibuffer, display by minibuffer."
  (interactive)
  ;; reset dabbrev's global variables
  (dabbrev--reset-global-variables)
  (let* ((target (dabbrev--abbrev-at-point))
         (abbrev (dabbrev--find-expansion target 0 dabbrev-case-fold-search))
         (prompt "")
         action overlay)
    (cond
     ;; Dynamic expansion found.
     (abbrev
      ;; insert abbrev
      (insert (substring abbrev (length target)))
      ;; hightlight abbrev
      (setq overlay (make-overlay (- (point) (length abbrev)) (point)))
      (overlay-put overlay 'face dabbrev-expand-multiple-highlight-face)
      ;; wait key input. and, record pushed key.
      (setq action (read-key-sequence-vector prompt))
      ;; reset hightlight
      (delete-overlay overlay)
      ;; record last-command-char
      (setq dabbrev-expand-multiple-last-key last-command-char)
      (cond
       ((dabbrev-expand-multiple-selection-keys-p
         action dabbrev-expand-multiple-multi-selection-keys t)
        ;; reset abbrev
        (delete-char (- (length abbrev)))
        (insert target)
        ;; start dabbrev-expand-multiple
        (dabbrev-expand-multiple-main 1 (list (list abbrev))))
       ;; execute command that bound to pushed key.
       (t
        (dabbrev-expand-multiple-do-last-command action))))
     ;; Dynamic expansion *Not* found.
     (t
      (message "No dynamic expansion for `%s' found" target)))))

(defun dabbrev-expand-multiple-main (num alist)
  "The function that do main part of dynamic abbrev expansion for
multiple selection."
  ;; setting local variable
  (let* ((target (dabbrev--abbrev-at-point))
         (keys (reverse dabbrev-expand-multiple-select-keys))
         i prompt abbrev-list sel abbrev action)
    (while (> num -1)
      ;; initialize local variables
      (setq i (length keys)
            prompt ""
            abbrev-list (nth num alist)
            sel (null abbrev-list)
            abbrev nil
            action nil)
      ;; setting abbrev for display
      (while (and (> i 0)
                  (setq abbrev
                        (if sel
                            ;; get new abbrev
                            (dabbrev--find-expansion
                             target 0 dabbrev-case-fold-search)
                          ;; next abbrev has been already gotten.
                          (nth (- (length keys) i) abbrev-list))))
        (add-to-list 'abbrev-list abbrev t)
        (setq prompt (format "%s(%s): %s%s" prompt (nth (1- i) keys) abbrev
                             (if dabbrev-expand-multiple-use-tooltip "\n" " ")))
        (setq i (1- i)))
      ;; couldn't get new abbrev.
      (when (null abbrev-list)
        (setq prompt "No dynamic expansion"))
      ;; create tooltip, overlay or minibuffer message.
      (cond
       ;; point in minibuffer
       ((dabbrev-expand-multiple-in-minibuffer-p)
        (setq action (read-key-sequence-vector prompt)))
       ;; use tooltip
       (dabbrev-expand-multiple-use-tooltip
        (let* ((P (dabbrev-expand-multiple-mouse-position))
               (frame (car P))
               (x (cadr P))
               (y (cddr P))
               (oP (mouse-position))
               (oframe (car oP))
               ;; unless mouse curosr is on the frame,
               ;; not be able to get original mouse position.
               ;; this line is the countermeasure.
               (ox (or (cadr oP) 0))
               (oy (or (cddr oP) 15)))
          ;; move mouse position.
          (set-mouse-position frame x y)
          (dabbrev-expand-multiple-show-tooltip prompt)
          (setq action (read-key-sequence-vector ""))
          (tooltip-hide)
          (set-mouse-position oframe ox oy)))
       ;; use inline show
       (t
        (dabbrev-expand-multiple-inline-show
         prompt dabbrev-expand-multiple-inline-show-face)
        (setq action (read-key-sequence-vector ""))
        (dabbrev-expand-multiple-inline-hide)))
      ;; clear minibuffer
      (message "")
      (setq sel (length (member (string (aref action 0)) keys)))
      (add-to-list 'alist abbrev-list t)
      (cond
       ;; next selection
       ((dabbrev-expand-multiple-selection-keys-p
         action dabbrev-expand-multiple-next-keys t)
        (when abbrev-list (setq num (1+ num))))
       ;; previous selection
       ((dabbrev-expand-multiple-selection-keys-p
         action dabbrev-expand-multiple-previous-keys)
        (when (> num 0) (setq num (1- num))))
       ;; exit while loop
       (t (setq num -1))))
    (cond
     ;; pushed a key except dabbrev-expand-multiple-select-keys
     ((or (= sel 0) (> sel (length abbrev-list)))
      (dabbrev-expand-multiple-do-last-command action))
     (t
      ;; insert selected string
      (insert
       (substring (nth (1- sel) abbrev-list) (length target)))))))

(defun dabbrev-expand-multiple-selection-keys-p (action keys &optional last)
  "Check whether these keys were pushed.
1. A key same as the key which you used to lauch dabbrev-expand-multiple.
2. one of variables keys.
"
  (not (not (memq (aref action 0)
                  (apply 'append
                         (when last
                           (list dabbrev-expand-multiple-last-key))
                         (mapcar
                          (lambda (x)
                            (cond
                             ((stringp x)
                              (listify-key-sequence x))
                             ((numberp x)
                              (list x))
                             ((listp x)
                              (list (event-convert-list x)))
                             ((vectorp x)
                              (list (event-convert-list (aref x 0))))))
                          keys))))))

(defun dabbrev-expand-multiple-do-last-command (action)
  "Execute command assigned to the key which you input."
  (let ((last-command-char (aref action 0))
        (command (key-binding action)))
    (when command
      (call-interactively command))
    (message "")))


;; Functions from SKK

(defun dabbrev-expand-multiple-in-minibuffer-p ()
  "Check whether point is current-buffer or minibuffer.
by skk-in-minibuffer-p of SKK."
  (eq (current-buffer) (window-buffer (minibuffer-window))))

(defun dabbrev-expand-multiple-show-tooltip (text)
  "Display tooltip.
by skk-tooltip-show-1 of SKK."
  (condition-case error
      (let ((params (copy-sequence tooltip-frame-parameters))
            fg bg)
        (if dabbrev-expand-multiple-tooltip-params
            ;; tooltip display config for user
            (dolist (cell dabbrev-expand-multiple-tooltip-params)
              (setq params (tooltip-set-param params
                                              (car cell)
                                              (cdr cell))))
          ;; set the default of tooltip
          (setq fg (face-attribute 'tooltip :foreground))
          (setq bg (face-attribute 'tooltip :background))
          (when (stringp fg)
            (setq params (tooltip-set-param params 'foreground-color fg))
            (setq params (tooltip-set-param params 'border-color fg)))
          (when (stringp bg)
            (setq params (tooltip-set-param params 'background-color bg))))
        (x-show-tip (propertize text 'face 'tooltip)
                    (selected-frame)
                    params
                    dabbrev-expand-multiple-tooltip-timeout
                    tooltip-x-offset
                    tooltip-y-offset))
    (error
     (message "Error while displaying tooltip: %s" error)
     (sit-for 1)
     (message "%s" text))))

(defun dabbrev-expand-multiple-mouse-position ()
  "Return the position of point as (FRAME X . Y).
Analogous to mouse-position.

by skk-e21-mouse-position of SKK."
  (let* ((w (selected-window))
         (edges (window-edges w))
         (list
          (compute-motion
           (max (window-start w) (point-min))
           '(0 . 0)
           (point)
           (cons (window-width w) (window-height w))
           (1- (window-width w))
           (cons (window-hscroll w) 0)
           w)))
    (cons (selected-frame)
          (cons (+ (car edges)       (car (cdr list)))
                (+ (car (cdr edges)) (car (cdr (cdr list))))))))

(defun dabbrev-expand-multiple-inline-show (string face)
  "Display string on inline by using overlay.
by skk-inline-show of SKK."
  (dabbrev-expand-multiple-inline-hide)
  (unless (dabbrev-expand-multiple-in-minibuffer-p)
    (setq dabbrev-expand-multiple-inline-overlay
          (make-overlay (point) (point)))
    (overlay-put dabbrev-expand-multiple-inline-overlay
                 'after-string
                 (apply #'propertize string
                        (if face `(face ,face) nil)))))

(defun dabbrev-expand-multiple-inline-hide ()
  "Delete overlay of dabbrev-expand-multiple-inline-show.
by skk-inline-hide of SKK."
  (when dabbrev-expand-multiple-inline-overlay
    (delete-overlay dabbrev-expand-multiple-inline-overlay)
    (setq dabbrev-expand-multiple-inline-overlay nil)))


;; From http://namazu.org/~tsuchiya/elisp/dabbrev-ja.el

;; dabbrev-expand-multiple for japanese words.
(or (boundp 'MULE)                      ; Mule2 and
    (featurep 'xemacs)                  ; XEmacs are setting-free.
    (let (current-load-list)
      (defadvice dabbrev-expand-multiple
        (around modify-regexp-for-japanese activate compile)
        "Modify `dabbrev-abbrev-char-regexp' dynamically for Japanese words."
        (if (bobp)
            ad-do-it
          (let ((dabbrev-abbrev-char-regexp
                 (let ((c (char-category-set (char-before))))
                   (cond
                    ((aref c ?a) "[-_A-Za-z0-9]") ; ASCII
                    ((aref c ?j)                  ; Japanese
                     (cond
                      ((aref c ?K) "\\cK") ; katakana
                      ((aref c ?A) "\\cA") ; 2byte alphanumeric
                      ((aref c ?H) "\\cH") ; hiragana
                      ((aref c ?C) "\\cC") ; kanji
                      (t "\\cj")))
                    ((aref c ?k) "\\ck") ; hankaku-kana
                    ((aref c ?r) "\\cr") ; Japanese roman ?
                    (t dabbrev-abbrev-char-regexp)))))
            ad-do-it)))))

(provide 'dabbrev-expand-multiple)

;; Local Variables:
;; Coding: iso-2022-7bit
;; End:

;; dabbrev-expand-multiple.el ends here
