;;; iy-go-to-char.el --- Go to next CHAR which is similar to "f" in vim
;; Copyright (C) 2009 Ian Yang

;; Author: Ian Yang <doit dot ian (at) gmail dot com>
;; Keywords: navigation, search
;; Filename: iy-go-to-char.el
;; Description: Go to char
;; Created: 2009-08-23 01:27:34
;; Version: 3.0
;; Last-Updated: 2013-04-08 08:18:00
;; URL: https://github.com/doitian/iy-go-to-char
;; Compatibility: GNU Emacs 23.1.1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package defines the function `iy-go-to-char' which behaves
;; like "f" in vim. It reads a char and go the next Nth occurence of
;; the char. User can continue such search using that char key.

;; To use, make sure this file is on your `load-path' and put the
;; following in your .emacs file:
;;
;;     (require 'iy-go-to-char)
;;
;; Then you can bind functions like:
;;
;;     (global-set-key (kbd "C-c f") 'iy-go-to-char)
;;     (global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
;;     (global-set-key (kbd "C-c ;") 'iy-go-to-char-continue)
;;     (global-set-key (kbd "C-c ,") 'iy-go-to-char-continue-backward)

;; Except repeating the char key, followings keys are defined before
;; quitting the search:
;;
;;    ;   -- search forward the char, customizable:
;;           `iy-go-to-char-key-forward'
;;
;;    ,   -- search backward the char, customizable:
;;           `iy-go-to-char-key-backward'
;;
;;    C-g -- quit
;;
;;    C-s -- start `isearch-forward' using char as initial search
;;           string
;;
;;    C-r -- start `isearch-backward' using char as initial search
;;           string
;;
;;    C-w -- quit and kill region between start and current point. If region is
;;           activated before search, then use the original mark instead of the
;;           start position.
;;
;;    M-w -- quit and save region between start and current point. If region is
;;           activated before search, use the mark instead of start position.
;;
;; All other keys will quit the search. Then the key event is
;; intepreted in the original environment before search.
;;
;; if the search quits because of error or using "C-g", point is set
;; back to the start position. Otherwise, point is not change and the
;; start position is set as marker. So you can use "C-x C-x" back to
;; that position.

;; `iy-go-to-char-backward' search backward by default. Also the search can
;; cross lines. To continue search last char, use `iy-go-to-char-continue' and
;; `iy-go-to-char-continue-backward'.

;;; Change Log:
;; 2013-04-08 (3.0)
;;
;;    - When jump is started backward (with negative parameter, or
;;      `iy-go-to-char-backward'). Repeating continues the search
;;      backward. Also `iy-go-to-char-key-forward' jumps with the same
;;      direction when jump is started, and `iy-go-to-char-key-backward' jumps
;;      to reverse direction.
;;
;; 2013-03-25 (2.1)
;;    - Fix a but that I forget to set `mc--this-command'
;; 2013-03-25 (2.0)
;;    - Use overriding-local-map to setup keymap
;;    - multiple-cursors compatible
;; 2012-04-16 (1.1)
;;    - fix C-s/C-r to enter isearch

;;; Code:

(defgroup iy-go-to-char nil
  "go to char like f in vim."
  :link '(emacs-commentary-link "iy-go-to-char")
  :prefix "iy-go-to-char-"
  :group 'matching)

(defcustom iy-go-to-char-key-forward ?\;
  "Default key used to go to next occurence of the char"
  :type 'character
  :group 'iy-go-to-char)
(defcustom iy-go-to-char-key-backward ?\,
  "Default key used to go to previous occurence of the char"
  :type 'character
  :group 'iy-go-to-char)

(defvar iy-go-to-char-start-pos nil
  "position where go to char mode is enabled")

(defvar iy-go-to-char-start-dir 1
  "jump start direction")

(defvar iy-go-to-char-last-char nil
  "last char used in iy-go-to-char")

(defvar iy-go-to-char-last-step 1
  "last jump step used in iy-go-to-char")

(defvar iy-go-to-char-keymap (let ((map (make-sparse-keymap)))
                               (define-key map (kbd "C-s") 'iy-go-to-char-isearch)
                               (define-key map (kbd "C-r") 'iy-go-to-char-isearch-backward)
                               (define-key map (kbd "C-w") 'iy-go-to-char-kill-region)
                               (define-key map (kbd "M-w") 'iy-go-to-char-kill-ring-save)
                               (define-key map (kbd "C-g") 'iy-go-to-char-quit)

                               map)
  "keymap used when iy-go-to-char is ongoing")

 

(defun iy-go-to-char--isearch-setup ()
  (remove-hook 'isearch-mode-hook 'iy-go-to-char--isearch-setup)
  (setq isearch-string (if iy-go-to-char-last-char (string iy-go-to-char-last-char) ""))
  (isearch-search-and-update))

(defun iy-go-to-char--override-local-map (char)
  "Override the local key map"
  (setq overriding-local-map
        (let ((map (copy-keymap iy-go-to-char-keymap)))

          (define-key map (string char) 'iy-go-to-char-continue)
          (define-key map (string iy-go-to-char-key-forward) 'iy-go-to-char-continue)
          (define-key map (string iy-go-to-char-key-backward) 'iy-go-to-char-continue-backward)

          (define-key map [t] 'iy-go-to-char-pass-through)

          map)))

 

(defun iy-go-to-char-done ()
  "Finish iy-go-to-char-mode"
  (interactive)
  (push-mark iy-go-to-char-start-pos t)
  (setq iy-go-to-char-start-pos nil)
  (setq overriding-local-map nil))

(defun iy-go-to-char-quit ()
  "Quit iy-go-to-char-mode"
  (interactive)
  (goto-char iy-go-to-char-start-pos)
  (setq iy-go-to-char-start-pos nil)
  (setq overriding-local-map nil))

(defun iy-go-to-char-pass-through ()
  "Finish iy-go-to-char-mode and invoke the corresponding command"
  (interactive)
  (iy-go-to-char-done)
  (let* ((keys (progn
                 (setq unread-command-events
                       (append (this-single-command-raw-keys)
                               unread-command-events))
                 (read-key-sequence-vector "")))
         (command (and keys (key-binding keys))))
    (when (commandp command)
      (setq this-command command
            this-original-command command
            mc--this-command command)
      (call-interactively command))))

(defun iy-go-to-char-isearch ()
  "Start isearch using the char"
  (interactive)
  (iy-go-to-char-done)
  (add-hook 'isearch-mode-hook 'iy-go-to-char--isearch-setup)
  (isearch-forward))

(defun iy-go-to-char-isearch-backward ()
  "Start isearch backward using the char"
  (interactive)
  (iy-go-to-char-done)
  (add-hook 'isearch-mode-hook 'iy-go-to-char--isearch-setup)
  (isearch-backward))

(defun iy-go-to-char-kill-region ()
  "Kill region between current position and the position where go to char starts"
  (interactive)
  (iy-go-to-char-done)
  (kill-region (point) (mark)))

(defun iy-go-to-char-kill-ring-save ()
  "Save region between current position and the position where go to char starts"
  (interactive)
  (iy-go-to-char-done)
  (kill-ring-save (point) (mark)))

(defun iy-go-to-char--command ()
  "Repeatable command to really move cursor."
  (interactive)
  (let ((n (if (< iy-go-to-char-start-dir 0)
               (- iy-go-to-char-last-step)
             iy-go-to-char-last-step)))
    (search-forward (string iy-go-to-char-last-char) nil nil n)))

(defun iy-go-to-char--internal (n char)
  "Store information when jump is started. Delegate real cursor
movement to `iy-go-to-char--command' and set it as
`mc--this-command' so that iy-go-to-char can be used with multiple-cursors.
"
  (interactive "p\ncGo to char: ")
  (setq iy-go-to-char-last-step n)
  (setq iy-go-to-char-last-char char)
  (unless iy-go-to-char-start-pos
    (setq iy-go-to-char-start-pos (point))
    (iy-go-to-char--override-local-map char))

  (setq this-original-command 'iy-go-to-char--command
        this-command 'iy-go-to-char--command
        mc--this-command 'iy-go-to-char--command)

  (message "%s %d" (make-string 1 iy-go-to-char-last-char) iy-go-to-char-start-dir)

  (call-interactively 'iy-go-to-char--command))

;;;###autoload
(defun iy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.

Typing key of CHAR will move to the next occurence of CHAR.
Typing ; will move to the next occurence of CHAR.
Typing , will move to the previous occurence of CHAR.
Typing C-g will quit and return to the original point.
Typing C-s or C-r will start `isearch` using CHAR.
Typing C-w or M-w will kill/copy between current point and the start point.
Unless quit using C-g or the region is activated before searching, the start
 point is set as mark.
"
  (interactive "p\ncGo to char: ")
  (setq iy-go-to-char-start-dir (if (< n 0) -1 1))
  (iy-go-to-char--internal n char))

;;;###autoload
(defun iy-go-to-char-backward (n char)
  "Move backward to Nth occurence of CHAR.
Typing key of CHAR will move to the previous occurence of CHAR.
Typing ; will move to the next occurence of CHAR.
Typing , will move to the previous occurence of CHAR.
Typing C-g will quit and return to the original point.
Typing C-s or C-r will start `isearch` using CHAR"
  (interactive "p\ncGo to char: ")
  (setq iy-go-to-char-start-dir (if (< n 0) 1 -1))
  (iy-go-to-char--internal n char))

;;;###autoload
(defun iy-go-to-char-continue (n)
  "Continue last `iy-go-to-char` or `iy-go-to-char-backward`"
  (interactive "p")
  (when iy-go-to-char-last-char
    (iy-go-to-char--internal n iy-go-to-char-last-char)))

;;;###autoload
(defun iy-go-to-char-continue-backward (n)
  "Continue last `iy-go-to-char` or `iy-go-to-char-backward`"
  (interactive "p")
  (when iy-go-to-char-last-char
    (iy-go-to-char--internal (- n) iy-go-to-char-last-char)))

 

(provide 'iy-go-to-char)

;;; iy-go-to-char.el ends here
