;;; oprofile-mode.el --- An oprofile callgraph viewer
;;
;; Author: Vikram Visweswaraiah (vviswesw at cisco.com)
;; Version: 0.1a
;; Copyright (c) 2008, Cisco Systems, Inc.
;; All rights reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;; Commentary:
;; 
;; If you don't know what oprofile is, read about it at 
;; <http://oprofile.sourceforge.net/>. If you start using it, particularly,
;; start generating callgraph profiles, you may find this program useful
;; in parsing it and providing commonly required transformations.
;; 
;; 1) 
;; Make sure oprofile-mode.el is in your load path, for example, in your
;; ~/.emacs, use:
;;
;; (setq load-path (append (list
;;                (expand-file-name "~/lib/emacs/lisp"))
;;                load-path))
;; 2) 
;; Add following lines in your ~/.emacs, change colors to taste, I
;; generally work with dark backgrounds
;;
;; (setq font-lock-ok-face 'font-lock-ok-face)
;; (setq font-lock-boldref-face 'font-lock-boldref-face)
;; (setq font-lock-face-attributes
;;        '(
;;         (font-lock-ok-face              "green" nil t nil nil)
;;         (font-lock-boldref-face         "khaki" nil t nil nil))
;; )
;; (add-hook 'oprofile-mode-hook 'turn-on-font-lock)
;; (autoload 'oprofile-mode "oprofile-mode" "OPROFILE editing mode." t)
;; 
;; 3)
;; Setup autoloading, we'll assume profile files end with .prof
;;
;; (setq auto-mode-alist (append 
;;                              '(
;;                                ("\\.prof$"     . oprofile-mode)
;;                                ) auto-mode-alist))
;;
;; 4)
;; Look at keybindings below for functions of interest: short summmary:
;;
;; ESC p     -- move forward to previous symbol
;; ESC n     -- move forward to next symbol
;; Ctrl-c /  -- search for symbol under cursor
;; Ctrl-c Ctrl-c -- generate callstack for symbol under cursor
;; Ctrl-c Ctrl-f -- generate flat profile from current callgraph data

;;; Code:


; local keybindings
(defvar oprofile-mode-map (make-sparse-keymap)
  "Keymap used in oprofile-mode.")
(define-key oprofile-mode-map
  (read-kbd-macro "C-c /") 'oprofile-find-sym)
(define-key oprofile-mode-map
  (read-kbd-macro "C-c C-c") 'oprofile-generate-callstack)
(define-key oprofile-mode-map
  (read-kbd-macro "C-c C-f") 'oprofile-generate-flat-profile)
(define-key oprofile-mode-map
  (read-kbd-macro "ESC p") 'oprofile-find-sym-backward)
(define-key oprofile-mode-map
  (read-kbd-macro "ESC n") 'oprofile-find-sym-forward)
                                                                                
; font-lock specs
(defvar oprofile-mode-fl-keywords
  (list
   '("^[0-9]+\\([ 0-9A-Za-z\\._-]\\)+"
     0 font-lock-boldref-face t)
   '("^-+"
     0 font-lock-ok-face t)))

; hooks to run
(defvar oprofile-mode-hook nil
  "Hook run when oprofile-mode is enabled.")


(defvar oprofile-match-sym-re "^[0-9]+\\([- 0-9A-Za-z\\._]\\)+")

(defvar oprofile-max-depth 50)

(make-variable-frame-local 'callstack-buf)
(make-variable-frame-local 'flatprof-buf)

;;
;; private
;;
(defun oprofile-regexp-str (symbol)
  (concat oprofile-match-sym-re " " symbol "$"))

;;
;; find a symbol
;;
(defun oprofile-find-sym (&optional sym)
  (interactive)
  (if (equal sym nil)
      (setq sym (thing-at-point 'symbol)))
  (beginning-of-buffer)
  (if (re-search-forward (oprofile-regexp-str sym) (buffer-end 1) t)
      (beginning-of-line)
    (message "**Error: symbol not found")))

;;
;; traverse symbols forward from current point
;;
(defun oprofile-find-sym-forward ()
  (interactive)
  (end-of-line)
  (if (re-search-forward oprofile-match-sym-re (buffer-end 1) t)
      (progn (beginning-of-line)
	     t)
    (progn (message "**Symbol not found")
	   nil)))

;;
;; traverse symbols backwards from current point
;;
(defun oprofile-find-sym-backward ()
  (interactive)
  (beginning-of-line)
  (if (re-search-backward oprofile-match-sym-re 1 t)
      (progn (beginning-of-line)
	     t)
    (progn (message "**Symbol not found")
	   nil)))

;;
;; generate callstack using starting symbol at point
;;
(defun oprofile-generate-callstack ()
  (interactive)
  (setq depth 0)
  (save-excursion 
    (make-variable-frame-local 'opstack-list)
    (setq sym (thing-at-point 'symbol))
    ;; if this is not a starting point figure that out first
    (beginning-of-line)
    (if (not (looking-at "^[0-9]+"))
	(setq sym (oprofile-find-sym sym)))
    (setq opdata (split-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (setq sym-perc (concat (car (cdr opdata)))) ;; this is where it ends
    (setq sym-name (concat (car (nreverse opdata))))
    (push (concat sym-perc "  " sym-name) opstack-list)
    ;; now backtrack
    (previous-line 1)
    (while (and (not (looking-at "-+$"))
		(<= depth oprofile-max-depth))
      (setq depth (+ 1 depth))
      (setq opdata (split-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (setq sym-perc (concat (car (cdr opdata))))
      (setq prev-sym-name sym-name)
      (setq sym-name (concat (car (nreverse opdata))))
      (if (equal prev-sym-name sym-name)
	  (progn 
	    (push (concat sym-perc "  [R] " sym-name) opstack-list)
	    (previous-line 1))
	(push (concat sym-perc "  " sym-name) opstack-list)
	(oprofile-find-sym sym-name)
	(previous-line 1)))
    ;; now print callstack in another buffer
    (if (not (equal callstack-buf nil))
	(kill-buffer callstack-buf))
    (setq callstack-buf (generate-new-buffer "*Callstack*"))
    (if (equal (count-windows) 1)
	(split-window-vertically))
    (other-window -1)
    (switch-to-buffer callstack-buf)
    (if (not (equal buffer-read-only nil))
	(toggle-read-only))
    (while (not (equal (car opstack-list) nil))
      (insert (concat (car opstack-list) "\n"))
      (setq opstack-list (cdr opstack-list)))
    (if (equal buffer-read-only nil)
	(toggle-read-only))
    (beginning-of-buffer)))

;;
;; generate flat profile from the callgraph data in main buffer
;;
(defun oprofile-generate-flat-profile ()
  (interactive)
  (setq depth 0)
  (save-excursion 
    (make-variable-frame-local 'opstack-list)
    (beginning-of-buffer)
    (while (not (equal (oprofile-find-sym-forward) nil))
      (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) opstack-list))
    (setq opstack-list (nreverse opstack-list))
    ;; now print flat profile in another buffer
    (if (not (equal flatprof-buf nil))
	(kill-buffer flatprof-buf))
    (setq flatprof-buf (generate-new-buffer "*FlatProfile*"))
    (if (equal (count-windows) 1)
	(split-window-vertically))
    (other-window -1)
    (switch-to-buffer flatprof-buf)
    (if (not (equal buffer-read-only nil))
	(toggle-read-only))
    (while (not (equal (car opstack-list) nil))
      (insert (concat (car opstack-list) "\n"))
      (setq opstack-list (cdr opstack-list)))
    (if (equal buffer-read-only nil)
	(toggle-read-only))
    (beginning-of-buffer)))

;
; the major mode
;
(defun oprofile-mode ()
  "Major mode for viewing oprofile callgraph data."
  (interactive)
  (kill-all-local-variables)

  (use-local-map oprofile-mode-map)

  (setq mode-name "OPROFILE")
  (setq major-mode 'oprofile-mode)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(oprofile-mode-fl-keywords))

  (if (equal buffer-read-only nil)
      (toggle-read-only))

  (setq callstack-buf nil)

  (run-hooks 'text-mode-hook)
  (run-hooks 'oprofile-mode-hook))

;
; oprofile-mode ends here
;
