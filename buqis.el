;;; buqis.el --- Quick buffers switcher.

;; Copyright (C) 2003 by Free Software Foundation, Inc.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Maintainer: none, mail me if you want to maintain this code.
;; Created: Thu Nov 20 14:28:55 MSK 2003
;; Keywords: buffers, matching
;; X-CVS: $Id$

;; This file is NOT part of XEmacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:
;;
;; Main feature of buqis is fast navigation and abbility to attach
;; some info, used by buqis to filter buffers to selected frame. Say
;; in frame1 you work with emacs lisp files, in frame2 you work under
;; project written in C and frame3 you use to browse Web with w3m. So
;; in frame1 you want swithing only to .el buffers, in frame2 to .c,
;; .h or info and frame3 only for w3m buffers. You may do it executing
;; `buqis-put-frame-ctx' command.
;;
;; You may start by adding next to your .emacs:
;;
;;	(require 'buqis)
;;	(buqis-install-keybinds)
;;	(buqis-add-default-ctxs)
;;
;; To change buqis context to selected frame use:
;;
;;	M-x buqis-put-frame-ctx <RET>
;;
;; If you want buqis context in title then set `frame-title-format' to
;; something like this:
;;
;;	(setq frame-title-format
;;	      '("Emacs: %b"
;;		(buqis-current-context " -")
;;		(buqis-current-context buqis-current-context)
;;		(buqis-current-context "-")
;;		" %*%& %f"))

;;; Bugs:
;;
;; If you like to kill *scratch* buffer at startup, than you will find
;; buqis unusable :).

;;; Code:
;;

;; Variables
(defvar buqis-color-face 'blue
  "*Face used to colorize current buffer name.")

(defvar buqis-current-context nil
  "Buqis context name for current frame.")

(defvar buqis-ctx-list '(("nil" . nil))
  "Contexts list for buqis. Use `buqis-add-ctx' to modify it.")


;; Functions
(defun buqis-install-keybinds ()
  "Installs default keyboard bindings for buqis switching."
  (define-key global-map (kbd "M-[") 'buqis-gotoprev)
  (define-key global-map (kbd "M-]") 'buqis-gotonext)
  )

(defun buqis-add-ctx (ctx-name ctx-regex)
  "Adds buqis context represented by CTX-NAME and CTX-REGEX."
  (add-to-list 'buqis-ctx-list (cons ctx-name ctx-regex)))

(defun buqis-add-default-ctxs ()
  "Adds some default buqis contexts."
  (buqis-add-ctx "c" ".*\\.[ch]\\|\\*info\\*")
  (buqis-add-ctx "w3m" "\\*w3m\\*\\|\\*w3m\\*<[0-9]+>")
  (buqis-add-ctx "elisp"  ".*\\.el\\|\\.emacs\\|\\.gnus")
  )

;; Default buqis buffer filter .. i don't want buffers like
;; " Warnings*" and " SPEEDBAR" to be in buqis list
;; Please keep *scratch* in it :) *scratch* rocks
(defun buqis-select-files (buff)
  "Return nil if BUFF need to be removed from list."
  (let ((bname (buffer-name buff))
	(fctx (frame-property (selected-frame) 'buqis-context)))
    (cond ((string= bname "*scratch*") t)
	  ((and fctx (cdr fctx)) (if (string-match (cdr fctx) bname) t nil))
	  ((string= bname "*info*") t)
	  ((string= bname " SPEEDBAR") nil)
	  ((char= (string-to-char bname) ?*) nil) ; "*lala.."
	  ((char= (aref (vconcat bname) 1) ?*) nil) ;" *lala.."
	  ((char= (car (reverse (string-to-list bname))) ?*) nil)
	  (t t))))

(defun buqis-colorize-text (text)
  "Colorize given TEXT with `buqis-color-face' face."
  (let ((newtext (concat text)))
    (put-text-property 0 (length text) 'face buqis-color-face newtext)
    newtext))

(defun buqis-display-bufstr (buffs)
  "Display buqis buffers list BUFFS in minibuffer."
  (display-message 'no-log
    (let ((cbufname (buffer-name (current-buffer)))
	  (rst nil))
      ;; Reorder elements in buffs so "*scratch*" will be first
      ;; What if there is no *scratch* buffer? :)
      (while (not (string= (buffer-name (car buffs)) "*scratch*"))
	(setq buffs (nconc (cdr buffs) (list (car buffs)))))

      (while buffs
	(progn
	  (setq cbuf (buffer-name (car buffs)))
	  (if (string= cbufname cbuf)
	      (setq rst (concat rst (when rst " | ") (buqis-colorize-text cbuf)))
	    (setq rst (concat rst (when rst " | ") cbuf)))
	  (setq buffs (cdr buffs))))
      rst)))

(defun buqis-make-buffs (buff-list &optional filter)
  "Make list of buffers by driving BUFF-LIST through the FILTER function."
  (let ((curr (current-buffer))
	(buffs buff-list)
	(nbuffs nil))

    (while buffs
      (let* ((buff (car buffs))
	     (name (buffer-name buff))
	     (isadd (if (null name) nil (funcall filter buff))))

	(setq buffs (cdr buffs))
	(when (eq isadd t)
	  (add-to-list 'nbuffs buff) nil)))
    nbuffs))


;; Commands
(defun buqis-put-frame-ctx (ctx-name)
  "Puts buqis context associated with CTX-NAME as property for
`selected-frame'."
  (interactive (list (completing-read "Select buqis context: "
				      buqis-ctx-list nil t nil nil)))

  (let ((clist buqis-ctx-list)
	(el nil))
    (while clist
      (if (string= (caar clist) ctx-name)
	  (progn
	    (setq el (car clist))
	    (setq clist nil))
      (setq clist (cdr clist))))

    (setq buqis-current-context el)
    (set-frame-property (selected-frame) 'buqis-context el)))

(defun buqis-gotoprev (arg)
  "Switches to previous buffer in filtered buffers list."
  (interactive "p")

  (let ((buffs (buqis-make-buffs (buffer-list) 'buqis-select-files)))
    (if (null buffs)
	(message "No valid buffers")

      (setq arg (1- arg))
      (let ((buf (nth arg buffs)))

	(let ((i 0))
	  (while (> arg i)
	    (switch-to-buffer (nth i buffs))
	    (setq i (1+ i))))

	(switch-to-buffer buf)
	(buqis-display-bufstr (reverse buffs))))))

(defun buqis-gotonext (arg)
  "Switches to next buffer in filtered buffers list."
  (interactive "p")

  (let ((buffs (buqis-make-buffs (reverse (buffer-list)) 'buqis-select-files)))
    (if (null buffs)
	(message "No valid buffers")

      (let ((buf (nth arg buffs)))
	(when (null buf)
	  (setq arg 1)
	  (setq buf (car buffs)))

	(while (> arg 0)
	  (bury-buffer)
	  (setq arg (1- arg)))

	(switch-to-buffer buf)
	(buqis-display-bufstr buffs)))))


;; Hooks
(add-hook 'select-frame-hook
	  (lambda ()
	    (setq buqis-current-context
		  (car (frame-property (selected-frame) 'buqis-context)))))
(add-hook 'map-frame-hook
	  (lambda (frame)
	    (setq buqis-current-context
		  (car (frame-property frame 'buqis-context)))))

(provide 'buqis)

;;; buqis.el ends here
