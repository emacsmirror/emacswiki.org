;;; tjic-shell.el
;;
;; Copyright (C) 2003 Travis J.I. Corcoran
;;
;; This is free software
;;
;; Author: Travis J.I. Corcoran <tjic_emacs@tjic.com>
;; Version: $Id: tjic-shell.el,v 1.11 2003/07/22 16:14:14 tjic Exp $
;; Keywords: shell, ring

;;; Commentary:

;; 0. TABLE OF CONTENTS
;; --------------------
;; 0. table of contents
;; 1. introduction
;; 2. setup
;; 3. use
;; 4. bugs
;;
;;
;; 1. INTRODUCTION
;; ---------------
;; This package enhances shell mode to allow a ring of shell buffers.
;;
;; The most up to date version of this package can be found at:
;;         http://www.tjic.com/computers
;;
;;
;; 2. SETUP
;; --------
;;   insert into your .emacs
;       (require 'tjic-shell.el)
;;
;; 3. USE
;; ------
;;   use M-x tjic-shell-new to create a new shell
;;   use M-x tjic-shell-next to go to next shell
;;   use M-x tjic-shell-prev to go to prev shell
;;
;; 4. BUGS
;; -------
;;   It would be nice to have a rotate command that created
;;   one shell if none exist
;;
;;   Ring is of fixed size; buffers can fall out of ring and be lost
;;   forever.
;;
;;   Bug reports and feature requests are desired; please mail me at
;;   the top of this file.

;;----------
;; Dependencies:
;;
(require 'shell)
(require 'ring)

;;----------
;; Variables:
;;
(defvar tjic-shell-ring (make-ring 10) "a ring of all the shells that exist")
(defvar tjic-shell-ring-index 0 "current location in ring")
(defvar tjic-shell-number 0 "Name of next shell to create")

;;----------
;; Functions:
;;
(defun tjic-shell-new () 
  "Stolen from shell.el.
Removed the 'if' statement which allowed only 1 shell."

  (interactive)
  (let* ((prog (or explicit-shell-file-name
				   (getenv "ESHELL")
				   (getenv "SHELL")
				   "/bin/sh"))		     
		 (name (file-name-nondirectory prog))
		 (startfile (concat "~/.emacs_" name))
		 (xargs-name (intern-soft (concat "explicit-" name "-args")))
		 shell-buffer)
	(save-excursion
	  (set-buffer (apply 'make-comint (concat "shell-" (int-to-string tjic-shell-number)) ;; XXX note hack here
						 prog
						 (if (file-exists-p startfile) startfile)
						 (if (and xargs-name (boundp xargs-name))
							 (symbol-value xargs-name)
						   '("-i"))))
	  (setq shell-buffer (current-buffer))
	  (shell-mode))
	(setq tjic-shell-number (+ 1  tjic-shell-number))
	(ring-insert tjic-shell-ring shell-buffer)
	(switch-to-buffer shell-buffer)))

(defun tjic-shell-rotate (offset &optional create)
  ""
  (interactive)
  (if (eq (ring-length tjic-shell-ring) 0)
	  (if create
		  (tjic-shell-new)
		(error "no shells exist"))
	(progn
	  ;; increment the current location
	  (setq tjic-shell-ring-index
			(mod (+ offset  tjic-shell-ring-index)
				 (ring-length tjic-shell-ring)))

	  ;; now, let's find the next/prev shell
	  (let ((buf (ring-ref tjic-shell-ring tjic-shell-ring-index)))
		
		;; If this buffer isn't live, keep iterating
		;;   until we find one that is, or we run out of buffers.
		;; In either case, edit the ring as we go.
		(while (not (buffer-live-p buf))
			(ring-remove tjic-shell-ring  tjic-shell-ring-index)
			(if (eq (ring-length tjic-shell-ring) 0)
				(if create
					(tjic-shell-new)
				  (error "no shells exist")))
			(setq buf (ring-ref tjic-shell-ring tjic-shell-ring-index)))

		;; OK, we've found a live one.
		;; Switch to it.
	  (switch-to-buffer buf)))))

(defun tjic-shell-next () "" (interactive) (tjic-shell-rotate 1))
(defun tjic-shell-prev () "" (interactive) (tjic-shell-rotate -1))

(defun tjic-shell-delete-all ()
  "delete all shells"
  (interactive)

  (if (y-or-n-p "do you really want to kill *all* buffers? ")
	  (progn
		(let ((startup t))
	
		  (while (or (not (eq 0  tjic-shell-ring-index))
					 startup)
			(kill-buffer (ring-ref tjic-shell-ring tjic-shell-ring-index))
			(setq startup nil)
			(setq tjic-shell-ring-index
				  (mod (+ 1 tjic-shell-ring-index)
					   (ring-length tjic-shell-ring)))))

		(setq  tjic-shell-number 0))))




;;----------
;; Keys:
;;
(global-set-key [kp-divide]   'tjic-shell-new)
(global-set-key [kp-up]       'tjic-shell-prev)
(global-set-key [begin]       '(lambda () "" (interactive) (tjic-shell-rotate 1 t)))
(global-set-key [kp-down]     'tjic-shell-next)
(global-set-key [kp-multiply] 'rename-buffer)
(global-set-key [kp-subtract] 'kill-buffer)

;;; tjic-shell.el
;;
;; Copyright (C) 2003 Travis J.I. Corcoran
;;
;; This is free software
;;
;; Author: Travis J.I. Corcoran <tjic_emacs@tjic.com>
;; Version: $Id: tjic-shell.el,v 1.11 2003/07/22 16:14:14 tjic Exp $
;; Keywords: shell, ring

;;; Commentary:

;; 0. TABLE OF CONTENTS
;; --------------------
;; 0. table of contents
;; 1. introduction
;; 2. setup
;; 3. use
;; 4. bugs
;;
;;
;; 1. INTRODUCTION
;; ---------------
;; This package enhances shell mode to allow a ring of shell buffers.
;;
;; The most up to date version of this package can be found at:
;;         http://www.tjic.com/computers
;;
;;
;; 2. SETUP
;; --------
;;   insert into your .emacs
;       (require 'tjic-shell.el)
;;
;; 3. USE
;; ------
;;   use M-x tjic-shell-new to create a new shell
;;   use M-x tjic-shell-next to go to next shell
;;   use M-x tjic-shell-prev to go to prev shell
;;
;; 4. BUGS
;; -------
;;   It would be nice to have a rotate command that created
;;   one shell if none exist
;;
;;   Ring is of fixed size; buffers can fall out of ring and be lost
;;   forever.
;;
;;   Bug reports and feature requests are desired; please mail me at
;;   the top of this file.

;;----------
;; Dependencies:
;;
(require 'shell)
(require 'ring)

;;----------
;; Variables:
;;
(defvar tjic-shell-ring (make-ring 10) "a ring of all the shells that exist")
(defvar tjic-shell-ring-index 0 "current location in ring")
(defvar tjic-shell-number 0 "Name of next shell to create")

;;----------
;; Functions:
;;
(defun tjic-shell-new () 
  "Stolen from shell.el.
Removed the 'if' statement which allowed only 1 shell."

  (interactive)
  (let* ((prog (or explicit-shell-file-name
				   (getenv "ESHELL")
				   (getenv "SHELL")
				   "/bin/sh"))		     
		 (name (file-name-nondirectory prog))
		 (startfile (concat "~/.emacs_" name))
		 (xargs-name (intern-soft (concat "explicit-" name "-args")))
		 shell-buffer)
	(save-excursion
	  (set-buffer (apply 'make-comint (concat "shell-" (int-to-string tjic-shell-number)) ;; XXX note hack here
						 prog
						 (if (file-exists-p startfile) startfile)
						 (if (and xargs-name (boundp xargs-name))
							 (symbol-value xargs-name)
						   '("-i"))))
	  (setq shell-buffer (current-buffer))
	  (shell-mode))
	(setq tjic-shell-number (+ 1  tjic-shell-number))
	(ring-insert tjic-shell-ring shell-buffer)
	(switch-to-buffer shell-buffer)))

(defun tjic-shell-rotate (offset &optional create)
  ""
  (interactive)
  (if (eq (ring-length tjic-shell-ring) 0)
	  (if create
		  (tjic-shell-new)
		(error "no shells exist"))
	(progn
	  ;; increment the current location
	  (setq tjic-shell-ring-index
			(mod (+ offset  tjic-shell-ring-index)
				 (ring-length tjic-shell-ring)))

	  ;; now, let's find the next/prev shell
	  (let ((buf (ring-ref tjic-shell-ring tjic-shell-ring-index)))
		
		;; If this buffer isn't live, keep iterating
		;;   until we find one that is, or we run out of buffers.
		;; In either case, edit the ring as we go.
		(while (not (buffer-live-p buf))
			(ring-remove tjic-shell-ring  tjic-shell-ring-index)
			(if (eq (ring-length tjic-shell-ring) 0)
				(if create
					(tjic-shell-new)
				  (error "no shells exist")))
			(setq buf (ring-ref tjic-shell-ring tjic-shell-ring-index)))

		;; OK, we've found a live one.
		;; Switch to it.
	  (switch-to-buffer buf)))))

(defun tjic-shell-next () "" (interactive) (tjic-shell-rotate 1))
(defun tjic-shell-prev () "" (interactive) (tjic-shell-rotate -1))

(defun tjic-shell-delete-all ()
  "delete all shells"
  (interactive)

  (if (y-or-n-p "do you really want to kill *all* buffers? ")
	  (progn
		(let ((startup t))
	
		  (while (or (not (eq 0  tjic-shell-ring-index))
					 startup)
			(kill-buffer (ring-ref tjic-shell-ring tjic-shell-ring-index))
			(setq startup nil)
			(setq tjic-shell-ring-index
				  (mod (+ 1 tjic-shell-ring-index)
					   (ring-length tjic-shell-ring)))))

		(setq  tjic-shell-number 0))))




;;----------
;; Keys:
;;
(global-set-key [kp-divide]   'tjic-shell-new)
(global-set-key [kp-up]       'tjic-shell-prev)
(global-set-key [begin]       '(lambda () "" (interactive) (tjic-shell-rotate 1 t)))
(global-set-key [kp-down]     'tjic-shell-next)
(global-set-key [kp-multiply] 'rename-buffer)
(global-set-key [kp-subtract] 'kill-buffer)

(provide 'tjic-shell)
;;; tjic-shell.el ends here
