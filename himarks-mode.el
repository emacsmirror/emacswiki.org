;;; himarks-mode.el --- Highlight marks in buffer.

;;{{{ Copyright notice

;; Copyright (C) 2003 by Free Software Foundation, Inc.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Maintainer: none, mail me if you want to maintain this code.
;; Created: Wed Nov 19 16:42:08 MSK 2003
;; Keywords: tools
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

;;}}}

;;{{{ Commentary

;;; Commentary:
;;
;; Just highlights markers.
;;
;; Use M-x himarks-mode RET to toggle `himarks-mode'.
;;
;; You may also want to use enhanced version of `set-mark-command',
;; which will understand prefix argument as numeric value.
;;
;;     (define-key global-mode-map (kbd "C-SPC") 'himarks-set-mark-command)
;;     (define-key global-mode-map (kbd "C-@") 'himarks-set-mark-command)
;;
;; And use C-2 C-SPC to jump to second mark.
;;
;; There also `himarks-set-mark-no-activate', which act as
;; `himarks-set-mark-command', but does not activates mark.  This
;; command is usefull for pushing marks to `mark-ring'.
;;
;;     (define-key global-mode-map (kbd "C-x SPC") 'himarks-set-mark-no-activate)
;;

;;}}}

;;{{{ TODO

;;; TODO:
;;
;; If you push mark at same position where is current marker, himarks
;; can't track this.
;;

;;}}}

;;; Code:


;;{{{ Customazible variables

(defgroup himarks nil
  "Group to customize marks highlighting."
  :prefix "himarks-"
  :group 'tools
  :link '(url-link ""))

(defcustom himarks-mode nil
  "Non-nil, if mode `himarks-mode' is enabled."
  :type 'boolean
  :set (lambda (sym val)
	 (himarks-mode (or val 0)))
  :require 'himarks-mode
  :initialize 'custom-initialize-default
  :group 'himarks)

(make-variable-buffer-local 'himarks-mode)

(defcustom himarks-modeline-string " HMark"
  "*Mode name to show in modeline."
  :type 'string
  :group 'himarks)

(defcustom himarks-extent-priority 1000
  "*Priority of himarks's extents."
  :type 'number
  :group 'himarks)

(defcustom himarks-max-marks 4
  "*Maximum number of marks to show."
  :type 'number
  :group 'himarks)

(defcustom himarks-mode-hook nil
  "*Run after the `himarks-mode' is switched on."
  :type 'hook
  :group 'himarks)

(defface himarks-mark-face
  `((t
     (:background "white")))
  "Face to highlight current marker."
  :group 'himarks)

;; These colors are easy to remeber, RGB -> 123
(defface himarks-ring1-face
  `((t
     (:background "tomato")))
  "Face to highlight first marker in `mark-ring'."
  :group 'himarks)

(defface himarks-ring2-face
  `((t
     (:background "SeaGreen2")))
  "Face to highlight second marker in `mark-ring'."
  :group 'himarks)

(defface himarks-ring3-face
  `((t
     (:background "DodgerBlue2")))
  "Face to highlight third marker in `mark-ring'."
  :group 'himarks)

(defface himarks-ring-face
  `((t
     (:background "#AAAAAA")))
  "Face to highlight other markers in `mark-ring'."
  :group 'himarks)

;;}}}

;;{{{ Internal variables

;;; Interval variables
(defvar himarks-last-marker nil "Last copy of `mark-marker'.")
(make-variable-buffer-local 'himarks-last-marker)

;;}}}

;;{{{ Functions

(defun himarks-roll-face (rface)
  "Return next face after RFACE."
  (cond ((eq rface 'himarks-ring1-face) 'himarks-ring2-face)
	((eq rface 'himarks-ring2-face) 'himarks-ring3-face)
	(t 'himarks-ring-face)))

(defun himarks-highlight-char (point face)
  "Highlight character at POINT with FACE."
  (when (and (numberp point)
	     (>= point (point-min))
	     (< point (point-max)))
    (let ((extent (make-extent point (1+ point))))
      (set-extent-face extent face)
      (when (numberp himarks-extent-priority)
	(set-extent-priority extent himarks-extent-priority))
      
      (set-extent-property extent 'himarks-internal-extent t)
      (set-extent-property extent 'start-open t)
      (set-extent-property extent 'end-open t))))

(defun himarks-show-markers (buffer)
  "Show markes in BUFFER."
  (with-current-buffer buffer
    (setq himarks-last-marker (copy-marker (mark-marker t)))
    (himarks-highlight-char (marker-position himarks-last-marker) 'himarks-mark-face)

    (let ((cface 'himarks-ring1-face)
	  (ml mark-ring)
	  (mcnt himarks-max-marks))

      (while (and ml (> mcnt 0))
	(himarks-highlight-char (marker-position (car ml)) cface)

	;; update face
	(setq cface (himarks-roll-face cface))
	(setq mcnt (1- mcnt))
	(setq ml (cdr ml)))
      )))

(defun himarks-hide-markers (buffer)
  "Hide markers in BUFFER."
  (with-current-buffer buffer
    (map-extents '(lambda (extent dummy)
		    (delete-extent extent)
		    nil)
		 nil (point-min) (point-max) nil nil 'himarks-internal-extent t)
    ))

(defun himarks-mark-tracker ()
  "Update extents if marker position changed.
To be used in `post-command-hook'."

  (when (and himarks-last-marker
	     (not (= (marker-position himarks-last-marker)
		     (marker-position (mark-marker t)))))

    (himarks-hide-markers (current-buffer))
    (himarks-show-markers (current-buffer))
    ))

;;}}}

;;{{{ Commands: `himarks-mode'

;;;###autoload
(defun himarks-mode (arg)
  "Enable/disable `himarks-mode' mode.
Positive ARG mean enable, negative mean disable."
  (interactive "P")

  (setq himarks-mode
	(if (null arg)
	    (not himarks-mode)
	  (> (prefix-numeric-value arg) 0)))

  (if himarks-mode
      (progn
	(himarks-show-markers (current-buffer))
	(add-hook 'post-command-hook 'himarks-mark-tracker)
	(run-hooks 'himarks-mode-hook)
	(message "Himarks mode ON")
	)
    
    (himarks-hide-markers (current-buffer))
    (remove-hook 'post-command-hook 'himarks-mark-tracker)
    (message "Himarks mode OFF")
    ))

;;}}}

;;{{{ Additional commands.

(defun himarks-set-mark-command (arg &optional dreg)
  "Like `set-mark-command' but understand ARG as numeric value.
If ARG is non-nil than jump to ARG's mark.
if DREG is non-nil than do not activate mark."
  (interactive "P")

  (if (null arg)
      (push-mark nil nil (not dreg))

    (setq arg (cond ((consp arg) 1)
		    (t (1+ (prefix-numeric-value arg)))))
    (while (> arg 0)
      (when (mark t)
	(goto-char (mark t))
	(pop-mark))
      (setq arg (1- arg)))))

(defun himarks-set-mark-no-activate (arg)
  "As `himarks-set-mark-command', but do not activate mark.
If ARG is non-nil than jump to ARG's mark."
  (interactive "P")
  (himarks-set-mark-command arg t))

;;}}}


;;;###autoload
(if (fboundp 'add-minor-mode)
    (add-minor-mode 'himarks-mode himarks-modeline-string)

  ;; No `add-minor-mode', maybe try to insert it directly to
  ;; `minor-mode-alist'?
  )


(provide 'himarks-mode)

;;; himarks-mode.el ends here
