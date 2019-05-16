;;; pinbar.el --- Display a pin bar in the header line

;; Copyright (C) 2007 Poppyer Huang

;; Author: Poppyer Huang <poppyer@gmail.com>
;; Created: 18 May 2007
;; Keywords: convenience
;; Revision: $Id: pinbar.el,v 0.1 2007/05/18 $

(defconst pinbar-version "0.11")

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This library provides a minor mode to display tabs in the header
;; line.  It works on GNU Emacs 21 or later version.
;;
;; M-x `pinbar-mode' toggle the display of the pin bar, globally.
;;
;; Installation:
;;		put the following in .emacs file
;;;;;;;;;;;;;;;
;;  (require 'pinbar)
;;  (global-set-key "\M-0" 'pinbar-add)
;;  (pinbar-mode t)
;;;;;;;;;;;;;;;
;;	now you can use ALT-0 to pin a buffer to the pin, and ALT-- ALT-0 to unpin it



(defvar pinbar-array (make-vector 9 nil) "array of tabs in pinbar")

;;; Faces
;;
(defface pinbar-default-face
  '(
    (t
     (
	  ;; :inherit variable-pitch
               :height 1.0
;               :foreground "lightgreen"
;               :background "black"
					   :foreground "black"
					   :background "lightgrey"
               )
     )
    )
  "Default face used in the tab bar."
  :group 'pinbar)

(defface pinbar-selected-face
  '(
    (t
     (:inherit pinbar-default-face
               :box (:line-width -1 :color "white" :style pressed-button)
               :foreground "blue"
;			   :weight bold
               )
     )
    )
  "Face used for the selected tab."
  :group 'pinbar)

(defface pinbar-unselected-face
  '(
    (t
     (:inherit pinbar-default-face
               ;; :box (:line-width -1 :color "white" :style released-button)
			   :foreground "black"
               )
     )
    )
  "Face used for uselected tabs."
  :group 'pinbar)

(defface pinbar-separator-face
  '(
    (t
     (:inherit pinbar-default-face
               :height 0.2
               )
     )
    )
  "Face for space width"
  :group 'pinbar)

(defface pinbar-button-face
  '(
    (t
     (:inherit pinbar-default-face
               ;; :box (:line-width -1 :color "white" :style released-button)
               :foreground "blue"
               )
     )
    )
  "Face used for the select mode button."
  :group 'pinbar)


(defun pinbar-del()
  (interactive)
  (dotimes (i (length pinbar-array))
	(let ((curbuf (aref pinbar-array i)))
	  (when (eq curbuf (current-buffer))
		(setq pinbar-tab-map nil)
	    (aset pinbar-array i nil)
	    )
	  )
	)
  )

(defvar pinbar-tab-map nil)
(make-variable-buffer-local 'pinbar-tab-map)

(defun pinbar-tab-make-keymap ()
  (unless pinbar-tab-map
    (setq pinbar-tab-map (make-sparse-keymap))
    (let* ((cur (current-buffer))
		   (f (lambda (fn) `(lambda (e) 
							   (interactive "e") 
							   (select-window (posn-window (event-start e)))
							   (switch-to-buffer ,cur)
							   ;; (,fn e ,cur)
							   ))))
	  (define-key pinbar-tab-map [header-line mouse-1] (funcall f nil))
	  )
	)
  )

(defun pinbar-add (&optional arg) 
  (interactive "P")

  (unless arg
    (setq arg 0)
    (while (and (< arg (length pinbar-array))
		(aref pinbar-array arg))
      (setq arg (+ arg 1))
      )
    (setq arg (+ arg 1))
    )


  (if(numberp arg)
	  (if (and (<= arg (length pinbar-array))
			   (> arg 0))
		  (progn
			(pinbar-tab-make-keymap)
			(aset pinbar-array (- arg 1) (current-buffer))
			;; (lexical-let ((buffer (current-buffer)) 
			;; 			  (key (number-to-string arg))
			;; 			  ) ;; lexical-let magic
			;;   (define-key esc-map key (lambda() ;; no quote here! why?
			;; 							(interactive) 
			;; 							(switch-to-buffer buffer)
			;; 							)
			;; 	)
			;;   )
			(let* ((buffer (current-buffer)) 
				   (key (number-to-string arg))
				   (f (lambda () `(lambda () 
									  (interactive) 
									  (switch-to-buffer ,buffer)
									  )))
				   )
			    (define-key esc-map key (funcall f))
			  )
			)
		(message "Tabs full, use pinbar-del to delete some tabs")
		)
	;; M--, negative, so call pinbar-del
	(pinbar-del)
	)
  )


(defun pinbar-line()
;;  (setq line (number-to-string pinbar-tabs))
  (setq pline "")
  (let ((padcolor (face-background 'pinbar-default-face)))
    (dotimes (i (length pinbar-array))
      (let ((curbuf (aref pinbar-array i)))
	(if curbuf
	    (setq pline (concat pline
				(propertize " " 'face 'pinbar-separator-face)
			       (if (eq curbuf (current-buffer))
				   (propertize
				    (concat "<" 
					    (number-to-string (+ i 1)) 
					    ">" 
					    (buffer-name curbuf) 
					    )
				    'face 'pinbar-selected-face
				    )
				 (concat 
				  (propertize
				   (concat
				    "[" 
				    (number-to-string (+ i 1)) 
				    "]"
				    )
				   'face 'pinbar-button-face
					'mouse-face 'pinbar-selected-face
				   )
				  (propertize
				    (buffer-name curbuf)
				   'face 'pinbar-unselected-face
					'mouse-face 'pinbar-selected-face
					'local-map (buffer-local-value 'pinbar-tab-map curbuf)
				   )
				  )
				 )
			       )
		  )
	  )
	)
      )
    (concat pline
	    (propertize "%-" 'face (list :background padcolor
					 :foreground padcolor))
	    )
    )
  ;;  plinen
  )
  

(defun pinbar-buffer-kill-buffer-hook ()
  (if pinbar-mode
      (pinbar-del)
      )
  )



;;; Minor modes
;;
(defvar pinbar-old-global-hlf nil
  "Global value of the header line when entering pin bar mode.")

(defconst pinbar-header-line-format '(:eval (pinbar-line))
  "The pin bar header line format.")

;;;###autoload
(define-minor-mode pinbar-mode
  "Toggle display of a pin bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled."
  :global t
  :group 'pinbar
  (if pinbar-mode
;;; ON
      (unless (eq header-line-format pinbar-header-line-format)
        ;; Save current default value of `header-line-format'.
        (setq pinbar-old-global-hlf (default-value 'header-line-format))
        (add-hook 'kill-buffer-hook 'pinbar-buffer-kill-buffer-hook)
;;        (tabbar-init-tabsets-store)
        (setq-default header-line-format pinbar-header-line-format))
;;; OFF
    ;; Restore previous `header-line-format', if it has not changed.
    (when (eq (default-value 'header-line-format)
              pinbar-header-line-format)
      (setq-default header-line-format pinbar-old-global-hlf))
    (remove-hook 'kill-buffer-hook 'pinbar-buffer-kill-buffer-hook)
;;    (tabbar-free-tabsets-store)
    ;; Turn off locals tab bar mode
;;     (mapc #'(lambda (b)
;;               (with-current-buffer b
;;                 (tabbar-local-mode -1)))
;;           (buffer-list))
    ))

(provide 'pinbar)

