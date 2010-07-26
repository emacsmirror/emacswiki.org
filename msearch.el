;;; msearch.el
;; Copyright (C) 2010  Tobias.Naehring

;; Author: Tobias.Naehring <i@tn-home.de>
;; Keywords: Search for mouse selection.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; After activating the minor mode "msearch-mode" mouse-dragging over
;; some text highlightes all matches of this text in the current
;; buffer. Msearch-mode can be (de)activated by (un)checking msearch
;; in the minor-mode menu of the mode line.
;;
;; Installation:
;; Put msearch.el into your load-path and add the following line into
;; your emacs start-up file (e.g. "~/.emacs"):
;; (require 'msearch.el)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changes:
;;
;; 2010-06-22, 18:45, TN:
;;
;; Error: Infinite recursion of msearch-event-handler at re-activation
;; of msearch-mode.
;;
;; Fix: Reset event handler by (local-unset-key (kbd
;; "<drag-mouse-1>")) rather than by (local-set-key (kbd ...)
;; msearch-next-event-handler).
;;
;; 2010-06-23, 23:00, TN:
;;
;; Error: msearch-event-handler didn't call msearch-next-handler. Thus
;; mouse-set-region was not called.
;;
;; Fix: Return value 't of (msearch-next-handler-ok).
;;
;; Feature: User can remove all highlights by dragging a region of zero length.
;;
;; Implementation: Allow msearch-word of zero length.
;;
;; 2010-06-24, 22:00, TN + SCZ:
;;
;; local-set-key is not buffer-local but major-mode local. Thus msearch-event-handler has been activated in all buffers with the same major-mode but local variables were missing.
;;
;; Fix: Introduced new mouse event handler for <drag-mouse-1>.
;;
;; 2010-07-25, 22:00, TN:
;;
;; Added msearch-enslave-buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(defcustom msearch-face '(background-color . "yellow")
"Face for highlighting matchings of mouse-selected text. See also msearch-mode.":type 'custom-face
:group 'msearch)

(defun msearch-lock-function (b e)
  "Highlight all matches of mouse-selection within the visible region."
  (if (and (stringp msearch-word) (> (length msearch-word) 0))
      (save-excursion
	(if (not (string-equal msearch-word msearch-old-word))
	    (remove-overlays nil nil 'msearch 't))
	(goto-char b)
	(while (search-forward msearch-word e 'noError)
	  (unless (get-char-property (match-beginning 0) 'msearch)
	    (setq ov (make-overlay (match-beginning 0) (match-end 0)))
	    (overlay-put ov 'face msearch-face)
	    (overlay-put ov 'msearch 't))
	  ))))

(defun msearch-cleanup ()
  "Remove overlays of msearch and deactivate msearch-lock-function."
  (remove-overlays nil nil 'msearch 't)
  (jit-lock-unregister 'msearch-lock-function))

(defvar drag-mouse-1-handler-list (list (key-binding (kbd "<drag-mouse-1>")))
  "List of event handlers for <drag-mouse-1> events.
Don't set this directly. Use the function
register-drag-mouse-1-handler instead.")
(make-variable-buffer-local 'drag-mouse-1-handler-list)

(defun drag-mouse-1-handler (e)
  "Generic handler for <drag-mouse-1> events."
  (interactive "e")
  (let ((n drag-mouse-1-handler-list))
    (while n
      (apply (car n) (list e))
      (setq n (cdr n)))))

(global-set-key (kbd "<drag-mouse-1>") 'drag-mouse-1-handler)

(defun msearch-set-word (word)
    (setq msearch-word word)
    (unless (string-equal msearch-old-word msearch-word)
      (setq msearch-old-word msearch-word)
      (msearch-cleanup)
      (jit-lock-register 'msearch-lock-function)))

(defun msearch-event-handler (e)
  "Must be bound to a mouse event."
  (interactive "e")
  (message "Running event handler in %s" (buffer-name))
  (let ((start (posn-point (event-start e)))
	(end (posn-point (event-end e))))
    (if (> start end)
	(let ((tmp start)) (setq start end) (setq end tmp)))
    (let ((new-word (buffer-substring-no-properties start end))
	  (slaves msearch-slaves)
	  slaves-released
	  (curbuf (current-buffer)))
      (msearch-set-word new-word)
      (save-excursion
	(while slaves
	  (if (get-buffer (car slaves))
	      (progn
		(set-buffer (car slaves))
		(if msearch-mode
		    (msearch-set-word new-word))))
	  (setq slaves (cdr slaves))
	  )))))

(defun msearch-enslave-buffer (buf)
  "Let the current buffer be the master of buf.
Msearch-strings of the current buffer are also high-lighted in buf.
The slave buf is released when msearch of the master is switched off."
  (interactive "bSlave buffer:")
  (if (equal (buffer-name) buf)
      (error "Cannot enslave myself."))
  ; Make sure that the current buffer is in msearch mode:
  (if (null msearch-mode)
      (msearch-mode))
  ; Make sure that the slave is in msearch mode:
  (with-current-buffer buf
    (if (null msearch-mode)
	(msearch-mode)))
  (add-to-list 'msearch-slaves buf))

(define-minor-mode msearch-mode
  "Mouse-drag high-lightes all corresponding matches within the current buffer."
  :lighter " msearch"
  (if msearch-mode
      (progn
	(set (make-local-variable 'msearch-word) "")
	(set (make-local-variable 'msearch-old-word) "")
	(set (make-local-variable 'msearch-slaves) nil)
	(add-to-list 'drag-mouse-1-handler-list 'msearch-event-handler))
    (msearch-cleanup)
    (kill-local-variable 'msearch-word)
    (kill-local-variable 'msearch-old-word)
    (kill-local-variable 'msearch-slaves)
    (setq drag-mouse-1-handler-list
	  (delete 'msearch-event-handler drag-mouse-1-handler-list))
    ))

(define-key mode-line-mode-menu [msearch-mode]
  `(menu-item ,(purecopy "msearch") msearch-mode
	      :help ,(purecopy "MSearch mode: Mouse-drag high-lightes all corresponding matches within the current buffer.")
	      :button (:toggle . msearch-mode)))

(provide 'msearch)
;;; 10msearch.el ends here
