;;; msearch.el --- 

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

(defun msearch-event-handler (e)
  "Must be bound to a mouse event."
  (interactive "e")
  (let ((start (posn-point (event-start e)))
	(end (posn-point (event-end e))))
    (if (/= start end)
	(progn
	  (if (> start end)
	      (let ((tmp start)) (setq start end) (setq end tmp)))
	  (setq msearch-word (buffer-substring-no-properties start end))
	  (unless (string-equal msearch-old-word msearch-word)
	    (setq msearch-old-word msearch-word)
	    (msearch-cleanup)
	    (jit-lock-register 'msearch-lock-function)))))
  (if msearch-next-handler (apply msearch-next-handler (list e))
    (local-set-key (kbd "<drag-mouse-1>") 'mouse-set-region)
    (error "Invalid mouse-next-handler. Reset drag-mouse-1 to mouse-set-region.")))

(define-minor-mode msearch-mode
  "Mouse-drag high-lightes all corresponding matches within the current buffer."
  :lighter " msearch" 
  (if msearch-mode
      (progn
	(set (make-local-variable 'msearch-word) "")
	(set (make-local-variable 'msearch-old-word) "")
	(set (make-local-variable 'msearch-next-handler) (key-binding (kbd "<drag-mouse-1>")))
	(local-set-key (kbd "<drag-mouse-1>") 'msearch-event-handler)
	)
    (local-set-key (kbd "<drag-mouse-1>") msearch-next-handler)
    (msearch-cleanup)
    (kill-local-variable 'msearch-word)
    (kill-local-variable 'msearch-old-word)
    (kill-local-variable 'msearch-next-handler)
    ))

(define-key mode-line-mode-menu [msearch-mode]
  `(menu-item ,(purecopy "msearch") msearch-mode
	      :help ,(purecopy "MSearch mode: Mouse-drag high-lightes all corresponding matches within the current buffer.")
	      :button (:toggle . msearch-mode)))

(provide 'msearch)
;;; 10msearch.el ends here
