;;;
;; screen-term.el
;; Basic GNU Screen integration in Term
;; Author: Denis Martinez

;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What The Fuck You Want
;; To Public License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

(defgroup screen-term nil
  "GNU screen integration."
  :group 'processes)

(defcustom screen-buffer-name "*Screen*"
  "Name of the term buffer with the screen sessions."
  :type '(string)
  :group 'screen-term)

(defcustom screen-escape-command "\^a"
  "The escape key for running screen commands."
  :type '(string)
  :group 'screen-term)

(defun screen-term ()
 (interactive)
 (let ((buf (get-buffer screen-buffer-name)))
   (if (equal buf nil)
       (progn (term "screen")
	      (rename-buffer screen-buffer-name))
     (progn (set-buffer buf)
	    (screen-term-create)
	    (switch-to-buffer buf)))))

(defun screen-term-copy ()
  (interactive)
  (term-send-raw-string (concat screen-escape-command "\033")))

(defun screen-term-create ()
  (interactive)
  (term-send-raw-string (concat screen-escape-command "c")))

(defun screen-term-next ()
  (interactive)
  (term-send-raw-string (concat screen-escape-command "n")))

(defun screen-term-prev ()
  (interactive)
  (term-send-raw-string (concat screen-escape-command "p")))

(defun screen-term-change-title ()
  (interactive)
  (term-send-raw-string (concat screen-escape-command "a")))

(defun screen-term-quit ()
  (interactive)
  (term-send-raw-string (concat screen-escape-command "\134")))

(provide 'screen-term)
