;;; auto-yasnippet.el --- Quickly create disposable yasnippets

;; Author: Oleh <ohwoeowho@gmail.com>

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Setup:
;; In your .emacs file:
;;   (require 'yasnippet)       
;;   (global-set-key (kbd "H-w") 'create-auto-yasnippet)
;;   (global-set-key (kbd "H-y") 'expand-auto-yasnippet)

;; Usage:
;; e.g. in Java write:
;; class Light$On implements Runnable {
;;   public Light$On() {}
;;   public void run() {
;;     System.out.println("Turning $on lights");
;;     light = $true;
;;   }
;; }
;; This differs from the code that you wanted to write only by 4 $ chars.
;; Since it's more than one line, select the region and call `create-auto-yasnippet'.
;; The $ chars disappear, yielding valid code.
;; `*current-auto-yasnippet-template* becomes:
;; "class Light$1 implements Runnable {
;;   public Light$1() {}
;;   public void run() {
;;     System.out.println(\"Turning $2 lights\");
;;     light = $3;
;;   }
;; }"
;; 
;; Now by calling `expand-auto-snippet', you can quickly fill in:
;; class LightOff implements Runnable {
;;   public LightOff() {}
;;   public void run() {
;;     System.out.println("Turning off lights");
;;     light = false;
;;   }
;; }
(defun line-indent-position ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (point)))
(defun current-string-position ()
  (interactive)
  (let ((head (if mark-active (region-beginning) (line-indent-position)))
	(tail (if mark-active (region-end) (line-end-position))))
    (list (buffer-substring-no-properties head tail)
	  head
	  tail)))
(defun create-auto-yasnippet ()
  "Works on either the current line, or, if `mark-active', the current region.
Removes $ prefixes, writes the corresponding snippet to `*current-auto-yasnippet-template*',
with words prefixed by $ as fields, and mirrors properly set up."
  (interactive)
  (multiple-value-bind (s head tail) (current-string-position)
    (flet ((parse (in vars out)
		  (if in
		      (let ((p (string-match "\\$\\([a-z0-9-]+\\)" in)))
			(if p
			    (let* ((var (match-string 1 in))
				   (mult (assoc var vars))
				   (vars (if mult vars
					   (cons (cons var (+ 1 (cdar vars)))
						 vars))))
			      (parse (substring in (+ p (length var) 1))
				     vars
				     (concat out
					     (substring in 0 p)
					     "$"
					     (number-to-string (if mult
								   (cdr mult)
								 (cdar vars))))))
			  (concat out in)))
		    out)))
      (setq *current-auto-yasnippet-template* (parse s (list (cons "" 0)) ""))
      (delete-region head tail)
      (insert (replace-regexp-in-string "\\$" "" s)))))
(defun expand-auto-yasnippet ()
  "Inserts the last yasnippet created by `create-auto-yasnippet'"
  (interactive)
  (yas/expand-snippet *current-auto-yasnippet-template*))
(provide 'auto-yasnippet)
;;; auto-yasnippet.el ends here
