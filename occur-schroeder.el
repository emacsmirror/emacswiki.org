;;; occur-schroeder.el --- Invoke occur from within isearch
;; 
;; Filename: occur-schroeder.el
;; Description: Invoke occur from within isearch
;; Author: Alex Schroeder < a l e x @ e m a  c s w i k i . o r g >
;; Maintainer: Drew Adams
;; Copyright (C) 2004, Alex Schroeder
;; Created: Thu Apr 08 16:29:45 2004
;; Version: 
;; Last-Updated: Tue May 27 10:11:36 2008 ()
;;           By: dradams
;;     Update #: 24
;; URL: http://www.emacswiki.org/cgi-bin/wiki/occur-schroeder.el
;; Keywords: search, occur
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; From Alex Schroeder's code at
;; http://www.emacswiki.org/cgi-bin/wiki/AlexSchroederConfigInit
;; http://www.emacswiki.org/cgi-bin/wiki.pl/AlexSchroeder
;;
;; See also: `color-moccur.el', at
;; http://www.emacswiki.org/cgi-bin/wiki/color-moccur.el
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 2004/04/08 dadams
;;     Created from Alex Schroeder's code cited above.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defun isearch-occur ()
  "Invoke `occur' from within Isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(defun occurs (&optional arg)
  "Switch to *Occur* buffer, or run `occur'.
Without a prefix argument, switch to the buffer.
With a universal prefix argument, run occur again.
With a numeric prefix argument, run occur with NLINES
set to that number."
  (interactive "P")
  (if (and (not arg) (get-buffer "*Occur*"))
      (switch-to-buffer "*Occur*")
    (occur (read-from-minibuffer "Regexp: ")
	   (if (listp arg) 0 arg))))

;;;;;;;;;;;;;;

(provide 'occur-schroeder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; occur-schroeder.el ends here
