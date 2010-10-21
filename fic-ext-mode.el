;;; fic-ext-mode.el --- Show FIXME/TODO/BUG(...) in special face only in comments and strings
;;--------------------------------------------------------------------
;;
;; Copyright (C) 2010, Trey Jackson <bigfaceworm(at)gmail(dot)com>
;; Copyright (C) 2010, Ivan Korotkov <twee(at)tweedle-dee(dot)org>
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;; To use, save fic-ext-mode.el to a directory in your load-path.
;;
;; (require 'fic-ext-mode)
;; (add-hook 'c++-mode-hook 'turn-on-fic-ext-mode)
;; (add-hook 'emacs-lisp-mode-hook 'turn-on-fic-ext-mode)
;;
;; or
;;
;; M-x fic-ext-mode
;;
;; NOTE: If you manually turn on fic-ext-mode, you you might need to force re-fontification initially
;;   M-x font-lock-fontify-buffer

(defgroup fic-ext-mode nil
  "Highlight FIXME/TODO(...) in comments"
  :tag "FIC"
  :group 'tools
  :group 'font-lock
  :group 'faces)

(defcustom fic-highlighted-words '("FIXME" "TODO" "BUG")
  "Words to highlight"
  :group 'fic-ext-mode)

(defcustom fic-author-name-regexp "[a-zA-Z0-9_\\.]+"
  "Regexp describing FIXME/TODO author name"
  :group 'fic-ext-mode)

(defconst font-lock-fic-face 'font-lock-fic-face
  "Face to fontify FIXME/TODO words")

(defface font-lock-fic-face
  '((((class color))
     (:background "white" :foreground "red" :weight bold))
    (t (:weight bold)))
  "Face to fontify FIXME/TODO words"
  :group 'fic-ext-mode)

(defconst font-lock-fic-author-face 'font-lock-fic-author-face
  "Face to fontify author/assignee of FIXME/TODO")

(defface font-lock-fic-author-face
  '((((class color))
     (:background "white" :foreground "orangered" :underline t))
    (t (:underline t)))
  "Face to fontify author/assignee of FIXME/TODO"
  :group 'fic-ext-mode)

(defun fic-search-re ()
  "Regexp to search for"
  (let ((fic-words-re (regexp-opt fic-highlighted-words t)))
    (concat fic-words-re "\\(?:(\\(" fic-author-name-regexp "\\))\\)?")))

(defun fic-in-doc/comment-region (pos)
  (memq (get-char-property pos 'face)
	(list font-lock-doc-face font-lock-string-face font-lock-comment-face)))

(defun fic-search-for-keyword (limit)
  (let ((match-data-to-set nil)
	found)
    (save-match-data
      (while (and (null match-data-to-set)
		  (re-search-forward (fic-search-re) limit t))
	(if (and (fic-in-doc/comment-region (match-beginning 0))
		 (fic-in-doc/comment-region (match-end 0))) 
	    (setq match-data-to-set (match-data)))))
    (when match-data-to-set
      (set-match-data match-data-to-set)
      (goto-char (match-end 0))
      t)))

(defun fic-ext-mode-font-lock-keywords ()
  "Font Lock keywords for fic-ext-mode"
  `((fic-search-for-keyword 
    (1 ,font-lock-fic-face t)
    (2 ,font-lock-fic-author-face t t))))

;;;###autoload
(define-minor-mode fic-ext-mode 
  "Fic mode -- minor mode for highlighting FIXME/TODO in comments"
  :lighter " FIC" :group 'fic-ext-mode
  (let ((kwlist (fic-ext-mode-font-lock-keywords)))
    (if fic-ext-mode 
	(font-lock-add-keywords nil kwlist 'append)
      (font-lock-remove-keywords nil kwlist))))

;;;###autoload(add-hook 'c-mode-common-hook 'fic-ext-mode)
;;;###autoload(add-hook 'python-mode-hook 'fic-ext-mode)

(provide 'fic-ext-mode)
