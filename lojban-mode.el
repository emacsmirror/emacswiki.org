;;; lojban-mode.el --- lojban text mode w/ syntactical highlighting

;; Copyright (c) 2002, 2003 Michele Bini

;; Author: Michele Bini
;; Maintainer: Michele Bini <mibin@libero.it>
;; Created: 21 Nov 2002
;; Version: 0.7
;; Keywords: lojban, languages

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This file provides a specialized minor mode for lojban text.

;; It includes syntactical highlighting for lojban words.
;; The three basic word types (cmavo, cmene, and brivla), sentence
;; separators and attitudinal indicators are highlighted with
;; different faces.

;; There is specialized support for `outline-mode' with valid lojban
;; headers, though this is quite an experimental feature.

;; Where ordinary outlines would look like these:
;;    * Header 1
;;    ** Subheader 1
;;    ** Subheader 2
;;    ** Subheader 3

;; In lojban mode you can use the cmavo ni'o in a similar (though
;; reversed) way you could use asterisks.
;;    ni'oni'o pamoi tcita
;;    ni'o pamoi tcita
;;    ni'o remoi tcita
;;    ni'o cimoi tcita

;; This file requires lojban.el, a lojban utility function library you
;; should have received in the same way you received this file.

;; Typical user .emacs configuration:
;;     (setq load-path
;;       (cons "/path/to/lojban-mode.el/and/lojban.el"
;;         load-path))
;;     (autoload 'lojban-mode "lojban-mode" nil t)
;; Additional configuration is available via M-x customize lojban-mode

;;; History:
;; v0.7 - lojban-quoted-face enabled.
;; v0.6 - support for quoting bu letterals.
;; v0.5 - first release on the Emacs Wiki.

(require 'lojban)
(require 'font-lock)

;;; Code:
(defgroup lojban-mode nil
  "Lojban language mode."
  :group 'lojban
  :group 'font-lock
  :group 'faces
  :prefix 'lojban-mode-)

(defcustom lojban-mode-hook nil
  "Hook run when lojban mode is activated."
  :group 'lojban-mode :type 'hook)
(defcustom lojban-mode-line-string " jbo"
  "String displayed on the mode line when lojban mode is active."
  :group 'lojban-mode :type '(choice (const :tag "No indicator." nil)
				  string))
(or (assq 'lojban-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(lojban-mode lojban-mode-line-string)
		minor-mode-alist)))

;;;; outline

(defconst lojban-outline-regexp "\\<\\(ni'o\\)+\\>")

(defun lojban-outline-level ()
  (save-excursion
    (let ((level 0))
      (while (looking-at "\\W*ni'o\\W*")
	(setq level (+ level 1))
	(goto-char (match-end 0)))
      level)))

(defun lojban-outline-mode ()
  (interactive)
  (outline-mode)
  ;;(make-local-variable 'outline-heading-end-regexp)
  ;;(setq outline-heading-end-regexp "\\( +.i *\\| *.i +\\)")
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-level)
  (setq outline-regexp lojban-outline-regexp)
  (setq outline-level 'lojban-outline-level))

;;;; syntax table

(defvar lojban-syntax-table nil)
(unless lojban-syntax-table
  (let ((s (make-syntax-table)))
    (mapcar
     (lambda (char)
       (modify-syntax-entry char " " s))
     (string-to-list "_- \t\r\n\""))
    (mapcar
     (lambda (char)
       (modify-syntax-entry char "w" s))
     (append
      (list ?,)
      (string-to-list lojban-word-letters)))
    (setq lojban-syntax-table s)))

;;;; font-lock

(defconst lojban-cmavo-face 'lojban-cmavo-face)
(defconst lojban-brivla-face 'lojban-brivla-face)
(defconst lojban-cmene-face 'lojban-cmene-face)
(defconst lojban-sentence-separator-face 'lojban-sentence-separator-face)
(defconst lojban-UI-face 'lojban-UI-face)
(defconst lojban-quoted-face 'lojban-quoted-face)
(defface lojban-cmavo-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for highlighting cmavo."
  :group 'lojban-mode)
(defface lojban-brivla-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for highlighting brivla."
  :group 'lojban-mode)
(defface lojban-cmene-face
  '((t (:inherit font-lock-constant-face)))
  "Face for highlighting cmene."
  :group 'lojban-mode)
(defface lojban-sentence-separator-face
  '((t (:inherit font-lock-type-face)))
  "Face for cmavo separating sentences or paragraphs.

Currently recognized separators are \".i\", \"ni'o\", \"no'i\" and
\"fa'o\"."
  :group 'lojban-mode)
(defface lojban-UI-face
  '((t (:inherit font-lock-string-face)))
  "Face for attitudinal indicators."
  :group 'lojban-mode)

(defface lojban-quoted-face
  '((t (:inherit font-lock-string-face)))
  "Face for non-lojban quoted parts.

This includes BU letterals, ZEI concatenated text, ZO quotations and
the last word of text cancelled with SI, SA or SU."
  :group 'lojban-mode)

(defun lojban-match-zoi (&optional limit)
  (let ((case-fold-search t)
	(start (point)))
    (and
     (> limit start)
     (re-search-forward
      (eval-when-compile
	(regexp-opt
	 (list "zoi" "la'o" "lo'u")))
	limit t)
     (progn
       (goto-char (match-beginning 0))
       ;; TODO determine if it is possible to concatenate any cmavo
       ;; before "zoi" or "la'o" (apparently not)
       (cond
	((looking-at
	  (eval-when-compile
	    (concat "\\<\\(zoi\\|la'o\\)\\>\\W+\\(\\.?\\)\\("
		    lojban-middle-letter-rgx "+\\)")))
	 (let ((delimiter (match-string 3))
	       (pos (match-beginning 2)))
	   ;; should the following search be undelimited?
	   (goto-char (match-end 0))
	   (or
	    (and delimiter
		 (> (length delimiter) 0)
		 ;;(search-forward delimiter limit t)
		 (progn
		   (or (re-search-forward
			(concat "\\<\\.?"
				(regexp-quote delimiter)
				"\\.?\\>")
			limit t)
		       (goto-char limit))
		   (set-match-data
		    (list pos (point)
			  pos (+ pos 1)
			  (- (point) 1) (point)))
		   t))
	    (progn (goto-char start) nil))))
	((and (looking-at "lo'u")
	      (save-excursion
		(and
		 (or (looking-at "\\<")
		     (re-search-backward "\\<" nil t))
		 (looking-at lojban-compound-cmavo-rgx))))
	 (let ((ok nil)
	       (pos (point)))
	   (forward-char 4)
	   (while (and
		   (not ok) (search-forward "le'u" limit t)
		   ;; make sure le'u is a proper cmavo
		   (save-excursion
		     (and
		      (re-search-backward "\\<" nil t)
		      (looking-at
		       lojban-compound-cmavo-rgx))))
	     (unless
		 ;; make sure it is not quoted by a zo (reference
		 ;; grammar chapter 19 warns about this).
		 (save-excursion
		   (let ((limit (- (point) 4)))
		     (goto-char limit)
		     (and
		      (re-search-backward "\\<" nil t)
		      (re-search-forward "zo\\>" limit t)
		      (= (match-end 0) limit))))
	       (setq ok (point))))
	   (if (not ok) (progn (goto-char start) nil)
	     (set-match-data (list pos ok pos (+ pos 1) (- ok 1) ok))
	     t))))))))

;; (and (re-search-forward "\\(a\\)b\\(c\\)") (match-data t)) abc

(defun lojban-match-quoted (&optional limit)
  (let ((case-fold-search t)
	(start (point)))
    (and
     (> limit start)
     (re-search-forward
      (eval-when-compile
	(regexp-opt
	 (list "su" "si" "sa" "bu" "zo" "zei")))
      limit t)
     (progn
       (goto-char (match-beginning 0))
       (cond
	((and (looking-at "zo\\>")
	      (save-excursion
		(or (looking-at "\\<")
		    (and
		     (re-search-backward "\\<" nil t)
		     (looking-at lojban-compound-cmavo-rgx)))))
	 ;; This time only the quoted word is marked not also the
	 ;; quoting one as the quoting cmavo may appear at the end of
	 ;; a compound cmavo (hum, this could be rephrased).
	 (forward-char 2)
	 (re-search-forward
	  "[^ \t\r\n]+" ;;"\\w+"
	  limit t))
	((looking-at "\\<zei\\>")
	 (or
	  (and
	   (re-search-backward "\\<" nil t)
	   (looking-at "\\w+\\(\\W+zei\\W+\\w+\\)+")
	   (progn (goto-char (match-end 0)) t))
	  (progn (goto-char start) nil)))
	((looking-at "\\<s[aiu]\\>")
	 ;; For simplicity we only mark the preceding word
	 (let ((end (match-end 0)))
	   (or
	    (if (re-search-backward "\\<\\w+\\>" nil t)
		(progn (goto-char end) t)
	      (goto-char start) nil))))
	;; FIXME: the reference grammar says "bu" concatenation is
	;; possible, but does not specify its meaning
	((looking-at (eval-when-compile
		       (concat "bu\\(\\>\\|" lojban-cmavo-rgx "\\)")))
	 (let ((end (match-beginning 1)))
	   (if (looking-at "\\<")
	       ;; quote the previous word
	       (or
		(and
		 (re-search-backward "\\<" nil t)
		 (looking-at "\\w+")
		 (progn (goto-char (max start end)) t))
		(progn (goto-char start) nil))
	     (progn (goto-char start) nil)
	     ;; quote the previous partial cmavo
	     ;;(or
	     ;; (and
	     ;;  (re-search-backward lojban-cmavo-rgx nil t)
	     ;;  ;;(looking-at "\\w+bu")
	     ;;  (progn (goto-char (max start end)) t))
	     ;; (progn (goto-char start) nil))
	     )))
	(t (goto-char start) nil))))))

(defconst lojban-font-lock-keywords
  (reverse
   (list
   (list
    (concat "\\<" lojban-compound-cmavo-rgx
	    ;; "\\>" already included in compound-cmavo-rgx
	    )
     0 lojban-cmavo-face
;;     'prepend
     )
   (list
    (concat "\\<"
	    lojban-UI-rgx
	    lojban-cmavo-rgx "*"
	    "\\>")
    0 lojban-UI-face
;;    'prepend
    )
   (list
    (concat lojban-sentence-separator-rgx  lojban-cmavo-rgx "*\\>")
    0 lojban-sentence-separator-face
;;    'prepend
    )
   (list
    lojban-cmene-rgx 0 lojban-cmene-face
;;    'prepend
    )
   (list
    lojban-brivla-rgx 0 lojban-brivla-face
;;    'prepend
    )
   ;;(list "\\<zo\\W+\\w+\\>" 0 lojban-quoted-face)
   
   (list 'lojban-match-quoted 0 lojban-quoted-face)
   )))

;; syntactic fontification of zoi, la'o and lo'u...le'u region is still
;; too problematic
;;(defconst lojban-font-lock-syntactic-keywords
;;  (list (list 'lojban-match-quoted 0 "_")))
;;(defconst lojban-font-lock-syntactic-keywords
;;  (list (list 'lojban-match-zoi
;;	      (list 1 "\"")
;;	      (list 2 "\""))))
;;(defconst lojban-font-lock-syntactic-keywords
;;  (list
;;   (list "zoi \\<\\.?\\("
;;	 (list 1 "\"")
;;	 (list 2 "\""))
;;   (list "\\<\\(l\\)o'u\\>.+\\<le'\\(u\\)\\>"
;;	 (list 1 "\"")
;;	 (list 2 "\""))))

(defvar lojban-mode-font-lock-keywords nil)

;;;; backup external variables

;; there may be occasional problems if other minor modes modify  these
;; variables

(defvar lojban-mode-variable-overrides nil)
(make-variable-buffer-local 'lojban-mode-variable-overrides)

(defun lojban-mode-override-variable (variable value)
  (let* ((bound (boundp variable))
	 (value (and bound (symbol-value variable)))
	 (local (and bound (local-variable-p variable))))
    (setq lojban-mode-variable-overrides
	  (cons
	   (cons variable (cons value (if (not bound) 'unbound local)))
	   (assq-delete-all
	    variable lojban-mode-variable-overrides))))
  (make-local-variable variable)
  (set variable value))

(defun lojban-mode-restore-variables ()
  (mapcar
   (lambda (entry)
     (let ((variable (car entry))
	   (value (cadr entry))
	   (local (cddr entry)))
       (when (boundp variable)
	 (cond
	  ((eq local 'unbound) (unintern variable))
	  (local (set variable value))
	  (t (kill-local-variable variable))))))
   lojban-mode-variable-overrides)
  (setq lojban-mode-variable-overrides nil))
  
(defvar lojban-mode-old-syntax-table nil)
(make-variable-buffer-local 'lojban-mode-old-syntax-table)
(defvar lojban-mode-old-font-lock nil)
(make-variable-buffer-local 'lojban-mode-old-font-lock)

(defvar lojban-mode nil)
(make-variable-buffer-local 'lojban-mode)

;;;###autoload
(defun lojban-mode (&optional arg)
  "Lojban language mode.
It includes syntactical highlighting for lojban words, via the
font-lock mode.  The three basic word types (cmavo, cmene, and brivla)
and are highlighted with different faces, respectively
`lojban-cmavo-face', `lojban-cmene-face', `lojban-brivla-face'.  In
addition some common attitudinal indicators are highlighted with
`lojban-UI-face', otherwise they appear like ordinary cmavo.
`lojban-quoted-face' is the face used to mark quoted words, though not
all the types of quotations allowable by lojban are recognized.

This is currently a minor mode.  This means that it can coexist with
other major or minor modes.  With prefix ARG, turn the mode on iff ARG
is positive.  When the mode is activated, `lojban-mode-hook' is run."
  (interactive "P")
  (let ((enable nil)
	(disable nil))
    (cond
     ((null arg)
      (if lojban-mode (setq disable t) (setq enable t)))
     ((> (setq arg (prefix-numeric-value arg)) 0)
      (setq enable t))
     (t (setq disable t)))
    (if disable
	(progn
	  (when lojban-mode-font-lock-keywords
	    (font-lock-remove-keywords
	     nil lojban-mode-font-lock-keywords)
	    (setq lojban-mode-font-lock-keywords nil)
	    (if font-lock-mode
		(if lojban-mode-old-font-lock
		    (font-lock-fontify-buffer)
		  (font-lock-mode -1))))
	  (when lojban-mode-old-syntax-table
	    (set-syntax-table lojban-mode-old-syntax-table))
	  (lojban-mode-restore-variables)
	  (setq lojban-mode nil))
      (if enable
	  (progn
	    (let ((a lojban-font-lock-keywords))
	      (mapcar 'make-local-variable
		      '(lojban-mode-font-lock-keywords))
	      (lojban-mode-override-variable
	       'sentence-end lojban-sentence-separator-rgx)
	      (lojban-mode-override-variable
	       'outline-level 'lojban-outline-level)
	      (lojban-mode-override-variable
	       'outline-regexp lojban-outline-regexp)
	      (lojban-mode-override-variable
	       'font-lock-keywords-case-fold-search t)
	      (lojban-mode-override-variable
	       'font-lock-multiline t)
	      (lojban-mode-override-variable
	       'font-lock-beginning-of-syntax-function
	       'beginning-of-buffer)
;;	      (lojban-mode-override-variable
;;	       'font-lock-syntactic-keywords
;;	       lojban-font-lock-syntactic-keywords)
	      (setq
	       ;; remember whether font-lock is active
	       lojban-mode-old-font-lock font-lock-mode
	       lojban-mode-font-lock-keywords
	       (append a
		       lojban-mode-font-lock-keywords)
	       lojban-mode-old-syntax-table (syntax-table))
	      (set-syntax-table lojban-syntax-table)
	      (font-lock-add-keywords nil a)
	      ;; (if t (font-lock-fontify-syntactically-region (point-min)
	      (if font-lock-mode
		  (font-lock-fontify-buffer)
		(font-lock-mode 1))
	      (setq lojban-mode t))
	    (run-hooks 'lojban-mode-hook))))))

(provide 'lojban-mode)

;;; lojban-mode.el ends here
