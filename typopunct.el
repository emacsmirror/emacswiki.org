;;; typopunct.el --- Automatic typographical punctuation marks

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Oliver Scholz <epameinondas@gmx.de>
;; Keywords: wp

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

;; This package provides the buffer local minor mode TypoPunct which
;; helps inserting typographical punctuation marks.  See the docstring
;; of the function `typopunct-mode'.

;;; Code:

(defgroup typopunct nil
  "Minor mode for typographical punctuation marks."
  :group 'local)

(defconst typopunct-en-dash 
  (decode-char 'ucs #x2013))

(defconst typopunct-em-dash
  (decode-char 'ucs #x2014))

(defun typopunct-insert-typographical-dashes ()
  "Insert a dashes, an en-dash or an em-dash."
  (interactive)
  (cond ((eq (char-before) ?-)
	 (delete-char -1)
	 (insert typopunct-en-dash))
	((eq (char-before) typopunct-en-dash)
	 (delete-char -1)
	 (insert typopunct-em-dash))
	(t (insert ?-))))

(defvar typopunct-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?-] 'typopunct-insert-typographical-dashes)
    (define-key map [?\"] 'typopunct-insert-quotation-mark)
    (define-key map [?\'] 'typopunct-insert-single-quotation-mark)
    map)
  "Keymap used by TypoPunct mode.")

(defcustom typopunct-language-alist
  `((deutsch ,(decode-char 'ucs #x201E)
	     ,(decode-char 'ucs #x201C)
	     ,(decode-char 'ucs #x201A)
	     ,(decode-char 'ucs #x2018))
    (deutsch-2 ,(decode-char 'ucs #xBB)
	       ,(decode-char 'ucs #xAB)
	       ,(decode-char 'ucs #x203A)
	       ,(decode-char 'ucs #x2039))
;;     (deutsch-latex "\"`" "\"'"
;; 		   ,(propertize "\\glq "
;; 				'display
;; 				(string
;; 				 (decode-char 'ucs #x201A)))
;; 		   ,(propertize "\\grq "
;; 				'display
;; 				(string
;; 				 (decode-char 'ucs #x2018))))
    (dutch ,(decode-char 'ucs #x201E)
	   ,(decode-char 'ucs #x201D)
	   ,(decode-char 'ucs #x201A)
	   ,(decode-char 'ucs #x2019))
    (dutch-2 ,(decode-char 'ucs #x201E)
	     ,(decode-char 'ucs #x201D)
	     ,(decode-char 'ucs #x2018)
	     ,(decode-char 'ucs #x2019))
    (francais ,(decode-char 'ucs #xAB) 
	      ,(decode-char 'ucs #xBB)
	      ,(decode-char 'ucs #x2039)
	      ,(decode-char 'ucs #x203A))
    (russian ,(decode-char 'ucs #xAB)
             ,(decode-char 'ucs #xBB)
             ,(decode-char 'ucs #x201E)
             ,(decode-char 'ucs #x201C))
    (english ,(decode-char 'ucs #x201C)
	     ,(decode-char 'ucs #x201D)
	     ,(decode-char 'ucs #x2018)
	     ,(decode-char 'ucs #x2019)))
  "Alist of languages and typographical quotation marks.
Each element of this alist is list of the form:

\(LANGUAGE OPENING-DOUBLE-QMARK CLOSING-DOUBLE-QMARK
           OPENING-SINGLE-QMARK CLOSING-SINGLE-QMARK)

LANGUAGE is a symbol, the other elements may be either characters or
strings."
  :group 'typopunct
  :type '(repeat (list symbol
		       (choice character string)
		       (choice character string)
		       (choice character string)
		       (choice character string))))

(defsubst typopunct-opening-quotation-mark (lang)
  (nth 1 (assq lang
	       typopunct-language-alist)))

(defsubst typopunct-closing-quotation-mark (lang)
  (nth 2 (assq lang
	       typopunct-language-alist)))

(defsubst typopunct-opening-single-quotation-mark (lang)
  (nth 3 (assq lang
	       typopunct-language-alist)))

(defsubst typopunct-closing-single-quotation-mark (lang)
  (nth 4 (assq lang
	       typopunct-language-alist)))

(defcustom typopunct-buffer-language 'deutsch
  "Default language of a buffer.
When inserting quotation marks, TypoPunct mode relies on the
typographical conventions used by that language.  See the variable
`typopunct-language-alist'.")
(make-variable-buffer-local 'typopunct-buffer-language)

(defvar typopunct-xml-max-search 1000
  "Maximum chars to search back for the opening of an XML tag.")

(defun typopunct-point-in-xml-tag-p ()
  "Return t if point is inside an XML tag, nil otherwise."
  (save-excursion
    (let ((pos (point))
	  (beg (search-backward "<" (min
				     (point-min)
				     (- (point) typopunct-xml-max-search))
				t)))
      (and beg
	   (not (search-forward ">" pos t))))))

(defcustom typopunct-mode-exeptions-alist
  '((sgml-mode . typopunct-point-in-xml-tag-p)
    (nxml-mode . typopunct-point-in-xml-tag-p)
    (html-mode . typopunct-point-in-xml-tag-p))
  "Alist for mode specific expections.

This alist specifies major mode specific expectional cases when the
function `typopunct-insert-quotation-mark' should *not* insert
typographical quotation marks.

Each element is a pair of a major mode (a symbol) and a predicate
function that should return non nil, when
`typopunct-insert-quotation-mark' should insert an ASCII `\"'."
  :group 'typopunct
  :type '(alist :key-type symbol :value-type function))


(defvar typopunct-opening-quote-syntax-list
  '(?\  ?\- ?\( )
  "Syntax classes for opening quotation marks.
If the character before point belongs to one of these syntax classes,
then the function `typopunct-insert-quotation-mark' inserts an opening
quotation mark.")

(defun typopunct-insert-quotation-mark (&optional single)
  "Insert typographical quotation marks depending on language.
The language assumed is either the value of the text property
`typopunct-language' or the value of the variable
`typopunct-buffer-language'."
  (interactive)
  (let ((lang (or (get-text-property (point) 'typopunct-language)
		  typopunct-buffer-language))
	(pfunc (cdr (assq major-mode typopunct-mode-exeptions-alist)))
	(qmark nil))
    (if (and lang
	     (not (and pfunc
		       (funcall pfunc))))
	(if (or (bobp)
		(memq (char-syntax (char-before))
		      typopunct-opening-quote-syntax-list))
	    ;; After whitespace etc.: Opening quotation mark.
	    (setq qmark (if single
			    (typopunct-opening-single-quotation-mark lang)
			  (typopunct-opening-quotation-mark lang)))
	  ;; Everywhere else: Closing quotation mark.
	  (setq qmark (if single
			  (typopunct-closing-single-quotation-mark lang)
			(typopunct-closing-quotation-mark lang)))))
      (insert (or qmark (if single ?\' ?\")))))

(defun typopunct-insert-single-quotation-mark ()
  (interactive)
  (typopunct-insert-quotation-mark t))

(define-minor-mode typopunct-mode
  "Toggle TypoPunct mode, a minor mode for automatic typographical punctuation.
With arg, turn TypoPunct mode on if arg is positive, off otherwise.

This buffer local minor mode binds the keys `-', `\"' and `\'' to
function that help typing typographical punctuation.  It provides the
following two features:

* Dashes 

Typing `-' when a dash is before point, changes this dash to an
en-dash.  Typing `-' with an en-dash before point, changes it to an
em-dash.  Thus type `-' two times to get an en-dash, three times to
get an em-dash.

* Quotation Marks

If TypoPunct mode is on, Emacs insertes opening or closing quotation
marks depending on context.  The actual characters inserted as
quotation marks depend on various national typographical conventions.
See the variable `typopunct-language-alist'.  Which typographical
convention is used depends on either the value of the text property
`typopunct-language' at point or on the value of the variable
`typopunct-buffer-language', in that order.  You may use the function
`typopunct-change-language' for conveniently changing the
typographical convention.

Note that you may always type `C-q C-\"', `C-q C-\' or `C-q C--' to
insert the default ASCII characters."
  nil
  " Typo"
  typopunct-map)


(defun typopunct-change-language (language &optional default)
  "Change language assumed by `typopunct-mode' to LANGUAGE.

This sets the buffer local variable `typopunct-buffer-language' to
LANGUAGE.  If called with a prefix, change the default value of
`typopunct-buffer-language'.

If `transient-mark-mode' is non-nil and if the region is active, then
add LANGUAGE as the value of the text property `typopunct-language' to
the text in the region instead of setting the variable."
  (interactive (list (completing-read "Switch to language: "
				      (mapcar (lambda (elt)
						(list (symbol-name (car elt))))
					      typopunct-language-alist))
		     current-prefix-arg))
  (when (stringp language)
    (setq language (intern language)))
  (if (and transient-mark-mode
	   mark-active)
      (put-text-property (region-beginning)
			 (region-end)
			 'typopunct-language
			 language)
    (if default
	(setq-default typopunct-buffer-language language)
      (setq typopunct-buffer-language language))))


(provide 'typopunct)

;; Local Variables:
;; sentence-end-double-space: t
;; End:

;;; typopunct.el ends here
