;;; el-markup.el --- Emacs Lisp to Markup

;; Copyright (C) 2007 Yoni Rabkin
;;
;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;     
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;     
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Code:

(defconst el-markup-empty-tags
  '(:area :base :br :col :hr :img :input :link :meta :param)
  "Tags which end with forward-slash-greater-then.")

(defconst el-markup-post-newline-tags
  '(:table :lj-cut :tr :p :td :div :li :ul :ol :html :head :body)
  "Tags which look better with a newline after them.")

(defun el-markup-node-p (sexp)
  "Return t if SEXP is a node."
  (and (listp (car sexp))
       (keywordp (caar sexp))
       (not (equal :out (caar sexp)))))

;; Emacs Lisp ain't got none of them purdy FORMAT loops...
(defun el-markup-print-attributes (att)
  "Format a list of attributes."
  (cond ((null att) "")
	(t (princ (format "%s=\"%s\"" (el-markup-prink-spc (car att)) (cadr att)))
	   (el-markup-print-attributes (cddr att)))))

;; cheating emacs' inability to print keywords with the `:'.
(defun el-markup-prink-open (k)
  (let ((s (format "%s" k)))
    (aset s 0 ?<)
    s))
(defun el-markup-prink-close (k)
  (let ((s (format "%s" k)))
    (aset s 0 ?/)
    s))
(defun el-markup-prink-spc (k)
  (let ((s (format "%s" k)))
    (aset s 0 ?\s)
    s))

(defun el-markup-int (sexp)
  "Return a string containing markup based on SEXP."
  (with-output-to-string
    (cond ((stringp sexp) 
	   (princ (format "\"%s\" " sexp))) ; string
	  ((and (symbolp sexp)
		(not (null sexp)))
	   (princ (format "%s " sexp)))
	  ((and sexp			; compound expr
		(listp sexp)
		(not (el-markup-node-p sexp)))
	   (princ (format "%s" (concat (el-markup-int (car sexp))
				       (el-markup-int (cdr sexp))))))
	  ((el-markup-node-p sexp)	; tag-expr aka node
	   (let ((tag (caar sexp))
		 (attributes (cdar sexp))
		 (payload (cdr sexp)))
	     (princ (format "%s" (el-markup-prink-open tag)))
	     (when attributes (el-markup-print-attributes attributes))
	     (if (member tag el-markup-empty-tags)
		 (princ (format " />
"))
	       (progn
		 (if (member tag el-markup-post-newline-tags) 
		     (princ (format ">
"))
		   (princ (format ">")))
		 (princ (format "%s" (el-markup-int payload)))
		 (if (member tag el-markup-post-newline-tags)
		     (princ (format "
<%s>
" (el-markup-prink-close tag)))
		   (princ (format "<%s>
" (el-markup-prink-close tag)))
		   ))))))))

(defun el-markup-convert ()
  "Convert the sexp in the current buffer to markup."
  (interactive)
  (let ((current-sexp (sexp-at-point)))
    (if current-sexp
	(progn
	  (switch-to-buffer (generate-new-buffer "*EL-MARKUP-OUTPUT*"))
	  (erase-buffer)
	  (insert (el-markup-int current-sexp))
	  (html-mode)
	  (indent-region (point-min) (point-max)))
      (error "no SEXP at point"))))

(defun el-markup-startup ()
  "Single entry point to `el-markup'."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*el-markup*"))
  (erase-buffer)
  (el-markup-mode)
  (message "C-c C-c to submit"))

(define-derived-mode el-markup-mode
  emacs-lisp-mode "Emacs-Lisp-Markup"
  "A major mode for converting sybolic expressions to markup.
          \\{el-markup-mode-map}")

(define-key el-markup-mode-map
  (kbd "C-c C-c") #'el-markup-convert)

(defun el-markup-insert-test ()
  (insert
   "(((:p)
  The HTML for most of this post was generated from SEXPs using
  ((:a :href \"http://www.emacswiki.org/cgi-bin/wiki/el-markup.el\")
   ((:code) el-markup.el)))

 ((:p) 
  It is still an undocumented, mostly unusable hack. But it
  gives me something to do at 1AM after my brain has already
  turned off.)

 ((:p)
  This text was not in double-quotes in the sexp.)

 ((:p)
  \"But this text was in double-quotes\")

 ((:p)
  This text's the cats pyjamas. Which shows we can print a single quote.))
"))

(provide 'el-markup)

;;; el-markup.el ends here.
