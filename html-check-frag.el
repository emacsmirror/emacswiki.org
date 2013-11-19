;;; html-check-frag.el --- Check html-fragments

;; Copyright (C) 2013  Tobias.Zawada

;; Author: Tobias.Zawada <i@tn-home.de>

;; This file is not part of GNU Emacs.

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

;; Compatibility: GNU Emacs 24.3

;; Keywords: html

;;; Commentary:

;; Mismatches of html tags are highlighted with the face html-check-frag-error-face.
;; You can go to the next mismatch with html-check-frag-next.
;;
;; Installation:
;; Put html-check-frag.el into your load-path and add the following line into
;; your emacs start-up file (e.g. "~/.emacs"):
;;
;; (require 'html-check-frag)
;;
;; If you want to start html-check-frag-mode together with html-mode then also add:
;;
;; (add-hook 'html-mode-hook (lambda () (html-check-frag-mode 1)))
;;
;;; Code:

(defface html-check-frag-error-face
  '((default (:foreground "red")))
  "Overlay properties for errorneous html tags."
  :group 'html-check-frag)

(defvar html-check-frag-debug nil
  "Debug stack for `html-check-frag-region'.
Set it to (t) to initialize debugging and set it to nil to stop.
Debug information is gathered in stack-like manner.
Note that the stack contents has reverse order.
You should look at (reverse html-check-frag-debug).")
(make-variable-buffer-local 'html-check-frag-debug)

(defvar html-check-frag-void-tags '("?xml" "!doctype" "area" "base" "br" "col" "embed" "hr" "img" "input" "keygen" "link" "meta" "param" "source" "track" "wbr")
  "Void tags not needed to be marked as <.../>.
Note, everything should be lower case here. Even !DOCTYPE should actually be !doctpype in this list.")

(defun html-invalid-context-p (&optional pos)
  "Return non-nil if point is inside string comment or the character at point is quoted."
  (save-excursion
    (save-match-data
      (let ((parserState (syntax-ppss (if (= (char-after pos) ?<) (1+ pos) pos))))
	(or (nth 3 parserState) ;; inside string
	    (nth 4 parserState) ;; inside comment
	    (nth 5 parserState))) ;; ?< is quoted
      )))

(defun html-search-for-tag (&optional bound noerror backward)
  "Starting at point search for tag.
If point is in the middle of a tag try to find the beginning of
the tag (it throws `error-html-tag' on failure).  Afterwards
point is at the end of tag (i.e., at >).  Comments, quoted
characters and strings are ignored. BOUND and NOERROR have
the (almost) the same meaning as for
`search-forward'. BACKWARD determines the search direction."
  (interactive)
  (declare (special html-search-for-tag-syntax))
  (when html-check-frag-debug
    (push (list 'search :args (list bound noerror backward) :point (point)) html-check-frag-debug))
  (let (type
	beg
	end
	opens ;;
	closes ;;
	parserState
	attrList
	name
	void
	found ;; temporary
	value ;; temporary
	(re "\\(?:<\\(/\\)?\\([[:alpha:]!?][[:alnum:]]*\\)\\|\\(>\\)\\)")
	)
    (with-syntax-table (or (and (boundp 'html-search-for-tag-syntax) (syntax-table-p html-search-for-tag-syntax) html-search-for-tag-syntax)
			   (prog1 
			       (setq-local html-search-for-tag-syntax (copy-syntax-table))
			     (modify-syntax-entry ?< "(>" html-search-for-tag-syntax)
			     (modify-syntax-entry ?> ")<" html-search-for-tag-syntax)
			     (modify-syntax-entry ?= "." html-search-for-tag-syntax) ;; for parsing attributes
			     ))
      (while (and (setq beg (apply (if backward 're-search-backward 're-search-forward) (list re bound noerror)))
		  (setq beg (match-beginning 0))
		  (html-invalid-context-p beg)))
      (when (and beg (match-beginning 3));; point is actually in the middle of a tag
	(goto-char (match-end 3))
	(condition-case err
	    (progn (backward-sexp)
		   (setq beg (point)))
	  (scan-error (setq beg nil)))
	(unless (and beg (looking-at re))
	  (put 'error-html-tag 'error-conditions '(error error-html))
	  (put 'error-html-tag 'error-message "Error during html fragment check")
	  (signal 'error-html-tag (list "Lonely > at:" (match-beginning 3)))))
      (when beg ;; matching head tag found
	(if (match-string 1)
	    (setq closes t)
	  (setq opens t))
	(setq type (match-string-no-properties 2))
	(goto-char beg)
	(condition-case err
	    (forward-sexp)
	  (error
	   (put 'error-html-tag 'error-conditions '(error error-html))
	   (put 'error-html-tag 'error-message "Error during html fragment check")
	   (signal 'error-html-tag (list "Lonely < at:" (match-beginning 0)))))
	(setq end (point))
	(when (= (char-after (- end 2)) ?/)
	  (setq closes t))
	;; look for attributes:
	(goto-char beg)
	(while (setq found (search-forward "=" end 'noErr))
	  (when (null (and (setq parserState (syntax-ppss))
			   (or (nth 3 parserState) ;; inside string
			       (nth 5 parserState)))) ;; quoted
	    (backward-sexp)
	    (setq name (sexp-at-point))
	    (goto-char found)
	    (skip-syntax-forward " " end)
	    (setq value (buffer-substring-no-properties
			 (if (looking-at "\"")
			     (1+ (point))
			   (point))
			 (save-excursion (forward-sexp)
					 (if (looking-back "\"")
					     (1- (point))
					   (point)))))
	    (setq attrList (cons (cons name value)  attrList))))
	(goto-char (if backward beg end))
	(list :type type
	      :beg beg :end end
	      :attrList attrList
	      :opens opens
	      :closes closes)))))

(defmacro html-check-frag-unless-void (tag &rest body)
  "Eval forms if tag is not a void tag.

\(fn TAG BODY...)"
  (declare (indent 1) (debug t))
  (append
   `(if (member (downcase (plist-get ,tag :type)) html-check-frag-void-tags)
	(when html-check-frag-debug
	  (push (list 'omit-void-tag :tag ,tag) html-check-frag-debug)))
   body))

(defun html-check-frag-region (&optional b e)
  "Check for invalid tags."
  (interactive)
  (declare (special html-check-frag-lighter html-check-frag-err))
  (unless (and b e)
    (if (and (called-interactively-p 'any) (use-region-p))
	(setq b (region-beginning)
	      e (region-end))
      (setq b (point-min)
	    e (point-max))
      ))
  (when html-check-frag-debug
    (push (list 'begin :status-html html-check-frag-err :bounds (list b e)) html-check-frag-debug))
  (condition-case err
      (let (stack-open stack-close pos tag tag-from-stack ol)
	(cl-flet* ((extend-err (type b e)
			       (if (null html-check-frag-err)
				   (setq-local html-check-frag-err (list :type type :beg b :end e))
				 (plist-put html-check-frag-err :type type)
				 (plist-put html-check-frag-err :beg (min b (plist-get html-check-frag-err :beg)))
				 (plist-put html-check-frag-err :end (max e (plist-get html-check-frag-err :end)))))
		   (deco-err (err tag)
			     (let* ((b (plist-get tag :beg))
				    (e (plist-get tag :end))
				    (ol (make-overlay b e)))
			       (extend-err err b e)
			       (overlay-put ol 'face 'html-check-frag-error-face))))
	  (save-excursion
	    (when html-check-frag-err
	      (setq b (min b (plist-get html-check-frag-err :beg))
		    e (max e (plist-get html-check-frag-err :end))
		    html-check-frag-err nil))
	    (remove-overlays b e 'face 'html-check-frag-error-face)
	    (setq-local html-check-frag-err nil)
	    (goto-char b)
	    (condition-case scan-err
		(progn
		  (while (or (and (<= (point) e) (setq tag (html-search-for-tag e 'noErr)))
			     (and stack-open (setq tag (html-search-for-tag nil 'noErr))))
		    (when html-check-frag-debug
		      (push (list 'found-tag :tag tag :stack-open stack-open :stack-close stack-close) html-check-frag-debug))
		    (when (< (plist-get tag :beg) b)
		      (setq b (plist-get tag :beg)))
		    (html-check-frag-unless-void tag
		     (if (plist-get tag :opens)
			 (unless (plist-get tag :closes)
			   (push tag stack-open)
			   (when  html-check-frag-debug
			     (push (list 'pushed-tag :stack-open stack-open) html-check-frag-debug)))
		       ;; closing
		       (if stack-open
			   (progn
			     (setq tag-from-stack (pop stack-open))
			     (unless (string= (plist-get tag :type) (plist-get tag-from-stack :type))
			       (when html-check-frag-debug
				 (push (list 'mismatch :tag tag :tag-from-stack tag-from-stack) html-check-frag-debug))
			       (deco-err 'mismatch tag)
			       (deco-err 'mismatch tag-from-stack)))
			 (push tag stack-close)
			 (when html-check-frag-debug
			   (push (list 'pushed-tag :stack-close stack-close) html-check-frag-debug))
			 ))))
		  (when html-check-frag-debug
		    (push (append (list 'after-open :stack-close stack-close) (when stack-open (list :stack-open stack-open))) html-check-frag-debug))
		  (when stack-open
		    (loop for tag in stack-open do
			  (when html-check-frag-debug
			    (push (list 'missing :tag tag) html-check-frag-debug))
			  (deco-err 'missing tag)
			  ))
		  (when stack-close
		    (setq stack-close (nreverse stack-close))
		    (goto-char b)
		    (while (and stack-close ;; trying to reduce stack-close
				(setq tag (html-search-for-tag nil 'noErr 'back)))
		      (when html-check-frag-debug
			(push (list 'found-tag :tag tag :stack-open stack-open :stack-close stack-close) html-check-frag-debug))
		      (html-check-frag-unless-void tag
		       (if (plist-get tag :closes)
			   (unless (plist-get tag :opens)
			     (push tag stack-close)
			     (when html-check-frag-debug
			       (push (list 'pushed-tag :stack-close stack-close) html-check-frag-debug)))
			 ;; matching opening tag
			 (setq tag-from-stack (pop stack-close))
			 (unless (string= (plist-get tag :type) (plist-get tag-from-stack :type))
			   (when html-check-frag-debug
			     (push (list 'mismatch :tag tag :tag-from-stack tag-from-stack) html-check-frag-debug))
			   (deco-err 'mismatch tag)
			   (deco-err 'mismatch tag-from-stack))
			 ))))
		  (if html-check-frag-err
		      (setq-local html-check-frag-lighter  (concat " " (upcase (symbol-name (plist-get html-check-frag-err :type)))))
		    (setq-local html-check-frag-lighter "")))
	      (error-html
	       (setq b (nth 2 scan-err))
	       (setq e (1+ (nth 2 scan-err)))
	       (extend-err 'tag b e)
	       (setq-local html-check-frag-lighter " TAG")
	       (setq ol (make-overlay b e))
	       (overlay-put ol 'face 'html-check-frag-error-face))
	      ))
	  (force-mode-line-update))
	(when html-check-frag-debug
	  (push (list 'end :status 'ok :status-html html-check-frag-err) html-check-frag-debug))
	)
    (error
     (when html-check-frag-debug
       (push (list 'end :status 'exception :err err) html-check-frag-debug))
     (signal (car err) (cdr err))
     )))

(defun html-check-frag-next-e (e)
  "`html-check-frag-next' for usage with a keymap."
  (interactive "e")
  (with-current-buffer (window-buffer (select-window (posn-window (event-start e))))
    (html-check-frag-region)
    (html-check-frag-next)
    ))

(defun html-check-frag-next ()
  "Go to the end of the next text marked with the face property `html-check-frag-error-face'.
Search starts from point."
  (interactive)
  (let ((old-point (point)) wrapped)
    (while (and
	    (progn
	      (goto-char (or (and wrapped (min old-point (next-overlay-change (point))))
			     (next-overlay-change (point))))
	      (when (= (point) (point-max))
		(goto-char (point-min))
		(setq wrapped t))
	      (null (and wrapped (= (point) old-point))))
	    (null (loop for ol in (overlays-at (point))
			thereis (and (eq (overlay-get ol 'face) 'html-check-frag-error-face)
				     (goto-char (overlay-end ol)))))))))

(defvar html-check-frag-lighter-map)
(setq html-check-frag-lighter-map (make-sparse-keymap))
(define-key html-check-frag-lighter-map (kbd "<mode-line> <S-mouse-1>") 'html-check-frag-next-e)
(put 'html-check-frag-lighter-map 'risky-local-variable t)

(defvar html-check-frag-lighter ""
  "Lighter for html-check-frag-mode.")
(make-variable-buffer-local 'html-check-frag-lighter)

(define-minor-mode html-check-frag-mode
  "Check html-fragments around point and decorate tags.
To be used with html-mode as major mode.
If you get a red lighter TAG or MISSING or MISSMATCH you can
get to the next bad tag by pressing S-mouse-1 on it.
This mouse event actually runs `html-check-frag-next'."
  :lighter (:eval (propertize html-check-frag-lighter
			      'keymap html-check-frag-lighter-map
			      'face 'html-check-frag-error-face))
  (declare (special html-check-frag-lighter html-check-frag-err))
  (if html-check-frag-mode
      (progn
	(setq-local html-check-frag-err nil)
	(setq-local html-check-frag-lighter "")
	(jit-lock-register 'html-check-frag-region))
    (remove-overlays (point-min) (point-max) 'face 'html-check-frag-error-face)
    (jit-lock-unregister 'html-check-frag-region)
    (setq-local html-check-frag-err nil)
    (unintern 'html-search-for-tag-syntax obarray)))

(provide 'html-check-frag)
;;; html-check-frag.el ends here
