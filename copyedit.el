;;; copyedit.el -- tweak editing commands for handling prose.

;; Copyright (c) 2009 Paul M. Rodriguez
;; Author: Paul M. Rodriguez <paulmrodriguez@gmail.com>
;; Created: 2009-12-27
;; Keywords: editing
;; %Id: 1%

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; This is a minor mode for writers and editors which tweaks the
;; behavior of some Emacs commands to make them more efficient at
;; editing English text, and defines some new commands which may be
;; found useful.

;; I am aware that this code defines more abstractions than it
;; actually uses.  But there are bound to be special cases I haven't
;; anticipated where they may come in handy.

;;; Use:

;; The basic commands that copyedit-mode changes should largely be
;; invisible and perceived only as a general sense of lightness,
;; quickness, and ease.

;; For extra keybindings, setq or customize `copyedit-bind-extra-keys'
;; to t.

;; C-h m to see all keybindings.

;;; Code:

(eval-when-compile (require 'cc-defs)
		   (require 'cl))
(require 'thingatpt)

;;; Customize
(defgroup copyedit nil
  "Edit prose."
  :tag "Copyedit"
  :group 'editing)
  
(defcustom copyedit-bind-extra-keys nil
  "Use `copyedit-extra-keymap'."
  :group 'copyedit
  :type 'boolean)

;;; Vars and constants
(defvar *copyedit-this-stop* nil)

(defconst copyedit-non-punctuation-re "[[:word:][:blank:] \']")
(defconst copyedit-punctuation-re
  (concat "^" copyedit-non-punctuation-re))
(defconst copyedit-scene-re "^[ \t]*[ *#\t\f\n]*$")
(defconst copyedit-stop-re "[.?!)]")
(defconst copyedit-blank-re "[ \t]*$")
(defconst copyedit-open-quote-re "[\"“‘]")
(defconst copyedit-close-quote-re "[\"”’]")
(defconst copyedit-dashes "[-—–‐‒−]")
;; hyphen-minus, hyphen, figure dash, en dash, em dash, minus sign.

;;; Context definitions

(defvar copyedit-contexts-alist '())

(defmacro copyedit-define-context (key &rest forms)
  "Define KEY as a context tested by FORMS.
Forms are evaluated inside `lambda'."
  (declare (indent 1))
  `(progn
     (assert (keywordp ,key) t "Not a keyword: %s")
     (push
      (cons ,key
	    (lambda ()
	      ,@forms))
      copyedit-contexts-alist)))

(copyedit-define-context :all
  t)
(copyedit-define-context :bol
  (copyedit-bolp))
(copyedit-define-context :boi
  (let ((boi (c-point 'boi))
	(bol (c-point 'bol)))
    (and (not (equal boi bol))
	 (or (equal (point) boi)
	     (< (point) boi)))))
(copyedit-define-context :not-bol
  (not (copyedit-bolp)))
(copyedit-define-context :eol
  (copyedit-eolp))
(copyedit-define-context :not-eol
  (not (copyedit-eolp)))
(copyedit-define-context :in-sentence
  (not (or (copyedit-bolp) (copyedit-eolp))))
(copyedit-define-context :bob
  (copyedit-bobp))
(copyedit-define-context :not-bob
  (not (copyedit-bobp)))
(copyedit-define-context :eob
  (copyedit-eobp))
(copyedit-define-context :not-eob
  (not (copyedit-eobp)))
(copyedit-define-context :stop
  (copyedit-get-stop))
(copyedit-define-context :no-stop
  (not (copyedit-get-stop)))
(copyedit-define-context :quote
  (thing-at-point 'quote))
(copyedit-define-context :eos
  (copyedit-end-of-scene-p))
(copyedit-define-context :not-eos
  (and (not (copyedit-end-of-scene-p))
       (not (copyedit-bobp))
       (not (copyedit-eobp))))
(copyedit-define-context :thisblank
  (copyedit-thisblank-p))
(copyedit-define-context :nextblank
  (copyedit-nextblank-p))
(copyedit-define-context :lastblank
  (copyedit-lastblank-p))
(copyedit-define-context :bothblank
  (and (copyedit-nextblank-p)
       (copyedit-lastblank-p)))
(copyedit-define-context :allblank
  (and (copyedit-nextblank-p)
       (copyedit-lastblank-p)
       (copyedit-thisblank-p)))
(copyedit-define-context :noblank
  (and (null (copyedit-thisblank-p))
       (null (copyedit-nextblank-p))))
(copyedit-define-context :region
  (use-region-p))
(copyedit-define-context :between-words
  (and (copyedit-test-last-char copyedit-non-punctuation-re)
       (not (eolp))
       (copyedit-test-next-char copyedit-non-punctuation-re)))
(copyedit-define-context :before-word
  (copyedit-test-next-char "[[:word:]]"))
(copyedit-define-context :inside-word
  (copyedit-inside-thing-p 'word))
(copyedit-define-context :noword
  (not (c-safe (thing-at-point 'word))))
(copyedit-define-context :after-point
  (not (copyedit-test-last-char "[[:word:]]")))
(copyedit-define-context :before-space
  (copyedit-test-next-char " "))
(copyedit-define-context :after-space
  (copyedit-test-last-char " "))
(copyedit-define-context :before-quote
  (and (not (quote-at-point))
       (copyedit-test-next-char copyedit-open-quote-re)))
(copyedit-define-context :boq
  (let ((boq (c-safe (quote-beginning-position))))
    (if boq (or (eq (point) boq)
		(eq (save-excursion
		      (backward-copyedit-ws)
		      (point))
		    boq)))))
(copyedit-define-context :eoq
  (let ((eoq (c-safe
	       (quote-end-position))))
    (if eoq (eq (point) eoq))))
(copyedit-define-context :last-word
  (save-excursion
    (and (forward-word 1)
	 (looking-at copyedit-stop-re))))
(copyedit-define-context :one-word-sentence
  (save-excursion
    (and
     (eq
      (c-safe (beginning-of-thing 'word))
      (c-safe (beginning-of-thing 'sentence)))
     (and
      (forward-word 1)
      (looking-at copyedit-stop-re)))))
(copyedit-define-context :indent
  (save-excursion
    (back-to-indentation)
    (not (zerop (current-column)))))
(copyedit-define-context :after-stray-point
  (save-excursion
    (and (progn (backward-copyedit-word 1)
		(skip-syntax-forward copyedit-punctuation-re)
		(forward-copyedit-ws))
	 (progn (forward-copyedit-word 1)
		(skip-syntax-backward copyedit-punctuation-re)
		(backward-copyedit-ws)))))
(copyedit-define-context :before-stray-point
  (save-excursion
    (and (progn (forward-copyedit-word 1)
		(skip-syntax-backward copyedit-punctuation-re)
		(backward-copyedit-ws))
	 (progn (backward-copyedit-word 1)
		(skip-syntax-forward copyedit-punctuation-re)
		(forward-copyedit-ws)))))
(copyedit-define-context :after-dash
  (save-excursion
    (or (copyedit-test-last-char copyedit-dashes)
	(and (backward-copyedit-ws)
	     (copyedit-test-last-char copyedit-dashes)))))
(copyedit-define-context :before-dash
  (save-excursion
    (or (copyedit-test-next-char copyedit-dashes)
	(and (forward-copyedit-ws)
	     (copyedit-test-next-char copyedit-dashes)))))


;;; Context macros

(defun copyedit-check-context-args (keyword list)
  "Return t if KEYWORD is a keyword and LIST is a list."
  (and
   (keywordp keyword)
   (assoc keyword copyedit-contexts-alist)
   (listp list)))

(defun in-context-p (context)
  "Test for a CONTEXT from `copyedit-contexts-alist'."
  (funcall (cdr (assoc context copyedit-contexts-alist))))

(defmacro in-one-context (&rest args)
  "Test for context and evaluate appropriate form.
ARGS should alternate between keywords from
`copyedit-contexts-alist' and a form to evaluate in that context."
  (let (forms)
    (setq args
	  (mapcar (lambda (arg)
		    (if arg arg '(ignore)))
		  args))
    (while args
	(let* ((keyword (pop args))
	       (value (pop args))
	       (func (cdr (assoc keyword copyedit-contexts-alist))))
	  (assert (copyedit-check-context-args keyword value) t
		  "Not a context-form pair: %s %s")
	  (push (cons (list func) (list value)) forms)))
      (setq forms (cons 'cond (nreverse forms)))
      `,@forms))

(defmacro in-each-context (&rest args)
  "Evaluate all forms in relevant contexts.
ARGS should alternate between keywords from
`copyedit-contexts-alist' and a form to evaluate in that context."
  (let (forms)
    (setq args
	  (mapcar (lambda (arg)
		    (if arg arg '(ignore)))
		  args))
    (while args
      (let* ((keyword (pop args))
	     (value (pop args))
	     (func (cdr (assoc keyword copyedit-contexts-alist))))
	(assert (copyedit-check-context-args keyword value) t
		"Not a context-form pair: %s %s")
	(push (list 'if (list func) value) forms)))
    (setq forms (cons 'progn (nreverse forms)))
    `,@forms))


;;; Utility functions

(defun backward-copyedit-ws ()
  "Like `c-skip-ws-backward', but return non-nil if point moves."
  (let ((point (point)))
    (c-skip-ws-backward)
    (not (eq point (point)))))
    
(defun forward-copyedit-ws ()
  "Like `c-skip-ws-forward', but return non-nil if point moves."
  (let ((point (point)))
    (c-skip-ws-forward)
    (not (eq point (point)))))

(defun bounds-of-copyedit-ws ()
  "Return bounds of whitespace."
  (save-excursion
    (let ((a (and (or (forward-copyedit-ws)
		      (backward-copyedit-ws))
		  (point)))
	  (b (and (or (backward-copyedit-ws)
		      (forward-copyedit-ws))
		  (point))))
      (if (> a b)
	  (cons b a)
	(cons a b)))))
(put 'copyedit-ws 'bounds-of-thing-at-point 'bounds-of-copyedit-ws)

(defun copyedit-zap (thing)
  "Kill all text between here and end of THING."
  (kill-region (point) (end-of-thing thing)))

(defun copyedit-zap-backward (thing)
  "Kill all text between here and beginning of THING."
  (kill-region (point) (beginning-of-thing thing)))

(defun copyedit-nuke (thing)
  "Kill THING."
  (kill-region (beginning-of-thing thing) (end-of-thing thing)))

(defun copyedit-skip-context-forward (keyword)
  "Skip context KEYWORD from `copyedit-contexts-alist'."
  (while (in-context-p keyword)
    (forward-char 1)))

(defun copyedit-skip-context-backward (keyword)
  "Skip backward context KEYWORD from `copyedit-contexts-alist.'."
  (while (in-context-p keyword)
    (backward-char 1)))

(defun copyedit-call-appropriately (function)
  "Call FUNCTION, interactively if it is a command."
  (assert (functionp function) nil "No function")
  (if (commandp function)
      (call-interactively function)
    (funcall function)))

(defun copyedit-goto-position (position)
  "Call `c-point' on POSITION and go to char."
  (let ((point (c-point position)))
    (if point (goto-char point))))

;;; Utility macros

(defmacro until (test &rest body)
  "Unless TEST returns non-nil, do BODY.
Until:while::unless:if."
  (declare (indent 1) (debug t))
  `(while (not ,test)
     ,@body))

(defmacro save-stop (&rest body)
  "Run BODY with `*copyedit-this-stop*' bound to the last stop, if any."
  (assert (not *copyedit-this-stop*))
  ;; this-stop should be globally accessible
  `(let ((*copyedit-this-stop* (copyedit-get-stop)))
     ,@body))

(defmacro restrict-to-paragraph (&rest body)
  "Run BODY like `progn' with restriction to paragraph."
  `(save-restriction
     (narrow-to-region
      (save-excursion
	(beginning-of-thing 'paragraph)
	(forward-copyedit-ws)
	(point))
      (save-excursion
	(end-of-thing 'paragraph)
	(backward-copyedit-ws)
	(point)))
     ,@body))

;;; Test functions

(defun copyedit-eolp ()
  "Test for end of line."
  (labels ((this-eolp ()
		      (or (eolp)
			  (looking-at copyedit-blank-re))))
    (if auto-fill-function
	(and (this-eolp)
	     (copyedit-nextblank-p))
      (this-eolp))))

(defun copyedit-eobp ()
  "Test for end of buffer."
  (or (eobp)
      (save-excursion
	(forward-copyedit-ws)
	(eobp))))

(defun copyedit-bobp ()
  "Test for beginning of buffer."
  (or (bobp)
      (save-excursion
	(backward-copyedit-ws)
	(bobp))))

(defun copyedit-bolp ()
  "Test for beginning of line."
  (labels ((this-bolp ()
			 (or (bolp)
			     (copyedit-test-last-char 'bolp))))
    (if auto-fill-function
	(and (this-bolp)
	     (copyedit-lastblank-p))
      (this-bolp))))

(defun copyedit-test-last-char (string-or-function)
  "Test STRING-OR-FUNCTION on last char.
If STRING-OR-FUNCTION is a string, return non-nil if it matches
the last char.
If STRING-OR-FUNCTION is a function, return non-nil it it returns
non-nil one char forward.
Return non-nil if char before point matches STRING."
  (unless (bobp)
    (typecase string-or-function
      (string
       (string-match-p string-or-function
		       (char-to-string (char-before (point)))))
      (function
       (save-excursion
	 (forward-char -1)
	 (copyedit-call-appropriately string-or-function))))))

(defun copyedit-test-next-char (string-or-function)
  "Test STRING-OR-FUNCTION on next char.
If STRING-OR-FUNCTION is a string, return non-nil if it matches the next char.
If STRING-OR-FUNCTION is a function, return non-nil it it returns non-nil one char forward."
  (unless (eobp)
    (typecase string-or-function
      (string
       (string-match-p string-or-function
		       (char-to-string (char-after (point)))))
      (function
       (save-excursion
	 (forward-char 1)
	 (copyedit-call-appropriately string-or-function))))))

(defun copyedit-get-stop ()
  "Return non-nil if point is at a stop."
  (save-excursion
    (backward-copyedit-ws)
    (if (copyedit-test-last-char copyedit-stop-re)
	(char-to-string
	 (char-before))
      nil)))

(defun copyedit-thisblank-p ()
  "Return non-nil if line at point is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at copyedit-blank-re)))

(defun copyedit-nextblank-p ()
  "Return non-nil if the next line is blank."
  (save-excursion
    (forward-line 1)
    (beginning-of-line)
    (looking-at copyedit-blank-re)))

(defun copyedit-lastblank-p ()
  "Return non-nil if the line before point is blank."
  (save-excursion
    (forward-line -1)
    (looking-at copyedit-blank-re)))

;;; Scenes

(defun copyedit-end-of-scene-p ()
  "Return non-nil if point is at end of scene or buffer."
  (or (eobp)
      (and
       (looking-at copyedit-scene-re)
       (or (save-excursion
	     (forward-line -1)
	     (looking-at copyedit-scene-re))
	   (save-excursion
	     (forward-line 1)
	     (looking-at copyedit-scene-re))))))

(defun forward-copyedit-scene ()
  "Go to next scene break or end of buffer."
  (interactive)
  (forward-line 1)
  (until (or (eobp)
	     (in-context-p :eos))
    (forward-line 1))
  (copyedit-skip-context-forward :eos))

(defun backward-copyedit-scene ()
  "Go to previous scene break or beginning of buffer."
  (interactive)
  (forward-line -1)
  (until (or (bobp)
	     (in-context-p :eos))
    (forward-line -1))
  (copyedit-skip-context-backward :eos))

;;; Quotes

(defmacro restrict-to-quote (&rest body)
  "When in a quote do BODY like `progn' with restriction to quote.
Otherwise just do BODY."
  `(if (c-safe (quote-at-point))
       (save-restriction
	 (narrow-to-region (quote-beginning-position)
			   (quote-end-position))
	 ,@body)
     (progn
       ,@body)))

(defun copyedit-get-quotes ()
  "Unless inside quotes, return nil.
If inside quotes, return their position as a list."
  (let ((open-quote
	 (save-excursion
	   (and
	    (re-search-backward copyedit-open-quote-re nil t 1)
	    (in-one-context
	     :bob		(point)
	     :bol		(point)
	     :after-space	(point)
	     :all               nil))))
	(close-quote
	 (save-excursion
	   (and
	    (re-search-forward copyedit-close-quote-re nil t 1)
	    (in-one-context
	     :eob		(point)
	     :eol		(point)
	     :before-space	(point)
	     :all               nil)))))
    (if (and open-quote close-quote)
	(cons (+ open-quote 1)
	      (- close-quote 1)) nil)))

(put 'quote 'bounds-of-thing-at-point 'copyedit-get-quotes)
(defun quote-at-point ()
  "Return the quote at point, or nil if none is found."
  (thing-at-point 'quote))
(defun beginning-of-quote ()
  "Go to beginning of quote at point."
  (beginning-of-thing 'quote))
(defun quote-beginning-position ()
  "Return position of beginning of quote at point."
  (save-excursion
    (beginning-of-quote)
    (point)))
(defun end-of-quote ()
  "Go to end of quote at point."
  (end-of-thing 'quote))
(defun quote-end-position ()
  "Return position of end of quote at point."
  (save-excursion
    (end-of-quote)
    (point)))

;;; Quote commands

(defun copyedit-kill-quote-forward ()
  "Kill all remaining text inside a quote."
  (interactive)
  (copyedit-zap 'quote)
  (copyedit-fixup))

(defun copyedit-kill-quote-backward ()
  "Kill all previous text inside a quote."
  (interactive)
  (copyedit-zap-backward 'quote)
  (copyedit-fixup))

(defun copyedit-kill-quote ()
  "Kill all text inside a quote."
  (interactive)
  (copyedit-nuke 'quote))

;; Internal

(defun copyedit-inside-thing-p (thing)
  "Return non-nil if there is a THING at point and point is inside it."
  (and (c-safe (thing-at-point thing))
       (not (eq (point) (save-excursion
			  (beginning-of-thing thing)
			  (point))))
       (not (eq (point) (save-excursion
			  (end-of-thing thing)
			  (point))))))

(defun copyedit-kill-subr (thing &optional direction)
  "Kill THING in DIRECTION.
If no DIRECTION is specified, kill entire THING.
If inside THING, do not delete whitespace.
Otherwise whitespaces in buffer always decrease by one."
  ;; the idea here is to treat the number of whitespaces as an
  ;; invariant: killing a thing should always decrease the total
  ;; number of whitespaces by one.
  (interactive)
  ;; eg the thing is followed or preceded by a point, or we are
  ;; trapped between points
  (case direction
    ('forward
     (or
      (forward-copyedit-ws)
      (if (or
      	   (not (c-safe (thing-at-point thing)))
      	   (eq (point) (save-excursion (end-of-thing thing))))
	  (forward-thing thing))))
    ('backward
     (or
      (backward-copyedit-ws)
      (if (or
	   (not (c-safe (thing-at-point thing)))
	   (eq (point) (save-excursion (beginning-of-thing thing))))
	  (forward-thing thing -1)))))
  ;; inside a thing, no changes to whitespace
  (if (copyedit-inside-thing-p thing)
      (case direction
	(nil (copyedit-nuke thing))
	('forward (copyedit-zap thing))
	('backward (copyedit-zap-backward thing)))
    (let ((space-before
	   (save-excursion
	     (beginning-of-thing thing)
	     (if (backward-copyedit-ws)
		 (c-safe (thing-at-point 'copyedit-ws)))))
	  (space-after
	   (save-excursion
	     (end-of-thing thing)
	     (if (forward-copyedit-ws)
		 (c-safe (thing-at-point 'copyedit-ws))))))
      (copyedit-nuke thing)
      (if (c-safe (thing-at-point 'copyedit-ws))
	  (copyedit-nuke 'copyedit-ws))
      (cond
       ((and space-before space-after) (insert space-before))
       ((and (not space-before) (not space-after))
	(kill-region (point) (progn
			       (forward-thing thing)
			       (beginning-of-thing thing)
			       (point))))))))

(defun copyedit-stop ()
  "Insert a stop with one or spaces."
  (delete-horizontal-space)
  (copyedit-depunctuate-backward)
  ;; if inside a save-stop form, use this-stop.
  (let ((stop (or *copyedit-this-stop* "."))
	(space (if sentence-end-double-space
		   "  " " ")))
    (in-one-context
     :after-point	(insert space)
     :eoq		(insert stop)
     :eol		(insert stop)
     :all		(insert (concat stop space)))))

(defun copyedit-find-space (&optional arg)
  "Go to the next space.  With ARG, ignore space at point."
  (interactive "P")
  (if arg (skip-chars-forward " "))
  (in-one-context
   :inside-word (skip-syntax-forward "^ ")))

(defun copyedit-find-space-backward (&optional arg)
  "Go to the last space.  With ARG, ignore space at point."
  (interactive "P")
  (if arg (skip-chars-backward " "))
  (in-one-context
   :inside-word (skip-syntax-backward "^ "))
  (backward-char 1))

(defun copyedit-delete-horizontal-space ()
  "Delete next horizontal space."
  (interactive)
  (copyedit-find-space)
  (in-one-context
   :eol (copyedit-nuke 'copyedit-ws)
   :all (delete-horizontal-space)))

(defun copyedit-depunctuate-forward ()
  "Remove punctuation after point."
  (save-excursion
    (forward-copyedit-ws)
    (let ((start (point))
	  (end (point)))
      (save-excursion
	(unless (zerop (skip-syntax-forward copyedit-punctuation-re))
	  (setq end (point))))
      (kill-region start end)))
  (in-one-context :bol (delete-horizontal-space)))

(defun copyedit-depunctuate-backward ()
  "Remove punctuation before point."
  (save-excursion
    (backward-copyedit-ws)
    (let ((start (point))
	  (end (point)))
      (save-excursion
	(unless (zerop (skip-syntax-backward copyedit-punctuation-re))
	  (setq end (point))))
      (kill-region start end))))

(defun copyedit-depunctuate ()
  "Remove any punctuation between words at point."
  (interactive)
  (forward-copyedit-word 1)
  (in-one-context
   :eol  nil
   :stop (save-excursion
	    (downcase-word 1)))
  (progn
    (copyedit-depunctuate-forward)
    (copyedit-depunctuate-backward))
  (in-one-context
   :bol nil
   :boi nil
   :all (just-one-space)))

(defun copyedit-fixup ()
  "Insert the right spacing for the context."
  (in-each-context
   :boi			(progn
			  (copyedit-depunctuate-forward)
			  (copyedit-capitalize))
   :bol			(progn
			  (copyedit-depunctuate-forward)
			  (copyedit-capitalize))
   :boq			(restrict-to-quote
			 (copyedit-capitalize))
   :stop		(in-one-context
			 :boi nil
			 :bol nil
			 :all (save-stop
			       (restrict-to-paragraph
				(copyedit-depunctuate-backward)
				(copyedit-depunctuate-forward)
				(copyedit-stop)
				(copyedit-capitalize))))
  :eol        		(delete-horizontal-space)))

(defun copyedit-capitalize ()
  "Capitalize the next word."
  (save-excursion
    (capitalize-word 1)))

;;; Yank
(defun copyedit-yank-clean ()
  "Yank text in lowercase, without punctuation."
  (interactive)
  (let (string)
    (with-temp-buffer
      (yank)
      (downcase-region (point-min) (point-max))
      (goto-char (point-min)) (forward-char 1)
      (until (eobp)
	(copyedit-depunctuate))
      (setq string (buffer-string)))
    (insert string)))


;;; Commands
(defun copyedit-mark-sentence ()
  "Mark a sentence."
  (interactive)
  (in-one-context
   :no-stop (backward-copyedit-sentence 1))
  (mark-end-of-sentence 1))

(defun copyedit-open-sentence ()
  "End the previous sentence here, fixing capitalization and spacing."
  (interactive)
  (in-one-context
   :in-sentence (progn
		  (copyedit-depunctuate)
		  (copyedit-stop)
		  (copyedit-capitalize))))

(defun kill-copyedit-sentence (arg)
  "Like `kill-sentence' with ARG, but insert a stop and spaces after."
  (interactive "p")
  (in-one-context
   :quote	(progn
		  (restrict-to-quote
		   (copyedit-kill-subr 'sentence 'forward)
		   (copyedit-stop))
		  (copyedit-fixup))
   :stop	(save-stop
		 (copyedit-kill-subr 'copyedit-sentence 'forward)
		 (copyedit-stop))
   :all		(progn
		  (copyedit-kill-subr 'copyedit-sentence 'forward)
		  (copyedit-stop)
		  (copyedit-fixup))))

(defun backward-kill-copyedit-sentence ()
  "Like `backward-kill-sentence', but insert stop and fix caps after."
  (interactive)
  (in-one-context
   :quote	(progn
		  (restrict-to-quote
		   (copyedit-kill-subr 'sentence 'backward))
		  (copyedit-fixup))
   :all	(progn
	  (copyedit-kill-subr 'sentence 'backward)
	  (copyedit-fixup))))

(defun kill-whole-copyedit-sentence ()
  "Kill an entire sentence."
  (interactive)
  (copyedit-kill-subr 'sentence)
  (copyedit-fixup))

(defun copyedit-join-sentence ()
  "Join two sentences, lowercasing as appropriate."
  (interactive)
  (forward-sentence 1)
  (copyedit-depunctuate)
  (copyedit-nuke 'copyedit-ws)
  (just-one-space)
  (save-excursion (downcase-word 1)))

(defun backward-copyedit-word (arg)
  "Like `backward-word' with ARG.
When arg = 1, point is always left before or after word."
  (interactive "p")
  (if (eq arg 1)
      (and (zerop (skip-chars-backward "[:word:]"))
	   (zerop (skip-chars-backward "^[:word:]"))
	   (backward-word 1))
    (backward-word arg)))
   
(defun backward-copyedit-sentence (arg)
  "Like `backward-sentence' with ARG, but never end up on a blank line."
  (interactive "p")
  (in-one-context
   :bol (progn
	  (backward-copyedit-ws)
	  (backward-sentence arg))
   :all (progn
	  (backward-sentence arg)
	  (in-one-context :thisblank (forward-line 1)))))

(defun backward-copyedit-paragraph (arg)
  "Like `backward-paragraph' with ARG, but put point at beginning of paragraph."
  (interactive "p")
  (backward-paragraph (+ 1 arg))
  (forward-copyedit-ws))

(defun backward-kill-copyedit-word ()
  "Like `kill-word' with ARG, but don't leave extra spaces."
  (interactive)
  (copyedit-depunctuate-backward)
  (copyedit-kill-subr 'word 'backward)
  (copyedit-fixup))
  
(defun forward-copyedit-word (arg)
  "Like `forward-word' with ARG.
When arg = 1, point is always left before or after word."
  (interactive "p")
  (if (eq arg 1)
      (and (zerop (skip-chars-forward "[:word:]"))
	   (zerop (skip-chars-forward "^[:word:]"))
	   (forward-word 1))
    (forward-word arg)))

(defun forward-copyedit-sentence (arg)
  "Like `forward-sentence' with ARG, but put point at beginning of sentence."
  (interactive "p")
  (forward-sentence arg)
  (in-one-context :not-eol (forward-copyedit-ws)))

(defun forward-copyedit-paragraph (arg)
  "Like `forward-paragraph' with ARG, but put point at beginning of paragraph."
  (interactive "p")
  (forward-paragraph arg)
  (forward-copyedit-ws))

(defun kill-copyedit-word ()
  "Like `kill-word' with ARG, but don't leave extra spaces."
  (interactive)
  (copyedit-depunctuate-forward)
  (in-one-context
   :one-word-sentence	(progn
			  (forward-copyedit-ws)
			  (copyedit-kill-subr 'sentence)
			  (copyedit-fixup))
   :quote		(restrict-to-quote
			 (copyedit-kill-subr 'word 'forward))
   :all                 (copyedit-kill-subr 'word 'forward))
  (copyedit-fixup))

(defun kill-copyedit-paragraph ()
  "Like `kill-line', but don't leave extra spaces."
  (interactive)
  (in-one-context
   :thisblank   (progn (copyedit-nuke 'copyedit-ws)
		       (copyedit-fixup))
   :all		(progn
		  (copyedit-kill-subr 'paragraph 'forward)
		  (copyedit-fixup))))

(defun backward-kill-copyedit-paragraph ()
  "Kill paragraph backwards and fix whitespaces and points."
  (interactive)
  (in-one-context
   :thisblank (progn (copyedit-nuke 'copyedit-w)
		     (copyedit-fixup))
   :all       (progn
		(copyedit-kill-subr 'paragraph 'backward)
		(copyedit-fixup))))

(defun copyedit-delete-blank-lines ()
  "Like `delete-blank-lines', but sans blank line, `join-line' forward."
  (interactive)
  (in-one-context
   :noblank	(join-line 1)
   :all		(delete-blank-lines)))


;;; Dragging
(defun copyedit-drag (arg)
  "Drag region ARG words forward or backward."
  (in-one-context
   :region	(copyedit-drag-region arg)
   :all		(transpose-words arg)))

(defun copyedit-drag-region (arg)
  "Drag region left or right one word at a time.
Positive ARG drags right, negative ARG drags left."
  (let (deactivate-mark)
    (if (use-region-p)
	(let ((beg (region-beginning))
	      (end (region-end)))
	  (if (> arg 0)
	      (transpose-regions
	       beg end
	       end
	       (progn
		 (goto-char end)
		 (forward-word arg)
		 (point)))
	    (transpose-regions
	     (progn
	       (goto-char beg)
	       (forward-word arg)
	       (point))
	     beg
	     beg end))))))

(defun copyedit-drag-right () "Drag region one word right."
  (interactive) (copyedit-drag 1))
(defun copyedit-drag-left () "Drag region one word left."
  (interactive) (copyedit-drag -1))

;;; Mode setup

(defvar copyedit-keymap
  (let ((map (make-sparse-keymap)))
    ;; redefined
    (define-key map [remap kill-word]			'kill-copyedit-word)
    (define-key map [remap forward-word]		'forward-copyedit-word)
    (define-key map [remap forward-sentence]		'forward-copyedit-sentence)
    (define-key map [remap backward-sentence]		'backward-copyedit-sentence)
    (define-key map [remap forward-paragraph]		'forward-copyedit-paragraph)
    (define-key map [remap backward-kill-word]		'backward-kill-copyedit-word)
    (define-key map [remap backward-word]		'backward-copyedit-word)
    (define-key map [remap backward-paragraph]		'backward-copyedit-paragraph)
    (define-key map [remap kill-sentence]		'kill-copyedit-sentence)
    (define-key map [remap kill-line]		        'kill-copyedit-paragraph)
    (define-key map [remap delete-horizontal-space]	'copyedit-delete-horizontal-space)
    (define-key map [remap delete-blank-lines]		'copyedit-delete-blank-lines)
    (define-key map [remap backward-page]		'backward-copyedit-scene)
    (define-key map [remap forward-page]		'forward-copyedit-scene)
    map))

(defvar copyedit-extra-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map copyedit-keymap)
    (define-key map (kbd "C-S-y")	'copyedit-yank-clean)
    (define-key map (kbd "C-j")		'copyedit-join-sentence)
    (define-key map (kbd "M-j")		'copyedit-mark-sentence)
    (define-key map (kbd "M-.")		'copyedit-open-sentence)
    (define-key map (kbd "M-,")		'copyedit-depunctuate)
    (define-key map (kbd "M-S-k")	'backward-kill-copyedit-sentence)
    (define-key map (kbd "C-S-k")	'backward-kill-copyedit-paragraph)
    (define-key map (kbd "C-\"")        'copyedit-kill-quote)
    (define-key map (kbd "C-'")         'copyedit-kill-quote-forward)
    (define-key map (kbd "M-'")         'copyedit-kill-quote-backward)
    (define-key map (kbd "M-<right>")	'copyedit-drag-right)
    (define-key map (kbd "M-<left>")	'copyedit-drag-left)
    map))

(define-minor-mode copyedit-mode
  "Tweak Emacs for editing prose."
  :init-value nil
  :lighter " copyedit"
  :keymap (if copyedit-bind-extra-keys
	      copyedit-extra-keymap
	    copyedit-keymap))

(provide 'copyedit)

;;; copyedit.el ends here
