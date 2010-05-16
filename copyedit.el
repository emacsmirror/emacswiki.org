;;; copyedit.el -- tweak editing commands for handling prose.

;; Copyright (c) 2009 Paul M. Rodriguez
;; Author: Paul M. Rodriguez <paulmrodriguez@gmail.com>
;; Created: 2009-12-27
;; Keywords: editing
;; %Id: 2%

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

(defconst copyedit-scene-re "^[ \t]*[ *#\t\f\n]*$")
(defconst copyedit-blank-re "[ \t]*$")

;;; Context definitions

;; Why contexts? -- The reason Copyedit exists is to handle the corner
;; cases of English punctuation smoothly and invisible.  A naive
;; implementation would require an unreadable and unmaintainable
;; spaghetti bowl of nested conditions.  Instead Copyedit uses
;; contexts.  Contexts are defined by `define-copyedit-context' as
;; functions interned in the `copyedit-contexts' obarray.  These
;; functions may be called directly with `in-context-p', but usually
;; they are supplied to one of the context macros -- `in-one-context',
;; `in-each-context', or `out-of-context' -- as keyword arguments.
;; Whether the value of the argument evaluated depends on which macro
;; is used.  `in-one-context' evaluates the value of only the first
;; true context; `in-each-context' evaluates the value of all true
;; contexts; `out-of-context' evaluates the value of all false
;; contexts.  (That is, under the hood, `in-one-context' expands into
;; a `cond' form; `in-each-context' expands into a series of `when'
;; forms; `out-of-context' expands into a series of `unless' forms.)

;; one less than a power of two (2^8) is supposed to be good
(defvar copyedit-contexts (make-vector 255 0)
  "Obarray of copyedit contexts.")

(defmacro define-copyedit-context (key &rest forms)
  "Define KEY as a context tested by FORMS.
Forms are evaluated inside `lambda'."
  (declare (indent 1))
  `(fset
    (intern (symbol-name ',key) copyedit-contexts)
	    (lambda ()
	      ,@forms)))

(define-copyedit-context all
  "Return t in all contexts."
  t)

(define-copyedit-context bol
  "Return t at beginning of line.
Return t after beginning of line and whitespace.
When `auto-fill-function' is non-nil, only return t only after a blank line."
  (labels ((this-bolp ()
		      (or (bolp)
			  (save-excursion
			    (backward-char 1)
			    (bolp)))))
    (if auto-fill-function
	(and (this-bolp)
	     (copyedit-lastblank-p))
      (this-bolp))))

(define-copyedit-context boi
  "Return t at beginning of indentation."
  (let ((boi (c-point 'boi))
	(bol (c-point 'bol)))
    (and (not (equal boi bol))
	 (or (equal (point) boi)
	     (< (point) boi)))))

(define-copyedit-context eol
  "Return t at end of line.
Return t before whitespace and end of line.
If `auto-fill-function' is non-nil, return t only before a blank line."
  (labels ((this-eolp ()
		      (or (eolp)
			  (looking-at copyedit-blank-re))))
    (if auto-fill-function
	(and (this-eolp)
	     (copyedit-nextblank-p))
      (this-eolp))))

(define-copyedit-context in-sentence
  "Return t inside a sentence.
\"Inside a sentence\" means at neither the end or beginning of a line."
  (copyedit-inside-thing-p 'sentence))

(define-copyedit-context bob
  "Return t at beginning of buffer.
Return t after beginning of buffer and whitespace."
  (or (bobp)
      (save-excursion
	(backward-copyedit-ws)
	(bobp))))

(define-copyedit-context eob
  "Return t at end of buffer.
Return t before whitespace and end of buffer."
  (or (eobp)
      (save-excursion
	(forward-copyedit-ws 1)
	(eobp))))

(define-copyedit-context stop
  "Return the last relevant stop, if there is one."
  (copyedit-get-stop))

(define-copyedit-context quote
  "Return quote at point."
  (thing-at-point 'quote))

(define-copyedit-context eos
  "Return t at end of scene."
  (or (eobp)
      (and
       (looking-at copyedit-scene-re)
       (or (save-excursion
	     (forward-line -1)
	     (looking-at copyedit-scene-re))
	   (save-excursion
	     (forward-line 1)
	     (looking-at copyedit-scene-re))))))

(define-copyedit-context thisblank
  "Return t if this line is blank."
  (copyedit-thisblank-p))

(define-copyedit-context nextblank
  "Return t if next line is blank."
  (copyedit-nextblank-p))

(define-copyedit-context lastblank
  "Return t if last line is blank."
  (copyedit-lastblank-p))

(define-copyedit-context bothblank
  "Return t if next and last lines are both blank."
  (and (copyedit-nextblank-p)
       (copyedit-lastblank-p)))

(define-copyedit-context allblank
  "Return t if this line, the last line, and the next line are all blank."
  (and (copyedit-nextblank-p)
       (copyedit-lastblank-p)
       (copyedit-thisblank-p)))

(define-copyedit-context noblank
  "Return t if neither this nor the next lines are blank."
  (and (null (copyedit-thisblank-p))
       (null (copyedit-nextblank-p))))

(define-copyedit-context region
  "Return t if region is active and appropriate to use."
  (use-region-p))

(define-copyedit-context between-words
  "Return t when between words."
  (and (not (copyedit-after-syntax-p ?.))
       (not (eolp))
       (not (copyedit-before-syntax-p ?.))))

(define-copyedit-context before-word
  "Return t if next character is part of a word."
  (copyedit-before-syntax-p ?w))

(define-copyedit-context inside-word
  "Return t if inside a word.
Being inside a word means neither after its last nor before its first character."
  (copyedit-inside-thing-p 'word))

(define-copyedit-context noword
  "Return it if there is no word at point."
  (not (c-safe (thing-at-point 'word))))

(define-copyedit-context after-point
  "Return t if last character is a punctuation mark."
  (copyedit-after-syntax-p ?.))

(define-copyedit-context before-point
  "Return t if next character is a punctuation mark."
  (copyedit-before-syntax-p ?.))

(define-copyedit-context space-before-point
  "Return t if a space precedes a point."
  (save-excursion
    (or
     (and
      (copyedit-before-syntax-p ?.)
      (backward-copyedit-ws))
     (and
      (forward-copyedit-ws 1)
      (copyedit-before-syntax-p ?.)))))

(define-copyedit-context before-ws
  "Return t is next character is whitespace."
  (copyedit-before-syntax-p ? ))

(define-copyedit-context after-ws
  "Return t if last character is whitespace."
  (copyedit-after-syntax-p ? ))

(define-copyedit-context before-quote
  "Return t if next character opens a quote."
  (and (not (quote-at-point))
       (copyedit-before-syntax-p ?\")))

(define-copyedit-context boq
  "Return t at beginning of a quote."
  (let ((boq (c-safe (quote-beginning-position))))
    (if boq (or (eq (point) boq)
		(eq (save-excursion
		      (backward-copyedit-ws)
		      (point))
		    boq)))))

(define-copyedit-context eoq
  "Return t at end of a quote."
  (let ((eoq (c-safe
	       (quote-end-position))))
    (if eoq (eq (point) eoq))))

(define-copyedit-context last-word
  "Return t if at last word in a sentence."
  (save-excursion
    (and (forward-word 1)
	 (looking-at (sentence-end)))))

(define-copyedit-context one-word-sentence
  "Return t if context is a one-word sentence."
  (save-excursion
    (and
     (eq
      (c-safe (beginning-of-thing 'word))
      (c-safe (beginning-of-thing 'sentence)))
     (and
      (forward-word 1)
      (looking-at (sentence-end))))))

(define-copyedit-context indent
  "Return t if this paragraph is indented."
  (save-excursion
    (back-to-indentation)
    (not (zerop (current-column)))))

(define-copyedit-context after-space
  "Return t is last character is whitespace."
  (copyedit-after-syntax-p ? ))

(define-copyedit-context before-space
  "Return t if next character is whitespace."
  (copyedit-before-syntax-p ? ))

(define-copyedit-context before-stray-point
  "Return t if there is a stray point ahead."
  (save-excursion
    (with-copyedit-syntax
     (and
      (not (zerop (skip-syntax-forward " ")))
      (not (zerop (skip-syntax-forward ".")))
      (not (zerop (skip-syntax-forward " ")))))))

(define-copyedit-context after-stray-point
  "Return t if there is a stray point ahead."
  (save-excursion
    (with-copyedit-syntax
     (and
      (not (zerop (skip-syntax-backward " ")))
      (not (zerop (skip-syntax-backward ".")))
      (not (zerop (skip-syntax-backward " ")))))))

;;; Context macros

(defun copyedit-keyword-name (keyword)
  "Return name (sans colon) of keyword KEYWORD."
  (and (keywordp keyword)
       (let ((name (symbol-name keyword)))
	 (substring name 1 (length name)))))

(defun in-context-p (context)
  "Test for a CONTEXT from `copyedit-contexts'."
  (not (not (funcall (intern (symbol-name context) copyedit-contexts)))))

(defmacro in-one-context (&rest args)
  "Test for context and evaluate appropriate form.
ARGS should alternate between functions from `copyedit-contexts',
as a keyword, and a form to evaluate in that context."
  (let (forms)
    ;; replace nil with (ignore)
    (setq args
	  (mapcar (lambda (arg)
		    (if arg arg '(ignore)))
		  args))
    ;; build a list of conditions
    (while args
	(let* ((keyword (pop args))
	       (value (pop args))
	       (func (symbol-function
		      (intern (copyedit-keyword-name keyword) copyedit-contexts))))
	  (push (cons (list func) (list value)) forms)))
    ;; build cond form
    (setq forms (cons 'cond (nreverse forms)))
    `,@forms))

(defmacro in-each-context (&rest args)
  "Evaluate all forms in relevant contexts.
ARGS should alternate between functions from `copyedit-contexts',
as a keyword, and a form to evaluate in that context."
  (let (forms)
    ;; replace nil with (ignore)
    (setq args
	  (mapcar (lambda (arg)
		    (if arg arg '(ignore)))
		  args))
    ;; build a series of if-forms
    (while args
      (let* ((keyword (pop args))
	     (value (pop args))
	     (func (symbol-function
		    (intern (copyedit-keyword-name keyword) copyedit-contexts))))
	(push (list 'when (list func) value) forms)))
    ;; build a progn form
    (setq forms (cons 'progn (nreverse forms)))
    `,@forms))

(defmacro out-of-context (&rest args)
  "Evaluate all forms not in stated contexts.
ARGS should alternate between keywords from
`copyedit-contexts' and a form to evaluate in that context."
  (let (forms)
    ;; replace nil with ignore
    (setq args
	  (mapcar (lambda (arg)
		    (if arg arg '(ignore)))
		  args))
    ;; build a series of unless-forms
    (while args
      (let* ((keyword (pop args))
	     (value (pop args))
	     (func (symbol-function
		    (intern (copyedit-keyword-name keyword) copyedit-contexts))))
	(push (list 'unless (list func) value) forms)))
    ;; build a progn form
    (setq forms (cons 'progn (nreverse forms)))
    `,@forms))

;;; Utility functions

(defun backward-copyedit-ws ()
  "Skip to beginning of whitespace."
  (interactive "p")
  (forward-copyedit-ws -1))
  
(defun forward-copyedit-ws (arg)
  "Skip to end of whitespace; with negative ARG, to beginning."
  (interactive "p")
  (with-copyedit-syntax
    (not (zerop
	  (if (natnump arg)
	      (skip-syntax-forward " ")
	    (skip-syntax-backward " "))))))

(defun copyedit-zap (thing)
  "Kill all text between here and end of THING."
  (kill-region (point) (end-of-thing thing)))

(defun copyedit-zap-backward (thing)
  "Kill all text between here and beginning of THING."
  (kill-region (point) (beginning-of-thing thing)))

(defun copyedit-nuke (thing)
  "Kill THING."
  (kill-region (beginning-of-thing thing) (end-of-thing thing)))

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
	(forward-copyedit-ws 1)
	(point))
      (save-excursion
	(end-of-thing 'paragraph)
	(backward-copyedit-ws)
	(point)))
     ,@body))

;;; Test functions

(defun copyedit-eobp ()
  "Test for end of buffer."
  (or (eobp)
      (save-excursion
	(forward-copyedit-ws 1)
	(eobp))))

(defmacro with-copyedit-syntax (&rest body)
  "Run BODY with copyedit syntax table."
  `(with-syntax-table (copyedit-extended-syntax-table)
     ,@body))

(defun copyedit-extended-syntax-table ()
  "Return an extended version of the current syntax table."
  (let ((table (copy-syntax-table)))
    (modify-syntax-entry (decode-char 'ucs #x201C) "\"" table)
    (modify-syntax-entry (decode-char 'ucs #x201D) "\"" table)
    (modify-syntax-entry (decode-char 'ucs #x2018) "\"" table)
    (modify-syntax-entry (decode-char 'ucs #x2019) "\"" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "\"" table)
    ;; en-dash
    (modify-syntax-entry (decode-char 'ucs #x2013) " " table)
    ;; em-dash
    (modify-syntax-entry (decode-char 'ucs #x2014) " " table)
    (modify-syntax-entry ?-  "-" table)
    (modify-syntax-entry ?\t "-" table)
    (modify-syntax-entry ?\n "-" table)
    (modify-syntax-entry ?\r "-" table)
    (modify-syntax-entry ?\f "-" table)
    (modify-syntax-entry ?\v "-" table)
    table))

(defun copyedit-before-syntax-p (syntax)
  "Return t when next char has syntax SYNTAX."
  (unless (eobp)
    (with-copyedit-syntax (= (char-syntax (char-after)) syntax))))

(defun copyedit-after-syntax-p (syntax)
  "Return t when last char has syntax SYNTAX."
  (unless (bobp)
    (with-copyedit-syntax (= (char-syntax (char-before)) syntax))))

(defun copyedit-get-stop ()
  "Return non-nil if point is at a stop."
  (with-copyedit-syntax
   (save-excursion
     (backward-copyedit-ws)
     (skip-syntax-backward ".")
     (let ((sentence-end-double-space nil))
      (if (looking-at (sentence-end))
	  (char-to-string (char-after))
	nil)))))

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

(defun forward-copyedit-scene ()
  "Go to next scene break or end of buffer."
  (interactive)
  (forward-line 1)
  (until (or (eobp)
	     (in-context-p 'eos))
    (forward-char 1))
  (while (in-context-p 'eos) (forward-char 1)))

(defun backward-copyedit-scene ()
  "Go to previous scene break or beginning of buffer."
  (interactive)
  (forward-line -1)
  (until (or (bobp)
	     (in-context-p 'eos))
    (forward-char -1))
  (while (in-context-p 'eos) (forward-char -1)))

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
	    (prog1
		(not (zerop (with-copyedit-syntax (skip-syntax-backward "^\""))))
		(c-safe (forward-char -1)))
	    (in-one-context
	     :bob		(point)
	     :bol		(point)
	     :after-space	(point)
	     :all               nil))))
	(close-quote
	 (save-excursion
	   (and
	    (prog1
		(not (zerop (with-copyedit-syntax (skip-syntax-forward "^\""))))
		(c-safe (forward-char 1)))
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

(defun copyedit-stop ()
  "Insert a stop with one or more spaces."
  (copyedit-delete-horizontal-space)
  (copyedit-depunctuate-backward)
  ;; if inside a save-stop form, use this-stop.
  (let ((stop (or *copyedit-this-stop* "."))
	(space (if sentence-end-double-space
		   "  " " ")))
    (in-one-context
     :boi               nil
     :bol               nil
     :after-point	(insert space)
     :eoq		(insert stop)
     :eol		(insert stop)
     :all		(insert (concat stop space)))))

(defun copyedit-find-space (&optional arg)
  "Go to the next space.  With ARG, ignore space at point."
  (interactive "P")
  (if arg (skip-syntax-forward " "))
  (in-one-context
   :inside-word (skip-syntax-forward "^ ")))

(defun copyedit-find-space-backward (&optional arg)
  "Go to the last space.  With ARG, ignore space at point."
  (interactive "P")
  (if arg (skip-syntax-backward " "))
  (skip-syntax-backward "^ "))

(defun copyedit-delete-next-space ()
  "Delete next horizontal space."
  (interactive)
  (copyedit-find-space)
  (in-one-context
   :eol (copyedit-nuke 'copyedit-ws)
   :all (copyedit-delete-horizontal-space)))

(defun copyedit-delete-horizontal-space ()
  "Delete all whitespace around point on this line."
  (with-copyedit-syntax
   (delete-region
    (max (line-beginning-position)
	 (save-excursion
	   (skip-syntax-backward " ")
	   (point)))
    (min (line-end-position)
	 (save-excursion
	   (skip-syntax-forward " ")
	   (point))))))

(defun copyedit-just-one-space ()
  "Remove whitespace around point, and insert a space if necessary.
Don't touch whitespace characters besides spaces or tabs."
  (interactive)
  (with-copyedit-syntax
   (skip-syntax-backward " ")
   (delete-horizontal-space)
   (skip-syntax-forward " ")
   (delete-horizontal-space)
   (unless 
       (or
	(out-of-context :eol (copyedit-before-syntax-p ? ))
	(out-of-context :bol (copyedit-after-syntax-p ? )))
     (insert " "))))

(defun copyedit-depunctuate-forward ()
  "Remove punctuation after point."
  (save-excursion
    (forward-copyedit-ws 1)
    (let ((start (point))
	  (end (point)))
      (save-excursion
	(unless (zerop (skip-syntax-forward "."))
	  (setq end (point))))
      (delete-region start end)))
  (in-one-context :bol (copyedit-delete-horizontal-space)))

(defun copyedit-depunctuate-backward ()
  "Remove punctuation before point."
  (save-excursion
    (backward-copyedit-ws)
    (let ((start (point))
	  (end (point)))
      (save-excursion
	(unless (zerop (skip-syntax-backward "."))
	  (setq end (point))))
      (delete-region start end))))

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
   :all (copyedit-just-one-space))
  (backward-copyedit-ws))

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
   :stop		(restrict-to-paragraph
			 (save-stop
			  (copyedit-stop)
			  (copyedit-capitalize)))
   :before-stray-point  (copyedit-depunctuate-forward)
   :after-stray-point   (copyedit-depunctuate-backward)
   :space-before-point  (copyedit-delete-horizontal-space)
   :before-point        (in-one-context :after-point
					(copyedit-depunctuate-backward))))

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
  (out-of-context
   :stop (backward-copyedit-sentence 1))
  (mark-end-of-sentence 1))

(defun copyedit-open-sentence ()
  "End the previous sentence here, fixing capitalization and spacing."
  (interactive)
  (in-one-context
   :in-sentence (progn
		  (copyedit-depunctuate)
		  (copyedit-stop)
		  (copyedit-capitalize))))

(defun kill-copyedit-sentence ()
  "Like `kill-sentence' with ARG, but insert a stop and spaces after."
  (interactive)
  (kill-region
   (point)
   (progn
     (forward-sentence 1)
     (with-copyedit-syntax
      (skip-syntax-backward "^.")
      (skip-syntax-backward "."))
     (point)))
  (copyedit-fixup)
  (in-one-context
   :boi (kill-region (point) (progn (forward-copyedit-ws 1) (point)))))

(defun backward-kill-copyedit-sentence ()
  "Like `backward-kill-sentence', but insert stop and fix caps after."
  (interactive)
  (kill-region
   (point)
   (progn
     (forward-sentence -1)
     (forward-copyedit-ws 1)
     (with-copyedit-syntax
      (skip-syntax-forward "^w"))
     (point)))
  (copyedit-fixup))

(defun kill-whole-copyedit-sentence ()
  "Kill an entire sentence."
  (interactive)
  (kill-region
   (progn (forward-sentence -1) (forward-copyedit-ws 1) (point))
   (progn (forward-sentence 1) (point)))
  (copyedit-fixup))

(defun copyedit-join-sentence ()
  "Join two sentences, lowercasing as appropriate."
  (interactive)
  (forward-sentence 1)
  (copyedit-depunctuate)
  (copyedit-nuke 'copyedit-ws)
  (copyedit-just-one-space)
  (save-excursion (downcase-word 1)))

(defun backward-copyedit-word (arg)
  "Like `backward-word' with ARG.
When arg = 1, point is always left before or after word."
  (interactive "p")
  (with-copyedit-syntax
   (if (eq arg 1)
       (and (zerop (skip-syntax-backward "w"))
	    (zerop (skip-syntax-backward "^w"))
	    (backward-word 1))
     (backward-word arg))))
   
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
  (forward-line -1)
  (forward-paragraph -1)
  (forward-copyedit-ws 1))

(defun backward-kill-copyedit-word ()
  "Like `kill-word', but fix up afterwards."
  (interactive)
  (with-copyedit-syntax
   (skip-syntax-backward "^w"))
  (kill-word -1)
  (in-one-context
   :bol nil
   :eol nil
   :boi (progn (delete-char -1) (forward-copyedit-ws 1))
   :all (copyedit-just-one-space))
  (copyedit-fixup))

(defun forward-copyedit-word (arg)
  "Like `forward-word' with ARG.
When arg = 1, point is always left before or after word."
  (interactive "p")
  (with-copyedit-syntax
   (if (eq arg 1)
       (and (zerop (skip-syntax-forward "w"))
	    (zerop (skip-syntax-forward "^w"))
	    (forward-word 1))
     (forward-word arg))))

(defun forward-copyedit-sentence (arg)
  "Like `forward-sentence' with ARG, but put point at beginning of sentence."
  (interactive "p")
  (forward-sentence arg)
  (out-of-context :eol (forward-copyedit-ws 1)))

(defun forward-copyedit-paragraph (arg)
  "Like `forward-paragraph' with ARG, but put point at beginning of paragraph."
  (interactive "p")
  (forward-paragraph arg)
  (forward-copyedit-ws 1))

(defun kill-copyedit-word ()
  "Like `kill-word', but fix up afterwards."
  (interactive)
  (with-copyedit-syntax
   (skip-syntax-forward "^w"))
  (kill-word 1)
  (in-one-context
   :bol nil
   :boi (progn (delete-char -1) (forward-copyedit-ws 1))
   :all (copyedit-just-one-space))
  (copyedit-fixup))

(defun kill-copyedit-paragraph ()
  "Kill whole paragraph and clean up afterwards."
  (interactive)
  (kill-region
   (progn (forward-paragraph -1) (forward-copyedit-ws 1) (point))
   (progn (forward-paragraph 1) (forward-copyedit-ws 1) (point))))

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
    (define-key map [remap forward-paragraph]		'forward-copyedit-paragraph)
    (define-key map [remap backward-kill-word]		'backward-kill-copyedit-word)
    (define-key map [remap backward-word]		'backward-copyedit-word)
    (define-key map [remap backward-sentence]		'backward-copyedit-sentence)
    (define-key map [remap backward-paragraph]		'backward-copyedit-paragraph)
    (define-key map [remap kill-sentence]		'kill-copyedit-sentence)
    (define-key map [remap kill-whole-line]		'kill-copyedit-paragraph)
    (define-key map [remap delete-horizontal-space]	'copyedit-delete-next-space)
    (define-key map [remap just-one-space]	        'copyedit-just-one-space)
    (define-key map [remap delete-blank-lines]		'copyedit-delete-blank-lines)
    (define-key map [remap backward-page]		'backward-copyedit-scene)
    (define-key map [remap forward-page]		'forward-copyedit-scene)
    map))

(defvar copyedit-extra-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map copyedit-keymap)
    (define-key map (kbd "C-M-y")		'copyedit-yank-clean)
    (define-key map (kbd "C-j")			'copyedit-join-sentence)
    (define-key map (kbd "M-j")			'copyedit-mark-sentence)
    (define-key map (kbd "M-.")			'copyedit-open-sentence)
    (define-key map (kbd "M-,")			'copyedit-depunctuate)
    (define-key map (kbd "M-<backspace>")	'backward-kill-copyedit-sentence)
    (define-key map (kbd "C-\"")		'copyedit-kill-quote)
    (define-key map (kbd "C-'")                 'copyedit-kill-quote-forward)
    (define-key map (kbd "M-'")                 'copyedit-kill-quote-backward)
    (define-key map (kbd "M-<right>")		'copyedit-drag-right)
    (define-key map (kbd "M-<left>")		'copyedit-drag-left)
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
