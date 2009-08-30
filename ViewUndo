;;; 2007-5-30
;;; Derek Peschel
;;; Peek at Emacs's undo state.

;;; TO DO: make view-undo not affect undo's idea of whether it is
;;; currently undoing or redoing

;;; ------------------------------------------------------------ helper fns.

(defun cdddr (lst) (cdr (cddr lst)))
(defun cadddr (lst) (cadr (cddr lst)))
(defun cddddr (lst) (cddr (cddr lst)))

(defun undo-elt-to-string (elt)
"Convert one element on an undo list to a string.
See the Emacs LISP manual for the patterns matched.  The string describes
what was done to create the element: \"i\" for insertion, \"d\" for deletion,
\"*\" for setting or updating the modification time, \"p\" for a property
change, \"m\" for marker motion, \"/\" for a command boundary, \"?\" for
anything else."
  (cond
   ;; this form records point movements; "." as in point
   ((integerp elt)		  ".")

   ;; this form records insertions; "i" for insert
   ((and (listp elt)
	 (integerp (car elt))
	 (integerp (cdr elt)))	  "i")

   ;; this form records deletions; "d" for delete
   ((and (listp elt)
	 (stringp (car elt))
	 (integerp (cdr elt)))	  "d")

   ;; this form records modification; "*" as used in mode line
   ((and (listp elt)
	 (listp (cdr elt))
	 (eq (car elt) t)
	 (integerp (cadr elt))
	 (integerp (cddr elt)))	  "*")

   ;; this form records text property changes; "p" for property
   ((and (listp elt)
	 (listp (cdr elt))
	 (listp (cddr elt))
	 (listp (cdddr elt))
	 (null (car elt))
	 ;; no type check for property = (cadr elt)
	 ;; no type check for value = (caddr elt)
	 (integerp (cadddr elt))
	 (integerp (cddddr elt))) "p")

   ;; this form records marker motions; "m" for marker
   ((and (listp elt)
	 (markerp (car elt))
	 (integerp (cdr elt)))	  "m")

   ;; this form records command boundaries; "|" too similar to "i"
   ((null elt)			  "/")

   (t				  "?")))

;;; ------------------------------------------------------------ main command

(defun undo-list-to-string (lst)
"Convert an undo list to a string.
See the Emacs LISP manual for the patterns matched.  \"-\" means no undo
information is being stored, a parenthesized string means undo information,
and \"?\" means anything else.	See `undo-elt-to-string' for the meanings
of the characters inside the parentheses."
  (cond
   ((eq lst t)	"-")

   ((listp lst) (concat "("
			(mapconcat 'undo-elt-to-string lst "")
			")"))

   (t		"?")))

(defun view-undo ()
"View the buffer undo information.

\"-\" means no undo information is being stored.

\"(...)\" means undo information is being stored, and the characters
in \"...\" (if any) describe the information.  The newest information
comes on the left.  Each character describes what was done (not how to
undo it).
	\"i\" means an insertion.
	\"d\" means a deletion.
	\"*\" means a buffer modification or an update of the time.
	\"p\" means a property change.
	\"m\" means a marker motion.
	\"/\" means a command boundary.
	\"?\" means anything else.

\"?\" means anything else."
  (interactive)
  (message (undo-list-to-string buffer-undo-list)))

;;; ------------------------------------------------------------ key binding

;; assume ctrl-_ which is ctrl-dash for undo; F11 near dash on my keyboard

(global-set-key [f11] 'view-undo)
