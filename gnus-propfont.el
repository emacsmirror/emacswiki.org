;;; gnus-propfont.el ---  adding faces to *Article* buffer in Gnus 
;; (for GNU Emacs 21.1)
;; 
;; Time-stamp: <18.07.2002 -- 14.52h cest>

;; Copyright (C) 2001, 2002 Oliver Scholz

;; Author: Oliver Scholz <epameinondas@gmx.de>
;; Keywords: news, mail

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

;; This adds some faces to the *Article* buffer in Gnus. The main
;; purpose of this package is to display articles with a proportional
;; font. Hence the name "gnus-propfont".

;; Put the file into your load-path, compile it and insert the
;; following lines into your .gnus:

;; The defaults are according to *my* taste, so there's a good chance
;; that you won't like them.


;; (require 'gnus-propfont)
;; (add-hook 'gnus-article-prepare-hook 'gpf-add-faces)

;; You can customize this package via `M-x customize-group RET
;; gnus-propfont RET'.

;; The you can define a different face for each group. See the
;; documentation for the variable `gpf-groups-alist' for detaills.

;; The variables `gpf-exclude-header' `gpf-exclude-signature' controll
;; if the header resp. the signature should get a face, too.

;; It is possible to exclude certain parts of an article. See the
;; variable `gpf-excluded-parts-alist'. Currently a decent mechanism
;; exists only for lisp snipplets in Emacs related newsgroups.

;;; Acknowledgments: 

;;

;;; Code:

(eval-when-compile (require 'cl))

(defvar gpf-overlays nil
  "List of overlays in the *Article* buffer.")
(make-variable-buffer-local 'gpf-overlays)

(defgroup gnus-propfont nil
  "A package for adding faces to the *Article* buffer in Gnus."
  :group 'local)

(defcustom gpf-line-spacing 3
  "Line-spacing in *Article* buffer"
  :group 'gnus-propfont
  :type 'integer)

(defcustom gpf-groups-alist
  '((".*\\.sources.*" . gpf-no-face)
    (".*\\.ascii-art.*" . gpf-fixed-width-face)
    (".*" . gpf-variable-width-face))
  "Defines which faces to use for which groups.

KEY is a regexp matching the name of the group, VALUE specifies the
face. If the \"face's\" name is `gpf-no-face' no face is added for
the matching groups at all.  

Only the first match is used. So, if you want \"arial\" for all
microsoft-newsgroups, \"Old English\" for your fantasy- (or gothic-)
mailing-lists and \"Times New Roman\" for the rest, create the
specific faces in your .emacs -- for example:

\(defface my-special-gnus-face
  `((((type x w32 mac))
     (:family \"microsoft-comic sans ms\" :height 140)))
 \"Face that I use to read mails from my boss.\"
  :group 'gnus-propfont)

-- and adjust this alist accordingly:

\".*microsoft.*\" --> my-arial-face
\".*gothic.*\\\\|.*fantasy.*\" --> my-gothic-face
\".*\" --> my-times-face

Note that if there is any group \"microsoft.gothic\", it will show up
in my-arial-face -- *not* in my-gothic-face. Adjust the regexp or
change the order to avoid this."
  :group 'gnus-propfont
  :type '(alist :key-type regexp :value-type face))

(let ((lisp-groups (regexp-opt
		    '("de.comm.software.gnus" "comp.emacs" "gnu.emacs.help"
		      "gnu.emacs.gnus" "de.comp.editoren" "comp.lang.lisp"))))
  (defcustom gpf-excluded-parts-alist
    `((,lisp-groups . gpf-find-lisp)
      (,lisp-groups . gpf-find-boxquote)
      ("comp\\.lang\\.lisp" . gpf-find-cl-prompt))

    "Defines which parts should not be \"facified\" in which group.

The key is a regexpr matching a group-name. The value names a function
that determines the part of the buffer to _exclude_. Using more than
one entry in the alist, it is possible to define several functions for
a single group.

The function `gpf-search' scans the buffer line-wise and calls each
function on each line. Each function should check for if the line is
the beginning of a text portion to be excluded. If it is, it should
move point to the end of that text portion and return beginning and
end of that part in a cons cell. Otherwise it should return nil and
don't move point.

If you want to use siply two regexps to determine such text portions,
you can use the function `gpf-find-regexps' for this purpose.

Thus, to exclude all parts beginning with \"Begin lirum larum\" and
ending with \"End lirum larum\" in a newsgroup called
\"alt.lirum.larum\" you could add \"alt\\.lirum\\.larum\" as a regexp and 

\(lambda ()
   (gpf-find-regexps \"^Begin lirum larum\"
		     \"^End lirum larum\"))

as a function to this alist."
    :group 'gnus-propfont
    :type '(alist :key-type regexp :value-type function)))

(defface gpf-no-face
  nil
  "Bogus face, used to omit fontification at all.")

(defface gpf-variable-width-face
  '((((type x w32 mac))
     (:family "adobe-new century schoolbook" :height 180)))
  "Basic face with variable width font."
  :group 'gnus-propfont)

(defface gpf-fixed-width-face
  '((((type x w32 mac)) 
     (:family "b&h-lucidatypewriter" :height 140)))
  "Basic face with fixed width font."
  :group 'gnus-propfont)

(defcustom gpf-exclude-header t
  "If non-nil, exclude header from being \"facified\"."
  :group 'gnus-propfont
  :type 'boolean)

(defcustom gpf-exclude-signature t
  "If non-nil, exclude signature from being beeing \"facified\"."
  :group 'gnus-propfont
  :type 'boolean)


(defmacro* gpf-define-state-machine (name (&key args let value)  &rest states)
  "Define a simple state machine.

If NAME is non-nil, this macro expands to a function definition,
providing a callable function NAME. If NAME is nil, it expands to a
lambda expression.

The second argument is a list of keywords:

    :args
    A list of symbols that form the arguments of the resulting
    function.

    :let
    A list of let bindings for the resulting function. 

    :value
    The return value of the function. This can be any valid sexpr.

This macro defines states as a list whose car is the name of the state
\(a symbol) followed by one ore more sexpr. The sexpr are
evaluated. Each state must return it's successor state.

Each state machine must have a state named `start'. Which is the first
state after a call to the resulting function. To exit the state
machine a state must return the symbol `exit', which -- so to say --
indicates a (virtual) exit state.

Example:

\(gpf-define-state-machine egoge-test-count (:args (beg end)
					    :let ((tmp nil)
						  count)
					    :value (list beg end tmp))
  (start (setq count beg)
	 'test)

  (test (if (> count end)
	    'exit
	  'add-to-results))

  (add-to-results (push count tmp)
		  'next)

  (next (setq count (1+ count))
	'test))



\(egoge-test-count 5 10)

 ==> (5 10 (10 9 8 7 6 5))"
  ;; First some checking for obvious errors.
  (unless (member 'start (mapcar 'first states))
    (error "`%s' does not provide a starting state." (or name "State-machine")))
  (let* ((exit (gensym))
	 (state (gensym))
	 (first (if name `(defun ,name) '(lambda))))
    `(,@first ,args
       (let (,@let (,state 'start))
	 (catch ',exit
	   (while t
	     (setq ,state (case ,state ,@states))
	     (when (eq ,state 'exit)
	       (throw ',exit ,value))))))))
  
(defun gpf-add-faces ()
  "Add face to *Article* buffer.
The variable `gpf-groups-alist' specifies, which face to use -- if
any.  The variable `gpf-excluded-parts-alist' specifies which parts of
the *Article* buffer this function should _not_ \"facify\"."
  (let ((face (cdr (find-if (lambda (elt)
			      (string-match (car elt) gnus-newsgroup-name))
			    gpf-groups-alist))))
    ;; Add the face. `begin' is either (point-min) or the end of the
    ;; header, `end' is either (point-max) or the beginning of the
    ;; signature:
    (unless (eq face 'gpf-no-face)
      (let ((begin (if gpf-exclude-header
		       (1+ (rfc822-goto-eoh))
		     (point-min)))
	    (end (if (and gpf-exclude-signature
			  (save-excursion
			    (re-search-forward "^-- $" nil t)))
		     (match-beginning 0)
		   (point-max)))
	    ;; Get a list of "exclusion functions":
	    (excl (delq nil
			(mapcar
			 (lambda (elt)
			   (if (string-match (car elt) gnus-newsgroup-name)
			       (cdr elt)
			     nil))
			 gpf-excluded-parts-alist))))
	(gpf-add-faces-overlays begin end face excl)))))

(defsubst gpf-make-overlays (funcs)
  "Return a list of overlays.
FUNCS is a list of functions that return buffer positions in a cons
cell each, indicating parts of the buffer that should _not_ be covered
by an overlay."
  (save-excursion
    (goto-char (point-min))
    (let ((overlays nil)
	  (excl-part nil)
	  (beg (point))
	  (end nil))
      (catch 'search-exit
	(while t
	  (if (setq excl-part (gpf-search funcs))
	      (progn	;(when (< (- (car excl-part) beg) 2) (edebug))
		     (setq end (car excl-part))
		     (unless (< (- end beg) 2)
		       (push (make-overlay beg end) overlays))
		     (setq beg (cdr excl-part)))
	    (push (make-overlay beg (point-max)) overlays)
	    (throw 'search-exit overlays))
	  (when (eobp)
	    (throw 'search-exit overlays)))))))

(defun gpf-add-faces-overlays (begin end face &optional funcs)
  "Put FACE on the text from BEGIN to END.
The optional argument FUNCS is a list of functions that return buffer
positions in a cons cell each, indicating parts of the buffer that
should _not_ be covered by an overlay."
  ;; delete existing overlays
  (when gpf-overlays
    (mapc 'delete-overlay gpf-overlays))
  ;; create overlays
  (if (null funcs)
      (setq gpf-overlays (list (make-overlay begin end)))
    (setq gpf-overlays
	  (save-restriction
	    (narrow-to-region begin end)
	    (gpf-make-overlays funcs))))
  ;; Put faces on the overlays.
  (mapc (lambda (overlay)
	  (overlay-put overlay 'face face))
	gpf-overlays)
  ;; Adjust line spacing.
  (when gpf-line-spacing
    (setq line-spacing gpf-line-spacing)))


(gpf-define-state-machine gpf-search (:args (funcs) :let ((part)) :value part)
  ;; This is the main searching routine, scanning for parts of the
  ;; buffer to be excluded from adding the face.
  (start 'examine-funcs)
  (examine-funcs (if (and funcs
			  (setq part (some 'funcall funcs)))
		     'exit
		   'end-of-buffer?))
  (end-of-buffer? (if (eobp)
		      'exit
		    'next-line))
  (next-line (forward-line 1)
	     'examine-funcs))

(defun gpf-find-regexps (regexp1 regexp2 &optional match1 match2)
  "Find a part of the buffer.

If the current line matches REGEXP1, move point to the next line
matching REGEXP2. Return the position of the part matching REGEXP1 and
the end of the part matching REGEXP2 in a cons cell then. If the
current line does not match REGEXP1, return nil and don't move point.

The optional arguments MATCH1 and MATCH2 allow to specify
sub-expressions instead of the whole matches for the return value."
  (let ((beg (and (looking-at regexp1)
		  (match-beginning (or match1 0)))))
    (if beg
	(if (re-search-forward regexp2 nil t)
	    (cons beg (match-end (or match2 0)))
	  (goto-char (point-max))
	  (cons beg (point-max)))
      nil)))

(defun gpf-find-boxquote ()
  (gpf-find-regexps "^,----" "^`----"))

(defun gpf-find-cl-prompt ()
  (gpf-find-regexps "^[ \t]*\\(?:* (\\|\\[[0-9]+\\]>\\)"
		    "^[ \t]*$"))

(defconst gpf-lisp-keyword-regexp
  (concat "^[ \t]*\\(?:"
	  (regexp-opt '("(defun" "(defmacro" "(defvar" "(defparameter"
			"(defsubst" "(defstruct" "(defclass" "(defmethod"
			"(defgeneric" "(setq" "(setf" "(with-" "(eval-"
			"(defcustom" "(defgroup" "(defadvice"
			"(setenv" "(mapcar" "(mapc" "(concat"
			"(let (" "(let* (" "(autoload"))
	  "\\)"))

(defconst gpf-lisp-conditional-loop-regexp
  (concat "^[ \t]*\\(?:"
	  (regexp-opt '("(if " "(when " "(unless " "(while " "(dolist "
			"(dotimes " "(loop "))
	  "\\)"))

(gpf-define-state-machine gpf-find-lisp (:let ((part)) :value part)
    ;; If there is a "(" or a ";;" at the beginning of a line, it is
    ;; _possibly_ a sexpr.
    (start (if (looking-at "^[ \t]*\\(?:(\\|;;\\)")
		    'check-comment
		  'exit))

    ;; Check for a comment line.
    (check-comment (if (looking-at "^[ \t]*;;")
		       'found-comment
		     'check-keywords))

    ;; Now go through a number of tests until either one of the tests
    ;; matches or until we can be sure that it is _not_ a sexpr.
    
    ;; First check for surely lispy words like "defun", "setq",
    ;; "defvar" etc. and for ";;".
    (check-keywords (if (looking-at gpf-lisp-keyword-regexp)
			'found-sexp
		      'check-dashes))

    ;; But perhaps the first symbol/word after the paren has "-" in it?
    (check-dashes (if (looking-at "^[ \t]*(\\w*-\\w*")
		      'found-sexp
		    'check-insert))

    ;; Is it an `insert'?
    (check-insert (if (looking-at "^[ \t]*(insert ")
		      'check-insert-2
		    'check-require))

    ;; Perhaps the second argument starts with a paren?
    (check-insert-2 (if (looking-at "^[ \t]*(insert (")
			'found-sexp
		      'check-insert-3))

    ;; Additional checking for `insert'.  
    (check-insert-3 (looking-at "^[ \t]*(insert \\(.*?\\)\\(?:)\\|$\\)")
		    ;; `(match-string 1)' is -- after this match --
		    ;; supposed to contain the arguments to `insert'.
		    (let ((args (split-string (match-string 1) " +"))
			  (count 0))
		      ;; Test the "arguments".  They have to start
		      ;; with a `"' (being strings) or with a `?'
		      ;; (being charakters). Else the expression is
		      ;; natural English.
		      (if (every (lambda (elt)
				   (or (string= (substring elt 0 1) "\"")
				       (string= (substring elt 0 1) "?")))
				 args)
			  'found-sexp
			'check-require)))

    ;; The first charakter after a "require" must be a `''.
    (check-require (if (looking-at "^[ \t]*(require \\(?:'\\|(quote \\)")
		       'found-sexp
		     'check-load))

    ;; A "load" is followed by a string.
    (check-load (if (looking-at "^[ \t]*(load \"")
		    'found-sexp
		  'check-form))

    ;; Look for "while", "if", "unless", "when" or similar. But
    ;; because those words are very common in English (even after a
    ;; paren), we need additional testing in `check-form-2'.
    (check-form (if (looking-at gpf-lisp-conditional-loop-regexp)
		      'check-form-2
		    'failure))

    ;; Check if either a parenthized expression or a symbol with
    ;; dashes follows after a "while", "if".
    (check-form-2 (if (looking-at "^[ \t]*(.* \\(?:(\\|[a-z]+-[a-z]+\\)")
		      'found-sexp
		    'check-form-3))

    ;; Check if the second line after a "when", "if" etc. is indented
    ;; and starts with a paren.
    (check-form-3 (if (save-excursion
			(forward-line 1)
			(looking-at "^[ \t]+("))
		      'found-sexp
		    'failure))

    (failure (setq part nil) ; just to make it explicit.
	     'exit)

    (found-sexp (unless part (setq part (cons (point) nil)))
		'find-end)

    (found-comment (unless part (setq part (cons (point) nil)))
		   'find-comment-end)

    (find-comment-end (forward-line 1)
		      (if (and (not (eobp))
				    (looking-at "^;"))
			  'find-comment-end
			'success))

    ;; Search the end of the Lisp part.
    (find-end (if (condition-case nil
		      (forward-sexp)
		    (error t))
		  'search-empty-line
		'more?))

    (more? (if (looking-at "[ \t]*;")
	       'find-comment-end
	     'success))

    (search-empty-line (if (or (eobp)
			       (looking-at "^[ \t]*$"))
			   'success
			 (forward-line 1)
			 'search-empty-line))

    (success (setcdr part (point))
	     'exit))
			 


;;; commands

;; (defun gpf-change-font-size (prefix)
;;   "Increase/decrease the height of the font in *Article* buffer temporarily.
;; PREFIX defines how much steps to go. Use the 'display
;; text-property. If prefix is 0, remove the 'display text-property in
;; toto."
;;   (interactive "p")
;;   (let ((inhibit-read-only t))
;;     (or prefix (setq prefix 0))
;;     (save-excursion
;;       (set-buffer (get-buffer "*Article*"))
;;       (cond
;;        ((> prefix 0)
;; 	(put-text-property (point-min) (point-max) 'display `(height (+ ,prefix))))
;;        ((< prefix 0)
;; 	(put-text-property (point-min) (point-max) 
;; 			   'display `(height (- ,(* -1 prefix)))))
;;        ((= prefix 0)
;; 	(remove-text-properties (point-min) (point-max) '(:display nil)))))))


;; (defun gpf-remove-overlay ()
;;   (interactive)
;;   (save-excursion
;;     (set-buffer "*Article*")
;;     (when gpf-overlays
;;       (mapc 'delete-overlay gpf-overlays)
;;       (setq gropf-overlays nil))))


(provide 'gnus-propfont)


;;;Local Variables:
;;;eval:(put 'gpf-define-state-machine 'lisp-indent-hook 2)
;;;End:

;;; gnus-propfont.el ends here
