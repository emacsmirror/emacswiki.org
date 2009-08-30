;;; replace-eval-xmas.el -- Xemacs port of gnu emacs query-replace-* functions.
;;
;;  This file is free software
;;
;;; Commentary:
;;
;;  This hack is a partial xemacs port of gnu emacs query-replace-* functions,
;;  that make possible to use lisp expressions in replacement. 
;;  Ported functions:
;;   query-replace-regexp, query-replace-regexp-eval, 
;;   dired-do-tags-query-replace, tags-query-replace.
;;  Function dired-replace-global is a small hack for dired-do-tags-query-replace
;;  to replace all matches in all files with no questions.
;;
;;; Code:


(defun query-replace-read-from (prompt regexp-flag)
  "Query and return the `from' argument of a query-replace operation.
The return value can also be a pair (FROM . TO) indicating that the user
wants to replace FROM with TO."
  (if query-replace-interactive
      (car (if regexp-flag regexp-search-ring search-ring))
    (let ((from
	   ;; The save-excursion here is in case the user marks and copies
	   ;; a region in order to specify the minibuffer input.
	   ;; That should not clobber the region for the query-replace itself.
	   (save-excursion
	     (read-from-minibuffer
	      (format "%s: " prompt)
	      nil nil nil
	      'query-replace-history))))
      (and regexp-flag
	   (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
	   (let ((match (match-string 3 from)))
	     (cond
	      ((string= match "\\n")
	       (message "Note: `\\n' here doesn't match a newline; to do that, type C-q C-j instead"))
	      ((string= match "\\t")
	       (message "Note: `\\t' here doesn't match a tab; to do that, just type TAB")))
	     (sit-for 2)))
      from)))

(defun query-replace-compile-replacement (to regexp-flag)
  "Maybe convert a regexp replacement TO to Lisp.
Returns a list suitable for `perform-replace' if necessary,
the original string if not."
  (if (and regexp-flag
	   (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to))
      (let (pos list char)
	(while
	    (progn
	      (setq pos (match-end 0))
	      (push (substring to 0 (- pos 2)) list)
	      (setq char (aref to (1- pos))
		    to (substring to pos))
	      (cond ((eq char ?\#)
		     (push '(number-to-string replace-count) list))
		    ((eq char ?\,)
		     (setq pos (read-from-string to))
		     (push `(replace-quote ,(car pos)) list)
		     (let ((end
			    ;; Swallow a space after a symbol
			    ;; if there is a space.
			    (if (and (or (symbolp (car pos))
					 ;; Swallow a space after 'foo
					 ;; but not after (quote foo).
					 (and (eq (car-safe (car pos)) 'quote)
					      (not (= ?\( (aref to 0)))))
				     (eq (string-match " " to (cdr pos))
					 (cdr pos)))
				(1+ (cdr pos))
			      (cdr pos))))
		       (setq to (substring to end)))))
	      (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to)))
	(setq to (nreverse (delete "" (cons to list))))
	(replace-match-string-symbols to)
	(cons 'replace-eval-replacement
	      (if (cdr to)
		  (cons 'concat to)
		(car to))))
    to))


(defun query-replace-read-to (from prompt regexp-flag)
  "Query and return the `to' argument of a query-replace operation."
  (query-replace-compile-replacement
   (save-excursion
     (read-from-minibuffer
      (format "%s %s with: " prompt from)
      nil nil nil
      'query-replace-history))
   regexp-flag))

(defun query-replace-read-args (prompt regexp-flag &optional noerror)
  (unless noerror
    (barf-if-buffer-read-only))
  (let* ((from (query-replace-read-from prompt regexp-flag))
	 (to (if (consp from) (prog1 (cdr from) (setq from (car from)))
	       (query-replace-read-to from prompt regexp-flag))))
    (list from to current-prefix-arg)))

(defun query-replace-regexp (regexp to-string &optional delimited)
  "Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the
minibuffer.

Matching is independent of case if `case-fold-search' is non-nil and
REGEXP has no uppercase letters.  Replacement transfers the case
pattern of the old text to the new text, if `case-replace' and
`case-fold-search' are non-nil and REGEXP has no uppercase letters.
\(Transferring the case pattern means that if the old text matched is
all caps, or capitalized, then its replacement is upcased or
capitalized.)

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.

In TO-STRING, `\\&' stands for whatever matched the whole of REGEXP,
and `\\=\\N' (where N is a digit) stands for
whatever what matched the Nth `\\(...\\)' in REGEXP.
`\\?' lets you edit the replacement text in the minibuffer
at the given position for each replacement.

In interactive calls, the replacement text can contain `\\,'
followed by a Lisp expression.  Each
replacement evaluates that expression to compute the replacement
string.  Inside of that expression, `\\&' is a string denoting the
whole match as a string, `\\N' for a partial match, `\\#&' and `\\#N'
for the whole or a partial match converted to a number with
`string-to-number', and `\\#' itself for the number of replacements
done so far (starting with zero).

If the replacement expression is a symbol, write a space after it
to terminate it.  One space there, if any, will be discarded.

When using those Lisp features interactively in the replacement
text, TO-STRING is actually made a list instead of a string.
Use \\[repeat-complex-command] after this command for details."
  (interactive
   (let ((common
	  (query-replace-read-args
	   "Query replace regexp"
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (perform-replace regexp to-string t t delimited nil nil))

(defun query-replace-regexp-eval (regexp to-expr &optional delimited)
  "Replace some things after point matching REGEXP with the result of TO-EXPR.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

TO-EXPR is a Lisp expression evaluated to compute each replacement.  It may
reference `replace-count' to get the number of replacements already made.
If the result of TO-EXPR is not a string, it is converted to one using
`prin1-to-string' with the NOESCAPE argument (which see).

For convenience, when entering TO-EXPR interactively, you can use `\\&' or
`\\0' to stand for whatever matched the whole of REGEXP, and `\\N' (where
N is a digit) to stand for whatever matched the Nth `\\(...\\)' in REGEXP.
Use `\\#&' or `\\#N' if you want a number instead of a string.
In interactive use, `\\#' in itself stands for `replace-count'.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the
minibuffer.

Preserves case in each replacement if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches that are surrounded by word boundaries."
  (interactive
   (let (from to)
     (if query-replace-interactive
         (setq from (car regexp-search-ring))
       (setq from (read-from-minibuffer "Query replace regexp: "
                                        nil nil nil
                                        'query-replace-history)))
     (setq to (list (read-from-minibuffer
                     (format "Query replace regexp %s with eval: " from)
                     nil nil t 'query-replace-history)))
     ;; We make TO a list because replace-match-string-symbols requires one,
     ;; and the user might enter a single token.
     (replace-match-string-symbols to)
     (list from (car to) current-prefix-arg)))
  (perform-replace regexp (cons 'replace-eval-replacement to-expr)
		   t t delimited nil nil))


(defun replace-match-string-symbols (n)
  "Process a list (and any sub-lists), expanding certain symbols.
Symbol  Expands To
N     (match-string N)           (where N is a string of digits)
#N    (string-to-number (match-string N))
&     (match-string 0)
#&    (string-to-number (match-string 0))
#     replace-count

Note that these symbols must be preceeded by a backslash in order to
type them."
  (while n
    (cond
     ((consp (car n))
      (replace-match-string-symbols (car n))) ;Process sub-list
     ((symbolp (car n))
      (let ((name (symbol-name (car n))))
        (cond
         ((string-match "^[0-9]+$" name)
          (setcar n (list 'match-string (string-to-number name))))
         ((string-match "^#[0-9]+$" name)
          (setcar n (list 'string-to-number
                          (list 'match-string
                                (string-to-number (substring name 1))))))
         ((string= "&" name)
          (setcar n '(match-string 0)))
         ((string= "#&" name)
          (setcar n '(string-to-number (match-string 0))))
	 ((string= "#" name)
	  (setcar n 'replace-count))))))
    (setq n (cdr n))))

(defun replace-eval-replacement (expression replace-count)
  (let ((replacement (eval expression)))
    (if (stringp replacement)
        replacement
      (prin1-to-string replacement t))))

(defun replace-quote (replacement)
  "Quote a replacement string.
This just doubles all backslashes in REPLACEMENT and
returns the resulting string.  If REPLACEMENT is not
a string, it is first passed through `prin1-to-string'
with the `noescape' argument set.

`match-data' is preserved across the call."
  (save-match-data
    (replace-regexp-in-string "\\\\" "\\\\"
			      (if (stringp replacement)
				  replacement
				(prin1-to-string replacement t))
			      t t)))

(defun replace-loop-through-replacements (data replace-count)
  ;; DATA is a vector contaning the following values:
  ;;   0 next-rotate-count
  ;;   1 repeat-count
  ;;   2 next-replacement
  ;;   3 replacements
  (if (= (aref data 0) replace-count)
      (progn
        (aset data 0 (+ replace-count (aref data 1)))
        (let ((next (cdr (aref data 2))))
          (aset data 2 (if (consp next) next (aref data 3))))))
  (car (aref data 2)))





(defun perform-replace (from-string replacements
		        query-flag regexp-flag delimited-flag
			&optional repeat-count map)
  "Subroutine of `query-replace'.  Its complexity handles interactive queries.
Don't use this in your own program unless you want to query and set the mark
just as `query-replace' does.  Instead, write a simple loop like this:
  (while (re-search-forward \"foo[ \t]+bar\" nil t)
    (replace-match \"foobar\" nil nil))
which will run faster and probably do exactly what you want.
When searching for a match, this function uses
`replace-search-function' and `replace-re-search-function'."
  (or map (setq map query-replace-map))
  (let* ((event (make-event))
	 (nocasify (not (and case-fold-search case-replace
			    (string-equal from-string
					  (downcase from-string)))))
	 (literal (not regexp-flag))
	 (search-function (if regexp-flag
			      replace-re-search-function
			    replace-search-function))
	 (search-string from-string)
	 (real-match-data nil)		; the match data for the current match
	 (next-replacement nil)
	 (keep-going t)
	 (stack nil)
	 (replace-count 0)
	 (lastrepl nil)			;Position after last match considered.
	 ;; If non-nil, it is marker saying where in the buffer to
	 ;; stop.
	 (limit nil)
	 (match-again t)
	 ;; XEmacs addition
	 (qr-case-fold-search
	  (if (and case-fold-search search-caps-disable-folding)
	      (no-upper-case-p search-string regexp-flag)
	    case-fold-search))
	 (message
	  (if query-flag
	      (substitute-command-keys
	       "Query replacing %s with %s: (\\<query-replace-map>\\[help] for help) "))))
    ;; If the region is active, operate on region.
    (when (region-active-p)
      ;; Original Per Abrahamsen's code simply narrowed the region,
      ;; thus providing a visual indication of the search boundary.
      ;; Stallman, on the other hand, handles it like this.
      (setq limit (copy-marker (region-end)))
      (goto-char (region-beginning))
      (zmacs-deactivate-region))
    ;;(if (stringp replacements)
;;	(setq next-replacement replacements)
 ;;     (or repeat-count (setq repeat-count 1)))

    (cond
     ((stringp replacements)
      (setq next-replacement replacements
            replacements     nil))
     ((stringp (car replacements)) ; If it isn't a string, it must be a cons
      (or repeat-count (setq repeat-count 1))
      (setq replacements (cons 'replace-loop-through-replacements
                               (vector repeat-count repeat-count
                                       replacements replacements)))))

    (if delimited-flag
	(setq search-function replace-re-search-function
	      search-string (concat "\\b"
				    (if regexp-flag from-string
				      (regexp-quote from-string))
				    "\\b")))
    (push-mark)
    (undo-boundary)
    (unwind-protect
	;; Loop finding occurrences that perhaps should be replaced.
	(while (and keep-going
		    (not (eobp))
		    (or (null limit) (< (point) limit))
		    (let ((case-fold-search qr-case-fold-search))
		      (funcall search-function search-string limit))
		    ;; If the search string matches immediately after
		    ;; the previous match, but it did not match there
		    ;; before the replacement was done, ignore the match.
		    (if (or (eq lastrepl (point))
			    (and regexp-flag
				 (eq lastrepl (match-beginning 0))
				 (not match-again)))
			(if (or (eobp)
				(and limit (>= (point) limit)))
			    nil
			  ;; Don't replace the null string
			  ;; right after end of previous replacement.
			  (forward-char 1)
			  (let ((case-fold-search qr-case-fold-search))
			    (funcall search-function search-string limit)))
		      t))

	  ;; Save the data associated with the real match.
	  (setq real-match-data (match-data))

	  ;; Before we make the replacement, decide whether the search string
	  ;; can match again just after this match.
	  (if regexp-flag
	      (progn
		(setq match-again (looking-at search-string))
		;; XEmacs addition
		(store-match-data real-match-data)))
	  ;; If time for a change, advance to next replacement string.
;	  (if (and (listp replacements)
;		   (= next-rotate-count replace-count))
;	      (progn
;		(setq next-rotate-count
;		      (+ next-rotate-count repeat-count))
;		(setq next-replacement (nth replacement-index replacements))
;		(setq replacement-index (% (1+ replacement-index) (length replacements)))))
	  (when replacements
	      (set-match-data real-match-data)
	      (setq next-replacement
		    (funcall (car replacements) (cdr replacements)
			     replace-count)))
	  (if (not query-flag)
	      (progn
		(store-match-data real-match-data)
		(replace-match next-replacement nocasify literal)
		(setq replace-count (1+ replace-count)))
	    (undo-boundary)
	    (let ((help-form
		   '(concat (format "Query replacing %s%s with %s.\n\n"
				    (if regexp-flag (gettext "regexp ") "")
				    from-string next-replacement)
			    (substitute-command-keys query-replace-help)))
		  done replaced def)
	      ;; Loop reading commands until one of them sets done,
	      ;; which means it has finished handling this occurrence.
	      (while (not done)
		;; Don't fill up the message log
		;; with a bunch of identical messages.
		;; XEmacs change
		(display-message 'prompt
				 (format message from-string next-replacement))
		(perform-replace-next-event event)
		(setq def (lookup-key map (vector event)))
		;; Restore the match data while we process the command.
		(store-match-data real-match-data)
		(cond ((eq def 'help)
		       (with-output-to-temp-buffer (gettext "*Help*")
			 (princ (concat
				 (format "Query replacing %s%s with %s.\n\n"
					 (if regexp-flag "regexp " "")
					 from-string next-replacement)
				 (substitute-command-keys
				  query-replace-help)))
			 (save-excursion
			   (set-buffer standard-output)
			   (help-mode))))
		      ((eq def 'exit)
		       (setq keep-going nil)
		       (setq done t))
		      ((eq def 'backup)
		       (if stack
			   (let ((elt (car stack)))
			     (goto-char (car elt))
			     (setq replaced (eq t (cdr elt)))
			     (or replaced
				 (store-match-data (cdr elt)))
			     (setq stack (cdr stack)))
			 (message "No previous match")
			 (ding 'no-terminate)
			 (sit-for 1)))
		      ((eq def 'act)
		       (or replaced
			   (replace-match next-replacement nocasify literal))
		       (setq done t replaced t))
		      ((eq def 'act-and-exit)
		       (or replaced
			   (replace-match next-replacement nocasify literal))
		       (setq keep-going nil)
		       (setq done t replaced t))
		      ((eq def 'act-and-show)
		       (if (not replaced)
			   (progn
			     (replace-match next-replacement nocasify literal)
			     (store-match-data nil)
			     (setq replaced t))))
		      ((eq def 'automatic)
		       (or replaced
			   (replace-match next-replacement nocasify literal))
		       (setq done t query-flag nil replaced t))
		      ((eq def 'skip)
		       (setq done t))
		      ((eq def 'recenter)
		       (recenter nil))
		      ((eq def 'edit)
		       (store-match-data
			(prog1 (match-data)
			  (save-excursion (recursive-edit))))
		       ;; Before we make the replacement,
		       ;; decide whether the search string
		       ;; can match again just after this match.
		       (if regexp-flag
			   (setq match-again (looking-at search-string))))
		      ((eq def 'delete-and-edit)
		       (delete-region (match-beginning 0) (match-end 0))
		       (store-match-data (prog1 (match-data)
					   (save-excursion (recursive-edit))))
		       (setq replaced t))
		      ;; Note: we do not need to treat `exit-prefix'
		      ;; specially here, since we reread
		      ;; any unrecognized character.
		      (t
		       (setq this-command 'mode-exited)
		       (setq keep-going nil)
		       (setq unread-command-events
			     (cons event unread-command-events))
		       (setq done t))))
	      ;; Record previous position for ^ when we move on.
	      ;; Change markers to numbers in the match data
	      ;; since lots of markers slow down editing.
	      (setq stack
		    (cons (cons (point)
				(or replaced
				    (match-data t)))
			  stack))
	      (if replaced (setq replace-count (1+ replace-count)))))
	  (setq lastrepl (point)))
      ;; Useless in XEmacs.  We handle (de)highlighting through
      ;; perform-replace-next-event.
      ;(replace-dehighlight)
      )
    (or unread-command-events
	(message "Replaced %d occurrence%s"
		 replace-count
		 (if (= replace-count 1) "" "s")))
    (and keep-going stack)))


(defun dired-do-tags-query-replace (from to &optional delimited)
  "Do `query-replace-regexp' of FROM with TO, on all marked files.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue]."
  (interactive
   (let ((common
	  (query-replace-read-args
	   "Query replace regexp in marked files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (dolist (file (dired-get-marked-files))
    (let ((buffer (get-file-buffer file)))
      (if (and buffer (with-current-buffer buffer
			buffer-read-only))
	  (error "File `%s' is visited read-only" file))))
  (tags-query-replace from to delimited
		      '(dired-get-marked-files)))


(defun tags-query-replace (from to &optional delimited file-list-form)
  "Do `query-replace-regexp' of FROM with TO on all files listed in tags table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue].

See documentation of variable `tags-file-name'."
  (interactive (query-replace-read-args "Tags query replace (regexp)" t t))
  (setq tags-loop-scan `(let ,(unless (equal from (downcase from))
				'((case-fold-search nil)))
			  (if (re-search-forward ',from nil t)
			      ;; When we find a match, move back
			      ;; to the beginning of it so perform-replace
			      ;; will see it.
			      (goto-char (match-beginning 0))))
	tags-loop-operate `(perform-replace ',from ',to t t ',delimited))
  (tags-loop-continue (or file-list-form t)))


(defun dired-replace-global ()
  "Replace all remaining matches in all files with no more questions."
  (interactive)
  (let ((cnt 0))
    (while (< cnt 1000)
      (re-search-backward (cadr query-replace-history) nil t)
      (execute-kbd-macro
       (read-kbd-macro "M-, !"))
      (setq cnt (1+ cnt)))))

(provide 'replace-eval-xmas)

;;; replace-eval-xmas.el ends here
