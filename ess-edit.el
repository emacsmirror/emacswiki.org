;;; ess-edit.el --- convenient editing of R code
;;{{{ header
;; Copyright (C) 2007,2008,2009,2010,2011  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tagteam@sund.ku.dk>
;; Keywords: convenience

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
;;}}}
;;; Commentary:
;; Compatibility: Emacs23 XEmacs21
;;
;;{{{ usage 
;;
;; load this file, then open some R-code, put
;; the point inside the a call to an R function, e.g.
;;
;;  c(1,2,3,4)
;; 
;; then do
;;
;;  M-x ess-edit-indent-call-sophisticatedly
;; 
;; the result is this:
;;
;;  c(1,
;;  2,
;;  3,
;;  4)
;;
;;  to get back to the original format just
;;  call the same command again:
;; 
;;  M-x ess-edit-indent-call-sophisticatedly
;;
;;  other features:
;;  
;;  M-x ess-edit-insert-vector RET letters[1:10] RET
;;
;;  M-x ess-edit-insert-vector RET names(mydata) RET
;; 
;;  M-x ess-edit-insert-call RET agrep RET
;;}}}
;;{{{Issues
;;
;; may not work perfectly when there are comments in a function call
;; 
;;
;;}}}
;;; code
;;{{{ variables

(defcustom ess-edit-expand-file-name-relative t
  "Controls behaviour of 'ess-edit-insert-path' and 'ess-edit-insert-file-name':
If 'nil' just call expand-file-name else replace user-home with `~`. This is useful, when
the code should work on machines with different values of user-home.")

;;}}}
;;{{{ emacs-xemacs translations

(defun ess-edit-buffer-string (buffer)
  (save-excursion (set-buffer buffer)
		  (buffer-substring (point-min) (point-max))))


;; flatten
(defun ess-edit-flatten (lis)
  "Removes nestings from a list."
  (cond ((atom lis) lis)
	((listp (car lis))
	 (append (flatten (car lis)) (flatten (cdr lis)))
	 )
	(t (append (list (car lis)) (flatten (cdr lis))))))

;; replace-in-string
;; from rs-info.el
(defun ess-edit-replace-in-string (string regexp newtext)
  "In STRING, replace all matches for REGEXP with NEWTEXT.
Hack to get a common function for all Emacsen.  Note that Oort Gnus
has
`gnus-replace-in-string', but we don't want to load Gnus."
  (cond
   ;; Emacs 21 and later
   ((fboundp 'replace-regexp-in-string)
    (replace-regexp-in-string regexp newtext string))
   ;; Emacs < 21; XEmacs
   (t
    ;; Code duplicated from `subr.el' revision 1.423 of Emacs. Neither
    ;; `replace-in-string' from XEmacs 21.4.15 nor the Gnus replacement works
    ;; correctly when an empty string is matched.
    (let ((rep newtext)
	  (l (length string))
	  (start 0) ;; (or start 0) in `subr.el'
	  fixedcase literal subexp
	  matches str mb me)
      (save-match-data
	(while (and (< start l) (string-match regexp string start))
	  (setq mb (match-beginning 0)
		me (match-end 0))
	  ;; If we matched the empty string, make sure we advance by
	  one char
	  (when (= me mb) (setq me (min l (1+ mb))))
	  ;; Generate a replacement for the matched substring.
	  ;; Operate only on the substring to minimize string consing.
	  ;; Set up match data for the substring for replacement;
	  ;; presumably this is likely to be faster than munging the
	  ;; match data directly in Lisp.
	  (string-match regexp (setq str (substring string mb me)))
	  (setq matches
		(cons (replace-match (if (stringp rep)
					 rep
				       (funcall rep (match-string 0
								  str)))
				     fixedcase literal str subexp)
		      (cons (substring string start mb) ; unmatched
			    prefix
			    matches)))
	  (setq start me))
	;; Reconstruct a string from the pieces.
	(setq matches (cons (substring string start l) matches)) ;
	leftover
	(apply #'concat (nreverse matches)))))))
;;}}}
;;{{{commands

(defun ess-edit-dev-off()
  (interactive)
  (ess-eval-linewise "dev.off()"))

(defun ess-edit-insert-vector (&optional vec)
  (interactive)
  (let ((vec (or vec (read-string "vector: ")))
	(tbuffer (get-buffer-create " *ess-insert*"))
	string)
    (save-excursion
      (set-buffer tbuffer)
      (ess-command (concat "as.list(" vec ")" "\n") tbuffer)
      (setq string (ess-edit-replace-in-string
		    (ess-edit-replace-in-string
		     (ess-edit-replace-in-string
		      (ess-edit-buffer-string tbuffer)
		      "^\\[[0-9]\\] \\|^\\[+[0-9]+\\]+\n\\|^\n" "")
		     "\n$" "")
		    "\n+" ",")))
    (insert "c(" string ")")))

(defun ess-edit-insert-path (&optional arg)
  (interactive "P")
  (if arg
      (insert "\""
	      (ess-edit-expand-file-name
	       (read-directory-name "insert directory name: ")) "\"")
    (insert "\"" default-directory "\"")
    (backward-char 1)))

(defun ess-edit-insert-file-name ()
  (interactive)
  (insert "\"" (ess-edit-expand-file-name (read-file-name "insert filename: ")) "\""))
  

(defun ess-edit-insert-read ()
  (interactive)
  (let* ((fun (ess-read-object-name "Function for reading "))
	 (datei (ess-edit-expand-file-name (read-file-name "Infile ")))
	 (name (read-string "Name for new object "))
	 (header (save-window-excursion
		   (when (string-match "read" fun)
		     (find-file-other-window datei))
		   (y-or-n-p "Variable names in first line? "))))
    (insert name " <- " fun "(\"" datei "\",\n"
	    (when (string-match "read" fun)
	      (concat "header=" (if header "T" "F")))
	    ")")))




(defun ess-edit-beginning-of-call (arg)
  (interactive "p")
  (ess-edit-read-call arg 'go))


(defun ess-edit-end-of-call (arg)
  (interactive "p")
  (ess-edit-beginning-of-call arg)
  (forward-sexp 1)
  (backward-char 1))


(defun ess-edit-mark-call (&optional arg)
  (interactive "p")
  (ess-edit-beginning-of-call arg)
  (skip-chars-backward "a-zA-Z0-9.")
  (push-mark (save-excursion
	       (skip-chars-forward "a-zA-Z0-9.")
		 (forward-sexp)
		 (point))
	     nil t))

(defun ess-edit-indent-call-sophisticatedly (&optional arg force)
  (interactive "p")
  (let* ((arg (or arg 1))
	 (fun (ess-edit-read-call arg 'go))
	 (beg (+ (point) 1))
	 (end (progn (forward-sexp) (point)))
	 breaks
	 delete-p)
    ;;	  (eq last-command 'ess-edit-indent-call-sophisticatedly)
    (goto-char beg)
    (while (setq match (re-search-forward "[\"\'{([,]" end t))
      (if (string= (match-string 0) ",")
	  (setq breaks (cons (cons (point)
				   (if (looking-at "[ \t]*\n") t nil)) breaks))
	(if (or (string= (match-string 0) "\"")  (string= (match-string 0) "\'"))
	    (re-search-forward (match-string 0) nil t)
	(backward-char 1)
	(forward-sexp))))
    ;; if there are more breaks than half the number of
    ;; arguments then delete breaks else add linebreaks
    (setq delete-p
	  (if force nil
	    (> (length (delete nil (mapcar 'cdr breaks))) (* 0.5 (length breaks)))))
    (while breaks (goto-char (caar breaks))
      (if delete-p
	  (if (cdar breaks)
	      (delete-region (caar breaks) (+ (point) (skip-chars-forward " \t\n"))))
	(if (not (cdar breaks))
	    (insert "\n")))
      (setq breaks (cdr breaks)))
    (goto-char (- beg 1))
    (ess-indent-exp)
    (ess-edit-read-call arg 'go)))


(defun ess-edit-summary-at-point ()
  (interactive)
  (let ((x (ess-edit-word-at-point)))
    (ess-eval-linewise (concat "summary(" x ")"))))


(defun ess-edit-insert-call (ask &optional object)
  (interactive "P")
  (let* ((ess-local-process-name ess-current-process-name)
	 (object-list (ess-get-object-list ess-local-process-name))
	 (object (or object (car (ess-read-object-name "function "))))
	 (case-fold-search nil)
	 (generic-list
	  (delete nil
		  (mapcar (lambda (x)
			    (if (and (string-match (concat "^" object) x)
				     (not (string= object x))) x nil))
			  object-list)))
	 args edit-point (no-dots t))
    (if (> (length generic-list) 1)
	(setq object
	      (completing-read
	       (format "Object %s is generic -- choose one: " object)
	       (mapcar 'list generic-list) nil nil object)))
    (insert object "()")
    (backward-char 1)
    (setq args
	  (delq nil
		(mapcar (lambda (x) (if  (string= "..." (car x))
					(setq no-dots nil) x))
			(ess-edit-get-function-args object))))
    (while args
      (let ((arg (car args)))
	(insert (concat (car arg) "="
			(if (cdr arg)
			    (car (cdr arg))
			  (if edit-point nil
			    (setq edit-point (+ (length (car arg)) 1 (point))) ""))
			(if (> (length args) 1) "," "")))
	(setq args (cdr args))))
    (goto-char edit-point)
    (save-excursion (ess-edit-indent-call-sophisticatedly))
    (if (not no-dots)
	(message (format "%s accepts extra arguments (...=)" object)))))


(defun ess-edit-delete-arg (&optional arg)
  (interactive)
  (let ((arg (or arg (ess-edit-arg-at-point)))
	(lim (progn 
	       (ess-edit-end-of-call 1)
	       (point)))
	beg end)
    (save-excursion
      (ess-edit-beginning-of-call 1)
      (re-search-forward arg nil t)
      (setq beg (match-beginning 0))
      (backward-char 1)
      (re-search-forward "=.*$" lim t)
      (setq end (match-end 0))
      (delete-region beg end))))


(defun ess-edit-vector2formula ()
  (interactive)
    (let* ((fname (ess-edit-read-call 1 'move))
	   (beg (- (point) 1))
	   (end (progn (forward-sexp) (point)))
	   vec)
      (if (not (string= fname "c"))
	  (error "Current cursor position is not on a vector")
	(setq vec (buffer-substring beg end))
	(delete-region beg end)
	(setq vec (ess-edit-replace-in-string vec "c([\"]?" "~ "))
	(setq vec (ess-edit-replace-in-string vec "[\"]?)" ""))
	(setq vec (ess-edit-replace-in-string vec "[\"]?,[\"]?" " + "))
	(insert vec))))
		 
(defun ess-edit-clean-up ()
  (interactive)
  (let* ((ess-local-process-name ess-current-process-name)
	 (call (ess-edit-read-call 1 'go))
	 (call-args (ess-edit-get-function-args call))
	 (required-args
	  (delete nil
		  (mapcar (lambda (x)
			    (if (cdr x) x nil))
			  call-args)))
	 (visible-args (ess-edit-read-args)))
    (while visible-args
      (let* ((arg (car visible-args))
	     (val (cadr arg))
	     default)
	(if (and (setq default
		       (cadr (assoc (car arg) required-args)))
		 (string= default val))
	    (ess-edit-delete-arg (car arg)))
	(setq visible-args (cdr visible-args))))
    (ess-edit-indent-call-sophisticatedly)))


(defun ess-edit-display-help-on-arg (&optional function)
  (interactive)
  (add-hook 'pre-command-hook 'ess-edit-pre-command-hook)
  (let* ((arg (ess-edit-arg-at-point))
	 (fname (or function (ess-edit-read-call)))
	 (help-buffer (concat "*help["
			      ess-current-process-name
			      "](" fname ")*"))
	 (args (flatten (ess-edit-get-function-args fname)))
	 (inferior-ess-help-command "help(\"%s\", htmlhelp=FALSE)\n")
	 window beg end arg-args)
    (ess-display-help-on-object fname)
    (if (not (setq arg-args
		   (car (sort 
			 (delete nil
				 (mapcar
				  (lambda (x)
				    (if (string-match arg x) x nil)) args))
			 (lambda (x y)(< (length x) (length y)))))))
	(error (format "%s is not an argument of %s!" arg fname))
      (setq arg arg-args)
      (set-buffer help-buffer)
      (setq window (get-buffer-window (get-buffer help-buffer)))
      (goto-char (point-min))
      (setq beg (progn (re-search-forward
			(concat "^\\s *" arg ": ") nil t)
		       (point))
	    end (progn (re-search-forward "^[\n\t ]*$" nil t)))
      (setq window (get-buffer-window (get-buffer help-buffer)))
      (ess-edit-highlight-arg window beg end))))


(defun ess-edit-read-call (&optional arg move all)
  "Return the name of the R-function call at point as a string.
if ARG return name of function call which is ARG function calls
above point. if MOVE is non-nil leave point after
 opening parentheses of call. If all is non-nil return the full string."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (error "Only backward reading of function calls possible."))
  (add-hook 'pre-command-hook 'ess-edit-pre-command-hook)
  ;; assume correct syntax, at least beyond previous paragraph-start 
  (let ((oldpoint (point))
	(lim (save-excursion
	       (backward-paragraph 1) (point)))
	fun beg end)
    ;;move outside comments and quotes first
    (ess-edit-backward-move-out-of-comments lim)
    (ess-edit-backward-move-out-of-quotes)
    ;;what if we are sitting on a function call?
    (if (save-excursion
	  (skip-chars-backward "a-zA-Z0-9.")
	  (looking-at "\\([a-zA-Z0-9.]+\\)\\((\\)"))
	(setq beg (match-beginning 1) end (match-end 1)
	      fun (list (match-string 1))
	      arg (- arg 1)))
    (while
	(and (> arg 0)
	 (re-search-backward "[\"\'()]" lim t)
	 (let ((matchcar (char-before (match-end 0)))
	       matchcall)
	   (if (eq ?\( matchcar)
	       ;; test if sitting on proper function call
	       (if (not (progn
			  (skip-chars-backward "a-zA-Z0-9.")
			  (looking-at "\\([a-zA-Z0-9.]+\\)\\((\\)")))
		   nil
		 (if (string= "\\(if\\|else\\|for\\)"
				   (setq matchcall (match-string 1)))
		     t
		   (setq beg (match-beginning 1) end (match-end 1)
			 fun (append (list matchcall) fun))
		   (if (= arg 1) nil (setq arg (- arg 1)))))
	     ;; skip balanced parentheses or quotes
	     (if (not (= ?\) matchcar))
		 (re-search-backward
		  (char-to-string matchcar) lim t)
	       (condition-case nil 
		   (progn
		     (forward-char 1)
		     (backward-sexp) t)
		 (t (goto-char oldpoint)
		    (error "Point is not in a proper function call or unbalanced parentheses paragraph."))))))))
	 (if (not fun)
	     (progn (goto-char oldpoint)
	     (error "Point is not in a proper function call or unbalanced parentheses in this paragraph."))
	   (ess-edit-highlight 0 beg end)
	   (message (car fun))
	   (goto-char (if move (+ (point) (skip-chars-forward "a-zA-Z0-9."))
			oldpoint))
	   (if all fun (car fun)))))

;;}}}
;;{{{ functions


(defun ess-edit-expand-file-name (name)
  (let ((ename (expand-file-name name)))
  (if ess-edit-expand-file-name-relative
      (ess-edit-replace-in-string ename (expand-file-name "~") "~")
    ename)))

;; modified copy of comint-how-many-region
(defun ess-edit-how-many-quotes-region (beg end)
  "Return number of matches for quotes skipping double quotes and escaped quotes from BEG to END."
  (let ((count 0))
    (save-excursion
      (save-match-data
	(goto-char beg)
	(while (re-search-forward "\"\\|\'" end t)
	  (if (or (save-excursion
		    (backward-char 3)
		    (looking-at "\\\\"))
		  (looking-at "\"\\|\'"))
	      (forward-char 1)
	    (setq count (1+ count))))))
    count))

;; modified copy of comint-within-quotes
(defun ess-edit-within-quotes (beg end)
  "Return t if the number of quotes between BEG and END is odd.
Quotes are single and double."
  (let (
	;; (countsq (ess-edit-how-many-quotes-region "\\(^\\|[^\\\\]\\)\'" beg end))
	;; (countdq (ess-edit-how-many-quotes-region "\\(^\\|[^\\\\]\\|^\"\"\\)\"" beg end)))
	(countsq (ess-edit-how-many-quotes-region beg end))
	(countdq (ess-edit-how-many-quotes-region beg end)))
    ;; (countsq (ess-edit-how-many-region "\'" beg end))
    ;; (countdq (ess-edit-how-many-region "\"" beg end)))
    (or (= (mod countsq 2) 1) (= (mod countdq 2) 1))))

(defun ess-edit-backward-move-out-of-quotes ()
  "If inside quotes, move the point backwards out."
  (let ((start 
	 (save-excursion
	   (beginning-of-line) (point))))
    (if (ess-edit-within-quotes start (point))
	(re-search-backward "[\'\"]" nil t))))


(defun ess-edit-backward-move-out-of-comments (lim)
  "If inside comments, move the point backwards out."
  (let ((opoint (point)) stop)
    (if (save-excursion
	  (beginning-of-line)
	  (search-forward "#" opoint 'move))
	(while (not stop)
	  (skip-chars-backward " \t\n\f" lim)
	  (setq opoint (point))
	  (beginning-of-line)
	  (search-forward "#" opoint 'move)
	  (skip-chars-backward " \t#")
	  (setq stop (or (/= (preceding-char) ?\n) (<= (point) lim)))
	  (if stop (point)
	    (beginning-of-line))))))



(defun ess-edit-pre-command-hook ()
  "Remove highlighing in both source and help buffers."
  ;; used as pre command hook in *toc* buffer
  (ess-edit-unhighlight 0)
  (ess-edit-unhighlight 1))

(defun ess-edit-highlight-arg (window beg end)
  (select-window window)
  (goto-char beg)
  (scroll-up (- (save-excursion (beginning-of-line) (line-number))
		(save-excursion (point-min) (line-number))))
	     (set-buffer (window-buffer window)) 
	     (ess-edit-highlight 1 beg end))

(defun ess-edit-arg-at-point ()
  (save-excursion
    (buffer-substring
     (+ (point) (skip-chars-backward "a-zA-Z0-9."))
     (+ (point) (skip-chars-forward "a-zA-Z0-9.")))))
       



(defun ess-edit-get-function-args (function)
  (let ((tbuffer (get-buffer-create " *ess-insert*"))
	args)
    (save-excursion
      (set-buffer tbuffer)
      (ess-command (concat "formals(" function ")\n") tbuffer)
      (setq args 
	    (mapcar (lambda (sub)
		      (split-string
		       (ess-edit-replace-in-string sub "^ \\|\\[[0-9]+\\]\\|[\n]+$" "")
		       "\n"))
		    (cdr (split-string (ess-edit-buffer-string tbuffer) "\\$"))))
      (kill-buffer tbuffer))
    args))


(defun ess-edit-read-args ()
  (save-excursion
    (ess-edit-indent-call-sophisticatedly 1 'force)
    (let ((lim (save-excursion (forward-sexp 1) (+ (point) 1)))
	  args)
      (forward-char 1)
      (while (< (point) lim)
	(setq arg
	      (cond ((re-search-forward
		      "\\([\t ]*\\)\\([\"\']?\\)\\([a-zA-Z0-9.]+\\)\\([\"\']?\\)\\(=\\)\\(.*\\)\\(,\\|)\\)\\(\n\\)"
		      lim t)
		     (list (match-string 3) (match-string 6)))
		    ((re-search-forward "\\([\t ]*\\)\\(.*\\)\\(,\\|)\\)\\(\n\\)"
					lim t)
		     (list nil (match-string 2)))
		    (t (error "Cant read proper argument here."))))
;	(beginning-of-line 2)
	(setq args (append args (list arg))))
      args)))


;from match.call
;expand.dots: logical. Should arguments matching `...' in the call be
;          included or left as a `...' argument?


;;; Highlighting (copied from reftex.el -- thankx Carsten (and Stephen)!)
;; Highlighting uses overlays.  If this is for XEmacs, we need to load
;; the overlay library, available in version 19.15
(and (not (fboundp 'make-overlay))
     (condition-case nil
         (require 'overlay)
       ('error 
        (error "Ess-Edit needs overlay emulation (available in XEmacs 19.15)"))))

;; We keep a vector with several different overlays to do our highlighting.
(defvar ess-edit-highlight-overlays [nil nil])


;; Initialize the overlays
(aset ess-edit-highlight-overlays 0 (make-overlay 1 1))
(overlay-put (aref ess-edit-highlight-overlays 0) 'face 'highlight)
(aset ess-edit-highlight-overlays 1 (make-overlay 1 1))
(overlay-put (aref ess-edit-highlight-overlays 1) 'face 'highlight)

;; Two functions for activating and deactivation highlight overlays
(defun ess-edit-highlight (index begin end &optional buffer)
  "Highlight a region with overlay INDEX."
  (move-overlay (aref ess-edit-highlight-overlays index)
                begin end (or buffer (current-buffer))))

(defun ess-edit-unhighlight (index)
  "Detatch overlay INDEX."
  (delete-overlay (aref ess-edit-highlight-overlays index)))
		  

(defun ess-function-name ()
  "Show the name of the current function in the minibuffer.
   Based on ess-beginning-of-function"
  (let ((init-point (point))
        beg end done)
    ;; First search for the start of the function definition
    ;; in case we're sitting in a function header:
    (if (search-forward "(" (ess-line-end-position 2) t); at most end of next line
        (forward-char 1))
    (while (not done)
      (if (re-search-backward ess-function-pattern (point-min) t)
          nil
        (goto-char init-point)
        (error "Point is not in a function."))
      (setq beg (point))
      ;; The point is now at the start of the function name
      (let (word regexp point end)
        (setq point (point))
        ; Look forward for one of '" ' '"_' '"<' ' ' '_ <' 
        (while (not (looking-at "\"*[ _<]"))
          (forward-char 1))
        (setq end (point))
        (goto-char point)
        ; Look backward for the start of function name, excluding quote
        (while (and (not (bobp)) (looking-at "\\w"))
          (backward-char 1))
        (or (looking-at "\\w")
            (forward-char 1))
        (and (= (point) end)
             (error "not looking at a word"))
        (setq word (buffer-substring (point) end))
        (goto-char init-point)
        (error word)
        )
      ;; current function must begin and end around point
      (setq done (<= beg init-point)))
    beg))



(defun ess-edit-word-at-point ()
  (save-excursion
    (buffer-substring
     (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
     (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))
       
;;}}}
(provide 'ess-edit)
;;; ess-edit.el ends here
