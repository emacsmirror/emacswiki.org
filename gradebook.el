;;; gradebook.el -- major mode for keeping a simple plain-text gradebook

;;; Version: 0.2
;;; Author: Hans Halvorson (www.princeton.edu/~hhalvors)
;;; Time-stamp: <2007-01-25 14:32:57 hhalvors>

;;; For suggestions: thanks to Kim Storm and to some guys on the common
;;; lisp irc channel.

;;; gradebook-mode allows you to do calculations (arbitrary elisp
;;; functions) on columns and insert the result in a new column.  It
;;; also allows you to do functions whose argument is a list
;;; (e.g. median, mean, mode) on the rows, and insert the result as a
;;; new row.  I use it to keep a gradebook, but in theory it could be
;;; used for any numeric data.  It does not have anything close to the
;;; functionality of a full-blown spreadsheet mode (e.g. ses.el), but
;;; it has the advantage that your data is kept in plain unformatted
;;; text.
;;;
;;; Here is what the gradebook buffer might look like:
;;;
;;; [--begin buffer--]
;;;           hw1     hw2     hw3     mt	
;;;
;;; Key        24      40      35      16        
;;;
;; Kurt       24      40      34      16	
;;
;; Derek      24      39      29      15	
;;
;; Hoyt       23      27      35      15.5      
;;
;;
;;
;; (gb-compute-column "hw-avg" '(/ (+ hw1 hw2 hw3) 3))
;;
;; [--end buffer--]
;;
;; You must manually create the row names.  Leave at least one empty
;; line above the row names for the column headings.  You do not have
;; to separate the lines with spaces, but it's easier to read if you
;; do.  There are two functions for addings columns, and one function
;; for adding rows:
;;
;; 1. The function "gb-enter-column" lets you input a column of "atomic"
;; grades, i.e. grades that are not calculated from other grades.  It
;; will ask for a column name.  Then it ask for the student names (the
;; names that label the rows); pressing TAB will autocomplete the
;; names.  Then it will ask for the grade.  It will exit after all
;; students have been assigned a grade, or you can press "C-g" to exit
;; the process.
;;
;; 2. The function "gb-compute-column" lets you compute a new column from
;; other columns.  It takes two arguments: the first is a string to
;; name the column, and the second is a a formula whose only free
;; variables are the column names.  (Sorry, no recursive formulas
;; allowed!)  (Note, the free variables should be symbols, not
;; strings, as in "(/ hw2 0.40)".)  By the way, if your formula
;; contains no decimal numbers, and the input numbers are not
;; decimals, then the output will be an integer -- this is just a
;; feature of emacs-lisp.  If you want to see decimals, then just make
;; sure that the formula has decimal numbers, e.g. use "(/ hw1 2.0)"
;; rather than "(/ hw1 2)"; then everything is converting to floats.
;;
;; 3. The function "gb-compute-row" lets you compute a new row
;; from the existing rows.  In this case, the function should take a
;; list as an argument.  For example, you could use the function "(/
;; (apply '+ list) (length list))" to get the average of the rows.
;;
;; If at any time you manually add a new row, you should do "M-x
;; get-row-names" to refresh the list "gb-row-names".
;;
;; Two cautions: 1. You must use monospaced fonts, or everything will
;; look like crap.  2. These functions might not work properly in
;; buffers that do not use "gradebook-mode-syntax-table."  In
;; particular, the functions apply "number-at-point" (from
;; thingatpt.el) to get their input values, but in some syntax tables
;; (e.g. "text-mode-syntax-table"), "number-at-point" does not
;; recognize "." as part of a number.


(require 'thingatpt)

;; with the text-mode syntax table, the "number-at-point" function
;; does not recognize floating point numbers (decimals).  So, we use
;; the following modification to the syntax table.

(defvar gradebook-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?. "_   " table)
    table))

;; run gb-get-cols and gb-get-rows upon startup

(defun gradebook-mode()
   (interactive)
   (make-variable-buffer-local 'gb-col-names)
   (make-variable-buffer-local 'gb-row-names)
   (setq major-mode 'gradebook-mode)
   (setq mode-name "Gradebook")
   (setq column-number-mode t)
   (set-syntax-table gradebook-mode-syntax-table)
   (use-local-map gradebook-mode-map)
   (gb-get-cols)
   (gb-get-rows))


(defgroup gradebook nil
  "Trivial spreadsheet functions."
  )


;; I originally intended for the gradebook table to begin at the
;; beginning of the buffer (so that the column headings are on the
;; first line).  But this can easily be changed by customizing the
;; grade-table-begin variable

(defcustom gb-table-begin 1
  "*Line on which the column headings can be found."
  :group 'gradebook)


;; the gb-get-rows function grabs the row names and numbers by
;; searching forward according to gb-row-regexp.  But one might want
;; to add text further down in the buffer that matches the regexp, but
;; that is not a row name. To deal with this, set gb-table-end to some
;; value that occurs before the text that you want to ignore.

(defcustom gb-table-end nil
  "*Where to stop looking for gradebook rows (a buffer position).  If nil, look to the end of the buffer"
  :group 'gradebook)


;; col name regexp

(defcustom gb-column-regexp " [a-z0-9]+"
  "*The regular expression that is used to identify columns.
This regexp must uniquely identify the columns among the strings
on the first line of the gradebook file.  The column numbers are
then set to the first character in each string that matches this
regexp."
  :group 'gradebook)


;; row name regexp

(defcustom gb-row-regexp "^[A-Z]+"
  "*The regular expression that is used to identify rows.  The
default is an alphabetic string that starts a line."
  :group 'gradebook)


;; how many spaces between columns?

(defcustom gb-column-spacing 8
  "*The distance from the beginning of one column to the beginning of the next."
  :group 'gradebook)

(defcustom gb-key-name "Key"
  "*The name of the row that contains the total possible points for each assignment or exam."
  :group 'gradebook)


;;; keybindings

(defvar gradebook-mode-map nil
   "Keymap for gradebook buffer.")

(if gradebook-mode-map
   ()
   (setq gradebook-mode-map (make-sparse-keymap))
   (define-key gradebook-mode-map "\C-c\C-c" 'gb-compute-column)
   (define-key gradebook-mode-map "\C-c\C-e" 'gb-enter-column)
   (define-key gradebook-mode-map '[M-down] 'gb-move-down)
   (define-key gradebook-mode-map '[M-up] 'gb-move-up)
   (define-key gradebook-mode-map '[M-left] 'gb-move-left)
   (define-key gradebook-mode-map '[M-right] 'gb-move-right)
)


;;; the functions

(defun gb-get-cols ()
  "Make an alist of column titles and numbers; use
gb-column-regexp to identify column names."
  (interactive)
  (setq gb-col-names '())
  (save-excursion
  (goto-char (point-min))
  (forward-line)
  (let ((my-bound (point)))
  (goto-char (point-min))
  (while (re-search-forward gb-column-regexp my-bound t)
    (goto-char (+ 1 (match-beginning 0)))
        ;; remove whitespace from assignment names
    (add-to-list 'gb-col-names (cons (intern (replace-regexp-in-string "[ ]+" "" (match-string-no-properties 0))) (current-column)))))
  (setq gb-col-names (reverse gb-col-names)))
  )


(defun gb-get-rows ()
  "Make an alist of row titles and numbers; use gb-row-regexp
to identify row names."
  (interactive)
  (setq gb-row-names '())
  (save-excursion
    (goto-char (point-min))
  (while (re-search-forward gb-row-regexp gb-table-end t)
    (add-to-list 'gb-row-names (cons (match-string-no-properties 0) (line-number-at-pos))))
  (setq gb-row-names (reverse gb-row-names))))

;; auxiliary function: remove element from list

(defun gb-remove-element (element list)
       "Remove elements from list, if it occurs."
       (cond
        ((not list) nil)
        ((eq element (car list))
	 (gb-remove-element element (cdr list)))
        (t (cons (car list) (gb-remove-element element (cdr list))))))

;; there are two ways to add columns 
;; 1. add manually = gb-enter-column
;; 2. compute from previous columns and some formula whose variables
;; are column labels = gb-compute-column

(defun gb-enter-column (title)
  (interactive "MColumn name: ")
  (save-excursion 
    (goto-char (point-min))
    ;; the new column should be col-width spaces to the right of the
    ;; greater of (1) the longest row name, (2) the rightmost of the
    ;; previously entered columns
    (let* ((max-row-name (apply 'max (mapcar 'length (mapcar 'car gb-row-names))))
	   (new-col 
	    (+ gb-column-spacing 
	       (or (cdr (car (reverse gb-col-names))) max-row-name))))
      ;; go to one place before new column, because we will insert one white space
      (move-to-column (- new-col 1) t)
      ;; add one white space before title
      (insert (concat " " title))
      (let ((studs gb-row-names))
	(while studs
	  (let ((stud-name (completing-read "Student name: " studs)))
	    (goto-line (cdr (assoc stud-name studs)))
	    (move-to-column new-col t)
	    (insert (read-from-minibuffer "Grade: "))
	    (setq studs (gb-remove-element (assoc stud-name studs) studs)))))))
  (gb-get-cols))


(defun gb-compute-column (title formula)
;; TITLE should be a string that names the column, and FORMULA should
;; be a sexp whose only unbound variables are column names
  "Compute a column as a function of other columns."
  (save-excursion
        (let* ((max-row-name (apply 'max (mapcar 'length (mapcar 'car gb-row-names))))
	       (new-col 
		(+ gb-column-spacing 
		   (or (cdr (car (reverse gb-col-names))) max-row-name))))
      (goto-char (point-min))
      ; one character before new column
      (move-to-column (- new-col 1) t)
      ; add one white space before the column name, so we can match the regexp
      (insert (concat " " title))
      (setq goo-lines (mapcar 'cdr gb-row-names))
      (while goo-lines
	  (setq current-line (car goo-lines))
	  (goto-line current-line)
	  (move-to-column new-col t)
	  ; (insert (number-to-string (gb-formula-on-line formula current-line)))
	  (insert (format "%.4g" (gb-formula-on-line formula (car goo-lines))))
	  (setq goo-lines (cdr goo-lines)))))
  (gb-get-cols))


;; DEPRECATED function: uses the yucky gb-bind-by-line

;; (defun old-gb-formula-on-line (formula line-number)
;;   "Given FORMULA whose free variables are column names, and
;; LINE-NUMBER, substitute the corresponding values in the formula."
;;   (gb-bind-by-line line-number)
;;   (eval formula))


;;; DEPRECATED function: pollutes name space

;; (defun gb-bind-by-line (line-number)
;;   "Binds column name symbols to numbers that occur on
;; line-number."
;;   (save-excursion
;;   (goto-line line-number)
;;   (let ((foobar gb-col-names))
;;     (while foobar
;;       (move-to-column (cdr (car foobar)))
;;       ;; the following is my attempt to limit these bindings
;;       (make-local-variable (car (car foobar)))
;;       ;; the following line is the offender -- it globally sets column
;;       ;; names to the values on the corresponding line number
;;       (set (car (car foobar)) (number-at-point))
;;       (setq foobar (cdr foobar))))))


;; auxliary function: given a LINE, return an assoc list of column
;; names and numbers occuring in the corresponding column on LINE

(defun gb-line-assoc (line-number)
  "Makes an alist consisting of column names and corresponding
numbers on LINE."
  (save-excursion
  (goto-line line-number)
  (let ((foobar gb-col-names)
	(line-alist '()))
    (while foobar
      (move-to-column (cdr (car foobar)))
      (add-to-list 'line-alist (cons (car (car foobar)) (number-at-point)))
      (setq foobar (cdr foobar)))
    (reverse line-alist)
    )
  ))

;; auxiliary function: substitute numbers for variables in a function,
;; and then evaluate

(defun gb-formula-substitute (formula alist)
  "Given FORMULA, evaluate based on substitutions in ALIST."
   (progv (mapcar 'car alist) (mapcar 'cdr alist)
     (eval formula)))

; example: (gb-formula-substitute '(+ x y) '((x . 2) (y . 3)))
;                   ==> 5


;; auxiliary function: given a function whose free variables are
;; column names, and a line number, compute the output of substituting
;; the numbers on that line for the corresponding column names.

(defun gb-formula-on-line (formula line-number)
  "Evaluate formula relative to line-number's values." 
  (gb-formula-substitute formula (gb-line-assoc line-number)))

 
;; the use of the 'number-at-point' function from 'thing-at-point.el'
;; depends on the buffer syntax table.  For example, in text mode, it
;; thinks that a number before a decimal is a complete sexp (that's
;; bad, because then our functions cannot see floating point numbers),
;; whereas in emacs lisp mode, it treats the entire number as the
;; sexp.  For example, if in text-mode you put the cursor on the "9"
;; in "98.5" and eval (number-at-point), it returns 98.  If you are in
;; emacs-lisp-mode, it returns 98.5.


(defun gb-compute-row (row-name function)
  "Compute a new row by applying a function."
  ;; the function must be one that can take an arbitrary number of arguments, e.g. '+
  (interactive)
  (save-excursion
  (let ((last-row-number (apply 'max (mapcar 'cdr gb-row-names))))
    (goto-line (+ 1 last-row-number)))
  (insert (concat "\n" row-name))
  ;; remove from row list any row whose title is the string kept in gb-key-name
  (let ((reduced-row-list (gb-remove-element (assoc gb-key-name gb-row-names) gb-row-names))
	(my-columns (mapcar 'cdr gb-col-names)))
    (while my-columns
      (move-to-column (car my-columns) t)
      (insert (format "%.2f" (gb-function-on-column function (car my-columns) (mapcar 'cdr reduced-row-list))))
      (setq my-columns (cdr my-columns))))))

; auxiliary function

(defun gb-function-on-column (function col-num row-list)
  "Apply FUNCTION to numbers in a column."
  ; the function must be one that takes a list as its argument
  ;
  ; we can define such a function from, say, + by using 
  ; (defun adder (list) (apply '+ list))
  (save-excursion
    (let ((num-list (gb-col2list col-num row-list)))
      (if num-list
	  (funcall function num-list)
	(message "Did not find any numbers in the column.")))))

; auxiliary function

(defun gb-col2list (col-num row-list)
  "Grab numbers from COL-NUM that occur on the rows in ROW-LIST."
  (save-excursion
    (let ((num-list '()))
      (while row-list
	(goto-line (car row-list))
	(move-to-column col-num)
	;; don't use 'add-to-list', that is like set theoretic union
	; (add-to-list 'num-list (cons (car row-list) (number-at-point)))
	(setq num-list (cons (number-at-point) num-list))
	(setq row-list (cdr row-list)))
    num-list)))

; auxiliary function

(defun gb-list-memb (number list)
  (car (nthcdr number list)))

;;; moving around in columns and rows.  

(defun gb-move-right ()
  (interactive)
  (let* ((col-nums (mapcar 'cdr gb-col-names))
	 (supremum (gb-supremum (current-column) col-nums)))
    ; force movement on blank rows
    (move-to-column supremum t)))


(defun gb-move-left ()
     (interactive)
  (let* ((col-nums (mapcar 'cdr gb-col-names))
	 (infimum (gb-infimum (current-column) col-nums)))
    (move-to-column infimum)))


(defun gb-move-down ()
  (interactive)
  ; maintain column
  (let* ((row-nums (mapcar 'cdr gb-row-names))
	 (supremum (gb-supremum (line-number-at-pos) row-nums))
	 (my-column (current-column)))
    (goto-line supremum)
    (move-to-column my-column)))
    

(defun gb-move-up ()
  (interactive)
  ; maintain column
  (let* ((row-nums (mapcar 'cdr gb-row-names))
	 (infimum (gb-infimum (line-number-at-pos) row-nums))
	 (my-column (current-column)))
    (goto-line infimum)
    (move-to-column my-column)))


; auxiliary functions

(defun gb-keep-greater (number list)
  ;; given NUMBER, return the sublist of LIST of numbers that are greater
  (cond
   ((not list) nil)
   ((> (car list) number)
    (cons (car list) (gb-keep-greater number (cdr list))))
   (t (gb-keep-greater number (cdr list)))))

(defun gb-supremum (number list)
;; given NUMBER, return the smallest element of LIST that is greater  
  (let* ((sublist (gb-keep-greater number list))
	 (sorted-sublist (sort sublist '<)))
    (car sorted-sublist)))

(defun gb-keep-less (number list)
  ;; given NUMBER, return the sublist of LIST of numbers that are less
  (cond
   ((not list) nil)
   ((< (car list) number)
    (cons (car list) (gb-keep-less number (cdr list))))
   (t (gb-keep-less number (cdr list)))))

(defun gb-infimum (number list)
;; given NUMBER, return the smallest element of LIST that is greater  
  (let* ((sublist (gb-keep-less number list))
	 (sorted-sublist (sort sublist '>)))
    (car sorted-sublist)))

;; what if we are already to the right of the rightmost column?


(provide 'gradebook)

;; TO DO:
 
;; 1. If a student is missing a grade, then the gb-compute-column
;; function will fail.  Of course, one can just insert "0" in the
;; relevant place.  But it might be desirable to have a way of
;; omitting students from the calculation if their record does not
;; have the requisite grades.

;; 2. Make "Key" and "Median" rows begin with a special symbol that
;; excludes them from the row regexp

;; 3. Consider making "gb-compute-column" an interactive function

;; 4. Make "compute-row" skip special rows -- e.g. rows already
;; inserted with calculations

;; 5. Add or remove spacing between columns (stretch or contract)

;; 6. Custom variable for how many decimal places to display


;;; gradebook.el ends here
