;;; eform.el
;; This file is free software.
;; Copyright Greg Novak (novak@ucolick.org) December 2004
;; Released under the GPL, available at http://www.gnu.org
;;
;; I wrote this because I was keeping my cycling log as a text file in
;; emacs.  I want to do a few simple things like total up the number
;; of miles or number of hours I've ridden, but a full-blown
;; spreadsheet seems like overkill for this.  So I defined a simple
;; mark-up that instructs emacs to sum up the numbers in a certain
;; rectangle.  I also wanted to create sub-totals which were then
;; added together to produce a grand total, so there's also a facility
;; for adding up all of the subtotals matching some regexp.
;;
;; I call this "Electric Forms Mode" because it functions sort of like
;; a web form where you fill in a few numbers and then they get
;; crunched into some sort of result, but the format is not so rigid
;; as a spreadsheet.
;;
;; To install, you'll probably want this in your .emacs file:
;; (require 'eform)
;; And you might want something like this:
;; (global-set-key "\C-cu" (lambda () (interactive) (eform-mode t) (eform-update)))
;;
;; I'll explain the format with an example.  You have a file that
;; looks like this (without the semicolons):
;;
;;     (#dist-jan  (#time-jan
;; 1-1   11.1         0:38
;; 1-2   15.3         1:01 
;; 1-3    5.5         0:25
;;     #dist-jan)   #time-jan)
;;
;;     (#dist-feb  (#time-feb
;; 2-1   11.0         0:37
;; 2-2   15.2         1:00 
;; 2-3    5.4         0:24
;;     #dist-feb)   #time-feb)
;;
;; SUBTOTALS
;; #dist-jan ()
;; #time-jan fhm + thm ()
;; #dist-feb ()
;; #time-feb fhm + thm ()
;;
;; GRAND TOTALS
;; (dist-.*) #total-dist ()
;; (time-.*) #total-time ()
;;
;; 
;; So, # identifies all the places where something interesting is
;; going to happen.  The (#name and #name) tags denote the beginning
;; and ending of rectangles that will be summed up.  #name ()
;; indicates where the result will go.  So, when you invoke enable
;; eform-mode and then invoke eform-update, this code will sum up all
;; the numbers in the rectangles demarcated by (#dist-jan and
;; #dist-jan) and then stick the result between the parenthesis on the
;; line #dist-jan ().
;;
;; Consider the case of times.  I don't want to have to type in the
;; length of my rides using only minutes (ie, 90 minutes instead of
;; 1:30).  Therefore you can specify a mapping function, a
;; reduction function, and a post-processing function on the line that
;; will hold the result of a computation.  These are fhm, +, and thm
;; above.  fhm takes a string of the form "hour:min" and converts it
;; to an integer number of minutes.  The function + is then used as a
;; binary "reduction" operator to combine all of the resulting
;; integers.  Finally thm is applied to the result.  It takes an
;; integer number of minutes and converts it to a string of the form
;; "hour:minute" which will be inserted back into the buffer between
;; the parenthesis on the "result" line #time-jan fhm + thm ()
;;
;; Note that these functions under discussion are just plain old elisp
;; functions looked up by name.  So you can easily write your own
;; functions to massage your data as needed.
;; 
;; You don't have to specify all of the the map, reduction, and post
;; functions.  They default to string-to-number, +, and
;; number-to-string, respectively.  If you specify one, two, or three
;; function, then eform-mode takes a guess about what you mean as
;; follows: 
;; #name reduction () 
;; #name map reduction () 
;; #name map reduction post () 
;; ie, first you override the reduction function,
;; then the mapping function, and finally the postprocessing
;; function.
;;
;; There's actually a fourth function you can specify which is a
;; preprocessing function.  It gets a crack at transforming the data
;; before anything else sees it.  This lets you write this:
;;
;; (#name 1 2 3 4 #name) 
;; #name split string-to-number * number-to-string ()
;; 
;; Ie, all the data is on one line instead of being on separate lines.
;; The function split is defined below and it simply splits up the
;; data so that the other functions can do their work.  Here 24 will
;; appear between the parenthesis.
;;
;; To summarize:
;; Function     Arg type          Result type   Default
;; preprocess   list of strings   list of A     identity
;; mapping      A                 B             string-to-number
;; reduction    B,B               C             +
;; postprocess  C                 string        number-to-string
;;
;; where A, B, and C are any types.
;;
;; Now, there's one final form of "result" line that lets you take the
;; results of previous calculations and use them somehow.  These are
;; the total-dist and total-time lines in the example above.  The set
;; of parenthesis on the left holds a regexp and the meaning of the
;; line is "Look for all previous computations in the buffer with a
;; name matching regexp and operate on those results."  Everything
;; else is the same as the "rectangle" style of data input--you can
;; specify up to four functions, etc.
;;
;; Now the documentation is getting as long as the code, so I'll stop.
;;
;; If you actually get any use out of this, e-mail me.  Suggestions
;; are welcome.  This is my first crack at elisp, though, so be kind.  
;; 
;; To Do list for Version 2:
;; 1) recalc until buffer stops changing.
;; 2) use buffer local lisp variables for results of computations?
;; 3) Figure out how to exclude open/close characters from .* regexp in result field
;; 4) Clear all results function
;; 5) Bug when total-d-dec starts with a d..

;;(require 'gsn)

(defvar eform-mode nil "Electric Form Mode")
(make-variable-buffer-local 'eform-mode)
(add-to-list 'minor-mode-alist '(eform-mode " EForm"))

;; "primitives" used to build up regexps
(setq eform-beg "(")
(setq eform-end ")")
(setq eform-tag "#")
(setq eform-char "[A-z0-9+/\*-]")
(setq eform-ws "[ \t]")

;; regexps
(setq eform-result-regexp-format (concat 
			     "\\(?:" eform-beg "\\(.*\\)" eform-end "\\)?" eform-ws "*"
			     ; eform-tag "\\(" eform-char "+\\)" eform-ws "*" 
			     eform-tag "\\(%s\\)" eform-ws "*"
			     "\\(" eform-char "*\\)" eform-ws "*"
			     "\\(" eform-char "*\\)" eform-ws "*"
			     "\\(" eform-char "*\\)" eform-ws "*"
			     "\\(" eform-char "*\\)" eform-ws "*"
			     eform-beg "\\(.*\\)" eform-end))

;; Should the whitespace be allowed?  Causes problems for whitespace splits
(setq eform-beg-rectangle-format (concat eform-beg 
					   eform-ws "*"
					   eform-tag "%s"))

(setq eform-end-rectangle-format (concat eform-tag "%s" 
					   eform-ws "*"
					   eform-end))

(defun split (lst)
  ;; Assumes that list has only one element
  (split-string (car lst)))

(defun fhm (str)
  "Convert from hour:minute to number of minutes"
  (let ((split (split-string str ":")))
    (if (= (length split) 1)
	;; only minutes
	(string-to-number str)
      (+ (* 60 (string-to-number (car split))) (string-to-number (cadr split))))))

(defun thm (total)
  "Convert from number of minutes to hour:minute"
  (let ((hour (/ total 60))
	(min (mod total 60)))
    (format "%d:%02d" hour min)))

(defun eform-mode (&optional arg)
  "Mode for doing light numerical calculations in buffers."
  (interactive "P")
  (setq eform-mode 
	(if (null arg) (not eform-mode)
	  (> (prefix-numeric-value arg) 0))))

(defun eform-extract-regexp (&optional regexp)
  "Extract data for results matching regexp"
  (mapcar (lambda (x) (nth 6 x)) (eform-find-all-results regexp)))

(defun eform-extract-rectangle (name)
  "Extract data from recangle
name is a string naming the rectangle
returns a list of strings"
  (save-excursion
    (let (start end)
      (goto-char (point-max))
      (setq start (re-search-backward (format eform-beg-rectangle-format name) nil t))
      (goto-char (point-min))
      (setq end   (re-search-forward  (format eform-end-rectangle-format name) nil t))
      (extract-rectangle start end))))

(defun eform-incremental-find-result (&optional name)
  "Find the next result corresponding to regexp <name>
Return list containing:
0 regexp contributing to result (string)
1 name of result (string)
2 preprocessing function list of strings -> obj-1
3 mapping function         obj-1 -> obj-2
4 operator for reduction   obj-2, obj-2 -> obj-2
5 postprocessing function obj-2 -> string 
6 current result (string)
7 result start (int)
8 result end (int)"
  (if (not (re-search-forward 
	    (format eform-result-regexp-format
		    (if (not (null name)) name
		      (concat eform-char "+"))) nil t))
      ;; if no match, return nil
      nil
    ;; otherwise, do a bunch of shit
    (let (pre map op post)    
      (cond ((string= (match-string 3) "") 
	     ;; Specified zero functions
	     (progn (setq pre  (symbol-function 'identity))
		    (setq map  (symbol-function 'string-to-number))
		    (setq op   (symbol-function '+))
		    (setq post (symbol-function 'number-to-string))))
	    ((string= (match-string 4) "") 
	     ;; Specified one function
	     (progn (setq pre  (symbol-function 'identity))
		    (setq map  (symbol-function 'string-to-number))
		    (setq op   (symbol-function (intern-soft (match-string 3))))
		    (setq post (symbol-function 'number-to-string))))
	    ((string= (match-string 5) "") 
	     ;; Specified two functions
	     (progn (setq pre  (symbol-function 'identity))
		    (setq map  (symbol-function (intern-soft (match-string 3))))
		    (setq op   (symbol-function (intern-soft (match-string 4))))
		    (setq post (symbol-function 'number-to-string))))
	    ((string= (match-string 6) "") 
	     ;; Specified three functions
	     (progn (setq pre  (symbol-function 'identity))
		    (setq map  (symbol-function (intern-soft (match-string 3))))
		    (setq op   (symbol-function (intern-soft (match-string 4))))
		    (setq post (symbol-function (intern-soft (match-string 5))))))
	    ;; Specified four functions
	    (t (progn (setq pre  (symbol-function (intern-soft (match-string 3))))
		      (setq map  (symbol-function (intern-soft (match-string 4))))
		      (setq op   (symbol-function (intern-soft (match-string 5))))
		      (setq post (symbol-function (intern-soft (match-string 6)))))))        
      (list 
       (match-string 1)                     ; 0 regexp   
       (match-string 2)                     ; 1 name     
       pre                                  ; 2 pre	     
       map                                  ; 3 map	     
       op                                   ; 4 operator 
       post                                 ; 5 post     
       (match-string 7)                     ; result     
       (match-beginning 7)                  ; result start
       (match-end 7)))))		   	; result end 
  
(defun eform-find-result (&optional name)
  "Find first result field with name matching regexp <name>"
  (save-excursion     
   (goto-char (point-min))
   (eform-incremental-find-result name)))

(defun eform-find-all-results (&optional name)
  "Return a list of all result fields in the document."
  (let (this-result results)
    (save-excursion
      (goto-char (point-min))
      (setq this-result (eform-incremental-find-result name))
      (while (not (null this-result))
	(setq results (cons this-result results))
	(setq this-result (eform-incremental-find-result name))))
    (reverse results)))

(defun eform-find-all-names (&optional regexp)
  "Return a list of all names matching regexp"
  (mapcar (lambda (x) (nth 1 x)) (eform-find-all-results regexp)))

(defun eform-reduce (data pre map op post)
  "Reduce data.  Take a list of strings, apply pre, map, reduction operator, and post functions to it."
  (funcall post (reduce op (mapcar map (funcall pre data)))))

(defun reduce (f lst &optional r)
  "Reduce <lst> with binary function <f> in a left-associative manner"
  (cond ((null lst) r)
	((null r) (reduce f (cdr lst) (car lst)))
	(t (reduce f (cdr lst) (funcall f r (car lst))))))

(defun eform-update-result (name)
  "Update result <name>"
  (let* ((dest (eform-find-result name))
	 (regexp (nth 0 dest))
	 (pre (nth 2 dest))
	 (map (nth 3 dest))
	 (op (nth 4 dest))
	 (post (nth 5 dest))
	 (start (nth 7 dest))
	 (end (nth 8 dest))
	 (data (if (not (null regexp))
		   (eform-extract-regexp regexp)
		 (eform-extract-rectangle name))))
    (save-excursion      
      (delete-region start end)
      (goto-char start)
      (insert (eform-reduce data pre map op post)))))

(defun eform-update ()
  "Update all electric forms"
  (interactive)
  (message (concat "Found names: "
		   (reduce 'concat 
			   (mapcar (lambda (x) (concat x " ")) 
				(eform-find-all-names)))))
  (if eform-mode 
      (dolist (result (eform-find-all-names))
	(message (concat "Working on " result))
	(eform-update-result result))))
  
(provide 'eform)
;;; eform.el ends here
