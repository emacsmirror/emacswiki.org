;;; tjicutil-funcs.el
;;
;;  Copyright (C) 2002-2003 Travis J.I. Corcoran
;;
;; This is free software.
;
;; Author: Travis J.I. Corcoran <tjic_emacs@tjic.com>
;; Version: $Header: /var/cvsroot/tjiclisp/tjicutil-funcs.el,v 1.9 2003/05/23 19:06:28 tjic Exp $
;; Keywords: 

;;; Commentary:


(provide 'tjicutil-funcs)

(defun tjic-regexp-create-OR (&rest l)
  "turn a sequence of strings into a single regexp string that 
   concats them, surrounded by \\(...\\) and seperated by \\|

   NOTE: works either on multiple arguments, each a string, or
   a single argument which is a list of strings.

   17 May 2002 - after a 5 day, 82 hr workweek, getting Permabit v1.0beta0 done"


   (let ((outputstr)
		 (firstpassB t))
	 (if (and ( eq 1 (length l))
			  (listp (car l)))
		 ;; l was a list before it was passed in
		 (setq l (car l)))
	 ;; POST-CONDITION: l is a list
	 ;; 
	 (setq outputstr "\\(")
	 (while (car l)
	   (if (not firstpassB)
		   (setq  outputstr (concat outputstr "\\|")))
	   (setq firstpassB nil )
	   (setq outputstr (concat outputstr (car l)))
	   (setq l (cdr l)))
	 
	 (setq outputstr (concat outputstr "\\)"))
	 outputstr))


; Test:
; (tjic-string-to-list "," "foo,bar,baz,quux")


;;----------
;; TEST CASES
;;----------
; (tjic-create-regexp-OR "a" "b" "v")
; (let ((z (list "a" "b" "v")))
;	 (tjic-create-regexp-OR z))

(defun tjic-regexp-a-then-not-b (need exclude)
  "create a regexp that matches all cases where expression
a is present, but is not immediately followed by expression b.
Examples: all places where 't' is present, and is not followed by 'jic',
and allowing 'tX', 'tjX', and 'tjiX')

Bug: resulting regexp will not work correctly in string-match against
string that is a truncated version of the exclude string (e.g.: the
output of this func on 't', 'jic' does not match against 'tj$', where
$ is the end of line char)."

  ; build a string starting w regexp choice begin
  (concat
   "\\("

   ; loop, building list of alternatives
   (let ((i 0) (len (length exclude)) (notstr "") (choicestr "")  )
	 (while (< i len)

	   (if (not (eq i 0))
			 (setq notstr
				   (concat notstr 
						   (substring exclude (- i 1) i))))

	   (setq choicestr 
			 (concat choicestr
					 (if (not (eq i 0)) "\\|")

					 ;need
					 ;notstr
					 ;"\\$"
					 ;"\\|"


					 need
					 notstr
					 "[^"
					 (substring exclude i (+ i 1))
					 "]"))

										; iterate
	   (setq i (+ i 1))) 
										; return the choicestr
	 choicestr)  

   "\\)"))

(defun tjic-string-to-list (separator string)
  "given a string, tokenize along separators and return as a list of elements"
  (let ((ptA) 
		(ret-list nil))
	(with-temp-buffer
	  (insert string)
	  (goto-char (point-min))
	  (setq ptA (point))
	  (while 
		  (search-forward separator nil t)
		(progn
		  (setq ret-list (cons (buffer-substring ptA (- (point) 1) ) ret-list))
		  (setq ptA (point) )))
	  (goto-char (point-max))
	  (setq ret-list (cons (buffer-substring ptA (point) ) ret-list))

	  (reverse ret-list))))


;;--------------------------------------------------
;;  assoc funcs
;;--------------------------------------------------


(defun tjic-assoc-both  (key alist compare-func)
  "This function is like assoc in that it looks for KEY in ALIST but
	(1) the COMPARE-FUNC is user-specified
	(2) it returns then full entry - a list of key and value. 
	The latter is interesting in cases where matching is fuzzy:
	trying to match with compare-func set equal to
	tjic-strings-strA-is-in-strB means that we might not know what lvalue
	in the alist our key triggered.  This func lets us know.
	TJIC 30 Apr 2003"

  (let ((ret nil)
		(foundP nil)
		(tail alist))
	(while (and (not foundP) tail)
	  (if (funcall compare-func key (car (car tail)))
		  (progn 
			(setq foundP t)
			(setq ret (car tail))))
	  (setq tail (cdr tail)))
	ret))


; test:
;    (tjic-assoc-both "a" '(("b" . "wrong") ("ar" . "medium") ("a" . "short") ( "bar" . "long"))  'equal) 
;       ==> ("a" . "short" )
;    (tjic-assoc-both "a" '(("b" . "wrong") ("ar" . "medium") ("a" . "short") ( "bar" . "long"))  'tjic-strings-strA-is-in-strB)
;       ==> ("ar" . "medium" )

(defun tjic-assoc (key alist compare-func)
  "This function is like assoc but the comparison function is
user-specified.

TJIC 28 Apr 2003"
  
  (cdr (tjic-assoc-both key alist compare-func)))
; test:
;    (tjic-assoc "a" '(("b" . "wrong") ("ar" . "medium") ("a" . "short") ( "bar" . "long"))  'equal)
;       ==> "short"
;    (tjic-assoc "a" '(("b" . "wrong") ("ar" . "medium") ("a" . "short") ( "bar" . "long"))  'tjic-strings-strA-is-in-strB)
;       ==> "medium"


(defun tjic-substr-assoc (key alist)
  "This function is like assoc in that it returns the first
association for KEY in ALIST, but it makes the comparison using 
tjic-strings-strA-is-in-strB instead of equal.

NOTE: it does not return the best match (whatever that might mean),
just the first match.  Thus, with an alist of the form

   (setq zlist '( (\"ar\" . \"medium\") (\"a\" . \"short\") (\"bar\" \"long\")))

calling

   (tjic-substr-assoc \"a\" zlist) will return \"medium\".

TJIC 28 Apr 2003"

  (tjic-assoc key alist 'tjic-strings-strA-is-in-strB))

; test:
;   (tjic-substr-assoc "a" '(("b" . "wrong") ("ar" . "medium") ("a" . "short") ( "bar" . "long")))
;     ==> medium

(defun tjic-prefixstr-assoc (key alist)
  "This function is like assoc in that it returns the first
association for KEY in ALIST, but it makes the comparison using 
tjic-str1-is-prefix-of-str2 instead of equal.

NOTE: it does not return the best match (whatever that might mean); just the first match.  Thus, with an 
alist of the form  
   (setq zlist '( (\"b\" . \"wrong\") 
                  (\"ra\" . \"present, but not prefix\")
                  (\"abb\" . \"YES\")
                  (\"aabb\" . \"valid, but too late\")))
calling
  (tjic-prefixstr-assoc \"a\" zlist) will return \"YES\".


TJIC 30 Apr 2003"

  (tjic-assoc key alist 'tjic-str1-is-prefix-of-str2))


; test:
;   ( tjic-prefixstr-assoc "a" '(("b" . "wrong") ("ra" . "not prefix") ("abb" . "YES") ( "aabb" . "valid, but too late")))
;     ==> YES



(defun tjic-list-alist-assoc (key-list alist &optional compare-func)
  "This takes a key list and an alist.  It tries each
key in the KEY-LIST against the the ALIST,  as follows:
the first key is tried against the entire alist, 
if no match is found, then the second key is tried against the entire alist,
etc.

As soon as any entry in the key-list matches against the alist, the
result of that match is returned.  The optional argument compare-func
allows the caller to specify what function should be used to do
comparisons.  By default 'equal' is used.

TJIC 28 Apr 2003"
  (if (not compare-func) 
	  (setq compare-func 'equal))

  (let ((ret nil)
		(key-list-tail key-list))

		(while (and (not ret) key-list-tail)
		  (setq ret (tjic-assoc (car key-list-tail) alist compare-func))
		  (if ret
			  (setq foundP t))
		  (setq key-list-tail (cdr key-list-tail)))
		ret))

; test:
;   (tjic-list-alist-assoc (list "z") '(("b" . "wrong") ("ar" . "medium") ("a" . "short") ( "bar" . "long")) )
;         ==> nil
;   (tjic-list-alist-assoc (list "a" "z") '(("b" . "wrong") ("ar" . "medium") ("a" . "short") ( "bar" . "long")) ) 
;         ==> short
;   (tjic-list-alist-assoc (list "z" "a") '(("b" . "wrong") ("ar" . "medium") ("a" . "short") ( "bar" . "long")) ) 
;         ==> short
;   (tjic-list-alist-assoc (list "a" ) '(("b" . "wrong") ("ar" . "med") ("a" . "short") ( "bar" . "long")) 'tjic-strings-strA-is-in-strB)
;         ==> med
;
	


;;
;; utility
;;
(defun tjic-str1-is-prefix-of-str2 (str1 str2)
  "returns bool: is str1 the begining of str2?"
  (if (not (stringp str1)) (error "str1 not string"))
  (if (not (stringp str2)) (error "str2 not string"))
  (if (>(length str1) (length str2))
	  nil
	(string= str1 (substring str2 0 (length str1)))))

(defun tjic-str2-is-prefix-of-str1 (str1 str2)
  ""
  (tjic-str1-is-prefix-of-str2 str2 str1))

(defun tjic-str2-in-excess-of-str1 (str1 str2)
  "if str1 is the prefix of str2, return str2 after that (NOTE: str1 *must* be a prefix)"
  (if (not (tjic-str1-is-prefix-of-str2 str1 str2))
	  (error "str1 not prefix str2"))
  (substring str2 (length str1)))

(defun tjic-str2-after-of-str1 (str1 str2)
  "if str1 occurs in str2, return str2 after 1st occurance (NOTE: str1 need not be prefix)"
  (let ((pos  (tjic-strings-find-pos-of-first str2 str1)))
	(if (not pos)
	  (error "str1 not present in str2"))
	(substring str2 (+ 1 pos) (length str2))))

; (tjic-str2-after-of-str1 "@" "foo@bar")
; (tjic-str2-after-of-str1 "@" "foobar")
; (tjic-str2-after-of-str1 "@" "foo@baz@bar")

(defun tjic-str2-is-suffix-of-str1 (str1 suffix)
  "returns bool: is suffix the suffix in str1?"
  (if (not (stringp str1)) (error "str1 not string"))
  (if (not (stringp suffix)) (error "str2 not string"))
  (if (<(length str1) (length suffix))
	  nil
	(string=  (substring str1
						 (- (length str1) (length suffix))
						 (length str1))
			  suffix)))

; test: 
;  (tjic-str2-is-suffix-of-str1 "foo.el" ".el") 
;    ==> t
;
;  (tjic-str2-is-suffix-of-str1 "foo.el" "foo.el.el") 
;    ==> nil
;
;  (tjic-str2-is-suffix-of-str1 "foo.el" "il") 
;    ==> nil


(defun tjic-strings-remove-suffix (str suffix)
  "return the value of str minus suffix"
  (if (not (tjic-str2-is-suffix-of-str1 str suffix))
	  (error "suffix not suffix of str"))
  (substring str 0 (- (length str)
					  (length suffix))))

; test:
;  (tjic-strings-remove-suffix "foo.el" ".el")

(defun tjic-strings-strA-is-in-strB (strA strB)
  ""
  (if (tjic-strings-find-pos-of-last strB strA) t nil))
;; test:
;;   (tjic-strings-strA-is-in-strB "a" "ab")
;;   (tjic-strings-strA-is-in-strB "a" "a")
;;   (tjic-strings-strA-is-in-strB "z" "ab")
;;   (tjic-strings-strA-is-in-strB "zaz" "zab")
;;   (tjic-strings-strA-is-in-strB "zaz" "zaz")

(defun tjic-strings-strB-is-in-strA (strA strB)
  ""
  (tjic-strings-strA-is-in-strB strB strA))

(defun tjic-strings-find-pos-of-last (str substr)
  "return the integer position of the *last* occurance of substr in str
   return nil if not present"
  (let ((iter (- (length str) (length substr)))
		(retval nil)
		(doneB nil))

	(while (and (not doneB) (>= iter 0))
	  (if (string= (substring str iter (+ iter (length substr)))
				substr)
		  (progn
			(setq retval iter)
			(setq doneB t)))
	  (setq iter (- iter 1)))
	retval))


; test:
;  (tjic-strings-find-pos-of-last "abc" "c")
;     ==> 2
;  (tjic-strings-find-pos-of-last "abc" "b")
;     ==> 1
;  (tjic-strings-find-pos-of-last "abc" "a")
;     ==> 0
;  (tjic-strings-find-pos-of-last "abccc" "c")
;     ==> 4
;  (tjic-strings-find-pos-of-last "abccc" "z")
;     ==> nil
;  (tjic-strings-find-pos-of-last "abccc" "bc")
;     ==> 1

(defun tjic-strings-tokenize-from-end (strA separator)
  ""
  (let* ((sep (tjic-strings-find-pos-of-last strA separator))
		 (back (substring strA (if sep (+ 1 sep) 0)))
		 (front (substring strA 0 sep)))

	(list front back)))

; (tjic-strings-tokenize-from-end "a/b/c" "/")
;    ==> "a/b" "c"
; (tjic-strings-tokenize-from-end "a/b/cde" "/")
;    ==> "a/b" "cde"
; (tjic-strings-tokenize-from-end "abcde" "/")
;    ==> "abcde"


(defun tjic-strings-find-pos-of-first (str substr)
  "return the integer position of the *first* occurance of substr in str
   return nil if not present"
  (let ((iter 0)
		(retval nil)
		(doneB nil))

	(while (and (not doneB) (< iter (length str)))
	  (if (string= (substring str iter (+ iter (length substr)))
				substr)
		  (progn
			(setq retval iter)
			(setq doneB t)))
	  (setq iter (+ iter 1)))
	retval))

; test:
;  (tjic-strings-find-pos-of-first "abc" "c")
;     ==> 2
;  (tjic-strings-find-pos-of-first "abc" "b")
;     ==> 1
;  (tjic-strings-find-pos-of-first "abc" "a")
;     ==> 0
;  (tjic-strings-find-pos-of-first "abccc" "c")
;     ==> 2
;  (tjic-strings-find-pos-of-first "abccc" "z")
;     ==> nil
;  (tjic-strings-find-pos-of-first "abccc" "bc")
;     ==> 1

(defun tjic-strings-find-suffix-after-delim (str delim)
  "given a separator char or substring, return the suffix following
   the last delimiter from input str.  e.g.  'foo.cpp', '.' --> 'cpp'"
  (let ((pos (tjic-strings-find-pos-of-last str delim)))
	(if (eq nil pos)
		; not found
		nil
	  ; found
	  (substring str (+ 1 pos)))))


(defun tjic-strings-find-prefix-before-delim (str delim)
  "given a delimiter char or substring, return the suffix following
   the last delimiter from input str.  e.g.  'foo.cpp', '.' --> 'cpp'"
  (let ((pos (tjic-strings-find-pos-of-first str delim)))
	(if (eq nil pos)
		; not found
		nil
	  ; found
	  (substring str 0 pos))))

; test:
;   (tjic-strings-find-prefix-before-delim "foo.cpp" ".")
;     ==> foo
;   (tjic-strings-find-prefix-before-delim "foo.cpp" "o")
;     ==> f
;   (tjic-strings-find-prefix-before-delim "foo.cpp" "f")
;     ==> ""
;   (tjic-strings-find-prefix-before-delim "foo.cpp" ":")
;     ==> nil


(defun tjic-strings-replace-X-with-Y (string old new)
  "given a string STRING replace occurances of OLD with NEW.
OLD is treated as a regexp"
  (let ((ret))
	(with-temp-buffer
	  (insert string)
	  (goto-char (point-min))
	  (replace-regexp old new)
	  (setq ret (buffer-substring 1 (point-max))))))

(defun tjic-byte-compile-load (fname)
  "Given a file name, find file in load path, byte-compile, then load .elc.
Base stolen from DaveG @ Permabit 2002.  
Tweaked, renamed"
  ;; canonicalize fname
  (if (not (tjic-str2-is-suffix-of-str1 fname ".el"))
	  (setq fname (concat fname ".el")))

  (let ((loadindex load-path)
		(loaddir))
	(while loadindex
	  (progn
		(setq loaddir (car loadindex))

		(let* ((elfile (expand-file-name fname loaddir))
			   (elcfile (concat elfile "c")))

		  (if (file-exists-p elfile)
			  ;; .el file found
			  ;;
			  (progn
				(if (or (not (file-exists-p elcfile))
						(file-newer-than-file-p elfile elcfile))
					(byte-compile-file elfile))
				(load-file elcfile)
				(setq loadindex nil))

			;; .el file not found
			;;
			(setq loadindex (cdr loadindex))))))))


;;; tjicutil-funcs.el ends here
