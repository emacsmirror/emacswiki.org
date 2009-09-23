;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is mon-hash-utils.el.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-hash-utils provides procedures to extend Elisp hash table functionality.
;;; The various authors/sources of routines are identified below and inline.
;;;
;;; !!!The MON KEY does not claim authorship of _any_ of the individual 
;;; components included of this file. The only act of authorship on MON's part
;;; is their assembly in the aggregaate.!!!
;;;
;;; FUNCTIONS:►►►
;;; `mon-hash-all-values',`mon-hash-all-keys',`mon-hash-to-list'
;;; `mon-hash-key-onto-list',`mon-hash-describe',`mon-hash-describe-descend'
;;; `mon-hash-readlines-file' , `mon-hash-readlines-buffer'
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;;
;;; MACROS:
;;; `mon-hash-get-items',`mon-hash-get-values', `mon-hash-get-symbol-keys', 
;;; `mon-hash-has-key', `mon-hash-get-string-keys', `cl-put-hash'
;;; 
;;; 
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED-AND-MOVED: 
;;; <Timestamp: Friday May 15, 2009 @ 02:24.59 PM - by MON KEY>
;;; `hash-get-values'       <- macros-func-thierry.el
;;; `hash-get-symbol-keys'  <- site-lisp/macros-func-thierry.el
;;; `hash-has-key'          <- site-lisp/macros-func-thierry.el
;;; `hash-get-string-keys'  <- site-lisp/macros-func-thierry.el
;;; `cl-put-hash'           <- site-lisp/macros-func-thierry.el
;;;
;;; REQUIRES:
;;;
;;; TODO:
;;; Import relevant portions of Beebe's hash routines.
;;;
;;; NOTES:
;;; See also: (elisp hash functions) (info "(elisp)Hash Tables"); 
;;; `make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
;;; `define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table', 
;;; `hash-table-count', `hash-table-test', `hash-table-weakness',
;;; `hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;; Majority of functions defined here are from Xah Lee's:
;;; (URL `http://xahlee.org/emacs/elisp_hash_table.html')
;;; -
;;; Author: Thierry Volpiatto's - Copyright (C) 2008 - "macros-func-thierry.el"
;;; (URL `http://www.emacswiki.org/emacs/HashMap')
;;;
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-hash-utils.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-09-22} - by MON KEY>
;;;
;;; FILE-CREATED:
;;; <Timestamp: Friday May 15, 2009 @ 02:18.14 PM - by MON KEY>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; CODE:

;;; ==============================
;; Used in Thierry's macros
(eval-when-compile (require 'cl)) 

;;; =======================
;;; Courtesy: Thierry Volpiatto - HIS: traverselisp.el WAS: `traverse-hash-readlines'
;;; (URL `http://freehg.org/u/thiedlecques/traverselisp/')
(defun mon-hash-readlines-file (file table)
  "Load all the lines of a file in an hash-table with the number of line as key.
See also; `mon-hash-readlines-buffer'."
  (let* ((my-string (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string)))
          (my-read-list (split-string my-string "\n"))
          (count 0))
     (dolist (i my-read-list)
       (puthash count i table)
       (incf count))))

;;; =======================
;;; COURTESY: Thierry Volpiatto HIS: traverselisp.el WAS: `traverse-hash-readlines-from-buffer'
(defun mon-hash-readlines-buffer (buffer table)
  "Load all lines of buffer in an hash-table with the number of line as key.
See also; `mon-hash-readlines-file'."
  (let* ((my-string (with-temp-buffer
                       (insert-buffer-substring buffer)
                       (buffer-string)))
          (my-read-list (split-string my-string "\n"))
          (count 0))
     (dolist (i my-read-list)
       (puthash count i table)
       (incf count))))

;;; ==============================
;;; COURTESY: Xah Lee
;;; (URL `http://xahlee.org/emacs/elisp_hash_table.html')
;;; Following hash functions: `hash-all-values', `hash-all-keys', `hash-to-list'
(defun mon-hash-all-values (hashtable)
  "Return all values in HASHTABLE.
See also; <FUNCTION(S)>:
`mon-hash-all-keys',`mon-hash-to-list',`mon-hash-describe',
`mon-hash-describe-descend'.\n
See also; <MACRO(S)>:
`mon-hash-get-items',`mon-hash-get-values', `mon-hash-get-symbol-keys', 
`mon-hash-has-key', `mon-hash-get-string-keys', `cl-put-hash'.\n
See also; (elisp hash functions) - info node `(elisp)Hash Tables'
`make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
`define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table',    
`hash-table-count', `hash-table-test', `hash-table-weakness',	      
`hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'."
  (let (allvals)
    (maphash 
     (lambda (kk vv) 
       (setq allvals (cons vv allvals)))
     hashtable)
    allvals))
;;
(defun mon-hash-all-keys (hashtable)
  "Return all keys in HASHTABLE.
See also; <FUNCTION(S)>:
`mon-hash-all-values',`mon-hash-to-list',`mon-hash-describe',
`mon-hash-describe-descend'.\n
See also; <MACRO(S)>:
`mon-hash-get-items',`mon-hash-get-values', `mon-hash-get-symbol-keys', 
`mon-hash-has-key', `mon-hash-get-string-keys', `cl-put-hash'.\n
See also; (elisp hash functions) - info node `(elisp)Hash Tables'
`make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
`define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table',    
`hash-table-count', `hash-table-test', `hash-table-weakness',	      
`hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'."
  (let (allkeys)
    (maphash 
     (lambda (kk vv) 
       (setq allkeys (cons kk allkeys)))
     hashtable)
    allkeys))
;;
(defun mon-hash-to-list (hashtable)
  "Return a list representing key/value pairs in HASHTABLE.
See also; <FUNCTION(S)>:
`mon-hash-all-values',`mon-hash-all-keys',`mon-hash-describe',
`mon-hash-describe-descend'.\n
See also; <MACRO(S)>:
`mon-hash-get-items',`mon-hash-get-values', `mon-hash-get-symbol-keys', 
`mon-hash-has-key', `mon-hash-get-string-keys', `cl-put-hash'.\n
See also; (elisp hash functions) - info node `(elisp)Hash Tables'
`make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
`define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table',    
`hash-table-count', `hash-table-test', `hash-table-weakness',	      
`hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'."
  (let (mylist)
    (maphash 
     (lambda (kk vv) 
       (setq mylist (cons (list kk vv) mylist)))
     hashtable)
    mylist))
;;
;; (defun mon-hash-key-onto-list (kk vv)
;;  "Prepend the key KK to the list 'allkeys'."
;; (setq allkeys (cons kk allkeys)))

;; (defvar allkeys '())
;; (maphash 'pullkeys myhash)
;;; ==============================
;; creating a hash
;; (defvar *myhash* (make-hash-table :test 'equal))
;; (puthash "Mary" "19" myhash)
;; (puthash "Jane" "20" myhash)
;; (puthash "Carrie" "17" myhash)
;; (puthash "Liz" "21" myhash)


;;; ==============================
;;; End Xah-lee Section
;;; ==============================

;;; ==============================

;;; ==============================
;;; COURTESY: Thierry Volpiatto HIS: macros-func-thierry.el
;;; Following: `hash-get-values', `hash-get-symbol-keys', `hash-has-key',
;;; `hash-get-string-keys', and `cl-put-hash'
;;; Hash-table procedures to emulate here methods of python dictionaries.
;;; ==============================

;;; ==============================
;; Get items of hash table -- return a list with (key value) as elements.
(defmacro mon-hash-get-items (hashtable)
  "Get the list of all keys/values of HASHTABLE.
Values are given under string form.\n
See also; <FUNCTION(S)>:
`mon-hash-all-values',`mon-hash-all-keys',`mon-hash-to-list'
`mon-hash-describe',`mon-hash-describe-descend'.\n
See also; <MACRO(S)>:
`mon-hash-get-values', `mon-hash-get-symbol-keys', `mon-hash-has-key',
`mon-hash-get-string-keys', `cl-put-hash'.\n
See also; (elisp hash functions) - info node `(elisp)Hash Tables'
`make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
`define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table',    
`hash-table-count', `hash-table-test', `hash-table-weakness',	      
`hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'."
  `(let ((li-items nil)) 
    (maphash #'(lambda (x y) (push (list x y) li-items))
             ,hashtable)
    li-items))

;;; ==============================
;; Get values of hash-table in string form (they are already in string form).
(defmacro mon-hash-get-values (hashtable)
  "Get the list of all values of HASHTABLE values are given under string form.
See also; <FUNCTION(S)>:
`mon-hash-all-values',`mon-hash-all-keys',`mon-hash-to-list'
`mon-hash-describe',`mon-hash-describe-descend'.\n
See also; <MACRO(S)>:
`mon-hash-get-items',`mon-hash-get-symbol-keys', `mon-hash-has-key',
`mon-hash-get-string-keys', `cl-put-hash'.\n
See also; (elisp hash functions) - info node `(elisp)Hash Tables'
`make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
`define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table',    
`hash-table-count', `hash-table-test', `hash-table-weakness',	      
`hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'."
  `(let ((li-values nil)
         (li-all (mon-hash-get-items ,hashtable)))
     (setq li-values (mapcar #'cadr li-all))
     li-values))

;;; ==============================
;; Get keys of hash-table in symbol form.
(defmacro mon-hash-get-symbol-keys (hashtable)
  "Get the list of all the keys in HASHTABLE.
Keys are returned in string form.
See also; <FUNCTION(S)>:
`mon-hash-all-values',`mon-hash-all-keys',`mon-hash-to-list'
`mon-hash-describe',`mon-hash-describe-descend'.\n
See also; <MACRO(S)>:
`mon-hash-get-items',`mon-hash-get-values', `mon-hash-has-key',
`mon-hash-get-string-keys', `cl-put-hash'.\n
See also; (elisp hash functions) - info node `(elisp)Hash Tables'
`make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
`define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table',    
`hash-table-count', `hash-table-test', `hash-table-weakness',	      
`hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'."
  `(let ((li-keys nil)
         (li-all (mon-hash-get-items ,hashtable)))
     (setq li-keys (mapcar #'car li-all))
     li-keys))

;;; ==============================
;; Return t if key exist nil otherwise (i use memq here: test ==> eq)
;; `member' is needed to work on cl.
(defmacro mon-hash-has-key (key hashtable)
  "Check if HASHTABLE has the key KEY.
Key here must be a symbol and not a string.
See also; <FUNCTION(S)>:
`mon-hash-all-values',`mon-hash-all-keys',`mon-hash-to-list'
`mon-hash-describe',`mon-hash-describe-descend'.\n
See also; <MACRO(S)>:
`mon-hash-get-items',`mon-hash-get-values', `mon-hash-get-symbol-keys', 
`mon-hash-get-string-keys', `cl-put-hash'.\n
See also; (elisp hash functions) - info node `(elisp)Hash Tables'
`make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
`define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table',    
`hash-table-count', `hash-table-test', `hash-table-weakness',	      
`hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'."
  `(let ((keys-list (mon-hash-get-symbol-keys ,hashtable)))
     (if (memq ,key keys-list)
         t
       nil)))

;;; ==============================
;; Get keys of hash-table in string form WAS: `mon-hash-get-string-keys'.
(defmacro mon-hash-get-string-keys (hashtable)
  "Get the list of all the keys in HASHTABLE.
Keys are given in string form.
See also; <FUNCTION(S)>:
`mon-hash-all-values',`mon-hash-all-keys',`mon-hash-to-list'
`mon-hash-describe',`mon-hash-describe-descend'.\n
See also; <MACRO(S)>:
`mon-hash-get-items',`mon-hash-get-values', `mon-hash-get-symbol-keys', 
`mon-hash-has-key', `mon-hash-get-string-keys', `cl-put-hash'.\n
See also; (elisp hash functions) - info node `(elisp)Hash Tables'
`make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
`define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table',    
`hash-table-count', `hash-table-test', `hash-table-weakness',	      
`hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'."
  `(let ((li-keys nil)
         (li-all (mon-hash-get-items ,hashtable))
         (li-keys-str nil))
     (setq li-keys (mapcar #'car li-all))
     (dolist (i li-keys)
       (push (symbol-name i) li-keys-str))
     li-keys-str))

;;;(defalias 'hash-get-string-keys 'mon-hash-get-string-keys)
;;;(makunbound 'hash-get-string-keys)
;;;(unintern 'hash-get-string-keys)

;;; ==============================
;;; Define an elisp puthash for CL.
(defmacro cl-put-hash (key table value)
"Defines an elisp compatible puthash for CL. 
Associate KEY with VALUE in hashtable TABLE. If key is already present
in TABLE, replace its current value with VALUE.\n
NOTE: The argument order of elisp `puthash' is: (puthash key value table)
The order of arguments in cl-put-hash is :(cl-puthash key table value)\n
NOTE: `cl-extra.el' already defines `cl-puthash' but it simply aliases the
elisp `puthash'.\n
See also; <FUNCTION(S)>:
`mon-hash-all-values',`mon-hash-all-keys',`mon-hash-to-list',
`mon-hash-describe-descend'.\n
See also; <MACRO(S)>:
`mon-hash-get-items',`mon-hash-get-values', `mon-hash-get-symbol-keys', 
`mon-hash-has-key', `mon-hash-get-string-keys', `cl-put-hash'.\n
See also; (elisp hash functions) - info node `(elisp)Hash Tables'
`make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
`define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table',    
`hash-table-count', `hash-table-test', `hash-table-weakness',	      
`hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'."
  `(setf (gethash ,key ,table) ,value))

;;; ==============================
;;; End Thierry Macros
;;; ==============================

;;; ==============================
;;; CREATED: <Timestamp: Wednesday May 13, 2009 @ 07:40.02 PM - by MON KEY>
;;; (URL `http://www.emacswiki.org/emacs/HashMap') WAS: `describe-hash'
;;; From the emacs-wiki:
;;; I use describe-hash all the time. 
;;; Its great unless except for when you have nested hash-tables. 
;;; For that I use a slightly modified version:
;;      (with-output-to-temp-buffer (help-buffer)
;;        (mon-describe-hash-descend (symbol-value variable)))
;;; ==============================
(defun mon-hash-describe (variable &optional buffer)
  "Display the full documentation of VARIABLE (a symbol).
Returns the documentation as a string, also.
If VARIABLE has a buffer-local value in BUFFER (default to the current buffer),
it is displayed along with the global value.
See also; <FUNCTION(S)>:
`mon-hash-all-values',`mon-hash-all-keys',`mon-hash-to-list',
`mon-hash-describe-descend'.\n
See also: <MACRO(S)>:
`mon-hash-get-items',`mon-hash-get-values', `mon-hash-get-symbol-keys', 
`mon-hash-has-key', `mon-hash-get-string-keys', `cl-put-hash'.\n
See also; (elisp hash functions) - info node `(elisp)Hash Tables'
`make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
`define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table',    
`hash-table-count', `hash-table-test', `hash-table-weakness',	      
`hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'."
  (interactive
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read
		(if (and (symbolp v)
			 (hash-table-p (symbol-value v)))
		    (format
		     "Describe hash-map (default %s): " v)
		  "Describe hash-map: ")
		obarray
		(lambda (atom) (and (boundp atom)
				    (hash-table-p (symbol-value atom))))
		t nil nil
		(if (hash-table-p v) (symbol-name v))))
     (list (if (equal val "")
	       v (intern val)))))
  (with-output-to-temp-buffer (help-buffer)
    (maphash (lambda (key value)
	       (pp key)
	       (princ " => ")
	       (pp value)
	       (terpri))
	     (symbol-value variable))))

;;; ==============================
;; WAS: `describe-hash-descend'
(defun mon-hash-describe-descend (hash)
  "Recursive describe hash func for nested hash-tables.
See also; <FUNCTION(S)>:
`mon-hash-all-values',`mon-hash-all-keys',`mon-hash-to-list'
`mon-hash-describe'.\n
See also; <MACRO(S)>:
`mon-hash-get-items',`mon-hash-get-values', `mon-hash-get-symbol-keys', 
`mon-hash-has-key', `mon-hash-get-string-keys', `cl-put-hash'.\n
See also; (elisp hash functions) - info node `(elisp)Hash Tables'
`make-hash-table', `gethash', `puthash', `remhash', `clrhash', `maphash', 
`define-hash-table-test', `sxhash', `hash-table-p', `copy-hash-table',    
`hash-table-count', `hash-table-test', `hash-table-weakness',	      
`hash-table-rehash-size', `hash-table-rehash-threshold', `hash-table-size'."
  (maphash (lambda (key value)
	     (pp key)
	     (princ " => ")
	     (if (hash-table-p value)
		 (progn
		   (princ " { ")
		   (terpri)
		   (mon-hash-describe-descend value)
		   (princ " } "))
	       (pp value))
	     (terpri))
	   hash))

;;; ==============================
(provide 'mon-hash-utils)
;;; ==============================

;;; ================================================================
;;; mon-hash-utils.el ends here
;;; EOF
