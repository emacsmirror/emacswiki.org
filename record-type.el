;;; record-type.el -- Define record type for Emacs Lisp
;;
;; Copyright (C) 2007  Cedric Lallain
;;
;; Author:   Cedric Lallain <kandjar76@hotmail.com>
;; Version:  0.9
;; Keywords: oop
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; Definition of a new Emacs type: record.
;; A record is a set of named fields with their associated type.
;;
;; The type definition is made through a predicate, the internal functions
;; will do type checking to make sure the type is correct.
;;
;; In order to define a new record the syntax is:
;;   (defrecord type-name "Comment string"
;;      :field-name1 'predicate-1
;;      :field-name2 'predicate-2
;;      ....)
;;
;; Once the type created, a generic set of functions are available in
;; order to work with it:
;; . recordp                 returns t if type-name is a record type
;; . get-record-comment      returns the comment associated with the
;;			     type
;; . make-new-record         creates and returns a new instance
;;			     (optional: initial values)
;; . instancep               returns t if the argument is an instance
;;			     of a record type
;; . get-record-type         returns the record-type associated with
;;			     an instance
;; . has-record-field        checks if a specific field name exists in
;;			     the record-type definition
;; . set-record-field-value  set a value to a specific field of an
;;			     instance
;; . set-record-field-values set values to different fields of an
;;			     instance
;; . get-record-field-value  returns the value of a specific field of
;;			     an instance
;;
;; The record type definition will auto-generate the definition of the
;; following functions:
;;   a constructor function:
;;      make-new-<record-name>
;; . a predicate function:
;;      <record-name>-p
;; . a set function and a get function for each field name:
;;      set-<record-name>-<field-name>
;;      get-<record-name>-<field-name>
;;
;; *Note*: the set functions will change the value of the instance
;;  without having to do a "setq".  If you just want to a field value
;;  without changing the variable, use: set-record-field-value.
;;
;;; Example:
;;
;;; Here is sample code to demonstrate the use of the general functions:
;;
;;   (defrecord test
;;     "Comment for the record test"
;;     :string 'stringp
;;     :symbol 'symbolp
;;     :int 'number-or-marker-p)
;;
;;   (recordp test)
;;   (get-record-comment test)
;;   (get-record-type (make-new-record test))
;;   (has-record-field test :string)
;;
;;   (setq itest (make-new-record test))
;;   (setq itest (set-record-field-value itest :symbol 'sym))
;;   (get-record-field-value
;;    (set-record-field-values itest :int 3 :string "test") :int)
;;
;;; Here is another sample, this time to demonstrate,
;;  how to use the newly-defined function:
;;
;;   (defrecord test2 "This is a comment for test2"
;;     :string 'stringp
;;     :symbol 'symbolp
;;     :int 'number-or-marker-p)
;;
;;   (setq itest2 (make-new-test2))
;;   (set-test2-string itest2 "pouet")
;;   (get-test2-int itest2)
;;
;;   (setq itest3 (make-new-test2 :string "pouet" :int 50))
;;
;;   (let ((field-name "test"))
;;     (get-test2-string itest2)
;;     (message field-name))
;;   (let ((field-name "test"))
;;     (set-test2-string itest2 "ok")
;;     (message field-name))
;;
;;
;;; TODO list:
;;
;;  Make the comment optional
;;  Support default values
;;  ... suggestion? ;-)
;;
;;; History:
;;
;; Tested with: GNU Emacs 21 and 22
;;
;;; Code:

;;; Private Section

(defmacro defrecord-define-set-code (record-type field)
  "Define the private set function for RECORD-TYPE for the FIELD value."
  (let ((field-name (make-symbol "field-name-private"))
	(set-function-name (make-symbol "set-function-name-private"))
	(set-function-comment (make-symbol "set-function-comment-private")))
    (let* ((field-name (if (string= (substring (symbol-name field) 0 1) ":")
			   (substring (symbol-name field) 1)
			   (symbol-name field)))
	   (set-function-name (eval `(intern ,(concat "set-" (symbol-name record-type) "-" field-name "-private"))))
	   (set-function-comment (format "PRIVATE - Set the %s value to VALUE of a specific instance INSTANCE whose type is %s"
					 field-name (symbol-name record-type))))
      `(defun ,set-function-name (instance value)
	 ,set-function-comment
	 (set-record-field-value instance ,field value)))))

(defmacro defrecord-define-set-macro-code (record-type field)
  "Define the set macro of RECORD-TYPE to directly write to FIELD."
  (let ((field-name (make-symbol "field-name"))
	(set-macro-name (make-symbol "set-macro-name"))
	(set-function-name (make-symbol "set-function-name"))
	(set-macro-comment (make-symbol "set-macro-comment")))
    (let* ((field-name (if (string= (substring (symbol-name field) 0 1) ":")
			   (substring (symbol-name field) 1)
			   (symbol-name field)))
	   (set-macro-name (eval `(intern ,(concat "set-" (symbol-name record-type) "-" field-name))))
	   (set-function-name (eval `(intern (concat "set-" (symbol-name record-type) "-" field-name "-private"))))
	   (set-macro-comment (format "Set the %s value to VALUE of a specific instance INSTANCE whose type is %s"
				      field-name (symbol-name record-type))))
      `(defmacro ,set-macro-name (instance value) ,set-macro-comment (list 'setq instance (list ',set-function-name instance value))))))

(defmacro defrecord-define-get-code (record-type field)
  "Define the get function of RECORD-TYPE to return the value for FIELD."
  (let ((field-name (make-symbol "field-name"))
	(record-name (make-symbol "record-name"))
	(get-function-name (make-symbol "get-function-name"))
	(get-function-comment (make-symbol "get-function-comment")))
    (let* ((field-name (if (string= (substring (symbol-name field) 0 1) ":")
			   (substring (symbol-name field) 1)
			   (symbol-name field)))
	   (record-name (symbol-name record-type))
	   (get-function-name (eval `(intern ,(concat "get-" record-name "-" field-name))))
	   (get-function-comment (format "Return the %s value of a specific instance INSTANCE whose type is %s"
					 field-name (symbol-name record-type))))
      `(defun ,get-function-name (instance) ,get-function-comment (get-record-field-value instance ,field)))))


(defmacro defrecord-define-field-functions(record-type field)
  "Define the get and set functions for FIELD of RECORD-TYPE."
  `(progn (defrecord-define-set-code ,record-type ,(eval field))
	  (defrecord-define-set-macro-code ,record-type ,(eval field))
	  (defrecord-define-get-code ,record-type ,(eval field))))


(defun defrecord-private(comment &rest args)
  "Define RECORD-TYPE with COMMENT and ARGS record definition.
A record definition consists of a list of symbol and type-checker pairs."
  (if (stringp comment)
      (let ((new-type (list :define-record comment))
	    (field-defs args))
	(while field-defs
	  (let ((field-name (pop field-defs))
		(type-checker (pop field-defs)))
	    (if (symbolp field-name)
		(if (symbol-function type-checker)
		    (setq new-type (append new-type (list (cons field-name type-checker)))))
		(error "Wrong type of argument, expecting a symbol type"))))
	new-type)
      (error "Invalid name type: the comment must be a string!")))


(defmacro defrecord-define-make-new (record-type)
  "Define the constructor function for RECORD-TYPE."
  (let ((record-name (make-symbol "record-name"))
	(make-new-function-name    (make-symbol "make-new-function-name"))
	(make-new-function-comment (make-symbol "make-new-function-comment")))
    (let* ((record-name (symbol-name record-type))
	   (make-new-function-name (eval `(intern ,(concat "make-new-" record-name))))
	   (make-new-function-comment (concat "Return a new instance of the record-type " (symbol-name record-type))))
      `(defun ,make-new-function-name (&rest args) ,make-new-function-comment (apply 'make-new-record ,record-type args)))))


(defmacro defrecord-define-predicate (record-type)
  "Define the predicate function for the record RECORD-TYPE."
  (let ((record-name (make-symbol "record-name"))
	(predicate-function-name    (make-symbol "predicate-function-name"))
	(predicate-function-comment (make-symbol "predicate-function-comment")))
    (let* ((record-name (symbol-name record-type))
	   (predicate-function-name (eval `(intern ,(concat record-name "-p"))))
	   (predicate-function-comment (concat "Return t if INSTANCE's type is " (symbol-name record-type))))
      `(defun ,predicate-function-name (instance) ,predicate-function-comment
	 (and (instancep instance)
	      (eq ,record-type (car instance)))))))
 
;;; Public Section

(defmacro defrecord(record-type comment &rest args)
  "Define RECORD-TYPE with COMMENT and .
A record is constituated with a list of coupe symbol / type-checker"
  (let ((fields (make-symbol "fields"))
	(current-field-name (make-symbol "current-field-name"))
	(current-field-comment (make-symbol "current-field-comment")))
    (let ((current-field-comment comment))
      `(progn (defconst ,record-type (defrecord-private ,current-field-comment ,@args) ,current-field-comment)
	      (defrecord-define-predicate ,record-type)
	      (defrecord-define-make-new ,record-type)
	      (let ((fields (cddr ,record-type)))
		(while fields
		  (let ((current-field-name (caar fields)))
		    (defrecord-define-field-functions ,record-type current-field-name)
		    (pop fields))))))))

(defun recordp(record)
  "Return t if RECORD is of type record."
  (if (and (listp record)
	   (and (symbolp (car record))
		(eq (car record) :define-record))
	   (stringp (cadr record)))
      (let ((result t)
	    (to-check (cddr record)))
	(while (and result to-check)
	  (setq result (and result
			    (symbolp (caar to-check))
			    (symbolp (cdar to-check))
			    (fboundp (cdar to-check))))
	  (pop to-check))
	result)))

(defun get-record-comment(record)
  "Return the comment for RECORD."
  (and (recordp record)
       (cadr record)))

(defun make-new-record(record-type &rest args)
  "Construct a new instance of RECORD-TYPE with definition ARGS."
  (if (recordp record-type)
      (let* ((instance (append (list record-type) (mapcar (lambda(head) nil) (cddr record-type)))))
        (apply 'set-record-field-values instance args))))

(defun instancep(instance)
  "Return t if INSTANCE is an instance of a record type."
  (and (listp instance)
       (recordp (car instance))))

(defun get-record-type(instance)
  "Return the record type of INSTANCE."
  (and (instancep instance)
       (car instance)))

(defun has-record-field(record-type field-name)
  "Return t if RECORD-TYPE has the field with FIELD-NAME."
  (if (and (recordp record-type)
	   (symbolp field-name))
      (let ((fields (cddr record-type))
	    (found nil))
	(while (and (not found)
		    fields)
	  (setq found (eq (car (pop fields))
			      field-name)))
	found)))

(defun set-record-field-value(instance field-name field-value)
  "Set the INSTANCE FIELD-NAME to FIELD-VALUE."
  (if (and (instancep instance)
	   (symbolp field-name))
      (let ((new-instance (list (car instance)))
	    (fields (cddar instance))
	    (values (cdr instance))
	    (not-found t))
	(while (and not-found
		    fields)
	  (if (and (eq (caar fields) field-name)
		   (funcall (cdar fields) field-value))
	      (progn (setq not-found nil)
		     (setq new-instance (append new-instance (list field-value) (cdr values))))
	      (progn (setq new-instance (append new-instance (list (pop values))))
		     (pop fields))))
	(if (not not-found)
	    new-instance
	    (error (format "This record doesn't contain the field %s or the associated value's type is invalid"
			   ;(get-record-name (car instance))
			   (symbol-name field-name)))))
      (if (instancep instance)
	  (error "Invalid data: you need to pass an instance of a struct to this function!")
	  (error "Invalid data: missing field"))))

(defun set-record-field-values(instance &rest args)
  "Set the value of several fields in INSTANCE using ARGS."
  (let ((new-instance instance))
    (while args
      (setq new-instance (set-record-field-value new-instance (pop args) (pop args))))
    new-instance))

(defun get-record-field-value(instance field-name)
  "Return the value of INSTANCE's field FIELD-NAME."
  (if (and (instancep instance)
	   (symbolp field-name))
      (let ((fields (cddar instance))
	    (values (cdr instance))
	    (not-found t)
	    (field-value nil))
	(progn (while (and not-found
			   fields)
		 (progn (if (eq (caar fields) field-name)
			    (progn (setq not-found nil)
				   (setq field-value (car values))))
			(pop fields)
			(pop values)))
	       field-value))))

(provide 'record-type)
;;; record-type.el ends here
