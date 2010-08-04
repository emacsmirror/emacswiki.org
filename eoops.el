;;; eoops.el --- An Object Oriented Programming System in Elisp

;; Copyright (C) 1992  Twin Sun, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'eoops)
(load-library "bytecomp") ;; bytecomp has no provide

(defvar eoops:class-path
  "/tmp/elisp/classes" "Where the eoops class source is stored.")

(defun map:new ()
  "Returns a new map.  We implement a map as a pair,
with head the symbol `map and tail a Lisp association list.  Here,
an association list is a possibly empty list of lists, each with first
element <argument> and second element <image>.  Each argument and
image is an arbitrary Lisp object."
  (list 'map))

(defun map:pairs (map)
  "Returns the association list of map."
  (cdr map))

(defun map:2nd (list)
  "is just like Common-Lisp's cadr."
  (car (cdr list)))

(defun map:get (map argument)
  "Returns the image of MAP at ARGUMENT or nil if argument is not in
the map's domain."
  (map:2nd (assoc argument (map:pairs map))))

(defun map:set (map argument image)
  "Updates the value of MAP at ARGUMENT to be IMAGE and returns the image."
  (let ((pair (assoc argument (map:pairs map))))
    (if pair
        (setcar (cdr pair) image)
      (setcdr map (cons (list argument image) 
                        (map:pairs map)))))
  image)

(defun map:range (map)
  "Returns the range of *map,* i.e., a list of the map's images."
  (mapcar 'map:2nd (map:pairs map)))

(defvar eoops:classes (map:new) "The eoops class database.")

(defun eoops:symbol-concat (&rest s)
  "Returns a symbol whose print-string is the concatenation of the
list of SYMBOLS/STRINGS."
  (let ((string '(lambda (x) (format "%s" x))))
    (intern (apply 'concat (mapcar string s)))))

(defmacro ce:for (symbol list &rest body)
  "Binds VARIABLE to successive car's of LIST and evaluates BODY.
returning the list of results.  This is just a convenient abbreviation
of mapcar.  The 'ce:' prefix denotes our library of Common
Emacs-lisp functions and macros."
  (` (mapcar '(lambda ((, symbol)) (,@ body)) 
            (, list))))

(defmacro ce:let (bindings &rest body)
  "This is a destructuring let.  (let BINDINGS . BODY) binds
BINDINGS, then evaluates forms in BODY, returning the value of the
last form.  BINDINGS is a list; each element is either a symbol
bound to nil or a list (pattern value) binding the symbols in
pattern to corresponding value's.  Each value can refer to
symbols already bound in bindings. For example:

   (ce:let (((first second . tail) '(1 2 3 4)))
      (list first second tail))
   =>  (1 2 (3 4))."

  (` (let* (, (ce:bindings bindings)) (,@ body))))

(defun ce:bindings (bindings)
  "Given PATTERN, returns a list of bindings.  PATTERN
is a tree of symbols.  [ce:bindings] returns a list ((symbol
access)*) where SYMBOL occurs in the PATTERN and ACCESS is
an accessing expression of nested calls to car and cdr."
  (let ((s 'ce:let)
        (b '(lambda (pattern path)
              (cond
               ((null pattern)
                nil)
               ((symbolp pattern)
                (list (list pattern path)))
               ((consp pattern)
                (append 
                  (funcall b (car pattern) 
                             (list 'car path))
                  (funcall b (cdr pattern) 
                             (list 'cdr path))))))))
    (apply 
      'append 
      (ce:for bind bindings
        (if (and (consp bind) (consp (car bind)))
            (cons (list s (nth 1 bind)) 
                  (funcall b (car bind) s))
          (list bind))))))

(defmacro class (nc np slots &rest methods)
  "This macro creates a new class record, fills in its fields, and
then compiles the class.  The class record remembers the class's name,
slots, methods, and other information."
  (let ((c (or (map:get eoops:classes nc) 
               (map:new))))
    (let* ((np (map:get c 'parent))
           (p  (map:get eoops:classes np))
           (s  (map:get p 'children)))
      (if p (map:set p 'children (delq nc s))))
    (eoops:require-class np)
    (let* ((p (map:get eoops:classes np))
           (s (map:get p 'children)))
      (if p (map:set p 'children (cons nc s))))
    (map:set c 'parent np)
    (map:set c 'slots slots)
    (map:set c 'name nc)
    (map:set c 'compiled nil)
    (map:set c 'methods
	     (if (stringp (car methods)) 
		 (cdr methods)
	       methods))
    (map:set eoops:classes nc c)
    (eoops:compile-class c)
    (map:set c 'modtime 
             (eoops:class-mod-time nc 'obj)))
  (list 'quote nc))

(defmacro new (nc)
  "Calls the function [eoops:new] after quoting the CLASS argument."
  (` (eoops:new (quote (, nc)))))

(defmacro @ (&rest arguments)
  "Send eoops message to self."
  (` ($ self (,@ arguments))))

(defmacro $ (receiver selector &rest args)
  "Implements fast message passing.  The $ macro implements a
partial method-lookup at compile-time, by precomputing the hash code
for the SELECTOR.  Then $ constructs code to dispatch on the class
of the *receiver* at run-time.  There are three cases which are
described in the Eoops paper."
  (let ((h (eoops:hash selector))
        (s (list 'quote selector)))
    (cond 
     ((eq receiver 'super)
      (` (let ((super 
                (map:get (map:get 
                           eoops:classes 
                           '(, eoops:super-class))
                         'compiled)))
           (funcall (cdr (assq '(, selector) 
                               (aref super (, h))))
                    self
                    (,@ args)))))
     ((symbolp receiver)
      (` (funcall 
          (cdr (assq (, s) (aref 
                             (aref (, receiver) 0) 
                             (, h))))       
          (, receiver)
          (,@ args))))
     (t (` (let ((eoops:receiver (, receiver)))
             ($ eoops:receiver 
                (, selector) 
                (,@ args))))))))

(defun eoops:compile-class (c)
  "Compile class C.  Writes compiled version to disk."
  (let* ((np (map:get c 'parent))
         (p  (map:get eoops:classes np))
         (pcv (map:get p 'compiled))
         (nc (map:get c 'name))
         (cv (make-vector eoops:vtable-size nil)))
    (message "eoops compiling %s" nc)
    (aset cv 0 nc)
    (let ((methods (map:get c 'methods))
          (eoops:super-class np))
      (ce:for method methods
        (ce:let ((((selector . parms) . body) method)
                 (doc 
                  (format "(%s %s) " nc selector))
                 (body 
                  (if (stringp (car body))
                      (cons (concat doc (car body)) 
                            (cdr body))
                    (cons doc body)))
                 (code
                  (` (lambda (, parms) (,@ body)))))
;          (message "Compiling %s %s..." nc selector)
          (eoops:store-method cv selector code))))
    (map:set c 'compiled cv)
    (eoops:write-class c)
    (eoops:compile-slot-accessors nc c cv pcv)
    (map:set c 'compiled cv)
    (let ((pv (map:get p 'compiled)))
      (map:set c 'compiled
               (eoops:inherit-behavior pv cv)))
    (mapcar '(lambda (ns) (eoops:compile-class
                           (map:get eoops:classes ns)))
            (map:get c 'children))))

(defun eoops:compile-slot-accessors (nc c cv pcv)
  "Compiles the slot accessors for class named NC, defined as C,
stores result in class vector CV, inherits from PCV."
  (let ((i (if pcv (aref pcv 1) 1))
	(slots (map:get c 'slots)))
    (aset cv 1 (+ i (length slots)))
    (ce:for slot slots
      (let* ((get (if (consp slot) (car slot) slot))
	     (set (eoops:symbol-concat get ":"))
	     (doc (if (consp slot) (nth 1 slot) ""))
	     (gdoc (format "(%s %s) %s" nc get doc))
	     (sdoc (format "(%s %s) %s" nc set doc)))
	(eoops:store-method
	 cv get (` (lambda () (, gdoc) (aref self (, i)))))
	(eoops:store-method
	 cv set
	 (` (lambda (v) (, sdoc) (aset self (, i) v))))
	(setq i (1+ i))))))

(defun eoops:new (nc)
  "Checks, at run-time, that the specified class, NC, is loaded.  If
it is not currently loaded, [eoops:require-class] is invoked to load
it.  Normally, a class is repeatedly instantiated but only the first
invocation of eoops:new may require the expense of loading the class.
After the class record, *c*, is retrieved, the *cv* is retrieved, the
instance vector is created and initialized, an init message is sent to
the new instance, and finally the new initialized instance is
returned."
  (let* ((c (or (map:get eoops:classes nc)
                (progn (eoops:require-class nc)
                       (map:get eoops:classes nc))))
         (cv (map:get c 'compiled))
         (self (make-vector (aref cv 1) nil)))
    (aset self 0 cv)
    ($ self init)
    self))

;; The first two elements of a class vector are the class's name and
;; instance vector size.  The rest of the class vector elements
;; implement a hash table for the methods for the selectors to which
;; the class responds.  The size of the hash table is stored in the
;; constant htable-size.  Therefore, the length of a class vector,
;; vtable-size, is 2 + htable-size. 

(defconst eoops:htable-size 23 "Size of hash table.")

(defconst eoops:vtable-size (+ 2 eoops:htable-size) "Size of class vector.")

(defun eoops:hash (symbol)
  "Returns a relatively unique integer for SYMBOL, between 2 and htable-size."
  (let* ((s (symbol-name symbol))
         (i (length s))
         (r 0))
    (while (< 0 i)
      (setq i (1- i))
      (setq r (+ r r (aref s i))))
    (+ 2 (% (max r (- r)) eoops:htable-size))))

(defun eoops:store-method (cv selector method)
  "Stores in CV's hash bucket at SELECTOR a byte-compiled METHOD."
  (let* ((f (byte-compile-lambda
              (eoops:add-self method)))
         (h (eoops:hash selector))
         (bucket (aref cv h)))
    (aset cv h 
      (eoops:update-cv-bucket bucket selector f))))

(defun eoops:add-self (f)
  "Given a lambda expression F, prepends the symbol 'self to the
arguments in f.  This is required since $ calls a method with the
receiver as an additional argument prepended to those specified in a
method definition."
  (ce:let (((lambda arguments . body) f))
    (` (lambda (self (,@ arguments)) (,@ body)))))

(defun eoops:inherit-behavior (pv cv)
  "Copies the methods from the parent-vector to the class-vector.
Called when the compiled code is loaded, this implements load-time
inheritance."
  (let ((ncv (make-vector eoops:vtable-size nil))
        (i eoops:vtable-size))
    (aset ncv 0 (aref cv 0))
    (aset ncv 1 (aref cv 1))
    (while (< 2 i)
      (setq i (1- i))
      (if pv (aset ncv i (copy-alist (aref pv i))))
      (ce:for entry (aref cv i)
        (aset ncv i (eoops:update-cv-bucket 
                      (aref ncv i)
                      (car entry)
                      (cdr entry)))))
    ncv))

(defun eoops:update-cv-bucket (bucket selector f)
  "Updates the hash table BUCKET's alist at index SELECTOR, to contain
the lambda expression F."
  (let ((pair (assq selector bucket)))
    (cond (pair (rplacd pair f) bucket)
          (t (cons (cons selector f) bucket)))))

(defun eoops:require-class (nc)
  "Loads the specified class, NC, when either the class has never been
loaded or when its sources, if available, are newer than the object
file."
  (cond
   ((not nc) t)
   ((and (map:get eoops:classes nc)
	 (eq (eoops:file-type-to-load nc) 'obj))
    t)
   (t (eoops:load-file nc))))

(defun eoops:load-file (nc)
  "Loads the class, NC, from disk."
  (let ((file-type (eoops:file-type-to-load nc)))
    (if (eq file-type 'src) (eoops:delete-locks nc))
    (message "eoops loading %s" nc)
    (load (eoops:class-file-name nc file-type) nil 'nomessage 'nosuffix)))

(defun eoops:load-class (nc c)
  "Stores the class record C in eoops:classes under the name NC.  This
function is explicitly invoked in a class's object file.  Loading a
class enforces that the parent is loaded.  The parent's class vector
is then copied and merged with the current class's class vector by
eoops:inherit-behavior.  This inheritance step is done at load time so
that a new object file need not be created and written for a class
when an ancestor is modified."
  (if (eq 'src (eoops:file-type-to-load nc))
      (eoops:load-file nc)
    (let ((np (map:get c 'parent)))
      (eoops:require-class np)
      (let* ((p  (map:get eoops:classes np))
	     (pv (map:get p 'compiled))
	     (cv (map:get c 'compiled))
	     (s (map:get p 'children)))
	(map:set eoops:classes nc c)
	(map:set c 'modtime 
		 (eoops:class-mod-time nc 'obj))
	(eoops:compile-slot-accessors nc c cv pv)
	(map:set c 'compiled 
		 (eoops:inherit-behavior pv cv))
	(if p (map:set p 'children (cons nc s)))))))

(defun eoops:write-class (c)
  "Writes the compiled class C, whose class name is nc, to the file
nc.elc in the directory that is the value of eoops:class-path.  The
resulting file, when loaded into Emacs, will install class c.  Each
file contains only one class and each class has to be in exactly one
file.  However, a file may include other expressions.  Therefore,
eoops:write-class first byte-compiles the source file and then
replaces the item corresponding to the class definition by the printed
representation of the class.  Setting print-depth to nil makes sure
that the prin1 prints the complete class.  Since a class's children
field is a list of its currently loaded subclasses, this field is set
to nil before printing."
  (let* ((standard-output 
           (get-buffer-create "*Compiled*"))
         (print-depth nil)
         (nc (map:get c 'name))
         (src-name (eoops:class-file-name nc 'src))
         (obj-name (eoops:class-file-name nc 'obj))
         (prefix 
           (format "(eoops:load-class '%s '" nc))
         (suffix ")"))
    (cond
     ((not (file-exists-p obj-name))
;      (message "Writing class %s..." nc)
      (byte-compile-file src-name)
      (set-buffer standard-output)
      (erase-buffer)
      (insert-file obj-name)
      (goto-char (point-max))
      (re-search-backward "^(class ")
      (delete-region (point)
		     (progn (forward-sexp) (point)))
      (insert prefix)
      (let ((s (map:get c 'children)))
	(map:set c 'children nil)
	(prin1 c)
	(map:set c 'children s)
	(insert suffix))
      (let ((make-backup-files nil))
	(write-file obj-name))
;      (message "Writing class %s...done" nc)
      ))
    (kill-buffer standard-output)))

(defun eoops:class-file-name (nc type)
  "Returns the full path name of a file storing the class NC.  TYPE
can either be 'src or 'obj."
  (format (cond ((eq type 'src) "%s/%s.el")
                ((eq type 'obj) "%s/%s.elc"))
          eoops:class-path nc))

(defun eoops:class-mod-time (nc type)
  "Returns the last modification time of the source or object file
corresponding to the class NC."
  (nth 5 (file-attributes 
           (eoops:class-file-name nc type))))

(defun eoops:time-newer (ta tb)
  "Compares two time values returned by eoops:class-mod-time and
returns true if its first argument is greater than its second."
  (ce:let (((ta1 ta2) ta)
           ((tb1 tb2) tb))
    (or (> ta1 tb1)
        (and (= ta1 tb1)
             (> ta2 tb2)))))

(defun eoops:file-type-to-load (nc)
  "Returns which file type should be loaded."
  (let ((s-time (eoops:class-mod-time nc 'src))
	(o-time (eoops:class-mod-time nc 'obj)))
    (cond
     ((not nc) nil)
     ((and o-time s-time 
	   (eoops:time-newer o-time s-time))
      'obj)
     ((and o-time (not s-time)) 'obj)
     (t 'src))))

(defun eoops:delete-locks (nc)
  "'Locks' are used to attempt to avoid disk contention when more than one
person tries to compile the same class simultaneously."
  (let ((fname (eoops:class-file-name nc 'obj)))
    (condition-case err
	(delete-file fname)
      (error nil))))
