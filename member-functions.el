;;; member-functions.el --- Expand C++ member functions into implementation file

;; Copyright (c) 1998 1999 2000  Ron Lawrence

;; Author: Ron Lawrence <ral@bit-net.com>
;; Version: 0.3.1a

;; member-functions.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.

;; member-functions.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You can obtain a copy of the GNU General Public License from
;; http://www.gnu.org, or write to the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; The following code is known to work with at least one recent
;; version of xemacs, and at least one recent version of emacs.  If
;; you find that you are able to make it work on another emacs
;; variant or version, where it doesn't already work, please let me
;; know.  I am especially interested in any modifications that might
;; be required.

;;; Commentary:

;; `member-function.el' defines an emacs utility to expand the
;; member functions declared in a class in a C++ header file into
;; the corresponding implementation file, provided that they aren't
;; already there.  The main entry point is the interactive command
;; `expand-member-functions'.

;; The code consists of four major parts:

;; 1. A simple lexical analyser for emacs buffers containing C++
;; sources. The entry point, `mf--tokenize', returns a list of
;; token-type symbols paired with the string matched for that token.
;; Note that the whole buffer is lexed, even though, for the
;; application implemented here, some (possibly large) savings could
;; be realized by using forward-sexp to skip over the bodies of
;; member function definitions.  (This is a possible optimization.)
;; (This optimization has been implemented)

;; 2. Code to analyse the results of a parse.  Used primarily to
;; identify member function declarations that appear in class
;; declarations, as well as member function definitions that appear
;; in the corresponding .C file.  The resulting lists are compared
;; to find undefined member function declarations.

;; 3. Code to format, and insert, the found, undefined, declarations
;; as member function definitions into C++ source code.  An initial
;; block of commentary is also formatted for insertion.

;; 4. Code to interface with emacs, including code to automatically
;; check out a .C file using `vc-toggle-read-only'.

;; Usage:

;; Byte compile this file.

;; Add the following two lines to .emacs: (customize to suit.)

;; (autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)
;; (add-hook 'c++-mode-hook (lambda () (local-set-key "\C-cm" #'expand-member-functions)))

;; BUGS and Limitations:

;; Ctor initializers are transferred into the .C file.  The
;; individual initializer expressions are treated as though they
;; were the names of member functions, i.e. they are prepended
;; with class name/scope resolution operator pairs.

;; There are still some issues with templates, but they are, AFAIK,
;; limited to advanced uses.  For example, default template
;; arguments aren't handled properly.  These may occasionally be
;; problematic, but until I see them appear in actual code, I'm
;; going to defer working on them.

;; Templates are not formatted as nicely as they could be.

;; There is no provision for overloaded operator(), or operator[].

;; Several warnings are emitted during byte code compilation.  Most
;; are a consequence of the use of lexical-let from cl.el.  Those
;; that are not are entirely my fault.

;; Namespaces and nested classes are not supported.

;; Inline functions defined outside of a class declaration in the
;; header file, or included into it, are not supported.

;; (fixed) When a member function doesn't specify a name for a parameter,
;; fails to recognise that the member function definition with the
;; parameter name specified has the same signature.

;; (fixed) Doesn't properly handle a throw declaration attached to a
;; member function declaration.

;; History:

;; Thu Sep 28 10:06:09 2000 Released under GPL.

;; Fri Sep 29 09:23:10 2000 Tested on emacs 20.4.1.  Added a hack
;; after discoveing that expand-member-functions didn't work there.

;; Tue Jan 23 12:55:12 2001 Fixed typo on line 839.  Thanks to Barney Dalton
;; Barnaby.Dalton@radioscape.com 

;; Wed Jan 24 13:40:18 2001 Added variables for header file and
;; source file extensions at the suggestion of Barney Dalton.

;;; Code:

(require 'cl)

;;;; Customization 

(defvar mf--insert-commentary nil
  "*non-nil means decorate member functions with commentary on insertion")

(defvar mf--user-name-function #'user-login-name
  "*The function to use to get the user's name for insertion in member
function commentary.  Should be one of #'user-full-name or
#'user-login-name")

(defvar mf--checkout-if-readonly nil
  "*Check the implementation file out if it is read only.
Otherwise, an error will occur when an attempt is made to expand
member functions into a read only file.")

(defvar  mf--header-file-extension "h"
  "*The extension to use for c++ header files.  Don't include the dot.")
(defvar  mf--source-file-extension "C"
  "*The extension to use for c++ source files.  Don't include the dot.")

;;;; Constants

(defconst mf--version "0.3.1"
  "First GPL version.")

(defconst c++-keywords 
  (sort 
   (list "and" "bool" "compl" "do" "export" "goto" "namespace" "or_eq" "return"
         "struct" "try" "using" "xor" "and_eq" "break" "const" "double" "extern"
         "if" "new" "private" "short" "switch" "typedef" "virtual" "xor_eq" "asm"
         "case" "const_cast" "dynamic_cast" "false" "inline" "not" "protected" 
         "signed" "template" "typeid" "void" "auto" "catch" "continue" "else" 
         "float" "int" "not_eq" "public" "sizeof" "this" "typename" "volatile"
         "bitand" "char" "default" "enum" "for" "long" "operator" "register"
         "static" "throw" "union" "wchar_t" "bitor" "class" "delete" "explicit"
         "friend" "mutable" "or" "reinterpret_cast" "static_cast" "true" 
         "unsigned" "while" ) #'(lambda (a b) (> (length a) (length b)))))

(defconst c++-operators                 ; doesn't include "()" or "[]"
  (sort (list "\\+" "-" "\\*" "/" "%" "\\^" "&" "|" "~" "!" "=" "<<" ">>" 
              "\\+=" "-=" "\\*=" "/=" "%=" "^=" "&=" "|=" "<" ">" ">>="
              "<<=" "==" "!=" "<=" ">=" "&&" "||" "\\+\\+" "--" "," "->\\*"
              "->" "\\.") #'(lambda (a b) (> (length a) (length b)))))

(defun make-c++-keywords-regexp (keywords-list)
  (let ((result (concat "\\b" (car keywords-list) "\\b" )))
    (dolist (kw (cdr keywords-list) result)
      (setq result (concat result "\\|\\b" kw "\\b" )))))

(defun make-c++-operators-regexp (operators-list)
  (let ((result (car operators-list)))
    (dolist (op (cdr operators-list) result)
      (setq result (concat result "\\|" op)))))

(defconst c++-token-regexp-alist
  `((keyword . ,(make-c++-keywords-regexp c++-keywords))
    (operator . ,(make-c++-operators-regexp c++-operators))
    (c++-comment-start . "//")
    (c-comment-start . "/\\*")
    (c-comment-end . "*/")
    (whitespace . "[ \t\n]+")           ; not really a token.
    (identifier . "[A-Za-z_][A-Za-z_0-9]*")
    (string-literal . "\"[^\"]*\"")                         
    (float-literal . "[+-]?[0-9]*\\.[0-9]+\\([eE][0-9]+\\)?[dDFf]?")
    (integer-literal . "[+-]?[0-9]+[Ll]?")
    (scope-resolution . "::")
    (open-paren . "(")
    (close-paren . ")")
    (open-brace . "{")
    (close-brace . "}")
    (open-bracket . "\\[")
    (close-bracket . "\\]") 
    (statement-separator . ";")
    (include-directive . "^[ \t]*#[ \t]*include.*$")
    (macro-define . "^[ \t]*#[ \t]*define [A-Za-z_][A-Za-z_0-9]*[(].*$") 
    (preproc-define . "^[ \t]*#[ \t]*define [A-Za-z_][A-Za-z_0-9]*[^(].*$")
    (preproc-directive . "^[ \t]*#.*$")
    (punctuation . "[:\\?]") ))

;; Private functions

;; an example of testing to see if we are looking at a particular
;; token type...
(defun mf--looking-at-keyword-p ()
  (looking-at (cdr (assoc 'keyword c++-token-regexp-alist))))

(defun mf--looking-at-token-p (token-type)
  (looking-at (cdr (assoc token-type c++-token-regexp-alist))))

(defun mf--skip-comments ()
  "Skip over whitespace and comments."
  ;; until looking at non-whitespace, & not '/'
  ;; also not end of buffer...
  (while (and (not (= (point) (point-max))) 
              (not (looking-at "[^ \t\n/]"))) 
    (while (looking-at "[ \t\n]")       ; skip over whitespace.
      (forward-char))
    (if (looking-at "//")               ; skip over C++ style comments.
        (end-of-line))
    (if (looking-at "/\\*")             ; skip over C style comments.
        (re-search-forward "\\*/"))))

;; a lexical analyser for C++ buffers.
(defun mf--next-token ()
  "Starting at point, find the next token regexp that matches the following text."
  (mf--skip-comments)
  (some #'(lambda (token-regexp-dotted-pair) 
            (if (looking-at (cdr token-regexp-dotted-pair))
                (progn (goto-char (match-end 0))
                       (list (car token-regexp-dotted-pair) 
                             (buffer-substring (match-beginning 0) 
                                               (match-end 0))))
              (progn 
                (setq next-token-status nil)
                nil))) c++-token-regexp-alist))

(defun mf--tokenize ()
  "Process the current buffer, returning a list of tokens."
  (lexical-let ((next-token-status t))
    (do ((result (list (mf--next-token)) (cons (mf--next-token) result)))
        ((or (not next-token-status) (= (point) (point-max))) (reverse result)))))

(defmacro mf--tokenize-to (where)
  "Tokenize the current buffer, placing the resulting list in where."
  `(setq ,where (mf--tokenize)))

(defun mf--tokenize-buffer (&optional buffer)
  "Tokenize the specified buffer, or the current buffer, if none specified."
  (when buffer
    (set-buffer buffer)
    (goto-char (point-min)))
  (mf--tokenize))

(defun mf--blockify (token-list)
  "Replace matching pairs of braces with sublists.

Handles braces, brackets, and parentheses.  Something special is done
for template arguments lists."
  (lexical-let ((sub-expr-stack (list nil)))
    (dolist (el token-list (reverse (pop sub-expr-stack)))
      (cond ((eq (car el) 'open-brace)
             (push (list 'block) sub-expr-stack))
            ((eq (car el) 'open-paren)
             (push (list 'parens) sub-expr-stack))
            ((eq (car el) 'open-bracket)
             (push (list 'brackets) sub-expr-stack))
            ((eq (car el) 'close-brace)
             (push (reverse (pop sub-expr-stack))
                   (car sub-expr-stack)))
            ((eq (car el) 'close-paren)
             (push (reverse (pop sub-expr-stack))
                   (car sub-expr-stack)))
            ((eq (car el) 'close-bracket)
             (push (reverse (pop sub-expr-stack))
                   (car sub-expr-stack)))
            (t (push el (car sub-expr-stack)))))))

(defun mf--template-specify (token-list)
  "Identify and subordinate template specifiers."
  (lexical-let ((sub-expr-stack (list nil))
                (in-template nil)
                (template-depth 0))
    (dolist (el token-list (reverse (pop sub-expr-stack)))
      (cond ((equal el '(keyword "template"))
             (setq in-template t)
             (push (list 'template-spec) sub-expr-stack))
            ((equal el '(operator "<")) 
             (if in-template
                 (progn
                   (setq template-depth (+ 1 template-depth))
                   (push el (car sub-expr-stack)))
               (push el (car sub-expr-stack))))
            ((equal el '(operator ">"))
             (if in-template
                 (progn
                   (setq template-depth (- template-depth 1))
                   (if (= template-depth 0)
                       (progn
                         (setq in-template nil)
                         (push el (car sub-expr-stack))
                         (push (reverse (pop sub-expr-stack))
                               (car sub-expr-stack)))
                     (push el (car sub-expr-stack))))
               (push el (car sub-expr-stack))))
            (t (push el (car sub-expr-stack)))))))

(let ((current-template-spec nil))
(defun mf--next-class (blockified-list)
    "Find the next occurance of \"class\" in a blockified token list.
The returned list is the start of a complete class declaration."
    (let ((position (position-if #'(lambda (el) 
                                     (and (eq (car el) 'keyword) 
                                          (string-equal (cadr el) "class"))) 
                                 blockified-list))
          (result blockified-list))
      (if position
          (progn
            (if (eq (car (nth position blockified-list)) 'template-spec)
                (setq current-template-spec (nth position blockified-list))
              (setq current-template-spec nil))
            (setq result (subseq blockified-list position))
            (if (mf--complete-class-decl-p result)
                result
              (mf--next-class (cdr result))))
        nil)))

(defun mf--attach-template-spec (decl)
    "Hook the current-template-spec onto the given decl, if there is one."
    (if current-template-spec
        (cons current-template-spec decl)
      decl)))

(defun mf--complete-class-decl-p (blockified-list)
  "Does the blockified-list point at a full class declaration?
blockified-list must point at a class keyword."
  (if (< (position-if #'(lambda (el)
                          (eq (car el) 'block)) 
                      blockified-list)
         (position-if #'(lambda (el)
                          (eq (car el) 'statement-separator)) 
                      blockified-list))
      blockified-list
    nil))

(defun mf--class-name (blockified-list)
  "Get the name of the class at the beginning of blockified-list."
  (cadr (find-if #'(lambda (el)
                     (eq (car el) 'identifier)) 
                 blockified-list)))

(defun mf--class-decl-block (blockified-list)
  "Return the class declaration block.
The blockified-list must point at a full class declaration."
  (when blockified-list
    (cdr (find-if #'(lambda (el)
                      (eq (car el) 'block)) 
                  blockified-list))))

(defun mf--tokens-after-class-decl-block (blockified-list)
  "Get the part of a blockified list that follows the class declaration.
The blockified list must begin with a complete class declaration."
  (when blockified-list
    (subseq blockified-list 
            (+ 1 (position-if #'(lambda (el)
                                  (eq (car el) 'block))
                              blockified-list)))))

(defun mf--list-decls (class-decl-block-list)
  "Break the given class-decl-block-list up into declarations."
  (lexical-let ((sub-expr-stack (list nil)))
    (dolist (el class-decl-block-list (reverse sub-expr-stack))
      (cond ((eq (car el) 'statement-separator)
             (setf (car sub-expr-stack) 
                   (reverse (car sub-expr-stack)))
             (push (list) sub-expr-stack))
            ;; remove access specifiers from the list of decl's.
            ((and (and (eq (car el) 'punctuation) (string-equal (cadr el) ":"))
                  (and (eq (car (car (car sub-expr-stack))) 'keyword) ; check the last thing pushed.
                       (string-match "public\\|private\\|protected" 
                                     (cadr (car (car sub-expr-stack))))))
             (pop (car sub-expr-stack) )
             (push (list) sub-expr-stack))
            ((eq (car el) 'block)
             (push el (car sub-expr-stack))
             (setf (car sub-expr-stack) 
                   (reverse (car sub-expr-stack)))
             (push (list) sub-expr-stack))
            (t (push el (car sub-expr-stack)))))))

(defun mf--pure-virtual-p (decl)
  "True if decle is a pure virtual declaration."
  (and (equal (car (last decl)) '(integer-literal "0"))
       (equal (car (last decl 2)) '(operator "="))))

(defun mf--member-fn-decls (decls-list)
  "Filter a list of decls, removing any that are not function declarations."
  (remove-if-not
   #'(lambda (decl)
       (if (mf--pure-virtual-p decl)
           nil
         (let ((args (position-if #'(lambda (el) (eq (car el) 'parens)) decl)))
           (if (or (not args)
                   (eq (car (car (last decl))) 'block))
               nil
             (or (eq (car (nth (- args 1) decl)) 'identifier)
                 (and (eq (car (nth (- args 2) decl)) 'keyword)
                      (string-equal (second (nth (- args 2) decl)) "operator")))))))
   (mapcar #'(lambda (decl) (mf--attach-template-spec decl)) decls-list)))

(defun mf--prepend-class-scope-designators (class-name member-fn-decls-list)
  "Attach class:: to each member function declaration."
  (mapcar #'(lambda (fn) (mf--prepend-1-class-scope-designator class-name fn)) member-fn-decls-list))

(defun mf--prepend-1-class-scope-designator (class-name member-fn)
  "Attach class:: to one member function declaration."
  (let ((result nil)
        (has-template-spec nil)
        (template-spec nil)
        (found-parens nil))

    (when (eq (car (first member-fn)) 'template-spec)
      (setq has-template-spec t)
      (setq template-spec (first member-fn)))

    (dolist (c member-fn (reverse result))
      (cond ((eq (car c) 'parens)       ; arguments found...
             (cond (found-parens
                    (push c result))
                   ((and (= (length (second result)) 2) ; handle dtor
                         (string-equal (cadr (second result)) "~"))
                    (let ((id (pop result)))
                      (pop result)
                      (push (list 'identifier class-name) result)
                      (when has-template-spec
                        (mapcar #'(lambda (arg) (push arg result))
                                (cdr template-spec)))
                      (push (list 'scope-resolution "::") result)
                      (push (list 'operator "~") result)
                      (push id result)
                      (push c result)))
                   ((eq (car (car result)) 'identifier) ; handle named function
                    (let ((id (pop result)))
                      (push (list 'identifier class-name) result)
                      (when has-template-spec
                        (mapcar #'(lambda (arg) (push arg result))
                                (cdr template-spec)))
                      (push (list 'scope-resolution "::") result)
                      (push id result)
                      (push c result)))
                   (t (push c result)))
             (setq found-parens t))
            ((and (eq (car c) 'keyword)
                  (string-equal (second c) "operator")) ; handle overloaded operator 
             (push (list 'identifier class-name) result) ; N.B. still won't handle operator()
             (when has-template-spec
               (mapcar #'(lambda (arg) (push arg result))
                       (cdr template-spec)))
             (push (list 'scope-resolution "::") result)
             (push c result))
            (t (push c result))))))

(defun mf--fn-dfns (decls-list)
  "Filter a list of decls, removing any that are not function definitions."
  (remove-if-not
   #'(lambda (decl)
       (let ((args (position-if #'(lambda (el) (eq (car el) 'parens)) decl))
             (blk (position-if #'(lambda (el) (eq (car el) 'block)) decl)))
         (if (or (not args) (not blk))
             nil
           (or (eq (car (nth (- args 1) decl)) 'identifier)
               (and (eq (car (nth (- args 2) decl)) 'keyword)
                    (string-equal (second (nth (- args 2) decl)) "operator"))))))
   decls-list))

(defun mf--remove-preproc-dirs (token-list)
  "Remove preprocessor directives from the token list."
  (remove-if #'(lambda (el) 
                 (or (eq (car el) 'preproc-directive)
                     (eq (car el) 'include-directive)
                     (eq (car el) 'preproc-define)
                     (eq (car el) 'macro-define)))
             token-list))

(defun mf--remove-blocks (fn-defs-list)
  "Remove function definition blocks."
  (mapcar #'(lambda (def)
              (remove-if #'(lambda (el) (eq (car el) 'block)) def))
          fn-defs-list))

(defun mf--list-fn-defs-buffer (buffer-name)
  "Extract a list of function definitions from a .C file"
  (mf--fn-dfns 
   (mf--list-decls
    (mf--template-specify
     (mf--blockify 
      (mf--remove-preproc-dirs 
       (mf--tokenize-buffer buffer-name)))))))

(defun mf--list-member-fn-decls-buffer (buffer-name)
  "Extract a list of member function declarations from a .h file"
  (let ((tl (mf--blockify (mf--remove-preproc-dirs 
                       (mf--tokenize-buffer buffer-name))))
        (result (list)))
    (while (setq tl (mf--next-class (mf--template-specify tl)))
      (let ((mf--class-name (mf--class-name tl))
            (mf--class-decl-block (mf--class-decl-block tl)))
        (setq result (append result 
                             (mf--prepend-class-scope-designators 
                              mf--class-name 
                              (mf--member-fn-decls (mf--list-decls mf--class-decl-block))))))
      (setq tl (mf--tokens-after-class-decl-block tl)))
    result))

(defun mf--split-at-every (list test)
  "Split a list at every element that matches test. 
Matching elements are discarded."
  (let ((both (mf--split-at list test)))
    (cond ((= (length both) 2)
           (cons (first both) (mf--split-at-every (second both) test)))
          (t both))))

(defun mf--split-at (list test)
  "Split a list at first element that matches test. 
Matching element is discarded."
  (do ((left nil (cons (car right) left))
       (right list (cdr right)))
      ((null right) (list (nreverse left)))
    (if (funcall test (car right))
          (return-from nil (list 
                          (nreverse left) 
                          (cdr right))))))

;; This one is tricky, hence the copious commentary.
(defun mf--equal-param-decl (d1 d2)
  "Check that a single parameter declaration matches another."
  (let ((where (mismatch d1 d2 :test #'equal)))
    (if (not where)                     ; no mismatch.
        t
      (let ((a1 (nth where d1))         ; get the thing at the point of mismatch
            (a2 (nth where d2)))        ; for both arguments.
        ;; There are several possible cases here:
        (cond
         ;; 1. both args have identifiers at the mismatch point, and
         ;; the identifiers are not the same string.
         ((and (eq (car a1) 'identifier) 
               (eq (car a2) 'identifier))
          (equal (nthcdr (+ 1 where) d1) ; check that the rest of the list is the same.
                  (nthcdr (+ 1 where) d2)))
         ;; 2. one or the other argument list is missing an identifier
         ;; at the point where the mismatch occurs.
         ((eq (car a1) 'identifier)
          (equal (nthcdr (+ 1 where) d1) 
                  (nthcdr where d2)))   ; check that the rest of the list is the same.
         ((eq (car a2) 'identifier)
          (equal (nthcdr (+ 1 where) d2)
                  (nthcdr where d1)))   ; check that the rest of the list is the same.
         ;; anything else means not equal.
         (t nil))))))

(defun mf--equal-args (args1 args2)
  "Compare two argument lists."
  (labels ((comma-op-p (el) (and 
                          (eq (car el) 'operator)
                          (string-equal (second el) ","))))
    (or (equal args1 args2)            ; identical args
        ;; Split the args lists into individual argument declarations
        ;; at comma operators.
        ;; Compare the individual declarations; they must be in the same
        ;; order, so a comparison function can be mapped over them
        ;; pairwise.
        (every #'(lambda (a b) (mf--equal-param-decl a b)) 
               (mf--split-at (cdr args1)
                #'(lambda (x) (comma-op-p x)))
               (mf--split-at (cdr args2)
                #'(lambda (x) (comma-op-p x)))))))

(defun mf--equal-decls (decl1 decl2) 
  "Compare two decls.  
Decls are the same even if they differ in the names, but
not the types, of their formal parameters."
  (or (equal decl1 decl2)              ; identical decls.
      (do* ((d1 decl1 (cdr d1))
            (d2 decl2 (cdr d2))
            (el1 (car d1) (car d1))
            (el2 (car d2) (car d2))
            (same (equal el1 el2) (equal el1 el2)))
          ((or (null d1) (null d2)) same)
        (if (and (eq (car el1) 'parens)
                 (eq (car el2) 'parens))
            (progn
              (setq same (mf--equal-args el1 el2))
              (return-from nil same))
          (if (not same)
              (return-from nil same))))))
        

(defun mf--undefined-decls (decl-list dfn-list)
  "Find all decls in decl-list that do not appear in dfn-list.  Need
to add some code that will treat decl's as equivalent if one or the
other has no variable named in a parameter declaration."
  (let ((result (list ))
        (clean-dfn-list (mf--remove-blocks dfn-list)))
    (dolist (decl decl-list result)
      (when (not (find decl clean-dfn-list :test #'mf--equal-decls))
        (push decl result)))))

(defun mf--remove-decl-specifiers (decl)
  "Strip virtual and static from a declaration."
  (remove-if #'(lambda (el) 
                 (and (eq (car el) 'keyword)
                      (or (string-equal (cadr el) "static")
                          (string-equal (cadr el) "virtual")
                          (string-equal (cadr el) "explicit"))))
             decl))

(defun mf--remove-specifiers (decl-list)
  "Strip virtual and static form all declarations in a declarations list."
  (mapcar (function mf--remove-decl-specifiers) decl-list))

(defun mf--remove-default-args (decl-list)
  "Remove default arguments from the formal args list of a declaration.  
This is definitely needed for xlC on AIX, though I haven't checked whether it is 
also needed for gcc."
  (mapcar #'(lambda (decl)
              (let ((result (list )))
                (dolist (d decl (reverse result))
                  (cond ((equal (car d) 'parens)
                         (let* ((rr (list ))
                                (pp (dolist (el (cdr d) (reverse rr))
                                      (cond ((equal (car rr) '(operator "="))
                                             (pop rr))
                                            (t (push el rr))))))
                           (if pp
                               (push (cons 'parens pp)
                                     result)
                             (push (list 'parens) result))))
                        (t (push d result))))))
          decl-list))

(defun mf--class-name-decl (decl)
  "Examine a function decl list, return the class name."
  (do* ((remaining-decl decl (cdr remaining-decl))
        (last-token nil token)
        (token (car decl) (car remaining-decl)))
      ((eq (car token) 'scope-resolution) (second last-token))))

(defun mf--function-name-decl (decl)
  "Examine a function decl list, return the function name."
  (do* ((remaining-decl decl (cdr remaining-decl))
        (last-token nil token)
        (token (car decl) (car remaining-decl)))
      ((eq (car token) 'parens) (second last-token))))


(defun mf--date-string ()
  (let ((time (decode-time (current-time))))
    (format "%s/%s/%s" (nth 4 time) (nth 3 time) (nth 5 time))))

(defun mf--format-comment-block (decl date-string)
  "Format a comment string for a member function definition."
  (if mf--insert-commentary
      (format (concat "%s\n// CLASS NAME: %s\n" 
                  "// MEMBER NAME: %s \n"
                  "// DESCRIPTION:\n//\n//\n"
                  "// HISTORY: \n//\t%s\t%s\tCreated\n//\n")
          (make-string 70 ?/)
          (mf--class-name-decl decl)
          (mf--function-name-decl decl)
          date-string
          (funcall mf--user-name-function))
    ""))

(defun mf--format-empty-template-definition (decl)
  "Format for insertion, where the given decl comes with a template spec."
  (let ((result-string "")
        (template-string "template")
        (spacer ""))
    (let ((template-part
           (dolist (el (cdr (first decl)) template-string)
             (setq template-string (concat template-string " " (second el)))))
          (final 
           (dolist (el (cdr decl) result-string)
             (cond ((eq (car el) 'parens)
                    (setq result-string (concat result-string (mf--format-parens (cdr el)))))
                   ((string-equal (cadr el) "::")
                    (setq result-string (concat result-string "::")) 
                    (setq spacer ""))
                   ((string-equal (cadr el) "~")
                    (setq result-string (concat result-string "~"))
                    (setq spacer ""))
                   (t (setq result-string (concat result-string spacer (cadr el)))
                      (setq spacer " "))))))
      (concat template-part "\n" final "\n{\n}\n"))))

(defun mf--format-empty-definition (decl)
  "Format a blank definition for insertion in the source code."
  (let ((date-string (mf--date-string)))
    (if (eq (car (car decl)) 'template-spec)
        (concat (mf--format-comment-block decl date-string) 
                (mf--format-empty-template-definition decl))
      (progn
        (let ((result-string "")
              (spacer ""))
          (let ((final 
                 (dolist (el decl result-string)
                   (cond ((eq (car el) 'parens)
                          (setq result-string (concat result-string 
                                                      (mf--format-parens (cdr el)))))
                         ((string-equal (cadr el) "::")
                          (setq result-string (concat result-string "::"))
                          (setq spacer ""))
                         ((string-equal (cadr el) "~")
                          (setq result-string (concat result-string "~"))
                          (setq spacer ""))
                         (t (setq result-string (concat result-string spacer (cadr el)))
                            (setq spacer " "))))))
            (concat (mf--format-comment-block decl date-string) final "\n{\n}\n")))))))

(defun mf--format-parens (parens-list)
  "Format the formal parameters list for insertion in the source code."
  (let* ((result-string "(")
         (final (dolist (el parens-list result-string)
                  (cond ((string-equal (cadr el) ",")
                         (setq result-string (concat 
                                              (substring result-string 0 -1) 
                                              (cadr el) " ")))
                        (t (setq result-string (concat 
                                                result-string 
                                                (cadr el) " ")))))))
    (if (not (string-equal "(" (substring final -1)))
        (concat (substring final 0 -1) ")")
      (concat final ")"))))

(defun mf--undefined-decls-from-files (header-file dot-c-file)
  "Find all member functions in the given header file that have no definitions in the given .C file."
  (mf--undefined-decls 
   (mf--remove-specifiers 
    (mf--remove-default-args 
     (mf--list-member-fn-decls-buffer header-file))) 
   (mf--list-fn-defs-buffer dot-c-file)))

(defun mf--format-undefined-decls-from-files (header-file dot-c-file)
  "Format all of the found undefined member function declarations for insertion into the .C file."
  (mapconcat #'(lambda (el) (mf--format-empty-definition el))
             (reverse (mf--undefined-decls-from-files 
                       header-file dot-c-file)) "\n"))

(defun mf--infer-c-filename (header-filename)
  "Guess a .C file name."
  (if (string-match 
       (concat "\\." (regexp-quote mf--header-file-extension) "$") 
       header-filename)
      (concat (substring header-filename 0 -1) mf--source-file-extension)
    nil))

(defun mf--expand-member-functions-args (str1 str2)
  "Prompt user for header and C file names."
  (let* ((header-file nil) 
         (c-file nil)
         (default-header (buffer-name (current-buffer)))
         (default-c-file (mf--infer-c-filename default-header)))
    (setq header-file 
          (read-from-minibuffer (format "%s: " str1)
                                default-header nil nil
                                'query-replace-history))
    (setq c-file 
          (read-from-minibuffer (format "%s: " str2)
                                default-c-file nil nil
                                'query-replace-history))
    (list header-file c-file)))

(defun mf--in-comment-p ()
  "Is point in a comment?"
  (let ((where (point)))
    (save-excursion
      (beginning-of-line)               ; check current line for c++ comment.
      (let ((this-line (buffer-substring (point) where)))
        (if (string-match "//.*$" this-line)
            t
          (progn
            (goto-char where)           ; check for potentially multi-line c style comment
            (search-backward-regexp "/\\*" (point-min) t)
            (let ((comment-end (search-forward-regexp "\\*/" (point-max) t)))
              (if comment-end
                  (> comment-end where)
                nil))))))))

(defun mf--evacuate-blocks ()
  "March across the current buffer removing the contents of top-level blocks."
  (let (start end)
    (while (search-forward "{" nil t)
      (if (not (mf--in-comment-p))
          (progn
            (backward-char)
            (setq start (point))
            (forward-sexp)
            (setq end (- (point) 1))
            (delete-region (+ start 1) end))))))

(defun mf--make-temp-c-file (cfile)
  "Create a temporary .C file, with top-level blocks evacuated.
This is an optimization.  The idea is that by removing the contents of
the top-level blocks from the .C file, we can save a great deal of
time tokenizing the buffer, especially when there are member function
definitions with large bodies.

Returns the name of a temporary buffer.  The caller should delete the
temp buffer when he is done with it."
  (let ((buffer-name (make-temp-name cfile))
        (contents (buffer-substring (point-min) (point-max))))  ; Assumes cfile is already current buffer.
    (save-excursion
      (find-file-noselect buffer-name t) ; t means don't warn.
      (set-buffer buffer-name)
      (insert contents)
      (goto-char (point-min))
      (mf--evacuate-blocks))
    buffer-name))

;;; Entry point

(defvar mf--saved-string nil)

(defun expand-member-functions (header c-file)
  "Expand C++ member function declarations into member function definitions, where needed.

When invoked interactively with \\[expand-member-functions], prompts
for a header file, and the corresponding implementation file.

Searches the header file for class declarations.  For every class
declaration found, all of the declared member functions are collected.
Functions defined inline are ignored.

After all declarations are found, the implementation file is searched
for member function definitions.  The list of member function
declarations is then compared to the list of member function
definitions.

Finally, the member function declarations that have no corresponding
definitions are formatted and inserted, with commentary, into the
implementation file."
  (interactive (mf--expand-member-functions-args "Header file" "Implementation file"))
  (save-window-excursion
    (save-excursion
      (save-restriction
        (find-file-noselect header)
        (find-file-noselect c-file)
        (set-buffer c-file)
        (goto-char (point-max))
        (when (and mf--checkout-if-readonly buffer-read-only)
            (vc-toggle-read-only))
        ;; note: Optimized by evacuating function definition blocks in c-file.
        (let ((temp-file (mf--make-temp-c-file c-file)))
	  (save-excursion
	    (set-buffer (get-buffer c-file))
	    (setq mf--saved-string (mf--format-undefined-decls-from-files header temp-file))
	    (set-buffer (get-buffer c-file))
	    (insert mf--saved-string)
	    (set-buffer (get-buffer temp-file))
	    (set-buffer-modified-p nil))
          (kill-buffer temp-file))))))

(provide 'member-function)

;;; member-function.el ends here.
