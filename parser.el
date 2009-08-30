;;; parser.el --- Elisp Macro DSL Parser Compiler

;; Copyright (C) 2008 Mike Mattie

;; Author: Mike Mattie <codermattie@gmail.com>
;; Maintainer: Mike Mattie <codermattie@gmail.com>
;; Created: 2008-01-04
;; Version: 0.0.6
;; Keywords: parser
;; License: LGPL <http://www.gnu.org/licenses/lgpl.html>

;;; Commentary:

;; An experimental parser-compiler DSL that emphasizes concise and
;; flexible expression with a lispy interface to render gnarly
;; scripting tasks trivial as parsing in Emacs should be.

;; - "This experiment calls forth the four horsemen of the Lisp
;;    Apocalypse: eval,apply,lambda,macro."

;; The detailed documentation is in the docstrings for "parser-compile"
;; and "parser-token-api". A lengthier tutorial and paper are in progress.

;; -> Status

;; greed,not operators support PEG grammars with right recursion only. Next
;; step is left-recursion via packrat-optimization.

;; If you do not know what PEG grammars are check out this brief introduction:
;; http://en.wikipedia.org/wiki/Parsing_expression_grammar

;; I will support the CFG grammar class with a lazy operator but that is
;; a lower priority than left recursion.

;; -> Hypothesis

;; Exposing the internal semantics of the generated parser as a well
;; defined programming interface allows a parser writer to freely mix
;; the semantics of a grammar classes such as CFG and PEG with
;; tailored parsing behavior.

;; The programming interface is powerful enough that user defined
;; functions can be reduced to a few typed hooks without losing the
;; ability to define useful or unusual parsers.

;; -> Benefits

;; A well defined programming interface has the following significant
;; advantages.

;; Increasing the generality of the parser compiler beyond a single
;; grammar class:

;; * brings more of the parser sum within formal validation reducing
;;   the opportunity to introduce defects.

;; * The ability to articulate a variety of phenomena [grammar classes here]
;;   with the analysis of expression implied by the compiler facilitates
;;   repeatable and defensible implementation.

;; The sum parser definition is more concise by virtue of raising the
;; abstraction level and encompassing more of the parser's true extent
;; of definition = less call-outs to write because the parser is
;; generating more code for you.

;; If you do need call-outs you should expect a good API and hooks
;; at all vital points of a Parser Function.

;; This compiler facilitates new parsing operators, making it a good
;; vehicle for experimenting with new grammar classes.

;; -> Features

;; 0. Parser compilation is integrated directly into lisp
;;    with a macro DSL that returns an entry point.

;; 1. The DSL is sugared with grammar classes and a define
;;    syntax keeping the parser definitions concise for
;;    both simple and complex parsers.

;; 2. Selective parse tracing for debugging parsers.

;; 3. Generated code dumper for debugging the compiler.

;; -> The Plan

;;   -> Phase 1: correctness

;; 0. new parser generator merged in, and debugged.

;;    UPDATE: testing new co-routine design generator.

;; 1. Canonical tree walk implemented as parser-ast-node. a real pre-requisite to
;;    sane user implementation of transforms.

;; 2. implement a error recovery routine. This involves marking tokens as being
;;    sync tokens, and generating a recovery function that scans for those tokens.

;;    -> Phase 2: optimization

;; 1. Implement packrat backtrack optimization.

;;    the lifetime of a memoization table for a recursion will be set
;;    by term relation operators, so that unusual things like longest or
;;    lazy can be implemented correctly.

;; memoization is part of the input domain. The lifetime of memoization
;; needs to be a property on the input stack.

;; 2. make left recursion work.

;;    -> Phase 3: Experiments

;;    ->Terminology

;; My reference for parsing terminology is the Dragon Book:

;; Compilers
;; Principles,Techniques,and Tools
;; Alfred V.Aho, Ravi Sethi, Jeffrey D.Ullman
;; 1986, Addison Wesley

;;; Code:

(require 'cl)
(require 'parser-fn)
(require 'closure)

(define-error parser-compile-error   "parser error")
(define-error parser-syntactic-error "syntactic error" parser-compile-error)

;; is this even useful anymore ?
(define-error parser-semantic-error  "semantic error" parser-compile-error)

(defconst parser-release-version "0.0.6"
  "the release number of parser.el")

;;----------------------------------------------------------------------
;; Backtracking.
;;----------------------------------------------------------------------

;; parser-position

;; The position of the parser in the input stream is maintained as a
;; stack of indexes into the buffer. As matching progresses the top of
;; the stack (car) is updated. The push and pop operations copy a
;; position to a next or previous stack position. backtrack discards
;; the top of the stack.

(defun parser-pos ()
  "Return the current position of the parser in the buffer"
  (car parser-position))

(defun parser-push ()
  "Copy the parser position to a new stack level so the parser can backtrack when necessary."
  (push (parser-pos) parser-position))

(defun parser-pop ()
  "Copy the parser position to a previous stack level When the possibility of a backtrack
   has been eliminated by matching."
  (let
    ((current (pop parser-position)))
    (setcar parser-position current) ))

(defun parser-backtrack ()
  "Restore the previous parser position by discarding the top of the parser-position stack.
   Always returns nil so it can be used as a failure Match Result."
  (pop parser-position)
  (goto-char (parser-pos))
  nil)

(defun parser-advance ( consumed )
  "Advance the input position of the parser to the next un-matched character."

  (when (> consumed 0)
    (goto-char (setcar parser-position (+ consumed (parser-pos)))) ))

(defun parser-consumed ()
  "The number of input characters consumed by the token's match in the input."
  (- (match-end 0) (match-beginning 0)))

;;----------------------------------------------------------------------
;; compiler diagnostics
;;----------------------------------------------------------------------

;; construct meaningful compiler error messages in the
;; "expected: foo got: bar" form.

(defun parser-expr-diagnostic ( form )
  (format "type(%s) %s" (symbol-name (type-of form)) (pp (eval form))))

;; TODO: can this be a defun ? if it can it should.
(defmacro parser-diagnostic ( form from expected )
  "syntax: (parser-diagnostic form from expected)

   Where form is the expr received, from is the component issuing the diagnostic,
   and expected is a message describing what the component expected"
  `(concat (format "[%s] expected: " ,from)  ,expected " not: " ,(parser-expr-diagnostic form)))

;;----------------------------------------------------------------------
;; Parser Tracing
;;----------------------------------------------------------------------

;; A tracing facility that can be selectively turned on and off for
;; productions. When tracing is turned on the result of all matches
;; attempted are printed to a buffer, or the Message buffer.

;; A report of how the compiled parser matched the input stream is
;; vital to developing a working grammar.

(defun parser-trace-message ( format &rest args )
  "Prints to a trace buffer instead of the Message buffer."
  (with-current-buffer parser-trace-buffer
    (goto-char (point-max))
    (insert (apply 'format format args))))

(defun parser-trace-match ( match-func match-result )
  "Trace the current match if the parser-trace-flag is bound to t"
  (if (and (boundp 'parser-trace-flag) (eq t parser-trace-flag))
    (funcall
      (if (boundp 'parser-trace-buffer)
        'parser-trace-message
        'message)

      "%s at: %d match: %s\n"

      (if (symbolp match-func)
        (symbol-name match-func)
        "anonymous")

      (parser-pos)
      (pp-to-string match-result)) ))

(defun parser-trace-p ( production )
  "Given a Match Function determine if parser-trace-flag should
   be set. The parser-trace list is scanned for a symbol match.
   The return value is a cons of a boolean indicating whether to
   set the flag, and the value of the flag.

   The parser-trace list is created by the macro parser-trace-list
   in utilities."

  (catch 'abort
    (unless (and (boundp 'parser-trace) (listp parser-trace)) (throw 'abort nil))

    ;; FIXME: replace this junk with assoc.
    (lexical-let
      ((toggle (eval (cons 'or (mapcar
                                 (lambda ( trace-on )
                                   (if (eqn production (car trace-on))
                                     (cdr trace-on)))
                                 parser-trace) ))))
      ;; a cons cell is returned so that a false value for the trace flag can be returned
      ;; without negating the truth value of the predicate itself.
      (if toggle
        (cons t toggle)
        (cons nil nil) )) ))

(defmacro parser-trace-on ( production &rest body )
  "parser-trace-on takes production and a code block code. If the production
   is on the parser-trace list a parser-trace-flag dynamically scoped is
   bound to the boolean toggle for tracing that production."

  ;; FIXME: debug why this debug spec is broken.
  (declare (debug symbolp body))

  ;; Using the dynamic scoping of let during the execution of the
  ;; compiled parser to scope parser-trace-flag gives tracing behavior
  ;; that precisely matches the execution of the parser.

  `(lexical-let*
    ((code-func (lambda () ,@body))
      (trace-p (parser-trace-p ,production))
      (trace-toggle (cdr trace-p)) )

     (if (and
           (car trace-p)

           ;; This expression attempts to minimize duplicate binding
           ;; of parser-trace-flag. If there are flaws in the tracing
           ;; behavior stemming from this expression it should be
           ;; removed entirely.
           (or
             (not (boundp 'parser-trace-flag))
             (not (eq parser-trace-flag trace-toggle))))
       (let
         ((parser-trace-flag trace-toggle))
         (funcall code-func))

       (funcall code-func)) ))

;;----------------------------------------------------------------------
;; Match Result Type
;;----------------------------------------------------------------------

;; The parser functions have a standard structure for returning the
;; result of a Match Function.

;; nil | (parser . AST ).

;; nil indicates failure to match.

;; A successful match has a parser and AST parts.

;; The parser part is either a count of input characters consumed
;; for a terminal or t|nil for non-terminals. When the parser
;; position is advanced the count is replaced with t.

;; The AST part is created by a token action handler or a combination
;; operator like and.

;; terminal     ( match-symbol . ( start . end ) | "user value")
;; non-terminal ( match-symbol . "a list/tree of AST" )

(defun parser-result-logic ( result )
  "The logical part of a Match Result cons cell is returned."
  (car result))

(defun parser-result-ast ( result )
  "The AST part of a Match Result cons cell is returned."
  (cdr result))

(defun parser-match-p ( result )
  "return true if the logic is a match result"
  (and (consp result) (parser-result-logic result)))

(defun parser-negate-match ( result )
  "negate the Match Result RESULT toggling the logical sense of the Match Result while
   preserving the AST value."
  (cons
    (if (parser-result-logic result)
      nil
      t)
    (parser-result-ast result)))

(defun parser-result-match ( &optional ignore )
  "return a data-less positive Match Result. Can be used as the
   logical optional operator with an optional argument that is
   ignored."
  (cons t nil))

(defun parser-result-token ( ast )
  "Return a token Match Result."
  (cons (parser-consumed) ast))

;;----------------------------------------------------------------------
;; AST Tree Type
;;----------------------------------------------------------------------

;; AST is a tree or list where the first element is an identity symbol
;; with the parser-ast property production. subsequent elements are
;; either sub-trees identifiable with the production property or
;; tokens which are arbitrarily structured data cons to an identity
;; symbol.

(defun parser-make-ast ()
  "Create an ast tree with a null identity."
  (lexical-let
    ((ast (cons 'null nil)))
    (put (car ast) 'parser-ast 'production)
    ast))

(defun parser-ast-p ( node )
  (eq 'production (get (car node) 'parser-ast)))

(defun parser-ast-merge-node ( node )
  "parser-ast-merge-node NODE

Merging AST nodes A and B is a union of A and B implemented as
list B appended to list A. The first element of list B is dropped
so that the merged node retains the identify of A. The complexity
of the merge is linear for n elements of B since the tail of A is
dynamically scoped.

the A node is dynamically scoped as the tail while the B node is
supplied as the single argument NODE."

  (lexical-let
    ((children (cdr node)))

    (setcdr parse-tree children)
    (setq parse-tree (do ((x children))
                       ((null (cdr x)) x)
                       (setq x (cdr x))) )))

(defun parser-ast-add-node ( node )
  "parser-ast-add-node NODE

Adding a node X to ast Y simply makes X an element of Y. The
structure of X is opaque to this operation unlike merging -
suitable for injecting arbitrary data into AST.

the A node is dynamically scoped as the tail while the X node is
supplied as the single argument NODE."
  (setq parse-tree (setcdr parse-tree (cons node nil))))

(defun parser-consume-match ( match-result )
  "Consume any unconsumed AST in Match Result. The AST
   is cleared from the Match Result so that the AST is
   only modified once. The logical value of the Match
   Result is preserved."

  (catch 'consumed-match
    (lexical-let
      ((match-status (parser-result-logic match-result))
       (match-data   (parser-result-ast   match-result)))

      (if (numberp match-status)
        (progn
          (parser-advance match-status)

          (unless (and (symbolp match-data) (null match-data))
            (parser-ast-add-node match-data))

          (throw 'consumed-match (parser-result-match)))

        ;; The alternative to a token is a possible un-consumed
        ;; production. Consumption occurs regardless of the logical
        ;; sense of the Match Result. This keeps logic and AST
        ;; orthogonal.

        ;; It is a trivial change to make consumption conditional if
        ;; the orthogonality is found to be too strange, but I doubt
        ;; this orthogonality will be visible at the semantics level
        ;; unless the user is deliberately introducing it.

        (when match-data
          (if (parser-ast-p match-data)
            (parser-ast-merge-node match-data)
            (parser-ast-add-node match-data))

          (throw 'consumed-match
            (if match-status
              (parser-result-match)
              (cons nil nil)))))

      match-result)))

;;----------------------------------------------------------------------
;; Parser Function Semantic Closure Type
;;----------------------------------------------------------------------

(closure-define parser-function-semantics

    ;; -> Call Phase

    (pf-terms           nil)
    ;; a list of Parser Functions that imply pf-relation as a Term
    ;; Relation.

    (pf-relation        nil)
    ;; any Parser Function, usually a Term Relation.

    (pf-closure         nil)
    ;; A function that receives the Match Call phase as a lambda for
    ;; delayed evaluation within the Recursion Environment.

    (pf-call-phase      nil)
    ;; The call phase is considered open when the only instructions
    ;; processed are calls. the first non-call instruction that
    ;; modifies the closure closes the call phase with a non-nil value.

    ;; -> Match Evaluation Phase

    (pf-eval-logic      nil)

    ;; -> Input Domain

    (pf-input-domain    nil)
    (pf-input-branch    nil)

    ;; -> AST domain

    (pf-ast-domain      nil)
    (pf-ast-branch      nil)

    (pf-ast-discard     nil)
    ;; Discard the ast tree.  Discard is mutually exclusive with the
    ;; set below.

    (pf-ast-transform   nil)
    ;; user defined AST transform function.

    (pf-ast-node        nil)
    ;; Create named AST trees that are attached instead of merged. The
    ;; value is the identifier for the tree.

    ;; -> Rvalue phase

    (pf-rvalue-phase   'eval)

    ;;r-value is derived from:

    ;; eval        : result of eval
    ;; match       : the match result, discard eval except effects, AST is still orthogonal.
    ;; entry-point : special case.

    (pf-rvalue-logic    nil)
    ;; apply an operator to the evaluation or match to alter the
    ;; logical value returned.
  )

(defun parser-any-changed-except ( closure definition exceptions )
  (catch 'terminate
    (mapc
      (lambda ( def )
        (lexical-let
          ((symbol     (car def))
           (init-value (eval (cdr def))))

          (unless (memq symbol exceptions)
            (unless (eq init-value (closure-value (car def) closure))
              (throw 'terminate t))) ))
      definition)
    nil))

(defun parser-pf-new ()
  "parser-pf-new

   Create a new Parser Function closure.
  "
  ;; This function is necessary because you currently cannot specify
  ;; properties in define-closure.
  (lexical-let
    ((closure (closure-create parser-function-semantics)))

    (put
      (closure-symbol 'pf-rvalue-phase closure)
      'parser-primitive 'weak)

    closure))

(defun parser-pf-closed-p ( pf-closure )
  "parser-pf-closed-p CLOSURE

   Return t if CLOSURE has the pf-call-phase flag set, nil
   otherwise. When pf-call-phase is set adding terms is a
   collision.
  "
  (if (closure-value 'pf-call-phase pf-closure)
    t))

(defun parser-pf-terms ( pf-closure )
  (closure-value 'pf-terms pf-closure))

(defun parser-pf-single-call-p ( pf-closure )
  (lexical-let
    ((terms (parser-pf-terms pf-closure)))

    (if (and
          terms
          (< (length terms) 2)
          (symbolp (car terms))
          (not (parser-any-changed-except pf-closure parser-function-semantics '(pf-terms pf-call-phase))))
      (car terms)) ))

(defun parser-pf-call ( pf-closure call )
  "match-function is pushed onto the current sequence, returns t
   if there was a semantic collision, nil otherwise."
  (if (parser-pf-closed-p pf-closure)
    t
    (progn
      (set
        (closure-symbol 'pf-terms pf-closure)
        (cons call (parser-pf-terms pf-closure)))
      nil)))

;;----------------------------------------------------------------------
;; Parser Primitive Type
;;----------------------------------------------------------------------

(defun parser-strong-primitive ( p &optional v )
  (lexical-let
    ((primitive (make-symbol (symbol-name p))))

    (set primitive (if v
                     v
                     t))
    primitive))

(defun parser-weak-primitive ( p &optional v )
  (lexical-let
    ((primitive (parser-strong-primitive p v)))

    (put primitive 'parser-primitive 'weak)

    primitive))

(defun parser-weak-p ( p )
  (eq (get p 'parser-primitive) 'weak))

;;----------------------------------------------------------------------
;; Compile Instruction Type
;;----------------------------------------------------------------------

(defun parser-instruction ( instr &optional data )
  (if data
    (cons instr data)
    instr))

(defun parser-pp-instruction ( x )
  (if (consp x)
    (concat
      "i> " (symbol-name (car x)) " data= " (pp-to-string (cdr x)))
    (concat
      "p> " (symbol-name x) " v= " (if (boundp x) (pp-to-string (symbol-value x)) "void"))))

(defun parser-pp-tape ( tape )
  (apply 'concat
    (mapcar
      (lambda (i)
        (format "%s\n" (parser-pp-instruction i)))
      tape)))

;;----------------------------------------------------------------------
;; compile trace
;;----------------------------------------------------------------------

(defun parser-compile-trace-p ()
  "return non-nil if a compile trace is available"
  (and (boundp 'parser-compile-trace) (bufferp parser-compile-trace)))

(defun parser-compile-trace-append ( trace )
  (with-current-buffer parser-compile-trace
    (goto-char (point-max))
      (insert trace)))

(defmacro parser-compile-trace ( name conv &rest args )
  `(when (parser-compile-trace-p)
     (parser-compile-trace-append (format "%s\n\n" ,name))

     ,@(if args
         (mapcar
           (lambda ( data )
             `(parser-compile-trace-append
                (format "%s\n" (,(if (> (length conv) 1)
                                   (pop conv)
                                   (car conv))
                                 ,data))))
           args)
         'nil) ))

(defun parser-compile-message ( from message )
  (when (parser-compile-trace-p)
     (parser-compile-trace-append (format "%s: %s\n" from message))))

;;----------------------------------------------------------------------
;; Parser Feedback
;;----------------------------------------------------------------------

;; the two co-routines of the compile: Semantic Union and the Function
;; Generator pass commands through feedback into the instruction
;; sequence.

;; these helpers construct r-values interpreted by parser-feedback.

(defun parser-retry-after ( &rest before )
  (cons 'retry (list-filter-nil before)))

(defun parser-continue-after ( &rest before )
  (cons 'continue (list-filter-nil before)))

;;----------------------------------------------------------------------
;; Semantic Union
;;----------------------------------------------------------------------

;; parser-pf-{set,merge,fold}-* express the rules for membership
;; in a valid set of Parser Function semantics (the closure). The
;; closure is not modified until validity is checked.

;; These functions either return nil, or a feedback signal used
;; to select a recovery method.

(defun parser-pf-set-primitive ( pf-symbol primitive )
  "parser-pf-set-primitive PF-SYMBOL PRIMITIVE

   Merge PRIMITIVE into PF-SYMBOL.

   If PF-SYMBOL is set as a strong primitive collision is
   returned; unless PRIMITIVE is weak, when PRIMITIVE is silently
   discarded.

   If PF-SYMBOL is a weak primitive the parser-primitive property
   is cleared to nil and the value of PRIMITIVE is merged.

   if PF-SYMBOL has no value, the value and primitive-property of
   PRIMITIVE is merged silently.
  "
  (if (and
        (symbol-value pf-symbol)
        (not (parser-weak-p pf-symbol)))

    (unless (parser-weak-p primitive)
      'collision)

    (progn
      (put pf-symbol 'parser-primitive
        (if (parser-weak-p pf-symbol)
          nil
          (get primitive 'parser-primitive)))

      (unless (symbol-value primitive)
        (signal 'parser-semantic-error
          (format "primitive %s is missing a value." (symbol-name primitive))))

      (set pf-symbol (symbol-value primitive))
      nil)))

(defun parser-pf-fold-primitive ( pf-symbol primitive )
  "parser-pf-fold-primitive PF-SYMBOL PRIMITIVE

  If PF-SYMBOL was previously set subsequent primitives are silently
  discarded
  Merge primitive P, silently discarding subsequent primitives
  assuming that V value is a constant non-nil such as 't.

  invalidate the merge with 'incomplete only when the function
  and value of V are nil or void.
  "
  (unless (symbol-value pf-symbol)
    (parser-pf-set-primitive pf-symbol primitive)))

(defun parser-merge-domain ( domain )
  "parser-merge-domain DOMAIN

   When primitives are exclusive within DOMAIN enforce one domain per function.

   'hard-collision is thrown for subsequent merges of a previous
   value.
  "
  (if (symbol-value domain)
    'collision
    (progn
      (set domain t)
      nil)))

(defun parser-merge-domain-exclusive ( domain pf-symbol primitive )
  "parser-merge-domain-exclusive DOMAIN PF-SYMBOL PRIMITIVE

   Merge PF-SYMBOL exclusive within DOMAIN with PRIMITIVE.

   any parser-primitive properties for PF-SYMBOL or PRIMITIVE
   supersede exclusion when PF-SYMBOL is not null.
  "
  (if (and
        (eq 'collision (parser-merge-domain domain))
        (null (symbol-value pf-symbol)))
    'collision
    (parser-pf-set-primitive pf-symbol primitive) ))

(defun parser-merge-domain-exclusive-except ( except domain pf-symbol primitve )
  "parser-merge-domain-exclusive-except EXCEPT DOMAIN PF-SYMBOL PRIMITIVE

   Merge PF-SYMBOL exclusive within DOMAIN with
   PRIMITIVE.

   the exclusion is superseded by the parser-primitive
   properties of PF-SYMBOL or primitive if PF-SYMBOL
   or EXCEPT is not null.
  "
  (if (and
        (eq 'collision (parser-merge-domain domain))
        (null (symbol-value except))
        (null (symbol-value pf-symbol)))
    'collision
    (parser-pf-set-primitive pf-symbol primitive) ))

(defun parser-merge-domain-combine ( domain pf-symbol primitive )
  "parser-merge-domain-combine DOMAIN PF-SYMBOL PRIMITIVE

   Combine PF-SYMBOL within DOMAIN.
  "
  (parser-pf-fold-primitive domain t)
  (parser-pf-set-primitive pf-symbol primitive))

(defun parser-merge-domain-combine-except ( except domain pf-symbol primitive )
  "FIXME"
  (if (symbol-value except)
    'collision
    (progn
      (parser-pf-fold-primitive domain t)
      (parser-pf-set-primitive pf-symbol primitive))))

;;----------------------------------------------------------------------
;; semantic union co-routine.
;;----------------------------------------------------------------------

(defun parser-union-feedback ( signal )
  (when signal
    (parser-retry-after
      (if (eq signal 'collsion)
        (parser-instruction 'compile)
        signal))))

(defun parser-union-closure ( pf-closure )
  "parser-union-closure SEMANTICS

   Merge semantic primitives from list TAPE into SEMANTICS.

   Each element of TAPE is a symbol (primitive) or list
   pair (instruction). The primitives are merged by the union to
   greedy pack a closure with primitives; the effect is to
   constant fold the emitted code to a great degree rendering the
   compiled code aesthetically pleasing, and provably minimal.

   When a primitive merge produces a collision or invalid Parser
   Function semantics a retry-after-compile is fed back into the
   instruction stream.
   "

  ;; FIXME: propogate the hooks through the domains. see if there are any other
  ;;        useful hook points.

  (save-lexical-closure pf-closure
    (lambda ( primitive )
      (parser-union-feedback
        (lexical-let
          ((merge
             (cond
               ((not (symbolp primitive)) 'unknown)

               ;;----------------------------------------------------------------------
               ;; call phase
               ;;----------------------------------------------------------------------

               ((eqn primitive 'relation) (parser-pf-set-primitive 'pf-relation primitive))
               ((eqn primitive 'closure)  (parser-pf-set-primitive 'pf-closure primitive))

               ;;----------------------------------------------------------------------
               ;; Effects phase
               ;;----------------------------------------------------------------------

               ;; -> logic effects

               ((eqn primitive 'eval-operator) (parser-pf-set-primitive 'pf-logic-domain primitive))

               ;; -> Input Domain

               ;; can merge with or without branching, only one input effect per function.

               ((eqn primitive 'input-branch)
                 (parser-merge-domain-exclusive 'pf-input-domain 'pf-input-branch primitive))

               ((eqn primitive 'input-discard)
                 (parser-merge-domain 'pf-input-domain))

               ;; -> AST Domain

               ((eqn primitive 'ast-branch)
                 (parser-merge-domain-combine 'pf-ast-domain 'pf-ast-branch primitive))

               ((eqn primitive 'ast-discard)
                 (parser-merge-domain-exclusive-except 'ast-branch
                   'pf-ast-domain 'pf-ast-discard primitive))

               ((eqn primitive 'ast-node)
                 (parser-merge-domain-combine-except 'pf-ast-discard
                   'pf-ast-domain 'pf-ast-node primitive))

               ((eqn primitive 'ast-transform)
                 (parser-merge-domain-combine-except 'pf-ast-discard
                   'pf-ast-domain 'pf-ast-transform data))

               ;; -> rvalue phase

               ;; TODO: an entry-point value for pf-rvalue-phase should
               ;;       exclude setting pf-rvalue-logic

               ((eqn primitive 'return)          (parser-pf-set-primitive 'pf-rvalue-phase primitive))

               ((eqn primitive 'return-operator) (parser-pf-set-primitive 'pf-rvalue-logic primitive))

               ;; when we don't know what it is.
               (t 'unknown) )))

          ;; make sure that a modification of the semantic closure
          ;; closes the call phase.
          (when (and (null merge) (null pf-call-phase))
            (setq pf-call-phase t))
          merge))) ))

;;----------------------------------------------------------------------
;; parser-function-generate
;;----------------------------------------------------------------------

;; The Parser Function Generator generates Parser Functions from a
;; Semantic Closure.

(defun parser-prune-lambda ( &rest generated )
  "parser-prune-lambda inserts a sequence of fragments into a lambda form pruning
   nils."
  `(lambda ()
     ,@(apply 'append (list-filter-nil generated))))

;;----------------------------------------------------------------------
;; match phase
;;----------------------------------------------------------------------

(defun parser-emit-match-call ()
  "The Match Call of a Parser Function includes the closure, predicate,
   and terms. Evaluation produces the logical Match Result of the
   function after parser-consume-match strips any AST.

   pf-terms is reversed allowing the list to be constructed as a stack.
  "
  `(parser-consume-match
     ,(lexical-let
       ((predicate nil))

       (if pf-terms
         (progn
           (setq predicate `(apply ',pf-relation '(,@(reverse pf-terms))) )
           (if pf-closure
             `(funcall ',pf-closure ,(parser-prune-lambda (list predicate)))
             predicate))

         (if pf-closure
           `(funcall ',pf-closure ',pf-relation)
           `(funcall ',pf-relation) )))))

(defun parser-prune-recursion-environment ( match-call )
  "parser-prune-recursion-environment MATCH-CALL

   The Match Call (parser-emit-match-call) is interpolated into a
   dynamic scope with the bindings of pf-dynamic-scope from the
   semantics table.
  "
  (if pf-dynamic-scope
    `(let
       ,pf-dynamic-scope
       ,match-call)
  match-call))

(defun parser-prune-match-phase ( match-call )
  "parser-prune-match-phase

   The match phase traps any parser-match-fail non-local exists
   from the recursion of the match call. This trap is only
   generated when there is conditional evaluation.
  "
  (lexical-let
    ((call-env (parser-prune-recursion-environment match-call)))

    (if pf-trap
      `(catch 'parser-match-fail
         ,call-env)
      call-env)))

;;----------------------------------------------------------------------
;; Input Domain
;;----------------------------------------------------------------------

(defun parser-emit-input-domain ()
  "Create the input effects of the Parser Function."
  (catch 'done
    (unless pf-input-domain (throw 'done nil))

    (push `(parser-push) pf-eval-setup)

    (if pf-input-branch
      (progn
        (push `(parser-pop) pf-match-effects)
        (push `(parser-backtrack) pf-fail-effects))
      (push `(parser-pop) pf-eval-effects)) ))

;;----------------------------------------------------------------------
;; AST Domain
;;----------------------------------------------------------------------

(defun parser-emit-node-transform ()
  "Create the call to an AST transform function that preserves the
   identity of the AST transformed.
  "
  (if pf-ast-transform
    `(cons (car ast-root) (cdr (funcall ',pf-ast-transform ast-root)))
    'ast-root))

(defun parser-emit-ast-domain ()
  "Generate any AST effects for the parser function.

   The most basic AST effect is creating a AST node or list which
   consists of a lexically scoped head and a dynamically scoped
   tail.

   Any AST effect will separate the logical and AST parts during
   evaluation. This is indicated by a non-nil pf-ast-value.

   pf-ast-value is nil or value interpreted as a code fragment or
   signal.
   "
  (catch 'done
    (unless pf-ast-domain (throw 'done nil))

    ;; Initialize the head of the AST in the lexical scope.
    (push `(ast-root ,(if pf-ast-node
                        `(cons ',pf-ast-node nil)
                        `(parser-make-ast))) pf-lexical-scope)

    ;; Initialize the tail pointer in the dynamic scope.
    (push `(parse-tree ast-root) pf-dynamic-scope)

    (if pf-ast-discard
      (setq pf-ast-value 'discard)

      (progn
        ;; a conditional selects the highest combinational complexity
        ;; as the entry point for generating the code.

        (cond
          (pf-ast-node
            (progn
              (push `(put (car ast-root) 'parser-ast 'production) pf-eval-effects)

              (setq pf-ast-value
                (if pf-ast-branch
                  (unless (eq pf-rvalue-phase 'entry-point)
                    (progn
                      (push `(parser-ast-add-node ,(parser-emit-node-transform)) pf-match-effects)
                      'discard))
                  `(progn
                     (parser-ast-add-node ,(parser-emit-node-transform))
                     nil))) ))

          (pf-ast-transform
            (if pf-ast-branch
              (push
                `(setq ast-root (funcall ',pf-ast-transform ast-root))
                pf-match-effects)
              (setq pf-ast-value `(funcall ',pf-ast-transform ast-root)))) )

        (when pf-ast-branch
          ;; deferred: does this generate dead code in any cases ?
          (push `(setq ast-root nil) pf-fail-effects))

        (unless pf-ast-value
          (setq pf-ast-value 'ast-root))
        ))
    ))

;;----------------------------------------------------------------------
;; Eval Phase
;;----------------------------------------------------------------------

(defun parser-emit-value-with-effects ( effects rvalue )
  "parser-emit-value-with-effects EFFECTS RVALUE

   Create a return value with an optional list of side-effects.

   the second parameter RVALUE is always returned. EFFECTS
   can be a list or nil."
  (if effects
    `(progn
       ,@effects
       ,rvalue)
    (if rvalue
      `,rvalue)))

(defun parser-return-discards-eval-p ()
  (and
    (or
      pf-branch
      pf-eval-logic)
    (eq 'match pf-rvalue-phase)))

(defun parser-save-result-p ()
  (or
    ;; eval-effects will always precede evaluation.  since the
    ;; eval-effects evaluation cannot preserve the r-value of the
    ;; match-call, binding is required.
    pf-eval-effects

    ;; when we lose the match call logical result in evaluation, but
    ;; require it later to compute the r-value binding is required.
    (parser-return-discards-eval-p) ))

(defun parser-prune-eval-phase ( match-phase )
  "Generate the evaluation phase stub."
  ;; the match result by this phase is always logic only as any
  ;; ast has already been consumed.

  (when (parser-save-result-p)
    (push `(match ,match-phase) pf-lexical-scope)
    (setq match-phase 'match))

  (cond
    ;; the r-value of a branch match is either a entry-point special
    ;; case of a complete match result, or a logical true value.
    ((eq pf-rvalue-phase 'entry-point)
      (progn
        (setq pf-match-rvalue `(cons (parser-pos) ,pf-ast-value))

        (when (null pf-eval-logic)
          (setq pf-eval-logic 'parser-match-p)) ))

      (pf-branch
        (progn
          (setq pf-match-rvalue 't)

          (when (null pf-eval-logic)
            (setq pf-eval-logic 'parser-match-p))

          (setq pf-eval-value 'logical) ))

      (pf-eval-logic (setq pf-eval-value 'logical)))

  (when pf-eval-logic
    (setq match-phase `(,pf-eval-logic ,match-phase)))

  (if pf-branch
    `(if ,match-phase
       ,@(seq-filter-nil
           (parser-emit-value-with-effects pf-match-effects pf-match-rvalue)
           (parser-emit-value-with-effects pf-fail-effects  pf-fail-rvalue)))
    match-phase))

;;----------------------------------------------------------------------
;; Result Phase
;;----------------------------------------------------------------------

(defun parser-prune-result-operator ( generated )
  (if pf-rvalue-logic
    `(,pf-rvalue-logic ,generated)
    generated))

(defun parser-eval-did-split-result-p ()
  (when (and
          (not (eq 'entry-point pf-rvalue-phase))

          (or
            pf-ast-value
            (eq 'logical pf-eval-value))) t))

(defun parser-prune-result-phase ( eval-phase )
;; FIXME: do the documentation work as a backtrack from this
;;        stage analysis wise.
  (lexical-let
    ((r-value (parser-prune-result-operator

                (if (parser-eval-did-split-result-p)
                  `(cons
                     ,(cond
                        ((parser-return-discards-eval-p)
                          `(progn
                             ,eval-phase
                             (parser-match-p match)))

                        ((eq 'match pf-eval-value)
                          `(parser-match-p ,eval-phase))

                        ((eq 'logical pf-eval-value)
                          eval-phase))

                     ,(if (or
                            (null pf-ast-value)
                            (eq 'discard pf-ast-value))
                        'nil
                        pf-ast-value))

                  eval-phase) )))

    (if pf-lexical-scope
      `(lexical-let*
         ;; the reverse is required so that the lexical bindings
         ;; of the logic phase will be ordered last.
         ,(reverse pf-lexical-scope)

         ,@(if pf-eval-effects
             (append pf-eval-effects (list r-value))
             (list r-value)))
      eval-phase)))

(defun parser-pf-emit ( pf-closure )
"parser-pf-emit CLOSURE

The goal of this generator design is to produce elisp that
appears more human, where human is opportunistic pruning of
redundant code.

To do this a set of valid primitives in the form of a closure is
taken as an input. It is assumed that this set is the largest
semantically valid set of primitives computed by
parser-semantic-union.

The minimal structure is a ordered sequence of a call phase
evaluation phase, and function phase. The requirements of
binding and special forms expand the minimal structure.

Each of the domains adjusts it's filling of the code fragments
based upon the structure required.
"
  (use-dynamic-closure-with
    (parser-function-semantics pf-closure)

    ((pf-trap           nil)
     (pf-branch         nil)

      ;; -> Binding structures.

      (pf-lexical-scope  nil)
      ;; list of bindings required by evaluation. These are largely due
      ;; to side? effect ordering.

      (pf-dynamic-scope  nil)
      ;; list of bindings for a recursion environment, currently used to
      ;; scope the AST tail.

      ;; -> code fragments

      (pf-eval-value     'match)

      (pf-ast-value      nil)
      ;; if the ast forces a split this is non-nil.

      (pf-eval-setup     nil)
      (pf-eval-effects   nil)

      ;; Functions that modify the parser/AST state need to run a setup before
      ;; the evaluation phase, and place their finish effects in either the
      ;; gen-eval-always fragments, or the branch fragments.

      ;; branching fragments.

      (pf-match-effects  nil)
      (pf-match-rvalue   nil)

      (pf-fail-effects   nil)
      (pf-fail-rvalue    nil))

    ;; FIXME: this optimization is still hosed.

    (cond
      ((= 1 (length pf-terms))
        ;; simple optimization, when a sequence has only a single call
        ;; make it the predicate.
        (progn
          (setq pf-relation (car pf-terms))
          (setq pf-terms nil)))

      ((and (> (length pf-terms) 1) (null pf-relation))
        ;; the default relational operator is and. to get a
        ;; specific relational operator make sure it's the
        ;; first instruction after the call phase.
        (setq pf-relation 'parser-relation-and)) )

    ;; any time we have a closure, or sequence, we will always need to
    ;; setup an AST recursion environment, even if there are no AST
    ;; effects. Term Relation Operators depend on the AST scoping to
    ;; construct a AST sequence of any sort.

    (when (or pf-closure pf-terms)
      (setq pf-ast-domain t))

    ;; effects cannot be generated until it's known if we are
    ;; branching.
    (when (or pf-ast-branch pf-input-branch)
      (setq pf-branch t))

    ;; whenever there is conditional evaluation trap match-fail.
    (when pf-branch
      (setq pf-trap t))

    ;; generate code fragments for the domains now that the function
    ;; structure is fully known.

    (parser-emit-ast-domain)
    (parser-emit-input-domain)

    ;; interpolate the fragments into the structure with "pruning".
    (parser-prune-lambda
      pf-eval-setup

      (list
        (parser-prune-result-phase
          (parser-prune-eval-phase
            (parser-prune-match-phase
              (parser-emit-match-call))))) )))

;;----------------------------------------------------------------------
;; compile co-routine
;;----------------------------------------------------------------------

(defvar parser-pf-table-tuning 13
  "initial size of the parser-pf-table objarray for storing match functions. the value
   was chosen based on the recommendation of prime numbers for good hashing.

   this value is a WAG. I still do not know how to compute a good value.")

(defun parser-make-pf-table ()
  "create a Match Function symbol table which is an objarray"
  (make-vector parser-pf-table-tuning 0))

(defun parser-pf-link ( identifier &optional definition )
  "Retrieve or define a Parser Function. the name of the production is required,
   as well."
  (lexical-let
    ((identity (symbol-name identifier)))

    (if (and
          (functionp (intern identity parser-pf-table))
          definition)

      (signal 'parser-semantic-error (format "illegal redefinition of Match Function %s" identity))

      (when definition
        (fset (intern identity parser-pf-table) (eval definition))))

    (intern identity parser-pf-table)))

(defun parser-unique-mf-symbol ( basename )
  (lexical-let
    ((unique-mf (make-symbol (concat basename (number-to-string parser-unique-id)))))
    (incf parser-unique-id)
    unique-mf))

(defun parser-compile-terminate ( pf-closure )
  (lexical-let
    ((entry-point (parser-pf-single-call-p pf-closure)))

    (unless entry-point
      (setq entry-point (parser-pf-emit pf-closure))
      (parser-compile-trace
        "parser-compile-terminate: emit function."
        (pp-closure
         pp-to-string)
        pf-closure
        entry-point))

    entry-point))

(defun parser-compile-closure ( semantics )
  (lexical-let
    ((closure semantics))

    (lambda ( instruction data )
      (cond
        ((eq instruction 'compile)
          (parser-continue-after
            'new-closure
            (if data
              (progn
                (parser-pf-link data (parser-compile-terminate closure))
                nil)
              (parser-instruction 'call (parser-compile-terminate closure)) )))

        ((eq instruction 'call)
          (when (parser-pf-call
                  closure
                  (if (symbolp data)
                    (parser-pf-link data)
                    (parser-pf-link (parser-unique-mf-symbol "anonymous-term")  data)))
            (parser-retry-after 'compile)))

        ((parser-retry-after 'unknown)) ))))

;;----------------------------------------------------------------------
;; compiler core
;;----------------------------------------------------------------------

;; The merging rules of the semantic union, the recovery mechanism of
;; emitting and nesting, and the feedback mechanism that couple them
;; as co-routines encompass the mechanism that reduces the grammar
;; classes to a uniform function design.

(defun parser-with-instruction ( f i )
  "parser-with-instruction F I

   apply function F to the instruction I extracting
   the instruction and data from I into the two
   arguments of F.

   The value of F is returned.
  "
  (lexical-let
    ((instruction i)
     (data        nil))

    (when (consp i)
      (setq instruction (car i))
      (setq data (cdr i)))

    (funcall f instruction data)))

(defun parser-feedback ( result present future )
  (if (null result)
    future
    (lexical-let
      ((splice-method (car result)))

      (throw 'parser-compile-yield
        (cond
          ((eq splice-method 'retry)
            (append (cdr result) (cons present future)))

          ((eq splice-method 'continue)
            (append (cdr result) future))
          ))) ))

(defun parser-co-compile ( closure )
  "parser-co-compile CLOSURE"
  (lexical-let
    ((continuation (parser-compile-closure closure)))

    (lambda ( present future )
      (parser-feedback (parser-with-instruction continuation present) present future))))

(defun parser-co-union ( closure )
  (lexical-let
    ((continuation (parser-union-closure closure)))

    (lambda ( present future )
      (parser-feedback (funcall continuation present) present future))))

(defun swap-cons ( x )
  (cons (cdr x) (car x)))

(defun parser-compile-run ( semantics instructions )
  "parser-compile-run SEMANTICS INSTRUCTIONS

   parser-compile-run consumes instructions using a co-routine
   like design to split the job of interpreting instructions
   into a compile and union part.

   The compile part performs linking and emitting, while the
   union part greedy packs a Parser Function with semantics.

   Both of the co-routines communicate through the instruction
   list with feedback. The core itself performs the switching
   between the co-routines, and manages the closures.
  "
  (lexical-let*
    ((closure    (if semantics
                   semantics
                   (parser-pf-new)))

     (union      (parser-co-union closure))
     (compile    (parser-co-compile closure))

     (co-pair    (cons 'compile 'union))
     (fail-count 0))

    (while
      (setq instructions
        (lexical-let
          ((yield (catch 'parser-compile-yield
                    (consume-list instructions
                      (if (lexical-let
                            ((current (car co-pair)))

                            (parser-compile-trace
                              (concat "parser-compile-run: applying " (symbol-name current))
                              (parser-pp-tape)
                              instructions)

                            (eq 'union current))
                        union
                        compile) )) ))

          (when yield
            (parser-with-instruction
              (lambda ( i data )
                (cond
                  ((eq i 'new-closure)
                    (progn
                      (parser-compile-message "parser-compile-run" "Creating new closure.")

                      (setq closure   (parser-pf-new))
                      (setq union     (parser-co-union closure))
                      (setq compile   (parser-co-compile closure))

                      (setq fail-count 0)
                      (cdr yield)) )

                  ((eq i 'unknown)
                    (progn
                      (incf fail-count)

                      (when (> fail-count 2)
                        (signal 'parser-syntactic-error
                          (parser-diagnostic instructions
                            "parser-compile-run"
                            "a recognizable primitive or instruction")))

                      (setq co-pair (swap-cons co-pair))
                      (cdr yield)))

                  ((eq i 'yield)
                    (progn
                      (setq fail-count 0)
                      (setq co-pair (swap-cons co-pair))
                      (cdr yield)))

                  ((progn
                     (setq fail-count 0)
                     yield)) ))
              (car yield)) ))))
    closure))

;;----------------------------------------------------------------------
;; Tokens
;;----------------------------------------------------------------------

;; The token part of the grammar definition contains a great deal of
;; flexibility or construction options for tokens. The second argument
;; is examined by type.

(defun parser-token-capture ( captures )
  "parser-token-capture CAPTURES
   Stub.
  "
  (if (and (listp captures) (> (length captures) 1))
    (lexical-let
      ((cap-list nil))
      (mapc
        (lambda ( cap )
          (push (cons (match-beginning cap) (match-end cap)) cap-list))
        captures)
      (reverse cap-list))
    (cons (match-beginning captures) (match-end captures))) )

(defun parser-token-api ( selection action )
  "parser-token-api SELECTION ACTION

   A token in the grammar has the form: /token id regex SELECTION ACTION.

   this function receives SELECTION ACTION only either of which may be
   nil. With these two forms the token API is compiled.

   SELECTION is examined first as ACTION is only valid in conjunction
   with a subset of the valid values for SELECTION.

   term: Capture.

   A capture is a cons cell of (beginning . end) that defines the bounds
   of the match.

   term: Capture List.

   A Capture List is a list of capture cons cells.

   SELECTION:

   nil           : disable ACTION ; \"0\" or entire match Capture returned.
   number        : capture number transformed into a Capture cons cell.
   list          : list of REGEX capture numbers transformed into a Capture List.
   function      : disable ACTION: \"0\" or entire capture is passed to function as an argument.
   symbol        : disable ACTION: return the symbol quoted.

   ACTION: optional!

   must be a symbol with a function bound. The arguments are formed according to
   SELECTION and the return value becomes the AST of the token.
  "
  (lexical-let*
    ((disable-action nil)
     (gen-select
       (cond
         ((eq nil    selection) (progn
                                  (setq disable-action t)
                                  `(parser-token-capture 0)))
         ((listp     selection) `(parser-token-capture ',selection))
         ((numberp   selection) `(parser-token-capture ,selection))
         ((functionp selection) (progn
                                  (setq disable-action t)
                                  `(funcall ',selection (parser-token-capture 0))))
         ((symbolp constructor) (progn
                                  (setq disable-action t)
                                  `(quote ',constructor)))

         ;; all other constructor types are un-handled.
         ((signal 'parser-syntactic-error
            (parser-diagnostic selection
              "parser-token-api"
              "lambda|function|number|symbol"))) )))

    (if (and (not disable-action) action)
      `(funcall ',@action ,gen-select)
      gen-select)))

(defun parser-token-function ( id &rest syntax )
  "Generate a token Match Function lambda."
  (lexical-let
    ((generated
       `(lambda ()
          (when (looking-at ,(car syntax))
            (parser-result-token
              ,(let
                 ((selection (if (listp syntax) (cadr syntax) syntax)))

                 (if (and (symbolp selection) (eq 'null selection))
                   'nil
                   `(cons
                      ',id
                      ,(parser-token-api selection (when (listp syntax) (cddr syntax)))) )) )))))
    (parser-compile-trace
      "parser-token-function: emit token."
      (pp-to-string)
      generated)
    generated))

;;----------------------------------------------------------------------
;; Parser Term Relations and Match Closures.
;;----------------------------------------------------------------------

;; Parser Term Relations are the lowest level primitives. Delayed
;; evaluation allows them to implement sequence logic.

(defun parser-relation-or ( &rest match-list )
  "Combine Match Functions by or ; the first successful match is returned.
   nil is returned if no matches are found."
  (catch 'match
    (dolist (match-func match-list)
      (parser-trace-on match-func
         (catch 'parser-match-fail
           (lexical-let
             ((match-result (funcall match-func)))

             (parser-trace-match match-func match-result)

             (when match-result
               (throw 'match match-result))) )))
    nil))

(defun parser-relation-and ( &rest match-list )
  (dolist (match-func match-list (parser-result-match))
    (parser-trace-on match-func
      (lexical-let
        ((match-result (funcall match-func)))
        (parser-trace-match match-func match-result)

        (unless (parser-match-p match-result)
          (throw 'parser-match-fail nil))

        (parser-consume-match match-result))) ))

(defun parser-closure-greedy ( match-func )
  "A positive closure predicate that applies the given
   Match Function as many times as it Matches and returns true
   if one or more matches was made."
  (lexical-let ((matched-once nil))

    (catch 'parser-match-fail
      (do ((production (funcall match-func) (funcall match-func)))
          ((progn
             (parser-consume-match production)
             (setq matched-once t)
             nil)) ))
    (if matched-once (parser-result-match)) ))

;;----------------------------------------------------------------------
;; syntax
;;----------------------------------------------------------------------

;; The parser hacking resides in the syntax where instruction sequences
;; are presented through an interface.

(define-hash-table-test 'string-hash 'string-equal 'sxhash)

(defun parser-create-syntax-table ()
  (lexical-let
    ((syntax (make-hash-table :test 'string-hash)))

    ;; call phase

    (puthash "relation"
      (lambda ( term-relation )
        (parser-strong-primitive 'relation term-relation)) syntax)

    (puthash "or"
      (parser-strong-primitive 'relation 'parser-relation-or) syntax)

    (puthash "and"
      (list
        (parser-weak-primitive 'ast-branch)
        (parser-weak-primitive 'input-branch)
        (parser-strong-primitive 'relation 'parser-relation-and)) syntax)

    (puthash "closure"
      (lambda ( closure )
        (parser-strong-primitive 'closure closure)) syntax)

    (puthash "greedy"
      (parser-strong-primitive 'closure 'parser-closure-greedy) syntax)

    ;; The not operator is how PEG grammars bound greed as they do not
    ;; do look-ahead.

    (puthash "not"
      (list
        (parser-strong-primitive 'ast-discard)
        (parser-strong-primitive 'input-discard)
        (parser-strong-primitive 'return-operator 'parser-negate-match)) syntax)

    ;; Like a regular PEG not operator except the input position is skipped
    ;; over the match.
    (puthash "not-skip"
      (list
        (parser-strong-primitive 'ast-discard)
        (parser-strong-primitive 'return-operator 'parser-negate-match)) syntax)

    ;; eval phase

    (puthash "negate-eval"
      (parser-strong-primitive 'eval-operator 'parser-negate-match) syntax)

    (puthash "production"
      (lambda ( name )
        ;; a node named and called NAME.
        (list
          (parser-instruction 'call name)
          (parser-instruction 'compile name)
          (parser-strong-primitive 'ast-node name))) syntax)

    (puthash "alias"
      (lambda ( alias name )
        ;; a term that creates a node NAME, called ALIAS.
        (list
          (parser-instruction 'call alias)
          (parser-instruction 'compile alias)
          (parser-strong-primitive 'ast-node name))) syntax)

    (puthash "term"
      (lambda ( name )
        ;; a term that does not create nodes.
        (list
          (parser-instruction 'call name)
          (parser-instruction 'compile name) )) syntax)

    (puthash "entry-point"
      ;; entry-point is unique in that the generator is tied into
      ;; the syntax to generate valid code. The entry-point is an
      ;; extreme corner case that makes a mess out of the already
      ;; hairy enough generator.

      (list
        (parser-instruction 'compile 'start)
        (parser-strong-primitive 'ast-node 'start)

        (parser-weak-primitive 'ast-branch)
        (parser-weak-primitive 'input-branch)
        (parser-weak-primitive 'relation 'parser-relation-or)

        (parser-strong-primitive 'return 'entry-point)) syntax)

    (puthash "always-match"
      (parser-strong-primitive 'return-operator 'parser-result-match) syntax)

    (puthash "token"
      (lambda ( id &rest token-syntax )
        (parser-pf-link id (apply 'parser-token-function id token-syntax))
        (parser-instruction 'call id)) syntax)

    (puthash "transform"
      (lambda ( func )
        (parser-strong-primitive 'ast-transform func)) syntax)

    syntax))

(defun parser-pp-defined-syntax ()
  (lexical-let
    ((name-list nil))

    (maphash
      (lambda ( name v )
        (push name name-list))
      parser-syntax)

    (apply 'concat (mapcar (lambda (x) (concat "|" x)) (cdr name-list))) ))

(defun parser-escaped-primitive ( symbol )
  "
  parser-escaped-primitive SYMBOL

  return the string of an escaped primitive if the symbol starts
  with a / character.
  "
  (lexical-let
    ((name (symbol-name symbol)))
    (if (char-equal ?/ (aref name 0))
      (substring name 1))))

(defun parser-apply-primitive-syntax ( primitive iterator next )
  "parser-apply-primitive-syntax PRIMITIVE ITERATOR NEXT

   translates PRIMITIVE passing the resulting instruction list to
   the iterator.

   Expansion may consume one or all of the remaining atoms
   from form given as the list NEXT. The tail of NEXT with
   the consumption of expansion's N arity removed is returned.
   "

  ;; FIXME: proper arity with argument checking instead of a blind
  ;; apply of MAX.

  (lexical-let
    ((expand (gethash primitive parser-syntax)))

    (unless expand
      (signal 'parser-syntactic-error (parser-diagnostic primitive
                                        "parser-apply-primitive-syntax"
                                        (format "a primitive / escaped symbol - one of: %s"
                                          (parser-pp-defined-syntax)))))
    (if (functionp expand)
      (lexical-let
        ((arity (cdr (function-arity expand))))

        (if (eq 'many arity)
          (progn
            (funcall iterator (apply expand next))
            nil)

          (lexical-let
            ((split (split-list arity next)))
            (funcall iterator (apply expand (car split)))
            (cdr split))))

        (progn
          (funcall iterator expand)
          next)) ))

(defun parser-expand-primitive ( primitive &rest args )
  (let
    ((translation nil))

    (parser-apply-primitive-syntax
      primitive
      (tail-iterator 'translation)
      args)

    (reverse (tail-list translation)) ))

(defun parser-nest-descent ( closure descent )
  (if descent
    (parser-compile-run closure
      (list (parser-instruction 'call (parser-compile-terminate descent))))
    closure))

(defun parser-nest-term ( closure term term-semantics )
  (parser-compile-run closure
    (list
      (parser-instruction 'call
        (if term-semantics
          (parser-compile-terminate
            (parser-compile-run nil
              (cons (parser-instruction 'call term) (reverse term-semantics))))
          term)))))

(defun parser-translate-form ( form )
  "parser-translate-form FORM

   Recursive compile of the parser definition syntax of a FORM
   The descent is depth first. After the calls or terms of the
   list have been resolved by descent the primitives are compiled
   by parser-compiler-run.

   primitives are symbols escaped by a leading / character, eg:
     /ast-branch
   and can take arguments that should not be escaped.

   Terms are either lists or Match Function identifiers. Any
   sequence of primitives right adjacent to a term will be merged
   with semantics of the term to the left, instead of the form.
  "
  (let ;; changing this to a lexical-let bugs tail-iterator. why ?
    ((closure        (parser-pf-new))

     (form-semantics nil)
     (form-iterator  nil)

     (call-to        nil)

     (call-semantics nil)
     (call-iterator  nil)

     (effect-only    (when (eq 'define (car form))
                       (setq form (cdr form))
                       t)))

    (setq form-iterator (tail-iterator 'form-semantics))

    (consume-list form
      (lambda ( current next )
;;;         (parser-compile-message "parser-translate-form" (format "c:n %s %s"
;;;                                                           (pp-to-string current)
;;;                                                           (pp-to-string next)))
        (if (listp current)
          (progn
            (when call-to
              (setq closure
                (parser-nest-term closure call-to (tail-list call-semantics)))
              (setq call-to nil))

            (parser-compile-message "parser-translate-form" "descend")
            (setq closure (parser-nest-descent closure (parser-translate-form current)))
            (parser-compile-message "parser-translate-form" "return")
            next)

          (lexical-let
            ((primitive (parser-escaped-primitive current)))

            (if primitive
              (parser-apply-primitive-syntax
                primitive
                (if call-to
                  call-iterator
                  form-iterator)
                next)

              (progn
                (when call-to
                  (setq closure
                    (parser-nest-term closure call-to (tail-list call-semantics))))

                (setq call-to current)
                (setq call-iterator (tail-iterator 'call-semantics))
                next)) )) ))

    (when call-to
      (setq closure
        (parser-nest-term closure call-to (tail-list call-semantics))))

    (lexical-let
      ((rvalue (if form-semantics
                 (parser-compile-run closure (reverse (tail-list form-semantics)))
                 closure)))

      ;; rvalue: even though it's only evaluated once, it must be evaluated forcing it
      ;; outside the if special form.

      (if effect-only
        (progn
          (parser-compile-message "parser-translate-form" "discarding effect-only descent.")
          nil)
        rvalue)) ))

(defun parser-compile-start ( grammar )
  "parser-compile-start GRAMMAR

   Start the compilation process with GRAMMAR producing either a
   un-evaluated entry-point lambda for the start production or
   nil.
  "
  (condition-case diagnostic
    (progn
      (parser-compile-run
        (parser-translate-form grammar)
        (parser-expand-primitive "entry-point"))

      `(lambda ( start-pos )
         (save-excursion
           (let
             ((parser-position (cons start-pos nil))) ;; initialize the backtrack stack
             (goto-char start-pos)

             (funcall ',(parser-pf-link 'start)) ))))

      (parser-syntactic-error
        (message "Syntactic Error: %s" (cdr diagnostic))
        nil)

      (parser-semantic-error
        (message "Semantic Error: %s" (cdr diagnostic))
        nil)

      (parser-compile-error
        (message "ICE: Internal Parser Compiler error %s" (cdr diagnostic))
        nil) ))

;;----------------------------------------------------------------------
;; utilities
;;----------------------------------------------------------------------

(defun parser-define ( fn parser )
  "parser-define SYMBOL PARSER

   Set SYMBOL as a compiled parser. Use like this:

   (parser-define 'foo (parser-compile grammar))

   You can then call the parser as a normal function
   symbol:

   (foo)

   The return value is either the symbol, or nil if the
   parser failed to compile.
  "
  (when parser
    (lexical-let
      ((parser-binding  (intern (symbol-name fn))))

      (fset parser-binding (eval parser))
      parser-binding)))

(defun parser-extract-string ( region )
  (filter-buffer-substring (car region) (cdr region) nil t))

(defun parser-token-string ( capture )
  "parser-token-string CAPTURE

   Return a string of the input bounded by CAPTURE - a cons
   cell or list.
  "
  (if (precise-list-p capture)
    (mapcar 'parser-extract-string capture)
    (parser-extract-string capture)))

(defun parser-compile-dump ( grammar )
  "Dump the code generation of the parser compiler given a quoted form."

  ;; The goal of dumping is to ensure that any user can create an
  ;; observation of the compiler's behavior that is complete enough
  ;; for analysis.

  ;; Compiler Version, User Grammar, emitted code, compile core
  ;; tracing, and light recursion tracing of form translation to
  ;; delineate the compilation of a form in the output.

  (let
    ((parser-compile-trace (get-buffer-create "parser-compile-dump"))
     (compiled nil))

    (with-current-buffer parser-compile-trace
      (erase-buffer))

    (parser-compile-trace-append
      (format "release: %s\ngrammar:\n%s\n" parser-release-version (pp-to-string grammar)))

    (parser-compile-trace
      "parser-compile-dump: dumping entry-point"
      (pp-to-string)
      (setq compiled (parser-compile-start grammar)))

    (pop-to-buffer parser-compile-trace t)
    compiled))

(defun parser-interactive ( parser )
  "run test-parser interactively for testing and debugging."
  (interactive "SParser? ")
  (lexical-let
    ((parse-result (funcall parser (point))))

    (message "PROD match? %s"
      (if parse-result
        (format "Yes matched to: %s, AST: %s" (car parse-result) (pp (cdr parse-result))
        "No"))) ))

(defmacro parser-trace-list ( list &rest productions )
  "create a parser trace list"
  `(setq ,list
    '(
       ,@(mapcar
           (lambda (trace)
             `(,(car trace) . ,(cadr trace))) productions) )))

(defun parser-trace (parser trace-list)
  "run test-parser interactively for testing and debugging."
  (interactive "SParser? 
STrace List? ")
  (let
    ((parser-trace-buffer (generate-new-buffer (format "parser-trace:%s" (symbol-name parser))))
     (parser-trace (eval trace-list)))

    (parser-interactive parser) ))

;;----------------------------------------------------------------------
;; macro interface.
;;----------------------------------------------------------------------

(defmacro parser-compile ( &rest grammar )
  "parser-compile &rest GRAMMAR

  parser-compile translates GRAMMAR into a Recursive Descent
  parser returning a lambda entry-point parsing the Start Symbol.

  GRAMMAR is a form interpreted as a DSL that defines the parser
  explained in [Syntax].

  -> Use

  (compiled-parser START) -> nil | (final position . AST)

  The compiled Parser Function is called with the starting
  position in the current buffer as a required argument.

  The return value is a production of the start symbol as a cons
  cell of the parser's final position and the AST generated, or
  nil if no match is found.

  The AST structure produced by the parser is documented under
  [AST Effects].

  -> Getting Started

  The easiest way to experiment with the Parser Compiler is to
  bind the parser to a symbol and use the provided function:

  parser-interactive

  to call the parser interactively.

  (parser-define 'test-parser
    (parser-compile
      (/token word \"[[:alpha:]]+\")
      (/token whitespace \"[[:blank:]]+\")))

  M-x parser-interactive

  <prompts for symbol: type test-parser>

  You then see the AST, or a message indicating that the match
  failed.

  Once you understand the AST structure, and have written your
  parser you need to embed it in your code.

  Since the parser-compile macro returns a function you can in-line
  it directly as the example below shows:

  (let
    ((parsed  (funcall (parser-compile ....) (point))))

    (if parsed
        ....
      ))

  Or you can define it externally which is better when it is
  used in more than one place:

  (define-parser 'parser-foo (parser-compile ....))

  (let
    ((parsed  (parser-foo (point))))

    (if parsed
        ....
      ))

  The parser function returns a *single* production of the start
  symbol per call. If you need to parse a file call the parser
  in a loop.

  -> Compiler tools

  The compiler has tools for tracing the execution of a parser
  and dumping the source of a parser.

  To dump the Parser Source specify dump as the first symbol of
  the grammar. The compile is traced in a separate buffer
  \"parser-compile-dump\" which is displayed when the compile is
  complete.

  FIXME: Tracing.

  -> Syntax

  Each form contains lists, Primitives, or Terms. Lists are
  translated into terms depth first. Terms are calls to Parser
  Functions defined elsewhere.

  Primitives are instructions to the Semantic Interpreter escaped
  from terms by a \"/\" character. Primitives can take parameters
  from the form that are not escaped.

  PRIMITIVE: /and
  TERM:      foo
  LIST:      (/token whitespace \"[[:blank:]]+\")

  Translation:

  When a form is translated all of the lists and Terms are
  resolved by descent and linking - producing the Call Phase of a
  Parser Function.

  The Parser Primitives [if any] are then translated into
  compiler instructions. The Compiler Core assembles the Call
  Phase and the instructions into a Parser Function.

  Primitive Associativity:

  An escaped primitive is applied to the Form Function or the
  Term. If the primitive is right of a term it is applied to the
  term immediately left. If there are no terms left of the
  primitive it is applied to the Form Function. see Example A.

  ->Example A

    form:    (/greedy /and foo /greedy bar)

    matches: foo foo foo bar foo bar

  In example A there are two terms foo and bar. The Term Relation
  is \"and\" which requires that all terms match once in order.

  The greedy closure /greedy matches the and of foo and bar one
  or more times. foo is also greedy with positive closure as
  well.

  Term relations must be the last form primitive, or left of the
  first term. Otherwise the default /and will be generated.

  A sequence of primitives can be arbitrarily long. The Compiler
  Core will generated nested functions as needed to preserve the
  semantics of a sequence.

  A primitive sequence is interpreted right to left with
  arbitrary primitive order Within the sequence delineated by the
  effects that can be combined in a Parser Function.

  The only form that cannot be compiled is primitives without
  terms, or in canonical parsing terminology a production with
  meta-operators must have at least one terminal/non-terminal to
  apply those meta-operators to.

  Primitive Expansion:

  The instructions of the Compiler Core are highly refined and
  multiple instructions are often needed to define useful Parser
  Functions.

  A Primitive Expansion expands primitives into sequences of
  instructions, taking any arguments needed from the form.

  Arguments must not be escaped, and should be quoted if their
  value is void. Arguments are eval'd so that compile time values
  can be used in the generated parser.

  /production 'foo

  takes a single argument ARG from form and expands to the sequence:

    call ARG
    link ARG
    compile
    ast-node ARG

  The arity of a primitive is either 0 for a list or symbol, and
  N for a function where N is zero or more as specified by the
  argument list of the function.

  All of the built-in primitives can be found in the
  function: parser-create-syntax-table.

  -> Parser Functions

  Parser Functions are the core abstraction of the generated
  parser. Each parser Function has up to three phases:

  * Call
  * Evaluation
  * Result.

  All parser functions return a Match Result cons cell.

  Call Phase:

  The Match Call phase creates a recursion environment if
  necessary, and executes the match calls to produce a Match
  Result.

  The recursion environment is a recursive scoping of internal
  variables - currently the AST. When a recursion environment is
  created the AST tree modified by the parser function is not
  connected to the caller's tree so that the isolated tree can
  easily be transformed and conditionally attached to the
  caller's tree.

  <Note>
    Since each recursion operates on it's own tree it might be
    possible to parallelize the non-deterministic matching. Only
    the token memoization would be shared.
  </Note>

  When there are many terms in a call phase a Term relation
  function is given the ordered list of terms by delayed
  evaluation.

  If there is a closure function such as greedy it will receive
  the term or term relation by delayed evaluation.

  Match result:

  The Match Result contains two parts. A logical match sense
  that is non-nil or nil and the AST produced by the match.

  The match sense is entirely independent of the AST, in other
  words you can return a match false, and AST that is merged if
  you wish.

  As a general principle of Parser Function Semantics the effects
  are orthogonal until you define relationships such as:

  /ast-branch

  which means that the AST effects are applied only when the
  match is positive. This is a relation between the logic
  and AST.

  Evaluation phase:

  If there is branching, AST effects, or input effects for a
  Parser Function an evaluation phase is constructed which
  implements those effects.

  The effects can be conditionalized with /[effect]-branch
  primitives. A logical operator such as negate can be
  applied to the branch evaluation of the Match Result
  when needed.

  Result Phase:

  The result phase constructs the Match Result returned by the
  Parse Function. By default the match sense of evaluation is
  returned.

  If the original Match Result of the Match Call must be returned
  use /return-match.

  A logical operator can be applied to the Result Phase
  independent of a logical operator for the evaluation phase if
  needed.

  Input effects:

  When parsing the parser can remember and restore the input
  position of the parser in a buffer. This is implemented as a
  stack.

  The position can be un-conditionally restored which is commonly
  referred to as look-ahead with:

    /input-discard

  The position can be conditionally restored when the match fails
  or back-track with:

    /input-branch

  AST Effects:

  The primary job of a parser is to construct a tree from pattern
  matching within a sequence of text. AST effects specify how the
  tree is constructed.

  By default AST lists or productions of the parser are merged
  into the parent node. This flattening is shown in Example B.

  Example B:

    form:     (and (and foo bar) (and baz bar))
    produces: (foo bar baz bar)

  Sub-trees are created with the Semantic Interpreter instruction
  ast-node which is accessed from the form with:

    /production NAME

  Which causes a named node to be created as the root of the AST
  tree scoped to the parse recursion. When the match call is
  completed this node is added instead of merged to the current
  node so it is not flattened. This is how you create AST trees.

  The production primitive also immediately compiles the form,
  and links the Parser Function as the definition of NAME,
  allowing the production to be called by NAME.

  AST trees consist entirely of nested production lists. It is
  essential that the trees be walkable Which means that every
  node must be labeled, and every tree [a node with children]
  must be distinguishable from leaves.

  All elements of a production list are cons cells with an
  identifier symbol [void value], and a data part. A tree
  has a 'parser-ast property with the value of 'production
  attached to the identifier symbol of the cons cell.

  This allows an AST walker to distinguish between the arbitrary
  data of a leaf, and a descent into a tree. In canonical parsing
  terms this is distinguishing between non-terminals and
  terminals.

    /transform FUNC

  Applies transform function FUNC to the AST. FUNC is a lambda
  that takes a AST tree as the sole argument and returns an AST
  tree or nil.

  The tree returned by FUNC must be either a proper AST tree as
  described above with an identifier and
  \":parser-ast 'production\" property.

  transform combines with both /production and /ast-branch.

    /ast-branch

  A conditional relation of the AST effects with the match
  sense of the Match Result. AST effects will be applied
  only if the match sense is positive.

  Otherwise AST effects are always applied.

  Note that eval phase logical operators can be used to
  transform the match sense.

   /ast-discard

  Unconditionally discard the AST produced by recursion. This
  primitive is exclusive of all other AST primitives.

    PF -> Logic Effects:

    /negate-function
    /optional

    /negate-match

    stub.
  "
  (let
    ((parser-syntax    (parser-create-syntax-table))
     (parser-pf-table  (parser-make-pf-table))
     (parser-unique-id 0))

    (if (eq 'dump (car grammar))
      (parser-compile-dump (cdr grammar))
      (parser-compile-start grammar)) ))

(provide 'parser)
;;; parser.el ends here
