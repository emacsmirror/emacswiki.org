;;; token-example.el --- example for Parser Compiler introducing tokens.

;; Copyright (C) 2008 Mike Mattie

;; Author: Mike Mattie <codermattie@gmail.com>
;; Maintainer: Mike Mattie <codermattie@gmail.com>

;; Keywords: parser example
;; License: LGPL <http://www.gnu.org/licenses/lgpl.html>

;;; Code:

(require 'parser)

;; simple key value example to demonstrate tokens and basic
;; organization.

;; evaluate the form below to try out the parser. There is a small
;; sample input at the bottom. Use the parser-interactive command to
;; run the parser interactively giving it: simple-kv when prompted for
;; a parser.

(parser-define 'simple-kv
  ;; ^^ bind the compiled function to a symbol
  (parser-compile
    ;; ^^ emits a parser function that takes a single argument (starting position)

    (define
      ;; ^^ I want to declare some stuff without causing matching [ you can declare and match at the
      ;;    same time ; define can go anywhere any number of times except inside tokens which is nuts. ]

      (/token record-delim       "$" null) ;; our record separator is newline. the "null" means
                                           ;; don't put anything in AST for this token.

      (/token kv-pair  "\\([^[:blank:]]+\\):[[:space:]]+\\([^[:blank:][:cntrl:]]+\\)"
      ;;      ^^ identifier  ^^ first capture              ^^ second capture

        (1 2)       parser-token-string)
      ;; ^^ select  ^^ handler

    ;; when I walk through the AST I can find a kv-pair because it will produce a cons of
    ;; ( identifier . data ) where identifier is a symbol you can check with eq.

    ;; (if (eq (car token) 'kv-var) (message "hello world, I have data: %s" pp-to-string (cdr token)))

    ;; select:

    ;; select is where you choose how the AST is built. a simple entry
    ;; in the "select" slot is null which means don't put anything in
    ;; AST. I chose the word select because when you have both a
    ;; "select" and a "handler" the select chooses what capture
    ;; information is passed to the handler function.

    ;; In this case I want two captures, so I specify in the
    ;; list (1 2) [ a single capture can be a single digit ]

    ;; Capture numbers are standard Elisp REGEX enumeration.

    ;; The function parser-token-string (in parser.el) will expect
    ;; either a cons of (beginning . end) [a region], or a list of
    ;; such pairs. It extracts the text with filtering for properties
    ;; from the region.

    ;; Note: my function precise-list-p distinguishes between simple
    ;;       cons cells and lists.

    ;; handler:

    ;; once you have captures you usually want to extract the text
    ;; from the buffer, or run a special data builder. The handler
    ;; must always be a function that accepts a cons cell, or a list
    ;; of cons cells.

    ;; do a find-function lookup of parser-token-string as an example.
      )

    /and
    ;; ^^ and/or are conceptually just like the lisp and/or special forms. The start
    ;;    symbol has a special default of "or" so that simple tables of tokens are
    ;;    easy to make: this is perl'ish.

    ;;    my /and will replace the default or.

    ;;    The normal [not start symbol or top-level] default is /and which produces
    ;;    the sequence of matches you would expect.

    kv-pair record-delim
    ;; ^^ call my terms, by the Term Relation /and both must match.
    ))

;; okay so if I have the input like this:

foo: bar

;; I get this from parser interactive.

;; PROD match? Yes matched to: 24, AST: (start
;;   (kv-pair "foo" "bar"))

;; If I drop the /and at let it default to /or for the toplevel, then
;; newlines match, but have no AST.

;; PROD match? Yes matched to: 25, AST: (start)

;;; token-example.el ends here
