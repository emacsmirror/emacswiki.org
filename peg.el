Parsing Expression Grammars (PEG) are a formalism in the spirit of
Context Free Grammars (CFG) with some simplifications which makes
the implementation of PEGs as recursive descent parsers particularly
simple and easy to understand [Ford, Baker].
PEGs are more expressive than regexps and potentially easier to use.

The [https://elpa.gnu.org/packages/peg.html peg.el] library implements the macros `define-peg-rule', `with-peg-rules', and
`peg-parse' which parses the current buffer according to a PEG.
E.g. we can match integers with:

    (with-peg-rules
        ((number sign digit (* digit))
         (sign   (or "+" "-" ""))
         (digit  [0-9]))
      (peg-run (peg number)))

