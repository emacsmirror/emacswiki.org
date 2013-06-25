As to a recent version of Emacs Trunk 24.3.50 (9.0), error messages appear when loading this file:

    (lambda (field) ...) quoted with ' rather than with #'

The solution is to remove five (5) single quotations that precede '(lambda (field) . . .

-- lawlist 2013-06-25 15:40 UTC

