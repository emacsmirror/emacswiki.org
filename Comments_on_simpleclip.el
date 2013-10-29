

in simpleclip-called-interactively-p you might consider using something like:

 (cond
  ((not (fboundp 'called-interactively-p))
   `(interactive-p))
  ((> 0 (cdr (subr-arity (symbol-function 'called-interactively-p))))
   `(called-interactively-p ,kind))
  (t
   `(called-interactively-p)
   )
  )

to compile it, you might want to do something like to suppress warnings, since we know what we're doing:

(when (listp byte-compile-warnings)
      (add-to-list 'byte-compile-warnings 'callargs))

-- whiskey-tango-foxtrot 2013-10-29 18:17 UTC

