I get an error: Symbol's function definition is void: add-to-alist.  If I change:

  (add-to-alist 'auto-mode-alist elt-cons))

to:

  (add-to-list 'auto-mode-alist elt-cons))

the error goes away. Note the change from add-to-alist to -->  add-to-list, alist vs list.  GNU Emacs 24.3.1  CC Mode version 5.33

-- Anonymous 2017-03-10 18:12 UTC


----

add-to-alist define at: https://www.emacswiki.org/emacs/basic-toolkit.el

-- [http://www.emacswiki.org/emacs/AndyStewart AndyStewart] 2017-03-19 11:14 UTC

