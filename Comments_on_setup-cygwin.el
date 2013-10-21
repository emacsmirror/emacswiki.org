minor warning on byte-compile:
<pre>
Compiling file c:/Program Files/gnu/emacs-24.3/site-lisp/setup-cygwin.el at Mon Oct 21 11:25:52 2013
setup-cygwin.el:82:1:Warning: defgroup for `setup-cygwin' fails to specify containing group
</pre>

Although the current help info for defgroup (24.3.1) doesn't specify this as a requirement.

<pre>
(defgroup SYMBOL MEMBERS DOC &rest ARGS)
</pre>

cygwin-mount.el defgroups <code>cygwin-mount</code> and places it in the <code>files</code> group.

setup-cygwin, however, seems more appropriately placed in the <code>Processes</code> group.
