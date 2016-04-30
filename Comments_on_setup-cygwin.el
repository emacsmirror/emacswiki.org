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

----

This seems to be broken with recent version of cygwin and GNU NT emacs.
You get below error when starting shell as cygwin from Win NT emacs

cygwin bash: cannot set terminal process group (-1): inappropriate ioctl for device

-- Anonymous 2015-12-07 16:19 UTC


----

Would like to see this file fixed and put on melpa.

-- Anonymous 2016-04-29 22:12 UTC


----

1. Anonymous0: I added a proper `defgroup'.

2. Anonymous1: Yes, see [[NTEmacsWithCygwin#bash-issue]].

3. Anonymous2: I don't know the fix to the issue Anonymous1 mentioned, if that's what you mean by fix it.  I could put it on [[MELPA]], but it's less a proper library than a set of initializations. Do you really think it's important to put it on MELPA?

-- DrewAdams 2016-04-30 04:26 UTC

