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


----

Drew,

first and foremost I meant all the issues that can be fixed easyly. Thanks for fixing the byte-compile-issue. Well for me it's the only file I don't get from melpa. Melpa makes updating easy and Emacs 25 even shows dependencies and they are resolved automatically and this file requires cygwin-mount as dependency. This file is usefull and I think there is a lot on melpa that is a lot less useful.

The problem with the broken shell is severe. It kills the everything goes into emacs approach. I don't get why this problem exists for years but no one with coding knowledge seems to care. Isn't cygwin maintained by Red-Hat? The workaround of using an old cygwin.dll doesn't seem to viable option. What are the side-effects regarding security, bugs, features and 64bit? Using the w32emacs from inside cywin isn't right for me either. This version has it's own disadvantages compared to native ntemacs.

-- Anonymous 2016-04-30 17:38 UTC


----

Anonymous: I don't know the answer to your question about the shell. I'm no expert on this. I'll think about adding it to MELPA.

-- DrewAdams 2016-05-01 04:53 UTC


----

Well, maybe one fixes the other. Melpa might bring attention to setup-cygwin and someone else will fix that part.

-- Anonymous 2016-05-01 10:05 UTC

