vc-darcs.el is an emacs script that has been written by Jorgen Schaefer and Juliusz Chroboczek.

From an [http://marc.info/?l=darcs-users&m=123127194230029&w=2 email] to the darcs-users list:

New features:

  - works with all variants of Darcs repositories (pristine-less, hashed,
    2.0, etc.);
  - integrates more smoothly with Emacs (thanks to whoever added the new VC
    interfaces);
  - works through tramp (yay!).

Limitations:

  - requires a Darcs 2.* binary;
  - not tested with either Emacs 21 or XEmacs;
  - somewhat slower than the previous version, as it doesn't know about
    repository internals any longer.

More precisely, this version only makes the following assumptions about
the format of Darcs repositories:

  1. the root of a Darcs repository is marked by the presence of a directory
     called _darcs;
  2. no files under _darcs are ever registered, except for files under
     _darcs/prefs.

As long as these assumptions are satisfied, it will work with new variants
of Darcs repositories.



The latest code can be found [http://www.pps.jussieu.fr/~jch/software/repos/vc-darcs/ here].
