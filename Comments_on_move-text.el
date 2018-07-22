The special handling for Emacs 24.3 breaks move-text for me, though I am running Gnu Emacs 24.3.1. I also haven't found anything the describes a change in the behavior of transpose-lines in that version of Emacs. What am I missing here?

-- GregLucas 2013-04-30 16:58 UTC


----

Yeah, might just be HEAD. Let me investigate and adjust things accordingly.

-- StevePurcell 2013-05-06 18:13 UTC


----

Okay, give that a try.

-- StevePurcell 2013-05-06 18:26 UTC


----

On "GNU Emacs 24.2.1 (i386-mingw-nt5.1.2600)
 of 2012-08-27 on MARVIN" I had to comment out the following block to make the code work properly:

          ;; (when (and (eval-when-compile
          ;;              '(and (>= emacs-major-version 24)
          ;;                    (>= emacs-minor-version 3)))
          ;;            (< arg 0))
          ;;   (forward-line -1))

-- [http://emacs-sbcl-slime.blogspot.com alkhimov] 2013-07-24 12:38 UTC


----

Any reason not to just refer to the more recent https://github.com/emacsfodder/move-text
It's also what's on MELPA

-- Anonymous 2018-07-22 10:00 UTC


----

@Anonymous 2018-07-22: using Emacs 24.5 or older perhaps?

-- npostavs 2018-07-22 15:47 UTC

