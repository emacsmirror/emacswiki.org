Borked on emacs 26.1.  I changed "holiday-face" to just "holiday", but now I'm getting "cond: Symbol’s function definition is void: extract-calendar-month" and I've already spent enough time at work not doing work. :/

-- JohnL4 2019-04-03 17:32 UTC


----

`calendar+.el` is *very* old code. The file header states the compatibility explicitly as Emacs 20.x. If you're not using Emacs 20 then it probably won't work/help out of the box.

Sorry for your waste of time. (On the other hand, playing with Elisp is always a learning experience. ;-))

-- DrewAdams 2019-04-03 23:27 UTC

