Lisp:flame.el

From the commentary: 

This program was inspired from a version written in 1986 by Ian G. Batten <batten@uk.ac.bham.multics>, which had been derived from
an ancient yacc program.  Other than reusing the same sentence fragment
templates for the grammar below, this is a complete rewrite.

The comments from the original version read as follows:

        "Flame" program.  This has a chequered past.

        The original was on a Motorola 286 running Vanilla V.1,
        about 2 years ago.  It was couched in terms of a yacc (I think)
        script.  I pulled the data out of it and rewrote it as a piece
        of PL/1 on Multics.  Now I've moved it into an emacs-lisp
        form.  If the original author cares to contact me, I'd
        be very happy to credit you!

        Ian G. Batten, Batten@uk.ac.bham.multics

On 1994-01-09, I discovered that rms dropped this file from the Emacs 19
distribution; this was sometime before 19.7 was released.  He made no
ChangeLog entry and didn't keep the source file around (by convention,
we usually renamed files we wanted to keep but not go into official
distributions so that they started with `=', e.g. `=flame.el').  This is
all he had to say about it when I asked:

      I think I decided I was unhappy with the legal papers for it.
      Removing it took less time than trying to deal with it
      any other way.

At the time, I was unable to locate Ian Batten though he has sinced
resurfaced via the web.
