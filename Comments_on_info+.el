Could you please add autoload cookie for info-good-fixed-pitch-font-families?
Without it, folks are running into problems as detailed at 
https://github.com/syl20bnr/spacemacs/issues/15010

The issue is that defface forms that do have autoload cookie refer to info-good-fixed-pitch-font-families.  Thus within autoload file we end up with code that use info-good-fixed-pitch-font-families which is not defined.  Thanks.

-- emacs18 2021-08-25 00:07 UTC


----

Done - please try the latest. And thanks for your comment. I had no idea that Spacemacs, or some Spacemacs users, used `info+.el'.

That missing autoload cookie was an oversight.  It's needed because an autoloaded `defface` makes use of it.

-- DrewAdams 2021-08-25 02:03 UTC

