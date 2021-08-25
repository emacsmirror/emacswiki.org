Could you please add autoload cookie for info-good-fixed-pitch-font-families?
Without it, folks are running into problems as detailed at 
https://github.com/syl20bnr/spacemacs/issues/15010

The issue is that defface forms that do have autoload cookie refer to info-good-fixed-pitch-font-families.  Thus within autoload file we end up with code that use info-good-fixed-pitch-font-families which is not defined.  Thanks.

-- emacs18 2021-08-25 00:07 UTC

