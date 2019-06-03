There is a 404 error when you click on the link/button "[Dired+ Help on the Web]" in the dired+ mode (C-h m) help page.

-- [http://yassinphilip.bitbucket.io/ yPhil] 2017-08-18 16:28 UTC


----

[new:DrewAdams:2017-08-18 21:13 UTC]
@yPhil: Please try the latest - should be OK now. They changed all of the Emacs-Wiki URLS, and I haven't gotten around to changing them in all of my libraries.

-- DrewAdams 2017-08-18 21:13 UTC


----

https://emacs.stackexchange.com/questions/50815/managing-libraries-hosted-on-emacswiki/50819?noredirect=1#comment78274_50819

I think The `Symbol’s value as variable is void: dired-omit-files` error mentioned in the link above is because `dired+' is referencing `dired-omit-files' in an `;;;###autoload' statement (the default value for `diredp-omit-files-regexp').  There shouldn't be any need to autoload `defcustom`s, as far as I know.

-- npostavs 2019-06-03 20:36 UTC

