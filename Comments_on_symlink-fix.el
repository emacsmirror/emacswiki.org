I'm still using this with emacs 26.1, however the \N in one of the docstrings triggers "invalid-read-syntax". Replacing it with \<N> (as it's meant to be a metasyntactic variable for a number, I think) helps.

-- [https://github.com/TauPan TauPan] 2018-06-11 07:16 UTC


----

All the backslashes in that docstring needed to doubled, which I just did.

-- npostavs 2018-06-13 11:56 UTC

