After pulling in latest from melpa, edit history is broken. I am not longer able to edit history properly as it is meddling with previous command.

-- Anonymous 2014-03-23 08:49 UTC


----

I just maintain multi-term.el on EmacsWiki, i'm sure melpa is older than EmacsWiki one.

-- AndyStewart 2014-03-23 13:49 UTC


----

I have the same problem. It seems to be caused by ("C-m" . term-send-input) in term-bind-key-alist - if you get rid of that, everything works fine (i.e. C-m and RET are bound to term-send-raw again).

Melpa pulls in updates automatically, I think, so it should always match the version on the wiki

-- Anonymous 2014-05-17 14:27 UTC

