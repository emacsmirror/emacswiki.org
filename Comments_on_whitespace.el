Is there any way to turn off cleaning up whitespace at the beginning of buffers? It is really annoying in some cases such as editing git commit messages.

----

Not directly. You could remove the element `empty' from `whitespace-style' using a hook for the Major Mode you care about using some elisp?
Or customize `whitespace-global-modes` and make sure Whitespace Mode is not used for the Major Modes you care about?

-- AlexSchroeder 2015-09-18 07:25 UTC

