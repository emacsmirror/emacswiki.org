Is there any way to turn off cleaning up whitespace at the beginning of buffers? It is really annoying in some cases such as editing git commit messages.

----

Not directly. You could remove the element `empty' from `whitespace-style' using a hook for the major-mode you care about using some elisp?

-- AlexSchroeder 2015-09-18 07:25 UTC

