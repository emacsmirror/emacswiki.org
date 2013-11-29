Great job with this .el!
I have a question.
Is it normal that this automatically happens?

{{{
for(i=0; i<n; i++)
    {
        blabla;
    }
}}}

Emacs autoindents the parenthesis, and again the ode inside the block.
Shouldn't it be like this?

{{{
for(i=0; i<n; i++)
{
    blabla;
}
}}}

How could I make this happen?
Thanks in advance.

-- Federico

----

See IndentingC -- you need to switch to a different style.
Example: ## (setq c-default-style "linux")##.

-- AlexSchroeder 2013-11-29 12:50 UTC

