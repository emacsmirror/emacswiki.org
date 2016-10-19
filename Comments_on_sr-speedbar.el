I keep getting the warning when helm is not available:

    Warning (bytecomp): reference to free variable ‘helm-alive-p’

You can change the lines

             (if (featurep 'helm)
		 (not helm-alive-p)
	       t))

to

             (not (bound-and-true-p helm-alive-p)))

to fix the warning.

Can someone incorporate this fix in the code?

-- [https://www.topbug.net xuhdev] 2016-10-19 23:55 UTC

