I found that this package won't load in 24.1.1 with the following error:
eval-buffer: Too many arguments

I was able to trace down the problem to line 95 
(defvar intel-hex-mode-map ()
which should read
(defvar intel-hex-mode-map

Best regards
Claudio

-- Anonymous 2012-11-12 13:34 UTC


----

Thanks a lot. It does not work in 23.3.1 either. Your solution has solved that problem.

-- Steven Tuxfield 2012-12-20 03:55 UTC


----

Thanks!  This fixed it for me in xemacs as well.

-- Joseph Chiu 2012-12-29 02:32 UTC

