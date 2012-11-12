I found that this package won't load in 24.1.1 with the following error:
eval-buffer: Too many arguments

I was able to trace down the problem to line 95 
(defvar intel-hex-mode-map ()
which should read
(defvar intel-hex-mode-map

Best regards
Claudio

-- Anonymous 2012-11-12 13:34 UTC

