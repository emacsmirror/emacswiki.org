When one of the swapping buffers is in a dedicated window, only the other buffer is changed. So, to avoid that behavior I did this improvement:

http://paste.lisp.org/display/142515

81a83,84
>       (if (window-dedicated-p other-win)
> 	  (error "The window above this one is dedicated"))
98a102,103
>       (if (window-dedicated-p other-win)
> 	  (error "The window under this one is dedicated"))
114a120,121
>       (if (window-dedicated-p other-win)
> 	  (error "The left split is dedicated"))
130a138,139
>       (if (window-dedicated-p other-win)
> 	  (error "The right split is dedicated"))

-- [http://hackingbits.com geyslan] 2014-05-14 03:12 UTC

