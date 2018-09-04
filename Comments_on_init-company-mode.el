{{{
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
}}}

I had to remove the <code>:with</code> to get it to work. Emacs 26.1.

-- Anonymous 2018-09-04 20:52 UTC

