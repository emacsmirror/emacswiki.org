;;; two-mode-mode.el -- switches between tcl and sgml(html) modes
;; $Id: two-mode-mode.el,v 1.2 1999/10/18 14:29:02 davidw Exp $ 
;; two-mode-mode.el is Copyright David Welton <davidw@efn.org> 1999
;; two-mode-mode.el is free software under the terms of the GNU GPL

;; As I'm sure is painfully obvious, I don't know much about elisp,
;; but I thought it would be cool to do this mode, for mod_dtcl.  So
;; if you have any comments or suggestions, please email me!

;; These same concepts could be used to do a number of neat 2-mode
;; modes, for things like PHP, or anything else where you have a
;; couple of modes you'd like to use.

;; Use of 'psgml-mode' is highly recommended.  It is, of course, a
;; part of Debian GNU/Linux.

;; configure these:
(defvar two-mode-lmatch "<?") 
(defvar two-mode-rmatch "?>")

(defvar default-mode (list 'html-mode "HTML"))
(defvar second-mode (list 'c++-mode "C++"))
;; ----------------

(defun two-mode-mode-setup ()
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'two-mode-mode-update-mode nil t)
  (make-local-variable 'minor-mode-alist)
  (or (assq 'two-mode-mode minor-mode-alist)
      (setq minor-mode-alist
            (cons '(two-mode-mode " two-mode") minor-mode-alist))))

(defun two-mode-change-mode (to-mode)
  (if (string= to-mode mode-name)
      t
    (progn 
      (if (string= to-mode (cadr second-mode))
          (funcall (car second-mode))
        (funcall (car default-mode)))
      (two-mode-mode-setup)
;      (if (eq font-lock-mode t)		  
;          (font-lock-fontify-buffer))
;	  (font-lock-fontify-buffer)
	  )))

(defun two-mode-mode-update-mode ()
  (let ((lm -1)
        (rm -1))
    (save-excursion 
      (if (search-backward two-mode-lmatch nil t)
          (setq lm (point))
        (setq lm -1)))
    (save-excursion
      (if (search-backward two-mode-rmatch nil t)
          (setq rm (point))
        (setq rm -1)))
    (if (and (= lm -1) (= rm -1))
        (two-mode-change-mode (cadr default-mode))
      (if (>= lm rm)
          (two-mode-change-mode (cadr second-mode))
        (two-mode-change-mode (cadr default-mode))))))

(defun two-mode-mode ()
  (interactive)
  (funcall (car default-mode))
  (setq two-mode-mode t)
  (two-mode-mode-setup))

(provide 'two-mode-mode)
