;;; Tag is a tiny and powerful tools for codes browse and edit just like Swiss Army Knife.
;; But build tag file precess is more complicate and other tools, so I write this elisp-file to make it easy.

(defconst emacs-wuxch-lisp-makefile-name (concat emacs-wuxch-lisp-dir "makefile"))
(defconst wuxch-makefile-special-str '(("emacs_nt/lisp/" . "lisp-file = ")
                                       ("site-lisp/" . "site-lisp-file = ")
                                       ("wuxch-lisp/" . "wuxch-lisp-file = ")))

(defun make-lisp-file-name-list (category-str dir-name file-name)
  "make-lisp-file-name-list:"
  (if (not (eq nil (string-match category-str dir-name)))
      (progn
        (cons category-str  (cons (substring dir-name (match-end 0)) file-name))
        )
    nil
    )
  )


(defun get-lisp-file-name-info (file-full-name)
  "get-lisp-file-name-info:"
  (let* ((file-name (file-name-nondirectory file-full-name))
         (dir-name (file-name-directory file-full-name))
         (info nil)
         (i 0)
         (len (safe-length wuxch-makefile-special-str))
         )
    (while (and (eq info nil) (< i len))
      (setq info (make-lisp-file-name-list (car (nth i wuxch-makefile-special-str)) dir-name file-name))
      (setq i (+ i 1))
      )
    info
    )
  )

(defun add-elisp-file-name-to-makefile ()
  "add-elisp-file-name-to-makefile:"
  (interactive)
  (let* ((makefile-buffer (find-file-noselect emacs-wuxch-lisp-makefile-name))
         (elisp-buffer (current-buffer))
         (elisp-file-name (buffer-file-name))
         (ret)
         (info-obj nil)
         (location-str "")
         (insert-str "")
         )

    (if (string-is-in-buffer (file-name-nondirectory elisp-file-name) makefile-buffer nil)
        (progn
          (message "%s has been added to %s already" elisp-file-name emacs-wuxch-lisp-makefile-name))
      (progn
        (setq info-obj (get-lisp-file-name-info elisp-file-name))
        (setq location-str (cdr (assoc (car info-obj) wuxch-makefile-special-str)))
        (if (string-is-in-buffer location-str makefile-buffer t)
            (progn
              (setq insert-str (concat (car (cdr info-obj)) (cdr (cdr info-obj))))
              (insert (concat insert-str " "))
              (save-buffer)
              (message "add %s to %s OK" insert-str emacs-wuxch-lisp-makefile-name)
              )
          (progn
            (message "can not locate position by string:%s" location-str))
          )
        )
      (switch-to-buffer elisp-buffer)
      )
    )
  )

(defun string-is-in-buffer (str buffer-name stay-in-search-buffer)
  "string-is-in-buffer:"
  (let ((cur-buf (current-buffer))
        (ret))
    (switch-to-buffer buffer-name)
    (goto-char (point-min))
    (setq ret (search-forward-regexp str nil t))
    (if (not stay-in-search-buffer)
        (switch-to-buffer cur-buf)
      )
    ret
    )
  )

(defun wuxch-elisp-build-tag ()
  "wuxch-elisp-build-tag:"
  (interactive)
  (setq default-directory emacs-wuxch-lisp-dir)
  (cd emacs-wuxch-lisp-dir)
  (wuxch-compile-make-arg (concat "re"))
  )

(provide 'wuxch-elisp-tag)
