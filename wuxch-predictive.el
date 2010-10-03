;;; configures and codes for predictive mode.
(add-to-list 'load-path (concat emacs-site-lisp-dir "predictive"))

;; dict path
(add-to-list 'load-path "~/wuxch-dict")

;; (require 'predictive)
(autoload 'predictive-mode "predictive.el" t)


(defconst predictive-elisp-dictionary-name "dict-elisp")
(defconst predictive-common-dictionary-name "dict-all")
(defconst predictive-self-defined-dictionary-name "dict-wuxch")

(defun wuxch-predictive-mode-hook ()
  (set-default 'predictive-auto-add-to-dict nil)
  ;; dict-all is meta-dictionary combined with dict-wuxch and dict-english
  ;; (set-default 'predictive-main-dict predictive-common-dictionary-name)

  (funcall 'predictive-load-dict predictive-self-defined-dictionary-name)
  (funcall 'predictive-load-dict predictive-common-dictionary-name)
  (funcall 'predictive-set-main-dict predictive-common-dictionary-name)

  (setq predictive-auto-learn nil
        predictive-add-to-dict-ask nil
        predictive-use-auto-learn-cache nil
        predictive-which-dict t)

  (custom-set-variables
   '(auto-completion-syntax-alist (quote (accept . word))))

  (local-set-key [(control meta insert)] 'predictive-add-to-dict)
  (local-set-key [(control meta delete)] 'predictive-remove-from-dict)

  (add-to-list 'predictive-major-mode-alist '(emacs-lisp-mode . predictive-setup-emacs-lisp))
  )

(add-hook 'predictive-mode-hook 'wuxch-predictive-mode-hook)

;; (defun prective-add-to-self-defined-dict ()
;;   "prective-add-to-dict-by-major-mode:"
;;   (interactive)
;;   (let ((str (thing-at-point 'word)))
;;     (funcall 'predictive-add-to-dict predictive-self-defined-dictionary-name str)
;;     (message "add \"%s\" to %s" str predictive-self-defined-dictionary-name)
;;     )
;;   )

;; (defun prective-remove-from-self-defined-dict ()
;;   "prective-remove-from-dict-by-major-mode:"
;;   (interactive)
;;   (let ((str (thing-at-point 'word)))
;;     (funcall 'predictive-remove-from-dict predictive-self-defined-dictionary-name str)
;;     (message "remove \"%s\" from %s" str predictive-self-defined-dictionary-name)
;;     )
;;   )

;; (global-set-key [(control c)(f1)]   'predictive-mode)
;; I fell stroke "p" is more convenient than "f1"
(global-set-key [(control c)(p)]   'predictive-mode)


(defun predictive-wuxch-get-dict-name-by-major-mode ()
  "predictive-wuxch-get-dict-name-by-major-mode:"
  (cond
   ((string= major-mode "emacs-lisp-mode")
    predictive-elisp-dictionary-name
    )
   (t
    predictive-common-dictionary-name)
   )
  )

;;------------------------------ for elisp
(defun predictive-wuxch-get-dict-by-major-mode ()
  "predictive-wuxch-get-dict-by-major-mode:"
  (eval (intern-soft (predictive-wuxch-get-dict-name-by-major-mode)))
  )

(defun predictive-setup-emacs-lisp (arg)
  "With a positive ARG, set up predictive mode for use with html major modes.
With a negative ARG, undo these changes. Called when predictive
mode is enabled via entry in `predictive-major-mode-alist'."
  (cond
   ;; ----- enabling elisp setup -----
   ((> arg 0)
    (funcall 'predictive-load-dict predictive-elisp-dictionary-name)
    (funcall 'predictive-set-main-dict predictive-elisp-dictionary-name)
    (local-set-key [(control insert)] 'prective-add-to-dict-by-major-mode)
    (local-set-key [(control delete)] 'prective-remove-from-dict-by-major-mode)
    t) ;; indicate successful setup

   ;; ----- disabling elisp setup -----
   ((< arg 0)
    t)) ;; indicate successful reversion of changes
  )

(defun prective-add-to-dict-by-major-mode ()
  "prective-add-to-dict-by-major-mode:"
  (interactive)
  (let ((str (thing-at-point 'word)))
    (funcall 'predictive-add-to-dict (predictive-wuxch-get-dict-by-major-mode) str)
    (message "add \"%s\" to %s" str (predictive-wuxch-get-dict-name-by-major-mode))
    )
  )

(defun prective-remove-from-dict-by-major-mode ()
  "prective-remove-from-dict-by-major-mode:"
  (interactive)
  (let ((str (thing-at-point 'word)))
    (funcall 'predictive-remove-from-dict (predictive-wuxch-get-dict-by-major-mode) str)
    (message "remove \"%s\" from %s" str (predictive-wuxch-get-dict-name-by-major-mode))
    )
  )

(defconst predictive-dump-dict-buffer-name "*dict*")
(defun predictive-dump-dict-to-buffer-by-major-mode ()
  "predictive-dump-dict-to-buffer-lisp:"
  (interactive)
  (dictree-dump-to-buffer (predictive-wuxch-get-dict-by-major-mode)
                          (get-buffer-create predictive-dump-dict-buffer-name)
                          'string)
  )

(provide 'wuxch-predictive)
