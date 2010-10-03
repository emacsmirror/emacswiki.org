;;; configure for icicles
(add-to-list 'load-path (concat emacs-site-lisp-dir "icicles"))

(require 'icicles)


(defun wuxch-icicle-mode-hook ()
  (global-set-key [(control x)(\')]   'ignore)
  (global-set-key [(control x)(\')]   'icicle-goto-marker)
  ;; i think iswitchb-buffer is more handy than icicle-buffer
  (global-set-key [(control x)(b)]   'iswitchb-buffer)
  )

(add-hook 'icicle-mode-hook 'wuxch-icicle-mode-hook)

(provide 'wuxch-icicles)
