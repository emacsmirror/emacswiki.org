;;; ajc-java-complete-config.el --- Auto Java Completion  for GNU Emacs
        
(require 'auto-complete)
(require 'yasnippet)
(require 'ajc-java-complete)
;; conflect with 
;; (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; when complete constructor 

(ajc-init)
;;hooks 
(defun ajc-java-complete-hook ()
  (ajc-init-when-load-first-java-file)
    (setq ac-sources (append 
                      '( ac-source-ajc-class
                         ac-source-ajc-import 
                         ac-source-ajc-constructor 
                         ac-source-ajc-method
                         ac-source-ajc-keywords ) ac-sources))
;; auto import all Class in source file    
(local-set-key (kbd "C-c i") (quote ajc-import-all-unimported-class))
;; import Class where under point 
(local-set-key (kbd "C-c m") (quote ajc-import-class-under-point))
    )

(add-hook 'java-mode-hook 'ajc-java-complete-hook)
;(add-hook 'emacs-lisp-mode-hook 'ajc-java-complete-hook)

;; sources for auto complete
(ac-define-source ajc-import
  '((candidates . (ajc-import-package-candidates))
   (prefix . "^[ \t]*import[ \t]+\\(.*\\)") 
))

(ac-define-source ajc-class
  '((candidates . (ajc-complete-class-candidates ))
   (prefix . "\\b\\([A-Z][a-zA-Z0-9_]*\\)")
   (cache)
;   (action . ajc-remove-package-name-when-complete-class-with-ac)
))

(ac-define-source ajc-constructor
  '((candidates . (ajc-complete-constructor-candidates ))
   (cache)
   (prefix . "\\bnew[ \t]+\\([A-Z][a-zA-Z0-9_]*[ \t]*(?\\)")
   (action . ajc-expand-constructor-yasnippet-templete-with-ac)
))

(ac-define-source ajc-method
  '((candidates . (ajc-complete-method-candidates ))
  (cache)
  (requires . 0)
  (prefix . "\\.\\(.*\\)") 
  (action .  ajc-expand-method-yasnippet-templete-with-ac)
))

(ac-define-source ajc-keywords
  '((candidates . (ajc-java-keywords-candidates))
) )
;; end of sources


;;;; actions after complete 
;;action after finished complete constructor.
;;it will try to find out the  templete from  a hashtable
;;named `ajc-constructor-templetes-4-yasnippet' for the
;;last-complete if found ,expand it  use yasnippet; if not
;;do nothing.
(defun ajc-expand-constructor-yasnippet-templete-with-ac ()
  (let* ((last-complete-string (cdr ac-last-completion))
         (yasnippet-templete (gethash last-complete-string ajc-constructor-templetes-4-yasnippet-hashmap )))
    (when  yasnippet-templete
      (delete-backward-char (length last-complete-string))
      (yas/expand-snippet yasnippet-templete))))


;;action after finished complete constructor.
;;it will try to find out the  templete from  a hashtable
;;named `ajc-method-templetes-4-yasnippet' for the
;;last-complete if found ,expand it  use yasnippet; if not
;;do nothing.
(defun ajc-expand-method-yasnippet-templete-with-ac ()
  (let* ((last-complete-string (cdr ac-last-completion))
         (yasnippet-templete (gethash last-complete-string ajc-method-templetes-4-yasnippet-hashmap )))
    (when yasnippet-templete
      (delete-backward-char (length last-complete-string)  )
      (yas/expand-snippet yasnippet-templete))))

;; (defun ajc-remove-package-name-when-complete-class-with-ac ()
;;   (let* ((last-complete-string (cdr ac-last-completion))
;;          (class-name (gethash last-complete-string ajc-class-name-candidates-hashmap )))
;;     (when class-name
;;      (delete-backward-char (length last-complete-string)  )
;;       (insert class-name))))

(defadvice ac-selected-candidate  (around ajc ) ""
  ad-do-it
  (let* ((original-return-string ad-return-value)
         (value (gethash original-return-string
                         ajc-full-short-candidate-hashmap)))
    (when value
      (add-text-properties 0
                           (length value)
                           (text-properties-at 0 ad-return-value)
                           value  )
      (setq ad-return-value value ))

    )
  )
(ad-activate 'ac-selected-candidate)

(defadvice  ac-expand-common (before ajc-expand-common) ""
  (when (and ac-common-part ajc-full-short-candidate-hashmap)
      (setq ac-common-part
            (gethash ac-common-part ajc-full-short-candidate-hashmap ac-common-part))
      ))
(ad-activate 'ac-expand-common)

(provide 'ajc-java-complete-config)


