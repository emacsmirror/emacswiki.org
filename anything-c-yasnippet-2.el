(require 'anything)
(require 'yasnippet)

;;; define ays:* functions for compatibility
(eval-and-compile
  (dolist (sym '(table-hash table-parents get-snippet-tables
                            template-content visit-snippet-file-1))
    (eval `(declare-function ,(intern (format "ays:%s" sym)) "anything-c-yasnippet-2"))
    (fset (intern (format "ays:%s" sym))
          (let ((newsym (intern-soft (format "yas--%s" sym)))
                (oldsym (intern-soft (format "yas/%s" sym))))
            (cond ((fboundp newsym) newsym)
                  ((fboundp oldsym) oldsym)
                  (t
                   (error "Use yasnippet at least 0.7.0"))))))
  (fset 'ays:expand-snippet
        (if (fboundp 'yas-expand-snippet)
            'yas-expand-snippet
          'yas/expand-snippet))
  (defvaralias 'ays:buffer-local-condition
    (if (boundp 'yas-buffer-local-condition)
        'yas-buffer-local-condition
      'yas/buffer-local-condition)))

(defun ays:candidate-1 (table)
  (with-no-warnings
    (let ((hashtab (ays:table-hash table))
          (parent (ays:table-parents table))
          candidates)
      (maphash (lambda (key namehash)
                 (maphash (lambda (name template)
                            (push (cons (format "%s: %s" key name)
                                        template)
                                  candidates))
                          namehash))
               hashtab)
      (when parent
        (setq candidates
              (append candidates (ays:candidate-1 parent))))
      candidates)))

(defun ays:candidates ()
  (with-current-buffer anything-current-buffer
    (let ((ays:buffer-local-condition t))
      (apply 'append (mapcar 'ays:candidate-1 (ays:get-snippet-tables))))))

(defun ays:expand (template)
  (ays:expand-snippet (ays:template-content template) (point) (point)))

(defun ays:show-snippet (template)
  (letf (((symbol-function 'find-file-other-window)
          (symbol-function 'find-file)))
    (ays:visit-snippet-file-1 template)))

(defvar anything-c-source-yasnippet-2
  '((name . "Yasnippet (reimplemented)")
    (candidates . ays:candidates)
    (action
     ("expand" . ays:expand)
     ("open snippet file" . ays:visit-snippet-file-1))
    (persistent-action . ays:show-snippet)
    (candidate-number-limit)))

(defun anything-yasnippet-2 ()
  "Yasnippet from `anything'."
  (interactive)
  (anything :sources 'anything-c-source-yasnippet-2
            :buffer "*anything-yasnippet-2*"))

(provide 'anything-c-yasnippet-2)
