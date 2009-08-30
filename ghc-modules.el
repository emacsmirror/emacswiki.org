;;; 
;; usage:
;; 1. $emacs -batch -l ghc-modules.el >dist-file
;; 2. eval following sexp: (with-temp-buffer (insert-file dist-file-path) (read (current-buffer)))
(require 'cl)

(defun nlp:haskell-ghc-list-exposed (package-name)
  "package-name -> (module-name ...)"
  (with-temp-buffer
    (call-process "ghc-pkg" nil (current-buffer) nil "field" package-name "exposed-modules")
    (goto-char (point-min))
    (let (b e)
      (when (re-search-forward "exposed-modules:" (point-max) t)
        (setq b (point))
        (setq e (point-max))
        (split-string (buffer-substring-no-properties b e))))))

(defun nlp:haskell-ghc-list-pakage ()
  "((conf . (package-name ...)) ...)"
  (with-temp-buffer
    (call-process "ghc-pkg" nil (current-buffer) nil "list")
    (save-excursion
      (goto-char (point-min))
      (flet ((f () (when (re-search-forward (rx (group (+ nonl)) ":") (point-max) t)
                     (let ((conf (match-string-no-properties 1))
                           (b (point)) e)
                       (forward-line)
                       (loop while (save-excursion
                                     (goto-char (line-beginning-position))
                                     (= ?\  (following-char)))
                             do (forward-line))
                       (setq e (point))
                       (cons conf (split-string (buffer-substring-no-properties b e) "[ \f\t\n\r\v,]+" t))))))
        (loop with ret = (f)
              while ret
              collect ret
              do (setq ret (f)))))))

(defun nlp:haskell-ghc-list-pakage-filter (lst)
  (remove-if (lambda (s) (string-match (rx bol (or "(" "{")) s)) lst))

(defun nlp:haskell-ghc-list-select-module (selector)
  "ie. (nlp:haskell-ghc-list-select-module (lambda (l) (completing-read ">" l)))"
  (let ((ret (nlp:haskell-ghc-list-pakage-filter
              (reduce (lambda (a x) (append (cdr x) a)) (nlp:haskell-ghc-list-pakage) :initial-value '()))))
    (let ((it  (funcall selector ret)))
      (funcall selector (nlp:haskell-ghc-list-exposed it)))))

(defun nlp:byte-compile-funciton (symbol)
  (fset symbol (byte-compile (symbol-function symbol))))

(defun main ()
  (print (delete-dups (reduce (lambda (a x)
                                (append (nlp:haskell-ghc-list-exposed x) a))
                              (reduce (lambda (a x) (append (cdr x) a)) (nlp:haskell-ghc-list-pakage) :initial-value '())
                              :initial-value '()))))

(mapcar 'nlp:byte-compile-funciton
        '(nlp:haskell-ghc-list-exposed nlp:haskell-ghc-list-pakage nlp:haskell-ghc-list-pakage-filter main))

(main)
