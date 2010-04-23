;;; wcy-complete.el --- extremely simple version of tempo, not as powerful as tempo. 
;;;
;;; how to use it?
;;;   create a file, for example, ~/.emacs.d/wcy-ac/emacs-lisp-mode/defun.ac 
;;; then when editing a elisp file, "defun" can be expanded according to content of "defun.ac"
;;;      
;;; in "defun.ac", it is just a skeleton definition. see function skeleton-insert for detail.
;;; for example,
;;;     
;;;     (nil 
;;;       "(defun " _ "()" ?\n > "\""  "\"" ?\n >  ")")
;;;
(defvar wcy-complete-directory "~/.emacs.d/wcy-ac") 
(defvar wcy-complete-history nil)
(make-variable-buffer-local 'wcy-complete-history)
;;;###autoload
(defun wcy-complete()
  (interactive)
  (let ((directory (expand-file-name (symbol-name major-mode) wcy-complete-directory)))
    (apply 
     #'(lambda (&optional ac-def beg end)
         (let ((file-name (expand-file-name (concat (or ac-def "") ".ac") directory)))
           (when (file-readable-p file-name)
             (delete-region beg end)
             (skeleton-insert 
              (with-temp-buffer
                (insert-file-contents file-name)
                (read (current-buffer)))))))
     (let ((ac-defines (mapcar 'file-name-sans-extension (directory-files directory nil "\\.ac$" nil))))
       (cond
        ((looking-back (regexp-opt ac-defines) (line-beginning-position))
         (list (match-string 0) (match-beginning 0) (match-end 0)))
        (t 
         (let ((bounds (bounds-of-thing-at-point 'symbol)))
           (when bounds
             (let* ((input-text (buffer-substring (car bounds) (cdr bounds)))
                    (completions (all-completions input-text ac-defines)))
               (list (if (= (length completions) 1)
                         (car completions)
                       (completing-read "wcy-completion:" 
                                        ac-defines nil t input-text 
                                        wcy-complete-history))
                     (car bounds) (cdr bounds)))))))))))



