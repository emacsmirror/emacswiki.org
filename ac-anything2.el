;;; ac-anything2.el --- ac-anything.el for the latest version of auto-complete.el
(provide 'ac-anything2)
(progn (mapc 'require '(anything auto-complete))
       (require 'pos-tip-show nil t))
(when (require 'anything-show-completion nil t)
    (use-anything-show-completion 'ac-anything2
                                  '(length ac-prefix)))
(defun ac-anything2 ()
         "Select auto-complete candidate by anything."
         (interactive)
         (let ((items (popup-list ac-menu)))
           (flet ((ac-moge-header-line() (when anything-follow-mode
                                         (with-current-buffer anything-buffer
                                           (anything-aif (popup-item-documentation (anything-get-selection))
                                               (if (fboundp 'pos-tip-show) (not (pos-tip-show it)) it))))))
           (let ((source '(((name . "Popup Items")
                            (init . (lambda () (with-current-buffer (anything-candidate-buffer 'local)
                                             (loop for x in items do (insert x "\n")))))
                            (candidates-in-buffer)
                            (get-line . buffer-substring)
                            (action . (("Select" . (lambda(x) (flet ((popup-selected-item (&rest _) x))
                                                            (call-interactively 'ac-complete))))))
                            (header-line . ac-moge-header-line)
                            (persistent-action . (lambda (x)))))))
             (anything source ac-prefix)))))
