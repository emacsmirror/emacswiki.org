;;; compile-helper.el
;; -*- Mode: Emacs-Lisp -*-

(setq build-helper-files
      '(("Makefile" . "make")
        (".couchapprc" . "couchapp push")))

(defun list-to-path (list)
  (mapconcat 'identity list "/"))

(defun check-build-file-exists (directory file)
  (when (file-exists-p (concat directory "/" (car file)))
    (compile (concat "cd " directory " && " (cdr file)))
    t))

(defun scan-dir-for-build-files (directory list)
  (if list
      (when (not (check-build-file-exists directory (car list)))
          (scan-dir-for-build-files directory (cdr list)))
    t))

(defun walk-back (directory)
  (when (> (length directory) 1)
    (setq check-dir (list-to-path (append directory '(""))))
    (if (scan-dir-for-build-files check-dir build-helper-files)
        (walk-back (butlast directory)))))

(defun compile-helper ()
  (interactive)
  (walk-back (butlast (split-string default-directory "/"))))

(provide 'compile-helper)
