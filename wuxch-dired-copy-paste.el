;;; wuxch-dired-copy-paste.el
(require 'dired)
(require 'dired-aux)

(define-key dired-mode-map [(control c)(control c)] 'ignore)
(define-key dired-mode-map [(control c)(control c)] 'wuxch-dired-copy)

(define-key dired-mode-map [(control c)(control x)] 'ignore)
(define-key dired-mode-map [(control c)(control x)] 'wuxch-dired-cut)

(define-key dired-mode-map [(control c)(control v)] 'ignore)
(define-key dired-mode-map [(control c)(control v)] 'wuxch-dired-paste)


(defvar dired-copied-cutted-files-pool nil "global variable to store copied or cutted files")
(defvar dired-is-copied nil "t:copy  nil:cut")

(defun wuxch-dired-copy()
  ""
  (interactive)
  (wuxch-dired-do-copy-cut t)
  )

(defun wuxch-dired-cut()
  ""
  (interactive)
  (wuxch-dired-do-copy-cut nil)
  )

(defun wuxch-dired-do-copy-cut(is-copy)
  "wuxch-dired-do-copy-cut:"
  (wuxch-clear-copied-cutted-files-pool)
  (wuxch-put-marked-files-name-to-pool)
  (let ((copy-cut-string)(num (safe-length dired-copied-cutted-files-pool)))
    (setq dired-is-copied is-copy)
    (if is-copy
        (setq copy-cut-string "copied")
      (setq copy-cut-string "cut")
      )
    (if (eq num 1)
        (progn
          (message "%s is %s" (car dired-copied-cutted-files-pool) copy-cut-string)
          )
      (progn
        (message "%d file/dir(s) %s" num copy-cut-string)
        )
      )
    )
  )

(defun wuxch-dired-paste()
  "wuxch-dired-paste:"
  (interactive)
  (if (not (eq dired-copied-cutted-files-pool nil))
      (let ((copy-cut-string)(current-file-number 0)(file-number (safe-length dired-copied-cutted-files-pool)))
        (if dired-is-copied
            (setq copy-cut-string "copied")
          (setq copy-cut-string "moved"))
        (dolist (src-file dired-copied-cutted-files-pool)
          (let ((dst-file))
            (setq dst-file (concat (dired-current-directory) (file-name-nondirectory src-file)))
            (if dired-is-copied
                (dired-copy-file src-file dst-file t)
              (dired-rename-file src-file dst-file t)
              )
            ;; revert buffer
            (wuxch-dired-revert)
            (dired-mark-files-regexp (file-name-nondirectory src-file))
            ;; show some information
            (setq current-file-number (+ current-file-number 1))
            (message "%d of %d file/dir(s) %s" current-file-number file-number copy-cut-string)
            )
          )
        (if (not dired-is-copied)
            (wuxch-clear-copied-cutted-files-pool))
        )
    )
  )

(defun wuxch-clear-copied-cutted-files-pool()
  "wuxch-clear-copied-cutted-files-pool: clear the pool if it's not nil"
  (if (not (eq dired-copied-cutted-files-pool nil))
      (progn
        (setq dired-copied-cutted-files-pool nil)
        )
    )
  )

(defun wuxch-put-marked-files-name-to-pool()
  "wuxch-put-marked-files-name-to-pool:"
  (let ((files))
    (setq files (dired-get-marked-files t))
    (if (listp files)
        (dolist (element files)
          (setq dired-copied-cutted-files-pool
                (append dired-copied-cutted-files-pool (list (concat (dired-current-directory) element))))
          )
      )
    )
  )

(provide 'wuxch-dired-copy-paste)

