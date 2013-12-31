(defcustom qml-mode-hook '()
  "Called upon entry into term mode.
This is run before the process is cranked up."
  :type 'hook
  :group 'qml-mode)

(defvar qml-indent-width 4)

(defconst qml-block-re "\\(^[ \t]*\\)\\([a-zA-Z0-9]*\\)[ \t]*[a-zA-Z0-9_]*[ \t]*[a-zA-Z0-9_(),: \t]*{")

(defun qml-get-beg-of-block ()
  (save-excursion
    (when (re-search-backward qml-block-re nil t)
      (match-beginning 2)))
  )

(defun qml-get-end-of-block ()
  (save-excursion
    (when (re-search-backward qml-block-re nil t)
      (goto-char (match-end 0))
      (backward-char)
      (condition-case nil
          (save-restriction
            (forward-list)
            (point))
        (error nil))
      ))
  )

(defun qml-indent-line ()
  (let ((cur (point))
        (start (qml-get-beg-of-block))
        (end (qml-get-end-of-block))
        (cur-indent nil))
    (save-excursion
      (if (not (and start end (> cur start) (< cur end)))
          (progn
            (if start
                (goto-char start))
            (setq start (qml-get-beg-of-block))
            (setq end (qml-get-end-of-block))
            (while (and (not (eq start nil)) (not (eq end nil)) (not (and (> cur start) (< cur end))))
              (goto-char start)
              (setq start (qml-get-beg-of-block))
              (setq end (qml-get-end-of-block))
              )
            (if (or (eq start nil) (= (point) (point-min)))
                (progn
                  (goto-char (point-min))
                  (when (re-search-forward qml-block-re nil t)
                    (goto-char (match-beginning 2))
                    (setq start (point))
                    (goto-char (match-end 0))
                    (backward-char)
                    (condition-case nil
                        (save-restriction
                          (forward-list)
                          (setq end (point))
                          (setq cur-indent 0))
                      (error nil)))))))
      (if (not cur-indent)
          (progn
            (goto-char start)
            (setq cur-indent (current-indentation))
            (goto-char cur)
            (setq cur-indent (+ cur-indent default-tab-width))
            )))
    (indent-line-to cur-indent)
    (if (string= (string (char-after (point))) "}")
        (indent-line-to (- cur-indent default-tab-width))
      )
    ))

(defun qml-indent-region (start end)
  (let ((indent-region-function nil))
    (indent-region start end nil)))

(require 'css-mode)
(require 'js)

(defvar qml-keywords
  (concat "\\<" (regexp-opt '("import")) "\\>\\|" js--keyword-re))

(defvar qml-font-lock-keywords
  `(("/\\*.*\\*/\\|//.*"                ; comment
     (0 font-lock-comment-face t t))
    ("\\<\\(true\\|false\\|[A-Z][a-zA-Z0-9]*\\.[A-Z][a-zA-Z0-9]*\\)\\>" ; constants
     (0 font-lock-constant-face))
    ("\\<\\([A-Z][a-zA-Z0-9]*\\)\\>"    ; Elements
     (1 font-lock-function-name-face nil t)
     (2 font-lock-function-name-face nil t))
    (,(concat qml-keywords "\\|\\<parent\\>") ; keywords
     (0 font-lock-keyword-face nil t))
    ("\\<\\([a-z][a-zA-Z.]*\\|property .+\\):\\|\\<\\(anchors\\|font\\|origin\\|axis\\)\\>" ; property
     (1 font-lock-variable-name-face nil t)
     (2 font-lock-variable-name-face nil t))
    ("\\<function +\\([a-z][a-zA-Z0-9]*\\)\\>" ; method
     (1 font-lock-function-name-face)))
  "Keywords to highlight in `qml-mode'.")

(defvar qml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table))

;;;###autoload

(defun qml-mode()
  "Major mode for Qt declarative UI"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table qml-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(qml-font-lock-keywords))
  (set (make-local-variable 'tab-width) qml-indent-width)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'qml-indent-line)
  (set (make-local-variable 'indent-region-function) 'qml-indent-region)
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) " */")
  (setq major-mode 'qml-mode)
  (setq mode-name "qml")
  (use-local-map qml-mode-map)
  (run-hooks 'qml-mode-hook)
  )

(defvar qml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-q") 'qml-indent-exp)
    map)
  "Keymap used by `yaoddmuse-mode'.")

(defun qml-indent-exp ()
  (interactive)
  (save-excursion
    (indent-buffer))
  )

(provide 'qml-mode)

;;; qml-mode.el ends here
