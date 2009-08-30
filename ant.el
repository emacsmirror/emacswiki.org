;;; ant.el - A major mode for editing ant files.
;; This is free software and in the public domain.
;; Written by Jorgen Schäfer
;; Version: 20040906

;; This file implements an editing mode for ant files.
;; See http://www.ant-wars.com/ for more information.

;; To use, put this in your .emacs:
;; (add-to-list 'auto-mode-alist '("\\.ant$" . ant-mode))

(defgroup ant nil
  "A major mode for editing ant files."
  :group 'languages)

(defcustom ant-mode-hook nil
  "Hook run at the end of the initialization of the ant mode."
  :type 'hook
  :group 'programming)

(defvar ant-font-lock-keywords
  `((,(concat "(" (regexp-opt '("begin" "cond" "else" "if"
                                "states")
                              t))
     1 font-lock-keyword-face)
    (,(concat "(" (regexp-opt '("drop" "turn")
                              t))
     1 font-lock-builtin-face)
    (,(concat "(" (regexp-opt '("mark" "unmark" "move" "maybe-move"
                                "pick-up" "maybe-pickup" "sense" "flip")
                              t))
     1 font-lock-variable-name-face)
    ("(\\(->\\)[ \t]+\\(\\sw+\\))"
     (1 font-lock-builtin-face)
     (2 font-lock-function-name-face))
    ("(\\(define-state\\)[ \t]+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ("(\\(define-constant\\)[ \t]+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face)))
  "Default font lock keywords for the ant mode.")

(autoload 'scheme-mode-initialize "scheme")

(defun ant-mode ()
  "Major mode for editing ant files.
This is based on `scheme-mode'."
  (interactive)
  (kill-all-local-variables)
  (scheme-mode-initialize)
  (setq major-mode 'ant-mode
        mode-name "Ant")
  (scheme-mode-variables)
  (setq font-lock-defaults '(ant-font-lock-keywords
                             nil t (("+-*/.<>=!?$%_&~^:" . "w"))
                             beginning-of-defun
                             (font-lock-mark-block-function . mark-defun)))
  ;;(run-hooks 'scheme-mode-hook)
  (run-hooks 'ant-mode-hook))

(put 'states 'scheme-indent-function 'scheme-let-indent)

(provide 'ant)
;;; ant.el ends here
