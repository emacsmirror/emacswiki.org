;;;; Hack to get fill mode to work correctly for ERC input
;;;; This is very Quick'n'Dirty, so please report =any= strange
;;;; behavior, preferably via IRC to Adlai @ freenode

(require 'erc)

(defvar erc-fill-wrapped-input-p nil
  "Keeps track of whether auto-fill-mode has wrapped the input text.
Reset to NIL after a message is successfully sent.")
(make-variable-buffer-local 'erc-wrapped-input-p)

(setq normal-auto-fill-function
      (lambda ()
        (setq erc-fill-wrapped-input-p t)
        (do-auto-fill)))

(defun erc-user-input ()
  "Return the input of the user in the current buffer.
If `erc-wrapped-input-p' is true, strips all newlines."
  (let ((literal-input (buffer-substring-no-properties
                        erc-input-marker
                        (erc-end-of-input-line))))
    (if erc-fill-wrapped-input-p
        (replace-regexp-in-string "\n *" " " literal-input)
      literal-input)))

(add-hook 'erc-mode-hook
          (lambda ()
            (set-fill-column erc-fill-column)
            (auto-fill-mode)))

(add-hook 'erc-send-completed-hook
          (lambda (message)
            (declare (ignore message))
            (setq erc-fill-wrapped-input-p nil)))

(provide 'erc-input-fill)
