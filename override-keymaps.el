;;; override-keymaps.el
;
; Provides a means of overriding major-modes' keymaps when you want a
; global key binding to really be global.

(eval-when-compile (require 'cl))

(defcustom override-keymap-rules
  '(("\M-\C-x" bury-buffer (ruby python emacs-lisp)))
  "A list of rules for overriding various modes that set their keymaps on your favorite keys"
  :group 'override-keymaps
  :type '(repeat (list string symbol (repeat symbol))))

(defun concat-symbols (&rest symbols)
  "Concatinate a list of symbols, returning a new interned symbol."
  (intern (apply 'concat (mapcar 'symbol-name symbols))))

(defun override-keymap (mode key fn)
  "Appends a hook for mode to set key for fn. This allows you to override a mode's default keys if you really really want a given key to invoke a function.

eg: \(override-keymap 'ruby \"\\M-\\C-x\" 'bury-buffer)"
  (add-hook (concat-symbols mode '-mode-hook)
            `(lambda ()
               (define-key (symbol-value (concat-symbols ',mode '-mode-map))
                 ,key ',fn)) t))

(defun override-keymaps (&optional rules)
  "Processes a list of rules of the form '(key fn (modes)) and calls override-keymap on each set of key/fn for each mode specified."
  (loop for (key fn modes) in (or rules override-keymap-rules)
        do (loop for mode in modes
                 do (override-keymap mode key fn))))

(provide 'override-keymaps)
