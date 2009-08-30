;;;
(require 'browse-kill-ring+)

;; 查看kill ring
(global-set-key [(control c)(k)] 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(define-key browse-kill-ring-mode-map [down] 'browse-kill-ring-forward)
(define-key browse-kill-ring-mode-map [(control down)] 'browse-kill-ring-forward)
(define-key browse-kill-ring-mode-map [up] 'browse-kill-ring-previous)
(define-key browse-kill-ring-mode-map [(control up)] 'browse-kill-ring-previous)

(custom-set-variables
 '(browse-kill-ring-display-duplicates nil)
 )

(provide 'wuxch-browse-kill-ring)

