;;; vsh-mode.el is free software
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct 3D Vertex shader major mode
;;
;; Martin Slater - mslater@hellinc.net
;; 13-03-2003
;; 14-03-2003 - Improved regex's.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vsh-keywords
  '(
	("\\(;.*\\|//.*\\)$" 1 'font-lock-comment-face)
	("\\(def \\|dp4 \\|dst \\|expp \\|logp \\|rcp \\|sge \\|slt \\|sub \\|exp \\|frc \\|log \\|m3x2 \\|m3x4 \\|mad \\|max \\|mov \\|dp3 \\|add \\|lit \\|mul \\|m3x3 \\|m4x4 \\|m4x3 \\|rsq \\|min \\|vs\.[0-9]\.[0-9]\\)" 1 'font-lock-keyword-face)
	("\\(-?c\\(?:[0-9][0-9]?\\|\\(?:\\[.*\\]\\)\\)\\(?:\\.[x|y|z|w]\\{1,4\\}\\)?\\)" 1 'font-lock-constant-face)
	("\\(-?r[0-9][0-9]?\\(?:\\.[x|y|z|w]\\{1,4\\}\\)?\\)" 1 'font-lock-type-face)
	("\\(\\(?:oPos\\|oD0\\|oD1\\|oFog\\|oT0\\)\\(?:\.x*y*z*w*\\)?\\)" 1 'font-lock-string-face)
	("\\(-?v[0-9][0-9]?\\(?:\\.[x|y|z|w]\\{1,4\\}\\)?\\)" 1 'font-lock-builtin-face)
	)
  )

(define-derived-mode vsh-mode text-mode "Vertex Shader"
  "Major mode to edit Vertex Shader files."
  (set (make-local-variable 'font-lock-defaults) 
	   '(vsh-keywords t nil nil nil (font-lock-multiline . t)))
  (set (make-local-variable 'comment-start) "//")
  (require 'font-lock)
  (turn-on-font-lock)
  )

(provide 'vsh-mode)

;;; vsh-mode.el ends here
