;;; fliptext.el --- Input method for flipping characters upside down

;; Copyright (C) 2007  André Riemann

;; Author: André Riemann <andre.riemann@web.de>
;; Maintainer: André Riemann <andre.riemann@web.de>
;; Created: 2007-09-16
;; Keywords: games i18n

;; Version: 0.1
;; Last-Updated: 2007-12-09

;; This file is in the public domain.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;; Provides an input method for flipping characters upside down by
;; selecting an appropriate character that looks upside down.
;; E.g. hello world -> p1ɹoʍ o11ǝɥ.

;; Copy the file in your load path and load it with
;;   (require 'fliptext).
;; Activate it with C-u C-\ fliptext RET.
;; Deactivate with C-\.

;; I didn't use `quail-define-package', because i couldn't figure out,
;; how to `backward-char' after conversion.

;; This file used uni-input.el as example, in particular
;; `fliptext-input-activate' is based on it.

;; This file is *NOT* part of GNU Emacs.

;;; Code:

(require 'quail)

(defconst fliptext-rules-alist
  '((?a . ?\ɐ)  (?b . ?\q)  (?c . ?\ɔ)  (?d . ?\p)  (?e . ?\ǝ)  (?f . ?\ɟ)
    (?g . ?\ƃ)  (?h . ?\ɥ)  (?i . ?\ı)  (?j . ?\ɾ)  (?k . ?\ʞ)  (?l . ?l)
    ;;(?l . ?\ʃ) ɼ І ι
    (?m . ?\ɯ)  (?n . ?u)   (?o . ?o)   (?p . ?d)   (?q . ?b)   (?r . ?\ɹ)
    (?s . ?s)   (?t . ?\ʇ)  (?u . ?n)   (?v . ?\ʌ)  (?w . ?\ʍ)  (?x . ?x)
    (?y . ?\ʎ) (?z . ?z)
    (?\[ . ?\]) (?\( . ?\)) (?\{ . ?\}) (?\< . ?\>) (?\« . ?\»)
    (?\. . ?\˙) (?\? . ?\¿) (?\! . ?\¡) (?\' . ?\,)
    (?\_ . ?\‾) (?\" . ?\„)
    ;;("‿" ?\⁀)
    ;;("⁅" ?\⁆)
    ;;("∴" ?\∵)
    ;; ∀ Ɔ Ǝ Ⅎ H I Ꮭ ℸ W N O d S ⊥ ⋂ Ʌ M X Z
    ))

(defun dassoc (key alist)
  "Returns the cdr of the first cons of ALIST if KEY is `equal' to the car of that cons of ALIST.
If that fails, returns the car of the first cons of ALIST if KEY
is `equal' to the cdr of that cons of ALIST. Otherwise returns
nil."
  (if (and key (setq new (cdr (assoc key alist))))
      new
    (car (rassoc key alist))))

(defun fliptext-input-method (key)
  (if buffer-read-only
      (list key)
    (if (setq new (dassoc (downcase key) fliptext-rules-alist))
        (list new ?\2) ;; ?\2 == backwards char
      (list key ?\2))))

(defun fliptext-input-activate (&optional arg)
  "Activate fliptext input method.
With arg, activate fliptext input method if and only if arg is
positive.

While this input method is active, the variable
`input-method-function' is bound to the function
`fliptext-input-method'."
  (if (and arg
           (< (prefix-numeric-value arg) 0))
      (unwind-protect
          (progn
            (quail-hide-guidance)
            (quail-delete-overlays)
            (setq describe-current-input-method-function nil))
        ;;(local-unset-key "\C-d")
        ;;(local-unset-key [delete])
        ;;(local-unset-key "\d")
        ;;(local-unset-key [backspace])
        (kill-local-variable 'input-method-function))
    ;;(local-set-key "\C-d" 'delete-backward-char)
    ;;(local-set-key [delete] 'delete-backward-char)
    ;;(local-set-key "\d" 'delete-char)
    ;;(local-set-key [backspace] 'delete-char)
    (setq inactivate-current-input-method-function 'fliptext-input-inactivate)
    (setq describe-current-input-method-function 'fliptext-input-help)
    (quail-delete-overlays)
    (if (eq (selected-window) (minibuffer-window))
        (add-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer))
    (set (make-local-variable 'input-method-function)
         'fliptext-input-method)))

(defun fliptext-input-inactivate ()
  "Inactivate fliptext input method."
  (interactive)
  (fliptext-input-activate -1))

(defun fliptext-input-help ()
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Flips text upside down.
E.g. hello world -> plɹoʍ ollǝɥ")))

(provide 'fliptext)

(register-input-method "fliptext" "UTF-8" 'fliptext-input-activate "ɐ"
                       "Input method for \"flipping characters upside down\".")

;;; fliptext.el ends here
