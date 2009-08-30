;;; rv-font-23.el --- set font(cjk) for emacs-23


(defvar rv-font-cjk
  "WenQuanYi Micro Hei Mono")
(defvar rv-font-latin
  "Courier New")
(defvar rv-font-symbol
  "DejaVu Sans Mono")

(defvar rv-font-latin-size 11.0)
(defvar rv-font-cjk-size 13.0)

;;(setq cjk-font "WenQuanYi Micro Hei Mono")
(setq rv-font-cjk "AR PL New Kai")

;;(unless (equal (frame-parameter nil 'font) "tty")
;;  (set-frame-font (concat latin-font "-11")))

(defvar rv-font-name "fontset-rv")

(defun rv-font-string ()
  (concat "-*-" rv-font-latin "-medium-r-normal-*-"
	  (format "%.0f" rv-font-latin-size)
	  "-*-*-*-*-*-"
	  rv-font-name))

;; also hide tool bar
(setq default-frame-alist
      `((font . ,rv-font-name)
        (tool-bar-lines nil)))

(defun rv-font-set (ch fn sz)
  (set-fontset-font rv-font-name ch
                    (font-spec
                     :family fn
                     :registry 'unicode-bmp
                     :size sz)))

(defun rv-font-set-cjk (ch)
  (rv-font-set ch rv-font-cjk rv-font-cjk-size))

(defun rv-font-set-latin (ch)
  (rv-font-set ch rv-font-latin rv-font-latin-size))
  
(defun rv-font-set-symbol (ch)
  (rv-font-set ch rv-font-symbol rv-font-latin-size))

(create-fontset-from-fontset-spec
 (rv-font-string))

;; reset all settings
(rv-font-set-latin 'unicode)
;; set others
(rv-font-set-cjk 'han)
(rv-font-set-cjk 'japanese-jisx0208)
(rv-font-set-cjk 'japanese-jisx0212)
;; 0x3000 ~ 0x303F
(rv-font-set '(12288 . 12351) "Kochi Mincho" rv-font-cjk-size)
(rv-font-set-symbol '(8000 . 9923))
(rv-font-set-symbol '(880 . 1024)) ;; part of greek 0x370~0x3FF
(rv-font-set-symbol '(?“ . ?”))    ;; fullwidth ", 0x201C ~ 0x210D


;; if not start neither as server nor in terminal
(when (eq window-system 'x) (set-frame-font rv-font-name))

(provide 'rv-font-23)

;;; end of rv-font-23.el
