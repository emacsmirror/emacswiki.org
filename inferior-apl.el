;;; inferior-apl.el ---  Basic Apl (Aplus) in a buffer functionality
;;; copyright Rusi Mody

;;; Uses Markus Triska's apl.el for encodings, input-methods etc
;;; Make sure apl.el is on emacs' load path and a+ on execution path

;;; Run with M-x run-apl

(add-to-list 'load-path "~/tryapl")  ;; where apl.el is
(require 'apl)
(require 'comint)

;; This is not proper on two counts:
;; These variables should not be globally set
;; And these values are probably wrong 
;; They are put here to remove the need for LANG=C
(setq coding-system-for-write 'iso-latin-1)
(setq coding-system-for-read 'iso-latin-1)

(defun map-string (f s)
  (apply 'string (mapcar f (string-to-list s))))

; Not sure about when conversion fails. Also about ASCII 127
(defun apl-unicode2aplus-char (c)
  "Converts unicode char to an Aplus char"
  (if (< c 128)
      c
    (let ((c2 (rassq c apl-aplus-table)))
      (unless c2 (error "Invalid char %c to apl-unicode2aplus" c))
      (car c2))))

(defun apl-unicode2aplus-str (s)
  "Converts a unicode string to an Aplus string"
  (map-string (function apl-unicode2aplus-char) s))

(defun apl-comint-send (process s)
  "`comint-simple-send' with string preprocessed with `apl-unicode2aplus-str'"
  (comint-simple-send process (apl-unicode2aplus-str s)))

;;replace comint-simple-send
(setq comint-input-sender 'apl-comint-send)

; Not sure about when conversion fails. Also about ASCII 127
(defun apl-aplus2unicode-char (c)
  "Converts Aplus char to a unicode char"
  (if (< c 128) ;; or 127??
      c
    (let ((c2 (assq c apl-aplus-table)))
      (unless c2 (error "Invalid char %c to apl-aplus2unicode" c))
      (cdr c2))))

(defun apl-aplus2unicode-str (s)
  "Converts a Aplus string to a unicode string"
  (map-string (function apl-aplus2unicode-char) s))

(add-hook 'comint-preoutput-filter-functions 'apl-aplus2unicode-str)

(defun run-apl()
  "Major mode for running APlus under emacs"
  (interactive)
  (if (not (comint-check-proc "*a+*"))
      (set-buffer (make-comint "a+" "a+")))
  (setq apl-buffer "*a+*")
  (inferior-apl-mode)
  (pop-to-buffer "*a+*"))

(custom-set-faces
 '(comint-highlight-input ((t (:foreground "DarkSeaGreen4"  :height 90)))))

;;; Below hacked from scheme.el and very incomplete
(defvar apl-mode-abbrev-table nil)
(defvar apl-mode-syntax-table (make-syntax-table))

(define-derived-mode inferior-apl-mode comint-mode "Inferior APL"
  "Copied with changes (mostly deletions) from scheme"
  (setq comint-prompt-regexp "^     ") ; 5 spaces at BOL
  (apl-mode-variables)
  (setq mode-line-process '(":%s"))
  ; Some unicode font
  (set-frame-font "-gnu-unifont-medium-r-normal--16-*-75-75-c-80-iso10646-1"))

(defun apl-mode-variables ()
  (setq local-abbrev-table apl-mode-abbrev-table)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (set-input-method "apl-ascii"))
