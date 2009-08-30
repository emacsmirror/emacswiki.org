;;;
;; -----------------------------------------------------------------------------
;; LIBRARY/SYSTEM DEPENDENT
;; -----------------------------------------------------------------------------

; pgup/pgdown like anyother human editor
(require 'scroll-in-place)

;setup tramp mode for windows
(require 'tramp)

;Adding Python mode setup
(require 'python-mode)
(autoload 'python-mode "python-mode" "Python editing mode." t)

;activate pc selection mode
;(require 'pc-selection-mode)
;(require 'desktop)

; requires ansi-color.el
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;load rst-mode
(autoload 'rst-mode "rst-mode" "mode for editing reStructuredText documents" t)

;Setup nxml mode
(load "C:\\Program Files\\emacs-21.3.50.1\\site-lisp\\nxml-mode-20041004\\rng-auto.el")

;;runs coding standards in a shell
(defun run-coding-standards (&optional n)
  (interactive)
  (split-window-vertically -10)
  (other-window 1)
  (shell)
  (insert "d:\\dev\\cvsdev\\philips\\utilities\\codingstandards\\csreport.py d:/dev/newcvs/mods/philips/application/consumersite d:/dev/newcvs/mods/philips/application/foundation > d:\\dev\\cvsdev\\philips\\utilities\\codingstandards\\results\\csreport.xml"))

;;runs iddcheck in a shell
(defun run-idd-check (&optional n)
  (interactive)
  (split-window-vertically -10)
  (other-window 1)
  (shell)
  (insert "D:\\dev\\cvsdev\\philips\\utilities\\iddcheck\\iddcheck.py D:/dev/newcvs/mods/philips/application/consumersite"))

; C-c t converts a reST txt file into html and opens the html
(defun restify (&optional n)
  (interactive)
  (shell-command 
    (concat "D:\\installed\\python\\docutils-0.3.3\\tools\\rst2html.py "
            "\"" (buffer-file-name) "\" " "\"" 
            (file-name-sans-extension (buffer-file-name)) ".html" "\""))
  (find-file (concat (file-name-sans-extension (buffer-file-name)) ".html")))
(global-set-key "\C-ct"  'restify)

;;(setenv "PYTHONPATH" ".;D:\\private\\dev;D:\\dev\\cvsdev\\philips\\utilities\\codingstandards")

;; Setup slime for Clisp
;; (setq inferior-lisp-program "C:/lisp/clisp-2.33.1/lisp.exe -B C:/lisp/clisp-2.33.1/ -M C:/lisp/clisp-2.33.1/base/lispinit.mem")
;; (add-to-list 'load-path "C:/lisp/slime-1.0")
;; (require 'slime)
;; (slime-setup)

;; requires w3
(setq load-path
      (cons "C:\\Program Files\\emacs-21.3.50.1\\site-lisp\\w3-4.0pre.47\\lisp"
            load-path))
(require 'w3-auto)

;; ---------------------------------------------------------------------------

