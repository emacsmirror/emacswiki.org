;;;
;; -----------------------------------------------------------------------------
;; LIBRARY/SYSTEM DEPENDENT
;; -----------------------------------------------------------------------------

;; If the environment variable EMACSCONF is defined, load .emacs.durdn/conf.el
(setq durdn-conf-folder "~/.emacs.durdn/")
(setq durdn-env-emacs-conf "EMACSCONF")
(let ((config (concat durdn-conf-folder (getenv durdn-env-emacs-conf) ".el")))
  (if (file-exists-p config)
      (load-file config)))

;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; VARIABLES AND SETTINGS
;; ---------------------------------------------------------------------------

;; default shell if you use cygwin
;; (setq shell-file-name "c:/cygwin/bin/bash.exe")

;; If a proxy is needed, define it here
(setq url-proxy-services '(("http"     . "130.144.19.70:8080")))


;; yes-or-no -> y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

(setq gnuserv-frame (selected-frame))
(setq font-lock-maximum-decoration t)

; initial position
(setq initial-frame-alist '((minibuffer . t) (width . 80) (height . 58) 
(left . 0) (top . 0)))
(setq default-frame-alist '((minibuffer . t) (menu-bar-lines . 1) ))
(setq-default inhibit-startup-message t)

; do not truncate lines
(setq-default truncate-lines t)

; scroll step of 1
(setq scroll-step 1)
(setq default-tab-width 4)

; do not create backup files
(setq make-backup-files nil)

;Correct cvs diff switch
(setq diff-switches "-u")
;Makes 'a' work as I expect
(put 'dired-find-alternate-file 'disabled nil)
(setq tramp-default-method "plink")
;Silence the bell
(setq ring-bell-function (lambda () ()))

;Open .env files with shell-script-mode
(setq auto-mode-alist
      (cons '("\\.\\(env\\|sh\\|bash\\|env1\\)\\'" . shell-script-mode)
            auto-mode-alist))

;Open .jhtml and .jsp files in html-mode
(setq auto-mode-alist
      (cons '("\\.\\(jhtml\\|jsp\\)\\'" . html-mode)
            auto-mode-alist))

;Open .py and .pyw in python-mode
(setq auto-mode-alist
      (cons '("\\.\\(py\\|pyw\\)$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))
; Associate .rst and .rest to rst-mode
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

; Associate .xml and .xsl etc to nxml-mode
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))

;setup for multi-hop jump to reach production systems
(add-to-list
 'tramp-multi-connection-function-alist
 '("gateway-andover" tramp-multi-connect-rlogin "plink -ssh -A -l %u %h %n"))

(setq ls-lisp-dirs-first t)             ;display dirs first in dired

;; Set the name of the host and current path/file in title bar:
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; ---------------------------------------------------------------------------

;; ------------------------------------------------------
;; KEY BINDINGS AND SIMPLE COMMANDS [CAN IMPORT ANYWHERE]
;; ------------------------------------------------------

;;Opens list-buffers but with focus on it
(defun list-buffers-other-win ()
  "Opens list-buffers and put focus on it"
  (interactive)
  (list-buffers)
  (other-window 1)
  (goto-char (+ 4 (point))))

;;Open small shell at the bottom
(defun open-shell-window (&optional n)
  (interactive)
  (split-window-vertically -10)
  (other-window 1)
  (shell))

;;Moves point/cursor to matching parens. C-c]
(defun match-paren () 
  "Go to the matching parenthesis if on parenthesis otherwise insert %." 
  (interactive) 
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1)) 
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1)) 
        ((progn (backward-char 1) (looking-at "\\s\)")) (forward-char 1) (backward-list 1)) 
        (t (forward-char) (message "Não está sobre ( ou )"))))

;Move current line to top
(defun line-to-top ()
  "Move current line to top of window."
  (interactive)
  (recenter 0))

;Move point to top
(defun point-to-top ()
  (interactive)
  (move-to-window-line 0))

;Move point to bottom
(defun point-to-bottom ()
  "Move current line to top of window."
  (interactive)
  (move-to-window-line -1))

;Copy current buffer path to kill ring C-c n, C-u C-c n copies the folder
(defun kill-buffer-file-name (&optional n) 
  (interactive "P")
  (kill-new
    (if n
      ;if C-u pressed we return the directory
      (file-name-directory (buffer-file-name))
      ;else we return the entire path to buffer
      (buffer-file-name))))

;To reload my init file, and attach it to C-c C-r
(defun load-dot-emacs-file () 
(interactive)
(load-file "~/.emacs"))

;Open .emacs with C-c e
(defun open-init-dot-el-file () 
  (interactive)
  (find-file "~/.emacs"))

;;Open .emacs with C-c e
(global-set-key "\C-ce"  'open-init-dot-el-file)
;;To reload my init file, and attach it to C-c C-r
(global-set-key "\C-c\C-r" 'load-dot-emacs-file)
;;Copy current buffer path to kill ring C-c n, C-u C-c n copies the folder
(global-set-key "\C-cn"  'kill-buffer-file-name)
;;Move current line to top
(global-set-key "\M-p"  'line-to-top)
;;Move point to top
(global-set-key '[(control ,)] 'point-to-top)
;;Move point to bottom
(global-set-key '[(control .)] 'point-to-bottom)
;;Desktop read associated to C-c D
(global-set-key "\C-cD"  'desktop-read)
;;Goto line with C-c G and C-c l
(global-set-key "\C-cG"  'goto-line)
(global-set-key "\C-cl"    'goto-line)
;;Comment or Uncomment Region python-mode style with C-c #
(global-set-key "\C-c#"  'comment-or-uncomment-region)
;;Open small shell at the bottom. C-cs
(global-set-key "\C-cs"  'open-shell-window)
;;Moves point/cursor to matching parens. C-c]
(global-set-key "\C-c]"  'match-paren)
;;opens list-buffers but with focus on it
(global-set-key "\C-x\C-b" 'list-buffers-other-win)


;; ----------------------------------------------------------------------------


;; ----------------------------------------------------------------------------
;; CUSTOM 
;; ----------------------------------------------------------------------------

;custom variables have to be put after desktop-read
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(custom-group-tag-face-1 ((((class color) (background light)) (:foreground "red" :bold t))))
 '(font-lock-comment-face ((t (:foreground "green4"))))
 '(font-lock-doc-string-face ((((class color) (background light)) (:foreground "yellow4"))))
 '(font-lock-function-name-face ((t (:foreground "red4" :underline t :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "blue4" :weight bold))))
 '(font-lock-string-face ((((class color) (background light)) (:foreground "yellow4"))))
 '(font-lock-type-face ((((class color) (background light)) (:foreground "blue" :bold t))))
 '(vhdl-font-lock-attribute-face ((((class color) (background light)) (:foreground "CadetBlue" :weight bold))))
 '(vhdl-font-lock-function-face ((((class color) (background light)) (:foreground "CadetBlue" :weight bold)))))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-raise-tool-bar-buttons t t)
 '(auto-resize-tool-bars t t)
 '(bar-cursor nil)
 '(browse-url-browser-function (quote browse-url-w3))
 '(buffers-menu-grouping-function nil)
 '(buffer-tab-filter-functions nil)
 '(buffers-tab-max-buffer-line-length 20)
 '(buffers-tab-max-size 15)
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "Latin-1")
;; '(cvs-program "C:\\philipsdev\\cvs.exe")
 '(default-input-method "latin-1-prefix")
 '(global-font-lock-mode t nil (font-lock))
 '(gnuserv-frame t t)
 '(indent-tabs-mode nil)
 '(line-number-mode t)
 '(paren-mode (quote paren) nil (paren))
 '(pc-selection-mode t nil (pc-select))
 '(py-python-command "C:\\Python24\\python.exe")
 '(recent-files-number-of-entries 30)
 '(save-place t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(tab-width 4)
 '(tool-bar-button-margin 4 t)
 '(tool-bar-button-relief 1 t)
 '(tool-bar-mode nil nil (tool-bar))
 '(toolbar-visible-p nil)
 '(tramp-methods (quote (("ftp") ("rcp" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "rsh") (tramp-copy-program "rcp") (tramp-remote-sh "/bin/sh") (tramp-login-args nil) (tramp-copy-args nil) (tramp-copy-keep-date-arg "-p") (tramp-password-end-of-line nil)) ("scp" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh") (tramp-copy-program "scp") (tramp-remote-sh "/bin/sh") (tramp-login-args ("-e" "none")) (tramp-copy-args nil) (tramp-copy-keep-date-arg "-p") (tramp-password-end-of-line nil)) ("scp1" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh") (tramp-copy-program "scp") (tramp-remote-sh "/bin/sh") (tramp-login-args ("-1" "-e" "none")) (tramp-copy-args ("-1")) (tramp-copy-keep-date-arg "-p") (tramp-password-end-of-line nil)) ("scp2" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh") (tramp-copy-program "scp") (tramp-remote-sh "/bin/sh") (tramp-login-args ("-2" "-e" "none")) (tramp-copy-args ("-2")) (tramp-copy-keep-date-arg "-p") (tramp-password-end-of-line nil)) ("scp1_old" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh1") (tramp-copy-program "scp1") (tramp-remote-sh "/bin/sh") (tramp-login-args ("-e" "none")) (tramp-copy-args nil) (tramp-copy-keep-date-arg "-p") (tramp-password-end-of-line nil)) ("scp2_old" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh2") (tramp-copy-program "scp2") (tramp-remote-sh "/bin/sh") (tramp-login-args ("-e" "none")) (tramp-copy-args nil) (tramp-copy-keep-date-arg "-p") (tramp-password-end-of-line nil)) ("rsync" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh") (tramp-copy-program "rsync") (tramp-remote-sh "/bin/sh") (tramp-login-args ("-e" "none")) (tramp-copy-args ("-e" "ssh")) (tramp-copy-keep-date-arg "-t") (tramp-password-end-of-line nil)) ("remcp" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "remsh") (tramp-copy-program "rcp") (tramp-remote-sh "/bin/sh") (tramp-login-args nil) (tramp-copy-args nil) (tramp-copy-keep-date-arg "-p") (tramp-password-end-of-line nil)) ("rsh" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "rsh") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args nil) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("ssh" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-e" "none")) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("ssh1" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-1" "-e" "none")) (tramp-copy-args ("-1")) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("ssh2" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-2" "-e" "none")) (tramp-copy-args ("-2")) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("ssh1_old" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh1") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-e" "none")) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("ssh2_old" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh2") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-e" "none")) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("remsh" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "remsh") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args nil) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("telnet" (tramp-connection-function tramp-open-connection-telnet) (tramp-login-program "telnet") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args nil) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("su" (tramp-connection-function tramp-open-connection-su) (tramp-login-program "su") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-" "%u")) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("sudo" (tramp-connection-function tramp-open-connection-su) (tramp-login-program "sudo") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-u" "%u" "-s" "-p" "Password:")) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("sudo2" (tramp-connection-function tramp-open-connection-su) (tramp-login-program "sudo") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("su" "-" "%u")) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("multi" (tramp-connection-function tramp-open-connection-multi) (tramp-login-program nil) (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args nil) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("scpx" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh") (tramp-copy-program "scp") (tramp-remote-sh "/bin/sh") (tramp-login-args ("-e" "none" "-t" "-t" "/bin/sh")) (tramp-copy-args nil) (tramp-copy-keep-date-arg "-p") (tramp-password-end-of-line nil)) ("sshx" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "ssh") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-e" "none" "-t" "-t" "/bin/sh")) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("krlogin" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "krlogin") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-x")) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line nil)) ("plink" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "plink") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-ssh" "-A")) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line "xy")) ("psess" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "plink") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-v" "-load" "nick@durdn.yi.org")) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line "xy")) ("plink1" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "plink") (tramp-copy-program nil) (tramp-remote-sh "/bin/sh") (tramp-login-args ("-1" "-ssh")) (tramp-copy-args nil) (tramp-copy-keep-date-arg nil) (tramp-password-end-of-line "xy")) ("pscp" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "plink") (tramp-copy-program "pscp") (tramp-remote-sh "/bin/sh") (tramp-login-args ("-ssh")) (tramp-copy-args nil) (tramp-copy-keep-date-arg "-p") (tramp-password-end-of-line "xy")) ("fcp" (tramp-connection-function tramp-open-connection-rsh) (tramp-login-program "fsh") (tramp-copy-program "fcp") (tramp-remote-sh "/bin/sh -i") (tramp-login-args ("sh" "-i")) (tramp-copy-args nil) (tramp-copy-keep-date-arg "-p") (tramp-password-end-of-line nil)))))
 '(transient-mark-mode t)
 '(truncate-lines t)
;; '(url-proxy-services (quote (("http" . "130.144.19.70:8080"))))
)
