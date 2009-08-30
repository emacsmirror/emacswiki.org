;;;
;; Version: %Id: 3%
;; http://www.emacswiki.org/cgi-bin/wiki?action=edit;id=.emacs-IzaakBeekman.el
;; .emacs initialization file

(add-to-list 'auto-mode-alist
            '("\\.[fF]\\(03\\|95\\)\\'" . f90-mode))

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))


;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)


;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs)
       (global-font-lock-mode t)
))


;; Always end a file with a newline
(setq require-final-newline t)


;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)


;; Enable wheelmouse support by default
(if (not running-xemacs)
    (require 'mwheel) ; Emacs
  (mwheel-install) ; XEmacs
)

(setq user-emacs-directory "~/.emacs.d/elpa")

;;;;;;;;;;;;;;;;;;;;;;;;;; Initialize packages ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use the highlight 80+ package
(add-to-list 'load-path "~/.emacs.d/")
(require 'highlight-80+)


;; elpa package manager
(add-to-list 'load-path "~/.emacs.d/elpa")
(require 'package)


;; url w3
(add-to-list 'load-path "~/.emacs.d/elpa/url-1.15")
(require 'url)


;; XML-RPC
(add-to-list 'load-path "~/.emacs.d/elpa/xml-rpc-1.6.4")
(require 'xml-rpc)


;; To enable erc.el (irc client) to run
(defface erc-notice-face
  (if (or (featurep 'xemacs)
          (< emacs-major-version 22))
      '((t (:bold t :foreground "blue")))
    '((((class color) (min-colors 88))
       (:bold t :foreground "SlateBlue"))
      (t (:bold t :foreground "blue"))))
  "ERC face for notices."
  :group 'erc-faces)
;; IRC client
(add-to-list 'load-path "~/.emacs.d/elpa/erc-5.3")
(require 'erc) 


;; lisppaste
(add-to-list 'load-path "~/.emacs.d/elpa/lisppaste-1.4")
(require 'lisppaste)


;; asciidoc
(add-to-list 'load-path "~/.emacs.d/elpa/asciidoc-0.1")
(require 'asciidoc)


;; Do isearch in Dired but match only at file names.
(add-to-list 'load-path "~/.emacs.d/elpa/dired-isearch-0.3")
(require 'dired-isearch) 
;; Recommended keybindings:
 (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
 (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
 (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
 (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)


;; Highlight-symbol
(add-to-list 'load-path "~/.emacs.d/elpa/highlight-symbol-1.0.3")
(require 'highlight-symbol)


;; Highlight-parentheses
(add-to-list 'load-path "~/.emacs.d/elpa/highlight-parentheses-0.9.1")
(require 'highlight-parentheses)


;; Highline-4.2
(add-to-list 'load-path "~/.emacs.d/elpa/highline-4.2")
(require 'highline)
 ;; Turn on local highlighting for Dired (C-x d)
 (add-hook 'dired-after-readin-hook 'highline-on)
 ;; Turn on local highlighting for list-buffers (C-x C-b)
 (defadvice list-buffers (after highlight-line activate)
   (save-excursion
     (set-buffer "*Buffer List*")
     (highline-on)))


;; iresize
;; (add-to-list 'load-path "~/.emacs.d/elpa/iresize-0.2")
;; (require 'iresize)


;; kill-ring-search
(add-to-list 'load-path "~/.emacs.d/elpa/kill-ring-search-1.1")
(require 'kill-ring-search)
(autoload 'kill-ring-search "kill-ring-search"
 "Search the kill ring in the minibuffer."
 (interactive))


;; pick-backup
(add-to-list 'load-path "~/.emacs.d/elpa/pick-backup-0.8")
(require 'pick-backup)


;; tex-math-preview (f8)
(add-to-list 'load-path "~/.emacs.d/elpa/tex-math-preview-5")
(require 'tex-math-preview)
(autoload 'tex-math-preview "tex-math-preview" nil t)



;; wtf
(add-to-list 'load-path "~/.emacs.d/elpa/wtf-2.0")
(require 'wtf)


;; Compile-bookmarks
(add-to-list 'load-path "~/.emacs.d/elpa/compile-bookmarks-0.2")
(require 'compile-bookmarks)


;; css-mode
(add-to-list 'load-path "~/.emacs.d/elpa/css-mode-1.0")
(require 'css-mode)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(autoload 'css-mode "css-mode" nil t)


;; dictionairy
(add-to-list 'load-path "~/.emacs.d/elpa/dictionary-1.8.7")
(require 'dictionary)


;; functions to insert common fortran syntax which may be tied to key bindings

(defun f-header ()
  (interactive)
  (insert "
!******************************************************************************|
! *TEXT HERE*|
!                                                                              |
! Originally written by Izaak Beekman at the Princeton University Crocco Lab,  |
! *DATE.*                                                                    |
! Dialect: f95/f03                                                             |
!                                                                              |
! Dependencies: types.f90, DNSread.f90                                         |
!                                                                              |
! Sticky Tag: $Name$
! Current Version $Revision$
! Latest change by $Author$ on $Date$
!..............................................................................!
"))

(defun f-new-line ()
  (interactive)
  (insert "PRINT*, ' '"))

(defun f-i-none ()
  (interactive)
  (insert "IMPLICIT NONE"))

(defun f-i-in ()
  (interactive)
  (insert "INTENT(in)"))

(defun f-i-out ()
  (interactive)
  (insert "INTENT(out)"))

(defun f-d-1 ()
  (interactive)
  (insert "DIMENSION(:)"))

(defun f-d-2 ()
  (interactive)
  (insert "DIMENSION(:,:)"))

(defun f-d-3 ()
  (interactive)
  (insert "DIMENSION(:,:,:)"))

(defun f-d-4 ()
  (interactive)
  (insert "DIMENSION(:,:,:,:)"))

(defun f-int ()
  (interactive)
  (insert "INTEGER(I4B)"))

(defun f-real-d ()
  (interactive)
  (insert "REAL(DP)"))

(defun f-real-s ()
  (interactive)
  (insert "REAL(SP)"))

(defun f-opt ()
  (interactive)
  (insert "OPTIONAL"))

(defun f-alloc ()
  (interactive)
  (insert "ALLOCATABLE"))

;;;;;;;;;;;;;;;;;;;;
;; Screen capture ;;
;;;;;;;;;;;;;;;;;;;;

(defvar screen-capture-command
        "xwd -root  2> /dev/null | xwdtopnm  2> /dev/null | pnmtopng 2> /dev/null"
        "Command to produce a screen shot.")

(defvar screen-capture-file-extension ".png"
        "File type extension the screen shot will be.")

(defun screen-cap ()
  (interactive)
  (let ((tmpfile (make-temp-file "emacs-screen" nil
				 screen-capture-file-extension)))
    (with-temp-file tmpfile
      (shell-command screen-capture-command t))
    tmpfile)
  )

(defun screen-paste ()
  (interactive)
  (let ((screen-file (screen-cap)))
    (
    (imagebin-post file)))

(defvar imagebin-url "http://imagebin.org/index.php")

(defvar imagebin-nickname "ibeekman")

(defun imagebin-post (file)
  (shell-command (format "curl '%s' -F 'nickname=%s' -F \"image=%s;type=image/png\" -F 'disclaimer_agree=Y' -F 'Submit=Submit' -F 'mode=add'"
			 imagebin-url imagebin-nickname file))
)

(global-set-key [C-kp-enter] 'screen-paste)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq make-backup-files 'non-nil)
(setq
   backup-by-copying t       ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 3
   version-control t)
(setq tramp-default-method "ssh")
(setq truncate-partial-width-windows nil)
(setq default-major-mode 'text-mode)
(setq text-mode-hook '(lambda () (auto-fill-mode 1)))

(setq visible-bell t)
(setq lpr-switches 
      '("-o number-up=2 gamma=400 page-left=9 page-right=9 page-top=9 page-bottom=9"))
(setq pr-switches 
      '("-o number-up=2 gamma=400 page-left=9 page-right=9 page-top=9 page-bottom=9"))

;;;;;;;;;;;;;;;;;;;
;      spell things
;;;;;;;;;;;;;;;;;;;

;; (setq-default ispell-program-name "/sw/share/aspell/ispell")
;; (setq ispell-dictionary "american")
(add-hook 'text-mode-hook 'flyspell-mode)
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)



;;;;;;;;;;;;;;;;;;;;;;
;  mail things
;;;;;;;;;;;;;;;;;;;;;;

(setq mail-self-blind t)
(setq mail-yank-prefix "|>  ")
(setq mail-default-reply-to "ibeekman@princeton.edu")
;; (setq rmail-file-name (expand-file-name "~/Mail/RMAIL"))
;; (setq rmail-last-rmail-file (expand-file-name "~/Mail/"))
;; (setq rmail-inbox-to-list (expand-file-name "~/Mail/"))
;; (setq rmail-output-to-rmail-file (expand-file-name "~/Mail/"))



;;;;;;;;;;;;;;;;;;;;;;;
;  TeX things
;;;;;;;;;;;;;;;;;;;;;;;
;
(setq tex-dvi-view-command "xdvi -S 20 -s 3 -bg #576 -fg white -hl blue4 -expert -geometry 850x1000+200+5")
(setq tex-dvi-print-command "dvips -f * | lpr")


;;
;;  custom functions
;;

; Full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))


; kills other buffer  
(defun kill-other-buffer (null)
  "Kills the buffer currently in other window"
  (interactive "B")
  (other-window 1)
  (kill-this-buffer)
  (other-window 1)
)


(defun my-colors (null)
  (set-background-color "#000") 
  (set-foreground-color "#0f9")
  (set-cursor-color "red")
  (set-mouse-color "green")
  (set-face-foreground 'bold "#fb0")
  (set-face-foreground 'highlight "black")
  (set-face-background 'highlight "#fb0")  
  (set-face-foreground 'modeline "steel blue")
  (set-face-background 'modeline "gray12")
  (set-face-foreground 'region "#004")
  (set-face-background 'region "#8bb")
  (set-face-foreground 'underline "#fc0")

;; (set-face-foreground 'font-lock-type-face "gold") ;Type decl. (REAL etc.)
;; (set-face-foreground 'font-lock-string-face "steelblue") ;Strings
;; (set-face-foreground 'font-lock-keyword-face "yellowgreen") ;Keywords,attributes
;; (set-face-foreground 'font-lock-constant-face "gold") ;statement labels
;; (set-face-foreground 'font-lock-warning-face "gold") ;?
;; (set-face-foreground 'font-lock-function-name-face "green") ;modules,funcs,etc
;; (set-face-foreground 'font-lock-variable-name-face "red") ;Variable declarations
  )


;
; initialize (to my own preferences)
;
 
(defun initialize (directory)
  "Initializes EMACS session to personal preferences"
  (interactive "D")
  (tool-bar-mode)
  (my-colors ())
  (display-time)
  (line-number-mode 1)
  (scroll-bar-mode 0)
  (shell)
  (split-window-horizontally)
)
(initialize "/home/ibeekman")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(case-fold-search t)
 '(column-number-mode t)
 '(comint-completion-autolist t)
 '(comint-completion-recexact t)
 '(comint-input-autoexpand t)
 '(current-language-environment "English")
 '(default-input-method "rfc1345")
 '(display-time-mode t)
 '(erc-modules (quote (autoaway autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(explicit-shell-file-name "")
 '(f90-auto-keyword-case (quote upcase-word))
 '(f90-mode-hook (quote (f90-add-imenu-menu)) t)
 '(fortran-blink-matching-if t)
 '(fringe-mode nil nil (fringe))
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode nil nil (hl-line))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(mouse-wheel-mode t nil (mwheel))
 '(remote-compile-prompt-for-host t)
 '(remote-compile-prompt-for-user t)
 '(remote-compile-user "ibeekman")
 '(save-place t nil (saveplace))
 '(shell-input-autoexpand t)
 '(show-paren-mode t nil (paren))
 '(text-mode-hook (quote (turn-on-auto-fill (lambda nil (auto-fill-mode 1)))))
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-initial-comment t)
 '(windmove-wrap-around nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(highline-face ((t (:background "midnightblue" :foreground "tomato1" :weight bold))))
 '(highline-vertical-face ((t (:background "lightcyan" :foreground "midnightblue")))))



(put 'downcase-region 'disabled nil)


(put 'upcase-region 'disabled nil)




;;;;;;;;;;;;;;;;;;;;;;;
;  custom bindings
;;;;;;;;;;;;;;;;;;;;;;;
;
(global-set-key [f11] 'fullscreen)
(global-set-key "\M-m"     'compile)
(global-set-key "\M-n"     'next-error)
(global-set-key "\C-z"     'shell)
(global-set-key "\M-1"     'shell-command)
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key "\C-x\C-]" 'what-line)
(global-set-key "\C-x\C-k" 'kill-other-buffer)
(global-set-key "\C-x\C-y" 'spell-buffer)
(global-set-key "\C-c\C-o" 'scroll-other-window)
(global-set-key "\C-c\C-t" 'current-line-to-top-of-screen)
(global-set-key "\C-c\C-e" 'clear-screen)
(global-set-key "\C-c\C-l" 'list-command-history)
(global-set-key "\C-c\C-s" 'ispell-buffer)
(global-set-key "\C-c\C-h" 'highlight-80+-mode)
(global-set-key "\C-c\C-p" 'highlight-parentheses-mode)
(global-set-key "\C-c\e"   'caps-mode)
(global-set-key [C-kp-5] 'highlight-symbol-at-point)
(global-set-key [C-kp-6] 'highlight-symbol-next)
(global-set-key [C-kp-4] 'highlight-symbol-prev)
(global-set-key "\M-\C-y" 'kill-ring-search)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(add-hook 'f90-mode-hook
	  '(lambda ()
	     (define-key f90-mode-map"\C-c\M-h" 'f-header)
	     (define-key f90-mode-map"\C-c\M-p" 'f-new-line)
	     (define-key f90-mode-map"\C-c\M-m" 'f-i-none)
	     (define-key f90-mode-map"\C-c\M-i" 'f-i-in)
	     (define-key f90-mode-map"\C-c\M-o" 'f-i-out)
	     (define-key f90-mode-map"\C-c\M-1" 'f-d-1)
	     (define-key f90-mode-map"\C-c\M-2" 'f-d-2)
	     (define-key f90-mode-map"\C-c\M-3" 'f-d-3)
	     (define-key f90-mode-map"\C-c\M-4" 'f-d-4)
	     (define-key f90-mode-map"\C-c\M-n" 'f-int)
	     (define-key f90-mode-map"\C-c\M-d" 'f-real-d)
	     (define-key f90-mode-map"\C-c\M-s" 'f-real-s)
	     (define-key f90-mode-map"\C-c\M-o" 'f-opt)
	     (define-key f90-mode-map"\C-c\M-a" 'f-alloc)
	     (highlight-80+-mode)
 	     (highlight-parentheses-mode)
	     (compile-bookmarks-mode)))

(add-hook 'erc-mode-hook
	  (lambda ()
	    (define-key erc-mode-map "\C-\M-w" 'wtf-is)
	    (highlight-parentheses-mode)))

(add-hook 'texinfo-mode-hook
	  (lambda ()
	    (define-key texinfo-mode-map [f8] 'tex-math-preview)
	     (highlight-80+-mode)
	     (highlight-parentheses-mode)))

(add-hook 'tex-mode-hook
	  (lambda ()
	    (define-key tex-mode-map [f8] 'tex-math-preview)
	     (highlight-80+-mode)
	     (highlight-parentheses-mode)))

(add-hook 'latex-mode-hook
	  (lambda ()
	    (define-key latex-mode-map [f8] 'tex-math-preview)
	     (highlight-80+-mode)
	     (highlight-parentheses-mode)))


(add-hook 'fortran-mode-hook
	  '(lambda ()
	     (define-key fortran-mode-map"\C-c\M-p" 'f-header)
	     (define-key fortran-mode-map"\C-c\M-p" 'f-new-line)
	     (define-key fortran-mode-map"\C-c\M-m" 'f-i-none)
	     (define-key fortran-mode-map"\C-c\M-i" 'f-i-in)
	     (define-key fortran-mode-map"\C-c\M-o" 'f-i-out)
	     (define-key fortran-mode-map"\C-c\M-1" 'f-d-1)
	     (define-key fortran-mode-map"\C-c\M-2" 'f-d-2)
	     (define-key fortran-mode-map"\C-c\M-3" 'f-d-3)
	     (define-key fortran-mode-map"\C-c\M-4" 'f-d-4)
	     (define-key fortran-mode-map"\C-c\M-n" 'f-int)
	     (define-key fortran-mode-map"\C-c\M-d" 'f-real-d)
	     (define-key fortran-mode-map"\C-c\M-s" 'f-real-s)
	     (define-key fortran-mode-map"\C-c\M-o" 'f-opt)
	     (define-key fortran-mode-map"\C-c\M-a" 'f-alloc)
	     (highlight-80+-mode)
	     (highlight-parentheses-mode)
	     (compile-bookmarks-mode)))


;(global-set-key "\M-g"     'gdb)
;(global-set-key "\M-o"     'overwrite-mode)
;(global-set-key "\M-2"     'another-shell)
;(global-set-key "\C-c\C-q" 'query-replace)


(add-hook 'text-mode-hook
	  '(lambda ()
	     (flyspell-mode)
	     (highlight-80+-mode)
	     (highlight-parentheses-mode)))

(add-hook 'lisp-mode-hook
	  (lambda ()
	    (highlight-parentheses-mode)))

(add-hook 'html-mode-hook
	  (lambda ()
	    (highlight-parentheses-mode)))

;; (add-hook 'iresize-mode
;; 	  (lambda ()
;; 	    (define-key iresize-mode-map "\C-l" 'enlarge-window-horizontal)
;; 	    (define-key iresize-mode-map "\C-s" 'shrink-window-horizontal)))



;;  ;; add g95 to compilation mode
;;   (eval-after-load "compile"
;;      '(setq compilation-error-regexp-alist
;;          (cons '("^In file \\(.+\\):\\([0-9]+\\)" 1 2)
;;             compilation-error-regexp-alist)))



(add-hook 'com-int-output-filter-functions
	  'comint-watch-for-password-prompt)

(set-register ?e '(file . "~/.emacs"))
(set-register ?b '(file . "~/.bashrc"))
(set-register ?p '(file . "~/.bash_profile"))

;;; This was installed by package-install.el.  This provides support
;;; for the package system and interfacing with ELPA, the package
;;; archive.  Move this code earlier if you want to reference packages
;;; in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;;;;;;;;;;;;;
;; Tinyurl ;;
;;;;;;;;;;;;;
(require 'mm-url)
(defun get-tinyurl ()
"Grabs the url at point and echos the equivalent tinyurl in the
minibuffer to ease cutting and pasting."
  (interactive)
  (let* ((long-url (thing-at-point 'url))
         (tinyurl
          (save-excursion
            (with-temp-buffer
              (mm-url-insert
               (concat "http://tinyurl.com/api-create.php?url=" long-url))
              (kill-ring-save (point-min) (point-max))
              (buffer-string)))))
    (message tinyurl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metamark - Free Short URL redirection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mm-url)
(defun get-metamark ()
  "Grabs the url at point and echos the equivalent metamark url
in the minibuffer to ease cutting and pasting."
  (interactive)
  (let* ((long-url (or (thing-at-point 'url)
                       (read-string "URL to shorten: ")))
         (url (concat "http://metamark.net/api/rest/simple?"
                      (mm-url-encode-www-form-urlencoded
                       (list (cons "long_url" long-url)))))
         (short-url
          (save-excursion
            (with-temp-buffer
              (mm-url-insert url)
              (goto-char (point-max))
              (goto-char (re-search-backward "[^[:cntrl:][:space:]]"))
              (delete-region (+ 1 (point)) (point-max))
              (kill-ring-save (point-min) (point-max))
              (buffer-string)))))
    (message "shortened: %s" short-url)))

