;;; .emacs.el ---
;;
;; Filename: .emacs.el
;; Description: My .emacs for laptop
;; Author: thierry
;; Maintainer:
;; Created: sam aoû 16 19:06:09 2008 (+0200)
;; Version:
;; Last-Updated: dim mar  8 09:52:52 2009 (+0100)
;;           By: thierry
;;     Update #: 1486
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;
;; (tv-ee-index-create)

;;;;«INDEX»
;;; «.add-common-lisp-package»                  (to "add-common-lisp-package")
;;; «.Load-all-gentoo's-files-from-site-lisp»   (to "Load-all-gentoo's-files-from-site-lisp")
;;; «.load-paths»                               (to "load-paths")
;;; «.desktop-save»                             (to "desktop-save")
;;; «.usage-memo»                               (to "usage-memo")
;;; «.gnus-config»                              (to "gnus-config")
;;; «.nognus»                                   (to "nognus")
;;; «.Authinfo-settings-with-epa»               (to "Authinfo-settings-with-epa")
;;; «.Debug-on-error»                           (to "Debug-on-error")
;;; «.no-menu-bar»                              (to "no-menu-bar")
;;; «.prefer-coding-system»                     (to "prefer-coding-system")
;;; «.basic-config»                             (to "basic-config")
;;; «.Pas-de-dialog-gtk»                        (to "Pas-de-dialog-gtk")
;;; «.Save-minibuffer-history»                  (to "Save-minibuffer-history")
;;; «.Recentf»                                  (to "Recentf")
;;; «.undo-limit»                               (to "undo-limit")
;;; «.Revert-buffer»                            (to "Revert-buffer")
;;; «.tags-table»                               (to "tags-table")
;;; «.kill-line/sexp»                           (to "kill-line/sexp")
;;; «.Frame-parameters»                         (to "Frame-parameters")
;;; «.emacs-w3m»                                (to "emacs-w3m")
;;; «.w3m-globals-keys»                         (to "w3m-globals-keys")
;;; «.w3m-mode-map»                             (to "w3m-mode-map")
;;; «.org-config»                               (to "org-config")
;;; «.muse-config»                              (to "muse-config")
;;; «.erc-config»                               (to "erc-config")
;;; «.Bitlbee»                                  (to "Bitlbee")
;;; «.subversion»                               (to "subversion")
;;; «.DVC-config»                               (to "DVC-config")
;;; «.winner-mode-config»                       (to "winner-mode-config")
;;; «.load-emms»                                (to "load-emms")
;;; «.ibuffer-config»                           (to "ibuffer-config")
;;; «.dired-tar»                                (to "dired-tar")
;;; «.confirm-quit-emacs»                       (to "confirm-quit-emacs")
;;; «.consequent-log-file»                      (to "consequent-log-file")
;;; «.Add-newline-at-end-of-files»              (to "Add-newline-at-end-of-files")
;;; «.No-startup-screen»                        (to "No-startup-screen")
;;; «.Message-displayed-in-scratch-on-startup»  (to "Message-displayed-in-scratch-on-startup")
;;; «.Ediff-config»                             (to "Ediff-config")
;;; «.Silly-little-window-in-ediff»             (to "Silly-little-window-in-ediff")
;;; «.highlight-current-line»                   (to "highlight-current-line")
;;; «.yaoddmuse»                                (to "yaoddmuse")
;;; «.Dired»                                    (to "Dired")
;;; «.wdired»                                   (to "wdired")
;;; «.yes-or-no»                                (to "yes-or-no")
;;; «.Affiche-l'heure-au-format-24h»            (to "Affiche-l'heure-au-format-24h")
;;; «.Battery»                                  (to "Battery")
;;; «.Wifi-wireless.el»                         (to "Wifi-wireless.el")
;;; «.Limite-max-lisp»                          (to "Limite-max-lisp")
;;; «.emacs-backup-config»                      (to "emacs-backup-config")
;;; «.Transient-mark-mode»                      (to "Transient-mark-mode")
;;; «.show-paren-mode»                          (to "show-paren-mode")
;;; «.Start-emacs-server»                       (to "Start-emacs-server")
;;; «.emacsclient-and-stumpish»                 (to "emacsclient-and-stumpish")
;;; «.Path-to-abbrev-file»                      (to "Path-to-abbrev-file")
;;; «.Compare-windows»                          (to "Compare-windows")
;;; «.Copy-and-cut-to-x-clipboard»              (to "Copy-and-cut-to-x-clipboard")
;;; «.Google-define»                            (to "Google-define")
;;; «.text-translator»                          (to "text-translator")
;;; «.html»                                     (to "html")
;;; «.htmlize»                                  (to "htmlize")
;;; «.htmlfontify»                              (to "htmlfontify")
;;; «.Whitespace-mode»                          (to "Whitespace-mode")
;;; «.Emacs-customize-have-it's-own-file»       (to "Emacs-customize-have-it's-own-file")
;;; «.Bind-comint-dynamic-complete»             (to "Bind-comint-dynamic-complete")
;;; «.regex-tool»                               (to "regex-tool")
;;; «.antiword»                                 (to "antiword")
;;; «.Elisp»                                    (to "Elisp")
;;; «.Eldoc»                                    (to "Eldoc")
;;; «.Indent-when-newline»                      (to "Indent-when-newline")
;;; «.eval-region»                              (to "eval-region")
;;; «.byte-compile-file»                        (to "byte-compile-file")
;;; «.Indent-only-with-spaces»                  (to "Indent-only-with-spaces")
;;; «.Lua-mode»                                 (to "Lua-mode")
;;; «.Python-config»                            (to "Python-config")
;;; «.terminal-ipython»                         (to "terminal-ipython")
;;; «.Pymacs»                                   (to "Pymacs")
;;; «.Pycomplete»                               (to "Pycomplete")
;;; «.config-python-mode»                       (to "config-python-mode")
;;; «.Search-in-python-library»                 (to "Search-in-python-library")
;;; «.Pylint-via-flymake»                       (to "Pylint-via-flymake")
;;; «.Flymake-pour-python»                      (to "Flymake-pour-python")
;;; «.Entete-py»                                (to "Entete-py")
;;; «.shell-config»                             (to "shell-config")
;;; «.prompt-shell-read-only»                   (to "prompt-shell-read-only")
;;; «.couleur-dans-le-shell»                    (to "couleur-dans-le-shell")
;;; «.newline-and-indent-in-sh-mode»            (to "newline-and-indent-in-sh-mode")
;;; «.Dirtrack-mode»                            (to "Dirtrack-mode")
;;; «.Eshell-config»                            (to "Eshell-config")
;;; «.Eshell-prompt»                            (to "Eshell-prompt")
;;; «.Eshell-banner»                            (to "Eshell-banner")
;;; «.Eshell-et-ansi-color»                     (to "Eshell-et-ansi-color")
;;; «.Eshell-save-history-on-exit»              (to "Eshell-save-history-on-exit")
;;; «.Eshell-directory»                         (to "Eshell-directory")
;;; «.Eshell-command»                           (to "Eshell-command")
;;; «.Eshell-toggle»                            (to "Eshell-toggle")
;;; «.eshell-visual»                            (to "eshell-visual")
;;; «.Term-et-ansi-term»                        (to "Term-et-ansi-term")
;;; «.Keys-to-access-different-consoles»        (to "Keys-to-access-different-consoles")
;;; «.shell-command-completion»                 (to "shell-command-completion")
;;; «.Entete-Bash»                              (to "Entete-Bash")
;;; «.Calculator»                               (to "Calculator")
;;; «.Binding-pour-le-calculateur-d'emacs»      (to "Binding-pour-le-calculateur-d'emacs")
;;; «.flyspell-aspell»                          (to "flyspell-aspell")
;;; «.Aspell»                                   (to "Aspell")
;;; «.Switch-dico-english-french»               (to "Switch-dico-english-french")
;;; «.Toggle-flyspell-mode»                     (to "Toggle-flyspell-mode")
;;; «.woman»                                    (to "woman")
;;; «.printing-config»                          (to "printing-config")
;;; «.Gtklp-par-defaut»                         (to "Gtklp-par-defaut")
;;; «.ext-view-config»                          (to "ext-view-config")
;;; «.auto-compression-mode»                    (to "auto-compression-mode")
;;; «.auctex-config»                            (to "auctex-config")
;;; «.Insertion-d'un-squelette-latex»           (to "Insertion-d'un-squelette-latex")
;;; «.Insertion-d'un-squelette-latex-de-lettre» (to "Insertion-d'un-squelette-latex-de-lettre")
;;; «.ledger-config»                            (to "ledger-config")
;;; «.Align-euro-device»                        (to "Align-euro-device")
;;; «.ledger-position-at-point»                 (to "ledger-position-at-point")
;;; «.newsticker-config»                        (to "newsticker-config")
;;; «.Tramp-config»                             (to "Tramp-config")
;;; «.Mode-lecture-photo-auto»                  (to "Mode-lecture-photo-auto")
;;; «.boxquote-config»                          (to "boxquote-config")
;;; «.bbdb-config»                              (to "bbdb-config")
;;; «.slime-config»                             (to "slime-config")
;;; «.common-lisp-info»                         (to "common-lisp-info")
;;; «.Save-slime-scratch-buffer»                (to "Save-slime-scratch-buffer")
;;; «.Stumpwm-mode»                             (to "Stumpwm-mode")
;;; «.Else-mode»                                (to "Else-mode")
;;; «.mozilla-javascript»                       (to "mozilla-javascript")
;;; «.warn-mail»                                (to "warn-mail")
;;; «.thumb-page-config»                        (to "thumb-page-config")
;;; «.traverselisp-config»                      (to "traverselisp-config")
;;; «.dar-backup-rules»                         (to "dar-backup-rules")
;;; «.Enable-commands-disabled-by-default»      (to "Enable-commands-disabled-by-default")
;;; «.setup-minibuffer»                         (to "setup-minibuffer")
;;; «.ffap-bindings»                            (to "ffap-bindings")
;;; «.Install-elisp»                            (to "Install-elisp")
;;; «.auto-install»                             (to "auto-install")
;;; «.anything-config»                          (to "anything-config")
;;; «.unit-tests»                               (to "unit-tests")
;;; «.delete-selection-mode»                    (to "delete-selection-mode")
;;; «.Info»                                     (to "Info")
;;; «.Require-Locate»                           (to "Require-Locate")
;;; «.align.el»                                 (to "align.el")
;;; «.linkd»                                    (to "linkd")
;;; «.eev-config»                               (to "eev-config")
;;; «.Scroll-down-Scroll-up»                    (to "Scroll-down-Scroll-up")
;;; «.Macros-thierry»                           (to "Macros-thierry")
;;; «.nxhtml-mode»                              (to "nxhtml-mode")
;;; «.Icicles»                                  (to "Icicles")
;;; «.Fuzzy-match»                              (to "Fuzzy-match")
;;; «.Frame-divers-D.A»                         (to "Frame-divers-D.A")
;;; «.buffer-menu+»                             (to "buffer-menu+")
;;; «.Doremi»                                   (to "Doremi")
;;; «.Get-rid-of-`mouse-set-font'»              (to "Get-rid-of-`mouse-set-font'")
;;; «.speed-bar»                                (to "speed-bar")
;;; «.sr-speedbar»                              (to "sr-speedbar")
;;; «.Load-tv-utils»                            (to "Load-tv-utils")
;;; «.save-scratch-buffer»                      (to "save-scratch-buffer")
;;; «.Enable-scroll-other-window-globally»      (to "Enable-scroll-other-window-globally")
;;; «.emacs-image-viewer»                       (to "emacs-image-viewer")
;;; «.xmodmap»                                  (to "xmodmap")
;;; «.convenient-keys-for-windows»              (to "convenient-keys-for-windows")
;;; «.Paredit»                                  (to "Paredit")
;;; «.Enable-time-stamp»                        (to "Enable-time-stamp")
;;; «.Auto-document»                            (to "Auto-document")
;;; «.screenshot»                               (to "screenshot")
;;; «.woof-config»                              (to "woof-config")
;;; «.Async-library»                            (to "Async-library")
;;; «.Make-header»                              (to "Make-header")
;;; «.sdcv-interface-to-stardict»               (to "sdcv-interface-to-stardict")
;;; «.sql-mode»                                 (to "sql-mode")
;;; «.ipa-mode»                                 (to "ipa-mode")
;;; «.windmove»                                 (to "windmove")
;;; «.key-for-copy-files-async»                 (to "key-for-copy-files-async")
;;; «.babel»                                    (to "babel")
;;; «.rectangles»                               (to "rectangles")
;;; «.bookmark+»                                (to "bookmark+")
;;; «.isearch»                                  (to "isearch")
;;; «.outline-minor-mode»                       (to "outline-minor-mode")
;;; «.align-let»                                (to "align-let")
;;; «.delete-pair»                              (to "delete-pair")
;;; «.Trash»                                    (to "Trash")
;;; «.line-move-visual»                         (to "line-move-visual")
;;; «.auto-complete»                            (to "auto-complete")
;;; «.anything-ipython»                         (to "anything-ipython")
;;; «.END»                                      (to "END")

;;;==UPDATE-EEV-BOUNDARY== ;; (Don't delete this line!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;; «add-common-lisp-package» (to ".add-common-lisp-package")
(require 'cl)

;; «Load-all-gentoo's-files-from-site-lisp» (to ".Load-all-gentoo's-files-from-site-lisp")
(mapc 'load
      (cddr (directory-files "/usr/share/emacs/site-lisp/site-gentoo.d" t)))


;; «load-paths» (to ".load-paths")
(add-to-list 'Info-default-directory-list "/home/thierry/elisp/info")

(dolist (i '("/home/thierry/elisp"
             "/home/thierry/elisp/AC/"
	     "/usr/local/share/emacs/site-lisp"
	     "/home/thierry/elisp/ipython"
	     "/home/thierry/elisp/python-mode"
	     "/home/thierry/elisp/emacs-w3m/"
	     "/home/thierry/elisp/ledger/"
	     "/home/thierry/elisp/emacs-jabber"
	     "/home/thierry/.emacs.d/"
	     "/home/thierry/.emacs.d/emacs-config-laptop/"
	     "/home/thierry/elisp/icicles/"
	     "/home/thierry/elisp/anything/"
	     "~/elisp/doremi/"
	     "~/elisp/eev/"
	     "~/elisp/D.A-libs/"
	     "/home/thierry/elisp/pymacs"
	     "/home/thierry/elisp/ngnus/lisp"
             "/home/thierry/elisp/bbdb/lisp"
	     "/usr/share/emacs/site-lisp/libidn"))
  (when (not (member i load-path))
    (add-to-list 'load-path i)))
	   
;; libidn is not in gentoo.d. load it
(require 'idna)
(require 'punycode)

;; «desktop-save» (to ".desktop-save")
(desktop-save-mode 1)
(setq desktop-restore-eager 5)

(defun tv-list-tramp-buffer-file-name ()
  (let* ((desktop-info-list (mapcar #'desktop-buffer-info (buffer-list)))
         (tramp-buf-list (loop for i in desktop-info-list
                            if (and (listp i)
                                    (stringp (car (nth 8 i)))
                                    (string-match "^/su:.*\\|^/sudo:.*\\|^/ssh:.*" (car (nth 8 i))))
                            collect (nth 2 i))))
    tramp-buf-list))

(add-hook 'desktop-save-hook #'(lambda ()
                                 (let ((del-buf-list
                                        (tv-list-tramp-buffer-file-name)))
                                   (dolist (i del-buf-list)
                                     (kill-buffer i)))))

;(setq desktop-files-not-to-save "^/[^/:]*:")


(defun delete-frame-save-desktop ()
  "to quit from emacsclient"
  (interactive)
  (desktop-save "~/")
  (save-some-buffers)
  (delete-frame))
(global-set-key (kbd "C-c 5 0") 'delete-frame-save-desktop)


;; «usage-memo» (to ".usage-memo")

;; Add memo to describe-func/variable
(require 'usage-memo)
(umemo-initialize)

;; «gnus-config» (to ".gnus-config")

;; «nognus» (to ".nognus")
(require 'gnus-load)
(require 'info)
(require 'gnus-async)
(setq gnus-asynchronous t)
(if (featurep 'xemacs)
    (add-to-list 'Info-directory-list "~/elisp/ngnus/texi/")
    (add-to-list 'Info-default-directory-list "~/elisp/ngnus/texi/"))

(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command 'gnus)
(setq send-mail-command 'gnus-msg-mail)
(setq gnus-init-file "/home/thierry/.emacs.d/emacs-config-laptop/.gnus.elc")
(defvar tv-gnus-loaded-p nil)
(add-hook 'message-mode-hook #'(lambda ()
                                 (when (not tv-gnus-loaded-p)
                                   (load gnus-init-file)
                                   (setq tv-gnus-loaded-p t))))
(add-hook 'gnus-startup-hook #'(lambda ()
                                 (setq tv-gnus-loaded-p t)))

(defun tv-gnus ()
  (interactive)
  (let ((gc-cons-threshold 3500000))
    (gnus)))

(global-set-key (kbd "<f7> m") 'tv-gnus)

(autoload 'gnus-dired-attach "gnus-dired.el")
(when (require 'dired)
  (define-key dired-mode-map (kbd "C-c C-a") 'gnus-dired-attach))

(defun tv-start-fetchmail ()
  (unless (tv-get-pid-from-process-name "fetchmail")
    (shell-command "fetchmail")
    (message "Fetchmail started")))

(add-hook 'gnus-startup-hook 'tv-start-fetchmail)
(add-hook 'gnus-startup-hook 'gnus-demon-init)
(add-hook 'gnus-exit-gnus-hook 'gnus-demon-cancel)
(add-hook 'gnus-exit-gnus-hook 'gnus-namazu-update-all-indices)

;; «Authinfo-settings-with-epa» (to ".Authinfo-settings-with-epa")
(require 'auth-source)
;(require 'epa-file "~/elisp/epa-file.el")
(require 'epa-file)
(epa-file-enable)
;(setq gnus-verbose 10)
(setq epa-file-cache-passphrase-for-symmetric-encryption t) ; VERY important
;(setq epa-file-cache-passphrase-for-symmetric-encryption-timeout 60)
(if (file-exists-p "~/.authinfo.gpg")
    (setq auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t)))
    (setq auth-sources '((:source "~/.authinfo" :host t :protocol t))))

;; «Debug-on-error» (to ".Debug-on-error")

(defun tv-toggle-debug-on-error ()
  (interactive)
  (let (state)
    (setq debug-on-error (not debug-on-error))
    (setq state (if debug-on-error "On" "Off"))
    (message "Debug-on-error is now `%s'" state)))

;; «no-menu-bar» (to ".no-menu-bar")
;; (find-fline "~/.Xdefaults" "Emacs.menuBar")
(unless (window-system)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1))

(setq-default cursor-in-non-selected-windows nil)

;; «prefer-coding-system» (to ".prefer-coding-system")
(prefer-coding-system 'utf-8)

;; «basic-config» (to ".basic-config")
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; «Pas-de-dialog-gtk» (to ".pas-de-dialog-gtk")
(setq use-file-dialog nil)

;; «Save-minibuffer-history» (to ".Save-minibuffer-history")
(setq savehist-file "~/.emacs.d/history")
(setq history-length 1000)
(savehist-mode 1)

;; «Recentf» (to ".Recentf")
(setq recentf-save-file "~/.emacs.d/recentf")
(recentf-mode 1)

;; «undo-limit» (to ".undo-limit")
(setq undo-limit 30000)

;; «Revert-buffer» (to ".Revert-buffer")
(global-set-key (kbd "C-c R") 'revert-buffer)

;; «tags-table» (to ".tags-table")

(global-set-key (kbd "C-M-.") 'visit-tags-table)

;; «kill-line/sexp» (to ".kill-line/sexp")
(global-set-key (kbd "C-M-j") #'(lambda ()
                                  (interactive)
                                  (kill-sexp -1)))
(global-set-key (kbd "C-c k") #'(lambda ()
                                  (interactive)
                                  (kill-line 0)))

;; «Frame-parameters» (to ".Frame parameters")
;; (find-fline "~/.Xdefaults" "Emacs*background")
;; current-font[EVAL]:(cdr (assoc 'font (frame-parameters)))
(add-to-list 'default-frame-alist '(foreground-color . "Wheat"))
(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(right-fringe . 1))
(add-to-list 'default-frame-alist '(left-fringe . 1))
;; (add-to-list 'default-frame-alist
;;              '(font . "-bitstream-Bitstream Vera Sans Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
(add-to-list 'default-frame-alist '(font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
(add-to-list 'default-frame-alist '(cursor-color . "red"))

;; «emacs-w3m» (to ".emacs-w3m")

(setq w3m-icon-directory "~/elisp/emacs-w3m/icons")

(when (require 'dired)
  (progn
    (require 'w3m-load)
    (require 'mime-w3m)
    (define-key dired-mode-map (kbd "C-c F") 'tv-find-file-as-url)))

;; (find-fline "~/.emacs.d/emacs-config-laptop/config-w3m.el" "INDEX")
(require 'config-w3m)

(setq browse-url-browser-function 'w3m-browse-url)

;; «w3m-globals-keys» (to ".w3m-globals-keys")

(global-set-key (kbd "<f7> h") 'w3m) 
(global-set-key (kbd "<f7> w") 'w3m-frweather) 
(global-set-key (kbd "<f7> t") 'w3m-dtree) 
(global-set-key (kbd "<f7> j") 'webjump) 
(global-set-key (kbd "<f7> z") 'w3m-namazu)
(global-set-key (kbd "<f7> s g") 'search-word)
(global-set-key (kbd "<f7> d e f") 'tv-dico-google-enfr)
(global-set-key (kbd "<f7> s d") 'tv-search-delicious)
(global-set-key (kbd "<f7> s p") 'tv-search-gentoo)
(global-set-key (kbd "<f7> s u") 'tv-search-gmane)
(global-set-key (kbd "<f7> s w") 'tv-search-wikipedia)
(global-set-key (kbd "\C-c j j") 'tv-jargon-at-point)
(global-set-key (kbd "<f7> s a") 'em-surfraw)

;; «w3m-mode-map» (to ".w3m-mode-map")
(define-key w3m-mode-map (kbd "C-c v") #'(lambda ()
                                       (interactive)
                                       (anything 'anything-c-source-w3m-bookmarks)))

(define-key w3m-mode-map (kbd "C-c M") 'w3m-view-this-page-in-chrome)

;; «org-config» (to ".org-config")
(require 'org-config-thierry)

;; «muse-config» (to ".muse-config")

(add-to-list 'load-path "~/elisp/muse/lisp")
(add-to-list 'load-path "~/elisp/muse/contrib")
(require 'muse-autoloads)
(add-hook 'find-file-hooks 'muse-mode-maybe)
(require 'muse-mode)     ; load authoring mode
(require 'muse-wiki)
(setq muse-wiki-allow-nonexistent-wikiword t)
(require 'muse-html)     ; load publishing styles I use
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)
(require 'muse-colors)
(require 'htmlize-hack)


;; «erc-config» (to ".erc-config")

(defun erc-freenode-connect ()
  (interactive)
  (let ((erc-auth
         (auth-source-user-or-password  '("login" "password")
                                        "irc.freenode.net:6667"
                                        "erc")))
    (erc :server   "irc.freenode.net"
         :port     "6667"
         :nick     (car erc-auth)
         :password (cadr erc-auth))))

(global-set-key (kbd "<f7> i") 'erc-freenode-connect)
         
(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs"
                                     "#gentoo-lisp"
                                     "#stumpwm")))

;; «Bitlbee» (to ".Bitlbee")

(defun bitlbee (server)
  "Connect to a Bitlbee server.
Actually i am registered on im.uk.bitlbee for talk.google.com.
Localhost is used for yahoo messenger.
I will have to register <password> on others
and also to add an account with
account add <protocol> moi@mail.com password."
  (interactive (list (completing-read "Choose a Bitlbee Server: "
                                      '("im.bitlbee.org:6667"
                                        "testing.bitlbee.org:6667"
                                        "im.rootdir.de:6668"
                                        "im.uk.bitlbee.org:6667"
                                        "irc.net:6667"
                                        "bitlbee1.asnetinc.net:6667"
                                        "bitlbee.hensema.net:6667"
                                        "bitlbee.extreme-players.de:6667"
                                        "irc2im.picasa.hu:6667"
                                        "im.sixxs.net:6667"
                                        "im.kernel-oops.de:7777"
                                        "im.rondom.org:7070"
                                        "im.okkernoot.net:6667"
                                        "im.codemonkey.be:6667"
                                        "im.se.bitlbee.org:6667"
                                        "localhost:6667")
                                      nil t nil nil "im.uk.bitlbee.org:6667")))
  
  (let ((server-sans-port (replace-regexp-in-string ":[0-9]*" "" server))
        (port             (when (string-match "[0-9]*" server) (match-string 1)))
        (bitlb-auth       (auth-source-user-or-password '("login" "password")
                                                        "im.uk.bitlbee.org:6667"
                                                        "bitlbee")))
    (erc :server   server-sans-port
         :port     port
         :nick     (car bitlb-auth)
         :password (cadr bitlb-auth))))

(global-set-key (kbd "<f7> g") 'bitlbee)

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)

;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

;; «Lisppaste» (to ".Lisppaste")
(require 'lisppaste-extension)

;; Don't use RET to send line
;; (define-key erc-mode-map (kbd "RET") nil)
;; (define-key erc-mode-map (kbd "C-c RET") 'erc-send-current-line)

;; «subversion» (to ".subversion")

(require 'psvn)
(global-set-key (kbd "<f5> s") 'svn-status)

;; «DVC-config» (to ".DVC-config")
(require 'dvc-init)

;; «winner-mode-config» (to ".winner-mode-config")

(winner-mode 1)
(setq winner-boring-buffers '("*Completions*"
                              "*Compile-Log*"
                              "*Traverse-directory*"
                              "*Apropos*"
                              "*anything*"
                              "*anything complete*"
                              "*Anything Occur*"
                              "*Help*"
                              "*Buffer List*"
                              "*Ibuffer*"
                              "*xhg-diff*"
                              "*xhg-commit*"
                              "*xhg-process*"
                              "*xhg-"
                              "*xhg-<2>"
                              "*xhg-<3>"
                              "*xhg-<4>"
                              "*xhg-pull*"
                              "*xhg-pull*<2>"
                              "*xhg-pull*<3>"
                              "*xhg-pull*<4>"
                              "*xhg-error*"
                              "*xhg-error*<2>"
                              "*xhg-error*<3>"
                              "*xhg-error*<4>"
                              ))

;; At this level save window config to register
;;(window-configuration-to-register 119 nil)

;; Show 2 level of dir in the paths of buffer-filenames
;; (when (require 'uniquify)
;;   (setq uniquify-min-dir-content 1)
;;   (setq uniquify-buffer-name-style 'forward))

;; «load-emms» (to ".load-emms")

;; (find-fline "~/.emacs.d/emacs-config-laptop/emms-mpd-config.el" "INDEX")

;(require 'emms-alsaplayer-config)
(require 'emms-mpd-config)
(define-key dired-mode-map (kbd "C-c p d") 'emms-play-dired)

;; «ibuffer-config» (to ".ibuffer-config")

(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; «dired-tar» (to ".dired-tar")
(require 'dired-tar)

(add-hook 'dired-mode-hook
          (function
           (lambda ()
             (define-key dired-mode-map "J" 'dired-tar-pack-unpack))))

;; «confirm-quit-emacs» (to ".confirm-quit-emacs")
(setq confirm-kill-emacs 'y-or-n-p)

;; «consequent-log-file» (to ".consequent-log-file")
(setq message-log-max 1000)

;; «Add-newline-at-end-of-files» (to ".Add-newline-at-end-of-files")
(setq require-final-newline t)

;; «No-startup-screen» (to ".No-startup-screen")
(setq inhibit-startup-message t)

;; «Message-displayed-in-scratch-on-startup» (to ".Message-displayed-in-scratch-on-startup")
;; Take effect only on last emacs versions unless
;; inhibit-startup-(message/screen) is nil.
(setq initial-scratch-message (purecopy "\
;; SCRATCH BUFFER\n;; ==============

"))

;; «Ediff-config» (to ".Ediff-config")
;; «Silly-little-window-in-ediff» (to ".Silly-little-window-in-ediff")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;(setq ediff-window-setup-function 'ediff-setup-windows-multiframe)
;; (find-evardescr 'ediff-window-setup-function)
(global-set-key (kbd "<f5> e f") 'ediff-files)
(global-set-key (kbd "<f5> e b") 'ediff-buffers)
(global-set-key (kbd "<f5> e m b") 'ediff-merge-buffers)
(global-set-key (kbd "<f5> e m f") 'ediff-merge-files)

;; «highlight-current-line» (to ".highlight-current-line")
(global-set-key "\C-c-h" 'hl-line-mode)

;; «yaoddmuse» (to ".yaoddmuse")
(require 'yaoddmuse)
(setq yaoddmuse-username "ThierryVolpiatto")
(setq yaoddmuse-directory "/home/thierry/.emacs.d/yaoddmuse")

;; «Dired» (to ".Dired")
;; use the directory in the other windows as default target
(setq dired-dwim-target t)
(require 'dired-details+)
(setq dired-details-initially-hide nil)

;; «wdired» (to ".wdired")
;; (when (require 'wdired nil t)
;;   (define-key dired-mode-map (kbd "C-c C-r") 'wdired-change-to-wdired-mode))
;; since emacs-23.0.90 it is bind by default to C-x C-q

;; «yes-or-no» (to ".yes-or-no")
(fset 'yes-or-no-p 'y-or-n-p)

;; «Affiche-l'heure-au-format-24h» (to ".Affiche-l'heure-au-format-24h")
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(setq display-time-use-mail-icon t)

;; «Battery» (to ".Battery")

(require 'battery)
(run-with-timer "2" 60 #'(lambda ()
                           (if (equal (cdr (assoc 76 (battery-linux-proc-acpi)))
                                      "on-line")
                               (setq battery-mode-line-format "[%b%p%%,%d°C,%L]")
                             (setq battery-mode-line-format "[%b%p%%,%d°C,%t]"))))
(display-battery-mode)

;; «Wifi-wireless.el» (to ".Wifi-wireless.el")
(defun tv-wireless-info ()
  "Start wireless only if ipw is up"
  (condition-case nil
      (progn
        (require 'wireless)
        (display-wireless-mode t))
    (error nil)))

;(tv-wireless-info)

;; «Limite-max-lisp» (to ".Limite-max-lisp")
(setq max-lisp-eval-depth '40000)
(setq max-specpdl-size '100000)

;; «emacs-backup-config» (to ".emacs-backup-config")
;; Backup
(setq backup-directory-alist '(("" . "/home/thierry/.emacs.d/emacs_backup"))
      backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 20
      delete-old-versions t)
(setq tramp-backup-directory-alist backup-directory-alist)
(setq auto-save-file-name-transforms nil)


;; Eval==> (describe-variable 'case-fold-search)
(setq case-fold-search t)

;; «Transient-mark-mode» (to ".Transient-mark-mode")
(transient-mark-mode 1) ; the function
;; «show-paren-mode» (to ".show-paren-mode")
(show-paren-mode 1)

;; «Start-emacs-server» (to ".Start-emacs-server")

;; (find-fline "~/labo/anything-hg-qpatch/anything-config.el" "anything-get-pid-from-process-name")

(add-hook 'after-init-hook #'(lambda ()
                               (server-start)
                               (setq server-raise-frame t)))

;; «emacsclient-and-stumpish» (to ".emacsclient-and-stumpish")
;; When using emacsclient from external programs, raise emacs and come back
;; to external program when finish
(if window-system
    (add-hook 'server-done-hook
              (lambda ()
                (shell-command "stumpish 'eval (stumpwm::return-es-called-win stumpwm::*es-win*)'"))))

;; «Path-to-abbrev-file» (to ".Path-to-abbrev-file")
(setq abbrev-file-name "/home/thierry/.emacs.d/.abbrev_defs")

;; «Compare-windows» (to ".Compare-windows")
(global-set-key (kbd "C-c w") 'compare-windows)

;; «Copy-and-cut-to-x-clipboard» (to ".Copy-and-cut-to-x-clipboard")
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; «Google-define» (to ".Google-define")
;; define a word with google
;; (require 'google-define)
;; (global-set-key (kbd "<f5> g") 'google-define)

;; «text-translator» (to ".text-translator")

(add-to-list 'load-path "/home/thierry/elisp/text-translator/")
(require 'text-translator-load)
(setq text-translator-default-engine "google.com_jaen")
(global-set-key (kbd "<f5> t r") 'text-translator)

;; «html» (to ".html")
;; «htmlize» (to ".htmlize")
(require 'htmlize)

;; «htmlfontify» (to ".htmlfontify")
(when (window-system)
  (require 'htmlfontify))

;; «Whitespace-mode» (to ".Whitespace-mode")
;(require 'whitespace)
(global-set-key (kbd "C-c W") 'whitespace-mode)

;; «Emacs-customize-have-it's-own-file» (to ".Emacs-customize-have-it's-own-file")
(setq custom-file "/home/thierry/.emacs.d/.emacs-custom.el")
(load custom-file)

;;Pour les versions cvs d'emacs(correct bug)
;;(setq dframe-xemacsp nil)


;; «Bind-comint-dynamic-complete» (to ".Bind-comint-dynamic-complete")
(global-set-key (kbd "M-²") 'comint-dynamic-complete)

;; «regex-tool» (to ".regex-tool")
(require 'regex-tool)
(global-set-key (kbd "<f11> r") 'regex-tool)

;; «antiword» (to ".antiword")

(require 'no-word)
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))


(global-set-key (kbd "<f5> l") 'locate)


;; «Elisp» (to ".Elisp")

;; «Eldoc» (to ".Eldoc")

(add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                    (outline-minor-mode 1)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'(lambda ()
                                          (outline-minor-mode 1)))
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)

;; «Indent-when-newline» (to ".Indent-when-newline")
;; (RET) in all elisp modes
(define-key lisp-interaction-mode-map (kbd "RET") 'newline-and-indent)
(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)
(define-key lisp-mode-map (kbd "RET") 'newline-and-indent)

;; «eval-region» (to ".eval-region")
(define-key lisp-interaction-mode-map (kbd "C-M-!") 'tv-eval-region)
(define-key emacs-lisp-mode-map (kbd "C-M-!") 'tv-eval-region)

;; «byte-compile-file» (to ".byte-compile-file")
(define-key emacs-lisp-mode-map (kbd "C-c C-c b") 'byte-compile-file)

;; «Indent-only-with-spaces» (to ".Indent-only-with-spaces")
(setq-default indent-tabs-mode nil)

;; «Lua-mode» (to ".Lua-mode")
(add-to-list 'load-path "/home/thierry/elisp/lua-mode")
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
(add-hook 'lua-mode-hook 'hs-minor-mode)

;; «Python-config» (to ".python-config")

;; «terminal-ipython» (to ".terminal-ipython")
(require 'ipython)
;(setenv "PYTHONSTARTUP" "/home/thierry/.pythonstartup")
;; (define-key py-shell-map (kbd "\t") 'ipython-complete)
;; (setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n")

;; «Pymacs» (to ".Pymacs")
(setenv "PYMACS_PYTHON" "python2.6") 
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; «Pycomplete» (to ".Pycomplete")
(require 'pycomplete)

;; «config-python-mode» (to ".config-python-mode")
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("ipython" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; «Search-in-python-library» (to ".Search-in-python-library")
;; (find-fline "~/.emacs.d/emacs-config-laptop/.emacs-config-w3m.el" "tv-python-search")
(define-key py-mode-map (kbd "<f7> s p") 'tv-python-search)

;;pdb==> python debugger (installation de gdb neccessaire)
(setq gud-pdb-command-name "/home/thierry/bin/pdb.py")
(add-to-list 'load-path "/home/thierry/elisp/pdb/")
(load "pdbtrack.el")

;; «Pylint-via-flymake» (to ".Pylint-via-flymake")

;;fonctionne avec le script python /usr/local/bin/epylint
;;initialisation de flymake

(require 'flymake)
(defvar flymake-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f4> f e") 'flymake-display-err-menu-for-current-line)
    (define-key map (kbd "<f4> f n") 'flymake-goto-next-error)
    map)
  "Keymap used for flymake commands.")

(global-set-key (kbd "M-<f4>") 'flymake-mode)

;; «Flymake-pour-python» (to ".Flymake-pour-python")
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
       (local-file (file-relative-name
                    temp-file
                    (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; «Entete-py» (to ".Entete-py")
(defun tv-insert-python-header ()
  "insert python header at point"
  (interactive)
  (insert "#!/usr/bin/env python\n"
          "# -*- coding: utf-8 -*-\n\n"
          "## Title: \n"
          "## Description: \n"
          "## Author:Thierry Volpiatto<thierry dot volpiatto FROM gmail DOT com>\n"
          "## Commentary:\n\n"))

(global-set-key (kbd "C-c e p") 'tv-insert-python-header)

;; «shell-config» (to ".shell-config")
(require 'ansi-color)

;; Set `undo-outer-limit' to hight value to avoid messages when gentoo emerge
(setq undo-outer-limit 5000000)

;; «prompt-shell-read-only» (to ".prompt-shell-read-only")
(setq comint-prompt-read-only t)

;; «couleur-dans-le-shell» (to ".couleur-dans-le-shell")
;; (j'ai ajouté dumb dans /etc/LS_COLOR egalement)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; «newline-and-indent-in-sh-mode» (to ".newline-and-indent-in-sh-mode")

(add-hook 'sh-mode-hook #'(lambda ()
                            (define-key sh-mode-map (kbd "RET") 'newline-and-indent)))

;; «Dirtrack-mode» (to ".Dirtrack-mode")
;; (find-efunctiondescr 'dirtrack-mode)
(dirtrack-mode 1)

;; «Eshell-config» (to ".Eshell-config")
(require 'em-xtra)

;; «Eshell-prompt» (to ".Eshell-prompt")
(setq eshell-prompt-function
      (lambda nil
        (concat
         (getenv "USER")
         "@"
         (eshell/pwd)
         (if (= (user-uid) 0) " # " " $ "))))
(add-hook 'eshell-mode-hook #'(lambda ()
                                (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue")))

;; «Eshell-banner» (to ".Eshell-banner")
(setq eshell-banner-message "Welcome Thierry on the Emacs shell\n\n")

;; «Eshell-et-ansi-color» (to ".Eshell-et-ansi-color")
(ignore-errors
  (dolist (i (list 'eshell-handle-ansi-color
                   'eshell-handle-control-codes
                   'eshell-watch-for-password-prompt))
    (add-to-list 'eshell-output-filter-functions i)))

;(setq eshell-password-prompt-regexp "[Pp]ass\\(word\\|phrase\\)\\|[Mm]ot de passe.*:\\s *\\'")

;; «Eshell-save-history-on-exit» (to ".Eshell-save-history-on-exit")
;; Possible values: t (always save), 'never, 'ask (default)

(setq eshell-save-history-on-exit t)

;; «Eshell-directory» (to ".Eshell-directory")
(setq eshell-directory-name "/home/thierry/.emacs.d/.eshell/")

;; «Eshell-command» (to ".Eshell-command")
(global-set-key (kbd "C-!") 'eshell-command)

;; «Eshell-toggle» (to ".Eshell-toggle")
(require 'esh-toggle)
(global-set-key (kbd "<f11> e c") 'eshell-toggle-cd)
(global-set-key (kbd "<f11> e t") 'eshell-toggle)

;; «eshell-visual» (to ".eshell-visual")
(setq eshell-term-name "eterm-color")
(when (require 'em-term)
  (dolist (i '("kop" "ledger"
               "mc" "htop"))
    (add-to-list 'eshell-visual-commands i)))

;; «Term-et-ansi-term» (to ".Term-et-ansi-term")
(defvar term-prompt-regexp)
(add-hook 'term-mode-hook
          (function
           (lambda ()
             (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
             (make-local-variable 'mouse-yank-at-point)
             (make-local-variable 'transient-mark-mode)
             (setq mouse-yank-at-point t)
             (setq transient-mark-mode nil)
             (auto-fill-mode -1)
             (setq tab-width 8 ))))

(defun tv-term ()
  (interactive)
  (ansi-term "/bin/bash"))

;; «Keys-to-access-different-consoles» (to ".Keys-to-access-different-consoles")
(global-set-key (kbd "<f11> s h") 'shell)
(global-set-key (kbd "<f11> t") 'tv-term)
(global-set-key (kbd "<f11> i") 'ielm)
(global-set-key (kbd "<f11> p") 'py-shell)

;;«shell-command-completion» (to ".shell-command-completion")
(require 'shell-command)
(shell-command-completion-mode)

;; «Entete-Bash» (to ".Entete-Bash")
(defun tv-insert-bash-header ()
  "insert bash header at point"
  (interactive)
  (insert "#!/bin/bash\n"
          "## Title:\n"
          "## Description: \n"
          "## Author:Thierry Volpiatto<thierry dot volpiatto FROM gmail DOT com>\n"
          "## Commentary:\n\n"))

(global-set-key (kbd "C-c e b") 'tv-insert-bash-header)



;; «Calculator» (to ".Calculator")


;; «Binding-pour-le-calculateur-d'emacs» (to ".Binding-pour-le-calculateur-d'emacs")
(global-set-key [(control return)] 'calculator)

;; «flyspell-aspell» (to ".flyspell-aspell")

;; «Aspell» (to ".Aspell")
(setq-default ispell-program-name "aspell")

(setq ispell-local-dictionary "francais")

;; «Switch-dico-english-french» (to ".Switch-dico-english-french")
(defun tv-change-dico ()
  "change de dictionnaire"
  (interactive)
  (let ((dico (or ispell-local-dictionary ispell-dictionary)))
    (setq dico (if (string= dico "francais")
                   "english"
                   "francais"))
    (message "Switched to %s" dico)
    (sit-for 0.5)
    (ispell-change-dictionary dico)
    (when flyspell-mode
      (flyspell-delete-all-overlays))))

;; «Toggle-flyspell-mode» (to ".Toggle-flyspell-mode")

(global-set-key (kbd "<f2> f") 'flyspell-mode)
(global-set-key (kbd "<f2> c") 'tv-change-dico)

;; «woman» (to ".woman")

(require 'woman)
(setq woman-use-own-frame nil)
(global-set-key (kbd "<f11> w") 'woman)

;; «printing-config» (to ".printing-config")

;; «Gtklp-par-defaut» (to ".Gtklp-par-defaut")
(setq lpr-command "gtklp")
(setq-default ps-print-header nil)
(set-variable 'lpr-switches '("-Pepson"))
(setq ps-font-size   '(10 . 11.5))
(setq ps-font-family 'Courier)

(defun tv-ps-print-buffer ()
  (interactive)
  (if current-prefix-arg
      (ps-print-buffer-with-faces)
      (ps-print-buffer)))

(defun tv-ps-print-region (beg end)
  (interactive "r")
  (if current-prefix-arg
      (ps-print-region-with-faces beg end)
      (ps-print-region beg end)))

(global-set-key (kbd "<f5> p s b") 'tv-ps-print-buffer)
(global-set-key (kbd "<f5> p s r") 'tv-ps-print-region)
(global-set-key (kbd "<f5> p b") 'print-buffer)
(global-set-key (kbd "<f5> p r") 'print-region)

;; (require 'printing)
;; (pr-update-menus)



;; «ext-view-config» (to ".ext-view-config")

;;utilisation de programmes externes avec extview.el via mailcap
(require 'extview)
(push '("\\.py$" . nil) extview-application-associations)
(push '("\\.sh$" . nil) extview-application-associations)
(push '("\\.py~$" . nil) extview-application-associations)
(push '("\\.sh~$" . nil) extview-application-associations)
(push '("\\.html$" . ask) extview-application-associations)
(push '("\\.jpeg$" . nil) extview-application-associations)
(push '("\\.jpg$" . nil) extview-application-associations)
(push '("\\.png$" . nil) extview-application-associations)
(push '("\\.gif$" . nil) extview-application-associations)
(push '("\\.tiff$" . nil) extview-application-associations)
(push '("\\.ogg$" . ask) extview-application-associations)
(push '("\\.mp3$" . ask) extview-application-associations)
(push '("\\.wav$" . ask) extview-application-associations)
(push '("\\.m3u$" . nil) extview-application-associations)
(push '("\\.tcl$" . nil) extview-application-associations)
(push '("\\.pls$" . nil) extview-application-associations)
(push '("\\.tex$" . nil) extview-application-associations)
(push '("\\.css$" . nil) extview-application-associations)
(push '("\\.xml$" . ask) extview-application-associations)
(push '("\\.ps$" . ask) extview-application-associations)
(push '("\\.*~$" . nil) extview-application-associations)
(push '("\\.h$" . nil) extview-application-associations)
(push '("\\.vcf$" . nil) extview-application-associations)
(push '("\\.svg$" . nil) extview-application-associations)
(push '("\\.dat$" . nil) extview-application-associations)
(push '("\\.patch$" . nil) extview-application-associations)
(push '("\\.pl$" . nil) extview-application-associations)
(push '("\\.flv$" . ask) extview-application-associations)
(push '("\\.cpp$" . nil) extview-application-associations)
(push '("\\.csv$" . nil) extview-application-associations)
(push '("\\.ogv$" . "mplayer %s") extview-application-associations)

;; «auto-compression-mode» (to ".auto-compression-mode")

(auto-compression-mode 1)

;; «auctex-config» (to ".auctex-config")

(require 'tex-site)


;; To turn on RefTeX Minor Mode for all LaTeX files,

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

;; Replace AUCTeX functions

(setq reftex-plug-into-AUCTeX t)

(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

;;parametres latex divers

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'TeX-language-fr-hook
               (lambda () (ispell-change-dictionary "french")))

;; «Insertion-d'un-squelette-latex» (to ".Insertion-d'un-squelette-latex")

(defun tv-insert-skel-latex-doc ()
  "Insert a LaTeX skeleton in an empty file."
  (interactive)
  (insert "\\documentclass[a4paper,11pt]{article}\n"
       "\\usepackage[french]{babel}\n"
       "\\usepackage[utf8]{inputenc}\n"
       "\\usepackage{textcomp}% Allow to use euro sign\n"
       "\n"
       "%\\usepackage[pdftex=true,
           %hyperindex=true,
           %colorlinks=true]{hyperref}"
       "\n"
       "%\\usepackage[hypertex=true,
           %hyperindex=true,
           %colorlinks=false]{hyperref}"
       "\n"
       "%\\usepackage{url}\n"
       "%\\usepackage{natbib}\n"
       "%\\usepackage{setspace}\n"
       "%\\usepackage{qtree}\n"
       "%\\usepackage{booktabs}\n"
       "\n"
       "\n"
       "\\begin{document}\n"
       "%\n"
       "%\\begin{titlepage}\n"
       "\\title{}\n"
       "\\date{\\today}\n"
       "\\author{}\n"
       "\\maketitle\n"
       "%\\tableofcontents\n"
       "%\\end{titlepage}\n"
       "\n"
       "\n"
       "\\end{document}\n")
  (goto-char (point-min))
  (when (re-search-forward "[\\]title")
    (beginning-of-line)
    (forward-char 7)))

;; «Insertion-d'un-squelette-latex-de-lettre» (to ".Insertion-d'un-squelette-latex-de-lettre")

(defun tv-insert-skel-latex-letter ()
  "Insert a latex skeleton letter in an empty file"
  (interactive)
  (insert "\\documentclass[a4paper,11pt]{letter}\n"
       "\\usepackage[french]{babel}\n"
       "\\usepackage[utf8]{inputenc}\n"
       "\\usepackage{textcomp}% Allow to use euro sign\n"
       "\\begin{document}\n"
       "%\\name{}% Nom de l'expéditeur\n"
       "\\address{Thierry Volpiatto \\\\ 141 Carraire des Lecques \\\\ 83270 St Cyr sur mer}% Adresse de l'expéditeur\n"
       "\\signature{Thierry Volpiatto}% Signature de l'expéditeur\n"
       "\\date{\\today}\n"
       "\n"
       "\n"
       "\\begin{letter}{}% Nom du destinataire\n"
       "\\opening{}% Formule de salutation : cher monsieur, etc.\n"
       "\n"
       "% Corps de la lettre\n"
       "\n"
       "\\closing{}% Formule de politesse : veuillez agréer, etc.\n"
       "\\ps{PS:}{}% Post-scriptum\n"
       "\\cc{}% Autres destinataires de la lettre\n"
       "\\encl{}% Pièces jointes\n"
       "\\end{letter}\n"
       "\\end{document}\n")
  (goto-char (point-min))
  (when
      (re-search-forward "[\\]begin\{letter\}")
    (beginning-of-line)
    (forward-char 15)))


;; «ledger-config» (to ".ledger-config")

(require 'ledger)

(define-key ledger-mode-map (kbd "C-c a l") 'ledger-align-device)
(setq ledger-default-device "€")

;; «Align-euro-device» (to ".Align-euro-device")
(defun ledger-align-device (&optional column)
  (interactive "p")
  (if (= column 1)
      (setq column 48))
  (while (search-forward ledger-default-device nil t)
    (backward-char)
    (let ((col (current-column))
          (beg (point))
          target-col len)
      (skip-chars-forward (concat "-" ledger-default-device "0-9,."))
      (setq len (- (point) beg))
      (setq target-col (- column len))
      (if (< col target-col)
          (progn
            (goto-char beg)
            (insert (make-string (- target-col col) ? )))
        (move-to-column target-col)
        (if (looking-back "  ")
            (delete-char (- col target-col))
          (skip-chars-forward "^ \t")
          (delete-horizontal-space)
          (insert "  ")))
      (forward-line))))

;; «ledger-position-at-point» (to ".ledger-position-at-point")
(defun ledger-position-at-point ()
  (interactive)
  (let* ((bal (with-temp-buffer
               (apply #'call-process "ledger" nil t nil
                      (list "-C" "bal" "socgen"))
               (split-string (buffer-string) "\n" t)))
         (result (car (last bal))))
    (string-match "€ [0-9.]*" result)
    (setq result (match-string 0 result))
    (insert (concat "[" result "]"))))

;; «newsticker-config» (to ".newsticker-config")
(setq newsticker-frontend 'newsticker-plainview)
(setq newsticker-show-descriptions-of-new-items nil)
(setq newsticker-html-renderer 'w3m-region)
(global-set-key (kbd "<f7> n") 'newsticker-show-news)

(defun newsticker-quit-and-stop ()
  (interactive)
  (with-current-buffer "*newsticker*" 
    (newsticker-close-buffer)
    (newsticker-stop)))
(when (require 'newsticker)
  (define-key newsticker-mode-map (kbd "Q") 'newsticker-quit-and-stop))

;; «Tramp-config» (to ".Tramp-config")
(partial-completion-mode t)
(require 'tramp)
;;methode par defaut
(setq tramp-default-method "ssh")


;; «Mode-lecture-photo-auto» (to ".Mode-lecture-photo-auto")
(auto-image-file-mode 1)

;; «boxquote-config» (to ".boxquote-config")

(require 'boxquote)
(global-set-key (kbd "<f7> q f") 'boxquote-describe-function)
(global-set-key (kbd "<f7> q v") 'boxquote-describe-variable)
(global-set-key (kbd "<f7> q k") 'boxquote-describe-key)
(global-set-key (kbd "<f7> q r") 'boxquote-region)
(global-set-key (kbd "<f7> q u") 'boxquote-unbox-region)
(global-set-key (kbd "<f7> q t") 'boxquote-title)
(global-set-key (kbd "<f7> q c") 'boxquote-copy-box-without-box)
;; (find-fline "~/.emacs.d/emacs-config-laptop/tv-utils.el" "defun boxquote-copy-box-without-box")

;; «bbdb-config» (to ".bbdb-config")

(require 'bbdb)
(require 'bbdb-gui)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
(setq bbdb-file-coding-system 'utf-8)
(setq bbdb-default-country "France")
(setq bbdb-expand-mail-aliases t)
(setq bbdb-file "/home/thierry/mail-system-gnus/.bbdb")
(global-set-key (kbd "<f7> b f") 'bbdb)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)
(setq bbdb-send-mail-style 'message)

;;pour créer un nouveau contact dans bbdb
(global-set-key (kbd "<f7> b n") 'bbdb-create)

;;pour enregistrer le contact sender
(global-set-key (kbd "<f7> b s") 'bbdb/gnus-annotate-sender)

;;pour completer l'adresse
(global-set-key (kbd "<f7> b c") 'bbdb-complete-name)

;; par defaut bbdb est configure pour accepter les numeros de
;; telephone au format americain ici on desactive cette option
(setq bbdb-north-american-phone-numbers-p nil)

;; permet d'eviter d'avoir une fenetre bbdb qui montre en permanence
;; les mises a jour dans bbdb lorsque l'on utilise GNUS
(setq bbdb-use-pop-up nil)

;; pas de code de localisation par defaut pour les numeros de
;; telephone
(setq bbdb-default-area-code nil)

;; permet d'empecher a bbdb de creer une nouvelle entree a chaque fois
;; qu'un mail d'une nouvelle personne est lu avec GNUS, RMAIL, VM ou
;; MH.
(setq bbdb/mail-auto-create-p nil)

;; nombre de lignes desire dans la fenetre popup de bbdb lorsque l'on
;; utilise VM/MH/RMAIL ou GNUS.
(setq bbdb-pop-up-target-lines 7)

;;ne check pas le code postal
(setq bbdb-check-zip-codes-p nil)

;;bbdb-print pour faire un carnet en latex(seem broken)

(add-hook 'bbdb-load-hook (function (lambda () (require 'bbdb-print))))
(setq bbdb-print-file-name "/home/thierry/latex/bbdb.tex")
(setq bbdb-print-require 'name)
;; (setq bbdb-print-omit-fields '(omit tex-name aka mail-alias creation-date timestamp))
(setq bbdb-print-full-alist
      '((columns . 2)
        (font-size . 9)
        (separator . 2)
        (include-files "/home/thierry/bbdbtex/bbdb-print.tex"
                       "/home/thierry/bbdbtex/bbdb-cols.tex")))

;; «slime-config» (to ".slime-config")

(add-to-list 'load-path "/home/thierry/elisp/slime")
(add-to-list 'load-path "/home/thierry/elisp/slime/contrib")
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime-autoloads)
(require 'slime)
(setq slime-backend "/home/thierry/elisp/slime/swank-loader.lisp")
(slime-setup '(slime-fancy
               slime-asdf
               slime-tramp
               slime-banner
               slime-autodoc
               slime-xref-browser))
(setq slime-net-coding-system 'utf-8-unix
      lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(add-hook 'slime-load-hook (lambda () (require 'slime-tramp)))

(defun tv-slime-port (process)
  (let ((slime-port (or (process-id process)
                        (process-contact process))))
    (setq slime-port (cadr slime-port))
    slime-port))

(defun tv-slime-stump-p ()
  (catch 'break
    (dolist (p slime-net-processes)
      (when (= 4004 (tv-slime-port p))
        (throw 'break t)))))

(defun tv-get-slime-buffer-list ()
  (let ((buf-list nil))
    (dolist (b (buffer-list))
      (when (string-match "*slime-repl sbcl*" (buffer-name b))
        (push (buffer-name b) buf-list)))
    buf-list))

(defun tv-start-slime ()
  (interactive)
  (if (slime-connected-p)
      (if (and (< (length slime-net-processes) 2)
               (tv-slime-stump-p))
          (slime)
        (slime-list-connections))
    (slime)))

(defun tv-slime-connect-to-stump ()
  (interactive)
  (if (slime-connected-p)
      (if (and (< (length slime-net-processes) 2)
               (not (tv-slime-stump-p)))
          (slime-connect "127.0.0.1" 4004)
        (slime-list-connections))
    (slime-connect "127.0.0.1" 4004)))

(global-set-key (kbd "<f11> l r") 'tv-start-slime)
(global-set-key (kbd "<f11> l e") 'slime-scratch)
(global-set-key (kbd "<f11> l s") 'tv-slime-connect-to-stump)
(global-set-key (kbd "<f11> l l") 'slime-list-connections)

;; «common-lisp-info» (to ".common-lisp-info")
(require 'cl-info)
(setq Info-additional-directory-list '("~/elisp/info/gcl-info/"))

;; «Save-slime-scratch-buffer» (to ".Save-slime-scratch-buffer")
(setq slime-scratch-file "~/.emacs.d/slime-scratch.lisp")

;; «Stumpwm-mode» (to ".Stumpwm-mode")
(require 'stumpwm-mode)

;; «Else-mode» (to ".Else-mode")
(add-to-list 'load-path "~/elisp/else-mode/")
(require 'else-mode)
;(add-hook 'python-mode-hook (lambda () (else-mode 1)))

;; «mozilla-javascript» (to ".mozilla-javascript")
;; Javascript and mozilla (interaction with firefox)
(require 'moz)
(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
(autoload 'espresso-mode "espresso" nil t)
;(add-to-list 'auto-mode-alist '("\\.js$" . java-mode))
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'java-mode-hook 'java-custom-setup)
(defun java-custom-setup ()
  (moz-minor-mode 1))

;; «warn-mail» (to ".warn-mail")

(require 'warn-mail)

(setq mail-list-to-watch (list
                          "/home/thierry/incoming/default"
                          "/home/thierry/incoming/friends"
                          "/home/thierry/incoming/gentoo"
                          "/home/thierry/incoming/developpez"
                          "/home/thierry/incoming/AFPY"
                          "/home/thierry/incoming/gna"
                          "/home/thierry/incoming/gentoo-user-fr"
                          "/home/thierry/incoming/gentoo-dev"
                          "/home/thierry/incoming/planner-el-discuss"
                          "/home/thierry/incoming/help-gnu-emacs"
                          "/home/thierry/incoming/junk"
                          "/home/thierry/incoming/spam"
                          "/home/thierry/incoming/probably-spam"
                          "/home/thierry/incoming/almost-certainly-spam"
                          "/home/thierry/incoming/IN.virus"))

(setq message-mode-line t
      message-mode-line-icon t
      display-time-mail-string "  ")

;; si fetchmail est lancé en crontab je lance que warn-mail:
;;(tv-launch-warn-without-fetch)

(global-set-key (kbd "C-c f m") 'tv-launch-mail-system)
(global-set-key (kbd "C-c q f m") 'tv-stop-warn-and-fetch)


;; «thumb-page-config» (to ".thumb-page-config")
(require 'thumb-page)
(setq host-url "http://thievol.homelinux.org:2222")
(global-set-key (kbd "C-c t p") 'muse-write-thumb-table)

;; «traverselisp-config» (to ".traverselisp-config")

;; (find-fline "~/labo/traverse-hash-qpatch/traverselisp.el")

;(require 'traverselisp "~/labo/traverse-hg-qpatch/traverselisp.elc")

(require 'traverselisp)
(setq traverse-use-avfs t)
(global-set-key (kbd "<f5> f") 'traverse-deep-rfind)
(global-set-key (kbd "<f5> u") 'traverse-build-tags-in-project)
(global-set-key (kbd "C-c o") 'traverse-occur-current-buffer)
(define-key dired-mode-map (kbd "A") 'traverse-dired-search-regexp-in-anything-at-point)
(define-key dired-mode-map (kbd "C-c C-z") 'traverse-dired-browse-archive)
(define-key dired-mode-map (kbd "C-c t") 'traverse-dired-find-in-all-files)
(mapc #'(lambda (x)
          (add-to-list 'traverse-ignore-files x))
      '(".ledger-cache"  "ANYTHING-TAG-FILE"))
(add-to-list 'traverse-ignore-dirs "emacs_backup")
(global-set-key (kbd "C-c C-g") 'anything-traverse)

;; (find-fline "~/labo/traverse-hash-qpatch/traverselisp.el" "traverse-window-split-h-or-t")
(global-set-key (kbd "C-M-|") 'traverse-toggle-split-window-h-v)

;; «dar-backup-rules» (to ".dar-backup-rules")

(require 'dar)
(setq dar-backup-rules-lisp-file "/home/thierry/.emacs.el")

;; Rules
(setq dar-backup-rules
      '((all
         (backup-dir "~/bak/dar")
         (log-file "~/bak/dar/dar-el.log")
         (backup-interval-differential daily)
         (backup-interval-full monthly))
        (create
         (compress bzip2))
        ("Thierry-org"
         (root "/home/thierry/org/")
         (backup-interval-differential daily)
         (backup-interval-full weekly))
        ("Thierry-work"
         (root "/home/thierry/labo/")
         (backup-interval-differential daily)
         (backup-interval-full weekly))
        ("Thierry-finance"
         (root "/home/thierry/finance/")
         (backup-interval-differential daily)
         (backup-interval-full weekly))
        ("Thierry-dvc-bookmarks"
         (root "/home/thierry/.dvc")
         (backup-interval-differential daily)
         (backup-interval-full weekly))
        ("Emacs-config"
         (root "/home/thierry/.emacs.d/emacs-config-laptop/")
         (backup-interval-differential daily)
         (backup-interval-full weekly)
        )))

;; Timer
(defun dar-backup-schedule-backup ()
  (message "dar-backup-schedule-backup run at %s" (current-time-string))
  (run-at-time "8:30am" nil 'dar-backup-all-rule-sets))

(require 'midnight)
(midnight-delay-set 'midnight-delay 4800) ;; 8:00am default is 3600==>6:00am
(add-hook 'midnight-hook 'dar-backup-schedule-backup)

(global-set-key (kbd "<f5> d b") 'dar-backups)


;; «Enable-commands-disabled-by-default» (to ".Enable-commands-disabled-by-default")
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'dired-find-alternate-file 'disabled nil) ; a
(put 'scroll-left 'disabled nil) ; C-x > or <
(put 'downcase-region 'disabled nil) ; C-x C-l
(put 'upcase-region 'disabled nil) ; C-x C-u
(put 'set-goal-column 'disabled nil) ; C-x C-n ==> disable with C-u

;; «setup-minibuffer» (to ".setup-minibuffer")
(require 'mb-depth)
(require 'mb-depth+)
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 99)

;; «ffap-bindings» (to ".ffap-bindings")
(ffap-bindings)
;; Use now icicle bindings: `M-.' that call `ffap-guesser'

;; «Install-elisp» (to ".Install-elisp")
(require 'install-elisp)
(setq install-elisp-repository-directory "~/elisp/")
(setq install-elisp-use-url-retrieve nil) ;; use wget
(define-key dired-mode-map (kbd "<f5> i e") 'tv-install-elisp)

(defun tv-install-elisp ()
  (interactive)
  (let* ((install-elisp-repository-directory default-directory)
         (file-abs (dired-filename-at-point)))
    (install-elisp-from-emacswiki file-abs)))

;; «auto-install» (to ".auto-install")
(require 'auto-install)

;; «anything-config» (to ".anything-config")

;; (find-epp anything-type-attributes)
;; (find-fline "~/labo/anything-config-qp/anything-config.el")
;; (find-fline "~/labo/anything-thierry-qpatch/init-anything-thierry.el")
(require 'imenu)
(require 'init-anything-thierry)
(global-set-key (kbd "M-I") #'(lambda ()
                                (interactive)
                                (anything 'anything-c-source-bookmark-regions)))

(global-set-key (kbd "C-x c <SPC>") #'(lambda ()
                                      (interactive)
                                      (if current-prefix-arg
                                          (anything-global-mark-ring)
                                          (anything-mark-ring))))

(setq anything-allow-skipping-current-buffer nil)

(global-set-key (kbd "C-c <SPC>") 'anything-traverse-record-pos)
(global-set-key (kbd "C-c j p") 'anything-traverse-positions-ring)

;; «unit-tests» (to ".unit-tests")
;(require 'el-expectations)
;(require 'el-mock)

;; «delete-selection-mode» (to ".delete-selection-mode")
;; (replace text in region when type)
(delete-selection-mode 1)

;; «Info» (to ".Info")
;(eval-after-load "info" '(require 'info+))
;(require 'finder+)
;(setq Info-enable-edit nil)
(global-set-key (kbd "\C-hK") 'find-function-on-key)
(global-set-key (kbd "C-h g") 'tv-get-index-at-point)

;; «Require-Locate» (to ".Require-Locate")
(require 'locate)

;; «align.el» (to ".align.el")
;; (require 'align)
;; now align is part of emacs

;; «linkd» (to ".linkd")
(require 'linkd)

;; «eev-config» (to ".eev-config")
(require 'eev-thierry)

;; Toggle eev-mode
(global-set-key (kbd "M-<f1>") 'eev-mode)

;; Make this key available outside of eev-mode
(global-set-key (kbd "M-e") 'eek-eval-sexp-eol)

;; «Scroll-down-Scroll-up» (to ".Scroll-down-Scroll-up")
(global-set-key (kbd "<M-down>") #'(lambda ()
                                   (interactive)
                                   (scroll-down -1)))

(global-set-key (kbd "<M-up>") #'(lambda ()
                                   (interactive)
                                   (scroll-down 1)))

;; «Macros-thierry» (to ".Macros-thierry")
;(load "~/elisp/macros-func-thierry.el")

;; «nxhtml-mode» (to ".nxhtml-mode")
;;(load "~/elisp/nxhtml/autostart.el")

;; «Icicles» (to ".Icicles")
(require 'icicles)
(require 'filesets)
(eval-after-load "icomplete" '(progn (require 'icomplete+)))
(require 'hexrgb)
(require 'lacarte)
;(global-set-key (kbd "<f5> i a") 'icicle-anything)
(icy-mode 1)
(icomplete-mode 99)
;(defalias 'PC-lisp-complete-symbol 'icicle-lisp-complete-symbol)
(global-set-key (kbd "C-h ;") 'icicle-Info-index)
(setq icicle-region-background "purple")
(defalias 'apropos-command 'icicle-apropos) ; C-h a

;; EXPERIMENTAL
(setq icicle-populate-interactive-history-flag t)

;; «Fuzzy-match» (to ".Fuzzy-match")
(require 'fuzzy-match) ; toggle/ON/OFF with C-( ; toggle scatter/apropos with M-(
                       ;                     TAB                              S-TAB
                       ; use C-, to toggle sorting methods

;;(w3m-browse-url "http://www.emacswiki.org/emacs/Icicles_-_Fuzzy_Completion")

;; «Frame-divers-D.A» (to ".Frame-divers-D.A")

(require 'frame-cmds) ;==>(require 'frame-fns)
(require 'zoom-frm)
(require 'palette)
(global-set-key (kbd "<f5> <")'zoom-frm-in)
(global-set-key (kbd "<f5> >") 'zoom-frm-out)

;; «buffer-menu+» (to ".buffer-menu+")
;(require 'buff-menu+)

;; «Doremi» (to ".Doremi")
;(setq color-theme-is-global nil)
(require 'doremi-cmd) ;==>(require 'doremi)
(global-set-key (kbd "<f5> d h") 'doremi-window-height)
(global-set-key (kbd "<f5> d w") 'doremi-window-width)
;; (defadvice doremi-color-themes (before color-theme-initialize () activate)
;;   (color-theme-initialize))

;; «Get-rid-of-`mouse-set-font'» (to ".Get-rid-of-`mouse-set-font'")
(global-set-key [S-down-mouse-1] nil)
(define-key ctl-x-map "o" 'other-window-or-frame) ;(C-x o)

;; «speed-bar» (to ".speed-bar")
(global-set-key (kbd "<f11> s b") 'speedbar)

;; «sr-speedbar» (to ".sr-speedbar")
;; be sure that unsafe package is loaded AFTER ICICLES
;; (require 'sr-speedbar)
;; (global-set-key (kbd "<f11> s b") 'sr-speedbar-toggle)
;; (defalias 'speedbar 'sr-speedbar-toggle)

;; «Load-tv-utils» (to ".Load-tv-utils")
;; (find-fline "~/.emacs.d/emacs-config-laptop/tv-utils.el" "INDEX")
(require 'tv-utils)

;; «save-scratch-buffer» (to ".save-scratch-buffer")
;; (find-fline "~/.emacs.d/emacs-config-laptop/tv-utils.el" "tv-save-scratch")
;; (find-fline "~/.emacs.d/emacs-config-laptop/tv-utils.el" "lisp-interaction-mode-map")
;;(add-hook 'kill-emacs-hook 'tv-save-scratch)
(setq tv-use-autosave-in-scratch t)
(when tv-use-autosave-in-scratch
  (run-autosave-in-scratch))

(add-hook 'emacs-startup-hook 'tv-restore-scratch-buffer)

;; «Enable-scroll-other-window-globally» (to ".Enable-scroll-other-window-globally")
(global-set-key (kbd "<C-M-down>") #'(lambda ()
                                       (interactive)
                                       (scroll-other-window 1)))

(global-set-key (kbd "<C-M-up>") #'(lambda ()
                                     (interactive)
                                     (scroll-other-window -1)))

;; «emacs-image-viewer» (to ".emacs-image-viewer")
;; (find-fline "~/labo/eiv-qpatch/eiv.el" "eiv-viewer")
;(require 'eiv "~/labo/eiv-qpatch/eiv.el")
(require 'eiv)
(global-set-key (kbd "<f5> v") 'eiv-viewer)
(define-key image-mode-map (kbd "S") 'eiv-fit-image-to-window)
(define-key image-mode-map (kbd "R") 'eiv-rotate-current-image)

;; «xmodmap» (to ".xmodmap")
(load "xmodmap.elc")

;; «convenient-keys-for-windows» (to ".convenient-keys-for-windows")
(global-set-key (kbd "C-x C-&") 'delete-other-windows)
(global-set-key (kbd "C-x C-é") 'split-window-vertically)
(global-set-key (kbd "C-x C-\"") 'split-window-horizontally)
(global-set-key (kbd "C-x C-( C-(") 'make-frame-command)
(global-set-key (kbd "C-x C-( C-\-") 'delete-frame)

;; «Paredit» (to ".Paredit")

;;(find-epp paredit-commands)

;; (autoload 'paredit-mode "paredit"
;;   "Minor mode for pseudo-structurally editing Lisp code."
;;   t)
;; (define-key slime-mode-map (kbd "<f5> p m") 'paredit-mode)
;; (define-key lisp-interaction-mode-map (kbd "<f5> p m") 'paredit-mode)
;; (define-key emacs-lisp-mode-map (kbd "<f5> p m") 'paredit-mode)
;; (add-hook 'emacs-lisp-mode-hook #'(lambda () (paredit-mode +1)))
;; (add-hook 'lisp-interaction-mode-hook #'(lambda () (paredit-mode +1)))

;; «Enable-time-stamp» (to ".Enable-time-stamp")
(add-hook 'before-save-hook 'time-stamp)

;; «Auto-document» (to ".Auto-document")
(require 'auto-document)

;; «screenshot» (to ".screenshot")
(require 'screenshot)
(setq screenshot-schemes
      '(
        ;; To local image directory
        ("local"
         :dir "~/screenshot/")            ; Image repository directory
        ;; To current directory
        ("current-directory"          ; No need to modify
         :dir default-directory)
        ;; upload to Flickr
        ("Flickr"
         :dir "~/mpflickrfs/stream")
        ;; To remote ssh host
        ("remote-ssh"
         :dir "/tmp/"                 ; Temporary saved directory
         :ssh-dir "www.example.org:public_html/archive/" ; SSH path
         :url "http://www.example.org/archive/")  ; Host URL prefix
        ;; To EmacsWiki (need yaoddmuse.el)
        ("EmacsWiki"                 ; Emacs users' most familiar Oddmuse wiki
         :dir "/home/thierry/.emacs.d/yaoddmuse"  ; same as yaoddmuse-directory
         :yaoddmuse "EmacsWiki")         ; You can specify another Oddmuse Wiki
        ;; To local web server
        ("local-server"
         :dir "~/public_html/"           ; local server directory
         :url "http://127.0.0.1/")))     ; local server URL prefix
(setq screenshot-default-scheme "local")
(global-set-key (kbd "C-c <print>") 'screenshot-to-flickr)

(defun screenshot-to-flickr ()
  (interactive)
  (let* ((rname (symbol-name (gensym)))
         (fname (concat rname ".png")))
    (screenshot fname "Flickr")))

;; «woof-config» (to ".woof-config")
(require 'woof)
(woof-set-download-url-list '("http://thievol.homelinux.org:2222"))
(global-set-key (kbd "<f5> w s") 'woof-provide-dwim) ;; serve
(global-set-key (kbd "<f5> w d") 'woof-receive) ;; download
(global-set-key (kbd "<f5> w c") 'woof-provide-cancel) ;; cancel

;; «Async-library» (to ".Async-library")
(require 'async-eval)

;; «Make-header» (to ".Make-header")
(require 'header2)
(setq make-header-hook '(
                         ;;header-mode-line
                         header-title
                         header-blank
                         ;header-file-name
                         ;header-description
                         ;;header-status
                         header-copyright
                         header-author
                         header-maintainer
                         header-blank
                         header-creation-date
                         ;;header-rcs-id
                         header-version
                         ;;header-sccs
                         ;header-modification-date
                         ;header-modification-author
                         ;header-update-count
                         header-url
                         header-keywords
                         header-compatibility
                         header-free-software
                         header-blank
                         header-lib-requires
                         ;header-blank
                         ;header-end-line
                         header-commentary
                         header-blank
                         header-blank
                         header-blank
                         header-end-line
                         header-history
                         header-rcs-log
                         header-blank
                         header-blank
                         header-end-line
                         ;;header-free-software
                         header-code
                         header-eof
                         ))

;(add-hook 'emacs-lisp-mode-hook 'auto-make-header)

;; «sdcv-interface-to-stardict» (to ".sdcv-interface-to-stardict")
(require 'sdcv)
(global-set-key (kbd "C-c d") 'sdcv-search-input)

;; «sql-mode» (to ".sql-mode")
(setq sql-sqlite-program "sqlite3")

;; «ipa-mode» (to ".ipa-mode")
;(require 'ipa)

;; «windmove» (to ".windmove")
(windmove-default-keybindings)
(define-key org-mode-map (kbd "C-<left>") 'windmove-left)
(define-key org-mode-map (kbd "C-<right>") 'windmove-right)
(define-key org-mode-map (kbd "C-<up>") 'windmove-up)
(define-key org-mode-map (kbd "C-<down>") 'windmove-down)
(define-key org-agenda-mode-map (kbd "C-<left>") 'windmove-left)
(define-key org-agenda-mode-map (kbd "C-<right>") 'windmove-right)
(define-key org-agenda-mode-map (kbd "C-<up>") 'windmove-up)
(define-key org-agenda-mode-map (kbd "C-<down>") 'windmove-down)

;; «key-for-copy-files-async» (to ".key-for-copy-files-async")
(define-key dired-mode-map (kbd "C-c C-S-c") 'tv-slime-dired-copy-files-or-dir-async)
(define-key dired-mode-map (kbd "C-c C-S-d") 'tv-slime-dired-delete-files-async)

;; «babel» (to ".babel")
(autoload 'babel "babel"
  "Use a web translation service to translate the message MSG." t)
(autoload 'babel-region "babel"
  "Use a web translation service to translate the current region." t)
(autoload 'babel-as-string "babel"
  "Use a web translation service to translate MSG, returning a string." t)
(autoload 'babel-buffer "babel"
  "Use a web translation service to translate the current buffer." t)

;; «rectangles» (to ".rectangles")
(global-set-key (kbd "C-x r v") 'string-insert-rectangle)

;; «bookmark+» (to ".bookmark+")
(require 'bookmark+)
;(setq bookmark-save-flag 1)
(defun bmk-bmenu-set-new-keys ()
  (define-key bookmark-bmenu-mode-map "W" 'bookmark-bmenu-list-only-w3m-entries)
  (define-key bookmark-bmenu-mode-map "G" 'bookmark-bmenu-list-only-gnus-entries)
  (define-key bookmark-bmenu-mode-map "I" 'bookmark-bmenu-list-only-info-entries)
  (define-key bookmark-bmenu-mode-map "F" 'bookmark-bmenu-list-only-files-entries)
  (define-key bookmark-bmenu-mode-map "R" 'bookmark-bmenu-list-only-regions))
  
(add-hook 'bookmark-bmenu-mode-hook 'bmk-bmenu-set-new-keys) 

;; «isearch» (to ".isearch")
(setq isearch-allow-scroll t)

;; «outline-minor-mode» (to ".outline-minor-mode")

(global-set-key (kbd "C-c C-à C-t") 'hide-body)
(global-set-key (kbd "C-c C-à C-e") 'show-entry)
(global-set-key (kbd "C-c C-à C-d") 'hide-subtree)
(global-set-key (kbd "C-c C-à C-a") 'show-all)

;; «align-let» (to ".align-let")
(autoload 'align-let-keybinding "align-let" nil t)
(add-hook 'emacs-lisp-mode-hook 'align-let-keybinding)
(add-hook 'lisp-interaction-mode-hook 'align-let-keybinding)
(add-hook 'lisp-mode-hook 'align-let-keybinding)

;; «delete-pair» (to ".delete-pair")
;; See also:
;; (find-fline "~/.emacs.d/emacs-config-laptop/tv-utils.el" "defun tv-insert-double-quote")
(define-key lisp-mode-map (kbd "C-c (") 'delete-pair)
(define-key lisp-interaction-mode-map (kbd "C-c (") 'delete-pair)
(define-key emacs-lisp-mode-map (kbd "C-c (") 'delete-pair)    

;; «Trash» (to ".Trash")
;(setq delete-by-moving-to-trash t)

;; «line-move-visual» (to ".line-move-visual")
(setq line-move-visual nil)

;; «auto-complete» (to ".auto-complete")
;; (require 'auto-complete)
;; (add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
;; (set-default 'ac-sources '(ac-source-abbrev ac-source-symbols))
;; (require 'auto-complete-emacs-lisp)
;(require 'autocomplete-ipython)
;(add-hook 'python-mode-hook 'auto-complete-mode)

;; «anything-ipython» (to ".anything-ipython")
(require 'anything-ipython)
(add-hook 'python-mode-hook #'(lambda ()
                                (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
(add-hook 'ipython-shell-hook #'(lambda ()
                                  (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'anything-ipython-complete
                                '(length initial-pattern)))
(define-key py-mode-map (kbd "C-c M") 'anything-ipython-import-modules-from-buffer)

; «END»  (to "INDEX")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs.el ends here
;; Local Variables:
;; eev-comment-prefix: ";"
;; ee-anchor-format: "«%s»"
;; End:
