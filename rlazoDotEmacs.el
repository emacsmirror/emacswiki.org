;;;
;; -*-emacs-lisp-*-
;;
;; Some ideas kindly borrowed from:
;;
;; Alvaro Lopez Ortega <alvaro [ at) alobbs ( dot] com>
;; J.T. Halbert
;; François Fleuret <francois ( dot)  fleuret {at} epf (_dot_) ch>
;; Eric Knauel <knauel _at_ informatik.uni-tuebingen _dot_ de>
;; Charles Curlye <charlescurley at charlescurley dot com>
;; StefanKamphausen (emacs-wiki)

;; It's better to set the preferences in the .Xresources so that the
;; window is not first displayed with the wrong options

;; Emacs.menuBar:          off
;; Emacs.verticalScrollBars:       off
;; Emacs.toolBar:          off
;; Emacs.internalBorder:      1

;; *custom
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(bongo-default-directory "/home/rodrigo/shared/musica")
 '(canlock-password "bda46fff3d84eefcb898fcd94bc0df3febf3efda")
 '(case-fold-search t)
 '(current-language-environment "Spanish")
 '(default-input-method "latin-9-prefix")
 '(display-time-mode t)
 '(erc-modules (quote (autojoin button fill irccontrols match netsplit noncommands completion readonly ring services stamp smiley track)))
 '(iso-ascii-convenient t)
 '(jabber-alert-message-hooks (quote (jabber-message-wave jabber-message-echo jabber-message-scroll)))
 '(jabber-alert-message-wave "/home/rodrigo/.sounds/receive.mp3")
 '(jabber-alert-presence-wave "/home/rodrigo/.sounds/arrive.mp3")
 '(jabber-password "PASSWORD")
 '(jabber-play-sound-file (quote bongo-play-file))
 '(jabber-roster-line-format "  %c %-25n %u %-8s  %S
")
 '(jabber-server "jabberes.org")
 '(jabber-username "USER")
 '(jde-jdk-registry (quote (("6" . "/home/rodrigo/lib/jdk1.6.0_01"))))
 '(muse-project-alist (quote (("WikiPlanner" ("~/.plans" :default "index" :major-mode planner-mode :visit-link planner-visit-link)))))
 '(weblogger-config-alist (quote (("default" ("user" . "USER") ("server-url" . "http://myblog.supersized.org/serendipity_xmlrpc.php") ("weblog" . "1"))))))
 

;; Some paths, always first
(load "/usr/share/emacs/site-lisp/site-gentoo")
(load "~/.emacs.d/skeletons")
(add-to-list 'load-path 
	     (expand-file-name "~/.emacs.d/site-lisp"))
(add-to-list 'load-path 
	     (expand-file-name "~/.emacs.d/site-lisp/jabber"))
(add-to-list 'load-path 
	     (expand-file-name "~/.emacs.d/site-lisp/jde/lisp"))
(setq diary-file "~/.emacs.d/diary")


;; Be sure not to exit by accident
;;;;;;;;;;;;;
(setq confirm-kill-emacs 'yes-or-no-p)

;; Requires
;;;;;;;;;;;;;
(require 'jabber)
(require 'no-word)
(require 'font-lock)
(require 'recentf)
(require 'pair-mode)
(require 'autoinsert)
(require 'background)
(require 'background-nobuffer)
(require 'autosmiley)
(require 'bongo)
(require 'jde)
(require 'doctest-mode)
(require 'python)

;;
;; Autoloads
;;----------------------------
(autoload 'table-insert "table" "insert new table" t)
(autoload 'table-recognize "table" "identifies a table" t)
;;(autoload 'mingus "mingus" "MPD client" t)

;;
;; General Customization
;;----------------------------

;; My info
(setq user-full-name "Rodrigo Lazo"
      user-mail-address "rlazo.paz(AT)gmail.com")

;; Default mode
(setq default-major-mode 'outline-mode)

;; abbrev mode
(setq abbrev-file-name "~/.emacs.d/abbrev_defs"
      default-abbrev-mode t)
(quietly-read-abbrev-file)

;; Color theme if inside X
(if window-system
    (progn
      (color-theme-vim-colors))
  )



;; avoid cjk
(setq utf-translate-cjk-mode nil)

;; Spell program
(setq-default ispell-program-name "aspell")

;; No splash screen
(setq inhibit-startup-message t)

;; Make yes - no into y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Show the clock
(display-time)

;; indent code
(setq c-default-style "k&r")

;; make paren matches visible
(show-paren-mode 1)

;; Open word files using antiword
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

;; To ease things with the mark ring
(setq set-mark-command-repeat-pop 1)

;; Don't throw autosave file everywhere
;; from wiki
;; (setq
;;    backup-directory-alist
;;    '(("." . "~/.saves")))    ; don't litter my fs tree

;;
;; Misc Modes
;;---------------------------

;; Recentf mode
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)

;; nxml autoload
(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "xt" "xd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
		   'nxml-mode))

(setq magic-mode-alist
      (cons '("<\\?xml " . nxml-mode)
	    magic-mode-alist))

;; Planner
(add-to-list 'load-path "~/.emacs.d/site-lisp/muse/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/planner")

(setq muse-project-alist
      '(("WikiPlanner"
	 ("~/.plans"   ;; Or wherever you want your planner files to be
	  :default "index"
	  :major-mode planner-mode
	  :visit-link planner-visit-link))))
(require 'planner)
(require 'planner-bbdb)
(require 'planner-gnus)
(planner-gnus-insinuate)



;; smtp mode
(setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
(setq smtpmail-default-smtp-server "smtp.gmail.com") ; set before loading library
;;(setq smtpmail-debug-info t) ; only to debug problems
(setq smtpmail-auth-credentials		; or use ~/.authinfo
      '(("smtp.gmail.com" 25 "rlazo.paz@gmail.com" "PASSWD")))
(setq smtpmail-starttls-credentials
      '(("smtp.gmail.com" 25 "~/.my_smtp_tls.key" "~/.my_smtp_tls.cert")))
;; weird but I dont have this files and it still works

;; auto-add signature
(setq mail-setup-hook
      (function
       (lambda ()
	 (mail-signature) )))

;;
;; ElScreen
;;----------------------------
(require 'elscreen)
(require 'elscreen-dired)
;; shell in a new tab of elscreen
(defun elscreen-shell-screen ()
  "Creates a new screen with shell in it"
  (interactive)
  (let ((cur-buffer (buffer-name)))
    (progn
      (eshell)
      (switch-to-buffer cur-buffer)
      (elscreen-create)
      (switch-to-buffer "*eshell*"))))

(defun irc-elscreen ()
  "Creates a new screen for each irc channel"
  (interactive)
  (select-frame-by-name "Emacs IRC")  
  (let ((lista (mapcar (function buffer-name) (buffer-list)))
	(first-buff (buffer-name)))
    (dolist (buff lista) 
      (if (and (string-match "^[#][.]*" buff) (not (string= first-buff buff)))
	  ((lambda ()
	     (elscreen-create)
	     (switch-to-buffer buff)))))))

(defun elscreen-kill-and-buffer ()
  "Kills the current tab and also the buffer that is editing. If
is just one screen, only kills the buffer"
  (interactive)
  (let ((cur-buf (buffer-name)))
    (kill-buffer (buffer-name))
    (if (> (elscreen-get-number-of-screens) 1)
	(elscreen-kill))))

;; Create a new w3m buffer and tab using elscreen
(defun w3m-url-to-new-tab ()
  "Creates a new elscreen tab for the new w3m sesion"
  (interactive)
  (call-interactively 'w3m-goto-url-new-session)
  (let ((cur-buf (buffer-name)))
    (switch-to-buffer (other-buffer))
    (elscreen-create)
    (switch-to-buffer cur-buf)))

(add-hook 'w3m-mode-hook
	  '(lambda ()
	     (define-key w3m-mode-map "G" 'w3m-url-to-new-tab)))

;;
;; Dired
;;----------------------------
(eval-after-load "dired"
  '(progn
     (require 'dired+)
     (require 'dired-details+)
;; included on the emacs22 package
     (require 'wdired)
     (require 'background-dired)
     (add-hook 'dired-mode-hook
	       (function (lambda()
			   (setq dired-omit-files-p t)
			   (setq dired-omit-files
				 (concat dired-omit-files "\\|^\\..+$"))
			   (delete ".pdf" dired-omit-extensions))))

     (setq dired-recursive-deletes t
	   dired-recursive-copies t)
     (toggle-dired-find-file-reuse-dir t)
     (setq dired-listing-switches "-lha"))) 


;;
;; Background
;;----------------------------

;;
;; BBDB
;;----------------------------
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)
(setq bbdb-send-mail-style 'message)

;;
;; ERC
;;----------------------------

;; fires up a new frame and opens your servers in there. You will need
;; to modify it to suit your needs.
(setq erc-server-coding-system 
      (quote
       (utf-8 . utf-8)))

;; new buffer with private messages
(setq erc-auto-query 'buffer)

;; Function to aid on starting erc
(defun irc ()
  "Start to waste time on IRC with ERC."
  (interactive)
  (select-frame (make-frame '((name . "Emacs IRC")
 			      (minibuffer . t))))
  (erc :server "irc.freenode.net" :port 6667 :full-name "Rodrigo Lazo"))

; (call-interactively 'erc-server-select))

;; channels
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#aqpglug" "#django" "#gentoo-gwn-es"
	 "#emacs" "#gentoo-scire" "#gentoo-soc" "#gentoo-lisp")))

;; passwords
(setq erc-prompt-for-nickserv-password nil)

(setq erc-nickserv-passwords
      '((freenode     (("rlazo" . "PASSWD")))))

;; smileys
(require 'smiley)
(add-to-list 'smiley-regexp-alist '("\\(:-?]\\)\\W" 1 "forced"))
(add-to-list 'smiley-regexp-alist '("\\s-\\(:-?/\\)\\W" 1 "wry"))
(add-to-list 'smiley-regexp-alist '("\\(:-?(\\)\\W" 1 "sad"))
(add-to-list 'smiley-regexp-alist '("\\((-?:\\)\\W" 1 "reverse-smile"))
(add-to-list 'smiley-regexp-alist '("\\(:-?D\\)\\W" 1 "grin"))
(add-to-list 'smiley-regexp-alist '("\\(:-?P\\)\\W" 1 "poke"))


;;
;; Jabber
;;----------------------------

(add-hook 'jabber-chat-mode-hook 'autosmiley-mode)
(setq abber-vcard-avatars-retrieve nil)

(defun jabber-start ()
  "Handle the jabber connection startup. If it's call and there
  isn't a buffer named *-jabber-* lunchs jabber-connect,
  otherwise opens a new frame named with *-jabber-* as the
  buffer. In the case there's alredy the byffer and the frame, it
  doesn't do anything"
  (interactive)
  (if (not (member "*-jabber-*" (mapcar (function buffer-name) (buffer-list))))
      (jabber-connect)
    (if (not (member "jabber" (mapcar '(lambda(x) (frame-parameter x 'name)) 
				      (frame-list))))
	(progn
	  (select-frame (make-frame '((name . "jabber")
				      (minibuffer . t))))
	  (switch-to-buffer "*-jabber-*"))
      )))

(defun jabber-stop ()
  "Disconnect from jabber doing some cleaning"
  (interactive)
  (jabber-disconnect)
  (switch-to-buffer "*-jabber-*")
  (kill-buffer nil))
  
;;
;; Alarm Clock
;;----------------------------p
;; by: Mathias Dahl <brakjollerATgmail.com> emacs-help list

(defvar alarm-clock-timer nil
  "Keep timer so that the user can cancel the alarm")

(defun alarm-clock-message (text)
  "The actual alarm action"
  (message-box text))

(defun alarm-clock ()
  "Set an alarm.
The time format is the same accepted by `run-at-time'.  For
example \"11:30am\"."
  (interactive)
  (let ((time (read-string "Time: "))
        (text (read-string "Alarm message: ")))
    (setq alarm-clock-timer (run-at-time time nil 'alarm-clock-message text))))

(defun alarm-clock-cancel ()
  "Cancel the alarm clock"
  (interactive)
  (cancel-timer alarm-clock-timer))

;;
;; Time Stamp
;;----------------------------
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")
(add-hook 'write-file-hooks 'time-stamp)

;; A function to insert the time stamp at point.
(defun stamp ()
  "Insert at point the dummy time stamp string to activate the time stamp facility."
  (interactive "*")
  (insert "Time-stamp: \" \"")             ;insert the bare bones
  (time-stamp)                          ;call the function to fill it in
                                        ;where we put it.
  )

;;
;; Python
;;----------------------------
 (autoload 'python-mode "python-mode" "Python Mode." t)
 (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
 (add-to-list 'interpreter-mode-alist '("python" . python-mode))


(eval-after-load "python-mode"
  '(require 'ipython))

; add my customization
(add-hook 'python-mode-hook 'my-python-hook)
; this gets called by outline to deteremine the level. Just use the length of the whitespace
(defun py-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))
; this get called after python mode is enabled
(defun my-python-hook ()
  ; outline uses this regexp to find headers. I match lines with no indent and indented "class"
  ; and "def" lines.
  (setq outline-regexp "[^ \t]\\|[ \t]*\\(def\\|class\\) ")
  ; enable our level computation
  (setq outline-level 'py-outline-level)
  ; turn on outline mode
  (outline-minor-mode t)
)

;; Auctex
;;---------------------------
(setq TeX-view-format "pdf")

;;
;; Language hooks
;;----------------------------

;; C
(add-hook 'c-mode-hook
	  (function (lambda ()
		      (outline-minor-mode)
		      (define-key c-mode-map "\C-cc" 'compile)
		      )))

;; C++ 
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (define-key c++-mode-map "\C-cc" 'compile)
	     (c-set-offset 'inline-open 0)
	     (outline-minor-mode)))

;; Python
(add-hook 'python-mode-hook
	  '(lambda ()
	     (pair-mode)))



;; Lisp
(add-hook 'lisp-mode-hook
	  '(lambda ()
	     (pair-mode)))
;; XML
(add-hook 'nxml-mode-hook
	  '(lambda()
	     (define-key nxml-mode-map "\C-c\C-t" 'sgml-tag)))

;; Calendar
(add-hook 'calendar-mode-hook
	  '(lambda()
	     (setq mark-diary-entries 1)))


;;
;; MSF-abbrev
;;----------------------------

;; ;; ensure abbrev mode is always on
;; (setq-default abbrev-mode t)

;; ;; do not bug me about saving my abbreviations
;; (setq save-abbrevs nil)

;; ;; load up modes I use
;; ;;(require 'cc-mode)
;; ;;(require 'php-mode)
;; (require 'python-mode) 
;; ;(require 'tex-site) ;; I use AUCTeX
;; ;(require 'latex)    ;; needed to define LaTeX-mode-hook under AUCTeX
;; ;(require 'tex)      ;; needed to define TeX-mode-hook under AUCTeX
;; (require 'python)   ;; I use python.el from Emacs CVS, uncomment if you do also

;; ;; load up abbrevs for these modes
;; (require 'msf-abbrev)
;; (setq msf-abbrev-verbose t) ;; optional
;; (setq msf-abbrev-root "~/.emacs.d/mode-abbrevs")
;; (global-set-key (kbd "C-c l") 'msf-abbrev-goto-root)
;; (global-set-key (kbd "C-c a") 'msf-abbrev-define-new-abbrev-this-mode)
;; (msf-abbrev-load)

;(msf-abbrev-load-tree "~/.emacs.d/mode-abbrevs")

;;
;; CEDET
;;----------------------------

;; Load CEDET
;;(load-file "~/.emacs.d/site-lisp/cedet-1.0pre3/common/cedet.el")

;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; Select one of the following
;;(semantic-load-enable-code-helpers)
;; (semantic-load-enable-guady-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)

;; Enable this if you develop in semantic, or develop grammars
;; (semantic-load-enable-semantic-debugging-helpers)



;;
;; Global Keybindings
;;----------------------------

;; Some global keybindings
(global-set-key "\eg" 'goto-line)
(define-key global-map [C-next] 'elscreen-next)
(define-key global-map [C-prior] 'elscreen-previous)
(define-key global-map [?\C-.] 'dabbrev-expand)
(define-key global-map [?\s->] 'hide-subtree)
(define-key global-map [?\s-<] 'show-entry)
(define-key global-map [?\C-<] 'show-children)
(define-key global-map "\C-z\C-K" 'elscreen-kill-and-buffer)
(define-key global-map [f4] 'elscreen-kill-and-buffer)
(define-key global-map [f7] 'bookmark-set)
(define-key global-map [f8] 'bookmark-jump)
(define-key global-map [f9] 'jabber-start)
(define-key global-map [C-f9] 'jabber-stop)
(define-key global-map [C-f10] 'w3m-frame-unfocus)
(define-key global-map [f11] 'elscreen-shell-screen)
(define-key global-map [f12] 'calendar)
(define-key global-map [C-f4] 'delete-frame-and-buffer)
(define-key global-map [?\C-ç] 'ispell-word)
(define-key global-map "\C-x5g" 'gnus-other-frame)
(define-key global-map "\C-x\C-b" 'electric-buffer-list)


;;
;; Misc Functions
;;----------------------------

;; kill buffer and frame
(defun delete-frame-and-buffer ()
  "Kills the current buffer and then closes the frame"
  (interactive)
  (progn
    (kill-buffer (buffer-name))
    (delete-frame)))

;; Make file starting with she-bangs executables at save-time
(add-hook 'after-save-hook
	  #'(lambda ()
	      (and (save-excursion
		     (save-restriction
		       (widen)
		       (goto-char (point-min))
		       (save-match-data
			 (looking-at "^#!"))))
		   (not (file-executable-p buffer-file-name))
		   (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
		   (message
		    (concat "Saved as script: " buffer-file-name)))))

;; Autoinsert
(auto-insert-mode)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory "~/.emacs.d/mytemplates/") ;;; Or use custom, *NOTE* Trailing slash important
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion
(define-auto-insert "\.tex" "latexTemplate.tex")
(define-auto-insert "\.py" "pythonTemplate.py")
(define-auto-insert "Makefile" "MakefileTemplate")

(defun my-c-mode-hook ()
  (c-set-offset (quote case-label) (quote +) nil)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-hook)


(put 'narrow-to-region 'disabled nil)

(defun w3m-frame-unfocus()
  "Opens a new frame with w3m on it but don't focus it"
  (interactive)
  (progn
    (select-frame (make-frame '((name . "w3m browser")
				(minibuffer . t))))
    (w3m)
    (other-frame -1)))
