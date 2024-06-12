;;; :FILE-CREATED <Timestamp: #{2024-02-16} - by MON KEY>
;;; :FILE ~/.emacs
;;; ==============================
;;
;; The Bare bones installation requirements to get a sane Emacs up and running.
;; use this sequence for  loading the emacs files:
;;
;; 1. ~/.emacs is loaded
;; 2. ~/Documents/HG-Repos/site-local-private.el
;; 3. ~/Documents/HG-Repos/mon-default-loads.el
;; 4. ~/Documents//HG-Repos/mon-default-start-loads.el
;;  - The :FILE mon-default-start-loads.el in turn does the following:
;; `mon-set-system-specific-and-load-init'
;; -> (require 'slime-loads-GNU-clbuild)
;; -> (mon-slime-setup-init)
;; -> (mon-define-common-lisp-style)
;; -> (mon-set-common-lisp-hspec-init)
;; -> (require 'mon-utils) ;; !!!!!! :NOTE This is where evyerthing else of the mon-*.el packages gets loaded. !!!!!!
;; 
;; ==============================
;; ‘C-x C-c’   Kill Emacs. Note, behaves specially if you are using Emacs as a server.  If
;; you type it from a client frame, it closes the client connection.  *Note
;;
;; (server-running-p)
;; server-process
;;
;;; ==============================
;; Some useful functions and variables to interogate/inspect a new Emacs install
;; environment while we get the full Mon system up and running:
;;
;; user-emacs-directory
;; user-init-file
;; init-file-user
;; site-run-file
;; default-directory
;; process-environment
;; initial-environment
;; invocation-name ;; emacs
;; invocation-directory
;; (emacs-pid)
;; system-type
;; system-name
;; (system-name) 
;; (user-login-name)
;; (user-original-login-name (user-real-uid))
;;
;; (setenv "<ENV-VAR>"
;;
;; (getenv "PATH")
;; (getenv "HOME")
;; (getenv "USER")
;; (getenv "SHELL")
;; (getenv "TERM")
;; (getenv "LANG")
;; (getenv "USER")
;; (getenv "LOGNAME")
;;
;; Envritonmental Variables we set to make Mon system configuration sane.
;; (getenv "DEVHOME")
;; (getenv "MON_HOME")
;; (getenv "CL_MON_CODE")
;; (getenv "SBCL_SOURCE_ROOT")
;; (getenv "SBCL_HOME")
;; (getenv "MON_EMACS_LOAD")
;;
;;
;; (substitute-env-vars "${DEVHOME}/SDP_EMACS/emacs-load-files")
;; (dired-other-window (substitute-env-vars "${DEVHOME}/SDP_EMACS/emacs-load-files"))
;; (expand-file-name (pwd))
;; (pwd)
;;
;; ==============================
;; Keybinding Interrogation for Darwin:
;; 
;; mac-function-modifier ;; none
;; mac-control-modifier ;; control
;; mac-command-modifier ;; super
;; mac-option-modifier ;; meta - consider making this something different.
;; mac-right-command-modifier
;; mac-right-control-modifier
;; mac-right-option-modifier
;; mac-right-option-modifier
;;
;; 
;; initial-buffer-choice
;;
;; ==============================

 
;; FIRST THINGS FIRST:
;; Make left Command 'alt/option
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

;; Make sure we're not driven crazy.
(blink-cursor-mode -1)

;; (setq tool-bar-mode nil)
;; (setq tool-bar-mode t)

;; Enough with the yes or no questions
(setq use-short-answers t)

;; Turn off the infernal ringing
(setq ring-bell-function 'ignore)
;; (setq visible-bell nil)

;; This from mon-default-start-loads
(unless show-paren-mode (show-paren-mode 1))

;; (set-scroll-bar-mode 'left)
;; (set-scroll-bar-mode 'right)
;; (set-scroll-bar-mode nil)
(unless (null scroll-bar-mode) (scroll-bar-mode -1))
(unless (null tool-bar-mode) (tool-bar-mode -1))

;; Comment this out we need it at moment
;; (unless (null menu-bar-mode) (menu-bar-mode -1)) 

(setq-default cursor-type '(bar . 3))
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

;; Set this later elsewhere, and make sure we open into the emacs-load-files directory.
;; (setq initial-buffer-choice "*scratch*")

(setq text-quoting-style 'grave)
(custom-note-var-changed 'text-quoting-style)

;; :NOTE Fullscreen on macOS is a bit special, in that apps get their own
;; desktop where other windows normally can’t exit behind or in front of the
;; app. Personally I hate this for of fullscreen, but emacs supports using a
;; non-native fullscreen mode: (setq ns-use-native-fullscreen nil) This turns
;; off the native macOS fullscreen mode, and instead make the current emacs
;; frame simply take up the whole screen on the current desktop. This means you
;; can switch to other apps/windows, and they’ll appear on top of emacs.
;; :SOURCE https://www.reddit.com/r/emacs/comments/n68q16/fullscreen_and_makeframe_on_osx/
(setq ns-use-native-fullscreen nil)


;; Begin setting up the local Emacs environment:
(let* ((uname  (apply 'concat (mapcar 'char-to-string  '(109 111 110 107 112 101 97 114 109 97 110))))
       (user  (concat "USER=" uname))
       (home  (concat "/Users/" uname))
       (uhome  (concat "HOME=" home)))
  (when (and  (member user process-environment)
              (member uhome process-environment)
              (string-equal (getenv "HOME") home))
    (cd (getenv "HOME"))
    (unless (equal (getenv "DEVHOME") (substitute-in-file-name "${HOME}/Documents/HG-Repos"))
      (setenv "DEVHOME" (substitute-in-file-name "${HOME}/Documents/HG-Repos")))
    ;; On i3_i3_i3 GNU This was i the file ~/emacs-environment because xdg didn't pick it up otherwise, do it here instead.
    (let ((environs '(("MON_HOME"    .  "$HOME")
                      ("MON_EMACS_LOAD" . "${DEVHOME}/SDP_EMACS/emacs-load-files")
                      ;; ("DEVHOME"     . "$MON_HOME/Documents/HG-Repos") ;; already set above keep here for reference
                      ;; ("LOCAL_MON"   .  "$MON_HOME/LOCAL-MON")
	              ;; ("BIN_MON"     . "$LOCAL_MON/BIN-MON")
	              ;; ("MON_SCRIPTS" . "$BIN_MON/mon-scripts")
                      ;; ("PATH"        . "$PATH:/usr/local/bin:$BIN_MON:$BIN_MON/mon-scripts")
                      ;;
                      ;; ("SHARE_MON"   . "$LOCAL_MON/SHARE-MON")
	              ;; ("DOC_MON"     . "$SHARE_MON/DOC-MON")
                      ;; :NOTE "INFO_MON" is a directory where we store site local info files like the ansicl info spec
                      ;; Don't add the trailing "/" we add it later in mon-default-start-loads with `file-name-as-directory'
	              ("INFO_MON"    .  "$DEVHOME/SDP_INFO") 
	              ;; ("MAN_MON"     . "$SHARE_MON/MAN-MON")
                      ("SBCL_HOME"  . "opt/homebrew/lib/sbcl")
                      ("SBCL_SOURCE_ROOT" . "/opt/homebrew/Cellar/sbcl/2.4.1/share/sbcl/src")
	              ("QUICKLISP_HOME" . "${DEVHOME}/quicklisp")
                      ;; :NOTE on i3-i3 there was a directory "CL-repo-HG" we aren't using that anymore.
                      ;; ("CL_MON_CODE" . "$DEVHOME/CL-repo-HG/CL-MON-CODE") 
                      ;; (let ((env '("CL_MON_CODE" . "$DEVHOME/CL-MON-CODE"))) 
                      ;;   (setenv (car env) (substitute-env-vars (cdr env))))
                      ("CL_MON_CODE" . "$DEVHOME/CL-MON-CODE") ;; (getenv "CL_MON_CODE")
                      )))
      (mapcar #'(lambda (x) (setenv (car x) (substitute-env-vars (cdr x)))) environs))
    ;; Push emacs-load-files onto the `load-path'. :NOTE This should be moved out if granularity requires it.
    (add-to-list 'load-path (substitute-env-vars "${MON_EMACS_LOAD}"))))

 
;;; ==============================
;; On i3_i3_i3 GNU This was i the file ~/emacs-environment because xdg didn't pick it up otherwise
;; keep it here for future reference in case it's neeeded
;; (let ((environs '(("EMACSCLIENT" . "emacsclient -c")
;; 	          ("EDITOR"      . "$EMACSCLIENT")
;; 	          ("VISUAL"      . "$EMACSCLIENT")
;; 	          ("EMACS"       . "$EDITOR")
;; 	          ("ALTERNATE_EDITOR" . "emacsclient --alternate-editor emacs +%d %s"))))
;;   (mapc #'(lambda (x) (setenv (car x) (substitute-env-vars (cdr x)))) environs))

;;; ==============================
;; This from the i3_i3_i3 Gnu .emacs
;;
;; (when (and (member "GDMSESSION=xfce" process-environment)
;;            (setq *mon-fixing-faces* t)
;;            (file-exists-p "emacs-environment"))
;;   (load (expand-file-name "emacs-environment") t nil t))

 
;;; ==============================
;;; :FRAME-PARAMETERS
;;; ==============================

;; :NOTE Following defaults are loaded after whatever is specified by the X
;; resources for the initial frame. IOW these only become active _during_ Emacs'
;; user specific initialization e.g. _after_ the creation of the initial frame. 
;; Inpsect documentation of `initial-frame-alist' for details.
;; :SEE struct `frame_parm_table' :FILE src/frame.c
;; :FILE src/xsettings.c src/xfns.c
;; :SEE-ALSO `x-get-resource', `x-parse-geometry', `frame-parameters',


;; `window-system-default-frame-alist'
(setq window-system-default-frame-alist
      '((x   (menu-bar-lines . 0) (tool-bar-lines   . 0))
        (nil (menu-bar-lines . 0) (tool-bar-lines   . 0))))
(custom-note-var-changed 'window-system-default-frame-alist)

;; `default-frame-alist'
(setq default-frame-alist
      '((tool-bar-lines . 0)
        ;; (vertical-scroll-bar . nil)
        (vertical-scroll-bars)
        (background-color . "black")
        (background-mode . dark)
        (border-color . "black")
        (cursor-color . "yellow")
        (foreground-color . "white")
        (mouse-color . "white")
        (menu-bar-lines . 0)))
(custom-note-var-changed 'default-frame-alist)

;; When they do become visible, put them on the right.
(setq default-frame-scroll-bars 'right)

 
;;; ==============================
;; If you want to use the default shortcuts for copy, paste, cut etc. macOS, put this is your .init.el.
;;  Since M-x is overwritten in line 2, don’t forget to reassign it. See the last line as an example.
;;
;; (global-set-key (kbd "M-c") 'kill-ring-save) ; ⌘-c = Copy
;; (global-set-key (kbd "M-x") 'kill-region) ; ⌘-x = Cut
;; (global-set-key (kbd "M-v") 'yank) ; ⌘-v = Paste
;; (global-set-key (kbd "M-a") 'mark-whole-buffer) ; ⌘-a = Select all
;; (global-set-key (kbd "M-z") 'undo) ; ⌘-z = Undo
;; (global-set-key (kbd "≈") 'execute-extended-command) ; Replace ≈ with whatever your option-x produces

;;; ==============================
;; :NOTE C-M-q locks the screen, this maps to Conntrol Command Q which the Darwin GUI eats first. 
;; How to inhibit or get around this???
;; (key-binding (kbd "C-M-q")) ;; -> `indent-pp-sexp'

;;; ==============================
(global-set-key "\C-x\C-d"            'dired)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)
(global-set-key (kbd "<S-backspace>") (kbd "DEL"))
(global-set-key "\C-cflr"             'fill-region)
(global-set-key "\C-cvm"              'view-mode)                        
(global-set-key "\C-c\M-/"            'hippie-expand)
;; (global-set-key "\M-n"             'mon-scroll-up-in-place)
;; (global-set-key "\M-p"             'mon-scroll-down-in-place)
;; (global-set-key "\C-cu:"           'mon-cln-up-colon)
;; (global-set-key "\C-cwou"          'mon-wrap-one-url)

;;; ==============================
;; Define this early so we can read the info files for the local Environment with sanity.
(defun mon-info-mode-hook ()
  "Function executed on the `ido-minibuffer-setup-hook' which adds bindings to
`Info-mode-map' for `Info-mode'.
:SEE-ALSO `mon-ido-completion-map-hook', `mon-post-load-hooks'"
  ;; (define-key Info-mode-map "\M-n" 'mon-scroll-up-in-place) 
  ;; :NOTE follwing synch w/ MON binding for `help-go-forward' and `help-go-back'
  ;; Move back in history to the last node you were at.
    (define-key Info-mode-map "\C-c\C-b" 'Info-history-back)
  ;; Move forward in history to the node you returned from after using l.
    (define-key Info-mode-map "\C-c\C-f" 'Info-history-forward)
    (define-key Info-mode-map "\C-cia" 'info-apropos))
    
(add-hook 'Info-mode-hook 'mon-info-mode-hook) 

;;; ==============================
(defun mon-ido-completion-map-hook ()
  "Function executed on the `ido-minibuffer-setup-hook' which adds bindings to
`ido-completion-map' for `ido-complete'.
:SEE-ALSO `mon-info-mode-hook', `mon-post-load-hooks'.\n▶▶▶ ."
  (define-key ido-completion-map (kbd "<backtab>") 'ido-complete))

(add-hook 'ido-minibuffer-setup-hook 'mon-ido-completion-map-hook)

 
;;; ==============================
;; (setq-default x-select-enable-clipboard 1)

;; Not sure why/if we need this anymore:
;;
;; (put 'narrow-to-region  'disabled nil)
;; (put 'downcase-region   'disabled nil)
;; (put 'upcase-region     'disabled nil)
;; (put 'capitalize-region 'disabled nil)
;; (put 'eval-expression   'disabled nil)

;;; ==============================
;; From mon-default-start-loads.el functions `mon-set-split-window-init'
(custom-set-variables 
    '(split-width-threshold  180 t nil ":DEFAULT 160")
    '(split-height-threshold 50  t nil ":DEFAULT 80")
    '(even-window-heights    nil t nil ":DEFAULT t"))

;;; ==============================
;; From `mon-set-color-themes-init'
(custom-set-variables 
      '(font-lock-verbose (1+ (* 8 1024)) t nil ":DEFAULT 0")
      '(global-font-lock-mode t)
      '(font-lock-maximum-decoration t))

;;; ==============================
(custom-set-variables
    '(ibuffer-default-shrink-to-minimum-size t)
    '(ibuffer-always-show-last-buffer nil)
    '(ibuffer-use-header-line t))

;;; ==============================
;; (require 'show-point-mode)


;;; ==============================
;; (getenv "MON_EMACS_LOAD")
;; uncomment to load everything at inti.
;; (let* ((melf (getenv "MON_EMACS_LOAD"))
;;        ;; (if-exists (and (stringp melf)
;;        ;;                 (file-exists-p melf)
;;        ;;                 ;; (setq user-emacs-directory (concat melf "/"))
;;        ;;                 melf))
;;        ;; just assume they loadfiles exist and are there and exist at this point.
;;        (loademup (mapcar #'(lambda (x) (substitute-in-file-name  (concat "${MON_EMACS_LOAD}/" x)))
;;                          (list "site-local-private.el"
;; 	                       "mon-default-loads.el"
;;                                "mon-default-start-loads.el"))))
;;   (when (and loademup (eq (length loademup) 3))
;;     (mapc #'(lambda (y) (load y)) loademup)))


;;; ==============================
;; Make sure our init-file is .emacs Note, this won't prevent site-start.el from being loaded tho.
;; (setq init-file-user load-file-name)

;;; ==============================
;; `debug-ignored-errors' `inhibit-debugger'
;; (setq debug-on-error t)
;; (setq debug-on-error nil)
(toggle-debug-on-error t)


 
;;; ==============================
;;; :NOTE The following three files are loaded in succession to bring up a sane Emacsen on Mon systems.
;;; :FILE site-local-private.el defines the following global variables which are used to pull private and
;;; site-specific configs that we don't necessarily want to share publically or
;;; which are otherwise irrelevant to others:
;;; `*IS-MON-OBARRAY*'  <- tells us if this is our system. we intern symbols here instead of into the global obarray
;;; `*mon-emacsd*' <- Alist to encapusulate common site local and default system paths
;;; `*mon-misc-path-alist*' <- alist of miscellaneous paths not available on all MON systems
;;; `*MON-NAME*' <- List of MON nameform representations needed with various `mon-*' functions.
;;; `*MON-ORG-NAME*' <- "List mapping key (integer) to values (strings) either an organization or URL.
;;;  `*mon-lisp-safe-local-variable-values*' <- List of `safe-local-variable-values' appearing in Common Lisp files.
;;; 
;;; `mon-user-name-conditionals'   <- interrogates the current environment to determine if it is a MON system.
;;; `mon-system-type-conditionals' <- interrogates the current environment to determine which type of MON system we're on.
;;; `mon-gnu-system-conditionals'  <- helps setup the environment for a GNU based MON system.
;;;
;;; :SEE the DESCRIPTION section at header of :FILE mon-default-loads.el and :FILE mon-default-start-loads.el
;;; for additional discussion of how these files configure the remainder of the MON system configs.

(load "site-local-private.el")
(load "mon-default-loads.el")
(load "mon-default-start-loads.el")

;;; ================================================================
;;; MON .emacs ends here
;;; EOF
