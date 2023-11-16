<pre>
;; -*- lexical-binding: t; -*-
;; .emacs.d/init.el, fichier de configuration d'emacs de Dom
;;
;; MODE D'EMPLOI
;;
;; Ce fichier d'initialisation d'Emacs s'appuie sur le module
;; `use-package', qu'il installe par ses propres moyens, pour
;; l'auto-installation et la configuration des dépendances. (Chercher
;; le mot «use-package» et ci-dessous pour voir les détails.)
;;
;; HISTORIQUE
;;
;; Tiré du .emacs de la config conscrits de l'ENS en 1997, et
;; continuellement adapté depuis. En 2022, testé pour Emacs 28.

;;;;;;;;;;;;;;;;;;; Support interne ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Si vous éditez ce .emacs manuellement, merci de ne rien toucher
;; ici; passer directement à la section suivante.

;;; Code:

(eval-when-compile
  (require 'cl-lib)  ;; Petites gâteries Lisp comme "cl-some"
  (require 'rx))     ;; Regexps faciles
(when (string-equal default-directory "/") (cd (getenv "HOME")))

(defvar pointemacs-progression nil)
(defvar pointemacs-progression-horodatage nil)
(defun pointemacs-progression (texte)
  "Affiche la progression du chargement du .emacs, si
`pointemacs-afficher-progression' est réglé à une valeur vraie"
  (when (and (boundp 'pointemacs-afficher-progression)
             pointemacs-afficher-progression)
    (let ((derniere-entree (last pointemacs-progression))
          (horodatage (float-time))
          (textecomplet (concat ".emacs : " texte)))
      (when (and derniere-entree pointemacs-progression-horodatage)
        (setcar derniere-entree (list (car derniere-entree)
                     (- horodatage pointemacs-progression-horodatage))))
      (setq pointemacs-progression-horodatage horodatage
            pointemacs-progression (append pointemacs-progression (list texte)))
      (message textecomplet))))

(defcustom pointemacs-afficher-progression nil
"Doit-on afficher des messages de progression pendant le chargement du .emacs ?"
:type '(boolean)
:group 'pointemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (pointemacs-progression "Paquetages, modules et customizations elisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dans cette section, les chemins d'accès doivent être adaptés en
;; fonction de l'architecture (distribution / OS / préférences pour
;; l'organisation du répertoire personnel).

(add-to-list 'set-message-functions 'inhibit-message)
(add-to-list 'inhibit-message-regexps
             (rx string-start "Loading" (+ anything) "..."))

;; `custom-file' est le fichier dans lequel Emacs sauvegarde les
;; customizations faites avec l'interface graphique.
(require 'custom)
(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(when (file-exists-p custom-file) (load-file custom-file))

(dolist (dir (list "/usr/local/share/emacs/site-lisp"
                   "/usr/share/doc/git-core/contrib/emacs"
                   ;; https://github.com/ocaml-community/utop#integration-with-emacs
                   (replace-regexp-in-string
                    "\n" "/share/emacs/site-lisp"
                    (shell-command-to-string "opam var prefix"))))
  (add-to-list 'load-path (expand-file-name dir)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Chemins des répertoires d'installation de paquetages séparés par
;; version d'Emacs, de façon à faciliter les opérations de mise à
;; jour :
(custom-set-variables
 '(package-user-dir
   (concat user-emacs-directory "elpa-" emacs-version "/"))
 '(quelpa-dir
   (concat user-emacs-directory "quelpa-" emacs-version "/")))
(package-initialize)

;; À partir de ce point, nous appuyons sur use-package et quelpa
;; (et quelpa-use-package).
;; ⚠ Sur Mac OS X, il faut d'abord installer GNU tar (`brew install
;; gnu-tar`)! Voir https://github.com/quelpa/quelpa/issues/221
(let ((inhibit-message t)) (package-install 'quelpa-use-package))
(require 'use-package)
(require 'quelpa-use-package)

(use-package quelpa :config
  (setq quelpa-update-melpa-p nil)
  (add-to-list 'inhibit-message-regexps (rx "Not upgrading." string-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "gccemacs")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables '(native-comp-driver-options '("-Wl,-w")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Autres réglages site-spécifiques")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adresse électronique
(custom-set-variables '(user-mail-address "dominique@quatravaux.org")
                      '(query-user-mail-address nil)
                      '(debian-changelog-mailing-address "dominique.quatravaux@epfl.ch"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Affectation des touches")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Voir aussi les instructions :bind au fil du texte, ci-dessous

(global-set-key [(control home)] 'beginning-of-buffer)
(global-set-key [(control end)] 'end-of-buffer)
(global-set-key [(f9)] 'compile)  ;; Nostalgie de Turbo Pascal...
(make-variable-buffer-local 'compile-command)
(unless (require 'kmacro nil t)
  (global-set-key [(f4)] 'call-last-kbd-macro))
(global-set-key [(f5)]
  (lambda () (interactive) (revert-buffer nil (not (buffer-modified-p)))))
(global-set-key [(meta g)] 'goto-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [(control backspace)] 'backward-kill-word)
;; Le setting par défaut est ridicule en mode graphique:
(when window-system (global-set-key [(control z)] 'undo))
(global-set-key [(control c) (control l)] 'org-store-link)
(global-set-key [(control c) (.)] 'org-time-stamp)

;; Rotation des buffers avec Ctrl-Arrêtes de poisson
(use-package  prev-next-buffer ;; Ce fichier se trouve sur emacswiki.org
  :bind (([(control prior)] . previous-buffer)
         ([(control pgup)]  . previous-buffer)
         ([(control next)]  . next-buffer)
         ([(control pgdn)]  . next-buffer)))

;; La touche Backspace sert à effacer un caractère à GAUCHE du curseur, BDM !
(global-set-key [backspace] 'delete-backward-char)
(global-set-key [delete] 'delete-char)
(define-key isearch-mode-map [backspace] 'isearch-delete-char)
(when (boundp 'shared-lisp-mode-map)
  (define-key shared-lisp-mode-map [delete] 'delete-char))

;;;;;;;; Suppression des bindings insupportables
(global-unset-key [(control x) (control q)]) ;; Trop proche de Ctrl-x Ctrl-s
(global-unset-key [(control x) s]) ;; Idem
(global-unset-key [(meta <)]) ;; Gros doigts sur la touche Alt
(global-unset-key [(control /)]) ;; Trop proche de méta-slash qui sert
                                 ;; pour de vrai (pour la complétion)
;; Moi quand je veux icônifier ou maximiser, j'utilise le window-manager:
(global-unset-key [(f11)])
(global-unset-key [(control x) (control z)])

;; Combinaison utile mais désactivée par défaut, parce que son effet
;; est désorientant.
(put 'narrow-to-region  'disabled nil)

;; C-c C-l déjà utilisé dans d'autres modes, mais je veux la version d'org-mode
(dolist (cons-mode-map `((cc-mode . c-mode-base-map)
                         (python-mode . python-mode-map)
                         (python . python-mode-map)
                         (python . python-ts-mode-map)
                         (sh-script . sh-mode-map)
                         (web-mode . web-mode-map)))
  (with-eval-after-load (car cons-mode-map)
    (define-key (symbol-value (cdr cons-mode-map))
      [(control c) (control l)] nil)))

;; Combinaisons Compose supplémentaires
;; Voir aussi ~/Library/KeyBindings/DefaultKeyBinding.dict et
;; http://lolengine.net/blog/2012/06/17/compose-key-on-os-x
(use-package iso-transl
  :config
  (let ((combinaisons
         '(("->" . [?→]) ("=>" . [?⇒]) ("<-" . [?←])
           ("tm" . [?™]) ("TM" . [?™])
           (":)" . [?☺]) (":(" . [?☹]) (",/" . [?✓]) (",x" . [?✗]) ("xx" . [?×])
           ("/!" . [?⚠]) ("O!" . [?💡]) ("O3" . [?☁]) (".." . [?…])
           ("!?" . [?‽]) ("?!" . [?⸘]) ("??" . [?¿]) ("!!" . [?¡])
           ("(0)" . [?⓪]) ("(1)" . [?①]) ("(2)" . [?②]) ("(3)" . [?③])
           ("(4)" . [?④]) ("(5)" . [?⑤]) ("(6)" . [?⑥]) ("(7)" . [?⑦])
           ("(8)" . [?⑧]) ("(9)" . [?⑨]) ("(10)" . [?⑩]) ("(11)" . [?⑪]) ("(12)" . [?⑫])
           ("(A)" . [?Ⓐ]) ("(B)" . [?Ⓑ]) ("(C)" . [?Ⓒ]) ("(D)" . [?Ⓓ]) ("(E)" . [?Ⓔ])
           ("(F)" . [?Ⓕ]) ("(G)" . [?Ⓖ]) ("(H)" . [?Ⓗ]) ("(I)" . [?Ⓘ]) ("(J)" . [?Ⓙ])
           ("(K)" . [?Ⓚ]) ("(L)" . [?Ⓛ]) ("(M)" . [?Ⓜ]) ("(N)" . [?Ⓝ]) ("(O)" . [?Ⓞ])
           ("(P)" . [?Ⓟ]) ("(Q)" . [?Ⓠ]) ("(R)" . [?Ⓡ]) ("(S)" . [?Ⓢ]) ("(T)" . [?Ⓣ])
           ("(U)" . [?Ⓤ]) ("(V)" . [?Ⓥ]) ("(W)" . [?Ⓦ]) ("(X)" . [?Ⓧ]) ("(Y)" . [?Ⓨ]) ("(Z)" . [?ⓩ])
           ("(+)" . [?⊕]) ("O+" . [?⊕]) ("O-" . [?⊖]) ("Mo" . [?⌘]) ("oM" . [?⌘])
           (" 0" . [?⁰]) (" 1" . [?¹]) (" 2" . [?²]) (" 3" . [?³]) (" 4" . [?⁴])
           (" 5" . [?⁵]) (" 6" . [?⁶]) (" 7" . [?⁷]) (" 8" . [?⁸]) (" 9" . [?⁹]))))
    (dolist (combinaison combinaisons)
      (let* ((touches (car combinaison))
             (valeur (cdr combinaison))
             (premiere-touche (substring touches 0 1)))
        (unless (keymapp (lookup-key iso-transl-ctl-x-8-map premiere-touche))
          (define-key iso-transl-ctl-x-8-map premiere-touche
            (make-sparse-keymap)))
        (define-key iso-transl-ctl-x-8-map touches valeur)))))

;; http://www.emacswiki.org/emacs/EmacsForMacOS
(when (eq system-type 'darwin)
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  ;; Control-crochet fermant sur claviers AZERTY et QWERTZ (suisse):
  (global-set-key [C-S-268632091] 'abort-recursive-edit)
  (global-set-key [C-S-268632093] 'abort-recursive-edit)
  ;; Control-x Shift-N remplace C-x ` (touche morte) :
  (global-set-key (kbd "C-x N") 'next-error)
  ;; ⌘ de droite = touche Compose, comme dans Karabiner:
  (define-key key-translation-map (kbd "<f13>") iso-transl-ctl-x-8-map)
  ;; ⌘-x est occupé à quelque chose d'utile, mais ⌘-c et ⌘-v pas vraiment :
  (global-set-key (kbd "M-c") 'kill-ring-save)
  (global-set-key (kbd "M-v") 'yank))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Cycle de vie de l'éditeur")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop : sauvegarde l'état complet de l'éditeur (fenêtres,
;; fichiers ouverts). Une sauvegarde desktop par tâche (chacune dans
;; son répertoire); taper M-x desktop-change-dir pour changer de tâche.
(defvar pointemacs-desktop-history nil)
(defadvice desktop-change-dir (around pointemacs-historique-separe activate)
  (interactive
   (let ((file-name-history pointemacs-desktop-history)
         (ido-work-directory-list pointemacs-desktop-history)
         (default (or (car pointemacs-desktop-history) default-directory))
         valeur-retour)
     (progn
       (setq valeur-retour (read-directory-name "Desktop directory: "))
       (let ((history-delete-duplicates t))
         (add-to-history 'pointemacs-desktop-history valeur-retour))
       (list valeur-retour))))
  (unwind-protect ad-do-it
  (desktop-save-mode 1)))

(custom-set-variables '(desktop-files-not-to-save "^$"))
(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'tramp-remote-path))

;; Sauvegarde des positions du curseur dans les fichiers ouverts.
(use-package saveplace :custom (save-place t))

;; Sauvegarde des historiques dans le minibuffer et autres variables utiles
(use-package savehist
  :config (savehist-mode 1)
  :custom (savehist-additional-variables
           '(find-function-C-source-directory pointemacs-desktop-history)))

;; Signets d'Emacs persistants : taper C-x r m pour poser un signet.
(custom-set-variables '(bookmark-save-flag 1))

;; Confirmation avant de quitter
(custom-set-variables '(confirm-kill-emacs 'yes-or-no-p))

(pointemacs-progression "Mode serveur") ;;;;;;;;;;;;;;;;;;;;;;;

;; Actif uniquement lorsqu'on lance Emacs sans arguments. gnuserv permet à un
;; programme tiers d'exécuter du code Lisp arbitraire, ce qui est fun pour le
;; débogueur Perl forké par ex.  Mais il n'est pas dispo de base sous les vieux
;; emacs.
(when (= 1 (length command-line-args))
  (use-package server
    :config
    (server-start)
    (setq server-temp-file-regexp "^/tmp/Re\\|/draft$\\|COMMIT_EDITMSG")
    (defun server-kill-buffer-query-function () t)
    (custom-set-variables '(server-kill-new-buffers nil))))
  ;; Utiliser ensuite 'emacsclient' comme éditeur (variable EDITOR
  ;; dans son .bash_profile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Traitement des fichiers ouverts par emacs")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mise à jour automatique des fichiers qui changent sur disque
(global-auto-revert-mode 1)
;; Pas de questions intempestives pour les fichiers de git :
(custom-set-variables '(revert-without-query
                        '("COMMIT_EDITMSG" "git-rebase-todo")))

;; Correspondance entre noms de fichiers et encodage
(when (boundp 'auto-coding-alist)
  (setf (alist-get "screenlog\\.[0-9]\\'" auto-coding-alist) 'utf-8-dos))

;; Correspondance entre première ligne et mode. Modifié pour tolérer
;; "#!/bin/env -S", façon https://stackoverflow.com/a/16365367
(setq auto-mode-interpreter-regexp
        (rx (seq "#!"
                 (opt (any " \t"))
                 (group (opt
                       (zero-or-more not-newline)
                       "/bin/env" (any " \t")
                       (opt (seq "-S" (any " \t"))))
                 (group (one-or-more (not (syntax whitespace))))))))

;; Fichiers de sauvegarde centralisés dans un répertoire
(custom-set-variables
 '(backup-directory-alist
   (list (cons "." (concat user-emacs-directory "/var/backups")))))

;; On lance les commandes avec /bin/sh...
(custom-set-variables '(shell-file-name "/bin/sh")
                      '(tex-shell-file-name "/bin/sh")
;; ... mais M-x shell utilise /bin/zsh
                      '(explicit-shell-file-name "/bin/zsh"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (pointemacs-progression "Configuration du minibuffer et de la mode-line")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Le minibuffer est la ligne en bas de l'écran dans laquelle on tape
;; les noms de fichiers à ouvrir, les mots à chercher, etc.
;; La mode-line est la ligne juste au-dessus qui décrit le buffer
(custom-set-variables '(sml/position-percentage-format nil))
(when (fboundp 'sml/setup) (sml/setup))
;; Pas de marqueur "MRev" dans la barre d'état :
(custom-set-variables '(magit-auto-revert-mode-lighter nil))

(use-package delight :ensure)

(use-package doom-modeline
  :ensure t
  :custom (doom-modeline-highlight-modified-buffer-name nil)
  :custom-face
  (mode-line-active ((t (:background "grey85"))))
  (doom-modeline-warning ((t (:inherit doom-modeline :foreground "firebrick1"))))
  :init (doom-modeline-mode 1))
;; Va bien avec M-x nerd-icons-install-fonts

(use-package ido
  :custom
  (ido-everywhere t)
  (ido-auto-merge-work-directories-length -1)
  (ido-confirm-unique-completion t)
  (ido-default-file-method 'selected-window)
  (ido-default-buffer-method 'samewindow)
  (ido-use-filename-at-point 'guess)
  :bind
  (:map ido-common-completion-map
  (" " . nil)
  :map ido-file-dir-completion-map
  ([(control backspace)] . ido-delete-backward-word-updir)
  ("\C-x\C-w" . 'ido-fallback-command)
  :map ido-buffer-completion-map
  ("\C-xb" . 'ido-fallback-command))
  :config
  (pointemacs-progression "Activation de ido pour Emacs")
  (ido-mode 1)
  (defun pointemacs--ido-backspace (func-orig &rest args-orig)
    "Dans ido, change l'action de Backspace au début du minibuffer.
Au lieu d'effacer tout le répertoire précédent, efface seulement le slash."
    (if (eq this-original-command 'ido-delete-backward-word-updir)
        (apply func-orig args-orig)
      (cl-letf (((symbol-function 'exit-minibuffer) 'ignore))
        (apply func-orig nil))
      (let ((dir (directory-file-name ido-current-directory)))
        (setq ido-current-directory (file-name-directory dir))
        (setq ido-text-init (file-name-nondirectory dir))
        (setq ido-exit 'refresh)
        (exit-minibuffer))))
  (advice-add 'ido-up-directory :around #'pointemacs--ido-backspace))

(use-package ido-completing-read+ :ensure
  :config (ido-ubiquitous-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (pointemacs-progression "Personnalisation générale de l'édition de texte")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Les réglages de ce paragraphe sont valables dans pratiquement tous les
;; modes d'emacs, pas juste "text-mode" qui de son côté a une section dans
;; le paragraphe "configuration des modes".

(tool-bar-mode -1)
(setq ignore-window-parameters t)  ;; “Excellent window manager” my foot

;; color-theme-domq, mon thème de couleurs à moi (dispo sur
;; http://www.emacswiki.org/cgi-bin/emacs/color-theme-domq.el) :
(use-package color-theme-domq
  :load-path "color-theme-domq"
  :if (and window-system (string-lessp "22" emacs-version)
           (require 'color-theme nil t)))

;; Fenêtre juste assez large pour deux fichiers en 80 colonnes + diff-hl
(when (and window-system (string-match "^[/:]" (getenv "DISPLAY")))
  (modify-frame-parameters nil '((user-position . t)
                                 (top . 0.5) (left . 0.5)
                                 (width . 166) (height . 60))))
(defadvice split-window-right (before petit-ecran activate)
  "Si l'écran est trop petit, Ctrl-X 3 réserve 80 colonnes du côté gauche."
  (when (and (> (window-width) 80) (<= (window-width) 166))
    (ad-set-arg 0 85)))

;; Font-lock automatiquement allumé dans tous les modes.
(global-font-lock-mode t)

(custom-set-variables
 '(next-line-add-newlines nil)
 '(require-final-newline 1)
 '(indent-tabs-mode nil)
 '(default-major-mode 'text-mode)
 '(sentence-end-double-space nil)) ;; Personne ne tape comme ça, mon pauvre RMS

;; Réglages de l'interface graphique d'Emacs
(custom-set-variables '(inhibit-startup-screen t))
(setq
      visible-bell t
      truncate-partial-width-windows nil)
(line-number-mode 1)
(column-number-mode 1)

;; Support accents et UTF-8
(prefer-coding-system 'utf-8)

;; Comportement type de la région (c'est-à-dire la zone de texte
;; sélectionnée, lorsqu'il y en a une).
(when transient-mark-mode   ;; Seulement si la sélection est *visible* :
  (custom-set-variables '(mark-even-if-inactive nil)
                        '(delete-selection-mode 1)))

;; Flyspell
(with-eval-after-load 'flyspell
  (when (eq system-type 'darwin)
    (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
    (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(use-package treesit-auto
  :quelpa (treesit-auto :fetcher github :repo "domq/treesit-auto")
  :load-path (lambda () (expand-file-name "treesit-auto" quelpa-build-dir))
  :config
  (setq treesit-auto-langs '(bash go gomod)
        treesit-auto-install t   ;; ⚠ Ne suffit pas toujours ⇒
                                 ;; M-x treesit-auto-install-all
        global-treesit-auto-modes '(sh-mode go-mode go-ts-mode))
  (global-treesit-auto-mode))

;; yasnippets
(use-package yasnippet
  :ensure
  :delight (yas-minor-mode " ✄")
  :custom (yas-global-mode t) (yas-verbosity 2))
(use-package yasnippet-snippets :ensure)

;; Complétion automatique du texte (méta-/ et control-méta-/)
(with-eval-after-load 'dabbrev
   (defalias 'dabbrev-expand 'hippie-expand)) ;; Trouve plus de complétions
(custom-set-variables '(dabbrev-upcase-means-case-search t))

;; Noms de buffer uniques mais utiles (en lieu et place de "Makefile<2>"):
(use-package uniquify
  :custom (uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package dtrt-indent
  :quelpa
  :custom (dtrt-indent-global-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Configuration des modes")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pointemacs-progression "Mode C") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remarque : le mode C (cc-mode) est le prototype de nombreux autres modes
;; pour la colorisation syntaxique et l'indentation; les réglages ci-dessous
;; ont donc un effet large (notamment pour PHP, Perl).

(add-hook 'after-change-major-mode-hook
          (lambda () (when (fboundp 'c-toggle-electric-state)
                       (c-toggle-electric-state -1))))
;; ... mais `c-toggle-electric-state' se prend les pieds le tapis au
;; moment de mettre à jour la modeline (à partir d'Emacs 27 ou 28):
(advice-add 'c-update-modeline
            :before-while #'(lambda () (stringp mode-name))
            '((name . pointemacs-seulement-si-mode-name-est-une-chaine)))

(put 'c-indentation-style 'safe-local-variable
     #'(lambda (sym)
         (and (symbolp sym) (boundp 'c-style-alist)
              (assoc (symbol-name sym) c-style-alist))))

(pointemacs-progression "Mode org") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://lists.gnu.org/archive/html/emacs-orgmode/2011-10/msg00304.html
(use-package org
  :ensure
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-startup-truncated nil)
  (org-lowest-priority ?D)
  (org-src-fontify-natively t)
  (org-babel-load-languages
   ;; C-c C-c sur un bloc de code Perl ou R pour l'exécuter ! Yow !!
   (cl-list* (cons 'perl t) (cons 'shell t) (cons 'R t)
             (eval (car (get 'org-babel-load-languages 'standard-value)))))
  :config
  (setq org-ctags-enabled-p nil)
  (use-package ox-md :quelpa)        ;; Traduction vers MarkDown
  (use-package ox-reveal :quelpa)
  (use-package org-mouse :quelpa)
  ;; https://emacs.stackexchange.com/questions/19169/org-mode-execute-region-in-session
  (advice-add 'org-babel--normalize-body
            :filter-return #'(lambda (orig-result)
                               "Return contents of region if it is active."
                               (if (use-region-p)
                                   (buffer-substring (region-beginning) (region-end))
                                 orig-result)))
  :hook
  (org-mode . (lambda () (electric-indent-mode -1))))

(use-package org-screen            ;; Liens vers des sessions "screen" ! Yow!
  :quelpa
  (org-screen
   :fetcher url
   :url "https://git.sr.ht/~bzg/org-contrib/blob/master/lisp/org-screen.el"))

(use-package ol-git-link          ;; Liens vers des révisions dans git !
  :quelpa
  (ol-git-link
   :fetcher url
   :url "https://git.sr.ht/~bzg/org-contrib/blob/master/lisp/ol-git-link.el")
  :config
  ;; Désactivé par défaut : mettre un préfixe C-u pour créer un lien git.
  (advice-add 'org-git-store-link :before-while
              #'(lambda (&rest _r) current-prefix-arg)))

;; Fabrication de liens en syntaxe org (associé à Ctrl-C Ctrl-L,
;; cf. «Affectation des touches» ci-dessus)
(autoload 'org-store-link "org" "Store an org-link to the current location." t)

;; Vu sur http://www.mail-archive.com/emacs-orgmode@gnu.org/msg06568.html
(add-hook 'org-create-file-search-functions
 (lambda ()
   "Crée des liens avec un numéro de ligne pour les fichiers de code source."
   (when (memq major-mode '(c-mode c++-mode java-mode python-mode sh-mode))
     ;; Je préfère éditer la description des liens manuellement, plutôt que
     ;; de la saisir au minibuffer.
     (setq description "")
     (number-to-string (org-current-line)))))

(pointemacs-progression "Mode texte") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package text-mode   ;; Le plus simple
  :mode ("/TODO\\'" ;; Todoo suxx
         "/DefaultKeyBinding.dict\\'"))

(pointemacs-progression "Modes pour le Web") ;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'xml-mode 'nxml-mode)
(use-package nxml-mode
  :mode ("\\.xml\\'" "\\.xhtml\\'" "\\.xmlinc\\'" "\\.xsd\\'"
         "\\.sch\\'" "\\.rng\\'" "\\.xslt\\'" "\\.svg\\'" "\\.rss\\'")
  :magic-fallback "<\\?xml")

(use-package web-mode
  :ensure
  :mode "\\.html?\\'"
  :bind (("C-c /" . web-mode-element-close))
  :hook (web-mode . pointemacs-web-mode-narrow-reveal-js)  ;; Définie ci-dessous
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-indentation nil))
(defun pointemacs-web-mode-narrow-reveal-js ()
  "Si le fichier HTML est un slide reveal.js, masque la cuisine."
  (interactive)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "div class=\"reveal\"")
        (let ((narrow-from (point-at-bol)))
          (web-mode-element-end)
          (narrow-to-region narrow-from (point-at-bol 2))))))

(use-package vue-mode :ensure :mode "\\.vue\\'")
(use-package ssass-mode :ensure :mode "\\.scss\\'")

(use-package js-mode
  :mode ("\\.js\\'" "\\.gypi\\$")
  :interpreter (rx (or "node" "npm"))
  :custom (js-indent-level 2) (js-auto-indent-flag nil)
  :config
  (put 'js-indent-level 'safe-local-variable 'integerp))

(use-package typescript-mode
  :ensure
  :mode "\\.tsx?\\'"
  :delight "ts"
  :custom (typescript-indent-level 2))

(use-package json-mode :ensure :mode "\\.babelrc\\'")

(pointemacs-progression "OCaml / ReasonML / BuckleScript") ;;;;;;;;;;;;

(use-package utop
  :ensure
  :commands utop
  :hook (tuareg-mode . utop-minor-mode)
  :config
  (setq utop-command "opam config exec -- dune utop . -- -emacs"))

(pointemacs-progression "Shell") ;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pointemacs:remplace-dans-alist (alist from to)
  "Remplace FROM par TO dans tous les cdr des éléments de ALIST."
  (when (symbolp alist) (setq alist (symbol-value alist)))
  (dolist (e alist) (when (eq from (cdr e)) (setcdr e to))))

(pointemacs-progression "Perl") ;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'perl-mode 'cperl-mode)
(use-package cperl-mode
  :ensure
  :commands cperl-mode
  :mode  ("\\.pl\\'" "\\.PL\\'" "\\.pm\\'" "\\.t\\'" "\\.pod\\'" "\\.cgi\\'")
  :interpreter "perl"
  :config
  ;; “electric” === “too much magic”
  (put 'cperl-toggle-electric 'disabled t)
  (define-key cperl-mode-map ":" 'self-insert-command))

;; Coding style façon IDEALX - Nostalgie !
(custom-set-variables '(cperl-indent-level 2)
		      '(cperl-label-offset 0)
		      '(cperl-continued-statement-offset 2)
                      '(cperl-indent-parens-as-block t)
		      '(cperl-highlight-variables-indiscriminately 1))

(use-package cperl-domq :load-path "cperl-domq")

(pointemacs-progression "Python / IPython / Jinja") ;;;;;;;;;;;;;;;;;;

(use-package python-mode
  :ensure
  :mode "\\.py\\'"
  :interpreter "python"
  :custom
  (python-shell-interpreter "python3"))

(defun pointemacs-python-ts-mode-vigipirate (python-ts-mode)
    "Empêche `python-ts-mode' de s'installer lui-même dans les alists de mode."
    (let (auto-mode-alist interpreter-mode-alist) (funcall python-ts-mode)))
(advice-add 'python-ts-mode :around #'pointemacs-python-ts-mode-vigipirate)

(defun ipython () "Run ipython." (interactive) (term "ipython3"))

(use-package jinja2-mode
  :quelpa
  :mode "\\.j2\\'")

(pointemacs-progression "YAML") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :ensure
  :hook (yaml-mode . (lambda () (electric-indent-mode -1))))

(pointemacs-progression "Modes R") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-x w sur un symbole d'un data.frame dans le buffer *R*, l'ouvre
;; dans LibreOffice ! Si on utilise C-x q à la place de C-x w, on peut
;; même modifier, sauvegarder, et récupérer les données modifiées dans
;; R ! https://github.com/GioBo/ess-view
(use-package ess-view
  :ensure
  :commands (ess-view-inspect-df ess-view-inspect-and-save-df)
  :hook (inferior-ess-r-mode . (lambda () (require 'ess-view)))
  :config (setq ess-view--spreadsheet-program
                "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
(advice-add 'ess-view-extract-R-process
            :filter-return (lambda (process)
                             (or process ess-local-process-name))
            '((name . ess-view-même-process-que-ess)))

(use-package poly-R :ensure)
(with-eval-after-load 'polymode-core
  (setf (alist-get 'd3 polymode-mode-name-aliases)  'js-mode))

(pointemacs-progression "PHP") ;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package php-mode :ensure
  :mode ("\\.php\\'" "\\.inc\\'")
  :hook (php-mode . (lambda () (setq c-basic-offset 2))))

(with-eval-after-load 'ffap
  (setf (alist-get 'php-mode ffap-alist) 'pointemacs:ffap-avec-point)
  (defun pointemacs:ffap-avec-point (name)
    "Fait marcher `find-file-at-point' avec un \"faux\" chemin absolu à la PHP.
Fait fonctioner find-file-at-point sur des constructions du genre

   require_once(__DIR__ + \"/where/ever.php\");"
    (save-excursion
      (let ((slash-pos (car ffap-string-at-point-region)))
        (and
         (goto-char slash-pos)
         (looking-at "/")
         (re-search-backward "__DIR__\\|__FILE__" nil t)
         (equal (line-number-at-pos) (line-number-at-pos slash-pos))
         (concat "." name))))))

(pointemacs-progression "Autres langages de programmation") ;;;;;;;;;;;;;;;

(use-package elisp-mode :delight (eldoc-mode))
(use-package go-ts-mode :ensure :mode "\\.go")
(use-package groovy-mode :ensure :mode "enkinsfile")
(use-package java-mode :mode "\\.jj\\'")
(use-package rust-mode :ensure :mode "\\.rs\\'")
(use-package cmake-mode :ensure)

(pointemacs-progression "Petits modes en vrac") ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mail-mode :quelpa :mode "^/tmp/mutt")
(use-package dockerfile-mode
  :ensure
  :delight ""
  :mode "Dockerfile")
(use-package strace-mode :ensure :mode "strace")
(use-package conf-mode :mode "\\.env\\'")
(use-package diff-mode :mode "\\.dpatch")
(use-package protobuf-mode :ensure :delight "➿" :mode "\\.proto\\'")
(use-package emacs-wiki :quelpa :commands emacs-wiki-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Environnement de Développement Intégré")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ici on configure tout ce qui permet à Emacs de parler avec des débogueurs,
;; des indexeurs de code source, des systèmes de contrôle de version, etc.

(use-package compile-domq :load-path "compile-domq")

(which-function-mode t)

(use-package company :ensure
  :delight ""
  :config (global-company-mode '(not python-mode))
  :custom (company-tooltip-limit 10))

(use-package eglot
  :hook (typescript-ts-mode . eglot-ensure)
  (typescript-mode . eglot-ensure))

(use-package info :delight (Info-mode "ⓘ"))

(pointemacs-progression "IBuffer") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ibuffer
  :ensure
  :bind (([(control x) (control b)] . ibuffer)))

(pointemacs-progression "Pliage du code source") ;;;;;;;;;;;;;;;;;;;;;;

;; C-x $ plie / déplie à partir de la colonne où se trouve le curseur.
(advice-add 'set-selective-display
            :filter-args (lambda (args)
                           (if (or (car args) selective-display)
                               args
                             (list (1+ (current-column)))))
            '((name . set-selective-display-colonne-courante)))

(pointemacs-progression "Recherche dans le code source") ;;;;;;;;;;;;;;

(custom-set-variables '(grep-program "zgrep")
                      '(wgrep-auto-save-buffer t))

(use-package ag :ensure
  :custom (ag-highlight-search t))
(use-package wgrep-ag
  :ensure
  :hook ((ag . wgrep-ag-setup))
  :config
  ;; Bugware : wgrep-ag s'imagine qu'un bloc de résultats ne fait jamais plus d'une ligne.
  (defalias 'wgrep-ag-prepare-header/footer 'ignore))

(use-package iedit :ensure
  :init
  (add-to-list 'inhibit-message-regexps
             (rx string-start "Iedit default key binding is C-;")))

(pointemacs-progression "Contrôle de versions") ;;;;;;;;;;;;;;;;;;;;;;;

(let* ((vcs-utilises '(Git RCS Hg Bzr SVN CVS))
       (vcs-installes
        (cl-remove-if-not
         (lambda (vc) (locate-library (downcase (format "vc-%s" vc))))
         vcs-utilises)))
  (custom-set-variables `(vc-handled-backends '(,@vcs-installes))))

(use-package vc-git-out-of-tree
  :load-path (lambda () (concat user-emacs-directory "vc-git-out-of-tree")))

;; Certains environnements de dév pour langages de script «compilent»
;; en tirant des liens symboliques :
(custom-set-variables '(vc-follow-symlinks t))

;; Par défaut sous emacs, vc-cvs fiche le bazar en matière de copies de
;; secours. Ceci désactive les copies en "machin.txt.~1.1~"...
(with-eval-after-load 'vc-cvs
  (defun vc-cvs-make-version-backups-p (ignored) nil))

;; ... et ceci réactive les copies de secours ordinaires pour les
;; fichiers sous CVS / RCS. C'est alors le comportement ordinaire,
;; (fichiers tilde ou un répertoire de fichiers de secours, selon la
;; valeur de backup-directory-alist ci-dessus), qui prend effet.
(custom-set-variables '(vc-make-backup-files t))

(pointemacs-progression "Git") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package git-commit
  :ensure
  :custom (git-commit-major-mode 'markdown-mode)
  :config
  (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill))

;; git-gutter (https://github.com/syohex/emacs-git-gutter)
(use-package git-gutter
  :ensure
  :delight git-gutter-mode   ;; Pas de marque dans la mode-line
  :config
  (setq git-gutter:update-hooks
        (cl-union git-gutter:update-hooks
                  '(vc-post-command-functions
                    magit-post-refresh-hook)))
  (global-git-gutter-mode t))

(defun pointemacs-vc-next-action (amend)
  "\\[vc-next-action] amélioré pour git.
\\[universal-argument] \\[vc-next-action] effectue un \"git commit --amend\".
Si l'index contient des modifications, appelle `magit-commit' (ce qui va
bien après avoir fait \\[git-gutter-stage-hunks])."
  (interactive "P")
  (with-current-buffer (current-buffer)
    (while (and (boundp 'vc-parent-buffer) vc-parent-buffer)
      (set-buffer vc-parent-buffer))
    (cond
     ((not (eq 'Git (vc-backend buffer-file-name)))
      (vc-next-action amend))  ;; Keep C-U semantics of other VC system
     ((and (fboundp 'magit-commit)
           ;; http://stackoverflow.com/questions/2657935
           (= 1 (vc-git-command nil 1 nil
                                "diff-index" "--quiet" "--cached" "HEAD")))
      (magit-commit))
     ((and amend buffer-file-name)
      (message "Checking in %s (amend)...done" buffer-file-name)
      (vc-git-command nil 0 buffer-file-name
                      "commit" "--amend" "-C" "HEAD" "--")
      (when (fboundp 'git-gutter) (git-gutter)))
     (t
      (vc-next-action nil)))))

(with-eval-after-load 'vc-hooks
  (define-key vc-prefix-map "v" 'pointemacs-vc-next-action))

(use-package magit :ensure
  :bind
  ([(control x) (v) (b)] . magit-status)
  :config
  (add-hook 'magit-mode-hook (lambda () (setq truncate-lines nil)))
  (add-hook 'magit-log-edit-mode-hook
            (lambda () (setq fill-paragraph-function nil))))

(pointemacs-progression "Linters / flycheck") ;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure
  :config (global-flycheck-mode 1)
  :custom
  (flycheck-global-modes '(not web-mode python-mode))
  (flycheck-ruby-rubocop-executable "/usr/local/Homebrew/Library/Homebrew/shims/gems/rubocop"))

(pointemacs-progression "direnv") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package direnv :ensure :config (direnv-mode))

(pointemacs-progression "Débogueurs") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; https://stackoverflow.com/a/29687732/435004
(advice-add 'set-window-dedicated-p
            :override (lambda (window flag) flag)
            '((name .  pointemacs-set-window-dedicated-p-sans-effet)))

;; Débogueur elisp interne d'Emacs
(custom-set-variables '(eval-expression-debug-on-error nil))
;; Pas de limite de taille pour afficher les valeurs Lisp:
(custom-set-variables '(eval-expression-print-length nil)
                      '(eval-expression-print-level nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Goodies")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Les "goodies" sont les fonctions d'Emacs qui ne sont pas des modes
;; d'édition (ex: le docteur, le serveur d'édition, etc.). Qui a dit
;; qu'Emacs, à l'instar d'UNIX, était un bon système d'exploitation
;; auquel il ne manquait qu'un bon éditeur de texte ? :-)

;; Impression : sous Linux j'utilise kprinter qui à l'inverse de l'icône «print
;; buffer» de la barre d'outils a l'avantage de posséder un bouton «annuler»...
(custom-set-variables '(ps-paper-type (quote a4)))
(let ((kprinter (cl-some 'executable-find '("kprinter" "gtklp"))))
  (when kprinter (custom-set-variables `(lpr-command ,kprinter))))

;; Dired (gestionnaire de fichiers)
(use-package dired
  :hook ((dired-mode . (lambda () (require 'dired-x))))
  :config
  (setq dired-omit-size-limit nil)
  (when (eq system-type 'darwin)
    (when (locate-file "gtar" exec-path)
      (setq dired-guess-shell-gnutar "gtar"))
    (when (locate-file "gls" exec-path)
      (setq insert-directory-program "gls"))))

;; shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(use-package with-editor
  :hook ((shell-mode . with-editor-export-editor)))

(pointemacs-progression "Tramp") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tramp
  :config
  (dolist (exported '("GIT_AUTHOR_NAME" "GIT_AUTHOR_EMAIL" "GIT_COMMITTER_EMAIL"))
    (let ((value (getenv exported)))
      (setq tramp-remote-process-environment
            (setenv-internal tramp-remote-process-environment exported value t))))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  :custom (tramp-kubernetes-namespace nil))

(pointemacs-progression "recentf") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Liste File -> Open Recent..., commune à tous les desktops.
(use-package recentf
  :init
  (dolist (it (list (rx "Wrote" (+ anything) "recentf" string-end)
                    (rx "Cleaning up the recentf list...")))
    (add-to-list 'inhibit-message-regexps it))
  :config
  (recentf-mode 1)
  (run-at-time 1 60 #'recentf-save-list)
  :custom
  (recentf-menu-open-all-flag t)
  (recentf-max-saved-items 100)
  (recentf-exclude
   '("/[.]recentf\\'" "/[.]ido[.]last\\'" "[.]rnc\\'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Chargement du .emacs terminé")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
;; End:
(provide 'init)
;;; init.el ends here

</pre>
