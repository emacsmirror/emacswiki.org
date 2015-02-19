;;; -*- emacs-lisp -*-
;; .emacs, fichier de configuration d'emacs de Dom - $Revision: 1.281 $
;;
;; MODE D'EMPLOI
;;
;; Normalement, ce .emacs fonctionne tel quel sur toute installation fraîche
;; d'Emacs 23 ou ultérieur.
;;
;; Ce .emacs fonctionne main dans la main avec la fonction "Customize" d'Emacs,
;; de sorte qu'il est possible de tout personnaliser sans modifier ni même
;; comprendre l'Emacs Lisp.  Essayez les combinaisons de touches suivantes :
;;
;;   <M-x> customize-variable <Entrée> load-path <Entrée>
;;   <M-x> customize-group <Entrée> pointemacs <Entrée>
;;
;; Naturellement on peut aussi vouloir modifier ce .emacs «à la main»; à cet
;; effet, les paragraphes sont (presque) classés par ordre pédagogique.  Le Wiki
;; Emacs (http://www.emacswiki.org/) est une ressource irremplaçable dans une
;; telle entreprise.  Sinon, "t" veut dire vrai, "nil" veut dire faux, à vos
;; crayons !
;;
;; HISTORIQUE
;;
;; Tiré du .emacs de la config conscrits de l'ENS en 1997, et continuellement
;; adapté depuis.

;;;;;;;;;;;;;;;;;;; Support interne ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Si vous éditez ce .emacs manuellement, merci de ne rien toucher
;; ici; passer directement à la section suivante.

(require 'cl)     ;; Petites gâteries Lisp comme "some" ou "remove-duplicates"
(require 'assoc)  ;; Encore des gâteries pour modifier les alists

(setq pointemacs-progression nil pointemacs-progression-horodatage nil)
(defun pointemacs-progression (texte)
  "Affiche la progression du chargement du .emacs, si
`pointemacs-afficher-progression' est réglé à une valeur vraie"
  (when (and (boundp 'pointemacs-afficher-progression)
             pointemacs-afficher-progression)
    (let ((derniere-entree (last pointemacs-progression))
          (horodatage (float-time))
          (textecomplet (concat ".emacs : " texte)))
      (if (and derniere-entree pointemacs-progression-horodatage)
          (setcar derniere-entree (list (car derniere-entree)
                     (- horodatage pointemacs-progression-horodatage))))
      (setq pointemacs-progression-horodatage horodatage
            pointemacs-progression (append pointemacs-progression (list texte)))
      (message textecomplet))))

(defun require-faible (pointemacs-symbol)
  "Comme `require', mais renvoie nil si le paquetage n'existe pas."
  (condition-case error
      (progn (require pointemacs-symbol) t)
    (file-error
     (if (not (member (symbol-name pointemacs-symbol) error))
         ;; Relance l'erreur si elle ne concerne pas ce fichier
         (signal 'file-error error)
       nil))))

(defun pointemacs-choisir-commande (&rest commandes)
  "Trouve une des commandes dans le PATH, par ordre de préférence."
  (if (not (fboundp 'locate-file))
      (car (last commandes))  ;; Au petit bonheur
    (some (lambda (cmd) (and (locate-file cmd exec-path) cmd)) commandes)))

;; Le .emacs lui-même est customizable !
(require 'custom)
(deftheme pointemacs "Réglages du .emacs; l'utilisateur peut y déroger.")
(defun pointemacs-personnalise (&rest args)
  "Fixe un réglage par défaut dans le thème `pointemacs', comme
`custom-set-variables' le fait dans le thème `user'."
  (apply #'custom-theme-set-variables 'pointemacs args))

(defcustom pointemacs-afficher-progression nil
"Doit-on afficher des messages de progression pendant le chargement du .emacs ?"
:type '(boolean)
:group 'pointemacs)

;; Rendons la variable `load-path' customizable, c'est plus convy :
(defcustom load-path `(list ,@load-path)
  "Répertoire de chargement des fichiers Emacs-Lisp.

Tous les paquetages ne mettent pas leurs fichiers dans l'un des répertoires
standard tels que /usr/share/emacs/site-lisp; dans ce cas il faut rajouter des
répertoires dans cette liste.  On peut également rajouter des répertoires
personnels, comme par exemple \"~/lib/emacs\".
"
  :type '(repeat file)
  :group 'pointemacs
)

(defmacro pointemacs-discretement (&rest body)
  "Invoque BODY sans arguments, en étouffant les messages dans le minibuffer.

Évite d'afficher (par exemple) \"Wrote ~/.emacs-session\" toutes les minutes."
  (if (fboundp 'append-message) ;; XEmacs seulement (pour l'instant ?)
      `(let ((append-message-orig (symbol-function 'append-message)))
         (flet ((append-message (label text &rest stuff)
                                (if (and (eq label 'message)
                                         (string-match "\\`Wrote " text))
                                    nil
                                  (apply append-message-orig
                                         label text stuff))))
           ,@body)))
  ;; Emacs (jusqu'à la version 22) n'a pas de callbacks en Lisp pour les
  ;; messages émis par le code C (dont le fameux "Wrote ~/.emacs-session"); on
  ;; emploie donc une arnaque : la variable executing-kbd-macro les inhibe (!)
  `(let ((executing-kbd-macro t)) ,@body))

(defvar pointemacs-taches-periodiques-hook nil
  "Liste de fonctions à lancer périodiquement et discrètement.")

(add-hook 'after-init-hook
    ;; Remarque : sous XEmacs, (run-at-time nil 60 ...) ne fonctionne pas
    ;; contrairement à ce qui est documenté.
    (lambda () (run-at-time 1 60 (lambda ()
        (pointemacs-discretement
         (run-hooks 'pointemacs-taches-periodiques-hook))))))

(defun pointemacs-sur-tous-les-noeuds (arbre func)
  (cl-subst-if nil #'null arbre :key (lambda (n) (prog1 t (funcall func n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Chemins d'accès")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dans cette section, les chemins d'accès doivent être adaptés en
;; fonction de l'architecture (distribution / OS / préférences pour
;; l'organisation du répertoire personnel).

;; `custom-file' est le fichier dans lequel Emacs sauvegarde les
;; customizations faites avec l'interface graphiques.
(setq custom-file "~/.emacs-custom.el")
(when (file-exists-p custom-file) (load-file custom-file))

(defvar pointemacs-repertoire-lib
  (expand-file-name
   (if (eq system-type 'darwin) "~/Library/emacs" "~/lib/emacs"))
  "Le répertoire personnel pour toutes choses Emacs : extensions,
fichiers temporaires, etc.")

(pointemacs-personnalise '(load-path 
  (let* ((mes-chemins
          (funcall #'nconc
                   (list pointemacs-repertoire-lib)
                   (mapcar (lambda (subdir)
                             (concat pointemacs-repertoire-lib "/" subdir))
                           (list emacs-version "magit" "el-get/el-get"))
                   '("/usr/local/share/emacs/site-lisp"
                     "/usr/share/doc/git-core/contrib/emacs"
                     "/usr/share/org-mode/lisp"))))
    (remove-duplicates (append (mapcar 'expand-file-name mes-chemins)
                               load-path)))))

;; Chemins des répertoires de travail des systèmes de paquetages elisp
(pointemacs-personnalise 
 '(package-user-dir
   (concat pointemacs-repertoire-lib "/elpa-" emacs-version "/"))
 `(auto-install-directory
   ,(concat pointemacs-repertoire-lib "/auto-install-" emacs-version "/"))
 '(el-get-dir (concat pointemacs-repertoire-lib "/el-get")))

(pointemacs-personnalise '(Info-default-directory-list
    (remove-duplicates
     (mapcar 'directory-file-name
             (append '("/usr/share/info" "/opt/local/share/info")
                     Info-default-directory-list
                     (list (expand-file-name "~/lib/info")
                           (concat pointemacs-repertoire-lib "/magit"))))
     :test 'equal :from-end t)))

;; Sous Mac OS X Leopard, Emacs ne reçoit aucun environnement utile au démarrage
;; (cause: launchd en état de mort cérébrale, lisez ses sources !) et du coup le
;; site-start utilise des magouilles plus ou moins infâmes pour régler le PATH,
;; qui est donc généralement FUBAR à ce stade. Le mieux est de recommencer de
;; zéro en invoquant un login shell pour connaître le «vrai» path:
(when (eq system-type 'darwin)
  (let ((path-d-apres-le-shell
         (shell-command-to-string
	  "env PATH= $SHELL -l -c 'echo -n \"$PATH\"'")))
    (setq exec-path (split-string path-d-apres-le-shell ":"))
    (setenv "PATH" path-d-apres-le-shell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Autres réglages site-spécifiques")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adresse électronique
(pointemacs-personnalise '(user-mail-address "domq@cpan.org")
                      '(query-user-mail-address nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Systèmes de paquetages elisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'package-initialize)
  (package-initialize)
  (dolist (archive-supplementaire
           '(("melpa" . "http://melpa.milkbox.net/packages/")
             ;; http://user42.tuxfamily.org/elpa/index.html
             ("user42" . "http://download.tuxfamily.org/user42/elpa/packages/")))
           (add-to-list 'package-archives archive-supplementaire t)))
(when (require-faible 'url) (require-faible 'auto-install))
(when (require-faible 'el-get) (el-get 'sync))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Affectation des touches")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [(control home)] 'beginning-of-buffer)
(global-set-key [(control end)] 'end-of-buffer)
(global-set-key [(control f9)] 'compile)  ;; Nostalgie de Turbo Pascal...
(unless (require-faible 'kmacro)
  (global-set-key [(f4)] 'call-last-kbd-macro))
(global-set-key [(f5)]
  (lambda () (interactive) (revert-buffer nil (not (buffer-modified-p)))))
(global-set-key [(meta g)] 'goto-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [(control backspace)] 'backward-kill-word)
(global-set-key [(control meta \])] 'pointemacs-reaffiche-debogueur)
;; Le setting par défaut est ridicule en mode graphique:
(if window-system (global-set-key [(control z)] 'undo))
(global-set-key [(control c) (control l)] 'org-store-link)
(global-set-key [(control c) (.)] 'org-time-stamp)
(global-set-key [(control x) (v) (b)] 'magit-status)

;; Rotation des buffers avec Ctrl-Arrêtes de poisson
(require-faible 'prev-next-buffer) ;; Ce fichier se trouve sur emacswiki.org
(when (fboundp 'next-buffer)
      (global-set-key [(control prior)] 'previous-buffer)
      (global-set-key [(control pgup)]  'previous-buffer)
      (global-set-key [(control next)]  'next-buffer)
      (global-set-key [(control pgdn)]  'next-buffer))

;; La touche Backspace sert à effacer un caractère à GAUCHE du curseur, BDM !
(global-set-key [backspace] 'delete-backward-char)
(global-set-key [delete] 'delete-char)
(define-key isearch-mode-map [backspace] 'isearch-delete-char)
(setq c-delete-function 'delete-char)
(if (boundp 'shared-lisp-mode-map)
    (define-key shared-lisp-mode-map [delete] 'delete-char))

;;;;;;;; Suppression des bindings insupportables
(global-unset-key [(control x) (control q)]) ;; Trop proche de Ctrl-x Ctrl-s
(global-unset-key [(control x) s]) ;; Idem
(global-unset-key [(meta <)]) ;; Gros doigts sur la touche Alt
(global-unset-key [(control /)]) ;; Trop proche de méta-slash qui sert
                                 ;; pour de vrai (pour la complétion)
;; Moi quand je veux icônifier, j'utilise le window-manager:
(global-unset-key [(control x) (control z)])

;; Control-C Control-E sous CPerl. Je me demande comment je fais pour
;; taper ça par hasard, mais j'y arrive... Et je déteste le résultat.
(put 'cperl-toggle-electric 'disabled t)

;; Ceux qui au contraire sont utiles mais désactivés par défaut, parce que
;; leur effet est désorientant.
(put 'narrow-to-region  'disabled nil)

;; C-c C-l déjà utilisé dans d'autres modes, mais je veux la version d'org-mode
(dolist (cons-mode-map '(("cc-mode" . c-mode-base-map)
                         ("python-mode" . py-mode-map)
                         ("python" . python-mode-map)
                         ("sh-script" . sh-mode-map)))
  (eval-after-load (car cons-mode-map)
    `(define-key ,(cdr cons-mode-map) [(control c) (control l)] nil)))

(add-hook 'cperl-mode-hook (lambda ()
    "Désactivation de certaines touches électriques insupportables"
    (cperl-define-key ":" 'self-insert-command)))

;; http://www.emacswiki.org/emacs/EmacsForMacOS
(when (eq system-type 'darwin)
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  ;; Control-crochet fermant sur claviers AZERTY et QWERTZ (suisse):
  (global-set-key [C-S-268632091] 'abort-recursive-edit)
  (global-set-key [C-S-268632093] 'abort-recursive-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Cycle de vie de l'éditeur")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop : sauvegarde l'état complet de l'éditeur (fenêtres,
;; fichiers ouverts). Une sauvegarde desktop par tâche (chacune dans
;; son répertoire); taper M-x desktop-change-dir pour changer de tâche.
(defvar pointemacs-desktop-history nil)
(defadvice desktop-change-dir (before pointemacs-historique-separe activate)
  (interactive
   (let ((file-name-history pointemacs-desktop-history)
         (ido-work-directory-list pointemacs-desktop-history)
         (default (or (car pointemacs-desktop-history) default-directory))
         valeur-retour)
     (progn
       (setq valeur-retour (read-directory-name "Desktop directory: "))
       (let ((history-delete-duplicates t))
         (add-to-history 'pointemacs-desktop-history valeur-retour))
       (list valeur-retour)))))
(defadvice desktop-change-dir (after pointemacs-save-mode activate)
  "Activation à la demande de desktop-save-mode."
  (desktop-save-mode 1))

;; Liste File -> Open Recent..., commune à tous les desktops.
(when (require-faible 'recentf)
  (recentf-mode 1)
  (add-hook 'pointemacs-taches-periodiques-hook 'recentf-save-list)
  (pointemacs-personnalise '(recentf-menu-open-all-flag t)
                        '(recentf-max-saved-items 100)
                        '(recentf-exclude
                          '("/[.]recentf\\'" "/[.]ido[.]last\\'" "[.]rnc\\'"))))

;; Sauvegarde des positions du curseur dans les fichiers ouverts.
(when (require-faible 'saveplace)
  (pointemacs-personnalise '(save-place t)))

;; Sauvegarde des historiques dans le minibuffer et autres variables utiles
(when (require-faible 'savehist)
  (savehist-mode 1)
  (pointemacs-personnalise '(savehist-additional-variables
                             '(find-function-C-source-directory
                               pointemacs-desktop-history))))

;; Signets d'Emacs persistants :taper C-x r m pour poser un signet.
(pointemacs-personnalise '(bookmark-save-flag 1))

;; Confirmation avant de quitter
(pointemacs-personnalise '(confirm-kill-emacs 'yes-or-no-p))

(pointemacs-progression "Mode serveur") ;;;;;;;;;;;;;;;;;;;;;;;

;; Actif uniquement lorsqu'on lance Emacs sans arguments. gnuserv permet à un
;; programme tiers d'exécuter du code Lisp arbitraire, ce qui est fun pour le
;; débogueur Perl forké par ex.  Mais il n'est pas dispo de base sous les vieux
;; emacs.
(when (= 1 (length command-line-args))
  (when (or (require-faible 'gnuserv-compat) (require-faible 'gnuserv))
    (gnuserv-start))
  (when (require-faible 'server)
    (server-start)
    (defun server-kill-buffer-query-function () t)
    (pointemacs-personnalise '(server-kill-new-buffers nil))
    ;; Intégration avec le plug-in Firefox ItsAllText (que je recommande):
    (add-hook 'server-visit-hook
              (lambda () "Désactive le passage à la ligne automatique pour
                          les pages Wiki"
                (if (and buffer-file-name
                         (string-match "wiki" buffer-file-name))
                    (turn-off-auto-fill)))))
  ;; Utiliser ensuite 'gnuclient' resp. 'emacsclient' comme éditeur
  ;; (variable EDITOR dans son .bash_profile)
  (when (require-faible 'edit-server)  ;; Extension Chrome "Edit with Emacs"
    (pointemacs-personnalise '(edit-server-new-frame nil))
    (edit-server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Traitement des fichiers ouverts par emacs")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mise à jour automatique des fichiers qui changent sur disque
(global-auto-revert-mode 1)
;; Pas de questions intempestives pour les fichiers de git :
(pointemacs-personnalise '(revert-without-query '("COMMIT_EDITMSG")))

;; Correspondance entre noms de fichiers et modes d'édition.  Note : certains
;; modes sont redéfinis plus bas à l'aide de "defalias" (par exemple 'perl-mode
;; sera probablement 'cperl-mode.  L'élégance est critiquable, mais c'est la
;; façon la plus simple de changer un mode *partout* (y compris dans les valeurs
;; déjà présentes au démarrage dans auto-mode-alist, interpreter-mode-alist etc)
(aput 'auto-mode-alist "^/tmp/mutt" 'mail-mode)
(aput 'auto-mode-alist "akefile" 'makefile-mode)
(aput 'auto-mode-alist "TODO" 'text-mode)  ;; Todoo suxx
(dolist (suffixes-et-mode
  '((("ml" "mli" "mly" "mll" "mlp") caml-mode)
    (("m") 'objc-mode) (("p") pascal-mode)
    (("php" "inc") php-mode)
    (("pl" "PL" "pm" "t" "pod" "cgi") perl-mode)
    (("dpatch") 'diff-mode) (("org") org-mode)
    (("xml" "xhtml" "xmlinc" "xsd" "sch" "rng" "xslt" "svg" "rss") xml-mode)
    (("html" "htm" "phtml" "pcomp" "cst") html-mode)
    (("jj") java-mode)
    (("js" "gypi") js-mode)))
  (aput 'auto-mode-alist
        (format "\\.\\(%s\\)$" (regexp-opt (car suffixes-et-mode)))
        (cadr suffixes-et-mode)))

;; Correspondance entre noms de fichiers et encodage
(if (boundp 'auto-coding-alist)
    (aput 'auto-coding-alist "screenlog\\.[0-9]\\'" 'utf-8-dos))

;; Idem mais en se basant sur la première ligne du fichier au
;; format "#!/usr/bin/truc" :
(aput 'interpreter-mode-alist "perl" 'perl-mode)
(aput 'interpreter-mode-alist "node" 'js-mode)

;; Idem mais en se basant sur la première ligne du fichier, peu
;; importe son nom (ne marche que sous les Emacs récents):
(when (boundp 'magic-mode-alist) (aput 'magic-mode-alist "<\\?xml" 'xml-mode))

;; Fichiers de sauvegarde centralisés dans un répertoire
(let ((rep-fichiers-secours (concat pointemacs-repertoire-lib "/var/backups")))
  (pointemacs-personnalise `(backup-directory-alist
                             (list (cons "." ,rep-fichiers-secours)))))

;; Le fait de ne pas utiliser /bin/sh pour lancer des commandes "toutes faites"
;; est un choix, disons... discutable de la part d'Emacs (quid d'un shell qui
;; rame au démarrage comme zsh, ou d'un shell non compatible Bourne qui va
;; se prendre les pieds dans le tapis au moindre caractère spécial ?)
(setq shell-file-name "/bin/sh")
(setq tex-shell-file-name "/bin/sh")

(defadvice shell (around pointemacs-shell-interactif activate)
  (let (explicit-shell-file-name)
    (when (not (and (buffer-file-name) (file-remote-p (buffer-file-name))))
      (setq explicit-shell-file-name "/opt/local/bin/bash"))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (pointemacs-progression "Personnalisation générale de l'édition de texte")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Les réglages de ce paragraphe sont valables dans pratiquement tous les
;; modes d'emacs, pas juste "text-mode" qui de son côté a une section dans
;; le paragraphe "configuration des modes".

(when (fboundp 'tool-bar-mode) (tool-bar-mode 1))

;; color-theme-domq, mon thème de couleurs à moi (dispo sur
;; http://www.emacswiki.org/cgi-bin/emacs/color-theme-domq.el) :
(when (and window-system (string-lessp "22" emacs-version)
           (require-faible 'color-theme)
           (require-faible 'color-theme-domq))
  (color-theme-domq))

;; Fenêtre juste assez large pour deux fichiers en 80 colonnes + git-gutter+
(when (and window-system (string-match "^:" (getenv "DISPLAY")))
  (aput 'initial-frame-alist 'width 166)
  (aput 'initial-frame-alist 'height 60))
;; Vu sur emacswiki.org :
(require-faible 'display-buffer-for-wide-screen)
(defadvice split-window-horizontally (before petit-ecran activate)
  "Si l'écran est trop petit, Ctrl-X 3 réserve 80 colonnes du côté gauche."
  (if (and (> (window-width) 80) (<= (window-width) 166))
      (ad-set-arg 0 85)))

;; Font-lock automatiquement allumé dans tous les modes.
(global-font-lock-mode t)

(pointemacs-personnalise
 '(next-line-add-newlines nil)
 '(require-final-newline 1)
 '(indent-tabs-mode nil) ;; Le monde OSS a fini par se rendre compte
 ;; que les tabs c'est MAL; tant mieux
 '(default-major-mode 'text-mode)
 '(sentence-end-double-space nil)  ;; Personne ne tape comme ça, mon pauvre RMS
)
;; Réglages de l'interface graphique d'Emacs
(setq inhibit-startup-message t ;; Pas de propagande pour le projet
                                ;; GNU, même si
                                ;; pointemacs-afficher-progression est
                                ;; désactivé
      visible-bell t
      truncate-partial-width-windows nil)
(line-number-mode 1)
(column-number-mode 1)

;; Support accents et UTF-8
(prefer-coding-system 'utf-8)

(setq-default ctl-arrow t)	; Les autres caractères de contrôle
				; sont affichés comme ceci : ^@

;; Comportement type de la région (c'est-à-dire la zone de texte
;; sélectionnée, lorsqu'il y en a une).
(defcustom pointemacs-selection-effacable-p
  (or window-system (string< "21" emacs-version))
"Si vrai, la frappe d'un caractère quand la région est active efface celle-ci.

C'est le comportement «naturel» pour les utilisateurs de systèmes
d'exploitation graphiques modernes.  Par défaut, la sélection est
effaçable (valeur t), sauf pour les vieux emacs en mode tty, car
ils sont incapables de marquer visuellement la région (et cette
fonction devient alors dangereuse)."
:group 'pointemacs
:type '(boolean))

(when pointemacs-selection-effacable-p
  (transient-mark-mode 1)
  (delete-selection-mode 1)
  (pointemacs-personnalise '(mark-even-if-inactive nil)))

;; Flyspell
(when (eq system-type 'darwin)
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined))))

;; Complétion automatique du texte (méta-/ et control-méta-/)
(eval-after-load "dabbrev"
   '(defalias 'dabbrev-expand 'hippie-expand)) ;; Trouve plus de complétions
(pointemacs-personnalise '(dabbrev-upcase-means-case-search t))

;; Noms de buffer uniques mais utiles (en lieu et place de "Makefile<2>"):
(if (require-faible 'uniquify)
    (pointemacs-personnalise '(uniquify-buffer-name-style
                            'post-forward-angle-brackets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Configuration du minibuffer")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Le minibuffer est la ligne en bas de l'écran dans laquelle on tape
;; les noms de fichiers à ouvrir, les mots à chercher, etc.

(if (not (require-faible 'ido))
    (progn
      (when (require-faible 'icomplete) (icomplete-mode))
      (setq minibuffer-confirm-incomplete t
            completion-ignored-extensions (append completion-ignored-extensions
                                                  '(".zo" ".zi" ".zix"))))
  (pointemacs-personnalise '(ido-everywhere t)
                        '(ido-auto-merge-work-directories-length -1)
                        '(ido-confirm-unique-completion t)
                        '(ido-default-file-method 'selected-window)
                        '(ido-default-buffer-method 'samewindow)
                        '(ido-use-filename-at-point 'guess))
  (defadvice ffap-file-at-point (after pointemacs-emacs22-bugfix activate)
    "Contourne un bug de ido d'Emacs 22 pour les noms de fichiers sans chemin."
    (if (and ad-return-value (not (string= "" ad-return-value)))
        (setq ad-return-value (expand-file-name ad-return-value))))
  (when (fboundp 'ido-completing-read)
    (defun pointemacs-ido-Mx ()
      "Utilise ido pour compléter à l'invite de M-x"
      (interactive)
      (call-interactively
       (intern (ido-completing-read "M-x "
          (let (cmd-list)
            (mapatoms (lambda (S) (when (commandp S)
                    (setq cmd-list (cons (format "%S" S) cmd-list)))))
            cmd-list)))))
    (global-set-key "\M-x" 'pointemacs-ido-Mx))
  (eval-after-load "ido" '(progn
    (ido-init-completion-maps)
    (defalias 'ido-init-completion-maps 'ignore)
    (define-key ido-common-completion-map " " nil)
    (define-key ido-file-dir-completion-map [(control backspace)]
      'ido-delete-backward-word-updir)
    (define-key ido-file-completion-map "\C-x\C-w" 'ido-fallback-command)
    (define-key ido-buffer-completion-map "\C-xb" 'ido-fallback-command)))
  (defadvice ido-delete-backward-updir (around pointemacs-backspace activate)
    "Dans ido, change l'action de Backspace au début du buffer.
Au lieu d'effacer tout le répertoire précédent, efface seulement le slash"
    (flet ((ido-up-directory (&rest ignored)
            (setq ido-text-init (file-name-nondirectory
                                 (directory-file-name ido-current-directory))
                  ido-current-directory (file-name-directory
                                 (directory-file-name ido-current-directory))
                  ido-exit 'refresh)
            (ido-trace "Trampoline vers le répertoire" ido-current-directory)
            (exit-minibuffer)))
        ad-do-it))
  (pointemacs-progression "Activation de ido pour Emacs")
  (ido-mode 1))

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

(put 'c-indentation-style 'safe-local-variable
     #'(lambda (sym)
         (and (symbolp sym) (boundp 'c-style-alist)
              (assoc (symbol-name sym) c-style-alist))))

(pointemacs-progression "Mode org") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://lists.gnu.org/archive/html/emacs-orgmode/2011-10/msg00304.html
(defalias 'org-called-interactively-p 'called-interactively-p)

(pointemacs-personnalise 
 '(org-startup-truncated nil)
 '(org-lowest-priority ?D)
 '(org-src-fontify-natively t)
 '(org-babel-load-languages
   ;; C-c C-c sur un bloc de code Perl pour l'exécuter ! Yow !!
   (list* (cons 'perl t) (cons 'sh t)
          (eval (car (get 'org-babel-load-languages 'standard-value))))))

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

(eval-after-load "org" '(progn
  (defalias 'org-called-interactively-p 'called-interactively-p)  ;; Oui, encore
  (require-faible 'org-mouse)
  (require-faible 'org-screen)      ;; Liens vers des sessions "screen" ! Yow !
  (when (require-faible 'org-git-link)  ;; Liens vers des révisions dans git !
    (setq org-store-link-functions
          (delete 'org-git-store-link org-store-link-functions)))))

(pointemacs-progression "Modes pour le Web") ;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pour XML, nxml-mode (http://www.emacswiki.org/cgi-bin/wiki/NxmlMode) est le
;; nouvel étalon-or en 2008.
(defalias 'xml-mode 'nxml-mode)

(pointemacs-personnalise '(js-indent-level 4) '(js-auto-indent-flag nil))
(put 'js-indent-level 'safe-local-variable 'integerp)

(pointemacs-progression "Mode Perl") ;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'perl-mode 'cperl-mode)
(autoload 'cerl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(when (require-faible 'ffap-perl-module)
  (put 'ffap-perl-module-path 'safe-local-variable 'listp))

;; Coding style façon IDEALX - Nostalgie !
(pointemacs-personnalise '(cperl-indent-level 2)
		      '(cperl-label-offset 0)
		      '(cperl-continued-statement-offset 2)
                      '(cperl-indent-parens-as-block t)
		      '(cperl-highlight-variables-indiscriminately 1))

(require-faible 'cperl-domq)

(pointemacs-progression "Mode CAML") ;;;;;;;;;;;;;;;;;;;;;;;;;
(require-faible 'caml-font)

(autoload 'caml-mode "caml"
  "Major mode for editing files of input for Caml.")
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
(autoload 'camldebug "camldebug" nil t)

;; Chargement de ProofGeneral, mais en l'empêchant de faire des effets
;; de bord sales
(let ((eval-after-load-alist nil)) (require-faible 'proof-site))

(pointemacs-progression "Mode Erlang") ;;;;;;;;;;;;;;;;;;;;
(require-faible 'erlang-start)

(pointemacs-progression "Mode ReST") ;;;;;;;;;;;;;;;;;;;;
;; Cf. /usr/share/doc/python-docutils/docs/user/emacs.txt.gz
(autoload 'rst-mode "rst" "Major mode for editing reStructuredText documents.")
(eval-after-load "rst"
  ;; J'ajoute le hook à rst-mode-hook de sorte qu'il ne s'active
  ;; *que* pour les fichiers que je veux traiter comme du ReST :
  '(add-hook 'rst-mode-hook 'rst-text-mode-bindings))

(pointemacs-progression "Petits modes en vrac") ;;;;;;;;;;;;;;;;;;;;;;;

;; Mode texte (=édition de fichiers ASCII uniquement):
;; Passage à la ligne automatique
(put 'auto-fill-mode 'safe-local-variable 'booleanp)
(add-hook 'text-mode-hook (lambda () (if (and (boundp 'auto-fill-mode)
                                              (not auto-fill-mode))
                                         'turn-off-auto-fill
                                       'turn-on-auto-fill)))
;; Mais pas pour d'autres modes où il juste m'insupporte :
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; Mode Wiki
(autoload 'emacs-wiki-mode "emacs-wiki" "Major mode to edit Wiki files." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Environnement de Développement Intégré")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ici on configure tout ce qui permet à Emacs de parler avec des débogueurs,
;; des indexeurs de code source, des systèmes de contrôle de version, etc.

;; Chargement de variables «projet» (pour le coding style etc.) dans
;; un fichier .emacs-dirvars
(require-faible 'dirvars)

(pointemacs-progression "Recherche dans le code source") ;;;;;;;;;;;;;;

(pointemacs-personnalise '(grep-program "zgrep"))

(eval-after-load "grep" '(progn
     ;; M-x grep, M-x rgrep : extension des motifs des fichiers consultés.
     (dolist (item '("all" "c")) (adelete 'grep-files-aliases item))
     (aput 'grep-files-aliases "ch" "*.c *.cc *.cpp *.h")

     (defadvice grep-read-files (around pointemacs-wildcards-multiples activate)
       "Sélectionne un wildcard multiple comme valeur par défaut si opportun."
       (let ((wildcard-to-regexp-orig (symbol-function 'wildcard-to-regexp)))
         (flet ((wildcard-to-regexp (wildcards)
                 (concat "\\(" (mapconcat (lambda (wildcard)
                                    (funcall wildcard-to-regexp-orig wildcard))
                                          (split-string wildcards) "\\|")
                         "\\)")))
           ad-do-it)))

     (defadvice grep-compute-defaults (around pointemacs-grep-P activate)
       "Utilisation de grep -P (PCRE) si supporté"
       (let* ((h (intern (or (file-remote-p default-directory) "localhost")))
              (nouveaux-host-defaults (not (assq h grep-host-defaults-alist))))
         ad-do-it
         (when (and nouveaux-host-defaults
                  (grep-probe "grep" `(nil t nil "-P" "." ,null-device) nil 1))
           (pointemacs-sur-tous-les-noeuds
            (assq h grep-host-defaults-alist)
            (lambda (n) (and (consp n) (stringp (car n))
                             (setcar n (replace-regexp-in-string
                                        "grep" "grep -P" (car n))))))
           ad-do-it)))))

(defun pointemacs-grep-find-moins-verbeux ()
  "Rend invisible la liste des exclusions dans la ligne de commande de find."
  (set (make-local-variable 'font-lock-extra-managed-props) '(invisible))
  (add-to-invisibility-spec '(find-prune . t))
  (font-lock-add-keywords
   nil '(("-type d \\\\( \\(.*?\\) \\\\) -prune"
          1 '(face font-lock-comment-face invisible 'find-prune)))))
(add-hook 'grep-mode-hook 'pointemacs-grep-find-moins-verbeux)

;; M-x find-dired : read-directory-name au lieu de read-file-name
(defadvice find-dired (before pointemacs-repertoire activate)
  "Correction de bug : lit le répertoire de départ avec `read-directory-name'"
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
		     (read-string "Run find (with args): " find-args
				  '(find-args-history . 1)))))

(pointemacs-progression "Contrôle de versions") ;;;;;;;;;;;;;;;;;;;;;;;

;; Support des systèmes de contrôle de versions que j'utilise, et
;; (surtout...) élimination des autres.
(let* ((vcs-preferes '("hg" "git" "svk" "svn" "CVS" "RCS"))
       (vcs-preferes (if (string-match "^21" emacs-version)
                         (remove "hg" vcs-preferes)
                       vcs-preferes))
       (est-connu
        (lambda (systeme)
          "Renvoie une valeur vraie si SYSTEME est utilisable.
Plus précisément, renvoie une liste dont l'unique élément est le symbole à
ajouter à la variable `vc-handled-backends'; ou la liste vide (nil) si le
système n'est pas utilisable."
          (if (require-faible (intern (concat "vc-" (downcase systeme))))
              (list (intern (upcase systeme)))))))
  (pointemacs-personnalise `(vc-handled-backends
                          (mapcan ,est-connu vcs-preferes))))

;; Certains environnements de dév pour langages de script «compilent»
;; en tirant des liens symboliques :
(pointemacs-personnalise '(vc-follow-symlinks t))

;; Par défaut sous emacs, vc-cvs fiche le bazar en matière de copies de
;; secours. Ceci désactive les copies en "machin.txt.~1.1~"...
(eval-after-load "vc-cvs" '(defun vc-cvs-make-version-backups-p (ignored) nil))

;; ... et ceci réactive les copies de secours ordinaires pour les
;; fichiers sous CVS / RCS. C'est alors le comportement ordinaire,
;; (fichiers tilde ou un répertoire de fichiers de secours, selon la
;; valeur de backup-directory-alist ci-dessus), qui prend effet.
(pointemacs-personnalise '(vc-make-backup-files t))

;; Support des équivalents de PCL-CVS pour d'autres SCM (se lance avec
;; M-x svn-status, M-x git-status etc)
(autoload 'svn-status "psvn"
  "Examine the status of Subversion working copy in directory DIR."
  'interactive)

(pointemacs-progression "Git") ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pointemacs-vc-next-action (amend)
  "C-x v v amélioré pour git.
\\[universal-argument] \\[vc-next-action] effectue un \"git commit --amend\".
Si l'index contient des modifications, appelle `git-gutter+-commit' (ce qui va
bien après avoir fait \\[git-gutter+-stage-hunks])."
  (interactive "P")
  (with-current-buffer (current-buffer)
    (while (and (boundp 'vc-parent-buffer) vc-parent-buffer)
      (set-buffer vc-parent-buffer))
    (cond
     ((not (eq 'GIT (vc-backend buffer-file-name)))
      (vc-next-action amend))  ;; Keep C-U semantics of other VC system
     ((and (fboundp 'git-gutter+-commit)
           ;; http://stackoverflow.com/questions/2657935
           (= 1 (vc-git-command nil 1 nil
                                "diff-index" "--quiet" "--cached" "HEAD")))
      (git-gutter+-commit))
     ((and amend buffer-file-name)
      (message "Checking in %s (amend)...done" buffer-file-name)
      (vc-git-command nil 0 buffer-file-name
                      "commit" "--amend" "-C" "HEAD" "--")
      (when (fboundp 'git-gutter+-refresh) (git-gutter+-refresh)))
     (t
      (vc-next-action nil)))))

(eval-after-load "vc-hooks" '(define-key vc-prefix-map "v"
                               'pointemacs-vc-next-action))

(autoload 'magit-status "magit" "Entry point into magit." 'interactive)
(eval-after-load "magit" '(progn
  (custom-set-faces
    '(magit-diff-file-header ((t (:inherit diff-header))))
    '(magit-diff-hunk-header ((t (:inherit diff-header))))
    '(magit-diff-add ((t (:inherit diff-added))))
    '(magit-diff-del ((t (:inherit diff-removed))))
    '(magit-diff-none ((t (:inherit diff-context))))
    '(magit-item-highlight ((t))))
  (when (not (fboundp 'start-file-process))  ;; Bugware pour vieux Emacs :
    (defalias 'start-file-process 'start-process))
  (defun pointemacs-magit-merge-tool ()
    (interactive)
    (let* ((section (magit-current-section))
           (status (and section (magit-section-diff-status section))))
      (unless (eq 'unmerged status)
        (user-error "Git mergetool works on unmerged files only"))
      (magit-run-git "mergetool" "-y" (magit-section-info section))))
  (let ((menu-merging (cdr (assoc 'merging magit-key-mode-groups))))
    (push (list "t" "Tool" 'pointemacs-magit-merge-tool)
          (cdr (assoc 'actions menu-merging))))
  (setq magit-key-mode-keymaps nil)))  ;; Calculé depuis magit-key-mode-groups

(add-hook 'magit-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'magit-log-edit-mode-hook
          (lambda () (setq fill-paragraph-function nil)))
;; Pas de marqueur "MRev" dans la barre d'état :
(pointemacs-personnalise '(magit-auto-revert-mode-lighter nil))

;; git-gutter+ (https://github.com/nonsequitur/git-gutter-plus)
(when (fboundp 'global-git-gutter+-mode)
  (global-git-gutter+-mode t)
  (global-set-key (kbd "C-x v n") 'git-gutter+-next-hunk)
  (global-set-key (kbd "C-x v p") 'git-gutter+-previous-hunk)
  (global-set-key (kbd "C-x v h") 'git-gutter+-stage-hunks))

(defadvice git-rebase-server-edit (after pointemacs-ferme activate)
  "Ferme le buffer de git-rebase-todo une fois édité.
L'information est reprise dans le buffer magit; un rebase-todo ouvert
à contretemps (ex : rebase terminé) est source de confusions."
  (kill-buffer))

(add-hook 'git-rebase-mode-hook 'pointemacs-git-rebase-mode-hook)

(pointemacs-progression "Débogueurs") ;;;;;;;;;;;;;;;;;;;;;;;

;; Configuration de GUD, le débogueur multi-langages d'Emacs
(pointemacs-personnalise
 '(gud-gdb-command-name (concat (pointemacs-choisir-commande "ggdb" "gdb")
                                " -i=mi"))
 '(gud-chdir-before-run nil)
 '(comint-history-isearch 'dwim))

;; (defadvice gdb-display-buffer (around pointemacs-pas-de-fenetre-dediee activate)
;;   (display-buffer (ad-get-arg 0)))
;; 
(defun pointemacs-GUD-piles-cliquables ()
  "Rend les piles d'exécution  (\"stack traces\") cliquables dans GUD."
  (compilation-shell-minor-mode t))
(add-hook 'gud-mode-hook 'pointemacs-GUD-piles-cliquables)
(eval-after-load "compile" '(progn
  (add-to-list 'compilation-error-regexp-alist 'perl-debugger)
  ;; Cf. perl5db.pl lignes 5611 et 5625-5627
  (add-to-list 'compilation-error-regexp-alist-alist '(perl-debugger "\\(file\\|at\\) '?\\([^' \t\n]+\\)'? [^\n]*line \\([0-9]+\\)[\\., \n]"
     2 3))))

(defadvice comint-send-input (around pointemacs-bt-dans-gdb activate)
  "Lorsqu'on tape \"bt\" dans gud-gdb, affiche la pile avec GUD à la place."
  (if (and (fboundp 'gdb-display-stack-buffer)
           (boundp 'gud-minor-mode)
           (eq gud-minor-mode 'gdbmi)
           (save-excursion
             (backward-word)
             (cond ((looking-at "bt")
                    (delete-forward-char 2)
                    t))))
      (gdb-display-stack-buffer)
    ad-do-it))

(defadvice gdb-json-string (before pointemacs-repertoire activate)
  "Correction de bug : lecture des chaînes MI avec un en-tête."
  (when (string-match "^\"" (ad-get-arg 0))
    (ad-set-arg 0 (concat "_header=" (ad-get-arg 0)))))

;; Débogueur interne d'Emacs
(defun pointemacs-reaffiche-debogueur ()
  "Affiche la fenêtre du débogueur Lisp.

Pour quitter une sous-session du débogueur en erreur (par exemple si on a tapé
\"e\" et une expression syntaxiquement incorrecte), taper successivement
\\<global-map>`\\[abort-recursive-edit]' et
\\<global-map>`\\[pointemacs-reaffiche-debogueur]'."
  (interactive)
  (let ((debugger-buffer (let ((default-major-mode 'fundamental-mode))
                           (get-buffer-create "*Backtrace*"))))
    (pop-to-buffer debugger-buffer)
    (debugger-mode)
    (debugger-setup-buffer debugger-args)))

;; Pas de limite de taille pour afficher les valeurs Lisp:
(pointemacs-personnalise '(eval-expression-print-length nil)
                      '(eval-expression-print-level nil))

;; MozLab: mettez un Emacs dans votre Mozilla
;; http://hyperstruct.net/projects/mozrepl/emacs-integration
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'run-mozilla "moz"
  "Show the inferior mozilla buffer.  Start the process if needed.")
;; Intégration entre MozLab et js2-mode
(add-hook 'js2-mode-hook (lambda () (moz-minor-mode 1)))
;; Les deux fonctions `moz-send-defun' et `moz-send-defun-and-go' sont
;; implémentées comme des crocks inutilisables dans moz.el : on les redéfinit.
(eval-after-load "moz" '(progn
  (defun moz-send-defun ()
    "Send the current function to Firefox via MozRepl.
Curent function is the one recognized by `js2-mode-function-at-point'."
    (interactive)
    (let ((node (js2-mode-function-at-point)))
      (moz-send-region (js2-node-abs-pos node) (js2-node-abs-end node))))
  (defun moz-send-defun-and-go ()
    "Send the current function to Firefox via MozRepl.
Also switch to the interaction buffer."
    (interactive)
    (moz-send-defun)
    ;; L'original a un bug à la ligne suivante :
    (inferior-moz-switch-to-mozilla nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Goodies")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Les "goodies" sont les fonctions d'Emacs qui ne sont pas des modes
;; d'édition (ex: le docteur, le serveur d'édition, etc.). Qui a dit
;; qu'Emacs, à l'instar d'UNIX, était un bon système d'exploitation
;; auquel il ne manquait qu'un bon éditeur de texte ? :-)

(require-faible 'emacs-goodies-el) ;; Automatique sous Debian, mais
                                   ;; pour mon MacBook faut aider un peu.

;; Impression : sous Linux j'utilise kprinter qui à l'inverse de l'icône «print
;; buffer» de la barre d'outils a l'avantage de posséder un bouton «annuler»...
(pointemacs-personnalise '(ps-paper-type (quote a4)))
(let ((kprinter (some 'executable-find '("kprinter" "gtklp"))))
  (if kprinter (pointemacs-personnalise `(lpr-command ,kprinter))))

;; Dired (gestionnaire de fichiers)
(setq dired-shell-have-gnutar "tar")
(add-hook 'dired-mode-hook (lambda () (setq dired-omit-files-p t)) t)

;; Wanderlust: client IMAP et SMTP
(when (boundp 'mail-user-agent) (setq mail-user-agent 'wl-user-agent))
(when (fboundp 'define-mail-user-agent)
    (define-mail-user-agent 'wl-user-agent 'wl-user-agent-compose
      'wl-draft-send 'wl-draft-kill 'mail-send-hook))

;; shell-mode en couleurs
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Tramp : accès par ssh + sudo selon /sudo:root@kilim:/etc/passwd
;; http://www.emacswiki.org/emacs/TrampMode#toc12
(pointemacs-personnalise
 '(tramp-default-host "localhost")
 '(tramp-default-proxies-alist
   '(("localhost" nil nil)
     ("..*" "^root$" "/ssh:%h:"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Chargement du .emacs terminé")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
