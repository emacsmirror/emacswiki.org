;;; .emacs-DominiqueQuatravaux.el - Le .emacs de Dominique Quatravaux, en français !

;;; -*- emacs-lisp -*-
;; .emacs, fichier de configuration d'emacs de Dom - $Revision: 1.183 $
;;
;; MODE D'EMPLOI
;;
;; Normalement, ce .emacs fonctionne tel quel sur toute installation fraîche
;; d'emacs et xemacs (les paquetages «exotiques» que j'utilise ne sont pas
;; indispensables, et leur absence ne provoquera pas une erreur).
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
;; adapté depuis. Compatible avec emacs 21.x et suivants et avec xemacs.
;;

;;;;;;;;;;;;;;;;;;; Support interne ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Si vous éditez ce .emacs manuellement, merci de ne rien toucher
;; ici; passer directement à la section suivante.

(require 'cl)  ;; Petites gâteries Lisp comme "some" ou "remove-duplicates"

(setq pointemacs-progression nil)
(defun pointemacs-progression (texte)
  "Affiche la progression du chargement du .emacs, si
`pointemacs-afficher-progression' est réglé à une valeur vraie"
  (when (and (boundp 'pointemacs-afficher-progression)
             pointemacs-afficher-progression)
    (setq pointemacs-progression (append pointemacs-progression (list texte)))
    (let ((textecomplet (concat ".emacs : " texte)))
	 (message textecomplet)
	 ;; C'est pas tout, il faut s'assurer que le tout dernier message
	 ;; ne sera pas mâchouillé par la propagande pour le projet GNU :-)
	 (setq inhibit-startup-message t) ;; XEmacs
	 (eval `(defun startup-echo-area-message () ,textecomplet)) ;; Emacs
)))

(defvar pointemacs-variante-emacs (if (featurep 'xemacs) "xemacs" "emacs")
  "Le nom court de la version d'Emacs que nous utilisons,
sous forme d'une chaîne (par exemple: emacs ou xemacs).")

(defvar pointemacs-reserves nil
"Liste des erreurs non fatales survenues au cours de la lecture du .emacs.

Ce .emacs est tolérant aux pannes, et essaye de continuer son
exécution en cas d'erreur. Dans ce cas, une explication (une
\"réserve\", comme dans \"réserves de recette\") sera ajoutée à cette
liste. Cela arrive notamment et principalement lorsqu'un paquetage
Emacs-Lisp requis est introuvable; ce n'est pas grave, tout fonctionnera
normalement (si ce n'est évidemment que la fonctionnalité
correspondant audit paquetage sera indisponible) et on peut donc ne
tenir aucun compte du message.
" )

(defun require-faible (symbol &rest options)
"Identique à `require', mais ne provoque pas une erreur si le
paquetage n'existe pas. Au contraire, insère SYMBOL dans
`pointemacs-reserves', à moins que le symbole 'quiet ne soit passé
parmi les paramètres optionnels auquel cas le paquetage manquant est
tout bonnement ignoré. Renvoie une valeur Booléenne indiquant si le
paquetage a bien été trouvé et s'est bien chargé.
"
(condition-case error
    (progn (require symbol) t)
  (file-error
   (cond 
    ((not (equal (symbol-name symbol) (nth 2 error)))
     (signal 'file-error error))
    ((member (symbol-name symbol) pointemacs-paquetages-ignores)   nil)
    ((memq 'quiet options)   nil)
    (t (add-to-list 'pointemacs-reserves
		    (format "Paquetage \"%s\" absent %S"
			    (symbol-name symbol) error)
		    'at-end-of-list)))
   nil)))

;; Le .emacs lui-même est customizable !
(defcustom pointemacs-message-final (not (featurep 'xemacs))
"Obsolète, remplacé par `pointemacs-afficher-progression'")

(defcustom pointemacs-afficher-progression
  pointemacs-message-final
"Doit-on afficher des messages de progression pendant le chargement du .emacs ?

Cela permet de s'ennuyer un peu moins, et aussi de savoir quels
paquetages sont absents (auquel cas ce .emacs, qui est tolérant aux pannes,
continue quand même).
"
:type '(boolean)
:group 'pointemacs)

(defcustom pointemacs-paquetages-ignores nil
"La liste des paquetages proposés par le .emacs dont vous n'avez pas usage.

Ce .emacs est tolérant aux pannes, et essaye de continuer son exécution
en cas de paquetage requis absent. Il rajoute dans ce cas un message dans
la liste `pointemacs-reserves'. Ceci dit, vous n'utiliserez jamais certains
modes; pour empêcher emacs de sans cesse vous rappeler qu'ils ne sont pas
installés, il suffit de rajouter leur nom à cette liste.
"
:type '(repeat string)
:group 'pointemacs
)

;; Rendons la variable `load-path' customizable, c'est plus convy :
(defcustom load-path `(list ,@load-path)
  "Répertoire de chargement des fichiers Emacs-Lisp.

Tous les paquetages ne mettent pas leurs fichiers dans l'un des répertoires
standard tels que /usr/share/emacs/site-lisp; dans ce cas il faut rajouter des
répertoires dans cette liste.

\"~/lib/emacs\" est réservé pour les fichiers Emacs-Lisp que l'on télécharge
soi-même.

\"~/lib/emacs/emacs-specific\" et \"~/lib/emacs/xemacs-specific\"
sont réservés, comme le nom le suggère, aux fichiers Emacs-Lisp
incompatibles entre les deux éditeurs.  C'est le cas de tous les
.elc (Lisp compilé) et aussi de certains paquetages non
portables (par exemple, les extensions de vc.el).
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Chemins d'accès")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dans cette section, les chemins d'accès doivent être adaptés en
;; fonction de l'architecture (distribution / OS / préférences pour
;; l'organisation du répertoire personnel).

;; `custom-file' est le fichier dans lequel Emacs sauvegarde les
;; customizations faites avec l'interface graphiques.  On peut même
;; avoir des customizations séparées pour emacs et xemacs !
(setq custom-file (format "~/.%s-custom" pointemacs-variante-emacs))
(if (file-exists-p custom-file) (load-file custom-file))

;; Fichiers d'état des informations persistantes
(custom-set-variables
 '(session-save-file (expand-file-name
                      (format "~/.%s-session" pointemacs-variante-emacs)))
 '(bookmark-default-file (expand-file-name "~/.emacs-bookmarks")))

(custom-set-variables '(load-path 
  (let* ((mes-chemins
          (list "~/lib/emacs"
                "~/lib/emacs/elisp-cache"
                (concat "~/lib/emacs/" emacs-version)
                "~/lib/emacs/magit"
                "~/lib/emacs/nxhtml/alts"
                "/usr/local/share/emacs/site-lisp"
                "/usr/lib/erlang/lib/tools-2.1/emacs"
                "/usr/share/emacs/ProofGeneral/generic"
                "/usr/share/doc/git-core/contrib/emacs"
                "/usr/lib/erlang/lib/tools-2.3/emacs")))
    (remove-duplicates (append (mapcar 'expand-file-name mes-chemins)
                               load-path)))))

(defvar pointemacs-chemin-autostart-nxhtml
  (expand-file-name "~/lib/emacs/nxhtml/autostart.el")
  "Chemin d'accès du fichier autostart.el du paquetage nxhtml.

Si ce fichier n'existe pas, ce n'est pas grave : nxhtml ne sera simplement
pas disponible.")

;; Comme pour load-path, la variable d'origine n'est pas customizable.
(defcustom Info-default-directory-list `(list ,@Info-default-directory-list)
"Liste de répertoires à explorer pour la documentation au format Info.

Cette liste dépend de la façon dont la distribution et/ou l'utilisateur
installe les .info"

:type '(repeat file)
:group 'pointemacs
)

(custom-set-variables '(Info-default-directory-list
    (remove-duplicates (append Info-default-directory-list
         (list "/usr/share/info"
               "/usr/share/info/emacs21"  ;; Debian
               "/usr/share/info/xemacs21" ;; Debian aussi
               (expand-file-name "~/lib/info")
               (expand-file-name "~/lib/emacs/magit"))))))

;; Chemins de recherche des schémas pour nxml / nxHtml
(custom-set-variables '(rng-schema-locating-files
        (list
         "/usr/local/lib/rng-schema/schemas.xml"
         "/usr/share/emacs/site-lisp/nxml-mode/schema/schemas.xml")))


;; Sous Mac OS X Leopard, Emacs ne reçoit aucun environnement utile au démarrage
;; (cause: launchd en état de mort cérébrale, lisez ses sources !) et du coup le
;; site-start utilise des magouilles plus ou moins infâmes pour régler le PATH,
;; qui est donc généralement FUBAR à ce stade. Le mieux est de recommencer de
;; zéro en invoquant un login shell pour connaître le «vrai» path:
(cond ((featurep 'carbon-emacs-package)
       (setenv "PATH" "")
       (let ((path-d-apres-le-shell
             (shell-command-to-string "$SHELL -l -c 'echo -n $PATH'")))
         (setq exec-path nil)
         (carbon-emacs-package-add-to-path path-d-apres-le-shell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Autres réglages site-spécifiques")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adresse électronique
(custom-set-variables '(user-mail-address "domq@cpan.org")
                      '(query-user-mail-address nil))

;; Personnalisations Google. Sans intérêt à l'extérieur.
(require-faible 'google-domq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Affectation des touches")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;; Réglages généraux

;; Pour ne pas faire Méta-h à chaque fois qu'on tape "è" dans le
;; terminal. Emacs X11 est immunisé contre cette insupportable
;; plaisanterie.  Si on veut quand même faire Méta-h, on peut : soit
;; en tapant "Esc h", soit en réglant son émulateur de terminal correctement
;; (pour xterm par exemple, c'est la ressource X "metaSendsEscape").
(if (not window-system)
   (custom-set-variables '(keyboard-coding-system 'iso-latin-1)))

;;;;;;; Affectation de touches dans tous les modes

(global-set-key [(control home)] 'beginning-of-buffer)
(global-set-key [(control end)] 'end-of-buffer)
(global-set-key [(control f9)] 'compile)  ;; Nostalgie de Turbo Pascal...
(unless (require-faible 'kmacro)
  (global-set-key [(f4)] 'call-last-kbd-macro))
(global-set-key [(meta g)] 'goto-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [(control backspace)] 'backward-kill-word)
(global-set-key [(control meta \])] 'pointemacs-reaffiche-debogueur)
;; Le setting par défaut est ridicule en mode graphique:
(if window-system (global-set-key [(control z)] 'undo))
(global-set-key [(control c) (control l)] 'org-store-link)
(global-set-key [(control c) (i)] 'magit-status)

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

;;;;;;; Affectation de touches dans des modes particuliers

;; F5 = actualiser. Pour mmm : reparser le bloc courant ;
;; pour les autres modes : refaire la fontification.
(global-set-key [(f5)] 'font-lock-fontify-buffer)
(eval-after-load "mmm-mode"
   '(define-key mmm-mode-map [(f5)] 'mmm-parse-block))
;; `font-lock-fontify-buffer' est incroyablement lent sous JS2, et en plus il
;; modifie le buffer (WTF?)
(eval-after-load "js2-font-lock-new"
   '(define-key js2-mode-map [(f5)] 'js2-mode-reset))

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

;; Control-X # en mode serveur : je m'en sers machinalement et à
;; mauvais escient.
(put 'server-edit 'disabled t)

;; Ceux qui au contraire sont utiles mais désactivés par défaut, parce que
;; leur effet est désorientant.
(put 'narrow-to-region  'disabled nil)

;; C-c C-l déjà utilisé (cf ci-dessus)
(eval-after-load "cc-mode"
  '(define-key c-mode-base-map [(control c) (control l)] nil))
(eval-after-load "python-mode"
  '(define-key py-mode-map [(control c) (control l)] nil))

(add-hook 'cperl-mode-hook 
          (lambda () (progn
             ;; Désactivation de certaines touches électriques
             ;; insupportables:
             (cperl-define-key ":" 'self-insert-command)
             )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Des goûts et des couleurs...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require-faible 'color-theme)
  (unless (boundp 'google-domq-color-theme-set)
    ;; color-theme-domq, mon thème de couleurs à moi (dispo sur
    ;; http://www.emacswiki.org/cgi-bin/emacs/color-theme-domq.el) :
    (require-faible 'color-theme-domq)
     (color-theme-domq)))

;; Font-lock automatiquement allumé dans tous les modes.
(cond ((featurep 'xemacs)
       (custom-set-variables '(font-lock-auto-fontify t)
                             '(font-lock-mode t nil (font-lock))))
      (t  ;; Non-xemacs
       (global-font-lock-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Cycle de vie de l'éditeur")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sauvegarde des historiques et des coordonnées des dernières modifs entre
;; deux sessions d'Emacs :
(custom-set-variables '(session-initialize t))
(when (require-faible 'session)
  ;; Sauvegarde la session essentiellement... à tout bout de champ :
  (add-hook 'pointemacs-taches-periodiques-hook 'session-save-session))

;; Signets d'Emacs (eux aussi sauvegardés entre deux sessions) :
;; taper C-x r m pour poser un signet.
(custom-set-variables '(bookmark-save-flag 1))

;; Confirmation avant de quitter
(setq kill-emacs-query-functions (list (lambda nil (interactive)
                                         (y-or-n-p "Really quit? "))))

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
    (custom-set-variables '(server-kill-new-buffers nil))
    ;; Intégration avec le plug-in Firefox ItsAllText (que je recommande):
    (add-hook 'server-visit-hook
              (lambda () "Désactive le passage à la ligne automatique pour
                          les pages Wiki"
                (if (and buffer-file-name
                         (string-match "wiki" buffer-file-name))
                    (turn-off-auto-fill))))))
;; Utiliser ensuite 'gnuclient' resp. 'emacsclient' comme éditeur
;; (variable EDITOR dans son .bash_profile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Traitement des fichiers ouverts par emacs")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mise à jour automatique des fichiers qui changent sur disque
(global-auto-revert-mode 1)

;; Lecture/écriture des fichiers compressés
(auto-compression-mode 1)

;; Correspondance entre noms de fichiers et modes d'édition.  Note : certains
;; modes sont redéfinis plus bas à l'aide de "defalias" (par exemple 'html-mode
;; pourra être en réalité 'nxhtml-mumamo-mode, et 'perl-mode sera probablement
;; 'cperl-mode).  L'élégance est critiquable, mais c'est la façon la plus simple
;; de changer un mode *partout* (y compris dans les valeurs déjà présentes au
;; démarrage dans auto-mode-alist, interpreter-mode-alist etc)
(flet ((regexp-pour-suffixes (&rest suffixes)
                             (format "\\.\\(%s\\)$" (regexp-opt suffixes))))
  (setq auto-mode-alist (append (list
    (cons (regexp-pour-suffixes "ml" "mli" "mly" "mll" "mlp") 'caml-mode)
    (cons (regexp-pour-suffixes "m") 'objc-mode)
    (cons (regexp-pour-suffixes "p") 'pascal-mode)
    (cons (regexp-pour-suffixes "php" "inc") 'php-mode)
    (cons (regexp-pour-suffixes "pl" "PL" "pm" "t" "pod" "cgi") 'perl-mode)
    (cons (regexp-pour-suffixes "dpatch") 'diff-mode)
    (cons (regexp-pour-suffixes "org") 'org-mode)
    (cons (regexp-pour-suffixes "xml" "xhtml" "xmlinc" "xsd" "sch"
                                "rng" "xslt" "svg" "rss")
          'xml-mode)
    (cons (regexp-pour-suffixes "html" "htm" "phtml" "pcomp" "cst")
          'html-mode)
    (cons "^/tmp/mutt" 'mail-mode)
    (cons "akefile" 'makefile-mode)
    (cons (regexp-pour-suffixes "jj") 'java-mode)
    (cons (regexp-pour-suffixes "js") 'javascript-mode)
    (cons "TODO" 'text-mode)  ;; Todoo suxx
  ) auto-mode-alist)))

;; Correspondance entre noms de fichiers et encodage
(if (boundp 'auto-coding-alist)
    (add-to-list 'auto-coding-alist '("screenlog\\.[0-9]\\'" . utf-8-dos)))

;; Idem mais en se basant sur la première ligne du fichier au
;; format "#!/usr/bin/truc" :
(setq interpreter-mode-alist (append (list
  (cons "perl" 'perl-mode)
) interpreter-mode-alist))

;; Idem mais en se basant sur la première ligne du fichier, peu
;; importe son nom (ne marche que sous les Emacs récents):
(if (boundp 'magic-mode-alist)
    (setq magic-mode-alist (append (list
      (cons "<\\?xml" 'xml-mode)
  ) magic-mode-alist)))

;; Fichiers de sauvegarde centralisés dans un répertoire
(let ((rep-fichiers-secours (expand-file-name "~/lib/emacs/var/backups")))
  (cond ((featurep 'xemacs)
         (custom-set-variables `(bkup-backup-directory-info
                                 (list (list "." ,rep-fichiers-secours
                                             'full-path))))
         (require-faible 'backup-dir))
        (t ;; Non-xemacs
         (custom-set-variables `(backup-directory-alist
                                 (list (cons "." ,rep-fichiers-secours)))))))

;; Le fait de ne pas utiliser /bin/sh pour lancer des commandes "toutes faites"
;; est un choix, disons... discutable de la part d'Emacs (quid d'un shell qui
;; rame au démarrage comme zsh, ou d'un shell non compatible Bourne qui va
;; se prendre les pieds dans le tapis au moindre caractère spécial ?)
(setq shell-file-name "/bin/sh")
(setq tex-shell-file-name "/bin/sh")
(setq explicit-shell-file-name (getenv "SHELL"))

;; Noms de buffer uniques mais utiles (en lieu et place de "Makefile<2>"):
(if (require-faible 'uniquify)
    (custom-set-variables '(uniquify-buffer-name-style
                            'post-forward-angle-brackets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (pointemacs-progression "Personnalisation générale de l'édition de texte")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Les réglages de ce paragraphe sont valables dans pratiquement tous les
;; modes d'emacs, pas juste "text-mode" qui de son côté a une section dans
;; le paragraphe "configuration des modes".

(custom-set-variables
 '(next-line-add-newlines nil)
 '(require-final-newline 1)
 '(indent-tabs-mode nil) ;; Le monde OSS a fini par se rendre compte
 ;; que les tabs c'est MAL, tant mieux
 '(default-major-mode 'text-mode)
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
(setq-default enable-multibyte-characters 1)
(if (not (featurep 'xemacs))
    (prefer-coding-system 'utf-8)
  ;; Au XXIè siècle, Unicode ne marche juste pas sous xemacs;
  ;; http://www.xemacs.org/Documentation/21.5/html/xemacs-faq_2.html#SEC72
  ;; Alors on fait ce qu'on peut.
  (require 'un-define)
  (set-coding-priority-list '(utf-8))
  (set-coding-category-system 'utf-8 'utf-8)
  ;; TODO : XEmacs 21.4.21 n'affiche même pas les accents par défaut lorsqu'il
  ;; ouvre un fichier Unicode, sauf si on fait
;  (standard-display-european t)
  ;; auquel cas... il se plante !!
  )

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
  (cond ((featurep 'xemacs)
         (require 'pending-del)
         (custom-set-variables '(pending-delete-mode t t)))
        (t ;; Non-xemacs
         (transient-mark-mode 1)
         (delete-selection-mode 1))))

;; Complétion automatique du texte (méta-/ et control-méta-/)
(eval-after-load "dabbrev"
   '(defalias 'dabbrev-expand 'hippie-expand)) ;; Trouve plus de complétions
(custom-set-variables '(dabbrev-upcase-means-case-search t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (pointemacs-progression "Configuration du minibuffer")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Le minibuffer est la ligne en bas de l'écran dans laquelle on tape
;; les noms de fichiers à ouvrir, les mots à chercher, etc.

(when (require-faible 'icomplete) (icomplete-mode))
(cond
 ((and (not (featurep 'xemacs)) (require-faible 'ido))
  (ido-mode 1)
  (custom-set-variables '(ido-everywhere t)
                        '(ido-auto-merge-work-directories-length -1)
                        '(ido-confirm-unique-completion t)
                        '(ido-default-file-method 'selected-window)
                        '(ido-default-buffer-method 'samewindow)
                        '(ido-use-filename-at-point 'guess))
  (when (fboundp 'ido-completing-read)
    (defun ido-execute ()
      "Utilise ido pour compléter à l'invite de M-x"
      (interactive)
      (call-interactively (intern (ido-completing-read "M-x "
          (let (cmd-list)
            (mapatoms (lambda (S) (when (commandp S)
                    (setq cmd-list (cons (format "%S" S) cmd-list)))))
            cmd-list)))))
    (global-set-key "\M-x" 'ido-execute))
  (define-key ido-file-dir-completion-map [(control backspace)]
    'ido-delete-backward-word-updir)
  (defadvice ido-delete-backward-updir (around pointemacs-backspace-doux
                                               activate)
    "Dans ido, change l'action de Backspace au début du buffer : au lieu
     d'effacer tout le répertoire précédent, efface seulement le slash"
    (flet ((ido-up-directory (&rest ignored)
            (setq ido-text-init (file-name-nondirectory
                                 (directory-file-name ido-current-directory))
                  ido-current-directory (file-name-directory
                                 (directory-file-name ido-current-directory))
                  ido-exit 'refresh)
            (ido-trace "Trampoline vers le répertoire" ido-current-directory)
            (exit-minibuffer)))
        ad-do-it)))
 (t ;; Pour XEmacs, où ido n'est encore ni standard ni stable:
  ;; Réglages de la complétion lors de l'ouverture d'un fichier
  (setq minibuffer-confirm-incomplete t
        completion-ignored-extensions (append completion-ignored-extensions
                                              '(".zo" ".zi" ".zix")))))

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

(pointemacs-progression "Modes pour le Web") ;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pour XML, nxml-mode (http://www.emacswiki.org/cgi-bin/wiki/NxmlMode) est le
;; nouvel étalon-or en 2008; mais il ne marche pas pour XEmacs.
(defalias 'xml-mode (if (featurep 'xemacs) 'sgml-mode 'nxml-mode))

;; Pour HTML et les copains (PHP etc) : nXHtml, qui rulez, quoique lui aussi
;; seulement pour Emacs 22 (http://www.emacswiki.org/cgi-bin/wiki/NxhtmlMode).
(when (and (string-lessp "22" emacs-version)
           (file-exists-p pointemacs-chemin-autostart-nxhtml))
  (custom-set-variables '(nxhtml-skip-welcome t)
                        '(mumamo-chunk-coloring 40))
  ;; Chargement de nXHtml.  Il bave pas mal sur d'autres réglages, il faut le
  ;; surveiller :
  (let ((auto-mode-alist nil)
        (debug-on-error nil))
      (load pointemacs-chemin-autostart-nxhtml))
  ;; WTF? Le chargement de nXHtml pourrit ma fonte 'custom-button !
  (face-spec-set 'custom-button (get 'custom-button 'face-defface-spec))
  ;; (defalias 'html-mode 'nxhtml-mumamo-mode)
)

;; pour JavaScript : js2, par Google (http://code.google.com/p/js2-mode/) est
;; bien mais pas top (mauvaises perfs); heureusement, nxhtml en propose une
;; version plus mieux.
(when (not (featurep 'xemacs))
  (autoload 'js2-fl-mode "js2-font-lock-new" nil t)
  (defalias 'javascript-mode 'js2-fl-mode)
  (setq js2-mode-must-byte-compile nil)
  (custom-set-variables '(js2-mirror-mode nil)))

(pointemacs-progression "Mode Perl") ;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'perl-mode 'cperl-mode)
(autoload 'cerl-mode "cperl-mode" "alternate mode for editing Perl programs"
t)

;; Respect du coding style IDEALX (Google n'aime pas Perl...)
(custom-set-variables '(cperl-indent-level 2)
		      '(cperl-label-offset 0)
		      '(cperl-continued-statement-offset 2)
		      '(cperl-highlight-variables-indiscriminately 1))

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
(custom-set-variables '(safe-local-variable-values (quote ((auto-fill-mode)))))
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

;; M-x grep, M-x rgrep : extension des motifs des fichiers consultés.
(eval-after-load "grep"
  '(progn
     (dolist (change-alias '(("ch" . "*.c *.cc *.cpp *.h")
                             ("c" . "*.c *.cc *.cpp")))
       (let* ((cle (car change-alias))
              (nouvelle-valeur (cdr change-alias))
              (noeud (assoc cle grep-files-aliases)))
         (if noeud (setcdr noeud nouvelle-valeur))))

     ;; Définition copiée et modifiée depuis grep.el afin d'améliorer le
     ;; mécanisme de sélection de valeur par défaut.
     (defun grep-read-files (regexp)
       "Read files arg for interactive grep."
       (let* ((bn (or (buffer-file-name) (buffer-name)))
              (fn (and bn (stringp bn) (file-name-nondirectory bn)))
              (default
                ;; Partie modifiée, afin de choisir une valeur adéquate dans
                ;; grep-files-aliases quand les cdrs contiennent des espaces.
                (or (and fn
                         (some (lambda (motifs)
                                 (some (lambda (motif)
                                         (and (string-match
                                               (wildcard-to-regexp motif) fn)
                                              motifs))
                                       (split-string motifs)))
                               (mapcar 'cdr grep-files-aliases)))
                ;; Fin de la partie modifiée
                    (and fn
                         (let ((ext (file-name-extension fn)))
                           (and ext (concat "*." ext))))
                    (car grep-files-history)
                    (car (car grep-files-aliases))))
              (files (read-string
                      (concat "Search for \"" regexp
                              "\" in files"
                              (if default (concat " (default " default ")"))
                              ": ")
                      nil 'grep-files-history default)))
         (and files
              (or (cdr (assoc files grep-files-aliases))
                  files))))))

;; M-x rgrep : utilisation de zpcregrep si possible
(setenv "PCREGREP_COLOR" "01;31")
(let ((meilleur-grep (if (not (fboundp 'locate-file)) "zgrep"
                       (some (lambda (nomgrep)
                               (locate-file nomgrep exec-path))
                             '("zpcregrep" "zgrep")))))
  (custom-set-variables '(grep-program (concat meilleur-grep " --color=always"))
                        '(grep-highlight-matches t)))

(pointemacs-progression "Contrôle de versions") ;;;;;;;;;;;;;;;;;;;;;;;

(if (featurep 'xemacs)
    ;; Le pauvre xemacs a un vc.el qui n'est pas maintenu depuis des
    ;; lustres.  Impossible de sortir des 3 Cavaliers de l'Apocalypse
    ;; (CVS, RCS, SCCS) !
    (vc-load-vc-hooks)

  ;; Sous Emacs: support des systèmes de contrôle de versions que
  ;; j'utilise, et (surtout...) élimination des autres.
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
            (let* ((nom-lib (concat "vc-" (downcase systeme)))
                   (nom-backend (upcase systeme))
                   (symbole-lib (intern nom-lib))
                   (symbole-backend (intern nom-backend)))
              (if (require-faible symbole-lib)
                  (list symbole-backend))))))
    (custom-set-variables '(vc-handled-backends
                            (mapcan est-connu vcs-preferes)))))

(defadvice vc-next-action (around pointemacs-git-universal-argument activate)
  "Avec git, \\[universal-argument] \\[vc-next-action] effectue un
\"git commit --amend\".

Ceci a pour effet d'oublier le précédent commit et de le recréer avec le même
message, mais avec les changements du présent fichier en plus.  Il est ainsi
possible de créer incrémentalement un commit sur plusieurs fichiers."
  (with-current-buffer (current-buffer)
    (while vc-parent-buffer (set-buffer vc-parent-buffer))
    (if (and current-prefix-arg buffer-file-name
             (eq 'GIT (vc-backend buffer-file-name)))
        (flet ((vc-git-checkin (files rev comment)
                  ;; Code copié et modifié depuis l'original dans vc-git.el :
                  (let ((coding-system-for-write git-commits-coding-system))
                    (vc-git-command nil 0 files "commit" "--amend" "-C" "HEAD"
                                    "--only" "--")))
               (vc-start-entry (file rev comment initial-contents msg action 
                                     &optional after-hook)
                               (funcall action file rev "No comment")))
          ;; Inhibe l'effet d'origine du Ctrl-U (à savoir, mode verbose):
          (ad-set-arg 0 nil)
          ad-do-it)
      ad-do-it)))

;; Les environnements de dév pour langages de script qui tirent des
;; liens symboliques au moment de «compiler», c'est BIEN.  Encore
;; faut-il qu'Emacs soit au courant :
(custom-set-variables '(vc-follow-symlinks t))

;; Par défaut sous emacs, vc-cvs fiche le bazar en matière de copies de
;; secours. Ceci désactive les copies en "machin.txt.~1.1~"...
(eval-after-load "vc-cvs" '(defun vc-cvs-make-version-backups-p (ignored) nil))

;; ... et ceci réactive les copies de secours ordinaires pour les
;; fichiers sous CVS / RCS. C'est alors le comportement ordinaire,
;; (fichiers tilde ou un répertoire de fichiers de secours, selon la
;; valeur de backup-directory-alist ci-dessus), qui prend effet.
(custom-set-variables '(vc-make-backup-files t))

;; Support des équivalents de PCL-CVS pour d'autres SCM (se lance avec
;; M-x svn-status, M-x git-status etc)
(autoload 'svn-status "psvn"
  "Examine the status of Subversion working copy in directory DIR."
  'interactive)

(autoload 'magit-status "magit"
  "Entry point into magit."
  'interactive)
(eval-after-load "magit" '(progn
  (custom-set-faces
    '(magit-diff-file-header ((t (:inherit diff-header))))
    '(magit-diff-hunk-header ((t (:inherit diff-header))))
    '(magit-diff-add ((t (:inherit diff-added))))
    '(magit-diff-del ((t (:inherit diff-removed))))
    '(magit-diff-none ((t (:inherit diff-context))))
    '(magit-item-highlight ((t))))
  (if (not (fboundp 'start-file-process))  ;; Bugware pour vieux Emacs :
      (defalias 'start-file-process 'start-process))))

(pointemacs-progression "Débogueurs") ;;;;;;;;;;;;;;;;;;;;;;;

;; Stupide GUD, qui chdir() dans le répertoire des programmes qu'il lance !
(custom-set-variables '(gud-chdir-before-run nil))

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
(custom-set-variables '(eval-expression-print-length nil)
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
(eval-after-load "moz"
  '(progn (defun moz-send-defun ()
            "Send the current function to Firefox via MozRepl.
Curent function is the one recognized by `js2-mode-function-at-point'."
            (interactive)
            (let ((node (js2-mode-function-at-point)))
              (moz-send-region (js2-node-abs-pos node)
                               (js2-node-abs-end node))))
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

(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
;;(eval-after-load "pymacs"
;; '(add-to-list 'pymacs-load-path "your-pymacs-directory"))

;; À mon âge, on apprécie les gadgets de confort...
(when (and (require-faible 'mwheel) (fboundp 'mwheel-install))
  (mwheel-install))

;; Impression : sous Linux j'utilise kprinter qui à l'inverse de l'icône «print
;; buffer» de la barre d'outils a l'avantage de posséder un bouton «annuler»...
(custom-set-variables '(ps-paper-type (quote a4)))
(let ((kprinter (executable-find "kprinter")))
  (if kprinter (custom-set-variables `(lpr-command ,kprinter))))

;; Dired (gestionnaire de fichiers)
(setq dired-guess-have-gnutar "tar"   ; pour XEmacs
      dired-shell-have-gnutar "tar") ; pour Emacs
(add-hook 'dired-mode-hook (lambda () (setq dired-omit-files-p t)) t)

;; Courrier électronique: le vieux mode s'appelait vm, le nouveau sendmail
(if (featurep 'mac-carbon)
    ;; Par défaut, Carbon Emacs envoie ses mails en faisant ouvrir une
    ;; url mailto: au shell graphique, mais 1) c'est crade et 2) ça ne
    ;; lance pas le mailer que je voudrais sur mon MacBook.  En
    ;; revanche, j'ai configuré Postfix !
    (custom-set-variables '(send-mail-function 'sendmail-send-it)))

;; org-mode : ma vie en texte plat
(custom-set-variables 
 '(org-startup-truncated nil)
 '(org-lowest-priority ?D))

;; Fabrication de liens en syntaxe org (associé à Ctrl-C Ctrl-L,
;; cf. «Affectation des touches» ci-dessus)
(autoload 'org-store-link "org"
  "Store an org-link to the current location." t)
  
;; Vu sur http://www.mail-archive.com/emacs-orgmode@gnu.org/msg06568.html
(defun pointemacs-org-lien-vers-numero-de-ligne ()
  "Crée des liens avec un numéro de ligne pour les fichiers de code source."
  (when (memq major-mode '(c-mode c++-mode java-mode python-mode))
    ;; Je préfère éditer la description des liens manuellement, plutôt que
    ;; de la saisir au minibuffer.
    (setq description "")
    (number-to-string (org-current-line))))

(add-hook 'org-create-file-search-functions
          'pointemacs-org-lien-vers-numero-de-ligne)

;; Liens vers des sessions "screen"!  Yow!
(eval-after-load "org" '(require-faible 'org-screen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fini ! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; On charge une *seconde fois* les préférences de customization
;; (.emacs-custom ou .xemacs-custom), de sorte que les customizations
;; sauvegardées par l'utilisateur ont le dernier mot.
(if (file-exists-p custom-file) (load-file custom-file))

;; On se met au bout de l'after-init-hook, pour que notre message
;; final ait vraiment le dernier mot.
(add-hook 'after-init-hook
  (lambda () (pointemacs-progression
     (if pointemacs-reserves
         "Chargement réussi avec réserves, taper C-h v pointemacs-reserves"
       "Chargement réussi"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cave ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ce cruft est rajouté automatiquement par xemacs si on ne l'y met pas
;; soi-même.

(custom-set-faces)
(setq load-home-init-file t) ; don't load init file from ~/.xemacs/init.el

;;; .emacs-DominiqueQuatravaux.el ends here
