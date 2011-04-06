;;; rtrt-script.el ---
;;to: someone@somecompany.com
;;subject: el pour rtrt, ne pas oublier de mettre à jour le .emacs pour le charger.
;; utilisation de:
;;         - font-lock : coloration syntaxique.
;;         - easy-menu : menu du spécifique.
;;  
(require 'font-lock)     ;; pour la coloration syntaxique.
(require 'compile)       ;; pour les message d'erreur de compilation.
(require 'easymenu)




;; définition du groupe de customisation.
(defgroup rtrt-script nil
  "Mode majeur pour les fichiers de script de Rational Test Real Time."
  :group 'languages)

(defcustom rtrt-script-indent 2
  "*Définition de l'indentation des scripts RTRT."
  :type  'integer
  :group 'rtrt-script)

;;(defcustom rtrt-script-highlight-tab t
;;  "*si vai alors Affichage des tabulations dans les scripts RTRT."
;;  :type  'boolean
;;  :group 'rtrt-script)

(defface rtrt-script-tab-face
  '((((class color)) (:background  "hotpink"))
    (t (:reverse-video t)))
  "*Couleur des tabultations pour les scripts RTRT."
  :group 'faces
  :group 'rtrt-script)

(defface rtrt-script-subl-face
  '((((class color)) (:foreground  "green")))
;;    (t (:reverse-video t)))
  "*Couleur des tabultations pour les scripts RTRT."
  :group 'faces
  :group 'rtrt-script)

; (defgroup rtrt-script-indent nil
;   "Groupe pour l'indentation des scripts pour Rational test real time."
;   :prefix "rtrt-script-indent-"
;   :group 'rtrt-script)

; (defgroup rtrt-script-face nil
;   "Groupe pour la coloration syntaxique des scripts pour Rational test real time."
;   :prefix "rtrt-script-face-"
;   :group 'rtrt-script)

(defvar rtrt-script-mode-hook nil
  "Action a faire quand un script est chargé.")
(defvar rtrt-script-mode-map nil
  "Racourcis clavier spécifiques")

;; -----------------------------------------------------------------------------
;; Définition de la coloration syntaxique.
;; -----------------------------------------------------------------------------

;(defconst rtrt-script-font-lock-keyword
;     '("\\<\\(E\\(NVIRONMENT\\|LEMENT\\)\\|INITIALIZATION\\|DEFINE[ 	]*STUB\\|TE\\(RMINATION\\|ST\\)\\|SERVICE\\)\\>"  . font-lock-keyword-face)

;;(defconst rtrt-script-block-keywords-end
;;  '("\\<\\END[ 	]*\\(E\\(NVIRONMENT\\|LEMENT\\)\\|SERVICE\\|INITIALIZATION\\|DEFINE\\|TEST\\)\\>"  (1 font-lock-keyword-face)
;;    (2 font-lock-function-face)
;;  "Mots clés de fin de block"))

(defconst rtrt-script-font-lock-keyword 
  (list
;;   '("\\<\\(E\\(NVIRONMENT\\|LEMENT\\)\\|INITIALIZATION\\|DEFINE[ \t]*STUB\\|TE\\(RMINATION\\|ST\\)\\|SERVICE\\)\\>[ \t]+\\(\\sw+\\)?"  
;;     (1 font-lock-warning-face)
;;     (2 font-lock-function-name-face)
;;     )
   '("^[ \t]*\\<\\END[ 	]*\\(E\\(NVIRONMENT\\|LEMENT\\)\\|SERVICE\\|INITIALIZATION\\|DEFINE\\|TEST\\)\\>"  . font-lock-keyword-face)
   '("^[ \t]*\\<\\(E\\(NVIRONMENT\\|LEMENT\\)\\|INITIALIZATION\\|USE\\|DEFINE[ 	]*STUB\\|TERMINATION\\|SERVICE\\)\\>"  (0 font-lock-keyword-face) ("\\<[a-z0-9_-]*\\>" nil nil (0 font-lock-variable-name-face)))
   '("^[ \t]*\\<\\(FAMILY\\|SERVICE_TYPE\\|USE\\)\\>" (0 font-lock-builtin-face) ("\\<[a-z0-9_-]*\\>" nil nil (0 font-lock-variable-name-face)))
   '("^[ \t]*\\<BEGIN\\>" . font-lock-builtin-face)
   '("^[ \t]*\\<HEADER\\>" (0 font-lock-builtin-face) ("\\<[a-z0-9$_-]*\\>" nil nil (0 font-lock-variable-name-face)))
;;   '("^[ \t]*\\<TEST\\>" (0 font-lock-keyword-face) ("\\<[a-z0-9_'éàçà-]*\\>" nil nil (0 font-lock-variable-name-face)))
   '("^[ \t]*\\(\\<TEST\\>\\)[ \t]*\\(.+\n\\)" (1 font-lock-keyword-face) (2 font-lock-variable-name-face nil t))
   '("^[ \t]*\\<COMMENT\\>*\\>" (0 font-lock-comment-face)  ("\\<[a-zéèàç'0-9_-]*\\>" nil nil (0 font-lock-string-face)))
;;   '("^[ \t]*\\(#\\)\\(.+\n\\)" (1 font-lock-builtin-face)  (2 rtrt-script-subl-face nil t))
   '("^[ \t]*#[ \t]*" (0 font-lock-builtin-face) )
   '("^[ \t]*\\<INCLUDE[ 	]*\\(CODE\\|PTU\\)\\>"  (0 font-lock-builtin-face)
     ("\\<[a-z0-9$_-]*\\>" nil nil (0 font-lock-variable-name-face)))
   '("^[ \t]*\\<SIMUL\\>"                        . font-lock-keyword-face)
   '("\\<\\(EV[ 	]+IN\\|EV\\)\\>"      . font-lock-keyword-face)
   '("^[ \t]*\\<FORMAT\\>"                       . font-lock-keyword-face)
   '("^[ \t]*\\<END[ 	]*IF\\>"              . font-lock-keyword-face) 
   '("^[ \t]*\\<IF\\>"                           . font-lock-keyword-face)
   '("^[ \t]*\\<ELSE_SIMUL\\>"                   . font-lock-keyword-face) 
   '("^[ \t]*\\<ELSE\\>"                         . font-lock-keyword-face)
   '("\\<\\(INIT[ 	]+IN\\|INIT\\)\\>"    . font-lock-keyword-face)
   '("\\<\\(MAX\\|MIN\\|DELTA\\)\\>"        . font-lock-builtin-face)
   '("^[ \t]*\\<NEXT_TEST\\>"                    . font-lock-keyword-face) 
   '("^[ \t]*\\(VAR\\|STR\\|ARRAY\\|STUB\\)[ \t]+\\([^(,\n]*\\)" (1 font-lock-type-face nil t) (2 font-lock-variable-name-face nil t))
   '("^[ \t]*\\(VAR\\|STR\\|ARRAY\\)[ \t]+\\([^(,]*\\)[ \t]*,[ \t]*\\(init\\)[ \t]*=*[ \t]*\\(.*\\)[ \t]*,[ \t]*\\(ev\\)[ \t]*=*[ \t]*\\(.*\\)\n"
    (1 font-lock-type-face nil t) (2 font-lock-variable-name-face nil t)
    (3 font-lock-keyword-face nil t) (4 font-lock-variable-name-face nil t)
    (5 font-lock-keyword-face nil t) (6 font-lock-variable-name-face nil t)
    )
   '("^[ \t]*\\(&\\)[ \t]+\\(init\\|ev\\)[ \t]*=*[ \t]*\\([^\n,]*\\)"
     (1 font-lock-builtin-face) (2 font-lock-keyword-face nil t)
     (3 font-lock-variable-name-face nil t))
   '("^[ \t]*\\<STUB\\>"                          . font-lock-type-face)
   '("^[ \t]*\\<ARRAY\\>"                        . font-lock-type-face) 
   '("^[ \t]*\\<STRUCTURE\\>"                    . font-lock-type-face) 
   '("^[ \t]*\\<STR\\>"                          . font-lock-type-face)
   '("^[ \t]*&[ \t]+"                   . font-lock-builtin-face)
   )
   ;;   (cond (rtrt-script-highlight-tab 
;;   '("\\(\\(\t*\\| *\\)*$\\|\t\\)" . 'rtrt-script-tab-face))
  ;;)
  "Mots clés du langage de script RTRT.")

;; -----------------------------------------------------------------------------
;; Indentation du code.
;; -----------------------------------------------------------------------------
(defun rtrt-script-indent-line ()
  "Indent current line as RTRT-SCRIPT code"
  (interactive)
  (save-excursion                ; si la ligne précedente est test ou 

    (beginning-of-line)
    (if (bobp)                          ; Check for rule 1
        (indent-line-to 0)
      (let ((not-indented t) 	  cur-indent)
        (if (looking-at "^[ \t]*--[ -=]*\n") ; indentation des commentaires
            (progn             
              (save-excursion    ; si la ligne précedente est test ou 
                (forward-line -1)       ; on indente au même niveau.
                (if (looking-at "^[ \t]*\\<\\(TEST\|SERVICE\\)\\>")
                    (setq cur-indent (current-indentation))
                  (while not-indented
                    (forward-line -1)
                
                    ;; on ignore les lignes blanches et les commentaires.
                    (while (looking-at "^[ \t]*\n")
                      (forward-line -1))
                    (if (looking-at "^[ \t]*\\<END\\>") ; Check for rule 3
                        (progn
                        
                          (setq cur-indent (current-indentation))
                          (setq not-indented nil))
					; Check for rule 4
                      (if (looking-at 
                           "^[ \t]*\\<\\(ENVIRONMENT\\|ELEMENT\\|INITIALIZATION\\|DEFINE[ \t]+STUB\\|TERMINATION\\|TEST\\|SERVICE\\|NEXT_TEST\\)\\>")
                          (progn
                            (setq cur-indent (+ (current-indentation) rtrt-script-indent))
                            (setq not-indented nil))
                        (if (bobp)      ; Check for rule 5
                            (setq not-indented nil))	
                        ))))
                )
              )
          (if (looking-at "^[ \t]*\\<\\(END\\|NEXT_TEST\\)\\>") ; Check for rule 2
              (progn
                (save-excursion
                  (forward-line -1)
                
                  ;; on ignore les lignes blanches.
                  (while (looking-at "^[ \t]*\\(\n\\|--.*\n\\)")
                    (forward-line -1))
                
                  (setq cur-indent (- (current-indentation) rtrt-script-indent)))
                (if (< cur-indent 0)
                    (setq cur-indent 0)))
            (save-excursion 
              (while not-indented
                (forward-line -1)
              
                ;; on ignore les lignes blanches et les commentaires.
                (while (looking-at "^[ \t]*\\(\n\\|--.*\n\\)")
                  (forward-line -1))
            
                (if (looking-at "^[ \t]*\\<END\\>") ; Check for rule 3
                    (progn
		    
                      (setq cur-indent (current-indentation))
                      (setq not-indented nil))
					; Check for rule 4
                  (if (looking-at 
                       "^[ \t]*\\<\\(ENVIRONMENT\\|ELEMENT\\|INITIALIZATION\\|DEFINE[ \t]+STUB\\|TERMINATION\\|TEST\\|SERVICE\\|NEXT_TEST\\)\\>")
                      (progn
                        (setq cur-indent (+ (current-indentation) rtrt-script-indent))
                        (setq not-indented nil))
                    (if (bobp)          ; Check for rule 5
                        (setq not-indented nil))	
                    ))))
            )
          )
        (if cur-indent
            (progn
              (save-excursion    ; si la ligne précedente est test ou 

                (message "cur_indent =>%d" cur-indent)
                (if (looking-at "^[ \t]*\n")
                    (indent-line-to 0)
                  (indent-line-to cur-indent)
                  ;; tant qu'a faire on retire aussi les tabulations.
                  (untabify (point-at-bol) (point-at-eol)))
                )
              )
          (indent-line-to 0)))
      )
    )
  ) ;; If we didn't see an indentation hint, then allow no indentation

;; -----------------------------------------------------------------------------
;; Définition des extensions de fichiers utilisant ce mode.
;; -----------------------------------------------------------------------------
(setq auto-mode-alist
      (append
       '(("\\.ptu\\'" . rtrt-script-mode))
       auto-mode-alist))

;; -----------------------------------------------------------------------------
;; paramètres de compilation.
;; -----------------------------------------------------------------------------
(defconst rtrt-script-error-regexp-alist
  (list
   '("^TestRT .+ \\[\\([^,]+\\),\\([0-9]+\\)\\]" 1 2)
   '("^TestRT-E-.+, \\[\\([^:]+\\):\\([0-9]+\\)\\] .*" 1 2)
   '("^TestRT-W-.+, \\[\\([^:]+\\):\\([0-9]+\\)\\] .*" 1 2)
   '("^TestRT-F-.+, \\[\\([^:]+\\):\\([0-9]+\\)\\] .*" 1 2)
   '("^TestRT-I-.+, \\[\\([^:]+\\):\\([0-9]+\\)\\] .*" 1 2)
   '("^TestRT .+ \\[\\([^,]+\\),\\([0-9]+\\)\\]" 1 2)
   '("^TestRT-E-.+, \\[\\([A-Z]:[^:]+\\):\\([0-9]+\\)\\] .*" 1 2) ;; DOS
   '("^TestRT-W-.+, \\[\\([A-Z]:[^:]+\\):\\([0-9]+\\)\\] .*" 1 2) ;; DOS 
   '("^TestRT-F-.+, \\[\\([A-Z]:[^:]+\\):\\([0-9]+\\)\\] .*" 1 2) ;; DOS
   '("^TestRT-I-.+, \\[\\([A-Z]:[^:]+\\):\\([0-9]+\\)\\] .*" 1 2) ;; DOS
   )
  "Message d'erreur de compilation avec RTRT.")

(setq compilation-error-regexp-alist
      (append
       rtrt-script-error-regexp-alist
       compilation-error-regexp-alist))

;; -----------------------------------------------------------------------------
;; Définition de la table syntaxique.
;; -----------------------------------------------------------------------------
(defvar rtrt-script-mode-syntax-table nil
  "Syntax table for rtrt-script-mode.")

(defun rtrt-script-create-syntax-table ()
  (if rtrt-script-mode-syntax-table
      ()
    (setq rtrt-script-mode-syntax-table (make-syntax-table))
    
                                        ; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" rtrt-script-mode-syntax-table)
;;    (modify-syntax-entry ?# "w" rtrt-script-mode-syntax-table)
;;    (modify-syntax-entry ?. "w" rtrt-script-mode-syntax-table)
    
    ; Commentaire C/C++
    (modify-syntax-entry ?/ ". 124b" rtrt-script-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" rtrt-script-mode-syntax-table)
    ; Plus les --
    (modify-syntax-entry ?- ". 12b" rtrt-script-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" rtrt-script-mode-syntax-table))
  (set-syntax-table rtrt-script-mode-syntax-table)
  )

;; ------------------------------------------------------------------------------
;; Définition du mode.
;; ------------------------------------------------------------------------------
(defun rtrt-script-mode ()
  "major mode for RTRT."
  (interactive)
  (kill-all-local-variables)
  (setq comment-start "--")
  (rtrt-script-create-syntax-table)
  
  ;; Set up font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(rtrt-script-font-lock-keyword nil t))

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'rtrt-script-indent-line)
  (setq major-mode 'rtrt-script-mode)
  (setq mode-name "rtrt-script")
  (run-hooks 'rtrt-script-mode-hook))

(provide 'rtrt-script-mode)
