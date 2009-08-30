;;; .emacs-planner.el -- configuration planner, muse, remember pour laptop thierry

;       $Id: .emacs-planner.el,v 1.17 2008/03/17 10:35:29 thierry Exp thierry $ 

;;; Code:

(setq planner-project "Thierry")
(add-to-list 'load-path "~/elisp/muse/lisp")
(add-to-list 'load-path "~/elisp/planner")
(add-to-list 'load-path "~/elisp/remember")


(setq muse-project-alist
      '(("Thierry"
         ("~/plans"   ;; ou le dossier de son choix peu importe
          :default "index"
          :major-mode planner-mode
          :visit-link planner-visit-link))))
;;(require 'planner-autoloads) 
(require 'planner)
;;(require 'remember-autoloads)
(require 'remember-planner)
(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)
(defvaralias 'remember-annotation-functions 'planner-annotation-functions) 
(require 'planner-zoom)
(require 'planner-notes-index)
(require 'planner-tasks-overview)
(require 'planner-lisp)
(require 'planner-multi)
(require 'planner-registry)
(planner-registry-initialize)
(planner-registry-insinuate)

;;planner deadline
;;(require 'planner-deadline)
;;(planner-deadline-update)

;;parametres muse
;;(require 'muse-autoloads) 
(add-hook 'find-file-hooks 'muse-mode-maybe)
(require 'muse-mode)     ; load authoring mode
(require 'muse-wiki)
(setq muse-wiki-allow-nonexistent-wikiword t) 
(require 'muse-html)     ; load publishing styles I use
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)
(require 'muse-colors)

;; Commit planner with svn
;; (defun tv-commit-planner (comment)
;;   (interactive "sComment: ")
;;   (and
;;    (shell-command (concat "svn " "update " (planner-directory)) nil nil)
;;    (shell-command (concat "svn " "ci " "-m " comment) nil nil)))

;;quelques racourcis pour planner
(global-set-key (kbd "<f9> t") 'planner-create-task-from-buffer)
(global-set-key (kbd "<f9> s") 'planner-search-notes)
(global-set-key (kbd "<f9> r") 'planner-goto-today)
(global-set-key (kbd "<f9> o") 'planner-tasks-overview-show-summary)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "C-x C-n") 'remember)
(global-set-key (kbd "<f9> y") 'planner-diary-add-entry)
(global-set-key (kbd "<f9> u") 'planner-create-note-from-task)
(global-set-key (kbd "<f9> n") 'planner-notes-index)
(global-set-key (kbd "<f9> l") 'planner-ledger-add-entry-from-task)
(global-set-key (kbd "<f9> x n") 'planner-multi-xref-note)
(global-set-key (kbd "<f9> e t") 'planner-edit-task-description)
(global-set-key (kbd "<f9> h") 'tv-planner-narrow-section-at-point)
(global-set-key (kbd "<f9> d t") 'planner-delete-task)
(define-key planner-mode-map (kbd "<f9> v") 'tv-commit-planner)

;;planner ID
(require 'planner-id)
(setq planner-id-add-task-id-flag t)

;;diary pour planner
(require 'planner-diary)
(add-hook 'diary-display-hook 'fancy-diary-display)
(setq planner-diary-use-diary t
      planner-diary-number-of-days '7
      diary-file "~/plans/diary"
      planner-diary-file diary-file)
(planner-diary-insinuate)

;;Ma daily page avec ledger 

(setq planner-day-page-template
      "* Schedule\n\n\n* Tasks\n\n\n* Ledger\n\n** Pending Transactions\n\n* \
Diary\n\n\n* Favoris\n\n* Notes\n")

;;planner-favoris est loadé depuis ~/elisp/planner
(require 'planner-favoris)
(setq planner-favoris-file "~/plans/.planner-favoris")
(add-hook 'planner-goto-hook 'planner-favoris-insert)
(define-key planner-mode-map (kbd "<f9> a") 'planner-favoris-append-favori)
(define-key planner-mode-map (kbd "<f9> k") 'planner-favoris-delete-favori)

;;Lien calendar et pages planner
(planner-insinuate-calendar)

;;cyclic task pour planner
(require 'planner-cyclic)
(setq planner-cyclic-diary-nag nil)
(setq planner-cyclic-diary-file "~/plans/.diary.cyclic-tasks")

;;apointement section
(require 'planner-appt)
(planner-appt-use-tasks)
(planner-appt-insinuate)
(planner-appt-calendar-insinuate)
(setq planner-appt-task-use-appointments-section-flag t)
(setq planner-appt-sort-schedule-on-update-flag t)
(planner-appt-schedule-cyclic-insinuate)

;;Link w3m in planner
;;planner-w3m should call emacs-w3m:

(if (charsetp 'unicode-bmp)
    (add-to-list 'load-path "~/elisp/emacs-w3m/")
  (add-to-list 'load-path "~/elisp/emacs-w3m-no-uni/"))

(require 'planner-w3m)

;;gnus pour planner
(require 'planner-gnus)
(planner-gnus-insinuate)

;;créer des liens vers irc dans planner
(require 'planner-erc)

;;pour créer un lien bbdb vers planner
(require 'planner-bbdb)

;;Planner-ledger
(require 'planner-ledger)
(add-hook 'planner-goto-hook 'planner-ledger-insert-maybe)

;; switch between last month and this month

(setq ledger-state1 '("this month" "Balance for this month"))

(setq ledger-state2 '("last month" "Balance for last month"))

(setq ledger-state-cur ledger-state1)

(defun planner-ledger-switch-state ()
  (interactive)
  (save-excursion
    (setq ledger-state-cur (if (equal ledger-state-cur ledger-state1)
                           ledger-state2
                         ledger-state1))
    (message (format "Switched to: %s" (nth 1 ledger-state-cur)))
    (sit-for 0.5)
    (message nil)
    (setq planner-ledger-balance-args (list "-p" (nth 0 ledger-state-cur) "-s" "balance"))
    (planner-ledger-insert-balance-maybe)))

(setq planner-ledger-data-file "/home/thierry/finance/ledger.dat")
(setq planner-ledger-balance-args (list "-p" (nth 0 ledger-state-cur) "-s" "balance"))
(setq planner-ledger-register-args '("-U" "register" "socgen"))
(setq planner-ledger-balance-accounts '("income" "expenses"))

;; insert and use table in planner

(defun tv-insert-table (col rows cell-wi cell-he)
  (interactive "sNbrColumns: \nsNbrRows: \nsCellWidth: \nsCellHeight: ")
  (if (equal major-mode (or 'planner-mode 'muse-mode))
      (progn
        (text-mode)
        (table-insert col rows cell-wi cell-he))
    (table-insert col rows cell-wi cell-he)))

(defun tv-back-to-planner-mode ()
  (interactive)
  (save-excursion
    (planner-mode)))

(defun tv-edit-planner-table ()
  (interactive)
  (text-mode)
  (table-recognize))

(global-set-key (kbd "<f9> p t q") 'tv-back-to-planner-mode)
(global-set-key (kbd "<f9> p t i") 'tv-insert-table)
(global-set-key (kbd "<f9> p t e") 'tv-edit-planner-table)

;; A way to index notes in a project page
(require 'planner-notes-index-page)

;; define keys for planner-notes-index-page
(global-set-key (kbd "<f9> i p") 'planner-notes-index-page)
(define-key planner-mode-map (kbd "<f9> p i") 'planner-notes-index-page-in-page)

;; template for indexing notes
(setq planner-plan-page-template "* Tasks\n\n\n* Notes-index\n\n\n* Notes\n\n\n")

;; go strait to project page
(defun tv-get-planner-project (page)
  (interactive (list (completing-read "ProjectPage: "
                                      (mapcar (lambda (x) (nth 0 x))
                                              (nth 3 (planner-tasks-overview-get-summary)))
                                      nil t nil 'minibuffer-history)))
  (find-file-other-window (expand-file-name (concat page ".muse") (planner-directory))))

(global-set-key (kbd "<f9> p p") 'tv-get-planner-project)

;; Toggle narrow/widen section at point
(defvar planner-narrow-status t)

(defun tv-planner-narrow-section-at-point ()
  "Le point doit etre sur la section et pas sur * "
  (interactive)
  (save-excursion
    (let ((section (thing-at-point 'word)))
      (if planner-narrow-status
          (progn
            (planner-narrow-to-section section)
            (setq planner-narrow-status nil))
        (widen)
        (setq planner-narrow-status t)))))

;; log commit svn dans planner
;; (require 'planner-log-edit)
;; (require 'planner-psvn)
;; (setq planner-psvn-log-edit-notice-commit-function t)

;; Cycling in planner-tasks-overview
(defun planner-tasks-overview-n-days (nbrdays)
  "The two functions |tv-cur-date-string
                     |tv-time-date-in-n-days
are needed, they are in macros-func-thierry.el"
  (interactive "nNbrDays: ")
  (if (> nbrdays 0)
      (planner-tasks-overview (tv-cur-date-string)
                              (tv-time-date-in-n-days nbrdays))
    (planner-tasks-overview (tv-time-date-in-n-days nbrdays)
                            (tv-cur-date-string))))

;; Add note to project page even if project page doesn't exist
(defun planner-create-note-in-project-page (newpage)
  "Create note in project page, if not exist create it
if project page is not given create note in current page"
  (interactive (list (completing-read "ProjectPage: "
                                      (mapcar (lambda (x) (nth 0 x))
                                              (nth 3 (planner-tasks-overview-get-summary))))))
  (when (equal major-mode 'planner-mode)
    (if (equal newpage "")
        (planner-create-note)
      (planner-create-note newpage))))

(define-key planner-mode-map (kbd "C-c n") 'planner-create-note-in-project-page)

;; Planner-todo-index - find todos in all project files
(require 'planner-todo-index)

;; Planner trunk
(require 'planner-trunk)
(setq planner-trunk-rule-list '(("." nil ("dvc-hacking"
                                          "TaskPool"
                                          "macros-thierry"
                                          "notes_elisp"
                                          "Shopping"
                                          "Courrier"))))
(add-hook 'planner-mode-hook 'planner-trunk-tasks)

;; Safe start of planner

(defun tv-planner-safe-start ()
  (interactive)
  (if (= (length (file-expand-wildcards "~/plans/#*#" nil)) 0)
      (plan)
    (set-buffer (get-buffer-create "*Planner-Warning*"))
    (erase-buffer) (insert "=====*Planner Warning*=====\n\n")
    (text-mode)
    (insert "Planner have backup files in ~/plans\n" 
            "Consider recovering planner backup files until starting planner again!")
    (display-buffer "*Planner-Warning*")))

(tv-planner-safe-start)

;;; .emacs-planner ends here
