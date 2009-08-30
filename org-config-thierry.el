;;; org-config-thierry.el --- My config for org
;; 
;; Author: Thierry Volpiatto
;; Maintainer: Thierry Volpiatto
;; 
;; Created: jeu. avril  2 14:10:06 2009 (+0200)
;; Version: 
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (tv-ee-index-create)
;;;;«INDEX»
;;; «.auto-fill-mode»                            (to "auto-fill-mode")
;;; «.Use-enter-to-follow-links»                 (to "Use-enter-to-follow-links")
;;; «.Use-my-func-with-ido-and-not-org-iswitchb» (to "Use-my-func-with-ido-and-not-org-iswitchb")
;;; «.Todo-rules»                                (to "Todo-rules")
;;; «.Tags-setting»                              (to "Tags-setting")
;;; «.Insinuate-remember»                        (to "Insinuate-remember")
;;; «.Org-remember-templates»                    (to "Org-remember-templates")
;;; «.org-annotation-helper»                     (to "org-annotation-helper")
;;; «.Insinuate-Diary»                           (to "Insinuate-Diary")
;;; «.Insinuate-appt»                            (to "Insinuate-appt")
;;; «.Subtasks»                                  (to "Subtasks")
;;; «.leave-a-blank-line-when-insert-new-item»   (to "leave-a-blank-line-when-insert-new-item")
;;; «.Insert-cyclic-entries-in-ledger»           (to "Insert-cyclic-entries-in-ledger")
;;; «.Use-anything-in-org»                       (to "Use-anything-in-org")
;;; «.org-cycle-date»                            (to "org-cycle-date")
;;; «.Provide»                                   (to "Provide")

;;;==UPDATE-EEV-BOUNDARY== ;; (Don't delete this line!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;; «auto-fill-mode» (to ".auto-fill-mode")
;; (set to 78 in files)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; «Use-enter-to-follow-links» (to ".Use-enter-to-follow-links")
(setq org-return-follows-link t)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-files '("~/org"))

;; «Use-my-func-with-ido-and-not-org-iswitchb» (to ".Use-my-func-with-ido-and-not-org-iswitchb")
(defvar my-org-files '("agenda.org" "notes.org" "my-blog.org"))
(defun tv-find-org-files (fname)
  (interactive (list (completing-read "OrgFiles: "
                                      (if (bufferp (get-buffer "*Org Agenda*"))
                                          (cons "*Org Agenda*" my-org-files)
                                          my-org-files))))
  (if (equal fname "*Org Agenda*")
      (display-buffer fname)
    (find-file (expand-file-name fname org-directory))))

(global-set-key (kbd "C-c b") 'tv-find-org-files)

;; «Todo-rules» (to ".Todo-rules")
;; (find-node "(org)Fast access to TODO states")
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "INPROGRESS(i)" "DEFERRED(d)" "|" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO"      .  traverse-match-face)
        ("INPROGRESS" . traverse-regex-face)
        ("BUG" . (:foreground "VioletRed4" :weight bold))
        ("FIXED" . (:foreground "SpringGreen4" :weight bold))
        ("DEFERRED"  . shadow)
        ("CANCELED"  . (:foreground "blue" :weight bold))))

(setq org-log-done 'time)
(setq org-use-fast-todo-selection t)
(setq org-reverse-note-order t)

;; «Tags-setting» (to ".Tags-setting")
;; (find-node "(org)Setting tags")
(setq org-tag-alist '(("@home" . ?h)
                      ("running" . ?r)
                      ("velo" . ?v)
                      ("@climbing" . ?c)
                      ("buy" . ?b)
                      ("laptop" . ?l)
                      ("pc" . ?p)
                      ("dev" . ?d)
                      ("courrier" . ?C)
                      ("mail" . ?M)
                      ("phone" . ?P)
                      ("bank" . ?B)
                      ("bug" . ?E)
                      ("travel" . ?t)))

;; «Insinuate-remember» (to ".Insinuate-remember")
(org-remember-insinuate)
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
(setq org-remember-store-without-prompt t)
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; «Org-remember-templates» (to ".Org-remember-templates")
;; (find-node "(org)Remember templates")
(setq org-remember-templates
      '(("BROWSER" ?W "* BROWSER %?\n %:description\n  (created: %U)\n\n  %c\n\n  %i" "~/org/notes.org" "Firefox")
        ("Todo" ?t "** TODO %?\n  %i\n  %a" "~/org/agenda.org" "Tasks")
        ("Report" ?R "** REPORT %?\n  %i\n  %a" "~/org/agenda.org" "Development")
        ("Notes" ?n "* %U %?\n\n  %i\n  %a" "~/org/notes.org" "General")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "New Ideas")
        ("Lisp" ?l "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Notes elisp")
        ("Python" ?p "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Notes python")
        ("Emacs" ?e "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Memo Emacs")
        ("Stump" ?s "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Memo Stumpwm")
        ("Linux" ?L "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Memo Linux")
        ("Gentoo" ?g "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Notes Gentoo")
        ("Web" ?w "* %u %c \n\n%i" "~/org/notes.org" "Web links")))

;; «org-annotation-helper» (to ".org-annotation-helper")
(require 'org-annotation-helper)

;; «Insinuate-Diary» (to ".Insinuate-Diary")
(setq org-agenda-include-diary t)

;; «Insinuate-appt» (to ".Insinuate-appt")
(require 'appt)
(setq appt-time-msg-list nil)
(org-agenda-to-appt)
;; When use 'r' (rebuild agenda) reload appt
(add-hook 'org-agenda-mode-hook #'(lambda ()
                                    (setq appt-time-msg-list nil)
                                    (org-agenda-to-appt)))
(setq appt-audible t)
(setq appt-display-format 'window);echo
(appt-activate 1)
(global-set-key (kbd "<f5> d a") 'appt-add)

;; «Subtasks» (to ".Subtasks")
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; «leave-a-blank-line-when-insert-new-item» (to ".leave-a-blank-line-when-insert-new-item")
(setq org-blank-before-new-entry '((heading . t)
                                   (plain-list-item . nil)))

;; «Insert-cyclic-entries-in-ledger» (to ".Insert-cyclic-entries-in-ledger")
(defun tv-org-add-ledger-entry-from-todo (payee amount)
  (interactive)
  (save-excursion
    (find-file "~/finance/financehg/ledger.dat")
    (ledger-add-entry (concat
                       (replace-regexp-in-string "\\." "/" (dvc-cur-date-string))
                       " "
                       payee
                       " "
                       amount))))

;; «Use-anything-in-org» (to ".Use-anything-in-org")
(defun anything-org-headlines-only ()
  (interactive)
  (tv-anything-in-miniwindows #'(lambda ()
                                  (anything 'anything-c-source-org-headline))))

(define-key org-mode-map (kbd "<f11> o") 'anything-org-headlines-only)
                                             

;; «Provide» (to ".Provide")
(provide 'org-config-thierry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-config-thierry.el ends here
;; Local Variables:
;; eev-comment-prefix: ";"
;; ee-anchor-format: "«%s»"
;; End:
