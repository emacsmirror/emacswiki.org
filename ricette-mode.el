;;; ricette-mode.el --- vedere ricette

;;; Copyright (C) 2002 Thien-Thi Nguyen

;; This program is available under the same terms as GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Usare: Mettere la formula:
;;
;;   (add-to-list 'auto-mode-alist
;;                '("^/home/ale/etc/ricette/" . ricette-mode))
;;
;; in ~/.emacs e garantire che questo documento (ricette-mode.el)
;; sia in una "directory" che è presente in `load-path'.  (Variare come vuoi.)
;;
;; La funzione `ricetta-nuova' domanda il nome della ricetta e prepara
;; il buffer vuoto con lo stile desiderato (titoli ecc.).
;; È possibile invocare questa funzione automaticamente aggiungendo la formula:
;;
;;   (add-hook 'ricette-mode-hook 'ricetta-nuova)
;;
;; Questo programma è dedicato alla mia maestra -- Alessandra Bertoni
;; (e il suo cervello bello 8-).

;;; Code:

;; questo macro usa gli altri, possibile è problema per alcuni. (mi scusa!)
(defmacro ricette-viso-nuovo (nome col speg)
  (let ((proprio (intern (concat "ricette-" (symbol-name nome) "-viso"))))
    `(progn
       (defvar  ,proprio ',proprio)
       (defface ,proprio '((t (:foreground ,(symbol-name col)))) ,speg))))

(ricette-viso-nuovo  testo  cyan2 "Viso da usare per il testo di ricette.")
(ricette-viso-nuovo titoli violet "Viso da usare per i titoli di ricette.")
(ricette-viso-nuovo quanto yellow "Viso da usare per \"g NNN\" e le altre quantità.")

;;- (defvar  ricette-testo-viso 'ricette-testo-viso)
;;- (defface ricette-testo-viso '((t (:foreground "cyan2")))
;;-   "Viso da usare per il testo di ricette.")
;;-
;;- (defvar  ricette-titoli-viso 'ricette-titoli-viso)
;;- (defface ricette-titoli-viso '((t (:foreground "violet")))
;;-   "Viso da usare per i titoli di ricette.")
;;-
;;- (defvar  ricette-quanto-viso 'ricette-quanto-viso)
;;- (defface ricette-quanto-viso '((t (:foreground "yellow")))
;;-   "Viso da usare per \"g NNN\" e le altre quantità.")

(defvar ricette-font-lock-keywords
  '(("^[ \t]*\\([A-ZÀÈÌÒÙ\t ,]+\\)[ \t]*$" 1 ricette-titoli-viso)
    ("^\\(Preparazione\\|Ingredienti\\)" 0 ricette-titoli-viso)
    ("^[*] \\([-,./0-9]+\\(\\s-*[mkK]*[gl]\\>\\)*\\)" 1 ricette-quanto-viso)
    ("\\<[kK]*g [-,./0-9]+$" 0 ricette-quanto-viso)
    ("^\\s-*\\([^\n:]+\\):" 1 ricette-titoli-viso)
    ("." 0 ricette-testo-viso)))	; default (forse lento)

(defvar ricette-font-lock-defaults
  '(ricette-font-lock-keywords t))	; t => solo "keywords"

(defun ricette-aggiungere-ingrediente ()
  (interactive)
  (goto-char (point-max))
  (re-search-backward "^Preparazione")
  (re-search-backward "^[*]")
  (end-of-line)
  (insert "\n* "))

(defun ricette-aggiungere-ingrediente-successivo ()
  (interactive)
  (insert "\n* "))

(defun ricetta-nuova ()
  "Domandare il nome e preparare i titoli."
  (interactive)
  (when (= 0 (buffer-size))
    (let ((nome (read-string "Ricette nuova! Nome? ")))
      (insert (upcase nome)))
    (center-line)
    (insert "\n\n\n\n\n")
    (insert "Ingredienti (per N persone)\n\n* \n\n")
    (insert "Preparazione\n\n...\n")))

(defun ricette-mode-flush-right ()
  (interactive)
  (let ((beg (progn (beginning-of-line) (delete-horizontal-space) (point)))
        (end (progn (end-of-line) (delete-horizontal-space) (point))))
    (beginning-of-line)
    (insert (make-string (- fill-column (- end beg)) 32))))

(define-derived-mode ricette-mode text-mode "Ricette"
  "Major mode per le ricette.
\\{ricette-mode-map}"
  ;; key bindings
  (define-key ricette-mode-map "\C-c\C-i" 'ricette-aggiungere-ingrediente)
  (define-key ricette-mode-map "\C-j" 'ricette-aggiungere-ingrediente-successivo)
  (define-key ricette-mode-map "\M-r" 'ricette-mode-flush-right)
  ;; font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults ricette-font-lock-defaults)
  ;; misc
  (setq fill-column 70))

(provide 'ricette-mode)
;;; ricette-mode.el ends here
