;;; gosh.el --- an interface to Gauche

;; LUSH Lisp Universal Shell
;;   Copyright (C) 2002 Leon Bottou, Yann Le Cun, AT&T Corp, NECI.
;; Includes parts of TL3:
;;   Copyright (C) 1987-1999 Leon Bottou and Neuristique.
;; Includes selected parts of SN3.2:
;;   Copyright (C) 1991-2001 AT&T Corp.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111, USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:

;; Adapted from the Lush <http://lush.sourceforge.net/> Emacs interface.
;; Modifications for Gauche are by Yury Sulsky and Issac Trotts.
;; Original license below:

;; this file contains useful definitions for emacs

;;; Code:

(require 'pcomplete)

;; if this is set to nil then tab completion will read the environment
;; from the gosh process every time, even when nothing new was defined
(defvar cache-symbols t)

(defvar *gosh-symbols* ())
(defvar *gosh-symbols-stale?* t)

(defun gosh-proc ()
  (condition-case err
      (scheme-proc)
    (error ())))

;; Call this with M-x gosh
(defun gosh ()
  "starts Gosh in another subwindow"
  (interactive)
  (switch-to-buffer-other-window "*scheme*")
  (run-scheme "gosh")
  (other-window -1)
  (set-buffer "*scheme*")
  (setq comint-prompt-regexp "^gosh>")
  (setq comint-get-old-input #'gosh-get-incomplete-input)
  (if cache-symbols
      (progn
        ;; override comint-send-string to check for definitions/file loads
        ;; entered by hand in the repl or through the lisp-load-file command
        (defvar prev-comint-send-string (symbol-function 'comint-send-string))
        (defun comint-send-string (proc inp)
          (let ((stale-re "load\\|\\use\\|def\\|require\\|set"))
            (if (and (eq proc (gosh-proc))
                     (string-match stale-re inp))
                (setq *gosh-symbols-stale?* t)))
          (funcall prev-comint-send-string proc inp))

        ;; override comint-send-region to mark the symbol cache stale
        ;; whenever the lisp-eval-* commands are used
        (defvar prev-comint-send-region (symbol-function 'comint-send-region))
        (defun comint-send-region (proc start end)
          (setq *gosh-symbols-stale?* t)
          (funcall prev-comint-send-region proc start end))))
    
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'gosh-parse-arguments)

  (local-set-key "\t" 'pcomplete))

(global-set-key "\C-xg" 'goto-line)

;; Gosh doesn't go into a debug mode the way Lush does.
(defun gosh-avail? (mark)
  t)

;; (defun gosh-avail? (mark)
;;   (let ((prompt (save-excursion
;;                      (search-backward-regexp "^gosh>" nil t))))
;;     (and prompt (= mark (+ prompt 6)))))

(defun filter (p lst)
  (let ((ret ()))
    (mapc (lambda (x) (if (funcall p x) (setq ret (cons x ret)))) lst)
    (nreverse ret)))

;; Be careful; calling this when gosh isn't available will cause this to hang
(defun gosh-symbols ()
  (let ((proc (gosh-proc)))
    (if proc
        (car (read-from-string
              (car (comint-redirect-results-list-from-process
                    proc "(map symbol->string (apply append (map (lambda (m) (hash-table-keys (module-table m))) (all-modules))))" "(.*)" 0))))
      ())))

(defun gosh-complete (sym)
  (let* ((sym-name (downcase (if (stringp sym) sym (symbol-name sym))))
         (minl (length sym-name))
         (proc (gosh-proc)))
    ;; if we have the prompt and the symbols are stale, update *gosh-symbols*
    (if (and *gosh-symbols-stale?*
             proc
             (eq (current-buffer) (process-buffer proc))
             (gosh-avail? (process-mark proc)))
        (progn
          (setq *gosh-symbols* (gosh-symbols))
          (if cache-symbols
              (setq *gosh-symbols-stale?* ()))))
    (filter (lambda (s)
              (and (>= (length s) minl)
                   (string= sym-name (substring s 0 minl))))
            *gosh-symbols*)))

(defun gosh-get-incomplete-input ()
  (let ((proc (gosh-proc)))
    (if (and proc (eq (current-buffer) (process-buffer proc)))
        (buffer-substring (process-mark proc) (point-max))
      "")))

;; see http://www.emacswiki.org/cgi-bin/wiki/PcompleteExamples
(defun gosh-parse-arguments ()
  (save-excursion
    (let* ((cur (point))
           (beg (search-backward-regexp "[][{}#():'\" \t\n]" nil t))
           (pos (if beg (+ beg 1) cur))
           (arg (buffer-substring-no-properties pos cur)))
      (cons (list "gosh-complete" arg)
            (list (point-min) pos)))))

(defun pcomplete/gosh-complete ()
  "Complete the symbol at point"
   (let* ((sym (cadr (car (gosh-parse-arguments))))
          (completions (gosh-complete sym)))
     (if completions
         (throw 'pcomplete-completions completions)
       (pcomplete-here (pcomplete-all-entries)))))

(provide 'gosh)
;;; gosh.el ends here
