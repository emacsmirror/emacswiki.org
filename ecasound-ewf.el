;;; ecasound-ewf.el --- Ecasound .ewf major mode

;; Copyright (C) 2001  Mario Lang 

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: audio, ecasound, wave, file

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a major mode for ecasound .ewf file format.

;;; Code:

(require 'ecasound)

(defgroup ecasound-ewf nil
  "Ecasound .ewf file mode related variables and faces."
  :prefix "ecasound-ewf-")

(defcustom ecasound-ewf-output-device "/dev/dsp"
  "*Default output device used for playing .ewf files."
  :group 'ecasound-ewf
  :type 'string)

(defface ecasound-ewf-keyword-face '((t (:foreground "IndianRed")))
  "The face used for highlighting keywords."
  :group 'ecasound-ewf)

(defface ecasound-ewf-time-face '((t (:foreground "Cyan")))
  "The face used for highlighting time information."
  :group 'ecasound-ewf)

(defface ecasound-ewf-file-face '((t (:foreground "Green")))
  "The face used for highlighting the filname."
  :group 'ecasound-ewf)

(defface ecasound-ewf-boolean-face '((t (:foreground "Orange")))
  "The face used for highlighting boolean values."
  :group 'ecasound-ewf)

(defvar ecasound-ewf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'pcomplete)
    (define-key map "\C-c\C-p" 'ecasound-ewf-play)
    map)
  "Keymap for `ecasound-ewf-mode'.")

(defvar ecasound-ewf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `ecasound-ewf-mode'.")

(defvar ecasound-ewf-font-lock-keywords
  '(("^\\s-*\\(source\\)[^=]+=\\s-*\\(.*\\)$"
     (1 'ecasound-ewf-keyword-face)
     (2 'ecasound-ewf-file-face))
    ("^\\s-*\\(offset\\)[^=]+=\\s-*\\([0-9.]+\\)$"
     (1 'ecasound-ewf-keyword-face)
     (2 'ecasound-ewf-time-face))
    ("^\\s-*\\(start-position\\)[^=]+=\\s-*\\([0-9.]+\\)$"
     (1 'ecasound-ewf-keyword-face)
     (2 'ecasound-ewf-time-face))
    ("^\\s-*\\(length\\)[^=]+=\\s-*\\([0-9.]+\\)$"
     (1 'ecasound-ewf-keyword-face)
     (2 'ecasound-ewf-time-face))
    ("^\\s-*\\(looping\\)[^=]+=\\s-*\\(true\\|false\\)$"
     (1 'ecasound-ewf-keyword-face)
     (2 'ecasound-ewf-boolean-face)))
  "Keyword highlighting specification for `ecasound-ewf-mode'.")

;;;###autoload
(define-derived-mode ecasound-ewf-mode fundamental-mode "EWF"
  "A major mode for editing ecasound .ewf files."
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults)
       '(ecasound-ewf-font-lock-keywords))
  (ecasound-ewf-setup-pcomplete))

;; Pcomplete

(defun ecasound-ewf-keyword-completion-function ()
  (pcomplete-here
   (list "source" "offset" "start-position" "length" "looping")))

(defun pcomplete/ecasound-ewf-mode/source ()
  (pcomplete-here (pcomplete-entries)))

(defun pcomplete/ecasound-ewf-mode/offset ()
  (message "insert audio object at offset (seconds) [read,write]")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-ewf-mode/start-position ()
  (message "start offset inside audio object (seconds) [read]")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-ewf-mode/length ()
  (message "how much of audio object data is used (seconds) [read]")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-ewf-mode/looping ()
  (pcomplete-here (list "true" "false")))

(defun ecasound-ewf-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  (let ((begin (save-excursion (beginning-of-line) (point)))
        (end (point))
        begins args)
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
        (skip-chars-forward " \t\n=")
        (setq begins (cons (point) begins))
        (let ((skip t))
          (while skip
            (skip-chars-forward "^ \t\n=")
            (if (eq (char-before) ?\\)
                (skip-chars-forward " \t\n=")
              (setq skip nil))))
        (setq args (cons (buffer-substring-no-properties
                          (car begins) (point))
                         args)))
      (cons (reverse args) (reverse begins)))))

(defun ecasound-ewf-setup-pcomplete ()
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'ecasound-ewf-parse-arguments)
  (set (make-local-variable 'pcomplete-command-completion-function)
       'ecasound-ewf-keyword-completion-function)
  (set (make-local-variable 'pcomplete-command-name-function)
       (lambda ()
         (pcomplete-arg 'first)))
  (set (make-local-variable 'pcomplete-arg-quote-list)
       (list ? )))

;; Interactive commands

(defun ecasound-ewf-play ()
  (interactive)
  (let ((ecasound-arguments (list "-c"
                                  "-i" buffer-file-name
                                  "-o" ecasound-ewf-output-device)))
    (and (buffer-modified-p)
         (y-or-n-p "Save file before playing? ")
         (save-buffer))
    (ecasound "*Ecasound-ewf Player*")))

(add-to-list 'auto-mode-alist (cons "\\.ewf$" 'ecasound-ewf-mode))

(provide 'ecasound-ewf)
;;; ecasound-ewf.el ends here
