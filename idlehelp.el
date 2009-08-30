;;; idlehelp.el --- Display help when being idle.

;; Copyright (C) 2005  Jorgen Schaefer

;; Version: 1.0
;; Keywords: help
;; Author: Jorgen Schaefer
;; URL: http://www.emacswiki.org/elisp/idlehelp.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; A help system that displays help information when you're idle.
;; Modeled after eldoc, but extensible.

;; An example I use for learning some important AucTeX key shortcuts:
;;
;; (setq idlehelp-help-list
;;       (append `((latex-mode
;;                  my-LaTeX-help-in-itemize-p
;;                  "M-RET inserts \\item, C-c C-c compiles, C-c C-v views")
;;                 (latex-mode
;;                  bolp
;;                  ,(concat "C-c C-s inserts section, C-c C-e inserts environment,"
;;                           " C-c C-c compiles, C-c C-v views")))
;;               idlehelp-help-list))
;;
;; (defun my-LaTeX-help-in-itemize-p ()
;;   "Return non-nil when we're in an itemize environment."
;;   (save-excursion
;;     (and (re-search-backward "\\\\\\(begin{\\(.*\\)}\\|end{itemize}\\)")
;;          (string= (match-string 2)
;;                   "itemize"))))

;; Code:

(defgroup idlehelp nil
  "The customizeable, extensible, context-sensitive help system."
  :group 'applications)

(defcustom idlehelp-idle-time-seconds 0.2
  "The idle time after which the system displays a help string,
in seconds."
  :type 'number
  :group 'idlehelp)

(defcustom idlehelp-help-list nil
  "The list of help items.
Each entry is a list with three elements:
1) The mode name where this should be active as a symbol.
2) The predicate to run to find the context, which can be nil if
   the context is always correct.
3) The help string to show, or a procedure that returns the help
   string when called."
  :type '(repeat (list symbol function string))
  :group 'idlehelp)

(defvar idlehelp-current-help-message nil
  "The current help message shown by idlehelp.")
(make-variable-buffer-local 'idlehelp-current-help-message)

(defvar idlehelp-idle-timer nil
  "The timer object, if non-nil.")

(define-minor-mode idlehelp-mode
  "Display context-sensitive help when the user is idle."
  :init-value nil
  (cond
   (idlehelp-mode
    (when idlehelp-idle-timer
      (cancel-timer idlehelp-idle-timer))
    (setq idlehelp-idle-timer (run-with-idle-timer 0.5 t 'idlehelp-idle-timer))
    (add-hook 'pre-command-hook 'idlehelp-pre-command-refresh-echo-area nil t))
   (t
    (remove-hook 'pre-command-hook 'idlehelp-pre-command-refresh-echo-area))))

(defun turn-on-idlehelp-mode ()
  "Turn on idlehelp mode."
  (interactive)
  (idlehelp-mode t))

(defun idlehelp-idle-timer ()
  "Run the actions described in `idlehelp-help-list'."
  (when idlehelp-mode
    (catch 'exit
      (mapc (lambda (entry)
              (let ((wanted-mode (nth 0 entry))
                    (correct-context-p (nth 1 entry))
                    (msg (nth 2 entry))
                    (message-log-max nil)) ; Don't log this
                (when (and (eq major-mode wanted-mode)
                           (or (not correct-context-p)
                               (funcall correct-context-p)))
                  (setq idlehelp-current-help-message
                        (if (functionp msg)
                            (funcall msg)
                          msg))
                  (message "%s" idlehelp-current-help-message)
                  (throw 'exit 1))))
            idlehelp-help-list)
      (setq idlehelp-current-help-message nil))))

(defun idlehelp-pre-command-refresh-echo-area ()
  "Show the last help output again.
This is necessary as emacs clears the echo area on movement."
  (if (and idlehelp-current-help-message
           (not executing-kbd-macro)
           (not (and (boundp 'edebug-active)
                     edebug-active))
           (not cursor-in-echo-area)
           (not (eq (selected-window)
                    (minibuffer-window))))
      (message "%s" idlehelp-current-help-message)
    (setq idlehelp-current-help-message nil)))

(provide 'idlehelp)
;;; idlehelp.el ends here
