;;; query.el
;----Le laboratoire de robotique de l'Institut de recherche d'Hydro-Quebec-----
; 
; Nom     : (confirm PROMPT) (query-{string,character} PROMPT CHOICES)
; Fonction: Functions to query the user.
; Fichiers: query.el
; Notes   : 
; 
; Cree    : 30 janvier 90 ------- Martin Boyer <mboyer@ireq-robot.uucp>
; Modifie :  1 mars 94 --------1- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
;           Copyright (c) 1990, 1994 Martin Boyer and Hydro-Quebec
; 
; Historique: 
; 
;  1 mars 94 --------1- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
;       Version 1.0: Prepared for LCD distribution.
;------------------------------------------------------------------------------

;;; COPYRIGHT NOTICE
;;;
;; Copyright (C) 1990, 1994 Martin Boyer and Hydro-Quebec.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; author of this program <mboyer@ireq-robot.hydro.qc.ca> or to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Send bug reports to the author, <mboyer@ireq-robot.hydro.qc.ca>.

;;; DESCRIPTION
;;;
;; This library implements a small number of functions that can be used
;; by other programs to prompt the user for answers, with easily
;; customizable default values.
;;
;; The user can specify (globally) how much confirmation is required
;; for a function in this library to accept the user's answer.
;;
;; The "confirm" function is a simple yes-or-no type of query.
;; The "query-string" function can be used to request a string answer.
;; The "query-character" function can be used to prompt for a single
;; character.

;;; INSTALLATION
;;;
;; Put this file somewhere in your load-path and byte-compile it.
;; Other libraries that wish to use this one should put a
;;
;; (require 'query)
;;
;; statement at the top of the file.
;;
;; OPTIONALLY, you can put the following lines in your .emacs:
;;
;; (require 'query)
;; (setq confirm-level 'single)
;; (setq allow-confirm-defaults t)
;; (fset 'yes-or-no-p 'confirm)
;; (fset 'y-or-n-p 'confirm)
;;
;; This will make all yes-or-no prompting accept a single 'y' or 'n'
;; for an answer.  See the documentation for the "confirm-level"
;; variable for other options.

;; LCD Archive Entry:
;; query|Martin Boyer|gamin@ireq-robot.hydro.qc.ca|
;; Functions to query the user|
;; 01-Mar-1994|1.0|~/packages/query.el.Z|

;; Changes from version 1.0 to version 1.0.1 (Claus Brunzema):
;; - Comment changes to enable upload to http://www.emacswiki.org.
;; - Typo fix.


(require 'cl)
(provide 'query)

(defconst query-version "1.0.1"
  "The version number of the \"query\" library.")

(defvar confirm-level 'expand
  "*Degree of confirmation required by the confirm function.
 'full    requires \"yes\" or \"no\" followed by RET
 'expand  requires any subset of \"yes\" or \"no\" followed by expansion and RET
 'partial requires any subset of \"yes\" or \"no\" followed by RET
 'single  requires \"y\" or \"n\".")

(defvar allow-confirm-defaults nil
  "*If true, allow use of default values in confirmation.")

(defun confirm (message &optional default)
  "Ask user to confirm, in a way depending on the value of confirm-level."
  (if (string= (substring message -1) " ")
      (setq message (substring message 0 -1)))
  (cond ((eq confirm-level 'single)
         (single-y-or-n-p message default))
        ((eq confirm-level 'partial)
         (partial-y-or-n-p message default))
        ((eq confirm-level 'expand)
         (expand-y-or-n-p message default))
        ((eq confirm-level 'full)
         (full-y-or-n-p message default)))
  )

(defun query-string (prompt choices)
  "Prints PROMPT and returns a string which must be one of CHOICES.
CHOICES is a list of strings. The first choice is the default,
which is returned if the user simply types RET."
  (let ((default (car choices))
        answer)
    (setq prompt (concat prompt " ["
                         (mapconcat (function (lambda (x) x))
                                    choices ",") "] "))
    (setq choices (nconc (mapcar (function (lambda (x) (list x t)))
                                  choices)
                         '('("" t))))
    (setq answer (completing-read prompt choices nil t ""))
    (if (string= answer "")
        (setq answer default))
    answer
    )
  )

(defun query-character (prompt choices &optional no-quit)
  "Prints PROMPT and returns a character which must be one of CHOICES.
CHOICES is a string of character choices. The first character is the default,
which is returned if the user simply types SPC, RET, or LFD.
Cursor is not moved during input.
If optional NO-QUIT is non-nil, returns nil instead of exiting if
keyboard-quit is invoked (and the key to invoke it is not one of the choices)."
  (let ((default (string-to-char choices))
        (inhibit-quit no-quit)
        answer)
    (message "%s [%s] " prompt choices)
    (setq answer (read-char))
    (cond ((or (eq answer ? ) (eq answer ?\r) (eq answer ?\n))
           (setq answer default))
          ((string-match (regexp-quote (char-to-string answer)) choices)
           answer)
          ((eq (key-binding (char-to-string answer)) 'keyboard-quit)
           (setq answer 'keyboard-quit))
          ((setq answer nil)))
    (while (not answer)
      (message "%s [%s only] " prompt choices)
      (setq answer (read-char))
      (cond ((or (eq answer ? ) (eq answer ?\r) (eq answer ?\n))
             (setq answer default))
            ((string-match (regexp-quote (char-to-string answer)) choices)
             answer)
            ((eq (key-binding (char-to-string answer)) 'keyboard-quit)
             (setq answer 'keyboard-quit))
            ((setq answer nil))))
    (if (eq answer 'keyboard-quit)
        (setq answer nil quit-flag nil))
    answer
    )
  )

(defun read-char-from-minibuffer (&optional prompt)
  (let (cursor-in-echo-area)
    (setq cursor-in-echo-area t)
    (and prompt (message prompt))
    (read-char))
  )

(defun single-y-or-n-p (&optional prompt default)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
No confirmation of the answer is requested; a single character is enough.
The PROMPT is optional.
If allow-confirm-defaults is t and DEFAULT is provided,
SPC, TAB, RET, and LFD return nil if DEFAULT is zero, otherwise return t."
  
  (let (char
        (choice (if (and allow-confirm-defaults default)
                    (if (eq default 0)
                        "[n/y]"
                      "[y/n]")
                  "(y/n)")))
    (setq char (downcase
                (read-char-from-minibuffer (format "%s %s " prompt choice))))
    (when (and allow-confirm-defaults default)
      (if (string-match (char-to-string char) " \t\n\r")
          (if (eq default 0)
              (setq char ?n)
            (setq char ?y))))
    (setq choice (if (and allow-confirm-defaults default)
                     (if (eq default 0)
                         "[n/y only]"
                       "[y/n only]")
                   "(y/n only)"))

    (while (not (or (= char ?y) (= char ?n)))
      (setq char (downcase
                  (read-char-from-minibuffer
                   (format "%s %s " prompt choice))))
      (when (and allow-confirm-defaults default)
        (if (string-match (char-to-string char) " \t\n\r")
            (if (eq default 0)
                (setq char ?n)
              (setq char ?y))))
      )

    (if (= char ?y) t)
    )
  )

(defun partial-y-or-n-p (&optional prompt default)
  "Ask user a \"yes or no\" question.  Return t if answer is \"yes\".
The user must confirm the answer with a newline, which is accepted as long as
the answer is a subset of \"yes\" or \"no\".
SPC and TAB provide expansion.
The PROMPT is optional.
If allow-confirm-defaults is t and DEFAULT is provided,
SPC and TAB expand to the DEFAULT,
RET and LFD return nil if DEFAULT is zero, otherwise return t."
  (if (not allow-confirm-defaults) (setq default nil))
  (let ((answer "") (completion "") (error nil)
        (choice (if default (if (eq default 0)
                                "[no/yes]"
                              "[yes/no]")
                  "(yes/no)")))

    (while (not (or (string= answer "yes") (string= answer "no")))
      (setq answer (read-no-blanks-input (format "%s %s " prompt choice) completion))
      (when default (if (string= answer "")
                        (if (eq default 0)
                            (setq answer "no")
                          (setq answer "yes"))))
    
      (when (or (= last-command-char ? ) (= last-command-char ?\t)
                (= last-command-char ?\n) (= last-command-char ?\r))
        (setq completion
              (try-completion answer '(("yes" 1) ("no" 2) ("YES" 3) ("NO" 4))))
        )
      (if (null completion)
          (setq answer "" completion "" error t)
        (progn
          (when (eq completion t) (setq completion answer))
          (if (or (= last-command-char ?\n) (= last-command-char ?\r))
              ;; If there is a completion, it is unique (check it!).
              (setq answer (downcase completion))
            (setq answer ""))
          )
        )

      (when error (setq choice (if default (if (eq default 0)
                                               "[no/yes only]"
                                             "[yes/no only]")
                                 "(yes/no only)")))
      )
    (if (string= answer "yes") t)
    )
  )

(defun expand-y-or-n-p (&optional prompt default)
  "Ask user a \"yes or no\" question.  Return t if answer is \"yes\".
The user must confirm the answer with a newline, which is accepted
if and only if the answer is one of \"yes\" or \"no\".
SPC and TAB provide expansion.
The PROMPT is optional.
If allow-confirm-defaults is t and DEFAULT is provided,
SPC and TAB expand to the DEFAULT,
RET and LFD return nil if DEFAULT is zero, otherwise return t."
  (if (not allow-confirm-defaults) (setq default nil))
  (let ((answer "") (completion "") (error nil)
        (choice (if default (if (eq default 0)
                                "[no/yes]"
                              "[yes/no]")
                  "(yes/no)")))

    (while (not (eq completion t))
      (setq answer (read-no-blanks-input (format "%s %s " prompt choice) completion))
      (when default (if (string= answer "")
                        (if (eq default 0)
                            (setq answer "n")
                          (setq answer "y"))))
    
      (when (or (= last-command-char ? ) (= last-command-char ?\t)
                (= last-command-char ?\n) (= last-command-char ?\r))
        (setq completion
              (try-completion answer '(("yes" 1) ("no" 2) ("YES" 3) ("NO" 4))))
        )
      (if (null completion)
          (setq answer "" completion "" error t)
        (if (eq completion t)
            (unless (or (= last-command-char ?\n) (= last-command-char ?\r))
              (setq completion answer))
          (setq answer (downcase completion)))
        )

      (when error (setq choice (if default (if (eq default 0)
                                               "[no/yes only]"
                                             "[yes/no only]")
                                 "(yes/no only)")))
      )
    (if (string= answer "yes") t)
    )
  )

(defun full-y-or-n-p (&optional prompt default)
  "Ask user a \"yes or no\" question.  Return t if answer is \"yes\".
The user must confirm the answer with a newline, which is accepted
if and only if the answer is one of \"yes\" or \"no\".
The PROMPT is optional.
If allow-confirm-defaults is t and DEFAULT is provided,
SPC and TAB expand to the DEFAULT,
RET and LFD return nil if DEFAULT is zero, otherwise return t."
  (if (not allow-confirm-defaults) (setq default nil))
  (let ((answer "") (completed nil) (error nil)
        (choice (if default (if (eq default 0)
                                "[no/yes]"
                              "[yes/no]")
                  "(yes/no)")))

    (while (not completed)
      (setq answer (read-no-blanks-input (format "%s %s " prompt choice) answer))
      (setq completed
            (and (or (string= answer "yes") (string= answer "no"))
                 (or (= last-command-char ?\n) (= last-command-char ?\r))))
      (when default (if (string= answer "")
                        (if (eq default 0)
                            (setq answer "no")
                          (setq answer "yes"))))
    
      (setq answer (downcase answer))
      (unless (or (string= answer "yes") (string= answer "no"))
        (setq choice (if default (if (eq default 0)
                                     "[no/yes only]"
                                   "[yes/no only]")
                       "(yes/no only)")))
      )
    (if (string= answer "yes") t)
    )
  )

;;; query.el ends here.
