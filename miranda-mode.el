;;; miranda-mode.el. Major mode for editing Miranda.
;; 
;; Originally hugs-mode.el.
;;
;; Copyright (C) 1989, Free Software Foundation, Inc., Lars Bo Nielsen
;; and Lennart Augustsson
;; modified by Peter Thiemann, March 1994
;; modified for hugs and xemacs by Olaf Chitil, April 1995
;; modified for Miranda by Martin Schwenke, December 1996
;;
;; $Id: miranda-mode.el,v 1.2 2001/03/15 04:17:22 martin Exp $

;; This file is not officially part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; ORIGINAL AUTHOR
;;         Lars Bo Nielsen
;;         Aalborg University
;;         Computer Science Dept.
;;         9000 Aalborg
;;         Denmark
;;
;;         lbn@iesd.dk
;;         or: ...!mcvax!diku!iesd!lbn
;;         or: mcvax!diku!iesd!lbn@uunet.uu.net
;;
;; MODIFIED FOR Haskell BY
;;	   Lennart Augustsson
;;	   indentation stuff by Peter Thiemann
;; MODIFIED FOR Hugs BY
;;         Olaf Chitil
;;         http://www-i2.informatik.RWTH-Aachen.de/~chitil
;; MODIFIED FOR Miranda BY
;;         Martin Schwenke <martin@meltin.net>
;;         http://meltin.net/hacks/emacs/
;;
;;
;; Please let me know if you come up with any ideas, bugs, or fixes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; src: https://web.archive.org/web/20060719004306/http://meltin.net/hacks/emacs/src/miranda-mode.el [2026-03-14]

(eval-when-compile
  (require 'shell))

(defconst miranda-mode-version-string
  "miranda-mode $Revision: 1.2 $")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTANTS CONTROLLING THE MODE.
;;;
;;; These are the constants you might want to change,
;;; eg. for using the mode with Gofer instead of Miranda.
;;; 

;; The command used to start up the miranda-program.
(defconst miranda-prog-name "mira" "*Name of program to run as miranda.")

;; The left delimmitter for `load file'
(defconst miranda-use-left-delim "/file "
  "*The left delimiter for the filename when using \"file\".")

;; The right delimmitter for `/file file'
(defconst miranda-use-right-delim "\n"
  "*The right delimiter for the filename when using \"file\".")

(defconst miranda-edit-string "/edit\n"
  "*Command for popping up window for the current script.")

(defconst miranda-use-left-delim-find "/find "
  "*The left delimiter for the identifier when using \"find\".")

(defconst miranda-use-right-delim-find "\n"
  "*The right delimiter for the identifier when using \"find\".")
  

;; A regular expression matching the prompt pattern in the inferior
;; shell
(defconst miranda-shell-prompt-pattern "^Miranda *"
  "*The prompt pattern for the inferior shell running Miranda.")

;;
(defconst miranda-first-command ""
  "*A command send to the inferior shell after starting miranda.")

;; The name of the process running Miranda.
(defconst miranda-process-name "Miranda" "*The name of the Miranda-process")

;; The name of the buffer displaying the Miranda process.
;; the buffer).
(defconst miranda-buffer-name (concat "*" miranda-process-name "*")
  "*The name of the Miranda-buffer")
;;;
;;; END OF CONSTANTS CONTROLLING THE MODE.
;;;
;;; If you change anything below, you are on your own.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar miranda-mode-syntax-table nil "The syntax table used in miranda-mode.")

(defvar miranda-mode-map nil "The mode map used in miranda-mode.")

(defvar miranda-mode-abbrev-table nil "The abbrev-table used in miranda-mode.")

(defun miranda-mode ()
  "Major mode for editing Miranda scripts and executing them in Miranda.
Tab indents for Miranda code.
Shift-Return executes a Return and a Tab.
Comments are delimited with --
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Key bindings:
=============

\\[miranda-mode-version]\t  Get the version of miranda-mode.
\\[miranda-pop-to-shell]\t  Pop to the Miranda window.
\\[miranda-load-saved-buffer]\t  Save the buffer, and load it into Miranda.
\\[miranda-load-file]\t  Asks for file name, and loads this script into Miranda.
\\[miranda-edit]\t  Pops up window for current script.
\\[miranda-find]\t  Prompts for an identifier and pops up window with its definition.
\\[miranda-evaluate-expression]\t  Prompts for an expression and evalute it.


Mode map
========
\\{miranda-mode-map}
Runs miranda-mode-hook if non nil."
  (interactive)
  (kill-all-local-variables)
  (if miranda-mode-map
      ()
    (setq miranda-mode-map (make-sparse-keymap))
    (define-key miranda-mode-map [(control c) (control v)] 'miranda-mode-version)
    (define-key miranda-mode-map [(control c) (control s)] 'miranda-pop-to-shell)
    (define-key miranda-mode-map [(control c) (control l)] 'miranda-load-saved-buffer)
    (define-key miranda-mode-map [(control c) (control o)] 'miranda-load-file)
    (define-key miranda-mode-map [(control c) (control e)] 'miranda-edit)
    (define-key miranda-mode-map [(control c) (control f)] 'miranda-find)
    (define-key miranda-mode-map [(control c) (control x)] 'miranda-evaluate-expression)
    (define-key miranda-mode-map [(shift return)] 'newline-and-indent)

    (make-variable-buffer-local 'indent-line-function)
    (setq indent-line-function 'indent-relative)
    (define-key miranda-mode-map "\177"     'backward-delete-char-untabify))
  (use-local-map miranda-mode-map)

  (setq major-mode 'miranda-mode)
  (setq mode-name "Miranda")
  (define-abbrev-table 'miranda-mode-abbrev-table ())
  (setq local-abbrev-table miranda-mode-abbrev-table)
  (if miranda-mode-syntax-table
      ()
    (setq miranda-mode-syntax-table (make-syntax-table))

    (modify-syntax-entry ?-  "_ 23" miranda-mode-syntax-table)
    (modify-syntax-entry ?\\ "\\"     miranda-mode-syntax-table)
    (modify-syntax-entry ?*  "_"      miranda-mode-syntax-table)
    (modify-syntax-entry ?_  "_"      miranda-mode-syntax-table)
    (modify-syntax-entry ?'  "_"      miranda-mode-syntax-table)
    (modify-syntax-entry ?:  "_"      miranda-mode-syntax-table)
    (modify-syntax-entry ?|  "."      miranda-mode-syntax-table)
    )
  (set-syntax-table miranda-mode-syntax-table)
  (make-local-variable 'require-final-newline) ; Always put a new-line
  (setq require-final-newline t)	; in the end of file
  (make-local-variable 'comment-start)
  (setq comment-start "|| ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 60)		; Start of comment in this column
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "{-+ *\\|--+ *") ; This matches a start of comment
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line nil)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(miranda-font-lock-keywords t t))

  (run-hooks 'miranda-mode-hook))		; Run the hook

(defun miranda-mode-version ()
  (interactive)
  (message miranda-mode-version-string))

(defun turn-on-miranda-menu ()
  (interactive)
  (require 'easymenu)
  (easy-menu-define
   miranda-mode-menu
   miranda-mode-map
   "Menu keymap for Miranda mode."
   '("Miranda"
     ["Load into Miranda" miranda-load-saved-buffer t]
     ["Find Definition" miranda-find t]
     ["Evaluate Expression" miranda-evaluate-expression t])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INFERIOR SHELL
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar miranda-shell-map nil "The mode map for miranda-shell.")

(defun miranda-shell ()
  "Inferior shell invoking Miranda.
It is not possible to have more than one shell running Miranda.
Like the shell mode with the same additional command as miranda-mode.

For variables controlling the mode see \"miranda-mode.el\".

Runs miranda-shell-hook if not nil."
  (interactive)
  (if (not (process-status miranda-process-name))
      (save-excursion			; Process is not running
	(message "Starting Miranda...")	; start up a new process

	(setenv "RECHECKMIRA" "YES")

	(require 'shell)
	(set-buffer (make-comint miranda-process-name miranda-prog-name))
	(erase-buffer)			; Erase the buffer if a previous
	(if miranda-shell-map		; process died in there
	    ()
	  (setq miranda-shell-map (copy-keymap shell-mode-map))
    (define-key miranda-shell-map [(control c) (control v)] 'miranda-mode-version)
    (define-key miranda-shell-map [(control c) (control l)] 'miranda-load-file)
    (define-key miranda-shell-map [(control c) (control o)] 'miranda-load-file)
    (define-key miranda-shell-map [(control c) (control e)] 'miranda-edit)
    (define-key miranda-shell-map [(control c) (control f)] 'miranda-find)
    (define-key miranda-shell-map [(control c) (control x)] 'miranda-evaluate-expression)
	  )
	(use-local-map miranda-shell-map)
	(make-local-variable 'shell-prompt-pattern)
	(setq shell-prompt-pattern miranda-shell-prompt-pattern)
	(setq major-mode 'miranda-shell)
	(setq mode-name "Miranda Shell")
	(setq mode-line-format 
	      "-----Emacs: %17b   %M   %[(%m: %s)%]----%3p--%-")
	(set-process-filter (get-process miranda-process-name) 'miranda-process-filter)
	(process-send-string miranda-process-name miranda-first-command)
	(insert miranda-first-command)	
	(message "Starting Miranda...done.")
	(run-hooks 'miranda-shell-hook))))

(defun miranda-process-filter (proc str)
  (let ((cur (current-buffer))
	(pop-up-windows t))
    (pop-to-buffer miranda-buffer-name)
    (goto-char (point-max))
    (if (string= str "\b\b\b  \b\b\b")
	(backward-delete-char 4)
      (insert str))
    (set-marker (process-mark proc) (point-max))
    (pop-to-buffer cur)))

;;--------------

(defun miranda-pop-to-shell ()
  (interactive)
  (miranda-shell)
  (pop-to-buffer miranda-buffer-name))

(defun miranda-load-file (fil)
  (interactive "FLoad script: ")
  (miranda-shell)
  (save-some-buffers)
  (process-send-string miranda-process-name
		       (concat miranda-use-left-delim (expand-file-name fil)
			       miranda-use-right-delim)))

(defun miranda-load-saved-buffer ()
  "Save the buffer, and send a `use file' to the inferior shell
running Miranda."
  (interactive)
  (let (file)
    (if (setq file (buffer-file-name))	; Is the buffer associated
	(progn				; with file ?
	  (save-buffer)
	  (miranda-shell)
	  (process-send-string miranda-process-name
		       (concat miranda-use-left-delim
			       (expand-file-name file)
			       miranda-use-right-delim)))
      (error "Buffer not associated with file."))))

(defun miranda-evaluate-expression (h-expr)
  "Prompt for and evaluate an expression"
  (interactive "sExpression: ")
  (let ((str (concat h-expr "\n"))
	(buf (current-buffer)))
    (miranda-pop-to-shell)
    (insert str)
    (process-send-string miranda-process-name str)
    (pop-to-buffer buf)))

(defun miranda-edit ()
   (interactive)
   (let ((buf (current-buffer)))
     (save-some-buffers)
     (miranda-pop-to-shell)
     (process-send-string miranda-process-name miranda-edit-string)
     (pop-to-buffer buf)))

(defun miranda-find (id)
  "Prompt for an identifier"
  (interactive "sIdentifier: ")
  (let ((buf (current-buffer)))
    (miranda-pop-to-shell)
    (process-send-string miranda-process-name
			 (concat miranda-use-left-delim-find
				 id
				 miranda-use-right-delim-find))
    (pop-to-buffer buf)))

;; ------------------------
;; font-lock-mode patterns, based on specs. in an earlier version
;; of haskell-mode.el


(defconst miranda-font-lock-keywords nil
 "Conservative highlighting of a Miranda buffer
(using font-lock.)")

(let ((miranda-id "[a-z_][a-zA-Z0-9_'#]+")
      (miranda-reserved-ids
	   (concat "\\b\\(" 
                   (mapconcat 
		       'identity
		       '("abstype" "with" "type"
		         "if" "otherwise"
		         "where"
		         "readvals" "show" "let"
			 "%include" "%export" "%free" "%nolist"
		         "then"  "to" "type" "where" "infix[rl]?")
		        "\\|")
	           "\\)[ \t\n:,]")))

      (setq miranda-font-lock-keywords
       (list
         '("||.*$". font-lock-comment-face)
         ; type declarations:
	 '("^>?[ \t\n]*\\([^:\n]*\\)\\(::=\\|::\\|==\\)" . font-lock-function-name-face)
         ; defining `='
         (list "[ \t\n]\\(=\\)[ \t\n]"   0 'font-lock-keyword-face)
         ; guard `|'
         (list "^>?[ \t\n]*\\(|\\)[ \t\n]"   0 'font-lock-keyword-face)
         (list miranda-reserved-ids   0 'font-lock-keyword-face)
       )))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; END OF Miranda-MODE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'miranda-mode)
