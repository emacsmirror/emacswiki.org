;;; slang-mode.el --- a major-mode for editing slang scripts

;; Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

;; Modified By: Joe Robertson <jmrobert@sbcs.com>
;; Modified From: tcl-mode.el
;; 
;; Original Author: Gregor Schmid <schmid@fb3-s7.math.tu-berlin.de>
;; Keywords: languages, processes, tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Add these lines to your .emacs file to enable slang mode
;; slang mode
;; (autoload 'slang-mode "slang-mode"
;;  "Mode for editing slang source files")
;; (setq auto-mode-alist
;;      (append '(("\\.sl$" . slang-mode)) auto-mode-alist))

;; Special Thanks to Simon Marshall <simonm@mail.esrin.esa.it> for
;; font-lock patches.

;; This file was written with emacs using Jamie Lokier's folding mode
;; That's what the funny ;;{{{ marks are there for

;;{{{ Usage

;; Slang-mode supports c-mode style formatting and sending of
;; lines/regions/files to a slang interpreter. An interpreter (see
;; variable `slang-default-application') will be started if you try to
;; send some code and none is running. You can use the process-buffer
;; (named after the application you chose) as if it were an
;; interactive shell. See the documentation for `comint.el' for
;; details.

;;}}}
;;{{{ Key-bindings

;; To see all the keybindings for folding mode, look at `slang-setup-keymap'
;; or start `slang-mode' and type `\C-h m'.
;; The keybindings may seem strange, since I prefer to use them with
;; slang-prefix-key set to nil, but since those keybindings are already used
;; the default for `slang-prefix-key' is `\C-c', which is the conventional
;; prefix for major-mode commands.

;; You can customise the keybindings either by setting `slang-prefix-key'
;; or by putting the following in your .emacs
;; 	(setq slang-mode-map (make-sparse-keymap))
;; and
;; 	(define-key slang-mode-map <your-key> <function>)
;; for all the functions you need.

;;}}}
;;{{{ Variables

;; You may want to customize the following variables:
;; 	slang-indent-level
;; 	slang-always-show
;;	slang-mode-map
;;	slang-prefix-key
;;	slang-mode-hook
;; 	slang-default-application
;; 	slang-default-command-switches

;;}}}

;;; Code:

;; We need that !
(require 'comint)

;;{{{ variables

(defgroup slang nil
  "Major mode for editing slang code."
  :prefix "slang-"
  :group 'languages)

(defcustom slang-default-application "/usr/bin/slsh"
  "Default slang application to run in slang subprocess."
  :type 'string
  :group 'slang)

(defcustom slang-default-command-switches nil
  "Command switches for `slang-default-application'.
Should be a list of strings."
  :type '(repeat string)
  :group 'slang)

(defvar slang-process nil
  "The active slang subprocess corresponding to current buffer.")

(defvar slang-process-buffer nil
  "Buffer used for communication with slang subprocess for current buffer.")

(defcustom slang-always-show t
  "*Non-nil means display slang-process-buffer after sending a command."
  :type 'boolean
  :group 'slang)

(defvar slang-mode-map nil
  "Keymap used with slang mode.")

(defvar slang-prefix-key "\C-c"
  "Prefix for all slang-mode commands.")

(defcustom slang-mode-hook nil
  "Hooks called when slang mode fires up."
  :type 'hook
  :group 'slang)

(defvar slang-region-start (make-marker)
  "Start of special region for slang communication.")

(defvar slang-region-end (make-marker)
  "End of special region for slang communication.")

(defcustom slang-indent-level 4
  "Amount by which slang subexpressions are indented."
  :type 'integer
  :group 'slang)

(defcustom slang-default-eval "eval"
  "Default command used when sending regions."
  :type 'string
  :group 'slang)

(defvar slang-mode-menu (make-sparse-keymap "Slang-Mode")
  "Keymap for slang-mode's menu.")

(defvar slang-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Function name declarations.
     '("\\<\\(islang_class\\|class\\|method\\|proc\\|body\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     ;;
     ;; Keywords.
;(make-regexp '("if" "then" "else" "elseif" "for" "foreach" "break"
;	       "continue" "while" "eval" "case" "in" "switch" "default"
;	       "exit" "error" "proc" "return" "uplevel" "constructor"
;	       "destructor" "islang_class" "loop" "for_array_keys"
;	       "for_recursive_glob" "for_file"))
     (concat "\\<\\("
	     "break\\|case\\|else\\|if\\|!if\\|for"
	     "\\|each\\|else\\|else if\\|loop"
	     "\\|namespace\\|eval\\|export"
	     "\\|orelse\\|andelse\\|message\\|(s|f|)print."
	     "\\|return\\|switch\\|while"
	     "\\|not\\|do\\|forever\\|foreach\\|using"
	     "\\|return\\|continue\\|error"
	     "\\|static\\|variable\\|implements\\|reshape"
	     "\\|struct\\|(EXECUTE_|)ERROR_BLOCK"
	     "\\|define"
	     "\\)\\>")
     ;;
     ;; Types.
;   (make-regexp '("global" "upvar" "variable" "inherit" "public"
;		   "private" "protected" "common"))
     (cons (concat "\\<\\("
		   "common\\|global\\|inherit\\|p\\(r\\(ivate\\|otected\\)\\|ublic\\)"
		   "\\|upvar\\|variable\\|.*_Type"
		   "\\)\\>")
	   'font-lock-type-face)
     ))
  "Default expressions to highlight in SLANG modes.")

(defvar slang-imenu-generic-expression
  '(
    (nil "^\\s-*\\(proc\\|body\\)\\s-+\\(\\(\\s_\\|\\sw\\)+\\)" 2)
    ("Classes" "^\\s-*class\\s-+\\(\\(\\s_\\|\\sw\\)+\\)" 1))
  
  "Imenu generic expression for slang-mode.  See `imenu-generic-expression'.")


;;}}}
;;{{{ slang-mode

;;;###autoload
(defun slang-mode ()
  "Major mode for editing slang scripts.
The following keys are bound:
\\{slang-mode-map}
"
  (interactive)
  (let ((switches nil)
	s)
    (kill-all-local-variables)
    (setq major-mode 'slang-mode)
    (setq mode-name "SLANG")
    (set (make-local-variable 'slang-process) nil)
    (set (make-local-variable 'slang-process-buffer) nil)
    (make-local-variable 'slang-default-command-switches)
    (set (make-local-variable 'indent-line-function) 'slang-indent-line)

    (set (make-local-variable 'comment-start) "% ")
    (set (make-local-variable 'comment-start-skip) "% *")
    (set (make-local-variable 'font-lock-defaults)
	 '(slang-font-lock-keywords nil nil ((?_ . "w") (?: . "w"))))


    (set (make-local-variable 'imenu-generic-expression)
	 slang-imenu-generic-expression)
    (setq imenu-case-fold-search nil)
    (setq imenu-syntax-alist '((?: . "w")))
    (make-local-variable 'slang-default-eval)
    (or slang-mode-map
	(slang-setup-keymap))
    (use-local-map slang-mode-map)
    (set-syntax-table (copy-syntax-table))
    ;;real comment keys is right here
    (modify-syntax-entry ?# "<")
    (modify-syntax-entry ?% "<")
    (modify-syntax-entry ?\n ">")
    ;; look for a #!.../wish -f line at bob
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "#![ \t]*\\([^ \t]*\\)[ \t]\\(.*[ \t]\\)-f")
	  (progn
	    (set (make-local-variable 'slang-default-application)
		 (buffer-substring (match-beginning 1)
				   (match-end 1)))
	    (if (match-beginning 2)
		(progn
		  (goto-char (match-beginning 2))
		  (set (make-local-variable 'slang-default-command-switches) nil)
		  (while (< (point) (match-end 2))
		    (setq s (read (current-buffer)))
		    (if (<= (point) (match-end 2))
			(setq slang-default-command-switches
			      (append slang-default-command-switches
				      (list (prin1-to-string s)))))))))
	;; if this fails, look for the #!/bin/csh ... exec hack
	(while (eq (following-char) ?#)
	  (forward-line 1))
	(or (bobp)
	    (forward-char -1))
	(if (eq (preceding-char) ?\\)
	    (progn
	      (forward-char 1)
	      (if (looking-at "exec[ \t]+\\([^ \t]*\\)[ \t]\\(.*[ \t]\\)*-f")
		  (progn
		    (set (make-local-variable 'slang-default-application)
			 (buffer-substring (match-beginning 1)
					   (match-end 1)))
		    (if (match-beginning 2)
			(progn
			  (goto-char (match-beginning 2))
			  (set (make-local-variable
				'slang-default-command-switches)
			       nil)
			  (while (< (point) (match-end 2))
			    (setq s (read (current-buffer)))
			    (if (<= (point) (match-end 2))
				(setq slang-default-command-switches
				      (append slang-default-command-switches
					      (list (prin1-to-string s)))))))))
		)))))
    (run-hooks 'slang-mode-hook)))

;;}}}
;;{{{ slang-setup-keymap

(defun slang-setup-keymap ()
  "Set up keymap for slang mode.
If the variable `slang-prefix-key' is nil, the bindings go directly
to `slang-mode-map', otherwise they are prefixed with `slang-prefix-key'."
  (setq slang-mode-map (make-sparse-keymap))
  (define-key slang-mode-map [menu-bar slang-mode]
    (cons "Slang-Mode" slang-mode-menu))
  (let ((map (if slang-prefix-key
		 (make-sparse-keymap)
	       slang-mode-map)))
  ;; indentation
  (define-key slang-mode-map [?}] 'slang-electric-brace)
  ;; communication
  (define-key map "\M-e" 'slang-send-current-line)
  (define-key map "\M-r" 'slang-send-region)
  (define-key map "\M-w" 'slang-send-proc)
  (define-key map "\M-a" 'slang-send-buffer)
  (define-key map "\M-q" 'slang-kill-process)
  (define-key map "\M-u" 'slang-restart-with-whole-file)
  (define-key map "\M-s" 'slang-show-process-buffer)
  (define-key map "\M-h" 'slang-hide-process-buffer)
  (define-key map "\M-i" 'slang-get-error-info)
  (define-key map "\M-[" 'slang-beginning-of-proc)
  (define-key map "\M-]" 'slang-end-of-proc)
  (define-key map "\C-\M-s" 'slang-set-slang-region-start)
  (define-key map "\C-\M-e" 'slang-set-slang-region-end)
  (define-key map "\C-\M-r" 'slang-send-slang-region)
  (if slang-prefix-key
      (define-key slang-mode-map slang-prefix-key map))
  ))

;;}}}
;;{{{ indentation

;;{{{ slang-indent-line

(defun slang-indent-line ()
  "Indent current line as slang code.
Return the amount the indentation changed by."
  (let ((indent (slang-calculate-indentation nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (save-excursion
      (while (eq (following-char) ?})
	(setq indent (max (- indent slang-indent-level) 0))
	(forward-char 1)
	(if (looking-at "\\([ \t]*\\)}")
	    (progn
	      (delete-region (match-beginning 1) (match-end 1))
	      (insert-char ?  (1- slang-indent-level))))))
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

;;}}}
;;{{{ slang-calculate-indentation

(defun slang-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as slang code.
In usual case returns an integer: the column to indent to."
  (let ((pos (point)))
    (save-excursion
      (if parse-start
	  (setq pos (goto-char parse-start)))
      (beginning-of-line)
      (if (bobp)
	  (current-indentation)
	(forward-char -1)
	(if (eq (preceding-char) ?\\)
	    (+ (current-indentation)
	       (progn
		 (beginning-of-line)
		 (if (bobp)
		     (* 2 slang-indent-level)
		   (forward-char -1)
		   (if (not (eq (preceding-char) ?\\))
		       (* 2 slang-indent-level)
		     0))))
	  (forward-char 1)
	  (if (re-search-backward
	       "\\(^[^ \t\n\r%]\\)\\|\\({\\s *[%\n]\\)\\|\\(}\\s *\n\\)"
	       nil  t)
	      (+ (- (current-indentation)
		    (if (save-excursion
			  (beginning-of-line)
			  (and (not (bobp))
			       (progn
				 (forward-char -1)
				 (eq (preceding-char) ?\\))))
			(* 2 slang-indent-level)
		      0))
		 (if (eq (following-char) ?{)
		     slang-indent-level
		   0))
	    (goto-char pos)
	    (beginning-of-line)
	    (forward-line -1)
	    (current-indentation)))))))

;;}}}
;;{{{ slang-electric-brace

(defun slang-electric-brace (arg)
  "Insert `}' and indent line for slang."
  (interactive "P")
  (insert-char ?} (prefix-numeric-value arg))
  (slang-indent-line)
  (blink-matching-open))

;;}}}

;;}}}
;;{{{ searching

;;{{{ slang-beginning-of-proc

(defun slang-beginning-of-proc (&optional arg)
  "Move backward to the beginning of a slang proc (or similar).
With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of proc.
Returns t unless search stops due to beginning or end of buffer."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
	(ret t))
    (if (and (< arg 0)
	     (looking-at "^[^ \t\n%][^\n]*{[ \t]*$"))
	(forward-char 1))
    (while (< arg 0)
      (if (re-search-forward "^[^ \t\n%][^\n]*{[ \t]*$" nil t)
	  (setq arg (1+ arg)
		found t)
	(setq ret nil
	      arg 0)))
    (if found
	(beginning-of-line))
    (while (> arg 0)
      (if (re-search-backward "^[^ \t\n%][^\n]*{[ \t]*$" nil t)
	  (setq arg (1- arg))
	(setq ret nil
	      arg 0)))
    ret))

;;}}}
;;{{{ slang-end-of-proc

(defun slang-end-of-proc (&optional arg)
  "Move forward to next end of slang proc (or similar).
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end of proc.

This function just searches for a `}' at the beginning of a line."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
	(ret t))
    (if (and (< arg 0)
	     (not (bolp))
	     (save-excursion
	       (beginning-of-line)
	       (eq (following-char) ?})))
	(forward-char -1))
    (while (> arg 0)
      (if (re-search-forward "^}" nil t)
	  (setq arg (1- arg)
		found t)
	(setq ret nil
	      arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^}" nil t)
	  (setq arg (1+ arg)
		found t)
	(setq ret nil
	      arg 0)))
    (if found
	(end-of-line))
    ret))

;;}}}

;;}}}
;;{{{ communication with a inferior process via comint

;;{{{ slang-start-process

(defun slang-start-process (name program &optional startfile &rest switches)
  "Start a slang process named NAME, running PROGRAM."
  (or switches
      (setq switches slang-default-command-switches))
  (setq slang-process-buffer (apply 'make-comint name program startfile switches))
  (setq slang-process (get-buffer-process slang-process-buffer))
  (save-excursion
    (set-buffer slang-process-buffer)
    (setq comint-prompt-regexp "^[^% ]*\\(\\)* *")
    )
  )

;;}}}
;;{{{ slang-kill-process

(defun slang-kill-process ()
  "Kill slang subprocess and its buffer."
  (interactive)
  (if slang-process-buffer
      (kill-buffer slang-process-buffer)))

;;}}}
;;{{{ slang-set-slang-region-start

(defun slang-set-slang-region-start (&optional arg)
  "Set start of region for use with `slang-send-slang-region'."
  (interactive)
  (set-marker slang-region-start (or arg (point))))

;;}}}
;;{{{ slang-set-slang-region-end

(defun slang-set-slang-region-end (&optional arg)
  "Set end of region for use with `slang-send-slang-region'."
  (interactive)
  (set-marker slang-region-end (or arg (point))))

;;}}}
;;{{{ send line/region/buffer to slang-process

;;{{{ slang-send-current-line

(defun slang-send-current-line ()
  "Send current line to slang subprocess, found in `slang-process'.
If `slang-process' is nil or dead, start a new process first."
  (interactive)
  (let ((start (save-excursion (beginning-of-line) (point)))
	(end (save-excursion (end-of-line) (point))))
    (or (and slang-process
	     (eq (process-status slang-process) 'run))
	(slang-start-process slang-default-application slang-default-application))
    (comint-simple-send slang-process (buffer-substring start end))
    (forward-line 1)
    (if slang-always-show
	(display-buffer slang-process-buffer))))

;;}}}
;;{{{ slang-send-region

(defun slang-send-region (start end)
  "Send region to slang subprocess, wrapped in `eval { ... }'."
  (interactive "r")
  (or (and slang-process
	   (comint-check-proc slang-process-buffer))
      (slang-start-process slang-default-application slang-default-application))
  (comint-simple-send slang-process
		      (concat slang-default-eval
			      " {\n"(buffer-substring start end) "\n}"))
  (if slang-always-show
      (display-buffer slang-process-buffer)))

;;}}}
;;{{{ slang-send-slang-region

(defun slang-send-slang-region ()
  "Send preset slang region to slang subprocess, wrapped in `eval { ... }'."
  (interactive)
  (or (and slang-region-start slang-region-end)
      (error "slang-region not set"))
  (or (and slang-process
	   (comint-check-proc slang-process-buffer))
      (slang-start-process slang-default-application slang-default-application))
  (comint-simple-send slang-process
		      (concat slang-default-eval
			      " {\n"
			      (buffer-substring slang-region-start slang-region-end)
			      "\n}"))
  (if slang-always-show
      (display-buffer slang-process-buffer)))

;;}}}
;;{{{ slang-send-proc

(defun slang-send-proc ()
  "Send proc around point to slang subprocess, wrapped in `eval { ... }'."
  (interactive)
  (let (beg end)
    (save-excursion
      (slang-beginning-of-proc)
      (setq beg (point))
      (slang-end-of-proc)
      (setq end (point)))
    (or (and slang-process
	     (comint-check-proc slang-process-buffer))
	(slang-start-process slang-default-application slang-default-application))
    (comint-simple-send slang-process
			(concat slang-default-eval
				" {\n"
				(buffer-substring beg end)
				"\n}"))
    (if slang-always-show
	(display-buffer slang-process-buffer))))

;;}}}
;;{{{ slang-send-buffer

(defun slang-send-buffer ()
  "Send whole buffer to slang subprocess, wrapped in `eval { ... }'."
  (interactive)
  (or (and slang-process
	   (comint-check-proc slang-process-buffer))
      (slang-start-process slang-default-application slang-default-application))
  (if (buffer-modified-p)
      (comint-simple-send slang-process
			  (concat
			   slang-default-eval
			   " {\n"
			   (buffer-substring (point-min) (point-max))
			   "\n}"))
    (comint-simple-send slang-process
			(concat "source "
				(buffer-file-name)
				"\n")))
  (if slang-always-show
      (display-buffer slang-process-buffer)))

;;}}}

;;}}}
;;{{{ slang-get-error-info

(defun slang-get-error-info ()
  "Send string `set errorInfo' to slang subprocess and display the slang buffer."
  (interactive)
  (or (and slang-process
	   (comint-check-proc slang-process-buffer))
      (slang-start-process slang-default-application slang-default-application))
  (comint-simple-send slang-process "set errorInfo\n")
  (display-buffer slang-process-buffer))

;;}}}
;;{{{ slang-restart-with-whole-file

(defun slang-restart-with-whole-file ()
  "Restart slang subprocess and send whole file as input."
  (interactive)
  (slang-kill-process)
  (slang-start-process slang-default-application slang-default-application)
  (slang-send-buffer))
  
;;}}}  
;;{{{ slang-show-process-buffer

(defun slang-show-process-buffer ()
  "Make sure `slang-process-buffer' is being displayed."
  (interactive)
  (display-buffer slang-process-buffer))

;;}}}
;;{{{ slang-hide-process-buffer

(defun slang-hide-process-buffer ()
  "Delete all windows that display `slang-process-buffer'."
  (interactive)
  (delete-windows-on slang-process-buffer))

;;}}}

;;}}}

;;{{{ menu bar

(define-key slang-mode-menu [restart-with-whole-file]
  '("Restart With Whole File" .  slang-restart-with-whole-file))
(define-key slang-mode-menu [kill-process]
  '("Kill Process" . slang-kill-process))

(define-key slang-mode-menu [hide-process-buffer]
  '("Hide Process Buffer" . slang-hide-process-buffer))
(define-key slang-mode-menu [get-error-info]
  '("Get Error Info" . slang-get-error-info))
(define-key slang-mode-menu [show-process-buffer]
  '("Show Process Buffer" . slang-show-process-buffer))

(define-key slang-mode-menu [end-of-proc]
  '("End Of Proc" . slang-end-of-proc))
(define-key slang-mode-menu [beginning-of-proc]
  '("Beginning Of Proc" . slang-beginning-of-proc))

(define-key slang-mode-menu [send-slang-region]
  '("Send Slang-Region" . slang-send-slang-region))
(define-key slang-mode-menu [set-slang-regio-end]
  '("Set Slang-Region End" . slang-set-slang-region-end))
(define-key slang-mode-menu [set-slang-region-start]
  '("Set Slang-Region Start" . slang-set-slang-region-start))

(define-key slang-mode-menu [send-current-line]
  '("Send Current Line" . slang-send-current-line))
(define-key slang-mode-menu [send-region]
  '("Send Region" . slang-send-region))
(define-key slang-mode-menu [send-proc]
  '("Send Proc" . slang-send-proc))
(define-key slang-mode-menu [send-buffer]
  '("Send Buffer" . slang-send-buffer))

;;}}}

(provide 'slang-mode)


;;{{{ Emacs local variables

;; Local Variables:
;; folded-file: t
;; End:

;;}}}

;;; slang-mode.el ends here
