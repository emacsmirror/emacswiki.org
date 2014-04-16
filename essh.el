;;; essh.el --- a set of commands that emulate for bash what ESS is to R.

;; Filename: essh.el


;; ------------------------------------------------------------------ ;;
;; TO INSTALL:                                                        ;;
;; 1. add essh.el in your load-path.                                  ;;
;;                                                                    ;;
;; 2. add to your .emacs file:                                        ;;
;;                                                                    ;;
;; (require 'essh)                                                    ;;
;; (defun essh-sh-hook ()                                             ;;
;;   (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)        ;;
;;   (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)        ;;
;;   (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)          ;;
;;   (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step) ;;
;;   (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)      ;;
;;   (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory)) ;;
;; (add-hook 'sh-mode-hook 'essh-sh-hook)                             ;;
;; ------------------------------------------------------------------ ;;

;; function taken from ess package
(defun essh-next-code-line (&optional arg)
  "Move ARG lines of code forward (backward if ARG is negative).
Skips past all empty and comment lines.	 Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
	(inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc)); n=0 is success
      (while (and (= n 0)
		  (looking-at "\\s-*\\($\\|\\s<\\)"))
	(setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))

(defun process-shell ()
  "returns a list with existing shell process."
  (interactive)
  (setq listpr (process-list))
  (setq lengthpr (length listpr))
  (setq i 0)
  (setq listshellp '())
  (while (< i lengthpr)
    (setq pos (string-match "shell" (prin1-to-string (elt listpr i))))
    (if pos (add-to-list 'listshellp (process-name (get-process (elt listpr i)))))
    (setq i (+ 1 i)))
  listshellp)


(defun process-shell-choose ()
  "returns which process to use."
(interactive)
(setq outpr 0)
(setq cbuf (current-buffer))
(setq shelllist (process-shell))
(setq shelln (length shelllist))
(if (eq shelln 0)
    (progn (shell)
	   (switch-to-buffer cbuf)
	   (setq outpr (get-process "shell"))
	   (sleep-for 0.5)))
(if (eq shelln 1)
    (setq outpr (get-process (elt shelllist 0))))
(if (> shelln 1)
(progn
(setq proc (completing-read "Send code to:" shelllist nil t (elt shelllist 0)))
(setq outpr (get-process proc))))
outpr)


(defun shell-eval-line (sprocess command)
  "Evaluates a single command into the shell process."
  (setq sbuffer (process-buffer sprocess))
  (setq command (concat command "\n"))
  (accept-process-output sprocess 0 10)
  (with-current-buffer sbuffer 
    (end-of-buffer) ;point is not seen being moved (unless sbuffer is focused)
    (insert command)			;pastes the command to shell
    (set-marker (process-mark sprocess) (point-max))
    (process-send-string sprocess command)
    ;; (accept-process-output sprocess 0 10)
    ))

(defun shell-cd-current-directory ()
  "Changes the shell working directory to the current buffer's one."
  (interactive)
  (setq sprocess (process-shell-choose))
  (setq com (format "cd %s" (file-name-directory default-directory)))
  (shell-eval-line sprocess com))


(defun pipe-line-to-shell (&optional step)
  "Evaluates the current line to the shell."
  (interactive ())
  (setq com (buffer-substring (point-at-bol) (point-at-eol)))
  (if (> (length com) 0)
      (progn
	(setq sprocess (process-shell-choose))
	(shell-eval-line sprocess com)
	(when step (essh-next-code-line)))
    (message "No command in this line")))

(defun pipe-line-to-shell-and-step ()
  "Evaluates the current line to the shell and goes to next line."
  (interactive)
  (pipe-line-to-shell t))

(defun pipe-region-to-shell (start end)
  "Sends a region to the shell."
  (interactive "r")
  (setq com (buffer-substring start end))	       ;reads command
  (setq lcom (length com))		       ;count chars
  (setq lastchar (substring com (1- lcom) lcom)) ;get last char
  (unless (string-match "\n" lastchar) ;if last char is not "\n", then...
    (setq com (concat com "\n")))	     ;...add it!
  (setq sprocess (process-shell-choose))
  (setq sbuffer (process-buffer sprocess))
  (while (> (length com) 0) 
    (setq pos (string-match "\n" com)) 
    (setq scom (substring com 0 pos))
    (setq com (substring com (min (length com) (1+ pos))))
    (shell-eval-line sprocess scom)
    (accept-process-output sprocess 0 10)
    )) 


(defun pipe-buffer-to-shell ()
  "Evaluate whole buffer to the shell."
  (interactive)
  (pipe-region-to-shell (point-min) (point-max)))

(defun pipe-function-to-shell ()
"Evaluate function to the shell."
(interactive)
(setq beg-end (essh-beg-end-of-function))
(if beg-end
    (save-excursion
      (setq beg (nth 0 beg-end))
      (setq end (nth 1 beg-end))
      (goto-line beg)
      (setq origin (point-at-bol))
      (goto-line end)
      (setq terminal (point-at-eol))
      (pipe-region-to-shell origin terminal))
  (message "No function at current point.")))

(defun essh-beg-end-of-function ()
  "Returns the lines where the function starts and ends. If there is no function at current line, it returns nil."
  (interactive)
  (setq curline (line-number-at-pos))	;current line
  (setq curcom (buffer-substring (point-at-bol) (point-at-eol)))
  (setq pos (string-match "function" curcom))
  (save-excursion 
    (if pos 
	(progn
	  (setq beg curline))
      (progn
	(while (not pos)
	  (setq curline (1- curline))	;current line
	  (previous-line)			;go to previous line
	  (setq curcom (buffer-substring (point-at-bol) (point-at-eol)))
	  (setq pos (string-match "function" curcom)))
      (setq beg curline)))
    (beginning-of-line)
    (forward-list)			; move pointer to first matching brace
    (setq end (line-number-at-pos)))
  ;; (message (format  "%d %d" beg end))
  (if (and (<= (line-number-at-pos) end) (>= (line-number-at-pos) beg))
      (list beg end)
    nil))
  

(provide 'essh)
