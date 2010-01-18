;; Background jobs in GNU Emacs
;; Copyright (C) 1998 Joe Keane <jgk@jgk.org>
;;               2010 rubikitch <rubikitch@ruby-lang.org>
;; This file is public domain.

;; Suggested usage:
;; (global-set-key "\M-!" 'background)
;; (autoload 'background "background" nil t)

(require 'shell)
(provide 'background)

;; define global variables
(defvar background-minibuffer-map (make-sparse-keymap)
  "The keymap to use when prompting the user for a background command.")
(defvar background-history-size 0
  "The number of commands in the background command history.")
(defvar background-history-vector (make-vector 16 nil)
  "This vector contains the background command history.  The first element is unused.")
(defvar background-search-last-string ""
  "Last string searched for by a background search.")
;; shut up compiler
(defvar background-history-index nil)

;; variables for keys
(defvar background-search-exit-char ?\C-\])
(defvar background-search-backward-char ?\C-r)
(defvar background-search-forward-char ?\C-s)
(defvar background-search-delete-char ?\C-?)
(defvar background-search-quote-char ?\C-q)

;; add bindings to shell-mode-map
;(define-key shell-mode-map "\C-c\C-k" 'background-kill-subjob)
;(define-key shell-mode-map "\C-c\C-m" 'background-continue-subjob)
;(define-key shell-mode-map "\C-c\C-z" 'background-stop-subjob)

;; add bindings to background-minibuffer-map
(define-key background-minibuffer-map "\C-g" 'abort-recursive-edit)
(define-key background-minibuffer-map "\C-i" 'comint-dynamic-complete)
(define-key background-minibuffer-map "\C-m" 'exit-minibuffer)
(define-key background-minibuffer-map "\C-n" 'background-next-command)
(define-key background-minibuffer-map "\C-p" 'background-previous-command)
(define-key background-minibuffer-map "\C-r" 'background-search-backward)
(define-key background-minibuffer-map "\C-s" 'background-search-forward)
(define-key background-minibuffer-map "\M-?"
  'comint-dynamic-list-filename-completions)

;; missing from shell-mode
(defun background-kill-subjob ()
  "Send a kill signal to this shell's current subjob."
  (interactive)
  (kill-process nil t))
(defun background-stop-subjob ()
  "Send a stop signal to this shell's current subjob."
  (interactive)
  (stop-process nil))
; (get-buffer-process (current-buffer)) t))
(defun background-continue-subjob ()
  "Send a continue signal to this shell's current subjob."
  (interactive)
  (continue-process nil t))

;; move around in command history
(defun background-previous-command (arg)
  "Move backward ARG positions in the background command history."
  (interactive "p")
  (delete-region (point-min) (point-max))
  (insert (format "!.%d" (- arg)))
  (exit-minibuffer))
(defun background-next-command (arg)
  "Move forward ARG positions in the background command history."
  (interactive "p")
  (delete-region (point-min) (point-max))
  (insert (format "!.%d" arg))
  (exit-minibuffer))

;; search in command history
(defun background-search-backward ()
  "Search backward through the background command history.  If you know how
to use isearch you should be able to figure this out."
  (interactive)
  (background-search nil))
(defun background-search-forward ()
  "Search forward through the background command history, like
background-search-backward except forward."
  (interactive)
  (background-search t))
(defun background-search (forward)
  "Function to do the work of searching through background command history."
  (let
      ((search-string "")
       failing
       wrapped
       (search-index
	(or background-history-index (if forward 1 background-history-size)))
       loop-index)
    (while
	(progn
	  (message
	   "%s%s%s\"%s\" %d%% %s"
	   (if failing "F" "")
	   (if wrapped "W" "")
	   (if forward "S" "R")
	   search-string
	   search-index
	   (aref background-history-vector search-index))
	  (setq loop-index search-index)
	  (let
	      ((char (read-char))
	       (continue t))
	    (cond
	     ((= char background-search-exit-char)
	      (setq continue nil))
	     ((= char background-search-backward-char)
	      (and (string-equal search-string "")
		   (setq search-string background-search-last-string))
	      (setq loop-index
	       (if (and failing (not forward))
		   (progn (setq wrapped t) background-history-size)
		 (1- search-index)))
	      (setq forward nil))
	     ((= char background-search-forward-char)
	      (and (string-equal search-string "")
		   (setq search-string background-search-last-string))
	      (setq loop-index
	       (if (and failing forward)
		   (progn (setq wrapped t) 1)
		 (1+ search-index)))
	      (setq forward t))
	     ((= char background-search-delete-char)
	      (setq failing nil)
	      (setq search-string
		(if (string-equal search-string "")
		    background-search-last-string
		  (substring search-string 0 -1))))
	     ((= char background-search-quote-char)
	      (message "quote")
	      (setq search-string
	       (concat search-string (char-to-string (read-quoted-char)))))
	     ((or (< char 32) (>= char 128))
	      (setq unread-command-events (cons char unread-command-events))
	      (setq continue nil))
	     (t
	      (setq search-string
	       (concat search-string (char-to-string char)))))
	    continue))
      (if forward
	  (while
	      (if (> loop-index background-history-size)
		  (progn (setq failing t) nil)
		(or
		 (not
		  (string-match
		   search-string
		   (aref background-history-vector loop-index)))
		 (progn
		   (setq failing nil)
		   (setq search-index loop-index)
		   nil)))
	    (setq loop-index (1+ loop-index)))
	(while
	    (if (<= loop-index 0)
		(progn (setq failing t) nil)
	      (or
	       (not
		(string-match
		 search-string
		 (aref background-history-vector loop-index)))
	       (progn
		 (setq failing nil)
		 (setq search-index loop-index)
		 nil)))
	  (setq loop-index (1- loop-index)))))
    (delete-region (point-min) (point-max))
    (insert (format "!%d" search-index))
    (setq background-search-last-string search-string))
  (exit-minibuffer))

(defun background-read-from-minibuffer (&rest args)
  (apply 'read-from-minibuffer args))

(defun background-read-args ()
  (list
    (let
	((prompt "% ")
	 default-command
	 command
	 background-history-index)
      (while
	  (progn
	    (while
		(let
		    (scrolling)
		  (setq command
                        (background-read-from-minibuffer
                         prompt
                         default-command
                         background-minibuffer-map))
		  scrolling)
	      (setq prompt (format "%d%% " background-history-index))
	      (setq default-command
	       (aref background-history-vector background-history-index)))
	    (string-match "^!" command))
	(cond
	 ((string-match "^!!" command)
	  (setq background-history-index background-history-size))
	 ((string-match "^!-?[0-9]+" command)
	  (let*
	      ((offset (string-to-int (substring command 1)))
	       (index
		(+
		 (if (< offset 0) (1+ background-history-size) 0)
		 offset)))
	    (if (and (> index 0) (<= index background-history-size))
		(setq background-history-index index)
	      (progn (message "Bad history index") (sit-for 1)))))
	 ((string-match "^!\\.-?[0-9]+" command)
	  (let*
	      ((offset (string-to-int (substring command 2)))
	       (index
		(+
		 (or
		  background-history-index
		  (if (< offset 0) (1+ background-history-size) 0))
		 offset)))
	    (cond
	     ((<= index 0)
	      (progn (message "Beginning of command history") (sit-for 1)))
	     ((> index background-history-size)
	      (progn (message "End of command history") (sit-for 1)))
	     (t (setq background-history-index index)))))
	 (t
	  (let*
	      ((index background-history-size)
	       (flag (string-match "^!\\?" command))
	       (event (substring command (if flag 2 1)))
	       (pattern (if flag event (concat "^" event))))
	    (while
		(if (<= index 0)
		    (progn
		      (message "%s: Event not found." event)
		      (sit-for 1)
		      nil)
		  (or
		   (not
		    (string-match
		     pattern
		     (aref background-history-vector index)))
		   (progn (setq background-history-index index) nil)))
	      (setq index (1- index))))))
	(and background-history-index
	     (progn
	       (setq prompt (format "%d%% " background-history-index))
	       (setq default-command
		(aref background-history-vector background-history-index)))))
      command)))

;; main function
(defun background (command)
  "Run COMMAND as a background job.  When entering a command, C-p and
C-n, and C-r and C-s, do interesting things with the command history.
Some bang commands are interpreted.  A message is displayed when the job
starts and finishes, or otherwise changes state.  The job's buffer is in
shell mode, so you can send input and signals to the job.  A prefix
argument suggests a job number, which is useful to store output."
  (interactive
   (background-read-args))
  (prog1
      (let*
	  ((job-number (if (numberp current-prefix-arg) current-prefix-arg 1))
	   (process
	    (let (job-name)
	      (while
		  (get-process (setq job-name (format "%%%d" job-number)))
		(setq job-number (1+ job-number)))
	      (setq default-directory
	       (prog1
		   (if (not (string-match
			     "^[\t ]*cd[\t ]+\\([^\t ;]+\\)[\t ]*;[\t ]*"
			     command))
		       default-directory
		     (prog1
			 (file-name-as-directory
			  (expand-file-name
			   (substring
			    command
			    (match-beginning 1)
			    (match-end 1))))
		       (setq command (substring command (match-end 0)))))
		  (or current-prefix-arg
		      (progn (with-output-to-temp-buffer job-name)))
		  (set-buffer (get-buffer-create job-name))
		  (setq buffer-read-only nil)
		  (erase-buffer)))
	      (start-process job-name (current-buffer)
			     shell-file-name "-c" command))))
	(insert (format "*** Start \"%s\" in %s at %s\n"
			command
			default-directory
			(substring (current-time-string) 11 19)))
	(set-marker (process-mark process) (point))
	(set-process-sentinel
	 process
	 (function
	  (lambda (process info)
	    (message
	     "[%s] %s %s"
	     (substring (process-name process) 1)
	     (setq info
	      (cond
	       ((string-equal info "finished\n") "Done")
	       ((string-match "^exit" info)
		(format "Exit %d" (process-exit-status process)))
	       ((zerop (length info)) "Continuing")
	       (t
		(concat (upcase (substring info 0 1)) (substring info 1 -1)))))
	     (nth 2 (process-command process)))
	    (let
		((buffer (process-buffer process)))
	      (if (null (buffer-name buffer))
		  (set-process-buffer process nil)
		(and
		 (memq (process-status process) '(signal exit))
		 (save-excursion
		   (set-buffer buffer)
		   (goto-char (point-max))
		   (insert (format "*** %s at %s\n"
				   info
				   (substring (current-time-string) 11 19)))
		   (set-buffer-modified-p nil)
		   (undo-boundary))))))))
	(message "[%d] %d" job-number (process-id process))
	process)
    (shell-mode)
    (setq mode-name "Background")
    (or
     (let
	 ((previous-command
	   (aref background-history-vector background-history-size)))
       (and previous-command (string-equal command previous-command)))
     (progn
       (let
	   ((capacity (length background-history-vector)))
	 (and
	  (>=
	   (setq background-history-size (1+ background-history-size))
	   capacity)
	  (progn
	    (let
		((index 0)
		 (new-vector (make-vector (* 2 capacity) nil)))
	      (while
		  (progn
		    (aset
		     new-vector
		     index
		     (aref background-history-vector index))
		    (< (setq index (1+ index)) background-history-size)))
	      (setq background-history-vector new-vector)))))
       (aset background-history-vector background-history-size command)))))
