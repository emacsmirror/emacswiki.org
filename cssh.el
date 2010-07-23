;;; cssh.el --- clusterssh implementation for emacs
;;
;; Copyright (C) 2008 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://pgsql.tapoueh.org/elisp
;; Version: 0.7
;; Created: 2008-09-26
;; Keywords: ClusterSSH ssh cssh
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Emacs Integration:
;; (require 'cssh)
;;
;; cssh bindings to open ClusterSSH controler in cssh-mode on some buffers:
;;
;;  C-=       asks remote hostname then opens a term and ssh to it
;;  C-=       from IBuffer mode opens ClusterSSH controler on marked buffers
;;  C-u C-=   asks for the name of the ClusterSSH controler buffer
;;  C-M-=     matches given regexp against ssh known_hosts and open
;;            buffers in which ssh <remote> is typed
;;  C-u C-M-= asks for a name before 
;;
;; Special keys while in the *cssh* controller you might want to know about:
;;
;;  C-=   redraw buffer selection in windows
;;  C-!   reconnect the ssh, for when your ssh buffers outlive the ssh inside
;;
;; Dired integration:
;;
;;  C-=   in dired uses a `dsh' style configuration file to get the remote
;;        hosts list
;;
;; TODO
;;
;;  * add some more documentation
;;
;; WON'T FIX
;;
;; * the line and char modes as in term.el are not a good idea here, as it
;;   seems much better for the *cssh* controller buffer to behave as much as
;;   possible like a plain emacs buffer.
;;
;; CHANGES
;;
;; 0.7
;;  Add `dsh' configuration file support from dired
;;
;; 0.6
;;  Use tramp to complete the host names
;;

(require 'ibuffer)
(require 'term)
(require 'tramp)

(defgroup cssh nil "ClusterSSH mode customization group"
  :group 'convenience)

(defcustom split-horizontally-first t
  "Do we first split horizontally or vertically"
  :group 'cssh
  :options '(t nil))

(defcustom cssh-prompt "cssh> "
  "cssh buffer prompt"
  :group 'cssh)

(defcustom cssh-input-ring-size 1024
  "*Size of input history ring."
  :type 'integer
  :group 'cssh)

(defcustom cssh-shell "/bin/bash"
  "cssh shell to use"
  :group 'cssh)

(defcustom cssh-term-type "screen"
  "cssh TERM environment variable to export at connection time"
  :group 'cssh)

(defcustom cssh-default-buffer-name "*cssh*"
  "cssh default buffer name, the one in cssh major mode"
  :group 'cssh)

(defcustom cssh-after-command "exit"
  "command to run when exiting from the remote (ssh) shell"
  :group 'cssh)

;;;###autoload
(defun cssh-turn-on-ibuffer-binding ()
  (local-set-key (kbd "C-=") 'cssh-ibuffer-start))

;;;###autoload
(add-hook 'ibuffer-mode-hook 'cssh-turn-on-ibuffer-binding)

;;;###autoload
(global-set-key (kbd "C-=") 'cssh-term-remote-open)

;;;###autoload
(global-set-key (kbd "C-M-=") 'cssh-regexp-host-start)

;;;###autoload
(define-key dired-mode-map (kbd "C-=") 'cssh-dired-find-file)

;; hostname completion, on C-=
(defun cssh-tramp-hosts ()
  "ask tramp for a list of hosts that we can reach through ssh"
  (reduce 'append (mapcar (lambda (x) 
			    (remove* nil (mapcar 'cadr (apply (car x) (cdr x)))))
			  (tramp-get-completion-function "ssh"))))

;;
;; This could be seen as recursion init step, opening a single remote host
;; shell
;;
;;;###autoload
(defun cssh-term-remote-open 
  (&optional remote-host dont-set-buffer dont-send-input)
  "Opens a M-x term and type in ssh remotehost with given hostname"
  (interactive) 
  (let*
      ((ssh-term-remote-host
	(or remote-host
	    (completing-read "Remote host: " (cssh-tramp-hosts))))
       (ssh-command (concat "ssh " ssh-term-remote-host))
       (ssh-buffer-name (concat "*" ssh-command "*"))
       (cssh-remote-open-command
	(if cssh-after-command
	    (format "TERM=%s %s ;%s" cssh-term-type ssh-command cssh-after-command)
	  (format "TERM=%s %s" cssh-term-type ssh-command))))

    (if (get-buffer ssh-buffer-name)
        (unless dont-set-buffer (switch-to-buffer ssh-buffer-name))
      
      (ansi-term cssh-shell ssh-command)
      (unless dont-set-buffer (set-buffer (get-buffer ssh-buffer-name)))
      (with-current-buffer ssh-buffer-name
	(insert cssh-remote-open-command))
      (unless dont-send-input (term-send-input)))
    ;; return the newly created buffer name
    ssh-buffer-name))

;;;
;;; open cssh windows and create buffers from a regexp
;;; the regexp matches host names as in cssh-tramp-hosts
;;;
;;;###autoload
(defun cssh-regexp-host-start (&optional cssh-buffer-name)
  "start ClusterSSH for all mathing hosts in  known_hosts"
  (interactive
   (list
    (and current-prefix-arg
	 (read-buffer "ClusterSSH buffer name: "
		      (generate-new-buffer-name cssh-default-buffer-name)))))
  (setq cssh-buffer-name
	(or cssh-buffer-name cssh-default-buffer-name))
  
  (let* ((re (read-from-minibuffer "Host regexp: "))
	 (buffer-list))

    (dolist (elt (cssh-tramp-hosts) buffer-list)
      (when (string-match re elt)
	(add-to-list 'buffer-list 
		     (get-buffer (cssh-term-remote-open elt t t)))))

    (message "%S" buffer-list)

    (if (endp buffer-list)
	(message "No match to %S" re)
      
      (cssh-open cssh-buffer-name buffer-list)
      (with-current-buffer cssh-buffer-name
	(cssh-send-string "")))))

;;;
;;; ibuffer interaction: open cssh mode for marked buffers
;;;
(defun cssh-ibuffer-start (&optional cssh-buffer-name)
  "start ClusterSSH from current iBuffer marked buffers list"
  (interactive
   (list
    (and current-prefix-arg
	 (read-buffer "ClusterSSH buffer name: "
		      (generate-new-buffer-name cssh-default-buffer-name)))))
  (setq cssh-buffer-name
	(or cssh-buffer-name cssh-default-buffer-name))

  (cssh-init-from-ibuffer-marked-buffers cssh-buffer-name))

(defun cssh-init-from-ibuffer-marked-buffers (cssh-buffer-name)
  "open cssh global input frame and the buffers windows from
marked ibuffers buffers" 
  (let* ((buffers-all-in-term-mode t)
	 (marked-buffers (ibuffer-get-marked-buffers)))
    
    (dolist (elt marked-buffers)
      (progn
	(message (buffer-name elt))
	;;(select-window (get-buffer-window elt))
	(with-current-buffer elt
	  (when (not (eq major-mode 'term-mode))
	    (progn
	      (setq buffers-all-in-term-mode nil)
	      (message "ClusterSSH only supports Term mode buffers"))))))

    (when buffers-all-in-term-mode
      (cssh-open cssh-buffer-name marked-buffers))))

;;;
;;; Dired integration for opening cssh from a `dsh' config file
;;;
(defun cssh-parse-dsh-config-file (filename)
  "Given a filename, parse it as a dsh filename, return the
remote hosts list"
  (with-temp-buffer 
    (insert-file-contents-literally filename)
    (let ((l     (split-string (buffer-string)))
	  (hosts))
      (dolist (elt l)
	(if (string-match "^@.+$" elt)
	    (mapc (lambda (x) (add-to-list 'hosts x)) 
		  (cssh-parse-dsh-config-file (substring elt 1)))
	  (add-to-list 'hosts elt)))
      ;; we return hosts
      hosts)))

(defun cssh-open-dsh-config-file (filename)
  "Given a filename, will parse it as a dsh filename and open
cssh on the hosts"
  (let ((hosts (cssh-parse-dsh-config-file filename))
	(buffer-list))
    (mapc (lambda (x) 
	    (add-to-list 'buffer-list 
			 (get-buffer (cssh-term-remote-open x t t))))
	  hosts)

    (if (endp buffer-list)
	(message "Empty file %S" filename)
      
      (cssh-open cssh-default-buffer-name buffer-list)
      (with-current-buffer cssh-default-buffer-name
	(cssh-send-string "")))))

;;;###autoload
(defun cssh-dired-find-file ()
  "In dired, have cssh connect to hosts in the `dsh' configuration file."
  (interactive)
  ;; dired-get-filename is defined in dired.el
  (cssh-open-dsh-config-file (dired-get-filename)))

;;;
;;; Entry point
;;;
(defun cssh-open (cssh-buffer-name buffer-list)
  "open the cssh global input frame then the ssh buffer windows"

  (cond ((endp buffer-list)
	 (cssh-term-remote-open))

	((eq 1 (length buffer-list))
	 (set-window-buffer (selected-window) (car buffer-list)))
	  
	(t
	 (set-window-buffer 
	  (selected-window) (get-buffer-create cssh-buffer-name))

	 ;; make the controler buffer then split the window
	 (let* ((cssh-controler (split-window-vertically -4)))
	   ;; switch to css-mode, which make-local-variable cssh-buffer-list
	   ;; which we overwrite
	   (set-buffer cssh-buffer-name)
	   (cssh-mode)
	   (setq cssh-buffer-list buffer-list)

	   ;; create the windows needed to host our buffer-list
	   (cssh-nsplit-window buffer-list)

	   ;; now place the user into the cssh-controler and prompt him
	   (select-window cssh-controler)
	   (insert (concat "\n" cssh-prompt))

	   ;; return the buffer list
	   cssh-buffer-list))))

;;;
;;; cssh editing mode
;;;

;; first, the input ring
(defvar cssh-input-ring nil)
(defvar cssh-input-ring-index 0)

(defun cssh-prev-input-string (arg)
  (ring-ref cssh-input-ring (+ cssh-input-ring-index arg)))

(defun cssh-insert-prev-input (arg)
  (interactive "p")
  (save-excursion 
    ;; only consider when on last line (even if not already at point-max)
    (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
      (let ((current-point (point))
	    (input-beginning-position (+ (length cssh-prompt) 
					 (search-backward cssh-prompt))))

	(when (<= input-beginning-position current-point)
	  (delete-region input-beginning-position (point-max))
	  (goto-char input-beginning-position)
	  (insert (cssh-prev-input-string 0))
	  (setq cssh-input-ring-index (1+ cssh-input-ring-index)))))))

(defun cssh-insert-next-input (arg)
  (interactive "p")
  (save-excursion 
    ;; only consider when on last line (even if not already at point-max)
    (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
      (let ((current-point (point))
	    (input-beginning-position (+ (length cssh-prompt) 
					 (search-backward cssh-prompt))))

	(when (<= input-beginning-position current-point)
	  (delete-region input-beginning-position (point-max))
	  (setq cssh-input-ring-index (1- cssh-input-ring-index))
	  (goto-char input-beginning-position))
	  (insert (cssh-prev-input-string -1))))))

(defun cssh-newline-and-prompt ()
  "prompt user"
  (insert (concat "\n"
		  (propertize cssh-prompt
			      'read-only t 'field t
			      'front-sticky t 'rear-nonsticky t))))

(defun cssh-bol ()
  "plain C-a is annoying, better target end of prompt"
  (interactive)
  (beginning-of-line) (search-forward cssh-prompt))

(defvar cssh-buffer-list '()
  "cssh controller buffer (*cssh*) local buffer list")

(defvar cssh-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]              'cssh-send-tab)
    (define-key map (kbd "RET")        'cssh-send-input)
    (define-key map (kbd "C-j")        'cssh-send-input)
    (define-key map (kbd "C-m")        'cssh-send-input)
    (define-key map (kbd "M-p")        'cssh-insert-prev-input)
    (define-key map (kbd "M-n")        'cssh-insert-next-input)
    (define-key map (kbd "C-c C-c")    'cssh-cancel-input)
    (define-key map (kbd "C-c C-l")    'cssh-clear)    
    (define-key map (kbd "C-c C-d")    'cssh-eof)
    (define-key map (kbd "C-c [up]")   'cssh-send-up)
    (define-key map (kbd "C-c [down]") 'cssh-send-down)
    (define-key map (kbd "C-=")        'cssh-reopen)
    (define-key map (kbd "C-!")        'cssh-reconnect-ssh)
    (define-key map (kbd "C-a")        'cssh-bol)
    map)
  "Keymap for `cssh-mode'.")

;;;###autoload
(define-derived-mode cssh-mode fundamental-mode "ClusterSSH"
  "A major mode for controlling multiple terms at once."
  :group 'cssh
  (make-local-variable 'cssh-buffer-list)
  (make-local-variable 'cssh-input-ring)
  (make-local-variable 'cssh-input-ring-index)

  (setq cssh-input-ring (make-ring cssh-input-ring-size)))

;;
;; Input functions
;;
(defun cssh-send-string (string)
  "generic function to send input to the terms"

  (dolist (elt cssh-buffer-list)
    ;; FIXME: get rid of artefacts elements in cssh-buffer-list
    (when (bufferp elt)
      (with-current-buffer elt
	(insert string)
	(term-send-input)))))

(defun cssh-send-defun (term-fun)
  "generic function to apply term function to the terms"

  (dolist (elt cssh-buffer-list)
    ;; FIXME: get rid of artefacts elements in cssh-buffer-list
    (when (bufferp elt)
      (with-current-buffer elt
	(funcall term-fun)))))

(defun cssh-send-up ()
  (interactive)
  (cssh-send-defun 'term-send-up))

(defun cssh-send-down ()
  (interactive)
  (cssh-send-defun 'term-send-down))

(defun cssh-send-tab ()
  (interactive)
  (cssh-send-string
   (buffer-substring (+ (length cssh-prompt) (line-beginning-position))
		     (line-end-position)))
  (cssh-send-string "\C-i"))

(defun cssh-cancel-input ()
  (interactive)
  (cssh-send-string "\C-c")
  (cssh-newline-and-prompt))

(defun cssh-send-input ()
  "send current line content to all cssh-mode buffers"
  (interactive)
  ;; only consider when on last line
  (when (= 1 (forward-line 1))
    (save-excursion
      (let* ((input-beginning-position (+ (length cssh-prompt) 
					  (search-backward cssh-prompt)))
	     (input (buffer-substring 
		     input-beginning-position (point-max))))

	(cssh-send-string input)
	(save-excursion
	  (ring-insert cssh-input-ring input)
	  (setq cssh-input-ring-index 0))))
    (cssh-newline-and-prompt)))

(defun cssh-clear ()
  (interactive)
  (cssh-send-string "\C-l"))

(defun cssh-eof ()
  (interactive)
  (cssh-send-string "\C-d"))

(defun cssh-reopen ()
  (interactive)
  (cssh-open (buffer-name) cssh-buffer-list))

(defun cssh-reconnect-ssh (&optional clear)
  (interactive "P")
  (when (consp clear) (cssh-clear))

  (dolist (elt cssh-buffer-list)
    ;; FIXME: get rid of artefacts elements in cssh-buffer-list
    (when (bufferp elt)
      (let* ((elt-name (buffer-name elt))
	     (buffer-ssh-command (substring elt-name 1 -1)))
	(with-current-buffer elt
	  (insert (concat "TERM=" cssh-term-type " " buffer-ssh-command))
	  (term-send-input))))))

;;;
;;; Window splitting code
;;;
(defun cssh-split-window (&optional backward? &optional size)
  "split current window either vertically or horizontally
depending on split-preference value"
  (let* ((go-horizontal
	 (if backward? (not split-horizontally-first)
	   split-horizontally-first)))

    (if size
	(if go-horizontal
	    (split-window-horizontally size)
	  (split-window-vertically size))

      (if go-horizontal
	  (split-window-horizontally)
	(split-window-vertically)))))

(defun cssh-get-third-size (backward? left top right bottom)
  "Given a window edges and a direction" 
  (let* ((go-horizontal
	 (if backward? (not split-horizontally-first)
	   split-horizontally-first)))

    (/ (+ 1 (if go-horizontal (- right left) (- bottom top))) 3)))

(defun cssh-nsplit-window (buffer-list &optional backward?)
  "split current window into n windows"
  (let* ((w (selected-window))
	 (n (length buffer-list)))

    (cond ((= n 2)
	   ;; if at least one of the list elements is a buffer, it's final
	   ;; recursion and we always prefer to maximize line length
	   (let* ((w1 (cssh-split-window (if (or (bufferp (car buffer-list))
						 (bufferp (cadr buffer-list)))
					     ;; force to split horizontally
					     split-horizontally-first
					   backward?))))

	     (when (bufferp (car buffer-list))
	       (set-window-buffer w (car buffer-list)))

	     (when (bufferp (cadr buffer-list))
	       (set-window-buffer w1 (cadr buffer-list)))

  	     (list w w1)))

	  ((= n 3)
	   ;; if at least one of the list elements is a buffer, it's final
	   ;; recursion and we always prefer to maximize line length
	   (let* ((edges (window-edges))
		  (direction (if (or (bufferp (car buffer-list))
				     (bufferp (cadr buffer-list))
				     (bufferp (cadr (cdr buffer-list))))
				 ;; force to split horizontally first
				 split-horizontally-first
			       backward?))
		  (size      (apply #'cssh-get-third-size
				    (cons direction edges)))
		  (w1        (cssh-split-window direction size))
		  (w2        (progn (select-window w1) 
				    (cssh-split-window direction size))))

	     (when (bufferp (car buffer-list))
	       (set-window-buffer w (car buffer-list)))

	     (when (bufferp (cadr buffer-list))
	       (set-window-buffer w1 (cadr buffer-list)))

	     (when (bufferp (cadr (cdr buffer-list)))
	       (set-window-buffer w2 (cadr (cdr buffer-list))))

	     (list w w1 w2)))

	  ((= n 5)
	   ;; cut in half then split one half in 2 and the other in 3
	   ;; cut in half then split other parts by n/2
	   ;; gives cssh-nsplit-window any 2 elements list
	   (let* ((halves (cssh-nsplit-window '(1 2) backward?)))

	     (select-window (nth 1 halves))

	     (let* ((h1l 
		     (cssh-nsplit-window
		      (butlast buffer-list 3) (not backward?))))

	       (select-window w)
	       (append h1l
		       (cssh-nsplit-window 
			(last buffer-list 3) (not backward?))))))

	  ((= 0 (% n 2))
	   ;; cut in half then split other parts by n/2
	   ;; gives cssh-nsplit-window any 2 elements list
	   (let* ((halves (cssh-nsplit-window '(1 2) backward?)))

	     (select-window (nth 1 halves))

	     (let* ((h1l 
		     (cssh-nsplit-window
		      (butlast buffer-list (/ n 2)) (not backward?))))

	       (select-window w)
	       (append h1l
		       (cssh-nsplit-window 
			(last buffer-list (/ n 2)) (not backward?))))))

	  ((= 0 (% n 3))
	   ;; cut in three parts then re split
	   (let* ((thirds (cssh-nsplit-window '(1 2 3) backward?)))
	     
	     (select-window (nth 1 thirds))

	     (let* ((t1l (cssh-nsplit-window
			  ;; take the first third of the list
			  (butlast (butlast buffer-list (/ n 3)) (/ n 3))
			  (not backward?))))

	       (select-window (nth 2 thirds))

	       (let* ((t2l (cssh-nsplit-window
			    ;; take the second third of the list
			    (last (butlast buffer-list (/ n 3)) (/ n 3))
			    (not backward?))))

		 (select-window w)
		 (append t1l
			 t2l
			 (cssh-nsplit-window
			  ;; take the last third of the list
			  (last buffer-list (/ n 3)) (not backward?)))))))

	  ;; n is not divisible by either 2 or 3, produce some more windows
	  ;; than necessary
	  ((= 0 (% (+ 1 n) 2))
	   (cssh-nsplit-window (cons 1 buffer-list)))

	  ((= 0 (% (+ 1 n) 3))
	   (cssh-nsplit-window (cons 1 buffer-list)))

	  (t (message "error: number of windows not a multiple of 2 or 3.")))))

(provide 'cssh)
;;; cssh.el ends here
