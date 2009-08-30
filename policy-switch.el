;;; policy-switch.el -- Window configuration navigation utility.

;; Copyright (C) 2007  Christoffer S. Hansen

;; Author: Christoffer S. Hansen <csh@freecode.dk>
;; Time-stamp: <2007-07-18 18:35:41 csh>

;; This file is part of policy-switch.

;; policy-switch is free software; you can redistribute it and/or modify
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
 
;; Commentary:

;; This file allows you to navigate sets of window configurations
;; (policies) conveniently.
;; 
;; HOOKS:
;; 
;; To achieve persistence across sessions add the following to your
;; .emacs:
;; 
;; (add-hook 'desktop-save-hook 'policy-switch-remove-unprintable-entities)
;; 
;; Restoring of a config within a session is automatically done when
;; the config's buffer objects are not alive anymore.  However, some
;; modes can alter their content significantly, yet still keep the
;; buffer object alive.  If the actual content upon creation time is
;; important to keep in a config's buffer, consider adding these major
;; modes to the `policy-switch-live-buffer-modes-restore' variable.
;; Then, the buffers will _always_ be restored when a config is
;; selected

;; KEYBINDINGS

;; The following is a suggested set of global keybindings for
;; policy-switch; binding all keys to the `C-c g' prefix:

;; (global-set-key (kbd "C-c g n") 'policy-switch-policy-next)
;; (global-set-key (kbd "C-c g a") 'policy-switch-policy-add)
;; (global-set-key (kbd "C-c g g") 'policy-switch-policy-goto)
;; (global-set-key (kbd "C-c g p") 'policy-switch-policy-prev)
;; (global-set-key (kbd "C-c g r") 'policy-switch-policy-remove)
;; (global-set-key (kbd "C-c g N") 'policy-switch-config-next)
;; (global-set-key (kbd "C-c g P") 'policy-switch-config-prev)
;; (global-set-key (kbd "C-c g G") 'policy-switch-config-goto)
;; (global-set-key (kbd "C-c g A") 'policy-switch-config-add)
;; (global-set-key (kbd "C-c g R") 'policy-switch-config-remove)
;; (global-set-key (kbd "C-c g u") 'policy-switch-config-restore)
;; (global-set-key (kbd "C-c g m") 'policy-switch-toggle-mode-line)

;; To make full benefit of policy-switch it is useful to install the
;; planner libraries since they are used to achieve persistence of
;; gnus and bbdb buffer major modes.  If not installed, they are
;; ignored by policy-switch (unless you provide a suitable replacement
;; function for them, see the documentation of
;; policy-switch-buffer-mode-handlers)

;; Code:

(require 'cl)
(require 'winner)
(require 'desktop)

(defvar policy-switch-policies-list ()
  "List of all policies maintained by policy-switch. The list has the following form:
      (<current-policy-name> ((<policy-name> . (<current-config-name> ((<window-config-name> .
      <window-config-obj> <window-data>))))))")

(defcustom policy-switch-live-buffer-modes-restore
'(gnus-summary-mode gnus-article-mode bbdb-mode dictionary-mode
help-mode)
  "List of major modes that needs to be restored in spite of a live buffer object being present.")
  
(defcustom policy-switch-config-restore-policy 'needs-restoring
  "Specifies when to restore the config when 
selected.  Valid values are:

'always Always restore the config.
'needs-restoring Restore the config when `policy-switch-config-needs-restoring', returns t.
'never Never restore the config automatically. ")

(defcustom policy-switch-mode-line-p t
  "Whether policy status should be shown in the mode line.")
  
(defcustom policy-switch-buffer-mode-handlers '((w3m-mode . policy-switch-buffer-info-w3m)
						(gnus-summary-mode . policy-switch-buffer-info-gnus)
						(bbdb-mode . policy-switch-buffer-info-bbdb)
						(help-mode . policy-switch-buffer-info-help)
						(dictionary-mode . policy-switch-buffer-info-dictionary)
						(gnus-article-mode . policy-switch-buffer-info-gnus))
"Alist of handler functions for buffer major modes.  When a
config is created this list is used to determine what function
should have the responsibilty of creating buffer recover
strings (if buffer is restorable by desktop, desktop takes
precedence and this variable is not used). The string must be
parsable by the Lisp interpreter and is evaluated whenever a
config, whose buffers have their matching major-modes included,
needs to be restored.  The buffer from which the restore string
must be created is current in the call.  Functions take no
arguments and must return the restored buffer object.")

(defvar policy-switch-mode-line-elm nil)
 

(defun policy-switch-policies-list-make-empty ()
  "Make policy list empty."
  (setq policy-switch-policies-list nil))

(defun policy-switch-policy-add (name)
  "Add a policy with `NAME' to policy-switch."
  (interactive "MPolicy name: ")
  (when (assoc-string name policy-switch-policies-list)
    (error "Policy already exists"))
  (setq policy-switch-policies-list (append (list (list name ())) policy-switch-policies-list))
  (message "Policy \"%s\" added" name))


(defun policy-switch-policy-remove (name)
  "Remove a policy given by NAME from policy-switch."
  (interactive
   (list (if policy-switch-policies-list
	     (cond ((= (length policy-switch-policies-list) 1)
		    (car (policy-switch-policy-get)))
		   (t
		    (completing-read "Remove policy: " 
				     (mapcar (lambda (policy)
					       (car policy))
					     policy-switch-policies-list)
				     nil t nil nil (caar
						    policy-switch-policies-list)
				     t))) nil)))
  (let ((policy-to-remove (policy-switch-policy-get name)))
    (setq policy-switch-policies-list (remq nil (mapcar (lambda (policy)
							  (if (string= (car policy)
								       (car policy-to-remove))
							      nil
							    policy))
							policy-switch-policies-list)))
    (message "Policy \"%s\" removed" name)))

;; getter's and setter's for policy list internals
(defun policy-switch-policy-get (&optional policy-name)
  "Get the policy list specified by POLICY-NAME.  Report error if
policy do not exist or if policy list is empty."
  (let ((policy nil))
    (setq policy-name (if (not policy-name)
			  (caar policy-switch-policies-list)
			policy-name))
    (if policy-switch-policies-list
	(progn
	  (setq policy (assoc-string policy-name policy-switch-policies-list))
	  (if (not policy)
	      (error "Policy \"%s\" do not exist" policy-name)
	    policy))
      (error "No policies defined"))))
  

(defun policy-switch-configs-get (policy &optional raise-error-p)
  "Get the configs list specified by POLICY (policy is assumed to
exist and extracted from call to `policy-switch-policy-get').If
RAISE-ERROR-P is non-nil, report error if configs list is empty."
  (let ((configs-list (cadr policy)))
    (if (and raise-error-p
	     (not configs-list))
	(error "Configs list is empty in policy \"%s\"" (car policy))
      configs-list)))

(defun policy-switch-config-get (configname configs-list &optional raise-error-p)
  "Get the config specified by `CONFIGNAME' in the `CONFIGS-LIST'."
  (let* ((configname (if (not configname)
			  (caar configs-list)
			configname))
	 (config (assoc-string configname configs-list)))
    (if (and raise-error-p
	     (not config))
	(error "Config \"%s\" do not exist" configname)
      config)))


(defun policy-switch-config-window-obj (&optional config)
  "Retrieve window config object from `CONFIG', if given.
Otherwise, get window config object from current config in
current policy."
  (let ((config (if (not config)
		    (policy-switch-config-get nil (policy-switch-configs-get (policy-switch-policy-get)))
		  config)))
    (cadr config)))

(defun policy-switch-config-win-data (&optional config)
  "Retrieve window data from `CONFIG', if given.
Otherwise, get window data from current config in current policy."
  (let ((config (if (not config)
		    (policy-switch-config-get
		     nil (policy-switch-configs-get (policy-switch-policy-get)))
		  config)))
    (caddr config)))


(defun policy-switch-configs-list-make-empty (&optional policy-name)
  "Remove all configs in policy `POLICY', if given.
Otherwise, remove all configs in current policy."
  (interactive)
  (let ((policy (policy-switch-policy-get policy-name)))
    (setcdr policy nil)))    


(defun policy-switch-config-add (name)
  "Add current config to current policy) and assign `NAME'."
  (interactive
   (list (if policy-switch-policies-list
	     (read-string "Config name: ")
	   nil)))
  (let* ((policy (policy-switch-policy-get))
	 (configs (policy-switch-configs-get policy))
	 (config (policy-switch-config-get name configs)))
    (when config
      (error "Config \"%s\" exists in policy \"%s\"" name (car policy)))
    (setq configs (append (list (list name (current-window-configuration) (policy-switch-window-info))) configs))
    (setcdr policy (list configs))
    (message "Config \"%s\" added to policy \"%s\"" name (car policy))))


(defun policy-switch-window-info (&optional config-win-data)
  "Get window data from current window configuration."
  (let ((window-data ()))
    (dolist (buffer-data (winner-win-data))
      (let* ((buffer-obj (cdr buffer-data))
	     (old-win-data (assoc (car buffer-data) config-win-data)))
	(setcdr buffer-data (list buffer-obj (if old-win-data
						 (nth (- (length old-win-data) 2) old-win-data)
					       (policy-switch-buffer-info-string
						buffer-obj))
				  (buffer-name buffer-obj)))
	(setq window-data (append window-data
				  (list buffer-data)))))
    window-data))


(defun policy-switch-config-remove (name)
  "Remove config with `NAME' from current policy."
  (interactive
   (list (if policy-switch-policies-list
	     (cond ((<= (length (policy-switch-configs-get (policy-switch-policy-get))) 1)
		    (caar (policy-switch-configs-get (policy-switch-policy-get))))
		   (t
		    (completing-read "Remove config: " 
				     (mapcar (lambda (config)
					       (car config))
					     (policy-switch-configs-get (policy-switch-policy-get)))
				     nil t nil nil (caar (policy-switch-configs-get (policy-switch-policy-get)))
				     t)))
	   nil)))
  (let* ((policy (policy-switch-policy-get))
	 (configs (policy-switch-configs-get policy t))
	 (config (policy-switch-config-get name configs t)))
    (setq configs (remove config configs))
    (setcdr policy (list configs))
    (message "Config \"%s\" removed from policy \"%s\"" name (car policy))))
   

;; Navigation functions
(defun policy-switch-policy-next ()
  "Switch to next policy."
  (interactive)
  (if policy-switch-policies-list
      (progn
	(when (> (length policy-switch-policies-list) 1)
	  (setq policy-switch-policies-list (append (list (nth 1 policy-switch-policies-list))
						    (nthcdr 2 policy-switch-policies-list)
						    (list (car policy-switch-policies-list)))))
	(policy-switch-set-window-configuration))
    (error "Policy list is empty")))

(defun policy-switch-policy-prev ()
  "Switch to next policy."
  (interactive)
  (if policy-switch-policies-list
      (progn
	(when (> (length policy-switch-policies-list) 1)
	  (setq policy-switch-policies-list (append (last policy-switch-policies-list)
						    (butlast policy-switch-policies-list 1))))
	(policy-switch-set-window-configuration))
    (error "Policy list is empty")))

(defun policy-switch-pos-policy (policy-name)
  "Index of policy with `POLICY-NAME' in the policy-list."
  (when (not policy-switch-policies-list)
    (error "No policies defined")))


(defun policy-switch-policy-goto (policy-name)
  "Goto policy by name."
  (interactive
   ;; FIXME: Should work a' la' config-goto
   (list (if policy-switch-policies-list
	     (completing-read "Goto policy: " 
			      (mapcar (lambda (policy)
					(car policy))
				      policy-switch-policies-list)
			      nil
			      t)
	   nil)))
  (let* ((policy (policy-switch-policy-get policy-name))
	 (pos-elem (position policy
			     policy-switch-policies-list)))
    (when (> (length policy-switch-policies-list) 1)
      (setq policy-switch-policies-list (append (subseq policy-switch-policies-list
							pos-elem)
						(subseq policy-switch-policies-list
							0
							pos-elem))))
    (policy-switch-set-window-configuration)))

;; config navigation functions
(defun policy-switch-config-next (&optional policy-name)
  "Switch to next config in policy `POLICY-NAME' (defaults to current policy)."
  (interactive)
  (let* ((policy (policy-switch-policy-get policy-name))
	 (configs (policy-switch-configs-get policy t)))
    (when (> (length configs) 1)
      (setq configs (append (list (nth 1 configs))
			    (nthcdr 2 configs)
			    (list (car configs))))
      (setcdr policy (list configs)))
    (policy-switch-set-window-configuration)))
  
(defun policy-switch-config-prev (&optional policy-name)   
  "Switch to previous config in policy `POLICY-NAME' (defaults to current policy)."
  (interactive)
  (let* ((policy (policy-switch-policy-get policy-name))
	 (configs (policy-switch-configs-get policy t)))
    (when (> (length configs) 1)
      (setq configs (append (last configs)
			    (butlast configs 1)))
      (setcdr policy (list configs)))
    (policy-switch-set-window-configuration)))
  

(defun policy-switch-config-goto (config-name &optional policy-name)
  "Switch to config `CONFIG-NAME' (if interactively called,provide auto-completion) in policy `POLICY-NAME' (defaults to current policy)."
  (interactive
   (list (if policy-switch-policies-list
	     (cond ((<= (length (policy-switch-configs-get (policy-switch-policy-get))) 1)
		    (caar (policy-switch-configs-get (policy-switch-policy-get))))
		   (t
		    (completing-read "Goto config: " 
				     (mapcar (lambda (config)
					       (car config))
					     (policy-switch-configs-get (policy-switch-policy-get)))
				     nil t nil nil (caar (policy-switch-configs-get (policy-switch-policy-get)))
				     t)))
	   nil)))
  (let* ((policy (policy-switch-policy-get policy-name))
	 (configs (policy-switch-configs-get policy t))
	 (config (policy-switch-config-get config-name configs t))
	 (pos-elem (position config configs)))
    (when (> (length configs) 1)
      (setq configs (append (subseq configs
				    pos-elem)
			    (subseq configs
				    0
				    pos-elem)))
      (setcdr policy (list configs)))
    (policy-switch-set-window-configuration)))

(defun policy-switch-set-window-configuration ()
  "Set the window configuration to the value of the current config in the current policy."
  (let* ((policy (policy-switch-policy-get))
	 (configs (policy-switch-configs-get policy))
	 (config (policy-switch-config-get nil configs))
	 (config-name (car config))
	 (config-obj (policy-switch-config-window-obj))
	 (config-win-data (policy-switch-config-win-data config)))
    
    (when (or (equal policy-switch-config-restore-policy 'always)
	      (and (equal policy-switch-config-restore-policy 'needs-restoring)
		   (policy-switch-config-needs-restoring config-name (car policy))))
      (policy-switch-config-restore config-name (car policy))
      (setq config-obj (policy-switch-config-window-obj)))
    (if config-obj
	(progn
	  (set-window-configuration config-obj)
	  (message "Config: \"%s\" in policy \"%s\"" config-name (car policy)))
      (message "Policy \"%s\" do not have any configs" (car policy)))))


(defun policy-switch-config-needs-restoring (&optional name policy-name)
  "Check if config with `NAME' in policy with `POLICY-NAME' needs restoring.
Defaults to current config in current policy.
A config needs restoring if any of its buffer objects are nil.
Return nil if restoring is needed, false otherwise."
  (catch 'needs-restoring 
    (dolist (buffer-restore-data config-win-data)
      (let* ((buf-data (cdr buffer-restore-data))
	     (buf-object (car buf-data)))
	(when (policy-switch-buffer-restore-p buf-object)
	  (throw 'needs-restoring t))))
    nil))
	 

(defun policy-switch-config-restore (&optional name policy-name)
  "Restore config with `NAME' in policy with `POLICY-NAME'(Config
defaults to current config in current policy)."
  (interactive)
  (let* ((policy (policy-switch-policy-get policy-name))
	 (configs (policy-switch-configs-get policy t))
	 (config (policy-switch-config-get name configs t))
	 (config-win-data (policy-switch-config-win-data config))
	 (restorable 0))
    (when (interactive-p)
      (policy-switch-remove-unprintable-entities))
    (delete-other-windows)
    (setq restorable (policy-switch-config-split-windows config-win-data))
    (setq configs (append (list (list (car config)
				      (current-window-configuration)
				      (policy-switch-window-info config-win-data)))
			  (remq config configs)))
    (setcdr policy (list configs))
    (message (if (= (length restorable) 0)
		 "All buffers restored"
	       "%s buffer(s) failed to restore" (length restorable)))))

(defun policy-switch-policy-restore (policy-name)
  "Restore policy with POLICY-NAME."
  (let* ((policy (policy-switch-policy-get policy-name))
	 (configs (policy-switch-configs-get policy t)))
    (dolist (config configs)
      (policy-switch-config-restore (car config) policy-name))))


(defun policy-switch-policies-restore ()
  "Restore policies."
  (save-window-excursion
    (dolist (policy policy-switch-policies-list)
      (policy-switch-policy-restore (car policy)))))


(defun policy-switch-config-split-windows (config-data)
  "Restore policy config from `CONFIG-DATA'."
  (let ((index 0)
	(vert-split nil)
	(hoz-split nil)
	(not-restorable ())
	(split-num 0))
    (dolist (buffer-info config-data)
      (let* ((win-data (car buffer-info))
	     (buffer-data (cdr buffer-info))
	     (buf-object (car buffer-data))
	     (restore-string (nth (- (length buffer-data) 2) buffer-data))
	     (buf-name (car (last buffer-data))))
	;; split until first horizantal or vertical border or end reached
	(dolist (buf-data (nthcdr (1+ index) config-data))
	  (when (< split-num 2)
	  ;; I should split vertically
	    (cond ((and (not vert-split)
			(= (car win-data) (caar buf-data)))
		   (setq vert-split (split-window nil (- (cadar buf-data)
							 (nth 1 win-data))
						  nil))
		   (setq split-num (1+ split-num)))
		  ;; I should split horizontally 
		  ((and (not hoz-split)
			(= (nth 1 win-data) (nth 1 (car buf-data))))
		   (setq hoz-split (split-window nil (- (caar buf-data)
							(car win-data))
						 t))
		   (setq split-num (1+ split-num))))))
	(when (policy-switch-buffer-restore-p buf-object)
	  (setq buf-object (policy-switch-config-restore-buffer restore-string buf-name)))
	(if buf-object
	    (set-window-buffer nil buf-object)
	  (setq not-restorable (append (list buffer-name) not-restorable)))
	(cond (vert-split
	       (select-window vert-split)
	       (setq vert-split nil))
	      (hoz-split
	       (select-window hoz-split)
	       (setq hoz-split nil))
	      (t
	       (other-window 1)))
	(setq index (1+ index)
	      split-num 0)))
    not-restorable))


(defun policy-switch-buffer-restore-p (buffer-obj)
  "Returns non-nil if buffer given by BUFFER-OBJ should be
restored."
  (or (not (buffer-live-p buf-object))
      (null buf-object)
      (find (cdr (assoc 'major-mode (buffer-local-variables buffer-obj)))
	    policy-switch-live-buffer-modes-restore)))
    

(defun policy-switch-config-restore-buffer (restore-string buf-name) 
  "Restore buffer with `RESTORE-STRING'."
  (cond (restore-string
	 (save-window-excursion
	   (eval (read restore-string))))
	(buf-name (get-buffer-create buf-name))))

(defun policy-switch-buffer-info-string (buffer)
  "Get restorable info in string form for BUFFER.  String must be
parsable by the Lisp interpreter.  Use desktop to retrieve
restore info (if `desktop-save-buffer-p' returns non-nil).
Otherwise, look in `policy-switch-mode-handlers' for appropriate
function to call."
  (let* ((create-buffer-string)
	 (buffer-info (policy-switch-buffer-info buffer))
	 (buffer-mode (cdr (assoc 'major-mode
				  (buffer-local-variables))))
	 (restore-function (cdr (assoc buffer-mode policy-switch-buffer-mode-handlers))))
    (setq create-buffer-string
	  (cond ((apply 'desktop-save-buffer-p buffer-info)
		 (concat "(let ((desktop-buffer-ok-count 0)\n"
			 "(desktop-first-buffer nil)\n"
			 "(desktop-buffer-fail-count 0))\n"
			 "("
			 (if (or (not (integerp desktop-restore-eager))
				 (if (zerop desktop-restore-eager)
				     nil
				   (setq desktop-restore-eager (1- desktop-restore-eager))))
			     "desktop-create-buffer"
			   "desktop-append-buffer-args")
			 " "
			 desktop-file-version
			 (let ((temp-string))
			   (dolist (e buffer-info)
			     (setq temp-string (concat temp-string
						       "\n  "
						       (desktop-value-to-string e))))
			   temp-string)
			 ")\n"
			 "desktop-first-buffer)\n\n"))
		(restore-function
		 (funcall restore-function))
		((when (buffer-file-name)
		   (concat "(find-file \"" (buffer-file-name) "\")\n"
			   "(current-buffer)\n\n")))))))

(defun policy-switch-buffer-info-w3m ()
  "Restorable buffer info for w3m buffers."
  (concat "(progn\n"
	  "(save-window-excursion\n"
	  "(delete-other-windows)\n"
	  "(w3m-goto-url-new-session \"" w3m-current-url "\")\n"
	  "(current-buffer)))\n\n"))

(defun policy-switch-buffer-info-dictionary ()
  "Restorable buffer info for dictionary buffers."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "definitions found" nil t)
      (forward-line 4)
      (concat "(progn\n"
	      "(save-window-excursion\n"
	      "(delete-other-windows)\n"
	      "(dictionary-search \"" (current-word) "\")\n"
	      "(current-buffer)))\n\n"))))

(defun policy-switch-buffer-info-help ()
  "Restorable buffer info for *help* buffers."
  (save-excursion
    (goto-char (point-min))
    (let ((doc-word)
	  (first-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (string-match "[^\[:space:\]]*" first-line)
      (setq doc-word (match-string 0 first-line))
      (concat "(progn\n"
	      "(save-window-excursion\n"
	      "(delete-other-windows)\n"
	      "(describe-"
	      (if (string-match "variable" first-line)
		  "variable "
		"function '")
	      doc-word ")\n"
	      "(car (remove nil (mapcar (lambda (buf-data)\n"
	      "(if (equal 'help-mode\n"
	      "(cdr (assoc 'major-mode (buffer-local-variables (cdr buf-data)))))\n"
	      "(cdr buf-data)\n"
	      "nil))\n"
	      "(winner-win-data))))))\n\n"))))

(defun policy-switch-buffer-info-bbdb ()
  "Restorable buffer info for *help* buffers."
  (when (featurep 'planner-bbdb)
    (concat "(let ((buffer-obj nil))\n"
	    "(progn\n"
	    "(save-window-excursion\n"
	    "(delete-other-windows)\n"
	    "(planner-bbdb-browse-url \n"
	    "\"" (substring (car (split-string (planner-bbdb-annotation-from-bbdb) "]")) 2) "\")\n"
	    "(setq buffer-obj (car (remove nil (mapcar (lambda (buf-data)\n"
	    "(if (equal '" (symbol-name (cdr (assoc 'major-mode (buffer-local-variables buffer)))) "\n"
	    "(cdr (assoc 'major-mode (buffer-local-variables (cdr buf-data)))))\n"
	    "(cdr buf-data)\n"
	    "nil))\n"
	    "(winner-win-data))))))))\n\n")))


(defun policy-switch-buffer-info-gnus ()
  "Restorable buffer info for gnus article and gnus summary
buffers."
  (when (featurep 'planner-gnus)
    (concat "(let ((buffer-obj nil))\n"
	    "(progn\n"
	    "(save-window-excursion\n"
	    "(delete-other-windows)\n"
	    "(planner-gnus-browse-url \n"
	    "\"" (substring (car (split-string (planner-gnus-annotation) "]")) 2) "\")\n"
	    "(setq buffer-obj (car (remove nil (mapcar (lambda (buf-data)\n"
	    "(if (equal '" (symbol-name (cdr (assoc 'major-mode (buffer-local-variables buffer)))) "\n"
	    "(cdr (assoc 'major-mode (buffer-local-variables (cdr buf-data)))))\n"
	    "(cdr buf-data)\n"
	    "nil))\n"
	    "(winner-win-data))))))))\n\n")))

(defun policy-switch-buffer-info (buffer)
  "Retrieve buffer info from BUFFER."
  (set-buffer buffer)
  (list
   ;; basic information
   (desktop-file-name (buffer-file-name) desktop-dirname)
   (buffer-name)
   major-mode
   ;; minor modes
   (let (ret)
     (mapc
      #'(lambda (minor-mode)
	  (and (boundp minor-mode)
	       (symbol-value minor-mode)
	       (let* ((special (assq minor-mode desktop-minor-mode-table))
		      (value (cond (special (cadr special))
				   ((functionp minor-mode) minor-mode))))
		 (when value (add-to-list 'ret value)))))
      (mapcar #'car minor-mode-alist))
     ret)
   ;; point and mark, and read-only status
   (point)
   (list (mark t) mark-active)
   buffer-read-only
   ;; auxiliary information
   (when (functionp desktop-save-buffer)
     (funcall desktop-save-buffer dirname))
   ;; local variables
   (let ((locals desktop-locals-to-save)
	 (loclist (buffer-local-variables))
	 (ll))
     (while locals
       (let ((here (assq (car locals) loclist)))
	 (if here
	     (setq ll (cons here ll))
	   (when (member (car locals) loclist)
	     (setq ll (cons (car locals) ll)))))
       (setq locals (cdr locals)))
     ll)))

(defun policy-switch-remove-unprintable-entities ()
  "Remove unprintable entities from policy-switch-policies-list."
  (dolist (policy policy-switch-policies-list)
    (let* ((configs (policy-switch-configs-get policy))
	   (config (policy-switch-config-get nil configs))
	   (config-name (car config))
	   (config-obj (policy-switch-config-window-obj config))
	   (config-win-data (policy-switch-config-win-data config)))
      (dolist (config configs)
	(setcar (cdr config) nil)
	(dolist (buffer-data (policy-switch-config-win-data config))
	  (setcar (cdr buffer-data) nil))))))

(defun policy-switch-toggle-mode-line ()
  "Toggle mode line."
  (interactive)
  (setq policy-switch-mode-line-p (not policy-switch-mode-line-p)))

;; Mode Line
(when (not policy-switch-mode-line-elm)
  (let ((mode-line mode-line-format))
    (setq policy-switch-mode-line-elm
	  '(policy-switch-mode-line-p
	    (:eval
	     (format
	      "[%s %s:%s %s]  " ; [<POLICY-NAME> <TOTAL-NUM>:<CONFIG-NAME> <TOTAL-NUM>]
	      (if policy-switch-policies-list
		  (car (policy-switch-policy-get))
		"None")
	      (int-to-string (length policy-switch-policies-list))
	      (if (and policy-switch-policies-list
		       (policy-switch-configs-get (policy-switch-policy-get)))
		  (car (policy-switch-config-get nil (policy-switch-configs-get (policy-switch-policy-get))))
		"None")
	      (int-to-string (if policy-switch-policies-list
				 (length (policy-switch-configs-get (policy-switch-policy-get)))
			       0))))))
    (let ((pos (position 'mode-line-modes mode-line)))
      (setcdr mode-line (append (subseq mode-line 0 pos) (list policy-switch-mode-line-elm) (nthcdr pos mode-line))))))

(provide 'policy-switch)

;; policy-switch.el ends here

