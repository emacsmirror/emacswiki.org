;;; workspaces.el -- implemented using registers

(defun workspace-create-new (deskid)
  "Create a blank workspace at id deskid, between 1 and 9"
  (interactive "cWhat ID do you want to give to blank workspace ?")
  (workspace-goto ?0)
  (window-configuration-to-register deskid)
  (add-to-list 'workspaces-list deskid)
  (workspace-goto deskid))


(defun workspace-goto (deskid)
  "Go to another workspace, deskid is workspace number between 1 and 9;
Workspace 0 is a template workspace, do not use it unless you know what you do;
You can kill a workspace with 'k' and fallback on 1."
  (interactive "cTo which workspace do you want to go ? ")
  (let (add)
    (setq add (if (eq deskid ?0) "\n!-!-! This is template workspace. New workspaces are based on it. " nil))
    (cond
     ((and (>= deskid ?0) (<= deskid ?9))
      (if (or (position deskid workspaces-list) (eq deskid ?0))
	  (progn
	    (window-configuration-to-register current-workspace)
	    (setq current-workspace deskid)
	    (jump-to-register deskid))
	(if (y-or-n-p "This workspace does not exist, should it be created ? ")
	    (progn
	      (window-configuration-to-register current-workspace)
	      (workspace-create-new deskid))
	  nil)))
     ((and (eq deskid ?k) (not (or (eq current-workspace ?0) (eq current-workspace ?1))))
      (let ((deskid-to-del current-workspace))
	(workspace-goto ?1)
	(setq workspaces-list (remove deskid-to-del workspaces-list))))
     (t (setq add "\n!-!-! Please specify a valid workspace number in (1-9), 0 do edit template, 'k' to kill current workspace in (2-9)")))
    (message (concat "Now on workspace " (char-to-string current-workspace) "\nWorkspaces list is : " (mapconcat 'char-to-string (sort (copy-sequence workspaces-list) '<) ", ") add))))

;; workspaces init
(window-configuration-to-register ?0)
(defvar workspaces-list nil)
(setq current-workspace ?0)
(workspace-create-new ?1)
